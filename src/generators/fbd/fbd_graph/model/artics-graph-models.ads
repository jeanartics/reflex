------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
--                                                                          --
-- Reflex is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware Foundation; either version 3, or (at your option) any later version --
-- Reflex is distributed in the hope that it will be useful, but WITH-      --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License distributed with Reflex; see file COPYING3. If not, go to --
-- http://www.gnu.org/licenses for a ocmplete copy of the license.          --
--                                                                          --
-- Reflex is originally developed by Artics
------------------------------------------------------------------------------

with Artics.Types; use Artics.Types;
with Artics.Namet; use Artics.Namet;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Artics.Objects; use Artics.Objects;
with Artics.Output; use Artics.Output;

--with Artics.Graph.Cells_Interfaces; use Artics.Graph.Cells_Interfaces;
with Artics.Graph.Cells; use Artics.Graph.Cells;
with Artics.Graph.Models_Interfaces; use Artics.Graph.Models_Interfaces;
with Artics.Geometry; use Artics.Geometry;
with Artics.Geometry.Points; use Artics.Geometry.Points;
with Artics.Geometry.Rectangles; use Artics.Geometry.Rectangles;
with Artics.Graph.Cells_Geometry; use Artics.Graph.Cells_Geometry;
with Artics.Graph.Events; use Artics.Graph.Events;
with Artics.Graph.Undoables.Edits; use Artics.Graph.Undoables.Edits;
with Artics.Graph.Models_Changes; use Artics.Graph.Models_Changes;

-- Extends mxEventSource to implement a graph model. The graph model acts as
-- a wrapper around the cells which are in charge of storing the actual graph
-- data structure. The model acts as a transactional wrapper with event
-- notification for all changes, whereas the cells contain the atomic 
-- operations for updating the actual datastructure.
-- 
-- Layers:
-- 
-- The cell hierarchy in the model must have a top-level root cell which
-- contains the layers (typically one default layer), which in turn contain the
-- top-level cells of the layers. This means each cell is contained in a layer.
-- If no layers are required, then all new cells should be added to the default
-- layer.
-- 
-- Layers are useful for hiding and showing groups of cells, or for placing
-- groups of cells on top of other cells in the display. To identify a layer,
-- the Is_Layer function is used. It returns true if the parent of the given
-- cell is the root of the model.
-- 
-- This class fires the following events:
-- 
-- mxEvent.CHANGE fires when an undoable edit is dispatched. The edit
-- property contains the mxUndoableEdit. The changes property contains the list
-- of undoable changes inside the undoable edit. The changes property is
-- deprecated, please use edit.getChanges() instead.
-- 
-- mxEvent.EXECUTE fires between begin and end Update and after an atomic
-- change was executed in the model. The change property contains the atomic
-- change that was executed.
-- 
-- mxEvent.BEGIN_UPDATE fires after the updateLevel was incremented in
-- beginUpdate. This event contains no properties.
-- 
-- mxEvent.END_UPDATE fires after the updateLevel was decreased in endUpdate
-- but before any notification or change dispatching. The <code>edit</code>
-- property contains the current mxUndoableEdit.
-- 
-- mxEvent.BEFORE_UNDO fires before the change is dispatched after the update
-- level has reached 0 in endUpdate. The <code>edit</code> property contains
-- the current mxUndoableEdit.
-- 
-- mxEvent.UNDO fires after the change was dispatched in endUpdate. The
-- <code>edit</code> property contains the current mxUndoableEdit.

package Artics.Graph.Models is
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   type Model_Record is new Event_Source_Record 
     and Model_Interface with private;
   type Model_Ptr is access all Model_Record;
   type Model_Class_Ptr is access all Model_Record'Class;

   No_Model_Record : constant Model_Record;
   
   function New_Graph_Model return access Model_Record'Class;
   -- Constructs a new empty graph model.
   
   function New_Graph_Model 
     (Root : access Cell_Record'Class) return access Model_Record'Class;
   -- Constructs a new graph model. If no root is specified
   -- then a new root mxCell with a default layer is created.
   -- @param root Cell that represents the root cell.
   
   procedure Clear (M : access Model_Record);
   -- Sets a new root using createRoot.
   
   function Get_Update_Level (M : access Model_Record) return Integer;
   
   function Create_Root
     (M : access Model_Record) return access Cell_Record'Class;
   -- Creates a new root cell with a default layer (child 0).
   
   function Get_Cells (M : access Model_Record) return Cells_Maps.Map;
   -- Returns the internal lookup table that is used to map from Ids to cells.
   
   function Get_Cell
     (M  : access Model_Record;
      Id : String) return access Cell_Record'Class;
   -- Returns the cell for the specified Id or null if no cell can be
   -- found for the given Id.
   -- @param id A string representing the Id of the cell.
   -- @return Returns the cell for the given Id.
   
   function Is_Maintain_Edge_Parent (M : access Model_Record) return Boolean;
   -- Returns true if the model automatically update parents of edges so that
   -- the edge is contained in the nearest-common-ancestor of its terminals.
   -- @return Returns true if the model maintains edge parents.
   
   procedure Set_Maintain_Edge_Parent
     (M                    : access Model_Record;
      Maintain_Edge_Parent : Boolean);
   -- Specifies if the model automatically updates parents of edges so that
   -- the edge is contained in the nearest-common-ancestor of its terminals.
   -- @param maintainEdgeParent Boolean indicating if the model should
   -- maintain edge parents.
   
   function Is_Create_Ids (M : access Model_Record) return Boolean;
   -- Returns true if the model automatically creates Ids and resolves Id
   -- collisions.
   -- @return Returns true if the model creates Ids.
   
   procedure Set_Create_Ids
     (M     : access Model_Record;
      Value : Boolean);
   -- Specifies if the model automatically creates Ids for new cells and
   -- resolves Id collisions.
   -- @param value Boolean indicating if the model should created Ids.
   
   function Get_Root
     (M : access Model_Record) return access Cell_Record'Class;
   
   procedure Set_Root
     (M    : access Model_Record;
      Root : access Cell_Record'Class);
   
   function Root_Changed 
     (M    : access Model_Record;
      Root : access Cell_Record'Class) return access Cell_Record'Class;
   -- Inner callback to change the root of the model and update the internal
   -- data structures, such as cells and nextId. Returns the previous root.
   
   function Create_Undoable_Edit
     (M : access Model_Record) return access Undoable_Edit_Record'Class;
   -- Creates a new undoable edit.
   
   function Clone_Cells
     (M                : access Model_Record;
      Cells            : Cells_Lists.List;
      Include_Children : Boolean) return Cells_Lists.List;
   
   function Clone_Cell
     (M                : access Model_Record;
      Cell             : access Cell_Record'Class;
      Mapping          : in out Cells_To_Cells_Maps.Map;
      Include_Children : Boolean) return access Cell_Record'Class;
   -- Inner helper method for cloning cells recursively.
   
   procedure Restore_Clone
     (M       : access Model_Record;
      Clone   : access Cell_Record'Class;
      Cell    : access Cell_Record'Class;
      Mapping : in out Cells_To_Cells_Maps.Map);
   -- Inner helper method for restoring the connections in
   -- a network of cloned cells.
   
   function Is_Ancestor
     (M      : access Model_Record;
      Parent : access Cell_Record'Class;
      Child  : access Cell_Record'Class) return Boolean;
   
   function Contains
     (M    : access Model_Record;
      Cell : access Cell_Record'Class) return Boolean;
   
   function Get_Parent
     (M     : access Model_Record;
      Child : access Cell_Record'Class) return access Cell_Record'Class;
   
   function Get_Top_Parent
     (M     : access Model_Record;
      Child : access Cell_Record'Class) return access Cell_Record'Class;
   
   procedure Add
     (M      : access Model_Record;
      Parent : access Cell_Record'Class;
      Child  : access Cell_Record'Class;
      Index  : Integer);

   procedure Cell_Added
     (M    : access Model_Record;
      Cell : access Cell_Record'Class);
   -- Invoked after a cell has been added to a parent. This recursively
   -- creates an Id for the new cell and/or resolves Id collisions.
   -- @param cell Cell that has been added.
   
   function Create_Id (M : access Model_Record) return String;
   -- Creates a new Id for the given cell and increments the global counter
   -- for creating new Ids.
   -- @param cell Cell for which a new Id should be created.
   -- @return Returns a new Id for the given cell.
   
   procedure Remove
     (M    : access Model_Record;
      Cell : access Cell_Record'Class);
   
   procedure Cell_Removed
     (M    : access Model_Record;
      Cell : access Cell_Record'Class);
   -- Invoked after a cell has been removed from the model. This recursively
   -- removes the cell from its terminals and removes the mapping from the Id
   -- to the cell.
   -- @param cell Cell that has been removed.
   
   function Parent_For_Cell_Changed
     (M      : access Model_Record;
      Cell   : access Cell_Record'Class;
      Parent : access Cell_Record'Class;
      Index  : Integer) return access Cell_Record'Class;
   -- Inner callback to update the parent of a cell using Cell.insert on the
   -- parent and return the previous parent.
   
   function Get_Children_List
     (M    : access Model_Record;
      Cell : access Cell_Record'Class) return Cells_Lists.List;
   
   function Get_Child_Count
     (M      : access Model_Record;
      Cell   : access Cell_Record'Class) return Integer;
   
   function Get_Child_At
     (M      : access Model_Record;
      Parent : access Cell_Record'Class;
      Index  : Integer) return access Cell_Record'Class;

   function Get_Terminal
     (M         : access Model_Record;
      Edge      : access Cell_Record'Class;
      Is_Source : Boolean) return access Cell_Record'Class;
   
   procedure Set_Terminal
     (M         : access Model_Record;
      Edge      : access Cell_Record'Class;
      Terminal  : access Cell_Record'Class;
      Is_Source : Boolean);
   
   procedure Terminal_For_Cell_Changed
     (M         : access Model_Record;
      Edge      : access Cell_Record'Class;
      Terminal  : access Cell_Record'Class;
      Is_Source : Boolean);
   -- Inner helper function to update the terminal of the edge using
   -- Cell.insertEdge and return the previous terminal.
   
   procedure Update_Edge_Parents
     (M    : access Model_Record;
      Cell : access Cell_Record'Class);
   -- Updates the parents of the edges connected to the given cell and all its
   -- descendants so that each edge is contained in the nearest common
   -- ancestor.
   -- @param cell Cell whose edges should be checked and updated.
   
   procedure Update_Edge_Parents
     (M    : access Model_Record;
      Cell : access Cell_Record'Class;
      Root : access Cell_Record'Class);
   -- Updates the parents of the edges connected to the given cell and all its
   -- descendants so that the edge is contained in the nearest-common-ancestor.
   -- @param cell Cell whose edges should be checked and updated.
   -- @param root Root of the cell hierarchy that contains all cells.
   
   procedure Update_Edge_Parent
     (M    : access Model_Record;
      Edge : access Cell_Record'Class;
      Root : access Cell_Record'Class);
   -- Inner helper method to update the parent of the specified edge to the
   -- nearest-common-ancestor of its two terminals.
   -- @param edge Specifies the edge to be updated.
   -- @param root Current root of the model.
   
   function Get_Origin
     (M    : access Model_Record;
      Cell : access Cell_Record'Class) return Point_Record;
   -- Returns the absolute, accumulated origin for the children inside the
   -- given parent. 
   
   function Get_Nearest_Common_Ancestor
     (M     : access Model_Record;
      Cell1 : access Cell_Record'Class;
      Cell2 : access Cell_Record'Class) return access Cell_Record'Class;
   -- Returns the nearest common ancestor for the specified cells.
   -- @param cell1 Cell that specifies the first cell in the tree.
   -- @param cell2 Cell that specifies the second cell in the tree.
   -- @return Returns the nearest common ancestor of the given cells.
   
   function Get_Edges_List
     (M    : access Model_Record;
      Cell : access Cell_Record'Class) return Cells_Lists.List;
   
   function Get_Edge_Count
     (M    : access Model_Record;
      Cell : access Cell_Record'Class) return Integer;
   
   function Get_Edge_At
     (M      : access Model_Record;
      Parent : access Cell_Record'Class;
      Index  : Integer) return access Cell_Record'Class;
   
   function Is_Vertex
     (M    : access Model_Record;
      Cell : access Cell_Record'Class) return Boolean;
   
   function Is_Edge
     (M    : access Model_Record;
      Cell : access Cell_Record'Class) return Boolean;

   function Get_Value
     (M    : access Model_Record;
      Cell : access Cell_Record'Class) return access Object_Record'Class;
   
   procedure Set_Value
     (M     : access Model_Record;
      Cell  : access Cell_Record'Class;
      Value : access Object_Record'Class);

   function Value_For_Cell_Changed
     (M     : access Model_Record;
      Cell  : access Cell_Record'Class;
      Value : access Object_Record'Class) return access Object_Record'Class;
   -- Inner callback to update the user object of the given mxCell
   -- using mxCell.setValue and return the previous value,
   -- that is, the return value of mxCell.getValue.
   
   function Get_Geometry
     (M     : access Model_Record;
      Cell  : access Cell_Record'Class) 
      return access Cell_Geometry_Record'Class;

   procedure Set_Geometry
     (M        : access Model_Record;
      Cell     : access Cell_Record'Class;
      Geometry : access Cell_Geometry_Record'Class);
      
   function Geometry_For_Cell_Changed
     (M        : access Model_Record;
      Cell     : access Cell_Record'Class;
      Geometry : access Cell_Geometry_Record'Class)
      return access Cell_Geometry_Record'Class;
   -- Inner callback to update the mxGeometry of the given mxCell using
   -- mxCell.setGeometry and return the previous mxGeometry.
  
   procedure Execute 
     (M      : access Model_Record;
      Change : access Model_Change_Record'Class);
   -- Executes the given atomic change and adds it to the current edit.
   -- @param change Atomic change to be executed.
   
   procedure Begin_Update (M : access Model_Record);
   
   procedure End_Update (M : access Model_Record);
   
   procedure Merge_Children
     (M               : access Model_Record;
      From            : access Cell_Record'Class;
      To              : access Cell_Record'Class;
      Clone_All_Edges : Boolean);
   -- Merges the children of the given cell into the given target cell inside
   -- this model. All cells are cloned unless there is a corresponding cell in
   -- the model with the same id, in which case the source cell is ignored and
   -- all edges are connected to the corresponding cell in this model. Edges
   -- are considered to have no identity and are always cloned unless the
   -- cloneAllEdges flag is set to false, in which case edges with the same
   -- id in the target model are reconnected to reflect the terminals of the
   -- source edges.
   -- @param from
   -- @param to
   -- @param cloneAllEdges
   
   procedure Merge_Children_Impl
     (M               : access Model_Record;
      From            : access Cell_Record'Class;
      To              : access Cell_Record'Class;
      Clone_All_Edges : Boolean;
      Mapping         : in out Cells_To_Cells_Maps.Map);
   -- Clones the children of the source cell into the given target cell in
   -- this model and adds an entry to the mapping that maps from the source
   -- cell to the target cell with the same id or the clone of the source cell
   -- that was inserted into this model.
   
   procedure Read_Object (M : access Model_Record);
   -- private void readObject(ObjectInputStream ois) throws IOException,
   -- ClassNotFoundException
   -- Initializes the currentEdit field if the model is deserialized.
   
private

   type Model_Record is new Event_Source_Record and Model_Interface with record
      Root : access Cell_Record'Class;
      -- Holds the root cell, which in turn contains the cells that represent
      -- the layers of the diagram as child cells. That is, the actual element
      -- of the diagram are supposed to live in the third generation of cells
      -- and below.      
      
      Maintain_Edge_Parent : Boolean;
      -- Specifies if edges should automatically be moved into the nearest
      -- common ancestor of their terminals. Default is true.
      
      Create_Ids : Boolean;
      -- Specifies if the model should automatically create Ids for new cells.
      -- Default is true.
      
      Next_Id : Integer;
      
      Cells : Cells_Maps.Map;
      
      Current_Edit : access Undoable_Edit_Record'Class; -- Ptr;
      -- Holds the changes for the current transaction. If the transaction is
      -- closed then a new object is created for this variable using
      -- createUndoableEdit.

      Update_Level : Integer;
      -- Counter for the depth of nested transactions. Each call to beginUpdate
      -- increments this counter and each call to endUpdate decrements it. When
      -- the counter reaches 0, the transaction is closed and the respective
      -- events are fired. Initial value is 0.

      Ending_Update : Boolean;
   end record;
   
   No_Model_Record : constant Model_Record :=
     (No_Event_Source_Record with
      Root               => null,
      Maintain_Edge_Parent => False,
      Create_Ids           => False,
      Next_Id              => 0,
      Cells                => Cells_Maps.Empty_Map,
      Current_Edit         => null,
      Update_Level         => 0,
      Ending_Update        => False);

end Artics.Graph.Models;
