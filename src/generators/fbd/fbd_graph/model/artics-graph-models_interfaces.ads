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
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- Reflex is originally developed by Artics
------------------------------------------------------------------------------

with Artics.Types; use Artics.Types;
with Artics.Namet; use Artics.Namet;
with Artics.Objects; use Artics.Objects;
with Artics.Output; use Artics.Output;

--with Artics.Graph.Cells_Interfaces; use Artics.Graph.Cells_Interfaces;
with Artics.Graph.Cells; use Artics.Graph.Cells;
with Artics.Graph.Cells_Geometry; use Artics.Graph.Cells_Geometry;
with Artics.Graph.Filters_Interfaces; use Artics.Graph.Filters_Interfaces;

with Artics.Graph.Events; use Artics.Graph.Events;

-- Extends mxEventSource to implement a graph model. The graph model acts as
-- a wrapper around the cells which are in charge of storing the actual graph
-- datastructure. The model acts as a transactional wrapper with event
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
-- the <isLayer> function is used. It returns true if the parent of the given
-- cell is the root of the model.
-- 
-- This class fires the following events:
-- 
-- mxEvent.CHANGE fires when an undoable edit is dispatched. The edit
-- property contains the mxUndoableEdit. The <code>changes</code> property
-- contains the list of undoable changes inside the undoable edit. The changes
-- property is deprecated, please use edit.getChanges() instead.
-- 
-- mxEvent.EXECUTE fires between begin- and endUpdate and after an atomic
-- change was executed in the model. The <code>change</code> property contains
-- the atomic change that was executed.
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
--

package Artics.Graph.Models_Interfaces is

   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;

   type Model_Interface is interface;
   type Model_Interface_Ptr is access all Model_Interface'Class;

   function Get_Cells
     (M : access Model_Interface) return Cells_Maps.Map is abstract;
   -- Returns the internal lookup table that is used to map from Ids to cells.
   
   function Get_Cell
     (M  : access Model_Interface;
      Id : String) return access Cell_Record'Class is abstract;
   -- Returns the cell for the specified Id or null if no cell can be
   -- found for the given Id.
   -- @param id A string representing the Id of the cell.
   -- @return Returns the cell for the given Id.
   
   function Is_Maintain_Edge_Parent
     (M : access Model_Interface) return Boolean is abstract;
   -- Returns true if the model automatically update parents of edges so that
   -- the edge is contained in the nearest-common-ancestor of its terminals.
   -- @return Returns true if the model maintains edge parents.
   
   procedure Set_Maintain_Edge_Parent
     (M                    : access Model_Interface;
      Maintain_Edge_Parent : Boolean) is abstract;
   -- Specifies if the model automatically updates parents of edges so that
   -- the edge is contained in the nearest-common-ancestor of its terminals.
   -- @param maintainEdgeParent Boolean indicating if the model should
   -- maintain edge parents.
   
   function Is_Create_Ids
     (M : access Model_Interface) return Boolean is abstract;
   -- Returns true if the model automatically creates Ids and resolves Id
   -- collisions.
   -- @return Returns true if the model creates Ids.
   
   procedure Set_Create_Ids
     (M     : access Model_Interface;
      Value : Boolean) is abstract;
   -- Specifies if the model automatically creates Ids for new cells and
   -- resolves Id collisions.
   -- @param value Boolean indicating if the model should created Ids.
   
   function Get_Root
     (M : access Model_Interface) return access Cell_Record'Class is abstract;
   procedure Set_Root
     (M     : access Model_Interface;
      Root  : access Cell_Record'Class) is abstract;
   -- Holds the root cell, which in turn contains the cells that represent
   -- the layers of the diagram as child cells. That is, the actual element
   -- of the diagram are supposed to live in the third generation of cells
   -- and below.      
   
   function Root_Changed 
     (M    : access Model_Interface;
      Root : access Cell_Record'Class) 
      return access Cell_Record'Class is abstract;
   -- Inner callback to change the root of the model and update the internal
   -- datastructures, such as cells and nextId. Returns the previous root.
   
   function Clone_Cells
     (M                : access Model_Interface;
      Cells            : Cells_Lists.List;
      Include_Children : Boolean) return Cells_Lists.List is abstract;
   -- Returns an array of clones for the given array of cells.
   -- Depending on the value of includeChildren, a deep clone is created for
   -- each cell. Connections are restored based if the corresponding
   -- cell is contained in the passed in array.
   -- @param cells Array of cells to be cloned.
   -- @param includeChildren Boolean indicating if the cells should be cloned
   -- with all descendants.
   -- @return Returns a cloned array of cells.
   
   function Is_Ancestor
     (M      : access Model_Interface;
      Parent : access Cell_Record'Class;
      Child  : access Cell_Record'Class) return Boolean is abstract;
   -- Returns true if the given parent is an ancestor of the given child.
   -- @param parent Cell that specifies the parent.
   -- @param child Cell that specifies the child.
   -- @return Returns true if child is an ancestor of parent.
   
   function Contains 
     (M     : access Model_Interface;
      Cells : access Cell_Record'Class) return Boolean is abstract;
   -- Returns true if the model contains the given cell.
   -- @param cell Cell to be checked.
   -- @return Returns true if the cell is in the model.
   
   function Get_Parent
     (M     : access Model_Interface;
      Child : access Cell_Record'Class) 
      return access Cell_Record'Class is abstract;
   -- Returns the parent of the given cell.
   -- @param child Cell whose parent should be returned.
   -- @return Returns the parent of the given cell.

   procedure Add
     (M      : access Model_Interface;
      Parent : access Cell_Record'Class;
      Child  : access Cell_Record'Class;
      Index  : Integer) is abstract;
   -- Adds the specified child to the parent at the given index. If no index
   -- is specified then the child is appended to the parent's array of
   -- children.
   -- @param parent Cell that specifies the parent to contain the child.
   -- @param child Cell that specifies the child to be inserted.
   -- @param index Integer that specifies the index of the child.
   -- @return Returns the inserted child.
   
   procedure Remove
     (M    : access Model_Interface;
      Cell : access Cell_Record'Class) is abstract;
   -- Removes the specified cell from the model. This operation will remove
   -- the cell and all of its children from the model.
   -- @param cell Cell that should be removed.
   -- @return Returns the removed cell.
   
   function Get_Children_List
     (M    : access Model_Interface;
      Cell : access Cell_Record'Class) return Cells_Lists.List is abstract;
   
   function Get_Child_Count
     (M    : access Model_Interface;
      Cell : access Cell_Record'Class) return Integer is abstract;
   -- Returns the number of children in the given cell.
   -- @param cell Cell whose number of children should be returned.
   -- @return Returns the number of children in the given cell.
   
   function Get_Child_At
     (M      : access Model_Interface;
      Parent : access Cell_Record'Class;
      Index  : Integer) 
      return access Cell_Record'Class is abstract;
   -- Returns the child of the given parent at the given index.
   -- @param parent Cell that represents the parent.
   -- @param index Integer that specifies the index of the child to be
   -- returned.
   -- @return Returns the child at index in parent.
   
   function Get_Terminal
     (M         : access Model_Interface;
      Edge      : access Cell_Record'Class;
      Is_Source : Boolean) return access Cell_Record'Class is abstract;
   -- Returns the source or target terminal of the given edge depending on the
   -- value of the boolean parameter.
   -- @param edge Cell that specifies the edge.
   -- @param isSource Boolean indicating which end of the edge should be
   -- returned.
   -- @return Returns the source or target of the given edge.
   
   procedure Set_Terminal
     (M         : access Model_Interface;
      Edge      : access Cell_Record'Class;
      Terminal  : access Cell_Record'Class;
      Is_Source : Boolean) is abstract;
   -- Sets the source or target terminal of the given edge using.
   -- @param edge Cell that specifies the edge.
   -- @param terminal Cell that specifies the new terminal.
   -- @param isSource Boolean indicating if the terminal is the new source or
   -- target terminal of the edge.
   -- @return Returns the new terminal.
   
   procedure Terminal_For_Cell_Changed
     (M         : access Model_Interface;
      Edge      : access Cell_Record'Class;
      Terminal  : access Cell_Record'Class;
      Is_Source : Boolean) is abstract;
   -- Inner helper function to update the terminal of the edge using
   -- mxCell.insertEdge and return the previous terminal.
   
   function Parent_For_Cell_Changed
     (M      : access Model_Interface;
      Cell   : access Cell_Record'Class;
      Parent : access Cell_Record'Class;
      Index  : Integer) 
      return access Cell_Record'Class is abstract;
   -- Inner callback to update the parent of a cell using mxCell.insert
   -- on the parent and return the previous parent.
   
   function Get_Edges_List
     (M    : access Model_Interface;
      Cell : access Cell_Record'Class) return Cells_Lists.List is abstract;
   
   function Get_Edge_Count
     (M    : access Model_Interface;
      Cell : access Cell_Record'Class) return Integer is abstract;
   -- Returns the number of distinct edges connected to the given cell.
   -- @param cell Cell that represents the vertex.
   -- @return Returns the number of edges connected to cell.
   
   function Get_Edge_At
     (M     : access Model_Interface;
      Cell  : access Cell_Record'Class;
      Index : Integer) 
      return access Cell_Record'Class is abstract;
   -- Returns the edge of cell at the given index.
   -- @param cell Cell that specifies the vertex.
   -- @param index Integer that specifies the index of the edge to return.
   -- @return Returns the edge at the given index.
   
   function Is_Vertex
     (M    : access Model_Interface;
      Cell : access Cell_Record'Class) return Boolean is abstract;
   -- Returns true if the given cell is a vertex.
   -- @param cell Cell that represents the possible vertex.
   -- @return Returns true if the given cell is a vertex.
   
   function Is_Edge
     (M    : access Model_Interface;
      Cell : access Cell_Record'Class) return Boolean is abstract;
   -- Returns true if the given cell is an edge.
   -- @param cell Cell that represents the possible edge.
   -- @return Returns true if the given cell is an edge.
 
   function Get_Value
     (M    : access Model_Interface;
      Cell : access Cell_Record'Class) 
      return access Object_Record'Class is abstract;
   -- Returns the user object of the given cell.
   -- @param cell Cell whose user object should be returned.
   -- @return Returns the user object of the given cell.
   
   procedure Set_Value
     (M     : access Model_Interface;
      Cell  : access Cell_Record'Class;
      Value : access Object_Record'Class) is abstract;
   -- Sets the user object of then given cell.
   -- @param cell Cell whose user object should be changed.
   -- @param value Object that defines the new user object.
   -- @return Returns the new value.
   
   function Get_Geometry
     (M    : access Model_Interface;
      Cell : access Cell_Record'Class) 
      return access Cell_Geometry_Record'Class is abstract;
   -- Returns the geometry of the given cell.
   -- @param cell Cell whose geometry should be returned.
   -- @return Returns the geometry of the given cell.
   
   procedure Set_Geometry
     (M        : access Model_Interface;
      Cell     : access Cell_Record'Class;
      Geometry : access Cell_Geometry_Record'Class) is abstract;
      
   
   --  function Get_Geometry
   --    (M        : access Model_Interface;
   --     Cell     : access Cell_Record'Class;
   --     Geometry : Cell_Geometry_Ptr) return Cell_Geometry_Ptr is abstract;
   -- Sets the geometry of the given cell.
   -- @param cell Cell whose geometry should be changed.
   -- @param geometry Object that defines the new geometry.
   -- @return Returns the new geometry.
   
   procedure Begin_Update (M : access Model_Interface) is abstract;
   -- Increments the updateLevel by one. The event notification is queued
   -- until updateLevel reaches 0 by use of endUpdate.

   procedure End_Update (M : access Model_Interface) is abstract;
   -- Decrements the updateLevel by one and fires a notification event if the
   -- updateLevel reaches 0.
   
   procedure Add_Listener
     (M        : access Model_Interface;
      Evt_Type : Event_Type_Enum;
      Listener : access Listener_Interface'Class) is abstract;
   -- Binds the specified function to the given event name. If no event name
   -- is given, then the listener is registered for all events.
   
   procedure Remove_Listener
     (M        : access Model_Interface;
      Listener : access Listener_Interface'Class;	    
      Evt_Type : Event_Type_Enum := No_Event_Type) is abstract;
   -- Removes the given listener from the list of listeners.
   
   function Get_Directed_Edge_Count
     (M        : access Model_Interface'Class;
      Cell     : access Cell_Record'Class;
      Outgoing : Boolean) return Integer;
   -- Returns the number of incoming or outgoing edges.
   -- @param model Graph model that contains the connection data.
   -- @param cell Cell whose edges should be counted.
   -- @param outgoing Boolean that specifies if the number of outgoing or
   -- incoming edges should be returned.
   -- @return Returns the number of incoming or outgoing edges.
   
   function Get_Directed_Edge_Count
     (M           : access Model_Interface'Class;
      Cell        : access Cell_Record'Class;
      Outgoing    : Boolean;
      Ignore_Edge : access Cell_Record'Class) return Integer;
   -- Returns the number of incoming or outgoing edges, ignoring the given
   -- edge.
   -- @param model Graph model that contains the connection data.
   -- @param cell Cell whose edges should be counted.
   -- @param outgoing Boolean that specifies if the number of outgoing or
   -- incoming edges should be returned.
   -- @param ignoredEdge Object that represents an edge to be ignored.
   -- @return Returns the number of incoming or outgoing edges.
   
   function Get_Edges
     (M    : access Model_Interface'Class;
      Cell : access Cell_Record'Class) return Cells_Lists.List;
   -- Returns all edges connected to this cell including loops.
   -- @param model Model that contains the connection information.
   -- @param cell Cell whose connections should be returned.
   -- @return Returns the array of connected edges for the given cell.
   
   function Get_Connections
     (M    : access Model_INterface'Class;
      Cell : access Cell_Record'Class) return Cells_Lists.List;
   -- Returns all edges connected to this cell without loops.
   -- @param model Model that contains the connection information.
   -- @param cell Cell whose connections should be returned.
   -- @return Returns the connected edges for the given cell.
   
   function Get_Incoming_Edges
     (M    : access Model_Interface'Class;
      Cell : access Cell_Record'Class) return Cells_Lists.List;
   -- Returns the incoming edges of the given cell without loops.
   -- @param model Graphmodel that contains the edges.
   -- @param cell Cell whose incoming edges should be returned.
   -- @return Returns the incoming edges for the given cell.

   function Get_Outgoing_Edges
     (M    : access Model_Interface'Class;
      Cell : access Cell_Record'Class) return Cells_Lists.List;
   -- Returns the outgoing edges of the given cell without loops.
   -- @param model Graphmodel that contains the edges.
   -- @param cell Cell whose outgoing edges should be returned.
   -- @return Returns the outgoing edges for the given cell.
   
   function Get_Edges
     (M             : access Model_Interface'Class;
      Cell          : access Cell_Record'Class;
      Incoming      : Boolean;
      Outgoing      : Boolean;
      Include_Loops : Boolean) return Cells_Lists.List;
   -- Returns all distinct edges connected to this cell.
   -- @param model Model that contains the connection information.
   -- @param cell Cell whose connections should be returned.
   -- @param incoming Specifies if incoming edges should be returned.
   -- @param outgoing Specifies if outgoing edges should be returned.
   -- @param includeLoops Specifies if loops should be returned.
   -- @return Returns the array of connected edges for the given cell.
   
   function Get_Edges_Between
     (M      : access Model_Interface'Class;
      Source : access Cell_Record'Class;
      Target : access Cell_Record'Class) return Cells_Lists.List;
   -- Returns all edges from the given source to the given target.
   -- @param model The graph model that contains the graph.
   -- @param source Object that defines the source cell.
   -- @param target Object that defines the target cell.
   -- @return Returns all edges from source to target.

   function Get_Edges_Between
     (M        : access Model_Interface'Class;
      Source   : access Cell_Record'Class;
      Target   : access Cell_Record'Class;
      Directed : Boolean) return Cells_Lists.List;
   -- Returns all edges between the given source and target pair. If directed
   -- is true, then only edges from the source to the target are returned,
   -- otherwise, all edges between the two cells are returned.
   -- @param model The graph model that contains the graph.
   -- @param source Object that defines the source cell.
   -- @param target Object that defines the target cell.
   -- @param directed Boolean that specifies if the direction of the edge
   -- should be taken into account.
   -- @return Returns all edges between the given source and target.
     
   function Get_Opposites
     (M        : access Model_Interface'Class;
      Edges    : Cells_Lists.List;
      Terminal : access Cell_Record'Class) return Cells_Lists.List;
   -- Returns all opposite cells of terminal for the given edges.
   -- @param model Model that contains the connection information.
   -- @param edges Array of edges to be examined.
   -- @param terminal Cell that specifies the known end of the edges.
   -- @return Returns the opposite cells of the given terminal.

   function Get_Opposites
     (M        : access Model_Interface'Class;
      Edges    : Cells_Lists.List;
      Terminal : access Cell_Record'Class;
      Sources  : Boolean;
      Targets  : Boolean) return Cells_Lists.List;
   -- Returns all opposite vertices wrt terminal for the given edges, only
   -- returning sources and/or targets as specified. The result is returned as
   -- an array of mxCells.
   -- @param model Model that contains the connection information.
   -- @param edges Array of edges to be examined.
   -- @param terminal Cell that specifies the known end of the edges.
   -- @param sources Boolean that specifies if source terminals should
   -- be contained in the result. Default is true.
   -- @param targets Boolean that specifies if target terminals should
   -- be contained in the result. Default is true.
   -- @return Returns the array of opposite terminals for the given edges.
     
   procedure Set_Terminals
     (M      : access Model_Interface'Class;
      Edge   : access Cell_Record'Class;
      Source : access Cell_Record'Class;
      Target : access Cell_Record'Class);
   -- Sets the source and target of the given edge in a single atomic change.
   -- @param edge Cell that specifies the edge.
   -- @param source Cell that specifies the new source terminal.
   -- @param target Cell that specifies the new target terminal.
     
   function Get_Children
     (M      : access Model_Interface'Class;
      Parent : access Cell_Record'Class) return Cells_Lists.List;
   -- Returns all children of the given cell regardless of their type.
   -- @param model Model that contains the hierarchical information.
   -- @param parent Cell whose child vertices or edges should be returned.
   -- @return Returns the child vertices and/or edges of the given parent.
     
   function Get_Child_Vertices
     (M      : access Model_Interface'Class;
      Parent : access Cell_Record'Class) return Cells_Lists.List;
   -- Returns the child vertices of the given parent.
   -- @param model Model that contains the hierarchical information.
   -- @param parent Cell whose child vertices should be returned.
   -- @return Returns the child vertices of the given parent.

   function Get_Child_Edges
     (M      : access Model_Interface'Class;
      Parent : access Cell_Record'Class) return Cells_Lists.List;
   -- Returns the child edges of the given parent.
   -- @param model Model that contains the hierarchical information.
   -- @param parent Cell whose child edges should be returned.
   -- @return Returns the child edges of the given parent.

   function Get_Child_Cells
     (M        : access Model_Interface'Class;
      Parent   : access Cell_Record'Class;
      Vertices : Boolean;
      Edges    : Boolean) return Cells_Lists.List;
   -- Returns the children of the given cell that are vertices and/or edges
   -- depending on the arguments. If both arguments are false then all
   -- children are returned regardless of their type.
   -- @param model Model that contains the hierarchical information.
   -- @param parent Cell whose child vertices or edges should be returned.
   -- @param vertices Boolean indicating if child vertices should be returned.
   -- @param edges Boolean indicating if child edges should be returned.
   -- @return Returns the child vertices and/or edges of the given parent.
     
   function Get_Parents
     (M     : access Model_Interface'Class;
      Cells : Cells_Lists.List) return Cells_Lists.List;
     
   function Filter_Cells
     (M      : access Model_Interface'Class;
      Cells  : Cells_Lists.List;
      Filter : access Filter_Interface'Class) return Cells_Lists.List;
     
   function Get_Descendants
     (M      : access Model_Interface'Class;
      Parent : access Cell_Record'Class) return Cells_Lists.List;
   -- Returns a all descendants of the given cell and the cell itself
   -- as a collection.

   function Filter_Descendants
     (M      : access Model_Interface'Class;
      Filter : access Filter_Interface'Class) return Cells_Lists.List;
   -- Creates a collection of cells using the visitor pattern.

   function Filter_Descendants
     (M      : access Model_Interface'Class;
      Filter : access Filter_Interface'Class;
      Parent : access Cell_Record'Class) return Cells_Lists.List;
   -- Creates a collection of cells using the visitor pattern.
     
   function Get_Topmost_Cells
     (M     : access Model_Interface'Class;
      Cells : Cells_Lists.List) return Cells_Lists.List;
   -- Function: getTopmostCells
   -- Returns the topmost cells of the hierarchy in an array that contains no
   -- desceandants for each <mxCell> that it contains. Duplicates should be
   -- removed in the cells array to improve performance.
   -- Parameters:
   -- cells - Array of <mxCells> whose topmost ancestors should be returned.

   function Value_For_Cell_Changed     
     (M     : access Model_Interface;
      Cell  : access Cell_Record'Class;
      Value : access Object_Record'Class) 
      return access Object_Record'Class is abstract; 
   
   function Geometry_For_Cell_Changed     
     (M        : access Model_Interface;
      Cell     : access Cell_Record'Class;
      Geometry : access Cell_Geometry_Record'Class)
      return access Cell_Geometry_Record'Class is abstract;
   
   function Get_Nearest_Common_Ancestor
     (M     : access Model_Interface;
      Cell1 : access Cell_Record'Class;
      Cell2 : access Cell_Record'Class) 
      return access Cell_Record'Class is abstract;
   
end Artics.Graph.Models_Interfaces;
