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
-- Reflex is originally developed  by the Artics team at Grenoble (France). --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Hash;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Artics.Types; use Artics.Types;
with Artics.Namet; use Artics.Namet;
with Artics.Strings_Hashes; use Artics.Strings_Hashes;
with Artics.Maths; use Artics.Maths;
with Artics.Utils; use Artics.Utils;
with Artics.Output; use Artics.Output;

with Artics.Objects; use Artics.Objects;

with Artics.Geometry; use Artics.Geometry;
with Artics.Geometry.Points; use Artics.Geometry.Points;
with Artics.Geometry.Rectangles; use Artics.Geometry.Rectangles;

with Artics.Graph.Names; 
with Artics.Graph.Graphs_Interfaces; use Artics.Graph.Graphs_Interfaces;
with Artics.Graph.Cells; use Artics.Graph.Cells;
with Artics.Graph.Models_Interfaces; use Artics.Graph.Models_Interfaces;
with Artics.Graph.Views; use Artics.Graph.Views;
with Artics.Graph.Cells_States; use Artics.Graph.Cells_States;
with Artics.Graph.Cells_Geometry; use Artics.Graph.Cells_Geometry;
with Artics.Graph.Cells_Visitor_Interfaces; use Artics.Graph.Cells_Visitor_Interfaces;

with Artics.Graph.Events; use Artics.Graph.Events;
with Artics.Graph.Connection_Constraints; use Artics.Graph.Connection_Constraints;
with Artics.Graph.Multiplicities; use Artics.Graph.Multiplicities;

with Artics.Graph.Selections.Models; use Artics.Graph.Selections.Models;
with Artics.Graph.Selections.Changes; use Artics.Graph.Selections.Changes;
with Artics.Graph.Undoables.Edits; use Artics.Graph.Undoables.Edits;
with Artics.Graph.Undoables.Changes_Interfaces; use Artics.Graph.Undoables.Changes_Interfaces;
with Artics.Graph.Undo_Managers; use Artics.Graph.Undo_Managers;

--  with Artics.Graph.Properties_Changes; use Artics.Graph.Properties_Changes;
--  with Artics.Graph.Properties_Changes_Support; use Artics.Graph.Properties_Changes_Support;

--  with Artics.Fbd.Tree; use Artics.Fbd.Tree;

-- Implements a graph object that allows to create diagrams from a graph model
-- and stylesheet.
-- 
-- <h3>Images</h3>
-- To create an image from a graph, use the following code for a given
-- XML document (doc) and File (file):
-- 
-- <code>
-- Image img = mxCellRenderer.createBufferedImage(
-- 		graph, null, 1, Color.WHITE, false, null);
-- ImageIO.write(img, "png", file);
-- </code>
-- 
-- If the XML is given as a string rather than a document, the document can
-- be obtained using mxUtils.parse.
-- 
-- This class fires the following events:
-- 
-- mxEvent.ROOT fires if the root in the model has changed. This event has no
-- properties.
-- 
-- mxEvent.ALIGN_CELLS fires between begin- and endUpdate in alignCells. The
-- <code>cells</code> and <code>align</code> properties contain the respective
-- arguments that were passed to alignCells.
-- 
-- mxEvent.FLIP_EDGE fires between begin- and endUpdate in flipEdge. The
-- <code>edge</code> property contains the edge passed to flipEdge.
-- 
-- mxEvent.ORDER_CELLS fires between begin- and endUpdate in orderCells. The
-- <code>cells</code> and <code>back</code> properties contain the respective
-- arguments that were passed to orderCells.
--
-- mxEvent.CELLS_ORDERED fires between begin- and endUpdate in cellsOrdered.
-- The <code>cells</code> and <code>back</code> arguments contain the
-- respective arguments that were passed to cellsOrdered.
-- 
-- mxEvent.GROUP_CELLS fires between begin- and endUpdate in groupCells. The
-- <code>group</code>, <code>cells</code> and <code>border</code> arguments
-- contain the respective arguments that were passed to groupCells.
-- 
-- mxEvent.UNGROUP_CELLS fires between begin- and endUpdate in ungroupCells.
-- The <code>cells</code> property contains the array of cells that was passed
-- to ungroupCells.
-- 
-- mxEvent.REMOVE_CELLS_FROM_PARENT fires between begin- and endUpdate in
-- removeCellsFromParent. The <code>cells</code> property contains the array of
-- cells that was passed to removeCellsFromParent.
-- 
-- mxEvent.ADD_CELLS fires between begin- and endUpdate in addCells. The
-- <code>cells</code>, <code>parent</code>, <code>index</code>,
-- <code>source</code> and <code>target</code> properties contain the
-- respective arguments that were passed to addCells.
-- 
-- mxEvent.CELLS_ADDED fires between begin- and endUpdate in cellsAdded. The
-- <code>cells</code>, <code>parent</code>, <code>index</code>,
-- <code>source</code>, <code>target</code> and <code>absolute</code>
-- properties contain the respective arguments that were passed to cellsAdded.
-- 
-- mxEvent.REMOVE_CELLS fires between begin- and endUpdate in removeCells. The
-- <code>cells</code> and <code>includeEdges</code> arguments contain the
-- respective arguments that were passed to removeCells.
-- 
-- mxEvent.CELLS_REMOVED fires between begin- and endUpdate in cellsRemoved.
-- The <code>cells</code> argument contains the array of cells that was
-- removed.
-- 
-- mxEvent.SPLIT_EDGE fires between begin- and endUpdate in splitEdge. The
-- <code>edge</code> property contains the edge to be splitted, the
-- <code>cells</code>, <code>newEdge</code>, <code>dx</code> and
-- <code>dy</code> properties contain the respective arguments that were passed
-- to splitEdge.
-- 
-- mxEvent.TOGGLE_CELLS fires between begin- and endUpdate in toggleCells. The
-- <code>show</code>, <code>cells</code> and <code>includeEdges</code>
-- properties contain the respective arguments that were passed to toggleCells.
-- 
-- mxEvent.FOLD_CELLS fires between begin- and endUpdate in foldCells. The
-- <code>collapse</code>, <code>cells</code> and <code>recurse</code>
-- properties contain the respective arguments that were passed to foldCells.
-- 
-- mxEvent.CELLS_FOLDED fires between begin- and endUpdate in cellsFolded. The
-- <code>collapse</code>, <code>cells</code> and <code>recurse</code>
-- properties contain the respective arguments that were passed to cellsFolded.
-- 
-- mxEvent.UPDATE_CELL_SIZE fires between begin- and endUpdate in
-- updateCellSize. The <code>cell</code> and <code>ignoreChildren</code>
-- properties contain the respective arguments that were passed to
-- updateCellSize.
-- 
-- mxEvent.RESIZE_CELLS fires between begin- and endUpdate in resizeCells. The
-- <code>cells</code> and <code>bounds</code> properties contain the respective
-- arguments that were passed to resizeCells.
-- 
-- mxEvent.CELLS_RESIZED fires between begin- and endUpdate in cellsResized.
-- The <code>cells</code> and <code>bounds</code> properties contain the
-- respective arguments that were passed to cellsResized.
-- 
-- mxEvent.MOVE_CELLS fires between begin- and endUpdate in moveCells. The
-- <code>cells</code>, <code>dx</code>, <code>dy</code>, <code>clone</code>,
-- <code>target</code> and <code>location</code> properties contain the
-- respective arguments that were passed to moveCells.
-- 
-- mxEvent.CELLS_MOVED fires between begin- and endUpdate in cellsMoved. The
-- <code>cells</code>, <code>dx</code>, <code>dy</code> and
-- <code>disconnect</code> properties contain the respective arguments that
-- were passed to cellsMoved.
-- 
-- mxEvent.CONNECT_CELL fires between begin- and endUpdate in connectCell. The
-- <code>edge</code>, <code>terminal</code> and <code>source</code> properties
-- contain the respective arguments that were passed to connectCell.
-- 
-- mxEvent.CELL_CONNECTED fires between begin- and endUpdate in cellConnected.
-- The <code>edge</code>, <code>terminal</code> and <code>source</code>
-- properties contain the respective arguments that were passed to
-- cellConnected.
-- 
-- mxEvent.REPAINT fires if a repaint was requested by calling repaint. The
-- <code>region</code> property contains the optional mxRectangle that was
-- passed to repaint to define the dirty region.

with Artics.Graph.Events.Repaint_Events; use Artics.Graph.Events.Repaint_Events;
with Artics.Graph.Events.Change_Events; use Artics.Graph.Events.Change_Events;

package Artics.Graph.Graphs is
   
   -------===============
   
   type Cell_Visitor_Record is record
      Dummy : Integer;
   end record;
   
   --     type Map_Cells_Strings is record
   --        Dummy : Integer;
   --     end record;
   --     
   --     No_Map_Cells_Strings : constant Map_Cells_Strings := 
   --       Map_Cells_Strings'(Dummy => 0);
   
   -------===============
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   type Graph_Record is 
     new Event_Source_Record and Graph_Interface with private;
   type Graph_Ptr is access all Graph_Record;
   type Graph_Class_Ptr is access all Graph_Record'Class;
   
   No_Graph_Record : constant Graph_Record;
   
   Event_Graph_Exception : exception;
   
   type Full_Repainter_Handler_Record is
     new Object_Record and Listener_Interface with record
      Graph : access Graph_Record;
   end record;
   
   function New_Full_Repainter_Handler
     (G : Graph_Ptr) return access Full_Repainter_Handler_Record;
   procedure Invoke 
     (Listener : access Full_Repainter_Handler_Record;
      Sender   : access Object_Record'Class;
      Evt      : access Graph_Events.Event_Object_Record'Class);
   
   type Update_Origin_Handler_Record is
     new Object_Record and Listener_Interface with record
      Graph : access Graph_Record;
   end record;
   
   function New_Update_Origin_Handler 
     (G : Graph_Ptr) return access Update_Origin_Handler_Record;
   procedure Invoke 
     (Listener : access Update_Origin_Handler_Record;
      Sender   : access Object_Record'Class;
      Evt      : access Graph_Events.Event_Object_Record'Class);
   
   type Graph_Model_Change_Handler_Record is
     new Object_Record and Listener_Interface with record
      Graph : access Graph_Record;
   end record;
   
   function New_Graph_Model_Change_Handler
     (G : Graph_Ptr) return access Graph_Model_Change_Handler_Record;
   procedure Invoke 
     (Listener : access Graph_Model_Change_Handler_Record;
      Sender   : access Object_Record'Class;
      Evt      : access Graph_Events.Event_Object_Record'Class);
   
   function New_Graph return access Graph_Record;
   
   function New_Graph
     (Model : access Model_Interface'Class) return access Graph_Record;
   -- Constructs a new graph for the specified model. If no model is
   -- specified, then a new, empty {@link com.mxgraph.model.mxGraphModel} is
   -- used.
   -- @param model Model that contains the graph data
   
   function Create_Selection_Model
     (G : access Graph_Record) return access Selection_Model_Record'Class;
   -- Constructs A new Selection Model To Be Used in This Graph.

   function Create_Graph_View 
     (G : access Graph_Record) return access View_Record'Class;
   -- Constructs A new View To Be Used in This Graph.
   
   function Get_Model 
     (G : access Graph_Record) return access Model_Interface'Class;
   procedure Set_Model
     (G : access Graph_Record;
      M : access Model_Interface'Class); 
   -- Holds the model that contains the cells to be displayed.

   function Get_View (G : access Graph_Record) return access View_Record'Class;
   procedure Set_View
     (G : access Graph_Record;
      V : access View_Record'Class);
   -- Holds the view that caches the cell states.
   
   function Get_Selection_Cells_For_Changes
     (G       : access Graph_Record;
      Changes : Undoables_Lists.List) return Cells_Lists.List;
   -- Returns the cells to be selected for the given list of changes.
   
   function Graph_Model_Changed
     (G       : access Graph_Record;
      Sender  : access Model_Interface'Class;
      Changes : Undoables_Lists.List) return Rectangle_Record;
   -- Called when the graph model changes. Invokes processChange on each
   -- item of the given array to update the view accordingly.
   
   procedure Update_Origin (G : access Graph_Record);
   -- Extends the canvas by doing another validation with a shifted
   -- global translation if the bounds of the graph are below (0,0).
   -- The first validation is required to compute the bounds of the graph
   -- while the second validation is required to apply the new translate.
   
   function Get_Removed_Cells_For_Changes
     (G       : access Graph_Record;
      Changes : Undoables_Lists.List) return Cells_Lists.List;
   -- Returns the cells that have been removed from the model.

   function Process_Changes
     (G            : access Graph_Record;
      Changes      : Undoables_Lists.List;
      Invalidate   : Boolean;
      Ignore_Dirty : Boolean) return Rectangle_Record;
   -- Processes the changes and returns the minimal rectangle to be
   -- repainted in the buffer. A return value of null means no repaint
   -- is required.
   
   function Process_Change
     (G            : access Graph_Record;
      Change       : access Undoable_Change_Interface'Class;
      Invalidate   : Boolean;
      Ignore_Dirty : Boolean) return Rectangle_Record;
   -- Processes the given change and invalidates the respective cached data
   -- in <view>. This fires a <root> event if the root has changed in the
   -- model.
   
   procedure Remove_State_For_Cell
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class);
   -- Removes all cached information for the given cell and its descendants.
   -- This is called when a cell was removed from the model.
   -- @param cell Cell that was removed from the model.

   function Align_Cells
     (G     : access Graph_Record;
      Align : String) return Cells_Lists.List;
   function Align_Cells
     (G     : access Graph_Record;
      Align : Name_Id) return Cells_Lists.List;
   -- Aligns the selection cells vertically or horizontally according to the
   -- given alignment.
   -- @param align Specifies the alignment. Possible values are all constants
   -- in mxConstants with an ALIGN prefix.

   function Align_Cells
     (G     : access Graph_Record;
      Align : String;
      Cells : Cells_Lists.List) return Cells_Lists.List;
   function Align_Cells
     (G     : access Graph_Record;
      Align : Name_Id;
      Cells : Cells_Lists.List) return Cells_Lists.List;
   -- Aligns the given cells vertically or horizontally according to the given
   -- alignment.
   -- @param align Specifies the alignment. Possible values are all constants
   -- in mxConstants with an ALIGN prefix.
   -- @param cells Array of cells to be aligned.

   function Align_Cells
     (G     : access Graph_Record;
      Align : String;
      Cells : Cells_Lists.List;
      Param : Coordinate) return Cells_Lists.List;
   function Align_Cells
     (G     : access Graph_Record;
      Align : Name_Id;
      Cells : Cells_Lists.List;
      Param : Coordinate) return Cells_Lists.List;
   -- Aligns the given cells vertically or horizontally according to the given
   -- alignment using the optional parameter as the coordinate.
   -- @param align Specifies the alignment. Possible values are all constants
   -- in mxConstants with an ALIGN prefix.
   -- @param cells Array of cells to be aligned.
   -- @param param Optional coordinate for the alignment.
   
   function Flip_Edge
     (G    : access Graph_Record;
      Edge : access Cell_Record'Class) return access Cell_Record'Class;
   -- Called when the main control point of the edge is double-clicked. This
   -- and resets the edges control points. Finally, a flip event is fired
   -- before endUpdate is called on the model.
   -- @param edge Cell that represents the edge to be flipped.
   -- @return Returns the edge that has been flipped.
   
   function Order_Cells
     (G     : access Graph_Record;
      Back  : Boolean;
      Cells : Cells_Lists.List := Cells_Lists.Empty_List)
      return Cells_Lists.List;
   -- Moves the given cells to the front or back. The change is carried out
   -- using cellsOrdered. This method fires mxEvent.ORDER_CELLS while the
   -- transaction is in progress.
   -- @param back Specifies if the cells should be moved to back.
   -- @param cells Array of cells whose order should be changed. If null is
   -- specified then the selection cells are used.
   
   procedure Cells_Ordered
     (G     : access Graph_Record;
      Cells : Cells_Lists.List;
      Back  : Boolean);
   -- Moves the given cells to the front or back. This method fires
   -- mxEvent.CELLS_ORDERED while the transaction is in progress.
   -- @param cells Array of cells whose order should be changed.
   -- @param back Specifies if the cells should be moved to back.
   
   function Group_Cells
     (G      : access Graph_Record;
      Group  : access Cell_Record'Class := null;
      Border : Coordinate := 0.0;
      Cells  : Cells_Lists.List := Cells_Lists.Empty_List)  
      return access Cell_Record'Class;
   -- Adds the cells into the given group. The change is carried out using
   -- cellsAdded, cellsMoved and cellsResized. This method fires
   -- mxEvent.GROUP_CELLS while the transaction is in progress. Returns the
   -- new group. A group is only created if there is at least one entry in the
   -- given array of cells.
   -- @param group Cell that represents the target group. If null is specified
   -- then a new group is created using createGroupCell.
   -- @param border Integer that specifies the border between the child area
   -- and the group bounds.
   -- @param cells Optional array of cells to be grouped. If null is specified
   -- then the selection cells are used.
   
   function Get_Cells_For_Group 
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) return Cells_Lists.List;
   -- Returns the cells with the same parent as the first cell
   -- in the given array.
   
   function Get_Bounds_For_Group
     (G        : access Graph_Record;
      Group    : access Cell_Record'Class;
      Children : Cells_Lists.List;
      Border   : Coordinate) return Rectangle_Record;
   -- Returns the bounds to be used for the given group and children. This
   -- implementation computes the bounding box of the geometries of all
   -- vertices in the given children array. Edges are ignored. If the group
   -- cell is a swimlane the title region is added to the bounds.
   
   function Create_Group_Cell
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) return access Cell_Record'Class;
   -- Hook for creating the group cell to hold the given array of <mxCells> if
   -- no group cell was given to the <group> function. The children are just
   -- for informational purpose, they will be added to the returned group
   -- later. Note that the returned group should have a geometry. The
   -- coordinates of which are later overridden.
   -- @param cells
   -- @return Returns a new group cell.
   
   function Ungroup_Cells
     (G     : access Graph_Record;
      Cells : Cells_Lists.List := Cells_Lists.Empty_List) 
      return Cells_Lists.List;
   -- Ungroups the given cells by moving the children the children to their
   -- parents parent and removing the empty groups.
   -- @param cells Array of cells to be ungrouped. If null is specified then
   -- the selection cells are used.
   -- @return Returns the children that have been removed from the groups.
   
   procedure Remove_Cells_From_Parent
     (G     : access Graph_Record;
      Cells : Cells_Lists.List := Cells_Lists.Empty_List);
   -- Removes the specified cells from their parents and adds them to the
   -- default parent.
   -- @param cells Array of cells to be removed from their parents.
   -- @return Returns the cells that were removed from their parents.
   
   procedure Update_Group_Bounds
     (G           : access Graph_Record;
      Cells       : Cells_Lists.List := Cells_Lists.Empty_List;
      Border      : Coordinate := 0.0;
      Move_Parent : Boolean := False);
   -- Updates the bounds of the given array of groups so that it includes
   -- all child vertices.
   -- @param cells The groups whose bounds should be updated.
   -- @param border The border to be added in the group.
   -- @param moveParent Specifies if the group should be moved.
   
   --
   -- Cell cloning, insertion and removal
   --
   
   function Clone_Cells
     (G                   : access Graph_Record;
      Cells               : Cells_Lists.List;
      Allow_Invalid_Edges : Boolean := True) return Cells_Lists.List;
   -- Returns the clones for the given cells. If the terminal of an edge is
   -- not in the given array, then the respective end is assigned a terminal
   -- point and the terminal is removed. If a cloned edge is invalid and
   -- allowInvalidEdges is false, then a null pointer will be at this position
   -- in the returned array. Use getCloneableCells on the input array to only
   -- clone the cells where isCellCloneable returns true.
   -- @param cells Array of mxCells to be cloned.
   -- @return Returns the clones of the given cells.
   
   function Insert_Vertex
     (G        : access Graph_Record;
      Parent   : access Cell_Record'Class;
      Id       : String;
      Value    : access Object_Record'Class;
      X        : Coordinate;
      Y        : Coordinate;
      Width    : Coordinate;
      Height   : Coordinate;
      Relative : Boolean := False) return access Cell_Record'Class;
   -- Adds a new vertex into the given parent using value as the user object
   -- and the given coordinates as the geometry of the new vertex. The id and
   -- @param parent Cell that specifies the parent of the new vertex.
   -- @param id Optional string that defines the Id of the new vertex.
   -- @param value Object to be used as the user object.
   -- @param x Integer that defines the x coordinate of the vertex.
   -- @param y Integer that defines the y coordinate of the vertex.
   -- @param width Integer that defines the width of the vertex.
   -- @param height Integer that defines the height of the vertex.
   -- @param relative Specifies if the geometry should be relative.
   -- @return Returns the new vertex that has been inserted.

   function Create_Vertex
     (G        : access Graph_Record;
      Parent   : access Cell_Record'Class;
      Id       : String;
      Value    : access Object_Record'Class;
      X        : Coordinate;
      Y        : Coordinate;
      Width    : Coordinate;
      Height   : Coordinate;
      Relative : Boolean := False) return access Cell_Record'Class;
   -- Hook method that creates the new vertex for insertVertex.
   -- @param parent Cell that specifies the parent of the new vertex.
   -- @param id Optional string that defines the Id of the new vertex.
   -- @param value Object to be used as the user object.
   -- @param x Integer that defines the x coordinate of the vertex.
   -- @param y Integer that defines the y coordinate of the vertex.
   -- @param width Integer that defines the width of the vertex.
   -- @param height Integer that defines the height of the vertex.
   -- @param relative Specifies if the geometry should be relative.
   -- @return Returns the new vertex to be inserted.
   
   function Insert_Edge
     (G      : access Graph_Record;
      Parent : access Cell_Record'Class;
      Id     : String;
      Value  : access Object_Record'Class;
      Source : access Cell_Record'Class;
      Target : access Cell_Record'Class) return access Cell_Record'Class;
   -- Adds a new edge into the given parent using value as the user object and
   -- the given source and target as the terminals of the new edge. The Id and
   -- @param parent Cell that specifies the parent of the new edge.
   -- @param id Optional string that defines the Id of the new edge.
   -- @param value Object to be used as the user object.
   -- @param source Cell that defines the source of the edge.
   -- @param target Cell that defines the target of the edge.
   -- @return Returns the new edge that has been inserted.
   
   function Create_Edge
     (G     : access Graph_Record;
      Id    : String;
      Value : access Object_Record'Class) return access Cell_Record'Class;
   -- Hook method that creates the new edge for insertEdge. This
   -- implementation does not set the source and target of the edge, these
   -- are set when the edge is added to the model.
   -- @param parent Cell that specifies the parent of the new edge.
   -- @param id Optional string that defines the Id of the new edge.
   -- @param value Object to be used as the user object.
   -- @param source Cell that defines the source of the edge.
   -- @param target Cell that defines the target of the edge.
   -- @return Returns the new edge to be inserted.
   
   procedure Add_Edge
     (G      : access Graph_Record;
      Edge   : access Cell_Record'Class;
      Parent : access Cell_Record'Class;
      Source : access Cell_Record'Class;
      Target : access Cell_Record'Class;
      Index  : Integer);
   -- Adds the edge to the parent and connects it to the given source and
   -- target terminals. This is a shortcut method.
   -- @param edge Edge to be inserted into the given parent.
   -- @param parent Object that represents the new parent. If no parent is
   -- given then the default parent is used.
   -- @param source Optional cell that represents the source terminal.
   -- @param target Optional cell that represents the target terminal.
   -- @param index Optional index to insert the cells at. Default is to append.
   -- @return Returns the edge that was added.
   
   procedure Add_Cell
     (G      : access Graph_Record;
      Cell   : access Cell_Record'Class;
      Parent : access Cell_Record'Class := null;
      Index  : Integer := 0;
      Source : access Cell_Record'Class := null;
      Target : access Cell_Record'Class := null);
   -- Adds the cell to the parent and connects it to the given source and
   -- target terminals. This is a shortcut method.
   -- @param cell Cell to be inserted into the given parent.
   -- @param parent Object that represents the new parent. If no parent is
   -- given then the default parent is used.
   -- @param index Optional index to insert the cells at. Default is to append.
   -- @param source Optional cell that represents the source terminal.
   -- @param target Optional cell that represents the target terminal.
   -- @return Returns the cell that was added.
   
   procedure Add_Cells
     (G      : access Graph_Record;
      Cells  : Cells_Lists.List;
      Parent : access Cell_Record'Class := null;
      Index  : Integer := 0;
      Source : access Cell_Record'Class := null;
      Target : access Cell_Record'Class := null);
   -- Adds the cells to the parent at the given index, connecting each cell to
   -- the optional source and target terminal. The change is carried out using
   -- cellsAdded. This method fires mxEvent.ADD_CELLS while the transaction
   -- is in progress.
   -- @param cells Array of cells to be added.
   -- @param parent Optional cell that represents the new parent. If no parent
   -- is specified then the default parent is used.
   -- @param index Optional index to insert the cells at. Default is to append.
   -- @param source Optional source terminal for all inserted cells.
   -- @param target Optional target terminal for all inserted cells.
   -- @return Returns the cells that were added.
   
   procedure Cells_Added
     (G         : access Graph_Record;
      Cells     : Cells_Lists.List;
      Parent    : access Cell_Record'Class;
      Index     : Integer;
      Source    : access Cell_Record'Class;
      Target    : access Cell_Record'Class;
      Absolute  : Boolean;
      Constrain : Boolean := True);
   -- Adds the specified cells to the given parent. This method fires
   -- mxEvent.CELLS_ADDED while the transaction is in progress.
   
   procedure Remove_Cells
     (G             : access Graph_Record;
      Cells         : Cells_Lists.List := Cells_Lists.Empty_List;
      Include_Edges : Boolean := True);
   -- Removes the given cells from the graph including all connected edges if
   -- includeEdges is true. The change is carried out using cellsRemoved. This
   -- method fires mxEvent.REMOVE_CELLS while the transaction is in progress.
   -- @param cells Array of cells to remove. If null is specified then the
   -- selection cells which are deletable are used.
   -- @param includeEdges Specifies if all connected edges should be removed as
   -- well.
   
   procedure Cells_Removed
     (G     : access Graph_Record;
      Cells : Cells_Lists.List);
   -- Removes the given cells from the model. This method fires
   -- mxEvent.CELLS_REMOVED while the transaction is in progress.
   -- @param cells Array of cells to remove.
   
   procedure Split_Edge 
     (G        : access Graph_Record;
      Edge     : access Cell_Record'Class;
      Cells    : Cells_Lists.List);
   procedure Split_Edge 
     (G        : access Graph_Record;
      Edge     : access Cell_Record'Class;
      Cells    : Cells_Lists.List;
      Dx       : Coordinate;
      Dy       : Coordinate);
   procedure Split_Edge 
     (G        : access Graph_Record;
      Edge     : access Cell_Record'Class;
      Cells    : Cells_Lists.List;
      New_Edge : access Cell_Record'Class;
      Dx       : Coordinate;
      Dy       : Coordinate);
   -- Splits the given edge by adding a newEdge between the previous source
   -- and the given cell and reconnecting the source of the given edge to the
   -- given cell. Fires mxEvent.SPLIT_EDGE while the transaction is in
   -- progress.
   -- @param edge Object that represents the edge to be splitted.
   -- @param cells Array that contains the cells to insert into the edge.
   -- @param newEdge Object that represents the edge to be inserted.
   -- @return Returns the new edge that has been inserted.
   
   procedure Swap_Bounds
     (G             : access Graph_Record;
      Cell          : access Cell_Record'Class);
   -- Swaps the alternate and the actual bounds in the geometry of the given
   -- cell invoking updateAlternateBounds before carrying out the swap.
   -- @param cell Cell for which the bounds should be swapped.
   -- @param willCollapse Boolean indicating if the cell is going to be 
   -- collapsed.
   
   procedure Update_Alternate_Bounds
     (G             : access Graph_Record;
      Cell          : access Cell_Record'Class;
      Geo           : access Cell_Geometry_Record'Class);
   -- Updates or sets the alternate bounds in the given geometry for the given
   -- cell depending on whether the cell is going to be collapsed. If no
   -- alternate bounds are defined in the geometry and
   -- collapseToPreferredSize is true, then the preferred size is used for
   -- the alternate bounds. The top, left corner is always kept at the same
   -- location.
   -- @param cell Cell for which the geometry is being udpated.
   -- @param geo Geometry for which the alternate bounds should be updated.
   -- @param willCollapse Boolean indicating if the cell is going to be 
   -- collapsed.
   
   function Add_All_Edges
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) return Cells_Lists.List;
   -- Returns an array with the given cells and all edges that are connected
   -- to a cell or one of its descendants.
   
   function Get_All_Edges
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) return Cells_Lists.List;
   -- Returns all edges connected to the given cells or their descendants.
   
   --
   -- Cell sizing
   --
   
   procedure Update_Cell_Size
     (G               : access Graph_Record;
      Cell            : access Cell_Record'Class;
      Ignore_Children : Boolean := False);
   -- Updates the size of the given cell in the model using
   -- getPreferredSizeForCell to get the new size. This function
   -- fires mxEvent.UPDATE_CELL_SIZE.
   -- @param cell Cell for which the size should be changed.
   
   procedure Cell_Size_Updated
     (G               : access Graph_Record;
      Cell            : access Cell_Record'Class;
      Ignore_Children : Boolean);
   -- Updates the size of the given cell in the model using
   -- getPreferredSizeForCell to get the new size.
   -- @param cell Cell for which the size should be changed.
   
   function Get_Preferred_Size_For_Cell
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Rectangle_Record;
   -- Returns the preferred width and height of the given <mxCell> as an
   -- <mxRectangle>.
   -- @param cell <mxCell> for which the preferred size should be returned.
   
   function Resize_Cell
     (G      : access Graph_Record;
      Cell   : access Cell_Record'Class;
      Bounds : Rectangle_Record) return access Cell_Record'Class;
   -- Sets the bounds of the given cell using resizeCells. Returns the
   -- cell which was passed to the function.
   -- @param cell <mxCell> whose bounds should be changed.
   -- @param bounds <mxRectangle> that represents the new bounds.
   
   function Resize_Cells
     (G      : access Graph_Record;
      Cells  : Cells_Lists.List;
      Bounds : Rectangles_Lists.List) return Cells_Lists.List;
   -- Sets the bounds of the given cells and fires a mxEvent.RESIZE_CELLS
   -- event. while the transaction is in progress. Returns the cells which
   -- have been passed to the function.
   -- @param cells Array of cells whose bounds should be changed.
   -- @param bounds Array of rectangles that represents the new bounds.

   procedure Cells_Resized
     (G      : access Graph_Record;
      Cells  : Cells_Lists.List;
      Bounds : Rectangles_Lists.List);
   -- Sets the bounds of the given cells and fires a <mxEvent.CELLS_RESIZED>
   -- event. If extendParents is true, then the parent is extended if a child
   -- size is changed so that it overlaps with the parent.
   -- @param cells Array of <mxCells> whose bounds should be changed.
   -- @param bounds Array of <mxRectangles> that represents the new bounds.
   
   procedure Extend_Parent
     (G      : access Graph_Record;
      Cell   : access Cell_Record'Class);
   -- Resizes the parents recursively so that they contain the complete area
   -- of the resized child cell.
   -- @param cell <mxCell> that has been resized.
   
   function Move_Cells
     (G        : access Graph_Record;
      Cells    : Cells_Lists.List;
      Dx       : Coordinate;
      Dy       : Coordinate;
      Clone    : Boolean := False;
      Target   : access Cell_Record'Class := null;
      Location : Point_Record := No_Point_Record) return Cells_Lists.List;
   -- Moves or clones the specified cells and moves the cells or clones by the
   -- given amount, adding them to the optional target cell. The location is
   -- the position of the mouse pointer as the mouse was released. The change
   -- is carried out using cellsMoved. This method fires mxEvent.MOVE_CELLS
   -- while the transaction is in progress.
   -- @param cells Array of cells to be moved, cloned or added to the target.
   -- @param dx Integer that specifies the x-coordinate of the vector.
   -- @param dy Integer that specifies the y-coordinate of the vector.
   -- @param clone Boolean indicating if the cells should be cloned.
   -- @param target Cell that represents the new parent of the cells.
   -- @param location Location where the mouse was released.
   -- @return Returns the cells that were moved.
   
   procedure Cells_Moved
     (G          : access Graph_Record;
      Cells      : Cells_Lists.List;
      Dx         : Coordinate;
      Dy         : Coordinate;
      Disconnect : Boolean;
      Constrain  : Boolean);
   -- Moves the specified cells by the given vector, disconnecting the cells
   -- using disconnectGraph if disconnect is true. This method fires
   -- mxEvent.CELLS_MOVED while the transaction is in progress.
   
   procedure Translate_Cell
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class;
      Dx   : Coordinate;
      Dy   : Coordinate);
   -- Translates the geometry of the given cell and stores the new,
   -- translated geometry in the model as an atomic change.
   
   function Get_Cell_Containment_Area
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Rectangle_Record;
   -- Returns the mxRectangle inside which a cell is to be kept.
   
   function Get_Maximum_Graph_Bounds
     (G : access Graph_Record) return Rectangle_Record;
   -- @return the maximumGraphBounds

   procedure Set_Maximum_Graph_Bounds
     (G     : access Graph_Record;
      Value : Rectangle_Record);
   -- @param value the maximumGraphBounds to set
   
   procedure Constrain_Child
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class);
   -- Keeps the given cell inside the bounds returned by
   -- getCellContainmentArea for its parent, according to the rules defined by
   -- getOverlap and isConstrainChild. This modifies the cell's geometry
   -- in-place and does not clone it.
   -- @param cell Cell which should be constrained.
   
   procedure Reset_Edges
     (G     : access Graph_Record;
      Cells : Cells_Lists.List);
   -- Resets the control points of the edges that are connected to the given
   -- cells if not both ends of the edge are in the given cells array.
   -- @param cells Array of mxCells for which the connected edges should be
   -- reset.
   
   procedure Reset_Edge
     (G    : access Graph_Record;
      Edge : access Cell_Record'Class);
   -- Resets the control points of the given edge.
   
   --
   -- Cell connecting and connection constraints
   --
   
   function Get_All_Connection_Constraints
     (G        : access Graph_Record;
      Terminal : access Cell_Record'Class;
      Source   : Boolean) return access Connection_Constraint_Record'Class;
   -- Returns an array of all constraints for the given terminal.
   -- @param terminal Cell state that represents the terminal.
   -- @param source Specifies if the terminal is the source or target.
   
   function Get_Connection_Constraint
     (G        : access Graph_Record;
      Edge     : access Cell_State_Record'Class;
      Terminal : access Cell_State_Record'Class;
      Source   : Boolean) return access Connection_Constraint_Record'Class;
   -- Returns an connection constraint that describes the given connection
   -- point. This result can then be passed to getConnectionPoint.
   -- @param edge Cell state that represents the edge.
   -- @param terminal Cell state that represents the terminal.
   -- @param source Boolean indicating if the terminal is the source or target.
   
   procedure Set_Connection_Constraint
     (G          : access Graph_Record;
      Edge       : access Cell_Record'Class;
      Terminal   : access Cell_Record'Class;
      Source     : Boolean;
      Constraint : access Connection_Constraint_Record'Class);
   -- Sets the connection constraint that describes the given connection point.
   -- If no constraint is given then nothing is changed. To remove an existing
   -- constraint from the given edge, use an empty constraint instead.
   -- @param edge Cell that represents the edge.
   -- @param terminal Cell that represents the terminal.
   -- @param source Boolean indicating if the terminal is the source or target.
   -- @param constraint Optional connection constraint to be used for this
   -- connection.
   
   function Get_Connection_Point
     (G          : access Graph_Record;
      Vertex     : access Cell_State_Record'Class;
      Constraint : access Connection_Constraint_Record'Class) 
      return Point_Record;
   -- Sets the connection constraint that describes the given connection point.
   -- If no constraint is given then nothing is changed. To remove an existing
   -- constraint from the given edge, use an empty constraint instead.
   -- @param vertex Cell state that represents the vertex.
   -- @param constraint Connection constraint that represents the connection 
   -- point constraint as returned by getConnectionConstraint.
   
   function Connect_Cell
     (G        : access Graph_Record;
      Edge     : access Cell_Record'Class;
      Terminal : access Cell_Record'Class;
      Source   : Boolean) return access Cell_Record'Class;
   -- Connects the specified end of the given edge to the given terminal
   -- using cellConnected and fires mxEvent.CONNECT_CELL while the transaction
   -- is in progress.
   
   function Connect_Cell
     (G          : access Graph_Record;
      Edge       : access Cell_Record'Class;
      Terminal   : access Cell_Record'Class;
      Source     : Boolean;
      Constraint : access Connection_Constraint_Record'Class) 
      return access Cell_Record'Class;
   -- Connects the specified end of the given edge to the given terminal
   -- using cellConnected and fires mxEvent.CONNECT_CELL while the transaction
   -- is in progress.
   -- @param edge Edge whose terminal should be updated.
   -- @param terminal New terminal to be used.
   -- @param source Specifies if the new terminal is the source or target.
   -- @param constraint Optional constraint to be used for this connection.
   -- @return Returns the update edge.
   
   procedure Cell_Connected
     (G          : access Graph_Record;
      Edge       : access Cell_Record'Class;
      Terminal   : access Cell_Record'Class;
      Source     : Boolean;
      Constraint : access Connection_Constraint_Record'Class);
   -- Sets the new terminal for the given edge and resets the edge points if
   -- isResetEdgesOnConnect returns true. This method fires
   -- <mxEvent.CELL_CONNECTED> while the transaction is in progress.
   -- @param edge Edge whose terminal should be updated.
   -- @param terminal New terminal to be used.
   -- @param source Specifies if the new terminal is the source or target.
   -- @param constraint Constraint to be used for this connection.
   
   procedure Disconnect_Graph
     (G     : access Graph_Record;
      Cells : Cells_Lists.List);
   -- Disconnects the given edges from the terminals which are not in the
   -- given array.
   -- @param cells Array of <mxCells> to be disconnected.
   
   --  Drilldown --
   ----------------
   
   function Get_Current_Root
     (G : access Graph_Record) return access Cell_Record'Class;
   -- Returns the current root of the displayed cell hierarchy. This is a
   -- shortcut to <mxGraphView.currentRoot> in <view>.
   -- @return Returns the current root in the view.
   
   function Get_Translate_For_Root
     (G : access Graph_Record;
      C : access Cell_Record'Class) return Point_Record;
   -- Returns the translation to be used if the given cell is the root cell as
   -- an <mxPoint>. This implementation returns null.
   -- @param cell Cell that represents the root of the view.
   -- @return Returns the translation of the graph for the given root cell.
   
   function Is_Port 
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean;
   -- Returns true if the given cell is a "port", that is, when connecting to
   -- it, the cell returned by getTerminalForPort should be used as the
   -- terminal and the port should be referenced by the ID in either the
   -- This implementation always returns false.
   -- A typical implementation of this method looks as follows:
   -- <code>
   -- public boolean isPort(Object cell)
   -- {
   --   mxGeometry geo = getCellGeometry(cell);
   --   
   --   return (geo != null) ? geo.isRelative() : false;
   -- }
   -- </code>
   -- @param cell Cell that represents the port.
   -- @return Returns true if the cell is a port.
   
   function Get_Terminal_For_Port
     (G      : access Graph_Record;
      Cell   : access Cell_Record'Class;
      Source : Boolean) return access Cell_Record'Class;
   -- Returns the terminal to be used for a given port. This implementation
   -- always returns the parent cell.
   -- @param cell Cell that represents the port.
   -- @param source If the cell is the source or target port.
   -- @return Returns the terminal to be used for the given port.
   
   function Get_Child_Offset_For_Cell
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Point_Record;
   -- Returns the offset to be used for the cells inside the given cell. The
   -- root and layer cells may be identified using mxGraphModel.isRoot and
   -- mxGraphModel.isLayer. This implementation returns null.
   -- @param cell Cell whose offset should be returned.
   -- @return Returns the child offset for the given cell.
   
   procedure Enter_Group
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class := null);
   -- Uses the given cell as the root of the displayed cell hierarchy. If no
   -- cell is specified then the selection cell is used. The cell is only used
   -- if <isValidRoot> returns true.
   -- @param cell
   
   procedure Exit_Group (G : access Graph_Record);
   -- Changes the current root to the next valid root in the displayed cell
   -- hierarchy.
   
   procedure Home (G : access Graph_Record);
   -- Uses the root of the model as the root of the displayed cell hierarchy
   -- and selects the previous root.
   
   function Is_Valid_Root
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean;
   -- Returns true if the given cell is a valid root for the cell display
   -- hierarchy. This implementation returns true for all non-null values.
   -- @param cell <mxCell> which should be checked as a possible root.
   -- @return Returns true if the given cell is a valid root.

   -- Graph display --
   -------------------
   
   function Get_Graph_Bounds (G : access Graph_Record)return Rectangle_Record;
   -- Returns the bounds of the visible graph.
   
   function Get_Cell_Bounds
     (G                   : access Graph_Record;
      Cell                : access Cell_Record'Class;
      Include_Edges       : Boolean := False;
      Include_Descendants : Boolean := False) return Rectangle_Record;
   -- Returns the bounds of the given cell including all connected edges
   -- if includeEdge is true.
   
   function Get_Bounding_Box_From_Geometry
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) return Rectangle_Record;
   -- Returns the bounding box for the geometries of the vertices in the
   -- given array of cells.
   
   function Get_Bounding_Box
     (G                   : access Graph_Record;
      Cell                : access Cell_Record'Class;
      Include_Edges       : Boolean := False;
      Include_Descendants : Boolean := False) return Rectangle_Record;
   -- Returns the bounding box of the given cell including all connected edges
   -- if includeEdge is true.
   
   function Get_Paint_Bounds
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) return Rectangle_Record;
   -- Returns the bounding box of the given cells and their descendants.
   
   function Get_Bounds_For_Cells
     (G                   : access Graph_Record;
      Cells               : Cells_Lists.List;
      Include_Edges       : Boolean;
      Include_Descendants : Boolean;
      Bounding_Box        : Boolean) return Rectangle_Record;
   -- Returns the bounds for the given cells.
   
   function Get_Cell_Bounds
     (G                   : access Graph_Record;
      Cell                : access Cell_Record'Class;
      Include_Edges       : Boolean;
      Include_Descendants : Boolean;
      Bounding_Box        : Boolean) return Rectangle_Record;
   -- Returns the bounds of the given cell including all connected edges
   -- if includeEdge is true.
   
   procedure Refresh (G : access Graph_Record);
   -- Clears all cell states or the states for the hierarchy starting at the
   -- given cell and validates the graph.
   
   procedure Repaint (G : access Graph_Record);
   -- Fires a repaint event.

   procedure Repaint 
     (G      : access Graph_Record;
      Region : Rectangle_Record);
   -- Fires a repaint event. The optional region is the rectangle that needs
   -- to be repainted.
   
   function Snap 
     (G     : access Graph_Record;
      Value : Coordinate) return Coordinate;
   -- Snaps the given numeric value to the grid if <gridEnabled> is true.
   -- @param value Numeric value to be snapped to the grid.
   -- @return Returns the value aligned to the grid.
   
   function Get_Cell_Geometry
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) 
      return access Cell_Geometry_Record'Class;
   -- Returns the geometry for the given cell.
   -- @param cell Cell whose geometry should be returned.
   -- @return Returns the geometry of the cell.
   
   function Is_Orthogonal
     (G    : access Graph_Record;
      Edge : access Cell_State_Record'Class) return Boolean;
   -- Returns true if perimeter points should be computed such that the
   -- resulting edge has only horizontal or vertical segments.
   -- @param edge Cell state that represents the edge.
   
   function Is_Loop
     (G     : access Graph_Record;
      State : access Cell_State_Record'Class) return Boolean;
   -- Returns true if the given cell state is a loop.
   -- @param state <mxCellState> that represents a potential loop.
   -- @return Returns true if the given cell is a loop.

   -- Cell validation --
   ---------------------
   
   procedure Set_Multiplicities
     (G     : access Graph_Record;
      Value : Multiplicties_Lists.List);
   
   function Get_Multiplicities 
     (G : access Graph_Record) return Multiplicties_Lists.List;
   
   function Is_Edge_Valid
     (G      : access Graph_Record;
      Edge   : access Cell_Record'Class;
      Source : access Cell_Record'Class;
      Target : access Cell_Record'Class) return Boolean;
   -- Checks if the return value of getEdgeValidationError for the given
   -- arguments is null.
   -- @param edge Cell that represents the edge to validate.
   -- @param source Cell that represents the source terminal.
   -- @param target Cell that represents the target terminal.
   
   function Get_Edge_Validation_Error
     (G      : access Graph_Record;
      Edge   : access Cell_Record'Class;
      Source : access Cell_Record'Class;
      Target : access Cell_Record'Class) return String;
   -- Returns the validation error message to be displayed when inserting or
   -- changing an edges' connectivity. A return value of null means the edge
   -- is valid, a return value of '' means it's not valid, but do not display
   -- an error message. Any other (non-empty) string returned from this method
   -- is displayed as an error message when trying to connect an edge to a
   -- source and target. This implementation uses the multiplicities, as
   -- well as multigraph and allowDanglingEdges to generate validation
   -- errors.
   -- @param edge Cell that represents the edge to validate.
   -- @param source Cell that represents the source terminal.
   -- @param target Cell that represents the target terminal.
   
   function Validate_Edge
     (G      : access Graph_Record;
      Edge   : access Cell_Record'Class;
      Source : access Cell_Record'Class;
      Target : access Cell_Record'Class) return String;
   -- Hook method for subclassers to return an error message for the given
   -- edge and terminals. This implementation returns null.
   -- @param edge Cell that represents the edge to validate.
   -- @param source Cell that represents the source terminal.
   -- @param target Cell that represents the target terminal.
   
   function Get_Cell_Validation_Error
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return String;
   -- Checks all multiplicities that cannot be enforced while the graph is
   -- being modified, namely, all multiplicities that require a minimum of
   -- 1 edge.
   -- @param cell Cell for which the multiplicities should be checked.
   
   function Validate_Cell
     (G       : access Graph_Record;
      Cell    : access Cell_Record'Class;
      Context : Cells_Maps.Map) return String;
   -- Hook method for subclassers to return an error message for the given
   -- cell and validation context. This implementation returns null.
   -- @param cell Cell that represents the cell to validate.
   -- @param context Hashtable that represents the global validation state.
   
   function Is_Labels_Visible (G : access Graph_Record) return Boolean;
   procedure Set_Labels_Visible
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies if labels should be visible. This is used in getLabel. 
   -- Default is true.
   
   function Is_Html_Labels (G : access Graph_Record) return Boolean;
   procedure Set_Html_Labels
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies the return value for isHtmlLabel. Default is false.
   
   function Convert_Value_To_String
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return String;
   function Convert_Value_To_Name
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Name_Id;
   -- Returns the textual representation for the given cell.
   -- @param cell Cell to be converted to a string.
   -- @return Returns the textual representation of the cell.
   
   function Get_Label 
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return String;
   function Get_Label 
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Name_Id;
   -- Returns a string or DOM node that represents the label for the given
   -- cell. This implementation uses <convertValueToString> if <labelsVisible>
   -- is true. Otherwise it returns an empty string.
   -- @param cell <mxCell> whose label should be returned.
   -- @return Returns the label for the given cell.
   
   procedure Cell_Label_Changed
     (G         : access Graph_Record;
      Cell      : access Cell_Record'Class;
      Value     : access Object_Record'Class;
      Auto_Size : Boolean);
   -- Sets the new label for a cell. If autoSize is true then <cellSizeUpdated>
   -- will be called.
   -- @param cell Cell whose label should be changed.
   -- @param value New label to be assigned.
   -- @param autoSize Specifies if cellSizeUpdated should be called.
   
   function Is_Html_Label
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean;
   -- Returns true if the label must be rendered as HTML markup. The default
   -- implementation returns <htmlLabels>.
   -- @param cell <mxCell> whose label should be displayed as HTML markup.
   -- @return Returns true if the given cell label is HTML markup.

   function Get_Tooltip_For_Cell
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return String;
   -- Returns the tooltip to be used for the given cell.

   function Get_Start_Size
     (G         : access Graph_Record;
      Swim_Lane : access Cell_Record'Class)
      return Rectangle_Record;
   -- Returns the start size of the given swimlane, that is, the width or
   -- height of the part that contains the title, depending on the
   -- horizontal style. The return value is an <mxRectangle> with either
   -- width or height set as appropriate.
   -- @param swimlane <mxCell> whose start size should be returned.
   -- @return Returns the startsize for the given swimlane.

   function Get_Image
     (G     : access Graph_Record;
      State : access Cell_State_Record'Class) return String;
   -- Returns the image URL for the given cell state. This implementation
   -- returns the value stored under <mxConstants.STYLE_IMAGE> in the cell
   -- style.
   -- @param state
   -- @return Returns the image associated with the given cell state.
   ----jma
   function Get_Border (G : access Graph_Record) return Integer;
   procedure Set_Border
     (G : access Graph_Record;
      V : Integer);
   -- Border to be added to the bottom and right side when the container is
   -- being resized after the graph has been changed. Default is 0.

   function Is_Swimlane
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean;
   -- Returns true if the given cell is a swimlane. This implementation always
   -- returns false.
   -- @param cell Cell that should be checked. 
   -- @return Returns true if the cell is a swimlane.
   
   
   --- A partir de la il faut changer
   -----------------------------------------------------------
   
   
   
   
   
   
   
   
   
   
   
   -- A couper Ici 
   ------------------------------------------
   
   --
   -- Cells and labels control options
   --

   function Is_Cell_Locked
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean;
   -- Returns true if the given cell may not be moved, sized, bended,
   -- disconnected, edited or selected. This implementation returns true for
   -- all vertices with a relative geometry if cellsLocked is false.
   -- @param cell Cell whose locked state should be returned.
   -- @return Returns true if the given cell is locked.
   
   function Is_Cells_Locked (G : access Graph_Record) return Boolean;
   procedure Set_Cells_Locked
     (G : access Graph_Record;
      L : Boolean);
   -- Specifies the return value for isCell(s)Locked. Default is false.
      
   function Is_Cell_Editable
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean;
   -- Returns true if the given cell is movable. This implementation returns 
   -- editable.
   -- @param cell Cell whose editable state should be returned.
   -- @return Returns true if the cell is editable.
	  
   function Is_Cells_Editable (G : access Graph_Record) return Boolean;
   procedure Set_Cells_Editable
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies the return value for isCell(s)Editable. Default is true.
   
   function Is_Cell_Resizable
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean;
   -- Returns true if the given cell is resizable. This implementation returns
   -- cellsSizable for all cells.
   -- @param cell Cell whose resizable state should be returned.
   -- @return Returns true if the cell is sizable.
   
   function Is_Cells_Resizable (G : access Graph_Record) return Boolean;
   procedure Set_Cells_Resizable
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies the return value for isCell(s)Sizable. Default is true.
   
   function Get_Movable_Cells
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) return Cells_Lists.List;
   -- Returns the cells which are movable in the given array of cells.
   
   function Is_Cell_Movable
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean;
   -- Returns true if the given cell is movable. This implementation
   -- returns movable.
   -- @param cell Cell whose movable state should be returned.
   -- @return Returns true if the cell is movable.

   function Is_Cells_Movable (G : access Graph_Record) return Boolean;
   -- Returns cellsMovable.
   
   procedure Set_Cells_Movable
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies the return value for isCell(s)Movable. Default is true.
   
   function Is_Terminal_Point_Movable
     (G      : access Graph_Record;
      Cell   : access Cell_Record'Class;
      Source : Boolean) return Boolean;
   -- Returns true if the given terminal point is movable. This is independent
   -- from isCellConnectable and isCellDisconnectable and controls if terminal
   -- points can be moved in the graph if the edge is not connected. Note that
   -- it is required for this to return true to connect unconnected edges.
   -- This implementation returns true.
   -- @param cell Cell whose terminal point should be moved.
   -- @param source Boolean indicating if the source or target terminal should 
   -- be moved.
   
   function Is_Cell_Bendable
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean;
   function Is_Cells_Bendable (G : access Graph_Record) return Boolean;
   procedure Set_Cells_Bendable
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies the return value for isCell(s)Bendable. Default is true.
   
   function Is_Cell_Selectable
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean;
   function Is_Cells_Selectable (G : access Graph_Record) return Boolean;
   procedure Set_Cells_Selectable
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies the return value for isCell(s)Selectable. Default is true.
   
   function Get_Deletable_Cells
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) return Cells_Lists.List;
   
   function Is_Cell_Deletable
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean;
   -- Returns true if the given cell is movable. This implementation always
   -- returns true.
   -- @param cell Cell whose movable state should be returned.
   -- @return Returns true if the cell is movable.

   function Is_Cells_Deletable (G : access Graph_Record) return Boolean;
   -- Returns cellsDeletable.
   
   function Get_Cloneable_Cells 
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) return Cells_Lists.List;
   -- Returns the cells which are movable in the given array of cells.
   
   function Is_Cell_Cloneable 
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean;
   -- Returns the constant true. This does not use the cloneable field to
   -- return a value for a given cell, it is simply a hook for subclassers
   -- to disallow cloning of individual cells.
   
   function Is_Cells_Cloneable (G : access Graph_Record) return Boolean;
   -- Returns cellsCloneable.
   
   procedure Set_Cells_Deletable
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies the return value for isCell(s)Deletable. Default is true.
   
   procedure Set_Cells_Cloneable
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies the return value for isCell(s)Cloneable. Default is true.
   
   function Is_Cell_Disconnectable
     (G        : access Graph_Record;
      Cell     : access Cell_Record'Class;
      Terminal : access Cell_Record'Class;
      Source   : Boolean) return Boolean;
   -- Returns true if the given cell is disconnectable from the source or
   -- target terminal. This returns <disconnectable> for all given cells if
   -- <isLocked> does not return true for the given cell.
   -- @param cell <mxCell> whose disconnectable state should be returned.
   -- @param terminal <mxCell> that represents the source or target terminal.
   -- @param source Boolean indicating if the source or target terminal is to be
   -- disconnected.
   -- @return Returns true if the given edge can be disconnected from the given
   -- terminal.
   
   function Is_Cells_Disconnectable (G : access Graph_Record) return Boolean;
   procedure Set_Cells_Disconnectable
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies the return value for isCellDisconntableFromTerminal. Default
   -- is true.
   
   function Is_Label_Clipped
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean;
   function Is_Labels_Clipped (G : access Graph_Record) return Boolean;
   procedure Set_Labels_Clipped
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies the return value for isLabel(s)Clipped. Default is false.
   
   function Is_Label_Movable
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean;
   -- Returns true if the given edges's label is moveable. This returns
   -- <movable> for all given cells if <isLocked> does not return true
   -- for the given cell.
   -- @param cell <mxCell> whose label should be moved.
   -- @return Returns true if the label of the given cell is movable.
   
   function Is_Vertex_Labels_Movable (G : access Graph_Record) return Boolean;
   procedure Set_Vertex_Labels_Movable
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies the return value for vertices in isLabelMovable. Default is
   -- false.
   
   function Is_Edge_Labels_Movable (G : access Graph_Record) return Boolean;
   procedure Set_Edge_Labels_Movable
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies the return value for edges in isLabelMovable. Default is
   -- true.
   
   function Is_Enabled (G : access Graph_Record) return Boolean;
   procedure Set_Enabled
     (G : access Graph_Record;
      E : Boolean);
   -- Specifies the return value for isEnabled. Default is true.
   
   function Is_Drop_Enabled (G : access Graph_Record) return Boolean;
   procedure Set_Drop_Enabled
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies the return value for isDropEnabled. Default is true.
   
   function Is_Split_Enabled (G : access Graph_Record) return Boolean;
   procedure Set_Split_Enabled
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies if dropping onto edges should be enabled. Default is true.
   
   function Is_Multigraph
     (G : access Graph_Record) return Boolean;
   procedure Set_Multigraph
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies if multiple edges in the same direction between the same
   -- pair of vertices are allowed. Default is true.
   
   function Is_Swimlane_Nesting (G : access Graph_Record) return Boolean;
   procedure Set_Swimlane_Nesting
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies if nesting of swimlanes is allowed. Default is true.
   
   function Is_Allow_Dangling_Edges (G : access Graph_Record) return Boolean;
   -- Returns allowDanglingEdges

   procedure Set_Allow_Dangling_Edges
     (G     : access Graph_Record;
      Value : Boolean);
   -- Sets allowDanglingEdges.

   function Is_Clone_Invalid_Edges (G : access Graph_Record) return Boolean;
   -- Returns cloneInvalidEdges.

   procedure Set_Clone_Invalid_Edges
     (G     : access Graph_Record;
      Value : Boolean);
   -- Sets cloneInvalidEdge.
   
   function Is_Disconnect_On_Move (G : access Graph_Record) return Boolean;
   procedure Set_Disconnect_On_Move
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies if edges should be disconnected from their terminals when
   -- they are moved. Default is true.
   
   function Is_Allow_Loops (G : access Graph_Record) return Boolean;
   procedure Set_Allow_Loops
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies if loops (aka self-references) are allowed. Default is false.
   
   function Is_Connectable_Edges (G : access Graph_Record) return Boolean;
   -- Returns connectableEdges.

   procedure Set_Connectable_Edges
     (G     : access Graph_Record;
      Value : Boolean);
   -- Sets connetableEdges.
   
   function Is_Reset_Edges_On_Move (G : access Graph_Record) return Boolean;
   procedure Set_Reset_Edges_On_Move
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies if edge control points should be reset after
   -- the move of a connected cell. Default is false.
   
   function Is_Reset_View_On_Root_Change
     (G : access Graph_Record) return Boolean;
   procedure Set_Reset_View_On_Root_Change
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies if the scale and translate should be reset if the root
   -- changes in the model. Default is true.
   
   function Is_Reset_Edges_On_Resize (G : access Graph_Record) return Boolean;
   procedure Set_Reset_Edges_On_Resize
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies if loops (aka self-references) are allowed. Default is false.
   
   function Is_Reset_Edges_On_Connect (G : access Graph_Record) return Boolean;
   procedure Set_Reset_Edges_On_Connect
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies if edge control points should be reset after the the edge
   -- has been reconnected. Default is true.
   
   function Is_Auto_Size_Cell
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean;
   -- Returns true if the size of the given cell should automatically be
   -- updated after a change of the label. This implementation returns
   -- autoSize for all given cells or checks if the cell style does specify
   -- mxConstants.STYLE_AUTOSIZE to be 1.
   -- @param cell Cell that should be resized.
   -- @return Returns true if the size of the given cell should be updated.
   
   function Is_Auto_Size_Cells (G : access Graph_Record) return Boolean;
   procedure Set_Auto_Size_Cells
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies if the graph should automatically update the cell size
   -- after an edit. This is used in isAutoSizeCell. Default is false.
   
   function Is_Extend_Parent
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean;
   -- Returns true if the parent of the given cell should be extended if the
   -- child has been resized so that it overlaps the parent. This
   -- implementation returns ExtendParents if cell is not an edge.
   -- @param cell Cell that has been resized.
   
   function Is_Extend_Parents (G : access Graph_Record) return Boolean;
   procedure Set_Extend_Parents
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies if a parent should contain the child bounds after a resize of
   -- the child. Default is true.
   
   function Is_Extend_Parents_On_Add (G : access Graph_Record) return Boolean;
   procedure Set_Extend_Parents_On_Add
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies if parents should be extended according to the
   -- <extendParents> switch if cells are added. Default is true.
   
   function Is_Constrain_Child
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean;
   -- Returns true if the given cell should be kept inside the bounds of its
   -- parent according to the rules defined by getOverlap and
   -- isAllowOverlapParent. This implementation returns false for all children
   -- of edges and isConstrainChildren() otherwise.
   
   function Is_Constraint_Children
     (G : access Graph_Record) return Boolean;
   procedure Set_Constraint_Children
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies the return value for isConstrainChildren. Default is true.
   
   function Is_Auto_Origin (G : access Graph_Record) return Boolean;
   procedure Set_Auto_Origin
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies if the origin should be automatically updated. 
   
   function Get_Origin (G : access Graph_Record) return Point_Record;
   procedure Set_Origin
     (G : access Graph_Record;
      V : Point_Record);
   -- Holds the current automatic origin.
   
   function Get_Changes_Repaint_Threshold
     (G : access Graph_Record) return Integer;
   procedure Set_Changes_Repaint_Threshold
     (G : access Graph_Record;
      V : Integer);
   -- Specifies the maximum number of changes that should be processed to
   -- find the dirty region. If the number of changes is larger, then the
   -- complete grah is repainted. A value of zero will always compute the
   -- dirty region for any number of changes. Default is 1000.
   
   function Is_Allow_Negative_Coordinates
     (G : access Graph_Record) return Boolean;
   procedure Set_Allow_Negative_Coordinates
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies if negative coordinates for vertices are allowed. Default is
   -- true.
   
   function Is_Collapse_To_Preferred_Size
     (G : access Graph_Record) return Boolean;
   procedure Set_Collapse_To_Preferred_Size
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies if the cell size should be changed to the preferred size when
   -- a cell is first collapsed. Default is true.
   
   function Is_Keep_Edges_In_Foreground
     (G : access Graph_Record) return Boolean;
   procedure Set_Keep_Edges_In_Foreground
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies if edges should appear in the foreground regardless of their
   -- order in the model. This has precendence over keepEdgeInBackground
   -- Default is false.
   
   function Is_Keep_Edges_In_Background
     (G : access Graph_Record) return Boolean;
   procedure Set_Keep_Edges_In_Background
     (G : access Graph_Record;
      V : Boolean);
   -- Specifies if edges should appear in the background regardless of their
   -- order in the model. Default is false.
   
   function Is_Valid_Source
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean;
   -- Returns true if the given cell is a valid source for new connections.
   -- This implementation returns true for all non-null values and is
   -- called by is called by <isValidConnection>.
   -- @param cell Object that represents a possible source or null.
   -- @return Returns true if the given cell is a valid source terminal.

   function Is_Valid_Target
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean;
   -- Returns isValidSource for the given cell. This is called by
   -- isValidConnection.
   -- @param cell Object that represents a possible target or null.
   -- @return Returns true if the given cell is a valid target.

   function Is_Valid_Connection
     (G      : access Graph_Record;
      Source : access Cell_Record'Class;
      Target : access Cell_Record'Class) return Boolean;
   -- Returns true if the given target cell is a valid target for source.
   -- This is a boolean implementation for not allowing connections between
   -- certain pairs of vertices and is called by <getEdgeValidationError>.
   -- This implementation returns true if <isValidSource> returns true for
   -- the source and <isValidTarget> returns true for the target.
   -- @param source Object that represents the source cell.
   -- @param target Object that represents the target cell.
   -- @return Returns true if the the connection between the given terminals
   -- is valid.
   
   function Get_Minimum_Graph_Size
     (G : access Graph_Record) return Rectangle_Record;
   procedure Set_Minimum_Graph_Size
     (G : access Graph_Record;
      V : Rectangle_Record);
   -- mxRectangle that specifies the minimum size of the graph canvas inside
   -- the scrollpane.
   
   function Get_Overlap
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Coordinate;
   -- Returns a decimal number representing the amount of the width and height
   -- of the given cell that is allowed to overlap its parent. A value of 0
   -- means all children must stay inside the parent, 1 means the child is
   -- allowed to be placed outside of the parent such that it touches one of
   -- the parents sides. If <isAllowOverlapParent> returns false for the given
   -- cell, then this method returns 0.
   -- @param cell
   -- @return Returns the overlapping value for the given cell inside its
   -- parent.
   
   function Get_Default_Overlap (G : access Graph_Record) return Coordinate;
   procedure Set_Default_Overlap
     (G : access Graph_Record;
      O : Coordinate);
   -- Value returned by getOverlap if isAllowOverlapParent returns
   -- true for the given cell. getOverlap is used in keepInside if
   -- isKeepInsideParentOnMove returns true. The value specifies the
   -- portion of the child which is allowed to overlap the parent.
      
   function Is_Allow_Overlap_Parent
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean;
   -- Returns true if the given cell is allowed to be placed outside of the
   -- parents area.
   -- @param cell
   -- @return Returns true if the given cell may overlap its parent.
 
   function Is_Grid_Enabled (G : access Graph_Record) return Boolean;
   procedure Set_Grid_Enabled
     (G : access Graph_Record;
      E : Boolean);
   -- Specifies if the grid is enabled. Default is true.
 
   function Is_Valid_Drop_Target
     (G     : access Graph_Record;
      Cell  : access Cell_Record'Class;
      Cells : Cells_Lists.List) return Boolean;
   -- Returns true if the given cell is a valid drop target for the specified
   -- cells. This returns true if the cell is a swimlane, has children and is
   -- not collapsed, or if splitEnabled is true and isSplitTarget returns
   -- true for the given arguments
   -- @param cell Object that represents the possible drop target.
   -- @param cells Objects that are going to be dropped.
   -- @return Returns true if the cell is a valid drop target for the given
   -- cells.

   function Is_Split_Target
     (G      : access Graph_Record;
      Target : access Cell_Record'Class;
      Cells  : Cells_Lists.List) return Boolean;
   -- Returns true if split is enabled and the given edge may be splitted into
   -- two edges with the given cell as a new terminal between the two.
   -- @param target Object that represents the edge to be splitted.
   -- @param cells Array of cells to add into the given edge.
   -- @return Returns true if the given edge may be splitted by the given
   -- cell.

   function Get_Drop_Target
     (G      : access Graph_Record;
      Cells  : Cells_Lists.List;
      Pt     : Point_Record;
      Cell   : access Cell_Record'Class) return access Cell_Record'Class;
   -- Returns the given cell if it is a drop target for the given cells or the
   -- nearest ancestor that may be used as a drop target for the given cells.
   -- If the given array contains a swimlane and swimlaneNesting is false
   -- then this always returns null. If no cell is given, then the bottommost
   -- swimlane at the location of the given event is returned.
   -- This function should only be used if isDropEnabled returns true.
   
   
   
   --
   -- Cell retrieval
   --
	  
   function Get_Default_Parent 
     (G : access Graph_Record) return access Cell_Record'Class;
   procedure Set_Default_Parent
     (G : access Graph_Record;
      P : access Cell_Record'Class);
   -- Specifies the default parent to be used to insert new cells.
   -- This is used in getDefaultParent. Default is null.
      
   
   function Get_Child_Vertices
     (G      : access Graph_Record;
      Parent : access Cell_Record'Class) return Cells_Lists.List;
   -- Returns the visible child vertices of the given parent.
   -- @param parent Cell whose children should be returned.
   
   function Get_Child_Edges
     (G      : access Graph_Record;
      Parent : access Cell_Record'Class) return Cells_Lists.List;
   -- Returns the visible child edges of the given parent.
   -- @param parent Cell whose children should be returned.
   
   function Get_Child_Cells
     (G      : access Graph_Record;
      Parent : access Cell_Record'Class) return Cells_Lists.List;
   -- Returns the visible children of the given parent.
   -- @param parent Cell whose children should be returned.
   
   function Get_Child_Cells
     (G        : access Graph_Record;
      Parent   : access Cell_Record'Class;
      Vertices : Boolean;
      Edges    : Boolean) return Cells_Lists.List;
   -- Returns the visible child vertices or edges in the given parent. If
   -- vertices and edges is false, then all children are returned.
   -- @param parent Cell whose children should be returned.
   -- @param vertices Specifies if child vertices should be returned.
   -- @param edges Specifies if child edges should be returned.
   -- @return Returns the child vertices and edges.
   
   function Get_Connections
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Cells_Lists.List;
   -- Returns all visible edges connected to the given cell without loops.
   -- @param cell Cell whose connections should be returned.
   -- @return Returns the connected edges for the given cell.
   
   function Get_Connections
     (G      : access Graph_Record;
      Cell   : access Cell_Record'Class;
      Parent : access Cell_Record'Class) return Cells_Lists.List;
   -- Returns all visible edges connected to the given cell without loops.
   -- If the optional parent argument is specified, then only child
   -- edges of the given parent are returned.
   -- @param cell Cell whose connections should be returned.
   -- @param parent Optional parent of the opposite end for a connection
   -- to be returned.
   -- @return Returns the connected edges for the given cell.
   
   function Get_Connections
     (G       : access Graph_Record;
      Cell    : access Cell_Record'Class;
      Parent  : access Cell_Record'Class;
      Recurse : Boolean) return Cells_Lists.List;
   -- Returns all visible edges connected to the given cell without loops.
   -- If the optional parent argument is specified, then only child
   -- edges of the given parent are returned.
   -- @param cell Cell whose connections should be returned.
   -- @param parent Optional parent of the opposite end for a connection
   -- to be returned.
   -- @return Returns the connected edges for the given cell.
   
   function Get_Incoming_Edges
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Cells_Lists.List;
   -- Returns all incoming visible edges connected to the given cell without
   -- loops.
   -- @param cell Cell whose incoming edges should be returned.
   -- @return Returns the incoming edges of the given cell.

   function Get_Incoming_Edges
     (G      : access Graph_Record;
      Cell   : access Cell_Record'Class;
      Parent : access Cell_Record'Class) return Cells_Lists.List;
   -- Returns the visible incoming edges for the given cell. If the optional
   -- parent argument is specified, then only child edges of the given parent
   -- are returned.
   -- @param cell Cell whose incoming edges should be returned.
   -- @param parent Optional parent of the opposite end for an edge
   -- to be returned.
   -- @return Returns the incoming edges of the given cell.

   function Get_Outgoing_Edges
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Cells_Lists.List;
   -- Returns all outgoing visible edges connected to the given cell without
   -- loops.
   -- @param cell Cell whose outgoing edges should be returned.
   -- @return Returns the outgoing edges of the given cell.

   function Get_Outgoing_Edges
     (G      : access Graph_Record;
      Cell   : access Cell_Record'Class;
      Parent : access Cell_Record'Class) return Cells_Lists.List;
   -- Returns the visible outgoing edges for the given cell. If the optional
   -- parent argument is specified, then only child edges of the given parent
   -- are returned.
   -- @param cell Cell whose outgoing edges should be returned.
   -- @param parent Optional parent of the opposite end for an edge
   -- to be returned.
   -- @return Returns the outgoing edges of the given cell.
   
   function Get_Edges
     (G             : access Graph_Record;
      Cell          : access Cell_Record'Class;
      Parent        : access Cell_Record'Class := null;
      Incoming      : Boolean := True;
      Outgoing      : Boolean := True;
      Include_Loops : Boolean := True;
      Recurse       : Boolean := False) return Cells_Lists.List;
   -- Returns the incoming and/or outgoing edges for the given cell.
   -- If the optional parent argument is specified, then only edges are returned
   -- where the opposite is in the given parent cell.
   -- @param cell Cell whose edges should be returned.
   -- @param parent Optional parent. If specified the opposite end of any edge
   -- must be a child of that parent in order for the edge to be returned. The
   -- recurse parameter specifies whether or not it must be the direct child
   -- or the parent just be an ancestral parent.
   -- @param incoming Specifies if incoming edges should be included in the
   -- result.
   -- @param outgoing Specifies if outgoing edges should be included in the
   -- result.
   -- @param includeLoops Specifies if loops should be included in the result.
   -- @param recurse Specifies if the parent specified only need be an ancestral
   -- parent, <code>true</code>, or the direct parent, <code>false</code>
   -- @return Returns the edges connected to the given cell.
   
   function Is_Valid_Ancestor
     (G       : access Graph_Record;
      Cell    : access Cell_Record'Class;
      Parent  : access Cell_Record'Class;
      Recurse : Boolean) return Boolean;
   -- Returns whether or not the specified parent is a valid
   -- ancestor of the specified cell, either direct or indirectly
   -- based on whether ancestor recursion is enabled.
   -- @param cell the possible child cell
   -- @param parent the possible parent cell
   -- @param recurse whether or not to recurse the child ancestors
   -- @return whether or not the specified parent is a valid
   -- ancestor of the specified cell, either direct or indirectly
   -- based on whether ancestor recursion is enabled.
   
   function Get_Opposites
     (G        : access Graph_Record;
      Edges    : Cells_Lists.List;
      Terminal : access Cell_Record'Class;
      Sources  : Boolean := True;
      Targets  : Boolean := True) return Cells_Lists.List;
   -- Returns all distincts visible opposite cells for the specified terminal
   -- on the given edges.
   -- @param edges Edges whose opposite terminals should be returned.
   -- @param terminal Terminal that specifies the end whose opposite should be
   -- returned.
   -- @param sources Specifies if source terminals should be included in the
   -- result.
   -- @param targets Specifies if target terminals should be included in the
   -- result.
   -- @return Returns the cells at the opposite ends of the given edges.
   
   function Get_Edges_Between
     (G        : access Graph_Record;
      Source   : access Cell_Record'Class;
      Target   : access Cell_Record'Class;
      Directed : Boolean := False) return Cells_Lists.List;
   -- Returns the edges between the given source and target. This takes into
   -- account collapsed and invisible cells and returns the connected edges
   -- as displayed on the screen.
   -- @param source
   -- @param target
   -- @param directed
   -- @return Returns all edges between the given terminals.
   
   function Get_Cells_Beyond
     (G                : access Graph_Record;
      X0               : Coordinate;
      Y0               : Coordinate;
      Parent           : access Cell_Record'Class;
      Right_Half_Pane  : Boolean;
      Bottom_Half_Pane : Boolean) return Cells_Lists.List;
   -- Returns the children of the given parent that are contained in the
   -- halfpane from the given point (x0, y0) rightwards and downwards
   -- depending on rightHalfpane and bottomHalfpane.
   -- @param x0 X-coordinate of the origin.
   -- @param y0 Y-coordinate of the origin.
   -- @param parent <mxCell> whose children should be checked.
   -- @param rightHalfpane Boolean indicating if the cells in the right halfpane
   -- from the origin should be returned.
   -- @param bottomHalfpane Boolean indicating if the cells in the bottom 
   -- halfpane from the origin should be returned.
   -- @return Returns the cells beyond the given halfpane.
   
   function Find_Tree_Roots
     (G       : access Graph_Record;
      Parent  : access Cell_Record'Class;
      Isolate : Boolean := False;
      Invert  : Boolean := False) return Cells_Lists.List;
   -- Returns all visible children in the given parent which do not have
   -- incoming edges. If the result is empty then the children with the
   -- maximum difference between incoming and outgoing edges are returned.
   -- This takes into account edges that are being promoted to the given
   -- root due to invisible children or collapsed cells.
   -- @param parent Cell whose children should be checked.
   -- @param isolate Specifies if edges should be ignored if the opposite
   -- end is not a child of the given parent cell.
   -- @param invert Specifies if outgoing or incoming edges should be counted
   -- for a tree root. If false then outgoing edges will be counted.
   -- @return List of tree roots in parent.
   
   procedure Traverse 
     (G        : access Graph_Record;
      Vertex   : access Cell_Record'Class;
      Directed : Boolean;
      Visitor  : access Cell_Visitor_Interface'Class);
   -- Traverses the tree starting at the given vertex. Here is how to use this
   -- method for a given vertex (root) which is typically the root of a tree:
   -- <code>
   -- graph.traverse(root, true, new mxICellVisitor()
   -- {
   --   public boolean visit(Object vertex, Object edge)
   --   {
   --     System.out.println("edge="+graph.convertValueToString(edge)+
   --       " vertex="+graph.convertValueToString(vertex));
   --     
   --     return true;
   --   }
   -- });
   -- </code>
   -- @param vertex
   -- @param directed
   -- @param visitor
   
   
   procedure Traverse 
     (G        : access Graph_Record;
      Vertex   : access Cell_Record'Class;
      Directed : Boolean;
      Visitor  : access Cell_Visitor_Interface'Class;
      Edge     : access Cell_Record'Class;
      Visited  : in out Cells_Lists.List);
   -- Traverses the (directed) graph invoking the given function for each
   -- visited vertex and edge. The function is invoked with the current vertex
   -- and the incoming edge as a parameter. This implementation makes sure
   -- each vertex is only visited once. The function may return false if the
   -- traversal should stop at the given vertex.
   -- @param vertex <mxCell> that represents the vertex where the traversal
   -- starts.
   -- @param directed Optional boolean indicating if edges should only be
   -- traversed from source to target. Default is true.
   -- @param visitor Visitor that takes the current vertex and the incoming 
   -- edge. The traversal stops if the function returns false.
   -- @param edge Optional <mxCell> that represents the incoming edge. This is
   -- null for the first step of the traversal.
   -- @param visited Optional array of cell paths for the visited cells.
   
   --
   -- Selection --
   --
   
   function Get_Selection_Model
     (G : access Graph_Record) return access Selection_Model_Record'Class;
   procedure Set_Selection_Model
     (G : access Graph_Record;
      M : access Selection_Model_Record'Class);
   -- Holds the <mxGraphSelection> that models the current selection.
      
   
   function Get_Selection_Count (G : access Graph_Record) return Integer;
   
   function Is_Cell_Selected 
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean;
   -- @param cell
   -- @return Returns true if the given cell is selected.
   
   function Is_Selection_Empty (G : access Graph_Record) return Boolean;
   -- @return Returns true if the selection is empty.
   
   procedure Clear_Selection (G : access Graph_Record);
     
   function Get_Selection_Cell
     (G : access Graph_Record) return access Cell_Record'Class;
   -- @return Returns the selection cell.
   
   procedure Set_Selection_Cell
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class);
   -- @param cell

   function Get_Selection_Cells
     (G : access Graph_Record) return Cells_Lists.List;
   -- @return Returns the selection cells.

   procedure Set_Selection_Cells
     (G     : access Graph_Record;
      Cells : Cells_Lists.List);
   -- @param cells
   
   --  --  public void setSelectionCells(Collection<Object> cells)
   --  --  	{
   --  --  		if (cells != null)
   --  --  		{
   --  --  			setSelectionCells(cells.toArray());
   --  --  		}
   --  --  	}

   procedure Add_Selection_Cell 
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class);

   procedure Add_Selection_Cells
     (G     : access Graph_Record;
      Cells : Cells_Lists.List);
   
   procedure Remove_Selection_Cell
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class);
     
   procedure Remove_Selection_Cells
     (G     : access Graph_Record;
      Cells : Cells_Lists.List);
   
   procedure Select_Next_Cell (G : access Graph_Record);
   -- Selects the next cell.

   procedure Select_Previous_Cell (G : access Graph_Record);
   -- Selects the previous cell.

   procedure Select_Parent_Cell (G : access Graph_Record);
   -- Selects the parent cell.
   
   procedure Select_Child_Cell (G : access Graph_Record);
   -- Selects the first child cell.
   
   procedure Select_Cell
     (G         : access Graph_Record;
      Is_Next   : Boolean;
      Is_Parent : Boolean;
      Is_Child  : Boolean);
   -- Selects the next, parent, first child or previous cell, if all arguments
   -- are false.
   -- @param isNext
   -- @param isParent
   -- @param isChild
   
   procedure Select_Vertices
     (G      : access Graph_Record;
      Parent : access Cell_Record'Class := null);
   -- Selects all vertices inside the given parent or the default parent
   -- if no parent is given.
   
   procedure Select_Edges
     (G      : access Graph_Record;
      Parent : access Cell_Record'Class := null);
   -- Selects all vertices inside the given parent or the default parent
   -- if no parent is given.
   
   procedure Select_Cells
     (G        : access Graph_Record;
      Vertices : Boolean;
      Edges    : Boolean;
      Parent   : access Cell_Record'Class := null);
   -- Selects all vertices and/or edges depending on the given boolean
   -- arguments recursively, starting at the given parent or the default
   -- parent if no parent is specified. Use <code>selectAll</code> to select
   -- all cells.
   -- @param vertices Boolean indicating if vertices should be selected.
   -- @param edges Boolean indicating if edges should be selected.
   -- @param parent Optional cell that acts as the root of the recursion.
   -- Default is <code>defaultParent</code>.
   
   procedure Select_All
     (G      : access Graph_Record;
      Parent : access Cell_Record'Class := null);
   -- Selects all children of the given parent cell or the children of the
   -- default parent if no parent is specified. To select leaf vertices and/or
   -- edges use <selectCells>.
   -- @param parent  Optional <mxCell> whose children should be selected.
   -- Default is <defaultParent>.
	  
   
   --  ===>>> protected mxEdgeStyle.mxEdgeStyleFunction defaultLoopStyle = mxEdgeStyle.Loop;
   -- Specifies the default style for loops.
   
   -- Holds the list of bundles.
   --protected static List<mxImageBundle> imageBundles = new LinkedList<mxImageBundle>();
	
   
   function Clone_Graph (G : Graph_Ptr) return Graph_Ptr;
   
   --     function Get_Root_Node
   --       (This : access Graph_Record) return access Node_Record'Class;
   --     procedure Set_Root_Node
   --       (This : access Graph_Record;
   --        Root : access Node_Record'Class);
   
private
	
   type Graph_Record is new Event_Source_Record and Graph_Interface with record
      Model : access Model_Interface'Class;
      -- Holds the model that contains the cells to be displayed.

      View : access View_Record'Class;
      -- Holds the view that caches the cell states.
                  
      Selection_Model : access Selection_Model_Record'Class;
      -- Holds the <mxGraphSelection> that models the current selection.
      
      Grid_Size : Coordinate;
      -- Specifies the grid size. Default is 10.
      
      Grid_Enabled : Boolean;
      -- Specifies if the grid is enabled. Default is true.
      
      Default_Overlap : Coordinate;
      -- Value returned by getOverlap if isAllowOverlapParent returns
      -- true for the given cell. getOverlap is used in keepInside if
      -- isKeepInsideParentOnMove returns true. The value specifies the
      -- portion of the child which is allowed to overlap the parent.
      
      Default_Parent : access Cell_Record'Class;
      -- Specifies the default parent to be used to insert new cells.
      -- This is used in getDefaultParent. Default is null.
      
      Enabled : Boolean;
      -- Specifies the return value for isEnabled. Default is true.
      
      Cells_Locked : Boolean;
      -- Specifies the return value for isCell(s)Locked. Default is false.
      
      Cells_Editable : Boolean;
      -- Specifies the return value for isCell(s)Editable. Default is true.
      
      Cells_Resizable : Boolean;
      -- Specifies the return value for isCell(s)Sizable. Default is true.
      
      Cells_Movable : Boolean;
      -- Specifies the return value for isCell(s)Movable. Default is true.
            

      Cells_Bendable : Boolean;
      -- Specifies the return value for isCell(s)Bendable. Default is true.
      
      Cells_Selectable : Boolean;
      -- Specifies the return value for isCell(s)Selectable. Default is true.
      
      Cells_Deletable : Boolean;
      -- Specifies the return value for isCell(s)Deletable. Default is true.
      
      Cells_Cloneable : Boolean;
      -- Specifies the return value for isCell(s)Cloneable. Default is true.
      
      Cells_Disconnectable : Boolean;
      -- Specifies the return value for isCellDisconntableFromTerminal. Default
      -- is true.

      Labels_Clipped : Boolean;
      -- Specifies the return value for isLabel(s)Clipped. Default is false.
      
      Edge_Labels_Movable : Boolean;
      -- Specifies the return value for edges in isLabelMovable. Default is
      -- true.
      
      Vertex_Labels_Movable : Boolean;
      -- Specifies the return value for vertices in isLabelMovable. Default is
      -- false.
      
      Drop_Enabled : Boolean;
      -- Specifies the return value for isDropEnabled. Default is true.
      
      Split_Enabled : Boolean;
      -- Specifies if dropping onto edges should be enabled. Default is true.
      
      Auto_Size_Cells : Boolean;
      -- Specifies if the graph should automatically update the cell size
      -- after an edit. This is used in isAutoSizeCell. Default is false.
      
      Maximum_Graph_Bounds : Rectangle_Record;
      -- <mxRectangle> that specifies the area in which all cells in the
      -- diagram should be placed. Uses in getMaximumGraphBounds. Use a width
      -- or height of 0 if you only want to give a upper, left corner.

      Minimum_Graph_Size : Rectangle_Record;
      -- mxRectangle that specifies the minimum size of the graph canvas inside
      -- the scrollpane.
      
      Border : Integer;
      -- Border to be added to the bottom and right side when the container is
      -- being resized after the graph has been changed. Default is 0.
      
      Keep_Edges_In_Foreground : Boolean;
      -- Specifies if edges should appear in the foreground regardless of their
      -- order in the model. This has precendence over keepEdgeInBackground
      -- Default is false.

      Keep_Edges_In_Background : Boolean;
      -- Specifies if edges should appear in the background regardless of their
      -- order in the model. Default is false.
      
      Collapse_To_Preferred_Size : Boolean;
      -- Specifies if the cell size should be changed to the preferred size when
      -- a cell is first collapsed. Default is true.
      
      Allow_Negative_Coordinates : Boolean;
      -- Specifies if negative coordinates for vertices are allowed. Default is
      -- true.
      
      Constraint_Children : Boolean;
      -- Specifies the return value for isConstrainChildren. Default is true.
      
      Extend_Parents : Boolean;
      -- Specifies if a parent should contain the child bounds after a resize of
      -- the child. Default is true.
      
      Extend_Parents_On_Add : Boolean;
      -- Specifies if parents should be extended according to the
      -- <extendParents> switch if cells are added. Default is true.
      
      Reset_View_On_Root_Change : Boolean;
      -- Specifies if the scale and translate should be reset if the root
      -- changes in the model. Default is true.
      
      Reset_Edges_On_Resize : Boolean;
      -- Specifies if loops (aka self-references) are allowed. Default is false.
      
      Reset_Edges_On_Move : Boolean;
      -- Specifies if edge control points should be reset after
      -- the move of a connected cell. Default is false.
      
      Reset_Edges_On_Connect : Boolean;
      -- Specifies if edge control points should be reset after the the edge
      -- has been reconnected. Default is true.
      
      Allow_Loops : Boolean;
      -- Specifies if loops (aka self-references) are allowed. Default is false.
      
      Multiplicities : Boolean;
      -- Specifies the multiplicities to be used for validation of the graph.
      
      Multigraph : Boolean;
      -- Specifies if multiple edges in the same direction between the same
      -- pair of vertices are allowed. Default is true.
      
      Connectable_Edges : Boolean;
      -- Specifies if edges are connectable. Default is false. This overrides
      -- the connectable field in edges.
      
      Allow_Dangling_Edges : Boolean;
      -- Specifies if edges with disconnected terminals are allowed in the
      -- graph. Default is false.
      
      Clone_Invalid_Edges : Boolean;
      -- Specifies if edges that are cloned should be validated and only
      -- inserted if they are valid. Default is true.
      
      Disconnect_On_Move : Boolean;
      -- Specifies if edges should be disconnected from their terminals when
      -- they are moved. Default is true.
      
      Labels_Visible : Boolean;
      -- Specifies if labels should be visible. This is used in getLabel. 
      -- Default is true.
      
      Html_Labels : Boolean;
      -- Specifies the return value for isHtmlLabel. Default is false.
      
      Swimlane_Nesting : Boolean;
      -- Specifies if nesting of swimlanes is allowed. Default is true.
      
      Changes_Repaint_Threshold : Integer;
      -- Specifies the maximum number of changes that should be processed to
      -- find the dirty region. If the number of changes is larger, then the
      -- complete grah is repainted. A value of zero will always compute the
      -- dirty region for any number of changes. Default is 1000.
      
      Auto_Origin : Boolean;
      -- Specifies if the origin should be automatically updated. 
      
      Origin : Point_Record;
      -- Holds the current automatic origin.
      
      Full_Repainter_Handler : access Full_Repainter_Handler_Record;
      -- Fires repaint events for full repaints.
      
      Update_Origin_Handler : access Update_Origin_Handler_Record;
      -- Fires repaint events for full repaints.
      
      Graph_Model_Change_Handler : access Graph_Model_Change_Handler_Record;
      -- Fires repaint events for model changes.
      
      ----- Root_Node : access Node_Record'Class;
   end record;
   
   
   No_Graph_Record : constant Graph_Record := Graph_Record'
     (No_Event_Source_Record with
      Model                         => null,
      View                          => null,
      Selection_Model               => null,
      Grid_Size                     => 10.0,
      Grid_Enabled                  => True,
      Default_Overlap               => 0.5,
      Default_Parent                => null,
      Enabled                       => True,
      Cells_Locked                  => False,
      Cells_Editable                => True,
      Cells_Resizable               => True,
      Cells_Movable                 => True,
      Cells_Bendable                => True,
      Cells_Selectable              => True,
      Cells_Deletable               => True,
      Cells_Cloneable               => True,
      Cells_Disconnectable          => True,
      Labels_Clipped                => False,
      Edge_Labels_Movable           => True,
      Vertex_Labels_Movable         => False,
      Drop_Enabled                  => True,
      Split_Enabled                 => True,
      Auto_Size_Cells               => False,
      Maximum_Graph_Bounds          => No_Rectangle_Record,
      Minimum_Graph_Size            => No_Rectangle_Record,
      Border                        => 0,
      Keep_Edges_In_Foreground      => False,
      Keep_Edges_In_Background      => False,
      Collapse_To_Preferred_Size    => True,
      Allow_Negative_Coordinates    => True,
      Constraint_Children           => True,
      Extend_Parents                => True,
      Extend_Parents_On_Add         => True,
      Reset_View_On_Root_Change     => True,
      Reset_Edges_On_Resize         => True, -- False,
      Reset_Edges_On_Move           => True, -- False,
      Reset_Edges_On_Connect        => True,
      Allow_Loops                   => False,
      Multiplicities                => False,
      Multigraph                    => True,
      Connectable_Edges             => False,
      Allow_Dangling_Edges          => True,
      Clone_Invalid_Edges           => True,
      Disconnect_On_Move            => True,
      Labels_Visible                => True,
      Html_Labels                   => False,
      Swimlane_Nesting              => True,
      Changes_Repaint_Threshold     => 1000,
      Auto_Origin                   => False,
      Origin                        => No_Point_Record,
      Full_Repainter_Handler        => null,
      Update_Origin_Handler         => null,
      Graph_Model_Change_Handler    => null
      ---- Root_Node                     => null
     );
   
end Artics.Graph.Graphs;

