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

with Artics.Types; use Artics.Types;
with Artics.Namet; use Artics.Namet;

with Artics.Objects; use Artics.Objects;

with Artics.Graph.Constants; use  Artics.Graph.Constants;
with Artics.Graph.Names; use  Artics.Graph.Names;

with Artics.Geometry; use Artics.Geometry;
with Artics.Geometry.Points; use Artics.Geometry.Points;
with Artics.Geometry.Rectangles; use Artics.Geometry.Rectangles;

with Artics.Graph.Graphs_Interfaces; use Artics.Graph.Graphs_Interfaces;
with Artics.Graph.Cells; use  Artics.Graph.Cells;
with Artics.Graph.Models_Interfaces; use  Artics.Graph.Models_Interfaces;
with Artics.Graph.Cells_Geometry; use  Artics.Graph.Cells_Geometry;
with Artics.Graph.Cells_States; use  Artics.Graph.Cells_States;
with Artics.Graph.Connection_Constraints; use Artics.Graph.Connection_Constraints;

with Artics.Graph.Events; use Artics.Graph.Events;

-- Implements a view for the graph. This class is in charge of computing the
-- absolute coordinates for the relative child geometries, the points for
-- perimeters and edge styles and keeping them cached in cell states for faster
-- retrieval. The states are updated whenever the model or the view state
-- (translate, scale) changes. The scale and translate are honoured in the
-- bounds.
-- 
-- This class fires the following events:
-- 
-- mxEvent.UNDO fires after the root was changed in setCurrentRoot. The
-- <code>edit</code> property contains the mxUndoableEdit which contains the
-- mxCurrentRootChange.
-- 
-- mxEvent.SCALE_AND_TRANSLATE fires after the scale and transle have been
-- changed in scaleAndTranslate. The <code>scale</code>,
-- <code>previousScale</code>, <code>translate</code> and
-- <code>previousTranslate</code> properties contain the new and previous scale
-- and translate, respectively.
-- 
-- mxEvent.SCALE fires after the scale was changed in setScale. The
-- <code>scale</code> and <code>previousScale</code> properties contain the new
-- and previous scale.
-- 
-- mxEvent.TRANSLATE fires after the translate was changed in setTranslate. The
-- <code>translate</code> and <code>previousTranslate</code> properties contain
-- the new and previous value for translate.
-- 
-- mxEvent.UP and mxEvent.DOWN fire if the current root is changed by executing
-- a mxCurrentRootChange. The event name depends on the location of the root in
-- the cell hierarchy with respect to the current root. The <code>root</code>
-- and <code>previous</code> properties contain the new and previous root,
-- respectively.

with Dummy; use Dummy;

package Artics.Graph.Views_Interfaces is
   
   type View_Interface is Interface;
   type View_Ptr is access all View_Interface'Class;
   
   function Get_Graph
     (V : access View_Interface) return access Graph_Interface'Class is abstract;
   procedure Set_Graph
     (V : access View_Interface;
      G : access Graph_Interface'Class) is abstract;
   -- Reference to the enclosing graph.
      
   function Get_Graph_Bounds (V : access View_Interface) return Rectangle_Record is abstract;
   procedure Set_Graph_Bounds
     (V : access View_Interface;
      R : Rectangle_Record) is abstract;
   -- Caches the current bounds of the graph.
   
   function Get_States (V : access View_Interface) return Cells_States_Maps.Map is abstract;
   procedure Set_States
     (V      : access View_Interface;
      States : Cells_States_Maps.Map) is abstract; 
   -- Maps from cells to cell states.
   
   function Get_Current_Root
     (V : access View_Interface) return access Cell_Record'Class is abstract;
   procedure Set_Current_Root
     (V    : access View_Interface;
      Root : access Cell_Record'Class) is abstract;
   -- mxCell that acts as the root of the displayed cell hierarchy.
      
   procedure Scale_And_Translate
     (V     : access View_Interface;
      Scale : Coordinate;
      Dx    : Coordinate;
      Dy    : Coordinate) is abstract;
   -- Sets the scale and translation. Fires a "scaleAndTranslate" event after
   -- calling revalidate. Revalidate is only called if isEventsEnabled.
   -- @param scale Decimal value that specifies the new scale (1 is 100%).
   -- @param dx X-coordinate of the translation.
   -- @param dy Y-coordinate of the translation.
   
   function Get_Scale (V : access View_Interface) return Coordinate is abstract;
   procedure Set_Scale
     (V     : access View_Interface;
      Value : Coordinate) is abstract;
   -- Specifies the scale. Default is 1 (100%).
      
   function Get_Translate
     (V : access View_Interface) return Point_Record is abstract;
   procedure Set_Translate
     (V     : access View_Interface;
      Value : Point_Record) is abstract;
   -- Point that specifies the current translation. Default is a new empty
   -- point.
   
   function Get_Bounding_Box
     (V     : access View_Interface;
      Cells : Cells_Lists.list) return Rectangle_Record is abstract;
   -- Returns the bounding box for an array of cells or null, if no cells are
   -- specified.
   -- @param cells
   -- @return Returns the bounding box for the given cells.
   
   function Get_Bounds 
     (V            : access View_Interface;
      Cells        : Cells_Lists.List;
      Bounding_Box : Boolean := False) return Rectangle_Record is abstract;
   -- Returns the bounding box for an array of cells or null, if no cells are
   -- specified.
   -- @param cells
   -- @return Returns the bounding box for the given cells.
   
   procedure Reload (V : access View_Interface) is abstract;
   -- Removes all existing cell states and invokes validate.
   
   procedure Revalidate (V : access View_Interface) is abstract;
   
   procedure Clear
     (V       : access View_Interface;
      Cell    : access Cell_Record'Class;
      Force   : Boolean;
      Recurse : Boolean) is abstract;
   -- Removes the state of the given cell and all descendants if the given cell
   -- is not the current root.
   -- @param cell
   -- @param force
   -- @param recurse
   
   procedure Invalidate
     (V : access View_Interface;
      C : access Cell_Record'Class := null) is abstract;
   -- Invalidates the state of the given cell, all its descendants and 
   -- connected edges. If c is null all cells are invalidated.
   
   procedure Validate (V : access View_Interface) is abstract;
   -- First validates all bounds and then validates all points recursively on
   -- all visible cells.
   
   function Get_Bounding_Box
     (V     : access View_Interface;
      State : access Cell_State_Record'Class) 
     return Rectangle_Record is abstract;
   -- Shortcut to validateCell with visible set to true.
   
   function Get_Bounding_Box
     (V       : access View_Interface;
      State   : access Cell_State_Record'Class;
      Recurse : Boolean) return Rectangle_Record is abstract;
   -- Returns the bounding box of the shape and the label for the given cell
   -- state and its children if recurse is true.
   -- @param state
   --            Cell state whose bounding box should be returned.
   -- @param recurse
   --            Boolean indicating if the children should be included.
   
   procedure Validate_Cell
     (V    : access View_Interface;
      Cell : access Cell_Record'Class) is abstract;
   -- Shortcut to validateCell with visible set to true.
   
   function Validate_Cell_State
     (V    : access View_Interface;
      Cell : access Cell_Record'Class) 
     return access Cell_State_Record'Class is abstract;
   -- Shortcut to validateCellState with recurse set to true.
   
   function Validate_Cell_State
     (V       : access View_Interface;
      Cell    : access Cell_Record'Class;
      Recurse : Boolean) return access Cell_State_Record'Class is abstract;
   -- Validates the cell state for the given cell.
   -- @param cell
   --            Cell whose cell state should be validated.
   -- @param recurse
   --            Boolean indicating if the children of the cell should be
   --            validated.
   
   procedure Update_Cell_State
     (V     : access View_Interface;
      State : access Cell_State_Record'Class) is abstract;
   -- Updates the given cell state.
   -- @param state
   --            Cell state to be updated.
	  
   procedure Update_Vertex_State
     (V     : access View_Interface;
      State : access Cell_State_Record'Class;
      Geo   : access Cell_Geometry_Record'Class) is abstract;
   -- Validates the given cell state.
   
   procedure Update_Edge_State
     (V     : access View_Interface;
      State : access Cell_State_Record'Class;
      Geo   : access Cell_Geometry_Record'Class) is abstract;
   -- Validates the given cell state.
   
   procedure Update_Vertex_Label_Offset
     (V     : access View_Interface;
      State : access Cell_State_Record'Class) is abstract;
   -- Updates the absoluteOffset of the given vertex cell state. This takes
   -- into account the label position styles.
   -- @param state
   --            Cell state whose absolute offset should be updated.
   
   procedure Update_Label
     (V     : access View_Interface;
      State : access Cell_State_Record'Class) is abstract;
   -- Updates the label of the given state.
   
   
   --------------------------------------------------------
   
   function Get_Word_Wrap_Width
     (V     : access View_Interface;
      State : access Cell_State_Record'Class) return Coordinate is abstract;
   -- Returns the width for wrapping the label of the given state at scale 1.
   
   procedure Update_Label_Bounds
     (V     : access View_Interface;
      State : access Cell_State_Record'Class) is abstract;
   -- Updates the label bounds in the given state.
   
   procedure Update_Bounding_Box
     (V     : access View_Interface;
      State : access Cell_State_Record'Class) is abstract;
   -- return Rectangle_Record is abstract;
   -- Updates the bounding box in the given cell state.
   -- @param state
   --            Cell state whose bounding box should be updated.
   
   procedure Update_Fixed_Terminal_Points
     (V      : access View_Interface;
      Edge   : access Cell_State_Record'Class;
      Source : access Cell_State_Record'Class;
      Target : access Cell_State_Record'Class) is abstract;
   -- Sets the initial absolute terminal points in the given state before the
   -- edge style is computed.
   -- @param edge
   --            Cell state whose initial terminal points should be updated.
   -- @param source
   --            Cell state which represents the source terminal.
   -- @param target
   --            Cell state which represents the target terminal.
   
   procedure Update_Fixed_Terminal_Point
     (V          : access View_Interface;
      Edge       : access Cell_State_Record'Class;
      Terminal   : access Cell_State_Record'Class;
      Source     : Boolean;
      Constraint : access Connection_Constraint_Record'Class) is abstract;
   -- Sets the fixed source or target terminal point on the given edge.
   -- @param edge
   --            Cell state whose initial terminal points should be updated.
   
   procedure Update_Points
     (V      : access View_Interface;
      Edge   : access Cell_State_Record'Class;
      Points : Point_Lists.List;
      Source : access Cell_State_Record'Class;
      Target : access Cell_State_Record'Class) is abstract;
   -- Updates the absolute points in the given state using the specified array
   -- of points as the relative points.
   -- @param edge
   --            Cell state whose absolute points should be updated.
   -- @param points
   --            Array of points that constitute the relative points.
   -- @param source
   --            Cell state that represents the source terminal.
   -- @param target
   --            Cell state that represents the target terminal.
   
   function Transform_Control_Point
     (V     : access View_Interface;
      State : access Cell_State_Record'Class;
      Point : Point_Record) return Point_Record is abstract;
   -- Transforms the given control point to an absolute point.
   
   function Get_Edge_Style
     (V      : access View_Interface;
      Edge   : access Cell_State_Record'Class;
      Points : Point_Lists.List;
      Source : access Cell_State_Record'Class;
      Target : access Cell_State_Record'Class) 
     return Name_Id is abstract; -- Access Edge_Style_Function_Interface'Class is abstract;
   -- Returns the edge style function to be used to compute the absolute points
   -- for the given state, control points and terminals.
   
   procedure Update_Floating_Terminal_Points
     (V      : access View_Interface;
      State  : access Cell_State_Record'Class;
      Source : access Cell_State_Record'Class;
      Target : access Cell_State_Record'Class) is abstract;
   -- Updates the terminal points in the given state after the edge style was
   -- computed for the edge.
   -- @param state
   --            Cell state whose terminal points should be updated.
   -- @param source
   --            Cell state that represents the source terminal.
   -- @param target
   --            Cell state that represents the target terminal.
   
   procedure Update_Floating_Terminal_Point
     (V      : access View_Interface;
      Edge   : access Cell_State_Record'Class;
      Start  : access Cell_State_Record'Class;
      Ends   : access Cell_State_Record'Class;
      Source : Boolean) is abstract;
   -- Updates the absolute terminal point in the given state for the given
   -- start and end state, where start is the source if source is true.
   -- @param edge
   --            Cell state whose terminal point should be updated.
   -- @param start
   --            Cell state for the terminal on "this" side of the edge.
   -- @param end
   --            Cell state for the terminal on the other side of the edge.
   -- @param source
   --            Boolean indicating if start is the source terminal state.
   
   function Get_Terminal_Port
     (V        : access View_Interface;
      State    : access Cell_State_Record'Class;
      Terminal : access Cell_State_Record'Class;
      Source   : Boolean) return access Cell_State_Record'Class is abstract;
   -- Returns a cell state that represents the source or target terminal or
   -- port for the given edge.
   
   function Get_Perimeter_Point
     (V          : access View_Interface;
      Terminal   : access Cell_State_Record'Class;
      Next       : Point_Record;
      Orthogonal : Boolean) return Point_Record is abstract;
   -- Returns a point that defines the location of the intersection point
   -- between the perimeter and the line between the center of the shape and
   -- the given point.
   
   function Get_Perimeter_Point
     (V          : access View_Interface;
      Terminal   : access Cell_State_Record'Class;
      Next       : Point_Record;
      Orthogonal : Boolean;
      Border     : Coordinate) return Point_Record is abstract;
   -- Returns a point that defines the location of the intersection point
   -- between the perimeter and the line between the center of the shape and
   -- the given point.
   -- @param terminal
   --            Cell state for the source or target terminal.
   -- @param next
   --            Point that lies outside of the given terminal.
   -- @param orthogonal
   --            Boolean that specifies if the orthogonal projection onto the
   --            perimeter should be returned. If this is false then the
   --            intersection of the perimeter and the line between the next
   --            and the center point is returned.
   -- @param border
   --            Optional border between the perimeter and the shape.
   
   function Get_Routing_Center_X
     (V     : access View_Interface;
      State : access Cell_State_Record'Class) return Coordinate is abstract;
   -- Returns the x-coordinate of the center point for automatic routing.
   -- @return Returns the x-coordinate of the routing center point.

   function Get_Routing_Center_y
     (V     : access View_Interface;
      State : access Cell_State_Record'Class) return Coordinate is abstract;
   -- Returns the y-coordinate of the center point for automatic routing.
   -- @return Returns the y-coordinate of the routing center point.
   
   function Get_Perimeter_Bounds
     (V          : access View_Interface;
      Terminal   : access Cell_State_Record'Class;
      Border     : Coordinate) return Rectangle_Record is abstract;
   -- Returns the perimeter bounds for the given terminal, edge pair.
   
   function Get_Perimeter_Function
     (V     : access View_Interface;
      State : access Cell_State_Record'Class) return Name_Id is abstract;
   -- Returns the perimeter function for the given state.
   
   function Get_Next_Point
     (V        : access View_Interface;
      Edge     : access Cell_State_Record'Class;
      Opposite : access Cell_State_Record'Class;
      Source   : Boolean) return Point_Record is abstract;
   -- Returns the nearest point in the list of absolute points or the center of
   -- the opposite terminal.
   -- @param edge
   --            Cell state that represents the edge.
   -- @param opposite
   --            Cell state that represents the opposite terminal.
   -- @param source
   --            Boolean indicating if the next point for the source or target
   --            should be returned.
   -- @return Returns the nearest point of the opposite side.
   
   function Get_Visible_Terminal
     (V        : access View_Interface;
      Edge     : access Cell_Record'Class;
      Source   : Boolean) return access Cell_Record'Class is abstract;
   -- Returns the nearest ancestor terminal that is visible. The edge appears
   -- to be connected to this terminal on the display.
   -- @param edge
   --            Cell whose visible terminal should be returned.
   -- @param source
   --            Boolean that specifies if the source or target terminal should
   --            be returned.
   -- @return Returns the visible source or target terminal.
   
   procedure Update_Edge_Bounds
     (V     : access View_Interface;
      State : access Cell_State_Record'Class) is abstract;
   -- Updates the given state using the bounding box of the absolute points.
   -- Also updates terminal distance, length and segments.
   -- @param state
   --            Cell state whose bounds should be updated.
   
   function Get_Point 
     (V     : access View_Interface;
      State : access Cell_State_Record'Class) return Point_Record is abstract;
   -- Returns the absolute center point along the given edge.
   
   function Get_Point
     (V        : access View_Interface;
      State    : access Cell_State_Record'Class;
      Geometry : access Cell_Geometry_Record'Class) 
     return Point_Record is abstract;
   -- Returns the absolute point on the edge for the given relative geometry as
   -- a point. The edge is represented by the given cell state.
   -- @param state
   --            Represents the state of the parent edge.
   -- @param geometry
   --            Optional geometry that represents the relative location.
   -- @return Returns the mxpoint that represents the absolute location of the
   --         given relative geometry.
   
   function Get_Relative_Point
     (V          : access View_Interface;
      Edge_State : access Cell_State_Record'Class;
      X          : Coordinate;
      Y          : Coordinate) return Point_Record is abstract;
   -- Gets the relative point that describes the given, absolute label position
   -- for the given edge state.
   
   function Get_Cell_States
     (V     : access View_Interface;
      Cells : Cells_Lists.List) return Cells_States_Lists.List is abstract;
   -- Returns the states for the given array of cells. The array contains all
   -- states that are not null, that is, the returned array may have less
   -- elements than the given array.
   
   function Get_State
     (V    : access View_Interface;
      Cell : access Cell_Record'Class)
     return access Cell_State_Record'Class is abstract;
   -- Returns the state for the given cell or null if no state is defined for
   -- the cell.
   -- @param cell
   --            Cell whose state should be returned.
   -- @return Returns the state for the given cell.
   
   function Get_State
     (V      : access View_Interface;
      Cell   : access Cell_Record'Class;
      Create : Boolean) return access Cell_State_Record'Class is abstract;
   -- Returns the cell state for the given cell. If create is true, then the
   -- state is created if it does not yet exist.
   -- @param cell
   --            Cell for which a new state should be returned.
   -- @param create
   --            Boolean indicating if a new state should be created if it does
   --            not yet exist.
   -- @return Returns the state for the given cell.
   
   procedure Remove_State
     (V    : access View_Interface;
      Cell : access Cell_Record'Class) is abstract;
   -- Removes and returns the mxCellState for the given cell.
   -- @param cell
   --            mxCell for which the mxCellState should be removed.
   
   function Create_State
     (V    : access View_Interface;
      Cell : access Cell_Record'Class) 
     return access Cell_State_Record'Class is abstract;
   -- Creates and returns a cell state for the given cell.
   -- @param cell
   --            Cell for which a new state should be created.
   -- @return Returns a new state for the given cell.
   
end Artics.Graph.Views_Interfaces;
