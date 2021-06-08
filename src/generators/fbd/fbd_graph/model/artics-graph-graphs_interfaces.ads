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

with Artics.Utils; use Artics.Utils;
with Artics.Geometry; use Artics.Geometry;
with Artics.Geometry.Points; use Artics.Geometry.Points;
with Artics.Geometry.Rectangles; use Artics.Geometry.Rectangles;

with Artics.Graph.Cells; use Artics.Graph.Cells;
with Artics.Graph.Cells_States; use Artics.Graph.Cells_States;
with Artics.Graph.Models_Interfaces; use Artics.Graph.Models_Interfaces;
with Artics.Graph.Cells_Geometry; use Artics.Graph.Cells_Geometry;
with Artics.Graph.Connection_Constraints; use Artics.Graph.Connection_Constraints;

-- Implements a graph object that allows to create diagrams from a graph model
-- and stylesheet.

package Artics.Graph.Graphs_Interfaces is
   
   type Graph_Interface is interface;
   
   function Get_Model 
     (G : access Graph_Interface) 
      return access Model_Interface'Class is abstract;
   procedure Set_Model
     (G : access Graph_Interface;
      M : access Model_Interface'Class) is abstract;  
   -- Holds the model that contains the cells to be displayed.
   
   function Get_Translate_For_Root
     (G : access Graph_Interface;
      C : access Cell_Record'Class) return Point_Record is abstract;
   -- Returns the translation to be used if the given cell is the root cell as
   -- an <mxPoint>. This implementation returns null.
   -- @param cell Cell that represents the root of the view.
   -- @return Returns the translation of the graph for the given root cell.
   
   function Get_Child_Offset_For_Cell
     (G    : access Graph_Interface;
      Cell : access Cell_Record'Class) return Point_Record is abstract;
   
   function Get_Cell_Geometry
     (G    : access Graph_Interface;
      Cell : access Cell_Record'Class) 
      return access Cell_Geometry_Record'Class is abstract;
   
   function Get_Label 
     (G    : access Graph_Interface;
      Cell : access Cell_Record'Class) return String is abstract;
   -- Returns a string or DOM node that represents the label for the given
   -- cell. This implementation uses <convertValueToString> if <labelsVisible>
   -- is true. Otherwise it returns an empty string.
   -- @param cell <mxCell> whose label should be returned.
   -- @return Returns the label for the given cell.
   
   function Is_Html_Label
     (G    : access Graph_Interface;
      Cell : access Cell_Record'Class) return Boolean is abstract;
   
   function Get_Connection_Constraint
     (G        : access Graph_Interface;
      Edge     : access Cell_State_Record'Class;
      Terminal : access Cell_State_Record'Class;
      Source   : Boolean) 
      return access Connection_Constraint_Record'Class is abstract;
   -- Returns an connection constraint that describes the given connection
   -- point. This result can then be passed to getConnectionPoint.
   -- @param edge Cell state that represents the edge.
   -- @param terminal Cell state that represents the terminal.
   -- @param source Boolean indicating if the terminal is the source or target.
   
   function Get_Connection_Point
     (G          : access Graph_Interface;
      Vertex     : access Cell_State_Record'Class;
      Constraint : access Connection_Constraint_Record'Class) 
      return Point_Record is abstract;
   -- Sets the connection constraint that describes the given connection point.
   -- If no constraint is given then nothing is changed. To remove an existing
   -- constraint from the given edge, use an empty constraint instead.
   -- @param vertex Cell state that represents the vertex.
   -- @param constraint Connection constraint that represents the connection 
   -- point constraint as returned by getConnectionConstraint.
   
   function Is_Orthogonal
     (G    : access Graph_Interface;
      Edge : access Cell_State_Record'Class) return Boolean is abstract;
   -- Returns true if perimeter points should be computed such that the
   -- resulting edge has only horizontal or vertical segments.
   -- @param edge Cell state that represents the edge.
   
   function Get_Paint_Bounds
     (G     : access Graph_Interface;
      Cells : Cells_Lists.List) return Rectangle_Record is abstract;
   -- Returns the bounding box of the given cells and their descendants.
   
   function Is_Cell_Selectable
     (G    : access Graph_Interface;
      Cell : access Cell_Record'Class) return Boolean is abstract;
   
end Artics.Graph.Graphs_Interfaces;

