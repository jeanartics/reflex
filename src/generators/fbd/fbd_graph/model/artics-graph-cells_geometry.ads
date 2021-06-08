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
with Artics.Output; use Artics.Output;
with Artics.Objects; use Artics.Objects;
with Artics.Geometry; use Artics.Geometry;
with Artics.Geometry.Points; use Artics.Geometry.Points;
with Artics.Geometry.Rectangles; use Artics.Geometry.Rectangles;

-- Implements a 2-dimensional rectangle with double precision coordinates.

package Artics.Graph.Cells_Geometry is
   
   type Cell_Geometry_Record is new Object_Record with private;
   type Cell_Geometry_Ptr is access all Cell_Geometry_Record;
   type Cell_Geometry_Class_Ptr is access all Cell_Geometry_Record'Class;
   
   No_Cell_Geometry_Record : constant Cell_Geometry_Record;
   
   function New_Cell_Geometry return Cell_Geometry_Ptr;
   -- Constructs a new rectangle at (0, 0) with the width and height set to 0.
   
   function New_Cell_Geometry
     (X      : Coordinate;
      Y      : Coordinate;
      Width  : Coordinate;
      height : Coordinate) return Cell_Geometry_Ptr;
   -- Constructs a geometry using the given parameters.
   -- @param x X-coordinate of the new geometry.
   -- @param y Y-coordinate of the new geometry.
   -- @param width Width of the new geometry.
   -- @param height Height of the new geometry.
   
   function Geometry_Rectangle
     (G : access Cell_Geometry_Record) return Rectangle_Record;
   procedure Set_Geometry_Rectangle
     (G : access Cell_Geometry_Record;
      R : Rectangle_Record);
   -- The internal cell rectangle
      
   function Get_Translate_Control_Points
     (G : access Cell_Geometry_Record) return Boolean;
   procedure Set_Translate_Control_Points
     (G : access Cell_Geometry_Record;
      B : Boolean);
   -- Global switch to translate the points in translate. Default is true.
      
   function Get_Alternate_Bounds
     (G : access Cell_Geometry_Record) return Rectangle_Record;
   procedure Set_Alternate_Bounds
     (G : access Cell_Geometry_Record;
      R : Rectangle_Record);
   -- Stores alternate values for x, y, width and height in a rectangle.
   -- Default is null.
   
   function Get_Source_Point
     (G : access Cell_Geometry_Record) return Point_Record;
   procedure Set_Source_Point
     (G : access Cell_Geometry_Record;
      S : Point_Record);
   
   function Get_Target_Point
     (G : access Cell_Geometry_Record) return Point_Record;
   procedure Set_Target_Point
     (G : access Cell_Geometry_Record;
      T : Point_Record);
   
   function Get_Terminal_Point
     (G      : access Cell_Geometry_Record;
      Source : boolean) return Point_Record;
   procedure Set_Terminal_Point
     (G      : access Cell_Geometry_Record;
      T      : Point_Record;
      Source : Boolean);
   -- Returns the point representing the source or target point of this edge.
   -- This is only used if the edge has no source or target vertex.
   
   function Get_Points
     (G : access Cell_Geometry_Record) return Point_Lists.List;
   procedure Set_Points
     (G : access Cell_Geometry_Record;
      L : Point_Lists.List);
   -- List of Points which specifies the control points along the edge.
   -- These points are the intermediate points on the edge, for the
   -- endpoints use targetPoint and sourcePoint or set the terminals of the
   -- edge to a non-null value. Default is null.      
   
   function Get_Offset (G : access Cell_Geometry_Record) return Point_Record;
   procedure Set_Offset
     (G : access Cell_Geometry_Record;
      O : Point_Record);
   -- Holds the offset of the label for edges. This is the absolute vector
   -- between the center of the edge and the top, left point of the label.
   -- Default is null.
   
   function Is_Relative (G : access Cell_Geometry_Record) return Boolean;
   procedure Set_Relative
     (G : access Cell_Geometry_Record;
      R : Boolean);
   -- Specifies if the coordinates in the geometry are to be interpreted as
   -- relative coordinates. Default is false. This is used to mark a
   -- geometry with an x- and y-coordinate that is used to describe an edge
   -- label position, or a relative location with respect to a parent cell's
   -- width and height.
      
   procedure Swap (G : access Cell_Geometry_Record);
   -- Swaps the x, y, width and height with the values stored in
   -- alternateBounds and puts the previous values into alternateBounds as
   -- a rectangle. This operation is carried-out in-place, that is, using the
   -- existing geometry instance. If this operation is called during a graph
   -- model transactional change, then the geometry should be cloned before
   -- calling this method and setting the geometry of the cell using
   -- mxGraphModel.setGeometry.
   
   procedure Translate
     (G  : access Cell_Geometry_Record;
      Dx : Coordinate;
      Dy : Coordinate);
   -- Translates the geometry by the specified amount. That is, x and y of the
   -- geometry, the sourcePoint, targetPoint and all elements of points are
   -- translated by the given amount. X and y are only translated if the
   -- geometry is not relative. If TRANSLATE_CONTROL_POINTS is false, then
   -- are not modified by this function.   
   
   function Get_Origin (G : access Cell_Geometry_Record) return Point_record;
   procedure Set_Origin
     (G : access Cell_Geometry_Record;
      P : Point_record);
   
   function Get_X (G : access Cell_Geometry_Record) return Coordinate;
   procedure Set_X
     (G : access Cell_Geometry_Record;
      X : Coordinate);
   -- Returns the x-coordinate of the center.
   
   function Get_Y (G : access Cell_Geometry_Record) return Coordinate;
   procedure Set_Y
     (G : access Cell_Geometry_Record;
      Y : Coordinate);
   -- Returns the y-coordinate of the center.
   
   function Get_Width (G : access Cell_Geometry_Record) return Coordinate;
   procedure Set_Width
     (G : access Cell_Geometry_Record;
      W : Coordinate);
   --  Returns or Sets the width of the rectangle.
   
   function Get_Height (G : access Cell_Geometry_Record) return Coordinate;
   procedure Set_Height
     (G : access Cell_Geometry_Record;
      H : Coordinate);
   --  Returns or Sets the height of the rectangle.
   
   function Clone
     (G : access Cell_Geometry_Record) return access Cell_Geometry_Record;
   -- Returns a clone of the cell.
   
   procedure Generate_Xml_Cell_Geometry
     (This : access Cell_Geometry_Record;
      Ob   : Output_Buffer);
   
   procedure Generate_Xml_Vertex_Geometry
     (This : access Cell_Geometry_Record;
      Ob   : Output_Buffer);
   
   procedure Generate_Xml_Edge_Geometry
     (This : access Cell_Geometry_Record;
      Ob   : Output_Buffer);
   
private
   
   type Cell_Geometry_Record is new Object_Record with record
      Rect : Rectangle_Record;
      -- A cell is always considered as a rectangle
      
      Translate_Control_Points : Boolean;
      -- Global switch to translate the points in translate. Default is true.
      
      Alternate_Bounds : Rectangle_Record;
      -- Stores alternate values for x, y, width and height in a rectangle.
      -- Default is null.
      
      Source_Point : Point_Record;
      Target_Point : Point_Record;
      -- Defines the source- and target-point of the edge. This is used if the
      -- corresponding edge does not have a source vertex. Otherwise it is
      -- ignored. Default is null.      
      
      Points : Point_Lists.List;
      -- List of Points which specifies the control points along the edge.
      -- These points are the intermediate points on the edge, for the
      -- endpoints use targetPoint and sourcePoint or set the terminals of the
      -- edge to a non-null value. Default is null.      
      
      Offset : Point_Record;
      -- Holds the offset of the label for edges. This is the absolute vector
      -- between the center of the edge and the top, left point of the label.
      -- Default is null.
      
      Relative : Boolean;
      -- Specifies if the coordinates in the geometry are to be interpreted as
      -- relative coordinates. Default is false. This is used to mark a
      -- geometry with an x- and y-coordinate that is used to describe an edge
      -- label position, or a relative location with respect to a parent cell's
      -- width and height.
      
   end record;
   
   No_Cell_Geometry_Record : constant Cell_Geometry_Record := 
     Cell_Geometry_Record'
     (No_Object_Record with 
	Rect                   => Zero_Rectangle_Record,
      Translate_Control_Points => True,
      Alternate_Bounds         => No_Rectangle_Record,
      Source_Point             => No_Point_Record,
      Target_Point             => No_Point_Record,
      Points                   => Point_Lists.Empty_List,
      Offset                   => No_Point_Record,
      Relative                 => False);
   
end Artics.Graph.Cells_Geometry;
