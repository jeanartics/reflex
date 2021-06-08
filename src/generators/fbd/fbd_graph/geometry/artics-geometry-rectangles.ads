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

pragma Style_Checks (Off);

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;

with Artics.Types; use Artics.Types;
with Artics.Generic_Lists;
with Artics.Geometry.Points; use Artics.Geometry.Points;
with Artics.Geometry.Dimensions; use Artics.Geometry.Dimensions;

-- Implements a 2-dimensional rectangle with double precision coordinates.

package Artics.Geometry.Rectangles is

   type Rectangle_Record is record
      Origin : Point_Record;
      Width  : Coordinate;
      Height : Coordinate;
   end record;
   
   function New_Rectangle return Rectangle_Record;
   -- Constructs a new rectangle at (0, 0) with the width and height set to 0.
   
   function New_Rectangle (Rect : Rectangle_Record)  return Rectangle_Record; 
   -- Constructs a copy of the given rectangle.
   -- @param rect Rectangle to construct a copy of.
  
   function New_Rectangle
     (X      : Coordinate;
      Y      : Coordinate;
      Width  : Coordinate;
      Height : Coordinate) return Rectangle_Record;
   -- Constructs a rectangle using the given parameters.
   -- param x X-coordinate of the new rectangle.
   -- param y Y-coordinate of the new rectangle.
   -- param width Width of the new rectangle.
   -- param height Height of the new rectangle.
   
   function New_Rectangle (D : Dimension_Record)  return Rectangle_Record; 
   -- Constructs a rectangle with dimension D at 0.0,0. for x,y
  
   function Get_Origin (R : Rectangle_Record) return Point_record;
   procedure Set_Origin
     (R : in out Rectangle_Record;
      C : Point_record);
   
   function Get_X (R : Rectangle_Record) return Coordinate;
   procedure Set_X
     (R : in out Rectangle_Record;
      X : Coordinate);
   -- Returns the x-coordinate of the origin
   
   function Get_Y (R : Rectangle_Record) return Coordinate;
   procedure Set_Y
     (R : in out Rectangle_Record;
      Y : Coordinate);
   -- Returns the y-coordinate of the origin.
   
   function Get_Width (R : Rectangle_Record) return Coordinate;
   procedure Set_Width
     (R : in out Rectangle_Record;
      W : Coordinate);
   --  Returns or Sets the width of the rectangle.
   
   function Get_Height (R : Rectangle_Record) return Coordinate;
   procedure Set_Height
     (R : in out Rectangle_Record;
      H : Coordinate);
   --  Returns or Sets the height of the rectangle.
   
   procedure Set_Rect
     (R : in out Rectangle_Record;
      X : Coordinate;
      Y : Coordinate;
      W : Coordinate;
      H : Coordinate);
   -- Sets this rectangle to the specified values
   -- @param x the new x-axis position
   -- @param y the new y-axis position
   -- @param w the new width of the rectangle
   -- @param h the new height of the rectangle
   
   procedure Add
     (R1 : in out Rectangle_Record;
      R2 : Rectangle_Record);
   -- Adds the given rectangle to this rectangle.
   
   procedure Add
     (R1    : in out Rectangle_Record;
      Point : Point_Record);
   -- Adds the given point to this rectangle.
   
   function Get_Center_X (R : Rectangle_Record) return Coordinate;
   -- Returns the x-coordinate of the center.
   -- @return Returns the x-coordinate of the center.

   function Get_Center_Y (R : Rectangle_Record) return Coordinate;
   -- Returns the y-coordinate of the center.
   -- @return Returns the y-coordinate of the center.
   
   procedure Grow
     (R      : in out Rectangle_Record;
      Amount : Coordinate);
   -- Grows the rectangle by the given amount, that is, this method subtracts
   -- the given amount from the x- and y-coordinates and adds twice the amount
   -- to the width and height.   
   
   procedure Translate
     (R  : in out rectangle_Record;
      Dx : Float;
      Dy : Float);
   
   function Contains
     (R : Rectangle_Record;
      X : Coordinate;
      Y : Coordinate) return Boolean;
   function Contains
     (R : Rectangle_Record;
      P : Point_Record) return Boolean;
   -- Returns true if the given point is contained in the rectangle.
   
   function Intersects
     (R1 : Rectangle_Record;
      R2 : rectangle_Record) return Boolean;
   -- Returns the point at which the specified point intersects the perimeter 
   -- of this rectangle or null if there is no intersection.   
   
   function Intersect_Line
     (R  : Rectangle_Record;
      P1 : Point_Record;
      P2 : Point_Record) return Point_Record;
   function Intersect_Line
     (R  : Rectangle_Record;
      P1 : Point_Record;
      P2 : Point_Record) return Boolean;
   -- Returns the point at which the specified point intersects the perimeter 
   -- of this rectangle or null if there is no intersection.   
   
   function Get_Rectangle (R : Rectangle_Record) return Rectangle_Record;
   -- Returns the bounds as a new rectangle.
   -- @return Returns a new rectangle for the bounds.
   
   function Equals
     (R1 : Rectangle_Record;
      R2 : Rectangle_Record) return Boolean;
   -- Returns true if the given object equals this rectangle.
   
   function Clone (R : Rectangle_Record) return Rectangle_Record;
   -- Returns a new instance of the same rectangle.
   
   function To_String (R : Rectangle_Record) return String;
   
   No_Rectangle_Record : constant Rectangle_Record := Rectangle_Record'
     (Origin => No_Point_Record,
      Width  => Coordinate'Last,
      Height => Coordinate'Last);
   
   Zero_Rectangle_Record : constant Rectangle_Record := Rectangle_Record'
     (Origin => Zero_Point_Record,
      Width  => 0.0,
      Height => 0.0);
   
   package Rectangles_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Rectangle_Record);
   use Rectangles_Lists;
   
end Artics.Geometry.Rectangles;
