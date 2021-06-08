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

-- The Dimension class encapsulates the width and height of a component (in
-- integer precision) in a single object. The class is associated with certain
-- properties of components. Several methods defined by the Component class and
-- the LayoutManager interface return a Dimension object. Normally the values
-- of width and height are non-negative integers. The constructors that allow
-- you to create a dimension do not prevent you from setting a negative value
-- for these properties. If the value of width or height is negative, the
-- behavior of some methods defined by other objects is undefined.

with Artics.Maths; use Artics.Maths;

-- Implements a 2-dimensional point with double precision coordinates.

package Artics.Geometry.Dimensions is

   type Dimension_Record is record
      Width  : Coordinate;
      Height : Coordinate;
   end record;
   
   function New_Dimension return Dimension_Record;
   function New_Dimension
     (Width  : Coordinate;
      Height : Coordinate) return Dimension_Record;
   
   function Get_Width (D : Dimension_Record) return Coordinate;
   procedure Set_Width
     (D : in out Dimension_Record;
      W : Coordinate);
   
   function Get_Height (D : Dimension_Record) return Coordinate;
   procedure Set_Height
     (D : in out Dimension_Record;
      H : Coordinate);
   
   procedure Set_Size
     (D      : in out Dimension_Record;
      Width  : Coordinate;
      Height : Coordinate);
   procedure Set_Size
     (D      : in out Dimension_Record;
      Width  : Integer;
      Height : Integer);
   -- Sets the size of this Dimension object to the specified width and height
   -- in double precision. Note that if width or height are larger than 
   -- Integer.MAX_VALUE they willbe reset to Integer.MAX_VALUE
   
   function To_String (D : Dimension_Record) return String;

   No_Dimension_Record : constant Dimension_Record := Dimension_Record'
     (Width  => Coordinate'Last, 
      Height => Coordinate'Last);
   
   Zero_Dimension_Record : constant Dimension_Record := Dimension_Record'
     (Width  => 0.0,
      Height => 0.0);
   
end Artics.Geometry.Dimensions;
