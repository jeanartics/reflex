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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;

with Ada.Numerics.Generic_Elementary_Functions;

package Artics.Geometry is
   
   -- subtype Coordinate is Gtkada.Canvas_View.Model_Coordinate; -- Float;
   subtype Coordinate is Float;
   
   No_Coordinate : constant Coordinate := Coordinate'Last;
   Zero_Coordinate : constant Coordinate := 0.0;
   
   package Coordinate_Elementary_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Coordinate);
   use Coordinate_Elementary_Functions;
   
   package Coordinates_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Coordinate);

end Artics.Geometry;
