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
-- Reflex is originally developed  by the Artics team at Grenoble.          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation; 

with Types; use Types;

package Reflex.Boxes.Coils is
   
   type Coil_Box_Record is new Box_Record with private;
   type Coil_Box_Ptr is access all Coil_Box_Record;
   type Coil_Box_Class_Ptr is access all Coil_Box_Record'Class;
   
   No_Coil_Box_Record : constant Coil_Box_Record;
   
   function New_Coil_Box return Coil_Box_Ptr;
   
   procedure Free_Coil_Box (This : in out Coil_Box_Ptr);
   
   procedure Place_Box (This : access Coil_Box_Record);
   procedure Absolute_Place_Box (This : access Coil_Box_Record);
   
private
   
   type Coil_Box_Record is new Box_Record with null record;
   
   No_Coil_Box_Record : constant Coil_Box_Record :=
     (No_Box_Record with null record);
   
end Reflex.Boxes.Coils;
