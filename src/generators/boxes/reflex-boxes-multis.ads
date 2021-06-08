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

package Reflex.Boxes.Multis is
   
   type Multi_Box_Record is new Box_Record with private;
   type Multi_Box_Ptr is access all Multi_Box_Record;
   type Multi_Box_Class_Ptr is access all Multi_Box_Record'Class;
   
   No_Multi_Box_Record : constant Multi_Box_Record;
   
   function New_Multi_Box return Multi_Box_Ptr;
   
   procedure Free_Multi_Box (This : in out Multi_Box_Ptr);
   
   function Get_Children 
     (This : access Multi_Box_Record) return Boxes_Lists.List;
   procedure Set_Children
     (This   : access Multi_Box_Record;
      Childs : Boxes_Lists.List);
   
   procedure Append_Child_Box 
     (This : access Multi_Box_Record;
      Box  : access Box_Record'Class);
   
   procedure Absolute_Place_Box (This : access Multi_Box_Record);

   procedure Place_Matrix
     (This   : access Multi_Box_Record;
      Matrix : access Matrices.Matrix_Record);
   
   procedure Dump_Box (This : access Multi_Box_Record);

private
   
   type Multi_Box_Record is new Box_Record with record
      Childs : Boxes_Lists.List;
   end record;
   
   No_Multi_Box_Record : constant Multi_Box_Record :=
     (No_Box_Record with
      Childs => Boxes_Lists.Empty_List);
   
end Reflex.Boxes.Multis;
