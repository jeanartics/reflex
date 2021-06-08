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

package Reflex.Boxes.Duals is

   type Dual_Box_Record is new Box_Record with private;
   type Dual_Box_Ptr is access all Dual_Box_Record;
   type Dual_Box_Class_Ptr is access all Dual_Box_Record'Class;

   No_Dual_Box_Record : constant Dual_Box_Record;

   function New_Dual_Box return Dual_Box_Ptr;

   procedure Free_Dual_Box (This : in out Dual_Box_Ptr);

   function Get_Box1
     (This : access Dual_Box_Record) return access Box_Record'Class;
   procedure Set_Box1
     (This : access Dual_Box_Record;
      Box  : access Box_Record'Class);

   function Get_Box2
     (This : access Dual_Box_Record) return access Box_Record'Class;
   procedure Set_Box2
     (This : access Dual_Box_Record;
      Box  : access Box_Record'Class);

   procedure Absolute_Place_Box (This : access Dual_Box_Record);

   procedure Place_Matrix
     (This   : access Dual_Box_Record;
      Matrix : access Matrices.Matrix_Record);

   procedure Dump_Box (This : access Dual_Box_Record);

private

   type Dual_Box_Record is new Box_Record with record
      Box1 : access Box_Record'Class;
      Box2 : access Box_Record'Class;
   end record;

   No_Dual_Box_Record : constant Dual_Box_Record :=
     (No_Box_Record with
      Box1        => null,
      Box2        => null);

end Reflex.Boxes.Duals;
