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

with Reflex.Boxes; use Reflex.Boxes;

package Reflex.Ladder.Cells is

   type Cell_Record is tagged private;
   type Cell_Ptr is access all Cell_Record;
   type Cell_Class_Ptr is access all Cell_Record'Class;
   
   type Cell_Kind is
     (Empty_Cell,
      Box_Cell,
      Hlink_Cell,
      Busy_Cell);
   
   function New_Cell return Cell_Ptr;
   
   procedure Initialize_Cell (This : access Cell_Record);

   procedure Free_Cell (This : in out Cell_Ptr);
   
   function Has_Open_Vlink (This : access Cell_Record) return Boolean;
   procedure Set_Open_Vlink
     (This : access Cell_Record;
      V    : Boolean);
   
   function Has_Closed_Vlink (This : access Cell_Record) return Boolean;
   procedure Set_Closed_Vlink
     (This : access Cell_Record;
      V    : Boolean);
      
   function Get_Cell_Kind (This : access Cell_Record) return Cell_Kind;
   procedure Set_Cell_Kind
     (This : access Cell_Record;
      K    : Cell_Kind);
      
   function Get_Cell_Box
     (This : access Cell_Record) return access Box_Record'Class;
   procedure Set_Cell_Box
     (This : access Cell_Record;
      B    : access Box_Record'Class);
   
private
   
   type Cell_Record is tagged record
      Open_Vlink : Boolean;
      Closed_Vlink : Boolean;
      
      Kind : Cell_Kind;
      
      Box : access Box_Record'Class;
   end record;
			      
   No_Cell_Record : constant Cell_Record :=
     (Open_Vlink   => False,
      Closed_Vlink => False,
      Kind         => Empty_Cell,
      Box          => null);

end Reflex.Ladder.Cells;
