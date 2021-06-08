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

package body Reflex.Ladder.Cells is

   --------------
   -- New_Cell --
   --------------
   
   function New_Cell return Cell_Ptr is
      This : Cell_Ptr := new Cell_Record'(No_Cell_Record);
   begin
      return This;
   end New_Cell;
   
   ---------------------
   -- Initialize_Cell --
   ---------------------
   
   procedure Initialize_Cell (This : access Cell_Record) is
   begin
      This.all := No_Cell_Record;
   end Initialize_Cell;
   
   ---------------
   -- Free_Cell --
   ---------------
   
   procedure Free_Cell (This : in out Cell_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Cell_Record, Cell_Ptr);
   begin
      if This /= null then
	 Free (This);
      end if;
   end Free_Cell;
   
   --------------------
   -- Has_Open_Vlink --
   --------------------
   
   function Has_Open_Vlink (This : access Cell_Record) return Boolean is
   begin
      return This.Open_Vlink;
   end Has_Open_Vlink;
   
   --------------------
   -- Set_Open_Vlink --
   --------------------
   
   procedure Set_Open_Vlink
     (This : access Cell_Record;
      V    : Boolean) is
   begin
      This.Open_Vlink := V;
   end Set_Open_Vlink;
   
   ----------------------
   -- Has_Closed_Vlink --
   ----------------------
   
   function Has_Closed_Vlink (This : access Cell_Record) return Boolean is
   begin
      return This.Closed_Vlink;
   end Has_Closed_Vlink;
   
   ----------------------
   -- Set_Closed_Vlink --
   ----------------------
   
   procedure Set_Closed_Vlink
     (This : access Cell_Record;
      V    : Boolean) is
   begin
      This.Closed_Vlink := V;
   end Set_Closed_Vlink;
   
   -------------------
   -- Get_Cell_Kind --
   -------------------
   
   function Get_Cell_Kind (This : access Cell_Record) return Cell_Kind is
   begin
      return This.Kind;
   end Get_Cell_Kind;
   
   -------------------
   -- Set_Cell_Kind --
   -------------------
   
   procedure Set_Cell_Kind
     (This : access Cell_Record;
      K    : Cell_Kind) is
   begin
      This.Kind := K;
   end Set_Cell_Kind;
   
   ------------------
   -- Get_Cell_Box --
   ------------------
   
   function Get_Cell_Box 
     (This : access Cell_Record) return access Box_Record'Class is
   begin
      return This.Box;
   end Get_Cell_Box;
   
   ------------------
   -- Set_Cell_Box --
   ------------------
   
   procedure Set_Cell_Box
     (This : access Cell_Record;
      B    : access Box_Record'Class) is
   begin
      This.Box := B;
   end Set_Cell_Box;
   
end Reflex.Ladder.Cells;
