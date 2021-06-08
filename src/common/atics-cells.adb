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

with Ada.Unchecked_Deallocation;

package body Artics.Cells is

   procedure Free is new Ada.Unchecked_Deallocation (Cell_Record, Cell_Id);

   --------------
   -- New_Cell --
   --------------

   function New_Cell return Cell_Id is
   begin
      return 
	new Cell_Record'(Next => No_Cell, Prev => No_Cell, Elmt => No_Item);
   end New_Cell;

   --------------
   -- New_Cell --
   --------------

   function New_Cell
     (Next : in Cell_Id;
      Prev : in Cell_Id;
      Elmt : in Item) return Cell_Id is
   begin
      return new Cell_Record'(Next => Next, Prev => Prev, Elmt => Elmt);
   end New_Cell;

   -----------------
   -- Delete_Cell --
   -----------------

   procedure Delete_Cell (C : in out Cell_Id) is
   begin
      Free (C);
   end Delete_Cell;

   --------------
   -- Get_Item --
   --------------

   function Get_Item (C : Cell_Id) return Item is
   begin
      return C.Elmt;
   end Get_Item;

   --------------
   -- Set_Item --
   --------------

   procedure Set_Item
     (C    : Cell_Id;
      Elmt : Item) is
   begin
      C.Elmt := Elmt;
   end Set_Item;

   --------------
   -- Get_Next --
   --------------

   function Get_Next (C : Cell_Id) return Cell_Id is
   begin
      return C.Next;
   end Get_Next;

   ------------------
   -- Get_Previous --
   ------------------

   function Get_Previous (C : Cell_Id) return Cell_Id is
   begin
      return C.Prev;
   end Get_Previous;

   --------------
   -- Set_Next --
   --------------

   procedure Set_Next
     (C    : Cell_Id;
      Next : Cell_Id) is
   begin
      C.Next := Next;
   end Set_Next;

   ------------------
   -- Set_Previous --
   ------------------

   procedure Set_Previous
     (C    : Cell_Id;
      Prev : Cell_Id) is
   begin
      C.Prev := Prev;
   end Set_Previous;

end Artics.Cells;
