-- ------------------------------------------------------------------------ --
--  Filename        : containers-stacks.adb
--  Description     : A generic stack implementation
--  Author          :                                                       --
--  Created On      : Tue Jan 28 15:02:18 2003                              --
--  Last Modified By: .                                                     --
--  Status          : Unknown, Use with caution!                            --
--                                                                          --
-- Copyright : (c) Itris 1999-2000. <open-control@itris.fr>                 --
-- ------------------------------------------------------------------------ --
--                                                                          --
-- This program is free software; you can redistribute it and-or modify     --
-- it under the terms of the GNU General Public License as published by     --
-- the Free Software Foundation; either version 2 of the License, or        --
-- (at your option) any later version.                                      --
--                                                                          --
-- This program is distributed in the hope that it will be useful,          --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of           --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            --
-- GNU General Public License for more details.                             --
--                                                                          --
-- You should have received a copy of the GNU General Public License        --
-- along with this software in a file called 'COPYING'; if not, write to:   --
--      Free Software Foundation, Inc.,                                     --
--      59 Temple Place, Suite 330,                                         --
--      Boston, MA                                                          --
--      02111-1307  USA                                                     --
--                                                                          --
-- For information see : Itris Inc. (http://www.itris.fr)                   --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body Artics.Containers.Stacks is


   procedure Free_Stack is new
     Ada.Unchecked_Deallocation (Stack_Record'Class, Stack_Id);

   procedure Free_String is new
     Ada.Unchecked_Deallocation (String, String_Ptr);

   -------------------------
   -- Internals functions --
   -------------------------

   function Alloc_Stack return Stack_Id;
   pragma Inline (Alloc_Stack);
   -- Allocate a new stack.
   function Get_Elmt_Number
     ( Stack: in Stack_Id) return Natural;
   pragma Inline(Get_Elmt_Number);


   ----------------
   -- Alloc_Item --
   ----------------

   function Alloc_Item return Item is
   begin
      return No_Item;
   end Alloc_Item;


   -----------------
   -- Delete_Item --
   -----------------

   procedure Delete_Item
     (Elmt : Item) is
   begin
      null;
   end Delete_Item;


   -----------------
   -- Alloc_Stack --
   -----------------

   function Alloc_Stack return Stack_Id is
   begin
      return new Stack_Record;
   end Alloc_Stack;


   --------------------
   -- Is_Empty_Stack --
   --------------------

   function Is_Empty_Stack
     (Stack : Stack_Id) return Boolean is
   begin
      pragma Assert(Stack /= null);
      return (Stack.Base = No_Cell);
   end Is_Empty_Stack;


   -----------------
   -- Clear_Stack --
   -----------------

   procedure Clear_Stack
     (Stack : Stack_Id) is

   begin
      if not Is_Empty_Stack(Stack) then
         Pop_Items(Stack, Get_Elmt_Number(Stack));
      end if;

      -- All the cells are removed so the stack is empty.
      Stack.Base  := No_Cell;
      Stack.Top   := No_Cell;
   end Clear_Stack;

   ------------------
   -- Delete_Stack --
   ------------------

   procedure Delete_Stack
     (Stack: in out Stack_Id) is
   begin
      Clear_Stack (Stack);
      Free_String (Stack.Name);
      Free_Stack (Stack);
   end Delete_Stack;


   ---------------
   -- Base_Cell --
   ---------------

   function Base_Cell
     (Stack : Stack_Id) return Cell_Id is

   begin
      if Stack.Base = No_Cell then
         return No_Cell;
      else
         return Stack.Base;
      end if;
   end Base_Cell;

   ---------------
   -- Top_Cell --
   ---------------

   function Top_Cell
     (Stack : Stack_Id) return Cell_Id is

   begin
      pragma Assert(Stack /= null);
      if Stack.Top = No_Cell then
         return No_Cell;
      else
         return Stack.Top;
      end if;
   end Top_Cell;


   ----------------
   -- First_Item --
   ----------------

   function First_Item
     (Stack : Stack_Id) return Item is

      C : Cell_Id := Base_Cell (Stack);
   begin
      if C = No_Cell then
         return No_Item;
      else
         return Get_Item (C);
      end if;
   end First_Item;


   ---------------
   -- Last_Item --
   ---------------

   function Last_Item
     (Stack : Stack_Id) return Item is

      C : Cell_Id := Top_Cell (Stack);
   begin
      if C = No_Cell then
         return No_Item;
      else
         return Get_Item (C);
      end if;
   end Last_Item;


   -------------------
   -- Set_Base_Cell --
   -------------------

   procedure Set_Base_Cell
     (Stack: Stack_Id;
      Cell : Cell_Id) is
   begin
      Stack.Base := Cell;
   end Set_Base_Cell;


   ------------------
   -- Set_Top_Cell --
   ------------------

   procedure Set_Top_Cell
     (Stack: Stack_Id;
      Cell : Cell_Id) is
   begin
      Stack.Top := Cell;
   end Set_Top_Cell;


   ---------------------
   -- Get_Elmt_Number --
   ---------------------

   function Get_Elmt_Number
     ( Stack: in Stack_Id) return Natural is
   begin
      return Stack.Number;
   end Get_Elmt_Number;


   ---------------------
   -- Inc_Elmt_Number --
   ---------------------

   procedure Inc_Elmt_Number
     ( Stack: in Stack_Id) is
   begin
      Stack.Number := Stack.Number + 1;
   end Inc_Elmt_Number;


   ---------------------
   -- Dec_Elmt_Number --
   ---------------------

   procedure Dec_Elmt_Number
     ( Stack: in Stack_Id) is
   begin
      Stack.Number := Stack.Number - 1;
   end Dec_Elmt_Number;


   ---------------
   -- New_Stack --
   ---------------

   function New_Stack
     (Name : String := "") return Stack_Id is

      S : Stack_Id := Alloc_Stack;
   begin
      S.Name  := new String'(Name);
      S.Base  := No_Cell;
      S.Top   := No_Cell;

      return S;
   end New_Stack;


   ---------------
   -- New_Stack --
   ---------------

   function New_Stack
     (Elmt : Item;
      Name : string) return Stack_Id is

      S : Stack_Id := Alloc_Stack;
      C : Cell_Id  := New_Cell ( Next => No_Cell,
                                 Prev => No_Cell,
                                 Elmt => Elmt);
   begin
      S.Name  := new String'(Name);
      S.Base  := C;
      S.Top   := C;

      return S;
   end New_Stack;


   --------------
   -- Top_Item --
   --------------

   function Top_Item
     ( Stack: in Stack_Id) return Item is
   begin
      if Is_Empty_Stack(Stack) then
         return No_Item;
      else
         return Get_Item(Stack.Top);
      end if;
   end Top_Item;


   ---------------
   -- Push_Item --
   ---------------

   procedure Push_Item
     ( Elmt : in Item;
       Stack: in Stack_Id) is

      NC: Cell_Id := New_Cell( Next => No_Cell,
                               Prev => Top_Cell(Stack),
                               Elmt => Elmt);
   begin
      if Is_Empty_Stack(Stack) then
         Set_Base_Cell (Stack, NC);
      else
         Set_Next (Top_Cell (Stack), NC);
      end if;
      Inc_Elmt_Number(Stack);
      Set_Top_Cell (Stack, NC);
   end Push_Item;


   --------------
   -- Pop_Item --
   --------------

   procedure Pop_Item
     ( Stack: in Stack_Id) is

      OC: Cell_Id := Top_Cell(Stack);
      C : Cell_Id := Get_Previous(OC);
   begin
      if Is_Empty_Stack(Stack) then
         raise Stack_Error;
      else
         if C /= No_Cell then
            Set_Next(C, No_Cell);
         else
            Set_Base_Cell (Stack, No_Cell);
         end if;
         Set_Top_Cell(Stack, C);
         Delete_Cell(OC);
         Dec_Elmt_Number(Stack);
      end if;
   end Pop_Item;


   --------------
   -- Pop_Item --
   --------------

   function Pop_Item
     ( Stack: in Stack_Id) return Item is

      I: Item := Top_Item(Stack);
   begin
      Pop_Item(Stack);
      return I;
   end Pop_Item;


   ---------------
   -- Pop_Items --
   ---------------

   procedure Pop_Items
     ( Stack: in Stack_Id;
       NbElt: in Positive) is
   begin
      for I in 1..NbElt loop
         Pop_Item(Stack);
      end loop;
   exception
      when Stack_Error => raise;
      when others => raise;
   end Pop_Items;


   ------------
   -- Length --
   ------------

   function Length
     ( Stack: in Stack_Id) return Natural is
   begin
      return Get_Elmt_Number(Stack);
   end Length;

end Artics.Containers.Stacks;
