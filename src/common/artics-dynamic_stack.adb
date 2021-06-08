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

package body Artics.Dynamic_Stack is

   procedure Free is new Ada.Unchecked_Deallocation
     (Stack_Record, Stack);

   ---------------
   -- New_Stack --
   ---------------

   function New_Stack return Stack is

      S : Stack := new Stack_Record;
   begin
      S.Table   := New_Vector (Initial_Size);
      S.Initial := Initial_Size;
      return S;
   end New_Stack;

   -----------
   -- Empty --
   -----------

   function Empty (S : in Stack) return Boolean is
   begin
      return Count (S.Table) = 0;
   end Empty;

   ----------
   -- Push --
   ----------

   procedure Push
     (S    : in Stack;
      Elmt : in Element_Type) is

   begin
      Append (S.Table, Elmt);
   end Push;

   ---------
   -- Top --
   ---------

   function Top (S : in Stack) return Element_Type is
   begin
      if Empty (S) then
         raise Stack_Empty;
      end if;

      return Get_Item_At (S.Table, Last (S.Table));
   end Top;

   ---------
   -- Pop --
   ---------

   function Pop(S : in Stack) return Element_Type is

      E : Element_Type;
   begin
      E := Top (S);
      Decrement_Last (S.Table);

      return E;
   end Pop;

   ----------
   -- Topn --
   ----------

   function Topn
     (S : in Stack;
      N : in Natural) return Element_Type is
   begin
      if N > Count (S) then
         raise Stack_Error;
      end if;

      return Get_Item_At (S.Table, Last (S.Table) - Integer (N));
   end Topn;

   ----------
   -- Popn --
   ----------

   function Popn
     (S : in Stack;
      N : in Natural) return Element_Type is

      E : Element_Type;
   begin
      E := Topn (S, N);
      Set_Last (S.Table, Last (S.Table) - Integer (N));

      return E;
   end Popn;

   -----------
   -- Count --
   -----------

   function Count (S : in Stack) return Natural is
   begin
      return Count (S.Table);
   end Count;

   ------------
   -- Freeze --
   ------------

   procedure Freeze (S : in Stack) is
   begin
      null;
   end Freeze;

   -----------
   -- Reset --
   -----------

   procedure Reset (S : in Stack) is

      E : Element_Type;
   begin
      while not Empty (S) loop
         E := Pop (S);
      end loop;
   end Reset;

   ------------------
   -- Delete_Stack --
   ------------------

   procedure Delete_Stack (S : in Stack) is
   begin
      null;
   end Delete_Stack;

   -----------
   -- First --
   -----------

   function First (S : in Stack) return Natural is
      F : Integer := First (S.Table);
   begin
      if F < 0 then
         return 0;
      else
         return Natural (F);
      end if;
   end First;

   ----------
   -- Last --
   ----------

   function Last (S : in Stack) return Natural is
      L : Integer := Last (S.Table);
   begin
      if L < 0 then
         return 0;
      else
         return Natural (L);
      end if;
   end Last;

   -----------------
   -- Get_Item_At --
   -----------------

   function Get_Item_At
     (S     : in Stack;
      Index : in Natural) return Element_Type is

   begin
      return Get_Item_At (S.Table, Integer (Index));
   end Get_Item_At;

   -----------------
   -- Set_Item_At --
   -----------------

   procedure Set_Item_At
     (S     : in out Stack;
      Index : in Natural;
      Value : in Element_Type) is
   begin
      Set_Item_At (S.Table, Integer (Index), Value);
   end Set_Item_At;

   ----------------
   -- Set_Top_At --
   ----------------

   procedure Set_Top_At
     (S : in out Stack;
      N : in Natural) is
   begin
      Set_Last (S.Table, Last (S.Table) - Integer (N));
   end Set_Top_At;

   -----------------
   -- Is_In_Stack --
   -----------------

   function Is_In_Stack
     (S    : in Stack;
      Elmt : in Element_Type) return Boolean is

      It : Element_Type;
      F  : Natural := First(S);
      L  : Natural := Last (S);
   begin
       for I in F..L loop
          It := Get_Item_At (S, I);
          if It = Elmt then
             return True;
          end if;
       end loop;

      return False;
   end Is_In_Stack;

   ------------------------
   -- Get_Stack_Position --
   ------------------------

   function Get_Stack_Position
     (S    : in Stack;
      Elmt : in Element_Type) return Natural is

      It : Element_Type;
      F  : Natural := First (S);
      L  : Natural := Last  (S);
   begin
      for I in F..L loop
          It := Get_Item_At (S, I);
          if It = Elmt then
             return I;
          end if;
      end loop;

      return 0;
   end Get_Stack_Position;

end Artics.Dynamic_Stack;

