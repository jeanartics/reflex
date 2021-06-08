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
with Ada.Text_IO;

package body Artics.Containers.Sets is

   procedure Free_Set is new Ada.Unchecked_Deallocation
     (Set_Record'Class, Set_Id);
   procedure Free_String is new Ada.Unchecked_Deallocation
     (String, String_Ptr);

   -------------------------
   -- Internals functions --
   -------------------------

   function Alloc_Set return Set_Id;
   pragma Inline (Alloc_Set);
   -- Allocate memory for a new set.

   function Get_Cell_By_Item
     (Set  : Set_Id;
      Elmt : Item) return Cell_Id;
   -- Search for the cell containing Item.

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

   procedure Delete_Item (Elmt : Item) is
   begin
      null;
   end Delete_Item;

   ---------------
   -- Alloc_Set --
   ---------------

   function Alloc_Set return Set_Id is
   begin
      return new Set_Record;
   end Alloc_Set;

   ----------------------
   -- Get_Cell_By_Item --
   ----------------------

   function Get_Cell_By_Item
     (Set  : Set_Id;
      Elmt : Item) return Cell_Id is

      C : Cell_Id;
   begin

      C := Set.First;
      while not (C = No_Cell) loop
         if Get_Item (C) = Elmt then
            return C;
         end if;

         C := Get_Next (C);
      end loop;

      return No_Cell;
   end Get_Cell_By_Item;

   ---------------
   -- Clear_Set --
   ---------------

   procedure Clear_Set (Set  : Set_Id) is

      C : Cell_Id := Set.First;
      Next : Cell_Id;
   begin

      while C /= No_Cell loop
         Next := Get_Next (C);
         Delete_Cell (C);
         C := Next;
      end loop;

      -- All the cells are removed so the list is empty.
      Set.First := No_Cell;
   end Clear_Set;

   ----------------
   -- Delete_Set --
   ----------------

   procedure Delete_Set (Set : in out Set_Id) is
   begin
      Clear_Set (Set);
      Free_String (Set.Name);
      Free_Set (Set);
   end Delete_Set;

   ------------------
   -- Is_Empty_Set --
   ------------------

   function Is_Empty_Set (Set : Set_Id) return Boolean is
   begin
      return (Set.First = No_Cell);
   end Is_Empty_Set;

   ---------------
   -- Is_Member --
   ---------------

   function Is_Member
     (Set  : Set_Id;
      Elmt : Item) return Boolean is

      C   : Cell_Id;
   begin
      C := Set.First;
      while C /= No_Cell loop
         exit when Get_Item(C) = Elmt;
         C := Get_Next(C);
      end loop;
      return C /= No_Cell;
   end Is_Member;

   ----------------
   -- Get_Member --
   ----------------

   function Get_Member
     (Set  : Set_Id;
      Elmt : Item) return Item is

      C   : Cell_Id;
   begin
      C := Set.First;
      while C /= No_Cell loop
         exit when Get_Item(C) = Elmt;
         C := Get_Next(C);
      end loop;
      if C /= No_Cell then
         return Get_Item(C);
      else
         return No_Item;
      end if;
   end Get_Member;

   -------------
   -- New_Set --
   -------------

   function New_Set (Name : String := "") return Set_Id is

      S : Set_Id := Alloc_Set;
   begin
      S.Name  := new String'(Name);
      S.First := No_Cell;

      return S;
   end New_Set;

   -------------
   -- New_Set --
   -------------

   function New_Set
     (Elmt : Item;
      Name : String) return Set_Id is

      S : Set_Id := Alloc_Set;
      C : Cell_Id := New_Cell (Next => No_Cell, Prev => No_Cell, Elmt => Elmt);
   begin
      S.Name  := new String'(Name);
      S.First := C;

      return S;
   end New_Set;

   ------------------
   -- New_Set_Copy --
   ------------------

   function New_Set_Copy
     (Set  : Set_Id;
      Name : String := "") return Set_Id is

      S : Set_Id := Alloc_Set;
   begin
      S.Name  := new String'(Name);

      Copy_Set_To (Set, S);

      return S;
   end New_Set_Copy;

   ----------------
   -- First_Cell --
   ----------------

   function First_Cell (Set : Set_Id) return Cell_Id is
   begin
      if Set.First = No_Cell then
         return No_Cell;
      else
         return Set.First;
      end if;
   end First_Cell;

   --------------------
   -- Set_First_Cell --
   --------------------

   procedure Set_First_Cell
     (Set  : Set_Id;
      Cell : Cell_Id) is
   begin
      set.First := Cell;
   end Set_First_Cell;

   ----------------
   -- First_Item --
   ----------------

   function First_Item (Set : Set_Id) return Item is
      C : Cell_Id := First_Cell (Set);
   begin
      if C = No_Cell then
         return No_Item;
      else
         return Get_Item (C);
      end if;
   end First_Item;

   ------------
   -- Length --
   ------------

   function Length (Set : Set_Id) return Natural is
      Count : Natural := 0;
      C     : Cell_Id := Set.First;
   begin
      while C /= No_Cell loop
         Count := Count + 1;
         C := Get_Next (C);
      end loop;

      return Count;
   end Length;

   -----------------
   -- Copy_Set_To --
   -----------------

   procedure Copy_Set_To
     (Set  : Set_Id;
      To   : Set_Id) is

      C    : Cell_Id;
      Nc   : Cell_Id;
      Prev : Cell_Id := No_Cell;
   begin
      Clear_Set (To);

      if Is_Empty_Set (Set) then
         return;
      end if;

      C := Set.First;

      Prev := New_Cell
	(Next => No_Cell,
	 Prev => No_Cell,
	 Elmt => Get_Item (C));
      Set_First_Cell (To, Prev);

      loop
         C := Get_Next (C);
         exit when C = No_Cell;

         Nc := New_Cell
	   (Next => No_Cell,
	    Prev => Prev,
	    Elmt => Get_Item (C));
         Set_Next (Prev, Nc);

         Prev := Nc;
      end loop;
   end Copy_Set_To;

   ------------
   -- Append --
   ------------

   procedure Append
     (Elmt      : Item;
      To        : Set_Id;
      OverWrite : Boolean := False) is

      nC : Cell_Id  := New_Cell
	(Next => First_Cell (To),
	 Prev => No_Cell,
	 Elmt => Elmt);
      C  : Cell_Id;
      Overwrite_Exception: exception;
   begin
      if Is_Empty_Set (To) then
         Set_First_Cell (To, NC);
      else
         C := First_Cell(To);
         loop
            exit when C = No_Cell;
            if Get_Item(C) = Elmt then
               if OverWrite then
                  raise Overwrite_Exception;
               else
                  raise Duplicate_Item_Error;
               end if;
            end if;
            C := Get_Next (C);
         end loop;
         Set_Previous(First_Cell(To), NC);
         Set_First_Cell (To, NC);
      end if;
   exception
      when Overwrite_Exception =>
         Set_Item(C, Elmt);
      when others =>
         raise;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (nC        : Cell_Id;
      To        : Set_Id;
      OverWrite : Boolean := False) is

      C  : Cell_Id;
      Overwrite_Exception: exception;
   begin
      if Is_Empty_Set (To) then
         Set_First_Cell (To, NC);
      else
         C := First_Cell(To);
         loop
            exit when C = No_Cell;
            if Get_Item(C) = Get_Item(NC) then
               if OverWrite then
                  raise Overwrite_Exception;
               else
                  raise Duplicate_Item_Error;
               end if;
            end if;
            C := Get_Next (C);
         end loop;
         Set_Previous(First_Cell(To), NC);
         Set_First_Cell (To, NC);
      end if;
   exception
      when Overwrite_Exception =>
         Set_Item(C, Get_Item(nC));
      when others =>
         raise;
   end Append;

   ----------------
   -- Append_Set --
   ----------------

   procedure Append_Set
     (Set  : Set_Id;
      To   : Set_Id) is

      C : Cell_Id;
      D : Cell_Id;
   begin
      C := First_Cell(Set);
      loop
         exit when C = No_Cell;
         D := Get_Next(C);
         Append(C, To, True);
         C := D;
      end loop;
      Set.First := No_Cell;
   end Append_Set;


   ------------
   -- Remove --
   ------------

   function Remove
     (Set  : Set_Id;
      Elmt : Item) return Item is

      Cell : Cell_Id;
      Data : Item;
   begin
      if Is_Empty_Set(Set) then
         raise Set_Error;
      end if;

      Cell := Set.First;
      while Cell /= No_Cell loop
         Data := Get_Item(Cell);
         exit when Data = Elmt;
         Cell := Get_Next(Cell);
      end loop;

      if Cell = No_Cell then
         raise Set_Error;
      end if;

      declare
         Prev: Cell_Id := Get_Previous(Cell);
         Next: Cell_Id := Get_Next(Cell);
      begin
         if Prev=No_Cell and Next=No_Cell then
            Set.First := No_Cell;
         elsif Prev = No_Cell then
            Set.First := Next;
            Set_Previous(Next, No_Cell);
         elsif Next = No_Cell then
            Set_Next(Prev, No_Cell);
         else
            Set_Previous(Next, Prev);
            Set_Next(Prev, Next);
         end if;
      end;

      if Cell /= No_Cell then
         Delete_Cell (Cell);
      end if;

      return Data;
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Set  : Set_Id;
      Elmt : Item) is
      Data : Item;
   begin
      Data := Remove(Set, Elmt);
   end Remove;

   ------------------
   -- Get_Set_Name --
   ------------------

   function Get_Set_Name (Set : Set_Id) return String is
   begin
      return set.Name.all;
   end Get_Set_Name;


   ------------------
   -- Set_Set_Name --
   ------------------

   procedure Set_Set_Name
     (Set  : Set_Id;
      Name : String) is
   begin
      Free_String(Set.Name);
      Set.Name := new String'(Name);
   end Set_Set_Name;


   -- Sets Iterators --
   ---------------------

   ------------------
   -- New_Iterator --
   ------------------

   function New_Iterator (Set : Set_Id) return Set_Iterator is
      It : Set_Iterator; -- := (L, First_Cell (L));
   begin
      It.Set := Set;
      It.Current_Cell := First_Cell (Set);

      return It;
   end New_Iterator;

   ---------------------
   -- Delete_Iterator --
   ---------------------

   procedure Delete_Iterator (It : Set_Iterator) is
   begin
      null;
   end Delete_Iterator;

   -----------
   -- Reset --
   -----------

   procedure Reset (It : in out Set_Iterator) is
   begin
      It.Current_Cell := First_Cell (It.Set);
   end Reset;

   ------------------
   -- Current_Item --
   ------------------

   function Current_Item (It : Set_Iterator) return Item is
   begin
      return Get_Item (It.Current_Cell);
   end Current_Item;

   ------------
   -- Is_End --
   ------------

   function Is_End (It : Set_Iterator) return Boolean is
   begin
      return It.Current_Cell = No_Cell;
   end Is_End;

   -------------------------
   -- Remove_Current_Item --
   -------------------------

   procedure Remove_Current_Item (It : in out Set_Iterator) is
      Cell_To_Delete          : Cell_Id;
      Cell_To_Delete_Previous : Cell_Id;
      Cell_To_Delete_Next     : Cell_Id;
   begin
      if Is_End (It) then
         raise Set_Error;
      else
         Cell_To_Delete := It.Current_Cell;
         It.Current_Cell := Get_Next (It.Current_Cell);
         Cell_To_Delete_Previous := Get_Previous (Cell_To_Delete);
         Cell_To_Delete_Next := Get_Next (Cell_To_Delete);
         if Cell_To_Delete_Previous = No_Cell then
            Set_First_Cell (It.Set, Cell_To_Delete_Next);
         else
            Set_Next (Cell_To_Delete_Previous,Cell_To_Delete_Next);
         end if;
         if not (Cell_To_Delete_Next = No_Cell) then
            Set_Previous (Cell_To_Delete_Next,Cell_To_Delete_Previous);
         end if;
         Delete_Cell (Cell_To_Delete);
      end if;
   end Remove_Current_Item;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Set_Iterator) is
   begin
      if Is_End (It) then
         raise Set_Error;
      else
         It.Current_Cell := Get_Next (It.Current_Cell);
      end if;
   end Next;

   -------------
   -- Iterate --
   -------------

   Procedure Iterate (Set: Set_Id) is
      It  : Set_Iterator := New_Iterator(Set);
      Elmt: Item;
   begin
      Reset(It);
      while not Is_End (It) loop
         Elmt := Current_Item (It);
         Visitor (Elmt);
         Next (It);
      end loop;
   end Iterate;

end Artics.Containers.Sets;
