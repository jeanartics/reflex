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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Artics.Containers.Lists is

   procedure Free_List is new Ada.Unchecked_Deallocation
     (List_Record'Class, List_Id);

   procedure Free_String is new Ada.Unchecked_Deallocation
     (String, String_Ptr);

   procedure Free_Iterator is new Ada.Unchecked_Deallocation
     (List_Iterator, List_Iterator_Ptr);

   procedure Remove
     (L    : in out List_Id;
      Cell : in out Cell_Id);

   -------------------------
   -- Internals functions --
   -------------------------

   function Alloc_List return List_Id;
   pragma Inline (Alloc_List);
   -- Alloue une nouvelle liste.

   function Get_Cell_By_Item
     (List : List_Id;
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

   procedure Delete_Item
     (Elmt : in Item) is
   begin
      null;
   end Delete_Item;

   ----------------
   -- Alloc_List --
   ----------------

   function Alloc_List return List_Id is
   begin
      return new List_Record;
   end Alloc_List;

   ----------------------
   -- Get_Cell_By_Item --
   ----------------------

   function Get_Cell_By_Item
     (List : in List_Id;
      Elmt : in Item) return Cell_Id is

      C : Cell_Id;
   begin

      C := List.First;
      while C /= No_Cell loop
         if Get_Item (C) = Elmt then
            return C;
         end if;

         C := Get_Next (C);
      end loop;

      return No_Cell;
   end Get_Cell_By_Item;

   ------------------------
   -- Generic_Clear_List --
   ------------------------

   procedure Generic_Clear_List
     (List : in List_Id) is

      C : Cell_Id;
      Next : Cell_Id;
      I : Item;
   begin
      if List = No_List then
         return;
      end if;

      C := List.First;
      while C /= No_Cell loop
         Next := Get_Next (C);
         I := Get_Item(C);
         Free_Cell(I);
         Delete_Cell (C);
         C := Next;
      end loop;

      -- All the cells are removed so the list is empty.
      List.First := No_Cell;
      List.Last  := No_Cell;
      List.Count := 0;
   end Generic_Clear_List;

   ----------------
   -- Clear_List --
   ----------------

   procedure Clear_List
     (List : in List_Id) is

      C : Cell_Id;
      Next : Cell_Id;
   begin
      if List = No_List then
         return;
      end if;

      C := List.First;
      while C /= No_Cell loop
         Next := Get_Next (C);
         Delete_Cell (C);
         C := Next;
      end loop;

      -- All the cells are removed so the list is empty.
      List.First := No_Cell;
      List.Last  := No_Cell;
      List.Count := 0;
   end Clear_List;

   ----------------------
   -- Bubble_Sort_List --
   ----------------------

   procedure Bubble_Sort_List (List : in out List_Id) is

      C        : Cell_Id;
      Nxt      : Cell_Id;
      Prv_Cell : Cell_Id;
      Nxt_Cell : Cell_Id;
      Switched : Boolean;
   begin
      if List = No_List then
         return;
      end if;

      loop
         Switched := False;

         C := List.First;
         while C /= No_Cell loop
            --Put_Line (" C   /= No_Cell ");
            Nxt := Get_Next (C);
            exit when Nxt = No_Cell;

            --Put_Line (" C   => "); -- & Debug.Print (Get_Item (C)));
            --Put_line (" Nxt => "); -- & Debug.Print (Get_Item (Nxt)));

            if Less_Than (Get_Item (C), Get_Item (Nxt)) then
               --Put_Line (" C less than Nxt");

               Prv_Cell := Get_Previous (C);
               Set_Previous (C, Nxt);
               Set_Previous (Nxt, Prv_Cell);

               Nxt_Cell := Get_Next (Nxt);
               Set_Next (Nxt, C);
               Set_Next (C, Nxt_Cell);

               if Prv_Cell /= No_Cell then
                  Set_Next (Prv_Cell, Nxt);
               end if;
               if Nxt_Cell /= No_Cell then
                  Set_Previous (Nxt_Cell, C);
               end if;

               if List.First = C then
                  List.First := Nxt;
               end if;
               if List.Last = Nxt then
                  List.Last := C;
               end if;

               Switched := True;
            else
               --Put_Line (" C no less than Nxt");
               C := Nxt;
            end if;
         end loop;

         exit when not Switched;
      end loop;
   end Bubble_Sort_List;


   -----------------
   -- Delete_List --
   -----------------

   procedure Delete_List (List : in out List_Id) is
   begin
      if List /= No_List then
         Clear_List (List);
         Free_String (List.Name);
         Free_List (List);
      end if;
   end Delete_List;

   -------------------
   -- Is_Empty_List --
   -------------------

   function Is_Empty_List (List : in List_Id) return Boolean is
   begin
      if List = No_List then return True; end if;
      return (List.First = No_Cell);
   end Is_Empty_List;

   --------------
   -- New_List --
   --------------

   function New_List (Name : in String := "") return List_Id is
      L : List_Id := Alloc_List;
   begin
      L.Name  := new String'(Name);
      L.First := No_Cell;
      L.Last  := No_Cell;
      L.Count := 0;
      return L;
   end New_List;

   --------------
   -- New_List --
   --------------

   function New_List
     (Elmt : in Item;
      Name : in String := "") return List_Id is

      L : List_Id := Alloc_List;
      C : Cell_Id := New_Cell (Next => No_Cell, Prev => No_Cell, Elmt => Elmt);
   begin
      L.Name  := new String'(Name);
      L.First := C;
      L.Last  := C;
      L.Count:= 1;
      return L;
   end New_List;

   -------------------
   -- New_List_Copy --
   -------------------

   function New_List_Copy
     (List : in List_Id;
      Name : in String := "") return List_Id
   is
      L : List_Id := Alloc_List;
   begin
      L.Name  := new String'(Name);
      Copy_List_To (List, L);

      return L;
   end New_List_Copy;

   ----------------
   -- First_Cell --
   ----------------

   function First_Cell (List : in List_Id) return Cell_Id is
   begin
      if List.First = No_Cell then
         return No_Cell;
      else
         return List.First;
      end if;
   end First_Cell;

   ---------------
   -- Last_Cell --
   ---------------

   function Last_Cell (List : in List_Id) return Cell_Id is
   begin
      if List = No_List or else List.Last = No_Cell then
         return No_Cell;
      else
         return List.Last;
      end if;
   end Last_Cell;

   --------------------
   -- Set_First_Cell --
   --------------------

   procedure Set_First_Cell
     (List : in List_Id;
      Cell : in Cell_Id) is
   begin
      List.First := Cell;
   end Set_First_Cell;

   -------------------
   -- Set_Last_Cell --
   -------------------

   procedure Set_Last_Cell
     (List : in List_Id;
      Cell : in Cell_Id) is
   begin
      List.Last := Cell;
   end Set_Last_Cell;

   ----------------
   -- First_Item --
   ----------------

   function First_Item (List : in List_Id) return Item is
      C : Cell_Id := First_Cell (List);
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

   function Last_Item (List : in List_Id) return Item is
      C : Cell_Id := Last_Cell (List);
   begin
      if C = No_Cell then
         return No_Item;
      else
         return Get_Item (C);
      end if;
   end Last_Item;

   ---------------
   -- Get_Item --
   ---------------

   function Get_Item
     (List      : in List_Id;
      Position  : in Positive) return Item is
      It       : List_Iterator := New_Iterator (List);
      Ret_Item : Item;
   begin
      Reset (It);
      while Current_Index (It) /= Position loop
         Next (It);
      end loop;

      Ret_Item := Current_Item (It);
      Delete_Iterator (It);
      return Ret_Item;
   end Get_Item;

   -------------
   -- In_List --
   -------------

   function In_List
     (Elmt : in Item;
      L    : in List_Id) return Boolean is
      LIt : List_Iterator := New_Iterator (L);
   begin
      while not Is_End (LIt) loop
         if Current_Item (LIt)= Elmt then
            return True;
         end if;
         Next (LIt);
      end loop;
      return False;
   end In_List;

   --------------------
   -- In_List_Before --
   --------------------

   function In_List_Before
     (Crt_Elmt : in Item;
      Elmt     : in Item;
      L        : in List_Id) return Boolean is
      LIt        : List_Iterator := New_Iterator (L);
      Crt_It     : Item := Current_Item (LIt);
      Dest_Found : Boolean := False;
   begin
      while not Is_End (LIt) loop
         if Crt_It = Elmt then
            Dest_Found := True;
         end if;
         if Crt_It = Crt_Elmt and not Dest_Found then
            return True;
         end if;
         Next (LIt);
         Crt_It := Current_Item (LIt);
      end loop;
      return False;
   end In_List_Before;

   ------------------------
   -- In_List_Occurences --
   ------------------------

   function In_List_Occurrences
     (Elmt : in Item;
      L    : in List_Id) return Integer is
      LIt : List_Iterator := New_Iterator (L);
      Cpt : Integer := 0;
   begin
      while not Is_End (LIt) loop
         if Current_Item (LIt)= Elmt then
            Cpt := Cpt + 1;
         end if;
         Next (LIt);
      end loop;
      return Cpt;
   end In_List_Occurrences;

   ------------
   -- Length --
   ------------

   function Length (List : in List_Id) return Natural is
      Count : Natural := 0;
      C : Cell_Id;
   begin
      if List = No_List then
         return 0;
      end if;
      C := List.First;
      while C /= No_Cell loop
         Count := Count + 1;
         C := Get_Next (C);
      end loop;

      return Count;
   end Length;

   -------------
   -- Length2 --
   -------------

   function Length2 (List : in List_Id) return Natural is
      Len : Natural := 0;
   begin
      if List /= No_List then
         Len := List.Count;
      end if;
      return Len;
   end Length2;

   ------------------
   -- Copy_List_To --
   ------------------

   procedure Copy_List_To
     (List : in List_Id;
      To   : in List_Id) is
      C    : Cell_Id;
      Nc   : Cell_Id;
      Prev : Cell_Id := No_Cell;
   begin
      Clear_List (To);

      if Is_Empty_List (List) then
         return;
      end if;

      C := List.First;

      Prev := New_Cell (Next => No_Cell,
                        Prev => No_Cell,
                        Elmt => Get_Item (C));
      Set_First_Cell (To, Prev);

      loop
         C := Get_Next (C);
         exit when C = No_Cell;

         Nc := New_Cell (Next => No_Cell,
                         Prev => Prev,
                         Elmt => Get_Item (C));
         Set_Next (Prev, Nc);
         To.Count := To.Count + 1;
         Prev := Nc;
      end loop;

      Set_Last_Cell (To, Prev);
   end Copy_List_To;

   ------------
   -- Append --
   ------------

   procedure Append
     (Elmt : in Item;
      To   : in List_Id) is
      C : Cell_Id := New_Cell
	(Next => No_Cell,
	 Prev => Last_Cell (To),
	 Elmt => Elmt);
   begin
      if Is_Empty_List (To) then
         Set_First_Cell (To, C);
      else
         Set_Next (Last_Cell (To), C);
      end if;
      Set_Last_Cell (To, C);
      To.Count := To.Count + 1;
   end Append;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Elmt : in Item;
      To   : in List_Id) is
      C : Cell_Id := New_Cell
	(Next => First_Cell(To),
	 Prev => No_Cell,
	 Elmt => Elmt);
   begin
      if Is_Empty_List (To) then
         Set_Last_Cell (To, C);
      else
         Set_Previous (First_Cell (To), C);
      end if;

      Set_First_Cell (To, C);
      To.Count := To.Count + 1;
   end Prepend;

   ----------
   -- Push --
   ----------

   procedure Push
     (Elmt : in Item;
      To   : in List_Id) is
      C : Cell_Id := New_Cell
	(Next => First_Cell (To),
	 Prev => No_Cell,
	 Elmt => Elmt);
   begin
      if Is_Empty_List (To) then
         Set_Last_Cell (To, C);
      else
         Set_Previous (First_Cell (To), C);
      end if;

      Set_First_Cell (To, C);
      To.Count := To.Count + 1;
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop
     (List       : in out List_Id;
      First_Item : out Item) is
      C : Cell_Id := First_Cell (List);
   begin
      if C = No_Cell then
         First_Item := No_Item;
      else
         Remove_First (List);
         First_Item := Get_Item (C);
      end if;
      if List.Count > 1 then
         List.Count := List.Count - 1;
      else
         Ada.Text_IO.Put_Line("Pop() List.Count is already 0 before popping");
      end if;
   end Pop;

   ---------
   -- Top --
   ---------

   function Top (List : in List_Id) return Item is
      C : Cell_Id := First_Cell (List);
      First_Item : Item;
   begin
      if C = No_Cell then
         First_Item := No_Item;
      else
         First_Item := Get_Item (C);
      end if;

      return First_Item;
   end Top;

   -----------------
   -- Append_List --
   -----------------

   procedure Append_List
     (List : in List_Id;
      To   : in out List_Id) is
   begin
      if Is_Empty_List (List) then
         return;
      end if;

      if To = No_List then
         To := New_List;
      end if;

      declare
         L  : constant Cell_Id := Last_Cell (To);
         F  : constant Cell_Id := First_Cell (List);
      begin
         if L = No_Cell then
            -- To is an empty list...
            Set_First_Cell(To, F);
         else
            Set_Next (L, F);
            Set_Previous (F, L);

         end if;
         Set_Last_Cell (To, Last_Cell (List));

         -- Now the list "List" is empty.
         Set_First_Cell (List, No_Cell);
         Set_Last_Cell (List, No_Cell);
      end;
      To.Count := To.Count + List.Count;
   end Append_List;

   ------------------
   -- Prepend_List --
   ------------------

   procedure Prepend_List
     (List : in List_Id;
      To   : in out List_Id) is
   begin
      if Is_Empty_List (List) then
         return;
      end if;

      if To = No_List then
         To := New_List;
      end if;

      declare
         L  : constant Cell_Id := Last_Cell (List);
         F  : constant Cell_Id := First_Cell (To);
      begin
         if F = No_Cell then
            -- To is an empty list...
            Set_Last_Cell(To, L);
         else
            Set_Next (L, F);
            Set_Previous (F, L);

         end if;
         Set_First_Cell (To, First_Cell (List));

         -- Now the list "List" is empty.
         Set_First_Cell (List, No_Cell);
         Set_Last_Cell (List, No_Cell);
      end;
      To.Count := To.Count + List.Count;
   end Prepend_List;

   -------------------
   -- Insert_Sorted --
   -------------------

   procedure Insert_Sorted
     (List : in List_Id;
      Elmt : in Item) is
      C : Cell_Id;
      Nc       : Cell_Id := New_Cell
	(Next => No_Cell,
	 Prev => No_Cell,
	 Elmt => Elmt);
      Inserted : Boolean := False;
   begin
      C := List.First;
      if C = No_Cell then
         Set_Previous(Nc, No_Cell);
         Set_Next(Nc, No_Cell);
         Set_First_Cell(List, Nc);
         Set_Last_Cell(List, Nc);
      else
         while C /= No_Cell and then not Inserted loop
            -- Two cases : 1. Elmt < C => insert it
            --             2. Go to next
            if Less_Than(Elmt, Get_Item (C)) then
               -- Two cases : 1. C is the list first cell
               --             2.
               if C = First_Cell(List) then
                  Set_First_Cell(List, Nc);
                  Set_Next(Nc, C);
                  Set_Previous(Nc, No_Cell);
                  Set_Previous(C, Nc);
                  Inserted := True;
               else
                  declare
                     P : Cell_Id := Get_Previous(C);
                  begin
                     Set_Next(P, Nc);
                     Set_Next(Nc, C);
                     Set_Previous(C, Nc);
                     Set_Previous(Nc, P);
                     Inserted := True;
                  end;
               end if;
            end if;
            C := Get_Next (C);
         end loop;

         if not Inserted then
            -- It will be the last
            C := Last_Cell(List);
            Set_Next(C, Nc);
            Set_Next(Nc, No_Cell);
            Set_Previous(Nc, C);
            Set_Last_Cell(List, Nc);
         end if;
      end if;
      List.Count := List.Count + 1;
   end Insert_Sorted;


   --------------------------------
   -- Insert_Sorted_No_Duplicate --
   --------------------------------

   procedure Insert_Sorted_No_Duplicate
     (List      : in List_Id;
      Elmt      : in Item) is
      
      C        : Cell_Id;
      -- Current_Cell
      
      Nc       : Cell_Id := New_Cell
	(Next => No_Cell,
	 Prev => No_Cell,
	 Elmt => Elmt);
      -- New Cell to be inserted
      
      Pc       : Cell_Id := No_Cell;
      -- Previous Cell
      
      Inserted : Boolean := False;
   begin

      C := List.First;

      if C = No_Cell then
         Set_Previous(Nc, No_Cell);
         Set_Next(Nc, No_Cell);
         Set_First_Cell(List, Nc);
         Set_Last_Cell(List, Nc);
         List.Count := List.Count + 1;
         Inserted := True;
      else
         while C /= No_Cell and then not Inserted loop

            if Less_Than(Get_Item(C), Elmt) then
               -- to be inserted cell is less than current item
               -- Do nothing
               null;

            elsif Equal(Get_Item(C), Elmt) then
               -- to be inserted already exist in the list
               -- delete the newly created Nc and set Inserted to true
               Delete_Cell(Nc);
               Inserted := True;

            else
               -- Two cases : 1. C is the list first cell
               --             2.
               if C = First_Cell(List) then
                  Set_First_Cell(List, Nc);
                  Set_Next(Nc, C);
                  Set_Previous(Nc, No_Cell);
                  Set_Previous(C, Nc);
                  List.Count := List.Count + 1;
                  Inserted := True;
               else
                  declare
                     P : Cell_Id := Get_Previous(C);
                  begin
                     Set_Next(P, Nc);
                     Set_Next(Nc, C);
                     Set_Previous(C, Nc);
                     Set_Previous(Nc, P);
                     List.Count := List.Count + 1;
                     Inserted := True;
                  end;
               end if;
            end if;

            Pc := C;
            C := Get_Next (C);

         end loop;

         if not Inserted then
            -- it means that the new element is greater than all other elmt
            -- we are also sure that there was at least one element
            Set_Next(Pc, Nc);
            Set_Previous(Nc, Pc);
            Set_Next(Nc, No_Cell);
            Set_Last_Cell(List, Nc);
            List.Count := List.Count + 1;
         end if;
      end if;

   end Insert_Sorted_No_Duplicate;

   ------------------
   -- Insert_After --
   ------------------

   procedure Insert_After
     (List  : in List_Id;
      After : in Item;
      Elmt  : in Item) is
      C : Cell_Id := Get_Cell_By_Item (List, After);
   begin
      if C = No_Cell then
         raise List_Error;

      else
         declare
            L : constant Cell_Id := Last_Cell (List);
            Nc : Cell_Id := New_Cell (Next => Get_Next (C),
                                      Prev => C,
                                      Elmt => Elmt);
         begin
            Set_Next (C, Nc);

            if L = C then
               Set_Last_Cell (List, Nc);
            end if;
         end;
      end if;
      List.Count := List.Count + 1;
   end Insert_After;

   -------------------
   -- Insert_Before --
   -------------------

   procedure Insert_Before
     (List   : in List_Id;
      Before : in Item;
      Elmt   : in Item) is
      C : Cell_Id := Get_Cell_By_Item (List, Before);
   begin
      if C = No_Cell then
         raise List_Error;

      else
         declare
            F : constant Cell_Id := First_Cell (List);
            Prev_C : Cell_Id := Get_Previous (C);
            Nc : Cell_Id := New_Cell (Next => C,
                                      Prev => Prev_C,
                                      Elmt => Elmt);
         begin
            Set_Previous (C, Nc);

            if F = C then
               Set_First_Cell (List, Nc);
            else
               Set_Next (Prev_C, Nc);
            end if;
         end;
      end if;
      List.Count := List.Count + 1;
   end Insert_Before;

   -----------------------
   -- Insert_List_After --
   -----------------------

   procedure Insert_List_After
     (L1    : in List_Id;
      L2    : in List_Id;
      After : in Item) is
   begin
      if Is_Empty_List (L1) then
         raise List_Error;
      end if;

      if Is_Empty_List (L2) then
         return;
      end if;

      declare
         Last1  : constant Cell_Id := Last_Cell (L1);
         First2 : constant Cell_Id := First_Cell (L2);
         Last2  : constant Cell_Id := Last_Cell (L2);

         C : Cell_Id := Get_Cell_By_Item (L1, After);
      begin
         if C = No_Cell then
            raise List_Error;
         end if;

         if C = Last1 then
            Set_Last_Cell (L1, Last2);
         else
            Set_Previous (Get_Next (C), Last2);
            Set_Next (Last2, Get_Next (C));
         end if;

         Set_Next (C, First2);
         Set_Previous (First2, C);

         -- The list L2 is now empty.
         Set_First_Cell (L2, No_Cell);
         Set_Last_Cell (L2, No_Cell);
      end;
      L1.Count := L1.Count + L2.Count;
   end Insert_List_After;

   ------------------------
   -- Insert_List_Before --
   ------------------------

   procedure Insert_List_Before
     (L1     : in List_Id;
      L2     : in List_Id;
      Before : in Item) is
   begin
      if Is_Empty_List (L1) then
         raise List_Error;
      end if;

      if Is_Empty_List (L2) then
         return;
      end if;

      declare
         First1 : constant Cell_Id := First_Cell (L1);
         First2 : constant Cell_Id := First_Cell (L2);
         Last2  : constant Cell_Id := Last_Cell (L2);

         C : Cell_Id := Get_Cell_By_Item (L1, Before);
      begin
         if C = No_Cell then
            raise List_Error;
         end if;

         if C = First1 then
            Set_First_Cell (L1, First2);
            Set_Previous (First2, No_Cell);

         else
            Set_Next (Get_Previous (C), First2);
            Set_Previous (First2, Get_Previous (C));
         end if;

         Set_Next (Last2, C);
         Set_Previous (C, Last2);

         -- The list L2 is now empty.
         Set_First_Cell (L2, No_Cell);
         Set_Last_Cell (L2, No_Cell);
      end;
      L1.Count := L1.Count + L2.Count;
   end Insert_List_Before;

   ------------------
   -- Remove_First --
   ------------------

   procedure Remove_First (L : in out List_Id) is
      Cell : Cell_Id := L.First;
   begin
      if L.First /= No_Cell then

         if L.First = L.Last then
            L.First := No_Cell;
            L.Last := No_Cell;

         else
            L.First := Get_Next (L.First);
            if L.First /= No_Cell then
               Set_Previous (L.First, No_Cell);
            end if;
         end if;

         Delete_Cell (Cell);
         L.Count := L.Count - 1;
      end if;
   end Remove_First;

   -----------------
   -- Remove_Last --
   -----------------

   procedure Remove_Last (L : in out List_Id) is
      Cell : Cell_Id := L.Last;
   begin
      if L.Last /= No_Cell then

         if L.First = L.Last then
            L.First := No_Cell;
            L.Last := No_Cell;

         else
            L.Last := Get_Previous (Cell);
            if L.Last /= No_Cell then
               Set_Next (L.Last, No_Cell);
            end if;
         end if;

         Delete_Cell (Cell);
      end if;
      L.Count := L.Count - 1;
   end Remove_Last;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (L    : in out List_Id;
      Cell : in out Cell_Id) is
   begin
      declare
         Prev: Cell_Id := Get_Previous(Cell);
         Next: Cell_Id := Get_Next(Cell);
      begin
         if Prev=No_Cell and Next=No_Cell then
            L.First := No_Cell;
         elsif Prev = No_Cell then
            L.First := Next;
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
      L.Count := L.Count - 1;
   end Remove;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (L    : in out List_Id;
      Pos  : in Positive;
      Elt  : in Item) is
      Cell: Cell_Id;
      Count : Positive := 1;
   begin
      Cell := L.First;
      while Cell /= No_Cell and then Count < Pos loop
         Cell := Get_Next(Cell);
         Count := Count + 1;
      end loop;
      if Cell /= No_Cell then
         Set_Item(Cell, Elt);
      else
         raise Program_Error;
      end if;
   end Replace;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (L    : in out List_Id;
      From : in Positive) is
      Cell: Cell_Id;
   begin
      Cell := L.First;
      for I in 2 .. From loop
         Cell := Get_Next(Cell);
      end loop;
      Remove(L, Cell);
      L.Count := L.Count - 1;
   exception
      when CONSTRAINT_ERROR =>
         -- Count is not good enough
         Ada.Text_IO.Put_Line
	   ("Remove() List.Count is already 0 before Removing");
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (L     : in out List_Id;
      From  : in Positive;
      Count : in Positive) is
      Next: Cell_Id;
      Cell: Cell_Id;
   begin
      Cell := L.First;
      for I in 2 .. From loop
         Cell := Get_Next(Cell);
      end loop;
      for I in 1 .. Count loop
         Next := Get_Next(Cell);
         Remove(L, Cell);
         Cell := Next;
      end loop;
   end Remove;

   -------------------
   -- Get_List_Name --
   -------------------

   function Get_List_Name (List : in List_Id) return String is
   begin
      return List.Name.all;
   end Get_List_Name;

   -------------------
   -- Set_List_Name --
   -------------------

   procedure Set_List_Name
     (List : in List_Id;
      Name : in String) is
   begin
      Free_String (List.Name);
      List.Name := new String'(Name);
   end Set_List_Name;

   ---------------------
   -- Lists Iterators --
   ---------------------

   ------------------
   -- New_Iterator --
   ------------------

   function New_Iterator
     (L     : in List_Id;
      Start : in Positive := Positive'First) return List_Iterator_Ptr
   is
      It : List_Iterator_Ptr := new List_Iterator;
   begin
      It.all := New_Iterator (L, Start);
      return It;
   end New_Iterator;

   ------------------
   -- New_Iterator --
   ------------------

   function New_Iterator
     (L     : in List_Id;
      Start : in Positive := Positive'First) return List_Iterator
   is
      It : List_Iterator; -- :=
      -- (L, No_Cell, Positive'First, No_Cell, Positive'First, True);
   begin
      It.List := L;

      -- Case of an empty list.
      if Is_Empty_List (It.List) then

         It.Start_Cell    := No_Cell;
         It.Start_Index   := Positive'First;
         It.Current_Index := Positive'First;
         It.Current_Cell  := It.Start_Cell;
         It.Is_End        := True;

         return It;
      end if;

      -- Case of the iterator is at the beginning of the list.

      if Start = Positive'First then

         It.Start_Cell    := First_Cell (L);
         It.Start_Index   := Positive'First;
         It.Current_Cell  := It.Start_Cell;
         It.Current_Index := It.Start_Index;
         It.Is_End        := False;

         return It;
      end if;

      -- Case of the iterator is not at the beginning of the list.

      declare
         C : Cell_Id;
         I : Positive := Positive'First;
      begin
         C := First_Cell (L);

         while I < Start loop
            pragma Assert (C /= No_Cell);
            C := Get_Next (C);
            I := I + 1;
         end loop;

         pragma Assert (C /= No_Cell);

         It.Start_Cell    := C;
         It.Start_Index   := I;
         It.Current_Cell  := It.Start_Cell;
         It.Current_Index := It.Start_Index;
         It.Is_End        := False;
      end;

      return It;
   end New_Iterator;

   ---------------------
   -- Delete_Iterator --
   ---------------------

   procedure Delete_Iterator (It : in out List_Iterator) is
   begin
      null;
   end Delete_Iterator;

   ---------------------
   -- Delete_Iterator --
   ---------------------

   procedure Delete_Iterator (It : in out List_Iterator_Ptr) is
   begin
      Delete_Iterator (It.all);
      Free_Iterator (It);
   end Delete_Iterator;

   -----------
   -- Reset --
   -----------

   procedure Reset (It : in out List_Iterator) is
   begin
      It.Current_Cell  := It.Start_Cell;
      It.Current_Index := It.Start_Index;

      if Is_Empty_List (It.List) then
         It.Is_End := True;
      else
         It.Is_End := False;
      end if;
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (It : in out List_Iterator_Ptr) is
   begin
      Reset (It.all);
   end Reset;

   ------------------
   -- Current_Item --
   ------------------

   function Current_Item (It : List_Iterator) return Item is
   begin
      return Get_Item (It.Current_Cell);
   end Current_Item;

   ------------------
   -- Current_Item --
   ------------------

   function Current_Item (It : List_Iterator_Ptr) return Item is
   begin
      return Current_Item (It.all);
   end Current_Item;

   -------------------
   -- Current_Index --
   -------------------

   function Current_Index (It : List_Iterator) return Positive is
   begin
      return It.Current_Index;
   end Current_Index;

   ------------
   -- Is_End --
   ------------

   function Is_End (It : in List_Iterator) return Boolean is
   begin
      return It.Is_End = True and then
        (It.Current_Cell = It.Start_Cell or It.Current_Cell = No_Cell);
   end Is_End;

   ------------
   -- Is_End --
   ------------

   function Is_End (It : List_Iterator_Ptr) return Boolean is
   begin
      return Is_End (It.all);
   end Is_End;

   -------------------------
   -- Remove_Current_Item --
   -------------------------

   procedure Remove_Current_Item (It : in out List_Iterator) is
      Cell_To_Delete : Cell_Id;
      Cell_To_Delete_Previous : Cell_Id;
      Cell_To_Delete_Next : Cell_Id;
   begin
      if Is_End (It) then
         raise List_Error;
      else
         Cell_To_Delete := It.Current_Cell;
         Cell_To_Delete_Previous := Get_Previous (Cell_To_Delete);
         Cell_To_Delete_Next := Get_Next (Cell_To_Delete);

         if Cell_To_Delete_Previous = No_Cell then
            Set_First_Cell (It.List, Cell_To_Delete_Next);
         else
            Set_Next (Cell_To_Delete_Previous,Cell_To_Delete_Next);
         end if;
         if Cell_To_Delete_Next = No_Cell then
            Set_Last_Cell (It.List, Cell_To_Delete_Previous);
         else
            Set_Previous (Cell_To_Delete_Next,Cell_To_Delete_Previous);
         end if;
         Delete_Cell (Cell_To_Delete);

         -- pfe : It.List.Count does not seem to be well managed. ?
         if It.List.Count >= 1 then
            It.List.Count := It.List.Count - 1;
         else
	    null;
         end if;

         It.Current_Cell := Cell_To_Delete_Next;
         -- Check for Is_End
         if It.Current_Cell = It.Start_Cell or
           It.Current_Cell = No_Cell Then
            It.Is_End := True;
         end if;

         -- Should do it now otherwise check for Is_End is always pass when
         -- remove first item in the list
         if Cell_To_Delete_Previous = No_Cell then
            It.Start_Cell := Cell_To_Delete_Next;
         end if;
      end if;
   end Remove_Current_Item;

   -------------------------
   -- Remove_Current_Item --
   -------------------------

   procedure Remove_Current_Item (It : in out List_Iterator_Ptr) is
   begin
      Remove_Current_Item (It.all);
   end Remove_Current_Item;

   -------------------------------
   -- Insert_After_Current_Item --
   -------------------------------
   
   procedure Insert_After_Current_Item
     (It     : in out List_Iterator;
      Insert : in Item)
   is
   begin
      if Is_End (It) then
         raise List_Error;
      end if;

      if It.Current_Cell = No_Cell then
         Prepend(Insert, It.List);
         It.Current_Cell := First_Cell(It.List);
         It.Current_Index := Positive'First;
         
      else
         Insert_After(It.List, Get_Item(It.Current_Cell), Insert);
         It.Current_Cell := Get_Next (It.Current_Cell);
         It.Current_Index := It.Current_Index + 1;
      end if;

      if It.Current_Cell = It.Start_Cell then
         It.Is_End := True;
      end if;
   end Insert_After_Current_Item;

   -------------------------------
   -- Insert_After_Current_Item --
   -------------------------------
   
   procedure Insert_After_Current_Item
     (It     : in out List_Iterator_Ptr;
      Insert : in Item)
   is
   begin
      Insert_After_Current_Item (It.all, Insert);
   end Insert_After_Current_Item;

   --------------------------
   -- Replace_Current_Item --
   --------------------------

   procedure Replace_Current_Item
     (It  : in out List_Iterator;
      Elt : in Item) is

   begin
      if Is_End (It) then
         raise List_Error;
      else
         Set_Item (It.Current_Cell, Elt);
      end if;
   end Replace_Current_Item;

   --------------------------
   -- Replace_Current_Item --
   --------------------------

   procedure Replace_Current_Item
     (It  : in out List_Iterator_Ptr;
      Elt : in Item) is
   begin
      Replace_Current_Item (It.all, Elt);
   end Replace_Current_Item;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out List_Iterator) is
   begin
      if Is_End (It) then
         raise List_Error;
      end if;

      It.Current_Cell := Get_Next (It.Current_Cell);
      It.Current_Index := It.Current_Index + 1;

      if It.Current_Cell = No_Cell then
         It.Current_Cell := First_Cell (It.List);
         It.Current_Index := Positive'First;
      end if;

      if It.Current_Cell = It.Start_Cell then
         It.Is_End := True;
      end if;

   end Next;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out List_Iterator_Ptr) is
   begin
      Next (It.all);
   end Next;

   -------------
   -- Iterate --
   -------------

   Procedure Iterate (L: in List_Id) is
      It  : List_Iterator := New_Iterator(L);
      Elmt: Item;
   begin
      Reset(It);
      while not Is_End (It) loop
         Elmt := Current_Item (It);
         Visitor (Elmt);
         Next (It);
      end loop;
   end Iterate;

   ---------------
   -- To_String --
   ---------------

   function Print_List (L : in List_Id) return String is
      Unb : Unbounded_String := Null_Unbounded_String;
      It : List_Iterator := New_Iterator(L);
   begin
      Reset(It);
      while not Is_End(It) loop
         Append(Unb, To_String(Current_Item(It)) & ",");
         Next(It);
      end loop;

      return To_String(Unb);
   end Print_List;

   ---------------------
   -- Reverse_Iterate --
   ---------------------
   procedure Reverse_Iterate (L: in List_Id) is
      C   : Cell_Id;
   begin
      C := L.Last;
      while C /= No_Cell loop
         Visitor (Get_Item(C));
         C := Get_Previous(C);
      end loop;
   end Reverse_Iterate;

   -----------
   -- First --
   -----------

   function First (L : in List_Id) return Item_Id is
   begin
      return Item_Id (L.First);
   end First;

   ----------
   -- Last --
   ----------

   function Last (L : in List_Id) return Item_Id is
   begin
      return Item_Id (L.Last);
   end Last;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Item_Id) is

   begin
      It := Item_Id (Get_Next (Lists_Cells.Cell_Id (It)));
   end Next;

   --------------
   -- Previous --
   --------------

   procedure Previous (It : in out Item_Id) is
   begin
      It := Item_Id (Get_Previous (Lists_Cells.Cell_Id (It)));
   end Previous;

   ----------------
   -- Start_From --
   ----------------

   function Start_From
     (L    : in List_Id;
      From : in Item) return Item_Id is

      It : Item_Id;
   begin
      It := First (L);
      while It /= No_Item_Id and then Current_Item (It) = From Loop
         Next (It);
      end loop;

      return It;
   end Start_From;

   ------------------
   -- Current_Item --
   ------------------

   function Current_Item (It : in Item_Id) return Item is
   begin
      return Get_Item (Lists_Cells.Cell_Id (It));
   end Current_Item;

   ----------------
   -- BubbleSort --
   ----------------

   procedure BubbleSort (L : in out List_Id) is
      Cur : Cell_Id;
      Nex : Cell_Id;
      Cur_It  : Item;
      Nex_It  : Item;
      Finished : Boolean := True;
   begin
      loop
         Finished := True;
         Cur := L.First;
         loop
            Nex := Get_Next(Cur);
            exit when Nex = No_Cell;
            Cur_It := Get_Item(Cur);
            Nex_It := Get_Item(Nex);

            if Less(Nex_It, Cur_It) then
               Set_Item(Nex, Cur_It);
               Set_Item(Cur, Nex_It);

               Finished := False;
            end if;
            Cur := Nex;
         end loop;

         exit when Finished;
      end loop;
   end BubbleSort;

end Artics.Containers.Lists;
