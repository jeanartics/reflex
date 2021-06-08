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

with Artics.Table;
with Unchecked_Conversion;
package body Artics.Elmt_Nlists is

   Free_List : constant List_Id := List_Low_Bound + 1;

   ------------------------------------
   -- Implementation of Object Lists --
   ------------------------------------

   --  An Elmt list is represented by a list header which contains
   --  three fields:

   type List_Header is record
      First : Elmt_Id;
      --  Pointer to first elmt in list. Empty if list is empty

      Last  : Elmt_Id;
      --  Pointer to last elmt in list. Empty if list is empty

      Parent : Elmt_Id;
      --  Pointer to parent of list. Empty if list has no parent
   end record;

   --  The elmt lists are stored in a table indexed by List_Id values

   package Lists is new Table.Table
     (Table_Component_Type => List_Header,
      Table_Index_Type     => List_Id,
      Table_Low_Bound      => First_List_Id,
      Table_Initial        => Lists_Initial,
      Table_Increment      => Lists_Increment,
      Table_Name           => "Lists");

   --  The elmts in the list all have the In_List flag set, and their Link
   --  fields (which otherwise point to the parent) contain the List_Id of
   --  the list header giving immediate access to the list containing the
   --  elmt, and its parent and first and last elements.

   --  Two auxiliary tables, indexed by Elmt_Id values and built in parallel
   --  with the main elmts table and always having the same size contain the
   --  list link values that allow locating the previous and next elmt in a
   --  list. The entries in these tables are valid only if the In_List flag
   --  is set in the corresponding elmt. Next_Elmt is Empty at the end of a
   --  list and Prev_Elmt is Empty at the start of a list.

   package Next_Elmt is new Table.Table
     (Table_Component_Type => Elmt_Id,
      Table_Index_Type     => Elmt_Id,
      Table_Low_Bound      => First_Elmt_Id,
      Table_Initial        => Elmts_Initial,
      Table_Increment      => Elmts_Increment,
      Table_Name           => "Next_Elmt");
   
   package Prev_Elmt is new Table.Table
     (Table_Component_Type => Elmt_Id,
      Table_Index_Type     => Elmt_Id,
      Table_Low_Bound      => First_Elmt_Id,
      Table_Initial        => Elmts_Initial,
      Table_Increment      => Elmts_Increment,
      Table_Name           => "Prev_Elmt");

   -- The table Elmts holds the instances of the Elmt

   type Elmt_Record is record
      Elmt      : Elmt_T;
      List_Link : List_Id;
      In_List   : Boolean;
   end record;

   package Elmts is new Table.Table
     (Table_Component_Type => Elmt_Record,
      Table_Index_Type     => Elmt_Id,
      Table_Low_Bound      => First_Elmt_Id,
      Table_Initial        => Elmts_Initial,
      Table_Increment      => Elmts_Increment,
      Table_Name           => "Elmts");
   
   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Set_First
     (List : List_Id;
      To   : Elmt_Id);
   pragma Inline (Set_First);
   --  Sets First field of list header List to reference To

   procedure Set_Last
     (List : List_Id;
      To   : Elmt_Id);
   pragma Inline (Set_Last);
   --  Sets Last field of list header List to reference To

   procedure Set_List_Link
     (Obj : Elmt_Id;
      To  : List_Id);
   pragma Inline (Set_List_Link);
   --  Sets list link of Elmt to list header To

   procedure Set_Next
     (Obj : Elmt_Id;
      To  : Elmt_Id);
   pragma Inline (Set_Next);
   --  Sets the Next_Elmt pointer for Elmt to reference To

   procedure Set_Prev
     (Obj : Elmt_Id;
      To  : Elmt_Id);
   pragma Inline (Set_Prev);
   --  Sets the Prev_Elmt pointer for Elmt to reference To

   -------------------
   -- Is_Valid_Elmt --
   -------------------

   function Is_Valid_Elmt (Obj : Elmt_Id) return Boolean is
   begin
      return Obj in First_Elmt_Id .. Elmts.Last;
   end Is_Valid_Elmt;

   -----------
   -- First --
   -----------

   function First (List : List_Id) return Elmt_Id is
   begin
      if List = No_List_Id then
	 return No_Elmt;

      else
         pragma Assert (List in First_List_Id .. Lists.Last);
         return Lists.Table (List).First;

      end if;
   end First;

   ----------
   -- Last --
   ----------

   function Last(List : List_Id) return Elmt_Id is
   begin
      if List = No_List_Id then
	 return No_Elmt;

      else
         pragma Assert (List in First_List_Id .. Lists.Last);
         return Lists.Table (List).Last;
      end if;

   end Last;

   ----------
   -- Next --
   ----------

   function Next (Obj : Elmt_Id) return Elmt_Id is
   begin
      return Next_Elmt.Table (Obj);
   end Next;

   ----------
   -- Next --
   ----------

   procedure Next (Obj : in out Elmt_Id) is
   begin
      Obj := Next (Obj);
   end Next;

   --------
   -- No --
   --------

   function No (List : List_Id) return Boolean is
   begin
      return List = No_List_Id;
   end No;

   --------
   -- No --
   --------

   function No (Obj : Elmt_Id) return Boolean is
   begin
      if Obj = Error_Elmt or else Obj = No_Elmt Then
         return True;
      end if;

      return Elmts.Table (Obj).In_List = False;
   end No;

   ----------
   -- Prev --
   ----------

   function Prev (Obj : Elmt_Id) return Elmt_Id is
   begin
      return Prev_Elmt.Table (Obj);
   end Prev;

   ----------
   -- Prev --
   ----------

   procedure Prev (Obj : in out Elmt_Id) is
   begin
      Obj := Prev (Obj);
   end Prev;

   ---------------
   -- Set_First --
   ---------------

   procedure Set_First
     (List : List_Id;
      To   : Elmt_Id) is
   begin
      Lists.Table (List).First := To;
   end Set_First;

   --------------
   -- Set_Last --
   --------------

   procedure Set_Last
     (List : List_Id;
      To   : Elmt_Id) is
   begin
      Lists.Table (List).Last := To;
   end Set_Last;

   -------------------
   -- Set_List_Link --
   -------------------

   procedure Set_List_Link
     (Obj : Elmt_Id;
      To  : List_Id) is
   begin
      Elmts.Table (Obj).List_Link := To;
   end Set_List_Link;

   --------------
   -- Set_Next --
   --------------

   procedure Set_Next
     (Obj : Elmt_Id;
      To  : Elmt_Id) is
   begin
      Next_Elmt.Table (Obj) := To;
   end Set_Next;

   --------------
   -- Set_Prev --
   --------------

   procedure Set_Prev
     (Obj : Elmt_Id;
      To  : Elmt_Id) is
   begin
      Prev_Elmt.Table (Obj) := To;
   end Set_Prev;

   --------------
   -- New_Elmt --
   --------------
   --
   -- CHANGED to support memory reuse in case of elmt deletion
   --
   function New_Elmt return Elmt_Id is
      Obj : Elmt_Id;
   begin
      if Is_Empty_List(Free_List) then
         Elmts.Increment_Last;
         Obj := Elmts.Last;

      else
         Obj := Lists.Table (Free_List).First;
         declare
            Prv : constant Elmt_Id := Prev (Obj);
            Nxt : constant Elmt_Id := Next (Obj);
         begin
            if No (Prv) then
               Set_First (Free_List, Nxt);
            else
               Set_Next(Prv, Nxt);
            end if;

            if No(Nxt) then
               Set_Last(Free_List, Prv);
            else
               Set_Prev(Nxt, Prv);
            end if;
         end;
      end if;
      Elmts.Table (Obj).List_Link := No_List_Id;
      Elmts.Table (Obj).In_List   := False;
      return Obj;
   end New_Elmt;

   --------------
   -- New_Elmt --
   --------------

   -- CHANGED to support memory reuse in case of elmt deletion

   function New_Elmt (Elmt : Elmt_T) return Elmt_Id is

      Obj : Elmt_Id := New_Elmt;
   begin
      Elmts.Table (Obj).Elmt      := Elmt;

      return Obj;
   end New_Elmt;

   -----------------
   -- Delete_Elmt --
   -----------------
   --
   -- CHANGED to support memory reuse after elmt deletion
   --
   procedure Delete_Elmt (Obj : Elmt_Id) is
   begin
      Append(Obj, Free_List);
      --Elmts.Table (Obj).List_Link := No_List;
      --Elmts.Table (Obj).In_List   := False;
   end Delete_Elmt;

   --------------
   -- Put_Elmt --
   --------------

   procedure Put_Elmt
     (Elmt : Elmt_T;
      Obj  : Elmt_Id) is
   begin
      if Obj = Error_Elmt then
	 return;
      end if;

      Elmts.Table (Obj).Elmt := Elmt;
   end Put_Elmt;

   --------------
   -- Get_Elmt --
   --------------

   function Get_Elmt (Obj : Elmt_Id) return Elmt_T is
   begin
      if Obj = Error_Elmt then
	 raise Exception_No_Elmt;
      end if;

      return Elmts.Table (Obj).Elmt;
   end Get_Elmt;

   --------------
   -- Set_Elmt --
   --------------

   procedure Set_Elmt
     (Obj  : Elmt_Id;
      Elmt : Elmt_T) is
   begin
      if Obj = Error_Elmt then
	 raise Exception_No_Elmt;
      end if;

      Elmts.Table (Obj).Elmt := Elmt;
   end Set_Elmt;

   --------------
   -- Get_Elmt --
   --------------

   function Get_Elmt_Ptr (Obj : Elmt_Id) return Elmt_T_Ptr is

      function Convert is new Unchecked_Conversion
	(Source => System.Address,
	 Target => Elmt_T_Ptr);
   begin
      if Obj = Error_Elmt then
	 raise Exception_No_Elmt;
      end if;

      return Convert(Elmts.Table (Obj).Elmt'Address);
   end Get_Elmt_ptr;

   --------------------------
   -- Allocate_List_Tables --
   --------------------------

   procedure Allocate_List_Tables (Obj : Elmt_Id) is

      --   Alloc a new place on the tables Next and Previous
      --   for Obj, and place the object in the Elmts table

   begin
      Next_Elmt.Set_Last (Obj);
      Prev_Elmt.Set_Last (Obj);
      Elmts.Set_Last (Obj);
   end Allocate_List_Tables;

   ------------
   -- Append --
   ------------

   procedure Append
     (Obj  : Elmt_Id;
      To   : List_Id) is

      L : constant Elmt_Id := Last (To);
   begin
      pragma Assert (not Is_List_Member (Obj));

      if Obj = Error_Elmt then
	 return;
      end if;

      if No (L) then
	 Set_First (To, Obj);
      else
	 Set_Next (L, Obj);
      end if;

      Set_Last (To, Obj);

      Elmts.Table (Obj).In_List := True;

      Set_Next      (Obj, No_Elmt);
      Set_Prev      (Obj, L);

      Set_List_Link (Obj, To);

   end Append;

   ---------------
   -- Append_To --
   ---------------

   procedure Append_To
     (To   : List_Id;
      Obj  : Elmt_Id) is
   begin
      Append (Obj, To);
   end Append_To;

   -----------------
   -- Append_List --
   -----------------

   procedure Append_List
     (List : List_Id;
      To   : List_Id) is
   begin
      if Is_Empty_List (List) then
	 return;

      else
	 declare
	    L   : constant Elmt_Id := Last (To);
	    F   : constant Elmt_Id := First (List);
	    Obj : Elmt_Id;

	 begin
	    Obj := F;
	    loop
	       Set_List_Link (Obj, To);
	       Obj := Next (Obj);
	       exit when No (Obj);
	    end loop;

	    if No (L) then
	       Set_First (To, F);
	    else
	       Set_Next (L, F);
	    end if;

	    Set_Prev (F, L);
	    Set_Last (To, Last (List));

	    Set_First (List, No_Elmt);
	    Set_Last  (List, No_Elmt);
	 end;
      end if;
   end Append_List;

   --------------------
   -- Append_List_To --
   --------------------

   procedure Append_List_To
     (To   : List_Id;
      List : List_Id) is
   begin
      Append_List (List, To);
   end Append_List_To;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

      E : constant List_Id := Error_List_Id;
      Dummy_Elmt : Elmt_Id;
   begin
      Lists.Init;
      Next_Elmt.Init;
      Prev_Elmt.Init;
      Elmts.Init;

      --  Allocate Error_List list header

      Lists.Increment_Last;
      Set_Parent (E, No_Elmt);
      Set_First  (E, No_Elmt);
      Set_Last   (E, No_Elmt);

      --  Allocate Free_List list header

      Lists.Increment_Last;
      Set_Parent (Free_List, No_Elmt);
      Set_First  (Free_List, No_Elmt);
      Set_Last   (Free_List, No_Elmt);

      -- Allocate No_Elmt
      Dummy_Elmt := New_Elmt;
      
      -- Allocate Error_Elmt
      Dummy_Elmt := New_Elmt;
   end Initialize;

   ------------------
   -- Insert_After --
   ------------------

   procedure Insert_After
     (After : Elmt_Id;
      Obj   : Elmt_Id) is
   begin
      pragma Assert
        (Is_List_Member (After) and then not Is_List_Member (Obj));

      if Obj = Error_Elmt then
	 return;
      end if;

      declare
         Before : constant Elmt_Id := Next (After);
         LC     : constant List_Id := List_Containing (After);

      begin
         if Present (Before) then
            Set_Prev (Before, Obj);
         else
            Set_Last (LC, Obj);
         end if;

         Set_Next (After, Obj);

         Elmts.Table (Obj).In_List := True;

         Set_Prev      (Obj, After);
         Set_Next      (Obj, Before);
         Set_List_Link (Obj, LC);
      end;
   end Insert_After;

   -------------------
   -- Insert_Before --
   -------------------

   procedure Insert_Before
     (Before : Elmt_Id;
      Obj    : Elmt_Id) is
   begin
      pragma Assert
        (Is_List_Member (Before) and then not Is_List_Member (Obj));

      if Obj = Error_Elmt then
         return;
      end if;

      declare
         After : constant Elmt_Id := Prev (Before);
         LC    : constant List_Id := List_Containing (Before);

      begin
         if Present (After) then
            Set_Next (After, Obj);
         else
            Set_First (LC, Obj);
         end if;

         Set_Prev (Before, Obj);

         Elmts.Table (Obj).In_List := True;

         Set_Prev      (Obj, After);
         Set_Next      (Obj, Before);
         Set_List_Link (Obj, LC);
      end;
   end Insert_Before;

   -----------------------
   -- Insert_List_After --
   -----------------------

   procedure Insert_List_After
     (After : Elmt_Id;
      List  : List_Id) is
   begin
      pragma Assert (Is_List_Member (After));

      if Is_Empty_List (List) then
	 return;
      else
	 declare
	    Before : constant Elmt_Id := Next (After);
	    LC     : constant List_Id := List_Containing (After);
	    F      : constant Elmt_Id := First (List);
	    L      : constant Elmt_Id := Last (List);
	    Obj    : Elmt_Id;

	 begin

	    Obj := F;
	    loop
	       Set_List_Link (Obj, LC);
	       exit when Obj = L;
	       Obj := Next (Obj);
	    end loop;

	    if Present (Before) then
	       Set_Prev (Before, L);
	    else
	       Set_Last (LC, L);
	    end if;

	    Set_Next (After, F);
	    Set_Prev (F, After);
	    Set_Next (L, Before);

	    Set_First (List, No_Elmt);
	    Set_Last  (List, No_Elmt);
	 end;
      end if;
   end Insert_List_After;

   ------------------------
   -- Insert_List_Before --
   ------------------------

   procedure Insert_List_Before
     (Before : Elmt_Id;
      List   : List_Id) is
   begin
      pragma Assert (Is_List_Member (Before));

      if Is_Empty_List (List) then
         return;

      else
         declare
            After : constant Elmt_Id := Prev (Before);
            LC    : constant List_Id   := List_Containing (Before);
            F     : constant Elmt_Id := First (List);
            L     : constant Elmt_Id := Last (List);
            Obj   : Elmt_Id;

         begin

            Obj := F;
            loop
               Set_List_Link (Obj, LC);
               exit when Obj = L;
               Obj := Next (Obj);
            end loop;

            if Present (After) then
               Set_Next (After, F);
            else
               Set_First (LC, F);
            end if;

            Set_Prev (Before, L);
            Set_Prev (F, After);
            Set_Next (L, Before);

            Set_First (List, No_Elmt);
            Set_Last  (List, No_Elmt);
         end;
      end if;
   end Insert_List_Before;

   -------------------
   -- Is_Empty_List --
   -------------------

   function Is_Empty_List (List : List_Id) return Boolean is
   begin
      return First (List) = No_Elmt;
   end Is_Empty_List;

   --------------------
   -- Is_List_Member --
   --------------------

   -- CHANGED to support memory reuse when elmt deletion

   function Is_List_Member (Obj : Elmt_Id) return Boolean is
   begin
      return Elmts.Table (Obj).In_List
        and Elmts.Table (Obj).List_Link /= Free_List;
   end Is_List_Member;

   -----------------------
   -- Is_Non_Empty_List --
   -----------------------

   function Is_Non_Empty_List (List : List_Id) return Boolean is
   begin
      return List /= No_List_Id and then First (List) /= No_Elmt;
   end Is_Non_Empty_List;

   ------------------
   -- Last_List_Id --
   ------------------

   function Last_List_Id return List_Id is
   begin
      return Lists.Last;
   end Last_List_Id;

   ---------------------
   -- List_Containing --
   ---------------------

   function List_Containing (Obj : Elmt_Id) return List_Id is
   begin
      pragma Assert (Is_List_Member (Obj));

      return List_Id (Elmts.Table (Obj).List_Link);
   end List_Containing;

   -----------------
   -- List_Length --
   -----------------

   function List_Length (List : List_Id) return Nat is

      Result : Nat;
      Obj    : Elmt_Id;
   begin
      Result := 0;
      Obj    := First (List);
      while Present (Obj) loop
	 Result := Result + 1;
	 Obj    := Next (Obj);
      end loop;

      return Result;
   end List_Length;

   -------------------
   -- Lists_Address --
   -------------------

   function Lists_Address return System.Address is

   begin
      return Lists.Table (First_List_Id)'Address;
   end Lists_Address;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      Lists.Locked := True;
      Lists.Release;

      Prev_Elmt.Locked := True;
      Next_Elmt.Locked := True;

      Prev_Elmt.Release;
      Next_Elmt.Release;
   end Lock;

   -----------------------
   -- Next_Elmt_Address --
   -----------------------

   function Next_Elmt_Address return System.Address is
   begin
      return Next_Elmt.Table (First_Elmt_Id)'Address;
   end Next_Elmt_Address;

   --------------
   -- New_List --
   --------------

   function New_List return List_Id is
   begin
      Lists.Increment_Last;

      declare
	 List : constant List_Id := Lists.Last;

      begin
	 Set_Parent (List, No_Elmt);
	 Set_First  (List, No_Elmt);
	 Set_Last   (List, No_Elmt);

	 return (List);
      end;
   end New_List;

   --------------
   -- New_List --
   --------------

   function New_List (Obj  : Elmt_Id) return List_Id is
   begin
      if Obj = Error_Elmt then
	 return New_List;

      else
	 pragma Assert (not Is_List_Member (Obj));

	 Lists.Increment_Last;

	 declare
	    List : constant List_Id := Lists.Last;

	 begin
	    Set_Parent (List, No_Elmt);
	    Set_First  (List, Obj);
	    Set_Last   (List, Obj);

	    Elmts.Table (Obj).In_List := True;

	    Set_List_Link (Obj, List);
	    Set_Prev (Obj, No_Elmt);
	    Set_Next (Obj, No_Elmt);
	    return List;
	 end;
      end if;
   end New_List;

   --------------
   -- New_List --
   -- -----------

   function New_List
     (Obj    : Elmt_Id;
      Parent : Elmt_Id) return List_Id is
   begin
      if Obj = Error_Elmt then
	 return New_List;

      else
	 pragma Assert (not Is_List_Member (Obj));

	 Lists.Increment_Last;

	 declare
	    List : constant List_Id := Lists.Last;

	 begin
	    if Parent = Error_Elmt then
	       Set_Parent (List, No_Elmt);
	    else
	       Set_Parent (List, Parent);
	    end if;

	    Set_First  (List, Obj);
	    Set_Last   (List, Obj);

	    Elmts.Table (Obj).In_List := True;

	    Set_List_Link (Obj, List);
	    Set_Prev (Obj, No_Elmt);
	    Set_Next (Obj, No_Elmt);
	    return List;
	 end;
      end if;
   end New_List;

   -------------------
   -- New_List_Copy --
   -------------------

   function New_List_Copy (List : List_Id) return List_Id is

      NL : List_Id;
      E  : Elmt_Id;
   begin
      if List = No_List_Id then
	 return No_List_Id;

      else
	 NL := New_List;
	 E  := First (List);

	 while Present (E) loop
	    --JMA               Append (New_Copy (E), NL);
	    E := Next (E);
	 end loop;

	 return NL;
      end if;
   end New_List_Copy;

   ---------------
   -- Num_Lists --
   ---------------

   function Num_Lists return Nat is
   begin
      return Int (Lists.Last) - Int (Lists.First) + 1;
   end Num_Lists;

   ------------
   -- Parent --
   ------------

   function Parent (List : List_Id) return Elmt_Id is
   begin
      pragma Assert (List in First_List_Id .. Lists.Last);
      return Lists.Table (List).Parent;
   end Parent;

   ------------
   -- Parent --
   ------------

   function Parent (Obj : Elmt_Id) return Elmt_Id is

      L : List_Id := Elmts.Table (Obj).List_Link;
   begin
      return Parent (L);
   end Parent;

   ----------
   -- Pick --
   ----------

   function Pick
     (List  : List_Id;
      Index : Pos) return Elmt_Id is

      Elmt : Elmt_Id;
   begin
      Elmt := First (List);

      for J in 1 .. Index - 1 loop
	 Elmt := Next (Elmt);
      end loop;

      return Elmt;
   end Pick;

   --------------
   -- Position --
   --------------

   function Position
     (List  : in List_Id;
      Elmt  : in Elmt_Id) return Nat
   is
      Cur : Elmt_Id;
      Count : Nat := 0;
      Found : Boolean := False;
   begin
      if List /= No_List_Id then
         Cur := First(List);
         while not No(Cur) loop
            if Elmt = Cur then
               Found := True;
               exit;
            end if;
            Count := Count + 1;
            Cur := Next (Cur);
         end loop;
      end if;

      return Count;
   end Position;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Obj  : Elmt_Id;
      To   : List_Id) is

      F : constant Elmt_Id := First (To);
   begin
      pragma Assert (not Is_List_Member (Obj));

      if Obj = Error_Elmt then
	 return;
      end if;

      if No (F) then
	 Set_Last (To, Obj);
      else
	 Set_Prev (F, Obj);
      end if;

      Set_First (To, Obj);

      Elmts.Table (Obj).In_List := True;

      Set_Next      (Obj, F);
      Set_Prev      (Obj, No_Elmt);
      Set_List_Link (Obj, To);
   end Prepend;

   ----------------
   -- Prepend_To --
   ----------------

   procedure Prepend_To
     (To   : List_Id;
      Obj  : Elmt_Id) is
   begin
      Prepend (Obj, To);
   end Prepend_To;

   -------------
   -- Present --
   -------------

   function Present (List : List_Id) return Boolean is
   begin
      return List /= No_List_Id;
   end Present;

   -------------
   -- Present --
   -------------

   function Present (Obj : Elmt_Id) return Boolean is
   begin
      return Obj /= No_Elmt;
   end Present;

   -----------------------
   -- Prev_Node_Address --
   -----------------------

   function Prev_Elmt_Address return System.Address is
   begin
      return Prev_Elmt.Table (First_Elmt_Id)'Address;
   end Prev_Elmt_Address;

   ------------
   -- Remove --
   ------------

   procedure Remove (Obj : Elmt_Id) is

      Lst : constant List_Id := List_Containing (Obj);
      Prv : constant Elmt_Id := Prev (Obj);
      Nxt : constant Elmt_Id := Next (Obj);
   begin
      if No (Prv) then
	 Set_First (Lst, Nxt);
      else
	 Set_Next (Prv, Nxt);
      end if;

      if No (Nxt) then
	 Set_Last (Lst, Prv);
      else
	 Set_Prev (Nxt, Prv);
      end if;

      Elmts.Table (Obj).In_List := False;

   end Remove;

   -----------------
   -- Remove_Head --
   -----------------

   function Remove_Head (List : List_Id) return Elmt_Id is

      Frst : constant Elmt_Id := First (List);
   begin
      if Frst = No_Elmt then
	 return No_Elmt;

      else
	 declare
	    Nxt : constant Elmt_Id := Next (Frst);

	 begin
	    Set_First (List, Nxt);

	    if No (Nxt) then
	       Set_Last (List, No_Elmt);
	    else
	       Set_Prev (Nxt, No_Elmt);
	    end if;

	    Elmts.Table (Frst).In_List := False;

	    return Frst;
	 end;
      end if;
   end Remove_Head;

   -----------------
   -- Remove_Next --
   -----------------

   function Remove_Next (Obj : Elmt_Id) return Elmt_Id is

      Nxt : constant Elmt_Id := Next (Obj);
   begin
      if Present (Nxt) then

	 declare
	    Nxt2 : constant Elmt_Id := Next (Nxt);
	    LC   : constant List_Id   := List_Containing (Obj);

	 begin
	    Set_Next (Obj, Nxt2);

	    if No (Nxt2) then
	       Set_Last (LC, Obj);
	    else
	       Set_Prev (Nxt2, Obj);
	    end if;

	    Elmts.Table (Nxt).In_List := False;
	 end;
      end if;

      return Nxt;
   end Remove_Next;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent
     (List : List_Id;
      Obj  : Elmt_Id) is
   begin
      pragma Assert (List in First_List_Id .. Lists.Last);
      Lists.Table (List).Parent := Obj;
   end Set_Parent;

end Artics.Elmt_Nlists;


















