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

with System;        use System;
with System.Memory; use System.Memory;
with System.Address_To_Access_Conversions;

package body Artics.Vector is


   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Reallocate (T : in out Vector);
   --  Reallocate the existing table according to the current value stored
   --  in Max. Works correctly to do an initial allocation if the table
   --  is currently null.

   package Table_Conversions is
      new System.Address_To_Access_Conversions (Big_Table_Type);
   --  Address and Access conversions for a Table object.

   function To_Address (Table : Table_Ptr) return Address;
   pragma Inline (To_Address);
   --  Returns the Address for the Table object.

   function To_Pointer (Table : Address) return Table_Ptr;
   pragma Inline (To_Pointer);
   --  Returns the Access pointer for the Table object.

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (T   : in out Vector;
      Num : Integer := 1) is
   begin
      T.Last_Val := T.Last_Val + Num;

      if T.Last_Val > T.Max then
         Reallocate (T);
      end if;
   end Allocate;

   ------------
   -- Append --
   ------------

   procedure Append
     (T       : in out Vector;
      New_Val : Table_Component_Type) is
   begin
      Increment_Last (T);
      T.Table (Table_Index_Type (T.Last_Val)) := New_Val;
   end Append;

   --------------------
   -- Decrement_Last --
   --------------------

   procedure Decrement_Last (T : in out Vector) is
   begin
      T.Last_Val := T.Last_Val - 1;
   end Decrement_Last;

   ----------
   -- Free --
   ----------

   procedure Free (T : in out Vector) is
   begin
      Free (To_Address (T.Table));
      T.Table := null;
      T.Length := 0;
   end Free;

   --------------------
   -- Increment_Last --
   --------------------

   procedure Increment_Last (T : in out Vector) is
   begin
      T.Last_Val := T.Last_Val + 1;

      if T.Last_Val > T.Max then
         Reallocate (T);
      end if;
   end Increment_Last;

   ----------
   -- Init --
   ----------

   procedure Init (T : in out Vector) is

      Old_Length : constant Integer := T.Length;
   begin
      T.Min      := Integer (Table_Low_Bound);
      T.Max      := T.Min + T.Table_Initial - 1;
      T.Length   := T.Max - T.Min + 1;
      T.Last_Val := T.Min - 1;

      --  If table is same size as before (happens when table is never
      --  expanded which is a common case), then simply reuse it. Note
      --  that this also means that an explicit Init call right after
      --  the implicit one in the package body is harmless.

      if Old_Length = T.Length then
         return;

      --  Otherwise we can use Reallocate to get a table of the right size.
      --  Note that Reallocate works fine to allocate a table of the right
      --  initial size when it is first allocated.

      else
         Reallocate (T);
      end if;
   end Init;

   ----------
   -- Last --
   ----------

   function Last (T : in Vector) return Table_Index_Type is
   begin
      if T.Last_Val < 0 then
         return 0;
      else
         return Table_Index_Type (T.Last_Val);
      end if;
   end Last;

   ----------------
   -- Reallocate --
   ----------------

   procedure Reallocate (T : in out Vector) is
      New_Size : Size_T;

   begin
      while T.Max < T.Last_Val loop
         T.Length := T.Length + Integer (T.Table_Increment);
         T.Max := T.Min + T.Length - 1;
      end loop;

      New_Size :=
        size_t ((T.Max - T.Min + 1) *
                (Table_Type'Component_Size / Storage_Unit));

      if T.Table = null then
         T.Table := To_Pointer (Alloc (New_Size));

      elsif New_Size > 0 then
         T.Table := To_Pointer (Realloc (Ptr  => To_Address (T.Table),
                                         Size => New_Size));
      end if;

      if T.Length /= 0 and then T.Table = null then
         raise Storage_Error;
      end if;
   end Reallocate;

   -------------
   -- Release --
   -------------

   procedure Release (T : in out Vector) is
   begin
      T.Length := T.Last_Val - Integer (Table_Low_Bound) + 1;
      T.Max    := T.Last_Val;
      Reallocate (T);
   end Release;

   --------------
   -- Set_Last --
   --------------

   procedure Set_Last
     (T       : in out Vector;
      New_Val : Table_Index_Type) is
   begin
      if Integer (New_Val) < T.Last_Val then
         T.Last_Val := Integer (New_Val);

      else
         T.Last_Val := Integer (New_Val);

         if T.Last_Val > T.Max then
            Reallocate (T);
         end if;
      end if;
   end Set_Last;

   ----------------
   -- To_Address --
   ----------------

   function To_Address (Table : Table_Ptr) return Address is
   begin
      return Table_Conversions.To_Address
        (Table_Conversions.Object_Pointer (Table));
   end To_Address;

   ----------------
   -- To_Pointer --
   ----------------

   function To_Pointer (Table : Address) return Table_Ptr is
   begin
      return Table_Ptr (Table_Conversions.To_Pointer (Table));
   end To_Pointer;

   ----------------
   -- New_Vector --
   ----------------

   function New_Vector return Vector is

      V : Vector;
   begin
      V := New_Vector (10, 10);

      return V;
   end New_Vector;

   ----------------
   -- New_Vector --
   ----------------

   function New_Vector (From : Vector) return Vector is

      V : Vector;
   begin
      V := New_Vector (From.Table_Initial, From.Table_Increment);
      Copy (From, V);

      return V;
   end New_Vector;

   ----------------
   -- New_Vector --
   ----------------

   function New_Vector (Initial : Positive) return Vector is

      V : Vector;
   begin
      V := New_Vector (Initial, 10);

      return V;
   end New_Vector;

   ----------------
   -- New_Vector --
   ----------------

   function New_Vector
     (Initial   : Positive;
      Increment : Natural) return Vector is

      V : Vector := new Vector_Record;
   begin

      V.Max    := Integer (Initial);
      V.Min    := Integer (Table_Low_Bound);
      V.Length := 0;

      V.Last_Val        := V.Min -1;
      V.Table_Initial   := Initial;
      V.Table_Increment := Increment;

      Init (V);

      return V;
   end New_Vector;

   ---------------
   -- Set_Empty --
   ---------------

   procedure Set_Empty (V : Vector) is
   begin
      V.Last_Val := V.Min -1;
   end Set_Empty;

   -------------------
   -- Initial_Value --
   -------------------

   procedure Initial_Value
     (V   : Vector;
      Val : Table_Component_Type) is
   begin
      if V.Last_Val < V.Min then
         V.Table (Table_Index_Type (V.Min)..Table_Index_Type (V.Max)) :=
           (others => Val);
      else
         V.Table (Table_Index_Type (V.Last_Val)..Table_Index_Type (V.Max))
           := (others => Val);
      end if;
   end Initial_Value;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (V : in out Vector) is

   begin
      Set_Last (V, Table_Index_Type (V.Min));
      --Free(V);
      Release (V);
   end Destroy;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (From : Vector;
      To   : in out Vector) is

      First_From : Table_Index_Type := Table_Index_Type (From.Min);
      Last_From  : Table_Index_Type := Last (From);
      Count_From : Natural := Count (From);
      First_To   : Table_Index_Type := Table_Index_Type (To.Min);
      Last_To    : Table_Index_Type;
   begin
      Last_To := First_To + Table_Index_Type (Count_From) - 1;
      Set_Last (To, Last_To);

      To.Table (First_To..Last_To) := From.Table (First_From..Last_From);
   end Copy;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (From       : in Vector;
      Start_From : in Table_Index_Type;
      To         : in out Vector;
      Start_To   : in Table_Index_Type;
      Count      : in Table_Index_Type) is

      Last_From : Table_Index_Type := Start_From + Count;
      Last_To   : Table_Index_Type := Start_To + Count;
   begin
      if Last_To > Last (To) then
         Set_Last (To, Last_To);
      end if;

      To.Table (Start_To..Last_To) := From.Table (Start_From..Last_From);
   end Copy;

   --------------------------
   -- Set_Vector_Increment --
   --------------------------

   procedure Set_Vector_Increment
     (V         : in out Vector;
      Increment : Natural) is
   begin
      V.Table_Increment := Increment;
   end Set_Vector_Increment;

   -----------
   -- Reset --
   -----------

   procedure Reset (V : in out Vector) is
   begin
      null;
   end Reset;

   ------------
   -- Length --
   ------------

   function Length (V : Vector) return Natural is
   begin
      return Natural (V.Length);
   end Length;

   -----------
   -- Count --
   -----------

   function Count (V : Vector) return Natural is
   begin
      if V.Last_Val < V.Min then
         return 0;
      else
         return Natural (V.Last_Val - V.Min + 1);
      end if;
   end Count;

   -----------
   -- First --
   -----------

   function First (V : in Vector) return Table_Index_Type is
   begin
      return Table_Index_Type (V.Min);
   end First;

   -----------------
   -- Get_Item_At --
   -----------------

   function Get_Item_At
     (V     : Vector;
      Index : Table_Index_Type) return Table_Component_Type is
   begin
      if Integer (Index) > V.Last_Val then
         raise Vector_Exception;
      end if;

      return V.Table (Index);
   end Get_Item_At;

   -----------------
   -- Set_Item_At --
   -----------------

   procedure Set_Item_At
     (V     : in out Vector;
      Index : Table_Index_Type;
      Value : Table_Component_Type) is
   begin
      if Integer (Index) > V.Max then
         Set_Last (V, Index);
      end if;

      V.Table (Index) := Value;
   end Set_Item_At;

   ----------------
   -- First_Item --
   ----------------

   function First_Item (V : Vector) return Table_Component_Type is
   begin
      return Get_Item_At (V, First (V));
   end First_Item;

   ---------------
   -- Last_Item --
   ---------------

   function Last_Item (V : Vector) return Table_Component_Type is
   begin
      return Get_Item_At (V, Last (V));
   end Last_Item;

   -------------------
   -- Append_Vector --
   -------------------

   procedure Append_Vector
     (To   : in out Vector;
      V    : Vector;
      Nb   : in Natural := 0) is

      Count_V : Natural := Nb;
      Last_V  : Table_Index_Type := Last (V);
      Last_To : Table_Index_Type := Last (To);
   begin
      if Count_V = 0 then
         Count_V := Count(V);
      end if;
      Set_Last (To, Last_To + Table_Index_Type (Count_V));

      To.Table (Last_To + 1 .. Last (To)) :=
        V.Table(Table_Index_Type(V.Min)..Table_Index_Type(V.Min +Count_V - 1));
   end Append_Vector;

   ---------------
   -- Get_Table --
   ---------------

   function Get_Table (V : Vector) return Table_Ptr is
   begin
      return V.Table;
   end Get_Table;

   -----------------
   -- Dump_Vector --
   -----------------

   procedure Dump_Vector (V : Vector) is
   begin
      null;
   end Dump_Vector;

   ----------
   -- Push --
   ----------

   procedure Push
     (V    : in out Vector;
      Item : in Table_Component_Type) is
   begin
      Append (V, Item);
   end Push;

   ---------
   -- Top --
   ---------

   function Top (V : in Vector) return Table_Component_Type is
   begin
      if Is_Empty (V) then
         raise Vector_Exception;
      end if;

      return Get_Item_At (V, Last (V));
   end Top;

   ---------
   -- Pop --
   ---------

   procedure Pop
     (V : in out Vector;
      Item : out Table_Component_Type) is

   begin
      if Is_Empty (V) then
         raise Vector_Exception;
      end if;

      Item := Get_Item_At (V, Last (V));
      Decrement_Last (V);
   end Pop;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (V : in Vector) return Boolean is
   begin
      return Count (V) = 0;
   end Is_Empty;

end Artics.Vector;
