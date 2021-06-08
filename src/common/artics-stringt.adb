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

package body Artics.Stringt is
   
   String_Chars_Initial             : constant := 2_500;   -- Stringt
   String_Chars_Increment           : constant := 150;
   
   Strings_Initial                  : constant := 5_00;    -- Stringt
   Strings_Increment                : constant := 150;

   
   --  The following table stores the sequence of character codes for the
   --  stored string constants. The entries are referenced from the
   --  separate Strings table.

   package String_Chars is new Table.Table (
     Table_Component_Type => Char_Code,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 0,
     Table_Initial        => String_Chars_Initial,
     Table_Increment      => String_Chars_Increment,
     Table_Name           => "String_Chars");

   --  The String_Id values reference entries in the Strings table, which
   --  contains String_Entry records that record the length of each stored
   --  string and its starting location in the String_Chars table.

   type String_Entry is record
      String_Index : Integer;
      Length       : Natural;
   end record;

   package Strings is new Table.Table (
     Table_Component_Type => String_Entry,
     Table_Index_Type     => String_Id'Base,
     Table_Low_Bound      => First_String_Id,
     Table_Initial        => Strings_Initial,
     Table_Increment      => Strings_Increment,
     Table_Name           => "Strings");

   --  Note: it is possible that two entries in the Strings table can share
   --  string data in the String_Chars table, and in particular this happens
   --  when Start_String is called with a parameter that is the last string
   --  currently allocated in the table.

   -------------------------------
   -- Add_String_To_Name_Buffer --
   -------------------------------

   procedure Add_String_To_Name_Buffer (S : String_Id) is
      Len : constant Natural := Natural (String_Length (S));

   begin
      for J in 1 .. Len loop
         Str_Name_Buffer (Str_Name_Len + J) :=
           Get_Character (Get_String_Char (S, Integer (J)));
      end loop;

      Str_Name_Len := Str_Name_Len + Len;
   end Add_String_To_Name_Buffer;

   ----------------
   -- End_String --
   ----------------

   function End_String return String_Id is
   begin
      return Strings.Last;
   end End_String;

   ---------------------
   -- Get_String_Char --
   ---------------------

   function Get_String_Char (Id : String_Id; Index : Integer) return Char_Code is
   begin
      pragma Assert (Id in First_String_Id .. Strings.Last
                       and then Index in 1 .. Strings.Table (Id).Length);

      return String_Chars.Table (Strings.Table (Id).String_Index + Index - 1);
   end Get_String_Char;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      String_Chars.Init;
      Strings.Init;
   end Initialize;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      String_Chars.Locked := True;
      Strings.Locked := True;
      String_Chars.Release;
      Strings.Release;
   end Lock;

   ------------------
   -- Start_String --
   ------------------

   --  Version to start completely new string

   procedure Start_String is
   begin
      Strings.Increment_Last;
      Strings.Table(Strings.Last) := 
        (String_Index => String_Chars.Last + 1, Length => 0);
   end Start_String;

   --  Version to start from initially stored string

   procedure Start_String (S : String_Id) is
   begin
      Strings.Increment_Last;

      --  Case of initial string value is at the end of the string characters
      --  table, so it does not need copying, instead it can be shared.

      if Strings.Table (S).String_Index + Strings.Table (S).Length =
                                                    String_Chars.Last + 1
      then
         Strings.Table (Strings.Last).String_Index :=
           Strings.Table (S).String_Index;

      --  Case of initial string value must be copied to new string

      else
         Strings.Table (Strings.Last).String_Index :=
           String_Chars.Last + 1;

         for J in 1 .. Strings.Table (S).Length loop
            String_Chars.Increment_Last;
            String_Chars.Table(String_Chars.Last) := 
              String_Chars.Table (Strings.Table (S).String_Index + (J - 1));
         end loop;
      end if;

      --  In either case the result string length is copied from the argument

      Strings.Table (Strings.Last).Length := Strings.Table (S).Length;
   end Start_String;

   -----------------------
   -- Store_String_Char --
   -----------------------

   procedure Store_String_Char (C : Char_Code) is
   begin
      String_Chars.Increment_Last;
      String_Chars.Table(String_Chars.Last) := C;
      Strings.Table (Strings.Last).Length :=
        Strings.Table (Strings.Last).Length + 1;
   end Store_String_Char;

   procedure Store_String_Char (C : Character) is
   begin
      Store_String_Char (Get_Char_Code (C));
   end Store_String_Char;

   ------------------------
   -- Store_String_Chars --
   ------------------------

   procedure Store_String_Chars (S : String) is
   begin
      for J in S'First .. S'Last loop
         Store_String_Char (Get_Char_Code (S (J)));
      end loop;
   end Store_String_Chars;

   procedure Store_String_Chars (S : String_Id) is

      --  We are essentially doing this:

      --   for J in 1 .. String_Length (S) loop
      --      Store_String_Char (Get_String_Char (S, J));
      --   end loop;

      --  but when the string is long it's more efficient to grow the
      --  String_Chars table all at once.

      S_First  : constant Integer := Strings.Table (S).String_Index;
      S_Len    : constant Integer := String_Length (S);
      Old_Last : constant Integer := String_Chars.Last;
      New_Last : constant Integer := Old_Last + S_Len;

   begin
      String_Chars.Set_Last (New_Last);
      String_Chars.Table (Old_Last + 1 .. New_Last) :=
        String_Chars.Table (S_First .. S_First + S_Len - 1);
      Strings.Table (Strings.Last).Length :=
        Strings.Table (Strings.Last).Length + S_Len;
   end Store_String_Chars;

   ----------------------
   -- Store_String_Int --
   ----------------------

   procedure Store_String_Int (N : Int) is
   begin
      if N < 0 then
         Store_String_Char ('-');
         Store_String_Int (-N);

      else
         if N > 9 then
            Store_String_Int (N / 10);
         end if;

         Store_String_Char (Character'Val (Character'Pos ('0') + N mod 10));
      end if;
   end Store_String_Int;

   --------------------------
   -- String_Chars_Address --
   --------------------------

   function String_Chars_Address return System.Address is
   begin
      return String_Chars.Table (0)'Address;
   end String_Chars_Address;

   ------------------
   -- String_Equal --
   ------------------

   function String_Equal (L, R : String_Id) return Boolean is
      Len : constant Natural := Strings.Table (L).Length;

   begin
      if Len /= Strings.Table (R).Length then
         return False;
      else
         for J in 1 .. Len loop
            if Get_String_Char (L, J) /= Get_String_Char (R, J) then
               return False;
            end if;
         end loop;

         return True;
      end if;
   end String_Equal;

   -----------------------------
   -- String_From_Name_Buffer --
   -----------------------------

   function String_From_Name_Buffer return String_Id is
   begin
      Start_String;

      for J in 1 .. Str_Name_Len loop
         Store_String_Char (Get_Char_Code (Str_Name_Buffer (J)));
      end loop;

      return End_String;
   end String_From_Name_Buffer;

   -------------------
   -- String_Length --
   -------------------

   function String_Length (Id : String_Id) return Natural is
   begin
      return Strings.Table (Id).Length;
   end String_Length;

   ---------------------------
   -- String_To_Name_Buffer --
   ---------------------------

   procedure String_To_Name_Buffer (S : String_Id) is
   begin
      Str_Name_Len := Natural (String_Length (S));

      for J in 1 .. Str_Name_Len loop
         Str_Name_Buffer (J) :=
           Get_Character (Get_String_Char (S, Integer (J)));
      end loop;
   end String_To_Name_Buffer;

   ---------------------
   -- Strings_Address --
   ---------------------

   function Strings_Address return System.Address is
   begin
      return Strings.Table (First_String_Id)'Address;
   end Strings_Address;

   -------------------------
   -- Unstore_String_Char --
   -------------------------

   procedure Unstore_String_Char is
   begin
      String_Chars.Decrement_Last;
      Strings.Table (Strings.Last).Length :=
        Strings.Table (Strings.Last).Length - 1;
   end Unstore_String_Char;

end Artics.Stringt;
