------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
--                                                                          --
-- Reflex is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public Licensea as published  by the Free Soft- --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Artics.Table;

package body Artics.Strings_Stocks is
   
   String_Chars_Initial   : constant := 2_500;
   String_Chars_Increment : constant := 150;
   Strings_Initial        : constant := 5_00;
   Strings_Increment      : constant := 150;
   
   --  The following table stores the sequence of character codes for the
   --  stored string constants. The entries are referenced from the
   --  separate Strings table.

   package String_Chars is new Artics.Table.Table (
     Table_Component_Type => Char_Code,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 0,
     Table_Initial        => String_Chars_Initial,
     Table_Increment      => String_Chars_Increment,
     Table_Name           => "String_Chars");

   --  The Str_Id values reference entries in the Strings table, which
   --  contains String_Entry records that record the length of each stored
   --  string and its starting location in the String_Chars table.

   type String_Entry is record
      String_Index : Integer;
      Length       : Natural;
   end record;

   package Strings is new Artics.Table.Table (
     Table_Component_Type => String_Entry,
     Table_Index_Type     => Str_Id'Base,
     Table_Low_Bound      => First_Str_Id,
     Table_Initial        => Strings_Initial,
     Table_Increment      => Strings_Increment,
     Table_Name           => "Strings");

   --  Note: it is possible that two entries in the Strings table can share
   --  string data in the String_Chars table, and in particular this happens
   --  when Start_String is called with a parameter that is the last string
   --  currently allocated in the table.

   Strings_Last      : Str_Id := First_Str_Id;
   String_Chars_Last : Integer := 0;
   --  Strings_Last and String_Chars_Last are used by procedure Mark and
   --  Release to get a snapshot of the tables and to restore them to their
   --  previous situation.

   ----------------
   -- Get_String --
   ----------------
   
   function Get_String (S : Str_Id) return String  is
      Len : Natural := String_Length (S);
   begin
      if Len = 0 then
	 return "";
      else
	 declare
	    Str : String (1..Len);
	 begin
	    for I in 1 .. Len loop
	       Str (Positive (I)) := Get_Character (Get_String_Char (S, I));
	    end loop;
            return Str;
	 end;
      end if;
   end Get_String;
   
   ------------------
   -- Enter_String --
   ------------------
   
   function Enter_String (S : String) return Str_Id is
   begin
      if S /= "" then
	 Start_String;
	 Store_String_Chars (S);
	 return End_String;
      else
	 return No_Str_Id;
      end if;
   end Enter_String;
   
   ----------------
   -- End_String --
   ----------------

   function End_String return Str_Id is
   begin
      return Strings.Last;
   end End_String;

   ---------------------
   -- Get_String_Char --
   ---------------------

   function Get_String_Char
     (Id    : Str_Id; 
      Index : Integer) return Char_Code is
   begin
      pragma Assert (Id in First_Str_Id .. Strings.Last
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

      --  Set up the null string

      Start_String;
      Null_Str_Id := End_String;
      
      True_String  := Enter_String ("true");
      False_String := Enter_String ("false");
   end Initialize;

   ----------
   -- Mark --
   ----------

   procedure Mark is
   begin
      Strings_Last := Strings.Last;
      String_Chars_Last := String_Chars.Last;
   end Mark;

   -------------
   -- Release --
   -------------

   procedure Release is
   begin
      Strings.Set_Last (Strings_Last);
      String_Chars.Set_Last (String_Chars_Last);
   end Release;

   ------------------
   -- Start_String --
   ------------------

   --  Version to start completely new string

   procedure Start_String is
   begin
      Strings.Append ((String_Index => String_Chars.Last + 1, Length => 0));
   end Start_String;

   --  Version to start from initially stored string
   
   --  Start a new string which begin with a copy of S. Is S was just entered 
   --  We can reuse the prefix for the new String.
   
   procedure Start_String (S : Str_Id) is
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
            String_Chars.Append
              (String_Chars.Table (Strings.Table (S).String_Index + (J - 1)));
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
      String_Chars.Append (C);
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

   procedure Store_String_Chars (S : Str_Id) is

      --  We are essentially doing this:

      --   for J in 1 .. String_Length (S) loop
      --      Store_String_Char (Get_String_Char (S, J));
      --   end loop;

      --  but when the string is long it's more efficient to grow the
      --  String_Chars table all at once.

      S_First  : constant Integer := Strings.Table (S).String_Index;
      S_Len    : constant Natural := String_Length (S);
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

   procedure Store_String_Int (N : Integer) is
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

   ------------------
   -- String_Equal --
   ------------------

   function String_Equal (L, R : Str_Id) return Boolean is
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

   -------------------
   -- String_Length --
   -------------------

   function String_Length (Id : Str_Id) return Natural is
   begin
      return Strings.Table (Id).Length;
   end String_Length;

   -------------------------
   -- Unstore_String_Char --
   -------------------------

   procedure Unstore_String_Char is
   begin
      String_Chars.Decrement_Last;
      Strings.Table (Strings.Last).Length :=
        Strings.Table (Strings.Last).Length - 1;
   end Unstore_String_Char;

end Artics.Strings_Stocks;
