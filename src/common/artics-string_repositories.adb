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

with Debug;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Artics.Semaphores;

--  The string repository use component from GNAT compiler especially Tables and
--  Namet components. Some string are initialized in the repository and their
--  ID is constant and well known:
--    0 : Not Found string
--    1 : Null string : this value is not saved in the repository
--         but Is_String, Get_String and Insert_String are doing
--         a quick hack to support it.

package body Artics.String_Repositories is

   Hash_Num : constant Integer := 2**12;
   --  Number of headers in the hash table. Current hash algorithm is closely
   --  tailored to this choice, so it can only be changed if a corresponding
   --  change is made to the hash alogorithm.

   Hash_Max : constant Integer := Hash_Num - 1;
   --  Indexes in the hash header table run from 0 to Hash_Num - 1

   subtype Hash_Index_Type is Integer range 0 .. Hash_Max;
   --  Range of hash index values

   Hash_Table : array (Hash_Index_Type) of String_Id;
   --  The hash table is used to locate existing entries in the names table.
   --  The entries point to the first names table entry whose hash value
   --  matches the hash code. Then subsequent names table entries with the
   --  same hash code value are linked through the Hash_Link fields.

   Sem : Artics.Semaphores.Semaphore(1);

   Offset_Upper_Lower : Integer := Character'Pos('a') - Character'Pos('A');

   Initialization_Done: Boolean := False;

   Local_Debug : Boolean := False;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Hash(Str: String) return Hash_Index_Type;
   pragma Inline (Hash);
   --  Compute hash code for name stored in Name_Buffer (length in Name_Len)

   function Is_String(Str: String;
                      Str_Hash: Hash_Index_Type) return String_Id;


   ----------
   -- Init --
   ----------
   procedure Init is
      Id: String_Id;
   begin
      if not Initialization_Done then
         String_Chars.Init;
         String_Entries.Init;
         Empty_Entries.Init;

         -- Id=0 => String not found
	 
         Id := Insert_String("Not Found");

         -- Id=1 => Null string
	 
         String_Entries.Increment_Last;
         Id := String_Entries.Last;
         String_Entries.Table(Id).String_Chars_Index := String_Chars.Last;
         String_Entries.Table(Id).String_Len := 0;
         String_Entries.Table(Id).Reference_Count := 1;

         -- This hash code was not used. Warning supposition that 0 hash is
	 --  still not used (should be the case)
	 
         Hash_Table(0) := Id;
         String_Entries.Table(Id).Hash_Link := Id;
         String_Chars.Increment_Last;
         String_Chars.Table (String_Chars.Last) := ASCII.NUL;
         Initialization_Done := True;
      end if;
   end Init;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Id : String_Id) return String is
      S : Character_Id;
   begin
      pragma Assert (Id in String_Entries.First .. String_Entries.Last);
      S := String_Entries.Table (Id).String_Chars_Index;
      declare
         R : String (1 .. Natural (String_Entries.Table (Id).String_Len));
      begin
         for J in R'Range loop
            R (J) := String_Chars.Table (S + Character_Id (J));
         end loop;

         return R;
      end;
   end Get_String;

   -----------------
   -- Get_Integer --
   -----------------

   function Get_Integer (Id : String_Id) return Integer is
      S     : Character_Id;
      Value : Integer;
      Last  : Positive;
      
      use Ada.Integer_Text_IO;
   begin
      pragma Assert (Id in String_Entries.First .. String_Entries.Last);
      
      S := String_Entries.Table (Id).String_Chars_Index;
      declare
         R : String (1 .. Natural (String_Entries.Table (Id).String_Len));
      begin
         for J in R'Range loop
            R (J) := String_Chars.Table (S + Character_Id (J));
         end loop;
         Get (R, Value, Last);
         return Value;
      end;
   end Get_Integer;

   ---------------
   -- Get_Float --
   ---------------

   function Get_Float (Id : String_Id) return Float is
      S     : Character_Id;
      Value : Float;
      Last  : Positive;
      
      use Ada.Float_Text_IO;
   begin
      pragma Assert (Id in String_Entries.First .. String_Entries.Last);
      
      S := String_Entries.Table (Id).String_Chars_Index;
      declare
         R : String (1 .. Natural (String_Entries.Table (Id).String_Len));
      begin
         for J in R'Range loop
            R (J) := String_Chars.Table (S + Character_Id (J));
         end loop;
         Get (R, Value, Last);
         return Value;
      end;
   end Get_Float;

   -----------------------
   -- Get_String_Length --
   -----------------------
   
   function Get_String_Length (Id : String_Id) return Integer is
   begin
      pragma Assert (Id in String_Entries.First .. String_Entries.Last);
      return String_Entries.Table (Id).String_Len;
   end Get_String_Length;

   -------------------
   -- Insert_String --
   -------------------
   
   function Insert_String (Str: String) return String_Id is
      
      Id       : String_Id := 0;
      Str_Hash : Hash_Index_Type;
   begin
      -- Quick hack for null string support
      
      if Str'Length = 0 then
         return 1;
      end if;

      Artics.Semaphores.P(Sem);

      Str_Hash := String_Repositories.Hash (Str);
      Id := Is_String (Str, Str_Hash);

      if Is_Not_Found (Id) then
	 
         -- Insert a new element in String_Entry
	 
         String_Entries.Increment_Last;
         Id := String_Entries.Last;
	 
         String_Entries.Table(Id).String_Chars_Index := String_Chars.Last;
         String_Entries.Table(Id).String_Len         := Str'Length;
         String_Entries.Table(Id).Reference_Count    := 1;
         
	 if Hash_Table(Str_Hash) = 0 then
	    
            -- This hash code was not used
            
	    Hash_Table (Str_Hash) := Id;
            String_Entries.Table(Id).Hash_Link := Id;
	    
         else
            -- This hash is already used, but the word was not found
            -- just insert the new one in the list
	    
            declare
               First_Entry  : String_Id := Hash_Table(Str_Hash);
               Second_Entry : String_Id :=
                 String_Entries.Table(First_Entry).Hash_Link;
            begin
               String_Entries.Table(First_Entry).Hash_Link := Id;
               String_Entries.Table(Id).Hash_Link := Second_Entry;
            end;
         end if;
	 
         -- Copy the string in the corresponding String_Chars
         
	 declare
            L: Integer := Str'Length;
            F: Integer := Str'First;
         begin
            for I in 1..L loop
               String_Chars.Increment_Last;
               String_Chars.Table (String_Chars.Last) := Str (F + I - 1);
            end loop;
	    
	    String_Chars.Increment_Last;
            String_Chars.Table (String_Chars.Last) := ASCII.NUL;
         end;
      end if;

      Artics.Semaphores.V(Sem);
      
      return Id;
      
   exception
      when others =>
         Artics.Semaphores.V(Sem);
         Dump;
         raise;
   end Insert_String;

   ---------------------
   -- Upper_character --
   ---------------------
   
   function Upper_Character (C: Character) return Character is
   begin
      if C in 'a'..'z' then
         return Character'Val( Character'Pos(C) - Offset_Upper_Lower);
      else
         return C;
      end if;
   end Upper_Character;
   pragma Inline(Upper_Character);

   ---------------
   -- Is_String --
   ---------------
   
   function Is_String
     (Str      : String;
      Str_Hash : Hash_Index_Type) return String_Id is
      
      Id: String_Id := 0;
   begin
      Id := Hash_Table (Str_Hash);

      if Id = 0 then
         return 0;
      end if;

      -- Search for the real string into the hashed values
      
      declare
         First_Id : String_Id := Id;
      begin
         loop
            declare
               Dif     : Boolean;
               Cmp_Str : String := Get_String (Id);
            begin
               Dif := True;
               if Str'Length = Cmp_Str'Length then
                  for I in 1..Str'Length loop
                     if Upper_Character(Cmp_Str(I + Cmp_Str'First -1)) /=
                        Upper_Character(Str(I + Str'First - 1)) then
                        Dif := False;
                     end if;
                  end loop;
               else
                  Dif := False;
               end if;
	       
               if Dif then
                  return Id;
               end if;
	       
               Id := String_Entries.Table(Id).Hash_Link;
               
	       if (First_Id = Id) then
                  return 0;
               else
		  null;
               end if;
            end;
         end loop;
      end;
      
   exception
      when others =>
         Dump;
         raise;
   end Is_String;

   ---------------
   -- Is_String --
   ---------------
   
   function Is_String(Str : String) return String_Id is
      
      Str_Hash: Hash_Index_Type;
   begin
      -- Quick hack for null string support
      
      if Str'Length = 0 then
         return 1;
      end if;

      -- Calculate the Hash for the String
      
      Str_Hash := String_Repositories.Hash(Str);
      
      return Is_String (Str, Str_Hash);
   end Is_String;

   ------------------
   -- Is_Not_Found --
   ------------------
   
   function Is_Not_Found (Id : String_Id) return Boolean is
   begin
      return Id = 0;
   end Is_Not_Found;

   --------------
   -- Is_Found --
   --------------
   
   function Is_Found (Id : String_Id) return Boolean is
   begin
      return Id /= 0;
   end Is_Found;

   ----------
   -- Dump --
   ----------
   
   procedure Dump is
      --use Ada.Text_IO;
      --use My_Integer_IO;
   begin
      null;
   end Dump;

   ----------
   -- Hash --
   ----------

   function Hash (Str : String) return Hash_Index_Type is
      
      subtype Int_1_12 is Integer range 1 .. 12;
      --  Used to avoid when others on case jump below

      Even_String_Len : Integer;
      Str_Len         : Integer := Str'Length;
      F               : Integer := Str'First - 1;
      --  Last even numbered position (used for >12 case)

   begin
      --  Special test for 12 (rather than counting on a when others for the
      --  case statement below) avoids some Ada compilers converting the case
      --  statement into successive jumps.

      --  The case of a name longer than 12 characters is handled by taking
      --  the first 6 odd numbered characters and the last 6 even numbered
      --  characters

      if Str_Len > 12 then
         Even_String_Len := (Str_Len) / 2 * 2;

         return ((((((((((((
           Character'Pos (Upper_Character(Str (F + 01)))) * 2 +
           Character'Pos (Upper_Character(Str (F + Even_String_Len - 10)))) * 2 +
           Character'Pos (Upper_Character(Str (F + 03)))) * 2 +
           Character'Pos (Upper_Character(Str (F + Even_String_Len - 08)))) * 2 +
           Character'Pos (Upper_Character(Str (F + 05)))) * 2 +
           Character'Pos (Upper_Character(Str (F + Even_String_Len - 06)))) * 2 +
           Character'Pos (Upper_Character(Str (F + 07)))) * 2 +
           Character'Pos (Upper_Character(Str (F + Even_String_Len - 04)))) * 2 +
           Character'Pos (Upper_Character(Str (F + 09)))) * 2 +
           Character'Pos (Upper_Character(Str (F + Even_String_Len - 02)))) * 2 +
           Character'Pos (Upper_Character(Str (F + 11)))) * 2 +
           Character'Pos (Upper_Character(Str (F + Even_String_Len)))) mod Hash_Num;
      end if;

      --  For the cases of 1-12 characters, all characters participate in the
      --  hash. The positioning is randomized, with the bias that characters
      --  later on participate fully (i.e. are added towards the right side).

      case Int_1_12 (Str_Len) is

         when 1 =>
            return
               Character'Pos (Upper_Character(Str (F + 1)));
         when 2 =>
            return ((
              Character'Pos (Upper_Character(Str (F + 1)))) * 64 +
              Character'Pos (Upper_Character(Str (F + 2)))) mod Hash_Num;

         when 3 =>
            return (((
              Character'Pos (Upper_Character(Str (F + 1)))) * 16 +
              Character'Pos (Upper_Character(Str (F + 3)))) * 16 +
              Character'Pos (Upper_Character(Str (F + 2)))) mod Hash_Num;

         when 4 =>
            return ((((
              Character'Pos (Upper_Character(Str (F + 1)))) * 8 +
              Character'Pos (Upper_Character(Str (F + 2)))) * 8 +
              Character'Pos (Upper_Character(Str (F + 3)))) * 8 +
              Character'Pos (Upper_Character(Str (F + 4)))) mod Hash_Num;

         when 5 =>
            return (((((
              Character'Pos (Upper_Character(Str (F + 4)))) * 8 +
              Character'Pos (Upper_Character(Str (F + 1)))) * 4 +
              Character'Pos (Upper_Character(Str (F + 3)))) * 4 +
              Character'Pos (Upper_Character(Str (F + 5)))) * 8 +
              Character'Pos (Upper_Character(Str (F + 2)))) mod Hash_Num;

         when 6 =>
            return ((((((
              Character'Pos (Upper_Character(Str (F + 5)))) * 4 +
              Character'Pos (Upper_Character(Str (F + 1)))) * 4 +
              Character'Pos (Upper_Character(Str (F + 4)))) * 4 +
              Character'Pos (Upper_Character(Str (F + 2)))) * 4 +
              Character'Pos (Upper_Character(Str (F + 6)))) * 4 +
              Character'Pos (Upper_Character(Str (F + 3)))) mod Hash_Num;

         when 7 =>
            return (((((((
              Character'Pos (Upper_Character(Str (F + 4)))) * 4 +
              Character'Pos (Upper_Character(Str (F + 3)))) * 4 +
              Character'Pos (Upper_Character(Str (F + 1)))) * 4 +
              Character'Pos (Upper_Character(Str (F + 2)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 5)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 7)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 6)))) mod Hash_Num;

         when 8 =>
            return ((((((((
              Character'Pos (Upper_Character(Str (F + 2)))) * 4 +
              Character'Pos (Upper_Character(Str (F + 1)))) * 4 +
              Character'Pos (Upper_Character(Str (F + 3)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 5)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 7)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 6)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 4)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 8)))) mod Hash_Num;

         when 9 =>
            return (((((((((
              Character'Pos (Upper_Character(Str (F + 2)))) * 4 +
              Character'Pos (Upper_Character(Str (F + 1)))) * 4 +
              Character'Pos (Upper_Character(Str (F + 3)))) * 4 +
              Character'Pos (Upper_Character(Str (F + 4)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 8)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 7)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 5)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 6)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 9)))) mod Hash_Num;

         when 10 =>
            return ((((((((((
              Character'Pos (Upper_Character(Str (F + 01)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 02)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 08)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 03)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 04)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 09)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 06)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 05)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 07)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 10)))) mod Hash_Num;

         when 11 =>
            return (((((((((((
              Character'Pos (Upper_Character(Str (F + 05)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 01)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 06)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 09)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 07)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 03)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 08)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 02)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 10)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 04)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 11)))) mod Hash_Num;

         when 12 =>
            return ((((((((((((
              Character'Pos (Upper_Character(Str (F + 03)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 02)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 05)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 01)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 06)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 04)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 08)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 11)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 07)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 09)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 10)))) * 2 +
              Character'Pos (Upper_Character(Str (F + 12)))) mod Hash_Num;

      end case;
   end Hash;

   ------------------------
   -- Debug of String_Id --
   ------------------------
   
   function To_String (Id : String_Id) return String is
      
      use Ada.Integer_Text_IO;
      Str: String (1..10);
   begin
      Put(Str, Integer(Id));
      return Str;
   end To_String;

   function To_Integer (Id : in String_Id) return Integer is
      
      function Convert is
	 new Ada.Unchecked_Conversion(String _Id, Integer);
   begin
      return Convert (Id);
   end To_Integer;

end Artics.String_Repositories;
