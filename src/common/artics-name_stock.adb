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
-- Reflex is originally developed by Artics
------------------------------------------------------------------------------

--  WARNING: There is a C version of this package. Any changes to this
--  source file must be properly reflected in the C header file namet.h
--  which is created manually from namet.ads and namet.adb.

with Artics.Widechar; use Artics.Widechar;
with Interfaces; use Interfaces;

package body Artics.Name_Stock is

   Name_Chars_Reserve   : constant := 5000;
   Name_Entries_Reserve : constant := 100;
   --  The names table is locked during gigi processing, since gigi assumes
   --  that the table does not move. After returning from gigi, the names
   --  table is unlocked again, since writing library file information needs
   --  to generate some extra names. To avoid the inefficiency of always
   --  reallocating during this second unlocked phase, we reserve a bit of
   --  extra space before doing the release call.

   Hash_Num : constant Int := 2**16;
   --  Number of headers in the hash table. Current hash algorithm is closely
   --  tailored to this choice, so it can only be changed if a corresponding
   --  change is made to the hash algorithm.

   Hash_Max : constant Int := Hash_Num - 1;
   --  Indexes in the hash header table run from 0 to Hash_Num - 1

   subtype Hash_Index_Type is Int range 0 .. Hash_Max;
   --  Range of hash index values

   Hash_Table : array (Hash_Index_Type) of Name_Id;
   --  The hash table is used to locate existing entries in the names table.
   --  The entries point to the first names table entry whose hash value
   --  matches the hash code. Then subsequent names table entries with the
   --  same hash code value are linked through the Hash_Link fields.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Hash return Hash_Index_Type;
   pragma Inline (Hash);
   --  Compute hash code for name stored in Name_Buffer (length in Name_Len)

   procedure Strip_Qualification_And_Suffixes;
   --  Given an encoded entity name in Name_Buffer, remove package body
   --  suffix as described for Strip_Package_Body_Suffix, and also remove
   --  all qualification, i.e. names followed by two underscores. The
   --  contents of Name_Buffer is modified by this call, and on return
   --  Name_Buffer and Name_Len reflect the stripped name.

   -----------------------------
   -- Add_Char_To_Name_Buffer --
   -----------------------------

   procedure Add_Char_To_Name_Buffer (C : Character) is
   begin
      if Name_Len < Name_Buffer'Last then
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := C;
      end if;
   end Add_Char_To_Name_Buffer;

   ----------------------------
   -- Add_Nat_To_Name_Buffer --
   ----------------------------

   procedure Add_Nat_To_Name_Buffer (V : Nat) is
   begin
      if V >= 10 then
         Add_Nat_To_Name_Buffer (V / 10);
      end if;

      Add_Char_To_Name_Buffer (Character'Val (Character'Pos ('0') + V rem 10));
   end Add_Nat_To_Name_Buffer;

   ----------------------------
   -- Add_Str_To_Name_Buffer --
   ----------------------------

   procedure Add_Str_To_Name_Buffer (S : String) is
   begin
      for J in S'Range loop
         Add_Char_To_Name_Buffer (S (J));
      end loop;
   end Add_Str_To_Name_Buffer;

   --------------------
   -- Add_New_String --
   --------------------

   procedure Add_New_String (S : String) is
   begin
      if S = "" then
         return;
      end if;

      Name_Len := Name_Buffer'First - 1;

      Add_Str_To_Name_Buffer (S);
   end Add_New_String;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      null;
   end Finalize;

   -----------------------------
   -- Get_Decoded_Name_String --
   -----------------------------

   procedure Get_Decoded_Name_String (Id : Name_Id) is
      C : Character;
      P : Natural;

   begin
      Get_Name_String (Id);

      --  Skip scan if we already know there are no encodings

      if Name_Entries.Table (Id).Name_Has_No_Encodings then
         return;
      end if;

      --  Quick loop to see if there is anything special to do

      P := 1;
      loop
         if P = Name_Len then
            Name_Entries.Table (Id).Name_Has_No_Encodings := True;
            return;

         else
            C := Name_Buffer (P);

            exit when
              C = 'U' or else
              C = 'W' or else
              C = 'Q' or else
              C = 'O';

            P := P + 1;
         end if;
      end loop;

      --  Here we have at least some encoding that we must decode

      Decode : declare
         New_Len : Natural;
         Old     : Positive;
         New_Buf : String (1 .. Name_Buffer'Last);

         procedure Copy_One_Character;
         --  Copy a character from Name_Buffer to New_Buf. Includes case
         --  of copying a Uhh,Whhhh,WWhhhhhhhh sequence and decoding it.

         function Hex (N : Natural) return Word;
         --  Scans past N digits using Old pointer and returns hex value

         procedure Insert_Character (C : Character);
         --  Insert a new character into output decoded name

         ------------------------
         -- Copy_One_Character --
         ------------------------

         procedure Copy_One_Character is
            C : Character;

         begin
            C := Name_Buffer (Old);

            --  U (upper half insertion case)

            if C = 'U'
              and then Old < Name_Len
              and then Name_Buffer (Old + 1) not in 'A' .. 'Z'
              and then Name_Buffer (Old + 1) /= '_'
            then
               Old := Old + 1;

               --  If we have upper half encoding, then we have to set an
               --  appropriate wide character sequence for this character.

               if Upper_Half_Encoding then
                  Widechar.Set_Wide (Char_Code (Hex (2)), New_Buf, New_Len);

                  --  For other encoding methods, upper half characters can
                  --  simply use their normal representation.

               else
                  Insert_Character (Character'Val (Hex (2)));
               end if;

            --  WW (wide wide character insertion)

            elsif C = 'W'
              and then Old < Name_Len
              and then Name_Buffer (Old + 1) = 'W'
            then
               Old := Old + 2;
               Widechar.Set_Wide (Char_Code (Hex (8)), New_Buf, New_Len);

            --  W (wide character insertion)

            elsif C = 'W'
              and then Old < Name_Len
              and then Name_Buffer (Old + 1) not in 'A' .. 'Z'
              and then Name_Buffer (Old + 1) /= '_'
            then
               Old := Old + 1;
               Widechar.Set_Wide (Char_Code (Hex (4)), New_Buf, New_Len);

            --  Any other character is copied unchanged

            else
               Insert_Character (C);
               Old := Old + 1;
            end if;
         end Copy_One_Character;

         ---------
         -- Hex --
         ---------

         function Hex (N : Natural) return Word is
            T : Word := 0;
            C : Character;

         begin
            for J in 1 .. N loop
               C := Name_Buffer (Old);
               Old := Old + 1;

               pragma Assert (C in '0' .. '9' or else C in 'a' .. 'f');

               if C <= '9' then
                  T := 16 * T + Character'Pos (C) - Character'Pos ('0');
               else -- C in 'a' .. 'f'
                  T := 16 * T + Character'Pos (C) - (Character'Pos ('a') - 10);
               end if;
            end loop;

            return T;
         end Hex;

         ----------------------
         -- Insert_Character --
         ----------------------

         procedure Insert_Character (C : Character) is
         begin
            New_Len := New_Len + 1;
            New_Buf (New_Len) := C;
         end Insert_Character;

      --  Start of processing for Decode

      begin
         New_Len := 0;
         Old := 1;

         --  Loop through characters of name

         while Old <= Name_Len loop

            --  Case of character literal, put apostrophes around character

            if Name_Buffer (Old) = 'Q'
              and then Old < Name_Len
            then
               Old := Old + 1;
               Insert_Character (''');
               Copy_One_Character;
               Insert_Character (''');

            --  Case of operator name

            elsif Name_Buffer (Old) = 'O'
              and then Old < Name_Len
              and then Name_Buffer (Old + 1) not in 'A' .. 'Z'
              and then Name_Buffer (Old + 1) /= '_'
            then
               Old := Old + 1;

               declare
                  --  This table maps the 2nd and 3rd characters of the name
                  --  into the required output. Two blanks means leave the
                  --  name alone

                  Map : constant String :=
                     "ab  " &               --  Oabs         => "abs"
                     "ad+ " &               --  Oadd         => "+"
                     "an  " &               --  Oand         => "and"
                     "co& " &               --  Oconcat      => "&"
                     "di/ " &               --  Odivide      => "/"
                     "eq= " &               --  Oeq          => "="
                     "ex**" &               --  Oexpon       => "**"
                     "gt> " &               --  Ogt          => ">"
                     "ge>=" &               --  Oge          => ">="
                     "le<=" &               --  Ole          => "<="
                     "lt< " &               --  Olt          => "<"
                     "mo  " &               --  Omod         => "mod"
                     "mu* " &               --  Omutliply    => "*"
                     "ne/=" &               --  One          => "/="
                     "no  " &               --  Onot         => "not"
                     "or  " &               --  Oor          => "or"
                     "re  " &               --  Orem         => "rem"
                     "su- " &               --  Osubtract    => "-"
                     "xo  ";                --  Oxor         => "xor"

                  J : Integer;

               begin
                  Insert_Character ('"');

                  --  Search the map. Note that this loop must terminate, if
                  --  not we have some kind of internal error, and a constraint
                  --  error may be raised.

                  J := Map'First;
                  loop
                     exit when Name_Buffer (Old) = Map (J)
                       and then Name_Buffer (Old + 1) = Map (J + 1);
                     J := J + 4;
                  end loop;

                  --  Special operator name

                  if Map (J + 2) /= ' ' then
                     Insert_Character (Map (J + 2));

                     if Map (J + 3) /= ' ' then
                        Insert_Character (Map (J + 3));
                     end if;

                     Insert_Character ('"');

                     --  Skip past original operator name in input

                     while Old <= Name_Len
                       and then Name_Buffer (Old) in 'a' .. 'z'
                     loop
                        Old := Old + 1;
                     end loop;

                  --  For other operator names, leave them in lower case,
                  --  surrounded by apostrophes

                  else
                     --  Copy original operator name from input to output

                     while Old <= Name_Len
                        and then Name_Buffer (Old) in 'a' .. 'z'
                     loop
                        Copy_One_Character;
                     end loop;

                     Insert_Character ('"');
                  end if;
               end;

            --  Else copy one character and keep going

            else
               Copy_One_Character;
            end if;
         end loop;

         --  Copy new buffer as result

         Name_Len := New_Len;
         Name_Buffer (1 .. New_Len) := New_Buf (1 .. New_Len);
      end Decode;
   end Get_Decoded_Name_String;

   -------------------------------------------
   -- Get_Decoded_Name_String_With_Brackets --
   -------------------------------------------

   procedure Get_Decoded_Name_String_With_Brackets (Id : Name_Id) is
      P : Natural;

   begin
      --  Case of operator name, normal decoding is fine

      if Name_Buffer (1) = 'O' then
         Get_Decoded_Name_String (Id);

      --  For character literals, normal decoding is fine

      elsif Name_Buffer (1) = 'Q' then
         Get_Decoded_Name_String (Id);

      --  Only remaining issue is U/W/WW sequences

      else
         Get_Name_String (Id);

         P := 1;
         while P < Name_Len loop
            if Name_Buffer (P + 1) in 'A' .. 'Z' then
               P := P + 1;

            --  Uhh encoding

            elsif Name_Buffer (P) = 'U' then
               for J in reverse P + 3 .. P + Name_Len loop
                  Name_Buffer (J + 3) := Name_Buffer (J);
               end loop;

               Name_Len := Name_Len + 3;
               Name_Buffer (P + 3) := Name_Buffer (P + 2);
               Name_Buffer (P + 2) := Name_Buffer (P + 1);
               Name_Buffer (P)     := '[';
               Name_Buffer (P + 1) := '"';
               Name_Buffer (P + 4) := '"';
               Name_Buffer (P + 5) := ']';
               P := P + 6;

            --  WWhhhhhhhh encoding

            elsif Name_Buffer (P) = 'W'
              and then P + 9 <= Name_Len
              and then Name_Buffer (P + 1) = 'W'
              and then Name_Buffer (P + 2) not in 'A' .. 'Z'
              and then Name_Buffer (P + 2) /= '_'
            then
               Name_Buffer (P + 12 .. Name_Len + 2) :=
                 Name_Buffer (P + 10 .. Name_Len);
               Name_Buffer (P)     := '[';
               Name_Buffer (P + 1) := '"';
               Name_Buffer (P + 10) := '"';
               Name_Buffer (P + 11) := ']';
               Name_Len := Name_Len + 2;
               P := P + 12;

            --  Whhhh encoding

            elsif Name_Buffer (P) = 'W'
              and then P < Name_Len
              and then Name_Buffer (P + 1) not in 'A' .. 'Z'
              and then Name_Buffer (P + 1) /= '_'
            then
               Name_Buffer (P + 8 .. P + Name_Len + 3) :=
                 Name_Buffer (P + 5 .. Name_Len);
               Name_Buffer (P + 2 .. P + 5) := Name_Buffer (P + 1 .. P + 4);
               Name_Buffer (P)     := '[';
               Name_Buffer (P + 1) := '"';
               Name_Buffer (P + 6) := '"';
               Name_Buffer (P + 7) := ']';
               Name_Len := Name_Len + 3;
               P := P + 8;

            else
               P := P + 1;
            end if;
         end loop;
      end if;
   end Get_Decoded_Name_String_With_Brackets;

   ------------------------
   -- Get_Last_Two_Chars --
   ------------------------

   procedure Get_Last_Two_Chars (N : Name_Id; C1, C2 : out Character) is
      NE  : Name_Entry renames Name_Entries.Table (N);
      NEL : constant Int := Int (NE.Name_Len);

   begin
      if NEL >= 2 then
         C1 := Name_Chars.Table (NE.Name_Chars_Index + NEL - 1);
         C2 := Name_Chars.Table (NE.Name_Chars_Index + NEL - 0);
      else
         C1 := ASCII.NUL;
         C2 := ASCII.NUL;
      end if;
   end Get_Last_Two_Chars;

   ---------------------
   -- Get_Name_String --
   ---------------------

   --  Procedure version leaving result in Name_Buffer, length in Name_Len

   procedure Get_Name_String (Id : Name_Id) is
      S : Int;

   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);

      S := Name_Entries.Table (Id).Name_Chars_Index;
      Name_Len := Natural (Name_Entries.Table (Id).Name_Len);

      for J in 1 .. Name_Len loop
         Name_Buffer (J) := Name_Chars.Table (S + Int (J));
      end loop;
   end Get_Name_String;

   ---------------------
   -- Get_Name_String --
   ---------------------

   --  Function version returning a string

   function Get_Name_String (Id : Name_Id) return String is
      S : Int;

   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      S := Name_Entries.Table (Id).Name_Chars_Index;

      declare
         R : String (1 .. Natural (Name_Entries.Table (Id).Name_Len));

      begin
         for J in R'Range loop
            R (J) := Name_Chars.Table (S + Int (J));
         end loop;

         return R;
      end;
   end Get_Name_String;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Id : Name_Id) return String is
   begin

      if Id = No_Name then
         return "";
      end if;

--      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);

      Get_Name_String (Id);

      return Name_Buffer (1..Name_Len);
   end Get_String;

   --------------------------------
   -- Get_Name_String_And_Append --
   --------------------------------

   procedure Get_Name_String_And_Append (Id : Name_Id) is
      S : Int;

   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);

      S := Name_Entries.Table (Id).Name_Chars_Index;

      for J in 1 .. Natural (Name_Entries.Table (Id).Name_Len) loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Name_Chars.Table (S + Int (J));
      end loop;
   end Get_Name_String_And_Append;

   -------------------------
   -- Get_Name_Table_Byte --
   -------------------------

   function Get_Name_Table_Byte (Id : Name_Id) return Byte is
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      return Name_Entries.Table (Id).Byte_Info;
   end Get_Name_Table_Byte;

  ------------------------
   -- Get_Name_Table_Int --
   ------------------------

   function Get_Name_Table_Int (Id : Name_Id) return Integer is
   begin
      return Name_Entries.Table (Id).User_Int;
   end Get_Name_Table_Int;

   ------------------------
   -- Set_Name_Table_Int --
   ------------------------

   procedure Set_Name_Table_Int
     (Id : Name_Id;
      V  : Integer) is
   begin
      Name_Entries.Table (Id).User_Int := V;
   end Set_Name_Table_Int;

   -------------------------
   -- Get_Name_Table_Info --
   -------------------------

   function Get_Name_Table_Info (Id : Name_Id) return Int is
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      return Name_Entries.Table (Id).Int_Info;
   end Get_Name_Table_Info;

   --------------------------
   -- Get_Name_Table_Info2 --
   --------------------------

   function Get_Name_Table_Info2 (Id : Name_Id) return Int is
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      return Name_Entries.Table (Id).Int2_Info;
   end Get_Name_Table_Info2;

   -----------------------------------------
   -- Get_Unqualified_Decoded_Name_String --
   -----------------------------------------

   procedure Get_Unqualified_Decoded_Name_String (Id : Name_Id) is
   begin
      Get_Decoded_Name_String (Id);
      Strip_Qualification_And_Suffixes;
   end Get_Unqualified_Decoded_Name_String;

   ---------------------------------
   -- Get_Unqualified_Name_String --
   ---------------------------------

   procedure Get_Unqualified_Name_String (Id : Name_Id) is
   begin
      Get_Name_String (Id);
      Strip_Qualification_And_Suffixes;
   end Get_Unqualified_Name_String;

   ----------
   -- Hash --
   ----------

   function Hash return Hash_Index_Type is

      --  This hash function looks at every character, in order to make it
      --  likely that similar strings get different hash values. The rotate by
      --  7 bits has been determined empirically to be good, and it doesn't
      --  lose bits like a shift would. The final conversion can't overflow,
      --  because the table is 2**16 in size. This function probably needs to
      --  be changed if the hash table size is changed.

      --  Note that we could get some speed improvement by aligning the string
      --  to 32 or 64 bits, and doing word-wise xor's. We could also implement
      --  a growable table. It doesn't seem worth the trouble to do those
      --  things, for now.

      Result : Unsigned_16 := 0;

   begin
      for J in 1 .. Name_Len loop
         Result := Rotate_Left (Result, 7) xor Character'Pos (Name_Buffer (J));
      end loop;

      return Hash_Index_Type (Result);
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if Initialized then
         --Out_Line ("Already initialized !");
         return;
      end if;
      Name_Chars.Init;
      Name_Entries.Init;

      --Name_Entries.Increment_Last;  -- No Node
      --Name_Entries.Increment_Last;  -- Error Node
      
      --  Initialize entries for one character names
      for C in Character loop
         Name_Entries.Increment_Last;
         Name_Entries.Table (Name_Entries.Last).Name_Chars_Index :=
           Name_Chars.Last;
         Name_Entries.Table (Name_Entries.Last).Name_Len  := 1;
         Name_Entries.Table (Name_Entries.Last).Hash_Link := No_Name;
         Name_Entries.Table (Name_Entries.Last).User      := User_Default;
         Name_Entries.Table (Name_Entries.Last).User_int  := 0;
         Name_Chars.Increment_Last;
         Name_Chars.Table (Name_Chars.Last) := C;
         Name_Chars.Increment_Last;
         Name_Chars.Table (Name_Chars.Last) := Ascii.NUL;
      end loop;

      --  Clear hash table
      for J in Hash_Index_Type loop
         Hash_Table (J) := No_Name;
      end loop;
      Initialized := True;
   end Initialize;

   -------------------------------
   -- Insert_Str_In_Name_Buffer --
   -------------------------------

   procedure Insert_Str_In_Name_Buffer (S : String; Index : Positive) is
      SL : constant Natural := S'Length;
   begin
      Name_Buffer (Index + SL .. Name_Len + SL) :=
        Name_Buffer (Index .. Name_Len);
      Name_Buffer (Index .. Index + SL - 1) := S;
      Name_Len := Name_Len + SL;
   end Insert_Str_In_Name_Buffer;

   ----------------------
   -- Is_Internal_Name --
   ----------------------

   --  Version taking an argument

   function Is_Internal_Name (Id : Name_Id) return Boolean is
   begin
      Get_Name_String (Id);
      return Is_Internal_Name;
   end Is_Internal_Name;

   ----------------------
   -- Is_Internal_Name --
   ----------------------

   --  Version taking its input from Name_Buffer

   function Is_Internal_Name return Boolean is
   begin
      if Name_Buffer (1) = '_'
        or else Name_Buffer (Name_Len) = '_'
      then
         return True;

      else
         --  Test backwards, because we only want to test the last entity
         --  name if the name we have is qualified with other entities.

         for J in reverse 1 .. Name_Len loop
            if Is_OK_Internal_Letter (Name_Buffer (J)) then
               return True;

            --  Quit if we come to terminating double underscore (note that
            --  if the current character is an underscore, we know that
            --  there is a previous character present, since we already
            --  filtered out the case of Name_Buffer (1) = '_' above.

            elsif Name_Buffer (J) = '_'
              and then Name_Buffer (J - 1) = '_'
              and then Name_Buffer (J - 2) /= '_'
            then
               return False;
            end if;
         end loop;
      end if;

      return False;
   end Is_Internal_Name;

   ---------------------------
   -- Is_OK_Internal_Letter --
   ---------------------------

   function Is_OK_Internal_Letter (C : Character) return Boolean is
   begin
      return C in 'A' .. 'Z'
        and then C /= 'O'
        and then C /= 'Q'
        and then C /= 'U'
        and then C /= 'W';
        --  and then C /= 'X';
   end Is_OK_Internal_Letter;

   ----------------------
   -- Is_Operator_Name --
   ----------------------

   function Is_Operator_Name (Id : Name_Id) return Boolean is
      S : Int;
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      S := Name_Entries.Table (Id).Name_Chars_Index;
      return Name_Chars.Table (S + 1) = 'O';
   end Is_Operator_Name;

   -------------------
   -- Is_Valid_Name --
   -------------------

   function Is_Valid_Name (Id : Name_Id) return Boolean is
   begin
      return Id in Name_Entries.First .. Name_Entries.Last;
   end Is_Valid_Name;

   --------------------
   -- Length_Of_Name --
   --------------------

   function Length_Of_Name (Id : Name_Id) return Nat is
   begin
      return Int (Name_Entries.Table (Id).Name_Len);
   end Length_Of_Name;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      Name_Chars.Set_Last (Name_Chars.Last + Name_Chars_Reserve);
      Name_Entries.Set_Last (Name_Entries.Last + Name_Entries_Reserve);
      Name_Chars.Locked := True;
      Name_Entries.Locked := True;
      Name_Chars.Release;
      Name_Entries.Release;
   end Lock;

   ------------------------
   -- Name_Chars_Address --
   ------------------------

   function Name_Chars_Address return System.Address is
   begin
      return Name_Chars.Table (0)'Address;
   end Name_Chars_Address;

   ----------------
   -- Name_Enter --
   ----------------

   function Name_Enter return Name_Id is
   begin
      Name_Entries.Append
        ((Name_Chars_Index      => Name_Chars.Last,
          Name_Len              => Short (Name_Len),
          Byte_Info             => 0,
          Int_Info              => 0,
          Int2_Info             => 0,
          Name_Has_No_Encodings => False,
          Hash_Link             => No_Name,
          User                  => User_Default,
          User_Int              => 0));

      --  Set corresponding string entry in the Name_Chars table

      for J in 1 .. Name_Len loop
         Name_Chars.Append (Name_Buffer (J));
      end loop;

      Name_Chars.Append (ASCII.NUL);

      return Name_Entries.Last;
   end Name_Enter;

   ------------------
   -- String_Enter --
   ------------------

   function String_Enter (S : String) return Name_Id is
   begin
      Add_New_String (S);
      return Name_Enter;
   end String_Enter;

   --------------------------------------------------
   -- Name_Look_Up                                 --
   --  Recherche la chaine placee dans Name_Buffer --
   --  Si on la trouve retourne l'Id correspondant --
   --  Sinon retourn No_Name                       --
   --------------------------------------------------

   function Name_Look_Up return Name_Id is

      New_Id : Name_Id;
      --  Id of entry in hash search, and value to be returned

      S : Int;
      --  Pointer into string table

      Hash_Index : Hash_Index_Type;
      --  Computed hash index

   begin
      --  Quick handling for one character names

      if Name_Len = 1 then
         return Name_Id (First_Name_Id + Character'Pos (Name_Buffer (1)));

         --  Otherwise search hash table for existing matching entry

      else
         Hash_Index := Name_Stock.Hash;
         New_Id     := Hash_Table (Hash_Index);

         if New_Id /= No_Name then

        Search :
            loop
               if Name_Len /= Integer (Name_Entries.Table (New_Id).Name_Len)
               then
                  goto No_Match;
               end if;

               S := Name_Entries.Table (New_Id).Name_Chars_Index;

               for I in 1 .. Name_Len loop
                  if Name_Chars.Table (S + Int (I)) /= Name_Buffer (I) then
                     goto No_Match;
                  end if;
               end loop;

               -- We found the name
               return New_Id;

               --  Current entry in hash chain does not match
               <<No_Match>>
                 if Name_Entries.Table (New_Id).Hash_Link /= No_Name then
                 New_Id := Name_Entries.Table (New_Id).Hash_Link;
                 else
                    exit Search;
                 end if;

            end loop Search;
         end if;

         --  We fall through here only if a matching entry was not found in the
         --  hash table.

         return No_Name;
      end if;
   end Name_Look_Up;

   --------------------
   -- String_Look_Up --
   --------------------

   function String_Look_Up (S : String) return Name_Id is
   begin
       Add_New_String (S);
       return Name_Look_Up;
   end String_Look_Up;

   --------------------------
   -- Name_Entries_Address --
   --------------------------

   function Name_Entries_Address return System.Address is
   begin
      return Name_Entries.Table (First_Name_Id)'Address;
   end Name_Entries_Address;

   ------------------------
   -- Name_Entries_Count --
   ------------------------

   function Name_Entries_Count return Nat is
   begin
      return Int (Name_Entries.Last - Name_Entries.First + 1);
   end Name_Entries_Count;

   ---------------
   -- Name_Find --
   ---------------

   function Name_Find return Name_Id is
      New_Id : Name_Id;
      --  Id of entry in hash search, and value to be returned

      S : Int;
      --  Pointer into string table

      Hash_Index : Hash_Index_Type;
      --  Computed hash index

   begin
      --  Quick handling for one character names

      if Name_Len = 1 then
         return Name_Id
	   (First_Name_Id + Character'Pos (Name_Buffer (1)));

      --  Otherwise search hash table for existing matching entry

      else
         Hash_Index := Hash;
         New_Id := Hash_Table (Hash_Index);

         if New_Id = No_Name then
            Hash_Table (Hash_Index) := Name_Entries.Last + 1;

         else
            Search : loop
               if Name_Len /=
                 Integer (Name_Entries.Table (New_Id).Name_Len)
               then
                  goto No_Match;
               end if;

               S := Name_Entries.Table (New_Id).Name_Chars_Index;

               for J in 1 .. Name_Len loop
                  if Name_Chars.Table (S + Int (J)) /= Name_Buffer (J) then
                     goto No_Match;
                  end if;
               end loop;

               return New_Id;

               --  Current entry in hash chain does not match

               <<No_Match>>
                  if Name_Entries.Table (New_Id).Hash_Link /= No_Name then
                     New_Id := Name_Entries.Table (New_Id).Hash_Link;
                  else
                     Name_Entries.Table (New_Id).Hash_Link :=
                       Name_Entries.Last + 1;
                     exit Search;
                  end if;
            end loop Search;
         end if;

         --  We fall through here only if a matching entry was not found in the
         --  hash table. We now create a new entry in the names table. The hash
         --  link pointing to the new entry (Name_Entries.Last+1) has been set.

         Name_Entries.Append
           ((Name_Chars_Index      => Name_Chars.Last,
             Name_Len              => Short (Name_Len),
             Hash_Link             => No_Name,
             Name_Has_No_Encodings => False,
             Int_Info              => 0,
             Int2_Info             => 0,
             Byte_Info             => 0,
             User                  => User_Default,
             User_Int              => 0));

         --  Set corresponding string entry in the Name_Chars table

         for J in 1 .. Name_Len loop
            Name_Chars.Append (Name_Buffer (J));
         end loop;

         Name_Chars.Append (ASCII.NUL);

         return Name_Entries.Last;
      end if;
   end Name_Find;

   ------------------
   -- Reinitialize --
   ------------------

   procedure Reinitialize is
   begin
      Name_Chars.Init;
      Name_Entries.Init;

      --  Initialize entries for one character names

      for C in Character loop
         Name_Entries.Append
           ((Name_Chars_Index      => Name_Chars.Last,
             Name_Len              => 1,
             Byte_Info             => 0,
             Int_Info              => 0,
             Int2_Info             => 0,
             Name_Has_No_Encodings => True,
             Hash_Link             => No_Name,
             User                  => User_Default,
             User_Int              => 0));


         Name_Chars.Append (C);
         Name_Chars.Append (ASCII.NUL);
      end loop;

      --  Clear hash table

      for J in Hash_Index_Type loop
         Hash_Table (J) := No_Name;
      end loop;
   end Reinitialize;

   ----------------------
   -- Reset_Name_Table --
   ----------------------

   procedure Reset_Name_Table is
   begin
      for J in First_Name_Id .. Name_Entries.Last loop
         Name_Entries.Table (J).Int2_Info := 0;
         Name_Entries.Table (J).Int_Info  := 0;
         Name_Entries.Table (J).Byte_Info := 0;
      end loop;
   end Reset_Name_Table;

   --------------------------------
   -- Set_Character_Literal_Name --
   --------------------------------

   procedure Set_Character_Literal_Name (C : Char_Code) is
   begin
      Name_Buffer (1) := 'Q';
      Name_Len := 1;
      Store_Encoded_Character (C);
   end Set_Character_Literal_Name;

   -------------------------
   -- Set_Name_Table_Byte --
   -------------------------

   procedure Set_Name_Table_Byte (Id : Name_Id; Val : Byte) is
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      Name_Entries.Table (Id).Byte_Info := Val;
   end Set_Name_Table_Byte;

   -------------------------
   -- Set_Name_Table_Info --
   -------------------------

   procedure Set_Name_Table_Info (Id : Name_Id; Val : Int) is
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      Name_Entries.Table (Id).Int_Info := Val;
   end Set_Name_Table_Info;

   --------------------------
   -- Set_Name_Table_Info2 --
   --------------------------

   procedure Set_Name_Table_Info2 (Id : Name_Id; Val : Int) is
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      Name_Entries.Table (Id).Int2_Info := Val;
   end Set_Name_Table_Info2;

   -----------------------------
   -- Store_Encoded_Character --
   -----------------------------

   procedure Store_Encoded_Character (C : Char_Code) is

      procedure Set_Hex_Chars (C : Char_Code);
      --  Stores given value, which is in the range 0 .. 255, as two hex
      --  digits (using lower case a-f) in Name_Buffer, incrementing Name_Len.

      -------------------
      -- Set_Hex_Chars --
      -------------------

      procedure Set_Hex_Chars (C : Char_Code) is
         Hexd : constant String := "0123456789abcdef";
         N    : constant Natural := Natural (C);
      begin
         Name_Buffer (Name_Len + 1) := Hexd (N / 16 + 1);
         Name_Buffer (Name_Len + 2) := Hexd (N mod 16 + 1);
         Name_Len := Name_Len + 2;
      end Set_Hex_Chars;

   --  Start of processing for Store_Encoded_Character

   begin
      Name_Len := Name_Len + 1;

      if In_Character_Range (C) then
         declare
            CC : constant Character := Get_Character (C);
         begin
            if CC in 'a' .. 'z' or else CC in '0' .. '9' then
               Name_Buffer (Name_Len) := CC;
            else
               Name_Buffer (Name_Len) := 'U';
               Set_Hex_Chars (C);
            end if;
         end;

      elsif In_Wide_Character_Range (C) then
         Name_Buffer (Name_Len) := 'W';
         Set_Hex_Chars (C / 256);
         Set_Hex_Chars (C mod 256);

      else
         Name_Buffer (Name_Len) := 'W';
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := 'W';
         Set_Hex_Chars (C / 2 ** 24);
         Set_Hex_Chars ((C / 2 ** 16) mod 256);
         Set_Hex_Chars ((C / 256) mod 256);
         Set_Hex_Chars (C mod 256);
      end if;
   end Store_Encoded_Character;

   --------------------------------------
   -- Strip_Qualification_And_Suffixes --
   --------------------------------------

   procedure Strip_Qualification_And_Suffixes is
      J : Integer;

   begin
      --  Strip package body qualification string off end

      for J in reverse 2 .. Name_Len loop
         if Name_Buffer (J) = 'X' then
            Name_Len := J - 1;
            exit;
         end if;

         exit when Name_Buffer (J) /= 'b'
           and then Name_Buffer (J) /= 'n'
           and then Name_Buffer (J) /= 'p';
      end loop;

      --  Find rightmost __ or $ separator if one exists. First we position
      --  to start the search. If we have a character constant, position
      --  just before it, otherwise position to last character but one

      if Name_Buffer (Name_Len) = ''' then
         J := Name_Len - 2;
         while J > 0 and then Name_Buffer (J) /= ''' loop
            J := J - 1;
         end loop;

      else
         J := Name_Len - 1;
      end if;

      --  Loop to search for rightmost __ or $ (homonym) separator

      while J > 1 loop

         --  If $ separator, homonym separator, so strip it and keep looking

         if Name_Buffer (J) = '$' then
            Name_Len := J - 1;
            J := Name_Len - 1;

         --  Else check for __ found

         elsif Name_Buffer (J) = '_' and then Name_Buffer (J + 1) = '_' then

            --  Found __ so see if digit follows, and if so, this is a
            --  homonym separator, so strip it and keep looking.

            if Name_Buffer (J + 2) in '0' .. '9' then
               Name_Len := J - 1;
               J := Name_Len - 1;

            --  If not a homonym separator, then we simply strip the
            --  separator and everything that precedes it, and we are done

            else
               Name_Buffer (1 .. Name_Len - J - 1) :=
                 Name_Buffer (J + 2 .. Name_Len);
               Name_Len := Name_Len - J - 1;
               exit;
            end if;

         else
            J := J - 1;
         end if;
      end loop;
   end Strip_Qualification_And_Suffixes;

   -----------------
   -- String_Find --
   -----------------

   function String_Find (S : String) return Name_Id is
   begin
       if S = "" then
           return No_Name;
       end if;
     
       Add_New_String (S);
       return Name_Find;
   end String_Find;

   ------------
   -- Unlock --
   ------------

   procedure Unlock is
   begin
      Name_Chars.Set_Last (Name_Chars.Last - Name_Chars_Reserve);
      Name_Entries.Set_Last (Name_Entries.Last - Name_Entries_Reserve);
      Name_Chars.Locked := False;
      Name_Entries.Locked := False;
      Name_Chars.Release;
      Name_Entries.Release;
   end Unlock;

end Artics.Name_Stock;
