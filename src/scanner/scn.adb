------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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
-- Reflex is a fork from the GNAT compiler. GNAT was originally developed   --
-- by the GNAT team at  New York University. Extensive  contributions to    --
-- GNAT were provided by Ada Core Technologies Inc. Reflex is developed  by --
-- the Artics team at Grenoble.                                             --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Csets;    use Csets;
with Hostparm;
with Namet;    use Namet;
with Opt;      use Opt;
with Scans;    use Scans;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Stringt;

--with Reflex.Infos;

package body Scn is

   use ASCII;

   Used_As_Identifier : array (Token_Type) of Boolean;
   --  Flags set True if a given keyword is used as an identifier (used to
   --  make sure that we only post an error message for incorrect use of a
   --  keyword as an identifier once for a given keyword).

   procedure Check_End_Of_Line;
   --  Called when end of line encountered. Checks that line is not
   --  too long, and that other style checks for the end of line are met.

   function Determine_License return License_Type;
   --  Scan header of file and check that it has an appropriate GNAT-style
   --  header with a proper license statement. Returns GPL, Unrestricted,
   --  or Modified_GPL depending on header. If none of these, returns Unknown.

   procedure Error_Long_Line;
   --  Signal error of excessively long line

   ---------------
   -- Post_Scan --
   ---------------

   procedure Post_Scan is
   begin
      case Token is
         when Tok_Char_Literal =>
            Token_Node := New_Node (N_Character_Literal, Token_Ptr);
            Set_Char_Literal_Value (Token_Node, Character_Code);
            Set_Chars (Token_Node, Token_Name);

         when Tok_Identifier =>
            Token_Node := New_Node (N_Identifier, Token_Ptr);
            Set_Chars (Token_Node, Token_Name);

         when Tok_Real_Literal =>
            Token_Node := New_Node (N_Real_Literal, Token_Ptr);
            Set_Realval (Token_Node, Real_Literal_Value);
	    Set_Ada_Real_Literal (Token_Node, String_Literal_Id);
	       
         when Tok_Integer_Literal =>
            Token_Node := New_Node (N_Integer_Literal, Token_Ptr);
            Set_Intval (Token_Node, Int_Literal_Value);

         when Tok_String_Literal =>
            Token_Node := New_Node (N_String_Literal, Token_Ptr);
            Set_Has_Wide_Character (Token_Node, Wide_Character_Found);
            Set_Strval (Token_Node, String_Literal_Id);

         when others =>
            null;
      end case;
   end Post_Scan;

   -----------------------
   -- Check_End_Of_Line --
   -----------------------

   procedure Check_End_Of_Line is
      Len : constant Int := Int (Scan_Ptr) - Int (Current_Line_Start);

   begin
      if Len > Hostparm.Max_Line_Length then
         Error_Long_Line;
      end if;
   end Check_End_Of_Line;

   -----------------------
   -- Determine_License --
   -----------------------

   function Determine_License return License_Type is
      GPL_Found : Boolean := False;

      function Contains (S : String) return Boolean;
      --  See if current comment contains successive non-blank characters
      --  matching the contents of S. If so leave Scan_Ptr unchanged and
      --  return True, otherwise leave Scan_Ptr unchanged and return False.

      procedure Skip_EOL;
      --  Skip to line terminator character

      --------------
      -- Contains --
      --------------

      function Contains (S : String) return Boolean is
         CP : Natural;
         SP : Source_Ptr;
         SS : Source_Ptr;

      begin
         SP := Scan_Ptr;
         while Source (SP) /= CR and then Source (SP) /= LF loop
            if Source (SP) = S (S'First) then
               SS := SP;
               CP := S'First;

               loop
                  SS := SS + 1;
                  CP := CP + 1;

                  if CP > S'Last then
                     return True;
                  end if;

                  while Source (SS) = ' ' loop
                     SS := SS + 1;
                  end loop;

                  exit when Source (SS) /= S (CP);
               end loop;
            end if;

            SP := SP + 1;
         end loop;

         return False;
      end Contains;

      --------------
      -- Skip_EOL --
      --------------

      procedure Skip_EOL is
      begin
         while Source (Scan_Ptr) /= CR
           and then Source (Scan_Ptr) /= LF
         loop
            Scan_Ptr := Scan_Ptr + 1;
         end loop;
      end Skip_EOL;

   --  Start of processing for Determine_License

   begin
      loop
         if Source (Scan_Ptr) /= '-'
           or else Source (Scan_Ptr + 1) /= '-'
         then
            if GPL_Found then
               return GPL;
            else
               return Unknown;
            end if;

         elsif Contains ("Asaspecialexception") then
            if GPL_Found then
               return Modified_GPL;
            end if;

         elsif Contains ("GNUGeneralPublicLicense") then
            GPL_Found := True;

         elsif
             Contains
               ("ThisspecificationisadaptedfromtheAdaSemanticInterface")
           or else
             Contains
              ("ThisspecificationisderivedfromtheAdaReferenceManual")
         then
            return Unrestricted;
         end if;

         Skip_EOL;

         Check_End_Of_Line;

         declare
            Physical : Boolean;

         begin
            Skip_Line_Terminators (Scan_Ptr, Physical);

            --  If we are at start of physical line, update scan pointers
            --  to reflect the start of the new line.

            if Physical then
               Current_Line_Start       := Scan_Ptr;
               Start_Column             := Scanner.Set_Start_Column;
               First_Non_Blank_Location := Scan_Ptr;
            end if;
         end;
      end loop;
   end Determine_License;

   ----------------------------
   -- Determine_Token_Casing --
   ----------------------------

   function Determine_Token_Casing return Casing_Type is
   begin
      return Scanner.Determine_Token_Casing;
   end Determine_Token_Casing;

   ---------------------
   -- Error_Long_Line --
   ---------------------

   procedure Error_Long_Line is
   begin
      Error_Msg
        ("this line is too long",
         Current_Line_Start + Hostparm.Max_Line_Length);
   end Error_Long_Line;

   ------------------------
   -- Initialize_Scanner --
   ------------------------

   procedure Initialize_Scanner
     (Unit  : Unit_Number_Type;
      Index : Source_File_Index)
   is
      GNAT_Hedr : constant Text_Buffer (1 .. 78) := (others => '-');

   begin
      Scanner.Initialize_Scanner (Unit, Index);

      --  Set default for Comes_From_Source (except if we are going to process
      --  an artificial string internally created within the compiler and
      --  placed into internal source duffer). All nodes built now until we
      --  reenter the analyzer will have Comes_From_Source set to True

      if Index /= Internal_Source_File then
         Set_Comes_From_Source_Default (True);
      end if;

      --  Check license if GNAT type header possibly present

      if Source_Last (Index) - Scan_Ptr > 80
        and then Source (Scan_Ptr .. Scan_Ptr + 77) = GNAT_Hedr
      then
         Set_License (Current_Source_File, Determine_License);
      end if;

      --  Because of the License stuff above, Scng.Initialize_Scanner cannot
      --  call Scan. Scan initial token (note this initializes Prev_Token,
      --  Prev_Token_Ptr).

      --  There are two reasons not to do the Scan step in case if we
      --  initialize the scanner for the internal source buffer:

      --  - The artificial string may not be created by the compiler in this
      --    buffer when we call Initialize_Scanner

      --  - For these artificial strings a special way of scanning is used, so
      --    the standard step of the scanner may just break the algorithm of
      --    processing these strings.

      if Index /= Internal_Source_File then
         Scan;
      end if;

      --  Clear flags for reserved words used as indentifiers

      for J in Token_Type loop
         Used_As_Identifier (J) := False;
      end loop;
   end Initialize_Scanner;

   ------------------------------
   -- Scan_Reserved_Identifier --
   ------------------------------

   procedure Scan_Reserved_Identifier (Force_Msg : Boolean) is
      Token_Chars : constant String := Token_Type'Image (Token);

   begin
      --  We have in Token_Chars the image of the Token name, i.e. Tok_xxx.
      --  This code extracts the xxx and makes an identifier out of it.

      Name_Len := 0;

      for J in 5 .. Token_Chars'Length loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Fold_Lower (Token_Chars (J));
      end loop;

      Token_Name := Name_Find;

      if not Used_As_Identifier (Token) or else Force_Msg then
         Error_Msg_Name_1 := Token_Name;
         Error_Msg_SC ("reserved word* cannot be used as identifier!");
         Used_As_Identifier (Token) := True;
      end if;

      Token := Tok_Identifier;
      Token_Node := New_Node (N_Identifier, Token_Ptr);
      Set_Chars (Token_Node, Token_Name);
   end Scan_Reserved_Identifier;

end Scn;
