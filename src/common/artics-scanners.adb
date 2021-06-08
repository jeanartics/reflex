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

with System.Wch_Con; use System.Wch_Con;

with Artics.Opt; use Artics.Opt;
with Artics.Widechar; use Artics.Widechar;
with Artics.Csets; use Artics.Csets;

package body Artics.Scanners is

   use Ascii;
   
   Name_Buffer : String (1 .. 4 * Max_Line_Length);
   --  This buffer is used to set the name to be stored in the table for the
   --  Name_Find call, and to retrieve the name for the Get_Name_String call.
   --  The limit here is intended to be an infinite value that ensures that we
   --  never overflow the buffer (names this long are too absurd to worry!)

   Name_Len : Natural;
   --  Length of name stored in Name_Buffer. Used as an input parameter for
   --  Name_Find, and as an output value by Get_Name_String, or Write_Name.

   -----------------
   -- New_Scanner --
   -----------------
   
   function New_Scanner return Scanner_Ptr is
      Sptr : Scanner_Ptr := new Scanner_Record'(No_Scanner_Record);
   begin
      return Sptr;
   end New_Scanner;
   
   ------------------------
   -- Initialize_Scanner --
   ------------------------
   
   procedure Initialize_Scanner (This : Scanner_Ptr) is
   begin
      null;
   end Initialize_Scanner;
   
   ----------------------
   -- Set_Input_Buffer --
   ----------------------
   
   procedure Set_Input_Buffer
     (This : access Scanner_Record;
      Buf  : Input_Buffer) is
   begin
      This.Current_Buffer := Buf;
      This.Source := Get_Source_Buffer (Buf);
   end Set_Input_Buffer;
   
   --------------------------
   -- Scan_Line_Terminator --
   --------------------------
   
   procedure Scan_Line_Terminator (This : access Scanner_Record) is
      
      Physical : Boolean := False;
   begin
      --  Check line too long

     This.Check_End_Of_Line;
     Skip_Line_Terminators (This.Current_Buffer, This.Scan_Ptr, Physical);

     --  If we are at start of physical line, update scan pointers to
     --  reflect the start of the new line.
     
     if Physical then
	This.Current_Line_Start       := This.Scan_Ptr;
	This.Start_Column             := This.Set_Start_Column;
	This.First_Non_Blank_Location := This.Scan_Ptr;
     end if;
   end Scan_Line_Terminator;
   
   ----------------------
   -- Set_Start_Column --
   ----------------------
   
   function Set_Start_Column 
     (This :  access Scanner_Record) return Column_Number is
   begin
      return Set_Start_Column (This.Current_Buffer, This.Scan_Ptr);
   end Set_Start_Column;
   
   ---------------------
   -- Scan_Identifier --
   ---------------------
    
   procedure Scan_Identifier (This : access Scanner_Record) is
   begin
      if This.Token_String /= null then
	 Artics.Types.Free (This.Token_String);
      end if;
      
      Name_Len := 1;
      
      loop
	 --  This loop scans as fast as possible past lower half letters and
	 --  digits, which we expect to be the most common characters.
	 
	 loop
	    if This.Source (This.Scan_Ptr) in 'a' .. 'z'
	      or else This.Source (This.Scan_Ptr) in '0' .. '9'
	    then
	       Name_Buffer (Name_Len + 1) := This.Source (This.Scan_Ptr);
	       Accumulate_Checksum 
		 (This.Current_Buffer, This.Source (This.Scan_Ptr));
	       
	    elsif This.Source (This.Scan_Ptr) in 'A' .. 'Z' then
	       Name_Buffer (Name_Len + 1) :=
                 Character'Val 
                   (Character'Pos (This.Source (This.Scan_Ptr)) + 32);
               Accumulate_Checksum
                 (This.Current_Buffer, Name_Buffer (Name_Len + 1));
	       
	    else
	       exit;
	    end if;
	    
	    This.Underline_Found := False;
	    This.Scan_Ptr := This.Scan_Ptr + 1;
	    Name_Len := Name_Len + 1;
	 end loop;
	 
	 --  If we fall through, then we have encountered either an underline
	 --  character, or an extended identifier character (i.e. one from the
	 --  upper half), or a wide character, or an identifier terminator. The
	 --  initial test speeds us up in the most common case where we have
	 --  an identifier terminator. Note that ESC is an identifier character
	 --  only if a wide character encoding method that uses ESC encoding
	 --  is active, so if we find an ESC character we know that we have a
	 --  wide character.
	 
	 if Identifier_Char (This.Source (This.Scan_Ptr))
           or else This.Source (This.Scan_Ptr) 
	   in Artics.Input_Buffers.Upper_Half_Character
	 then
	    --  Case of underline
	    
	    if This.Source (This.Scan_Ptr) = '_' then
	       Accumulate_Checksum (This.Current_Buffer, '_');
	       
	       if This.Underline_Found then
		  Error_No_Double_Underline (This);
	       else
		  This.Underline_Found := True;
		  Name_Len := Name_Len + 1;
		  Name_Buffer (Name_Len) := '_';
	       end if;
	    end if;
	    
            This.Scan_Ptr := This.Scan_Ptr + 1;
	    
	 else
	    exit;
	 end if;
      end loop;
      
      --  Scan of identifier is complete. The identifier is stored in
      --  Name_Buffer, and Scan_Ptr points past the last character.
      
      if Name_Len /= 0 then
         This.Token_String := 
	   new String'(Name_Buffer (1..Name_Len)); -- Name_Find;
      end if;

      --  Check for identifier ending with underline or punctuation char

      if This.Underline_Found then
         This.Underline_Found := False;

         if This.Source (This.Scan_Ptr - 1) = '_' then
            Error_Msg
              (This, "identifier cannot end with underline", This.Scan_Ptr - 1);
	 end if;
      end if;

      --  We will assume it is an identifier, not a keyword, so that the
      --  checksum is independent of the Ada version.
   end Scan_Identifier;
   
   -----------------------
   -- Double_Char_Token --
   -----------------------

   function Double_Char_Token 
     (This : access Scanner_Record;
      C    : Character) return Boolean is
   begin
      if This.Source (This.Scan_Ptr + 1) = C then
	 This.Accumulate_Checksum (C);
	 This.Scan_Ptr := This.Scan_Ptr + 2;
	 return True;

      elsif This.Source (This.Scan_Ptr + 1) = ' '
	and then This.Source (This.Scan_Ptr + 2) = C
      then
	 This.Scan_Ptr := This.Scan_Ptr + 1;
	 This.Error_Msg_S ("no space allowed here", This.Scan_Ptr);
	 This.Scan_Ptr := This.Scan_Ptr + 2;
	 return True;

      else
	 return False;
      end if;
   end Double_Char_Token;
   
   -----------------------
   -- Check_End_Of_Line --
   -----------------------

   procedure Check_End_Of_Line (This : access Scanner_Record) is
      
      Len : constant Int :=
        Int (This.Scan_Ptr) -
        Int (This.Current_Line_Start) -
        Int (This.Wide_Char_Byte_Count);
   begin
      --  Deal with checking maximum line length

         --  If style checking is inactive, check maximum line length against
         --  standard value.

      if Len > Max_Line_Length then
         This.Error_Msg
	   ("this line is too long", 
	    This.Current_Line_Start + Source_Ptr (Max_Line_Length));
      end if;

      --  Now one more checking circuit. Normally we are only enforcing a limit
      --  of physical characters, with tabs counting as one character. But if
      --  after tab expansion we would have a total line length that exceeded
      --  32766, that would really cause trouble, because column positions
      --  would exceed the maximum we allow for a column count. Note: the limit
      --  is 32766 rather than 32767, since we use a value of 32767 for special
      --  purposes (see Sinput). Now we really do not want to go messing with
      --  tabs in the normal case, so what we do is to check for a line that
      --  has more than 4096 physical characters. Any shorter line could not
      --  be a problem, even if it was all tabs.

      if Len >= 4096 then
         declare
            Col : Natural;
            Ptr : Source_Ptr;

         begin
            Col := 1;
            Ptr := This.Current_Line_Start;
            loop
               exit when Ptr = This.Scan_Ptr;

               if This.Source (Ptr) = Ascii.Ht then
                  Col := (Col - 1 + 8) / 8 * 8 + 1;
               else
                  Col := Col + 1;
               end if;

               if Col > 32766 then
                  This.Error_Msg
                    ("this line is longer than 32766 characters",
                     This.Current_Line_Start);
                  raise Unrecoverable_Error;
               end if;

               Ptr := Ptr + 1;
            end loop;
         end;
      end if;

      --  Reset wide character byte count for next line

      This.Wide_Char_Byte_Count := 0;
   end Check_End_Of_Line;
   
   ----------
   -- Nlit --
   ----------
   
   procedure Nlit (This : access Scanner_Record) is
      
      C : Character;
      --  Current source program character

      Base_Char : Character;
      --  Either # or : (character at start of based number)

      Base : Int;
      --  Value of base

      Ui_Base : Uint;
      --  Value of base in Uint format

      Ui_Int_Value : Uint;
      --  Value of integer scanned by Scan_Integer in Uint format

      Ui_Num_Value : Uint;
      --  Value of integer in numeric value being scanned

      Scale : Int;
      --  Scale value for real literal

      Ui_Scale : Uint;
      --  Scale in Uint format

      Exponent_Is_Negative : Boolean;
      --  Set true for negative exponent

      Extended_Digit_Value : Int;
      --  Extended digit value

      Point_Scanned : Boolean;
      --  Flag for decimal point scanned in numeric literal

   begin
      Base := 10;
      Ui_Base := Uint_10;
      Ui_Int_Value := Uint_0;
      This.Based_Literal_Uses_Colon := False;
      Scale := 0;
      
      Ui_Int_Value := Scan_Integer (This);
      
      Point_Scanned := False;
      Ui_Num_Value := Ui_Int_Value;

      --  Various possibilities now for continuing the literal are period,
      --  E/e (for exponent), or :/# (for based literal).

      Scale := 0;
      C := This.Source (This.Scan_Ptr);

      if C = '.' then

	 --  Scan out point, but do not scan past .. which is a range
	 --  sequence, and must not be eaten up scanning a numeric literal.

	 while C = '.' and then This.Source (This.Scan_Ptr + 1) /= '.' loop
	    This.Accumulate_Checksum ('.');

	    if Point_Scanned then
	      This.Error_Msg_S ("duplicate point ignored", This.Scan_Ptr);
	    end if;

	    Point_Scanned := True;
	    This.Scan_Ptr := This.Scan_Ptr + 1;
	    C := This.Source (This.Scan_Ptr);

	    if C not in '0' .. '9' then
	       Error_Msg
		 (This,
		  "real literal cannot end with point", This.Scan_Ptr - 1);
	    else
	       Ui_Int_Value := Scan_Integer (This);
	       Ui_Num_Value := Ui_Int_Value;
	    end if;
	 end loop;

	 --  Based literal case. The base is the value we already scanned.
	 --  In the case of colon, we insist that the following character
	 --  is indeed an extended digit or a period. This catches a number
	 --  of common errors, as well as catching the well known tricky
	 --  bug otherwise arising from "x : integer range 1 .. 10:= 6;"

      elsif C = '#'
	or else (C = ':' and then
		   (This.Source (This.Scan_Ptr + 1) = '.'
		      or else
		      This.Source (This.Scan_Ptr + 1) in '0' .. '9'
		      or else
		      This.Source (This.Scan_Ptr + 1) in 'A' .. 'Z'
		      or else
		      This.Source (This.Scan_Ptr + 1) in 'a' .. 'z'))
      then
	 This.Accumulate_Checksum (C);
	 Base_Char := C;
	 Ui_Base := Ui_Int_Value;

	 if Base_Char = ':' then
	    This.Based_Literal_Uses_Colon := True;
	 end if;

	 if Ui_Base < 2 or else Ui_Base > 16 then
	    Error_Msg_S (This, "base not 2-16", This.Scan_Ptr);
	    Ui_Base := Uint_16;
	 end if;

	 Base := Ui_To_Int (Ui_Base);
	 This.Scan_Ptr := This.Scan_Ptr + 1;

	 --  Scan out extended integer [. integer]

	 C := This.Source (This.Scan_Ptr);
	 Ui_Int_Value := Uint_0;
	 Scale := 0;

	 loop
	    if C in '0' .. '9' then
	       This.Accumulate_Checksum (C);
	       Extended_Digit_Value :=
		 Int'(Character'Pos (C)) - Int'(Character'Pos ('0'));

	    elsif C in 'A' .. 'F' then
	       This.Accumulate_Checksum
		 (Character'Val (Character'Pos (C) + 32));
	       Extended_Digit_Value :=
		 Int'(Character'Pos (C)) - Int'(Character'Pos ('A')) + 10;

	    elsif C in 'a' .. 'f' then
	       This.Accumulate_Checksum (C);
	       Extended_Digit_Value :=
		 Int'(Character'Pos (C)) - Int'(Character'Pos ('a')) + 10;

	    else
	       This.Error_Msg_S ("extended digit expected", This.Scan_Ptr);
	       exit;
	    end if;

	    if Extended_Digit_Value >= Base then
	       This.Error_Msg_S ("digit '>= base", This.Scan_Ptr);
	    end if;

	    Ui_Int_Value := Ui_Int_Value * Ui_Base + Extended_Digit_Value;
	    Scale := Scale - 1;
	    This.Scan_Ptr := This.Scan_Ptr + 1;
	    C := This.Source (This.Scan_Ptr);

	    if C = '_' then
	       loop
		  This.Accumulate_Checksum ('_');
		  This.Scan_Ptr := This.Scan_Ptr + 1;
		  C := This.Source (This.Scan_Ptr);
		  exit when C /= '_';
		  This.Error_No_Double_Underline;
	       end loop;

	    elsif C = '.' then
	       This.Accumulate_Checksum ('.');

	       if Point_Scanned then
		  This.Error_Msg_S ("duplicate point ignored", This.Scan_Ptr);
	       end if;

	       This.Scan_Ptr := This.Scan_Ptr + 1;
	       C := This.Source (This.Scan_Ptr);
	       Point_Scanned := True;
	       Scale := 0;

	    elsif C = Base_Char then
	       This.Accumulate_Checksum (C);
	       This.Scan_Ptr := This.Scan_Ptr + 1;
	       exit;

	    elsif C = '#' or else C = ':' then
	       This.Error_Msg_S
		 ("based number delimiters must match", This.Scan_Ptr);
	       This.Scan_Ptr := This.Scan_Ptr + 1;
	       exit;

	    elsif not Identifier_Char (C) then
	       if Base_Char = '#' then
		  This.Error_Msg_S ("missing '#", This.Scan_Ptr);
	       else
		  This.Error_Msg_S ("missing ':", This.Scan_Ptr);
	       end if;

	       exit;
	    end if;

	 end loop;

	 Ui_Num_Value := Ui_Int_Value;
      end if;

      --  Scan out exponent

      if not Point_Scanned then
	 Scale := 0;
	 Ui_Scale := Uint_0;
      else
	 Ui_Scale := Ui_From_Int (Scale);
      end if;

      if This.Source (This.Scan_Ptr) = 'e' 
	or else This.Source (This.Scan_Ptr) = 'E' 
      then
	 This.Accumulate_Checksum ('e');
	 This.Scan_Ptr := This.Scan_Ptr + 1;
	 Exponent_Is_Negative := False;

	 if This.Source (This.Scan_Ptr) = '+' then
	    This.Accumulate_Checksum ('+');
	    This.Scan_Ptr := This.Scan_Ptr + 1;

	 elsif This.Source (This.Scan_Ptr) = '-' then
	    This.Accumulate_Checksum ('-');

	    if not Point_Scanned then
	       This.Error_Msg_S
		 ("negative exponent not allowed for integer literal",
		 This.Scan_Ptr);
	    else
	       Exponent_Is_Negative := True;
	    end if;
	    
	    This.Scan_Ptr := This.Scan_Ptr + 1;
	 end if;

	 Ui_Int_Value := Uint_0;

	 if This.Source (This.Scan_Ptr) in '0' .. '9' then
	    Ui_Int_Value := Scan_Integer (This);
	 else
	    This.Error_Digit_Expected;
	 end if;

	 if Exponent_Is_Negative then
	    Ui_Scale := Ui_Scale - Ui_Int_Value;
	 else
	    Ui_Scale := Ui_Scale + Ui_Int_Value;
	 end if;
      end if;

      --  Case of real literal to be returned

      if Point_Scanned then
	 This.Current_Token := Generic_Tok_Real_Literal;
	 This.Real_Literal_Value :=
	   Ur_From_Components (
			       Num   => Ui_Num_Value,
			       Den   => -Ui_Scale,
			       Rbase => Base);

	 --  Case of integer literal to be returned

      else
	 This.Current_Token := Generic_Tok_Integer_Literal;

	 if Ui_Scale = 0 then
	    This.Int_Literal_Value := Ui_Num_Value;

	    --  Avoid doing possibly expensive calculations in cases like
	    --  parsing 163E800_000# when semantics will not be done anyway.
	    --  This is especially useful when parsing garbled input.
	    This.Int_Literal_Value := Ui_Num_Value * Ui_Base ** Ui_Scale;

         end if;
      end if;

      return;
   end Nlit;
   
   ----------
   -- Slit --
   ----------
   
   procedure Slit (This : access Scanner_Record) is
      
      Delimiter : Character;
      --  Delimiter (first character of string)
      
      C : Character;
      --  Current source program character
      
      Code : Char_Code;
      --  Current character code value
      
      Err : Boolean;
      --  Error flag for Scan_Wide call
      
      String_Start : Source_Ptr;
      --  Point to first character of string
      procedure Error_Bad_String_Char;
      --  Signal bad character in string/character literal. On entry
      --  Scan_Ptr points to the improper character encountered during the
      --  scan. Scan_Ptr is not modified, so it still points to the bad
      --  character on return.
      
      procedure Error_Unterminated_String;
      --  Procedure called if a line terminator character is encountered
      --  during scanning a string, meaning that the string is not properly
      --  terminated.
      
      ---------------------------
      -- Error_Bad_String_Char --
      ---------------------------
      
      procedure Error_Bad_String_Char is
	 C : constant Character := This.Source (This.Scan_Ptr);
	 
      begin
	 if C = Ht then
	    This.Error_Msg_S
	      ("horizontal tab not allowed in string", This.Scan_ptr);
	    
	 elsif C = Vt or else C = Ff then
	    This.Error_Msg_S
	      ("format effector not allowed in string", This.Scan_Ptr);
	    
	 elsif C in Upper_Half_Character then
	    This.Error_Msg_S
	      ("(Ada 83) upper half character not allowed", This.Scan_Ptr);
	    
	 else
	    This.Error_Msg_S
	      ("control character not allowed in string", This.Scan_Ptr);
	 end if;
      end Error_Bad_String_Char;

      -------------------------------
      -- Error_Unterminated_String --
      -------------------------------
      
      procedure Error_Unterminated_String is
	 S : Source_Ptr;
	 
      begin
	 --  An interesting little refinement. Consider the following
	 --  examples:
	 
	 --     A := "this is an unterminated string;
	 --     A := "this is an unterminated string &
	 --     P(A, "this is a parameter that didn't get terminated);
	 --     P("this is a parameter that didn't get terminated, A);
	 
	 --  We fiddle a little to do slightly better placement in these
	 --  cases also if there is white space at the end of the line we
	 --  place the flag at the start of this white space, not at the
	 --  end. Note that we only have to test for blanks, since tabs
	 --  aren't allowed in strings in the first place and would have
	 --  caused an error message.
	 
	 --  Two more cases that we treat specially are:
	 
	 --     A := "this string uses the wrong terminator'
	 --     A := "this string uses the wrong terminator' &
	 
	 --  In these cases we give a different error message as well
	 
	 --  We actually reposition the scan pointer to the point where we
	 --  place the flag in these cases, since it seems a better bet on
	 --  the original intention.
	 
	 while This.Source (This.Scan_Ptr - 1) = ' '
	   or else This.Source (This.Scan_Ptr - 1) = '&'
	 loop
	    This.Scan_Ptr := This.Scan_Ptr - 1;
	    Unstore_String_Char;
	 end loop;
	 
	 --  Check for case of incorrect string terminator, but single quote
	 --  is not considered incorrect if the opening terminator misused
	 --  a single quote (error message already given).
	 
	 if Delimiter /= '''
	   and then This.Source (This.Scan_Ptr - 1) = '''
	 then
	    Unstore_String_Char;
	    This.Error_Msg
	      ("incorrect string terminator character", This.Scan_Ptr - 1);
	    return;
	 end if;
	 
	 --  Backup over semicolon or right-paren/semicolon sequence
	 
	 if This.Source (This.Scan_Ptr - 1) = ';' then
	    This.Scan_Ptr := This.Scan_Ptr - 1;
	    Unstore_String_Char;
	    
	    if This.Source (This.Scan_Ptr - 1) = ')' then
	       This.Scan_Ptr := This.Scan_Ptr - 1;
	       Unstore_String_Char;
	    end if;
	 end if;
	 
	 --  See if there is a comma in the string, if so, guess that
	 --  the first comma terminates the string.
	 
	 S := String_Start;
	 while S < This.Scan_Ptr loop
	    if This.Source (S) = ',' then
	       while This.Scan_Ptr > S loop
		  This.Scan_Ptr := This.Scan_Ptr - 1;
		  Unstore_String_Char;
	       end loop;
	       
	       exit;
	    end if;
	    
	    S := S + 1;
	 end loop;
	 
	 --  Now we have adjusted the scan pointer, give message
	 
	 This.Error_Msg_S ("missing string quote", This.Scan_Ptr);
      end Error_Unterminated_String;
      
      --  Start of processing for Slit
      
   begin
      --  On entry, Scan_Ptr points to the opening character of the string
      --  which is either a percent, double quote, or apostrophe (single
      --  quote). The latter case is an error detected by the character
      --  literal circuit.
      
      String_Start := This.Scan_Ptr;
      
      Delimiter := This.Source (This.Scan_Ptr);
      This.Accumulate_Checksum (Delimiter);
      
      Start_String;
      This.Wide_Character_Found      := False;
      This.Wide_Wide_Character_Found := False;
      This.Scan_Ptr := This.Scan_Ptr + 1;
      
      --  Loop to scan out characters of string literal
      
      loop
	 C := This.Source (This.Scan_Ptr);
	 
	 if C = Delimiter then
	    This.Accumulate_Checksum (C);
	    This.Scan_Ptr := This.Scan_Ptr + 1;
	    exit when This.Source (This.Scan_Ptr) /= Delimiter;
	    Code := Get_Char_Code (C);
	    This.Accumulate_Checksum (C);
	    This.Scan_Ptr := This.Scan_Ptr + 1;
	    
	 else
	    if C = '"' and then Delimiter = '%' then
	       This.Error_Msg_S
		 ("quote not allowed in percent delimited string",
		  This.Scan_Ptr);
	       
	       Code := Get_Char_Code (C);
	       This.Scan_Ptr := This.Scan_Ptr + 1;
	    
	    elsif This.Start_Of_Wide_Character then
	       This.Wptr := This.Scan_Ptr;
	       Scan_Wide (This.Source, This.Scan_Ptr, Code, Err);
	       
	       if Err then
		  This.Error_Illegal_Wide_Character;
		  Code := Get_Char_Code (' ');
	       end if;
	       
	       This.Accumulate_Checksum (Code);
	       
	    else
	       This.Accumulate_Checksum (C);
	       
	       if C not in Graphic_Character then
		  if C in Line_Terminator then
		     Error_Unterminated_String;
		     exit;
		     
		  elsif C in Upper_Half_Character then
		     null;
		  else
		     Error_Bad_String_Char;
		  end if;
	       end if;
	       
	       Code := Get_Char_Code (C);
	       This.Scan_Ptr := This.Scan_Ptr + 1;
	    end if;
	 end if;
	 
	 Store_String_Char (Code);
	 
	 if not In_Character_Range (Code) then
	    if In_Wide_Character_Range (Code) then
	       This.Wide_Character_Found := True;
	    else
	       This.Wide_Wide_Character_Found := True;
	    end if;
	 end if;
      end loop;
      
      This.String_Literal_Id := End_String;
      This.Current_Token := Generic_Tok_String_Literal;
      
      return;
   end Slit;
   
   -----------------------------
   -- Start_Of_Wide_Character --
   -----------------------------

   function Start_Of_Wide_Character
     (This : access Scanner_Record) return Boolean is
      
      C : constant Character := This.Source (This.Scan_Ptr);
   begin
      --  ESC encoding method with ESC present

      if C = Esc
	and then Wide_Character_Encoding_Method in Wc_Esc_Encoding_Method
      then
	 return True;

	 --  Upper half character with upper half encoding

      elsif C in Upper_Half_Character and then Upper_Half_Encoding then
	 return True;

	 --  Brackets encoding

      elsif C = '['
	and then This.Source (This.Scan_Ptr + 1) = '"'
	and then Identifier_Char (This.Source (This.Scan_Ptr + 2))
      then
	 return True;

	 --  Not the start of a wide character

      else
	 return False;
      end if;
   end Start_Of_Wide_Character;
   
   ------------------
   -- Scan_Integer --
   ------------------
   
   function Scan_Integer (This : access Scanner_Record) return Uint is
      
      C : Character;
      --  Next character scanned
      
      Ui_Int_Value : Uint := Uint_0;
      --  Value of integer scanned by Scan_Integer in Uint format
     begin
      C := This.Source (This.Scan_Ptr);
      
      --  Loop through digits (allowing underlines)
      
      loop
	 Accumulate_Checksum (This.Current_Buffer, C);
	 Ui_Int_Value :=
	   Ui_Int_Value * 10 + (Character'Pos (C) - Character'Pos ('0'));
	 This.Scan_Ptr := This.Scan_Ptr + 1;
	 
	 C := This.Source (This.Scan_Ptr);
	 
	 --  Case of underline encountered
	 
	 if C = '_' then
	    
	    --  We do not accumulate the '_' in the checksum, so that
	    --  1_234 is equivalent to 1234, and does not trigger
	    --  compilation for "minimal recompilation" (gnatmake -m).
	    
	    loop
	       This.Scan_Ptr := This.Scan_Ptr + 1;
	       C := This.Source (This.Scan_Ptr);
	       exit when C /= '_';
	       Error_No_Double_Underline (This);
	    end loop;
	    
	    if C not in '0' .. '9' then
	       Error_Digit_Expected (This);
	       exit;
	    end if;
	    
	 else
	    exit when C not in '0' .. '9';
	 end if;
      end loop;
      
      return Ui_Int_Value;
   end Scan_Integer;
   
   ----------------------
   -- Get_Token_String --
   ----------------------
   
   function Get_Token_String (This : access Scanner_Record) return String is
   begin
      if This.Token_String /= null then
         return This.Token_String.all;
      else
         return "";
      end if;
   end Get_Token_String;
   
   ---------------
   -- Error_Msg --
   ---------------
   
   procedure Error_Msg 
     (This : access Scanner_Record;
      Msg  : String;
      Loc  : Source_Ptr) is
   begin
      null;
   end Error_Msg;
   
   ---------------
   -- Error_Msg --
   ---------------
   
   procedure Error_Msg_S 
     (This : access Scanner_Record;
      Msg  : String;
      Loc  : Source_Ptr) is
   begin
      null;
   end Error_Msg_S;
   
   -----------------------------
   -- Error_Illegal_Character --
   -----------------------------

   procedure Error_Illegal_Character (This : access Scanner_Record) is
   begin
      This.Error_Msg_S ("illegal character", This.Scan_Ptr);
      This.Scan_Ptr := This.Scan_Ptr + 1;
   end Error_Illegal_Character;

   ----------------------------------
   -- Error_Illegal_Wide_Character --
   ----------------------------------

   procedure Error_Illegal_Wide_Character (This : access Scanner_Record) is
   begin
      This.Scan_Ptr := This.Scan_Ptr + 1;
      This.Error_Msg ("illegal wide character", This.Wptr);
   end Error_Illegal_Wide_Character;

   -------------------------------
   -- Error_No_Double_Underline --
   -------------------------------
   
   procedure Error_No_Double_Underline (This : access Scanner_Record) is
   begin
      This.Underline_Found := False;
      
      --  There are four cases, and we special case the messages
      
      if This.Source (This.Scan_Ptr) = '_' then
	 if This.Source (This.Scan_Ptr - 1) = '_' then
	    Error_Msg_S 
	      (This, 
	       "two consecutive underlines not permitted",
	       This.Scan_Ptr);
	 else
	    Error_Msg_S
	      (This, 
	       "underline cannot follow punctuation character",
	       This.Scan_Ptr);
	 end if;
	 
      else
	 if This.Source (This.Scan_Ptr - 1) = '_' then
	    Error_Msg_S
	      (This, 
	       "punctuation character cannot follow underline",
	       This.Scan_Ptr);
	 else
	    Error_Msg_S
	      (This,
	       "two consecutive punctuation characters not permitted",
	       This.Scan_Ptr);
	 end if;
      end if;
   end Error_No_Double_Underline;
   
   --------------------------
   -- Error_Digit_Expected --
   --------------------------
   
   procedure Error_Digit_Expected (This : access Scanner_Record) is
   begin
      Error_Msg_S (This, "digit expected", This.Scan_Ptr);
   end Error_Digit_Expected;
   
   -------------------------
   -- Accumulate_Checksum --
   -------------------------
   
   procedure Accumulate_Checksum
     (This : access Scanner_Record;
      C    : Character) is
   begin
      Artics.Input_Buffers.Accumulate_Checksum (This.Current_Buffer, C);
   end Accumulate_Checksum;
   
   -------------------------
   -- Accumulate_Checksum --
   -------------------------
   
   procedure Accumulate_Checksum
     (This : access Scanner_Record;
      C    : Char_Code) is
   begin
      null; 
      --  Artics.Input_Buffers.Accumulate_Checksum (This.Current_Buffer, C);
   end Accumulate_Checksum;
   
   -------------------------
   -- Initialize_Checksum --
   -------------------------
   
   procedure Initialize_Checksum (This : access Scanner_Record) is
   begin
      Artics.Input_Buffers.Initialize_Checksum (This.Current_Buffer);
   end Initialize_Checksum;
   
end Artics.Scanners;
