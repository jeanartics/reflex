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

with Artics.Csets; use Artics.Csets;
with Artics.Rx_Names; use Artics.Rx_Names;

package body Artics.Scanners.Rx_Scans is

   use Ascii;
   
   Special_Characters : array (Character) of Boolean := (others => False);
   --  For characters that are Special token, the value is True
   
   ------------------------------
   -- Reset_Special_Characters --
   ------------------------------

   procedure Reset_Special_Characters is
   begin
      Special_Characters := (others => False);
   end Reset_Special_Characters;
   
   ---------------------------
   -- Set_Special_Character --
   ---------------------------

   procedure Set_Special_Character (C : Character) is
   begin
      case C is
         when '#' | '$' | '_' | '?' | '@' | '`' | '\' | '^' | '~' =>
            Special_Characters (C) := True;

         when others =>
            null;
      end case;
   end Set_Special_Character;
   
   --------------------
   -- New_Rx_Scanner --
   --------------------
   
   function New_Rx_Scanner return Rx_Scanner_Ptr is
      This : Rx_Scanner_Ptr := new Rx_Scanner_Record'(No_Rx_Scanner_Record);
   begin
      return This;
   end New_Rx_Scanner;
   
   --------------------
   -- Get_Token_Name --
   --------------------
   
   function Get_Token_Name (This : access Rx_Scanner_Record) return Name_Id is
   begin
      return This.Token_Name;
   end Get_Token_Name;
   
   ----------------------------------
   -- Skip_Other_Format_Characters --
   ----------------------------------

   procedure Skip_Other_Format_Characters (This : access Rx_Scanner_Record) is
      
      P    : Source_Ptr;
      Code : Char_Code;
      Err  : Boolean;
   begin
      while This.Start_Of_Wide_Character loop
	 P := This.Scan_Ptr;
	 Scan_Wide (This.Source, This.Scan_Ptr, Code, Err);

	 if not Is_Utf_32_Other (Utf_32 (Code)) then
	    This.Scan_Ptr := P;
	    return;
	 end if;
      end loop;
   end Skip_Other_Format_Characters;

   ---------------------
   -- Scan_Identifier --
   ---------------------
    
   procedure Scan_Identifier (This : access Rx_Scanner_Record) is
   begin
      Scan_Identifier (Scanner_Ptr (This));
      
      This.Token      := Tok_Identifier;
      This.Token_Name := String_Find (This.Token_String.all);
      
      --  Here is where we check if it was a keyword
      
      if Is_Keyword_Name (This.Token_Name) then
	 
	 This.Token := Token_Type'Val (Get_Name_Table_Byte (This.Token_Name));

         --  We must reset Token_Name since this is not an identifier and
         --  if we leave Token_Name set, the parser gets confused because
         --  it thinks it is dealing with an identifier instead of the
         --  corresponding keyword.

         This.Token_Name := No_Name;
	 
      else
	 null;

         -- Post_Scan;
      end if;
   end Scan_Identifier;
   
   -------------------------
   -- Scan_Wide_Character --
   -------------------------
   
   --  Wide_Character scanning routine. On entry we have encountered the
   --  initial character of a wide character sequence.

   function Scan_Wide_Character 
     (This : access Rx_Scanner_Record) return Boolean is
      
      Code : Char_Code;
      Cat  : Category;
      Err  : Boolean := False;
   begin
      This.Wptr := This.Scan_Ptr;
      Scan_Wide (This.Source, This.Scan_Ptr, Code, Err);
      
      --  If bad wide character, signal error and continue scan
      
      if Err then
	 This.Error_Illegal_Wide_Character;
	 return False;
      end if;
      
      Cat := Get_Category (Utf_32 (Code));
      
      --  If OK letter, reset scan ptr and go scan identifier
      
      if Is_Utf_32_Letter (Cat) then
	 This.Scan_Ptr := This.Wptr;
	 This.Underline_Found := False;
	 This.Scan_Identifier;
	 return True;
	 
	 --  If OK wide space, ignore and keep scanning (we do not include
	 --  any ignored spaces in checksum)

      elsif Is_Utf_32_Space (Cat) then
	 return False;

	 --  If other format character, ignore and keep scanning (again we
	 --  do not include in the checksum) (this is for AI-0079).

      elsif Is_Utf_32_Other (Cat) then
	 return False;

	 --  If OK wide line terminator, terminate current line

      elsif Is_Utf_32_Line_Terminator (Utf_32 (Code)) then
	 This.Scan_Ptr := This.Wptr;
	 This.Scan_Line_Terminator;
	 return False;

	 --  Punctuation is an error (at start of identifier)

      elsif Is_Utf_32_Punctuation (Cat) then
	 This.Error_Msg
	   ("identifier cannot start with punctuation", This.Wptr);
	 This.Scan_Ptr := This.Wptr;
	 This.Underline_Found := False;
	 This.Scan_Identifier;
	 return True;

	 --  Mark character is an error (at start of identifier)

      elsif Is_Utf_32_Mark (Cat) then
	 This.Error_Msg
	   ("identifier cannot start with mark character", This.Wptr);
	 This.Scan_Ptr := This.Wptr;
	 This.Underline_Found := False;
	 This.Scan_Identifier;
	 return True;

	 --  Extended digit character is an error. Could be bad start of
	 --  identifier or bad literal. Not worth doing too much to try to
	 --  distinguish these cases, but we will do a little bit.

      elsif Is_Utf_32_Digit (Cat) then
	 This.Error_Msg
	   ("identifier cannot start with digit character", This.Wptr);
	 This.Scan_Ptr := This.Wptr;
	 This.Underline_Found := False;
	 This.Scan_Identifier;
	 return True;

	 --  All other wide characters are illegal here

      else
	 This.Error_Illegal_Wide_Character;
	 return False;
      end if;
   end Scan_Wide_Character;
   
   ---------------------
   -- Save_Scan_State --
   ---------------------
   
   procedure Save_Scan_State
     (This : access Rx_Scanner_Record;
      Sav  : in out Rx_Scanner_Record) is
   begin
      Sav := This.all;
   end Save_Scan_State;
   
   ------------------------
   -- Restore_Scan_State --
   ------------------------
   
   procedure Restore_Scan_State 
     (This : access Rx_Scanner_Record;
      Sav  : Rx_Scanner_Record) is
   begin
      This.all := Sav;
   end Restore_Scan_State;
   
   ----------
   -- Scan --
   ----------
   
   procedure Scan (This : access Rx_Scanner_Record) is
   begin
      This.Prev_Token     := This.Token;
      This.Prev_Token_Ptr := This.Token_Ptr;
      if This.Token_String /= null then
	 Free (This.Token_String);
      end if;

      --  The following loop runs more than once only if a format effector
      --  (tab, vertical tab, form  feed, line feed, carriage return) is
      --  encountered and skipped, or some error situation, such as an
      --  illegal character, is encountered.

      loop
         --  Skip past blanks, loop is opened up for speed

         while This.Source (This.Scan_Ptr) = ' ' loop
            if This.Source (This.Scan_Ptr + 1) /= ' ' then
               This.Scan_Ptr := This.Scan_Ptr + 1;
               exit;
            end if;

            if This.Source (This.Scan_Ptr + 2) /= ' ' then
               This.Scan_Ptr := This.Scan_Ptr + 2;
               exit;
            end if;

            if This.Source (This.Scan_Ptr + 3) /= ' ' then
               This.Scan_Ptr := This.Scan_Ptr + 3;
               exit;
            end if;

            if This.Source (This.Scan_Ptr + 4) /= ' ' then
               This.Scan_Ptr := This.Scan_Ptr + 4;
               exit;
            end if;

            if This.Source (This.Scan_Ptr + 5) /= ' ' then
               This.Scan_Ptr := This.Scan_Ptr + 5;
               exit;
            end if;

            if This.Source (This.Scan_Ptr + 6) /= ' ' then
               This.Scan_Ptr := This.Scan_Ptr + 6;
               exit;
            end if;

            if This.Source (This.Scan_Ptr + 7) /= ' ' then
               This.Scan_Ptr := This.Scan_Ptr + 7;
               exit;
            end if;

            This.Scan_Ptr := This.Scan_Ptr + 8;
         end loop;

         --  We are now at a non-blank character, which is the first character
         --  of the token we will scan, and hence the value of Token_Ptr.

         This.Token_Ptr := This.Scan_Ptr;

         --  Here begins the main case statement which transfers control on the
         --  basis of the non-blank character we have encountered.

         case This.Source (This.Scan_Ptr) is

            --  Line terminator characters

         when Cr | Lf | Ff | Vt =>
            This.Scan_Line_Terminator;

            --  Horizontal tab, just skip past it

         when Ht =>
            This.Scan_Ptr := This.Scan_Ptr + 1;

            --  End of file character, treated as an end of file only if it is
            --  the last character in the buffer, otherwise it is ignored.

         when Eof =>
            if This.Scan_Ptr = 
	      Source_Last (This.Current_Buffer) 
	    then
               This.Check_End_Of_Line;
               This.Token := Tok_Eof;
               return;
            else
               This.Scan_Ptr := This.Scan_Ptr + 1;
            end if;

            --  Ampersand

         when '&' =>
            This.Accumulate_Checksum ('&');

            if This.Source (This.Scan_Ptr + 1) = '&' then
               This.Error_Msg_S ("'&'& should be `AND THEN`", This.Scan_Ptr);
	       This.Scan_Ptr := This.Scan_Ptr + 2;
               This.Token := Tok_And;
               return;

            else
               This.Scan_Ptr := This.Scan_Ptr + 1;
               This.Token := Tok_Ampersand;
               return;
            end if;

            --  Asterisk(can be multiplication operator or double asterisk which
            --  is the exponentiation compound delimiter).

         when '*' =>
            This.Accumulate_Checksum ('*');

            if This.Source (This.Scan_Ptr + 1) = '*' then
               This.Accumulate_Checksum ('*');
               This.Scan_Ptr := This.Scan_Ptr + 2;
               This.Token := Tok_Double_Asterisk;
               return;

            else
               This.Scan_Ptr := This.Scan_Ptr + 1;
               This.Token := Tok_Asterisk;
               return;
            end if;

            --  Colon, which can either be an isolated colon, or part of an
            --  assignment compound delimiter.

         when ':' =>
            This.Accumulate_Checksum (':');

            if This.Double_Char_Token ('=') then
               This.Token := Tok_Colon_Equal;
               return;

            elsif This.Source (This.Scan_Ptr + 1) = '-'
              and then This.Source (This.Scan_Ptr + 2) /= '-'
            then
               This.Token := Tok_Colon_Equal;
               This.Error_Msg (":- should be :=", This.Scan_Ptr);
               This.Scan_Ptr := This.Scan_Ptr + 2;
               return;

            else
               This.Scan_Ptr := This.Scan_Ptr + 1;
               This.Token := Tok_Colon;
               return;
            end if;

            --  Left parenthesis

         when '(' =>
            This.Accumulate_Checksum ('(');
            This.Scan_Ptr := This.Scan_Ptr + 1;
            This.Token := Tok_Left_Paren;
            return;

            --  Left bracket

         when '[' =>
            if This.Source (This.Scan_Ptr + 1) = '"' then
               if This.Scan_Wide_Character then
		  return;
	       end if;

            else
               This.Error_Msg_S
		 ("illegal character, replaced by ""(""", This.Scan_Ptr);
               This.Scan_Ptr := This.Scan_Ptr + 1;
               This.Token := Tok_Left_Paren;
               return;
            end if;

            --  Left brace

         when '{' =>
            This.Error_Msg_S
	      ("illegal character, replaced by ""(""", This.Scan_Ptr);
            This.Scan_Ptr := This.Scan_Ptr + 1;
            This.Token := Tok_Left_Paren;
            return;

            --  Comma

         when ',' =>
            This.Accumulate_Checksum (',');
            This.Scan_Ptr := This.Scan_Ptr + 1;
            This.Token := Tok_Comma;
            return;

            --  Dot, which is either an isolated period, or part of a double dot
            --  compound delimiter sequence. We also check for the case of a
            --  digit following the period, to give a better error message.

         when '.' =>
            This.Accumulate_Checksum ('.');

            if This.Double_Char_Token ('.') then
               This.Token := Tok_Dot_Dot;
               return;

            elsif This.Source (This.Scan_Ptr + 1) in '0' .. '9' then
               This.Error_Msg_S
		 ("numeric literal cannot start with point", This.Scan_Ptr);
               This.Scan_Ptr := This.Scan_Ptr + 1;

            else
               This.Scan_Ptr := This.Scan_Ptr + 1;
               This.Token := Tok_Dot;
               return;
            end if;

            --  Equal, which can either be an equality operator, or part of the
            --  arrow (=>) compound delimiter.

         when '=' =>
            This.Accumulate_Checksum ('=');

            if This.Double_Char_Token ('>') then
               This.Token := Tok_Arrow;
               return;

            elsif This.Source (This.Scan_Ptr + 1) = '=' then
               This.Error_Msg_S ("== should be =", This.Scan_Ptr);
               This.Scan_Ptr := This.Scan_Ptr + 1;
            end if;

            This.Scan_Ptr := This.Scan_Ptr + 1;
            This.Token := Tok_Equal;
            return;

            --  Greater than, which can be a greater than operator, greater than
            --  or equal operator, or first character of a right label bracket.

         when '>' =>
            This.Accumulate_Checksum ('>');

            if This.Double_Char_Token ('=') then
               This.Token := Tok_Greater_Equal;
               return;

            elsif This.Double_Char_Token ('>') then
               This.Token := Tok_Greater_Greater;
               return;

            else
               This.Scan_Ptr := This.Scan_Ptr + 1;
               This.Token := Tok_Greater;
               return;
            end if;

            --  Less than, which can be a less than operator, less than or equal
            --  operator, or the first character of a left label bracket, or the
            --  first character of a box (<>) compound delimiter.

         when '<' =>
            This.Accumulate_Checksum ('<');

            if This.Double_Char_Token ('=') then
               This.Token := Tok_Less_Equal;
               return;

            elsif This.Double_Char_Token ('>') then
               This.Token := Tok_Box;
               return;

            elsif This.Double_Char_Token ('<') then
               This.Token := Tok_Less_Less;
               return;

            else
               This.Scan_Ptr := This.Scan_Ptr + 1;
               This.Token := Tok_Less;
               return;
            end if;

            --  Minus, which is either a subtraction operator, or the first
            --  character of double minus starting a comment

         when '-' => 
	    if This.Source (This.Scan_Ptr + 1) = '>' then
	       This.Error_Msg_S ("invalid token", This.Scan_Ptr);
	       This.Scan_Ptr := This.Scan_Ptr + 2;
	       This.Token := Tok_Arrow;
	       return;
	       
	    elsif This.Source (This.Scan_Ptr + 1) /= '-' then
	       This.Accumulate_Checksum ('-');
	       This.Scan_Ptr := This.Scan_Ptr + 1;
	       This.Token := Tok_Minus;
	       return;
	       
	       --  Comment
	       
	    else -- Source (Scan_Ptr + 1) = '-' then
	       This.Scan_Ptr := This.Scan_Ptr + 2;
	       
	       This.Start_Of_Comment := This.Scan_Ptr;
	       
	       --  Loop to scan comment (this loop runs more than once only
	       --  if a horizontal tab or other non-graphic character is
	       --  scanned)
	       
	       loop
		  --  Scan to non graphic character (opened up for speed)
		  
		  --  Note that we just eat left brackets, which means that
		  --  bracket notation cannot be used for end of line
		  --  characters in comments. This seems a reasonable choice,
		  --  since no one would ever use brackets notation in a real
		  --  program in this situation, and if we allow brackets
		  --  notation, we forbid some valid comments which contain a
		  --  brackets sequence that happens to match an end of line
		  --  character.
		  
		  loop
		     exit when 
		       This.Source (This.Scan_Ptr) not in Graphic_Character;
		     This.Scan_Ptr := This.Scan_Ptr + 1;
		     
		     exit when
		       This.Source (This.Scan_Ptr) not in Graphic_Character;
		     This.Scan_Ptr := This.Scan_Ptr + 1;
		     
		     exit when
		       This.Source (This.Scan_Ptr) not in Graphic_Character;
		     This.Scan_Ptr := This.Scan_Ptr + 1;
		     
		     exit when
		       This.Source (This.Scan_Ptr) not in Graphic_Character;
		     This.Scan_Ptr := This.Scan_Ptr + 1;
		     
		     exit when
		       This.Source (This.Scan_Ptr) not in Graphic_Character;
		     This.Scan_Ptr := This.Scan_Ptr + 1;
		  end loop;
		  
		  --  Keep going if horizontal tab
		  
		  if This.Source (This.Scan_Ptr) = Ht then
		     This.Scan_Ptr := This.Scan_Ptr + 1;
		     
		     --  Terminate scan of comment if line terminator
		     
		  elsif This.Source (This.Scan_Ptr) in Line_Terminator then
		     exit;
		     
		     --  Terminate scan of comment if end of file encountered
		     --  (embedded EOF character or real last character in 
		     --  file)
		     
		  elsif This.Source (This.Scan_Ptr) = Eof then
		     exit;
		     
		     --  If we have a wide character, we have to scan it out,
		     --  because it might be a legitimate line terminator
		     
		  elsif This.Start_Of_Wide_Character then
		     declare
			Wptr : constant Source_Ptr := This.Scan_Ptr;
			Code : Char_Code;
			Err  : Boolean;
			
		     begin
			Scan_Wide (This.Source, This.Scan_Ptr, Code, Err);
			
			--  If not well formed wide character, then just skip
			--  past it and ignore it.
			
			if Err then
			   This.Scan_Ptr := Wptr + 1;
			   
			   --  If UTF_32 terminator, terminate comment scan
			   
			elsif Is_Utf_32_Line_Terminator (Utf_32 (Code)) then
			   This.Scan_Ptr := Wptr;
			   exit;
			end if;
		     end;

		     --  Keep going if character in 80-FF range, or is ESC. 
		     --  These characters are allowed in comments by 
		     --  RM-2.1(1), 2.7(2). They are allowed even in Ada 83
		     --  mode according to the approved AI. ESC was added to
		     --  the AI in June 93.
		     
		  elsif This.Source (This.Scan_Ptr) in Upper_Half_Character
		    or else This.Source (This.Scan_Ptr) = Esc
		  then
		     This.Scan_Ptr := This.Scan_Ptr + 1;
		     
		     --  Otherwise we have an illegal comment character,
		     --  ignore this error in relaxed semantics mode.
		     
		  else
		     --  if Relaxed_Rm_Semantics then
		     --     Scan_Ptr := Scan_Ptr + 1;
		     --  else
		     This.Error_Illegal_Character;
		     --  end if;
		  end if;
	       end loop;
	    end if;

            --  Double quote or percent starting a string literal

         when '"' | '%' =>
            This.Slit;
            --  Post_Scan;
            return;

            --  Apostrophe. This can either be the start of a character literal,
            --  or an isolated apostrophe used in a qualified expression or an
            --  attribute. In the following:

            --    A := CHARACTER'('A');

            --  the first apostrophe is treated as an isolated apostrophe, and 
	    --  the second one is treated as the start of the character literal
	    --  'A'. Note that RM-2.2(7) does not require a separator between
	    --  "'" and "(" in the above, so we cannot use lookahead to 
	    --  distinguish the cases; we use look-back instead. Analysis of 
	    --  the grammar shows that some tokens can be followed by an 
	    --  apostrophe, and some by a character literal, but none by both. 
	    --  Some cannot be followed by either, so it doesn't matter what 
	    --  we do in those cases, except to get good error behavior.

         when ''' => Char_Literal_Case : declare
               Code : Char_Code;
               Err  : Boolean;

            begin
               This.Accumulate_Checksum (''');
               This.Scan_Ptr := This.Scan_Ptr + 1;

               --  Distinguish between apostrophe and character literal. It's an
               --  apostrophe if the previous token is one of the following.
               --  Reserved words are included for things like A.all'Address and
               --  T'Digits'Img. Strings literals are included for things like
               --  "abs"'Address. Other literals are included to give better 
	       --  error behavior for illegal cases like 123'Img.

               if This.Prev_Token = Tok_Identifier
                 or else This.Prev_Token = Tok_Right_Paren
                 or else This.Prev_Token = Tok_All
                 or else This.Prev_Token = Tok_Delta
                 or else This.Prev_Token = Tok_Digits
                 or else This.Prev_Token = Tok_Project
                 or else This.Prev_Token in Token_Class_Literal
               then
                  This.Token := Tok_Apostrophe;
                  return;

                  --  Otherwise the apostrophe starts a character literal

               else
                  --  Case of wide character literal

                  if This.Start_Of_Wide_Character then
                     This.Wptr := This.Scan_Ptr;
                     Scan_Wide (This.Source, This.Scan_Ptr, Code, Err);
                     This.Accumulate_Checksum (Code);

                     if Err then
                        This.Error_Illegal_Wide_Character;
                        Code := Character'Pos (' ');
		     end if;

                     if This.Source (This.Scan_Ptr) /= ''' then
                        This.Error_Msg_S ("missing apostrophe", This.Scan_Ptr);
                     else
                        This.Scan_Ptr := This.Scan_Ptr + 1;
                     end if;

                     --  If we do not find a closing quote in the expected 
		     --  place then assume that we have a misguided attempt at
		     --  a string literal. However, if previous token is RANGE,
		     --  then we return an apostrophe instead since this gives
		     --  better error recovery

                  elsif This.Source (This.Scan_Ptr + 1) /= ''' then
                     if This.Prev_Token = Tok_Range then
                        This.Token := Tok_Apostrophe;
                        return;

                     else
                        This.Scan_Ptr := This.Scan_Ptr - 1;
                        This.Error_Msg_S
                          ("strings are delimited by double quote character",
			   This.Scan_Ptr);
                        This.Slit;
                        --  Post_Scan;
                        return;
                     end if;

                     --  Otherwise we have a (non-wide) character literal

                  else
                     This.Accumulate_Checksum (This.Source (This.Scan_Ptr));

                     if This.Source (This.Scan_Ptr) not in Graphic_Character 
		     then
                        if This.Source (This.Scan_Ptr) in Upper_Half_Character 
			then
                           This.Error_Illegal_Character;
                        end if;
                     end if;

                     Code := Get_Char_Code (This.Source (This.Scan_Ptr));
                     This.Scan_Ptr := This.Scan_Ptr + 2;
                  end if;

                  --  Fall through here with Scan_Ptr updated past the closing
                  --  quote, and Code set to the Char_Code value for the literal

                  This.Accumulate_Checksum (''');
                  This.Token := Tok_Char_Literal;
                  Set_Character_Literal_Name (Code);
                  This.Token_Name := Name_Find;
                  This.Character_Code := Code;
                  --  Post_Scan;
                  return;
               end if;
            end Char_Literal_Case;

            --  Right parenthesis

         when ')' =>
            This.Accumulate_Checksum (')');
            This.Scan_Ptr := This.Scan_Ptr + 1;
            This.Token := Tok_Right_Paren;
            return;

            --  Right bracket or right brace, treated as right paren

         when ']' | '}' =>
            This.Error_Msg_S
	      ("illegal character, replaced by "")""", This.Scan_Ptr);
            This.Scan_Ptr := This.Scan_Ptr + 1;
            This.Token := Tok_Right_Paren;
            return;

            --  Slash (can be division operator or first character of not equal)

         when '/' =>
            This.Accumulate_Checksum ('/');

            if This.Double_Char_Token ('=') then
               This.Token := Tok_Not_Equal;
               return;
            else
               This.Scan_Ptr := This.Scan_Ptr + 1;
               This.Token := Tok_Slash;
               return;
            end if;

            --  Semicolon

         when ';' =>
            This.Accumulate_Checksum (';');
            This.Scan_Ptr := This.Scan_Ptr + 1;
            This.Token := Tok_Semicolon;
            return;

            --  Vertical bar

         when '|' => Vertical_Bar_Case : begin
	    This.Accumulate_Checksum ('|');

               --  Special check for || to give nice message

               if This.Source (This.Scan_Ptr + 1) = '|' then
                  This.Error_Msg_S
		    ("""'|'|"" should be `OR ELSE`", This.Scan_Ptr);
                  This.Scan_Ptr := This.Scan_Ptr + 2;
                  This.Token := Tok_Or;
                  return;

               else
                  This.Scan_Ptr := This.Scan_Ptr + 1;
                  This.Token := Tok_Vertical_Bar;
                  --  Post_Scan;
                  return;
               end if;
            end Vertical_Bar_Case;

            --  Exclamation, replacement character for vertical bar

         when '!' => Exclamation_Case : begin
	    This.Accumulate_Checksum ('!');

               if This.Source (This.Scan_Ptr + 1) = '=' then
                  This.Error_Msg_S ("'!= should be /=", This.Scan_Ptr);
                  This.Scan_Ptr := This.Scan_Ptr + 2;
                  This.Token := Tok_Not_Equal;
                  return;

               else
                  This.Scan_Ptr := This.Scan_Ptr + 1;
                  This.Token := Tok_Vertical_Bar;
                  --  Post_Scan;
                  return;
               end if;
            end Exclamation_Case;

            --  Plus

         when '+' => Plus_Case : begin
	    This.Accumulate_Checksum ('+');
               This.Scan_Ptr := This.Scan_Ptr + 1;
               This.Token := Tok_Plus;
               return;
            end Plus_Case;

            --  Digits starting a numeric literal

         when '0' .. '9' =>

            --  First a bit of a scan ahead to see if we have a case of an
            --  identifier starting with a digit (remembering exponent case).

            declare
               C : constant Character := This.Source (This.Scan_Ptr + 1);

            begin
               --  OK literal if digit followed by digit or underscore

               if C in '0' .. '9' or else C = '_' then
                  null;

                  --  OK literal if digit not followed by identifier char

               elsif not Identifier_Char (C) then
                  null;

                  --  OK literal if digit followed by e/E followed by 
		  --  digit/sign. We also allow underscore after the E, which
		  --  is an error, but better handled by Nlit than deciding
		  --  this is an identifier.

               elsif (C = 'e' or else C = 'E')
                 and then (This.Source (This.Scan_Ptr + 2) in '0' .. '9'
                           or else This.Source (This.Scan_Ptr + 2) = '+'
                           or else This.Source (This.Scan_Ptr + 2) = '-'
                           or else This.Source (This.Scan_Ptr + 2) = '_')
               then
                  null;

                  --  Here we have what really looks like an identifier that
                  --  starts with a digit, so give error msg.

               else
                  This.Error_Msg_S
		    ("identifier may not start with digit", This.Scan_Ptr);
                  This.Underline_Found := False;
                  This.Scan_Identifier;
		  return;
               end if;
            end;

            --  Here we have an OK integer literal

            This.Nlit;

            --  Check for proper delimiter, ignoring other format characters

            This.Skip_Other_Format_Characters;

            if Identifier_Char (This.Source (This.Scan_Ptr)) then
               This.Error_Msg_S
                 ("delimiter required between literal and identifier", 
		  This.Scan_Ptr);
            end if;

            --  Post_Scan;
            return;

            --  Lower case letters

         when 'a' .. 'z' =>
            This.Underline_Found := False;
	    This.Scan_Identifier;
	    return;

            --  Upper case letters

         when 'A' .. 'Z' =>
            This.Underline_Found := False;
            This.Scan_Identifier;
	    return;

            --  Underline character

         when '_' =>
            if Special_Characters ('_') then
               This.Token_Ptr := This.Scan_Ptr;
               This.Scan_Ptr := This.Scan_Ptr + 1;
               This.Token := Tok_Special;
               This.Special_Character := '_';
               return;
            end if;

            This.Error_Msg_S
	      ("identifier cannot start with underline", This.Scan_Ptr);
            This.Underline_Found := False;
            This.Scan_Identifier;
	    return;

            --  Space (not possible, because we scanned past blanks)

         when ' ' =>
            raise Program_Error;

            --  Characters in top half of ASCII 8-bit chart

         when Upper_Half_Character =>

            --  Wide character case

            if Upper_Half_Encoding then
               if This.Scan_Wide_Character then
		  return;
	       end if;

               --  Otherwise we have OK Latin-1 character

            else
               --  Upper half characters may possibly be identifier letters
               --  but can never be digits, so Identifier_Char can be used to
               --  test for a valid start of identifier character.

               if Identifier_Char (This.Source (This.Scan_Ptr)) then
                  This.Underline_Found := False;
		  This.Scan_Identifier;
		  return;
               else
                  This.Error_Illegal_Character;
               end if;
            end if;

         when Esc =>

            --  ESC character, possible start of identifier if wide characters
            --  using ESC encoding are allowed in identifiers, which we can
            --  tell by looking at the Identifier_Char flag for ESC, which is
            --  only true if these conditions are met. In Ada 2005 mode, may
            --  also be valid UTF_32 space or line terminator character.

            if Identifier_Char (Esc) then
               if This.Scan_Wide_Character then
		  return;
	       end if;
	       
            else
               This.Error_Illegal_Character;
            end if;

            --  Invalid control characters

         when Nul | Soh | Stx | Etx | Eot | Enq | Ack | Bel | Bs  | Ascii.So |
              Si  | Dle | Dc1 | Dc2 | Dc3 | Dc4 | Nak | Syn | Etb | Can |
              Em  | Fs  | Gs  | Rs  | Us  | Del
            =>
            This.Error_Illegal_Character;

            --  Invalid graphic characters

         when '#' | '$' | '?' | '@' | '`' | '\' | '^' | '~' =>

            --  If Set_Special_Character has been called for this character,
            --  set Scans.Special_Character and return a Special token.

            if Special_Characters (This.Source (This.Scan_Ptr)) then
               This.Token_Ptr := This.Scan_Ptr;
               This.Token := Tok_Special;
               This.Special_Character := This.Source (This.Scan_Ptr);
               This.Scan_Ptr := This.Scan_Ptr + 1;
               return;

               --  Check for something looking like a preprocessor directive

            elsif This.Source (This.Scan_Ptr) = '#'
              and then
	      (This.Source (This.Scan_Ptr + 1 .. This.Scan_Ptr + 2) = "if"
		 or else
		 This.Source (This.Scan_Ptr + 1 .. This.Scan_Ptr + 5) = "elsif"
		 or else
		 This.Source (This.Scan_Ptr + 1 .. This.Scan_Ptr + 4) = "else"
		 or else
		 This.Source (This.Scan_Ptr + 1 .. This.Scan_Ptr + 3) = "end")
            then
               This.Error_Msg_S
		 ("preprocessor directive ignored, preprocessor not active",
		  This.Scan_Ptr);
	       
               --  Skip to end of line
	       
               loop
                  if This.Source (This.Scan_Ptr) in Graphic_Character
                    or else
                      This.Source (This.Scan_Ptr) = Ht
                  then
                     This.Scan_Ptr := This.Scan_Ptr + 1;

                     --  Done if line terminator or EOF

                  elsif This.Source (This.Scan_Ptr) in Line_Terminator
                    or else
                      This.Source (This.Scan_Ptr) = Eof
                  then
                     exit;

                     --  If we have a wide character, we have to scan it out,
                     --  because it might be a legitimate line terminator

                  elsif This.Start_Of_Wide_Character then
                     declare
                        Wptr : constant Source_Ptr := This.Scan_Ptr;
                        Code : Char_Code;
                        Err  : Boolean;

                     begin
                        Scan_Wide (This.Source, This.Scan_Ptr, Code, Err);

                        --  If not well formed wide character, then just skip
                        --  past it and ignore it.

                        if Err then
                           This.Scan_Ptr := Wptr + 1;

                           --  If UTF_32 terminator, terminate comment scan

                        elsif Is_Utf_32_Line_Terminator (Utf_32 (Code)) then
                           This.Scan_Ptr := Wptr;
                           exit;
                        end if;
                     end;

                     --  Else keep going (don't worry about bad comment chars
                     --  in this context, we just want to find the end of line.

                  else
                     This.Scan_Ptr := This.Scan_Ptr + 1;
                  end if;
               end loop;

               --  Otherwise, this is an illegal character

            else
               This.Error_Illegal_Character;
            end if;

            --  End switch on non-blank character

         end case;

         --  End loop past format effectors. The exit from this loop is by
         --  executing a return statement following completion of token scan
         --  (control never falls out of this loop to the code which follows)

      end loop;
   end Scan;

end Artics.Scanners.Rx_Scans;
