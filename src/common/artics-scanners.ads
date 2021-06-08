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

with System; use System;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation; 
with Gnat.Os_Lib; use Gnat.Os_Lib;
with GNAT.Case_Util; use GNAT.Case_Util;

with Artics.Types; use Artics.Types;
--  with Artics.Namet; use Artics.Namet;
with Artics.Uintp; use Artics.Uintp;
with Artics.Urealp; use Artics.Urealp;
with Artics.Stringt; use Artics.Stringt;

with Artics.Input_Buffers; use Artics.Input_Buffers;

package Artics.Scanners is
   
   type Generic_Token_Type is
     (Generic_No_Token,
      Generic_Tok_Identifier,
      Generic_Tok_Integer_Literal,
      Generic_Tok_Real_Literal,
      Generic_Tok_String_Literal);
   
   type Scanner_Record is tagged private;
   type Scanner_Ptr is access all Scanner_Record;
   type Scanner_Class_Ptr is access all Scanner_Record'Class;
   
   function New_Scanner return Scanner_Ptr;
   
   procedure Initialize_Scanner (This : Scanner_Ptr);
   
   procedure Set_Input_Buffer
     (This : access Scanner_Record;
      Buf  : Input_Buffer);
   
   procedure Scan_Line_Terminator (This : access Scanner_Record);
   --  Routine to scan line terminator. On entry Scan_Ptr points to a
   --  character which is one of FF,LR,CR,VT, or one of the wide characters
   --  that is treated as a line terminator.
   
   function Set_Start_Column 
     (This :  access Scanner_Record) return Column_Number;
   --  This routine is called with Scan_Ptr pointing to the first character
   --  of a line. On exit, Scan_Ptr is advanced to the first non-blank
   --  character of this line (or to the terminating format effector if the
   --  line contains no non-blank characters), and the returned result is the
   --  column number of this non-blank character (zero origin), which is the
   --  value to be stored in the Start_Column scan variable.
   
   procedure Scan_Identifier (This : access Scanner_Record);
   --  Identifier scanning routine. On entry, some initial characters of
   --  the identifier may have already been stored in Name_Buffer. If so,
   --  Name_Len has the number of characters stored, otherwise Name_Len is
   --  set to zero on entry. Underline_Found is also set False on entry.
   
   procedure Nlit (This : access Scanner_Record);
   --  This is the procedure for scanning out numeric literals. On entry,
   --  Scan_Ptr points to the digit that starts the numeric literal (the
   --  checksum for this character has not been accumulated yet). On return
   --  Scan_Ptr points past the last character of the numeric literal, Token
   --  and Token_Node are set appropriately, and the checksum is updated.
   
   procedure Slit (This : access Scanner_Record);
   --  This is the procedure for scanning out string literals. On entry,
   --  Scan_Ptr points to the opening string quote (the checksum for this
   --  character has not been accumulated yet). On return Scan_Ptr points
   --  past the closing quote of the string literal, Token and Token_Node
   --  are set appropriately, and the checksum is updated.
   
   function Start_Of_Wide_Character
     (This : access Scanner_Record) return Boolean;
   --  Returns True if the scan pointer is pointing to the start of a wide
   --  character sequence, does not modify the scan pointer in any case.

   function Scan_Integer (This : access Scanner_Record) return Uint;
   --  Scan integer literal. On entry, Scan_Ptr points to a digit, on
   --  exit Scan_Ptr points past the last character of the integer.
   --
   --  For each digit encountered, UI_Int_Value is multiplied by 10, and
   --  the value of the digit added to the result. In addition, the value
   --  in Scale is decremented by one for each actual digit scanned.
   
   function Get_Token_String (This : access Scanner_Record) return String;
   
   function Double_Char_Token
     (This : access Scanner_Record;
      C    : Character) return Boolean;
   --  This function is used for double character tokens like := or <>. It
   --  checks if the character following Source (Scan_Ptr) is C, and if so
   --  bumps Scan_Ptr past the pair of characters and returns True. A space
   --  between the two characters is also recognized with an appropriate
   --  error message being issued. If C is not present, False is returned.
   --  Note that Double_Char_Token can only be used for tokens defined in
   --  the Ada syntax (it's use for error cases like && is not appropriate
   --  since we do not want a junk message for a case like &-space-&).

   procedure Check_End_Of_Line (This : access Scanner_Record);
   
   procedure Error_No_Double_Underline (This : access Scanner_Record);
   --  Signal error of two underline or punctuation characters in a row.
   --  Called with Scan_Ptr pointing to second underline/punctuation char.
   
   procedure Error_Digit_Expected (This : access Scanner_Record);
   --  Signal error of bad digit, Scan_Ptr points to the location at
   --  which the digit was expected on input, and is unchanged on return.
   
   procedure Error_Illegal_Character (This : access Scanner_Record);
   --  Give illegal character error, Scan_Ptr points to character. On
   --  return, Scan_Ptr is bumped past the illegal character.

   procedure Error_Illegal_Wide_Character (This : access Scanner_Record);
   --  Give illegal wide character message. On return, Scan_Ptr is bumped
   --  past the illegal character, which may still leave us pointing to
   --  junk, not much we can do if the escape sequence is messed up.

   procedure Error_Msg 
     (This : access Scanner_Record;
      Msg  : String;
      Loc  : Source_Ptr);
   
   procedure Error_Msg_S 
     (This : access Scanner_Record;
      Msg  : String;
      Loc  : Source_Ptr);
   
   procedure Accumulate_Checksum
     (This : access Scanner_Record;
      C    : Character);
   
   procedure Accumulate_Checksum
     (This : access Scanner_Record;
      C    : Char_Code);
   
   procedure Initialize_Checksum (This : access Scanner_Record);
   
   
private
   
   type Scanner_Record is tagged record
      Current_Buffer            : Artics.Input_Buffers.Input_Buffer;
      Source                    : Artics.Input_Buffers.Source_Buffer_Ptr;
      Scan_Ptr                  : Artics.Input_Buffers.Source_Ptr;
      
      First_Non_Blank_Location  : Artics.Input_Buffers.Source_Ptr;
      Current_Line_Start        : Artics.Input_Buffers.Source_Ptr;
      Start_Column              : Artics.Input_Buffers.Column_Number;
      Token_String              : String_Ptr;
      Current_Token             : Generic_Token_Type;
      --  Token_Node                : Node_Id;
      Wide_Char_Byte_Count      : Natural;
      
      Start_Of_Comment : Artics.Input_Buffers.Source_Ptr;
      --  Record start of comment position

      Underline_Found : Boolean;
      --  During scanning of an identifier, set to True if last character
      --  scanned was an underline or other punctuation character. This
      --  is used to flag the error of two underlines/punctuations in a
      --  row or ending an identifier with a underline/punctuation. Here
      --  punctuation means any UTF_32 character in the Unicode category
      --  Punctuation,Connector.

      Wptr : Artics.Input_Buffers.Source_Ptr;
      --  Used to remember start of last wide character scanned
      
      Real_Literal_Value : Ureal;
      --  Valid only when Token is Tok_Real_Literal, contains the value of the
      --  scanned literal.
      
      Int_Literal_Value : Uint;
      --  Valid only when Token = Tok_Integer_Literal, contains the value of the
      --  scanned literal.
      
      Based_Literal_Uses_Colon : Boolean;
      --  Valid only when Token = Tok_Integer_Literal or Tok_Real_Literal. Set
      --  True only for the case of a based literal using ':' instead of '#'.
      
      String_Literal_Id : String_Id;
      --  Valid only when Token = Tok_String_Literal or Tok_Operator_Symbol.
      --  Contains the Id for currently scanned string value.
      
      Wide_Character_Found : Boolean := False;
      --  Valid only when Token = Tok_String_Literal. Set True if wide character
      --  found (i.e. a character that does not fit in Character, but fits in
      --  Wide_Wide_Character).
      
      Wide_Wide_Character_Found : Boolean := False;
      --  Valid only when Token = Tok_String_Literal. Set True if wide wide
      --  character found (i.e. a character that does not fit in Character or
      --  Wide_Character).
      
   end record;
   
   No_Scanner_Record : constant Scanner_Record := 
     (Current_Buffer            => null,
      Source                    => null,
      Scan_Ptr                  => 0,
      
      First_Non_Blank_Location  => 0,
      Current_Line_Start        => 0,
      Start_Column              => 0,
      Token_String              => null,
      Current_Token             => Generic_No_Token,
      Wide_Char_Byte_Count      => 0,
      Start_Of_Comment          => 0,
      Underline_Found           => False,
      Wptr                      => 0,
      Real_Literal_Value        => No_Ureal,
      Int_Literal_Value         => No_Uint,
      Based_Literal_Uses_Colon  => False,
      String_Literal_Id         => No_String,
      Wide_Character_Found      => False,
      Wide_Wide_Character_Found => False);
   
end Artics.Scanners;
