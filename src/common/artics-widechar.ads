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

--  Subprograms for manipulation of wide character sequences. Note that in
--  this package, wide character and wide wide character are not distinguished
--  since this package is basically concerned with syntactic notions, and it
--  deals with Char_Code values, rather than values of actual Ada types.

with Artics.Types; use Artics.Types;
with Artics.Input_Buffers; use Artics.Input_Buffers;

package Artics.Widechar is

  -- Wide_Character_Encoding_Method : WC_Encoding_Method := WCEM_Brackets;
   --  Method used for encoding wide characters in the source program. See
   --  description of type in unit System.WCh_Con for a list of the methods
   --  that are currently supported. Note that brackets notation is always
   --  recognized in source programs regardless of the setting of this
   --  variable. The default setting causes only the brackets notation to be
   --  recognized. If this is the main unit, this setting also controls the
   --  output of the W=? parameter in the ALI file, which is used to provide
   --  the default for encoding [Wide_[Wide_]]Text_IO files. For the binder,
   --  the value set here overrides this main unit default.

   Wide_Char_Byte_Count : Nat := 0;
   --  This value is incremented whenever Scan_Wide or Skip_Wide is called.
   --  The amount added is the length of the wide character sequence minus
   --  one. This means that the count that accumulates here represents the
   --  difference between the length in characters and the length in bytes.
   --  This is used for checking the line length in characters.

   function Length_Wide return Nat;
   --  Returns the maximum length in characters for the escape sequence that
   --  is used to encode wide character literals outside the ASCII range. Used
   --  only in the implementation of the attribute Width for Wide_Character
   --  and Wide_Wide_Character.

   procedure Scan_Wide
     (S   : Source_Buffer_Ptr;
      P   : in out Source_Ptr;
      C   : out Char_Code;
      Err : out Boolean);
   --  On entry S (P) points to the first character in the source text for
   --  a wide character (i.e. to an ESC character, a left bracket, or an
   --  upper half character, depending on the representation method). A
   --  single wide character is scanned. If no error is found, the value
   --  stored in C is the code for this wide character, P is updated past
   --  the sequence and Err is set to False. If an error is found, then
   --  P points to the improper character, C is undefined, and Err is
   --  set to True.

   procedure Set_Wide
     (C : Char_Code;
      S : in out String;
      P : in out Natural);
   --  The escape sequence (including any leading ESC character) for the
   --  given character code is stored starting at S (P + 1), and on return
   --  P points to the last stored character (i.e. P is the count of stored
   --  characters on entry and exit, and the escape sequence is appended to
   --  the end of the stored string). The character code C represents a code
   --  originally constructed by Scan_Wide, so it is known to be in a range
   --  that is appropriate for the encoding method in use.

   procedure Skip_Wide (S : String; P : in out Natural);
   --  On entry, S (P) points to an ESC character for a wide character escape
   --  sequence or to an upper half character if the encoding method uses the
   --  upper bit, or to a left bracket if the brackets encoding method is in
   --  use. On exit, P is bumped past the wide character sequence. No error
   --  checking is done, since this is only used on escape sequences generated
   --  by Set_Wide, which are known to be correct.

   procedure Skip_Wide (S : Source_Buffer_Ptr; P : in out Source_Ptr);
   --  Similar to the above procedure, but operates on a source buffer
   --  instead of a string, with P being a Source_Ptr referencing the
   --  contents of the source buffer.

   function Is_Start_Of_Wide_Char
     (S : Source_Buffer_Ptr;
      P : Source_Ptr) return Boolean;
   --  Determines if S (P) is the start of a wide character sequence

end Artics.Widechar;
