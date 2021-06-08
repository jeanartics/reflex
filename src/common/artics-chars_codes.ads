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

--  The type Char is used for character data internally in the compiler, but
--  character codes in the source are represented by the Char_Code type.
--  Each character literal in the source is interpreted as being one of the
--  16#7FFF_FFFF# possible Wide_Wide_Character codes, and a unique Integer
--  value is assigned, corresponding to the UTF-32 value, which also
--  corresponds to the Pos value in the Wide_Wide_Character type, and also
--  corresponds to the Pos value in the Wide_Character and Character types
--  for values that are in appropriate range. String literals are similarly
--  interpreted as a sequence of such codes.

package Artics.Chars_Codes is

   type Char_Code_Base is mod 2 ** 32;
   for Char_Code_Base'Size use 32;

   subtype Char_Code is Char_Code_Base range 0 .. 16#7FFF_FFFF#;
   for Char_Code'Value_Size use 32;
   for Char_Code'Object_Size use 32;

   function Get_Char_Code (C : Character) return Char_Code;
   pragma Inline (Get_Char_Code);
   --  Function to obtain internal character code from source character. For
   --  the moment, the internal character code is simply the Pos value of the
   --  input source character, but we provide this interface for possible
   --  later support of alternative character sets.

   function In_Character_Range (C : Char_Code) return Boolean;
   pragma Inline (In_Character_Range);
   --  Determines if the given character code is in range of type Character,
   --  and if so, returns True. If not, returns False.

   function In_Wide_Character_Range (C : Char_Code) return Boolean;
   pragma Inline (In_Wide_Character_Range);
   --  Determines if the given character code is in range of the type
   --  Wide_Character, and if so, returns True. If not, returns False.

   function Get_Character (C : Char_Code) return Character;
   pragma Inline (Get_Character);
   --  For a character C that is in Character range (see above function), this
   --  function returns the corresponding Character value. It is an error to
   --  call Get_Character if C is not in Character range.

   function Get_Wide_Character (C : Char_Code) return Wide_Character;
   --  For a character C that is in Wide_Character range (see above function),
   --  this function returns the corresponding Wide_Character value. It is an
   --  error to call Get_Wide_Character if C is not in Wide_Character range.

end Artics.Chars_Codes;
