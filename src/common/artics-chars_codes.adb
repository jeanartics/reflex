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

package body Artics.Chars_Codes is

   -------------------
   -- Get_Char_Code --
   -------------------

   function Get_Char_Code (C : Character) return Char_Code is
   begin
      return Char_Code'Val (Character'Pos (C));
   end Get_Char_Code;

   -------------------
   -- Get_Character --
   -------------------

   function Get_Character (C : Char_Code) return Character is
   begin
      pragma Assert (C <= 255);
      return Character'Val (C);
   end Get_Character;

   --------------------
   -- Get_Hex_String --
   --------------------
   
   type Word is mod 2 ** 32;
   --  Unsigned 32-bit integer

   subtype Wordh is Word range 0 .. 15;
   Hex : constant array (Wordh) of Character := "0123456789abcdef";
   
   
   function Get_Hex_String (W : Word) return String is
      X  : Word := W;
      WS : String (1 .. 8);

   begin
      for J in reverse 1 .. 8 loop
         WS (J) := Hex (X mod 16);
         X := X / 16;
      end loop;

      return WS;
   end Get_Hex_String;

   ------------------------
   -- Get_Wide_Character --
   ------------------------

   function Get_Wide_Character (C : Char_Code) return Wide_Character is
   begin
      pragma Assert (C <= 65535);
      return Wide_Character'Val (C);
   end Get_Wide_Character;

   ------------------------
   -- In_Character_Range --
   ------------------------

   function In_Character_Range (C : Char_Code) return Boolean is
   begin
      return (C <= 255);
   end In_Character_Range;

   -----------------------------
   -- In_Wide_Character_Range --
   -----------------------------

   function In_Wide_Character_Range (C : Char_Code) return Boolean is
   begin
      return (C <= 65535);
   end In_Wide_Character_Range;

end Artics.Chars_Codes;
