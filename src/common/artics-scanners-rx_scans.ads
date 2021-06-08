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
with Gnat.UTF_32; use Gnat.UTF_32;

with Artics.Types; use Artics.Types;
with Artics.Opt; use Artics.Opt;
with Artics.Widechar; use Artics.Widechar;
with Artics.Namet; use Artics.Namet;
with Artics.Input_Buffers; use Artics.Input_Buffers;
with Artics.Rx_Tokens; use Artics.Rx_Tokens;

package Artics.Scanners.Rx_Scans is
   
   type Rx_Scanner_Record is new Scanner_Record with private;
   type Rx_Scanner_Ptr is access all Rx_Scanner_Record'Class;
   
   function New_Rx_Scanner return Rx_Scanner_Ptr;
   
   procedure Scan (This : access Rx_Scanner_Record);
   
   function Get_Token_Name (This : access Rx_Scanner_Record) return Name_Id;
   
   procedure Scan_Identifier (This : access Rx_Scanner_Record);
   
   procedure Skip_Other_Format_Characters (This : access Rx_Scanner_Record);
   
   procedure Set_Special_Character (C : Character);
   --  Indicate that one of the following character '#', '$', '?', '@', '`',
   --  '\', '^', '_' or '~', when found is a Special token.

   procedure Reset_Special_Characters;
   --  Indicate that there is no characters that are Special tokens., which
   --  is the default.
   
   function Scan_Wide_Character 
     (This : access Rx_Scanner_Record) return Boolean;
   
   procedure Save_Scan_State
     (This : access Rx_Scanner_Record;
      Sav  : in out Rx_Scanner_Record);
   
   procedure Restore_Scan_State 
     (This : access Rx_Scanner_Record;
      Sav  : Rx_Scanner_Record);
   
private
   
   type Rx_Scanner_Record is new Scanner_Record with record
      Token_Name : Name_Id;
      Token_Ptr : Artics.Input_Buffers.Source_Ptr;
      Token : Artics.Rx_Tokens.Token_Type;
      Prev_Token : Artics.Rx_Tokens.Token_Type;
      Prev_Token_Ptr : Artics.Input_Buffers.Source_Ptr;

      Character_Code : Char_Code;
      --  Valid only when Token is Tok_Char_Literal. Contains the value of the
      --  scanned literal.

      Special_Character : Character;
      --  Valid only when Token = Tok_Special. Returns one of the characters
      --  '#', '$', '?', '@', '`', '\', '^', '~', or '_'.
      --
      --  Why only this set? What about wide characters???
      
      
   end record;
   
   No_Rx_Scanner_Record : constant Rx_Scanner_Record :=
     (No_Scanner_Record with
	Token_Name   => No_Name,
      Token          => No_Token,
      Token_Ptr      => 0,
      Prev_Token     => No_Token,
      Prev_Token_Ptr => 0,
      Character_Code => Char_Code'First,
      Special_Character => Character'First);
   
end Artics.Scanners.Rx_Scans;
