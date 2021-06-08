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

with Opt; use Opt;
with Types; use Types;

package body Reflex.Gen.Types is
   
   ----------
   -- Hash --
   ----------
   
   function Hash (N : Node_Id) return Header_Num is
   begin
      return Header_Num (1 + N mod Node_Id (Header_Num'Last));
   end Hash;
   
   ---------------
   -- Name_Hash --
   ---------------
   
   function Name_Hash (N : Name_Id) return Header_Num is
      Nid : Header_Num := Header_Num (N - Name_Id'First);
   begin
      return Header_Num (1 + Nid mod Header_Num'Last);
   end Name_Hash;
   
   ----------------
   -- Initialize --
   ----------------
   
   procedure Initialize is
      Sprint_Line_Limit : Int;
   begin
      Sprint_Line_Limit := 120;

      --  Initialize constants for Write_Uint

      LNegInt  := -(Uint_2 ** (ints - 1));
      LPosInt  := abs (LNegInt + 1);
      LNegLong := -(Uint_2 ** (longs - 1));
      LPosLong := abs (LNegLong + 1);
      LNegLL   := -(Uint_2 ** (lls - 1));
      LPosLL   := abs (LNegLL + 1);

      LPosU    := (Uint_2 ** ints) - 1;
      LNegU    := -LPosU;
      LPosUL   := (Uint_2 ** longs) - 1;
      LNegUL   := -LPosUL;
      LPosULL  := (Uint_2 ** lls) - 1;
      LNegULL  := -LPosULL;
   end Initialize;
   
end Reflex.Gen.Types;
