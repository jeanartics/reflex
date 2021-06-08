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

with Interfaces; use Interfaces;

package body Artics.Supports is
   
   ----------------
   -- Shift_Left --
   ----------------
   
   function Shift_Left
     (X     : Integer;
      Count : Natural) return Integer is
      U : Unsigned;
   begin
      U := Unsigned (X);
      return Integer (Shift_Left (U, Count));
   end Shift_Left;
   
   -----------------
   -- Shift_Right --
   -----------------
   
   function Shift_Right 
     (X     : Integer;
      Count : Natural) return Integer is
   begin
      return Integer (Shift_Right (Unsigned (X), Count));
   end Shift_Right;
   
   ----------------
   -- Shift_Left --
   ----------------
   
   function Shift_Left
     (X     : Unsigned;
      Count : Natural) return Unsigned is
   begin
      return Unsigned (Shift_Left (Unsigned_32 (X), Count));
   end Shift_Left;
   
   -----------------
   -- Shift_Right --
   -----------------
   
   function Shift_Right 
     (X     : Unsigned;
      Count : Natural) return Unsigned is
   begin
      return Unsigned (Shift_Right (Unsigned_32 (X), Count));
   end Shift_Right;
   
   --------
   -- or --
   --------
   
   function "or"(Left, Right: Integer) return Integer is
   begin
      return Integer(Unsigned(Left) or Unsigned(Right));
   end "or";
   
   ---------
   -- and --
   ---------
   
   function "and" (Left, Right: Integer) return Integer is
   begin
      return Integer(Unsigned(Left) and Unsigned(Right));
   end "and";
   
   ---------
   -- xor --
   ---------
   
   function "xor" (Left, Right: Integer) return Integer is
   begin
      return Integer(Unsigned(Left) xor Unsigned(Right));
   end "xor";

   ---------
   -- not --
   ---------
   
   function "not" (Left : Integer) return Integer is
   begin
      return Integer (not Unsigned(Left));
   end "not";
   
end Artics.Supports;
