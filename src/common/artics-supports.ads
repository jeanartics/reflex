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

with Interfaces.C; use Interfaces.C;

package Artics.Supports is
   
   function "or"  (Left, Right : Integer) return Integer;
   function "and" (Left, Right : Integer) return Integer;
   function "xor" (Left, Right : Integer) return Integer;
   function "not" (Left        : Integer) return Integer;
   
   function Shift_Left
     (X     : Integer;
      Count : Natural) return Integer;
   function Shift_Right
     (X     : Integer;
      Count : Natural) return Integer;
   
   function Shift_Left
     (X     : Unsigned;
      Count : Natural) return Unsigned;
   function Shift_Right
     (X     : Unsigned;
      Count : Natural) return Unsigned;
   
end Artics.Supports;
