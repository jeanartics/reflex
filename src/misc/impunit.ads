------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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
-- Reflex is a fork from the GNAT compiler. GNAT was originally developed   --
-- by the GNAT team at  New York University. Extensive  contributions to    --
-- GNAT were provided by Ada Core Technologies Inc. Reflex is developed  by --
-- the Artics team at Grenoble.                                             --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains data and functions used to determine if a given
--  unit is an internal unit intended only for use by the implementation
--  and which should not be directly WITH'ed by user code.

with Types; use Types;

package Impunit is

   function Implementation_Unit (U : Unit_Number_Type) return Boolean;
   --  Given the unit number of a unit, this function determines if it is a
   --  unit that is intended to be used only internally by the implementation.
   --  This is used for posting warnings for improper WITH's of such units
   --  (such WITH's are allowed without warnings only in GNAT_Mode set by
   --  the use of -gnatg). True is returned if a warning should be posted.

end Impunit;
