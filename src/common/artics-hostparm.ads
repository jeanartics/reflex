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

--  This package defines some system dependent parameters for GNAT. These
--  are parameters that are relevant to the host machine on which the
--  compiler is running, and thus this package is part of the compiler.

package Artics.Hostparm is
-- pragma Pure (Hostparm);

   ---------------------
   -- HOST Parameters --
   ---------------------

   Normalized_CWD : constant String := "./";
   --  Normalized string to access current directory

   Max_Line_Length : constant := 255;
   --  Maximum source line length. This can be set to any value up to
   --  2**15 - 1, a limit imposed by the assumption that column numbers
   --  can be stored in 16 bits (see Types.Column_Number). A value of
   --  200 is the minimum value required (RM 2.2(15)), but we use 255
   --  for most GNAT targets since this is DEC Ada compatible.

   Max_Name_Length : constant := 8192; --4096;
   --  Maximum length of unit name (including all dots, and " (spec)") and
   --  of file names in the library, must be at least Max_Line_Length, but
   --  can be larger.

   Max_Instantiations : constant := 4000;
   --  Maximum number of instantiations permitted (to stop runaway cases
   --  of nested instantiations). These situations probably only occur in
   --  specially concocted test cases.

end Artics.Hostparm;
