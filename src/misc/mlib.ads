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

--  This package provides the core high level routines used by GNATMLIB
--  and GNATMAKE to build libraries

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Osint;       use Osint;
with Types;       use Types;

package MLib is

   Max_Characters_In_Library_Name : constant := 20;
   --  Maximum number of characters in a library name.
   --  Used by Check_Library_Name below.

   type Fail_Proc is access procedure
     (S1 : String; S2 : String := ""; S3 : String := "");

   Fail : Fail_Proc := Osint.Fail'Access;
   --  This procedure is used in the MLib hierarchy, instead of
   --  directly calling Osint.Fail.
   --  It is redirected to Make.Make_Failed by gnatmake.

   procedure Check_Library_Name (Name : String);
   --  Verify that the name of a library has the following characteristics
   --   - starts with a letter
   --   - includes only letters and digits
   --   - contains not more than Max_Characters_In_Library_Name characters

   procedure Build_Library
     (Ofiles      : Argument_List;
      Afiles      : Argument_List;
      Output_File : String;
      Output_Dir  : String);
   --  Build a static library from a set of object files

   procedure Copy_ALI_Files
     (Files      : Argument_List;
      To         : Name_Id;
      Interfaces : String_List);
   --  Copy all ALI files Files to directory To.
   --  Mark Interfaces ALI files as interfaces, if any.

private

   Preserve : Attribute := Time_Stamps;
   --  Used by Copy_ALI_Files. Changed to None for OpenVMS, because
   --  Copy_Attributes always fails on VMS.

end MLib;
