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

--  This package is used by gnatmake and by the Project Manager to create
--  temporary files.  If environment variable TMPDIR is defined and
--  designates an absolute path, temporary files are create in this directory.
--  Otherwise, temporary files are created in the current working directory.

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Types;       use Types;

package Tempdir is

   procedure Create_Temp_File
     (FD   : out File_Descriptor;
      Name : out Name_Id);
   --  Create a temporary text file and return its file descriptor and
   --  its path name as a Name_Id. If environment variable TMPDIR is defined
   --  and its value is an absolute path, the temp file is created in the
   --  directory designated by TMPDIR, otherwise, it is created in the current
   --  directory. If temporary file cannot be created, FD gets the value
   --  Invalid_FD and Name gets the value No_Name.

end Tempdir;
