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

--  This package provides an easy way of calling various tools such as gcc,
--  ar, etc...

package MLib.Utl is

   No_Argument_List : constant Argument_List := (1 .. 0 => null);
   --  Comment needed ???

   procedure Delete_File (Filename : in String);
   --  Delete the file Filename
   --  Why is this different from the standard OS_Lib routine???

   procedure Gcc
     (Output_File : String;
      Objects     : Argument_List;
      Options     : Argument_List;
      Driver_Name : Name_Id       := No_Name;
      Options_2   : Argument_List := No_Argument_List);
   --  Driver_Name indicates the "driver" to invoke; by default, the "driver"
   --  is gcc.
   --  This procedure invokes the driver to create a shared library.
   --  Options are passed to gcc before the objects, Options_2 after.
   --  Output_File is the name of the library file to create.
   --  Objects are the names of the object files to put in the library.

   procedure Ar
     (Output_File : String;
      Objects     : Argument_List);
   --  Run ar to move all the binaries inside the archive.
   --  If ranlib is on the path, run it also.
   --  Arguments need documenting ???

   function Lib_Directory return String;
   --  Return the directory containing libgnat

end MLib.Utl;
