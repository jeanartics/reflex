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

--  This package builds a library for a library project file

with Prj; use Prj;

package MLib.Prj is

   procedure Build_Library
     (For_Project   : Project_Id;
      Gnatbind      : String;
      Gnatbind_Path : String_Access;
      Gcc           : String;
      Gcc_Path      : String_Access;
      Bind          : Boolean := True;
      Link          : Boolean := True);
   --  Build the library of library project For_Project.
   --  Fails if For_Project is not a library project file.
   --  Gnatbind, Gnatbind_Path, Gcc, Gcc_Path are used for standalone
   --  libraries, to call the binder and to compile the binder generated
   --  files. If Bind is False the binding of a stand-alone library is skipped.
   --  If Link is False, the library is not linked/built.

   procedure Check_Library (For_Project : Project_Id);
   --  Check if the library of a library project needs to be rebuilt,
   --  because its time-stamp is earlier than the time stamp of one of its
   --  object files.

end MLib.Prj;
