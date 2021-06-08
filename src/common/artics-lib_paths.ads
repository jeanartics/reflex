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

with Artics.Containers.Lists;
with Artics.Types; use Artics.Types;

package Artics.Lib_Paths is

   File_Not_Found : exception;

   type Path_Entry_Record is record
      Str : String_Ptr;
      Ref : Natural;
   end record;

   type Path_Entry is access all Path_Entry_Record;

   procedure Initialize;

   procedure Add_New_Lib_Path
     (S     : in String;
      First : in Boolean := False);
   -- Call mainly after command line analysis. This function add a new path to
   -- the list of path checked each time a file is required. When First is
   -- true, the path is add to the front of the list.

   procedure Remove_Lib_Path (S : in String);

   function Resolve_Filename (S : in String) return String;
   -- Call when access to a file is required. It searches this in the list of
   -- Paths. First found, first returned.

   procedure Dump_Library_Path;
   -- Mainly for debug purposes, print the list of paths

   procedure Split_Path_Filename
     (PF : in String;
      P  : out String_Ptr;
      F  : out String_Ptr);

private

   package Path_List is new Containers.Lists(Path_Entry, null);
   use Path_List;

   Library_Path : Path_List.List_Id;

end Artics.Lib_Paths;
