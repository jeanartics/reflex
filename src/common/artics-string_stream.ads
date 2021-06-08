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

--  This package provides an example of an Ada stream that reads or
--  writes to an in-memory string. This can be used when printing a
--  DOM tree for instance (see tostring.adb)

with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Artics.String_Stream is

   type String_Stream_Type is new Root_Stream_Type with record
      Str        : Unbounded_String;
      Read_Index : Natural := 1;
   end record;
   procedure Read
     (Stream : in out String_Stream_Type;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset);
   procedure Write
      (Stream : in out String_Stream_Type;
       Item   : Stream_Element_Array);
   procedure Open
      (Stream : in out String_Stream_Type'Class;
       Str    : String);
   --  Declare a new stream type to write strings in memory

end Artics.String_Stream;
