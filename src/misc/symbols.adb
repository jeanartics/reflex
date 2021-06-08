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

--  This is the default version of this package, used when the creation
--  of symbol files is not supported.

with Ada.Text_IO; use Ada.Text_IO;

package body Symbols is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Symbol_File   : String;
      Reference     : String;
      Symbol_Policy : Policy;
      Quiet         : Boolean;
      Version       : String;
      Success       : out Boolean)
   is
      pragma Unreferenced (Symbol_File);
      pragma Unreferenced (Reference);
      pragma Unreferenced (Symbol_Policy);
      pragma Unreferenced (Quiet);
      pragma Unreferenced (Version);
   begin
      Put_Line
        ("creation of symbol files are not supported on this platform");
      Success := False;
   end Initialize;

   -------------
   -- Process --
   -------------

   procedure Process
     (Object_File : String;
      Success     : out Boolean)
   is
      pragma Unreferenced (Object_File);
   begin
      Success := False;
   end Process;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (Quiet   : Boolean;
      Success : out Boolean)
   is
      pragma Unreferenced (Quiet);
   begin
      Success := False;
   end Finalize;

end Symbols;
