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

--  Simple services used by GNATDLL to deal with Filename extension

package MDLL.Fil is

   No_Ext : constant String := "";
   --  Used to mark the absence of an extension

   function Get_Ext (Filename : String) return String;
   --  Return extension of Filename

   function Is_Ali (Filename : String) return Boolean;
   --  Test if Filename is an Ada library file (.ali).

   function Is_Obj (Filename : String) return Boolean;
   --  Test if Filename is an object file (.o or .obj)

   function Ext_To
     (Filename : String;
      New_Ext  : String := No_Ext)
      return     String;
   --  Return Filename with the extension change to New_Ext

end MDLL.Fil;
