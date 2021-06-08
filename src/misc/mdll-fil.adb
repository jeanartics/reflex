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

--  Simple services used by GNATDLL to deal with Filename extension.

with Ada.Strings.Fixed;

package body MDLL.Fil is

   use Ada;

   -------------
   -- Get_Ext --
   -------------

   function Get_Ext (Filename : String) return String is
      use Strings.Fixed;
      I : constant Natural := Index (Filename, ".", Strings.Backward);
   begin
      if I = 0 then
         return "";
      else
         return Filename (I .. Filename'Last);
      end if;
   end Get_Ext;

   ------------
   -- Is_Ali --
   ------------

   function Is_Ali (Filename : String) return Boolean is
   begin
      return Get_Ext (Filename) = ".ali";
   end Is_Ali;

   ------------
   -- Is_Obj --
   ------------

   function Is_Obj (Filename : String) return Boolean is
      Ext : constant String := Get_Ext (Filename);
   begin
      return Ext = ".o" or else Ext = ".obj";
   end Is_Obj;

   ------------
   -- Ext_To --
   ------------

   function Ext_To
     (Filename : String;
      New_Ext  : String := No_Ext)
      return     String
   is
      use Strings.Fixed;
      I : constant Natural := Index (Filename, ".", Strings.Backward);
   begin
      if I = 0 then
         return Filename;
      else
         if New_Ext = "" then
            return Head (Filename, I - 1);
         else
            return Head (Filename, I - 1) & '.' & New_Ext;
         end if;
      end if;
   end Ext_To;

end MDLL.Fil;
