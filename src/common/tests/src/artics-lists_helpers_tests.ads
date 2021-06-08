------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 2015-2016, Free Software Foundation, Inc.         --
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
-- Reflex is originally developed by Artics
------------------------------------------------------------------------------

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;

with Artics.Lists_Helpers;

package Artics.Lists_Helpers_Tests is
   
   No_Elmt : Integer := 0;
   
   package Integers_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Integer);
   
   package Integers_Lists_Helpers is new Artics.Lists_Helpers
     (Element_Type => Integer,
      No_Element   => No_Elmt,
      Lists        => Integers_Lists);
   
end Artics.Lists_Helpers_Tests;
