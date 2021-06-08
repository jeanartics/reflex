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

with Ada.Containers.Doubly_Linked_Lists;

with Reflex.Boxes; use Reflex.Boxes;
with Reflex.Ladder.Rungs; use Reflex.Ladder.Rungs;

package Reflex.Ladder.Networks is

   type Network_Record is tagged private;
   type Network_Ptr is access all Network_Record;
   type Network_Class_Ptr is access all Network_Record'Class;
   
   package Rungs_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Rung_Class_Ptr);
   
   function New_Network return Network_Ptr;
   
   function Get_Rungs_Lists
     (This : access Network_Record) return Rungs_Lists.List;
   
   procedure Set_Rungs_Lists
     (This : access Network_Record;
      L    : Rungs_Lists.List);
   
   function Get_Page_Height (This : access Network_Record) return Natural;
   procedure Set_Page_Height
     (This : access Network_Record;
      H    : Natural);
   
private
   
   type Network_Record is tagged record
      Rungs : Rungs_Lists.List;
      Page_Height : Natural;
   end record;
			      
   No_Network_Record : constant Network_Record :=
     (Rungs       => Rungs_Lists.Empty_List,
      Page_Height => 0);

end Reflex.Ladder.Networks;
