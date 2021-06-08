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
-- Reflex is originally developed by Artics
------------------------------------------------------------------------------

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;

-- Implements a 2-dimensional rectangle with double precision coordinates.

package Artics.Graph.Undoables.Changes_Interfaces is
   
   type Change_Type is
     (No_Change,
      Child_Change,
      Geometry_Change,
      Root_Change,
      Terminal_Change,
      Value_Change,
      Visible_Change,
      Select_Change,
      Current_Root_Change);
      
   type Undoable_Change_Interface is interface;
   type Undoable_Change_Class_Ptr is access all Undoable_Change_Interface'Class;
   
   package Undoables_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Undoable_Change_Class_Ptr);
   
   function Get_Change_Type
     (This : access Undoable_Change_Interface) return Change_Type is abstract;
   procedure Set_Change_Type
     (This : access Undoable_Change_Interface;
      Typ  : Change_Type) is abstract;
   
   procedure Execute (Change : access Undoable_Change_Interface) is abstract;
   -- Undoes or redoes the change depending on its undo state.
   
end Artics.Graph.Undoables.Changes_Interfaces;
			       
			       

