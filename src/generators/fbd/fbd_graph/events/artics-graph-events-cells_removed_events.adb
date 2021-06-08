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

with Ada.Unchecked_Deallocation;

package body Artics.Graph.Events.Cells_Removed_Events is
   
   -----------------------------
   -- New_Cells_Removed_Event --
   -----------------------------
   
   function New_Cells_Removed_Event return Cells_Removed_Event_Ptr is
      Evt : Cells_Removed_Event_Ptr :=
	new Cells_Removed_Event_Record'(No_Cells_Removed_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Remove_Cells);
      return Evt;
   end New_Cells_Removed_Event;
   
   -----------------------------
   -- New_Cells_Removed_Event --
   -----------------------------
   
   function New_Cells_Removed_Event
     (Cells : Cells_Lists.List) return Cells_Removed_Event_Ptr is
      
      This : Cells_Removed_Event_Ptr := New_Cells_Removed_Event;
   begin
      This.Cells := Cells;
      
      return This;
   end New_Cells_Removed_Event;
   
   -----------------------------
   -- Free_Cells_Removed_Event --
   -----------------------------
   
   procedure Free_Cells_Removed_Event (This : in out Cells_Removed_Event_Ptr)
   is
      procedure Free is new Ada.Unchecked_Deallocation
	(Cells_Removed_Event_Record, Cells_Removed_Event_Ptr);
   begin
      Free (This);
   end Free_Cells_Removed_Event;
   
   ---------------
   -- Get_Cells --
   ---------------
   
   function Get_Cells
     (Evt : access Cells_Removed_Event_Record) return Cells_Lists.List is
   begin
      return Evt.Cells;
   end Get_Cells;
   
   ---------------
   -- Set_Cells --
   ---------------
   
   procedure Set_Cells
     (Evt   : access Cells_Removed_Event_Record;
      Cells : Cells_Lists.List) is
   begin
      Evt.Cells := Cells;
   end Set_Cells;
   
end Artics.Graph.Events.Cells_Removed_Events;
