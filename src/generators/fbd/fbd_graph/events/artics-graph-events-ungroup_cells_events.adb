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

package body Artics.Graph.Events.Ungroup_Cells_Events is
   
   ---------------------------
   -- New_Ungroup_Cells_Event --  
   ---------------------------
  
   function New_Ungroup_Cells_Event return Ungroup_Cells_Event_Ptr is
      Evt : Ungroup_Cells_Event_Ptr := 
	new Ungroup_Cells_Event_Record'(No_Ungroup_Cells_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Ungroup_Cells);
      return Evt;
   end New_Ungroup_Cells_Event;
     
   -------------------------
   -- New_Ungroup_Cells_Event --  
   -------------------------
  
   function New_Ungroup_Cells_Event
     (Cells  : Cells_Lists.List) return Ungroup_Cells_Event_Ptr is
      
      Evt : Ungroup_Cells_Event_Ptr := New_Ungroup_Cells_Event;
   begin
      Evt.Cells  := Cells;
      
      return Evt;
   end New_Ungroup_Cells_Event;
   
   ------------------------------
   -- Free_Ungroup_Cells_Event --
   ------------------------------
   
   procedure Free_Ungroup_Cells_Event (This : in out Ungroup_Cells_Event_Ptr)
   is
     procedure Free is new Ada.Unchecked_Deallocation
       (Ungroup_Cells_Event_Record, Ungroup_Cells_Event_Ptr);
   begin
      Free (This);
   end Free_Ungroup_Cells_Event;
   
   ---------------
   -- Get_Cells --
   ---------------
   
   function Get_Cells
     (Evt : access Ungroup_Cells_Event_Record) return Cells_Lists.List is
   begin
      return Evt.Cells;
   end Get_Cells;
   
   ---------------
   -- Set_Cells --
   ---------------
   
   procedure Set_Cells
     (Evt  : access Ungroup_Cells_Event_Record;
      Cells : Cells_Lists.List) is
   begin
      Evt.Cells := Cells;
   end Set_Cells;
   
end Artics.Graph.Events.Ungroup_Cells_Events;
