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

package body Artics.Graph.Events.Ordered_Cells_Events is
   
   -----------------------------
   -- New_Ordered_Cells_Event --  
   -----------------------------
  
   function New_Ordered_Cells_Event return Ordered_Cells_Event_Ptr is
      Evt : Ordered_Cells_Event_Ptr :=
	new Ordered_Cells_Event_Record'(No_Ordered_Cells_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Cells_Ordered);
      return Evt;
   end New_Ordered_Cells_Event;
     
   -------------------------
   -- New_Ordered_Cells_Event --  
   -------------------------
  
   function New_Ordered_Cells_Event
     (Back  : Boolean;
      Cells : Cells_Lists.List) return Ordered_Cells_Event_Ptr is
      
      Evt : Ordered_Cells_Event_Ptr := New_Ordered_Cells_Event;
   begin
      Evt.Back  := Back;
      Evt.Cells := Cells;
      
      return Evt;
   end New_Ordered_Cells_Event;
   
   ------------------------------
   -- Free_Ordered_Cells_Event --
   ------------------------------
   
   procedure Free_Ordered_Cells_Event
     (This : in out Ordered_Cells_Event_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Ordered_Cells_Event_Record, Ordered_Cells_Event_Ptr);
   begin
      Free (This);
   end Free_Ordered_Cells_Event;
      
   ---------------
   -- Get_Cells --
   ---------------
   
   function Get_Cells
     (Evt : access Ordered_Cells_Event_Record) return Cells_Lists.List is
   begin
      return Evt.Cells;
   end Get_Cells;
   
   ---------------
   -- Set_Cells --
   ---------------
   
   procedure Set_Cells
     (Evt  : access Ordered_Cells_Event_Record;
      Cells : Cells_Lists.List) is
   begin
      Evt.Cells := Cells;
   end Set_Cells;
   
   --------------
   -- Get_Back --
   --------------
   
   function Get_Back
     (Evt : access Ordered_Cells_Event_Record) return Boolean is
   begin
      return Evt.Back;
   end Get_Back;
   
   --------------
   -- Set_Back --
   --------------
   
   procedure Set_Back
     (Evt  : access Ordered_Cells_Event_Record;
      Back : Boolean) is
   begin
      Evt.Back := Back;
   end Set_Back;
   
end Artics.Graph.Events.Ordered_Cells_Events;
