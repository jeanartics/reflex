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

package body Artics.Graph.Events.Rezised_Cells_Events is
   
   -------------------------
   -- New_Rezised_Cells_Event --  
   -------------------------
  
   function New_Rezised_Cells_Event return Rezised_Cells_Event_Ptr is
      Evt : Rezised_Cells_Event_Ptr := 
	new Rezised_Cells_Event_Record'(No_Rezised_Cells_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Cells_Resized);
      return Evt;
   end New_Rezised_Cells_Event;
     
   -------------------------
   -- New_Rezised_Cells_Event --  
   -------------------------
  
   function New_Rezised_Cells_Event
     (Cells  : Cells_Lists.List;
      Bounds : Rectangles_Lists.List) return Rezised_Cells_Event_Ptr is
      
      Evt : Rezised_Cells_Event_Ptr := New_Rezised_Cells_Event;
   begin
      Evt.Cells  := Cells;
      Evt.Bounds := Bounds;
      
      return Evt;
   end New_Rezised_Cells_Event;
   
   -------------------------
   -- New_Rezised_Cells_Event --  
   -------------------------
  
   function New_Rezised_Cells_Event return Rezised_Cells_Event_Record is
      Evt : Rezised_Cells_Event_Record := No_Rezised_Cells_Event_Record;
   begin
      Set_Event_Type (Evt, Event_Cells_Resized);
      return Evt;
   end New_Rezised_Cells_Event;
     
   -------------------------
   -- New_Rezised_Cells_Event --  
   -------------------------
  
   function New_Rezised_Cells_Event
     (Cells  : Cells_Lists.List;
      Bounds : Rectangles_Lists.List) return Rezised_Cells_Event_Record is
      
      Evt : Rezised_Cells_Event_Record := New_Rezised_Cells_Event;
   begin
      Evt.Cells  := Cells;
      Evt.Bounds := Bounds;
      
      return Evt;
   end New_Rezised_Cells_Event;
   
   ---------------
   -- Get_Cells --
   ---------------
   
   function Get_Cells
     (Evt : access Rezised_Cells_Event_Record) return Cells_Lists.List is
   begin
      return Evt.Cells;
   end Get_Cells;
   
   ---------------
   -- Set_Cells --
   ---------------
   
   procedure Set_Cells
     (Evt  : access Rezised_Cells_Event_Record;
      Cells : Cells_Lists.List) is
   begin
      Evt.Cells := Cells;
   end Set_Cells;
   
   ----------------
   -- Get_Bounds --
   ----------------
   
   function Get_Bounds
     (Evt : access Rezised_Cells_Event_Record) return Rectangles_Lists.List is
   begin
      return Evt.Bounds;
   end Get_Bounds;
   
   ----------------
   -- Set_Bounds --
   ----------------
   
   procedure Set_Bounds
     (Evt    : access Rezised_Cells_Event_Record;
      Bounds : Rectangles_Lists.List) is
   begin
      Evt.Bounds := Bounds;
   end Set_Bounds;
   
   
end Artics.Graph.Events.Rezised_Cells_Events;
