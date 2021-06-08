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

package body Artics.Graph.Events.Group_Cells_Events is
   
   ---------------------------
   -- New_Group_Cells_Event --  
   ---------------------------
  
   function New_Group_Cells_Event return Group_Cells_Event_Ptr is
      Evt : Group_Cells_Event_Ptr := 
	new Group_Cells_Event_Record'(No_Group_Cells_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Group_Cells);
      return Evt;
   end New_Group_Cells_Event;
     
   -------------------------
   -- New_Group_Cells_Event --  
   -------------------------
  
   function New_Group_Cells_Event
     (Group  : access Cell_Record'Class;
      Border : Coordinate;
      Cells  : Cells_Lists.List) return Group_Cells_Event_Ptr is
      
      Evt : Group_Cells_Event_Ptr := New_Group_Cells_Event;
   begin
      Evt.Group  := Group;
      Evt.Border := Border;
      Evt.Cells  := Cells;
      
      return Evt;
   end New_Group_Cells_Event;
   
   ----------------------------
   -- Free_Group_Cells_Event --
   ----------------------------
   
   procedure Free_Group_Cells_Event (This : in out Group_Cells_Event_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Group_Cells_Event_Record, Group_Cells_Event_Ptr);
   begin
      Free (This);
   end Free_Group_Cells_Event;
   
   ---------------
   -- Get_Group --
   ---------------
   
   function Get_Group
     (Evt : access Group_Cells_Event_Record) return access Cell_Record'Class is
   begin
      return Evt.Group;
   end Get_Group;
   
   ---------------
   -- Set_Group --
   ---------------
   
   procedure Set_Group
     (Evt   : access Group_Cells_Event_Record;
      Group : access Cell_Record'Class) is
   begin
      Evt.Group := Group;
   end Set_Group;
   
   ---------------
   -- Get_Boder --
   ---------------
   
   function Get_Border
     (Evt : access Group_Cells_Event_Record) return Coordinate is
   begin
      return Evt.Border;
   end Get_Border;
   
   ----------------
   -- Set_Border --
   ----------------
   
   procedure Set_Border
     (Evt    : access Group_Cells_Event_Record;
      Border : Coordinate) is
   begin
      Evt.Border := Border;
   end Set_Border;
   
   ---------------
   -- Get_Cells --
   ---------------
   
   function Get_Cells
     (Evt : access Group_Cells_Event_Record) return Cells_Lists.List is
   begin
      return Evt.Cells;
   end Get_Cells;
   
   ---------------
   -- Set_Cells --
   ---------------
   
   procedure Set_Cells
     (Evt  : access Group_Cells_Event_Record;
      Cells : Cells_Lists.List) is
   begin
      Evt.Cells := Cells;
   end Set_Cells;
   
end Artics.Graph.Events.Group_Cells_Events;
