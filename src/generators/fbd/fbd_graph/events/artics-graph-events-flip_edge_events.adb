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

package body Artics.Graph.Events.Flip_Edge_Events is
   
   -------------------------
   -- New_Flip_Edge_Event --
   -------------------------
   
   function New_Flip_Edge_Event return Flip_Edge_Event_Ptr is
      Evt : Flip_Edge_Event_Ptr := 
	new Flip_Edge_Event_Record'(No_Flip_Edge_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Flip_Edge);
      return Evt;
  end New_Flip_Edge_Event;
     
   -------------------------
   -- New_Flip_Edge_Event --
   -------------------------
   
   function New_Flip_Edge_Event
     (Edge : access Cell_INterface'Class) return Flip_Edge_Event_Ptr is
      
      Evt : Flip_Edge_Event_Ptr := New_Flip_Edge_Event;
   begin
      Evt.Edge := Edge;
      
      return Evt;
   end New_Flip_Edge_Event;
   
   --------------------------
   -- Free_Flip_Edge_Event --
   --------------------------
   
   procedure Free_Flip_Edge_Event (This : in out Flip_Edge_Event_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Flip_Edge_Event_Record, Flip_Edge_Event_Ptr);
   begin
      Free (This);
   end Free_Flip_Edge_Event;
   
   --------------
   -- Get_Edge --
   --------------
   
   function Get_Edge
     (Evt : access Flip_Edge_Event_Record) return access Cell_Interface'Class is
   begin
      return Evt.Edge;
   end Get_Edge;
   
   --------------
   -- Set_Edge --
   --------------
   
   procedure Set_Edge
     (Evt  : access Flip_Edge_Event_Record;
      Edge : access Cell_Interface'Class) is
   begin
      Evt.Edge := Edge;
   end Set_Edge;
   
end Artics.Graph.Events.Flip_Edge_Events;
