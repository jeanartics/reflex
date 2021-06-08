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

package body Artics.Graph.Events.Start_Events is
   
   -----------------------------
   -- New_Start_Event --
   -----------------------------
   
   function New_Start_Event return Start_Event_Ptr is
      Evt : Start_Event_Ptr := 
	new Start_Event_Record'(No_Start_Event_Record);	
   begin
      Set_Event_Type (Evt, Event_Start);
      return Evt;
   end New_Start_Event;
   
   -----------------------------
   -- New_Start_Event --
   -----------------------------
   
   function New_Start_Event
     (Mouse_Event : access Mouse_Event_Record;
      State       : access Cell_State_Record) return Start_Event_Ptr is
      
      This : Start_Event_Ptr := New_Start_Event;
   begin
      This.Mouse_Event := Mouse_Event;
      This.State := State;
      return This;
   end New_Start_Event;
   
   ----------------------
   -- Free_Start_Event --
   ----------------------
   
   procedure Free_Start_Event (This : in out Start_Event_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Start_Event_Record, Start_Event_Ptr);
   begin
      Free (This);
   end Free_Start_Event;
   
   ---------------
   -- Get_State --
   ---------------
   
   function Get_State
     (This : access Start_Event_Record) return access Cell_State_Record is
   begin
      return This.State;
   end Get_State;
   
   ---------------
   -- Set_State --
   ---------------
   
   procedure Set_State
     (This  : access Start_Event_Record;
      State : access Cell_State_Record) is
   begin
      This.State := State;
   end Set_State;
   
   ---------------------
   -- Get_Mouse_Event --
   ---------------------
   
   function Get_Mouse_Event
     (This : access Start_Event_Record) return access Mouse_Event_Record is
   begin
      return This.Mouse_Event;
   end Get_Mouse_Event;
   
   ---------------------
   -- Set_Mouse_Event --
   ---------------------
   
   procedure Set_Mouse_Event
     (This        : access Start_Event_Record;
      Mouse_Event : access Mouse_Event_Record) is
   begin
      This.Mouse_Event := Mouse_Event;
   end Set_Mouse_Event;
   
end Artics.Graph.Events.Start_Events;
