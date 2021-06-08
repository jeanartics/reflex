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

package body Artics.Graph.Events.Start_Editing_Events is
   
   -----------------------------
   -- New_Start_Editing_Event --
   -----------------------------
   
   function New_Start_Editing_Event return Start_Editing_Event_Ptr is
      Evt : Start_Editing_Event_Ptr := 
	new Start_Editing_Event_Record'(No_Start_Editing_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Start_Editing);
      return Evt;
   end New_Start_Editing_Event;
   
   -----------------------------
   -- New_Start_Editing_Event --
   -----------------------------
   
   function New_Start_Editing_Event
     (Cell : access Cell_Record'Class;
      Evt  : access Mouse_Event_Record'Class)
   return Start_Editing_Event_Ptr is
     
     This : Start_Editing_Event_Ptr := New_Start_Editing_Event;
   begin
      This.Cell := Cell;
      This.Evt  := Evt;
      
      return This;
   end New_Start_Editing_Event;
   
   ------------------------------
   -- Free_Start_Editing_Event --
   ------------------------------
   
   procedure Free_Start_Editing_Event (This : in out Start_Editing_Event_Ptr) 
   is
      procedure Free is new Ada.Unchecked_Deallocation
	(Start_Editing_Event_Record, Start_Editing_Event_Ptr);
   begin
      Free (This);
   end Free_Start_Editing_Event;
   
   --------------
   -- Get_Cell --
   --------------
   
   function Get_Cell
     (This : access Start_Editing_Event_Record)
     return access Cell_Record'Class is
   begin
      return This.Cell;
   end Get_Cell;
   
   --------------
   -- Set_Cell --
   --------------
   
   procedure Set_Cell
     (This : access Start_Editing_Event_Record;
      Cell : access Cell_Record'Class) is
   begin
      This.Cell := Cell;
   end Set_Cell;
   
   ---------------------
   -- Get_Mouse_Event --
   ---------------------
   
   function Get_Mouse_Event
     (This : access Start_Editing_Event_Record)
     return access Mouse_Event_Record'Class is
   begin
      return This.Evt;
   end Get_Mouse_Event;
   
   ---------------------
   -- Set_Mouse_Event --
   ---------------------
   
   procedure Set_Mouse_Event
     (This : access Start_Editing_Event_Record;
      Evt  : access Mouse_Event_Record'Class) is
   begin
      This.Evt := Evt;
   end Set_Mouse_Event;
     
end Artics.Graph.Events.Start_Editing_Events;
