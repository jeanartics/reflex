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

package body Artics.Graph.Events.Update_Cell_Size_Events is
   
   --------------------------------
   -- New_Update_Cell_Size_Event --  
   --------------------------------
  
   function New_Update_Cell_Size_Event return Update_Cell_Size_Event_Ptr is
      Evt : Update_Cell_Size_Event_Ptr := 
	new Update_Cell_Size_Event_Record'(No_Update_Cell_Size_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Update_Cell_Size);
      return Evt;
   end New_Update_Cell_Size_Event;
     
   --------------------------------
   -- New_Update_Cell_Size_Event --  
   --------------------------------
  
   function New_Update_Cell_Size_Event
     (Cell            : access Cell_Record'Class;
      Ignore_Children : Boolean) return Update_Cell_Size_Event_Ptr is
      
      Evt : Update_Cell_Size_Event_Ptr := New_Update_Cell_Size_Event;
   begin
      Evt.Cell            := Cell;
      Evt.Ignore_Children := Ignore_Children;
      
      return Evt;
   end New_Update_Cell_Size_Event;
   
   ---------------------------------
   -- Free_Update_Cell_Size_Event --
   ---------------------------------
   
   procedure Free_Update_Cell_Size_Event 
     (This : in out Update_Cell_Size_Event_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Update_Cell_Size_Event_Record, Update_Cell_Size_Event_Ptr);
   begin
      Free (This);
   end Free_Update_Cell_Size_Event;
     
   --------------
   -- Get_Cell --
   --------------
   
   function Get_Cell
     (Evt : access Update_Cell_Size_Event_Record)
     return access Cell_Record'Class is 
   begin
      return Evt.Cell;
   end Get_Cell;
   
   --------------
   -- Set_Cell --
   --------------
   
   procedure Set_Cell
     (Evt  : access Update_Cell_Size_Event_Record;
      Cell : access Cell_Record'Class) is
   begin
      Evt.Cell := Cell;
   end Set_Cell;
   
   -------------------------
   -- Get_Ignore_Children --
   -------------------------
   
   function Get_Ignore_Children
     (Evt : access Update_Cell_Size_Event_Record) return Boolean is
   begin
      return Evt.Ignore_Children;
   end Get_Ignore_Children;
   
   -------------------------
   -- Set_Ignore_Children --
   -------------------------
   
   procedure Set_Ignore_Children
     (Evt             : access Update_Cell_Size_Event_Record;
      Ignore_Children : Boolean) is
   begin
      Evt.Ignore_Children := Ignore_Children;
   end Set_Ignore_Children;
   
end Artics.Graph.Events.Update_Cell_Size_Events;
