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

package body Artics.Graph.Events.Execute_Events is
   
   -----------------------
   -- New_Execute_Event --
   -----------------------
   
   function New_Execute_Event return Execute_Event_Ptr is
      Evt : Execute_Event_Ptr :=
	new Execute_Event_Record'(No_Execute_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Execute);
      return Evt;
   end New_Execute_Event;
   
   -----------------------
   -- New_Execute_Event --
   -----------------------
   
   function New_Execute_Event
     (Change : access Model_Change_Record'Class) return Execute_Event_Ptr is
      Ex : Execute_Event_Ptr := New_Execute_Event;
   begin
      Ex.Change := Change;
      
      return Ex;
   end New_Execute_Event;
   
   -----------------------
   -- New_Execute_Event --
   -----------------------
   
   function New_Execute_Event
     (Change : access Model_Change_Record'Class;
      Name   : Name_id) return Execute_Event_Ptr is
      
      Ex : Execute_Event_Ptr := New_Execute_Event;
   begin
      Ex.Change := Change;
      Ex.Name   := Name;
      
      return Ex;
   end New_Execute_Event;
   
   ------------------------
   -- Free_Execute_Event --
   ------------------------
   
   procedure Free_Execute_Event (This : in out Execute_Event_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Execute_Event_Record, Execute_Event_Ptr);
   begin
      Free (This);
   end Free_Execute_Event;
   
   ----------------
   -- Get_Change --
   ----------------
   
   function Get_Change
     (This : access Execute_Event_Record) 
     return access Model_Change_Record'Class is
   begin
      return This.Change;
   end Get_Change;
   
   ----------------
   -- Set_Change --
   ----------------
   
   procedure Set_Change 
     (This   : access Execute_Event_Record;
      Change : access Model_Change_Record'Class) is
   begin
      This.Change := Change;
   end Set_Change;
   
   --------------
   -- Get_Name --
   --------------
   
   function Get_Name (This : access Execute_Event_Record) return Name_Id is
   begin
      return This.Name;
   end Get_Name;
   
   --------------
   -- Set_Name --
   --------------
   
   procedure Set_Name
     (This : access Execute_Event_Record;
      Name : Name_Id) is
   begin
      This.Name := Name;
   end Set_Name;
   
   
     
end Artics.Graph.Events.Execute_Events;
