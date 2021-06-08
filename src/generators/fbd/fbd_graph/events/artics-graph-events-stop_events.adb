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

package body Artics.Graph.Events.Stop_Events is
   
   -----------------------------
   -- New_Stop_Event --
   -----------------------------
   
   function New_Stop_Event return Stop_Event_Ptr is
      Evt : Stop_Event_Ptr := 
	new Stop_Event_Record'(No_Stop_Event_Record);	
   begin
      Set_Event_Type (Evt, Event_Stop);
      return Evt;
   end New_Stop_Event;
   
   -----------------------------
   -- New_Stop_Event --
   -----------------------------
   
   function New_Stop_Event
     (Evt    : access Mouse_Event_Record;
      Commit : Boolean;
      Obj    : access Object_Record'Class := null)
     return Stop_Event_Ptr is
      
      This : Stop_Event_Ptr := New_Stop_Event;
   begin
      This.Evt    := Evt;
      This.Commit := Commit;
      This.Obj    := Obj;
      return This;
   end New_Stop_Event;
   
   ---------------------
   -- Free_Stop_Event --
   ---------------------
   
   procedure Free_Stop_Event (This : in out Stop_Event_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Stop_Event_Record, Stop_Event_Ptr);
   begin
      Free (This);
   end Free_Stop_Event;
   
   ---------------------
   -- Get_Mouse_Event --
   ---------------------
   
   function Get_Mouse_Event
     (This : access Stop_Event_Record) return access Mouse_Event_Record is
   begin
      return This.Evt;
   end Get_Mouse_Event;
   
   ---------------------
   -- Set_Mouse_Event --
   ---------------------
   
   procedure Set_Mouse_Event
     (This : access Stop_Event_Record;
      Evt  : access Mouse_Event_Record) is
   begin
      This.Evt := Evt;
   end Set_Mouse_Event;
   
   ----------------
   -- Get_Commit --
   ----------------
   
   function Get_Commit
     (This : access Stop_Event_Record) return Boolean is
   begin
      return This.Commit;
   end Get_Commit;
   
   ----------------
   -- Set_Commit --
   ----------------
   
   procedure Set_Commit
     (This   : access Stop_Event_Record;
      Commit : Boolean) is
   begin
      This.Commit := Commit;
   end Set_Commit;
   
   ---------------------
   -- Get_User_Object --
   ---------------------
   
   function Get_User_Object
     (This : access Stop_Event_Record) return access Object_Record'Class is
   begin
      return This.Obj;
   end Get_User_Object;
   
   ---------------------
   -- Set_User_Object --
   ---------------------
   
   procedure Set_User_Object
     (This : access Stop_Event_Record;
      Obj  : access Object_Record'Class) is
   begin
      This.Obj := Obj;
   end Set_User_Object;
   
end Artics.Graph.Events.Stop_Events;
