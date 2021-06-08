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

package body Artics.Graph.Events.Connect_Cell_Events is
   
   ----------------------------
   -- New_Connect_Cell_Event --
   ----------------------------
   
   function New_Connect_Cell_Event return Connect_Cell_Event_Ptr is
      
      Evt : Connect_Cell_Event_Ptr :=
	new Connect_Cell_Event_Record'(No_Connect_Cell_Event_Record);	
   begin
      Set_Event_Type (Evt, Event_Connect_Cell);
      return Evt;
  end New_Connect_Cell_Event;
     
   ----------------------------
   -- New_Connect_Cell_Event --
   ----------------------------
   
   function New_Connect_Cell_Event
     (Edge     : access Cell_Interface'Class;
      Terminal : access Cell_Interface'Class;
      Source   : Boolean;
      Previous : access Cell_Interface'Class)
      return Connect_Cell_Event_Ptr is
      
      Evt : Connect_Cell_Event_Ptr := New_Connect_Cell_Event;
   begin
      Evt.Edge     := Edge;
      Evt.Terminal := Terminal;
      Evt.Source   := Source;
      Evt.Previous := Previous;
      
      return Evt;
   end New_Connect_Cell_Event;
   
   -----------------------------
   -- Free_Connect_Cell_Event --
   -----------------------------
   
   procedure Free_Connect_Cell_Event (This : in out Connect_Cell_Event_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Connect_Cell_Event_Record, Connect_Cell_Event_Ptr);
   begin
      Free (This);
   end Free_Connect_Cell_Event;
   
   --------------
   -- Get_Edge --
   --------------
   
   function Get_Edge
     (Evt : access Connect_Cell_Event_Record) 
     return access Cell_Interface'Class is
   begin
      return Evt.Edge;
   end Get_Edge;
   
   --------------
   -- Set_Edge --
   --------------
   
   procedure Set_Edge
     (Evt  : access Connect_Cell_Event_Record;
      Edge : access Cell_Interface'Class) is
   begin
      Evt.Edge := Edge;
   end Set_Edge;
   
   ------------------
   -- Get_Terminal --
   ------------------
   
   function Get_Terminal
     (Evt : access Connect_Cell_Event_Record) 
     return access Cell_Interface'Class is
   begin
      return Evt.Terminal;
   end Get_Terminal;
   
   ------------------
   -- Set_Terminal --
   ------------------
   
   procedure Set_Terminal
     (Evt      : access Connect_Cell_Event_Record;
      Terminal : access Cell_Interface'Class'Class) is
   begin
      Evt.Terminal := Terminal;
   end Set_Terminal;
   
   ----------------
   -- Get_Source --
   ----------------
   
   function Get_Source
     (Evt : access Connect_Cell_Event_Record) return Boolean is
   begin
      return Evt.Source;
   end Get_Source;
   
   ----------------
   -- Set_Source --
   ----------------
   
   procedure Set_Source
     (Evt    : access Connect_Cell_Event_Record;
      Source : Boolean) is
   begin
      Evt.Source := Source;
   end Set_Source;
   
   
   ------------------
   -- Get_Previous --
   ------------------
   
   function Get_Previous
     (Evt : access Connect_Cell_Event_Record) return access Cell_Interface'Class is
   begin
      return Evt.Previous;
   end Get_Previous;
   
   ------------------
   -- Set_Previous --
   ------------------
   
   procedure Set_Previous
     (Evt      : access Connect_Cell_Event_Record;
      Previous : access Cell_Interface'Class) is
   begin
      Evt.Previous := Previous;
   end Set_Previous;
   
end Artics.Graph.Events.Connect_Cell_Events;
