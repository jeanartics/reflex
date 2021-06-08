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

package body Artics.Graph.Events.Add_Cells_Events is
   
   -------------------------
   -- New_Add_Cells_Event --  
   -------------------------
  
   function New_Add_Cells_Event return Add_Cells_Event_Ptr is
      This : Add_Cells_Event_Ptr := 
	new Add_Cells_Event_Record'(No_Add_Cells_Event_Record);
   begin
      Set_Event_Type (This, Event_Add_Cells);
      return This;
   end New_Add_Cells_Event;
     
   -------------------------
   -- New_Add_Cells_Event --  
   -------------------------
  
   function New_Add_Cells_Event
     (Cells  : Cells_Lists.List;
      Parent : access Cell_Record'Class;
      Index  : Integer;
      Source : access Cell_Record'Class;
      Target : access Cell_Record'Class)
     return Add_Cells_Event_Ptr is
      
      Evt : Add_Cells_Event_Ptr := New_Add_Cells_Event;
   begin
      Evt.Cells  := Cells;
      Evt.Parent := Parent;
      Evt.Index  := Index;
      Evt.Source := Source;
      Evt.Target := Target;
      
      return Evt;
   end New_Add_Cells_Event;
   
   --------------------------
   -- Free_Add_Cells_Event --
   --------------------------
   
   procedure Free_Add_Cells_Event (This : in out Add_Cells_Event_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Add_Cells_Event_Record, Add_Cells_Event_Ptr);
   begin
      Free (This);
   end Free_Add_Cells_Event;
   
   ---------------
   -- Get_Cells --
   ---------------
   
   function Get_Cells
     (Evt : access Add_Cells_Event_Record) return Cells_Lists.List is
   begin
      return Evt.Cells;
   end Get_Cells;
   
   ---------------
   -- Set_Cells --
   ---------------
   
   procedure Set_Cells
     (Evt  : access Add_Cells_Event_Record;
      Cells : Cells_Lists.List) is
   begin
      Evt.Cells := Cells;
   end Set_Cells;
   
   ----------------
   -- Get_Parent --
   ----------------
   
   function Get_Parent
     (Evt : access Add_Cells_Event_Record) return access Cell_Record'Class is
   begin
      return Evt.Parent;
   end Get_Parent;
   
   ----------------
   -- Set_Parent --
   ----------------
   
   procedure Set_Parent
     (Evt    : access Add_Cells_Event_Record;
      Parent : access Cell_Record'Class) is
   begin
      Evt.Parent := Parent;
   end Set_Parent;
   
   ---------------
   -- Get_Index --
   ---------------
   
   function Get_Index
     (Evt : access Add_Cells_Event_Record) return Integer is
   begin
      return Evt.Index;
   end Get_Index;
   
   ---------------
   -- Set_Index --
   ---------------
   
   procedure Set_Index
     (Evt   : access Add_Cells_Event_Record;
      Index : Integer) is
   begin
      Evt.Index := Index;
   end Set_Index;
   
   ----------------
   -- Get_Source --
   ----------------
   
   function Get_Source
     (Evt : access Add_Cells_Event_Record) return access Cell_Record'Class is
   begin
      return Evt.Source;
   end Get_Source;
   
   ----------------
   -- Set_Source --
   ----------------
   
   procedure Set_Source
     (Evt    : access Add_Cells_Event_Record;
      Source : access Cell_Record'Class) is
   begin
      Evt.Source := Source;
   end Set_Source;
   
   ----------------
   -- Get_Target --
   ----------------
   
   function Get_Target
     (Evt : access Add_Cells_Event_Record) return access Cell_Record'Class is
   begin
      return Evt.Target;
   end Get_Target;
   
   ----------------
   -- Set_Target --
   ----------------
   
   procedure Set_Target
     (Evt    : access Add_Cells_Event_Record;
      Target : access Cell_Record'Class) is
   begin
      Evt.Target := Target;
   end Set_Target;
   
end Artics.Graph.Events.Add_Cells_Events;
