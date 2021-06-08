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

package body Artics.Graph.Events.Cells_Added_Events is
   
   ---------------------------
   -- New_Cells_Added_Event --
   ---------------------------
   
   function New_Cells_Added_Event return Cells_Added_Event_Ptr is
      
      Evt : Cells_Added_Event_Ptr :=
	new Cells_Added_Event_Record'(No_Cells_Added_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Cells_Added);
      return Evt;
  end New_Cells_Added_Event;
     
   -------------------------
   -- New_Cells_Added_Event --
   -------------------------
   
   function New_Cells_Added_Event
     (Cells    : Cells_Lists.List;
      Parent   : access Cell_Record'Class;
      Index    : Integer;
      Source   : access Cell_Record'Class;
      Target   : access Cell_Record'Class;
      Absolute : Boolean) return Cells_Added_Event_Ptr is
      
      Evt : Cells_Added_Event_Ptr := New_Cells_Added_Event;
   begin
      Evt.Cells    := Cells;
      Evt.Parent   := Parent;
      Evt.Index    := Index;
      Evt.Source   :=  Source;
      Evt.Target   := Target;
      Evt.Absolute := Absolute;
      
      return Evt;
   end New_Cells_Added_Event;
   
   ----------------------------
   -- Free_Cells_Added_Event --
   ----------------------------
   
   procedure Free_Cells_Added_Event (This : in out Cells_Added_Event_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Cells_Added_Event_Record, Cells_Added_Event_Ptr);
   begin
      Free (This);
   end Free_Cells_Added_Event;
   
   ---------------
   -- Get_Cells --
   ---------------
   
   function Get_Cells
     (Evt : access Cells_Added_Event_Record) return Cells_Lists.List is
   begin
      return Evt.Cells;
   end Get_Cells;
   
   ---------------
   -- Set_Cells --
   ---------------
   
   procedure Set_Cells
     (Evt   : access Cells_Added_Event_Record;
      Cells : Cells_Lists.List) is
   begin
      Evt.Cells := Cells;
   end Set_Cells;
   
   ----------------
   -- Get_Parent --
   ----------------
   
   function Get_Parent
     (Evt : access Cells_Added_Event_Record) return access Cell_Record'Class
   is
   begin
      return Evt.Parent;
   end Get_Parent;
   
   ----------------
   -- Set_Parent --
   ----------------
   
   procedure Set_Parent
     (Evt    : access Cells_Added_Event_Record;
      Parent : access Cell_Record'Class) is
   begin
      Evt.Parent := Parent;
   end Set_Parent;
   
   ---------------
   -- Get_Index --
   ---------------
   
   function Get_Index
     (Evt : access Cells_Added_Event_Record) return Integer is
   begin
      return Evt.Index;
   end Get_Index;
   
   ---------------
   -- Set_Index --
   ---------------
   
   procedure Set_Index
     (Evt   : access Cells_Added_Event_Record;
      Index : Integer) is
   begin
      Evt.Index := Index;
   end Set_Index;
   
   ----------------
   -- Get_Source --
   ----------------
   
   function Get_Source
     (Evt : access Cells_Added_Event_Record) return access Cell_Record'Class
   is
   begin
      return Evt.Source;
   end Get_Source;
   
   ----------------
   -- Set_Source --
   ----------------
   
   procedure Set_Source
     (Evt    : access Cells_Added_Event_Record;
      Source : access Cell_Record'Class) is
   begin
      Evt.Source := Source;
   end Set_Source;
   
   ----------------
   -- Get_Target --
   ----------------
   
   function Get_Target
     (Evt : access Cells_Added_Event_Record) return access Cell_Record'Class
   is
   begin
      return Evt.Target;
   end Get_Target;
   
   ----------------
   -- Set_Target --
   ----------------
   
   procedure Set_Target
     (Evt    : access Cells_Added_Event_Record;
      Target : access Cell_Record'Class) is
   begin
      Evt.Target := Target;
   end Set_Target;
   
   ------------------
   -- Get_Absolute --
   ------------------
   
   function Get_Absolute 
     (Evt : access Cells_Added_Event_Record) return Boolean is
   begin
      return Evt.Absolute;
   end Get_Absolute;
   
   ------------------
   -- Set_Absolute --
   ------------------
   
   procedure Set_Absolute
     (Evt      : access Cells_Added_Event_Record;
      Absolute : Boolean) is
   begin
      Evt.Absolute := Absolute;
   end Set_Absolute;
   
end Artics.Graph.Events.Cells_Added_Events;
