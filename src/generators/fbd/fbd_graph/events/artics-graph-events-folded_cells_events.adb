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

package body Artics.Graph.Events.Folded_Cells_Events is
   
   -------------------------
   -- New_Folded_Cells_Event --  
   -------------------------
  
   function New_Folded_Cells_Event return Folded_Cells_Event_Ptr is
      Evt : Folded_Cells_Event_Ptr := 
	new Folded_Cells_Event_Record'(No_Folded_Cells_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Cells_Folded);
      return Evt;
   end New_Folded_Cells_Event;
     
   -------------------------
   -- New_Folded_Cells_Event --  
   -------------------------
  
   function New_Folded_Cells_Event
     (Cells    : Cells_Lists.List;
      Collapse : Boolean;
      Recurse  : Boolean)
     return Folded_Cells_Event_Ptr is
      
      Evt : Folded_Cells_Event_Ptr := New_Folded_Cells_Event;
   begin
      Evt.Cells    := Cells;
      Evt.Collapse := Collapse;
      Evt.Recurse  := Recurse;
      
      return Evt;
   end New_Folded_Cells_Event;
   
   -----------------------------
   -- Free_Folded_Cells_Event --
   -----------------------------
   
   procedure Free_Folded_Cells_Event
     (This : in out Folded_Cells_Event_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Folded_Cells_Event_Record, Folded_Cells_Event_Ptr);
   begin
      Free (This);
   end Free_Folded_Cells_Event;
   
   ---------------
   -- Get_Cells --
   ---------------
   
   function Get_Cells
     (Evt : access Folded_Cells_Event_Record) return Cells_Lists.List is
   begin
      return Evt.Cells;
   end Get_Cells;
   
   ---------------
   -- Set_Cells --
   ---------------
   
   procedure Set_Cells
     (Evt  : access Folded_Cells_Event_Record;
      Cells : Cells_Lists.List) is
   begin
      Evt.Cells := Cells;
   end Set_Cells;
   
   ------------------
   -- Get_Collapse --
   ------------------
   
   function Get_Collapse
     (Evt : access Folded_Cells_Event_Record) return Boolean is
   begin
      return Evt.Collapse;
   end Get_Collapse;
   
   ------------------
   -- Set_Collapse --
   ------------------
   
   procedure Set_Collapse
     (Evt      : access Folded_Cells_Event_Record;
      Collapse : Boolean) is
   begin
      Evt.Collapse := Collapse;
   end Set_Collapse;
   
   -----------------
   -- Get_Recurse --
   -----------------
   
   function Get_Recurse
     (Evt : access Folded_Cells_Event_Record) return Boolean is
   begin
      return Evt.Recurse;
   end Get_Recurse;
   
   -----------------
   -- Set_Recurse --
   -----------------
   
   procedure Set_Recurse
     (Evt     : access Folded_Cells_Event_Record;
      Recurse : Boolean) is
   begin
      Evt.Recurse := Recurse;
   end Set_Recurse;
   
end Artics.Graph.Events.Folded_Cells_Events;
