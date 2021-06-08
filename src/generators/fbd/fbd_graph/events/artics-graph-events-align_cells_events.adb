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

package body Artics.Graph.Events.Align_Cells_Events is
   
   -------------------------
   -- New_Align_Cells_Event --
   -------------------------
   
   function New_Align_Cells_Event return Align_Cells_Event_Ptr is
      Evt : Align_Cells_Event_Ptr :=
	new Align_Cells_Event_Record'(No_Align_Cells_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Align_Cells);
      return Evt;
  end New_Align_Cells_Event;
     
   ---------------------------
   -- New_Align_Cells_Event --
   ---------------------------
   
   function New_Align_Cells_Event
     (Cells : Cells_Lists.List;
      Align : String)
     return Align_Cells_Event_Ptr is
   begin
      return New_Align_Cells_Event (Cells, String_Find (Align)); 
   end New_Align_Cells_Event;
   
   function New_Align_Cells_Event
     (Cells : Cells_Lists.List;
      Align : Name_id)
     return Align_Cells_Event_Ptr is
      
      Evt : Align_Cells_Event_Ptr := New_Align_Cells_Event;
   begin
      Evt.Cells := Cells;
      Evt.Align := Align;
      
      return Evt;
   end New_Align_Cells_Event;
   
   ----------------------------
   -- Free_Align_Cells_Event --
   ----------------------------
   
   procedure Free_Align_Cells_Event (This : in out Align_Cells_Event_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Align_Cells_Event_Record, Align_Cells_Event_Ptr);
   begin
      Free (This);
   end Free_Align_Cells_Event;
   
   ---------------
   -- Get_Cells --
   ---------------
   
   function Get_Cells
     (Evt : access Align_Cells_Event_Record) return Cells_Lists.List is
   begin
      return Evt.Cells;
   end Get_Cells;
   
   ---------------
   -- Set_Cells --
   ---------------
   
   procedure Set_Cells
     (Evt   : access Align_Cells_Event_Record;
      Cells : Cells_Lists.List) is
   begin
      Evt.Cells := Cells;
   end Set_Cells;
   
   ---------------
   -- Get_Align --
   ---------------
   
   function Get_Align
     (Evt : access Align_Cells_Event_Record) return String is
   begin
      return Get_String (Get_Align (Evt));
   end Get_Align;
   
   function Get_Align
     (Evt : access Align_Cells_Event_Record) return Name_Id is
   begin
      return Evt.Align;
   end Get_Align;
   
   ---------------
   -- Set_Align --
   ---------------
   
   procedure Set_Align
     (Evt   : access Align_Cells_Event_Record;
      Align : Name_Id) is
   begin
      Evt.Align := Align;
   end Set_Align;
   
   procedure Set_Align
     (Evt   : access Align_Cells_Event_Record;
      Align : String) is
   begin
      Set_Align (Evt, String_Find (Align));
   end Set_Align;
   
end Artics.Graph.Events.Align_Cells_Events;
