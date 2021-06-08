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

package body Artics.Graph.Events.Moved_Cells_Events is
   
   --------------------------
   -- New_Moved_Cells_Event --
   --------------------------
   
   function New_Moved_Cells_Event return Moved_Cells_Event_Ptr is
      Evt : Moved_Cells_Event_Ptr :=
	new Moved_Cells_Event_Record'(No_Moved_Cells_Event_Record);	
   begin
      Set_Event_Type (Evt, Event_Cells_Moved);
      return Evt;
   end New_Moved_Cells_Event;
     
   --------------------------
   -- New_Moved_Cells_Event --
   --------------------------
   
   function New_Moved_Cells_Event
     (Cells      : Cells_Lists.List;
      Dx         : Coordinate;
      Dy         : Coordinate;
      Disconnect : Boolean) return Moved_Cells_Event_Ptr is
      
      Evt : Moved_Cells_Event_Ptr := New_Moved_Cells_Event;
   begin
      Evt.Cells      := Cells;
      Evt.Dx         := Dx;
      Evt.Dy         := Dy;
      Evt.Disconnect := Disconnect;
      
      return Evt;
   end New_Moved_Cells_Event;
   
   ----------------------------
   -- Free_Moved_Cells_Event --
   ----------------------------
   
   procedure Free_Moved_Cells_Event (This : in out Moved_Cells_Event_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Moved_Cells_Event_Record, Moved_Cells_Event_Ptr);
   begin
      Free (This);
   end Free_Moved_Cells_Event;
   
   ---------------
   -- Get_Cells --
   ---------------
   
   function Get_Cells
     (Evt : access Moved_Cells_Event_Record) return Cells_Lists.List is
   begin
      return Evt.Cells;
   end Get_Cells;
   
   ---------------
   -- Set_Cells --
   ---------------
   
   procedure Set_Cells
     (Evt  : access Moved_Cells_Event_Record;
      Cells : Cells_Lists.List) is
   begin
      Evt.Cells := Cells;
   end Set_Cells;
   
   ------------
   -- Get_Dx --
   ------------
   
   function Get_Dx (Evt : access Moved_Cells_Event_Record) return Coordinate is
   begin
      return Evt.Dx;
   end Get_Dx;
   
   ------------
   -- Set_Dx --
   ------------
   
   procedure Set_Dx
     (Evt : access Moved_Cells_Event_Record;
      Dx  : Coordinate) is
   begin
      Evt.Dx := Dx;
   end Set_Dx;
   
   ------------
   -- Get_Dy --
   ------------
   
   function Get_Dy (Evt : access Moved_Cells_Event_Record) return Coordinate is
   begin
      return Evt.Dy;
   end Get_Dy;
   
   ------------
   -- Set_Dy --
   ------------
   
   procedure Set_Dy
     (Evt : access Moved_Cells_Event_Record;
      Dy  : Coordinate) is
   begin
      Evt.Dy := Dy;
   end Set_Dy;
   
   --------------------
   -- Get_Disconnect --
   --------------------
   
   function Get_Disconnect
     (Evt : access Moved_Cells_Event_Record) return Boolean is
   begin
      return Evt.Disconnect;
   end Get_Disconnect;
   
   --------------------
   -- Set_Disconnect --
   --------------------
   
   procedure Set_Disconnect
     (Evt        : access Moved_Cells_Event_Record;
      Disconnect : Boolean) is
   begin
      Evt.Disconnect := Disconnect;
   end Set_Disconnect;
   
end Artics.Graph.Events.Moved_Cells_Events;
