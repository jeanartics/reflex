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

package body Artics.Graph.Events.Move_Cells_Events is
   
   --------------------------
   -- New_Move_Cells_Event --
   --------------------------
   
   function New_Move_Cells_Event return Move_Cells_Event_Ptr is
      Evt : Move_Cells_Event_Ptr :=
	new Move_Cells_Event_Record'(No_Move_Cells_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Move_Cells);
      return Evt;
   end New_Move_Cells_Event;
     
   --------------------------
   -- New_Move_Cells_Event --
   --------------------------
   
   function New_Move_Cells_Event
     (Cells    : Cells_Lists.List;
      Dx       : Coordinate;
      Dy       : Coordinate;
      Clone    : Boolean;
      Target   : access Cell_Record'Class;
      Location : Point_Record) return Move_Cells_Event_Ptr is
      
      Evt : Move_Cells_Event_Ptr := New_Move_Cells_Event;
   begin
      Evt.Cells    := Cells;
      Evt.Dx       := Dx;
      Evt.Dy       := Dy;
      Evt.Clone    := Clone;
      Evt.Target   := Target;
      Evt.Location := Location; 
      
      return Evt;
   end New_Move_Cells_Event;
   
   ---------------------------
   -- Free_Move_Cells_Event --
   ---------------------------
   
   procedure Free_Move_Cells_Event (This : in out Move_Cells_Event_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Move_Cells_Event_Record, Move_Cells_Event_Ptr);
   begin
      Free (This);
   end Free_Move_Cells_Event;
   
   ---------------
   -- Get_Cells --
   ---------------
   
   function Get_Cells
     (Evt : access Move_Cells_Event_Record) return Cells_Lists.List is
   begin
      return Evt.Cells;
   end Get_Cells;
   
   ---------------
   -- Set_Cells --
   ---------------
   
   procedure Set_Cells
     (Evt  : access Move_Cells_Event_Record;
      Cells : Cells_Lists.List) is
   begin
      Evt.Cells := Cells;
   end Set_Cells;
   
   ------------
   -- Get_Dx --
   ------------
   
   function Get_Dx (Evt : access Move_Cells_Event_Record) return Coordinate is
   begin
      return Evt.Dx;
   end Get_Dx;
   
   ------------
   -- Set_Dx --
   ------------
   
   procedure Set_Dx
     (Evt : access Move_Cells_Event_Record;
      Dx  : Coordinate) is
   begin
      Evt.Dx := Dx;
   end Set_Dx;
   
   ------------
   -- Get_Dy --
   ------------
   
   function Get_Dy (Evt : access Move_Cells_Event_Record) return Coordinate is
   begin
      return Evt.Dy;
   end Get_Dy;
   
   ------------
   -- Set_Dy --
   ------------
   
   procedure Set_Dy
     (Evt : access Move_Cells_Event_Record;
      Dy  : Coordinate) is
   begin
      Evt.Dy := Dy;
   end Set_Dy;
   
   ---------------
   -- Get_Clone --
   ---------------
   
   function Get_Clone (Evt : access Move_Cells_Event_Record) return Boolean is
   begin
      return Evt.Clone;
   end Get_Clone;
   
   ---------------
   -- Set_Clone --
   ---------------
   
   procedure Set_Clone
     (Evt   : access Move_Cells_Event_Record;
      Clone : Boolean) is
   begin
      Evt.Clone := Clone;
   end Set_Clone;
   
   ----------------
   -- Get_Target --
   ----------------
   
   function Get_Target
     (Evt : access Move_Cells_Event_Record) 
     return access Cell_Record'Class is
   begin
      return Evt.Target;
   end Get_Target;
   
   ----------------
   -- Set_Target --
   ----------------
   
   procedure Set_Target
     (Evt    : access Move_Cells_Event_Record;
      Target : access Cell_Record'Class) is
   begin
      Evt.Target := Target;
   end Set_Target;
   
   ------------------
   -- Get_Location --
   ------------------
   
   function Get_Location
     (Evt : access Move_Cells_Event_Record) return Point_Record is
   begin
      return Evt.Location;
   end Get_Location;
   
   ------------------
   -- Set_Location --
   ------------------
   
   procedure Set_Location
     (Evt      : access Move_Cells_Event_Record;
      Location : Point_Record) is
   begin
      Evt.Location := Location;
   end Set_Location;
   
end Artics.Graph.Events.Move_Cells_Events;
