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

with Ada.Unchaecked_Deallocation;

with Artics.Objects; use Artics.Objects;
with Artics.Graph.Events; use Artics.Graph.Events;

package body Artics.Graph.Events.Split_Edge_Events is
   
   --------------------------
   -- New_Split_Edge_Event --
   --------------------------
   
   function New_Split_Edge_Event return Split_Edge_Event_Ptr is
      Evt : Split_Edge_Event_Ptr := 
	new Split_Edge_Event_Record'(No_Split_Edge_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Split_Edge);
      return Evt;
   end New_Split_Edge_Event;
   
   --------------------------
   -- New_Split_Edge_Event -- 
   --------------------------
   
   function New_Split_Edge_Event
     (Edge     : access Cell_Record'Class;
      Cells    : Cells_Lists.List;
      New_Edge : access Cell_Record'Class;
      Dx       : Coordinate;
      Dy       : Coordinate)
     return Split_Edge_Event_Ptr is
      
      Evt : Split_Edge_Event_Ptr := New_Split_Edge_Event;
   begin
      Evt.Edge     := Edge;
      Evt.Cells    := Cells;
      Evt.New_Edge := New_Edge;
      Evt.Dx       := Dx;
      Evt.Dy       := Dy;
      
      return Evt;
   end New_Split_Edge_Event;
   
   ---------------------------
   -- Free_Split_Edge_Event --
   ---------------------------
   
   procedure Free_Split_Edge_Event (This : in out Split_Edge_Event_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Split_Edge_Event_Record, Split_Edge_Event_Ptr);
   begin
      Free (This);
   end Free_Split_Edge_Event;
   
   --------------
   -- Get_Edge --
   --------------
   
   function Get_Edge
     (Evt : access Split_Edge_Event_Record) 
     return access Cell_Record'Class is
   begin
      return Evt.Edge;
   end Get_Edge;
   
   --------------
   -- Set_Edge --
   --------------
   
   procedure Set_Edge
     (Evt  : access Split_Edge_Event_Record;
      Edge : access Cell_Record'Class) is
   begin
      Evt.Edge := Edge;
   end Set_Edge;
   
   ---------------
   -- Get_Cells --
   ---------------
   
   function Get_Cells
     (Evt : access Split_Edge_Event_Record) return Cells_Lists.List is
   begin
      return Evt.Cells;
   end Get_Cells;
   
   ---------------
   -- Set_Cells --
   ---------------
   
   procedure Set_Cells
     (Evt  : access Split_Edge_Event_Record;
      Cells : Cells_Lists.List) is
   begin
      Evt.Cells := Cells;
   end Set_Cells;
   
   ------------------
   -- Get_New_Edge --
   ------------------
   
   function Get_New_Edge
     (Evt : access Split_Edge_Event_Record) 
     return access Cell_Record'Class is
   begin
      return Evt.New_Edge;
   end Get_New_Edge;
   
   ------------------
   -- Set_New_Edge --
   ------------------
   
   procedure Set_New_Edge
     (Evt      : access Split_Edge_Event_Record;
      New_Edge : access Cell_Record'Class) is
   begin
      Evt.New_Edge := New_Edge;
   end Set_New_Edge;
   
   ------------
   -- Get_Dx --
   ------------
   
   function Get_Dx (Evt : access Split_Edge_Event_Record) return Coordinate is
   begin
      return Evt.Dx;
   end Get_Dx;
   
   ------------
   -- Set_Dx --
   ------------
   
   procedure Set_Dx
     (Evt : access Split_Edge_Event_Record;
      Dx  : Coordinate) is
   begin
      
      Evt.Dx := Dx;
   end Set_Dx;
   
   ------------
   -- Get_Dy --
   ------------
   
   function Get_Dy (Evt : access Split_Edge_Event_Record) return Coordinate is
   begin
      return Evt.Dy;
   end Get_Dy;
   
   ------------
   -- Set_Dy --
   ------------
   
   procedure Set_Dy
     (Evt : access Split_Edge_Event_Record;
      Dy  : Coordinate) is
   begin
      Evt.Dy := Dy;
   end Set_Dy;
   
end Artics.Graph.Events.Split_Edge_Events;
