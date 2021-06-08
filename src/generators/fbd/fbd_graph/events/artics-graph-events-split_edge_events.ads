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

with Artics.Graph.Cells; use Artics.Graph.Cells;
with Artics.Objects; use Artics.Objects;

with Artics.Graph.Events; use Artics.Graph.Events;
with Artics.Geometry; use Artics.Geometry;

package Artics.Graph.Events.Split_Edge_Events is
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   type Split_Edge_Event_Record is new Graph_Event_Record with private;
   type Split_Edge_Event_Ptr is access all Split_Edge_Event_Record;
   type Split_Edge_Event_Class_Ptr is access all Split_Edge_Event_Record'Class;
   
   No_Split_Edge_Event_Record  : constant Split_Edge_Event_Record;
   
   function New_Split_Edge_Event return Split_Edge_Event_Ptr;
     
   function New_Split_Edge_Event
     (Edge     : access Cell_Record'Class;
      Cells    : Cells_Lists.List;
      New_Edge : access Cell_Record'Class;
      Dx       : Coordinate;
      Dy       : Coordinate) return Split_Edge_Event_Ptr;
     
   procedure Free_Split_Edge_Event (This : in out Split_Edge_Event_Ptr);
   
   function Get_Edge
     (Evt : access Split_Edge_Event_Record) return access Cell_Record'Class;
   procedure Set_Edge
     (Evt  : access Split_Edge_Event_Record;
      Edge : access Cell_Record'Class);
   
   function Get_Cells
     (Evt : access Split_Edge_Event_Record) return Cells_Lists.List;
   procedure Set_Cells
     (Evt  : access Split_Edge_Event_Record;
      Cells : Cells_Lists.List);
   
   function Get_New_Edge
     (Evt : access Split_Edge_Event_Record) return access Cell_Record'Class;
   procedure Set_New_Edge
     (Evt      : access Split_Edge_Event_Record;
      New_Edge : access Cell_Record'Class);
   
   function Get_Dx (Evt : access Split_Edge_Event_Record) return Coordinate;
   procedure Set_Dx
     (Evt : access Split_Edge_Event_Record;
      Dx  : Coordinate);
   
   function Get_Dy (Evt : access Split_Edge_Event_Record) return Coordinate;
   procedure Set_Dy
     (Evt : access Split_Edge_Event_Record;
      Dy  : Coordinate);
   
private
   
   type Split_Edge_Event_Record is new Graph_Event_Record with record
      Edge     : access Cell_Record'Class;
      Cells    : Cells_Lists.List;
      New_Edge : access Cell_Record'Class;
      Dx       : Coordinate;
      Dy       : Coordinate;
   end record;
   
   No_Split_Edge_Event_Record  : constant Split_Edge_Event_Record := 
     Split_Edge_Event_Record'
     (No_Graph_Event_Record with 
	Edge   => null,
      Cells    => Cells_Lists.Empty_List,
      New_Edge => null,
      Dx       => 0.0,
      Dy       => 0.0);
   
end Artics.Graph.Events.Split_Edge_Events;
