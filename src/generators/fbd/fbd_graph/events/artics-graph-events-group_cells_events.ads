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

package Artics.Graph.Events.Group_Cells_Events is
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   type Group_Cells_Event_Record is new Graph_Event_Record with private;
   type Group_Cells_Event_Ptr is access all Group_Cells_Event_Record;
   type Group_Cells_Event_Class_Ptr is
     access all Group_Cells_Event_Record'Class;
   
   No_Group_Cells_Event_Record  : constant Group_Cells_Event_Record;
   
   function New_Group_Cells_Event return Group_Cells_Event_Ptr;
     
   function New_Group_Cells_Event
     (Group  : access Cell_Record'Class;
      Border : Coordinate;
      Cells  : Cells_Lists.List) return Group_Cells_Event_Ptr;
   
   procedure Free_Group_Cells_Event (This : in out Group_Cells_Event_Ptr);
   
   function Get_Group
     (Evt : access Group_Cells_Event_Record) return access Cell_Record'Class;
   procedure Set_Group
     (Evt   : access Group_Cells_Event_Record;
      Group : access Cell_Record'Class);
   
   function Get_Border
     (Evt : access Group_Cells_Event_Record) return Coordinate;
   procedure Set_Border
     (Evt    : access Group_Cells_Event_Record;
      Border : Coordinate);
   
   function Get_Cells
     (Evt : access Group_Cells_Event_Record) return Cells_Lists.List;
   procedure Set_Cells
     (Evt  : access Group_Cells_Event_Record;
      Cells : Cells_Lists.List);
private
   
   type Group_Cells_Event_Record is new Graph_Event_Record with record
      Group  : access Cell_Record'Class;
      Border : Coordinate;
      Cells  : Cells_Lists.List;
   end record;
   
   No_Group_Cells_Event_Record  : constant Group_Cells_Event_Record := 
     Group_Cells_Event_Record'
     (No_Graph_Event_Record with 
	Group  => null,
      Border => Zero_Coordinate,
      Cells  => Cells_Lists.Empty_List);
   
end Artics.Graph.Events.Group_Cells_Events;
