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
with Artics.Geometry.Points; use Artics.Geometry.Points;

package Artics.Graph.Events.Moved_Cells_Events is
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   type Moved_Cells_Event_Record is new Graph_Event_Record with private;
   type Moved_Cells_Event_Ptr is access all Moved_Cells_Event_Record;
   type Moved_Cells_Event_Class_Ptr is
     access all Moved_Cells_Event_Record'Class;
   
   No_Moved_Cells_Event_Record  : constant Moved_Cells_Event_Record;
   
   function New_Moved_Cells_Event return Moved_Cells_Event_Ptr;
     
   function New_Moved_Cells_Event
     (Cells      : Cells_Lists.List;
      Dx         : Coordinate;
      Dy         : Coordinate;
      Disconnect : Boolean) return Moved_Cells_Event_Ptr;
     
   procedure Free_Moved_Cells_Event (This : in out Moved_Cells_Event_Ptr);
   
   function Get_Cells
     (Evt : access Moved_Cells_Event_Record) return Cells_Lists.List;
   procedure Set_Cells
     (Evt  : access Moved_Cells_Event_Record;
      Cells : Cells_Lists.List);
   
   function Get_Dx (Evt : access Moved_Cells_Event_Record) return Coordinate;
   procedure Set_Dx
     (Evt : access Moved_Cells_Event_Record;
      Dx  : Coordinate);
   
   function Get_Dy (Evt : access Moved_Cells_Event_Record) return Coordinate;
   procedure Set_Dy
     (Evt : access Moved_Cells_Event_Record;
      Dy  : Coordinate);
   
   function Get_Disconnect 
     (Evt : access Moved_Cells_Event_Record) return Boolean;
   procedure Set_Disconnect
     (Evt        : access Moved_Cells_Event_Record;
      Disconnect : Boolean);
   
private
   
   type Moved_Cells_Event_Record is new Graph_Event_Record with record
      Cells      : Cells_Lists.List;
      Dx         : Coordinate;
      Dy         : Coordinate;
      Disconnect : Boolean;
   end record;
   
   No_Moved_Cells_Event_Record  : constant Moved_Cells_Event_Record := 
     Moved_Cells_Event_Record'
     (No_Graph_Event_Record with 
	Cells    => Cells_Lists.Empty_List,
      Dx         => Zero_Coordinate,
      Dy         => Zero_Coordinate,
      Disconnect => False);
   
end Artics.Graph.Events.Moved_Cells_Events;
