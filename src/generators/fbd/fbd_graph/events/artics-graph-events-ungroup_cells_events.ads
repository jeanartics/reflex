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

package Artics.Graph.Events.Ungroup_Cells_Events is
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   type Ungroup_Cells_Event_Record is new Graph_Event_Record with private;
   type Ungroup_Cells_Event_Ptr is access all Ungroup_Cells_Event_Record;
   type Ungroup_Cells_Event_Class_Ptr is
     access all Ungroup_Cells_Event_Record'Class;
   
   No_Ungroup_Cells_Event_Record  : constant Ungroup_Cells_Event_Record;
   
   function New_Ungroup_Cells_Event return Ungroup_Cells_Event_Ptr;
     
   function New_Ungroup_Cells_Event
     (Cells  : Cells_Lists.List) return Ungroup_Cells_Event_Ptr;
   
   procedure Free_Ungroup_Cells_Event (This : in out Ungroup_Cells_Event_Ptr);
   
   function Get_Cells
     (Evt : access Ungroup_Cells_Event_Record) return Cells_Lists.List;
   procedure Set_Cells
     (Evt   : access Ungroup_Cells_Event_Record;
      Cells : Cells_Lists.List);
private
   
   type Ungroup_Cells_Event_Record is new Graph_Event_Record with record
      Cells  : Cells_Lists.List;
   end record;
   
   No_Ungroup_Cells_Event_Record  : constant Ungroup_Cells_Event_Record := 
     Ungroup_Cells_Event_Record'
     (No_Graph_Event_Record with Cells  => Cells_Lists.Empty_List);
   
end Artics.Graph.Events.Ungroup_Cells_Events;
