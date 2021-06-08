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

package Artics.Graph.Events.Ordered_Cells_Events is
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   type Ordered_Cells_Event_Record is new Graph_Event_Record with private;
   type Ordered_Cells_Event_Ptr is access all Ordered_Cells_Event_Record;
   type Ordered_Cells_Event_Class_Ptr is
     access all Ordered_Cells_Event_Record'Class;
   
   No_Ordered_Cells_Event_Record  : constant Ordered_Cells_Event_Record;
   
   function New_Ordered_Cells_Event return Ordered_Cells_Event_Ptr;
     
   function New_Ordered_Cells_Event
     (Back  : Boolean;
      Cells : Cells_Lists.List) return Ordered_Cells_Event_Ptr;
     
   procedure Free_Ordered_Cells_Event
     (This : in out Ordered_Cells_Event_Ptr);
   
   function Get_Cells
     (Evt : access Ordered_Cells_Event_Record) return Cells_Lists.List;
   procedure Set_Cells
     (Evt   : access Ordered_Cells_Event_Record;
      Cells : Cells_Lists.List);
   
   function Get_Back
     (Evt : access Ordered_Cells_Event_Record) return Boolean;
   procedure Set_Back
     (Evt  : access Ordered_Cells_Event_Record;
      Back : Boolean);
   
private
   
   type Ordered_Cells_Event_Record is new Graph_Event_Record with record
      Back  : Boolean;
      Cells : Cells_Lists.List;
   end record;
   
   No_Ordered_Cells_Event_Record  : constant Ordered_Cells_Event_Record := 
     Ordered_Cells_Event_Record'
     (No_Graph_Event_Record with 
	Back => False,
      Cells  => Cells_Lists.Empty_List);
   
end Artics.Graph.Events.Ordered_Cells_Events;
