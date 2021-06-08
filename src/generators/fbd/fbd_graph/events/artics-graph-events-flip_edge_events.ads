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

with Artics.Graph.Cells_Interfaces; use Artics.Graph.Cells_Interfaces;
with Artics.Objects; use Artics.Objects;

with Artics.Graph.Events; use Artics.Graph.Events;

package Artics.Graph.Events.Flip_Edge_Events is
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   type Flip_Edge_Event_Record is new Graph_Event_Record with private;
   type Flip_Edge_Event_Ptr is access all Flip_Edge_Event_Record;
   type Flip_Edge_Event_Calss_Ptr is access all Flip_Edge_Event_Record'Class;
   
   No_Flip_Edge_Event_Record  : constant Flip_Edge_Event_Record;
   
   function New_Flip_Edge_Event return Flip_Edge_Event_Ptr;
   
   function New_Flip_Edge_Event
     (Edge : access Cell_Interface'Class) return Flip_Edge_Event_Ptr;
   
   procedure Free_Flip_Edge_Event (This : in out Flip_Edge_Event_Ptr);
   
   function Get_Edge
     (Evt : access Flip_Edge_Event_Record) return access Cell_Interface'Class;
   
   procedure Set_Edge
     (Evt  : access Flip_Edge_Event_Record;
      Edge : access Cell_Interface'Class);
   
private
   
   type Flip_Edge_Event_Record is new Graph_Event_Record with record
      Edge : access Cell_Interface'Class;
   end record;
   
   No_Flip_Edge_Event_Record  : constant Flip_Edge_Event_Record := 
     Flip_Edge_Event_Record'(No_Graph_Event_Record with Edge => null);
   
end Artics.Graph.Events.Flip_Edge_Events;
