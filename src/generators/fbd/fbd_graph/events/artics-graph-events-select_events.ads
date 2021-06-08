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
with Artics.Graph.Cells; use Artics.Graph.Cells;

package Artics.Graph.Events.Select_Events is
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   type Select_Event_Record is new Graph_Event_Record with private;
   type Select_Event_Ptr is access all Select_Event_Record;
   type Select_Event_Class_Ptr is access all Select_Event_Record'Class;
   
   No_Select_Event_Record  : constant Select_Event_Record;
   
   function New_Select_Event return Select_Event_Ptr;
   
   function New_Select_Event
     (Added   : Cells_Lists.List;
      Removed : Cells_Lists.List) Return Select_Event_Ptr;
   
   procedure Free_Select_Event (This : in out Select_Event_Ptr);
    
   function Get_Added
     (This : access Select_Event_Record) return Cells_Lists.List;
   procedure Set_Added
     (This  : access Select_Event_Record;
      Cells : Cells_Lists.List);
   
   function Get_Removed
     (This : access Select_Event_Record) return Cells_Lists.List;
   procedure Set_Removed
     (This  : access Select_Event_Record;
      Cells : Cells_Lists.List);
   
private
   
   type Select_Event_Record is new Graph_Event_Record with record
      Added   : Cells_Lists.List;
      Removed : Cells_Lists.List;
   end record;
   
   No_Select_Event_Record  : constant Select_Event_Record := 
     Select_Event_Record'
     (No_Graph_Event_Record with
	Added => Cells_Lists.Empty_list,
      Removed => Cells_Lists.Empty_list);
   
end Artics.Graph.Events.Select_Events;
