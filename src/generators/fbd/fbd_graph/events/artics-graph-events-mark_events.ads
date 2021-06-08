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

with Artics.Objects; use Artics.Objects;

with Artics.Graph.Cells_Interfaces; use Artics.Graph.Cells_Interfaces;
with Artics.Graph.Cells_States; use Artics.Graph.Cells_States;

with Artics.Graph.Events; use Artics.Graph.Events;

package Artics.Graph.Events.Mark_Events is
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   type Mark_Event_Record is new Graph_Event_Record with private;
   type Mark_Event_Ptr is access all Mark_Event_Record;
   type Mark_Event_Class_Ptr is access all Mark_Event_Record'Class;
   
   No_Mark_Event_Record  : constant Mark_Event_Record;
   
   function New_Mark_Event return Mark_Event_Ptr;
   
  function New_Mark_Event
    (State : access Cell_State_Record'Class) return Mark_Event_Ptr;
     
   procedure Free_Mark_Event (This : in out Mark_Event_Ptr);
   
   function Get_State
     (This : access Mark_Event_Record) return access Cell_State_Record'Class;
   
   procedure Set_State
     (This  : access Mark_Event_Record;
      State : access Cell_State_Record'Class);
   
private
   
   type Mark_Event_Record is new Graph_Event_Record with record 
      State : access Cell_State_Record'Class;
   end record;
   
   No_Mark_Event_Record  : constant Mark_Event_Record := 
     Mark_Event_Record'
     (No_Graph_Event_Record with State => null);
   
end Artics.Graph.Events.Mark_Events;
