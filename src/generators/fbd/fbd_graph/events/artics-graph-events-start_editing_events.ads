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

with Artics.Graph.Cells; use Artics.Graph.Cells;
with Artics.Rxwt.Mouse_Events; use Artics.Rxwt.Mouse_Events;

package Artics.Graph.Events.Start_Editing_Events is
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   type Start_Editing_Event_Record is new Graph_Event_Record with private;
   type Start_Editing_Event_Ptr is access all Start_Editing_Event_Record;
   type Start_Editing_Event_Class_Ptr is
     access all Start_Editing_Event_Record'Class;
   
   No_Start_Editing_Event_Record  : constant Start_Editing_Event_Record;
   
   function New_Start_Editing_Event return Start_Editing_Event_Ptr;
   
   function New_Start_Editing_Event
     (Cell : access Cell_Record'Class;
      Evt  : access Mouse_Event_Record'Class)
     return Start_Editing_Event_Ptr;
   
   procedure Free_Start_Editing_Event (This : in out Start_Editing_Event_Ptr);
   
   function Get_Cell
     (This : access Start_Editing_Event_Record)
     return access Cell_Record'Class;
   
   procedure Set_Cell
     (This : access Start_Editing_Event_Record;
      Cell : access Cell_Record'Class);
   
   function Get_Mouse_Event
     (This : access Start_Editing_Event_Record)
     return access Mouse_Event_Record'Class;
     
   procedure Set_Mouse_Event
     (This : access Start_Editing_Event_Record;
      Evt  : access Mouse_Event_Record'Class);
     
private
   
   type Start_Editing_Event_Record is new Graph_Event_Record with record
      Cell : access Cell_Record'Class;
      Evt  : access Mouse_Event_Record'Class;
   end record;
   
   No_Start_Editing_Event_Record  : constant Start_Editing_Event_Record := 
     Start_Editing_Event_Record'
     (No_Graph_Event_Record with 
	Cell => null,
      Evt    => null);
   
end Artics.Graph.Events.Start_Editing_Events;
