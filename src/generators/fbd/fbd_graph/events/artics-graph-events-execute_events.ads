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

with Artics.Types; use Artics.Types;
with Artics.Namet; use Artics.Namet;

with Artics.Graph.Cells_Interfaces; use Artics.Graph.Cells_Interfaces;
with Artics.Objects; use Artics.Objects;

with Artics.Graph.Events; use Artics.Graph.Events;
with Artics.Graph.Models_Changes; use Artics.Graph.Models_Changes;

package Artics.Graph.Events.Execute_Events is
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   type Execute_Event_Record is new Graph_Event_Record with private;
   type Execute_Event_Ptr is access all Execute_Event_Record;
   
   No_Execute_Event_Record  : constant Execute_Event_Record;
   
   function New_Execute_Event return Execute_Event_Ptr;
   
   function New_Execute_Event
     (Change : access Model_Change_Record'Class) return Execute_Event_Ptr;
   
   function New_Execute_Event
     (Change : access Model_Change_Record'Class;
      Name   : Name_id) return Execute_Event_Ptr;
   
   procedure Free_Execute_Event (This : in out Execute_Event_Ptr);
   
   function Get_Change
     (This : access Execute_Event_Record) 
     return access Model_Change_Record'Class;
   
   procedure Set_Change 
     (This   : access Execute_Event_Record;
      Change : access Model_Change_Record'Class);
   
   function Get_Name (This : access Execute_Event_Record) return Name_Id;
   procedure Set_Name
     (This : access Execute_Event_Record;
      Name : Name_Id);
   
private
   
   type Execute_Event_Record is new Graph_Event_Record with record
      Change : access Model_Change_Record'Class;
      Name   : Name_Id;
   end record;
   
   No_Execute_Event_Record  : constant Execute_Event_Record := 
     Execute_Event_Record'
     (No_Graph_Event_Record with 
	Change => null,
      Name     => No_Name);
   
end Artics.Graph.Events.Execute_Events;
