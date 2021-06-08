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
with Artics.Rxwt.Mouse_Events; use Artics.Rxwt.Mouse_Events;

package Artics.Graph.Events.Stop_Events is
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   type Stop_Event_Record is new Graph_Event_Record with private;
   type Stop_Event_Ptr is access all Stop_Event_Record;
   type Stop_Event_Class_Ptr is access all Stop_Event_Record'Class;
   
   No_Stop_Event_Record  : constant Stop_Event_Record;
   
   function New_Stop_Event return Stop_Event_Ptr;
   
   function New_Stop_Event
     (Evt    : access Mouse_Event_Record;
      Commit : Boolean;
      Obj    : access Object_Record'Class := null)
     return Stop_Event_Ptr;
   
   procedure Free_Stop_Event (This : in out Stop_Event_Ptr);
   
   function Get_Mouse_Event
     (This : access Stop_Event_Record) return access Mouse_Event_Record;
   procedure Set_Mouse_Event
     (This : access Stop_Event_Record;
      Evt  : access Mouse_Event_Record);
   
   function Get_Commit
     (This : access Stop_Event_Record) return Boolean;
   procedure Set_Commit
     (This   : access Stop_Event_Record;
      Commit : Boolean);
   
   function Get_User_Object
     (This : access Stop_Event_Record) return access Object_Record'Class;
   
   procedure Set_User_Object
     (This : access Stop_Event_Record;
      Obj  : access Object_Record'Class);
   
private
   
   type Stop_Event_Record is new Graph_Event_Record with record
      Evt    : access Mouse_Event_Record;
      Obj    : access Object_Record'Class;
      Commit : Boolean;
   end record;
   
   No_Stop_Event_Record  : constant Stop_Event_Record := 
     Stop_Event_Record'
     (No_Graph_Event_Record with
	Evt  => null,
      Obj    => null,
      Commit => False);
   
end Artics.Graph.Events.Stop_Events;
