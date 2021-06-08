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

package Artics.Graph.Events.Continue_Events is
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   type Continue_Event_Record is new Graph_Event_Record with private;
   type Continue_Event_Ptr is access all Continue_Event_Record;
   type Continue_Event_Class_Ptr is access all Continue_Event_Record'Class;
   
   No_Continue_Event_Record  : constant Continue_Event_Record;
   
   function New_Continue_Event return Continue_Event_Ptr;
   
   function New_Continue_Event
     (Evt : access Mouse_Event_Record;
      Dx  : Float;
      Dy  : Float) return Continue_Event_Ptr;
   
   procedure Free_Continue_Event (This : in out Continue_Event_Ptr);
   
   function Get_Mouse_Event 
     (This : access Continue_Event_Record) return access Mouse_Event_Record;
   
   procedure Set_Mouse_Event 
     (This : access Continue_Event_Record;
      Evt  : access Mouse_Event_Record);
   
   function Get_Dx (This : access Continue_Event_Record) return Float;
   procedure Set_Dx
     (This : access Continue_Event_Record;
      X    : Float);
   
   function Get_DY (This : access Continue_Event_Record) return Float;
   procedure Set_DY
     (This : access Continue_Event_Record;
      Y    : Float);
   
private
   
   type Continue_Event_Record is new Graph_Event_Record with record
      Evt : access Mouse_Event_Record;
      Dx  : Float;
      Dy  : Float;
   end record;
   
   No_Continue_Event_Record  : constant Continue_Event_Record := 
     Continue_Event_Record'
     (No_Graph_Event_Record with 
	Evt => null,
      Dx    => 0.0,
      Dy    => 0.0);
   
end Artics.Graph.Events.Continue_Events;
