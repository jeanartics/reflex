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

with Artics.Graphics.Gc_Cairo; use Artics.Graphics.Gc_Cairo;

package Artics.Graph.Events.Paint_Events is
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   type Paint_Event_Record is new Graph_Event_Record with private;
   type Paint_Event_Ptr is access all Paint_Event_Record;
   type Paint_Event_Class_Ptr is access all Paint_Event_Record'Class;
   
   No_Paint_Event_Record  : constant Paint_Event_Record;
   
   function New_Paint_Event return Paint_Event_Ptr;
   
   function New_Paint_Event
     (G : access Gc_Record'Class) return Paint_Event_Ptr;
   
   procedure Free_Paint_Event (This : in out Paint_Event_Ptr);
   
   function Get_Graphic
     (This : access Paint_Event_Record) return access Gc_Record'Class;
   
   procedure Set_Graphic
     (This : access Paint_Event_Record;
      G    : access Gc_Record'Class);
   
private
   
   type Paint_Event_Record is new Graph_Event_Record with record
      G : access Gc_Record'Class;
   end record;
   
   No_Paint_Event_Record  : constant Paint_Event_Record := 
     Paint_Event_Record'(No_Graph_Event_Record with G => null);
   
end Artics.Graph.Events.Paint_Events;
