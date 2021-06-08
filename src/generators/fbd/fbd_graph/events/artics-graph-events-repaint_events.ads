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
with Artics.Geometry; use Artics.Geometry;
with Artics.Geometry.Points; use Artics.Geometry.Points;
with Artics.Geometry.Rectangles; use Artics.Geometry.Rectangles;

package Artics.Graph.Events.Repaint_Events is
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   type Repaint_Event_Record is new Graph_Event_Record with private;
   type Repaint_Event_Ptr is access all Repaint_Event_Record;
   type Repaint_Event_Class_Ptr is access all Repaint_Event_Record'Class;
   
   No_Repaint_Event_Record  : constant Repaint_Event_Record;
   
   function New_Repaint_Event return Repaint_Event_Ptr;
   
   function New_Repaint_Event 
    (Region : Rectangle_Record) return Repaint_Event_Ptr;
  
   procedure Free_Repaint_Event (This : in out Repaint_Event_Ptr);
   
   function Get_Region
     (Evt : access Repaint_Event_Record) return Rectangle_Record;
   procedure Set_Region
     (Evt    : access Repaint_Event_Record;
      Region : Rectangle_Record);
   
private
   
   type Repaint_Event_Record is new Graph_Event_Record with record 
      Region : Rectangle_Record;
   end record;
   
   No_Repaint_Event_Record  : constant Repaint_Event_Record := 
     Repaint_Event_Record'
     (No_Graph_Event_Record with Region => No_Rectangle_Record); 
   
end Artics.Graph.Events.Repaint_Events;
