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

package Artics.Graph.Events.Cells_Added_Events is
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   type Cells_Added_Event_Record is new Graph_Event_Record with private;
   type Cells_Added_Event_Ptr is access all Cells_Added_Event_Record;
   
   No_Cells_Added_Event_Record  : constant Cells_Added_Event_Record;
   
   function New_Cells_Added_Event return Cells_Added_Event_Ptr;
     
   function New_Cells_Added_Event
     (Cells    : Cells_Lists.List;
      Parent   : access Cell_Record'Class;
      Index    : Integer;
      Source   : access Cell_Record'Class;
      Target   : access Cell_Record'Class;
      Absolute : Boolean) return Cells_Added_Event_Ptr;
     
   procedure Free_Cells_Added_Event (This : in out Cells_Added_Event_Ptr);
   
   function Get_Cells
     (Evt : access Cells_Added_Event_Record) return Cells_Lists.List;
   procedure Set_Cells
     (Evt   : access Cells_Added_Event_Record;
      Cells : Cells_Lists.List);
   
   function Get_Parent
     (Evt : access Cells_Added_Event_Record) return access Cell_Record'Class;
   procedure Set_Parent
     (Evt    : access Cells_Added_Event_Record;
      Parent : access Cell_Record'Class);
   
   function Get_Index
     (Evt : access Cells_Added_Event_Record) return Integer;
   procedure Set_Index
     (Evt   : access Cells_Added_Event_Record;
      Index : Integer);
   
   function Get_Source
     (Evt : access Cells_Added_Event_Record) return access Cell_Record'Class;
   procedure Set_Source
     (Evt    : access Cells_Added_Event_Record;
      Source : access Cell_Record'Class);
   
   function Get_Target
     (Evt : access Cells_Added_Event_Record) return access Cell_Record'Class;
   procedure Set_Target
     (Evt    : access Cells_Added_Event_Record;
      Target : access Cell_Record'Class);
   
   function Get_Absolute (Evt : access Cells_Added_Event_Record) return Boolean;
   procedure Set_Absolute
     (Evt      : access Cells_Added_Event_Record;
      Absolute : Boolean);
   
private
   
   type Cells_Added_Event_Record is new Graph_Event_Record with record
     Cells    : Cells_Lists.List;
     Parent   : access Cell_Record'Class;
     Index    : Integer;
     Source   : access Cell_Record'Class;
     Target   : access Cell_Record'Class;
     Absolute : Boolean;
   end record;
	 
   No_Cells_Added_Event_Record  : constant Cells_Added_Event_Record := 
     Cells_Added_Event_Record'
     (No_Graph_Event_Record with 
	Cells  => Cells_Lists.Empty_List,
      Parent   => null,
      Index    => 0,
      Source   => null,
      Target   => null,
      Absolute => False);
   
end Artics.Graph.Events.Cells_Added_Events;
