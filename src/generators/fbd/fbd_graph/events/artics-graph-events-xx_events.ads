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

package Artics.Graph.Events.Done_Events is
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   type Add_Cells_Event_Record is new Graph_Event_Record with private;
   
   No_Add_Cells_Event_Record  : constant Add_Cells_Event_Record;
   
   function New_Add_Cells_Event
     (Cells  : Cells_Lists.List;
      Parent : access Cell_Interface'Class;
      Index  : Integer;
      Source : access Cell_Interface'Class;
      Target : access Cell_Interface'Class)
     return access Add_Cells_Event_Record;
     
   function Get_Cells
     (Evt : access Add_Cells_Event_Record) return Cells_Lists.List;
   procedure Set_Cells
     (Evt  : access Add_Cells_Event_Record;
      Cells : Cells_Lists.List);
   
   function Get_Parent
     (Evt : access Add_Cells_Event_Record) return access Cell_Interface'Class;
   procedure Set_Parent
     (Evt    : access Add_Cells_Event_Record;
      Parent : access Cell_Interface'Class);
   
   function Get_Index
     (Evt : access Add_Cells_Event_Record) return Integer;
   procedure Set_Index
     (Evt   : access Add_Cells_Event_Record;
      Index : Integer);
   
   function Get_Source
     (Evt : access Add_Cells_Event_Record) return access Cell_Interface'Class;
   procedure Set_Source
     (Evt    : access Add_Cells_Event_Record;
      Source : access Cell_Interface'Class);
   
   function Get_Target
     (Evt : access Add_Cells_Event_Record) return access Cell_Interface'Class;
   procedure Set_Target
     (Evt    : access Add_Cells_Event_Record;
      Target : access Cell_Interface'Class);
   
private
   
   type Add_Cells_Event_Record is new Graph_Event_Record with record
      Cells  : Cells_Lists.List;
      Parent : access Cell_Interface'Class;
      Index  : Integer;
      Source : access Cell_Interface'Class;
      Target : access Cell_Interface'Class;
   end record;
   
   No_Add_Cells_Event_Record  : constant Add_Cells_Event_Record := 
     Add_Cells_Event_Record'
     (No_Graph_Event_Record with 
	Cells  => Cells_Lists.Empty_List,
      Parent   => access Cell_Interface'Class,
      Index    => Integer,
      Source   => access Cell_Interface'Class,
      Target   => access Cell_Interface'Class);
   
   
   
   
   type Update_Cell_Size_Event_Record is new Graph_Event_Record with record
      Cell : Cell_Id;
      Ignore_Children : Boolean;
   end record;
   
   
   
   
   
   
   
end Artics.Graph.Events.Done_Events;
