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

package Artics.Graph.Events.Update_Cell_Size_Events is
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   type Update_Cell_Size_Event_Record is new Graph_Event_Record with private;
   type Update_Cell_Size_Event_Ptr is access all Update_Cell_Size_Event_Record;
     type Update_Cell_Size_Event_Class_Ptr is
     access all Update_Cell_Size_Event_Record'Class;
   
   No_Update_Cell_Size_Event_Record  : constant Update_Cell_Size_Event_Record;
   
   function New_Update_Cell_Size_Event return Update_Cell_Size_Event_Ptr;
     
   function New_Update_Cell_Size_Event
     (Cell            : access Cell_Record'Class;
      Ignore_Children : Boolean) return Update_Cell_Size_Event_Ptr;
     
   procedure Free_Update_Cell_Size_Event 
     (This : in out Update_Cell_Size_Event_Ptr);
   
   function Get_Cell
     (Evt : access Update_Cell_Size_Event_Record)
     return access Cell_Record'Class;
   procedure Set_Cell
     (Evt  : access Update_Cell_Size_Event_Record;
      Cell : access Cell_Record'Class);
   
   function Get_Ignore_Children
     (Evt : access Update_Cell_Size_Event_Record) return Boolean;
   procedure Set_Ignore_Children
     (Evt             : access Update_Cell_Size_Event_Record;
      Ignore_Children : Boolean);
   
private
   
   type Update_Cell_Size_Event_Record is new Graph_Event_Record with record
      Cell            : access Cell_Record'Class;
      Ignore_Children : Boolean;
   end record;
   
   No_Update_Cell_Size_Event_Record  : constant Update_Cell_Size_Event_Record := 
     Update_Cell_Size_Event_Record'
     (No_Graph_Event_Record with 
	Cell          => null,
      Ignore_Children => False);
   
end Artics.Graph.Events.Update_Cell_Size_Events;
