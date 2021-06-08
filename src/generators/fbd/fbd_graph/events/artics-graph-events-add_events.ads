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
with Artics.Graph.Undoables.Edits; use Artics.Graph.Undoables.Edits;

package Artics.Graph.Events.Add_Events is
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   type Add_Event_Record is new Graph_Event_Record with private;
   type Add_Event_Ptr is access all Add_Event_Record;
   type Add_Event_Class_Ptr is access all Add_Event_Record'Class;
   
   No_Add_Event_Record  : constant Add_Event_Record;
   
   function New_Add_Event return Add_Event_Ptr;
   
   function New_Add_Event
     (Undo : access Undoable_Edit_Record'Class) return Add_Event_Ptr;
   
   function New_Add_Event
     (Undo : access Undoable_Edit_Record'Class;
      Name : Name_Id) return Add_Event_Ptr;
   
   procedure Free_Add_Event (This : in out Add_Event_Ptr);
   
   function Get_Undoable_Edit
     (This : access Add_Event_Record)
     return access Undoable_Edit_Record'Class;
   
   procedure Set_Undoable_Edit
     (This : access Add_Event_Record;
      Undo : access Undoable_Edit_Record'Class);
   
   function Get_Name
     (This : access Add_Event_Record) return Name_Id;
   
   procedure Set_Name
     (This : access Add_Event_Record;
      Name : Name_Id);
   
private
   
   type Add_Event_Record is new Graph_Event_Record with record
      Undo : access Undoable_Edit_Record'Class;
      Name : Name_Id;
   end record;
   
   No_Add_Event_Record  : constant Add_Event_Record := 
     Add_Event_Record'
     (No_Graph_Event_Record with
   	Undo => null,
      Name   => No_Name);
   
end Artics.Graph.Events.Add_Events;
