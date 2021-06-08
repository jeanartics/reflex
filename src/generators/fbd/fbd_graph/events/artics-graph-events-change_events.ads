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
with Artics.Graph.Undoables.Changes_Interfaces; use Artics.Graph.Undoables.Changes_Interfaces;
with Artics.Graph.Undoables.Edits; use Artics.Graph.Undoables.Edits;
				     
package Artics.Graph.Events.Change_Events is
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   type Change_Event_Record is new Graph_Event_Record with private;
   type Change_Event_Ptr is access all Change_Event_Record;
   type Change_Event_Class_Ptr is access all Change_Event_Record'Class;
   
   No_Change_Event_Record  : constant Change_Event_Record;
   
   function New_Change_Event return Change_Event_Ptr;
   function New_Change_Event
     (Edit    : access Undoable_Edit_Record;
      Changes : Undoables_Lists.List) return Change_Event_Ptr;
   
   procedure Free_Change_Event 
     (This : in out Change_Event_Ptr);
   
   function Get_Edit
     (Evt : access Change_Event_Record) return access Undoable_Edit_Record;
   
   procedure Set_Edit
     (Evt  : access Change_Event_Record;
      Edit : access Undoable_Edit_Record);
   
   function Get_Changes 
     (Evt : access Change_Event_Record) return Undoables_Lists.List;
   
private
   
   type Change_Event_Record is new Graph_Event_Record with record
      Edit    : access Undoable_Edit_Record;
      Changes : Undoables_Lists.List;
   end record;
   
   No_Change_Event_Record  : constant Change_Event_Record := 
     Change_Event_Record'
       (No_Graph_Event_Record with 
        Edit    => null,
        Changes => Undoables_Lists.Empty_List);
   
end Artics.Graph.Events.Change_Events;
