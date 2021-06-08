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
-- Reflex is originally developed  by the Artics team at Grenoble (France). --
--                                                                          --
------------------------------------------------------------------------------

with Artics.Graph.Undoables.Edits; use Artics.Graph.Undoables.Edits;
with Artics.Graph.Events; use Artics.Graph.Events;

-- Implements an undo history. This class fires the following events:
-- 
-- mxEvent.CLEAR fires after clear was executed. The event has no properties.
--
-- mxEvent.UNDO fires afer a significant edit was undone in undo. The
-- edit property contains the mxUndoableEdit that was undone.
-- 
-- mxEvent.REDO fires afer a significant edit was redone in redo. The
-- edit property contains the mxUndoableEdit that was redone.
-- 
-- mxEvent.ADD fires after an undoable edit was added to the history. The
-- edit property contains the mxUndoableEdit that was added.

package Artics.Graph.Undo_Managers is
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   use Artics.Graph.Undoables.Edits.Undoable_Edits_Lists;
   
   type Undo_Manager_Record is new Event_Source_Record with private;
   type Undo_Manager_Ptr is access all Undo_Manager_Record'Class;
   
   No_Undo_Manager_Record : constant Undo_Manager_Record;
   
   function New_Undo_Manager return access Undo_Manager_Record;
   -- Constructs a new undo manager with a default history size.

   function New_Undo_Manager
     (Size : Integer) return access Undo_Manager_Record;
   
   function Is_Empty (This : access Undo_Manager_Record) return Boolean;
   
   procedure Clear (This : access Undo_Manager_Record);
   -- Clears the command history.
   
   function Can_Undo (This : access Undo_Manager_Record) return Boolean;
   -- Returns true if an undo is possible.
   
   procedure Undo (This : access Undo_Manager_Record);
   -- Undoes the last change.
   
   function Can_Redo (This : access Undo_Manager_Record) return Boolean;
   -- Returns true if a redo is possible.
   
   procedure Redo (This : access Undo_Manager_Record);
   -- Redoes the last change.
   
   procedure Undoable_Edit_Happened
     (This          : access Undo_Manager_Record;
      Undoable_Edit : access Undoable_Edit_Record'Class);
   -- Method to be called to add new undoable edits to the history.
   
   procedure Trim (This : access Undo_Manager_Record);
   -- Removes all pending steps after indexOfNextAdd from the history,
   -- invoking die on each edit. This is called from undoableEditHappened.
   
private
     
   type Undo_Manager_Record is new Event_Source_Record with record
      Size : Integer;
      -- Maximum command history size. 0 means unlimited history. Default is 
      -- 100.
      
      History : Undoable_Edits_Lists.List;
      -- List that contains the steps of the command history.
      
      Index_Of_Next_Add : Integer;
      -- Index of the element to be added next.
   end record;
   
   No_Undo_Manager_Record : constant Undo_Manager_Record := Undo_Manager_Record'
     (No_Event_Source_Record with 
	Size            => 100,
      History           => Undoable_Edits_Lists.Empty_List,
      Index_Of_Next_Add => 0);
   
end Artics.Graph.Undo_Managers;
		  
		  
