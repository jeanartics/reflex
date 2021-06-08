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

with Ada.Text_IO; use Ada.Text_IO;

with Artics.Graph.Models; use Artics.Graph.Models;
with Artics.Graph.Events; use Artics.Graph.Events;
with Artics.Graph.Events.Change_Events; use Artics.Graph.Events.Change_Events;

package body Artics.Graph.Undoables.Edits is
   
   -- procedure Put_Line (S : String) is null; -- renames Ada.Text_IO.Put_Line;
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   -----------------------
   -- New_Undoable_Edit --
   -----------------------
   
   function New_Undoable_Edit
     (Source : access Object_Record'Class) 
     return Undoable_Edit_Ptr is
   begin
      return New_Undoable_Edit (Source, True);
   end New_Undoable_Edit;
   
   -----------------------
   -- New_Undoable_Edit --
   -----------------------
   
   function New_Undoable_Edit
     (Source      : access Object_Record'Class;
      Significant : Boolean) return Undoable_Edit_Ptr is
      
      U : Undoable_Edit_Ptr := 
	new Undoable_Edit_Record'(No_Undoable_Edit_Record);
   begin
      U.Source      := Source;
      U.Significant := Significant;
      return U;
   end New_Undoable_Edit;
   
   --------------
   -- Dispatch --
   --------------
   
   procedure Dispatch (E : access Undoable_Edit_Record) is
      
      M : access Model_Record'Class := Model_Class_Ptr (E.Source);
   begin
      -- LATER: Remove changes property (deprecated)
      Put_Line ("Dispatch Begin");
      declare
	 use Artics.Graph.Events.Change_Events;
	 Evt : access Change_Event_Record := New_Change_Event (E, E.Changes);
      begin
	 Put_Line ("Dispatch Fire");
 	 Fire_Event (M, Evt);
	 Put_Line ("Dispatch Fire ended");
	 Free_Change_Event (Change_Event_Ptr (Evt));
	 -- (new mxEventObject(mxEvent.CHANGE, 
	 --            "edit", this, "changes", changes));
      end;
      Put_Line ("Dispatch End");
   end Dispatch;
   
   ---------
   -- Die --
   ---------
   
   procedure Die (E : access Undoable_Edit_Record) is
   begin
      null;
   end Die;
   
   ----------------
   -- Get_Source --
   ----------------
   
   function Get_Source
     (E : access Undoable_Edit_Record) return access Object_Record'Class is
   begin
      return E.Source;
   end Get_Source;
   
   -----------------
   -- Get_Changes --
   -----------------
   
   function Get_Changes
     (E : access Undoable_Edit_Record) return Undoables_Lists.List is
   begin
      return E.Changes;
   end Get_Changes;
   
   --------------------
   -- Is_Significant --
   --------------------
   
   function Is_Significant (E : access Undoable_Edit_Record) return Boolean is
   begin
      return E.Significant;
   end Is_Significant;
   
   ---------------
   -- Is_Undone --
   ---------------
   
   function Is_Undone (E : access Undoable_Edit_Record) return Boolean is
   begin
      return E.Undone;
   end Is_Undone;
   
   ---------------
   -- Is_Redone --
   ---------------
   
   function Is_Redone (E : access Undoable_Edit_Record) return Boolean is
   begin
      return E.Redone;
   end Is_Redone;
   
   --------------
   -- Is_Empty --
   --------------
   
   function Is_Empty (E : access Undoable_Edit_Record) return Boolean is
   begin
      return Undoables_Lists.Is_Empty (E.Changes);
   end Is_Empty;
   
   ---------
   -- Add --
   ---------
   
   procedure Add 
     (E      : access Undoable_Edit_Record;
      Change : access Undoable_Change_Interface'Class) is
   begin
      Undoables_Lists.Append (E.Changes, Change);
   end Add;
   
   ----------
   -- Undo --
   ----------
   
   procedure Undo (E : access Undoable_Edit_Record) is
   begin
      if not E.Undone then
	 for Undone of E.Changes loop
	    Undone.Execute;
	 end loop;
	 
	 E.Undone := True;
	 E.Redone := False;
      end if;
      
      Dispatch (E);
   end Undo;
   
   ----------
   -- Redo --
   ----------
   
   procedure Redo (E : access Undoable_Edit_Record) is
   begin
      if not E.Redone then
	 for Redone of E.Changes loop
	    Redone.Execute;
	 end loop;
	 
	 E.Undone := False;
	 E.Redone := True;
      end if;
      
      Dispatch (E);
   end Redo;
   
end Artics.Graph.Undoables.Edits;
			       
			       
			       
