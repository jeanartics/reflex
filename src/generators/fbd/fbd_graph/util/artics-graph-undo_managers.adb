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

with Artics.Graph.Events.Clear_Events;
with Artics.Graph.Events.Undo_Events;
with Artics.Graph.Events.Redo_Events;
with Artics.Graph.Events.Add_Events;

package body Artics.Graph.Undo_Managers is
   
   use Undoable_Edit_Lists_Helpers;
   
   ----------------------
   -- New_Undo_Manager --
   ----------------------
   
   function New_Undo_Manager return access Undo_Manager_Record is
      Um : access Undo_Manager_Record := 
	new Undo_Manager_Record'(No_Undo_Manager_Record);
   begin
      return Um;
   end New_Undo_Manager;
   
   ----------------------
   -- New_Undo_Manager --
   ----------------------
   
   function New_Undo_Manager
     (Size : Integer) return access Undo_Manager_Record is
      This : access Undo_Manager_Record := 
	new Undo_Manager_Record'(No_Undo_Manager_Record);
   begin
      This.Size := Size;
      
      return This;
   end New_Undo_Manager;
   
   --------------
   -- Is_Empty --
   --------------
   
   function Is_Empty (This : access Undo_Manager_Record) return Boolean is
      use Undoable_Edits_Lists;
   begin
      return This.History /= Undoable_Edits_Lists.Empty_List 
	or else Undoable_Edits_Lists.Is_Empty (This.History);
   end Is_Empty;
   
   -----------
   -- Clear --
   -----------
   
   procedure Clear (This : access Undo_Manager_Record) is
   begin
      Undoable_Edits_Lists.Clear (This.History);
      This.Index_Of_Next_Add := 0;
      
      declare
	 use Artics.Graph.Events.Clear_Events;
	 Evt : access Clear_Event_Record := New_Clear_Event;
      begin
         Fire_Event (This, Evt);
         Free_Clear_Event (Clear_Event_Ptr (Evt));
	 --  fireEvent(new mxEventObject(mxEvent.CLEAR));
      end;
   end Clear;
   
   --------------
   -- Can_Undo --
   --------------
   
   function Can_Undo (This : access Undo_Manager_Record) return Boolean is
   begin
      return This.Index_Of_Next_Add > 0;
   end Can_Undo;
   
   ----------
   -- Undo --
   ----------
   
   procedure Undo (This : access Undo_Manager_Record) is
      Edit : access Undoable_Edit_Record;
   begin
      while This.Index_Of_Next_Add > 0 loop
	 This.Index_Of_Next_Add := This.Index_Of_Next_Add - 1;
	 
	 Edit := Undoable_Edit_Lists_Helpers.Get_At
	   (This.History, This.Index_Of_Next_Add);
	   
	 Edit.Undo;
					  
	 if Edit.Is_Significant then
	    declare
	       use Artics.Graph.Events.Undo_Events;
	       Evt : access Undo_Event_Record :=
		 New_Undo_Event (Edit);
	    begin
               Fire_Event (This, Evt);
               Free_Undo_Event (Undo_Event_Ptr (Evt));
	       --  fireEvent(new mxEventObject(mxEvent.UNDO, "edit", edit));
	    end;
	    
	    exit;
	 end if;
      end loop;
   end Undo;
   
   --------------
   -- Can_Redo --
   --------------
   
   function Can_Redo (This : access Undo_Manager_Record) return Boolean is
      L : Integer;
   begin
      L := Integer (Undoable_Edits_Lists.Length (This.History));
      return 
	This.Index_Of_Next_Add < L;
   end Can_Redo;
   
   ----------
   -- Redo --
   ----------
   
   procedure Redo (This : access Undo_Manager_Record) is
      N    : Integer;
      Edit : access Undoable_Edit_Record;
   begin
      N := Integer (undoable_Edits_Lists.Length (This.History));

      while This.Index_Of_Next_Add < N loop
	 Edit := Undoable_Edit_Lists_Helpers.Get_At
	   (This.History, This.Index_Of_Next_Add);
	 
	 This.Index_Of_Next_Add := This.Index_Of_Next_Add + 1;
	 
	 Edit.Redo;

	 if Edit.Is_Significant then
	    declare
	       use Artics.Graph.Events.Redo_Events;
	       Evt : access Redo_Event_Record :=
		 New_Redo_Event (Edit);
	    begin
               Fire_Event (This, Evt);
               Free_Redo_Event (Redo_Event_Ptr (Evt));
	       --  fireEvent(new mxEventObject(mxEvent.REDO, "edit", edit));
	    end;
	    exit;
	 end if;
      end loop;
   end Redo;
   
   ----------------------------
   -- Undoable_Edit_Happened --
   ----------------------------
   
   procedure Undoable_Edit_Happened
     (This          : access Undo_Manager_Record;
      Undoable_Edit : access Undoable_Edit_Record'Class) is
      
   begin
      This.trim;
      
      if This.Size > 0 
	and This.Size = Integer (undoable_Edits_Lists.Length (This.History))
      then
	 Undoable_Edits_Lists.Delete_First (This.History);
      end if;

      Undoable_Edits_Lists.Append (This.History, Undoable_Edit_Class_Ptr (Undoable_Edit));
      This.Index_Of_Next_Add := 
        Integer (undoable_Edits_Lists.Length (This.History));
      
      declare
	 use Artics.Graph.Events.Add_Events;
	 Evt : access Add_Event_Record := New_Add_Event (Undoable_Edit);
      begin
         Fire_Event (This, Evt);
         Free_Add_Event (Add_Event_Ptr (Evt));
	 --  fireEvent(new mxEventObject(mxEvent.ADD, "edit", undoableEdit));
      end;
   end Undoable_Edit_Happened;
   
   ----------
   -- Trim --
   ----------
   
   procedure Trim (This : access Undo_Manager_Record) is
      Edit : access Undoable_Edit_Record'Class;
   begin
      while Integer (undoable_Edits_Lists.Length (This.History)) >
        This.Index_Of_Next_Add
      loop
	 Edit := Undoable_Edit_Lists_Helpers.Get_At
	   (This.History, This.Index_Of_Next_Add);
	 
	 Undoable_Edit_Lists_Helpers.Remove_At
	   (This.History, This.Index_Of_Next_Add);
	 
	 Edit.Die;
      end loop;
   end Trim;
   
end Artics.Graph.Undo_Managers;
		  
		  
