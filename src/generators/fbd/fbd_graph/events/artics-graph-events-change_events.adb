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

with Ada.Unchecked_Deallocation;

package body Artics.Graph.Events.Change_Events is
   
   ----------------------
   -- New_Change_Event --
   ----------------------
   
   function New_Change_Event return Change_Event_Ptr is
      Evt : Change_Event_Ptr :=
	new Change_Event_Record'(No_Change_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Change);
      return Evt;
  end New_Change_Event;
  
   ----------------------
   -- New_Change_Event --
   ----------------------
   
   function New_Change_Event 
     (Edit    : access Undoable_Edit_Record;
      Changes : Undoables_Lists.List) return Change_Event_Ptr is
      
      Evt : Change_Event_Ptr := New_Change_Event;
   begin
      Evt.Edit := Edit;
      Evt.Changes := Changes;
      return Evt;
  end New_Change_Event;
  
   -----------------------
   -- Free_Change_Event --
   -----------------------
   
   procedure Free_Change_Event 
     (This : in out Change_Event_Ptr) is
      
      procedure Free is new Ada.Unchecked_Deallocation
	(Change_Event_Record, Change_Event_Ptr);
   begin
      Free (This);
  end Free_Change_Event;
  
  --------------
  -- Get_Edit --
  --------------
  
  function Get_Edit
    (Evt : access Change_Event_Record) return access Undoable_Edit_Record is
  begin
     return Evt.Edit;
  end Get_Edit;
  
  --------------
  -- Set_Edit --
  --------------
  
  procedure Set_Edit
    (Evt  : access Change_Event_Record;
     Edit : access Undoable_Edit_Record) is
  begin
     Evt.Edit := Edit;
  end Set_Edit;
  
  -----------------
  -- Get_Changes --
  -----------------
  
  function Get_Changes 
    (Evt : access Change_Event_Record) return Undoables_Lists.List is
  begin
     return Evt.Changes;
  end Get_Changes;
  
  
end Artics.Graph.Events.Change_Events;
