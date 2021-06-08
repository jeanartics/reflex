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

package body Artics.Graph.Events.Redo_Events is
   
   --------------------
   -- New_Redo_Event --
   --------------------
   
   function New_Redo_Event return Redo_Event_Ptr is
      Evt : Redo_Event_Ptr := new Redo_Event_Record'(No_Redo_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Redo);
      return Evt;
  end New_Redo_Event;
     
   --------------------
   -- New_Redo_Event --
   --------------------
   
   function New_Redo_Event
     (Redo : access Undoable_Edit_Record'Class) return Redo_Event_Ptr is
      This : Redo_Event_Ptr := New_Redo_Event;
   begin
      This.Redo := Redo;
      
      return This;
   end New_Redo_Event;
   
   --------------------
   -- New_Redo_Event --
   --------------------
   
   function New_Redo_Event
     (Redo : access Undoable_Edit_Record'Class;
      Name : Name_Id) return Redo_Event_Ptr is
      
      This : Redo_Event_Ptr := New_Redo_Event;
   begin
      This.Redo := Redo;
      This.Name := Name;
      
      return This;
   end New_Redo_Event;
   
   ---------------------
   -- Free_Redo_Event --
   ---------------------
   
   procedure Free_Redo_Event (This : in out Redo_Event_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Redo_Event_Record, Redo_Event_Ptr);
   begin
      Free (This);
   end Free_Redo_Event;
   
   -----------------------
   -- Get_Undoable_Edit --
   -----------------------
   
   function Get_Undoable_Edit
     (This : access Redo_Event_Record)
     return access Undoable_Edit_Record'Class is
   begin
      return This.Redo;
   end Get_Undoable_Edit;
   
   -----------------------
   -- Set_Undoable_Edit --
   -----------------------
   
   procedure Set_Undoable_Edit
     (This : access Redo_Event_Record;
      Redo : access Undoable_Edit_Record'Class) is
   begin
      This.Redo := Redo;
   end Set_Undoable_Edit;
   
   --------------
   -- Get_Name --
   --------------
   
   function Get_Name
     (This : access Redo_Event_Record) return Name_Id is
   begin
      return This.Name;
   end Get_Name;
   
   --------------
   -- Set_Name --
   --------------
   
   procedure Set_Name
     (This : access Redo_Event_Record;
      Name : Name_Id) is
   begin
      This.Name := Name;
   end Set_Name;
   
end Artics.Graph.Events.Redo_Events;
