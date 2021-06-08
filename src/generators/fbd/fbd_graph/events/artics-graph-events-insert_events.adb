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

package body Artics.Graph.Events.Insert_Events is
   
   ----------------------
   -- New_Insert_Event --
   ----------------------
   
   function New_Insert_Event return Insert_Event_Ptr is
      Evt : Insert_Event_Ptr :=
	new Insert_Event_Record'(No_Insert_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Insert);
      return Evt;
   end New_Insert_Event;
   
   ----------------------
   -- New_Insert_Event --
   ----------------------
   
   function New_Insert_Event
     (Cell : access Cell_Interface'Class) return Insert_Event_Ptr is
      
      Evt : Insert_Event_Ptr := New_Insert_Event;
   begin
      Evt.Cell := Cell;
      return Evt;
   end New_Insert_Event;
   
   -----------------------
   -- Free_Insert_Event --
   -----------------------
   
   procedure Free_Insert_Event (This : in out Insert_Event_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Insert_Event_Record, Insert_Event_Ptr);
   begin
      Free (This);
   end Free_Insert_Event;
   
   --------------
   -- Get_Cell --
   --------------
   
   function Get_Cell
     (This : access Insert_Event_Record) return access Cell_Interface'Class is
   begin
      return This.Cell;
   end Get_Cell;
   
   --------------
   -- Set_Cell --
   --------------
   
   procedure Set_Cell
     (This : access Insert_Event_Record;
      Cell : access Cell_Interface'Class) is
   begin
      This.Cell := Cell;
   end Set_Cell;
   
end Artics.Graph.Events.Insert_Events;
