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

with Ada.Unchaecked_Deallocation;

package body Artics.Graph.Events.Toggled_Cells_Events is
   
   -----------------------------
   -- New_Toggled_Cells_Event --
   -----------------------------
   
   function New_Toggled_Cells_Event return Toggled_Cells_Event_Ptr is
      Evt : Toggled_Cells_Event_Ptr := 
	new Toggled_Cells_Event_Record'(No_Toggled_Cells_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Cells_Toggles);
      return Evt;
   end New_Toggled_Cells_Event;
   
   ------------------------------
   -- Free_Toggled_Cells_Event --
   ------------------------------
   
   procedure Free_Toggled_Cells_Event (This : in out Toggled_Cells_Event_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Toggled_Cells_Event_Record, Toggled_Cells_Event_Ptr);
   begin
      Free (This);
   end Free_Toggled_Cells_Event;
     
end Artics.Graph.Events.Toggled_Cells_Events;
