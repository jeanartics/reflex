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

package body Artics.Graph.Events.Done_Events is
   
   --------------------
   -- New_Done_Event --
   --------------------
   
   function New_Done_Event return Done_Event_Ptr is
      Evt : Done_Event_Ptr :=
	new Done_Event_Record'(No_Done_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Done);
      return Evt;
   end New_Done_Event;
   
   --------------------
   -- New_Done_Event --
   --------------------
   
   procedure New_Done_Event (This : in out Done_Event_Record) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Done_Event_Record, Done_Event_Ptr);
   begin
      Free (This);
   end New_Done_Event;
     
end Artics.Graph.Events.Done_Events;
