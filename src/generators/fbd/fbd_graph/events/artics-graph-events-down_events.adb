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

package body Artics.Graph.Events.Down_Events is
   
   --------------------
   -- New_Down_Event --
   --------------------
   
   function New_Down_Event return Down_Event_Ptr is
      Evt : Down_Event_Ptr :=
	new Down_Event_Record'(No_Down_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Down);
      return Evt;
  end New_Down_Event;
  
  ---------------------
  -- Free_Down_Event --
  ---------------------
  
  procedure Free_Down_Event (This : in out Down_Event_Record) is
     procedure Free is new Ada.Unchecked_Deallocation
       (Down_Event_Record, Down_Event_Ptr);
  begin
     Free (This);
  end Free_Down_Event;
  
end Artics.Graph.Events.Down_Events;
