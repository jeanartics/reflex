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

package body Artics.Graph.Events.Clear_Events is
   
   ---------------------
   -- New_Clear_Event --
   ---------------------
   
   function New_Clear_Event return Clear_Event_Ptr is
      Evt : Clear_Event_Ptr :=
	new Clear_Event_Record'(No_Clear_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Clear);
      return Evt;
  end New_Clear_Event;
  
  ----------------------
  -- Free_Clear_Event --
  ----------------------
  
  procedure Free_Clear_Event (This : in out Clear_Event_Ptr) is
     procedure Free is new Ada.Unchecked_Deallocation
       (Clear_Event_Record, Clear_Event_Ptr);
  begin
     Free (This);
  end Free_Clear_Event;
  
end Artics.Graph.Events.Clear_Events;
