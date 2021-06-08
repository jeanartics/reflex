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

package body Artics.Graph.Events.Mark_Events is
   
   --------------------
   -- New_Mark_Event --
   --------------------
   
   function New_Mark_Event return Mark_Event_Ptr is
   begin
     return New_Mark_Event (null);
  end New_Mark_Event;
  
   --------------------
   -- New_Mark_Event --
   --------------------
   
  function New_Mark_Event
    (State : access Cell_State_Record'Class) Return Mark_Event_Ptr is
     
     Evt : Mark_Event_Ptr := new Mark_Event_Record'(No_Mark_Event_Record);
  begin
     Set_Event_Type (Evt, Event_Mark);
     Evt.State := State;
     return Evt;
  end New_Mark_Event;
  
  ---------------------
  -- Free_Mark_Event --
  ---------------------
  
  procedure Free_Mark_Event (This : in out Mark_Event_Ptr) is
     procedure Free is new Ada.Unchecked_Deallocation
       (Mark_Event_Record, Mark_Event_Ptr);
  begin
     Free (This);
  end Free_Mark_Event;
  
  ---------------
  -- Get_State --
  ---------------
  
  function Get_State
    (This : access Mark_Event_Record) return access Cell_State_Record'Class is
  begin
     return This.State;
  end Get_State;
  
  ---------------
  -- Set_State --
  ---------------
  
  procedure Set_State
    (This  : access Mark_Event_Record;
     State : access Cell_State_Record'Class) is
  begin
     This.State := State;
  end Set_State;
  
  
end Artics.Graph.Events.Mark_Events;
