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

package body Artics.Graph.Events.Select_Events is
   
   ----------------------
   -- New_Select_Event --
   ----------------------
   
   function New_Select_Event return Select_Event_Ptr is
      Evt : Select_Event_Ptr := 
	new Select_Event_Record'(No_Select_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Select);
      return Evt;
  end New_Select_Event;
  
   ----------------------
   -- New_Select_Event --
   ----------------------
   
   function New_Select_Event
     (Added   : Cells_Lists.List;
      Removed : Cells_Lists.List) return Select_Event_Ptr is
      
      This : Select_Event_Ptr := New_Select_Event;
   begin
      This.Added   := Added;
      This.Removed := Removed;
      
      return This;
   end New_Select_Event;
   
   
  -----------------------
  -- Free_Select_Event --
  -----------------------
  
  procedure Free_Select_Event (This : in out Select_Event_Ptr) is
     procedure Free is new Ada.Unchecked_Deallocation
       (Select_Event_Record, Select_Event_Ptr);
  begin
     Free (This);
  end Free_Select_Event;
  
  ---------------
  -- Get_Added --
  ---------------
  
  function Get_Added
    (This : access Select_Event_Record) return Cells_Lists.List is
  begin
     return This.Added;
  end Get_Added;
  
  ---------------
  -- Set_Added --
  ---------------
  
  procedure Set_Added
    (This  : access Select_Event_Record;
     Cells : Cells_Lists.List) is
  begin
     This.Added := Cells;
  end Set_Added;
  
  -----------------
  -- Get_Removed --
  -----------------
  
  function Get_Removed
    (This : access Select_Event_Record) return Cells_Lists.List is
  begin
     return This.Removed;
  end Get_Removed;
  
  -----------------
  -- Set_Removed --
  -----------------
  
  procedure Set_Removed
    (This  : access Select_Event_Record;
     Cells : Cells_Lists.List) is
  begin
     This.Removed := Cells;
  end Set_Removed;
  
end Artics.Graph.Events.Select_Events;
