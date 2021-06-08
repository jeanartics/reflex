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

package body Artics.Graph.Events.Continue_Events is
   
   -----------------------------
   -- New_Continue_Event --
   -----------------------------
   
   function New_Continue_Event return Continue_Event_Ptr is
      Evt : Continue_Event_Ptr := 
	new Continue_Event_Record'(No_Continue_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Continue);
      return Evt;
  end New_Continue_Event;
  
   -----------------------------
   -- New_Continue_Event --
   -----------------------------
   
   function New_Continue_Event
     (Evt : access Mouse_Event_Record;
      Dx  : Float;
      Dy  : Float) return Continue_Event_Ptr is
     
     This : Continue_Event_Ptr := New_Continue_Event;
   begin
      This.Evt := Evt;
      This.Dx  := Dx;
      This.Dy  := Dy;
      
      return This;
  end New_Continue_Event;
  
  -------------------------
  -- Free_Continue_Event --
  -------------------------
  
  procedure Free_Continue_Event (This : in out Continue_Event_Ptr) is
     procedure Free is new Ada.Unchecked_Deallocation
       (Continue_Event_Record, Continue_Event_Ptr);
  begin
     Free (This);
  end Free_Continue_Event;
  
  ---------------------
  -- Get_Mouse_Event --
  ---------------------
  
  function Get_Mouse_Event 
    (This : access Continue_Event_Record) return access Mouse_Event_Record is
  begin
     return This.Evt;
  end Get_Mouse_Event;
  
  ---------------------
  -- Set_Mouse_Event --
  ---------------------
  
  procedure Set_Mouse_Event 
    (This : access Continue_Event_Record;
     Evt  : access Mouse_Event_Record) is
  begin
     This.Evt := Evt;
  end Set_Mouse_Event;
  
  ------------
  -- Get_Dx --
  ------------
  
  function Get_Dx (This : access Continue_Event_Record) return Float is
  begin
     return This.Dx;
  end Get_Dx;
  
  procedure Set_Dx
    (This : access Continue_Event_Record;
     X    : Float) is
  begin
     This.Dx := X;
  end Set_Dx;
  
  ------------
  -- Get_DY --
  ------------
  
  function Get_DY (This : access Continue_Event_Record) return Float is
  begin
     return This.Dy;
  end Get_DY;
  
  ------------
  -- Set_DY --
  ------------
  
  procedure Set_DY
    (This : access Continue_Event_Record;
     Y    : Float) is
  begin
     This.Dy := Y;
  end Set_DY;
  
  
end Artics.Graph.Events.Continue_Events;
