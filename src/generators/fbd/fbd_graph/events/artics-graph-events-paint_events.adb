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

package body Artics.Graph.Events.Paint_Events is
   
   -----------------------------
   -- New_Paint_Event --
   -----------------------------
   
   function New_Paint_Event return Paint_Event_Ptr is
      Evt : Paint_Event_Ptr := new Paint_Event_Record'(No_Paint_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Paint);
      return Evt;
  end New_Paint_Event;
     
  ----------------------------
  -- New_Paint_Event --
  ----------------------------
  
   function New_Paint_Event
     (G : access Gc_Record'Class) return Paint_Event_Ptr is
      
      This : Paint_Event_Ptr := New_Paint_Event;
   begin
      This.G := G;
      
      return This;
   end New_Paint_Event;
   
   ----------------------
   -- Free_Paint_Event --
   ----------------------
   
   procedure Free_Paint_Event (This : in out Paint_Event_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Paint_Event_Record, Paint_Event_Ptr);
   begin
      Free (This);
   end Free_Paint_Event;
   
   -----------------
   -- Get_Graphic --
   -----------------
   
   function Get_Graphic
     (This : access Paint_Event_Record) return access Gc_Record'Class is
   begin
      return This.G;
   end Get_Graphic;
   
   -----------------
   -- Set_Graphic --
   -----------------
   
   procedure Set_Graphic
     (This : access Paint_Event_Record;
      G    : access Gc_Record'Class) is
   begin
      This.G := G;
   end Set_Graphic;
  
end Artics.Graph.Events.Paint_Events;
