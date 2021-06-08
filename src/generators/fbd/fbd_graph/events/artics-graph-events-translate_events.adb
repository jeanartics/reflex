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

package body Artics.Graph.Events.Translate_Events is
   
   -----------------------------
   -- New_Translate_Event --
   -----------------------------
   
   function New_Translate_Event return Translate_Event_Ptr is
      Evt : Translate_Event_Ptr := 
	new Translate_Event_Record'(No_Translate_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Translate);
      return Evt;
  end New_Translate_Event;
     
   -----------------------------
   -- New_Translate_Event --
   -----------------------------
   
  function New_Translate_Event
     (Translate          : Point_Record;
      Previous_Translate : Point_Record) return Translate_Event_Ptr is
     
     This : Translate_Event_Ptr := New_Translate_Event;
   begin
      This.Translate          := Translate;
      This.Previous_Translate := Previous_Translate;
      
      return This;
   end New_Translate_Event;
   
   --------------------------
   -- Free_Translate_Event --
   --------------------------
   
   procedure Free_Translate_Event (This : in out Translate_Event_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Translate_Event_Record, Translate_Event_Ptr);
   begin
      Free (This);
   end Free_Translate_Event;
   
   -------------------
   -- Get_Translate --
   -------------------
   
   function Get_Translate
     (This : access Translate_Event_Record'Class) return Point_Record is
   begin
      return This.Translate;
   end Get_Translate;
   
   ---------------
   -- Set_Translate --
   ---------------
   
   procedure Set_Translate
     (This      : access Translate_Event_Record'Class;
      Translate : Point_Record) is
   begin
      This.Translate := Translate;
   end Set_Translate;
   
   ------------------------
   -- Get_Previous_Translate --
   ------------------------
   
   function Get_Previous_Translate
     (This : access Translate_Event_Record'Class) return Point_Record is
   begin
      return This.Previous_Translate;
   end Get_Previous_Translate;
   
   ------------------------
   -- Set_Previous_Translate --
   ------------------------
   
   procedure Set_Previous_Translate
     (This : access Translate_Event_Record'Class;
      Translate : Point_Record) is
   begin
      This.Previous_Translate := Translate;
   end Set_Previous_Translate;
   
end Artics.Graph.Events.Translate_Events;
