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

package body Artics.Graph.Events.Scale_And_Translate_Events is
   
   -----------------------------------
   -- New_Scale_And_Translate_Event --
   -----------------------------------
   
   function New_Scale_And_Translate_Event 
     return Scale_And_Translate_Event_Ptr is
      Evt : Scale_And_Translate_Event_Ptr := 
	new Scale_And_Translate_Event_Record'
	(No_Scale_And_Translate_Event_Record);
   begin
      Set_Event_Type (Evt, Event_Scale_And_Translate);
      return Evt;
  end New_Scale_And_Translate_Event;
  
  -----------------------------------
  -- New_Scale_And_Translate_Event --
  -----------------------------------
  
  function New_Scale_And_Translate_Event
     (Scale              : Coordinate;
      Previous_Scale     : Coordinate;
      Translate          : Point_Record;
      Previous_Translate : Point_Record)
     return Scale_And_Translate_Event_Ptr is
     
     This : Scale_And_Translate_Event_Ptr := New_Scale_And_Translate_Event;
   begin
      This.Scale              := Scale;
      This.Previous_Scale     := Previous_Scale;
      This.Translate          := Translate;
      This.Previous_Translate := Previous_Translate;
      
      return This;
   end New_Scale_And_Translate_Event;
   
   -----------------------------------
   -- Scale_And_Translate_Event_Ptr --
   -----------------------------------
   
   procedure Free_Scale_And_Translate_Event
     (This : in out Scale_And_Translate_Event_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Scale_And_Translate_Event_Record, Scale_And_Translate_Event_Ptr);
   begin
      Free (This);
   end Free_Scale_And_Translate_Event;

   ---------------
   -- Get_Scale --
   ---------------
   
   function Get_Scale
     (This : access Scale_And_Translate_Event_Record'Class) return Coordinate is
   begin
      return This.Scale;
   end Get_Scale;
   
   ---------------
   -- Set_Scale --
   ---------------
   
   procedure Set_Scale
     (This : access Scale_And_Translate_Event_Record'Class;
      Scale : Coordinate) is
   begin
      This.Scale := Scale;
   end Set_Scale;
   
   ------------------------
   -- Get_Previous_Scale --
   ------------------------
   
   function Get_Previous_Scale
     (This : access Scale_And_Translate_Event_Record'Class) return Coordinate is
   begin
      return This.Previous_Scale;
   end Get_Previous_Scale;
   
   ------------------------
   -- Set_Previous_Scale --
   ------------------------
   
   procedure Set_Previous_Scale
     (This : access Scale_And_Translate_Event_Record'Class;
      Scale : Coordinate) is
   begin
      This.Previous_Scale := Scale;
   end Set_Previous_Scale;
   
   -------------------
   -- Get_Translate --
   -------------------
   
   function Get_Translate
     (This : access Scale_And_Translate_Event_Record'Class) 
     return Point_Record is
   begin
      return This.Translate;
   end Get_Translate;
   
   -------------------
   -- Set_Translate --
   -------------------
   
   procedure Set_Translate
     (This      : access Scale_And_Translate_Event_Record'Class;
      Translate : Point_Record) is
   begin
      This.Translate := Translate;
   end Set_Translate;
   
   ----------------------------
   -- Get_Previous_Translate --
   ----------------------------
   
   function Get_Previous_Translate
     (This : access Scale_And_Translate_Event_Record'Class)
     return Point_Record is
   begin
      return This.Previous_Translate;
   end Get_Previous_Translate;
   
   ----------------------------
   -- Set_Previous_Translate --
   ----------------------------
   
   procedure Set_Previous_Translate
     (This      : access Scale_And_Translate_Event_Record'Class;
      Translate : Point_Record) is
   begin
      This.Previous_Translate := Translate;
   end Set_Previous_Translate;
   
end Artics.Graph.Events.Scale_And_Translate_Events;
