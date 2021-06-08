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

package body Artics.Generic_Events_Objects is
   
   ----------------------
   -- New_Event_Object --
   ----------------------
   
   function New_Event_Object return Event_Object_Ptr is
      
      Evt : Event_Object_Ptr := 
	new Event_Object_Record'(No_Event_Object_Record);
   begin
      return Evt;
   end New_Event_Object;
   
   ----------------------
   -- New_Event_Object --
   ----------------------
   
   function New_Event_Object
     (Event_Type : Event_Type_Enum) return Event_Object_Ptr is
      
      Evt : Event_Object_Ptr := 
	new Event_Object_Record'(No_Event_Object_Record);
   begin
      Evt.Event_Type := Event_Type;
      return Evt;
   end New_Event_Object;
   
   ----------------
   -- Event_Type --
   ----------------
   
   function Get_Event_Type
     (Evt : access Event_Object_Record) return Event_Type_Enum is
   begin
      return Evt.Event_Type;
   end Get_Event_Type;
   
   function Get_Event_Type
     (Evt : Event_Object_Record) return Event_Type_Enum is
   begin
      return Evt.Event_Type;
   end Get_Event_Type;
   
   --------------------
   -- Set_Event_Type --
   --------------------
   
   procedure Set_Event_Type
     (Evt        : access Event_Object_Record;
      Event_Type : Event_Type_Enum) is
   begin
      Evt.Event_Type := Event_Type;
   end Set_Event_Type;
   
   procedure Set_Event_Type
     (Evt        : in out Event_Object_Record;
      Event_Type : Event_Type_Enum) is
   begin
      Evt.Event_Type := Event_Type;
   end Set_Event_Type;
   
   ------------
   -- Object --
   ------------
   
   function Get_Object
     (Evt : access Event_Object_Record) return access Object_Record'Class is
   begin
      return Evt.Object;
   end Get_Object;
   
   ----------------
   -- Set_Object --
   ----------------
   
   procedure Set_Object
     (Evt    : access Event_Object_Record;
      Object : access Object_Record'Class) is
   begin
      Evt.Object := Object;
   end Set_Object;
   
   ----------
   -- Time --
   ----------
   
   function Get_Time (Evt : access Event_Object_Record) return Integer is
   begin
      return Evt.Time;
   end Get_Time;
   
   --------------
   -- Set_Time --
   --------------
   
   procedure Set_Time
     (Evt  : access Event_Object_Record;
      Time : Integer) is
   begin
      Evt.Time := Time;
   end Set_Time;
   
   ------------
   -- Sender --
   ------------
   
   function Get_Sender
     (Evt : access Event_Object_Record) return access Object_Record'Class is
   begin
      return Evt.Sender;
   end Get_Sender;
   
   ----------------
   -- Set_Sender --
   ----------------
   
   procedure Set_Sender
     (Evt    : access Event_Object_Record;
      Sender : access Object_Record'Class) is
   begin
      Evt.Sender := Sender;
   end Set_Sender;
   
   -----------------
   -- Is_Consumed --
   -----------------
   
   function Is_Consumed (Evt : access Event_Object_Record) return Boolean is
   begin
      return Evt.Consumed;
   end Is_Consumed;
   
   -------------
   -- Consume --
   -------------
   
   procedure Consume (Evt : access Event_Object_Record) is
   begin
      Evt.Consumed := True;
   end Consume;
      
end Artics.Generic_Events_Objects;
   
