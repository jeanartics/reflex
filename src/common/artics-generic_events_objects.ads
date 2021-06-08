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

with Artics.Objects; use Artics.Objects;

-- Base class for objects that dispatch named events.

generic
   type Event_Type_Enum is (<>);
package Artics.Generic_Events_Objects is
   
   No_Event_Type : constant Event_Type_Enum := Event_Type_Enum'First;
   
   type Event_Object_Record is new Object_Record with private;
   type Event_Object_Ptr is access all Event_Object_Record;
   type Event_Object_Class_Ptr is access all Event_Object_Record'Class;
   
   No_Event_Object_Record : constant Event_Object_Record;
   
   function New_Event_Object return Event_Object_Ptr;
   function New_Event_Object
     (Event_Type : Event_Type_Enum) return Event_Object_Ptr;
   -- Constructs a new event for the given name and properties. The optional
   -- properties are specified using a sequence of keys and values, eg.
   -- <code>new mxEventObject("eventName", key1, val1, .., keyN, valN))</code>
   
   function Get_Event_Type
     (Evt : access Event_Object_Record) return Event_Type_Enum;
   procedure Set_Event_Type
     (Evt        : access Event_Object_Record;
      Event_Type : Event_Type_Enum);
   
   --  function Get_Event_Type
   --    (Evt : Event_Object_Record) return Event_Type_Enum;
   --  procedure Set_Event_Type
   --    (Evt        : in out Event_Object_Record;
   --     Event_Type : Event_Type_Enum);
   -- Returns the name of the event.
     
     function Get_Object
     (Evt : access Event_Object_Record) return access Object_Record'Class;
   procedure Set_Object
     (Evt    : access Event_Object_Record;
      Object : access Object_Record'Class);
   -- User Object
      
   function Get_Time (Evt : access Event_Object_Record) return Integer;
   procedure Set_Time
     (Evt  : access Event_Object_Record;
      Time : Integer);
   -- Time when the event was fired
      
   function Get_Sender
     (Evt : access Event_Object_Record) return access Object_Record'Class;
   procedure Set_Sender
     (Evt    : access Event_Object_Record;
      Sender : access Object_Record'Class);
   -- The object origin of the event
      
   function Is_Consumed (Evt : access Event_Object_Record) return Boolean;
   -- Returns true if the event has been consumed.
   
   procedure Consume (Evt : access Event_Object_Record);
   -- Consumes the event.
   
private
   
   type Event_Object_Record is new Object_Record with record
      Event_Type : Event_Type_Enum;
      -- Holds the name of the event.
      
      Object : access Object_Record'Class;
      -- User Object
      
      Time : Integer;
      -- Time when the event was fired
      
      Sender : access Object_Record'Class;
      
      Consumed : Boolean;
      -- Holds the consumed state of the event. Default is false.
   end record;
   
   No_Event_Object_Record : constant Event_Object_Record := 
     Event_Object_Record'
     (No_Object_Record with 
	Event_Type => No_Event_Type,
      Object       => null,
      Time         => 0,
      Sender       => null,
      Consumed     => False);
   
   pragma Inline (Get_Event_Type);
   pragma Inline (Set_Event_Type);
   pragma Inline (Get_Object);
   pragma Inline (Set_Object);
   pragma Inline (Get_Time);
   pragma Inline (Set_Time);
   pragma Inline (Get_Sender);
   pragma Inline (Set_Sender);
   pragma Inline (Consume);
   pragma Inline (Is_Consumed);
   
end Artics.Generic_Events_Objects;
   
