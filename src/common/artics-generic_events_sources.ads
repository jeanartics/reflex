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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;

with Artics.Types; use Artics.Types;
with Artics.Namet; use Artics.Namet;
with Artics.Objects; use Artics.Objects;
with Artics.Generic_Listeners_Interfaces;
with Artics.Generic_Events_Objects;

generic
   type Event_Type_Enum is (<>);
   
   with package Events_Objects is
     new Generic_Events_Objects (Event_Type_Enum); 
   
   with package Listeners_Interfaces is 
     new Artics.Generic_Listeners_Interfaces (Event_Type_Enum, Events_Objects);
package Artics.Generic_Events_Sources is
   
   use Events_Objects;
   use Listeners_Interfaces;
   
   type Listener_Item_Record is record
      Evt_Type : Event_Type_Enum;
      Listener : access Listener_Interface'Class;
   end record;
   
   package Listeners_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Listener_Item_Record);
   use Listeners_Lists;
     
   type Event_Source_Record is new Object_Record with private;
   type Event_Source_Ptr is access all Event_Source_Record;
   
   No_Event_Source_Record : constant Event_Source_Record;
   
   function New_Event_Source return Event_Source_Ptr;
   -- Constructs a new event source using this as the source object.
   
   function New_Event_Source 
     (Source : access Object_Record'Class) return Event_Source_Ptr;
   -- Constructs a new event source using this as the source object.
   
   function Get_Listeners
     (Event_Source : access Event_Source_Record) return Listeners_Lists.List;
   
   procedure Set_Listeners
     (Event_Source : access Event_Source_Record;
      Listeners    : Listeners_Lists.List);
   -- Holds the event names and associated listeners in an array. The array
   -- contains the event name followed by the respective listener for each
   -- registered listener.
	
   function Get_Sender
     (Event_Source : access Event_Source_Record)
     return access Object_Record'Class;
   
   procedure Set_Sender
     (Event_Source : access Event_Source_Record;
      Sender       : access Object_Record'Class);
   -- Holds the source object for this event source.
      
   function Is_Events_Enabled
     (Event_Source : access Event_Source_Record) return Boolean;
   procedure Set_Events_Enabled
     (Event_Source : access Event_Source_Record;
      Value        : Boolean);
   -- Specifies if events can be fired. Default is true.
   
   procedure Add_Listener
     (Event_Source : access Event_Source_Record;
      Evt_type     : Event_Type_Enum;
      Listener     : access Listener_Interface'Class);
   -- Binds the specified function to the given event name. If no event name
   -- is given, then the listener is registered for all events.
   
   procedure Remove_Listener
     (Event_Source : access Event_Source_Record;
      Listener     : access Listener_Interface'Class;
      Evt_Type     : Event_Type_Enum := No_Event_Type);
   -- Removes all occurances of the given listener from the list of listeners.
   
   procedure Fire_Event
     (Event_Source : access Event_Source_Record;
      Event_Object : access Events_Objects.Event_Object_Record'Class);
   -- Dispatches the given event name with this object as the event source.
   -- <code>
   -- fireEvent(new mxEventObject("eventName", key1, val1, .., keyN, valN))
   -- </code>
   
   procedure Fire_Event
     (Event_Source  : access Event_Source_Record;
      Event_Object  : access Events_Objects.Event_Object_Record'Class;
      Sender_Source : access Object_Record'Class);
   -- Dispatches the given event name, passing all arguments after the given
   -- name to the registered listeners for the event.
   
private
   
   type Event_Source_Record is new Object_Record with record
      Listeners : Listeners_Lists.List;
      -- Holds the event names and associated listeners in an array. The array
      -- contains the event name followed by the respective listener for each
      -- registered listener.
      
      Sender : access Object_Record'Class;
      -- Holds the source object for this event source.
      
      Events_Enabled : Boolean;
      -- Specifies if events can be fired. Default is true.
   end record;
   
   No_Event_Source_Record : constant Event_Source_Record := Event_Source_Record'
     (No_Object_Record with
	Listeners    => Listeners_Lists.Empty_List,
      Sender         => null,
      Events_Enabled => True);
   
end Artics.Generic_Events_Sources;
