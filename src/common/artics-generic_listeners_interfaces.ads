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
with Artics.Generic_Events_Objects; 
--with Artics.Events_Tests_Types; use Artics.Events_Tests_Types;

-- Defines the requirements for an object that listens to an event source

generic 
   type Event_Type_Enum is (<>);
   with package Events_Objects is new Artics.Generic_Events_Objects
     (Event_Type_Enum);
   
package Artics.Generic_Listeners_Interfaces is
   
   type Listener_Interface is Interface;
   type Listener_Interface_Ptr is access all Listener_Interface'Class;
   
   procedure Invoke
     (Listener : access Listener_Interface;
      Sender   : access Object_Record'Class;
      Evt      : access Events_Objects.Event_Object_Record'Class) is abstract;
   -- Called when the graph model has changed. 
   -- @param sender Reference to the source of the event.
   -- @param evt Event object to be dispatched.
   
end Artics.Generic_Listeners_Interfaces;
