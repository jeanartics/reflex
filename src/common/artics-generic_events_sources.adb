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

with Ada.Text_Io; 

package body Artics.Generic_Events_Sources is
   
   procedure Put_Line(S:String) is null;-- renames Ada.Text_IO.Put_Line;
   ----------------------
   -- New_Event_Source --
   ----------------------
   
   function New_Event_Source return Event_Source_Ptr is
   begin
      return new Event_Source_Record'(No_Event_Source_Record);
   end New_Event_Source;
   
   ----------------------
   -- New_Event_Source --
   ----------------------
   
   function New_Event_Source
     (Source : access Object_Record'Class) return Event_Source_Ptr is
      Event  : Event_Source_Ptr :=  new Event_Source_Record;
   begin
      Event.Set_Sender (Source);
      return Event;
   end New_Event_Source;
   
   ---------------
   -- Listeners --
   ---------------
   
   function Get_Listeners
     (Event_Source : access Event_Source_Record) return Listeners_Lists.List is
   begin
      return Event_Source.Listeners;
   end Get_Listeners;
   
   -------------------
   -- Set_Listeners --
   -------------------
   
   procedure Set_Listeners
     (Event_Source : access Event_Source_Record;
      Listeners    : Listeners_Lists.List) is
   begin
      Event_Source.Listeners := Listeners;
   end Set_Listeners;
   
   ------------
   -- Sender --
   ------------
   
   function Get_Sender
     (Event_Source : access Event_Source_Record) 
     return access Object_Record'class is
   begin
      return Event_Source.Sender;
   end Get_Sender;
   
   ----------------
   -- Set_Sender --
   ----------------
   
   procedure Set_Sender
     (Event_Source : access Event_Source_Record;
      Sender       : access Object_Record'Class) is
   begin
      Event_Source.Sender := Sender;
   end Set_Sender;
   
   --------------------
   -- Events_Enabled --
   --------------------
   
   function Is_Events_Enabled
     (Event_Source : access Event_Source_Record) return Boolean is
   begin
      return Event_Source.Events_Enabled;
   end Is_Events_Enabled;
   
   ------------------------
   -- Set_Events_Enabled --
   ------------------------
   
   procedure Set_Events_Enabled
     (Event_Source : access Event_Source_Record;
      Value        : Boolean) is
   begin
      Event_Source.Events_Enabled := Value;
   end Set_Events_Enabled;
   
   ------------------
   -- Add_Listener --
   ------------------
   
   procedure Add_Listener
     (Event_Source : access Event_Source_Record;
      Evt_Type     : Event_Type_Enum;
      Listener     : access Listener_Interface'Class) is
   begin
      Listeners_Lists.Append
        (Event_Source.Listeners, Listener_Item_Record'(Evt_Type, Listener));
   end Add_Listener;
   
   ---------------------
   -- Remove_Listener --
   ---------------------
   
   procedure Remove_Listener
     (Event_Source : access Event_Source_Record;
      Listener     : access Listener_Interface'Class;
      Evt_Type     : Event_Type_Enum := No_Event_Type)
   is
      
      Cur         : Listeners_Lists.Cursor;
      Nxt         : Listeners_Lists.Cursor;
      Listen      : Listener_Item_Record;
      
      use Listeners_Lists;
   begin
      Put_Line ("remove_Listener Begin");
      if Event_Source.Listeners /= Listeners_Lists.Empty_List then
	 
	 Put_Line ("Evt_Type to remove => " & Evt_Type'Img);
	 
	 Cur := Listeners_Lists.First (Event_Source.Listeners);
	 while Listeners_Lists.Has_Element (Cur) loop
	    Nxt := Listeners_Lists.Next (Cur);
	    Listen := Listeners_Lists.Element (Cur);
	    
	    Put_Line ("Listen.Evt_Type => " & Listen.Evt_Type'Img);
	    if Listen.Listener = Listener then
	       Put_Line ("Listen = Listener");
	       if Evt_Type = No_Event_Type
		 or else Listen.Evt_Type = Evt_Type
	       then
		  Put_Line ("   Delete");
		  Listeners_Lists.Delete (Event_Source.Listeners, Cur);
	       end if;
	    end if;
	    
	    Cur := Nxt;
	 end loop;
      end if;
      Put_Line ("remove_Listener End");
     end Remove_Listener;
   
   ----------------
   -- Fire_Event --
   ----------------
   
   procedure Fire_Event
     (Event_Source : access Event_Source_Record;
      Event_Object : access Events_Objects.Event_Object_Record'Class) is
   begin
      Fire_Event (Event_Source, Event_Object, null);
   end Fire_Event;
   
   ----------------
   -- Fire_Event --
   ----------------
   
   procedure Fire_Event
     (Event_Source  : access Event_Source_Record;
      Event_Object  : access Events_Objects.Event_Object_Record'Class;
      Sender_Source : access Object_Record'Class) is
      
      Send        : access Object_Record'Class := Sender_Source;
      Cur         : Listeners_Lists.Cursor;
      Source_Type : Event_Type_Enum;
      T           : Event_Type_Enum := Get_Event_Type (Event_Object);
   begin
      if Event_Source.Listeners /= Listeners_Lists.Empty_List
	and Event_Source.Events_Enabled 
      then
	 if Send = null then
	    Send := Event_Source.Sender;
	 end if;
	 
	 if Send = null then
	    Send := Event_Object.Get_Sender;
	 end if;
	 
	 Source_Type := Get_Event_Type (Event_Object);
         
	 for Listen of Event_Source.Listeners loop
            if Listen.Evt_Type = Source_Type then
	       declare
		  Listener : access Listener_Interface'Class := 
		    Listen.Listener;
	       begin
                  Listener.Invoke (Send, Event_Object);
               end;
	    end if;
	 end loop;
      end if;
   end Fire_Event;
   
end Artics.Generic_Events_Sources;
