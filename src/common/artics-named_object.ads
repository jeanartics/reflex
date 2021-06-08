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

with Artics.Types;   use Artics.Types;
with Artics.Namet;   use Artics.Namet;
with Artics.Objects; use Artics.Objects;

package Artics.Named_Object is
   
   -- type Named_Object_Record 
   type Named_Object_Record is new Object_Record with private;
   type Named_Object_Ptr is access all Named_Object_Record'Class;
   
   function New_Named_Object (Name : String) 
                              return access Named_Object_Record'Class;
   -- Creates a new abject with a name.
   -- Can be used as Label, Tooltip etc.... for graph Values
   
   function To_String (This : access Named_Object_Record) return String;
   -- Return the name (mimics Java'a toString)
      
   procedure Set_Name (This : access Named_Object_Record;
                       Name : String);
   -- Set name with given String

   procedure Set_Name (This : access Named_Object_Record;
                       Name : Name_Id);   
   -- Set name with given name_id
   
   function Get_Name (This : access Named_Object_Record) return String;
   -- Return name as String
   
   function Get_Name (This : access Named_Object_Record) return Name_Id;
   -- Return name as Name_id
   
private
   
   -- type Named_Object_Record 
   type Named_Object_Record is new Object_Record with record
      Name : Name_Id := No_Name;
   end record;
end Artics.Named_Object;
