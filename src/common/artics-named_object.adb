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

package body Artics.Named_Object is

   -----------------------------
   -- New_Named_Object_Record --
   -----------------------------
   
   function New_Named_Object (Name : String) 
                              return access Named_Object_Record'Class
   is
      N    : Name_Id := String_Find (Name);
      This : access Named_Object_Record;
   begin
      This := new Named_Object_Record;
      This.Name := N;
      return This;
   end New_Named_Object;
   
   ---------------
   -- To_String --
   ---------------
   
   function To_String (This : access Named_Object_Record) return String
   is
   begin
      return Get_String (This.Name);
   end To_String;
   
   --------------
   -- Set_Name --
   --------------
   
   procedure Set_Name (This : access Named_Object_Record;
                       Name : String)
   is
   begin
      This.Name := String_Find (Name);
   end Set_Name;

   --------------
   -- Set_Name --
   --------------
   
   procedure Set_Name (This : access Named_Object_Record;
                       Name : Name_Id)
   is
   begin
      This.Name := Name;
   end Set_Name;
   
   --------------
   -- Get_Name --
   --------------
   
   function Get_Name (This : access Named_Object_Record) return Name_Id
   is
   begin
      return This.Name;
   end Get_Name;
   
   --------------
   -- Get_Name --
   --------------
   
   function Get_Name (This : access Named_Object_Record) return String
   is
   begin
      return Get_String (This.Name);
   end Get_Name;
   
end Artics.Named_Object;
