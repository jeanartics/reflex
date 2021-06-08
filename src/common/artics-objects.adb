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

package body Artics.Objects is
   
   ---------------
   -- To_String --
   ---------------
   
--   function To_String (This : access Object_Record) return String
--   is
--   begin
--      return "";
--   end To_String;
   
   -----------
   -- Clone --
   -----------
   
   function Clone (O : Object_Ptr) return Object_Ptr is
   begin
      return new Object_Record'(No_Object_Record);
   end Clone;
   
   -------------------
   -- Hash_Function --
   -------------------
   
   function Equivalent_Key (Left, Right : Unbounded_String) return Boolean is
   begin
      return Left = Right;
   end Equivalent_Key;
 
   function Hash_Func(Key : Unbounded_String) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash(To_String(Key));
   end Hash_Func;
   
   --------------------
   -- Get_Label_Name --
   --------------------
   
   function Get_Label_Name (This : access Object_Record) return Name_Id is
   begin
      return This.Label_Name;
   end Get_Label_Name;
   
   --------------------
   -- Get_Label_Name --
   --------------------
   
   function Get_Label_Name (This : access Object_Record) return String is
      Label : Name_Id := Get_Label_Name (This);
   begin
      return Get_String (Label);
   end Get_Label_Name;
   
   --------------------
   -- Set_Label_Name --
   --------------------
   
   procedure Set_Label_Name
     (This  : access Object_Record;
      Label : Name_Id) is
   begin
      This.Label_Name := Label;
   end Set_Label_Name;

   --------------------
   -- Set_Label_Name --
   --------------------
   
   procedure Set_Label_Name
     (This  : access Object_Record;
      Label : String) is
   begin
      Set_Label_Name (This, String_Find (Label));
   end Set_Label_Name;
   
end Artics.Objects;
