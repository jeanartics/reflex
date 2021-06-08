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

with Ada.Finalization; use Ada.Finalization;

with Artics.Types; use Artics.Types;
with Artics.Namet; use Artics.Namet;

with Ada.Strings.Hash;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Artics.Objects is
   
   -- type Object_Record is new Controlled with private;
   type Object_Record is tagged private;
   type Object_Ptr is access all Object_Record'Class;
   type Object_Class_Ptr is access all Object_Record'Class;

   No_Object_Record : constant Object_Record;
   
 --  function To_String (This : access Object_Record) return String;
   
   function Clone (O : Object_Ptr) return Object_Ptr;
   
   function Equivalent_Key (Left, Right : Unbounded_String) return Boolean;
 
   function Hash_Func(Key : Unbounded_String) return Ada.Containers.Hash_Type;
   
   package Objects_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Object_Ptr,
      Hash            => Hash_Func,
      Equivalent_Keys => Equivalent_Key);
   
   package Objects_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Object_Ptr);
   
   function Get_Label_Name (This : access Object_Record) return String;
   function Get_Label_Name (This : access Object_Record) return Name_Id;
   
   procedure Set_Label_Name
     (This  : access Object_Record;
      Label : String);
   procedure Set_Label_Name
     (This  : access Object_Record;
      Label : Name_Id);
   
private
   
   -- type Object_Record is new Controlled with record 
   type Object_Record is tagged record 
      Label_Name : Name_Id;
   end record;
   
   --  No_Object_Record : constant Object_Record := Object_Record'
   --    (Controlled with Label_Name => No_Name);
   No_Object_Record : constant Object_Record := Object_Record'
     (Label_Name => No_Name);

end Artics.Objects;
