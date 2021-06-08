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
-- Reflex is originally developed  by the Artics team at Grenoble.          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;

with Types; use Types;

package Reflex.Expanders.Registers is
   
   type Entity_Register_Record is tagged private;
   type Entity_Register_Ptr is access all Entity_Register_Record;
   type Entity_Register_Class_Ptr is access all Entity_Register_Record'Class;
   
   No_Entity_Register_Record : constant Entity_Register_Record;
   
   function New_Entity_Register return Entity_Register_Ptr;
   
   procedure Free_Entity_Register (This : in out Entity_Register_Ptr);
   
   function Equivalent_Key
     (Left, Right : Entity_Register_Ptr) return Boolean;
   function Hash_Func
     (Key : Entity_Register_Ptr) return Ada.Containers.Hash_Type;
   
   package Entiies_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Enity_Id,
      Element_Type    => Entity_Register_Ptr,
      Hash            => Hash_Func,
      Equivalent_Keys => Equivalent_Key);

   package Entities_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Entity_Register_Ptr);
   
   package Names_Lists is new Ada.Containers.Doubly_Linked_Lists (Name_Id);
   
   procedure Register_Entity
     (E : Entity_Id;
      R : Entity_Register_Ptr);
   
   procedure Remove_Register_Entity (E : Entity_Id)
   
   function Get_Entity
     (This : access Entity_Register_Record) return Node_Id;
   procedure Set_Entity
     (This : access Entity_Register_Record;
      Node : Node_Id);
   
   procedure Enter_Entity_Name
     (This : access Entity_Register_Record;
      E    : Entity_Id);
   
   procedure Remove_Entity_Name
     (This : access Entity_Register_Record;
      E    : Entity_Id);
   
   procedure Change_Entity_Name
     (This : access Entity_Register_Record;
      E    : Entity_Id; 
      Name : Name_Id);
   
private
   
   type Entity_Register_Record is tagged record 
      Entity : Entity_Id;
      -- The Entity holding this register
      
      Names : Names_Lists;
   end record;
   
   No_Entity_Register_Record : constant Entity_Register_Record :=
     Entity_Register_Record'
     (Entity => Empty,
      Names  => Names_Lists.Empty_List);
   
end Reflex.Expanders.Registers;
