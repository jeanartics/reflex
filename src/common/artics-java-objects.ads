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

with Artics.Objects; use Artics.Objects;
with Artics.Java.JNI; use Artics.Java.JNI;

package Artics.Java.Objects is
   
   type Java_Object_Record is new Object_Record with private;
   type Java_Object_Ptr is access all Java_Object_Record'Class;
   
   No_Java_Object_Record : constant Java_Object_Record;
   
   package Java_Objects_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Java_Object_Ptr);
   
   Jni_Env : Jni_Env_Access := null;
   
   procedure Local_To_Global_Java_Object 
     (Ada_Obj        : access Java_Object_Record'Class;
      Local_Java_Obj : J_Object);
   
   procedure Dispose_Java_Object (Ada_Obj : access Java_Object_Record'Class);
   
   function Get_Java_Object
     (Ada_Obj : access Java_Object_Record'Class) return J_Object;
   
   procedure Set_Java_Object
     (Ada_Obj     : access Java_Object_Record'Class;
      Java_Object : J_Object);
   
private
   
   type Java_Object_Record is new Object_Record with record
      Java_Object : J_Object;
   end record;
   
   No_Java_Object_Record : constant Java_Object_Record :=
     Java_Object_Record'(No_Object_Record with Java_Object => J_Null_Object);
     
end Artics.Java.Objects;

