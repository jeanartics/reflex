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

package body Artics.Java.Objects is
   
   ---------------------------------
   -- Local_To_Global_Java_Object --
   ---------------------------------
   
   procedure Local_To_Global_Java_Object 
     (Ada_Obj        : access Java_Object_Record'Class;
      Local_Java_Obj : J_Object) is
      
      Global : J_Object;
   begin
      Global := New_Global_Ref
	(Env  => JNI_Env,
	 Lobj => Local_Java_Obj);
      
      Set_Java_Object (Ada_Obj, Global);
   end Local_To_Global_Java_Object;
   
   -------------------------
   -- Dispose_Java_Object --
   -------------------------
   
   procedure Dispose_Java_Object (Ada_Obj : access Java_Object_Record'Class) is
      Obj : J_Object := Ada_Obj.Java_Object;
   begin
      if Obj /= J_Null_Object then
	 Delete_Global_Ref (Jni_Env, Obj);
	 Set_Java_Object (Ada_Obj, J_Null_Object);
      end if;
   end Dispose_Java_Object;
   
   ---------------------
   -- Get_Java_Object --
   ---------------------
   
   function Get_Java_Object
     (Ada_Obj : access Java_Object_Record'Class) return J_Object is
   begin
      return Ada_Obj.Java_Object;
   end Get_Java_Object;
   
   ---------------------
   -- Set_Java_Object --
   ---------------------
   
   procedure Set_Java_Object
     (Ada_Obj     : access Java_Object_Record'Class;
      Java_Object : J_Object) is
   begin
      Ada_Obj.Java_Object := Java_Object;
   end Set_Java_Object;
   
end Artics.Java.Objects;

