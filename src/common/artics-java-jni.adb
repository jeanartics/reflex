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

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Artics.Java.JNI is

   ---------------------
   -- Destroy_Java_VM --
   ---------------------

   function Destroy_Java_VM
     (VM : Java_VM_Access)
      return J_Int is
   begin
      return VM.all.all.Destroy_Java_VM (VM);
   end Destroy_Java_VM;

   ---------------------------
   -- Attach_Current_Thread --
   ---------------------------

   function Attach_Current_Thread
     (VM    : Java_VM_Access;
      P_Env : access JNI_Env_Access;
      Args  : System.Address)
      return J_Int is
   begin
      return VM.all.all.Attach_Current_Thread (VM, P_Env, Args);
   end Attach_Current_Thread;

   ---------------------------
   -- Detach_Current_Thread --
   ---------------------------

   function Detach_Current_Thread
     (VM : Java_VM_Access)
      return J_Int is
   begin
      return VM.all.all.Detach_Current_Thread (VM);
   end Detach_Current_Thread;

   -------------
   -- Get_Env --
   -------------

   function Get_Env (VM      : Java_VM_Access;
                     Penv    : access JNI_Env_Access;
                     Version : J_Int := JNI_Version_1_2)
                     return J_Int
   is
   begin
      return VM.all.all.Get_Env (VM, Penv, Version);
   exception
      when Constraint_Error =>
         raise Program_Error with
         "The VM parameter in JNI.Operations.Get_Env must not be null.";
   end Get_Env;

   -------------------------------------
   -- Attach_Current_Thread_As_Daemon --
   -------------------------------------

   function Attach_Current_Thread_As_Daemon
     (VM : Java_VM_Access;
      P_Env : JNI_Env_Access;
      Args  : System.Address)
      return J_Int is
   begin
      return VM.all.all.Attach_Current_Thread_As_Daemon (VM, P_Env, Args);
   end Attach_Current_Thread_As_Daemon;

   -----------------
   -- Get_Version --
   -----------------

   function Get_Version
     (Env : JNI_Env_Access)
      return J_Int is
   begin
      return Env.all.all.Get_Version (Env);
   end Get_Version;

   ------------------
   -- Define_Class --
   ------------------

   function Define_Class
     (Env    : JNI_Env_Access;
      Name   : String;
      Loader : J_Object;
      Buf    : J_Byte_Array;
      Len    : J_Size)
      return J_Class is
   begin
      return Env.all.all.Define_Class (Env, Name, Loader, Buf, Len);
   end Define_Class;

   ----------------
   -- Find_Class --
   ----------------

   function Find_Class (Env  : JNI_Env_Access;
                        Name : String)
                        return J_Class
   is
   begin
      return Env.all.all.Find_Class (Env, Name & ASCII.NUL);
   end Find_Class;

   ---------------------------
   -- From_Reflected_Method --
   ---------------------------

   function From_Reflected_Method
     (Env    : JNI_Env_Access;
      Method : J_Object)
      return J_Method_ID is
   begin
      return Env.all.all.From_Reflected_Method (Env, Method);
   end From_Reflected_Method;

   --------------------------
   -- From_Reflected_Field --
   --------------------------

   function From_Reflected_Field
     (Env   : JNI_Env_Access;
      Field : J_Object)
      return J_Field_ID is
   begin
      return Env.all.all.From_Reflected_Field (Env, Field);
   end From_Reflected_Field;

   -------------------------
   -- To_Reflected_Method --
   -------------------------

   function To_Reflected_Method
     (Env       : JNI_Env_Access;
      Cls       : J_Class;
      Method_ID : J_Method_ID;
      Is_Static : J_Boolean)
      return J_Object is
   begin
      return Env.all.all.To_Reflected_Method (Env, Cls, Method_ID, Is_Static);
   end To_Reflected_Method;

   --------------------
   -- Get_Superclass --
   --------------------

   function Get_Superclass
     (Env : JNI_Env_Access;
      Sub : J_Class)
      return J_Class is
   begin
      return Env.all.all.Get_Superclass (Env, Sub);
   end Get_Superclass;

   ------------------------
   -- Is_Assignable_From --
   ------------------------

   function Is_Assignable_From
     (Env : JNI_Env_Access;
      Sub : J_Class;
      Sup : J_Class)
      return J_Boolean is
   begin
      return Env.all.all.Is_Assignable_From (Env, Sub, Sup);
   end Is_Assignable_From;

   ------------------------
   -- To_Reflected_Field --
   ------------------------

   function To_Reflected_Field
     (Env       : JNI_Env_Access;
      Cls       : J_Class;
      Field_ID  : J_Field_ID;
      Is_Static : J_Boolean)
      return J_Object is
   begin
      return Env.all.all.To_Reflected_Field (Env, Cls, Field_ID, Is_Static);
   end To_Reflected_Field;

   -----------
   -- Throw --
   -----------

   function Throw
     (Env : JNI_Env_Access;
      Obj : J_Throwable)
      return J_Int is
   begin
      return Env.all.all.Throw (Env, Obj);
   end Throw;

   ---------------
   -- Throw_New --
   ---------------

   function Throw_New
     (Env   : JNI_Env_Access;
      Class : J_Class;
      Msg   : String)
      return J_Int is
   begin
      return Env.all.all.Throw_New (Env, Class, Msg & ASCII.NUL);
   end Throw_New;

   ------------------------
   -- Exception_Occurred --
   ------------------------

   function Exception_Occurred (Env : JNI_Env_Access) return J_Throwable
   is
   begin
      return Env.all.all.Exception_Occurred (Env);
   end Exception_Occurred;

   ------------------------
   -- Exception_Describe --
   ------------------------

   procedure Exception_Describe (Env : JNI_Env_Access)
   is
   begin
      Env.all.all.Exception_Describe (Env);
   end Exception_Describe;

   ---------------------
   -- Exception_Clear --
   ---------------------

   procedure Exception_Clear (Env : JNI_Env_Access) is
   begin
      Env.all.all.Exception_Clear (Env);
   end Exception_Clear;

   -----------------
   -- Fatal_Error --
   -----------------

   procedure Fatal_Error
     (Env : JNI_Env_Access;
      Msg : String) is
   begin
      Env.all.all.Fatal_Error (Env, Msg & ASCII.NUL);
   end Fatal_Error;

   ----------------------
   -- Push_Local_Frame --
   ----------------------

   function Push_Local_Frame
     (Env      : JNI_Env_Access;
      Capacity : J_Int)
      return J_Int is
   begin
      return Env.all.all.Push_Local_Frame (Env, Capacity);
   end Push_Local_Frame;

   ---------------------
   -- Pop_Local_Frame --
   ---------------------

   function Pop_Local_Frame
     (Env    : JNI_Env_Access;
      Result : J_Object)
      return J_Object is
   begin
      return Env.all.all.Pop_Local_Frame (Env, Result);
   end Pop_Local_Frame;

   --------------------
   -- New_Global_Ref --
   --------------------

   function New_Global_Ref (Env  : JNI_Env_Access;
                            Lobj : J_Object)
                            return J_Object
   is
   begin
      return Env.all.all.New_Global_Ref (Env, Lobj);
   end New_Global_Ref;

   -----------------------
   -- Delete_Global_Ref --
   -----------------------

   procedure Delete_Global_Ref (Env  : JNI_Env_Access;
                                Gref : J_Object)
   is
   begin
      Env.all.all.Delete_Global_Ref (Env, Gref);
   end Delete_Global_Ref;

   -----------------------
   -- Delete_Global_Ref --
   -----------------------

   procedure Delete_Local_Ref (Env : JNI_Env_Access;
                               Obj : J_Object)
   is
   begin
      Env.all.all.Delete_Local_Ref (Env, Obj);
   end Delete_Local_Ref;

   function Is_Same_Object
     (Env   : JNI_Env_Access;
      Obj_1 : J_Object;
      Obj_2 : J_Object)
      return J_Boolean is
   begin
      return Env.all.all.Is_Same_Object (Env, Obj_1, Obj_2);
   end Is_Same_Object;

   -------------------
   -- New_Local_Ref --
   -------------------

   function New_Local_Ref (Env : JNI_Env_Access;
                           Ref : J_Object)
                           return J_Object
   is
   begin
      return Env.all.all.New_Local_Ref (Env, Ref);
   end New_Local_Ref;

   ---------------------------
   -- Ensure_Local_Capacity --
   ---------------------------

   function Ensure_Local_Capacity
     (Env      : JNI_Env_Access;
      Capacity : J_Int)
      return J_Int is
   begin
      return Env.all.all.Ensure_Local_Capacity (Env, Capacity);
   end Ensure_Local_Capacity;

   ------------------
   -- Alloc_Object --
   ------------------

   function Alloc_Object
     (Env   : JNI_Env_Access;
      Class : J_Class)
      return J_Object is
   begin
      return Env.all.all.Alloc_Object (Env, Class);
   end Alloc_Object;

   ------------------
   -- New_Object_A --
   ------------------

   function New_Object_A (Env       : JNI_Env_Access;
                          Class     : J_Class;
                          Method_ID : J_Method_ID;
                          Args      : J_Value_Array)
                          return J_Object
   is
   begin
      return Env.all.all.New_Object_A (Env, Class, Method_ID, Args);
   end New_Object_A;

   ----------------------
   -- Get_Object_Class --
   ----------------------

   function Get_Object_Class
     (Env : JNI_Env_Access;
      Obj : J_Object)
      return J_Class is
   begin
      return Env.all.all.Get_Object_Class (Env, Obj);
   end Get_Object_Class;

   --------------------
   -- Is_Instance_Of --
   --------------------

   function Is_Instance_Of
     (Env   : JNI_Env_Access;
      Obj   : J_Object;
      Class : J_Class)
      return J_Boolean is
   begin
      return Env.all.all.Is_Instance_Of (Env, Obj, Class);
   end Is_Instance_Of;

   -------------------
   -- Get_Method_ID --
   -------------------

   function Get_Method_ID (Env   : JNI_Env_Access;
                           Class : J_Class;
                           Name  : String;
                           Sig   : String)
                           return J_Method_ID
   is
   begin
      return Env.all.all.Get_Method_ID
	(Env, Class, Name & ASCII.NUL, Sig & ASCII.NUL);
   end Get_Method_ID;

   --------------------------
   -- Call_Object_Method_A --
   --------------------------

   function Call_Object_Method_A (Env       : JNI_Env_Access;
                                  Obj       : J_Object;
                                  Method_ID : J_Method_ID;
                                  Args      : J_Value_Array)
                                  return J_Object
   is
   begin
      return Env.all.all.Call_Object_Method_A (Env, Obj, Method_ID, Args);
   end Call_Object_Method_A;

   ---------------------------
   -- Call_Boolean_Method_A --
   ---------------------------

   function Call_Boolean_Method_A (Env       : JNI_Env_Access;
                                   Obj       : J_Object;
                                   Method_ID : J_Method_ID;
                                   Args      : J_Value_Array)
                                   return J_Boolean
   is
   begin
      return Env.all.all.Call_Boolean_Method_A (Env, Obj, Method_ID, Args);
   end Call_Boolean_Method_A;

   ------------------------
   -- Call_Byte_Method_A --
   ------------------------

   function Call_Byte_Method_A (Env       : JNI_Env_Access;
                                Obj       : J_Object;
                                Method_ID : J_Method_ID;
                                Args      : J_Value_Array)
                                return J_Byte
   is
   begin
      return Env.all.all.Call_Byte_Method_A (Env, Obj, Method_ID, Args);
   end Call_Byte_Method_A;

   ------------------------
   -- Call_Char_Method_A --
   ------------------------

   function Call_Char_Method_A (Env       : JNI_Env_Access;
                                Obj       : J_Object;
                                Method_ID : J_Method_ID;
                                Args      : J_Value_Array)
                                return J_Char
   is
   begin
      return Env.all.all.Call_Char_Method_A (Env, Obj, Method_ID, Args);
   end Call_Char_Method_A;

   --------------------------
   --  Call_Short_Method_A --
   --------------------------

   function Call_Short_Method_A (Env       : JNI_Env_Access;
                                 Obj       : J_Object;
                                 Method_ID : J_Method_ID;
                                 Args      : J_Value_Array)
                                 return J_Short
   is
   begin
      return Env.all.all.Call_Short_Method_A (Env, Obj, Method_ID, Args);
   end Call_Short_Method_A;

   -----------------------
   -- Call_Int_Method_A --
   -----------------------

   function Call_Int_Method_A (Env       : JNI_Env_Access;
                               Obj       : J_Object;
                               Method_ID : J_Method_ID;
                               Args      : J_Value_Array)
                               return J_Int
   is
   begin
      return Env.all.all.Call_Int_Method_A (Env, Obj, Method_ID, Args);
   end Call_Int_Method_A;

   ------------------------
   -- Call_Long_Method_A --
   ------------------------

   function Call_Long_Method_A (Env       : JNI_Env_Access;
                                Obj       : J_Object;
                                Method_ID : J_Method_ID;
                                Args      : J_Value_Array)
                                return J_Long
   is
   begin
      return Env.all.all.Call_Long_Method_A (Env, Obj, Method_ID, Args);
   end Call_Long_Method_A;

   -------------------------
   -- Call_Float_Method_A --
   -------------------------

   function Call_Float_Method_A (Env       : JNI_Env_Access;
                                 Obj       : J_Object;
                                 Method_ID : J_Method_ID;
                                 Args      : J_Value_Array)
                                 return J_Float
   is
   begin
      return Env.all.all.Call_Float_Method_A (Env, Obj, Method_ID, Args);
   end Call_Float_Method_A;

   --------------------------
   -- Call_Double_Method_A --
   --------------------------

   function Call_Double_Method_A (Env       : JNI_Env_Access;
                                  Obj       : J_Object;
                                  Method_ID : J_Method_ID;
                                  Args      : J_Value_Array)
                                  return J_Double
   is
   begin
      return Env.all.all.Call_Double_Method_A (Env, Obj, Method_ID, Args);
   end Call_Double_Method_A;

   ------------------------
   -- Call_Void_Method_A --
   ------------------------

   procedure Call_Void_Method_A (Env       : JNI_Env_Access;
                                 Obj       : J_Object;
                                 Method_ID : J_Method_ID;
                                 Args      : J_Value_Array)
   is
   begin
      Env.all.all.Call_Void_Method_A (Env, Obj, Method_ID, Args);
   end Call_Void_Method_A;

   --------------------------------------
   -- Call_Non_Virtual_Object_Method_A --
   --------------------------------------

   function Call_Non_Virtual_Object_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array)
      return J_Object is
   begin
      return Env.all.all.Call_Non_Virtual_Object_Method_A
        (Env, Obj, Method_ID, Args);
   end Call_Non_Virtual_Object_Method_A;

   ---------------------------------------
   -- Call_Non_Virtual_Boolean_Method_A --
   ---------------------------------------

   function Call_Non_Virtual_Boolean_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array)
      return J_Boolean is
   begin
      return Env.all.all.Call_Non_Virtual_Boolean_Method_A
        (Env, Obj, Method_ID, Args);
   end Call_Non_Virtual_Boolean_Method_A;

   ------------------------------------
   -- Call_Non_Virtual_Byte_Method_A --
   ------------------------------------

   function Call_Non_Virtual_Byte_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array)
      return J_Byte is
   begin
      return Env.all.all.Call_Non_Virtual_Byte_Method_A
        (Env, Obj, Method_ID, Args);
   end Call_Non_Virtual_Byte_Method_A;

   ------------------------------------
   -- Call_Non_Virtual_Char_Method_A --
   ------------------------------------

   function Call_Non_Virtual_Char_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array)
      return J_Char is
   begin
      return Env.all.all.Call_Non_Virtual_Char_Method_A
        (Env, Obj, Method_ID, Args);
   end Call_Non_Virtual_Char_Method_A;

   -------------------------------------
   -- Call_Non_Virtual_Short_Method_A --
   -------------------------------------

   function Call_Non_Virtual_Short_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array)
      return J_Short is
   begin
      return Env.all.all.Call_Non_Virtual_Short_Method_A
        (Env, Obj, Method_ID, Args);
   end Call_Non_Virtual_Short_Method_A;

   -----------------------------------
   -- Call_Non_Virtual_Int_Method_A --
   -----------------------------------

   function Call_Non_Virtual_Int_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array)
      return J_Int is
   begin
      return Env.all.all.Call_Non_Virtual_Int_Method_A
        (Env, Obj, Method_ID, Args);
   end Call_Non_Virtual_Int_Method_A;

   ------------------------------------
   -- Call_Non_Virtual_Long_Method_A --
   ------------------------------------

   function Call_Non_Virtual_Long_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array)
      return J_Long is
   begin
      return Env.all.all.Call_Non_Virtual_Long_Method_A
        (Env, Obj, Method_ID, Args);
   end Call_Non_Virtual_Long_Method_A;

   -------------------------------------
   -- Call_Non_Virtual_Float_Method_A --
   -------------------------------------

   function Call_Non_Virtual_Float_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array)
      return J_Float is
   begin
      return Env.all.all.Call_Non_Virtual_Float_Method_A
        (Env, Obj, Method_ID, Args);
   end Call_Non_Virtual_Float_Method_A;

   --------------------------------------
   -- Call_Non_Virtual_Double_Method_A --
   --------------------------------------

   function Call_Non_Virtual_Double_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array)
      return J_Double is
   begin
      return Env.all.all.Call_Non_Virtual_Double_Method_A
        (Env, Obj, Method_ID, Args);
   end Call_Non_Virtual_Double_Method_A;

   ------------------------------------
   -- Call_Non_Virtual_Void_Method_A --
   ------------------------------------

   procedure Call_Non_Virtual_Void_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) is
   begin
      Env.all.all.Call_Non_Virtual_Void_Method_A
        (Env, Obj, Method_ID, Args);
   end Call_Non_Virtual_Void_Method_A;

   ------------------
   -- Get_Field_ID --
   ------------------

   function Get_Field_ID (Env       : JNI_Env_Access;
                          Class     : J_Class;
                          Name      : String;
                          Sig       : String)
                          return J_Field_ID
   is
   begin
      return Env.all.all.Get_Field_ID
	(Env, Class, Name & ASCII.NUL, Sig & ASCII.NUL);
   end Get_Field_ID;

   ----------------------
   -- Get_Object_Field --
   ----------------------

   function Get_Object_Field (Env      : JNI_Env_Access;
                              Obj      : J_Object;
                              Field_ID : J_Field_ID)
                              return J_Object
   is
   begin
      return Env.all.all.Get_Object_Field (Env, Obj, Field_ID);
   end Get_Object_Field;

   -----------------------
   -- Get_Boolean_Field --
   -----------------------

   function Get_Boolean_Field (Env      : JNI_Env_Access;
                               Obj      : J_Object;
                               Field_ID : J_Field_ID)
                               return J_Boolean
   is
   begin
      return Env.all.all.Get_Boolean_Field (Env, Obj, Field_ID);
   end Get_Boolean_Field;

   --------------------
   -- Get_Byte_Field --
   --------------------

   function Get_Byte_Field (Env      : JNI_Env_Access;
                            Obj      : J_Object;
                            Field_ID : J_Field_ID)
                            return J_Byte
   is
   begin
      return Env.all.all.Get_Byte_Field (Env, Obj, Field_ID);
   end Get_Byte_Field;

   --------------------
   -- Get_Char_Field --
   --------------------

   function Get_Char_Field (Env      : JNI_Env_Access;
                            Obj      : J_Object;
                            Field_ID : J_Field_ID)
                            return J_Char
   is
   begin
      return Env.all.all.Get_Char_Field (Env, Obj, Field_ID);
   end Get_Char_Field;

   ---------------------
   -- Get_Short_Field --
   ---------------------

   function Get_Short_Field (Env      : JNI_Env_Access;
                             Obj      : J_Object;
                             Field_ID : J_Field_ID)
                             return J_Short
   is
   begin
      return Env.all.all.Get_Short_Field (Env, Obj, Field_ID);
   end Get_Short_Field;

   -------------------
   -- Get_Int_Field --
   -------------------

   function Get_Int_Field (Env      : JNI_Env_Access;
                           Obj      : J_Object;
                           Field_ID : J_Field_ID)
                           return J_Int
   is
   begin
      return Env.all.all.Get_Int_Field (Env, Obj, Field_ID);
   end Get_Int_Field;

   --------------------
   -- Get_Long_Field --
   --------------------

   function Get_Long_Field (Env      : JNI_Env_Access;
                            Obj      : J_Object;
                            Field_ID : J_Field_ID)
                            return J_Long
   is
   begin
      return Env.all.all.Get_Long_Field (Env, Obj, Field_ID);
   end Get_Long_Field;

   ---------------------
   -- Get_Float_Field --
   ---------------------

   function Get_Float_Field (Env      : JNI_Env_Access;
                             Obj      : J_Object;
                             Field_ID : J_Field_ID)
                              return J_Float
   is
   begin
      return Env.all.all.Get_Float_Field (Env, Obj, Field_ID);
   end Get_Float_Field;

   ----------------------
   -- Get_Double_Field --
   ----------------------

   function Get_Double_Field (Env      : JNI_Env_Access;
                              Obj      : J_Object;
                              Field_ID : J_Field_ID)
                              return J_Double
   is
   begin
      return Env.all.all.Get_Double_Field (Env, Obj, Field_ID);
   end Get_Double_Field;

   ----------------------
   -- Set_Object_Field --
   ----------------------

   procedure Set_Object_Field (Env      : JNI_Env_Access;
                               Obj      : J_Object;
                               Field_ID : J_Field_ID;
                               Val      : J_Object)
   is
   begin
      Env.all.all.Set_Object_Field (Env, Obj, Field_ID, Val);
   end Set_Object_Field;

   -----------------------
   -- Set_Boolean_Field --
   -----------------------

   procedure Set_Boolean_Field (Env      : JNI_Env_Access;
                                Obj      : J_Object;
                                Field_ID : J_Field_ID;
                                Val      : J_Boolean)
   is
   begin
      Env.all.all.Set_Boolean_Field (Env, Obj, Field_ID, Val);
   end Set_Boolean_Field;

   --------------------
   -- Set_Byte_Field --
   --------------------

   procedure Set_Byte_Field (Env      : JNI_Env_Access;
                             Obj      : J_Object;
                             Field_ID : J_Field_ID;
                             Val      : J_Byte)
   is
   begin
      Env.all.all.Set_Byte_Field (Env, Obj, Field_ID, Val);
   end Set_Byte_Field;

   --------------------
   -- Set_Char_Field --
   --------------------

   procedure Set_Char_Field (Env      : JNI_Env_Access;
                             Obj      : J_Object;
                             Field_ID : J_Field_ID;
                             Val      : J_Char)
   is
   begin
      Env.all.all.Set_Char_Field (Env, Obj, Field_ID, Val);
   end Set_Char_Field;

   ---------------------
   -- Set_Short_Field --
   ---------------------

   procedure Set_Short_Field (Env      : JNI_Env_Access;
                              Obj      : J_Object;
                              Field_ID : J_Field_ID;
                              Val      : J_Short)
   is
   begin
      Env.all.all.Set_Short_Field (Env, Obj, Field_ID, Val);
   end Set_Short_Field;

   -------------------
   -- Set_Int_Field --
   -------------------

   procedure Set_Int_Field (Env      : JNI_Env_Access;
                            Obj      : J_Object;
                            Field_ID : J_Field_ID;
                            Val      : J_Int)
   is
   begin
      Env.all.all.Set_Int_Field (Env, Obj, Field_ID, Val);
   end Set_Int_Field;

   --------------------
   -- Set_Long_Field --
   --------------------

   procedure Set_Long_Field (Env      : JNI_Env_Access;
                             Obj      : J_Object;
                             Field_ID : J_Field_ID;
                             Val      : J_Long)
   is
   begin
      Env.all.all.Set_Long_Field (Env, Obj, Field_ID, Val);
   end Set_Long_Field;

   ---------------------
   -- Set_Float_Field --
   ---------------------

   procedure Set_Float_Field (Env      : JNI_Env_Access;
                              Obj      : J_Object;
                              Field_ID : J_Field_ID;
                              Val      : J_Float)
   is
   begin
      Env.all.all.Set_Float_Field (Env, Obj, Field_ID, Val);
   end Set_Float_Field;

   ----------------------
   -- Set_Double_Field --
   ----------------------

   procedure Set_Double_Field (Env      : JNI_Env_Access;
                               Obj      : J_Object;
                               Field_ID : J_Field_ID;
                               Val      : J_Double)
   is
   begin
      Env.all.all.Set_Double_Field (Env, Obj, Field_ID, Val);
   end Set_Double_Field;

   --------------------------
   -- Get_Static_Method_ID --
   --------------------------

   function Get_Static_Method_ID (Env   : JNI_Env_Access;
                                  Class : J_Class;
                                  Name  : String;
                                  Sig   : String)
                                  return J_Method_ID
   is
   begin
      return Env.all.all.Get_Static_Method_ID
	(Env, Class, Name & ASCII.NUL, Sig & ASCII.NUL);
   end  Get_Static_Method_ID;

   ---------------------------------
   -- Call_Static_Object_Method_A --
   ---------------------------------

   function Call_Static_Object_Method_A (Env       : JNI_Env_Access;
                                         Class     : J_Class;
                                         Method_ID : J_Method_ID;
                                         Args      : J_Value_Array)
                                         return J_Object
   is
   begin
      return Env.all.all.Call_Static_Object_Method_A
        (Env, Class, Method_ID, Args);
   end Call_Static_Object_Method_A;

   ----------------------------------
   -- Call_Static_Boolean_Method_A --
   ----------------------------------

   function Call_Static_Boolean_Method_A (Env       : JNI_Env_Access;
                                          Class     : J_Class;
                                          Method_ID : J_Method_ID;
                                          Args      : J_Value_Array)
                                          return J_Boolean
   is
   begin
      return Env.all.all.Call_Static_Boolean_Method_A
        (Env, Class, Method_ID, Args);
   end Call_Static_Boolean_Method_A;

   -------------------------------
   -- Call_Static_Byte_Method_A --
   --------------------------------

   function Call_Static_Byte_Method_A (Env       : JNI_Env_Access;
                                       Class     : J_Class;
                                       Method_ID : J_Method_ID;
                                       Args      : J_Value_Array)
                                       return J_Byte
   is
   begin
      return Env.all.all.Call_Static_Byte_Method_A
        (Env, Class, Method_ID, Args);
   end Call_Static_Byte_Method_A;

   -------------------------------
   -- Call_Static_Char_Method_A --
   -------------------------------

   function Call_Static_Char_Method_A (Env       : JNI_Env_Access;
                                       Class     : J_Class;
                                       Method_ID : J_Method_ID;
                                       Args      : J_Value_Array)
                                       return J_Char
   is
   begin
      return Env.all.all.Call_Static_Char_Method_A
        (Env, Class, Method_ID, Args);
   end Call_Static_Char_Method_A;

   ---------------------------------
   --  Call_Static_Short_Method_A --
   ---------------------------------

   function Call_Static_Short_Method_A (Env       : JNI_Env_Access;
                                        Class     : J_Class;
                                        Method_ID : J_Method_ID;
                                        Args      : J_Value_Array)
                                        return J_Short
   is
   begin
      return Env.all.all.Call_Static_Short_Method_A
        (Env, Class, Method_ID, Args);
   end Call_Static_Short_Method_A;

   ------------------------------
   -- Call_Static_Int_Method_A --
   ------------------------------

   function Call_Static_Int_Method_A (Env       : JNI_Env_Access;
                                      Class     : J_Class;
                                      Method_ID : J_Method_ID;
                                      Args      : J_Value_Array)
                                      return J_Int
   is
   begin
      return Env.all.all.Call_Static_Int_Method_A
        (Env, Class, Method_ID, Args);
   end Call_Static_Int_Method_A;

   -------------------------------
   -- Call_Static_Long_Method_A --
   -------------------------------

   function Call_Static_Long_Method_A (Env       : JNI_Env_Access;
                                       Class     : J_Class;
                                       Method_ID : J_Method_ID;
                                       Args      : J_Value_Array)
                                       return J_Long
   is
   begin
      return Env.all.all.Call_Static_Long_Method_A
        (Env, Class, Method_ID, Args);
   end Call_Static_Long_Method_A;

   --------------------------------
   -- Call_Static_Float_Method_A --
   --------------------------------

   function Call_Static_Float_Method_A (Env       : JNI_Env_Access;
                                        Class     : J_Class;
                                        Method_ID : J_Method_ID;
                                        Args      : J_Value_Array)
                                        return J_Float
   is
   begin
      return Env.all.all.Call_Static_Float_Method_A
        (Env, Class, Method_ID, Args);
   end Call_Static_Float_Method_A;

   ---------------------------------
   -- Call_Static_Double_Method_A --
   ---------------------------------

   function Call_Static_Double_Method_A (Env       : JNI_Env_Access;
                                         Class     : J_Class;
                                         Method_ID : J_Method_ID;
                                         Args      : J_Value_Array)
                                         return J_Double
   is
   begin
      return Env.all.all.Call_Static_Double_Method_A
        (Env, Class, Method_ID, Args);
   end Call_Static_Double_Method_A;

   -------------------------------
   -- Call_Static_Void_Method_A --
   -------------------------------

   procedure Call_Static_Void_Method_A (Env       : JNI_Env_Access;
                                        Class     : J_Class;
                                        Method_ID : J_Method_ID;
                                        Args      : J_Value_Array)
   is
   begin
      Env.all.all.Call_Static_Void_Method_A
        (Env, Class, Method_ID, Args);
   end Call_Static_Void_Method_A;

   -------------------------
   -- Get_Static_Field_ID --
   -------------------------

   function Get_Static_Field_ID (Env       : JNI_Env_Access;
                                 Class     : J_Class;
                                 Name      : String;
                                 Sig       : String)
                                 return J_Field_ID
   is
   begin
      return Env.all.all.Get_Static_Field_ID
	(Env, Class, Name & ASCII.NUL, Sig & ASCII.NUL);
   end Get_Static_Field_ID;

   -----------------------------
   -- Get_Static_Object_Field --
   -----------------------------

   function Get_Static_Object_Field (Env      : JNI_Env_Access;
                                     Class    : J_Class;
                                     Field_ID : J_Field_ID)
                                     return J_Object
   is
   begin
      return Env.all.all.Get_Static_Object_Field (Env, Class, Field_ID);
   end Get_Static_Object_Field;

   ------------------------------
   -- Get_Static_Boolean_Field --
   ------------------------------

   function Get_Static_Boolean_Field (Env      : JNI_Env_Access;
                                      Class    : J_Class;
                                      Field_ID : J_Field_ID)
                                      return J_Boolean
   is
   begin
      return Env.all.all.Get_Static_Boolean_Field (Env, Class, Field_ID);
   end Get_Static_Boolean_Field;

   ---------------------------
   -- Get_Static_Byte_Field --
   ---------------------------

   function Get_Static_Byte_Field (Env      : JNI_Env_Access;
                                   Class    : J_Class;
                                   Field_ID : J_Field_ID)
                                   return J_Byte
   is
   begin
      return Env.all.all.Get_Static_Byte_Field (Env, Class, Field_ID);
   end Get_Static_Byte_Field;

   ---------------------------
   -- Get_Static_Char_Field --
   ---------------------------

   function Get_Static_Char_Field (Env      : JNI_Env_Access;
                                   Class    : J_Class;
                                   Field_ID : J_Field_ID)
                                   return J_Char
   is
   begin
      return Env.all.all.Get_Static_Char_Field (Env, Class, Field_ID);
   end Get_Static_Char_Field;

   ----------------------------
   -- Get_Static_Short_Field --
   ----------------------------

   function Get_Static_Short_Field (Env      : JNI_Env_Access;
                                    Class    : J_Class;
                                    Field_ID : J_Field_ID)
                                    return J_Short
   is
   begin
      return Env.all.all.Get_Static_Short_Field (Env, Class, Field_ID);
   end Get_Static_Short_Field;

   --------------------------
   -- Get_Static_Int_Field --
   --------------------------

   function Get_Static_Int_Field (Env      : JNI_Env_Access;
                                  Class    : J_Class;
                                  Field_ID : J_Field_ID)
                                  return J_Int
   is
   begin
      return Env.all.all.Get_Static_Int_Field (Env, Class, Field_ID);
   end Get_Static_Int_Field;

   ---------------------------
   -- Get_Static_Long_Field --
   ---------------------------

   function Get_Static_Long_Field (Env      : JNI_Env_Access;
                                   Class    : J_Class;
                                   Field_ID : J_Field_ID)
                                   return J_Long
   is
   begin
      return Env.all.all.Get_Static_Long_Field (Env, Class, Field_ID);
   end Get_Static_Long_Field;

   ----------------------------
   -- Get_Static_Float_Field --
   ----------------------------

   function Get_Static_Float_Field (Env      : JNI_Env_Access;
                                    Class    : J_Class;
                                    Field_ID : J_Field_ID)
                                     return J_Float
   is
   begin
      return Env.all.all.Get_Static_Float_Field (Env, Class, Field_ID);
   end Get_Static_Float_Field;

   -----------------------------
   -- Get_Static_Double_Field --
   -----------------------------

   function Get_Static_Double_Field (Env      : JNI_Env_Access;
                                     Class    : J_Class;
                                     Field_ID : J_Field_ID)
                                     return J_Double
   is
   begin
      return Env.all.all.Get_Static_Double_Field (Env, Class, Field_ID);
   end Get_Static_Double_Field;

   -----------------------------
   -- Set_Static_Object_Field --
   -----------------------------

   procedure Set_Static_Object_Field (Env      : JNI_Env_Access;
                                      Class    : J_Class;
                                      Field_ID : J_Field_ID;
                                      Val      : J_Object)
   is
   begin
      Env.all.all.Set_Static_Object_Field (Env, Class, Field_ID, Val);
   end Set_Static_Object_Field;

   ------------------------------
   -- Set_Static_Boolean_Field --
   ------------------------------

   procedure Set_Static_Boolean_Field (Env      : JNI_Env_Access;
                                       Class    : J_Class;
                                       Field_ID : J_Field_ID;
                                       Val      : J_Boolean)
   is
   begin
      Env.all.all.Set_Static_Boolean_Field (Env, Class, Field_ID, Val);
   end Set_Static_Boolean_Field;

   ---------------------------
   -- Set_Static_Byte_Field --
   ---------------------------

   procedure Set_Static_Byte_Field (Env      : JNI_Env_Access;
                                    Class    : J_Class;
                                    Field_ID : J_Field_ID;
                                    Val      : J_Byte)
   is
   begin
      Env.all.all.Set_Static_Byte_Field (Env, Class, Field_ID, Val);
   end Set_Static_Byte_Field;

   ---------------------------
   -- Set_Static_Char_Field --
   ---------------------------

   procedure Set_Static_Char_Field (Env      : JNI_Env_Access;
                                    Class    : J_Class;
                                    Field_ID : J_Field_ID;
                                    Val      : J_Char)
   is
   begin
      Env.all.all.Set_Static_Char_Field (Env, Class, Field_ID, Val);
   end Set_Static_Char_Field;

   ----------------------------
   -- Set_Static_Short_Field --
   ----------------------------

   procedure Set_Static_Short_Field (Env      : JNI_Env_Access;
                                     Class    : J_Class;
                                     Field_ID : J_Field_ID;
                                     Val      : J_Short)
   is
   begin
      Env.all.all.Set_Static_Short_Field (Env, Class, Field_ID, Val);
   end Set_Static_Short_Field;

   --------------------------
   -- Set_Static_Int_Field --
   --------------------------

   procedure Set_Static_Int_Field (Env      : JNI_Env_Access;
                                   Class    : J_Class;
                                   Field_ID : J_Field_ID;
                                   Val      : J_Int)
   is
   begin
      Env.all.all.Set_Static_Int_Field (Env, Class, Field_ID, Val);
   end Set_Static_Int_Field;

   ---------------------------
   -- Set_Static_Long_Field --
   ---------------------------

   procedure Set_Static_Long_Field (Env      : JNI_Env_Access;
                                    Class    : J_Class;
                                    Field_ID : J_Field_ID;
                                    Val      : J_Long)
   is
   begin
      Env.all.all.Set_Static_Long_Field (Env, Class, Field_ID, Val);
   end Set_Static_Long_Field;

   ----------------------------
   -- Set_Static_Float_Field --
   ----------------------------

   procedure Set_Static_Float_Field (Env      : JNI_Env_Access;
                                     Class    : J_Class;
                                     Field_ID : J_Field_ID;
                                     Val      : J_Float)
   is
   begin
      Env.all.all.Set_Static_Float_Field (Env, Class, Field_ID, Val);
   end Set_Static_Float_Field;

   -----------------------------
   -- Set_Static_Double_Field --
   -----------------------------

   procedure Set_Static_Double_Field (Env      : JNI_Env_Access;
                                      Class    : J_Class;
                                      Field_ID : J_Field_ID;
                                      Val      : J_Double)
   is
   begin
      Env.all.all.Set_Static_Double_Field (Env, Class, Field_ID, Val);
   end Set_Static_Double_Field;

   ----------------
   -- New_String --
   ----------------

   function New_String
     (Env     : JNI_Env_Access;
      Unicode : J_Char_Array;
      Len     : J_Size)
      return J_String is
   begin
      return Env.all.all.New_String (Env, Unicode, Len);
   end New_String;

   -----------------------
   -- Get_String_Length --
   -----------------------

   function Get_String_Length
     (Env : JNI_Env_Access;
      Str : J_String)
      return J_Size is
   begin
      return Env.all.all.Get_String_Length (Env, Str);
   end Get_String_Length;

   ----------------------
   -- Get_String_Chars --
   ----------------------

   function Get_String_Chars
     (Env     : JNI_Env_Access;
      Str     : J_String;
      Is_Copy : access J_Boolean)
      return J_Char_Array is
   begin
      return Env.all.all.Get_String_Chars (Env, Str, Is_Copy);
   end Get_String_Chars;

   --------------------------
   -- Release_String_Chars --
   --------------------------

   procedure Release_String_Chars
     (Env   : JNI_Env_Access;
      Str   : J_String;
      Chars : J_Char_Array) is
   begin
      Env.all.all.Release_String_Chars (Env, Str, Chars);
   end Release_String_Chars;

   --------------------
   -- New_String_UTF --
   --------------------

   function New_String_UTF
     (Env : JNI_Env_Access;
      UTF : chars_ptr)
      return J_String is
   begin
      return Env.all.all.New_String_UTF (Env, UTF);
   end New_String_UTF;

   ---------------------------
   -- Get_String_UTF_Length --
   ---------------------------

   function Get_String_UTF_Length
     (Env : JNI_Env_Access;
      Str : J_String)
      return J_Size is
   begin
      return Env.all.all.Get_String_UTF_Length (Env, Str);
   end Get_String_UTF_Length;

   --------------------------
   -- Get_String_UTF_Chars --
   --------------------------

   function Get_String_UTF_Chars
     (Env     : JNI_Env_Access;
      Str     : J_String;
      Is_Copy : access J_Boolean)
      return chars_ptr is
   begin
      return Env.all.all.Get_String_UTF_Chars (Env, Str, Is_Copy);
   end Get_String_UTF_Chars;

   function Get_String_UTF_Chars
     (Env     : JNI_Env_Access;
      Str     : J_String) return chars_ptr
   is
      Dummy_Bool : aliased J_Boolean;
   begin
      return Env.all.all.Get_String_UTF_Chars (Env, Str, Dummy_Bool'Access);
   end Get_String_UTF_Chars;

   ------------------------------
   -- Release_String_UTF_Chars --
   ------------------------------

   procedure Release_String_UTF_Chars
     (Env   : JNI_Env_Access;
      Str   : J_String;
      Chars : chars_ptr) is
   begin
      Env.all.all.Release_String_UTF_Chars (Env, Str, Chars);
   end Release_String_UTF_Chars;

   ----------------------
   -- Get_Array_Length --
   ----------------------

   function Get_Array_Length (Env : JNI_Env_Access;
                              Arr : J_Array)
                              return J_Size
   is
   begin
      return Env.all.all.Get_Array_Length (Env, Arr);
   end Get_Array_Length;

   ----------------------
   -- New_Object_Array --
   ----------------------

   function New_Object_Array (Env   : JNI_Env_Access;
                              Len   : J_Size;
                              Class : J_Class;
                              Init  : J_Object)
                              return J_Object_J_Array
   is
   begin
      return Env.all.all.New_Object_Array (Env, Len, Class, Init);
   end New_Object_Array;

   ------------------------------
   -- Get_Object_Array_Element --
   ------------------------------

   function Get_Object_Array_Element (Env   : JNI_Env_Access;
                                      Arr   : J_Array;
                                      Index : J_Size)
                                      return J_Object
   is
   begin
      return Env.all.all.Get_Object_Array_Element (Env, Arr, Index);
   end Get_Object_Array_Element;

   ------------------------------
   -- Get_Object_Array_Element --
   ------------------------------

   procedure Set_Object_Array_Element (Env   : JNI_Env_Access;
                                       Arr   : J_Array;
                                       Index : J_Size;
                                       Val   : J_Object)
   is
   begin
      Env.all.all.Set_Object_Array_Element (Env, Arr, Index, Val);
   end Set_Object_Array_Element;

   -----------------------
   -- New_Boolean_Array --
   -----------------------

   function New_Boolean_Array (Env : JNI_Env_Access;
                               Len : J_Size)
                               return J_Boolean_J_Array
   is
   begin
      return Env.all.all.New_Boolean_Array (Env, Len);
   end New_Boolean_Array;

   --------------------
   -- New_Char_Array --
   --------------------

   function New_Char_Array (Env : JNI_Env_Access;
                            Len : J_Size)
                            return J_Char_J_Array
   is
   begin
      return Env.all.all.New_Char_Array (Env, Len);
   end New_Char_Array;

   --------------------
   -- New_Byte_Array --
   --------------------

   function New_Byte_Array (Env : JNI_Env_Access;
                            Len : J_Size)
                            return J_Byte_J_Array
   is
   begin
      return Env.all.all.New_Byte_Array (Env, Len);
   end New_Byte_Array;

   ---------------------
   -- New_Short_Array --
   ---------------------

   function New_Short_Array (Env : JNI_Env_Access;
                             Len : J_Size)
                             return J_Short_J_Array
   is
   begin
      return Env.all.all.New_Short_Array (Env, Len);
   end New_Short_Array;

   -------------------
   -- New_Int_Array --
   -------------------

   function New_Int_Array (Env : JNI_Env_Access;
                           Len : J_Size)
                           return J_Int_J_Array
   is
   begin
      return Env.all.all.New_Int_Array (Env, Len);
   end New_Int_Array;

   --------------------
   -- New_Long_Array --
   --------------------

   function New_Long_Array (Env : JNI_Env_Access;
                            Len : J_Size)
                            return J_Long_J_Array
   is
   begin
      return Env.all.all.New_Long_Array (Env, Len);
   end New_Long_Array;

   ---------------------
   -- New_Float_Array --
   ---------------------

   function New_Float_Array (Env : JNI_Env_Access;
                             Len : J_Size)
                             return J_Float_J_Array
   is
   begin
      return Env.all.all.New_Float_Array (Env, Len);
   end New_Float_Array;

   ----------------------
   -- New_Double_Array --
   ----------------------

   function New_Double_Array (Env : JNI_Env_Access;
                              Len : J_Size)
                              return J_Double_J_Array
   is
   begin
      return Env.all.all.New_Double_Array (Env, Len);
   end New_Double_Array;

   ---------------------------------------
   -- Get_Boolean_Array_Elements_Access --
   ---------------------------------------

   function Get_Boolean_Array_Elements (Env     : JNI_Env_Access;
                                        Arr     : J_Boolean_J_Array;
                                        Is_Copy : access J_Boolean)
                                        return J_Boolean_Star
   is
   begin
      return Env.all.all.Get_Boolean_Array_Elements (Env, Arr, Is_Copy);
   end Get_Boolean_Array_Elements;

   ------------------------------------
   -- Get_Char_Array_Elements_Access --
   ------------------------------------

   function Get_Char_Array_Elements (Env     : JNI_Env_Access;
                                     Arr     : J_Char_J_Array;
                                     Is_Copy : access J_Boolean)
                                     return J_Char_Star
   is
   begin
      return Env.all.all.Get_Char_Array_Elements (Env, Arr, Is_Copy);
   end Get_Char_Array_Elements;

   ------------------------------------
   -- Get_Byte_Array_Elements_Access --
   ------------------------------------

   function Get_Byte_Array_Elements (Env     : JNI_Env_Access;
                                     Arr     : J_Byte_J_Array;
                                     Is_Copy : access J_Boolean)
                                     return J_Byte_Star
   is
   begin
      return Env.all.all.Get_Byte_Array_Elements (Env, Arr, Is_Copy);
   end Get_Byte_Array_Elements;

   -------------------------------------
   -- Get_Short_Array_Elements_Access --
   -------------------------------------

   function Get_Short_Array_Elements (Env     : JNI_Env_Access;
                                      Arr     : J_Short_J_Array;
                                      Is_Copy : access J_Boolean)
                                      return J_Short_Star
   is
   begin
      return Env.all.all.Get_Short_Array_Elements (Env, Arr, Is_Copy);
   end Get_Short_Array_Elements;

   -----------------------------------
   -- Get_Int_Array_Elements_Access --
   -----------------------------------

   function Get_Int_Array_Elements (Env     : JNI_Env_Access;
                                    Arr     : J_Int_J_Array;
                                    Is_Copy : access J_Boolean)
                                    return J_Int_Star
   is
   begin
      return Env.all.all.Get_Int_Array_Elements (Env, Arr, Is_Copy);
   end Get_Int_Array_Elements;

   ------------------------------------
   -- Get_Long_Array_Elements_Access --
   ------------------------------------

   function Get_Long_Array_Elements (Env     : JNI_Env_Access;
                                     Arr     : J_Long_J_Array;
                                     Is_Copy : access J_Boolean)
                                     return J_Long_Star
   is
   begin
      return Env.all.all.Get_Long_Array_Elements (Env, Arr, Is_Copy);
   end Get_Long_Array_Elements;

   -------------------------------------
   -- Get_Float_Array_Elements_Access --
   -------------------------------------

   function Get_Float_Array_Elements (Env     : JNI_Env_Access;
                                      Arr     : J_Float_J_Array;
                                      Is_Copy : access J_Boolean)
                                      return J_Float_Star
   is
   begin
      return Env.all.all.Get_Float_Array_Elements (Env, Arr, Is_Copy);
   end Get_Float_Array_Elements;

   --------------------------------------
   -- Get_Double_Array_Elements_Access --
   --------------------------------------

   function Get_Double_Array_Elements (Env     : JNI_Env_Access;
                                       Arr     : J_Double_J_Array;
                                       Is_Copy : access J_Boolean)
                                       return J_Double_Star
   is
   begin
      return Env.all.all.Get_Double_Array_Elements (Env, Arr, Is_Copy);
   end Get_Double_Array_Elements;

   ------------------------------------
   -- Release_Boolean_Array_Elements --
   ------------------------------------

   procedure Release_Boolean_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Boolean_J_Array;
      Elems   : J_Boolean_Star;
      Mode    : J_Int) is
   begin
      Env.all.all.Release_Boolean_Array_Elements (Env, Arr, Elems, Mode);
   end Release_Boolean_Array_Elements;

   ---------------------------------
   -- Release_Byte_Array_Elements --
   ---------------------------------

   procedure Release_Byte_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Byte_J_Array;
      Elems   : J_Byte_Star;
      Mode    : J_Int) is
   begin
      Env.all.all.Release_Byte_Array_Elements (Env, Arr, Elems, Mode);
   end Release_Byte_Array_Elements;

   ---------------------------------
   -- Release_Char_Array_Elements --
   ---------------------------------

   procedure Release_Char_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Char_J_Array;
      Elems   : J_Char_Star;
      Mode    : J_Int) is
   begin
      Env.all.all.Release_Char_Array_Elements (Env, Arr, Elems, Mode);
   end Release_Char_Array_Elements;

   ----------------------------------
   -- Release_Short_Array_Elements --
   ----------------------------------

   procedure Release_Short_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Short_J_Array;
      Elems   : J_Short_Star;
      Mode    : J_Int) is
   begin
      Env.all.all.Release_Short_Array_Elements (Env, Arr, Elems, Mode);
   end Release_Short_Array_Elements;

   --------------------------------
   -- Release_Int_Array_Elements --
   --------------------------------

   procedure Release_Int_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Int_J_Array;
      Elems   : J_Int_Star;
      Mode    : J_Int) is
   begin
      Env.all.all.Release_Int_Array_Elements (Env, Arr, Elems, Mode);
   end Release_Int_Array_Elements;

   ---------------------------------
   -- Release_Long_Array_Elements --
   ---------------------------------

   procedure Release_Long_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Long_J_Array;
      Elems   : J_Long_Star;
      Mode    : J_Int) is
   begin
      Env.all.all.Release_Long_Array_Elements (Env, Arr, Elems, Mode);
   end Release_Long_Array_Elements;

   ----------------------------------
   -- Release_Float_Array_Elements --
   ----------------------------------

   procedure Release_Float_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Float_J_Array;
      Elems   : J_Float_Star;
      Mode    : J_Int) is
   begin
      Env.all.all.Release_Float_Array_Elements (Env, Arr, Elems, Mode);
   end Release_Float_Array_Elements;

   -----------------------------------
   -- Release_Double_Array_Elements --
   -----------------------------------

   procedure Release_Double_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Double_J_Array;
      Elems   : J_Double_Star;
      Mode    : J_Int) is
   begin
      Env.all.all.Release_Double_Array_Elements (Env, Arr, Elems, Mode);
   end Release_Double_Array_Elements;

   -------------------------------------
   -- Get_Boolean_Array_Region_Access --
   -------------------------------------

   procedure Get_Boolean_Array_Region (Env   : JNI_Env_Access;
                                       Arr   : J_Boolean_J_Array;
                                       Start : J_Size;
                                       Len   : J_Size;
                                       Buf   : out J_Boolean_Array)
   is
   begin
      Env.all.all.Get_Boolean_Array_Region (Env, Arr, Start, Len, Buf);
   end Get_Boolean_Array_Region;

   ----------------------------------
   -- Get_Char_Array_Region_Access --
   ----------------------------------

   procedure Get_Char_Array_Region (Env   : JNI_Env_Access;
                                    Arr   : J_Char_J_Array;
                                    Start : J_Size;
                                    Len   : J_Size;
                                    Buf   : out J_Char_Array)
   is
   begin
      Env.all.all.Get_Char_Array_Region (Env, Arr, Start, Len, Buf);
   end Get_Char_Array_Region;

   ----------------------------------
   -- Get_Byte_Array_Region_Access --
   ----------------------------------

   procedure Get_Byte_Array_Region (Env   : JNI_Env_Access;
                                    Arr   : J_Byte_J_Array;
                                    Start : J_Size;
                                    Len   : J_Size;
                                    Buf   : out J_Byte_Array)
   is
   begin
      Env.all.all.Get_Byte_Array_Region (Env, Arr, Start, Len, Buf);
   end Get_Byte_Array_Region;

   -----------------------------------
   -- Get_Short_Array_Region_Access --
   -----------------------------------

   procedure Get_Short_Array_Region (Env   : JNI_Env_Access;
                                     Arr   : J_Short_J_Array;
                                     Start : J_Size;
                                     Len   : J_Size;
                                     Buf   : out J_Short_Array)
   is
   begin
      Env.all.all.Get_Short_Array_Region (Env, Arr, Start, Len, Buf);
   end Get_Short_Array_Region;

   ---------------------------------
   -- Get_Int_Array_Region_Access --
   ---------------------------------

   procedure Get_Int_Array_Region (Env   : JNI_Env_Access;
                                   Arr   : J_Int_J_Array;
                                   Start : J_Size;
                                   Len   : J_Size;
                                   Buf   : out J_Int_Array)
   is
   begin
      Env.all.all.Get_Int_Array_Region (Env, Arr, Start, Len, Buf);
   end Get_Int_Array_Region;

   ----------------------------------
   -- Get_Long_Array_Region_Access --
   ----------------------------------

   procedure Get_Long_Array_Region (Env   : JNI_Env_Access;
                                    Arr   : J_Long_J_Array;
                                    Start : J_Size;
                                    Len   : J_Size;
                                    Buf   : out J_Long_Array)
   is
   begin
      Env.all.all.Get_Long_Array_Region (Env, Arr, Start, Len, Buf);
   end Get_Long_Array_Region;

   -----------------------------------
   -- Get_Float_Array_Region_Access --
   -----------------------------------

   procedure Get_Float_Array_Region (Env   : JNI_Env_Access;
                                     Arr   : J_Float_J_Array;
                                     Start : J_Size;
                                     Len   : J_Size;
                                     Buf   : out J_Float_Array)
   is
   begin
      Env.all.all.Get_Float_Array_Region (Env, Arr, Start, Len, Buf);
   end Get_Float_Array_Region;

   ------------------------------------
   -- Get_Double_Array_Region_Access --
   ------------------------------------

   procedure Get_Double_Array_Region (Env   : JNI_Env_Access;
                                      Arr   : J_Double_J_Array;
                                      Start : J_Size;
                                      Len   : J_Size;
                                      Buf   : out J_Double_Array)
   is
   begin
      Env.all.all.Get_Double_Array_Region (Env, Arr, Start, Len, Buf);
   end Get_Double_Array_Region;

   ------------------------------
   -- Set_Boolean_Array_Region --
   ------------------------------

   procedure Set_Boolean_Array_Region (Env   : JNI_Env_Access;
                                       Arr   : J_Boolean_J_Array;
                                       Start : J_Size;
                                       Len   : J_Size;
                                       Buf   : J_Boolean_Array)
   is
   begin
      Env.all.all.Set_Boolean_Array_Region (Env, Arr, Start, Len, Buf);
   end Set_Boolean_Array_Region;

   ---------------------------
   -- Set_Char_Array_Region --
   ---------------------------

   procedure Set_Char_Array_Region (Env   : JNI_Env_Access;
                                    Arr   : J_Char_J_Array;
                                    Start : J_Size;
                                    Len   : J_Size;
                                    Buf   : J_Char_Array)
   is
   begin
      Env.all.all.Set_Char_Array_Region (Env, Arr, Start, Len, Buf);
   end Set_Char_Array_Region;

   ---------------------------
   -- Set_Byte_Array_Region --
   ---------------------------

   procedure Set_Byte_Array_Region (Env   : JNI_Env_Access;
                                    Arr   : J_Byte_J_Array;
                                    Start : J_Size;
                                    Len   : J_Size;
                                    Buf   : J_Byte_Array)
   is
   begin
      Env.all.all.Set_Byte_Array_Region (Env, Arr, Start, Len, Buf);
   end Set_Byte_Array_Region;

   ----------------------------
   -- Set_Short_Array_Region --
   ----------------------------

   procedure Set_Short_Array_Region (Env   : JNI_Env_Access;
                                     Arr   : J_Short_J_Array;
                                     Start : J_Size;
                                     Len   : J_Size;
                                     Buf   : J_Short_Array)
   is
   begin
      Env.all.all.Set_Short_Array_Region (Env, Arr, Start, Len, Buf);
   end Set_Short_Array_Region;

   --------------------------
   -- Set_Int_Array_Region --
   --------------------------

   procedure Set_Int_Array_Region (Env   : JNI_Env_Access;
                                   Arr   : J_Int_J_Array;
                                   Start : J_Size;
                                   Len   : J_Size;
                                   Buf   : J_Int_Array)
   is
   begin
      Env.all.all.Set_Int_Array_Region (Env, Arr, Start, Len, Buf);
   end Set_Int_Array_Region;

   ---------------------------
   -- Set_Long_Array_Region --
   ---------------------------

   procedure Set_Long_Array_Region (Env   : JNI_Env_Access;
                                    Arr   : J_Long_J_Array;
                                    Start : J_Size;
                                    Len   : J_Size;
                                    Buf   : J_Long_Array)
   is
   begin
      Env.all.all.Set_Long_Array_Region (Env, Arr, Start, Len, Buf);
   end Set_Long_Array_Region;

   ----------------------------
   -- Set_Float_Array_Region --
   ----------------------------

   procedure Set_Float_Array_Region (Env   : JNI_Env_Access;
                                     Arr   : J_Float_J_Array;
                                     Start : J_Size;
                                     Len   : J_Size;
                                     Buf   : J_Float_Array)
   is
   begin
      Env.all.all.Set_Float_Array_Region (Env, Arr, Start, Len, Buf);
   end Set_Float_Array_Region;

   -----------------------------
   -- Set_Double_Array_Region --
   -----------------------------

   procedure Set_Double_Array_Region (Env   : JNI_Env_Access;
                                      Arr   : J_Double_J_Array;
                                      Start : J_Size;
                                      Len   : J_Size;
                                      Buf   : J_Double_Array)
   is
   begin
      Env.all.all.Set_Double_Array_Region (Env, Arr, Start, Len, Buf);
   end Set_Double_Array_Region;

   ----------------------
   -- Register_Natives --
   ----------------------

   function Register_Natives
     (Env       : JNI_Env_Access;
      Class     : J_Class;
      Methods   : JNI_Native_Method_Arr;
      N_Methods : J_Int)
      return J_Int is
   begin
      return Env.all.all.Register_Natives (Env, Class, Methods, N_Methods);
   end Register_Natives;

   ------------------------
   -- Unregister_Natives --
   ------------------------

   function Unregister_Natives
     (Env   : JNI_Env_Access;
      Class : J_Class)
      return J_Int is
   begin
      return Env.all.all.Unregister_Natives (Env, Class);
   end Unregister_Natives;

   -------------------
   -- Monitor_Enter --
   -------------------

   function Monitor_Enter
     (Env : JNI_Env_Access;
      Obj : J_Object)
      return J_Int is
   begin
      return Env.all.all.Monitor_Enter (Env, Obj);
   end Monitor_Enter;

   ------------------
   -- Monitor_Exit --
   ------------------

   function Monitor_Exit
     (Env : JNI_Env_Access;
      Obj : J_Object)
      return J_Int is
   begin
      return Env.all.all.Monitor_Exit (Env, Obj);
   end Monitor_Exit;

   -----------------
   -- Get_Java_VM --
   -----------------

   function Get_Java_VM
     (Env : JNI_Env_Access;
      VM  : access Java_VM_Access)
      return J_Int is
   begin
      return Env.all.all.Get_Java_VM (Env, VM);
   end Get_Java_VM;

   -----------------------
   -- Get_String_Region --
   -----------------------

   procedure Get_String_Region
     (Env   : JNI_Env_Access;
      Str   : J_String;
      Start : J_Size;
      Len   : J_Size;
      Buf   : out J_Char_Array) is
   begin
      Env.all.all.Get_String_Region (Env, Str, Start, Len, Buf);
   end Get_String_Region;

   ---------------------------
   -- Get_String_UTF_Region --
   ---------------------------

   procedure Get_String_UTF_Region
     (Env   : JNI_Env_Access;
      Str   : J_String;
      Start : J_Size;
      Len   : J_Size;
      Buf   : out C.char_array) is
   begin
      Env.all.all.Get_String_UTF_Region (Env, Str, Start, Len, Buf);
   end Get_String_UTF_Region;

   ----------------------------------
   -- Get_Primitive_Array_Critical --
   ----------------------------------

   procedure Get_Primitive_Array_Critical
     (Env : JNI_Env_Access;
      Arr : J_Array;
      Is_Copy : access Boolean) is
   begin
      Env.all.all.Get_Primitive_Array_Critical (Env, Arr, Is_Copy);
   end Get_Primitive_Array_Critical;

   --------------------------------------
   -- Release_Primitive_Array_Critical --
   --------------------------------------

   procedure Release_Primitive_Array_Critical
     (Env     : JNI_Env_Access;
      Arr     : J_Array;
      C_Array : System.Address;
      Mode    : J_Int) is
   begin
      Env.all.all.Release_Primitive_Array_Critical (Env, Arr, C_Array, Mode);
   end Release_Primitive_Array_Critical;

   -------------------------
   -- Get_String_Critical --
   -------------------------

   procedure Get_String_Critical
     (Env     : JNI_Env_Access;
      Str     : J_String;
      Is_Copy : access Boolean) is
   begin
      Env.all.all.Get_String_Critical (Env, Str, Is_Copy);
   end Get_String_Critical;

   -----------------------------
   -- Release_String_Critical --
   -----------------------------

   procedure Release_String_Critical
     (Env      : JNI_Env_Access;
      Str      : J_String;
      C_String : J_Char_Array) is
   begin
      Env.all.all.Release_String_Critical (Env, Str, C_String);
   end Release_String_Critical;

   -------------------------
   -- New_Weak_Global_Ref --
   -------------------------

   function New_Weak_Global_Ref
     (Env : JNI_Env_Access;
      Obj : J_Object)
      return J_Weak is
   begin
      return Env.all.all.New_Weak_Global_Ref (Env, Obj);
   end New_Weak_Global_Ref;

   ----------------------------
   -- Delete_Weak_Global_Ref --
   ----------------------------

   procedure Delete_Weak_Global_Ref
     (Env : JNI_Env_Access;
      Ref : J_Weak) is
   begin
      Env.all.all.Delete_Weak_Global_Ref (Env, Ref);
   end Delete_Weak_Global_Ref;

   ---------------------
   -- Exception_Check --
   ---------------------

   function Exception_Check (Env : JNI_Env_Access) return J_Boolean
   is
   begin
      return Env.all.all.Exception_Check (Env);
   end Exception_Check;

   ----------------------------
   -- New_Direct_Byte_Buffer --
   ----------------------------

   function New_Direct_Byte_Buffer
     (Env      : JNI_Env_Access;
      Addr     : System.Address;
      Capacity : J_Long)
      return J_Object is
   begin
      return Env.all.all.New_Direct_Byte_Buffer (Env, Addr, Capacity);
   end New_Direct_Byte_Buffer;

   -------------------------------
   -- Get_Direct_Buffer_Address --
   -------------------------------

   function Get_Direct_Buffer_Address
     (Env : JNI_Env_Access;
      Buf : J_Object) return System.Address is
   begin
      return Env.all.all.Get_Direct_Buffer_Address (Env, Buf);
   end Get_Direct_Buffer_Address;

   --------------------------------
   -- Get_Direct_Buffer_Capacity --
   --------------------------------

   function Get_Direct_Buffer_Capacity
     (Env : JNI_Env_Access;
      Buf : J_Object)
      return J_Long is
   begin
      return Env.all.all.Get_Direct_Buffer_Capacity (Env, Buf);
   end Get_Direct_Buffer_Capacity;

end Artics.Java.JNI;
