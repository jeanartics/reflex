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

--  This package implements a binding to the Java JNI interface.

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;

package Artics.Java.JNI is

   use Interfaces;

   pragma Convention_Identifier (JNICall, Stdcall);
   --pragma Convention_Identifier (JNICall, C);

   ---------------------
   -- Primitive types --
   ---------------------

   type J_Boolean is new Boolean;
   for J_Boolean'Size use 8;
   pragma Convention (C, J_Boolean);
   type J_Boolean_Array is array (Positive range <>) of aliased J_Boolean;

   type J_Byte is range -128 .. 127;
   for J_Byte'Size use 8;
   type J_Byte_Array is array (Positive range <>) of aliased J_Byte;

   type J_Char is new Wide_Character range
     Wide_Character'Val (16#00000000#) .. Wide_Character'Val (16#0000FFFF#);
   for J_Char'Size use 16;
   type J_Char_Array is array (Positive range <>) of aliased J_Char;

   type J_Short is range -32768 .. 32767;
   for J_Short'Size use 16;
   type J_Short_Array is array (Positive range <>) of aliased J_Short;

   type J_Int is range -2147483648 .. 2147483647;
   for J_Int'Size use 32;
   type J_Int_Array is array (Positive range <>) of aliased J_Int;

   type J_Long is range -9223372036854775808 .. 9223372036854775807;
   for J_Long'Size use 64;
   type J_Long_Array is array (Positive range <>) of aliased J_Long;

   type J_Float is new Standard.Float;
   --  TODO : this is not portable
   type J_Float_Array is array (Positive range <>) of aliased J_Float;

   type J_Double is new Standard.Long_Float;
   --  TODO : this is not portable
   type J_Double_Array is array (Positive range <>) of aliased J_Double;

   subtype J_Size is J_Int;

   ---------------------
   -- Reference types --
   ---------------------

   type J_Object is new System.Address;
   J_Null_Object : constant J_Object := J_Object (System.Null_Address);

   subtype J_Class is J_Object;
   J_Null_Class : constant J_Class := J_Class (J_Null_Object);

   subtype J_String is J_Object;
   J_Null_String : constant J_String := J_String (J_Null_Object);

   subtype J_Array  is J_Object;
   J_Null_Array : constant J_Array := J_Array (J_Null_Object);

   subtype J_Throwable is J_Object;
   J_Null_Throwable : constant J_Throwable
     := J_Throwable (System.Null_Address);

   subtype J_Weak is J_Object;
   J_Null_Weak : constant J_Weak
     := J_Weak (System.Null_Address);

   subtype J_Boolean_J_Array is J_Array;
   J_Null_Boolean_J_Array : constant J_Boolean_J_Array :=
                              J_Boolean_J_Array (System.Null_Address);
   subtype J_Byte_J_Array    is J_Array;
   J_Null_Byte_J_Array : constant J_Byte_J_Array :=
                           J_Byte_J_Array (System.Null_Address);
   subtype J_Char_J_Array    is J_Array;
   J_Null_Char_J_Array : constant J_Char_J_Array :=
                           J_Char_J_Array (System.Null_Address);
   subtype J_Short_J_Array   is J_Array;
   J_Null_Short_J_Array : constant J_Short_J_Array :=
                            J_Short_J_Array (System.Null_Address);
   subtype J_Int_J_Array     is J_Array;
   J_Null_Int_J_Array : constant J_Int_J_Array :=
                          J_Int_J_Array (System.Null_Address);
   subtype J_Long_J_Array    is J_Array;
   J_Null_Long_J_Array : constant J_Long_J_Array :=
                           J_Long_J_Array (System.Null_Address);
   subtype J_Float_J_Array   is J_Array;
   J_Null_Float_J_Array : constant J_Float_J_Array :=
                            J_Float_J_Array (System.Null_Address);
   subtype J_Double_J_Array  is J_Array;
   J_Null_Double_J_Array : constant J_Double_J_Array :=
                             J_Double_J_Array (System.Null_Address);
   subtype J_Object_J_Array  is J_Array;
   J_Null_Object_Array : constant J_Object_J_Array :=
                           J_Object_J_Array (System.Null_Address);

   ---------------------------------
   -- Pointers to Primitive Types --
   ---------------------------------

   package Byte_Pointers is
     new Interfaces.C.Pointers (Index              => Positive,
                                Element            => J_Byte,
                                Element_Array      => J_Byte_Array,
                                Default_Terminator => 0);

   subtype J_Byte_Star is Byte_Pointers.Pointer;

   package Char_Pointers is
     new Interfaces.C.Pointers (Index              => Positive,
                                Element            => J_Char,
                                Element_Array      => J_Char_Array,
                                Default_Terminator =>
                                J_Char (Wide_Character'Val (0)));

   subtype J_Char_Star is Char_Pointers.Pointer;

   package Boolean_Pointers is
     new Interfaces.C.Pointers (Index              => Positive,
                                Element            => J_Boolean,
                                Element_Array      => J_Boolean_Array,
                                Default_Terminator => False);

   subtype J_Boolean_Star is Boolean_Pointers.Pointer;

   package Short_Pointers is
     new Interfaces.C.Pointers (Index              => Positive,
                                Element            => J_Short,
                                Element_Array      => J_Short_Array,
                                Default_Terminator => 0);

   subtype J_Short_Star is Short_Pointers.Pointer;

   package Int_Pointers is
     new Interfaces.C.Pointers (Index              => Positive,
                                Element            => J_Int,
                                Element_Array      => J_Int_Array,
                                Default_Terminator => 0);

   subtype J_Int_Star is Int_Pointers.Pointer;

   package Long_Pointers is
     new Interfaces.C.Pointers (Index              => Positive,
                                Element            => J_Long,
                                Element_Array      => J_Long_Array,
                                Default_Terminator => 0);

   subtype J_Long_Star is Long_Pointers.Pointer;

   package Float_Pointers is
     new Interfaces.C.Pointers (Index              => Positive,
                                Element            => J_Float,
                                Element_Array      => J_Float_Array,
                                Default_Terminator => 0.0);

   subtype J_Float_Star is Float_Pointers.Pointer;

   package Double_Pointers is
     new Interfaces.C.Pointers (Index              => Positive,
                                Element            => J_Double,
                                Element_Array      => J_Double_Array,
                                Default_Terminator => 0.0);

   subtype J_Double_Star is Double_Pointers.Pointer;

   ----------------------------
   -- Possible return values --
   ----------------------------

   JNI_OK        : J_Int :=  0;
   JNI_Err       : J_Int := -1;
   JNI_Edetached : J_Int := -2;
   JNI_Eversion  : J_Int := -3;
   JNI_Enomem    : J_Int := -4;
   JNI_Eexist    : J_Int := -5;
   JNI_Einval    : J_Int := -6;

   ----------------
   -- Union type --
   ----------------

   type JNI_Type is (Jboolean, Jbyte, Jchar, Jshort, Jint,
                     Jlong, Jfloat, Jdouble, Jobject);

   type J_Value (T : JNI_Type := Jobject) is
      record
         case T is
            when Jboolean =>
               Z : J_Boolean;
            when Jbyte =>
               B : J_Byte;
            when Jchar =>
               C : J_Char;
            when Jshort =>
               S : J_Short;
            when Jint =>
               I : J_Int;
            when Jlong =>
               J : J_Long;
            when Jfloat =>
               F : J_Float;
            when Jdouble =>
               D : J_Double;
            when Jobject  =>
               L : J_Object;
         end case;
      end record;
   pragma Unchecked_Union (J_Value);
   pragma Convention (C, J_Value);

   type J_Value_Array is array (Positive range <>) of J_Value;
   pragma Convention (C, J_Value_Array);

   --------------------------
   -- Field and method IDs --
   --------------------------

   type J_Field_ID is new System.Address;
   type J_Method_ID is new System.Address;
   type J_Method_ID_Arr is array (Positive range <>) of aliased J_Method_ID;
   J_Null_Field_ID  : J_Field_ID  := J_Field_ID  (System.Null_Address);
   J_Null_Method_ID : J_Method_ID := J_Method_ID (System.Null_Address);

   --------------------
   -- Version number --
   --------------------

   JNI_Version_1_1 : J_Int := 16#0001_0001#;
   JNI_Version_1_2 : J_Int := 16#0001_0002#;
   JNI_Version_1_4 : J_Int := 16#0001_0004#;

   ---------
   -- API --
   ---------

   type JNI_Env is private;
   type JNI_Env_Access is access all JNI_Env;

   type Java_VM is private;
   type Java_VM_Access is access all Java_VM;
   type Java_VM_Arr is array (Positive range <>) of aliased Java_VM;

   --------------------
   -- Java_VM_Access --
   --------------------

   type Java_VM_Option is record
      Option_String : Interfaces.C.Strings.chars_ptr;
      Extra_Info    : System.Address;
   end record;
   pragma Convention (C, Java_VM_Option);

   type Java_VM_Option_Array is array (Natural range <>) of Java_VM_Option;
   pragma Convention (C, Java_VM_Option_Array);

   type Java_VM_Option_Array_Access is access all Java_VM_Option_Array;
   for Java_VM_Option_Array_Access'Size use System.Word_Size;
   pragma Convention (C, Java_VM_Option_Array_Access);

   type Java_VM_Init_Args is record
      Version             : J_Int := JNI_Version_1_4;
      N_Options           : J_Int := 0;
      Options             : Java_VM_Option_Array_Access := null;
      Ignore_Unrecognized : J_Boolean := True;
   end record;
   pragma Convention (C, Java_VM_Init_Args);

   function Destroy_Java_VM (VM : Java_VM_Access) return J_Int;

   function Attach_Current_Thread
     (VM    : Java_VM_Access;
      P_Env : access JNI_Env_Access;
      Args  : System.Address) return J_Int;

   function Detach_Current_Thread (VM : Java_VM_Access) return J_Int;

   function Get_Env
     (VM      : Java_VM_Access;
      Penv    : access JNI_Env_Access;
      Version : J_Int := JNI_Version_1_2) return J_Int;
   --  Call the JNI subprogram GetEnv to set the Penv variable.
   --  If VM is null, raise Program_Error.

   function Attach_Current_Thread_As_Daemon
     (VM : Java_VM_Access;
      P_Env : JNI_Env_Access;
      Args  : System.Address) return J_Int;

   function JNI_Create_Java_VM
     (Pvm     : access Java_VM_Access;
      Penv    : access JNI_Env_Access;
      Vm_Args : Java_VM_Init_Args) return J_Int;
   --pragma Import (C, JNI_Create_Java_VM, "JNI_CreateJavaVM");
   pragma Import (JNICall, JNI_Create_Java_VM, "JNI_CreateJavaVM");

   --------------------
   -- JNI_Env_Access --
   --------------------

   function Get_Version (Env : JNI_Env_Access) return J_Int;

   function Define_Class
     (Env    : JNI_Env_Access;
      Name   : String;
      Loader : J_Object;
      Buf    : J_Byte_Array;
      Len    : J_Size) return J_Class;

   function Find_Class
     (Env  : JNI_Env_Access;
      Name : String) return J_Class;

   function From_Reflected_Method
     (Env    : JNI_Env_Access;
      Method : J_Object) return J_Method_ID;

   function From_Reflected_Field
     (Env   : JNI_Env_Access;
      Field : J_Object) return J_Field_ID;

   function To_Reflected_Method
     (Env       : JNI_Env_Access;
      Cls       : J_Class;
      Method_ID : J_Method_ID;
      Is_Static : J_Boolean) return J_Object;

   function Get_Superclass
     (Env : JNI_Env_Access; Sub : J_Class) return J_Class;

   function Is_Assignable_From
     (Env : JNI_Env_Access;
      Sub : J_Class;
      Sup : J_Class) return J_Boolean;

   function To_Reflected_Field
     (Env       : JNI_Env_Access;
      Cls       : J_Class;
      Field_ID  : J_Field_ID;
      Is_Static : J_Boolean) return J_Object;

   function Throw (Env : JNI_Env_Access; Obj : J_Throwable) return J_Int;

   function Throw_New
     (Env   : JNI_Env_Access;
      Class : J_Class;
      Msg   : String) return J_Int;

   function Exception_Occurred (Env : JNI_Env_Access) return J_Throwable;

   procedure Exception_Describe (Env : JNI_Env_Access);

   procedure Exception_Clear (Env : JNI_Env_Access);

   procedure Fatal_Error (Env : JNI_Env_Access; Msg : String);

   function Push_Local_Frame
     (Env : JNI_Env_Access; Capacity : J_Int) return J_Int;

   function Pop_Local_Frame
     (Env : JNI_Env_Access; Result : J_Object) return J_Object;

   function New_Global_Ref
     (Env  : JNI_Env_Access;
      Lobj : J_Object) return J_Object;

   procedure Delete_Global_Ref
     (Env  : JNI_Env_Access;
      Gref : J_Object);

   procedure Delete_Local_Ref (Env : JNI_Env_Access; Obj : J_Object);

   function Is_Same_Object
     (Env   : JNI_Env_Access;
      Obj_1 : J_Object;
      Obj_2 : J_Object) return J_Boolean;

   function New_Local_Ref
     (Env : JNI_Env_Access; Ref : J_Object) return J_Object;

   function Ensure_Local_Capacity
      (Env      : JNI_Env_Access;
       Capacity : J_Int) return J_Int;

   function Alloc_Object
     (Env   : JNI_Env_Access;
      Class : J_Class) return J_Object;

   function New_Object_A
     (Env       : JNI_Env_Access;
      Class     : J_Class;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Object;

   function Get_Object_Class
     (Env : JNI_Env_Access; Obj : J_Object) return J_Class;

   function Is_Instance_Of
     (Env   : JNI_Env_Access;
      Obj   : J_Object;
      Class : J_Class) return J_Boolean;

   function Get_Method_ID
     (Env   : JNI_Env_Access;
      Class : J_Class;
      Name  : String;
      Sig   : String) return J_Method_ID;

   function Call_Object_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Object;

   function Call_Boolean_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Boolean;

   function Call_Byte_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Byte;

   function Call_Char_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Char;

   function Call_Short_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Short;

   function Call_Int_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Int;

   function Call_Long_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Long;

   function Call_Float_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Float;

   function Call_Double_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Double;

   procedure Call_Void_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array);

   function Call_Non_Virtual_Object_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Object;

   function Call_Non_Virtual_Boolean_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Boolean;

   function Call_Non_Virtual_Byte_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Byte;

   function Call_Non_Virtual_Char_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Char;

   function Call_Non_Virtual_Short_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Short;

   function Call_Non_Virtual_Int_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Int;

   function Call_Non_Virtual_Long_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Long;

   function Call_Non_Virtual_Float_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Float;

   function Call_Non_Virtual_Double_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Double;

   procedure Call_Non_Virtual_Void_Method_A
     (Env       : JNI_Env_Access;
      Obj       : J_Object;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array);

   function Get_Field_ID
     (Env       : JNI_Env_Access;
      Class     : J_Class;
      Name      : String;
      Sig       : String) return J_Field_ID;

   function Get_Object_Field
     (Env      : JNI_Env_Access;
      Obj      : J_Object;
      Field_ID : J_Field_ID) return J_Object;

   function Get_Boolean_Field
     (Env      : JNI_Env_Access;
      Obj      : J_Object;
      Field_ID : J_Field_ID) return J_Boolean;

   function Get_Byte_Field
     (Env      : JNI_Env_Access;
      Obj      : J_Object;
      Field_ID : J_Field_ID) return J_Byte;

   function Get_Char_Field
     (Env      : JNI_Env_Access;
      Obj      : J_Object;
      Field_ID : J_Field_ID) return J_Char;

   function Get_Short_Field
     (Env      : JNI_Env_Access;
      Obj      : J_Object;
      Field_ID : J_Field_ID) return J_Short;

   function Get_Int_Field
     (Env      : JNI_Env_Access;
      Obj      : J_Object;
      Field_ID : J_Field_ID) return J_Int;

   function Get_Long_Field
     (Env      : JNI_Env_Access;
      Obj      : J_Object;
      Field_ID : J_Field_ID) return J_Long;

   function Get_Float_Field
     (Env      : JNI_Env_Access;
      Obj      : J_Object;
      Field_ID : J_Field_ID) return J_Float;

   function Get_Double_Field
     (Env      : JNI_Env_Access;
      Obj      : J_Object;
      Field_ID : J_Field_ID) return J_Double;

   procedure Set_Object_Field
     (Env      : JNI_Env_Access;
      Obj      : J_Object;
      Field_ID : J_Field_ID;
      Val      : J_Object);

   procedure Set_Boolean_Field
     (Env      : JNI_Env_Access;
      Obj      : J_Object;
      Field_ID : J_Field_ID;
      Val      : J_Boolean);

   procedure Set_Byte_Field
     (Env      : JNI_Env_Access;
      Obj      : J_Object;
      Field_ID : J_Field_ID;
      Val      : J_Byte);

   procedure Set_Char_Field
     (Env      : JNI_Env_Access;
      Obj      : J_Object;
      Field_ID : J_Field_ID;
      Val      : J_Char);

   procedure Set_Short_Field
     (Env      : JNI_Env_Access;
      Obj      : J_Object;
      Field_ID : J_Field_ID;
      Val      : J_Short);

   procedure Set_Int_Field
     (Env      : JNI_Env_Access;
      Obj      : J_Object;
      Field_ID : J_Field_ID;
      Val      : J_Int);

   procedure Set_Long_Field
     (Env      : JNI_Env_Access;
      Obj      : J_Object;
      Field_ID : J_Field_ID;
      Val      : J_Long);

   procedure Set_Float_Field
     (Env      : JNI_Env_Access;
      Obj      : J_Object;
      Field_ID : J_Field_ID;
      Val      : J_Float);

   procedure Set_Double_Field
     (Env      : JNI_Env_Access;
      Obj      : J_Object;
      Field_ID : J_Field_ID;
      Val      : J_Double);

   function Get_Static_Method_ID
     (Env   : JNI_Env_Access;
      Class : J_Class;
      Name  : String;
      Sig   : String) return J_Method_ID;

   function Call_Static_Object_Method_A
     (Env       : JNI_Env_Access;
      Class     : J_Class;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Object;

   function Call_Static_Boolean_Method_A
     (Env       : JNI_Env_Access;
      Class     : J_Class;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Boolean;

   function Call_Static_Byte_Method_A
     (Env       : JNI_Env_Access;
      Class     : J_Class;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Byte;

   function Call_Static_Char_Method_A
     (Env       : JNI_Env_Access;
      Class     : J_Class;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Char;

   function Call_Static_Short_Method_A
     (Env       : JNI_Env_Access;
      Class     : J_Class;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Short;

   function Call_Static_Int_Method_A
     (Env       : JNI_Env_Access;
      Class     : J_Class;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Int;

   function Call_Static_Long_Method_A
     (Env       : JNI_Env_Access;
      Class     : J_Class;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Long;

   function Call_Static_Float_Method_A
     (Env       : JNI_Env_Access;
      Class     : J_Class;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Float;

   function Call_Static_Double_Method_A
     (Env       : JNI_Env_Access;
      Class     : J_Class;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array) return J_Double;

   procedure Call_Static_Void_Method_A
     (Env       : JNI_Env_Access;
      Class     : J_Class;
      Method_ID : J_Method_ID;
      Args      : J_Value_Array);

   function Get_Static_Field_ID
     (Env       : JNI_Env_Access;
      Class     : J_Class;
      Name      : String;
      Sig       : String) return J_Field_ID;

   function Get_Static_Object_Field
     (Env      : JNI_Env_Access;
      Class    : J_Class;
      Field_ID : J_Field_ID) return J_Object;

   function Get_Static_Boolean_Field
     (Env      : JNI_Env_Access;
      Class    : J_Class;
      Field_ID : J_Field_ID) return J_Boolean;

   function Get_Static_Byte_Field
     (Env      : JNI_Env_Access;
      Class    : J_Class;
      Field_ID : J_Field_ID) return J_Byte;

   function Get_Static_Char_Field
     (Env      : JNI_Env_Access;
      Class    : J_Class;
      Field_ID : J_Field_ID) return J_Char;

   function Get_Static_Short_Field
     (Env      : JNI_Env_Access;
      Class    : J_Class;
      Field_ID : J_Field_ID) return J_Short;

   function Get_Static_Int_Field
     (Env      : JNI_Env_Access;
      Class    : J_Class;
      Field_ID : J_Field_ID) return J_Int;

   function Get_Static_Long_Field
     (Env      : JNI_Env_Access;
      Class    : J_Class;
      Field_ID : J_Field_ID) return J_Long;

   function Get_Static_Float_Field
     (Env      : JNI_Env_Access;
      Class    : J_Class;
      Field_ID : J_Field_ID) return J_Float;

   function Get_Static_Double_Field
     (Env      : JNI_Env_Access;
      Class    : J_Class;
      Field_ID : J_Field_ID) return J_Double;

   procedure Set_Static_Object_Field
     (Env      : JNI_Env_Access;
      Class    : J_Class;
      Field_ID : J_Field_ID;
      Val      : J_Object);

   procedure Set_Static_Boolean_Field
     (Env      : JNI_Env_Access;
      Class    : J_Class;
      Field_ID : J_Field_ID;
      Val      : J_Boolean);

   procedure Set_Static_Byte_Field
     (Env      : JNI_Env_Access;
      Class    : J_Class;
      Field_ID : J_Field_ID;
      Val      : J_Byte);

   procedure Set_Static_Char_Field
     (Env      : JNI_Env_Access;
      Class    : J_Class;
      Field_ID : J_Field_ID;
      Val      : J_Char);

   procedure Set_Static_Short_Field
     (Env      : JNI_Env_Access;
      Class    : J_Class;
      Field_ID : J_Field_ID;
      Val      : J_Short);

   procedure Set_Static_Int_Field
     (Env      : JNI_Env_Access;
      Class    : J_Class;
      Field_ID : J_Field_ID;
      Val      : J_Int);

   procedure Set_Static_Long_Field
     (Env      : JNI_Env_Access;
      Class    : J_Class;
      Field_ID : J_Field_ID;
      Val      : J_Long);

   procedure Set_Static_Float_Field
     (Env      : JNI_Env_Access;
      Class    : J_Class;
      Field_ID : J_Field_ID;
      Val      : J_Float);

   procedure Set_Static_Double_Field
     (Env      : JNI_Env_Access;
      Class    : J_Class;
      Field_ID : J_Field_ID;
      Val      : J_Double);

   function New_String
     (Env     : JNI_Env_Access;
      Unicode : J_Char_Array;
      Len     : J_Size) return J_String;

   function Get_String_Length
     (Env : JNI_Env_Access;
      Str : J_String) return J_Size;

   function Get_String_Chars
     (Env     : JNI_Env_Access;
      Str     : J_String;
      Is_Copy : access J_Boolean) return J_Char_Array;

   procedure Release_String_Chars
     (Env   : JNI_Env_Access;
      Str   : J_String;
      Chars : J_Char_Array);

   function New_String_UTF
     (Env : JNI_Env_Access;
      UTF : Interfaces.C.Strings.chars_ptr) return J_String;

   function Get_String_UTF_Length
     (Env : JNI_Env_Access;
      Str : J_String) return J_Size;

   function Get_String_UTF_Chars
     (Env     : JNI_Env_Access;
      Str     : J_String;
      Is_Copy : access J_Boolean) return Interfaces.C.Strings.chars_ptr;

   function Get_String_UTF_Chars
     (Env     : JNI_Env_Access;
      Str     : J_String) return Interfaces.C.Strings.chars_ptr;

   procedure Release_String_UTF_Chars
     (Env   : JNI_Env_Access;
      Str   : J_String;
      Chars : Interfaces.C.Strings.chars_ptr);

   function Get_Array_Length
     (Env : JNI_Env_Access; Arr : J_Array) return J_Size;

   function New_Object_Array
     (Env   : JNI_Env_Access;
      Len   : J_Size;
      Class : J_Class;
      Init  : J_Object) return J_Object_J_Array;

   function Get_Object_Array_Element
     (Env   : JNI_Env_Access;
      Arr   : J_Array;
      Index : J_Size) return J_Object;

   procedure Set_Object_Array_Element
     (Env   : JNI_Env_Access;
      Arr   : J_Array;
      Index : J_Size;
      Val   : J_Object);

   function New_Boolean_Array
     (Env : JNI_Env_Access;
      Len : J_Size) return J_Boolean_J_Array;

   function New_Char_Array
     (Env : JNI_Env_Access;
      Len : J_Size) return J_Char_J_Array;

   function New_Byte_Array
     (Env : JNI_Env_Access;
      Len : J_Size) return J_Byte_J_Array;

   function New_Short_Array
     (Env : JNI_Env_Access;
      Len : J_Size) return J_Short_J_Array;

   function New_Int_Array
     (Env : JNI_Env_Access;
      Len : J_Size) return J_Int_J_Array;

   function New_Long_Array
     (Env : JNI_Env_Access;
      Len : J_Size) return J_Long_J_Array;

   function New_Float_Array
     (Env : JNI_Env_Access;
      Len : J_Size) return J_Float_J_Array;

   function New_Double_Array
     (Env : JNI_Env_Access;
      Len : J_Size) return J_Double_J_Array;

   function Get_Boolean_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Boolean_J_Array;
      Is_Copy : access J_Boolean) return J_Boolean_Star;

   function Get_Char_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Char_J_Array;
      Is_Copy : access J_Boolean) return J_Char_Star;

   function Get_Byte_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Byte_J_Array;
      Is_Copy : access J_Boolean) return J_Byte_Star;

   function Get_Short_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Short_J_Array;
      Is_Copy : access J_Boolean) return J_Short_Star;

   function Get_Int_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Int_J_Array;
      Is_Copy : access J_Boolean) return J_Int_Star;

   function Get_Long_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Long_J_Array;
      Is_Copy : access J_Boolean) return J_Long_Star;

   function Get_Float_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Float_J_Array;
      Is_Copy : access J_Boolean) return J_Float_Star;

   function Get_Double_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Double_J_Array;
      Is_Copy : access J_Boolean) return J_Double_Star;

   procedure Release_Boolean_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Boolean_J_Array;
      Elems   : J_Boolean_Star;
      Mode    : J_Int);

   procedure Release_Byte_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Byte_J_Array;
      Elems   : J_Byte_Star;
      Mode    : J_Int);

   procedure Release_Char_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Char_J_Array;
      Elems   : J_Char_Star;
      Mode    : J_Int);

   procedure Release_Short_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Short_J_Array;
      Elems   : J_Short_Star;
      Mode    : J_Int);

   procedure Release_Int_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Int_J_Array;
      Elems   : J_Int_Star;
      Mode    : J_Int);

   procedure Release_Long_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Long_J_Array;
      Elems   : J_Long_Star;
      Mode    : J_Int);

   procedure Release_Float_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Float_J_Array;
      Elems   : J_Float_Star;
      Mode    : J_Int);

   procedure Release_Double_Array_Elements
     (Env     : JNI_Env_Access;
      Arr     : J_Double_J_Array;
      Elems   : J_Double_Star;
      Mode    : J_Int);

   procedure Get_Boolean_Array_Region
     (Env   : JNI_Env_Access;
      Arr   : J_Boolean_J_Array;
      Start : J_Size;
      Len   : J_Size;
      Buf   : out J_Boolean_Array);

   procedure Get_Char_Array_Region
     (Env   : JNI_Env_Access;
      Arr   : J_Char_J_Array;
      Start : J_Size;
      Len   : J_Size;
      Buf   : out J_Char_Array);

   procedure Get_Byte_Array_Region
     (Env   : JNI_Env_Access;
      Arr   : J_Byte_J_Array;
      Start : J_Size;
      Len   : J_Size;
      Buf   : out J_Byte_Array);

   procedure Get_Short_Array_Region
     (Env   : JNI_Env_Access;
      Arr   : J_Short_J_Array;
      Start : J_Size;
      Len   : J_Size;
      Buf   : out J_Short_Array);

   procedure Get_Int_Array_Region
     (Env   : JNI_Env_Access;
      Arr   : J_Int_J_Array;
      Start : J_Size;
      Len   : J_Size;
      Buf   : out J_Int_Array);

   procedure Get_Long_Array_Region
     (Env   : JNI_Env_Access;
      Arr   : J_Long_J_Array;
      Start : J_Size;
      Len   : J_Size;
      Buf   : out J_Long_Array);

   procedure Get_Float_Array_Region
     (Env   : JNI_Env_Access;
      Arr   : J_Float_J_Array;
      Start : J_Size;
      Len   : J_Size;
      Buf   : out J_Float_Array);

   procedure Get_Double_Array_Region
     (Env   : JNI_Env_Access;
      Arr   : J_Double_J_Array;
      Start : J_Size;
      Len   : J_Size;
      Buf   : out J_Double_Array);

   procedure Set_Boolean_Array_Region
     (Env   : JNI_Env_Access;
      Arr   : J_Boolean_J_Array;
      Start : J_Size;
      Len   : J_Size;
      Buf   : J_Boolean_Array);

   procedure Set_Char_Array_Region
     (Env   : JNI_Env_Access;
      Arr   : J_Char_J_Array;
      Start : J_Size;
      Len   : J_Size;
      Buf   : J_Char_Array);

   procedure Set_Byte_Array_Region
     (Env   : JNI_Env_Access;
      Arr   : J_Byte_J_Array;
      Start : J_Size;
      Len   : J_Size;
      Buf   : J_Byte_Array);

   procedure Set_Short_Array_Region
     (Env   : JNI_Env_Access;
      Arr   : J_Short_J_Array;
      Start : J_Size;
      Len   : J_Size;
      Buf   : J_Short_Array);

   procedure Set_Int_Array_Region
     (Env   : JNI_Env_Access;
      Arr   : J_Int_J_Array;
      Start : J_Size;
      Len   : J_Size;
      Buf   : J_Int_Array);

   procedure Set_Long_Array_Region
     (Env   : JNI_Env_Access;
      Arr   : J_Long_J_Array;
      Start : J_Size;
      Len   : J_Size;
      Buf   : J_Long_Array);

   procedure Set_Float_Array_Region
     (Env   : JNI_Env_Access;
      Arr   : J_Float_J_Array;
      Start : J_Size;
      Len   : J_Size;
      Buf   : J_Float_Array);

   procedure Set_Double_Array_Region
     (Env   : JNI_Env_Access;
      Arr   : J_Double_J_Array;
      Start : J_Size;
      Len   : J_Size;
      Buf   : J_Double_Array);

   type JNI_Native_Method is record
      Name      : Interfaces.C.Strings.chars_ptr;
      Signature : Interfaces.C.Strings.chars_ptr;
      Fn_Ptr    : System.Address;
   end record;
   pragma Convention (C, JNI_Native_Method);

   type JNI_Native_Method_Arr is array
     (Positive range <>) of aliased JNI_Native_Method;
   pragma Convention (C, JNI_Native_Method_Arr);

   function Register_Natives
     (Env       : JNI_Env_Access;
      Class     : J_Class;
      Methods   : JNI_Native_Method_Arr;
      N_Methods : J_Int) return J_Int;

   function Unregister_Natives
     (Env   : JNI_Env_Access;
      Class : J_Class) return J_Int;

   function Monitor_Enter
     (Env : JNI_Env_Access;
      Obj : J_Object) return J_Int;

   function Monitor_Exit
     (Env : JNI_Env_Access;
      Obj : J_Object) return J_Int;

   function Get_Java_VM
     (Env : JNI_Env_Access;
      VM  : access Java_VM_Access) return J_Int;

   procedure Get_String_Region
     (Env   : JNI_Env_Access;
      Str   : J_String;
      Start : J_Size;
      Len   : J_Size;
      Buf   : out J_Char_Array);

   procedure Get_String_UTF_Region
     (Env   : JNI_Env_Access;
      Str   : J_String;
      Start : J_Size;
      Len   : J_Size;
      Buf   : out C.char_array);

   procedure Get_Primitive_Array_Critical
     (Env : JNI_Env_Access;
      Arr : J_Array;
      Is_Copy : access Boolean);

   procedure Release_Primitive_Array_Critical
     (Env     : JNI_Env_Access;
      Arr     : J_Array;
      C_Array : System.Address;
      Mode    : J_Int);

   procedure Get_String_Critical
     (Env     : JNI_Env_Access;
      Str     : J_String;
      Is_Copy : access Boolean);

   procedure Release_String_Critical
     (Env      : JNI_Env_Access;
      Str      : J_String;
      C_String : J_Char_Array);

   function New_Weak_Global_Ref
     (Env : JNI_Env_Access; Obj : J_Object) return J_Weak;

   procedure Delete_Weak_Global_Ref (Env : JNI_Env_Access; Ref : J_Weak);

   function Exception_Check (Env : JNI_Env_Access) return J_Boolean;

   function New_Direct_Byte_Buffer
     (Env      : JNI_Env_Access;
      Addr     : System.Address;
      Capacity : J_Long) return J_Object;

   function Get_Direct_Buffer_Address
     (Env : JNI_Env_Access; Buf : J_Object) return System.Address;

   function Get_Direct_Buffer_Capacity
     (Env : JNI_Env_Access; Buf : J_Object) return J_Long;

private

   pragma Interrupt_State (SIGABRT, System);
   pragma Interrupt_State (SIGFPE,  System);
   pragma Interrupt_State (SIGILL,  System);
   pragma Interrupt_State (SIGSEGV, System);
--   pragma Interrupt_State (SIGBUS,  System);

   ------------
   -- JavaVM --
   ------------

   type Destroy_Java_VM_Access is access
     function (VM : Java_VM_Access)
               return J_Int;
   pragma Convention (JNICall, Destroy_Java_VM_Access);

   type Attach_Current_Thread_Access is access
     function (VM    : Java_VM_Access;
               P_Env : access JNI_Env_Access;
               Args  : System.Address)
               return J_Int;
   pragma Convention (JNICall, Attach_Current_Thread_Access);

   type Detach_Current_Thread_Access is access
     function (VM : Java_VM_Access)
               return J_Int;
   pragma Convention (JNICall, Detach_Current_Thread_Access);

   type Get_Env_Access is access
     function (VM      : Java_VM_Access;
               Penv    : access JNI_Env_Access;
               Version : J_Int := JNI_Version_1_2)
               return J_Int;
   pragma Convention (JNICall, Get_Env_Access);

   type Attach_Current_Thread_As_Daemon_Access is access
     function (VM : Java_VM_Access;
               P_Env : JNI_Env_Access;
               Args  : System.Address)
               return J_Int;
   pragma Convention (JNICall, Attach_Current_Thread_As_Daemon_Access);

   type JNI_Invoke_Interface is record
      Reserved0                       : System.Address;               --  0
      Reserved1                       : System.Address;               --  1
      Reserved2                       : System.Address;               --  2
      Destroy_Java_VM                 : Destroy_Java_VM_Access;       --  3
      Attach_Current_Thread           : Attach_Current_Thread_Access; --  4
      Detach_Current_Thread           : Detach_Current_Thread_Access; --  5
      Get_Env                         : Get_Env_Access;               --  6
      Attach_Current_Thread_As_Daemon :
        Attach_Current_Thread_As_Daemon_Access;                       --  7
   end record;

   type Java_VM is access JNI_Invoke_Interface;

   --------------
   --  JNI_Env --
   --------------

   type Get_Version_Access is access
     function (Env : JNI_Env_Access) return J_Int;
   pragma Convention (JNICall, Get_Version_Access);

   type Define_Class_Access is access
     function (Env    : JNI_Env_Access;
               Name   : String;
               Loader : J_Object;
               Buf    : J_Byte_Array;
               Len    : J_Size)
               return J_Class;
   pragma Convention (JNICall, Define_Class_Access);

   type Find_Class_Access is access
     function (Env  : JNI_Env_Access;
               Name : String)
               return J_Class;
   pragma Convention (JNICall, Find_Class_Access);

   type From_Reflected_Method_Access is access
     function (Env    : JNI_Env_Access;
               Method : J_Object)
               return J_Method_ID;
   pragma Convention (JNICall, From_Reflected_Method_Access);

   type From_Reflected_Field_Access is access
     function (Env   : JNI_Env_Access;
               Field : J_Object)
               return J_Field_ID;
   pragma Convention (JNICall, From_Reflected_Field_Access);

   type To_Reflected_Method_Access is access
     function (Env       : JNI_Env_Access;
               Cls       : J_Class;
               Method_ID : J_Method_ID;
               Is_Static : J_Boolean)
               return J_Object;
   pragma Convention (JNICall, To_Reflected_Method_Access);

   type Get_Superclass_Access is access
     function (Env : JNI_Env_Access;
               Sub : J_Class)
               return J_Class;
   pragma Convention (JNICall, Get_Superclass_Access);

   type Is_Assignable_From_Access is access
     function (Env : JNI_Env_Access;
               Sub : J_Class;
               Sup : J_Class)
               return J_Boolean;
   pragma Convention (JNICall, Is_Assignable_From_Access);

   type To_Reflected_Field_Access is access
     function (Env       : JNI_Env_Access;
               Cls       : J_Class;
               Field_ID  : J_Field_ID;
               Is_Static : J_Boolean)
               return J_Object;
   pragma Convention (JNICall, To_Reflected_Field_Access);

   type Throw_Access is access
     function (Env : JNI_Env_Access;
               Obj : J_Throwable)
               return J_Int;
   pragma Convention (JNICall, Throw_Access);

   type Throw_New_Access is access
     function (Env   : JNI_Env_Access;
               Class : J_Class;
               Msg   : String)
               return J_Int;
   pragma Convention (JNICall, Throw_New_Access);

   type Exception_Occurred_Access is access
     function (Env : JNI_Env_Access) return J_Throwable;
   pragma Convention (JNICall, Exception_Occurred_Access);

   type Exception_Describe_Access is access procedure (Env : JNI_Env_Access);
   pragma Convention (JNICall, Exception_Describe_Access);

   type Exception_Clear_Access is access procedure (Env : JNI_Env_Access);
   pragma Convention (JNICall, Exception_Clear_Access);

   type Fatal_Error_Access is access
     procedure (Env : JNI_Env_Access;
                Msg : String);
   pragma Convention (JNICall, Fatal_Error_Access);

   type Push_Local_Frame_Access is access
     function (Env      : JNI_Env_Access;
               Capacity : J_Int)
               return J_Int;
   pragma Convention (JNICall, Push_Local_Frame_Access);

   type Pop_Local_Frame_Access is access
     function (Env    : JNI_Env_Access;
               Result : J_Object)
               return J_Object;
   pragma Convention (JNICall, Pop_Local_Frame_Access);

   type New_Global_Ref_Access is access
     function (Env  : JNI_Env_Access;
               Lobj : J_Object)
               return J_Object;
   pragma Convention (JNICall, New_Global_Ref_Access);

   type Delete_Global_Ref_Access is access
     procedure (Env : JNI_Env_Access;
                Obj : J_Object);
   pragma Convention (JNICall, Delete_Global_Ref_Access);

   type Delete_Local_Ref_Access is access
     procedure (Env : JNI_Env_Access;
                Obj : J_Object);
   pragma Convention (JNICall, Delete_Local_Ref_Access);

   type Is_Same_Object_Access is access
     function (Env   : JNI_Env_Access;
               Obj_1 : J_Object;
               Obj_2 : J_Object)
               return J_Boolean;
   pragma Convention (JNICall, Is_Same_Object_Access);

   type New_Local_Ref_Access is access
     function (Env : JNI_Env_Access;
               Ref : J_Object)
               return J_Object;
   pragma Convention (JNICall, New_Local_Ref_Access);

   type Ensure_Local_Capacity_Access is access
     function (Env      : JNI_Env_Access;
               Capacity : J_Int)
               return J_Int;
   pragma Convention (JNICall, Ensure_Local_Capacity_Access);

   type Alloc_Object_Access is access
     function (Env   : JNI_Env_Access;
               Class : J_Class)
               return J_Object;
   pragma Convention (JNICall, Alloc_Object_Access);

   type New_Object_A_Access is access
     function (Env       : JNI_Env_Access;
               Class     : J_Class;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Object;
   pragma Convention (JNICall, New_Object_A_Access);

   type Get_Object_Class_Access is access
     function (Env : JNI_Env_Access;
               Obj : J_Object)
               return J_Class;
   pragma Convention (JNICall, Get_Object_Class_Access);

   type Is_Instance_Of_Access is access
     function (Env   : JNI_Env_Access;
               Obj   : J_Object;
               Class : J_Class)
               return J_Boolean;
   pragma Convention (JNICall, Is_Instance_Of_Access);

   type Get_Method_ID_Access is access
     function (Env   : JNI_Env_Access;
               Class : J_Class;
               Name  : String;
               Sig   : String)
               return J_Method_ID;
   pragma Convention (Stdcall, Get_Method_ID_Access);
   
   type Call_Object_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Obj       : J_Object;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Object;
   pragma Convention (JNICall, Call_Object_Method_A_Access);

   type Call_Boolean_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Obj       : J_Object;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Boolean;
   pragma Convention (JNICall, Call_Boolean_Method_A_Access);

   type Call_Byte_Method_A_Access is access
     function  (Env       : JNI_Env_Access;
                Obj       : J_Object;
                Method_ID : J_Method_ID;
                Args      : J_Value_Array)
                return J_Byte;
   pragma Convention (JNICall, Call_Byte_Method_A_Access);

   type Call_Char_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Obj       : J_Object;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Char;
   pragma Convention (JNICall, Call_Char_Method_A_Access);

   type Call_Short_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Obj       : J_Object;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Short;
   pragma Convention (JNICall, Call_Short_Method_A_Access);

   type Call_Int_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Obj       : J_Object;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Int;
   pragma Convention (JNICall, Call_Int_Method_A_Access);

   type Call_Long_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Obj       : J_Object;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Long;
   pragma Convention (JNICall, Call_Long_Method_A_Access);

   type Call_Float_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Obj       : J_Object;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Float;
   pragma Convention (JNICall, Call_Float_Method_A_Access);

   type Call_Double_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Obj       : J_Object;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Double;
   pragma Convention (JNICall, Call_Double_Method_A_Access);

   type Call_Void_Method_A_Access is access
     procedure (Env       : JNI_Env_Access;
                Obj       : J_Object;
                Method_ID : J_Method_ID;
                Args      : J_Value_Array);
   pragma Convention (JNICall, Call_Void_Method_A_Access);

   type Call_Non_Virtual_Object_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Obj       : J_Object;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Object;
   pragma Convention (JNICall, Call_Non_Virtual_Object_Method_A_Access);

   type Call_Non_Virtual_Boolean_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Obj       : J_Object;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Boolean;
   pragma Convention (JNICall, Call_Non_Virtual_Boolean_Method_A_Access);

   type Call_Non_Virtual_Byte_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Obj       : J_Object;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Byte;
   pragma Convention (JNICall, Call_Non_Virtual_Byte_Method_A_Access);

   type Call_Non_Virtual_Char_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Obj       : J_Object;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Char;
   pragma Convention (JNICall, Call_Non_Virtual_Char_Method_A_Access);

   type Call_Non_Virtual_Short_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Obj       : J_Object;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Short;
   pragma Convention (JNICall, Call_Non_Virtual_Short_Method_A_Access);

   type Call_Non_Virtual_Int_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Obj       : J_Object;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Int;
   pragma Convention (JNICall, Call_Non_Virtual_Int_Method_A_Access);

   type Call_Non_Virtual_Long_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Obj       : J_Object;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Long;
   pragma Convention (JNICall, Call_Non_Virtual_Long_Method_A_Access);

   type Call_Non_Virtual_Float_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Obj       : J_Object;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Float;
   pragma Convention (JNICall, Call_Non_Virtual_Float_Method_A_Access);

   type Call_Non_Virtual_Double_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Obj       : J_Object;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Double;
   pragma Convention (JNICall, Call_Non_Virtual_Double_Method_A_Access);

   type Call_Non_Virtual_Void_Method_A_Access is access
     procedure (Env       : JNI_Env_Access;
               Obj       : J_Object;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array);
   pragma Convention (JNICall, Call_Non_Virtual_Void_Method_A_Access);

   type Get_Field_ID_Access is access
     function (Env       : JNI_Env_Access;
               Class     : J_Class;
               Name      : String;
               Sig       : String)
               return J_Field_ID;
   pragma Convention (JNICall, Get_Field_ID_Access);

   type Get_Object_Field_Access is access
     function (Env      : JNI_Env_Access;
               Obj      : J_Object;
               Field_ID : J_Field_ID)
               return J_Object;
   pragma Convention (JNICall, Get_Object_Field_Access);

   type Get_Boolean_Field_Access is access
     function (Env      : JNI_Env_Access;
               Obj      : J_Object;
               Field_ID : J_Field_ID)
               return J_Boolean;
   pragma Convention (JNICall, Get_Boolean_Field_Access);

   type Get_Byte_Field_Access is access
     function (Env      : JNI_Env_Access;
               Obj      : J_Object;
               Field_ID : J_Field_ID)
               return J_Byte;
   pragma Convention (JNICall, Get_Byte_Field_Access);

   type Get_Char_Field_Access is access
     function (Env      : JNI_Env_Access;
               Obj      : J_Object;
               Field_ID : J_Field_ID)
               return J_Char;
   pragma Convention (JNICall, Get_Char_Field_Access);

   type Get_Short_Field_Access is access
     function (Env      : JNI_Env_Access;
               Obj      : J_Object;
               Field_ID : J_Field_ID)
               return J_Short;
   pragma Convention (JNICall, Get_Short_Field_Access);

   type Get_Int_Field_Access is access
     function (Env      : JNI_Env_Access;
               Obj      : J_Object;
               Field_ID : J_Field_ID)
               return J_Int;
   pragma Convention (JNICall, Get_Int_Field_Access);

   type Get_Long_Field_Access is access
     function (Env      : JNI_Env_Access;
               Obj      : J_Object;
               Field_ID : J_Field_ID)
               return J_Long;
   pragma Convention (JNICall, Get_Long_Field_Access);

   type Get_Float_Field_Access is access
     function (Env      : JNI_Env_Access;
               Obj      : J_Object;
               Field_ID : J_Field_ID)
               return J_Float;
   pragma Convention (JNICall, Get_Float_Field_Access);

   type Get_Double_Field_Access is access
     function (Env      : JNI_Env_Access;
               Obj      : J_Object;
               Field_ID : J_Field_ID)
               return J_Double;
   pragma Convention (JNICall, Get_Double_Field_Access);

   type Set_Object_Field_Access is access
     procedure (Env      : JNI_Env_Access;
                Obj      : J_Object;
                Field_ID : J_Field_ID;
                Val      : J_Object);
   pragma Convention (JNICall, Set_Object_Field_Access);

   type Set_Boolean_Field_Access is access
     procedure (Env      : JNI_Env_Access;
                Obj      : J_Object;
                Field_ID : J_Field_ID;
                Val      : J_Boolean);
   pragma Convention (JNICall, Set_Boolean_Field_Access);

   type Set_Byte_Field_Access is access
     procedure (Env      : JNI_Env_Access;
                Obj      : J_Object;
                Field_ID : J_Field_ID;
                Val      : J_Byte);
   pragma Convention (JNICall, Set_Byte_Field_Access);

   type Set_Char_Field_Access is access
     procedure (Env      : JNI_Env_Access;
                Obj      : J_Object;
                Field_ID : J_Field_ID;
                Val      : J_Char);
   pragma Convention (JNICall, Set_Char_Field_Access);

   type Set_Short_Field_Access is access
     procedure (Env      : JNI_Env_Access;
                Obj      : J_Object;
                Field_ID : J_Field_ID;
                Val      : J_Short);
   pragma Convention (JNICall, Set_Short_Field_Access);

   type Set_Int_Field_Access is access
     procedure (Env      : JNI_Env_Access;
                Obj      : J_Object;
                Field_ID : J_Field_ID;
                Val      : J_Int);
   pragma Convention (JNICall, Set_Int_Field_Access);

   type Set_Long_Field_Access is access
     procedure (Env      : JNI_Env_Access;
                Obj      : J_Object;
                Field_ID : J_Field_ID;
                Val      : J_Long);
   pragma Convention (JNICall, Set_Long_Field_Access);

   type Set_Float_Field_Access is access
     procedure (Env      : JNI_Env_Access;
                Obj      : J_Object;
                Field_ID : J_Field_ID;
                Val      : J_Float);
   pragma Convention (JNICall, Set_Float_Field_Access);

   type Set_Double_Field_Access is access
     procedure (Env      : JNI_Env_Access;
                Obj      : J_Object;
                Field_ID : J_Field_ID;
                Val      : J_Double);
   pragma Convention (JNICall, Set_Double_Field_Access);

   type Get_Static_Method_ID_Access is access
     function (Env   : JNI_Env_Access;
               Class : J_Class;
               Name  : String;
               Sig   : String)
               return J_Method_ID;
   pragma Convention (JNICall, Get_Static_Method_ID_Access);

   type Call_Static_Object_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Class     : J_Class;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Object;
   pragma Convention (JNICall, Call_Static_Object_Method_A_Access);

   type Call_Static_Boolean_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Class     : J_Class;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Boolean;
   pragma Convention (JNICall, Call_Static_Boolean_Method_A_Access);

   type Call_Static_Byte_Method_A_Access is access
     function  (Env       : JNI_Env_Access;
                Class     : J_Class;
                Method_ID : J_Method_ID;
                Args      : J_Value_Array)
                return J_Byte;
   pragma Convention (JNICall, Call_Static_Byte_Method_A_Access);

   type Call_Static_Char_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Class     : J_Class;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Char;
   pragma Convention (JNICall, Call_Static_Char_Method_A_Access);

   type Call_Static_Short_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Class     : J_Class;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Short;
   pragma Convention (JNICall, Call_Static_Short_Method_A_Access);

   type Call_Static_Int_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Class     : J_Class;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Int;
   pragma Convention (JNICall, Call_Static_Int_Method_A_Access);

   type Call_Static_Long_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Class     : J_Class;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Long;
   pragma Convention (JNICall, Call_Static_Long_Method_A_Access);

   type Call_Static_Float_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Class     : J_Class;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Float;
   pragma Convention (JNICall, Call_Static_Float_Method_A_Access);

   type Call_Static_Double_Method_A_Access is access
     function (Env       : JNI_Env_Access;
               Class     : J_Class;
               Method_ID : J_Method_ID;
               Args      : J_Value_Array)
               return J_Double;
   pragma Convention (JNICall, Call_Static_Double_Method_A_Access);

   type Call_Static_Void_Method_A_Access is access
     procedure (Env       : JNI_Env_Access;
                Class     : J_Class;
                Method_ID : J_Method_ID;
                Args      : J_Value_Array);
   pragma Convention (JNICall, Call_Static_Void_Method_A_Access);

   type Get_Static_Field_ID_Access is access
     function (Env       : JNI_Env_Access;
               Class     : J_Class;
               Name      : String;
               Sig       : String)
               return J_Field_ID;
   pragma Convention (JNICall, Get_Static_Field_ID_Access);

   type Get_Static_Object_Field_Access is access
     function (Env      : JNI_Env_Access;
               Class    : J_Class;
               Field_ID : J_Field_ID)
               return J_Object;
   pragma Convention (JNICall, Get_Static_Object_Field_Access);

   type Get_Static_Boolean_Field_Access is access
     function (Env      : JNI_Env_Access;
               Class    : J_Class;
               Field_ID : J_Field_ID)
               return J_Boolean;
   pragma Convention (JNICall, Get_Static_Boolean_Field_Access);

   type Get_Static_Byte_Field_Access is access
     function (Env      : JNI_Env_Access;
               Class    : J_Class;
               Field_ID : J_Field_ID)
               return J_Byte;
   pragma Convention (JNICall, Get_Static_Byte_Field_Access);

   type Get_Static_Char_Field_Access is access
     function (Env      : JNI_Env_Access;
               Class    : J_Class;
               Field_ID : J_Field_ID)
               return J_Char;
   pragma Convention (JNICall, Get_Static_Char_Field_Access);

   type Get_Static_Short_Field_Access is access
     function (Env      : JNI_Env_Access;
               Classd   : J_Class;
               Field_ID : J_Field_ID)
               return J_Short;
   pragma Convention (JNICall, Get_Static_Short_Field_Access);

   type Get_Static_Int_Field_Access is access
     function (Env      : JNI_Env_Access;
               Class    : J_Class;
               Field_ID : J_Field_ID)
               return J_Int;
   pragma Convention (JNICall, Get_Static_Int_Field_Access);

   type Get_Static_Long_Field_Access is access
     function (Env      : JNI_Env_Access;
               Class    : J_Class;
               Field_ID : J_Field_ID)
               return J_Long;
   pragma Convention (JNICall, Get_Static_Long_Field_Access);

   type Get_Static_Float_Field_Access is access
     function (Env      : JNI_Env_Access;
               Class    : J_Class;
               Field_ID : J_Field_ID)
               return J_Float;
   pragma Convention (JNICall, Get_Static_Float_Field_Access);

   type Get_Static_Double_Field_Access is access
     function (Env      : JNI_Env_Access;
               Class    : J_Class;
               Field_ID : J_Field_ID)
               return J_Double;
   pragma Convention (JNICall, Get_Static_Double_Field_Access);

   type Set_Static_Object_Field_Access is access
     procedure (Env      : JNI_Env_Access;
                Class    : J_Class;
                Field_ID : J_Field_ID;
                Val      : J_Object);
   pragma Convention (JNICall, Set_Static_Object_Field_Access);

   type Set_Static_Boolean_Field_Access is access
     procedure (Env      : JNI_Env_Access;
                Class    : J_Class;
                Field_ID : J_Field_ID;
                Val      : J_Boolean);
   pragma Convention (JNICall, Set_Static_Boolean_Field_Access);

   type Set_Static_Byte_Field_Access is access
     procedure (Env      : JNI_Env_Access;
                Class    : J_Class;
                Field_ID : J_Field_ID;
                Val      : J_Byte);
   pragma Convention (JNICall, Set_Static_Byte_Field_Access);

   type Set_Static_Char_Field_Access is access
     procedure (Env      : JNI_Env_Access;
                Class    : J_Class;
                Field_ID : J_Field_ID;
                Val      : J_Char);
   pragma Convention (JNICall, Set_Static_Char_Field_Access);

   type Set_Static_Short_Field_Access is access
     procedure (Env      : JNI_Env_Access;
                Classd   : J_Class;
                Field_ID : J_Field_ID;
                Val      : J_Short);
   pragma Convention (JNICall, Set_Static_Short_Field_Access);

   type Set_Static_Int_Field_Access is access
     procedure (Env      : JNI_Env_Access;
                Class    : J_Class;
                Field_ID : J_Field_ID;
                Val      : J_Int);
   pragma Convention (JNICall, Set_Static_Int_Field_Access);

   type Set_Static_Long_Field_Access is access
     procedure (Env      : JNI_Env_Access;
                Class    : J_Class;
                Field_ID : J_Field_ID;
                Val      : J_Long);
   pragma Convention (JNICall, Set_Static_Long_Field_Access);

   type Set_Static_Float_Field_Access is access
     procedure (Env      : JNI_Env_Access;
                Class    : J_Class;
                Field_ID : J_Field_ID;
                Val      : J_Float);
   pragma Convention (JNICall, Set_Static_Float_Field_Access);

   type Set_Static_Double_Field_Access is access
     procedure (Env      : JNI_Env_Access;
                Class    : J_Class;
                Field_ID : J_Field_ID;
                Val      : J_Double);
   pragma Convention (JNICall, Set_Static_Double_Field_Access);

   type New_String_Access is access
     function (Env     : JNI_Env_Access;
               Unicode : J_Char_Array;
               Len     : J_Size)
               return J_String;
   pragma Convention (JNICall, New_String_Access);
   --  ??? Check that Unicode is correct

   type Get_String_Length_Access is access
     function (Env : JNI_Env_Access;
               Str : J_String)
               return J_Size;
   pragma Convention (JNICall, Get_String_Length_Access);

   type Get_String_Chars_Access is access
     function (Env     : JNI_Env_Access;
               Str     : J_String;
               Is_Copy : access J_Boolean)
               return J_Char_Array;
   pragma Convention (JNICall, Get_String_Chars_Access);
   --  ??? Check that the returned type is correct

   type Release_String_Chars_Access is access
     procedure (Env   : JNI_Env_Access;
                Str   : J_String;
                Chars : J_Char_Array);
   pragma Convention (JNICall, Release_String_Chars_Access);
   --  ??? Check that Chars is correct

   type New_String_UTF_Access is access
     function (Env : JNI_Env_Access;
               Utf : Interfaces.C.Strings.chars_ptr)
               return J_String;
   pragma Convention (JNICall, New_String_UTF_Access);

   type Get_String_UTF_Length_Access is access
     function (Env : JNI_Env_Access;
               Str : J_String)
               return J_Size;
   pragma Convention (JNICall, Get_String_UTF_Length_Access);

   type Get_String_UTF_Chars_Access is access
     function (Env  : JNI_Env_Access;
               Str  : J_String;
               Addr : access J_Boolean)
               return Interfaces.C.Strings.chars_ptr;
   pragma Convention (JNICall, Get_String_UTF_Chars_Access);

   type Release_String_UTF_Chars_Access is access
     procedure (Env   : JNI_Env_Access;
                Str   : J_String;
                Chars : Interfaces.C.Strings.chars_ptr);
   pragma Convention (JNICall, Release_String_UTF_Chars_Access);

   type Get_Array_Length_Access is access
     function (Env : JNI_Env_Access; Arr : J_Array) return J_Size;
   pragma Convention (JNICall, Get_Array_Length_Access);

   type New_Object_Array_Access is access
     function (Env   : JNI_Env_Access;
               Len   : J_Size;
               Class : J_Class;
               Init  : J_Object)
               return J_Object_J_Array;
   pragma Convention (JNICall, New_Object_Array_Access);

   type Get_Object_Array_Element_Access is access
     function (Env   : JNI_Env_Access;
               Arr   : J_Array;
               Index : J_Size)
              return J_Object;
   pragma Convention (JNICall, Get_Object_Array_Element_Access);

   type Set_Object_Array_Element_Access is access
     procedure (Env   : JNI_Env_Access;
                Arr   : J_Array;
                Index : J_Size;
                Val   : J_Object);
   pragma Convention (JNICall, Set_Object_Array_Element_Access);

   type New_Boolean_Array_Access is access
     function (Env : JNI_Env_Access;
               Len : J_Size)
               return J_Boolean_J_Array;
   pragma Convention (JNICall, New_Boolean_Array_Access);

   type New_Char_Array_Access is access
     function (Env : JNI_Env_Access;
               Len : J_Size)
               return J_Char_J_Array;
   pragma Convention (JNICall, New_Char_Array_Access);

   type New_Byte_Array_Access is access
     function (Env : JNI_Env_Access;
               Len : J_Size)
               return J_Byte_J_Array;
   pragma Convention (JNICall, New_Byte_Array_Access);

   type New_Short_Array_Access is access
     function (Env : JNI_Env_Access;
               Len : J_Size)
               return J_Short_J_Array;
   pragma Convention (JNICall, New_Short_Array_Access);

   type New_Int_Array_Access is access
     function (Env : JNI_Env_Access;
               Len : J_Size)
               return J_Int_J_Array;
   pragma Convention (JNICall, New_Int_Array_Access);

   type New_Float_Array_Access is access
     function (Env : JNI_Env_Access;
               Len : J_Size)
               return J_Float_J_Array;
   pragma Convention (JNICall, New_Float_Array_Access);

   type New_Double_Array_Access is access
     function (Env : JNI_Env_Access;
               Len : J_Size)
               return J_Double_J_Array;
   pragma Convention (JNICall, New_Double_Array_Access);

   type New_Long_Array_Access is access
     function (Env : JNI_Env_Access;
               Len : J_Size)
               return J_Long_J_Array;
   pragma Convention (JNICall, New_Long_Array_Access);

   type Get_Boolean_Array_Elements_Access is access
     function (Env     : JNI_Env_Access;
               Arr     : J_Boolean_J_Array;
               Is_Copy : access J_Boolean)
               return J_Boolean_Star;
   pragma Convention (JNICall, Get_Boolean_Array_Elements_Access);

   type Get_Char_Array_Elements_Access is access
     function (Env     : JNI_Env_Access;
               Arr     : J_Char_J_Array;
               Is_Copy : access J_Boolean)
               return J_Char_Star;
   pragma Convention (JNICall, Get_Char_Array_Elements_Access);

   type Get_Byte_Array_Elements_Access is access
     function (Env     : JNI_Env_Access;
               Arr     : J_Byte_J_Array;
               Is_Copy : access J_Boolean)
               return J_Byte_Star;
   pragma Convention (JNICall, Get_Byte_Array_Elements_Access);

   type Get_Short_Array_Elements_Access is access
     function (Env     : JNI_Env_Access;
               Arr     : J_Short_J_Array;
               Is_Copy : access J_Boolean)
               return J_Short_Star;
   pragma Convention (JNICall, Get_Short_Array_Elements_Access);

   type Get_Int_Array_Elements_Access is access
     function (Env     : JNI_Env_Access;
               Arr     : J_Int_J_Array;
               Is_Copy : access J_Boolean)
               return J_Int_Star;
   pragma Convention (JNICall, Get_Int_Array_Elements_Access);

   type Get_Long_Array_Elements_Access is access
     function (Env     : JNI_Env_Access;
               Arr     : J_Long_J_Array;
               Is_Copy : access J_Boolean)
               return J_Long_Star;
   pragma Convention (JNICall, Get_Long_Array_Elements_Access);

   type Get_Float_Array_Elements_Access is access
     function (Env     : JNI_Env_Access;
               Arr     : J_Float_J_Array;
               Is_Copy : access J_Boolean)
               return J_Float_Star;
   pragma Convention (JNICall, Get_Float_Array_Elements_Access);

   type Get_Double_Array_Elements_Access is access
     function (Env     : JNI_Env_Access;
               Arr     : J_Double_J_Array;
               Is_Copy : access J_Boolean)
               return J_Double_Star;
   pragma Convention (JNICall, Get_Double_Array_Elements_Access);

   type Release_Boolean_Array_Elements_Access is access
     procedure (Env     : JNI_Env_Access;
                Arr     : J_Boolean_J_Array;
                Elems   : J_Boolean_Star;
                Mode    : J_Int);
   pragma Convention (JNICall, Release_Boolean_Array_Elements_Access);

   type Release_Byte_Array_Elements_Access is access
     procedure (Env     : JNI_Env_Access;
                Arr     : J_Byte_J_Array;
                Elems   : J_Byte_Star;
                Mode    : J_Int);
   pragma Convention (JNICall, Release_Byte_Array_Elements_Access);

   type Release_Char_Array_Elements_Access is access
     procedure (Env     : JNI_Env_Access;
                Arr     : J_Char_J_Array;
                Elems   : J_Char_Star;
                Mode    : J_Int);
   pragma Convention (JNICall, Release_Char_Array_Elements_Access);

   type Release_Short_Array_Elements_Access is access
     procedure (Env     : JNI_Env_Access;
                Arr     : J_Short_J_Array;
                Elems   : J_Short_Star;
                Mode    : J_Int);
   pragma Convention (JNICall, Release_Short_Array_Elements_Access);

   type Release_Int_Array_Elements_Access is access
     procedure (Env     : JNI_Env_Access;
                Arr     : J_Int_J_Array;
                Elems   : J_Int_Star;
                Mode    : J_Int);
   pragma Convention (JNICall, Release_Int_Array_Elements_Access);

   type Release_Long_Array_Elements_Access is access
     procedure (Env     : JNI_Env_Access;
                Arr     : J_Long_J_Array;
                Elems   : J_Long_Star;
                Mode    : J_Int);
   pragma Convention (JNICall, Release_Long_Array_Elements_Access);

   type Release_Float_Array_Elements_Access is access
     procedure (Env     : JNI_Env_Access;
                Arr     : J_Float_J_Array;
                Elems   : J_Float_Star;
                Mode    : J_Int);
   pragma Convention (JNICall, Release_Float_Array_Elements_Access);

   type Release_Double_Array_Elements_Access is access
     procedure (Env     : JNI_Env_Access;
                Arr     : J_Double_J_Array;
                Elems   : J_Double_Star;
                Mode    : J_Int);
   pragma Convention (JNICall, Release_Double_Array_Elements_Access);

   type Get_Boolean_Array_Region_Access is access
     procedure (Env   : JNI_Env_Access;
                Arr   : J_Boolean_J_Array;
                Start : J_Size;
                Len   : J_Size;
                Buf   : out J_Boolean_Array);
   pragma Convention (JNICall, Get_Boolean_Array_Region_Access);

   type Get_Char_Array_Region_Access is access
     procedure (Env   : JNI_Env_Access;
                Arr   : J_Char_J_Array;
                Start : J_Size;
                Len   : J_Size;
                Buf   : out J_Char_Array);
   pragma Convention (JNICall, Get_Char_Array_Region_Access);

   type Get_Byte_Array_Region_Access is access
     procedure (Env   : JNI_Env_Access;
                Arr   : J_Byte_J_Array;
                Start : J_Size;
                Len   : J_Size;
                Buf   : out J_Byte_Array);
   pragma Convention (JNICall, Get_Byte_Array_Region_Access);

   type Get_Short_Array_Region_Access is access
     procedure (Env   : JNI_Env_Access;
                Arr   : J_Short_J_Array;
                Start : J_Size;
                Len   : J_Size;
                Buf   : out J_Short_Array);
   pragma Convention (JNICall, Get_Short_Array_Region_Access);

   type Get_Int_Array_Region_Access is access
     procedure (Env   : JNI_Env_Access;
                Arr   : J_Int_J_Array;
                Start : J_Size;
                Len   : J_Size;
                Buf   : out J_Int_Array);
   pragma Convention (JNICall, Get_Int_Array_Region_Access);

   type Get_Long_Array_Region_Access is access
     procedure (Env   : JNI_Env_Access;
                Arr   : J_Long_J_Array;
                Start : J_Size;
                Len   : J_Size;
                Buf   : out J_Long_Array);
   pragma Convention (JNICall, Get_Long_Array_Region_Access);

   type Get_Float_Array_Region_Access is access
     procedure (Env   : JNI_Env_Access;
                Arr   : J_Float_J_Array;
                Start : J_Size;
                Len   : J_Size;
                Buf   : out J_Float_Array);
   pragma Convention (JNICall, Get_Float_Array_Region_Access);

   type Get_Double_Array_Region_Access is access
     procedure (Env   : JNI_Env_Access;
                Arr   : J_Double_J_Array;
                Start : J_Size;
                Len   : J_Size;
                Buf   : out J_Double_Array);
   pragma Convention (JNICall, Get_Double_Array_Region_Access);

   type Set_Boolean_Array_Region_Access is access
     procedure (Env   : JNI_Env_Access;
                Arr   : J_Boolean_J_Array;
                Start : J_Size;
                Len   : J_Size;
                Buf   : J_Boolean_Array);
   pragma Convention (JNICall, Set_Boolean_Array_Region_Access);

   type Set_Char_Array_Region_Access is access
     procedure (Env   : JNI_Env_Access;
                Arr   : J_Char_J_Array;
                Start : J_Size;
                Len   : J_Size;
                Buf   : J_Char_Array);
   pragma Convention (JNICall, Set_Char_Array_Region_Access);

   type Set_Byte_Array_Region_Access is access
     procedure (Env   : JNI_Env_Access;
                Arr   : J_Byte_J_Array;
                Start : J_Size;
                Len   : J_Size;
                Buf   : J_Byte_Array);
   pragma Convention (JNICall, Set_Byte_Array_Region_Access);

   type Set_Short_Array_Region_Access is access
     procedure (Env   : JNI_Env_Access;
                Arr   : J_Short_J_Array;
                Start : J_Size;
                Len   : J_Size;
                Buf   : J_Short_Array);
   pragma Convention (JNICall, Set_Short_Array_Region_Access);

   type Set_Int_Array_Region_Access is access
     procedure (Env   : JNI_Env_Access;
                Arr   : J_Int_J_Array;
                Start : J_Size;
                Len   : J_Size;
                Buf   : J_Int_Array);
   pragma Convention (JNICall, Set_Int_Array_Region_Access);

   type Set_Long_Array_Region_Access is access
     procedure (Env   : JNI_Env_Access;
                Arr   : J_Long_J_Array;
                Start : J_Size;
                Len   : J_Size;
                Buf   : J_Long_Array);
   pragma Convention (JNICall, Set_Long_Array_Region_Access);

   type Set_Float_Array_Region_Access is access
     procedure (Env   : JNI_Env_Access;
                Arr   : J_Float_J_Array;
                Start : J_Size;
                Len   : J_Size;
                Buf   : J_Float_Array);
   pragma Convention (JNICall, Set_Float_Array_Region_Access);

   type Set_Double_Array_Region_Access is access
     procedure (Env   : JNI_Env_Access;
                Arr   : J_Double_J_Array;
                Start : J_Size;
                Len   : J_Size;
                Buf   : J_Double_Array);
   pragma Convention (JNICall, Set_Double_Array_Region_Access);

   type Register_Natives_Access is access
     function (Env       : JNI_Env_Access;
               Class     : J_Class;
               Methods   : JNI_Native_Method_Arr;
               N_Methods : J_Int) return J_Int;
   pragma Convention (JNICall, Register_Natives_Access);

   type Unregister_Natives_Access is access
     function (Env   : JNI_Env_Access;
               Class : J_Class)
               return J_Int;
   pragma Convention (JNICall, Unregister_Natives_Access);

   type Monitor_Enter_Access is access
     function (Env : JNI_Env_Access;
               Obj : J_Object)
               return J_Int;
   pragma Convention (JNICall, Monitor_Enter_Access);

   type Monitor_Exit_Access is access
     function (Env : JNI_Env_Access;
               Obj : J_Object)
               return J_Int;
   pragma Convention (JNICall, Monitor_Exit_Access);

   type Get_Java_VM_Access is access
     function (Env : JNI_Env_Access;
               VM  : access Java_VM_Access)
               return J_Int;
   pragma Convention (JNICall, Get_Java_VM_Access);
   --  ??? check that VM parameter works

   type Get_String_Region_Access is access
     procedure (Env   : JNI_Env_Access;
                Str   : J_String;
                Start : J_Size;
                Len   : J_Size;
                Buf   : out J_Char_Array);
   pragma Convention (JNICall, Get_String_Region_Access);
   --  ??? Check Buf.

   type Get_String_UTF_Region_Access is access
     procedure (Env   : JNI_Env_Access;
                Str   : J_String;
                Start : J_Size;
                Len   : J_Size;
                Buf   : out C.char_array);
   pragma Convention (JNICall, Get_String_UTF_Region_Access);
   --  ??? Check Buf.

   type Get_Primitive_Array_Critical_Access is access
     procedure (Env : JNI_Env_Access;
                Arr : J_Array;
                Is_Copy : access Boolean);
   pragma Convention (JNICall, Get_Primitive_Array_Critical_Access);

   type Release_Primitive_Array_Critical_Access is access
     procedure (Env     : JNI_Env_Access;
                Arr     : J_Array;
                C_Array : System.Address;
                Mode    : J_Int);
   pragma Convention (JNICall, Release_Primitive_Array_Critical_Access);

   type Get_String_Critical_Access is access
     procedure (Env     : JNI_Env_Access;
                Str     : J_String;
                Is_Copy : access Boolean);
   pragma Convention (JNICall, Get_String_Critical_Access);

   type Release_String_Critical_Access is access
     procedure (Env      : JNI_Env_Access;
                Str      : J_String;
                C_String : J_Char_Array);
   pragma Convention (JNICall, Release_String_Critical_Access);

   type New_Weak_Global_Ref_Access is access
     function (Env : JNI_Env_Access;
               Obj : J_Object)
               return J_Weak;
   pragma Convention (JNICall, New_Weak_Global_Ref_Access);

   type Delete_Weak_Global_Ref_Access is access
     procedure (Env : JNI_Env_Access;
                Ref : J_Weak);
   pragma Convention (JNICall, Delete_Weak_Global_Ref_Access);

   type Exception_Check_Access is access function (Env : JNI_Env_Access)
                                                   return J_Boolean;
   pragma Convention (JNICall, Exception_Check_Access);

   type New_Direct_Byte_Buffer_Access is access
     function (Env      : JNI_Env_Access;
               Addr     : System.Address;
               Capacity : J_Long)
               return J_Object;
   pragma Convention (JNICall, New_Direct_Byte_Buffer_Access);

   type Get_Direct_Buffer_Address_Access is access
     function (Env : JNI_Env_Access;
               Buf : J_Object) return System.Address;
   pragma Convention (JNICall, Get_Direct_Buffer_Address_Access);

   type Get_Direct_Buffer_Capacity_Access is access
     function (Env : JNI_Env_Access;
               Buf : J_Object)
               return J_Long;
   pragma Convention (JNICall, Get_Direct_Buffer_Capacity_Access);

   type JNI_Native_Interface is record
      Reserved0             : System.Address;                           --    0
      Reserved1             : System.Address;                           --    1
      Reserved2             : System.Address;                           --    2
      Reserved3             : System.Address;                           --    3
      Get_Version           : Get_Version_Access;                       --    4
      Define_Class          : Define_Class_Access;                      --    5
      Find_Class            : Find_Class_Access;                        --    6
      From_Reflected_Method : From_Reflected_Method_Access;             --    7
      From_Reflected_Field  : From_Reflected_Field_Access;              --    8
      To_Reflected_Method   : To_Reflected_Method_Access;               --    9
      Get_Superclass        : Get_Superclass_Access;                    --   10
      Is_Assignable_From    : Is_Assignable_From_Access;                --   11
      To_Reflected_Field    : To_Reflected_Field_Access;                --   12
      Throw                 : Throw_Access;                             --   13
      Throw_New             : Throw_New_Access;                         --   14
      Exception_Occurred    : Exception_Occurred_Access;                --   15
      Exception_Describe    : Exception_Describe_Access;                --   16
      Exception_Clear       : Exception_Clear_Access;                   --   17
      Fatal_Error           : Fatal_Error_Access;                       --   18
      Push_Local_Frame      : Push_Local_Frame_Access;                  --   19
      Pop_Local_Frame       : Pop_Local_Frame_Access;                   --   20
      New_Global_Ref        : New_Global_Ref_Access;                    --   21
      Delete_Global_Ref     : Delete_Global_Ref_Access;                 --   22
      Delete_Local_Ref      : Delete_Local_Ref_Access;                  --   23
      Is_Same_Object        : Is_Same_Object_Access;                    --   24
      New_Local_Ref         : New_Local_Ref_Access;                     --   25
      Ensure_Local_Capacity : Ensure_Local_Capacity_Access;             --   26
      Alloc_Object          : Alloc_Object_Access;                      --   27
      New_Object            : System.Address;                           --   28
      New_Object_V          : System.Address;                           --   29
      New_Object_A          : New_Object_A_Access;                      --   30
      Get_Object_Class      : Get_Object_Class_Access;                  --   31
      Is_Instance_Of        : Is_Instance_Of_Access;                    --   32
      Get_Method_ID         : Get_Method_ID_Access;                     --   33
      Call_Object_Method    : System.Address;                           --   34
      Call_Object_Method_V  : System.Address;                           --   35
      Call_Object_Method_A  : Call_Object_Method_A_Access;              --   36
      Call_Boolean_Method   : System.Address;                           --   37
      Call_Boolean_Method_V : System.Address;                           --   38
      Call_Boolean_Method_A : Call_Boolean_Method_A_Access;             --   39
      Call_Byte_Method      : System.Address;                           --   40
      Call_Byte_Method_V    : System.Address;                           --   41
      Call_Byte_Method_A    : Call_Byte_Method_A_Access;                --   42
      Call_Char_Method      : System.Address;                           --   43
      Call_Char_Method_V    : System.Address;                           --   44
      Call_Char_Method_A    : Call_Char_Method_A_Access;                --   45
      Call_Short_Method     : System.Address;                           --   46
      Call_Short_Method_V   : System.Address;                           --   47
      Call_Short_Method_A   : Call_Short_Method_A_Access;               --   48
      Call_Int_Method       : System.Address;                           --   49
      Call_Int_Method_V     : System.Address;                           --   50
      Call_Int_Method_A     : Call_Int_Method_A_Access;                 --   51
      Call_Long_Method      : System.Address;                           --   52
      Call_Long_Method_V    : System.Address;                           --   53
      Call_Long_Method_A    : Call_Long_Method_A_Access;                --   54
      Call_Float_Method     : System.Address;                           --   55
      Call_Float_Method_V   : System.Address;                           --   56
      Call_Float_Method_A   : Call_Float_Method_A_Access;               --   57
      Call_Double_Method    : System.Address;                           --   58
      Call_Double_Method_V  : System.Address;                           --   59
      Call_Double_Method_A  : Call_Double_Method_A_Access;              --   60
      Call_Void_Method      : System.Address;                           --   61
      Call_Void_Method_V    : System.Address;                           --   62
      Call_Void_Method_A    : Call_Void_Method_A_Access;                --   63
      Call_Non_Virtual_Object_Method    : System.Address;               --   64
      Call_Non_Virtual_Object_Method_V  : System.Address;               --   65
      Call_Non_Virtual_Object_Method_A  :
        Call_Non_Virtual_Object_Method_A_Access;                        --   66
      Call_Non_Virtual_Boolean_Method   : System.Address;               --   67
      Call_Non_Virtual_Boolean_Method_V : System.Address;               --   68
      Call_Non_Virtual_Boolean_Method_A :
        Call_Non_Virtual_Boolean_Method_A_Access;                       --   69
      Call_Non_Virtual_Byte_Method      : System.Address;               --   70
      Call_Non_Virtual_Byte_Method_V    : System.Address;               --   71
      Call_Non_Virtual_Byte_Method_A    :
        Call_Non_Virtual_Byte_Method_A_Access;                          --   72
      Call_Non_Virtual_Char_Method      : System.Address;               --   73
      Call_Non_Virtual_Char_Method_V    : System.Address;               --   74
      Call_Non_Virtual_Char_Method_A    :
        Call_Non_Virtual_Char_Method_A_Access;                          --   75
      Call_Non_Virtual_Short_Method     : System.Address;               --   76
      Call_Non_Virtual_Short_Method_V   : System.Address;               --   77
      Call_Non_Virtual_Short_Method_A   :
        Call_Non_Virtual_Short_Method_A_Access;                         --   78
      Call_Non_Virtual_Int_Method       : System.Address;               --   79
      Call_Non_Virtual_Int_Method_V     : System.Address;               --   80
      Call_Non_Virtual_Int_Method_A     :
        Call_Non_Virtual_Int_Method_A_Access;                           --   81
      Call_Non_Virtual_Long_Method      : System.Address;               --   82
      Call_Non_Virtual_Long_Method_V    : System.Address;               --   83
      Call_Non_Virtual_Long_Method_A    :
        Call_Non_Virtual_Long_Method_A_Access;                          --   84
      Call_Non_Virtual_Float_Method     : System.Address;               --   85
      Call_Non_Virtual_Float_Method_V   : System.Address;               --   86
      Call_Non_Virtual_Float_Method_A   :
        Call_Non_Virtual_Float_Method_A_Access;                         --   87
      Call_Non_Virtual_Double_Method    : System.Address;               --   88
      Call_Non_Virtual_Double_Method_V  : System.Address;               --   89
      Call_Non_Virtual_Double_Method_A  :
        Call_Non_Virtual_Double_Method_A_Access;                        --   90
      Call_Non_Virtual_Void_Method      : System.Address;               --   91
      Call_Non_Virtual_Void_Method_V    : System.Address;               --   92
      Call_Non_Virtual_Void_Method_A    :
        Call_Non_Virtual_Void_Method_A_Access;                          --   93
      Get_Field_ID      : Get_Field_ID_Access;                          --   94
      Get_Object_Field  : Get_Object_Field_Access;                      --   95
      Get_Boolean_Field : Get_Boolean_Field_Access;                     --   96
      Get_Byte_Field    : Get_Byte_Field_Access;                        --   97
      Get_Char_Field    : Get_Char_Field_Access;                        --   98
      Get_Short_Field   : Get_Short_Field_Access;                       --   99
      Get_Int_Field     : Get_Int_Field_Access;                         --  100
      Get_Long_Field    : Get_Long_Field_Access;                        --  101
      Get_Float_Field   : Get_Float_Field_Access;                       --  102
      Get_Double_Field  : Get_Double_Field_Access;                      --  103
      Set_Object_Field  : Set_Object_Field_Access;                      --  104
      Set_Boolean_Field : Set_Boolean_Field_Access;                     --  105
      Set_Byte_Field    : Set_Byte_Field_Access;                        --  106
      Set_Char_Field    : Set_Char_Field_Access;                        --  107
      Set_Short_Field   : Set_Short_Field_Access;                       --  108
      Set_Int_Field     : Set_Int_Field_Access;                         --  109
      Set_Long_Field    : Set_Long_Field_Access;                        --  110
      Set_Float_Field   : Set_Float_Field_Access;                       --  111
      Set_Double_Field  : Set_Double_Field_Access;                      --  112
      Get_Static_Method_ID : Get_Static_Method_ID_Access;               --  113
      Call_Static_Object_Method    : System.Address;                    --  114
      Call_Static_Object_Method_V  : System.Address;                    --  115
      Call_Static_Object_Method_A  :
        Call_Static_Object_Method_A_Access;                             --  116
      Call_Static_Boolean_Method   : System.Address;                    --  117
      Call_Static_Boolean_Method_V : System.Address;                    --  118
      Call_Static_Boolean_Method_A :
        Call_Static_Boolean_Method_A_Access;                            --  119
      Call_Static_Byte_Method      : System.Address;                    --  120
      Call_Static_Byte_Method_V    : System.Address;                    --  121
      Call_Static_Byte_Method_A    :
        Call_Static_Byte_Method_A_Access;                               --  122
      Call_Static_Char_Method      : System.Address;                    --  123
      Call_Static_Char_Method_V    : System.Address;                    --  124
      Call_Static_Char_Method_A    :
        Call_Static_Char_Method_A_Access;                               --  125
      Call_Static_Short_Method     : System.Address;                    --  126
      Call_Static_Short_Method_V   : System.Address;                    --  127
      Call_Static_Short_Method_A   :
        Call_Static_Short_Method_A_Access;                              --  128
      Call_Static_Int_Method       : System.Address;                    --  129
      Call_Static_Int_Method_V     : System.Address;                    --  130
      Call_Static_Int_Method_A     :
        Call_Static_Int_Method_A_Access;                                --  131
      Call_Static_Long_Method      : System.Address;                    --  132
      Call_Static_Long_Method_V    : System.Address;                    --  133
      Call_Static_Long_Method_A    :
        Call_Static_Long_Method_A_Access;                               --  134
      Call_Static_Float_Method     : System.Address;                    --  135
      Call_Static_Float_Method_V   : System.Address;                    --  136
      Call_Static_Float_Method_A   :
        Call_Static_Float_Method_A_Access;                              --  137
      Call_Static_Double_Method    : System.Address;                    --  138
      Call_Static_Double_Method_V  : System.Address;                    --  139
      Call_Static_Double_Method_A  :
        Call_Static_Double_Method_A_Access;                             --  140
      Call_Static_Void_Method      : System.Address;                    --  141
      Call_Static_Void_Method_V    : System.Address;                    --  142
      Call_Static_Void_Method_A    :
        Call_Static_Void_Method_A_Access;                               --  143
      Get_Static_Field_ID          :
        Get_Static_Field_ID_Access;                                     --  144
      Get_Static_Object_Field  :
        Get_Static_Object_Field_Access;                                 --  145
      Get_Static_Boolean_Field : Get_Static_Boolean_Field_Access;       --  146
      Get_Static_Byte_Field    : Get_Static_Byte_Field_Access;          --  147
      Get_Static_Char_Field    : Get_Static_Char_Field_Access;          --  148
      Get_Static_Short_Field   : Get_Static_Short_Field_Access;         --  149
      Get_Static_Int_Field     : Get_Static_Int_Field_Access;           --  150
      Get_Static_Long_Field    : Get_Static_Long_Field_Access;          --  151
      Get_Static_Float_Field   : Get_Static_Float_Field_Access;         --  152
      Get_Static_Double_Field  : Get_Static_Double_Field_Access;        --  153
      Set_Static_Object_Field  : Set_Static_Object_Field_Access;        --  154
      Set_Static_Boolean_Field : Set_Static_Boolean_Field_Access;       --  155
      Set_Static_Byte_Field    : Set_Static_Byte_Field_Access;          --  156
      Set_Static_Char_Field    : Set_Static_Char_Field_Access;          --  157
      Set_Static_Short_Field   : Set_Static_Short_Field_Access;         --  158
      Set_Static_Int_Field     : Set_Static_Int_Field_Access;           --  159
      Set_Static_Long_Field    : Set_Static_Long_Field_Access;          --  160
      Set_Static_Float_Field   : Set_Static_Float_Field_Access;         --  161
      Set_Static_Double_Field  : Set_Static_Double_Field_Access;        --  162
      New_String               : New_String_Access;                     --  163
      Get_String_Length        : Get_String_Length_Access;              --  164
      Get_String_Chars         : Get_String_Chars_Access;               --  165
      Release_String_Chars     : Release_String_Chars_Access;           --  166
      New_String_UTF           : New_String_UTF_Access;                 --  167
      Get_String_UTF_Length    : Get_String_UTF_Length_Access;          --  168
      Get_String_UTF_Chars     : Get_String_UTF_Chars_Access;           --  169
      Release_String_UTF_Chars : Release_String_UTF_Chars_Access;       --  170
      Get_Array_Length         : Get_Array_Length_Access;               --  171
      New_Object_Array         : New_Object_Array_Access;               --  172
      Get_Object_Array_Element : Get_Object_Array_Element_Access;       --  173
      Set_Object_Array_Element : Set_Object_Array_Element_Access;       --  174
      New_Boolean_Array        : New_Boolean_Array_Access;              --  175
      New_Byte_Array           : New_Byte_Array_Access;                 --  176
      New_Char_Array           : New_Char_Array_Access;                 --  177
      New_Short_Array          : New_Short_Array_Access;                --  178
      New_Int_Array            : New_Int_Array_Access;                  --  179
      New_Long_Array           : New_Long_Array_Access;                 --  180
      New_Float_Array          : New_Float_Array_Access;                --  181
      New_Double_Array         : New_Double_Array_Access;               --  182
      Get_Boolean_Array_Elements : Get_Boolean_Array_Elements_Access;   --  183
      Get_Byte_Array_Elements    : Get_Byte_Array_Elements_Access;      --  184
      Get_Char_Array_Elements    : Get_Char_Array_Elements_Access;      --  185
      Get_Short_Array_Elements   : Get_Short_Array_Elements_Access;     --  186
      Get_Int_Array_Elements     : Get_Int_Array_Elements_Access;       --  187
      Get_Long_Array_Elements    : Get_Long_Array_Elements_Access;      --  188
      Get_Float_Array_Elements   : Get_Float_Array_Elements_Access;     --  189
      Get_Double_Array_Elements  : Get_Double_Array_Elements_Access;    --  190
      Release_Boolean_Array_Elements :
        Release_Boolean_Array_Elements_Access;                          --  191
      Release_Byte_Array_Elements    :
        Release_Byte_Array_Elements_Access;                             --  192
      Release_Char_Array_Elements    :
        Release_Char_Array_Elements_Access;                             --  193
      Release_Short_Array_Elements   :
        Release_Short_Array_Elements_Access;                            --  194
      Release_Int_Array_Elements     :
        Release_Int_Array_Elements_Access;                              --  195
      Release_Long_Array_Elements    :
        Release_Long_Array_Elements_Access;                             --  196
      Release_Float_Array_Elements   :
        Release_Float_Array_Elements_Access;                            --  197
      Release_Double_Array_Elements  :
        Release_Double_Array_Elements_Access;                           --  198
      Get_Boolean_Array_Region : Get_Boolean_Array_Region_Access;       --  199
      Get_Byte_Array_Region    : Get_Byte_Array_Region_Access;          --  200
      Get_Char_Array_Region    : Get_Char_Array_Region_Access;          --  201
      Get_Short_Array_Region   : Get_Short_Array_Region_Access;         --  202
      Get_Int_Array_Region     : Get_Int_Array_Region_Access;           --  203
      Get_Long_Array_Region    : Get_Long_Array_Region_Access;          --  204
      Get_Float_Array_Region   : Get_Float_Array_Region_Access;         --  205
      Get_Double_Array_Region  : Get_Double_Array_Region_Access;        --  206
      Set_Boolean_Array_Region : Set_Boolean_Array_Region_Access;       --  207
      Set_Byte_Array_Region    : Set_Byte_Array_Region_Access;          --  208
      Set_Char_Array_Region    : Set_Char_Array_Region_Access;          --  209
      Set_Short_Array_Region   : Set_Short_Array_Region_Access;         --  210
      Set_Int_Array_Region     : Set_Int_Array_Region_Access;           --  211
      Set_Long_Array_Region    : Set_Long_Array_Region_Access;          --  212
      Set_Float_Array_Region   : Set_Float_Array_Region_Access;         --  213
      Set_Double_Array_Region  : Set_Double_Array_Region_Access;        --  214
      Register_Natives         : Register_Natives_Access;               --  215
      Unregister_Natives       : Unregister_Natives_Access;             --  216
      Monitor_Enter            : Monitor_Enter_Access;                  --  217
      Monitor_Exit             : Monitor_Exit_Access;                   --  218
      Get_Java_VM              : Get_Java_VM_Access;                    --  219
      Get_String_Region        : Get_String_Region_Access;              --  220
      Get_String_UTF_Region    : Get_String_UTF_Region_Access;          --  221
      Get_Primitive_Array_Critical :
        Get_Primitive_Array_Critical_Access;                            --  222
      Release_Primitive_Array_Critical  :
        Release_Primitive_Array_Critical_Access;                        --  223
      Get_String_Critical        : Get_String_Critical_Access;          --  224
      Release_String_Critical    : Release_String_Critical_Access;      --  225
      New_Weak_Global_Ref        : New_Weak_Global_Ref_Access;          --  226
      Delete_Weak_Global_Ref     : Delete_Weak_Global_Ref_Access;       --  227
      Exception_Check            : Exception_Check_Access;              --  228
      New_Direct_Byte_Buffer     : New_Direct_Byte_Buffer_Access;       --  229
      Get_Direct_Buffer_Address  : Get_Direct_Buffer_Address_Access;    --  230
      Get_Direct_Buffer_Capacity : Get_Direct_Buffer_Capacity_Access;   --  231
   end record;
   pragma Convention (C, JNI_Native_Interface);

   type JNI_Env is access all JNI_Native_Interface;

end Artics.Java.JNI;
