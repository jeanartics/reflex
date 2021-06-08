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

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with System; use System;
with Interfaces.C; use Interfaces.C;
with Artics.Dynamic_Arrays;
with Artics.Objects; use Artics.Objects;

package Artics.Array_Types is
   
   package Boolean_Arrays is new Artics.Dynamic_Arrays (Boolean, False);
   subtype Boolean_Array is Boolean_Arrays.Dynamic_Array;
   type Boolean_Array_Ptr is access all Boolean_Array;

   type Byte is mod 2**8;
   package Byte_Arrays is new Artics.Dynamic_Arrays (Byte, 0);
   subtype Byte_Array  is Byte_Arrays.Dynamic_Array; 
   subtype Byte_Array_Ptr is Byte_Arrays.Dynamic_Array_Ptr;
   
   type Short is mod 2**16;
   package Short_Arrays is new Artics.Dynamic_Arrays (Short, 0);
   subtype Short_Array is Short_Arrays.Dynamic_Array;
   subtype Short_Array_Ptr is Short_Arrays.Dynamic_Array_Ptr;
   
   package Integer_Arrays is new Artics.Dynamic_Arrays (Integer, 0);
   subtype Integer_Array is Integer_Arrays.Dynamic_Array;
   subtype Integer_Array_Ptr is Integer_Arrays.Dynamic_Array_Ptr;
   type Integer_Ptr is access all Integer;
   
   package Float_Arrays is new Artics.Dynamic_Arrays (Float, 0.0);
   subtype Float_Array is Float_Arrays.Dynamic_Array;
   subtype Float_Array_Ptr is Float_Arrays.Dynamic_Array_Ptr;
   
   type String_Ptr is access all String;
   type Wide_String_Ptr is access all Wide_String;
   package String_Arrays is new Artics.Dynamic_Arrays (String_Ptr, null);
   subtype String_Array is String_Arrays.Dynamic_Array;
   subtype String_Array_Ptr is String_Arrays.Dynamic_Array_Ptr;
   
   procedure Free_String is new Ada.Unchecked_Deallocation
     (String, String_Ptr);
   procedure Free_Wide_String is new Ada.Unchecked_Deallocation
     (Wide_String, Wide_String_Ptr);
   
   subtype Char is Interfaces.C.Char;
   Char_Nul : constant Char := Interfaces.C.nul;
   package Char_Arrays is new Artics.Dynamic_Arrays (Char, Char_Nul);
   subtype Char_Array is Char_Arrays.Dynamic_Array;
   subtype Char_Array_Ptr is Char_Arrays.Dynamic_Array_Ptr;
   
   function To_Byte is new Ada.Unchecked_Conversion (Char, Byte);
   function To_Char is new Ada.Unchecked_Conversion (Byte, Char);
   function To_Byte_Array_Ptr is new Ada.Unchecked_Conversion
     (Char_Array_Ptr, Byte_Array_Ptr);
   function To_Char_Array_Ptr is new Ada.Unchecked_Conversion
     (Byte_Array_Ptr, Char_Array_Ptr);
   function To_String_Ptr is new Ada.Unchecked_Conversion
     (Byte_Array_Ptr, String_Ptr);
   function To_String_Ptr is new Ada.Unchecked_Conversion
     (Char_Array_Ptr, String_Ptr);
   
   subtype WChar is Interfaces.C.Wchar_T;
   Wide_Nul : constant Wchar := Interfaces.C.Wide_Nul;
   package WChar_Arrays is new Artics.Dynamic_Arrays (WChar, Interfaces.C.Wide_Nul);
   subtype WChar_Array is WChar_Arrays.Dynamic_Array;
   -- subtype WChar_Array is Win32.WChar_Array;
   -- Array (Natural range <>) of aliased Char;
   subtype WChar_Array_Ptr is WChar_Arrays.Dynamic_Array_Ptr;
   
   function To_Short is new Ada.Unchecked_Conversion (WChar, Short);
   function To_Wchar is new Ada.Unchecked_Conversion (Short, WChar);  
   function To_Short_Array_Ptr is new Ada.Unchecked_Conversion
     (Wchar_Array_Ptr, Short_Array_Ptr);
   function To_Wchar_Array_Ptr is new Ada.Unchecked_Conversion
     (Short_Array_Ptr, Wchar_Array_Ptr);
   function To_Wide_String_Ptr is new Ada.Unchecked_Conversion
     (Wchar_Array_Ptr, Wide_String_Ptr);
 
   subtype Os_Pointer is System.Address;
   Null_Os_Pointer : Os_Pointer := System.Null_Address;
   package Os_Pointer_Arrays is 
      new Artics.Dynamic_Arrays (Os_Pointer, Null_Os_Pointer);
   subtype Os_Pointer_Array is Os_Pointer_Arrays.Dynamic_Array;
   subtype Os_Pointer_Array_Ptr is Os_Pointer_Arrays.Dynamic_Array_Ptr;
   
   function To_Address is new Ada.Unchecked_Conversion
     (Os_Pointer, System.Address);
   function To_Os_Pointer is new Ada.Unchecked_Conversion
     (System.Address, Os_Pointer);
   
   function To_Address is new Ada.Unchecked_Conversion
     (Integer, System.Address);
   function To_Integer is new Ada.Unchecked_Conversion
     (System.Address, Integer);
   
end Artics.Array_Types;
  
