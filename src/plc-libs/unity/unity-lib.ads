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

package Unity.Lib is
   
   type Int is range -2 ** 15 .. +2 ** 15 - 1;
   for Int'Size use 16;
   --  Signed 16-bit integer

   type Uint is mod 2 ** 16;
   for Uint'Size use 16;
   --  Unsigned 16-bit integer

   type Dint is range -2 ** 31 .. +2 ** 31 - 1;
   for Dint'Size use 32;
   --  Signed 32-bit integer

   type Udint is mod 2 ** 32;
   for Udint'Size use 32;
   --  Signed 32-bit integer

   subtype Nat is Int range 0 .. Int'Last;
   --  Non-negative Int values

   subtype Pos is Int range 1 .. Int'Last;
   --  Positive Int values

   type Word is mod 2 ** 16;
   for Word'Size use 16;
   --  Unsigned 32-bit integer

   type Dword is mod 2 ** 32;
   for Dword'Size use 32;
   --  Unsigned 32-bit integer

   type Short is range -32768 .. +32767;
   for Short'Size use 16;
   --  16-bit signed integer

   type Byte is mod 2 ** 8;
   for Byte'Size use 8;
   --  8-bit unsigned integer
   
   procedure Initialize_Unity_Lib;
   
   ----------------------------------
   -- Numeric Conversion Functions --
   ----------------------------------
   
   function Byte_To_Int   (N : Node_Id) return Int;
   function Byte_To_Uint  (N : Node_Id) return Uint;
   function Byte_To_Dint  (N : Node_Id) return Dint;
   function Byte_To_Udint (N : Node_Id) return Udint;
   function Byte_To_Real  (N : Node_Id) return Float;
   
   function Int_To_Byte   (N : Node_Id) return Byte;
   function Int_To_Uint   (N : Node_Id) return Uint;
   function Int_To_Dint   (N : Node_Id) return Dint;
   function Int_To_Udint  (N : Node_Id) return Udint;
   function Int_To_Real   (N : Node_Id) return Float;
   
   function Uint_To_Byte  (N : Node_Id) return Byte;
   function Uint_To_Int   (N : Node_Id) return Int;
   function Uint_To_Dint  (N : Node_Id) return Dint;
   function Uint_To_Udint (N : Node_Id) return Udint;
   function Uint_To_Real  (N : Node_Id) return Float;
   
   function Dint_To_Byte  (N : Node_Id) return Byte;
   function Dint_To_Int   (N : Node_Id) return Int;
   function Dint_To_Uint  (N : Node_Id) return Uint;
   function Dint_To_Udint (N : Node_Id) return Udint;
   function Dint_To_Real  (N : Node_Id) return Float;
   
   function Udint_To_Byte (N : Node_Id) return Byte;
   function Udint_To_Int  (N : Node_Id) return Int;
   function Udint_To_Uint (N : Node_Id) return Uint;
   function Udint_To_Dint (N : Node_Id) return Dint;
   function Udint_To_Real (N : Node_Id) return Float;
   
   function Real_To_Byte  (N : Node_Id) return Byte;
   function Real_To_Int   (N : Node_Id) return Int;
   function Real_To_Uint  (N : Node_Id) return Uint;
   function Real_To_Dint  (N : Node_Id) return Dint;
   function Real_To_Udint (N : Node_Id) return Udint;
   
end Unity.Lib;
