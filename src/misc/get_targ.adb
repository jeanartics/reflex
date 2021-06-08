------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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
-- Reflex is a fork from the GNAT compiler. GNAT was originally developed   --
-- by the GNAT team at  New York University. Extensive  contributions to    --
-- GNAT were provided by Ada Core Technologies Inc. Reflex is developed  by --
-- the Artics team at Grenoble.                                             --
--                                                                          --
------------------------------------------------------------------------------

package body Get_Targ is

   ----------------------
   -- Digits_From_Size --
   ----------------------

   function Digits_From_Size (Size : Pos) return Pos is
   begin
      if    Size =  32 then
         return  6;
      elsif Size =  48 then
         return  9;
      elsif Size =  64 then
         return 15;
      elsif Size =  96 then
         return 18;
      elsif Size = 128 then
         return 18;
      else
         raise Program_Error;
      end if;
   end Digits_From_Size;

   -----------------------------
   -- Get_Max_Unaligned_Field --
   -----------------------------

   function Get_Max_Unaligned_Field return Pos is
   begin
      return 64;  -- Can be different on some targets (e.g., AAMP)
   end Get_Max_Unaligned_Field;

   ---------------------
   -- Width_From_Size --
   ---------------------

   function Width_From_Size  (Size : Pos) return Pos is
   begin
      if    Size =  8 then
         return  4;
      elsif Size = 16 then
         return  6;
      elsif Size = 32 then
         return 11;
      elsif Size = 64 then
         return 21;
      else
         raise Program_Error;
      end if;
   end Width_From_Size;


   function Get_Bits_Per_Unit return Pos is
   begin
      return 8;
   end Get_Bits_Per_Unit;

   function Get_Bits_Per_Word return Pos is
   begin
      return 16;
   end Get_Bits_Per_Word;

   function Get_Char_Size return Pos is -- Standard.Character'Size
   begin
      return 8;
   end Get_Char_Size;

   function Get_Wchar_T_Size return Pos is -- Interfaces.C.wchar_t'Size
   begin
      return 16;
   end Get_Wchar_T_Size;

   function Get_Short_Size return Pos is -- Standard.Short_Integer'Size
   begin
      return 16;
   end Get_Short_Size;

   function Get_Int_Size return Pos is -- Standard.Integer'Size
   begin
      return 32;
   end Get_Int_Size;

   function Get_Long_Size return Pos is -- Standard.Long_Integer'Size
   begin
      return 32;
   end Get_Long_Size;

   function Get_Long_Long_Size return Pos is -- Standard.Long_Long_Integer'Size
   begin
      return 64;
   end Get_Long_Long_Size;

   function Get_Float_Size return Pos is -- Standard.Float'Size
   begin
      return 32;
   end Get_Float_Size;

   function Get_Double_Size return Pos is -- Standard.Long_Float'Size
   begin
      return 32;
   end Get_Double_Size;

   function Get_Long_Double_Size return Pos is -- Standard.Long_Long_Float'Size
   begin
      return 64;
   end Get_Long_Double_Size;

   function Get_Pointer_Size return Pos is -- System.Address'Size
   begin
      return 32;
   end Get_Pointer_Size;

   function Get_Maximum_Alignment return Pos is
   begin
      return 64;
   end Get_Maximum_Alignment;

   function Get_Float_Words_BE return Nat is
   begin
      return 1;
   end Get_Float_Words_BE;

   function Get_Words_BE return Nat is
   begin
      return 1;
   end Get_Words_BE;

   function Get_Bytes_BE return Nat is
   begin
      return 4;
   end Get_Bytes_BE;

   function Get_Bits_BE return Nat is
   begin
      return 8;
   end Get_Bits_BE;

   function Get_Strict_Alignment return Nat is
   begin
      return 8;
   end Get_Strict_Alignment;

   function Get_System_Allocator_Alignment return Nat is
   begin
      return 4;
   end Get_System_Allocator_Alignment;

   function Get_Double_Float_Alignment return Nat is
   begin
      return 0;
   end Get_Double_Float_Alignment;

   function Get_Double_Scalar_Alignment return Nat is
   begin
      return 0;
   end Get_Double_Scalar_Alignment;
   
end Get_Targ;
