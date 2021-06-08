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

--  This package provides an Import to the C functions which provide
--  values related to types on the target system. It is only needed for
--  exp_dbug and the elaboration of ttypes.

--  NOTE:  Any changes in this package must be reflected in jgettarg.ads
--  and aa_getta.ads!

--  Note that all these values return sizes of C types with corresponding
--  names. This allows GNAT to define the corresponding Ada types to have
--  the same representation. There is one exception to this: the
--  Wide_Character_Type uses twice the size of a C char, instead of the
--  size of wchar_t.

with Types; use Types;

package Get_Targ is
   pragma Preelaborate;

   function Get_Bits_Per_Unit return Pos;

   function Get_Bits_Per_Word return Pos;

   function Get_Char_Size return Pos; -- Standard.Character'Size

   function Get_Wchar_T_Size return Pos; -- Interfaces.C.wchar_t'Size

   function Get_Short_Size return Pos; -- Standard.Short_Integer'Size

   function Get_Int_Size return Pos; -- Standard.Integer'Size

   function Get_Long_Size return Pos; -- Standard.Long_Integer'Size

   function Get_Long_Long_Size return Pos; -- Standard.Long_Long_Integer'Size

   function Get_Float_Size return Pos; -- Standard.Float'Size

   function Get_Double_Size return Pos; -- Standard.Long_Float'Size

   function Get_Long_Double_Size return Pos; -- Standard.Long_Long_Float'Size

   function Get_Pointer_Size return Pos; -- System.Address'Size

   function Get_Maximum_Alignment return Pos;

   function Get_Float_Words_BE return Nat;

   function Get_Words_BE return Nat;

   function Get_Bytes_BE return Nat;

   function Get_Bits_BE return Nat;

   function Get_Strict_Alignment return Nat;

   function Get_System_Allocator_Alignment return Nat;

   function Get_Double_Float_Alignment return Nat;

   function Get_Double_Scalar_Alignment return Nat;

   function Get_Max_Unaligned_Field return Pos;
   --  Returns the maximum supported size in bits for a field that is
   --  not aligned on a storage unit boundary.

   function Width_From_Size  (Size : Pos) return Pos;
   function Digits_From_Size (Size : Pos) return Pos;
   --  Calculate values for 'Width or 'Digits from 'Size

end Get_Targ;
