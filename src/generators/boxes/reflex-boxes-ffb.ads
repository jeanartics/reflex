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
with Ada.Unchecked_Deallocation;

with Reflex_Options;         use Reflex_Options;
with Reflex.Boxes.Terminals; use Reflex.Boxes.Terminals;

package Reflex.Boxes.Ffb is

   --  soit une FFB = Une classe
   --  soit FFB avec 1 IN 1 OUT = une classe, 2 IN 1 out = une classe...
   --  soit une classe pour toutes les ffb

   type Ffb_Kind is
     (No_Ffb_Kind,

      --  1 IN 1 OUT
      Abs_Ffb,
      --  Abs_dint_ffb, abs_int_ffb, abs_real_abs_udint_ffb, abs_uint_ffb ??

      --  1 IN 1 OUT
      Exp_Ffb,

      --  1 IN 1 OUT
      Sqrt_Ffb,

      --  2 IN 1 OUT (IN1 & IN2)
      Mod_Ffb,

      --  2 IN 1 OUT (IN1 & IN2)
      Max_Ffb,

      --  2 IN 1 OUT (IN1 & IN2)
      Min_Ffb,

      --  3 IN 1 OUT (MN & IN & MX)
      Limit_Ffb,

      --  1 IN 1 OUT
      Inc_Ffb,

      --  1 IN 1 OUT
      Acos_Ffb,
      --  Acos_real ??

      --  1 IN 1 OUT
      Asin_Ffb,
      --  Asin_real ??

      --  1 IN 1 OUT
      Atan_Ffb,
      --  Atan_real ??

      --  1 IN 1 OUT
      Sin_Ffb,

      --  1 IN 1 OUT
      Cos_Ffb,

      --  1 IN 1 OUT
      Tan_Ffb,

      --  2 IN 1 OUT (IN1 & N)
      Shl_Ffb,

      --  2 IN 1 OUT (IN1 & N)
      Shr_Ffb);

   type Ffb_Box_Record is new Terminal_Box_Record with private;
   type Ffb_Box_Ptr is access all Ffb_Box_Record;
   type Ffb_Box_Class_Ptr is access all Ffb_Box_Record'Class;

   No_Ffb_Box_Record : constant Ffb_Box_Record;

   function New_Ffb_Box return Ffb_Box_Ptr;

   procedure Free_Ffb_Box (This : in out Ffb_Box_Ptr);

   procedure Place_Box (This : access Ffb_Box_Record);
   procedure Absolute_Place_Box (This : access Ffb_Box_Record);

   function Get_Ffb_Kind (This : access Ffb_Box_Record) return Ffb_Kind;
   procedure Set_Ffb_Kind
     (This : access Ffb_Box_Record;
      Kind : Ffb_Kind);

   function Get_En (This : access Ffb_Box_Record) return Boolean;
   procedure Set_En
     (This : access Ffb_Box_Record;
      En : Boolean);

   function Get_Eno (This : access Ffb_Box_Record) return Boolean;
   procedure Set_Eno
     (This : access Ffb_Box_Record;
      Eno : Boolean);

   function Get_In_1 (This : access Ffb_Box_Record) return Node_Id;
   procedure Set_In_1
     (This : access Ffb_Box_Record;
      In_1 : Node_Id);

   function Get_In_2 (This : access Ffb_Box_Record) return Node_Id;
   procedure Set_In_2
     (This : access Ffb_Box_Record;
      In_2 : Node_Id);

   function Get_In_3 (This : access Ffb_Box_Record) return Node_Id;
   procedure Set_In_3
     (This : access Ffb_Box_Record;
      In_3 : Node_Id);

   function Get_Out_1 (This : access Ffb_Box_Record) return Node_Id;
   procedure Set_Out_1
     (This : access Ffb_Box_Record;
      Out_1 : Node_Id);

private

   type Ffb_Box_Record is new Terminal_Box_Record with record
      Kind_Ffb : Ffb_Kind;
      En       : Boolean;
      Eno      : Boolean;

      In_1  : Node_Id;
      In_2  : Node_Id;
      In_3  : Node_Id;
      Out_1 : Node_Id;

      --  for more FFB
      --  In_4 : Node_Id;
      --  In_5 : Node_Id;
      --  In_6 : Node_Id;
      --  In_7 : Node_Id;
   end record;

   No_Ffb_Box_Record : constant Ffb_Box_Record :=
     (No_Terminal_Box_Record with
      Kind_Ffb => No_Ffb_Kind,
      En       => False,
      Eno      => False,

      In_1     => Empty,
      In_2     => Empty,
      In_3     => Empty,
      Out_1    => Empty);

end Reflex.Boxes.Ffb;
