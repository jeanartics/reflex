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
-- Reflex is originally developed  by the Artics team at Grenoble (France). --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Numerics.Elementary_Functions;

package Artics.Maths is
   
   package Maths_Functions renames Ada.Numerics.Elementary_Functions;
   
   Default_Epsilon : constant Float := 0.00001;
   
   Pi : constant Float :=
          3.14159_26535_89793_23846_26433_83279_50288_41971_69399_37511;

   e : constant Float :=
         2.71828_18284_59045_23536_02874_71352_66249_77572_47093_69996;
   
   Degre_In_Radian : constant Float := 0.0174533;
   
   Mm_To_Inch : constant Float := 0.0393700787402;
   Inch_To_Mm : constant Float := 1.0 / Mm_To_Inch;
   
   Mm_To_Point : constant Float := 2.83465;
   Point_To_Mm : constant Float := 1.0 / Mm_To_Point;
   
   Deg_To_Rad : constant Float := 0.0174533;
   Rad_To_Deg : constant Float := 57.2958;
   
   -- Format A4
   --Xmax_Landscape_Mn : Float := 297.0;
   --Ymax_Landscape_Mn : Float := 210.0;
   -- Format A3
   Xmax_Landscape_Mn : Float := 420.0;
   Ymax_Landscape_Mn : Float := 297.0;
   
   Xmax_Portrait_Mn : Float := Ymax_Landscape_Mn;
   Ymax_Portrait_Mn : Float := Xmax_Landscape_Mn;
   
   
   function Min
     (Val1 : Integer;
      Val2 : Integer) return Integer;

   function Max
     (Val1 : Integer;
      Val2 : Integer) return Integer;
   
   
   function Sqrt (X : Float) return Float renames Maths_Functions.Sqrt;

   function Log (X : Float) return Float renames Maths_Functions.Log;

   function Log (X, Base : Float) return Float renames Maths_Functions.Log;

   function Exp (X : Float) return Float renames Maths_Functions.Exp;

   function "**" (Left, Right : Float) return Float 
     renames Maths_Functions."**";
   
   function Pow (Left, Right : Float) return Float 
     renames Maths_Functions."**";
   
   function Sin (X : Float) return Float renames Maths_Functions.Sin;

   function Sin (X, Cycle : Float) return Float renames Maths_Functions.Sin;

   function Cos (X : Float) return Float renames Maths_Functions.Cos;

   function Cos (X, Cycle : Float) return Float renames Maths_Functions.Cos;

   function Tan (X : Float) return Float renames Maths_Functions.Tan;

   function Tan (X, Cycle : Float) return Float renames Maths_Functions.Tan;

   function Cot (X : Float) return Float renames Maths_Functions.Cot;

   function Cot (X, Cycle : Float) return Float renames Maths_Functions.Cot;

   function Arcsin (X : Float) return Float renames Maths_Functions.Arcsin;

   function Arcsin (X, Cycle : Float) return Float
     renames Maths_Functions.Arcsin;

   function Arccos (X : Float) return Float renames Maths_Functions.Arccos;

   function Arccos (X, Cycle : Float) return Float 
     renames Maths_Functions.Arccos;

   function Arctan
     (Y : Float;
      X : Float := 1.0) return Float renames Maths_Functions.Arctan;

   function Arctan
     (Y     : Float;
      X     : Float := 1.0;
      Cycle : Float) return Float renames Maths_Functions.Arctan;

   function Arccot
     (X   : Float;
      Y   : Float := 1.0) return Float renames Maths_Functions.Arccot;

   function Arccot
     (X     : Float;
      Y     : Float := 1.0;
      Cycle : Float) return Float renames Maths_Functions.Arccot;

   function Sinh (X : Float) return Float renames Maths_Functions.Sinh;

   function Cosh (X : Float) return Float renames Maths_Functions.Cosh;

   function Tanh (X : Float) return Float renames Maths_Functions.Tanh;

   function Coth (X : Float) return Float renames Maths_Functions.Coth;
     
   function Arcsinh (X : Float) return Float renames Maths_Functions.Arcsinh;

   function Arccosh (X : Float) return Float renames Maths_Functions.Arccosh;

   function Arctanh (X : Float) return Float renames Maths_Functions.Arctanh;

   function Arccoth (X : Float) return Float renames Maths_Functions.Arccoth;
   
   function Arctan2
     (X : Float;
      Y : Float) return Float;
   
   function Epsilon_Equal
     (V1      : Float;
      V2      : Float;
      Epsilon : Float := Default_Epsilon) return Boolean;
   
   function Min
     (Val1 : Float;
      Val2 : Float) return Float;

   function Max
     (Val1 : Float;
      Val2 : Float) return Float;
   
   function Round (X : Float) return Float;
   
   function Truncate (X : Float) return Float;
   
   function Floor (X : Float) return Float;
   
   function To_Radians (Deg : Float) return Float;
   function To_Degres  (Rad : Float) return Float;
   
   function Ceil (X : Float) return Float;
   
   function Millimeter_To_Inch (X : Float) return Float;
   function Inch_To_Millimeter (X : Float) return Float;
   
   function Millimeter_To_Point (X : Float) return Float;
   function Point_To_Millimeter (X : Float) return Float;
   
   function Degre_To_Radian (X : Float) return Float;
   function Radian_To_Degre (X : Float) return Float;
   
   function IEEE_Remainder 
     (Dividend : Float;
      Divisor  : Float) return Float;
   
end Artics.Maths;
