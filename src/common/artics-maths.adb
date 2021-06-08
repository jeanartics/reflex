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

package body Artics.Maths is
   
   ---------
   -- Max --
   ---------

   function Max
     (Val1 : Integer;
      Val2 : Integer) return Integer
   is
      Val : Integer;
   begin
      if Val1 > Val2 then
	 Val := Val1;
      else
	 Val := Val2;
      end if;
      return Val;
   end Max;

  ----------
   -- Min --
   ---------

   function Min
     (Val1 : Integer;
      Val2 : Integer) return Integer
   is
      Val : Integer;
   begin
      if Val1 < Val2 then
	 Val := Val1;
      else
	 Val := Val2;
      end if;
      return Val;
   end Min;
   
   
   -------------
   -- Arctan2 --
   -------------
   
   function Arctan2
     (X : Float;
      Y : Float) return Float is
      
      Result      : Float;
      Pi          : Float := 3.14159265358979323846;
      Pi_Over_Two : Float := 1.57079632679489661923;
   begin
      if x = 0.0 and y = 0.0 then
	 Result := 0.0;
	 
      elsif abs (x) <= abs (y) then
	 Result := arctan (x / y);

	 if Y <= 0.0 then
	    if X <= 0.0 then
	       Result := Result - Pi;
	    else
	       Result := Result + Pi;
	    end if;
	 end if;
      else
	 Result := Arctan (-y / x);

	 if X <= 0.0 then
	    Result := Result - Pi_Over_Two;
	 else
	    Result := Result +  Pi_Over_Two;
	 end if;
      end if;

      return result;
   end Arctan2;

   --------------------
   -- Epsilon_Equal --
   --------------------
   
   function Epsilon_Equal
     (V1      : Float;
      V2      : Float;
      Epsilon : Float := Default_Epsilon) return Boolean is
   begin
      return (abs (V2-V1)) < Epsilon;
   end Epsilon_Equal;
   
   ----------------
   -- Saturation --
   ----------------

   function Saturation
     (Value : in Float;
      Low   : in Float;
      High  : in Float) return Float
   is
      Val : Float;
   begin
      if Value < Low then
	 Val := Low;
      elsif Value > High then
	 Val := High;
      else
	 Val := Value;
      end if;
      return Val;
   end Saturation;

   ----------------------
   -- Saturation_Value --
   ----------------------

   procedure Saturate_Value
     (Value : in out Float;
      Low   : in Float;
      High  : in Float) is
      
      Val : Float;
   begin
      if Value < Low then
	 Val := Low;
      elsif Value > High then
	 Val := High;
      else
	 Val := Value;
      end if;
      Value := Val;
   end Saturate_Value;
   
   ---------------
   -- Dead_Band --
   ---------------
   
   procedure Dead_Band
     (In_Value       : in Float;
      Out_Value      : out Float;
      Dead_Band_High : in Float;
      Dead_Band_Low  : in Float) is
   begin
      if In_Value > Dead_Band_Low and In_Value < Dead_Band_High then
	 Out_Value := 0.0;
      else
	 Out_Value := In_Value;
      end if;
   end Dead_Band;
   
   ----------
   -- Gain --
   ----------
   
   procedure Gain 
     (Value : in Float;
      G     : in Float;
      Q     : out Float) is
   begin
      Q := Value * G;
   end Gain;
   
   ---------
   -- Max --
   ---------

   function Max
     (Val1 : Float;
      Val2 : Float) return Float
   is
      Val : Float;
   begin
      if Val1 > Val2 then
	 Val := Val1;
      else
	 Val := Val2;
      end if;
      return Val;
   end Max;

  ----------
   -- Min --
   ---------

   function Min
     (Val1 : Float;
      Val2 : Float) return Float
   is
      Val : Float;
   begin
      if Val1 < Val2 then
	 Val := Val1;
      else
	 Val := Val2;
      end if;
      return Val;
   end Min;
   
   -----------
   -- Round --
   -----------
   
   function Round (X : Float) return Float is
   begin
      return Truncate (X);
   end Round;
   
   --------------
   -- Truncate --
   --------------
   
   function Truncate (X : Float) return Float is
      Y : Integer := Integer (X);
   begin
      return Float (Y);
   end Truncate;
   
   -----------
   -- Floor --
   -----------
   
   function Floor (X : Float) return Float is
      Y : Float := Truncate (X);
   begin
      if X > 0.0 then
	 return Y;
      elsif X = Y then
	 return Y;
      else
	 return Y - 1.0;
      end if;
   end Floor;
   
   ----------------
   -- To_Radians --
   ----------------
   
   function To_Radians (Deg : Float) return Float is
   begin
      return  (Pi * Deg / 180.0); 
   end To_Radians;
   
   --------------
   -- To_Degre --
   --------------
   
   function To_Degres (Rad : Float) return Float is
   begin
      return  (Rad * 180.0) / Pi; 
   end To_Degres;
   
   ----------
   -- Ceil --
   ----------
   
   function Ceil (X : Float) return Float is
      Y : Float := abs (X);
   begin
      if X < 0.0 then
	 if -X - Y > 0.0 then
	    return -Y - 1.0;
	 else
	    return -Y;
	 end if;
      else
	 if X - Y > 0.0 then
	    return Y + 1.0;
	 else
	    return Y;
	 end if;
      end if;
   end Ceil;
   
   ------------------------
   -- Millimeter_To_Inch --
   ------------------------
   
   function Millimeter_To_Inch (X : Float) return Float is
   begin
      return X * Mm_To_Inch;
   end Millimeter_To_Inch;
   
   ------------------------
   -- Inch_To_Millimeter --
   ------------------------
   
   function Inch_To_Millimeter (X : Float) return Float is
   begin
      return X * Inch_To_Mm;
   end Inch_To_Millimeter;
   
   -------------------------
   -- Millimeter_To_Point --
   -------------------------
   
   function Millimeter_To_Point (X : Float) return Float is
   begin
      return X * Mm_To_Point;
   end Millimeter_To_Point;
   
   -------------------------
   -- Point_To_Millimeter --
   -------------------------
   
   function Point_To_Millimeter (X : Float) return Float is
   begin
      return X * Point_To_Mm;
   end Point_To_Millimeter;
   
   ---------------------
   -- Degre_To_Radian --
   ---------------------
   
   function Degre_To_Radian (X : Float) return Float is
   begin
      return X * Deg_To_Rad;
   end Degre_To_Radian;
   
   ---------------------
   -- Radian_Ti_Degre --
   ---------------------
   
   function Radian_To_Degre (X : Float) return Float is
   begin
      return X * Rad_To_Deg;
   end Radian_To_Degre;
   
   --------------------
   -- IEEE_Remainder --
   --------------------
   
   function IEEE_Remainder 
     (Dividend : Float;
      Divisor  : Float) return Float is
      
      Res : Float;
   begin
      if Epsilon_Equal (Divisor, 0.0) then
	 return 0.0;
      end if;
      
      Res := Dividend -(Divisor * Maths.Round (Dividend / Divisor));
      
      return Res;
   end IEEE_Remainder;
   
end Artics.Maths;
