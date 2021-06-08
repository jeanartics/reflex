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

with Artics.Utils; use Artics.Utils;

package body Artics.Geometry.Rectangles is

   ---------
   -- Min --
   ---------
   
   function Min (X1, X2 : Coordinate) return Coordinate is
   begin
      if X1 < X2 then
         return X1;
      else
         return X2;
      end if;
   end Min;
   
   ---------
   -- Max --
   ---------
   
   function Max (X1, X2 : Coordinate) return Coordinate is
   begin
      if X1 > X2 then
         return X1;
      else
         return X2;
      end if;
   end Max;
   
   -------------------
   -- New_Rectangle --
   -------------------
   
   function New_Rectangle return Rectangle_Record is
   begin
      return No_Rectangle_Record;
   end New_Rectangle;
   
   -------------------
   -- New_Rectangle --
   -------------------
   
   function New_Rectangle (Rect : Rectangle_Record)  return Rectangle_Record is
      R : Rectangle_Record;
   begin
      R := New_Rectangle;
      R := Rect;
      
      return R;
   end New_Rectangle;
  
   -------------------
   -- New_Rectangle --
   -------------------
   
   function New_Rectangle
     (X      : Coordinate;
      Y      : Coordinate;
      Width  : Coordinate;
      Height : Coordinate) return Rectangle_Record is
      
      R : Rectangle_Record;
   begin
      R := New_Rectangle;
      Set_Rect (R, X, Y, Width, Height);
      return R;
   end New_Rectangle;
   
   -------------------
   -- New_Rectangle --
   -------------------
   
   function New_Rectangle (D : Dimension_Record)  return Rectangle_Record is
   begin
      return New_Rectangle (0.0, 0.0, Get_Width (D), Get_Height (D));
   end New_Rectangle;
   
   ----------------
   -- Get_Origin --
   ----------------
   
   function Get_Origin (R : Rectangle_Record) return Point_Record is
   begin
      return R.Origin;
   end Get_Origin;
   
   ----------------
   -- Set_Origin --
   ----------------
   
   procedure Set_Origin
     (R : in out Rectangle_Record;
      C : Point_Record) is
   begin
      R.Origin := C;
   end Set_Origin;
   
   -----------
   -- Get_X --
   -----------
   
   function Get_X (R : Rectangle_Record) return Coordinate is
   begin
      return R.Origin.X;
   end Get_X;
   
   -- Set_Y --
   -----------
   
   procedure Set_X
     (R : in out Rectangle_Record;
      X : Coordinate) is
   begin
      R.Origin.X := X;
   end Set_X;
   
   -----------
   -- Get_Y --
   -----------
   
   function Get_Y (R : Rectangle_Record) return Coordinate is
   begin
      return R.Origin.Y;
   end Get_Y;

   -----------
   -- Set_Y --
   -----------
   
   procedure Set_Y
     (R : in out Rectangle_Record;
      Y : Coordinate) is
   begin
      R.Origin.Y := Y;
   end Set_Y;
   
   ---------------
   -- Get_Width --
   ---------------
   
   function Get_Width (R : Rectangle_Record) return Coordinate is
   begin
      return R.Width;
   end Get_Width;
   
   ---------------
   -- Set_Width --
   ---------------
   
   procedure Set_Width
     (R : in out Rectangle_Record;
      W : Coordinate) is
   begin
      R.Width := W;
   end Set_Width;
   
   ----------------
   -- Get_Height --
   ----------------
   
   function Get_Height (R : Rectangle_Record) return Coordinate is
   begin
      return R.Height;
   end Get_Height;
   
   ----------------
   -- Set_Height --
   ----------------
   
   procedure Set_Height
     (R : in out Rectangle_Record;
      H : Coordinate) is
   begin
      R.Height := H;
   end Set_Height;
   
   --------------
   -- Set_Rect --
   --------------
   
   procedure Set_Rect
     (R : in out Rectangle_Record;
      X : Coordinate;
      Y : Coordinate;
      W : Coordinate;
      H : Coordinate) is
   begin
      R.Origin.X := X;
      R.Origin.Y := Y;
      R.Width := W;
      R.Height := H;
   end Set_Rect;
   
   ---------
   -- Add --
   ---------
   
   procedure Add
     (R1 : in out Rectangle_Record;
      R2 : Rectangle_Record) is
      
      MinX : Coordinate;
      MinY : Coordinate;
      MaxX : Coordinate;
      MaxY : Coordinate;
   begin
      if R2 /= No_Rectangle_Record then
	 MinX := Min (R1.Origin.X, R2.Origin.X);
	 MinY := Min (R1.Origin.Y, R2.Origin.Y);
	 
	 MaxX := Max
	   (R1.Origin.X + R1.Width, 
	    R2.Origin.X + R2.Width);
	 MaxY := Max
	   (R1.Origin.Y + R1.Height, 
	    R2.Origin.Y + R2.Height);
      
	 R1.Origin.X      := MinX;
	 R1.Origin.Y      := MinY;
	 R1.Width  := MaxX - MinX;
	 R1.Height := MaxY - MinY;
      end if;
   end Add;
   
   ---------
   -- Add --
   ---------
   
   procedure Add
     (R1    : in out Rectangle_Record;
      Point : Point_Record) is
      
      MinX : Coordinate;
      MinY : Coordinate;
      MaxX : Coordinate;
      MaxY : Coordinate;
      R2   : Rectangle_Record := Rectangle_Record'(Point, 0.0, 0.0);
   begin
      if Point /= No_Point_Record then
	 MinX := Min (R1.Origin.X, R2.Origin.X);
	 MinY := Min (R1.Origin.Y, R2.Origin.Y);
	 
	 MaxX := Max
	   (R1.Origin.X + R1.Width, 
	    R2.Origin.X + R2.Width);
	 MaxY := Max
	   (R1.Origin.Y + R1.Height, 
	    R2.Origin.Y + R2.Height);
	 
	 R1.Origin.X      := MinX;
	 R1.Origin.Y      := MinY;
	 R1.Width  := MaxX - MinX;
	 R1.Height := MaxY - MinY;
      end if;
   end Add;
   
   -------------------
   -- Get_Center_X ---
   -------------------
   
   function Get_Center_X (R : Rectangle_Record) return Coordinate is
   begin
      return Get_X (R.Origin) + Get_Width (R) / 2.0;
   end Get_Center_X;

   -------------------
   -- Get_Center_Y ---
   -------------------
   
   function Get_Center_Y (R : Rectangle_Record) return Coordinate is
   begin
      return Get_Y (R.Origin) + Get_Height (R) / 2.0;
   end Get_Center_Y;
      
   ----------
   -- Grow --
   ----------
   
   procedure Grow
     (R      : in out Rectangle_Record;
      Amount : Coordinate) 
   is
   begin
      R.Origin.X := R.Origin.X - Amount;
      R.Origin.Y := R.Origin.Y - Amount;
      R.Width  := R.Width  + 2.0 * Amount;
      R.Height := R.Height + 2.0 * Amount;
   end Grow;
   
   ---------------
   -- Translate --
   ---------------
   
   procedure Translate
     (R  : in out rectangle_Record;
      Dx : Coordinate;
      Dy : Coordinate) is
   begin
      R.Origin.X := R.Origin.X + Dx;
      R.Origin.Y := R.Origin.Y + Dy;
   end Translate;
   
   
   --------------
   -- Contains --
   --------------
   
   function Contains
     (R : Rectangle_Record;
      X : Coordinate;
      Y : Coordinate) return Boolean is
   begin
      return R.Origin.X <= X 
	and (R.Origin.X + R.Width) >= X 
	and R.Origin.Y <= Y 
	and (R.Origin.Y + R.Height) >= y;
   end Contains;
   
   --------------
   -- Contains --
   --------------
   
   function Contains
     (R : Rectangle_Record;
      P : Point_Record) return Boolean is
   begin
      return Contains (R, P.X, P.Y);
   end Contains;
   
   ----------------
   -- Intersects --
   ----------------
   
   function Intersects
     (R1 : Rectangle_Record;
      R2 : rectangle_Record) return Boolean is
      
      X1 : Float := Get_X      (R1);
      Y1 : Float := Get_Y      (R1);
      W1 : Float := Get_Width  (R1);
      H1 : Float := Get_Height (R1);
      X2 : Float := Get_X      (R2);
      Y2 : Float := Get_Y      (R2);
      W2 : Float := Get_Width  (R2);
      H2 : Float := Get_Height (R2);
   begin
      return not 
	(
	 ((X1 + W1) < X2) 
	   or ((X2 + W2) < X1) 
	   or ((Y1 + H1) < Y2)
	   or ((Y2 + H2) < Y1)
	);
   end Intersects;
   
   --------------------
   -- Intersect_Line --
   --------------------
   
   function Intersect_Line
     (R  : Rectangle_Record;
      P1 : Point_Record;
      P2 : Point_Record) return Point_Record is
      
      Result : Point_Record := No_Point_Record;
   begin
      Result := Artics.Geometry.Points.Intersection
        (R.Origin.X, 
         R.Origin.Y, 
         R.Origin.X + R.Width, 
         R.Origin.Y, 
         P1.x, 
         P1.y, 
         P2.x, 
         P2.Y);
      
      if Result = No_Point_Record then
	 Result := Artics.Geometry.Points.Intersection
	   (R.Origin.X + R.Width, 
	    R.Origin.Y,
	    R.Origin.X + R.Width,
	    R.Origin.Y + R.Height, 
	    P1.x, 
	    P1.y, 
	    P2.x, 
	    P2.Y);
      end if;

      if Result = No_Point_Record then
	 Result := Artics.Geometry.Points.Intersection
           (R.Origin.X + R.Width, 
            R.Origin.Y + R.Height,
            R.Origin.X,
            R.Origin.Y + R.Height, 
            P1.x, 
            P1.y, 
            P2.x, 
            P2.Y);
      end if;

      if Result = No_Point_Record then
	 Result := Artics.Geometry.Points.Intersection
	   (R.Origin.X,
	    R.Origin.Y,
	    R.Origin.X,
	    R.Origin.Y + R.Height, 
	    P1.x, 
	    P1.y, 
	    P2.x, 
	    P2.Y);
      end if;
      return Result;
   end Intersect_Line;
   
   --------------------
   -- Intersect_Line --
   --------------------
   
   function Intersect_Line
     (R  : Rectangle_Record;
      P1 : Point_Record;
      P2 : Point_Record) return Boolean is
   begin
      return Intersect_Line (R, P1, P2) /= No_Point_Record;
   end Intersect_Line;
   
   -------------------
   -- Get_Rectangle --
   -------------------
   
   function Get_Rectangle (R : Rectangle_Record) return Rectangle_Record is
   begin
      return R;
   end Get_Rectangle;
   
   ------------
   -- Equals --
   ------------
   
   function Equals
     (R1 : Rectangle_Record;
      R2 : Rectangle_Record) return Boolean is
   begin
      return R1 = R2;
   end Equals;
   
   
   -----------
   -- Clone --
   -----------
   
   function Clone (R : Rectangle_Record) return Rectangle_Record is
   begin
      return R;
   end Clone;
   
   ---------------
   -- To_String --
   ---------------
   
   function To_String (R : Rectangle_Record) return String is
   begin
      return 
	"Rectangle [x=" & R.Origin.X'Img & ",y=" & R.Origin.Y'Img & 
	",w=" & R.Width'Img & ",h=" & R.Height'Img & "]";
   end To_String;
   
end Artics.Geometry.Rectangles;
