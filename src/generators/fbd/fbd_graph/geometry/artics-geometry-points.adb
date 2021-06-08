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

package body Artics.Geometry.Points is
   
   ----------
   -- Free --
   ----------
   
   procedure Free  (Data : in out Point_Record) is
   begin
      null;
   end Free;
   
   ---------------
   -- New_Point --
   ---------------
   
   function New_Point (X, Y : Coordinate) return Point_Record is
   begin
      return Point_Record'(X, Y);
   end New_Point;
   
   -----------
   -- Get_X --
   -----------
   
   function Get_X (P : Point_Record) return Coordinate is
   begin
      return P.X;
   end Get_X;
   
   -----------
   -- Set_X --
   -----------
   
   procedure Set_X
     (P : in out Point_Record;
      X : Coordinate) is
   begin
      P.X := X;
   end Set_X;
   
   -----------
   -- Get_Y --
   -----------
   
   function Get_Y (P : Point_Record) return Coordinate is
   begin
      return P.Y;
   end Get_Y;
    
   -----------
   -- Set_Y --
   -----------
   
   procedure Set_Y
     (P : in out Point_Record;
      Y : Coordinate) is
   begin
      P.Y := Y;
   end Set_Y;
   
   ---------------------
   -- Translate_Point --
   ---------------------
   
   function Translate_Point
     (Pt : Point_Record;
      Dx : Coordinate;
      Dy : Coordinate) return Point_Record is
   begin
      return Point_Record'(Get_X (Pt) + Dx, Get_Y (Pt) + Dy);
   end Translate_Point;
   
   ---------------------
   -- Translate_Point --
   ---------------------
   
   procedure Translate_Point 
     (Pt : in out Point_Record;
      Dx : Coordinate;
      Dy : Coordinate) is
   begin
      Pt := Translate_Point (Pt, Dx, Dy);
   end Translate_Point;
   
   ---------------
   -- To_String --
   ---------------
   
   function To_String (P : Point_Record) return String is
   begin
      return "Point_Record => [x='" & P.X'Img & ", y=" & P.Y'Img & "]";
   end To_String;
   
   -----------------------------
   -- Forward_Cursor_Point_At --
   -----------------------------
   
   function Forward_Cursor_Point_At 
     (Pts   : Point_Lists.List;
      Index : Integer) return Point_Lists.Cursor is
      
      Cur : Point_Lists.Cursor;
   begin
      if Index < 1 
	or else Point_Lists.Is_Empty (Pts) 
	or else Integer (Point_Lists.Length (Pts)) < Index 
      then
	 return Point_Lists.No_Element;
	 
	 -- Here we know that index is a valid position in the list, no need to
	 -- test Has_Element, as Index is <= length of the list
	 
      else
	 Cur := Point_Lists.First (Pts);
	 for I in 2..Index loop
	    Point_Lists.Next (Cur);
	 end loop;
      end if;
      
      return Cur;
   end Forward_Cursor_Point_At;
   
   ------------------
   -- Get_Point_At --
   ------------------
   
   function Get_Point_At 
     (Pts   : Point_Lists.List;
      Index : Integer) return Point_Record is
      
      Cur : Point_Lists.Cursor;
   begin
      Cur := Forward_Cursor_Point_At (Pts, Index);
      
      if Cur /= Point_Lists.No_Element then
	 return Point_Lists.Element (Cur);
      else
	 return No_Point_Record;
      end if;
   end Get_Point_At;
   
   ------------------
   -- Set_Point_At --
   ------------------
   
   procedure Set_Point_At
     (Pts   : in out Point_Lists.List;
      Index : Integer;
      Point : Point_Record) 
   is 
      Cur : Point_Lists.Cursor;
   begin
      Cur := Forward_Cursor_Point_At (Pts, Index);
      
      if Cur = Point_Lists.No_Element then
	 Point_Lists.Append (Pts, Point);
      else
	 Point_Lists.Replace_Element (Pts, Cur, Point);
      end if;
   end Set_Point_At;
   
   ---------------------
   -- Remove_Point_At --
   ---------------------
   
   procedure Remove_Point_At
     (Pts   : in out Point_Lists.List;
      Index : Integer) is
      
      Cur : Point_Lists.Cursor;
   begin
      Cur := Forward_Cursor_Point_At (Pts, Index);
      
      if Cur = Point_Lists.No_Element then
	 return; -----Point_Lists.Append (Pts, Point);
      else
	 Point_Lists.Delete (Pts, Cur);
      end if;
   end Remove_Point_At;
   
   ---------------------
   -- Get_First_Point --
   ---------------------
   
   function Get_First_Point (Pts : Point_Lists.List) return Point_Record is
   begin
      if Point_Lists.Is_Empty (Pts) then
	 return No_Point_Record;
      else
	 return Point_Lists.First_Element (Pts);
      end if;
   end Get_First_Point;
   
   ----------------------
   -- Get_Second_Point --
   ----------------------
   
   function Get_Second_Point (Pts : Point_Lists.List) return Point_Record is
      
      use Point_Lists;

      Cur : Point_Lists.Cursor;
   begin
      Cur := Forward_Cursor_Point_At (Pts, 2);
      
      if Cur /= Point_Lists.No_Element then
	 return Point_Lists.Element (Cur);
      else
	 return No_Point_Record;
      end if;
   end Get_Second_Point;
   
   --------------------
   -- Get_Last_Point --
   --------------------
   
   function Get_Last_Point (Pts : Point_Lists.List) return Point_Record is
   begin
      if Point_Lists.Is_Empty (Pts) then
	 return No_Point_Record;
      else
	 return Point_Lists.Last_Element (Pts);
      end if;
   end Get_Last_Point;
   
   ------------------------------
   -- Get_Last_Minus_One_Point --
   ------------------------------
   
   function Get_Last_Minus_One_Point
     (Pts : Point_Lists.List) return Point_Record is
      
      use Point_Lists;
      
      Cur : Point_Lists.Cursor;
   begin
      Cur := Point_Lists.Last (Pts);
      Cur := Point_Lists.Previous (Cur);
      if Cur /= Point_Lists.No_Element then
	 return Point_Lists.Element (Cur);
      else
	 return No_Point_Record;
      end if;
   end Get_Last_Minus_One_Point;
   
   ----------------
   -- Dump_Point --
   ----------------
   
   function Dump_Point (P : Point_Record) return String is
   begin
      return "(x=" & P.X'Img & ",Y=" & P.Y'Img &")";
   end Dump_Point;
   
   ------------------
   -- Intersection --
   ------------------
   
   function Intersection
     (X0 : Coordinate;
      Y0 : Coordinate;
      X1 : Coordinate;
      Y1 : Coordinate;
      X2 : Coordinate;
      Y2 : Coordinate;
      X3 : Coordinate;
      Y3 : Coordinate) return Point_Record is
      
      Denom  : Coordinate;
      Nume_A : Coordinate;
      Nume_B : Coordinate;
      Ua     : Coordinate;
      Ub     : Coordinate;
      Intersection_X : Coordinate;
      Intersection_Y : Coordinate;
      
    begin
      Denom  := ((y3 - y2) * (x1 - x0)) - ((x3 - x2) * (y1 - y0));
      Nume_A := ((x3 - x2) * (y0 - y2)) - ((y3 - y2) * (x0 - x2));
      Nume_B := ((x1 - x0) * (y0 - y2)) - ((y1 - y0) * (x0 - x2));

      Ua := Nume_A / Denom;
      Ub := Nume_B / Denom;

      if (Ua >= 0.0 and Ua <= 1.0) and (Ub >= 0.0 and Ub <= 1.0) then
	 
	 -- Get the intersection point
	 
	 Intersection_X := X0 + Ua * (X1 - X0);
	 Intersection_Y := Y0 + Ua * (Y1 - Y0);

	 return Point_Record'(X => Intersection_X, Y => Intersection_Y);
      end if;

      return No_Point_Record;
   end Intersection;
   
   ------------------------
   -- Lines_Intersection --
   ------------------------
   
   function Lines_Intersection
     (P0 : Point_Record;
      P1 : Point_Record;
      P2 : Point_Record;
      P3 : Point_Record) return Point_Record is
   begin
      return Intersection
	(P0.X, P0.Y,
	 P1.X, P1.Y,
	 P2.X, P2.Y,
	 P3.X, P3.Y);
   end Lines_Intersection;
   
end Artics.Geometry.Points;
