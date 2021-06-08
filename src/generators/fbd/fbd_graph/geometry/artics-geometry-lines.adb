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

pragma Style_Checks (Off);

with Artics.Generic_Lists;
with Artics.Utils; use Artics.Utils;
with Artics.Geometry.Points; use Artics.Geometry.Points;

-- Implements a line with double precision coordinates.

package body Artics.Geometry.Lines is
   
   --------------
   -- New_Line --
   --------------
   
   function New_Line return Line_Record is
   begin
      return No_Line_Record;
   end New_Line;
   
   --------------
   -- New_Line --
   --------------
   
   function New_Line
     (Xstart, Ystart, Xend, Yend : Coordinate) return Line_Record is
   begin
      return
	Line_Record'(Point_Record'(Xstart, Ystart), 
		     Point_Record'(Xend, Yend));
   end New_Line;
   
   --------------
   -- New_Line --
   --------------
   
   function New_Line (Pstart, Pend : Point_Record) return Line_Record is
   begin
      return Line_Record'(Pstart, Pend);
   end New_Line;
   
   -----------------
   -- Start_Point --
   -----------------
   
   function Start_Point (L : Line_Record) return Point_Record is
   begin
      return L.Start_Point;
   end Start_Point;
   
   ---------------------
   -- Set_Start_Point --
   ---------------------
   
   procedure Set_Start_Point
     (L : in out Line_Record;
      S : Point_Record) is
   begin
      L.Start_Point := S;
   end Set_Start_Point;
   
   ---------------
   -- End_Point --
   ---------------
   
   function End_Point (L : Line_Record) return Point_Record is
   begin
      return L.End_Point;
   end End_Point;
   
   -------------------
   -- Set_End_Point --
   -------------------
   
   procedure Set_End_Point
     (L : in out Line_Record;
      E : Point_Record) is
   begin
      L.End_Point := E;
   end Set_End_Point;
   
   --------------------------------
   -- Point_Line_Distance_Square --
   --------------------------------
   
   function Point_Line_Distance_Square
     (L  : Line_Record;
      Pt : Point_Record) return Coordinate is
   begin
      return 0.0;
	-- new Line2D.Double(getX(), getY(), endPoint.getX(), endPoint
	--			.getY()).ptLineDistSq(pt.getX(), pt.getY());
   end Point_Line_Distance_Square;
   
   -----------------------------------
   -- Point_Segment_Distance_Square --
   -----------------------------------
   
   function Point_Segment_Distance_Square
     (L  : Line_Record;
      Pt : Point_Record) return Coordinate is
   begin
      return 0.0;
	-- new Line2D.Double(getX(), getY(), endPoint.getX(), endPoint
	--			.getY()).ptSegDistSq(pt.getX(), pt.getY());
   end Point_Segment_Distance_Square;
   
   ------------------
   -- Relative_CCW --
   ------------------
   
   function Relative_CCW_Old
     (X1 : Coordinate;
      Y1 : Coordinate;
      X2 : Coordinate;
      Y2 : Coordinate;
      Px : Coordinate;
      Py : Coordinate) return Integer is
      
      CCw : Coordinate;
      X_2 : Coordinate := X2;
      Y_2 : Coordinate := Y2;
      P_x : Coordinate := Px;
      P_y : Coordinate := Py;
   begin
      X_2 := X_2 - X1;
      Y_2 := Y_2 - Y1;
      P_x := P_x - X1;
      P_y := P_y - Y1;
      
      Ccw := P_x * Y_2 - P_y * X_2;
      
      if Ccw = 0.0 then
	 
	 -- The point is colinear, classify based on which side of
	 -- the segment the point falls on.  We can calculate a
	 -- relative value using the projection of px,py onto the
	 -- segment - a negative value indicates the point projects
	 -- outside of the segment in the direction of the particular
	 -- endpoint used as the origin for the projection.
	   
	 Ccw := P_x * X_2 + P_y * Y_2;
	 
	 if Ccw > 0.0 then
	    
	    -- Reverse the projection to be relative to the original x2,y2
	    -- x2 and y2 are simply negated.
	    -- px and py need to have (x2 - x1) or (y2 - y1) subtracted
	    -- from them (based on the original values)
	    -- Since we really want to get a positive answer when the
	    -- point is "beyond (x2,y2)", then we want to calculate
	    -- the inverse anyway - thus we leave x2 & y2 negated.
	    
	    P_x := P_x - x_2;
	    P_y := P_y - Y_2;
	    Ccw := P_x * X_2 + P_Y * Y_2;
	    
	    if Ccw < 0.0 then
	       Ccw := 0.0;
	      
	    end if;
	 end if;
      end if;
      
      if Ccw < 0.0 then
	 return  -1;
      elsif Ccw > 0.0 then
	 return 1;
      else
	 return 0;
      end if;
      
   end Relative_CCW_Old;
    
   ------------------
   -- Relative_CCW --
   ------------------
   
   function Relative_CCW
     (X1 : Coordinate;
      Y1 : Coordinate;
      X2 : Coordinate;
      Y2 : Coordinate;
      Px : Coordinate;
      Py : Coordinate) return Integer is
      
      CCw : Coordinate;
      X_2 : Coordinate := X2;
      Y_2 : Coordinate := Y2;
      P_x : Coordinate := Px;
      P_y : Coordinate := Py;
   begin
      if not Epsilon_Equal (X_2, X1) then
	 X_2 := X_2 - X1;
      else
	 X_2 := 0.0;
      end if;
      
      if not Epsilon_Equal (Y_2, Y1) then
	 Y_2 := Y_2 - Y1;
      else
	 Y_2 := 0.0;
      end if;
      
      if not Epsilon_Equal (P_x, X1) then
	 P_x := P_x - X1;
      else
	 P_X := 0.0;
      end if;
      
      if not Epsilon_Equal (P_Y, Y1) then
	 P_y := P_y - Y1;
      else
	 P_Y := 0.0;
      end if;
      
      Ccw := P_x * Y_2 - P_y * X_2;
      
      if Epsilon_Equal (Ccw, 0.0) then
	 
	 -- The point is colinear, classify based on which side of
	 -- the segment the point falls on.  We can calculate a
	 -- relative value using the projection of px,py onto the
	 -- segment - a negative value indicates the point projects
	 -- outside of the segment in the direction of the particular
	 -- endpoint used as the origin for the projection.
	   
	 Ccw := P_x * X_2 + P_y * Y_2;
	 
	 if Epsilon_Sup (Ccw, 0.0) then
	    
	    -- Reverse the projection to be relative to the original x2,y2
	    -- x2 and y2 are simply negated.
	    -- px and py need to have (x2 - x1) or (y2 - y1) subtracted
	    -- from them (based on the original values)
	    -- Since we really want to get a positive answer when the
	    -- point is "beyond (x2,y2)", then we want to calculate
	    -- the inverse anyway - thus we leave x2 & y2 negated.
	    
	    if not Epsilon_Equal (P_x, X_2) then
	       P_x := P_x - x_2;
	    else
	       P_X := 0.0;
	    end if;
	    
	    if not Epsilon_Equal (P_y, Y_2) then
	       P_y := P_y - Y_2;
	    else
	       P_Y := 0.0;
	    end if;
	    Ccw := P_x * X_2 + P_Y * Y_2;
	    
	    if Epsilon_Inf (Ccw, 0.0) then
	       Ccw := 0.0;
	      
	    end if;
	 end if;
      end if;
      
      if Epsilon_Inf (Ccw, 0.0) then
	 return  -1;
      elsif Epsilon_Sup (Ccw, 0.0) then
	 return 1;
      else
	 return 0;
      end if;
      
   end Relative_CCW;
    
   ------------------
   -- Relative_CCW --
   ------------------
   
   function Relative_CCW
     (L  : Line_Record;
      Px : Coordinate;
      Py : Coordinate) return Integer is
   begin
      return Relative_CCW 
	(L.Start_Point.X,
	 L.Start_Point.Y,
	 L.End_Point.X,
	 L.End_Point.Y,
	 Px,
	 Py);
   end Relative_CCW;
   
   ------------------
   -- Relative_CCW --
   ------------------
   
   function Relative_CCW
     (L : Line_Record;
      P : Point_Record) return Integer is
   begin
      return Relative_CCW 
	(L.Start_Point.X,
	 L.Start_Point.Y,
	 L.End_Point.X,
	 L.End_Point.Y,
	 P.X,
	 P.Y);
   end Relative_CCW;
    
   ---------------------
   -- Intersect_Lines --
   ---------------------
   
   function Intersect_Lines
     (X1 : Coordinate;
      Y1 : Coordinate;
      X2 : Coordinate;
      Y2 : Coordinate;
      X3 : Coordinate;
      Y3 : Coordinate;
      X4 : Coordinate;
      Y4 : Coordinate) return Boolean is
   begin
      return 
	((Relative_CCW (X1, Y1, X2, Y2, X3, Y3) *
	    Relative_CCW (X1, Y1, X2, Y2, X4, Y4)) <= 0)
	and 
	((Relative_CCW (X3, Y3, X4, Y4, X1, Y1) *
	    Relative_CCW (X3, Y3, X4, Y4, X2, Y2)) <= 0);
   end Intersect_Lines;
   
   ---------------------
   -- Intersect_Lines --
   ---------------------
   
   function Intersect_Lines
     (L : Line_Record;
      X1 : Coordinate;
      Y1 : Coordinate;
      X2 : Coordinate;
      Y2 : Coordinate) return Boolean is
   begin
      return Intersect_Lines
	(X1, Y1, 
	 X2, Y2,
	 L.Start_Point.X,
	 L.Start_Point.Y,
	 L.End_Point.X,
	 L.End_Point.Y);
   end Intersect_Lines;
    
   ---------------------
   -- Intersect_Lines --
   ---------------------
   
   function Intersect_Lines
     (L1 : Line_Record;
      L2 : Line_Record) return Boolean is
   begin
      return Intersect_Lines
	(L1.Start_Point.X,
	 L1.Start_Point.Y,
	 L1.End_Point.X,
	 L1.End_Point.Y,
	 L2.Start_Point.X,
	 L2.Start_Point.Y,
	 L2.End_Point.X,
	 L2.End_Point.Y);
   end Intersect_Lines;
   
   -----------------------------------
   -- Point_Segment_Distance_Square --
   -----------------------------------
   
   function Point_Segment_Distance_Square
     (Line_Start : Point_Record;
      Line_End   : Point_Record;
      Point      : Point_Record) return Coordinate is
   begin
      return Point_Segment_Distance_Square
	(Get_X (Line_Start), Get_Y (Line_Start),
	 Get_X (Line_End),   Get_Y (Line_End),
	 Get_X (Point),      Get_Y (Point));
   end Point_Segment_Distance_Square;
   
   function Point_Segment_Distance_Square
     (X1 : Coordinate;
      Y1 : Coordinate;
      X2 : Coordinate;
      Y2 : Coordinate;
      Px : Coordinate;
      Py : Coordinate) return Coordinate is
      
      Dotprod   : Coordinate;
      ProjlenSq : Coordinate;
      LenSq     : Coordinate;
      X_2       : Coordinate := X2;
      Y_2       : Coordinate := Y2;
      P_X       : Coordinate := Px;
      P_Y       : Coordinate := Py;
   begin
      -- Adjust vectors relative to x1,y1 --
      --------------------------------------
      
      -- x2,y2 becomes relative vector from x1,y1 to end of segment
      X_2 := X_2 - X1;
      Y_2 := Y_2 - Y1;
      
      -- px,py becomes relative vector from x1,y1 to test point
      
      P_x := P_x - X1;
      P_y := P_y - y1;
      
      Dotprod := P_x * X_2 + P_y * Y_2;
      
      if Dotprod <= 0.0 then
	 
	 -- px,py is on the side of x1,y1 away from x2,y2
	 -- distance to segment is length of px,py vector
	 -- "length of its (clipped) projection" is now 0.0
	 
	 ProjlenSq := 0.0;
	 
      else 
	 -- switch to backwards vectors relative to x2,y2
	 -- x2,y2 are already the negative of x1,y1=>x2,y2
	 -- to get px,py to be the negative of px,py=>x2,y2
	 -- the dot product of two negated vectors is the same
	 -- as the dot product of the two normal vectors
	 
	 P_x := X_2 - P_x;
	 P_y := Y_2 - P_y;
	 Dotprod := P_x * X_2 + P_y * Y_2;
	 
	 if Dotprod <= 0.0 then
	    
	    -- px,py is on the side of x2,y2 away from x1,y1
	    -- distance to segment is length of (backwards) px,py vector
	    -- "length of its (clipped) projection" is now 0.0
	    
	    ProjlenSq := 0.0;
	    
	 else 
	    -- px,py is between x1,y1 and x2,y2
	    -- dotprod is the length of the px,py vector
	    -- projected on the x2,y2=>x1,y1 vector times the
	    -- length of the x2,y2=>x1,y1 vector
	    
	    ProjlenSq := Dotprod * Dotprod / (X_2 * X_2 + Y_2 * Y_2);
	    
	 end if;
      end if;
      
      -- Distance to line is now the length of the relative point
      -- vector minus the length of its projection onto the line
      -- (which is zero if the projection falls outside the range
      -- of the line segment).
      
      LenSq := P_x * P_x + P_y * P_y - ProjlenSq;
      
      if LenSq < 0.0 then
	 LenSq := 0.0;
      end if;
      
      return LenSq;
   end Point_Segment_Distance_Square;
   
   ----------------------------
   -- Point_Segment_Distance --
   ----------------------------
   
   function Point_Segment_Distance
     (Line_Start : Point_Record;
      Line_End   : Point_Record;
      Point      : Point_Record) return Coordinate is
   begin
      return Point_Segment_Distance
	(Get_X (Line_Start), Get_Y (Line_Start),
	 Get_X (Line_End),   Get_Y (Line_End),
	 Get_X (Point),      Get_Y (Point));
   end Point_Segment_Distance;
   
   function Point_Segment_Distance
     (X1 : Coordinate;
      Y1 : Coordinate;
      X2 : Coordinate;
      Y2 : Coordinate;
      Px : Coordinate;
      Py : Coordinate) return Coordinate is
   begin
      return Sqrt (Point_Segment_Distance_Square (X1, Y1, X2, Y2, Px, Py));
   end Point_Segment_Distance;
   
   ---------------
   -- To_String --
   ---------------
   
   function To_String (L : Line_Record) return String is
   begin
      return
	"Line => [" & 
	"Start_Point =>(" & L.Start_Point.X'Img & ", " & L.Start_Point.Y'Img &
	"; End_Point =>(" & 
	L.End_Point.X'Img & "," & L.End_Point.Y'Img & ")]";
   end To_String;
   
end Artics.Geometry.Lines;
