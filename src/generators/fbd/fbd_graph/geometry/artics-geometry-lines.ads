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

with Artics.Geometry.Points; use Artics.Geometry.Points;

-- Implements a line with double precision coordinates.

package Artics.Geometry.Lines is

   type Line_Record is record
      Start_Point : Point_Record;
      End_Point   : Point_Record;
   end record;
   
   package Lines_Lists is new Ada.Containers.Doubly_Linked_Lists (Line_Record);
   use Lines_Lists;
   
   function New_Line return Line_Record;
   function New_Line
     (Xstart, Ystart, Xend, Yend : Coordinate) return Line_Record;
   function New_Line
     (Pstart, Pend : Point_Record) return Line_Record;
   -- Creates a new line
   
   function Start_Point (L : Line_Record) return Point_Record;
   procedure Set_Start_Point
     (L : in out Line_Record;
      S : Point_Record);
   -- Returns the start point of the line.
   
   function End_Point (L : Line_Record) return Point_Record;
   procedure Set_End_Point
     (L : in out Line_Record;
      E : Point_Record);
   -- Returns the end point of the line.
   
   function Point_Line_Distance_Square
     (L  : Line_Record;
      Pt : Point_Record) return Coordinate;
   -- Returns the square of the shortest distance from a point to this line.
   -- The line is considered extrapolated infinitely in both directions for 
   -- the purposes of the calculation.   
   
   function Point_Segment_Distance_Square
     (L  : Line_Record;
      Pt : Point_Record) return Coordinate;
   -- Returns the square of the shortest distance from a point to this 
   -- line segment.   
   
   
   function Relative_CCW
     (X1 : Coordinate;
      Y1 : Coordinate;
      X2 : Coordinate;
      Y2 : Coordinate;
      Px : Coordinate;
      Py : Coordinate) return Integer;
   -- Returns an indicator of where the specified point (px,py) lies with
   -- respect to the line segment from (x1,y1) to (x2,y2). The return value can
   -- be either 1, -1, or 0 and indicates in which direction the specified line
   -- must pivot around its first end point, (x1,y1), in order to point at the
   -- specified point (px,py).

   -- A return value of 1 indicates that the line segment must turn in the
   -- direction that takes the positive X axis towards the negative Y axis. In
   -- the default coordinate system used by Java 2D, this direction is
   -- counterclockwise.

   -- A return value of -1 indicates that the line segment must turn in the
   -- direction that takes the positive X axis towards the positive Y axis. In
   -- the default coordinate system, this direction is clockwise.

   -- A return value of 0 indicates that the point lies exactly on the line
   -- segment. Note that an indicator value of 0 is rare and not useful for
   -- determining colinearity because of Coordinateing point rounding issues.

   -- If the point is colinear with the line segment, but not between the end
   -- points, then the value will be -1 if the point lies "beyond (x1,y1)" or
   -- 1 if the point lies "beyond (x2,y2)".

   -- Parameters:
   -- x1 the X coordinate of the start point of the specified line segment
   -- y1 the Y coordinate of the start point of the specified line segment
   -- x2 the X coordinate of the end point of the specified line segment
   -- y2 the Y coordinate of the end point of the specified line segment
   -- px the X coordinate of the specified point to be compared with the
   --    specified line segment
   -- py the Y coordinate of the specified point to be compared with the
   --    specified line segment
   -- Returns:
   -- an integer that indicates the position of the third specified coordinates
   -- with respect to the line segment formed by the first two specified 
   -- coordinates.
   
   function Relative_CCW
     (L  : Line_Record;
      Px : Coordinate;
      Py : Coordinate) return Integer;
   -- Returns an indicator of where the specified point (px,py) lies with
   -- respect to this line segment. See the method comments of 
   -- relativeCCW(double,double,double,double,double,double) to interpret the
   -- return value.

   -- Parameters:
   -- px the X coordinate of the specified point to be compared with this Line2D
   -- py the Y coordinate of the specified point to be compared with this Line2D
   -- Returns:
   -- an integer that indicates the position of the specified coordinates with
   -- respect to this Line2D
   -- See also:
   --relativeCCW(double,double,double,double,double,double)
   
   function Relative_CCW
     (L : Line_Record;
      P : Point_Record) return Integer;
   -- Returns an indicator of where the specified Point2D lies with respect to
   -- this line segment. See the method comments of 
   -- relativeCCW(double,double,double,double,double,double) to interpret the
   -- return value.

   -- Parameters:
   -- p the specified Point2D to be compared with this Line2D
   -- Returns:
   -- an integer that indicates the position of the specified Point2D with
   -- respect to this Line2D
   -- See also:
   --   relativeCCW(double,double,double,double,double,double)

   function Intersect_Lines
     (X1 : Coordinate;
      Y1 : Coordinate;
      X2 : Coordinate;
      Y2 : Coordinate;
      X3 : Coordinate;
      Y3 : Coordinate;
      X4 : Coordinate;
      Y4 : Coordinate) return Boolean;
   -- Tests if the line segment from (x1,y1) to (x2,y2) intersects the line
   -- segment from (x3,y3) to (x4,y4).
   -- Parameters:
   -- x1 the X coordinate of the start point of the first specified line segment
   -- y1 the Y coordinate of the start point of the first specified line segment
   -- x2 the X coordinate of the end point of the first specified line segment
   -- y2 the Y coordinate of the end point of the first specified line segment
   -- x3 the X coordinate of the start point of the second specified line segment
   -- y3 the Y coordinate of the start point of the second specified line segment
   -- x4 the X coordinate of the end point of the second specified line segment
   -- y4 the Y coordinate of the end point of the second specified line segment
   -- Returns:
   --  true if the first specified line segment and the second specified line 
   -- segment intersect each other; false otherwise.
   
   function Intersect_Lines
     (L : Line_Record;
      X1 : Coordinate;
      Y1 : Coordinate;
      X2 : Coordinate;
      Y2 : Coordinate) return Boolean;
   -- Tests if the line segment from (x1,y1) to (x2,y2) intersects this line
   -- segment.
   -- Parameters:
    -- x1 the X coordinate of the start point of the specified line segment
    -- y1 the Y coordinate of the start point of the specified line segment
    -- x2 the X coordinate of the end point of the specified line segment
    -- y2 the Y coordinate of the end point of the specified line segment
    -- Returns:
    -- <true> if this line segment and the specified line segment intersect
    -- each other; false otherwise.
   
   function Intersect_Lines
     (L1 : Line_Record;
      L2 : Line_Record) return Boolean;
   -- Tests if the specified line segment intersects this line segment.
   -- Parameters:
   -- l the specified Line2D
   -- Returns:
   -- true if this line segment and the specified line segment intersect each
   -- other; false otherwise.
   
   function Point_Segment_Distance_Square
     (Line_Start : Point_Record;
      Line_End   : Point_Record;
      Point      : Point_Record) return Coordinate;
   function Point_Segment_Distance_Square
     (X1 : Coordinate;
      Y1 : Coordinate;
      X2 : Coordinate;
      Y2 : Coordinate;
      Px : Coordinate;
      Py : Coordinate) return Coordinate;
   -- Returns the square of the distance from a point to a line segment. The
   -- distance measured is the distance between the specified point and the
   -- closest point between the specified end points. If the specified point 
   -- intersects the line segment in between the end points, this method
   -- returns 0.0.
   -- x1 the X coordinate of the start point of the specified line segment
   -- y1 the Y coordinate of the start point of the specified line segment
   -- x2 the X coordinate of the end point of the specified line segment
   -- y2 the Y coordinate of the end point of the specified line segment
   -- px the X coordinate of the specified point being measured against the
   -- specified line segment py the Y coordinate of the specified point being
   -- measured against the specified line segment
   -- Returns:
   -- a double value that is the square of the distance from the specified
   -- point to the specified line segment.
   
   function Point_Segment_Distance
     (Line_Start : Point_Record;
      Line_End   : Point_Record;
      Point      : Point_Record) return Coordinate;
   function Point_Segment_Distance
     (X1 : Coordinate;
      Y1 : Coordinate;
      X2 : Coordinate;
      Y2 : Coordinate;
      Px : Coordinate;
      Py : Coordinate) return Coordinate;
   -- Returns the distance from a point to a line segment. The distance 
   -- measured is the distance between the specified point and the closest
   -- point between the specified end points. If the specified point intersects
   -- the line segment in between the end points, this method returns 0.0.
   -- x1 the X coordinate of the start point of the specified line segment
   -- y1 the Y coordinate of the start point of the specified line segment
   -- x2 the X coordinate of the end point of the specified line segment
   -- y2 the Y coordinate of the end point of the specified line segment
   -- px the X coordinate of the specified point being measured against the
   -- specified line segment
   -- py the Y coordinate of the specified point being measured against the
   -- specified line segment
   -- Returns:
   -- a double value that is the distance from the specified point to the
   -- specified line segment.
   --See also:
   --   ptLineDist(double,double,double,double,double,double)
   
   
   function To_String (L : Line_Record) return String;

   No_Line_Record : constant Line_Record := Line_Record'
     (Start_Point => No_Point_Record,
      End_Point   => No_Point_Record);
   

end Artics.Geometry.Lines;
