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

with Artics.Lists_Helpers; 

-- Implements a 2-dimensional point with double precision coordinates.

package Artics.Geometry.Points is

   type Point_Record is record
      X : Coordinate;
      Y : Coordinate;
   end record;

   procedure Free (Data : in out Point_Record);
   
   function Get_X (P : Point_Record) return Coordinate;
   procedure Set_X
     (P : in out Point_Record;
      X : Coordinate);
   
   function Get_Y (P : Point_Record) return Coordinate;
   procedure Set_Y
     (P : in out Point_Record;
      Y : Coordinate);
   
   function New_Point (X, Y : Coordinate) return Point_Record;
   
   function Translate_Point
     (Pt : Point_Record;
      Dx : Coordinate;
      Dy : Coordinate) return Point_Record;
   
   procedure Translate_Point 
     (Pt : in out Point_Record;
      Dx : Coordinate;
      Dy : Coordinate);
   
   function To_String (P : Point_Record) return String;

   package Point_Lists is new Ada.Containers.Doubly_Linked_Lists (Point_Record);
   use Point_Lists;

   No_Point_Record : constant Point_Record := Point_Record'
     (X => Coordinate'Last, 
      Y => Coordinate'Last);
   
   package Point_List_Helpers is new Artics.Lists_Helpers
     (Point_Record, No_Point_Record, Point_Lists);
   use Point_List_Helpers;
   
   Zero_Point_Record : constant Point_Record := Point_Record'
     (X => 0.0,
      Y => 0.0);
   
   function Forward_Cursor_Point_At 
     (Pts   : Point_Lists.List;
      Index : Integer) return Point_Lists.Cursor;
   -- Put the cursor of Pts at the Index-nth position
   
   function Get_Point_At 
     (Pts   : Point_Lists.List;
      Index : Integer) return Point_Record;
   -- Returns the point at the given index.
   -- @return the mxPoint at the given index
   
   procedure Set_Point_At
     (Pts   : in out Point_Lists.List;
      Index : Integer;
      Point : Point_Record);
   -- Returns the point at the given index.
   -- @return the mxPoint at the given index
   
   procedure Remove_Point_At
     (Pts   : in out Point_Lists.List;
      Index : Integer);
   -- Remove the point at position Integer
   
   function Get_First_Point (Pts : Point_Lists.List) return Point_Record;
   -- Return the first pont of the points lists.
   
   function Get_Second_Point (Pts : Point_Lists.List) return Point_Record;
   -- Return the first pont of the points lists.
   
   function Get_Last_Point (Pts : Point_Lists.List) return Point_Record;
   -- Return the last pont of the points lists.
   
   function Get_Last_Minus_One_Point
     (Pts : Point_Lists.List) return Point_Record;
   -- Return the last pont of the points lists.
   
   function Dump_Point (P : Point_Record) return String;
   
   function Intersection
     (X0 : Coordinate;
      Y0 : Coordinate;
      X1 : Coordinate;
      Y1 : Coordinate;
      X2 : Coordinate;
      Y2 : Coordinate;
      X3 : Coordinate;
      Y3 : Coordinate) return Point_Record;
   -- Returns the intersection of two lines as a Point.
   --    x0 : X-coordinate of the first line's startpoint.
   --    y0 : Y-coordinate of the first line's startpoint.
   --    x1 : X-coordinate of the first line's endpoint.
   --    y1 : Y-coordinate of the first line's endpoint.
   --    x2 : X-coordinate of the second line's startpoint.
   --    y2 : Y-coordinate of the second line's startpoint.
   --    x3 : X-coordinate of the second line's endpoint.
   --    y3 : Y-coordinate of the second line's endpoint.
   
   function Lines_Intersection
     (P0 : Point_Record;
      P1 : Point_Record;
      P2 : Point_Record;
      P3 : Point_Record) return Point_Record;
   -- Returns the intersection of two lines as a Point.
   --    P0 : is the start point of the first line 
   --    P1 : is the end point of the first line 
   --    P2 : is the start point of the second line 
   --    P3 : is the end point of the second line 
   
end Artics.Geometry.Points;
