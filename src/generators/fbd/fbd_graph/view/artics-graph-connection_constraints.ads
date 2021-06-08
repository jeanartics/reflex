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

with Artics.Objects; use Artics.Objects;

with Artics.Geometry.Points; use Artics.Geometry.Points;

-- Defines an object that contains the constraints about how to connect one
-- side of an edge to its terminal.

package Artics.Graph.Connection_Constraints is
   
   type Connection_Constraint_Record is new Object_Record with private;
   type Connection_Constraint_Ptr is access all Connection_Constraint_Record;
   
   No_Connection_Constraint_Record : constant Connection_Constraint_Record;
   
   function New_Connection_Constraint return Connection_Constraint_Ptr;
   -- Constructs an empty connection constraint.

   function New_Connection_Constraint
     (Point : Point_Record) return Connection_Constraint_Ptr;
   -- Constructs a connection constraint for the given point.

   function New_Connection_Constraint
     (Point     : Point_Record;
      Perimeter : Boolean) return Connection_Constraint_Ptr;
   -- Constructs a new connection constraint for the given point and boolean
   -- arguments.
   -- @param point Optional mxPoint that specifies the fixed location of the
   -- point in relative coordinates. Default is null.
   -- @param perimeter Optional boolean that specifies if the fixed point
   -- should be projected onto the perimeter of the terminal. Default is true.
   
   function Point (C : access Connection_Constraint_Record) return Point_Record;
   procedure Set_Point
     (C    : access Connection_Constraint_Record;
     Point : Point_Record);
   -- Point that specifies the fixed location of the connection point.
      
   function Perimeter (C : access Connection_Constraint_Record) return Boolean;
   procedure Set_Perimeter
     (C         : access Connection_Constraint_Record;
      Perimeter : Boolean);
   -- Boolean that specifies if the point should be projected onto the
   -- perimeter of the terminal.
   
private
   
   type Connection_Constraint_Record is new Object_Record with record
      Point : Point_Record;
      -- Point that specifies the fixed location of the connection point.
      
      Perimeter : Boolean;
      -- Boolean that specifies if the point should be projected onto the
      -- perimeter of the terminal.
   end record;
   
   No_Connection_Constraint_Record : constant Connection_Constraint_Record :=
     Connection_Constraint_Record'
     (No_Object_Record with
	Point     => No_Point_Record,
      Perimeter   => False);
   
end Artics.Graph.Connection_Constraints;
	  
	  
