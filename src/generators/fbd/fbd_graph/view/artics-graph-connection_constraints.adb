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

package body Artics.Graph.Connection_Constraints is
   
   -------------------------------
   -- New_Connection_Constraint --
   -------------------------------
   
   function New_Connection_Constraint return Connection_Constraint_Ptr is
      C : Connection_Constraint_Ptr := 
	new Connection_Constraint_Record'(No_Connection_Constraint_Record);
   begin
      return C;
   end New_Connection_Constraint;
   
   -------------------------------
   -- New_Connection_Constraint --
   -------------------------------
   
   function New_Connection_Constraint
     (Point : Point_Record) Return Connection_Constraint_Ptr is
      C : Connection_Constraint_Ptr := New_Connection_Constraint;
   begin
      C.Point := Point;
      return C;
   end New_Connection_Constraint;
   
   -------------------------------
   -- New_Connection_Constraint --
   -------------------------------
   
   function New_Connection_Constraint
     (Point     : Point_Record;
      Perimeter : Boolean) return Connection_Constraint_Ptr is
      C : Connection_Constraint_Ptr := New_Connection_Constraint;
   begin
      C.Point     := Point;
      C.Perimeter := Perimeter;
      return C;
   end New_Connection_Constraint;
   
   -----------
   -- Point --
   -----------
   
   function Point 
     (C : access Connection_Constraint_Record) return Point_Record is
   begin
      return C.Point;
   end Point;
   
   ---------------
   -- Set_Point --
   ---------------
   
   procedure Set_Point
     (C     : access Connection_Constraint_Record;
      Point : Point_Record) is
   begin
      C.Point := Point;
   end Set_Point;
   
   ---------------
   -- Perimeter --
   ---------------
   
   function Perimeter 
     (C : access Connection_Constraint_Record) return Boolean is
   begin
      return C.Perimeter;
   end Perimeter;
   
   -------------------
   -- Set_Perimeter --
   -------------------
   
   procedure Set_Perimeter
     (C         : access Connection_Constraint_Record;
      Perimeter : Boolean) is
   begin
      C.Perimeter := Perimeter;
   end Set_Perimeter;
   
end Artics.Graph.Connection_Constraints;
	  
	  
