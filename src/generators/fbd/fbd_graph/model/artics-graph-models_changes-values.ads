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

with Artics.Types; use Artics.Types;
with Artics.Graph.Cells; use Artics.Graph.Cells;
with Artics.Graph.Models_Interfaces; use Artics.Graph.Models_Interfaces;

package Artics.Graph.Models_Changes.Values is
   
   type Value_Change_Record is new Model_Change_Record with private;
   type Value_Change_Ptr is access all Value_Change_Record;
   type Value_Change_Class_Ptr is access all Value_Change_Record'Class;

   No_Value_Change_Record : constant Value_Change_Record;
   
   function New_Value_Change return Value_Change_Ptr;
   
   function New_Value_Change
     (Model  : access Model_Interface'Class;
      Cell   : access Cell_Record'Class;
      Value  : access Object_Record'Class) return Value_Change_Ptr;
   
   function Get_Cell
     (C : access  Value_Change_Record) return access Cell_Record'Class;
   procedure Set_Cell
     (C     : access Value_Change_Record;
      Value : access Cell_Record'Class);
		   
   function Get_Value
     (C : access Value_Change_Record) return access Object_Record'Class;

   procedure Set_Value
     (C     : access Value_Change_Record;
      Value : access Object_Record'Class);
   
   function Get_Previous
     (C : access Value_Change_Record) return access Object_Record'Class;
   procedure Set_Previous
     (C     : access Value_Change_Record;
      Value : access Object_Record'Class);
   -- @return the previous
   
   procedure Execute (C : access Value_Change_Record);
   -- Changes the root of the model.
   
private

   type Value_Change_Record is new Model_Change_Record with record
      Cell     : access Cell_Record'Class;
      Value    : access Object_Record'Class;
--        Previous : access Cell_Record'Class;
      Previous : access Object_Record'Class;
   end record;
   
   No_Value_Change_Record : constant Value_Change_Record := 
     Value_Change_Record'
     (No_Model_Change_Record with
	Cell   => null,
      Value    => null,
      Previous => null);
   
end Artics.Graph.Models_Changes.Values;
