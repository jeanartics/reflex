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
with Artics.Graph.Cells_Geometry; use Artics.Graph.Cells_Geometry;

package Artics.Graph.Models_Changes.Geometries is
   
   type Geometry_Change_Record is new Model_Change_Record with private;
   type Geometry_Change_Ptr is access all Geometry_Change_Record;
   type Geometry_Change_Class_Ptr is access all Geometry_Change_Record'Class;

   No_Geometry_Change_Record : constant Geometry_Change_Record;
   
   function New_Geometry_Change return Geometry_Change_Ptr;
   
   function New_Geometry_Change
     (Model    : access Model_Interface'Class;
      Cell     : access Cell_Record'Class;
      Geometry : Cell_Geometry_Ptr) return Geometry_Change_Ptr;
   
   function Get_Cell
     (C : access  Geometry_Change_Record) return access Cell_Record'Class;
   procedure Set_Cell
     (C     : access Geometry_Change_Record;
      Value : access Cell_Record'Class);
		   
   function Get_Geometry
     (C : access Geometry_Change_Record) return Cell_Geometry_Ptr;

   procedure Set_Geometry
     (C     : access Geometry_Change_Record;
      Value : Cell_Geometry_Ptr);
   
   function Get_Previous
     (C : access Geometry_Change_Record) return Cell_Geometry_Ptr;
   procedure Set_Previous
     (C     : access Geometry_Change_Record;
      Value : Cell_Geometry_Ptr);
   -- @return the previous
   
   procedure Execute (C : access Geometry_Change_Record);
   -- Changes the root of the model.
   
private

   type Geometry_Change_Record is new Model_Change_Record with record
      Cell     : access Cell_Record'Class;
      Geometry : Cell_Geometry_Ptr;
      Previous : Cell_Geometry_Ptr;
   end record;
   
   No_Geometry_Change_Record : constant Geometry_Change_Record := 
     Geometry_Change_Record'
     (No_Model_Change_Record with
	Cell    => null,
      Geometry  => null,
      Previous  => null);
   
end Artics.Graph.Models_Changes.Geometries;
