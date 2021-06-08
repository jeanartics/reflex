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

package body Artics.Graph.Models_Changes.Geometries is
   
   -------------------------
   -- New_Geometry_Change --
   -------------------------
   
   function New_Geometry_Change return Geometry_Change_Ptr is
   begin
      return new Geometry_Change_Record'(No_Geometry_Change_Record);
   end New_Geometry_Change;
   
   -------------------------
   -- New_Geometry_Change --
   -------------------------
   
   function New_Geometry_Change
     (Model    : access Model_Interface'Class;
      Cell     : access Cell_Record'Class;
      Geometry : Cell_Geometry_Ptr) return Geometry_Change_Ptr is
      
      Geo : Geometry_Change_Ptr := New_Geometry_Change;
   begin
      Geo.Initialize_Change (Model, Geometry_Change);
      Geo.Cell := Cell;
      Geo.Geometry := Geometry;
      
      return Geo;
   end New_Geometry_Change;
   
   --------------
   -- Get_Cell --
   --------------
   
   function Get_Cell
     (C : access  Geometry_Change_Record) return access Cell_Record'Class is
   begin
      return C.Cell;
   end Get_Cell;
   
   --------------
   -- Set_Cell --
   --------------
   
   procedure Set_Cell
     (C     : access Geometry_Change_Record;
      Value : access Cell_Record'Class) is
   begin
      C.Cell := Value;
   end Set_Cell;
   
   ------------------
   -- Get_Geometry --
   ------------------
   
   function Get_Geometry
     (C : access Geometry_Change_Record) return Cell_Geometry_Ptr is
   begin
      return C.Geometry;
   end Get_Geometry;
   
   ------------------
   -- Set_Geometry --
   ------------------
   
   procedure Set_Geometry
     (C     : access Geometry_Change_Record;
      Value : Cell_Geometry_Ptr) is
   begin
      C.Geometry := Value;
   end Set_Geometry;
   
   ------------------
   -- Get_Previous --
   ------------------
   
   function Get_Previous
     (C : access Geometry_Change_Record) return Cell_Geometry_Ptr is
   begin
      return C.Previous;
   end Get_Previous;
   
   ------------------
   -- Set_Previous --
   ------------------
   
   procedure Set_Previous
     (C     : access Geometry_Change_Record;
      Value : Cell_Geometry_Ptr) is
   begin
      C.Previous := Value;
   end Set_Previous;
   
   -------------
   -- Execute --
   -------------
   
   procedure Execute (C : access Geometry_Change_Record) 
   is
      M : access Model_Interface'Class;
   begin
      M := C.Model;
      C.Previous := M.Geometry_For_Cell_Changed (C.Cell, C.Geometry);
   end Execute;
   
end Artics.Graph.Models_Changes.Geometries;
