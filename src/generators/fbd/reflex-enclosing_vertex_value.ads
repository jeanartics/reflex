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

with Artics.Objects;     use Artics.Objects;
with Artics.Graph.Cells; use Artics.Graph.Cells;

package Reflex.Enclosing_Vertex_Value is
   
   type Enclosing_Vertex_Value_Record is new Object_Record with private;
   type Enclosing_Vertex_Value_Ptr is access all Enclosing_Vertex_Value_Record;
   type Enclosing_Vertex_Value_Class_Ptr is access all Enclosing_Vertex_Value_Record'Class;
   
   type Cell_Orientation is
     (Horizontal,
      Vertical);
   
   No_Enclosing_Vertex_Value_Record : constant Enclosing_Vertex_Value_Record;
        
   function New_Enclosing_Vertex_Value return Enclosing_Vertex_Value_Ptr;
   
   procedure Free_Enclosing_Vertex_Value (This : in out Enclosing_Vertex_Value_Ptr);
   
   function Get_Node (This : access Enclosing_Vertex_Value_Record) return Node_Id;
   procedure Set_Node (This : access Enclosing_Vertex_Value_Record; Node : Node_Id);

   function Get_Cell_To_Link
     (This : access Enclosing_Vertex_Value_Record) return access Cell_Record;
   procedure Set_Cell_To_Link
     (This : access Enclosing_Vertex_Value_Record;
      Cell : access Cell_Record);

   function Get_Orientation
     (This : access Enclosing_Vertex_Value_Record) 
      return Cell_Orientation;
   procedure Set_Orientation
     (This        : access Enclosing_Vertex_Value_Record;
      Orientation : Cell_Orientation);
   
private
   
   type Enclosing_Vertex_Value_Record is new Object_Record with record
      Node : Node_Id;
      Cell_To_Link : access Cell_Record;
      Orientation  : Cell_Orientation;
   end record;
   
   No_Enclosing_Vertex_Value_Record : constant Enclosing_Vertex_Value_Record := Enclosing_Vertex_Value_Record'
     (No_Object_Record with
      Node         => Empty,
      Cell_To_Link => null,
      Orientation  => Horizontal);

end Reflex.Enclosing_Vertex_Value;
