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
with Artics.Graph.Cells;     use Artics.Graph.Cells;
with Artics.Objects;         use Artics.Objects;
with Artics.Maths;           use Artics.Maths;

with Namet; use Namet;

with Reflex.Fbd_Builders; use Reflex.Fbd_Builders;
with Reflex.Vertex_Value; use Reflex.Vertex_Value;

package Reflex.Fbd_Util is
   
   Formal_In_Width   : Float := 0.0;
   Formal_In_Height  : Float := 0.0;
   Formal_Out_Width  : Float := 0.0;
   Formal_Out_Height : Float := 0.0;
   
   Operator_Width  : Float := 7.0;
   Operator_Height : Float := 6.0;
   
   Identifier_Width  : Float := Max (Formal_In_Width, Formal_Out_Width) + 1.0;
   Identifier_Height : Float := Max (Formal_In_Height, Formal_Out_Height) + 1.0;
      
   procedure Create_Formal_Vertex 
     (This             : access Fbd_Builder_Record;
      Node             : Node_Id;
      Param_Node       : Node_Id;
      Is_In_Param      : Boolean;
      Is_Out_Param     : Boolean;
      Formal_Name      : Name_Id);
   --  Create a formal vertex for a Node vertex. 
   --  Node is current node which is being handle,
   --  Param_node is node which will be dispatch or linked,
   --  Is_In_Param/Is_Out_Param specify mode of param_node,
   --  Formal_name is IN1/IN2/OUT for operands and name for a procedure call.
   
   function Determinate_Vertex_Kind 
     (This : access Fbd_Builder_Record; 
      Node : Node_Id) return Vertex_Kind;
   --  Determinate if formal of an identifier vertex is a formal_in or a 
   --  formal_out vertex.
      
   function Is_Local_Variable_Reference (Expr : Node_Id) return Boolean;

   function Calculate_Proc_Width  (Node : Node_Id) return Float;
   function Calculate_Proc_Height (Node : Node_Id) return Float;

   function Calculate_Enclose_Width  (Node : Node_Id) return Float;
   function Calculate_Enclose_Height (Node : Node_Id) return Float;
      
   procedure Dump_Vertex 
     (This   : access Fbd_Builder_Record;
      Vertex : access Cell_Record'Class);
   
   function Is_Global_Vertex
     (Cell : access Cell_Record'Class) return Boolean;
      
   function Is_Local_Vertex
     (Cell : access Cell_Record'Class) return Boolean;
      
   function Is_Local_Global_Vertex
     (Cell : access Cell_Record'Class) return Boolean;
   
   function Is_In_Vertex
     (Cell : access Cell_Record'Class) return Boolean;
   
   function Is_Out_Vertex
     (Cell : access Cell_Record'Class) return Boolean;

   function Is_Id_Vertex 
     (Cell : access Cell_Record'Class) return Boolean;
     
   function Compute_Absolute_Coordinate 
     (Root : access Cell_Record'Class;
      Cell : access Cell_Record'Class) return Point_Record;
   
   function Edge_Intersection
     (Root : access Cell_Record'Class;
      P1   : Point_Record;
      P2   : Point_Record) return Float;
   
end Reflex.Fbd_Util;
