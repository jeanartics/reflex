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
-- Reflex is originally developed  by the Artics team at Grenoble.          --
--                                                                          --
------------------------------------------------------------------------------

with Artics.Graph.Cells_Geometry; use Artics.Graph.Cells_Geometry;
with Artics.Geometry.Rectangles;  use Artics.Geometry.Rectangles;
with Artics.Geometry.Points;      use Artics.Geometry.Points;
with Artics.Graph.Cells;          use Artics.Graph.Cells;
with Artics.Geometry;             use Artics.Geometry;
with Artics.Types;                use Artics.Types;

with Reflex.Fbd_Builders; use Reflex.Fbd_Builders;

package Reflex.Fbd_Placements is
   
   X_Distance_Min_Block : Coordinate := 1.0;
   Y_Distance_Min_Block : Coordinate := 1.0;
   X_Distance_Near_Pin  : Coordinate := 1.0;
   Delta_X_Cnx          : Coordinate := 1.0;
   Delta_Y_Cnx          : Coordinate := 1.0;

   --  Three following procedures are used for relative placement.
   
   procedure Place_Vertex_In
     (This      : access Fbd_Builder_Record;
      Vertex_In : access Cell_Record'Class);
   --  With Node Vertex for Vertex_in, Place Node vertex but not his formals 
   --  vertices (Node vertex can be an operand or a procedure call).
   
   procedure Place_Formals_Vertices
     (This      : access Fbd_Builder_Record;
      Vertex_In : access Cell_Record'Class);
   --  With Node Vertex for Vertex_in, Place formals vertices with respect to 
   --  Node vertex (Node vertex could be an operand or a procedure call).
   
   procedure Place_Vertices
     (This      : access Fbd_Builder_Record;
      Vertex_In : access Cell_Record'Class);
   --  With Node vertex for vertex_In, this procedure will place node vertex
   --  and all his formals (Node vertex can be an operand or a procedure call).
   
   -- Following procedures and functions are used for absolute placement.
   
   procedure Place_Graph
     (This : access Fbd_Builder_Record);
   --  Call Make_Layer_Placement with graph defalut parent. Build_connex_graph
   --  is currently not used.
   
   procedure Place_Cell
     (Cell : access Cell_Record;
      Xc   : Float;
      Yc   : Float);
   
   procedure Place_Cell_On_Y
     (Cell : access Cell_Record;
      Yc   : Float);
   procedure Place_Cell_On_X
     (Cell : access Cell_Record;
      Xc   : Float);
   
   procedure Place_In_Out_Near (Root : access Cell_Record'Class);
   procedure Place_Variables_Near (Root : access Cell_Record'Class);
      
   procedure Make_Placement (Root : access Cell_Record'Class);
   
   procedure Make_Layer_Placement (Root : access Cell_Record'Class);
   
   procedure Route_Edges (Root : access Cell_Record'Class);
   
   procedure Edge_Routing
     (Root : access Cell_Record'Class;
      Edge : access Cell_Record'Class);
   
   procedure Workaround 
     (Root : access Cell_Record'Class;
      Edge : access Cell_Record'Class);
   
   function Has_Collision
     (Src       : access Cell_Record'Class;
      Trg       : access Cell_Record'Class;
      Src_Point : Point_Record;
      Trg_Point : Point_Record;
      Cells     : Cells_Lists.List) return Rectangle_Record;
   
end Reflex.Fbd_Placements;
