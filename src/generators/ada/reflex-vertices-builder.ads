------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
--                                                                          --
-- Reflex is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public Licensea as published  by the Free Soft- --
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

with Types; use Types;
with Namet; use Namet;
with Table;

with Reflex.Types; use Reflex.Types;

package Reflex.Vertices.Builder is
   
   function Make_Reactive_Graph (Node : in Node_Id) return Vertex_Id;
   
   function Make_Graph_Vertex 
     (Node : Node_Id;
      L    : List_Id) return Vertex_Id;
   
   function Make_Exit_Vertex (Node : Node_Id) return Vertex_Id;
   
   function Make_If_Vertex (Node : Node_Id) return Vertex_Id;
   
   function Make_Loop_Vertex (Node : Node_Id) return Vertex_Id;
   function Make_Case_Vertex (Node : Node_Id) return Vertex_Id;
   
   function Make_Pause_Vertex (Node : Node_Id) return Vertex_Id;
   
   function Make_Wait_Vertex (Node : Node_Id) return Vertex_Id;
   
   function Make_Fork_Vertex (Node : Node_Id) return Vertex_Id;
   
   function Make_Select_Vertex (Node : Node_Id) return Vertex_Id;
   
   function Make_Abort_Vertex (Node : Node_Id) return Vertex_Id;
   
   function Make_Begin_Vertex (Node : Node_Id) return Vertex_Id;
   
   function Make_End_Vertex (Node : Node_Id) return Vertex_Id;

   function If_Has_Waiting_Statement (N : Node_Id) return Boolean;

   function Loop_Has_Waiting_Statement (N : Node_Id) return Boolean;

   function Case_Has_Waiting_Statement (N : Node_Id) return Boolean;

   function Has_Waiting_Statement (Stmts : List_Id) return Boolean;
   
end Reflex.Vertices.Builder;
