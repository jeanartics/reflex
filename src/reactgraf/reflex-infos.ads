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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;

with Types; use Types;
with Namet; use Namet;

with Artics.Elmt_Nlists; use Artics.Elmt_Nlists;

package Rx.Graf.Nodes is
   
   type Vnode_Id is new Integer;
   type VList_Id is new Integer;
   
   type Vertex_Kind is
     (V_No_Vetex,
      V_Graph_Vertex,
      
      V_Exit_Vertex,
      V_If_Vertex,
      V_Loop_Vertex,
      V_Case_Vertex,
      
      V_Pause_Vertex,
      V_Wait_Vertex,
      V_Fork_Vertex,
      V_Select_Vertex,
      V_Abort_Vertex,
      
      V_Last_vertex);
   
   subtype Transient_Vertex is Vertex_Kind range
     V_Exit_Vertex .. V_Case_Vertex;
   
   
   type Vertex_Node_Record is tagged private;
   type Vertex_Node_Ptr is access all Vertex_Node_Record;
   type Vertex_Node_Class_Ptr is access all Vertex_Node_Record'Class;

   No_Vertex_Node_Record : constant Vertex_Node_Record;
   
   type Graph_Vertex_Record is new Vertex_Record with private;
   
   type Transient_Vertex_Record is new Vertex_Node_Record with private;
   
   type Exit_Vertex_Record is new Transient_Node_Record with private;
   
   type If_Vertex_Record is new Transient_Node_Record with private;
   
   type Loop_Vertex_Record is new Transient_Node_Record with private;
   
   type Case_Vertex_Record is new Transient_Node_Record with private;
   
   type Waiting_Vertex_Record is new Vertex_Node_Record with private;
   
   type Pause_Vertex_Record is new Waiting_Node_Record with private;
   
   type Wait_Vertex_Record is new Waiting_Node_Record with private;
   
   type Select_Vertex_Record is new Waiting_Node_Record with private;
   
   type Fork_Vertex_Record is new Waiting_Node_Record with private;
   
   type Abort_Vertex_Record is new Waiting_Node_Record with private;
   
   
private
   
   type Vertex_Node_Record is tagged record
      Associated_Node : Node_Id;
      Parent : Vertex_Id;
      Enter_Code : List_Id;
      Exit_Code  : List_Id;
      
      Begin_Vertex : Vertex_Id;
      End_Vertex : Vertex_Id;
   end record;
   
   type Graph_Vertex_Record is new Vertex_Record with record
      Vertices : Vertices_Lists.List_Id;
   end record;
   
   type Transient_Vertex_Record is new Vertex_Node_Record with null record;
   
   type Exit_Vertex_Record is new Transient_Node_Record with record
      Condition : Node_Id;
   end record;
   
   type If_Vertex_Record is new Transient_Node_Record with record
      Condition : Node_Id;
      Then_Graph : Vertices_List.List;
      Alternatives : Vertices_Lists.List; -- Graph vertex
      Else_Graph : Vertices_List.List;
   end record;
   
   type Loop_Vertex_Record is new Transient_Node_Record with record
      Body_Graph : Vertices_Lists.List;
   end record;
   
   type Case_Vertex_Record is new Transient_Node_Record with record
      Alternatives : Vertices_Lists.List; -- Graph vertex
   end record;
   
   type Waiting_Vertex_Record is new Vertex_Node_Record with null record;
   
   type Pause_Vertex_Record is new Waiting_Node_Record with null record;
   
   type Wait_Vertex_Record is new Waiting_Node_Record with record
      Condition : Node_Id;
   end record;
   
   type Select_Vertex_Record is new Waiting_Node_Record with record
      Condition : Node_Id;
      Alternatives : Vertices_Lists.List;
   end record;
   
   type Fork_Vertex_Record is new Waiting_Node_Record with record
      Condition : Node_Id;
      Alternatives : Vertices_Lists.List;
   end record;
   
   type Abort_Vertex_Record is new Waiting_Node_Record with record
      Condition : Node_Id;
      Body_Graph : Vertex_Id; -- Graph
   end record;
   
   type Vertex_Node_Entry is record
      Vkind         : Vertex_Kind;
      Vertex_Access : Vertex_Class_Ptr;
   end record;
   
   package Vertex_Nodes is new Artics.Elmt_Nlists 
     (Elmt_T  => Vertex_Id,
      Elmt_Id  => Vertex_Entry_Record,
      List_Id  => Vertex_List_Id,
      Elmts_Initial => 50_000,
      Elmts_Increment  => 100,
      Lists_Initial    => 10_000,
      Lists_Increment  => 100);
   use Vertex_Nodes;
   
end Rx.Graf.Nodes;
