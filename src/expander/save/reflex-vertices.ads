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

with Artics.Types; use Artics.Types;

package Rx.Vertices is
   
   --  List of vertices kind
   
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
      
      V_Begin_Vertex,
      V_End_Vertex,
      
      V_Last_vertex);
   
   subtype Transient_Vertex is Vertex_Kind range
     V_Exit_Vertex .. V_Case_Vertex;
   
   type Vertex_Record is tagged private;
   type Vertex_Ptr is access all Vertex_Record;
   type Vertex_Class_Ptr is access all Vertex_Record'Class;

   No_Vertex_Node_Record : constant Vertex_Node_Record;
   
   function Corresponding_Node (This : access Vertex_Record) return Node_Id;
   procedure Set_Corresponding_Node
     (This : access Vertex_Record;
      Node : Node_Id);
   
   function Parent (This : access Vertex_Record) return Vertex_Id;
   procedure Set_Parent
     (This : access Vertex_Record;
      V    : Vertex_Id);
   
   function Enter_Code (This : access Vertex_Record) return List_Id;
   procedure Set_Enter_Code
     (This : access Vertex_Record;
      L    : List_Id);
   
   function Exit_Code (This : access Vertex_Record) return List_Id;
   procedure Set_Exit_Code
     (This : access Vertex_Record;
      L    : List_Id);
   
   function Begin_Vertex (This : access Vertex_Record) return Vertex_Id;
   procedure Set_Begin_Vertex
     (This : access Vertex_Record;
      V    ! Vertex_Id);
   
   function End_Vertex (This : access Vertex_Record) return Vertex_Id;
   procedure Set_End_Vertex
     (This : access Vertex_Record;
      V    : Vertex_Id);
   
private
   
   type Vertex_Record is tagged record
      Associated_Node : Node_Id;
      Parent : Vertex_Id;
      Enter_Code : List_Id;
      Exit_Code  : List_Id;
      Begin_Vertex : Vertex_Id;
      End_Vertex : Vertex_Id;
   end record;
   
   No_Vertex_Record : constant Vertex_Record :=
     (Corresponding_Node => Empty,
      Parent             => No_Vertex,
      Enter_Code         => Empty_List,
      Exit_Code          => Empty_List,
      Begin_Vertex       => No_Vertex,
      End_Vertex         => No_Vertex);
   
   No_Vertex : constant Vertex_Id := -1;
   
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
   
   package Vertex_Nodes is new Table.Table
     (Table_Component_Type => Vertex_Node_Entry,
      Table_Index_Type     => Vertex_Id,
      Table_Low_Bound      => First_Vertex_Id,
      Table_Initial        => 50_000,
      Table_Increment      => 100,
      Table_Name           => "Vertex_Nodes");
   
end Rx.Graf.Nodes;
