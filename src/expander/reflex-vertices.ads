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
with Ada.Containers.Doubly_Linked_Lists;

with Types; use Types;
with Namet; use Namet;

with Artics.Types; use Artics.Types;

package Reflex.Vertices is
   
   --  List of vertices kind
   
   type Vertex_Kind_Type is
     (V_No_Vertex,
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
   
   subtype Waiting_Vertex is Vertex_Kind range
     V_Pause_Vertex .. V_Abort_Vertex;
   
   package Vertices_Lists is 
      new Ada.Containers.Doubly_Linked_Lists (Vertex_Id);
   
   type Vertex_Record is tagged private;
   type Vertex_Ptr is access all Vertex_Record;
   type Vertex_Class_Ptr is access all Vertex_Record'Class;

   No_Vertex_Record : constant Vertex_Record;
   
   function New_Graph_Vertex return Vertex_Id;
   function New_Exit_Vertex return Vertex_Id;
   function New_If_Vertex return Vertex_Id;
   function New_Loop_Vertex return Vertex_Id;
   function New_Case_Vertex return Vertex_Id;
   function New_Pause_Vertex return Vertex_Id;
   function New_Wait_Vertex return Vertex_Id;
   function New_Fork_Vertex return Vertex_Id;
   function New_Select_Vertex return Vertex_Id;
   function New_Abort_Vertex return Vertex_Id;
   function New_Begin_Vertex return Vertex_Id;
   function New_End_Vertex return Vertex_Id;
   
   function Vertex_Kind (V : Vertex_Id) return Vertex_Kind_Type;
   procedure Set_Vertex_Kind
     (V : Vertex_Id;
      K : Vertex_Kind_Type);
   
   function Corresponding_Node (V : Vertex_Id) return Node_Id;
   procedure Set_Corresponding_Node
     (V    : Vertex_Id;
      Node : Node_Id);
   
   function Parent (V : Vertex_Id) return Vertex_Id;
   procedure Set_Parent
     (V : Vertex_Id;
      P : Vertex_Id);
   
   function Enter_Code (V : Vertex_Id) return List_Id;
   procedure Set_Enter_Code
     (V : Vertex_Id;
      L : List_Id);
   
   function Exit_Code (V : Vertex_Id) return List_Id;
   procedure Set_Exit_Code
     (V : Vertex_Id;
      L : List_Id);
   
   function Begin_Vertex (V : Vertex_Id) return Vertex_Id;
   procedure Set_Begin_Vertex
     (V : Vertex_Id;
      B ! Vertex_Id);
   
   function End_Vertex (V : Vertex_Id) return Vertex_Id;
   procedure Set_End_Vertex
     (V : Vertex_Id;
      E : Vertex_Id);
   
   function Is_Transient_Vertex (V : Vertex_Id) return Boolean;
   function Is_Waiting_Vertex (V : Vertex_Id) return Boolean;
   
   function Body_Graph (V : Vertex_Id) return Vertices_Lists.List;
   procedure Set_Body_Graph
     (V : Vertex_Id;
      G : Vertices_Lists.List);
      --  Graph_Vertex, Loop_Vertex_Record

   function Condition (V : Vertex_Id) return Node_Id;
   procedure Set_Condition 
     (V : Vertex_Id;
      N : Node_Id);
      --  If_Vertex, Exit_Vertex, Wait_Vertex, Select_vertex, Fork_Vertex,
      --  Abort_Vertex
      
   function Then_Graph (V : Vertex_Id) return Vertices_List.List;
   procedure Set_Then_Graph
     (V : Vertex_Id;
      G : Vertices_List.List);
      --  If_Vertex
      
   function Alternatives (V : Vertex_Id) return Vertices_Lists.List;
   procedure Set_Alternatives
     (V   : Vertex_Id;
      Alt : Vertices_Lists.List);
      --  If_Vertex, Select_vertex, Fork_Vertex,

   function Else_Graph (V : Vertex_Id) return Vertices_List.List;
   procedure Set_Else_Graph
     (V : Vertex_Id;
      G : Vertices_List.List);
      --  If_Vertex
   
   function Has_Waiting_Statement      (Stmts : List_Id) return Boolean;
   function If_Has_Waiting_Statement   (N : Node_Id) return Boolean;
   function Loop_Has_Waiting_Statement (N : Node_Id) return Boolean;

private
   
   type Vertex_Record is tagged record
      --  All vertices
      
      Kind : Vertex_Kind_Type;
      Associated_Node : Node_Id;
      Parent : Vertex_Id;
      Enter_Code : List_Id;
      Exit_Code  : List_Id;
      Begin_Vertex : Vertex_Id;
      End_Vertex : Vertex_Id;
      
      Body_Graph : Vertices_Lists.List;
      --  Graph_Vertex, Loop_Vertex_Record

      Condition : Node_Id;
      --  If_Vertex, Exit_Vertex, Wait_Vertex, Select_vertex, Fork_Vertex,
      --  Abort_Vertex
      
      Then_Graph : Vertices_List.List;
      --  If_Vertex
      
      Alternatives : Vertices_Lists.List;
      --  If_Vertex, Select_vertex, Fork_Vertex,

      Else_Graph : Vertices_List.List;
      --  If_Vertex
   end record;
   
   No_Vertex_Record : constant Vertex_Record :=
     (Kind               => No_Vertex,
      Corresponding_Node => Empty,
      Parent             => No_Vertex,
      Enter_Code         => Empty_List,
      Exit_Code          => Empty_List,
      Begin_Vertex       => No_Vertex,
      End_Vertex         => No_Vertex,
      Body_Graph         => Vertices_Lists.Empty_List,
      Condition          => Empty,
      Then_Graph         => Vertices_List.Empty_List,
      Alternatives       => Vertices_Lists.Empty_List,
      Else_Graph         => Vertices_List.Empty_List);
   
   package Vertex_Nodes is new Table.Table
     (Table_Component_Type => Vertex_Class_Ptr,
      Table_Index_Type     => Vertex_Id,
      Table_Low_Bound      => First_Vertex_Id,
      Table_Initial        => 50_000,
      Table_Increment      => 100,
      Table_Name           => "Vertex_Nodes");
   
   pragma Inline (New_Graph_Vertex);
   pragma Inline (New_Exit_Vertex);
   pragma Inline (New_If_Vertex);
   pragma Inline (New_Loop_Vertex);
   pragma Inline (New_Case_Vertex);
   pragma Inline (New_Pause_Vertex);
   pragma Inline (New_Wait_Vertex);
   pragma Inline (New_Fork_Vertex);
   pragma Inline (New_Select_Vertex);
   pragma Inline (New_Abort_Vertex);
   pragma Inline (New_Begin_Vertex);
   pragma Inline (New_End_Vertex);
   
end Reflex.Vertices;
