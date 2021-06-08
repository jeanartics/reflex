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

with Artics.Elmt_Nlists;

with Types; use Types;
with Namet; use Namet;
with Table;

with Reflex.Types; use Reflex.Types;

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
   
   subtype Transient_Vertex is Vertex_Kind_Type range
     V_Exit_Vertex .. V_Case_Vertex;
   
   subtype Waiting_Vertex is Vertex_Kind_Type range
     V_Pause_Vertex .. V_Abort_Vertex;
   
   type Vertex_Record is tagged private;
   type Vertex_Ptr is access all Vertex_Record;
   type Vertex_Class_Ptr is access all Vertex_Record'Class;

   No_Vertex_Record : constant Vertex_Record;
   
   procedure Initialize_Vertices;
   
   function New_Vertex return Vertex_Id;
   
   function New_Vertex
     (Node : Node_Id;
      K    : Vertex_Kind_Type) return Vertex_Id;
   
   function New_Vertex (V : access Vertex_Record) return Vertex_Id;
   
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
   procedure Append_Enter_Code
     (V : Vertex_Id;
      C : Node_Id);
   procedure Append_List_Enter_Code
     (V : Vertex_Id;
      L : List_Id);
   
   function Exit_Code (V : Vertex_Id) return List_Id;
   procedure Set_Exit_Code
     (V : Vertex_Id;
      L : List_Id);
   procedure Append_Exit_Code
     (V : Vertex_Id;
      C : Node_Id);
   procedure Append_List_Exit_Code
     (V : Vertex_Id;
      L : List_Id);
   
   function Begin_Vertex (V : Vertex_Id) return Vertex_Id;
   procedure Set_Begin_Vertex
     (V : Vertex_Id;
      B : Vertex_Id);
   
   function End_Vertex (V : Vertex_Id) return Vertex_Id;
   procedure Set_End_Vertex
     (V : Vertex_Id;
      E : Vertex_Id);
   
   function Is_Transient_Vertex (V : Vertex_Id) return Boolean;
   function Is_Waiting_Vertex (V : Vertex_Id) return Boolean;
   
   function Body_Graph (V : Vertex_Id) return Vertex_List_Id;
   procedure Set_Body_Graph
     (V : Vertex_Id;
      G : Vertex_List_Id);
      --  Graph_Vertex, Loop_Vertex_Record

   function Condition (V : Vertex_Id) return Node_Id;
   procedure Set_Condition 
     (V : Vertex_Id;
      N : Node_Id);
      --  If_Vertex, Exit_Vertex, Wait_Vertex, Select_vertex, Fork_Vertex,
      --  Abort_Vertex
      
   function Loop_Graph (V : Vertex_Id) return Vertex_Id;
   procedure Set_Loop_Graph
     (V : Vertex_Id;
      G : Vertex_Id);
      --  Loop_Vertex
      
   function Then_Graph (V : Vertex_Id) return Vertex_Id;
   procedure Set_Then_Graph
     (V : Vertex_Id;
      G : Vertex_Id);
      --  If_Vertex
      
   function Alternatives (V : Vertex_Id) return Vertex_List_Id;
   procedure Set_Alternatives
     (V   : Vertex_Id;
      Alt : Vertex_List_Id);
      --  If_Vertex, Select_vertex, Fork_Vertex,
   
   procedure Append_Alternative 
     (To  : Vertex_Id;
      Alt : Vertex_Id);
   
   function Else_Graph (V : Vertex_Id) return Vertex_Id;
   procedure Set_Else_Graph
     (V : Vertex_Id;
      G : Vertex_Id);
      --  If_Vertex
   
private
   
   type Vertex_Record is tagged record
      --  All vertices
      
      Kind : Vertex_Kind_Type;
      Corresponding_Node : Node_Id;
      Parent : Vertex_Id;
      Enter_Code : List_Id;
      Exit_Code  : List_Id;
      Begin_Vertex : Vertex_Id;
      End_Vertex : Vertex_Id;
      
      Body_Graph : Vertex_List_Id;
      --  Graph_Vertex, Loop_Vertex_Record

      Condition : Node_Id;
      --  If_Vertex, Exit_Vertex, Wait_Vertex, Select_vertex, Fork_Vertex,
      --  Abort_Vertex
      
      Loop_Graph : Vertex_Id;
      --  Loop_Vertex
      
      Then_Graph : Vertex_Id;
      --  If_Vertex
      
      Alternatives : Vertex_List_Id;
      --  If_Vertex, Select_vertex, Fork_Vertex,

      Else_Graph : Vertex_Id;
      --  If_Vertex
      
      --  Corresponding_State : Node_Id;
      --  Corresponding_Lit   : Node_Id;
   end record;
   
   No_Vertex_Record : constant Vertex_Record :=
     (Kind               => V_No_Vertex,
      Corresponding_Node => Empty,
      Parent             => No_Vertex,
      Enter_Code         => No_List,
      Exit_Code          => No_List,
      Begin_Vertex       => No_Vertex,
      End_Vertex         => No_Vertex,
      Body_Graph         => No_Vertex_List,
      Condition          => Empty,
      Loop_Graph         => No_Vertex,
      Then_Graph         => No_Vertex,
      Alternatives       => No_Vertex_List,
      Else_Graph         => No_Vertex);
   
   package Vertices_Nodes is new Artics.Elmt_Nlists
     (Elmt_T          => Vertex_Class_Ptr,
      Elmt_Id         => Vertex_Id,
      List_Id         => Vertex_List_Id,
      Elmts_Initial   => 50_000,
      Elmts_Increment => 100,
      Lists_Initial   => 5_000,
      Lists_Increment => 100);
   use Vertices_Nodes;

end Reflex.Vertices;
