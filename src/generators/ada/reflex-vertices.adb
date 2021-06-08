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

with Sinfo; use Sinfo;
with Nlists; use Nlists;
with Atree; use Atree;

with Reflex.Infos; use Reflex.Infos;

package body Reflex.Vertices is
   
   function Get_Vertex_Access (V : Vertex_Id) return access Vertex_Record;
   pragma Inline (Get_Vertex_Access);
   
   ----------------
   -- Initialize --
   ----------------
   
   procedure Initialize_Vertices is
   begin
      Vertices_Nodes.Initialize;
   end Initialize_Vertices;
   
   ----------------
   -- New_Vertex --
   ----------------
   
   function New_Vertex return Vertex_Id is
      
      V  : Vertex_Class_Ptr := new Vertex_Record'(No_Vertex_Record);
      Id : Vertex_Id;
   begin
      Id := Vertices_Nodes.New_Elmt (V);
      
      return Id;
   end New_Vertex;
   
   ----------------
   -- New_Vertex --
   ----------------
   
   function New_Vertex
     (Node : Node_Id;
      K    : Vertex_Kind_Type) return Vertex_Id is
      
      V : Vertex_Id := New_Vertex;
   begin
      Set_Vertex_Kind (V, K);
      Set_Corresponding_Node (V, Node);
      return V;
   end New_Vertex;
   
   ----------------
   -- New_Vertex --
   ----------------
   
   function New_Vertex (V : access Vertex_Record) return Vertex_Id is
      Id : Vertex_Id;
   begin
      Id := Vertices_Nodes.New_Elmt (V);
      
      return Id;
   end New_Vertex;
   
   -----------------------
   -- Get_Vertex_Access --
   -----------------------
   
   function Get_Vertex_Access (V : Vertex_Id) return access Vertex_Record is
   begin
      return Vertices_Nodes.Get_Elmt (V);
   end Get_Vertex_Access;
   
   -----------------
   -- Vertex_Kind --
   -----------------
   
   function Vertex_Kind (V : Vertex_Id) return Vertex_Kind_Type is

      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      return This.Kind;
   end Vertex_Kind;
   
   ---------------------
   -- Set_Vertex_Kind --
   ---------------------
   
   procedure Set_Vertex_Kind
     (V : Vertex_Id;
      K : Vertex_Kind_Type) is

      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      This.Kind := K;
   end Set_Vertex_Kind;
   
   ------------------------
   -- Corresponding_Node --
   ------------------------
   
   function Corresponding_Node (V : Vertex_Id) return Node_Id is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      return This.Corresponding_Node;
   end Corresponding_Node;
   
   ----------------------------
   -- Set_Corresponding_Node --
   ----------------------------
   
   procedure Set_Corresponding_Node
     (V    : Vertex_Id;
      Node : Node_Id) is
      
      This  : access Vertex_Record := Get_Vertex_Access (V);
   begin
      This.Corresponding_Node := Node;
      Reflex.Infos.Set_Vertex (Node, V);
   end Set_Corresponding_Node;
   
   ------------
   -- Parent --
   ------------
   
   function Parent (V : Vertex_Id) return Vertex_Id is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      return This.Parent;
   end Parent;
   
   ----------------
   -- Set_Parent --
   ----------------
   
   procedure Set_Parent
     (V : Vertex_Id;
      P : Vertex_Id) is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      This.Parent := P;
   end Set_Parent;
   
   ----------------
   -- Enter_Code --
   ----------------
   
   function Enter_Code (V : Vertex_Id) return List_Id is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      return This.Enter_Code;
   end Enter_Code;
   
   --------------------
   -- Set_Enter_Code --
   --------------------
   
   procedure Set_Enter_Code
     (V : Vertex_Id;
      L : List_Id) is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      This.Enter_Code := L;
   end Set_Enter_Code;
   
   -----------------------
   -- Append_Enter_Code --
   -----------------------
   
   procedure Append_Enter_Code
     (V : Vertex_Id;
      C : Node_Id) is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      if This.Enter_Code = No_List then
	 This.Enter_Code := New_List;
      end if;
      
      Nlists.Append (C, This.Enter_Code);
   end Append_Enter_Code;
   
   ----------------------------
   -- Append_List_Enter_Code --
   ----------------------------
   
   procedure Append_List_Enter_Code
     (V : Vertex_Id;
      L : List_Id) is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      if This.Enter_Code = No_List then
	 This.Enter_Code := New_List;
      end if;
      
      Nlists.Append_List (L, This.Enter_Code);
   end Append_List_Enter_Code;
   
   ---------------
   -- Exit_Code --
   ---------------
   
   function Exit_Code (V : Vertex_Id) return List_Id is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      return This.Exit_Code;
   end Exit_Code;
   
   -------------------
   -- Set_Exit_Code --
   -------------------
   
   procedure Set_Exit_Code
     (V : Vertex_Id;
      L : List_Id) is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      This.Exit_Code := L;
   end Set_Exit_Code;
   
   ----------------------
   -- Append_Exit_Code --
   ----------------------
   
   procedure Append_Exit_Code
     (V : Vertex_Id;
      C : Node_Id) is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      if This.Exit_Code = No_List then
	 This.Exit_Code := New_List;
      end if;
      
      Nlists.Append (C, This.Exit_Code);
   end Append_Exit_Code;
   
   ----------------------------
   -- Append_List_Exit_Code --
   ----------------------------
   
   procedure Append_List_Exit_Code
     (V : Vertex_Id;
      L : List_Id) is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      if This.Exit_Code = No_List then
	 This.Exit_Code := New_List;
      end if;
      
      Nlists.Append_List (L, This.Exit_Code);
   end Append_List_Exit_Code;
   
   ------------------
   -- Begin_Vertex --
   ------------------
   
   function Begin_Vertex (V : Vertex_Id) return Vertex_Id is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      return This.Begin_Vertex;
   end Begin_Vertex;
   
   ----------------------
   -- Set_Begin_Vertex --
   ----------------------
   
   procedure Set_Begin_Vertex
     (V : Vertex_Id;
      B : Vertex_Id) is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      This.Begin_Vertex := B;
   end Set_Begin_Vertex;
   
   ----------------
   -- End_Vertex --
   ----------------
   
   function End_Vertex (V : Vertex_Id) return Vertex_Id is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      return This.End_Vertex;
   end End_Vertex;
   
   --------------------
   -- Set_End_Vertex --
   --------------------
   
   procedure Set_End_Vertex
     (V : Vertex_Id;
      E : Vertex_Id) is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      This.End_Vertex := E;
   end Set_End_Vertex;
   
   -------------------------
   -- Is_Transient_Vertex --
   -------------------------
   
   function Is_Transient_Vertex (V : Vertex_Id) return Boolean is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      return This.Kind in Transient_Vertex;
   end Is_Transient_Vertex;
   
   -----------------------
   -- Is_Waiting_Vertex --
   -----------------------
   
   function Is_Waiting_Vertex (V : Vertex_Id) return Boolean is
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      return This.Kind in Waiting_Vertex;
   end Is_Waiting_Vertex;
   
   ----------------
   -- Body_Graph --
   ----------------
   
   function Body_Graph (V : Vertex_Id) return Vertex_List_Id is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      return This.Body_Graph;
   end Body_Graph;
   
   --------------------
   -- Set_Body_Graph --
   --------------------
   
   procedure Set_Body_Graph
     (V : Vertex_Id;
      G : Vertex_List_Id) is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      This.Body_Graph := G;
   end Set_Body_Graph;
   
   ---------------
   -- Condition --
   ---------------
   
   function Condition (V : Vertex_Id) return Node_Id is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      return This.Condition;
   end Condition;
   
   -------------------
   -- Set_Condition --
   -------------------
   
   procedure Set_Condition 
     (V : Vertex_Id;
      N : Node_Id) is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      This.Condition := N;
   end Set_Condition;
   
   ----------------
   -- Then_Graph --
   ----------------
   
   function Loop_Graph (V : Vertex_Id) return Vertex_Id is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      return This.Loop_Graph;
   end Loop_Graph;
   
   --------------------
   -- Set_Loop_Graph --
   --------------------
   
   procedure Set_Loop_Graph
     (V : Vertex_Id;
      G : Vertex_Id) is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      This.Loop_Graph := G;
   end Set_Loop_Graph;
   
   ----------------
   -- Then_Graph --
   ----------------
   
   function Then_Graph (V : Vertex_Id) return Vertex_Id is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      return This.Then_Graph;
   end Then_Graph;
   
   --------------------
   -- Set_Then_Graph --
   --------------------
   
   procedure Set_Then_Graph
     (V : Vertex_Id;
      G : Vertex_Id) is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      This.Then_Graph := G;
   end Set_Then_Graph;
   
   ------------------
   -- Alternatives --
   ------------------
   
   function Alternatives (V : Vertex_Id) return Vertex_List_Id is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      return This.Alternatives;
   end Alternatives;
   
   ----------------------
   -- Set_Alternatives --
   ----------------------
   
   procedure Set_Alternatives
     (V   : Vertex_Id;
      Alt : Vertex_List_Id) is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      This.Alternatives := Alt;
   end Set_Alternatives;
   
   ------------------------
   -- Append_Alternative --
   ------------------------
   
   procedure Append_Alternative 
     (To  : Vertex_Id;
      Alt : Vertex_Id) is
      
      This : access Vertex_Record := Get_Vertex_Access (To);
   begin
      Append (Alt, This.Alternatives);
   end Append_Alternative;
   
   ----------------
   -- Else_Graph --
   ----------------
   
   function Else_Graph (V : Vertex_Id) return Vertex_Id is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      return This.Else_Graph;
   end Else_Graph;
   
   --------------------
   -- Set_Else_Graph --
   --------------------
   
   procedure Set_Else_Graph
     (V : Vertex_Id;
      G : Vertex_Id) is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      This.Else_Graph := G;
   end Set_Else_Graph;
   
end Reflex.Vertices;
