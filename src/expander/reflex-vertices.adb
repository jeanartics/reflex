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
with Reflex.Infos; use Reflex.Infos;

package Reflex.Vertices is
   
   function Get_Vertex_Access (V : Vertex_Id) return access Vertex_Record;
   pragma Inline (Get_Vertex_Access);
   
   ----------------
   -- Initialize --
   ----------------
   
   procedure Initialize is
      
      No_Vertex_Ptr : Vertex_Class_Ptr := new Vertex_Record'(No_Vertex_Record);
   begin
      --  No_Vertex
      Vertex_Nodes.Append (No_Vertex_Ptr);
      
      --  Error_Vertex
      Vertex_Nodes.Append (No_Vertex_Ptr);
   end Initialize;
   
   ----------------
   -- New_Vertex --
   ----------------
   
   function New_Vertex return Vertex_Id is
      
      V  : Vertex_Class_Ptr := new Vertex_Record'(No_Vertex_Record);
      Id : Vertex_Id;
   begin
      Vertex_Nodes.Append (V);
      Id := Vertxex_Nodes.Last;
      Reflex.Infos.Set_Vertex (N, Id);
      
      return Id;
   end New_Vertex;
   
   ----------------
   -- New_Vertex --
   ----------------
   
   function New_Vertex (K : Vertex_Kind_Type) return Vertex_Id is
      
      V : Vertex_Id := New_Vertex;
   begin
      Set_Vertex_Kind (V, K);
      return V;
   end New_Vertex;
   
   ----------------
   -- New_Vertex --
   ----------------
   
   function New_Vertex (V : access Vertex_Record) return Vertex_Id is
      Id : Vertex_Id;
   begin
      Vertex_Nodes.Append (V);
      Id := Vertxex_Nodes.Last;
      Reflex.Infos.Set_Vertex (N, Id);
      
      return Id;
   end New_Vertex;
   
   ----------------------
   -- New_Graph_Vertex --
   ----------------------
   
   function New_Graph_Vertex return Vertex_Id is
   begin
      return New_Vertex (V_Graph_Vertex);
   end New_Graph_Vertex;
      
   ---------------------
   -- New_Exit_Vertex --
   ---------------------
   
   function New_Exit_Vertex return Vertex_Id is
   begin
      return New_Vertex (V_Exit_Vertex);
   end New_Exit_Vertex;
   
   -------------------
   -- New_If_Vertex --
   -------------------
   
   function New_If_Vertex return Vertex_Id is
   begin
      return New_Vertex (V_If_Vertex);
   end New_If_Vertex;
   
   ---------------------
   -- New_Loop_Vertex --
   ---------------------
   
   function New_Loop_Vertex return Vertex_Id is
   begin
      return New_Vertex (V_Loop_Vertex);
   end New_Loop_Vertex;
   
   ---------------------
   -- New_Case_Vertex --
   ---------------------
   
   function New_Case_Vertex return Vertex_Id is
   begin
      return New_Vertex (V_Case_Vertex);
   end New_Case_Vertex;
   
   ----------------------
   -- New_Pause_Vertex --
   ----------------------
   
   function New_Pause_Vertex return Vertex_Id is
   begin
      return New_Vertex (V_Pause_Vertex);
   end New_Pause_Vertex;
   
   ---------------------
   -- New_Wait_Vertex --
   ---------------------
   
   function New_Wait_Vertex return Vertex_Id is
   begin
      return New_Vertex (V_Wait_Vertex);
   end New_Wait_Vertex;
   
   ---------------------
   -- New_Fork_Vertex --
   ---------------------
   
   function New_Fork_Vertex return Vertex_Id is
   begin
      return New_Vertex (V_Fork_Vertex);
   end New_Fork_Vertex;
   
   -----------------------
   -- New_Select_Vertex --
   -----------------------
   
   function New_Select_Vertex return Vertex_Id is
   begin
      return New_Vertex (V_Select_Vertex);
   end New_Select_Vertex;
   
   ----------------------
   -- New_Abort_Vertex --
   ----------------------
   
   function New_Abort_Vertex return Vertex_Id is 
   begin
      return New_Vertex (V_Abort_Vertex);
   end New_Abort_Vertex;
   
   ----------------------
   -- New_Begin_Vertex --
   ----------------------
   
   function New_Begin_Vertex return Vertex_Id is
   begin
      return New_Vertex (V_Begin_Vertex);
   end New_Begin_Vertex;
   
   --------------------
   -- New_End_Vertex --
   --------------------
   
   function New_End_Vertex return Vertex_Id is
   begin
      return New_Vertex (V_End_Vertex);
   end New_End_Vertex;
   
   -----------------------
   -- Get_Vertex_Access --
   -----------------------
   
   function Get_Vertex_Access (V : Vertex_Id) return access Vertex_Record is
   begin
      return Vertex_Nodes.Table (V);
   end Get_Vertex_Access;
   
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
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      This.Corresponding_Node := Node;
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
      This.Enter_Code := Enter_Code;
   end Set_Enter_Code;
   
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
      B ! Vertex_Id) is
      
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
   
   function Body_Graph (V : Vertex_Id) return Vertices_Lists.List is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      return This.Body_Graph;
   end Body_Graph;
   
   --------------------
   -- Set_Body_Graph --
   --------------------
   
   procedure Set_Body_Graph
     (V : Vertex_Id;
      G : Vertices_Lists.List) is
      
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
   
   function Then_Graph (V : Vertex_Id) return Vertices_List.List is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      return This.Then_Graph;
   end Then_Graph;
   
   --------------------
   -- Set_Then_Graph --
   --------------------
   
   procedure Set_Then_Graph
     (V : Vertex_Id;
      G : Vertices_List.List) is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      This.Then_Graph := G;
   end Set_Then_Graph;
   
   ------------------
   -- Alternatives --
   ------------------
   
   function Alternatives (V : Vertex_Id) return Vertices_Lists.List is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      return This.Alternatives;
   end Alternatives;
   
   ----------------------
   -- Set_Alternatives --
   ----------------------
   
   procedure Set_Alternatives
     (V   : Vertex_Id;
      Alt : Vertices_Lists.List) is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      This.Alternatives := Alt;
   end Set_Alternatives;
   
   ----------------
   -- Else_Graph --
   ----------------
   
   function Else_Graph (V : Vertex_Id) return Vertices_List.List is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      return This.Else_Graph;
   end Else_Graph;
   
   --------------------
   -- Set_Else_Graph --
   --------------------
   
   procedure Set_Else_Graph
     (V : Vertex_Id;
      G : Vertices_List.List) is
      
      This : access Vertex_Record := Get_Vertex_Access (V);
   begin
      This.Else_Graph := G;
   end Set_Else_Graph;
   
   ------------------------------
   -- If_Has_Waiting_Statement --
   ------------------------------

   function If_Has_Waiting_Statement (N : Node_Id) return Boolean is
      E : Node_Id;
   begin
      if Is_Non_Empty_List (Then_Statements (N)) then
	 if Has_Waiting_Statement (Then_Statements (N)) then
	    return True;
	 end if;
      end if;

      if Present (Elsif_Parts (N)) then
         E := First (Elsif_Parts (N));
         while Present (E) loop
	    if Is_Non_Empty_List (Then_Statements (E)) then
	       if Has_Waiting_Statement (Then_Statements (E)) then
		  return True;
	       end if;
	    end if;

            Next (E);
         end loop;
      end if;

      if Present (Else_Statements (N)) then
	 if Is_Non_Empty_List (Else_Statements (N)) then
	    if Has_Waiting_Statement (Else_Statements (N)) then
	       return True;
	    end if;
	 end if;
      end if;

      return False;
   end If_Has_Waiting_Statement;

   --------------------------------
   -- Loop_Has_Waiting_Statement --
   --------------------------------

   function Loop_Has_Waiting_Statement (N : Node_Id) return Boolean is
   begin
      if Is_Non_Empty_List (Statements (N)) then
	 if Has_Waiting_Statement (Statements (N)) then
	    return True;
	 end if;
      end if;

      return False;
   end Loop_Has_Waiting_Statement;

   --------------------------------
   -- Case_Has_Waiting_Statement --
   --------------------------------

   function Case_Has_Waiting_Statement (N : Node_Id) return Boolean is
      Alt : Node_Id;
   begin
      if Is_Non_Empty_List (Alternatives (N)) then
	 Alt := First (Alternatives (N));
	 while Present (Alt) loop
	    if Has_Waiting_Statement (Statements (Alt)) then
	       return True;
	    end if;
	    Next (Alt);
	 end loop;
      end if;

      return False;
   end Case_Has_Waiting_Statement;

   ---------------------------
   -- Has_Waiting_Statement --
   ---------------------------

   function Has_Waiting_Statement (Stmts : List_Id) return Boolean is

      Node : Node_Id;
   begin
      Node := First (Stmts);
      while Present (Node) loop
	 case Nkind (Node) is
	    when N_If_Statement =>
	       if If_Has_Waiting_Statement (Node) then
		  return True;
	       end if;

	    when N_Loop_Statement =>
	       if Loop_Has_Waiting_Statement (Node) then
		  return True;
	       end if;

	    when N_Case_Statement =>
	       if Case_Has_Waiting_Statement (Node) then
		  return True;
	       end if;

	    when N_Exit_Statement =>
	       Par := Parent_Loop (Node);
	       if Is_Waiting_Statement (Par) then
		  return True;
	       end if;

	    when N_Reactive_Pause_Statement
	      | N_Reactive_Wait_Statement
	      | N_Reactive_Select_Statement
	      | N_Reactive_Fork_Statement
	      | N_Reactive_Abort_Statement =>
	       return True;

	    when others =>
	      null;
	 end case;

	 Next (Node);
      end loop;

      return False;
   end Has_Waiting_Statement;

end Reflex.Vertices;
