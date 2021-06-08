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
with Sem_Util; use Sem_Util;
with Nmake; use Nmake;
with Tbuild; use Tbuild;
with Stand; use Stand;

with Reflex.Infos; use Reflex.Infos;

package body Reflex.Vertices.Builder is
   
   --  Reactive_Type : Entity_Id;
   --  --  Current Reactive type
   
   --  Reactive_Type_Decl : Node_Id;
   --  --  The full declaration of current reactive type
   
   --  Reactive_Formal : Entity_Id;
   --  --  The formal of the reactive procedure for the reactive type
   
   --  Reactive_Procedure_Decl : Node_Id;
   --  --  Declaration of reactive procedure 
   
   --  Reactive_Procedure_Body : Node_Id;
   --  --  Body of reactive procedure 
   
   -------------------------
   -- Make_Reactive_Graph --
   -------------------------

   function Make_Reactive_Graph (Node : in Node_Id) return Vertex_Id is
      
      Stmts : List_Id;
      Reactive_Graph : Vertex_Id;
   begin
      --  Reactive_Formal := First_Formal (Node);
      --  Reactive_Type := Etype (Formal);
      --  Reactive_Type_Decl := Parent (Reactive_Type);
      
      --  Build vertices
      
      Stmts := Statements (Handled_Statement_Sequence (Node));
      Reactive_Graph := Make_Graph_Vertex (Node, Stmts);
      
      --  Declare the associated record of reactive
      --  Make_Reactive_Type_Declaration;
      
      --  Declare locals activation and desactivation
      
      
      --  Build the reactive Interpreter
      
      return Reactive_Graph;
   end Make_Reactive_Graph;
   
   ------------------------
   -- Analyze_Statements --
   ------------------------

   function Analyze_Statements
     (Graph : Vertex_Id;
      Stmts : List_Id) return Vertex_List_Id is

      Vertices        : Vertex_List_Id;
      Current_Vertex  : Vertex_Id := No_Vertex;
      Previous_Vertex : Vertex_Id;
      Code            : List_Id := No_List;
      Stmt            : Node_Id;

      procedure Append_Transitional_Code (S : Node_Id);
      -- Append a copy of the statement S to the list of non reactive
      -- statements executed when the current actif step is
      -- desactivated (Exit_Code).

      procedure Append_Exit_Code_To_Previous_Vertex;
      -- helper function for attached the Exit_Code to the preceding vertex
      -- and the current vertex Vx is now the preceding vertex. The Exit_Code
      -- is reset to accumulate the statements to be excetuted by the current
      -- step which become preceding step.

      -----------------------
      -- Append_Trans_Code --
      -----------------------

      procedure Append_Transitional_Code (S : Node_Id) is
         New_S : Node_id := New_Copy_Tree (S);
      begin
	 if No (Code) then
	    Code := New_List;
	 end if;
	 Nlists.Append_To (Code, New_S);
      end Append_Transitional_Code;

      ---------------------------
      -- Append_Pred_Exit_Code --
      ---------------------------

      procedure Append_Exit_Code_To_Previous_Vertex is
      begin
	 -- The exit is rattached to the end vertex of a composed vertex
	 -- and is attached to the vertex for non composed vertex.
	 
	 if Code /= No_List then
	    if Previous_Vertex = No_Vertex then
	       Append_List_Enter_Code (Graph, Code);
	    else
	       Append_List_Exit_Code (Previous_Vertex, Code);
	    end if;
	 end if;
	 
	 Code := No_List;
	 Previous_Vertex := Current_Vertex;
	 
	 Append (Current_Vertex, Vertices);
      end Append_Exit_Code_To_Previous_Vertex;
      
      --  Start of Analyze_Statements
   begin
      Vertices := New_List;
      Previous_Vertex := No_Vertex; 
      
      --  Walk throught statements and create the corresponding vertex.
      --  For control construct of if, loop and case statements, a vertex is
      --  build only if the construct has wating statement, reactive statement.
      --  During the walk, transitional code is collected and put in the exit
      --  code of previous waiting statement.
      
      Stmt := First (Stmts);
      while Present (Stmt) loop
	 Current_Vertex := No_Vertex;
	 case Nkind (Stmt) is
	    when N_Reactive_Pause_Statement    =>
	       Current_Vertex := Make_Pause_Vertex (Stmt);
	       Append_Exit_Code_To_Previous_Vertex;

	    when N_Reactive_Wait_Statement     =>
	       Current_Vertex := Make_Wait_Vertex (Stmt);
	       Append_Exit_Code_To_Previous_Vertex;

	    when N_Reactive_Select_Statement   =>
	       Current_Vertex := Make_Select_Vertex (Stmt);
	       Append_Exit_Code_To_Previous_Vertex;

	    when N_Reactive_Fork_Statement     =>
	       Current_Vertex := Make_Fork_Vertex (Stmt);
	       Append_Exit_Code_To_Previous_Vertex;

	    when N_Reactive_Abort_Statement    =>
	       null; -- Append_To (RL, Make_Abort_Vertex (S));

	    when N_If_Statement =>
	       if If_Has_Waiting_Statement (Stmt) then
		  Current_Vertex := Make_If_Vertex (Stmt);
		  Append_Exit_Code_To_Previous_Vertex;
	       else
		  Append_Transitional_Code (Stmt);
	       end if;

	    when N_Loop_Statement =>
	       if Loop_Has_Waiting_Statement (Stmt) then
		  Current_Vertex := Make_Loop_Vertex (Stmt);
		  Append_Exit_Code_To_Previous_Vertex;
	       else
		  Append_Transitional_Code (Stmt);
	       end if;

	    when N_Exit_Statement =>
	       Current_Vertex := Make_Exit_Vertex (Stmt);
	       Append_Exit_Code_To_Previous_Vertex;

	    when others =>
	       Append_Transitional_Code (Stmt);
	 end case;

         Next (Stmt);
      end loop;

      if Code /= No_List then
         if Current_Vertex /= No_Vertex then
            Append_List_Exit_Code (Current_Vertex, Code);
         else
            Append_List_Exit_Code (Graph, Code);
         end if;
      end if;

      return Vertices;
   end Analyze_Statements;

   -----------------------
   -- Make_Graph_Vertex --
   -----------------------
   
   function Make_Graph_Vertex 
     (Node : Node_Id;
      L    : List_Id) return Vertex_Id is
      
      V      : Vertex_Id;
      Vend   : Vertex_Id;
      Vbegin : Vertex_Id;
   begin
      V := New_Vertex (Node, V_Graph_Vertex);

      --  Create Begin and End Vertex of the graph
      
      Vbegin := New_Vertex (Node, V_Begin_Vertex);
      Vend   := New_Vertex (Node, V_End_Vertex);
      
      Set_Begin_Vertex (V, Vbegin);
      Set_End_Vertex (V, Vend);

      -- Now create the vertices corresponding to the statements in
      -- the list L.

      if Present (L) and then Is_Non_Empty_List (L) then
	 Set_Body_Graph (V, Analyze_Statements (V, L));
      end if;

      return V;
   end Make_Graph_Vertex;
      
   ----------------------
   -- Make_Exit_Vertex --
   ----------------------
   
   function Make_Exit_Vertex (Node : Node_Id) return Vertex_Id is
   begin
      return New_Vertex (Node, V_Exit_Vertex);
   end Make_Exit_Vertex;
   
   --------------------
   -- Make_If_Vertex --
   --------------------
   
   function Make_If_Vertex (Node : Node_Id) return Vertex_Id is
      
      function Make_If_Alternative (Alt_Node : Node_Id) return Vertex_Id;
      --  This is applied to either the N_If_Statement node itself or
      --  to an N_Elsif_Part node. It deals with analyzing the condition
      --  and the THEN statements associated with it.

      -------------------------
      -- Make_If_Alternative --
      -------------------------

      function Make_If_Alternative (Alt_Node : Node_Id) return Vertex_Id is
	 
	 Alt_Graph : Vertex_Id;
      begin
	 Alt_Graph := Make_Graph_Vertex (Alt_Node, Then_Statements (Alt_Node));

	 Set_Condition (Alt_Graph, New_Copy_Tree (Condition (Alt_Node)));
	 
	 return Alt_Graph;
      end Make_If_Alternative;

      V      : Vertex_Id;
      Vend   : Vertex_Id;
      Vbegin : Vertex_Id;
      Alt    : Node_Id;
   begin
      V := New_Vertex (Node, V_If_Vertex);
      
      Vbegin := Make_Begin_Vertex (Node);
      Vend   := Make_End_Vertex (Node);
      
      Set_Begin_Vertex (V, Vbegin);
      Set_End_Vertex (V, Vend);

      Set_Then_Graph (V, Make_Graph_Vertex (Empty, Then_Statements (Node)));
      Set_Condition (V, New_Copy_Tree (Condition (Node)));
      
      -- Build graf of all alternatives.

      if Present (Elsif_Parts (Node)) then
         Alt := First (Elsif_Parts (Node));
         while Present (Alt) loop
            Append_Alternative (V, Make_If_Alternative (Alt));
            Next (Alt);
         end loop;
      end if;

      if Present (Else_Statements (Node)) then
	 Set_Else_Graph (V, Make_Graph_Vertex (Empty, Else_Statements (Node)));
      end if;
      
      --  Mate transient state for the if statement and add it to the states 
      --  list of reactive types
      
      declare
	 S : Node_Id;
      begin
	 S := Make_Reactive_State 
	   (Sloc (Node),
	    Defining_Identifier => 
	      Make_Defining_Identifier 
	      (Sloc => Sloc (Node),
	       Chars => String_Find ("s")),
	    Corresponding_Node => Node);
	 
	 Set_Is_Transient (S, True);
	 
	 --  Append to the list of states
	 
      end;
	  
      return V;
   end Make_If_Vertex;
   
   ----------------------
   -- Make_Loop_Vertex --
   ----------------------
   
   function Make_Loop_Vertex (Node : Node_Id) return Vertex_Id is
      
      V      : Vertex_Id;
      Vend   : Vertex_Id;
      Vbegin : Vertex_Id;
   begin
      --  The WHILE and FOR loops are transformed in simple loop 
      -- 
      --  for a While statement the transformation is 
      --  loop
      --    exit when While_Condition;
      --    While_Statements;
      --  end loop;
      --
      --  For a for loop statement the transformtion is :
      --  declare
      --    I : type_Of (Iteration_Scheme);
      --  begin
      --    if Reverse then
      --        I := High_Bound (Iteration)
      --    else
      --        I := Low_Bound (Iteration)
      --    end if;
      --    loop
      --      exit when not I in iteratin_scheme;
      --      For_Statements;
      --      if Reverse then
      --          I := Prev (I);
      --      else
      --          I := Succ (I);
      --      end if;
      --    end loop;
      --  end;
      
      V := New_Vertex (Node, V_Loop_Vertex);

      Vbegin := New_Vertex (Node, V_Begin_Vertex);
      Vend   := New_Vertex (Node, V_End_Vertex);
      
      Set_Begin_Vertex (V, Vbegin);
      Set_End_Vertex (V, Vend);

      --  Vrepeat := New_Node (R_Repeat_Loop_Vertex);
      --  Set_Repeat_Vertex (Rn, Vrepeat);

      --  Vexit := New_Node (R_Exit_Vertex);
      --  Set_Exit_Vertex (Rn, Vexit);

      Set_Loop_Graph
	(V, Make_Graph_Vertex (Empty, Statements (Node)));
      
      return V;
   end Make_Loop_Vertex;
   
   ----------------------
   -- Make_Case_Vertex --
   ----------------------
   
   function Make_Case_Vertex (Node : Node_Id) return Vertex_Id is
   begin
      return New_Vertex (Node, V_Case_Vertex);
   end Make_Case_Vertex;
   
   -----------------------
   -- Make_Pause_Vertex --
   -----------------------
   
   function Make_Pause_Vertex (Node : Node_Id) return Vertex_Id is
   begin
      return New_Vertex (Node, V_Pause_Vertex);
   end Make_Pause_Vertex;
   
   ----------------------
   -- Make_Wait_Vertex --
   ----------------------
   
   function Make_Wait_Vertex (Node : Node_Id) return Vertex_Id is
      
      V : Vertex_Id;
   begin
      V := New_Vertex (Node, V_Wait_Vertex);
      Set_Condition (V, New_Copy_Tree (Condition (Node)));
      
      return V;
   end Make_Wait_Vertex;
   
   ----------------------
   -- Make_Fork_Vertex --
   ----------------------
   
   function Make_Fork_Vertex (Node : Node_Id) return Vertex_Id is
      
      function Make_Fork_Alternative (Alt_Node : Node_Id) return Vertex_Id;
      --  Build a graph for a fork alternative.
      
      ---------------------------
      -- Make_Fork_Alternative --
      ---------------------------

      function Make_Fork_Alternative (Alt_Node : Node_Id) return Vertex_id is
	 Alt_Graph  : Vertex_Id;
      begin
	 Alt_Graph := Make_Graph_Vertex (Alt_Node, Statements (Alt_Node));

	 return Alt_Graph;
      end Make_Fork_Alternative;

      V      : Vertex_Id;
      Vend   : Vertex_Id;
      Vbegin : Vertex_Id;
      Alt    : Node_Id;
   begin
      V := New_Vertex (Node, V_Fork_Vertex);
      
      Vbegin := Make_Begin_Vertex (Node);
      Vend   := Make_End_Vertex (Node);
      
      Set_Begin_Vertex (V, Vbegin);
      Set_End_Vertex (V, Vend);
      
      Set_Condition (V, New_Copy_Tree (Condition (Node)));

      -- Build graf of all alternatives.

      if Present (Alternatives (Node)) then
	 Alt := First (Alternatives (Node));
	 while Present (Alt) loop
	    Append_Alternative (V, Make_Fork_Alternative (Alt));
	    Next (Alt);
	 end loop;
      end if;
      
      return V;
   end Make_Fork_Vertex;
   
   ------------------------
   -- Make_Select_Vertex --
   ------------------------
   
   function Make_Select_Vertex (Node : Node_Id) return Vertex_Id is
      
      function Make_Select_Alternative (Alt_Node : Node_Id) return Vertex_Id;
      --  Create the graph for a select alternative. The graph is condtioned

      -----------------------------
      -- Make_Select_Alternative --
      -----------------------------

      function Make_Select_Alternative (Alt_Node : Node_Id) return Vertex_id is
	 Alt_Graph : Vertex_Id;
      begin
	 Alt_Graph := Make_Graph_Vertex (Alt_Node, Statements (Alt_Node));

	 Set_Condition (Alt_Graph, New_Copy_Tree (Condition (Alt_Node)));
	 
	 return Alt_Graph;
      end Make_Select_Alternative;

      V      : Vertex_Id;
      Vend   : Vertex_Id;
      Vbegin : Vertex_Id;
      Alt    : Node_Id;
   begin
      V := New_Vertex (Node, V_Select_Vertex);
      
      -- Create the end select vertex.

      Vbegin := Make_Begin_Vertex (Node);
      Vend   := Make_End_Vertex (Node);
			      
      Set_Begin_Vertex (V, Vbegin);
      Set_End_Vertex (V, Vend);

      -- Build graf of all alternatives.

      if Present (Alternatives (Node)) then
         Alt := First (Alternatives (Node));
         while Present (Alt) loop
            Append_Alternative (V, Make_Select_Alternative (Alt));
	    Next (Alt);
         end loop;
      end if;

      return V;
   end Make_Select_Vertex;
   
   -----------------------
   -- Make_Abort_Vertex --
   -----------------------
   
   function Make_Abort_Vertex (Node : Node_Id) return Vertex_Id is 
   begin
      return New_Vertex (Node, V_Abort_Vertex);
   end Make_Abort_Vertex;
   
   -----------------------
   -- Make_Begin_Vertex --
   -----------------------
   
   function Make_Begin_Vertex (Node : Node_Id) return Vertex_Id is
   begin
      return New_Vertex (Node, V_Begin_Vertex);
   end Make_Begin_Vertex;
   
   ---------------------
   -- Make_End_Vertex --
   ---------------------
   
   function Make_End_Vertex (Node : Node_Id) return Vertex_Id is
   begin
      return New_Vertex (Node, V_End_Vertex);
   end Make_End_Vertex;
   
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

      function Parent_Loop (N : Node_Id) return Node_Id;
      
      -----------------
      -- Parent_Loop --
      -----------------

      function Parent_Loop (N : Node_Id) return Node_Id is
         P : Node_Id;
      begin
         P := Parent (N);
         while Present (P) loop
            if Nkind (P) = N_Loop_Statement then
               return P;
            end if;
            P := Parent (P);
         end loop;
         return Empty;
      end Parent_Loop;
      
      Node : Node_Id;
      --Par  : Node_Id;
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
--              Par := Parent_Loop (Node);
--              if Present (Par) then 
--  	       if Is_Waiting_Statement (Par) then
--  		  return True;
--  	       end if;
--              end if;
null;
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

   ------------------------------------
   -- Make_Reactive_Type_Declaration --
   ------------------------------------
   
   procedure Make_Reactive_Type_Declaration
     (T     : Node_Id;
      React : Node_Id) is
      
      
      Lits : List_Id;
      States_Enum_Def : Node_Id;
      States_Enum_Type : Node_Id;
      State            : Node_Id;
      Lit_Id    : Node_Id;
      
      Array_Type_Def : Node_Id;   
      Array_Type_Id : Node_Id;
      Array_Full_Type : Node_Id;
      
      Items : List_Id;
      Comps : Node_Id;
      
      Cuurent_Array : Node_Id;
      
      Rec_Def : Node_Id; 
      Type_Decls : Node_Id;
      Pack_Node : Node_Id;
      Loc : Source_Ptr := Sloc (T);
      
      Xa : Node_Id;
      Xd : Node_Id;
      Decls : List_Id;
   begin
      --  Replace the reactive Type type definition by declaration a private
      --  type declaration
      
      --  Create the states enumeration type definition. 
      
      Lits := New_List;
      States_Enum_Def := Make_Enumeration_Type_Definition
	(Sloc       => Loc,
	 Literals   => Lits);
      -- End_Label                    : Node_Id := Empty)
      declare
	 S : String := Get_String (Chars (T)) & "_" & "state_type";
      begin
	 States_Enum_Type := Make_Full_Type_Declaration
	   (Sloc              => Loc,
	    Defining_Identifier => 
	      Make_Defining_Identifier
	      (Sloc => Loc,
	       Chars => String_Find (S)),
	    Type_Definition     => States_Enum_Def);
      end;
      
      --  Create the literal enumeration for each state, and add it to the state
      --  enumeration type
      
      State := First (States (Type_Definition (T)));
      while Present (State) loop
	 Lit_Id := Make_Defining_Identifier
	   (Loc, Chars (Defining_Identifier (State)));
	 Append (Lit_Id, Lits);
	 Next (State);
      end loop;
      
      --  Make array of states type definition with index of type enumeartion 
      --  type just defined. The array is a aboolean array indexed by the state
      --  literals. 
      
      Array_Type_Def := Make_Constrained_Array_Definition
        (Sloc                         => Loc,
         Discrete_Subtype_Definitions => 
          New_List
            (Make_Range_Constraint 
             (Sloc             => Loc,
              Range_Expression =>
                (Make_Range
                   (Sloc       => Loc,
                    Low_Bound  => 
                      New_Occurrence_Of (First (Lits), Loc),
                    High_Bound => 
                      New_Occurrence_Of (Last (Lits), Loc))))),
	 Component_Definition =>
	   New_Occurrence_Of (Standard_Boolean, Loc));
      
      --  Declare the array full type definition  of states
      
      declare
	 S : String := Get_String (Chars (T)) & "_" & "state_array";
      begin
	 Array_Type_Id := Make_Defining_Identifier
	      (Sloc => Loc,
	       Chars => String_Find (S));
	 
	 Array_Full_Type := Make_Full_Type_Declaration
	   (Sloc                => Loc,
	    Defining_Identifier => Array_Type_Id,
	    Type_Definition     => Array_Type_Def);
      end;
      
      --  Create the record which will replace the reactive type. The meber of 
      --  the record are: the states array, the init boolean of the reactive
      
      Items := New_List;
      Comps := Make_Component_List
	(Sloc             => Loc,
	 Component_Items  => Items);
      
      --  Initialize the states array to false.
      
      Cuurent_Array := Make_Component_Declaration
	(Sloc                  => Loc,
	 Defining_Identifier   =>
	   Make_Defining_Identifier 
	   (Sloc => Loc,
	    Chars => String_Find ("xc")),
	 Component_Definition  => New_Occurrence_Of (Array_Type_Id, loc),
	 Expression            => Empty);
      --  (others => False)
      
      Append (Cuurent_Array, Items);
	
      --  Full type declaration, type definition is a Record
      
      Rec_Def := Make_Record_Definition
	(Sloc            => loc,
	 Tagged_Present  => True,
	 Component_List  => Comps);
	 
      Type_Decls := Make_Full_Type_Declaration
	(Sloc                       => Loc,
	 Defining_Identifier        => 
	   Make_Defining_Identifier
	      (Sloc => Loc,
	       Chars => Chars (T)),
	 Type_Definition          => Rec_Def);
      
      --   Declare the newly created entity
      
      Pack_Node := Unit_Declaration_Node (Defining_Entity (T));
      
      --  Declare the private type extension in public declarations
      
      Replace (T, Type_Decls);
      
      
      --  Declare the state enum type in public declaration
      
      -- Locals variables --
      ----------------------
      
      --  The local variables are the activation/desactivation array boolean 
      
      Xa := Make_Object_Declaration
	(Sloc => Loc,
	 Defining_Identifier => 
	   Make_Defining_Identifier 
	   (Sloc => Loc,
	    Chars => String_Find ("xa")),
	 
	 Aliased_Present   => False,
	 Constant_Present  => False,
	 Object_Definition => New_Occurrence_Of (Array_Type_Id, Loc),
	 Expression        => Empty);
      
      Xd := Make_Object_Declaration
	(Sloc => Loc,
	 Defining_Identifier => 
	   Make_Defining_Identifier 
	   (Sloc => loc,
	    Chars => String_Find ("xa")),
	 
	 Aliased_Present   => False,
	 Constant_Present  => False,
	 Object_Definition => New_Occurrence_Of (Array_Type_Id, loc),
	 Expression        => Empty);
      
      Decls := Declarations (React);
      if No (Decls) then
	 Decls := New_List;
	 Set_Declarations (React, Decls);
      end if;
      Append (Xa, Decls);
      Append (Xd, Decls);
      
   end Make_Reactive_Type_Declaration;
   
end Reflex.Vertices.Builder;
