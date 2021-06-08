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

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Atree; use Atree;
with Errout; use Errout;
with Einfo; use Einfo;
with Sinfo; use Sinfo;
with Namet; use Namet;
with Nlists; use Nlists;
with Sem_Util; use Sem_Util;
with Sem_Eval; use Sem_Eval;
with Types; use Types;
with Stringt; use Stringt;
with Uintp; use Uintp;
with Urealp; use Urealp;

with Artics.Buffers; use Artics.Buffers;
with Reflex.Gen.Types; use Reflex.Gen.Types;
with Reflex.Gen.Ada_Outputs; use Reflex.Gen.Ada_Outputs;
with Reflex.Gen.Utils; use Reflex.Gen.Utils;
with Rxada.Gen.Ch3; use Rxada.Gen.Ch3;
with Rxada.Gen.Attrs; use Rxada.Gen.Attrs;

with Reflex.Formats;
--with Reflex.Infos;

package body Rxada.Gen.Ch4 is
   
   ---------------------------------
   -- Boolean_Op_Need_Parenthesis --
   ---------------------------------
   
   function Boolean_Op_Need_Parenthesis (Node : Node_Id) return Boolean is
      Kind        : constant Node_Kind := Nkind (Node);
      Parent_Kind : constant Node_Kind := (Nkind (Parent (Node)));
   begin
      if Nkind_In (Parent (Node), N_Op_And, N_Op_Or, N_Op_Xor) then
	 return Kind /= Parent_Kind;
      end if;
      
      return False;
   end Boolean_Op_Need_Parenthesis;

   ------------------
   -- Generate_Sum --
   ------------------

   procedure Generate_Sum
     (This : access Ada_Generator_Record;
      Val1 : Node_Id; 
      Val2 : Uint; 
      B    : Boolean) is
      
      Ob      : Output_Buffer := This.Get_Output_Buffer;
      Modular : constant Boolean := Is_Modular_Integer_Type (Etype (Val1));
   begin
      if Compile_Time_Known_Value (Val1) then
         Write_Uint (Ob, Expr_Value (Val1) + Val2, Modular => Modular);

      elsif Val2 = 0 then
         Generate_Node (This, Val1);

      elsif B then
         Write_Str_Col_Check (Ob, "(");
         Generate_Node (This, Val1);
         Write_Str_Col_Check (Ob, " + ");
         Write_Uint (Ob, Val2, Modular => Modular);
         Write_Str_Col_Check (Ob, ")");

      else
         Generate_Node (This, Val1);
         Write_Str_Col_Check (Ob, " + ");
         Write_Uint (Ob, Val2, Modular => Modular);
      end if;
   end Generate_Sum;
   
   -------------------------
   -- Generate_Difference --
   -------------------------

   procedure Generate_Difference
     (This : access Ada_Generator_Record;
      Val1 : Node_Id; 
      Val2 : Uint; 
      B    : Boolean) is
      
      Ob      : Output_Buffer := This.Get_Output_Buffer;
      Modular : constant Boolean := Is_Modular_Integer_Type (Etype (Val1));
   begin
      if Compile_Time_Known_Value (Val1) then
         Write_Uint (Ob, Expr_Value (Val1) - Val2, Modular => Modular);

      elsif Val2 = Uint_0 then
         Generate_Node (This, Val1);

      elsif B then
         Write_Str_Col_Check (Ob, "(");
         Generate_Node (This, Val1);
         Write_Str_Col_Check (Ob, " - ");
         Write_Uint (Ob, Val2, Modular => Modular);
         Write_Str_Col_Check (Ob, ")");

      else
         Generate_Node (This, Val1);
         Write_Str_Col_Check (Ob, " - ");
         Write_Uint (Ob, Val2, Modular => Modular);
      end if;
   end Generate_Difference;

   procedure Generate_Difference
     (This          : access Ada_Generator_Record;
      Val1          : Node_Id;
      Val2          : Node_Id;
      Minus_One_Min : Boolean)
   is
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      if Compile_Time_Known_Value (Val2) then
         Generate_Difference (This, Val1, Expr_Value (Val2), Minus_One_Min);

      elsif Is_Entity_Name (Val1) and then Is_Entity_Name (Val2)
        and then Entity (Val1) = Entity (Val2)
      then
         Write_Str_Col_Check (Ob, "0");

      else
         --  When Minus_One_Min is True, then generate safeguard:

         --  (Val1 < Val2 ? -1 : Val1 - Val2)

         --  Note that we rely on the front end to remove side effects by
         --  stabilizing values into temporaries, so we do not need to worry
         --  about side effects here.

         if Minus_One_Min then
	    --  Declare result in scope
	    
            Write_Str_Col_Check (Ob, "if ");
            Generate_Node (This, Val1);
            Write_Str_Col_Check (Ob, " < ");
            Generate_Node (This, Val2);
            Write_Str_Col_Check (Ob, "then");
	    Write_Eol (Ob);
	    Write_Indent_Str (Ob, "tmp := -1");
	    Write_Eol (Ob);
	    Write_Indent_Str (Ob, "else");
	    Write_Eol (Ob);
         end if;

         Generate_Node (This, Val1);
         Write_Str_Col_Check (Ob, " - ");

         --  Add parens around expression if needed

         if Nkind_In (Val2, N_Identifier, N_Expanded_Name) then
            Generate_Node (This, Val2);
         else
            Write_Str_Col_Check (Ob, "(");
            Generate_Node (This, Val2);
            Write_Str_Col_Check (Ob, ")");
         end if;

         if Minus_One_Min then
	    Write_Eol (Ob);
            Write_Indent_Str (Ob, "end if;");
         end if;
      end if;
   end Generate_Difference;
   
   ----------------------
   -- Handle_Attribute --
   ----------------------

   procedure Handle_Attribute (N : Node_Id) is
   begin
      null;
   end Handle_Attribute;
   
   --------------------------
   -- Generate_Format_List --
   --------------------------
   
   procedure Generate_Format_List
     (This : access Ada_Generator_Record;
      Lst  : List_Id;
      Line : Boolean := False) is
      
      Ob   : Output_Buffer := This.Get_Output_Buffer;
      Expr : Node_Id;
   begin
      Expr := First (Lst);
      while Present (Expr) loop
	 
	 Generate_Node (This, Expr);
	 
	 Next (Expr);
	 if Present (Expr) then
	    Write_Str (Ob, ", ");
	    
	    if Line then
	       Write_Eol (Ob);
	       Write_Indent_Str (Ob, "");
	    end if;
	 end if;
      end loop;
   end Generate_Format_List;
   
   ------------------------
   -- Generate_Aggregate --
   ------------------------
   
   procedure Generate_Aggregate
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob      : Output_Buffer := This.Get_Output_Buffer;
      Exprs   : List_Id;
      Assos   : List_Id;
      Parenth : Boolean;
   begin
      Parenth := Nkind (Parent (Node)) /= N_Qualified_Expression;
      
      Exprs := Expressions (Node);
      Assos := Component_Associations (Node);
      
      if Null_Record_Present (Node) then
	 null;
	 
      elsif Present (Exprs) then
	 if Parenth then
	    Write_Indent_Str (Ob, "(");
	 end if;
	 Indent_Begin (Ob);
	 
	 Generate_Format_List (This, Exprs, List_Length (Exprs) > 3);
	 
	 if Parenth then
	    Write_Str (Ob, ")");
	 end if;
	 
	 Indent_End (Ob);
	 Write_Eol (Ob);
      
      elsif Present (Assos) and then not Is_Empty_List (Assos) then
	 if Parenth then
	    Write_Indent_Str (Ob, "(");
	 end if;
	 Indent_Begin (Ob);
	 
	 Generate_Format_List (This, Assos, List_Length (Assos) > 3);
	 
	 if Parenth then
	    Write_Str (Ob, ")");
	 end if;
	 
	 Indent_End (Ob);
	 Write_Eol (Ob);
	 
      else
	 null;
      end if;
   end Generate_Aggregate;

   ----------------------------------
   -- Generate_Extension_Aggregate --
   ----------------------------------
   
   procedure Generate_Extension_Aggregate
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob      : Output_Buffer := This.Get_Output_Buffer;
      Parenth : Boolean;
   begin
      pragma Assert
	(Nkind_In (Ancestor_Part (Node), N_Identifier, N_Expanded_Name));
      
      Parenth := Nkind (Parent (Node)) /= N_Qualified_Expression;
      
      if Parenth then
	 Write_Str (Ob, "(");
      end if;
      
      Write_Id (Ob, Ancestor_Part (Node));
      Write_Str (Ob, " with ");
      
      if Null_Record_Present (Node) then
	 Write_Str (Ob, "null record");
	 if Parenth then
	    Write_Str (Ob, ")");
	 end if;
	 
      else
	 Generate_Aggregate (This, Node);
      end if;
   end Generate_Extension_Aggregate;
   
   -----------------------
   -- Generate_And_Then --
   -----------------------

   procedure Generate_And_Then
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Left_Opnd (This, Node);
      Write_Str (Ob, " and then ");
      Generate_Right_Opnd (This, Node);
   end Generate_And_Then;
   
   ----------------------------------
   -- Generate_Attributae_Reference --
   ----------------------------------
   
   procedure Generate_Attribute_Reference
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      Generate_Attribute (This, Node);
   end Generate_Attribute_Reference;
   
   ------------------------------
   -- Generate_Case_Expression --
   ------------------------------
   
   procedure Generate_Case_Expression
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Case_Expression;
   
   ------------------------------------------
   -- Generate_Case_Expression_Alternative --
   ------------------------------------------
   
   procedure Generate_Case_Expression_Alternative
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Case_Expression_Alternative;
   
   ------------------------------------
   -- Generate_Component_Association --
   ------------------------------------
   
   procedure Generate_Component_Association
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob          : Output_Buffer := This.Get_Output_Buffer;
      Choice_List : List_Id;
      Choice      : Node_Id;
   begin
      ------------------------------------------------------
      -- 4.3.1  Record Component Association (also 4.3.3) --
      ------------------------------------------------------

      --  RECORD_COMPONENT_ASSOCIATION ::=
      --    [COMPONENT_CHOICE_LIST =>] EXPRESSION

      --  N_Component_Association
      --  Sloc points to first selector name
      --  Choices (List1)
      --  Loop_Actions (List2-Sem)
      --  Expression (Node3) (empty if Box_Present)
      --  Box_Present (Flag15)
      --  Inherited_Discriminant (Flag13)

      --  Note: this structure is used for both record component associations
      --  and array component associations, since the two cases aren't always
      --  separable by the parser. The choices list may represent either a
      --  list of selector names in the record aggregate case, or a list of
      --  discrete choices in the array aggregate case or an N_Others_Choice
      --  node (which appears as a singleton list). Box_Present gives support
      --  to Ada 2005 (AI-287).
      
      Choice_List := Choices (Node);
      if Present (Choice_List) then
	 Choice := First (Choice_List);
	 while Present (Choice) loop
	    if Nkind (Choice) = N_Others_Choice then
	       Write_Str (Ob, "others");
	       exit;
	    else
	      Generate_Node (This, Choice);
	      if Present (Next (Choice)) then
		 Write_Str (Ob, " | ");
	      end if;
	    end if;
	    Next (Choice);
	 end loop;
	 Write_Str (Ob, " => ");
	 Generate_Node (This, Expression (Node));
      end if;
   end Generate_Component_Association;
   
   ---------------------------------------
   -- Generate_Discriminant_Association --
   ---------------------------------------
   
   procedure Generate_Discriminant_Association
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Discriminant_Association;
   
   -----------------------------------
   -- Generate_Explicit_Dereference --
   -----------------------------------
   
   procedure Generate_Explicit_Dereference
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Node (This, Prefix (Node));
      Write_Str (Ob, ".all");
   end Generate_Explicit_Dereference;
   
   --------------------------------------
   -- Generate_Expression_With_Actions --
   --------------------------------------
   
   procedure Generate_Expression_With_Actions
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Acts : List_Id;
      Act  : Node_Id;
      Expr : Node_Id;
   begin
      -----------------------------
      -- Expression With Actions --
      -----------------------------

      --  This node is created by the analyzer/expander to handle some
      --  expansion cases, notably short-circuit forms where there are
      --  actions associated with the right-hand side operand.

      --  The N_Expression_With_Actions node represents an expression with
      --  an associated set of actions (which are executable statements and
      --  declarations, as might occur in a handled statement sequence).

      --  The required semantics is that the set of actions is executed in
      --  the order in which it appears just before the expression is
      --  evaluated (and these actions must only be executed if the value
      --  of the expression is evaluated). The node is considered to be
      --  a subexpression, whose value is the value of the Expression after
      --  executing all the actions.

      --  If the actions contain declarations, then these declarations may
      --  be referenced within the expression. However note that there is
      --  no proper scope associated with the expression-with-action, so the
      --  back-end will elaborate them in the context of the enclosing scope.

      --  Sprint syntax:  do
      --                    action;
      --                    action;
      --                    ...
      --                    action;
      --                  in expression end

      --  N_Expression_With_Actions
      --  Actions (List1)
      --  Expression (Node3)
      --  plus fields for expression

      --  Note: In the final generated tree presented to the code generator,
      --  the actions list is always non-null, since there is no point in this
      --  node if the actions are Empty. During semantic analysis there are
      --  cases where it is convenient to temporarily generate an empty actions
      --  list. This arises in cases where we create such an empty actions
      --  list, and it may or may not end up being a place where additional
      --  actions are inserted. The expander removes such empty cases after
      --  the expression of the node is fully analyzed and expanded, at which
      --  point it is safe to remove it, since no more actions can be inserted.

      --  Note: In Modify_Tree_For_C, we never generate any declarations in
      --  the action list, which can contain only non-declarative statements.
      
      Acts := Actions (Node);
      if Present (Acts) then
	 Act := First (Acts);
	 while Present (Act) loop
	    Generate_Node (This, Act);
	    Next (Act);
	 end loop;
      end if;
      
      Expr := Expression (Node);
      if Present (Expr) then
	 Generate_Node (This, Expr);
      end if;
   end Generate_Expression_With_Actions;
   
   ----------------------------------
   -- Generate_Expression_Function --
   ----------------------------------
   
   procedure Generate_Expression_Function
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Expression_Function;
   
   ----------------------------------------
   -- Generate_Extended_Return_Statement --
   ----------------------------------------
   
   procedure Generate_Extended_Return_Statement
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Extended_Return_Statement;
   
   ----------------------------
   -- Generate_Function_Call --
   ----------------------------
   
   procedure Generate_Function_Call
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      Generate_Call (This, Node);
   end Generate_Function_Call;
   
   ----------------------------
   -- Generate_If_Expression --
   ----------------------------
   
   procedure Generate_If_Expression
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_If_Expression;
   
   -----------------
   -- Generate_In --
   -----------------
   
   procedure Generate_In
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Node (This, Left_Opnd (Node));
      Write_Str (Ob, " in ");
      Generate_Node (This, Right_Opnd (Node));
   end Generate_In;
   
   --------------------------------
   -- Generate_Indexed_Component --
   --------------------------------
   
   procedure Generate_Indexed_Component
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Node (This, Prefix (Node));
      Write_Str (Ob, "(");
      Generate_Comma_List (This, Expressions (Node));
      Write_Str (Ob, ")");
   end Generate_Indexed_Component;
   
   ------------------------------
   -- Generate_Integer_Literal --
   ------------------------------
   
   procedure Generate_Integer_Literal
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Write_Uint
	(Ob,
	 U       => Intval (Node),
	 Modular => Is_Modular_Integer_Type (Etype (Node)));
   end Generate_Integer_Literal;
   
   -------------------------
   -- Generate_Mod_Clause --
   -------------------------
   
   procedure Generate_Mod_Clause
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Mod_Clause;
   
   ---------------------
   -- Generate_Not_In --
   ---------------------
   
   procedure Generate_Not_In
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Node (This, Left_Opnd (Node));
      Write_Str (Ob, " not in ");
      Generate_Node (This, Right_Opnd (Node));
   end Generate_Not_In;
   
   ---------------------
   -- Generate_Op_Abs --
   ---------------------
   
   procedure Generate_Op_Abs
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Write_Str (Ob, "abs(");
      Generate_Right_Opnd (This, Node);
      Write_Str (Ob, ")");
   end Generate_Op_Abs;
   
   ---------------------
   -- Generate_Op_Add --
   ---------------------
   
   procedure Generate_Op_Add
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Left_Opnd (This, Node);
      Write_Str (Ob, " + ");
      Generate_Right_Opnd (This, Node);
   end Generate_Op_Add;
   
   ---------------------
   -- Generate_Op_And --
   ---------------------
   
   procedure Generate_Op_And
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob    : Output_Buffer := This.Get_Output_Buffer;
      Paren : constant Boolean := Boolean_Op_Need_Parenthesis (Node);
   begin
      if Paren then 
	 Write_Char (Ob, '(');
      end if;
      Generate_Left_Opnd (This, Node);
      Write_Str (Ob, " and ");
      Generate_Right_Opnd (This, Node);
      if Paren then 
	 Write_Char (Ob, ')');
      end if;
   end Generate_Op_And;
   
   ------------------------
   -- Generate_Op_Concat --
   ------------------------
   
   procedure Generate_Op_Concat
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      raise Program_Error; -- should always be expanded
   end Generate_Op_Concat;
   
   ------------------------
   -- Generate_Op_Divide --
   ------------------------
   
   procedure Generate_Op_Divide
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Left_Opnd (This, Node);
      Write_Str (Ob, " / ");
      Generate_Right_Opnd (This, Node);
   end Generate_Op_Divide;
   
   --------------------
   -- Generate_Op_Eq --
   --------------------
   
   procedure Generate_Op_Eq
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob    : Output_Buffer := This.Get_Output_Buffer;
      --  LHS   : constant Node_Id := Left_Opnd (Node);
      --  RHS   : constant Node_Id := Right_Opnd (Node);
      --  L_Typ : constant Node_Id := Get_Type_Full_View (Etype (LHS));
      --  R_Typ : constant Node_Id := Get_Type_Full_View (Etype (RHS));
   begin
      --  if Has_Fat_Pointer (L_Typ)
      --  	or else Has_Fat_Pointer (R_Typ)
      --  then
      --  	 Write_Fatptr_Compare (LHS, RHS);
	 
      --  else
	 Generate_Left_Opnd (This, Node);
	 Write_Str (Ob, " = ");
	 Generate_Right_Opnd (This, Node);
      --  end if;
   end Generate_Op_Eq;
   
   -----------------------
   -- Generate_Op_Expon --
   -----------------------
   
   procedure Generate_Op_Expon
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Left_Opnd (This, Node);
      Write_Str (Ob, " ** ");
      Generate_Right_Opnd (This, Node);
   end Generate_Op_Expon;
   
   --------------------
   -- Generate_Op_Ge --
   --------------------
   
   procedure Generate_Op_Ge
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Left_Opnd (This, Node);
      Write_Str (Ob, " >= ");
      Generate_Right_Opnd (This, Node);
   end Generate_Op_Ge;
   
   --------------------
   -- Generate_Op_Gt --
   --------------------
   
   procedure Generate_Op_Gt
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Left_Opnd (This, Node);
      Write_Str (Ob, " > ");
      Generate_Right_Opnd (This, Node);
   end Generate_Op_Gt;
   
   --------------------
   -- Generate_Op_Le --
   --------------------
   
   procedure Generate_Op_Le
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Left_Opnd (This, Node);
      Write_Str (Ob, " <= ");
      Generate_Right_Opnd (This, Node);
   end Generate_Op_Le;
   
   --------------------
   -- Generate_Op_Lt --
   --------------------
   
   procedure Generate_Op_Lt
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Left_Opnd (This, Node);
      Write_Str (Ob, " < ");
      Generate_Right_Opnd (This, Node);
   end Generate_Op_Lt;
   
   -----------------------
   -- Generate_Op_Minus --
   -----------------------
   
   procedure Generate_Op_Minus
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Write_Str (Ob, "-");
      Generate_Right_Opnd (This, Node);
   end Generate_Op_Minus;
   
   ---------------------
   -- Generate_Op_Mod --
   ---------------------
   
   procedure Generate_Op_Mod
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Left_Opnd (This, Node);
      Write_Str (Ob, " mod ");
      Generate_Right_Opnd (This, Node);
   end Generate_Op_Mod;
   
   --------------------------
   -- Generate_Op_Multiply --
   --------------------------
   
   procedure Generate_Op_Multiply
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Left_Opnd (This, Node);
      Write_Str (Ob, " * ");
      Generate_Right_Opnd (This, Node);
   end Generate_Op_Multiply;
   
   --------------------
   -- Generate_Op_Ne --
   --------------------
   
   procedure Generate_Op_Ne
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Left_Opnd (This, Node);
      Write_Str (Ob, " /= ");
      Generate_Right_Opnd (This, Node);
   end Generate_Op_Ne;
   
   ---------------------
   -- Generate_Op_Not --
   ---------------------
   
   procedure Generate_Op_Not
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Write_Str (Ob, " not ");
      Generate_Right_Opnd (This, Node);
   end Generate_Op_Not;
   
   --------------------
   -- Generate_Op_Or --
   --------------------
   
   procedure Generate_Op_Or
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob    : Output_Buffer := This.Get_Output_Buffer;
      Paren : constant Boolean := Boolean_Op_Need_Parenthesis (Node);
   begin
      if Paren then
	 Write_Char (Ob, '(');
      end if;
      Generate_Left_Opnd (This, Node);
      Write_Str (Ob, " or ");
      Generate_Right_Opnd (This, Node);
      if Paren then
	 Write_Char (Ob, ')');
      end if;
   end Generate_Op_Or;
   
   ----------------------
   -- Generate_Op_Plus --
   ----------------------
   
   procedure Generate_Op_Plus
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Write_Str (Ob, "+");
      Generate_Right_Opnd (This, Node);
   end Generate_Op_Plus;
   
   ---------------------
   -- Generate_Op_Rem --
   ---------------------
   
   procedure Generate_Op_Rem
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Left_Opnd (This, Node);
      Write_Str (Ob, " rem ");
      Generate_Right_Opnd (This, Node);
   end Generate_Op_Rem;
   
   -----------------------------
   -- Generate_Op_Rotate_Left --
   -----------------------------
   
   procedure Generate_Op_Rotate_Left
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Op_Rotate_Left;
   
   ------------------------------
   -- Generate_Op_Rotate_Right --
   ------------------------------
   
   procedure Generate_Op_Rotate_Right
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Op_Rotate_Right;
   
   -----------------------------
   -- Generate_Op_Shift_Right --
   -----------------------------
   
   procedure Generate_Op_Shift_Right
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Op_Shift_Right;
   
   ----------------------------------------
   -- Generate_Op_Shift_Right_Arithmetic --
   ----------------------------------------
   
   procedure Generate_Op_Shift_Right_Arithmetic
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Op_Shift_Right_Arithmetic;
   
   ----------------------------
   -- Generate_Op_Shift_Left --
   ----------------------------
   
   procedure Generate_Op_Shift_Left
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Op_Shift_Left;
   
   --------------------------
   -- Generate_Op_Subtract --
   --------------------------
   
   procedure Generate_Op_Subtract
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Left_Opnd (This, Node);
      Write_Str (Ob, " - ");
      Generate_Right_Opnd (This, Node);
   end Generate_Op_Subtract;
   
   ---------------------
   -- Generate_Op_Xor --
   ---------------------
   
   procedure Generate_Op_Xor
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Left_Opnd (This, Node);
      Write_Str (Ob, " xor ");
      Generate_Right_Opnd (This, Node);
   end Generate_Op_Xor;
   
   ------------------------------
   -- Generate_Operator_Symbol --
   ------------------------------
   
   procedure Generate_Operator_Symbol
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Operator_Symbol;
   
   ----------------------
   -- Generate_Or_Else --
   ----------------------
   
   procedure Generate_Or_Else
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Left_Opnd (This, Node);
      Write_Str (Ob, " or else ");
      Generate_Right_Opnd (This, Node);
   end Generate_Or_Else;
   
   ----------------------------
   -- Generate_Others_Choice --
   ----------------------------
   
   procedure Generate_Others_Choice
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Others_Choice;
   
   ------------------------------------
   -- Generate_Parameter_Association --
   ------------------------------------
   
   procedure Generate_Parameter_Association
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Parameter_Association;
   
   -----------------------------------
   -- Generate_Qualified_Expression --
   -----------------------------------
   
   procedure Generate_Qualified_Expression
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Subtype_Mark (This, Subtype_Mark (Node));
      Write_Str (Ob, "'");
      Write_Str (Ob, "(");
      Generate_Node (This, Expression (Node));
      Write_Str (Ob, ")");
   end Generate_Qualified_Expression;
   
   ------------------------------------
   -- Generate_Quantified_Expression --
   ------------------------------------
   
   procedure Generate_Quantified_Expression
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Quantified_Expression;
   
   --------------------
   -- Generate_Range --
   --------------------
   
   procedure Generate_Range
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Node (This, Low_Bound (Node));
      Write_Str (Ob, " .. ");
      Generate_Node (This, High_Bound (Node));
   end Generate_Range;
   
   
   --  ---------------
   --  -- Image_Out --
   --  ---------------

   --  procedure Write_Uint
   --    (Ob     : Output_Buffer;
   --     Input  : Uint;
   --     Format : UI_Format)
   --  is
   --     Ob : 
   --     Base   : Uint;
   --     Ainput : Uint;

   --     Digs_Output : Natural := 0;
   --     --  Counts digits output. In hex mode, but not in decimal mode, we
   --     --  put an underline after every four hex digits that are output.

   --     procedure Image_Uint (U : Uint);
   --     --  Internal procedure to output characters of non-negative Uint

   --     ----------------
   --     -- Image_Uint --
   --     ----------------

   --     procedure Image_Uint (U : Uint) is
   --        H : constant array (Int range 0 .. 15) of Character :=
   --              "0123456789ABCDEF";

   --        Q, R : Uint;
   --     begin
   --        UI_Div_Rem (U, Base, Q, R);

   --        if Q > Uint_0 then
   --           Image_Uint (Q);
   --        end if;

   --        if Digs_Output = 4 and then Base = Uint_16 then
   --           Write_Char ('_');
   --           Digs_Output := 0;
   --        end if;

   --        Write_Char (H (UI_To_Int (R)));

   --        Digs_Output := Digs_Output + 1;
   --     end Image_Uint;

   --  --  Start of processing for Image_Out

   --  begin
   --     if Input = No_Uint then
   --        Write_Char ('?');
   --        return;
   --     end if;

   --     if Input < Uint_0 then
   --        Write_Char (Ob, '-');
   --        Ainput := -Input;
   --     else
   --        Ainput := Input;
   --     end if;

   --     if Format = Hex then
   --        Base := Uint_16;
   --        Write_Char ('1');
   --        Write_Char ('6');
   --        Write_Char ('#');
   --        Write_Uint (Ainput);
   --        Write_Char ('#');

   --     else
   --        Base := Uint_10;
   --        Image_Uint (Ainput);
   --     end if;
   --  end Write_Uint;

   --  ---------------------------
   --  -- Write_Ureal_Col_Check --
   --  ---------------------------

   --  procedure Write_Ureal_Col_Check (U : Ureal) is
      
   --     procedure Write (Real : Ureal);
   --     --  Writes value of Real to standard output. As a result of evaluation of
   --     --  static expressions, it is possible to generate constants (e.g. 1/13)
   --     --  which have no such representation.

   --     -----------
   --     -- Write --
   --     -----------

   --     procedure Write (Real : Ureal) is
   --        T : Uint;
	 
   --  	 Ob : Output_Buffer;
   --     begin
   --        --  If value is negative, we precede the constant by a minus sign

   --        if UR_Is_Negative (Real) then
   --           Write_Char (Ob, '-');
   --        end if;

   --        --  Zero is zero

   --        if UR_Is_Zero (Real) then
   --           Write_Str (Ob, "0.0");

   --        --  For constants with a denominator of zero, the value is simply the
   --        --  numerator value, since we are dividing by base**0, which is 1.

   --        elsif Denominator (Real) = 0 then
   --           Write_Ui (Ob, Numerator (Real), Decimal);
   --           Write_Str (Ob, ".0");

   --        --  Small powers of 2 get written in decimal fixed-point format

   --        elsif Rbase (Real) = 2
   --          and then Denominator (Real) <= 3
   --          and then Denominator (Real) >= -16
   --        then
   --           if Denominator (Real) = 1 then
   --              T := Numerator (Real) * (10 / 2);
   --              Write_Ui (Ob, T / 10, Decimal);
   --              Write_Char (Ob, '.');
   --              Write_Ui (Ob, T mod 10, Decimal);

   --           elsif Denominator (Real) = 2 then
   --              T := Numerator (Real) * (100 / 4);
   --              UI_Write (T / 100, Decimal);
   --              Write_Char (Ob, '.');
   --              UI_Write (T mod 100 / 10, Decimal);

   --              if T mod 10 /= 0 then
   --                 UI_Write (T mod 10, Decimal);
   --              end if;

   --           elsif Denominator (Real) = 3 then
   --              T := Numerator (Real) * (1000 / 8);
   --              UI_Write (T / 1000, Decimal);
   --              Write_Char (Ob, '.');
   --              UI_Write (T mod 1000 / 100, Decimal);

   --              if T mod 100 /= 0 then
   --                 UI_Write (T mod 100 / 10, Decimal);

   --                 if T mod 10 /= 0 then
   --                    UI_Write (T mod 10, Decimal);
   --                 end if;
   --              end if;

   --           else
   --              UI_Write
   --                (Numerator (Real) * (Uint_2 ** (-Denominator (Real))),
   --                 Decimal);
   --              Write_Str (Ob, ".0");
   --           end if;

   --        --  If the base is non-zero, we normalize the real number and
   --        --  use recursion to process the resulting number.

   --        elsif Rbase (Real) /= 0 then

   --           --  Note that we do not propagate the negative sign since
   --           --  the minus character was alredy sent to the output

   --           Write
   --             (UR_From_Components
   --               (Num => Norm_Num (Real),
   --                Den => Norm_Den (Real)));

   --        --  Rationals where numerator is divisible by denominator can be
   --        --  output as literals after we do the division. This includes the
   --        --  common case where the denominator is 1.

   --        elsif Numerator (Real) mod Denominator (Real) = 0 then
   --           UI_Write (Numerator (Real) / Denominator (Real), Decimal);
   --           Write_Str (Ob, ".0");

   --        --  Other non-based (rational) constants are written in num/den style

   --        else
   --           UI_Write (Numerator (Real), Decimal);
   --           Write_Str (Ob, ".0/");
   --           UI_Write (Denominator (Real), Decimal);
   --           Write_Str (Ob, ".0");
   --        end if;
   --     end Write;

   --     --  Local variables

   --     D : constant Uint := Denominator (U);
   --     N : constant Uint := Numerator (U);

   --  begin
   --     Write (U);
   --  end Write_Ureal_Col_Check;

   
   ---------------------------
   -- Generate_Real_Literal --
   ---------------------------
   
   procedure Generate_Real_Literal
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob  : Output_Buffer := This.Get_Output_Buffer;
      Str : String_Id;
   begin
      Str := Ada_Real_Literal (Node);
      declare
	 S : String := To_String (Str);
      begin
	 Write_Str (Ob, S);
      end;
   end Generate_Real_Literal;
   
   ------------------------
   -- Generate_Reference --
   ------------------------
   
   procedure Generate_Reference
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      if Nkind (Prefix (Node)) = N_Function_Call then
	 Error_Msg_N ("unsupported kind of function call", Node);
	 
      elsif Nkind (Prefix (Node)) = N_Procedure_Call_Statement then
	 Error_Msg_N ("unsupported kind of procedure call", Node);
	 
      else      
	 Write_Char (Ob, '&');
	 Generate_Node_Paren (This, Prefix (Node));
      end if;
   end Generate_Reference;
   
   --------------------
   -- Generate_Slice --
   --------------------
   
   procedure Generate_Slice
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Node (This, Prefix (Node));
      Write_Str (Ob, "(");
      Generate_Node (This, Discrete_Range (Node));
      Write_Str (Ob, ")");
   end Generate_Slice;
   
   -----------------------------
   -- Generate_String_Literal --
   -----------------------------
   
   procedure Generate_String_Literal
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob  : Output_Buffer := This.Get_Output_Buffer;
      Str : constant String_Id := Strval (Node);
   begin
      --  Output string literal
      
      Write_Char (Ob, '"');
      
      for J in 1 .. String_Length (Str) loop
	 Write_C_Char_Code (Ob, Get_String_Char (Str, J));
      end loop;
      
      Write_Char (Ob, '"');
   end Generate_String_Literal;
   
   ------------------------------
   -- Generate_Type_Conversion --
   ------------------------------
   
   procedure Generate_Type_Conversion
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob  : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Subtype_Mark (This, Subtype_Mark (Node));
      Write_Str (Ob, "(");
      Generate_Node (This, Expression (Node));
      Write_Str (Ob, ")");
   end Generate_Type_Conversion;
   
   -----------------------------------
   -- Generate_Unchecked_Expression --
   -----------------------------------
   
   procedure Generate_Unchecked_Expression
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Unchecked_Expression;
   
   ----------------------------------------
   -- Generate_Unchecked_Type_Conversion --
   ----------------------------------------
   
   procedure Generate_Unchecked_Type_Conversion
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob  : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Subtype_Mark (This, Subtype_Mark (Node));
      Write_Str (Ob, "(");
      Generate_Node (This, Expression (Node));
      Write_Str (Ob, ")");
   end Generate_Unchecked_Type_Conversion;

   -------------------
   -- Generate_Call --
   -------------------

   procedure Generate_Call
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob        : Output_Buffer := This.Get_Output_Buffer;
      Call      : Node_Id;
      Sel       : Entity_Id;
      Actuals   : List_Id;
      Actual    : Node_Id;
      Len       : Natural;
      Count     : Natural;
      Frst_Seen : Boolean := False;
   begin
      if Nkind (Node) = N_Procedure_Call_Statement then
	 Write_Indent (Ob);
      end if;
      
      Actuals := Parameter_Associations (Node);
      Actual := First (Actuals);
      
      if Parameter_List_Truncated (Node) then
	 Generate_Node (This, Actual);
	 Write_Str (Ob, ".");
	 Next (Actual);
      end if;
      
      Call := Name (Node);
      Write_Id (Ob, Call);
      
      Count  := 0;
      Len    := 0;
      
      while Present (Actual) loop
	 if Nkind (Actual) = N_Parameter_Association then
	    Sel := Selector_Name (Actual);
	    declare
	       S : String := Get_Name_String (Chars (Entity (Sel)));
	    begin
	       if S'Length > Len then
		  Len := S'Length;
	       end if;
	    end;
	 end if;
	 Count := Count + 1;
	 Next (Actual);
      end loop;
      
      if Count = 0 then
	 return;
      end if;
      
      if Count <= 3 then
	 Write_Str (Ob, " (");
      else
	 Write_Eol (Ob);
	 Indent_Begin (Ob);
	 Write_Indent_Str (Ob, "(");
      end if;
      
      Actuals := Parameter_Associations (Node);
      Actual  := First (Actuals);
      
      if Parameter_List_Truncated (Node) then
	 Next (Actual);
      end if;
      
      while Present (Actual) loop
	 if Nkind (Actual) = N_Parameter_Association then
	    Sel := Selector_Name (Actual);
	    declare
	       S      : String := Get_Name_String (Chars (Entity (Sel)));
	       Spaces : Natural;
	    begin
	       Spaces := Len - S'Length;
	       if Frst_Seen then
		  if Count > 3 then
		     Write_Indent_Str (Ob, " ");
		  end if;
	       end if;
	       
	       Write_Id (Ob, Entity (Sel));
	       
	       if Count > 3 then
		  for I in 1..Spaces loop
		     Write_Char (Ob, ' ');
		  end loop;
	       end if;
	       
	       Write_Str (Ob, " => ");
	    end;
	    
	    --  Now emit actual
	    
	    Generate_Node (This, Explicit_Actual_Parameter (Actual));
	    
	 else
	    if Frst_Seen then
	       Write_Indent (Ob);
	       Write_Str (Ob, " ");
	    end if;
	    
	    Generate_Node (This, Actual);
	 end if;
	 
	 Frst_Seen := True;
	 
	 Next (Actual);
	 exit when No (Actual);
	 
	 Write_Str (Ob, ", ");
	 if Count > 3 then
	    Write_Eol (Ob);
	 end if;
      end loop;
      
      Write_Char (Ob, ')');
      
      if Count > 3 then
	 Indent_End (Ob);
      end if;
      
   end Generate_Call;
   
end Rxada.Gen.Ch4;
