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
with Reflex.Gen.Outputs; use Reflex.Gen.Outputs;
with Reflex.Gen.Utils; use Reflex.Gen.Utils;
with Glips.Gen.Attrs; use Glips.Gen.Attrs;
with Glips.Gen.Ch3; 

with Reflex.Formats;

package body Glips.Gen.Ch4 is
   
   ------------------
   -- Generate_Sum --
   ------------------

   procedure Generate_Sum
     (This : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
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
     (This          : access Glips_Generator_Record;
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

   ------------------------
   -- Generate_Aggregate --
   ------------------------
   
   procedure Generate_Aggregate
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob    : Output_Buffer := This.Get_Output_Buffer;
      Exprs : List_Id;
      Assos : List_Id;
   begin
      -----------------------------
      -- 4.3.1  Record Aggregate --
      -----------------------------

      --  RECORD_AGGREGATE ::= (RECORD_COMPONENT_ASSOCIATION_LIST)

      --  N_Aggregate
      --  Sloc points to left parenthesis
      --  Expressions (List1) (set to No_List if none or null record case)
      --  Component_Associations (List2) (set to No_List if none)
      --  Null_Record_Present (Flag17)
      --  Aggregate_Bounds (Node3-Sem)
      --  Associated_Node (Node4-Sem)
      --  Compile_Time_Known_Aggregate (Flag18-Sem)
      --  Expansion_Delayed (Flag11-Sem)
      --  Has_Self_Reference (Flag13-Sem)
      --  plus fields for expression

      --  Note: this structure is used for both record and array aggregates
      --  since the two cases are not separable by the parser. The parser
      --  makes no attempt to enforce consistency here, so it is up to the
      --  semantic phase to make sure that the aggregate is consistent (i.e.
      --  that it is not a "half-and-half" case that mixes record and array
      --  syntax. In particular, for a record aggregate, the expressions
      --  field will be set if there are positional associations.

      --  Note: N_Aggregate is not used for all aggregates; in particular,
      --  there is a separate node kind for extension aggregates.

      --  Note: gigi/gcc can handle array aggregates correctly providing that
      --  they are entirely positional, and the array subtype involved has a
      --  known at compile time length and is not bit packed, or a convention
      --  Fortran array with more than one dimension. If these conditions
      --  are not met, then the front end must translate the aggregate into
      --  an appropriate set of assignments into a temporary.

      --  Note: for the record aggregate case, gigi/gcc can handle most cases
      --  of record aggregates, including those for packed, and rep-claused
      --  records, and also variant records, providing that there are no
      --  variable length fields whose size is not known at compile time,
      --  and providing that the aggregate is presented in fully named form.

      --  The other situation in which array aggregates and record aggregates
      --  cannot be passed to the back end is if assignment to one or more
      --  components itself needs expansion, e.g. in the case of an assignment
      --  of an object of a controlled type. In such cases, the front end
      --  must expand the aggregate to a series of assignments, and apply
      --  the required expansion to the individual assignment statements.
      
      Exprs := Expressions (Node);
      Assos := Component_Associations (Node);
      
      if Null_Record_Present (Node) then
	 null;
	 
      else
	 Write_Str_Col_Check (Ob, "(");
	 
	 if Present (Expressions (Node)) then
	    Generate_Comma_List (This, Expressions (Node));
	    
	 
	 elsif Present (Component_Associations (Node))
	   and then not Is_Empty_List (Component_Associations (Node))
	 then
	    Indent_Begin (Ob);
	    
	    declare
	       Nd : Node_Id;
	    begin
	       Nd := First (Component_Associations (Node));
	       
	       loop
		  Write_Indent (Ob);
		  Generate_Node (This, Nd);
		  Next (Nd);
		  exit when No (Nd);
		  Write_Str (Ob, ", ");
	       end loop;
	    end;
	    
	    Indent_End (Ob);
	 end if;
	 
	 Write_Char (Ob, ')');
      end if;
   end Generate_Aggregate;

   ------------------------
   -- Generate_Aggregate --
   ------------------------
   
   procedure Generate_Extension_Aggregate
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob       : Output_Buffer := This.Get_Output_Buffer;
      Exprs    : List_Id;
      Assos    : List_Id;
      Ancestor : Node_Id;
   begin
      --  EXTENSION_AGGREGATE ::=
      --    (ANCESTOR_PART with RECORD_COMPONENT_ASSOCIATION_LIST)

      --  Note: extension aggregates are not permitted in Ada 83 mode

      --  N_Extension_Aggregate
      --  Sloc points to left parenthesis
      --  Ancestor_Part (Node3)
      --  Associated_Node (Node4-Sem)
      --  Expressions (List1) (set to No_List if none or null record case)
      --  Component_Associations (List2) (set to No_List if none)
      --  Null_Record_Present (Flag17)
      --  Expansion_Delayed (Flag11-Sem)
      --  plus fields for expression

      Exprs := Expressions (Node);
      Assos := Component_Associations (Node);
      Ancestor := Ancestor_Part (Node);
      
      if Null_Record_Present (Node) then
	 Write_Str (Ob, "(");
	 Generate_Node (This, Ancestor);
	 Put_Line ("/=/=/=");
	 Write_Char (Ob, ')');
	 
      else
	 Write_Str_Col_Check (Ob, "(");
	 
	 if Present (Expressions (Node)) then
	    Generate_Node (This, Ancestor);
	    Write_Eol (Ob);
	    Generate_Comma_List (This, Expressions (Node));
	    
	 elsif Present (Component_Associations (Node)) then
	    Indent_Begin (Ob);
	    Write_Str (Ob, "_parent => ");
	    Generate_Node (This, Ancestor);
	    if not Is_Empty_List (Component_Associations (Node)) then
	       Write_Str (Ob, ",");
	       Write_Eol (Ob);
	       
	       declare
		  Nd : Node_Id;
	       begin
		  Nd := First (Component_Associations (Node));
		  
		  loop
		     Write_Indent (Ob);
		     Generate_Node (This, Nd);
		     Next (Nd);
		     exit when No (Nd);
		     Write_Str (Ob, ", ");
		  end loop;
	       end;
	       
	    end if;
	    
	    Indent_End (Ob);
	 end if;
	 
	 Write_Char (Ob, ')');
      end if;
      
   end Generate_Extension_Aggregate;

   -----------------------
   -- Generate_And_Then --
   -----------------------

   procedure Generate_And_Then
     (This : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      Generate_Attribute (This, Node);
   end Generate_Attribute_Reference;
   
   ------------------------------
   -- Generate_Case_Expression --
   ------------------------------
   
   procedure Generate_Case_Expression
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Case_Expression;
   
   ------------------------------------------
   -- Generate_Case_Expression_Alternative --
   ------------------------------------------
   
   procedure Generate_Case_Expression_Alternative
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Case_Expression_Alternative;
   
   ------------------------------------
   -- Generate_Component_Association --
   ------------------------------------
   
   procedure Generate_Component_Association
     (This : access Glips_Generator_Record;
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
	    pragma Assert (Nkind (Choice) /= N_Others_Choice);
	    
	    if Nkind (Choice) = N_Others_Choice then
	       Write_Str (Ob, "others => ");
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
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Discriminant_Association;
   
   -----------------------------------
   -- Generate_Explicit_Dereference --
   -----------------------------------
   
   procedure Generate_Explicit_Dereference
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Node (This, Prefix (Node));
      Write_Char (Ob, '^');
   end Generate_Explicit_Dereference;
   
   --------------------------------------
   -- Generate_Expression_With_Actions --
   --------------------------------------
   
   procedure Generate_Expression_With_Actions
     (This : access Glips_Generator_Record;
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
      --  cases wher_ore it is convenient to temporarily generate an empty actions
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
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Expression_Function;
   
   ----------------------------------------
   -- Generate_Extended_Return_Statement --
   ----------------------------------------
   
   procedure Generate_Extended_Return_Statement
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Extended_Return_Statement;
   
   ----------------------------
   -- Generate_Function_Call --
   ----------------------------
   
   procedure Generate_Function_Call
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Function_Call;
   
   ----------------------------
   -- Generate_If_Expression --
   ----------------------------
   
   procedure Generate_If_Expression
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_If_Expression;
   
   -----------------
   -- Generate_In --
   -----------------
   
   procedure Generate_In
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob  : Output_Buffer := This.Get_Output_Buffer;
   begin
      Write_Str (Ob, " in ");
      Generate_Left_Opnd (This, Node);
      Write_Str (Ob, " .. ");
      Generate_Right_Opnd (This, Node);
   end Generate_In;
   
   --------------------------------
   -- Generate_Indexed_Component --
   --------------------------------
   
   procedure Generate_Indexed_Component
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Indexed_Component;
   
   ------------------------------
   -- Generate_Integer_Literal --
   ------------------------------
   
   procedure Generate_Integer_Literal
     (This : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Mod_Clause;
   
   ---------------------
   -- Generate_Not_In --
   ---------------------
   
   procedure Generate_Not_In
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob  : Output_Buffer := This.Get_Output_Buffer;
   begin
      Write_Str (Ob, " not in ");
      Generate_Left_Opnd (This, Node);
      Write_Str (Ob, " .. ");
      Generate_Right_Opnd (This, Node);
   end Generate_Not_In;
   
   ---------------------
   -- Generate_Op_Abs --
   ---------------------
   
   procedure Generate_Op_Abs
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Write_Str (Ob, "abs(");
      Generate_Right_Opnd (This, Node);
      Write_Str (Ob, ");");
   end Generate_Op_Abs;
   
   ---------------------
   -- Generate_Op_Add --
   ---------------------
   
   procedure Generate_Op_Add
     (This : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Left_Opnd (This, Node);
      Write_Str (Ob, " and ");
      Generate_Right_Opnd (This, Node);
   end Generate_Op_And;
   
   ------------------------
   -- Generate_Op_Concat --
   ------------------------
   
   procedure Generate_Op_Concat
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      raise Program_Error; -- should always be expanded
   end Generate_Op_Concat;
   
   ------------------------
   -- Generate_Op_Divide --
   ------------------------
   
   procedure Generate_Op_Divide
     (This : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob    : Output_Buffer := This.Get_Output_Buffer;
   begin
	 Generate_Left_Opnd (This, Node);
	 Write_Str (Ob, " = ");
	 Generate_Right_Opnd (This, Node);
      --  end if;
   end Generate_Op_Eq;
   
   -----------------------
   -- Generate_Op_Expon --
   -----------------------
   
   procedure Generate_Op_Expon
     (This : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Generate_Left_Opnd (This, Node);
      Write_Str (Ob, " or ");
      Generate_Right_Opnd (This, Node);
   end Generate_Op_Or;
   
   ----------------------
   -- Generate_Op_Plus --
   ----------------------
   
   procedure Generate_Op_Plus
     (This : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Op_Rotate_Left;
   
   ------------------------------
   -- Generate_Op_Rotate_Right --
   ------------------------------
   
   procedure Generate_Op_Rotate_Right
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Op_Rotate_Right;
   
   -----------------------------
   -- Generate_Op_Shift_Right --
   -----------------------------
   
   procedure Generate_Op_Shift_Right
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Op_Shift_Right;
   
   ----------------------------------------
   -- Generate_Op_Shift_Right_Arithmetic --
   ----------------------------------------
   
   procedure Generate_Op_Shift_Right_Arithmetic
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Op_Shift_Right_Arithmetic;
   
   ----------------------------
   -- Generate_Op_Shift_Left --
   ----------------------------
   
   procedure Generate_Op_Shift_Left
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Op_Shift_Left;
   
   --------------------------
   -- Generate_Op_Subtract --
   --------------------------
   
   procedure Generate_Op_Subtract
     (This : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Operator_Symbol;
   
   ----------------------
   -- Generate_Or_Else --
   ----------------------
   
   procedure Generate_Or_Else
     (This : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Others_Choice;
   
   ------------------------------------
   -- Generate_Parameter_Association --
   ------------------------------------
   
   procedure Generate_Parameter_Association
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Parameter_Association;
   
   -----------------------------------
   -- Generate_Qualified_Expression --
   -----------------------------------
   
   procedure Generate_Qualified_Expression
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob   : Output_Buffer := This.Get_Output_Buffer;
      --  Mark : Node_Id;
   begin
      --  Mark := Subtype_Mark (Node);
      --  Write_Id (Ob, Mark);
      --  Write_Str (Ob, " (");
      Generate_Node (This, Expression (Node));
      --  Write_Str (Ob, ")");
   end Generate_Qualified_Expression;
   
   ------------------------------------
   -- Generate_Quantified_Expression --
   ------------------------------------
   
   procedure Generate_Quantified_Expression
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Quantified_Expression;
   
   --------------------
   -- Generate_Range --
   --------------------
   
   procedure Generate_Range
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Range;
   
   ---------------------------
   -- Generate_Real_Literal --
   ---------------------------
   
   procedure Generate_Real_Literal
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
      Val : Ureal;
      Denom : int;
      Num   : Int;
      Vfloat : Float;
   begin
      Val := Realval (Node);
      Denom := Ui_To_Int (Norm_Den (Val));
      Num   := Ui_To_Int (Norm_Num (Val));
      if Denom /= 0 then
	 Vfloat := Float (Num) / Float (Denom);
      else
	 Vfloat := Float'Last;
      end if;
      Write_Str (Ob, Float_To_String (Vfloat));
   end Generate_Real_Literal;
   
   ------------------------
   -- Generate_Reference --
   ------------------------
   
   procedure Generate_Reference
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      if Nkind (Prefix (Node)) = N_Function_Call then
	 Error_Msg_N ("unsupported kind of function call", Node);
	 
      elsif Nkind (Prefix (Node)) = N_Procedure_Call_Statement then
	 Error_Msg_N ("unsupported kind of procedure call", Node);
	 
      else      
	 Generate_Node (This, Prefix (Node));
	 Write_Str (Ob, "'Access");
      end if;
   end Generate_Reference;
   
   --------------------
   -- Generate_Slice --
   --------------------
   
   procedure Generate_Slice
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Prefx : Node_Id;
      Rng   : Node_Id;
   begin
      Put_Line ("Generate_Slice Begin");
      
      Prefx := Prefix (Node);
      Rng   := Discrete_Range (Node);
      if Present (Prefx) then
	 Put_Line (" Generate_Slice Prefix " & Nkind (Prefix (Node))'Img);
      end if;
      
      Generate_Node (This, Rng);
      Put_Line ("Generate_Slice End");
   end Generate_Slice;
   
   -----------------------------
   -- Generate_String_Literal --
   -----------------------------
   
   procedure Generate_String_Literal
     (This : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Glips.Gen.Ch3.Generate_Subtype_Mark (This, Subtype_Mark (Node));
      Write_Str (Ob, " (");
      Generate_Node (This, Expression (Node));
      Write_Str (Ob, ")");
   end Generate_Type_Conversion;
   
   -----------------------------------
   -- Generate_Unchecked_Expression --
   -----------------------------------
   
   procedure Generate_Unchecked_Expression
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      Generate_Node (This, Expression (Node));
   end Generate_Unchecked_Expression;
   
   ----------------------------------------
   -- Generate_Unchecked_Type_Conversion --
   ----------------------------------------
   
   procedure Generate_Unchecked_Type_Conversion
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob   : Output_Buffer := This.Get_Output_Buffer;
      Mark : Node_Id;
   begin
      Mark := Subtype_Mark (Node);
      Write_Id (Ob, Mark);
      Write_Str (Ob, " (");
      Generate_Node (This, Expression (Node));
      Write_Str (Ob, ") ");
   end Generate_Unchecked_Type_Conversion;

   -------------------
   -- Generate_Call --
   -------------------

   procedure Generate_Call
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob     : Output_Buffer := This.Get_Output_Buffer;
      Call   : Entity_Id;
      Formal : Entity_Id;
      Actual : Node_Id;
      Len    : Natural;
      Count  : Natural;
   begin
      Call   := Entity (Name (Node));
      Formal := First_Formal_With_Extras (Call);
      
      Write_Indent (Ob);
      Generate_Node (This, Call);
      
      Count  := 0;
      Len    := 0;
      Formal := First_Formal_With_Extras (Call);
      
      while Present (Formal) loop
	 declare
	    S : String := Get_Name_String (Chars (Formal));
	 begin
	    if S'Length > Len then
	       Len := S'Length;
	    end if;
	 end;
	 Count := Count + 1;
	 Next_Formal_With_Extras (Formal);
      end loop;
      
      if Count = 0 then
	 return;
      end if;
      
      if Count = 1 then
	 Write_Str (Ob, " (");
      else
	 Write_Eol (Ob);
	 Indent_Begin (Ob);
      end if;
      
      Formal := First_Formal_With_Extras (Call);
      Actual := First_Actual (Node);
      while Present (Actual) loop
	 if Present (Formal) then
	    
	    --  Emit Format "Formal  => "
	    
	    declare
	       S      : String := Get_Name_String (Chars (Formal));
	       Spaces : Natural;
	    begin
	       Spaces := Len - S'Length;
	       if First_Formal_With_Extras (Call) /= Formal then
		  Write_Indent_Str (Ob, " ");
	       else
		  Write_Indent_Str (Ob, "(");
	       end if;
	       
	       Write_Id (Ob, Formal);
	       
	       for I in 1..Spaces loop
		  Write_Char (Ob, ' ');
	       end loop;
	       
	       Write_Str (Ob, " => ");
	    end;
	    
	    --  Now emit actual
	    
	    Generate_Node (This, Actual);
	    
	    Next_Formal_With_Extras (Formal);
	 else
	    Generate_Node (This, Actual);
	 end if;
	 
	 Next_Actual (Actual);
	 exit when No (Actual);
	 
	 Write_Str (Ob, ", ");
	 Write_Eol (Ob);
      end loop;
      
      Write_Char (Ob, ')');
      
      if Count /= 1 then
	 Indent_End (Ob);
      end if;
      
   end Generate_Call;
   
end Glips.Gen.Ch4;
