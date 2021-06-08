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
with Reflex.Infos; use Reflex.Infos;
with Reflex.Gen.Types; use Reflex.Gen.Types;
with Reflex.Gen.Outputs; use Reflex.Gen.Outputs;
with Reflex.Gen.Utils; use Reflex.Gen.Utils;
with Unity.Gen.Attrs; use Unity.Gen.Attrs;
with Reflex.Predicates; use Reflex.Predicates;
with Unity.Gen.Ch5; use Unity.Gen.Ch5;

with Reflex.Formats;

package body Unity.Gen.Ch4 is
   
   ------------------
   -- Generate_Sum --
   ------------------

   procedure Generate_Sum
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
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
     (This          : access Unity_Generator_Record;
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
   
   -------------------------------------
   -- Generate_Array_Aggregate_Values --
   -------------------------------------
   
   procedure Generate_Array_Aggregate_Values
     (This : access Unity_Generator_Record;
      N    : Node_Id) is
      
      Ob         : Output_Buffer := This.Get_Output_Buffer;
      Assoc_List : List_Id;
      Assoc      : Node_Id;
      Choice     : Node_Id;
      Expr       : Node_Id;
      Aggr       : Node_Id;
      Ancest     : Node_Id;
   begin
      pragma Assert (Nkind (N) = N_Aggregate);
      
   end Generate_Array_Aggregate_Values;
   
   --------------------------------------
   -- Generate_Record_Aggregate_Values --
   --------------------------------------
   
   procedure Generate_Record_Aggregate_Values
     (This : access Unity_Generator_Record;
      N    : Node_Id) is
      
      Ob         : Output_Buffer := This.Get_Output_Buffer;
      Assoc_List : List_Id;
      Assoc      : Node_Id;
      Choice     : Node_Id;
      Expr       : Node_Id;
      Aggr       : Node_Id;
      Ancest     : Node_Id;
   begin
      --  There two exclusives cases, the first one is when the record variable
      --  is initialzed by the mean of an aggregate expression and the second
      --  one is when some compoment of the record are initialized on the
      --  type declaration. In the second case, we must recurse on component
      --  which are record. All initializations are staic in reflex, so all of
      --  them can be initialized at the declaration and there is no need 
      --  to create an init procedure.
      
      --  First case : the record is initialized by an aggregate expression. 
      --  Walk througth aggregate and generate initialization for each field
      
      --  Second case : Here there is no aggregate expression to initialize the
      --  record. We must look to the type definition to see if component are
      --  directly initialized in the type. The walk recurse on compoment whose
      --  type is a record.
     
      pragma Assert
	(Nkind (N) = N_Aggregate or else Nkind (N) = N_Extension_Aggregate);
      
      if Nkind (N) = N_Extension_Aggregate then
	 Ancest := Designate_Aggregate (Ancestor_Part (N));
	 if Present (Ancest) then
	    Write_Indent_Str (Ob, "<instanceElementDesc name=""");
	    Write_Str (Ob, "_parent");
	    Write_Str (Ob, """>");
	    Write_Eol (Ob);
	    Indent_Begin (Ob);
	    
	    Generate_Record_Aggregate_Values (This, Ancest);
	    
	    Indent_End (Ob);
	    Write_Indent_Str (Ob, "</instanceElementDesc>");
	    Write_Eol (Ob);
	 end if;
      end if;
	 
      Assoc_List := Component_Associations (N);
      
      --  The component association list is not empty, Semantic has verified it
      
      pragma Assert (Present (Assoc_List));
      
      Assoc := First (Assoc_List);
      while Present (Assoc) loop
	 --  Semantic generate individual component association for all field 
	 --  of the record aggregate, so there is at least one choice.
	 
	 pragma Assert (Present (Choices (Assoc)));
	 
	 Choice := First (Choices (Assoc));
	 
	 --  Here we know that there is only one association choice as 
	 --  Semantic has rewritten the N_Aggregate or N_Extension_Aggregate
	 
	 pragma Assert (No (Next (Choice)));
	 
	 Expr := Expression (Assoc);
	 
	 Write_Indent_Str (Ob, "<instanceElementDesc name=""");
	 Write_Id (Ob, Choice);
	 Write_Str (Ob, """>");
	 Write_Eol (Ob);
	 Indent_Begin (Ob);
	 
	 --  Dispatch regarding if expression is an Array Aggregate or a Record
	 --  Aggregate or an others Type of Expression
	 
	 Aggr := Designate_Aggregate (Expr);
	 if Present (Aggr) then	 
	    if Is_Record_Type (Etype (Aggr)) then
	       Generate_Record_Aggregate_Values (This, Aggr);
	    elsif Is_Array_Type (Etype (Aggr)) then
	       null;
	    else
--	       pragma Assert (Is_Array_Type (Etype (Aggr)));
	       null;
	    end if;
	    
	 else
	    Write_Indent_Str (Ob, "<value>");
	    if Compile_Time_Known_Value (Expr) then
	       declare
		  Val : Integer;
	       begin
		  Val := Integer (Ui_To_Int (Expr_Value (Expr)));
		  Write_Int (Ob , Val);
	       end;
	    else
	       Generate_Node (This, Expr);
	    end if;
	    Write_Indent_Str (Ob, "</value>");
	    Write_Eol (Ob);
	 end if;
	 
	 Indent_End (Ob);
	 Write_Indent_Str (Ob, "</instanceElementDesc>");
	 Write_Eol (Ob);
	 
	Next (Assoc);
      end loop;
      
   exception
      when others =>
	 Put_Line ("Generate_Record_Aggregate_Values exception");
   end Generate_Record_Aggregate_Values;
   
   -----------------------------------
   -- Generate_Aggregate_Expression --
   -----------------------------------
   
   procedure Generate_Aggregate
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob    : Output_Buffer := This.Get_Output_Buffer;
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
      
      --  pragma Assert (Nkind (Node) = N_Aggregate 
      --  		       or Else Nkind (Node) = N_Extension_Aggregate);
      
      null;
   end Generate_Aggregate;

   ----------------------------------------------
   -- Generate_Record_Aggregate_In_Declaration --
   ----------------------------------------------
   
   procedure Generate_Record_Aggregate_In_Declaration
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob    : Output_Buffer := This.Get_Output_Buffer;
      Assos : List_Id;
   begin
      pragma Assert (Nkind (Node) = N_Aggregate);
      
      Assos := Component_Associations (Node);
      
      pragma Assert (Present (Component_Associations (Node)));
      
      declare
	 Nd : Node_Id;
      begin
	 Nd := First (Component_Associations (Node));
	 
	 while Present (Nd) loop
	    Indent_Begin (Ob);
	    Generate_Record_Component_Association_In_Declaration (This, Nd);
	    Indent_End (Ob);
	    Next (Nd);
	 end loop;
      end;
   end Generate_Record_Aggregate_In_Declaration;

   -----------------------
   -- Generate_And_Then --
   -----------------------

   procedure Generate_And_Then
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      Generate_Attribute (This, Node);
   end Generate_Attribute_Reference;
   
   ------------------------------
   -- Generate_Case_Expression --
   ------------------------------
   
   procedure Generate_Case_Expression
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Case_Expression;
   
   ------------------------------------------
   -- Generate_Case_Expression_Alternative --
   ------------------------------------------
   
   procedure Generate_Case_Expression_Alternative
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Case_Expression_Alternative;
   
   ------------------------------------
   -- Generate_Component_Association --
   ------------------------------------
   
   procedure Generate_Component_Association
     (This : access Unity_Generator_Record;
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
   
   ----------------------------------------------------------
   -- Generate_Record_Component_Association_In_Declaration --
   ----------------------------------------------------------
   
   procedure Generate_Record_Component_Association_In_Declaration
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob          : Output_Buffer := This.Get_Output_Buffer;
      Choice_List : List_Id;
      Choice      : Node_Id;
      Comp_Id     : Entity_Id;
      Expr        : Node_Id;
   begin
      Expr := Expression (Node);	    
      
      pragma Assert (Present (Expr));
      pragma Assert (not Present (Loop_Actions (Node)));
      
      Choice_List := Choices (Node);
      if Choice_List /= No_List then
	 Choice := First (Choice_List);
	 while Present (Choice) loop
	    pragma Assert (Nkind (Choice) /= N_Others_Choice);
	    
	    Comp_Id := Entity (Choice);
	    
	    Write_Indent_Str (Ob, "<instanceElementDesc name=""");
	    Write_Id (Ob, Comp_Id);
	    Write_Str (Ob, """>");
	    Write_Eol (Ob);
	       
	    Indent_Begin (Ob);
	    Write_Indent_Str (Ob, "<value>");
	    if Compile_Time_Known_Value (Expr) then
	       declare
		  Val : Integer;
	       begin
		  Val := Integer (Ui_To_Int (Expr_Value (Expr)));
		  Write_Int (Ob , Val);
	       end;
	    else
	       Generate_Node (This, Expr);
	    end if;
	    
	    Write_Str (Ob, "</value>");
	    Write_Eol (Ob);
	    Indent_End (Ob);
	    
	    Write_Indent_Str (Ob, "</instanceElementDesc>");
	    Write_Eol (Ob);
	    
	    Next (Choice);
	 end loop;
      end if;
   end Generate_Record_Component_Association_In_Declaration;

   ----------------------------------------
   -- Generate_Component_Association_Old --
   ----------------------------------------
   
   procedure Generate_Component_Association_Old
     (This : access Unity_Generator_Record;
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
   end Generate_Component_Association_Old;
   
   ---------------------------------------
   -- Generate_Discriminant_Association --
   ---------------------------------------
   
   procedure Generate_Discriminant_Association
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Discriminant_Association;
   
   -----------------------------------
   -- Generate_Explicit_Dereference --
   -----------------------------------
   
   procedure Generate_Explicit_Dereference
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Expression_Function;
   
   ----------------------------------------
   -- Generate_Extended_Return_Statement --
   ----------------------------------------
   
   procedure Generate_Extended_Return_Statement
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Extended_Return_Statement;
   
   ----------------------------------
   -- Generate_Extension_Aggregate --
   ----------------------------------
   
   procedure Generate_Extension_Aggregate
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Extension_Aggregate;
   
   ----------------------------
   -- Generate_Function_Call --
   ----------------------------
   
   procedure Generate_Function_Call
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      Call_Entity : Entity_Id;
   begin
      Call_Entity := Entity (Name (Node));
      Unity.Gen.Ch5.Generate_Procedure_Call_Statement
	(This, Node, Get_Internal_Function (Call_Entity));
   end Generate_Function_Call;
   
   ----------------------------
   -- Generate_If_Expression --
   ----------------------------
   
   procedure Generate_If_Expression
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_If_Expression;
   
   -----------------
   -- Generate_In --
   -----------------
   
   procedure Generate_In
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_In;
   
   --------------------------------
   -- Generate_Indexed_Component --
   --------------------------------
   
   procedure Generate_Indexed_Component
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Exprs : List_Id;
      Expr  : Node_Id;
      Ob    : Output_Buffer := This.Get_Output_Buffer;
      
   begin
      Generate_Node (This, Prefix (Node));
      
      Write_Str (Ob, "[");
      
      Exprs := Expressions (Node);
      Expr := First (Exprs);
      while Present (Expr) loop
	 Generate_Node (This, Expr);
	 Next (Expr);
	 if Present (Expr) then
	   Write_Str (Ob, ", ");
	 end if;
      end loop;
      
      Write_Str (Ob, "]");
   end Generate_Indexed_Component;
   
   ------------------------------
   -- Generate_Integer_Literal --
   ------------------------------
   
   procedure Generate_Integer_Literal
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Mod_Clause;
   
   ---------------------
   -- Generate_Not_In --
   ---------------------
   
   procedure Generate_Not_In
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Not_In;
   
   ---------------------
   -- Generate_Op_Abs --
   ---------------------
   
   procedure Generate_Op_Abs
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      raise Program_Error; -- should always be expanded
   end Generate_Op_Concat;
   
   ------------------------
   -- Generate_Op_Divide --
   ------------------------
   
   procedure Generate_Op_Divide
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
      --  LHS   : constant Node_Id := Left_Opnd (Node);
      --  L_Typ : constant Node_Id := Get_Type_Full_View (Etype (LHS));
      --  RHS   : constant Node_Id := Right_Opnd (Node);
      --  R_Typ : constant Node_Id := Get_Type_Full_View (Etype (RHS));
   begin
      --  if Has_Fat_Pointer (L_Typ) or else Has_Fat_Pointer (R_Typ) then
      --  	 Write_Str (" not ");
      --  	 Write_Fatptr_Compare (LHS, RHS);
	 
      --  else
	 Generate_Left_Opnd (This, Node);
	 Write_Str (Ob, " <> ");
	 Generate_Right_Opnd (This, Node);
      --  end if;
   end Generate_Op_Ne;
   
   ---------------------
   -- Generate_Op_Not --
   ---------------------
   
   procedure Generate_Op_Not
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Op_Rotate_Left;
   
   ------------------------------
   -- Generate_Op_Rotate_Right --
   ------------------------------
   
   procedure Generate_Op_Rotate_Right
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Op_Rotate_Right;
   
   -----------------------------
   -- Generate_Op_Shift_Right --
   -----------------------------
   
   procedure Generate_Op_Shift_Right
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Op_Shift_Right;
   
   ----------------------------------------
   -- Generate_Op_Shift_Right_Arithmetic --
   ----------------------------------------
   
   procedure Generate_Op_Shift_Right_Arithmetic
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Op_Shift_Right_Arithmetic;
   
   ----------------------------
   -- Generate_Op_Shift_Left --
   ----------------------------
   
   procedure Generate_Op_Shift_Left
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Op_Shift_Left;
   
   --------------------------
   -- Generate_Op_Subtract --
   --------------------------
   
   procedure Generate_Op_Subtract
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Operator_Symbol;
   
   ----------------------
   -- Generate_Or_Else --
   ----------------------
   
   procedure Generate_Or_Else
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Others_Choice;
   
   ------------------------------------
   -- Generate_Parameter_Association --
   ------------------------------------
   
   procedure Generate_Parameter_Association
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Parameter_Association;
   
   -----------------------------------
   -- Generate_Qualified_Expression --
   -----------------------------------
   
   procedure Generate_Qualified_Expression
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Qualified_Expression;
   
   ------------------------------------
   -- Generate_Quantified_Expression --
   ------------------------------------
   
   procedure Generate_Quantified_Expression
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Quantified_Expression;
   
   --------------------
   -- Generate_Range --
   --------------------
   
   procedure Generate_Range
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Range;
   
   ---------------------------
   -- Generate_Real_Literal --
   ---------------------------
   
   procedure Generate_Real_Literal
     (This : access Unity_Generator_Record;
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
	 -- Write_Ureal_Col_Check (Ob, Realval (Node));
   end Generate_Real_Literal;
   
   ------------------------
   -- Generate_Reference --
   ------------------------
   
   procedure Generate_Reference
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Slice;
   
   -----------------------------
   -- Generate_String_Literal --
   -----------------------------
   
   procedure Generate_String_Literal
     (This : access Unity_Generator_Record;
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
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Type_Conversion;
   
   -----------------------------------
   -- Generate_Unchecked_Expression --
   -----------------------------------
   
   procedure Generate_Unchecked_Expression
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Unchecked_Expression;
   
   ----------------------------------------
   -- Generate_Unchecked_Type_Conversion --
   ----------------------------------------
   
   procedure Generate_Unchecked_Type_Conversion
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Unchecked_Type_Conversion;

end Unity.Gen.Ch4;
