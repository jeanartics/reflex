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
with Exp_Util; use Exp_Util;
with Sinfo; use Sinfo;
with Namet; use Namet;
with Nlists; use Nlists;
with Nmake; use Nmake;
with Stand; use Stand;
with Sem_Util; use Sem_Util;
with Sem_Eval; use Sem_Eval;
with Tbuild; use Tbuild;
with Types; use Types;
with Stringt; use Stringt;
with Uintp; use Uintp;
with Urealp; use Urealp;

with Reflex.Expanders.Types; use Reflex.Expanders.Types;
with Reflex.Expanders.Utils; use Reflex.Expanders.Utils;
with Reflex.Expanders.Dispatch; use Reflex.Expanders.Dispatch;
with Reflex.Expanders.Supports; use Reflex.Expanders.Supports;
with Reflex.Expanders.Ch6; use Reflex.Expanders.Ch6;
with Reflex.Boxes.Exp_Ch4; use Reflex.Boxes.Exp_Ch4;
with Reflex_Options; use Reflex_Options;
with Reflex.External_Names; use Reflex.External_Names;
with Reflex.Predicates; use Reflex.Predicates;
with Reflex.Global_Arecs; use Reflex.Global_Arecs;
wIth Unity_Standard_Lib; use Unity_Standard_Lib;

--with Exp_Ch6; use Exp_Ch6;

package body Reflex.Expanders.Ch4 is
   
   ------------------
   -- Expand_Sum --
   ------------------

   procedure Expand_Sum
     (This : access Reflex_Expander_Record;
      Val1 : Node_Id; 
      Val2 : Uint; 
      B    : Boolean) is
      
      Modular : constant Boolean := Is_Modular_Integer_Type (Etype (Val1));
   begin
      if Compile_Time_Known_Value (Val1) then
         null; -- Write_Uint (Ob, Expr_Value (Val1) + Val2, Modular => Modular);

      elsif Val2 = 0 then
         Expand_Node (This, Val1);

      elsif B then
         Expand_Node (This, Val1);
         --  Write_Str_Col_Check (Ob, " + ");
         --  Write_Uint (Ob, Val2, Modular => Modular);

      else
         Expand_Node (This, Val1);
         --  Write_Str_Col_Check (Ob, " + ");
         --  Write_Uint (Ob, Val2, Modular => Modular);
      end if;
   end Expand_Sum;
   
   -------------------------
   -- Expand_Difference --
   -------------------------

   procedure Expand_Difference
     (This : access Reflex_Expander_Record;
      Val1 : Node_Id; 
      Val2 : Uint; 
      B    : Boolean) is
      
      Modular : constant Boolean := Is_Modular_Integer_Type (Etype (Val1));
   begin
      if Compile_Time_Known_Value (Val1) then
         null; -- Write_Uint (Ob, Expr_Value (Val1) - Val2, Modular => Modular);
	 
      elsif Val2 = Uint_0 then
         Expand_Node (This, Val1);

      elsif B then
         Expand_Node (This, Val1);
         --  Write_Str_Col_Check (Ob, " - ");
         --  Write_Uint (Ob, Val2, Modular => Modular);

      else
         Expand_Node (This, Val1);
         --  Write_Str_Col_Check (Ob, " - ");
         --  Write_Uint (Ob, Val2, Modular => Modular);
      end if;
   end Expand_Difference;

   procedure Expand_Difference
     (This          : access Reflex_Expander_Record;
      Val1          : Node_Id;
      Val2          : Node_Id;
      Minus_One_Min : Boolean)
   is
   begin
      if Compile_Time_Known_Value (Val2) then
         Expand_Difference (This, Val1, Expr_Value (Val2), Minus_One_Min);

      elsif Is_Entity_Name (Val1) and then Is_Entity_Name (Val2)
        and then Entity (Val1) = Entity (Val2)
      then
         null;

      else
         --  When Minus_One_Min is True, then Expand safeguard:

         --  (Val1 < Val2 ? -1 : Val1 - Val2)

         --  Note that we rely on the front end to remove side effects by
         --  stabilizing values into temporaries, so we do not need to worry
         --  about side effects here.

         if Minus_One_Min then
            --  Declare result in scope
	    
            --  Write_Str_Col_Check (Ob, "if ");
            --  Expand_Node (This, Val1);
            --  Write_Str_Col_Check (Ob, " < ");
            --  Expand_Node (This, Val2);
            --  Write_Str_Col_Check (Ob, "then");
            --  Write_Eol (Ob);
            --  Write_Indent_Str (Ob, "tmp := -1");
            --  Write_Eol (Ob);
            --  Write_Indent_Str (Ob, "else");
            --  Write_Eol (Ob);
            null;
         end if;

         Expand_Node (This, Val1);
         --  Write_Str_Col_Check (Ob, " - ");

         --  Add parens around expression if needed

         if Nkind_In (Val2, N_Identifier, N_Expanded_Name) then
            Expand_Node (This, Val2);
         else
            --  Write_Str_Col_Check (Ob, "(");
            Expand_Node (This, Val2);
            --  Write_Str_Col_Check (Ob, ")");
         end if;

         if Minus_One_Min then
            null;
            --  Write_Eol (Ob);
            --  Write_Indent_Str (Ob, "end if;");
         end if;
      end if;
   end Expand_Difference;
   
   ----------------------
   -- Handle_Attribute --
   ----------------------

   procedure Handle_Attribute (N : Node_Id) is
   begin
      null;
   end Handle_Attribute;

   ------------------------
   -- Expand_Aggregate --
   ------------------------
   
   procedure Expand_Aggregate
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
   begin
      return;
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

      --  if not in object declaration create a var and replace it
      --  else nothing to
      
      --  In declartion an aggregate can appear only as initialization of an
      --  object declaration or a component declaration
      
--        Aggr := Directly_Designate_Aggregate (Node);
--        if Present (Aggr) then
--  	
--  	 T := Etype (Node);
--  	 
--  	 Cst := Make_Defining_Identifier
--  	   (Sloc  => Sloc (T),
--  	    Chars => New_Variable_Name (Chars (T)));
--  	 
--  	 Set_Etype (Cst, T);
--  	 Set_Scope (Cst, Scope (T));
--  	 Set_Ekind (Cst, E_Variable);
--  	 
--  	 --  Link Entities
--  	 
--  	 Set_Next_Entity (Cst, Next_Entity (T));
--  	 Set_Next_Entity (T, Cst);
--  	 
--  	 --  Preserve Rhs
--  	 
--  	 Sav_Node := New_Node (Nkind (Node), Sloc (Node));
--  	 Copy_Node (Node, Sav_Node);
--  	 
--  	 Replace (Node, New_Occurrence_Of (Cst, Sloc (Node)));
--  	 
--  	 Aggr_Cst := Make_Object_Declaration
--  	   (Sloc                => Sloc (Node),
--  	    Defining_Identifier => Cst,
--  	    Constant_Present    => True,
--  	    Object_Definition   => New_Occurrence_Of (T, Sloc (Node)),
--  	    Expression          => New_Occurrence_Of (Sav_Node, Sloc (Node)));
--  	 
--  	 Insert_After (T, Aggr_Cst);
--        end if;
--        
--     exception
--        when others => 
--  	 Put_Line ("Exception //===============> Expand_Aggregate");
   end Expand_Aggregate;

   -----------------------
   -- Expand_And_Then --
   -----------------------

   procedure Expand_And_Then
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
   begin
      Expand_Short_Circuit_Operator (This, Node);
      --  Expand_Left_Opnd (This, Node);
      --  Expand_Right_Opnd (This, Node);
   end Expand_And_Then;
   
   ----------------------------------
   -- Expand_Attributae_Reference --
   ----------------------------------
   
   procedure Expand_Attribute_Reference
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Attribute_Reference;
   
   ------------------------------
   -- Expand_Case_Expression --
   ------------------------------
   
   procedure Expand_Case_Expression
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Case_Expression;
   
   ------------------------------------------
   -- Expand_Case_Expression_Alternative --
   ------------------------------------------
   
   procedure Expand_Case_Expression_Alternative
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Case_Expression_Alternative;
   
   ------------------------------------
   -- Expand_Component_Association --
   ------------------------------------
   
   procedure Expand_Component_Association
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
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
               null;
            else
               Expand_Node (This, Choice);
               if Present (Next (Choice)) then
                  null;
               end if;
            end if;
            Next (Choice);
         end loop;
         Expand_Node (This, Expression (Node));
      end if;
   end Expand_Component_Association;
   
   ---------------------------------------
   -- Expand_Discriminant_Association --
   ---------------------------------------
   
   procedure Expand_Discriminant_Association
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Discriminant_Association;
   
   -----------------------------------
   -- Expand_Explicit_Dereference --
   -----------------------------------
   
   procedure Expand_Explicit_Dereference
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      Expand_Node (This, Prefix (Node));
   end Expand_Explicit_Dereference;
   
   --------------------------------------
   -- Expand_Expression_With_Actions --
   --------------------------------------
   
   procedure Expand_Expression_With_Actions
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Acts        : List_Id;
      Act         : Node_Id;
      Expr        : Node_Id;
      Nxt         : Node_Id;
      Assign      : Node_Id;
      Insert_Node : Node_Id;
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

      --  Note: In the final Expandd tree presented to the code generator,
      --  the actions list is always non-null, since there is no point in this
      --  node if the actions are Empty. During semantic analysis there are
      --  cases where it is convenient to temporarily Expand an empty actions
      --  list. This arises in cases where we create such an empty actions
      --  list, and it may or may not end up being a place where additional
      --  actions are inserted. The expander removes such empty cases after
      --  the expression of the node is fully analyzed and expanded, at which
      --  point it is safe to remove it, since no more actions can be inserted.

      --  Note: In Modify_Tree_For_C, we never Expand any declarations in
      --  the action list, which can contain only non-declarative statements.
      
      Insert_Node := Search_Insertion_Node (Node);
      
      Acts := Actions (Node);
      if Present (Acts) then
         Act := First (Acts);
         while Present (Act) loop
            Nxt := Next (Act);
            if Nkind (Act) = N_Object_Declaration then
               Expr := Expression (Act);
	       
               Set_Expression (Act, Empty);
               Expand_Node (This, Act);

               if Present (Expr) then
                  Expand_Node (This, Expr);
                  Assign := Make_Assignment_Statement
                    (Sloc (Act),
                     Name       => 
                       New_Occurrence_Of
                         (Defining_Identifier (Act), Sloc (Act)),
                     Expression => Expr);
		  
                  Insert_Before (Insert_Node, Assign);
               end if;
	       
               Remove (Act);
               This.Declare_Current_Scope (Act);
	       
            elsif Nkind_In (Act,  
                            N_Full_Type_Declaration,
                            N_Subtype_Declaration) 
            then
               Expand_Node (This, Act);
               Remove (Act);
               This.Declare_Current_Scope (Act);
            else	    
               Expand_Node (This, Act);
               Remove (Act);
               Insert_Before (Insert_Node, Act);
            end if;
	    
            Act := Nxt;
         end loop;
      end if;
      
      Expr := Expression (Node);
      if Present (Expr) then
         Expand_Node (This, Expr);
         -- Replace (Insert_Node, Expr);
      else
         Remove (Node);
      end if;
   exception
      when others =>
         Put_Line ("Exception in Expand_Expression_With_Actions");
   end Expand_Expression_With_Actions;
   
   ----------------------------------
   -- Expand_Expression_Function --
   ----------------------------------
   
   procedure Expand_Expression_Function
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Expression_Function;
   
   ----------------------------------------
   -- Expand_Extended_Return_Statement --
   ----------------------------------------
   
   procedure Expand_Extended_Return_Statement
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Extended_Return_Statement;
   
   ----------------------------------
   -- Expand_Extension_Aggregate --
   ----------------------------------
   
   procedure Expand_Extension_Aggregate
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Extension_Aggregate;
   
   ---------------------------------
   -- Remove_Function_Call_Effect --
   ---------------------------------
   
   Function_As_Procedure : Boolean := True;
   procedure Remove_Function_Call_Effect (Call : Node_Id) is
      
      Call_Entity : Entity_Id;
      Result_Type : Entity_Id;
      Result      : Node_Id;
      Assign      : Node_Id;
      Insert_Node : Node_Id;
      Ref         : Node_Id;
      New_Call    : Node_Id;
      Par         : Node_Id;
      Formal      : Entity_Id;
      Lhs         : Node_Id;
   begin
      Put_Line ("Remove_Function_Call_Effect Begin");
      Call_Entity := Entity (Name (Call));
      Result_Type := Etype (Call_Entity);
      
      Put_Line
	("Remove_Function_Call_Effect " & Get_String (Chars (Call_Entity)));
      Put_Line ("Result_Type " & Get_Name_String (Chars (Result_Type)));
      --  Create a varibale V of type of the function Result
      
      if Get_Internal_Function (Call_Entity) then
	 Put_Line ("Remove_Function_Call_Effect (Internal) End");
	 return;
      end if;
      
      if Is_Inlined (Call_Entity) then
	 declare
	    Spec : constant Node_Id := Unit_Declaration_Node (Call_Entity);
	 begin
	    if No (Spec)
	      or else Nkind (Spec) /= N_Subprogram_Declaration
	      or else No (Body_To_Inline (Spec))
	    then
	       null;
	    else
	       Expand_Inlined_Call (Call, Call_Entity, Call_Entity);
	       return;
	    end if;
	 end;
      end if;
      
      if Function_As_Procedure then
	 
	 New_Call := New_Copy_Tree (Call);
	 Formal := Get_Function_Result_Formal (Call_Entity);
	 
	 pragma Assert (Present (Formal));
	 
	 Par := Parent (Call);
	 Put_Line ("Par Kind " & Nkind (Par)'Img);
	 if Nkind (Par) = N_Assignment_Statement then
	    Put_Line ("Proc Assign");
	    
	    --  Append Extra parameter for Result
	    
	    Put_Line ("Nkind Name (par)  " & Nkind (Name (Par))'Img);
	    Lhs := Name (Par);
	    if Is_Entity_Name (Lhs) then
	       Put_Line ("" & Get_String (Chars (Lhs)));
	    
	       Add_Extra_Actual_To_Call
		 (New_Call, 
		  Formal, 
		  New_Occurrence_Of (Entity (Lhs), Sloc (Lhs)));
	       
	       Rewrite (Par, New_Call);
	    else
	       Put_Line ("Raise Program_Error");
	       raise Program_Error;
	    end if;
	 else
	    Put_Line ("Proc No Assign");
	    
	    Result := Create_Expanded_Variable
	      (Call, Result_Type, Chars (Call_Entity));
	    
	    Declare_Body_Scope (Call, Result);
	    
	    Add_Extra_Actual_To_Call
	      (New_Call, Formal, 
	       New_Occurrence_Of (Defining_Identifier (Result), Sloc (Call)));
	    
	    Insert_Node := Search_Insertion_Node (Call);
	    Insert_Before (Insert_Node, New_Call);
	    
	    Ref := New_Occurrence_Of
	      (Defining_Identifier (Result), Sloc (Call));
	    Rewrite (Call, Ref);
	 end if;
	 
      else
	 Result := Create_Expanded_Variable
	   (Call, Result_Type, Chars (Call_Entity));
	 
	 Declare_Body_Scope (Call, Result);
	 
	 --  Create the affectation  V := F();
	 
	 Assign := Make_Assignment_Statement
	   (Sloc       => Sloc (Call),
	    Name       => 
	      New_Occurrence_Of (Defining_Identifier (Result), Sloc (Call)),
	    Expression => Empty);
	 
	 Insert_Node := Search_Insertion_Node (Call);
	 Insert_Before (Insert_Node, Assign);
	 
	 New_Call := New_Copy_Tree (Call);
	 Set_Expression (Assign, New_Call);
	 
	 Put_Line ("    call " & Nkind (Call)'Img);
	 Put_Line ("    call " & Get_String (Chars (Name (Call))));
	 Put_Line ("New_call " & Nkind (New_Call)'Img);
	 Put_Line ("New_call " & Get_String (Chars (Name (New_Call))));
	 
	 Ref := New_Occurrence_Of (Defining_Identifier (Result), Sloc (Call));
	 Rewrite (Call, Ref);
      end if;
      Put_Line ("Remove_Function_Call_Effect End");
   end Remove_Function_Call_Effect;
   
   ----------------------------
   -- Expand_Function_Call --
   ----------------------------
   
   procedure Expand_Function_Call
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Actual : Node_Id;
   begin
      Put_Line ("Expand_Function_Call Begin");
      --  If the parent of call is not an assignament then Remove the Call Side
      --  Effect
      
      Put_Line ("Before Actual");
      Actual := First_Actual (Node);
      while Present (Actual) loop
	 Expand_Node (This, Actual);
	 Next (Actual);
      end loop;
      Put_Line ("After Actual");
      
      if Nkind (Parent (Node)) /= N_Assignment_Statement then
	 Put_Line ("Remove_Function_Call_Effect going...");
	 Remove_Function_Call_Effect (Node);
      end if;
      
      Put_Line ("Expand_Function_Call End");
   end Expand_Function_Call;
   
   ----------------------------
   -- Expand_If_Expression --
   ----------------------------
   
   procedure Expand_If_Expression
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Loc         : constant Source_Ptr := Sloc (Node);
      Exprs       : List_Id;
      Cond        : Node_Id;
      ThenX       : Node_Id;
      Elsex       : Node_Id;
      Typ         : Entity_Id := Etype (Node);
      Cnn         : Node_Id;
      Decl        : Node_Id;
      New_If      : Node_Id;
      New_N       : Node_Id;
      Insert_Node : Node_Id;
   begin
      Exprs  := Expressions (Node);
      Cond   := First (Exprs);
      ThenX  := Next (Cond);
      Elsex  := Next (ThenX);
	  
      Expand_Node (This, Cond);
      if Compile_Time_Known_Value (Cond) then
         if Expr_Value (Cond) = Uint_1 then
            Expand_Node (This, ThenX);
            Rewrite (Node, ThenX);
            return;
         else
            Expand_Node (This, ElseX);
            Rewrite (Node, ElseX);
            return;
         end if;
      end if;
      
      --  Expand_Node (This, Thenx);
      --  Expand_Node (This, Elsex);
      
      --  Optimize simple construction like :
      --    a := if (b then c else d);
      --  Replace by :
      --    if b then a := c else a := d);
      
      --  Create Tempory and declare it
      
      Insert_Node := Search_Insertion_Node (Node);
      
      if Nkind (Insert_Node) = N_Assignment_Statement 
        and then Parent (Node) = Insert_Node 
      then
         Cnn := Entity (Name (Insert_Node));
         Decl := Empty;
      else
         -- Cnn := Make_Temporary (Loc, 'C', Node);
         --  Cnn := Make_Reuse_Entity
         --    (This, Loc, Name_Find ("c"), Standard_Boolean);
         Cnn := Search_Reuse_Entity
           (This, Name_Find ("c"), Standard_Boolean);
	 
         if No (Cnn) then
            Cnn := Make_Reuse_Entity
              (This, Loc, Name_Find ("c"), Standard_Boolean);
         end if;
	 
         Set_Ekind (Cnn, E_Variable);
         Set_Etype (Cnn, Standard_Boolean);
         Set_In_Use (Cnn, True);
	 
         Decl :=
           Make_Object_Declaration
             (Loc,
              Defining_Identifier => Cnn,
              Object_Definition   => New_Occurrence_Of (Typ, Loc));
      end if;
      
      --  Create New if statement
      
      New_If :=
        Make_If_Statement 
          (Loc,
           Condition       => Relocate_Node (Cond),
	 
           Then_Statements => 
             New_List
               (Make_Assignment_Statement
                  (Sloc (Thenx),
                   Name       => New_Occurrence_Of (Cnn, Sloc (Thenx)),
                   Expression => Relocate_Node (Thenx))),

           Else_Statements => 
             New_List
               (Make_Assignment_Statement
                  (Sloc (Elsex),
                   Name       => New_Occurrence_Of (Cnn, Sloc (Elsex)),
                   Expression => Relocate_Node (Elsex))));
      
      --  Replace N by a reference to newly created temporary
      
      New_N := New_Occurrence_Of (Cnn, Loc);
      
      if Present (Decl) then
         -- Declare_Enclosing_Subprogram (Node, Decl);
         Declare_Current_Scope (This, Decl);
      end if;
      
      Insert_Before (Insert_Node, New_If);
      if Present (Decl) then
         Rewrite (Node, New_N);
      else
         Remove (Insert_Node);
      end if;
      
      Expand_Node_List (This, Then_Statements (New_If));
      Expand_Node_List (This, Else_Statements (New_If));
      
      Set_In_Use (Cnn, False);
      
   exception
      when others =>
         Put_Line ("Expand_If_Expression =================> exception" );
   end Expand_If_Expression;
   
   -----------------
   -- Expand_In --
   -----------------
   
   procedure Expand_In
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_In;
   
   --------------------------------
   -- Expand_Indexed_Component --
   --------------------------------
   
   procedure Expand_Indexed_Component
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Indexed_Component;
   
   ------------------------------
   -- Expand_Integer_Literal --
   ------------------------------
   
   procedure Expand_Integer_Literal
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
   begin
      --  Write_Uint
      --  	(Ob,
      --  	 U       => Intval (Node),
      --  	 Modular => Is_Modular_Integer_Type (Etype (Node)));
      null;
   end Expand_Integer_Literal;
   
   -------------------------
   -- Expand_Mod_Clause --
   -------------------------
   
   procedure Expand_Mod_Clause
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Mod_Clause;
   
   ---------------------
   -- Expand_Not_In --
   ---------------------
   
   procedure Expand_Not_In
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Not_In;
   
   ---------------------
   -- Expand_Op_Abs --
   ---------------------
   
   procedure Expand_Op_Abs
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      Expand_Right_Opnd (This, Node);
   end Expand_Op_Abs;
   
   ---------------------
   -- Expand_Op_Add --
   ---------------------
   
   procedure Expand_Op_Add
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      Expand_Left_Opnd (This, Node);
      Expand_Right_Opnd (This, Node);
   end Expand_Op_Add;
   
   ---------------------
   -- Expand_Op_And --
   ---------------------
   
   procedure Expand_Op_And
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Lhs         : Node_Id;
      Rhs         : Node_Id;
      Left_Value  : Uint;
      Right_Value : Uint;
   begin
      Expand_Left_Opnd (This, Node);
      Expand_Right_Opnd (This, Node);
      
      Lhs := Left_Opnd (Node);
      Rhs := Right_Opnd (Node);
      if Compile_Time_Known_Value (Lhs) then
         Left_Value := Expr_Value (Lhs);
      else
         Left_Value := No_Uint;
      end if;
      
      if Compile_Time_Known_Value (Rhs) then
         Right_Value := Expr_Value (Rhs);
      else
         Right_Value := No_Uint;
      end if;
      
      --  If Left is True and Right is True replace Node by True
      
      if Left_Value = Uint_1 and Right_Value = Uint_1 then
         Rewrite (Node, New_Occurrence_Of (Standard_True, Sloc (Node)));
      
         --  If Left is False replace Node by False
      elsif Left_Value = Uint_0 then
         Rewrite (Node, New_Occurrence_Of (Standard_False, Sloc (Node)));
      
         --  If Right is False replace Node by False
      elsif Right_Value = Uint_0 then
         Rewrite (Node, New_Occurrence_Of (Standard_False, Sloc (Node)));
      
         --  If Left is True then replace node by Right
      elsif Left_Value = Uint_1 then
         Rewrite (Node, Rhs);
      
         --  If Right is True then replace node by Left
      elsif Right_Value = Uint_1 then
         Rewrite (Node, Lhs);
	 
      else
         null;
      end if;
   end Expand_Op_And;
   
   ----------------------
   -- Expand_Op_Concat --
   ----------------------
   
   procedure Expand_Op_Concat
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      raise Program_Error; -- should always be expanded
   end Expand_Op_Concat;
   
   ------------------------
   -- Expand_Op_Divide --
   ------------------------
   
   procedure Expand_Op_Divide
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      Expand_Left_Opnd (This, Node);
      Expand_Right_Opnd (This, Node);
   end Expand_Op_Divide;
   
   --------------------
   -- Expand_Op_Eq --
   --------------------
   
   procedure Expand_Op_Eq
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
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
      Expand_Left_Opnd (This, Node);
      Expand_Right_Opnd (This, Node);
      --  end if;
   end Expand_Op_Eq;
   
   -----------------------
   -- Expand_Op_Expon --
   -----------------------
   
   procedure Expand_Op_Expon
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      Expand_Left_Opnd (This, Node);
      Expand_Right_Opnd (This, Node);
   end Expand_Op_Expon;
   
   --------------------
   -- Expand_Op_Ge --
   --------------------
   
   procedure Expand_Op_Ge
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      Expand_Left_Opnd (This, Node);
      Expand_Right_Opnd (This, Node);
   end Expand_Op_Ge;
   
   --------------------
   -- Expand_Op_Gt --
   --------------------
   
   procedure Expand_Op_Gt
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      Expand_Left_Opnd (This, Node);
      Expand_Right_Opnd (This, Node);
   end Expand_Op_Gt;
   
   --------------------
   -- Expand_Op_Le --
   --------------------
   
   procedure Expand_Op_Le
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      Expand_Left_Opnd (This, Node);
      Expand_Right_Opnd (This, Node);
   end Expand_Op_Le;
   
   --------------------
   -- Expand_Op_Lt --
   --------------------
   
   procedure Expand_Op_Lt
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      Expand_Left_Opnd (This, Node);
      Expand_Right_Opnd (This, Node);
   end Expand_Op_Lt;
   
   -----------------------
   -- Expand_Op_Minus --
   -----------------------
   
   procedure Expand_Op_Minus
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      Expand_Right_Opnd (This, Node);
   end Expand_Op_Minus;
   
   ---------------------
   -- Expand_Op_Mod --
   ---------------------
   
   procedure Expand_Op_Mod
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      Expand_Left_Opnd (This, Node);
      Expand_Right_Opnd (This, Node);
   end Expand_Op_Mod;
   
   --------------------------
   -- Expand_Op_Multiply --
   --------------------------
   
   procedure Expand_Op_Multiply
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      Expand_Left_Opnd (This, Node);
      Expand_Right_Opnd (This, Node);
   end Expand_Op_Multiply;
   
   --------------------
   -- Expand_Op_Ne --
   --------------------
   
   procedure Expand_Op_Ne
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      --  LHS   : constant Node_Id := Left_Opnd (Node);
      --  L_Typ : constant Node_Id := Get_Type_Full_View (Etype (LHS));
      --  RHS   : constant Node_Id := Right_Opnd (Node);
      --  R_Typ : constant Node_Id := Get_Type_Full_View (Etype (RHS));
   begin
      --  if Has_Fat_Pointer (L_Typ) or else Has_Fat_Pointer (R_Typ) then
      --  	 Write_Str (" not ");
      --  	 Write_Fatptr_Compare (LHS, RHS);
	 
      --  else
      Expand_Left_Opnd (This, Node);
      Expand_Right_Opnd (This, Node);
      --  end if;
   end Expand_Op_Ne;
   
   ---------------------
   -- Expand_Op_Not --
   ---------------------
   
   procedure Expand_Op_Not
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Rhs         : Node_Id;
      Right_Value : Uint;
   begin
      Expand_Right_Opnd (This, Node);
      
      Rhs := Right_Opnd (Node);
      if Compile_Time_Known_Value (Rhs) then
         Right_Value := Expr_Value (Rhs);
      else
         Right_Value := No_Uint;
      end if;
      
      if Right_Value = Uint_0 then
         Rewrite (Node, New_Occurrence_Of (Standard_True, Sloc (Node)));
	 
      elsif Right_Value = Uint_1 then
         Rewrite (Node, New_Occurrence_Of (Standard_False, Sloc (Node)));
      else
         null;
      end if;
      
      if Reflex_Options.Ladder_Language then
         Expand_Op_Not_For_Ladder (This, Node);
      end if;

   end Expand_Op_Not;
   
   --------------------
   -- Expand_Op_Or --
   --------------------
   
   procedure Expand_Op_Or
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Lhs         : Node_Id;
      Rhs         : Node_Id;
      Left_Value  : Uint;
      Right_Value : Uint;
   begin
      Expand_Left_Opnd (This, Node);
      Expand_Right_Opnd (This, Node);
      
      Lhs := Left_Opnd (Node);
      Rhs := Right_Opnd (Node);
      if Compile_Time_Known_Value (Lhs) then
         Left_Value := Expr_Value (Lhs);
      else
         Left_Value := No_Uint;
      end if;
      
      if Compile_Time_Known_Value (Rhs) then
         Right_Value := Expr_Value (Rhs);
      else
         Right_Value := No_Uint;
      end if;

      --  If the two are false, replace node by false
      
      if Left_Value = Uint_0 and Right_Value = Uint_0 then
         Rewrite (Node, New_Occurrence_Of (Standard_False, Sloc (Node)));
	 
         --  If left is true, replace node by true
      
      elsif Left_Value = Uint_1 then
         Rewrite (Node, New_Occurrence_Of (Standard_True, Sloc (Node)));
	 
         --  If right is true replace by true
	 
      elsif Right_Value = Uint_1 then
         Rewrite (Node, New_Occurrence_Of (Standard_True, Sloc (Node)));
	 
         --  If left is false replace node by Right
	 
      elsif Left_Value = Uint_0 then
         Rewrite (Node, Rhs);
	 
         --  If Right is false replace node by Left
      
      elsif Right_Value = Uint_0 then
         Rewrite (Node, Lhs);
	 
      else
         null;
      end if;
   end Expand_Op_Or;
   
   ----------------------
   -- Expand_Op_Plus --
   ----------------------
   
   procedure Expand_Op_Plus
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      Expand_Right_Opnd (This, Node);
      null;   end Expand_Op_Plus;
   
   ---------------------
   -- Expand_Op_Rem --
   ---------------------
   
   procedure Expand_Op_Rem
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      Expand_Left_Opnd (This, Node);
      Expand_Right_Opnd (This, Node);
   end Expand_Op_Rem;
   
   -----------------------------
   -- Expand_Op_Rotate_Left --
   -----------------------------
   
   procedure Expand_Op_Rotate_Left
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Op_Rotate_Left;
   
   ------------------------------
   -- Expand_Op_Rotate_Right --
   ------------------------------
   
   procedure Expand_Op_Rotate_Right
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Op_Rotate_Right;
   
   -----------------------------
   -- Expand_Op_Shift_Right --
   -----------------------------
   
   procedure Expand_Op_Shift_Right
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Op_Shift_Right;
   
   ----------------------------------------
   -- Expand_Op_Shift_Right_Arithmetic --
   ----------------------------------------
   
   procedure Expand_Op_Shift_Right_Arithmetic
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Op_Shift_Right_Arithmetic;
   
   ----------------------------
   -- Expand_Op_Shift_Left --
   ----------------------------
   
   procedure Expand_Op_Shift_Left
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Op_Shift_Left;
   
   --------------------------
   -- Expand_Op_Subtract --
   --------------------------
   
   procedure Expand_Op_Subtract
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      Expand_Left_Opnd (This, Node);
      Expand_Right_Opnd (This, Node);
   end Expand_Op_Subtract;
   
   ---------------------
   -- Expand_Op_Xor --
   ---------------------
   
   procedure Expand_Op_Xor
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      Expand_Left_Opnd (This, Node);
      Expand_Right_Opnd (This, Node);
   end Expand_Op_Xor;
   
   ------------------------------
   -- Expand_Operator_Symbol --
   ------------------------------
   
   procedure Expand_Operator_Symbol
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Operator_Symbol;
   
   ----------------------
   -- Expand_Or_Else --
   ----------------------
   
   procedure Expand_Or_Else
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      Expand_Short_Circuit_Operator (This, Node);
      --  Expand_Left_Opnd (This, Node);
      --  Expand_Right_Opnd (This, Node);
   end Expand_Or_Else;
   
   ----------------------------
   -- Expand_Others_Choice --
   ----------------------------
   
   procedure Expand_Others_Choice
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Others_Choice;
   
   ------------------------------------
   -- Expand_Parameter_Association --
   ------------------------------------
   
   procedure Expand_Parameter_Association
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Parameter_Association;
   
   -----------------------------------
   -- Expand_Qualified_Expression --
   -----------------------------------
   
   procedure Expand_Qualified_Expression
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Qualified_Expression;
   
   ------------------------------------
   -- Expand_Quantified_Expression --
   ------------------------------------
   
   procedure Expand_Quantified_Expression
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Quantified_Expression;
   
   --------------------
   -- Expand_Range --
   --------------------
   
   procedure Expand_Range
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Range;
   
   ---------------------------
   -- Expand_Real_Literal --
   ---------------------------
   
   procedure Expand_Real_Literal
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Val    : Ureal;
      Denom  : Int;
      Num    : Int;
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
   end Expand_Real_Literal;
   
   ------------------------
   -- Expand_Reference --
   ------------------------
   
   procedure Expand_Reference
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      if Nkind (Prefix (Node)) = N_Function_Call then
         Error_Msg_N ("unsupported kind of function call", Node);
	 
      elsif Nkind (Prefix (Node)) = N_Procedure_Call_Statement then
         Error_Msg_N ("unsupported kind of procedure call", Node);
	 
      else      
         Expand_Node (This, Prefix (Node));
      end if;
   end Expand_Reference;
   
   --------------------
   -- Expand_Slice --
   --------------------
   
   procedure Expand_Slice
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Slice;
   
   -----------------------------
   -- Expand_String_Literal --
   -----------------------------
   
   procedure Expand_String_Literal
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Str : constant String_Id := Strval (Node);
   begin
      --  Output string literal
      
      for J in 1 .. String_Length (Str) loop
         null; -- Write_C_Char_Code (Ob, Get_String_Char (Str, J));
      end loop;
   end Expand_String_Literal;
   
   ------------------------------
   -- Expand_Type_Conversion --
   ------------------------------
   
   procedure Expand_Type_Conversion
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Expr          : Node_Id;
      Source_Type   : Entity_Id;
      Target_Type   : Entity_Id;
      Conv_Function : Entity_Id;
      Conv_Call     : Node_Id;
   begin
      Put_Line ("Expand_Type_Conversion Begin");
      Expr := Unqual_Conv (Node);
      
      if Reflex_Options.Plc_Target = Unity_Target then
	 
	 Source_Type := Etype (Expr);
	 Target_Type := Etype (Subtype_Mark (Node));
	 
	 Put_Line ("Source_Type " & Get_String (Chars (Source_Type)));
	 Put_Line ("Target_Type " & Get_String (Chars (Target_Type)));
	 
	 if Is_Numeric_Type (Target_Type) then
	    pragma Assert (Is_Numeric_Type (Source_Type));
	    
	    Conv_Function := Unity_Nuneric_Converion_Function
	      (Source_Type, Target_Type);
	    
	    if Present (Conv_Function) then
	       Put_Line ("Conv Function " & Get_String (Chars (Conv_Function)));
	       
	       Conv_Call := Make_Function_Call
		 (Sloc  => Sloc (Node),
		  Name  => New_Occurrence_Of (Conv_Function, Sloc (Node)),
		  Parameter_Associations =>
		    New_List (Expr));
	       
	       Replace (Node, Conv_Call);
	    else
	       Replace (Node, Expr);
	    end if;
	    
	 else
	    Replace (Node, Expr);
	 end if;
      else
	 Replace (Node, Expr);
      end if;
      
      Put_Line ("Expand_Type_Conversion End");
   end Expand_Type_Conversion;
   
   -----------------------------------
   -- Expand_Unchecked_Expression --
   -----------------------------------
   
   procedure Expand_Unchecked_Expression
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Unchecked_Expression;
   
   ----------------------------------------
   -- Expand_Unchecked_Type_Conversion --
   ----------------------------------------
   
   procedure Expand_Unchecked_Type_Conversion
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Unchecked_Type_Conversion;
   
   -----------------------------------
   -- Expand_Short_Circuit_Operator --
   -----------------------------------

   --  Deal with special expansion if actions are present for the right operand
   --  and deal with optimizing case of arguments being True or False. We also
   --  deal with the special case of non-standard boolean values.

   procedure Expand_Short_Circuit_Operator
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Loc     : constant Source_Ptr := Sloc (Node);
      Typ     : constant Entity_Id  := Etype (Node);
      Left    : constant Node_Id    := Left_Opnd (Node);
      Right   : constant Node_Id    := Right_Opnd (Node);
      LocR    : constant Source_Ptr := Sloc (Right);
      LocL    : constant Source_Ptr := Sloc (Left);
      Actlist : List_Id;

      Shortcut_Value : constant Boolean := Nkind (Node) = N_Or_Else;
      Shortcut_Ent   : constant Entity_Id := Boolean_Literals (Shortcut_Value);
      --  If Left = Shortcut_Value then Right need not be evaluated

      function Make_Test_Expr (Opnd : Node_Id) return Node_Id;
      --  For Opnd a boolean expression, return a Boolean expression equivalent
      --  to Opnd /= Shortcut_Value.

      --------------------
      -- Make_Test_Expr --
      --------------------

      function Make_Test_Expr (Opnd : Node_Id) return Node_Id is
      begin
         if Shortcut_Value then
            return Make_Op_Not (Sloc (Opnd), Opnd);
         else
            return Opnd;
         end if;
      end Make_Test_Expr;

      --  Local variables

      Op_Var : Entity_Id;
      --  Entity for a temporary variable holding the value of the operator,
      --  used for expansion in the case where actions are present.

      Decl        : Node_Id;
      New_If_R    : Node_Id;
      New_If_L    : Node_Id;
      Insert_Node : Node_Id;      
      Val         : Uint;
      Decl_Assign : Node_Id;
      
      --  Start of processing for Expand_Short_Circuit_Operator

   begin
      if Shortcut_Value then
         Val := Uint_0;
      else
         Val := Uint_1;
      end if;
      
      Expand_Node (This, Left);
      
      --  Check for cases where left argument is known to be True or False

      if Compile_Time_Known_Value (Left) then
	 
         --  Rewrite True AND THEN Right / False OR ELSE Right to Right.
         --  Any actions associated with Right will be executed unconditionally
         --  and can thus be inserted into the tree unconditionally.

         if Expr_Value_E (Left) /= Shortcut_Ent then
            --  if Present (Actions (Right)) then
            --     Insert_Actions (Node, Actions (Right));
            --  end if;
	    
            Expand_Node (This, Right);
	    
            Rewrite (Node, Right);

            --  Rewrite False AND THEN Right / True OR ELSE Right to Left.
            --  In this case we can forget the actions associated with Right,
            --  since they will never be executed.

         else
            Rewrite (Node, New_Occurrence_Of (Shortcut_Ent, Loc));
         end if;

         return;
      end if;
      
      Actlist := Actions (Node);

      --  No actions present, check for cases of right argument True/False

      if Compile_Time_Known_Value (Right) 
      then

         --  Change (Left and then True), (Left or else False) to Left. Note
         --  that we know there are no actions associated with the right
         --  operand, since we just checked for this case above.
	 
         if Expr_Value_E (Right) /= Shortcut_Ent then
            Rewrite (Node, Left);

            --  Change (Left and then False), (Left or else True) to Right,
            --  making sure to preserve any side effects associated with the Left
            --  operand.

         else
            Rewrite (Node, New_Occurrence_Of (Shortcut_Ent, Loc));
         end if;
	 
         return;
      end if;
      
      --  If Actions are present for the right operand, we have to do some
      --  special processing. We can't just let these actions filter back into
      --  code preceding the short circuit (which is what would have happened
      --  if we had not trapped them in the short-circuit form), since they
      --  must only be executed if the right operand of the short circuit is
      --  executed and not otherwise.

      --     left AND THEN right
      
      --  into
      
      --     C : Boolean := False;
      --     IF left THEN
      --        Actions;
      --        IF right THEN
      --           C := True;
      --        END IF;
      --     END IF;
      
      --  and finally rewrite the operator into a reference to C. Similarly
      --  for left OR ELSE right, with negated values. Note that this
      --  rewrite causes some difficulties for coverage analysis because
      --  of the introduction of the new variable C, which obscures the
      --  structure of the test.
      
      --  We use this "old approach" if Minimize_Expression_With_Actions
      --  is True.
      
      --  Op_Var := Make_Temporary (Loc, 'C', Related_Node => Node);
      
      Op_Var := Search_Reuse_Entity (This, Name_Find ("c"), Standard_Boolean);
      
      if No (Op_Var) then
         Op_Var := Make_Reuse_Entity
           (This, Loc, Name_Find ("c"), Standard_Boolean);
      end if;
      
      Set_Ekind (Op_Var, E_Variable);
      Set_Etype (Op_Var, Standard_Boolean);
      Set_Entity_In_Use (Op_Var, True);
      
      --  Set_Scope
      
      Decl := Make_Object_Declaration
        (Loc,
         Defining_Identifier => Op_Var,
         Object_Definition   =>
           New_Occurrence_Of (Standard_Boolean, Loc));
      --  Expression          =>
      --    New_Occurrence_Of (Shortcut_Ent, Loc));
      
      Decl_Assign := Make_Assignment_Statement
        (Loc,
         Name       => New_Occurrence_Of (Op_Var, Loc),
         Expression => 
           New_Occurrence_Of (Shortcut_Ent, Loc));
	
      New_If_R := Make_If_Statement 
        (LocR,
         Condition       => Make_Test_Expr (Right),
         Then_Statements => 
           New_List
             (Make_Assignment_Statement
                  (LocR,
                   Name       => New_Occurrence_Of (Op_Var, LocR),
                   Expression =>
                     New_Occurrence_Of
                       (Boolean_Literals (not Shortcut_Value), LocR))));
      
      if Is_Empty_List (Actlist) then
         Actlist := New_List;
      end if;
      Append (New_If_R, Actlist);
      
      New_If_L := Make_If_Statement
        (LocL,
         Condition       => Make_Test_Expr (Left),
         Then_Statements => Actlist);
      
      Insert_Node := Search_Insertion_Node (Node);
      
      This.Declare_Current_Scope (Decl);
      -- Declare_Enclosing_Subprogram (Node, Decl);
      --  Insert_Before (Insert_Node, Decl);
      Insert_Before (Insert_Node, Decl_Assign);
      
      Insert_Before (Insert_Node, New_If_L);
      
      Expand_Node (This, Right);
      
      Rewrite (Node, New_Occurrence_Of (Op_Var, Loc));
      
      Set_Entity_In_Use (Op_Var, False);
      
   end Expand_Short_Circuit_Operator;
   
   -----------------------------------
   -- Remove_Expression_Side_Effect --
   -----------------------------------
   
   procedure Remove_Expression_Side_Effect
   is
   begin
      null;
   end Remove_Expression_Side_Effect;
   
end Reflex.Expanders.Ch4;
