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
with Nmake; use Nmake;
with Sem_Util; use Sem_Util;
with Sem_Eval; use Sem_Eval;
with Tbuild; use Tbuild;
with Types; use Types;

with Reflex_Options; use Reflex_Options;
with Reflex.Expanders.Types; use Reflex.Expanders.Types;
with Reflex.Expanders.Utils; use Reflex.Expanders.Utils;
with Reflex.Expanders.Dispatch; use Reflex.Expanders.Dispatch;
with Reflex.Expanders.Supports; use Reflex.Expanders.Supports;
with Reflex.Expanders.Ch4; use Reflex.Expanders.Ch4;
with Reflex.Expanders.Expressions; use Reflex.Expanders.Expressions;
with Reflex.Boxes.Exp_Ch5; use Reflex.Boxes.Exp_Ch5;
with Reflex.Predicates; use Reflex.Predicates;
with Reflex.External_Names; use Reflex.External_Names;

package body Reflex.Expanders.Ch5 is
   
   -------------------------------------
   -- Expand_Sequence_Of_Statements --
   -------------------------------------
   
   procedure Expand_Sequence_Of_Statements
     (This  : access Reflex_Expander_Record;
      Stmts : List_Id) is
      
      Stmt : Node_Id;
      Nxt  : Node_Id;
   begin
      Put_Line ("Expand_Sequence_Of_Statements Begin");
      if Is_Non_Empty_List (Stmts) then
         Stmt := First (Stmts);

         while Present (Stmt) loop
            Nxt := Next (Stmt);
            Expand_Node (This, Stmt);
            Stmt := Nxt;
         end loop;
      end if;
      Put_Line ("Expand_Sequence_Of_Statements End");
   end Expand_Sequence_Of_Statements;
   
   -------------------
   -- Expand_Call --
   -------------------

   procedure Expand_Call 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Call;
   
   -----------------------------------
   -- Expand_Assignment_Statement --
   -----------------------------------
   
   procedure Expand_Assignment_Statement 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      LHS : Node_Id := Name (Node);
      RHS : Node_Id := Expression (Node);
      Typ : constant Node_Id := Get_Type_Full_View (Etype (LHS));
      --        Act : Node_Id;
      --        Nxt : Node_Id;
      -- Op  : Character;
   begin
      --  Write_Indent;
      --  Write_Itypes_In_Subtree (Node);
      
      --  Do not output LHS when RHS is a raise statement (to leave
      --  the C output cleaner).
      
      --  if Is_Raise_Statement (RHS) then
	 
      --  Cprint_Node (RHS);
      --  Error_Msg_N ("raise statement not supported", RHS);
	 
      --  elsif Ekind (Typ) in Composite_Kind
      --  	or else Nkind (RHS) = N_Unchecked_Type_Conversion
      --  then
      --  memcpy() is only safe to use when both Forwards_OK and
      --  Backwards_OK are True.
	 
      --  Cprint_Copy
      --    (Target     => LHS,
      --     Source     => RHS,
      --     Use_Memcpy => Forwards_OK (Node)
      --       and then Backwards_OK (Node));
	 
      null;
	 
      --  elsif Is_Access_Type (Typ)
      --  	and then Has_Fat_Pointer (Typ)
      --  	and then Nkind (RHS) = N_Allocator
      --  then
      --  Cprint_Node (LHS, Declaration => True);
      --  Write_Str (" = ");
      --  Write_Fatptr_Init (RHS, Typ);
	 
      null;
	 
      --  Handle conversion of access-to-constrained-array type to
      --  access-to-unconstrained array type. The reverse case is
      --  handled when procesing the N_Type_Conversion node.
	 
      --  elsif Is_Access_Type (Typ)
      --  	and then Has_Fat_Pointer (Typ)
      --  	and then Nkind (RHS) = N_Type_Conversion
      --  	and then not Has_Fat_Pointer (Etype (Expression (RHS)))
      --  then
      --  Cprint_Node (LHS, Declaration => True);
      --  Write_Str (" = ");
      --  Write_Fatptr_Init (Expression (RHS), Typ);
	 
      null;
	 
      --  elsif Is_Access_Type (Typ)
      --  	and then
      --  	((Is_Array_Formal (LHS) and then not Is_Array_Formal (RHS))
      --  	 or else
      --  	   (not Is_Array_Formal (LHS) and then Is_Array_Formal (RHS)))
      --  	and then Is_Constrained_Array_Type
      --  	(Get_Full_View (Designated_Type (Typ)))
      --  then
      --  Cprint_Node (LHS, Declaration => True);
      --  Write_Str (" = ");
	 
      --  if Is_Array_Formal (LHS) then
	    
      --     --  No casting needed for OUT and IN-OUT access formals
	    
      --     if Nkind (LHS) in N_Has_Entity
      --       and then Is_Out_Mode_Access_Formal (Entity (LHS))
      --     then
      --        null;
	       
      --        --  No casting needed for constrained multidimensional
      --        --  array types.
	       
      --     elsif Is_Unidimensional_Array_Type (Designated_Type (Typ))
      --     then
      --        Write_Char ('(');
      --        Write_Id
      --  	 (Component_Type
      --  	    (Get_Full_View (Designated_Type (Typ))));
      --        Write_Str ("*)");
      --     end if;
      --  else
      --     Write_Char ('(');
      --     Write_Id (Typ);
      --     Write_Char (')');
      --  end if;
	 
      --  Cprint_Node (RHS);
	 
      null;
	 
      --  elsif Is_Access_Type (Typ)
      --  	and then Is_AREC_Reference (LHS)
      --  then
      --  Cprint_Node (LHS, Declaration => True);
      --  Write_Str (" = (");
      --  Write_Id (Etype (Get_AREC_Field (LHS)));
      --  Write_Str (")");
      --  Cprint_Node (RHS);
	 
      null;
	 
      --  else
      --  Use simple assignment
	 
--        if Nkind (Lhs) = N_Expression_With_Actions then
--           Expand_Expression_With_Actions (This, Lhs);
         --  and then Present (Actions (Lhs)) then
         --   Act := First (Actions (Lhs));
         --   while Present (Act) loop
         --      Nxt := Next (Act);
         --      if Nkind (Act) = N_Object_Declaration then
         --  	  Remove (Act);
         --  	  This.Declare_Current_Scope (Act);
         --      end if;
         --      Expand_Node (This, Act);
         --      Act := Nxt;
         --   end loop;
--        else	 
         Expand_Node (This, Lhs); --  := Expression (Lhs);
--        end if;
--        if Nkind (Rhs) = N_Expression_With_Actions then
--           Expand_Expression_With_Actions (This, Rhs);
         --  and then Present (Actions (Rhs)) then
         --   Act := First (Actions (Rhs));
         --   while Present (Act) loop
         --      Nxt := Next (Act);
         --      if Nkind (Act) = N_Object_Declaration then
         --  	  Remove (Act);
         --  	  This.Declare_Current_Scope (Act);
         --      end if;
         --      Expand_Node (This, Act);
         --      Act := Nxt;
         --   end loop;
	   
	 --        else
	 
	 if Nkind (Node) = N_Aggregate 
	   or else Nkind (Node) = N_Extension_Aggregate 
	 then
	    --  Create a constant for the aggregate and rewrite the assignament
	    --  as "Lhs := Constant_Aggaregate". The aggregate constnt  is 
	    --  declared in the current scope.
	    
	    --  Create the constant and initiliaze it with the aggregate
	    
	    --  declare the constant in the current scope
	    
	    null;
	 end if;
	 
         Expand_Node (This, Rhs); -- := Expression (Rhs);
	 
	 if Expression_Has_Side_Effect (Rhs) then
	    Remove_Function_Call_Effect (Rhs);
	 end if;

	 --        end if;
	 
      -- Expand_Node (This, LHS);
	 
      --  A special case, if we have X = X +/- const, convert to
      --  the more natural ++/-- or +=/-= notation in the C output.
	 
      --  if Is_Entity_Name (LHS)
      --    and then Nkind_In (RHS, N_Op_Add, N_Op_Subtract)
      --    and then Is_Entity_Name (Left_Opnd (RHS))
      --    and then Entity (LHS) = Entity (Left_Opnd (RHS))
      --    and then Nkind (Right_Opnd (RHS)) = N_Integer_Literal
      --  then
      --     if Nkind (RHS) = N_Op_Add then
      --        Op := '+';
      --     else
      --        Op := '-';
      --     end if;
	    
      --     if Intval (Right_Opnd (RHS)) = 1 then
      --        Write_Char (Op);
      --        Write_Char (Op);
      --     else
      --        Write_Char (' ');
      --        Write_Char (Op);
      --        Write_Str ("= ");
      --        Cprint_Node (Right_Opnd (RHS));
      --     end if;
	    
      --  elsif Is_Access_Type (Typ)
      --    and then Has_Fat_Pointer (Typ)
      --    and then Nkind (RHS) = N_Null
      --  then
      --     Write_Str (" = ");
      --     Write_Fatptr_Init (RHS, Typ);
	    
      --  elsif Is_Access_Type (Typ)
      --    and then not Has_Fat_Pointer (Typ)
      --    and then Has_Fat_Pointer (Etype (RHS))
      --  then
      --     Write_Str (" = ");
	    
      --     Write_Char ('(');
      --     Write_Id (Typ);
      --     Write_Str (") ");
	    
      --     Cprint_Node (RHS);
      --     Write_Fatptr_Dereference;
	    
      --  elsif Is_Access_Type (Typ)
      --    and then Typ /= Get_Full_View (Etype (RHS))
      --  then
      --     Write_Str (" = (");
      --     Write_Id (Typ);
      --     Write_Str (") ");
      --     Cprint_Node (RHS);
	    
      --     --  Normal case of C assignment
	    
      --  else
	 
      -- Expand_Node (This, RHS);
      --  end if;
      --  end if;
   end Expand_Assignment_Statement;
   
   ------------------------------
   -- Expand_Block_Statement --
   ------------------------------
   
   procedure Expand_Block_Statement 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      --  Expand declarations
      
      --  Put Declarations in englobing Subprogram Scope
      
      --  Expand Statements
      null;
   end Expand_Block_Statement;
   
   -----------------------------
   -- Expand_Case_Statement --
   -----------------------------
   
   procedure Expand_Case_Statement 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Use_If     : Boolean := False;
      Alt        : Node_Id;
      Choice     : Node_Id;
      Cond       : Node_Id;
      Lhs        : Node_Id;
      Rhs        : Node_Id;
      If_Node    : Node_Id;
      Elsif_Node : Node_Id;
      Cur_Node   : Node_Id;
      Elsif_List : List_Id := No_List;
      Expr       : Node_Id;
      Lop        : Node_Id;
      Rop        : Node_Id;
      Stmts      : List_Id;
   begin
      --  The sfirst setp is to do stndard expansion on the case 
      
      --  Expand the condition
      
      Cond := Condition (Node);
      Expand_Node (This, Cond);
      
      --  Expand Choices 
      
      Alt := First (Alternatives (Node));
      while Present (Next (Alt)) loop
	 Choice := First (Discrete_Choices (Alt));
	 while Present (Choice) loop
	    Expand_Node (This, Choice);
	    Next (Choice);
	 end loop;
	 
	 --  Expand statements
	 
	 --  Expand_Sequence_Of_Statements (This, Statements (Alt));
	 Next (Alt);
      end loop;
      
	 
      --  If the lang to generate is the special case Ladder, do the ladder
      --  expansion for case
      
      if Reflex_Options.Ladder_Language then
         Reflex.Boxes.Exp_Ch5.Expand_Case_Statement_For_Ladder (This, Node);
         return;
      end if;
      
      --  First we do a prescan to see if there are any ranges, if so, we will
      --  have to use an if/else translation since the Plc case statement does
      --  all accomodate ranges. Note that we do not have to test the last
      --  alternative, since it translates to a default anyway without any
      --  range tests.
      
      case Reflex_Options.Case_Statement_Generation is
	 when Always_As_If   => Use_If := True;
	 when Always_As_Case => Use_If := False;
	 when Range_As_If =>
	    Alt := First (Alternatives (Node));
	Outer : while Present (Next (Alt)) loop
	       Choice := First (Discrete_Choices (Alt));
	       while Present (Choice) loop
		  if Nkind (Choice) = N_Range
		    or else Nkind (Choice) = N_Attribute_reference
		    or else (Is_Entity_Name (Choice)
			       and then Is_Type (Entity (Choice)))
		  then
		     Use_If := True;
		     exit Outer;
		  end if;
		  
		  Next (Choice);
	       end loop;
	       
	       Next (Alt);
	    end loop Outer;
      end case;
      
      --  Case where we have to use if's
      
      if Use_If then
         Alt := First (Alternatives (Node));
         loop
            --  First alternative, use if
	    
            if No (Prev (Alt)) then
               If_Node := Make_If_Statement (Sloc (Node), Empty, New_List);
               Cur_Node := If_Node;
	       
               --  All but last alternative, use else if
	       
            elsif Present (Next (Alt)) then
               if Is_Empty_List (Elsif_List) then
                  Elsif_List := New_List;
                  Set_Elsif_Parts (If_Node, Elsif_List);
               end if;
	       
               Elsif_Node := Make_Elsif_Part (Sloc (If_Node), Empty, No_List);
               Append (Elsif_Node, Elsif_List);
	       
               Cur_Node := Elsif_Node;
	       
               --  Last alternative, use else and we are done
	       
            else
               Stmts := Statements (Alt);
               Expand_Sequence_Of_Statements (This, Stmts);
               Set_Else_Statements (If_Node, Stmts);
               exit;
            end if;
	    
            Cond := Empty;
            Choice := First (Discrete_Choices (Alt));
            loop
               --  Simple expression, equality test
	       
               if not Nkind_In (Choice, N_Range, N_Subtype_Indication)
                 and then (not Is_Entity_Name (Choice)
                           or else not Is_Type (Entity (Choice)))
               then
                  Lhs := Expression (Node);
                  Expand_Node (This, Lhs);
                  Rhs :=  Choice;
                  Expand_Node (This, Rhs);
		  
                  Expr := Make_Op_Eq (Sloc (Choice), Lhs, Rhs);
		  
                  --  Range, do range test
		  
               else
                  declare
                     LBD : Node_Id;
                     HBD : Node_Id;
		     
                  begin
                     case Nkind (Choice) is
                     when N_Range =>
                        LBD := Low_Bound  (Choice);
                        HBD := High_Bound (Choice);
			   
                     when N_Subtype_Indication =>
                        pragma Assert
                          (Nkind (Constraint (Choice)) =
                             N_Range_Constraint);
			   
                        LBD :=
                          Low_Bound (Range_Expression
                                     (Constraint (Choice)));
                        HBD :=
                          High_Bound (Range_Expression
                                      (Constraint (Choice)));
			   
                     when others =>
                        LBD := Type_Low_Bound  (Entity (Choice));
                        HBD := Type_High_Bound (Entity (Choice));
                     end case;
		     
                     Lhs := Expression (Node);
                     Expand_Node (This, Lhs);
                     Expand_Node (This, LBD);
                     Expand_Node (This, HBD);
		     
                     Lop  := Make_Op_Ge (Sloc (Choice), Lhs, LBD);
                     Rop  := Make_Op_Le (Sloc (Choice), Lhs, HBD);
                     Expr := Make_Op_And (Sloc (Choice), Lop, Rop);
                  end;
               end if;
	       
               if Present (Cond) then
                  Cond := Make_Op_Or (Sloc (Choice), Cond, Expr);
               else
                  Cond := Expr;
               end if;
	       
               if Present (Next (Choice)) then
                  Next (Choice);
               else
                  exit;
               end if;
            end loop;
	    
            Set_Condition (Cur_Node, Cond);
	    
            Stmts := Statements (Alt);
	    
            Expand_Sequence_Of_Statements (This, Stmts);
	    
            Set_Then_Statements (Cur_Node, Stmts);
	    
            Next (Alt);
         end loop;
	 
         --  Insert If Statement before Case Statement and remove the 
         --  Case Statment
	 
         Insert_Before (Node, If_Node);
         Remove (Node);
	 
         --  Case where we can use Switch
	 
      else
         Expand_Node (This, Expression (Node));
	 
         Alt := First (Alternatives (Node));
         while Present (Alt) loop
            Expand_Case_Statement_Alternative (This, Alt);
            Next (Alt);
         end loop;
      end if;
   end Expand_Case_Statement;
   
   -----------------------------------------
   -- Expand_Case_Statement_Alternative --
   -----------------------------------------
   
   procedure Expand_Case_Statement_Alternative 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Choices : constant List_Id := Discrete_Choices (Node);
      Choice  : Node_Id;
      Nxt     : Node_Id;
   begin
      Choice := First (Choices);
      
      if Nkind (Choice) = N_Others_Choice then
         null;
	 
      else
         while Present (Choice) loop
            Nxt := Next (Choice);
            Expand_Node (This, Choice);
            Choice := Nxt;
         end loop;
      end if;
      
      if Has_Non_Null_Statements (Statements (Node)) then
         Expand_Sequence_Of_Statements (This, Statements (Node));
      else
         null;
      end if;
   end Expand_Case_Statement_Alternative;
   
   -----------------------------
   -- Expand_Code_Statement --
   -----------------------------
   
   procedure Expand_Code_Statement 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Code_Statement;
   
   ---------------------------------
   -- Expand_Compound_Statement --
   ---------------------------------
   
   procedure Expand_Compound_Statement 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Compound_Statement;
   
   -------------------------
   -- Expand_Elsif_Part --
   -------------------------
   
   procedure Expand_Elsif_Part 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Elsif_Part;
   
   ------------------------------
   -- Enclosing_Loop_Statement --
   ------------------------------
   
   function Enclosing_Loop_Statement (Node : Node_Id) return Node_Id is
      Par : Node_Id := Node;
   begin
      while Present (Par) loop
	 if Nkind (Par) = N_Loop_Statement then
	    return Par;
	 end if;
	 Par := Parent (Par);
      end loop;
      
      return Empty;
   end Enclosing_Loop_Statement;
   
   -----------------------------
   -- Expand_Exit_Statement --
   -----------------------------
   
   procedure Expand_Exit_Statement 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Cond     : Node_Id := Condition (Node);
      Lab      : Node_Id := Name (Node);
      Go_To    : Node_Id;
      Cur_Loop : Node_Id;
      If_Node  : Node_Id;
   begin
      --  If the exit is going in a upper loop than the current loop, change
      --  it as a goto statement.
      
      --  If Exit Condition is present, then change it to an if sttement.
      
      if Present (Cond) then
	 Expand_Node (This, Cond);
	 
	 If_Node := Make_If_Statement
	   (Sloc (Node), Condition (Node), New_List);
	 
	 Insert_Before (Node, If_Node);
	 Remove (Node);
	 
	 if Present (Lab) then
	    Cur_Loop := Enclosing_Loop_Statement (Node);
	    if Present (Cur_Loop) 
	      and then Chars (Lab) = Chars (Identifier (Cur_Loop))
	    then
	       Set_Condition (Node, Empty);
	       Set_Name (Node, Empty);
	       Append (Node, Then_Statements (If_Node));
	    else
	       Go_To := Make_Goto_Statement (Sloc (Node), Lab);
	       Append (Go_To, Then_Statements (If_Node));
	    end if;
	 else
	    Set_Condition (Node, Empty);
	    Set_Name (Node, Empty);
	    Append (Node, Then_Statements (If_Node));
	 end if;
      end if;
   end Expand_Exit_Statement;
   
   -----------------------------
   -- Expand_Free_Statement --
   -----------------------------
   
   procedure Expand_Free_Statement 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Free_Statement;
   
   ----------------------------
   -- Expand_Function_Call --
   ----------------------------
   
   procedure Expand_Function_Call 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Function_Call;
   
   -----------------------------
   -- Expand_Goto_Statement --
   -----------------------------
   
   procedure Expand_Goto_Statement 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Goto_Statement;
   
   ---------------------------------------------
   -- Expand_Handled_Sequence_Of_Statements --
   ---------------------------------------------
   
   procedure Expand_Handled_Sequence_Of_Statements 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Saved_Value : constant Boolean := This.Get_In_Package_Body_Init;
      Stmts       : List_Id;
   begin
      Put_Line ("Expand_Handled_Sequence_Of_Statements Begin");
      This.Set_In_Package_Body_Init (Nkind (Parent (Node)) = N_Package_Body);
      
      Stmts := Statements (Node);
      Expand_Sequence_Of_Statements (This, Stmts);
      
      if not Is_Empty_List (Exception_Handlers (Node)) then
         Error_Msg_N
           ("??exception handlers are ignored",
            First (Exception_Handlers (Node)));
      end if;
      
      if Present (At_End_Proc (Node)) then
         Error_Msg_N
           ("clean up procedures not supported yet",
            At_End_Proc (Node));
      end if;
      
      This.Set_In_Package_Body_Init (Saved_Value);
      Put_Line ("Expand_Handled_Sequence_Of_Statements End");
   end Expand_Handled_Sequence_Of_Statements;
   
   -------------------------------
   -- Rewrite_Elsif_Side_Effect --
   -------------------------------
   
   procedure Rewrite_Elsif_Side_Effect (Node : Node_Id) is
      
      Else_Stmts        : List_Id;
      Cur               : Node_Id;
      Prv               : Node_Id;
      New_Elsif         : Node_Id;
      Elif              : Node_Id;
      Nxt               : Node_Id;
      Side_Effect_Found : Boolean;
   begin
      --  If some elsif has side effect, the elsif is rewritten as
      --  an else which its first statements is an if with elsif condition
      --  example :
      --    if c1 then
      --       ...
      --    elsif C2 then
      --       ...
      --    elsif C3 then
      --       ...
      --    else
      --      ;;;
      --    end if;
      -- 
      --  C3 has a side effect so the change is 
      --    if c1 then
      --       ...
      --    else
      --       ipoint to insert the side effect
      --       if C2 then
      --          ...
      --       elsif C3 then
      --          ...
      --       else
      --          ...
      --       end if;
      --   end if;
      
      Else_Stmts := Else_Statements (Node);
      
      if Present (Elsif_Parts (Node)) then
	 Cur := Last (Elsif_Parts (Node));
	 while Present (Cur) loop
	    Prv := Prev (Cur);
	    
	    if Expression_Has_Side_Effect (Condition (Cur)) then
	       Side_Effect_Found := True;
	       
	       New_Elsif := New_Node (N_If_Statement, Sloc (Cur));
	       Set_Condition (New_Elsif, Condition (Cur));
	       
	       --  Remove Side Effect from Expression in the new If Condition
	       
	       Set_Then_Statements (New_Elsif, Then_Statements (Cur));
	       Set_Else_Statements (New_Elsif, Else_Stmts);
	       
	       Elif := Next (Cur);
	       
	       if Present (Elif) then
		  Set_Elsif_Parts (New_Elsif, New_List);
		  while Present (Elif) loop
		     Nxt := Next (ELif);
		     Remove (Elif);
		     Append (Elif, Elsif_Parts (New_Elsif));
		     Elif := Nxt;
		  end loop;
	       end if;
	       
	       Else_Stmts := New_List (New_Elsif);
	       Remove (Cur);
	    end if;
	    
	    Cur := Prv;
	 end loop;
	 
	 if Present (Else_Stmts) then
	    Set_Else_Statements (Node, Else_Stmts);
	 end if;
      end if;
   end Rewrite_Elsif_Side_Effect;
   
   ---------------------------
   -- Expand_If_Statement --
   ---------------------------
   
   procedure Expand_If_Statement 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Els   : Node_Id;
      Stmts : List_Id;
   begin
      Rewrite_Elsif_Side_Effect (Node);

      Expand_Node (This, Condition (Node));
      
      -- Open_Scope;
      Expand_Sequence_Of_Statements (This, Then_Statements (Node));
      
      if Present (Elsif_Parts (Node)) then
         Els := First (Elsif_Parts (Node));
         while Present (Els) loop
            Expand_Node (This, Condition (Els));
            Expand_Sequence_Of_Statements (This, Then_Statements (Els));
            Next (Els);
         end loop;
      end if;
      
      Stmts := Else_Statements (Node);
      if not Is_Empty_List (Stmts) then
	 Expand_Sequence_Of_Statements (This, Stmts);
      end if;
      
      if Reflex_Options.Ladder_Language then
         if Has_Only_Simple_Statement (Node) then
            Expand_Simple_If_Statement_For_Ladder (This, Node);
            if Must_Break_If (Node) 
              or else (Present (Elsif_Parts (Node))
                       or else Present (Else_Statements (Node)))
            then
               Break_If_Statement (This, Node);
            end if;
         else
            Expand_If_Statement_As_Goto (This, Node);
         end if;
      end if;
      
      New_Line;
   end Expand_If_Statement;
   
   -------------------------------
   -- Expand_Iteration_Scheme --
   -------------------------------
   
   procedure Expand_Iteration_Scheme 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Iteration_Scheme;
   
   -------------------------------------
   -- Expand_Iterator_Specification --
   -------------------------------------
   
   procedure Expand_Iterator_Specification 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Iterator_Specification;
   
   --------------------
   -- Expand_Label --
   --------------------
   
   procedure Expand_Label 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Label;
   
   -------------------------------------------
   -- Expand_Loop_Parameter_Specification --
   -------------------------------------------
   
   procedure Expand_Loop_Parameter_Specification 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Loop_Parameter_Specification;
   
   -----------------------------
   -- Expand_Loop_Statement --
   -----------------------------
   
   procedure Expand_Loop_Statement 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      ISS              : constant Node_Id := Iteration_Scheme (Node);
      For_Loop_Id      : Entity_Id := Empty;
      For_Loop_Type    : Entity_Id := Empty;
      For_Loop_Var     : Node_Id   := Empty;
      --  Set to defining identifier of for loop variable for FOR loop
      LBD              : Node_Id;
      HBD              : Node_Id;
      For_Loop_Reverse : Boolean;
      --  Set True if reverse for loop, False for normal for loop
   begin
      if Present (Identifier (Node))
        and then not Has_Created_Identifier (Node)
      then
         null;
      end if;
      
      --  Handle iteration scheme
      
      if Present (ISS) then
	 
         --  WHILE loop case, Expands C while
	 
         if Present (Condition (ISS)) then
            Expand_Node (This, Condition (ISS));
	    
            --  FOR loop case
	    
         else
            declare
               LPS : constant Node_Id :=
                 Loop_Parameter_Specification (ISS);
               DSD : constant Node_Id :=
                 Discrete_Subtype_Definition (LPS);
               Rng : Node_Id;
               Id  : Entity_Id;
            begin
               Id   := Defining_Identifier (LPS);
               For_Loop_Type := Etype (Id);
	       
               For_Loop_Id := Search_Reuse_Entity 
                 (This, Chars (Id), For_Loop_Type);
	       
               if No (For_Loop_Id) then
		  
                  For_Loop_Id := Make_Reuse_Entity 
                    (This, Sloc (Id), Chars (Id), For_Loop_Type);
		  
                  For_Loop_Var := Make_Object_Declaration
                    (Sloc                => Sloc (Node),
                     Defining_Identifier => For_Loop_Id,
                     Object_Definition   => 
                       New_Occurrence_Of (For_Loop_Type, Sloc (Node)));
	       
                  Declare_Current_Scope (This, For_Loop_Var);
               end if;
	       
               Set_Defining_Identifier (LPS, For_Loop_Id);
               Set_Entity_In_Use (For_Loop_Id, True);
	       
               For_Loop_Reverse := Reverse_Present (LPS);
	       
               case Nkind (DSD) is
               when N_Range =>
                  Rng := DSD;
               when N_Subtype_Indication =>
                  Rng := Range_Expression (Constraint (DSD));
               when others =>
                  raise Program_Error;
               end case;
	       
               LBD := Low_Bound (Rng);
               HBD := High_Bound (Rng);
	       
               if For_Loop_Reverse then
                  null;
               else
                  null;
               end if;
               Expand_Node (This, LBD);
               Expand_Node (This, HBD);
            end;
         end if;

         --  No iteration scheme present
	 
      else
         null;
      end if;
      
      --  Expand the loop body
      
      Expand_Sequence_Of_Statements (This, Statements (Node));
      
      if Present (For_Loop_Id) then
         Set_Entity_In_Use (For_Loop_Id, False);
      end if;

      --  Expand label at end of loop as possible exit target
      
      if Present (Identifier (Node))
        and then not Has_Created_Identifier (Node)
      then
         null;
      end if;
     
      if Reflex_Options.Ladder_Language then
         Reflex.Boxes.Exp_Ch5.Expand_Loop_Statement_For_Ladder (This, Node);
      end if;
     
      New_Line;
   exception
      when others =>
         Put_Line (" EXCEPTION in Expand_Loop_Statement");
   end Expand_Loop_Statement;
   
   -----------------------------
   -- Expand_Null_Statement --
   -----------------------------
   
   procedure Expand_Null_Statement 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Null_Statement;
   
   ------------------------------------
   -- Expand_Parameter_Association --
   ------------------------------------
   
   procedure Expand_Parameter_Association 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Parameter_Association;
   
   ---------------------------------------
   -- Expand_Procedure_Call_Statement --
   ---------------------------------------
   
   -----------------
   -- Dump_Actual --
   -----------------
   
   procedure Dump_Actual
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Call   : Entity_Id;
      Params : List_Id;
      Param  : Node_Id;
      Formal : Entity_Id;
      Actual : Node_Id;
   begin
      Call := Entity (Name (Node));
      
      Params := Parameter_Associations (Node);
      Param := First (Params);
      Formal := First_Formal_With_Extras (Call);
      while Present (Param) loop
	 if Nkind (Param) = N_Parameter_Association then 
	    Actual := Explicit_Actual_Parameter (Param);
	    
	 else
	    Actual := Param;
	 end if;
	 
	 Next (Param);
	 Next_Formal_With_Extras (Formal);
      end loop;
      
      Actual := First_Named_Actual (Node);
      Formal := First_Formal_With_Extras (Call);
      while Present (Actual) loop
	 Next_Named_Actual (Actual);
	 Next_Formal_With_Extras (Formal);
      end loop;
      
      Actual := First_Actual (Node);
      Formal := First_Formal_With_Extras (Call);
      while Present (Actual) loop
	 Next_Actual (Actual);
	 Next_Formal_With_Extras (Formal);
      end loop;
      
   end Dump_Actual;
   
   
   procedure Expand_Procedure_Call_Statement 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Call   : Entity_Id;
      Actual : Node_Id;
   begin
      Dump_Actual (This, Node);
      Call := Entity (Name (Node));
      
      Actual := First_Actual (Node);
      while Present (Actual) loop
	 Expand_Node (This, Actual);
	 Next (Actual);
      end loop;
      
   end Expand_Procedure_Call_Statement;
   
   -------------------------------
   -- Expand_Raise_Expression --
   -------------------------------
   
   procedure Expand_Raise_Expression 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Raise_Expression;
   
   --------------------------
   -- Expand_Raise_Error --
   --------------------------
   
   procedure Expand_Raise_Error 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Raise_Error;
   
   -----------------------------
   -- Expand_Return_Statement --
   -----------------------------
   
   procedure Expand_Return_Statement 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Return_Statement;
   
   ---------------------------
   -- Change_Globals_To_Ref --
   ---------------------------
   
   procedure Change_Globals_To_Ref
     (This : access Reflex_Expander_Record;
      Node : Node_Id;
      Decl_Node : Node_Id) is
   begin
      null;
   end Change_Globals_To_Ref;
   
end Reflex.Expanders.Ch5;
