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

with Ada.text_Io; use Ada.text_IO;

with Atree; use Atree;
with Errout; use Errout;
with Einfo; use Einfo;
with Sinfo; use Sinfo;
with Namet; use Namet;
with Nlists; use Nlists;
with Sem_Util; use Sem_Util;
with Sem_Eval; use Sem_Eval;
with Types; use Types;
with Snames; use Snames;

with Artics.Buffers; use Artics.Buffers;
with Reflex.Gen.Types; use Reflex.Gen.Types;
with Reflex.Gen.Outputs; use Reflex.Gen.Outputs;
with Reflex.Gen.Utils; use Reflex.Gen.Utils;

with Reflex.Global_Arecs; use Reflex.Global_Arecs;
with Reflex.Infos; use Reflex.Infos;
with Stand; use Stand;

package body Unity.Gen.Ch5 is
   
   -------------------------------------
   -- Generate_Sequence_Of_Statements --
   -------------------------------------
   
   procedure Generate_Sequence_Of_Statements
     (This  : access Unity_Generator_Record;
      Stmts : List_Id) is
      
      Ob   : Output_Buffer := This.Get_Output_Buffer;
      Stmt : Node_id;
   begin
      if Is_Non_Empty_List (Stmts) then
         Stmt := First (Stmts);

         while Present (Stmt) loop
	    Write_Comment_Line_To_Node (Generator_Ptr (This), Stmt);
	    Generate_Node (This, Stmt);
            Next (Stmt);
         end loop;
      end if;
   end Generate_Sequence_Of_Statements;
   
   -------------------
   -- Generate_Call --
   -------------------

   procedure Generate_Call 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Call;
   
   -----------------------------------
   -- Generate_Assignment_Statement --
   -----------------------------------
   
   procedure Generate_Assignment_Statement 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob  : Output_Buffer := This.Get_Output_Buffer;
      LHS : Node_Id := Name (Node);
      RHS : Node_Id := Expression (Node);
      Typ : constant Node_Id := Get_Type_Full_View (Etype (LHS));
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
	 
--  	 If Nkind (Lhs) = N_Expression_With_Actions 
--  	   and then Present (Actions (Lhs)) then
--  	    Act := First (Actions (Lhs));
--  	    while Present (Act) loop
--  	       Generate_Node (This, Act);
--  	       Next (Act);
--  	    end loop;
--  	    
--  	    Lhs := Expression (Lhs);
--  	 end if;
	 
--  	 If Nkind (Rhs) = N_Expression_With_Actions 
--  	   and then Present (Actions (Rhs)) then
--  	    Act := First (Actions (Rhs));
--  	    while Present (Act) loop
--  	       Generate_Node (This, Act);
--  	       Next (Act);
--  	    end loop;
--  	    
--  	    Rhs := Expression (Rhs);
--  	 end if;
	 
	 Write_Indent (Ob);
	 Generate_Node (This, LHS);
	 
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
	 
	 Write_Str (Ob, " := ");
	 Generate_Node (This, RHS);
	 --  end if;
   --  end if;
      
      Write_Char (Ob, ';');
      Write_Eol (Ob);
   end Generate_Assignment_Statement;
   
   ------------------------------
   -- Generate_Block_Statement --
   ------------------------------
   
   procedure Generate_Block_Statement 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Block_Statement;
   
   -----------------------------
   -- Generate_Case_Statement --
   -----------------------------
   
   procedure Generate_Case_Statement 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob     : Output_Buffer := This.Get_Output_Buffer;
      Use_If : Boolean := False;
      Alt    : Node_Id;
   begin
      Write_Indent_Str (Ob, "case ");
      Generate_Node (This, Expression (Node));
      Write_Str (Ob, " of ");
      Write_Eol (Ob);
      
      Indent_Begin (Ob);
      
      Alt := First (Alternatives (Node));
      while Present (Alt) loop
	 Write_Comment_Line_To_Node (Generator_Ptr (This), Alt);
	 --  Indent_Begin (Ob);
	 Generate_Case_Statement_Alternative (This, Alt);
	 --  Indent_End (Ob);
	 Next (Alt);
      end loop;
      
      Indent_End (Ob);
      Write_Indent_Str (Ob, "end_case;");
      Write_Eol (Ob);
   end Generate_Case_Statement;
   
   -----------------------------------------
   -- Generate_Case_Statement_Alternative --
   -----------------------------------------
   
   procedure Generate_Case_Statement_Alternative 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob          : Output_Buffer := This.Get_Output_Buffer;
      Choices     : constant List_Id := Discrete_Choices (Node);
      Choice      : Node_Id;
      Extra_Block : Boolean := False;
      Nxt         : Node_Id;
      Pref        : Node_Id;
   begin
      Choice := First (Choices);
      
      if Nkind (Choice) = N_Others_Choice then
	 Write_Comment_Line_To_Node (Generator_Ptr (This), Node);
	 Write_Indent_Str (Ob, "else");
	 
      else
	 Write_Indent_Str (Ob, "");
	 while Present (Choice) loop
	    Nxt := Next (Choice);
	    
	    --  Simple expression
	    
	    if not Nkind_In
	      (Choice, N_Range, N_Subtype_Indication, N_Attribute_Reference)
	      and then (not Is_Entity_Name (Choice)
			  or else not Is_Type (Entity (Choice)))
	    then
	       Write_Comment_Line_To_Node (Generator_Ptr (This), Choice);
	       Generate_Node (This, Choice);
	       
	       --  Range
	       
	    else
	       declare
		  LBD : Node_Id;
		  HBD : Node_Id;
		  
	       begin
		  Write_Comment_Line_To_Node (Generator_Ptr (This), Choice);
		  case Nkind (Choice) is
		     when N_Range =>
			LBD := Low_Bound  (Choice);
			HBD := High_Bound (Choice);
			
		     when N_Attribute_Reference =>
			pragma Assert (Attribute_Name (Choice) = Name_Range);
			Pref := Prefix (Choice);
			if Nkind_In (Pref, N_Identifier, N_Expanded_Name) then
			   pragma Assert (Is_Type (Entity (Pref)));
			   LBD := Type_Low_Bound  (Entity (Pref));
			   HBD := Type_High_Bound (Entity (Pref));
			end if;
			
		     when N_Subtype_Indication =>
			pragma Assert
			  (Nkind (Constraint (Choice)) = N_Range_Constraint);
			
			LBD :=
			  Low_Bound (Range_Expression (Constraint (Choice)));
			HBD :=
			  High_Bound (Range_Expression (Constraint (Choice)));
			
		     when others =>
			LBD := Type_Low_Bound  (Entity (Choice));
			HBD := Type_High_Bound (Entity (Choice));
		  end case;
		  
		  Write_Uint (Ob, Expr_Value (LBD));
		  Write_Str (Ob, " .. ");
		  Write_Uint (Ob, Expr_Value (HBD));
	       end;
	    end if;
	    
	    if Present (Nxt) then
	       Write_Str (Ob, ", ");
	    end if;
	 
	    Choice := Nxt;
	 end loop;
      end if;
      
      if Nkind (Choice) /= N_Others_Choice then
	 Write_Str (Ob, ": ");
      end if;
      Write_Eol (Ob);
      
      Indent_Begin (Ob);
      if Has_Non_Null_Statements (Statements (Node)) then
	 Generate_Sequence_Of_Statements (This, Statements (Node));
      else
	 Write_Str (Ob, ";");
      end if;
      Indent_End (Ob);
   end Generate_Case_Statement_Alternative;
   
   -----------------------------
   -- Generate_Code_Statement --
   -----------------------------
   
   procedure Generate_Code_Statement 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Code_Statement;
   
   ---------------------------------
   -- Generate_Compound_Statement --
   ---------------------------------
   
   procedure Generate_Compound_Statement 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Compound_Statement;
   
   -------------------------
   -- Generate_Elsif_Part --
   -------------------------
   
   procedure Generate_Elsif_Part 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Elsif_Part;
   
   -----------------------------
   -- Generate_Exit_Statement --
   -----------------------------
   
   procedure Generate_Exit_Statement 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob         : Output_Buffer := This.Get_Output_Buffer;
      Label      : Node_Id;
      Cond       : Node_Id;
      Loop_Stmt  : Node_Id;
      Loop_Label : Node_Id;
      Exit_Form  : Boolean;
   begin
      Write_Comment_Line_To_Node (Generator_Ptr (This), Node);
      
      Label := Name (Node);
      Cond  := Condition (Node);
      
      if No (Label) and then No (Cond) then
	 Write_Indent_Str (Ob, "exit;");
	 Write_Eol (Ob);
      else
	 Write_Indent_Str (Ob, "if ");
	 Generate_Node (This, Cond);
	 Write_Str (Ob, " then");
	 Write_Eol (Ob);
	 Indent_Begin (Ob);
	 Exit_Form := False;
	 if Present (Label) then
	    Loop_Stmt := Get_Inner_Loop_Node (Node);
	    if Present (Loop_Stmt) then
	       Loop_Label := Identifier (Loop_Stmt);
	       Exit_Form := Present (Loop_Label)
		 and then Entity (Loop_Label) = Entity (Label);
	    end if;
	    
	    if Exit_Form then
	       Write_Indent_Str (Ob, "exit;");
	    else
	       Write_Indent_Str (Ob, "jmp ");
	       pragma Assert (Nkind (Label) = N_Identifier);
	       Write_Name (Ob, Chars (Label));
	       Write_Str (Ob, ";");
	    end if;
	 else
	    Write_Indent_Str (Ob, "exit;");
	 end if;
	 Write_Eol (Ob);
	 Indent_End (Ob);
	 Write_Indent_Str (Ob, "end_if;");
	 Write_Eol (Ob);
      end if;
   end Generate_Exit_Statement;
   
   -----------------------------
   -- Generate_Free_Statement --
   -----------------------------
   
   procedure Generate_Free_Statement 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Free_Statement;
   
   ----------------------------
   -- Generate_Function_Call --
   ----------------------------
   
   procedure Generate_Function_Call 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Function_Call;
   
   -----------------------------
   -- Generate_Goto_Statement --
   -----------------------------
   
   procedure Generate_Goto_Statement 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
      Id : Node_Id;
   begin
      Id := Name (Node);
      
      if Present (Id) then
	 
	 pragma Assert (Nkind (Id) = N_Identifier);
	 
	 Write_Indent_Str (Ob, "jmp ");
	 Write_Id (Ob, Get_Name_String (Chars (Id)));
	 Write_Str (Ob, ";");
	 Write_Eol (Ob);
      end if;
   end Generate_Goto_Statement;
   
   ---------------------------------------------
   -- Generate_Handled_Sequence_Of_Statements --
   ---------------------------------------------
   
   procedure Generate_Handled_Sequence_Of_Statements 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob          : Output_Buffer := This.Get_Output_Buffer;
      Saved_Value : constant Boolean := This.Get_In_Package_Body_Init;
      Stmts       : List_Id;
      Stmt        : Node_id;
   begin
      This.Set_In_Package_Body_Init (Nkind (Parent (Node)) = N_Package_Body);
      
      Stmts := Statements (Node);
      if Is_Non_Empty_List (Stmts) then
         Stmt := First (Stmts);

         while Present (Stmt) loop
	    Write_Comment_Line_To_Node (Generator_Ptr (This), Stmt);
	    Generate_Node (This, Stmt);
            Next (Stmt);
         end loop;
      end if;
      
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
   end Generate_Handled_Sequence_Of_Statements;
   
   ---------------------------
   -- Generate_If_Statement --
   ---------------------------
   
   procedure Generate_If_Statement 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob  : Output_Buffer := This.Get_Output_Buffer;
      Els : Node_Id;
   begin
      Write_Eol (Ob);
      Write_Indent_Str (Ob, "if ");
      Generate_Node (This, Condition (Node));
      Write_Str (Ob, " then");
      Write_Eol (Ob);
      
      -- Open_Scope;
      Indent_Begin (Ob);
      Generate_Sequence_Of_Statements (This, Then_Statements (Node));
      Indent_End (Ob);
      
      if Present (Elsif_Parts (Node)) then
	 Els := First (Elsif_Parts (Node));
	 while Present (Els) loop
	    Write_Comment_Line_To_Node (Generator_Ptr (This), Els);
	    Write_Indent_Str (Ob, "elsif ");
	    Generate_Node (This, Condition (Els));
	    Write_Str (Ob, " then");
	    Write_Eol (Ob);
	    Indent_Begin (Ob);
	    Generate_Sequence_Of_Statements (This, Then_Statements (Els));
	    Indent_End (Ob);
	    Next (Els);
	 end loop;
      end if;
	 
      if Present (Else_Statements (Node)) then
	 Write_Indent_Str (Ob, "else ");
	 Write_Eol (Ob);
	 Indent_Begin (Ob);
	 Generate_Sequence_Of_Statements (This, Else_Statements (Node));
	 Indent_End (Ob);
      end if;
      
      Write_Indent_Str (Ob, "end_if;");
      Write_Eol (Ob);
   end Generate_If_Statement;
   
   -------------------------------
   -- Generate_Iteration_Scheme --
   -------------------------------
   
   procedure Generate_Iteration_Scheme 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Iteration_Scheme;
   
   -------------------------------------
   -- Generate_Iterator_Specification --
   -------------------------------------
   
   procedure Generate_Iterator_Specification 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Iterator_Specification;
   
   --------------------
   -- Generate_Label --
   --------------------
   
   procedure Generate_Label 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
      Id : Node_Id;
   begin
      Id := Identifier (Node);
      pragma Assert (Present (Id));
      Write_Indent (Ob);
      Write_Id (Ob, Id);
      Write_Str (Ob, ":");
      Write_Eol (Ob);
   end Generate_Label;
   
   -------------------------------------------
   -- Generate_Loop_Parameter_Specification --
   -------------------------------------------
   
   procedure Generate_Loop_Parameter_Specification 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Loop_Parameter_Specification;
   
   -----------------------------
   -- Generate_Loop_Statement --
   -----------------------------
   
   procedure Generate_Loop_Statement 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob               : Output_Buffer := This.Get_Output_Buffer;
      ISS              : constant Node_Id := Iteration_Scheme (Node);
      For_Loop_Var     : Entity_Id := Empty;
      --  Set to defining identifier of for loop variable for FOR loop
      LBD              : Node_Id;
      HBD              : Node_Id;
      For_Loop_Reverse : Boolean;
      --  Set True if reverse for loop, False for normal for loop
   begin
      Write_Eol (Ob);
      Write_Indent (Ob);
      --  if Present (Identifier (Node))
      --  	and then not Has_Created_Identifier (Node)
      --  then
      --  	 Write_Id (This, Identifier (Node));
      --  	 Write_Str (Ob, ": ");
      --  end if;
      
      --  Handle iteration scheme
      
      if Present (ISS) then
	 
	 --  WHILE loop case, generates C while
	 
	 if Present (Condition (ISS)) then
	    Write_Str (Ob, "while ");
	    Generate_Node (This, Condition (ISS));
	    Write_Str (Ob, " do ");
	    Write_Eol (Ob);
	    
	    Indent_Begin (Ob);
	    Generate_Sequence_Of_Statements (This, Statements (Node));
	    Indent_End (Ob);
	    
	    Write_Indent_Str (Ob, "end_while;");
	    Write_Eol (Ob);
	    
	    --  FOR loop case
	    
	 else
	    declare
	       LPS : constant Node_Id :=
		 Loop_Parameter_Specification (ISS);
	       DSD : constant Node_Id :=
		 Discrete_Subtype_Definition (LPS);
	       Rng : Node_Id;
	       
	    begin
	       For_Loop_Var     := Defining_Identifier (LPS);
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
	       
	       Write_Str (Ob, "for ");
	       Write_Id (Ob, For_Loop_Var);
	       Write_Str (Ob, " := ");
	       Generate_Node (This, LBD);
	       Write_Str (Ob, " to ");
	       Generate_Node (This, HBD);
	       if For_Loop_Reverse then
		  Write_Str (Ob, " by - 1 ");
	       end if;
	       Write_Str (Ob, " do");
	       Write_Eol (Ob);
	       
	       Indent_Begin (Ob);
	       Generate_Sequence_Of_Statements (This, Statements (Node));
	       Indent_End (Ob);
	       
	       Write_Indent_Str (Ob, "end_for;");
	       Write_Eol (Ob);
	    end;
	 end if;

	 --  No iteration scheme present
	 
      else
	 Write_Str (Ob, "while True do");
	 Write_Eol (Ob);
	    
	 Indent_Begin (Ob);
	 Generate_Sequence_Of_Statements (This, Statements (Node));
	 Indent_End (Ob);
	 
	 Write_Indent_Str (Ob, "end_while;");
	 Write_Eol (Ob);
      end if;
      
      --  Output label at end of loop as possible exit target
      
      if Present (Identifier (Node))
	and then not Has_Created_Identifier (Node)
      then
	 Write_Indent_Str (Ob, "");
	 Write_Id (Ob, Identifier (Node));
	 Write_Str (Ob, ": ");
	 Write_Eol (Ob);
      end if;
   end Generate_Loop_Statement;
   
   -----------------------------
   -- Generate_Null_Statement --
   -----------------------------
   
   procedure Generate_Null_Statement 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Write_Indent_Str (Ob, ";"); 
      Write_Eol (Ob);
   end Generate_Null_Statement;
   
   ------------------------------------
   -- Generate_Parameter_Association --
   ------------------------------------
   
   procedure Generate_Parameter_Association 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Parameter_Association;
   
   ---------------------------------------
   -- Generate_Procedure_Call_Statement --
   ---------------------------------------
   
   procedure Generate_Procedure_Call_Statement 
     (This        : access Unity_Generator_Record;
      Node        : Node_Id;
      As_Function : Boolean := False) is
      
      Ob            : Output_Buffer := This.Get_Output_Buffer;
      Call          : Entity_Id;
      Formal        : Node_Id;
      Actual        : Node_Id;
      Kind          : Entity_Kind;
      Instance_Name : Name_Id;
      Scp           : Entity_Id;
      Frst_Seen     : Boolean;
      Gen           : Generation_Type;
      Global        : Entity_Id;
   begin
      Put_Line ("Generate_Procedure_Call_Statement Begin");
      Call := Entity (Name (Node));
      Put_Line ("1 "); --  & Get_String (Chars (Call)));
      Scp := Get_Scope_Entity (This);
      Put_Line ("2");
      
      Gen := Get_Generation_Type (Call);
      Put_Line ("3");
      
      Gen := Sr_Type;
      Gen := Fb_Type;
      if Gen = Section_Type then
	 null;
	 
      elsif Gen = Sr_Type then
	 
	 --  Assign In Parameters before call
	 
	 Actual := First_Actual (Node);
	 Formal := First_Formal_With_Extras (Call);
	 while Present (Actual) loop
	    
	    Global := Get_Formal_Global (Formal);
	    pragma Assert (Present (Global));
	    
	    Kind := Ekind (Formal);
	    if Kind = E_In_Parameter 
	      or else Kind = E_In_Out_Parameter
	    then
	       Write_Indent_Str (Ob, "");
	       Write_Id (Ob, Global);
	       Write_Str (Ob, " := ");
	       Generate_Node (This, Actual);
	       Write_Str (Ob, ";");
	       Write_Eol (Ob);
	    end if;
	    
	    Next_Actual (Actual);
	    Next_Formal_With_Extras (Formal);
	 end loop;
	 
	 --  Generate Call
	 
	 Write_Indent_Str (Ob, "");
	 Write_Id (Ob, Call);
	 Write_Str (Ob, "();");
	 Write_Eol (Ob);
	 
	 --  Assign Actuals from Out Parameters after call
	 
	 Actual := First_Actual (Node);
	 Formal := First_Formal_With_Extras (Call);
	 while Present (Actual) loop
	    Global := Get_Formal_Global (Formal);
	    pragma Assert (Present (Global));
	    
	    Kind := Ekind (Formal);
	    if Kind = E_In_Out_Parameter 
	      or else Kind = E_Out_Parameter
	    then
	       Write_Indent_Str (Ob, "");
	       Generate_Node (This, Actual);
	       Write_Str (Ob, " := ");
	       Write_Id (Ob, Global);
	       Write_Str (Ob, ";");
	       Write_Eol (Ob);
	    end if;
	    
	    Next_Actual (Actual);
	    Next_Formal_With_Extras (Formal);
	 end loop;
	 
      else
	 Instance_Name := Get_Instance_Name_In_Scope (Scp, Call);
	 
	 --  Retreive The Instance Name From Calld of enclosing subprogram
	 
	 if Instance_Name /= No_Name then
	    if not As_Function then
	       Write_Indent_Str (Ob, "");
	    end if;
	    Write_Name (Ob, Instance_Name);
	    
	 else
	    if not As_Function then
	       Write_Indent_Str (Ob, "");
	    end if;
	    Write_Id (Ob, Call);
	 end if;
	 
	 Write_Str (Ob, " (");
	 
	 --  First Generate the IN Parameters
	 
	 Frst_Seen := False;
	 
	 Actual := First_Actual (Node);
	 Formal := First_Formal_With_Extras (Call);
	 while Present (Actual) loop
	    Kind := Ekind (Formal);
	    if Kind = E_In_Parameter then
	       if Frst_Seen then
		  Write_Str (Ob, ", ");
	       end if;
	       Generate_Node (This, Actual);
	       Frst_Seen := True;
	    end if;
	    
	    Next_Actual (Actual);
	    Next_Formal_With_Extras (Formal);
	 end loop;
	 
	 --  Then Generate the IN OUT parameters
	 
	 Actual := First_Actual (Node);
	 Formal := First_Formal_With_Extras (Call);
	 while Present (Actual) loop
	    Kind := Ekind (Formal);
	    if Kind = E_In_Out_Parameter then
	       if Frst_Seen then
		  Write_Str (Ob, ", ");
	    end if;
	    Generate_Node (This, Actual);
	    Frst_Seen := True;
	    end if;
	    
	    Next_Actual (Actual);
	    Next_Formal_With_Extras (Formal);
	 end loop;
	 
	 
	 --  Last Generate the OUT parameters
	 
	 Actual := First_Actual (Node);
	 Formal := First_Formal_With_Extras (Call);
	 while Present (Actual) loop
	    Kind := Ekind (Formal);
	    if Kind = E_Out_Parameter then
	       if Frst_Seen then
		  Write_Str (Ob, ", ");
	       end if;
	       Generate_Node (This, Actual);
	       Frst_Seen := True;
	    end if;
	    
	    Next_Actual (Actual);
	    Next_Formal_With_Extras (Formal);
	 end loop;
	 
	 if As_Function then
	    Write_Str (Ob, ")");
	 else
	    Write_Str (Ob, ");");
	    Write_Eol (Ob);
	    end if;

      end if;
      Put_Line ("Generate_Procedure_Call_Statement End");
   end Generate_Procedure_Call_Statement;
   
   procedure Generate_Procedure_Call_Statement_Old
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob            : Output_Buffer := This.Get_Output_Buffer;
      Call          : Entity_Id;
      Formal        : Node_Id;
      Actual        : Node_Id;
      Param         : Node_Id;
      Params        : List_Id;
      Kind          : Entity_Kind;
      Instance_Name : Name_Id;
      Scp           : Entity_Id;
      Frst_Seen     : Boolean;
      Gen           : Generation_Type;
      Global        : Entity_Id;
   begin
      Call := Entity (Name (Node));
      Scp := Get_Scope_Entity (This);
      
      Gen := Get_Generation_Type (Call);
      
      Gen := Sr_Type;
      Gen := Fb_Type;
      if Gen = Section_Type then
	 null;
	 
      elsif Gen = Sr_Type then
	 
	 --  Assign In Parameters before call
	 
	 Params := Parameter_Associations (Node);
	 Param := First (Params);
	 Formal := First_Formal_With_Extras (Call);
	 while Present (Param) loop
	    if Nkind (Param) = N_Parameter_Association then 
	       Actual := Explicit_Actual_Parameter (Param);
	    else
	       Actual := Param;
	    end if;
	    
	    Global := Get_Formal_Global (Formal);
	    pragma Assert (Present (Global));
	    
	    Kind := Ekind (Formal);
	    if Kind = E_In_Parameter 
	      or else Kind = E_In_Out_Parameter
	    then
	       Write_Indent_Str (Ob, "");
	       Write_Id (Ob, Global);
	       Write_Str (Ob, " := ");
	       Generate_Node (This, Actual);
	       Write_Str (Ob, ";");
	       Write_Eol (Ob);
	    end if;
	    
	    Next (Param);
	    Next_Formal_With_Extras (Formal);
	 end loop;
	 
	 --  Generate Call
	 
	 Write_Indent_Str (Ob, "");
	 Write_Id (Ob, Call);
	 Write_Str (Ob, "();");
	 Write_Eol (Ob);
	 
	 --  Assign Actuals from Out Parameters after call
	 
	 Params := Parameter_Associations (Node);
	 Param := First (Params);
	 Formal := First_Formal_With_Extras (Call);
	 while Present (Param) loop
	    if Nkind (Param) = N_Parameter_Association then 
	       Actual := Explicit_Actual_Parameter (Param);
	    else
	       Actual := Param;
	    end if;
	    
	    Global := Get_Formal_Global (Formal);
	    pragma Assert (Present (Global));
	    
	    Kind := Ekind (Formal);
	    if Kind = E_In_Out_Parameter 
	      or else Kind = E_Out_Parameter
	    then
	       Write_Indent_Str (Ob, "");
	       Generate_Node (This, Actual);
	       Write_Str (Ob, " := ");
	       Write_Id (Ob, Global);
	       Write_Str (Ob, ";");
	       Write_Eol (Ob);
	    end if;
	    
	    Next (Param);
	    Next_Formal_With_Extras (Formal);
	 end loop;
	 
      else
      
	 Instance_Name := Get_Instance_Name_In_Scope (Scp, Call);
	 
	 --  Retreive The Instance Name From Calld of enclosing subprogram
	 
	 if Instance_Name /= No_Name then
	    Write_Indent_Str (Ob, "");
	    Write_Name (Ob, Instance_Name);
	    
	 else
	    Write_Indent_Str (Ob, "");
	    Write_Id (Ob, Call);
	 end if;
	 
	 Write_Str (Ob, " (");
	 
	 --  First Generate the IN Parameters
	 
	 Frst_Seen := False;
	 
	 Params := Parameter_Associations (Node);
	 Param := First (Params);
	 Formal := First_Formal_With_Extras (Call);
	 while Present (Param) loop
	    if Nkind (Param) = N_Parameter_Association then 
	       Actual := Explicit_Actual_Parameter (Param);
	    else
	       Actual := Param;
	    end if;
	    
	    Kind := Ekind (Formal);
	    if Kind = E_In_Parameter then
	       if Frst_Seen then
		  Write_Str (Ob, ", ");
	       end if;
	       Generate_Node (This, Actual);
	       Frst_Seen := True;
	    end if;
	    
	    Next (Param);
	    Next_Formal_With_Extras (Formal);
	 end loop;
	 
	 --  Then Generate the IN OUT parameters
	 
	 Params := Parameter_Associations (Node);
	 Param := First (Params);
	 Formal := First_Formal_With_Extras (Call);
	 while Present (Param) loop
	    if Nkind (Param) = N_Parameter_Association then 
	       Actual := Explicit_Actual_Parameter (Param);
	 else
	    Actual := Param;
	    end if;
	    
	    Kind := Ekind (Formal);
	    if Kind = E_In_Out_Parameter then
	       if Frst_Seen then
		  Write_Str (Ob, ", ");
	    end if;
	    Generate_Node (This, Actual);
	    Frst_Seen := True;
	    end if;
	    
	    Next (Param);
	    Next_Formal_With_Extras (Formal);
	 end loop;
	 
	 
	 --  Last Generate the OUT parameters
	 
	 Params := Parameter_Associations (Node);
	 Param := First (Params);
	 Formal := First_Formal_With_Extras (Call);
	 while Present (Param) loop
	    if Nkind (Param) = N_Parameter_Association then 
	       Actual := Explicit_Actual_Parameter (Param);
	    else
	       Actual := Param;
	    end if;
	    
	    Kind := Ekind (Formal);
	    if Kind = E_Out_Parameter then
	       if Frst_Seen then
		  Write_Str (Ob, ", ");
	       end if;
	       Generate_Node (This, Actual);
	       Frst_Seen := True;
	    end if;
	    
	    Next (Param);
	    Next_Formal_With_Extras (Formal);
	 end loop;
	 
	 Write_Str (Ob, ");");
	 Write_Eol (Ob);
      end if;
   end Generate_Procedure_Call_Statement_Old;
   
   -------------------------------
   -- Generate_Raise_Expression --
   -------------------------------
   
   procedure Generate_Raise_Expression 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Raise_Expression;
   
   --------------------------
   -- Generate_Raise_Error --
   --------------------------
   
   procedure Generate_Raise_Error 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Raise_Error;
   
   --------------------------------------
   -- Generate_Simple_Return_Statement --
   --------------------------------------
   
   procedure Generate_Simple_Return_Statement 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Simple_Return_Statement;

end Unity.Gen.Ch5;
