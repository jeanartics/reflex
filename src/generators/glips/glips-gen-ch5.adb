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

with Artics.Buffers; use Artics.Buffers;
with Reflex.Gen.Types; use Reflex.Gen.Types;
with Reflex.Gen.Outputs; use Reflex.Gen.Outputs;
with Reflex.Gen.Utils; use Reflex.Gen.Utils;

--with Reflex.Global_Arecs; use Reflex.Global_Arecs;
with Reflex.Infos; use Reflex.Infos;

with Glips.Gen.Ch4; use Glips.Gen.Ch4;

package body Glips.Gen.Ch5 is
   
   -------------------------------------
   -- Generate_Sequence_Of_Statements --
   -------------------------------------
   
   procedure Generate_Sequence_Of_Statements
     (This  : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Call;
   
   -----------------------------------
   -- Generate_Assignment_Statement --
   -----------------------------------
   
   procedure Generate_Assignment_Statement 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob  : Output_Buffer := This.Get_Output_Buffer;
      LHS : Node_Id := Name (Node);
      RHS : Node_Id := Expression (Node);
   begin
      Write_Indent (Ob);
      Generate_Node (This, LHS);
      Write_Str (Ob, " := ");
      Generate_Node (This, RHS);
      
      Write_Char (Ob, ';');
      Write_Eol (Ob);
   end Generate_Assignment_Statement;
   
   ------------------------------
   -- Generate_Block_Statement --
   ------------------------------
   
   procedure Generate_Block_Statement 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob    : Output_Buffer := This.Get_Output_Buffer;
      Id    : Node_Id;
      Decls : List_Id;
      Stmts : Node_Id;
   begin
      Write_Indent_Str (Ob, "");
      if not Has_Created_Identifier (Node) then
	 Id := Identifier (Node);
	 Write_Id (Ob, Id);
	 Write_Str (Ob, ": ");
      end if;
      
      Write_Str (Ob, "declare");
      Write_Eol (Ob);
      Decls := Declarations (Node);
      if not Is_Empty_List (Decls) then
	 Indent_Begin (Ob);
	 Generate_Node_List (This, Decls);
	 Indent_End (Ob);
      end if;
      Write_Indent_Str (Ob, "begin");
      Write_Eol (Ob);
      Indent_Begin (Ob);
      Stmts := Handled_Statement_Sequence (Node);
      Generate_Sequence_Of_Statements (This, Statements (Stmts));      
      Indent_End (Ob);
      Write_Indent_Str (Ob, "end;");
      Write_Eol (Ob);
   end Generate_Block_Statement;
   
   -----------------------------
   -- Generate_Case_Statement --
   -----------------------------
   
   procedure Generate_Case_Statement 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob     : Output_Buffer := This.Get_Output_Buffer;
      Use_If : Boolean := False;
      Alt    : Node_Id;
   begin
      Write_Indent_Str (Ob, "case ");
      Generate_Node (This, Expression (Node));
      Write_Str (Ob, " is ");
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
      Write_Indent_Str (Ob, "end case;");
      Write_Eol (Ob);
   end Generate_Case_Statement;
   
   -----------------------------------------
   -- Generate_Case_Statement_Alternative --
   -----------------------------------------
   
   procedure Generate_Case_Statement_Alternative 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob          : Output_Buffer := This.Get_Output_Buffer;
      Choices     : constant List_Id := Discrete_Choices (Node);
      Choice      : Node_Id;
      Extra_Block : Boolean := False;
      Nxt         : Node_Id;
   begin
      Choice := First (Choices);
      
      if Nkind (Choice) = N_Others_Choice then
	 Write_Comment_Line_To_Node (Generator_Ptr (This), Node);
	 Write_Indent_Str (Ob, "when others");
	 
      else
	 Write_Indent_Str (Ob, "when ");
	 while Present (Choice) loop
	    Nxt := Next (Choice);
	    
	    --  Simple expression
	    
	    if not Nkind_In (Choice, N_Range, N_Subtype_Indication)
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
		  
		  Write_Uint (Ob, Expr_Value (LBD));
		  Write_Str (Ob, " .. ");
		  Write_Uint (Ob, Expr_Value (HBD));
	       end;
	    end if;
	    
	    if Present (Nxt) then
	       Write_Str (Ob, " | ");
	    end if;
	 
	    Choice := Nxt;
	 end loop;
      end if;
      
      Write_Str (Ob, " => ");
      Write_Eol (Ob);
      
      Indent_Begin (Ob);
      if Has_Non_Null_Statements (Statements (Node)) then
	 Generate_Sequence_Of_Statements (This, Statements (Node));
      else
	 Write_Indent_Str (Ob, "null;");
	 Write_Eol (Ob);
      end if;
      Indent_End (Ob);
   end Generate_Case_Statement_Alternative;
   
   -----------------------------
   -- Generate_Code_Statement --
   -----------------------------
   
   procedure Generate_Code_Statement 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Code_Statement;
   
   ---------------------------------
   -- Generate_Compound_Statement --
   ---------------------------------
   
   procedure Generate_Compound_Statement 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Compound_Statement;
   
   -------------------------
   -- Generate_Elsif_Part --
   -------------------------
   
   procedure Generate_Elsif_Part 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Elsif_Part;
   
   -----------------------------
   -- Generate_Exit_Statement --
   -----------------------------
   
   procedure Generate_Exit_Statement 
     (This : access Glips_Generator_Record;
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
      
      Write_Indent_Str (Ob, "exit");
      if Present (Label) then
	 Write_Str (Ob, " ");
	 Write_Id (Ob, Label);
      end if;
      
      if Present (Cond) then
	 Write_Str (Ob, " when ");
	 Generate_Node (This, Cond);
      end if;
      
      Write_Str (Ob, ";");
      Write_Eol (Ob);
   end Generate_Exit_Statement;
   
   -----------------------------
   -- Generate_Free_Statement --
   -----------------------------
   
   procedure Generate_Free_Statement 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Free_Statement;
   
   ----------------------------
   -- Generate_Function_Call --
   ----------------------------
   
   procedure Generate_Function_Call 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      Write_Comment_Line_To_Node (This, Node);
      
      Glips.Gen.Ch4.Generate_Call (This, Node);
   end Generate_Function_Call;
   
   -----------------------------
   -- Generate_Goto_Statement --
   -----------------------------
   
   procedure Generate_Goto_Statement 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
      Id : Node_Id;
   begin
      Id := Name (Node);
      
      if Present (Id) then
	 
	 pragma Assert (Nkind (Id) = N_Identifier);
	 
	 Write_Indent_Str (Ob, "goto ");
	 Write_Id (Ob, Get_Name_String (Chars (Id)));
	 Write_Str (Ob, ";");
	 Write_Eol (Ob);
      end if;
   end Generate_Goto_Statement;
   
   ---------------------------------------------
   -- Generate_Handled_Sequence_Of_Statements --
   ---------------------------------------------
   
   procedure Generate_Handled_Sequence_Of_Statements 
     (This : access Glips_Generator_Record;
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
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob    : Output_Buffer := This.Get_Output_Buffer;
      Els   : Node_Id;
      Stmts : List_Id;
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
      
      Stmts := Else_Statements (Node);
      if not Is_Empty_List (Stmts) then
	 Write_Indent_Str (Ob, "else ");
	 Write_Eol (Ob);
	 Indent_Begin (Ob);
	 Generate_Sequence_Of_Statements (This, Stmts);
	 Indent_End (Ob);
      end if;
      
      Write_Indent_Str (Ob, "end if;");
      Write_Eol (Ob);
   end Generate_If_Statement;
   
   -------------------------------
   -- Generate_Iteration_Scheme --
   -------------------------------
   
   procedure Generate_Iteration_Scheme 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Iteration_Scheme;
   
   -------------------------------------
   -- Generate_Iterator_Specification --
   -------------------------------------
   
   procedure Generate_Iterator_Specification 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Iterator_Specification;
   
   --------------------
   -- Generate_Label --
   --------------------
   
   procedure Generate_Label 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
      Id : Node_Id;
   begin
      Id := Identifier (Node);
      pragma Assert (Present (Id));
      Write_Str (Ob, "<<");
      Write_Indent (Ob);
      Write_Id (Ob, Id);
      Write_Str (Ob, ">>");
      Write_Eol (Ob);
   end Generate_Label;
   
   -------------------------------------------
   -- Generate_Loop_Parameter_Specification --
   -------------------------------------------
   
   procedure Generate_Loop_Parameter_Specification 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Loop_Parameter_Specification;
   
   -----------------------------
   -- Generate_Loop_Statement --
   -----------------------------
   
   procedure Generate_Loop_Statement 
     (This : access Glips_Generator_Record;
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
	    Write_Str (Ob, " loop ");
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
	       if For_Loop_Reverse then
		  Write_Str (Ob, " in reverse ");
	       else
		  Write_Str (Ob, " in ");
	       end if;
	       Generate_Node (This, LBD);
	       Write_Str (Ob, " .. ");
	       Generate_Node (This, HBD);
	       Write_Str (Ob, " loop");
	       Write_Eol (Ob);
	    end;
	    
	 end if;

	 --  No iteration scheme present
	 
      else
	 Write_Str (Ob, "loop");
	 Write_Eol (Ob);
      end if;
      
      Indent_Begin (Ob);
      Generate_Sequence_Of_Statements (This, Statements (Node));
      Indent_End (Ob);
      
      Write_Indent_Str (Ob, "end loop;");
      Write_Eol (Ob);
      
      --  Output label at end of loop as possible exit target
      
      if Present (Identifier (Node))
	and then not Has_Created_Identifier (Node)
      then
	 Write_Indent_Str (Ob, "<<");
	 Write_Id (Ob, Identifier (Node));
	 Write_Str (Ob, ">> ");
	 Write_Eol (Ob);
      end if;
   end Generate_Loop_Statement;
   
   -----------------------------
   -- Generate_Null_Statement --
   -----------------------------
   
   procedure Generate_Null_Statement 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is

      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Write_Comment_Line_To_Node (This, Node);
      
      Write_Indent_Str (Ob, "null;");
      Write_Eol (Ob);
   end Generate_Null_Statement;
   
   ------------------------------------
   -- Generate_Parameter_Association --
   ------------------------------------
   
   procedure Generate_Parameter_Association 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Parameter_Association;
   
   ---------------------------------------
   -- Generate_Procedure_Call_Statement --
   ---------------------------------------
   
   procedure Generate_Procedure_Call_Statement 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob : output_Buffer := This.get_Output_Buffer;
   begin
      Write_Comment_Line_To_Node (This, Node);
      
      Glips.Gen.Ch4.Generate_Call (This, Node);
      
      Write_Char (Ob, ';');
      Write_Eol (Ob);
   end Generate_Procedure_Call_Statement;
   
   -------------------------------
   -- Generate_Raise_Expression --
   -------------------------------
   
   procedure Generate_Raise_Expression 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Raise_Expression;
   
   --------------------------
   -- Generate_Raise_Error --
   --------------------------
   
   procedure Generate_Raise_Error 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Raise_Error;
   
   --------------------------------------
   -- Generate_Simple_Return_Statement --
   --------------------------------------
   
   procedure Generate_Simple_Return_Statement 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      if Present (Expression (Node)) then
	 Write_Indent_Str (Ob, "return ");
	 Generate_Node (This, Expression (Node));
      else
	 Write_Indent_Str (Ob, "return");
      end if;
      Write_Char (Ob, ';');
      Write_Eol (Ob);
   end Generate_Simple_Return_Statement;

end Glips.Gen.Ch5;
