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

with Sinfo;     use Sinfo;
with Einfo;     use Einfo;
with Atree;     use Atree;
with Namet;     use Namet;
with Nlists;    use Nlists;
with Nmake;     use Nmake;
with Sem_Util;  use Sem_Util;
with Tbuild;    use Tbuild;

with Reflex.Expanders.Expressions;  use Reflex.Expanders.Expressions;
with Reflex.Expanders.Dispatch;     use Reflex.Expanders.Dispatch;
with Reflex.Boxes.Utils;            use Reflex.Boxes.Utils;
with Reflex.Infos;                  use Reflex.Infos;
with Reflex.Expanders.Ch5;

package body Reflex.Boxes.Exp_Ch5 is
   
   ------------------------------
   -- Break_If_Statements_List --
   ------------------------------
   
   procedure Break_If_Statements_List 
     (This        : access Reflex_Expander_Record;
      Insert_Node : Node_Id;
      Stmts       : in out List_Id;
      Cond        : Node_Id) is
      
      New_If    : Node_Id;
      New_Stmts : List_Id;
      Nxt       : Node_Id;
      Stmt      : Node_Id;
      Count     : Natural;
   begin
      Put_Line ("Break_If_Statements_List Begin");
      New_If :=
        Make_If_Statement
          (Sloc            => Sloc (Insert_Node),
           Condition       =>  Cond,
           Then_Statements => New_List);

      Insert_Before (Insert_Node, New_If);
      New_Stmts := Then_Statements (New_If);
      
      Stmt := First (Stmts);

      Nxt := Next (Stmt);

      Count := 0;
      while Present (Stmt) loop
         if Nkind (Stmt) = N_Procedure_Call_Statement then
            Remove (Stmt);
            Append (Stmt, New_Stmts);
            Break_If_Statements_List (This, Insert_Node, Stmts, New_Copy_Tree (Cond));
            return;
         end if;

         Count := Count + 1;
         if Count > Max_Unity_Ladder_Vertical then
            Break_If_Statements_List (This, Insert_Node, Stmts, New_Copy_Tree (Cond));
            return;
         end if;

         --           Put_Line ("  Count = " & Count'Img);
         Remove (Stmt);
         Append (Stmt, New_Stmts);
         Stmt := Nxt;
         Nxt := Next (Nxt);

      end loop;
      
      Put_Line ("Break_If_Statements_List End");
      New_Line;
   end Break_If_Statements_List;
     
   ------------------------
   -- Break_If_Statement --
   ------------------------

   procedure Break_If_Statement
     (This : access Reflex_Expander_Record;
      N    : Node_Id) is
      
      Stmts : List_Id;
      Elif  : Node_Id;
   begin
      Stmts := Then_Statements (N);
      Break_If_Statements_List
        (This        => This,
         Insert_Node => N,
         Stmts       => Stmts,
         Cond        => Condition (N));
	
      if Present (Elsif_Parts (N)) then
	    
         Elif := First (Elsif_Parts (N));
         while Present (Elif) loop
            Stmts := Then_Statements (Elif);
            Break_If_Statements_List
              (This        => This,
               Insert_Node => N,
               Stmts       => Stmts,
               Cond        => Condition (Elif));
	    
            Next (Elif);
         end loop;
      end if;
      
      if Present (Else_Statements (N)) then
         Insert_List_Before (N, Else_Statements (N));
      end if;
      
      Remove (N);
   end Break_If_Statement;
   
   -------------------------------------------
   -- Expand_Simple_If_Statement_For_Ladder --
   -------------------------------------------

   procedure Expand_Simple_If_Statement_For_Ladder
     (This : access Reflex_Expander_Record;
      N    : Node_Id) is
      
      Node    : Node_Id;
      E       : Entity_Id;
      Elif    : Node_Id;
      Stmts   : List_Id;
      Stmt    : Node_Id;
      Lend    : Node_Id;
      Lab_End : Node_Id;
      Jmp     : Node_Id;
   begin
      Put_Line ("Expand_Simple_If_Statement_For_Ladder begin");
      
      if Present (Elsif_Parts (N)) or Present (Else_Statements (N)) then
	 
         Node := This.Get_Scope_Node;
         E := Unique_Defining_Entity (Node);
	 
         --  Create le end label
	 
         Lend := Make_Unique_Label_Entity
           (This, Sloc (E), String_Find ("Lend"));
	 
         --  Add got end at the end of statement list of the then part
	 
         Jmp := Make_Goto_Statement
           (Sloc (N), 
            New_Occurrence_Of (Lend, Sloc (N)));
         Append (Jmp, Then_Statements (N));

         if Present (Elsif_Parts (N)) then
	    
            Elif := First (Elsif_Parts (N));
            while Present (Elif) loop
               
               Stmts := Then_Statements (Elif);
               if Present (Next (Elif))
                 or else Present (Else_Statements (N)) 
               then
                  Jmp := Make_Goto_Statement
                    (Sloc (N), 
                     New_Occurrence_Of (Lend, Sloc (Elif)));
		  
                  Append (Jmp, Stmts);
               end if;
	       
               Next (Elif);
            end loop;
         end if;
         if Present (Else_Statements (N)) then
            Stmt := First (Else_Statements (N));
            while Present (Stmt) loop
               Expand_Node (This,Stmt);
               Next (Stmt);
            end loop;
         end if;

         Lab_End := Make_Label (Sloc (N), New_Occurrence_Of (Lend, Sloc (N)));
         Insert_After (N, Lab_End);

      end if;
      Put_Line ("Expand_Simple_If_Statement_For_Ladder End");
      New_Line;
   end Expand_Simple_If_Statement_For_Ladder;
   
   ---------------------------------
   -- Expand_If_Statement_As_Goto --
   ---------------------------------
   
   procedure Expand_If_Statement_As_Goto
     (This : access Reflex_Expander_Record;
      N    : Node_Id) is
   
      Loc         : constant Source_Ptr := Sloc (N);
      New_If      : Node_Id;
      E           : Node_Id;
      Alt_Present : Boolean;
      Stmts       : List_Id;
      Stmt        : Node_Id;
      Lnxt        : Entity_Id;
      Lend        : Entity_Id;
      Cond        : Node_Id;
      Nxt         : Node_Id;
   begin
      Put_Line ("Expand_If_Statement_As_Goto Begin");
         
      -- Negate condition of all conditons of the branches and change if and
      -- elsif as if not cond then jmp next_branch expcept for else part.
         
      Alt_Present := Present (Elsif_Parts (N)) 
        or else Present (Else_Statements (N));
         
      --  If no alternative present other than the if altrnative. Create a end
      --  label. In others cases the procedure 
      --  Expand_Simple_If_Statement_For_Ladder has already do the job
         
      Lend := Make_Unique_Label_Entity (This, Loc, String_Find ("Lend"));
      Lnxt := Lend;
         
      if Alt_Present then
         Lnxt := Make_Unique_Label_Entity (This, Loc, String_Find ("L"));
      end if;
         
      --  If part
         
      Cond := Condition (N);
      Negate_Expr (Cond);
         
      New_If :=
        Make_If_Statement
          (Sloc            => Loc,
           Condition       => Cond,
           Then_Statements => 
             New_List 
               (Make_Goto_Statement
                  (Loc, New_Occurrence_Of (Lnxt, Loc))));
   	   
      Insert_Before (N, New_If);
      Stmts := Then_Statements (N);
      if Alt_Present then 
         Append
           (Make_Goto_Statement
              (Loc, New_Occurrence_Of (Lend, Loc)),
            Stmts);
      end if;
      Insert_List_Before (N, Stmts);
         
      --  Elsif Part
         
      if Present (Elsif_Parts (N)) then
   	 
         E := First (Elsif_Parts (N));
         while Present (E) loop
            Nxt := Next (E);
   	    
            Insert_Before
              (N, 
               Make_Label (Sloc (E), New_Occurrence_Of (Lnxt, Sloc (E))));
   	    
            Cond := Condition (E);
            Negate_Expr (Cond);
   	    
            if Present (Nxt) or else Present (Else_Statements (N)) then
               Lnxt := Make_Unique_Label_Entity (This, Sloc (E), String_Find ("L"));
            else
               Lnxt := Lend;
            end if;
   	    
            New_If :=
              Make_If_Statement
                (Sloc            => Loc,
                 Condition       => Cond,
                 Then_Statements => 
                   New_List 
                     (Make_Goto_Statement
                        (Loc, New_Occurrence_Of (Lnxt, Loc))));
   	    
            Insert_Before (N, New_If);
            Stmts := Then_Statements (E);
            if Present (Nxt) or else Present (Else_Statements (N)) then 
               Append
                 (Make_Goto_Statement
                    (Sloc (E), New_Occurrence_Of (Lend, Sloc (E))),
                  Stmts);
            end if;
            Insert_List_Before (N, Stmts);
   	    
            E := Nxt;
         end loop;
      end if;
         
      --  Else Part
   
      if Present (Else_Statements (N)) then
         Stmt := First (Else_Statements (N));
         while Present (Stmt) loop
            Expand_Node (This,Stmt);
            Next (Stmt);
         end loop;
         Insert_Before
           (N, 
            Make_Label (Loc, New_Occurrence_Of (Lnxt, Loc)));
         
         Stmts := Else_Statements (N);
     
         Insert_List_Before (N, Stmts);
      end if;
      
      Insert_Before
        (N, 
         Make_Label (Sloc (E), New_Occurrence_Of (Lend, Loc)));
         
      Remove (N);
      Put_Line ("Expand_If_Statement_As_Goto End");
      New_Line;
   end Expand_If_Statement_As_Goto;
   
   --------------------------------------
   -- Expand_Case_Statement_For_Ladder --
   --------------------------------------

   procedure Expand_Case_Statement_For_Ladder
     (This : access Reflex_Expander_Record;
      N    : Node_Id) is

      Alt        : Node_Id;
      Choice     : Node_Id;
      Cond       : Node_Id;
      Lhs        : Node_Id;
      Rhs        : Node_Id;
      If_Node    : Node_Id;
      Elsif_Node : Node_Id;
      Cur_Node   : Node_Id;
      Elsif_List : List_Id := New_List;
      Expr       : Node_Id;
      Lop        : Node_Id;
      Rop        : Node_Id;
      Stmts      : List_Id;
   begin
      Put_Line ("Expand_Case_Statement_For_Ladder Begin");
      
      Alt := First (Alternatives (N));
      loop
         --  First alternative, use if

         if No (Prev (Alt)) then
            If_Node := Make_If_Statement (Sloc (N), Empty, New_List);
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
            Reflex.Expanders.Ch5.Expand_Sequence_Of_Statements (This, Stmts);
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
               Lhs := Expression (N);
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

                  Lhs := Expression (N);
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
         
         Reflex.Expanders.Ch5.Expand_Sequence_Of_Statements (This, Stmts);

         Set_Then_Statements (Cur_Node, Stmts);

         Next (Alt);
      end loop;

      --  Here we have a Node_If which may contain Elsif Parts or/and Else
      --  Parts Then we call if's expander
      --  Here we have a Node_If which may contain Elsif Parts or/and Else
      --  Parts Then we call if's expander
       
      -- ---- ??? JMA Expand_If_Statement_For_Ladder (This, If_Node);
      Insert_Before (N, If_Node);
      Remove (N);
      Expand_Node (This, If_Node);

      -- Expand_Node (This, If_Node);
      --  Insert If Statement before Case Statement and remove the
      --  Case Statment

      Put_Line ("Expand_Case_Statement_For_Ladder End");
      New_Line;
   end Expand_Case_Statement_For_Ladder;

   --------------------------------------
   -- Expand_Loop_Statement_For_Ladder --
   --------------------------------------

   procedure Expand_Loop_Statement_For_Ladder
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

      Goto_Begin_Id    : Node_Id;
      Goto_Begin       : Node_Id;
      Label_Begin      : Node_Id;

      Goto_End_Id      : Node_Id;
      Goto_End         : Node_Id;
      Label_End        : Node_Id;

      Stmts            : List_Id;
      New_If           : Node_Id;

      Has_Open_Scope   : Boolean := False;
   begin
      Put_Line ("Expand_Loop_Statement_For_Ladder Begin");
      
      --  Handle iteration scheme

      if Present (ISS) then

         --  WHILE loop case

         if Present (Condition (ISS)) then
            Put_Line ("   While case");

            --  Expand :

            --  While CONDITION loop
            --       STATEMENTS;
            --  end loop;

            --  To

            --  <<Lbegin>>
            --  if not CONDITION then
            --       goto Lend;
            --  end if;

            --  STATEMENTS;
            --  goto Lbegin;

            --  <<Lend>>;
            
            Goto_Begin_Id := Make_Unique_Label_Entity (This, Sloc (Node), String_Find ("Lbegin"));
            Label_Begin := Make_Label (Sloc (Node), Goto_Begin_Id);
            Declare_Label_Current_Scope (This, Label_Begin);

            Insert_Before (Node, Label_Begin);

            Negate_Expr (Condition (ISS));

            Goto_End_Id := Make_Unique_Label_Entity (This, Sloc (Node), String_Find ("Lend"));
            Goto_End := Make_Goto_Statement (Sloc (Node), Goto_End_Id);

            New_If :=
              Make_If_Statement
                (Sloc            => Sloc (Node),
                 Condition       => Condition (ISS),
                 Then_Statements => New_List);

            Append (Goto_End, Then_Statements (New_If));

            Insert_Before (Node, New_If);
	    
            Insert_List_Before (Node ,Statements (Node));

            Goto_Begin := Make_Goto_Statement (Sloc (Node), Goto_Begin_Id);
            Insert_Before (Node, Goto_Begin);

            Label_End := Make_Label (Sloc (Node), Goto_End_Id);
            Declare_Label_Current_Scope (This, Label_End);

            Insert_Before (Node, Label_End);

            --  FOR loop case

         else
            Put_Line ("   For case");
            
            --  Expand :

            --  for I in 1..10 loop
            --       STATEMENTS;
            --  end loop;

            --  To

            --  I := 1;
            --  <<Lbegin>>

            --  if I >= 10 then
            --       goto end;
            --  end if;

            --  STATEMENTS;
            --  I := I+1;
            --  goto Lbegin;

            --  <<Lend>>

            declare
               LPS : constant Node_Id :=
                 Loop_Parameter_Specification (ISS);
               DSD : constant Node_Id :=
                 Discrete_Subtype_Definition (LPS);

               Rng        : Node_Id;
               Id         : Entity_Id;
               New_Assign : Node_Id;
               Cond       : Node_Id;
               Incr       : Node_Id;
            begin
               Id := Defining_Identifier (LPS);
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
                  New_Assign := Make_Assignment_Statement
                    (Sloc (Node), 
                     New_Occurrence_Of (Id, Sloc (Id)),
                     New_Copy_Tree (LBD));
               else
                  New_Assign := Make_Assignment_Statement
                    (Sloc (Node), 
                     New_Occurrence_Of (Id, Sloc (Id)),
                     New_Copy_Tree (HBD));
               end if;
		  
               Insert_Before (Node, New_Assign);

               Goto_Begin_Id := Make_Unique_Label_Entity (This, Sloc (Node), String_Find ("Lbegin"));
               Label_Begin := Make_Label (Sloc (Node), New_Occurrence_Of (Goto_Begin_Id, Sloc (Node)));
               Declare_Label_Current_Scope (This, Label_Begin);
	       
               Insert_Before (Node, Label_Begin);
	       
               if For_Loop_Reverse then
                  Cond := Make_Op_Lt 
                    (Sloc (Node), 
                     New_Occurrence_Of (Id, Sloc (Id)),
                     New_Copy_Tree (LBD));
               else
                  Cond := Make_Op_Gt
                    (Sloc (Node), 
                     New_Occurrence_Of (Id, Sloc (Id)),
                     New_Copy_Tree (HBD));
               end if;

               New_If :=
                 Make_If_Statement
                   (Sloc            => Sloc (Node),
                    Condition       => Cond,
                    Then_Statements => New_List);
	       
               Goto_End_Id := Make_Unique_Label_Entity (This, Sloc (Node), String_Find ("Lend"));
               Goto_End := Make_Goto_Statement (Sloc (Node), New_Occurrence_Of (Goto_End_Id, Sloc (Node)));
	       
               Append (Goto_End, Then_Statements (New_If));
               Insert_Before (Node, New_If);

               Stmts := Statements (Node);
	       
               if For_Loop_Reverse then
                  Incr := Make_Op_Subtract
                    (Sloc (Node), Id, Make_Integer_Literal (Sloc (Node), 1));
               else
                  Incr := Make_Op_Add
                    (Sloc (Node), Id, Make_Integer_Literal (Sloc (Node), 1));
               end if;
	       
               New_Assign := Make_Assignment_Statement
                 (Sloc (Node), 
                  New_Occurrence_Of (Id, Sloc (Id)),
                  Incr);
	       
               Append (New_Assign, Stmts);
	       
               Goto_Begin := Make_Goto_Statement (Sloc (Node),New_Occurrence_Of (Goto_Begin_Id, Sloc (Node)));
               Append (Goto_Begin, Stmts);
	       
               Goto_End := Make_Label (Sloc (Node), New_Occurrence_Of (Goto_End_Id, Sloc (Node)));
               Append (Goto_End, Stmts);

               Insert_List_Before (Node, Stmts);
	       
            end;
         end if;
         
         --  No iteration scheme present
         --  Simple loop with exit(s) statement(s)

      else
         Put_Line ("   Loop case");
         
         --  expand :

         --  loop
         --       STATEMENTS
         --  end loop;

         -- To

         --  <<Lbegin>>
         --  STATEMENTS
         --  goto Lbegin;

         --  <<Lend>>
         --  Here, we expect an EXIT_STATEMENT in STATEMENTS of loop to jump
         --  on Lend

         Goto_Begin_Id := Make_Unique_Label_Entity (This, Sloc (Node), String_Find ("Lbegin"));

         Label_Begin := Make_Label (Sloc (Node), Goto_Begin_Id);
         Declare_Label_Current_Scope (This, Label_Begin);

         Insert_Before (Node, Label_Begin);

         Goto_End_Id := Make_Unique_Label_Entity (This, Sloc (Node), String_Find ("Lend"));

         Stmts := Statements (Node);
         Check_Exit (This, Stmts, Goto_End_Id);
         Goto_Begin := Make_Goto_Statement (Sloc (Node), Goto_Begin_Id);
         Append (Goto_Begin, Stmts);
	 
         Insert_List_Before (Node, Stmts);

         Label_End := Make_Label (Sloc (Node), Goto_End_Id);
         Declare_Label_Current_Scope (This, Label_End);
         Insert_Before (Node, Label_End);
      end if;
      
      Remove (Node);
      Put_Line ("Expand_Loop_Statement_For_Ladder End");
      New_Line;
   end Expand_Loop_Statement_For_Ladder;

end Reflex.Boxes.Exp_Ch5;
