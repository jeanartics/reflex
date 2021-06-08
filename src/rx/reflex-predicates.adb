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

with Atree;  use Atree;
with Sinfo;  use Sinfo;
with Nlists; use Nlists;
with Nmake;  use Nmake;
with Sem_Util; use Sem_Util;
with Snames; use Snames;
with Tbuild; use Tbuild;

-- with Reflex.Boxes.Utils; use Reflex.Boxes.Utils;
with Reflex_Options;     use Reflex_Options;
with Reflex.External_Names; use Reflex.External_Names;

with Stand; use Stand;

package body Reflex.Predicates is

   function Max
     (V1 : Natural;
      V2 : Natural) return Natural is
   begin
      if V1 > V2 then
         return V1;
      else
         return V2;
      end if;
   end Max;
   
   ----------------------
   -- Expression_Width --
   ----------------------

   function Expression_Width (Node : Node_Id) return Natural is
      Wl : Natural := 0;
      Wr : Natural := 0;
   begin

      case Nkind (Node) is
         when N_Op_And =>
            Wl := Expression_Width (Left_Opnd (Node));
            Wr := Expression_Width (Right_Opnd (Node));
            return Wl + Wr;

         when N_Op_Or =>
            Wl := Expression_Width (Left_Opnd (Node));
            Wr := Expression_Width (Right_Opnd (Node));
            return Max (Wl, Wr);

         when N_Op_Compare =>
            return 2;

         when N_Identifier =>
            return 1;

         when others =>
            return 0;
      end case;
   end Expression_Width;

   ------------------------------
   -- Expression_Height_Worker --
   ------------------------------

   function Expression_Height_Worker (Node : Node_Id) return Natural is
      Hl : Natural := 0;
      Hr : Natural := 0;
   begin

      case Nkind (Node) is
         when N_Op_And =>
            Hl := Expression_Height_Worker (Left_Opnd (Node));
            Hr := Expression_Height_Worker (Right_Opnd (Node));
            return Max (Hl, Hr);

         when N_Op_Or =>
            Hl := Expression_Height_Worker (Left_Opnd (Node)) + 1;
            Hr := Expression_Height_Worker (Right_Opnd (Node)) + 1;
            return Max (Hl, Hr);

         when others =>
            return 0;
      end case;
   end Expression_Height_Worker;

   -----------------------
   -- Expression_Height --
   -----------------------

   function Expression_Height (Node : Node_Id) return Natural is
      H : Natural := 0;
   begin
      H := Expression_Height_Worker (Node) + 1;
      return H;
   end Expression_Height;

   -----------------
   -- Resize_Expr --
   -----------------

   procedure Resize_Expr (Node : Node_Id) is
      H : Natural;
      W : Natural;

      Nb_Line     : Natural;
      Ob_Per_Line : Natural;
   begin

      H := Expression_Height (Node);
      W := Expression_Width (Node);

      if W > Max_Unity_Ladder_Horizontal then

         -- if we have only N_Op_And :

         Nb_Line     := W mod (Max_Unity_Ladder_Horizontal) + 1;
         Ob_Per_Line := W mod (Nb_Line) + 1;

      end if;

      if H > Max_Unity_Ladder_Vertical then
         null;
      end if;
   end Resize_Expr;
   
--     -----------------------------
--     -- If_Statement_Predicates --
--     -----------------------------
--  
--     procedure If_Statement_Predicates (Node : Node_Id) is
--  
--        Stmts    : List_Id;
--        N_Elsifs : Node_Id;
--     begin
--  
--        --  if part
--  
--        Stmts := Then_Statements (Node);
--        --  Set_Expandable_If (Node, Check_Assign_Etype (Stmts));
--  
--        --  Elsif parts
--  
--        if Present (Elsif_Parts (Node)) then
--           N_Elsifs := First (Elsif_Parts (Node));
--           while Present (N_Elsifs) loop
--              Stmts := Then_Statements (N_Elsifs);
--              --  Set_Expandable_If (N_Elsifs, Check_Assign_Etype (Stmts));
--  
--              Next (N_Elsifs);
--           end loop;
--        end if;
--  
--     end If_Statement_Predicates;

--     ------------------------
--     -- Check_Assign_Etype --
--     ------------------------
--  
--     function Check_Assign_Etype (Stmts : List_Id) return Boolean is
--        Stmt : Node_Id := First (Stmts);
--        Bool : Boolean := False;
--     begin
--        while Present (Stmt) loop
--           if Nkind (Stmt) = N_Assignment_Statement then
--              if Etype (Stmt) /= Standard_Boolean then
--                 Bool := True;
--              end if;
--           else
--              Bool := True;
--           end if;
--  
--           Next (Stmt);
--        end loop;
--  
--        return Bool;
--     end Check_Assign_Etype;
   
   -------------------------------
   -- Has_Only_Simple_Statement --
   -------------------------------
   
   function Has_Only_Simple_Statement (N : Node_Id) return Boolean is
      Elif : Node_Id;
   begin
      case Nkind (N) is
	 when N_If_Statement =>
	    
	    if not Is_Simple_Statements_List (Then_Statements (N)) then
	       return False;
	    end if;
      
	    --  Elsif Parts
	    
	    if Present (Elsif_Parts (N)) then
	       
	       Elif := First (Elsif_Parts (N));
	       while Present (Elif) loop
		  
		  if not Is_Simple_Statements_List (Then_Statements (Elif)) 
		  then
		     return False;
		  end if;
	    
		  Next (Elif);
	       end loop;
	    end if;
      
	    return True;
	    
	 when others =>
	    return False;
      end case;
   end Has_Only_Simple_Statement;
     
   -------------------------------
   -- Is_Simple_Statements_List --
   -------------------------------

   function Is_Simple_Statements_List (Stmts : List_Id) return Boolean is
      Stmt : Node_Id;
   begin
      if not Is_Empty_List (Stmts) then
	 
	 Stmt := First (Stmts);
	 while Present (Stmt) loop
	    if not Is_Simple_Statement (Stmt) then
	       return False;
	    end if;
	    
	    Next (Stmt);
	 end loop;
      end if;
      
      return True;
   end Is_Simple_Statements_List;

   -------------------------
   -- Is_Simple_Statement --
   -------------------------
   
   function Is_Simple_Statement (Stmt : Node_Id) return Boolean is
   begin
      return Nkind_In
	      (Stmt, 
	       N_Null_Statement,
	       N_Assignment_Statement, 
	       N_Goto_Statement, 
	       N_Procedure_Call_Statement);
   end Is_Simple_Statement;
   
   -----------------------------
   -- Is_Null_Statements_List --
   -----------------------------

   function Is_Null_Statements_List (Stmts : List_Id) return Boolean is
      Stmt : Node_Id;
   begin
      if Is_Empty_List (Stmts) then
	 return True;
      end if;
      
      Stmt := First (Stmts);
      while Present (Stmt) loop
	 if Nkind (Stmt) /= N_Null_Statement then
	    return False;
	 end if;
	 Next (Stmt);
      end loop;
      
      return True;
   end Is_Null_Statements_List;
   
   --------------------------------
   -- Must_Statements_List_Break --
   --------------------------------
   
   function Must_Statements_List_Break (Stmts : List_Id) return Boolean is
      
      Count : Natural;
      Stmt  : Node_Id;
   begin
      if not Is_Empty_List (Stmts) then
	 Count := 0;
	 Stmt := First (Stmts);
	 while Present (Stmt) loop
	    Count := Count + 1;
	    if Count > Max_Unity_Ladder_Vertical 
	      or else Nkind (Stmt) = N_Procedure_Call_Statement 
	    then
	       return True;
	    end if;
	    
	    Next (Stmt);
	 end loop;
      end if;
      
      return False;
   end Must_Statements_List_Break;
   
   -------------------
   -- Must_Break_If --
   -------------------
   
   function Must_Break_If (N : Node_Id) return Boolean is
      Elif : Node_Id;
   begin
      --  Then parts
      
      if Must_Statements_List_Break (Then_Statements (N)) then
	 return True;
      end if;
      
      --  Elsif Parts
      
      if Present (Elsif_Parts (N)) then
	 
	 Elif := First (Elsif_Parts (N));
	 while Present (Elif) loop
	    
	    if Must_Statements_List_Break (Then_Statements (Elif)) then
	       return True;
	    end if;
	    
	    Next (Elif);
	 end loop;
      end if;
      
      --  Else Parts
      
      if Present (Else_Statements (N)) then
	 if Must_Statements_List_Break (Else_Statements (N)) then
	    return True;
	 end if;
      end if;
      
      return False;
   end Must_Break_If;
     
   -------------------------
   -- Designate_Aggregate --
   -------------------------
   
   function Directly_Designate_Aggregate (N : Node_Id) return Node_Id is
   begin
      if Nkind (N) = N_Aggregate
	or else Nkind (N) = N_Extension_Aggregate
      then 
	 return N;
      end if;
      
      if Nkind (N) = N_Qualified_Expression then
	 if Nkind (Expression (N)) = N_Aggregate
	   or else Nkind (Expression (N)) = N_Extension_Aggregate 
	 then	   
	    return Expression (N);
	 end if;
      end if;
      
      return Empty;
      
   exception
      when others =>
         Put_Line ("Exception ================> Directly_Designate_Aggregate");
         return Empty;
   end Directly_Designate_Aggregate;
   
   -------------------------
   -- Designate_Aggregate --
   -------------------------
   
   function Designate_Aggregate (N : Node_Id) return Node_Id is
      Des : Node_Id;
      E   : Entity_Id;
      Par : Node_Id;
   begin
      Des := Directly_Designate_Aggregate (N);
      if Present (Des) then
	 return Des;
      end if;
      
      if Nkind (N) = N_Identifier
	or else	Nkind (N) = N_Expanded_Name
      then
	 E := Entity (N);
	 Par := Parent (E);
	 
	 if Nkind (Par) = N_Object_Renaming_Declaration then
	    return Designate_Aggregate (Name (Par));
	    
	 elsif Nkind (Par) = N_Object_Declaration then
	    return Designate_Aggregate (Expression (Par));
	    
	 elsif Nkind (Par) = N_Component_Declaration then
	    return Designate_Aggregate (Expression (Par));
	    
	 elsif Nkind (Par) = N_Parameter_Specification then
	    return Designate_Aggregate (Expression (Par));
	 end if;
      end if;
      
      return Empty;
      
   exception
      when others =>
         Put_Line ("Exception ================> Designate_Aggregate");
         return Empty;
   end Designate_Aggregate;
   
   ---------------------
   -- Has_Side_Effect --
   ---------------------
   
   function Expression_Has_Side_Effect (N : Node_Id) return Boolean is
      Found : Boolean;
      
      function Process (N : Node_Id) return Traverse_Result;
      
      -------------
      -- Process --
      -------------
      
      function Process (N : Node_Id) return Traverse_Result is
      begin 
	 if Nkind (N) = N_Function_Call then
	    declare
	       Call_Entity : Entity_Id;
	    begin
	       Call_Entity := Entity (Name (N));
	       if not Get_Internal_Function (Call_Entity) then
		  Found := True;
		  return Abandon;
	       end if;
	    end;
	 end if;
	 return Ok;
     end Process;
	 
     function Traverse is new Traverse_Func (Process => Process);
     
      Dummy : Traverse_Final_Result;
   begin
      Found := False;
      Dummy := Traverse (N);
      
      return Found;
   end Expression_Has_Side_Effect;
   
   ------------------------
   -- Remove_Side_Effect --
   ------------------------
   
   procedure Remove_Side_Effect (N : Node_Id) is
      Found : Boolean;
      
      function Process (N : Node_Id) return Traverse_Result;
      
      -------------
      -- Process --
      -------------
      
      function Process (N : Node_Id) return Traverse_Result is
      begin 
	 if Nkind (N) = N_Function_Call then
	    null;
	    return Abandon;
	 end if;
	 return Ok;
     end Process;
	 
     function Traverse is new Traverse_Func (Process => Process);
     
      Dummy : Traverse_Final_Result;
   begin
      Found := False;
      Dummy := Traverse (N);
      
   end Remove_Side_Effect;
   
   ---------------------
   -- Has_Side_Effect --
   ---------------------
   
   function Has_Call (N : Node_Id) return Boolean is
      
      Found : Boolean;
      
      function Process (N : Node_Id) return Traverse_Result;
      
      -------------
      -- Process --
      -------------
      
      function Process (N : Node_Id) return Traverse_Result is
      begin 
	 if Nkind (N) = N_Function_Call then
	    Found := True;
	    return Abandon;
	 end if;
	 return Ok;
     end Process;
	 
     function Traverse is new Traverse_Func (Process => Process);
     
      Dummy : Traverse_Final_Result;
   begin
      Found := False;
      Dummy := Traverse (N);
      
      return Found;
   end Has_Call;
   
   ---------------------------------
   -- Remove_Call_From_Expression --
   ---------------------------------
   
   procedure Remove_Call_From_Expression (N : Node_Id) is
   begin
      
      null;
   end Remove_Call_From_Expression;
   
   ------------------------------
   -- Insert_Expr_With_Actions --
   ------------------------------
   
   procedure Insert_Expr_With_Actions is
   begin
      null;
   end Insert_Expr_With_Actions;
   
   ------------------------------
   -- Insert_Statement_Actions --
   ------------------------------

   procedure Insert_Statement_Actions
     (Assoc_Node  : Node_Id; 
      Ins_Actions : List_Id) is
      
      N : Node_Id;
      P : Node_Id;

   begin
      if No (Ins_Actions) or else Is_Empty_List (Ins_Actions) then
         return;
      end if;

      --  We intend to climb up the tree to find the right point to 
      --  insert the actions. We start at Assoc_Node, unless this node is
      --  a subexpression in which case we start with its parent. We do this
      --  for two reasons. First it speeds things up. Second, if Assoc_Node
      --  is itself one of the special nodes like N_And_Then, then we assume
      --  that an initial request to insert actions for such a node does not
      --  expect the actions to get deposited in the node for later handling
      --  when the node is expanded, since clearly the node is being dealt
      --  with by the caller. Note that in the subexpression case, N is
      --  always the child we came from.

      --  N_Raise_xxx_Error is an annoying special case, it is a statement
      --  if it has type Standard_Void_Type, and a subexpression otherwise.
      --  otherwise. Procedure attribute references are also statements.

      if Nkind (Assoc_Node) in N_Subexpr
        and then (Nkind (Assoc_Node) in N_Raise_xxx_Error
                   or else Etype (Assoc_Node) /= Standard_Void_Type)
        and then (Nkind (Assoc_Node) /= N_Attribute_Reference
                   or else
                     not Is_Procedure_Attribute_Name
                           (Attribute_Name (Assoc_Node)))
      then
         P := Assoc_Node;             -- ??? does not agree with above!
         N := Parent (Assoc_Node);

      --  Non-subexpression case. Note that N is initially Empty in this
      --  case (N is only guaranteed Non-Empty in the subexpr case).

      else
         P := Assoc_Node;
         N := Empty;
      end if;

      loop
         pragma Assert (Present (P));

         case Nkind (P) is

            --  Case of right operand of AND THEN or OR ELSE. Put the actions
            --  in the Actions field of the right operand. They will be moved
            --  out further when the AND THEN or OR ELSE operator is expanded.
            --  Nothing special needs to be done for the left operand since
            --  in that case the actions are executed unconditionally.

            when N_And_Then | N_Or_Else =>
               --  if N = Right_Opnd (P) then
               --     if Present (Actions (P)) then
               --        Insert_List_After
               --         (Last (Actions (P)), Ins_Actions);
               --     else
               --        Set_Actions (P, Ins_Actions);
               --        --  Analyze_List (Actions (P));
               --     end if;

               --     return;
               --  end if;
	       null;
	       
            --  Then or Else operand of conditional expression. Add actions to
            --  Then_Actions or Else_Actions field as appropriate. The actions
            --  will be moved further out when the conditional is expanded.

            when N_Conditional_Expression =>
               declare
                  ThenX : constant Node_Id := Next (First (Expressions (P)));
                  ElseX : constant Node_Id := Next (ThenX);

               begin
                  --  Actions belong to the then expression, temporarily
                  --  place them as Then_Actions of the conditional expr.
                  --  They will be moved to the proper place later when
                  --  the conditional expression is expanded.

                  if N = ThenX then
                     if Present (Then_Actions (P)) then
                        Insert_List_After
                          (Last (Then_Actions (P)), Ins_Actions);
                     else
                        Set_Then_Actions (P, Ins_Actions);
                        --  Analyze_List (Then_Actions (P));
                     end if;

                     return;

                  --  Actions belong to the else expression, temporarily
                  --  place them as Else_Actions of the conditional expr.
                  --  They will be moved to the proper place later when
                  --  the conditional expression is expanded.

                  elsif N = ElseX then
                     if Present (Else_Actions (P)) then
                        Insert_List_After
                          (Last (Else_Actions (P)), Ins_Actions);
                     else
                        Set_Else_Actions (P, Ins_Actions);
                        --  Analyze_List (Else_Actions (P));
                     end if;

                     return;

                  --  Actions belong to the condition. In this case they are
                  --  unconditionally executed, and so we can continue the
                  --  search for the proper insert point.

                  else
                     null;
                  end if;
               end;

            --  Case of appearing in the condition of a while expression or
            --  elsif. We insert the actions into the Condition_Actions field.
            --  They will be moved further out when the while loop or elsif
            --  is analyzed.

            when N_Iteration_Scheme |
                 N_Elsif_Part
            =>
               --  if N = Condition (P) then
               --     if Present (Condition_Actions (P)) then
               --        Insert_List_After
               --          (Last (Condition_Actions (P)), Ins_Actions);
               --     else
               --        Set_Condition_Actions (P, Ins_Actions);

               --        --  Set the parent of the insert actions explicitly.
               --        --  This is not a syntactic field, but we need the
               --        --  parent field set, in particular so that freeze
               --        --  can understand that it is dealing with condition
               --        --  actions, and properly insert the freezing actions.

               --        Set_Parent (Ins_Actions, P);
               --        -- Analyze_List (Condition_Actions (P));
               --     end if;

               --     return;
               --  end if;
null;

	       --  Statements, declarations, pragmas, representation clauses.

            when
               --  Statements

               N_Procedure_Call_Statement               |
               N_Statement_Other_Than_Procedure_Call    |

               --  Pragmas

               N_Pragma                                 |

               --  Representation_Clause

               N_At_Clause                              |
               N_Attribute_Definition_Clause            |
               N_Enumeration_Representation_Clause      |
               N_Record_Representation_Clause           |

               --  Declarations


               --  Freeze entity behaves like a declaration or statement

               N_Freeze_Entity
            =>
               --  Do not insert here if the item is not a list member (this
               --  happens for example with a triggering statement, and the
               --  proper approach is to insert before the entire select).

               if not Is_List_Member (P) then
                  null;

               --  Do not insert if parent of P is an N_Component_Association
               --  node (i.e. we are in the context of an N_Aggregate node.
               --  In this case we want to insert before the entire aggregate.

               --  elsif Nkind (Parent (P)) = N_Component_Association then
               --     null;

               --  Do not insert if the parent of P is either an N_Variant
               --  node or an N_Record_Definition node, meaning in either
               --  case that P is a member of a component list, and that
               --  therefore the actions should be inserted outside the
               --  complete record declaration.

               --  elsif Nkind (Parent (P)) = N_Record_Definition
               --  then
               --     null;

               --  Do not insert freeze nodes within the loop generated for
               --  an aggregate, because they may be elaborated too late for
               --  subsequent use in the back end: within a package spec the
               --  loop is part of the elaboration procedure and is only
               --  elaborated during the second pass.
               --  If the loop comes from source, or the entity is local to
               --  the loop itself it must remain within.

               --  elsif Nkind (Parent (P)) = N_Loop_Statement
               --    and then not Comes_From_Source (Parent (P))
               --    and then Nkind (First (Ins_Actions)) = N_Freeze_Entity
               --    and then
               --      Scope (Entity (First (Ins_Actions))) /= Current_Scope
               --  then
               --     null;

               else
                  Insert_List_Before (P, Ins_Actions);
                  return;
               end if;

            --  A special case, N_Raise_xxx_Error can act either as a
            --  statement or a subexpression. We tell the difference
            --  by looking at the Etype. It is set to Standard_Void_Type
            --  in the statement case.

            when
               N_Raise_xxx_Error =>
                  if Etype (P) = Standard_Void_Type then
                        Insert_List_Before (P, Ins_Actions);

                     return;

                  --  In the subexpression case, keep climbing

                  else
                     null;
                  end if;

            --  If a component association appears within a loop created for
            --  an array aggregate, attach the actions to the association so
            --  they can be subsequently inserted within the loop. For other
            --  component associations insert outside of the aggregate. For
            --  an association that will generate a loop, its Loop_Actions
            --  attribute is already initialized (see exp_aggr.adb).

            --  The list of loop_actions can in turn generate additional ones,
            --  that are inserted before the associated node. If the associated
            --  node is outside the aggregate, the new actions are collected
            --  at the end of the loop actions, to respect the order in which
            --  they are to be elaborated.

            when
               N_Component_Association =>
                  --  if Nkind (Parent (P)) = N_Aggregate
                  --    and then Present (Loop_Actions (P))
                  --  then
                  --     if Is_Empty_List (Loop_Actions (P)) then
                  --        Set_Loop_Actions (P, Ins_Actions);
                  --        --  Analyze_List (Ins_Actions);

                  --     else
                  --        declare
                  --           Decl : Node_Id := Assoc_Node;

                  --        begin
                  --           --  Check whether these actions were generated
                  --           --  by a declaration that is part of the loop_
                  --           --  actions for the component_association.

                  --           while Present (Decl) loop
                  --              exit when Parent (Decl) = P
                  --                and then Is_List_Member (Decl)
                  --                and then
                  --                  List_Containing (Decl) = Loop_Actions (P);
                  --              Decl := Parent (Decl);
                  --           end loop;

                  --           if Present (Decl) then
                  --              Insert_List_Before
                  --                (Decl, Ins_Actions);
                  --           else
                  --              Insert_List_After
                  --                (Last (Loop_Actions (P)), Ins_Actions);
                  --           end if;
                  --        end;
                  --     end if;

                  --     return;

                  --  else
                     null;
                  --  end if;

            --  Another special case, an attribute denoting a procedure call

            when
               N_Attribute_Reference =>
                  --  if Is_Procedure_Attribute_Name (Attribute_Name (P)) then
                  --        Insert_List_Before (P, Ins_Actions);

                  --     return;

                  --  --  In the subexpression case, keep climbing

                  --  else
                     null;
                  --  end if;

            --  For all other node types, keep climbing tree

            when
	      N_Abstract_Subprogram_Declaration        |
	      N_Exception_Declaration                  |
	      N_Exception_Renaming_Declaration         |
	      N_Formal_Object_Declaration              |
	      N_Formal_Subprogram_Declaration          |
	      N_Formal_Type_Declaration                |
	      N_Full_Type_Declaration                  |
	      N_Function_Instantiation                 |
	      N_Generic_Function_Renaming_Declaration  |
	      N_Generic_Package_Declaration            |
	      N_Generic_Package_Renaming_Declaration   |
	      N_Generic_Procedure_Renaming_Declaration |
	      N_Generic_Subprogram_Declaration         |
	      N_Implicit_Label_Declaration             |
	      N_Incomplete_Type_Declaration            |
	      N_Number_Declaration                     |
	      N_Object_Declaration                     |
	      N_Object_Renaming_Declaration            |
	      N_Package_Body                           |
	      N_Package_Declaration                    |
	      N_Package_Instantiation                  |
	      N_Package_Renaming_Declaration           |
	      N_Private_Extension_Declaration          |
	      N_Private_Type_Declaration               |
	      N_Procedure_Instantiation                |
	      N_Subprogram_Body                        |
	      N_Subprogram_Declaration                 |
	      N_Subprogram_Renaming_Declaration        |
	      N_Subtype_Declaration                    |
	      N_Access_Definition                      |
	      N_Access_Function_Definition             |
	      N_Access_Procedure_Definition            |
	      N_Access_To_Object_Definition            |
	      N_Aggregate                              |
	      N_Allocator                              |
	      N_Case_Statement_Alternative             |
	      N_Character_Literal                      |
	      N_Compilation_Unit                       |
	      N_Compilation_Unit_Aux                   |
	      N_Component_Clause                       |
	      N_Component_Declaration                  |
	      N_Component_Definition                   |
	      N_Component_List                         |
	      N_Constrained_Array_Definition           |
	      N_Defining_Character_Literal             |
	      N_Defining_Identifier                    |
	      N_Defining_Operator_Symbol               |
	      N_Defining_Program_Unit_Name             |
	      N_Derived_Type_Definition                |
	      N_Designator                             |
	      N_Digits_Constraint                      |
	      N_Discriminant_Association               |
	      N_Empty                                  |
	      N_Enumeration_Type_Definition            |
	      N_Error                                  |
	      N_Exception_Handler                      |
	      N_Expanded_Name                          |
	      N_Explicit_Dereference                   |
	      N_Extension_Aggregate                    |
	      N_Floating_Point_Definition              |
	      N_Formal_Derived_Type_Definition         |
	      N_Formal_Discrete_Type_Definition        |
	      N_Formal_Floating_Point_Definition       |
	      N_Formal_Modular_Type_Definition         |
	      N_Formal_Package_Declaration             |
	      N_Formal_Private_Type_Definition         |
	      N_Formal_Signed_Integer_Type_Definition  |
	      N_Function_Call                          |
	      N_Function_Specification                 |
	      N_Generic_Association                    |
	      N_Handled_Sequence_Of_Statements         |
	      N_Identifier                             |
	      N_In                                     |
	      N_Index_Or_Discriminant_Constraint       |
	      N_Indexed_Component                      |
	      N_Integer_Literal                        |
	      N_Label                                  |
	      N_Loop_Parameter_Specification           |
	      N_Mod_Clause                             |
	      N_Modular_Type_Definition                |
	      N_Not_In                                 |
	      N_Null                                   |
	      N_Op_Abs                                 |
	      N_Op_Add                                 |
	      N_Op_And                                 |
	      N_Op_Concat                              |
	      N_Op_Divide                              |
	      N_Op_Eq                                  |
	      N_Op_Expon                               |
	      N_Op_Ge                                  |
	      N_Op_Gt                                  |
	      N_Op_Le                                  |
	      N_Op_Lt                                  |
	      N_Op_Minus                               |
	      N_Op_Mod                                 |
	      N_Op_Multiply                            |
	      N_Op_Ne                                  |
	      N_Op_Not                                 |
	      N_Op_Or                                  |
	      N_Op_Plus                                |
	      N_Op_Rem                                 |
	      N_Op_Rotate_Left                         |
	      N_Op_Rotate_Right                        |
	      N_Op_Shift_Left                          |
	      N_Op_Shift_Right                         |
	      N_Op_Shift_Right_Arithmetic              |
	      N_Op_Subtract                            |
	      N_Op_Xor                                 |
	      N_Operator_Symbol                        |
	      N_Others_Choice                          |
	      N_Package_Specification                  |
	      N_Parameter_Association                  |
	      N_Parameter_Specification                |
	      N_Pragma_Argument_Association            |
	      N_Procedure_Specification                |
	      N_Qualified_Expression                   |
	      N_Range                                  |
	      N_Range_Constraint                       |
	      N_Real_Literal                           |
	      N_Real_Range_Specification               |
	      N_Record_Definition                      |
	      N_Reference                              |
	      N_Selected_Component                     |
	      N_Signed_Integer_Type_Definition         |
	      N_Slice                                  |
	      N_String_Literal                         |
	      N_Subprogram_Info                        |
	      N_Subtype_Indication                     |
	      N_Type_Conversion                        |
	      N_Unchecked_Expression                   |
	      N_Unchecked_Type_Conversion              |
	      N_Unconstrained_Array_Definition         |
	      N_Unused_At_End                          |
	      N_Unused_At_Start                        |
	      N_Use_Package_Clause                     |
	      N_Use_Type_Clause                        |
	      N_Validate_Unchecked_Conversion          |
	      N_With_Clause                            |
	      N_With_Type_Clause
            =>
               null;

         end case;

         --  Make sure that inserted actions stay in the transient scope

         --  If we fall through above tests, keep climbing tree

         N := P;

	 P := Parent (N);
      end loop;

   end Insert_Statement_Actions;

   
end Reflex.Predicates;
