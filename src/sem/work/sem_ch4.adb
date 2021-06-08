------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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
-- Reflex is a fork from the GNAT compiler. GNAT was originally developed   --
-- by the GNAT team at  New York University. Extensive  contributions to    --
-- GNAT were provided by Ada Core Technologies Inc. Reflex is developed  by --
-- the Artics team at Grenoble.                                             --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Util; use Exp_Util;
with Hostparm; use Hostparm;
with Itypes;   use Itypes;
with Lib.Xref; use Lib.Xref;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Elists;   use Elists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Restrict; use Restrict;
with Sem;      use Sem;
with Sem_Cat;  use Sem_Cat;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch8;  use Sem_Ch8;
-- with Sem_Dist; use Sem_Dist;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sem_Type; use Sem_Type;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Tbuild;   use Tbuild;

with GNAT.Spelling_Checker; use GNAT.Spelling_Checker;

package body Sem_Ch4 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Analyze_Expression (N : Node_Id);
   --  For expressions that are not names, this is just a call to analyze.
   --  If the expression is a name, it may be a call to a parameterless
   --  function, and if so must be converted into an explicit call node
   --  and analyzed as such. This deproceduring must be done during the first
   --  pass of overload resolution, because otherwise a procedure call with
   --  overloaded actuals may fail to resolve. See 4327-001 for an example.

   procedure Analyze_Overloaded_Selected_Component (N : Node_Id);
   --  If the prefix of a selected_component is overloaded, the proper
   --  interpretation that yields a record type with the proper selector
   --  name must be selected.

   procedure Ambiguous_Operands (N : Node_Id);
   --  for equality, membership, and comparison operators with overloaded
   --  arguments, list possible interpretations.

   procedure Analyze_One_Call
      (N          : Node_Id;
       Nam        : Entity_Id;
       Report     : Boolean;
       Success    : out Boolean;
       Skip_First : Boolean := False);
   --  Check one interpretation of an overloaded subprogram name for
   --  compatibility with the types of the actuals in a call. If there is a
   --  single interpretation which does not match, post error if Report is
   --  set to True.
   --
   --  Nam is the entity that provides the formals against which the actuals
   --  are checked. Nam is either the name of a subprogram, or the internal
   --  subprogram type constructed for an access_to_subprogram. If the actuals
   --  are compatible with Nam, then Nam is added to the list of candidate
   --  interpretations for N, and Success is set to True.

   procedure Check_Misspelled_Selector
     (Prefix : Entity_Id;
      Sel    : Node_Id);
   --  Give possible misspelling diagnostic if Sel is likely to be
   --  a misspelling of one of the selectors of the Prefix.
   --  This is called by Analyze_Selected_Component after producing
   --  an invalid selector error message.

   function Defined_In_Scope (T : Entity_Id; S : Entity_Id) return Boolean;
   --  Verify that type T is declared in scope S. Used to find intepretations
   --  for operators given by expanded names. This is abstracted as a separate
   --  function to handle extensions to System, where S is System, but T is
   --  declared in the extension.

   procedure Find_Arithmetic_Types
     (L, R  : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id);
   --  L and R are the operands of an arithmetic operator. Find
   --  consistent pairs of interpretations for L and R that have a
   --  numeric type consistent with the semantics of the operator.

   procedure Find_Comparison_Types
     (L, R  : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id);
   --  L and R are operands of a comparison operator. Find consistent
   --  pairs of interpretations for L and R.

   procedure Find_Concatenation_Types
     (L, R  : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id);
   --  For the four varieties of concatenation.

   procedure Find_Equality_Types
     (L, R  : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id);
   --  Ditto for equality operators.

   procedure Find_Boolean_Types
     (L, R  : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id);
   --  Ditto for binary logical operations.

   procedure Find_Negation_Types
     (R     : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id);
   --  Find consistent interpretation for operand of negation operator.

   procedure Find_Non_Universal_Interpretations
     (N     : Node_Id;
      R     : Node_Id;
      Op_Id : Entity_Id;
      T1    : Entity_Id);
   --  For equality and comparison operators, the result is always boolean,
   --  and the legality of the operation is determined from the visibility
   --  of the operand types. If one of the operands has a universal interpre-
   --  tation,  the legality check uses some compatible non-universal
   --  interpretation of the other operand. N can be an operator node, or
   --  a function call whose name is an operator designator.

   function Find_Primitive_Operation (N : Node_Id) return Boolean;
   --  Find candidate interpretations for the name Obj.Proc when it appears
   --  in a subprogram renaming declaration.

   procedure Find_Unary_Types
     (R     : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id);
   --  Unary arithmetic types: plus, minus, abs.

   procedure Check_Arithmetic_Pair
     (T1, T2 : Entity_Id;
      Op_Id  : Entity_Id;
      N      : Node_Id);
   --  Subsidiary procedure to Find_Arithmetic_Types. T1 and T2 are valid
   --  types for left and right operand. Determine whether they constitute
   --  a valid pair for the given operator, and record the corresponding
   --  interpretation of the operator node. The node N may be an operator
   --  node (the usual case) or a function call whose prefix is an operator
   --  designator. In  both cases Op_Id is the operator name itself.

   procedure Diagnose_Call (N : Node_Id; Nam : Node_Id);
   --  Give detailed information on overloaded call where none of the
   --  interpretations match. N is the call node, Nam the designator for
   --  the overloaded entity being called.

   function Junk_Operand (N : Node_Id) return Boolean;
   --  Test for an operand that is an inappropriate entity (e.g. a package
   --  name or a label). If so, issue an error message and return True. If
   --  the operand is not an inappropriate entity kind, return False.

   procedure Operator_Check (N : Node_Id);
   --  Verify that an operator has received some valid interpretation.
   --  If none was found, determine whether a use clause would make the
   --  operation legal. The variable Candidate_Type (defined in Sem_Type) is
   --  set for every type compatible with the operator, even if the operator
   --  for the type is not directly visible. The routine uses this type to emit
   --  a more informative message.

   function Try_Indexed_Call
     (N          : Node_Id;
      Nam        : Entity_Id;
      Typ        : Entity_Id;
      Skip_First : Boolean) return Boolean;
   --  If a function has defaults for all its actuals, a call to it may
   --  in fact be an indexing on the result of the call. Try_Indexed_Call
   --  attempts the interpretation as an indexing, prior to analysis as
   --  a call. If both are possible,  the node is overloaded with both
   --  interpretations (same symbol but two different types). If the call is
   --  written in prefix form, the prefix becomes the first parameter in the
   --  call, and only the remaining actuals must be checked for the presence of
   --  defaults.

   function Try_Indirect_Call
     (N   : Node_Id;
      Nam : Entity_Id;
      Typ : Entity_Id) return Boolean;
   --  Similarly, a function F that needs no actuals can return an access
   --  to a subprogram, and the call F (X)  interpreted as F.all (X). In
   --  this case the call may be overloaded with both interpretations.
   
   function Try_Object_Operation
     (N            : Node_Id;
      CW_Test_Only : Boolean := False) return Boolean;
   --  Ada 2005 (AI-252): Support the object.operation notation. If node N
   --  is a call in this notation, it is transformed into a normal subprogram
   --  call where the prefix is a parameter, and True is returned. If node
   --  N is not of this form, it is unchanged, and False is returned. If
   --  CW_Test_Only is true then N is an N_Selected_Component node which
   --  is part of a call to an entry or procedure of a tagged concurrent
   --  type and this routine is invoked to search for class-wide subprograms
   --  conflicting with the target entity.

   ------------------------
   -- Ambiguous_Operands --
   ------------------------

   procedure Ambiguous_Operands (N : Node_Id) is
      procedure List_Operand_Interps (Opnd : Node_Id);

      procedure List_Operand_Interps (Opnd : Node_Id) is
         Nam   : Node_Id;
         Err   : Node_Id := N;

      begin
         if Is_Overloaded (Opnd) then
            if Nkind (Opnd) in N_Op then
               Nam := Opnd;

            elsif Nkind (Opnd) = N_Function_Call then
               Nam := Name (Opnd);

            else
               return;
            end if;

         else
            return;
         end if;

         if Opnd = Left_Opnd (N) then
            Error_Msg_N
              ("\left operand has the following interpretations", N);
         else
            Error_Msg_N
              ("\right operand has the following interpretations", N);
            Err := Opnd;
         end if;

         List_Interps (Nam, Err);
      end List_Operand_Interps;

   begin
      if Nkind (N) = N_In
        or else Nkind (N) = N_Not_In
      then
         Error_Msg_N ("ambiguous operands for membership",  N);

      elsif Nkind (N) = N_Op_Eq
        or else Nkind (N) = N_Op_Ne
      then
         Error_Msg_N ("ambiguous operands for equality",  N);

      else
         Error_Msg_N ("ambiguous operands for comparison",  N);
      end if;

      if All_Errors_Mode then
         List_Operand_Interps (Left_Opnd  (N));
         List_Operand_Interps (Right_Opnd (N));
      else
	 Error_Msg_N ("\use -gnatf for details", N);
      end if;
   end Ambiguous_Operands;

   -----------------------
   -- Analyze_Aggregate --
   -----------------------

   --  Most of the analysis of Aggregates requires that the type be known,
   --  and is therefore put off until resolution.

   procedure Analyze_Aggregate (N : Node_Id) is
   begin
      if No (Etype (N)) then
         Set_Etype (N, Any_Composite);
      end if;
   end Analyze_Aggregate;

   ---------------------------
   -- Analyze_Arithmetic_Op --
   ---------------------------

   procedure Analyze_Arithmetic_Op (N : Node_Id) is
      L     : constant Node_Id := Left_Opnd (N);
      R     : constant Node_Id := Right_Opnd (N);
      Op_Id : Entity_Id;

   begin
      Candidate_Type := Empty;
      Analyze_Expression (L);
      Analyze_Expression (R);

      --  If the entity is already set, the node is the instantiation of
      --  a generic node with a non-local reference, or was manufactured
      --  by a call to Make_Op_xxx. In either case the entity is known to
      --  be valid, and we do not need to collect interpretations, instead
      --  we just get the single possible interpretation.

      Op_Id := Entity (N);

      if Present (Op_Id) then
         if Ekind (Op_Id) = E_Operator then
	    Set_Etype (N, Any_Type);
	    Find_Arithmetic_Types (L, R, Op_Id, N);

         else
            Set_Etype (N, Any_Type);
            Add_One_Interp (N, Op_Id, Etype (Op_Id));
         end if;

      --  Entity is not already set, so we do need to collect interpretations

      else
         Op_Id := Get_Name_Entity_Id (Chars (N));
         Set_Etype (N, Any_Type);

         while Present (Op_Id) loop
            if Ekind (Op_Id) = E_Operator
              and then Present (Next_Entity (First_Entity (Op_Id)))
            then
               Find_Arithmetic_Types (L, R, Op_Id, N);
            end if;

            Op_Id := Homonym (Op_Id);
         end loop;
      end if;

      Operator_Check (N);
   end Analyze_Arithmetic_Op;

   ------------------
   -- Analyze_Call --
   ------------------

   --  Function, procedure, and entry calls are checked here. The Name
   --  in the call may be overloaded. The actuals have been analyzed
   --  and may themselves be overloaded. On exit from this procedure, the node
   --  N may have zero, one or more interpretations. In the first case an error
   --  message is produced. In the last case, the node is flagged as overloaded
   --  and the interpretations are collected in All_Interp.

   --  If the name is an Access_To_Subprogram, it cannot be overloaded, but
   --  the type-checking is similar to that of other calls.

   procedure Analyze_Call (N : Node_Id) is
      Actuals : constant List_Id := Parameter_Associations (N);
      Nam     : Node_Id          := Name (N);
      X       : Interp_Index;
      It      : Interp;
      Nam_Ent : Entity_Id;
      Success : Boolean := False;

      function Name_Denotes_Function return Boolean;
      --  If the type of the name is an access to subprogram, this may be
      --  the type of a name, or the return type of the function being called.
      --  If the name is not an entity then it can denote a protected function.
      --  Until we distinguish Etype from Return_Type, we must use this
      --  routine to resolve the meaning of the name in the call.

      ---------------------------
      -- Name_Denotes_Function --
      ---------------------------

      function Name_Denotes_Function return Boolean is
      begin
         if Is_Entity_Name (Nam) then
            return Ekind (Entity (Nam)) = E_Function;

         elsif Nkind (Nam) = N_Selected_Component then
            return Ekind (Entity (Selector_Name (Nam))) = E_Function;

         else
            return False;
         end if;
      end Name_Denotes_Function;

   --  Start of processing for Analyze_Call

   begin
      --  Initialize the type of the result of the call to the error type,
      --  which will be reset if the type is successfully resolved.

      Set_Etype (N, Any_Type);

      if not Is_Overloaded (Nam) then

         --  Only one interpretation to check

         if Ekind (Etype (Nam)) = E_Subprogram_Type then
            Nam_Ent := Etype (Nam);

         elsif Is_Access_Type (Etype (Nam))
           and then Ekind (Designated_Type (Etype (Nam))) = E_Subprogram_Type
           and then not Name_Denotes_Function
         then
            Nam_Ent := Designated_Type (Etype (Nam));
            Insert_Explicit_Dereference (Nam);

         elsif not Is_Entity_Name (Nam) then
            Error_Msg_N ("name in call is not a callable entity", Nam);
            Set_Etype (N, Any_Type);
            return;

         else
            Nam_Ent := Entity (Nam);

            --  If no interpretations, give error message

            if not Is_Overloadable (Nam_Ent) then
               declare
                  L : constant Boolean   := Is_List_Member (N);
                  K : constant Node_Kind := Nkind (Parent (N));

               begin
                  --  If the node is in a list whose parent is not an
                  --  expression then it must be an attempted procedure call.

                  if L and then K not in N_Subexpr then
                     if Ekind (Entity (Nam)) = E_Generic_Procedure then
                        Error_Msg_NE
                          ("must instantiate generic procedure& before call",
                           Nam, Entity (Nam));
                     else
                        Error_Msg_N
                          ("procedure name expected", Nam);
                     end if;

                  --  Check for tasking cases where only an entry call will do

                  --  Otherwise give general error message

                  else
                     Error_Msg_N ("invalid prefix in call", Nam);
                  end if;

                  return;
               end;
            end if;
         end if;

         Analyze_One_Call (N, Nam_Ent, True, Success);

      else
         --  An overloaded selected component must denote overloaded
         --  operations of a concurrent type. The interpretations are
         --  attached to the simple name of those operations.

         if Nkind (Nam) = N_Selected_Component then
            Nam := Selector_Name (Nam);
         end if;

         Get_First_Interp (Nam, X, It);

         while Present (It.Nam) loop
            Nam_Ent := It.Nam;

            --  Name may be call that returns an access to subprogram, or more
            --  generally an overloaded expression one of whose interpretations
            --  yields an access to subprogram. If the name is an entity, we
            --  do not dereference, because the node is a call that returns
            --  the access type: note difference between f(x), where the call
            --  may return an access subprogram type, and f(x)(y), where the
            --  type returned by the call to f is implicitly dereferenced to
            --  analyze the outer call.

            if Is_Access_Type (Nam_Ent) then
               Nam_Ent := Designated_Type (Nam_Ent);

            elsif Is_Access_Type (Etype (Nam_Ent))
              and then not Is_Entity_Name (Nam)
              and then Ekind (Designated_Type (Etype (Nam_Ent)))
                                                          = E_Subprogram_Type
            then
               Nam_Ent := Designated_Type (Etype (Nam_Ent));
            end if;

            --  If the call has been rewritten from a prefixed call, the first
            --  parameter has been analyzed, but may need a subsequent
            --  dereference, so skip its analysis now.

            if N /= Original_Node (N)
              and then Nkind (Original_Node (N)) = Nkind (N)
              and then Nkind (Name (N)) /= Nkind (Name (Original_Node (N)))
              and then Present (Parameter_Associations (N))
              and then Present (Etype (First (Parameter_Associations (N))))
            then
               Analyze_One_Call
                 (N, Nam_Ent, False, Success, Skip_First => True);
            else
               Analyze_One_Call (N, Nam_Ent, False, Success);
            end if;

            --  If the interpretation succeeds, mark the proper type of the
            --  prefix (any valid candidate will do). If not, remove the
            --  candidate interpretation. This only needs to be done for
            --  overloaded protected operations, for other entities disambi-
            --  guation is done directly in Resolve.

            if Success then
               Set_Etype (Nam, It.Typ);

            elsif Nkind (Name (N)) = N_Selected_Component
              or else Nkind (Name (N)) = N_Function_Call
            then
               Remove_Interp (X);
            end if;

            Get_Next_Interp (X, It);
         end loop;

         --  If the name is the result of a function call, it can only
         --  be a call to a function returning an access to subprogram.
         --  Insert explicit dereference.

         if Nkind (Nam) = N_Function_Call then
            Insert_Explicit_Dereference (Nam);
         end if;

         if Etype (N) = Any_Type then

            --  None of the interpretations is compatible with the actuals

            Diagnose_Call (N, Nam);

            --  Special checks for uninstantiated put routines

         elsif not Is_Overloaded (N)
           and then Is_Entity_Name (Nam)
         then
            --  Resolution yields a single interpretation. Verify that
            --  is has the proper capitalization.

            Set_Entity_With_Style_Check (Nam, Entity (Nam));
            Generate_Reference (Entity (Nam), Nam);

            Set_Etype (Nam, Etype (Entity (Nam)));
         end if;

         End_Interp_List;
      end if;
   end Analyze_Call;

   ---------------------------
   -- Analyze_Comparison_Op --
   ---------------------------

   procedure Analyze_Comparison_Op (N : Node_Id) is
      L     : constant Node_Id := Left_Opnd (N);
      R     : constant Node_Id := Right_Opnd (N);
      Op_Id : Entity_Id        := Entity (N);

   begin
      Set_Etype (N, Any_Type);
      Candidate_Type := Empty;

      Analyze_Expression (L);
      Analyze_Expression (R);

      if Present (Op_Id) then

         if Ekind (Op_Id) = E_Operator then
            Find_Comparison_Types (L, R, Op_Id, N);
         else
            Add_One_Interp (N, Op_Id, Etype (Op_Id));
         end if;

         if Is_Overloaded (L) then
            Set_Etype (L, Intersect_Types (L, R));
         end if;

      else
         Op_Id := Get_Name_Entity_Id (Chars (N));

         while Present (Op_Id) loop

            if Ekind (Op_Id) = E_Operator then
               Find_Comparison_Types (L, R, Op_Id, N);
            end if;

            Op_Id := Homonym (Op_Id);
         end loop;
      end if;

      Operator_Check (N);
   end Analyze_Comparison_Op;

   ---------------------------
   -- Analyze_Concatenation --
   ---------------------------

   --  If the only one-dimensional array type in scope is String,
   --  this is the resulting type of the operation. Otherwise there
   --  will be a concatenation operation defined for each user-defined
   --  one-dimensional array.

   procedure Analyze_Concatenation (N : Node_Id) is
      L     : constant Node_Id := Left_Opnd (N);
      R     : constant Node_Id := Right_Opnd (N);
      Op_Id : Entity_Id        := Entity (N);
      LT    : Entity_Id;
      RT    : Entity_Id;

   begin
      Set_Etype (N, Any_Type);
      Candidate_Type := Empty;

      Analyze_Expression (L);
      Analyze_Expression (R);

      --  If the entity is present, the  node appears in an instance,
      --  and denotes a predefined concatenation operation. The resulting
      --  type is obtained from the arguments when possible. If the arguments
      --  are aggregates, the array type and the concatenation type must be
      --  visible.

      if Present (Op_Id) then
         if Ekind (Op_Id) = E_Operator then

            LT := Base_Type (Etype (L));
            RT := Base_Type (Etype (R));

            if Is_Array_Type (LT)
              and then (RT = LT or else RT = Base_Type (Component_Type (LT)))
            then
               Add_One_Interp (N, Op_Id, LT);

            elsif Is_Array_Type (RT)
              and then LT = Base_Type (Component_Type (RT))
            then
               Add_One_Interp (N, Op_Id, RT);

            --  If one operand is a string type or a user-defined array type,
            --  and the other is a literal, result is of the specific type.

            elsif
              (Root_Type (LT) = Standard_String
                 or else Scope (LT) /= Standard_Standard)
              and then Etype (R) = Any_String
            then
               Add_One_Interp (N, Op_Id, LT);

            elsif
              (Root_Type (RT) = Standard_String
                 or else Scope (RT) /= Standard_Standard)
              and then Etype (L) = Any_String
            then
               Add_One_Interp (N, Op_Id, RT);

            elsif not Is_Generic_Type (Etype (Op_Id)) then
               Add_One_Interp (N, Op_Id, Etype (Op_Id));

            else
               --  Type and its operations must be visible.

               Set_Entity (N, Empty);
               Analyze_Concatenation (N);

            end if;

         else
            Add_One_Interp (N, Op_Id, Etype (Op_Id));
         end if;

      else
         Op_Id  := Get_Name_Entity_Id (Name_Op_Concat);

         while Present (Op_Id) loop
            if Ekind (Op_Id) = E_Operator then
               Find_Concatenation_Types (L, R, Op_Id, N);
            end if;

            Op_Id := Homonym (Op_Id);
         end loop;
      end if;

      Operator_Check (N);
   end Analyze_Concatenation;

   ------------------------------------
   -- Analyze_Conditional_Expression --
   ------------------------------------

   procedure Analyze_Conditional_Expression (N : Node_Id) is
      Condition : constant Node_Id := First (Expressions (N));
      Then_Expr : constant Node_Id := Next (Condition);
      Else_Expr : constant Node_Id := Next (Then_Expr);

   begin
      Analyze_Expression (Condition);
      Analyze_Expression (Then_Expr);
      Analyze_Expression (Else_Expr);
      Set_Etype (N, Etype (Then_Expr));
   end Analyze_Conditional_Expression;

   -------------------------
   -- Analyze_Equality_Op --
   -------------------------

   procedure Analyze_Equality_Op (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      L      : constant Node_Id := Left_Opnd (N);
      R      : constant Node_Id := Right_Opnd (N);
      Op_Id  : Entity_Id;

   begin
      Set_Etype (N, Any_Type);
      Candidate_Type := Empty;

      Analyze_Expression (L);
      Analyze_Expression (R);

      --  If the entity is set, the node is a generic instance with a non-local
      --  reference to the predefined operator or to a user-defined function.
      --  It can also be an inequality that is expanded into the negation of a
      --  call to a user-defined equality operator.

      --  For the predefined case, the result is Boolean, regardless of the
      --  type of the  operands. The operands may even be limited, if they are
      --  generic actuals. If they are overloaded, label the left argument with
      --  the common type that must be present, or with the type of the formal
      --  of the user-defined function.

      if Present (Entity (N)) then

         Op_Id := Entity (N);

         if Ekind (Op_Id) = E_Operator then
            Add_One_Interp (N, Op_Id, Standard_Boolean);
         else
            Add_One_Interp (N, Op_Id, Etype (Op_Id));
         end if;

         if Is_Overloaded (L) then

            if Ekind (Op_Id) = E_Operator then
               Set_Etype (L, Intersect_Types (L, R));
            else
               Set_Etype (L, Etype (First_Formal (Op_Id)));
            end if;
         end if;

      else
         Op_Id := Get_Name_Entity_Id (Chars (N));

         while Present (Op_Id) loop

            if Ekind (Op_Id) = E_Operator then
               Find_Equality_Types (L, R, Op_Id, N);
            end if;

            Op_Id := Homonym (Op_Id);
         end loop;
      end if;

      --  If there was no match, and the operator is inequality, this may
      --  be a case where inequality has not been made explicit, as for
      --  tagged types. Analyze the node as the negation of an equality
      --  operation. This cannot be done earlier, because before analysis
      --  we cannot rule out the presence of an explicit inequality.

      if Etype (N) = Any_Type
        and then Nkind (N) = N_Op_Ne
      then
         Op_Id := Get_Name_Entity_Id (Name_Op_Eq);

         while Present (Op_Id) loop

            if Ekind (Op_Id) = E_Operator then
               Find_Equality_Types (L, R, Op_Id, N);
            end if;

            Op_Id := Homonym (Op_Id);
         end loop;

         if Etype (N) /= Any_Type then
            Op_Id := Entity (N);

            Rewrite (N,
              Make_Op_Not (Loc,
                Right_Opnd =>
                  Make_Op_Eq (Loc,
                    Left_Opnd =>  Relocate_Node (Left_Opnd (N)),
                    Right_Opnd => Relocate_Node (Right_Opnd (N)))));

            Set_Entity (Right_Opnd (N), Op_Id);
            Analyze (N);
         end if;
      end if;

      Operator_Check (N);
   end Analyze_Equality_Op;

   ----------------------------------
   -- Analyze_Explicit_Dereference --
   ----------------------------------

   procedure Analyze_Explicit_Dereference (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      P     : constant Node_Id := Prefix (N);
      T     : Entity_Id;
      I     : Interp_Index;
      It    : Interp;
      New_N : Node_Id;

      function Is_Function_Type return Boolean;
      --  Check whether node may be interpreted as an implicit function call.

      function Is_Function_Type return Boolean is
         I     : Interp_Index;
         It    : Interp;

      begin
         if not Is_Overloaded (N) then
            return Ekind (Base_Type (Etype (N))) = E_Subprogram_Type
              and then Etype (Base_Type (Etype (N))) /= Standard_Void_Type;

         else
            Get_First_Interp (N, I, It);

            while Present (It.Nam) loop
               if Ekind (Base_Type (It.Typ)) /= E_Subprogram_Type
                 or else Etype (Base_Type (It.Typ)) = Standard_Void_Type
               then
                  return False;
               end if;

               Get_Next_Interp (I, It);
            end loop;

            return True;
         end if;
      end Is_Function_Type;

   begin
      Analyze (P);
      Set_Etype (N, Any_Type);

      --  Normal processing for other than remote access to subprogram type

      if not Is_Overloaded (P) then
         if Is_Access_Type (Etype (P)) then

            --  Set the Etype. We need to go thru Is_For_Access_Subtypes
            --  to avoid other problems caused by the Private_Subtype
            --  and it is safe to go to the Base_Type because this is the
            --  same as converting the access value to its Base_Type.

            declare
               DT : Entity_Id := Designated_Type (Etype (P));

            begin
               if Ekind (DT) = E_Private_Subtype
                 and then Is_For_Access_Subtype (DT)
               then
                  DT := Base_Type (DT);
               end if;

               Set_Etype (N, DT);
            end;

         elsif Etype (P) /= Any_Type then
            Error_Msg_N ("prefix of dereference must be an access type", N);
            return;
         end if;

      else
         Get_First_Interp (P, I, It);

         while Present (It.Nam) loop
            T := It.Typ;

            if Is_Access_Type (T) then
               Add_One_Interp (N, Designated_Type (T), Designated_Type (T));
            end if;

            Get_Next_Interp (I, It);
         end loop;

         End_Interp_List;

         --  Error if no interpretation of the prefix has an access type.

         if Etype (N) = Any_Type then
            Error_Msg_N
              ("access type required in prefix of explicit dereference", P);
            Set_Etype (N, Any_Type);
            return;
         end if;
      end if;

      if Is_Function_Type
        and then Nkind (Parent (N)) /= N_Indexed_Component

        and then (Nkind (Parent (N)) /= N_Function_Call
                   or else N /= Name (Parent (N)))

        and then (Nkind (Parent (N)) /= N_Procedure_Call_Statement
                   or else N /= Name (Parent (N)))

        and then Nkind (Parent (N)) /= N_Subprogram_Renaming_Declaration
        and then (Nkind (Parent (N)) /= N_Attribute_Reference
                    or else
                      (Attribute_Name (Parent (N)) /= Name_Address
                        and then
                       Attribute_Name (Parent (N)) /= Name_Access))
      then
         --  Name is a function call with no actuals, in a context that
         --  requires deproceduring (including as an actual in an enclosing
         --  function or procedure call). We can conceive of pathological cases
         --  where the prefix might include functions that return access to
         --  subprograms and others that return a regular type. Disambiguation
         --  of those will have to take place in Resolve. See e.g. 7117-014.

         New_N :=
           Make_Function_Call (Loc,
           Name => Make_Explicit_Dereference (Loc, P),
           Parameter_Associations => New_List);

         --  If the prefix is overloaded, remove operations that have formals,
         --  we know that this is a parameterless call.

         if Is_Overloaded (P) then
            Get_First_Interp (P, I, It);

            while Present (It.Nam) loop
               T := It.Typ;

               if No (First_Formal (Base_Type (Designated_Type (T)))) then
                  Set_Etype (P, T);
               else
                  Remove_Interp (I);
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end if;

         Rewrite (N, New_N);
         Analyze (N);
      end if;

   end Analyze_Explicit_Dereference;

   ------------------------
   -- Analyze_Expression --
   ------------------------

   procedure Analyze_Expression (N : Node_Id) is
   begin
      Analyze (N);
      Check_Parameterless_Call (N);
   end Analyze_Expression;

   ------------------------------------
   -- Analyze_Indexed_Component_Form --
   ------------------------------------

   procedure Analyze_Indexed_Component_Form (N : Node_Id) is
      P     : constant Node_Id := Prefix (N);
      Exprs : constant List_Id := Expressions (N);
      Exp   : Node_Id;
      P_T   : Entity_Id;
      E     : Node_Id;
      U_N   : Entity_Id;

      procedure Process_Function_Call;
      --  Prefix in indexed component form is an overloadable entity,
      --  so the node is a function call. Reformat it as such.

      procedure Process_Indexed_Component;
      --  Prefix in indexed component form is actually an indexed component.
      --  This routine processes it, knowing that the prefix is already
      --  resolved.

      procedure Process_Indexed_Component_Or_Slice;
      --  An indexed component with a single index may designate a slice if
      --  the index is a subtype mark. This routine disambiguates these two
      --  cases by resolving the prefix to see if it is a subtype mark.

      procedure Process_Overloaded_Indexed_Component;
      --  If the prefix of an indexed component is overloaded, the proper
      --  interpretation is selected by the index types and the context.

      ---------------------------
      -- Process_Function_Call --
      ---------------------------

      procedure Process_Function_Call is
         Actual : Node_Id;

      begin
         Change_Node (N, N_Function_Call);
         Set_Name (N, P);
         Set_Parameter_Associations (N, Exprs);
         Actual := First (Parameter_Associations (N));

         while Present (Actual) loop
            Analyze (Actual);
            Check_Parameterless_Call (Actual);
            Next_Actual (Actual);
         end loop;

         Analyze_Call (N);
      end Process_Function_Call;

      -------------------------------
      -- Process_Indexed_Component --
      -------------------------------

      procedure Process_Indexed_Component is
         Exp          : Node_Id;
         Array_Type   : Entity_Id;
         Index        : Node_Id;

      begin
         Exp := First (Exprs);

         if Is_Overloaded (P) then
            Process_Overloaded_Indexed_Component;

         else
            Array_Type := Etype (P);

            --  Prefix must be appropriate for an array type.
            --  Dereference the prefix if it is an access type.

            if Is_Access_Type (Array_Type) then
               Array_Type := Designated_Type (Array_Type);
               Error_Msg_NW (Warn_On_Dereference, "?implicit dereference", N);
            end if;

            if Is_Array_Type (Array_Type) then
               null;

            elsif Array_Type = Any_Type then
               Set_Etype (N, Any_Type);
               return;

            --  Here we definitely have a bad indexing

            else
               if Is_Entity_Name (P)
                 and then Etype (P) = Standard_Void_Type
               then
                  Error_Msg_NE ("incorrect use of&", P, Entity (P));

               else
                  Error_Msg_N ("array type required in indexed component", P);
               end if;

               Set_Etype (N, Any_Type);
               return;
            end if;

            Index := First_Index (Array_Type);

            while Present (Index) and then Present (Exp) loop
               if not Has_Compatible_Type (Exp, Etype (Index)) then
                  Wrong_Type (Exp, Etype (Index));
                  Set_Etype (N, Any_Type);
                  return;
               end if;

               Next_Index (Index);
               Next (Exp);
            end loop;

            Set_Etype (N, Component_Type (Array_Type));

            if Present (Index) then
               Error_Msg_N
                 ("too few subscripts in array reference", First (Exprs));

            elsif Present (Exp) then
               Error_Msg_N ("too many subscripts in array reference", Exp);
            end if;
         end if;

      end Process_Indexed_Component;

      ----------------------------------------
      -- Process_Indexed_Component_Or_Slice --
      ----------------------------------------

      procedure Process_Indexed_Component_Or_Slice is
      begin
         Exp := First (Exprs);

         while Present (Exp) loop
            Analyze_Expression (Exp);
            Next (Exp);
         end loop;

         Exp := First (Exprs);

         --  If one index is present, and it is a subtype name, then the
         --  node denotes a slice (note that the case of an explicit range
         --  for a slice was already built as an N_Slice node in the first
         --  place, so that case is not handled here).

         --  We use a replace rather than a rewrite here because this is one
         --  of the cases in which the tree built by the parser is plain wrong.

         if No (Next (Exp))
           and then Is_Entity_Name (Exp)
           and then Is_Type (Entity (Exp))
         then
            Replace (N,
               Make_Slice (Sloc (N),
                 Prefix => P,
                 Discrete_Range => New_Copy (Exp)));
            Analyze (N);

         --  Otherwise (more than one index present, or single index is not
         --  a subtype name), then we have the indexed component case.

         else
            Process_Indexed_Component;
         end if;
      end Process_Indexed_Component_Or_Slice;

      ------------------------------------------
      -- Process_Overloaded_Indexed_Component --
      ------------------------------------------

      procedure Process_Overloaded_Indexed_Component is
         Exp   : Node_Id;
         I     : Interp_Index;
         It    : Interp;
         Typ   : Entity_Id;
         Index : Node_Id;
         Found : Boolean;

      begin
         Set_Etype (N, Any_Type);
         Get_First_Interp (P, I, It);

         while Present (It.Nam) loop
            Typ := It.Typ;

            if Is_Access_Type (Typ) then
               Typ := Designated_Type (Typ);
               Error_Msg_NW (Warn_On_Dereference, "?implicit dereference", N);
            end if;

            if Is_Array_Type (Typ) then

               --  Got a candidate: verify that index types are compatible

               Index := First_Index (Typ);
               Found := True;

               Exp := First (Exprs);

               while Present (Index) and then Present (Exp) loop
                  if Has_Compatible_Type (Exp, Etype (Index)) then
                     null;
                  else
                     Found := False;
                     Remove_Interp (I);
                     exit;
                  end if;

                  Next_Index (Index);
                  Next (Exp);
               end loop;

               if Found and then No (Index) and then No (Exp) then
                  Add_One_Interp (N,
                     Etype (Component_Type (Typ)),
                     Etype (Component_Type (Typ)));
               end if;
            end if;

            Get_Next_Interp (I, It);
         end loop;

         if Etype (N) = Any_Type then
            Error_Msg_N ("no legal interpetation for indexed component", N);
            Set_Is_Overloaded (N, False);
         end if;

         End_Interp_List;
      end Process_Overloaded_Indexed_Component;

   ------------------------------------
   -- Analyze_Indexed_Component_Form --
   ------------------------------------

   begin
      --  Get name of array, function or type

      Analyze (P);
      if Nkind (N) = N_Function_Call
        or else Nkind (N) = N_Procedure_Call_Statement
      then
         --  If P is an explicit dereference whose prefix is of a
         --  remote access-to-subprogram type, then N has already
         --  been rewritten as a subprogram call and analyzed.

         return;
      end if;

      pragma Assert (Nkind (N) = N_Indexed_Component);

      P_T := Base_Type (Etype (P));

      if Is_Entity_Name (P)
        or else Nkind (P) = N_Operator_Symbol
      then
         U_N := Entity (P);

         if Ekind (U_N) in  Type_Kind then

            --  Reformat node as a type conversion.

            E := Remove_Head (Exprs);

            if Present (First (Exprs)) then
               Error_Msg_N
                ("argument of type conversion must be single expression", N);
            end if;

            Change_Node (N, N_Type_Conversion);
            Set_Subtype_Mark (N, P);
            Set_Etype (N, U_N);
            Set_Expression (N, E);

            --  After changing the node, call for the specific Analysis
            --  routine directly, to avoid a double call to the expander.

            Analyze_Type_Conversion (N);
            return;
         end if;

         if Is_Overloadable (U_N) then
            Process_Function_Call;

         elsif Ekind (Etype (P)) = E_Subprogram_Type
           or else (Is_Access_Type (Etype (P))
                      and then
                    Ekind (Designated_Type (Etype (P))) = E_Subprogram_Type)
         then
            --  Call to access_to-subprogram with possible implicit dereference

            Process_Function_Call;

         elsif Is_Generic_Subprogram (U_N) then

            --  A common beginner's (or C++ templates fan) error.

            Error_Msg_N ("generic subprogram cannot be called", N);
            Set_Etype (N, Any_Type);
            return;

         else
            Process_Indexed_Component_Or_Slice;
         end if;

      --  If not an entity name, prefix is an expression that may denote
      --  an array or an access-to-subprogram.

      else
         if Ekind (P_T) = E_Subprogram_Type
           or else (Is_Access_Type (P_T)
                     and then
                    Ekind (Designated_Type (P_T)) = E_Subprogram_Type)
         then
            Process_Function_Call;

         elsif Nkind (P) = N_Selected_Component
           and then Ekind (Entity (Selector_Name (P))) = E_Function
         then
            Process_Function_Call;

         else
            --  Indexed component, slice, or a call to a member of a family
            --  entry, which will be converted to an entry call later.

            Process_Indexed_Component_Or_Slice;
         end if;
      end if;
   end Analyze_Indexed_Component_Form;

   ------------------------
   -- Analyze_Logical_Op --
   ------------------------

   procedure Analyze_Logical_Op (N : Node_Id) is
      L     : constant Node_Id := Left_Opnd (N);
      R     : constant Node_Id := Right_Opnd (N);
      Op_Id : Entity_Id := Entity (N);

   begin
      Set_Etype (N, Any_Type);
      Candidate_Type := Empty;

      Analyze_Expression (L);
      Analyze_Expression (R);

      if Present (Op_Id) then

         if Ekind (Op_Id) = E_Operator then
            Find_Boolean_Types (L, R, Op_Id, N);
         else
            Add_One_Interp (N, Op_Id, Etype (Op_Id));
         end if;

      else
         Op_Id := Get_Name_Entity_Id (Chars (N));

         while Present (Op_Id) loop
            if Ekind (Op_Id) = E_Operator then
               Find_Boolean_Types (L, R, Op_Id, N);
            end if;

            Op_Id := Homonym (Op_Id);
         end loop;
      end if;

      Operator_Check (N);
   end Analyze_Logical_Op;

   ---------------------------
   -- Analyze_Membership_Op --
   ---------------------------

   procedure Analyze_Membership_Op (N : Node_Id) is
      L     : constant Node_Id := Left_Opnd (N);
      R     : constant Node_Id := Right_Opnd (N);

      Index : Interp_Index;
      It    : Interp;
      Found : Boolean := False;
      I_F   : Interp_Index;
      T_F   : Entity_Id;

      procedure Try_One_Interp (T1 : Entity_Id);
      --  Routine to try one proposed interpretation. Note that the context
      --  of the operation plays no role in resolving the arguments, so that
      --  if there is more than one interpretation of the operands that is
      --  compatible with a membership test, the operation is ambiguous.

      procedure Try_One_Interp (T1 : Entity_Id) is
      begin
         if Has_Compatible_Type (R, T1) then
            if Found
              and then Base_Type (T1) /= Base_Type (T_F)
            then
               It := Disambiguate (L, I_F, Index, Any_Type);

               if It = No_Interp then
                  Ambiguous_Operands (N);
                  Set_Etype (L, Any_Type);
                  return;

               else
                  T_F := It.Typ;
               end if;

            else
               Found := True;
               T_F   := T1;
               I_F   := Index;
            end if;

            Set_Etype (L, T_F);
         end if;

      end Try_One_Interp;

   --  Start of processing for Analyze_Membership_Op

   begin
      Analyze_Expression (L);

      if Nkind (R) = N_Range
        or else (Nkind (R) = N_Attribute_Reference
                  and then Attribute_Name (R) = Name_Range)
      then
         Analyze (R);

         if not Is_Overloaded (L) then
            Try_One_Interp (Etype (L));

         else
            Get_First_Interp (L, Index, It);

            while Present (It.Typ) loop
               Try_One_Interp (It.Typ);
               Get_Next_Interp (Index, It);
            end loop;
         end if;

      --  If not a range, it can only be a subtype mark, or else there
      --  is a more basic error, to be diagnosed in Find_Type.

      else
         Find_Type (R);

         if Is_Entity_Name (R) then
            Check_Fully_Declared (Entity (R), R);
         end if;
      end if;

      --  Compatibility between expression and subtype mark or range is
      --  checked during resolution. The result of the operation is Boolean
      --  in any case.

      Set_Etype (N, Standard_Boolean);
   end Analyze_Membership_Op;

   ----------------------
   -- Analyze_Negation --
   ----------------------

   procedure Analyze_Negation (N : Node_Id) is
      R     : constant Node_Id := Right_Opnd (N);
      Op_Id : Entity_Id := Entity (N);

   begin
      Set_Etype (N, Any_Type);
      Candidate_Type := Empty;

      Analyze_Expression (R);

      if Present (Op_Id) then
         if Ekind (Op_Id) = E_Operator then
            Find_Negation_Types (R, Op_Id, N);
         else
            Add_One_Interp (N, Op_Id, Etype (Op_Id));
         end if;

      else
         Op_Id := Get_Name_Entity_Id (Chars (N));

         while Present (Op_Id) loop
            if Ekind (Op_Id) = E_Operator then
               Find_Negation_Types (R, Op_Id, N);
            end if;

            Op_Id := Homonym (Op_Id);
         end loop;
      end if;

      Operator_Check (N);
   end Analyze_Negation;

   -------------------
   --  Analyze_Null --
   -------------------

   procedure Analyze_Null (N : Node_Id) is
   begin
      Set_Etype (N, Any_Access);
   end Analyze_Null;

   ----------------------
   -- Analyze_One_Call --
   ----------------------

   procedure Analyze_One_Call
      (N          : Node_Id;
       Nam        : Entity_Id;
       Report     : Boolean;
       Success    : out Boolean;
       Skip_First : Boolean := False)
   is
      Actuals    : constant List_Id   := Parameter_Associations (N);
      Prev_T     : constant Entity_Id := Etype (N);
      
      Must_Skip  : constant Boolean := Skip_First
                     or else Nkind (Original_Node (N)) = N_Selected_Component
                     or else
                       (Nkind (Original_Node (N)) = N_Indexed_Component
                          and then Nkind (Prefix (Original_Node (N)))
                            = N_Selected_Component);
      --  The first formal must be omitted from the match when trying to find
      --  a primitive operation that is a possible interpretation, and also
      --  after the call has been rewritten, because the corresponding actual
      --  is already known to be compatible, and because this may be an
      --  indexing of a call with default parameters.
      
      Formal     : Entity_Id;
      Actual     : Node_Id;
      Is_Indexed : Boolean := False;
      Subp_Type  : constant Entity_Id := Etype (Nam);
      Norm_OK    : Boolean;

      procedure Indicate_Name_And_Type;
      --  If candidate interpretation matches, indicate name and type of
      --  result on call node.

      ----------------------------
      -- Indicate_Name_And_Type --
      ----------------------------

      procedure Indicate_Name_And_Type is
      begin
         Add_One_Interp (N, Nam, Etype (Nam));
         Success := True;

         --  If the prefix of the call is a name, indicate the entity
         --  being called. If it is not a name,  it is an expression that
         --  denotes an access to subprogram or else an entry or family. In
         --  the latter case, the name is a selected component, and the entity
         --  being called is noted on the selector.

         if not Is_Type (Nam) then
            if Is_Entity_Name (Name (N))
              or else Nkind (Name (N)) = N_Operator_Symbol
            then
               Set_Entity (Name (N), Nam);

            elsif Nkind (Name (N)) = N_Selected_Component then
               Set_Entity (Selector_Name (Name (N)),  Nam);
            end if;
         end if;

         if Debug_Flag_E and not Report then
            Write_Str (" Overloaded call ");
            Write_Int (Int (N));
            Write_Str (" compatible with ");
            Write_Int (Int (Nam));
            Write_Eol;
         end if;
      end Indicate_Name_And_Type;

   --  Start of processing for Analyze_One_Call

   begin
      Success := False;

      --  If the subprogram has no formals, or if all the formals have
      --  defaults, and the return type is an array type, the node may
      --  denote an indexing of the result of a parameterless call.

      if Needs_No_Actuals (Nam)
        and then Present (Actuals)
      then
         if Is_Array_Type (Subp_Type) then
            Is_Indexed := Try_Indexed_Call (N, Nam, Subp_Type, Must_Skip);

         elsif Is_Access_Type (Subp_Type)
           and then Is_Array_Type (Designated_Type (Subp_Type))
         then
            Is_Indexed :=
              Try_Indexed_Call
	      (N, Nam, Designated_Type (Subp_Type), Must_Skip);

         elsif Is_Access_Type (Subp_Type)
           and then Ekind (Designated_Type (Subp_Type))  = E_Subprogram_Type
         then
            Is_Indexed := Try_Indirect_Call (N, Nam, Subp_Type);
         end if;

      end if;

      Normalize_Actuals (N, Nam, (Report and not Is_Indexed), Norm_OK);

      if not Norm_OK then

         --  Mismatch in number or names of parameters

         if Debug_Flag_E then
            Write_Str (" normalization fails in call ");
            Write_Int (Int (N));
            Write_Str (" with subprogram ");
            Write_Int (Int (Nam));
            Write_Eol;
         end if;

      --  If the context expects a function call, discard any interpretation
      --  that is a procedure. If the node is not overloaded, leave as is for
      --  better error reporting when type mismatch is found.

      elsif Nkind (N) = N_Function_Call
        and then Is_Overloaded (Name (N))
        and then Ekind (Nam) = E_Procedure
      then
         return;

      --  Ditto for function calls in a procedure context.

      elsif Nkind (N) = N_Procedure_Call_Statement
         and then Is_Overloaded (Name (N))
         and then Etype (Nam) /= Standard_Void_Type
      then
         return;

      elsif not Present (Actuals) then

         --  If Normalize succeeds, then there are default parameters for
         --  all formals.

         Indicate_Name_And_Type;

      else
         --  Normalize_Actuals has chained the named associations in the
         --  correct order of the formals.

         Actual := First_Actual (N);
         Formal := First_Formal (Nam);

         while Present (Actual) and then Present (Formal) loop

            if Nkind (Parent (Actual)) /= N_Parameter_Association
              or else Chars (Selector_Name (Parent (Actual))) = Chars (Formal)
            then
               if Has_Compatible_Type (Actual, Etype (Formal)) then
                  Next_Actual (Actual);
                  Next_Formal (Formal);

               else
                  if Debug_Flag_E then
                     Write_Str (" type checking fails in call ");
                     Write_Int (Int (N));
                     Write_Str (" with formal ");
                     Write_Int (Int (Formal));
                     Write_Str (" in subprogram ");
                     Write_Int (Int (Nam));
                     Write_Eol;
                  end if;

                  if Report and not Is_Indexed then

                     Wrong_Type (Actual, Etype (Formal));

                     if Nkind (Actual) = N_Op_Eq
                       and then Nkind (Left_Opnd (Actual)) = N_Identifier
                     then
                        Formal := First_Formal (Nam);

                        while Present (Formal) loop

                           if Chars (Left_Opnd (Actual)) = Chars (Formal) then
                              Error_Msg_N
                                ("possible misspelling of `='>`!", Actual);
                              exit;
                           end if;

                           Next_Formal (Formal);
                        end loop;
                     end if;

                     if All_Errors_Mode then
                        Error_Msg_Sloc := Sloc (Nam);

                        if Is_Overloadable (Nam)
                          and then Present (Alias (Nam))
                          and then not Comes_From_Source (Nam)
                        then
                           Error_Msg_NE
                             ("  =='> in call to &#(inherited)!", Actual, Nam);
                        else
                           Error_Msg_NE ("  =='> in call to &#!", Actual, Nam);
                        end if;
                     end if;
                  end if;

                  return;
               end if;

            else
               --  Normalize_Actuals has verified that a default value exists
               --  for this formal. Current actual names a subsequent formal.

               Next_Formal (Formal);
            end if;
         end loop;

         --  On exit, all actuals match.

         Indicate_Name_And_Type;
      end if;
   end Analyze_One_Call;

   -------------------------------------------
   -- Analyze_Overloaded_Selected_Component --
   -------------------------------------------

   procedure Analyze_Overloaded_Selected_Component (N : Node_Id) is
      Nam   : constant Node_Id := Prefix (N);
      Sel   : constant Node_Id := Selector_Name (N);
      Comp  : Entity_Id;
      I     : Interp_Index;
      It    : Interp;
      T     : Entity_Id;

   begin
      Get_First_Interp (Nam, I, It);

      Set_Etype (Sel,  Any_Type);

      while Present (It.Typ) loop
         if Is_Access_Type (It.Typ) then
            T := Designated_Type (It.Typ);
            Error_Msg_NW (Warn_On_Dereference, "?implicit dereference", N);

         else
            T := It.Typ;
         end if;

         if Is_Record_Type (T) then
            Comp := First_Entity (T);

            while Present (Comp) loop

               if Chars (Comp) = Chars (Sel)
                 and then Is_Visible_Component (Comp)
               then
                  Set_Entity_With_Style_Check (Sel, Comp);
                  Generate_Reference (Comp, Sel);

                  Set_Etype (Sel, Etype (Comp));
                  Add_One_Interp (N, Etype (Comp), Etype (Comp));

                  --  This also specifies a candidate to resolve the name.
                  --  Further overloading will be resolved from context.

                  Set_Etype (Nam, It.Typ);
               end if;

               Next_Entity (Comp);
            end loop;
         end if;

         Get_Next_Interp (I, It);
      end loop;

      if Etype (N) = Any_Type 
        and then not Try_Object_Operation (N)
      then
         Error_Msg_NE ("undefined selector& for overloaded prefix", N, Sel);
         Set_Entity (Sel, Any_Id);
         Set_Etype  (Sel, Any_Type);
      end if;

   end Analyze_Overloaded_Selected_Component;

   ----------------------------------
   -- Analyze_Qualified_Expression --
   ----------------------------------

   procedure Analyze_Qualified_Expression (N : Node_Id) is
      Mark : constant Entity_Id := Subtype_Mark (N);
      T    : Entity_Id;

   begin
      Set_Etype (N, Any_Type);
      Find_Type (Mark);
      T := Entity (Mark);

      if T = Any_Type then
         return;
      end if;
      Check_Fully_Declared (T, N);

      Analyze_Expression (Expression (N));
      Set_Etype  (N, T);
   end Analyze_Qualified_Expression;

   -------------------
   -- Analyze_Range --
   -------------------

   procedure Analyze_Range (N : Node_Id) is
      L        : constant Node_Id := Low_Bound (N);
      H        : constant Node_Id := High_Bound (N);
      I1, I2   : Interp_Index;
      It1, It2 : Interp;

      procedure Check_Common_Type (T1, T2 : Entity_Id);
      --  Verify the compatibility of two types,  and choose the
      --  non universal one if the other is universal.

      procedure Check_High_Bound (T : Entity_Id);
      --  Test one interpretation of the low bound against all those
      --  of the high bound.

      procedure Check_Universal_Expression (N : Node_Id);
      --  In Ada83, reject bounds of a universal range that are not
      --  literals or entity names.

      -----------------------
      -- Check_Common_Type --
      -----------------------

      procedure Check_Common_Type (T1, T2 : Entity_Id) is
      begin
         if Covers (T1, T2) or else Covers (T2, T1) then
            if T1 = Universal_Integer
              or else T1 = Universal_Real
              or else T1 = Any_Character
            then
               Add_One_Interp (N, Base_Type (T2), Base_Type (T2));

            elsif T1 = T2 then
               Add_One_Interp (N, T1, T1);

            else
               Add_One_Interp (N, Base_Type (T1), Base_Type (T1));
            end if;
         end if;
      end Check_Common_Type;

      ----------------------
      -- Check_High_Bound --
      ----------------------

      procedure Check_High_Bound (T : Entity_Id) is
      begin
         if not Is_Overloaded (H) then
            Check_Common_Type (T, Etype (H));
         else
            Get_First_Interp (H, I2, It2);

            while Present (It2.Typ) loop
               Check_Common_Type (T, It2.Typ);
               Get_Next_Interp (I2, It2);
            end loop;
         end if;
      end Check_High_Bound;

      -----------------------------
      -- Is_Universal_Expression --
      -----------------------------

      procedure Check_Universal_Expression (N : Node_Id) is
      begin
         if Etype (N) = Universal_Integer
           and then Nkind (N) /= N_Integer_Literal
           and then not Is_Entity_Name (N)
           and then Nkind (N) /= N_Attribute_Reference
         then
            Error_Msg_N ("illegal bound in discrete range", N);
         end if;
      end Check_Universal_Expression;

   --  Start of processing for Analyze_Range

   begin
      Set_Etype (N, Any_Type);
      Analyze_Expression (L);
      Analyze_Expression (H);

      if Etype (L) = Any_Type or else Etype (H) = Any_Type then
         return;

      else
         if not Is_Overloaded (L) then
            Check_High_Bound (Etype (L));
         else
            Get_First_Interp (L, I1, It1);

            while Present (It1.Typ) loop
               Check_High_Bound (It1.Typ);
               Get_Next_Interp (I1, It1);
            end loop;
         end if;

         --  If result is Any_Type, then we did not find a compatible pair

         if Etype (N) = Any_Type then
            Error_Msg_N ("incompatible types in range ", N);
         end if;
      end if;

      if Ada_83
        and then
          (Nkind (Parent (N)) = N_Loop_Parameter_Specification
            or else Nkind (Parent (N)) = N_Constrained_Array_Definition)
      then
         Check_Universal_Expression (L);
         Check_Universal_Expression (H);
      end if;
   end Analyze_Range;

   -----------------------
   -- Analyze_Reference --
   -----------------------

   procedure Analyze_Reference (N : Node_Id) is
      P        : constant Node_Id := Prefix (N);
      Acc_Type : Entity_Id;

   begin
      Analyze (P);
      Acc_Type := Create_Itype (E_Allocator_Type, N);
      Set_Etype                    (Acc_Type,  Acc_Type);
      Init_Size_Align              (Acc_Type);
      Set_Directly_Designated_Type (Acc_Type, Etype (P));
      Set_Etype (N, Acc_Type);
   end Analyze_Reference;

   --------------------------------
   -- Analyze_Selected_Component --
   --------------------------------

   --  Prefix is a record type or a task or protected type. In the
   --  later case, the selector must denote a visible entry.

   procedure Analyze_Selected_Component (N : Node_Id) is
      Name        : constant Node_Id := Prefix (N);
      Sel         : constant Node_Id := Selector_Name (N);
      Comp        : Entity_Id;
      Entity_List : Entity_Id;
      Prefix_Type : Entity_Id;
      Act_Decl    : Node_Id;
      Parent_N    : Node_Id;

   --  Start of processing for Analyze_Selected_Component

   begin
      Set_Etype (N, Any_Type);

      if Is_Overloaded (Name) then
         Analyze_Overloaded_Selected_Component (N);
         return;

      elsif Etype (Name) = Any_Type then
         Set_Entity (Sel, Any_Id);
         Set_Etype (Sel, Any_Type);
         return;

      else
         --  Function calls that are prefixes of selected components must be
         --  fully resolved in case we need to build an actual subtype, or
         --  do some other operation requiring a fully resolved prefix.

         --  Note: Resolving all Nkinds of nodes here doesn't work.
         --  (Breaks 2129-008) ???.

         if Nkind (Name) = N_Function_Call then
            Resolve (Name);
         end if;

         Prefix_Type := Etype (Name);
      end if;

      if Is_Access_Type (Prefix_Type) then

	 Error_Msg_NW (Warn_On_Dereference, "?implicit dereference", N);
	 
         Prefix_Type := Designated_Type (Prefix_Type);
      end if;

      if Ekind (Prefix_Type) = E_Private_Subtype then
         Prefix_Type := Base_Type (Prefix_Type);
      end if;

      Entity_List := Prefix_Type;

      --  For class-wide types, use the entity list of the root type. This
      --  indirection is specially important for private extensions because
      --  only the root type get switched (not the class-wide type).

      if Is_Class_Wide_Type (Prefix_Type) then
         Entity_List := Root_Type (Prefix_Type);
      end if;

      Comp := First_Entity (Entity_List);

      --  If the selector has an original discriminant, the node appears in
      --  an instance. Replace the discriminant with the corresponding one
      --  in the current discriminated type. For nested generics, this must
      --  be done transitively, so note the new original discriminant.

      if Is_Record_Type (Prefix_Type) then

         --  Find component with given name

         while Present (Comp) loop

            if Chars (Comp) = Chars (Sel)
              and then Is_Visible_Component (Comp)
            then
               Set_Entity_With_Style_Check (Sel, Comp);
               Generate_Reference (Comp, Sel);

               Set_Etype (Sel, Etype (Comp));

               --  Resolve the prefix early otherwise it is not possible to
               --  build the actual subtype of the component: it may need
               --  to duplicate this prefix and duplication is only allowed
               --  on fully resolved expressions.

               Resolve (Name);

               --  We never need an actual subtype for the case of a selection
               --  for a indexed component of a non-packed array, since in
               --  this case gigi generates all the checks and can find the
               --  necessary bounds information.

               --  We also do not need an actual subtype for the case of
               --  a first, last, length, or range attribute applied to a
               --  non-packed array, since gigi can again get the bounds in
               --  these cases (gigi cannot handle the packed case, since it
               --  has the bounds of the packed array type, not the original
               --  bounds of the type). However, if the prefix is itself a
               --  selected component, as in a.b.c (i), gigi may regard a.b.c
               --  as a dynamic-sized temporary, so we do generate an actual
               --  subtype for this case.

               Parent_N := Parent (N);

               if not Is_Packed (Etype (Comp))
                 and then
                   ((Nkind (Parent_N) = N_Indexed_Component
                      and then Nkind (Name) /= N_Selected_Component)
                     or else
                      (Nkind (Parent_N) = N_Attribute_Reference
                         and then (Attribute_Name (Parent_N) = Name_First
                                    or else
                                   Attribute_Name (Parent_N) = Name_Last
                                    or else
                                   Attribute_Name (Parent_N) = Name_Length
                                    or else
                                   Attribute_Name (Parent_N) = Name_Range)))
               then
                  Set_Etype (N, Etype (Comp));

               --  In all other cases, we currently build an actual subtype. It
               --  seems likely that many of these cases can be avoided, but
               --  right now, the front end makes direct references to the
               --  bounds (e.g. in generating a length check), and if we do
               --  not make an actual subtype, we end up getting a direct
               --  reference to a discriminant which will not do.

               else
                  Act_Decl :=
                    Build_Actual_Subtype_Of_Component (Etype (Comp), N);
                  Insert_Action (N, Act_Decl);

                  if No (Act_Decl) then
                     Set_Etype (N, Etype (Comp));

                  else
                     --  Component type depends on discriminants. Enter the
                     --  main attributes of the subtype.

                     declare
                        Subt : constant Entity_Id :=
                                 Defining_Identifier (Act_Decl);

                     begin
                        Set_Etype (Subt, Base_Type (Etype (Comp)));
                        Set_Ekind (Subt, Ekind (Etype (Comp)));
                        Set_Etype (N, Subt);
                     end;
                  end if;
               end if;

               return;
            end if;

            Next_Entity (Comp);
         end loop;
	 
         --  Ada 2005 (AI-252): The selected component can be interpreted as
         --  a prefixed view of a subprogram. Depending on the context, this is
         --  either a name that can appear in a renaming declaration, or part
         --  of an enclosing call given in prefix form.

         if Is_Tagged_Type (Prefix_Type)
         then
            if Nkind (Parent (N)) = N_Generic_Association
              or else Nkind (Parent (N)) = N_Subprogram_Renaming_Declaration
            then
               if Find_Primitive_Operation (N) then
                  return;
               end if;

            elsif Try_Object_Operation (N) then
               return;
            end if;
         end if;

	 
      elsif Is_Private_Type (Prefix_Type) then

         --  Allow access only to discriminants of the type. If the
         --  type has no full view, gigi uses the parent type for
         --  the components, so we do the same here.

         if No (Full_View (Prefix_Type)) then
            Entity_List := Root_Type (Base_Type (Prefix_Type));
            Comp := First_Entity (Entity_List);
         end if;

         while Present (Comp) loop

            if Chars (Comp) = Chars (Sel) then
               --  Before declaring an error, check whether this is tagged
               --  private type and a call to a primitive operation.

	       if Is_Tagged_Type (Prefix_Type)
                 and then Try_Object_Operation (N)
               then
                  return;
	       else
		  Error_Msg_NE
		    ("invisible selector for }",
                  N, First_Subtype (Prefix_Type));
		  Set_Entity (Sel, Any_Id);
		  Set_Etype (N, Any_Type);
	       end if;
               return;
            end if;

            Next_Entity (Comp);
         end loop;

      else
         --  Invalid prefix

         Error_Msg_NE ("invalid prefix in selected component&", N, Sel);
      end if;

      --  If N still has no type, the component is not defined in the prefix.

      if Etype (N) = Any_Type then

            --  If this is a derived formal type, the parent may have a
            --  different visibility at this point. Try for an inherited
            --  component before reporting an error.

         if Is_Generic_Type (Prefix_Type)
           and then Ekind (Prefix_Type) = E_Record_Type_With_Private
           and then Prefix_Type /= Etype (Prefix_Type)
           and then Is_Record_Type (Etype (Prefix_Type))
         then
            Set_Etype (Prefix (N), Etype (Prefix_Type));
            Analyze_Selected_Component (N);
            return;

         elsif Ekind (Prefix_Type) = E_Record_Subtype_With_Private
           and then Is_Generic_Actual_Type (Prefix_Type)
           and then Present (Full_View (Prefix_Type))
         then
            --  Similarly, if this the actual for a formal derived type,
            --  the component inherited from the generic parent may not
            --  be visible in the actual, but the selected component is
            --  legal.

            declare
               Comp : Entity_Id;
            begin
               Comp :=
                 First_Component (Generic_Parent_Type (Parent (Prefix_Type)));

               while Present (Comp) loop
                  if Chars (Comp) = Chars (Sel) then
                     Set_Entity_With_Style_Check (Sel, Comp);
                     Set_Etype (Sel, Etype (Comp));
                     Set_Etype (N,   Etype (Comp));
                     exit;
                  end if;

                  Next_Component (Comp);
               end loop;

               pragma Assert (Etype (N) /= Any_Type);
            end;

         else
            if Ekind (Prefix_Type) = E_Record_Subtype then

               --  Check whether this is a component of the base type
               --  which is absent from a statically constrained subtype.
               --  This will raise constraint error at run-time, but is
               --  not a compile-time error. When the selector is illegal
               --  for base type as well fall through and generate a
               --  compilation error anyway.

               Comp := First_Component (Base_Type (Prefix_Type));

               while Present (Comp) loop

                  if Chars (Comp) = Chars (Sel)
                    and then Is_Visible_Component (Comp)
                  then
                     Set_Entity_With_Style_Check (Sel, Comp);
                     Generate_Reference (Comp, Sel);
                     Set_Etype (Sel, Etype (Comp));
                     Set_Etype (N,   Etype (Comp));

                     --  Emit appropriate message. Gigi will replace the
                     --  node subsequently with the appropriate Raise.

--                       Apply_Compile_Time_Constraint_Error
--                         (N, "component not present in }?",
--                          CE_Discriminant_Check_Failed,
--                          Ent => Prefix_Type, Rep => False);
                     Set_Raises_Constraint_Error (N);
                     return;
                  end if;

                  Next_Component (Comp);
               end loop;

            end if;

            Error_Msg_Node_2 := First_Subtype (Prefix_Type);
            Error_Msg_NE ("no selector& for}", N, Sel);

            Check_Misspelled_Selector (Entity_List, Sel);

         end if;

         Set_Entity (Sel, Any_Id);
         Set_Etype (Sel, Any_Type);
      end if;
   end Analyze_Selected_Component;

   ---------------------------
   -- Analyze_Short_Circuit --
   ---------------------------

   procedure Analyze_Short_Circuit (N : Node_Id) is
      L   : constant Node_Id := Left_Opnd  (N);
      R   : constant Node_Id := Right_Opnd (N);
      Ind : Interp_Index;
      It  : Interp;

   begin
      Analyze_Expression (L);
      Analyze_Expression (R);
      Set_Etype (N, Any_Type);

      if not Is_Overloaded (L) then

         if Root_Type (Etype (L)) = Standard_Boolean
           and then Has_Compatible_Type (R, Etype (L))
         then
            Add_One_Interp (N, Etype (L), Etype (L));
         end if;

      else
         Get_First_Interp (L, Ind, It);

         while Present (It.Typ) loop
            if Root_Type (It.Typ) = Standard_Boolean
              and then Has_Compatible_Type (R, It.Typ)
            then
               Add_One_Interp (N, It.Typ, It.Typ);
            end if;

            Get_Next_Interp (Ind, It);
         end loop;
      end if;

      --  Here we have failed to find an interpretation. Clearly we
      --  know that it is not the case that both operands can have
      --  an interpretation of Boolean, but this is by far the most
      --  likely intended interpretation. So we simply resolve both
      --  operands as Booleans, and at least one of these resolutions
      --  will generate an error message, and we do not need to give
      --  a further error message on the short circuit operation itself.

      if Etype (N) = Any_Type then
         Resolve (L, Standard_Boolean);
         Resolve (R, Standard_Boolean);
         Set_Etype (N, Standard_Boolean);
      end if;
   end Analyze_Short_Circuit;

   -------------------
   -- Analyze_Slice --
   -------------------

   procedure Analyze_Slice (N : Node_Id) is
      P          : constant Node_Id := Prefix (N);
      D          : constant Node_Id := Discrete_Range (N);
      Array_Type : Entity_Id;

      procedure Analyze_Overloaded_Slice;
      --  If the prefix is overloaded, select those interpretations that
      --  yield a one-dimensional array type.

      procedure Analyze_Overloaded_Slice is
         I   : Interp_Index;
         It  : Interp;
         Typ : Entity_Id;

      begin
         Set_Etype (N, Any_Type);
         Get_First_Interp (P, I, It);

         while Present (It.Nam) loop
            Typ := It.Typ;

            if Is_Access_Type (Typ) then
               Typ := Designated_Type (Typ);
               Error_Msg_NW (Warn_On_Dereference, "?implicit dereference", N);
            end if;

            if Is_Array_Type (Typ)
              and then Number_Dimensions (Typ) = 1
              and then Has_Compatible_Type (D, Etype (First_Index (Typ)))
            then
               Add_One_Interp (N, Typ, Typ);
            end if;

            Get_Next_Interp (I, It);
         end loop;

         if Etype (N) = Any_Type then
            Error_Msg_N ("expect array type in prefix of slice",  N);
         end if;
      end Analyze_Overloaded_Slice;

   --  Start of processing for Analyze_Slice

   begin
      --  Analyze the prefix if not done already

      if No (Etype (P)) then
         Analyze (P);
      end if;

      Analyze (D);

      if Is_Overloaded (P) then
         Analyze_Overloaded_Slice;

      else
         Array_Type := Etype (P);
         Set_Etype (N, Any_Type);

         if Is_Access_Type (Array_Type) then
            Array_Type := Designated_Type (Array_Type);
            Error_Msg_NW (Warn_On_Dereference, "?implicit dereference", N);
         end if;

         if not Is_Array_Type (Array_Type) then
            Wrong_Type (P, Any_Array);

         elsif Number_Dimensions (Array_Type) > 1 then
            Error_Msg_N
              ("type is not one-dimensional array in slice prefix", N);

         elsif not
           Has_Compatible_Type (D, Etype (First_Index (Array_Type)))
         then
            Wrong_Type (D, Etype (First_Index (Array_Type)));

         else
            Set_Etype (N, Array_Type);
         end if;
      end if;
   end Analyze_Slice;

   -----------------------------
   -- Analyze_Type_Conversion --
   -----------------------------

   procedure Analyze_Type_Conversion (N : Node_Id) is
      Expr : constant Node_Id := Expression (N);
      T    : Entity_Id;

   begin
      --  If Conversion_OK is set, then the Etype is already set, and the
      --  only processing required is to analyze the expression. This is
      --  used to construct certain "illegal" conversions which are not
      --  allowed by Ada semantics, but can be handled OK by Gigi, see
      --  Sinfo for further details.

      if Conversion_OK (N) then
         Analyze (Expr);
         return;
      end if;

      --  Otherwise full type analysis is required, as well as some semantic
      --  checks to make sure the argument of the conversion is appropriate.

      Find_Type (Subtype_Mark (N));
      T := Entity (Subtype_Mark (N));
      Set_Etype (N, T);
      Check_Fully_Declared (T, N);
      Analyze_Expression (Expr);

      --  Only remaining step is validity checks on the argument. These
      --  are skipped if the conversion does not come from the source.

      if not Comes_From_Source (N) then
         return;

      elsif Nkind (Expr) = N_Null then
         Error_Msg_N ("argument of conversion cannot be null", N);
         Error_Msg_N ("\use qualified expression instead", N);
         Set_Etype (N, Any_Type);

      elsif Nkind (Expr) = N_Aggregate then
         Error_Msg_N ("argument of conversion cannot be aggregate", N);
         Error_Msg_N ("\use qualified expression instead", N);

      elsif Nkind (Expr) = N_Allocator then
         Error_Msg_N ("argument of conversion cannot be an allocator", N);
         Error_Msg_N ("\use qualified expression instead", N);

      elsif Nkind (Expr) = N_String_Literal then
         Error_Msg_N ("argument of conversion cannot be string literal", N);
         Error_Msg_N ("\use qualified expression instead", N);

      elsif Nkind (Expr) = N_Character_Literal then
         if Ada_83 then
            Resolve (Expr, T);
         else
            Error_Msg_N ("argument of conversion cannot be character literal",
              N);
            Error_Msg_N ("\use qualified expression instead", N);
         end if;

      elsif Nkind (Expr) = N_Attribute_Reference
        and then
          (Attribute_Name (Expr) = Name_Access            or else
           Attribute_Name (Expr) = Name_Unchecked_Access  or else
           Attribute_Name (Expr) = Name_Unrestricted_Access)
      then
         Error_Msg_N ("argument of conversion cannot be access", N);
         Error_Msg_N ("\use qualified expression instead", N);
      end if;

   end Analyze_Type_Conversion;

   ----------------------
   -- Analyze_Unary_Op --
   ----------------------

   procedure Analyze_Unary_Op (N : Node_Id) is
      R     : constant Node_Id := Right_Opnd (N);
      Op_Id : Entity_Id := Entity (N);

   begin
      Set_Etype (N, Any_Type);
      Candidate_Type := Empty;

      Analyze_Expression (R);

      if Present (Op_Id) then
         if Ekind (Op_Id) = E_Operator then
            Find_Unary_Types (R, Op_Id,  N);
         else
            Add_One_Interp (N, Op_Id, Etype (Op_Id));
         end if;

      else
         Op_Id := Get_Name_Entity_Id (Chars (N));

         while Present (Op_Id) loop

            if Ekind (Op_Id) = E_Operator then
               if No (Next_Entity (First_Entity (Op_Id))) then
                  Find_Unary_Types (R, Op_Id,  N);
               end if;
            end if;

            Op_Id := Homonym (Op_Id);
         end loop;
      end if;

      Operator_Check (N);
   end Analyze_Unary_Op;

   ----------------------------------
   -- Analyze_Unchecked_Expression --
   ----------------------------------

   procedure Analyze_Unchecked_Expression (N : Node_Id) is
   begin
      Analyze (Expression (N), Suppress => All_Checks);
      Set_Etype (N, Etype (Expression (N)));
      Save_Interps (Expression (N), N);
   end Analyze_Unchecked_Expression;

   ---------------------------------------
   -- Analyze_Unchecked_Type_Conversion --
   ---------------------------------------

   procedure Analyze_Unchecked_Type_Conversion (N : Node_Id) is
   begin
      Find_Type (Subtype_Mark (N));
      Analyze_Expression (Expression (N));
      Set_Etype (N, Entity (Subtype_Mark (N)));
   end Analyze_Unchecked_Type_Conversion;

   ---------------------------
   -- Check_Arithmetic_Pair --
   ---------------------------

   procedure Check_Arithmetic_Pair
     (T1, T2 : Entity_Id;
      Op_Id  : Entity_Id;
      N      : Node_Id)
   is
      Op_Name : constant Name_Id   := Chars (Op_Id);

      function Specific_Type (T1, T2 : Entity_Id) return Entity_Id;
      --  Get specific type (i.e. non-universal type if there is one)

      function Specific_Type (T1, T2 : Entity_Id) return Entity_Id is
      begin
         if T1 = Universal_Integer or else T1 = Universal_Real then
            return Base_Type (T2);
         else
            return Base_Type (T1);
         end if;
      end Specific_Type;

   --  Start of processing for Check_Arithmetic_Pair

   begin
      if Op_Name = Name_Op_Add or else Op_Name = Name_Op_Subtract then

         if Is_Numeric_Type (T1)
           and then Is_Numeric_Type (T2)
           and then (Covers (T1, T2) or else Covers (T2, T1))
         then
            Add_One_Interp (N, Op_Id, Specific_Type (T1, T2));
         end if;

      elsif Op_Name = Name_Op_Multiply or else Op_Name = Name_Op_Divide then

         if Is_Numeric_Type (T1)
           and then Is_Numeric_Type (T2)
           and then (Covers (T1, T2) or else Covers (T2, T1))
         then
            Add_One_Interp (N, Op_Id, Specific_Type (T1, T2));

         elsif T2 = Universal_Real
           and then Base_Type (T1) = Base_Type (Standard_Integer)
           and then Op_Name = Name_Op_Multiply
         then
            Add_One_Interp (N, Op_Id, Any_Fixed);

         elsif T1 = Universal_Real
           and then Base_Type (T2) = Base_Type (Standard_Integer)
         then
            Add_One_Interp (N, Op_Id, Any_Fixed);

         elsif T1 = Universal_Real and then T2 = Universal_Integer then
            Add_One_Interp (N, Op_Id, T1);

         elsif T2 = Universal_Real
           and then T1 = Universal_Integer
           and then Op_Name = Name_Op_Multiply
         then
            Add_One_Interp (N, Op_Id, T2);
         end if;

      elsif Op_Name = Name_Op_Mod or else Op_Name = Name_Op_Rem then

         --  Note: The fixed-point operands case with Treat_Fixed_As_Integer
         --  set does not require any special processing, since the Etype is
         --  already set (case of operation constructed by Exp_Fixed).

         if Is_Integer_Type (T1)
           and then (Covers (T1, T2) or else Covers (T2, T1))
         then
            Add_One_Interp (N, Op_Id, Specific_Type (T1, T2));
         end if;

      elsif Op_Name = Name_Op_Expon then

         if Is_Numeric_Type (T1)
           and then (Base_Type (T2) = Base_Type (Standard_Integer)
                      or else T2 = Universal_Integer)
         then
            Add_One_Interp (N, Op_Id, Base_Type (T1));
         end if;

      else pragma Assert (Nkind (N) in N_Op_Shift);

         --  If not one of the predefined operators, the node may be one
         --  of the intrinsic functions. Its kind is always specific, and
         --  we can use it directly, rather than the name of the operation.

         if Is_Integer_Type (T1)
           and then (Base_Type (T2) = Base_Type (Standard_Integer)
                      or else T2 = Universal_Integer)
         then
            Add_One_Interp (N, Op_Id, Base_Type (T1));
         end if;
      end if;
   end Check_Arithmetic_Pair;

   -------------------------------
   -- Check_Misspelled_Selector --
   -------------------------------

   procedure Check_Misspelled_Selector
     (Prefix : Entity_Id;
      Sel    : Node_Id)
   is
      Max_Suggestions   : constant := 2;
      Nr_Of_Suggestions : Natural := 0;

      Suggestion_1 : Entity_Id := Empty;
      Suggestion_2 : Entity_Id := Empty;

      Comp : Entity_Id;

   begin
      --  All the components of the prefix of selector Sel are matched
      --  against  Sel and a count is maintained of possible misspellings.
      --  When at the end of the analysis there are one or two (not more!)
      --  possible misspellings, these misspellings will be suggested as
      --  possible correction.

      if not (Is_Private_Type (Prefix) or Is_Record_Type (Prefix)) then
         return;
      end if;

      Get_Name_String (Chars (Sel));

      declare
         S  : constant String (1 .. Name_Len) :=
                Name_Buffer (1 .. Name_Len);

      begin
         Comp  := First_Entity (Prefix);

         while Nr_Of_Suggestions <= Max_Suggestions
            and then Present (Comp)
         loop

            if Is_Visible_Component (Comp) then
               Get_Name_String (Chars (Comp));

               if Is_Bad_Spelling_Of (Name_Buffer (1 .. Name_Len), S) then
                  Nr_Of_Suggestions := Nr_Of_Suggestions + 1;

                  case Nr_Of_Suggestions is
                     when 1      => Suggestion_1 := Comp;
                     when 2      => Suggestion_2 := Comp;
                     when others => exit;
                  end case;
               end if;
            end if;

            Comp := Next_Entity (Comp);
         end loop;

         --  Report at most two suggestions

         if Nr_Of_Suggestions = 1 then
            Error_Msg_NE ("\possible misspelling of&", Sel, Suggestion_1);

         elsif Nr_Of_Suggestions = 2 then
            Error_Msg_Node_2 := Suggestion_2;
            Error_Msg_NE ("\possible misspelling of& or&",
              Sel, Suggestion_1);
         end if;
      end;
   end Check_Misspelled_Selector;

   ----------------------
   -- Defined_In_Scope --
   ----------------------

   function Defined_In_Scope (T : Entity_Id; S : Entity_Id) return Boolean
   is
      S1 : constant Entity_Id := Scope (Base_Type (T));

   begin
      return S1 = S
        or else (S1 = System_Aux_Id and then S = Scope (S1));
   end Defined_In_Scope;

   -------------------
   -- Diagnose_Call --
   -------------------

   procedure Diagnose_Call (N : Node_Id; Nam : Node_Id) is
      X                : Interp_Index;
      It               : Interp;
      Success          : Boolean;
      Err_Mode         : Boolean;
      New_Nam          : Node_Id;
      Void_Interp_Seen : Boolean := False;

   begin
      --   Analyze each candidate call again, with full error reporting
      --   for each.

      Error_Msg_N
        ("no candidate interpretations match the actuals:!", Nam);
      Err_Mode := All_Errors_Mode;
      All_Errors_Mode := True;

      --  If this is a call to an operation of a concurrent type,
      --  the failed interpretations have been removed from the
      --  name. Recover them to provide full diagnostics.

      if Nkind (Parent (Nam)) = N_Selected_Component then
         Set_Entity (Nam, Empty);
         New_Nam := New_Copy_Tree (Parent (Nam));
         Set_Is_Overloaded (New_Nam, False);
         Set_Is_Overloaded (Selector_Name (New_Nam), False);
         Set_Parent (New_Nam, Parent (Parent (Nam)));
         Analyze_Selected_Component (New_Nam);
         Get_First_Interp (Selector_Name (New_Nam), X, It);
      else
         Get_First_Interp (Nam, X, It);
      end if;

      while Present (It.Nam) loop
         if Etype (It.Nam) = Standard_Void_Type then
            Void_Interp_Seen := True;
         end if;

         Analyze_One_Call (N, It.Nam, True, Success);
         Get_Next_Interp (X, It);
      end loop;

      if Nkind (N) = N_Function_Call then
         Get_First_Interp (Nam, X, It);

         while Present (It.Nam) loop
            if Ekind (It.Nam) = E_Function
              or else Ekind (It.Nam) = E_Operator
            then
               return;
            else
               Get_Next_Interp (X, It);
            end if;
         end loop;

         --  If all interpretations are procedures, this deserves a
         --  more precise message. Ditto if this appears as the prefix
         --  of a selected component, which may be a lexical error.

         Error_Msg_N (
         "\context requires function call, found procedure name", Nam);

         if Nkind (Parent (N)) = N_Selected_Component
           and then N = Prefix (Parent (N))
         then
            Error_Msg_N (
              "\period should probably be semicolon", Parent (N));
         end if;

      elsif Nkind (N) = N_Procedure_Call_Statement
        and then not Void_Interp_Seen
      then
         Error_Msg_N (
         "\function name found in procedure call", Nam);
      end if;

      All_Errors_Mode := Err_Mode;
   end Diagnose_Call;

   ---------------------------
   -- Find_Arithmetic_Types --
   ---------------------------

   procedure Find_Arithmetic_Types
     (L, R  : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id)
   is
      Index1, Index2 : Interp_Index;
      It1, It2 : Interp;

      procedure Check_Right_Argument (T : Entity_Id);
      --  Check right operand of operator

      procedure Check_Right_Argument (T : Entity_Id) is
      begin
         if not Is_Overloaded (R) then
            Check_Arithmetic_Pair (T, Etype (R), Op_Id,  N);
         else
            Get_First_Interp (R, Index2, It2);

            while Present (It2.Typ) loop
               Check_Arithmetic_Pair (T, It2.Typ, Op_Id, N);
               Get_Next_Interp (Index2, It2);
            end loop;
         end if;
      end Check_Right_Argument;

   --  Start processing for Find_Arithmetic_Types

   begin
      if not Is_Overloaded (L) then
         Check_Right_Argument (Etype (L));

      else
         Get_First_Interp (L, Index1, It1);

         while Present (It1.Typ) loop
            Check_Right_Argument (It1.Typ);
            Get_Next_Interp (Index1, It1);
         end loop;
      end if;

   end Find_Arithmetic_Types;

   ------------------------
   -- Find_Boolean_Types --
   ------------------------

   procedure Find_Boolean_Types
     (L, R  : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id)
   is
      Index : Interp_Index;
      It    : Interp;

      procedure Check_Numeric_Argument (T : Entity_Id);
      --  Special case for logical operations one of whose operands is an
      --  integer literal. If both are literal the result is any modular type.

      procedure Check_Numeric_Argument (T : Entity_Id) is
      begin
         if T = Universal_Integer then
            Add_One_Interp (N, Op_Id, Any_Modular);

         elsif Is_Modular_Integer_Type (T) then
            Add_One_Interp (N, Op_Id, T);
         end if;
      end Check_Numeric_Argument;

   --  Start of processing for Find_Boolean_Types

   begin
      if not Is_Overloaded (L) then

         if Etype (L) = Universal_Integer
           or else Etype (L) = Any_Modular
         then
            if not Is_Overloaded (R) then
               Check_Numeric_Argument (Etype (R));

            else
               Get_First_Interp (R, Index, It);

               while Present (It.Typ) loop
                  Check_Numeric_Argument (It.Typ);

                  Get_Next_Interp (Index, It);
               end loop;
            end if;

         elsif Valid_Boolean_Arg (Etype (L))
           and then Has_Compatible_Type (R, Etype (L))
         then
            Add_One_Interp (N, Op_Id, Etype (L));
         end if;

      else
         Get_First_Interp (L, Index, It);

         while Present (It.Typ) loop
            if Valid_Boolean_Arg (It.Typ)
              and then Has_Compatible_Type (R, It.Typ)
            then
               Add_One_Interp (N, Op_Id, It.Typ);
            end if;

            Get_Next_Interp (Index, It);
         end loop;
      end if;
   end Find_Boolean_Types;

   ---------------------------
   -- Find_Comparison_Types --
   ---------------------------

   procedure Find_Comparison_Types
     (L, R  : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id)
   is
      Index : Interp_Index;
      It    : Interp;
      Found : Boolean := False;
      I_F   : Interp_Index;
      T_F   : Entity_Id;
      Scop  : Entity_Id := Empty;

      procedure Try_One_Interp (T1 : Entity_Id);
      --  Routine to try one proposed interpretation. Note that the context
      --  of the operator plays no role in resolving the arguments, so that
      --  if there is more than one interpretation of the operands that is
      --  compatible with comparison, the operation is ambiguous.

      procedure Try_One_Interp (T1 : Entity_Id) is
      begin

         --  If the operator is an expanded name, then the type of the operand
         --  must be defined in the corresponding scope. If the type is
         --  universal, the context will impose the correct type.

         if Present (Scop)
            and then not Defined_In_Scope (T1, Scop)
            and then T1 /= Universal_Integer
            and then T1 /= Universal_Real
            and then T1 /= Any_String
            and then T1 /= Any_Composite
         then
            return;
         end if;

         if Valid_Comparison_Arg (T1)
           and then Has_Compatible_Type (R, T1)
         then
            if Found
              and then Base_Type (T1) /= Base_Type (T_F)
            then
               It := Disambiguate (L, I_F, Index, Any_Type);

               if It = No_Interp then
                  Ambiguous_Operands (N);
                  Set_Etype (L, Any_Type);
                  return;

               else
                  T_F := It.Typ;
               end if;

            else
               Found := True;
               T_F   := T1;
               I_F   := Index;
            end if;

            Set_Etype (L, T_F);
            Find_Non_Universal_Interpretations (N, R, Op_Id, T1);

         end if;
      end Try_One_Interp;

   --  Start processing for Find_Comparison_Types

   begin
      --  If left operand is aggregate, the right operand has to
      --  provide a usable type for it.

      if Nkind (L) = N_Aggregate
        and then Nkind (R) /= N_Aggregate
      then
         Find_Comparison_Types (R, L, Op_Id, N);
         return;
      end if;

      if Nkind (N) = N_Function_Call
         and then Nkind (Name (N)) = N_Expanded_Name
      then
         Scop := Entity (Prefix (Name (N)));

         --  The prefix may be a package renaming, and the subsequent test
         --  requires the original package.

         if Ekind (Scop) = E_Package
           and then Present (Renamed_Entity (Scop))
         then
            Scop := Renamed_Entity (Scop);
            Set_Entity (Prefix (Name (N)), Scop);
         end if;
      end if;

      if not Is_Overloaded (L) then
         Try_One_Interp (Etype (L));

      else
         Get_First_Interp (L, Index, It);

         while Present (It.Typ) loop
            Try_One_Interp (It.Typ);
            Get_Next_Interp (Index, It);
         end loop;
      end if;
   end Find_Comparison_Types;

   ----------------------------------------
   -- Find_Non_Universal_Interpretations --
   ----------------------------------------

   procedure Find_Non_Universal_Interpretations
     (N     : Node_Id;
      R     : Node_Id;
      Op_Id : Entity_Id;
      T1    : Entity_Id)
   is
      Index : Interp_Index;
      It   : Interp;

   begin
      if T1 = Universal_Integer
        or else T1 = Universal_Real
      then
         if not Is_Overloaded (R) then
            Add_One_Interp
              (N, Op_Id, Standard_Boolean, Base_Type (Etype (R)));
         else
            Get_First_Interp (R, Index, It);

            while Present (It.Typ) loop
               if Covers (It.Typ, T1) then
                  Add_One_Interp
                    (N, Op_Id, Standard_Boolean, Base_Type (It.Typ));
               end if;

               Get_Next_Interp (Index, It);
            end loop;
         end if;
      else
         Add_One_Interp (N, Op_Id, Standard_Boolean, Base_Type (T1));
      end if;
   end Find_Non_Universal_Interpretations;

   ------------------------------
   -- Find_Concatenation_Types --
   ------------------------------

   procedure Find_Concatenation_Types
     (L, R  : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id)
   is
      Op_Type : constant Entity_Id := Etype (Op_Id);

   begin
      if Is_Array_Type (Op_Type)

        and then (Has_Compatible_Type (L, Op_Type)
                    or else
                  Has_Compatible_Type (L, Component_Type (Op_Type)))

        and then (Has_Compatible_Type (R, Op_Type)
                    or else
                  Has_Compatible_Type (R, Component_Type (Op_Type)))
      then
         Add_One_Interp (N, Op_Id, Op_Type);
      end if;
   end Find_Concatenation_Types;

   -------------------------
   -- Find_Equality_Types --
   -------------------------

   procedure Find_Equality_Types
     (L, R  : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id)
   is
      Index : Interp_Index;
      It    : Interp;
      Found : Boolean := False;
      I_F   : Interp_Index;
      T_F   : Entity_Id;
      Scop  : Entity_Id := Empty;

      procedure Try_One_Interp (T1 : Entity_Id);
      --  The context of the operator plays no role in resolving the
      --  arguments,  so that if there is more than one interpretation
      --  of the operands that is compatible with equality, the construct
      --  is ambiguous and an error can be emitted now, after trying to
      --  disambiguate, i.e. applying preference rules.

      procedure Try_One_Interp (T1 : Entity_Id) is
      begin

         --  If the operator is an expanded name, then the type of the operand
         --  must be defined in the corresponding scope. If the type is
         --  universal, the context will impose the correct type. An anonymous
         --  type for a 'Access reference is also universal in this sense, as
         --  the actual type is obtained from context.

         if Present (Scop)
            and then not Defined_In_Scope (T1, Scop)
            and then T1 /= Universal_Integer
            and then T1 /= Universal_Real
            and then T1 /= Any_Access
            and then T1 /= Any_String
            and then T1 /= Any_Composite
            and then (Ekind (T1) /= E_Access_Subprogram_Type
                        or else Comes_From_Source (T1))
         then
            return;
         end if;

         if T1 /= Standard_Void_Type
           and then Ekind (T1) /= E_Anonymous_Access_Type
           and then Has_Compatible_Type (R, T1)
         then
            if Found
              and then Base_Type (T1) /= Base_Type (T_F)
            then
               It := Disambiguate (L, I_F, Index, Any_Type);

               if It = No_Interp then
                  Ambiguous_Operands (N);
                  Set_Etype (L, Any_Type);
                  return;

               else
                  T_F := It.Typ;
               end if;

            else
               Found := True;
               T_F   := T1;
               I_F   := Index;
            end if;

            if not Analyzed (L) then
               Set_Etype (L, T_F);
            end if;

            Find_Non_Universal_Interpretations (N, R, Op_Id, T1);

            if Etype (N) = Any_Type then

               --  Operator was not visible.

               Found := False;
            end if;
         end if;
      end Try_One_Interp;

   --  Start of processing for Find_Equality_Types

   begin
      --  If left operand is aggregate, the right operand has to
      --  provide a usable type for it.

      if Nkind (L) = N_Aggregate
        and then Nkind (R) /= N_Aggregate
      then
         Find_Equality_Types (R, L, Op_Id, N);
         return;
      end if;

      if Nkind (N) = N_Function_Call
         and then Nkind (Name (N)) = N_Expanded_Name
      then
         Scop := Entity (Prefix (Name (N)));

         --  The prefix may be a package renaming, and the subsequent test
         --  requires the original package.

         if Ekind (Scop) = E_Package
           and then Present (Renamed_Entity (Scop))
         then
            Scop := Renamed_Entity (Scop);
            Set_Entity (Prefix (Name (N)), Scop);
         end if;
      end if;

      if not Is_Overloaded (L) then
         Try_One_Interp (Etype (L));
      else

         Get_First_Interp (L, Index, It);

         while Present (It.Typ) loop
            Try_One_Interp (It.Typ);
            Get_Next_Interp (Index, It);
         end loop;
      end if;
   end Find_Equality_Types;

   -------------------------
   -- Find_Negation_Types --
   -------------------------

   procedure Find_Negation_Types
     (R     : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id)
   is
      Index : Interp_Index;
      It    : Interp;

   begin
      if not Is_Overloaded (R) then

         if Etype (R) = Universal_Integer then
            Add_One_Interp (N, Op_Id, Any_Modular);

         elsif Valid_Boolean_Arg (Etype (R)) then
            Add_One_Interp (N, Op_Id, Etype (R));
         end if;

      else
         Get_First_Interp (R, Index, It);

         while Present (It.Typ) loop
            if Valid_Boolean_Arg (It.Typ) then
               Add_One_Interp (N, Op_Id, It.Typ);
            end if;

            Get_Next_Interp (Index, It);
         end loop;
      end if;
   end Find_Negation_Types;

   ------------------------------
   -- Find_Primitive_Operation --
   ------------------------------

   function Find_Primitive_Operation (N : Node_Id) return Boolean is
      Obj : constant Node_Id := Prefix (N);
      Op  : constant Node_Id := Selector_Name (N);

      Prim  : Elmt_Id;
      Prims : Elist_Id;
      Typ   : Entity_Id;

   begin
      Set_Etype (Op, Any_Type);

      if Is_Access_Type (Etype (Obj)) then
         Typ := Designated_Type (Etype (Obj));
      else
         Typ := Etype (Obj);
      end if;

      if Is_Class_Wide_Type (Typ) then
         Typ := Root_Type (Typ);
      end if;

      Prims := Primitive_Operations (Typ);

      Prim := First_Elmt (Prims);
      while Present (Prim) loop
         if Chars (Node (Prim)) = Chars (Op) then
            Add_One_Interp (Op, Node (Prim), Etype (Node (Prim)));
            Set_Etype (N, Etype (Node (Prim)));
         end if;

         Next_Elmt (Prim);
      end loop;

      --  Now look for class-wide operations of the type or any of its
      --  ancestors by iterating over the homonyms of the selector.

      declare
         Cls_Type : constant Entity_Id := Class_Wide_Type (Typ);
         Hom      : Entity_Id;

      begin
         Hom := Current_Entity (Op);
         while Present (Hom) loop
            if (Ekind (Hom) = E_Procedure
                  or else
                Ekind (Hom) = E_Function)
              and then Scope (Hom) = Scope (Typ)
              and then Present (First_Formal (Hom))
              and then
                (Base_Type (Etype (First_Formal (Hom))) = Cls_Type
                  or else
                    (Is_Access_Type (Etype (First_Formal (Hom)))
                      and then
                        Ekind (Etype (First_Formal (Hom))) =
                          E_Anonymous_Access_Type
                      and then
                        Base_Type
                          (Designated_Type (Etype (First_Formal (Hom)))) =
                                                                Cls_Type))
            then
               Add_One_Interp (Op, Hom, Etype (Hom));
               Set_Etype (N, Etype (Hom));
            end if;

            Hom := Homonym (Hom);
         end loop;
      end;

      return Etype (Op) /= Any_Type;
   end Find_Primitive_Operation;

   ----------------------
   -- Find_Unary_Types --
   ----------------------

   procedure Find_Unary_Types
     (R     : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id)
   is
      Index : Interp_Index;
      It    : Interp;

   begin
      if not Is_Overloaded (R) then
         if Is_Numeric_Type (Etype (R)) then
            Add_One_Interp (N, Op_Id, Base_Type (Etype (R)));
         end if;

      else
         Get_First_Interp (R, Index, It);

         while Present (It.Typ) loop
            if Is_Numeric_Type (It.Typ) then
               Add_One_Interp (N, Op_Id, Base_Type (It.Typ));
            end if;

            Get_Next_Interp (Index, It);
         end loop;
      end if;
   end Find_Unary_Types;

   ------------------
   -- Junk_Operand --
   ------------------

   function Junk_Operand (N : Node_Id) return Boolean is
      Enode : Node_Id;

   begin
      if Error_Posted (N) then
         return False;
      end if;

      --  Get entity to be tested

      if Is_Entity_Name (N)
        and then Present (Entity (N))
      then
         Enode := N;

      --  An odd case, a procedure name gets converted to a very peculiar
      --  function call, and here is where we detect this happening.

      elsif Nkind (N) = N_Function_Call
        and then Is_Entity_Name (Name (N))
        and then Present (Entity (Name (N)))
      then
         Enode := Name (N);

      --  Another odd case, there are at least some cases of selected
      --  components where the selected component is not marked as having
      --  an entity, even though the selector does have an entity

      elsif Nkind (N) = N_Selected_Component
        and then Present (Entity (Selector_Name (N)))
      then
         Enode := Selector_Name (N);

      else
         return False;
      end if;

      --  Now test the entity we got to see if it a bad case

      case Ekind (Entity (Enode)) is

         when E_Package =>
            Error_Msg_N
              ("package name cannot be used as operand", Enode);

         when Generic_Unit_Kind =>
            Error_Msg_N
              ("generic unit name cannot be used as operand", Enode);

         when Type_Kind =>
            Error_Msg_N
              ("subtype name cannot be used as operand", Enode);

         when E_Procedure =>
            Error_Msg_N
              ("procedure name cannot be used as operand", Enode);

         when E_Block | E_Label | E_Loop =>
            Error_Msg_N
              ("label name cannot be used as operand", Enode);

         when others =>
            return False;

      end case;

      return True;
   end Junk_Operand;

   --------------------
   -- Operator_Check --
   --------------------

   procedure Operator_Check (N : Node_Id) is
   begin
      --  Test for case of no interpretation found for operator

      if Etype (N) = Any_Type then
         declare
            L : Node_Id;
            R : Node_Id;

         begin
            R := Right_Opnd (N);

            if Nkind (N) in N_Binary_Op then
               L := Left_Opnd (N);
            else
               L := Empty;
            end if;

            --  If either operand has no type, then don't complain further,
            --  since this simply means that we have a propragated error.

            if R = Error
              or else Etype (R) = Any_Type
              or else (Nkind (N) in N_Binary_Op and then Etype (L) = Any_Type)
            then
               return;

            --  We explicitly check for the case of concatenation of
            --  component with component to avoid reporting spurious
            --  matching array types that might happen to be lurking
            --  in distant packages (such as run-time packages). This
            --  also prevents inconsistencies in the messages for certain
            --  ACVC B tests, which can vary depending on types declared
            --  in run-time interfaces. A further improvement, when
            --  aggregates are present, is to look for a well-typed operand.

            elsif Present (Candidate_Type)
              and then (Nkind (N) /= N_Op_Concat
                         or else Is_Array_Type (Etype (L))
                         or else Is_Array_Type (Etype (R)))
            then

               if Nkind (N) = N_Op_Concat then
                  if Etype (L) /= Any_Composite
                    and then Is_Array_Type (Etype (L))
                  then
                     Candidate_Type := Etype (L);

                  elsif Etype (R) /= Any_Composite
                    and then Is_Array_Type (Etype (R))
                  then
                     Candidate_Type := Etype (R);
                  end if;
               end if;

               Error_Msg_NE
                 ("operator for} is not directly visible!",
                  N, First_Subtype (Candidate_Type));
               Error_Msg_N ("use clause would make operation legal!",  N);
               return;

            --  If either operand is a junk operand (e.g. package name), then
            --  post appropriate error messages, but do not complain further.

            --  Note that the use of OR in this test instead of OR ELSE
            --  is quite deliberate, we may as well check both operands
            --  in the binary operator case.

            elsif Junk_Operand (R)
              or (Nkind (N) in N_Binary_Op and then Junk_Operand (L))
            then
               return;

            --  If we have a logical operator, one of whose operands is
            --  Boolean, then we know that the other operand cannot resolve
            --  to Boolean (since we got no interpretations), but in that
            --  case we pretty much know that the other operand should be
            --  Boolean, so resolve it that way (generating an error)

            elsif Nkind (N) = N_Op_And
                    or else
                  Nkind (N) = N_Op_Or
                    or else
                  Nkind (N) = N_Op_Xor
            then
               if Etype (L) = Standard_Boolean then
                  Resolve (R, Standard_Boolean);
                  return;
               elsif Etype (R) = Standard_Boolean then
                  Resolve (L, Standard_Boolean);
                  return;
               end if;

            --  For an arithmetic operator or comparison operator, if one
            --  of the operands is numeric, then we know the other operand
            --  is not the same numeric type. If it is a non-numeric type,
            --  then probably it is intended to match the other operand.

            elsif Nkind (N) = N_Op_Add      or else
                  Nkind (N) = N_Op_Divide   or else
                  Nkind (N) = N_Op_Ge       or else
                  Nkind (N) = N_Op_Gt       or else
                  Nkind (N) = N_Op_Le       or else
                  Nkind (N) = N_Op_Lt       or else
                  Nkind (N) = N_Op_Mod      or else
                  Nkind (N) = N_Op_Multiply or else
                  Nkind (N) = N_Op_Rem      or else
                  Nkind (N) = N_Op_Subtract
            then
               if Is_Numeric_Type (Etype (L))
                 and then not Is_Numeric_Type (Etype (R))
               then
                  Resolve (R, Etype (L));
                  return;

               elsif Is_Numeric_Type (Etype (R))
                 and then not Is_Numeric_Type (Etype (L))
               then
                  Resolve (L, Etype (R));
                  return;
               end if;

            --  Comparisons on A'Access are common enough to deserve a
            --  special message.

            elsif (Nkind (N) = N_Op_Eq  or else
                   Nkind (N) = N_Op_Ne)
               and then Ekind (Etype (L)) = E_Access_Attribute_Type
               and then Ekind (Etype (R)) = E_Access_Attribute_Type
            then
               Error_Msg_N
                 ("two access attributes cannot be compared directly", N);
               Error_Msg_N
                 ("\they must be converted to an explicit type for comparison",
                   N);
               return;

            --  Another one for C programmers

            elsif Nkind (N) = N_Op_Concat
              and then Valid_Boolean_Arg (Etype (L))
              and then Valid_Boolean_Arg (Etype (R))
            then
               Error_Msg_N ("invalid operands for concatenation", N);
               Error_Msg_N ("\maybe AND was meant", N);
               return;

            --  A special case for comparison of access parameter with null

            elsif Nkind (N) = N_Op_Eq
              and then Is_Entity_Name (L)
              and then Nkind (Parent (Entity (L))) = N_Parameter_Specification
              and then Nkind (Parameter_Type (Parent (Entity (L)))) =
                                                  N_Access_Definition
              and then Nkind (R) = N_Null
            then
               Error_Msg_N ("access parameter is not allowed to be null", L);
               Error_Msg_N ("\(call would raise Constraint_Error)", L);
               return;
            end if;

            --  If we fall through then just give general message. Note
            --  that in the following messages, if the operand is overloaded
            --  we choose an arbitrary type to complain about, but that is
            --  probably more useful than not giving a type at all.

            if Nkind (N) in N_Unary_Op then
               Error_Msg_Node_2 := Etype (R);
               Error_Msg_N ("operator& not defined for}", N);
               return;

            else
               if Nkind (N) in N_Binary_Op then
                  if not Is_Overloaded (L)
                    and then not Is_Overloaded (R)
                    and then Base_Type (Etype (L)) = Base_Type (Etype (R))
                  then
                     Error_Msg_Node_2 := Etype (R);
                     Error_Msg_N ("there is no applicable operator& for}", N);

                  else
                     Error_Msg_N ("invalid operand types for operator&", N);

                     if Nkind (N) /= N_Op_Concat then
                        Error_Msg_NE ("\left operand has}!",  N, Etype (L));
                        Error_Msg_NE ("\right operand has}!", N, Etype (R));
                     end if;
                  end if;
               end if;
            end if;
         end;
      end if;
   end Operator_Check;

   -----------------------
   -- Try_Indirect_Call --
   -----------------------

   function Try_Indirect_Call
     (N   : Node_Id;
      Nam : Entity_Id;
      Typ : Entity_Id) return Boolean
   is
      Actuals : constant List_Id := Parameter_Associations (N);
      Actual  : Node_Id;
      Formal  : Entity_Id;

   begin
      Actual := First (Actuals);
      Formal := First_Formal (Designated_Type (Typ));
      while Present (Actual)
        and then Present (Formal)
      loop
         if not Has_Compatible_Type (Actual, Etype (Formal)) then
            return False;
         end if;

         Next (Actual);
         Next_Formal (Formal);
      end loop;

      if No (Actual) and then No (Formal) then
         Add_One_Interp (N, Nam, Etype (Designated_Type (Typ)));

         --  Nam is a candidate interpretation for the name in the call,
         --  if it is not an indirect call.

         if not Is_Type (Nam)
            and then Is_Entity_Name (Name (N))
         then
            Set_Entity (Name (N), Nam);
         end if;

         return True;
      else
         return False;
      end if;
   end Try_Indirect_Call;

   ----------------------
   -- Try_Indexed_Call --
   ----------------------

   function Try_Indexed_Call
     (N          : Node_Id;
      Nam        : Entity_Id;
      Typ        : Entity_Id;
      Skip_First : Boolean) return Boolean
   is
      Actuals : constant List_Id   := Parameter_Associations (N);
      Actual : Node_Id;
      Index  : Entity_Id;

   begin
      Actual := First (Actuals);
      
      --  If the call was originally written in prefix form, skip the first
      --  actual, which is obviously not defaulted.

      if Skip_First then
         Next (Actual);
      end if;

      Index := First_Index (Typ);
      while Present (Actual)
        and then Present (Index)
      loop
         --  If the parameter list has a named association, the expression
         --  is definitely a call and not an indexed component.

         if Nkind (Actual) = N_Parameter_Association then
            return False;
         end if;

         if not Has_Compatible_Type (Actual, Etype (Index)) then
            return False;
         end if;

         Next (Actual);
         Next_Index (Index);
      end loop;

      if No (Actual) and then No (Index) then
         Add_One_Interp (N, Nam, Component_Type (Typ));

         --  Nam is a candidate interpretation for the name in the call,
         --  if it is not an indirect call.

         if not Is_Type (Nam)
            and then Is_Entity_Name (Name (N))
         then
            Set_Entity (Name (N), Nam);
         end if;

         return True;
      else
         return False;
      end if;

   end Try_Indexed_Call;

   --------------------------
   -- Try_Object_Operation --
   --------------------------

   function Try_Object_Operation
     (N : Node_Id; CW_Test_Only : Boolean := False) return Boolean
   is
      K              : constant Node_Kind  := Nkind (Parent (N));
      Is_Subprg_Call : constant Boolean    := K in N_Subprogram_Call;
      Loc            : constant Source_Ptr := Sloc (N);
      Obj            : constant Node_Id    := Prefix (N);

      Subprog : constant Node_Id    :=
                  Make_Identifier (Sloc (Selector_Name (N)),
                    Chars => Chars (Selector_Name (N)));
      --  Identifier on which possible interpretations will be collected

      Report_Error : Boolean := False;
      --  If no candidate interpretation matches the context, redo analysis
      --  with Report_Error True to provide additional information.

      Actual          : Node_Id;
      Candidate       : Entity_Id := Empty;
      New_Call_Node   : Node_Id := Empty;
      Node_To_Replace : Node_Id;
      Obj_Type        : Entity_Id := Etype (Obj);
      Success         : Boolean := False;

      function Valid_Candidate
        (Success : Boolean;
         Call    : Node_Id;
         Subp    : Entity_Id) return Entity_Id;
      --  If the subprogram is a valid interpretation, record it, and add
      --  to the list of interpretations of Subprog. Otherwise return Empty.

      procedure Complete_Object_Operation
        (Call_Node       : Node_Id;
         Node_To_Replace : Node_Id);
      --  Make Subprog the name of Call_Node, replace Node_To_Replace with
      --  Call_Node, insert the object (or its dereference) as the first actual
      --  in the call, and complete the analysis of the call.

      procedure Report_Ambiguity (Op : Entity_Id);
      --  If a prefixed procedure call is ambiguous, indicate whether the
      --  call includes an implicit dereference or an implicit 'Access.

      procedure Transform_Object_Operation
        (Call_Node       : out Node_Id;
         Node_To_Replace : out Node_Id);
      --  Transform Obj.Operation (X, Y,,) into Operation (Obj, X, Y ..)
      --  Call_Node is the resulting subprogram call, Node_To_Replace is
      --  either N or the parent of N, and Subprog is a reference to the
      --  subprogram we are trying to match.

      function Try_Class_Wide_Operation
        (Call_Node       : Node_Id;
         Node_To_Replace : Node_Id) return Boolean;
      --  Traverse all ancestor types looking for a class-wide subprogram
      --  for which the current operation is a valid non-dispatching call.

      procedure Try_One_Prefix_Interpretation (T : Entity_Id);
      --  If prefix is overloaded, its interpretation may include different
      --  tagged types, and we must examine the primitive operations and
      --  the class-wide operations of each in order to find candidate
      --  interpretations for the call as a whole.

      function Try_Primitive_Operation
        (Call_Node       : Node_Id;
         Node_To_Replace : Node_Id) return Boolean;
      --  Traverse the list of primitive subprograms looking for a dispatching
      --  operation for which the current node is a valid call .

      ---------------------
      -- Valid_Candidate --
      ---------------------

      function Valid_Candidate
        (Success : Boolean;
         Call    : Node_Id;
         Subp    : Entity_Id) return Entity_Id
      is
      begin
         --  If the subprogram is a valid interpretation, record it in global
         --  variable Subprog, to collect all possible overloadings.

         if Success then
            if Subp /= Entity (Subprog) then
               Add_One_Interp (Subprog, Subp, Etype (Subp));
            end if;
         end if;

         if Etype (Call) /= Any_Type then
            return Subp;
         else
            return Empty;
         end if;
      end Valid_Candidate;

      -------------------------------
      -- Complete_Object_Operation --
      -------------------------------

      procedure Complete_Object_Operation
        (Call_Node       : Node_Id;
         Node_To_Replace : Node_Id)
      is
         Control      : constant Entity_Id := First_Formal (Entity (Subprog));
         Formal_Type  : constant Entity_Id := Etype (Control);
         First_Actual : Node_Id;

      begin
         --  Place the name of the operation, with its interpretations,
         --  on the rewritten call.

         Set_Name (Call_Node, Subprog);

         First_Actual := First (Parameter_Associations (Call_Node));

         --  For cross-reference purposes, treat the new node as being in the
         --  source if the original one is. Set entity and type, even though
         --  they may be overwritten during resolution if overloaded.

         Set_Comes_From_Source (Subprog, Comes_From_Source (N));
         Set_Comes_From_Source (Call_Node, Comes_From_Source (N));

         if Nkind (N) = N_Selected_Component
           and then not Inside_A_Generic
         then
            Set_Entity (Selector_Name (N), Entity (Subprog));
            Set_Etype  (Selector_Name (N), Etype (Entity (Subprog)));
         end if;

         --  If need be, rewrite first actual as an explicit dereference. If
         --  the call is overloaded, the rewriting can only be done once the
         --  primitive operation is identified.

         if Is_Overloaded (Subprog) then

            --  The prefix itself may be overloaded, and its interpretations
            --  must be propagated to the new actual in the call.

            if Is_Overloaded (Obj) then
               Save_Interps (Obj, First_Actual);
            end if;

            Rewrite (First_Actual, Obj);

         elsif not Is_Access_Type (Formal_Type)
           and then Is_Access_Type (Etype (Obj))
         then
            Rewrite (First_Actual,
              Make_Explicit_Dereference (Sloc (Obj), Obj));
            Analyze (First_Actual);

            --  If we need to introduce an explicit dereference, verify that
            --  the resulting actual is compatible with the mode of the formal.

            if Ekind (First_Formal (Entity (Subprog))) /= E_In_Parameter
              and then Is_Access_Constant (Etype (Obj))
            then
               Error_Msg_NE
                 ("expect variable in call to&", Prefix (N), Entity (Subprog));
            end if;

         --  Conversely, if the formal is an access parameter and the object
         --  is not, replace the actual with a 'Access reference. Its analysis
         --  will check that the object is aliased.

         elsif Is_Access_Type (Formal_Type)
           and then not Is_Access_Type (Etype (Obj))
         then
            --  A special case: A.all'access is illegal if A is an access to a
            --  constant and the context requires an access to a variable.

            if not Is_Access_Constant (Formal_Type) then
               if (Nkind (Obj) = N_Explicit_Dereference
                    and then Is_Access_Constant (Etype (Prefix (Obj))))
                 or else not Is_Variable (Obj)
               then
                  Error_Msg_NE
                    ("actual for & must be a variable", Obj, Control);
               end if;
            end if;

            Rewrite (First_Actual,
              Make_Attribute_Reference (Loc,
                Attribute_Name => Name_Access,
                Prefix => Relocate_Node (Obj)));

            if not Is_Aliased_View (Obj) then
               Error_Msg_NE
                 ("object in prefixed call to & must be aliased "
                  & "(RM 4.1.3 (13 1/2))", Prefix (First_Actual), Subprog);
            end if;

            Analyze (First_Actual);

         else
            if Is_Overloaded (Obj) then
               Save_Interps (Obj, First_Actual);
            end if;

            Rewrite (First_Actual, Obj);
         end if;

         --  The operation is obtained from the dispatch table and not by
         --  visibility, and may be declared in a unit that is not explicitly
         --  referenced in the source, but is nevertheless required in the
         --  context of the current unit. Indicate that operation and its scope
         --  are referenced, to prevent spurious and misleading warnings. If
         --  the operation is overloaded, all primitives are in the same scope
         --  and we can use any of them.

         Set_Referenced (Entity (Subprog), True);
         Set_Referenced (Scope (Entity (Subprog)), True);

         Rewrite (Node_To_Replace, Call_Node);

         --  Propagate the interpretations collected in subprog to the new
         --  function call node, to be resolved from context.

         if Is_Overloaded (Subprog) then
            Save_Interps (Subprog, Node_To_Replace);

         else

            Analyze (Node_To_Replace);

            --  If the operation has been rewritten into a call, which may get
            --  subsequently an explicit dereference, preserve the type on the
            --  original node (selected component or indexed component) for
            --  subsequent legality tests, e.g. Is_Variable. which examines
            --  the original node.

            if Nkind (Node_To_Replace) = N_Function_Call then
               Set_Etype
                 (Original_Node (Node_To_Replace), Etype (Node_To_Replace));
            end if;
         end if;
      end Complete_Object_Operation;

      ----------------------
      -- Report_Ambiguity --
      ----------------------

      procedure Report_Ambiguity (Op : Entity_Id) is
         Access_Actual : constant Boolean :=
                           Is_Access_Type (Etype (Prefix (N)));
         Access_Formal : Boolean := False;

      begin
         Error_Msg_Sloc := Sloc (Op);

         if Present (First_Formal (Op)) then
            Access_Formal := Is_Access_Type (Etype (First_Formal (Op)));
         end if;

         if Access_Formal and then not Access_Actual then
            if Nkind (Parent (Op)) = N_Full_Type_Declaration then
               Error_Msg_N
                 ("\possible interpretation "
                  & "(inherited, with implicit 'Access) #", N);
            else
               Error_Msg_N
                 ("\possible interpretation (with implicit 'Access) #", N);
            end if;

         elsif not Access_Formal and then Access_Actual then
            if Nkind (Parent (Op)) = N_Full_Type_Declaration then
               Error_Msg_N
                 ("\possible interpretation "
                  & "(inherited, with implicit dereference) #", N);
            else
               Error_Msg_N
                 ("\possible interpretation (with implicit dereference) #", N);
            end if;

         else
            if Nkind (Parent (Op)) = N_Full_Type_Declaration then
               Error_Msg_N ("\possible interpretation (inherited)#", N);
            else
               Error_Msg_N -- CODEFIX
                 ("\possible interpretation#", N);
            end if;
         end if;
      end Report_Ambiguity;

      --------------------------------
      -- Transform_Object_Operation --
      --------------------------------

      procedure Transform_Object_Operation
        (Call_Node       : out Node_Id;
         Node_To_Replace : out Node_Id)
      is
         Dummy : constant Node_Id := New_Copy (Obj);
         --  Placeholder used as a first parameter in the call, replaced
         --  eventually by the proper object.

         Parent_Node : constant Node_Id := Parent (N);

         Actual  : Node_Id;
         Actuals : List_Id;

      begin
         --  Common case covering 1) Call to a procedure and 2) Call to a
         --  function that has some additional actuals.

         if Nkind (Parent_Node) in N_Subprogram_Call

            --  N is a selected component node containing the name of the
            --  subprogram. If N is not the name of the parent node we must
            --  not replace the parent node by the new construct. This case
            --  occurs when N is a parameterless call to a subprogram that
            --  is an actual parameter of a call to another subprogram. For
            --  example:
            --            Some_Subprogram (..., Obj.Operation, ...)

            and then Name (Parent_Node) = N
         then
            Node_To_Replace := Parent_Node;

            Actuals := Parameter_Associations (Parent_Node);

            if Present (Actuals) then
               Prepend (Dummy, Actuals);
            else
               Actuals := New_List (Dummy);
            end if;
	    
            if Nkind (Parent_Node) = N_Procedure_Call_Statement then
               Call_Node :=
                 Make_Procedure_Call_Statement (Loc,
                   Name => New_Copy (Subprog),
                   Parameter_Associations => Actuals);

            else
               Call_Node :=
                 Make_Function_Call (Loc,
                   Name                   => New_Copy (Subprog),
                   Parameter_Associations => Actuals);
            end if;
	    Set_Parameter_List_Truncated (Call_Node, True);

         --  Before analysis, a function call appears as an indexed component
         --  if there are no named associations.

         elsif Nkind (Parent_Node) = N_Indexed_Component
           and then N = Prefix (Parent_Node)
         then
            Node_To_Replace := Parent_Node;
            Actuals := Expressions (Parent_Node);

            Actual := First (Actuals);
            while Present (Actual) loop
               Analyze (Actual);
               Next (Actual);
            end loop;

            Prepend (Dummy, Actuals);

            Call_Node :=
               Make_Function_Call (Loc,
                 Name                   => New_Copy (Subprog),
                 Parameter_Associations => Actuals);
	    
	    Set_Parameter_List_Truncated (Call_Node, True);
	    
         --  Parameterless call: Obj.F is rewritten as F (Obj)

         else
            Node_To_Replace := N;

            Call_Node :=
               Make_Function_Call (Loc,
                 Name                   => New_Copy (Subprog),
                 Parameter_Associations => New_List (Dummy));
	    
	    Set_Parameter_List_Truncated (Call_Node, True);
         end if;
      end Transform_Object_Operation;

      ------------------------------
      -- Try_Class_Wide_Operation --
      ------------------------------

      function Try_Class_Wide_Operation
        (Call_Node       : Node_Id;
         Node_To_Replace : Node_Id) return Boolean
      is
         Anc_Type    : Entity_Id;
         Matching_Op : Entity_Id := Empty;
         Error       : Boolean;

         procedure Traverse_Homonyms
           (Anc_Type : Entity_Id;
            Error    : out Boolean);
         --  Traverse the homonym chain of the subprogram searching for those
         --  homonyms whose first formal has the Anc_Type's class-wide type,
         --  or an anonymous access type designating the class-wide type. If
         --  an ambiguity is detected, then Error is set to True.

         -----------------------
         -- Traverse_Homonyms --
         -----------------------

         procedure Traverse_Homonyms
           (Anc_Type : Entity_Id;
            Error    : out Boolean)
         is
            Cls_Type    : Entity_Id;
            Hom         : Entity_Id;
            Hom_Ref     : Node_Id;
            Success     : Boolean;

         begin
            Error := False;

            Cls_Type := Class_Wide_Type (Anc_Type);

            Hom := Current_Entity (Subprog);

            --  Find a non-hidden operation whose first parameter is of the
            --  class-wide type, a subtype thereof, or an anonymous access
            --  to same. If in an instance, the operation can be considered
            --  even if hidden (it may be hidden because the instantiation
            --  is expanded after the containing package has been analyzed).

            while Present (Hom) loop
               if Ekind_In (Hom, E_Procedure, E_Function)
                 and then (not Is_Hidden (Hom) or else In_Instance)
                 and then Scope (Hom) = Scope (Anc_Type)
                 and then Present (First_Formal (Hom))
                 and then
                   (Base_Type (Etype (First_Formal (Hom))) = Cls_Type
                     or else
                       (Is_Access_Type (Etype (First_Formal (Hom)))
                         and then
                           Ekind (Etype (First_Formal (Hom))) =
                             E_Anonymous_Access_Type
                         and then
                           Base_Type
                             (Designated_Type (Etype (First_Formal (Hom)))) =
                                                                   Cls_Type))
               then
                  --  If the context is a procedure call, ignore functions
                  --  in the name of the call.

                  if Ekind (Hom) = E_Function
                    and then Nkind (Parent (N)) = N_Procedure_Call_Statement
                    and then N = Name (Parent (N))
                  then
                     goto Next_Hom;

                  --  If the context is a function call, ignore procedures
                  --  in the name of the call.

                  elsif Ekind (Hom) = E_Procedure
                    and then Nkind (Parent (N)) /= N_Procedure_Call_Statement
                  then
                     goto Next_Hom;
                  end if;

                  Set_Etype (Call_Node, Any_Type);
                  Set_Is_Overloaded (Call_Node, False);
                  Success := False;

                  if No (Matching_Op) then
                     Hom_Ref := New_Occurrence_Of (Hom, Sloc (Subprog));
                     Set_Etype (Call_Node, Any_Type);
                     Set_Parent (Call_Node, Parent (Node_To_Replace));

                     Set_Name (Call_Node, Hom_Ref);

                     Analyze_One_Call
                       (N          => Call_Node,
                        Nam        => Hom,
                        Report     => Report_Error,
                        Success    => Success,
                        Skip_First => True);

                     Matching_Op :=
                       Valid_Candidate (Success, Call_Node, Hom);

                  else
                     Analyze_One_Call
                       (N          => Call_Node,
                        Nam        => Hom,
                        Report     => Report_Error,
                        Success    => Success,
                        Skip_First => True);

                     if Present (Valid_Candidate (Success, Call_Node, Hom))
                       and then Nkind (Call_Node) /= N_Function_Call
                     then
                        Error_Msg_NE ("ambiguous call to&", N, Hom);
                        Report_Ambiguity (Matching_Op);
                        Report_Ambiguity (Hom);
                        Error := True;
                        return;
                     end if;
                  end if;
               end if;

               <<Next_Hom>>
                  Hom := Homonym (Hom);
            end loop;
         end Traverse_Homonyms;

      --  Start of processing for Try_Class_Wide_Operation

      begin
         --  If we are searching only for conflicting class-wide subprograms
         --  then initialize directly Matching_Op with the target entity.

         if CW_Test_Only then
            Matching_Op := Entity (Selector_Name (N));
         end if;

         --  Loop through ancestor types (including interfaces), traversing
         --  the homonym chain of the subprogram, trying out those homonyms
         --  whose first formal has the class-wide type of the ancestor, or
         --  an anonymous access type designating the class-wide type.

         Anc_Type := Obj_Type;
         loop
            --  Look for a match among homonyms associated with the ancestor

            Traverse_Homonyms (Anc_Type, Error);

            if Error then
               return True;
            end if;

            exit when Etype (Anc_Type) = Anc_Type;
            Anc_Type := Etype (Anc_Type);
         end loop;

         if Present (Matching_Op) then
            Set_Etype (Call_Node, Etype (Matching_Op));
         end if;

         return Present (Matching_Op);
      end Try_Class_Wide_Operation;

      -----------------------------------
      -- Try_One_Prefix_Interpretation --
      -----------------------------------

      procedure Try_One_Prefix_Interpretation (T : Entity_Id) is

         --  If the interpretation does not have a valid candidate type,
         --  preserve current value of Obj_Type for subsequent errors.

         Prev_Obj_Type : constant Entity_Id := Obj_Type;

      begin
         Obj_Type := T;

         if Is_Access_Type (Obj_Type) then
            Obj_Type := Designated_Type (Obj_Type);
         end if;

         if Ekind (Obj_Type) = E_Private_Subtype then
            Obj_Type := Base_Type (Obj_Type);
         end if;

         if Is_Class_Wide_Type (Obj_Type) then
            Obj_Type := Etype (Class_Wide_Type (Obj_Type));
         end if;

         --  If the object is not tagged, or the type is still an incomplete
         --  type, this is not a prefixed call.

         if not Is_Tagged_Type (Obj_Type)
           or else Is_Incomplete_Type (Obj_Type)
         then

            --  Restore previous type if current one is not legal candidate

            Obj_Type := Prev_Obj_Type;
            return;
         end if;

         declare
            Dup_Call_Node : constant Node_Id := New_Copy (New_Call_Node);
            CW_Result     : Boolean;
            Prim_Result   : Boolean;
            pragma Unreferenced (CW_Result);

         begin
            if not CW_Test_Only then
               Prim_Result :=
                  Try_Primitive_Operation
                   (Call_Node       => New_Call_Node,
                    Node_To_Replace => Node_To_Replace);
            end if;

            --  Check if there is a class-wide subprogram covering the
            --  primitive. This check must be done even if a candidate
            --  was found in order to report ambiguous calls.

            if not (Prim_Result) then
               CW_Result :=
                 Try_Class_Wide_Operation
                   (Call_Node       => New_Call_Node,
                    Node_To_Replace => Node_To_Replace);

            --  If we found a primitive we search for class-wide subprograms
            --  using a duplicate of the call node (done to avoid missing its
            --  decoration if there is no ambiguity).

            else
               CW_Result :=
                 Try_Class_Wide_Operation
                   (Call_Node       => Dup_Call_Node,
                    Node_To_Replace => Node_To_Replace);
            end if;
         end;
      end Try_One_Prefix_Interpretation;

      -----------------------------
      -- Try_Primitive_Operation --
      -----------------------------

      function Try_Primitive_Operation
        (Call_Node       : Node_Id;
         Node_To_Replace : Node_Id) return Boolean
      is
         Elmt        : Elmt_Id;
         Prim_Op     : Entity_Id;
         Matching_Op : Entity_Id := Empty;
         Prim_Op_Ref : Node_Id   := Empty;

         Success : Boolean   := False;

         function Collect_Generic_Type_Ops (T : Entity_Id) return Elist_Id;
         --  For tagged types the candidate interpretations are found in
         --  the list of primitive operations of the type and its ancestors.
         --  For formal tagged types we have to find the operations declared
         --  in the same scope as the type (including in the generic formal
         --  part) because the type itself carries no primitive operations,
         --  except for formal derived types that inherit the operations of
         --  the parent and progenitors.
         --
         --  If the context is a generic subprogram body, the generic formals
         --  are visible by name, but are not in the entity list of the
         --  subprogram because that list starts with the subprogram formals.
         --  We retrieve the candidate operations from the generic declaration.

         function Extended_Primitive_Ops (T : Entity_Id) return Elist_Id;
         --  Prefix notation can also be used on operations that are not
         --  primitives of the type, but are declared in the same immediate
         --  declarative part, which can only mean the corresponding package
         --  body (See RM 4.1.3 (9.2/3)). If we are in that body we extend the
         --  list of primitives with body operations with the same name that
         --  may be candidates, so that Try_Primitive_Operations can examine
         --  them if no real primitive is found.

         function Is_Private_Overriding (Op : Entity_Id) return Boolean;
         --  An operation that overrides an inherited operation in the private
         --  part of its package may be hidden, but if the inherited operation
         --  is visible a direct call to it will dispatch to the private one,
         --  which is therefore a valid candidate.

         function Valid_First_Argument_Of (Op : Entity_Id) return Boolean;
         --  Verify that the prefix, dereferenced if need be, is a valid
         --  controlling argument in a call to Op. The remaining actuals
         --  are checked in the subsequent call to Analyze_One_Call.

         ------------------------------
         -- Collect_Generic_Type_Ops --
         ------------------------------

         function Collect_Generic_Type_Ops (T : Entity_Id) return Elist_Id is
            Bas        : constant Entity_Id := Base_Type (T);
            Candidates : constant Elist_Id := New_Elmt_List;
            Subp       : Entity_Id;
            Formal     : Entity_Id;

            procedure Check_Candidate;
            --  The operation is a candidate if its first parameter is a
            --  controlling operand of the desired type.

            -----------------------
            --  Check_Candidate; --
            -----------------------

            procedure Check_Candidate is
            begin
               Formal := First_Formal (Subp);

               if Present (Formal)
                 and then Is_Controlling_Formal (Formal)
                 and then
                   (Base_Type (Etype (Formal)) = Bas
                     or else
                       (Is_Access_Type (Etype (Formal))
                         and then Designated_Type (Etype (Formal)) = Bas))
               then
                  Append_Elmt (Subp, Candidates);
               end if;
            end Check_Candidate;

         --  Start of processing for Collect_Generic_Type_Ops

         begin
            if Is_Derived_Type (T) then
               return Primitive_Operations (T);

            elsif Ekind_In (Scope (T), E_Procedure, E_Function) then

               --  Scan the list of generic formals to find subprograms
               --  that may have a first controlling formal of the type.

               if Nkind (Unit_Declaration_Node (Scope (T))) =
                                         N_Generic_Subprogram_Declaration
               then
                  declare
                     Decl : Node_Id;

                  begin
                     Decl :=
                       First (Generic_Formal_Declarations
                               (Unit_Declaration_Node (Scope (T))));
                     while Present (Decl) loop
                        if Nkind (Decl) in N_Formal_Subprogram_Declaration then
                           Subp := Defining_Entity (Decl);
                           Check_Candidate;
                        end if;

                        Next (Decl);
                     end loop;
                  end;
               end if;
               return Candidates;

            else
               --  Scan the list of entities declared in the same scope as
               --  the type. In general this will be an open scope, given that
               --  the call we are analyzing can only appear within a generic
               --  declaration or body (either the one that declares T, or a
               --  child unit).

               --  For a subtype representing a generic actual type, go to the
               --  base type.

               if Is_Generic_Actual_Type (T) then
                  Subp := First_Entity (Scope (Base_Type (T)));
               else
                  Subp := First_Entity (Scope (T));
               end if;

               while Present (Subp) loop
                  if Is_Overloadable (Subp) then
                     Check_Candidate;
                  end if;

                  Next_Entity (Subp);
               end loop;

               return Candidates;
            end if;
         end Collect_Generic_Type_Ops;

         ----------------------------
         -- Extended_Primitive_Ops --
         ----------------------------

         function Extended_Primitive_Ops (T : Entity_Id) return Elist_Id is
            Type_Scope : constant Entity_Id := Scope (T);

            Op_List    : Elist_Id;

         begin
            Op_List := Primitive_Operations (T);

            return Op_List;
         end Extended_Primitive_Ops;

         ---------------------------
         -- Is_Private_Overriding --
         ---------------------------

         function Is_Private_Overriding (Op : Entity_Id) return Boolean is
            Visible_Op : constant Entity_Id := Homonym (Op);

         begin
            return Present (Visible_Op)
              and then Scope (Op) = Scope (Visible_Op)
              and then not Comes_From_Source (Visible_Op)
              and then Alias (Visible_Op) = Op
              and then not Is_Hidden (Visible_Op);
         end Is_Private_Overriding;

         -----------------------------
         -- Valid_First_Argument_Of --
         -----------------------------

         function Valid_First_Argument_Of (Op : Entity_Id) return Boolean is
            Typ : Entity_Id := Etype (First_Formal (Op));

         begin
	    
            --  Simple case. Object may be a subtype of the tagged type or
            --  may be the corresponding record of a synchronized type.

            return Obj_Type = Typ
              or else Base_Type (Obj_Type) = Typ

               --  Prefix can be dereferenced

              or else
                (Is_Access_Type (Obj_Type)
                  and then Designated_Type (Obj_Type) = Typ)

               --  Formal is an access parameter, for which the object
               --  can provide an access.

              or else
                (Ekind (Typ) = E_Anonymous_Access_Type
                  and then
                    Base_Type (Designated_Type (Typ)) = Base_Type (Obj_Type));
         end Valid_First_Argument_Of;

      --  Start of processing for Try_Primitive_Operation

      begin
         --  Look for subprograms in the list of primitive operations. The name
         --  must be identical, and the kind of call indicates the expected
         --  kind of operation (function or procedure). If the type is a
         --  (tagged) synchronized type, the primitive ops are attached to the
         --  corresponding record (base) type.

	 Elmt := First_Elmt (Extended_Primitive_Ops (Obj_Type));

         while Present (Elmt) loop
            Prim_Op := Node (Elmt);

            if Chars (Prim_Op) = Chars (Subprog) 
              and then Present (First_Formal (Prim_Op))
              and then Valid_First_Argument_Of (Prim_Op)
              and then
                (Nkind (Call_Node) = N_Function_Call)
                    =
                (Ekind (Prim_Op) = E_Function)
            then
	       
	       --  Do not consider hidden primitives unless the type is in an
	       --  open scope or we are within an instance, where visibility
	       --  is known to be correct, or else if this is an overriding
	       --  operation in the private part for an inherited operation.
	       
	       if (Is_Hidden (Prim_Op)
		     and then not Is_Immediately_Visible (Obj_Type)
		     and then not In_Instance
		     and then not Is_Private_Overriding (Prim_Op))
               then
                  goto Continue;
               end if;

               Set_Etype (Call_Node, Any_Type);
               Set_Is_Overloaded (Call_Node, False);

               if No (Matching_Op) then
                  Prim_Op_Ref := New_Occurrence_Of (Prim_Op, Sloc (Subprog));
                  Candidate := Prim_Op;

                  Set_Parent (Call_Node, Parent (Node_To_Replace));

                  Set_Name (Call_Node, Prim_Op_Ref);
                  Success := False;

                  Analyze_One_Call
                    (N          => Call_Node,
                     Nam        => Prim_Op,
                     Report     => Report_Error,
                     Success    => Success,
                     Skip_First => True);

                  Matching_Op := Valid_Candidate (Success, Call_Node, Prim_Op);

               --  More than one interpretation, collect for subsequent
               --  disambiguation. If this is a procedure call and there
               --  is another match, report ambiguity now.

               else
                  Analyze_One_Call
                    (N          => Call_Node,
                     Nam        => Prim_Op,
                     Report     => Report_Error,
                     Success    => Success,
                     Skip_First => True);

                  if Present (Valid_Candidate (Success, Call_Node, Prim_Op))
                    and then Nkind (Call_Node) /= N_Function_Call
                  then
                     Error_Msg_NE ("ambiguous call to&", N, Prim_Op);
                     Report_Ambiguity (Matching_Op);
                     Report_Ambiguity (Prim_Op);
                     return True;
                  end if;
               end if;
            end if;

            <<Continue>>
            Next_Elmt (Elmt);
         end loop;

         if Present (Matching_Op) then
            Set_Etype (Call_Node, Etype (Matching_Op));
         end if;

         return Present (Matching_Op);
      end Try_Primitive_Operation;

   --  Start of processing for Try_Object_Operation

   begin
      Analyze_Expression (Obj);

      --  Analyze the actuals if node is known to be a subprogram call

      if Is_Subprg_Call and then N = Name (Parent (N)) then
         Actual := First (Parameter_Associations (Parent (N)));
         while Present (Actual) loop
            Analyze_Expression (Actual);
            Next (Actual);
         end loop;
      end if;

      --  Build a subprogram call node, using a copy of Obj as its first
      --  actual. This is a placeholder, to be replaced by an explicit
      --  dereference when needed.

      Transform_Object_Operation
        (Call_Node       => New_Call_Node,
         Node_To_Replace => Node_To_Replace);

      Set_Etype (New_Call_Node, Any_Type);
      Set_Etype (Subprog, Any_Type);
      Set_Parent (New_Call_Node, Parent (Node_To_Replace));

      if not Is_Overloaded (Obj) then
         Try_One_Prefix_Interpretation (Obj_Type);

      else
         declare
            I  : Interp_Index;
            It : Interp;
         begin
            Get_First_Interp (Obj, I, It);
            while Present (It.Nam) loop
               Try_One_Prefix_Interpretation (It.Typ);
               Get_Next_Interp (I, It);
            end loop;
         end;
      end if;

      if Etype (New_Call_Node) /= Any_Type then

         --  No need to complete the tree transformations if we are only
         --  searching for conflicting class-wide subprograms

         if CW_Test_Only then
            return False;
         else
            Complete_Object_Operation
              (Call_Node       => New_Call_Node,
               Node_To_Replace => Node_To_Replace);
            return True;
         end if;

      elsif Present (Candidate) then

         --  The argument list is not type correct. Re-analyze with error
         --  reporting enabled, and use one of the possible candidates.
         --  In All_Errors_Mode, re-analyze all failed interpretations.

         if All_Errors_Mode then
            Report_Error := True;
            if Try_Primitive_Operation
                 (Call_Node       => New_Call_Node,
                  Node_To_Replace => Node_To_Replace)

              or else
                Try_Class_Wide_Operation
                  (Call_Node       => New_Call_Node,
                   Node_To_Replace => Node_To_Replace)
            then
               null;
            end if;

         else
            Analyze_One_Call
              (N          => New_Call_Node,
               Nam        => Candidate,
               Report     => True,
               Success    => Success,
               Skip_First => True);
         end if;

         --  No need for further errors

         return True;

      else
         --  There was no candidate operation, so report it as an error
         --  in the caller: Analyze_Selected_Component.

         return False;
      end if;
   end Try_Object_Operation;

end Sem_Ch4;
