------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ U T I L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2004, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Checks;   use Checks;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
--  with Exp_Ch7;  use Exp_Ch7;
--  with Exp_Ch11; use Exp_Ch11;
--  with Exp_Tss;  use Exp_Tss;
with Hostparm; use Hostparm;
with Inline;   use Inline;
with Itypes;   use Itypes;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Sem;      use Sem;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;
with Urealp;   use Urealp;
with Validsw;  use Validsw;

package body Exp_Util is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Make_CW_Equivalent_Type
     (T    : Entity_Id;
      E    : Node_Id)
      return Entity_Id;
   --  T is a class-wide type entity, E is the initial expression node that
   --  constrains T in case such as: " X: T := E" or "new T'(E)"
   --  This function returns the entity of the Equivalent type and inserts
   --  on the fly the necessary declaration such as:
   --
   --    type anon is record
   --       _parent : Root_Type (T); constrained with E discriminants (if any)
   --       Extension : String (1 .. expr to match size of E);
   --    end record;
   --
   --  This record is compatible with any object of the class of T thanks
   --  to the first field and has the same size as E thanks to the second.

   function Make_Literal_Range
     (Loc         : Source_Ptr;
      Literal_Typ : Entity_Id)
      return        Node_Id;
   --  Produce a Range node whose bounds are:
   --    Low_Bound (Literal_Type) ..
   --        Low_Bound (Literal_Type) + Length (Literal_Typ) - 1
   --  this is used for expanding declarations like X : String := "sdfgdfg";

   function New_Class_Wide_Subtype
     (CW_Typ : Entity_Id;
      N      : Node_Id)
      return   Entity_Id;
   --  Create an implicit subtype of CW_Typ attached to node N.

   ----------------------
   -- Adjust_Condition --
   ----------------------

   procedure Adjust_Condition (N : Node_Id) is
   begin
      if No (N) then
         return;
      end if;

      declare
         Loc : constant Source_Ptr := Sloc (N);
         T   : constant Entity_Id  := Etype (N);
         Ti  : Entity_Id;

      begin
         --  For now, we simply ignore a call where the argument has no
         --  type (probably case of unanalyzed condition), or has a type
         --  that is not Boolean. This is because this is a pretty marginal
         --  piece of functionality, and violations of these rules are
         --  likely to be truly marginal (how much code uses Fortran Logical
         --  as the barrier to a protected entry?) and we do not want to
         --  blow up existing programs. We can change this to an assertion
         --  after 3.12a is released ???

         if No (T) or else not Is_Boolean_Type (T) then
            return;
         end if;

         --  Apply validity checking if needed

         if Validity_Checks_On and Validity_Check_Tests then
            Ensure_Valid (N);
         end if;

         --  Immediate return if standard boolean, the most common case,
         --  where nothing needs to be done.

         if Base_Type (T) = Standard_Boolean then
            return;
         end if;

         --  Case of zero/non-zero semantics or non-standard enumeration
         --  representation. In each case, we rewrite the node as:

         --      ityp!(N) /= False'Enum_Rep

         --  where ityp is an integer type with large enough size to hold
         --  any value of type T.

         if Nonzero_Is_True (T) or else Has_Non_Standard_Rep (T) then
            if Esize (T) <= Esize (Standard_Integer) then
               Ti := Standard_Integer;
            else
               Ti := Standard_Long_Long_Integer;
            end if;

            Rewrite (N,
              Make_Op_Ne (Loc,
                Left_Opnd  => Unchecked_Convert_To (Ti, N),
                Right_Opnd =>
                  Make_Attribute_Reference (Loc,
                    Attribute_Name => Name_Enum_Rep,
                    Prefix         =>
                      New_Occurrence_Of (First_Literal (T), Loc))));
            Analyze_And_Resolve (N, Standard_Boolean);

         else
            Rewrite (N, Convert_To (Standard_Boolean, N));
            Analyze_And_Resolve (N, Standard_Boolean);
         end if;
      end;
   end Adjust_Condition;

   ------------------------
   -- Adjust_Result_Type --
   ------------------------

   procedure Adjust_Result_Type (N : Node_Id; T : Entity_Id) is
   begin
      --  Ignore call if current type is not Standard.Boolean

      if Etype (N) /= Standard_Boolean then
         return;
      end if;

      --  If result is already of correct type, nothing to do. Note that
      --  this will get the most common case where everything has a type
      --  of Standard.Boolean.

      if Base_Type (T) = Standard_Boolean then
         return;

      else
         declare
            KP : constant Node_Kind := Nkind (Parent (N));

         begin
            --  If result is to be used as a Condition in the syntax, no need
            --  to convert it back, since if it was changed to Standard.Boolean
            --  using Adjust_Condition, that is just fine for this usage.

            if KP in N_Raise_xxx_Error or else KP in N_Has_Condition then
               return;

            --  If result is an operand of another logical operation, no need
            --  to reset its type, since Standard.Boolean is just fine, and
            --  such operations always do Adjust_Condition on their operands.

            elsif KP in N_Op_Boolean
              or else KP = N_And_Then
              or else KP = N_Or_Else
              or else KP = N_Op_Not
            then
               return;

            --  Otherwise we perform a conversion from the current type,
            --  which must be Standard.Boolean, to the desired type.

            else
               Set_Analyzed (N);
               Rewrite (N, Convert_To (T, N));
               Analyze_And_Resolve (N, T);
            end if;
         end;
      end if;
   end Adjust_Result_Type;

   --------------------------
   -- Append_Freeze_Action --
   --------------------------

   procedure Append_Freeze_Action (T : Entity_Id; N : Node_Id) is
      Fnode : Node_Id := Freeze_Node (T);

   begin
      Ensure_Freeze_Node (T);
      Fnode := Freeze_Node (T);

      if not Present (Actions (Fnode)) then
         Set_Actions (Fnode, New_List);
      end if;

      Append (N, Actions (Fnode));
   end Append_Freeze_Action;

   ---------------------------
   -- Append_Freeze_Actions --
   ---------------------------

   procedure Append_Freeze_Actions (T : Entity_Id; L : List_Id) is
      Fnode : constant Node_Id := Freeze_Node (T);

   begin
      if No (L) then
         return;

      else
         if No (Actions (Fnode)) then
            Set_Actions (Fnode, L);

         else
            Append_List (L, Actions (Fnode));
         end if;

      end if;
   end Append_Freeze_Actions;

   ----------------------------------
   -- Component_May_Be_Bit_Aligned --
   ----------------------------------

   function Component_May_Be_Bit_Aligned (Comp : Entity_Id) return Boolean is
   begin
      --  If no component clause, then everything is fine, since the
      --  back end never bit-misaligns by default, even if there is
      --  a pragma Packed for the record.

      if No (Component_Clause (Comp)) then
         return False;
      end if;

      --  It is only array and record types that cause trouble

      if not Is_Record_Type (Etype (Comp))
        and then not Is_Array_Type (Etype (Comp))
      then
         return False;

      --  If we know that we have a small (64 bits or less) record
      --  or bit-packed array, then everything is fine, since the
      --  back end can handle these cases correctly.

      elsif Esize (Comp) <= 64
        and then (Is_Record_Type (Etype (Comp))
                   or else Is_Bit_Packed_Array (Etype (Comp)))
      then
         return False;

      --  Otherwise if the component is not byte aligned, we
      --  know we have the nasty unaligned case.

      elsif Normalized_First_Bit (Comp) /= Uint_0
        or else Esize (Comp) mod System_Storage_Unit /= Uint_0
      then
         return True;

      --  If we are large and byte aligned, then OK at this level

      else
         return False;
      end if;
   end Component_May_Be_Bit_Aligned;

   -------------------------------
   -- Convert_To_Actual_Subtype --
   -------------------------------

   procedure Convert_To_Actual_Subtype (Exp : Entity_Id) is
      Act_ST : Entity_Id;

   begin
      Act_ST := Get_Actual_Subtype (Exp);

      if Act_ST = Etype (Exp) then
         return;

      else
         Rewrite (Exp,
           Convert_To (Act_ST, Relocate_Node (Exp)));
         Analyze_And_Resolve (Exp, Act_ST);
      end if;
   end Convert_To_Actual_Subtype;

   -----------------------------------
   -- Current_Sem_Unit_Declarations --
   -----------------------------------

   function Current_Sem_Unit_Declarations return List_Id is
      U     : Node_Id := Unit (Cunit (Current_Sem_Unit));
      Decls : List_Id;

   begin
      --  If the current unit is a package body, locate the visible
      --  declarations of the package spec.

      if Nkind (U) = N_Package_Body then
         U := Unit (Library_Unit (Cunit (Current_Sem_Unit)));
      end if;

      if Nkind (U) = N_Package_Declaration then
         U := Specification (U);
         Decls := Visible_Declarations (U);

         if No (Decls) then
            Decls := New_List;
            Set_Visible_Declarations (U, Decls);
         end if;

      else
         Decls := Declarations (U);

         if No (Decls) then
            Decls := New_List;
            Set_Declarations (U, Decls);
         end if;
      end if;

      return Decls;
   end Current_Sem_Unit_Declarations;

   -----------------------
   -- Duplicate_Subexpr --
   -----------------------

   function Duplicate_Subexpr
     (Exp      : Node_Id;
      Name_Req : Boolean := False)
      return     Node_Id
   is
   begin
      Remove_Side_Effects (Exp, Name_Req);
      return New_Copy_Tree (Exp);
   end Duplicate_Subexpr;

   ---------------------------------
   -- Duplicate_Subexpr_No_Checks --
   ---------------------------------

   function Duplicate_Subexpr_No_Checks
     (Exp      : Node_Id;
      Name_Req : Boolean := False)
      return     Node_Id
   is
      New_Exp : Node_Id;

   begin
      Remove_Side_Effects (Exp, Name_Req);
      New_Exp := New_Copy_Tree (Exp);
      Remove_Checks (New_Exp);
      return New_Exp;
   end Duplicate_Subexpr_No_Checks;

   -----------------------------------
   -- Duplicate_Subexpr_Move_Checks --
   -----------------------------------

   function Duplicate_Subexpr_Move_Checks
     (Exp      : Node_Id;
      Name_Req : Boolean := False)
      return     Node_Id
   is
      New_Exp : Node_Id;

   begin
      Remove_Side_Effects (Exp, Name_Req);
      New_Exp := New_Copy_Tree (Exp);
      Remove_Checks (Exp);
      return New_Exp;
   end Duplicate_Subexpr_Move_Checks;

   --------------------
   -- Ensure_Defined --
   --------------------

   procedure Ensure_Defined (Typ : Entity_Id; N : Node_Id) is
      IR : Node_Id;
      P  : Node_Id;

   begin
      if Is_Itype (Typ) then
         IR := Make_Itype_Reference (Sloc (N));
         Set_Itype (IR, Typ);

         if not In_Open_Scopes (Scope (Typ))
           and then Is_Subprogram (Current_Scope)
           and then Scope (Current_Scope) /= Standard_Standard
         then
            --  Insert node in front of subprogram, to avoid scope anomalies
            --  in gigi.

            P := Parent (N);

            while Present (P)
              and then Nkind (P) /= N_Subprogram_Body
            loop
               P := Parent (P);
            end loop;

            if Present (P) then
               Insert_Action (P, IR);
            else
               Insert_Action (N, IR);
            end if;

         else
            Insert_Action (N, IR);
         end if;
      end if;
   end Ensure_Defined;

   ---------------------
   -- Evolve_And_Then --
   ---------------------

   procedure Evolve_And_Then (Cond : in out Node_Id; Cond1 : Node_Id) is
   begin
      if No (Cond) then
         Cond := Cond1;
      else
         Cond :=
           Make_And_Then (Sloc (Cond1),
             Left_Opnd  => Cond,
             Right_Opnd => Cond1);
      end if;
   end Evolve_And_Then;

   --------------------
   -- Evolve_Or_Else --
   --------------------

   procedure Evolve_Or_Else (Cond : in out Node_Id; Cond1 : Node_Id) is
   begin
      if No (Cond) then
         Cond := Cond1;
      else
         Cond :=
           Make_Or_Else (Sloc (Cond1),
             Left_Opnd  => Cond,
             Right_Opnd => Cond1);
      end if;
   end Evolve_Or_Else;

   ------------------------------
   -- Expand_Subtype_From_Expr --
   ------------------------------

   --  This function is applicable for both static and dynamic allocation of
   --  objects which are constrained by an initial expression. Basically it
   --  transforms an unconstrained subtype indication into a constrained one.
   --  The expression may also be transformed in certain cases in order to
   --  avoid multiple evaulation. In the static allocation case, the general
   --  scheme is :

   --     Val : T := Expr;

   --        is transformed into

   --     Val : Constrained_Subtype_of_T := Maybe_Modified_Expr;
   --
   --  Here are the main cases :
   --
   --  <if Expr is a Slice>
   --    Val : T ([Index_Subtype (Expr)]) := Expr;
   --
   --  <elsif Expr is a String Literal>
   --    Val : T (T'First .. T'First + Length (string literal) - 1) := Expr;
   --
   --  <elsif Expr is Constrained>
   --    subtype T is Type_Of_Expr
   --    Val : T := Expr;
   --
   --  <elsif Expr is an entity_name>
   --    Val : T (constraints taken from Expr) := Expr;
   --
   --  <else>
   --    type Axxx is access all T;
   --    Rval : Axxx := Expr'ref;
   --    Val  : T (constraints taken from Rval) := Rval.all;

   --    ??? note: when the Expression is allocated in the secondary stack
   --              we could use it directly instead of copying it by declaring
   --              Val : T (...) renames Rval.all

   procedure Expand_Subtype_From_Expr
     (N             : Node_Id;
      Unc_Type      : Entity_Id;
      Subtype_Indic : Node_Id;
      Exp           : Node_Id)
   is
      Loc     : constant Source_Ptr := Sloc (N);
      Exp_Typ : constant Entity_Id  := Etype (Exp);
      T       : Entity_Id;

   begin
      --  In general we cannot build the subtype if expansion is disabled,
      --  because internal entities may not have been defined. However, to
      --  avoid some cascaded errors, we try to continue when the expression
      --  is an array (or string), because it is safe to compute the bounds.
      --  It is in fact required to do so even in a generic context, because
      --  there may be constants that depend on bounds of string literal.

      if not Expander_Active
        and then (No (Etype (Exp))
                   or else Base_Type (Etype (Exp)) /= Standard_String)
      then
         return;
      end if;

      if Nkind (Exp) = N_Slice then
         declare
            Slice_Type : constant Entity_Id := Etype (First_Index (Exp_Typ));

         begin
            Rewrite (Subtype_Indic,
              Make_Subtype_Indication (Loc,
                Subtype_Mark => New_Reference_To (Unc_Type, Loc),
                Constraint =>
                  Make_Index_Or_Discriminant_Constraint (Loc,
                    Constraints => New_List
                      (New_Reference_To (Slice_Type, Loc)))));

            --  This subtype indication may be used later for contraint checks
            --  we better make sure that if a variable was used as a bound of
            --  of the original slice, its value is frozen.

            Force_Evaluation (Low_Bound (Scalar_Range (Slice_Type)));
            Force_Evaluation (High_Bound (Scalar_Range (Slice_Type)));
         end;

      elsif Ekind (Exp_Typ) = E_String_Literal_Subtype then
         Rewrite (Subtype_Indic,
           Make_Subtype_Indication (Loc,
             Subtype_Mark => New_Reference_To (Unc_Type, Loc),
             Constraint =>
               Make_Index_Or_Discriminant_Constraint (Loc,
                 Constraints => New_List (
                   Make_Literal_Range (Loc,
                     Literal_Typ => Exp_Typ)))));

      elsif Is_Constrained (Exp_Typ)
        and then not Is_Class_Wide_Type (Unc_Type)
      then
         if Is_Itype (Exp_Typ) then

            --  No need to generate a new one.

            T := Exp_Typ;

         else
            T :=
              Make_Defining_Identifier (Loc,
                Chars => New_Internal_Name ('T'));

            Insert_Action (N,
              Make_Subtype_Declaration (Loc,
                Defining_Identifier => T,
                Subtype_Indication  => New_Reference_To (Exp_Typ, Loc)));

            --  This type is marked as an itype even though it has an
            --  explicit declaration because otherwise it can be marked
            --  with Is_Generic_Actual_Type and generate spurious errors.
            --  (see sem_ch8.Analyze_Package_Renaming and sem_type.covers)

            Set_Is_Itype (T);
            Set_Associated_Node_For_Itype (T, Exp);
         end if;

         Rewrite (Subtype_Indic, New_Reference_To (T, Loc));

      else
         Remove_Side_Effects (Exp);
         Rewrite (Subtype_Indic,
           Make_Subtype_From_Expr (Exp, Unc_Type));
      end if;
   end Expand_Subtype_From_Expr;

   ------------------
   -- Find_Prim_Op --
   ------------------

   function Find_Prim_Op (T : Entity_Id; Name : Name_Id) return Entity_Id is
      Prim : Elmt_Id;
      Typ  : Entity_Id := T;

   begin
      if Is_Class_Wide_Type (Typ) then
         Typ := Root_Type (Typ);
      end if;

      Typ := Underlying_Type (Typ);

      Prim := First_Elmt (Primitive_Operations (Typ));
      while Chars (Node (Prim)) /= Name loop
         Next_Elmt (Prim);
         pragma Assert (Present (Prim));
      end loop;

      return Node (Prim);
   end Find_Prim_Op;

   function Find_Prim_Op
     (T    : Entity_Id;
      Name : TSS_Name_Type) return Entity_Id
   is
      Prim : Elmt_Id;
      Typ  : Entity_Id := T;

   begin
      if Is_Class_Wide_Type (Typ) then
         Typ := Root_Type (Typ);
      end if;

      Typ := Underlying_Type (Typ);

      Prim := First_Elmt (Primitive_Operations (Typ));
      while not Is_TSS (Node (Prim), Name) loop
         Next_Elmt (Prim);
         pragma Assert (Present (Prim));
      end loop;

      return Node (Prim);
   end Find_Prim_Op;

   ----------------------
   -- Force_Evaluation --
   ----------------------

   procedure Force_Evaluation (Exp : Node_Id; Name_Req : Boolean := False) is
   begin
      Remove_Side_Effects (Exp, Name_Req, Variable_Ref => True);
   end Force_Evaluation;

   ---------------------------------
   -- Get_Current_Value_Condition --
   ---------------------------------

   procedure Get_Current_Value_Condition
     (Var : Node_Id;
      Op  : out Node_Kind;
      Val : out Node_Id)
   is
      Loc  : constant Source_Ptr := Sloc (Var);
      CV   : constant Node_Id    := Current_Value (Entity (Var));
      Sens : Boolean;
      Stm  : Node_Id;
      Cond : Node_Id;

   begin
      Op  := N_Empty;
      Val := Empty;

      --  If statement. Condition is known true in THEN section, known False
      --  in any ELSIF or ELSE part, and unknown outside the IF statement.

      if Nkind (CV) = N_If_Statement then

         --  Before start of IF statement

         if Loc < Sloc (CV) then
            return;

         --  After end of IF statement

         elsif Loc >= Sloc (CV) + Text_Ptr (UI_To_Int (End_Span (CV))) then
            return;
         end if;

         --  At this stage we know that we are within the IF statement, but
         --  unfortunately, the tree does not record the SLOC of the ELSE so
         --  we cannot use a simple SLOC comparison to distinguish between
         --  the then/else statements, so we have to climb the tree.

         declare
            N : Node_Id;

         begin
            N := Parent (Var);
            while Parent (N) /= CV loop
               N := Parent (N);

               --  If we fall off the top of the tree, then that's odd, but
               --  perhaps it could occur in some error situation, and the
               --  safest response is simply to assume that the outcome of
               --  the condition is unknown. No point in bombing during an
               --  attempt to optimize things.

               if No (N) then
                  return;
               end if;
            end loop;

            --  Now we have N pointing to a node whose parent is the IF
            --  statement in question, so now we can tell if we are within
            --  the THEN statements.

            if Is_List_Member (N)
              and then List_Containing (N) = Then_Statements (CV)
            then
               Sens := True;

            --  Otherwise we must be in ELSIF or ELSE part

            else
               Sens := False;
            end if;
         end;

      --  ELSIF part. Condition is known true within the referenced
      --  ELSIF, known False in any subsequent ELSIF or ELSE part,
      --  and unknown before the ELSE part or after the IF statement.

      elsif Nkind (CV) = N_Elsif_Part then
         Stm := Parent (CV);

         --  Before start of ELSIF part

         if Loc < Sloc (CV) then
            return;

         --  After end of IF statement

         elsif Loc >= Sloc (Stm) +
                        Text_Ptr (UI_To_Int (End_Span (Stm)))
         then
            return;
         end if;

         --  Again we lack the SLOC of the ELSE, so we need to climb the
         --  tree to see if we are within the ELSIF part in question.

         declare
            N : Node_Id;

         begin
            N := Parent (Var);
            while Parent (N) /= Stm loop
               N := Parent (N);

               --  If we fall off the top of the tree, then that's odd, but
               --  perhaps it could occur in some error situation, and the
               --  safest response is simply to assume that the outcome of
               --  the condition is unknown. No point in bombing during an
               --  attempt to optimize things.

               if No (N) then
                  return;
               end if;
            end loop;

            --  Now we have N pointing to a node whose parent is the IF
            --  statement in question, so see if is the ELSIF part we want.
            --  the THEN statements.

            if N = CV then
               Sens := True;

            --  Otherwise we must be in susbequent ELSIF or ELSE part

            else
               Sens := False;
            end if;
         end;

      --  All other cases of Current_Value settings

      else
         return;
      end if;

      --  If we fall through here, then we have a reportable
      --  condition, Sens is True if the condition is true and
      --  False if it needs inverting.

      Cond := Condition (CV);

      --  Deal with NOT operators, inverting sense

      while Nkind (Cond) = N_Op_Not loop
         Cond := Right_Opnd (Cond);
         Sens := not Sens;
      end loop;

      --  Now we must have a relational operator

      pragma Assert (Entity (Var) = Entity (Left_Opnd (Cond)));
      Val := Right_Opnd (Cond);
      Op  := Nkind (Cond);

      if Sens = False then
         case Op is
            when N_Op_Eq => Op := N_Op_Ne;
            when N_Op_Ne => Op := N_Op_Eq;
            when N_Op_Lt => Op := N_Op_Ge;
            when N_Op_Gt => Op := N_Op_Le;
            when N_Op_Le => Op := N_Op_Gt;
            when N_Op_Ge => Op := N_Op_Lt;

            --  No other entry should be possible

            when others =>
               raise Program_Error;
         end case;
      end if;
   end Get_Current_Value_Condition;

   --------------------
   -- Homonym_Number --
   --------------------

   function Homonym_Number (Subp : Entity_Id) return Nat is
      Count : Nat;
      Hom   : Entity_Id;

   begin
      Count := 1;
      Hom := Homonym (Subp);
      while Present (Hom) loop
         if Scope (Hom) = Scope (Subp) then
            Count := Count + 1;
         end if;

         Hom := Homonym (Hom);
      end loop;

      return Count;
   end Homonym_Number;

   ------------------------------
   -- In_Unconditional_Context --
   ------------------------------

   function In_Unconditional_Context (Node : Node_Id) return Boolean is
      P : Node_Id;

   begin
      P := Node;
      while Present (P) loop
         case Nkind (P) is
            when N_Subprogram_Body =>
               return True;

            when N_If_Statement =>
               return False;

            when N_Loop_Statement =>
               return False;

            when N_Case_Statement =>
               return False;

            when others =>
               P := Parent (P);
         end case;
      end loop;

      return False;
   end In_Unconditional_Context;

   -------------------
   -- Insert_Action --
   -------------------

   procedure Insert_Action (Assoc_Node : Node_Id; Ins_Action : Node_Id) is
   begin
      if Present (Ins_Action) then
         Insert_Actions (Assoc_Node, New_List (Ins_Action));
      end if;
   end Insert_Action;

   --  Version with check(s) suppressed

   procedure Insert_Action
     (Assoc_Node : Node_Id; Ins_Action : Node_Id; Suppress : Check_Id)
   is
   begin
      Insert_Actions (Assoc_Node, New_List (Ins_Action), Suppress);
   end Insert_Action;

   --------------------
   -- Insert_Actions --
   --------------------

   procedure Insert_Actions (Assoc_Node : Node_Id; Ins_Actions : List_Id) is
      N : Node_Id;
      P : Node_Id;

      Wrapped_Node : Node_Id := Empty;

   begin
      if No (Ins_Actions) or else Is_Empty_List (Ins_Actions) then
         return;
      end if;

      --  Ignore insert of actions from inside default expression in the
      --  special preliminary analyze mode. Any insertions at this point
      --  have no relevance, since we are only doing the analyze to freeze
      --  the types of any static expressions. See section "Handling of
      --  Default Expressions" in the spec of package Sem for further details.

      if In_Default_Expression then
         return;
      end if;

      --  If the action derives from stuff inside a record, then the actions
      --  are attached to the current scope, to be inserted and analyzed on
      --  exit from the scope. The reason for this is that we may also
      --  be generating freeze actions at the same time, and they must
      --  eventually be elaborated in the correct order.

      if Is_Record_Type (Current_Scope)
        and then not Is_Frozen (Current_Scope)
      then
         if No (Scope_Stack.Table
           (Scope_Stack.Last).Pending_Freeze_Actions)
         then
            Scope_Stack.Table (Scope_Stack.Last).Pending_Freeze_Actions :=
              Ins_Actions;
         else
            Append_List
              (Ins_Actions,
               Scope_Stack.Table (Scope_Stack.Last).Pending_Freeze_Actions);
         end if;

         return;
      end if;

      --  We now intend to climb up the tree to find the right point to
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
               if N = Right_Opnd (P) then
                  if Present (Actions (P)) then
                     Insert_List_After_And_Analyze
                      (Last (Actions (P)), Ins_Actions);
                  else
                     Set_Actions (P, Ins_Actions);
                     Analyze_List (Actions (P));
                  end if;

                  return;
               end if;

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
                        Insert_List_After_And_Analyze
                          (Last (Then_Actions (P)), Ins_Actions);
                     else
                        Set_Then_Actions (P, Ins_Actions);
                        Analyze_List (Then_Actions (P));
                     end if;

                     return;

                  --  Actions belong to the else expression, temporarily
                  --  place them as Else_Actions of the conditional expr.
                  --  They will be moved to the proper place later when
                  --  the conditional expression is expanded.

                  elsif N = ElseX then
                     if Present (Else_Actions (P)) then
                        Insert_List_After_And_Analyze
                          (Last (Else_Actions (P)), Ins_Actions);
                     else
                        Set_Else_Actions (P, Ins_Actions);
                        Analyze_List (Else_Actions (P));
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
               if N = Condition (P) then
                  if Present (Condition_Actions (P)) then
                     Insert_List_After_And_Analyze
                       (Last (Condition_Actions (P)), Ins_Actions);
                  else
                     Set_Condition_Actions (P, Ins_Actions);

                     --  Set the parent of the insert actions explicitly.
                     --  This is not a syntactic field, but we need the
                     --  parent field set, in particular so that freeze
                     --  can understand that it is dealing with condition
                     --  actions, and properly insert the freezing actions.

                     Set_Parent (Ins_Actions, P);
                     Analyze_List (Condition_Actions (P));
                  end if;

                  return;
               end if;

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
               N_Package_Body_Stub                      |
               N_Package_Declaration                    |
               N_Package_Instantiation                  |
               N_Package_Renaming_Declaration           |
               N_Private_Extension_Declaration          |
               N_Private_Type_Declaration               |
               N_Procedure_Instantiation                |
               N_Subprogram_Body                        |
               N_Subprogram_Body_Stub                   |
               N_Subprogram_Declaration                 |
               N_Subprogram_Renaming_Declaration        |
               N_Subtype_Declaration                    |

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

               elsif Nkind (Parent (P)) = N_Component_Association then
                  null;

               --  Do not insert if the parent of P is either an N_Variant
               --  node or an N_Record_Definition node, meaning in either
               --  case that P is a member of a component list, and that
               --  therefore the actions should be inserted outside the
               --  complete record declaration.

               elsif Nkind (Parent (P)) = N_Record_Definition
               then
                  null;

               --  Do not insert freeze nodes within the loop generated for
               --  an aggregate, because they may be elaborated too late for
               --  subsequent use in the back end: within a package spec the
               --  loop is part of the elaboration procedure and is only
               --  elaborated during the second pass.
               --  If the loop comes from source, or the entity is local to
               --  the loop itself it must remain within.

               elsif Nkind (Parent (P)) = N_Loop_Statement
                 and then not Comes_From_Source (Parent (P))
                 and then Nkind (First (Ins_Actions)) = N_Freeze_Entity
                 and then
                   Scope (Entity (First (Ins_Actions))) /= Current_Scope
               then
                  null;

               else
                  Insert_List_Before_And_Analyze (P, Ins_Actions);
                  return;
               end if;

            --  A special case, N_Raise_xxx_Error can act either as a
            --  statement or a subexpression. We tell the difference
            --  by looking at the Etype. It is set to Standard_Void_Type
            --  in the statement case.

            when
               N_Raise_xxx_Error =>
                  if Etype (P) = Standard_Void_Type then
                        Insert_List_Before_And_Analyze (P, Ins_Actions);

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
                  if Nkind (Parent (P)) = N_Aggregate
                    and then Present (Loop_Actions (P))
                  then
                     if Is_Empty_List (Loop_Actions (P)) then
                        Set_Loop_Actions (P, Ins_Actions);
                        Analyze_List (Ins_Actions);

                     else
                        declare
                           Decl : Node_Id := Assoc_Node;

                        begin
                           --  Check whether these actions were generated
                           --  by a declaration that is part of the loop_
                           --  actions for the component_association.

                           while Present (Decl) loop
                              exit when Parent (Decl) = P
                                and then Is_List_Member (Decl)
                                and then
                                  List_Containing (Decl) = Loop_Actions (P);
                              Decl := Parent (Decl);
                           end loop;

                           if Present (Decl) then
                              Insert_List_Before_And_Analyze
                                (Decl, Ins_Actions);
                           else
                              Insert_List_After_And_Analyze
                                (Last (Loop_Actions (P)), Ins_Actions);
                           end if;
                        end;
                     end if;

                     return;

                  else
                     null;
                  end if;

            --  Another special case, an attribute denoting a procedure call

            when
               N_Attribute_Reference =>
                  if Is_Procedure_Attribute_Name (Attribute_Name (P)) then
                        Insert_List_Before_And_Analyze (P, Ins_Actions);

                     return;

                  --  In the subexpression case, keep climbing

                  else
                     null;
                  end if;

            --  For all other node types, keep climbing tree

            when
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
               N_Itype_Reference                        |
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
               N_Subunit                                |
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

   end Insert_Actions;

   --  Version with check(s) suppressed

   procedure Insert_Actions
     (Assoc_Node : Node_Id; Ins_Actions : List_Id; Suppress : Check_Id)
   is
   begin
      if Suppress = All_Checks then
         declare
            Svg : constant Suppress_Array := Scope_Suppress;

         begin
            Scope_Suppress := (others => True);
            Insert_Actions (Assoc_Node, Ins_Actions);
            Scope_Suppress := Svg;
         end;

      else
         declare
            Svg : constant Boolean := Scope_Suppress (Suppress);

         begin
            Scope_Suppress (Suppress) := True;
            Insert_Actions (Assoc_Node, Ins_Actions);
            Scope_Suppress (Suppress) := Svg;
         end;
      end if;
   end Insert_Actions;

   --------------------------
   -- Insert_Actions_After --
   --------------------------

   procedure Insert_Actions_After
     (Assoc_Node  : Node_Id;
      Ins_Actions : List_Id)
   is
   begin
         Insert_List_After_And_Analyze (Assoc_Node, Ins_Actions);
   end Insert_Actions_After;

   ---------------------------------
   -- Insert_Library_Level_Action --
   ---------------------------------

   procedure Insert_Library_Level_Action (N : Node_Id) is
      Aux : constant Node_Id := Aux_Decls_Node (Cunit (Main_Unit));

   begin
      New_Scope (Cunit_Entity (Main_Unit));

      if No (Actions (Aux)) then
         Set_Actions (Aux, New_List (N));
      else
         Append (N, Actions (Aux));
      end if;

      Analyze (N);
      Pop_Scope;
   end Insert_Library_Level_Action;

   ----------------------------------
   -- Insert_Library_Level_Actions --
   ----------------------------------

   procedure Insert_Library_Level_Actions (L : List_Id) is
      Aux : constant Node_Id := Aux_Decls_Node (Cunit (Main_Unit));

   begin
      if Is_Non_Empty_List (L) then
         New_Scope (Cunit_Entity (Main_Unit));

         if No (Actions (Aux)) then
            Set_Actions (Aux, L);
            Analyze_List (L);
         else
            Insert_List_After_And_Analyze (Last (Actions (Aux)), L);
         end if;

         Pop_Scope;
      end if;
   end Insert_Library_Level_Actions;

   ----------------------
   -- Inside_Init_Proc --
   ----------------------

   function Inside_Init_Proc return Boolean is
      S : Entity_Id;

   begin
      S := Current_Scope;
      while Present (S)
        and then S /= Standard_Standard
      loop
         if Is_Init_Proc (S) then
            return True;
         else
            S := Scope (S);
         end if;
      end loop;

      return False;
   end Inside_Init_Proc;

   ----------------------------
   -- Is_All_Null_Statements --
   ----------------------------

   function Is_All_Null_Statements (L : List_Id) return Boolean is
      Stm : Node_Id;

   begin
      Stm := First (L);
      while Present (Stm) loop
         if Nkind (Stm) /= N_Null_Statement then
            return False;
         end if;

         Next (Stm);
      end loop;

      return True;
   end Is_All_Null_Statements;

   ----------------------------------
   -- Is_Possibly_Unaligned_Object --
   ----------------------------------

   function Is_Possibly_Unaligned_Object (P : Node_Id) return Boolean is
   begin
      --  If target does not have strict alignment, result is always
      --  False, since correctness of code does no depend on alignment.

      if not Target_Strict_Alignment then
         return False;
      end if;

      --  If renamed object, apply test to underlying object

      if Is_Entity_Name (P)
        and then Is_Object (Entity (P))
        and then Present (Renamed_Object (Entity (P)))
      then
         return Is_Possibly_Unaligned_Object (Renamed_Object (Entity (P)));
      end if;

      --  If this is an element of a packed array, may be unaligned

      if Is_Ref_To_Bit_Packed_Array (P) then
         return True;
      end if;

      --  Case of component reference

      if Nkind (P) = N_Selected_Component then

         --  If component reference is for a record that is bit packed
         --  or has a specified alignment (that might be too small) or
         --  the component reference has a component clause, then the
         --  object may be unaligned.

         if Is_Packed (Etype (Prefix (P)))
           or else Known_Alignment (Etype (Prefix (P)))
           or else Present (Component_Clause (Entity (Selector_Name (P))))
         then
            return True;

         --  Otherwise, for a component reference, test prefix

         else
            return Is_Possibly_Unaligned_Object (Prefix (P));
         end if;

      --  If not a component reference, must be aligned

      else
         return False;
      end if;
   end Is_Possibly_Unaligned_Object;

   ---------------------------------
   -- Is_Possibly_Unaligned_Slice --
   ---------------------------------

   function Is_Possibly_Unaligned_Slice (P : Node_Id) return Boolean is
   begin
      if Is_Entity_Name (P)
        and then Is_Object (Entity (P))
        and then Present (Renamed_Object (Entity (P)))
      then
         return Is_Possibly_Unaligned_Slice (Renamed_Object (Entity (P)));
      end if;

      --  We only need to worry if the target has strict alignment, unless
      --  it is a nested record component with a component clause, which
      --  Gigi does not handle well. This patch should disappear with GCC 3.0
      --  and it is not clear why it is needed even when the representation
      --  clause is a confirming one, but in its absence gigi complains that
      --  the slice is not addressable.???

      if not Target_Strict_Alignment then
         if Nkind (P) /= N_Slice
           or else Nkind (Prefix (P)) /= N_Selected_Component
           or else Nkind (Prefix (Prefix (P))) /= N_Selected_Component
         then
            return False;
         end if;
      end if;

      --  The reference must be a slice

      if Nkind (P) /= N_Slice then
         return False;
      end if;

      --  If it is a slice, then look at the array type being sliced

      declare
         Pref : constant Node_Id   := Prefix (P);
         Typ  : constant Entity_Id := Etype (Prefix (P));

      begin
         --  The worrisome case is one where we don't know the alignment
         --  of the array, or we know it and it is greater than 1 (if the
         --  alignment is one, then obviously it cannot be misaligned).

         if Known_Alignment (Typ) and then Alignment (Typ) = 1 then
            return False;
         end if;

         --  The only way we can be unaligned is if the array being sliced
         --  is a component of a record, and either the record is packed,
         --  or the component has a component clause, or the record has
         --  a specified alignment (that might be too small).

         return
            Nkind (Pref) = N_Selected_Component
              and then
                 (Is_Packed (Etype (Prefix (Pref)))
                    or else
                  Known_Alignment (Etype (Prefix (Pref)))
                    or else
                  Present (Component_Clause (Entity (Selector_Name (Pref)))));
      end;
   end Is_Possibly_Unaligned_Slice;

   --------------------------------
   -- Is_Ref_To_Bit_Packed_Array --
   --------------------------------

   function Is_Ref_To_Bit_Packed_Array (P : Node_Id) return Boolean is
      Result : Boolean;
      Expr   : Node_Id;

   begin
      if Is_Entity_Name (P)
        and then Is_Object (Entity (P))
        and then Present (Renamed_Object (Entity (P)))
      then
         return Is_Ref_To_Bit_Packed_Array (Renamed_Object (Entity (P)));
      end if;

      if Nkind (P) = N_Indexed_Component
           or else
         Nkind (P) = N_Selected_Component
      then
         if Is_Bit_Packed_Array (Etype (Prefix (P))) then
            Result := True;
         else
            Result := Is_Ref_To_Bit_Packed_Array (Prefix (P));
         end if;

         if Result and then Nkind (P) = N_Indexed_Component then
            Expr := First (Expressions (P));

            while Present (Expr) loop
               Force_Evaluation (Expr);
               Next (Expr);
            end loop;
         end if;

         return Result;

      else
         return False;
      end if;
   end Is_Ref_To_Bit_Packed_Array;

   --------------------------------
   -- Is_Ref_To_Bit_Packed_Slice --
   --------------------------------

   function Is_Ref_To_Bit_Packed_Slice (P : Node_Id) return Boolean is
   begin
      if Is_Entity_Name (P)
        and then Is_Object (Entity (P))
        and then Present (Renamed_Object (Entity (P)))
      then
         return Is_Ref_To_Bit_Packed_Slice (Renamed_Object (Entity (P)));
      end if;

      if Nkind (P) = N_Slice
        and then Is_Bit_Packed_Array (Etype (Prefix (P)))
      then
         return True;

      elsif Nkind (P) = N_Indexed_Component
           or else
         Nkind (P) = N_Selected_Component
      then
         return Is_Ref_To_Bit_Packed_Slice (Prefix (P));

      else
         return False;
      end if;
   end Is_Ref_To_Bit_Packed_Slice;

   -----------------------
   -- Is_Renamed_Object --
   -----------------------

   function Is_Renamed_Object (N : Node_Id) return Boolean is
      Pnod : constant Node_Id   := Parent (N);
      Kind : constant Node_Kind := Nkind (Pnod);

   begin
      if Kind = N_Object_Renaming_Declaration then
         return True;

      elsif Kind = N_Indexed_Component
        or else Kind = N_Selected_Component
      then
         return Is_Renamed_Object (Pnod);

      else
         return False;
      end if;
   end Is_Renamed_Object;

   ----------------------------
   -- Is_Untagged_Derivation --
   ----------------------------

   function Is_Untagged_Derivation (T : Entity_Id) return Boolean is
   begin
      return (not Is_Tagged_Type (T) and then Is_Derived_Type (T))
               or else
             (Is_Private_Type (T) and then Present (Full_View (T))
               and then not Is_Tagged_Type (Full_View (T))
               and then Is_Derived_Type (Full_View (T))
               and then Etype (Full_View (T)) /= T);

   end Is_Untagged_Derivation;

   --------------------
   -- Kill_Dead_Code --
   --------------------

   procedure Kill_Dead_Code (N : Node_Id) is
   begin
      if Present (N) then
         Remove_Handler_Entries (N);
         Remove_Warning_Messages (N);

         --  Recurse into block statements and bodies to process declarations
         --  and statements

         if Nkind (N) = N_Block_Statement
           or else Nkind (N) = N_Subprogram_Body
           or else Nkind (N) = N_Package_Body
         then
            Kill_Dead_Code (Declarations (N));
            Kill_Dead_Code (Statements (Handled_Statement_Sequence (N)));

            if Nkind (N) = N_Subprogram_Body then
               Set_Is_Eliminated (Defining_Entity (N));
            end if;

         --  Recurse into composite statement to kill individual statements,
         --  in particular instantiations.

         elsif Nkind (N) = N_If_Statement then
            Kill_Dead_Code (Then_Statements (N));
            Kill_Dead_Code (Elsif_Parts (N));
            Kill_Dead_Code (Else_Statements (N));

         elsif Nkind (N) = N_Loop_Statement then
            Kill_Dead_Code (Statements (N));

         elsif Nkind (N) = N_Case_Statement then
            declare
               Alt : Node_Id := First (Alternatives (N));

            begin
               while Present (Alt) loop
                  Kill_Dead_Code (Statements (Alt));
                  Next (Alt);
               end loop;
            end;

         elsif Nkind (N) = N_Case_Statement_Alternative then
            Kill_Dead_Code (Statements (N));

         --  Deal with dead instances caused by deleting instantiations

         elsif Nkind (N) in N_Generic_Instantiation then
            Remove_Dead_Instance (N);
         end if;

         Delete_Tree (N);
      end if;
   end Kill_Dead_Code;

   --  Case where argument is a list of nodes to be killed

   procedure Kill_Dead_Code (L : List_Id) is
      N : Node_Id;

   begin
      if Is_Non_Empty_List (L) then
         loop
            N := Remove_Head (L);
            exit when No (N);
            Kill_Dead_Code (N);
         end loop;
      end if;
   end Kill_Dead_Code;

   ------------------------
   -- Known_Non_Negative --
   ------------------------

   function Known_Non_Negative (Opnd : Node_Id) return Boolean is
   begin
      if Is_OK_Static_Expression (Opnd)
        and then Expr_Value (Opnd) >= 0
      then
         return True;

      else
         declare
            Lo : constant Node_Id := Type_Low_Bound (Etype (Opnd));

         begin
            return
              Is_OK_Static_Expression (Lo) and then Expr_Value (Lo) >= 0;
         end;
      end if;
   end Known_Non_Negative;

   --------------------
   -- Known_Non_Null --
   --------------------

   function Known_Non_Null (N : Node_Id) return Boolean is
   begin
      pragma Assert (Is_Access_Type (Underlying_Type (Etype (N))));

      --  Case of entity for which Is_Known_Non_Null is True

      if Is_Entity_Name (N) and then Is_Known_Non_Null (Entity (N)) then

         --  If the entity is aliased or volatile, then we decide that
         --  we don't know it is really non-null even if the sequential
         --  flow indicates that it is, since such variables can be
         --  changed without us noticing.

         if Is_Aliased (Entity (N))
           or else Treat_As_Volatile (Entity (N))
         then
            return False;

         --  For all other cases, the flag is decisive

         else
            return True;
         end if;

      --  True if access attribute

      elsif Nkind (N) = N_Attribute_Reference
        and then (Attribute_Name (N) = Name_Access
                    or else
                  Attribute_Name (N) = Name_Unchecked_Access
                    or else
                  Attribute_Name (N) = Name_Unrestricted_Access)
      then
         return True;

      --  True if allocator

      elsif Nkind (N) = N_Allocator then
         return True;

      --  For a conversion, true if expression is known non-null

      elsif Nkind (N) = N_Type_Conversion then
         return Known_Non_Null (Expression (N));

      --  One more case is when Current_Value references a condition
      --  that ensures a non-null value.

      elsif Is_Entity_Name (N) then
         declare
            Op  : Node_Kind;
            Val : Node_Id;

         begin
            Get_Current_Value_Condition (N, Op, Val);
            return Op = N_Op_Ne and then Nkind (Val) = N_Null;
         end;

      --  Above are all cases where the value could be determined to be
      --  non-null. In all other cases, we don't know, so return False.

      else
         return False;
      end if;
   end Known_Non_Null;

   -----------------------------
   -- Make_CW_Equivalent_Type --
   -----------------------------

   --  Create a record type used as an equivalent of any member
   --  of the class which takes its size from exp.

   --  Generate the following code:

   --   type Equiv_T is record
   --     _parent :  T (List of discriminant constaints taken from Exp);
   --     Ext__50 : Storage_Array (1 .. (Exp'size - Typ'object_size)/8);
   --   end Equiv_T;
   --
   --   ??? Note that this type does not guarantee same alignment as all
   --   derived types

   function Make_CW_Equivalent_Type
     (T    : Entity_Id;
      E    : Node_Id)
      return Entity_Id
   is
      Loc         : constant Source_Ptr := Sloc (E);
      Root_Typ    : constant Entity_Id  := Root_Type (T);
      List_Def    : constant List_Id    := Empty_List;
      Equiv_Type  : Entity_Id;
      Range_Type  : Entity_Id;
      Str_Type    : Entity_Id;
      Constr_Root : Entity_Id;
      Sizexpr     : Node_Id;

   begin
      Constr_Root := Root_Typ;

      --  subtype rg__xx is Storage_Offset range
      --                           (Expr'size - typ'size) / Storage_Unit

      Range_Type := Make_Defining_Identifier (Loc, New_Internal_Name ('G'));

      Sizexpr :=
        Make_Op_Subtract (Loc,
          Left_Opnd =>
            Make_Attribute_Reference (Loc,
              Prefix =>
                OK_Convert_To (T, Duplicate_Subexpr_No_Checks (E)),
              Attribute_Name => Name_Size),
          Right_Opnd =>
            Make_Attribute_Reference (Loc,
              Prefix => New_Reference_To (Constr_Root, Loc),
              Attribute_Name => Name_Object_Size));

      Set_Paren_Count (Sizexpr, 1);

      Append_To (List_Def,
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Range_Type,
          Subtype_Indication =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (RTE (RE_Storage_Offset), Loc),
              Constraint => Make_Range_Constraint (Loc,
                Range_Expression =>
                  Make_Range (Loc,
                    Low_Bound => Make_Integer_Literal (Loc, 1),
                    High_Bound =>
                      Make_Op_Divide (Loc,
                        Left_Opnd => Sizexpr,
                        Right_Opnd => Make_Integer_Literal (Loc,
                            Intval => System_Storage_Unit)))))));

      --  subtype str__nn is Storage_Array (rg__x);

      Str_Type := Make_Defining_Identifier (Loc, New_Internal_Name ('S'));
      Append_To (List_Def,
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Str_Type,
          Subtype_Indication =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (RTE (RE_Storage_Array), Loc),
              Constraint =>
                Make_Index_Or_Discriminant_Constraint (Loc,
                  Constraints =>
                    New_List (New_Reference_To (Range_Type, Loc))))));

      --  type Equiv_T is record
      --    _parent : Tnn;
      --    E : Str_Type;
      --  end Equiv_T;

      Equiv_Type := Make_Defining_Identifier (Loc, New_Internal_Name ('T'));

      --  When the target requires front-end layout, it's necessary to allow
      --  the equivalent type to be frozen so that layout can occur (when the
      --  associated class-wide subtype is frozen, the equivalent type will
      --  be frozen, see freeze.adb). For other targets, Gigi wants to have
      --  the equivalent type marked as frozen and deals with this type itself.
      --  In the Gigi case this will also avoid the generation of an init
      --  procedure for the type.

      if not Frontend_Layout_On_Target then
         Set_Is_Frozen (Equiv_Type);
      end if;

      Set_Ekind (Equiv_Type, E_Record_Type);
      Set_Parent_Subtype (Equiv_Type, Constr_Root);

      Append_To (List_Def,
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Equiv_Type,

          Type_Definition =>
            Make_Record_Definition (Loc,
              Component_List => Make_Component_List (Loc,
                Component_Items => New_List (
                  Make_Component_Declaration (Loc,
                    Defining_Identifier =>
                      Make_Defining_Identifier (Loc, Name_uParent),
                    Component_Definition =>
                      Make_Component_Definition (Loc,
                        Aliased_Present    => False,
                        Subtype_Indication =>
                          New_Reference_To (Constr_Root, Loc))),

                  Make_Component_Declaration (Loc,
                    Defining_Identifier =>
                      Make_Defining_Identifier (Loc,
                        Chars => New_Internal_Name ('C')),
                    Component_Definition =>
                      Make_Component_Definition (Loc,
                        Aliased_Present    => False,
                        Subtype_Indication =>
                          New_Reference_To (Str_Type, Loc))))

                ))));

      Insert_Actions (E, List_Def);
      return Equiv_Type;
   end Make_CW_Equivalent_Type;

   ------------------------
   -- Make_Literal_Range --
   ------------------------

   function Make_Literal_Range
     (Loc         : Source_Ptr;
      Literal_Typ : Entity_Id)
      return        Node_Id
   is
      Lo : constant Node_Id :=
             New_Copy_Tree (String_Literal_Low_Bound (Literal_Typ));

   begin
      Set_Analyzed (Lo, False);

         return
           Make_Range (Loc,
             Low_Bound => Lo,

             High_Bound =>
               Make_Op_Subtract (Loc,
                  Left_Opnd =>
                    Make_Op_Add (Loc,
                      Left_Opnd  => New_Copy_Tree (Lo),
                      Right_Opnd =>
                        Make_Integer_Literal (Loc,
                          String_Literal_Length (Literal_Typ))),
                  Right_Opnd => Make_Integer_Literal (Loc, 1)));
   end Make_Literal_Range;

   ----------------------------
   -- Make_Subtype_From_Expr --
   ----------------------------

   --  1. If Expr is an uncontrained array expression, creates
   --    Unc_Type(Expr'first(1)..Expr'Last(1),..., Expr'first(n)..Expr'last(n))

   --  2. If Expr is a unconstrained discriminated type expression, creates
   --    Unc_Type(Expr.Discr1, ... , Expr.Discr_n)

   --  3. If Expr is class-wide, creates an implicit class wide subtype

   function Make_Subtype_From_Expr
     (E       : Node_Id;
      Unc_Typ : Entity_Id)
      return    Node_Id
   is
      Loc         : constant Source_Ptr := Sloc (E);
      List_Constr : constant List_Id    := New_List;
      D           : Entity_Id;

      Full_Subtyp  : Entity_Id;
      Priv_Subtyp  : Entity_Id;
      Utyp         : Entity_Id;
      Full_Exp     : Node_Id;

   begin
      if Is_Array_Type (Unc_Typ) then
         for J in 1 .. Number_Dimensions (Unc_Typ) loop
            Append_To (List_Constr,
              Make_Range (Loc,
                Low_Bound =>
                  Make_Attribute_Reference (Loc,
                    Prefix => Duplicate_Subexpr_No_Checks (E),
                    Attribute_Name => Name_First,
                    Expressions => New_List (
                      Make_Integer_Literal (Loc, J))),

                High_Bound =>
                  Make_Attribute_Reference (Loc,
                    Prefix         => Duplicate_Subexpr_No_Checks (E),
                    Attribute_Name => Name_Last,
                    Expressions    => New_List (
                      Make_Integer_Literal (Loc, J)))));
         end loop;

      elsif Is_Class_Wide_Type (Unc_Typ) then
         declare
            CW_Subtype : Entity_Id;
            EQ_Typ     : Entity_Id := Empty;

         begin
            --  A class-wide equivalent type is not needed when Java_VM
            --  because the JVM back end handles the class-wide object
            --  initialization itself (and doesn't need or want the
            --  additional intermediate type to handle the assignment).

            if Expander_Active and then not Java_VM then
               EQ_Typ := Make_CW_Equivalent_Type (Unc_Typ, E);
            end if;

            CW_Subtype := New_Class_Wide_Subtype (Unc_Typ, E);
            Set_Equivalent_Type (CW_Subtype, EQ_Typ);

            if Present (EQ_Typ) then
               Set_Is_Class_Wide_Equivalent_Type (EQ_Typ);
            end if;

            Set_Cloned_Subtype (CW_Subtype, Base_Type (Unc_Typ));

            return New_Occurrence_Of (CW_Subtype, Loc);
         end;
      end if;

      return
        Make_Subtype_Indication (Loc,
          Subtype_Mark => New_Reference_To (Unc_Typ, Loc),
          Constraint   =>
            Make_Index_Or_Discriminant_Constraint (Loc,
              Constraints => List_Constr));
   end Make_Subtype_From_Expr;

   -----------------------------
   -- May_Generate_Large_Temp --
   -----------------------------

   --  At the current time, the only types that we return False for (i.e.
   --  where we decide we know they cannot generate large temps) are ones
   --  where we know the size is 128 bits or less at compile time, and we
   --  are still not doing a thorough job on arrays and records ???

   function May_Generate_Large_Temp (Typ : Entity_Id) return Boolean is
   begin
      if not Stack_Checking_Enabled then
         return False;

      elsif not Size_Known_At_Compile_Time (Typ) then
         return False;

      elsif Esize (Typ) /= 0 and then Esize (Typ) <= 256 then
         return False;

      elsif Is_Array_Type (Typ)
        and then Present (Packed_Array_Type (Typ))
      then
         return May_Generate_Large_Temp (Packed_Array_Type (Typ));

      --  We could do more here to find other small types ???

      else
         return True;
      end if;
   end May_Generate_Large_Temp;

   ----------------------------
   -- New_Class_Wide_Subtype --
   ----------------------------

   function New_Class_Wide_Subtype
     (CW_Typ : Entity_Id;
      N      : Node_Id)
      return   Entity_Id
   is
      Res       : constant Entity_Id := Create_Itype (E_Void, N);
      Res_Name  : constant Name_Id   := Chars (Res);
      Res_Scope : constant Entity_Id := Scope (Res);

   begin
      Copy_Node (CW_Typ, Res);
      Set_Sloc (Res, Sloc (N));
      Set_Is_Itype (Res);
      Set_Associated_Node_For_Itype (Res, N);
      Set_Is_Public (Res, False);   --  By default, may be changed below.
      Set_Public_Status (Res);
      Set_Chars (Res, Res_Name);
      Set_Scope (Res, Res_Scope);
      Set_Ekind (Res, E_Class_Wide_Subtype);
      Set_Next_Entity (Res, Empty);
      Set_Etype (Res, Base_Type (CW_Typ));

      --  For targets where front-end layout is required, reset the Is_Frozen
      --  status of the subtype to False (it can be implicitly set to true
      --  from the copy of the class-wide type). For other targets, Gigi
      --  doesn't want the class-wide subtype to go through the freezing
      --  process (though it's unclear why that causes problems and it would
      --  be nice to allow freezing to occur normally for all targets ???).

      if Frontend_Layout_On_Target then
         Set_Is_Frozen (Res, False);
      end if;

      Set_Freeze_Node (Res, Empty);
      return (Res);
   end New_Class_Wide_Subtype;

   -------------------------
   -- Remove_Side_Effects --
   -------------------------

   procedure Remove_Side_Effects
     (Exp          : Node_Id;
      Name_Req     : Boolean := False;
      Variable_Ref : Boolean := False)
   is
      Loc          : constant Source_Ptr := Sloc (Exp);
      Exp_Type     : constant Entity_Id      := Etype (Exp);
      Svg_Suppress : constant Suppress_Array := Scope_Suppress;
      Def_Id       : Entity_Id;
      Ref_Type     : Entity_Id;
      Res          : Node_Id;
      Ptr_Typ_Decl : Node_Id;
      New_Exp      : Node_Id;
      E            : Node_Id;

      function Side_Effect_Free (N : Node_Id) return Boolean;
      --  Determines if the tree N represents an expession that is known
      --  not to have side effects, and for which no processing is required.

      function Side_Effect_Free (L : List_Id) return Boolean;
      --  Determines if all elements of the list L are side effect free

      function Safe_Prefixed_Reference (N : Node_Id) return Boolean;
      --  The argument N is a construct where the Prefix is dereferenced
      --  if it is a an access type and the result is a variable. The call
      --  returns True if the construct is side effect free (not considering
      --  side effects in other than the prefix which are to be tested by the
      --  caller).

      function Within_In_Parameter (N : Node_Id) return Boolean;
      --  Determines if N is a subcomponent of a composite in-parameter.
      --  If so, N is not side-effect free when the actual is global and
      --  modifiable indirectly from within a subprogram, because it may
      --  be passed by reference. The front-end must be conservative here
      --  and assume that this may happen with any array or record type.
      --  On the other hand, we cannot create temporaries for all expressions
      --  for which this condition is true, for various reasons that might
      --  require clearing up ??? For example, descriminant references that
      --  appear out of place, or spurious type errors with class-wide
      --  expressions. As a result, we limit the transformation to loop
      --  bounds, which is so far the only case that requires it.

      -----------------------------
      -- Safe_Prefixed_Reference --
      -----------------------------

      function Safe_Prefixed_Reference (N : Node_Id) return Boolean is
      begin
         --  If prefix is not side effect free, definitely not safe

         if not Side_Effect_Free (Prefix (N)) then
            return False;

         --  If the prefix is of an access type that is not access-to-constant,
         --  then this construct is a variable reference, which means it is to
         --  be considered to have side effects if Variable_Ref is set True
         --  Exception is an access to an entity that is a constant or an
         --  in-parameter which does not come from source, and is the result
         --  of a previous removal of side-effects.

         elsif Is_Access_Type (Etype (Prefix (N)))
           and then not Is_Access_Constant (Etype (Prefix (N)))
           and then Variable_Ref
         then
            if not Is_Entity_Name (Prefix (N)) then
               return False;
            else
               return Ekind (Entity (Prefix (N))) = E_Constant
                 or else Ekind (Entity (Prefix (N))) = E_In_Parameter;
            end if;

         --  The following test is the simplest way of solving a complex
         --  problem uncovered by BB08-010: Side effect on loop bound that
         --  is a subcomponent of a global variable:
         --    If a loop bound is a subcomponent of a global variable, a
         --    modification of that variable within the loop may incorrectly
         --    affect the execution of the loop.

         elsif not
           (Nkind (Parent (Parent (N))) /= N_Loop_Parameter_Specification
              or else not Within_In_Parameter (Prefix (N)))
         then
            return False;

         --  All other cases are side effect free

         else
            return True;
         end if;
      end Safe_Prefixed_Reference;

      ----------------------
      -- Side_Effect_Free --
      ----------------------

      function Side_Effect_Free (N : Node_Id) return Boolean is
      begin
         --  Note on checks that could raise Constraint_Error. Strictly, if
         --  we take advantage of 11.6, these checks do not count as side
         --  effects. However, we would just as soon consider that they are
         --  side effects, since the backend CSE does not work very well on
         --  expressions which can raise Constraint_Error. On the other
         --  hand, if we do not consider them to be side effect free, then
         --  we get some awkward expansions in -gnato mode, resulting in
         --  code insertions at a point where we do not have a clear model
         --  for performing the insertions. See 4908-002/comment for details.

         --  Special handling for entity names

         if Is_Entity_Name (N) then

            --  If the entity is a constant, it is definitely side effect
            --  free. Note that the test of Is_Variable (N) below might
            --  be expected to catch this case, but it does not, because
            --  this test goes to the original tree, and we may have
            --  already rewritten a variable node with a constant as
            --  a result of an earlier Force_Evaluation call.

            if Ekind (Entity (N)) = E_Constant
              or else Ekind (Entity (N)) = E_In_Parameter
            then
               return True;

            --  Functions are not side effect free

            elsif Ekind (Entity (N)) = E_Function then
               return False;

            --  Variables are considered to be a side effect if Variable_Ref
            --  is set or if we have a volatile variable and Name_Req is off.
            --  If Name_Req is True then we can't help returning a name which
            --  effectively allows multiple references in any case.

            elsif Is_Variable (N) then
               return not Variable_Ref
                 and then (not Treat_As_Volatile (Entity (N))
                             or else Name_Req);

            --  Any other entity (e.g. a subtype name) is definitely side
            --  effect free.

            else
               return True;
            end if;

         --  A value known at compile time is always side effect free

         elsif Compile_Time_Known_Value (N) then
            return True;
         end if;

         --  For other than entity names and compile time known values,
         --  check the node kind for special processing.

         case Nkind (N) is

            --  An attribute reference is side effect free if its expressions
            --  are side effect free and its prefix is side effect free or
            --  is an entity reference.

            --  Is this right? what about x'first where x is a variable???

            when N_Attribute_Reference =>
               return Side_Effect_Free (Expressions (N))
                 and then (Is_Entity_Name (Prefix (N))
                            or else Side_Effect_Free (Prefix (N)));

            --  A binary operator is side effect free if and both operands
            --  are side effect free. For this purpose binary operators
            --  include membership tests and short circuit forms

            when N_Binary_Op |
                 N_In        |
                 N_Not_In    |
                 N_And_Then  |
                 N_Or_Else
            =>
               return Side_Effect_Free (Left_Opnd  (N))
                 and then Side_Effect_Free (Right_Opnd (N));

            --  An explicit dereference is side effect free only if it is
            --  a side effect free prefixed reference.

            when N_Explicit_Dereference =>
               return Safe_Prefixed_Reference (N);

            --  A call to _rep_to_pos is side effect free, since we generate
            --  this pure function call ourselves. Moreover it is critically
            --  important to make this exception, since otherwise we can
            --  have discriminants in array components which don't look
            --  side effect free in the case of an array whose index type
            --  is an enumeration type with an enumeration rep clause.

            --  All other function calls are not side effect free

            when N_Function_Call =>
               return Nkind (Name (N)) = N_Identifier
                 and then Is_TSS (Name (N), TSS_Rep_To_Pos)
                 and then
                   Side_Effect_Free (First (Parameter_Associations (N)));

            --  An indexed component is side effect free if it is a side
            --  effect free prefixed reference and all the indexing
            --  expressions are side effect free.

            when N_Indexed_Component =>
               return Side_Effect_Free (Expressions (N))
                 and then Safe_Prefixed_Reference (N);

            --  A type qualification is side effect free if the expression
            --  is side effect free.

            when N_Qualified_Expression =>
               return Side_Effect_Free (Expression (N));

            --  A selected component is side effect free only if it is a
            --  side effect free prefixed reference.

            when N_Selected_Component =>
               return Safe_Prefixed_Reference (N);

            --  A range is side effect free if the bounds are side effect free

            when N_Range =>
               return Side_Effect_Free (Low_Bound (N))
                 and then Side_Effect_Free (High_Bound (N));

            --  A slice is side effect free if it is a side effect free
            --  prefixed reference and the bounds are side effect free.

            when N_Slice =>
               return Side_Effect_Free (Discrete_Range (N))
                 and then Safe_Prefixed_Reference (N);

            --  A type conversion is side effect free if the expression
            --  to be converted is side effect free.

            when N_Type_Conversion =>
               return Side_Effect_Free (Expression (N));

            --  A unary operator is side effect free if the operand
            --  is side effect free.

            when N_Unary_Op =>
               return Side_Effect_Free (Right_Opnd (N));

            --  An unchecked type conversion is side effect free only if it
            --  is safe and its argument is side effect free.

            when N_Unchecked_Type_Conversion =>
               return Safe_Unchecked_Type_Conversion (N)
                 and then Side_Effect_Free (Expression (N));

            --  An unchecked expression is side effect free if its expression
            --  is side effect free.

            when N_Unchecked_Expression =>
               return Side_Effect_Free (Expression (N));

            --  We consider that anything else has side effects. This is a bit
            --  crude, but we are pretty close for most common cases, and we
            --  are certainly correct (i.e. we never return True when the
            --  answer should be False).

            when others =>
               return False;
         end case;
      end Side_Effect_Free;

      --  A list is side effect free if all elements of the list are
      --  side effect free.

      function Side_Effect_Free (L : List_Id) return Boolean is
         N : Node_Id;

      begin
         if L = No_List or else L = Error_List then
            return True;

         else
            N := First (L);

            while Present (N) loop
               if not Side_Effect_Free (N) then
                  return False;
               else
                  Next (N);
               end if;
            end loop;

            return True;
         end if;
      end Side_Effect_Free;

      -------------------------
      -- Within_In_Parameter --
      -------------------------

      function Within_In_Parameter (N : Node_Id) return Boolean is
      begin
         if not Comes_From_Source (N) then
            return False;

         elsif Is_Entity_Name (N) then
            return
              Ekind (Entity (N)) = E_In_Parameter;

         elsif Nkind (N) = N_Indexed_Component
           or else Nkind (N) = N_Selected_Component
         then
            return Within_In_Parameter (Prefix (N));
         else

            return False;
         end if;
      end Within_In_Parameter;

   --  Start of processing for Remove_Side_Effects

   begin
      --  If we are side effect free already or expansion is disabled,
      --  there is nothing to do.

      if Side_Effect_Free (Exp) or else not Expander_Active then
         return;
      end if;

      --  All this must not have any checks

      Scope_Suppress := (others => True);

      --  If the expression has the form v.all then we can just capture
      --  the pointer, and then do an explicit dereference on the result.

      if Nkind (Exp) = N_Explicit_Dereference then
         Def_Id :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('R'));
         Res :=
           Make_Explicit_Dereference (Loc, New_Reference_To (Def_Id, Loc));

         Insert_Action (Exp,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Def_Id,
             Object_Definition   =>
               New_Reference_To (Etype (Prefix (Exp)), Loc),
             Constant_Present    => True,
             Expression          => Relocate_Node (Prefix (Exp))));

      --  Similar processing for an unchecked conversion of an expression
      --  of the form v.all, where we want the same kind of treatment.

      elsif Nkind (Exp) = N_Unchecked_Type_Conversion
        and then Nkind (Expression (Exp)) = N_Explicit_Dereference
      then
         Remove_Side_Effects (Expression (Exp), Variable_Ref);
         Scope_Suppress := Svg_Suppress;
         return;

      --  If this is a type conversion, leave the type conversion and remove
      --  the side effects in the expression. This is important in several
      --  circumstances: for change of representations, and also when this
      --  is a view conversion to a smaller object, where gigi can end up
      --  its own temporary of the wrong size.

      --  ??? this transformation is inhibited for elementary types that are
      --  not involved in a change of representation because it causes
      --  regressions that are not fully understood yet.

      elsif Nkind (Exp) = N_Type_Conversion
        and then (not Is_Elementary_Type (Underlying_Type (Exp_Type))
                   or else Nkind (Parent (Exp)) = N_Assignment_Statement)
      then
         Remove_Side_Effects (Expression (Exp), Variable_Ref);
         Scope_Suppress := Svg_Suppress;
         return;

      --  For expressions that denote objects, we can use a renaming scheme.
      --  We skip using this if we have a volatile variable and we do not
      --  have Nam_Req set true (see comments above for Side_Effect_Free).
      --  We also skip this scheme for class-wide expressions in order to
      --  avoid recursive expansion (see Expand_N_Object_Renaming_Declaration)
      --  If the object is a function call, we need to create a temporary and
      --  not a renaming.

      --  Note that we could use ordinary object declarations in the case of
      --  expressions not appearing as lvalues. That is left as a possible
      --  optimization in the future but we prefer to generate renamings
      --  right now, since we may indeed be transforming an lvalue.

      elsif Is_Object_Reference (Exp)
        and then Nkind (Exp) /= N_Function_Call
        and then not Variable_Ref
        and then (Name_Req
                   or else not Is_Entity_Name (Exp)
                   or else not Treat_As_Volatile (Entity (Exp)))
        and then not Is_Class_Wide_Type (Exp_Type)
      then
         Def_Id := Make_Defining_Identifier (Loc, New_Internal_Name ('R'));

         if Nkind (Exp) = N_Selected_Component
           and then Nkind (Prefix (Exp)) = N_Function_Call
           and then Is_Array_Type (Etype (Exp))
         then
            --  Avoid generating a variable-sized temporary, by generating
            --  the renaming declaration just for the function call. The
            --  transformation could be refined to apply only when the array
            --  component is constrained by a discriminant???

            Res :=
              Make_Selected_Component (Loc,
                Prefix => New_Occurrence_Of (Def_Id, Loc),
                Selector_Name => Selector_Name (Exp));

            Insert_Action (Exp,
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => Def_Id,
                Subtype_Mark        =>
                  New_Reference_To (Base_Type (Etype (Prefix (Exp))), Loc),
                Name                => Relocate_Node (Prefix (Exp))));

            --  The temporary must be elaborated by gigi, and is of course
            --  not to be replaced in-line by the expression it renames,
            --  which would defeat the purpose of removing the side-effect.

            Set_Is_Renaming_Of_Object (Def_Id, False);

         else
            Res := New_Reference_To (Def_Id, Loc);

            Insert_Action (Exp,
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => Def_Id,
                Subtype_Mark        => New_Reference_To (Exp_Type, Loc),
                Name                => Relocate_Node (Exp)));

            Set_Is_Renaming_Of_Object (Def_Id, False);
         end if;

      --  If it is a scalar type, just make a copy.

      elsif Is_Elementary_Type (Exp_Type) then
         Def_Id := Make_Defining_Identifier (Loc, New_Internal_Name ('R'));
         Set_Etype (Def_Id, Exp_Type);
         Res := New_Reference_To (Def_Id, Loc);

         E :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Def_Id,
             Object_Definition   => New_Reference_To (Exp_Type, Loc),
             Constant_Present    => True,
             Expression          => Relocate_Node (Exp));

         Set_Assignment_OK (E);
         Insert_Action (Exp, E);

      --  Always use a renaming for an unchecked conversion
      --  If this is an unchecked conversion that Gigi can't handle, make
      --  a copy or a use a renaming to capture the value.

      elsif Nkind (Exp) = N_Unchecked_Type_Conversion
        and then not Safe_Unchecked_Type_Conversion (Exp)
      then
            Def_Id := Make_Defining_Identifier (Loc, New_Internal_Name ('R'));
            Set_Etype (Def_Id, Exp_Type);
            Res := New_Reference_To (Def_Id, Loc);

            E :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Def_Id,
                Object_Definition   => New_Reference_To (Exp_Type, Loc),
                Constant_Present    => not Is_Variable (Exp),
                Expression          => Relocate_Node (Exp));

            Set_Assignment_OK (E);
            Insert_Action (Exp, E);


      --  Otherwise we generate a reference to the value

      else
         Ref_Type := Make_Defining_Identifier (Loc, New_Internal_Name ('A'));

         Ptr_Typ_Decl :=
           Make_Full_Type_Declaration (Loc,
             Defining_Identifier => Ref_Type,
             Type_Definition =>
               Make_Access_To_Object_Definition (Loc,
                 All_Present => True,
                 Subtype_Indication =>
                   New_Reference_To (Exp_Type, Loc)));

         E := Exp;
         Insert_Action (Exp, Ptr_Typ_Decl);

         Def_Id := Make_Defining_Identifier (Loc, New_Internal_Name ('R'));
         Set_Etype (Def_Id, Exp_Type);

         Res :=
           Make_Explicit_Dereference (Loc,
             Prefix => New_Reference_To (Def_Id, Loc));

         if Nkind (E) = N_Explicit_Dereference then
            New_Exp := Relocate_Node (Prefix (E));
         else
            E := Relocate_Node (E);
            New_Exp := Make_Reference (Loc, E);
         end if;

         if Nkind (E) = N_Aggregate and then Expansion_Delayed (E) then
            Set_Expansion_Delayed (E, False);
            Set_Analyzed (E, False);
         end if;

         Insert_Action (Exp,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Def_Id,
             Object_Definition   => New_Reference_To (Ref_Type, Loc),
             Expression          => New_Exp));
      end if;

      --  Preserve the Assignment_OK flag in all copies, since at least
      --  one copy may be used in a context where this flag must be set
      --  (otherwise why would the flag be set in the first place).

      Set_Assignment_OK (Res, Assignment_OK (Exp));

      --  Finally rewrite the original expression and we are done

      Rewrite (Exp, Res);
      Analyze_And_Resolve (Exp, Exp_Type);
      Scope_Suppress := Svg_Suppress;
   end Remove_Side_Effects;

   ------------------------------------
   -- Safe_Unchecked_Type_Conversion --
   ------------------------------------

   --  Note: this function knows quite a bit about the exact requirements
   --  of Gigi with respect to unchecked type conversions, and its code
   --  must be coordinated with any changes in Gigi in this area.

   --  The above requirements should be documented in Sinfo ???

   function Safe_Unchecked_Type_Conversion (Exp : Node_Id) return Boolean is
      Otyp   : Entity_Id;
      Ityp   : Entity_Id;
      Oalign : Uint;
      Ialign : Uint;
      Pexp   : constant Node_Id := Parent (Exp);

   begin
      --  If the expression is the RHS of an assignment or object declaration
      --   we are always OK because there will always be a target.

      --  Object renaming declarations, (generated for view conversions of
      --  actuals in inlined calls), like object declarations, provide an
      --  explicit type, and are safe as well.

      if (Nkind (Pexp) = N_Assignment_Statement
           and then Expression (Pexp) = Exp)
        or else Nkind (Pexp) = N_Object_Declaration
        or else Nkind (Pexp) = N_Object_Renaming_Declaration
      then
         return True;

      --  If the expression is the prefix of an N_Selected_Component
      --  we should also be OK because GCC knows to look inside the
      --  conversion except if the type is discriminated. We assume
      --  that we are OK anyway if the type is not set yet or if it is
      --  controlled since we can't afford to introduce a temporary in
      --  this case.

      elsif Nkind (Pexp) = N_Selected_Component
         and then Prefix (Pexp) = Exp
      then
         if No (Etype (Pexp)) then
            return True;
         else
            return Is_Constrained (Etype (Pexp));
         end if;
      end if;

      --  Set the output type, this comes from Etype if it is set, otherwise
      --  we take it from the subtype mark, which we assume was already
      --  fully analyzed.

      if Present (Etype (Exp)) then
         Otyp := Etype (Exp);
      else
         Otyp := Entity (Subtype_Mark (Exp));
      end if;

      --  The input type always comes from the expression, and we assume
      --  this is indeed always analyzed, so we can simply get the Etype.

      Ityp := Etype (Expression (Exp));

      --  Initialize alignments to unknown so far

      Oalign := No_Uint;
      Ialign := No_Uint;

      --  Replace a concurrent type by its corresponding record type
      --  and each type by its underlying type and do the tests on those.
      --  The original type may be a private type whose completion is a
      --  concurrent type, so find the underlying type first.

      if Present (Underlying_Type (Otyp)) then
         Otyp := Underlying_Type (Otyp);
      end if;

      if Present (Underlying_Type (Ityp)) then
         Ityp := Underlying_Type (Ityp);
      end if;

      --  If the base types are the same, we know there is no problem since
      --  this conversion will be a noop.

      if Implementation_Base_Type (Otyp) = Implementation_Base_Type (Ityp) then
         return True;

      --  If the size of output type is known at compile time, there is
      --  never a problem.  Note that unconstrained records are considered
      --  to be of known size, but we can't consider them that way here,
      --  because we are talking about the actual size of the object.

      --  We also make sure that in addition to the size being known, we do
      --  not have a case which might generate an embarrassingly large temp
      --  in stack checking mode.

      elsif Size_Known_At_Compile_Time (Otyp)
        and then not May_Generate_Large_Temp (Otyp)
        and then not (Is_Record_Type (Otyp) and then not Is_Constrained (Otyp))
      then
         return True;

      --  If either type is tagged, then we know the alignment is OK so
      --  Gigi will be able to use pointer punning.

      elsif Is_Tagged_Type (Otyp) or else Is_Tagged_Type (Ityp) then
         return True;

      --  Conversions to and from packed array types are always ignored and
      --  hence are safe.

      elsif Is_Packed_Array_Type (Otyp)
        or else Is_Packed_Array_Type (Ityp)
      then
         return True;
      end if;

      --  The only other cases known to be safe is if the input type's
      --  alignment is known to be at least the maximum alignment for the
      --  target or if both alignments are known and the output type's
      --  alignment is no stricter than the input's.  We can use the alignment
      --  of the component type of an array if a type is an unpacked
      --  array type.

      if Present (Alignment_Clause (Otyp)) then
         Oalign := Expr_Value (Expression (Alignment_Clause (Otyp)));

      elsif Is_Array_Type (Otyp)
        and then Present (Alignment_Clause (Component_Type (Otyp)))
      then
         Oalign := Expr_Value (Expression (Alignment_Clause
                                           (Component_Type (Otyp))));
      end if;

      if Present (Alignment_Clause (Ityp)) then
         Ialign := Expr_Value (Expression (Alignment_Clause (Ityp)));

      elsif Is_Array_Type (Ityp)
        and then Present (Alignment_Clause (Component_Type (Ityp)))
      then
         Ialign := Expr_Value (Expression (Alignment_Clause
                                           (Component_Type (Ityp))));
      end if;

      if Ialign /= No_Uint and then Ialign > Maximum_Alignment then
         return True;

      elsif Ialign /= No_Uint and then Oalign /= No_Uint
        and then Ialign <= Oalign
      then
         return True;

      --   Otherwise, Gigi cannot handle this and we must make a temporary.

      else
         return False;
      end if;

   end Safe_Unchecked_Type_Conversion;

   --------------------------
   -- Set_Elaboration_Flag --
   --------------------------

   procedure Set_Elaboration_Flag (N : Node_Id; Spec_Id : Entity_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Ent : constant Entity_Id  := Elaboration_Entity (Spec_Id);
      Asn : Node_Id;

   begin
      if Present (Ent) then

         --  Nothing to do if at the compilation unit level, because in this
         --  case the flag is set by the binder generated elaboration routine.

         if Nkind (Parent (N)) = N_Compilation_Unit then
            null;

         --  Here we do need to generate an assignment statement

         else
            Check_Restriction (No_Elaboration_Code, N);
            Asn :=
              Make_Assignment_Statement (Loc,
                Name       => New_Occurrence_Of (Ent, Loc),
                Expression => New_Occurrence_Of (Standard_True, Loc));

            if Nkind (Parent (N)) = N_Subunit then
               Insert_After (Corresponding_Stub (Parent (N)), Asn);
            else
               Insert_After (N, Asn);
            end if;

            Analyze (Asn);

            --  Kill current value indication. This is necessary because
            --  the tests of this flag are inserted out of sequence and must
            --  not pick up bogus indications of the wrong constant value.

            Set_Current_Value (Ent, Empty);
         end if;
      end if;
   end Set_Elaboration_Flag;

   --------------------------
   -- Target_Has_Fixed_Ops --
   --------------------------

   Integer_Sized_Small : Ureal;
   --  Set to 2.0 ** -(Integer'Size - 1) the first time that this
   --  function is called (we don't want to compute it more than once!)

   Long_Integer_Sized_Small : Ureal;
   --  Set to 2.0 ** -(Long_Integer'Size - 1) the first time that this
   --  functoin is called (we don't want to compute it more than once)

   First_Time_For_THFO : Boolean := True;
   --  Set to False after first call (if Fractional_Fixed_Ops_On_Target)

   function Target_Has_Fixed_Ops
     (Left_Typ   : Entity_Id;
      Right_Typ  : Entity_Id;
      Result_Typ : Entity_Id)
      return       Boolean
   is
      function Is_Fractional_Type (Typ : Entity_Id) return Boolean;
      --  Return True if the given type is a fixed-point type with a small
      --  value equal to 2 ** (-(T'Object_Size - 1)) and whose values have
      --  an absolute value less than 1.0. This is currently limited
      --  to fixed-point types that map to Integer or Long_Integer.

      ------------------------
      -- Is_Fractional_Type --
      ------------------------

      function Is_Fractional_Type (Typ : Entity_Id) return Boolean is
      begin
         if Esize (Typ) = Standard_Integer_Size then
            return Small_Value (Typ) = Integer_Sized_Small;

         elsif Esize (Typ) = Standard_Long_Integer_Size then
            return Small_Value (Typ) = Long_Integer_Sized_Small;

         else
            return False;
         end if;
      end Is_Fractional_Type;

   --  Start of processing for Target_Has_Fixed_Ops

   begin
      --  Return False if Fractional_Fixed_Ops_On_Target is false

      if not Fractional_Fixed_Ops_On_Target then
         return False;
      end if;

      --  Here the target has Fractional_Fixed_Ops, if first time, compute
      --  standard constants used by Is_Fractional_Type.

      if First_Time_For_THFO then
         First_Time_For_THFO := False;

         Integer_Sized_Small :=
           UR_From_Components
             (Num   => Uint_1,
              Den   => UI_From_Int (Standard_Integer_Size - 1),
              Rbase => 2);

         Long_Integer_Sized_Small :=
           UR_From_Components
             (Num   => Uint_1,
              Den   => UI_From_Int (Standard_Long_Integer_Size - 1),
              Rbase => 2);
      end if;

      --  Return True if target supports fixed-by-fixed multiply/divide
      --  for fractional fixed-point types (see Is_Fractional_Type) and
      --  the operand and result types are equivalent fractional types.

      return Is_Fractional_Type (Base_Type (Left_Typ))
        and then Is_Fractional_Type (Base_Type (Right_Typ))
        and then Is_Fractional_Type (Base_Type (Result_Typ))
        and then Esize (Left_Typ) = Esize (Right_Typ)
        and then Esize (Left_Typ) = Esize (Result_Typ);
   end Target_Has_Fixed_Ops;

   ------------------------------------------
   -- Type_May_Have_Bit_Aligned_Components --
   ------------------------------------------

   function Type_May_Have_Bit_Aligned_Components
     (Typ : Entity_Id) return Boolean
   is
   begin
      --  Array type, check component type

      if Is_Array_Type (Typ) then
         return
           Type_May_Have_Bit_Aligned_Components (Component_Type (Typ));

      --  Record type, check components

      elsif Is_Record_Type (Typ) then
         declare
            E : Entity_Id;

         begin
            E := First_Entity (Typ);
            while Present (E) loop
               if Ekind (E) = E_Component
               then
                  if Component_May_Be_Bit_Aligned (E)
                    or else
                      Type_May_Have_Bit_Aligned_Components (Etype (E))
                  then
                     return True;
                  end if;
               end if;

               Next_Entity (E);
            end loop;

            return False;
         end;

      --  Type other than array or record is always OK

      else
         return False;
      end if;
   end Type_May_Have_Bit_Aligned_Components;

end Exp_Util;