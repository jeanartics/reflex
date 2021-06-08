------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 2                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
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
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
-- with Exp_Smem; use Exp_Smem;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
--  with Exp_VFpt; use Exp_VFpt;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Sem;      use Sem;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_Ch2 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Expand_Current_Value (N : Node_Id);
   --  Given a node N for a variable whose Current_Value field is set.
   --  If the node is for a discrete type, replaces the node with a
   --  copy of the referenced value. This provides a limited form of
   --  value propagation for variables which are initialized and have
   --  not been modified at the time of reference. The call has no
   --  effect if the Current_Value refers to a conditional with a
   --  condition other than equality.

   procedure Expand_Entity_Reference (N : Node_Id);
   --  Common processing for expansion of identifiers and expanded names

   procedure Expand_Renaming (N : Node_Id);
   --  For renamings, just replace the identifier by the corresponding
   --  name expression. Note that this has been evaluated (see routine
   --  Exp_Ch8.Expand_N_Object_Renaming.Evaluate_Name) so this gives
   --  the correct renaming semantics.

   --------------------------
   -- Expand_Current_Value --
   --------------------------

   procedure Expand_Current_Value (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      E   : constant Entity_Id  := Entity (N);
      CV  : constant Node_Id    := Current_Value (E);
      T   : constant Entity_Id  := Etype (N);
      Val : Node_Id;
      Op  : Node_Kind;

      function In_Appropriate_Scope return Boolean;
      --  Returns true if the current scope is the scope of E, or is a nested
      --  (to any level) package declaration, package body, or block of this
      --  scope. The idea is that such references are in the sequential
      --  execution sequence of statements executed after E is elaborated.

      --------------------------
      -- In_Appropriate_Scope --
      --------------------------

      function In_Appropriate_Scope return Boolean is
         ES : constant Entity_Id := Scope (E);
         CS : Entity_Id;

      begin
         CS := Current_Scope;

         loop
            --  If we are in right scope, replacement is safe

            if CS = ES then
               return True;

            --  Packages do not affect the determination of safety

            elsif Ekind (CS) = E_Package then
               CS := Scope (CS);
               exit when CS = Standard_Standard;

            --  Blocks do not affect the determination of safety

            elsif Ekind (CS) = E_Block then
               CS := Scope (CS);

            --  Otherwise, the reference is dubious, and we cannot be
            --  sure that it is safe to do the replacement. Note in
            --  particular, in a loop (except for the special case
            --  tested above), we cannot safely do a replacement since
            --  there may be an assignment at the bottom of the loop
            --  that will affect a reference at the top of the loop.

            else
               exit;
            end if;
         end loop;

         return False;
      end In_Appropriate_Scope;

   --  Start of processing for Expand_Current_Value

   begin
      if True

         --  Do this only for discrete types

         and then Is_Discrete_Type (T)

         --  Do not replace biased types, since it is problematic to
         --  consistently generate a sensible constant value in this case.

         and then not Has_Biased_Representation (T)

         --  Do not replace lvalues

         and then not Is_Lvalue (N)

         --  Do not replace occurrences that are not in the current scope,
         --  because in a nested subprogram we know absolutely nothing about
         --  the sequence of execution.

         and then In_Appropriate_Scope

         --  Do not replace statically allocated objects, because they may
         --  be modified outside the current scope.

         and then not Is_Statically_Allocated (E)

         --  Do not replace aliased or volatile objects, since we don't know
         --  what else might change the value

         and then not Is_Aliased (E) and then not Treat_As_Volatile (E)

         --  Debug flag -gnatdM disconnects this optimization

         and then not Debug_Flag_MM

         --  Do not replace occurrences in pragmas (where names typically
         --  appear not as values, but as simply names. If there are cases
         --  where values are required, it is only a very minor efficiency
         --  issue that they do not get replaced when they could be).

         and then Nkind (Parent (N)) /= N_Pragma_Argument_Association
      then
         --  Case of Current_Value is a compile time known value

         if Nkind (CV) in N_Subexpr then
            Val := CV;

         --  Case of Current_Value is a conditional expression reference

         else
            Get_Current_Value_Condition (N, Op, Val);

            if Op /= N_Op_Eq then
               return;
            end if;
         end if;

         --  If constant value is an occurrence of an enumeration literal,
         --  then we just make another occurence of the same literal.

         if Is_Entity_Name (Val)
           and then Ekind (Entity (Val)) = E_Enumeration_Literal
         then
            Rewrite (N,
              Unchecked_Convert_To (T,
                New_Occurrence_Of (Entity (Val), Loc)));

         --  Otherwise get the value, and convert to appropriate type

         else
            Rewrite (N,
              Unchecked_Convert_To (T,
                Make_Integer_Literal (Loc,
                  Intval => Expr_Rep_Value (Val))));
         end if;

         Analyze_And_Resolve (N, T);
         Set_Is_Static_Expression (N, False);
      end if;
   end Expand_Current_Value;

   -----------------------------
   -- Expand_Entity_Reference --
   -----------------------------

   procedure Expand_Entity_Reference (N : Node_Id) is
      E : constant Entity_Id := Entity (N);

   begin
      --  Defend against errors

      if No (E) and then Total_Errors_Detected /= 0 then
         return;
      end if;

      if Is_Renaming_Of_Object (E) then
         Expand_Renaming (N);

      elsif (Ekind (E) = E_Variable
               or else
             Ekind (E) = E_In_Out_Parameter
               or else
             Ekind (E) = E_Out_Parameter)
        and then Present (Current_Value (E))
        and then Nkind (Current_Value (E)) /= N_Raise_Constraint_Error
      then
         Expand_Current_Value (N);

         --  We do want to warn for the case of a boolean variable (not
         --  a boolean constant) whose value is known at compile time.

         if Is_Boolean_Type (Etype (N)) then
            Warn_On_Known_Condition (N);
         end if;
      end if;
   end Expand_Entity_Reference;

   ----------------------------
   -- Expand_N_Expanded_Name --
   ----------------------------

   procedure Expand_N_Expanded_Name (N : Node_Id) is
   begin
      Expand_Entity_Reference (N);
   end Expand_N_Expanded_Name;

   -------------------------
   -- Expand_N_Identifier --
   -------------------------

   procedure Expand_N_Identifier (N : Node_Id) is
   begin
      Expand_Entity_Reference (N);
   end Expand_N_Identifier;

   ---------------------------
   -- Expand_N_Real_Literal --
   ---------------------------

   procedure Expand_N_Real_Literal (N : Node_Id) is
   begin
      null;
   end Expand_N_Real_Literal;

   ---------------------
   -- Expand_Renaming --
   ---------------------

   procedure Expand_Renaming (N : Node_Id) is
      E : constant Entity_Id := Entity (N);
      T : constant Entity_Id := Etype (N);

   begin
      Rewrite (N, New_Copy_Tree (Renamed_Object (E)));

      --  We mark the copy as unanalyzed, so that it is sure to be
      --  reanalyzed at the top level. This is needed in the packed
      --  case since we specifically avoided expanding packed array
      --  references when the renaming declaration was analyzed.

      Reset_Analyzed_Flags (N);
      Analyze_And_Resolve (N, T);
   end Expand_Renaming;

   ------------------
   -- Param_Entity --
   ------------------

   --  This would be trivial, simply a test for an identifier that was a
   --  reference to a formal, if it were not for the fact that a previous
   --  call to Expand_Entry_Parameter will have modified the reference
   --  to the identifier. A formal of a protected entity is rewritten as

   --    typ!(recobj).rec.all'Constrained

   --  where rec is a selector whose Entry_Formal link points to the formal
   --  For a formal of a task entity, the formal is rewritten as a local
   --  renaming.

   function Param_Entity (N : Node_Id) return Entity_Id is
   begin
      --  Simple reference case

      if Nkind (N) = N_Identifier then
         if Is_Formal (Entity (N)) then
            return Entity (N);

         end if;

      else
         if Nkind (N) = N_Explicit_Dereference then
            declare
               P : constant Node_Id := Prefix (N);
               S : Node_Id;

            begin
               if Nkind (P) = N_Selected_Component then
                  S := Selector_Name (P);
               end if;
            end;
         end if;
      end if;

      return (Empty);
   end Param_Entity;

end Exp_Ch2;
