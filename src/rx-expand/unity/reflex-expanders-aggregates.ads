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

package Reflex.Expanders.Aggregates is
   
   function Has_Default_Init_Comps (N : Node_Id) return Boolean;
   --  N is an aggregate (record or array). Checks the presence of default
   --  initialization (<>) in any component (Ada0Y: AI-287)

   function Backend_Processing_Possible (N : Node_Id) return Boolean;
   --  This function checks if array aggregate N can be processed directly
   --  by Gigi. If this is the case True is returned.

   procedure Convert_To_Positional
     (N                    : Node_Id;
      Max_Others_Replicate : Nat     := 5;
      Handle_Bit_Packed    : Boolean := False);
   --  If possible, convert named notation to positional notation. This
   --  conversion is possible only in some static cases. If the conversion
   --  is possible, then N is rewritten with the analyzed converted
   --  aggregate. The parameter Max_Others_Replicate controls the maximum
   --  number of values corresponding to an others choice that will be
   --  converted to positional notation (the default of 5 is the normal
   --  limit, and reflects the fact that normally the loop is better than
   --  a lot of separate assignments). Note that this limit gets overridden
   --  in any case if either of the restrictions No_Elaboration_Code or
   --  No_Implicit_Loops is set. The parameter Handle_Bit_Packed is usually
   --  set False (since we do not expect the back end to handle bit packed
   --  arrays, so the normal case of conversion is pointless), but in the
   --  special case of a call from Packed_Array_Aggregate_Handled, we set
   --  this parameter to True, since these are cases we handle in there.
   
   function Build_Array_Aggr_Code
     (N           : Node_Id;
      Ctype       : Entity_Id;
      Index       : Node_Id;
      Into        : Node_Id;
      Scalar_Comp : Boolean;
      Indices     : List_Id := No_List;
      Flist       : Node_Id := Empty) return List_Id;
   --  This recursive routine returns a list of statements containing the
   --  loops and assignments that are needed for the expansion of the array
   --  aggregate N.
   --
   --    N is the (sub-)aggregate node to be expanded into code. This node
   --    has been fully analyzed, and its Etype is properly set.
   --
   --    Index is the index node corresponding to the array sub-aggregate N.
   --
   --    Into is the target expression into which we are copying the aggregate.
   --    Note that this node may not have been analyzed yet, and so the Etype
   --    field may not be set.
   --
   --    Scalar_Comp is True if the component type of the aggregate is scalar.
   --
   --    Indices is the current list of expressions used to index the
   --    object we are writing into.
   --
   --    Flist is an expression representing the finalization list on which
   --    to attach the controlled components if any.
   
   
   ----------------------------
   -- Has_Default_Init_Comps --
   ----------------------------

   function Has_Default_Init_Comps (N : Node_Id) return Boolean is
      Comps : constant List_Id := Component_Associations (N);
      C     : Node_Id;
      Expr  : Node_Id;
   begin
      pragma Assert (Nkind (N) = N_Aggregate
         or else Nkind (N) = N_Extension_Aggregate);

      if No (Comps) then
         return False;
      end if;

      --  Check if any direct component has default initialized components

      C := First (Comps);
      while Present (C) loop
         if Box_Present (C) then
            return True;
         end if;

         Next (C);
      end loop;

      --  Recursive call in case of aggregate expression

      C := First (Comps);
      while Present (C) loop
         Expr := Expression (C);

         if Present (Expr)
           and then (Nkind (Expr) = N_Aggregate
                     or else Nkind (Expr) = N_Extension_Aggregate)
           and then Has_Default_Init_Comps (Expr)
         then
            return True;
         end if;

         Next (C);
      end loop;

      return False;
   end Has_Default_Init_Comps;

   ---------------------------------
   -- Backend_Processing_Possible --
   ---------------------------------

   --  Backend processing by Gigi/gcc is possible only if all the following
   --  conditions are met:

   --    1. N is fully positional

   --    2. N is not a bit-packed array aggregate;

   --    3. The size of N's array type must be known at compile time. Note
   --       that this implies that the component size is also known

   --    4. The array type of N does not follow the Fortran layout convention
   --       or if it does it must be 1 dimensional.

   --    5. The array component type is tagged, which may necessitate
   --       reassignment of proper tags.

   --    6. The array component type might have unaligned bit components

   function Backend_Processing_Possible (N : Node_Id) return Boolean is
      Typ : constant Entity_Id := Etype (N);
      --  Typ is the correct constrained array subtype of the aggregate.

      function Static_Check (N : Node_Id; Index : Node_Id) return Boolean;
      --  Recursively checks that N is fully positional, returns true if so.

      ------------------
      -- Static_Check --
      ------------------

      function Static_Check (N : Node_Id; Index : Node_Id) return Boolean is
         Expr : Node_Id;

      begin
         --  Check for component associations

         if Present (Component_Associations (N)) then
            return False;
         end if;

         --  Recurse to check subaggregates, which may appear in qualified
         --  expressions. If delayed, the front-end will have to expand.

         Expr := First (Expressions (N));

         while Present (Expr) loop

            if Is_Delayed_Aggregate (Expr) then
               return False;
            end if;

            if Present (Next_Index (Index))
               and then not Static_Check (Expr, Next_Index (Index))
            then
               return False;
            end if;

            Next (Expr);
         end loop;

         return True;
      end Static_Check;

   --  Start of processing for Backend_Processing_Possible

   begin
      --  Checks 4 (array must not be multi-dimensional Fortran case)

      if Convention (Typ) = Convention_Fortran
        and then Number_Dimensions (Typ) > 1
      then
         return False;
      end if;

      --  Checks 3 (size of array must be known at compile time)

      if not Size_Known_At_Compile_Time (Typ) then
         return False;
      end if;

      --  Checks 1 (aggregate must be fully positional)

      if not Static_Check (N, First_Index (Typ)) then
         return False;
      end if;

      --  Checks 5 (if the component type is tagged, then we may need
      --    to do tag adjustments; perhaps this should be refined to
      --    check for any component associations that actually
      --    need tag adjustment, along the lines of the test that's
      --    done in Has_Delayed_Nested_Aggregate_Or_Tagged_Comps
      --    for record aggregates with tagged components, but not
      --    clear whether it's worthwhile ???; in the case of the
      --    JVM, object tags are handled implicitly)

      if Is_Tagged_Type (Component_Type (Typ)) and then not Java_VM then
         return False;
      end if;

      --  Checks 6 (component type must not have bit aligned components)

      if Type_May_Have_Bit_Aligned_Components (Component_Type (Typ)) then
         return False;
      end if;

      --  Backend processing is possible

      Set_Compile_Time_Known_Aggregate (N, True);
      Set_Size_Known_At_Compile_Time (Etype (N), True);
      return True;
   end Backend_Processing_Possible;

   ---------------------------
   -- Build_Array_Aggr_Code --
   ---------------------------

   --  The code that we generate from a one dimensional aggregate is

   --  1. If the sub-aggregate contains discrete choices we

   --     (a) Sort the discrete choices

   --     (b) Otherwise for each discrete choice that specifies a range we
   --         emit a loop. If a range specifies a maximum of three values, or
   --         we are dealing with an expression we emit a sequence of
   --         assignments instead of a loop.

   --     (c) Generate the remaining loops to cover the others choice if any.

   --  2. If the aggregate contains positional elements we

   --     (a) translate the positional elements in a series of assignments.

   --     (b) Generate a final loop to cover the others choice if any.
   --         Note that this final loop has to be a while loop since the case

   --             L : Integer := Integer'Last;
   --             H : Integer := Integer'Last;
   --             A : array (L .. H) := (1, others =>0);

   --         cannot be handled by a for loop. Thus for the following

   --             array (L .. H) := (.. positional elements.., others =>E);

   --         we always generate something like:

   --             J : Index_Type := Index_Of_Last_Positional_Element;
   --             while J < H loop
   --                J := Index_Base'Succ (J)
   --                Tmp (J) := E;
   --             end loop;

   function Build_Array_Aggr_Code
     (N           : Node_Id;
      Ctype       : Entity_Id;
      Index       : Node_Id;
      Into        : Node_Id;
      Scalar_Comp : Boolean;
      Indices     : List_Id := No_List;
      Flist       : Node_Id := Empty) return List_Id
   is
      Loc          : constant Source_Ptr := Sloc (N);
      Index_Base   : constant Entity_Id  := Base_Type (Etype (Index));
      Index_Base_L : constant Node_Id := Type_Low_Bound (Index_Base);
      Index_Base_H : constant Node_Id := Type_High_Bound (Index_Base);

      function Add (Val : Int; To : Node_Id) return Node_Id;
      --  Returns an expression where Val is added to expression To,
      --  unless To+Val is provably out of To's base type range.
      --  To must be an already analyzed expression.

      function Empty_Range (L, H : Node_Id) return Boolean;
      --  Returns True if the range defined by L .. H is certainly empty.

      function Equal (L, H : Node_Id) return Boolean;
      --  Returns True if L = H for sure.

      function Index_Base_Name return Node_Id;
      --  Returns a new reference to the index type name.

      function Gen_Assign (Ind : Node_Id; Expr : Node_Id) return List_Id;
      --  Ind must be a side-effect free expression. If the input aggregate
      --  N to Build_Loop contains no sub-aggregates, then this function
      --  returns the assignment statement:
      --
      --     Into (Indices, Ind) := Expr;
      --
      --  Otherwise we call Build_Code recursively.
      --
      --  Ada0Y (AI-287): In case of default initialized component, Expr is
      --  empty and we generate a call to the corresponding IP subprogram.

      function Gen_Loop (L, H : Node_Id; Expr : Node_Id) return List_Id;
      --  Nodes L and H must be side-effect free expressions.
      --  If the input aggregate N to Build_Loop contains no sub-aggregates,
      --  This routine returns the for loop statement
      --
      --     for J in Index_Base'(L) .. Index_Base'(H) loop
      --        Into (Indices, J) := Expr;
      --     end loop;
      --
      --  Otherwise we call Build_Code recursively.
      --  As an optimization if the loop covers 3 or less scalar elements we
      --  generate a sequence of assignments.

      function Gen_While (L, H : Node_Id; Expr : Node_Id) return List_Id;
      --  Nodes L and H must be side-effect free expressions.
      --  If the input aggregate N to Build_Loop contains no sub-aggregates,
      --  This routine returns the while loop statement
      --
      --     J : Index_Base := L;
      --     while J < H loop
      --        J := Index_Base'Succ (J);
      --        Into (Indices, J) := Expr;
      --     end loop;
      --
      --  Otherwise we call Build_Code recursively

      function Local_Compile_Time_Known_Value (E : Node_Id) return Boolean;
      function Local_Expr_Value               (E : Node_Id) return Uint;
      --  These two Local routines are used to replace the corresponding ones
      --  in sem_eval because while processing the bounds of an aggregate with
      --  discrete choices whose index type is an enumeration, we build static
      --  expressions not recognized by Compile_Time_Known_Value as such since
      --  they have not yet been analyzed and resolved. All the expressions in
      --  question are things like Index_Base_Name'Val (Const) which we can
      --  easily recognize as being constant.

      ---------
      -- Add --
      ---------

      function Add (Val : Int; To : Node_Id) return Node_Id is
         Expr_Pos : Node_Id;
         Expr     : Node_Id;
         To_Pos   : Node_Id;
         U_To     : Uint;
         U_Val    : constant Uint := UI_From_Int (Val);

      begin
         --  Note: do not try to optimize the case of Val = 0, because
         --  we need to build a new node with the proper Sloc value anyway.

         --  First test if we can do constant folding

         if Local_Compile_Time_Known_Value (To) then
            U_To := Local_Expr_Value (To) + Val;

            --  Determine if our constant is outside the range of the index.
            --  If so return an Empty node. This empty node will be caught
            --  by Empty_Range below.

            if Compile_Time_Known_Value (Index_Base_L)
              and then U_To < Expr_Value (Index_Base_L)
            then
               return Empty;

            elsif Compile_Time_Known_Value (Index_Base_H)
              and then U_To > Expr_Value (Index_Base_H)
            then
               return Empty;
            end if;

            Expr_Pos := Make_Integer_Literal (Loc, U_To);
            Set_Is_Static_Expression (Expr_Pos);

            if not Is_Enumeration_Type (Index_Base) then
               Expr := Expr_Pos;

            --  If we are dealing with enumeration return
            --     Index_Base'Val (Expr_Pos)

            else
               Expr :=
                 Make_Attribute_Reference
                   (Loc,
                    Prefix         => Index_Base_Name,
                    Attribute_Name => Name_Val,
                    Expressions    => New_List (Expr_Pos));
            end if;

            return Expr;
         end if;

         --  If we are here no constant folding possible

         if not Is_Enumeration_Type (Index_Base) then
            Expr :=
              Make_Op_Add (Loc,
                           Left_Opnd  => Duplicate_Subexpr (To),
                           Right_Opnd => Make_Integer_Literal (Loc, U_Val));

         --  If we are dealing with enumeration return
         --    Index_Base'Val (Index_Base'Pos (To) + Val)

         else
            To_Pos :=
              Make_Attribute_Reference
                (Loc,
                 Prefix         => Index_Base_Name,
                 Attribute_Name => Name_Pos,
                 Expressions    => New_List (Duplicate_Subexpr (To)));

            Expr_Pos :=
              Make_Op_Add (Loc,
                           Left_Opnd  => To_Pos,
                           Right_Opnd => Make_Integer_Literal (Loc, U_Val));

            Expr :=
              Make_Attribute_Reference
                (Loc,
                 Prefix         => Index_Base_Name,
                 Attribute_Name => Name_Val,
                 Expressions    => New_List (Expr_Pos));
         end if;

         return Expr;
      end Add;

      -----------------
      -- Empty_Range --
      -----------------

      function Empty_Range (L, H : Node_Id) return Boolean is
         Is_Empty : Boolean := False;
         Low      : Node_Id;
         High     : Node_Id;

      begin
         --  First check if L or H were already detected as overflowing the
         --  index base range type by function Add above. If this is so Add
         --  returns the empty node.

         if No (L) or else No (H) then
            return True;
         end if;

         for J in 1 .. 3 loop
            case J is

               --  L > H    range is empty

               when 1 =>
                  Low  := L;
                  High := H;

               --  B_L > H  range must be empty

               when 2 =>
                  Low  := Index_Base_L;
                  High := H;

               --  L > B_H  range must be empty

               when 3 =>
                  Low  := L;
                  High := Index_Base_H;
            end case;

            if Local_Compile_Time_Known_Value (Low)
              and then Local_Compile_Time_Known_Value (High)
            then
               Is_Empty :=
                 UI_Gt (Local_Expr_Value (Low), Local_Expr_Value (High));
            end if;

            exit when Is_Empty;
         end loop;

         return Is_Empty;
      end Empty_Range;

      -----------
      -- Equal --
      -----------

      function Equal (L, H : Node_Id) return Boolean is
      begin
         if L = H then
            return True;

         elsif Local_Compile_Time_Known_Value (L)
           and then Local_Compile_Time_Known_Value (H)
         then
            return UI_Eq (Local_Expr_Value (L), Local_Expr_Value (H));
         end if;

         return False;
      end Equal;

      ----------------
      -- Gen_Assign --
      ----------------

      function Gen_Assign (Ind : Node_Id; Expr : Node_Id) return List_Id is
         L : constant List_Id := New_List;
         F : Entity_Id;
         A : Node_Id;

         New_Indices  : List_Id;
         Indexed_Comp : Node_Id;
         Expr_Q       : Node_Id;
         Comp_Type    : Entity_Id := Empty;

         function Add_Loop_Actions (Lis : List_Id) return List_Id;
         --  Collect insert_actions generated in the construction of a
         --  loop, and prepend them to the sequence of assignments to
         --  complete the eventual body of the loop.

         ----------------------
         -- Add_Loop_Actions --
         ----------------------

         function Add_Loop_Actions (Lis : List_Id) return List_Id is
            Res : List_Id;

         begin
            --  Ada0Y (AI-287): Do nothing else in case of default initialized
            --  component

            if not Present (Expr) then
               return Lis;

            elsif Nkind (Parent (Expr)) = N_Component_Association
              and then Present (Loop_Actions (Parent (Expr)))
            then
               Append_List (Lis, Loop_Actions (Parent (Expr)));
               Res := Loop_Actions (Parent (Expr));
               Set_Loop_Actions (Parent (Expr), No_List);
               return Res;

            else
               return Lis;
            end if;
         end Add_Loop_Actions;

      --  Start of processing for Gen_Assign

      begin
         if No (Indices) then
            New_Indices := New_List;
         else
            New_Indices := New_Copy_List_Tree (Indices);
         end if;

         Append_To (New_Indices, Ind);

         if Present (Flist) then
            F := New_Copy_Tree (Flist);

         else
            F := Empty;
         end if;

         if Present (Next_Index (Index)) then
            return
              Add_Loop_Actions (
                Build_Array_Aggr_Code
                  (N           => Expr,
                   Ctype       => Ctype,
                   Index       => Next_Index (Index),
                   Into        => Into,
                   Scalar_Comp => Scalar_Comp,
                   Indices     => New_Indices,
                   Flist       => F));
         end if;

         --  If we get here then we are at a bottom-level (sub-)aggregate

         Indexed_Comp :=
           Checks_Off
             (Make_Indexed_Component (Loc,
                Prefix      => New_Copy_Tree (Into),
                Expressions => New_Indices));

         Set_Assignment_OK (Indexed_Comp);

         --  Ada0Y (AI-287): In case of default initialized component, Expr
         --  is not present (and therefore we also initialize Expr_Q to empty)

         if not Present (Expr) then
            Expr_Q := Empty;
         elsif Nkind (Expr) = N_Qualified_Expression then
            Expr_Q := Expression (Expr);
         else
            Expr_Q := Expr;
         end if;

         if Present (Etype (N))
           and then Etype (N) /= Any_Composite
         then
            Comp_Type := Component_Type (Etype (N));
            pragma Assert (Comp_Type = Ctype); --  AI-287

         elsif Present (Next (First (New_Indices))) then

            --  Ada0Y (AI-287): Do nothing in case of default initialized
            --  component because we have received the component type in
            --  the formal parameter Ctype.
            --  ??? I have added some assert pragmas to check if this new
            --      formal can be used to replace this code in all cases.

            if Present (Expr) then

               --  This is a multidimensional array. Recover the component
               --  type from the outermost aggregate, because subaggregates
               --  do not have an assigned type.

               declare
                  P : Node_Id := Parent (Expr);

               begin
                  while Present (P) loop

                     if Nkind (P) = N_Aggregate
                       and then Present (Etype (P))
                     then
                        Comp_Type := Component_Type (Etype (P));
                        exit;

                     else
                        P := Parent (P);
                     end if;
                  end loop;
                  pragma Assert (Comp_Type = Ctype); --  AI-287
               end;
            end if;
         end if;

         --  Ada0Y (AI-287): We only analyze the expression in case of non
         --  default initialized components (otherwise Expr_Q is not present)

         if Present (Expr_Q)
           and then (Nkind (Expr_Q) = N_Aggregate
                     or else Nkind (Expr_Q) = N_Extension_Aggregate)
         then
            --  At this stage the Expression may not have been
            --  analyzed yet because the array aggregate code has not
            --  been updated to use the Expansion_Delayed flag and
            --  avoid analysis altogether to solve the same problem
            --  (see Resolve_Aggr_Expr) so let's do the analysis of
            --  non-array aggregates now in order to get the value of
            --  Expansion_Delayed flag for the inner aggregate ???

            if Present (Comp_Type) and then not Is_Array_Type (Comp_Type) then
               Analyze_And_Resolve (Expr_Q, Comp_Type);
            end if;

            if Is_Delayed_Aggregate (Expr_Q) then
               return
                 Add_Loop_Actions (
                   Late_Expansion (Expr_Q, Etype (Expr_Q), Indexed_Comp, F));
            end if;
         end if;

         --  Ada0Y (AI-287): In case of default initialized component, call
         --  the initialization subprogram associated with the component type

         if not Present (Expr) then

            Append_List_To (L,
                 Build_Initialization_Call (Loc,
                   Id_Ref            => Indexed_Comp,
                   Typ               => Ctype,
                   With_Default_Init => True));

         else

            --  Now generate the assignment with no associated controlled
            --  actions since the target of the assignment may not have
            --  been initialized, it is not possible to Finalize it as
            --  expected by normal controlled assignment. The rest of the
            --  controlled actions are done manually with the proper
            --  finalization list coming from the context.

            A :=
              Make_OK_Assignment_Statement (Loc,
                Name       => Indexed_Comp,
                Expression => New_Copy_Tree (Expr));

            Append_To (L, A);

            --  Adjust the tag if tagged (because of possible view
            --  conversions), unless compiling for the Java VM
            --  where tags are implicit.

            if Present (Comp_Type)
              and then Is_Tagged_Type (Comp_Type)
              and then not Java_VM
            then
               A :=
                 Make_OK_Assignment_Statement (Loc,
                   Name =>
                     Make_Selected_Component (Loc,
                       Prefix =>  New_Copy_Tree (Indexed_Comp),
                       Selector_Name =>
                         New_Reference_To (Tag_Component (Comp_Type), Loc)),

                   Expression =>
                     Unchecked_Convert_To (RTE (RE_Tag),
                       New_Reference_To (
                         Access_Disp_Table (Comp_Type), Loc)));

               Append_To (L, A);
            end if;

            --  Adjust and Attach the component to the proper final list
            --  which can be the controller of the outer record object or
            --  the final list associated with the scope


         end if;

         return Add_Loop_Actions (L);
      end Gen_Assign;

      --------------
      -- Gen_Loop --
      --------------

      function Gen_Loop (L, H : Node_Id; Expr : Node_Id) return List_Id is
         L_J : Node_Id;

         L_Range : Node_Id;
         --  Index_Base'(L) .. Index_Base'(H)

         L_Iteration_Scheme : Node_Id;
         --  L_J in Index_Base'(L) .. Index_Base'(H)

         L_Body : List_Id;
         --  The statements to execute in the loop

         S : constant List_Id := New_List;
         --  List of statements

         Tcopy : Node_Id;
         --  Copy of expression tree, used for checking purposes

      begin
         --  If loop bounds define an empty range return the null statement

         if Empty_Range (L, H) then
            Append_To (S, Make_Null_Statement (Loc));

            --  Ada0Y (AI-287): Nothing else need to be done in case of
            --  default initialized component

            if not Present (Expr) then
               null;

            else
               --  The expression must be type-checked even though no component
               --  of the aggregate will have this value. This is done only for
               --  actual components of the array, not for subaggregates. Do
               --  the check on a copy, because the expression may be shared
               --  among several choices, some of which might be non-null.

               if Present (Etype (N))
                 and then Is_Array_Type (Etype (N))
                 and then No (Next_Index (Index))
               then
                  Expander_Mode_Save_And_Set (False);
                  Tcopy := New_Copy_Tree (Expr);
                  Set_Parent (Tcopy, N);
                  Analyze_And_Resolve (Tcopy, Component_Type (Etype (N)));
                  Expander_Mode_Restore;
               end if;
            end if;

            return S;

         --  If loop bounds are the same then generate an assignment

         elsif Equal (L, H) then
            return Gen_Assign (New_Copy_Tree (L), Expr);

         --  If H - L <= 2 then generate a sequence of assignments
         --  when we are processing the bottom most aggregate and it contains
         --  scalar components.

         elsif No (Next_Index (Index))
           and then Scalar_Comp
           and then Local_Compile_Time_Known_Value (L)
           and then Local_Compile_Time_Known_Value (H)
           and then Local_Expr_Value (H) - Local_Expr_Value (L) <= 2
         then

            Append_List_To (S, Gen_Assign (New_Copy_Tree (L), Expr));
            Append_List_To (S, Gen_Assign (Add (1, To => L), Expr));

            if Local_Expr_Value (H) - Local_Expr_Value (L) = 2 then
               Append_List_To (S, Gen_Assign (Add (2, To => L), Expr));
            end if;

            return S;
         end if;

         --  Otherwise construct the loop, starting with the loop index L_J

         L_J := Make_Defining_Identifier (Loc, New_Internal_Name ('J'));

         --  Construct "L .. H"

         L_Range :=
           Make_Range
             (Loc,
              Low_Bound  => Make_Qualified_Expression
                              (Loc,
                               Subtype_Mark => Index_Base_Name,
                               Expression   => L),
              High_Bound => Make_Qualified_Expression
                              (Loc,
                               Subtype_Mark => Index_Base_Name,
                               Expression => H));

         --  Construct "for L_J in Index_Base range L .. H"

         L_Iteration_Scheme :=
           Make_Iteration_Scheme
             (Loc,
              Loop_Parameter_Specification =>
                Make_Loop_Parameter_Specification
                  (Loc,
                   Defining_Identifier         => L_J,
                   Discrete_Subtype_Definition => L_Range));

         --  Construct the statements to execute in the loop body

         L_Body := Gen_Assign (New_Reference_To (L_J, Loc), Expr);

         --  Construct the final loop

         Append_To (S, Make_Implicit_Loop_Statement
                         (Node             => N,
                          Identifier       => Empty,
                          Iteration_Scheme => L_Iteration_Scheme,
                          Statements       => L_Body));

         return S;
      end Gen_Loop;

      ---------------
      -- Gen_While --
      ---------------

      --  The code built is

      --     W_J : Index_Base := L;
      --     while W_J < H loop
      --        W_J := Index_Base'Succ (W);
      --        L_Body;
      --     end loop;

      function Gen_While (L, H : Node_Id; Expr : Node_Id) return List_Id is
         W_J : Node_Id;

         W_Decl : Node_Id;
         --  W_J : Base_Type := L;

         W_Iteration_Scheme : Node_Id;
         --  while W_J < H

         W_Index_Succ : Node_Id;
         --  Index_Base'Succ (J)

         W_Increment : Node_Id;
         --  W_J := Index_Base'Succ (W)

         W_Body : constant List_Id := New_List;
         --  The statements to execute in the loop

         S : constant List_Id := New_List;
         --  list of statement

      begin
         --  If loop bounds define an empty range or are equal return null

         if Empty_Range (L, H) or else Equal (L, H) then
            Append_To (S, Make_Null_Statement (Loc));
            return S;
         end if;

         --  Build the decl of W_J

         W_J    := Make_Defining_Identifier (Loc, New_Internal_Name ('J'));
         W_Decl :=
           Make_Object_Declaration
             (Loc,
              Defining_Identifier => W_J,
              Object_Definition   => Index_Base_Name,
              Expression          => L);

         --  Theoretically we should do a New_Copy_Tree (L) here, but we know
         --  that in this particular case L is a fresh Expr generated by
         --  Add which we are the only ones to use.

         Append_To (S, W_Decl);

         --  Construct " while W_J < H"

         W_Iteration_Scheme :=
           Make_Iteration_Scheme
             (Loc,
              Condition => Make_Op_Lt
                             (Loc,
                              Left_Opnd  => New_Reference_To (W_J, Loc),
                              Right_Opnd => New_Copy_Tree (H)));

         --  Construct the statements to execute in the loop body

         W_Index_Succ :=
           Make_Attribute_Reference
             (Loc,
              Prefix         => Index_Base_Name,
              Attribute_Name => Name_Succ,
              Expressions    => New_List (New_Reference_To (W_J, Loc)));

         W_Increment  :=
           Make_OK_Assignment_Statement
             (Loc,
              Name       => New_Reference_To (W_J, Loc),
              Expression => W_Index_Succ);

         Append_To (W_Body, W_Increment);
         Append_List_To (W_Body,
           Gen_Assign (New_Reference_To (W_J, Loc), Expr));

         --  Construct the final loop

         Append_To (S, Make_Implicit_Loop_Statement
                         (Node             => N,
                          Identifier       => Empty,
                          Iteration_Scheme => W_Iteration_Scheme,
                          Statements       => W_Body));

         return S;
      end Gen_While;

      ---------------------
      -- Index_Base_Name --
      ---------------------

      function Index_Base_Name return Node_Id is
      begin
         return New_Reference_To (Index_Base, Sloc (N));
      end Index_Base_Name;

      ------------------------------------
      -- Local_Compile_Time_Known_Value --
      ------------------------------------

      function Local_Compile_Time_Known_Value (E : Node_Id) return Boolean is
      begin
         return Compile_Time_Known_Value (E)
           or else
             (Nkind (E) = N_Attribute_Reference
               and then Attribute_Name (E) = Name_Val
               and then Compile_Time_Known_Value (First (Expressions (E))));
      end Local_Compile_Time_Known_Value;

      ----------------------
      -- Local_Expr_Value --
      ----------------------

      function Local_Expr_Value (E : Node_Id) return Uint is
      begin
         if Compile_Time_Known_Value (E) then
            return Expr_Value (E);
         else
            return Expr_Value (First (Expressions (E)));
         end if;
      end Local_Expr_Value;

      --  Build_Array_Aggr_Code Variables

      Assoc  : Node_Id;
      Choice : Node_Id;
      Expr   : Node_Id;
      Typ    : Entity_Id;

      Others_Expr         : Node_Id := Empty;
      Others_Mbox_Present : Boolean := False;

      Aggr_L : constant Node_Id := Low_Bound (Aggregate_Bounds (N));
      Aggr_H : constant Node_Id := High_Bound (Aggregate_Bounds (N));
      --  The aggregate bounds of this specific sub-aggregate. Note that if
      --  the code generated by Build_Array_Aggr_Code is executed then these
      --  bounds are OK. Otherwise a Constraint_Error would have been raised.

      Aggr_Low  : constant Node_Id := Duplicate_Subexpr_No_Checks (Aggr_L);
      Aggr_High : constant Node_Id := Duplicate_Subexpr_No_Checks (Aggr_H);
      --  After Duplicate_Subexpr these are side-effect free.

      Low        : Node_Id;
      High       : Node_Id;

      Nb_Choices : Nat := 0;
      Table      : Case_Table_Type (1 .. Number_Of_Choices (N));
      --  Used to sort all the different choice values

      Nb_Elements : Int;
      --  Number of elements in the positional aggregate

      New_Code : constant List_Id := New_List;

   --  Start of processing for Build_Array_Aggr_Code

   begin
      --  First before we start, a special case. if we have a bit packed
      --  array represented as a modular type, then clear the value to
      --  zero first, to ensure that unused bits are properly cleared.

      Typ := Etype (N);

      --  We can skip this
      --  STEP 1: Process component associations
      --  For those associations that may generate a loop, initialize
      --  Loop_Actions to collect inserted actions that may be crated.

      if No (Expressions (N)) then

         --  STEP 1 (a): Sort the discrete choices

         Assoc := First (Component_Associations (N));
         while Present (Assoc) loop
            Choice := First (Choices (Assoc));
            while Present (Choice) loop
               if Nkind (Choice) = N_Others_Choice then
                  Set_Loop_Actions (Assoc, New_List);

                  if Box_Present (Assoc) then
                     Others_Mbox_Present := True;
                  else
                     Others_Expr := Expression (Assoc);
                  end if;
                  exit;
               end if;

               Get_Index_Bounds (Choice, Low, High);

               if Low /= High then
                  Set_Loop_Actions (Assoc, New_List);
               end if;

               Nb_Choices := Nb_Choices + 1;
               if Box_Present (Assoc) then
                  Table (Nb_Choices) := (Choice_Lo   => Low,
                                         Choice_Hi   => High,
                                         Choice_Node => Empty);
               else
                  Table (Nb_Choices) := (Choice_Lo   => Low,
                                         Choice_Hi   => High,
                                         Choice_Node => Expression (Assoc));
               end if;
               Next (Choice);
            end loop;

            Next (Assoc);
         end loop;

         --  If there is more than one set of choices these must be static
         --  and we can therefore sort them. Remember that Nb_Choices does not
         --  account for an others choice.

         if Nb_Choices > 1 then
            Sort_Case_Table (Table);
         end if;

         --  STEP 1 (b):  take care of the whole set of discrete choices.

         for J in 1 .. Nb_Choices loop
            Low  := Table (J).Choice_Lo;
            High := Table (J).Choice_Hi;
            Expr := Table (J).Choice_Node;
            Append_List (Gen_Loop (Low, High, Expr), To => New_Code);
         end loop;

         --  STEP 1 (c): generate the remaining loops to cover others choice
         --  We don't need to generate loops over empty gaps, but if there is
         --  a single empty range we must analyze the expression for semantics

         if Present (Others_Expr) or else Others_Mbox_Present then
            declare
               First : Boolean := True;

            begin
               for J in 0 .. Nb_Choices loop
                  if J = 0 then
                     Low := Aggr_Low;
                  else
                     Low := Add (1, To => Table (J).Choice_Hi);
                  end if;

                  if J = Nb_Choices then
                     High := Aggr_High;
                  else
                     High := Add (-1, To => Table (J + 1).Choice_Lo);
                  end if;

                  --  If this is an expansion within an init proc, make
                  --  sure that discriminant references are replaced by
                  --  the corresponding discriminal.

                  if First
                    or else not Empty_Range (Low, High)
                  then
                     First := False;
                     Append_List
                       (Gen_Loop (Low, High, Others_Expr), To => New_Code);
                  end if;
               end loop;
            end;
         end if;

      --  STEP 2: Process positional components

      else
         --  STEP 2 (a): Generate the assignments for each positional element
         --  Note that here we have to use Aggr_L rather than Aggr_Low because
         --  Aggr_L is analyzed and Add wants an analyzed expression.

         Expr        := First (Expressions (N));
         Nb_Elements := -1;

         while Present (Expr) loop
            Nb_Elements := Nb_Elements + 1;
            Append_List (Gen_Assign (Add (Nb_Elements, To => Aggr_L), Expr),
                         To => New_Code);
            Next (Expr);
         end loop;

         --  STEP 2 (b): Generate final loop if an others choice is present
         --  Here Nb_Elements gives the offset of the last positional element.

         if Present (Component_Associations (N)) then
            Assoc := Last (Component_Associations (N));

            --  Ada0Y (AI-287)
            if Box_Present (Assoc) then
               Append_List (Gen_While (Add (Nb_Elements, To => Aggr_L),
                                       Aggr_High,
                                       Empty),
                            To => New_Code);
            else
               Expr  := Expression (Assoc);

               Append_List (Gen_While (Add (Nb_Elements, To => Aggr_L),
                                       Aggr_High,
                                       Expr), --  AI-287
                            To => New_Code);
            end if;
         end if;
      end if;

      return New_Code;
   end Build_Array_Aggr_Code;

   
   ---------------------------
   -- Convert_To_Positional --
   ---------------------------

   procedure Convert_To_Positional
     (N                    : Node_Id;
      Max_Others_Replicate : Nat     := 5;
      Handle_Bit_Packed    : Boolean := False)
   is
      Typ : constant Entity_Id := Etype (N);

      function Flatten
        (N   : Node_Id;
         Ix  : Node_Id;
         Ixb : Node_Id) return Boolean;
      --  Convert the aggregate into a purely positional form if possible.

      function Is_Flat (N : Node_Id; Dims : Int) return Boolean;
      --  Non trivial for multidimensional aggregate.

      -------------
      -- Flatten --
      -------------

      function Flatten
        (N   : Node_Id;
         Ix  : Node_Id;
         Ixb : Node_Id) return Boolean
      is
         Loc : constant Source_Ptr := Sloc (N);
         Blo : constant Node_Id    := Type_Low_Bound (Etype (Ixb));
         Lo  : constant Node_Id    := Type_Low_Bound (Etype (Ix));
         Hi  : constant Node_Id    := Type_High_Bound (Etype (Ix));
         Lov : Uint;
         Hiv : Uint;

         --  The following constant determines the maximum size of an
         --  aggregate produced by converting named to positional
         --  notation (e.g. from others clauses). This avoids running
         --  away with attempts to convert huge aggregates.

         --  The normal limit is 5000, but we increase this limit to
         --  2**24 (about 16 million) if Restrictions (No_Elaboration_Code)
         --  or Restrictions (No_Implicit_Loops) is specified, since in
         --  either case, we are at risk of declaring the program illegal
         --  because of this limit.

         Max_Aggr_Size : constant Nat :=
            5000 + (2 ** 24 - 5000) * Boolean'Pos
                              (Restrictions (No_Elaboration_Code)
                                 or else
                               Restrictions (No_Implicit_Loops));
      begin

         if Nkind (Original_Node (N)) = N_String_Literal then
            return True;
         end if;

         --  Bounds need to be known at compile time

         if not Compile_Time_Known_Value (Lo)
           or else not Compile_Time_Known_Value (Hi)
         then
            return False;
         end if;

         --  Get bounds and check reasonable size (positive, not too large)
         --  Also only handle bounds starting at the base type low bound
         --  for now since the compiler isn't able to handle different low
         --  bounds yet. Case such as new String'(3..5 => ' ') will get
         --  the wrong bounds, though it seems that the aggregate should
         --  retain the bounds set on its Etype (see C64103E and CC1311B).

         Lov := Expr_Value (Lo);
         Hiv := Expr_Value (Hi);

         if Hiv < Lov
           or else (Hiv - Lov > Max_Aggr_Size)
           or else not Compile_Time_Known_Value (Blo)
           or else (Lov /= Expr_Value (Blo))
         then
            return False;
         end if;

         --  Bounds must be in integer range (for array Vals below)

         if not UI_Is_In_Int_Range (Lov)
             or else
            not UI_Is_In_Int_Range (Hiv)
         then
            return False;
         end if;

         --  Determine if set of alternatives is suitable for conversion
         --  and build an array containing the values in sequence.

         declare
            Vals : array (UI_To_Int (Lov) .. UI_To_Int (Hiv))
                     of Node_Id := (others => Empty);
            --  The values in the aggregate sorted appropriately

            Vlist : List_Id;
            --  Same data as Vals in list form

            Rep_Count : Nat;
            --  Used to validate Max_Others_Replicate limit

            Elmt   : Node_Id;
            Num    : Int := UI_To_Int (Lov);
            Choice : Node_Id;
            Lo, Hi : Node_Id;

         begin
            if Present (Expressions (N)) then
               Elmt := First (Expressions (N));

               while Present (Elmt) loop
                  if Nkind (Elmt) = N_Aggregate
                    and then Present (Next_Index (Ix))
                    and then
                         not Flatten (Elmt, Next_Index (Ix), Next_Index (Ixb))
                  then
                     return False;
                  end if;

                  Vals (Num) := Relocate_Node (Elmt);
                  Num := Num + 1;

                  Next (Elmt);
               end loop;
            end if;

            if No (Component_Associations (N)) then
               return True;
            end if;

            Elmt := First (Component_Associations (N));

            if Nkind (Expression (Elmt)) = N_Aggregate then
               if Present (Next_Index (Ix))
                 and then
                   not Flatten
                        (Expression (Elmt), Next_Index (Ix), Next_Index (Ixb))
               then
                  return False;
               end if;
            end if;

            Component_Loop : while Present (Elmt) loop
               Choice := First (Choices (Elmt));
               Choice_Loop : while Present (Choice) loop

                  --  If we have an others choice, fill in the missing elements
                  --  subject to the limit established by Max_Others_Replicate.

                  if Nkind (Choice) = N_Others_Choice then
                     Rep_Count := 0;

                     for J in Vals'Range loop
                        if No (Vals (J)) then
                           Vals (J) := New_Copy_Tree (Expression (Elmt));
                           Rep_Count := Rep_Count + 1;

                           --  Check for maximum others replication. Note that
                           --  we skip this test if either of the restrictions
                           --  No_Elaboration_Code or No_Implicit_Loops is
                           --  active, or if this is a preelaborable unit.

                           declare
                              P : constant Entity_Id :=
                                    Cunit_Entity (Current_Sem_Unit);

                           begin
                              if Restrictions (No_Elaboration_Code)
                                or else Restrictions (No_Implicit_Loops)
                                or else Is_Preelaborated (P)
                                or else (Ekind (P) = E_Package_Body
                                          and then
                                            Is_Preelaborated (Spec_Entity (P)))
                              then
                                 null;
                              elsif Rep_Count > Max_Others_Replicate then
                                 return False;
                              end if;
                           end;
                        end if;
                     end loop;

                     exit Component_Loop;

                  --  Case of a subtype mark

                  elsif Nkind (Choice) = N_Identifier
                    and then Is_Type (Entity (Choice))
                  then
                     Lo := Type_Low_Bound  (Etype (Choice));
                     Hi := Type_High_Bound (Etype (Choice));

                  --  Case of subtype indication

                  elsif Nkind (Choice) = N_Subtype_Indication then
                     Lo := Low_Bound  (Range_Expression (Constraint (Choice)));
                     Hi := High_Bound (Range_Expression (Constraint (Choice)));

                  --  Case of a range

                  elsif Nkind (Choice) = N_Range then
                     Lo := Low_Bound (Choice);
                     Hi := High_Bound (Choice);

                  --  Normal subexpression case

                  else pragma Assert (Nkind (Choice) in N_Subexpr);
                     if not Compile_Time_Known_Value (Choice) then
                        return False;

                     else
                        Vals (UI_To_Int (Expr_Value (Choice))) :=
                          New_Copy_Tree (Expression (Elmt));
                        goto Continue;
                     end if;
                  end if;

                  --  Range cases merge with Lo,Hi said

                  if not Compile_Time_Known_Value (Lo)
                       or else
                     not Compile_Time_Known_Value (Hi)
                  then
                     return False;
                  else
                     for J in UI_To_Int (Expr_Value (Lo)) ..
                              UI_To_Int (Expr_Value (Hi))
                     loop
                        Vals (J) := New_Copy_Tree (Expression (Elmt));
                     end loop;
                  end if;

               <<Continue>>
                  Next (Choice);
               end loop Choice_Loop;

               Next (Elmt);
            end loop Component_Loop;

            --  If we get here the conversion is possible

            Vlist := New_List;
            for J in Vals'Range loop
               Append (Vals (J), Vlist);
            end loop;

            Rewrite (N, Make_Aggregate (Loc, Expressions => Vlist));
            Set_Aggregate_Bounds (N, Aggregate_Bounds (Original_Node (N)));
            return True;
         end;
      end Flatten;

      -------------
      -- Is_Flat --
      -------------

      function Is_Flat (N : Node_Id; Dims : Int) return Boolean is
         Elmt : Node_Id;

      begin
         if Dims = 0 then
            return True;

         elsif Nkind (N) = N_Aggregate then
            if Present (Component_Associations (N)) then
               return False;

            else
               Elmt := First (Expressions (N));

               while Present (Elmt) loop
                  if not Is_Flat (Elmt, Dims - 1) then
                     return False;
                  end if;

                  Next (Elmt);
               end loop;

               return True;
            end if;
         else
            return True;
         end if;
      end Is_Flat;

   --  Start of processing for Convert_To_Positional

   begin
      --  Ada0Y (AI-287): Do not convert in case of default initialized
      --  components because in this case will need to call the corresponding
      --  IP procedure.

      if Has_Default_Init_Comps (N) then
         return;
      end if;

      if Is_Flat (N, Number_Dimensions (Typ)) then
         return;
      end if;

      --  Do not convert to positional if controlled components are
      --  involved since these require special processing

      if Has_Controlled_Component (Typ) then
         return;
      end if;

      if Flatten (N, First_Index (Typ), First_Index (Base_Type (Typ))) then
         Analyze_And_Resolve (N, Typ);
      end if;
   end Convert_To_Positional;

   ----------------------------
   -- Expand_Array_Aggregate --
   ----------------------------

   --  Array aggregate expansion proceeds as follows:

   --  1. If requested we generate code to perform all the array aggregate
   --     bound checks, specifically

   --         (a) Check that the index range defined by aggregate bounds is
   --             compatible with corresponding index subtype.

   --         (b) If an others choice is present check that no aggregate
   --             index is outside the bounds of the index constraint.

   --         (c) For multidimensional arrays make sure that all subaggregates
   --             corresponding to the same dimension have the same bounds.

   --  2. Check for packed array aggregate which can be converted to a
   --     constant so that the aggregate disappeares completely.

   --  3. Check case of nested aggregate. Generally nested aggregates are
   --     handled during the processing of the parent aggregate.

   --  4. Check if the aggregate can be statically processed. If this is the
   --     case pass it as is to Gigi. Note that a necessary condition for
   --     static processing is that the aggregate be fully positional.

   --  5. If in place aggregate expansion is possible (i.e. no need to create
   --     a temporary) then mark the aggregate as such and return. Otherwise
   --     create a new temporary and generate the appropriate initialization
   --     code.

   procedure Expand_Array_Aggregate (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      Typ  : constant Entity_Id := Etype (N);
      Ctyp : constant Entity_Id := Component_Type (Typ);
      --  Typ is the correct constrained array subtype of the aggregate
      --  Ctyp is the corresponding component type.

      Aggr_Dimension : constant Pos := Number_Dimensions (Typ);
      --  Number of aggregate index dimensions.

      Aggr_Low  : array (1 .. Aggr_Dimension) of Node_Id;
      Aggr_High : array (1 .. Aggr_Dimension) of Node_Id;
      --  Low and High bounds of the constraint for each aggregate index.

      Aggr_Index_Typ : array (1 .. Aggr_Dimension) of Entity_Id;
      --  The type of each index.

      Maybe_In_Place_OK : Boolean;
      --  If the type is neither controlled nor packed and the aggregate
      --  is the expression in an assignment, assignment in place may be
      --  possible, provided other conditions are met on the LHS.

      Others_Present : array (1 .. Aggr_Dimension) of Boolean :=
                         (others => False);
      --  If Others_Present (J) is True, then there is an others choice
      --  in one of the sub-aggregates of N at dimension J.

      procedure Build_Constrained_Type (Positional : Boolean);
      --  If the subtype is not static or unconstrained, build a constrained
      --  type using the computable sizes of the aggregate and its sub-
      --  aggregates.

      procedure Check_Bounds (Aggr_Bounds : Node_Id; Index_Bounds : Node_Id);
      --  Checks that the bounds of Aggr_Bounds are within the bounds defined
      --  by Index_Bounds.

      procedure Compute_Others_Present (Sub_Aggr : Node_Id; Dim : Pos);
      --  Computes the values of array Others_Present. Sub_Aggr is the
      --  array sub-aggregate we start the computation from. Dim is the
      --  dimension corresponding to the sub-aggregate.

      function Has_Address_Clause (D : Node_Id) return Boolean;
      --  If the aggregate is the expression in an object declaration, it
      --  cannot be expanded in place. This function does a lookahead in the
      --  current declarative part to find an address clause for the object
      --  being declared.

      function In_Place_Assign_OK return Boolean;
      --  Simple predicate to determine whether an aggregate assignment can
      --  be done in place, because none of the new values can depend on the
      --  components of the target of the assignment.

      function Must_Slide (N : Node_Id; Typ : Entity_Id) return Boolean;
      --  A static aggregate in an object declaration can in most cases be
      --  expanded in place. The one exception is when the aggregate is given
      --  with component associations that specify different bounds from those
      --  of the type definition in the object declaration. In this rather
      --  pathological case the aggregate must slide, and we must introduce
      --  an intermediate temporary to hold it.

      ----------------------------
      -- Build_Constrained_Type --
      ----------------------------

      procedure Build_Constrained_Type (Positional : Boolean) is
         Loc      : constant Source_Ptr := Sloc (N);
         Agg_Type : Entity_Id;
         Comp     : Node_Id;
         Decl     : Node_Id;
         Typ      : constant Entity_Id := Etype (N);
         Indices  : constant List_Id   := New_List;
         Num      : Int;
         Sub_Agg  : Node_Id;

      begin
         Agg_Type :=
           Make_Defining_Identifier (
             Loc, New_Internal_Name ('A'));

         --  If the aggregate is purely positional, all its subaggregates
         --  have the same size. We collect the dimensions from the first
         --  subaggregate at each level.

         if Positional then
            Sub_Agg := N;

            for D in 1 .. Number_Dimensions (Typ) loop
               Comp := First (Expressions (Sub_Agg));

               Sub_Agg := Comp;
               Num := 0;

               while Present (Comp) loop
                  Num := Num + 1;
                  Next (Comp);
               end loop;

               Append (
                 Make_Range (Loc,
                   Low_Bound => Make_Integer_Literal (Loc, 1),
                   High_Bound =>
                          Make_Integer_Literal (Loc, Num)),
                 Indices);
            end loop;

         else
            --  We know the aggregate type is unconstrained and the
            --  aggregate is not processable by the back end, therefore
            --  not necessarily positional. Retrieve the bounds of each
            --  dimension as computed earlier.

            for D in 1 .. Number_Dimensions (Typ) loop
               Append (
                 Make_Range (Loc,
                    Low_Bound  => Aggr_Low  (D),
                    High_Bound => Aggr_High (D)),
                 Indices);
            end loop;
         end if;

         Decl :=
           Make_Full_Type_Declaration (Loc,
               Defining_Identifier => Agg_Type,
               Type_Definition =>
                 Make_Constrained_Array_Definition (Loc,
                   Discrete_Subtype_Definitions => Indices,
                   Component_Definition =>
                     Make_Component_Definition (Loc,
                       Aliased_Present => False,
                       Subtype_Indication =>
                         New_Occurrence_Of (Component_Type (Typ), Loc))));

         Insert_Action (N, Decl);
         Analyze (Decl);
         Set_Etype (N, Agg_Type);
         Set_Is_Itype (Agg_Type);
         Freeze_Itype (Agg_Type, N);
      end Build_Constrained_Type;

      ------------------
      -- Check_Bounds --
      ------------------

      procedure Check_Bounds (Aggr_Bounds : Node_Id; Index_Bounds : Node_Id) is
         Aggr_Lo : Node_Id;
         Aggr_Hi : Node_Id;

         Ind_Lo  : Node_Id;
         Ind_Hi  : Node_Id;

         Cond    : Node_Id := Empty;

      begin
         Get_Index_Bounds (Aggr_Bounds, Aggr_Lo, Aggr_Hi);
         Get_Index_Bounds (Index_Bounds, Ind_Lo, Ind_Hi);

         --  Generate the following test:
         --
         --    [constraint_error when
         --      Aggr_Lo <= Aggr_Hi and then
         --        (Aggr_Lo < Ind_Lo or else Aggr_Hi > Ind_Hi)]
         --
         --  As an optimization try to see if some tests are trivially vacuos
         --  because we are comparing an expression against itself.

         if Aggr_Lo = Ind_Lo and then Aggr_Hi = Ind_Hi then
            Cond := Empty;

         elsif Aggr_Hi = Ind_Hi then
            Cond :=
              Make_Op_Lt (Loc,
                Left_Opnd  => Duplicate_Subexpr_Move_Checks (Aggr_Lo),
                Right_Opnd => Duplicate_Subexpr_Move_Checks (Ind_Lo));

         elsif Aggr_Lo = Ind_Lo then
            Cond :=
              Make_Op_Gt (Loc,
                Left_Opnd  => Duplicate_Subexpr_Move_Checks (Aggr_Hi),
                Right_Opnd => Duplicate_Subexpr_Move_Checks (Ind_Hi));

         else
            Cond :=
              Make_Or_Else (Loc,
                Left_Opnd =>
                  Make_Op_Lt (Loc,
                    Left_Opnd  => Duplicate_Subexpr_Move_Checks (Aggr_Lo),
                    Right_Opnd => Duplicate_Subexpr_Move_Checks (Ind_Lo)),

                Right_Opnd =>
                  Make_Op_Gt (Loc,
                    Left_Opnd  => Duplicate_Subexpr (Aggr_Hi),
                    Right_Opnd => Duplicate_Subexpr (Ind_Hi)));
         end if;

         if Present (Cond) then
            Cond :=
              Make_And_Then (Loc,
                Left_Opnd =>
                  Make_Op_Le (Loc,
                    Left_Opnd  => Duplicate_Subexpr_Move_Checks (Aggr_Lo),
                    Right_Opnd => Duplicate_Subexpr_Move_Checks (Aggr_Hi)),

                Right_Opnd => Cond);

            Set_Analyzed (Left_Opnd  (Left_Opnd (Cond)), False);
            Set_Analyzed (Right_Opnd (Left_Opnd (Cond)), False);
            Insert_Action (N,
              Make_Raise_Constraint_Error (Loc,
                Condition => Cond,
                Reason    => CE_Length_Check_Failed));
         end if;
      end Check_Bounds;

      ----------------------------
      -- Compute_Others_Present --
      ----------------------------

      procedure Compute_Others_Present (Sub_Aggr : Node_Id; Dim : Pos) is
         Assoc : Node_Id;
         Expr  : Node_Id;

      begin
         if Present (Component_Associations (Sub_Aggr)) then
            Assoc := Last (Component_Associations (Sub_Aggr));

            if Nkind (First (Choices (Assoc))) = N_Others_Choice then
               Others_Present (Dim) := True;
            end if;
         end if;

         --  Now look inside the sub-aggregate to see if there is more work

         if Dim < Aggr_Dimension then

            --  Process positional components

            if Present (Expressions (Sub_Aggr)) then
               Expr := First (Expressions (Sub_Aggr));
               while Present (Expr) loop
                  Compute_Others_Present (Expr, Dim + 1);
                  Next (Expr);
               end loop;
            end if;

            --  Process component associations

            if Present (Component_Associations (Sub_Aggr)) then
               Assoc := First (Component_Associations (Sub_Aggr));
               while Present (Assoc) loop
                  Expr := Expression (Assoc);
                  Compute_Others_Present (Expr, Dim + 1);
                  Next (Assoc);
               end loop;
            end if;
         end if;
      end Compute_Others_Present;

      ------------------------
      -- Has_Address_Clause --
      ------------------------

      function Has_Address_Clause (D : Node_Id) return Boolean is
         Id   : constant Entity_Id := Defining_Identifier (D);
         Decl : Node_Id := Next (D);

      begin
         while Present (Decl) loop
            if Nkind (Decl) = N_At_Clause
               and then Chars (Identifier (Decl)) = Chars (Id)
            then
               return True;

            elsif Nkind (Decl) = N_Attribute_Definition_Clause
               and then Chars (Decl) = Name_Address
               and then Chars (Name (Decl)) = Chars (Id)
            then
               return True;
            end if;

            Next (Decl);
         end loop;

         return False;
      end Has_Address_Clause;

      ------------------------
      -- In_Place_Assign_OK --
      ------------------------

      function In_Place_Assign_OK return Boolean is
         Aggr_In : Node_Id;
         Aggr_Lo : Node_Id;
         Aggr_Hi : Node_Id;
         Obj_In  : Node_Id;
         Obj_Lo  : Node_Id;
         Obj_Hi  : Node_Id;

         function Is_Others_Aggregate (Aggr : Node_Id) return Boolean;
         --   Aggregates that consist of a single Others choice are safe
         --  if the single expression is.

         function Safe_Aggregate (Aggr : Node_Id) return Boolean;
         --  Check recursively that each component of a (sub)aggregate does
         --  not depend on the variable being assigned to.

         function Safe_Component (Expr : Node_Id) return Boolean;
         --  Verify that an expression cannot depend on the variable being
         --  assigned to. Room for improvement here (but less than before).

         -------------------------
         -- Is_Others_Aggregate --
         -------------------------

         function Is_Others_Aggregate (Aggr : Node_Id) return Boolean is
         begin
            return No (Expressions (Aggr))
              and then Nkind
                (First (Choices (First (Component_Associations (Aggr)))))
                  = N_Others_Choice;
         end Is_Others_Aggregate;

         --------------------
         -- Safe_Aggregate --
         --------------------

         function Safe_Aggregate (Aggr : Node_Id) return Boolean is
            Expr : Node_Id;

         begin
            if Present (Expressions (Aggr)) then
               Expr := First (Expressions (Aggr));

               while Present (Expr) loop
                  if Nkind (Expr) = N_Aggregate then
                     if not Safe_Aggregate (Expr) then
                        return False;
                     end if;

                  elsif not Safe_Component (Expr) then
                     return False;
                  end if;

                  Next (Expr);
               end loop;
            end if;

            if Present (Component_Associations (Aggr)) then
               Expr := First (Component_Associations (Aggr));

               while Present (Expr) loop
                  if Nkind (Expression (Expr)) = N_Aggregate then
                     if not Safe_Aggregate (Expression (Expr)) then
                        return False;
                     end if;

                  elsif not Safe_Component (Expression (Expr)) then
                     return False;
                  end if;

                  Next (Expr);
               end loop;
            end if;

            return True;
         end Safe_Aggregate;

         --------------------
         -- Safe_Component --
         --------------------

         function Safe_Component (Expr : Node_Id) return Boolean is
            Comp : Node_Id := Expr;

            function Check_Component (Comp : Node_Id) return Boolean;
            --  Do the recursive traversal, after copy.

            ---------------------
            -- Check_Component --
            ---------------------

            function Check_Component (Comp : Node_Id) return Boolean is
            begin
               if Is_Overloaded (Comp) then
                  return False;
               end if;

               return Compile_Time_Known_Value (Comp)

                 or else (Is_Entity_Name (Comp)
                           and then  Present (Entity (Comp))
                           and then No (Renamed_Object (Entity (Comp))))

                 or else (Nkind (Comp) = N_Attribute_Reference
                           and then Check_Component (Prefix (Comp)))

                 or else (Nkind (Comp) in N_Binary_Op
                           and then Check_Component (Left_Opnd  (Comp))
                           and then Check_Component (Right_Opnd (Comp)))

                 or else (Nkind (Comp) in N_Unary_Op
                           and then Check_Component (Right_Opnd (Comp)))

                 or else (Nkind (Comp) = N_Selected_Component
                           and then Check_Component (Prefix (Comp)));
            end Check_Component;

         --  Start of processing for Safe_Component

         begin
            --  If the component appears in an association that may
            --  correspond to more than one element, it is not analyzed
            --  before the expansion into assignments, to avoid side effects.
            --  We analyze, but do not resolve the copy, to obtain sufficient
            --  entity information for the checks that follow. If component is
            --  overloaded we assume an unsafe function call.

            if not Analyzed (Comp) then
               if Is_Overloaded (Expr) then
                  return False;

               elsif Nkind (Expr) = N_Aggregate
                  and then not Is_Others_Aggregate (Expr)
               then
                  return False;

               elsif Nkind (Expr) = N_Allocator then
                  --  For now, too complex to analyze.

                  return False;
               end if;

               Comp := New_Copy_Tree (Expr);
               Set_Parent (Comp, Parent (Expr));
               Analyze (Comp);
            end if;

            if Nkind (Comp) = N_Aggregate then
               return Safe_Aggregate (Comp);
            else
               return Check_Component (Comp);
            end if;
         end Safe_Component;

      --  Start of processing for In_Place_Assign_OK

      begin
         if Present (Component_Associations (N)) then

            --  On assignment, sliding can take place, so we cannot do the
            --  assignment in place unless the bounds of the aggregate are
            --  statically equal to those of the target.

            --  If the aggregate is given by an others choice, the bounds
            --  are derived from the left-hand side, and the assignment is
            --  safe if the expression is.

            if Is_Others_Aggregate (N) then
               return
                 Safe_Component
                  (Expression (First (Component_Associations (N))));
            end if;

            Aggr_In := First_Index (Etype (N));
            Obj_In  := First_Index (Etype (Name (Parent (N))));

            while Present (Aggr_In) loop
               Get_Index_Bounds (Aggr_In, Aggr_Lo, Aggr_Hi);
               Get_Index_Bounds (Obj_In, Obj_Lo, Obj_Hi);

               if not Compile_Time_Known_Value (Aggr_Lo)
                 or else not Compile_Time_Known_Value (Aggr_Hi)
                 or else not Compile_Time_Known_Value (Obj_Lo)
                 or else not Compile_Time_Known_Value (Obj_Hi)
                 or else Expr_Value (Aggr_Lo) /= Expr_Value (Obj_Lo)
                 or else Expr_Value (Aggr_Hi) /= Expr_Value (Obj_Hi)
               then
                  return False;
               end if;

               Next_Index (Aggr_In);
               Next_Index (Obj_In);
            end loop;
         end if;

         --  Now check the component values themselves.

         return Safe_Aggregate (N);
      end In_Place_Assign_OK;

      ----------------
      -- Must_Slide --
      ----------------

      function Must_Slide (N : Node_Id; Typ : Entity_Id) return Boolean
      is
         Obj_Type : constant Entity_Id :=
                      Etype (Defining_Identifier (Parent (N)));

         L1, L2, H1, H2 : Node_Id;

      begin
         --  No sliding if the type of the object is not established yet, if
         --  it is an unconstrained type whose actual subtype comes from the
         --  aggregate, or if the two types are identical.

         if not Is_Array_Type (Obj_Type) then
            return False;

         elsif not Is_Constrained (Obj_Type) then
            return False;

         elsif Typ = Obj_Type then
            return False;

         else
            --  Sliding can only occur along the first dimension

            Get_Index_Bounds (First_Index (Typ), L1, H1);
            Get_Index_Bounds (First_Index (Obj_Type), L2, H2);

            if not Is_Static_Expression (L1)
              or else not Is_Static_Expression (L2)
              or else not Is_Static_Expression (H1)
              or else not Is_Static_Expression (H2)
            then
               return False;
            else
               return Expr_Value (L1) /= Expr_Value (L2)
                 or else Expr_Value (H1) /= Expr_Value (H2);
            end if;
         end if;
      end Must_Slide;

      --  Remaining Expand_Array_Aggregate variables

      Tmp : Entity_Id;
      --  Holds the temporary aggregate value

      Tmp_Decl : Node_Id;
      --  Holds the declaration of Tmp

      Aggr_Code   : List_Id;
      Parent_Node : Node_Id;
      Parent_Kind : Node_Kind;

   --  Start of processing for Expand_Array_Aggregate

   begin
      --  If the semantic analyzer has determined that aggregate N will raise
      --  Constraint_Error at run-time, then the aggregate node has been
      --  replaced with an N_Raise_Constraint_Error node and we should
      --  never get here.

      pragma Assert (not Raises_Constraint_Error (N));

      --  STEP 1a.

      --  Check that the index range defined by aggregate bounds is
      --  compatible with corresponding index subtype.

      Index_Compatibility_Check : declare
         Aggr_Index_Range : Node_Id := First_Index (Typ);
         --  The current aggregate index range

         Index_Constraint : Node_Id := First_Index (Etype (Typ));
         --  The corresponding index constraint against which we have to
         --  check the above aggregate index range.

      begin
         Compute_Others_Present (N, 1);

         for J in 1 .. Aggr_Dimension loop
	    
            --  Save the low and high bounds of the aggregate index as well
            --  as the index type for later use in checks (b) and (c) below.

            Aggr_Low  (J) := Low_Bound (Aggr_Index_Range);
            Aggr_High (J) := High_Bound (Aggr_Index_Range);

            Aggr_Index_Typ (J) := Etype (Index_Constraint);

            Next_Index (Aggr_Index_Range);
            Next_Index (Index_Constraint);
         end loop;
      end Index_Compatibility_Check;

      --  STEP 2.

      --  At this point we try to convert to positional form

      Convert_To_Positional (N);

      --  if the result is no longer an aggregate (e.g. it may be a string
      --  literal, or a temporary which has the needed value), then we are
      --  done, since there is no longer a nested aggregate.

      if Nkind (N) /= N_Aggregate then
         return;

      --  We are also done if the result is an analyzed aggregate
      --  This case could use more comments ???

      elsif Analyzed (N)
        and then N /= Original_Node (N)
      then
         return;
      end if;

      --  Now see if back end processing is possible

      if Backend_Processing_Possible (N) then

         --  If the aggregate is static but the constraints are not, build
         --  a static subtype for the aggregate, so that Gigi can place it
         --  in static memory. Perform an unchecked_conversion to the non-
         --  static type imposed by the context.

         declare
            Itype      : constant Entity_Id := Etype (N);
            Index      : Node_Id;
            Needs_Type : Boolean := False;

         begin
            Index := First_Index (Itype);

            while Present (Index) loop
               if not Is_Static_Subtype (Etype (Index)) then
                  Needs_Type := True;
                  exit;
               else
                  Next_Index (Index);
               end if;
            end loop;

            if Needs_Type then
               Build_Constrained_Type (Positional => True);
               Rewrite (N, Unchecked_Convert_To (Itype, N));
               Analyze (N);
            end if;
         end;

         return;
      end if;

      --  STEP 3.

      --  Delay expansion for nested aggregates it will be taken care of
      --  when the parent aggregate is expanded

      Parent_Node := Parent (N);
      Parent_Kind := Nkind (Parent_Node);

      if Parent_Kind = N_Qualified_Expression then
         Parent_Node := Parent (Parent_Node);
         Parent_Kind := Nkind (Parent_Node);
      end if;

      if Parent_Kind = N_Aggregate
        or else Parent_Kind = N_Extension_Aggregate
        or else Parent_Kind = N_Component_Association
        or else (Parent_Kind = N_Assignment_Statement
                  and then Inside_Init_Proc)
      then
         Set_Expansion_Delayed (N);
         return;
      end if;

      --  STEP 4.

      --  Look if in place aggregate expansion is possible

      --  For object declarations we build the aggregate in place, unless
      --  the array is bit-packed or the component is controlled.

      --  For assignments we do the assignment in place if all the component
      --  associations have compile-time known values. For other cases we
      --  create a temporary. The analysis for safety of on-line assignment
      --  is delicate, i.e. we don't know how to do it fully yet ???

      if Has_Default_Init_Comps (N) then
         Maybe_In_Place_OK := False;
      else
         Maybe_In_Place_OK :=
           Comes_From_Source (N)
             and then Nkind (Parent (N)) = N_Assignment_Statement
             and then not Is_Bit_Packed_Array (Typ)
             and then not Has_Controlled_Component (Typ)
             and then In_Place_Assign_OK;
      end if;

      if not Has_Default_Init_Comps (N)
         and then Comes_From_Source (Parent (N))
         and then Nkind (Parent (N)) = N_Object_Declaration
         and then not Must_Slide (N, Typ)
         and then N = Expression (Parent (N))
         and then not Is_Bit_Packed_Array (Typ)
         and then not Has_Controlled_Component (Typ)
         and then not Has_Address_Clause (Parent (N))
      then
         Tmp := Defining_Identifier (Parent (N));
         Set_No_Initialization (Parent (N));
         Set_Expression (Parent (N), Empty);

         --  Set the type of the entity, for use in the analysis of the
         --  subsequent indexed assignments. If the nominal type is not
         --  constrained, build a subtype from the known bounds of the
         --  aggregate. If the declaration has a subtype mark, use it,
         --  otherwise use the itype of the aggregate.

         if not Is_Constrained (Typ) then
            Build_Constrained_Type (Positional => False);
	    
         elsif Is_Entity_Name (Object_Definition (Parent (N)))
           and then Is_Constrained (Entity (Object_Definition (Parent (N))))
         then
            Set_Etype (Tmp, Entity (Object_Definition (Parent (N))));
         else
            Set_Size_Known_At_Compile_Time (Typ, False);
            Set_Etype (Tmp, Typ);
         end if;

      elsif Maybe_In_Place_OK
        and then Is_Entity_Name (Name (Parent (N)))
      then
         Tmp := Entity (Name (Parent (N)));

         if Etype (Tmp) /= Etype (N) then
            Apply_Length_Check (N, Etype (Tmp));

            if Nkind (N) = N_Raise_Constraint_Error then

               --  Static error, nothing further to expand

               return;
            end if;
         end if;

      elsif Maybe_In_Place_OK
        and then Nkind (Name (Parent (N))) = N_Explicit_Dereference
        and then Is_Entity_Name (Prefix (Name (Parent (N))))
      then
         Tmp := Name (Parent (N));

         if Etype (Tmp) /= Etype (N) then
            Apply_Length_Check (N, Etype (Tmp));
         end if;

      elsif Maybe_In_Place_OK
        and then Nkind (Name (Parent (N))) = N_Slice
        and then Safe_Slice_Assignment (N)
      then
         --  Safe_Slice_Assignment rewrites assignment as a loop

         return;

      --  Step 5

      --  In place aggregate expansion is not possible

      else
         Maybe_In_Place_OK := False;
         Tmp := Make_Defining_Identifier (Loc, New_Internal_Name ('A'));
         Tmp_Decl :=
           Make_Object_Declaration
             (Loc,
              Defining_Identifier => Tmp,
              Object_Definition   => New_Occurrence_Of (Typ, Loc));
         Set_No_Initialization (Tmp_Decl, True);

         --  If we are within a loop, the temporary will be pushed on the
         --  stack at each iteration. If the aggregate is the expression for
         --  an allocator, it will be immediately copied to the heap and can
         --  be reclaimed at once. We create a transient scope around the
         --  aggregate for this purpose.

--           if Ekind (Current_Scope) = E_Loop
--             and then Nkind (Parent (Parent (N))) = N_Allocator
--           then
--              Establish_Transient_Scope (N, False);
--           end if;

         Insert_Action (N, Tmp_Decl);
      end if;

      --  Construct and insert the aggregate code. We can safely suppress
      --  index checks because this code is guaranteed not to raise CE
      --  on index checks. However we should *not* suppress all checks.

      declare
         Target : Node_Id;

      begin
         if Nkind (Tmp) = N_Defining_Identifier then
            Target := New_Reference_To (Tmp, Loc);

         else

            if Has_Default_Init_Comps (N) then

               --  Ada0Y (AI-287): This case has not been analyzed???

               pragma Assert (False);
               null;
            end if;

            --  Name in assignment is explicit dereference.

            Target := New_Copy (Tmp);
         end if;

         Aggr_Code :=
           Build_Array_Aggr_Code (N,
             Ctype       => Ctyp,
             Index       => First_Index (Typ),
             Into        => Target,
             Scalar_Comp => Is_Scalar_Type (Ctyp));
      end;

      if Comes_From_Source (Tmp) then
         Insert_Actions_After (Parent (N), Aggr_Code);

      else
         Insert_Actions (N, Aggr_Code);
      end if;

      --  If the aggregate has been assigned in place, remove the original
      --  assignment.

      if Nkind (Parent (N)) = N_Assignment_Statement
        and then Maybe_In_Place_OK
      then
         Rewrite (Parent (N), Make_Null_Statement (Loc));

      elsif Nkind (Parent (N)) /= N_Object_Declaration
        or else Tmp /= Defining_Identifier (Parent (N))
      then
         Rewrite (N, New_Occurrence_Of (Tmp, Loc));
         Analyze_And_Resolve (N, Typ);
      end if;
   end Expand_Array_Aggregate;
   
end Reflex.Expanders.Aggregates;
