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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with Atree;    use Atree;
--with Checks;   use Checks;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Eval_Fat;
--  with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
--  with Expander; use Expander;
with Freeze;   use Freeze;
with Lib;      use Lib;
with Lib.Xref; use Lib.Xref;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rtsfind;  use Rtsfind;
with Sdefault; use Sdefault;
with Sem;      use Sem;
with Sem_Cat;  use Sem_Cat;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
-- with Sem_Dist; use Sem_Dist;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stand;
with Stringt;  use Stringt;
with Targparm; use Targparm;
with Ttypes;   use Ttypes;
with Ttypef;   use Ttypef;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Urealp;   use Urealp;
with Widechar; use Widechar;

package body Sem_Attr is

   True_Value  : constant Uint := Uint_1;
   False_Value : constant Uint := Uint_0;
   --  Synonyms to be used when these constants are used as Boolean values

   Bad_Attribute : exception;
   --  Exception raised if an error is detected during attribute processing,
   --  used so that we can abandon the processing so we don't run into
   --  trouble with cascaded errors.

   --  The following array is the list of attributes defined in the Ada 83 RM

   Attribute_83 : constant Attribute_Class_Array := Attribute_Class_Array'(
      Attribute_Address           |
      Attribute_Alignment         |
      Attribute_Base              |
      Attribute_Constrained       |
      Attribute_First             |
      Attribute_First_Bit         |
      Attribute_Fore              |
      Attribute_Image             |
      Attribute_Last              |
      Attribute_Last_Bit          |
      Attribute_Length            |
      Attribute_Machine_Emax      |
      Attribute_Machine_Emin      |
      Attribute_Machine_Mantissa  |
      Attribute_Machine_Overflows |
      Attribute_Machine_Radix     |
      Attribute_Machine_Rounds    |
      Attribute_Mantissa          |
      Attribute_Pos               |
      Attribute_Position          |
      Attribute_Range             |
      Attribute_Size              |
      Attribute_Storage_Size      |
      Attribute_Succ              |
      Attribute_Terminated        |
      Attribute_Val               |
      Attribute_Value             => True,
      others                      => False);

   -----------------------
   -- Local_Subprograms --
   -----------------------

   procedure Eval_Attribute (N : Node_Id);
   --  Performs compile time evaluation of attributes where possible, leaving
   --  the Is_Static_Expression/Raises_Constraint_Error flags appropriately
   --  set, and replacing the node with a literal node if the value can be
   --  computed at compile time. All static attribute references are folded,
   --  as well as a number of cases of non-static attributes that can always
   --  be computed at compile time (e.g. floating-point model attributes that
   --  are applied to non-static subtypes). Of course in such cases, the
   --  Is_Static_Expression flag will not be set on the resulting literal.
   --  Note that the only required action of this procedure is to catch the
   --  static expression cases as described in the RM. Folding of other cases
   --  is done where convenient, but some additional non-static folding is in
   --  N_Expand_Attribute_Reference in cases where this is more convenient.

   function Is_Anonymous_Tagged_Base
     (Anon : Entity_Id;
      Typ  : Entity_Id)
      return Boolean;
   --  For derived tagged types that constrain parent discriminants we build
   --  an anonymous unconstrained base type. We need to recognize the relation
   --  between the two when analyzing an access attribute for a constrained
   --  component, before the full declaration for Typ has been analyzed, and
   --  where therefore the prefix of the attribute does not match the enclosing
   --  scope.

   -----------------------
   -- Analyze_Attribute --
   -----------------------

   procedure Analyze_Attribute (N : Node_Id) is
      Loc     : constant Source_Ptr   := Sloc (N);
      Aname   : constant Name_Id      := Attribute_Name (N);
      P       : constant Node_Id      := Prefix (N);
      Exprs   : constant List_Id      := Expressions (N);
      Attr_Id : constant Attribute_Id := Get_Attribute_Id (Aname);
      E1      : Node_Id;
      E2      : Node_Id;

      P_Type : Entity_Id;
      --  Type of prefix after analysis

      P_Base_Type : Entity_Id;
      --  Base type of prefix after analysis

      -----------------------
      -- Local Subprograms --
      -----------------------

      procedure Analyze_Access_Attribute;
      --  Used for Access, Unchecked_Access, Unrestricted_Access attributes.
      --  Internally, Id distinguishes which of the three cases is involved.

      procedure Check_Array_Or_Scalar_Type;
      --  Common procedure used by First, Last, Range attribute to check
      --  that the prefix is a constrained array or scalar type, or a name
      --  of an array object, and that an argument appears only if appropriate
      --  (i.e. only in the array case).

      procedure Check_Array_Type;
      --  Common semantic checks for all array attributes. Checks that the
      --  prefix is a constrained array type or the name of an array object.
      --  The error message for non-arrays is specialized appropriately.

      procedure Check_Asm_Attribute;
      --  Common semantic checks for Asm_Input and Asm_Output attributes

      procedure Check_Component;
      --  Common processing for Bit_Position, First_Bit, Last_Bit, and
      --  Position. Checks prefix is an appropriate selected component.

      procedure Check_Dereference;
      --  If the prefix of attribute is an object of an access type, then
      --  introduce an explicit deference, and adjust P_Type accordingly.

      procedure Check_Discrete_Type;
      --  Verify that prefix of attribute N is a discrete type

      procedure Check_E0;
      --  Check that no attribute arguments are present

      procedure Check_Either_E0_Or_E1;
      --  Check that there are zero or one attribute arguments present

      procedure Check_E1;
      --  Check that exactly one attribute argument is present

      procedure Check_E2;
      --  Check that two attribute arguments are present

      procedure Check_Enum_Image;
      --  If the prefix type is an enumeration type, set all its literals
      --  as referenced, since the image function could possibly end up
      --  referencing any of the literals indirectly.

      procedure Check_Floating_Point_Type;
      --  Verify that prefix of attribute N is a float type

      procedure Check_Floating_Point_Type_0;
      --  Verify that prefix of attribute N is a float type and that
      --  no attribute expressions are present

      procedure Check_Floating_Point_Type_1;
      --  Verify that prefix of attribute N is a float type and that
      --  exactly one attribute expression is present

      procedure Check_Floating_Point_Type_2;
      --  Verify that prefix of attribute N is a float type and that
      --  two attribute expressions are present

      procedure Legal_Formal_Attribute;
      --  Common processing for attributes Definite, and Has_Discriminants

      procedure Check_Integer_Type;
      --  Verify that prefix of attribute N is an integer type

      procedure Check_Library_Unit;
      --  Verify that prefix of attribute N is a library unit

      procedure Check_Not_Incomplete_Type;
      --  Check that P (the prefix of the attribute) is not an incomplete
      --  type or a private type for which no full view has been given.

      procedure Check_Object_Reference (P : Node_Id);
      --  Check that P (the prefix of the attribute) is an object reference

      procedure Check_Program_Unit;
      --  Verify that prefix of attribute N is a program unit

      procedure Check_Real_Type;
      --  Verify that prefix of attribute N is fixed or float type

      procedure Check_Scalar_Type;
      --  Verify that prefix of attribute N is a scalar type

      procedure Check_Standard_Prefix;
      --  Verify that prefix of attribute N is package Standard

      procedure Check_Type;
      --  Verify that the prefix of attribute N is a type

      procedure Check_Unit_Name (Nod : Node_Id);
      --  Check that Nod is of the form of a library unit name, i.e that
      --  it is an identifier, or a selected component whose prefix is
      --  itself of the form of a library unit name. Note that this is
      --  quite different from Check_Program_Unit, since it only checks
      --  the syntactic form of the name, not the semantic identity. This
      --  is because it is used with attributes (Elab_Body, Elab_Spec, and
      --  UET_Address) which can refer to non-visible unit.

      procedure Error_Attr (Msg : String; Error_Node : Node_Id);
      pragma No_Return (Error_Attr);
      procedure Error_Attr;
      pragma No_Return (Error_Attr);
      --  Posts error using Error_Msg_N at given node, sets type of attribute
      --  node to Any_Type, and then raises Bad_Attribute to avoid any further
      --  semantic processing. The message typically contains a % insertion
      --  character which is replaced by the attribute name. The call with
      --  no arguments is used when the caller has already generated the
      --  required error messages.

      procedure Standard_Attribute (Val : Int);
      --  Used to process attributes whose prefix is package Standard which
      --  yield values of type Universal_Integer. The attribute reference
      --  node is rewritten with an integer literal of the given value.

      procedure Unexpected_Argument (En : Node_Id);
      --  Signal unexpected attribute argument (En is the argument)

      procedure Validate_Non_Static_Attribute_Function_Call;
      --  Called when processing an attribute that is a function call to a
      --  non-static function, i.e. an attribute function that either takes
      --  non-scalar arguments or returns a non-scalar result. Verifies that
      --  such a call does not appear in a preelaborable context.

      ------------------------------
      -- Analyze_Access_Attribute --
      ------------------------------

      procedure Analyze_Access_Attribute is
         Acc_Type : Entity_Id;

         Scop : Entity_Id;
         Typ  : Entity_Id;

         function Build_Access_Object_Type (DT : Entity_Id) return Entity_Id;
         --  Build an access-to-object type whose designated type is DT,
         --  and whose Ekind is appropriate to the attribute type. The
         --  type that is constructed is returned as the result.

         procedure Build_Access_Subprogram_Type (P : Node_Id);
         --  Build an access to subprogram whose designated type is
         --  the type of the prefix. If prefix is overloaded, so it the
         --  node itself. The result is stored in Acc_Type.

         ------------------------------
         -- Build_Access_Object_Type --
         ------------------------------

         function Build_Access_Object_Type (DT : Entity_Id) return Entity_Id is
            Typ : Entity_Id;

         begin
            if Aname = Name_Unrestricted_Access then
               Typ :=
                 New_Internal_Entity
                   (E_Allocator_Type, Current_Scope, Loc, 'A');
            else
               Typ :=
                 New_Internal_Entity
                   (E_Access_Attribute_Type, Current_Scope, Loc, 'A');
            end if;

            Set_Etype                     (Typ, Typ);
            Init_Size_Align               (Typ);
            Set_Is_Itype                  (Typ);
            Set_Associated_Node_For_Itype (Typ, N);
            Set_Directly_Designated_Type  (Typ, DT);
            return Typ;
         end Build_Access_Object_Type;

         ----------------------------------
         -- Build_Access_Subprogram_Type --
         ----------------------------------

         procedure Build_Access_Subprogram_Type (P : Node_Id) is
            Index : Interp_Index;
            It    : Interp;

            function Get_Kind (E : Entity_Id) return Entity_Kind;
            --  Distinguish between access to regular and protected
            --  subprograms.

            --------------
            -- Get_Kind --
            --------------

            function Get_Kind (E : Entity_Id) return Entity_Kind is
            begin
                  return E_Access_Subprogram_Type;
            end Get_Kind;

         --  Start of processing for Build_Access_Subprogram_Type

         begin
            --  In the case of an access to subprogram, use the name of the
            --  subprogram itself as the designated type. Type-checking in
            --  this case compares the signatures of the designated types.

            if not Is_Overloaded (P) then
               Acc_Type :=
                 New_Internal_Entity
                   (Get_Kind (Entity (P)), Current_Scope, Loc, 'A');
               Set_Etype (Acc_Type, Acc_Type);
               Set_Directly_Designated_Type (Acc_Type, Entity (P));
               Set_Etype (N, Acc_Type);

            else
               Get_First_Interp (P, Index, It);
               Set_Etype (N, Any_Type);

               while Present (It.Nam) loop
                  if not Is_Intrinsic_Subprogram (It.Nam) then
                     Acc_Type :=
                       New_Internal_Entity
                         (Get_Kind (It.Nam), Current_Scope, Loc, 'A');
                     Set_Etype (Acc_Type, Acc_Type);
                     Set_Directly_Designated_Type (Acc_Type, It.Nam);
                     Add_One_Interp (N, Acc_Type, Acc_Type);
                  end if;

                  Get_Next_Interp (Index, It);
               end loop;

               if Etype (N) = Any_Type then
                  Error_Attr ("prefix of % attribute cannot be intrinsic", P);
               end if;
            end if;
         end Build_Access_Subprogram_Type;

      --  Start of processing for Analyze_Access_Attribute

      begin
         Check_E0;

         if Nkind (P) = N_Character_Literal then
            Error_Attr
              ("prefix of % attribute cannot be enumeration literal", P);
         end if;

         --  Case of access to subprogram

         if Is_Entity_Name (P)
           and then Is_Overloadable (Entity (P))
         then
            --  Not allowed for nested subprograms if No_Implicit_Dynamic_Code
            --  restriction set (since in general a trampoline is required).

            if not Is_Library_Level_Entity (Entity (P)) then
               Check_Restriction (No_Implicit_Dynamic_Code, P);
            end if;

            --  Build the appropriate subprogram type

            Build_Access_Subprogram_Type (P);

            --  For unrestricted access, kill current values, since this
            --  attribute allows a reference to a local subprogram that
            --  could modify local variables to be passed out of scope

            if Aname = Name_Unrestricted_Access then
               Kill_Current_Values;
            end if;

            return;

         --  Component is an operation of a protected type

         elsif Nkind (P) = N_Selected_Component
           and then Is_Overloadable (Entity (Selector_Name (P)))
         then
            Build_Access_Subprogram_Type (Selector_Name (P));
            return;
         end if;

         --  Deal with incorrect reference to a type, but note that some
         --  accesses are allowed (references to the current type instance).

         if Is_Entity_Name (P) then
            Scop := Current_Scope;
            Typ := Entity (P);

            if Is_Type (Typ) then

               --  OK if we are within the scope of a limited type
               --  let's mark the component as having per object constraint

               if Is_Anonymous_Tagged_Base (Scop, Typ) then
                  Typ := Scop;
                  Set_Entity (P, Typ);
                  Set_Etype  (P, Typ);
               end if;

               if Typ = Scop then
                  declare
                     Q : Node_Id := Parent (N);

                  begin
                     while Present (Q)
                       and then Nkind (Q) /= N_Component_Declaration
                     loop
                        Q := Parent (Q);
                     end loop;
                     if Present (Q) then
                        Set_Has_Per_Object_Constraint (
                          Defining_Identifier (Q), True);
                     end if;
                  end;

                  if Nkind (P) = N_Expanded_Name then
                     Error_Msg_N
                       ("current instance prefix must be a direct name", P);
                  end if;

                  --  If a current instance attribute appears within a
                  --  a component constraint it must appear alone; other
                  --  contexts (default expressions, within a task body)
                  --  are not subject to this restriction.

                  if not In_Default_Expression
                    and then not Has_Completion (Scop)
                    and then
                      Nkind (Parent (N)) /= N_Discriminant_Association
                    and then
                      Nkind (Parent (N)) /= N_Index_Or_Discriminant_Constraint
                  then
                     Error_Msg_N
                       ("current instance attribute must appear alone", N);
                  end if;

               --  OK if we are in initialization procedure for the type
               --  in question, in which case the reference to the type
               --  is rewritten as a reference to the current object.

--                 elsif Ekind (Scop) = E_Procedure
--                   and then Is_Init_Proc (Scop)
--                   and then Etype (First_Formal (Scop)) = Typ
--                 then
--                    Rewrite (N,
--                      Make_Attribute_Reference (Loc,
--                        Prefix         => Make_Identifier (Loc, Name_uInit),
--                        Attribute_Name => Name_Unrestricted_Access));
--                    Analyze (N);
--                    return;
--
--                 --  Otherwise we have an error case

               else
                  Error_Attr ("% attribute cannot be applied to type", P);
                  return;
               end if;
            end if;
         end if;

         --  If we fall through, we have a normal access to object case.
         --  Unrestricted_Access is legal wherever an allocator would be
         --  legal, so its Etype is set to E_Allocator. The expected type
         --  of the other attributes is a general access type, and therefore
         --  we label them with E_Access_Attribute_Type.

         if not Is_Overloaded (P) then
            Acc_Type := Build_Access_Object_Type (P_Type);
            Set_Etype (N, Acc_Type);
         else
            declare
               Index : Interp_Index;
               It    : Interp;

            begin
               Set_Etype (N, Any_Type);
               Get_First_Interp (P, Index, It);

               while Present (It.Typ) loop
                  Acc_Type := Build_Access_Object_Type (It.Typ);
                  Add_One_Interp (N, Acc_Type, Acc_Type);
                  Get_Next_Interp (Index, It);
               end loop;
            end;
         end if;

         --  If we have an access to an object, and the attribute comes
         --  from source, then set the object as potentially source modified.
         --  We do this because the resulting access pointer can be used to
         --  modify the variable, and we might not detect this, leading to
         --  some junk warnings.

         if Is_Entity_Name (P) then
            Set_Never_Set_In_Source (Entity (P), False);
         end if;

         --  Check for aliased view unless unrestricted case. We allow
         --  a nonaliased prefix when within an instance because the
         --  prefix may have been a tagged formal object, which is
         --  defined to be aliased even when the actual might not be
         --  (other instance cases will have been caught in the generic).

         if Aname /= Name_Unrestricted_Access
           and then not Is_Aliased_View (P)
           and then not In_Instance
         then
            Error_Attr ("prefix of % attribute must be aliased", P);
         end if;
      end Analyze_Access_Attribute;

      --------------------------------
      -- Check_Array_Or_Scalar_Type --
      --------------------------------

      procedure Check_Array_Or_Scalar_Type is
         Index : Entity_Id;

         D : Int;
         --  Dimension number for array attributes.

      begin
         --  Case of string literal or string literal subtype. These cases
         --  cannot arise from legal Ada code, but the expander is allowed
         --  to generate them. They require special handling because string
         --  literal subtypes do not have standard bounds (the whole idea
         --  of these subtypes is to avoid having to generate the bounds)

         if Ekind (P_Type) = E_String_Literal_Subtype then
            Set_Etype (N, Etype (First_Index (P_Base_Type)));
            return;

         --  Scalar types

         elsif Is_Scalar_Type (P_Type) then
            Check_Type;

            if Present (E1) then
               Error_Attr ("invalid argument in % attribute", E1);
            else
               Set_Etype (N, P_Base_Type);
               return;
            end if;

         --  The following is a special test to allow 'First to apply to
         --  private scalar types if the attribute comes from generated
         --  code. This occurs in the case of Normalize_Scalars code.

         elsif Is_Private_Type (P_Type)
           and then Present (Full_View (P_Type))
           and then Is_Scalar_Type (Full_View (P_Type))
           and then not Comes_From_Source (N)
         then
            Set_Etype (N, Implementation_Base_Type (P_Type));

         --  Array types other than string literal subtypes handled above

         else
            Check_Array_Type;

            --  We know prefix is an array type, or the name of an array
            --  object, and that the expression, if present, is static
            --  and within the range of the dimensions of the type.

            if Is_Array_Type (P_Type) then
               Index := First_Index (P_Base_Type);

            else pragma Assert (Is_Access_Type (P_Type));
               Index := First_Index (Base_Type (Designated_Type (P_Type)));
            end if;

            if No (E1) then

               --  First dimension assumed

               Set_Etype (N, Base_Type (Etype (Index)));

            else
               D := UI_To_Int (Intval (E1));

               for J in 1 .. D - 1 loop
                  Next_Index (Index);
               end loop;

               Set_Etype (N, Base_Type (Etype (Index)));
               Set_Etype (E1, Standard_Integer);
            end if;
         end if;
      end Check_Array_Or_Scalar_Type;

      ----------------------
      -- Check_Array_Type --
      ----------------------

      procedure Check_Array_Type is
         D : Int;
         --  Dimension number for array attributes.

      begin
         --  If the type is a string literal type, then this must be generated
         --  internally, and no further check is required on its legality.

         if Ekind (P_Type) = E_String_Literal_Subtype then
            return;

         --  If the type is a composite, it is an illegal aggregate, no point
         --  in going on.

         elsif P_Type = Any_Composite then
            raise Bad_Attribute;
         end if;

         --  Normal case of array type or subtype

         Check_Either_E0_Or_E1;

         if Is_Array_Type (P_Type) then
            if not Is_Constrained (P_Type)
              and then Is_Entity_Name (P)
              and then Is_Type (Entity (P))
            then
               --  Note: we do not call Error_Attr here, since we prefer to
               --  continue, using the relevant index type of the array,
               --  even though it is unconstrained. This gives better error
               --  recovery behavior.

               Error_Msg_Name_1 := Aname;
               Error_Msg_N
                 ("prefix for % attribute must be constrained array", P);
            end if;

            D := Number_Dimensions (P_Type);

         elsif Is_Access_Type (P_Type)
           and then Is_Array_Type (Designated_Type (P_Type))
         then
            if Is_Entity_Name (P) and then Is_Type (Entity (P)) then
               Error_Attr ("prefix of % attribute cannot be access type", P);
            end if;

            D := Number_Dimensions (Designated_Type (P_Type));

            --  If there is an implicit dereference, then we must freeze
            --  the designated type of the access type, since the type of
            --  the referenced array is this type (see AI95-00106).

            Freeze_Before (N, Designated_Type (P_Type));

         else
            if Is_Private_Type (P_Type) then
               Error_Attr
                 ("prefix for % attribute may not be private type", P);

            elsif Attr_Id = Attribute_First
                    or else
                  Attr_Id = Attribute_Last
            then
               Error_Attr ("invalid prefix for % attribute", P);

            else
               Error_Attr ("prefix for % attribute must be array", P);
            end if;
         end if;

         if Present (E1) then
            Resolve (E1, Any_Integer);
            Set_Etype (E1, Standard_Integer);

            if not Is_Static_Expression (E1)
              or else Raises_Constraint_Error (E1)
            then
               Flag_Non_Static_Expr
                 ("expression for dimension must be static!", E1);
               Error_Attr;

            elsif  UI_To_Int (Expr_Value (E1)) > D
              or else UI_To_Int (Expr_Value (E1)) < 1
            then
               Error_Attr ("invalid dimension number for array type", E1);
            end if;
         end if;
      end Check_Array_Type;

      -------------------------
      -- Check_Asm_Attribute --
      -------------------------

      procedure Check_Asm_Attribute is
      begin
         Check_Type;
         Check_E2;

         --  Check first argument is static string expression

         Analyze_And_Resolve (E1, Standard_String);

         if Etype (E1) = Any_Type then
            return;

         elsif not Is_OK_Static_Expression (E1) then
            Flag_Non_Static_Expr
              ("constraint argument must be static string expression!", E1);
            Error_Attr;
         end if;

         --  Check second argument is right type

         Analyze_And_Resolve (E2, Entity (P));

         --  Note: that is all we need to do, we don't need to check
         --  that it appears in a correct context. The Ada type system
         --  will do that for us.

      end Check_Asm_Attribute;

      ---------------------
      -- Check_Component --
      ---------------------

      procedure Check_Component is
      begin
         Check_E0;

         if Nkind (P) /= N_Selected_Component
           or else Ekind (Entity (Selector_Name (P))) /= E_Component
         then
            Error_Attr
              ("prefix for % attribute must be selected component", P);
         end if;
      end Check_Component;

      ------------------------------------
      -- Check_Decimal_Fixed_Point_Type --
      ------------------------------------

      procedure Check_Decimal_Fixed_Point_Type is
      begin
         Check_Type;

         Error_Attr
           ("prefix of % attribute must be decimal type", P);
      end Check_Decimal_Fixed_Point_Type;

      -----------------------
      -- Check_Dereference --
      -----------------------

      procedure Check_Dereference is
      begin
         if Is_Object_Reference (P)
           and then Is_Access_Type (P_Type)
         then
            Rewrite (P,
              Make_Explicit_Dereference (Sloc (P),
                Prefix => Relocate_Node (P)));

            Analyze_And_Resolve (P);
            P_Type := Etype (P);

            if P_Type = Any_Type then
               raise Bad_Attribute;
            end if;

            P_Base_Type := Base_Type (P_Type);
         end if;
      end Check_Dereference;

      -------------------------
      -- Check_Discrete_Type --
      -------------------------

      procedure Check_Discrete_Type is
      begin
         Check_Type;

         if not Is_Discrete_Type (P_Type) then
            Error_Attr ("prefix of % attribute must be discrete type", P);
         end if;
      end Check_Discrete_Type;

      --------------
      -- Check_E0 --
      --------------

      procedure Check_E0 is
      begin
         if Present (E1) then
            Unexpected_Argument (E1);
         end if;
      end Check_E0;

      --------------
      -- Check_E1 --
      --------------

      procedure Check_E1 is
      begin
         Check_Either_E0_Or_E1;

         if No (E1) then

            --  Special-case attributes that are functions and that appear as
            --  the prefix of another attribute. Error is posted on parent.

            if Nkind (Parent (N)) = N_Attribute_Reference
              and then (Attribute_Name (Parent (N)) = Name_Address
                          or else
                        Attribute_Name (Parent (N)) = Name_Access)
            then
               Error_Msg_Name_1 := Attribute_Name (Parent (N));
               Error_Msg_N ("illegal prefix for % attribute", Parent (N));
               Set_Etype (Parent (N), Any_Type);
               Set_Entity (Parent (N), Any_Type);
               raise Bad_Attribute;

            else
               Error_Attr ("missing argument for % attribute", N);
            end if;
         end if;
      end Check_E1;

      --------------
      -- Check_E2 --
      --------------

      procedure Check_E2 is
      begin
         if No (E1) then
            Error_Attr ("missing arguments for % attribute (2 required)", N);
         elsif No (E2) then
            Error_Attr ("missing argument for % attribute (2 required)", N);
         end if;
      end Check_E2;

      ---------------------------
      -- Check_Either_E0_Or_E1 --
      ---------------------------

      procedure Check_Either_E0_Or_E1 is
      begin
         if Present (E2) then
            Unexpected_Argument (E2);
         end if;
      end Check_Either_E0_Or_E1;

      ----------------------
      -- Check_Enum_Image --
      ----------------------

      procedure Check_Enum_Image is
         Lit : Entity_Id;

      begin
         if Is_Enumeration_Type (P_Base_Type) then
            Lit := First_Literal (P_Base_Type);
            while Present (Lit) loop
               Set_Referenced (Lit);
               Next_Literal (Lit);
            end loop;
         end if;
      end Check_Enum_Image;

      -------------------------------
      -- Check_Floating_Point_Type --
      -------------------------------

      procedure Check_Floating_Point_Type is
      begin
         Check_Type;

         if not Is_Floating_Point_Type (P_Type) then
            Error_Attr ("prefix of % attribute must be float type", P);
         end if;
      end Check_Floating_Point_Type;

      ---------------------------------
      -- Check_Floating_Point_Type_0 --
      ---------------------------------

      procedure Check_Floating_Point_Type_0 is
      begin
         Check_Floating_Point_Type;
         Check_E0;
      end Check_Floating_Point_Type_0;

      ---------------------------------
      -- Check_Floating_Point_Type_1 --
      ---------------------------------

      procedure Check_Floating_Point_Type_1 is
      begin
         Check_Floating_Point_Type;
         Check_E1;
      end Check_Floating_Point_Type_1;

      ---------------------------------
      -- Check_Floating_Point_Type_2 --
      ---------------------------------

      procedure Check_Floating_Point_Type_2 is
      begin
         Check_Floating_Point_Type;
         Check_E2;
      end Check_Floating_Point_Type_2;

      ------------------------
      -- Check_Integer_Type --
      ------------------------

      procedure Check_Integer_Type is
      begin
         Check_Type;

         if not Is_Integer_Type (P_Type) then
            Error_Attr ("prefix of % attribute must be integer type", P);
         end if;
      end Check_Integer_Type;

      ------------------------
      -- Check_Library_Unit --
      ------------------------

      procedure Check_Library_Unit is
      begin
         if not Is_Compilation_Unit (Entity (P)) then
            Error_Attr ("prefix of % attribute must be library unit", P);
         end if;
      end Check_Library_Unit;

      -------------------------------
      -- Check_Not_Incomplete_Type --
      -------------------------------

      procedure Check_Not_Incomplete_Type is
      begin
         if not Is_Entity_Name (P)
           or else not Is_Type (Entity (P))
           or else In_Default_Expression
         then
            return;

         else
            Check_Fully_Declared (P_Type, P);
         end if;
      end Check_Not_Incomplete_Type;

      ----------------------------
      -- Check_Object_Reference --
      ----------------------------

      procedure Check_Object_Reference (P : Node_Id) is
         Rtyp : Entity_Id;

      begin
         --  If we need an object, and we have a prefix that is the name of
         --  a function entity, convert it into a function call.

         if Is_Entity_Name (P)
           and then Ekind (Entity (P)) = E_Function
         then
            Rtyp := Etype (Entity (P));

            Rewrite (P,
              Make_Function_Call (Sloc (P),
                Name => Relocate_Node (P)));

            Analyze_And_Resolve (P, Rtyp);

         --  Otherwise we must have an object reference

         elsif not Is_Object_Reference (P) then
            Error_Attr ("prefix of % attribute must be object", P);
         end if;
      end Check_Object_Reference;

      ------------------------
      -- Check_Program_Unit --
      ------------------------

      procedure Check_Program_Unit is
      begin
         if Is_Entity_Name (P) then
            declare
               K : constant Entity_Kind := Ekind (Entity (P));
               T : constant Entity_Id   := Etype (Entity (P));

            begin
               if K in Subprogram_Kind
                 or else K = E_Package
                 or else K in Generic_Unit_Kind
               then
                  return;
               end if;
            end;
         end if;

         Error_Attr ("prefix of % attribute must be program unit", P);
      end Check_Program_Unit;

      ---------------------
      -- Check_Real_Type --
      ---------------------

      procedure Check_Real_Type is
      begin
         Check_Type;

         if not Is_Real_Type (P_Type) then
            Error_Attr ("prefix of % attribute must be real type", P);
         end if;
      end Check_Real_Type;

      -----------------------
      -- Check_Scalar_Type --
      -----------------------

      procedure Check_Scalar_Type is
      begin
         Check_Type;

         if not Is_Scalar_Type (P_Type) then
            Error_Attr ("prefix of % attribute must be scalar type", P);
         end if;
      end Check_Scalar_Type;

      ---------------------------
      -- Check_Standard_Prefix --
      ---------------------------

      procedure Check_Standard_Prefix is
      begin
         Check_E0;

         if Nkind (P) /= N_Identifier
           or else Chars (P) /= Name_Standard
         then
            Error_Attr ("only allowed prefix for % attribute is Standard", P);
         end if;

      end Check_Standard_Prefix;

      ----------------
      -- Check_Type --
      ----------------

      --  The possibilities are an entity name denoting a type, or an
      --  attribute reference that denotes a type (Base or Class). If
      --  the type is incomplete, replace it with its full view.

      procedure Check_Type is
      begin
         if not Is_Entity_Name (P)
           or else not Is_Type (Entity (P))
         then
            Error_Attr ("prefix of % attribute must be a type", P);

         elsif Ekind (Entity (P)) = E_Incomplete_Type
            and then Present (Full_View (Entity (P)))
         then
            P_Type := Full_View (Entity (P));
            Set_Entity (P, P_Type);
         end if;
      end Check_Type;

      ---------------------
      -- Check_Unit_Name --
      ---------------------

      procedure Check_Unit_Name (Nod : Node_Id) is
      begin
         if Nkind (Nod) = N_Identifier then
            return;

         elsif Nkind (Nod) = N_Selected_Component then
            Check_Unit_Name (Prefix (Nod));

            if Nkind (Selector_Name (Nod)) = N_Identifier then
               return;
            end if;
         end if;

         Error_Attr ("argument for % attribute must be unit name", P);
      end Check_Unit_Name;

      ----------------
      -- Error_Attr --
      ----------------

      procedure Error_Attr is
      begin
         Set_Etype (N, Any_Type);
         Set_Entity (N, Any_Type);
         raise Bad_Attribute;
      end Error_Attr;

      procedure Error_Attr (Msg : String; Error_Node : Node_Id) is
      begin
         Error_Msg_Name_1 := Aname;
         Error_Msg_N (Msg, Error_Node);
         Error_Attr;
      end Error_Attr;

      ----------------------------
      -- Legal_Formal_Attribute --
      ----------------------------

      procedure Legal_Formal_Attribute is
      begin
         Check_E0;

         if not Is_Entity_Name (P)
           or else not Is_Type (Entity (P))
         then
            Error_Attr ("prefix of % attribute must be generic type", N);

         elsif Is_Generic_Actual_Type (Entity (P))
           or else In_Instance
           or else In_Inlined_Body
         then
            null;

         elsif Is_Generic_Type (Entity (P)) then
            if not Is_Indefinite_Subtype (Entity (P)) then
               Error_Attr
                 ("prefix of % attribute must be indefinite generic type", N);
            end if;

         else
            Error_Attr
              ("prefix of % attribute must be indefinite generic type", N);
         end if;

         Set_Etype (N, Standard_Boolean);
      end Legal_Formal_Attribute;

      ------------------------
      -- Standard_Attribute --
      ------------------------

      procedure Standard_Attribute (Val : Int) is
      begin
         Check_Standard_Prefix;

         --  First a special check (more like a kludge really). For GNAT5
         --  on Windows, the alignments in GCC are severely mixed up. In
         --  particular, we have a situation where the maximum alignment
         --  that GCC thinks is possible is greater than the guaranteed
         --  alignment at run-time. That causes many problems. As a partial
         --  cure for this situation, we force a value of 4 for the maximum
         --  alignment attribute on this target. This still does not solve
         --  all problems, but it helps.

         --  A further (even more horrible) dimension to this kludge is now
         --  installed. There are two uses for Maximum_Alignment, one is to
         --  determine the maximum guaranteed alignment, that's the one we
         --  want the kludge to yield as 4. The other use is to maximally
         --  align objects, we can't use 4 here, since for example, long
         --  long integer has an alignment of 8, so we will get errors.

         --  It is of course impossible to determine which use the programmer
         --  has in mind, but an approximation for now is to disconnect the
         --  kludge if the attribute appears in an alignment clause.

         --  To be removed if GCC ever gets its act together here ???

         Alignment_Kludge : declare
            P : Node_Id;

            function On_X86 return Boolean;
            --  Determine if target is x86 (ia32), return True if so

            ------------
            -- On_X86 --
            ------------

            function On_X86 return Boolean is
               T : constant String := Sdefault.Target_Name.all;

            begin
               --  There is no clean way to check this. That's not surprising,
               --  the front end should not be doing this kind of test ???. The
               --  way we do it is test for either "86" or "pentium" being in
               --  the string for the target name.

               for J in T'First .. T'Last - 1 loop
                  if T (J .. J + 1) = "86"
                    or else (J <= T'Last - 6
                               and then T (J .. J + 6) = "pentium")
                  then
                     return True;
                  end if;
               end loop;

               return False;
            end On_X86;

         begin
            if Aname = Name_Maximum_Alignment and then On_X86 then
               P := Parent (N);

               while Nkind (P) in N_Subexpr loop
                  P := Parent (P);
               end loop;

               if Nkind (P) /= N_Attribute_Definition_Clause
                 or else Chars (P) /= Name_Alignment
               then
                  Rewrite (N, Make_Integer_Literal (Loc, 4));
                  Analyze (N);
                  return;
               end if;
            end if;
         end Alignment_Kludge;

         --  Normally we get the value from gcc ???

         Rewrite (N, Make_Integer_Literal (Loc, Val));
         Analyze (N);
      end Standard_Attribute;

      -------------------------
      -- Unexpected Argument --
      -------------------------

      procedure Unexpected_Argument (En : Node_Id) is
      begin
         Error_Attr ("unexpected argument for % attribute", En);
      end Unexpected_Argument;

      -------------------------------------------------
      -- Validate_Non_Static_Attribute_Function_Call --
      -------------------------------------------------

      --  This function should be moved to Sem_Dist ???

      procedure Validate_Non_Static_Attribute_Function_Call is
      begin
         if In_Preelaborated_Unit
           and then not In_Subprogram_Or_Concurrent_Unit
         then
            Flag_Non_Static_Expr
              ("non-static function call in preelaborated unit!", N);
         end if;
      end Validate_Non_Static_Attribute_Function_Call;

   -----------------------------------------------
   -- Start of Processing for Analyze_Attribute --
   -----------------------------------------------

   begin
      --  Immediate return if unrecognized attribute (already diagnosed
      --  by parser, so there is nothing more that we need to do)

      if not Is_Attribute_Name (Aname) then
         raise Bad_Attribute;
      end if;

      --   Remote access to subprogram type access attribute reference needs
      --   unanalyzed copy for tree transformation. The analyzed copy is used
      --   for its semantic information (whether prefix is a remote subprogram
      --   name), the unanalyzed copy is used to construct new subtree rooted
      --   with N_aggregate which represents a fat pointer aggregate.

      if Aname = Name_Access then
         Discard_Node (Copy_Separate_Tree (N));
      end if;

      --  Analyze prefix and exit if error in analysis. If the prefix is an
      --  incomplete type, use full view if available. A special case is
      --  that we never analyze the prefix of an Elab_Body or Elab_Spec
      --  or UET_Address attribute.

         Analyze (P);
         P_Type := Etype (P);

         if Is_Entity_Name (P)
           and then Present (Entity (P))
           and then Is_Type (Entity (P))
           and then Ekind (Entity (P)) = E_Incomplete_Type
         then
            P_Type := Get_Full_View (P_Type);
            Set_Entity (P, P_Type);
            Set_Etype  (P, P_Type);
         end if;

         if P_Type = Any_Type then
            raise Bad_Attribute;
         end if;

         P_Base_Type := Base_Type (P_Type);

      --  Analyze expressions that may be present, exiting if an error occurs

      if No (Exprs) then
         E1 := Empty;
         E2 := Empty;

      else
         E1 := First (Exprs);
         Analyze (E1);

         --  Check for missing or bad expression (result of previous error)

         if No (E1) or else Etype (E1) = Any_Type then
            raise Bad_Attribute;
         end if;

         E2 := Next (E1);

         if Present (E2) then
            Analyze (E2);

            if Etype (E2) = Any_Type then
               raise Bad_Attribute;
            end if;

            if Present (Next (E2)) then
               Unexpected_Argument (Next (E2));
            end if;
         end if;
      end if;

      if Is_Overloaded (P)
        and then Aname /= Name_Access
        and then Aname /= Name_Address
        and then Aname /= Name_Unchecked_Access
      then
         Error_Attr ("ambiguous prefix for % attribute", P);
      end if;

      --  Remaining processing depends on attribute

      case Attr_Id is

      ------------
      -- Access --
      ------------

      when Attribute_Access =>
         Analyze_Access_Attribute;

      -------------
      -- Address --
      -------------

      when Attribute_Address =>
         Check_E0;

         --  Check for some junk cases, where we have to allow the address
         --  attribute but it does not make much sense, so at least for now
         --  just replace with Null_Address.

         --  We also do this if the prefix is a reference to the AST_Entry
         --  attribute. If expansion is active, the attribute will be
         --  replaced by a function call, and address will work fine and
         --  get the proper value, but if expansion is not active, then
         --  the check here allows proper semantic analysis of the reference.

         --  An Address attribute created by expansion is legal even when it
         --  applies to other entity-denoting expressions.

         if Is_Entity_Name (P) then
            declare
               Ent : constant Entity_Id := Entity (P);

            begin
               if Is_Subprogram (Ent) then
                  if not Is_Library_Level_Entity (Ent) then
                     Check_Restriction (No_Implicit_Dynamic_Code, P);
                  end if;

                  Set_Address_Taken (Ent);

               elsif Is_Object (Ent)
                 or else Ekind (Ent) = E_Label
               then
                  Set_Address_Taken (Ent);

               --  If we have an address of an object, and the attribute
               --  comes from source, then set the object as potentially
               --  source modified. We do this because the resulting address
               --  can potentially be used to modify the variable and we
               --  might not detect this, leading to some junk warnings.

                  Set_Never_Set_In_Source (Ent, False);

               elsif Ekind (Ent) = E_Package
                 or else Is_Generic_Unit (Ent)
               then
                  Rewrite (N,
                    New_Occurrence_Of (RTE (RE_Null_Address), Sloc (N)));

               else
                  Error_Attr ("invalid prefix for % attribute", P);
               end if;
            end;

         elsif Is_Object_Reference (P) then
            null;

         elsif Nkind (P) = N_Selected_Component
           and then Is_Subprogram (Entity (Selector_Name (P)))
         then
            null;

         --  What exactly are we allowing here ??? and is this properly
         --  documented in the sinfo documentation for this node ???

         elsif not Comes_From_Source (N) then
            null;

         else
            Error_Attr ("invalid prefix for % attribute", P);
         end if;

         Set_Etype (N, RTE (RE_Address));

      ------------------
      -- Address_Size --
      ------------------

      when Attribute_Address_Size =>
         Standard_Attribute (System_Address_Size);

      ---------------
      -- Alignment --
      ---------------

      when Attribute_Alignment =>

         --  Don't we need more checking here, cf Size ???

         Check_E0;
         Check_Not_Incomplete_Type;
         Set_Etype (N, Universal_Integer);

      ----------
      -- Base --
      ----------

      --  Note: when the base attribute appears in the context of a subtype
      --  mark, the analysis is done by Sem_Ch8.Find_Type, rather than by
      --  the following circuit.

      when Attribute_Base => Base : declare
         Typ : Entity_Id;

      begin
         Check_Either_E0_Or_E1;
         Find_Type (P);
         Typ := Entity (P);

         if Ada_95
           and then not Is_Scalar_Type (Typ)
           and then not Is_Generic_Type (Typ)
         then
            Error_Msg_N ("prefix of Base attribute must be scalar type", N);

         elsif Sloc (Typ) = Standard_Location
           and then Base_Type (Typ) = Typ
           and then Warn_On_Redundant_Constructs
         then
            Error_Msg_NE
              ("?redudant attribute, & is its own base type", N, Typ);
         end if;

         Set_Etype (N, Base_Type (Entity (P)));

         --  If we have an expression present, then really this is a conversion
         --  and the tree must be reformed. Note that this is one of the cases
         --  in which we do a replace rather than a rewrite, because the
         --  original tree is junk.

         if Present (E1) then
            Replace (N,
              Make_Type_Conversion (Loc,
                Subtype_Mark =>
                  Make_Attribute_Reference (Loc,
                    Prefix => Prefix (N),
                    Attribute_Name => Name_Base),
                Expression => Relocate_Node (E1)));

            --  E1 may be overloaded, and its interpretations preserved.

            Save_Interps (E1, Expression (N));
            Analyze (N);

         --  For other cases, set the proper type as the entity of the
         --  attribute reference, and then rewrite the node to be an
         --  occurrence of the referenced base type. This way, no one
         --  else in the compiler has to worry about the base attribute.

         else
            Set_Entity (N, Base_Type (Entity (P)));
            Rewrite (N,
              New_Reference_To (Entity (N), Loc));
            Analyze (N);
         end if;
      end Base;

      ---------
      -- Bit --
      ---------

      when Attribute_Bit => Bit :
      begin
         Check_E0;

         if not Is_Object_Reference (P) then
            Error_Attr ("prefix for % attribute must be object", P);

         --  What about the access object cases ???

         else
            null;
         end if;

         Set_Etype (N, Universal_Integer);
      end Bit;

      ------------------
      -- Bit_Position --
      ------------------

      --  Note: in generated code, we can have a Bit_Position attribute
      --  applied to a (naked) record component (i.e. the prefix is an
      --  identifier that references an E_Component or E_Discriminant
      --  entity directly, and this is interpreted as expected by Gigi.
      --  The following code will not tolerate such usage, but when the
      --  expander creates this special case, it marks it as analyzed
      --  immediately and sets an appropriate type.

      when Attribute_Bit_Position =>

         if Comes_From_Source (N) then
            Check_Component;
         end if;

         Set_Etype (N, Universal_Integer);

      -----------
      -- Class --
      -----------

      when Attribute_Class => Class : declare
      begin
         Check_Restriction (No_Dispatch, N);
         Check_Either_E0_Or_E1;

         --  If we have an expression present, then really this is a conversion
         --  and the tree must be reformed into a proper conversion. This is a
         --  Replace rather than a Rewrite, because the original tree is junk.
         --  If expression is overloaded, propagate interpretations to new one.

         if Present (E1) then
            Replace (N,
              Make_Type_Conversion (Loc,
                Subtype_Mark =>
                  Make_Attribute_Reference (Loc,
                    Prefix => Prefix (N),
                    Attribute_Name => Name_Class),
                Expression => Relocate_Node (E1)));

            Save_Interps (E1, Expression (N));
            Analyze (N);

         --  Otherwise we just need to find the proper type

         else
            Find_Type (N);
         end if;

      end Class;

      --------------------
      -- Component_Size --
      --------------------

      when Attribute_Component_Size =>
         Check_E0;
         Set_Etype (N, Universal_Integer);

         --  Note: unlike other array attributes, unconstrained arrays are OK

         if Is_Array_Type (P_Type) and then not Is_Constrained (P_Type) then
            null;
         else
            Check_Array_Type;
         end if;

      -----------------
      -- Constrained --
      -----------------

      when Attribute_Constrained =>
         Check_E0;
         Set_Etype (N, Standard_Boolean);

         --  Case from RM J.4(2) of constrained applied to private type

         if Is_Entity_Name (P) and then Is_Type (Entity (P)) then

            --  If we are within an instance, the attribute must be legal
            --  because it was valid in the generic unit. Ditto if this is
            --  an inlining of a function declared in an instance.

            if In_Instance
              or else In_Inlined_Body
            then
               return;

            --  For sure OK if we have a real private type itself, but must
            --  be completed, cannot apply Constrained to incomplete type.

            elsif Is_Private_Type (Entity (P)) then

               --  Note: this is one of the Annex J features that does not
               --  generate a warning from -gnatwj, since in fact it seems
               --  very useful, and is used in the GNAT runtime.

               Check_Not_Incomplete_Type;
               return;
            end if;

         --  Normal (non-obsolescent case) of application to object of
         --  a discriminated type.

         else
            Check_Object_Reference (P);

            --  If N does not come from source, then we allow the
            --  the attribute prefix to be of a private type whose
            --  full type has discriminants. This occurs in cases
            --  involving expanded calls to stream attributes.

            if not Comes_From_Source (N) then
               P_Type := Underlying_Type (P_Type);
            end if;

            --  Also allow an object of a generic type if extensions allowed
            --  and allow this for any type at all.

            if (Is_Generic_Type (P_Type)
                     or else Is_Generic_Actual_Type (P_Type))
              and then Extensions_Allowed
            then
               return;
            end if;
         end if;

         --  Fall through if bad prefix

         Error_Attr
           ("prefix of % attribute must be object of discriminated type", P);

      --------------
      -- Enum_Rep --
      --------------

      when Attribute_Enum_Rep => Enum_Rep : declare
      begin
         if Present (E1) then
            Check_E1;
            Check_Discrete_Type;
            Resolve (E1, P_Base_Type);

         else
            if not Is_Entity_Name (P)
              or else (not Is_Object (Entity (P))
                         and then
                       Ekind (Entity (P)) /= E_Enumeration_Literal)
            then
               Error_Attr
                 ("prefix of %attribute must be " &
                  "discrete type/object or enum literal", P);
            end if;
         end if;

         Set_Etype (N, Universal_Integer);
      end Enum_Rep;

      --------------
      -- Exponent --
      --------------

      when Attribute_Exponent =>
         Check_Floating_Point_Type_1;
         Set_Etype (N, Universal_Integer);
         Resolve (E1, P_Base_Type);

      -----------
      -- First --
      -----------

      when Attribute_First =>
         Check_Array_Or_Scalar_Type;

      ---------------
      -- First_Bit --
      ---------------

      when Attribute_First_Bit =>
         Check_Component;
         Set_Etype (N, Universal_Integer);

      -----------
      -- Floor --
      -----------

      when Attribute_Floor =>
         Check_Floating_Point_Type_1;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);

      ----------
      -- Fore --
      ----------

      when Attribute_Fore => null;

      --------------
      -- Fraction --
      --------------

      when Attribute_Fraction =>
         Check_Floating_Point_Type_1;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);

      -----------
      -- Image --
      -----------

      when Attribute_Image => Image :
      begin
         Set_Etype (N, Standard_String);
         Check_Scalar_Type;

         if Is_Enumeration_Type (P_Type) then
            Check_Restriction (No_Enumeration_Maps, N);
         end if;

         Check_E1;
         Resolve (E1, P_Base_Type);
         Check_Enum_Image;
         Validate_Non_Static_Attribute_Function_Call;
      end Image;

      ---------
      -- Img --
      ---------

      when Attribute_Img => Img :
      begin
         Set_Etype (N, Standard_String);

         if not Is_Scalar_Type (P_Type)
           or else (Is_Entity_Name (P) and then Is_Type (Entity (P)))
         then
            Error_Attr
              ("prefix of % attribute must be scalar object name", N);
         end if;

         Check_Enum_Image;
      end Img;

      -------------------
      -- Integer_Value --
      -------------------

      when Attribute_Integer_Value =>
         Check_E1;
         Check_Integer_Type;
         Resolve (E1, Any_Fixed);
         Set_Etype (N, P_Base_Type);

      ----------
      -- Last --
      ----------

      when Attribute_Last =>
         Check_Array_Or_Scalar_Type;

      --------------
      -- Last_Bit --
      --------------

      when Attribute_Last_Bit =>
         Check_Component;
         Set_Etype (N, Universal_Integer);

      ------------
      -- Length --
      ------------

      when Attribute_Length =>
         Check_Array_Type;
         Set_Etype (N, Universal_Integer);

      ------------------
      -- Machine_Emax --
      ------------------

      when Attribute_Machine_Emax =>
         Check_Floating_Point_Type_0;
         Set_Etype (N, Universal_Integer);

      ------------------
      -- Machine_Emin --
      ------------------

      when Attribute_Machine_Emin =>
         Check_Floating_Point_Type_0;
         Set_Etype (N, Universal_Integer);

      ----------------------
      -- Machine_Mantissa --
      ----------------------

      when Attribute_Machine_Mantissa =>
         Check_Floating_Point_Type_0;
         Set_Etype (N, Universal_Integer);

      -----------------------
      -- Machine_Overflows --
      -----------------------

      when Attribute_Machine_Overflows =>
         Check_Real_Type;
         Check_E0;
         Set_Etype (N, Standard_Boolean);

      -------------------
      -- Machine_Radix --
      -------------------

      when Attribute_Machine_Radix =>
         Check_Real_Type;
         Check_E0;
         Set_Etype (N, Universal_Integer);

      --------------------
      -- Machine_Rounds --
      --------------------

      when Attribute_Machine_Rounds =>
         Check_Real_Type;
         Check_E0;
         Set_Etype (N, Standard_Boolean);

      ------------------
      -- Machine_Size --
      ------------------

      when Attribute_Machine_Size =>
         Check_E0;
         Check_Type;
         Check_Not_Incomplete_Type;
         Set_Etype (N, Universal_Integer);

      --------------
      -- Mantissa --
      --------------

      when Attribute_Mantissa =>
         Check_E0;
         Check_Real_Type;
         Set_Etype (N, Universal_Integer);

      ---------
      -- Max --
      ---------

      when Attribute_Max =>
         Check_E2;
         Check_Scalar_Type;
         Resolve (E1, P_Base_Type);
         Resolve (E2, P_Base_Type);
         Set_Etype (N, P_Base_Type);

      ----------------------------------
      -- Max_Size_In_Storage_Elements --
      ----------------------------------

      when Attribute_Max_Size_In_Storage_Elements =>
         Check_E0;
         Check_Type;
         Check_Not_Incomplete_Type;
         Set_Etype (N, Universal_Integer);

      ---------
      -- Min --
      ---------

      when Attribute_Min =>
         Check_E2;
         Check_Scalar_Type;
         Resolve (E1, P_Base_Type);
         Resolve (E2, P_Base_Type);
         Set_Etype (N, P_Base_Type);

      -----------
      -- Model --
      -----------

      when Attribute_Model =>
         Check_Floating_Point_Type_1;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);

      -------------
      -- Modulus --
      -------------

      when Attribute_Modulus =>
         Check_E0;
         Check_Type;

         if not Is_Modular_Integer_Type (P_Type) then
            Error_Attr ("prefix of % attribute must be modular type", P);
         end if;

         Set_Etype (N, Universal_Integer);

      -----------------
      -- Object_Size --
      -----------------

      when Attribute_Object_Size =>
         Check_E0;
         Check_Type;
         Check_Not_Incomplete_Type;
         Set_Etype (N, Universal_Integer);

      -------------------------
      -- Passed_By_Reference --
      -------------------------

      when Attribute_Passed_By_Reference =>
         Check_E0;
         Check_Type;
         Set_Etype (N, Standard_Boolean);

      ------------------
      -- Pool_Address --
      ------------------

      when Attribute_Pool_Address =>
         Check_E0;
         Set_Etype (N, RTE (RE_Address));

      ---------
      -- Pos --
      ---------

      when Attribute_Pos =>
         Check_Discrete_Type;
         Check_E1;
         Resolve (E1, P_Base_Type);
         Set_Etype (N, Universal_Integer);

      --------------
      -- Position --
      --------------

      when Attribute_Position =>
         Check_Component;
         Set_Etype (N, Universal_Integer);

      -----------
      -- Range --
      -----------

      when Attribute_Range =>
         Check_Array_Or_Scalar_Type;

      ------------------
      -- Range_Length --
      ------------------

      when Attribute_Range_Length =>
         Check_Discrete_Type;
         Set_Etype (N, Universal_Integer);

      ---------------
      -- Remainder --
      ---------------

      when Attribute_Remainder =>
         Check_Floating_Point_Type_2;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);
         Resolve (E2, P_Base_Type);

      -----------
      -- Round --
      -----------

      when Attribute_Round =>
         Check_E1;
         Check_Decimal_Fixed_Point_Type;
         Set_Etype (N, P_Base_Type);

         --  Because the context is universal_real (3.5.10(12)) it is a legal
         --  context for a universal fixed expression. This is the only
         --  attribute whose functional description involves U_R.

         if Etype (E1) = Universal_Fixed then
            declare
               Conv : constant Node_Id := Make_Type_Conversion (Loc,
                  Subtype_Mark => New_Occurrence_Of (Universal_Real, Loc),
                  Expression   => Relocate_Node (E1));

            begin
               Rewrite (E1, Conv);
               Analyze (E1);
            end;
         end if;

         Resolve (E1, Any_Real);

      --------------
      -- Rounding --
      --------------

      when Attribute_Rounding =>
         Check_Floating_Point_Type_1;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);

      -----------
      -- Scale --
      -----------

      when Attribute_Scale =>
         Check_E0;
         Check_Decimal_Fixed_Point_Type;
         Set_Etype (N, Universal_Integer);

      -------------
      -- Scaling --
      -------------

      when Attribute_Scaling =>
         Check_Floating_Point_Type_2;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);

      ----------
      -- Size --
      ----------

      when Attribute_Size =>
         Check_E0;

         if Is_Object_Reference (P)
           or else (Is_Entity_Name (P)
                     and then Ekind (Entity (P)) = E_Function)
         then
            Check_Object_Reference (P);

         elsif Is_Entity_Name (P)
           and then Is_Type (Entity (P))
         then
            null;

         elsif Nkind (P) = N_Type_Conversion
           and then not Comes_From_Source (P)
         then
            null;

         else
            Error_Attr ("invalid prefix for % attribute", P);
         end if;

         Check_Not_Incomplete_Type;
         Set_Etype (N, Universal_Integer);

      ------------------
      -- Storage_Pool --
      ------------------

      when Attribute_Storage_Pool =>
         if Is_Access_Type (P_Type) then
            Check_E0;

            --  Set appropriate entity

            if Present (Associated_Storage_Pool (Root_Type (P_Type))) then
               Set_Entity (N, Associated_Storage_Pool (Root_Type (P_Type)));
            else
               Set_Entity (N, RTE (RE_Global_Pool_Object));
            end if;

            Set_Etype (N, Class_Wide_Type (RTE (RE_Root_Storage_Pool)));

         else
            Error_Attr ("prefix of % attribute must be access type", P);
         end if;

      ------------------
      -- Storage_Size --
      ------------------

      when Attribute_Storage_Size =>

         if Is_Access_Type (P_Type) then
            if Is_Entity_Name (P)
              and then Is_Type (Entity (P))
            then
               Check_E0;
               Check_Type;
               Set_Etype (N, Universal_Integer);

            end if;
         end if;

      ------------------
      -- Storage_Unit --
      ------------------

      when Attribute_Storage_Unit =>
         Standard_Attribute (Ttypes.System_Storage_Unit);

      ----------
      -- Succ --
      ----------

      when Attribute_Succ =>
         Check_Scalar_Type;
         Check_E1;
         Resolve (E1, P_Base_Type);
         Set_Etype (N, P_Base_Type);

         --  Nothing to do for real type case

         if Is_Real_Type (P_Type) then
            null;
         end if;

      ---------
      -- Tag --
      ---------

      when Attribute_Tag =>
         Check_E0;
         Check_Dereference;

         if not Is_Tagged_Type (P_Type) then
            Error_Attr ("prefix of % attribute must be tagged", P);

         --  Next test does not apply to generated code
         --  why not, and what does the illegal reference mean???

         elsif Is_Object_Reference (P)
           and then not Is_Class_Wide_Type (P_Type)
           and then Comes_From_Source (N)
         then
            Error_Attr
              ("% attribute can only be applied to objects of class-wide type",
               P);
         end if;

         Set_Etype (N, RTE (RE_Tag));

      -----------------
      -- Target_Name --
      -----------------

      when Attribute_Target_Name => Target_Name : declare
         TN : constant String := Sdefault.Target_Name.all;
         TL : Integer := TN'Last;

      begin
         Check_Standard_Prefix;
         Check_E0;
         Start_String;

         if TN (TL) = '/' or else TN (TL) = '\' then
            TL := TL - 1;
         end if;

         Store_String_Chars (TN (TN'First .. TL));

         Rewrite (N,
           Make_String_Literal (Loc,
             Strval => End_String));
         Analyze_And_Resolve (N, Standard_String);
      end Target_Name;

      ----------------
      -- Terminated --
      ----------------

      when Attribute_Terminated => null;

      ----------------
      -- To_Address --
      ----------------

      when Attribute_To_Address =>
         Check_E1;
         Analyze (P);

         if Nkind (P) /= N_Identifier
           or else Chars (P) /= Name_System
         then
            Error_Attr ("prefix of %attribute must be System", P);
         end if;

         Generate_Reference (RTE (RE_Address), P);
         Analyze_And_Resolve (E1, Any_Integer);
         Set_Etype (N, RTE (RE_Address));

      ----------------
      -- Truncation --
      ----------------

      when Attribute_Truncation =>
         Check_Floating_Point_Type_1;
         Resolve (E1, P_Base_Type);
         Set_Etype (N, P_Base_Type);

      ----------------
      -- Type_Class --
      ----------------

      when Attribute_Type_Class =>
         Check_E0;
         Check_Type;
         Check_Not_Incomplete_Type;
         Set_Etype (N, RTE (RE_Type_Class));

      -----------------------
      -- Unbiased_Rounding --
      -----------------------

      when Attribute_Unbiased_Rounding =>
         Check_Floating_Point_Type_1;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);

      ----------------------
      -- Unchecked_Access --
      ----------------------

      when Attribute_Unchecked_Access =>
         if Comes_From_Source (N) then
            Check_Restriction (No_Unchecked_Access, N);
         end if;

         Analyze_Access_Attribute;

      -------------------------
      -- Unconstrained_Array --
      -------------------------

      when Attribute_Unconstrained_Array =>
         Check_E0;
         Check_Type;
         Check_Not_Incomplete_Type;
         Set_Etype (N, Standard_Boolean);

      ------------------------------
      -- Universal_Literal_String --
      ------------------------------

      --  This is a GNAT specific attribute whose prefix must be a named
      --  number where the expression is either a single numeric literal,
      --  or a numeric literal immediately preceded by a minus sign. The
      --  result is equivalent to a string literal containing the text of
      --  the literal as it appeared in the source program with a possible
      --  leading minus sign.

      when Attribute_Universal_Literal_String => Universal_Literal_String :
      begin
         Check_E0;

         if not Is_Entity_Name (P)
           or else Ekind (Entity (P)) not in Named_Kind
         then
            Error_Attr ("prefix for % attribute must be named number", P);

         else
            declare
               Expr     : Node_Id;
               Negative : Boolean;
               S        : Source_Ptr;
               Src      : Source_Buffer_Ptr;

            begin
               Expr := Original_Node (Expression (Parent (Entity (P))));

               if Nkind (Expr) = N_Op_Minus then
                  Negative := True;
                  Expr := Original_Node (Right_Opnd (Expr));
               else
                  Negative := False;
               end if;

               if Nkind (Expr) /= N_Integer_Literal
                 and then Nkind (Expr) /= N_Real_Literal
               then
                  Error_Attr
                    ("named number for % attribute must be simple literal", N);
               end if;

               --  Build string literal corresponding to source literal text

               Start_String;

               if Negative then
                  Store_String_Char (Get_Char_Code ('-'));
               end if;

               S := Sloc (Expr);
               Src := Source_Text (Get_Source_File_Index (S));

               while Src (S) /= ';' and then Src (S) /= ' ' loop
                  Store_String_Char (Get_Char_Code (Src (S)));
                  S := S + 1;
               end loop;

               --  Now we rewrite the attribute with the string literal

               Rewrite (N,
                 Make_String_Literal (Loc, End_String));
               Analyze (N);
            end;
         end if;
      end Universal_Literal_String;

      -------------------------
      -- Unrestricted_Access --
      -------------------------

      --  This is a GNAT specific attribute which is like Access except that
      --  all scope checks and checks for aliased views are omitted.

      when Attribute_Unrestricted_Access =>
         if Comes_From_Source (N) then
            Check_Restriction (No_Unchecked_Access, N);
         end if;

         if Is_Entity_Name (P) then
            Set_Address_Taken (Entity (P));
         end if;

         Analyze_Access_Attribute;

      ---------
      -- Val --
      ---------

      when Attribute_Val => Val : declare
      begin
         Check_E1;
         Check_Discrete_Type;
         Resolve (E1, Any_Integer);
         Set_Etype (N, P_Base_Type);

         --  Note, we need a range check in general, but we wait for the
         --  Resolve call to do this, since we want to let Eval_Attribute
         --  have a chance to find an static illegality first!
      end Val;

      -----------
      -- Valid --
      -----------

      when Attribute_Valid =>
         Check_E0;

         --  Ignore check for object if we have a 'Valid reference generated
         --  by the expanded code, since in some cases valid checks can occur
         --  on items that are names, but are not objects (e.g. attributes).

         if Comes_From_Source (N) then
            Check_Object_Reference (P);
         end if;

         if not Is_Scalar_Type (P_Type) then
            Error_Attr ("object for % attribute must be of scalar type", P);
         end if;

         Set_Etype (N, Standard_Boolean);

      -----------
      -- Value --
      -----------

      when Attribute_Value => Value :
      begin
         Check_E1;
         Check_Scalar_Type;

         if Is_Enumeration_Type (P_Type) then
            Check_Restriction (No_Enumeration_Maps, N);
         end if;

         --  Set Etype before resolving expression because expansion of
         --  expression may require enclosing type. Note that the type
         --  returned by 'Value is the base type of the prefix type.

         Set_Etype (N, P_Base_Type);
         Validate_Non_Static_Attribute_Function_Call;
      end Value;

      ----------------
      -- Value_Size --
      ----------------

      when Attribute_Value_Size =>
         Check_E0;
         Check_Type;
         Check_Not_Incomplete_Type;
         Set_Etype (N, Universal_Integer);

      ------------------
      -- Wchar_T_Size --
      ------------------

      when Attribute_Wchar_T_Size =>
         Standard_Attribute (Interfaces_Wchar_T_Size);

      ----------------
      -- Wide_Image --
      ----------------

      when Attribute_Wide_Image => Wide_Image :
      begin
         Check_Scalar_Type;
         Set_Etype (N, Standard_Wide_String);
         Check_E1;
         Resolve (E1, P_Base_Type);
         Validate_Non_Static_Attribute_Function_Call;
      end Wide_Image;

      ----------------
      -- Wide_Value --
      ----------------

      when Attribute_Wide_Value => Wide_Value :
      begin
         Check_E1;
         Check_Scalar_Type;

         --  Set Etype before resolving expression because expansion
         --  of expression may require enclosing type.

         Set_Etype (N, P_Type);
         Validate_Non_Static_Attribute_Function_Call;
      end Wide_Value;

      ---------------
      -- Word_Size --
      ---------------

      when Attribute_Word_Size =>
         Standard_Attribute (System_Word_Size);

      -----------
      end case;

   --  All errors raise Bad_Attribute, so that we get out before any further
   --  damage occurs when an error is detected (for example, if we check for
   --  one attribute expression, and the check succeeds, we want to be able
   --  to proceed securely assuming that an expression is in fact present.

   exception
      when Bad_Attribute =>
         Set_Etype (N, Any_Type);
         return;

   end Analyze_Attribute;

   --------------------
   -- Eval_Attribute --
   --------------------

   procedure Eval_Attribute (N : Node_Id) is
      Loc   : constant Source_Ptr   := Sloc (N);
      Aname : constant Name_Id      := Attribute_Name (N);
      Id    : constant Attribute_Id := Get_Attribute_Id (Aname);
      P     : constant Node_Id      := Prefix (N);

      C_Type : constant Entity_Id := Etype (N);
      --  The type imposed by the context.

      E1 : Node_Id;
      --  First expression, or Empty if none

      E2 : Node_Id;
      --  Second expression, or Empty if none

      P_Entity : Entity_Id;
      --  Entity denoted by prefix

      P_Type : Entity_Id;
      --  The type of the prefix

      P_Base_Type : Entity_Id;
      --  The base type of the prefix type

      P_Root_Type : Entity_Id;
      --  The root type of the prefix type

      Static : Boolean;
      --  True if the result is Static. This is set by the general processing
      --  to true if the prefix is static, and all expressions are static. It
      --  can be reset as processing continues for particular attributes

      Lo_Bound, Hi_Bound : Node_Id;
      --  Expressions for low and high bounds of type or array index referenced
      --  by First, Last, or Length attribute for array, set by Set_Bounds.

      ------------CE_Node : Node_Id;
      --  Constraint error node used if we have an attribute reference has
      --  an argument that raises a constraint error. In this case we replace
      --  the attribute with a raise constraint_error node. This is important
      --  processing, since otherwise gigi might see an attribute which it is
      --  unprepared to deal with.

      procedure Check_Expressions;
      --  In case where the attribute is not foldable, the expressions, if
      --  any, of the attribute, are in a non-static context. This procedure
      --  performs the required additional checks.

      function Compile_Time_Known_Bounds (Typ : Entity_Id) return Boolean;
      --  Determines if the given type has compile time known bounds. Note
      --  that we enter the case statement even in cases where the prefix
      --  type does NOT have known bounds, so it is important to guard any
      --  attempt to evaluate both bounds with a call to this function.

      procedure Compile_Time_Known_Attribute (N : Node_Id; Val : Uint);
      --  This procedure is called when the attribute N has a non-static
      --  but compile time known value given by Val. It includes the
      --  necessary checks for out of range values.

      procedure Float_Attribute_Universal_Integer
        (IEEES_Val : Int;
         IEEEL_Val : Int;
         IEEEX_Val : Int;
         VAXFF_Val : Int;
         VAXDF_Val : Int;
         VAXGF_Val : Int;
         AAMPS_Val : Int;
         AAMPL_Val : Int);
      --  This procedure evaluates a float attribute with no arguments that
      --  returns a universal integer result. The parameters give the values
      --  for the possible floating-point root types. See ttypef for details.
      --  The prefix type is a float type (and is thus not a generic type).

      procedure Float_Attribute_Universal_Real
        (IEEES_Val : String;
         IEEEL_Val : String;
         IEEEX_Val : String;
         VAXFF_Val : String;
         VAXDF_Val : String;
         VAXGF_Val : String;
         AAMPS_Val : String;
         AAMPL_Val : String);
      --  This procedure evaluates a float attribute with no arguments that
      --  returns a universal real result. The parameters give the values
      --  required for the possible floating-point root types in string
      --  format as real literals with a possible leading minus sign.
      --  The prefix type is a float type (and is thus not a generic type).

      function Fore_Value return Nat;
      --  Computes the Fore value for the current attribute prefix, which is
      --  known to be a static fixed-point type. Used by Fore and Width.

      function Mantissa return Uint;
      --  Returns the Mantissa value for the prefix type

      procedure Set_Bounds;
      --  Used for First, Last and Length attributes applied to an array or
      --  array subtype. Sets the variables Lo_Bound and Hi_Bound to the low
      --  and high bound expressions for the index referenced by the attribute
      --  designator (i.e. the first index if no expression is present, and
      --  the N'th index if the value N is present as an expression). Also
      --  used for First and Last of scalar types. Static is reset to False
      --  if the type or index type is not statically constrained.

      -----------------------
      -- Check_Expressions --
      -----------------------

      procedure Check_Expressions is
         E : Node_Id := E1;

      begin
         while Present (E) loop
            Check_Non_Static_Context (E);
            Next (E);
         end loop;
      end Check_Expressions;

      ----------------------------------
      -- Compile_Time_Known_Attribute --
      ----------------------------------

      procedure Compile_Time_Known_Attribute (N : Node_Id; Val : Uint) is
         T : constant Entity_Id := Etype (N);

      begin
         Fold_Uint (N, Val, False);

         --  Check that result is in bounds of the type if it is static

         if Is_In_Range (N, T) then
            null;

         elsif Is_Out_Of_Range (N, T) then
            Apply_Compile_Time_Constraint_Error
              (N, "value not in range of}?", CE_Range_Check_Failed);

         else
            Set_Do_Range_Check (N, False);
         end if;
      end Compile_Time_Known_Attribute;

      -------------------------------
      -- Compile_Time_Known_Bounds --
      -------------------------------

      function Compile_Time_Known_Bounds (Typ : Entity_Id) return Boolean is
      begin
         return
           Compile_Time_Known_Value (Type_Low_Bound (Typ))
             and then
           Compile_Time_Known_Value (Type_High_Bound (Typ));
      end Compile_Time_Known_Bounds;

      ---------------------------------------
      -- Float_Attribute_Universal_Integer --
      ---------------------------------------

      procedure Float_Attribute_Universal_Integer
        (IEEES_Val : Int;
         IEEEL_Val : Int;
         IEEEX_Val : Int;
         VAXFF_Val : Int;
         VAXDF_Val : Int;
         VAXGF_Val : Int;
         AAMPS_Val : Int;
         AAMPL_Val : Int)
      is
         Val  : Int;
         Digs : constant Nat := UI_To_Int (Digits_Value (P_Base_Type));

      begin
         if Vax_Float (P_Base_Type) then
            if Digs = VAXFF_Digits then
               Val := VAXFF_Val;
            elsif Digs = VAXDF_Digits then
               Val := VAXDF_Val;
            else pragma Assert (Digs = VAXGF_Digits);
               Val := VAXGF_Val;
            end if;

         elsif Is_AAMP_Float (P_Base_Type) then
            if Digs = AAMPS_Digits then
               Val := AAMPS_Val;
            else pragma Assert (Digs = AAMPL_Digits);
               Val := AAMPL_Val;
            end if;

         else
            if Digs = IEEES_Digits then
               Val := IEEES_Val;
            elsif Digs = IEEEL_Digits then
               Val := IEEEL_Val;
            else pragma Assert (Digs = IEEEX_Digits);
               Val := IEEEX_Val;
            end if;
         end if;

         Fold_Uint (N, UI_From_Int (Val), True);
      end Float_Attribute_Universal_Integer;

      ------------------------------------
      -- Float_Attribute_Universal_Real --
      ------------------------------------

      procedure Float_Attribute_Universal_Real
        (IEEES_Val : String;
         IEEEL_Val : String;
         IEEEX_Val : String;
         VAXFF_Val : String;
         VAXDF_Val : String;
         VAXGF_Val : String;
         AAMPS_Val : String;
         AAMPL_Val : String)
      is
         Val  : Node_Id;
         Digs : constant Nat := UI_To_Int (Digits_Value (P_Base_Type));

      begin
         if Vax_Float (P_Base_Type) then
            if Digs = VAXFF_Digits then
               Val := Real_Convert (VAXFF_Val);
            elsif Digs = VAXDF_Digits then
               Val := Real_Convert (VAXDF_Val);
            else pragma Assert (Digs = VAXGF_Digits);
               Val := Real_Convert (VAXGF_Val);
            end if;

         elsif Is_AAMP_Float (P_Base_Type) then
            if Digs = AAMPS_Digits then
               Val := Real_Convert (AAMPS_Val);
            else pragma Assert (Digs = AAMPL_Digits);
               Val := Real_Convert (AAMPL_Val);
            end if;

         else
            if Digs = IEEES_Digits then
               Val := Real_Convert (IEEES_Val);
            elsif Digs = IEEEL_Digits then
               Val := Real_Convert (IEEEL_Val);
            else pragma Assert (Digs = IEEEX_Digits);
               Val := Real_Convert (IEEEX_Val);
            end if;
         end if;

         Set_Sloc (Val, Loc);
         Rewrite (N, Val);
         Set_Is_Static_Expression (N, Static);
         Analyze_And_Resolve (N, C_Type);
      end Float_Attribute_Universal_Real;

      ----------------
      -- Fore_Value --
      ----------------

      --  Note that the Fore calculation is based on the actual values
      --  of the bounds, and does not take into account possible rounding.

      function Fore_Value return Nat is
         Lo      : constant Uint  := Expr_Value (Type_Low_Bound (P_Type));
         Hi      : constant Uint  := Expr_Value (Type_High_Bound (P_Type));
         Small   : constant Ureal := Small_Value (P_Type);
         Lo_Real : constant Ureal := Lo * Small;
         Hi_Real : constant Ureal := Hi * Small;
         T       : Ureal;
         R       : Nat;

      begin
         --  Bounds are given in terms of small units, so first compute
         --  proper values as reals.

         T := UR_Max (abs Lo_Real, abs Hi_Real);
         R := 2;

         --  Loop to compute proper value if more than one digit required

         while T >= Ureal_10 loop
            R := R + 1;
            T := T / Ureal_10;
         end loop;

         return R;
      end Fore_Value;

      --------------
      -- Mantissa --
      --------------

      --  Table of mantissa values accessed by function  Computed using
      --  the relation:

      --    T'Mantissa = integer next above (D * log(10)/log(2)) + 1)

      --  where D is T'Digits (RM83 3.5.7)

      Mantissa_Value : constant array (Nat range 1 .. 40) of Nat := (
          1 =>   5,
          2 =>   8,
          3 =>  11,
          4 =>  15,
          5 =>  18,
          6 =>  21,
          7 =>  25,
          8 =>  28,
          9 =>  31,
         10 =>  35,
         11 =>  38,
         12 =>  41,
         13 =>  45,
         14 =>  48,
         15 =>  51,
         16 =>  55,
         17 =>  58,
         18 =>  61,
         19 =>  65,
         20 =>  68,
         21 =>  71,
         22 =>  75,
         23 =>  78,
         24 =>  81,
         25 =>  85,
         26 =>  88,
         27 =>  91,
         28 =>  95,
         29 =>  98,
         30 => 101,
         31 => 104,
         32 => 108,
         33 => 111,
         34 => 114,
         35 => 118,
         36 => 121,
         37 => 124,
         38 => 128,
         39 => 131,
         40 => 134);

      function Mantissa return Uint is
      begin
         return
           UI_From_Int (Mantissa_Value (UI_To_Int (Digits_Value (P_Type))));
      end Mantissa;

      ----------------
      -- Set_Bounds --
      ----------------

      procedure Set_Bounds is
         Ndim : Nat;
         Indx : Node_Id;
         Ityp : Entity_Id;

      begin
         --  For a string literal subtype, we have to construct the bounds.
         --  Valid Ada code never applies attributes to string literals, but
         --  it is convenient to allow the expander to generate attribute
         --  references of this type (e.g. First and Last applied to a string
         --  literal).

         --  Note that the whole point of the E_String_Literal_Subtype is to
         --  avoid this construction of bounds, but the cases in which we
         --  have to materialize them are rare enough that we don't worry!

         --  The low bound is simply the low bound of the base type. The
         --  high bound is computed from the length of the string and this
         --  low bound.

         if Ekind (P_Type) = E_String_Literal_Subtype then
            Ityp := Etype (First_Index (Base_Type (P_Type)));
            Lo_Bound := Type_Low_Bound (Ityp);

            Hi_Bound :=
              Make_Integer_Literal (Sloc (P),
                Intval =>
                  Expr_Value (Lo_Bound) + String_Literal_Length (P_Type) - 1);

            Set_Parent (Hi_Bound, P);
            Analyze_And_Resolve (Hi_Bound, Etype (Lo_Bound));
            return;

         --  For non-array case, just get bounds of scalar type

         elsif Is_Scalar_Type (P_Type) then
            Ityp := P_Type;

         --  For array case, get type of proper index

         else
            if No (E1) then
               Ndim := 1;
            else
               Ndim := UI_To_Int (Expr_Value (E1));
            end if;

            Indx := First_Index (P_Type);
            for J in 1 .. Ndim - 1 loop
               Next_Index (Indx);
            end loop;

            --  If no index type, get out (some other error occurred, and
            --  we don't have enough information to complete the job!)

            if No (Indx) then
               Lo_Bound := Error;
               Hi_Bound := Error;
               return;
            end if;

            Ityp := Etype (Indx);
         end if;

         --  A discrete range in an index constraint is allowed to be a
         --  subtype indication. This is syntactically a pain, but should
         --  not propagate to the entity for the corresponding index subtype.
         --  After checking that the subtype indication is legal, the range
         --  of the subtype indication should be transfered to the entity.
         --  The attributes for the bounds should remain the simple retrievals
         --  that they are now.

         Lo_Bound := Type_Low_Bound (Ityp);
         Hi_Bound := Type_High_Bound (Ityp);

         if not Is_Static_Subtype (Ityp) then
            Static := False;
         end if;
      end Set_Bounds;

   --  Start of processing for Eval_Attribute

   begin
      --  Acquire first two expressions (at the moment, no attributes
      --  take more than two expressions in any case).

      if Present (Expressions (N)) then
         E1 := First (Expressions (N));
         E2 := Next (E1);
      else
         E1 := Empty;
         E2 := Empty;
      end if;

      --  Special processing for cases where the prefix is an object. For
      --  this purpose, a string literal counts as an object (attributes
      --  of string literals can only appear in generated code).

      if Is_Object_Reference (P) or else Nkind (P) = N_String_Literal then

         --  For Component_Size, the prefix is an array object, and we apply
         --  the attribute to the type of the object. This is allowed for
         --  both unconstrained and constrained arrays, since the bounds
         --  have no influence on the value of this attribute.

         if Id = Attribute_Component_Size then
            P_Entity := Etype (P);

         --  For First and Last, the prefix is an array object, and we apply
         --  the attribute to the type of the array, but we need a constrained
         --  type for this, so we use the actual subtype if available.

         elsif Id = Attribute_First
                 or else
               Id = Attribute_Last
                 or else
               Id = Attribute_Length
         then
            declare
               AS : constant Entity_Id := Get_Actual_Subtype_If_Available (P);

            begin
               if Present (AS) and then Is_Constrained (AS) then
                  P_Entity := AS;

               --  If we have an unconstrained type, cannot fold

               else
                  Check_Expressions;
                  return;
               end if;
            end;

         --  For Size, give size of object if available, otherwise we
         --  cannot fold Size.

         elsif Id = Attribute_Size then
            if Is_Entity_Name (P)
              and then Known_Esize (Entity (P))
            then
               Compile_Time_Known_Attribute (N, Esize (Entity (P)));
               return;

            else
               Check_Expressions;
               return;
            end if;

         --  For Alignment, give size of object if available, otherwise we
         --  cannot fold Alignment.

         elsif Id = Attribute_Alignment then
            if Is_Entity_Name (P)
              and then Known_Alignment (Entity (P))
            then
               Fold_Uint (N, Alignment (Entity (P)), False);
               return;

            else
               Check_Expressions;
               return;
            end if;

         --  No other attributes for objects are folded

         else
            Check_Expressions;
            return;
         end if;

      --  Cases where P is not an object. Cannot do anything if P is
      --  not the name of an entity.

      elsif not Is_Entity_Name (P) then
         Check_Expressions;
         return;

      --  Otherwise get prefix entity

      else
         P_Entity := Entity (P);
      end if;

      --  At this stage P_Entity is the entity to which the attribute
      --  is to be applied. This is usually simply the entity of the
      --  prefix, except in some cases of attributes for objects, where
      --  as described above, we apply the attribute to the object type.

      --  First foldable possibility is a scalar or array type (RM 4.9(7))
      --  that is not generic (generic types are eliminated by RM 4.9(25)).
      --  Note we allow non-static non-generic types at this stage as further
      --  described below.

      if Is_Type (P_Entity)
        and then (Is_Scalar_Type (P_Entity) or Is_Array_Type (P_Entity))
        and then (not Is_Generic_Type (P_Entity))
      then
         P_Type := P_Entity;

      --  Second foldable possibility is an array object (RM 4.9(8))

      elsif (Ekind (P_Entity) = E_Variable
               or else
             Ekind (P_Entity) = E_Constant)
        and then Is_Array_Type (Etype (P_Entity))
        and then (not Is_Generic_Type (Etype (P_Entity)))
      then
         P_Type := Etype (P_Entity);

         --  If the entity is an array constant with an unconstrained
         --  nominal subtype then get the type from the initial value.
         --  If the value has been expanded into assignments, the expression
         --  is not present and the attribute reference remains dynamic.
         --  We could do better here and retrieve the type ???

         if Ekind (P_Entity) = E_Constant
           and then not Is_Constrained (P_Type)
         then
            if No (Constant_Value (P_Entity)) then
               return;
            else
               P_Type := Etype (Constant_Value (P_Entity));
            end if;
         end if;

      --  Definite must be folded if the prefix is not a generic type,
      --  that is to say if we are within an instantiation. Same processing
      --  applies to the GNAT attributes Has_Discriminants, Type_Class,
      --  and Unconstrained_Array.

      elsif (Id = Attribute_Type_Class
               or else
             Id = Attribute_Unconstrained_Array)
        and then not Is_Generic_Type (P_Entity)
      then
         P_Type := P_Entity;

      --  We can fold 'Size applied to a type if the size is known
      --  (as happens for a size from an attribute definition clause).
      --  At this stage, this can happen only for types (e.g. record
      --  types) for which the size is always non-static. We exclude
      --  generic types from consideration (since they have bogus
      --  sizes set within templates).

      elsif Id = Attribute_Size
        and then Is_Type (P_Entity)
        and then (not Is_Generic_Type (P_Entity))
        and then Known_Static_RM_Size (P_Entity)
      then
         Compile_Time_Known_Attribute (N, RM_Size (P_Entity));
         return;

      --  We can fold 'Alignment applied to a type if the alignment is known
      --  (as happens for an alignment from an attribute definition clause).
      --  At this stage, this can happen only for types (e.g. record
      --  types) for which the size is always non-static. We exclude
      --  generic types from consideration (since they have bogus
      --  sizes set within templates).

      elsif Id = Attribute_Alignment
        and then Is_Type (P_Entity)
        and then (not Is_Generic_Type (P_Entity))
        and then Known_Alignment (P_Entity)
      then
         Compile_Time_Known_Attribute (N, Alignment (P_Entity));
         return;

      --  If this is an access attribute that is known to fail accessibility
      --  check, rewrite accordingly.

      elsif Attribute_Name (N) = Name_Access
        and then Raises_Constraint_Error (N)
      then
         return;

      --  No other cases are foldable (they certainly aren't static, and at
      --  the moment we don't try to fold any cases other than these three).

      else
         Check_Expressions;
         return;
      end if;

      --  If either attribute or the prefix is Any_Type, then propagate
      --  Any_Type to the result and don't do anything else at all.

      if P_Type = Any_Type
        or else (Present (E1) and then Etype (E1) = Any_Type)
        or else (Present (E2) and then Etype (E2) = Any_Type)
      then
         Set_Etype (N, Any_Type);
         return;
      end if;

      --  Scalar subtype case. We have not yet enforced the static requirement
      --  of (RM 4.9(7)) and we don't intend to just yet, since there are cases
      --  of non-static attribute references (e.g. S'Digits for a non-static
      --  floating-point type, which we can compute at compile time).

      --  Note: this folding of non-static attributes is not simply a case of
      --  optimization. For many of the attributes affected, Gigi cannot handle
      --  the attribute and depends on the front end having folded them away.

      --  Note: although we don't require staticness at this stage, we do set
      --  the Static variable to record the staticness, for easy reference by
      --  those attributes where it matters (e.g. Succ and Pred), and also to
      --  be used to ensure that non-static folded things are not marked as
      --  being static (a check that is done right at the end).

      P_Root_Type := Root_Type (P_Type);
      P_Base_Type := Base_Type (P_Type);

      --  If the root type or base type is generic, then we cannot fold. This
      --  test is needed because subtypes of generic types are not always
      --  marked as being generic themselves (which seems odd???)

      if Is_Generic_Type (P_Root_Type)
        or else Is_Generic_Type (P_Base_Type)
      then
         return;
      end if;

      if Is_Scalar_Type (P_Type) then
         Static := Is_OK_Static_Subtype (P_Type);

      --  Array case. We enforce the constrained requirement of (RM 4.9(7-8))
      --  since we can't do anything with unconstrained arrays. In addition,
      --  only the First, Last and Length attributes are possibly static.
      --  In addition Component_Size is possibly foldable, even though it
      --  can never be static.

      --  Definite, Has_Discriminants, Type_Class and Unconstrained_Array are
      --  again exceptions, because they apply as well to unconstrained types.

      elsif Id = Attribute_Type_Class
              or else
            Id = Attribute_Unconstrained_Array
      then
         Static := False;

      else
         if not Is_Constrained (P_Type)
           or else (Id /= Attribute_Component_Size and then
                    Id /= Attribute_First          and then
                    Id /= Attribute_Last           and then
                    Id /= Attribute_Length)
         then
            Check_Expressions;
            return;
         end if;

         --  The rules in (RM 4.9(7,8)) require a static array, but as in the
         --  scalar case, we hold off on enforcing staticness, since there are
         --  cases which we can fold at compile time even though they are not
         --  static (e.g. 'Length applied to a static index, even though other
         --  non-static indexes make the array type non-static). This is only
         --  an optimization, but it falls out essentially free, so why not.
         --  Again we compute the variable Static for easy reference later
         --  (note that no array attributes are static in Ada 83).

         Static := Ada_95;

         declare
            N : Node_Id;

         begin
            N := First_Index (P_Type);
            while Present (N) loop
               Static := Static and then Is_Static_Subtype (Etype (N));

               --  If however the index type is generic, attributes cannot
               --  be folded.

               if Is_Generic_Type (Etype (N))
                 and then Id /= Attribute_Component_Size
               then
                  return;
               end if;

               Next_Index (N);
            end loop;
         end;
      end if;

      --  Check any expressions that are present. Note that these expressions,
      --  depending on the particular attribute type, are either part of the
      --  attribute designator, or they are arguments in a case where the
      --  attribute reference returns a function. In the latter case, the
      --  rule in (RM 4.9(22)) applies and in particular requires the type
      --  of the expressions to be scalar in order for the attribute to be
      --  considered to be static.

      declare
         E : Node_Id;

      begin
         E := E1;
         while Present (E) loop

            --  If expression is not static, then the attribute reference
            --  result certainly cannot be static.

            if not Is_Static_Expression (E) then
               Static := False;
            end if;

            --  If the result is not known at compile time, or is not of
            --  a scalar type, then the result is definitely not static,
            --  so we can quit now.

            if not Compile_Time_Known_Value (E)
              or else not Is_Scalar_Type (Etype (E))
            then
               --  An odd special case, if this is a Pos attribute, this
               --  is where we need to apply a range check since it does
               --  not get done anywhere else.

--                 if Id = Attribute_Pos then
--                    if Is_Integer_Type (Etype (E)) then
--                       Apply_Range_Check (E, Etype (N));
--                    end if;
--                 end if;

               Check_Expressions;
               return;

            --  If the expression raises a constraint error, then so does
            --  the attribute reference. We keep going in this case because
            --  we are still interested in whether the attribute reference
            --  is static even if it is not static.

            elsif Raises_Constraint_Error (E) then
               Set_Raises_Constraint_Error (N);
            end if;

            Next (E);
         end loop;

         if Raises_Constraint_Error (Prefix (N)) then
            return;
         end if;
      end;

      --  Deal with the case of a static attribute reference that raises
      --  constraint error. The Raises_Constraint_Error flag will already
      --  have been set, and the Static flag shows whether the attribute
      --  reference is static. In any case we certainly can't fold such an
      --  attribute reference.

      --  Note that the rewriting of the attribute node with the constraint
      --  error node is essential in this case, because otherwise Gigi might
      --  blow up on one of the attributes it never expects to see.

      --  The constraint_error node must have the type imposed by the context,
      --  to avoid spurious errors in the enclosing expression.

      if Raises_Constraint_Error (N) then
         return;
      end if;

      --  At this point we have a potentially foldable attribute reference.
      --  If Static is set, then the attribute reference definitely obeys
      --  the requirements in (RM 4.9(7,8,22)), and it definitely can be
      --  folded. If Static is not set, then the attribute may or may not
      --  be foldable, and the individual attribute processing routines
      --  test Static as required in cases where it makes a difference.

      --  In the case where Static is not set, we do know that all the
      --  expressions present are at least known at compile time (we
      --  assumed above that if this was not the case, then there was
      --  no hope of static evaluation). However, we did not require
      --  that the bounds of the prefix type be compile time known,
      --  let alone static). That's because there are many attributes
      --  that can be computed at compile time on non-static subtypes,
      --  even though such references are not static expressions.

      case Id is

      ---------------
      -- Alignment --
      ---------------

      when Attribute_Alignment => Alignment_Block : declare
         P_TypeA : constant Entity_Id := Underlying_Type (P_Type);

      begin
         --  Fold if alignment is set and not otherwise

         if Known_Alignment (P_TypeA) then
            Fold_Uint (N, Alignment (P_TypeA), Is_Discrete_Type (P_TypeA));
         end if;
      end Alignment_Block;

      ---------
      -- Bit --
      ---------

      --  Bit can never be folded

      when Attribute_Bit =>
         null;

      --------------------
      -- Component_Size --
      --------------------

      when Attribute_Component_Size =>
         if Known_Static_Component_Size (P_Type) then
            Fold_Uint (N, Component_Size (P_Type), False);
         end if;

      -----------------
      -- Constrained --
      -----------------

      --  Constrained is never folded for now, there may be cases that
      --  could be handled at compile time. to be looked at later.

      when Attribute_Constrained =>
         null;

      --------------
      -- Enum_Rep --
      --------------

      when Attribute_Enum_Rep =>

         --  For an enumeration type with a non-standard representation
         --  use the Enumeration_Rep field of the proper constant. Note
         --  that this would not work for types Character/Wide_Character,
         --  since no real entities are created for the enumeration
         --  literals, but that does not matter since these two types
         --  do not have non-standard representations anyway.

         if Is_Enumeration_Type (P_Type)
           and then Has_Non_Standard_Rep (P_Type)
         then
            Fold_Uint (N, Enumeration_Rep (Expr_Value_E (E1)), Static);

         --  For enumeration types with standard representations and all
         --  other cases (i.e. all integer and modular types), Enum_Rep
         --  is equivalent to Pos.

         else
            Fold_Uint (N, Expr_Value (E1), Static);
         end if;

      --------------
      -- Exponent --
      --------------

      when Attribute_Exponent =>
         Fold_Uint (N,
           Eval_Fat.Exponent (P_Root_Type, Expr_Value_R (E1)), Static);

      -----------
      -- First --
      -----------

      when Attribute_First => First_Attr :
      begin
         Set_Bounds;

         if Compile_Time_Known_Value (Lo_Bound) then
            if Is_Real_Type (P_Type) then
               Fold_Ureal (N, Expr_Value_R (Lo_Bound), Static);
            else
               Fold_Uint  (N, Expr_Value (Lo_Bound), Static);
            end if;
         end if;
      end First_Attr;

      -----------
      -- Floor --
      -----------

      when Attribute_Floor =>
         Fold_Ureal (N,
           Eval_Fat.Floor (P_Root_Type, Expr_Value_R (E1)), Static);

      ----------
      -- Fore --
      ----------

      when Attribute_Fore =>
         if Compile_Time_Known_Bounds (P_Type) then
            Fold_Uint (N, UI_From_Int (Fore_Value), Static);
         end if;

      --------------
      -- Fraction --
      --------------

      when Attribute_Fraction =>
         Fold_Ureal (N,
           Eval_Fat.Fraction (P_Root_Type, Expr_Value_R (E1)), Static);

      -----------
      -- Image --
      -----------

      --  Image is a scalar attribute, but is never static, because it is
      --  not a static function (having a non-scalar argument (RM 4.9(22))

      when Attribute_Image =>
         null;

      ---------
      -- Img --
      ---------

      --  Img is a scalar attribute, but is never static, because it is
      --  not a static function (having a non-scalar argument (RM 4.9(22))

      when Attribute_Img =>
         null;

      -------------------
      -- Integer_Value --
      -------------------

      when Attribute_Integer_Value =>
         null;

      ----------
      -- Last --
      ----------

      when Attribute_Last => Last :
      begin
         Set_Bounds;

         if Compile_Time_Known_Value (Hi_Bound) then
            if Is_Real_Type (P_Type) then
               Fold_Ureal (N, Expr_Value_R (Hi_Bound), Static);
            else
               Fold_Uint  (N, Expr_Value (Hi_Bound), Static);
            end if;
         end if;
      end Last;

      ------------
      -- Length --
      ------------

      when Attribute_Length => Length : declare
         Ind : Node_Id;

      begin
         --  In the case of a generic index type, the bounds may
         --  appear static but the computation is not meaningful,
         --  and may generate a spurious warning.

         Ind := First_Index (P_Type);

         while Present (Ind) loop
            if Is_Generic_Type (Etype (Ind)) then
               return;
            end if;

            Next_Index (Ind);
         end loop;

         Set_Bounds;

         if Compile_Time_Known_Value (Lo_Bound)
           and then Compile_Time_Known_Value (Hi_Bound)
         then
            Fold_Uint (N,
              UI_Max (0, 1 + (Expr_Value (Hi_Bound) - Expr_Value (Lo_Bound))),
              True);
         end if;
      end Length;

      ------------------
      -- Machine_Emax --
      ------------------

      when Attribute_Machine_Emax =>
         Float_Attribute_Universal_Integer (
           IEEES_Machine_Emax,
           IEEEL_Machine_Emax,
           IEEEX_Machine_Emax,
           VAXFF_Machine_Emax,
           VAXDF_Machine_Emax,
           VAXGF_Machine_Emax,
           AAMPS_Machine_Emax,
           AAMPL_Machine_Emax);

      ------------------
      -- Machine_Emin --
      ------------------

      when Attribute_Machine_Emin =>
         Float_Attribute_Universal_Integer (
           IEEES_Machine_Emin,
           IEEEL_Machine_Emin,
           IEEEX_Machine_Emin,
           VAXFF_Machine_Emin,
           VAXDF_Machine_Emin,
           VAXGF_Machine_Emin,
           AAMPS_Machine_Emin,
           AAMPL_Machine_Emin);

      ----------------------
      -- Machine_Mantissa --
      ----------------------

      when Attribute_Machine_Mantissa =>
         Float_Attribute_Universal_Integer (
           IEEES_Machine_Mantissa,
           IEEEL_Machine_Mantissa,
           IEEEX_Machine_Mantissa,
           VAXFF_Machine_Mantissa,
           VAXDF_Machine_Mantissa,
           VAXGF_Machine_Mantissa,
           AAMPS_Machine_Mantissa,
           AAMPL_Machine_Mantissa);

      -----------------------
      -- Machine_Overflows --
      -----------------------

      when Attribute_Machine_Overflows =>

         --  Floating point case

            Fold_Uint (N,
              UI_From_Int (Boolean'Pos (Machine_Overflows_On_Target)),
              True);

      -------------------
      -- Machine_Radix --
      -------------------

      when Attribute_Machine_Radix =>
         --  All floating-point type always have radix 2

            Fold_Uint (N, Uint_2, True);

      --------------------
      -- Machine_Rounds --
      --------------------

      when Attribute_Machine_Rounds =>

         --  Else yield proper floating-point result

            Fold_Uint
              (N, UI_From_Int (Boolean'Pos (Machine_Rounds_On_Target)), True);

      ------------------
      -- Machine_Size --
      ------------------

      --  Note: Machine_Size is identical to Object_Size

      when Attribute_Machine_Size => Machine_Size : declare
         P_TypeA : constant Entity_Id := Underlying_Type (P_Type);

      begin
         if Known_Esize (P_TypeA) then
            Fold_Uint (N, Esize (P_TypeA), True);
         end if;
      end Machine_Size;

      --------------
      -- Mantissa --
      --------------

      when Attribute_Mantissa =>

         --  Fixed-point mantissa

            Fold_Uint (N, Mantissa, True);

      ---------
      -- Max --
      ---------

      when Attribute_Max => Max :
      begin
         if Is_Real_Type (P_Type) then
            Fold_Ureal
              (N, UR_Max (Expr_Value_R (E1), Expr_Value_R (E2)), Static);
         else
            Fold_Uint (N, UI_Max (Expr_Value (E1), Expr_Value (E2)), Static);
         end if;
      end Max;

      ----------------------------------
      -- Max_Size_In_Storage_Elements --
      ----------------------------------

      --  Max_Size_In_Storage_Elements is simply the Size rounded up to a
      --  Storage_Unit boundary. We can fold any cases for which the size
      --  is known by the front end.

      when Attribute_Max_Size_In_Storage_Elements =>
         if Known_Esize (P_Type) then
            Fold_Uint (N,
              (Esize (P_Type) + System_Storage_Unit - 1) /
                                          System_Storage_Unit,
               Static);
         end if;

      ---------
      -- Min --
      ---------

      when Attribute_Min => Min :
      begin
         if Is_Real_Type (P_Type) then
            Fold_Ureal
              (N, UR_Min (Expr_Value_R (E1), Expr_Value_R (E2)), Static);
         else
            Fold_Uint (N, UI_Min (Expr_Value (E1), Expr_Value (E2)), Static);
         end if;
      end Min;

      -----------
      -- Model --
      -----------

      when Attribute_Model =>
         Fold_Ureal (N,
           Eval_Fat.Model (P_Root_Type, Expr_Value_R (E1)), Static);

      -------------
      -- Modulus --
      -------------

      when Attribute_Modulus =>
         Fold_Uint (N, Modulus (P_Type), True);

      -----------------
      -- Object_Size --
      -----------------

      --  The Object_Size attribute for a type returns the Esize of the
      --  type and can be folded if this value is known.

      when Attribute_Object_Size => Object_Size : declare
         P_TypeA : constant Entity_Id := Underlying_Type (P_Type);

      begin
         if Known_Esize (P_TypeA) then
            Fold_Uint (N, Esize (P_TypeA), True);
         end if;
      end Object_Size;

      -------------------------
      -- Passed_By_Reference --
      -------------------------

      --  Scalar types are never passed by reference

      when Attribute_Passed_By_Reference =>
         Fold_Uint (N, False_Value, True);

      ---------
      -- Pos --
      ---------

      when Attribute_Pos =>
         Fold_Uint (N, Expr_Value (E1), True);

      ----------
      -- Pred --
      ----------

--      when Attribute_Pred => 
--           Pred :
--        begin
--           --  Floating-point case
--  
--           if Is_Floating_Point_Type (P_Type) then
--              Fold_Ureal (N,
--                Eval_Fat.Pred (P_Root_Type, Expr_Value_R (E1)), Static);
--  
--           elsif Is_Modular_Integer_Type (P_Type) then
--              Fold_Uint (N, (Expr_Value (E1) - 1) mod Modulus (P_Type), Static);
--  
--           --  Other scalar cases
--  
--           else
--              pragma Assert (Is_Scalar_Type (P_Type));
--  
--              if Is_Enumeration_Type (P_Type)
--                and then Expr_Value (E1) =
--                           Expr_Value (Type_Low_Bound (P_Base_Type))
--              then
--                 Apply_Compile_Time_Constraint_Error
--                   (N, "Pred of `&''First`",
--                    CE_Overflow_Check_Failed,
--                    Ent  => P_Base_Type,
--                    Warn => not Static);
--  
--                 Check_Expressions;
--                 return;
--              end if;
--  
--              Fold_Uint (N, Expr_Value (E1) - 1, Static);
--           end if;
--        end Pred;

      -----------
      -- Range --
      -----------

      --  No processing required, because by this stage, Range has been
      --  replaced by First .. Last, so this branch can never be taken.

      when Attribute_Range =>
         raise Program_Error;

      ------------------
      -- Range_Length --
      ------------------

      when Attribute_Range_Length =>
         Set_Bounds;

         if Compile_Time_Known_Value (Hi_Bound)
           and then Compile_Time_Known_Value (Lo_Bound)
         then
            Fold_Uint (N,
              UI_Max
                (0, Expr_Value (Hi_Bound) - Expr_Value (Lo_Bound) + 1),
                 Static);
         end if;

      ---------------
      -- Remainder --
      ---------------

      when Attribute_Remainder =>
         Fold_Ureal (N,
           Eval_Fat.Remainder
             (P_Root_Type, Expr_Value_R (E1), Expr_Value_R (E2)),
           Static);

      -----------
      -- Round --
      -----------

      when Attribute_Round => Round :
      declare
         Sr : Ureal;
         Si : Uint;

      begin
         --  First we get the (exact result) in units of small

         Sr := Expr_Value_R (E1) / Small_Value (C_Type);

         --  Now round that exactly to an integer

         Si := UR_To_Uint (Sr);

         --  Finally the result is obtained by converting back to real

         Fold_Ureal (N, Si * Small_Value (C_Type), Static);
      end Round;

      --------------
      -- Rounding --
      --------------

      when Attribute_Rounding =>
         Fold_Ureal (N,
           Eval_Fat.Rounding (P_Root_Type, Expr_Value_R (E1)), Static);

      -----------
      -- Scale --
      -----------

      when Attribute_Scale =>
         Fold_Uint (N, Scale_Value (P_Type), True);

      -------------
      -- Scaling --
      -------------

      when Attribute_Scaling =>
         Fold_Ureal (N,
           Eval_Fat.Scaling
             (P_Root_Type, Expr_Value_R (E1), Expr_Value (E2)), Static);

      ----------
      -- Size --
      ----------

      --  Size attribute returns the RM size. All scalar types can be folded,
      --  as well as any types for which the size is known by the front end,
      --  including any type for which a size attribute is specified.

      when Attribute_Size => Size : declare
         P_TypeA : constant Entity_Id := Underlying_Type (P_Type);

      begin
         if RM_Size (P_TypeA) /= Uint_0 then

            --  Normal case (Size) in which case we want the RM_Size

	    Fold_Uint (N,
		       RM_Size (P_TypeA),
		       Static and then Is_Discrete_Type (P_TypeA));
         end if;
      end Size;

      ----------
      -- Succ --
      ----------

      when Attribute_Succ => Succ :
      begin
         --  Floating-point case

         if Is_Floating_Point_Type (P_Type) then
            Fold_Ureal (N,
              Eval_Fat.Succ (P_Root_Type, Expr_Value_R (E1)), Static);

         --  Modular integer case (wraps)

         elsif Is_Modular_Integer_Type (P_Type) then
            Fold_Uint (N, (Expr_Value (E1) + 1) mod Modulus (P_Type), Static);

         --  Other scalar cases

         else
            pragma Assert (Is_Scalar_Type (P_Type));

            if Is_Enumeration_Type (P_Type)
              and then Expr_Value (E1) =
                         Expr_Value (Type_High_Bound (P_Base_Type))
            then
               Apply_Compile_Time_Constraint_Error
                 (N, "Succ of `&''Last`",
                  CE_Overflow_Check_Failed,
                  Ent  => P_Base_Type,
                  Warn => not Static);

               Check_Expressions;
               return;
            else
               Fold_Uint (N, Expr_Value (E1) + 1, Static);
            end if;
         end if;
      end Succ;

      ----------------
      -- Truncation --
      ----------------

      when Attribute_Truncation =>
         Fold_Ureal (N,
           Eval_Fat.Truncation (P_Root_Type, Expr_Value_R (E1)), Static);

      ----------------
      -- Type_Class --
      ----------------

      when Attribute_Type_Class => Type_Class : declare
         Typ : constant Entity_Id := Underlying_Type (P_Base_Type);
         Id  : RE_Id;

      begin
         if Is_Enumeration_Type (Typ) then
            Id := RE_Type_Class_Enumeration;

         elsif Is_Integer_Type (Typ) then
            Id := RE_Type_Class_Integer;

         elsif Is_Floating_Point_Type (Typ) then
            Id := RE_Type_Class_Floating_Point;

         elsif Is_Array_Type (Typ) then
            Id := RE_Type_Class_Array;

         elsif Is_Record_Type (Typ) then
            Id := RE_Type_Class_Record;

         elsif Is_Access_Type (Typ) then
            Id := RE_Type_Class_Access;

         elsif Is_Enumeration_Type (Typ) then
            Id := RE_Type_Class_Enumeration;

         --  Not clear if there are any other possibilities, but if there
         --  are, then we will treat them as the address case.

         else
            Id := RE_Type_Class_Address;
         end if;

         Rewrite (N, New_Occurrence_Of (RTE (Id), Loc));

      end Type_Class;

      -----------------------
      -- Unbiased_Rounding --
      -----------------------

      when Attribute_Unbiased_Rounding =>
         Fold_Ureal (N,
           Eval_Fat.Unbiased_Rounding (P_Root_Type, Expr_Value_R (E1)),
           Static);

      -------------------------
      -- Unconstrained_Array --
      -------------------------

      when Attribute_Unconstrained_Array => Unconstrained_Array : declare
         Typ : constant Entity_Id := Underlying_Type (P_Type);

      begin
         if Is_Array_Type (P_Type)
           and then not Is_Constrained (Typ)
         then
            Rewrite (N, New_Occurrence_Of (Standard_True, Loc));
         else
            Rewrite (N, New_Occurrence_Of (Standard_False, Loc));
         end if;

         --  Analyze and resolve as boolean, note that this attribute is
         --  a static attribute in GNAT.

         Analyze_And_Resolve (N, Standard_Boolean);
         Static := True;
      end Unconstrained_Array;

      ---------
      -- Val --
      ---------

      when Attribute_Val => Val :
      begin
         if  Expr_Value (E1) < Expr_Value (Type_Low_Bound (P_Base_Type))
           or else
             Expr_Value (E1) > Expr_Value (Type_High_Bound (P_Base_Type))
         then
            Apply_Compile_Time_Constraint_Error
              (N, "Val expression out of range",
               CE_Range_Check_Failed,
               Warn => not Static);

            Check_Expressions;
            return;

         else
            Fold_Uint (N, Expr_Value (E1), Static);
         end if;
      end Val;

      ----------------
      -- Value_Size --
      ----------------

      --  The Value_Size attribute for a type returns the RM size of the
      --  type. This an always be folded for scalar types, and can also
      --  be folded for non-scalar types if the size is set.

      when Attribute_Value_Size => Value_Size : declare
         P_TypeA : constant Entity_Id := Underlying_Type (P_Type);

      begin
         if RM_Size (P_TypeA) /= Uint_0 then
            Fold_Uint (N, RM_Size (P_TypeA), True);
         end if;

      end Value_Size;

      ----------------
      -- Wide_Image --
      ----------------

      --  Wide_Image is a scalar attribute, but is never static, because it
      --  is not a static function (having a non-scalar argument (RM 4.9(22))

      when Attribute_Wide_Image =>
         null;

      ----------------
      -- Wide_Width --
      ----------------

      --  Processing for Wide_Width is combined with Width

      --  The following attributes can never be folded, and furthermore we
      --  should not even have entered the case statement for any of these.
      --  Note that in some cases, the values have already been folded as
      --  a result of the processing in Analyze_Attribute.

      when Attribute_Access                   |
           Attribute_Address                  |
           Attribute_Address_Size             |
           Attribute_Base                     |
           Attribute_Bit_Position             |
           Attribute_Class                    |
           Attribute_First_Bit                |
           Attribute_Last_Bit                 |
           Attribute_Pool_Address             |
           Attribute_Position                 |
           Attribute_Storage_Pool             |
           Attribute_Storage_Size             |
           Attribute_Storage_Unit             |
           Attribute_Tag                      |
           Attribute_Target_Name              |
           Attribute_Terminated               |
           Attribute_To_Address               |
           Attribute_Unchecked_Access         |
           Attribute_Universal_Literal_String |
           Attribute_Unrestricted_Access      |
           Attribute_Valid                    |
           Attribute_Value                    |
           Attribute_Wchar_T_Size             |
           Attribute_Wide_Value               |
           Attribute_Word_Size                =>

         raise Program_Error;

      end case;

      --  At the end of the case, one more check. If we did a static evaluation
      --  so that the result is now a literal, then set Is_Static_Expression
      --  in the constant only if the prefix type is a static subtype. For
      --  non-static subtypes, the folding is still OK, but not static.

      --  An exception is the GNAT attribute Constrained_Array which is
      --  defined to be a static attribute in all cases.

      if Nkind (N) = N_Integer_Literal
        or else Nkind (N) = N_Real_Literal
        or else Nkind (N) = N_Character_Literal
        or else Nkind (N) = N_String_Literal
        or else (Is_Entity_Name (N)
                  and then Ekind (Entity (N)) = E_Enumeration_Literal)
      then
         Set_Is_Static_Expression (N, Static);

      --  If this is still an attribute reference, then it has not been folded
      --  and that means that its expressions are in a non-static context.

      elsif Nkind (N) = N_Attribute_Reference then
         Check_Expressions;

      --  Note: the else case not covered here are odd cases where the
      --  processing has transformed the attribute into something other
      --  than a constant. Nothing more to do in such cases.

      else
         null;
      end if;

   end Eval_Attribute;

   ------------------------------
   -- Is_Anonymous_Tagged_Base --
   ------------------------------

   function Is_Anonymous_Tagged_Base
     (Anon : Entity_Id;
      Typ  : Entity_Id)
      return Boolean
   is
   begin
      return
        Anon = Current_Scope
          and then Is_Itype (Anon)
          and then Associated_Node_For_Itype (Anon) = Parent (Typ);
   end Is_Anonymous_Tagged_Base;

   -----------------------
   -- Resolve_Attribute --
   -----------------------

   procedure Resolve_Attribute (N : Node_Id; Typ : Entity_Id) is
      Loc      : constant Source_Ptr   := Sloc (N);
      P        : constant Node_Id      := Prefix (N);
      Aname    : constant Name_Id      := Attribute_Name (N);
      Attr_Id  : constant Attribute_Id := Get_Attribute_Id (Aname);
      Btyp     : constant Entity_Id    := Base_Type (Typ);
      Index    : Interp_Index;
      It       : Interp;
      Nom_Subt : Entity_Id;

   begin
      --  If error during analysis, no point in continuing, except for
      --  array types, where we get  better recovery by using unconstrained
      --  indices than nothing at all (see Check_Array_Type).

      if Error_Posted (N)
        and then Attr_Id /= Attribute_First
        and then Attr_Id /= Attribute_Last
        and then Attr_Id /= Attribute_Length
        and then Attr_Id /= Attribute_Range
      then
         return;
      end if;

      --  If attribute was universal type, reset to actual type

      if Etype (N) = Universal_Integer
        or else Etype (N) = Universal_Real
      then
         Set_Etype (N, Typ);
      end if;

      --  Remaining processing depends on attribute

      case Attr_Id is

         ------------
         -- Access --
         ------------

         --  For access attributes, if the prefix denotes an entity, it is
         --  interpreted as a name, never as a call. It may be overloaded,
         --  in which case resolution uses the profile of the context type.
         --  Otherwise prefix must be resolved.

         when Attribute_Access
            | Attribute_Unchecked_Access
            | Attribute_Unrestricted_Access =>

            if Is_Variable (P) then
               Note_Possible_Modification (P);
            end if;

            if Is_Entity_Name (P) then
               if Is_Overloaded (P) then
                  Get_First_Interp (P, Index, It);

                  while Present (It.Nam) loop

                     if Type_Conformant (Designated_Type (Typ), It.Nam) then
                        Set_Entity (P, It.Nam);

                        --  The prefix is definitely NOT overloaded anymore
                        --  at this point, so we reset the Is_Overloaded
                        --  flag to avoid any confusion when reanalyzing
                        --  the node.

                        Set_Is_Overloaded (P, False);
                        Generate_Reference (Entity (P), P);
                        exit;
                     end if;

                     Get_Next_Interp (Index, It);
                  end loop;

               --  If it is a subprogram name or a type, there is nothing
               --  to resolve.

               elsif not Is_Overloadable (Entity (P))
                 and then not Is_Type (Entity (P))
               then
                  Resolve (P);
               end if;

               Error_Msg_Name_1 := Aname;

               if not Is_Entity_Name (P) then
                  null;

               elsif Is_Abstract (Entity (P))
                 and then Is_Overloadable (Entity (P))
               then
                  Error_Msg_N ("prefix of % attribute cannot be abstract", P);
                  Set_Etype (N, Any_Type);

               elsif Convention (Entity (P)) = Convention_Intrinsic then
                  if Ekind (Entity (P)) = E_Enumeration_Literal then
                     Error_Msg_N
                       ("prefix of % attribute cannot be enumeration literal",
                          P);
                  else
                     Error_Msg_N
                       ("prefix of % attribute cannot be intrinsic", P);
                  end if;

                  Set_Etype (N, Any_Type);

               elsif Is_Thread_Body (Entity (P)) then
                  Error_Msg_N
                    ("prefix of % attribute cannot be a thread body", P);
               end if;

               --  Assignments, return statements, components of aggregates,
               --  generic instantiations will require convention checks if
               --  the type is an access to subprogram. Given that there will
               --  also be accessibility checks on those, this is where the
               --  checks can eventually be centralized ???

               if Ekind (Btyp) = E_Access_Subprogram_Type then
                  if Convention (Btyp) /= Convention (Entity (P)) then
                     Error_Msg_N
                      ("subprogram has invalid convention for context", P);

                  else
                     Check_Subtype_Conformant
                       (New_Id  => Entity (P),
                        Old_Id  => Designated_Type (Btyp),
                        Err_Loc => P);
                  end if;

                  if Attr_Id = Attribute_Unchecked_Access then
                     Error_Msg_Name_1 := Aname;
                     Error_Msg_N
                       ("attribute% cannot be applied to a subprogram", P);

                  elsif Aname = Name_Unrestricted_Access then
                     null;  --  Nothing to check

                  --  Check the static accessibility rule of 3.10.2(32)
                  --  In an instance body, if subprogram and type are both
                  --  local, other rules prevent dangling references, and no
                  --  warning  is needed.

                  elsif Attr_Id = Attribute_Access
                    and then Subprogram_Access_Level (Entity (P))
                      > Type_Access_Level (Btyp)
                  then
                     if not In_Instance_Body then
                        Error_Msg_N
                          ("subprogram must not be deeper than access type",
                            P);

                     elsif Scope (Entity (P)) /= Scope (Btyp) then
                        Error_Msg_N
                          ("subprogram must not be deeper than access type?",
                             P);
                        Error_Msg_N
                          ("Constraint_Error will be raised ?", P);
                        Set_Raises_Constraint_Error (N);
                     end if;

                  --  Check the restriction of 3.10.2(32) that disallows
                  --  the type of the access attribute to be declared
                  --  outside a generic body when the subprogram is declared
                  --  within that generic body.

                  elsif Enclosing_Generic_Body (Entity (P))
                    /= Enclosing_Generic_Body (Btyp)
                  then
                     Error_Msg_N
                       ("access type must not be outside generic body", P);
                  end if;
               end if;

               --  if this is a renaming, an inherited operation, or a
               --  subprogram instance, use the original entity.

               if Is_Entity_Name (P)
                 and then Is_Overloadable (Entity (P))
                 and then Present (Alias (Entity (P)))
               then
                  Rewrite (P,
                    New_Occurrence_Of (Alias (Entity (P)), Sloc (P)));
               end if;

            elsif Nkind (P) = N_Selected_Component
              and then Is_Overloadable (Entity (Selector_Name (P)))
            then
               --  Protected operation. If operation is overloaded, must
               --  disambiguate. Prefix that denotes protected object itself
               --  is resolved with its own type.

               if Attr_Id = Attribute_Unchecked_Access then
                  Error_Msg_Name_1 := Aname;
                  Error_Msg_N
                    ("attribute% cannot be applied to protected operation", P);
               end if;

               Resolve (Prefix (P));
               Generate_Reference (Entity (Selector_Name (P)), P);

            elsif Is_Overloaded (P) then

               --  Use the designated type of the context  to disambiguate.
               declare
                  Index : Interp_Index;
                  It    : Interp;
               begin
                  Get_First_Interp (P, Index, It);

                  while Present (It.Typ) loop
                     if Covers (Designated_Type (Typ), It.Typ) then
                        Resolve (P, It.Typ);
                        exit;
                     end if;

                     Get_Next_Interp (Index, It);
                  end loop;
               end;
            else
               Resolve (P);
            end if;

            --  X'Access is illegal if X denotes a constant and the access
            --  type is access-to-variable. Same for 'Unchecked_Access.
            --  The rule does not apply to 'Unrestricted_Access.

            if not (Ekind (Btyp) = E_Access_Subprogram_Type
                     or else Is_Access_Constant (Btyp)
                     or else Is_Variable (P)
                     or else Attr_Id = Attribute_Unrestricted_Access)
            then
               if Comes_From_Source (N) then
                  Error_Msg_N ("access-to-variable designates constant", P);
               end if;
            end if;

            if (Attr_Id = Attribute_Access
                  or else
                Attr_Id = Attribute_Unchecked_Access)
              and then (Ekind (Btyp) = E_General_Access_Type
                         or else Ekind (Btyp) = E_Anonymous_Access_Type)
            then
               if Is_Dependent_Component_Of_Mutable_Object (P) then
                  Error_Msg_N
                    ("illegal attribute for discriminant-dependent component",
                     P);
               end if;

               --  Check the static matching rule of 3.10.2(27). The
               --  nominal subtype of the prefix must statically
               --  match the designated type.

               Nom_Subt := Etype (P);

               if Is_Constr_Subt_For_U_Nominal (Nom_Subt) then
                  Nom_Subt := Etype (Nom_Subt);
               end if;

               if Is_Tagged_Type (Designated_Type (Typ)) then

                  --  If the attribute is in the context of an access
                  --  parameter, then the prefix is allowed to be of
                  --  the class-wide type (by AI-127).

                  if Ekind (Typ) = E_Anonymous_Access_Type then
                     if not Covers (Designated_Type (Typ), Nom_Subt)
                       and then not Covers (Nom_Subt, Designated_Type (Typ))
                     then
                        declare
                           Desig : Entity_Id;

                        begin
                           Desig := Designated_Type (Typ);

                           if Is_Class_Wide_Type (Desig) then
                              Desig := Etype (Desig);
                           end if;

                           if Is_Anonymous_Tagged_Base (Nom_Subt, Desig) then
                              null;

                           else
                              Error_Msg_NE
                                ("type of prefix: & not compatible",
                                  P, Nom_Subt);
                              Error_Msg_NE
                                ("\with &, the expected designated type",
                                  P, Designated_Type (Typ));
                           end if;
                        end;
                     end if;

                  elsif not Covers (Designated_Type (Typ), Nom_Subt)
                    or else
                      (not Is_Class_Wide_Type (Designated_Type (Typ))
                        and then Is_Class_Wide_Type (Nom_Subt))
                  then
                     Error_Msg_NE
                       ("type of prefix: & is not covered", P, Nom_Subt);
                     Error_Msg_NE
                       ("\by &, the expected designated type" &
                           " ('R'M 3.10.2 (27))", P, Designated_Type (Typ));
                  end if;


               elsif not Subtypes_Statically_Match
                           (Designated_Type (Base_Type (Typ)), Nom_Subt)
                 and then
                   not (not Is_Constrained
                                  (Designated_Type (Base_Type (Typ))))
               then
                  Error_Msg_N
                    ("object subtype must statically match "
                     & "designated subtype", P);

                  if Is_Entity_Name (P)
                    and then Is_Array_Type (Designated_Type (Typ))
                  then

                     declare
                        D : constant Node_Id := Declaration_Node (Entity (P));

                     begin
                        Error_Msg_N ("aliased object has explicit bounds?",
                          D);
                        Error_Msg_N ("\declare without bounds"
                          & " (and with explicit initialization)?", D);
                        Error_Msg_N ("\for use with unconstrained access?", D);
                     end;
                  end if;
               end if;

               --  Check the static accessibility rule of 3.10.2(28).
               --  Note that this check is not performed for the
               --  case of an anonymous access type, since the access
               --  attribute is always legal in such a context.

               if Attr_Id /= Attribute_Unchecked_Access
                 and then Object_Access_Level (P) > Type_Access_Level (Btyp)
                 and then Ekind (Btyp) = E_General_Access_Type
               then
                  --  In an instance, this is a runtime check, but one we
                  --  know will fail, so generate an appropriate warning.

                  if In_Instance_Body then
                     Error_Msg_N
                       ("?non-local pointer cannot point to local object", P);
                     Error_Msg_N
                       ("?Program_Error will be raised at run time", P);
                     return;

                  else
                     Error_Msg_N
                       ("non-local pointer cannot point to local object", P);
                  end if;
               end if;
            end if;

            --  The context cannot be a pool-specific type, but this is a
            --  legality rule, not a resolution rule, so it must be checked
            --  separately, after possibly disambiguation (see AI-245).

            if Ekind (Btyp) = E_Access_Type
              and then Attr_Id /= Attribute_Unrestricted_Access
            then
               Wrong_Type (N, Typ);
            end if;

            Set_Etype (N, Typ);

            --  Check for incorrect atomic/volatile reference (RM C.6(12))

--              if Attr_Id /= Attribute_Unrestricted_Access then
--                 if Is_Atomic_Object (P)
--                   and then not Is_Atomic (Designated_Type (Typ))
--                 then
--                    Error_Msg_N
--                      ("access to atomic object cannot yield access-to-" &
--                       "non-atomic type", P);
--  
--                 elsif Is_Volatile_Object (P)
--                   and then not Is_Volatile (Designated_Type (Typ))
--                 then
--                    Error_Msg_N
--                      ("access to volatile object cannot yield access-to-" &
--                       "non-volatile type", P);
--                 end if;
--              end if;

         -------------
         -- Address --
         -------------

         --  Deal with resolving the type for Address attribute, overloading
         --  is not permitted here, since there is no context to resolve it.

         when Attribute_Address =>

            --  To be safe, assume that if the address of a variable is taken,
            --  it may be modified via this address, so note modification.

            if Is_Variable (P) then
               Note_Possible_Modification (P);
            end if;

            if Nkind (P) in  N_Subexpr
              and then Is_Overloaded (P)
            then
               Get_First_Interp (P, Index, It);
               Get_Next_Interp (Index, It);

               if Present (It.Nam) then
                  Error_Msg_Name_1 := Aname;
                  Error_Msg_N
                    ("prefix of % attribute cannot be overloaded", N);
                  return;
               end if;
            end if;

            if not Is_Entity_Name (P)
               or else not Is_Overloadable (Entity (P))
            then
               Resolve (P);
            end if;

            --  If this is the name of a derived subprogram, or that of a
            --  generic actual, the address is that of the original entity.

            if Is_Entity_Name (P)
              and then Is_Overloadable (Entity (P))
              and then Present (Alias (Entity (P)))
            then
               Rewrite (P,
                 New_Occurrence_Of (Alias (Entity (P)), Sloc (P)));
            end if;

         ------------------
         -- Code_Address --
         ------------------

         --  Shares processing with Address attribute

         when Attribute_Pool_Address =>
            Resolve (P);

         -----------
         -- Range --
         -----------

         --  We replace the Range attribute node with a range expression
         --  whose bounds are the 'First and 'Last attributes applied to the
         --  same prefix. The reason that we do this transformation here
         --  instead of in the expander is that it simplifies other parts of
         --  the semantic analysis which assume that the Range has been
         --  replaced; thus it must be done even when in semantic-only mode
         --  (note that the RM specifically mentions this equivalence, we
         --  take care that the prefix is only evaluated once).

         when Attribute_Range => Range_Attribute :
            declare
               LB   : Node_Id;
               HB   : Node_Id;
            begin
               if not Is_Entity_Name (P)
                 or else not Is_Type (Entity (P))
               then
                  Resolve (P);
               end if;
	       
	       HB :=
		 Make_Attribute_Reference 
		 (Loc,
		  Prefix         => Duplicate_Subexpr (P),
		  Attribute_Name => Name_Last,
		  Expressions    => Expressions (N));
	       
	       LB :=
		 Make_Attribute_Reference 
		 (Loc,
		  Prefix         => P,
		  Attribute_Name => Name_First,
		  Expressions    => Expressions (N));
	       
               --  If the original was marked as Must_Not_Freeze (see code
               --  in Sem_Ch3.Make_Index), then make sure the rewriting
               --  does not freeze either.
	       
               if Must_Not_Freeze (N) then
                  Set_Must_Not_Freeze (HB);
                  Set_Must_Not_Freeze (LB);
                  Set_Must_Not_Freeze (Prefix (HB));
                  Set_Must_Not_Freeze (Prefix (LB));
               end if;

               if Raises_Constraint_Error (Prefix (N)) then

                  --  Preserve Sloc of prefix in the new bounds, so that
                  --  the posted warning can be removed if we are within
                  --  unreachable code.

                  Set_Sloc (LB, Sloc (Prefix (N)));
                  Set_Sloc (HB, Sloc (Prefix (N)));
               end if;

               Rewrite (N, Make_Range (Loc, LB, HB));
               Analyze_And_Resolve (N, Typ);

               --  Normally after resolving attribute nodes, Eval_Attribute
               --  is called to do any possible static evaluation of the node.
               --  However, here since the Range attribute has just been
               --  transformed into a range expression it is no longer an
               --  attribute node and therefore the call needs to be avoided
               --  and is accomplished by simply returning from the procedure.

               return;
            end Range_Attribute;

         ----------------------
         -- Unchecked_Access --
         ----------------------

         --  Processing is shared with Access

         -------------------------
         -- Unrestricted_Access --
         -------------------------

         --  Processing is shared with Access

         ---------
         -- Val --
         ---------

         --  Apply range check. Note that we did not do this during the
         --  analysis phase, since we wanted Eval_Attribute to have a
         --  chance at finding an illegal out of range value.

         when Attribute_Val =>

            --  Note that we do our own Eval_Attribute call here rather than
            --  use the common one, because we need to do processing after
            --  the call, as per above comment.

            Eval_Attribute (N);

            --  Eval_Attribute may replace the node with a raise CE, or
            --  fold it to a constant. Obviously we only apply a scalar
            --  range check if this did not happen!

--              if Nkind (N) = N_Attribute_Reference
--                and then Attribute_Name (N) = Name_Val
--              then
--                 Apply_Scalar_Range_Check (First (Expressions (N)), Btyp);
--              end if;

            return;

         ----------------------
         -- Other Attributes --
         ----------------------

         --  For other attributes, resolve prefix unless it is a type. If
         --  the attribute reference itself is a type name ('Base and 'Class)
         --  then this is only legal within a task or protected record.

         when others =>
            if not Is_Entity_Name (P)
              or else not Is_Type (Entity (P))
            then
               Resolve (P);
            end if;

            --  If the attribute reference itself is a type name ('Base,
            --  'Class) then this is only legal within a task or protected
            --  record. What is this all about ???

            if Is_Entity_Name (N)
              and then Is_Type (Entity (N))
            then
                  Error_Msg_N
                    ("invalid use of subtype name in expression or call", N);
            end if;

            --  For attributes whose argument may be a string, complete
            --  resolution of argument now. This avoids premature expansion
            --  (and the creation of transient scopes) before the attribute
            --  reference is resolved.

            case Attr_Id is
               when Attribute_Value =>
                  Resolve (First (Expressions (N)), Standard_String);

               when Attribute_Wide_Value =>
                  Resolve (First (Expressions (N)), Standard_Wide_String);

               when others => null;
            end case;
      end case;

      --  Normally the Freezing is done by Resolve but sometimes the Prefix
      --  is not resolved, in which case the freezing must be done now.

      Freeze_Expression (P);

      --  Finally perform static evaluation on the attribute reference

      Eval_Attribute (N);

   end Resolve_Attribute;

end Sem_Attr;
