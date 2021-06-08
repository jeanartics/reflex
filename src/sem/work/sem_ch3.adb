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

with Ada.Text_Io; use Ada.Text_Io;

with Atree;    use Atree;
with Elists;   use Elists;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Eval_Fat; use Eval_Fat;
with Exp_Util; use Exp_Util;
with Freeze;   use Freeze;
with Itypes;   use Itypes;
with Layout;   use Layout;
with Lib;      use Lib;
with Lib.Xref; use Lib.Xref;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Sem;      use Sem;
with Sem_Case; use Sem_Case;
with Sem_Cat;  use Sem_Cat;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch13; use Sem_Ch13;
with Sem_Disp; use Sem_Disp;
with Sem_Elim; use Sem_Elim;
with Sem_Eval; use Sem_Eval;
with Sem_Mech; use Sem_Mech;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

package body Sem_Ch3 is

   -----------------------
   -- Local Subprograms --
   -----------------------
   
   procedure Expand_Derived_Record (T : Entity_Id; Def : Node_Id);
   
   procedure Build_Derived_Type
     (N             : Node_Id;
      Parent_Type   : Entity_Id;
      Derived_Type  : Entity_Id;
      Is_Completion : Boolean;
      Derive_Subps  : Boolean := True);
   --  Create and decorate a Derived_Type given the Parent_Type entity.
   --  N is the N_Full_Type_Declaration node containing the derived type
   --  definition. Parent_Type is the entity for the parent type in the derived
   --  type definition and Derived_Type the actual derived type. Is_Completion
   --  must be set to False if Derived_Type is the N_Defining_Identifier node
   --  in N (ie Derived_Type = Defining_Identifier (N)). In this case N is not
   --  the completion of a private type declaration. If Is_Completion is
   --  set to True, N is the completion of a private type declaration and
   --  Derived_Type is different from the defining identifier inside N (i.e.
   --  Derived_Type /= Defining_Identifier (N)). Derive_Subps indicates whether
   --  the parent subprograms should be derived. The only case where this
   --  parameter is False is when Build_Derived_Type is recursively called to
   --  process an implicit derived full type for a type derived from a private
   --  type (in that case the subprograms must only be derived for the private
   --  view of the type).
   --  ??? These flags need a bit of re-examination and re-documentation:
   --  ???  are they both necessary (both seem related to the recursion)?

   procedure Build_Derived_Access_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id);
   --  Subsidiary procedure to Build_Derived_Type. For a derived access type,
   --  create an implicit base if the parent type is constrained or if the
   --  subtype indication has a constraint.

   procedure Build_Derived_Array_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id);
   --  Subsidiary procedure to Build_Derived_Type. For a derived array type,
   --  create an implicit base if the parent type is constrained or if the
   --  subtype indication has a constraint.

   procedure Build_Derived_Enumeration_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id);
   --  Subsidiary procedure to Build_Derived_Type. For a derived enumeration
   --  type, we must create a new list of literals. Types derived from
   --  Character and Wide_Character are special-cased.

   procedure Build_Derived_Numeric_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id);
   --  Subsidiary procedure to Build_Derived_Type. For numeric types, create
   --  an anonymous base type, and propagate constraint to subtype if needed.

   procedure Build_Derived_Private_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id;
      Is_Completion : Boolean;
      Derive_Subps  : Boolean := True);
   --  Subsidiary procedure to Build_Derived_Type. This procedure is complex
   --  because the parent may or may not have a completion, and the derivation
   --  may itself be a completion.

   procedure Build_Derived_Record_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id;
      Derive_Subps : Boolean := True);
   --  Subsidiary procedure to Build_Derived_Type and
   --  Analyze_Private_Extension_Declaration used for tagged and untagged
   --  record types. All parameters are as in Build_Derived_Type except that
   --  N, in addition to being an N_Full_Type_Declaration node, can also be an
   --  N_Private_Extension_Declaration node. See the definition of this routine
   --  for much more info. Derive_Subps indicates whether subprograms should
   --  be derived from the parent type. The only case where Derive_Subps is
   --  False is for an implicit derived full type for a type derived from a
   --  private type (see Build_Derived_Type).

   function Inherit_Components
     (N             : Node_Id;
      Parent_Base   : Entity_Id;
      Derived_Base  : Entity_Id;
      Is_Tagged     : Boolean) return Elist_Id;
   --  Called from Build_Derived_Record_Type to inherit the components of
   --  Parent_Base (a base type) into the Derived_Base (the derived base type).
   --  For more information on derived types and component inheritance please
   --  consult the comment above the body of Build_Derived_Record_Type.
   --
   --    N is the original derived type declaration.
   --
   --    Is_Tagged is set if we are dealing with tagged types.
   --
   --    If Inherit_Discr is set, Derived_Base inherits its discriminants
   --    from Parent_Base, otherwise no discriminants are inherited.
   --
   --    Discs gives the list of constraints that apply to Parent_Base in the
   --    derived type declaration. If Discs is set to No_Elist, then we have
   --    the following situation:
   --
   --      type Parent (D1..Dn : ..) is [tagged] record ...;
   --      type Derived is new Parent [with ...];
   --
   --    which gets treated as
   --
   --      type Derived (D1..Dn : ..) is new Parent (D1,..,Dn) [with ...];
   --
   --  For untagged types the returned value is an association list. The list
   --  starts from the association (Parent_Base => Derived_Base), and then it
   --  contains a sequence of the associations of the form
   --
   --    (Old_Component => New_Component),
   --
   --  where Old_Component is the Entity_Id of a component in Parent_Base
   --  and New_Component is the Entity_Id of the corresponding component
   --  in Derived_Base. For untagged records, this association list is
   --  needed when copying the record declaration for the derived base.
   --  In the tagged case the value returned is irrelevant.

   procedure Build_Discriminated_Subtype
     (T           : Entity_Id;
      Def_Id      : Entity_Id;
      Elist       : Elist_Id;
      Related_Nod : Node_Id;
      For_Access  : Boolean := False);
   --  Subsidiary procedure to Constrain_Discriminated_Type and to
   --  Process_Incomplete_Dependents. Given
   --
   --     T (a possibly discriminated base type)
   --     Def_Id (a very partially built subtype for T),
   --
   --  the call completes Def_Id to be the appropriate E_*_Subtype.
   --
   --  The Elist is the list of discriminant constraints if any (it is set to
   --  No_Elist if T is not a discriminated type, and to an empty list if
   --  T has discriminants but there are no discriminant constraints). The
   --  Related_Nod is the same as Decl_Node in Create_Constrained_Components.
   --  The For_Access says whether or not this subtype is really constraining
   --  an access type. That is its sole purpose is the designated type of an
   --  access type -- in which case a Private_Subtype Is_For_Access_Subtype
   --  is built to avoid freezing T when the access subtype is frozen.

   function Build_Scalar_Bound
     (Bound : Node_Id;
      Par_T : Entity_Id;
      Der_T : Entity_Id) return Node_Id;
   --  The bounds of a derived scalar type are conversions of the bounds of
   --  the parent type. Optimize the representation if the bounds are literals.
   --  Needs a more complete spec--what are the parameters exactly, and what
   --  exactly is the returned value, and how is Bound affected???

   procedure Build_Underlying_Full_View
     (N   : Node_Id;
      Typ : Entity_Id;
      Par : Entity_Id);
   --  If the completion of a private type is itself derived from a private
   --  type, or if the full view of a private subtype is itself private, the
   --  back-end has no way to compute the actual size of this type. We build
   --  an internal subtype declaration of the proper parent type to convey
   --  this information. This extra mechanism is needed because a full
   --  view cannot itself have a full view (it would get clobbered during
   --  view exchanges).

   procedure Check_Real_Bound (Bound : Node_Id);
   --  Check given bound for being of real type and static. If not, post an
   --  appropriate message, and rewrite the bound with the real literal zero.

   procedure Constant_Redeclaration
     (Id : Entity_Id;
      N  : Node_Id;
      T  : out Entity_Id);
   --  Various checks on legality of full declaration of deferred constant.
   --  Id is the entity for the redeclaration, N is the N_Object_Declaration,
   --  node. The caller has not yet set any attributes of this entity.

   procedure Convert_Scalar_Bounds
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id;
      Loc          : Source_Ptr);
   --  For derived scalar types, convert the bounds in the type definition
   --  to the derived type, and complete their analysis. Given a constraint
   --  of the form:
   --                   ..  new T range Lo .. Hi;
   --  Lo and Hi are analyzed and resolved with T'Base, the parent_type.
   --  The bounds of the derived type (the anonymous base) are copies of
   --  Lo and Hi.  Finally, the bounds of the derived subtype are conversions
   --  of those bounds to the derived_type, so that their typing is
   --  consistent.

   procedure Copy_Array_Base_Type_Attributes (T1, T2 : Entity_Id);
   --  Copies attributes from array base type T2 to array base type T1.
   --  Copies only attributes that apply to base types, but not subtypes.

   procedure Copy_Array_Subtype_Attributes (T1, T2 : Entity_Id);
   --  Copies attributes from array subtype T2 to array subtype T1. Copies
   --  attributes that apply to both subtypes and base types.

   procedure Constrain_Access
     (Def_Id      : in out Entity_Id;
      S           : Node_Id;
      Related_Nod : Node_Id);
   --  Apply a list of constraints to an access type. If Def_Id is empty,
   --  it is an anonymous type created for a subtype indication. In that
   --  case it is created in the procedure and attached to Related_Nod.

   procedure Constrain_Array
     (Def_Id      : in out Entity_Id;
      SI          : Node_Id;
      Related_Nod : Node_Id;
      Related_Id  : Entity_Id;
      Suffix      : Character);
   --  Apply a list of index constraints to an unconstrained array type. The
   --  first parameter is the entity for the resulting subtype. A value of
   --  Empty for Def_Id indicates that an implicit type must be created, but
   --  creation is delayed (and must be done by this procedure) because other
   --  subsidiary implicit types must be created first (which is why Def_Id
   --  is an in/out parameter). The second parameter is a subtype indication
   --  node for the constrained array to be created (e.g. something of the
   --  form string (1 .. 10)). Related_Nod gives the place where this type
   --  has to be inserted in the tree. The Related_Id and Suffix parameters
   --  are used to build the associated Implicit type name.

   procedure Constrain_Enumeration (Def_Id : Node_Id; S : Node_Id);
   --  Constrain an enumeration type with a range constraint. This is
   --  identical to Constrain_Integer, but for the Ekind of the
   --  resulting subtype.

   procedure Constrain_Float (Def_Id : Node_Id; S : Node_Id);
   --  Constrain a floating point type with either a digits constraint
   --  and/or a range constraint, building a E_Floating_Point_Subtype.

   procedure Constrain_Index
     (Index        : Node_Id;
      S            : Node_Id;
      Related_Nod  : Node_Id;
      Related_Id   : Entity_Id;
      Suffix       : Character;
      Suffix_Index : Nat);
   --  Process an index constraint in a constrained array declaration.
   --  The constraint can be a subtype name, or a range with or without
   --  an explicit subtype mark. The index is the corresponding index of the
   --  unconstrained array. The Related_Id and Suffix parameters are used to
   --  build the associated Implicit type name.

   procedure Constrain_Integer (Def_Id : Node_Id; S : Node_Id);
   --  Build subtype of a signed or modular integer type.

   procedure Copy_And_Swap (Priv, Full : Entity_Id);
   --  Copy the Priv entity into the entity of its full declaration
   --  then swap the two entities in such a manner that the former private
   --  type is now seen as a full type.

   procedure Complete_Private_Subtype
     (Priv        : Entity_Id;
      Full        : Entity_Id;
      Full_Base   : Entity_Id;
      Related_Nod : Node_Id);
   --  Complete the implicit full view of a private subtype by setting
   --  the appropriate semantic fields. If the full view of the parent is
   --  a record type, build constrained components of subtype.

   procedure Derived_Standard_Character
     (N             : Node_Id;
      Parent_Type   : Entity_Id;
      Derived_Type  : Entity_Id);
   --  Subsidiary procedure to Build_Derived_Enumeration_Type which handles
   --  derivations from types Standard.Character and Standard.Wide_Character.

   procedure Derived_Type_Declaration
     (T             : Entity_Id;
      N             : Node_Id;
      Is_Completion : Boolean);
   --  Process a derived type declaration. This routine will invoke
   --  Build_Derived_Type to process the actual derived type definition.
   --  Parameters N and Is_Completion have the same meaning as in
   --  Build_Derived_Type. T is the N_Defining_Identifier for the entity
   --  defined in the N_Full_Type_Declaration node N, that is T is the
   --  derived type.

   function Find_Type_Of_Subtype_Indic (S : Node_Id) return Entity_Id;
   --  Given a subtype indication S (which is really an N_Subtype_Indication
   --  node or a plain N_Identifier), find the type of the subtype mark.

   procedure Enumeration_Type_Declaration (T : Entity_Id; Def : Node_Id);
   --  Insert each literal in symbol table, as an overloadable identifier
   --  Each enumeration type is mapped into a sequence of integers, and
   --  each literal is defined as a constant with integer value. If any
   --  of the literals are character literals, the type is a character
   --  type, which means that strings are legal aggregates for arrays of
   --  components of the type.

   function Find_Type_Of_Object
     (Obj_Def     : Node_Id;
      Related_Nod : Node_Id) return Entity_Id;
   --  Get type entity for object referenced by Obj_Def, attaching the
   --  implicit types generated to Related_Nod

   procedure Floating_Point_Type_Declaration (T : Entity_Id; Def : Node_Id);
   --  Create a new float, and apply the constraint to obtain subtype of it

   function Has_Range_Constraint (N : Node_Id) return Boolean;
   --  Given an N_Subtype_Indication node N, return True if a range constraint
   --  is present, either directly, or as part of a digits or delta constraint.
   --  In addition, a digits constraint in the decimal case returns True, since
   --  it establishes a default range if no explicit range is present.

   function Is_Valid_Constraint_Kind
     (T_Kind          : Type_Kind;
      Constraint_Kind : Node_Kind) return Boolean;
   --  Returns True if it is legal to apply the given kind of constraint
   --  to the given kind of type (index constraint to an array type,
   --  for example).

   procedure Modular_Type_Declaration (T : Entity_Id; Def : Node_Id);
   --  Create new modular type. Verify that modulus is in  bounds and is
   --  a power of two (implementation restriction).

   procedure New_Concatenation_Op (Typ : Entity_Id);
   --  Create an abbreviated declaration for an operator in order to
   --  materialize concatenation on array types.

   procedure Prepare_Private_Subtype_Completion
     (Id          : Entity_Id;
      Related_Nod : Node_Id);
   --  Id is a subtype of some private type. Creates the full declaration
   --  associated with Id whenever possible, i.e. when the full declaration
   --  of the base type is already known. Records each subtype into
   --  Private_Dependents of the base type.

   procedure Process_Incomplete_Dependents
     (N      : Node_Id;
      Full_T : Entity_Id;
      Inc_T  : Entity_Id);
   --  Process all entities that depend on an incomplete type. There include
   --  subtypes, subprogram types that mention the incomplete type in their
   --  profiles, and subprogram with access parameters that designate the
   --  incomplete type.

   --  Inc_T is the defining identifier of an incomplete type declaration, its
   --  Ekind is E_Incomplete_Type.
   --
   --    N is the corresponding N_Full_Type_Declaration for Inc_T.
   --
   --    Full_T is N's defining identifier.
   --
   --  Subtypes of incomplete types with discriminants are completed when the
   --  parent type is. This is simpler than private subtypes, because they can
   --  only appear in the same scope, and there is no need to exchange views.
   --  Similarly, access_to_subprogram types may have a parameter or a return
   --  type that is an incomplete type, and that must be replaced with the
   --  full type.

   --  If the full type is tagged, subprogram with access parameters that
   --  designated the incomplete may be primitive operations of the full type,
   --  and have to be processed accordingly.

   procedure Process_Real_Range_Specification (Def : Node_Id);
   --  Given the type definition for a real type, this procedure processes
   --  and checks the real range specification of this type definition if
   --  one is present. If errors are found, error messages are posted, and
   --  the Real_Range_Specification of Def is reset to Empty.

   procedure Record_Type_Declaration
     (T    : Entity_Id;
      N    : Node_Id;
      Prev : Entity_Id);
   --  Process a record type declaration (for both untagged and tagged
   --  records). Parameters T and N are exactly like in procedure
   --  Derived_Type_Declaration, except that no flag Is_Completion is
   --  needed for this routine. If this is the completion of an incomplete
   --  type declaration, Prev is the entity of the incomplete declaration,
   --  used for cross-referencing. Otherwise Prev = T.

   procedure Record_Type_Definition (Def : Node_Id; Prev_T : Entity_Id);
   --  This routine is used to process the actual record type definition
   --  (both for untagged and tagged records). Def is a record type
   --  definition node. This procedure analyzes the components in this
   --  record type definition. Prev_T is the entity for the enclosing record
   --  type. It is provided so that its Has_Task flag can be set if any of
   --  the component have Has_Task set. If the declaration is the completion
   --  of an incomplete type declaration, Prev_T is the original incomplete
   --  type, whose full view is the record type.

   procedure Replace_Components (Typ : Entity_Id; Decl : Node_Id);
   --  Subsidiary to Build_Derived_Record_Type. For untagged records, we
   --  build a copy of the declaration tree of the parent, and we create
   --  independently the list of components for the derived type. Semantic
   --  information uses the component entities, but record representation
   --  clauses are validated on the declaration tree. This procedure replaces
   --  discriminants and components in the declaration with those that have
   --  been created by Inherit_Components.

   procedure Set_Scalar_Range_For_Subtype
     (Def_Id : Entity_Id;
      R      : Node_Id;
      Subt   : Entity_Id);
   --  This routine is used to set the scalar range field for a subtype
   --  given Def_Id, the entity for the subtype, and R, the range expression
   --  for the scalar range. Subt provides the parent subtype to be used
   --  to analyze, resolve, and check the given range.

   procedure Signed_Integer_Type_Declaration (T : Entity_Id; Def : Node_Id);
   --  Create a new signed integer entity, and apply the constraint to obtain
   --  the required first named subtype of this type.

   -----------------------
   -- Access_Definition --
   -----------------------

   function Access_Definition
     (Related_Nod : Node_Id;
      N           : Node_Id) return Entity_Id
   is
      Anon_Type : constant Entity_Id :=
                    Create_Itype (E_Anonymous_Access_Type, Related_Nod,
                                  Scope_Id => Scope (Current_Scope));
      Desig_Type : Entity_Id;

   begin
      Find_Type (Subtype_Mark (N));
      Desig_Type := Entity (Subtype_Mark (N));

      Set_Directly_Designated_Type (Anon_Type, Desig_Type);
      Set_Etype                    (Anon_Type, Anon_Type);
      Init_Size_Align              (Anon_Type);
      Set_Depends_On_Private (Anon_Type, Has_Private_Component (Anon_Type));

      --  The anonymous access type is as public as the discriminated type or
      --  subprogram that defines it. It is imported (for back-end purposes)
      --  if the designated type is.

      Set_Is_Public (Anon_Type, Is_Public (Scope (Anon_Type)));

      --  The context is either a subprogram declaration or an access
      --  discriminant, in a private or a full type declaration. In
      --  the case of a subprogram, If the designated type is incomplete,
      --  the operation will be a primitive operation of the full type, to
      --  be updated subsequently.

      if Ekind (Desig_Type) = E_Incomplete_Type
        and then Is_Overloadable (Current_Scope)
      then
         Append_Elmt (Current_Scope, Private_Dependents (Desig_Type));
         Set_Has_Delayed_Freeze (Current_Scope);
      end if;

      return Anon_Type;
   end Access_Definition;

   -----------------------------------
   -- Access_Subprogram_Declaration --
   -----------------------------------

   procedure Access_Subprogram_Declaration
     (T_Name : Entity_Id;
      T_Def  : Node_Id)
   is
      Formals : constant List_Id := Parameter_Specifications (T_Def);
      Formal  : Entity_Id;

      Desig_Type : constant Entity_Id :=
                   Create_Itype (E_Subprogram_Type, Parent (T_Def));

   begin
      if Nkind (T_Def) = N_Access_Function_Definition then
         Analyze (Subtype_Mark (T_Def));
         Set_Etype (Desig_Type, Entity (Subtype_Mark (T_Def)));

         if not (Is_Type (Etype (Desig_Type))) then
            Error_Msg_N
             ("expect type in function specification", Subtype_Mark (T_Def));
         end if;

      else
         Set_Etype (Desig_Type, Standard_Void_Type);
      end if;

      if Present (Formals) then
         New_Scope (Desig_Type);
         Process_Formals (Formals, Parent (T_Def));

         --  A bit of a kludge here, End_Scope requires that the parent
         --  pointer be set to something reasonable, but Itypes don't
         --  have parent pointers. So we set it and then unset it ???
         --  If and when Itypes have proper parent pointers to their
         --  declarations, this kludge can be removed.

         Set_Parent (Desig_Type, T_Name);
         End_Scope;
         Set_Parent (Desig_Type, Empty);
      end if;

      --  The return type and/or any parameter type may be incomplete. Mark
      --  the subprogram_type as depending on the incomplete type, so that
      --  it can be updated when the full type declaration is seen.

      if Present (Formals) then
         Formal := First_Formal (Desig_Type);

         while Present (Formal) loop

            if Ekind (Formal) /= E_In_Parameter
              and then Nkind (T_Def) = N_Access_Function_Definition
            then
               Error_Msg_N ("functions can only have IN parameters", Formal);
            end if;

            if Ekind (Etype (Formal)) = E_Incomplete_Type then
               Append_Elmt (Desig_Type, Private_Dependents (Etype (Formal)));
               Set_Has_Delayed_Freeze (Desig_Type);
            end if;

            Next_Formal (Formal);
         end loop;
      end if;

      if Ekind (Etype (Desig_Type)) = E_Incomplete_Type
        and then not Has_Delayed_Freeze (Desig_Type)
      then
         Append_Elmt (Desig_Type, Private_Dependents (Etype (Desig_Type)));
         Set_Has_Delayed_Freeze (Desig_Type);
      end if;

      Check_Delayed_Subprogram (Desig_Type);

      Set_Ekind (T_Name, E_Access_Subprogram_Type);

      Set_Etype                    (T_Name, T_Name);
      Init_Size_Align              (T_Name);
      Set_Directly_Designated_Type (T_Name, Desig_Type);

      Check_Restriction (No_Access_Subprograms, T_Def);
   end Access_Subprogram_Declaration;

   ----------------------------
   -- Access_Type_Declaration --
   ----------------------------

   procedure Access_Type_Declaration (T : Entity_Id; Def : Node_Id) is
      S : constant Node_Id := Subtype_Indication (Def);
      P : constant Node_Id := Parent (Def);
   begin
      --  Check for permissible use of incomplete type

      if Nkind (S) /= N_Subtype_Indication then
         Analyze (S);

         if Ekind (Root_Type (Entity (S))) = E_Incomplete_Type then
            Set_Directly_Designated_Type (T, Entity (S));
         else
            Set_Directly_Designated_Type (T,
              Process_Subtype (S, P, T, 'P'));
         end if;

      else
         Set_Directly_Designated_Type (T,
           Process_Subtype (S, P, T, 'P'));
      end if;

      if All_Present (Def) or Constant_Present (Def) then
         Set_Ekind (T, E_General_Access_Type);
      else
         Set_Ekind (T, E_Access_Type);
      end if;

      if Base_Type (Designated_Type (T)) = T then
         Error_Msg_N ("access type cannot designate itself", S);
      end if;

      Set_Etype (T, T);

      Init_Size_Align (T);

      Set_Is_Access_Constant (T, Constant_Present (Def));
   end Access_Type_Declaration;

   -------------------------------
   -- Reactive_Type_Declaration --
   -------------------------------

   procedure Reactive_Type_Declaration (T : Entity_Id; Def : Node_Id) is
   begin
      Put_Line ("Reactive_Type_Declaration");
      Set_Ekind       (T, E_Reactive_Type);
      Set_Etype       (T, T);
      Set_Is_Constrained (T, True);
      Init_Size_Align (T);
   end Reactive_Type_Declaration;

   -----------------------------------
   -- Analyze_Component_Declaration --
   -----------------------------------

   procedure Analyze_Component_Declaration (N : Node_Id) is
      Id : constant Entity_Id := Defining_Identifier (N);
      T  : Entity_Id;
      P  : Entity_Id;

   begin
      Generate_Definition (Id);
      Enter_Name (Id);
      T := Find_Type_Of_Object 
	(Subtype_Indication (Component_Definition (N)), N);

      --  If the subtype is a constrained subtype of the enclosing record,
      --  (which must have a partial view) the back-end does not handle
      --  properly the recursion. Rewrite the component declaration with
      --  an explicit subtype indication, which is acceptable to Gigi. We
      --  can copy the tree directly because side effects have already been
      --  removed from discriminant constraints.

      if Ekind (T) = E_Access_Subtype
        and then Is_Entity_Name (Subtype_Indication (Component_Definition (N)))
        and then Comes_From_Source (T)
        and then Nkind (Parent (T)) = N_Subtype_Declaration
        and then Etype (Directly_Designated_Type (T)) = Current_Scope
      then
         Rewrite
           (Subtype_Indication (Component_Definition (N)),
             New_Copy_Tree (Subtype_Indication (Parent (T))));
         T := Find_Type_Of_Object
                 (Subtype_Indication (Component_Definition (N)), N);
      end if;

      --  If the component declaration includes a default expression, then we
      --  check that the component is not of a limited type (RM 3.7(5)),
      --  and do the special preanalysis of the expression (see section on
      --  "Handling of Default and Per-Object Expressions" in the spec of
      --  package Sem).

      if Present (Expression (N)) then
         Analyze_Per_Use_Expression (Expression (N), T);
      end if;

      --  The parent type may be a private view with unknown discriminants,
      --  and thus unconstrained. Regular components must be constrained.

      if Is_Indefinite_Subtype (T) and then Chars (Id) /= Name_uParent then
         Error_Msg_N
           ("unconstrained subtype in component declaration",
            Subtype_Indication (Component_Definition (N)));

      --  Components cannot be abstract, except for the special case of
      --  the _Parent field (case of extending an abstract tagged type)

      elsif Is_Abstract (T) and then Chars (Id) /= Name_uParent then
         Error_Msg_N ("type of a component cannot be abstract", N);
      end if;

      Set_Etype (Id, T);
      Set_Is_Aliased (Id, Aliased_Present (Component_Definition (N)));

      --  If this component is private (or depends on a private type),
      --  flag the record type to indicate that some operations are not
      --  available.

      P := Private_Component (T);

      if Present (P) then
         --  Check for circular definitions.

         if P = Any_Type then
            Set_Etype (Id, Any_Type);

         --  There is a gap in the visibility of operations only if the
         --  component type is not defined in the scope of the record type.

         elsif Scope (P) = Scope (Current_Scope) then
            null;

         else
            Set_Is_Private_Composite (Current_Scope);
         end if;
      end if;

      Set_Original_Record_Component (Id, Id);
   end Analyze_Component_Declaration;

   --------------------------
   -- Analyze_Declarations --
   --------------------------

   procedure Analyze_Declarations (L : List_Id) is
      D           : Node_Id;
      Next_Node   : Node_Id;
      Freeze_From : Entity_Id := Empty;

      procedure Adjust_D;
      --  Adjust D not to include implicit label declarations, since these
      --  have strange Sloc values that result in elaboration check problems.
      --  (They have the sloc of the label as found in the source, and that
      --  is ahead of the current declarative part).

      --------------
      -- Adjust_D --
      --------------

      procedure Adjust_D is
      begin
         while Present (Prev (D))
           and then Nkind (D) = N_Implicit_Label_Declaration
         loop
            Prev (D);
         end loop;
      end Adjust_D;

   --  Start of processing for Analyze_Declarations

   begin
      D := First (L);
      while Present (D) loop

         --  Complete analysis of declaration

         Analyze (D);
         Next_Node := Next (D);

         if No (Freeze_From) then
            Freeze_From := First_Entity (Current_Scope);
         end if;

         --  At the end of a declarative part, freeze remaining entities
         --  declared in it. The end of the visible declarations of a
         --  package specification is not the end of a declarative part
         --  if private declarations are present. The end of a package
         --  declaration is a freezing point only if it a library package.
         --  A task definition or protected type definition is not a freeze
         --  point either. Finally, we do not freeze entities in generic
         --  scopes, because there is no code generated for them and freeze
         --  nodes will be generated for the instance.

         --  The end of a package instantiation is not a freeze point, but
         --  for now we make it one, because the generic body is inserted
         --  (currently) immediately after. Generic instantiations will not
         --  be a freeze point once delayed freezing of bodies is implemented.
         --  (This is needed in any case for early instantiations ???).

         if No (Next_Node) then
            if Nkind (Parent (L)) = N_Component_List
            then
               null;

            elsif Nkind (Parent (L)) /= N_Package_Specification then
               if Nkind (Parent (L)) = N_Package_Body then
                  Freeze_From := First_Entity (Current_Scope);
               end if;

               Adjust_D;
               Freeze_All (Freeze_From, D);
               Freeze_From := Last_Entity (Current_Scope);

            elsif Scope (Current_Scope) /= Standard_Standard
              and then not Is_Child_Unit (Current_Scope)
              and then No (Generic_Parent (Parent (L)))
            then
               null;

            elsif L /= Visible_Declarations (Parent (L))
               or else No (Private_Declarations (Parent (L)))
               or else Is_Empty_List (Private_Declarations (Parent (L)))
            then
               Adjust_D;
               Freeze_All (Freeze_From, D);
               Freeze_From := Last_Entity (Current_Scope);
            end if;

         --  If next node is a body then freeze all types before the body.
         --  An exception occurs for expander generated bodies, which can
         --  be recognized by their already being analyzed. The expander
         --  ensures that all types needed by these bodies have been frozen
         --  but it is not necessary to freeze all types (and would be wrong
         --  since it would not correspond to an RM defined freeze point).

         elsif not Analyzed (Next_Node)
           and then (Nkind (Next_Node) = N_Subprogram_Body
             or else Nkind (Next_Node) = N_Package_Body)
         then
            Adjust_D;
            Freeze_All (Freeze_From, D);
            Freeze_From := Last_Entity (Current_Scope);
         end if;

         D := Next_Node;
      end loop;
   end Analyze_Declarations;

   ----------------------------------
   -- Analyze_Incomplete_Type_Decl --
   ----------------------------------

   procedure Analyze_Incomplete_Type_Decl (N : Node_Id) is
      F : constant Boolean := Is_Pure (Current_Scope);
      T : Entity_Id;

   begin
      Generate_Definition (Defining_Identifier (N));

      --  Process an incomplete declaration. The identifier must not have been
      --  declared already in the scope. However, an incomplete declaration may
      --  appear in the private part of a package, for a private type that has
      --  already been declared.

      --  In this case, the discriminants (if any) must match.

      T := Find_Type_Name (N);

      Set_Ekind (T, E_Incomplete_Type);
      Init_Size_Align (T);
      Set_Is_First_Subtype (T, True);
      Set_Etype (T, T);
      New_Scope (T);

      Set_Stored_Constraint (T, No_Elist);

      End_Scope;

      --  If the type has discriminants, non-trivial subtypes may be
      --  be declared before the full view of the type. The full views
      --  of those subtypes will be built after the full view of the type.

      Set_Private_Dependents (T, New_Elmt_List);
      Set_Is_Pure (T, F);
   end Analyze_Incomplete_Type_Decl;

   --------------------------------
   -- Analyze_Number_Declaration --
   --------------------------------

   procedure Analyze_Number_Declaration (N : Node_Id) is
      Id    : constant Entity_Id := Defining_Identifier (N);
      E     : constant Node_Id   := Expression (N);
      T     : Entity_Id;
      Index : Interp_Index;
      It    : Interp;

   begin
      Generate_Definition (Id);
      Enter_Name (Id);

      --  This is an optimization of a common case of an integer literal

      if Nkind (E) = N_Integer_Literal then
         Set_Is_Static_Expression (E, True);
         Set_Etype                (E, Universal_Integer);

         Set_Etype     (Id, Universal_Integer);
         Set_Ekind     (Id, E_Named_Integer);
         Set_Is_Frozen (Id, True);
         return;
      end if;

      Set_Is_Pure (Id, Is_Pure (Current_Scope));

      --  Process expression, replacing error by integer zero, to avoid
      --  cascaded errors or aborts further along in the processing

      --  Replace Error by integer zero, which seems least likely to
      --  cause cascaded errors.

      if E = Error then
         Rewrite (E, Make_Integer_Literal (Sloc (E), Uint_0));
         Set_Error_Posted (E);
      end if;

      Analyze (E);

      --  Verify that the expression is static and numeric. If
      --  the expression is overloaded, we apply the preference
      --  rule that favors root numeric types.

      if not Is_Overloaded (E) then
         T := Etype (E);

      else
         T := Any_Type;
         Get_First_Interp (E, Index, It);

         while Present (It.Typ) loop
            if (Is_Integer_Type (It.Typ)
                 or else Is_Real_Type (It.Typ))
              and then (Scope (Base_Type (It.Typ))) = Standard_Standard
            then
               if T = Any_Type then
                  T := It.Typ;

               elsif It.Typ = Universal_Real
                 or else It.Typ = Universal_Integer
               then
                  --  Choose universal interpretation over any other.

                  T := It.Typ;
                  exit;
               end if;
            end if;

            Get_Next_Interp (Index, It);
         end loop;
      end if;

      if Is_Integer_Type (T)  then
         Resolve (E, T);
         Set_Etype (Id, Universal_Integer);
         Set_Ekind (Id, E_Named_Integer);

      elsif Is_Real_Type (T) then

         --  Because the real value is converted to universal_real, this
         --  is a legal context for a universal fixed expression.

         if T = Universal_Fixed then
            declare
               Loc  : constant Source_Ptr := Sloc (N);
               Conv : constant Node_Id := Make_Type_Conversion (Loc,
                        Subtype_Mark =>
                          New_Occurrence_Of (Universal_Real, Loc),
                        Expression => Relocate_Node (E));

            begin
               Rewrite (E, Conv);
               Analyze (E);
            end;

         elsif T = Any_Fixed then
            Error_Msg_N ("illegal context for mixed mode operation", E);

            --  Expression is of the form : universal_fixed * integer.
            --  Try to resolve as universal_real.

            T := Universal_Real;
            Set_Etype (E, T);
         end if;

         Resolve (E, T);
         Set_Etype (Id, Universal_Real);
         Set_Ekind (Id, E_Named_Real);

      else
         Wrong_Type (E, Any_Numeric);
         Resolve (E, T);

         Set_Etype               (Id, T);
         Set_Ekind               (Id, E_Constant);
         Set_Never_Set_In_Source (Id, True);
         Set_Is_True_Constant    (Id, True);
         return;
      end if;

      if Nkind (E) = N_Integer_Literal
        or else Nkind (E) = N_Real_Literal
      then
         Set_Etype (E, Etype (Id));
      end if;

      if not Is_OK_Static_Expression (E) then
         Flag_Non_Static_Expr
           ("non-static expression used in number declaration!", E);
         Rewrite (E, Make_Integer_Literal (Sloc (N), 1));
         Set_Etype (E, Any_Type);
      end if;
   end Analyze_Number_Declaration;

   --------------------------------
   -- Analyze_Object_Declaration --
   --------------------------------

   procedure Analyze_Object_Declaration (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Id    : constant Entity_Id  := Defining_Identifier (N);
      T     : Entity_Id;
      Act_T : Entity_Id;

      E : Node_Id := Expression (N);
      --  E is set to Expression (N) throughout this routine. When
      --  Expression (N) is modified, E is changed accordingly.

      Prev_Entity : Entity_Id := Empty;

   begin
      --  There are three kinds of implicit types generated by an
      --  object declaration:

      --   1. Those for generated by the original Object Definition

      --   2. Those generated by the Expression

      --   3. Those used to constrained the Object Definition with the
      --       expression constraints when it is unconstrained

      --  They must be generated in this order to avoid order of elaboration
      --  issues. Thus the first step (after entering the name) is to analyze
      --  the object definition.

      if Constant_Present (N) then
         Prev_Entity := Current_Entity_In_Scope (Id);

         --  If homograph is an implicit subprogram, it is overridden by the
         --  current declaration.

         if Present (Prev_Entity)
           and then Is_Overloadable (Prev_Entity)
           and then Is_Inherited_Operation (Prev_Entity)
         then
            Prev_Entity := Empty;
         end if;
      end if;

      if Present (Prev_Entity) then
         Constant_Redeclaration (Id, N, T);

         Generate_Reference (Prev_Entity, Id, 'c');
         Set_Completion_Referenced (Id);

         if Error_Posted (N) then
            --  Type mismatch or illegal redeclaration, Do not analyze
            --  expression to avoid cascaded errors.

            T := Find_Type_Of_Object (Object_Definition (N), N);
            Set_Etype (Id, T);
            Set_Ekind (Id, E_Variable);
            return;
         end if;

      --  In the normal case, enter identifier at the start to catch
      --  premature usage in the initialization expression.

      else
         Generate_Definition (Id);
         Enter_Name (Id);

         T := Find_Type_Of_Object (Object_Definition (N), N);

         if Error_Posted (Id) then
            Set_Etype (Id, T);
            Set_Ekind (Id, E_Variable);
            return;
         end if;
      end if;

      Set_Is_Pure (Id, Is_Pure (Current_Scope));

      --  If deferred constant, make sure context is appropriate. We detect
      --  a deferred constant as a constant declaration with no expression.
      --  A deferred constant can appear in a package body if its completion
      --  is by means of an interface pragma.

      if Constant_Present (N)
        and then No (E)
      then
         if not Is_Package (Current_Scope) then
            Error_Msg_N
              ("invalid context for deferred constant declaration ('R'M 7.4)",
                N);
            Error_Msg_N
              ("\declaration requires an initialization expression",
                N);
            Set_Constant_Present (N, False);
         end if;

      --  If not a deferred constant, then object declaration freezes its type

      else
         Check_Fully_Declared (T, N);
         Freeze_Before (N, T);
      end if;

      --  If the object was created by a constrained array definition, then
      --  set the link in both the anonymous base type and anonymous subtype
      --  that are built to represent the array type to point to the object.
      
      if Nkind (Object_Definition (Declaration_Node (Id))) =
                        N_Constrained_Array_Definition
      then
         Set_Related_Array_Object (T, Id);
         Set_Related_Array_Object (Base_Type (T), Id);
      end if;

      --  The actual subtype of the object is the nominal subtype, unless
      --  the nominal one is unconstrained and obtained from the expression.

      Act_T := T;

      --  Process initialization expression if present and not in error

      if Present (E) and then E /= Error then
         Analyze (E);

         --  If an initialization expression is present, then we set the
         --  Is_True_Constant flag. It will be reset if this is a variable
         --  and it is indeed modified.

         Set_Is_True_Constant (Id, True);

         Set_Etype (Id, T);             --  may be overridden later on.
         Resolve (E, T);
         Check_Unset_Reference (E);

         if Compile_Time_Known_Value (E) then
            Set_Current_Value (Id, E);
         end if;

         --  Check incorrect use of dynamically tagged expressions. Note
         --  the use of Is_Tagged_Type (T) which seems redundant but is in
         --  fact important to avoid spurious errors due to expanded code
         --  for dispatching functions over an anonymous access type

         if (Is_Class_Wide_Type (Etype (E)) or else Is_Dynamically_Tagged (E))
           and then Is_Tagged_Type (T)
           and then not Is_Class_Wide_Type (T)
         then
            Error_Msg_N ("dynamically tagged expression not allowed!", E);
         end if;

--           Apply_Scalar_Range_Check (E, T);
--           Apply_Static_Length_Check (E, T);
      end if;

      --  Abstract type is never permitted for a variable or constant.
      --  Note: we inhibit this check for objects that do not come from
      --  source because there is at least one case (the expansion of
      --  x'class'input where x is abstract) where we legitimately
      --  generate an abstract object.

      if Is_Abstract (T) and then Comes_From_Source (N) then
         Error_Msg_N ("type of object cannot be abstract",
           Object_Definition (N));

      --  Case of unconstrained type

      elsif Is_Indefinite_Subtype (T) then
         --  Nothing to do in deferred constant case

         if Constant_Present (N) and then No (E) then
            null;

         --  Case of no initialization present

         elsif No (E) then
            if No_Initialization (N) then
               null;

            elsif Is_Class_Wide_Type (T) then
               Error_Msg_N
                 ("initialization required in class-wide declaration ", N);

            else
               Error_Msg_N
                 ("unconstrained subtype not allowed (need initialization)",
                  Object_Definition (N));
            end if;

         --  Case of initialization present but in error. Set initial
         --  expression as absent (but do not make above complaints)

         elsif E = Error then
            Set_Expression (N, Empty);
            E := Empty;

         --  Case of initialization present

         else
            --  Now we constrain the variable from the initializing expression

            --  If the expression is an aggregate, it has been expanded into
            --  individual assignments. Retrieve the actual type from the
            --  expanded construct.

            if Is_Array_Type (T)
              and then No_Initialization (N)
              and then Nkind (Original_Node (E)) = N_Aggregate
            then
               Act_T := Etype (E);

            else
               Expand_Subtype_From_Expr (N, T, Object_Definition (N), E);
               Act_T := Find_Type_Of_Object (Object_Definition (N), N);
            end if;

            Set_Is_Constr_Subt_For_U_Nominal (Act_T);

            if Aliased_Present (N) then
               Set_Is_Constr_Subt_For_UN_Aliased (Act_T);
            end if;

            Freeze_Before (N, Act_T);
            Freeze_Before (N, T);
         end if;

      elsif Is_Array_Type (T)
        and then No_Initialization (N)
        and then Nkind (Original_Node (E)) = N_Aggregate
      then
         if not Is_Entity_Name (Object_Definition (N)) then
            Act_T := Etype (E);
            Check_Compile_Time_Size (Act_T);

            if Aliased_Present (N) then
               Set_Is_Constr_Subt_For_UN_Aliased (Act_T);
            end if;
         end if;

         --  When the given object definition and the aggregate are specified
         --  independently, and their lengths might differ do a length check.
         --  This cannot happen if the aggregate is of the form (others =>...)

         if not Is_Constrained (T) then
            null;

         elsif T = Etype (E) then
            null;

         elsif Nkind (E) = N_Aggregate
           and then Present (Component_Associations (E))
           and then Present (Choices (First (Component_Associations (E))))
           and then Nkind (First
            (Choices (First (Component_Associations (E))))) = N_Others_Choice
         then
            null;

--           else
--              Apply_Length_Check (E, T);
         end if;
      end if;

      if T = Standard_Wide_Character
        or else Root_Type (T) = Standard_Wide_String
      then
         Check_Restriction (No_Wide_Characters, Object_Definition (N));
      end if;

      --  Now establish the proper kind and type of the object

      if Constant_Present (N) then
         Set_Ekind               (Id, E_Constant);
         Set_Never_Set_In_Source (Id, True);
         Set_Is_True_Constant    (Id, True);

      else
         Set_Ekind (Id, E_Variable);

         --  Case of no initializing expression present. If the type is not
         --  fully initialized, then we set Never_Set_In_Source, since this
         --  is a case of a potentially uninitialized object. Note that we
         --  do not consider access variables to be fully initialized for
         --  this purpose, since it still seems dubious if someone declares

         --  Note that we only do this for source declarations. If the object
         --  is declared by a generated declaration, we assume that it is not
         --  appropriate to generate warnings in that case.

         if No (E) then
            if (Is_Access_Type (T)
                 or else not Is_Fully_Initialized_Type (T))
              and then Comes_From_Source (N)
            then
               Set_Never_Set_In_Source (Id);
            end if;
         end if;
      end if;

      Init_Alignment (Id);
      Init_Esize     (Id);

      if Aliased_Present (N) then
         Set_Is_Aliased (Id);
      end if;
      
      Set_Etype (Id, Act_T);

      --  Some simple constant-propagation: if the expression is a constant
      --  string initialized with a literal, share the literal. This avoids
      --  a run-time copy.

      if Present (E)
        and then Is_Entity_Name (E)
        and then Ekind (Entity (E)) = E_Constant
        and then Base_Type (Etype (E)) = Standard_String
      then
         declare
            Val : constant Node_Id := Constant_Value (Entity (E));

         begin
            if Present (Val)
              and then Nkind (Val) = N_String_Literal
            then
               Rewrite (E, New_Copy (Val));
            end if;
         end;
      end if;

      if Present (Prev_Entity)
        and then Is_Frozen (Prev_Entity)
        and then not Error_Posted (Id)
      then
         Error_Msg_N ("full constant declaration appears too late", N);
      end if;
      
   end Analyze_Object_Declaration;

   ---------------------------
   -- Analyze_Others_Choice --
   ---------------------------

   --  Nothing to do for the others choice node itself, the semantic analysis
   --  of the others choice will occur as part of the processing of the parent

   procedure Analyze_Others_Choice (N : Node_Id) is
      pragma Warnings (Off, N);

   begin
      null;
   end Analyze_Others_Choice;

   --------------------------------
   -- Analyze_Per_Use_Expression --
   --------------------------------

   procedure Analyze_Per_Use_Expression (N : Node_Id; T : Entity_Id) is
      Save_In_Default_Expression : constant Boolean := In_Default_Expression;

   begin
      In_Default_Expression := True;
      Pre_Analyze_And_Resolve (N, T);
      In_Default_Expression := Save_In_Default_Expression;
   end Analyze_Per_Use_Expression;

   -------------------------------------------
   -- Analyze_Private_Extension_Declaration --
   -------------------------------------------

   procedure Analyze_Private_Extension_Declaration (N : Node_Id) is
      T           : constant Entity_Id := Defining_Identifier (N);
      Indic       : constant Node_Id   := Subtype_Indication (N);
      Parent_Type : Entity_Id;
      Parent_Base : Entity_Id;

   begin
      Generate_Definition (T);
      Enter_Name (T);

      Parent_Type := Find_Type_Of_Subtype_Indic (Indic);
      Parent_Base := Base_Type (Parent_Type);

      if Parent_Type = Any_Type
        or else Etype (Parent_Type) = Any_Type
      then
         Set_Ekind (T, Ekind (Parent_Type));
         Set_Etype (T, Any_Type);
         return;

      elsif not Is_Tagged_Type (Parent_Type) then
         Error_Msg_N
           ("parent of type extension must be a tagged type ", Indic);
         return;

      elsif Ekind (Parent_Type) = E_Void
        or else Ekind (Parent_Type) = E_Incomplete_Type
      then
         Error_Msg_N ("premature derivation of incomplete type", Indic);
         return;
      end if;

      --  Perhaps the parent type should be changed to the class-wide type's
      --  specific type in this case to prevent cascading errors ???

      if Is_Class_Wide_Type (Parent_Type) then
         Error_Msg_N
           ("parent of type extension must not be a class-wide type", Indic);
         return;
      end if;

      if (not Is_Package (Current_Scope)
           and then Nkind (Parent (N)) /= N_Generic_Subprogram_Declaration)
        or else In_Private_Part (Current_Scope)

      then
         Error_Msg_N ("invalid context for private extension", N);
      end if;

      --  Set common attributes

      Set_Is_Pure          (T, Is_Pure (Current_Scope));
      Set_Scope            (T, Current_Scope);
      Set_Ekind            (T, E_Record_Type_With_Private);
      Init_Size_Align      (T);

      Set_Etype            (T, Parent_Base);

      Set_Convention       (T, Convention     (Parent_Type));
      Set_First_Rep_Item   (T, First_Rep_Item (Parent_Type));
      Set_Is_First_Subtype (T);
      Make_Class_Wide_Type (T);

      Build_Derived_Record_Type (N, Parent_Type, T);
   end Analyze_Private_Extension_Declaration;

   ---------------------------------
   -- Analyze_Subtype_Declaration --
   ---------------------------------

   procedure Analyze_Subtype_Declaration (N : Node_Id) is
      Id : constant Entity_Id := Defining_Identifier (N);
      T  : Entity_Id;

   begin
      Generate_Definition (Id);
      Set_Is_Pure (Id, Is_Pure (Current_Scope));
      Init_Size_Align (Id);

      --  The following guard condition on Enter_Name is to handle cases
      --  where the defining identifier has already been entered into the
      --  scope but the declaration as a whole needs to be analyzed.

      --  This case in particular happens for derived enumeration types.
      --  The derived enumeration type is processed as an inserted enumeration
      --  type declaration followed by a rewritten subtype declaration. The
      --  defining identifier, however, is entered into the name scope very
      --  early in the processing of the original type declaration and
      --  therefore needs to be avoided here, when the created subtype
      --  declaration is analyzed. (See Build_Derived_Types)

      --  This also happens when the full view of a private type is a
      --  derived type with constraints. In this case the entity has been
      --  introduced in the private declaration.

      if Present (Etype (Id))
        and then (Is_Private_Type (Etype (Id))
                   or else Is_Rewrite_Substitution (N))
      then
         null;

      else
         Enter_Name (Id);
      end if;

      T := Process_Subtype (Subtype_Indication (N), N, Id, 'P');

      --  Inherit common attributes

      Set_Is_Generic_Type   (Id, Is_Generic_Type   (Base_Type (T)));
      Set_Is_Volatile       (Id, Is_Volatile       (T));
      Set_Treat_As_Volatile (Id, Treat_As_Volatile (T));
--      Set_Is_Atomic         (Id, Is_Atomic         (T));

      --  In the case where there is no constraint given in the subtype
      --  indication, Process_Subtype just returns the Subtype_Mark,
      --  so its semantic attributes must be established here.

      if Nkind (Subtype_Indication (N)) /= N_Subtype_Indication then
         Set_Etype (Id, Base_Type (T));

         case Ekind (T) is
            when Array_Kind =>
               Set_Ekind                (Id, E_Array_Subtype);

               --  Shouldn't we call Copy_Array_Subtype_Attributes here???

               Set_First_Index          (Id, First_Index        (T));
               Set_Is_Aliased           (Id, Is_Aliased         (T));
               Set_Is_Constrained       (Id, Is_Constrained     (T));

            when Enumeration_Kind =>
               Set_Ekind                (Id, E_Enumeration_Subtype);
               Set_First_Literal        (Id, First_Literal (Base_Type (T)));
               Set_Scalar_Range         (Id, Scalar_Range       (T));
               Set_Is_Character_Type    (Id, Is_Character_Type  (T));
               Set_Is_Constrained       (Id, Is_Constrained     (T));
               Set_RM_Size              (Id, RM_Size            (T));

            when Float_Kind =>
               Set_Ekind                (Id, E_Floating_Point_Subtype);
               Set_Scalar_Range         (Id, Scalar_Range       (T));
               Set_Digits_Value         (Id, Digits_Value       (T));
               Set_Is_Constrained       (Id, Is_Constrained     (T));

            when Signed_Integer_Kind =>
               Set_Ekind                (Id, E_Signed_Integer_Subtype);
               Set_Scalar_Range         (Id, Scalar_Range       (T));
               Set_Is_Constrained       (Id, Is_Constrained     (T));
               Set_RM_Size              (Id, RM_Size            (T));

            when Modular_Integer_Kind =>
               Set_Ekind                (Id, E_Modular_Integer_Subtype);
               Set_Scalar_Range         (Id, Scalar_Range       (T));
               Set_Is_Constrained       (Id, Is_Constrained     (T));
               Set_RM_Size              (Id, RM_Size            (T));

            when Class_Wide_Kind =>
               Set_Ekind                (Id, E_Class_Wide_Subtype);
               Set_First_Entity         (Id, First_Entity       (T));
               Set_Last_Entity          (Id, Last_Entity        (T));
               Set_Class_Wide_Type      (Id, Class_Wide_Type    (T));
               Set_Cloned_Subtype       (Id, T);
               Set_Is_Tagged_Type       (Id, True);

               if Ekind (T) = E_Class_Wide_Subtype then
                  Set_Equivalent_Type   (Id, Equivalent_Type    (T));
               end if;

            when E_Record_Type | E_Record_Subtype =>
               Set_Ekind                (Id, E_Record_Subtype);

               if Ekind (T) = E_Record_Subtype
                 and then Present (Cloned_Subtype (T))
               then
                  Set_Cloned_Subtype    (Id, Cloned_Subtype (T));
               else
                  Set_Cloned_Subtype    (Id, T);
               end if;

               Set_First_Entity         (Id, First_Entity       (T));
               Set_Last_Entity          (Id, Last_Entity        (T));
               Set_Is_Constrained       (Id, Is_Constrained     (T));

               if Is_Tagged_Type (T) then
                  Set_Is_Tagged_Type    (Id);
                  Set_Is_Abstract       (Id, Is_Abstract (T));
                  Set_Primitive_Operations
                                        (Id, Primitive_Operations (T));
                  Set_Class_Wide_Type   (Id, Class_Wide_Type (T));
               end if;

            when Private_Kind =>
               Set_Ekind              (Id, Subtype_Kind (Ekind   (T)));
               Set_Is_Constrained     (Id, Is_Constrained        (T));
               Set_First_Entity       (Id, First_Entity          (T));
               Set_Last_Entity        (Id, Last_Entity           (T));
               Set_Private_Dependents (Id, New_Elmt_List);

               if Is_Tagged_Type (T) then
                  Set_Is_Tagged_Type  (Id);
                  Set_Is_Abstract     (Id, Is_Abstract (T));
                  Set_Primitive_Operations
                                        (Id, Primitive_Operations (T));
                  Set_Class_Wide_Type (Id, Class_Wide_Type (T));
               end if;

               Prepare_Private_Subtype_Completion (Id, N);

            when Access_Kind =>
               Set_Ekind             (Id, E_Access_Subtype);
               Set_Is_Constrained    (Id, Is_Constrained        (T));
               Set_Is_Access_Constant
                                     (Id, Is_Access_Constant    (T));
               Set_Directly_Designated_Type
                                     (Id, Designated_Type       (T));

               --  A Pure library_item must not contain the declaration of a
               --  named access type, except within a subprogram, generic
               --  subprogram, task unit, or protected unit (RM 10.2.1(16)).

               if Comes_From_Source (Id)
                 and then In_Pure_Unit
               then
                  Error_Msg_N
                    ("named access types not allowed in pure unit", N);
               end if;

            --  If the subtype name denotes an incomplete type
            --  an error was already reported by Process_Subtype.

            when E_Incomplete_Type =>
               Set_Etype (Id, Any_Type);

            when others =>
               raise Program_Error;
         end case;
      end if;

      if Etype (Id) = Any_Type then
         return;
      end if;

      --  Some common processing on all types

      Set_Size_Info      (Id, T);
      Set_First_Rep_Item (Id, First_Rep_Item (T));

      T := Etype (Id);

      Set_Is_Immediately_Visible (Id, True);
      Set_Depends_On_Private     (Id, Has_Private_Component (T));

      if Present (Generic_Parent_Type (N))
        and then
          (Nkind
             (Parent (Generic_Parent_Type (N))) /= N_Formal_Type_Declaration
            or else Nkind
              (Formal_Type_Definition (Parent (Generic_Parent_Type (N))))
                /=  N_Formal_Private_Type_Definition)
      then
         if Is_Tagged_Type (Id) then
            if Is_Class_Wide_Type (Id) then
               Derive_Subprograms (Generic_Parent_Type (N), Id, Etype (T));
            else
               Derive_Subprograms (Generic_Parent_Type (N), Id, T);
            end if;

         elsif Scope (Etype (Id)) /= Standard_Standard then
            Derive_Subprograms (Generic_Parent_Type (N), Id);
         end if;
      end if;

      if Is_Private_Type (T)
        and then Present (Full_View (T))
      then
         Conditional_Delay (Id, Full_View (T));

      --  The subtypes of components or subcomponents of protected types
      --  do not need freeze nodes, which would otherwise appear in the
      --  wrong scope (before the freeze node for the protected type). The
      --  proper subtypes are those of the subcomponents of the corresponding
      --  record.

      elsif Present (Scope (Scope (Id))) -- error defense!
      then
         Conditional_Delay (Id, T);
      end if;
   end Analyze_Subtype_Declaration;

   --------------------------------
   -- Analyze_Subtype_Indication --
   --------------------------------

   procedure Analyze_Subtype_Indication (N : Node_Id) is
      T : constant Entity_Id := Subtype_Mark (N);
      R : constant Node_Id   := Range_Expression (Constraint (N));

   begin
      Analyze (T);

      if R /= Error then
         Analyze (R);
         Set_Etype (N, Etype (R));
      else
         Set_Error_Posted (R);
         Set_Error_Posted (T);
      end if;
   end Analyze_Subtype_Indication;

   ------------------------------
   -- Analyze_Type_Declaration --
   ------------------------------

   procedure Analyze_Type_Declaration (N : Node_Id) is
      Def    : constant Node_Id   := Type_Definition (N);
      Def_Id : constant Entity_Id := Defining_Identifier (N);
      T      : Entity_Id;
      Prev   : Entity_Id;

   begin
      Put_Line ("Analyze_Type_Declaration Begin");
      Prev := Find_Type_Name (N);

      --  The full view, if present, now points to the current type

      --  Ada0Y (AI-50217): If the type was previously decorated when imported
      --  through a LIMITED WITH clause, it appears as incomplete but has no
      --  full view.

      if Ekind (Prev) = E_Incomplete_Type
        and then Present (Full_View (Prev))
      then
         T := Full_View (Prev);
      else
         T := Prev;
      end if;

      Set_Is_Pure (T, Is_Pure (Current_Scope));

      --  We set the flag Is_First_Subtype here. It is needed to set the
      --  corresponding flag for the Implicit class-wide-type created
      --  during tagged types processing.

      Set_Is_First_Subtype (T, True);

      --  Elaborate the type definition according to kind, and generate
      --  subsidiary (implicit) subtypes where needed. We skip this if
      --  it was already done (this happens during the reanalysis that
      --  follows a call to the high level optimizer).

      if not Analyzed (T) then
         Set_Analyzed (T);

         case Nkind (Def) is

            when N_Access_To_Subprogram_Definition =>
               Access_Subprogram_Declaration (T, Def);

               --  Validate categorization rule against access type declaration
               --  usually a violation in Pure unit, Shared_Passive unit.

               Validate_Access_Type_Declaration (T, N);

            when N_Access_To_Object_Definition =>
               Access_Type_Declaration (T, Def);

               --  Validate categorization rule against access type declaration
               --  usually a violation in Pure unit, Shared_Passive unit.

               Validate_Access_Type_Declaration (T, N);

               --  If we are in a Remote_Call_Interface package and define
               --  a RACW, Read and Write attribute must be added.

            when N_Reactive_Type =>
	       Put_Line ("Analyze_Type_Declaration: reactive_Type ");
               Reactive_Type_Declaration (T, Def);

            when N_Array_Type_Definition =>
               Array_Type_Declaration (T, Def);

            when N_Derived_Type_Definition =>
               Derived_Type_Declaration (T, N, T /= Def_Id);

            when N_Enumeration_Type_Definition =>
               Enumeration_Type_Declaration (T, Def);

            when N_Floating_Point_Definition =>
               Floating_Point_Type_Declaration (T, Def);

            when N_Signed_Integer_Type_Definition =>
               Signed_Integer_Type_Declaration (T, Def);

            when N_Modular_Type_Definition =>
               Modular_Type_Declaration (T, Def);

            when N_Record_Definition =>
               Record_Type_Declaration (T, N, Prev);

            when others =>
               raise Program_Error;

         end case;
      end if;

      if Etype (T) = Any_Type then
         return;
      end if;

      --  Some common processing for all types

      Set_Depends_On_Private (T, Has_Private_Component (T));

      --  Both the declared entity, and its anonymous base type if one
      --  was created, need freeze nodes allocated.

      declare
         B : constant Entity_Id := Base_Type (T);

      begin
         --  In the case where the base type is different from the first
         --  subtype, we pre-allocate a freeze node, and set the proper
         --  link to the first subtype. Freeze_Entity will use this
         --  preallocated freeze node when it freezes the entity.

         if B /= T then
            Ensure_Freeze_Node (B);
            Set_First_Subtype_Link (Freeze_Node (B), T);
         end if;

         if not From_With_Type (T) then
            Set_Has_Delayed_Freeze (T);
         end if;
      end;

      --  Case of T is the full declaration of some private type which has
      --  been swapped in Defining_Identifier (N).

      if T /= Def_Id and then Is_Private_Type (Def_Id) then
         Process_Full_View (N, T, Def_Id);

         --  Record the reference. The form of this is a little strange,
         --  since the full declaration has been swapped in. So the first
         --  parameter here represents the entity to which a reference is
         --  made which is the "real" entity, i.e. the one swapped in,
         --  and the second parameter provides the reference location.

         Generate_Reference (T, T, 'c');
         Set_Completion_Referenced (Def_Id);

      --  For completion of incomplete type, process incomplete dependents
      --  and always mark the full type as referenced (it is the incomplete
      --  type that we get for any real reference).

      elsif Ekind (Prev) = E_Incomplete_Type then
         Process_Incomplete_Dependents (N, T, Prev);
         Generate_Reference (Prev, Def_Id, 'c');
         Set_Completion_Referenced (Def_Id);

      --  If not private type or incomplete type completion, this is a real
      --  definition of a new entity, so record it.

      else
         Generate_Definition (Def_Id);
      end if;
   end Analyze_Type_Declaration;

   ----------------------------
   -- Array_Type_Declaration --
   ----------------------------

   procedure Array_Type_Declaration (T : in out Entity_Id; Def : Node_Id) is
      Component_Def : constant Node_Id := Component_Definition (Def);
      Element_Type  : Entity_Id;
      Implicit_Base : Entity_Id;
      Index         : Node_Id;
      Related_Id    : Entity_Id := Empty;
      Nb_Index      : Nat;
      P             : constant Node_Id := Parent (Def);
      Priv          : Entity_Id;

   begin
      if Nkind (Def) = N_Constrained_Array_Definition then

         Index := First (Discrete_Subtype_Definitions (Def));
	 
         --  Find proper names for the implicit types which may be public.
         --  in case of anonymous arrays we use the name of the first object
         --  of that type as prefix.

         if No (T) then
            Related_Id :=  Defining_Identifier (P);
         else
            Related_Id := T;
         end if;

      else
         Index := First (Subtype_Marks (Def));
      end if;

      Nb_Index := 1;

      while Present (Index) loop
	 Analyze (Index);
	 Make_Index (Index, P, Related_Id, Nb_Index);
         Next_Index (Index);
         Nb_Index := Nb_Index + 1;
      end loop;

      Element_Type := Process_Subtype (Subtype_Indication (Component_Def),
                                       P, Related_Id, 'C');

      --  Constrained array case

      if No (T) then
         T := Create_Itype (E_Void, P, Related_Id, 'T');
      end if;

      if Nkind (Def) = N_Constrained_Array_Definition then

         --  Establish Implicit_Base as unconstrained base type

         Implicit_Base := Create_Itype (E_Array_Type, P, Related_Id, 'B');

         Init_Size_Align        (Implicit_Base);
         Set_Etype              (Implicit_Base, Implicit_Base);
         Set_Scope              (Implicit_Base, Current_Scope);
         Set_Has_Delayed_Freeze (Implicit_Base);

         --  The constrained array type is a subtype of the unconstrained one

         Set_Ekind          (T, E_Array_Subtype);
         Init_Size_Align    (T);
         Set_Etype          (T, Implicit_Base);
         Set_Scope          (T, Current_Scope);
         Set_Is_Constrained (T, True);
         Set_First_Index    (T, First (Discrete_Subtype_Definitions (Def)));
         Set_Has_Delayed_Freeze (T);
	 
         --  Complete setup of implicit base type

         Set_First_Index    (Implicit_Base, First_Index (T));
         Set_Component_Type (Implicit_Base, Element_Type);
         Set_Component_Size (Implicit_Base, Uint_0);
         Set_Finalize_Storage_Only
                            (Implicit_Base, Finalize_Storage_Only
                                                          (Element_Type));
	 declare
	    An : Node_Id;
	    Tdef : Node_Id;
	 begin
	    An := Associated_Node_For_Itype (Implicit_Base);
	    if Nkind (An) = N_Full_Type_Declaration then
	       Tdef := Type_Definition (An);
	    end if;
	 end;
	 
      --  Unconstrained array case

      else
         Set_Ekind                    (T, E_Array_Type);
         Init_Size_Align              (T);
         Set_Etype                    (T, T);
         Set_Scope                    (T, Current_Scope);
         Set_Component_Size           (T, Uint_0);
         Set_Is_Constrained           (T, False);
         Set_First_Index              (T, First (Subtype_Marks (Def)));
         Set_Has_Delayed_Freeze       (T, True);
         Set_Finalize_Storage_Only    (T, Finalize_Storage_Only
                                                        (Element_Type));
      end if;
      
      
      Set_Component_Type (Base_Type (T), Element_Type);

      if Aliased_Present (Component_Definition (Def)) then
         Set_Has_Aliased_Components (Etype (T));
      end if;

      Priv := Private_Component (Element_Type);

      if Present (Priv) then

         --  Check for circular definitions

         if Priv = Any_Type then
            Set_Component_Type (Etype (T), Any_Type);

         --  There is a gap in the visibility of operations on the composite
         --  type only if the component type is defined in a different scope.

         elsif Scope (Priv) = Current_Scope then
            null;

         else
            Set_Is_Private_Composite (Etype (T));
            Set_Is_Private_Composite (T);
         end if;
      end if;

      --  Create a concatenation operator for the new type. Internal
      --  array types created for packed entities do not need such, they
      --  are compatible with the user-defined type.

      if Number_Dimensions (T) = 1
         and then not Is_Packed_Array_Type (T)
      then
         New_Concatenation_Op (T);
      end if;

      --  In the case of an unconstrained array the parser has already
      --  verified that all the indices are unconstrained but we still
      --  need to make sure that the element type is constrained.

      if Is_Indefinite_Subtype (Element_Type) then
         Error_Msg_N
           ("unconstrained element type in array declaration",
            Subtype_Indication (Component_Def));

      elsif Is_Abstract (Element_Type) then
         Error_Msg_N
           ("The type of a component cannot be abstract",
            Subtype_Indication (Component_Def));
      end if;
   end Array_Type_Declaration;

   -------------------------------
   -- Build_Derived_Access_Type --
   -------------------------------

   procedure Build_Derived_Access_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id)
   is
      S : constant Node_Id := Subtype_Indication (Type_Definition (N));


      Subt : Entity_Id;

   begin
      --  Set the designated type so it is available in case this is
      --  an access to a self-referential type, e.g. a standard list
      --  type with a next pointer. Will be reset after subtype is built.

      Set_Directly_Designated_Type
        (Derived_Type, Designated_Type (Parent_Type));

      Subt := Process_Subtype (S, N);

      if Nkind (S) /= N_Subtype_Indication
        and then Subt /= Base_Type (Subt)
      then
         Set_Ekind (Derived_Type, E_Access_Subtype);
      end if;

      if Ekind (Derived_Type) = E_Access_Subtype then
         declare
            Pbase      : constant Entity_Id := Base_Type (Parent_Type);
            Ibase      : constant Entity_Id :=
                           Create_Itype (Ekind (Pbase), N, Derived_Type, 'B');
            Svg_Chars  : constant Name_Id   := Chars (Ibase);
            Svg_Next_E : constant Entity_Id := Next_Entity (Ibase);

         begin
            Copy_Node (Pbase, Ibase);

            Set_Chars             (Ibase, Svg_Chars);
            Set_Next_Entity       (Ibase, Svg_Next_E);
            Set_Sloc              (Ibase, Sloc (Derived_Type));
            Set_Scope             (Ibase, Scope (Derived_Type));
            Set_Freeze_Node       (Ibase, Empty);
            Set_Is_Frozen         (Ibase, False);
            Set_Comes_From_Source (Ibase, False);
            Set_Is_First_Subtype  (Ibase, False);

            Set_Etype (Ibase, Pbase);
            Set_Etype (Derived_Type, Ibase);
         end;
      end if;

      Set_Directly_Designated_Type
        (Derived_Type, Designated_Type (Subt));

      Set_Is_Constrained     (Derived_Type, Is_Constrained (Subt));
      Set_Is_Access_Constant (Derived_Type, Is_Access_Constant (Parent_Type));
      Set_Size_Info          (Derived_Type,                     Parent_Type);
      Set_RM_Size            (Derived_Type, RM_Size            (Parent_Type));
      Set_Depends_On_Private (Derived_Type,
                              Has_Private_Component (Derived_Type));
      Conditional_Delay      (Derived_Type, Subt);

   end Build_Derived_Access_Type;

   ------------------------------
   -- Build_Derived_Array_Type --
   ------------------------------

   procedure Build_Derived_Array_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id)
   is
      Loc           : constant Source_Ptr := Sloc (N);
      Tdef          : constant Node_Id    := Type_Definition (N);
      Indic         : constant Node_Id    := Subtype_Indication (Tdef);
      Parent_Base   : constant Entity_Id  := Base_Type (Parent_Type);
      Implicit_Base : Entity_Id;
      New_Indic     : Node_Id;

      procedure Make_Implicit_Base;
      --  If the parent subtype is constrained, the derived type is a
      --  subtype of an implicit base type derived from the parent base.

      ------------------------
      -- Make_Implicit_Base --
      ------------------------

      procedure Make_Implicit_Base is
      begin
         Implicit_Base :=
           Create_Itype (Ekind (Parent_Base), N, Derived_Type, 'B');

         Set_Ekind (Implicit_Base, Ekind (Parent_Base));
         Set_Etype (Implicit_Base, Parent_Base);

         Copy_Array_Subtype_Attributes   (Implicit_Base, Parent_Base);
         Copy_Array_Base_Type_Attributes (Implicit_Base, Parent_Base);

         Set_Has_Delayed_Freeze (Implicit_Base, True);
	 
      declare
	 Id : Entity_Id := Implicit_Base;
	 An : Node_Id;
	 Tdef : Node_Id;
      begin
	 An := Associated_Node_For_Itype (Id);
	 if Nkind (An) = N_Full_Type_Declaration then
	    Tdef := Type_Definition (An);
	 end if;
      end;	 
      end Make_Implicit_Base;

   --  Start of processing for Build_Derived_Array_Type

   begin
      if not Is_Constrained (Parent_Type) then
         if Nkind (Indic) /= N_Subtype_Indication then
            Set_Ekind (Derived_Type, E_Array_Type);

            Copy_Array_Subtype_Attributes   (Derived_Type, Parent_Type);
            Copy_Array_Base_Type_Attributes (Derived_Type, Parent_Type);

            Set_Has_Delayed_Freeze (Derived_Type, True);

         else
            Make_Implicit_Base;
            Set_Etype (Derived_Type, Implicit_Base);

            New_Indic :=
              Make_Subtype_Declaration (Loc,
                Defining_Identifier => Derived_Type,
                Subtype_Indication  =>
                  Make_Subtype_Indication (Loc,
                    Subtype_Mark => New_Reference_To (Implicit_Base, Loc),
                    Constraint => Constraint (Indic)));

            Rewrite (N, New_Indic);
            Analyze (N);
         end if;

      else
         if Nkind (Indic) /= N_Subtype_Indication then
            Make_Implicit_Base;

            Set_Ekind             (Derived_Type, Ekind (Parent_Type));
            Set_Etype             (Derived_Type, Implicit_Base);
            Copy_Array_Subtype_Attributes (Derived_Type, Parent_Type);

         else
            Error_Msg_N ("illegal constraint on constrained type", Indic);
         end if;
      end if;

      --  If the parent type is not a derived type itself, and is
      --  declared in a closed scope (e.g., a subprogram), then we
      --  need to explicitly introduce the new type's concatenation
      --  operator since Derive_Subprograms will not inherit the
      --  parent's operator. If the parent type is unconstrained, the
      --  operator is of the unconstrained base type.

      if Number_Dimensions (Parent_Type) = 1
        and then not Is_Derived_Type (Parent_Type)
        and then not Is_Package (Scope (Base_Type (Parent_Type)))
      then
         if not Is_Constrained (Parent_Type)
           and then Is_Constrained (Derived_Type)
         then
            New_Concatenation_Op (Implicit_Base);
         else
            New_Concatenation_Op (Derived_Type);
         end if;
      end if;
   end Build_Derived_Array_Type;

   ------------------------------------
   -- Build_Derived_Enumeration_Type --
   ------------------------------------

   procedure Build_Derived_Enumeration_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id)
   is
      Loc           : constant Source_Ptr := Sloc (N);
      Def           : constant Node_Id    := Type_Definition (N);
      Indic         : constant Node_Id    := Subtype_Indication (Def);
      Implicit_Base : Entity_Id;
      Literal       : Entity_Id;
      New_Lit       : Entity_Id;
      Literals_List : List_Id;
      Type_Decl     : Node_Id;
      Hi, Lo        : Node_Id;
      Rang_Expr     : Node_Id;

   begin
      --  Since types Standard.Character and Standard.Wide_Character do
      --  not have explicit literals lists we need to process types derived
      --  from them specially. This is handled by Derived_Standard_Character.
      --  If the parent type is a generic type, there are no literals either,
      --  and we construct the same skeletal representation as for the generic
      --  parent type.

      if Root_Type (Parent_Type) = Standard_Character
        or else Root_Type (Parent_Type) = Standard_Wide_Character
      then
         Derived_Standard_Character (N, Parent_Type, Derived_Type);

      elsif Is_Generic_Type (Root_Type (Parent_Type)) then
         declare
            Lo : Node_Id;
            Hi : Node_Id;

         begin
            Lo :=
               Make_Attribute_Reference (Loc,
                 Attribute_Name => Name_First,
                 Prefix => New_Reference_To (Derived_Type, Loc));
            Set_Etype (Lo, Derived_Type);

            Hi :=
               Make_Attribute_Reference (Loc,
                 Attribute_Name => Name_Last,
                 Prefix => New_Reference_To (Derived_Type, Loc));
            Set_Etype (Hi, Derived_Type);

            Set_Scalar_Range (Derived_Type,
               Make_Range (Loc,
                 Low_Bound => Lo,
                 High_Bound => Hi));
         end;

      else
         --  If a constraint is present, analyze the bounds to catch
         --  premature usage of the derived literals.

         if Nkind (Indic) = N_Subtype_Indication
           and then Nkind (Range_Expression (Constraint (Indic))) = N_Range
         then
            Analyze (Low_Bound  (Range_Expression (Constraint (Indic))));
            Analyze (High_Bound (Range_Expression (Constraint (Indic))));
         end if;

         --  Introduce an implicit base type for the derived type even
         --  if there is no constraint attached to it, since this seems
         --  closer to the Ada semantics. Build a full type declaration
         --  tree for the derived type using the implicit base type as
         --  the defining identifier. The build a subtype declaration
         --  tree which applies the constraint (if any) have it replace
         --  the derived type declaration.

         Literal := First_Literal (Parent_Type);
         Literals_List := New_List;

         while Present (Literal)
           and then Ekind (Literal) = E_Enumeration_Literal
         loop
            --  Literals of the derived type have the same representation as
            --  those of the parent type, but this representation can be
            --  overridden by an explicit representation clause. Indicate
            --  that there is no explicit representation given yet. These
            --  derived literals are implicit operations of the new type,
            --  and can be overriden by explicit ones.

            if Nkind (Literal) = N_Defining_Character_Literal then
               New_Lit :=
                 Make_Defining_Character_Literal (Loc, Chars (Literal));
            else
               New_Lit := Make_Defining_Identifier (Loc, Chars (Literal));
            end if;

            Set_Ekind                (New_Lit, E_Enumeration_Literal);
            Set_Enumeration_Pos      (New_Lit, Enumeration_Pos (Literal));
            Set_Enumeration_Rep      (New_Lit, Enumeration_Rep (Literal));
            Set_Enumeration_Rep_Expr (New_Lit, Empty);
            Set_Alias                (New_Lit, Literal);
            Set_Is_Known_Valid       (New_Lit, True);

            Append (New_Lit, Literals_List);
            Next_Literal (Literal);
         end loop;

         Implicit_Base :=
           Make_Defining_Identifier (Sloc (Derived_Type),
             New_External_Name (Chars (Derived_Type), 'B'));

         --  Indicate the proper nature of the derived type. This must
         --  be done before analysis of the literals, to recognize cases
         --  when a literal may be hidden by a previous explicit function
         --  definition (cf. c83031a).

         Set_Ekind (Derived_Type, E_Enumeration_Subtype);
         Set_Etype (Derived_Type, Implicit_Base);

         Type_Decl :=
           Make_Full_Type_Declaration (Loc,
             Defining_Identifier => Implicit_Base,
             Type_Definition =>
               Make_Enumeration_Type_Definition (Loc, Literals_List));

         Mark_Rewrite_Insertion (Type_Decl);
         Insert_Before (N, Type_Decl);
         Analyze (Type_Decl);

         --  After the implicit base is analyzed its Etype needs to be
         --  changed to reflect the fact that it is derived from the
         --  parent type which was ignored during analysis. We also set
         --  the size at this point.

         Set_Etype (Implicit_Base, Parent_Type);

         Set_Size_Info      (Implicit_Base,                 Parent_Type);
         Set_RM_Size        (Implicit_Base, RM_Size        (Parent_Type));
         Set_First_Rep_Item (Implicit_Base, First_Rep_Item (Parent_Type));

         Set_Has_Non_Standard_Rep
                            (Implicit_Base, Has_Non_Standard_Rep
                                                           (Parent_Type));
         Set_Has_Delayed_Freeze (Implicit_Base);

         --  Process the subtype indication including a validation check
         --  on the constraint, if any. If a constraint is given, its bounds
         --  must be implicitly converted to the new type.

         if Nkind (Indic) = N_Subtype_Indication then

            declare
               R   : constant Node_Id :=
                       Range_Expression (Constraint (Indic));

            begin
               if Nkind (R) = N_Range then
                  Hi := Build_Scalar_Bound
                          (High_Bound (R), Parent_Type, Implicit_Base);
                  Lo := Build_Scalar_Bound
                          (Low_Bound  (R), Parent_Type, Implicit_Base);

               else
                  --  Constraint is a Range attribute. Replace with the
                  --  explicit mention of the bounds of the prefix, which
                  --  must be a subtype.

                  Analyze (Prefix (R));
                  Hi :=
                    Convert_To (Implicit_Base,
                      Make_Attribute_Reference (Loc,
                        Attribute_Name => Name_Last,
                        Prefix =>
                          New_Occurrence_Of (Entity (Prefix (R)), Loc)));

                  Lo :=
                    Convert_To (Implicit_Base,
                      Make_Attribute_Reference (Loc,
                        Attribute_Name => Name_First,
                        Prefix =>
                          New_Occurrence_Of (Entity (Prefix (R)), Loc)));
               end if;

            end;

         else
            Hi :=
              Build_Scalar_Bound
                (Type_High_Bound (Parent_Type),
                 Parent_Type, Implicit_Base);
            Lo :=
               Build_Scalar_Bound
                 (Type_Low_Bound (Parent_Type),
                  Parent_Type, Implicit_Base);
         end if;

         Rang_Expr :=
           Make_Range (Loc,
             Low_Bound  => Lo,
             High_Bound => Hi);

         --  If we constructed a default range for the case where no range
         --  was given, then the expressions in the range must not freeze
         --  since they do not correspond to expressions in the source.

         if Nkind (Indic) /= N_Subtype_Indication then
            Set_Must_Not_Freeze (Lo);
            Set_Must_Not_Freeze (Hi);
            Set_Must_Not_Freeze (Rang_Expr);
         end if;

         Rewrite (N,
           Make_Subtype_Declaration (Loc,
             Defining_Identifier => Derived_Type,
             Subtype_Indication =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark => New_Occurrence_Of (Implicit_Base, Loc),
                 Constraint =>
                   Make_Range_Constraint (Loc,
                     Range_Expression => Rang_Expr))));

         Analyze (N);

         --  If pragma Discard_Names applies on the first subtype
         --  of the parent type, then it must be applied on this
         --  subtype as well.

         if Einfo.Discard_Names (First_Subtype (Parent_Type)) then
            Set_Discard_Names (Derived_Type);
         end if;
      end if;
   end Build_Derived_Enumeration_Type;

   --------------------------------
   -- Build_Derived_Numeric_Type --
   --------------------------------

   procedure Build_Derived_Numeric_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id)
   is
      Loc           : constant Source_Ptr := Sloc (N);
      Tdef          : constant Node_Id    := Type_Definition (N);
      Indic         : constant Node_Id    := Subtype_Indication (Tdef);
      Parent_Base   : constant Entity_Id  := Base_Type (Parent_Type);
      No_Constraint : constant Boolean    := Nkind (Indic) /=
                                                  N_Subtype_Indication;
      Implicit_Base    : Entity_Id;

      Lo : Node_Id;
      Hi : Node_Id;

   begin
      --  Process the subtype indication including a validation check on
      --  the constraint if any.

      Discard_Node (Process_Subtype (Indic, N));

      --  Introduce an implicit base type for the derived type even if
      --  there is no constraint attached to it, since this seems closer
      --  to the Ada semantics.

      Implicit_Base :=
        Create_Itype (Ekind (Parent_Base), N, Derived_Type, 'B');

      Set_Etype          (Implicit_Base, Parent_Base);
      Set_Ekind          (Implicit_Base, Ekind          (Parent_Base));
      Set_Size_Info      (Implicit_Base,                 Parent_Base);
      Set_RM_Size        (Implicit_Base, RM_Size        (Parent_Base));
      Set_First_Rep_Item (Implicit_Base, First_Rep_Item (Parent_Base));
      Set_Parent         (Implicit_Base, Parent (Derived_Type));

      if Is_Discrete_Or_Fixed_Point_Type (Parent_Base) then
         Set_RM_Size (Implicit_Base, RM_Size (Parent_Base));
      end if;

      Set_Has_Delayed_Freeze (Implicit_Base);

      Lo := New_Copy_Tree (Type_Low_Bound  (Parent_Base));
      Hi := New_Copy_Tree (Type_High_Bound (Parent_Base));

      Set_Scalar_Range (Implicit_Base,
        Make_Range (Loc,
          Low_Bound  => Lo,
          High_Bound => Hi));

      if Has_Infinities (Parent_Base) then
         Set_Includes_Infinities (Scalar_Range (Implicit_Base));
      end if;

      --  The Derived_Type, which is the entity of the declaration, is
      --  a subtype of the implicit base. Its Ekind is a subtype, even
      --  in the absence of an explicit constraint.

      Set_Etype (Derived_Type, Implicit_Base);
      declare
         Id : Entity_Id := Implicit_Base;
         An : Node_Id;
	 Tdef : Node_Id;
      begin
	 An := Associated_Node_For_Itype (Id);
	 if Nkind (An) = N_Full_Type_Declaration then
	    Tdef := Type_Definition (An);
	 end if;
      end;	 
      
      --  If we did not have a constraint, then the Ekind is set from the
      --  parent type (otherwise Process_Subtype has set the bounds)

      if No_Constraint then
         Set_Ekind (Derived_Type, Subtype_Kind (Ekind (Parent_Type)));
      end if;

      --  If we did not have a range constraint, then set the range
      --  from the parent type. Otherwise, the call to Process_Subtype
      --  has set the bounds.

      if No_Constraint
        or else not Has_Range_Constraint (Indic)
      then
         Set_Scalar_Range (Derived_Type,
           Make_Range (Loc,
             Low_Bound  => New_Copy_Tree (Type_Low_Bound  (Parent_Type)),
             High_Bound => New_Copy_Tree (Type_High_Bound (Parent_Type))));
         Set_Is_Constrained (Derived_Type, Is_Constrained (Parent_Type));

         if Has_Infinities (Parent_Type) then
            Set_Includes_Infinities (Scalar_Range (Derived_Type));
         end if;
      end if;

      --  Set remaining type-specific fields, depending on numeric type

      if Is_Modular_Integer_Type (Parent_Type) then
         Set_Modulus (Implicit_Base, Modulus (Parent_Base));

         Set_Non_Binary_Modulus
           (Implicit_Base, Non_Binary_Modulus (Parent_Base));

      elsif Is_Floating_Point_Type (Parent_Type) then

         --  Digits of base type is always copied from the digits value of
         --  the parent base type, but the digits of the derived type will
         --  already have been set if there was a constraint present.

         Set_Digits_Value (Implicit_Base, Digits_Value (Parent_Base));
         Set_Vax_Float    (Implicit_Base, Vax_Float    (Parent_Base));

         if No_Constraint then
            Set_Digits_Value (Derived_Type, Digits_Value (Parent_Type));
         end if;

      end if;

      --  The type of the bounds is that of the parent type, and they
      --  must be converted to the derived type.

      Convert_Scalar_Bounds (N, Parent_Type, Derived_Type, Loc);

      --  The implicit_base should be frozen when the derived type is frozen,
      --  but note that it is used in the conversions of the bounds. For
      --  fixed types we delay the determination of the bounds until the proper
      --  freezing point. For other numeric types this is rejected by GCC, for
      --  reasons that are currently unclear (???), so we choose to freeze the
      --  implicit base now. In the case of integers and floating point types
      --  this is harmless because subsequent representation clauses cannot
      --  affect anything, but it is still baffling that we cannot use the
      --  same mechanism for all derived numeric types.

         Freeze_Before (N, Implicit_Base);
   end Build_Derived_Numeric_Type;

   --------------------------------
   -- Build_Derived_Private_Type --
   --------------------------------

   procedure Build_Derived_Private_Type
     (N             : Node_Id;
      Parent_Type   : Entity_Id;
      Derived_Type  : Entity_Id;
      Is_Completion : Boolean;
      Derive_Subps  : Boolean := True)
   is
      Full_Decl   : Node_Id := Empty;
      Full_Der    : Entity_Id;
      Full_P      : Entity_Id;
      Par_Scope   : constant Entity_Id := Scope (Base_Type (Parent_Type));
      Swapped     : Boolean := False;

      procedure Copy_And_Build;
      --  Copy derived type declaration, replace parent with its full view,
      --  and analyze new declaration.

      --------------------
      -- Copy_And_Build --
      --------------------

      procedure Copy_And_Build is
         Full_N  : Node_Id;

      begin
         if Ekind (Parent_Type) in Record_Kind
           or else (Ekind (Parent_Type) in Enumeration_Kind
             and then Root_Type (Parent_Type) /= Standard_Character
             and then Root_Type (Parent_Type) /= Standard_Wide_Character
             and then not Is_Generic_Type (Root_Type (Parent_Type)))
         then
            Full_N := New_Copy_Tree (N);
            Insert_After (N, Full_N);
            Build_Derived_Type (
              Full_N, Parent_Type, Full_Der, True, Derive_Subps => False);

         else
            Build_Derived_Type (
              N, Parent_Type, Full_Der, True, Derive_Subps => False);
         end if;
      end Copy_And_Build;

   --  Start of processing for Build_Derived_Private_Type

   begin
      if Is_Tagged_Type (Parent_Type) then
         Build_Derived_Record_Type
           (N, Parent_Type, Derived_Type, Derive_Subps);
         return;

      else
         --  Untagged type, No discriminants on either view

         if Nkind (Subtype_Indication (Type_Definition (N)))
           = N_Subtype_Indication
         then
            Error_Msg_N
              ("illegal constraint on type without discriminants", N);
         end if;

         Set_Stored_Constraint (Derived_Type, No_Elist);
         Set_Is_Constrained    (Derived_Type, Is_Constrained (Parent_Type));

	 Set_Finalize_Storage_Only
	   (Base_Type (Derived_Type), Finalize_Storage_Only (Parent_Type));

         --  Construct the implicit full view by deriving from full
         --  view of the parent type. In order to get proper visibility,
         --  we install the parent scope and its declarations.

         --  ??? if the parent is untagged private and its
         --  completion is tagged, this mechanism will not
         --  work because we cannot derive from the tagged
         --  full view unless we have an extension

         if Present (Full_View (Parent_Type))
           and then not Is_Tagged_Type (Full_View (Parent_Type))
           and then not Is_Completion
         then
            Full_Der := Make_Defining_Identifier (Sloc (Derived_Type),
                                              Chars (Derived_Type));
            Set_Is_Itype (Full_Der);
            Set_Has_Private_Declaration (Full_Der);
            Set_Has_Private_Declaration (Derived_Type);
            Set_Associated_Node_For_Itype (Full_Der, N);
            Set_Parent (Full_Der, Parent (Derived_Type));
            Set_Full_View (Derived_Type, Full_Der);

            if not In_Open_Scopes (Par_Scope) then
               Install_Private_Declarations (Par_Scope);
               Install_Visible_Declarations (Par_Scope);
               Copy_And_Build;
               Uninstall_Declarations (Par_Scope);

            --  If parent scope is open and in another unit, and
            --  parent has a completion, then the derivation is taking
            --  place in the visible part of a child unit. In that
            --  case retrieve the full view of the parent momentarily.

            elsif not In_Same_Source_Unit (N, Parent_Type) then
               Full_P := Full_View (Parent_Type);
               Exchange_Declarations (Parent_Type);
               Copy_And_Build;
               Exchange_Declarations (Full_P);

            --  Otherwise it is a local derivation.

            else
               Copy_And_Build;
            end if;

            Set_Scope                (Full_Der, Current_Scope);
            Set_Is_First_Subtype     (Full_Der,
                                       Is_First_Subtype (Derived_Type));
            Set_Has_Size_Clause      (Full_Der, False);
            Set_Has_Alignment_Clause (Full_Der, False);
            Set_Next_Entity          (Full_Der, Empty);
            Set_Has_Delayed_Freeze   (Full_Der);
            Set_Is_Frozen            (Full_Der, False);
            Set_Freeze_Node          (Full_Der, Empty);
            Set_Depends_On_Private   (Full_Der,
                                        Has_Private_Component    (Full_Der));
            Set_Public_Status        (Full_Der);
         end if;
      end if;

      if Is_Private_Type (Derived_Type) then
         Set_Private_Dependents (Derived_Type, New_Elmt_List);
      end if;

      if Is_Private_Type (Parent_Type)
        and then Base_Type (Parent_Type) = Parent_Type
        and then In_Open_Scopes (Scope (Parent_Type))
      then
         Append_Elmt (Derived_Type, Private_Dependents (Parent_Type));

         if Is_Child_Unit (Scope (Current_Scope))
           and then Is_Completion
           and then In_Private_Part (Current_Scope)
           and then Scope (Parent_Type) /= Current_Scope
         then
            --  This is the unusual case where a type completed by a private
            --  derivation occurs within a package nested in a child unit,
            --  and the parent is declared in an ancestor. In this case, the
            --  full view of the parent type will become visible in the body
            --  of the enclosing child, and only then will the current type
            --  be possibly non-private. We build a underlying full view that
            --  will be installed when the enclosing child body is compiled.

	    Full_Der :=
	      Make_Defining_Identifier (Sloc (Derived_Type),
					Chars (Derived_Type));
	    Set_Is_Itype (Full_Der);

	    --  The full view will be used to swap entities on entry/exit
	    --  to the body, and must appear in the entity list for the
	    --  package.

	    Append_Entity (Full_Der, Scope (Derived_Type));
	    Set_Has_Private_Declaration (Full_Der);
	    Set_Has_Private_Declaration (Derived_Type);
	    Set_Associated_Node_For_Itype (Full_Der, N);
	    Set_Parent (Full_Der, Parent (Derived_Type));
	    Full_P := Full_View (Parent_Type);
	    Exchange_Declarations (Parent_Type);
	    Copy_And_Build;
	    Exchange_Declarations (Full_P);
	    Set_Underlying_Full_View (Derived_Type, Full_Der);
         end if;
      end if;
   end Build_Derived_Private_Type;
   
   ---------------------------
   -- Expand_Derived_Record --
   ---------------------------

   --  Add a field _parent at the beginning of the record extension. This is
   --  used to implement inheritance. Here are some examples of expansion:

   --  1. no discriminants
   --      type T2 is new T1 with null record;
   --   gives
   --      type T2 is new T1 with record
   --        _Parent : T1;
   --      end record;

   --  2. renamed discriminants
   --    type T2 (B, C : Int) is new T1 (A => B) with record
   --       _Parent : T1 (A => B);
   --       D : Int;
   --    end;

   --  3. inherited discriminants
   --    type T2 is new T1 with record -- discriminant A inherited
   --       _Parent : T1 (A);
   --       D : Int;
   --    end;

   procedure Expand_Derived_Record (T : Entity_Id; Def : Node_Id) is
      Indic        : constant Node_Id    := Subtype_Indication (Def);
      Loc          : constant Source_Ptr := Sloc (Def);
      Rec_Ext_Part : Node_Id             := Record_Extension_Part (Def);
      Par_Subtype  : Entity_Id;
      Comp_List    : Node_Id;
      Comp_Decl    : Node_Id;
      Parent_N     : Node_Id;
      List_Constr  : constant List_Id    := New_List;

   begin
      --  Expand_Tagged_Extension is called directly from the semantics, so
      --  we must check to see whether expansion is active before proceeding

      --  if not Expander_Active then
      --     return;
      --  end if;

      --  This may be a derivation of an untagged private type whose full
      --  view is tagged, in which case the Derived_Type_Definition has no
      --  extension part. Build an empty one now.

      if No (Rec_Ext_Part) then
         Rec_Ext_Part :=
           Make_Record_Definition (Loc,
             End_Label      => Empty,
             Component_List => Empty,
             Null_Present   => True);

         Set_Record_Extension_Part (Def, Rec_Ext_Part);
         Mark_Rewrite_Insertion (Rec_Ext_Part);
      end if;

      Comp_List := Component_List (Rec_Ext_Part);

      Parent_N := Make_Defining_Identifier (Loc, Name_uParent);

      --  If the derived type inherits its discriminants the type of the
      --  _parent field must be constrained by the inherited discriminants

      Par_Subtype := Process_Subtype (New_Copy_Tree (Indic), Def);

      Set_Parent_Subtype (T, Par_Subtype);

      Comp_Decl :=
        Make_Component_Declaration 
	(Loc,
	 Defining_Identifier => Parent_N,
	 Component_Definition =>
	   Make_Component_Definition
	   (Loc,
	    Aliased_Present => False,
	    Subtype_Indication => 
	      New_Reference_To (Base_Type (Par_Subtype), Loc)));

      if Null_Present (Rec_Ext_Part) then
         Set_Component_List (Rec_Ext_Part,
           Make_Component_List (Loc,
             Component_Items => New_List (Comp_Decl),
             Null_Present => False));
         Set_Null_Present (Rec_Ext_Part, False);

      elsif Null_Present (Comp_List)
        or else Is_Empty_List (Component_Items (Comp_List))
      then
         Set_Component_Items (Comp_List, New_List (Comp_Decl));
         Set_Null_Present (Comp_List, False);

      else
         Insert_Before (First (Component_Items (Comp_List)), Comp_Decl);
      end if;
      
      -- Analyze (Comp_Decl);
   end Expand_Derived_Record;
   
   -------------------------------
   -- Build_Derived_Record_Type --
   -------------------------------

   procedure Build_Derived_Record_Type
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id;
      Derive_Subps : Boolean := True)
   is
      Loc          : constant Source_Ptr := Sloc (N);
      Parent_Base  : Entity_Id;

      Type_Def     : Node_Id;
      Indic        : Node_Id;

      --  An empty Discs list means that there were no constraints in the
      --  subtype indication or that there was an error processing it.

      Assoc_List   : Elist_Id;

      New_Decl     : Node_Id;

      Is_Tagged          : constant Boolean := Is_Tagged_Type (Parent_Type);
      Private_Extension  : constant Boolean :=
                             (Nkind (N) = N_Private_Extension_Declaration);

      Constraint_Present : Boolean;

      Save_Etype        : Entity_Id;
      Save_Next_Entity  : Entity_Id;

   begin
      Parent_Base := Base_Type (Parent_Type);

      --  Before we start the previously documented transformations, here is
      --  a little fix for size and alignment of tagged types. Normally when
      --  we derive type D from type P, we copy the size and alignment of P
      --  as the default for D, and in the absence of explicit representation
      --  clauses for D, the size and alignment are indeed the same as the
      --  parent.

      --  But this is wrong for tagged types, since fields may be added,
      --  and the default size may need to be larger, and the default
      --  alignment may need to be larger.

      --  We therefore reset the size and alignment fields in the tagged
      --  case. Note that the size and alignment will in any case be at
      --  least as large as the parent type (since the derived type has
      --  a copy of the parent type in the _parent field)

      if Is_Tagged then
         Init_Size_Align (Derived_Type);
      end if;

      --  STEP 0a: figure out what kind of derived type declaration we have.

      if Private_Extension then
         Type_Def := N;
         Set_Ekind (Derived_Type, E_Record_Type_With_Private);

      else
         Type_Def := Type_Definition (N);

         --  Ekind (Parent_Base) in not necessarily E_Record_Type since
         --  Parent_Base can be a private type or private extension. However,
         --  for tagged types with an extension the newly added fields are
         --  visible and hence the Derived_Type is always an E_Record_Type.
         --  (except that the parent may have its own private fields).
         --  For untagged types we preserve the Ekind of the Parent_Base.

         if Present (Record_Extension_Part (Type_Def)) then
            Set_Ekind (Derived_Type, E_Record_Type);
         else
            Set_Ekind (Derived_Type, Ekind (Parent_Base));
         end if;
      end if;

      --  Indic can either be an N_Identifier if the subtype indication
      --  contains no constraint or an N_Subtype_Indication if the subtype
      --  indication has a constraint.

      Indic := Subtype_Indication (Type_Def);
      Constraint_Present := (Nkind (Indic) = N_Subtype_Indication);

      --  If we get here Derived_Type will have no discriminants or it will be
      --  a discriminated unconstrained base type.

      --  STEP 1a: perform preliminary actions/checks for derived tagged types

      if Is_Tagged then
         --  The parent type is frozen for non-private extensions (RM 13.14(7))

         if not Private_Extension then
            Freeze_Before (N, Parent_Type);
         end if;

         if Type_Access_Level (Derived_Type) /= Type_Access_Level (Parent_Type)
           and then not Is_Generic_Type (Derived_Type)
         then
	    Error_Msg_N
	      ("type extension at deeper accessibility level than parent",
	       Indic);
	    
         else
            declare
               GB : constant Node_Id := Enclosing_Generic_Body (Derived_Type);

            begin
               if Present (GB)
                 and then GB /= Enclosing_Generic_Body (Parent_Base)
               then
                  Error_Msg_NE
                    ("parent type of& must not be outside generic body"
                       & " ('R'M 3.9.1(4))",
                         Indic, Derived_Type);
               end if;
            end;
         end if;
      end if;

      --  In all other cases wipe out the list of inherited components (even
      --  inherited discriminants), it will be properly rebuilt here.

      Set_First_Entity (Derived_Type, Empty);
      Set_Last_Entity  (Derived_Type, Empty);

      --  STEP 1c: Initialize some flags for the Derived_Type

      --  The following flags must be initialized here so that
      --  Process_Discriminants can check that discriminants of tagged types
      --  do not have a default initial value and that access discriminants
      --  are only specified for limited records. For completeness, these
      --  flags are also initialized along with all the other flags below.

      Set_Is_Tagged_Type    (Derived_Type, Is_Tagged);

      --  STEP 2a: process discriminants of derived type if any.

      New_Scope (Derived_Type);

      --  For now mark a new derived type as constrained only if it has no
      --  discriminants. At the end of Build_Derived_Record_Type we properly
      --  set this flag in the case of private extensions. See comments in
      --  point 9. just before body of Build_Derived_Record_Type.

      Set_Is_Constrained (Derived_Type, True);

      --  Fields inherited from the Parent_Type

      Set_Discard_Names
	(Derived_Type, Einfo.Discard_Names (Parent_Type));
      Set_Has_Specified_Layout 
	(Derived_Type, Has_Specified_Layout (Parent_Type));
      Set_Is_Private_Composite
        (Derived_Type, Is_Private_Composite (Parent_Type));

      --  Fields inherited from the Parent_Base

      Set_Has_Non_Standard_Rep
        (Derived_Type, Has_Non_Standard_Rep     (Parent_Base));
      Set_Has_Primitive_Operations
        (Derived_Type, Has_Primitive_Operations (Parent_Base));
      Set_Finalize_Storage_Only
	(Derived_Type, Finalize_Storage_Only (Parent_Type));

      --  Set fields for private derived types.

      if Is_Private_Type (Derived_Type) then
         Set_Depends_On_Private (Derived_Type, True);
         Set_Private_Dependents (Derived_Type, New_Elmt_List);

      --  Inherit fields from non private record types. If this is the
      --  completion of a derivation from a private type, the parent itself
      --  is private, and the attributes come from its full view, which must
      --  be present.

      else
         if Is_Private_Type (Parent_Base)
           and then not Is_Record_Type (Parent_Base)
         then
            Set_Component_Alignment
              (Derived_Type, Component_Alignment (Full_View (Parent_Base)));
            Set_C_Pass_By_Copy
              (Derived_Type, C_Pass_By_Copy      (Full_View (Parent_Base)));
         else
            Set_Component_Alignment
              (Derived_Type, Component_Alignment (Parent_Base));

            Set_C_Pass_By_Copy
              (Derived_Type, C_Pass_By_Copy      (Parent_Base));
         end if;
      end if;

      --  Set fields for tagged types

      if Is_Tagged then
         Set_Primitive_Operations (Derived_Type, New_Elmt_List);

         Make_Class_Wide_Type (Derived_Type);
         Set_Is_Abstract      (Derived_Type, Abstract_Present (Type_Def));

      else
         Set_Is_Packed (Derived_Type, Is_Packed (Parent_Base));
         Set_Has_Non_Standard_Rep
                       (Derived_Type, Has_Non_Standard_Rep (Parent_Base));
      end if;

      Assoc_List := Inherit_Components 
	(N, Parent_Base, Derived_Type, Is_Tagged);

      --  STEP 5a: Copy the parent record declaration for untagged types

      if not Is_Tagged then

         --  Save the Etype field of Derived_Type. It is correctly set now, but
         --  the call to New_Copy tree may remap it to point to itself, which
         --  is not what we want. Ditto for the Next_Entity field.

         Save_Etype       := Etype (Derived_Type);
         Save_Next_Entity := Next_Entity (Derived_Type);

         --  Assoc_List maps all stored discriminants in the Parent_Base to
         --  stored discriminants in the Derived_Type. It is fundamental that
         --  no types or itypes with discriminants other than the stored
         --  discriminants appear in the entities declared inside
         --  Derived_Type. Gigi won't like it.

         New_Decl :=
           New_Copy_Tree
             (Parent (Parent_Base), Map => Assoc_List, New_Sloc => Loc);

         --  Restore the fields saved prior to the New_Copy_Tree call
         --  and compute the stored constraint.

         Set_Etype       (Derived_Type, Save_Etype);
         Set_Next_Entity (Derived_Type, Save_Next_Entity);

         --  Insert the new derived type declaration

         Rewrite (N, New_Decl);

      --  STEP 5b: Complete the processing for record extensions in generics

      --  There is no completion for record extensions declared in the
      --  parameter part of a generic, so we need to complete processing for
      --  these generic record extensions here. The Record_Type_Definition call
      --  will change the Ekind of the components from E_Void to E_Component.

      elsif Private_Extension and then Is_Generic_Type (Derived_Type) then
         Record_Type_Definition (Empty, Derived_Type);

      --  STEP 5c: Process the record extension for non private tagged types.

      elsif not Private_Extension then
         --  Add the _parent field in the derived type.

	 Expand_Derived_Record (Derived_Type, Type_Def);

         --  Analyze the record extension
         Record_Type_Definition
           (Record_Extension_Part (Type_Def), Derived_Type);
      end if;

      End_Scope;

      if Etype (Derived_Type) = Any_Type then
         return;
      end if;

      --  Set delayed freeze and then derive subprograms, we need to do
      --  this in this order so that derived subprograms inherit the
      --  derived freeze if necessary.

      Set_Has_Delayed_Freeze (Derived_Type);
      if Derive_Subps then
         Derive_Subprograms (Parent_Type, Derived_Type);
      end if;

   end Build_Derived_Record_Type;

   ------------------------
   -- Build_Derived_Type --
   ------------------------

   procedure Build_Derived_Type
     (N             : Node_Id;
      Parent_Type   : Entity_Id;
      Derived_Type  : Entity_Id;
      Is_Completion : Boolean;
      Derive_Subps  : Boolean := True)
   is
      Parent_Base : constant Entity_Id := Base_Type (Parent_Type);

   begin
      --  Set common attributes

      Set_Scope          (Derived_Type, Current_Scope);

      Set_Ekind          (Derived_Type, Ekind     (Parent_Base));
      Set_Etype          (Derived_Type,            Parent_Base);

      Set_Size_Info      (Derived_Type,                 Parent_Type);
      Set_RM_Size        (Derived_Type, RM_Size        (Parent_Type));
      Set_Convention     (Derived_Type, Convention     (Parent_Type));

      --  The derived type inherits the representation clauses of the parent.
      --  However, for a private type that is completed by a derivation, there
      --  may be operation attributes that have been specified already (stream
      --  attributes and External_Tag) and those must be provided. Finally,
      --  if the partial view is a private extension, the representation items
      --  of the parent have been inherited already, and should not be chained
      --  twice to the derived type.

      if Is_Tagged_Type (Parent_Type)
        and then Present (First_Rep_Item (Derived_Type))
      then
         --  The existing items are either operational items or items inherited
         --  from a private extension declaration.

         declare
            Rep   : Node_Id := First_Rep_Item (Derived_Type);
            Found : Boolean := False;

         begin
            while Present (Rep) loop
               if Rep = First_Rep_Item (Parent_Type) then
                  Found := True;
                  exit;
               else
                  Rep := Next_Rep_Item (Rep);
               end if;
            end loop;

            if not Found then
               Set_Next_Rep_Item
                 (First_Rep_Item (Derived_Type), First_Rep_Item (Parent_Type));
            end if;
         end;

      else
         Set_First_Rep_Item (Derived_Type, First_Rep_Item (Parent_Type));
      end if;

      case Ekind (Parent_Type) is
         when Numeric_Kind =>
            Build_Derived_Numeric_Type (N, Parent_Type, Derived_Type);

         when Array_Kind =>
            Build_Derived_Array_Type (N, Parent_Type,  Derived_Type);

         when E_Record_Type
            | E_Record_Subtype
            | Class_Wide_Kind  =>
            Build_Derived_Record_Type
              (N, Parent_Type, Derived_Type, Derive_Subps);
            return;

         when Enumeration_Kind =>
            Build_Derived_Enumeration_Type (N, Parent_Type, Derived_Type);

         when Access_Kind =>
            Build_Derived_Access_Type (N, Parent_Type, Derived_Type);

         when Incomplete_Or_Private_Kind =>
            Build_Derived_Private_Type
              (N, Parent_Type, Derived_Type, Is_Completion, Derive_Subps);

            --  For discriminated types, the derivation includes deriving
            --  primitive operations. For others it is done below.

            if Is_Tagged_Type (Parent_Type)
            then
               return;
            end if;

         when others =>
            raise Program_Error;
      end case;

      if Etype (Derived_Type) = Any_Type then
         return;
      end if;

      --  Set delayed freeze and then derive subprograms, we need to do
      --  this in this order so that derived subprograms inherit the
      --  derived freeze if necessary.

      Set_Has_Delayed_Freeze (Derived_Type);
      if Derive_Subps then
         Derive_Subprograms (Parent_Type, Derived_Type);
      end if;

      Set_Has_Primitive_Operations
        (Base_Type (Derived_Type), Has_Primitive_Operations (Parent_Type));
   end Build_Derived_Type;

   ---------------------------------
   -- Build_Discriminated_Subtype --
   ---------------------------------

   procedure Build_Discriminated_Subtype
     (T           : Entity_Id;
      Def_Id      : Entity_Id;
      Elist       : Elist_Id;
      Related_Nod : Node_Id;
      For_Access  : Boolean := False)
   is
   begin
      if Ekind (T) = E_Record_Type then
         if For_Access then
            Set_Ekind (Def_Id, E_Private_Subtype);
            Set_Is_For_Access_Subtype (Def_Id, True);
         else
            Set_Ekind (Def_Id, E_Record_Subtype);
         end if;

      elsif Is_Private_Type (T) then
         Set_Ekind (Def_Id, Subtype_Kind (Ekind (T)));

      elsif Is_Class_Wide_Type (T) then
         Set_Ekind (Def_Id, E_Class_Wide_Subtype);

      else
         --  Incomplete type. Attach subtype to list of dependents, to be
         --  completed with full view of parent type.

         Set_Ekind (Def_Id, Ekind (T));
         Append_Elmt (Def_Id, Private_Dependents (T));
      end if;

      Set_Etype             (Def_Id, T);
      Init_Size_Align       (Def_Id);
      Set_Is_Constrained    (Def_Id, Is_Constrained (T));

      Set_First_Entity      (Def_Id, First_Entity   (T));
      Set_Last_Entity       (Def_Id, Last_Entity    (T));
      Set_First_Rep_Item    (Def_Id, First_Rep_Item (T));

      if Is_Tagged_Type (T) then
         Set_Is_Tagged_Type  (Def_Id);
         Make_Class_Wide_Type (Def_Id);
      end if;

      if Is_Tagged_Type (T) then
         Set_Primitive_Operations (Def_Id, Primitive_Operations (T));
         Set_Is_Abstract (Def_Id, Is_Abstract (T));
      end if;

      --  Subtypes introduced by component declarations do not need to be
      --  marked as delayed, and do not get freeze nodes, because the semantics
      --  verifies that the parents of the subtypes are frozen before the
      --  enclosing record is frozen.

      if not Is_Type (Scope (Def_Id)) then
         Set_Depends_On_Private (Def_Id, Depends_On_Private (T));

         if Is_Private_Type (T)
           and then Present (Full_View (T))
         then
            Conditional_Delay (Def_Id, Full_View (T));
         else
            Conditional_Delay (Def_Id, T);
         end if;
      end if;

      if Is_Record_Type (T) then

         if not For_Access then
            Set_Cloned_Subtype (Def_Id, T);
         end if;
      end if;

   end Build_Discriminated_Subtype;

   ------------------------
   -- Build_Scalar_Bound --
   ------------------------

   function Build_Scalar_Bound
     (Bound : Node_Id;
      Par_T : Entity_Id;
      Der_T : Entity_Id) return Node_Id
   is
      New_Bound : Entity_Id;

   begin
      --  Note: not clear why this is needed, how can the original bound
      --  be unanalyzed at this point? and if it is, what business do we
      --  have messing around with it? and why is the base type of the
      --  parent type the right type for the resolution. It probably is
      --  not! It is OK for the new bound we are creating, but not for
      --  the old one??? Still if it never happens, no problem!

      Analyze_And_Resolve (Bound, Base_Type (Par_T));

      if Nkind (Bound) = N_Integer_Literal
        or else Nkind (Bound) = N_Real_Literal
      then
         New_Bound := New_Copy (Bound);
         Set_Etype (New_Bound, Der_T);
         Set_Analyzed (New_Bound);

      elsif Is_Entity_Name (Bound) then
         New_Bound := OK_Convert_To (Der_T, New_Copy (Bound));

      --  The following is almost certainly wrong. What business do we have
      --  relocating a node (Bound) that is presumably still attached to
      --  the tree elsewhere???

      else
         New_Bound := OK_Convert_To (Der_T, Relocate_Node (Bound));
      end if;

      Set_Etype (New_Bound, Der_T);
      return New_Bound;
   end Build_Scalar_Bound;

   --------------------------------
   -- Build_Underlying_Full_View --
   --------------------------------

   procedure Build_Underlying_Full_View
     (N   : Node_Id;
      Typ : Entity_Id;
      Par : Entity_Id)
   is
      Loc  : constant Source_Ptr := Sloc (N);
      Subt : constant Entity_Id :=
               Make_Defining_Identifier
                 (Loc, New_External_Name (Chars (Typ), 'S'));

      Constr : Node_Id;
      Indic  : Node_Id;

   begin
      if Nkind (N) = N_Full_Type_Declaration then
         Constr := Constraint (Subtype_Indication (Type_Definition (N)));

      --  ??? ??? is this assert right, I assume so otherwise Constr
      --  would not be defined below (this used to be an elsif)

      else pragma Assert (Nkind (N) = N_Subtype_Declaration);
         Constr := New_Copy_Tree (Constraint (Subtype_Indication (N)));
      end if;

      Indic := Make_Subtype_Declaration (Loc,
         Defining_Identifier => Subt,
         Subtype_Indication  =>
           Make_Subtype_Indication (Loc,
             Subtype_Mark => New_Reference_To (Par, Loc),
             Constraint   => New_Copy_Tree (Constr)));

      Insert_Before (N, Indic);
      Analyze (Indic);
      Set_Underlying_Full_View (Typ, Full_View (Subt));
   end Build_Underlying_Full_View;

   -------------------------------
   -- Check_Abstract_Overriding --
   -------------------------------

   procedure Check_Abstract_Overriding (T : Entity_Id) is
      Op_List  : Elist_Id;
      Elmt     : Elmt_Id;
      Subp     : Entity_Id;
      Type_Def : Node_Id;

   begin
      Op_List := Primitive_Operations (T);

      --  Loop to check primitive operations

      Elmt := First_Elmt (Op_List);
      while Present (Elmt) loop
         Subp := Node (Elmt);

         --  Special exception, do not complain about failure to
         --  override _Input and _Output, since we always provide
         --  automatic overridings for these subprograms.

         if Is_Abstract (Subp)
           and then not Is_Abstract (T)
         then
            if Present (Alias (Subp)) then
               --  Only perform the check for a derived subprogram when
               --  the type has an explicit record extension. This avoids
               --  incorrectly flagging abstract subprograms for the case
               --  of a type without an extension derived from a formal type
               --  with a tagged actual (can occur within a private part).

               Type_Def := Type_Definition (Parent (T));
               if Nkind (Type_Def) = N_Derived_Type_Definition
                 and then Present (Record_Extension_Part (Type_Def))
               then
                  Error_Msg_NE
                    ("type must be declared abstract or & overridden",
                     T, Subp);
               end if;
            else
               Error_Msg_NE
                 ("abstract subprogram not allowed for type&",
                  Subp, T);
               Error_Msg_NE
                 ("nonabstract type has abstract subprogram&",
                  T, Subp);
            end if;
         end if;

         Next_Elmt (Elmt);
      end loop;
   end Check_Abstract_Overriding;

   ----------------------
   -- Check_Completion --
   ----------------------

   procedure Check_Completion (Body_Id : Node_Id := Empty) is
      E : Entity_Id;

      procedure Post_Error;
      --  Post error message for lack of completion for entity E

      ----------------
      -- Post_Error --
      ----------------

      procedure Post_Error is
      begin
         --  If a generated entity has no completion, then either previous
         --  semantic errors have disabled the expansion phase, or else
         --  we had missing subunits, or else we are compiling without expan-
         --  sion, or else something is very wrong.

         if not Comes_From_Source (E) then
            pragma Assert
              (Serious_Errors_Detected > 0
                or else Configurable_Run_Time_Violations > 0
                or else Subunits_Missing
                or else not Expander_Active);
            return;

         --  Here for source entity

         else
            --  Here if no body to post the error message, so we post the error
            --  on the declaration that has no completion. This is not really
            --  the right place to post it, think about this later ???

            if No (Body_Id) then
               if Is_Type (E) then
                  Error_Msg_NE
                    ("missing full declaration for }", Parent (E), E);
               else
                  Error_Msg_NE
                    ("missing body for &", Parent (E), E);
               end if;

            --  Package body has no completion for a declaration that appears
            --  in the corresponding spec. Post error on the body, with a
            --  reference to the non-completed declaration.

            else
               Error_Msg_Sloc := Sloc (E);

               if Is_Type (E) then
                  Error_Msg_NE
                    ("missing full declaration for }!", Body_Id, E);

               elsif Is_Overloadable (E)
                 and then Current_Entity_In_Scope (E) /= E
               then
                  --  It may be that the completion is mistyped and appears
                  --  as a  distinct overloading of the entity.

                  declare
                     Candidate : constant Entity_Id :=
                                   Current_Entity_In_Scope (E);
                     Decl      : constant Node_Id :=
                                   Unit_Declaration_Node (Candidate);

                  begin
                     if Is_Overloadable (Candidate)
                       and then Ekind (Candidate) = Ekind (E)
                       and then Nkind (Decl) = N_Subprogram_Body
                       and then Acts_As_Spec (Decl)
                     then
                        Check_Type_Conformant (Candidate, E);

                     else
                        Error_Msg_NE ("missing body for & declared#!",
                           Body_Id, E);
                     end if;
                  end;
               else
                  Error_Msg_NE ("missing body for & declared#!",
                     Body_Id, E);
               end if;
            end if;
         end if;
      end Post_Error;

   --  Start processing for Check_Completion

   begin
      E := First_Entity (Current_Scope);
      while Present (E) loop
         if Is_Intrinsic_Subprogram (E) then
            null;

         --  The following situation requires special handling: a child
         --  unit that appears in the context clause of the body of its
         --  parent:

         --    procedure Parent.Child (...);
         --
         --    with Parent.Child;
         --    package body Parent is

         --  Here Parent.Child appears as a local entity, but should not
         --  be flagged as requiring completion, because it is a
         --  compilation unit.

         elsif     Ekind (E) = E_Function
           or else Ekind (E) = E_Procedure
           or else Ekind (E) = E_Generic_Function
           or else Ekind (E) = E_Generic_Procedure
         then
            if not Has_Completion (E)
              and then not Is_Abstract (E)
              and then Nkind (Parent (Unit_Declaration_Node (E))) /=
                                                       N_Compilation_Unit
              and then Chars (E) /= Name_uSize
            then
               Post_Error;
            end if;

         elsif Is_Package (E) then
            if Unit_Requires_Body (E) then
               if not Has_Completion (E)
                 and then Nkind (Parent (Unit_Declaration_Node (E))) /=
                                                       N_Compilation_Unit
               then
                  Post_Error;
               end if;

            elsif not Is_Child_Unit (E) then
               May_Need_Implicit_Body (E);
            end if;

         elsif Ekind (E) = E_Incomplete_Type
           and then No (Underlying_Type (E))
         then
            Post_Error;

         elsif Ekind (E) = E_Record_Type then
            if Is_Tagged_Type (E) then
               Check_Abstract_Overriding (E);
            end if;

         elsif Ekind (E) = E_Array_Type then
            null;

         end if;

         Next_Entity (E);
      end loop;
   end Check_Completion;

   ----------------------
   -- Check_Real_Bound --
   ----------------------

   procedure Check_Real_Bound (Bound : Node_Id) is
   begin
      if not Is_Real_Type (Etype (Bound)) then
         Error_Msg_N
           ("bound in real type definition must be of real type", Bound);

      elsif not Is_OK_Static_Expression (Bound) then
         Flag_Non_Static_Expr
           ("non-static expression used for real type bound!", Bound);

      else
         return;
      end if;

      Rewrite
        (Bound, Make_Real_Literal (Sloc (Bound), Ureal_0));
      Analyze (Bound);
      Resolve (Bound, Standard_Float);
   end Check_Real_Bound;

   ------------------------------
   -- Complete_Private_Subtype --
   ------------------------------

   procedure Complete_Private_Subtype
     (Priv        : Entity_Id;
      Full        : Entity_Id;
      Full_Base   : Entity_Id;
      Related_Nod : Node_Id)
   is
      Save_Next_Entity : Entity_Id;
      Save_Homonym     : Entity_Id;

   begin
      --  Set semantic attributes for (implicit) private subtype completion.
      --  If the full type has no discriminants, then it is a copy of the full
      --  view of the base. Otherwise, it is a subtype of the base with a
      --  possible discriminant constraint. Save and restore the original
      --  Next_Entity field of full to ensure that the calls to Copy_Node
      --  do not corrupt the entity chain.

      --  Note that the type of the full view is the same entity as the
      --  type of the partial view. In this fashion, the subtype has
      --  access to the correct view of the parent.

      Save_Next_Entity := Next_Entity (Full);
      Save_Homonym     := Homonym (Priv);

      case Ekind (Full_Base) is

         when E_Record_Type    |
              E_Record_Subtype |
              Class_Wide_Kind  |
              Private_Kind     =>
            Copy_Node (Priv, Full);

            Set_First_Entity       (Full, First_Entity (Full_Base));
            Set_Last_Entity        (Full, Last_Entity (Full_Base));

         when others =>
            Copy_Node (Full_Base, Full);
            Set_Chars          (Full, Chars (Priv));
            Conditional_Delay  (Full, Priv);
            Set_Sloc           (Full, Sloc (Priv));

      end case;

      Set_Next_Entity (Full, Save_Next_Entity);
      Set_Homonym     (Full, Save_Homonym);
      Set_Associated_Node_For_Itype (Full, Related_Nod);

      --  Set common attributes for all subtypes.

      Set_Ekind (Full, Subtype_Kind (Ekind (Full_Base)));

      --  The Etype of the full view is inconsistent. Gigi needs to see the
      --  structural full view,  which is what the current scheme gives:
      --  the Etype of the full view is the etype of the full base. However,
      --  if the full base is a derived type, the full view then looks like
      --  a subtype of the parent, not a subtype of the full base. If instead
      --  we write:

      --       Set_Etype (Full, Full_Base);

      --  then we get inconsistencies in the front-end (confusion between
      --  views). Several outstanding bugs are related to this.

      Set_Is_First_Subtype (Full, False);
      Set_Scope            (Full, Scope (Priv));
      Set_Size_Info        (Full, Full_Base);
      Set_RM_Size          (Full, RM_Size (Full_Base));
      Set_Is_Itype         (Full);

      --  A subtype of a private-type-without-discriminants, whose full-view
      --  has discriminants with default expressions, is not constrained!

      Set_Is_Constrained (Full, Is_Constrained (Full_Base));

      Set_First_Rep_Item     (Full, First_Rep_Item (Full_Base));
      Set_Depends_On_Private (Full, Has_Private_Component (Full));

      --  Freeze the private subtype entity if its parent is delayed,
      --  and not already frozen. We skip this processing if the type
      --  is an anonymous subtype of a record component, or is the
      --  corresponding record of a protected type, since ???

      if not Is_Type (Scope (Full)) then
         Set_Has_Delayed_Freeze (Full,
           Has_Delayed_Freeze (Full_Base)
               and then (not Is_Frozen (Full_Base)));
      end if;

      Set_Freeze_Node (Full, Empty);
      Set_Is_Frozen (Full, False);
      Set_Full_View (Priv, Full);

      if Is_Record_Type (Full_Base) then

         --  Show Full is simply a renaming of Full_Base.

         Set_Cloned_Subtype (Full, Full_Base);
      end if;

      --  It is unsafe to share to bounds of a scalar type, because the
      --  Itype is elaborated on demand, and if a bound is non-static
      --  then different orders of elaboration in different units will
      --  lead to different external symbols.

      if Is_Scalar_Type (Full_Base) then
         Set_Scalar_Range (Full,
           Make_Range (Sloc (Related_Nod),
             Low_Bound  =>
               Duplicate_Subexpr (Type_Low_Bound  (Full_Base)),
             High_Bound =>
               Duplicate_Subexpr (Type_High_Bound (Full_Base))));

         --  This completion inherits the bounds of the full parent, but if
         --  the parent is an unconstrained floating point type, so is the
         --  completion.

         if Is_Floating_Point_Type (Full_Base) then
            Set_Includes_Infinities
             (Scalar_Range (Full), Has_Infinities (Full_Base));
         end if;
      end if;

      --  ??? It seems that a lot of fields are missing that should be
      --  copied from  Full_Base to Full. Here are some that are introduced
      --  in a non-disruptive way but a cleanup is necessary.

      if Is_Tagged_Type (Full_Base) then
         Set_Is_Tagged_Type (Full);
         Set_Primitive_Operations (Full, Primitive_Operations (Full_Base));
         Set_Class_Wide_Type      (Full, Class_Wide_Type (Full_Base));
      end if;

   end Complete_Private_Subtype;

   ----------------------------
   -- Constant_Redeclaration --
   ----------------------------

   procedure Constant_Redeclaration
     (Id : Entity_Id;
      N  : Node_Id;
      T  : out Entity_Id)
   is
      Prev    : constant Entity_Id := Current_Entity_In_Scope (Id);
      Obj_Def : constant Node_Id := Object_Definition (N);
      New_T   : Entity_Id;

      procedure Check_Recursive_Declaration (Typ : Entity_Id);
      --  If deferred constant is an access type initialized with an
      --  allocator, check whether there is an illegal recursion in the
      --  definition, through a default value of some record subcomponent.
      --  This is normally detected when generating init procs, but requires
      --  this additional mechanism when expansion is disabled.

      ---------------------------------
      -- Check_Recursive_Declaration --
      ---------------------------------

      procedure Check_Recursive_Declaration (Typ : Entity_Id) is
         Comp : Entity_Id;

      begin
         if Is_Record_Type (Typ) then
            Comp := First_Component (Typ);

            while Present (Comp) loop
               if Comes_From_Source (Comp) then
                  if Present (Expression (Parent (Comp)))
                    and then Is_Entity_Name (Expression (Parent (Comp)))
                    and then Entity (Expression (Parent (Comp))) = Prev
                  then
                     Error_Msg_Sloc := Sloc (Parent (Comp));
                     Error_Msg_NE
                       ("illegal circularity with declaration for&#",
                         N, Comp);
                     return;

                  elsif Is_Record_Type (Etype (Comp)) then
                     Check_Recursive_Declaration (Etype (Comp));
                  end if;
               end if;

               Next_Component (Comp);
            end loop;
         end if;
      end Check_Recursive_Declaration;

   --  Start of processing for Constant_Redeclaration

   begin
      if Nkind (Parent (Prev)) = N_Object_Declaration then
         if Nkind (Object_Definition
                     (Parent (Prev))) = N_Subtype_Indication
         then
            --  Find type of new declaration. The constraints of the two
            --  views must match statically, but there is no point in
            --  creating an itype for the full view.

            if Nkind (Obj_Def) = N_Subtype_Indication then
               Find_Type (Subtype_Mark (Obj_Def));
               New_T := Entity (Subtype_Mark (Obj_Def));

            else
               Find_Type (Obj_Def);
               New_T := Entity (Obj_Def);
            end if;

            T := Etype (Prev);

         else
            --  The full view may impose a constraint, even if the partial
            --  view does not, so construct the subtype.

            New_T := Find_Type_Of_Object (Obj_Def, N);
            T     := New_T;
         end if;

      else
         --  Current declaration is illegal, diagnosed below in Enter_Name.

         T := Empty;
         New_T := Any_Type;
      end if;

      --  If previous full declaration exists, or if a homograph is present,
      --  let Enter_Name handle it, either with an error, or with the removal
      --  of an overridden implicit subprogram.

      if Ekind (Prev) /= E_Constant
        or else Present (Expression (Parent (Prev)))
        or else Present (Full_View (Prev))
      then
         Enter_Name (Id);

      --  Verify that types of both declarations match.

      elsif Base_Type (Etype (Prev)) /= Base_Type (New_T) then
         Error_Msg_Sloc := Sloc (Prev);
         Error_Msg_N ("type does not match declaration#", N);
         Set_Full_View (Prev, Id);
         Set_Etype (Id, Any_Type);

      --  If so, process the full constant declaration

      else
         Set_Full_View (Prev, Id);
         Set_Is_Public (Id, Is_Public (Prev));
         Set_Is_Internal (Id);
         Append_Entity (Id, Current_Scope);

         --  Check ALIASED present if present before (RM 7.4(7))

         if Is_Aliased (Prev)
           and then not Aliased_Present (N)
         then
            Error_Msg_Sloc := Sloc (Prev);
            Error_Msg_N ("ALIASED required (see declaration#)", N);
         end if;

         --  Check that placement is in private part and that the incomplete
         --  declaration appeared in the visible part.

         if Ekind (Current_Scope) = E_Package
           and then not In_Private_Part (Current_Scope)
         then
            Error_Msg_Sloc := Sloc (Prev);
            Error_Msg_N ("full constant for declaration#"
                         & " must be in private part", N);

         elsif Ekind (Current_Scope) = E_Package
           and then List_Containing (Parent (Prev))
           /= Visible_Declarations
             (Specification (Unit_Declaration_Node (Current_Scope)))
         then
            Error_Msg_N
              ("deferred constant must be declared in visible part",
                 Parent (Prev));
         end if;

         if Is_Access_Type (T)
           and then Nkind (Expression (N)) = N_Allocator
         then
            Check_Recursive_Declaration (Designated_Type (T));
         end if;
      end if;
   end Constant_Redeclaration;

   ----------------------
   -- Constrain_Access --
   ----------------------

   procedure Constrain_Access
     (Def_Id      : in out Entity_Id;
      S           : Node_Id;
      Related_Nod : Node_Id)
   is
      T             : constant Entity_Id := Entity (Subtype_Mark (S));
      Desig_Type    : constant Entity_Id := Designated_Type (T);
      Desig_Subtype : Entity_Id := Create_Itype (E_Void, Related_Nod);
      Constraint_OK : Boolean := True;

   begin
      if Is_Array_Type (Desig_Type) then
         Constrain_Array (Desig_Subtype, S, Related_Nod, Def_Id, 'P');

      else
         Error_Msg_N ("invalid constraint on access type", S);
         Desig_Subtype := Desig_Type; -- Ignore invalid constraint.
         Constraint_OK := False;
      end if;

      if No (Def_Id) then
         Def_Id := Create_Itype (E_Access_Subtype, Related_Nod);
      else
         Set_Ekind (Def_Id, E_Access_Subtype);
      end if;

      if Constraint_OK then
         Set_Etype (Def_Id, Base_Type (T));

         if Is_Private_Type (Desig_Type) then
            Prepare_Private_Subtype_Completion (Desig_Subtype, Related_Nod);
         end if;
      else
         Set_Etype (Def_Id, Any_Type);
      end if;

      Set_Size_Info                (Def_Id, T);
      Set_Is_Constrained           (Def_Id, Constraint_OK);
      Set_Directly_Designated_Type (Def_Id, Desig_Subtype);
      Set_Depends_On_Private       (Def_Id, Has_Private_Component (Def_Id));
      Set_Is_Access_Constant       (Def_Id, Is_Access_Constant (T));

      --  Itypes created for constrained record components do not receive
      --  a freeze node, they are elaborated when first seen.

      if not Is_Record_Type (Current_Scope) then
         Conditional_Delay (Def_Id, T);
      end if;
   end Constrain_Access;

   ---------------------
   -- Constrain_Array --
   ---------------------

   procedure Constrain_Array
     (Def_Id      : in out Entity_Id;
      SI          : Node_Id;
      Related_Nod : Node_Id;
      Related_Id  : Entity_Id;
      Suffix      : Character)
   is
      C                     : constant Node_Id := Constraint (SI);
      Number_Of_Constraints : Nat := 0;
      Index                 : Node_Id;
      S, T                  : Entity_Id;
      Constraint_OK         : Boolean := True;

   begin
      T := Entity (Subtype_Mark (SI));

      if Ekind (T) in Access_Kind then
         T := Designated_Type (T);
      end if;
      
      --  If an index constraint follows a subtype mark in a subtype indication
      --  then the type or subtype denoted by the subtype mark must not already
      --  impose an index constraint. The subtype mark must denote either an
      --  unconstrained array type or an access type whose designated type
      --  is such an array type... (RM 3.6.1)

      if Is_Constrained (T) then
         Error_Msg_N
           ("array type is already constrained", Subtype_Mark (SI));
         Constraint_OK := False;

      else
         S := First (Constraints (C));

         while Present (S) loop
            Number_Of_Constraints := Number_Of_Constraints + 1;
            Next (S);
         end loop;

         --  In either case, the index constraint must provide a discrete
         --  range for each index of the array type and the type of each
         --  discrete range must be the same as that of the corresponding
         --  index. (RM 3.6.1)

         if Number_Of_Constraints /= Number_Dimensions (T) then
            Error_Msg_NE ("incorrect number of index constraints for }", C, T);
            Constraint_OK := False;

         else
            S := First (Constraints (C));
            Index := First_Index (T);
            -- Analyze (Index);

            --  Apply constraints to each index type

            for J in 1 .. Number_Of_Constraints loop
	       -- Analyze (Index);
               Constrain_Index (Index, S, Related_Nod, Related_Id, Suffix, J);
               Next (Index);
               Next (S);
            end loop;

         end if;
      end if;

      if No (Def_Id) then
         Def_Id :=
           Create_Itype (E_Array_Subtype, Related_Nod, Related_Id, Suffix);
         Set_Parent (Def_Id, Related_Nod);

      else
         Set_Ekind (Def_Id, E_Array_Subtype);
      end if;

      Set_Size_Info      (Def_Id,                (T));
      Set_First_Rep_Item (Def_Id, First_Rep_Item (T));
      Set_Etype          (Def_Id, Base_Type      (T));

      if Constraint_OK then
         Set_First_Index (Def_Id, First (Constraints (C)));
      end if;

      Set_Is_Constrained     (Def_Id, True);
      Set_Is_Aliased         (Def_Id, Is_Aliased (T));
      Set_Depends_On_Private (Def_Id, Has_Private_Component (Def_Id));

      Set_Is_Private_Composite (Def_Id, Is_Private_Composite (T));

      --  If the subtype is not that of a record component, make sure
      --  that the Depends_On_Private status is set (explanation ???)
      --  and also that a conditional delay is set.

      if not Is_Type (Scope (Def_Id)) then
         Set_Depends_On_Private (Def_Id, Depends_On_Private (T));
         Conditional_Delay (Def_Id, T);
      end if;

   end Constrain_Array;

   ---------------------------
   -- Constrain_Enumeration --
   ---------------------------

   procedure Constrain_Enumeration (Def_Id : Node_Id; S : Node_Id) is
      T : constant Entity_Id := Entity (Subtype_Mark (S));
      C : constant Node_Id   := Constraint (S);

   begin
      Set_Ekind (Def_Id, E_Enumeration_Subtype);

      Set_First_Literal     (Def_Id, First_Literal (Base_Type (T)));

      Set_Etype             (Def_Id, Base_Type         (T));
      Set_Size_Info         (Def_Id,                   (T));
      Set_First_Rep_Item    (Def_Id, First_Rep_Item    (T));
      Set_Is_Character_Type (Def_Id, Is_Character_Type (T));

      Set_Scalar_Range_For_Subtype (Def_Id, Range_Expression (C), T);

      Set_Discrete_RM_Size (Def_Id);

   end Constrain_Enumeration;

   ----------------------
   -- Constrain_Float --
   ----------------------

   procedure Constrain_Float (Def_Id : Node_Id; S : Node_Id) is
      T : constant Entity_Id := Entity (Subtype_Mark (S));

   begin
      Set_Ekind (Def_Id, E_Floating_Point_Subtype);

      Set_Etype          (Def_Id, Base_Type      (T));
      Set_Size_Info      (Def_Id,                (T));
      Set_First_Rep_Item (Def_Id, First_Rep_Item (T));

      Set_Digits_Value (Def_Id, Digits_Value (T));

      Set_Scalar_Range (Def_Id, Scalar_Range (T));

      Set_Is_Constrained (Def_Id);
   end Constrain_Float;

   ---------------------
   -- Constrain_Index --
   ---------------------

   procedure Constrain_Index
     (Index        : Node_Id;
      S            : Node_Id;
      Related_Nod  : Node_Id;
      Related_Id   : Entity_Id;
      Suffix       : Character;
      Suffix_Index : Nat)
   is
      Def_Id     : Entity_Id;
      R          : Node_Id := Empty;
      Checks_Off : Boolean := False;
      T          : constant Entity_Id := Etype (Index);

   begin
      if Nkind (S) = N_Range
        or else
          (Nkind (S) = N_Attribute_Reference
            and then Attribute_Name (S) = Name_Range)
      then
         --  A Range attribute will transformed into N_Range by Resolve.

         Analyze (S);
         Set_Etype (S, T);
         R := S;

         --  ??? Why on earth do we turn checks of in this very specific case ?

         --  From the revision history: (Constrain_Index): Call
         --  Process_Range_Expr_In_Decl with range checking off for range
         --  bounds that are attributes. This avoids some horrible
         --  constraint error checks.

         if Nkind (R) = N_Range
           and then Nkind (Low_Bound (R)) = N_Attribute_Reference
           and then Nkind (High_Bound (R)) = N_Attribute_Reference
         then
            Checks_Off := True;
         end if;

         Process_Range_Expr_In_Decl (R, T, Empty_List, Checks_Off);

         if not Error_Posted (S)
           and then
             (Nkind (S) /= N_Range
               or else not Covers (T, (Etype (Low_Bound (S))))
               or else not Covers (T, (Etype (High_Bound (S)))))
         then
            if Base_Type (T) /= Any_Type
              and then Etype (Low_Bound (S)) /= Any_Type
              and then Etype (High_Bound (S)) /= Any_Type
            then
               Error_Msg_N ("range expected", S);
            end if;
         end if;

      elsif Nkind (S) = N_Subtype_Indication then
         --  the parser has verified that this is a discrete indication.

         Resolve_Discrete_Subtype_Indication (S, T);
         R := Range_Expression (Constraint (S));

      elsif Nkind (S) = N_Discriminant_Association then

         --  syntactically valid in subtype indication.

         Error_Msg_N ("invalid index constraint", S);
         Rewrite (S, New_Occurrence_Of (T, Sloc (S)));
         return;

      --  Subtype_Mark case, no anonymous subtypes to construct

      else
         Analyze (S);

         if Is_Entity_Name (S) then
            if not Is_Type (Entity (S)) then
               Error_Msg_N ("expect subtype mark for index constraint", S);

            elsif Base_Type (Entity (S)) /= Base_Type (T) then
               Wrong_Type (S, Base_Type (T));
            end if;

            return;

         else
            Error_Msg_N ("invalid index constraint", S);
            Rewrite (S, New_Occurrence_Of (T, Sloc (S)));
            return;
         end if;
      end if;

      Def_Id :=
        Create_Itype (E_Void, Related_Nod, Related_Id, Suffix, Suffix_Index);

      Set_Etype (Def_Id, Base_Type (T));

      if Is_Modular_Integer_Type (T) then
         Set_Ekind (Def_Id, E_Modular_Integer_Subtype);

      elsif Is_Integer_Type (T) then
         Set_Ekind (Def_Id, E_Signed_Integer_Subtype);

      else
         Set_Ekind (Def_Id, E_Enumeration_Subtype);
         Set_Is_Character_Type (Def_Id, Is_Character_Type (T));
      end if;

      Set_Size_Info      (Def_Id,                (T));
      Set_RM_Size        (Def_Id, RM_Size        (T));
      Set_First_Rep_Item (Def_Id, First_Rep_Item (T));

      Set_Scalar_Range   (Def_Id, R);

      Set_Etype (S, Def_Id);
      Set_Discrete_RM_Size (Def_Id);
   end Constrain_Index;

   -----------------------
   -- Constrain_Integer --
   -----------------------

   procedure Constrain_Integer (Def_Id : Node_Id; S : Node_Id) is
      T : constant Entity_Id := Entity (Subtype_Mark (S));
      C : constant Node_Id   := Constraint (S);
   begin
      Set_Scalar_Range_For_Subtype (Def_Id, Range_Expression (C), T);
      
      if Is_Modular_Integer_Type (T) then
         Set_Ekind (Def_Id, E_Modular_Integer_Subtype);
      else
         Set_Ekind (Def_Id, E_Signed_Integer_Subtype);
      end if;

      Set_Etype            (Def_Id, Base_Type        (T));
      Set_Size_Info        (Def_Id,                  (T));
      Set_First_Rep_Item   (Def_Id, First_Rep_Item   (T));
      Set_Discrete_RM_Size (Def_Id);

   end Constrain_Integer;

   ---------------------------
   -- Convert_Scalar_Bounds --
   ---------------------------

   procedure Convert_Scalar_Bounds
     (N            : Node_Id;
      Parent_Type  : Entity_Id;
      Derived_Type : Entity_Id;
      Loc          : Source_Ptr)
   is
      Implicit_Base : constant Entity_Id := Base_Type (Derived_Type);

      Lo  : Node_Id;
      Hi  : Node_Id;
      Rng : Node_Id;

   begin
      Lo := Build_Scalar_Bound
              (Type_Low_Bound (Derived_Type),
               Parent_Type, Implicit_Base);

      Hi := Build_Scalar_Bound
              (Type_High_Bound (Derived_Type),
               Parent_Type, Implicit_Base);

      Rng :=
        Make_Range (Loc,
          Low_Bound  => Lo,
          High_Bound => Hi);

      Set_Includes_Infinities (Rng, Has_Infinities (Derived_Type));

      Set_Parent (Rng, N);
      Set_Scalar_Range (Derived_Type, Rng);

      --  Analyze the bounds

      Analyze_And_Resolve (Lo, Implicit_Base);
      Analyze_And_Resolve (Hi, Implicit_Base);

      --  Analyze the range itself, except that we do not analyze it if
      --  the bounds are real literals, and we have a fixed-point type.
      --  The reason for this is that we delay setting the bounds in this
      --  case till we know the final Small and Size values (see circuit
      --  in Freeze.Freeze_Fixed_Point_Type for further details).

      
      --  Here we do the analysis of the range.

      --  Note: we do this manually, since if we do a normal Analyze and
      --  Resolve call, there are problems with the conversions used for
      --  the derived type range.

         Set_Etype    (Rng, Implicit_Base);
         Set_Analyzed (Rng, True);
      
   end Convert_Scalar_Bounds;

   -------------------
   -- Copy_And_Swap --
   -------------------

   procedure Copy_And_Swap (Priv, Full : Entity_Id) is

   begin
      --  Initialize new full declaration entity by copying the pertinent
      --  fields of the corresponding private declaration entity.

      --  We temporarily set Ekind to a value appropriate for a type to
      --  avoid assert failures in Einfo from checking for setting type
      --  attributes on something that is not a type. Ekind (Priv) is an
      --  appropriate choice, since it allowed the attributes to be set
      --  in the first place. This Ekind value will be modified later.

      Set_Ekind (Full, Ekind (Priv));

      --  Also set Etype temporarily to Any_Type, again, in the absence
      --  of errors, it will be properly reset, and if there are errors,
      --  then we want a value of Any_Type to remain.

      Set_Etype (Full, Any_Type);

      --  Now start copying attributes

      Set_First_Rep_Item             (Full, First_Rep_Item          (Priv));
      Set_Homonym                    (Full, Homonym                 (Priv));
      Set_Is_Immediately_Visible     (Full, Is_Immediately_Visible  (Priv));
      Set_Is_Public                  (Full, Is_Public               (Priv));
      Set_Is_Pure                    (Full, Is_Pure                 (Priv));
      Set_Is_Tagged_Type             (Full, Is_Tagged_Type          (Priv));

      Conditional_Delay              (Full,                          Priv);

      if Is_Tagged_Type (Full) then
         Set_Primitive_Operations    (Full, Primitive_Operations    (Priv));

         if Priv = Base_Type (Priv) then
            Set_Class_Wide_Type      (Full, Class_Wide_Type         (Priv));
         end if;
      end if;

      Set_Is_Volatile                (Full, Is_Volatile             (Priv));
      Set_Treat_As_Volatile          (Full, Treat_As_Volatile       (Priv));
      Set_Scope                      (Full, Scope                   (Priv));
      Set_Next_Entity                (Full, Next_Entity             (Priv));
      Set_First_Entity               (Full, First_Entity            (Priv));
      Set_Last_Entity                (Full, Last_Entity             (Priv));

      --  If access types have been recorded for later handling, keep them
      --  in the full view so that they get handled when the full view
      --  freeze node is expanded.

      if Present (Freeze_Node (Priv))
        and then Present (Access_Types_To_Process (Freeze_Node (Priv)))
      then
         Ensure_Freeze_Node (Full);
         Set_Access_Types_To_Process
           (Freeze_Node (Full),
            Access_Types_To_Process (Freeze_Node (Priv)));
      end if;

      --  Swap the two entities. Now Privat is the full type entity and
      --  Full is the private one. They will be swapped back at the end
      --  of the private part. This swapping ensures that the entity that
      --  is visible in the private part is the full declaration.

      Exchange_Entities (Priv, Full);
      Append_Entity (Full, Scope (Full));
   end Copy_And_Swap;

   -------------------------------------
   -- Copy_Array_Base_Type_Attributes --
   -------------------------------------

   procedure Copy_Array_Base_Type_Attributes (T1, T2 : Entity_Id) is
   begin
      Set_Component_Alignment      (T1, Component_Alignment      (T2));
      Set_Component_Type           (T1, Component_Type           (T2));
      Set_Component_Size           (T1, Component_Size           (T2));
      Set_Finalize_Storage_Only    (T1, Finalize_Storage_Only    (T2));
      Set_Has_Non_Standard_Rep     (T1, Has_Non_Standard_Rep     (T2));
      Set_Is_Packed                (T1, Is_Packed                (T2));
      Set_Has_Aliased_Components   (T1, Has_Aliased_Components   (T2));
      -- Set_Has_Atomic_Components    (T1, Has_Atomic_Components    (T2));
      Set_Has_Volatile_Components  (T1, Has_Volatile_Components  (T2));
   end Copy_Array_Base_Type_Attributes;

   -----------------------------------
   -- Copy_Array_Subtype_Attributes --
   -----------------------------------

   procedure Copy_Array_Subtype_Attributes (T1, T2 : Entity_Id) is
   begin
      Set_Size_Info (T1, T2);

      Set_First_Index          (T1, First_Index           (T2));
      Set_Is_Aliased           (T1, Is_Aliased            (T2));
--      Set_Is_Atomic            (T1, Is_Atomic             (T2));
      Set_Is_Volatile          (T1, Is_Volatile           (T2));
      Set_Treat_As_Volatile    (T1, Treat_As_Volatile     (T2));
      Set_Is_Constrained       (T1, Is_Constrained        (T2));
      Set_Depends_On_Private   (T1, Has_Private_Component (T2));
      Set_First_Rep_Item       (T1, First_Rep_Item        (T2));
      Set_Convention           (T1, Convention            (T2));
      Set_Is_Private_Composite (T1, Is_Private_Composite  (T2));
   end Copy_Array_Subtype_Attributes;

   -----------------------
   -- Derive_Subprogram --
   -----------------------

   procedure Derive_Subprogram
     (New_Subp     : in out Entity_Id;
      Parent_Subp  : Entity_Id;
      Derived_Type : Entity_Id;
      Parent_Type  : Entity_Id;
      Actual_Subp  : Entity_Id := Empty)
   is
      Formal     : Entity_Id;
      New_Formal : Entity_Id;
      Same_Subt  : constant Boolean :=
        Is_Scalar_Type (Parent_Type)
          and then Subtypes_Statically_Compatible (Parent_Type, Derived_Type);
      Visible_Subp : Entity_Id := Parent_Subp;

      function Is_Private_Overriding return Boolean;
      --  If Subp is a private overriding of a visible operation, the in-
      --  herited operation derives from the overridden op (even though
      --  its body is the overriding one) and the inherited operation is
      --  visible now. See sem_disp to see the details of the handling of
      --  the overridden subprogram, which is removed from the list of
      --  primitive operations of the type. The overridden subprogram is
      --  saved locally in Visible_Subp, and used to diagnose abstract
      --  operations that need overriding in the derived type.

      procedure Replace_Type (Id, New_Id : Entity_Id);
      --  When the type is an anonymous access type, create a new access type
      --  designating the derived type.

      procedure Set_Derived_Name;
      --  This procedure sets the appropriate Chars name for New_Subp. This
      --  is normally just a copy of the parent name. An exception arises for
      --  type support subprograms, where the name is changed to reflect the
      --  name of the derived type, e.g. if type foo is derived from type bar,
      --  then a procedure barDA is derived with a name fooDA.

      ---------------------------
      -- Is_Private_Overriding --
      ---------------------------

      function Is_Private_Overriding return Boolean is
         Prev : Entity_Id;

      begin
         Prev := Homonym (Parent_Subp);

         --  The visible operation that is overriden is a homonym of
         --  the parent subprogram. We scan the homonym chain to find
         --  the one whose alias is the subprogram we are deriving.

         while Present (Prev) loop
            if Is_Dispatching_Operation (Parent_Subp)
              and then Present (Prev)
              and then Ekind (Prev) = Ekind (Parent_Subp)
              and then Alias (Prev) = Parent_Subp
              and then Scope (Parent_Subp) = Scope (Prev)
              and then not Is_Hidden (Prev)
            then
               Visible_Subp := Prev;
               return True;
            end if;

            Prev := Homonym (Prev);
         end loop;

         return False;
      end Is_Private_Overriding;

      ------------------
      -- Replace_Type --
      ------------------

      procedure Replace_Type (Id, New_Id : Entity_Id) is
         Acc_Type : Entity_Id;

      begin
         --  When the type is an anonymous access type, create a new access
         --  type designating the derived type. This itype must be elaborated
         --  at the point of the derivation, not on subsequent calls that may
         --  be out of the proper scope for Gigi, so we insert a reference to
         --  it after the derivation.

         if Ekind (Etype (Id)) = E_Anonymous_Access_Type then
            declare
               Desig_Typ : Entity_Id := Designated_Type (Etype (Id));

            begin
               if Ekind (Desig_Typ) = E_Record_Type_With_Private
                 and then Present (Full_View (Desig_Typ))
                 and then not Is_Private_Type (Parent_Type)
               then
                  Desig_Typ := Full_View (Desig_Typ);
               end if;

               if Base_Type (Desig_Typ) = Base_Type (Parent_Type) then
                  Acc_Type := New_Copy (Etype (Id));
                  Set_Etype (Acc_Type, Acc_Type);
                  Set_Scope (Acc_Type, New_Subp);

                  --  Compute size of anonymous access type.

                  if Is_Array_Type (Desig_Typ)
                    and then not Is_Constrained (Desig_Typ)
                  then
                     Init_Size (Acc_Type, 2 * System_Address_Size);
                  else
                     Init_Size (Acc_Type, System_Address_Size);
                  end if;

                  Init_Alignment (Acc_Type);

                  Set_Directly_Designated_Type (Acc_Type, Derived_Type);

                  Set_Etype (New_Id, Acc_Type);
                  Set_Scope (New_Id, New_Subp);

                  --  Create a reference to it.

               else
                  Set_Etype (New_Id, Etype (Id));
               end if;
            end;
         elsif Base_Type (Etype (Id)) = Base_Type (Parent_Type)
           or else
             (Ekind (Etype (Id)) = E_Record_Type_With_Private
               and then Present (Full_View (Etype (Id)))
               and then Base_Type (Full_View (Etype (Id))) =
                 Base_Type (Parent_Type))
         then

            --  Constraint checks on formals are generated during expansion,
            --  based on the signature of the original subprogram. The bounds
            --  of the derived type are not relevant, and thus we can use
            --  the base type for the formals. However, the return type may be
            --  used in a context that requires that the proper static bounds
            --  be used (a case statement, for example)  and for those cases
            --  we must use the derived type (first subtype), not its base.

            if Etype (Id) = Parent_Type
              and then Same_Subt
            then
               Set_Etype (New_Id, Derived_Type);
            else
               Set_Etype (New_Id, Base_Type (Derived_Type));
            end if;

         else
            Set_Etype (New_Id, Etype (Id));
         end if;
      end Replace_Type;

      ----------------------
      -- Set_Derived_Name --
      ----------------------

      procedure Set_Derived_Name is
         -- Nm : constant TSS_Name_Type := Get_TSS_Name (Parent_Subp);
      begin
         --  if Nm = TSS_Null then
            Set_Chars (New_Subp, Chars (Parent_Subp));
         --  else
         --     Set_Chars (New_Subp, Make_TSS_Name (Base_Type (Derived_Type), Nm));
         --  end if;
      end Set_Derived_Name;

   --  Start of processing for Derive_Subprogram

   begin
      New_Subp :=
         New_Entity (Nkind (Parent_Subp), Sloc (Derived_Type));
      Set_Ekind (New_Subp, Ekind (Parent_Subp));

      --  Check whether the inherited subprogram is a private operation that
      --  should be inherited but not yet made visible. Such subprograms can
      --  become visible at a later point (e.g., the private part of a public
      --  child unit) via Declare_Inherited_Private_Subprograms. If the
      --  following predicate is true, then this is not such a private
      --  operation and the subprogram simply inherits the name of the parent
      --  subprogram. Note the special check for the names of controlled
      --  operations, which are currently exempted from being inherited with
      --  a hidden name because they must be findable for generation of
      --  implicit run-time calls.

      if not Is_Hidden (Parent_Subp)
        or else Is_Internal (Parent_Subp)
        or else Is_Private_Overriding
        or else Is_Internal_Name (Chars (Parent_Subp))
        or else Chars (Parent_Subp) = Name_Initialize
        or else Chars (Parent_Subp) = Name_Finalize
      then
         Set_Derived_Name;

      --  If parent is hidden, this can be a regular derivation if the
      --  parent is immediately visible in a non-instantiating context,
      --  or if we are in the private part of an instance. This test
      --  should still be refined ???

      --  The test for In_Instance_Not_Visible avoids inheriting the
      --  derived operation as a non-visible operation in cases where
      --  the parent subprogram might not be visible now, but was
      --  visible within the original generic, so it would be wrong
      --  to make the inherited subprogram non-visible now. (Not
      --  clear if this test is fully correct; are there any cases
      --  where we should declare the inherited operation as not
      --  visible to avoid it being overridden, e.g., when the
      --  parent type is a generic actual with private primitives ???)

      --  (they should be treated the same as other private inherited
      --  subprograms, but it's not clear how to do this cleanly). ???

      elsif (In_Open_Scopes (Scope (Base_Type (Parent_Type)))
              and then Is_Immediately_Visible (Parent_Subp)
              and then not In_Instance)
        or else In_Instance_Not_Visible
      then
         Set_Derived_Name;

      --  The type is inheriting a private operation, so enter
      --  it with a special name so it can't be overridden.

      else
         Set_Chars (New_Subp, New_External_Name (Chars (Parent_Subp), 'P'));
      end if;

      Set_Parent (New_Subp, Parent (Derived_Type));
      Replace_Type (Parent_Subp, New_Subp);
      Conditional_Delay (New_Subp, Parent_Subp);

      Formal := First_Formal (Parent_Subp);
      while Present (Formal) loop
         New_Formal := New_Copy (Formal);

         --  Normally we do not go copying parents, but in the case of
         --  formals, we need to link up to the declaration (which is
         --  the parameter specification), and it is fine to link up to
         --  the original formal's parameter specification in this case.

         Set_Parent (New_Formal, Parent (Formal));

         Append_Entity (New_Formal, New_Subp);

         Replace_Type (Formal, New_Formal);
         Next_Formal (Formal);
      end loop;

      --  If this derivation corresponds to a tagged generic actual, then
      --  primitive operations rename those of the actual. Otherwise the
      --  primitive operations rename those of the parent type, If the
      --  parent renames an intrinsic operator, so does the new subprogram.
      --  We except concatenation, which is always properly typed, and does
      --  not get expanded as other intrinsic operations.

      if No (Actual_Subp) then
         if Is_Intrinsic_Subprogram (Parent_Subp) then
            Set_Is_Intrinsic_Subprogram (New_Subp);

            if Present (Alias (Parent_Subp))
              and then Chars (Parent_Subp) /= Name_Op_Concat
            then
               Set_Alias (New_Subp, Alias (Parent_Subp));
            else
               Set_Alias (New_Subp, Parent_Subp);
            end if;

         else
            Set_Alias (New_Subp, Parent_Subp);
         end if;

      else
         Set_Alias (New_Subp, Actual_Subp);
      end if;

      --  Derived subprograms of a tagged type must inherit the convention
      --  of the parent subprogram (a requirement of AI-117). Derived
      --  subprograms of untagged types simply get convention Ada by default.

      if Is_Tagged_Type (Derived_Type) then
         Set_Convention  (New_Subp, Convention  (Parent_Subp));
      end if;

      Set_Is_Imported (New_Subp, Is_Imported (Parent_Subp));
      Set_Is_Exported (New_Subp, Is_Exported (Parent_Subp));

      if Ekind (Parent_Subp) = E_Procedure then
         Set_Is_Valued_Procedure
           (New_Subp, Is_Valued_Procedure (Parent_Subp));
      end if;

      --  A derived function with a controlling result is abstract.
      --  If the Derived_Type is a nonabstract formal generic derived
      --  type, then inherited operations are not abstract: check is
      --  done at instantiation time. If the derivation is for a generic
      --  actual, the function is not abstract unless the actual is.

      if Is_Generic_Type (Derived_Type)
        and then not Is_Abstract (Derived_Type)
      then
         null;

      elsif Is_Abstract (Alias (New_Subp))
        or else (Is_Tagged_Type (Derived_Type)
                   and then Etype (New_Subp) = Derived_Type
                   and then No (Actual_Subp))
      then
         Set_Is_Abstract (New_Subp);

      --  Finally, if the parent type is abstract  we must verify that all
      --  inherited operations are either non-abstract or overridden, or
      --  that the derived type itself is abstract (this check is performed
      --  at the end of a package declaration, in Check_Abstract_Overriding).
      --  A private overriding in the parent type will not be visible in the
      --  derivation if we are not in an inner package or in a child unit of
      --  the parent type, in which case the abstractness of the inherited
      --  operation is carried to the new subprogram.

      elsif Is_Abstract (Parent_Type)
        and then not In_Open_Scopes (Scope (Parent_Type))
        and then Is_Private_Overriding
        and then Is_Abstract (Visible_Subp)
      then
         Set_Alias (New_Subp, Visible_Subp);
         Set_Is_Abstract (New_Subp);
      end if;
      
      New_Overloaded_Entity (New_Subp, Derived_Type);

      --  Check for case of a derived subprogram for the instantiation
      --  of a formal derived tagged type, if so mark the subprogram as
      --  dispatching and inherit the dispatching attributes of the
      --  parent subprogram. The derived subprogram is effectively a
      --  renaming of the actual subprogram, so it needs to have the
      --  same attributes as the actual.

      if Present (Actual_Subp)
        and then Is_Dispatching_Operation (Parent_Subp)
      then
         Set_Is_Dispatching_Operation (New_Subp);
         if Present (DTC_Entity (Parent_Subp)) then
            Set_DTC_Entity (New_Subp, DTC_Entity (Parent_Subp));
            Set_DT_Position (New_Subp, DT_Position (Parent_Subp));
         end if;
      end if;

      --  Indicate that a derived subprogram does not require a body
      --  and that it does not require processing of default expressions.

      Set_Has_Completion (New_Subp);
      Set_Default_Expressions_Processed (New_Subp);

      if Ekind (New_Subp) = E_Function then
         Set_Mechanism (New_Subp, Mechanism (Parent_Subp));
      end if;
   end Derive_Subprogram;

   ------------------------
   -- Derive_Subprograms --
   ------------------------

   procedure Derive_Subprograms
     (Parent_Type    : Entity_Id;
      Derived_Type   : Entity_Id;
      Generic_Actual : Entity_Id := Empty)
   is
      Op_List     : constant Elist_Id :=
                      Collect_Primitive_Operations (Parent_Type);
      Act_List    : Elist_Id;
      Act_Elmt    : Elmt_Id;
      Elmt        : Elmt_Id;
      Subp        : Entity_Id;
      New_Subp    : Entity_Id := Empty;
      Parent_Base : Entity_Id;

   begin
      Parent_Base := Parent_Type;

      Elmt := First_Elmt (Op_List);

      if Present (Generic_Actual) then
         Act_List := Collect_Primitive_Operations (Generic_Actual);
         Act_Elmt := First_Elmt (Act_List);
      else
         Act_Elmt := No_Elmt;
      end if;

      --  Literals are derived earlier in the process of building the
      --  derived type, and are skipped here.

      while Present (Elmt) loop
         Subp := Node (Elmt);

         if Ekind (Subp) /= E_Enumeration_Literal then
            if No (Generic_Actual) then
               Derive_Subprogram
                 (New_Subp, Subp, Derived_Type, Parent_Base);

            else
               Derive_Subprogram (New_Subp, Subp,
                 Derived_Type, Parent_Base, Node (Act_Elmt));
               Next_Elmt (Act_Elmt);
            end if;
         end if;

         Next_Elmt (Elmt);
      end loop;
   end Derive_Subprograms;

   --------------------------------
   -- Derived_Standard_Character --
   --------------------------------

   procedure Derived_Standard_Character
     (N             : Node_Id;
      Parent_Type   : Entity_Id;
      Derived_Type  : Entity_Id)
   is
      Loc           : constant Source_Ptr := Sloc (N);
      Def           : constant Node_Id    := Type_Definition (N);
      Indic         : constant Node_Id    := Subtype_Indication (Def);
      Parent_Base   : constant Entity_Id  := Base_Type (Parent_Type);
      Implicit_Base : constant Entity_Id  :=
                        Create_Itype
                          (E_Enumeration_Type, N, Derived_Type, 'B');

      Lo : Node_Id;
      Hi : Node_Id;

   begin
      Discard_Node (Process_Subtype (Indic, N));

      Set_Etype     (Implicit_Base, Parent_Base);
      Set_Size_Info (Implicit_Base, Root_Type (Parent_Type));
      Set_RM_Size   (Implicit_Base, RM_Size (Root_Type (Parent_Type)));

      Set_Is_Character_Type  (Implicit_Base, True);
      Set_Has_Delayed_Freeze (Implicit_Base);

      --  The bounds of the implicit base are the bounds of the parent base.
      --  Note that their type is the parent base.

      Lo := New_Copy_Tree (Type_Low_Bound  (Parent_Base));
      Hi := New_Copy_Tree (Type_High_Bound (Parent_Base));

      Set_Scalar_Range (Implicit_Base,
        Make_Range (Loc,
          Low_Bound  => Lo,
          High_Bound => Hi));

      Conditional_Delay (Derived_Type, Parent_Type);

      Set_Ekind (Derived_Type, E_Enumeration_Subtype);
      Set_Etype (Derived_Type, Implicit_Base);
      Set_Size_Info         (Derived_Type, Parent_Type);

      if Unknown_RM_Size (Derived_Type) then
         Set_RM_Size (Derived_Type, RM_Size (Parent_Type));
      end if;

      Set_Is_Character_Type (Derived_Type, True);

      if Nkind (Indic) /= N_Subtype_Indication then

         --  If no explicit constraint, the bounds are those
         --  of the parent type.

         Lo := New_Copy_Tree (Type_Low_Bound  (Parent_Type));
         Hi := New_Copy_Tree (Type_High_Bound (Parent_Type));
         Set_Scalar_Range (Derived_Type, Make_Range (Loc, Lo, Hi));
      end if;

      Convert_Scalar_Bounds (N, Parent_Type, Derived_Type, Loc);

      --  Because the implicit base is used in the conversion of the bounds,
      --  we have to freeze it now. This is similar to what is done for
      --  numeric types, and it equally suspicious, but otherwise a non-
      --  static bound will have a reference to an unfrozen type, which is
      --  rejected by Gigi (???).

      Freeze_Before (N, Implicit_Base);
   end Derived_Standard_Character;

   ------------------------------
   -- Derived_Type_Declaration --
   ------------------------------

   procedure Derived_Type_Declaration
     (T             : Entity_Id;
      N             : Node_Id;
      Is_Completion : Boolean)
   is
      Def          : constant Node_Id := Type_Definition (N);
      Indic        : constant Node_Id := Subtype_Indication (Def);
      Extension    : constant Node_Id := Record_Extension_Part (Def);
      Parent_Type  : Entity_Id;
      Parent_Scope : Entity_Id;
      Taggd        : Boolean;

   begin
      Parent_Type := Find_Type_Of_Subtype_Indic (Indic);

      if Parent_Type = Any_Type
        or else Etype (Parent_Type) = Any_Type
        or else (Is_Class_Wide_Type (Parent_Type)
                  and then Etype (Parent_Type) = T)
      then
         --  If Parent_Type is undefined or illegal, make new type into
         --  a subtype of Any_Type, and set a few attributes to prevent
         --  cascaded errors. If this is a self-definition, emit error now.

         if T = Parent_Type
           or else T = Etype (Parent_Type)
         then
            Error_Msg_N ("type cannot be used in its own definition", Indic);
         end if;

         Set_Ekind        (T, Ekind (Parent_Type));
         Set_Etype        (T, Any_Type);
         Set_Scalar_Range (T, Scalar_Range (Any_Type));

         if Is_Tagged_Type (T) then
            Set_Primitive_Operations (T, New_Elmt_List);
         end if;

         return;
      end if;

      --  Check for early use of incomplete or private type

      if Ekind (Parent_Type) = E_Void
        or else Ekind (Parent_Type) = E_Incomplete_Type
      then
         Error_Msg_N ("premature derivation of incomplete type", Indic);
         return;

      elsif (Is_Incomplete_Or_Private_Type (Parent_Type)
              and then not Is_Generic_Type (Parent_Type)
              and then not Is_Generic_Type (Root_Type (Parent_Type))
              and then not Is_Generic_Actual_Type (Parent_Type))
        or else Has_Private_Component (Parent_Type)
      then
         --  The ancestor type of a formal type can be incomplete, in which
         --  case only the operations of the partial view are available in
         --  the generic. Subsequent checks may be required when the full
         --  view is analyzed, to verify that derivation from a tagged type
         --  has an extension.

         if Nkind (Original_Node (N)) = N_Formal_Type_Declaration then
            null;

         elsif No (Underlying_Type (Parent_Type))
           or else Has_Private_Component (Parent_Type)
         then
            Error_Msg_N
              ("premature derivation of derived or private type", Indic);

            --  Flag the type itself as being in error, this prevents some
            --  nasty problems with people looking at the malformed type.

            Set_Error_Posted (T);

         --  Check that within the immediate scope of an untagged partial
         --  view it's illegal to derive from the partial view if the
         --  full view is tagged. (7.3(7))

         --  We verify that the Parent_Type is a partial view by checking
         --  that it is not a Full_Type_Declaration (i.e. a private type or
         --  private extension declaration), to distinguish a partial view
         --  from  a derivation from a private type which also appears as
         --  E_Private_Type.

         elsif Present (Full_View (Parent_Type))
           and then Nkind (Parent (Parent_Type)) /= N_Full_Type_Declaration
           and then not Is_Tagged_Type (Parent_Type)
           and then Is_Tagged_Type (Full_View (Parent_Type))
         then
            Parent_Scope := Scope (T);
            while Present (Parent_Scope)
              and then Parent_Scope /= Standard_Standard
            loop
               if Parent_Scope = Scope (Parent_Type) then
                  Error_Msg_N
                    ("premature derivation from type with tagged full view",
                     Indic);
               end if;

               Parent_Scope := Scope (Parent_Scope);
            end loop;
         end if;
      end if;

      --  Check that form of derivation is appropriate

      Taggd := Is_Tagged_Type (Parent_Type);

      --  Perhaps the parent type should be changed to the class-wide type's
      --  specific type in this case to prevent cascading errors ???

      if Present (Extension) and then Is_Class_Wide_Type (Parent_Type) then
         Error_Msg_N ("parent type must not be a class-wide type", Indic);
         return;
      end if;

      if Present (Extension) and then not Taggd then
         Error_Msg_N
           ("type derived from untagged type cannot have extension", Indic);

      elsif No (Extension) and then Taggd then
         --  If this is within a private part (or body) of a generic
         --  instantiation then the derivation is allowed (the parent
         --  type can only appear tagged in this case if it's a generic
         --  actual type, since it would otherwise have been rejected
         --  in the analysis of the generic template).

         if not Is_Generic_Actual_Type (Parent_Type)
           or else In_Visible_Part (Scope (Parent_Type))
         then
            Error_Msg_N
              ("type derived from tagged type must have extension", Indic);
         end if;
      end if;

      Build_Derived_Type (N, Parent_Type, T, Is_Completion);
   end Derived_Type_Declaration;

   ----------------------------------
   -- Enumeration_Type_Declaration --
   ----------------------------------

   procedure Enumeration_Type_Declaration (T : Entity_Id; Def : Node_Id) is
      Ev     : Uint;
      L      : Node_Id;
      R_Node : Node_Id;
      B_Node : Node_Id;

   begin
      --  Create identifier node representing lower bound

      B_Node := New_Node (N_Identifier, Sloc (Def));
      L := First (Literals (Def));
      Set_Chars (B_Node, Chars (L));
      Set_Entity (B_Node,  L);
      Set_Etype (B_Node, T);
      Set_Is_Static_Expression (B_Node, True);

      R_Node := New_Node (N_Range, Sloc (Def));
      Set_Low_Bound  (R_Node, B_Node);

      Set_Ekind (T, E_Enumeration_Type);
      Set_First_Literal (T, L);
      Set_Etype (T, T);
      Set_Is_Constrained (T);

      Ev := Uint_0;

      --  Loop through literals of enumeration type setting pos and rep values
      --  except that if the Ekind is already set, then it means that the
      --  literal was already constructed (case of a derived type declaration
      --  and we should not disturb the Pos and Rep values.

      while Present (L) loop
         if Ekind (L) /= E_Enumeration_Literal then
            Set_Ekind (L, E_Enumeration_Literal);
            Set_Enumeration_Pos (L, Ev);
            Set_Enumeration_Rep (L, Ev);
            Set_Is_Known_Valid  (L, True);
         end if;

         Set_Etype (L, T);
         New_Overloaded_Entity (L);
         Generate_Definition (L);
         Set_Convention (L, Convention_Intrinsic);

         if Nkind (L) = N_Defining_Character_Literal then
            Set_Is_Character_Type (T, True);
         end if;

         Ev := Ev + 1;
         Next (L);
      end loop;

      --  Now create a node representing upper bound

      B_Node := New_Node (N_Identifier, Sloc (Def));
      Set_Chars (B_Node, Chars (Last (Literals (Def))));
      Set_Entity (B_Node,  Last (Literals (Def)));
      Set_Etype (B_Node, T);
      Set_Is_Static_Expression (B_Node, True);

      Set_High_Bound (R_Node, B_Node);
      Set_Scalar_Range (T, R_Node);
      Set_RM_Size (T, UI_From_Int (Minimum_Size (T)));
      Set_Enum_Esize (T);

      --  Set Discard_Names if configuration pragma set, or if there is
      --  a parameterless pragma in the current declarative region

      if Global_Discard_Names
        or else Discard_Names (Scope (T))
      then
         Set_Discard_Names (T);
      end if;

      --  Process end label if there is one

      if Present (Def) then
         Process_End_Label (Def, 'e', T);
      end if;
   end Enumeration_Type_Declaration;

   --------------------
   -- Find_Type_Name --
   --------------------

   function Find_Type_Name (N : Node_Id) return Entity_Id is
      Id       : constant Entity_Id := Defining_Identifier (N);
      Prev     : Entity_Id;
      New_Id   : Entity_Id;
      Prev_Par : Node_Id;

   begin
Put_Line ("Find_Type_Name Begin");
      --  Find incomplete declaration, if some was given.

      Prev := Current_Entity_In_Scope (Id);

      if Present (Prev) then

         --  Previous declaration exists. Error if not incomplete/private case
         --  except if previous declaration is implicit, etc. Enter_Name will
         --  emit error if appropriate.

         Prev_Par := Parent (Prev);

         if not Is_Incomplete_Or_Private_Type (Prev) then
            Enter_Name (Id);
            New_Id := Id;

         elsif Nkind (N) /= N_Full_Type_Declaration
         then
            --  Completion must be a full type declarations (RM 7.3(4))

            Error_Msg_Sloc := Sloc (Prev);
            Error_Msg_NE ("invalid completion of }", Id, Prev);

            --  Set scope of Id to avoid cascaded errors. Entity is never
            --  examined again, except when saving globals in generics.

            Set_Scope (Id, Current_Scope);
            New_Id := Id;

         --  Case of full declaration of incomplete type

         elsif Ekind (Prev) = E_Incomplete_Type then

            --  Indicate that the incomplete declaration has a matching
            --  full declaration. The defining occurrence of the incomplete
            --  declaration remains the visible one, and the procedure
            --  Get_Full_View dereferences it whenever the type is used.

            if Present (Full_View (Prev)) then
               Error_Msg_NE ("invalid redeclaration of }", Id, Prev);
            end if;

            Set_Full_View (Prev,  Id);
            Append_Entity (Id, Current_Scope);
            Set_Is_Public (Id, Is_Public (Prev));
            Set_Is_Internal (Id);
            New_Id := Prev;

         --  Case of full declaration of private type

         else
            if Nkind (Parent (Prev)) /= N_Private_Extension_Declaration then
               if Etype (Prev) /= Prev then

                  --  Prev is a private subtype or a derived type, and needs
                  --  no completion.

                  Error_Msg_NE ("invalid redeclaration of }", Id, Prev);
                  New_Id := Id;

               end if;

            elsif Nkind (N) /= N_Full_Type_Declaration
              or else Nkind (Type_Definition (N)) /= N_Derived_Type_Definition
            then
               Error_Msg_N ("full view of private extension must be"
                 & " an extension", N);

            elsif not (Abstract_Present (Parent (Prev)))
              and then Abstract_Present (Type_Definition (N))
            then
               Error_Msg_N ("full view of non-abstract extension cannot"
                 & " be abstract", N);
            end if;

            if not In_Private_Part (Current_Scope) then
               Error_Msg_N
                 ("declaration of full view must appear in private part",  N);
            end if;

            Copy_And_Swap (Prev, Id);
            Set_Has_Private_Declaration (Prev);
            Set_Has_Private_Declaration (Id);

            --  If no error, propagate freeze_node from private to full view.
            --  It may have been generated for an early operational item.

            if Present (Freeze_Node (Id))
              and then Serious_Errors_Detected = 0
              and then No (Full_View (Id))
            then
               Set_Freeze_Node (Prev, Freeze_Node (Id));
               Set_Freeze_Node (Id, Empty);
               Set_First_Rep_Item (Prev, First_Rep_Item (Id));
            end if;

            Set_Full_View (Id, Prev);
            New_Id := Prev;
         end if;

         --  A prior untagged private type can have an associated
         --  class-wide type due to use of the class attribute,
         --  and in this case also the full type is required to
         --  be tagged.

         if Is_Type (Prev)
           and then (Is_Tagged_Type (Prev)
                      or else Present (Class_Wide_Type (Prev)))
         then
            --  The full declaration is either a tagged record or an
            --  extension otherwise this is an error

            if Nkind (Type_Definition (N)) = N_Record_Definition then
               if not Tagged_Present (Type_Definition (N)) then
                  Error_Msg_NE
                    ("full declaration of } must be tagged", Prev, Id);
                  Set_Is_Tagged_Type (Id);
                  Set_Primitive_Operations (Id, New_Elmt_List);
               end if;

            elsif Nkind (Type_Definition (N)) = N_Derived_Type_Definition then
               if No (Record_Extension_Part (Type_Definition (N))) then
                  Error_Msg_NE (
                    "full declaration of } must be a record extension",
                    Prev, Id);
                  Set_Is_Tagged_Type (Id);
                  Set_Primitive_Operations (Id, New_Elmt_List);
               end if;

            else
               Error_Msg_NE
                 ("full declaration of } must be a tagged type", Prev, Id);

            end if;
         end if;

         return New_Id;

      else
         --  New type declaration
Put_Line ("Before Enter Name");
         Enter_Name (Id);
         return Id;
      end if;
   end Find_Type_Name;

   -------------------------
   -- Find_Type_Of_Object --
   -------------------------

   function Find_Type_Of_Object
     (Obj_Def     : Node_Id;
      Related_Nod : Node_Id) return Entity_Id
   is
      Def_Kind : constant Node_Kind := Nkind (Obj_Def);
      P        : Node_Id := Parent (Obj_Def);
      T        : Entity_Id;
      Nam      : Name_Id;

   begin
      --  If the parent is a component_definition node we climb to the
      --  component_declaration node

      if Nkind (P) = N_Component_Definition then
         P := Parent (P);
      end if;

      --  Case of an anonymous array subtype

      if Def_Kind = N_Constrained_Array_Definition
        or else Def_Kind = N_Unconstrained_Array_Definition
      then
         T := Empty;
         Array_Type_Declaration (T, Obj_Def);

      --  Create an explicit subtype whenever possible.

      elsif Nkind (P) /= N_Component_Declaration
        and then Def_Kind = N_Subtype_Indication
      then
         --  Base name of subtype on object name, which will be unique in
         --  the current scope.

         --  If this is a duplicate declaration, return base type, to avoid
         --  generating duplicate anonymous types.
	 
         if Error_Posted (P) then
            Analyze (Subtype_Mark (Obj_Def));
            return Entity (Subtype_Mark (Obj_Def));
         end if;
	 
	 --  JMA ??? The following code is never used ??? in Plc generation
	 --  as the genrators generate anonymous array or access 
	 
         Nam :=
            New_External_Name
             (Chars (Defining_Identifier (Related_Nod)), 'S', 0, 'T');
	 
         T := Make_Defining_Identifier (Sloc (P), Nam);

         Insert_Action
	   (Obj_Def,
	    Make_Subtype_Declaration 
	      (Sloc (P),
	       Defining_Identifier => T,
	       Subtype_Indication  => Relocate_Node (Obj_Def)));
	 
         --  This subtype may need freezing and it will not be done
         --  automatically if the object declaration is not in a
         --  declarative part. Since this is an object declaration, the
         --  type cannot always be frozen here. Deferred constants do not
         --  freeze their type (which often enough will be private).

         if Nkind (P) = N_Object_Declaration
           and then Constant_Present (P)
           and then No (Expression (P))
         then
            null;

         else
            Insert_Actions (Obj_Def, Freeze_Entity (T, Sloc (P)));
         end if;

      elsif Def_Kind = N_Access_Definition then
         T := Empty;
         T := Access_Definition (Related_Nod, Obj_Def);

      else
         T := Process_Subtype (Obj_Def, Related_Nod);
      end if;

      return T;
   end Find_Type_Of_Object;

   --------------------------------
   -- Find_Type_Of_Subtype_Indic --
   --------------------------------

   function Find_Type_Of_Subtype_Indic (S : Node_Id) return Entity_Id is
      Typ : Entity_Id;

   begin
      --  Case of subtype mark with a constraint

      if Nkind (S) = N_Subtype_Indication then
         Find_Type (Subtype_Mark (S));
         Typ := Entity (Subtype_Mark (S));

         if not
           Is_Valid_Constraint_Kind (Ekind (Typ), Nkind (Constraint (S)))
         then
            Error_Msg_N
              ("incorrect constraint for this kind of type", Constraint (S));
            Rewrite (S, New_Copy_Tree (Subtype_Mark (S)));
         end if;

      --  Otherwise we have a subtype mark without a constraint

      elsif Error_Posted (S) then
         Rewrite (S, New_Occurrence_Of (Any_Id, Sloc (S)));
         return Any_Type;

      else
         Find_Type (S);
         Typ := Entity (S);
      end if;

      if Typ = Standard_Wide_Character
        or else Typ = Standard_Wide_String
      then
         Check_Restriction (No_Wide_Characters, S);
      end if;

      return Typ;
   end Find_Type_Of_Subtype_Indic;

   -------------------------------------
   -- Floating_Point_Type_Declaration --
   -------------------------------------

   procedure Floating_Point_Type_Declaration (T : Entity_Id; Def : Node_Id) is
      Digs          : constant Node_Id := Digits_Expression (Def);
      Digs_Val      : Uint;
      Base_Typ      : Entity_Id;
      Implicit_Base : Entity_Id;
      Bound         : Node_Id;

      function Can_Derive_From (E : Entity_Id) return Boolean;
      --  Find if given digits value allows derivation from specified type

      ---------------------
      -- Can_Derive_From --
      ---------------------

      function Can_Derive_From (E : Entity_Id) return Boolean is
         Spec : constant Entity_Id := Real_Range_Specification (Def);

      begin
         if Digs_Val > Digits_Value (E) then
            return False;
         end if;

         if Present (Spec) then
            if Expr_Value_R (Type_Low_Bound (E)) >
               Expr_Value_R (Low_Bound (Spec))
            then
               return False;
            end if;

            if Expr_Value_R (Type_High_Bound (E)) <
               Expr_Value_R (High_Bound (Spec))
            then
               return False;
            end if;
         end if;

         return True;
      end Can_Derive_From;

   --  Start of processing for Floating_Point_Type_Declaration

   begin
      Check_Restriction (No_Floating_Point, Def);

      --  Create an implicit base type

      Implicit_Base :=
        Create_Itype (E_Floating_Point_Type, Parent (Def), T, 'B');

      --  Analyze and verify digits value

      Analyze_And_Resolve (Digs, Any_Integer);
      Digs_Val := Expr_Value (Digs);

      --  Process possible range spec and find correct type to derive from

      Process_Real_Range_Specification (Def);

      if Can_Derive_From (Standard_Short_Float) then
         Base_Typ := Standard_Short_Float;
      elsif Can_Derive_From (Standard_Float) then
         Base_Typ := Standard_Float;
      elsif Can_Derive_From (Standard_Long_Float) then
         Base_Typ := Standard_Long_Float;
      elsif Can_Derive_From (Standard_Long_Long_Float) then
         Base_Typ := Standard_Long_Long_Float;

      --  If we can't derive from any existing type, use long long float
      --  and give appropriate message explaining the problem.

      else
         Base_Typ := Standard_Long_Long_Float;

         if Digs_Val >= Digits_Value (Standard_Long_Long_Float) then
            Error_Msg_Uint_1 := Digits_Value (Standard_Long_Long_Float);
            Error_Msg_N ("digits value out of range, maximum is ^", Digs);

         else
            Error_Msg_N
              ("range too large for any predefined type",
               Real_Range_Specification (Def));
         end if;
      end if;

      --  If there are bounds given in the declaration use them as the bounds
      --  of the type, otherwise use the bounds of the predefined base type
      --  that was chosen based on the Digits value.

      if Present (Real_Range_Specification (Def)) then
         Set_Scalar_Range (T, Real_Range_Specification (Def));
         Set_Is_Constrained (T);

         --  The bounds of this range must be converted to machine numbers
         --  in accordance with RM 4.9(38).

         Bound := Type_Low_Bound (T);

         if Nkind (Bound) = N_Real_Literal then
            Set_Realval
              (Bound, Machine (Base_Typ, Realval (Bound), Round, Bound));
            Set_Is_Machine_Number (Bound);
         end if;

         Bound := Type_High_Bound (T);

         if Nkind (Bound) = N_Real_Literal then
            Set_Realval
              (Bound, Machine (Base_Typ, Realval (Bound), Round, Bound));
            Set_Is_Machine_Number (Bound);
         end if;

      else
         Set_Scalar_Range (T, Scalar_Range (Base_Typ));
      end if;

      --  Complete definition of implicit base and declared first subtype

      Set_Etype          (Implicit_Base, Base_Typ);

      Set_Scalar_Range   (Implicit_Base, Scalar_Range   (Base_Typ));
      Set_Size_Info      (Implicit_Base,                (Base_Typ));
      Set_RM_Size        (Implicit_Base, RM_Size        (Base_Typ));
      Set_First_Rep_Item (Implicit_Base, First_Rep_Item (Base_Typ));
      Set_Digits_Value   (Implicit_Base, Digits_Value   (Base_Typ));
      Set_Vax_Float      (Implicit_Base, Vax_Float      (Base_Typ));

      Set_Ekind          (T, E_Floating_Point_Subtype);
      Set_Etype          (T, Implicit_Base);

      Set_Size_Info      (T,                (Implicit_Base));
      Set_RM_Size        (T, RM_Size        (Implicit_Base));
      Set_First_Rep_Item (T, First_Rep_Item (Implicit_Base));
      Set_Digits_Value   (T, Digs_Val);

   end Floating_Point_Type_Declaration;

   --------------------------
   -- Has_Range_Constraint --
   --------------------------

   function Has_Range_Constraint (N : Node_Id) return Boolean is
      C : constant Node_Id := Constraint (N);

   begin
      if Nkind (C) = N_Range_Constraint then
         return True;

      elsif Nkind (C) = N_Digits_Constraint then
         return
            Present (Range_Constraint (C));

      else
         return False;
      end if;
   end Has_Range_Constraint;

   ------------------------
   -- Inherit_Components --
   ------------------------

   function Inherit_Components
     (N             : Node_Id;
      Parent_Base   : Entity_Id;
      Derived_Base  : Entity_Id;
      Is_Tagged     : Boolean) return Elist_Id
   is
      Assoc_List : constant Elist_Id := New_Elmt_List;

      procedure Inherit_Component (Old_C : Entity_Id);
      --  Inherits component Old_C from Parent_Base to the Derived_Base.
      --  If Plain_Discrim is True, Old_C is a discriminant.
      --  If Stored_Discrim is True, Old_C is a stored discriminant.
      --  If they are both false then Old_C is a regular component.

      -----------------------
      -- Inherit_Component --
      -----------------------

      procedure Inherit_Component (Old_C : Entity_Id)
      is
         New_C : constant Entity_Id := New_Copy (Old_C);


      begin
         pragma Assert (not Is_Tagged);

         Set_Parent (New_C, Parent (Old_C));

         --  Regular discriminants and components must be inserted
         --  in the scope of the Derived_Base. Do it here.

	 Enter_Name (New_C);

         --  For tagged types the Original_Record_Component must point to
         --  whatever this field was pointing to in the parent type. This has
         --  already been achieved by the call to New_Copy above.

         if not Is_Tagged then
            Set_Original_Record_Component (New_C, New_C);
         end if;

         --  If we have inherited a component then see if its Etype contains
         --  references to Parent_Base discriminants. In this case, replace
         --  these references with the constraints given in Discs. We do not
         --  do this for the partial view of private types because this is
         --  not needed (only the components of the full view will be used
         --  for code generation) and cause problem. We also avoid this
         --  transformation in some error situations.

         if Ekind (New_C) = E_Component then
	    Set_Etype (New_C, Etype (Old_C));
         end if;

         --  In derived tagged types it is illegal to reference a non
         --  discriminant component in the parent type. To catch this, mark
         --  these components with an Ekind of E_Void. This will be reset in
         --  Record_Type_Definition after processing the record extension of
         --  the derived type.

         if Is_Tagged and then Ekind (New_C) = E_Component then
            Set_Ekind (New_C, E_Void);
         end if;

         if not Is_Tagged then
            Append_Elmt (Old_C, Assoc_List);
            Append_Elmt (New_C, Assoc_List);
         end if;
      end Inherit_Component;

      --  Variables local to Inherit_Components.

      Loc : constant Source_Ptr := Sloc (N);

      Component      : Entity_Id;

   --  Start of processing for Inherit_Components

   begin
      if not Is_Tagged then
         Append_Elmt (Parent_Base,  Assoc_List);
         Append_Elmt (Derived_Base, Assoc_List);
      end if;

      --  Finally, inherit non-discriminant components unless they are not
      --  visible because defined or inherited from the full view of the
      --  parent. Don't inherit the _parent field of the parent type.

      Component := First_Entity (Parent_Base);
      while Present (Component) loop
         if Ekind (Component) /= E_Component
           or else Chars (Component) = Name_uParent
         then
            null;

         --  If the derived type is within the parent type's declarative
         --  region, then the components can still be inherited even though
         --  they aren't visible at this point. This can occur for cases
         --  such as within public child units where the components must
         --  become visible upon entering the child unit's private part.

         elsif not Is_Visible_Component (Component)
           and then not In_Open_Scopes (Scope (Parent_Base))
         then
            null;

         elsif Ekind (Derived_Base) = E_Private_Type
         then
            null;

         else
            Inherit_Component (Component);
         end if;

         Next_Entity (Component);
      end loop;

      return Assoc_List;
   end Inherit_Components;

   ------------------------------
   -- Is_Valid_Constraint_Kind --
   ------------------------------

   function Is_Valid_Constraint_Kind
     (T_Kind          : Type_Kind;
      Constraint_Kind : Node_Kind) return Boolean
   is
   begin
      case T_Kind is

         when Enumeration_Kind |
              Integer_Kind =>
            return Constraint_Kind = N_Range_Constraint;

         when Float_Kind =>
            return
              Constraint_Kind = N_Digits_Constraint
                or else
              Constraint_Kind = N_Range_Constraint;

         when Access_Kind       |
              Array_Kind        |
              E_Record_Type     |
              E_Record_Subtype  |
              Class_Wide_Kind   |
              E_Incomplete_Type |
              Private_Kind      =>
            return Constraint_Kind = N_Index_Or_Discriminant_Constraint;

         when others =>
            return True; -- Error will be detected later.
      end case;

   end Is_Valid_Constraint_Kind;

   --------------------------
   -- Is_Visible_Component --
   --------------------------

   function Is_Visible_Component (C : Entity_Id) return Boolean is
      Original_Comp  : Entity_Id := Empty;
      Original_Scope : Entity_Id;
      Type_Scope     : Entity_Id;

      function Is_Local_Type (Typ : Entity_Id) return Boolean;
      --  Check whether parent type of inherited component is declared
      --  locally, possibly within a nested package or instance. The
      --  current scope is the derived record itself.

      -------------------
      -- Is_Local_Type --
      -------------------

      function Is_Local_Type (Typ : Entity_Id) return Boolean is
         Scop : Entity_Id := Scope (Typ);

      begin
         while Present (Scop)
           and then Scop /= Standard_Standard
         loop
            if Scop = Scope (Current_Scope) then
               return True;
            end if;

            Scop := Scope (Scop);
         end loop;
         return False;
      end Is_Local_Type;

   --  Start of processing for Is_Visible_Component

   begin
      if Ekind (C) = E_Component
      then
         Original_Comp := Original_Record_Component (C);
      end if;

      if No (Original_Comp) then

         --  Premature usage, or previous error

         return False;

      else
         Original_Scope := Scope (Original_Comp);
         Type_Scope     := Scope (Base_Type (Scope (C)));
      end if;

      --  This test only concerns tagged types

      if not Is_Tagged_Type (Original_Scope) then
         return True;

      --  If it is _Parent or _Tag, there is no visibility issue

      elsif not Comes_From_Source (Original_Comp) then
         return True;

      --  If we are in the body of an instantiation, the component is
      --  visible even when the parent type (possibly defined in an
      --  enclosing unit or in a parent unit) might not.

      elsif In_Instance_Body then
         return True;

      --  If the component has been declared in an ancestor which is
      --  currently a private type, then it is not visible. The same
      --  applies if the component's containing type is not in an
      --  open scope and the original component's enclosing type
      --  is a visible full type of a private type (which can occur
      --  in cases where an attempt is being made to reference a
      --  component in a sibling package that is inherited from a
      --  visible component of a type in an ancestor package; the
      --  component in the sibling package should not be visible
      --  even though the component it inherited from is visible).
      --  This does not apply however in the case where the scope
      --  of the type is a private child unit, or when the parent
      --  comes from a local package in which the ancestor is
      --  currently visible. The latter suppression of visibility
      --  is needed for cases that are tested in B730006.

      elsif Is_Private_Type (Original_Scope)
        or else
          (not Is_Private_Descendant (Type_Scope)
            and then not In_Open_Scopes (Type_Scope)
            and then Has_Private_Declaration (Original_Scope))
      then
         --  If the type derives from an entity in a formal package, there
         --  are no additional visible components.

         if Nkind (Original_Node (Unit_Declaration_Node (Type_Scope))) =
            N_Formal_Package_Declaration
         then
            return False;

         --  if we are not in the private part of the current package, there
         --  are no additional visible components.

         elsif Ekind (Scope (Current_Scope)) = E_Package
           and then not In_Private_Part (Scope (Current_Scope))
         then
            return False;
         else
            return
              Is_Child_Unit (Cunit_Entity (Current_Sem_Unit))
                and then Is_Local_Type (Type_Scope);
         end if;

      --  There is another weird way in which a component may be invisible
      --  when the private and the full view are not derived from the same
      --  ancestor. Here is an example :

      --       type A1 is tagged      record F1 : integer; end record;
      --       type A2 is new A1 with record F2 : integer; end record;
      --       type T is new A1 with private;
      --     private
      --       type T is new A2 with null record;

      --  In this case, the full view of T inherits F1 and F2 but the
      --  private view inherits only F1

      else
         declare
            Ancestor : Entity_Id := Scope (C);

         begin
            loop
               if Ancestor = Original_Scope then
                  return True;
               elsif Ancestor = Etype (Ancestor) then
                  return False;
               end if;

               Ancestor := Etype (Ancestor);
            end loop;
         end;
      end if;
   end Is_Visible_Component;

   --------------------------
   -- Make_Class_Wide_Type --
   --------------------------

   procedure Make_Class_Wide_Type (T : Entity_Id) is
      CW_Type : Entity_Id;
      CW_Name : Name_Id;
      Next_E  : Entity_Id;

   begin
      --  The class wide type can have been defined by the partial view in
      --  which case everything is already done

      if Present (Class_Wide_Type (T)) then
         return;
      end if;

      CW_Type :=
        New_External_Entity (E_Void, Scope (T), Sloc (T), T, 'C', 0, 'T');

      --  Inherit root type characteristics

      CW_Name := Chars (CW_Type);
      Next_E  := Next_Entity (CW_Type);
      Copy_Node (T, CW_Type);
      Set_Comes_From_Source (CW_Type, False);
      Set_Chars (CW_Type, CW_Name);
      Set_Parent (CW_Type, Parent (T));
      Set_Next_Entity (CW_Type, Next_E);
      Set_Has_Delayed_Freeze (CW_Type);

      --  Customize the class-wide type: It has no prim. op., it cannot be
      --  abstract and its Etype points back to the specific root type.

      Set_Ekind                (CW_Type, E_Class_Wide_Type);
      Set_Is_Tagged_Type       (CW_Type, True);
      Set_Primitive_Operations (CW_Type, New_Elmt_List);
      Set_Is_Abstract          (CW_Type, False);
      Set_Is_Constrained       (CW_Type, False);
      Set_Is_First_Subtype     (CW_Type, Is_First_Subtype (T));
      Init_Size_Align          (CW_Type);

      if Ekind (T) = E_Class_Wide_Subtype then
         Set_Etype             (CW_Type, Etype (Base_Type (T)));
      else
         Set_Etype             (CW_Type, T);
      end if;

      --  If this is the class_wide type of a constrained subtype, it does
      --  not have discriminants.

      Set_Class_Wide_Type (T, CW_Type);
      Set_Equivalent_Type (CW_Type, Empty);

      --  The class-wide type of a class-wide type is itself (RM 3.9(14))

      Set_Class_Wide_Type (CW_Type, CW_Type);

   end Make_Class_Wide_Type;

   ----------------
   -- Make_Index --
   ----------------

   procedure Make_Index
     (I            : Node_Id;
      Related_Nod  : Node_Id;
      Related_Id   : Entity_Id := Empty;
      Suffix_Index : Nat := 1)
   is
      R      : Node_Id;
      T      : Entity_Id;
      Def_Id : Entity_Id := Empty;
      Found  : Boolean := False;

   begin
      --  For a discrete range used in a constrained array definition and
      --  defined by a range, an implicit conversion to the predefined type
      --  INTEGER is assumed if each bound is either a numeric literal, a named
      --  number, or an attribute, and the type of both bounds (prior to the
      --  implicit conversion) is the type universal_integer. Otherwise, both
      --  bounds must be of the same discrete type, other than universal
      --  integer; this type must be determinable independently of the
      --  context, but using the fact that the type must be discrete and that
      --  both bounds must have the same type.

      --  Character literals also have a universal type in the absence of
      --  of additional context,  and are resolved to Standard_Character.

      if Nkind (I) = N_Range then

         --  The index is given by a range constraint. The bounds are known
         --  to be of a consistent type.

         if not Is_Overloaded (I) then
            T := Etype (I);

            --  If the bounds are universal, choose the specific predefined
            --  type.

            if T = Universal_Integer then
               T := Standard_Integer;

            elsif T = Any_Character then

               if not Ada_83 then
                  Error_Msg_N
                    ("ambiguous character literals (could be Wide_Character)",
                      I);
               end if;

               T := Standard_Character;
            end if;

         else
            T := Any_Type;

            declare
               Ind : Interp_Index;
               It  : Interp;

            begin
               Get_First_Interp (I, Ind, It);

               while Present (It.Typ) loop
                  if Is_Discrete_Type (It.Typ) then

                     if Found
                       and then not Covers (It.Typ, T)
                       and then not Covers (T, It.Typ)
                     then
                        Error_Msg_N ("ambiguous bounds in discrete range", I);
                        exit;
                     else
                        T := It.Typ;
                        Found := True;
                     end if;
                  end if;

                  Get_Next_Interp (Ind, It);
               end loop;

               if T = Any_Type then
                  Error_Msg_N ("discrete type required for range", I);
                  Set_Etype (I, Any_Type);
                  return;

               elsif T = Universal_Integer then
                  T := Standard_Integer;
               end if;
            end;
         end if;

         if not Is_Discrete_Type (T) then
            Error_Msg_N ("discrete type required for range", I);
            Set_Etype (I, Any_Type);
            return;
         end if;

         if Nkind (Low_Bound (I)) = N_Attribute_Reference
           and then Attribute_Name (Low_Bound (I)) = Name_First
           and then Is_Entity_Name (Prefix (Low_Bound (I)))
           and then Is_Type (Entity (Prefix (Low_Bound (I))))
           and then Is_Discrete_Type (Entity (Prefix (Low_Bound (I))))
         then
            --  The type of the index will be the type of the prefix,
            --  as long as the upper bound is 'Last of the same type.

            Def_Id := Entity (Prefix (Low_Bound (I)));

            if Nkind (High_Bound (I)) /= N_Attribute_Reference
              or else Attribute_Name (High_Bound (I)) /= Name_Last
              or else not Is_Entity_Name (Prefix (High_Bound (I)))
              or else Entity (Prefix (High_Bound (I))) /= Def_Id
            then
               Def_Id := Empty;
            end if;
         end if;

         R := I;
         Process_Range_Expr_In_Decl (R, T);

      elsif Nkind (I) = N_Subtype_Indication then

         --  The index is given by a subtype with a range constraint.

         T :=  Base_Type (Entity (Subtype_Mark (I)));
	 
         if not Is_Discrete_Type (T) then
            Error_Msg_N ("discrete type required for range", I);
            Set_Etype (I, Any_Type);
            return;
         end if;

         R := Range_Expression (Constraint (I));

         Resolve (R, T);
         Process_Range_Expr_In_Decl (R, Entity (Subtype_Mark (I)));

      elsif Nkind (I) = N_Attribute_Reference then

         --  The parser guarantees that the attribute is a RANGE attribute

         --  If the node denotes the range of a type mark, that is also the
         --  resulting type, and we do no need to create an Itype for it.

         if Is_Entity_Name (Prefix (I))
           and then Comes_From_Source (I)
           and then Is_Type (Entity (Prefix (I)))
           and then Is_Discrete_Type (Entity (Prefix (I)))
         then
            Def_Id := Entity (Prefix (I));
         end if;

         Analyze_And_Resolve (I);
         T := Etype (I);
         R := I;

      --  If none of the above, must be a subtype. We convert this to a
      --  range attribute reference because in the case of declared first
      --  named subtypes, the types in the range reference can be different
      --  from the type of the entity. A range attribute normalizes the
      --  reference and obtains the correct types for the bounds.

      --  This transformation is in the nature of an expansion, is only
      --  done if expansion is active. In particular, it is not done on
      --  formal generic types,  because we need to retain the name of the
      --  original index for instantiation purposes.

      else
         if not Is_Entity_Name (I) or else not Is_Type (Entity (I)) then
            Error_Msg_N ("invalid subtype mark in discrete range ", I);
            Set_Etype (I, Any_Integer);
            return;
         else
            --  The type mark may be that of an incomplete type. It is only
            --  now that we can get the full view, previous analysis does
            --  not look specifically for a type mark.
	    
            Set_Entity (I, Get_Full_View (Entity (I)));
            Set_Etype  (I, Entity (I));
            Def_Id := Entity (I);
            if not Is_Discrete_Type (Def_Id) then
               Error_Msg_N ("discrete type required for index", I);
               Set_Etype (I, Any_Type);
               return;
            end if;
         end if;
	 
	 Rewrite
	   (I,
	    Make_Attribute_Reference
	      (Sloc (I),
	       Attribute_Name => Name_Range,
	       Prefix         => Relocate_Node (I)));
	 
	 --  The original was a subtype mark that does not freeze. This
	 --  means that the rewritten version must not freeze either.
	 
	 Set_Must_Not_Freeze (I);
	 Set_Must_Not_Freeze (Prefix (I));
	 
	 --  Is order critical??? if so, document why, if not
	 --  use Analyze_And_Resolve
	 
	 Analyze (I);
	 T := Etype (I);
	 Resolve (I);
	 R := I;
      end if;

      if not Is_Discrete_Type (T) then
         Error_Msg_N ("discrete type required for range", I);
         Set_Etype (I, Any_Type);
         return;

      elsif T = Any_Type then
         Set_Etype (I, Any_Type);
         return;
      end if;

      --  We will now create the appropriate Itype to describe the
      --  range, but first a check. If we originally had a subtype,
      --  then we just label the range with this subtype. Not only
      --  is there no need to construct a new subtype, but it is wrong
      --  to do so for two reasons:

      --    1. A legality concern, if we have a subtype, it must not
      --       freeze, and the Itype would cause freezing incorrectly

      --    2. An efficiency concern, if we created an Itype, it would
      --       not be recognized as the same type for the purposes of
      --       eliminating checks in some circumstances.

      --  We signal this case by setting the subtype entity in Def_Id.

      if No (Def_Id) then
         Def_Id :=
           Create_Itype (E_Void, Related_Nod, Related_Id, 'D', Suffix_Index);
         Set_Etype (Def_Id, Base_Type (T));

         if Is_Signed_Integer_Type (T) then
            Set_Ekind (Def_Id, E_Signed_Integer_Subtype);

         elsif Is_Modular_Integer_Type (T) then
            Set_Ekind (Def_Id, E_Modular_Integer_Subtype);

         else
            Set_Ekind             (Def_Id, E_Enumeration_Subtype);
            Set_Is_Character_Type (Def_Id, Is_Character_Type (T));
            Set_First_Literal     (Def_Id, First_Literal (T));
         end if;

         Set_Size_Info      (Def_Id,                  (T));
         Set_RM_Size        (Def_Id, RM_Size          (T));
         Set_First_Rep_Item (Def_Id, First_Rep_Item   (T));

         Set_Scalar_Range   (Def_Id, R);
         Conditional_Delay  (Def_Id, T);

         --  In the subtype indication case, if the immediate parent of the
         --  new subtype is non-static, then the subtype we create is non-
         --  static, even if its bounds are static.

         if Nkind (I) = N_Subtype_Indication
           and then not Is_Static_Subtype (Entity (Subtype_Mark (I)))
         then
            Set_Is_Non_Static_Subtype (Def_Id);
         end if;
      end if;

      --  Final step is to label the index with this constructed type

      Set_Etype (I, Def_Id);
      
   end Make_Index;

   ------------------------------
   -- Modular_Type_Declaration --
   ------------------------------

   procedure Modular_Type_Declaration (T : Entity_Id; Def : Node_Id) is
      Mod_Expr : constant Node_Id := Expression (Def);
      M_Val    : Uint;

      procedure Set_Modular_Size (Bits : Int);
      --  Sets RM_Size to Bits, and Esize to normal word size above this

      ----------------------
      -- Set_Modular_Size --
      ----------------------

      procedure Set_Modular_Size (Bits : Int) is
      begin
         Set_RM_Size (T, UI_From_Int (Bits));

         if Bits <= 8 then
            Init_Esize (T, 8);

         elsif Bits <= 16 then
            Init_Esize (T, 16);

         elsif Bits <= 32 then
            Init_Esize (T, 32);

         else
            Init_Esize (T, System_Max_Binary_Modulus_Power);
         end if;
      end Set_Modular_Size;

   --  Start of processing for Modular_Type_Declaration

   begin
      Analyze_And_Resolve (Mod_Expr, Any_Integer);
      Set_Etype (T, T);
      Set_Ekind (T, E_Modular_Integer_Type);
      Init_Alignment (T);
      Set_Is_Constrained (T);

      if not Is_OK_Static_Expression (Mod_Expr) then
         Flag_Non_Static_Expr
           ("non-static expression used for modular type bound!", Mod_Expr);
         M_Val := 2 ** System_Max_Binary_Modulus_Power;
      else
         M_Val := Expr_Value (Mod_Expr);
      end if;

      if M_Val < 1 then
         Error_Msg_N ("modulus value must be positive", Mod_Expr);
         M_Val := 2 ** System_Max_Binary_Modulus_Power;
      end if;

      Set_Modulus (T, M_Val);

      --   Create bounds for the modular type based on the modulus given in
      --   the type declaration and then analyze and resolve those bounds.

      Set_Scalar_Range (T,
        Make_Range (Sloc (Mod_Expr),
          Low_Bound  =>
            Make_Integer_Literal (Sloc (Mod_Expr), 0),
          High_Bound =>
            Make_Integer_Literal (Sloc (Mod_Expr), M_Val - 1)));

      --  Properly analyze the literals for the range. We do this manually
      --  because we can't go calling Resolve, since we are resolving these
      --  bounds with the type, and this type is certainly not complete yet!

      Set_Etype (Low_Bound  (Scalar_Range (T)), T);
      Set_Etype (High_Bound (Scalar_Range (T)), T);
      Set_Is_Static_Expression (Low_Bound  (Scalar_Range (T)));
      Set_Is_Static_Expression (High_Bound (Scalar_Range (T)));

      --  Loop through powers of two to find number of bits required

      for Bits in Int range 0 .. System_Max_Binary_Modulus_Power loop

         --  Binary case

         if M_Val = 2 ** Bits then
            Set_Modular_Size (Bits);
            return;

         --  Non-binary case

         elsif M_Val < 2 ** Bits then
            Set_Non_Binary_Modulus (T);

            if Bits > System_Max_Nonbinary_Modulus_Power then
               Error_Msg_Uint_1 :=
                 UI_From_Int (System_Max_Nonbinary_Modulus_Power);
               Error_Msg_N
                 ("nonbinary modulus exceeds limit (2 '*'*^ - 1)", Mod_Expr);
               Set_Modular_Size (System_Max_Binary_Modulus_Power);
               return;

            else
               --  In the non-binary case, set size as per RM 13.3(55).

               Set_Modular_Size (Bits);
               return;
            end if;
         end if;

      end loop;

      --  If we fall through, then the size exceed System.Max_Binary_Modulus
      --  so we just signal an error and set the maximum size.

      Error_Msg_Uint_1 := UI_From_Int (System_Max_Binary_Modulus_Power);
      Error_Msg_N ("modulus exceeds limit (2 '*'*^)", Mod_Expr);

      Set_Modular_Size (System_Max_Binary_Modulus_Power);
      Init_Alignment (T);

   end Modular_Type_Declaration;

   --------------------------
   -- New_Concatenation_Op --
   --------------------------

   procedure New_Concatenation_Op (Typ : Entity_Id) is
      Loc : constant Source_Ptr := Sloc (Typ);
      Op  : Entity_Id;

      function Make_Op_Formal (Typ, Op : Entity_Id) return Entity_Id;
      --  Create abbreviated declaration for the formal of a predefined
      --  Operator 'Op' of type 'Typ'

      --------------------
      -- Make_Op_Formal --
      --------------------

      function Make_Op_Formal (Typ, Op : Entity_Id) return Entity_Id is
         Formal : Entity_Id;

      begin
         Formal := New_Internal_Entity (E_In_Parameter, Op, Loc, 'P');
         Set_Etype (Formal, Typ);
         Set_Mechanism (Formal, Default_Mechanism);
         return Formal;
      end Make_Op_Formal;

   --  Start of processing for New_Concatenation_Op

   begin
      Op := Make_Defining_Operator_Symbol (Loc, Name_Op_Concat);

      Set_Ekind                   (Op, E_Operator);
      Set_Scope                   (Op, Current_Scope);
      Set_Etype                   (Op, Typ);
      Set_Homonym                 (Op, Get_Name_Entity_Id (Name_Op_Concat));
      Set_Is_Immediately_Visible  (Op);
      Set_Is_Intrinsic_Subprogram (Op);
      Set_Has_Completion          (Op);
      Append_Entity               (Op, Current_Scope);

      Set_Name_Entity_Id (Name_Op_Concat, Op);

      Append_Entity (Make_Op_Formal (Typ, Op), Op);
      Append_Entity (Make_Op_Formal (Typ, Op), Op);

   end New_Concatenation_Op;

   ----------------------------------------
   -- Prepare_Private_Subtype_Completion --
   ----------------------------------------

   procedure Prepare_Private_Subtype_Completion
     (Id          : Entity_Id;
      Related_Nod : Node_Id)
   is
      Id_B   : constant Entity_Id := Base_Type (Id);
      Full_B : constant Entity_Id := Full_View (Id_B);
      Full   : Entity_Id;

   begin
      if Present (Full_B) then

         --  The Base_Type is already completed, we can complete the
         --  subtype now. We have to create a new entity with the same name,
         --  Thus we can't use Create_Itype.
         --  This is messy, should be fixed ???

         Full := Make_Defining_Identifier (Sloc (Id), Chars (Id));
         Set_Is_Itype (Full);
         Set_Associated_Node_For_Itype (Full, Related_Nod);
         Complete_Private_Subtype (Id, Full, Full_B, Related_Nod);
      end if;

      --  The parent subtype may be private, but the base might not, in some
      --  nested instances. In that case, the subtype does not need to be
      --  exchanged. It would still be nice to make private subtypes and their
      --  bases consistent at all times ???

      if Is_Private_Type (Id_B) then
         Append_Elmt (Id, Private_Dependents (Id_B));
      end if;

   end Prepare_Private_Subtype_Completion;

   -----------------------
   -- Process_Full_View --
   -----------------------

   procedure Process_Full_View (N : Node_Id; Full_T, Priv_T : Entity_Id) is
      Priv_Parent : Entity_Id;
      Full_Parent : Entity_Id;
      Full_Indic  : Node_Id;

   begin
      --  First some sanity checks that must be done after semantic
      --  decoration of the full view and thus cannot be placed with other
      --  similar checks in Find_Type_Name

      if Is_Abstract (Full_T) and then not Is_Abstract (Priv_T) then
         Error_Msg_N
           ("completion of nonabstract type cannot be abstract", Full_T);

      elsif Is_Generic_Type (Priv_T) then
         Error_Msg_N ("generic type cannot have a completion", Full_T);
      end if;

      if Is_Tagged_Type (Priv_T)
        and then Nkind (Parent (Priv_T)) = N_Private_Extension_Declaration
        and then Is_Derived_Type (Full_T)
      then
         Priv_Parent := Etype (Priv_T);

         --  The full view of a private extension may have been transformed
         --  into an unconstrained derived type declaration and a subtype
         --  declaration (see build_derived_record_type for details).

         if Nkind (N) = N_Subtype_Declaration then
            Full_Indic  := Subtype_Indication (N);
            Full_Parent := Etype (Base_Type (Full_T));
         else
            Full_Indic  := Subtype_Indication (Type_Definition (N));
            Full_Parent := Etype (Full_T);
         end if;

         --  Check that the parent type of the full type is a descendant of
         --  the ancestor subtype given in the private extension. If either
         --  entity has an Etype equal to Any_Type then we had some previous
         --  error situation [7.3(8)].

         if Priv_Parent = Any_Type or else Full_Parent = Any_Type then
            return;

         elsif not Is_Ancestor (Base_Type (Priv_Parent), Full_Parent) then
            Error_Msg_N
              ("parent of full type must descend from parent"
                  & " of private extension", Full_Indic);

         --  Check the rules of 7.3(12): if a partial view has neither known
         --  or unknown discriminants, then the full type declaration shall
         --  define a definite subtype.

         elsif not Is_Constrained (Full_T)
         then
            Error_Msg_N
              ("full view must define a constrained type if partial view"
               & " has no discriminants", Full_T);
         end if;

         --  ??????? Do we implement the following properly ?????
         --  If the ancestor subtype of a private extension has constrained
         --  discriminants, then the parent subtype of the full view shall
         --  impose a statically matching constraint on those discriminants
         --  [7.3(13)].

      else
         --  For untagged types, verify that a type without discriminants
         --  is not completed with an unconstrained type.

         if not Is_Indefinite_Subtype (Priv_T)
           and then Is_Indefinite_Subtype (Full_T)
         then
            Error_Msg_N ("full view of type must be definite subtype", Full_T);
         end if;
      end if;

      --  Create a full declaration for all its subtypes recorded in
      --  Private_Dependents and swap them similarly to the base type.
      --  These are subtypes that have been define before the full
      --  declaration of the private type. We also swap the entry in
      --  Private_Dependents list so we can properly restore the
      --  private view on exit from the scope.

      declare
         Priv_Elmt : Elmt_Id;
         Priv      : Entity_Id;
         Full      : Entity_Id;

      begin
         Priv_Elmt := First_Elmt (Private_Dependents (Priv_T));
         while Present (Priv_Elmt) loop
            Priv := Node (Priv_Elmt);

            if Ekind (Priv) = E_Private_Subtype
              or else Ekind (Priv) = E_Record_Subtype_With_Private
            then
               Full := Make_Defining_Identifier (Sloc (Priv), Chars (Priv));
               Set_Is_Itype (Full);
               Set_Parent (Full, Parent (Priv));
               Set_Associated_Node_For_Itype (Full, N);

               --  Now we need to complete the private subtype, but since the
               --  base type has already been swapped, we must also swap the
               --  subtypes (and thus, reverse the arguments in the call to
               --  Complete_Private_Subtype).

               Copy_And_Swap (Priv, Full);
               Complete_Private_Subtype (Full, Priv, Full_T, N);
               Replace_Elmt (Priv_Elmt, Full);
            end if;

            Next_Elmt (Priv_Elmt);
         end loop;
      end;

      --  If the private view was tagged, copy the new Primitive
      --  operations from the private view to the full view.

      if Is_Tagged_Type (Full_T) then
         declare
            Priv_List : Elist_Id;
            Full_List : constant Elist_Id := Primitive_Operations (Full_T);
            P1, P2    : Elmt_Id;
            Prim      : Entity_Id;
            D_Type    : Entity_Id;

         begin
            if Is_Tagged_Type (Priv_T) then
               Priv_List := Primitive_Operations (Priv_T);

               P1 := First_Elmt (Priv_List);
               while Present (P1) loop
                  Prim := Node (P1);

                  --  Transfer explicit primitives, not those inherited from
                  --  parent of partial view, which will be re-inherited on
                  --  the full view.

                  if Comes_From_Source (Prim) then
                     P2 := First_Elmt (Full_List);
                     while Present (P2) and then Node (P2) /= Prim loop
                        Next_Elmt (P2);
                     end loop;

                     --  If not found, that is a new one

                     if No (P2) then
                        Append_Elmt (Prim, Full_List);
                     end if;
                  end if;

                  Next_Elmt (P1);
               end loop;

            else
               --  In this case the partial view is untagged, so here we
               --  locate all of the earlier primitives that need to be
               --  treated as dispatching (those that appear between the
               --  two views). Note that these additional operations must
               --  all be new operations (any earlier operations that
               --  override inherited operations of the full view will
               --  already have been inserted in the primitives list and
               --  marked as dispatching by Check_Operation_From_Private_View.
               --  Note that implicit "/=" operators are excluded from being
               --  added to the primitives list since they shouldn't be
               --  treated as dispatching (tagged "/=" is handled specially).

               Prim := Next_Entity (Full_T);
               while Present (Prim) and then Prim /= Priv_T loop
                  if Ekind (Prim) = E_Procedure
                       or else
                     Ekind (Prim) = E_Function
                  then

                     D_Type := Find_Dispatching_Type (Prim);

                     if D_Type = Full_T
                       and then (Chars (Prim) /= Name_Op_Ne
                                  or else Comes_From_Source (Prim))
                     then
                        Check_Controlling_Formals (Full_T, Prim);

                        if not Is_Dispatching_Operation (Prim) then
                           Append_Elmt (Prim, Full_List);
                           Set_Is_Dispatching_Operation (Prim, True);
                           Set_DT_Position (Prim, No_Uint);
                        end if;

                     elsif Is_Dispatching_Operation (Prim)
                       and then D_Type  /= Full_T
                     then

                        --  Verify that it is not otherwise controlled by
                        --  a formal or a return value ot type T.

                        Check_Controlling_Formals (D_Type, Prim);
                     end if;
                  end if;

                  Next_Entity (Prim);
               end loop;
            end if;

            --  For the tagged case, the two views can share the same
            --  Primitive Operation list and the same class wide type.
            --  Update attributes of the class-wide type which depend on
            --  the full declaration.

            if Is_Tagged_Type (Priv_T) then
               Set_Primitive_Operations (Priv_T, Full_List);
               Set_Class_Wide_Type
                 (Base_Type (Full_T), Class_Wide_Type (Priv_T));

            end if;
         end;
      end if;
   end Process_Full_View;

   -----------------------------------
   -- Process_Incomplete_Dependents --
   -----------------------------------

   procedure Process_Incomplete_Dependents
     (N      : Node_Id;
      Full_T : Entity_Id;
      Inc_T  : Entity_Id)
   is
      Inc_Elmt : Elmt_Id;
      Priv_Dep : Entity_Id;
      New_Subt : Entity_Id;

      Disc_Constraint : Elist_Id;

   begin
      if No (Private_Dependents (Inc_T)) then
         return;

      else
         Inc_Elmt := First_Elmt (Private_Dependents (Inc_T));

         --  Itypes that may be generated by the completion of an incomplete
         --  subtype are not used by the back-end and not attached to the tree.
         --  They are created only for constraint-checking purposes.
      end if;

      while Present (Inc_Elmt) loop
         Priv_Dep := Node (Inc_Elmt);

         if Ekind (Priv_Dep) = E_Subprogram_Type then

            --  An Access_To_Subprogram type may have a return type or a
            --  parameter type that is incomplete. Replace with the full view.

            if Etype (Priv_Dep) = Inc_T then
               Set_Etype (Priv_Dep, Full_T);
            end if;

            declare
               Formal : Entity_Id;

            begin
               Formal := First_Formal (Priv_Dep);

               while Present (Formal) loop

                  if Etype (Formal) = Inc_T then
                     Set_Etype (Formal, Full_T);
                  end if;

                  Next_Formal (Formal);
               end loop;
            end;

         elsif  Is_Overloadable (Priv_Dep) then

            if Is_Tagged_Type (Full_T) then

               --  Subprogram has an access parameter whose designated type
               --  was incomplete. Reexamine declaration now, because it may
               --  be a primitive operation of the full type.

               Check_Operation_From_Incomplete_Type (Priv_Dep, Inc_T);
               Set_Is_Dispatching_Operation (Priv_Dep);
               Check_Controlling_Formals (Full_T, Priv_Dep);
            end if;

         elsif Ekind (Priv_Dep) = E_Subprogram_Body then

            --  Can happen during processing of a body before the completion
            --  of a TA type. Ignore, because spec is also on dependent list.

            return;

         --  Dependent is a subtype

         else
            --  We build a new subtype indication using the full view of the
            --  incomplete parent. The discriminant constraints have been
            --  elaborated already at the point of the subtype declaration.

            New_Subt := Create_Itype (E_Void, N);

	    Disc_Constraint := No_Elist;

            Build_Discriminated_Subtype (Full_T, New_Subt, Disc_Constraint, N);
            Set_Full_View (Priv_Dep, New_Subt);
         end if;

         Next_Elmt (Inc_Elmt);
      end loop;

   end Process_Incomplete_Dependents;

   --------------------------------
   -- Process_Range_Expr_In_Decl --
   --------------------------------

   procedure Process_Range_Expr_In_Decl
     (R           : Node_Id;
      T           : Entity_Id;
      Check_List  : List_Id := Empty_List;
      R_Check_Off : Boolean := False)
   is
      Lo, Hi    : Node_Id;

   begin
      Analyze_And_Resolve (R, Base_Type (T));

      if Nkind (R) = N_Range then
         Lo := Low_Bound (R);
         Hi := High_Bound (R);

         --  If there were errors in the declaration, try and patch up some
         --  common mistakes in the bounds. The cases handled are literals
         --  which are Integer where the expected type is Real and vice versa.
         --  These corrections allow the compilation process to proceed further
         --  along since some basic assumptions of the format of the bounds
         --  are guaranteed.

         if Etype (R) = Any_Type then

            if Nkind (Lo) = N_Integer_Literal and then Is_Real_Type (T) then
               Rewrite (Lo,
                 Make_Real_Literal (Sloc (Lo), UR_From_Uint (Intval (Lo))));

            elsif Nkind (Hi) = N_Integer_Literal and then Is_Real_Type (T) then
               Rewrite (Hi,
                 Make_Real_Literal (Sloc (Hi), UR_From_Uint (Intval (Hi))));

            elsif Nkind (Lo) = N_Real_Literal and then Is_Integer_Type (T) then
               Rewrite (Lo,
                 Make_Integer_Literal (Sloc (Lo), UR_To_Uint (Realval (Lo))));

            elsif Nkind (Hi) = N_Real_Literal and then Is_Integer_Type (T) then
               Rewrite (Hi,
                 Make_Integer_Literal (Sloc (Hi), UR_To_Uint (Realval (Hi))));
            end if;

            Set_Etype (Lo, T);
            Set_Etype (Hi, T);
         end if;

         --  If the bounds of the range have been mistakenly given as
         --  string literals (perhaps in place of character literals),
         --  then an error has already been reported, but we rewrite
         --  the string literal as a bound of the range's type to
         --  avoid blowups in later processing that looks at static
         --  values.

         if Nkind (Lo) = N_String_Literal then
            Rewrite (Lo,
              Make_Attribute_Reference (Sloc (Lo),
                Attribute_Name => Name_First,
                Prefix => New_Reference_To (T, Sloc (Lo))));
            Analyze_And_Resolve (Lo);
         end if;

         if Nkind (Hi) = N_String_Literal then
            Rewrite (Hi,
              Make_Attribute_Reference (Sloc (Hi),
                Attribute_Name => Name_First,
                Prefix => New_Reference_To (T, Sloc (Hi))));
            Analyze_And_Resolve (Hi);
         end if;

         --  If bounds aren't scalar at this point then exit, avoiding
         --  problems with further processing of the range in this procedure.

         if not Is_Scalar_Type (Etype (Lo)) then
            return;
         end if;

         --  Resolve (actually Sem_Eval) has checked that the bounds are in
         --  then range of the base type. Here we check whether the bounds
         --  are in the range of the subtype itself. Note that if the bounds
         --  represent the null range the Constraint_Error exception should
         --  not be raised.

         --  ??? The following code should be cleaned up as follows
         --  1. The Is_Null_Range (Lo, Hi) test should disappear since it
         --     is done in the call to Range_Check (R, T); below
         --  2. The use of R_Check_Off should be investigated and possibly
         --     removed, this would clean up things a bit.

         if Is_Null_Range (Lo, Hi) then
            null;

         else
            --  Capture values of bounds and generate temporaries for them
            --  if needed, before applying checks, since checks may cause
            --  duplication of the expression without forcing evaluation.

	    if Expander_Active then
	       Force_Evaluation (Lo);
	       Force_Evaluation (Hi);
	    end if;
         end if;

      elsif Expander_Active then
         Get_Index_Bounds (R, Lo, Hi);
         Force_Evaluation (Lo);
         Force_Evaluation (Hi);
      end if;
   end Process_Range_Expr_In_Decl;

   --------------------------------------
   -- Process_Real_Range_Specification --
   --------------------------------------

   procedure Process_Real_Range_Specification (Def : Node_Id) is
      Spec : constant Node_Id := Real_Range_Specification (Def);
      Lo   : Node_Id;
      Hi   : Node_Id;
      Err  : Boolean := False;

      procedure Analyze_Bound (N : Node_Id);
      --  Analyze and check one bound

      -------------------
      -- Analyze_Bound --
      -------------------

      procedure Analyze_Bound (N : Node_Id) is
      begin
         Analyze_And_Resolve (N, Any_Real);

         if not Is_OK_Static_Expression (N) then
            Flag_Non_Static_Expr
              ("bound in real type definition is not static!", N);
            Err := True;
         end if;
      end Analyze_Bound;

   --  Start of processing for Process_Real_Range_Specification

   begin
      if Present (Spec) then
         Lo := Low_Bound (Spec);
         Hi := High_Bound (Spec);
         Analyze_Bound (Lo);
         Analyze_Bound (Hi);

         --  If error, clear away junk range specification

         if Err then
            Set_Real_Range_Specification (Def, Empty);
         end if;
      end if;
   end Process_Real_Range_Specification;

   ---------------------
   -- Process_Subtype --
   ---------------------

   function Process_Subtype
     (S           : Node_Id;
      Related_Nod : Node_Id;
      Related_Id  : Entity_Id := Empty;
      Suffix      : Character := ' ') return Entity_Id
   is
      P               : Node_Id;
      Def_Id          : Entity_Id;
      Subtype_Mark_Id : Entity_Id;

      procedure Check_Incomplete (T : Entity_Id);
      --  Called to verify that an incomplete type is not used prematurely

      ----------------------
      -- Check_Incomplete --
      ----------------------

      procedure Check_Incomplete (T : Entity_Id) is
      begin
         if Ekind (Root_Type (Entity (T))) = E_Incomplete_Type then
            Error_Msg_N ("invalid use of type before its full declaration", T);
         end if;
      end Check_Incomplete;

   --  Start of processing for Process_Subtype

   begin
      --  Case of no constraints present

      if Nkind (S) /= N_Subtype_Indication then
         Find_Type (S);
         Check_Incomplete (S);
         return Entity (S);

      --  Case of constraint present, so that we have an N_Subtype_Indication
      --  node (this node is created only if constraints are present).

      else
         Find_Type (Subtype_Mark (S));
         if Nkind (Parent (S)) /= N_Access_To_Object_Definition
           and then not
            (Nkind (Parent (S)) = N_Subtype_Declaration
              and then
             Is_Itype (Defining_Identifier (Parent (S))))
         then
            Check_Incomplete (Subtype_Mark (S));
         end if;

         P := Parent (S);
         Subtype_Mark_Id := Entity (Subtype_Mark (S));
	 
         --  Explicit subtype declaration case

         if Nkind (P) = N_Subtype_Declaration then
            Def_Id := Defining_Identifier (P);
	    
	    --  Explicit derived type definition case

         elsif Nkind (P) = N_Derived_Type_Definition then
            Def_Id := Defining_Identifier (Parent (P));

         --  Implicit case, the Def_Id must be created as an implicit type.
         --  The one exception arises in the case of concurrent types,
         --  array and access types, where other subsidiary implicit types
         --  may be created and must appear before the main implicit type.
         --  In these cases we leave Def_Id set to Empty as a signal that
         --  Create_Itype has not yet been called to create Def_Id.

         else
            if Is_Array_Type (Subtype_Mark_Id)
              or else Is_Access_Type (Subtype_Mark_Id)
            then
               Def_Id := Empty;

            --  For the other cases, we create a new unattached Itype,
            --  and set the indication to ensure it gets attached later.

            else
               Def_Id :=
                 Create_Itype (E_Void, Related_Nod, Related_Id, Suffix);
            end if;
         end if;

         --  If the kind of constraint is invalid for this kind of type,
         --  then give an error, and then pretend no constraint was given.

         if not Is_Valid_Constraint_Kind
                   (Ekind (Subtype_Mark_Id), Nkind (Constraint (S)))
         then
            Error_Msg_N
              ("incorrect constraint for this kind of type", Constraint (S));

            Rewrite (S, New_Copy_Tree (Subtype_Mark (S)));

            --  Make recursive call, having got rid of the bogus constraint

            return Process_Subtype (S, Related_Nod, Related_Id, Suffix);
         end if;

         --  Remaining processing depends on type

         case Ekind (Subtype_Mark_Id) is

            when Access_Kind =>
               Constrain_Access (Def_Id, S, Related_Nod);

            when Array_Kind =>
               Constrain_Array (Def_Id, S, Related_Nod, Related_Id, Suffix);

            when Enumeration_Kind =>
               Constrain_Enumeration (Def_Id, S);

            when Float_Kind =>
               Constrain_Float (Def_Id, S);

            when Integer_Kind =>
               Constrain_Integer (Def_Id, S);

            when E_Record_Type     |
                 E_Record_Subtype  |
                 Class_Wide_Kind   |
                 E_Incomplete_Type =>
               null;

            when Private_Kind =>
               Set_Private_Dependents (Def_Id, New_Elmt_List);

               --  In case of an invalid constraint prevent further processing
               --  since the type constructed is missing expected fields.

               if Etype (Def_Id) = Any_Type then
                  return Def_Id;
               end if;

               --  If the full view is that of a task with discriminants,
               --  we must constrain both the concurrent type and its
               --  corresponding record type. Otherwise we will just propagate
               --  the constraint to the full view, if available.

	       Prepare_Private_Subtype_Completion (Def_Id, Related_Nod);

            when others =>
               Error_Msg_N ("invalid subtype mark in subtype indication", S);
         end case;

         --  Size and Convention are always inherited from the base type

         Set_Size_Info  (Def_Id,            (Subtype_Mark_Id));
         Set_Convention (Def_Id, Convention (Subtype_Mark_Id));

         return Def_Id;

      end if;
   end Process_Subtype;

   -----------------------------
   -- Record_Type_Declaration --
   -----------------------------

   procedure Record_Type_Declaration
     (T    : Entity_Id;
      N    : Node_Id;
      Prev : Entity_Id)
   is
      Def : constant Node_Id := Type_Definition (N);

      Is_Tagged : Boolean;

   begin
      --  The flag Is_Tagged_Type might have already been set by Find_Type_Name
      --  if it detected an error for declaration T. This arises in the case of
      --  private tagged types where the full view omits the word tagged.

      Is_Tagged := Tagged_Present (Def)
        or else (Serious_Errors_Detected > 0 and then Is_Tagged_Type (T));

      --  Records constitute a scope for the component declarations within.
      --  The scope is created prior to the processing of these declarations.
      --  Discriminants are processed first, so that they are visible when
      --  processing the other components. The Ekind of the record type itself
      --  is set to E_Record_Type (subtypes appear as E_Record_Subtype).

      --  Enter record scope

      New_Scope (T);

      --  These flags must be initialized before calling Process_Discriminants
      --  because this routine makes use of them.

      Set_Is_Tagged_Type     (T, Is_Tagged);

      --  Type is abstract if full declaration carries keyword, or if
      --  previous partial view did.

      Set_Is_Abstract (T, Is_Abstract (T) or else Abstract_Present (Def));

      Set_Ekind       (T, E_Record_Type);
      Set_Etype       (T, T);
      Init_Size_Align (T);

      Set_Stored_Constraint (T, No_Elist);

      --  If an incomplete or private type declaration was already given for
      --  the type, then this scope already exists, and the discriminants have
      --  been declared within. We must verify that the full declaration
      --  matches the incomplete one.

      Set_Is_Constrained     (T, True);
      Set_Has_Delayed_Freeze (T, True);

      --  For tagged types add a manually analyzed component corresponding
      --  to the component _tag, the corresponding piece of tree will be
      --  expanded as part of the freezing actions if it is not a CPP_Class.

      if Is_Tagged then
         --  Do not add the tag unless we are in expansion mode.

         --  if Expander_Active then
         --     Tag_Comp := Make_Defining_Identifier (Sloc (Def), Name_uTag);
         --     Enter_Name (Tag_Comp);

         --     Set_Is_Tag                    (Tag_Comp);
         --     Set_Ekind                     (Tag_Comp, E_Component);
         --     Set_Etype                     (Tag_Comp, RTE (RE_Tag));
         --     Set_DT_Entry_Count            (Tag_Comp, No_Uint);
         --     Set_Original_Record_Component (Tag_Comp, Tag_Comp);
         --     Init_Component_Location       (Tag_Comp);
         --  end if;

         Make_Class_Wide_Type (T);
         Set_Primitive_Operations (T, New_Elmt_List);
      end if;

      --  We must suppress range checks when processing the components
      --  of a record in the presence of discriminants, since we don't
      --  want spurious checks to be generated during their analysis, but
      --  must reset the Suppress_Range_Checks flags after having processed
      --  the record definition.

      Record_Type_Definition (Def, Prev);

      --  Exit from record scope

      End_Scope;
   end Record_Type_Declaration;

   ----------------------------
   -- Record_Type_Definition --
   ----------------------------

   procedure Record_Type_Definition (Def : Node_Id; Prev_T : Entity_Id) is
      Component : Entity_Id;
      T         : Entity_Id;

   begin
      if Ekind (Prev_T) = E_Incomplete_Type then
         T := Full_View (Prev_T);
      else
         T := Prev_T;
      end if;

      --  If the component list of a record type is defined by the reserved
      --  word null and there is no discriminant part, then the record type has
      --  no components and all records of the type are null records (RM 3.7)
      --  This procedure is also called to process the extension part of a
      --  record extension, in which case the current scope may have inherited
      --  components.

      if No (Def)
        or else No (Component_List (Def))
        or else Null_Present (Component_List (Def))
      then
         null;

      else
         Analyze_Declarations (Component_Items (Component_List (Def)));
      end if;

      --  After completing the semantic analysis of the record definition,
      --  record components, both new and inherited, are accessible. Set
      --  their kind accordingly.

      Component := First_Entity (Current_Scope);
      while Present (Component) loop

         if Ekind (Component) = E_Void then
            Set_Ekind (Component, E_Component);
            Init_Component_Location (Component);
         end if;

         Next_Entity (Component);
      end loop;
      
      --  Place reference to end record on the proper entity, which may
      --  be a partial view.

      if Present (Def) then
         Process_End_Label (Def, 'e', Prev_T);
      end if;
   end Record_Type_Definition;

   ------------------------
   -- Replace_Components --
   ------------------------

   procedure Replace_Components (Typ : Entity_Id; Decl : Node_Id) is
      function Process (N : Node_Id) return Traverse_Result;

      -------------
      -- Process --
      -------------

      function Process (N : Node_Id) return Traverse_Result is
         Comp : Entity_Id;

      begin
         if Nkind (N) = N_Component_Declaration then
            Comp := First_Component (Typ);

            while Present (Comp) loop
               if Chars (Comp) = Chars (Defining_Identifier (N)) then
                  Set_Defining_Identifier (N, Comp);
                  exit;
               end if;

               Next_Component (Comp);
            end loop;
         end if;

         return OK;
      end Process;

      procedure Replace is new Traverse_Proc (Process);

   --  Start of processing for Replace_Components

   begin
      Replace (Decl);
   end Replace_Components;

   -------------------------------
   -- Set_Completion_Referenced --
   -------------------------------

   procedure Set_Completion_Referenced (E : Entity_Id) is
   begin
      --  If in main unit, mark entity that is a completion as referenced,
      --  warnings go on the partial view when needed.

      if In_Extended_Main_Source_Unit (E) then
         Set_Referenced (E);
      end if;
   end Set_Completion_Referenced;

   ----------------------------------
   -- Set_Scalar_Range_For_Subtype --
   ----------------------------------

   procedure Set_Scalar_Range_For_Subtype
     (Def_Id : Entity_Id;
      R      : Node_Id;
      Subt   : Entity_Id)
   is
      Kind : constant Entity_Kind :=  Ekind (Def_Id);
   begin
      Set_Scalar_Range (Def_Id, R);

      --  We need to link the range into the tree before resolving it so
      --  that types that are referenced, including importantly the subtype
      --  itself, are properly frozen (Freeze_Expression requires that the
      --  expression be properly linked into the tree). Of course if it is
      --  already linked in, then we do not disturb the current link.

      if No (Parent (R)) then
         Set_Parent (R, Def_Id);
      end if;

      --  Reset the kind of the subtype during analysis of the range, to
      --  catch possible premature use in the bounds themselves.

      Set_Ekind (Def_Id, E_Void);
      Process_Range_Expr_In_Decl (R, Subt);
      Set_Ekind (Def_Id, Kind);

   end Set_Scalar_Range_For_Subtype;

   -------------------------------------
   -- Signed_Integer_Type_Declaration --
   -------------------------------------

   procedure Signed_Integer_Type_Declaration (T : Entity_Id; Def : Node_Id) is
      Implicit_Base : Entity_Id;
      Base_Typ      : Entity_Id;
      Lo_Val        : Uint;
      Hi_Val        : Uint;
      Errs          : Boolean := False;
      Lo            : Node_Id;
      Hi            : Node_Id;

      function Can_Derive_From (E : Entity_Id) return Boolean;
      --  Determine whether given bounds allow derivation from specified type

      procedure Check_Bound (Expr : Node_Id);
      --  Check bound to make sure it is integral and static. If not, post
      --  appropriate error message and set Errs flag

      ---------------------
      -- Can_Derive_From --
      ---------------------

      function Can_Derive_From (E : Entity_Id) return Boolean is
         Lo : constant Uint := Expr_Value (Type_Low_Bound (E));
         Hi : constant Uint := Expr_Value (Type_High_Bound (E));

      begin
         --  Note we check both bounds against both end values, to deal with
         --  strange types like ones with a range of 0 .. -12341234.

         return Lo <= Lo_Val and then Lo_Val <= Hi
                  and then
                Lo <= Hi_Val and then Hi_Val <= Hi;
      end Can_Derive_From;

      -----------------
      -- Check_Bound --
      -----------------

      procedure Check_Bound (Expr : Node_Id) is
      begin
         --  If a range constraint is used as an integer type definition, each
         --  bound of the range must be defined by a static expression of some
         --  integer type, but the two bounds need not have the same integer
         --  type (Negative bounds are allowed.) (RM 3.5.4)

         if not Is_Integer_Type (Etype (Expr)) then
            Error_Msg_N
              ("integer type definition bounds must be of integer type", Expr);
            Errs := True;

         elsif not Is_OK_Static_Expression (Expr) then
            Flag_Non_Static_Expr
              ("non-static expression used for integer type bound!", Expr);
            Errs := True;

         --  The bounds are folded into literals, and we set their type to be
         --  universal, to avoid typing difficulties: we cannot set the type
         --  of the literal to the new type, because this would be a forward
         --  reference for the back end,  and if the original type is user-
         --  defined this can lead to spurious semantic errors (e.g. 2928-003).

         else
            if Is_Entity_Name (Expr) then
               Fold_Uint (Expr, Expr_Value (Expr), True);
            end if;

            Set_Etype (Expr, Universal_Integer);
         end if;
      end Check_Bound;

   --  Start of processing for Signed_Integer_Type_Declaration

   begin
      --  Create an anonymous base type

      Implicit_Base :=
        Create_Itype (E_Signed_Integer_Type, Parent (Def), T, 'B');

      --  Analyze and check the bounds, they can be of any integer type

      Lo := Low_Bound (Def);
      Hi := High_Bound (Def);

      --  Arbitrarily use Integer as the type if either bound had an error

      if Hi = Error or else Lo = Error then
         Base_Typ := Any_Integer;
         Set_Error_Posted (T, True);

      --  Here both bounds are OK expressions

      else
         Analyze_And_Resolve (Lo, Any_Integer);
         Analyze_And_Resolve (Hi, Any_Integer);

         Check_Bound (Lo);
         Check_Bound (Hi);

         if Errs then
            Hi := Type_High_Bound (Standard_Long_Long_Integer);
            Lo := Type_Low_Bound (Standard_Long_Long_Integer);
         end if;

         --  Find type to derive from

         Lo_Val := Expr_Value (Lo);
         Hi_Val := Expr_Value (Hi);

         if Can_Derive_From (Standard_Short_Short_Integer) then
            Base_Typ := Base_Type (Standard_Short_Short_Integer);

         elsif Can_Derive_From (Standard_Short_Integer) then
            Base_Typ := Base_Type (Standard_Short_Integer);

         elsif Can_Derive_From (Standard_Integer) then
            Base_Typ := Base_Type (Standard_Integer);

         elsif Can_Derive_From (Standard_Long_Integer) then
            Base_Typ := Base_Type (Standard_Long_Integer);

         elsif Can_Derive_From (Standard_Long_Long_Integer) then
            Base_Typ := Base_Type (Standard_Long_Long_Integer);

         else
            Base_Typ := Base_Type (Standard_Long_Long_Integer);
            Error_Msg_N ("integer type definition bounds out of range", Def);
            Hi := Type_High_Bound (Standard_Long_Long_Integer);
            Lo := Type_Low_Bound (Standard_Long_Long_Integer);
         end if;
      end if;

      --  Complete both implicit base and declared first subtype entities

      Set_Etype          (Implicit_Base, Base_Typ);
      Set_Scalar_Range   (Implicit_Base, Scalar_Range   (Base_Typ));
      Set_Size_Info      (Implicit_Base,                (Base_Typ));
      Set_RM_Size        (Implicit_Base, RM_Size        (Base_Typ));
      Set_First_Rep_Item (Implicit_Base, First_Rep_Item (Base_Typ));

      Set_Ekind          (T, E_Signed_Integer_Subtype);
      Set_Etype          (T, Implicit_Base);

      Set_Size_Info      (T,                (Implicit_Base));
      Set_First_Rep_Item (T, First_Rep_Item (Implicit_Base));
      Set_Scalar_Range   (T, Def);
      Set_RM_Size        (T, UI_From_Int (Minimum_Size (T)));
      Set_Is_Constrained (T);
   end Signed_Integer_Type_Declaration;

end Sem_Ch3;
