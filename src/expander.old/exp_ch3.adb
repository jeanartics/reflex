------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 3                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2004 Free Software Foundation, Inc.          --
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
with Exp_Aggr; use Exp_Aggr;
with Exp_Ch4;  use Exp_Ch4;
with Exp_Ch7;  use Exp_Ch7;
-- with Exp_Ch9;  use Exp_Ch9;
with Exp_Ch11; use Exp_Ch11;
with Exp_Disp; use Exp_Disp;
--  with Exp_Dist; use Exp_Dist;
-- with Exp_Smem; use Exp_Smem;
--with Exp_Strm; use Exp_Strm;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Freeze;   use Freeze;
with Hostparm; use Hostparm;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Mech; use Sem_Mech;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Snames;   use Snames;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;
with Validsw;  use Validsw;
with Namet; use Namet;

package body Exp_Ch3 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Build_Array_Init_Proc (A_Type : Entity_Id; Nod : Node_Id);
   --  Build initialization procedure for given array type. Nod is a node
   --  used for attachment of any actions required in its construction.
   --  It also supplies the source location used for the procedure.

   procedure Build_Record_Init_Proc (N : Node_Id; Pe : Entity_Id);
   --  Build record initialization procedure. N is the type declaration
   --  node, and Pe is the corresponding entity for the record type.

   procedure Expand_Tagged_Root (T : Entity_Id);
   --  Add a field _Tag at the beginning of the record. This field carries
   --  the value of the access to the Dispatch table. This procedure is only
   --  called on root (non CPP_Class) types, the _Tag field being inherited
   --  by the descendants.

   procedure Expand_Record_Controller (T : Entity_Id);
   --  T must be a record type that Has_Controlled_Component. Add a field
   --  _controller of type Record_Controller or Limited_Record_Controller
   --  in the record T.

   procedure Freeze_Array_Type (N : Node_Id);
   --  Freeze an array type. Deals with building the initialization procedure,
   --  creating the packed array type for a packed array and also with the
   --  creation of the controlling procedures for the controlled case. The
   --  argument N is the N_Freeze_Entity node for the type.

   procedure Freeze_Enumeration_Type (N : Node_Id);
   --  Freeze enumeration type with non-standard representation. Builds the
   --  array and function needed to convert between enumeration pos and
   --  enumeration representation values. N is the N_Freeze_Entity node
   --  for the type.

   procedure Freeze_Record_Type (N : Node_Id);
   --  Freeze record type. Builds all necessary discriminant checking
   --  and other ancillary functions, and builds dispatch tables where
   --  needed. The argument N is the N_Freeze_Entity node. This processing
   --  applies only to E_Record_Type entities, not to class wide types,
   --  record subtypes, or private types.

   function Init_Formals (Typ : Entity_Id) return List_Id;
   --  This function builds the list of formals for an initialization routine.
   --  The first formal is always _Init with the given type. For task value
   --  record types and types containing tasks, three additional formals are
   --  added:
   --
   --    _Master    : Master_Id
   --    _Chain     : in out Activation_Chain
   --    _Task_Name : String
   --
   --  The caller must append additional entries for discriminants if required.

   function In_Runtime (E : Entity_Id) return Boolean;
   --  Check if E is defined in the RTL (in a child of Ada or System). Used
   --  to avoid to bring in the overhead of _Input, _Output for tagged types.

   function Make_Eq_Case (Node : Node_Id; CL : Node_Id) return List_Id;
   --  Building block for variant record equality. Defined to share the
   --  code between the tagged and non-tagged case. Given a Component_List
   --  node CL, it generates an 'if' followed by a 'case' statement that
   --  compares all components of local temporaries named X and Y (that
   --  are declared as formals at some upper level). Node provides the
   --  Sloc to be used for the generated code.

   function Make_Eq_If (Node : Node_Id; L : List_Id) return Node_Id;
   --  Building block for variant record equality. Defined to share the
   --  code between the tagged and non-tagged case. Given the list of
   --  components (or discriminants) L, it generates a return statement
   --  that compares all components of local temporaries named X and Y
   --  (that are declared as formals at some upper level). Node provides
   --  the Sloc to be used for the generated code.

   procedure Make_Predefined_Primitive_Specs
     (Tag_Typ     : Entity_Id;
      Predef_List : out List_Id;
      Renamed_Eq  : out Node_Id);
   --  Create a list with the specs of the predefined primitive operations.
   --  The following entries are present for all tagged types, and provide
   --  the results of the corresponding attribute applied to the object.
   --  Dispatching is required in general, since the result of the attribute
   --  will vary with the actual object subtype.
   --
   --     _alignment     provides result of 'Alignment attribute
   --     _size          provides result of 'Size attribute
   --     typSR          provides result of 'Read attribute
   --     typSW          provides result of 'Write attribute
   --     typSI          provides result of 'Input attribute
   --     typSO          provides result of 'Output attribute
   --
   --  The following entries are additionally present for non-limited
   --  tagged types, and implement additional dispatching operations
   --  for predefined operations:
   --
   --     _equality      implements "=" operator
   --     _assign        implements assignment operation
   --     typDF          implements deep finalization
   --     typDA          implements deep adust
   --
   --  The latter two are empty procedures unless the type contains some
   --  controlled components that require finalization actions (the deep
   --  in the name refers to the fact that the action applies to components).
   --
   --  The list is returned in Predef_List. The Parameter Renamed_Eq
   --  either returns the value Empty, or else the defining unit name
   --  for the predefined equality function in the case where the type
   --  has a primitive operation that is a renaming of predefined equality
   --  (but only if there is also an overriding user-defined equality
   --  function). The returned Renamed_Eq will be passed to the
   --  corresponding parameter of Predefined_Primitive_Bodies.

   function Has_New_Non_Standard_Rep (T : Entity_Id) return Boolean;
   --  returns True if there are representation clauses for type T that
   --  are not inherited. If the result is false, the init_proc and the
   --  discriminant_checking functions of the parent can be reused by
   --  a derived type.

   function Predef_Spec_Or_Body
     (Loc      : Source_Ptr;
      Tag_Typ  : Entity_Id;
      Name     : Name_Id;
      Profile  : List_Id;
      Ret_Type : Entity_Id := Empty;
      For_Body : Boolean   := False)
      return     Node_Id;
   --  This function generates the appropriate expansion for a predefined
   --  primitive operation specified by its name, parameter profile and
   --  return type (Empty means this is a procedure). If For_Body is false,
   --  then the returned node is a subprogram declaration. If For_Body is
   --  true, then the returned node is a empty subprogram body containing
   --  no declarations and no statements.

   function Predef_Deep_Spec
     (Loc      : Source_Ptr;
      Tag_Typ  : Entity_Id;
      Name     : TSS_Name_Type;
      For_Body : Boolean := False)
      return     Node_Id;
   --  Specialized version of Predef_Spec_Or_Body that apply to _deep_adjust
   --  and _deep_finalize

   function Predefined_Primitive_Bodies
     (Tag_Typ    : Entity_Id;
      Renamed_Eq : Node_Id)
      return       List_Id;
   --  Create the bodies of the predefined primitives that are described in
   --  Predefined_Primitive_Specs. When not empty, Renamed_Eq must denote
   --  the defining unit name of the type's predefined equality as returned
   --  by Make_Predefined_Primitive_Specs.

   function Predefined_Primitive_Freeze (Tag_Typ : Entity_Id) return List_Id;
   --  Freeze entities of all predefined primitive operations. This is needed
   --  because the bodies of these operations do not normally do any freezeing.

   ---------------------------
   -- Build_Array_Init_Proc --
   ---------------------------

   procedure Build_Array_Init_Proc (A_Type : Entity_Id; Nod : Node_Id) is
      Loc        : constant Source_Ptr := Sloc (Nod);
      Comp_Type  : constant Entity_Id  := Component_Type (A_Type);
      Index_List : List_Id;
      Proc_Id    : Entity_Id;
      Body_Stmts : List_Id;

      function Init_Component return List_Id;
      --  Create one statement to initialize one array component, designated
      --  by a full set of indices.

      function Init_One_Dimension (N : Int) return List_Id;
      --  Create loop to initialize one dimension of the array. The single
      --  statement in the loop body initializes the inner dimensions if any,
      --  or else the single component. Note that this procedure is called
      --  recursively, with N being the dimension to be initialized. A call
      --  with N greater than the number of dimensions simply generates the
      --  component initialization, terminating the recursion.

      --------------------
      -- Init_Component --
      --------------------

      function Init_Component return List_Id is
         Comp : Node_Id;

      begin
         Comp :=
           Make_Indexed_Component (Loc,
             Prefix => Make_Identifier (Loc, Name_uInit),
             Expressions => Index_List);

         if Needs_Simple_Initialization (Comp_Type) then
            Set_Assignment_OK (Comp);
            return New_List (
              Make_Assignment_Statement (Loc,
                Name => Comp,
                Expression => Get_Simple_Init_Val (Comp_Type, Loc)));

         else
            return
              Build_Initialization_Call (Loc, Comp, Comp_Type, True, A_Type);
         end if;
      end Init_Component;

      ------------------------
      -- Init_One_Dimension --
      ------------------------

      function Init_One_Dimension (N : Int) return List_Id is
         Index      : Entity_Id;

      begin
         --  If the component does not need initializing, then there is nothing
         --  to do here, so we return a null body. This occurs when generating
         --  the dummy Init_Proc needed for Initialize_Scalars processing.

         if not Has_Non_Null_Base_Init_Proc (Comp_Type)
           and then not Needs_Simple_Initialization (Comp_Type)
         then
            return New_List (Make_Null_Statement (Loc));

         --  If all dimensions dealt with, we simply initialize the component

         elsif N > Number_Dimensions (A_Type) then
            return Init_Component;

         --  Here we generate the required loop

         else
            Index :=
              Make_Defining_Identifier (Loc, New_External_Name ('J', N));

            Append (New_Reference_To (Index, Loc), Index_List);

            return New_List (
              Make_Implicit_Loop_Statement (Nod,
                Identifier => Empty,
                Iteration_Scheme =>
                  Make_Iteration_Scheme (Loc,
                    Loop_Parameter_Specification =>
                      Make_Loop_Parameter_Specification (Loc,
                        Defining_Identifier => Index,
                        Discrete_Subtype_Definition =>
                          Make_Attribute_Reference (Loc,
                            Prefix => Make_Identifier (Loc, Name_uInit),
                            Attribute_Name  => Name_Range,
                            Expressions => New_List (
                              Make_Integer_Literal (Loc, N))))),
                Statements =>  Init_One_Dimension (N + 1)));
         end if;
      end Init_One_Dimension;

   --  Start of processing for Build_Array_Init_Proc

   begin
      if Suppress_Init_Proc (A_Type) then
         return;
      end if;

      Index_List := New_List;

      --  We need an initialization procedure if any of the following is true:

      --    1. The component type has an initialization procedure
      --    2. The component type needs simple initialization
      --    3. Tasks are present
      --    4. The type is marked as a publc entity

      --  The reason for the public entity test is to deal properly with the
      --  Initialize_Scalars pragma. This pragma can be set in the client and
      --  not in the declaring package, this means the client will make a call
      --  to the initialization procedure (because one of conditions 1-3 must
      --  apply in this case), and we must generate a procedure (even if it is
      --  null) to satisfy the call in this case.

      --  Exception: do not build an array init_proc for a type whose root type
      --  is Standard.String or Standard.Wide_String, since there is no place
      --  to put the code, and in any case we handle initialization of such
      --  types (in the Initialize_Scalars case, that's the only time the issue
      --  arises) in a special manner anyway which does not need an init_proc.

      if Has_Non_Null_Base_Init_Proc (Comp_Type)
        or else Needs_Simple_Initialization (Comp_Type)
        or else (not Restrictions (No_Initialize_Scalars)
                   and then Is_Public (A_Type)
                   and then Root_Type (A_Type) /= Standard_String
                   and then Root_Type (A_Type) /= Standard_Wide_String)
      then
         Proc_Id :=
           Make_Defining_Identifier (Loc, Make_Init_Proc_Name (A_Type));

         Body_Stmts := Init_One_Dimension (1);

         Discard_Node (
           Make_Subprogram_Body (Loc,
             Specification =>
               Make_Procedure_Specification (Loc,
                 Defining_Unit_Name => Proc_Id,
                 Parameter_Specifications => Init_Formals (A_Type)),
             Declarations => New_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Body_Stmts)));

         Set_Ekind          (Proc_Id, E_Procedure);
         Set_Is_Public      (Proc_Id, Is_Public (A_Type));
         Set_Is_Internal    (Proc_Id);
         Set_Has_Completion (Proc_Id);

         if not Debug_Generated_Code then
            Set_Debug_Info_Off (Proc_Id);
         end if;

         --  Associate Init_Proc with type, and determine if the procedure
         --  is null (happens because of the Initialize_Scalars pragma case,
         --  where we have to generate a null procedure in case it is called
         --  by a client with Initialize_Scalars set). Such procedures have
         --  to be generated, but do not have to be called, so we mark them
         --  as null to suppress the call.

         Set_Init_Proc (A_Type, Proc_Id);

         if List_Length (Body_Stmts) = 1
           and then Nkind (First (Body_Stmts)) = N_Null_Statement
         then
            Set_Is_Null_Init_Proc (Proc_Id);
         end if;
      end if;
   end Build_Array_Init_Proc;

   -------------------------------
   -- Build_Initialization_Call --
   -------------------------------

   --  References to a discriminant inside the record type declaration
   --  can appear either in the subtype_indication to constrain a
   --  record or an array, or as part of a larger expression given for
   --  the initial value of a component. In both of these cases N appears
   --  in the record initialization procedure and needs to be replaced by
   --  the formal parameter of the initialization procedure which
   --  corresponds to that discriminant.

   --  In the example below, references to discriminants D1 and D2 in proc_1
   --  are replaced by references to formals with the same name
   --  (discriminals)

   --  A similar replacement is done for calls to any record
   --  initialization procedure for any components that are themselves
   --  of a record type.

   --  type R (D1, D2 : Integer) is record
   --     X : Integer := F * D1;
   --     Y : Integer := F * D2;
   --  end record;

   --  procedure proc_1 (Out_2 : out R; D1 : Integer; D2 : Integer) is
   --  begin
   --     Out_2.D1 := D1;
   --     Out_2.D2 := D2;
   --     Out_2.X := F * D1;
   --     Out_2.Y := F * D2;
   --  end;

   function Build_Initialization_Call
     (Loc               : Source_Ptr;
      Id_Ref            : Node_Id;
      Typ               : Entity_Id;
      In_Init_Proc      : Boolean := False;
      Enclos_Type       : Entity_Id := Empty;
      Discr_Map         : Elist_Id := New_Elmt_List;
      With_Default_Init : Boolean := False)
      return              List_Id
   is
      First_Arg      : Node_Id;
      Args           : List_Id;
      Decls          : List_Id;
      Decl           : Node_Id;
      Proc           : constant Entity_Id := Base_Init_Proc (Typ);
      Init_Type      : constant Entity_Id := Etype (First_Formal (Proc));
      Full_Init_Type : constant Entity_Id := Underlying_Type (Init_Type);
      Res            : constant List_Id   := New_List;
      Full_Type      : Entity_Id := Typ;

   begin
      --  Nothing to do if the Init_Proc is null, unless Initialize_Sclalars
      --  is active (in which case we make the call anyway, since in the
      --  actual compiled client it may be non null).

      if Is_Null_Init_Proc (Proc) and then not Init_Or_Norm_Scalars then
         return Empty_List;
      end if;

      --  Go to full view if private type. In the case of successive
      --  private derivations, this can require more than one step.

      while Is_Private_Type (Full_Type)
        and then Present (Full_View (Full_Type))
      loop
         Full_Type := Full_View (Full_Type);
      end loop;

      --  If Typ is derived, the procedure is the initialization procedure for
      --  the root type. Wrap the argument in an conversion to make it type
      --  honest. Actually it isn't quite type honest, because there can be
      --  conflicts of views in the private type case. That is why we set
      --  Conversion_OK in the conversion node.
      if (Is_Record_Type (Typ)
          or else Is_Array_Type (Typ)
          or else Is_Private_Type (Typ))
        and then Init_Type /= Base_Type (Typ)
      then
         First_Arg := OK_Convert_To (Etype (Init_Type), Id_Ref);
         Set_Etype (First_Arg, Init_Type);

      else
         First_Arg := Id_Ref;
      end if;

      Args := New_List (First_Arg);

      Decls := No_List;
      Decl  := Empty;

      --  If this is a call to initialize the parent component of a derived
      --  tagged type, indicate that the tag should not be set in the parent.

      if Is_Tagged_Type (Full_Init_Type)
        and then not Is_CPP_Class (Full_Init_Type)
        and then Nkind (Id_Ref) = N_Selected_Component
        and then Chars (Selector_Name (Id_Ref)) = Name_uParent
      then
         Append_To (Args, New_Occurrence_Of (Standard_False, Loc));
      end if;

      Append_To (Res,
        Make_Procedure_Call_Statement (Loc,
                   Name => New_Occurrence_Of (Proc, Loc),
          Parameter_Associations => Args));



      return Res;

   exception
      when RE_Not_Available =>
         return Empty_List;
   end Build_Initialization_Call;

   ----------------------------
   -- Build_Record_Init_Proc --
   ----------------------------

   procedure Build_Record_Init_Proc (N : Node_Id; Pe : Entity_Id) is
   begin
      null;
   end Build_Record_Init_Proc;

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

      if not Expander_Active then
         return;
      end if;

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
        Make_Component_Declaration (Loc,
          Defining_Identifier => Parent_N,
          Component_Definition =>
            Make_Component_Definition (Loc,
              Aliased_Present => False,
              Subtype_Indication => New_Reference_To (Par_Subtype, Loc)));

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

      Analyze (Comp_Decl);
   end Expand_Derived_Record;

   ------------------------------------
   -- Expand_N_Full_Type_Declaration --
   ------------------------------------

   procedure Expand_N_Full_Type_Declaration (N : Node_Id) is
      Def_Id : constant Entity_Id := Defining_Identifier (N);
      B_Id   : constant Entity_Id := Base_Type (Def_Id);
      Par_Id : Entity_Id;
      FN     : Node_Id;

   begin
      Par_Id := Etype (B_Id);

      --  The parent type is private then we need to inherit
      --  any TSS operations from the full view.

      if Ekind (Par_Id) in Private_Kind
        and then Present (Full_View (Par_Id))
      then
         Par_Id := Base_Type (Full_View (Par_Id));
      end if;

      if Nkind (Type_Definition (Original_Node (N)))
         = N_Derived_Type_Definition
        and then not Is_Tagged_Type (Def_Id)
        and then Present (Freeze_Node (Par_Id))
        and then Present (TSS_Elist (Freeze_Node (Par_Id)))
      then
         Ensure_Freeze_Node (B_Id);
         FN :=  Freeze_Node (B_Id);

         if No (TSS_Elist (FN)) then
            Set_TSS_Elist (FN, New_Elmt_List);
         end if;

         declare
            T_E   : constant Elist_Id := TSS_Elist (FN);
            Elmt  : Elmt_Id;

         begin
            Elmt  := First_Elmt (TSS_Elist (Freeze_Node (Par_Id)));

            while Present (Elmt) loop
               if Chars (Node (Elmt)) /= Name_uInit then
                  Append_Elmt (Node (Elmt), T_E);
               end if;

               Next_Elmt (Elmt);
            end loop;

            --  If the derived type itself is private with a full view,
            --  then associate the full view with the inherited TSS_Elist
            --  as well.

            if Ekind (B_Id) in Private_Kind
              and then Present (Full_View (B_Id))
            then
               Ensure_Freeze_Node (Base_Type (Full_View (B_Id)));
               Set_TSS_Elist
                 (Freeze_Node (Base_Type (Full_View (B_Id))), TSS_Elist (FN));
            end if;
         end;
      end if;
   end Expand_N_Full_Type_Declaration;

   ---------------------------------
   -- Expand_N_Object_Declaration --
   ---------------------------------

   --  First we do special processing for objects of a tagged type where this
   --  is the point at which the type is frozen. The creation of the dispatch
   --  table and the initialization procedure have to be deferred to this
   --  point, since we reference previously declared primitive subprograms.

   --  For all types, we call an initialization procedure if there is one

   procedure Expand_N_Object_Declaration (N : Node_Id) is
      Def_Id  : constant Entity_Id  := Defining_Identifier (N);
      Typ     : constant Entity_Id  := Etype (Def_Id);
      Loc     : constant Source_Ptr := Sloc (N);
      Expr    : constant Node_Id    := Expression (N);
      New_Ref : Node_Id;
      Expr_Q  : Node_Id;

   begin
      --  Don't do anything for deferred constants. All proper actions will
      --  be expanded during the full declaration.

      if No (Expr) and Constant_Present (N) then
         return;
      end if;

      --  Default initialization required, and no expression present

         --  Obtain actual expression from qualified expression

         if Nkind (Expr) = N_Qualified_Expression then
            Expr_Q := Expression (Expr);
         else
            Expr_Q := Expr;
         end if;

         --  When we have the appropriate type of aggregate in the
         --  expression (it has been determined during analysis of the
         --  aggregate by setting the delay flag), let's perform in
         --  place assignment and thus avoid creating a temporary.

         if Is_Delayed_Aggregate (Expr_Q) then
            Convert_Aggr_In_Object_Decl (N);

         else
            --  In most cases, we must check that the initial value meets
            --  any constraint imposed by the declared type. However, there
            --  is one very important exception to this rule. If the entity
            --  has an unconstrained nominal subtype, then it acquired its
            --  constraints from the expression in the first place, and not
            --  only does this mean that the constraint check is not needed,
            --  but an attempt to perform the constraint check can
            --  cause order of elaboration problems.

            if not Is_Constr_Subt_For_U_Nominal (Typ) then

               --  If this is an allocator for an aggregate that has been
               --  allocated in place, delay checks until assignments are
               --  made, because the discriminants are not initialized.

               if Nkind (Expr) = N_Allocator
                 and then No_Initialization (Expr)
               then
                  null;
               else
                  Apply_Constraint_Check (Expr, Typ);
               end if;
            end if;


            --  For tagged types, when an init value is given, the tag has
            --  to be re-initialized separately in order to avoid the
            --  propagation of a wrong tag coming from a view conversion
            --  unless the type is class wide (in this case the tag comes
            --  from the init value). Suppress the tag assignment when
            --  Java_VM because JVM tags are represented implicitly
            --  in objects. Ditto for types that are CPP_CLASS.

            if Is_Tagged_Type (Typ)
              and then not Is_Class_Wide_Type (Typ)
              and then not Is_CPP_Class (Typ)
            then
               --  The re-assignment of the tag has to be done even if
               --  the object is a constant

               New_Ref :=
                 Make_Selected_Component (Loc,
                    Prefix => New_Reference_To (Def_Id, Loc),
                    Selector_Name =>
                      New_Reference_To (Tag_Component (Typ), Loc));

               Set_Assignment_OK (New_Ref);

               Insert_After (N,
                 Make_Assignment_Statement (Loc,
                   Name => New_Ref,
                   Expression =>
                     Unchecked_Convert_To (RTE (RE_Tag),
                       New_Reference_To
                         (Access_Disp_Table (Base_Type (Typ)), Loc))));

            --  For discrete types, set the Is_Known_Valid flag if the
            --  initializing value is known to be valid.

            elsif Is_Discrete_Type (Typ)
              and then Expr_Known_Valid (Expr)
            then
               Set_Is_Known_Valid (Def_Id);

            --  For access types set the Is_Known_Non_Null flag if the
            --  initializing value is known to be non-null. We can also
            --  set Can_Never_Be_Null if this is a constant.

            elsif Is_Access_Type (Typ)
              and then Known_Non_Null (Expr)
            then
               Set_Is_Known_Non_Null (Def_Id);

               if Constant_Present (N) then
                  Set_Can_Never_Be_Null (Def_Id);
               end if;
            end if;

            --  If validity checking on copies, validate initial expression

            if Validity_Checks_On
               and then Validity_Check_Copies
            then
               Ensure_Valid (Expr);
               Set_Is_Known_Valid (Def_Id);
            end if;
         end if;

         if Is_Possibly_Unaligned_Slice (Expr) then

            --  Make a separate assignment that will be expanded into a
            --  loop, to bypass back-end problems with misaligned arrays.

            declare
               Stat : constant Node_Id :=
                       Make_Assignment_Statement (Loc,
                         Name => New_Reference_To (Def_Id, Loc),
                         Expression => Relocate_Node (Expr));

            begin
               Set_Expression (N, Empty);
               Set_No_Initialization (N);
               Set_Assignment_OK (Name (Stat));
               Insert_After (N, Stat);
               Analyze (Stat);
            end;
         end if;

      --  For array type, check for size too large
      --  We really need this for record types too???

      if Is_Array_Type (Typ) then
         Apply_Array_Size_Check (N, Typ);
      end if;

   exception
      when RE_Not_Available =>
         return;
   end Expand_N_Object_Declaration;

   ---------------------------------
   -- Expand_N_Subtype_Indication --
   ---------------------------------

   --  Add a check on the range of the subtype. The static case is
   --  partially duplicated by Process_Range_Expr_In_Decl in Sem_Ch3,
   --  but we still need to check here for the static case in order to
   --  avoid generating extraneous expanded code.

   procedure Expand_N_Subtype_Indication (N : Node_Id) is
      Ran : constant Node_Id   := Range_Expression (Constraint (N));
      Typ : constant Entity_Id := Entity (Subtype_Mark (N));

   begin
      if Nkind (Parent (N)) = N_Constrained_Array_Definition or else
         Nkind (Parent (N)) = N_Slice
      then
         Resolve (Ran, Typ);
         Apply_Range_Check (Ran, Typ);
      end if;
   end Expand_N_Subtype_Indication;

   ------------------------------
   -- Expand_Record_Controller --
   ------------------------------

   procedure Expand_Record_Controller (T : Entity_Id) is
      Def             : Node_Id := Type_Definition (Parent (T));
      Comp_List       : Node_Id;
      Comp_Decl       : Node_Id;
      Loc             : Source_Ptr;
      First_Comp      : Node_Id;
      Controller_Type : Entity_Id;
      Ent             : Entity_Id;

   begin
      if Nkind (Def) = N_Derived_Type_Definition then
         Def := Record_Extension_Part (Def);
      end if;

      if Null_Present (Def) then
         Set_Component_List (Def,
           Make_Component_List (Sloc (Def),
             Component_Items => Empty_List,
             Null_Present => True));
      end if;

      Comp_List := Component_List (Def);

      if Null_Present (Comp_List)
        or else Is_Empty_List (Component_Items (Comp_List))
      then
         Loc := Sloc (Comp_List);
      else
         Loc := Sloc (First (Component_Items (Comp_List)));
      end if;

      if Is_Return_By_Reference_Type (T) then
         Controller_Type := RTE (RE_Limited_Record_Controller);
      else
         Controller_Type := RTE (RE_Record_Controller);
      end if;

      Ent := Make_Defining_Identifier (Loc, Name_uController);

      Comp_Decl :=
        Make_Component_Declaration (Loc,
          Defining_Identifier =>  Ent,
          Component_Definition =>
            Make_Component_Definition (Loc,
              Aliased_Present => False,
              Subtype_Indication => New_Reference_To (Controller_Type, Loc)));

      if Null_Present (Comp_List)
        or else Is_Empty_List (Component_Items (Comp_List))
      then
         Set_Component_Items (Comp_List, New_List (Comp_Decl));
         Set_Null_Present (Comp_List, False);

      else
         --  The controller cannot be placed before the _Parent field
         --  since gigi lays out field in order and _parent must be
         --  first to preserve the polymorphism of tagged types.

         First_Comp := First (Component_Items (Comp_List));

         if Chars (Defining_Identifier (First_Comp)) /= Name_uParent
           and then Chars (Defining_Identifier (First_Comp)) /= Name_uTag
         then
            Insert_Before (First_Comp, Comp_Decl);
         else
            Insert_After (First_Comp, Comp_Decl);
         end if;
      end if;

      New_Scope (T);
      Analyze (Comp_Decl);
      Set_Ekind (Ent, E_Component);
      Init_Component_Location (Ent);

      --  Move the _controller entity ahead in the list of internal
      --  entities of the enclosing record so that it is selected
      --  instead of a potentially inherited one.

      declare
         E    : constant Entity_Id := Last_Entity (T);
         Comp : Entity_Id;

      begin
         pragma Assert (Chars (E) = Name_uController);

         Set_Next_Entity (E, First_Entity (T));
         Set_First_Entity (T, E);

         Comp := Next_Entity (E);
         while Next_Entity (Comp) /= E loop
            Next_Entity (Comp);
         end loop;

         Set_Next_Entity (Comp, Empty);
         Set_Last_Entity (T, Comp);
      end;

      End_Scope;

   exception
      when RE_Not_Available =>
         return;
   end Expand_Record_Controller;

   ------------------------
   -- Expand_Tagged_Root --
   ------------------------

   procedure Expand_Tagged_Root (T : Entity_Id) is
      Def       : constant Node_Id := Type_Definition (Parent (T));
      Comp_List : Node_Id;
      Comp_Decl : Node_Id;
      Sloc_N    : Source_Ptr;

   begin
      if Null_Present (Def) then
         Set_Component_List (Def,
           Make_Component_List (Sloc (Def),
             Component_Items => Empty_List,
             Null_Present => True));
      end if;

      Comp_List := Component_List (Def);

      if Null_Present (Comp_List)
        or else Is_Empty_List (Component_Items (Comp_List))
      then
         Sloc_N := Sloc (Comp_List);
      else
         Sloc_N := Sloc (First (Component_Items (Comp_List)));
      end if;

      Comp_Decl :=
        Make_Component_Declaration (Sloc_N,
          Defining_Identifier => Tag_Component (T),
          Component_Definition =>
            Make_Component_Definition (Sloc_N,
              Aliased_Present => False,
              Subtype_Indication => New_Reference_To (RTE (RE_Tag), Sloc_N)));

      if Null_Present (Comp_List)
        or else Is_Empty_List (Component_Items (Comp_List))
      then
         Set_Component_Items (Comp_List, New_List (Comp_Decl));
         Set_Null_Present (Comp_List, False);

      else
         Insert_Before (First (Component_Items (Comp_List)), Comp_Decl);
      end if;

      --  We don't Analyze the whole expansion because the tag component has
      --  already been analyzed previously. Here we just insure that the
      --  tree is coherent with the semantic decoration

      Find_Type (Subtype_Indication (Component_Definition (Comp_Decl)));

   exception
      when RE_Not_Available =>
         return;
   end Expand_Tagged_Root;

   -----------------------
   -- Freeze_Array_Type --
   -----------------------

   procedure Freeze_Array_Type (N : Node_Id) is
      Typ  : constant Entity_Id  := Entity (N);
      Base : constant Entity_Id  := Base_Type (Typ);

   begin
      if not Is_Bit_Packed_Array (Typ) then

         --  If the component contains tasks, so does the array type.
         --  This may not be indicated in the array type because the
         --  component may have been a private type at the point of
         --  definition. Same if component type is controlled.

         Set_Has_Controlled_Component (Base,
           Has_Controlled_Component (Component_Type (Typ))
             or else Is_Controlled (Component_Type (Typ)));

         if No (Init_Proc (Base)) then

            --  If this is an anonymous array created for a declaration
            --  with an initial value, its init_proc will never be called.
            --  The initial value itself may have been expanded into assign-
            --  ments, in which case the object declaration is carries the
            --  No_Initialization flag.

            if Is_Itype (Base)
              and then Nkind (Associated_Node_For_Itype (Base)) =
                                                    N_Object_Declaration
              and then (Present (Expression (Associated_Node_For_Itype (Base)))
                          or else
                        No_Initialization (Associated_Node_For_Itype (Base)))
            then
               null;

            --  We do not need an init proc for string or wide string, since
            --  the only time these need initialization in normalize or
            --  initialize scalars mode, and these types are treated specially
            --  and do not need initialization procedures.

            elsif Root_Type (Base) = Standard_String
              or else Root_Type (Base) = Standard_Wide_String
            then
               null;

            --  Otherwise we have to build an init proc for the subtype

            else
               Build_Array_Init_Proc (Base, N);
            end if;
         end if;

      --  For packed case, there is a default initialization, except
      --  if the component type is itself a packed structure with an
      --  initialization procedure.

      elsif Present (Init_Proc (Component_Type (Base)))
        and then No (Base_Init_Proc (Base))
      then
         Build_Array_Init_Proc (Base, N);
      end if;
   end Freeze_Array_Type;

   -----------------------------
   -- Freeze_Enumeration_Type --
   -----------------------------

   procedure Freeze_Enumeration_Type (N : Node_Id) is
      Typ           : constant Entity_Id  := Entity (N);
      Loc           : constant Source_Ptr := Sloc (Typ);
      Ent           : Entity_Id;
      Lst           : List_Id;
      Num           : Nat;
      Arr           : Entity_Id;
      Fent          : Entity_Id;
      Ityp          : Entity_Id;
      Is_Contiguous : Boolean;
      Pos_Expr      : Node_Id;
      Last_Repval   : Uint;

      Func : Entity_Id;
      pragma Warnings (Off, Func);

   begin
      --  Various optimization are possible if the given representation
      --  is contiguous.

      Is_Contiguous := True;
      Ent := First_Literal (Typ);
      Last_Repval := Enumeration_Rep (Ent);
      Next_Literal (Ent);

      while Present (Ent) loop
         if Enumeration_Rep (Ent) - Last_Repval /= 1 then
            Is_Contiguous := False;
            exit;
         else
            Last_Repval := Enumeration_Rep (Ent);
         end if;

         Next_Literal (Ent);
      end loop;

      if Is_Contiguous then
         Set_Has_Contiguous_Rep (Typ);
         Ent := First_Literal (Typ);
         Num := 1;
         Lst := New_List (New_Reference_To (Ent, Sloc (Ent)));

      else
         --  Build list of literal references

         Lst := New_List;
         Num := 0;

         Ent := First_Literal (Typ);
         while Present (Ent) loop
            Append_To (Lst, New_Reference_To (Ent, Sloc (Ent)));
            Num := Num + 1;
            Next_Literal (Ent);
         end loop;
      end if;

      --  Now build an array declaration.

      --    typA : array (Natural range 0 .. num - 1) of ctype :=
      --             (v, v, v, v, v, ....)

      --  where ctype is the corresponding integer type. If the
      --  representation is contiguous, we only keep the first literal,
      --  which provides the offset for Pos_To_Rep computations.

      Arr :=
        Make_Defining_Identifier (Loc,
          Chars => New_External_Name (Chars (Typ), 'A'));

      Append_Freeze_Action (Typ,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Arr,
          Constant_Present    => True,

          Object_Definition   =>
            Make_Constrained_Array_Definition (Loc,
              Discrete_Subtype_Definitions => New_List (
                Make_Subtype_Indication (Loc,
                  Subtype_Mark => New_Reference_To (Standard_Natural, Loc),
                  Constraint =>
                    Make_Range_Constraint (Loc,
                      Range_Expression =>
                        Make_Range (Loc,
                          Low_Bound  =>
                            Make_Integer_Literal (Loc, 0),
                          High_Bound =>
                            Make_Integer_Literal (Loc, Num - 1))))),

              Component_Definition =>
                Make_Component_Definition (Loc,
                  Aliased_Present => False,
                  Subtype_Indication => New_Reference_To (Typ, Loc))),

          Expression =>
            Make_Aggregate (Loc,
              Expressions => Lst)));

      Set_Enum_Pos_To_Rep (Typ, Arr);

      --  Now we build the function that converts representation values to
      --  position values. This function has the form:

      --    function _Rep_To_Pos (A : etype; F : Boolean) return Integer is
      --    begin
      --       case ityp!(A) is
      --         when enum-lit'Enum_Rep => return posval;
      --         when enum-lit'Enum_Rep => return posval;
      --         ...
      --         when others   =>
      --           [raise Constraint_Error when F "invalid data"]
      --           return -1;
      --       end case;
      --    end;

      --  Note: the F parameter determines whether the others case (no valid
      --  representation) raises Constraint_Error or returns a unique value
      --  of minus one. The latter case is used, e.g. in 'Valid code.

      --  Note: the reason we use Enum_Rep values in the case here is to
      --  avoid the code generator making inappropriate assumptions about
      --  the range of the values in the case where the value is invalid.
      --  ityp is a signed or unsigned integer type of appropriate width.

      --  Note: if exceptions are not supported, then we suppress the raise
      --  and return -1 unconditionally (this is an erroneous program in any
      --  case and there is no obligation to raise Constraint_Error here!)
      --  We also do this if pragma Restrictions (No_Exceptions) is active.

      --  Representations are signed

      if Enumeration_Rep (First_Literal (Typ)) < 0 then

         --  The underlying type is signed. Reset the Is_Unsigned_Type
         --  explicitly, because it might have been inherited from a
         --  parent type.

         Set_Is_Unsigned_Type (Typ, False);

         if Esize (Typ) <= Standard_Integer_Size then
            Ityp := Standard_Integer;
         else
            Ityp := Universal_Integer;
         end if;

      --  Representations are unsigned

      else
         if Esize (Typ) <= Standard_Integer_Size then
            Ityp := RTE (RE_Unsigned);
         else
            Ityp := RTE (RE_Long_Long_Unsigned);
         end if;
      end if;

      --  The body of the function is a case statement. First collect
      --  case alternatives, or optimize the contiguous case.

      Lst := New_List;

      --  If representation is contiguous, Pos is computed by subtracting
      --  the representation of the first literal.

      if Is_Contiguous then
         Ent := First_Literal (Typ);

         if Enumeration_Rep (Ent) = Last_Repval then

            --  Another special case: for a single literal, Pos is zero.

            Pos_Expr := Make_Integer_Literal (Loc, Uint_0);

         else
            Pos_Expr :=
              Convert_To (Standard_Integer,
                Make_Op_Subtract (Loc,
                  Left_Opnd =>
                     Unchecked_Convert_To (Ityp,
                       Make_Identifier (Loc, Name_uA)),
                   Right_Opnd =>
                     Make_Integer_Literal (Loc,
                        Intval =>
                          Enumeration_Rep (First_Literal (Typ)))));
         end if;

         Append_To (Lst,
              Make_Case_Statement_Alternative (Loc,
                Discrete_Choices => New_List (
                  Make_Range (Sloc (Enumeration_Rep_Expr (Ent)),
                    Low_Bound =>
                      Make_Integer_Literal (Loc,
                       Intval =>  Enumeration_Rep (Ent)),
                    High_Bound =>
                      Make_Integer_Literal (Loc, Intval => Last_Repval))),

                Statements => New_List (
                  Make_Return_Statement (Loc,
                    Expression => Pos_Expr))));

      else
         Ent := First_Literal (Typ);

         while Present (Ent) loop
            Append_To (Lst,
              Make_Case_Statement_Alternative (Loc,
                Discrete_Choices => New_List (
                  Make_Integer_Literal (Sloc (Enumeration_Rep_Expr (Ent)),
                    Intval => Enumeration_Rep (Ent))),

                Statements => New_List (
                  Make_Return_Statement (Loc,
                    Expression =>
                      Make_Integer_Literal (Loc,
                        Intval => Enumeration_Pos (Ent))))));

            Next_Literal (Ent);
         end loop;
      end if;

      --  In normal mode, add the others clause with the test

      if not Restrictions (No_Exception_Handlers) then
         Append_To (Lst,
           Make_Case_Statement_Alternative (Loc,
             Discrete_Choices => New_List (Make_Others_Choice (Loc)),
             Statements => New_List (
               Make_Raise_Constraint_Error (Loc,
                 Condition => Make_Identifier (Loc, Name_uF),
                 Reason    => CE_Invalid_Data),
               Make_Return_Statement (Loc,
                 Expression =>
                   Make_Integer_Literal (Loc, -1)))));

      --  If Restriction (No_Exceptions_Handlers) is active then we always
      --  return -1 (since we cannot usefully raise Constraint_Error in
      --  this case). See description above for further details.

      else
         Append_To (Lst,
           Make_Case_Statement_Alternative (Loc,
             Discrete_Choices => New_List (Make_Others_Choice (Loc)),
             Statements => New_List (
               Make_Return_Statement (Loc,
                 Expression =>
                   Make_Integer_Literal (Loc, -1)))));
      end if;

      --  Now we can build the function body

      Fent :=
        Make_Defining_Identifier (Loc, Make_TSS_Name (Typ, TSS_Rep_To_Pos));

      Func :=
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Function_Specification (Loc,
              Defining_Unit_Name       => Fent,
              Parameter_Specifications => New_List (
                Make_Parameter_Specification (Loc,
                  Defining_Identifier =>
                    Make_Defining_Identifier (Loc, Name_uA),
                  Parameter_Type => New_Reference_To (Typ, Loc)),
                Make_Parameter_Specification (Loc,
                  Defining_Identifier =>
                    Make_Defining_Identifier (Loc, Name_uF),
                  Parameter_Type => New_Reference_To (Standard_Boolean, Loc))),

              Subtype_Mark => New_Reference_To (Standard_Integer, Loc)),

            Declarations => Empty_List,

            Handled_Statement_Sequence =>
              Make_Handled_Sequence_Of_Statements (Loc,
                Statements => New_List (
                  Make_Case_Statement (Loc,
                    Expression =>
                      Unchecked_Convert_To (Ityp,
                        Make_Identifier (Loc, Name_uA)),
                    Alternatives => Lst))));

      Set_TSS (Typ, Fent);
      Set_Is_Pure (Fent);

      if not Debug_Generated_Code then
         Set_Debug_Info_Off (Fent);
      end if;

   exception
      when RE_Not_Available =>
         return;
   end Freeze_Enumeration_Type;

   ------------------------
   -- Freeze_Record_Type --
   ------------------------

   procedure Freeze_Record_Type (N : Node_Id) is
   begin
      null;
   end Freeze_Record_Type;

   -----------------
   -- Freeze_Type --
   -----------------

   --  Full type declarations are expanded at the point at which the type
   --  is frozen. The formal N is the Freeze_Node for the type. Any statements
   --  or declarations generated by the freezing (e.g. the procedure generated
   --  for initialization) are chained in the Acions field list of the freeze
   --  node using Append_Freeze_Actions.

   procedure Freeze_Type (N : Node_Id) is
      Def_Id    : constant Entity_Id := Entity (N);
      RACW_Seen : Boolean := False;

   begin
      --  Freeze processing for record types

      if Is_Record_Type (Def_Id) then
         if Ekind (Def_Id) = E_Record_Type then
            Freeze_Record_Type (N);
            end if;

      --  Freeze processing for array types

      elsif Is_Array_Type (Def_Id) then
         Freeze_Array_Type (N);

      --  Freeze processing for access types

      --  For pool-specific access types, find out the pool object used for
      --  this type, needs actual expansion of it in some cases. Here are the
      --  different cases :

      --  1. Rep Clause "for Def_Id'Storage_Size use 0;"
      --      ---> don't use any storage pool

      --  2. Rep Clause : for Def_Id'Storage_Size use Expr.
      --     Expand:
      --      Def_Id__Pool : Stack_Bounded_Pool (Expr, DT'Size, DT'Alignment);

      --  3. Rep Clause "for Def_Id'Storage_Pool use a_Pool_Object"
      --      ---> Storage Pool is the specified one

      --  See GNAT Pool packages in the Run-Time for more details

      elsif Ekind (Def_Id) = E_Access_Type
        or else Ekind (Def_Id) = E_General_Access_Type
      then
         declare
            Loc         : constant Source_Ptr := Sloc (N);
            Desig_Type  : constant Entity_Id := Designated_Type (Def_Id);
            Pool_Object : Entity_Id;
            Siz_Exp     : Node_Id;

            Freeze_Action_Typ : Entity_Id;

         begin
            if Has_Storage_Size_Clause (Def_Id) then
               Siz_Exp := Expression (Parent (Storage_Size_Variable (Def_Id)));
            else
               Siz_Exp := Empty;
            end if;

            --  Case 1

            --    Rep Clause "for Def_Id'Storage_Size use 0;"
            --    ---> don't use any storage pool

            if Has_Storage_Size_Clause (Def_Id)
              and then Compile_Time_Known_Value (Siz_Exp)
              and then Expr_Value (Siz_Exp) = 0
            then
               null;

            --  Case 2

            --    Rep Clause : for Def_Id'Storage_Size use Expr.
            --    ---> Expand:
            --           Def_Id__Pool : Stack_Bounded_Pool
            --                            (Expr, DT'Size, DT'Alignment);

            elsif Has_Storage_Size_Clause (Def_Id) then
               declare
                  DT_Size  : Node_Id;
                  DT_Align : Node_Id;

               begin
                  --  For unconstrained composite types we give a size of
                  --  zero so that the pool knows that it needs a special
                  --  algorithm for variable size object allocation.

                  if Is_Composite_Type (Desig_Type)
                    and then not Is_Constrained (Desig_Type)
                  then
                     DT_Size :=
                       Make_Integer_Literal (Loc, 0);

                     DT_Align :=
                       Make_Integer_Literal (Loc, Maximum_Alignment);

                  else
                     DT_Size :=
                       Make_Attribute_Reference (Loc,
                         Prefix => New_Reference_To (Desig_Type, Loc),
                         Attribute_Name => Name_Max_Size_In_Storage_Elements);

                     DT_Align :=
                       Make_Attribute_Reference (Loc,
                         Prefix => New_Reference_To (Desig_Type, Loc),
                         Attribute_Name => Name_Alignment);
                  end if;

                  Pool_Object :=
                    Make_Defining_Identifier (Loc,
                      Chars => New_External_Name (Chars (Def_Id), 'P'));

                  --  We put the code associated with the pools in the
                  --  entity that has the later freeze node, usually the
                  --  acces type but it can also be the designated_type;
                  --  because the pool code requires both those types to be
                  --  frozen

                  if Is_Frozen (Desig_Type)
                    and then (not Present (Freeze_Node (Desig_Type))
                               or else Analyzed (Freeze_Node (Desig_Type)))
                  then
                     Freeze_Action_Typ := Def_Id;

                  --  A Taft amendment type cannot get the freeze actions
                  --  since the full view is not there.

                  elsif Is_Incomplete_Or_Private_Type (Desig_Type)
                    and then No (Full_View (Desig_Type))
                  then
                     Freeze_Action_Typ := Def_Id;

                  else
                     Freeze_Action_Typ := Desig_Type;
                  end if;

                  Append_Freeze_Action (Freeze_Action_Typ,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Pool_Object,
                      Object_Definition =>
                        Make_Subtype_Indication (Loc,
                          Subtype_Mark =>
                            New_Reference_To
                              (RTE (RE_Stack_Bounded_Pool), Loc),

                          Constraint =>
                            Make_Index_Or_Discriminant_Constraint (Loc,
                              Constraints => New_List (

                              --  First discriminant is the Pool Size

                                New_Reference_To (
                                  Storage_Size_Variable (Def_Id), Loc),

                              --  Second discriminant is the element size

                                DT_Size,

                              --  Third discriminant is the alignment

                                DT_Align)))));
               end;

               Set_Associated_Storage_Pool (Def_Id, Pool_Object);

            --  Case 3

            --    Rep Clause "for Def_Id'Storage_Pool use a_Pool_Object"
            --    ---> Storage Pool is the specified one

            elsif Present (Associated_Storage_Pool (Def_Id)) then

               --  Nothing to do the associated storage pool has been attached
               --  when analyzing the rep. clause

               null;
            end if;

            --  For access-to-controlled types (including class-wide types
            --  and Taft-amendment types which potentially have controlled
            --  components), expand the list controller object that will
            --  store the dynamically allocated objects. Do not do this
            --  transformation for expander-generated access types, but do it
            --  for types that are the full view of types derived from other
            --  private types. Also suppress the list controller in the case
            --  of a designated type with convention Java, since this is used
            --  when binding to Java API specs, where there's no equivalent
            --  of a finalization list and we don't want to pull in the
            --  finalization support if not needed.

            if not Comes_From_Source (Def_Id)
               and then not Has_Private_Declaration (Def_Id)
            then
               null;

            end if;
         end;

      --  Freeze processing for enumeration types

      elsif Ekind (Def_Id) = E_Enumeration_Type then

         --  We only have something to do if we have a non-standard
         --  representation (i.e. at least one literal whose pos value
         --  is not the same as its representation)

         if Has_Non_Standard_Rep (Def_Id) then
            Freeze_Enumeration_Type (N);
         end if;

      --  Private types that are completed by a derivation from a private
      --  type have an internally generated full view, that needs to be
      --  frozen. This must be done explicitly because the two views share
      --  the freeze node, and the underlying full view is not visible when
      --  the freeze node is analyzed.

      elsif Is_Private_Type (Def_Id)
        and then Is_Derived_Type (Def_Id)
        and then Present (Full_View (Def_Id))
        and then Is_Itype (Full_View (Def_Id))
        and then Has_Private_Declaration (Full_View (Def_Id))
        and then Freeze_Node (Full_View (Def_Id)) = N
      then
         Set_Entity (N, Full_View (Def_Id));
         Freeze_Type (N);
         Set_Entity (N, Def_Id);
      end if;

   exception
      when RE_Not_Available =>
         return;
   end Freeze_Type;

   -------------------------
   -- Get_Simple_Init_Val --
   -------------------------

   function Get_Simple_Init_Val
     (T    : Entity_Id;
      Loc  : Source_Ptr)
      return Node_Id
   is
      Val    : Node_Id;
      Typ    : Node_Id;
      Result : Node_Id;
      Val_RE : RE_Id;

   begin
      --  For a private type, we should always have an underlying type
      --  (because this was already checked in Needs_Simple_Initialization).
      --  What we do is to get the value for the underlying type and then
      --  do an Unchecked_Convert to the private type.

      if Is_Private_Type (T) then
         Val := Get_Simple_Init_Val (Underlying_Type (T), Loc);

         --  A special case, if the underlying value is null, then qualify
         --  it with the underlying type, so that the null is properly typed
         --  Similarly, if it is an aggregate it must be qualified, because
         --  an unchecked conversion does not provide a context for it.

         if Nkind (Val) = N_Null
           or else Nkind (Val) = N_Aggregate
         then
            Val :=
              Make_Qualified_Expression (Loc,
                Subtype_Mark =>
                  New_Occurrence_Of (Underlying_Type (T), Loc),
                Expression => Val);
         end if;

         Result := Unchecked_Convert_To (T, Val);

         --  Don't truncate result (important for Initialize/Normalize_Scalars)

         if Nkind (Result) = N_Unchecked_Type_Conversion
           and then Is_Scalar_Type (Underlying_Type (T))
         then
            Set_No_Truncation (Result);
         end if;

         return Result;

      --  For scalars, we must have normalize/initialize scalars case

      elsif Is_Scalar_Type (T) then
         pragma Assert (Init_Or_Norm_Scalars);

         --  Processing for Normalize_Scalars case

         if Normalize_Scalars then

            --  First prepare a value (out of subtype range if possible)

            if Is_Real_Type (T) or else Is_Integer_Type (T) then
               Val :=
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Occurrence_Of (Base_Type (T), Loc),
                   Attribute_Name => Name_First);

            elsif Is_Modular_Integer_Type (T) then
               Val :=
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Occurrence_Of (Base_Type (T), Loc),
                   Attribute_Name => Name_Last);

            else
               pragma Assert (Is_Enumeration_Type (T));

               if Esize (T) <= 8 then
                  Typ := RTE (RE_Unsigned_8);
               elsif Esize (T) <= 16 then
                  Typ := RTE (RE_Unsigned_16);
               elsif Esize (T) <= 32 then
                  Typ := RTE (RE_Unsigned_32);
               else
                  Typ := RTE (RE_Unsigned_64);
               end if;

               Val :=
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Occurrence_Of (Typ, Loc),
                   Attribute_Name => Name_Last);
            end if;

         --  Here for Initialize_Scalars case

         else
            if Is_Floating_Point_Type (T) then
               if Root_Type (T) = Standard_Short_Float then
                  Val_RE := RE_IS_Isf;
               elsif Root_Type (T) = Standard_Float then
                  Val_RE := RE_IS_Ifl;
               elsif Root_Type (T) = Standard_Long_Float then
                  Val_RE := RE_IS_Ilf;
               else pragma Assert (Root_Type (T) = Standard_Long_Long_Float);
                  Val_RE := RE_IS_Ill;
               end if;

            elsif Is_Unsigned_Type (Base_Type (T)) then
               if Esize (T) = 8 then
                  Val_RE := RE_IS_Iu1;
               elsif Esize (T) = 16 then
                  Val_RE := RE_IS_Iu2;
               elsif Esize (T) = 32 then
                  Val_RE := RE_IS_Iu4;
               else pragma Assert (Esize (T) = 64);
                  Val_RE := RE_IS_Iu8;
               end if;

            else -- signed type
               if Esize (T) = 8 then
                  Val_RE := RE_IS_Is1;
               elsif Esize (T) = 16 then
                  Val_RE := RE_IS_Is2;
               elsif Esize (T) = 32 then
                  Val_RE := RE_IS_Is4;
               else pragma Assert (Esize (T) = 64);
                  Val_RE := RE_IS_Is8;
               end if;
            end if;

            Val := New_Occurrence_Of (RTE (Val_RE), Loc);
         end if;

         --  The final expression is obtained by doing an unchecked
         --  conversion of this result to the base type of the
         --  required subtype. We use the base type to avoid the
         --  unchecked conversion from chopping bits, and then we
         --  set Kill_Range_Check to preserve the "bad" value.

         Result := Unchecked_Convert_To (Base_Type (T), Val);

         --  Ensure result is not truncated, since we want the "bad" bits
         --  and also kill range check on result.

         if Nkind (Result) = N_Unchecked_Type_Conversion then
            Set_No_Truncation (Result);
            Set_Kill_Range_Check (Result, True);
         end if;

         return Result;

      --  String or Wide_String (must have Initialize_Scalars set)

      elsif Root_Type (T) = Standard_String
              or else
            Root_Type (T) = Standard_Wide_String
      then
         pragma Assert (Init_Or_Norm_Scalars);

         return
           Make_Aggregate (Loc,
             Component_Associations => New_List (
               Make_Component_Association (Loc,
                 Choices => New_List (
                   Make_Others_Choice (Loc)),
                 Expression =>
                   Get_Simple_Init_Val (Component_Type (T), Loc))));

      --  Access type is initialized to null

      elsif Is_Access_Type (T) then
         return
           Make_Null (Loc);

      --  We initialize modular packed bit arrays to zero, to make sure that
      --  unused bits are zero, as required (see spec of Exp_Pakd). Also note
      --  that this improves gigi code, since the value tracing knows that
      --  all bits of the variable start out at zero. The value of zero has
      --  to be unchecked converted to the proper array type.

      elsif Is_Bit_Packed_Array (T) then
         declare
            PAT : constant Entity_Id := Packed_Array_Type (T);
            Nod : Node_Id;

         begin
            pragma Assert (Is_Modular_Integer_Type (PAT));

            Nod :=
              Make_Unchecked_Type_Conversion (Loc,
                Subtype_Mark => New_Occurrence_Of (T, Loc),
                Expression   => Make_Integer_Literal (Loc, 0));

            Set_Etype (Expression (Nod), PAT);
            return Nod;
         end;

      --  No other possibilities should arise, since we should only be
      --  calling Get_Simple_Init_Val if Needs_Simple_Initialization
      --  returned True, indicating one of the above cases held.

      else
         raise Program_Error;
      end if;

   exception
      when RE_Not_Available =>
         return Empty;
   end Get_Simple_Init_Val;

   ------------------------------
   -- Has_New_Non_Standard_Rep --
   ------------------------------

   function Has_New_Non_Standard_Rep (T : Entity_Id) return Boolean is
   begin
      if not Is_Derived_Type (T) then
         return Has_Non_Standard_Rep (T)
           or else Has_Non_Standard_Rep (Root_Type (T));

      --  If Has_Non_Standard_Rep is not set on the derived type, the
      --  representation is fully inherited.

      elsif not Has_Non_Standard_Rep (T) then
         return False;

      else
         return First_Rep_Item (T) /= First_Rep_Item (Root_Type (T));
      end if;
   end Has_New_Non_Standard_Rep;

   ----------------
   -- In_Runtime --
   ----------------

   function In_Runtime (E : Entity_Id) return Boolean is
      S1 : Entity_Id := Scope (E);

   begin
      while Scope (S1) /= Standard_Standard loop
         S1 := Scope (S1);
      end loop;

      return Chars (S1) = Name_System or else Chars (S1) = Name_Ada;
   end In_Runtime;

   ------------------
   -- Init_Formals --
   ------------------

   function Init_Formals (Typ : Entity_Id) return List_Id is
      Loc     : constant Source_Ptr := Sloc (Typ);
      Formals : List_Id;

   begin
      --  First parameter is always _Init : in out typ. Note that we need
      --  this to be in/out because in the case of the task record value,
      --  there are default record fields (_Priority, _Size, -Task_Info)
      --  that may be referenced in the generated initialization routine.

      Formals := New_List (
        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uInit),
          In_Present  => True,
          Out_Present => True,
          Parameter_Type => New_Reference_To (Typ, Loc)));

      --  For task record value, or type that contains tasks, add two more
      --  formals, _Master : Master_Id and _Chain : in out Activation_Chain
      --  We also add these parameters for the task record type case.

      return Formals;

   exception
      when RE_Not_Available =>
         return Empty_List;
   end Init_Formals;

   ------------------
   -- Make_Eq_Case --
   ------------------

   --  <Make_Eq_if shared components>
   --  case X.D1 is
   --     when V1 => <Make_Eq_Case> on subcomponents
   --     ...
   --     when Vn => <Make_Eq_Case> on subcomponents
   --  end case;

   function Make_Eq_Case (Node : Node_Id; CL : Node_Id) return List_Id is
      Loc      : constant Source_Ptr := Sloc (Node);
      Result   : constant List_Id    := New_List;

   begin
      Append_To (Result, Make_Eq_If (Node, Component_Items (CL)));

      return Result;
   end Make_Eq_Case;

   ----------------
   -- Make_Eq_If --
   ----------------

   --  Generates:

   --    if
   --      X.C1 /= Y.C1
   --        or else
   --      X.C2 /= Y.C2
   --        ...
   --    then
   --       return False;
   --    end if;

   --  or a null statement if the list L is empty

   function Make_Eq_If (Node : Node_Id; L : List_Id) return Node_Id is
      Loc        : constant Source_Ptr := Sloc (Node);
      C          : Node_Id;
      Field_Name : Name_Id;
      Cond       : Node_Id;

   begin
      if No (L) then
         return Make_Null_Statement (Loc);

      else
         Cond := Empty;

         C := First_Non_Pragma (L);
         while Present (C) loop
            Field_Name := Chars (Defining_Identifier (C));

            --  The tags must not be compared they are not part of the value.
            --  Note also that in the following, we use Make_Identifier for
            --  the component names. Use of New_Reference_To to identify the
            --  components would be incorrect because the wrong entities for
            --  discriminants could be picked up in the private type case.

            if Field_Name /= Name_uTag then
               Evolve_Or_Else (Cond,
                 Make_Op_Ne (Loc,
                   Left_Opnd =>
                     Make_Selected_Component (Loc,
                       Prefix        => Make_Identifier (Loc, Name_X),
                       Selector_Name =>
                         Make_Identifier (Loc, Field_Name)),

                   Right_Opnd =>
                     Make_Selected_Component (Loc,
                       Prefix        => Make_Identifier (Loc, Name_Y),
                       Selector_Name =>
                         Make_Identifier (Loc, Field_Name))));
            end if;

            Next_Non_Pragma (C);
         end loop;

         if No (Cond) then
            return Make_Null_Statement (Loc);

         else
            return
              Make_Implicit_If_Statement (Node,
                Condition => Cond,
                Then_Statements => New_List (
                  Make_Return_Statement (Loc,
                    Expression => New_Occurrence_Of (Standard_False, Loc))));
         end if;
      end if;
   end Make_Eq_If;

   -------------------------------------
   -- Make_Predefined_Primitive_Specs --
   -------------------------------------

   procedure Make_Predefined_Primitive_Specs
     (Tag_Typ     : Entity_Id;
      Predef_List : out List_Id;
      Renamed_Eq  : out Node_Id)
   is
      Loc       : constant Source_Ptr := Sloc (Tag_Typ);
      Res       : constant List_Id    := New_List;
      Eq_Name   : Name_Id := Name_Op_Eq;

      function Is_Predefined_Eq_Renaming (Prim : Node_Id) return Boolean;
      --  Returns true if Prim is a renaming of an unresolved predefined
      --  equality operation.

      -------------------------------
      -- Is_Predefined_Eq_Renaming --
      -------------------------------

      function Is_Predefined_Eq_Renaming (Prim : Node_Id) return Boolean is
      begin
         return Chars (Prim) /= Name_Op_Eq
           and then Present (Alias (Prim))
           and then Comes_From_Source (Prim)
           and then Is_Intrinsic_Subprogram (Alias (Prim))
           and then Chars (Alias (Prim)) = Name_Op_Eq;
      end Is_Predefined_Eq_Renaming;

   --  Start of processing for Make_Predefined_Primitive_Specs

   begin
      Renamed_Eq := Empty;

      --  Spec of _Alignment

      Append_To (Res, Predef_Spec_Or_Body (Loc,
        Tag_Typ => Tag_Typ,
        Name    => Name_uAlignment,
        Profile => New_List (
          Make_Parameter_Specification (Loc,
            Defining_Identifier => Make_Defining_Identifier (Loc, Name_X),
            Parameter_Type      => New_Reference_To (Tag_Typ, Loc))),

        Ret_Type => Standard_Integer));

      --  Spec of _Size

      Append_To (Res, Predef_Spec_Or_Body (Loc,
        Tag_Typ => Tag_Typ,
        Name    => Name_uSize,
        Profile => New_List (
          Make_Parameter_Specification (Loc,
            Defining_Identifier => Make_Defining_Identifier (Loc, Name_X),
            Parameter_Type      => New_Reference_To (Tag_Typ, Loc))),

        Ret_Type => Standard_Long_Long_Integer));

         --  Spec for dispatching assignment

         Append_To (Res, Predef_Spec_Or_Body (Loc,
           Tag_Typ => Tag_Typ,
           Name    => Name_uAssign,
           Profile => New_List (
             Make_Parameter_Specification (Loc,
               Defining_Identifier => Make_Defining_Identifier (Loc, Name_X),
               Out_Present         => True,
               Parameter_Type      => New_Reference_To (Tag_Typ, Loc)),

             Make_Parameter_Specification (Loc,
               Defining_Identifier => Make_Defining_Identifier (Loc, Name_Y),
               Parameter_Type      => New_Reference_To (Tag_Typ, Loc)))));

      --  Specs for finalization actions that may be required in case a
      --  future extension contain a controlled element. We generate those
      --  only for root tagged types where they will get dummy bodies or
      --  when the type has controlled components and their body must be
      --  generated. It is also impossible to provide those for tagged
      --  types defined within s-finimp since it would involve circularity
      --  problems

      --  We also skip these if finalization is not available

      Predef_List := Res;
   end Make_Predefined_Primitive_Specs;

   ---------------------------------
   -- Needs_Simple_Initialization --
   ---------------------------------

   function Needs_Simple_Initialization (T : Entity_Id) return Boolean is
   begin
      --  Check for private type, in which case test applies to the
      --  underlying type of the private type.

      if Is_Private_Type (T) then
         declare
            RT : constant Entity_Id := Underlying_Type (T);

         begin
            if Present (RT) then
               return Needs_Simple_Initialization (RT);
            else
               return False;
            end if;
         end;

      --  Cases needing simple initialization are access types, and, if pragma
      --  Normalize_Scalars or Initialize_Scalars is in effect, then all scalar
      --  types.

      elsif Is_Access_Type (T)
        or else (Init_Or_Norm_Scalars and then (Is_Scalar_Type (T)))

        or else (Is_Bit_Packed_Array (T)
                   and then Is_Modular_Integer_Type (Packed_Array_Type (T)))
      then
         return True;

      --  If Initialize/Normalize_Scalars is in effect, string objects also
      --  need initialization, unless they are created in the course of
      --  expanding an aggregate (since in the latter case they will be
      --  filled with appropriate initializing values before they are used).

      elsif Init_Or_Norm_Scalars
        and then
          (Root_Type (T) = Standard_String
            or else Root_Type (T) = Standard_Wide_String)
        and then
          (not Is_Itype (T)
            or else Nkind (Associated_Node_For_Itype (T)) /= N_Aggregate)
      then
         return True;

      else
         return False;
      end if;
   end Needs_Simple_Initialization;

   ----------------------
   -- Predef_Deep_Spec --
   ----------------------

   function Predef_Deep_Spec
     (Loc      : Source_Ptr;
      Tag_Typ  : Entity_Id;
      Name     : TSS_Name_Type;
      For_Body : Boolean := False)
      return     Node_Id
   is
      Prof   : List_Id;
      Type_B : Entity_Id;

   begin
      if Name = TSS_Deep_Finalize then
         Prof := New_List;
         Type_B := Standard_Boolean;

      else
         Prof := New_List (
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_L),
             In_Present          => True,
             Out_Present         => True,
             Parameter_Type      =>
               New_Reference_To (RTE (RE_Finalizable_Ptr), Loc)));
         Type_B := Standard_Short_Short_Integer;
      end if;

      Append_To (Prof,
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
             In_Present          => True,
             Out_Present         => True,
             Parameter_Type      => New_Reference_To (Tag_Typ, Loc)));

      Append_To (Prof,
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_B),
             Parameter_Type      => New_Reference_To (Type_B, Loc)));

      return Predef_Spec_Or_Body (Loc,
        Name     => Make_TSS_Name (Tag_Typ, Name),
        Tag_Typ  => Tag_Typ,
        Profile  => Prof,
        For_Body => For_Body);

   exception
      when RE_Not_Available =>
         return Empty;
   end Predef_Deep_Spec;

   -------------------------
   -- Predef_Spec_Or_Body --
   -------------------------

   function Predef_Spec_Or_Body
     (Loc      : Source_Ptr;
      Tag_Typ  : Entity_Id;
      Name     : Name_Id;
      Profile  : List_Id;
      Ret_Type : Entity_Id := Empty;
      For_Body : Boolean := False)
      return     Node_Id
   is
      Id   : constant Entity_Id := Make_Defining_Identifier (Loc, Name);
      Spec : Node_Id;

   begin
      Set_Is_Public (Id, Is_Public (Tag_Typ));

      --  The internal flag is set to mark these declarations because
      --  they have specific properties. First they are primitives even
      --  if they are not defined in the type scope (the freezing point
      --  is not necessarily in the same scope), furthermore the
      --  predefined equality can be overridden by a user-defined
      --  equality, no body will be generated in this case.

      Set_Is_Internal (Id);

      if not Debug_Generated_Code then
         Set_Debug_Info_Off (Id);
      end if;

      if No (Ret_Type) then
         Spec :=
           Make_Procedure_Specification (Loc,
             Defining_Unit_Name       => Id,
             Parameter_Specifications => Profile);
      else
         Spec :=
           Make_Function_Specification (Loc,
             Defining_Unit_Name       => Id,
             Parameter_Specifications => Profile,
             Subtype_Mark             =>
               New_Reference_To (Ret_Type, Loc));
      end if;

      --  If body case, return empty subprogram body. Note that this is
      --  ill-formed, because there is not even a null statement, and
      --  certainly not a return in the function case. The caller is
      --  expected to do surgery on the body to add the appropriate stuff.

      if For_Body then
         return Make_Subprogram_Body (Loc, Spec, Empty_List, Empty);

      --  Normal spec case, where we return a subprogram declaration

      else
         return Make_Subprogram_Declaration (Loc, Spec);
      end if;
   end Predef_Spec_Or_Body;

   ---------------------------------
   -- Predefined_Primitive_Bodies --
   ---------------------------------

   function Predefined_Primitive_Bodies
     (Tag_Typ    : Entity_Id;
      Renamed_Eq : Node_Id)
      return       List_Id
   is
      Loc       : constant Source_Ptr := Sloc (Tag_Typ);
      Res       : constant List_Id    := New_List;
      Decl      : Node_Id;
      Prim      : Elmt_Id;
      Eq_Needed : Boolean;
      Eq_Name   : Name_Id;

   begin
      --  See if we have a predefined "=" operator

      if Present (Renamed_Eq) then
         Eq_Needed := True;
         Eq_Name   := Chars (Renamed_Eq);

      else
         Eq_Needed := False;
         Eq_Name   := No_Name;

         Prim := First_Elmt (Primitive_Operations (Tag_Typ));
         while Present (Prim) loop
            if Chars (Node (Prim)) = Name_Op_Eq
              and then Is_Internal (Node (Prim))
            then
               Eq_Needed := True;
               Eq_Name := Name_Op_Eq;
            end if;

            Next_Elmt (Prim);
         end loop;
      end if;

      --  Body of _Alignment

      Decl := Predef_Spec_Or_Body (Loc,
        Tag_Typ => Tag_Typ,
        Name    => Name_uAlignment,
        Profile => New_List (
          Make_Parameter_Specification (Loc,
            Defining_Identifier => Make_Defining_Identifier (Loc, Name_X),
            Parameter_Type      => New_Reference_To (Tag_Typ, Loc))),

        Ret_Type => Standard_Integer,
        For_Body => True);

      Set_Handled_Statement_Sequence (Decl,
        Make_Handled_Sequence_Of_Statements (Loc, New_List (
          Make_Return_Statement (Loc,
            Expression =>
              Make_Attribute_Reference (Loc,
                Prefix => Make_Identifier (Loc, Name_X),
                Attribute_Name  => Name_Alignment)))));

      Append_To (Res, Decl);

      --  Body of _Size

      Decl := Predef_Spec_Or_Body (Loc,
        Tag_Typ => Tag_Typ,
        Name    => Name_uSize,
        Profile => New_List (
          Make_Parameter_Specification (Loc,
            Defining_Identifier => Make_Defining_Identifier (Loc, Name_X),
            Parameter_Type      => New_Reference_To (Tag_Typ, Loc))),

        Ret_Type => Standard_Long_Long_Integer,
        For_Body => True);

      Set_Handled_Statement_Sequence (Decl,
        Make_Handled_Sequence_Of_Statements (Loc, New_List (
          Make_Return_Statement (Loc,
            Expression =>
              Make_Attribute_Reference (Loc,
                Prefix => Make_Identifier (Loc, Name_X),
                Attribute_Name  => Name_Size)))));

      Append_To (Res, Decl);

         --  Body for equality

         if Eq_Needed then

            Decl := Predef_Spec_Or_Body (Loc,
              Tag_Typ => Tag_Typ,
              Name    => Eq_Name,
              Profile => New_List (
                Make_Parameter_Specification (Loc,
                  Defining_Identifier =>
                    Make_Defining_Identifier (Loc, Name_X),
                  Parameter_Type      => New_Reference_To (Tag_Typ, Loc)),

                Make_Parameter_Specification (Loc,
                  Defining_Identifier =>
                    Make_Defining_Identifier (Loc, Name_Y),
                  Parameter_Type      => New_Reference_To (Tag_Typ, Loc))),

              Ret_Type => Standard_Boolean,
              For_Body => True);

            declare
               Def          : constant Node_Id := Parent (Tag_Typ);
               Stmts        : constant List_Id := New_List;
               Comps        : Node_Id := Empty;
               Typ_Def      : Node_Id := Type_Definition (Def);

            begin
                  Append_To (Stmts,
                    Make_Return_Statement (Loc,
                      Expression =>
                        Expand_Record_Equality (Tag_Typ,
                          Typ => Tag_Typ,
                          Lhs => Make_Identifier (Loc, Name_X),
                          Rhs => Make_Identifier (Loc, Name_Y),
                          Bodies => Declarations (Decl))));

               Set_Handled_Statement_Sequence (Decl,
                 Make_Handled_Sequence_Of_Statements (Loc, Stmts));
            end;
            Append_To (Res, Decl);
         end if;

         --  Body for dispatching assignment

         Decl := Predef_Spec_Or_Body (Loc,
           Tag_Typ => Tag_Typ,
           Name    => Name_uAssign,
           Profile => New_List (
             Make_Parameter_Specification (Loc,
               Defining_Identifier => Make_Defining_Identifier (Loc, Name_X),
               Out_Present         => True,
               Parameter_Type      => New_Reference_To (Tag_Typ, Loc)),

             Make_Parameter_Specification (Loc,
               Defining_Identifier => Make_Defining_Identifier (Loc, Name_Y),
               Parameter_Type      => New_Reference_To (Tag_Typ, Loc))),
           For_Body => True);

         Set_Handled_Statement_Sequence (Decl,
           Make_Handled_Sequence_Of_Statements (Loc, New_List (
             Make_Assignment_Statement (Loc,
                                             Name       => Make_Identifier (Loc, Name_X),
               Expression => Make_Identifier (Loc, Name_Y)))));

         Append_To (Res, Decl);

         --  Generate dummy bodies for finalization actions of types that have
      --  no controlled components.

      --  Skip this if finalization is not available

         null;

      return Res;
   end Predefined_Primitive_Bodies;

   ---------------------------------
   -- Predefined_Primitive_Freeze --
   ---------------------------------

   function Predefined_Primitive_Freeze
     (Tag_Typ : Entity_Id) return List_Id
   is
      Loc     : constant Source_Ptr := Sloc (Tag_Typ);
      Res     : constant List_Id    := New_List;
      Prim    : Elmt_Id;
      Frnodes : List_Id;

   begin
      Prim := First_Elmt (Primitive_Operations (Tag_Typ));
      while Present (Prim) loop
         if Is_Internal (Node (Prim)) then
            Frnodes := Freeze_Entity (Node (Prim), Loc);

            if Present (Frnodes) then
               Append_List_To (Res, Frnodes);
            end if;
         end if;

         Next_Elmt (Prim);
      end loop;

      return Res;
   end Predefined_Primitive_Freeze;
end Exp_Ch3;
