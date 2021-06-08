
--  Package containing utility procedures used throughout the semantics

with Einfo;  use Einfo;
with Types;  use Types;
with Namet; use Namet;
with Uintp;  use Uintp;
with Urealp; use Urealp;
with Snames; use Snames;

package Sem_Util is

   procedure Add_Access_Type_To_Process (E : Entity_Id; A : Entity_Id);
   --  Add A to the list of access types to process when expanding the
   --  freeze node of E.

   function Alignment_In_Bits (E : Entity_Id) return Uint;
   --  If the alignment of the type or object E is currently known to the
   --  compiler, then this function returns the alignment value in bits.
   --  Otherwise Uint_0 is returned, indicating that the alignment of the
   --  entity is not yet known to the compiler.

   procedure Apply_Compile_Time_Constraint_Error
     (N      : Node_Id;
      Msg    : String;
      Reason : RT_Exception_Code;
      Ent    : Entity_Id  := Empty;
      Typ    : Entity_Id  := Empty;
      Loc    : Source_Ptr := No_Location;
      Rep    : Boolean    := True;
      Warn   : Boolean    := False);
   --  N is a subexpression which will raise constraint error when evaluated
   --  at runtime. Msg is a message that explains the reason for raising the
   --  exception. The last character is ? if the message is always a warning,
   --  even in Ada 95, and is not a ? if the message represents an illegality
   --  (because of violation of static expression rules) in Ada 95 (but not
   --  in Ada 83). Typically this routine posts all messages at the Sloc of
   --  node N. However, if Loc /= No_Location, Loc is the Sloc used to output
   --  the message. After posting the appropriate message, and if the flag
   --  Rep is set, this routine replaces the expression with an appropriate
   --  N_Raise_Constraint_Error node using the given Reason code. This node
   --  is then marked as being static if the original node is static, but
   --  sets the flag Raises_Constraint_Error, preventing further evaluation.
   --  The error message may contain a } or & insertion character. This
   --  normally references Etype (N), unless the Ent argument is given
   --  explicitly, in which case it is used instead. The type of the raise
   --  node that is built is normally Etype (N), but if the Typ parameter
   --  is present, this is used instead. Warn is normally False. If it is
   --  True then the message is treated as a warning even though it does
   --  not end with a ? (this is used when the caller wants to parametrize
   --  whether an error or warning is given.

   function Build_Actual_Subtype
     (T : Entity_Id;
      N : Node_Or_Entity_Id) return Node_Id;
   --  Build an anonymous subtype for an entity or expression, using the
   --  bounds of the entity or the discriminants of the enclosing record.
   --  T is the type for which the actual subtype is required, and N is either
   --  a defining identifier, or any subexpression.

   function Build_Actual_Subtype_Of_Component
     (T : Entity_Id;
      N : Node_Id) return Node_Id;
   --  Determine whether a selected component has a type that depends on
   --  discriminants, and build actual subtype for it if so.

   procedure Build_Elaboration_Entity (N : Node_Id; Spec_Id : Entity_Id);
   --  Given a compilation unit node N, allocate an elaboration boolean for
   --  the compilation unit, and install it in the Elaboration_Entity field
   --  of Spec_Id, the entity for the compilation unit.

   function Cannot_Raise_Constraint_Error (Expr : Node_Id) return Boolean;
   --  Returns True if the expression cannot possibly raise Constraint_Error.
   --  The response is conservative in the sense that a result of False does
   --  not necessarily mean that CE could be raised, but a response of True
   --  means that for sure CE cannot be raised.

   procedure Check_Fully_Declared (T : Entity_Id; N : Node_Id);
   --  Verify that the full declaration of type T has been seen. If not,
   --  place error message on node N. Used in  object declarations, type
   --  conversions, qualified expressions.

   function Collect_Primitive_Operations (T : Entity_Id) return Elist_Id;
   --  Called upon type derivation and extension. We scan the declarative
   --  part in  which the type appears, and collect subprograms that have
   --  one subsidiary subtype of the type. These subprograms can only
   --  appear after the type itself.

   function Compile_Time_Constraint_Error
     (N    : Node_Id;
      Msg  : String;
      Ent  : Entity_Id  := Empty;
      Loc  : Source_Ptr := No_Location;
      Warn : Boolean    := False) return Node_Id;
   --  Subsidiary to Apply_Compile_Time_Constraint_Error and Checks routines.
   --  Does not modify any nodes, but generates a warning (or error) message.
   --  For convenience, the function always returns its first argument. The
   --  message is a warning if the message ends with ?, or we are operating
   --  in Ada 83 mode, or if the Warn parameter is set to True.

   procedure Conditional_Delay (New_Ent, Old_Ent : Entity_Id);
   --  Sets the Has_Delayed_Freeze flag of New if the Delayed_Freeze flag
   --  of Old is set and Old has no yet been Frozen (i.e. Is_Frozen is false);

   function Current_Entity (N : Node_Id) return Entity_Id;
   --  Find the currently visible definition for a given identifier, that is to
   --  say the first entry in the visibility chain for the Chars of N.

   function Current_Entity_In_Scope (N : Node_Id) return Entity_Id;
   --  Find whether there is a previous definition for identifier N in the
   --  current scope. Because declarations for a scope are not necessarily
   --  contiguous (e.g. for packages) the first entry on the visibility chain
   --  for N is not necessarily in the current scope.

   function Current_Scope return Entity_Id;
   --  Get entity representing current scope

   function Current_Subprogram return Entity_Id;
   --  Returns current enclosing subprogram. If Current_Scope is a subprogram,
   --  then that is what is returned, otherwise the Enclosing_Subprogram of
   --  the Current_Scope is returned. The returned value is Empty if this
   --  is called from a library package which is not within any subprogram.

   function Defining_Entity (N : Node_Id) return Entity_Id;
   --  Given a declaration N, returns the associated defining entity. If
   --  the declaration has a specification, the entity is obtained from
   --  the specification. If the declaration has a defining unit name,
   --  then the defining entity is obtained from the defining unit name
   --  ignoring any child unit prefixes.

   function Designate_Same_Unit
     (Name1 : Node_Id;
      Name2 : Node_Id) return  Boolean;
   --  Return true if Name1 and Name2 designate the same unit name;
   --  each of these names is supposed to be a selected component name,
   --  an expanded name, a defining program unit name or an identifier

   function Enclosing_Comp_Unit_Node (N : Node_Id) return Node_Id;
   --  Returns the enclosing N_Compilation_Unit node that is the root of a
   --  subtree containing N.

   function Enclosing_Declaration (N : Node_Id) return Node_Id;
   --  Returns the declaration node enclosing N (including possibly N itself),
   --  if any, or Empty otherwise.

   function Enclosing_Generic_Unit
     (N : Node_Id) return Node_Id;
   --  Returns the Node_Id associated with the innermost enclosing generic
   --  unit, if any. If none, then returns Empty.

   function Enclosing_Package (E : Entity_Id) return Entity_Id;
   --  Utility function to return the Ada entity of the package enclosing
   --  the entity E, if any. Returns Empty if no enclosing package.

   function Enclosing_Package_Or_Subprogram (E : Entity_Id) return Entity_Id;
   --  Returns the entity of the package or subprogram enclosing E, if any.
   --  Returns Empty if no enclosing package or subprogram.

   function Enclosing_Generic_Body
     (E : Entity_Id) return Node_Id;
   --  Returns the Node_Id associated with the innermost enclosing
   --  generic body, if any. If none, then returns Empty.

   function Enclosing_Lib_Unit_Entity
      (E : Entity_Id := Current_Scope) return Entity_Id;
   --  Returns the entity of enclosing N_Compilation_Unit Node which is the
   --  root of the current scope (which must not be Standard_Standard, and
   --  the caller is responsible for ensuring this condition).

   function Enclosing_Lib_Unit_Node (N : Node_Id) return Node_Id;
   --  Returns the enclosing N_Compilation_Unit Node that is the root
   --  of a subtree containing N.

   function Enclosing_Subprogram (E : Entity_Id) return Entity_Id;
   --  Utility function to return the Ada entity of the subprogram enclosing
   --  the entity E, if any. Returns Empty if no enclosing subprogram.

   procedure Ensure_Freeze_Node (E : Entity_Id);
   --  Make sure a freeze node is allocated for entity E. If necessary,
   --  build and initialize a new freeze node and set Has_Delayed_Freeze
   --  true for entity E.

   procedure Enter_Name (Def_Id : Node_Id);
   --  Insert new name in symbol table of current scope with check for
   --  duplications (error message is issued if a conflict is found)
   --  Note: Enter_Name is not used for overloadable entities, instead
   --  these are entered using Sem_Ch6.Enter_Overloadable_Entity.

   function First_Actual (Node : Node_Id) return Node_Id;
   --  Node is an N_Function_Call or N_Procedure_Call_Statement node. The
   --  result returned is the first actual parameter in declaration order
   --  (not the order of parameters as they appeared in the source, which
   --  can be quite different as a result of the use of named parameters).
   --  Empty is returned for a call with no parameters. The procedure for
   --  iterating through the actuals in declaration order is to use this
   --  function to find the first actual, and then use Next_Actual to obtain
   --  the next actual in declaration order. Note that the value returned
   --  is always the expression (not the N_Parameter_Association nodes
   --  even if named association is used).

   function Full_Qualified_Name (E : Entity_Id) return String_Id;
   --  Generates the string literal corresponding to the E's full qualified
   --  name in upper case. An ASCII.NUL is appended as the last character

   function Find_Static_Alternative (N : Node_Id) return Node_Id;
   --  N is a case statement whose expression is a compile-time value.
   --  Determine the alternative chosen, so that the code of non-selected
   --  alternatives, and the warnings that may apply to them, are removed.

   procedure Gather_Components
     (Comp_List : Node_Id;
      Into      : Elist_Id);
   --  The purpose of this procedure is to gather the valid components
   --  in a record type according to the values of its discriminants, in order
   --  to validate the components of a record aggregate.
   --
   --    Typ is the type of the aggregate when its constrained discriminants
   --      need to be collected, otherwise it is Empty.
   --
   --    Comp_List is an N_Component_List node.
   --
   --    Governed_By is a list of N_Component_Association nodes,
   --     where each choice list contains the name of a discriminant and
   --     the expression field gives its value. The values of the
   --     discriminants governing the (possibly nested) variant parts in
   --     Comp_List are found in this Component_Association List.
   --
   --    Into is the list where the valid components are appended.
   --     Note that Into need not be an Empty list. If it's not, components
   --     are attached to its tail.
   --
   --    Report_Errors is set to True if the values of the discriminants
   --     are non-static.

   --  This procedure is also used when building a record subtype. If the
   --  discriminant constraint of the subtype is static, the components of the
   --  subtype are only those of the variants selected by the values of the
   --  discriminants. Otherwise all components of the parent must be included
   --  in the subtype for semantic analysis.

   function Get_Actual_Subtype (N : Node_Id) return Entity_Id;
   --  Given a node for an expression, obtain the actual subtype of the
   --  expression. In the case of a parameter where the formal is an
   --  unconstrained array or discriminated type, this will be the
   --  previously constructed subtype of the actual. Note that this is
   --  not quite the "Actual Subtype" of the RM, since it is always
   --  a constrained type, i.e. it is the subtype of the value of the
   --  actual. The actual subtype is also returned in other cases where
   --  it has already been constructed for an object. Otherwise the
   --  expression type is returned unchanged, except for the case of an
   --  unconstrained array type, where an actual subtype is created, using
   --  Insert_Actions if necessary to insert any associated actions.

   function Get_Actual_Subtype_If_Available (N : Node_Id) return Entity_Id;
   --  This is like Get_Actual_Subtype, except that it never constructs an
   --  actual subtype. If an actual subtype is already available, i.e. the
   --  Actual_Subtype field of the corresponding entity is set, then it is
   --  returned. Otherwise the Etype of the node is returned.

   function Get_Default_External_Name (E : Node_Or_Entity_Id) return Node_Id;
   --  This is used to construct the string literal node representing a
   --  default external name, i.e. one that is constructed from the name
   --  of an entity, or (in the case of extended DEC import/export pragmas,
   --  an identifier provided as the external name. Letters in the name are
   --  according to the setting of Opt.External_Name_Default_Casing.

   function Get_Generic_Entity (N : Node_Id) return Entity_Id;
   --  Returns the true generic entity in an instantiation. If the name in
   --  the instantiation is a renaming, the function returns the renamed
   --  generic.

   procedure Get_Index_Bounds (N : Node_Id; L, H : out Node_Id);
   --  This procedure assigns to L and H respectively the values of the
   --  low and high bounds of node N, which must be a range, subtype
   --  indication, or the name of a scalar subtype. The result in L, H
   --  may be set to Error if there was an earlier error in the range.

   function Get_Enum_Lit_From_Pos
     (T   : Entity_Id;
      Pos : Uint;
      Loc : Source_Ptr) return Entity_Id;
   --  This function obtains the E_Enumeration_Literal entity for the
   --  specified value from the enumneration type or subtype T. The
   --  second argument is the Pos value, which is assumed to be in range.
   --  The third argument supplies a source location for constructed
   --  nodes returned by this function.

   function Get_Name_Entity_Id (Id : Name_Id) return Entity_Id;
   --  An entity value is associated with each name in the name table. The
   --  Get_Name_Entity_Id function fetches the Entity_Id of this entity,
   --  which is the innermost visible entity with the given name. See the
   --  body of Sem_Ch8 for further details on handling of entity visibility.

   function Get_Referenced_Object (N : Node_Id) return Node_Id;
   --  Given a node, return the renamed object if the node represents
   --  a renamed object, otherwise return the node unchanged. The node
   --  may represent an arbitrary expression.

   function Get_Subprogram_Body (E : Entity_Id) return Node_Id;
   --  Given the entity for a subprogram (E_Function or E_Procedure),
   --  return the corresponding N_Subprogram_Body node. If the corresponding
   --  body of the declaration is missing (as for an imported subprogram)
   --  return Empty.

   function Has_Infinities (E : Entity_Id) return Boolean;
   --  Determines if the range of the floating-point type E includes
   --  infinities. Returns False if E is not a floating-point type.

   function Has_Private_Component (Type_Id : Entity_Id) return Boolean;
   --  Check if a type has a (sub)component of a private type that has not
   --  yet received a full declaration.

   function Has_Tagged_Component (Typ : Entity_Id) return Boolean;
   --  Typ must be a composite type (array or record). This function is used
   --  to check if '=' has to be expanded into a bunch component comparaisons.

   function In_Instance return Boolean;
   --  Returns True if the current scope is within a generic instance.

   function In_Instance_Body return Boolean;
   --  Returns True if current scope is within the body of an instance, where
   --  several semantic checks (e.g. accessibility checks) are relaxed.

   function In_Instance_Not_Visible return Boolean;
   --  Returns True if current scope is with the private part or the body of
   --  an instance. Other semantic checks are suppressed in this context.

   function In_Instance_Visible_Part return Boolean;
   --  Returns True if current scope is within the visible part of a package
   --  instance, where several additional semantic checks apply.

   function In_Package_Body return Boolean;
   --  Returns True if current scope is within a package body

   function In_Subprogram_Or_Concurrent_Unit return Boolean;
   --  Determines if the current scope is within a subprogram compilation
   --  unit (inside a subprogram declaration, subprogram body, or generic
   --  subprogram declaration) or within a task or protected body. The test
   --  is for appearing anywhere within such a construct (that is it does not
   --  need to be directly within).

   function In_Visible_Part (Scope_Id : Entity_Id) return Boolean;
   --  Determine whether a declaration occurs within the visible part of a
   --  package specification. The package must be on the scope stack, and the
   --  corresponding private part must not.

   procedure Insert_Explicit_Dereference (N : Node_Id);
   --  In a context that requires a composite or subprogram type and
   --  where a prefix is an access type, rewrite the access type node
   --  N (which is the prefix, e.g. of an indexed component) as an
   --  explicit dereference.

   function Is_AAMP_Float (E : Entity_Id) return Boolean;
   --  Defined for all type entities. Returns True only for the base type
   --  of float types with AAMP format. The particular format is determined
   --  by the Digits_Value value which is 6 for the 32-bit floating point type,
   --  or 9 for the 48-bit type. This is not an attribute function (like
   --  VAX_Float) in order to not use up an extra flag and to prevent
   --  the dependency of Einfo on Targparm which would be required for a
   --  synthesized attribute.

   function Is_Actual_Parameter (N : Node_Id) return Boolean;
   --  Determines if N is an actual parameter in a subprogram call.

   function Is_Aliased_View (Obj : Node_Id) return Boolean;
   --  Determine if Obj is an aliased view, i.e. the name of an
   --  object to which 'Access or 'Unchecked_Access can apply.

--   function Is_Atomic_Object (N : Node_Id) return Boolean;
   --  Determines if the given node denotes an atomic object in the sense
   --  of the legality checks described in RM C.6(12).

   function Is_Dependent_Component_Of_Mutable_Object
     (Object : Node_Id) return Boolean;
   --  Returns True if Object is the name of a subcomponent that
   --  depends on discriminants of a variable whose nominal subtype
   --  is unconstrained and not indefinite, and the variable is
   --  not aliased.  Otherwise returns False.  The nodes passed
   --  to this function are assumed to denote objects.

   function Is_Dereferenced (N : Node_Id) return Boolean;
   --  N is a subexpression node of an access type. This function returns
   --  true if N appears as the prefix of a node that does a dereference
   --  of the access value (selected/indexed component, explicit dereference
   --  or a slice), and false otherwise.

   function Is_False (U : Uint) return Boolean;
   --  The argument is a Uint value which is the Boolean'Pos value of a
   --  Boolean operand (i.e. is either 0 for False, or 1 for True). This
   --  function simply tests if it is False (i.e. zero)

   function Is_Fully_Initialized_Type (Typ : Entity_Id) return Boolean;
   --  Typ is a type entity. This function returns true if this type is
   --  fully initialized, meaning that an object of the type is fully
   --  initialized. Note that initialization resulting from the use of
   --  pragma Normalized_Scalars does not count. Note that this is only
   --  used for the purpose of issuing warnings for objects that are
   --  potentially referenced uninitialized. This means that the result
   --  returned is not crucial, but probably should err on the side of
   --  thinking things are fully initialized if it does not know.

   function Is_Inherited_Operation (E : Entity_Id) return Boolean;
   --  E is a subprogram. Return True is E is an implicit operation inherited
   --  by a derived type declarations.

   function Is_Lvalue (N : Node_Id) return Boolean;
   --  Determines if N could be an lvalue (e.g. an assignment left hand side).
   --  This determination is conservative, it must never answer False if N is
   --  an lvalue, but it can answer True when N is not an lvalue. An lvalue is
   --  defined as any expression which appears in a context where a name is
   --  required by the syntax, and the identity, rather than merely the value
   --  of the node is needed (for example, the prefix of an attribute is in
   --  this category).

   function Is_Library_Level_Entity (E : Entity_Id) return Boolean;
   --  A library-level declaration is one that is accessible from Standard,
   --  i.e. a library unit or an entity declared in a library package.

   function Is_Local_Variable_Reference (Expr : Node_Id) return Boolean;
   --  Determines whether Expr is a refeference to a variable or IN OUT
   --  mode parameter of the current enclosing subprogram.

   function Is_Object_Reference (N : Node_Id) return Boolean;
   --  Determines if the tree referenced by N represents an object. Both
   --  variable and constant objects return True (compare Is_Variable).

   function Is_OK_Variable_For_Out_Formal (AV : Node_Id) return Boolean;
   --  Used to test if AV is an acceptable formal for an OUT or IN OUT
   --  formal. Note that the Is_Variable function is not quite the right
   --  test because this is a case in which conversions whose expression
   --  is a variable (in the Is_Variable sense) with a non-tagged type
   --  target are considered view conversions and hence variables.

   function Is_Partially_Initialized_Type (Typ : Entity_Id) return Boolean;
   --  Typ is a type entity. This function returns true if this type is
   --  partly initialized, meaning that an object of the type is at least
   --  partly initialized (in particular in the record case, that at least
   --  one field has an initialization expression). Note that initialization
   --  resulting from the use of pragma Normalized_Scalars does not count.

   function Is_RCI_Pkg_Spec_Or_Body (Cunit : Node_Id) return Boolean;
   --  Return True if a compilation unit is the specification or the
   --  body of a remote call interface package.

   function Is_Selector_Name (N : Node_Id) return Boolean;
   --  Given an N_Identifier node N, determines if it is a Selector_Name.
   --  As described in Sinfo, Selector_Names are special because they
   --  represent use of the N_Identifier node for a true identifer, when
   --  normally such nodes represent a direct name.

   function Is_Statement (N : Node_Id) return Boolean;
   --  Check if the node N is a statement node. Note that this includes
   --  the case of procedure call statements (unlike the direct use of
   --  the N_Statement_Other_Than_Procedure_Call subtype from Sinfo)

   function Is_Transfer (N : Node_Id) return Boolean;
   --  Returns True if the node N is a statement which is known to cause
   --  an unconditional transfer of control at runtime, i.e. the following
   --  statement definitely will not be executed.

   function Is_True (U : Uint) return Boolean;
   --  The argument is a Uint value which is the Boolean'Pos value of a
   --  Boolean operand (i.e. is either 0 for False, or 1 for True). This
   --  function simply tests if it is True (i.e. non-zero)

   function Is_Variable (N : Node_Id) return Boolean;
   --  Determines if the tree referenced by N represents a variable, i.e.
   --  can appear on the left side of an assignment. There is one situation,
   --  namely formal parameters, in which non-tagged type conversions are
   --  also considered variables, but Is_Variable returns False for such
   --  cases, since it has no knowledge of the context. Note that this is
   --  the point at which Assignment_OK is checked, and True is returned
   --  for any tree thus marked.

   function Is_Volatile_Object (N : Node_Id) return Boolean;
   --  Determines if the given node denotes an volatile object in the sense
   --  of the legality checks described in RM C.6(12). Note that the test
   --  here is for something actually declared as volatile, not for an object
   --  that gets treated as volatile (see Einfo.Treat_As_Volatile).

   procedure Kill_Current_Values;
   --  This procedure is called to clear all constant indications from all
   --  entities in the current scope and in any parent scopes if the current
   --  scope is a block or a pacakage (and that recursion continues to the
   --  top scope that is not a block or a package). This is used when the
   --  sequential flow-of-control assumption is violated (occurence of a
   --  label, head of a loop, or start of an exception handler). The effect
   --  of the call is to clear the Constant_Value field (but we do not need
   --  to clear the Is_True_Constant flag, since that only gets reset if
   --  there really is an assignment somewhere in the entity scope). This
   --  procedure also calls Kill_All_Checks, since this is a special case
   --  of needing to forget saved values. This procedure also clears any
   --  Is_Known_Non_Null flags in variables, constants or parameters
   --  since these are also not known to be valid.

   procedure Kill_Size_Check_Code (E : Entity_Id);
   --  Called when an address clause or pragma Import is applied to an
   --  entity. If the entity is a variable or a constant, and size check
   --  code is present, this size check code is killed, since the object
   --  will not be allocated by the program.

   function New_Copy_List_Tree (List : List_Id) return List_Id;
   --  Copy recursively an analyzed list of nodes. Uses New_Copy_Tree defined
   --  below. As for New_Copy_Tree, it is illegal to attempt to copy extended
   --  nodes (entities) either directly or indirectly using this function.

   function New_Copy_Tree
     (Source    : Node_Id;
      Map       : Elist_Id   := No_Elist;
      New_Sloc  : Source_Ptr := No_Location;
      New_Scope : Entity_Id  := Empty) return Node_Id;
   --  Given a node that is the root of a subtree, Copy_Tree copies the entire
   --  syntactic subtree, including recursively any descendants whose parent
   --  field references a copied node (descendants not linked to a copied node
   --  by the parent field are not copied, instead the copied tree references
   --  the same descendant as the original in this case, which is appropriate
   --  for non-syntactic fields such as Etype). The parent pointers in the
   --  copy are properly set. Copy_Tree (Empty/Error) returns Empty/Error.
   --  The one exception to the rule of not copying semantic fields is that
   --  any implicit types attached to the subtree are duplicated, so that
   --  the copy contains a distinct set of implicit type entities. Thus this
   --  function is used when it is necessary to duplicate an analyzed tree,
   --  declared in the same or some other compilation unit. This function is
   --  declared here rather than in atree because it uses semantic information
   --  in particular concerning the structure of itypes and the generation of
   --  public symbols.

   --  The Map argument, if set to a non-empty Elist, specifies a set of
   --  mappings to be applied to entities in the tree. The map has the form:
   --
   --     old entity 1
   --     new entity to replace references to entity 1
   --     old entity 2
   --     new entity to replace references to entity 2
   --     ...
   --
   --  The call destroys the contents of Map in this case
   --
   --  The parameter New_Sloc, if set to a value other than No_Location, is
   --  used as the Sloc value for all nodes in the new copy. If New_Sloc is
   --  set to its default value No_Location, then the Sloc values of the
   --  nodes in the copy are simply copied from the corresponding original.
   --
   --  The Comes_From_Source indication is unchanged if New_Sloc is set to
   --  the default No_Location value, but is reset if New_Sloc is given, since
   --  in this case the result clearly is neither a source node or an exact
   --  copy of a source node.
   --
   --  The parameter New_Scope, if set to a value other than Empty, is the
   --  value to use as the Scope for any Itypes that are copied. The most
   --  typical value for this parameter, if given, is Current_Scope.

   function New_External_Entity
     (Kind         : Entity_Kind;
      Scope_Id     : Entity_Id;
      Sloc_Value   : Source_Ptr;
      Related_Id   : Entity_Id;
      Suffix       : Character;
      Suffix_Index : Nat := 0;
      Prefix       : Character := ' ') return Entity_Id;
   --  This function creates an N_Defining_Identifier node for an internal
   --  created entity, such as an implicit type or subtype, or a record
   --  initialization procedure. The entity name is constructed with a call
   --  to New_External_Name (Related_Id, Suffix, Suffix_Index, Prefix), so
   --  that the generated name may be referenced as a public entry, and the
   --  Is_Public flag is set if needed (using Set_Public_Status). If the
   --  entity is for a type or subtype, the size/align fields are initialized
   --  to unknown (Uint_0).

   function New_Internal_Entity
     (Kind       : Entity_Kind;
      Scope_Id   : Entity_Id;
      Sloc_Value : Source_Ptr;
      Id_Char    : Character) return Entity_Id;
   --  This function is similar to New_External_Entity, except that the
   --  name is constructed by New_Internal_Name (Id_Char). This is used
   --  when the resulting entity does not have to be referenced as a
   --  public entity (and in this case Is_Public is not set).

   procedure Next_Actual (Actual_Id : in out Node_Id);
   pragma Inline (Next_Actual);
   --  Next_Actual (N) is equivalent to N := Next_Actual (N)

   function Next_Actual (Actual_Id : Node_Id) return Node_Id;
   --  Find next actual parameter in declaration order. As described for
   --  First_Actual, this is the next actual in the declaration order, not
   --  the call order, so this does not correspond to simply taking the
   --  next entry of the Parameter_Associations list. The argument is an
   --  actual previously returned by a call to First_Actual or Next_Actual.
   --  Note tha the result produced is always an expression, not a parameter
   --  assciation node, even if named notation was used.

   procedure Normalize_Actuals
     (N       : Node_Id;
      S       : Entity_Id;
      Report  : Boolean;
      Success : out Boolean);
   --  Reorders lists of actuals according to names of formals, value returned
   --  in Success indicates sucess of reordering. For more details, see body.
   --  Errors are reported only if Report is set to True.

   procedure Note_Possible_Modification (N : Node_Id);
   --  This routine is called if the sub-expression N maybe the target of
   --  an assignment (e.g. it is the left side of an assignment, used as
   --  an out parameters, or used as prefixes of access attributes). It
   --  sets May_Be_Modified in the associated entity if there is one,
   --  taking into account the rule that in the case of renamed objects,
   --  it is the flag in the renamed object that must be set.

   function Object_Access_Level (Obj : Node_Id) return Uint;
   --  Return the accessibility level of the view of the object Obj.
   --  For convenience, qualified expressions applied to object names
   --  are also allowed as actuals for this function.

   function Private_Component (Type_Id : Entity_Id) return Entity_Id;
   --  Returns some private component (if any) of the given Type_Id.
   --  Used to enforce the rules on visibility of operations on composite
   --  types, that depend on the full view of the component type. For a
   --  record type there may be several such components, we just return
   --  the first one.

   procedure Process_End_Label
     (N   : Node_Id;
      Typ : Character;
      Ent : Entity_Id);
   --  N is a node whose End_Label is to be processed, generating all
   --  appropriate cross-reference entries, and performing style checks
   --  for any identifier references in the end label. Typ is either
   --  'e' or 't indicating the type of the cross-reference entity
   --  (e for spec, t for body, see Lib.Xref spec for details). The
   --  parameter Ent gives the entity to which the End_Label refers,
   --  and to which cross-references are to be generated.

   function Real_Convert (S : String) return Node_Id;
   --  S is a possibly signed syntactically valid real literal. The result
   --  returned is an N_Real_Literal node representing the literal value.

   function Rep_To_Pos_Flag (E : Entity_Id; Loc : Source_Ptr) return Node_Id;
   --  This is used to construct the second argument in a call to Rep_To_Pos
   --  which is Standard_True if range checks are enabled (E is an entity to
   --  which the Range_Checks_Suppressed test is applied), and Standard_False
   --  if range checks are suppressed. Loc is the location for the node that
   --  is returned (which is a New_Occurrence of the appropriate entity).
   --
   --  Note: one might think that it would be fine to always use True and
   --  to ignore the suppress in this case, but it is generally better to
   --  believe a request to suppress exceptions if possible, and further
   --  more there is at least one case in the generated code (the code for
   --  array assignment in a loop) that depends on this suppression.

   procedure Require_Entity (N : Node_Id);
   --  N is a node which should have an entity value if it is an entity name.
   --  If not, then check if there were previous errors. If so, just fill
   --  in with Any_Id and ignore. Otherwise signal a program error exception.
   --  This is used as a defense mechanism against ill-formed trees caused by
   --  previous errors (particularly in -gnatq mode).

   function Requires_Transient_Scope (Id : Entity_Id) return Boolean;
   --  E is a type entity. The result is True when temporaries of this
   --  type need to be wrapped in a transient scope to be reclaimed
   --  properly when a secondary stack is in use. Examples of types
   --  requiring such wrapping are controlled types and variable-sized
   --  types including unconstrained arrays

   procedure Reset_Analyzed_Flags (N : Node_Id);
   --  Reset the Analyzed flags in all nodes of the tree whose root is N

   function Safe_To_Capture_Value
     (N    : Node_Id;
      Ent  : Entity_Id)
      return Boolean;
   --  The caller is interested in capturing a value (either the current
   --  value, or an indication that the value is non-null) for the given
   --  entity Ent. This value can only be captured if sequential execution
   --  semantics can be properly guaranteed so that a subsequent reference
   --  will indeed be sure that this current value indication is correct.
   --  The node N is the construct which resulted in the possible capture
   --  of the value (this is used to check if we are in a conditional).

   function Same_Name (N1, N2 : Node_Id) return Boolean;
   --  Determine if two (possibly expanded) names are the same name

   function Same_Type (T1, T2 : Entity_Id) return Boolean;
   --  Determines if T1 and T2 represent exactly the same type. Two types
   --  are the same if they are identical, or if one is an unconstrained
   --  subtype of the other, or they are both common subtypes of the same
   --  type with identical constraints. The result returned is conservative.
   --  It is True if the types are known to be the same, but a result of
   --  False is indecisive (e.g. the compiler may not be able to tell that
   --  two constraints are identical).

   function Scope_Within_Or_Same (Scope1, Scope2 : Entity_Id) return Boolean;
   --  Determines if the entity Scope1 is the same as Scope2, or if it is
   --  inside it, where both entities represent scopes. Note that scopes
   --  are only partially ordered, so Scope_Within_Or_Same (A,B) and
   --  Scope_Within_Or_Same (B,A) can both be False for a given pair A,B.

   function Scope_Within (Scope1, Scope2 : Entity_Id) return Boolean;
   --  Like Scope_Within_Or_Same, except that this function returns
   --  False in the case where Scope1 and Scope2 are the same scope.

   procedure Set_Convention (E : Entity_Id; Val : Convention_Id);
   --  Same as Basic_Set_Convention, but with an extra check for access types.
   --  In particular, if E is an access-to-subprogram type, and Val is a
   --  foreign convention, then we set Can_Use_Internal_Rep to False on E.
   --  Also, if the Etype of E is set and is an anonymous access type with
   --  no convention set, this anonymous type inherits the convention of E.

   procedure Set_Current_Entity (E : Entity_Id);
   --  Establish the entity E as the currently visible definition of its
   --  associated name (i.e. the Node_Id associated with its name)

   procedure Set_Name_Entity_Id (Id : Name_Id; Val : Entity_Id);
   --  Sets the Entity_Id value associated with the given name, which is the
   --  Id of the innermost visible entity with the given name. See the body
   --  of package Sem_Ch8 for further details on the handling of visibility.

   procedure Set_Next_Actual (Ass1_Id : Node_Id; Ass2_Id : Node_Id);
   --  The arguments may be parameter associations, whose descendants
   --  are the optional formal name and the actual parameter. Positional
   --  parameters are already members of a list, and do not need to be
   --  chained separately. See also First_Actual and Next_Actual.

   procedure Set_Public_Status (Id : Entity_Id);
   --  If an entity (visible or otherwise) is defined in a library
   --  package, or a package that is itself public, then this subprogram
   --  labels the entity public as well.

   procedure Set_Scope_Is_Transient (V : Boolean := True);
   --  Set the flag Is_Transient of the current scope

   procedure Set_Size_Info (T1, T2 : Entity_Id);
   --  Copies the Esize field and Has_Biased_Representation flag from
   --  (sub)type entity T2 to (sub)type entity T1. Also copies the
   --  Is_Unsigned_Type flag in the fixed-point and discrete cases,
   --  and also copies the alignment value from T2 to T1. It does NOT
   --  copy the RM_Size field, which must be separately set if this
   --  is required to be copied also.

   function Scope_Is_Transient  return Boolean;
   --  True if the current scope is transient.

   function Static_Integer (N : Node_Id) return Uint;
   --  This function analyzes the given expression node and then resolves it
   --  as any integer type. If the result is static, then the value of the
   --  universal expression is returned, otherwise an error message is output
   --  and a value of No_Uint is returned.

   function Statically_Different (E1, E2 : Node_Id) return Boolean;
   --  Return True if it can be statically determined that the Expressions
   --  E1 and E2 refer to different objects

   function Subprogram_Access_Level (Subp : Entity_Id) return Uint;
   --  Return the accessibility level of the view denoted by Subp.

   procedure Trace_Scope (N : Node_Id; E : Entity_Id; Msg : String);
   --  Print debugging information on entry to each unit being analyzed.

   procedure Transfer_Entities (From : Entity_Id; To : Entity_Id);
   --  Move a list of entities from one scope to another, and recompute
   --  Is_Public based upon the new scope.

   function Type_Access_Level (Typ : Entity_Id) return Uint;
   --  Return the accessibility level of Typ

   function Unique_Defining_Entity (N : Node_Id) return Entity_Id;
   --  Return the entity which represents declaration N, so that different
   --  views of the same entity have the same unique defining entity:
   --    * entry declaration and entry body
   --    * package spec and body
   --    * protected type declaration, protected body stub and protected body
   --    * private view and full view of a deferred constant
   --    * private view and full view of a type
   --    * subprogram declaration, subprogram stub and subprogram body
   --    * task type declaration, task body stub and task body
   --  In other cases, return the defining entity for N.

   function Unique_Entity (E : Entity_Id) return Entity_Id;
   --  Return the unique entity for entity E, which would be returned by
   --  Unique_Defining_Entity if applied to the enclosing declaration of E.

   function Unique_Name (E : Entity_Id) return String;
   --  Return a unique name for entity E, which could be used to identify E
   --  across compilation units.

   function Unit_Declaration_Node (Unit_Id : Entity_Id) return Node_Id;
   --  Unit_Id is the simple name of a program unit, this function returns
   --  the corresponding xxx_Declaration node for the entity. Also applies
   --  to the body entities for subprograms, tasks and protected units, in
   --  which case it returns the subprogram, task or protected body node
   --  for it. The unit may be a child unit with any number of ancestors.

   function Universal_Interpretation (Opnd : Node_Id) return Entity_Id;
   --  Yields universal_Integer or Universal_Real if this is a candidate.

   function Within_Init_Proc return Boolean;
   --  Determines if Current_Scope is within an init proc

   function Within_Scope (E : Entity_Id; S : Entity_Id) return Boolean;
   --  Returns True if entity E is declared within scope S

   procedure Wrong_Type (Expr : Node_Id; Expected_Type : Entity_Id);
   --  Output error message for incorrectly typed expression. Expr is the
   --  node for the incorrectly typed construct (Etype (Expr) is the type
   --  found), and Expected_Type is the entity for the expected type. Note
   --  that Expr does not have to be a subexpression, anything with an
   --  Etype field may be used.

   function Copy_Subprogram_Spec (Spec : Node_Id) return Node_Id;
   --  Replicate a function or a procedure specification denoted by Spec. The
   --  resulting tree is an exact duplicate of the original tree. New entities
   --  are created for the unit name and the formal parameters.
   
   function Current_Reactive_Type (State : Node_Id) return Entity_Id;
   --  State is a state of a reactive procedure, retreive the corresponding
   --  reactive type
   
private
   pragma Inline (Current_Entity);
   pragma Inline (Get_Name_Entity_Id);
   pragma Inline (Is_False);
   pragma Inline (Is_Statement);
   pragma Inline (Is_True);
   pragma Inline (Set_Current_Entity);
   pragma Inline (Set_Name_Entity_Id);
   pragma Inline (Set_Size_Info);

end Sem_Util;
