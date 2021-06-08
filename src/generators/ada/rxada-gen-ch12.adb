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

with Atree; use Atree;
with Sinfo; use Sinfo;
with Nlists; use Nlists;
with Reflex.Gen.Ada_Outputs; use Reflex.Gen.Ada_Outputs;
with Rxada.Gen.Ch2; use Rxada.Gen.Ch2;
with Rxada.Gen.Ch3; use Rxada.Gen.Ch3;
with Rxada.Gen.Ch6; use Rxada.Gen.Ch6;

package body Rxada.Gen.Ch12 is
   
      -------------------------------
      -- 12.1  Generic Declaration --
      -------------------------------

      --  GENERIC_DECLARATION ::=
      --    GENERIC_SUBPROGRAM_DECLARATION | GENERIC_PACKAGE_DECLARATION
   
   ---------------------------------------------
   -- Generate_Generic_Subprogram_Declaration --
   ---------------------------------------------
   
   procedure Generate_Generic_Subprogram_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      ------------------------------------------
      -- 12.1  Generic Subprogram Declaration --
      ------------------------------------------

      --  GENERIC_SUBPROGRAM_DECLARATION ::=
      --    GENERIC_FORMAL_PART SUBPROGRAM_SPECIFICATION;

      --  Note: Generic_Formal_Declarations can include pragmas

      --  N_Generic_Subprogram_Declaration
      --  Sloc points to GENERIC
      --  Specification (Node1) subprogram specification
      --  Corresponding_Body (Node5-Sem)
      --  Generic_Formal_Declarations (List2) from generic formal part
      --  Parent_Spec (Node4-Sem)
      
      Write_Indent_Str (Ob, "generic");
      Write_Eol (Ob);
      
      Generate_Generic_Formal_Parameter_Declaration (This, Node);
      
      Rxada.Gen.Ch6.Generate_Subprogram_Specification
	(This, Specification (Node));
      
      Write_Str (Ob, ";");
      Write_Eol (Ob);
   end Generate_Generic_Subprogram_Declaration;
   
   ------------------------------------------
   -- Generate_Generic_Package_Declaration --
   ------------------------------------------
   
   procedure Generate_Generic_Package_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      ---------------------------------------
      -- 12.1  Generic Package Declaration --
      ---------------------------------------

      --  GENERIC_PACKAGE_DECLARATION ::=
      --    GENERIC_FORMAL_PART PACKAGE_SPECIFICATION;

      --  Note: when we do generics right, the Activation_Chain_Entity entry
      --  for this node can be removed (since the expander won't see generic
      --  units any more)???.

      --  Note: Generic_Formal_Declarations can include pragmas

      --  N_Generic_Package_Declaration
      --  Sloc points to GENERIC
      --  Specification (Node1) package specification
      --  Corresponding_Body (Node5-Sem)
      --  Generic_Formal_Declarations (List2) from generic formal part
      --  Parent_Spec (Node4-Sem)
      --  Activation_Chain_Entity (Node3-Sem)
      null;
   end Generate_Generic_Package_Declaration;
   
   ----------------------------------
   -- Generate_Generic_Formal_Part --
   ----------------------------------
   
   procedure Generate_Generic_Formal_Part
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      -------------------------------
      -- 12.1  Generic Formal Part --
      -------------------------------

      --  GENERIC_FORMAL_PART ::=
      --    generic {GENERIC_FORMAL_PARAMETER_DECLARATION | USE_CLAUSE}
      null;
   end Generate_Generic_Formal_Part;
   
   ---------------------------------------------------
   -- Generate_Generic_Formal_Parameter_Declaration --
   ---------------------------------------------------
   
   procedure Generate_Generic_Formal_Parameter_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob   : Output_Buffer := This.Get_Output_Buffer;
      Kind : Node_Kind;
   begin
      ------------------------------------------------
      -- 12.1  Generic Formal Parameter Declaration --
      ------------------------------------------------

      --  GENERIC_FORMAL_PARAMETER_DECLARATION ::=
      --    FORMAL_OBJECT_DECLARATION
      --  | FORMAL_TYPE_DECLARATION
      --  | FORMAL_SUBPROGRAM_DECLARATION
      --  | FORMAL_PACKAGE_DECLARATION
      
      Kind := Nkind (Node);
      if kind = N_Formal_Object_Declaration then
	 Generate_Formal_Object_Declaration (This, Node);
      elsif Kind = N_Formal_Type_Declaration then
	 Generate_Formal_Type_Declaration (This, Node);
      elsif Kind = N_Formal_Subprogram_Declaration then
	 Generate_Formal_Subprogram_Declaration (This, Node);
      elsif Kind = N_Formal_Package_Declaration then
	 Generate_Formal_Package_Declaration (This, Node);
      else
	 raise Program_Error;
      end if;
   end Generate_Generic_Formal_Parameter_Declaration;
   
   ------------------------------------
   -- Generate_Generic_Instantiation --
   ------------------------------------
   
   procedure Generate_Generic_Instantiation
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob   : Output_Buffer := This.Get_Output_Buffer;
      Kind : Node_Kind;
   begin
      ---------------------------------
      -- 12.3  Generic Instantiation --
      ---------------------------------

      --  GENERIC_INSTANTIATION ::=
      --    package DEFINING_PROGRAM_UNIT_NAME is
      --      new generic_package_NAME [GENERIC_ACTUAL_PART];
      --  | procedure DEFINING_PROGRAM_UNIT_NAME is
      --      new generic_procedure_NAME [GENERIC_ACTUAL_PART];
      --  | function DEFINING_DESIGNATOR is
      --      new generic_function_NAME [GENERIC_ACTUAL_PART];
      
      Kind := Nkind (Node);
      if Kind = N_Package_Instantiation then
	 Generate_Package_Instantiation (This, Node);
      elsif Kind = N_Procedure_Instantiation then
	 Generate_Procedure_Instantiation (This, Node);
      elsif Kind = N_Function_Instantiation then
	 Generate_Function_Instantiation (This, Node);
      else
	 raise Program_Error;
      end if;
   end Generate_Generic_Instantiation;
   
   ------------------------------------
   -- Generate_Package_Instantiation --
   ------------------------------------
   
   procedure Generate_Package_Instantiation
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob    : Output_Buffer := This.Get_Output_Buffer;
      Assoc : Node_Id;
      L     : List_Id;
   begin
      --    package DEFINING_PROGRAM_UNIT_NAME is
      --      new generic_package_NAME [GENERIC_ACTUAL_PART];
      
      --  N_Package_Instantiation
      --  Sloc points to PACKAGE
      --  Defining_Unit_Name (Node1)
      --  Name (Node2)
      --  Generic_Associations (List3) (set to No_List if no
      --   generic actual part)
      --  Parent_Spec (Node4-Sem)
      --  Instance_Spec (Node5-Sem)
      --  ABE_Is_Certain (Flag18-Sem)
      
      Write_Indent_Str (Ob, "package ");
      Generate_Defining_Program_Unit_Name (This, Defining_Unit_Name (Node));
      Write_Str (Ob, " is new ");
      Generate_Expanded_Name (This, Name (Node));
      Write_Eol (Ob);
      
      L := Generic_Associations (Node);
      if not Is_Empty_List (L) then
	 Write_Indent_Str (Ob, "(");
	 Assoc := First (L);
	 while Present (Assoc) loop
	    Generate_Generic_Association (This, Assoc);
	    Next (Assoc);
	 end loop;
	 Write_Str (Ob, ")");
      end if;
      
      Write_Str (Ob, ";");
      Write_Eol (Ob);
      
   end Generate_Package_Instantiation;
   
   --------------------------------------
   -- Generate_Procedure_Instantiation --
   --------------------------------------
   
   procedure Generate_Procedure_Instantiation
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob    : Output_Buffer := This.Get_Output_Buffer;
      Assoc : Node_Id;
      L     : List_Id;
   begin
      --  procedure DEFINING_PROGRAM_UNIT_NAME is
      --      new generic_procedure_NAME [GENERIC_ACTUAL_PART];
      
      --  N_Procedure_Instantiation
      --  Sloc points to PROCEDURE
      --  Defining_Unit_Name (Node1)
      --  Name (Node2)
      --  Parent_Spec (Node4-Sem)
      --  Generic_Associations (List3) (set to No_List if no
      --   generic actual part)
      --  Instance_Spec (Node5-Sem)
      --  ABE_Is_Certain (Flag18-Sem)
      
      Write_Indent_Str (Ob, "procedure ");
      Generate_Defining_Program_Unit_Name (This, Defining_Unit_Name (Node));
      Write_Str (Ob, " is new ");
      Generate_Expanded_Name (This, Name (Node));
      Write_Eol (Ob);
      
      L := Generic_Associations (Node);
      if not Is_Empty_List (L) then
	 Write_Indent_Str (Ob, "(");
	 Assoc := First (L);
	 while Present (Assoc) loop
	    Generate_Generic_Association (This, Assoc);
	    Next (Assoc);
	 end loop;
	 Write_Str (Ob, ")");
      end if;
      
      Write_Str (Ob, ";");
      Write_Eol (Ob);
      
   end Generate_Procedure_Instantiation;
   
   -------------------------------------
   -- Generate_Function_Instantiation --
   -------------------------------------
   
   procedure Generate_Function_Instantiation
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob    : Output_Buffer := This.Get_Output_Buffer;
      Assoc : Node_Id;
      L     : List_Id;
   begin
      --  N_Function_Instantiation
      --  Sloc points to FUNCTION
      --  Defining_Unit_Name (Node1)
      --  Name (Node2)
      --  Generic_Associations (List3) (set to No_List if no
      --   generic actual part)
      --  Parent_Spec (Node4-Sem)
      --  Instance_Spec (Node5-Sem)
      --  ABE_Is_Certain (Flag18-Sem)
      
      --  function DEFINING_DESIGNATOR is
      --      new generic_function_NAME [GENERIC_ACTUAL_PART];
      
      Write_Indent_Str (Ob, "function ");
      Generate_Defining_Program_Unit_Name (This, Defining_Unit_Name (Node));
      Write_Str (Ob, " is new ");
      Generate_Expanded_Name (This, Name (Node));
      Write_Eol (Ob);
      
      L := Generic_Associations (Node);
      if not Is_Empty_List (L) then
	 Write_Indent_Str (Ob, "(");
	 Assoc := First (L);
	 while Present (Assoc) loop
	    Generate_Generic_Association (This, Assoc);
	    Next (Assoc);
	 end loop;
	 Write_Str (Ob, ")");
      end if;
      
      Write_Str (Ob, ";");
      Write_Eol (Ob);
      
   end Generate_Function_Instantiation;
   
   ----------------------------------
   -- Generate_Generic_Association --
   ----------------------------------
   
   procedure Generate_Generic_Association
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob     : Output_Buffer := This.Get_Output_Buffer;
      Sel    : Node_Id;
      Actual : Node_Id;
   begin
      ------------------------------
      -- 12.3 Generic Actual Part --
      ------------------------------

      --  GENERIC_ACTUAL_PART ::=
      --    (GENERIC_ASSOCIATION {, GENERIC_ASSOCIATION})
   
      -------------------------------
      -- 12.3  Generic Association --
      -------------------------------

      --  GENERIC_ASSOCIATION ::=
      --    [generic_formal_parameter_SELECTOR_NAME =>]
      --      EXPLICIT_GENERIC_ACTUAL_PARAMETER

      --  Note: unlike the procedure call case, a generic association node
      --  is generated for every association, even if no formal is present.
      --  In this case the parser will leave the Selector_Name field set
      --  to Empty, to be filled in later by the semantic pass.

      --  N_Generic_Association
      --  Sloc points to first token of generic association
      --  Selector_Name (Node2) (set to Empty if no formal
      --   parameter selector name)
      --  Explicit_Generic_Actual_Parameter (Node1)
      
      Sel    := Selector_Name (Node);
      Actual := Explicit_Generic_Actual_Parameter (Node);
      
      if Present (Sel) then
	 Write_Id (Ob, Sel);
	 Write_Str (Ob, " => ");
      end if;
      
      Generate_Explicit_Generic_Actual_Parameter (This, Node);
   end Generate_Generic_Association;
   
   ------------------------------------------------
   -- Generate_Explicit_Generic_Actual_Parameter --
   ------------------------------------------------
   
   procedure Generate_Explicit_Generic_Actual_Parameter
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      ---------------------------------------------
      -- 12.3  Explicit Generic Actual Parameter --
      ---------------------------------------------

      --  EXPLICIT_GENERIC_ACTUAL_PARAMETER ::=
      --    EXPRESSION      | variable_NAME   | subprogram_NAME
      --   | SUBTYPE_MARK    | package_instance_NAME
      
      
      Generate_Node (This, Node);
   end Generate_Explicit_Generic_Actual_Parameter;
   
   ----------------------------------------
   -- Generate_Formal_Object_Declaration --
   ----------------------------------------
   
   procedure Generate_Formal_Object_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob     : Output_Buffer := This.Get_Output_Buffer;
      Def_Id : Node_Id;
   begin
      -------------------------------------
      -- 12.4  Formal Object Declaration --
      -------------------------------------

      --  FORMAL_OBJECT_DECLARATION ::=
      --    DEFINING_IDENTIFIER_LIST :
      --      MODE SUBTYPE_MARK [:= DEFAULT_EXPRESSION];

      --  Although the syntax allows multiple identifiers in the list, the
      --  semantics is as though successive declarations were given with
      --  identical type definition and expression components. To simplify
      --  semantic processing, the parser represents a multiple declaration
      --  case as a sequence of single declarations, using the More_Ids and
      --  Prev_Ids flags to preserve the original source form as described
      --  in the section on "Handling of Defining Identifier Lists".

      --  N_Formal_Object_Declaration
      --  Sloc points to first identifier
      --  Defining_Identifier (Node1)
      --  In_Present (Flag15)
      --  Out_Present (Flag17)
      --  Subtype_Mark (Node4)
      --  Expression (Node3) (set to Empty if no default expression)
      --  More_Ids (Flag5) (set to False if no more identifiers in list)
      --  Prev_Ids (Flag6) (set to False if no previous identifiers in list)
      
      Write_Indent (Ob);
      Def_Id := Defining_Identifier (Node);
      Write_Id (Ob, Def_Id);
      Write_Str (Ob, " : ");
      if In_Present (Node) then
	 Write_Str (Ob, " in ");
      end if;
      if Out_Present (Node) then
	 Write_Str (Ob, " out ");
      end if;
      
      Generate_Subtype_Mark (This, Subtype_Mark (Node));
      
      if Present (Expression (Node)) then
	 Write_Str (Ob, " := ");
	 Generate_Node (This, Expression (Node));
      end if;
      
      Write_Str (Ob, ";");
   end Generate_Formal_Object_Declaration;
   
   --------------------------------------
   -- Generate_Formal_Type_Declaration --
   --------------------------------------
   
   procedure Generate_Formal_Type_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      -----------------------------------
      -- 12.5  Formal Type Declaration --
      -----------------------------------

      --  FORMAL_TYPE_DECLARATION ::=
      --    type DEFINING_IDENTIFIER [DISCRIMINANT_PART]
      --      is FORMAL_TYPE_DEFINITION;

      --  N_Formal_Type_Declaration
      --  Sloc points to TYPE
      --  Defining_Identifier (Node1)
      --  Formal_Type_Definition (Node3)
      --  Discriminant_Specifications (List4) (set to No_List if no
      --   discriminant part)
      --  Unknown_Discriminants_Present (Flag13) set if (<>) discriminant
      
      Write_Indent_Str (Ob, "type ");
      Write_Id (Ob, Defining_Identifier (Node));
      Write_Str (Ob, " is ");
      Generate_Formal_Type_Definition (This, Formal_Type_Definition (Node));
      Write_Str (Ob, ";");
      Write_Eol (Ob);
   end Generate_Formal_Type_Declaration;
   
   -------------------------------------
   -- Generate_Formal_Type_Definition --
   -------------------------------------
   
   procedure Generate_Formal_Type_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob   : Output_Buffer := This.Get_Output_Buffer;
      Kind : Node_Kind;
   begin
      ----------------------------------
      -- 12.5  Formal type definition --
      ----------------------------------

      --  FORMAL_TYPE_DEFINITION ::=
      --    FORMAL_PRIVATE_TYPE_DEFINITION
      --  | FORMAL_DERIVED_TYPE_DEFINITION
      --  | FORMAL_DISCRETE_TYPE_DEFINITION
      --  | FORMAL_SIGNED_INTEGER_TYPE_DEFINITION
      --  | FORMAL_MODULAR_TYPE_DEFINITION
      --  | FORMAL_FLOATING_POINT_DEFINITION
      --  | FORMAL_ORDINARY_FIXED_POINT_DEFINITION
      --  | FORMAL_DECIMAL_FIXED_POINT_DEFINITION
      --  | FORMAL_ARRAY_TYPE_DEFINITION
      --  | FORMAL_ACCESS_TYPE_DEFINITION
      
      Kind := Nkind (Node);
      if Kind = N_Formal_Private_Type_Definition then
	 Generate_Formal_Private_Type_Definition (This, Node);
      elsif Kind = N_Formal_Derived_Type_Definition then
	 Generate_Formal_Derived_Type_Definition (This, Node);
      elsif Kind = N_Formal_Discrete_Type_Definition then
	 Generate_Formal_Discrete_Type_Definition (This, Node);
      elsif Kind = N_Formal_Signed_Integer_Type_Definition then
	 Generate_Formal_Signed_Integer_Type_Definition (This, Node);
      elsif Kind = N_Formal_Modular_Type_Definition then
	 Generate_Formal_Modular_Type_Definition (This, Node);
      elsif Kind = N_Unconstrained_Array_Definition 
	or else Kind = N_Constrained_Array_Definition then
	 Generate_Formal_Array_Type_Definition (This, Node);	 	
      elsif Kind = N_Access_Definition then
	 Generate_Formal_Access_Type_Definition (This, Node);
      else
	 raise Program_Error;
      end if;
   end Generate_Formal_Type_Definition;
   
   ---------------------------------------------
   -- Generate_Formal_Private_Type_Definition --
   ---------------------------------------------
   
   procedure Generate_Formal_Private_Type_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      ---------------------------------------------
      -- 12.5.1  Formal Private Type Definition --
      ---------------------------------------------

      --  FORMAL_PRIVATE_TYPE_DEFINITION ::=
      --    [[abstract] tagged] [limited] private

      --  Note: TAGGED is not allowed in Ada 83 mode

      --  N_Formal_Private_Type_Definition
      --  Sloc points to PRIVATE
      --  Abstract_Present (Flag4)
      --  Tagged_Present (Flag15)
      --  Limited_Present (Flag17)
      
      if Abstract_Present (Node) then
	 Write_Str (Ob, "abstract ");
      end if;
      if Tagged_Present (Node) then
	 Write_Str (Ob, "tagged ");
      end if;
      Write_Str (Ob, "private");
   end Generate_Formal_Private_Type_Definition;
   
   ---------------------------------------------
   -- Generate_Formal_Derived_Type_Definition --
   ---------------------------------------------
   
   procedure Generate_Formal_Derived_Type_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      --------------------------------------------
      -- 12.5.1  Formal Derived Type Definition --
      --------------------------------------------

      --  FORMAL_DERIVED_TYPE_DEFINITION ::=
      --    [abstract] new SUBTYPE_MARK [with private]

      --  Note: this construct is not allowed in Ada 83 mode

      --  N_Formal_Derived_Type_Definition
      --  Sloc points to NEW
      --  Subtype_Mark (Node4)
      --  Private_Present (Flag15)
      --  Abstract_Present (Flag4)
      
      if Abstract_Present (Node) then
	 Write_Str (Ob, "abstract ");
      end if;
      Write_Str (Ob, "new ");
      Generate_Subtype_Mark (This, Subtype_Mark (Node));
      if Private_Present (Node) then
	 Write_Str (Ob, " with private");
      end if;
   end Generate_Formal_Derived_Type_Definition;
   
   ----------------------------------------------
   -- Generate_Formal_Discrete_Type_Definition --
   ----------------------------------------------
   
   procedure Generate_Formal_Discrete_Type_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      ---------------------------------------------
      -- 12.5.2  Formal Discrete Type Definition --
      ---------------------------------------------

      --  FORMAL_DISCRETE_TYPE_DEFINITION ::= (<>)

      --  N_Formal_Discrete_Type_Definition
      --  Sloc points to (
      
      Write_Str (Ob, "(<>)");
   end Generate_Formal_Discrete_Type_Definition;
   
   ----------------------------------------------------
   -- Generate_Formal_Signed_Integer_Type_Definition --
   ----------------------------------------------------
   
   procedure Generate_Formal_Signed_Integer_Type_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      ---------------------------------------------------
      -- 12.5.2  Formal Signed Integer Type Definition --
      ---------------------------------------------------

      --  FORMAL_SIGNED_INTEGER_TYPE_DEFINITION ::= range <>

      --  N_Formal_Signed_Integer_Type_Definition
      --  Sloc points to RANGE
      
      Write_Str (Ob, " range <>");
   end Generate_Formal_Signed_Integer_Type_Definition;
   
   ---------------------------------------------
   -- Generate_Formal_Modular_Type_Definition --
   ---------------------------------------------
   
   procedure Generate_Formal_Modular_Type_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      --------------------------------------------
      -- 12.5.2  Formal Modular Type Definition --
      --------------------------------------------

      --  FORMAL_MODULAR_TYPE_DEFINITION ::= mod <>

      --  N_Formal_Modular_Type_Definition
      --  Sloc points to MOD
      
      Write_Str (Ob, " mod <>");
   end Generate_Formal_Modular_Type_Definition;
   
   -------------------------------------------
   -- Generate_Formal_Array_Type_Definition --
   -------------------------------------------
   
   procedure Generate_Formal_Array_Type_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob   : Output_Buffer := This.Get_Output_Buffer;
      Kind : Node_Kind;
   begin
      ------------------------------------------
      -- 12.5.3  Formal Array Type Definition --
      ------------------------------------------

      --  FORMAL_ARRAY_TYPE_DEFINITION ::= ARRAY_TYPE_DEFINITION
      
      Kind := Nkind (Node);
      if Kind = N_Unconstrained_Array_Definition then
         Generate_Unconstrained_Array_Declaration (This, Node);
      elsif Kind = N_Constrained_Array_Definition then
         Generate_Constrained_Array_Declaration (This, Node);
      else
	 raise Program_Error;
      end if;

   end Generate_Formal_Array_Type_Definition;
   
   --------------------------------------------
   -- Generate_Formal_Access_Type_Definition --
   --------------------------------------------
   
   procedure Generate_Formal_Access_Type_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      -------------------------------------------
      -- 12.5.4  Formal Access Type Definition --
      -------------------------------------------

      --  FORMAL_ACCESS_TYPE_DEFINITION ::= ACCESS_TYPE_DEFINITION
      
      Generate_Access_To_Object_Definition (This, Node);
   end Generate_Formal_Access_Type_Definition;
   
   --------------------------------------------
   -- Generate_Formal_Subprogram_Declaration --
   --------------------------------------------
   
   procedure Generate_Formal_Subprogram_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      -----------------------------------------
      -- 12.6  Formal Subprogram Declaration --
      -----------------------------------------

      --  FORMAL_SUBPROGRAM_DECLARATION ::=
      --    with SUBPROGRAM_SPECIFICATION [is SUBPROGRAM_DEFAULT];

      --  N_Formal_Subprogram_Declaration
      --  Sloc points to WITH
      --  Specification (Node1)
      --  Default_Name (Node2) (set to Empty if no subprogram default)
      --  Box_Present (Flag15)

      --  Note: if no subprogram default is present, then Name is set
      --  to Empty, and Box_Present is False.
      
      Write_Indent_Str (Ob, "with ");
      Generate_Subprogram_Specification (This, Node);
      
      if Present (Default_Name (Node)) then
	 Write_Id (Ob, Default_Name (Default_Name (Node)));
      elsif Box_Present (Node) then
	 Write_Str (Ob, "<>");
      end if;
   end Generate_Formal_Subprogram_Declaration;
   
   ---------------------------------
   -- Generate_Subprogram_Default --
   ---------------------------------
   
   procedure Generate_Subprogram_Default
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      ------------------------------
      -- 12.6  Subprogram Default --
      ------------------------------

      --  SUBPROGRAM_DEFAULT ::= DEFAULT_NAME | <>

      --  There is no separate node in the tree for a subprogram default.
      --  Instead the parent (N_Formal_Subprogram_Declaration) node contains
      --  the default name or box indication, as needed.
      null;
   end Generate_Subprogram_Default;
   
   -----------------------------------------
   -- Generate_Formal_Package_Declaration --
   -----------------------------------------
   
   procedure Generate_Formal_Package_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob    : Output_Buffer := This.Get_Output_Buffer;
      L     : List_Id;
      Assoc : Node_Id;
   begin
      --------------------------------------
      -- 12.7  Formal Package Declaration --
      --------------------------------------

      --  FORMAL_PACKAGE_DECLARATION ::=
      --    with package DEFINING_IDENTIFIER
      --      is new generic_package_NAME FORMAL_PACKAGE_ACTUAL_PART;

      --  Note: formal package declarations not allowed in Ada 83 mode

      --  N_Formal_Package_Declaration
      --  Sloc points to WITH
      --  Defining_Identifier (Node1)
      --  Name (Node2)
      --  Generic_Associations (List3) (set to No_List if (<>) case or
      --   empty generic actual part)
      --  Box_Present (Flag15)
      --  Instance_Spec (Node5-Sem)
      --  ABE_Is_Certain (Flag18-Sem)
      
      Write_Indent_Str (Ob, "with ");
      Write_Id (Ob, Defining_Identifier (Node));
      Write_Str (Ob, " is new ");
      Generate_Expanded_Name (This, Name (Node));
      Write_Str (Ob, " ");
      L := Generic_Associations (Node);
      if not Is_Empty_List (L) then
	 Assoc := First (L);
	 while Present (Assoc) loop
	    Generate_Generic_Association (This, Assoc);
	    Next (Assoc);
	 end loop;
      elsif Box_Present (Node) then
	 Write_Str (Ob, "<>");
      end if;
   end Generate_Formal_Package_Declaration;
   
   -----------------------------------------
   -- Generate_Formal_Package_Actual_Part --
   -----------------------------------------
   
   procedure Generate_Formal_Package_Actual_Part
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      --------------------------------------
      -- 12.7  Formal Package Actual Part --
      --------------------------------------

      --  FORMAL_PACKAGE_ACTUAL_PART ::=
      --    (<>) | [GENERIC_ACTUAL_PART]

      --  There is no explicit node in the tree for a formal package
      --  actual part. Instead the information appears in the parent node
      --  (i.e. the formal package declaration node itself).
      
      null;
   end Generate_Formal_Package_Actual_Part;
   
end Rxada.Gen.Ch12;
