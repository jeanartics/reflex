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

with Atree; use Atree;
with Reflex.Infos;

package body Sinfo is

   use Atree.Unchecked_Access;
   --  This package is one of the few packages which is allowed to make direct
   --  references to tree nodes (since it is in the business of providing a
   --  higher level of tree access which other clients are expected to use and
   --  which implements checks).

   use Atree_Private_Part;
   --  The only reason that we ask for direct access to the private part of
   --  the tree package is so that we can directly reference the Nkind field
   --  of nodes table entries. We do this since it helps the efficiency of
   --  the Sinfo debugging checks considerably (note that when we are checking
   --  Nkind values, we don't need to check for a valid node reference, because
   --  we will check that anyway when we reference the field).

   NT : Nodes.Table_Ptr renames Nodes.Table;
   --  A short hand abbreviation, useful for the debugging checks

   ----------------------------
   -- Field Access Functions --
   ----------------------------

   function ABE_Is_Certain
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Formal_Package_Declaration
        or else NT (N).Nkind = N_Function_Call
        or else NT (N).Nkind = N_Function_Instantiation
        or else NT (N).Nkind = N_Package_Instantiation
        or else NT (N).Nkind = N_Procedure_Call_Statement
        or else NT (N).Nkind = N_Procedure_Instantiation);
      return Flag18 (N);
   end ABE_Is_Certain;

   function Abort_Handler
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Reactive_Abort_Statement);
      return Node4 (N);
   end Abort_Handler;

   function Abstract_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Derived_Type_Definition
        or else NT (N).Nkind = N_Formal_Derived_Type_Definition
        or else NT (N).Nkind = N_Formal_Private_Type_Definition
        or else NT (N).Nkind = N_Private_Extension_Declaration
        or else NT (N).Nkind = N_Private_Type_Declaration
        or else NT (N).Nkind = N_Record_Definition);
      return Flag4 (N);
   end Abstract_Present;

   function Access_Definition
     (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Definition
        or else NT (N).Nkind = N_Formal_Object_Declaration
        or else NT (N).Nkind = N_Object_Renaming_Declaration);
      return Node3 (N);
   end Access_Definition;

   function Access_To_Subprogram_Definition
     (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Access_Definition);
      return Node3 (N);
   end Access_To_Subprogram_Definition;

   function Access_Types_To_Process
      (N : Node_Id) return Elist_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Freeze_Entity);
      return Elist2 (N);
   end Access_Types_To_Process;

   function Actions
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_And_Then
        or else NT (N).Nkind = N_Compilation_Unit_Aux
        or else NT (N).Nkind = N_Freeze_Entity
        or else NT (N).Nkind = N_Or_Else);
      return List1 (N);
   end Actions;

   function Acts_As_Spec
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit
        or else NT (N).Nkind = N_Subprogram_Body);
      return Flag4 (N);
   end Acts_As_Spec;

   function Aggregate_Bounds
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aggregate);
      return Node3 (N);
   end Aggregate_Bounds;

   function Aliased_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Definition
        or else NT (N).Nkind = N_Object_Declaration);
      return Flag4 (N);
   end Aliased_Present;

   function All_Others
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Others_Choice);
      return Flag11 (N);
   end All_Others;

   function All_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Access_Definition
        or else NT (N).Nkind = N_Access_To_Object_Definition);
      return Flag15 (N);
   end All_Present;

   function Alternatives
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Case_Statement
        or else NT (N).Nkind = N_Reactive_Fork_Statement
        or else NT (N).Nkind = N_Reactive_Select_Statement);
      return List4 (N);
   end Alternatives;

   function Ancestor_Part
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Extension_Aggregate);
      return Node3 (N);
   end Ancestor_Part;

   function Array_Aggregate
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Enumeration_Representation_Clause);
      return Node3 (N);
   end Array_Aggregate;

   function Aspect_Rep_Item
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aspect_Specification);
      return Node2 (N);
   end Aspect_Rep_Item;

   function Assignment_OK
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Object_Declaration
        or else NT (N).Nkind in N_Subexpr);
      return Flag15 (N);
   end Assignment_OK;

   function Associated_Node
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Has_Entity
        or else NT (N).Nkind = N_Aggregate
        or else NT (N).Nkind = N_Extension_Aggregate
        or else NT (N).Nkind = N_Selected_Component);
      return Node4 (N);
   end Associated_Node;

   function At_End_Proc
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Handled_Sequence_Of_Statements);
      return Node1 (N);
   end At_End_Proc;

   function Attribute_Name
      (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Attribute_Reference);
      return Name2 (N);
   end Attribute_Name;

   function Aux_Decls_Node
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit);
      return Node5 (N);
   end Aux_Decls_Node;

   function Backwards_OK
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Assignment_Statement);
      return Flag6 (N);
   end Backwards_OK;

   function Bad_Is_Detected
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Subprogram_Body);
      return Flag15 (N);
   end Bad_Is_Detected;

   function Body_Required
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit);
      return Flag13 (N);
   end Body_Required;

   function Body_To_Inline
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Subprogram_Declaration);
      return Node3 (N);
   end Body_To_Inline;

   function Box_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Association
        or else NT (N).Nkind = N_Formal_Package_Declaration
        or else NT (N).Nkind = N_Formal_Subprogram_Declaration);
      return Flag15 (N);
   end Box_Present;

   function Char_Literal_Value
      (N : Node_Id) return Char_Code is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Character_Literal);
      return Char_Code (N);
      --  return Char_Code2 (N);
   end Char_Literal_Value;

   function Chars
      (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Has_Chars);
      return Name1 (N);
   end Chars;

   function Check_Address_Alignment
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Attribute_Definition_Clause);
      return Flag11 (N);
   end Check_Address_Alignment;

   function Choices
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Association);
      return List1 (N);
   end Choices;

   function Compile_Time_Known_Aggregate
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aggregate);
      return Flag18 (N);
   end Compile_Time_Known_Aggregate;

   function Component_Associations
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aggregate
        or else NT (N).Nkind = N_Extension_Aggregate);
      return List2 (N);
   end Component_Associations;

   function Component_Clauses
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Record_Representation_Clause);
      return List3 (N);
   end Component_Clauses;

   function Component_Definition
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Declaration
        or else NT (N).Nkind = N_Constrained_Array_Definition
        or else NT (N).Nkind = N_Unconstrained_Array_Definition);
      return Node4 (N);
   end Component_Definition;

   function Component_Items
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_List);
      return List3 (N);
   end Component_Items;

   function Component_List
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Record_Definition);
      return Node1 (N);
   end Component_List;

   function Component_Name
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Clause);
      return Node1 (N);
   end Component_Name;

   function Condition
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Elsif_Part
        or else NT (N).Nkind = N_Exit_Statement
        or else NT (N).Nkind = N_If_Statement
        or else NT (N).Nkind = N_Iteration_Scheme
        or else NT (N).Nkind = N_Reactive_Abort_Statement
        or else NT (N).Nkind = N_Reactive_Fork_Statement);
      return Node1 (N);
   end Condition;

   function Condition_Actions
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Elsif_Part
        or else NT (N).Nkind = N_Iteration_Scheme);
      return List3 (N);
   end Condition_Actions;

   function Config_Pragmas
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit_Aux);
      return List4 (N);
   end Config_Pragmas;

   function Constant_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Access_Definition
        or else NT (N).Nkind = N_Access_To_Object_Definition
        or else NT (N).Nkind = N_Object_Declaration);
      return Flag17 (N);
   end Constant_Present;

   function Constraint
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Subtype_Indication);
      return Node3 (N);
   end Constraint;

   function Constraints
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Index_Or_Discriminant_Constraint);
      return List1 (N);
   end Constraints;

   function Context_Installed
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_With_Clause);
      return Flag13 (N);
   end Context_Installed;

   function Context_Pending
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit);
      return Flag16 (N);
   end Context_Pending;

   function Context_Items
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit);
      return List1 (N);
   end Context_Items;

   function Controlling_Argument
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Function_Call
        or else NT (N).Nkind = N_Procedure_Call_Statement);
      return Node1 (N);
   end Controlling_Argument;

   function Conversion_OK
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Type_Conversion);
      return Flag14 (N);
   end Conversion_OK;

   function Corresponding_Aspect
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Pragma);
      return Node3 (N);
   end Corresponding_Aspect;

   function Corresponding_Body
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Generic_Package_Declaration
        or else NT (N).Nkind = N_Generic_Subprogram_Declaration
        or else NT (N).Nkind = N_Package_Declaration
        or else NT (N).Nkind = N_Subprogram_Declaration);
      return Node5 (N);
   end Corresponding_Body;

   function Corresponding_Generic_Association
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Object_Declaration
        or else NT (N).Nkind = N_Object_Renaming_Declaration);
      return Node5 (N);
   end Corresponding_Generic_Association;

   function Corresponding_Node
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Reactive_State);
      return Node5 (N);
   end Corresponding_Node;

   function Corresponding_Spec
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Package_Body
        or else NT (N).Nkind = N_Subprogram_Body
        or else NT (N).Nkind = N_Subprogram_Renaming_Declaration
        or else NT (N).Nkind = N_With_Clause);
      return Node5 (N);
   end Corresponding_Spec;

   function Debug_Statement
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Pragma);
      return Node3 (N);
   end Debug_Statement;

   function Declarations
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Block_Statement
        or else NT (N).Nkind = N_Compilation_Unit_Aux
        or else NT (N).Nkind = N_Package_Body
        or else NT (N).Nkind = N_Subprogram_Body);
      return List2 (N);
   end Declarations;

   function Default_Expression
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Parameter_Specification);
      return Node5 (N);
   end Default_Expression;

   function Default_Name
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Formal_Subprogram_Declaration);
      return Node2 (N);
   end Default_Name;

   function Defining_Identifier
      (N : Node_Id) return Entity_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Declaration
        or else NT (N).Nkind = N_Defining_Program_Unit_Name
        or else NT (N).Nkind = N_Formal_Object_Declaration
        or else NT (N).Nkind = N_Formal_Package_Declaration
        or else NT (N).Nkind = N_Formal_Type_Declaration
        or else NT (N).Nkind = N_Full_Type_Declaration
        or else NT (N).Nkind = N_Implicit_Label_Declaration
        or else NT (N).Nkind = N_Incomplete_Type_Declaration
        or else NT (N).Nkind = N_Loop_Parameter_Specification
        or else NT (N).Nkind = N_Number_Declaration
        or else NT (N).Nkind = N_Object_Declaration
        or else NT (N).Nkind = N_Object_Renaming_Declaration
        or else NT (N).Nkind = N_Parameter_Specification
        or else NT (N).Nkind = N_Private_Extension_Declaration
        or else NT (N).Nkind = N_Private_Type_Declaration
        or else NT (N).Nkind = N_Reactive_State
        or else NT (N).Nkind = N_Subtype_Declaration);
      return Node1 (N);
   end Defining_Identifier;

   function Defining_Unit_Name
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Function_Instantiation
        or else NT (N).Nkind = N_Function_Specification
        or else NT (N).Nkind = N_Generic_Function_Renaming_Declaration
        or else NT (N).Nkind = N_Generic_Package_Renaming_Declaration
        or else NT (N).Nkind = N_Generic_Procedure_Renaming_Declaration
        or else NT (N).Nkind = N_Package_Body
        or else NT (N).Nkind = N_Package_Instantiation
        or else NT (N).Nkind = N_Package_Renaming_Declaration
        or else NT (N).Nkind = N_Package_Specification
        or else NT (N).Nkind = N_Procedure_Instantiation
        or else NT (N).Nkind = N_Procedure_Specification);
      return Node1 (N);
   end Defining_Unit_Name;

   function Digits_Expression
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Floating_Point_Definition);
      return Node2 (N);
   end Digits_Expression;

   function Discrete_Choices
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Case_Statement_Alternative);
      return List4 (N);
   end Discrete_Choices;

   function Discrete_Range
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Slice);
      return Node4 (N);
   end Discrete_Range;

   function Discrete_Subtype_Definition
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Loop_Parameter_Specification);
      return Node4 (N);
   end Discrete_Subtype_Definition;

   function Discrete_Subtype_Definitions
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Constrained_Array_Definition);
      return List2 (N);
   end Discrete_Subtype_Definitions;

   function Do_Division_Check
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Op_Divide
        or else NT (N).Nkind = N_Op_Mod
        or else NT (N).Nkind = N_Op_Rem);
      return Flag13 (N);
   end Do_Division_Check;

   function Do_Length_Check
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Assignment_Statement
        or else NT (N).Nkind = N_Op_And
        or else NT (N).Nkind = N_Op_Or
        or else NT (N).Nkind = N_Op_Xor
        or else NT (N).Nkind = N_Type_Conversion);
      return Flag4 (N);
   end Do_Length_Check;

   function Do_Overflow_Check
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Op
        or else NT (N).Nkind = N_Attribute_Reference
        or else NT (N).Nkind = N_Type_Conversion);
      return Flag17 (N);
   end Do_Overflow_Check;

   function Do_Range_Check
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Subexpr);
      return Flag9 (N);
   end Do_Range_Check;

   function Do_Storage_Check
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Allocator
        or else NT (N).Nkind = N_Subprogram_Body);
      return Flag17 (N);
   end Do_Storage_Check;

   function Do_Tag_Check
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Assignment_Statement
        or else NT (N).Nkind = N_Function_Call
        or else NT (N).Nkind = N_Procedure_Call_Statement
        or else NT (N).Nkind = N_Return_Statement
        or else NT (N).Nkind = N_Type_Conversion);
      return Flag13 (N);
   end Do_Tag_Check;

   function Elaborate_All_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_With_Clause);
      return Flag15 (N);
   end Elaborate_All_Present;

   function Elaborate_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_With_Clause);
      return Flag4 (N);
   end Elaborate_Present;

   function Elaboration_Boolean
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Function_Specification
        or else NT (N).Nkind = N_Procedure_Specification);
      return Node2 (N);
   end Elaboration_Boolean;

   function Else_Actions
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Conditional_Expression);
      return List3 (N);
   end Else_Actions;

   function Else_Statements
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_If_Statement);
      return List4 (N);
   end Else_Statements;

   function Elsif_Parts
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_If_Statement);
      return List3 (N);
   end Elsif_Parts;

   function End_Label
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Enumeration_Type_Definition
        or else NT (N).Nkind = N_Handled_Sequence_Of_Statements
        or else NT (N).Nkind = N_Loop_Statement
        or else NT (N).Nkind = N_Package_Specification
        or else NT (N).Nkind = N_Record_Definition);
      return Node4 (N);
   end End_Label;

   function End_Span
      (N : Node_Id) return Uint is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Case_Statement
        or else NT (N).Nkind = N_If_Statement);
      return Uint5 (N);
   end End_Span;

   function Entity
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Has_Entity
        or else NT (N).Nkind = N_Freeze_Entity);
      return Node4 (N);
   end Entity;

   function Entity_Or_Associated_Node
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Has_Entity
        or else NT (N).Nkind = N_Freeze_Entity);
      return Node4 (N);
   end Entity_Or_Associated_Node;

   function Etype
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Has_Etype);
      return Node5 (N);
   end Etype;

   function Expansion_Delayed
     (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aggregate
        or else NT (N).Nkind = N_Extension_Aggregate);
      return Flag11 (N);
   end Expansion_Delayed;

   function Explicit_Actual_Parameter
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Parameter_Association);
      return Node3 (N);
   end Explicit_Actual_Parameter;

   function Explicit_Generic_Actual_Parameter
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Generic_Association);
      return Node1 (N);
   end Explicit_Generic_Actual_Parameter;

   function Expression
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Allocator
        or else NT (N).Nkind = N_Assignment_Statement
        or else NT (N).Nkind = N_At_Clause
        or else NT (N).Nkind = N_Attribute_Definition_Clause
        or else NT (N).Nkind = N_Case_Statement
        or else NT (N).Nkind = N_Code_Statement
        or else NT (N).Nkind = N_Component_Association
        or else NT (N).Nkind = N_Component_Declaration
        or else NT (N).Nkind = N_Discriminant_Association
        or else NT (N).Nkind = N_Formal_Object_Declaration
        or else NT (N).Nkind = N_Free_Statement
        or else NT (N).Nkind = N_Mod_Clause
        or else NT (N).Nkind = N_Modular_Type_Definition
        or else NT (N).Nkind = N_Number_Declaration
        or else NT (N).Nkind = N_Object_Declaration
        or else NT (N).Nkind = N_Parameter_Specification
        or else NT (N).Nkind = N_Pragma_Argument_Association
        or else NT (N).Nkind = N_Qualified_Expression
        or else NT (N).Nkind = N_Return_Statement
        or else NT (N).Nkind = N_Type_Conversion
        or else NT (N).Nkind = N_Unchecked_Expression
        or else NT (N).Nkind = N_Unchecked_Type_Conversion);
      return Node3 (N);
   end Expression;

   function Expressions
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aggregate
        or else NT (N).Nkind = N_Attribute_Reference
        or else NT (N).Nkind = N_Conditional_Expression
        or else NT (N).Nkind = N_Extension_Aggregate
        or else NT (N).Nkind = N_Indexed_Component);
      return List1 (N);
   end Expressions;

   function First_Bit
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Clause);
      return Node3 (N);
   end First_Bit;

   function First_Inlined_Subprogram
      (N : Node_Id) return Entity_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit);
      return Node3 (N);
   end First_Inlined_Subprogram;

   function First_Name
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_With_Clause);
      return Flag5 (N);
   end First_Name;

   function First_Named_Actual
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Function_Call
        or else NT (N).Nkind = N_Procedure_Call_Statement);
      return Node4 (N);
   end First_Named_Actual;

   function First_Real_Statement
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Handled_Sequence_Of_Statements);
      return Node2 (N);
   end First_Real_Statement;

   function First_Subtype_Link
      (N : Node_Id) return Entity_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Freeze_Entity);
      return Node5 (N);
   end First_Subtype_Link;

   function Float_Truncate
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Type_Conversion);
      return Flag11 (N);
   end Float_Truncate;

   function Formal_Type_Definition
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Formal_Type_Declaration);
      return Node3 (N);
   end Formal_Type_Definition;

   function Forwards_OK
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Assignment_Statement);
      return Flag5 (N);
   end Forwards_OK;

   function From_Aspect_Specification
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Attribute_Definition_Clause
        or else NT (N).Nkind = N_Pragma);
      return Flag13 (N);
   end From_Aspect_Specification;

   function From_At_Mod
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Attribute_Definition_Clause);
      return Flag4 (N);
   end From_At_Mod;

   function Generic_Associations
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Formal_Package_Declaration
        or else NT (N).Nkind = N_Function_Instantiation
        or else NT (N).Nkind = N_Package_Instantiation
        or else NT (N).Nkind = N_Procedure_Instantiation);
      return List3 (N);
   end Generic_Associations;

   function Generic_Formal_Declarations
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Generic_Package_Declaration
        or else NT (N).Nkind = N_Generic_Subprogram_Declaration);
      return List2 (N);
   end Generic_Formal_Declarations;

   function Generic_Parent
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Function_Specification
        or else NT (N).Nkind = N_Package_Specification
        or else NT (N).Nkind = N_Procedure_Specification);
      return Node5 (N);
   end Generic_Parent;

   function Generic_Parent_Type
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Subtype_Declaration);
      return Node4 (N);
   end Generic_Parent_Type;

   function Handled_Statement_Sequence
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Block_Statement
        or else NT (N).Nkind = N_Package_Body
        or else NT (N).Nkind = N_Subprogram_Body);
      return Node4 (N);
   end Handled_Statement_Sequence;

   function Has_Created_Identifier
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Block_Statement
        or else NT (N).Nkind = N_Loop_Statement);
      return Flag15 (N);
   end Has_Created_Identifier;

   function Has_Dynamic_Length_Check
      (N : Node_Id) return Boolean is
   begin
      return Flag10 (N);
   end Has_Dynamic_Length_Check;

   function Has_Dynamic_Range_Check
      (N : Node_Id) return Boolean is
   begin
      return Flag12 (N);
   end Has_Dynamic_Range_Check;

   function Has_No_Elaboration_Code
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit);
      return Flag17 (N);
   end Has_No_Elaboration_Code;

   function Has_Priority_Pragma
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Subprogram_Body);
      return Flag6 (N);
   end Has_Priority_Pragma;

   function Has_Private_View
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
       or else NT (N).Nkind in N_Op
       or else NT (N).Nkind = N_Character_Literal
       or else NT (N).Nkind = N_Expanded_Name
       or else NT (N).Nkind = N_Identifier);
--       or else NT (N).Nkind = N_Operator_Symbol);
      return Flag11 (N);
   end Has_Private_View;

   function Has_Wide_Character
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_String_Literal);
      return Flag11 (N);
   end Has_Wide_Character;

   function Hidden_By_Use_Clause
     (N : Node_Id) return Elist_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Use_Package_Clause
        or else NT (N).Nkind = N_Use_Type_Clause);
      return Elist4 (N);
   end Hidden_By_Use_Clause;

   function High_Bound
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Range
        or else NT (N).Nkind = N_Real_Range_Specification
        or else NT (N).Nkind = N_Signed_Integer_Type_Definition);
      return Node2 (N);
   end High_Bound;

   function Identifier
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_At_Clause
        or else NT (N).Nkind = N_Block_Statement
        or else NT (N).Nkind = N_Designator
        or else NT (N).Nkind = N_Enumeration_Representation_Clause
        or else NT (N).Nkind = N_Label
        or else NT (N).Nkind = N_Loop_Statement
        or else NT (N).Nkind = N_Record_Representation_Clause
        or else NT (N).Nkind = N_Subprogram_Info);
      return Node1 (N);
   end Identifier;

   function Implicit_With
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_With_Clause);
      return Flag16 (N);
   end Implicit_With;

   function Init_Procedure
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Reactive_Type);
      return Node4 (N);
   end Init_Procedure;

   function In_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Formal_Object_Declaration
        or else NT (N).Nkind = N_Parameter_Specification);
      return Flag15 (N);
   end In_Present;

   function Includes_Infinities
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Range);
      return Flag11 (N);
   end Includes_Infinities;

   function Instance_Spec
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Formal_Package_Declaration
        or else NT (N).Nkind = N_Function_Instantiation
        or else NT (N).Nkind = N_Package_Instantiation
        or else NT (N).Nkind = N_Procedure_Instantiation);
      return Node5 (N);
   end Instance_Spec;

   function Intval
      (N : Node_Id) return Uint is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Integer_Literal);
      return Uint3 (N);
   end Intval;

   function Is_Boolean_Aspect
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aspect_Specification);
      return Flag16 (N);
   end Is_Boolean_Aspect;

   function Is_Checked
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aspect_Specification
        or else NT (N).Nkind = N_Pragma);
      return Flag11 (N);
   end Is_Checked;

   function Is_Component_Left_Opnd
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Op_Concat);
      return Flag13 (N);
   end Is_Component_Left_Opnd;

   function Is_Component_Right_Opnd
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Op_Concat);
      return Flag14 (N);
   end Is_Component_Right_Opnd;

   function Is_Controlling_Actual
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Subexpr);
      return Flag16 (N);
   end Is_Controlling_Actual;

   function Is_Disabled
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aspect_Specification
        or else NT (N).Nkind = N_Pragma);
      return Flag15 (N);
   end Is_Disabled;

   function Is_Delayed_Aspect
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aspect_Specification
        or else NT (N).Nkind = N_Attribute_Definition_Clause
        or else NT (N).Nkind = N_Pragma);
      return Flag14 (N);
   end Is_Delayed_Aspect;

   function Is_Ignored
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aspect_Specification
        or else NT (N).Nkind = N_Pragma);
      return Flag9 (N);
   end Is_Ignored;

   function Is_Machine_Number
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Real_Literal);
      return Flag11 (N);
   end Is_Machine_Number;

   function Is_Null_Loop
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Loop_Statement);
      return Flag16 (N);
   end Is_Null_Loop;

   function Is_Overloaded
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Subexpr);
      return Flag5 (N);
   end Is_Overloaded;

   function Is_Power_Of_2_For_Shift
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Op_Expon);
      return Flag13 (N);
   end Is_Power_Of_2_For_Shift;

   function Is_Reactive
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Subprogram_Body
        or else NT (N).Nkind = N_Subprogram_Declaration
        or else NT (N).Nkind = N_Exit_Statement
        or else NT (N).Nkind = N_Loop_Statement
        or else NT (N).Nkind = N_Goto_Statement);
      return Flag18 (N);
   end Is_Reactive;

   function Is_Static_Expression
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Subexpr);
      return Flag6 (N);
   end Is_Static_Expression;

   function Is_Transient
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Subexpr);
      return Flag13 (N);
   end Is_Transient;

   function Iteration_Scheme
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Loop_Statement);
      return Node2 (N);
   end Iteration_Scheme;

   function Kill_Range_Check
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Unchecked_Type_Conversion);
      return Flag11 (N);
   end Kill_Range_Check;

   function Label_Construct
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Implicit_Label_Declaration);
      return Node2 (N);
   end Label_Construct;

   function Last_Bit
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Clause);
      return Node4 (N);
   end Last_Bit;

   function Last_Name
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_With_Clause);
      return Flag6 (N);
   end Last_Name;

   function Left_Opnd
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_And_Then
        or else NT (N).Nkind = N_In
        or else NT (N).Nkind = N_Not_In
        or else NT (N).Nkind = N_Or_Else
        or else NT (N).Nkind in N_Binary_Op);
      return Node2 (N);
   end Left_Opnd;

   function Library_Unit
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit
        or else NT (N).Nkind = N_With_Clause);
      return Node4 (N);
   end Library_Unit;

   function Literals
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Enumeration_Type_Definition);
      return List1 (N);
   end Literals;

   function Loop_Actions
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Association);
      return List2 (N);
   end Loop_Actions;

   function Loop_Parameter_Specification
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Iteration_Scheme);
      return Node4 (N);
   end Loop_Parameter_Specification;

   function Low_Bound
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Range
        or else NT (N).Nkind = N_Real_Range_Specification
        or else NT (N).Nkind = N_Signed_Integer_Type_Definition);
      return Node1 (N);
   end Low_Bound;

   function Mod_Clause
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Record_Representation_Clause);
      return Node2 (N);
   end Mod_Clause;

   function More_Ids
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Declaration
        or else NT (N).Nkind = N_Formal_Object_Declaration
        or else NT (N).Nkind = N_Number_Declaration
        or else NT (N).Nkind = N_Object_Declaration
        or else NT (N).Nkind = N_Parameter_Specification);
      return Flag5 (N);
   end More_Ids;

   function Must_Be_Byte_Aligned
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Attribute_Reference);
      return Flag14 (N);
   end Must_Be_Byte_Aligned;

   function Must_Not_Freeze
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Subtype_Indication
        or else NT (N).Nkind in N_Subexpr);
      return Flag8 (N);
   end Must_Not_Freeze;

   function Name
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Assignment_Statement
        or else NT (N).Nkind = N_Attribute_Definition_Clause
        or else NT (N).Nkind = N_Defining_Program_Unit_Name
        or else NT (N).Nkind = N_Designator
        or else NT (N).Nkind = N_Exit_Statement
        or else NT (N).Nkind = N_Formal_Package_Declaration
        or else NT (N).Nkind = N_Function_Call
        or else NT (N).Nkind = N_Function_Instantiation
        or else NT (N).Nkind = N_Generic_Function_Renaming_Declaration
        or else NT (N).Nkind = N_Generic_Package_Renaming_Declaration
        or else NT (N).Nkind = N_Generic_Procedure_Renaming_Declaration
        or else NT (N).Nkind = N_Goto_Statement
        or else NT (N).Nkind = N_Object_Renaming_Declaration
        or else NT (N).Nkind = N_Package_Instantiation
        or else NT (N).Nkind = N_Package_Renaming_Declaration
        or else NT (N).Nkind = N_Procedure_Call_Statement
        or else NT (N).Nkind = N_Procedure_Instantiation
        or else NT (N).Nkind = N_Subprogram_Renaming_Declaration
        or else NT (N).Nkind = N_With_Clause
        or else NT (N).Nkind = N_With_Type_Clause);
      return Node2 (N);
   end Name;

   function Names
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Use_Package_Clause);
      return List2 (N);
   end Names;

   function Next_Entity
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Defining_Character_Literal
        or else NT (N).Nkind = N_Defining_Identifier);
--        or else NT (N).Nkind = N_Defining_Operator_Symbol);
      return Node2 (N);
   end Next_Entity;

   function Next_Named_Actual
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Parameter_Association);
      return Node4 (N);
   end Next_Named_Actual;

   function Next_Rep_Item
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aspect_Specification
        or else NT (N).Nkind = N_Attribute_Definition_Clause
        or else NT (N).Nkind = N_Enumeration_Representation_Clause
        or else NT (N).Nkind = N_Pragma
        or else NT (N).Nkind = N_Record_Representation_Clause);
      return Node4 (N);
   end Next_Rep_Item;

   function Next_Use_Clause
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Use_Package_Clause
        or else NT (N).Nkind = N_Use_Type_Clause);
      return Node3 (N);
   end Next_Use_Clause;

   function No_Elaboration_Check
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Function_Call
        or else NT (N).Nkind = N_Procedure_Call_Statement);
      return Flag14 (N);
   end No_Elaboration_Check;

   function No_Entities_Ref_In_Spec
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_With_Clause);
      return Flag8 (N);
   end No_Entities_Ref_In_Spec;

   function No_Initialization
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Allocator
        or else NT (N).Nkind = N_Object_Declaration);
      return Flag13 (N);
   end No_Initialization;

   function No_Truncation
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Unchecked_Type_Conversion);
      return Flag17 (N);
   end No_Truncation;

   function Null_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_List
        or else NT (N).Nkind = N_Procedure_Specification
        or else NT (N).Nkind = N_Record_Definition);
      return Flag13 (N);
   end Null_Present;

   function Null_Record_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aggregate
        or else NT (N).Nkind = N_Extension_Aggregate);
      return Flag17 (N);
   end Null_Record_Present;

   function Object_Definition
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Object_Declaration);
      return Node4 (N);
   end Object_Definition;

   function Original_Entity
      (N : Node_Id) return Entity_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Integer_Literal
        or else NT (N).Nkind = N_Real_Literal);
      return Node2 (N);
   end Original_Entity;

   function Others_Discrete_Choices
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Others_Choice);
      return List1 (N);
   end Others_Discrete_Choices;

   function Out_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Formal_Object_Declaration
        or else NT (N).Nkind = N_Parameter_Specification);
      return Flag17 (N);
   end Out_Present;

   function Parameter_Associations
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Function_Call
        or else NT (N).Nkind = N_Procedure_Call_Statement);
      return List3 (N);
   end Parameter_Associations;

   function Parameter_List_Truncated
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Function_Call
        or else NT (N).Nkind = N_Procedure_Call_Statement);
      return Flag17 (N);
   end Parameter_List_Truncated;

   function Parameter_Specifications
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Access_Function_Definition
        or else NT (N).Nkind = N_Access_Procedure_Definition
        or else NT (N).Nkind = N_Function_Specification
        or else NT (N).Nkind = N_Procedure_Specification);
      return List3 (N);
   end Parameter_Specifications;

   function Parameter_Type
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Parameter_Specification);
      return Node2 (N);
   end Parameter_Type;

   function Parent_Spec
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Function_Instantiation
        or else NT (N).Nkind = N_Generic_Function_Renaming_Declaration
        or else NT (N).Nkind = N_Generic_Package_Declaration
        or else NT (N).Nkind = N_Generic_Package_Renaming_Declaration
        or else NT (N).Nkind = N_Generic_Procedure_Renaming_Declaration
        or else NT (N).Nkind = N_Generic_Subprogram_Declaration
        or else NT (N).Nkind = N_Package_Declaration
        or else NT (N).Nkind = N_Package_Instantiation
        or else NT (N).Nkind = N_Package_Renaming_Declaration
        or else NT (N).Nkind = N_Procedure_Instantiation
        or else NT (N).Nkind = N_Subprogram_Declaration
        or else NT (N).Nkind = N_Subprogram_Renaming_Declaration);
      return Node4 (N);
   end Parent_Spec;

   function Position
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Clause);
      return Node2 (N);
   end Position;

   function Pragma_Argument_Associations
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Pragma);
      return List2 (N);
   end Pragma_Argument_Associations;

   function Pragmas_After
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit_Aux);
      return List5 (N);
   end Pragmas_After;

   function Pragmas_Before
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Mod_Clause);
      return List4 (N);
   end Pragmas_Before;

   function Prefix
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Attribute_Reference
        or else NT (N).Nkind = N_Expanded_Name
        or else NT (N).Nkind = N_Explicit_Dereference
        or else NT (N).Nkind = N_Indexed_Component
        or else NT (N).Nkind = N_Reference
        or else NT (N).Nkind = N_Selected_Component
        or else NT (N).Nkind = N_Slice);
      return Node3 (N);
   end Prefix;

   function Prev_Ids
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Declaration
        or else NT (N).Nkind = N_Formal_Object_Declaration
        or else NT (N).Nkind = N_Number_Declaration
        or else NT (N).Nkind = N_Object_Declaration
        or else NT (N).Nkind = N_Parameter_Specification);
      return Flag6 (N);
   end Prev_Ids;

   function Print_In_Hex
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Integer_Literal);
      return Flag13 (N);
   end Print_In_Hex;

   function Private_Declarations
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Package_Specification);
      return List3 (N);
   end Private_Declarations;

   function Private_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit
        or else NT (N).Nkind = N_Formal_Derived_Type_Definition);
      return Flag15 (N);
   end Private_Present;

   function Raises_Constraint_Error
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Subexpr);
      return Flag7 (N);
   end Raises_Constraint_Error;

   function Range_Constraint
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Digits_Constraint);
      return Node4 (N);
   end Range_Constraint;

   function Range_Expression
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Range_Constraint);
      return Node4 (N);
   end Range_Expression;

   function React_Procedure
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Reactive_Type);
      return Node5 (N);
   end React_Procedure;

   function Real_Range_Specification
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Floating_Point_Definition);
      return Node4 (N);
   end Real_Range_Specification;

   function Realval
      (N : Node_Id) return Ureal is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Real_Literal);
      return Ureal3 (N);
   end Realval;

   function Record_Extension_Part
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Derived_Type_Definition);
      return Node3 (N);
   end Record_Extension_Part;

   function Redundant_Use
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Attribute_Reference
        or else NT (N).Nkind = N_Expanded_Name
        or else NT (N).Nkind = N_Identifier);
      return Flag13 (N);
   end Redundant_Use;

   function Return_Type
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Return_Statement);
      return Node2 (N);
   end Return_Type;

   function Reverse_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Loop_Parameter_Specification);
      return Flag15 (N);
   end Reverse_Present;

   function Right_Opnd
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Op
        or else NT (N).Nkind = N_And_Then
        or else NT (N).Nkind = N_In
        or else NT (N).Nkind = N_Not_In
        or else NT (N).Nkind = N_Or_Else);
      return Node3 (N);
   end Right_Opnd;

   function Scope
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Defining_Character_Literal
        or else NT (N).Nkind = N_Defining_Identifier);
--        or else NT (N).Nkind = N_Defining_Operator_Symbol);
      return Node3 (N);
   end Scope;

   function Selector_Name
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Expanded_Name
        or else NT (N).Nkind = N_Generic_Association
        or else NT (N).Nkind = N_Parameter_Association
        or else NT (N).Nkind = N_Selected_Component);
      return Node2 (N);
   end Selector_Name;

   function Selector_Names
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Discriminant_Association);
      return List1 (N);
   end Selector_Names;

   function Shift_Count_OK
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Op_Rotate_Left
        or else NT (N).Nkind = N_Op_Rotate_Right
        or else NT (N).Nkind = N_Op_Shift_Left
        or else NT (N).Nkind = N_Op_Shift_Right
        or else NT (N).Nkind = N_Op_Shift_Right_Arithmetic);
      return Flag4 (N);
   end Shift_Count_OK;

   function Source_Type
      (N : Node_Id) return Entity_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Validate_Unchecked_Conversion);
      return Node1 (N);
   end Source_Type;

   function Specification
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Abstract_Subprogram_Declaration
        or else NT (N).Nkind = N_Formal_Subprogram_Declaration
        or else NT (N).Nkind = N_Generic_Package_Declaration
        or else NT (N).Nkind = N_Generic_Subprogram_Declaration
        or else NT (N).Nkind = N_Package_Declaration
        or else NT (N).Nkind = N_Subprogram_Body
        or else NT (N).Nkind = N_Subprogram_Declaration
        or else NT (N).Nkind = N_Subprogram_Renaming_Declaration);
      return Node1 (N);
   end Specification;

   function Split_PPC
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aspect_Specification
        or else NT (N).Nkind = N_Pragma);
      return Flag17 (N);
   end Split_PPC;

   function Statements
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Case_Statement_Alternative
        or else NT (N).Nkind = N_Handled_Sequence_Of_Statements
        or else NT (N).Nkind = N_Loop_Statement
        or else NT (N).Nkind = N_Reactive_Abort_Handler
        or else NT (N).Nkind = N_Reactive_Abort_Statement
        or else NT (N).Nkind = N_Reactive_Fork_Alternative
        or else NT (N).Nkind = N_Reactive_Select_Alternative);
      return List3 (N);
   end Statements;

   function States
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Reactive_Type);
      return List2 (N);
   end States;

   function Static_Processing_OK
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aggregate);
      return Flag4 (N);
   end Static_Processing_OK;

   function State_Identifier
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Reactive_Pause_Statement
        or else NT (N).Nkind = N_Reactive_Select_Statement
        or else NT (N).Nkind = N_Reactive_Wait_Statement);
      return Node5 (N);
   end State_Identifier;

   function Storage_Pool
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Allocator
        or else NT (N).Nkind = N_Free_Statement
        or else NT (N).Nkind = N_Return_Statement);
      return Node1 (N);
   end Storage_Pool;

   function Strval
      (N : Node_Id) return String_Id is
   begin
      pragma Assert (False
  --      or else NT (N).Nkind = N_Operator_Symbol
        or else NT (N).Nkind = N_String_Literal);
      return Str3 (N);
   end Strval;

   function Subtype_Indication
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Access_To_Object_Definition
        or else NT (N).Nkind = N_Component_Definition
        or else NT (N).Nkind = N_Derived_Type_Definition
        or else NT (N).Nkind = N_Private_Extension_Declaration
        or else NT (N).Nkind = N_Subtype_Declaration);
      return Node5 (N);
   end Subtype_Indication;

   function Subtype_Mark
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Access_Definition
        or else NT (N).Nkind = N_Access_Function_Definition
        or else NT (N).Nkind = N_Formal_Derived_Type_Definition
        or else NT (N).Nkind = N_Formal_Object_Declaration
        or else NT (N).Nkind = N_Function_Specification
        or else NT (N).Nkind = N_Object_Renaming_Declaration
        or else NT (N).Nkind = N_Qualified_Expression
        or else NT (N).Nkind = N_Subtype_Indication
        or else NT (N).Nkind = N_Type_Conversion
        or else NT (N).Nkind = N_Unchecked_Type_Conversion);
      return Node4 (N);
   end Subtype_Mark;

   function Subtype_Marks
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Unconstrained_Array_Definition
        or else NT (N).Nkind = N_Use_Type_Clause);
      return List2 (N);
   end Subtype_Marks;

   function Tagged_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Formal_Private_Type_Definition
        or else NT (N).Nkind = N_Private_Type_Declaration
        or else NT (N).Nkind = N_Record_Definition
        or else NT (N).Nkind = N_With_Type_Clause);
      return Flag15 (N);
   end Tagged_Present;

   function Target_Type
      (N : Node_Id) return Entity_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Validate_Unchecked_Conversion);
      return Node2 (N);
   end Target_Type;

   function Then_Actions
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Conditional_Expression);
      return List2 (N);
   end Then_Actions;

   function Then_Statements
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Elsif_Part
        or else NT (N).Nkind = N_If_Statement);
      return List2 (N);
   end Then_Statements;

   function TSS_Elist
      (N : Node_Id) return Elist_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Freeze_Entity);
      return Elist3 (N);
   end TSS_Elist;

   function Type_Definition
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Full_Type_Declaration);
      return Node3 (N);
   end Type_Definition;

   function Unit
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit);
      return Node2 (N);
   end Unit;

   function Unreferenced_In_Spec
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_With_Clause);
      return Flag7 (N);
   end Unreferenced_In_Spec;

   function Visible_Declarations
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Package_Specification);
      return List2 (N);
   end Visible_Declarations;

   function Zero_Cost_Handling
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Handled_Sequence_Of_Statements);
      return Flag5 (N);
   end Zero_Cost_Handling;

   --------------------------
   -- Field Set Procedures --
   --------------------------

   procedure Set_ABE_Is_Certain
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Formal_Package_Declaration
        or else NT (N).Nkind = N_Function_Call
        or else NT (N).Nkind = N_Function_Instantiation
        or else NT (N).Nkind = N_Package_Instantiation
        or else NT (N).Nkind = N_Procedure_Call_Statement
        or else NT (N).Nkind = N_Procedure_Instantiation);
      Set_Flag18 (N, Val);
   end Set_ABE_Is_Certain;

   procedure Set_Abort_Handler
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Reactive_Abort_Statement);
      Set_Node4_With_Parent (N, Val);
   end Set_Abort_Handler;

   procedure Set_Abstract_Present
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Derived_Type_Definition
        or else NT (N).Nkind = N_Formal_Derived_Type_Definition
        or else NT (N).Nkind = N_Formal_Private_Type_Definition
        or else NT (N).Nkind = N_Private_Extension_Declaration
        or else NT (N).Nkind = N_Private_Type_Declaration
        or else NT (N).Nkind = N_Record_Definition);
      Set_Flag4 (N, Val);
   end Set_Abstract_Present;

   procedure Set_Access_Definition
     (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Definition
        or else NT (N).Nkind = N_Formal_Object_Declaration
        or else NT (N).Nkind = N_Object_Renaming_Declaration);
      Set_Node3_With_Parent (N, Val);
   end Set_Access_Definition;

   procedure Set_Access_To_Subprogram_Definition
     (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Access_Definition);
      Set_Node3_With_Parent (N, Val);
   end Set_Access_To_Subprogram_Definition;

   procedure Set_Access_Types_To_Process
      (N : Node_Id; Val : Elist_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Freeze_Entity);
      Set_Elist2 (N, Val); -- semantic field, no parent set
   end Set_Access_Types_To_Process;

   procedure Set_Actions
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_And_Then
        or else NT (N).Nkind = N_Compilation_Unit_Aux
        or else NT (N).Nkind = N_Freeze_Entity
        or else NT (N).Nkind = N_Or_Else);
      Set_List1_With_Parent (N, Val);
   end Set_Actions;

   procedure Set_Acts_As_Spec
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit
        or else NT (N).Nkind = N_Subprogram_Body);
      Set_Flag4 (N, Val);
   end Set_Acts_As_Spec;

   procedure Set_Aggregate_Bounds
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aggregate);
      Set_Node3 (N, Val); -- semantic field, no parent set
   end Set_Aggregate_Bounds;

   procedure Set_Aliased_Present
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Definition
        or else NT (N).Nkind = N_Object_Declaration);
      Set_Flag4 (N, Val);
   end Set_Aliased_Present;

   procedure Set_All_Others
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Others_Choice);
      Set_Flag11 (N, Val);
   end Set_All_Others;

   procedure Set_All_Present
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Access_Definition
        or else NT (N).Nkind = N_Access_To_Object_Definition);
      Set_Flag15 (N, Val);
   end Set_All_Present;

   procedure Set_Alternatives
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Case_Statement
        or else NT (N).Nkind = N_Reactive_Fork_Statement
        or else NT (N).Nkind = N_Reactive_Select_Statement);
      Set_List4_With_Parent (N, Val);
   end Set_Alternatives;

   procedure Set_Ancestor_Part
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Extension_Aggregate);
      Set_Node3_With_Parent (N, Val);
   end Set_Ancestor_Part;

   procedure Set_Array_Aggregate
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Enumeration_Representation_Clause);
      Set_Node3_With_Parent (N, Val);
   end Set_Array_Aggregate;

   procedure Set_Aspect_Rep_Item
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aspect_Specification);
      Set_Node2 (N, Val);
   end Set_Aspect_Rep_Item;

   procedure Set_Assignment_OK
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Object_Declaration
        or else NT (N).Nkind in N_Subexpr);
      Set_Flag15 (N, Val);
   end Set_Assignment_OK;

   procedure Set_Associated_Node
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Has_Entity
        or else NT (N).Nkind = N_Aggregate
        or else NT (N).Nkind = N_Extension_Aggregate
        or else NT (N).Nkind = N_Selected_Component);
      Set_Node4 (N, Val); -- semantic field, no parent set
   end Set_Associated_Node;

   procedure Set_At_End_Proc
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Handled_Sequence_Of_Statements);
      Set_Node1 (N, Val);
   end Set_At_End_Proc;

   procedure Set_Attribute_Name
      (N : Node_Id; Val : Name_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Attribute_Reference);
      Set_Name2 (N, Val);
   end Set_Attribute_Name;

   procedure Set_Aux_Decls_Node
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit);
      Set_Node5_With_Parent (N, Val);
   end Set_Aux_Decls_Node;

   procedure Set_Backwards_OK
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Assignment_Statement);
      Set_Flag6 (N, Val);
   end Set_Backwards_OK;

   procedure Set_Bad_Is_Detected
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Subprogram_Body);
      Set_Flag15 (N, Val);
   end Set_Bad_Is_Detected;

   procedure Set_Body_Required
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit);
      Set_Flag13 (N, Val);
   end Set_Body_Required;

   procedure Set_Body_To_Inline
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Subprogram_Declaration);
      Set_Node3 (N, Val);
   end Set_Body_To_Inline;

   procedure Set_Box_Present
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Association
        or else NT (N).Nkind = N_Formal_Package_Declaration
        or else NT (N).Nkind = N_Formal_Subprogram_Declaration);
      Set_Flag15 (N, Val);
   end Set_Box_Present;

   procedure Set_Char_Literal_Value
      (N : Node_Id; Val : Char_Code) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Character_Literal);
      null; -- Set_Char_Code (N, Val);
      --  Set_Char_Code2 (N, Val);
   end Set_Char_Literal_Value;

   procedure Set_Chars
      (N : Node_Id; Val : Name_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Has_Chars);
      Set_Name1 (N, Val);
   end Set_Chars;

   procedure Set_Check_Address_Alignment
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Attribute_Definition_Clause);
      Set_Flag11 (N, Val);
   end Set_Check_Address_Alignment;

   procedure Set_Choices
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Association);
      Set_List1_With_Parent (N, Val);
   end Set_Choices;

   procedure Set_Compile_Time_Known_Aggregate
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aggregate);
      Set_Flag18 (N, Val);
   end Set_Compile_Time_Known_Aggregate;

   procedure Set_Component_Associations
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aggregate
        or else NT (N).Nkind = N_Extension_Aggregate);
      Set_List2_With_Parent (N, Val);
   end Set_Component_Associations;

   procedure Set_Component_Clauses
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Record_Representation_Clause);
      Set_List3_With_Parent (N, Val);
   end Set_Component_Clauses;

   procedure Set_Component_Definition
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Declaration
        or else NT (N).Nkind = N_Constrained_Array_Definition
        or else NT (N).Nkind = N_Unconstrained_Array_Definition);
      Set_Node4_With_Parent (N, Val);
   end Set_Component_Definition;

   procedure Set_Component_Items
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_List);
      Set_List3_With_Parent (N, Val);
   end Set_Component_Items;

   procedure Set_Component_List
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Record_Definition);
      Set_Node1_With_Parent (N, Val);
   end Set_Component_List;

   procedure Set_Component_Name
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Clause);
      Set_Node1_With_Parent (N, Val);
   end Set_Component_Name;

   procedure Set_Condition
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Elsif_Part
        or else NT (N).Nkind = N_Exit_Statement
        or else NT (N).Nkind = N_If_Statement
        or else NT (N).Nkind = N_Iteration_Scheme
        or else NT (N).Nkind = N_Reactive_Abort_Statement
        or else NT (N).Nkind = N_Reactive_Fork_Statement);
      Set_Node1_With_Parent (N, Val);
   end Set_Condition;

   procedure Set_Condition_Actions
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Elsif_Part
        or else NT (N).Nkind = N_Iteration_Scheme);
      Set_List3 (N, Val); -- semantic field, no parent set
   end Set_Condition_Actions;

   procedure Set_Config_Pragmas
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit_Aux);
      Set_List4_With_Parent (N, Val);
   end Set_Config_Pragmas;

   procedure Set_Constant_Present
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Access_Definition
        or else NT (N).Nkind = N_Access_To_Object_Definition
        or else NT (N).Nkind = N_Object_Declaration);
      Set_Flag17 (N, Val);
   end Set_Constant_Present;

   procedure Set_Constraint
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Subtype_Indication);
      Set_Node3_With_Parent (N, Val);
   end Set_Constraint;

   procedure Set_Constraints
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Index_Or_Discriminant_Constraint);
      Set_List1_With_Parent (N, Val);
   end Set_Constraints;

   procedure Set_Context_Installed
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_With_Clause);
      Set_Flag13 (N, Val);
   end Set_Context_Installed;

   procedure Set_Context_Items
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit);
      Set_List1_With_Parent (N, Val);
   end Set_Context_Items;

   procedure Set_Context_Pending
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit);
      Set_Flag16 (N, Val);
   end Set_Context_Pending;

   procedure Set_Controlling_Argument
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Function_Call
        or else NT (N).Nkind = N_Procedure_Call_Statement);
      Set_Node1 (N, Val); -- semantic field, no parent set
   end Set_Controlling_Argument;

   procedure Set_Conversion_OK
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Type_Conversion);
      Set_Flag14 (N, Val);
   end Set_Conversion_OK;

   procedure Set_Corresponding_Aspect
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Pragma);
      Set_Node3 (N, Val);
   end Set_Corresponding_Aspect;

   procedure Set_Corresponding_Body
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Generic_Package_Declaration
        or else NT (N).Nkind = N_Generic_Subprogram_Declaration
        or else NT (N).Nkind = N_Package_Declaration
        or else NT (N).Nkind = N_Subprogram_Declaration);
      Set_Node5 (N, Val); -- semantic field, no parent set
   end Set_Corresponding_Body;

   procedure Set_Corresponding_Generic_Association
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Object_Declaration
        or else NT (N).Nkind = N_Object_Renaming_Declaration);
      Set_Node5 (N, Val); -- semantic field, no parent set
   end Set_Corresponding_Generic_Association;

   procedure Set_Corresponding_Node
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Reactive_State);
      Set_Node5_With_Parent (N, Val);
   end Set_Corresponding_Node;

   procedure Set_Corresponding_Spec
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Package_Body
        or else NT (N).Nkind = N_Subprogram_Body
        or else NT (N).Nkind = N_Subprogram_Renaming_Declaration
        or else NT (N).Nkind = N_With_Clause);
      Set_Node5 (N, Val); -- semantic field, no parent set
   end Set_Corresponding_Spec;

   procedure Set_Debug_Statement
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Pragma);
      Set_Node3_With_Parent (N, Val);
   end Set_Debug_Statement;

   procedure Set_Declarations
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Block_Statement
        or else NT (N).Nkind = N_Compilation_Unit_Aux
        or else NT (N).Nkind = N_Package_Body
        or else NT (N).Nkind = N_Subprogram_Body);
      Set_List2_With_Parent (N, Val);
   end Set_Declarations;

   procedure Set_Default_Expression
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Parameter_Specification);
      Set_Node5 (N, Val); -- semantic field, no parent set
   end Set_Default_Expression;

   procedure Set_Default_Name
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Formal_Subprogram_Declaration);
      Set_Node2_With_Parent (N, Val);
   end Set_Default_Name;

   procedure Set_Defining_Identifier
      (N : Node_Id; Val : Entity_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Declaration
        or else NT (N).Nkind = N_Defining_Program_Unit_Name
        or else NT (N).Nkind = N_Formal_Object_Declaration
        or else NT (N).Nkind = N_Formal_Package_Declaration
        or else NT (N).Nkind = N_Formal_Type_Declaration
        or else NT (N).Nkind = N_Full_Type_Declaration
        or else NT (N).Nkind = N_Implicit_Label_Declaration
        or else NT (N).Nkind = N_Incomplete_Type_Declaration
        or else NT (N).Nkind = N_Loop_Parameter_Specification
        or else NT (N).Nkind = N_Number_Declaration
        or else NT (N).Nkind = N_Object_Declaration
        or else NT (N).Nkind = N_Object_Renaming_Declaration
        or else NT (N).Nkind = N_Parameter_Specification
        or else NT (N).Nkind = N_Private_Extension_Declaration
        or else NT (N).Nkind = N_Private_Type_Declaration
        or else NT (N).Nkind = N_Reactive_State
        or else NT (N).Nkind = N_Subtype_Declaration);
      Set_Node1_With_Parent (N, Val);
   end Set_Defining_Identifier;

   procedure Set_Defining_Unit_Name
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Function_Instantiation
        or else NT (N).Nkind = N_Function_Specification
        or else NT (N).Nkind = N_Generic_Function_Renaming_Declaration
        or else NT (N).Nkind = N_Generic_Package_Renaming_Declaration
        or else NT (N).Nkind = N_Generic_Procedure_Renaming_Declaration
        or else NT (N).Nkind = N_Package_Body
        or else NT (N).Nkind = N_Package_Instantiation
        or else NT (N).Nkind = N_Package_Renaming_Declaration
        or else NT (N).Nkind = N_Package_Specification
        or else NT (N).Nkind = N_Procedure_Instantiation
        or else NT (N).Nkind = N_Procedure_Specification);
      Set_Node1_With_Parent (N, Val);
   end Set_Defining_Unit_Name;

   procedure Set_Digits_Expression
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Floating_Point_Definition);
      Set_Node2_With_Parent (N, Val);
   end Set_Digits_Expression;

   procedure Set_Discrete_Choices
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Case_Statement_Alternative);
      Set_List4_With_Parent (N, Val);
   end Set_Discrete_Choices;

   procedure Set_Discrete_Range
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Slice);
      Set_Node4_With_Parent (N, Val);
   end Set_Discrete_Range;

   procedure Set_Discrete_Subtype_Definition
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Loop_Parameter_Specification);
      Set_Node4_With_Parent (N, Val);
   end Set_Discrete_Subtype_Definition;

   procedure Set_Discrete_Subtype_Definitions
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Constrained_Array_Definition);
      Set_List2_With_Parent (N, Val);
   end Set_Discrete_Subtype_Definitions;

   procedure Set_Do_Division_Check
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Op_Divide
        or else NT (N).Nkind = N_Op_Mod
        or else NT (N).Nkind = N_Op_Rem);
      Set_Flag13 (N, Val);
   end Set_Do_Division_Check;

   procedure Set_Do_Length_Check
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Assignment_Statement
        or else NT (N).Nkind = N_Op_And
        or else NT (N).Nkind = N_Op_Or
        or else NT (N).Nkind = N_Op_Xor
        or else NT (N).Nkind = N_Type_Conversion);
      Set_Flag4 (N, Val);
   end Set_Do_Length_Check;

   procedure Set_Do_Overflow_Check
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Op
        or else NT (N).Nkind = N_Attribute_Reference
        or else NT (N).Nkind = N_Type_Conversion);
      Set_Flag17 (N, Val);
   end Set_Do_Overflow_Check;

   procedure Set_Do_Range_Check
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Subexpr);
      Set_Flag9 (N, Val);
   end Set_Do_Range_Check;

   procedure Set_Do_Storage_Check
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Allocator
        or else NT (N).Nkind = N_Subprogram_Body);
      Set_Flag17 (N, Val);
   end Set_Do_Storage_Check;

   procedure Set_Do_Tag_Check
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Assignment_Statement
        or else NT (N).Nkind = N_Function_Call
        or else NT (N).Nkind = N_Procedure_Call_Statement
        or else NT (N).Nkind = N_Return_Statement
        or else NT (N).Nkind = N_Type_Conversion);
      Set_Flag13 (N, Val);
   end Set_Do_Tag_Check;

   procedure Set_Elaborate_All_Present
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_With_Clause);
      Set_Flag15 (N, Val);
   end Set_Elaborate_All_Present;

   procedure Set_Elaborate_Present
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_With_Clause);
      Set_Flag4 (N, Val);
   end Set_Elaborate_Present;

   procedure Set_Elaboration_Boolean
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Function_Specification
        or else NT (N).Nkind = N_Procedure_Specification);
      Set_Node2 (N, Val);
   end Set_Elaboration_Boolean;

   procedure Set_Else_Actions
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Conditional_Expression);
      Set_List3 (N, Val); -- semantic field, no parent set
   end Set_Else_Actions;

   procedure Set_Else_Statements
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_If_Statement);
      Set_List4_With_Parent (N, Val);
   end Set_Else_Statements;

   procedure Set_Elsif_Parts
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_If_Statement);
      Set_List3_With_Parent (N, Val);
   end Set_Elsif_Parts;

   procedure Set_End_Label
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Enumeration_Type_Definition
        or else NT (N).Nkind = N_Handled_Sequence_Of_Statements
        or else NT (N).Nkind = N_Loop_Statement
        or else NT (N).Nkind = N_Package_Specification
        or else NT (N).Nkind = N_Record_Definition);
      Set_Node4_With_Parent (N, Val);
   end Set_End_Label;

   procedure Set_End_Span
      (N : Node_Id; Val : Uint) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Case_Statement
        or else NT (N).Nkind = N_If_Statement);
      Set_Uint5 (N, Val);
   end Set_End_Span;

   procedure Set_Entity
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Has_Entity
        or else NT (N).Nkind = N_Freeze_Entity);
      Set_Node4 (N, Val); -- semantic field, no parent set
      Reflex.Infos.Set_Entity_Reference (Val, N);
   end Set_Entity;

   procedure Set_Etype
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Has_Etype);
      Set_Node5 (N, Val); -- semantic field, no parent set
   end Set_Etype;

   procedure Set_Expansion_Delayed
     (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aggregate
        or else NT (N).Nkind = N_Extension_Aggregate);
      Set_Flag11 (N, Val);
   end Set_Expansion_Delayed;

   procedure Set_Explicit_Actual_Parameter
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Parameter_Association);
      Set_Node3_With_Parent (N, Val);
   end Set_Explicit_Actual_Parameter;

   procedure Set_Explicit_Generic_Actual_Parameter
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Generic_Association);
      Set_Node1_With_Parent (N, Val);
   end Set_Explicit_Generic_Actual_Parameter;

   procedure Set_Expression
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Allocator
        or else NT (N).Nkind = N_Assignment_Statement
        or else NT (N).Nkind = N_At_Clause
        or else NT (N).Nkind = N_Attribute_Definition_Clause
        or else NT (N).Nkind = N_Case_Statement
        or else NT (N).Nkind = N_Code_Statement
        or else NT (N).Nkind = N_Component_Association
        or else NT (N).Nkind = N_Component_Declaration
        or else NT (N).Nkind = N_Discriminant_Association
        or else NT (N).Nkind = N_Formal_Object_Declaration
        or else NT (N).Nkind = N_Free_Statement
        or else NT (N).Nkind = N_Mod_Clause
        or else NT (N).Nkind = N_Modular_Type_Definition
        or else NT (N).Nkind = N_Number_Declaration
        or else NT (N).Nkind = N_Object_Declaration
        or else NT (N).Nkind = N_Parameter_Specification
        or else NT (N).Nkind = N_Pragma_Argument_Association
        or else NT (N).Nkind = N_Qualified_Expression
        or else NT (N).Nkind = N_Return_Statement
        or else NT (N).Nkind = N_Type_Conversion
        or else NT (N).Nkind = N_Unchecked_Expression
        or else NT (N).Nkind = N_Unchecked_Type_Conversion);
      Set_Node3_With_Parent (N, Val);
   end Set_Expression;

   procedure Set_Expressions
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aggregate
        or else NT (N).Nkind = N_Attribute_Reference
        or else NT (N).Nkind = N_Conditional_Expression
        or else NT (N).Nkind = N_Extension_Aggregate
        or else NT (N).Nkind = N_Indexed_Component);
      Set_List1_With_Parent (N, Val);
   end Set_Expressions;

   procedure Set_First_Bit
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Clause);
      Set_Node3_With_Parent (N, Val);
   end Set_First_Bit;

   procedure Set_First_Inlined_Subprogram
      (N : Node_Id; Val : Entity_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit);
      Set_Node3 (N, Val);  -- semantic field, no parent set
   end Set_First_Inlined_Subprogram;

   procedure Set_First_Name
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_With_Clause);
      Set_Flag5 (N, Val);
   end Set_First_Name;

   procedure Set_First_Named_Actual
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Function_Call
        or else NT (N).Nkind = N_Procedure_Call_Statement);
      Set_Node4 (N, Val); -- semantic field, no parent set
   end Set_First_Named_Actual;

   procedure Set_First_Real_Statement
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Handled_Sequence_Of_Statements);
      Set_Node2 (N, Val); -- semantic field, no parent set
   end Set_First_Real_Statement;

   procedure Set_First_Subtype_Link
      (N : Node_Id; Val : Entity_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Freeze_Entity);
      Set_Node5 (N, Val); -- semantic field, no parent set
   end Set_First_Subtype_Link;

   procedure Set_Float_Truncate
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Type_Conversion);
      Set_Flag11 (N, Val);
   end Set_Float_Truncate;

   procedure Set_Formal_Type_Definition
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Formal_Type_Declaration);
      Set_Node3_With_Parent (N, Val);
   end Set_Formal_Type_Definition;

   procedure Set_Forwards_OK
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Assignment_Statement);
      Set_Flag5 (N, Val);
   end Set_Forwards_OK;

   procedure Set_From_Aspect_Specification
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Attribute_Definition_Clause
        or else NT (N).Nkind = N_Pragma);
      Set_Flag13 (N, Val);
   end Set_From_Aspect_Specification;

   procedure Set_From_At_Mod
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Attribute_Definition_Clause);
      Set_Flag4 (N, Val);
   end Set_From_At_Mod;

   procedure Set_Generic_Associations
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Formal_Package_Declaration
        or else NT (N).Nkind = N_Function_Instantiation
        or else NT (N).Nkind = N_Package_Instantiation
        or else NT (N).Nkind = N_Procedure_Instantiation);
      Set_List3_With_Parent (N, Val);
   end Set_Generic_Associations;

   procedure Set_Generic_Formal_Declarations
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Generic_Package_Declaration
        or else NT (N).Nkind = N_Generic_Subprogram_Declaration);
      Set_List2_With_Parent (N, Val);
   end Set_Generic_Formal_Declarations;

   procedure Set_Generic_Parent
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Function_Specification
        or else NT (N).Nkind = N_Package_Specification
        or else NT (N).Nkind = N_Procedure_Specification);
      Set_Node5 (N, Val);
   end Set_Generic_Parent;

   procedure Set_Generic_Parent_Type
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Subtype_Declaration);
      Set_Node4 (N, Val);
   end Set_Generic_Parent_Type;

   procedure Set_Handled_Statement_Sequence
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Block_Statement
        or else NT (N).Nkind = N_Package_Body
        or else NT (N).Nkind = N_Subprogram_Body);
      Set_Node4_With_Parent (N, Val);
   end Set_Handled_Statement_Sequence;

   procedure Set_Has_Created_Identifier
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Block_Statement
        or else NT (N).Nkind = N_Loop_Statement);
      Set_Flag15 (N, Val);
   end Set_Has_Created_Identifier;

   procedure Set_Has_Dynamic_Length_Check
      (N : Node_Id; Val : Boolean := True) is
   begin
      Set_Flag10 (N, Val);
   end Set_Has_Dynamic_Length_Check;

   procedure Set_Has_Dynamic_Range_Check
      (N : Node_Id; Val : Boolean := True) is
   begin
      Set_Flag12 (N, Val);
   end Set_Has_Dynamic_Range_Check;

   procedure Set_Has_No_Elaboration_Code
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit);
      Set_Flag17 (N, Val);
   end Set_Has_No_Elaboration_Code;

   procedure Set_Has_Priority_Pragma
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Subprogram_Body);
      Set_Flag6 (N, Val);
   end Set_Has_Priority_Pragma;

   procedure Set_Has_Private_View
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
       or else NT (N).Nkind in N_Op
       or else NT (N).Nkind = N_Character_Literal
       or else NT (N).Nkind = N_Expanded_Name
       or else NT (N).Nkind = N_Identifier);
--       or else NT (N).Nkind = N_Operator_Symbol);
      Set_Flag11 (N, Val);
   end Set_Has_Private_View;

   procedure Set_Has_Wide_Character
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_String_Literal);
      Set_Flag11 (N, Val);
   end Set_Has_Wide_Character;

   procedure Set_Hidden_By_Use_Clause
     (N : Node_Id; Val : Elist_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Use_Package_Clause
        or else NT (N).Nkind = N_Use_Type_Clause);
      Set_Elist4 (N, Val);
   end Set_Hidden_By_Use_Clause;

   procedure Set_High_Bound
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Range
        or else NT (N).Nkind = N_Real_Range_Specification
        or else NT (N).Nkind = N_Signed_Integer_Type_Definition);
      Set_Node2_With_Parent (N, Val);
   end Set_High_Bound;

   procedure Set_Identifier
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_At_Clause
        or else NT (N).Nkind = N_Block_Statement
        or else NT (N).Nkind = N_Designator
        or else NT (N).Nkind = N_Enumeration_Representation_Clause
        or else NT (N).Nkind = N_Label
        or else NT (N).Nkind = N_Loop_Statement
        or else NT (N).Nkind = N_Record_Representation_Clause
        or else NT (N).Nkind = N_Subprogram_Info);
      Set_Node1_With_Parent (N, Val);
   end Set_Identifier;

   procedure Set_Implicit_With
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_With_Clause);
      Set_Flag16 (N, Val);
   end Set_Implicit_With;

   procedure Set_Init_Procedure
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Reactive_Type);
      Set_Node4_With_Parent (N, Val);
   end Set_Init_Procedure;

   procedure Set_In_Present
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Formal_Object_Declaration
        or else NT (N).Nkind = N_Parameter_Specification);
      Set_Flag15 (N, Val);
   end Set_In_Present;

   procedure Set_Includes_Infinities
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Range);
      Set_Flag11 (N, Val);
   end Set_Includes_Infinities;

   procedure Set_Instance_Spec
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Formal_Package_Declaration
        or else NT (N).Nkind = N_Function_Instantiation
        or else NT (N).Nkind = N_Package_Instantiation
        or else NT (N).Nkind = N_Procedure_Instantiation);
      Set_Node5 (N, Val); -- semantic field, no Parent set
   end Set_Instance_Spec;

   procedure Set_Intval
      (N : Node_Id; Val : Uint) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Integer_Literal);
      Set_Uint3 (N, Val);
   end Set_Intval;

   procedure Set_Is_Boolean_Aspect
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aspect_Specification);
      Set_Flag16 (N, Val);
   end Set_Is_Boolean_Aspect;

   procedure Set_Is_Checked
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aspect_Specification
        or else NT (N).Nkind = N_Pragma);
      Set_Flag11 (N, Val);
   end Set_Is_Checked;

   procedure Set_Is_Component_Left_Opnd
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Op_Concat);
      Set_Flag13 (N, Val);
   end Set_Is_Component_Left_Opnd;

   procedure Set_Is_Component_Right_Opnd
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Op_Concat);
      Set_Flag14 (N, Val);
   end Set_Is_Component_Right_Opnd;

   procedure Set_Is_Delayed_Aspect
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aspect_Specification
        or else NT (N).Nkind = N_Attribute_Definition_Clause
        or else NT (N).Nkind = N_Pragma);
      Set_Flag14 (N, Val);
   end Set_Is_Delayed_Aspect;

   procedure Set_Is_Ignored
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aspect_Specification
        or else NT (N).Nkind = N_Pragma);
      Set_Flag9 (N, Val);
   end Set_Is_Ignored;

   procedure Set_Is_Disabled
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aspect_Specification
        or else NT (N).Nkind = N_Pragma);
      Set_Flag15 (N, Val);
   end Set_Is_Disabled;

   procedure Set_Is_Controlling_Actual
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Subexpr);
      Set_Flag16 (N, Val);
   end Set_Is_Controlling_Actual;

   procedure Set_Is_Machine_Number
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Real_Literal);
      Set_Flag11 (N, Val);
   end Set_Is_Machine_Number;

   procedure Set_Is_Null_Loop
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Loop_Statement);
      Set_Flag16 (N, Val);
   end Set_Is_Null_Loop;

   procedure Set_Is_Overloaded
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Subexpr);
      Set_Flag5 (N, Val);
   end Set_Is_Overloaded;

   procedure Set_Is_Power_Of_2_For_Shift
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Op_Expon);
      Set_Flag13 (N, Val);
   end Set_Is_Power_Of_2_For_Shift;

   procedure Set_Is_Reactive
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Subprogram_Body
        or else NT (N).Nkind = N_Subprogram_Declaration
        or else NT (N).Nkind = N_Exit_Statement
        or else NT (N).Nkind = N_Loop_Statement
        or else NT (N).Nkind = N_Goto_Statement);
      Set_Flag18 (N, Val);
   end Set_Is_Reactive;

   procedure Set_Is_Static_Expression
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Subexpr);
      Set_Flag6 (N, Val);
   end Set_Is_Static_Expression;

   procedure Set_Is_Transient
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Subexpr);
      Set_Flag13 (N, Val);
   end Set_Is_Transient;

   procedure Set_Iteration_Scheme
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Loop_Statement);
      Set_Node2_With_Parent (N, Val);
   end Set_Iteration_Scheme;

   procedure Set_Kill_Range_Check
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Unchecked_Type_Conversion);
      Set_Flag11 (N, Val);
   end Set_Kill_Range_Check;

   procedure Set_Label_Construct
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Implicit_Label_Declaration);
      Set_Node2 (N, Val); -- semantic field, no parent set
   end Set_Label_Construct;

   procedure Set_Last_Bit
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Clause);
      Set_Node4_With_Parent (N, Val);
   end Set_Last_Bit;

   procedure Set_Last_Name
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_With_Clause);
      Set_Flag6 (N, Val);
   end Set_Last_Name;

   procedure Set_Left_Opnd
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_And_Then
        or else NT (N).Nkind = N_In
        or else NT (N).Nkind = N_Not_In
        or else NT (N).Nkind = N_Or_Else
        or else NT (N).Nkind in N_Binary_Op);
      Set_Node2_With_Parent (N, Val);
   end Set_Left_Opnd;

   procedure Set_Library_Unit
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit
        or else NT (N).Nkind = N_With_Clause);
      Set_Node4 (N, Val); -- semantic field, no parent set
   end Set_Library_Unit;

   procedure Set_Literals
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Enumeration_Type_Definition);
      Set_List1_With_Parent (N, Val);
   end Set_Literals;

   procedure Set_Loop_Actions
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Association);
      Set_List2 (N, Val); -- semantic field, no parent set
   end Set_Loop_Actions;

   procedure Set_Loop_Parameter_Specification
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Iteration_Scheme);
      Set_Node4_With_Parent (N, Val);
   end Set_Loop_Parameter_Specification;

   procedure Set_Low_Bound
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Range
        or else NT (N).Nkind = N_Real_Range_Specification
        or else NT (N).Nkind = N_Signed_Integer_Type_Definition);
      Set_Node1_With_Parent (N, Val);
   end Set_Low_Bound;

   procedure Set_Mod_Clause
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Record_Representation_Clause);
      Set_Node2_With_Parent (N, Val);
   end Set_Mod_Clause;

   procedure Set_More_Ids
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Declaration
        or else NT (N).Nkind = N_Formal_Object_Declaration
        or else NT (N).Nkind = N_Number_Declaration
        or else NT (N).Nkind = N_Object_Declaration
        or else NT (N).Nkind = N_Parameter_Specification);
      Set_Flag5 (N, Val);
   end Set_More_Ids;

   procedure Set_Must_Be_Byte_Aligned
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Attribute_Reference);
      Set_Flag14 (N, Val);
   end Set_Must_Be_Byte_Aligned;

   procedure Set_Must_Not_Freeze
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Subtype_Indication
        or else NT (N).Nkind in N_Subexpr);
      Set_Flag8 (N, Val);
   end Set_Must_Not_Freeze;

   procedure Set_Name
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Assignment_Statement
        or else NT (N).Nkind = N_Attribute_Definition_Clause
        or else NT (N).Nkind = N_Defining_Program_Unit_Name
        or else NT (N).Nkind = N_Designator
        or else NT (N).Nkind = N_Exit_Statement
        or else NT (N).Nkind = N_Formal_Package_Declaration
        or else NT (N).Nkind = N_Function_Call
        or else NT (N).Nkind = N_Function_Instantiation
        or else NT (N).Nkind = N_Generic_Function_Renaming_Declaration
        or else NT (N).Nkind = N_Generic_Package_Renaming_Declaration
        or else NT (N).Nkind = N_Generic_Procedure_Renaming_Declaration
        or else NT (N).Nkind = N_Goto_Statement
        or else NT (N).Nkind = N_Object_Renaming_Declaration
        or else NT (N).Nkind = N_Package_Instantiation
        or else NT (N).Nkind = N_Package_Renaming_Declaration
        or else NT (N).Nkind = N_Procedure_Call_Statement
        or else NT (N).Nkind = N_Procedure_Instantiation
        or else NT (N).Nkind = N_Subprogram_Renaming_Declaration
        or else NT (N).Nkind = N_With_Clause
        or else NT (N).Nkind = N_With_Type_Clause);
      Set_Node2_With_Parent (N, Val);
   end Set_Name;

   procedure Set_Names
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Use_Package_Clause);
      Set_List2_With_Parent (N, Val);
   end Set_Names;

   procedure Set_Next_Entity
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Defining_Character_Literal
        or else NT (N).Nkind = N_Defining_Identifier);
--        or else NT (N).Nkind = N_Defining_Operator_Symbol);
      Set_Node2 (N, Val); -- semantic field, no parent set
   end Set_Next_Entity;

   procedure Set_Next_Named_Actual
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Parameter_Association);
      Set_Node4 (N, Val); -- semantic field, no parent set
   end Set_Next_Named_Actual;

   procedure Set_Next_Rep_Item
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aspect_Specification
        or else NT (N).Nkind = N_Attribute_Definition_Clause
        or else NT (N).Nkind = N_Enumeration_Representation_Clause
        or else NT (N).Nkind = N_Pragma
        or else NT (N).Nkind = N_Record_Representation_Clause);
      Set_Node4 (N, Val); -- semantic field, no parent set
   end Set_Next_Rep_Item;

   procedure Set_Next_Use_Clause
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Use_Package_Clause
        or else NT (N).Nkind = N_Use_Type_Clause);
      Set_Node3 (N, Val); -- semantic field, no parent set
   end Set_Next_Use_Clause;

   procedure Set_No_Elaboration_Check
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Function_Call
        or else NT (N).Nkind = N_Procedure_Call_Statement);
      Set_Flag14 (N, Val);
   end Set_No_Elaboration_Check;

   procedure Set_No_Entities_Ref_In_Spec
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_With_Clause);
      Set_Flag8 (N, Val);
   end Set_No_Entities_Ref_In_Spec;

   procedure Set_No_Initialization
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Allocator
        or else NT (N).Nkind = N_Object_Declaration);
      Set_Flag13 (N, Val);
   end Set_No_Initialization;

   procedure Set_No_Truncation
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Unchecked_Type_Conversion);
      Set_Flag17 (N, Val);
   end Set_No_Truncation;

   procedure Set_Null_Present
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_List
        or else NT (N).Nkind = N_Procedure_Specification
        or else NT (N).Nkind = N_Record_Definition);
      Set_Flag13 (N, Val);
   end Set_Null_Present;

   procedure Set_Null_Record_Present
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aggregate
        or else NT (N).Nkind = N_Extension_Aggregate);
      Set_Flag17 (N, Val);
   end Set_Null_Record_Present;

   procedure Set_Object_Definition
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Object_Declaration);
      Set_Node4_With_Parent (N, Val);
   end Set_Object_Definition;

   procedure Set_Original_Entity
      (N : Node_Id; Val : Entity_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Integer_Literal
        or else NT (N).Nkind = N_Real_Literal);
      Set_Node2 (N, Val); --  semantic field, no parent set
   end Set_Original_Entity;

   procedure Set_Others_Discrete_Choices
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Others_Choice);
      Set_List1_With_Parent (N, Val);
   end Set_Others_Discrete_Choices;

   procedure Set_Out_Present
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Formal_Object_Declaration
        or else NT (N).Nkind = N_Parameter_Specification);
      Set_Flag17 (N, Val);
   end Set_Out_Present;

   procedure Set_Parameter_Associations
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Function_Call
        or else NT (N).Nkind = N_Procedure_Call_Statement);
      Set_List3_With_Parent (N, Val);
   end Set_Parameter_Associations;

   procedure Set_Parameter_List_Truncated
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Function_Call
        or else NT (N).Nkind = N_Procedure_Call_Statement);
      Set_Flag17 (N, Val);
   end Set_Parameter_List_Truncated;

   procedure Set_Parameter_Specifications
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Access_Function_Definition
        or else NT (N).Nkind = N_Access_Procedure_Definition
        or else NT (N).Nkind = N_Function_Specification
        or else NT (N).Nkind = N_Procedure_Specification);
      Set_List3_With_Parent (N, Val);
   end Set_Parameter_Specifications;

   procedure Set_Parameter_Type
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Parameter_Specification);
      Set_Node2_With_Parent (N, Val);
   end Set_Parameter_Type;

   procedure Set_Parent_Spec
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Function_Instantiation
        or else NT (N).Nkind = N_Generic_Function_Renaming_Declaration
        or else NT (N).Nkind = N_Generic_Package_Declaration
        or else NT (N).Nkind = N_Generic_Package_Renaming_Declaration
        or else NT (N).Nkind = N_Generic_Procedure_Renaming_Declaration
        or else NT (N).Nkind = N_Generic_Subprogram_Declaration
        or else NT (N).Nkind = N_Package_Declaration
        or else NT (N).Nkind = N_Package_Instantiation
        or else NT (N).Nkind = N_Package_Renaming_Declaration
        or else NT (N).Nkind = N_Procedure_Instantiation
        or else NT (N).Nkind = N_Subprogram_Declaration
        or else NT (N).Nkind = N_Subprogram_Renaming_Declaration);
      Set_Node4 (N, Val); -- semantic field, no parent set
   end Set_Parent_Spec;

   procedure Set_Position
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Clause);
      Set_Node2_With_Parent (N, Val);
   end Set_Position;

   procedure Set_Pragma_Argument_Associations
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Pragma);
      Set_List2_With_Parent (N, Val);
   end Set_Pragma_Argument_Associations;

   procedure Set_Pragmas_After
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit_Aux);
      Set_List5_With_Parent (N, Val);
   end Set_Pragmas_After;

   procedure Set_Pragmas_Before
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Mod_Clause);
      Set_List4_With_Parent (N, Val);
   end Set_Pragmas_Before;

   procedure Set_Prefix
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Attribute_Reference
        or else NT (N).Nkind = N_Expanded_Name
        or else NT (N).Nkind = N_Explicit_Dereference
        or else NT (N).Nkind = N_Indexed_Component
        or else NT (N).Nkind = N_Reference
        or else NT (N).Nkind = N_Selected_Component
        or else NT (N).Nkind = N_Slice);
      Set_Node3_With_Parent (N, Val);
   end Set_Prefix;

   procedure Set_Prev_Ids
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Component_Declaration
        or else NT (N).Nkind = N_Formal_Object_Declaration
        or else NT (N).Nkind = N_Number_Declaration
        or else NT (N).Nkind = N_Object_Declaration
        or else NT (N).Nkind = N_Parameter_Specification);
      Set_Flag6 (N, Val);
   end Set_Prev_Ids;

   procedure Set_Print_In_Hex
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Integer_Literal);
      Set_Flag13 (N, Val);
   end Set_Print_In_Hex;

   procedure Set_Private_Declarations
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Package_Specification);
      Set_List3_With_Parent (N, Val);
   end Set_Private_Declarations;

   procedure Set_Private_Present
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit
        or else NT (N).Nkind = N_Formal_Derived_Type_Definition);
      Set_Flag15 (N, Val);
   end Set_Private_Present;

   procedure Set_Raises_Constraint_Error
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Subexpr);
      Set_Flag7 (N, Val);
   end Set_Raises_Constraint_Error;

   procedure Set_Range_Constraint
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Digits_Constraint);
      Set_Node4_With_Parent (N, Val);
   end Set_Range_Constraint;

   procedure Set_Range_Expression
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Range_Constraint);
      Set_Node4_With_Parent (N, Val);
   end Set_Range_Expression;

   procedure Set_React_Procedure
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Reactive_Type);
      Set_Node5_With_Parent (N, Val);
   end Set_React_Procedure;

   procedure Set_Real_Range_Specification
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Floating_Point_Definition);
      Set_Node4_With_Parent (N, Val);
   end Set_Real_Range_Specification;

   procedure Set_Realval
     (N : Node_Id; Val : Ureal) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Real_Literal);
      Set_Ureal3 (N, Val);
   end Set_Realval;

   procedure Set_Record_Extension_Part
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Derived_Type_Definition);
      Set_Node3_With_Parent (N, Val);
   end Set_Record_Extension_Part;

   procedure Set_Redundant_Use
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Attribute_Reference
        or else NT (N).Nkind = N_Expanded_Name
        or else NT (N).Nkind = N_Identifier);
      Set_Flag13 (N, Val);
   end Set_Redundant_Use;

   procedure Set_Return_Type
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Return_Statement);
      Set_Node2 (N, Val); -- semantic field, no parent set
   end Set_Return_Type;

   procedure Set_Reverse_Present
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Loop_Parameter_Specification);
      Set_Flag15 (N, Val);
   end Set_Reverse_Present;

   procedure Set_Right_Opnd
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind in N_Op
        or else NT (N).Nkind = N_And_Then
        or else NT (N).Nkind = N_In
        or else NT (N).Nkind = N_Not_In
        or else NT (N).Nkind = N_Or_Else);
      Set_Node3_With_Parent (N, Val);
   end Set_Right_Opnd;

   procedure Set_Scope
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Defining_Character_Literal
        or else NT (N).Nkind = N_Defining_Identifier);
--        or else NT (N).Nkind = N_Defining_Operator_Symbol);
      Set_Node3 (N, Val); -- semantic field, no parent set
   end Set_Scope;

   procedure Set_Selector_Name
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Expanded_Name
        or else NT (N).Nkind = N_Generic_Association
        or else NT (N).Nkind = N_Parameter_Association
        or else NT (N).Nkind = N_Selected_Component);
      Set_Node2_With_Parent (N, Val);
   end Set_Selector_Name;

   procedure Set_Selector_Names
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Discriminant_Association);
      Set_List1_With_Parent (N, Val);
   end Set_Selector_Names;

   procedure Set_Shift_Count_OK
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Op_Rotate_Left
        or else NT (N).Nkind = N_Op_Rotate_Right
        or else NT (N).Nkind = N_Op_Shift_Left
        or else NT (N).Nkind = N_Op_Shift_Right
        or else NT (N).Nkind = N_Op_Shift_Right_Arithmetic);
      Set_Flag4 (N, Val);
   end Set_Shift_Count_OK;

   procedure Set_Source_Type
      (N : Node_Id; Val : Entity_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Validate_Unchecked_Conversion);
      Set_Node1 (N, Val); -- semantic field, no parent set
   end Set_Source_Type;

   procedure Set_Specification
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Abstract_Subprogram_Declaration
        or else NT (N).Nkind = N_Formal_Subprogram_Declaration
        or else NT (N).Nkind = N_Generic_Package_Declaration
        or else NT (N).Nkind = N_Generic_Subprogram_Declaration
        or else NT (N).Nkind = N_Package_Declaration
        or else NT (N).Nkind = N_Subprogram_Body
        or else NT (N).Nkind = N_Subprogram_Declaration
        or else NT (N).Nkind = N_Subprogram_Renaming_Declaration);
      Set_Node1_With_Parent (N, Val);
   end Set_Specification;

   procedure Set_Split_PPC
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aspect_Specification
        or else NT (N).Nkind = N_Pragma);
      Set_Flag17 (N, Val);
   end Set_Split_PPC;

   procedure Set_Statements
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Case_Statement_Alternative
        or else NT (N).Nkind = N_Handled_Sequence_Of_Statements
        or else NT (N).Nkind = N_Loop_Statement
        or else NT (N).Nkind = N_Reactive_Abort_Handler
        or else NT (N).Nkind = N_Reactive_Abort_Statement
        or else NT (N).Nkind = N_Reactive_Fork_Alternative
        or else NT (N).Nkind = N_Reactive_Select_Alternative);
      Set_List3_With_Parent (N, Val);
   end Set_Statements;

   procedure Set_States
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Reactive_Type);
      Set_List2_With_Parent (N, Val);
   end Set_States;

   procedure Set_Static_Processing_OK
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Aggregate);
      Set_Flag4 (N, Val);
   end Set_Static_Processing_OK;

   procedure Set_State_Identifier
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Reactive_Pause_Statement
        or else NT (N).Nkind = N_Reactive_Select_Statement
        or else NT (N).Nkind = N_Reactive_Wait_Statement);
      Set_Node5_With_Parent (N, Val);
   end Set_State_Identifier;

   procedure Set_Storage_Pool
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Allocator
        or else NT (N).Nkind = N_Free_Statement
        or else NT (N).Nkind = N_Return_Statement);
      Set_Node1 (N, Val); -- semantic field, no parent set
   end Set_Storage_Pool;

   procedure Set_Strval
      (N : Node_Id; Val : String_Id) is
   begin
      pragma Assert (False
--        or else NT (N).Nkind = N_Operator_Symbol
        or else NT (N).Nkind = N_String_Literal);
      Set_Str3 (N, Val);
   end Set_Strval;

   procedure Set_Subtype_Indication
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Access_To_Object_Definition
        or else NT (N).Nkind = N_Component_Definition
        or else NT (N).Nkind = N_Derived_Type_Definition
        or else NT (N).Nkind = N_Private_Extension_Declaration
        or else NT (N).Nkind = N_Subtype_Declaration);
      Set_Node5_With_Parent (N, Val);
   end Set_Subtype_Indication;

   procedure Set_Subtype_Mark
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Access_Definition
        or else NT (N).Nkind = N_Access_Function_Definition
        or else NT (N).Nkind = N_Formal_Derived_Type_Definition
        or else NT (N).Nkind = N_Formal_Object_Declaration
        or else NT (N).Nkind = N_Function_Specification
        or else NT (N).Nkind = N_Object_Renaming_Declaration
        or else NT (N).Nkind = N_Qualified_Expression
        or else NT (N).Nkind = N_Subtype_Indication
        or else NT (N).Nkind = N_Type_Conversion
        or else NT (N).Nkind = N_Unchecked_Type_Conversion);
      Set_Node4_With_Parent (N, Val);
   end Set_Subtype_Mark;

   procedure Set_Subtype_Marks
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Unconstrained_Array_Definition
        or else NT (N).Nkind = N_Use_Type_Clause);
      Set_List2_With_Parent (N, Val);
   end Set_Subtype_Marks;

   procedure Set_Tagged_Present
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Formal_Private_Type_Definition
        or else NT (N).Nkind = N_Private_Type_Declaration
        or else NT (N).Nkind = N_Record_Definition
        or else NT (N).Nkind = N_With_Type_Clause);
      Set_Flag15 (N, Val);
   end Set_Tagged_Present;

   procedure Set_Target_Type
      (N : Node_Id; Val : Entity_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Validate_Unchecked_Conversion);
      Set_Node2 (N, Val); -- semantic field, no parent set
   end Set_Target_Type;

   procedure Set_Then_Actions
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Conditional_Expression);
      Set_List2 (N, Val); -- semantic field, no parent set
   end Set_Then_Actions;

   procedure Set_Then_Statements
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Elsif_Part
        or else NT (N).Nkind = N_If_Statement);
      Set_List2_With_Parent (N, Val);
   end Set_Then_Statements;

   procedure Set_TSS_Elist
      (N : Node_Id; Val : Elist_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Freeze_Entity);
      Set_Elist3 (N, Val); -- semantic field, no parent set
   end Set_TSS_Elist;

   procedure Set_Type_Definition
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Full_Type_Declaration);
      Set_Node3_With_Parent (N, Val);
   end Set_Type_Definition;

   procedure Set_Unit
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Compilation_Unit);
      Set_Node2_With_Parent (N, Val);
   end Set_Unit;

   procedure Set_Unreferenced_In_Spec
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_With_Clause);
      Set_Flag7 (N, Val);
   end Set_Unreferenced_In_Spec;

   procedure Set_Visible_Declarations
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Package_Specification);
      Set_List2_With_Parent (N, Val);
   end Set_Visible_Declarations;

   procedure Set_Zero_Cost_Handling
      (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (False
        or else NT (N).Nkind = N_Handled_Sequence_Of_Statements);
      Set_Flag5 (N, Val);
   end Set_Zero_Cost_Handling;
   
   function Ada_Real_Literal (N : Node_Id) return String_Id is
   begin
      return Str1 (N);
   end Ada_Real_Literal;
   
   procedure Set_Ada_Real_Literal (N : Node_Id; Val : String_Id) is
   begin
      Set_Str1 (N, Val);
   end Set_Ada_Real_Literal;
   
   -------------------------
   -- Iterator Procedures --
   -------------------------

   procedure Next_Entity       (N : in out Node_Id) is
   begin
      N := Next_Entity (N);
   end Next_Entity;

   procedure Next_Named_Actual (N : in out Node_Id) is
   begin
      N := Next_Named_Actual (N);
   end Next_Named_Actual;

   procedure Next_Rep_Item     (N : in out Node_Id) is
   begin
      N := Next_Rep_Item (N);
   end Next_Rep_Item;

   procedure Next_Use_Clause   (N : in out Node_Id) is
   begin
      N := Next_Use_Clause (N);
   end Next_Use_Clause;

   ------------------
   -- End_Location --
   ------------------

   function End_Location (N : Node_Id) return Source_Ptr is
      L : constant Uint := End_Span (N);

   begin
      if L = No_Uint then
         return No_Location;
      else
         return Source_Ptr (Int (Sloc (N)) + UI_To_Int (L));
      end if;
   end End_Location;

   ----------------------
   -- Set_End_Location --
   ----------------------

   procedure Set_End_Location (N : Node_Id; S : Source_Ptr) is
   begin
      Set_End_Span (N,
        UI_From_Int (Int (S) - Int (Sloc (N))));
   end Set_End_Location;

   --------------
   -- Nkind_In --
   --------------

   function Nkind_In
     (T  : Node_Kind;
      V1 : Node_Kind;
      V2 : Node_Kind) return Boolean
   is
   begin
      return T = V1 or else
             T = V2;
   end Nkind_In;

   function Nkind_In
     (T  : Node_Kind;
      V1 : Node_Kind;
      V2 : Node_Kind;
      V3 : Node_Kind) return Boolean
   is
   begin
      return T = V1 or else
             T = V2 or else
             T = V3;
   end Nkind_In;

   function Nkind_In
     (T  : Node_Kind;
      V1 : Node_Kind;
      V2 : Node_Kind;
      V3 : Node_Kind;
      V4 : Node_Kind) return Boolean
   is
   begin
      return T = V1 or else
             T = V2 or else
             T = V3 or else
             T = V4;
   end Nkind_In;

   function Nkind_In
     (T  : Node_Kind;
      V1 : Node_Kind;
      V2 : Node_Kind;
      V3 : Node_Kind;
      V4 : Node_Kind;
      V5 : Node_Kind) return Boolean
   is
   begin
      return T = V1 or else
             T = V2 or else
             T = V3 or else
             T = V4 or else
             T = V5;
   end Nkind_In;

   function Nkind_In
     (T  : Node_Kind;
      V1 : Node_Kind;
      V2 : Node_Kind;
      V3 : Node_Kind;
      V4 : Node_Kind;
      V5 : Node_Kind;
      V6 : Node_Kind) return Boolean
   is
   begin
      return T = V1 or else
             T = V2 or else
             T = V3 or else
             T = V4 or else
             T = V5 or else
             T = V6;
   end Nkind_In;

   function Nkind_In
     (T  : Node_Kind;
      V1 : Node_Kind;
      V2 : Node_Kind;
      V3 : Node_Kind;
      V4 : Node_Kind;
      V5 : Node_Kind;
      V6 : Node_Kind;
      V7 : Node_Kind) return Boolean
   is
   begin
      return T = V1 or else
             T = V2 or else
             T = V3 or else
             T = V4 or else
             T = V5 or else
             T = V6 or else
             T = V7;
   end Nkind_In;

   function Nkind_In
     (T  : Node_Kind;
      V1 : Node_Kind;
      V2 : Node_Kind;
      V3 : Node_Kind;
      V4 : Node_Kind;
      V5 : Node_Kind;
      V6 : Node_Kind;
      V7 : Node_Kind;
      V8 : Node_Kind) return Boolean
   is
   begin
      return T = V1 or else
             T = V2 or else
             T = V3 or else
             T = V4 or else
             T = V5 or else
             T = V6 or else
             T = V7 or else
             T = V8;
   end Nkind_In;

   function Nkind_In
     (T  : Node_Kind;
      V1 : Node_Kind;
      V2 : Node_Kind;
      V3 : Node_Kind;
      V4 : Node_Kind;
      V5 : Node_Kind;
      V6 : Node_Kind;
      V7 : Node_Kind;
      V8 : Node_Kind;
      V9 : Node_Kind) return Boolean
   is
   begin
      return T = V1 or else
             T = V2 or else
             T = V3 or else
             T = V4 or else
             T = V5 or else
             T = V6 or else
             T = V7 or else
             T = V8 or else
             T = V9;
   end Nkind_In;

end Sinfo;
