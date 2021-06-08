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

with Sinfo; use Sinfo;

package body Reflex.Simple_Accept is
   
   ---------------------
   -- Accept_Unary_Op --
   ---------------------
   
   procedure Accept_Unary_Op 
     (N : Node_Id;
      V : access Visitor_Record'Class) is
   begin
      Accept_Node (Right_Opnd (N), V);
      
   end Accept_Unary_Op;
   
   ----------------------
   -- Accept_Binary_Op --
   ----------------------
   
   procedure Accept_Binary_Op 
     (N : Node_Id;
      V : access Visitor_Record'Class) is
   begin
      Accept_Node (Left_Opnd (N), V);
      Accept_Node (Right_Opnd (N), V);
   end Accept_Binary_Op;
   
   -----------------
   -- Accept_Node --
   -----------------

   procedure Accept_Node
     (N : Node_Id;
      V : access Visitor_Record'Class) is
   begin
      if Empty (N) then
	 return;
      end if;
      
      case Nkind (N) is

         when N_Abstract_Subprogram_Declaration =>
            V.Visit_Abstract_Subprogram_Declaration (N);
	    Accept_Node (Specification, V);
	    
         when N_Aggregate =>
            V.Visit_Aggregate (N);
	    Accept_List (Expressions (N), V);
	    Accept_List (Component_Associations (N), V);
	    
         when N_Allocator =>
            null; -- V.Visit_Allocator (N);

         when N_And_Then =>
            V.Visit_Short_Circuit (N);
	    Accept_Node (Left_Opnd (N), V);
	    Accept_Node (Right_Opnd (N), V);
	    
         when N_Aspect_Specification =>
            V.Visit_Aspect_Specification (N);
	    Accept_Node (Identifier (N), V);
	    Accept_Noode (Expression (N), V);

         when N_Assignment_Statement =>
            V.Visit_Assignment (N);
	    Accept_Node (Name (N), V);
	    Accept_Noode (Expression (N), V);
	    
         when N_At_Clause =>
            V.Visit_At_Clause (N);

         when N_Attribute_Reference =>
            V.Visit_Attribute (N);
	    Accept_Node (Prefix (N), V);
	    Accept_Node (Attribute_Name (N), V);
	    Accept_List (Expressions (N), V);
	    
         when N_Attribute_Definition_Clause   =>
            V.Visit_Attribute_Definition_Clause (N);
	    Accept_Node (Name (N), V);
	    Accept_Noode (Expression (N), V);

         when N_Block_Statement =>
            V.Visit_Block_Statement (N);
	    Accept_Node (Identifier (N), V);
	    Accept_List (Declarations (N), V);
	    Accept_Node (Handled_Statement_Sequence (N), V);
	    
         when N_Case_Statement =>
            V.Visit_Case_Statement (N);
	    Accept_Noode (Expression (N), V);
	    Accept_List (Alternatives (N), V);
	    
         when N_Character_Literal =>
            V.Visit_Character_Literal (N);

         when N_Code_Statement =>
            V.Visit_Code_Statement (N);

         when N_Compilation_Unit =>
            V.Visit_Compilation_Unit (N);
	    Accept_Node (Library_Unit (N), V);
	    Accept_List (Context_Items (N), V);
	    Accept_Node (Unit (N), V);
	    Accept_Node (Aux_Decls_Node (N), V);
	    
         when N_Component_Declaration =>
            V.Visit_Component_Declaration (N);
	    Accept_Node (Defining_Identifier (N), V);
	    Accept_Node (Component_Definition (N), V);
	    Accept_Node (Expression (N), V);

         when N_Conditional_Expression =>
            V.Visit_Conditional_Expression (N);
	    Accept_List (Expressions (N), V);
	    Accept_List (Then_Actions (N), V);
	    Accept_List (Else_Actions (N), V);
	    
         when N_Enumeration_Representation_Clause =>
            V.Visit_Enumeration_Representation_Clause (N);

         when N_Exit_Statement =>
            V.Visit_Exit_Statement (N);
	    Accept_Node (Name (N), V);
	    Accept_Node (Condition (N), V);

         when N_Expanded_Name =>
            V.Visit_Expanded_Name (N);
	    Accept_Node (Prefix (N), V);
	    Accept_Node (Selector (N), V);

         when N_Explicit_Dereference =>
            V.Visit_Explicit_Dereference (N);
	    Accept_Node (Prefix (N), V);

         when N_Extension_Aggregate =>
            V.Visit_Aggregate (N);
	    Accept_Node (Ancestor_Part (N), V);
	    Accept_List (Expressions (N), V);
	    Accept_List (Component_Associations (N), V);

         when N_Formal_Object_Declaration =>
            V.Visit_Formal_Object_Declaration (N);
	    Accept_Node (Defining_Identifier (N), V);
	    Accept_Node (Subtype_Mark (N), V);
	    Accept_Node (Expression (N), V);

         when N_Formal_Package_Declaration =>
            V.Visit_Formal_Package (N);
	    Accept_Node (Defining_Identifier (N), V);
	    Accept_Node (Generic_Associations (N), V);
	    Accept_Node (Instance_Spec (N), V);

         when N_Formal_Subprogram_Declaration =>
            V.Visit_Formal_Subprogram (N);
	    Accept_Node (Specification (N), V);

         when N_Formal_Type_Declaration =>
            V.Visit_Formal_Type_Declaration (N);
	    Accept_Node (Defining_Identifier (N), V);
	    Accept_Node (Formal_Type_Definition (N), V);

         when N_Free_Statement =>
            V.Visit_Free_Statement (N);

         when N_Freeze_Entity =>
            null;  -- no semantic processing required

         when N_Full_Type_Declaration =>
            V.Visit_Type_Declaration (N);
	    Accept_Node (Defining_Identifier (N), V);
	    Accept_Node (Type_Definition (N), V);

         when N_Function_Call =>
            V.Visit_Function_Call (N);
	    Accept_Node (Name (N), V);
	    Accept_List (Parameter_Associations (N), V);

         when N_Function_Instantiation =>
            V.Visit_Function_Instantiation (N);
	    Accept_Node (Defining_Unit_Name (N), V);
	    Accept_List (Generic_Associations (N), V);

         when N_Generic_Function_Renaming_Declaration =>
            V.Visit_Generic_Function_Renaming (N);
	    Accept_Node (Defining_Unit_Name (N), V);

         when N_Generic_Package_Declaration =>
            V.Visit_Generic_Package_Declaration (N);

         when N_Generic_Package_Renaming_Declaration =>
            V.Visit_Generic_Package_Renaming (N);

         when N_Generic_Procedure_Renaming_Declaration =>
            V.Visit_Generic_Procedure_Renaming (N);

         when N_Generic_Subprogram_Declaration =>
            V.Visit_Generic_Subprogram_Declaration (N);

         when N_Goto_Statement =>
            V.Visit_Goto_Statement (N);
	    Accept_Node (Name (N), V);
	    
         when N_Handled_Sequence_Of_Statements =>
            V.Visit_Handled_Statements (N);
	    Accept_List (Statements (N), V);
	    
         when N_Identifier =>
            V.Visit_Identifier (N);

         when N_If_Statement =>
            V.Visit_If_Statement (N);
	    Accept_Node (Condition (N), V);
	    Accept_List (Then_Statements (N), V);
	    Accept_List (Elsif_Parts (N), V);
	    Accept_List (Else_Statements (N), V);

         when N_Implicit_Label_Declaration =>
            V.Visit_Implicit_Label_Declaration (N);

         when N_In =>
            V.Visit_Membership_Op (N);
	    Accept_Node (Left_Opnd (N), V);
	    Accept_Node (Right_Opnd (N), V);
	    
         when N_Incomplete_Type_Declaration =>
            V.Visit_Incomplete_Type_Decl (N);
	    Accept_Node (Defining_Identifier (N), V);

         when N_Indexed_Component =>
            V.Visit_Indexed_Component_Form (N);
	    Accept_Node (Prefix (N), V);
	    Accept_List (Expressions (N), V);
	    
         when N_Integer_Literal =>
            V.Visit_Integer_Literal (N);

         when N_Label =>
            V.Visit_Label (N);

         when N_Loop_Statement =>
            V.Visit_Loop_Statement (N);
	    Accept_Node (Iteration_Scheme (N), V);
	    Accept_List (Statements (N), V);

         when N_Not_In =>
            V.Visit_Membership_Op (N);
	    Accept_Node (Left_Opnd (N), V);
	    Accept_Node (Right_Opnd (N), V);

         when N_Null =>
            V.Visit_Null (N);

         when N_Null_Statement =>
            V.Visit_Null_Statement (N);

         when N_Number_Declaration =>
            V.Visit_Number_Declaration (N);
	    Accept_Node (Defining_Identifier (N), V);
	    Accept_Node (Expression (N), V);
	    
         when N_Object_Declaration =>
            V.Visit_Object_Declaration (N);
	    Accept_Node (Defining_Identifier (N), V);
	    Accept_Node (Object_Definition (N), V);
	    Accept_Node (Expression (N), V);

         when N_Object_Renaming_Declaration  =>
            V.Visit_Object_Renaming (N);
	    Accept_Node (Defining_Identifier (N), V);
	    Accept_Node (Subtype_Mark (N), V);
	    Accept_Node (Name (N), V);

         when N_Op_Abs =>
            V.Visit_Unary_Op (N);
	    Accept_Unary_Op (N, V);
	    
         when N_Op_Add =>
            V.Visit_Arithmetic_Op (N);
	    Accept_Binary_Op (N, V);

         when N_Op_And =>
            V.Visit_Logical_Op (N);
	    Accept_Binary_Op (N, V);

         when N_Op_Concat =>
            V.Visit_Concatenation (N);
	    Accept_Binary_Op (N, V);

         when N_Op_Divide =>
            V.Visit_Arithmetic_Op (N);
	    Accept_Binary_Op (N, V);

         when N_Op_Eq =>
            V.Visit_Equality_Op (N);
	    Accept_Binary_Op (N, V);

         when N_Op_Expon =>
            V.Visit_Arithmetic_Op (N);
	    Accept_Binary_Op (N, V);
			   
         when N_Op_Ge =>
            V.Visit_Comparison_Op (N);
	    Accept_Binary_Op (N, V);

         when N_Op_Gt =>
            V.Visit_Comparison_Op (N);
	    Accept_Binary_Op (N, V);

         when N_Op_Le =>
            V.Visit_Comparison_Op (N);
	    Accept_Binary_Op (N, V);

         when N_Op_Lt =>
            V.Visit_Comparison_Op (N);
	    Accept_Binary_Op (N, V);

         when N_Op_Minus =>
            V.Visit_Unary_Op (N);
	    Accept_Unary_Op (N, V);

         when N_Op_Mod =>
            V.Visit_Arithmetic_Op (N);
	    Accept_Binary_Op (N, V);

         when N_Op_Multiply =>
            V.Visit_Arithmetic_Op (N);
	    Accept_Binary_Op (N, V);

         when N_Op_Ne =>
            V.Visit_Equality_Op (N);
	    Accept_Binary_Op (N, V);

         when N_Op_Not =>
            V.Visit_Negation (N);
	    Accept_Unary_Op (N, V);

         when N_Op_Or =>
            V.Visit_Logical_Op (N);
	    Accept_Binary_Op (N, V);

         when N_Op_Plus =>
            V.Visit_Unary_Op (N);
	    Accept_Binary_Op (N, V);

         when N_Op_Rem =>
            V.Visit_Arithmetic_Op (N);
	    Accept_Binary_Op (N, V);

         when N_Op_Rotate_Left =>
            V.Visit_Arithmetic_Op (N);
	    Accept_Binary_Op (N, V);

         when N_Op_Rotate_Right =>
            V.Visit_Arithmetic_Op (N);
	    Accept_Binary_Op (N, V);

         when N_Op_Shift_Left =>
            V.Visit_Arithmetic_Op (N);
	    Accept_Binary_Op (N, V);

         when N_Op_Shift_Right =>
            V.Visit_Arithmetic_Op (N);
	    Accept_Binary_Op (N, V);

         when N_Op_Shift_Right_Arithmetic =>
            V.Visit_Arithmetic_Op (N);
	    Accept_Binary_Op (N, V);

         when N_Op_Subtract =>
            V.Visit_Arithmetic_Op (N);
	    Accept_Binary_Op (N, V);

         when N_Op_Xor =>
            V.Visit_Logical_Op (N);
	    Accept_Binary_Op (N, V);

         when N_Or_Else =>
            V.Visit_Short_Circuit (N);
	    Accept_Binary_Op (N, V);

         when N_Others_Choice =>
            V.Visit_Others_Choice (N);
	    Accept_List (Others_Discrete_Choices (N), V);
	    
         when N_Package_Body =>
            V.Visit_Package_Body (N);
	    Accept_Node (Defining_Unit_Name (N), V);
	    Accept_List (Declarations (N), V);
	    Accept_Node (Handled_Statement_Sequence (N), V);
	    
         when N_Package_Declaration =>
            V.Visit_Package_Declaration (N);
	    Accept_Node (Defining_Unit_Name (N), V);
	    Accept_List (Visible_Declarations (N), V);
	    Accept_List (Private_Declarations (N), V);

         when N_Package_Instantiation =>
            V.Visit_Package_Instantiation (N);
	    Accept_Node (Defining_Unit_Name (N), V);
	    Accept_List (Generic_Associations (N), V);

         when N_Package_Renaming_Declaration =>
            V.Visit_Package_Renaming (N);
	    Accept_Node (Defining_Unit_Name (N), V);

         when N_Package_Specification =>
            V.Visit_Package_Specification (N);
	    Accept_Node (Defining_Unit_Name (N), V);
	    Accept_List (Visible_Declarations (N), V);
	    Accept_List (Private_Declarations (N), V);

         when N_Parameter_Association =>
            V.Visit_Parameter_Association (N);
	    Accept_Node (Selector_Name (N), V);
	    Accept_Node (Explicit_Actual_Parameter (N), V);
	    
         when N_Pragma =>
            V.Visit_Pragma (N);

         when N_Private_Extension_Declaration =>
            V.Visit_Private_Extension_Declaration (N);
	    Accept_Node (Defining_Identifier (N), V);
	    Accept_Node (Subtype_Indication (N), V);

         when N_Private_Type_Declaration =>
            V.Visit_Private_Type_Declaration (N);
	    Accept_Node (Defining_Identifier (N), V);

         when N_Procedure_Call_Statement =>
            V.Visit_Procedure_Call (N);
	    Accept_List (Parameter_Associations (N), V);

         when N_Procedure_Instantiation =>
            V.Visit_Procedure_Instantiation (N);
	    Accept_Node (Defining_Unit_Name (N), V);
	    Accept_List (Generic_Associations (N), V);

         when N_Qualified_Expression =>
            V.Visit_Qualified_Expression (N);
	    Accept_Node (Subtype_Mark (N), V);
	    Accept_Node (Expression (N), V);

         when N_Range =>
            V.Visit_Range (N);
	    Accept_Node (Low_Bound (N), V);
	    Accept_Node (High_Bound (N), V);

         when N_Range_Constraint =>
            V.Visit_Range (N);
	    Accept_Node (Range_Expression (N), V);
	    
	 when N_Reactive_Type =>
	    null;
	 when N_Reactive_State =>
	    null;
	 when N_Reactive_Wait_Statement =>
	    V.Visit_Reactive_Wait_Statement (N);

	 when N_Reactive_Pause_Statement =>
	    V.Visit_Reactive_Pause_Statement (N);
	    
	 when N_Reactive_Fork_Statement =>
	    V.Visit_Reactive_Fork_Statement (N);

	 when N_Reactive_Fork_Alternative =>
	    raise Program_Error;
	    
	 when N_Reactive_Select_Statement =>
	    V.Visit_Reactive_Select_Statement (N);

	 when N_Reactive_Select_Alternative =>
	    raise Program_Error;

	 when N_Reactive_Abort_Statement =>
	    null;
	 when N_Reactive_Abort_Handler =>
	    null;
	    
         when N_Real_Literal =>
            V.Visit_Real_Literal (N);

         when N_Record_Representation_Clause =>
            V.Visit_Record_Representation_Clause (N);

         when N_Reference =>
            V.Visit_Reference (N);
	    Accept_Node (Prefix (N), V);

         when N_Return_Statement =>
            V.Visit_Return_Statement (N);
	    Accept_Node (Expression (N), V);

         when N_Selected_Component =>
            V.Visit_Selected_Component (N);
	    Accept_Node (Prefix (N), V);
	    Accept_Node (Selector_Name (N), V);

         when N_Slice =>
            V.Visit_Slice (N);
	    Accept_Node (Prefix (N), V);
	    Accept_Node (Discrete_Range (N), V);

         when N_String_Literal =>
            V.Visit_String_Literal (N);

         when N_Subprogram_Body =>
            V.Visit_Subprogram_Body (N);
	    Accept_Node (Specification (N), V);
	    Accept_List (Declarations (N), V);
	    Accept_Node (Handled_Statement_Sequence (N), V);

         when N_Subprogram_Declaration =>
            V.Visit_Subprogram_Declaration (N);
	    Accept_Node (Specification (N), V);

         when N_Subprogram_Info =>
            V.Visit_Subprogram_Info (N);

         when N_Subprogram_Renaming_Declaration =>
            V.Visit_Subprogram_Renaming (N);
	    Accept_Node (Specification (N), V);

         when N_Subtype_Declaration =>
            V.Visit_Subtype_Declaration (N);
	    Accept_Node (Defining_Identifier (N), V);
	    Accept_Node (Subtype_Indication (N), V);

         when N_Subtype_Indication =>
            V.Visit_Subtype_Indication (N);
	    Accept_Node (Subtype_Mark (N), V);
	    Accept_Node (Constraint (N), V);

         when N_Type_Conversion =>
            V.Visit_Type_Conversion (N);
	    Accept_Node (Subtype_Mark (N), V);
	    Accept_Node (Expression (N), V);

         when N_Unchecked_Expression =>
            V.Visit_Unchecked_Expression (N);
	    Accept_Node (Expression (N), V);

         when N_Unchecked_Type_Conversion =>
            V.Visit_Unchecked_Type_Conversion (N);
	    Accept_Node (Subtype_Mark (N), V);
	    Accept_Node (Expression (N), V);

         when N_Use_Package_Clause =>
            V.Visit_Use_Package (N);
	    Accept_List (Names (N), V);

         when N_Use_Type_Clause =>
            null; -- V.Visit_Use_Type (N);

         when N_Validate_Unchecked_Conversion =>
            null;

         when N_With_Clause =>
            V.Visit_With_Clause (N);
	    Accept_Node (Name (N), V);

	 when N_Access_Definition                      =>
	    V.Visit_Access_Definition (N);
	    Accept_Node (Subtype_Mark (N), V);
	    
	 when N_Access_Function_Definition             =>
	    V.Visit_Access_Function_Definition (N);
	    Accept_List (Parameter_Specifications (N), V);
	    Accept_Node (Subtype_Mark (N), V);
	    
	 when N_Access_Procedure_Definition            =>
	    V.Visit_Access_Procedure_Definition (N);
	    Accept_List (Parameter_Specifications (N), V);
	    
	 when N_Access_To_Object_Definition            =>
	    V.Visit_Access_To_Object_Definition (N);
	    Accept_Node (Subtype_Indication (N), V);
	    
	 when N_Case_Statement_Alternative             =>
	    V.Visit_Case_Statement_Alternative (N);
	    Accept_List (Discrete_Choices (N), V);
	    Accept_List (Statements (N), V);
	    
	 when N_Compilation_Unit_Aux                   =>
	    V.Visit_Compilation_Unit_Aux (N);
	    
	 when N_Component_Association                  =>
	    V.Visit_Component_Association (N);
	    
	 when N_Component_Clause                       =>
	    V.Visit_Component_Clause (N);
	    
	 when N_Component_Definition                   =>
	    V.Visit_Component_Definition (N);
	    
	 when N_Component_List                         =>
	    V.Visit_Component_List (N);
	    
	 when N_Constrained_Array_Definition           =>
	    V.Visit_Constrained_Array_Definition (N);
	    
	 when N_Defining_Character_Literal             =>
	    V.Visit_Defining_Character_Literal (N);
	    
	 when N_Defining_Identifier                    =>
	    V.Visit_Defining_Identifier (N);

	 when N_Defining_Program_Unit_Name             =>
	    V.Visit_Defining_Program_Unit_Name (N);
	    
	 when N_Derived_Type_Definition                =>
	    V.Visit_Derived_Type_Definition (N);
	    
	 when N_Designator                             =>
	    V.Visit_Designator (N);
	    
	 when N_Digits_Constraint                      =>
	    V.Visit_Digits_Constraint (N);
	    
	 when N_Discriminant_Association               =>
	    V.Visit_Discriminant_Association (N);
	    
	 when N_Elsif_Part                             =>
	    V.Visit_Elsif_Part (N);
	    
	 when N_Enumeration_Type_Definition            =>
	    V.Visit_Enumeration_Type_Definition (N);
	    
	 when N_Floating_Point_Definition              =>
	    V.Visit_Floating_Point_Definition (N);
	    
	 when N_Formal_Derived_Type_Definition         =>
	    V.Visit_Formal_Derived_Type_Definition (N);
	    
	 when N_Formal_Discrete_Type_Definition        =>
	    V.Visit_Formal_Discrete_Type_Definition (N);
	    
	 when N_Formal_Floating_Point_Definition       =>
	    V.Visit_Formal_Floating_Point_Definition (N);
	    
	 when N_Formal_Modular_Type_Definition         =>
	    V.Visit_Formal_Modular_Type_Definition (N);
	    
	 when N_Formal_Private_Type_Definition         =>
	    V.Visit_Formal_Private_Type_Definition (N);
	    
	 when N_Formal_Signed_Integer_Type_Definition  =>
	    V.Visit_Formal_Signed_Integer_Type_Definition (N);
	    
	 when N_Function_Specification                 =>
	    V.Visit_Function_Specification (N);
	    
	 when N_Generic_Association                    =>
	    V.Visit_Generic_Association (N);
	    
	 when N_Index_Or_Discriminant_Constraint       =>
	    V.Visit_Index_Or_Discriminant_Constraint (N);
	    
	 when N_Iteration_Scheme                       =>
	    V.Visit_Iteration_Scheme (N);
	    
	 when N_Loop_Parameter_Specification           =>
	    V.Visit_Loop_Parameter_Specification (N);
	    
	 when N_Mod_Clause                             =>
	    V.Visit_Mod_Clause (N);
	    
	 when N_Modular_Type_Definition                =>
	    V.Visit_Modular_Type_Definition (N);
	    
	 when N_Parameter_Specification                =>
	    V.Visit_Parameter_Specification (N);
	    
	 when N_Pragma_Argument_Association            =>
	    V.Visit_Pragma_Argument_Association (N);
	    
	 when N_Procedure_Specification                =>
	    V.Visit_Procedure_Specification (N);
	    
	 when N_Real_Range_Specification               =>
	    V.Visit_Real_Range_Specification (N);
	    
	 when N_Record_Definition                      =>
	    V.Visit_Record_Definition (N);
	    
	 when N_Signed_Integer_Type_Definition         =>
	    V.Visit_Signed_Integer_Type_Definition (N);
	    
	 when N_Unconstrained_Array_Definition         =>
	    V.Visit_Unconstrained_Array_Definition (N);
	    
	 when N_Unused_At_Start                        =>
	      V.Visit_Unused_At_Start (N);
	      
	 when N_Unused_At_End                          =>
	    V.Visit_Unused_At_End (N);
      end case;
      
   end Accept_Node;

   -----------------
   -- Accept_List --
   -----------------

   procedure Accept_List
     (L : List_Id;
      V : access Visitor_Record'Class) is
      
      Node : Node_Id;
   begin
      Node := First (L);
      while Present (Node) loop
         Accept_Node (Node, V);
         Next (Node);
      end loop;
   end Accept_List;

end Reflex.Simple_Accept;
