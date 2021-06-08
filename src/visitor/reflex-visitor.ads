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

with Types; use Types;

package Reflex.Visitor is
   
   type Visitor_Record is tagged private;
   type Visitor_Ptr is access all Visitor_Record;
   type Visitor_Class_Ptr is access all Visitor_Record'Class;
   
   function New_Reflex_Visitor return Visitor_Ptr;
   
   function New_Reflex_Visitor (N : Node_Id) return Visitor_Ptr;
   
   procedure Delete_Reflex_Visitor (This : in out Visitor_Ptr);
   
   procedure Visit_Abstract_Subprogram_Declaration
     (This : access Visitor_Record;
      N    : Node_id);

   procedure Visit_Aggregate
     (This : access Visitor_Record;
      N    : Node_id);
   
   procedure Visit_Allocator
     (This : access Visitor_Record;
      N    : Node_id);
   
   procedure Visit_Short_Circuit
     (This : access Visitor_Record;
      N    : Node_id);
   
   procedure Visit_Aspect_Specification 
     (This : access Visitor_Record;
      N    : Node_id);
   
   procedure Visit_Assignment 
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_At_Clause 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Attribute 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Attribute_Definition_Clause 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Block_Statement 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Case_Statement 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Character_Literal 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Code_Statement 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Compilation_Unit 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Component_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Conditional_Expression 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Enumeration_Representation_Clause 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Exit_Statement 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Expanded_Name 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Explicit_Dereference 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Formal_Object_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Formal_Package 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Formal_Subprogram 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Formal_Type_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Free_Statement 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Type_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Function_Call 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Function_Instantiation 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Generic_Function_Renaming 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Generic_Package_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Generic_Package_Renaming 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Generic_procedure_Renaming 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Generic_Subprogram_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Goto_Statement 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Handled_Statements 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Identifier 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_If_Statement 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Implicit_Label_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Membership_Op 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Incomplete_Type_Decl 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Indexed_Component_Form 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Integer_Literal 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Label 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Loop_Statement 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Null 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Null_Statement 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Number_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Object_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Object_Renaming 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Unary_Op 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Arithmetic_Op 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Logical_Op 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Concatenation 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Equality_Op 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Comparison_Op 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Negation 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Others_Choice 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Package_Body 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Package_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Package_Instantiation 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Package_Renaming 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Package_Specification 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Parameter_Association 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Pragma 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Private_Extension_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Private_Type_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Procedure_Call 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Procedure_Instantiation 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Qualified_Expression 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Range 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Reactive_Type 
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Reactive_State
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Reactive_Wait_Statement 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Reactive_Pause_Statement 
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Reactive_Fork_Statement 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Reactive_Fork_Alternative
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Reactive_Select_Statement 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Reactive_Select_Alternative
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Reactive_Abort_Statement
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Reactive_Abort_Handler
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Real_Literal 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Record_Representation_Clause 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Reference 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Return_Statement 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Selected_Component
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Slice 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_String_Literal 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Subprogram_Body 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Subprogram_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Subprogram_Info 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Subprogram_Renaming 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Subtype_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Subtype_Indication 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Type_Conversion 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Unchecked_Expression 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Unchecked_Type_Conversion 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Use_Package 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Validate_Unchecked_Conversion
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_With_Clause 
     (This : access Visitor_Record;
      N    : Node_Id);

   procedure Visit_Access_Definition
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Access_Function_Definition
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Access_Procedure_Definition            
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Access_To_Object_Definition
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Case_Statement_Alternative
     (This : access  Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Compilation_Unit_Aux
     (This : access  Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Component_Association
     (This : access  Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Component_Clause
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Component_Definition
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Component_List
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Constrained_Array_Definition
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Defining_Character_Literal
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Defining_Identifier
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Defining_Program_Unit_Name
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Derived_Type_Definition
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Designator
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Digits_Constraint
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Discriminant_Association
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Elsif_Part
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Enumeration_Type_Definition
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Floating_Point_Definition
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Formal_Derived_Type_Definition
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Formal_Discrete_Type_Definition
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Formal_Floating_Point_Definition
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Formal_Modular_Type_Definition
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Formal_Private_Type_Definition
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Formal_Signed_Integer_Type_Definition
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Function_Specification
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Generic_Association
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Index_Or_Discriminant_Constraint
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Iteration_Scheme
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Loop_Parameter_Specification
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Mod_Clause
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Modular_Type_Definition
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Parameter_Specification
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Pragma_Argument_Association
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Procedure_Specification
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Real_Range_Specification
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Record_Definition
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Signed_Integer_Type_Definition
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Unconstrained_Array_Definition
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Unused_At_Start
     (This : access Visitor_Record;
      N    : Node_Id);
   
   procedure Visit_Unused_At_End
     (This : access Visitor_Record;
      N    : Node_Id);
   
private
   
   type Visitor_Record is tagged record
      Root_Node : Node_Id;
   end record;
   
   No_Visitor_Record : constant Visitor_Record :=
     (Root_Node => Empty);
   
end Reflex.Visitor;
