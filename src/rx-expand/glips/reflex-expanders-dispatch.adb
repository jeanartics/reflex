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

with Ada.Text_Io; use Ada.Text_Io;

with Atree; use Atree;
with Sinfo; use Sinfo;
with Output;

with Reflex.Expanders.Types; use Reflex.Expanders.Types;
with Reflex.Expanders.Ch2; use Reflex.Expanders.Ch2;
with Reflex.Expanders.Ch3; use Reflex.Expanders.Ch3;
with Reflex.Expanders.Ch4; use Reflex.Expanders.Ch4;
with Reflex.Expanders.Ch5; use Reflex.Expanders.Ch5;
with Reflex.Expanders.Ch6; use Reflex.Expanders.Ch6;
with Reflex.Expanders.Ch7; use Reflex.Expanders.Ch7;
with Reflex.Expanders.Ch10; use Reflex.Expanders.Ch10;
with Reflex.Expanders.Itypes; use Reflex.Expanders.Itypes;

package body Reflex.Expanders.Dispatch is

   -------------------
   -- Expand_Node --
   -------------------

   procedure Expand_Node
     (This        : access Reflex_Expander_Record;
      Node        : Node_Id; 
      Declaration : Boolean := False) is
      
   begin
      if Node = Empty then --  or else not Comes_From_Source (Node) then
         return;
      end if;
      
      Put_Line ("Expand_Node Begin " & Nkind (Node)'Img);
      --  Select print circuit based on node kind
      
      case Nkind (Node) is
         when N_Abstract_Subprogram_Declaration =>
	    Expand_Abstract_Subprogram_Declaration (This, Node);

         when N_Access_Definition =>
	    null; --  Expand_Access_Definition (This, Node);
	    
         when N_Access_To_Object_Definition |
              N_Access_Function_Definition  |
              N_Access_Procedure_Definition =>

            --  Processed by Expand_Declare as part of processing the parent
            --  node (N_Full_Type_Declaration) or the itypes associated with
            --  anonymous access-to-subprogram types.

            raise Program_Error;

         when N_Aggregate =>
	    if not Declaration then
	       Reflex.Expanders.Ch4.Expand_Aggregate (This, Node);
	    end if;
	    
         when N_Allocator =>
	    
	    -- No yet Implemented
	    
	    raise Program_Error;

         when N_And_Then =>
	    Reflex.Expanders.Ch4.Expand_And_Then (This, Node);
	    
         --  Note: the following code for N_Aspect_Specification is not used,
         --  since we deal with aspects as part of a declaration.

         when N_Assignment_Statement =>
	    Reflex.Expanders.Ch5.Expand_Assignment_Statement (This, Node);
	    
         when N_At_Clause =>
            raise Program_Error;

         when N_Attribute_Definition_Clause =>
            null; ----------------  raise Program_Error;

         when N_Attribute_Reference =>
            Expand_Attribute_Reference (This, Node);

         when N_Block_Statement =>
	    Expand_Block_Statement (This, Node);
	    
         when N_Case_Statement =>
	    Reflex.Expanders.Ch5.Expand_Case_Statement (This, Node);

         when N_Case_Statement_Alternative =>
	    Expand_Case_Statement_Alternative (This, Node);

         when N_Character_Literal =>
	    null;
	    
         when N_Code_Statement =>
	    Expand_Code_Statement (This, Node);
	    
         when N_Compilation_Unit =>
	    Expand_Compilation_Unit (This, Node);

         when N_Compilation_Unit_Aux =>
            Expand_Compilation_Unit_Aux (This, Node);
	    -- nothing to do, never used, see above

         when N_Component_Association =>
            Expand_Component_Association (This, Node);

         when N_Component_Clause =>
            raise Program_Error;

         when N_Component_Definition =>
	   null; --  Expand_Component_Definition (This, Node);

         when N_Component_Declaration =>
            null; -- Glips.Decls.Expand_Component_Declaration (This, Node);

         when N_Component_List =>
	    Expand_Component_List (This, Node);

         when N_Defining_Character_Literal =>
	    Expand_Defining_Character_Literal (This, Node);

         when N_Defining_Identifier =>
	    Expand_Defining_Identifier (This, Node);
	    
         when N_Defining_Operator_Symbol =>
            Expand_Defining_Operator_Symbol (This, Node);

         when N_Defining_Program_Unit_Name =>
            Expand_Defining_Program_Unit_Name (This, Node);

         when N_Derived_Type_Definition =>
	    Expand_Derived_Type_Definition (This, Node);

         when N_Designator | N_Digits_Constraint =>
            raise Program_Error;

         when N_Discriminant_Association =>
	    Expand_Discriminant_Association (This, Node);

         when N_Elsif_Part =>
	    null; --  Reflex.Expanders.Ch5.Expand_Elsif_Part (This, Node);

         when N_Empty =>
            null;

         when N_Enumeration_Representation_Clause |
              N_Enumeration_Type_Definition
         =>
            Reflex.Expanders.Ch3.Expand_Enumeration_Type_Definition (This, Node);

         when N_Error =>
            null;

         when N_Exception_Handler =>
            null; -- not output in C code

         when N_Exception_Declaration          |
              N_Exception_Renaming_Declaration
         =>
	    raise Program_Error;
	    
         when N_Exit_Statement =>
	    Reflex.Expanders.Ch5.Expand_Exit_Statement (This, Node);
	    
         when N_Expanded_Name =>
	    Reflex.Expanders.Ch2.Expand_Expanded_Name (This, Node);

         when N_Explicit_Dereference =>
	    Expand_Explicit_Dereference (This, Node);

--           when N_Expression_With_Actions =>
--              Expand_Expression_With_Actions (This, Node);
	    
         when N_Return_Statement =>
            Expand_Return_Statement (This, Node);

         when N_Extension_Aggregate =>
	    if not Declaration then
	       Expand_Extension_Aggregate (This, Node);
	    end if;

         when N_Floating_Point_Definition              |
              N_Formal_Derived_Type_Definition         |
              N_Formal_Discrete_Type_Definition        |
              N_Formal_Floating_Point_Definition       |
              N_Formal_Modular_Type_Definition         |
              N_Formal_Object_Declaration              |
              N_Formal_Package_Declaration             |
              N_Formal_Private_Type_Definition         |
              N_Formal_Signed_Integer_Type_Definition  |
              N_Formal_Type_Declaration
         =>
            null; -- not output in C code

         when N_Free_Statement =>
            Expand_Free_Statement (This, Node);

         when N_Freeze_Entity =>
--              Freeze_Level := Freeze_Level + 1;
--              Expand_Node_List (This, Actions (Node));
--              Freeze_Level := Freeze_Level - 1;
            null;
            
         when N_Full_Type_Declaration =>
	    Expand_Full_Type_Declaration (This, Node);

         when N_Function_Call =>
            Expand_Function_Call (This, Node);

         when N_Function_Instantiation =>
	    Expand_Function_Instantiation (This, Node);

         when N_Function_Specification =>
	    Expand_Function_Specification (This, Node);

         when N_Generic_Association                    |
              N_Generic_Function_Renaming_Declaration  |
              N_Generic_Package_Declaration            |
              N_Generic_Package_Renaming_Declaration   |
              N_Generic_Procedure_Renaming_Declaration |
              N_Generic_Subprogram_Declaration
         =>
	    null;
	    
         when N_Goto_Statement =>
            Expand_Goto_Statement (This, Node);

         when N_Handled_Sequence_Of_Statements =>
	    Reflex.Expanders.Ch5.Expand_Handled_Sequence_Of_Statements (This, Node);

         when N_Identifier =>	
	    Reflex.Expanders.Ch2.Expand_Identifier (This, Node);

         when N_If_Statement =>
	    Reflex.Expanders.Ch5.Expand_If_Statement (This, Node);

         when N_Implicit_Label_Declaration =>
	    Expand_Implicit_Label_Declaration (This, Node);

         when N_In =>
	    Expand_In (This, Node);

         when N_Incomplete_Type_Declaration =>
            Expand_Incomplete_Type_Declaration (This, Node);

         when N_Index_Or_Discriminant_Constraint =>
            Expand_Index_Or_Discriminant_Constraint (This, Node);

         when N_Indexed_Component =>
	    Expand_Indexed_Component (This, Node);

         when N_Integer_Literal =>
	    Reflex.Expanders.Ch4.Expand_Integer_Literal (This, Node);

         when N_Iteration_Scheme =>
            raise Program_Error; -- handled as part of loop handling

--           when N_Itype_Reference =>
--              null;

         when N_Label =>
	    Expand_Label (This, Node);
	    
         when N_Loop_Parameter_Specification =>
            raise Program_Error; -- handled by N_Loop_Statement

         when N_Loop_Statement =>
	    Reflex.Expanders.Ch5.Expand_Loop_Statement (This, Node);

         when N_Mod_Clause =>
            raise Program_Error;

         when N_Modular_Type_Definition =>
            raise Program_Error;

         when N_Not_In =>
	    Expand_Not_In (This, Node);

         when N_Null =>
	    null;

         when N_Null_Statement =>
	    Expand_Null_Statement (This, Node);

         when N_Number_Declaration =>
	    Expand_Number_Declaration (This, Node);

         when N_Object_Declaration =>
	    Reflex.Expanders.Ch3.Expand_Object_Declaration (This, Node);
	    
         when N_Object_Renaming_Declaration =>
	    Expand_Object_Renaming_Declaration (This, Node);
	    
         when N_Op_Abs =>
	    Reflex.Expanders.Ch4.Expand_Op_Abs (This, Node);
	    
         when N_Op_Add =>
	    Reflex.Expanders.Ch4.Expand_Op_Add (This, Node);

         when N_Op_And =>
	    Reflex.Expanders.Ch4.Expand_Op_And (This, Node);

         when N_Op_Concat =>
            raise Program_Error; -- should always be expanded

         when N_Op_Divide =>
	    Reflex.Expanders.Ch4.Expand_Op_Divide (This, Node);

         when N_Op_Eq =>
	    Reflex.Expanders.Ch4.Expand_Op_Eq (This, Node);
	    
         when N_Op_Expon =>
	    Reflex.Expanders.Ch4.Expand_Op_Expon (This, Node);

         when N_Op_Ge =>
	    Reflex.Expanders.Ch4.Expand_Op_Ge (This, Node);

         when N_Op_Gt =>
	    Reflex.Expanders.Ch4.Expand_Op_Gt (This, Node);

         when N_Op_Le =>
	    Reflex.Expanders.Ch4.Expand_Op_Le (This, Node);

         when N_Op_Lt =>
	    Reflex.Expanders.Ch4.Expand_Op_Lt (This, Node);

         when N_Op_Minus =>
	    Reflex.Expanders.Ch4.Expand_Op_Minus (This, Node);

         when N_Op_Mod =>
	    Reflex.Expanders.Ch4.Expand_Op_Mod (This, Node);

         when N_Op_Multiply =>
	    Reflex.Expanders.Ch4.Expand_Op_Multiply (This, Node);

         when N_Op_Ne =>
	    Reflex.Expanders.Ch4.Expand_Op_Ne (This, Node);

         when N_Op_Not =>
	    Reflex.Expanders.Ch4.Expand_Op_Not (This, Node);

         when N_Op_Or =>
	    Reflex.Expanders.Ch4.Expand_Op_Or (This, Node);

         when N_Op_Plus =>
	    Reflex.Expanders.Ch4.Expand_Op_Plus (This, Node);

         when N_Op_Rem =>
	    Reflex.Expanders.Ch4.Expand_Op_Rem (This, Node);

         when N_Op_Rotate_Left | N_Op_Rotate_Right =>
	    
            --  Should have been rewritten in Modify_Tree_For_C mode

            raise Program_Error;

         when N_Op_Shift_Right =>
            Expand_Op_Shift_Right (This, Node);

         when N_Op_Shift_Right_Arithmetic =>
	    
            --  Should have been rewritten in Modify_Tree_For_C mode

            raise Program_Error;

         when N_Op_Shift_Left =>
            Expand_Op_Shift_Left (This, Node);

         when N_Op_Subtract =>
	    Reflex.Expanders.Ch4.Expand_Op_Subtract (This, Node);

         when N_Op_Xor =>
	    Reflex.Expanders.Ch4.Expand_Op_Xor (This, Node);

         when N_Operator_Symbol =>

            --  Replaced by the corresponding N_Op_XX node by the expander

            raise Program_Error;

         when N_Or_Else =>
	    Reflex.Expanders.Ch4.Expand_Or_Else (This, Node);

         when N_Others_Choice =>
            raise Program_Error;

         when N_Package_Body =>
            Expand_Package_Body (This, Node);

         when N_Package_Declaration =>
            Expand_Package_Declaration (This, Node);

         when N_Package_Instantiation | N_Package_Renaming_Declaration =>
            Expand_Package_Instantiation (This, Node);

         when N_Package_Specification =>
            Expand_Package_Specification (This, Node);

         when N_Parameter_Association =>
            raise Program_Error;

         when N_Parameter_Specification =>
            Expand_Parameter_Specification (This, Node);

         when N_Private_Extension_Declaration | N_Private_Type_Declaration =>
	    Expand_Private_Extension_Declaration (This, Node);

         when N_Pragma =>
	    null;

         when N_Pragma_Argument_Association =>
            raise Program_Error;

         when N_Procedure_Call_Statement =>
            Expand_Procedure_Call_Statement (This, Node);

         when N_Procedure_Instantiation =>
            Expand_Procedure_Instantiation (This, Node);

         when N_Procedure_Specification =>
            Expand_Procedure_Specification (This, Node);

         when N_Qualified_Expression =>
	    Expand_Qualified_Expression (This, Node);

         when N_Raise_xxx_Error | N_Raise_Statement =>
            Expand_Raise_Error (This, Node);

         when N_Range =>
            Expand_Range (This, Node);

         when N_Range_Constraint =>
            Expand_Range_Constraint (This, Node);

         when N_Real_Literal =>
            Expand_Real_Literal (This, Node);

         when N_Real_Range_Specification | N_Record_Definition =>
            Expand_Real_Range_Specification (This, Node);

         when N_Record_Representation_Clause =>
            null;

         when N_Reference =>
            Expand_Reference (This, Node);

         when N_Selected_Component =>
	    Reflex.Expanders.Ch2.Expand_Selected_Component (This, Node);
	    
         when N_Signed_Integer_Type_Definition 
              =>
            raise Program_Error;

         when N_Slice =>
            Expand_Slice (This, Node);

         when N_String_Literal =>
            Reflex.Expanders.Ch4.Expand_String_Literal (This, Node);

         when N_Subprogram_Body =>
	    Reflex.Expanders.Ch6.Expand_Subprogram_Body (This, Node);

         when N_Subprogram_Declaration =>
            Reflex.Expanders.Ch6.Expand_Subprogram_Declaration (This, Node);

         when N_Subprogram_Renaming_Declaration =>
            Expand_Subprogram_Renaming_Declaration (This, Node);

         when N_Subtype_Declaration =>
            Expand_Subtype_Declaration (This, Node);

         when N_Subtype_Indication =>
	    
            --  Should have been handled higher up in tree

            raise Program_Error;

         when N_Type_Conversion =>
            Reflex.Expanders.Ch4.Expand_Type_Conversion (This, Node);

         when N_Unchecked_Expression =>
            raise Program_Error;

         when N_Unchecked_Type_Conversion =>
            Expand_Unchecked_Type_Conversion (This, Node);
	    
         when N_Unconstrained_Array_Definition |
              N_Unused_At_Start                |
              N_Unused_At_End
         =>
            raise Program_Error;

         when N_Use_Package_Clause            |
              N_Use_Type_Clause               |
              N_Validate_Unchecked_Conversion
         =>
            Expand_Use_Package_Clause (This, Node);

         when N_With_Clause =>
            Expand_With_Clause (This, Node);
	    
	 when others => 
	    raise Program_Error;
      end case;
      
      Put_Line ("Expand_Node End " & Nkind (Node)'Img);
   exception
      when others =>
	 Put_Line ("Expansion Exception " & Nkind (Node)'Img);
	 Output.Set_Standard_Error;
	 Output.Write_Str
	   ("expansion exception =================> " & Nkind (Node)'Img);
	 Output.Write_Eol;
   end Expand_Node;

   -------------------------
   -- Terminate_Expansion --
   -------------------------

   procedure Terminate_Expansion
     (This        : access Reflex_Expander_Record;
      Node        : Node_Id; 
      Declaration : Boolean := False) is
      
   begin
      if Node = Empty then --  or else not Comes_From_Source (Node) then
         return;
      end if;
      
      --  Select print circuit based on node kind
      
      case Nkind (Node) is
         when N_Abstract_Subprogram_Declaration =>
	    null;
         when N_Access_Definition =>
	    null; --  Expand_Access_Definition (This, Node);
	    
         when N_Access_To_Object_Definition |
              N_Access_Function_Definition  |
              N_Access_Procedure_Definition =>

            --  Processed by Expand_Declare as part of processing the parent
            --  node (N_Full_Type_Declaration) or the itypes associated with
            --  anonymous access-to-subprogram types.

            raise Program_Error;

         when N_Aggregate =>
	    null;
	    
         when N_Allocator =>
	    
	    -- No yet Implemented
	    
	    raise Program_Error;

         when N_And_Then =>
	    null;
	    
         --  Note: the following code for N_Aspect_Specification is not used,
         --  since we deal with aspects as part of a declaration.

         when N_Assignment_Statement =>
	    null;
	    
         when N_At_Clause =>
            raise Program_Error;

         when N_Attribute_Definition_Clause =>
            null; ----------------  raise Program_Error;

         when N_Attribute_Reference =>
            null;

         when N_Block_Statement =>
	    null;
	    
         when N_Case_Statement =>
	    null;

         when N_Case_Statement_Alternative =>
	    null;

         when N_Character_Literal =>
	    null;
	    
         when N_Code_Statement =>
	    null;
	    
         when N_Compilation_Unit =>
	    null;

         when N_Compilation_Unit_Aux =>
            null;
	    -- nothing to do, never used, see above

         when N_Component_Association =>
            null;

         when N_Component_Clause =>
            raise Program_Error;

         when N_Component_Definition =>
	   null; --  Expand_Component_Definition (This, Node);

         when N_Component_Declaration =>
            null; -- Glips.Decls.Expand_Component_Declaration (This, Node);

         when N_Component_List =>
	    null;

         when N_Defining_Character_Literal =>
	    null;

         when N_Defining_Identifier =>
	    null;
	    
         when N_Defining_Operator_Symbol =>
            null;

         when N_Defining_Program_Unit_Name =>
            null;

         when N_Derived_Type_Definition =>
	    null;

         when N_Designator | N_Digits_Constraint =>
            raise Program_Error;

         when N_Discriminant_Association =>
	    null;

         when N_Elsif_Part =>
	    null; --  Reflex.Expanders.Ch5.Expand_Elsif_Part (This, Node);

         when N_Empty =>
            null;

         when N_Enumeration_Representation_Clause |
              N_Enumeration_Type_Definition
         =>
            null;

         when N_Error =>
            null;

         when N_Exception_Handler =>
            null; -- not output in C code

         when N_Exception_Declaration          |
              N_Exception_Renaming_Declaration
         =>
	    raise Program_Error;
	    
         when N_Exit_Statement =>
	    null;
	    
         when N_Expanded_Name =>
	    null;
	    
         when N_Explicit_Dereference =>
	    null;

--           when N_Expression_With_Actions =>
--              Expand_Expression_With_Actions (This, Node);
	    
         when N_Return_Statement =>
            raise Program_Error;

         when N_Extension_Aggregate =>
	    null;

         when N_Floating_Point_Definition              |
              N_Formal_Derived_Type_Definition         |
              N_Formal_Discrete_Type_Definition        |
              N_Formal_Floating_Point_Definition       |
              N_Formal_Modular_Type_Definition         |
              N_Formal_Object_Declaration              |
              N_Formal_Package_Declaration             |
              N_Formal_Private_Type_Definition         |
              N_Formal_Signed_Integer_Type_Definition  |
              N_Formal_Type_Declaration
         =>
            null; -- not output in C code

         when N_Free_Statement =>
            null;

         when N_Freeze_Entity =>
--              Freeze_Level := Freeze_Level + 1;
--              Expand_Node_List (This, Actions (Node));
--              Freeze_Level := Freeze_Level - 1;
            null;
            
         when N_Full_Type_Declaration =>
	    null;
	    
         when N_Function_Call =>
            null;

         when N_Function_Instantiation =>
	    null;

         when N_Function_Specification =>
	    null;

         when N_Generic_Association                    |
              N_Generic_Function_Renaming_Declaration  |
              N_Generic_Package_Declaration            |
              N_Generic_Package_Renaming_Declaration   |
              N_Generic_Procedure_Renaming_Declaration |
              N_Generic_Subprogram_Declaration
         =>
	    null;
	    
         when N_Goto_Statement =>
            null;
	    
         when N_Handled_Sequence_Of_Statements =>
	    null;

         when N_Identifier =>	
	    null;

         when N_If_Statement =>
	    null;

         when N_Implicit_Label_Declaration =>
	    null;

         when N_In =>
	    null;

         when N_Incomplete_Type_Declaration =>
            null;

         when N_Index_Or_Discriminant_Constraint =>
            null;

         when N_Indexed_Component =>
	    null;

         when N_Integer_Literal =>
	    null;

         when N_Iteration_Scheme =>
            raise Program_Error; -- handled as part of loop handling

         when N_Label =>
	    null;
	    
         when N_Loop_Parameter_Specification =>
            raise Program_Error; -- handled by N_Loop_Statement

         when N_Loop_Statement =>
	    null;

         when N_Mod_Clause =>
            raise Program_Error;

         when N_Modular_Type_Definition =>
            raise Program_Error;

         when N_Not_In =>
	    null;

         when N_Null =>
	    null;

         when N_Null_Statement =>
	    null;

         when N_Number_Declaration =>
	    null;

         when N_Object_Declaration =>
	    null;
	    
         when N_Object_Renaming_Declaration =>
	    null;
	    
         when N_Op_Abs =>
	    null;
	    
         when N_Op_Add =>
	    null;

         when N_Op_And =>
	    null;

         when N_Op_Concat =>
            raise Program_Error; -- should always be expanded

         when N_Op_Divide =>
	    null;

         when N_Op_Eq =>
	    null;
	    
         when N_Op_Expon =>
	    null;

         when N_Op_Ge =>
	    null;

         when N_Op_Gt =>
	    null;

         when N_Op_Le =>
	    null;

         when N_Op_Lt =>
	    null;

         when N_Op_Minus =>
	    null;

         when N_Op_Mod =>
	    null;

         when N_Op_Multiply =>
	    null;

         when N_Op_Ne =>
	    null;

         when N_Op_Not =>
	    null;

         when N_Op_Or =>
	    null;

         when N_Op_Plus =>
	    null;

         when N_Op_Rem =>
	    null;

         when N_Op_Rotate_Left | N_Op_Rotate_Right =>
	    
            --  Should have been rewritten in Modify_Tree_For_C mode

            raise Program_Error;

         when N_Op_Shift_Right =>
            null;

         when N_Op_Shift_Right_Arithmetic =>
	    
            --  Should have been rewritten in Modify_Tree_For_C mode

            raise Program_Error;

         when N_Op_Shift_Left =>
            null;

         when N_Op_Subtract =>
	    null;

         when N_Op_Xor =>
	    null;

         when N_Operator_Symbol =>

            --  Replaced by the corresponding N_Op_XX node by the expander

            raise Program_Error;

         when N_Or_Else =>
	    null;

         when N_Others_Choice =>
            raise Program_Error;

         when N_Package_Body =>
            null;

         when N_Package_Declaration =>
            null;

         when N_Package_Instantiation | N_Package_Renaming_Declaration =>
            null;

         when N_Package_Specification =>
            null;

         when N_Parameter_Association =>
            raise Program_Error;

         when N_Parameter_Specification =>
            null;

         when N_Private_Extension_Declaration | N_Private_Type_Declaration =>
	    null;

         when N_Pragma =>
	    null;

         when N_Pragma_Argument_Association =>
            raise Program_Error;

         when N_Procedure_Call_Statement =>
            null;

         when N_Procedure_Instantiation =>
            null;

         when N_Procedure_Specification =>
            null;

         when N_Qualified_Expression =>
	    null;

         when N_Raise_xxx_Error | N_Raise_Statement =>
            null;

         when N_Range =>
            null;

         when N_Range_Constraint =>
            null;

         when N_Real_Literal =>
            null;

         when N_Real_Range_Specification | N_Record_Definition =>
            null;

         when N_Record_Representation_Clause =>
            null;

         when N_Reference =>
            null;

         when N_Selected_Component =>
	    null;
	    
         when N_Signed_Integer_Type_Definition 
              =>
            raise Program_Error;

         when N_Slice =>
            null;

         when N_String_Literal =>
            null;

         when N_Subprogram_Body =>
	    null;

         when N_Subprogram_Declaration =>
            null;

         when N_Subprogram_Renaming_Declaration =>
            null;

         when N_Subtype_Declaration =>
            null;

         when N_Subtype_Indication =>
	    
            --  Should have been handled higher up in tree

            raise Program_Error;

         when N_Type_Conversion =>
            null;

         when N_Unchecked_Expression =>
            raise Program_Error;

         when N_Unchecked_Type_Conversion =>
            null;
	    
         when N_Unconstrained_Array_Definition |
              N_Unused_At_Start                |
              N_Unused_At_End
         =>
            raise Program_Error;

         when N_Use_Package_Clause            |
              N_Use_Type_Clause               |
              N_Validate_Unchecked_Conversion
         =>
            null;

         when N_With_Clause =>
            null;
	    
	 when others => 
	    raise Program_Error;
      end case;
   exception
      when others =>
	 Put_Line ("Expansion Exception " & Nkind (Node)'Img);
	 Output.Set_Standard_Error;
	 Output.Write_Str
	   ("expansion exception =================> " & Nkind (Node)'Img);
	 Output.Write_Eol;
   end Terminate_Expansion;

end Reflex.Expanders.Dispatch;
