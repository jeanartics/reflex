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

with Reflex.Gen.Types; use Reflex.Gen.Types;
with Glips.Gen.Ch2; use Glips.Gen.Ch2;
with Glips.Gen.Ch3; use Glips.Gen.Ch3;
with Glips.Gen.Ch4; use Glips.Gen.Ch4;
with Glips.Gen.Ch5; use Glips.Gen.Ch5;
with Glips.Gen.Ch6; use Glips.Gen.Ch6;
with Glips.Gen.Ch7; use Glips.Gen.Ch7;
with Glips.Gen.Ch10; use Glips.Gen.Ch10;
with Reflex.Gen.Outputs; use Reflex.Gen.Outputs;

package body Glips.Gen.Dispatch is

   -------------------
   -- Generate_Node --
   -------------------

   procedure Generate_Node
     (This        : access Glips_Generator_Record;
      Node        : Node_Id; 
      Declaration : Boolean := False) is
   begin
      -- Put_Line ("Glips.Gen.Dispatch: Generate_Node " & Nkind (Node)'Img);
      if Node = Empty then --  or else not Comes_From_Source (Node) then
         return;
      end if;
      
--      This.Set_Output_Buffer (This.Get_Dummy_Output_Buffer);
      
      --  Select print circuit based on node kind
      
      case Nkind (Node) is
	 
	 --  Types declarations 
	 
         when N_Full_Type_Declaration
	   | N_Incomplete_Type_Declaration
	   | N_Private_Extension_Declaration 
	   | N_Private_Type_Declaration        =>
	    
	    Generate_Type_Declaration (This, Node);
	    
	    --  Subtypes declarations 
	    
         when N_Subtype_Declaration =>
            Generate_Subtype_Declaration (This, Node);
	    
	    --  Variable Declaration
	    
         when N_Object_Declaration =>
	    Glips.Gen.Ch3.Generate_Object_Declaration (This, Node);
	    
         when N_Object_Renaming_Declaration =>
	    Generate_Object_Renaming_Declaration (This, Node);
	    
	    
	    
	 
         when N_Abstract_Subprogram_Declaration =>
	    null; -- Generate_Abstract_Subprogram_Declaration (This, Node);

         when N_Access_Definition =>
	    Generate_Access_Definition (This, Node);
	    
         when N_Access_To_Object_Definition |
              N_Access_Function_Definition  |
              N_Access_Procedure_Definition =>

            --  Processed by Generate_Declare as part of processing the parent
            --  node (N_Full_Type_Declaration) or the itypes associated with
            --  anonymous access-to-subprogram types.

            raise Program_Error;

         when N_Aggregate =>
	    Glips.Gen.Ch4.Generate_Aggregate (This, Node);
	    
         when N_Allocator =>
	    
	    -- No yet Implemented
	    
	    raise Program_Error;

         when N_And_Then =>
	    Glips.Gen.Ch4.Generate_And_Then (This, Node);
	    
         --  Note: the following code for N_Aspect_Specification is not used,
         --  since we deal with aspects as part of a declaration.

--           when N_Aspect_Specification =>
--              raise Program_Error;

         when N_Assignment_Statement =>
	    Glips.Gen.Ch5.Generate_Assignment_Statement (This, Node);
	    
--           when N_Asynchronous_Select | N_At_Clause =>
--              raise Program_Error;

         when N_Attribute_Definition_Clause =>
            null; ----------------  raise Program_Error;

         when N_Attribute_Reference =>
            Generate_Attribute_Reference (This, Node);

         when N_Block_Statement =>
	    Generate_Block_Statement (This, Node);
	    
--           when N_Body_Stub =>
--  	    Generate_Body_Stub (This, Node);
--  	    
--           when N_Case_Expression =>
--  
--              --  We should not see case expressions in a fully expanded tree,
--              --  since they are always replaced by case statements.
--  
--              raise Program_Error;

--           when N_Case_Expression_Alternative =>
--              raise Program_Error;

         when N_Case_Statement =>
	    Glips.Gen.Ch5.Generate_Case_Statement (This, Node);

         when N_Case_Statement_Alternative =>
	    Generate_Case_Statement_Alternative (This, Node);

         when N_Character_Literal =>
	    null;
	    
         when N_Code_Statement =>
	    Generate_Code_Statement (This, Node);
	    
         when N_Compilation_Unit =>
	    Generate_Compilation_Unit (This, Node);

         when N_Compilation_Unit_Aux =>
            Generate_Compilation_Unit_Aux (This, Node);
	    -- nothing to do, never used, see above

         when N_Component_Association =>
            Generate_Component_Association (This, Node);

         when N_Component_Clause =>
            raise Program_Error;

         when N_Component_Definition =>
	    raise Program_Error; --  Generate_Component_Definition (This, Node);

         when N_Component_Declaration =>
            null; -- Glips.Decls.Generate_Component_Declaration (This, Node);

         when N_Component_List =>
	    raise Program_Error; --  Generate_Component_List (This, Node);

--           when N_Compound_Statement =>
--  	    Generate_Compound_Statement (This, Node);
--  
--           when N_Conditional_Entry_Call         |
--                N_Constrained_Array_Definition   |
--                N_Contract                       |
--                N_Decimal_Fixed_Point_Definition
--           =>
--              raise Program_Error;

         when N_Defining_Character_Literal =>
	    Generate_Defining_Character_Literal (This, Node);

         when N_Defining_Identifier =>
	    Generate_Defining_Identifier (This, Node, Declaration);
	    
         when N_Defining_Operator_Symbol =>
            Generate_Defining_Operator_Symbol (This, Node);

         when N_Defining_Program_Unit_Name =>
            Generate_Defining_Program_Unit_Name (This, Node);

--           when N_Delay_Alternative        |
--                N_Delay_Relative_Statement |
--                N_Delay_Until_Statement
--           =>
--              raise Program_Error; -- should not occur in generated code

         when N_Derived_Type_Definition =>
	    raise Program_Error; --  Generate_Derived_Type_Definition (This, Node);

         when N_Designator | N_Digits_Constraint =>
            raise Program_Error;

         when N_Discriminant_Association =>
	    null; -- Generate_Discriminant_Association (This, Node);

         when N_Elsif_Part =>
	    null; --  Glips.Gen.Ch5.Generate_Elsif_Part (This, Node);

         when N_Empty =>
            null;

         when N_Enumeration_Representation_Clause |
              N_Enumeration_Type_Definition
         =>
            Glips.Gen.Ch3.Generate_Enumeration_Type_Definition (This, Node);

         when N_Error =>
            null;

         when N_Exception_Handler =>
            null; -- not output in C code

         when N_Exception_Declaration          |
              N_Exception_Renaming_Declaration
         =>
	    raise Program_Error;
	    
         when N_Exit_Statement =>
	    Glips.Gen.Ch5.Generate_Exit_Statement (This, Node);
	    
         when N_Expanded_Name =>
	    Glips.Gen.Ch2.Generate_Expanded_Name (This, Node);

         when N_Explicit_Dereference =>
	    Generate_Explicit_Dereference (This, Node);

--           when N_Expression_With_Actions =>
--              Generate_Expression_With_Actions (This, Node);
	    
--           when N_Expression_Function =>
--              Generate_Expression_Function (This, Node);

         when N_Extension_Aggregate =>
	    Generate_Extension_Aggregate (This, Node);

         when N_Floating_Point_Definition              |
              N_Formal_Derived_Type_Definition         |
--              N_Formal_Abstract_Subprogram_Declaration |
--              N_Formal_Concrete_Subprogram_Declaration |
              N_Formal_Discrete_Type_Definition        |
              N_Formal_Floating_Point_Definition       |
              N_Formal_Modular_Type_Definition         |
              N_Formal_Object_Declaration              |
              N_Formal_Package_Declaration             |
              N_Formal_Private_Type_Definition         |
--              N_Formal_Incomplete_Type_Definition      |
              N_Formal_Signed_Integer_Type_Definition  |
              N_Formal_Type_Declaration
         =>
            null; -- not output in C code

         when N_Free_Statement =>
            Generate_Free_Statement (This, Node);

         when N_Freeze_Entity =>
--              Freeze_Level := Freeze_Level + 1;
--              Generate_Node_List (This, Actions (Node));
--              Freeze_Level := Freeze_Level - 1;
            null;
            
--           when N_Freeze_Generic_Entity =>
--              null; -- not output in C code

         when N_Function_Call =>
            Generate_Function_Call (This, Node);

         when N_Function_Instantiation =>
	    null; -- Generate_Function_Instantiation (This, Node);

         when N_Function_Specification =>
	    null; -- Generate_Function_Specification (This, Node);

         when N_Generic_Association                    |
              N_Generic_Function_Renaming_Declaration  |
              N_Generic_Package_Declaration            |
              N_Generic_Package_Renaming_Declaration   |
              N_Generic_Procedure_Renaming_Declaration |
              N_Generic_Subprogram_Declaration
         =>
	    null;
	    
         when N_Goto_Statement =>
            Generate_Goto_Statement (This, Node);

         when N_Handled_Sequence_Of_Statements =>
	    Glips.Gen.Ch5.Generate_Handled_Sequence_Of_Statements (This, Node);

         when N_Identifier =>	
	    Glips.Gen.Ch2.Generate_Identifier (This, Node);

--           when N_If_Expression =>
--  	    Generate_If_Expression (This, Node);

         when N_If_Statement =>
	    Glips.Gen.Ch5.Generate_If_Statement (This, Node);

         when N_Implicit_Label_Declaration =>
	    null; -- Generate_Implicit_Label_Declaration (This, Node);

         when N_In =>
	    Generate_In (This, Node);

         when N_Index_Or_Discriminant_Constraint =>
            null; -- Generate_Index_Or_Discriminant_Constraint (This, Node);

         when N_Indexed_Component =>
	    Generate_Indexed_Component (This, Node);

         when N_Integer_Literal =>
	    Glips.Gen.Ch4.Generate_Integer_Literal (This, Node);

         when N_Iteration_Scheme =>
            raise Program_Error; -- handled as part of loop handling

--           when N_Iterator_Specification =>
--  	    Generate_Iterator_Specification (This, Node);

--           when N_Itype_Reference =>
--              null;

         when N_Label =>
	    Generate_Label (This, Node);
	    
         when N_Loop_Parameter_Specification =>
            raise Program_Error; -- handled by N_Loop_Statement

         when N_Loop_Statement =>
	    Glips.Gen.Ch5.Generate_Loop_Statement (This, Node);

         when N_Mod_Clause =>
            raise Program_Error;

         when N_Modular_Type_Definition =>
            raise Program_Error;

         when N_Not_In =>
	    Generate_Not_In (This, Node);

         when N_Null =>
	    null;

         when N_Null_Statement =>
	    Generate_Null_Statement (This, Node);

         when N_Number_Declaration =>
	    Generate_Number_Declaration (This, Node);

         when N_Op_Abs =>
	    Glips.Gen.Ch4.Generate_Op_Abs (This, Node);
	    
         when N_Op_Add =>
	    Glips.Gen.Ch4.Generate_Op_Add (This, Node);

         when N_Op_And =>
	    Glips.Gen.Ch4.Generate_Op_And (This, Node);

         when N_Op_Concat =>
            raise Program_Error; -- should always be expanded

         when N_Op_Divide =>
	    Glips.Gen.Ch4.Generate_Op_Divide (This, Node);

         when N_Op_Eq =>
	    Glips.Gen.Ch4.Generate_Op_Eq (This, Node);
	    
         when N_Op_Expon =>
	    Glips.Gen.Ch4.Generate_Op_Expon (This, Node);

         when N_Op_Ge =>
	    Glips.Gen.Ch4.Generate_Op_Ge (This, Node);

         when N_Op_Gt =>
	    Glips.Gen.Ch4.Generate_Op_Gt (This, Node);

         when N_Op_Le =>
	    Glips.Gen.Ch4.Generate_Op_Le (This, Node);

         when N_Op_Lt =>
	    Glips.Gen.Ch4.Generate_Op_Lt (This, Node);

         when N_Op_Minus =>
	    Glips.Gen.Ch4.Generate_Op_Minus (This, Node);

         when N_Op_Mod =>
	    Glips.Gen.Ch4.Generate_Op_Mod (This, Node);

         when N_Op_Multiply =>
	    Glips.Gen.Ch4.Generate_Op_Multiply (This, Node);

         when N_Op_Ne =>
	    Glips.Gen.Ch4.Generate_Op_Ne (This, Node);

         when N_Op_Not =>
	    Glips.Gen.Ch4.Generate_Op_Not (This, Node);

         when N_Op_Or =>
	    Glips.Gen.Ch4.Generate_Op_Or (This, Node);

         when N_Op_Plus =>
	    Glips.Gen.Ch4.Generate_Op_Plus (This, Node);

         when N_Op_Rem =>
	    Glips.Gen.Ch4.Generate_Op_Rem (This, Node);

         when N_Op_Rotate_Left | N_Op_Rotate_Right =>
	    
            --  Should have been rewritten in Modify_Tree_For_C mode

            raise Program_Error;

         when N_Op_Shift_Right =>
            Generate_Op_Shift_Right (This, Node);

         when N_Op_Shift_Right_Arithmetic =>
	    
            --  Should have been rewritten in Modify_Tree_For_C mode

            raise Program_Error;

         when N_Op_Shift_Left =>
            Generate_Op_Shift_Left (This, Node);

         when N_Op_Subtract =>
	    Glips.Gen.Ch4.Generate_Op_Subtract (This, Node);

         when N_Op_Xor =>
	    Glips.Gen.Ch4.Generate_Op_Xor (This, Node);

         when N_Operator_Symbol =>

            --  Replaced by the corresponding N_Op_XX node by the expander

            raise Program_Error;

         when N_Or_Else =>
	    Glips.Gen.Ch4.Generate_Or_Else (This, Node);

         when N_Others_Choice =>
            raise Program_Error;

         when N_Package_Body =>
            Generate_Package_Body (This, Node);

         when N_Package_Declaration =>
            Generate_Package_Declaration (This, Node);

         when N_Package_Instantiation | N_Package_Renaming_Declaration =>
            null; -- Generate_Package_Instantiation (This, Node);

         when N_Package_Specification =>
            Generate_Package_Specification (This, Node);

         when N_Parameter_Association =>
            raise Program_Error;

         when N_Parameter_Specification =>
            Generate_Parameter_Specification (This, Node);

         when N_Pragma =>
	    null;

         when N_Pragma_Argument_Association =>
            raise Program_Error;

         when N_Procedure_Call_Statement =>
            Generate_Procedure_Call_Statement (This, Node);

         when N_Procedure_Instantiation =>
            Generate_Procedure_Instantiation (This, Node);

         when N_Procedure_Specification =>
            null; -- Generate_Procedure_Specification (This, Node);

         when N_Qualified_Expression =>
	    Generate_Qualified_Expression (This, Node);

--           when N_Raise_Expression =>
--              Generate_Raise_Expression (This, Node);

         when N_Raise_xxx_Error | N_Raise_Statement =>
            Generate_Raise_Error (This, Node);

         when N_Range =>
            Generate_Range (This, Node);

         when N_Range_Constraint =>
            Generate_Range_Constraint (This, Node);

         when N_Real_Literal =>
            Generate_Real_Literal (This, Node);

         when N_Real_Range_Specification | N_Record_Definition =>
            Generate_Real_Range_Specification (This, Node);

         when N_Record_Representation_Clause =>
            null;

         when N_Reference =>
            Generate_Reference (This, Node);

         when N_Return_Statement =>
            Generate_Simple_Return_Statement (This, Node);

         when N_Selected_Component =>
	    Glips.Gen.Ch2.Generate_Selected_Component (This, Node);
	    
         when N_Signed_Integer_Type_Definition =>
null;
         when N_Slice =>
            Generate_Slice (This, Node);

         when N_String_Literal =>
            Glips.Gen.Ch4.Generate_String_Literal (This, Node);

         when N_Subprogram_Body =>
	    Glips.Gen.Ch6.Generate_Subprogram_Body (This, Node);

         when N_Subprogram_Declaration =>
            Glips.Gen.Ch6.Generate_Subprogram_Declaration (This, Node);

         when N_Subprogram_Renaming_Declaration =>
            Generate_Subprogram_Renaming_Declaration (This, Node);

         when N_Subtype_Indication =>
	    
            --  Should have been handled higher up in tree

            raise Program_Error;

--           when N_Subunit =>
--  
--              --  This kind of node is not visible to the back end, since it has
--              --  been replaced by the corresponding N_Body_Stub node.
--  
--              Generate_Subunit (This, Node);

         when N_Type_Conversion =>
            Glips.Gen.Ch4.Generate_Type_Conversion (This, Node);

         when N_Unchecked_Expression =>
            raise Program_Error;

         when N_Unchecked_Type_Conversion =>
            Generate_Unchecked_Type_Conversion (This, Node);
	    
--  	 when N_Reactive_Pause_Statement..N_Reactive_Step_Definition =>
--  	    null;
         when N_Unconstrained_Array_Definition |
              N_Unused_At_Start                |
              N_Unused_At_End
         =>
            raise Program_Error;

         when N_Use_Package_Clause            |
              N_Use_Type_Clause               |
              N_Validate_Unchecked_Conversion
         =>
            Generate_Use_Package_Clause (This, Node);

         when N_With_Clause =>
            Generate_With_Clause (This, Node);
	    
	 when others => 
	    raise Program_Error;
      end case;
   exception
      when others =>
	 Put_Line ("exception generator => "  & Nkind (Node)'Img);
	 Write_Str (This.Get_Output_Buffer, "!!EROR!! exception");
	 Output.Set_Standard_Error;
	 Output.Write_Str
	   ("exception Generator =================> " & Nkind (Node)'Img);
	 Output.Write_Eol;
   end Generate_Node;
   
   ---------------------------------
   -- Generate_Literal_Expression --
   ---------------------------------
   
   procedure Generate_Literal_Expression
     (This : access Glips_Generator_Record;
      Node : Node_Id;
      Ob   : Output_Buffer) is
      
      Prv : Output_Buffer;
   begin
      Prv := This.Get_Output_Buffer;
      This.Set_Output_Buffer (Ob);
      
      if Present (Node) then
	 Generate_Node (This, Node);
      else
	 Write_Eol (Ob);
	 Write_Str (Ob, "NO STATEMENT");
	 Write_Eol (Ob);
      end if;
      
      This.Set_Output_Buffer (Prv);
   end Generate_Literal_Expression;
   
end Glips.Gen.Dispatch;
