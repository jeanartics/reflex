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

with Ada.Text_IO; use Ada.Text_IO;

with Atree;  use Atree;
with Sinfo;  use Sinfo;
with Output;

with Reflex.Boxes.Ch2;  use Reflex.Boxes.Ch2;
with Reflex.Boxes.Ch4;  use Reflex.Boxes.Ch4;
with Reflex.Boxes.Ch5;  use Reflex.Boxes.Ch5;
with Reflex.Boxes.Ch6;  use Reflex.Boxes.Ch6;

package body Reflex.Boxes.Dispatch is

   -------------------------------
   -- Boxes_Build_Node_Dispatch --
   -------------------------------

   procedure Boxes_Node_Dispatch
     (This        : access Builder_Record;
      Node        : Node_Id; 
      Declaration : Boolean := False) is
   begin
      if Node = Empty then
         return;
      end if;
      
      case Nkind (Node) is
         
         when N_Full_Type_Declaration
            | N_Incomplete_Type_Declaration
            | N_Private_Extension_Declaration 
            | N_Private_Type_Declaration        =>
            null;
	    
         when N_Subtype_Declaration =>
            null;
  	    
         when N_Object_Declaration =>
            null;
            
         when N_Object_Renaming_Declaration =>
            null;
            
         when N_Abstract_Subprogram_Declaration =>
            null;

         when N_Access_Definition =>
            null;
            
         when N_Access_To_Object_Definition |
              N_Access_Function_Definition  |
              N_Access_Procedure_Definition =>
            raise Program_Error;

         when N_Aggregate =>
            null;
            
         when N_Allocator =>
            raise Program_Error;

         when N_And_Then =>
            raise Program_Error;	    

         when N_Assignment_Statement =>
            Boxes_Build_Assignment_Statement (This, Node);
            
         when N_Attribute_Definition_Clause =>
            null; 

         when N_Attribute_Reference =>
            null;
            
         when N_Block_Statement =>
            null;
            
         when N_Case_Statement =>
            raise Program_Error;  -- handled by expander
            
         when N_Case_Statement_Alternative =>
            raise Program_Error; -- handled by expander
            
         when N_Character_Literal =>
            null;
            
         when N_Code_Statement =>
            null;
            
         when N_Compilation_Unit =>
            null;
            
         when N_Compilation_Unit_Aux =>
            null;
            
         when N_Component_Association =>
            null;
            
         when N_Component_Clause =>
            raise Program_Error;

         when N_Component_Definition =>
            raise Program_Error;

         when N_Component_Declaration =>
            null;
            
         when N_Component_List =>
            raise Program_Error;

         when N_Constrained_Array_Definition   
            =>
            raise Program_Error;

         when N_Defining_Character_Literal =>
            null;
            
         when N_Defining_Identifier =>
            null;
            
         when N_Defining_Operator_Symbol =>
            null;
         when N_Defining_Program_Unit_Name =>
            null;
            
         when N_Derived_Type_Definition =>
            raise Program_Error;

         when N_Designator | N_Digits_Constraint =>
            raise Program_Error;

         when N_Discriminant_Association =>
            null;
            
         when N_Elsif_Part =>
            raise Program_Error; -- handled by expander

         when N_Empty =>
            null;

         when N_Enumeration_Representation_Clause |
              N_Enumeration_Type_Definition
            =>
            null;
            
         when N_Error =>
            null;

         when N_Exception_Handler =>
            null; 

         when N_Exception_Declaration          |
              N_Exception_Renaming_Declaration
            =>
            raise Program_Error;
	    
         when N_Exit_Statement =>
            raise Program_Error; -- handled by expander
            
         when N_Expanded_Name =>
            null;
            
         when N_Explicit_Dereference =>
            null;
            
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
            null; 

         when N_Free_Statement =>
            null;
            
         when N_Freeze_Entity =>
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
            Boxes_Build_Goto_Statement (This, Node);
                  
         when N_Handled_Sequence_Of_Statements =>
            Reflex.Boxes.Ch5.Boxes_Build_Handled_Sequence_Of_Statements
              (This, Node);
            
         when N_Identifier =>	
            Boxes_Build_Identifier (This, Node);

         when N_If_Statement =>
            Boxes_Build_If_Statement (This, Node);
            
         when N_Implicit_Label_Declaration =>
            null;
            
         when N_In =>
            raise Program_Error; -- handled by expander

         when N_Index_Or_Discriminant_Constraint =>
            null;
            
         when N_Indexed_Component =>
            null;
            
         when N_Integer_Literal =>
            null;
            
         when N_Iteration_Scheme =>
            raise Program_Error; --  handled by expander

--           when N_Itype_Reference =>
--              null;

         when N_Label =>
            Boxes_Build_Label (This, Node);
            
         when N_Loop_Parameter_Specification =>
            raise Program_Error; --  handled by expander

         when N_Loop_Statement =>
            raise Program_Error; --  handled by expander
            
         when N_Mod_Clause =>
            raise Program_Error; 

         when N_Modular_Type_Definition =>
            raise Program_Error;

         when N_Not_In =>
            raise Program_Error; -- handled by expander
            
         when N_Null =>
            null;

         when N_Null_Statement =>
            Boxes_Build_Null_Statement (This, Node);
            
         when N_Number_Declaration =>
            null;
            
         when N_Op_Abs =>
            --  Boxes_Build_Op_Abs (This, Node);
            null;
            
         when N_Op_Add =>
            null;
            
         when N_Op_And =>
            Boxes_Build_Op_And (This, Node);
            
         when N_Op_Concat =>
            null;

         when N_Op_Divide =>
            null;
            
         when N_Op_Eq =>
            Boxes_Build_Op_Eq (This, Node);
            
         when N_Op_Expon =>
            -- Boxes_Build_Op_Expon (This, Node);
            null;
            
         when N_Op_Ge =>
            Boxes_Build_Op_Ge (This, Node);
            
         when N_Op_Gt =>
            Boxes_Build_Op_Gt (This, Node);
         
         when N_Op_Le =>
            Boxes_Build_Op_Le (This, Node);

         when N_Op_Lt =>
            Boxes_Build_Op_Lt (This, Node);
            
         when N_Op_Minus =>
            Boxes_Build_Op_Minus (This, Node);
            
         when N_Op_Mod =>
            -- Boxes_Build_Op_Mod (This, Node);
            null;
         when N_Op_Multiply =>
            null;
            
         when N_Op_Ne =>
            Boxes_Build_Op_Ne (This, Node);
            
         when N_Op_Not =>
            Boxes_Build_Op_Not (This, Node);
            
         when N_Op_Or =>
            Boxes_Build_Op_Or (This, Node);
            
         when N_Op_Plus =>
            Boxes_Build_Op_Plus (This, Node);
            
         when N_Op_Rem =>
            Boxes_Build_Op_Rem (This, Node);
            
         when N_Op_Rotate_Left | N_Op_Rotate_Right =>
            
            raise Program_Error;

         when N_Op_Shift_Right =>
            null;
         when N_Op_Shift_Right_Arithmetic =>
	    
            raise Program_Error;

         when N_Op_Shift_Left =>
            null;
            
         when N_Op_Subtract =>
            null;
            
         when N_Op_Xor =>
            raise Program_Error; -- handled by expander
            
         when N_Operator_Symbol =>
            raise Program_Error;

         when N_Or_Else =>
            raise Program_Error;
            
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
            
         when N_Pragma =>
            null;

         when N_Pragma_Argument_Association =>
            raise Program_Error;
            
         when N_Procedure_Call_Statement =>
            Boxes_Build_Procedure_Call_Statement (This, Node);
            null;
            
         when N_Procedure_Instantiation =>
            Boxes_Build_Procedure_Instantiation (This, Node);
            null;
            
         when N_Procedure_Specification =>
            Boxes_Build_Procedure_Specification (This, Node);
            null;
            
         when N_Qualified_Expression =>
            null;
            
         when N_Raise_Xxx_Error | N_Raise_Statement =>
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
            
         when N_Return_Statement =>
            Boxes_Build_Simple_Return_Statement (This, Node);

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
            
         when N_Subtype_Indication =>
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
         Put_Line("exception =================> " & Nkind (Node)'Img);
   end Boxes_Node_Dispatch;

end  Reflex.Boxes.Dispatch;
