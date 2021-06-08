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

with Ada.Unchecked_Deallocation;

package body Reflex.Visitor is
   
   ------------------------
   -- New_Reflex_Visitor --
   ------------------------
   
   function New_Reflex_Visitor return Visitor_Ptr is
   begin
      return new Visitor_Record'(No_Visitor_Record);
   end New_Reflex_Visitor;
   
   ------------------------
   -- New_Reflex_Visitor --
   ------------------------
   
   function New_Reflex_Visitor (N : Node_Id) return Visitor_Ptr is
      This : Visitor_Ptr := New_Reflex_Visitor;
   begin
      This.Root_Node := N;
      return This;
   end New_Reflex_Visitor;
   
   ---------------------------
   -- Delete_Reflex_Visitor --
   ---------------------------
   
   procedure Delete_Reflex_Visitor (This : in out Visitor_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Visitor_Record, Visitor_Ptr);
   begin
      if This /= null then
	 Free (This);
      end if;
   end Delete_Reflex_Visitor;
   
   -------------------------------------------
   -- Visit_Abstract_Subprogram_Declaration --
   -------------------------------------------
   
   procedure Visit_Abstract_Subprogram_Declaration
     (This : access Visitor_Record;
      N    : Node_id) is
   begin
      null;
   end Visit_Abstract_Subprogram_Declaration;
   
   ---------------------
   -- Visit_Aggregate --
   ---------------------
   
   procedure Visit_Aggregate
     (This : access Visitor_Record;
      N    : Node_id) is
   begin
      null;
   end Visit_Aggregate;
   
   ---------------------
   -- Visit_Allocator --
   ---------------------
   
   procedure Visit_Allocator
     (This : access Visitor_Record;
      N    : Node_id) is
   begin
      null;
   end Visit_Allocator;
   
   -------------------------
   -- Visit_Short_Circuit --
   -------------------------
   
   procedure Visit_Short_Circuit
     (This : access Visitor_Record;
      N    : Node_id) is
   begin
      null;
   end Visit_Short_Circuit;
   
   --------------------------------
   -- Visit_Aspect_Specification --
   --------------------------------
   
   procedure Visit_Aspect_Specification 
     (This : access Visitor_Record;
      N    : Node_id) is
   begin
      null;
   end Visit_Aspect_Specification;
   
   ----------------------
   -- Visit_Assignment --
   ----------------------
   
   procedure Visit_Assignment 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Assignment;
   
   ---------------------
   -- Visit_At_Clause --
   ---------------------
   
   procedure Visit_At_Clause 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_At_Clause;
   
   ---------------------
   -- Visit_Attribute --
   ---------------------
   
   procedure Visit_Attribute 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Attribute;
   
   ---------------------------------------
   -- Visit_Attribute_Definition_Clause --
   ---------------------------------------
   
   procedure Visit_Attribute_Definition_Clause 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Attribute_Definition_Clause;
   
   ---------------------------
   -- Visit_Block_Statement --
   ---------------------------
   
   procedure Visit_Block_Statement 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Block_Statement;
   
   --------------------------
   -- Visit_Case_Statement --
   --------------------------
   
   procedure Visit_Case_Statement 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Case_Statement;
   
   -----------------------------
   -- Visit_Character_Literal --
   -----------------------------
   
   procedure Visit_Character_Literal 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Character_Literal;
   
   --------------------------
   -- Visit_Code_Statement --
   --------------------------
   
   procedure Visit_Code_Statement 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Code_Statement;
   
   ----------------------------
   -- Visit_Compilation_Unit --
   ----------------------------
   
   procedure Visit_Compilation_Unit 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Compilation_Unit;
   
   ---------------------------------
   -- Visit_Component_Declaration --
   ---------------------------------
   
   procedure Visit_Component_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Component_Declaration;
   
   ----------------------------------
   -- Visit_Conditional_Expression --
   ----------------------------------
   
   procedure Visit_Conditional_Expression 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Conditional_Expression;
   
   ---------------------------------------------
   -- Visit_Enumeration_Representation_Clause --
   ---------------------------------------------
   
   procedure Visit_Enumeration_Representation_Clause 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Enumeration_Representation_Clause;
   
   --------------------------
   -- Visit_Exit_Statement --
   --------------------------
   
   procedure Visit_Exit_Statement 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Exit_Statement;
   
   -------------------------
   -- Visit_Expanded_Name --
   -------------------------
   
   procedure Visit_Expanded_Name 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Expanded_Name;
   
   --------------------------------
   -- Visit_Explicit_Dereference --
   --------------------------------
   
   procedure Visit_Explicit_Dereference 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Explicit_Dereference;
   
   -------------------------------------
   -- Visit_Formal_Object_Declaration --
   -------------------------------------
   
   procedure Visit_Formal_Object_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Formal_Object_Declaration;
   
   --------------------------
   -- Visit_Formal_Package --
   --------------------------
   
   procedure Visit_Formal_Package 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Formal_Package;
   
   -----------------------------
   -- Visit_Formal_Subprogram --
   -----------------------------
   
   procedure Visit_Formal_Subprogram 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Formal_Subprogram;
   
   -----------------------------------
   -- Visit_Formal_Type_Declaration --
   -----------------------------------
   
   procedure Visit_Formal_Type_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Formal_Type_Declaration;
   
   --------------------------
   -- Visit_Free_Statement --
   --------------------------
   
   procedure Visit_Free_Statement 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Free_Statement;
   
   ----------------------------
   -- Visit_Type_Declaration --
   ----------------------------
   
   procedure Visit_Type_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Type_Declaration;
   
   -------------------------
   -- Visit_Function_Call --
   -------------------------
   
   procedure Visit_Function_Call 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Function_Call;
   
   ----------------------------------
   -- Visit_Function_Instantiation --
   ----------------------------------
   
   procedure Visit_Function_Instantiation 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Function_Instantiation;
   
   -------------------------------------
   -- Visit_Generic_Function_Renaming --
   -------------------------------------
   
   procedure Visit_Generic_Function_Renaming 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Generic_Function_Renaming;
   
   ---------------------------------------
   -- Visit_Generic_Package_Declaration --
   ---------------------------------------
   
   procedure Visit_Generic_Package_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Generic_Package_Declaration;
   
   ------------------------------------
   -- Visit_Generic_Package_Renaming --
   ------------------------------------
   
   procedure Visit_Generic_Package_Renaming 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Generic_Package_Renaming;
   
   --------------------------------------
   -- Visit_Generic_procedure_Renaming --
   --------------------------------------
   
   procedure Visit_Generic_procedure_Renaming 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Generic_procedure_Renaming;
   
   ------------------------------------------
   -- Visit_Generic_Subprogram_Declaration --
   ------------------------------------------
   
   procedure Visit_Generic_Subprogram_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Generic_Subprogram_Declaration;
   
   --------------------------
   -- Visit_Goto_Statement --
   --------------------------
   
   procedure Visit_Goto_Statement 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Goto_Statement;
   
   ------------------------------
   -- Visit_Handled_Statements --
   ------------------------------
   
   procedure Visit_Handled_Statements 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Handled_Statements;
   
   ----------------------
   -- Visit_Identifier --
   ----------------------
   
   procedure Visit_Identifier 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Identifier;
   
   ------------------------
   -- Visit_If_Statement --
   ------------------------
   
   procedure Visit_If_Statement 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_If_Statement;
   
   --------------------------------------
   -- Visit_Implicit_Label_Declaration --
   --------------------------------------
   
   procedure Visit_Implicit_Label_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Implicit_Label_Declaration;
   
   -------------------------
   -- Visit_Membership_Op --
   -------------------------
   
   procedure Visit_Membership_Op 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Membership_Op;
   
   --------------------------------
   -- Visit_Incomplete_Type_Decl --
   --------------------------------
   
   procedure Visit_Incomplete_Type_Decl 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Incomplete_Type_Decl;
   
   ----------------------------------
   -- Visit_Indexed_Component_Form --
   ----------------------------------
   
   procedure Visit_Indexed_Component_Form 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Indexed_Component_Form;
   
   ---------------------------
   -- Visit_Integer_Literal --
   ---------------------------
   
   procedure Visit_Integer_Literal 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Integer_Literal;
   
   -----------------
   -- Visit_Label --
   -----------------
   
   procedure Visit_Label 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Label;
   
   --------------------------
   -- Visit_Loop_Statement --
   --------------------------
   
   procedure Visit_Loop_Statement 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Loop_Statement;
   
   ----------------
   -- Visit_Null --
   ----------------
   
   procedure Visit_Null 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Null;
   
   --------------------------
   -- Visit_Null_Statement --
   --------------------------
   
   procedure Visit_Null_Statement 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Null_Statement;
   
   ------------------------------
   -- Visit_Number_Declaration --
   ------------------------------
   
   procedure Visit_Number_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Number_Declaration;
   
   ------------------------------
   -- Visit_Object_Declaration --
   ------------------------------
   
   procedure Visit_Object_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Object_Declaration;
   
   ---------------------------
   -- Visit_Object_Renaming --
   ---------------------------
   
   procedure Visit_Object_Renaming 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Object_Renaming;
   
   --------------------
   -- Visit_Unary_Op --
   --------------------
   
   procedure Visit_Unary_Op 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Unary_Op;
   
   -------------------------
   -- Visit_Arithmetic_Op --
   -------------------------
   
   procedure Visit_Arithmetic_Op 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Arithmetic_Op;
   
   ----------------------
   -- Visit_Logical_Op --
   ----------------------
   
   procedure Visit_Logical_Op 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Logical_Op;
   
   -------------------------
   -- Visit_Concatenation --
   -------------------------
   
   procedure Visit_Concatenation 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Concatenation;
   
   -----------------------
   -- Visit_Equality_Op --
   -----------------------
   
   procedure Visit_Equality_Op 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Equality_Op;
   
   -------------------------
   -- Visit_Comparison_Op --
   -------------------------
   
   procedure Visit_Comparison_Op 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Comparison_Op;
   
   --------------------
   -- Visit_Negation --
   --------------------
   
   procedure Visit_Negation 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Negation;
   
   -------------------------
   -- Visit_Others_Choice --
   -------------------------
   
   procedure Visit_Others_Choice 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Others_Choice;
   
   ------------------------
   -- Visit_Package_Body --
   ------------------------
   
   procedure Visit_Package_Body 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Package_Body;
   
   -------------------------------
   -- Visit_Package_Declaration --
   -------------------------------
   
   procedure Visit_Package_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Package_Declaration;
   
   ---------------------------------
   -- Visit_Package_Instantiation --
   ---------------------------------
   
   procedure Visit_Package_Instantiation 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Package_Instantiation;
   
   ----------------------------
   -- Visit_Package_Renaming --
   ----------------------------
   
   procedure Visit_Package_Renaming 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Package_Renaming;
   
   ---------------------------------
   -- Visit_Package_Specification --
   ---------------------------------
   
   procedure Visit_Package_Specification 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Package_Specification;
   
   ---------------------------------
   -- Visit_Parameter_Association --
   ---------------------------------
   
   procedure Visit_Parameter_Association 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Parameter_Association;
   
   ------------------
   -- Visit_Pragma --
   ------------------
   
   procedure Visit_Pragma 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Pragma;
   
   -----------------------------------------
   -- Visit_Private_Extension_Declaration --
   -----------------------------------------
   
   procedure Visit_Private_Extension_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Private_Extension_Declaration;
   
   ------------------------------------
   -- Visit_Private_Type_Declaration --
   ------------------------------------
   
   procedure Visit_Private_Type_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Private_Type_Declaration;
   
   --------------------------
   -- Visit_Procedure_Call --
   --------------------------
   
   procedure Visit_Procedure_Call 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Procedure_Call;
   
   -----------------------------------
   -- Visit_Procedure_Instantiation --
   -----------------------------------
   
   procedure Visit_Procedure_Instantiation 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Procedure_Instantiation;
   
   --------------------------------
   -- Visit_Qualified_Expression --
   --------------------------------
   
   procedure Visit_Qualified_Expression 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Qualified_Expression;
   
   -----------------
   -- Visit_Range --
   -----------------
   
   procedure Visit_Range 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Range;
   
   -------------------------
   -- Visit_Reactive_Type --
   -------------------------
   
   procedure Visit_Reactive_Type 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Reactive_Type;
   
   --------------------------
   -- Visit_Reactive_State --
   --------------------------
   
   procedure Visit_Reactive_State
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Reactive_State;
   
   -----------------------------------
   -- Visit_Reactive_Wait_Statement --
   -----------------------------------
   
   procedure Visit_Reactive_Wait_Statement 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Reactive_Wait_Statement;
   
   ------------------------------------
   -- Visit_Reactive_Pause_Statement --
   ------------------------------------
   
   procedure Visit_Reactive_Pause_Statement 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Reactive_Pause_Statement;
   
   -----------------------------------
   -- Visit_Reactive_Fork_Statement --
   -----------------------------------
   
   procedure Visit_Reactive_Fork_Statement 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Reactive_Fork_Statement;
   
   -------------------------------------
   -- Visit_Reactive_Fork_Alternative --
   -------------------------------------
   
   procedure Visit_Reactive_Fork_Alternative
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Reactive_Fork_Alternative;
   
   -------------------------------------
   -- Visit_Reactive_Select_Statement --
   -------------------------------------
   
   procedure Visit_Reactive_Select_Statement 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Reactive_Select_Statement;
   
   ---------------------------------------
   -- Visit_Reactive_Select_Alternative --
   ---------------------------------------
   
   procedure Visit_Reactive_Select_Alternative
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Reactive_Select_Alternative;
   
   ------------------------------------
   -- Visit_Reactive_Abort_Statement --
   ------------------------------------
   
   procedure Visit_Reactive_Abort_Statement
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Reactive_Abort_Statement;
   
   ----------------------------------
   -- Visit_Reactive_Abort_Handler --
   ----------------------------------
   
   procedure Visit_Reactive_Abort_Handler
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Reactive_Abort_Handler;
   
   ------------------------
   -- Visit_Real_Literal --
   ------------------------
   
   procedure Visit_Real_Literal 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Real_Literal;
   
   ----------------------------------------
   -- Visit_Record_Representation_Clause --
   ----------------------------------------
   
   procedure Visit_Record_Representation_Clause 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Record_Representation_Clause;
   
   ---------------------
   -- Visit_Reference --
   ---------------------
   
   procedure Visit_Reference 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Reference;
   
   ----------------------------
   -- Visit_Return_Statement --
   ----------------------------
   
   procedure Visit_Return_Statement 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Return_Statement;
   
   ------------------------------
   -- Visit_Selected_Component --
   ------------------------------
   
   procedure Visit_Selected_Component
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Selected_Component;
   
   -----------------
   -- Visit_Slice --
   -----------------
   
   procedure Visit_Slice 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Slice;
   
   --------------------------
   -- Visit_String_Literal --
   --------------------------
   
   procedure Visit_String_Literal 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_String_Literal;
   
   ---------------------------
   -- Visit_Subprogram_Body --
   ---------------------------
   
   procedure Visit_Subprogram_Body 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Subprogram_Body;
   
   ----------------------------------
   -- Visit_Subprogram_Declaration --
   ----------------------------------
   
   procedure Visit_Subprogram_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Subprogram_Declaration;
   
   ---------------------------
   -- Visit_Subprogram_Info --
   ---------------------------
   
   procedure Visit_Subprogram_Info 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Subprogram_Info;
   
   -------------------------------
   -- Visit_Subprogram_Renaming --
   -------------------------------
   
   procedure Visit_Subprogram_Renaming 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Subprogram_Renaming;
   
   -------------------------------
   -- Visit_Subtype_Declaration --
   -------------------------------
   
   procedure Visit_Subtype_Declaration 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Subtype_Declaration;
   
   ------------------------------
   -- Visit_Subtype_Indication --
   ------------------------------
   
   procedure Visit_Subtype_Indication 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Subtype_Indication;
   
   ---------------------------
   -- Visit_Type_Conversion --
   ---------------------------
   
   procedure Visit_Type_Conversion 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Type_Conversion;
   
   --------------------------------
   -- Visit_Unchecked_Expression --
   --------------------------------
   
   procedure Visit_Unchecked_Expression 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Unchecked_Expression;
   
   -------------------------------------
   -- Visit_Unchecked_Type_Conversion --
   -------------------------------------
   
   procedure Visit_Unchecked_Type_Conversion 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Unchecked_Type_Conversion;
   
   -----------------------
   -- Visit_Use_Package --
   -----------------------
   
   procedure Visit_Use_Package 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Use_Package;
   
   -----------------------------------------
   -- Visit_Validate_Unchecked_Conversion --
   -----------------------------------------
   
   procedure Visit_Validate_Unchecked_Conversion
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Validate_Unchecked_Conversion;
   
   -----------------------
   -- Visit_With_Clause --
   -----------------------
   
   procedure Visit_With_Clause 
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_With_Clause;
   
   -----------------------------
   -- Visit_Access_Definition --
   -----------------------------
   
   procedure Visit_Access_Definition
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Access_Definition;
   
   --------------------------------------
   -- Visit_Access_Function_Definition --
   --------------------------------------
   
   procedure Visit_Access_Function_Definition
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Access_Function_Definition;
   
   ---------------------------------------
   -- Visit_Access_Procedure_Definition --
   ---------------------------------------
   
   procedure Visit_Access_Procedure_Definition            
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Access_Procedure_Definition;
   
   ---------------------------------------
   -- Visit_Access_To_Object_Definition --
   ---------------------------------------
   
   procedure Visit_Access_To_Object_Definition
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Access_To_Object_Definition;
   
   --------------------------------------
   -- Visit_Case_Statement_Alternative --
   --------------------------------------
   
   procedure Visit_Case_Statement_Alternative
     (This : access  Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Case_Statement_Alternative;
   
   --------------------------------
   -- Visit_Compilation_Unit_Aux --
   --------------------------------
   
   procedure Visit_Compilation_Unit_Aux
     (This : access  Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Compilation_Unit_Aux;
   
   ---------------------------------
   -- Visit_Component_Association --
   ---------------------------------
   
   procedure Visit_Component_Association
     (This : access  Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Component_Association;
   
   ----------------------------
   -- Visit_Component_Clause --
   ----------------------------
   
   procedure Visit_Component_Clause
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Component_Clause;
   
   --------------------------------
   -- Visit_Component_Definition --
   --------------------------------
   
   procedure Visit_Component_Definition
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Component_Definition;
   
   --------------------------
   -- Visit_Component_List --
   --------------------------
   
   procedure Visit_Component_List
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Component_List;
   
   ----------------------------------------
   -- Visit_Constrained_Array_Definition --
   ----------------------------------------
   
   procedure Visit_Constrained_Array_Definition
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Constrained_Array_Definition;
   
   --------------------------------------
   -- Visit_Defining_Character_Literal --
   --------------------------------------
   
   procedure Visit_Defining_Character_Literal
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Defining_Character_Literal;

   -------------------------------
   -- Visit_Defining_Identifier --
   -------------------------------
   
   procedure Visit_Defining_Identifier
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Defining_Identifier;
   
   --------------------------------------
   -- Visit_Defining_Program_Unit_Name --
   --------------------------------------
   
   procedure Visit_Defining_Program_Unit_Name
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Defining_Program_Unit_Name;
   
   -----------------------------------
   -- Visit_Derived_Type_Definition --
   -----------------------------------
   
   procedure Visit_Derived_Type_Definition
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Derived_Type_Definition;
   
   ----------------------
   -- Visit_Designator --
   ----------------------
   
   procedure Visit_Designator
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Designator;
   
   -----------------------------
   -- Visit_Digits_Constraint --
   -----------------------------
   
   procedure Visit_Digits_Constraint
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Digits_Constraint;
   
   ------------------------------------
   -- Visit_Discriminant_Association --
   ------------------------------------
   
   procedure Visit_Discriminant_Association
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Discriminant_Association;
   
   ----------------------
   -- Visit_Elsif_Part --
   ----------------------
   
   procedure Visit_Elsif_Part
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Elsif_Part;
   
   ---------------------------------------
   -- Visit_Enumeration_Type_Definition --
   ---------------------------------------
   
   procedure Visit_Enumeration_Type_Definition
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Enumeration_Type_Definition;
   
   -------------------------------------
   -- Visit_Floating_Point_Definition --
   -------------------------------------
   
   procedure Visit_Floating_Point_Definition
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Floating_Point_Definition;
   
   ------------------------------------------
   -- Visit_Formal_Derived_Type_Definition --
   ------------------------------------------
   
   procedure Visit_Formal_Derived_Type_Definition
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Formal_Derived_Type_Definition;
   
   -------------------------------------------
   -- Visit_Formal_Discrete_Type_Definition --
   -------------------------------------------
   
   procedure Visit_Formal_Discrete_Type_Definition
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Formal_Discrete_Type_Definition;
   
   --------------------------------------------
   -- Visit_Formal_Floating_Point_Definition --
   --------------------------------------------
   
   procedure Visit_Formal_Floating_Point_Definition
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Formal_Floating_Point_Definition;
   
   ------------------------------------------
   -- Visit_Formal_Modular_Type_Definition --
   ------------------------------------------
   
   procedure Visit_Formal_Modular_Type_Definition
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Formal_Modular_Type_Definition;
   
   ------------------------------------------
   -- Visit_Formal_Private_Type_Definition --
   ------------------------------------------
   
   procedure Visit_Formal_Private_Type_Definition
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Formal_Private_Type_Definition;
   
   -------------------------------------------------
   -- Visit_Formal_Signed_Integer_Type_Definition --
   -------------------------------------------------
   
   procedure Visit_Formal_Signed_Integer_Type_Definition
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Formal_Signed_Integer_Type_Definition;
   
   ----------------------------------
   -- Visit_Function_Specification --
   ----------------------------------
   
   procedure Visit_Function_Specification
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Function_Specification;
   
   -------------------------------
   -- Visit_Generic_Association --
   -------------------------------
   
   procedure Visit_Generic_Association
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Generic_Association;
   
   --------------------------------------------
   -- Visit_Index_Or_Discriminant_Constraint --
   --------------------------------------------
   
   procedure Visit_Index_Or_Discriminant_Constraint
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Index_Or_Discriminant_Constraint;
   
   ----------------------------
   -- Visit_Iteration_Scheme --
   ----------------------------
   
   procedure Visit_Iteration_Scheme
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Iteration_Scheme;

   ----------------------------------------
   -- Visit_Loop_Parameter_Specification --
   ----------------------------------------
   
   procedure Visit_Loop_Parameter_Specification
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Loop_Parameter_Specification;
   
   ----------------------
   -- Visit_Mod_Clause --
   ----------------------
   
   procedure Visit_Mod_Clause
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Mod_Clause;
   
   -----------------------------------
   -- Visit_Modular_Type_Definition --
   -----------------------------------
   
   procedure Visit_Modular_Type_Definition
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Modular_Type_Definition;
   
   -----------------------------------
   -- Visit_Parameter_Specification --
   -----------------------------------
   
   procedure Visit_Parameter_Specification
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Parameter_Specification;
   
   ---------------------------------------
   -- Visit_Pragma_Argument_Association --
   ---------------------------------------
   
   procedure Visit_Pragma_Argument_Association
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Pragma_Argument_Association;
   
   -----------------------------------
   -- Visit_Procedure_Specification --
   -----------------------------------
   
   procedure Visit_Procedure_Specification
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Procedure_Specification;
   
   ------------------------------------
   -- Visit_Real_Range_Specification -- 
   ------------------------------------
   
   procedure Visit_Real_Range_Specification
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Real_Range_Specification;
   
   -----------------------------
   -- Visit_Record_Definition --
   -----------------------------
   
   procedure Visit_Record_Definition
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Record_Definition;
   
   ------------------------------------------
   -- Visit_Signed_Integer_Type_Definition --
   ------------------------------------------
   
   procedure Visit_Signed_Integer_Type_Definition
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Signed_Integer_Type_Definition;
   
   ------------------------------------------
   -- Visit_Unconstrained_Array_Definition --
   ------------------------------------------
   
   procedure Visit_Unconstrained_Array_Definition
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Unconstrained_Array_Definition;
   
   ---------------------------
   -- Visit_Unused_At_Start --
   ---------------------------
   
   procedure Visit_Unused_At_Start
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Unused_At_Start;

   -------------------------
   -- Visit_Unused_At_End --
   -------------------------
   
   procedure Visit_Unused_At_End
     (This : access Visitor_Record;
      N    : Node_Id) is
   begin
      null;
   end Visit_Unused_At_End;
   
end Reflex.Visitor;
