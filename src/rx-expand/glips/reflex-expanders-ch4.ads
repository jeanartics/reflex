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

package Reflex.Expanders.Ch4 is

   procedure Handle_Attribute (N : Node_Id);

   procedure Expand_Aggregate
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_And_Then
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Attribute_Reference
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   --  procedure Expand_Case_Expression
   --   (This : access Reflex_Expander_Record;
   --    Node : Node_Id);

   --  procedure Expand_Case_Expression_Alternative
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   procedure Expand_Component_Association
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   --  Ch3 procedure Expand_Discriminant_Association
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   procedure Expand_Explicit_Dereference
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Expression_With_Actions
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Expression_Function
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Extended_Return_Statement
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   --  procedure Expand_Extension_Aggregate
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   procedure Expand_Function_Call
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_If_Expression
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_In
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Indexed_Component
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Integer_Literal
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   --  procedure Expand_Mod_Clause
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   procedure Expand_Not_In
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Op_Abs
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Op_Add
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Op_And
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   --  procedure Expand_Op_Concat
   --   (This : access Reflex_Expander_Record;
   --    Node : Node_Id);

   procedure Expand_Op_Divide
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Op_Eq
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Op_Expon
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Op_Ge
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Op_Gt
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Op_Le
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Op_Lt
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Op_Minus
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Op_Mod
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Op_Multiply
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Op_Ne
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Op_Not
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Op_Or
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Op_Plus
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Op_Rem
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   --  procedure Expand_Op_Rotate_Left
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   --  procedure Expand_Op_Rotate_Right
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   procedure Expand_Op_Shift_Right
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   --  procedure Expand_Op_Shift_Right_Arithmetic
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   procedure Expand_Op_Shift_Left
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Op_Subtract
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Op_Xor
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   --  procedure Expand_Operator_Symbol
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   procedure Expand_Or_Else
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   --  procedure Expand_Others_Choice
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   --  Ch3 procedure Expand_Parameter_Association
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   procedure Expand_Qualified_Expression
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   --  procedure Expand_Quantified_Expression
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   --  procedure Expand_Range
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   procedure Expand_Real_Literal
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   --  procedure Expand_Real_Range_Specification
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   procedure Expand_Reference
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Slice
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_String_Literal
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Type_Conversion
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   --  procedure Expand_Unchecked_Expression
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   procedure Expand_Unchecked_Type_Conversion
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Short_Circuit_Operator
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   --  Common expansion processing for short-circuit boolean operators

   procedure Remove_Function_Call_Effect (Call : Node_Id);

end Reflex.Expanders.Ch4;
