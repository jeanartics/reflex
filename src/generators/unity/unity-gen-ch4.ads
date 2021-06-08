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

package Unity.Gen.Ch4 is
   
   procedure Handle_Attribute (N : Node_Id);
   
   procedure Generate_Aggregate
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_And_Then
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Reference
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   --  procedure Generate_Case_Expression
   --   (This : access Unity_Generator_Record;
   --    Node : Node_Id);

   --  procedure Generate_Case_Expression_Alternative
   --    (This : access Unity_Generator_Record;
   --     Node : Node_Id);

   procedure Generate_Component_Association
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   --  Ch3 procedure Generate_Discriminant_Association
   --    (This : access Unity_Generator_Record;
   --     Node : Node_Id);

   procedure Generate_Explicit_Dereference
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Expression_With_Actions
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Expression_Function
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Extended_Return_Statement
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   --  procedure Generate_Extension_Aggregate
   --    (This : access Unity_Generator_Record;
   --     Node : Node_Id);

   procedure Generate_Function_Call
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_If_Expression
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_In
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Indexed_Component
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Integer_Literal
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   --  procedure Generate_Mod_Clause
   --    (This : access Unity_Generator_Record;
   --     Node : Node_Id);

   procedure Generate_Not_In
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Op_Abs
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Op_Add
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Op_And
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   --  procedure Generate_Op_Concat
   --   (This : access Unity_Generator_Record;
   --    Node : Node_Id);

   procedure Generate_Op_Divide
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Op_Eq
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Op_Expon
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Op_Ge
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Op_Gt
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Op_Le
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Op_Lt
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Op_Minus
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Op_Mod
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Op_Multiply
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Op_Ne
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Op_Not
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Op_Or
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Op_Plus
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Op_Rem
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   --  procedure Generate_Op_Rotate_Left
   --    (This : access Unity_Generator_Record;
   --     Node : Node_Id);
   
   --  procedure Generate_Op_Rotate_Right
   --    (This : access Unity_Generator_Record;
   --     Node : Node_Id);

   procedure Generate_Op_Shift_Right
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   --  procedure Generate_Op_Shift_Right_Arithmetic
   --    (This : access Unity_Generator_Record;
   --     Node : Node_Id);

   procedure Generate_Op_Shift_Left
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Op_Subtract
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Op_Xor
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   --  procedure Generate_Operator_Symbol
   --    (This : access Unity_Generator_Record;
   --     Node : Node_Id);

   procedure Generate_Or_Else
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   --  procedure Generate_Others_Choice
   --    (This : access Unity_Generator_Record;
   --     Node : Node_Id);

   --  Ch3 procedure Generate_Parameter_Association
   --    (This : access Unity_Generator_Record;
   --     Node : Node_Id);

   procedure Generate_Qualified_Expression
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   --  procedure Generate_Quantified_Expression
   --    (This : access Unity_Generator_Record;
   --     Node : Node_Id);

   --  procedure Generate_Range
   --    (This : access Unity_Generator_Record;
   --     Node : Node_Id);

   procedure Generate_Real_Literal
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   --  procedure Generate_Real_Range_Specification
   --    (This : access Unity_Generator_Record;
   --     Node : Node_Id);

   procedure Generate_Reference
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Slice
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_String_Literal
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Type_Conversion
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   --  procedure Generate_Unchecked_Expression
   --    (This : access Unity_Generator_Record;
   --     Node : Node_Id);

   procedure Generate_Unchecked_Type_Conversion
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Record_Aggregate_Values
     (This : access Unity_Generator_Record;
      N    : Node_Id);
   
   procedure Generate_Record_Aggregate_In_Declaration
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Record_Component_Association_In_Declaration
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
end Unity.Gen.Ch4;
