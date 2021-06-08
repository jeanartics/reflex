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

package Reflex.Expanders.Ch5 is

   procedure Expand_Sequence_Of_Statements
     (This  : access Reflex_Expander_Record;
      Stmts : List_Id);
   --  Expand Statements of list Stmts

   procedure Expand_Assignment_Statement
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Block_Statement
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Case_Statement
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Case_Statement_Alternative
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Code_Statement
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Compound_Statement
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Elsif_Part
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Exit_Statement
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Free_Statement
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   --  Ch4 procedure Expand_Function_Call
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   procedure Expand_Goto_Statement
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Handled_Sequence_Of_Statements
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_If_Statement
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   --  procedure Expand_Iteration_Scheme
   --   (This : access Reflex_Expander_Record;
   --    Node : Node_Id);

   procedure Expand_Iterator_Specification
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Label
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   --  procedure Expand_Loop_Parameter_Specification
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   procedure Expand_Loop_Statement
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Null_Statement
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   --  Ch3 procedure Expand_Parameter_Association
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   procedure Expand_Procedure_Call_Statement
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Raise_Expression
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Raise_Error
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Return_Statement
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Rewrite_Elsif_Side_Effect (Node : Node_Id);

end Reflex.Expanders.Ch5;
