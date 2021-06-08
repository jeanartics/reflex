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

package Reflex.Expanders.Ch6 is

   --  Ch3 procedure Expand_Abstract_Subprogram_Declaration
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   procedure Expand_Body_Stub
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Defining_Operator_Symbol
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Defining_Program_Unit_Name
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   --  procedure Expand_Designator
   --   (This : access Reflex_Expander_Record;
   --    Node : Node_Id);

   --  procedure Expand_Extended_Return_Statement
   --   (This : access Reflex_Expander_Record;
   --    Node : Node_Id);

   --  Ch3 procedure Expand_Function_Instantiation
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   --  ??? procedure Expand_Function_Specification
   --   (This : access Reflex_Expander_Record;
   --    Node : Node_Id);

   --  procedure Expand_Operator_Symbol
   --   (This : access Reflex_Expander_Record;
   --    Node : Node_Id);

   --  Ch3 procedure Expand_Parameter_Association
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   procedure Expand_Parameter_Specification
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Procedure_Instantiation
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Subprogram_Specification
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   --  ??? procedure Expand_Procedure_Specification
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   --  Ch5 procedure Expand_Simple_Return_Statement
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   procedure Expand_Subprogram_Body
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Subprogram_Declaration
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Subprogram_Renaming_Declaration
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Subprogram_As_Sr
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   --  Ch3 procedure Expand_Subtype_Declaration
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   procedure Expand_Subunit
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Replace_Returns
     (Subp      : Node_Id;
      Param_Id  : Entity_Id;
      Goto_Form : Boolean := False);

   procedure Change_Function_To_Procedure
     (Subp      : Node_Id;
      Goto_Form : Boolean := False);

   procedure Expand_Inlined_Call
    (N         : Node_Id;
     Subp      : Entity_Id;
     Orig_Subp : Entity_Id);

end Reflex.Expanders.Ch6;
