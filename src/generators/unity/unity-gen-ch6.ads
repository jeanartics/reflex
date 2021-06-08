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

package Unity.Gen.Ch6 is

   --  Ch3 procedure Generate_Abstract_Subprogram_Declaration 
   --    (This : access Unity_Generator_Record;
   --     Node : Node_Id);

   procedure Generate_Body_Stub 
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Defining_Operator_Symbol 
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Defining_Program_Unit_Name 
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   --  procedure Generate_Designator 
   --   (This : access Unity_Generator_Record;
   --    Node : Node_Id);

   --  procedure Generate_Extended_Return_Statement 
   --   (This : access Unity_Generator_Record;
   --    Node : Node_Id);

   --  Ch3 procedure Generate_Function_Instantiation 
   --    (This : access Unity_Generator_Record;
   --     Node : Node_Id);

   --  ??? procedure Generate_Function_Specification 
   --   (This : access Unity_Generator_Record;
   --    Node : Node_Id);

   --  procedure Generate_Operator_Symbol 
   --   (This : access Unity_Generator_Record;
   --    Node : Node_Id);

   --  Ch3 procedure Generate_Parameter_Association 
   --    (This : access Unity_Generator_Record;
   --     Node : Node_Id);

   procedure Generate_Parameter_Specification 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Procedure_Instantiation 
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Subprogram_Specification
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   --  ??? procedure Generate_Procedure_Specification 
   --    (This : access Unity_Generator_Record;
   --     Node : Node_Id);

   --  Ch5 procedure Generate_Simple_Return_Statement 
   --    (This : access Unity_Generator_Record;
   --     Node : Node_Id);

   procedure Generate_Subprogram_Body 
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Subprogram_Declaration 
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Subprogram_Renaming_Declaration 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   --  Ch3 procedure Generate_Subtype_Declaration 
   --    (This : access Unity_Generator_Record;
   --     Node : Node_Id);
   
   procedure Generate_Subunit 
     (This : access Unity_Generator_Record;
      Node : Node_Id);
    
  procedure Declare_Subprogram_Instance
     (This   : access Unity_generator_Record;
      Def_Id : Entity_Id;
      Inst   : Entity_Id);
   
end Unity.Gen.Ch6;
