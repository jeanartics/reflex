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

with Types;  use Types;

package Rxada.Gen.Ch3 is

   procedure Generate_Type_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Abstract_Subprogram_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   procedure Generate_Access_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   procedure Generate_Access_To_Object_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Reactive_Type_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   --  procedure Generate_Access_Function_Definition
   --   (This : access Ada_Generator_Record;
   --    Node : Node_Id);
   
   --  procedure Generate_Access_Procedure_Definition
   --   (This : access Ada_Generator_Record;
   --    Node : Node_Id);

   -- procedure Generate_Component_Association
   --   (This : access Ada_Generator_Record;
   --    Node : Node_Id);

   procedure Generate_Component_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   procedure Generate_Component_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   procedure Generate_Component_List
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   --  procedure Generate_Delta_Constraint
   --   (This : access Ada_Generator_Record;
   --    Node : Node_Id);

   procedure Generate_Derived_Type_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   procedure Generate_Subtype_Mark
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Range_Constraint
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Range
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Index_Or_Discriminant_Constraint
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Discriminant_Association
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Discriminant_Specification
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Enumeration_Type_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Full_Type_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Function_Specification
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Implicit_Label_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Incomplete_Type_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Modular_Type_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   procedure Generate_Floating_Point_Definition 
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Number_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Object_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Object_Renaming_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Ordinary_Fixed_Point_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   --  procedure Generate_Package_Renaming_Declaration
   --   (This : access Ada_Generator_Record;
   --    Node : Node_Id);

   procedure Generate_Private_Extension_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Private_Type_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   --  procedure Generate_Procedure_Instantiation
   --   (This : access Ada_Generator_Record;
   --    Node : Node_Id);

   procedure Generate_Procedure_Specification
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   procedure Generate_Real_Range_Specification
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Constrained_Array_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Unconstrained_Array_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Unconstrained_Array_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Constrained_Array_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Record_Definition 
     (This : access Ada_Generator_Record;
      Node : Node_Id);
--     procedure Generate_Record_Declaration
--       (This : access Ada_Generator_Record;
--        Node : Node_Id);

   procedure Generate_Enumeration_Literal_Constants
     (This : access Ada_Generator_Record;
      Node : Node_Id);
      
   procedure Generate_Signed_Integer_Type_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   --  procedure Generate_Subprogram_Renaming_Declaration
   --   (This : access Ada_Generator_Record;
   --    Node : Node_Id);

   procedure Generate_Subtype_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   procedure Generate_Subtype_Indication
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   --  procedure Generate_Variant
   --   (This : access Ada_Generator_Record;
   --    Node : Node_Id);
   
   --  procedure Generate_Variant_Part
   --   (This : access Ada_Generator_Record;
   --   Node : Node_Id);
   
   procedure Subtype_Indication_As_Anonymous_Array 
     (This   : access Ada_Generator_Record;
      Subtyp : Node_Id);
   
   procedure Subtype_Indication_As_String 
     (This : access Ada_Generator_Record;
      Typ  : Entity_Id);
   
end Rxada.Gen.Ch3;
