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

package Reflex.Expanders.Ch3 is

   procedure Expand_Type_Declaration
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Abstract_Subprogram_Declaration
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Access_Definition
     (This      : access Reflex_Expander_Record;
      Node      : Node_Id; 
      Decl_Node : Node_Id);

   procedure Expand_Access_To_Object_Definition
     (This      : access Reflex_Expander_Record;
      Node      : Node_Id;
      Decl_Node : Node_Id);
   
   --  procedure Expand_Access_Function_Definition
   --   (This : access Reflex_Expander_Record;
   --    Node : Node_Id);
   
   --  procedure Expand_Access_Procedure_Definition
   --   (This : access Reflex_Expander_Record;
   --    Node : Node_Id);

   -- procedure Expand_Component_Association
   --   (This : access Reflex_Expander_Record;
   --    Node : Node_Id);

   procedure Expand_Component_Definition
     (This      : access Reflex_Expander_Record;
      Node      : Node_Id;
      Decl_Node : Node_Id);

   procedure Expand_Component_Declaration
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Component_List
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   --  procedure Expand_Delta_Constraint
   --   (This : access Reflex_Expander_Record;
   --    Node : Node_Id);

   procedure Expand_Derived_Type_Definition
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Subtype_Mark
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Range_Constraint
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Range
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Index_Or_Discriminant_Constraint
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Discriminant_Association
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Discriminant_Specification
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Enumeration_Type_Definition
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   --  The enumeration type is translate as integer in Glips. The literals are
   --  replaced by their values, unless the option Expand_Enumeration_Literal
   --  is set and then the literals are translated as integer constant which 
   --  value is the representation value of the literal
   
   procedure Expand_Extension_Aggregate
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Full_Type_Declaration
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Function_Instantiation
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Function_Specification
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Implicit_Label_Declaration
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Incomplete_Type_Declaration
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Modular_Type_Definition
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Floating_Point_Definition 
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Number_Declaration
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Object_Declaration
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Object_Renaming_Declaration
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Ordinary_Fixed_Point_Definition
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Package_Instantiation
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   --  procedure Expand_Package_Renaming_Declaration
   --   (This : access Reflex_Expander_Record;
   --    Node : Node_Id);

   procedure Expand_Private_Extension_Declaration
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   --  procedure Expand_Private_Type_Declaration
   --   (This : access Reflex_Expander_Record;
   --    Node : Node_Id);

   --  procedure Expand_Procedure_Instantiation
   --   (This : access Reflex_Expander_Record;
   --    Node : Node_Id);

   procedure Expand_Procedure_Specification
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Real_Range_Specification
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Constrained_Array_Declaration
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Unconstrained_Array_Declaration
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Unconstrained_Array_Definition
     (This      : access Reflex_Expander_Record;
      Node      : Node_Id;
      Decl_Node : Node_Id);
   
   procedure Expand_Constrained_Array_Definition
     (This      : access Reflex_Expander_Record;
      Node      : Node_Id;
      Decl_Node : Node_Id);
   
   procedure Expand_Record_Definition
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Enumeration_Literal_Constants
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
      
   procedure Expand_Signed_Integer_Type_Definition
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   --  They are emit as integer or unsigned if the type is a modular type. So
   --  nothing to do with discrete type as there are declared as integer_x or
   --  unisgned_x for modular types, with x the size in bits of the type
   --  (8, 16, 32, 64)

   --  procedure Expand_Subprogram_Renaming_Declaration
   --   (This : access Reflex_Expander_Record;
   --    Node : Node_Id);

   procedure Expand_Subtype_Declaration
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Subtype_Indication
     (This      : access Reflex_Expander_Record;
      Node      : Node_Id;
      Decl_Node : Node_Id);

   --  procedure Expand_Variant
   --   (This : access Reflex_Expander_Record;
   --    Node : Node_Id);
   
   --  procedure Expand_Variant_Part
   --   (This : access Reflex_Expander_Record;
   --   Node : Node_Id);
   
   procedure Subtype_Indication_As_Anonymous_Array 
     (This      : access Reflex_Expander_Record;
      Subtyp    : Node_Id;
      Decl_Node : Node_Id);
   
   procedure Subtype_Indication_As_String 
     (This : access Reflex_Expander_Record;
      Typ  : Entity_Id);
   
end Reflex.Expanders.Ch3;
