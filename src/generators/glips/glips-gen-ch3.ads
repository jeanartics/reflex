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

package Glips.Gen.Ch3 is

   procedure Generate_Type_Declaration
     (This : access Glips_Generator_Record;
      Node : Node_Id);
   --  Emit the type declaration. This procedure dispatch to the right type
   --  declaration regarding the kind of Type declaration. 
   --  TYPE_DECLARATION ::=
   --    FULL_TYPE_DECLARATION
   --  | INCOMPLETE_TYPE_DECLARATION
   --  | PRIVATE_TYPE_DECLARATION
   --  | PRIVATE_EXTENSION_DECLARATION
   
   procedure Generate_Full_Type_Declaration 
     (This : access Glips_Generator_Record;
      Node : Node_Id);
   --  This procedure dispatch to the right type declaration regarding the
   --  its Type Definition
   --  TYPE_DEFINITION ::=
   --    ENUMERATION_TYPE_DEFINITION  | INTEGER_TYPE_DEFINITION
   --  | REAL_TYPE_DEFINITION         | ARRAY_TYPE_DEFINITION
   --  | RECORD_TYPE_DEFINITION       | ACCESS_TYPE_DEFINITION
   --  | DERIVED_TYPE_DEFINITION      | INTERFACE_TYPE_DEFINITION
   
   procedure Generate_Access_Definition
     (This : access Glips_Generator_Record;
      Node : Node_Id);

   procedure Generate_Access_To_Object_Definition
     (This : access Glips_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Component_Definition
     (This : access Glips_Generator_Record;
      Node : Node_Id);

   procedure Generate_Component_Declaration
     (This : access Glips_Generator_Record;
      Node : Node_Id);

   procedure Generate_Component_List
     (This : access Glips_Generator_Record;
      Node : Node_Id);

   procedure Generate_Derived_Type_Definition
     (This : access Glips_Generator_Record;
      Node : Node_Id);

   procedure Generate_Subtype_Mark
     (This : access Glips_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Range_Constraint
     (This : access Glips_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Range
     (This : access Glips_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Enumeration_Type_Definition
     (This : access Glips_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Incomplete_Type_Declaration
     (This : access Glips_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Modular_Type_Definition
     (This : access Glips_Generator_Record;
      Node : Node_Id);

   procedure Generate_Floating_Point_Definition 
     (This : access Glips_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Number_Declaration
     (This : access Glips_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Object_Declaration
     (This : access Glips_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Object_Renaming_Declaration
     (This : access Glips_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Real_Range_Specification
     (This : access Glips_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Constrained_Array_Declaration
     (This : access Glips_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Unconstrained_Array_Declaration
     (This : access Glips_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Unconstrained_Array_Definition
     (This : access Glips_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Constrained_Array_Definition
     (This : access Glips_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Record_Definition 
     (This : access Glips_Generator_Record;
      Node : Node_Id);

   procedure Generate_Enumeration_Literal_Constants
     (This : access Glips_Generator_Record;
      Node : Node_Id);
      
   procedure Generate_Signed_Integer_Type_Definition
     (This : access Glips_Generator_Record;
      Node : Node_Id);

   procedure Generate_Subtype_Declaration
     (This : access Glips_Generator_Record;
      Node : Node_Id);

   procedure Generate_Subtype_Indication
     (This : access Glips_Generator_Record;
      Node : Node_Id);

   procedure Subtype_Indication_As_Anonymous_Array 
     (This   : access Glips_Generator_Record;
      Subtyp : Node_Id);
   
   procedure Subtype_Indication_As_String 
     (This : access Glips_Generator_Record;
      Typ  : Entity_Id);
   
   procedure Handle_Anonymous_Type
     (This       : access Glips_Generator_Record;
      E          : Node_Id;
      From_Array : Boolean := False);
   
end Glips.Gen.Ch3;
