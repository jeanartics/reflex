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

package Rxada.Gen.Ch12 is
   
   procedure Generate_Generic_Subprogram_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Generic_Package_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Generic_Formal_Part
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Generic_Formal_Parameter_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Generic_Instantiation
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Package_Instantiation
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Procedure_Instantiation
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Function_Instantiation
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Generic_Association
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Explicit_Generic_Actual_Parameter
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Formal_Object_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Formal_Type_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Formal_Type_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Formal_Private_Type_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Formal_Derived_Type_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Formal_Discrete_Type_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Formal_Signed_Integer_Type_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Formal_Modular_Type_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Formal_Array_Type_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Formal_Access_Type_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Formal_Subprogram_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Subprogram_Default
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Formal_Package_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Formal_Package_Actual_Part
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
end Rxada.Gen.Ch12;
