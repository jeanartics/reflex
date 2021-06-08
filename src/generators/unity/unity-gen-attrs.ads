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

with Types; use Types;

package Unity.Gen.Attrs is

   procedure Generate_Attribute
     (This : access Unity_Generator_Record;
      Node : Node_id);

   procedure Generate_Attribute_Access 
     (This : access Unity_Generator_Record;
      Node : Node_Id);
      
   procedure Generate_Attribute_Address 
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Attribute_Address_Size 
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Attribute_Alignment 
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Attribute_Base 
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Attribute_Bit 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Bit_Order 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Bit_Position 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Body_Version 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Class 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Code_Address 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Compiler_Version 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Component_Size 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Constrained 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Copy_Sign 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Default_Bit_Order 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Default_Scalar_Storage_Order 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Deref 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Elaboration
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Elaborated 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Enabled 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Enum_Rep 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Enum_Val 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Exponent 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_First 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_First_Bit 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_First_Valid 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Floor 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Fraction 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Has_Access_Values 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Has_Same_Storage 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Has_Tagged_Values 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Image 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Img 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Invalid_Value 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Last 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Last_Bit 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Leading_Part 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Length 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Machine_Size 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Mantissa 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Max 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Max_Size_In_Storage_Elements 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Max_Alignment_For_Allocation 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Maximum_Alignment 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Mechanism_Code 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Min 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Mod 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Modulus 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Null_Parameter 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Object_Size 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Old 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Overlaps_Storage 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Passed_By_Reference 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Pool_Address 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Pos 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Position 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Pred 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Range 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Ref 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Remainder 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Round 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Rounding 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Scalar_Storage_Order 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Size
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Storage_Pool      
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Storage_Size 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Storage_Unit 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Succ 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_System_Allocator_Alignment 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Tag 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Target_Name 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Terminated 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_To_Address 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_To_Any 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Truncation 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Type_Class 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_TypeCode 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Type_Key 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Unchecked_Access 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Unconstrained_Array 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Universal_Literal_String 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Unrestricted_Access 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Val 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Value 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Value_Size 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Version 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Wchar_T_Size 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Wide_Image 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Wide_Wide_Image 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Wide_Value 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Wide_Wide_Value 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Wide_Wide_Width 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Wide_Width 
     (This : access Unity_Generator_Record;
      Node : Node_Id);

   procedure Generate_Attribute_Word_Size 
     (This : access Unity_Generator_Record;
      Node : Node_Id);
   
end Unity.Gen.Attrs;
