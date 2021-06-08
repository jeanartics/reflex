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

with ada.text_io; use ada.text_io;

with Types; use Types;
with Atree; use Atree;
with Namet; use Namet;
with Stand; use Stand;
with Sinfo; use Sinfo;
with Snames; use Snames;
with Einfo; use Einfo;
with Sem_Util; use Sem_Util;
with Sem_Aux; use Sem_Aux;
with Uintp; use Uintp;
with Urealp; use Urealp;
with Errout; use Errout;

with Reflex.Infos; use Reflex.Infos;
with Reflex.Gen.Outputs; use Reflex.Gen.Outputs;
with Reflex.Gen.Utils; use Reflex.Gen.Utils;
with Glips.Gen.Dispatch; use Glips.Gen.Dispatch;

package body Glips.Gen.Attrs is
   
   ------------------------
   -- Generate_Attribute --
   ------------------------
   
   procedure Generate_Attribute
     (This : access Glips_Generator_Record;
      Node : Node_id) is
      
      Aname   : constant Name_Id      := Attribute_Name (Node);
      Attr_Id : constant Attribute_Id := Get_Attribute_Id (Aname);
   begin
      case Attr_Id is

	 --  Attributes related to Ada 2012 iterators. Attribute specifications
	 --  exist for these, but they cannot be queried.
	 
	    --  Internal attributes used to deal with Ada 2012 delayed aspects.
	    --  These were already rejected by the parser. Thus they shouldn't 
	    --  appear here.
	    
--  	 when Internal_Attribute_Id =>
--  	    null;
	    
	    ------------------
	    -- Abort_Signal --
	    ------------------

	 when Attribute_Abort_Signal
	   | Attribute_Adjacent
	   | Attribute_Aft
	   | Attribute_Alignment
	   | Attribute_Asm_Input
	   | Attribute_Callable
	   | Attribute_Caller
	   | Attribute_Ceiling
	   | Attribute_Class
	   | Attribute_Code_Address
	   | Attribute_Compose
	   | Attribute_Count
	   | Attribute_Definite
	   | Attribute_Delta
	   | Attribute_Denorm
	   | Attribute_Digits
	   | Attribute_Emax
	   | Attribute_Epsilon
	   | Attribute_External_Tag
	   | Attribute_Fixed_Value
	   | Attribute_Fore
	   | Attribute_Has_Discriminants
	   | Attribute_Identity
	   | Attribute_Input
	   | Attribute_Integer_Value
	   | Attribute_Large
	   | Attribute_Machine
	   | Attribute_Machine_Emax
	   | Attribute_Machine_Emin
	   | Attribute_Machine_Mantissa
	   | Attribute_Machine_Overflows
	   | Attribute_Machine_Radix
	   | Attribute_Machine_Rounds
	   | Attribute_Model
	   | Attribute_Model_Emin
	   | Attribute_Model_Epsilon
	   | Attribute_Model_Mantissa
	   | Attribute_Model_Small
	   | Attribute_Output
	   | Attribute_Partition_ID
	   | Attribute_Range_Length
	   | Attribute_Read
	   | Attribute_Safe_Emax
	   | Attribute_Safe_First
	   | Attribute_Safe_Large
	   | Attribute_Safe_Last
	   | Attribute_Safe_Small
	   | Attribute_Scale
	   | Attribute_Scaling
	   | Attribute_Signed_Zeros
	   | Attribute_Small
	   | Attribute_Unbiased_Rounding
	   | Attribute_Uet_Address
	   | Attribute_Valid
	   | Attribute_Width
	   | Attribute_Write =>
	    Error_Msg_N ("attribute not supported by reflex", Node);
	    
	    
	    ------------
	    -- Access --
	    ------------
	    
	 when Attribute_Access =>
	    Generate_Attribute_Access (This, Node);
	    
	    -------------
	    -- Address --
	    -------------
	    
	 when Attribute_Address =>
	    Generate_Attribute_Address (This, Node);
	    
	    ------------------
	    -- Address_Size --
	    ------------------
	    
	 when Attribute_Address_Size =>
	    Generate_Attribute_Address_Size (This, Node);
	    
	    ----------
	    -- Base --
	    ----------
	    
	 when Attribute_Base =>
	    Generate_Attribute_Base (This, Node);
	    
	    ---------
	    -- Bit --
	    ---------
	    
	 when Attribute_Bit =>
	    Generate_Attribute_Bit (This, Node);

	    ---------------
	    -- Bit_Order --
	    ---------------

	 when Attribute_Bit_Order =>
	    Generate_Attribute_Bit_Order (This, Node);

	    ------------------
	    -- Bit_Position --
	    ------------------

	 when Attribute_Bit_Position =>
	    Generate_Attribute_Bit_Position (This, Node);

	    ------------------
	    -- Body_Version --
	    ------------------

	 when Attribute_Body_Version =>
	    Generate_Attribute_Body_Version (This, Node);

	    --------------------
	    -- Component_Size --
	    --------------------

	 when Attribute_Component_Size =>
	    Generate_Attribute_Component_Size (This, Node);

	    -----------------
	    -- Constrained --
	    -----------------

	 when Attribute_Constrained =>
	    Generate_Attribute_Constrained (This, Node);

	    ---------------
	    -- Copy_Sign --
	    ---------------

	 when Attribute_Copy_Sign =>
	    Generate_Attribute_Copy_Sign (THis, Node);

	    -----------------------
	    -- Default_Bit_Order --
	    -----------------------

	 when Attribute_Default_Bit_Order => 
	    Generate_Attribute_Default_Bit_Order (This, Node);

	    ---------------
	    -- Elab_Body --
	    ---------------

	    --  Also handles processing for Elab_Spec and Elab_Subp_Body

	 when Attribute_Elab_Body      |
           Attribute_Elab_Spec      =>
	    Generate_Attribute_Elaboration (This, Node);


	    ---------------
	    -- Elab_Spec --
	    ---------------

	    --  Shares processing with Elab_Body

	    ----------------
	    -- Elaborated --
	    ----------------

	 when Attribute_Elaborated =>
	    Generate_Attribute_Elaborated (This, Node);

	    --------------
	    -- Enum_Rep --
	    --------------

	 when Attribute_Enum_Rep => 
	    Generate_Attribute_Enum_Rep (THis, Node);

	    --------------
	    -- Enum_Val --
	    --------------

--  	 when Attribute_Enum_Val => 
--  	    Generate_Attribute_Enum_Val (This, Node);

	    --------------
	    -- Exponent --
	    --------------

	 when Attribute_Exponent =>
	    Generate_Attribute_Exponent (This, Node);

	    -----------
	    -- First --
	    -----------

	 when Attribute_First =>
	    Generate_Attribute_First (This, Node);

	    ---------------
	    -- First_Bit --
	    ---------------

	 when Attribute_First_Bit =>
	    Generate_Attribute_First_Bit (This, Node);

	    -----------
	    -- Floor --
	    -----------

	 when Attribute_Floor =>
	    Generate_Attribute_Floor (This, Node);

	    --------------
	    -- Fraction --
	    --------------

	 when Attribute_Fraction =>
	    Generate_Attribute_Fraction (This, Node);

	    -----------
	    -- Image --
	    -----------

	 when Attribute_Image => 
	    Generate_Attribute_Image (This, Node);

	    ---------
	    -- Img --
	    ---------

	 when Attribute_Img => 
	    Generate_Attribute_Img (This, Node);

	    -------------------
	    -- Invalid_Value --
	    -------------------

	    ----------
	    -- Last --
	    ----------

	 when Attribute_Last =>
	    Generate_Attribute_Last (This, Node);

	    --------------
	    -- Last_Bit --
	    --------------

	 when Attribute_Last_Bit =>
	    Generate_Attribute_Last_Bit (This, Node);

	    ------------------
	    -- Leading_Part --
	    ------------------

	 when Attribute_Leading_Part =>
	    Generate_Attribute_Leading_Part (This, Node);

	    ------------
	    -- Length --
	    ------------

	 when Attribute_Length =>
	    Generate_Attribute_Length (This, Node);

	    ------------------
	    -- Machine_Size --
	    ------------------

	 when Attribute_Machine_Size =>
	    Generate_Attribute_Machine_Size (This, Node);

	    --------------
	    -- Mantissa --
	    --------------

	 when Attribute_Mantissa =>
	    Generate_Attribute_Mantissa (This, Node);

	    ---------
	    -- Max --
	    ---------

	 when Attribute_Max =>
	    Generate_Attribute_Max (This, Node);

	    ----------------------------------
	    -- Max_Alignment_For_Allocation --
	    ----------------------------------

	 when Attribute_Max_Size_In_Storage_Elements =>
	    Generate_Attribute_Max_Size_In_Storage_Elements (This, Node);

	    ----------------------------------
	    -- Max_Size_In_Storage_Elements --
	    ----------------------------------

	    -----------------------
	    -- Maximum_Alignment --
	    -----------------------

	 when Attribute_Maximum_Alignment =>
	    Generate_Attribute_Maximum_Alignment (This, Node);

	    --------------------
	    -- Mechanism_Code --
	    --------------------

	 when Attribute_Mechanism_Code =>
	    Generate_Attribute_Mechanism_Code (This, Node);

	    ---------
	    -- Min --
	    ---------

	 when Attribute_Min =>
	    Generate_Attribute_Min (This, Node);

	    -------------
	    -- Modulus --
	    -------------

	 when Attribute_Modulus =>
	    Generate_Attribute_Modulus (This, Node);

	    --------------------
	    -- Null_Parameter --
	    --------------------

	 when Attribute_Null_Parameter => 
	    Generate_Attribute_Null_Parameter (This, Node);
	    
	    -----------------
	    -- Object_Size --
	    -----------------

	 when Attribute_Object_Size =>
	    Generate_Attribute_Object_Size (This, Node);

	    ---------
	    -- Old --
	    ---------

--  	 when Attribute_Old => 
--  	    Generate_Attribute_Old (This, Node);
	    
	    ----------------------
	    -- Overlaps_Storage --
	    ----------------------

--  	 when Attribute_Overlaps_Storage =>
--  	    Generate_Attribute_Overlaps_Storage (This, Node);

	    -------------------------
	    -- Passed_By_Reference --
	    -------------------------

	 when Attribute_Passed_By_Reference =>
	    Generate_Attribute_Passed_By_Reference (This, Node);

	    ------------------
	    -- Pool_Address --
	    ------------------

	 when Attribute_Pool_Address =>
	    Generate_Attribute_Pool_Address (This, Node);

	    ---------
	    -- Pos --
	    ---------

	 when Attribute_Pos =>
	    Generate_Attribute_Pos (This, Node);

	    --------------
	    -- Position --
	    --------------

	 when Attribute_Position =>
	    Generate_Attribute_Position (This, Node);

	    ----------
	    -- Pred --
	    ----------

	 when Attribute_Pred =>
	    Generate_Attribute_Pred (This, Node);

	    -----------
	    -- Range --
	    -----------

	 when Attribute_Range =>
	    Generate_Attribute_Range (This, Node);

	    ---------
	    -- Ref --
	    ---------

--  	 when Attribute_Ref =>
--  	    Generate_Attribute_Ref (This, Node);

	    ---------------
	    -- Remainder --
	    ---------------

	 when Attribute_Remainder =>
	    Generate_Attribute_Remainder (This, Node);

	    -----------
	    -- Round --
	    -----------

	 when Attribute_Round =>
	    Generate_Attribute_Round (This, Node);

	    --------------
	    -- Rounding --
	    --------------

	 when Attribute_Rounding =>
	    Generate_Attribute_Rounding (This, Node);

	    --------------------------
	    -- Scalar_Storage_Order --
	    --------------------------

--  	 when Attribute_Scalar_Storage_Order => 
--  	    Generate_Attribute_Scalar_Storage_Order (This, Node);
	    
	    ----------
	    -- Size --
	    ----------

	 when Attribute_Size | Attribute_VADS_Size => 
	    Generate_Attribute_Size (This, Node);
	    
	    ------------------
	    -- Storage_Pool --
	    ------------------

	 when Attribute_Storage_Pool       =>
	    Generate_Attribute_Storage_Pool (This, Node);
	    
	    ------------------
	    -- Storage_Size --
	    ------------------

	 when Attribute_Storage_Size => 
	    Generate_Attribute_Storage_Size (This, Node);
	    
	    ------------------
	    -- Storage_Unit --
	    ------------------

	 when Attribute_Storage_Unit =>
	    Generate_Attribute_Storage_Unit (This, Node);

	    ----------
	    -- Succ --
	    ----------

	 when Attribute_Succ =>
	    Generate_Attribute_Succ (This, Node);

	    ---------
	    -- Tag --
	    ---------

	 when Attribute_Tag => 
	    Generate_Attribute_Tag (This, Node);
	    
	    -----------------
	    -- Target_Name --
	    -----------------

	 when Attribute_Target_Name => 
	    Generate_Attribute_Target_Name (This, Node);

	    ----------------
	    -- Terminated --
	    ----------------

	 when Attribute_Terminated =>
	    Generate_Attribute_Terminated (This, Node);

	    ----------------
	    -- To_Address --
	    ----------------

	 when Attribute_To_Address => 
	    Generate_Attribute_To_Address (This, Node);
	    
	    ----------------
	    -- Truncation --
	    ----------------

	 when Attribute_Truncation =>
	    Generate_Attribute_Truncation (This, Node);

	    ----------------
	    -- Type_Class --
	    ----------------

	 when Attribute_Type_Class =>
	    Generate_Attribute_Type_Class (This, Node);

	    --------------
	    -- TypeCode --
	    --------------

--  	 when Attribute_TypeCode =>
--  	    Generate_Attribute_TypeCode (This, Node);

	    ----------------------
	    -- Unchecked_Access --
	    ----------------------

	 when Attribute_Unchecked_Access =>
	    Generate_Attribute_Unchecked_Access (This, Node);

	    -------------------------
	    -- Unconstrained_Array --
	    -------------------------

	 when Attribute_Unconstrained_Array =>
	    Generate_Attribute_Unconstrained_Array (This, Node);

	    ------------------------------
	    -- Universal_Literal_String --
	    ------------------------------

	 when Attribute_Universal_Literal_String => 
	    Generate_Attribute_Universal_Literal_String (This, Node);
	    
	    -------------------------
	    -- Unrestricted_Access --
	    -------------------------

	 when Attribute_Unrestricted_Access =>
	    Generate_Attribute_Unrestricted_Access (This, Node);

	    ---------
	    -- Val --
	    ---------

	 when Attribute_Val => 
	    Generate_Attribute_Val (This, Node);

	    -----------
	    -- Value --
	    -----------

	 when Attribute_Value => 
	    Generate_Attribute_Value (This, Node);

	    ----------------
	    -- Value_Size --
	    ----------------

	 when Attribute_Value_Size =>
	    Generate_Attribute_Value_Size (This, Node);

	    -------------
	    -- Version --
	    -------------

	 when Attribute_Version =>
	    Generate_Attribute_Version (This, Node);

	    ------------------
	    -- Wchar_T_Size --
	    ------------------

	 when Attribute_Wchar_T_Size =>
	   Generate_Attribute_Wchar_T_Size (This, Node);

	   ----------------
	   -- Wide_Image --
	   ----------------

	 when Attribute_Wide_Image => 
	    Generate_Attribute_Wide_Image (This, Node);

	    ----------------
	    -- Wide_Value --
	    ----------------

	 when Attribute_Wide_Value => 
	    Generate_Attribute_Wide_Value (This, Node);

	    ----------------
	    -- Wide_Width --
	    ----------------

	 when Attribute_Wide_Width =>
	    Generate_Attribute_Wide_Width (This, Node);

	    ---------------
	    -- Word_Size --
	    ---------------

	 when Attribute_Word_Size =>
	    Generate_Attribute_Word_Size (This, Node);

         when others =>
            null;
      end case;
   end Generate_Attribute;
   
   ------------
   -- Access --
   ------------
   
   procedure Generate_Attribute_Access 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Access;
   
   -------------
   -- Address --
   -------------
   
   procedure Generate_Attribute_Address 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Address;
   
   ------------------
   -- Address_Size --
   ------------------
   
   procedure Generate_Attribute_Address_Size 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Address_Size;
   
   ---------------
   -- Alignment --
   ---------------
   
   procedure Generate_Attribute_Alignment 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Alignment;
   
   ----------
   -- Base --
   ----------
   
   procedure Generate_Attribute_Base 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Base;
   
   ---------
   -- Bit --
   ---------
   
   procedure Generate_Attribute_Bit 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Bit;

   ---------------
   -- Bit_Order --
   ---------------

   procedure Generate_Attribute_Bit_Order 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Bit_Order;

   ------------------
   -- Bit_Position --
   ------------------

   procedure Generate_Attribute_Bit_Position 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Bit_Position;

   ------------------
   -- Body_Version --
   ------------------

   procedure Generate_Attribute_Body_Version 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Body_Version;

   -----------
   -- Class --
   -----------

   procedure Generate_Attribute_Class 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Class;

   ------------------
   -- Code_Address --
   ------------------

   procedure Generate_Attribute_Code_Address 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Code_Address;

   ----------------------
   -- Compiler_Version --
   ----------------------

   procedure Generate_Attribute_Compiler_Version 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Compiler_Version;

   --------------------
   -- Component_Size --
   --------------------

   procedure Generate_Attribute_Component_Size 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Component_Size;

   -----------------
   -- Constrained --
   -----------------

   procedure Generate_Attribute_Constrained 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Constrained;

   ---------------
   -- Copy_Sign --
   ---------------

   procedure Generate_Attribute_Copy_Sign 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Copy_Sign;

   -----------------------
   -- Default_Bit_Order --
   -----------------------

   procedure Generate_Attribute_Default_Bit_Order 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Default_Bit_Order;

   ----------------------------------
   -- Default_Scalar_Storage_Order --
   ----------------------------------

   procedure Generate_Attribute_Default_Scalar_Storage_Order 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Default_Scalar_Storage_Order;

   -----------
   -- Deref --
   -----------

   procedure Generate_Attribute_Deref 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Deref;

   ---------------
   -- Elab_Body --
   ---------------

   --  Also handles processing for Elab_Spec and Elab_Subp_Body

   procedure Generate_Attribute_Elaboration
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Elaboration;


   ---------------
   -- Elab_Spec --
   ---------------

   --  Shares processing with Elab_Body

   ----------------
   -- Elaborated --
   ----------------

   procedure Generate_Attribute_Elaborated 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Elaborated;

   -------------
   -- Enabled --
   -------------

   procedure Generate_Attribute_Enabled 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Enabled;

   --------------
   -- Enum_Rep --
   --------------

   procedure Generate_Attribute_Enum_Rep 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Enum_Rep;

   --------------
   -- Enum_Val --
   --------------

   procedure Generate_Attribute_Enum_Val 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Enum_Val;

   --------------
   -- Exponent --
   --------------

   procedure Generate_Attribute_Exponent 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Exponent;

   -----------
   -- First --
   -----------

   procedure Generate_Attribute_First 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_First;

   ---------------
   -- First_Bit --
   ---------------

   procedure Generate_Attribute_First_Bit 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_First_Bit;

   -----------------
   -- First_Valid --
   -----------------

   procedure Generate_Attribute_First_Valid 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_First_Valid;

   -----------
   -- Floor --
   -----------

   procedure Generate_Attribute_Floor 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Floor;

   --------------
   -- Fraction --
   --------------

   procedure Generate_Attribute_Fraction 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Fraction;

   -----------------------
   -- Has_Access_Values --
   -----------------------

   procedure Generate_Attribute_Has_Access_Values 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Has_Access_Values;

   ----------------------
   -- Has_Same_Storage --
   ----------------------

   procedure Generate_Attribute_Has_Same_Storage 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Has_Same_Storage;

   -----------------------
   -- Has_Tagged_Values --
   -----------------------

   procedure Generate_Attribute_Has_Tagged_Values 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Has_Tagged_Values;

   -----------
   -- Image --
   -----------

   procedure Generate_Attribute_Image 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Image;

   ---------
   -- Img --
   ---------

   procedure Generate_Attribute_Img 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Img; 

   -------------------
   -- Invalid_Value --
   -------------------

   procedure Generate_Attribute_Invalid_Value 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Invalid_Value;

   ----------
   -- Last --
   ----------

   procedure Generate_Attribute_Last 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Last;

   --------------
   -- Last_Bit --
   --------------

   procedure Generate_Attribute_Last_Bit 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Last_Bit;

   ------------------
   -- Leading_Part --
   ------------------

   procedure Generate_Attribute_Leading_Part 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Leading_Part;

   ------------
   -- Length --
   ------------

   procedure Generate_Attribute_Length 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Length;

   ------------------
   -- Machine_Size --
   ------------------

   procedure Generate_Attribute_Machine_Size 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Machine_Size;

   --------------
   -- Mantissa --
   --------------

   procedure Generate_Attribute_Mantissa 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Mantissa;

   ---------
   -- Max --
   ---------

   procedure Generate_Attribute_Max 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Max;

   ----------------------------------
   -- Max_Alignment_For_Allocation --
   ----------------------------------

   procedure Generate_Attribute_Max_Size_In_Storage_Elements 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Max_Size_In_Storage_Elements;

   ----------------------------------
   -- Max_Size_In_Storage_Elements --
   ----------------------------------

   procedure Generate_Attribute_Max_Alignment_For_Allocation 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Max_Alignment_For_Allocation;

   -----------------------
   -- Maximum_Alignment --
   -----------------------

   procedure Generate_Attribute_Maximum_Alignment 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Maximum_Alignment;

   --------------------
   -- Mechanism_Code --
   --------------------

   procedure Generate_Attribute_Mechanism_Code 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Mechanism_Code;

   ---------
   -- Min --
   ---------

   procedure Generate_Attribute_Min 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Min;

   ---------
   -- Mod --
   ---------

   procedure Generate_Attribute_Mod 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Mod;

   -------------
   -- Modulus --
   -------------

   procedure Generate_Attribute_Modulus 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Modulus;

   --------------------
   -- Null_Parameter --
   --------------------

   procedure Generate_Attribute_Null_Parameter 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Null_Parameter;
   
   -----------------
   -- Object_Size --
   -----------------

   procedure Generate_Attribute_Object_Size 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Object_Size;

   ---------
   -- Old --
   ---------

   procedure Generate_Attribute_Old 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Old;
   
   ----------------------
   -- Overlaps_Storage --
   ----------------------

   procedure Generate_Attribute_Overlaps_Storage 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Overlaps_Storage;

   -------------------------
   -- Passed_By_Reference --
   -------------------------

   procedure Generate_Attribute_Passed_By_Reference 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Passed_By_Reference;

   ------------------
   -- Pool_Address --
   ------------------

   procedure Generate_Attribute_Pool_Address 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Pool_Address;

   ---------
   -- Pos --
   ---------

   procedure Generate_Attribute_Pos 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Pos;

   --------------
   -- Position --
   --------------

   procedure Generate_Attribute_Position 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Position;

   ----------
   -- Pred --
   ----------

   procedure Generate_Attribute_Pred 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Pred;

   -----------
   -- Range --
   -----------

   procedure Generate_Attribute_Range 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Range;

   ---------
   -- Ref --
   ---------

   procedure Generate_Attribute_Ref 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Ref;

   ---------------
   -- Remainder --
   ---------------

   procedure Generate_Attribute_Remainder 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Remainder;

   -----------
   -- Round --
   -----------

   procedure Generate_Attribute_Round 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Round;

   --------------
   -- Rounding --
   --------------

   procedure Generate_Attribute_Rounding 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Rounding;

   --------------------------
   -- Scalar_Storage_Order --
   --------------------------

   procedure Generate_Attribute_Scalar_Storage_Order 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Scalar_Storage_Order;
   
   ----------
   -- Size --
   ----------

   procedure Generate_Attribute_Size
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Size;
   
   ------------------
   -- Storage_Pool --
   ------------------

   procedure Generate_Attribute_Storage_Pool
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Storage_Pool;
   
   ------------------
   -- Storage_Size --
   ------------------

   procedure Generate_Attribute_Storage_Size 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Storage_Size;
   
   ------------------
   -- Storage_Unit --
   ------------------

   procedure Generate_Attribute_Storage_Unit 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Storage_Unit;

   ----------
   -- Succ --
   ----------

   procedure Generate_Attribute_Succ 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Succ;

   --------------------------------
   -- System_Allocator_Alignment --
   --------------------------------

   procedure Generate_Attribute_System_Allocator_Alignment 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_System_Allocator_Alignment;

   ---------
   -- Tag --
   ---------

   procedure Generate_Attribute_Tag 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Tag;
   
   -----------------
   -- Target_Name --
   -----------------

   procedure Generate_Attribute_Target_Name 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Target_Name; 

   ----------------
   -- Terminated --
   ----------------

   procedure Generate_Attribute_Terminated 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Terminated;

   ----------------
   -- To_Address --
   ----------------

   procedure Generate_Attribute_To_Address 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_To_Address;
   
   ------------
   -- To_Any --
   ------------

   procedure Generate_Attribute_To_Any 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_To_Any; 

   ----------------
   -- Truncation --
   ----------------

   procedure Generate_Attribute_Truncation 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Truncation;

   ----------------
   -- Type_Class --
   ----------------

   procedure Generate_Attribute_Type_Class 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Type_Class;

   --------------
   -- TypeCode --
   --------------

   procedure Generate_Attribute_TypeCode 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_TypeCode;

   --------------
   -- Type_Key --
   --------------

   procedure Generate_Attribute_Type_Key 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Type_Key;

   ----------------------
   -- Unchecked_Access --
   ----------------------

   procedure Generate_Attribute_Unchecked_Access 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Unchecked_Access;

   -------------------------
   -- Unconstrained_Array --
   -------------------------

   procedure Generate_Attribute_Unconstrained_Array 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Unconstrained_Array;

   ------------------------------
   -- Universal_Literal_String --
   ------------------------------

   procedure Generate_Attribute_Universal_Literal_String 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Universal_Literal_String;
   
   -------------------------
   -- Unrestricted_Access --
   -------------------------

   procedure Generate_Attribute_Unrestricted_Access 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Unrestricted_Access;

   ---------
   -- Val --
   ---------

   procedure Generate_Attribute_Val 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Val; 

   -----------
   -- Value --
   -----------

   procedure Generate_Attribute_Value 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Value;

   ----------------
   -- Value_Size --
   ----------------

   procedure Generate_Attribute_Value_Size 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Value_Size;

   -------------
   -- Version --
   -------------

   procedure Generate_Attribute_Version 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Version;

   ------------------
   -- Wchar_T_Size --
   ------------------

   procedure Generate_Attribute_Wchar_T_Size 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Wchar_T_Size;

   ----------------
   -- Wide_Image --
   ----------------

   procedure Generate_Attribute_Wide_Image 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Wide_Image;

   ---------------------
   -- Wide_Wide_Image --
   ---------------------

   procedure Generate_Attribute_Wide_Wide_Image 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Wide_Wide_Image; 

   ----------------
   -- Wide_Value --
   ----------------

   procedure Generate_Attribute_Wide_Value 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Wide_Value;

   ---------------------
   -- Wide_Wide_Value --
   ---------------------

   procedure Generate_Attribute_Wide_Wide_Value 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Wide_Wide_Value; 

   ---------------------
   -- Wide_Wide_Width --
   ---------------------

   procedure Generate_Attribute_Wide_Wide_Width 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Wide_Wide_Width;

   ----------------
   -- Wide_Width --
   ----------------

   procedure Generate_Attribute_Wide_Width 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Wide_Width;

   ---------------
   -- Word_Size --
   ---------------

   procedure Generate_Attribute_Word_Size 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Attribute_Word_Size;
   
end Glips.Gen.Attrs;
