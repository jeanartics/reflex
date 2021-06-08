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

package Unity.Lib is
   
   
   -- Unity Numeric Types Entities --
   ----------------------------------
   
   Byte_Type_Entity : Entity_Id;
   --  Unsigned 8-bit integer
   
   Int_Type_Entity : Entity_Id;
   --  Signed 16-bit integer

   Uint_Type_Entity : Entity_Id;
   for Uint'Size use 16;
   --  Unsigned 16-bit integer

   Dint_Type_Entity : Entity_Id;
   for Dint'Size use 32;
   --  Signed 32-bit integer

   Udint_Type_Entity : Entity_Id;
   for Udint'Size use 32;
   --  Unsigned 32-bit integer

   
   ----------------------------------
   -- Numeric Conversion Functions --
   ----------------------------------
   
   Byte_To_Int_Entity   : Entity_Id;
   Byte_To_Uint_Entity  : Entity_Id;
   Byte_To_Dint_Entity  : Entity_Id;
   Byte_To_Udint_Entity : Entity_Id;
   Byte_To_Real_Entity  : Entity_Id;
   
   Int_To_Byte_Entity   : Entity_Id;
   Int_To_Uint_Entity   : Entity_Id;
   Int_To_Dint_Entity   : Entity_Id;
   Int_To_Udint_Entity  : Entity_Id;
   Int_To_Real_Entity   : Entity_Id;
   
   Uint_To_Byte_Entity  : Entity_Id;
   Uint_To_Int_Entity   : Entity_Id;
   Uint_To_Dint_Entity  : Entity_Id;
   Uint_To_Udint_Entity : Entity_Id;
   Uint_To_Real_Entity  : Entity_Id;
   
   Dint_To_Byte_Entity  : Entity_Id;
   Dint_To_Int_Entity   : Entity_Id;
   Dint_To_Uint_Entity  : Entity_Id;
   Dint_To_Udint_Entity : Entity_Id;
   Dint_To_Real_Entity  : Entity_Id;
   
   Udint_To_Byte_Entity : Entity_Id;
   Udint_To_Int_Entity  : Entity_Id;
   Udint_To_Uint_Entity : Entity_Id;
   Udint_To_Dint_Entity : Entity_Id;
   Udint_To_Real_Entity : Entity_Id;
   
   Real_To_Byte_Entity  : Entity_Id;
   Real_To_Int_Entity   : Entity_Id;
   Real_To_Uint_Entity  : Entity_Id;
   Real_To_Dint_Entity  : Entity_Id;
   Real_To_Udint_Entity : Entity_Id;
   
   -- Unity target numeric type --
   -------------------------------
   
   type Plc_Numeric_Type is
     (Byte_Type,
      Int_Type,
      Uint_Type,
      Dint_Type,
      Udint_Type,
      Real_Type);
   
   -- Unity numeric convrsion functions set --
   -------------------------------------------
   
   type Plc_Numeric_Convert_Function is
     (Int_To_Dint,
      Int_To_Uint,
      Int_To_Udint,
      Int_To_Real,
      
      Uint_To_Int,
      Uint_To_Dint,
      Uint_To_Udint,
      Uint_To_Real,
      
      Dint_To_Int,
      Dint_To_Uint,
      Dint_To_Udint,
      Dint_To_Real,
      
      Udint_To_Int,
      Udint_To_Uint,
      Udint_To_Dint,
      Udint_To_Real,
      
      Real_To_Int,
      Real_To_Uint,
      Real_To_Dint,
      Real_To_Udint);
	
   procedure Initialize_Unity_Lib;
   
private
   
   -- Associtaive table from numeric source to target conversion --
   ----------------------------------------------------------------
   
   type Plc_Convert_Function_Array is 
     array (Plc_Numeric_Type, 
	    Plc_Numeric_Type) of Plc_Convert_Function_Set;
   
   Plc_Convert_Function_Table : Plc_Convert_Function_Array :=
     (Int_Type      =>
	(Int_Type     => Unused_Convert,
	 Dint_Type    => Int_To_Dint,
	 Uint_Type    => Int_To_Uint,
	 Udint_Type   => Int_To_Udint,
	 Real_Type    => Int_To_Real)
     ),
     
     (Uint_Type     =>
	(Int_Type     => Uint_To_Int,
	 Uint_Type    => Unused_Convert,
	 Dint_Type    => Uint_To_Dint,
	 Udint_Type   => Uint_To_Udint,
	 Real_Type    => Uint_To_Real)
     ),
     
     (Dint_Type    =>
	(Int_Type     => Dint_To_Int,
	 Uint_Type    => Dint_To_Uint,
	 Dint_Type    => Unused_Convert,
	 Udint_Type   => Dint_To_Udint,
	 Real_Type    => Dint_To_Real)
     ),
     
     (Udint_Type    =>
	(Int_Type     => Udint_To_Int,
	 Uint_Type    => Udint_To_Uint,
	 Dint_Type    => Udint_To_Dint,
	 UDint_Type   => Unused_Convert,
	 Real_Type    => Udint_To_Real)
     ),
     
     (Real_Type =>
	(Int_Type     => Real_To_Int,
	 Uint_Type    => Real_To_Uint,
	 Dint_Type    => Real_To_Dint,
	 Udint_Type   => Real_To_Udint,
	 Real_Type    => Unused_Convert)
     );
   
   type Plc_Numeric_Convert_Function_Table_type
      is array (Plc_Numeric_Convert_Function) of Entity_Id;
   
   Plc_Numeric_Convert_Function_Table :
     Plc_Numeric_Convert_Function_Table_Type :=
     
     (Int_To_Dint   => Int_To_Dint_Entity,
      Int_To_Uint   => Int_To_Uint_Entity,
      Int_To_Udint  => Int_To_Udint_Entity,
      Int_To_Real   => Int_To_Real_Entity,
      
      Uint_To_Int   => Uint_To_Int_Entity,
      Uint_To_Dint  => Uint_To_Dint_Entity,
      Uint_To_Udint => Uint_To_Udint_Entity,
      Uint_To_Real  => Uint_To_Real_Entity,
      
      Dint_To_Int   => Dint_To_Int_Entity,
      Dint_To_Uint  => Dint_To_Uint_Entity,
      Dint_To_Udint => Dint_To_Udint_Entity,
      Dint_To_Real  => Dint_To_Real_Entity,
      
      Udint_To_Int  => Dint_To_Real_Entity,
      Udint_To_Uint => Udint_To_Uint_Entity,
      Udint_To_Dint => Udint_To_Dint_Entity,
      Udint_To_Real => Udint_To_Real_Entity,
      
      Real_To_Int   => Real_To_Int_Entity,
      Real_To_Uint  => Real_To_Uint_Entity,
      Real_To_Dint  => Real_To_Dint_Entity,
      Real_To_Udint => Real_To_Udint_Entity);
   
   function Build_Function_Entity 
     (Chars  : Name_Id;
      Result : Entity_Id) return Entity_Id;
   
   function Build_Basic_Numeric_Type
     (Chars  : Name_Id;
      Typ    : Entity_Id) return Entity_Id;
   
      
end Unity.Lib;
