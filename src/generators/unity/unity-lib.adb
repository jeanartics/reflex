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

package body Unity.Lib is
   
   -------------------------
   -- New_Standard_Entity --
   -------------------------

   function New_Standard_Entity
     (New_Node_Kind : Node_Kind := N_Defining_Identifier) return Entity_Id
   is
      E : constant Entity_Id := New_Entity (New_Node_Kind, Stloc);

   begin
      --  All standard entities are Pure and Public

      Set_Is_Pure (E);
      Set_Is_Public (E);

      --  All standard entity names are analyzed manually, and are thus
      --  frozen as soon as they are created.

      Set_Is_Frozen (E);

      --  Set debug information required for all standard types

      Set_Needs_Debug_Info (E);

      --  All standard entities are built with fully qualified names, so
      --  set the flag to prevent an abortive attempt at requalification!

      Set_Has_Qualified_Name (E);

      --  Return newly created entity to be completed by caller

      return E;
   end New_Standard_Entity;

   ---------------
   -- Make_Name --
   ---------------

   procedure Make_Name (Id : Entity_Id; Nam : String) is
   begin
      for J in 1 .. Nam'Length loop
         Name_Buffer (J) := Fold_Lower (Nam (Nam'First + (J - 1)));
      end loop;

      Name_Len := Nam'Length;
      Set_Chars (Id, Name_Find);
   end Make_Name;
   
   ---------------------------
   -- Build_Function_Entity --
   ---------------------------
   
   function Build_Function_Entity 
     (Chars  : Name_Id;
      Result : Entity_Id) return Entity_Id is
   begin
   end Build_Function_Entity;
   
   
   ----------------------
   -- Build_Float_Type --
   ----------------------

   procedure Build_Float_Type (E : Entity_Id; Siz : Int; Digs : Int) is
   begin
      Set_Type_Definition (Parent (E),
        Make_Floating_Point_Definition (Stloc,
          Digits_Expression => Make_Integer (UI_From_Int (Digs))));
      Set_Ekind                      (E, E_Floating_Point_Type);
      Set_Etype                      (E, E);
      Init_Size                      (E, Siz);
      Set_Prim_Alignment             (E);
      Init_Digits_Value              (E, Digs);
      Set_Float_Bounds               (E);
      Set_Is_Frozen                  (E);
      Set_Is_Public                  (E);
      Set_Size_Known_At_Compile_Time (E);
   end Build_Float_Type;

   -------------------------------
   -- Build_Signed_Integer_Type --
   -------------------------------

   procedure Build_Signed_Integer_Type (E : Entity_Id; Siz : Int) is
      U2Siz1 : constant Uint := 2 ** (Siz - 1);
      Lbound : constant Uint := -U2Siz1;
      Ubound : constant Uint := U2Siz1 - 1;

   begin
      Set_Type_Definition (Parent (E),
        Make_Signed_Integer_Type_Definition (Stloc,
          Low_Bound  => Make_Integer (Lbound),
          High_Bound => Make_Integer (Ubound)));

      Set_Ekind                      (E, E_Signed_Integer_Type);
      Set_Etype                      (E, E);
      Init_Size                      (E, Siz);
      Set_Prim_Alignment             (E);
      Set_Integer_Bounds             (E, E, Lbound, Ubound);
      Set_Is_Frozen                  (E);
      Set_Is_Public                  (E);
      Set_Is_Known_Valid             (E);
      Set_Size_Known_At_Compile_Time (E);
   end Build_Signed_Integer_Type;

   
   ------------------------------
   -- Build_Basic_Numeric_Type --
   ------------------------------
   
   function Build_Basic_Numeric_Type
     (Chars  : Name_Id;
      Typ    : Entity_Id) return Entity_Id is
   begin
   end Build_Basic_Numeric_Type;
   
   
   --------------------------
   -- Initialize_Unity_Lib --
   --------------------------
   
   procedure Initialize_Unity_Lib is
   begin
      
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

      
   Int_Type_Entity := New_Unit_Entity;
   Decl := New_Node (N_Full_Type_Declaration, Stloc);
   Set_Defining_Identifier (Decl, Int_Type_Entity);
   Make_Name (Int_Type_Entity, "int");
   Set_Scope (Int_Type_Entity, Standard_Standard);
   Build_Signed_Integer_Type (Int_Type_Entity, 16);

      Standard_Integer_32 := New_Standard_Entity;
      Decl := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Standard_Integer_32);
      Make_Name (Standard_Integer_32, "integer_32");
      Set_Scope (Standard_Integer_32, Standard_Standard);
      Build_Signed_Integer_Type (Standard_Integer_32, 32);

      Standard_Integer_64 := New_Standard_Entity;
      Decl := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Standard_Integer_64);
      Make_Name (Standard_Integer_64, "integer_64");
      Set_Scope (Standard_Integer_64, Standard_Standard);
      Build_Signed_Integer_Type (Standard_Integer_64, 64);
   end Initialize_Unity_Lib;
   
      ------------------------------------
      -- Corresponding_Numeric_Plc_Type --
      ------------------------------------
      
      function Corresponding_Numeric_Plc_Type
     (Typ : Entity_Id) return Plc_Numeric_Type is
      
      Target_Type     : Entity_Id;
      Target_Size     : Uint;
      Target_Unsigned : Boolean;
      Target_Plc      : Plc_Numeric_Type;
   begin
      pragma Assert (Is_Numeric_Type (Typ));
      
      Target_Type     := Base_Type (Typ);
      Target_Size     := Esize (Target_Type);
      Target_Unsigned := Is_Unsigned_Type (Target_Type);
      
      if Is_Floating_Point_Type (Target_Type) then
	 return Plc_Real_Type;
	 
      elsif Is_Unsigned_Type (Target_Type) then
	 if Target_Size <= 8 then
	    Target_Plc := Byte_Type;
	 elsif Target_Size <= 16 then
	    Target_Plc := Uint_Type;
	 elsif Target_Size <= 32 then
	    Target_Plc := Udint_Type;
	 else
	    Target_Plc := Udint_Type;
	 end if;
	 
      else
	 if Target_Size <= 16 then
	    Target_Plc := Int_Type;
	 elsif Target_Size <= 32 then
	    Target_Plc := Dint_Type;
	 else
	    Target_Plc := Dint_Type;
	 end if;
      end if;	 
      
      return Target_Plc;
   end Corresponding_Numeric_Plc_Type;
   
   ------------------------------------
   -- Plc_Nuneric_Converion_Function --
   ------------------------------------
   
   function Plc_Nuneric_Converion_Function
     (Source : Entity_Id;
      Target : Entity_Id) return Plc_Numeric_Convert_Function is
      
   begin
      pragma Assert
	(Is_Numeric_Type (Source) and then Is_Numeric_Type (Target));
      
      Plc_Source_Type := Corresponding_Numeric_Plc_Type (Source);
      Plc_Target_Type := Corresponding_Numeric_Plc_Type (Target);
      
      return Plc_Convert_Function_Table (Plc_Source_Type, Plc_Target_Type);
   end Plc_Nuneric_Converion_Function;
   
   
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
   
   
   
   ------------------
   -- Create_Unity --
   ------------------

   --  The tree for the package Standard is prefixed to all compilations.
   --  Several entities required by semantic analysis are denoted by global
   --  variables that are initialized to point to the corresponding
   --  occurrences in STANDARD. The visible entities of STANDARD are
   --  created here. The private entities defined in STANDARD are created
   --  by Initialize_Standard in the semantics module.

   procedure Create_Unity is
      Decl_S : constant List_Id := New_List;
      --  List of declarations in Standard

      Decl_A : constant List_Id := New_List;
      --  List of declarations in ASCII

      Decl       : Node_Id;
      Pspec      : Node_Id;
      Tdef_Node  : Node_Id;
      Ident_Node : Node_Id;
      Ccode      : Char_Code;
      E_Id       : Entity_Id;
      R_Node     : Node_Id;
      B_Node     : Node_Id;

   begin
      --  Initialize scanner for internal scans of literals

      Scn.Initialize_Scanner (No_Unit, Internal_Source_File);

      --  First step is to create defining identifiers for each entity

      for S in Unity_Entity_Type loop
         declare
            S_Name : constant String := Unity_Entity_Type'Image (S);
            --  Name of entity (note we skip S_ at the start)

            Ident_Node : Node_Id;
            --  Defining identifier node

         begin
            Ident_Node := New_Standard_Entity;
            Make_Name (Ident_Node, S_Name (3 .. S_Name'Length));
            Standard_Entity (S) := Ident_Node;
         end;
      end loop;

      --  Create package declaration node for package Standard

      Unity_Package_Node := New_Node (N_Package_Declaration, Stloc);

      Pspec := New_Node (N_Package_Specification, Stloc);
      Set_Specification (Standard_Package_Node, Pspec);

      Set_Defining_Unit_Name (Pspec, Standard_Standard);
      Set_Visible_Declarations (Pspec, Decl_S);

      Set_Ekind (Standard_Standard, E_Package);
      Set_Is_Pure (Standard_Standard);
      Set_Is_Compilation_Unit (Standard_Standard);

      --  Create type declaration nodes for standard types

      for S in S_Types loop
         Decl := New_Node (N_Full_Type_Declaration, Stloc);
         Set_Defining_Identifier (Decl, Standard_Entity (S));
         Set_Is_Frozen (Standard_Entity (S));
         Set_Is_Public (Standard_Entity (S));
         Append (Decl, Decl_S);
      end loop;

   end Create_Unity;

end Unity.Lib;
