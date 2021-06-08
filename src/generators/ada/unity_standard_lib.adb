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

with Ada.Text_Io; use Ada.Text_Io;

with Atree;    use Atree;
with Csets;    use Csets;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Layout;   use Layout;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Ttypef;   use Ttypef;
with Scn;
with Sem_Mech; use Sem_Mech;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

with Reflex.Infos; use Reflex.Infos;

package body Unity_Standard_Lib is

   Stloc  : constant Source_Ptr := Standard_Location;
   Staloc : constant Source_Ptr := Standard_ASCII_Location;
   --  Standard abbreviations used throughout this package

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Build_Float_Type (E : Entity_Id; Siz : Int; Digs : Int);
   --  Procedure to build standard predefined float base type. The first
   --  parameter is the entity for the type, and the second parameter
   --  is the size in bits. The third parameter is the digits value.

   procedure Build_Signed_Integer_Type (E : Entity_Id; Siz : Int);
   --  Procedure to build standard predefined signed integer subtype. The
   --  first parameter is the entity for the subtype. The second parameter
   --  is the size in bits. The corresponding base type is not built by
   --  this routine but instead must be built by the caller where needed.

   function Identifier_For (S : Unity_Entity_Type) return Node_Id;
   --  Returns an identifier node with the same name as the defining
   --  identifier corresponding to the given Standard_Entity_Type value

   procedure Make_Component
     (Rec : Entity_Id;
      Typ : Entity_Id;
      Nam : String);
   --  Build a record component with the given type and name, and append to
   --  the list of components of Rec.

   function Make_Formal
     (Typ         : Entity_Id;
      Formal_Name : String) return Entity_Id;
   --  Construct entity for subprogram formal with given name and type

   function Make_Integer (V : Uint) return Node_Id;
   --  Builds integer literal with given value

   procedure Make_Name (Id : Entity_Id; Nam : String);
   --  Make an entry in the names table for Nam, and set as Chars field of Id

   function New_Unity_Entity
     (New_Node_Kind : Node_Kind := N_Defining_Identifier) return Entity_Id;
   --  Builds a new entity for Unity

   procedure Set_Integer_Bounds
     (Id  : Entity_Id;
      Typ : Entity_Id;
      Lb  : Uint;
      Hb  : Uint);
   --  Procedure to set bounds for integer type or subtype. Id is the entity
   --  whose bounds and type are to be set. The Typ parameter is the Etype
   --  value for the entity (which will be the same as Id for all predefined
   --  integer base types. The third and fourth parameters are the bounds.
   
   procedure Build_Conversion_Function 
     (E    : Entity_Id;
      From : Entity_Id;
      To   : Entity_Id);
   --  Procedure To build Unity conversion functions. E is the functio entity
   --  from is the type to convert to type To.
   
   procedure Set_Float_Bounds (Id  : Entity_Id);
   --  Procedure to set bounds for float type or subtype. Id is the entity
   --  whose bounds and type are to be set (a floating-point type).

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

   procedure Build_Signed_Integer_Type
     (E   : Entity_Id; 
      Siz : Int) is
      
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
   
   ---------------------------------
   -- Build_Unsigned_Integer_Type --
   ---------------------------------

   procedure Build_Unsigned_Integer_Type
     (E   : Entity_Id; 
      Siz : Int) is
      
      R_Node : Node_Id;
   begin
      Set_Ekind                      (E, E_Modular_Integer_Type);
      Set_Etype                      (E, E);
      Set_Scope                      (E, Unity_Standard);
      Init_Size                      (E, Siz);
      Set_Prim_Alignment             (E);
      Set_Modulus                    (E, Uint_2 ** Siz);
      Set_Is_Unsigned_Type           (E);
      Set_Size_Known_At_Compile_Time (E);

      R_Node := New_Node (N_Range, Stloc);
      Set_Low_Bound    (R_Node, Make_Integer (Uint_0));
      Set_High_Bound   (R_Node, Make_Integer (Modulus (E) - 1));
      Set_Etype        (Low_Bound (R_Node), E);
      Set_Etype        (High_Bound (R_Node), E);
      Set_Scalar_Range (E, R_Node);
   end Build_Unsigned_Integer_Type;

   -------------------------------
   -- Build_Conversion_Function --
   -------------------------------
   
   procedure Build_Conversion_Function 
     (E    : Entity_Id;
      From : Entity_Id;
      To   : Entity_Id) is
      
      N    : Node_Id;
      Spec : Node_Id;
   begin	   
      N := New_Node (N_Subprogram_Declaration, Stloc);
      Spec := New_Node (N_Function_Specification, Stloc);
      Set_Specification (N, Spec);
      Set_Defining_Unit_Name (Spec, E);
      
      Set_Ekind (E, E_Function);
      Set_Etype (E, To);
      Set_Scope (E, Unity_Standard);
      Set_Has_Completion (E, True);
      
      Append_Entity (Make_Formal (From, "in"),  E);
   end Build_Conversion_Function;
   
   ---------------------------
   -- Create_Unity_Standard --
   ---------------------------

   --  The tree for the package Standard is prefixed to all compilations.
   --  Several entities required by semantic analysis are denoted by global
   --  variables that are initialized to point to the corresponding
   --  occurrences in STANDARD. The visible entities of STANDARD are
   --  created here. The private entities defined in STANDARD are created
   --  by Initialize_Standard in the semantics module.

   procedure Create_Unity_Standard is
      
      Decl_U : constant List_Id := New_List;
      --  List of declarations in Standard

      Decl_A : constant List_Id := New_List;
      --  List of declarations in ASCII

      Decl       : Node_Id;
      Pspec      : Node_Id;
      Tdef_Node  : Node_Id;
      E_Id       : Entity_Id;
      R_Node     : Node_Id;
      B_Node     : Node_Id;

   begin
      --  Initialize scanner for internal scans of literals

      Scn.Initialize_Scanner (No_Unit, Internal_Source_File);

      --  First step is to create defining identifiers for each entity

      for U in Unity_Entity_Type loop
         declare
            U_Name : constant String := Unity_Entity_Type'Image (U);
            --  Name of entity (note we skip S_ at the start)

            Id     : Entity_Id;
            --  Defining identifier node

         begin
            Id := New_Unity_Entity;
            Make_Name (Id, U_Name (3 .. U_Name'Length));
            Unity_Entity (U) := Id;
         end;
      end loop;

      --  Create package declaration node for package Unity Standard

      Unity_Package_Node := New_Node (N_Package_Declaration, Stloc);

      Pspec := New_Node (N_Package_Specification, Stloc);
      Set_Specification (Unity_Package_Node, Pspec);

      Set_Defining_Unit_Name (Pspec, Unity_Standard);
      Set_Visible_Declarations (Pspec, Decl_U);

      Set_Ekind (Unity_Standard, E_Package);
      Set_Is_Compilation_Unit (Unity_Standard);
      
      Set_Etype (Unity_Standard, Standard_Void_Type);
      
      --  Create type declaration nodes for standard types

      for U in Unity_Standard_Types loop
         Decl := New_Node (N_Full_Type_Declaration, Stloc);
         Set_Defining_Identifier (Decl, Unity_Entity (U));
         Set_Is_Frozen (Unity_Entity (U));
         Set_Is_Public (Unity_Entity (U));
         Append (Decl, Decl_U);
      end loop;
      
      --  Create type definition node for type Boolean. The Size is set to
      --  1 as required by Ada 95 and current ARG interpretations for Ada/83.

      --  Note: Object_Size of Boolean is 8. This means that we do NOT in
      --  general know that Boolean variables have valid values, so we do
      --  not set the Is_Known_Valid flag.

      Tdef_Node := New_Node (N_Enumeration_Type_Definition, Stloc);
      Set_Literals (Tdef_Node, New_List);
      Append (Unity_Standard_False, Literals (Tdef_Node));
      Append (Unity_Standard_True, Literals (Tdef_Node));
      Set_Type_Definition (Parent (Unity_Standard_Bool), Tdef_Node);

      Set_Ekind          (Unity_Standard_Bool, E_Enumeration_Type);
      Set_First_Literal  (Unity_Standard_Bool, Unity_Standard_False);
      Set_Etype          (Unity_Standard_Bool, Unity_Standard_Bool);
      Init_Esize         (Unity_Standard_Bool, Standard_Character_Size);
      Init_RM_Size       (Unity_Standard_Bool, 1);
      Set_Prim_Alignment (Unity_Standard_Bool);

      Set_Is_Unsigned_Type           (Unity_Standard_Bool);
      Set_Size_Known_At_Compile_Time (Unity_Standard_Bool);

      Set_Ekind           (Unity_Standard_True, E_Enumeration_Literal);
      Set_Etype           (Unity_Standard_True, Unity_Standard_Bool);
      Set_Enumeration_Pos (Unity_Standard_True, Uint_1);
      Set_Enumeration_Rep (Unity_Standard_True, Uint_1);
      Set_Is_Known_Valid  (Unity_Standard_True, True);

      Set_Ekind           (Unity_Standard_False, E_Enumeration_Literal);
      Set_Etype           (Unity_Standard_False, Unity_Standard_Bool);
      Set_Enumeration_Pos (Unity_Standard_False, Uint_0);
      Set_Enumeration_Rep (Unity_Standard_False, Uint_0);
      Set_Is_Known_Valid  (Unity_Standard_False, True);

      --  For the bounds of Boolean, we create a range node corresponding to

      --    range False .. True

      --  where the occurrences of the literals must point to the
      --  corresponding  definition.

      R_Node := New_Node (N_Range, Stloc);
      B_Node := New_Node (N_Identifier, Stloc);
      Set_Chars  (B_Node, Chars (Unity_Standard_False));
      Set_Entity (B_Node,  Unity_Standard_False);
      Set_Etype  (B_Node, Unity_Standard_Bool);
      Set_Is_Static_Expression (B_Node);
      Set_Low_Bound  (R_Node, B_Node);

      B_Node := New_Node (N_Identifier, Stloc);
      Set_Chars  (B_Node, Chars (Unity_Standard_True));
      Set_Entity (B_Node,  Unity_Standard_True);
      Set_Etype  (B_Node, Unity_Standard_Bool);
      Set_Is_Static_Expression (B_Node);
      Set_High_Bound (R_Node, B_Node);

      Set_Scalar_Range (Unity_Standard_Bool, R_Node);
      Set_Etype (R_Node, Unity_Standard_Bool);
      Set_Parent (R_Node, Unity_Standard_Bool);

      --  Record entity identifiers for boolean literals in the
      --  Boolean_Literals array, for easy reference during expansion.

      Boolean_Literals := 
	(False => Unity_Standard_False, True => Unity_Standard_True);
      
      --  Create type definition nodes for predefined integer types

      Build_Signed_Integer_Type
        (Unity_Standard_Byte, Standard_Short_Short_Integer_Size);
      Set_Internal_Function (Unity_Standard_Byte, True);
      Append_Entity (Unity_Standard_Byte, Unity_Standard);
      Put_Line
	("Unity_Standard_Byte " & Get_String (Chars (Unity_Standard_Byte)));
      
      Build_Signed_Integer_Type
        (Unity_Standard_Int, Standard_Short_Integer_Size);
      Set_Internal_Function (Unity_Standard_Int, True);
      Append_Entity (Unity_Standard_Int, Unity_Standard);
      Put_Line
	("Unity_Standard_Int " & Get_String (Chars (Unity_Standard_Int)));

      Build_Signed_Integer_Type
        (Unity_Standard_Dint, Standard_Integer_Size);
      Set_Internal_Function (Unity_Standard_Dint, True);
      Append_Entity (Unity_Standard_Dint, Unity_Standard);
      Put_Line
	("Unity_Standard_Dint " & Get_String (Chars (Unity_Standard_Dint)));

      Build_Unsigned_Integer_Type
        (Unity_Standard_Uint, Standard_Short_Integer_Size);
      Set_Internal_Function (Unity_Standard_Uint, True);
      Append_Entity (Unity_Standard_Uint, Unity_Standard);
      Put_Line
	("Unity_Standard_Uint " & Get_String (Chars (Unity_Standard_Uint)));

      Build_Unsigned_Integer_Type
        (Unity_Standard_Udint, Standard_Integer_Size);
      Set_Internal_Function (Unity_Standard_Udint, True);
      Append_Entity (Unity_Standard_Udint, Unity_Standard);
      Put_Line
	("Unity_Standard_Udint " & Get_String (Chars (Unity_Standard_Udint)));
      
      Build_Float_Type
        (Unity_Standard_Real, 
	 Standard_Short_Float_Size, 
	 Standard_Short_Float_Digits);
      Set_Internal_Function (Unity_Standard_Real, True);
      Append_Entity (Unity_Standard_Real, Unity_Standard);
      Put_Line
	("Unity_Standard_Real " & Get_String (Chars (Unity_Standard_Real)));
      
      --  Create the functions for numeric types conversion
      
      --  Byte_To_xxx
      
      Build_Conversion_Function
	(Unity_Byte_To_Int, Unity_Standard_Byte, Unity_Standard_Int);
      Set_Internal_Function (Unity_Standard_Byte, True);
      Append_Entity (Unity_Byte_To_Int, Unity_Standard);
      Put_Line
	("Unity_Byte_To_Int " & Get_String (Chars (Unity_Byte_To_Int)));
	 
      Build_Conversion_Function
	(Unity_Byte_To_Uint, Unity_Standard_Byte, Unity_Standard_Uint);
      Set_Internal_Function (Unity_Byte_To_Uint, True);
      Append_Entity (Unity_Byte_To_Uint, Unity_Standard);
      Put_Line
	("Unity_Byte_To_Uint " & Get_String (Chars (Unity_Byte_To_Uint)));
	 
      Build_Conversion_Function
	(Unity_Byte_To_Dint, Unity_Standard_Byte, Unity_Standard_Dint);
      Set_Internal_Function (Unity_Byte_To_Dint, True);
      Append_Entity (Unity_Byte_To_Dint, Unity_Standard);
      Put_Line
	("Unity_Byte_To_Dint " & Get_String (Chars (Unity_Byte_To_Dint)));
	 
      Build_Conversion_Function
	(Unity_Byte_To_Udint, Unity_Standard_Byte, Unity_Standard_Udint);
      Set_Internal_Function (Unity_Byte_To_Udint, True);
      Append_Entity (Unity_Byte_To_Udint, Unity_Standard);
      Put_Line
	("Unity_Byte_To_Udint " & Get_String (Chars (Unity_Byte_To_Udint)));
	  
      Build_Conversion_Function
	(Unity_Byte_To_Real, Unity_Standard_Byte, Unity_Standard_Real);
      Set_Internal_Function (Unity_Byte_To_Real, True);
      Append_Entity (Unity_Byte_To_Real, Unity_Standard);
      Put_Line
	("Unity_Byte_To_Real " & Get_String (Chars (Unity_Byte_To_Real)));
	 
      --  Int_To_xxx
      
      Build_Conversion_Function
	(Unity_Int_To_Byte, Unity_Standard_Int, Unity_Standard_Byte);
      Set_Internal_Function (Unity_Int_To_Byte, True);
      Append_Entity (Unity_Int_To_Byte, Unity_Standard);
	 
      Build_Conversion_Function
	(Unity_Int_To_Uint, Unity_Standard_Int, Unity_Standard_Uint);
      Set_Internal_Function (Unity_Int_To_Uint, True);
      Append_Entity (Unity_Int_To_Uint, Unity_Standard);
	 
      Build_Conversion_Function
	(Unity_Int_To_Dint, Unity_Standard_Int, Unity_Standard_Dint);
      Set_Internal_Function (Unity_Int_To_Dint, True);
      Append_Entity (Unity_Int_To_Dint, Unity_Standard);
	 
      Build_Conversion_Function
	(Unity_Int_To_Udint, Unity_Standard_Int, Unity_Standard_Udint);
      Set_Internal_Function (Unity_Int_To_Udint, True);
      Append_Entity (Unity_Int_To_Udint, Unity_Standard);
	 
      Build_Conversion_Function
	(Unity_Int_To_Real, Unity_Standard_Int, Unity_Standard_Real);
      Set_Internal_Function (Unity_Int_To_Real, True);
      Append_Entity (Unity_Int_To_Real, Unity_Standard);
	 
      --  Uint_To_xxx
      
      Build_Conversion_Function
	(Unity_Uint_To_Byte, Unity_Standard_Uint, Unity_Standard_Byte);	
      Set_Internal_Function (Unity_Uint_To_Byte, True);
      Append_Entity (Unity_Uint_To_Byte, Unity_Standard);
 
      Build_Conversion_Function
	(Unity_Uint_To_Int, Unity_Standard_Uint, Unity_Standard_Int);
      Set_Internal_Function (Unity_Uint_To_Int, True);
      Append_Entity (Unity_Uint_To_Int, Unity_Standard);
	 
      Build_Conversion_Function
	(Unity_Uint_To_Dint, Unity_Standard_Uint, Unity_Standard_Dint);
      Set_Internal_Function (Unity_Uint_To_Dint, True);
      Append_Entity (Unity_Uint_To_Dint, Unity_Standard);
	 
      Build_Conversion_Function
	(Unity_Uint_To_Udint, Unity_Standard_Uint, Unity_Standard_Udint);
      Set_Internal_Function (Unity_Uint_To_Udint, True);
      Append_Entity (Unity_Uint_To_Udint, Unity_Standard);
	 
      Build_Conversion_Function
	(Unity_Uint_To_Real, Unity_Standard_Uint, Unity_Standard_Real);
      Set_Internal_Function (Unity_Uint_To_Real, True);
      Append_Entity (Unity_Uint_To_Real, Unity_Standard);
	 
      --  Dint_To_xxx
      
      Build_Conversion_Function
	(Unity_Dint_To_Byte, Unity_Standard_Dint, Unity_Standard_Byte);
      Set_Internal_Function (Unity_Dint_To_Byte, True);
      Append_Entity (Unity_Dint_To_Byte, Unity_Standard);
	 
      Build_Conversion_Function
	(Unity_Dint_To_Int, Unity_Standard_Dint, Unity_Standard_Int);
      Set_Internal_Function (Unity_Dint_To_Int, True);
      Append_Entity (Unity_Dint_To_Int, Unity_Standard);
	 
      Build_Conversion_Function
	(Unity_Dint_To_Uint, Unity_Standard_Dint, Unity_Standard_Uint);
      Set_Internal_Function (Unity_Dint_To_Uint, True);
      Append_Entity (Unity_Dint_To_Uint, Unity_Standard);
	 
      Build_Conversion_Function
	(Unity_Dint_To_Udint, Unity_Standard_Dint, Unity_Standard_Udint);
      Set_Internal_Function (Unity_Dint_To_Udint, True);
      Append_Entity (Unity_Dint_To_Udint, Unity_Standard);
	 
      Build_Conversion_Function
	(Unity_Dint_To_Real, Unity_Standard_Dint, Unity_Standard_Real);
      Set_Internal_Function (Unity_Dint_To_Real, True);
      Append_Entity (Unity_Dint_To_Real, Unity_Standard);
	 
      --  Udint_To_xxx
      
      Build_Conversion_Function
	(Unity_Udint_To_Byte, Unity_Standard_Udint, Unity_Standard_Byte);
      Set_Internal_Function (Unity_Udint_To_Byte, True);
      Append_Entity (Unity_Udint_To_Byte, Unity_Standard);
	 
      Build_Conversion_Function
	(Unity_Udint_To_Int, Unity_Standard_Udint, Unity_Standard_Int);
      Set_Internal_Function (Unity_Udint_To_Int, True);
      Append_Entity (Unity_Udint_To_Int, Unity_Standard);
	 
      Build_Conversion_Function
	(Unity_Udint_To_Uint, Unity_Standard_Udint, Unity_Standard_Uint);
      Set_Internal_Function (Unity_Udint_To_Uint, True);
      Append_Entity (Unity_Udint_To_Uint, Unity_Standard);
	 
      Build_Conversion_Function
	(Unity_Udint_To_Dint, Unity_Standard_Udint, Unity_Standard_Dint);
      Set_Internal_Function (Unity_Udint_To_Dint, True);
      Append_Entity (Unity_Udint_To_Dint, Unity_Standard);
	 
      Build_Conversion_Function
	(Unity_Udint_To_Real, Unity_Standard_Udint, Unity_Standard_Real);
      Set_Internal_Function (Unity_Udint_To_Real, True);
      Append_Entity (Unity_Udint_To_Real, Unity_Standard);
	 
      --  Real_To_xxx
      
      Build_Conversion_Function
	(Unity_Real_To_Byte, Unity_Standard_Real, Unity_Standard_Byte);
      Set_Internal_Function (Unity_Real_To_Byte, True);
      Append_Entity (Unity_Real_To_Byte, Unity_Standard);
	 
      Build_Conversion_Function
	(Unity_Real_To_Int, Unity_Standard_Real, Unity_Standard_Int);
      Set_Internal_Function (Unity_Real_To_Int, True);
      Append_Entity (Unity_Real_To_Int, Unity_Standard);
	 
      Build_Conversion_Function
	(Unity_Real_To_Uint, Unity_Standard_Real, Unity_Standard_Uint);
      Set_Internal_Function (Unity_Real_To_Uint, True);
      Append_Entity (Unity_Real_To_Uint, Unity_Standard);
	 
      Build_Conversion_Function
	(Unity_Real_To_Dint, Unity_Standard_Real, Unity_Standard_Dint);
      Set_Internal_Function (Unity_Real_To_Dint, True);
      Append_Entity (Unity_Real_To_Dint, Unity_Standard);
	 
      Build_Conversion_Function
	(Unity_Real_To_Udint, Unity_Standard_Real, Unity_Standard_Udint);
      Set_Internal_Function (Unity_Real_To_Udint, True);
      Append_Entity (Unity_Real_To_Udint, Unity_Standard);
	 
      --  Create type definition node for type Character. Note that we do not
      --  set the Literals field, since type Character is handled with special
      --  routine that do not need a literal list.

      Tdef_Node := New_Node (N_Enumeration_Type_Definition, Stloc);
      Set_Type_Definition (Parent (Unity_Standard_Char), Tdef_Node);

      Set_Ekind          (Unity_Standard_Char, E_Enumeration_Type);
      Set_Etype          (Unity_Standard_Char, Standard_Character);
      Init_Esize         (Unity_Standard_Char, Standard_Character_Size);
      Init_RM_Size       (Unity_Standard_Char, 8);
      Set_Prim_Alignment (Unity_Standard_Char);

      Set_Is_Unsigned_Type           (Unity_Standard_Char);
      Set_Is_Character_Type          (Unity_Standard_Char);
      Set_Is_Known_Valid             (Unity_Standard_Char);
      Set_Size_Known_At_Compile_Time (Unity_Standard_Char);

      --  Create the bounds for type Character.

      R_Node := New_Node (N_Range, Stloc);

      --  Low bound for type Character (Standard.Nul)

      B_Node := New_Node (N_Character_Literal, Stloc);
      Set_Is_Static_Expression (B_Node);
      Set_Chars                (B_Node, No_Name);
      Set_Char_Literal_Value   (B_Node, 16#00#);
      Set_Entity               (B_Node,  Empty);
      Set_Etype                (B_Node, Unity_Standard_Char);
      Set_Low_Bound (R_Node, B_Node);

      --  High bound for type Character

      B_Node := New_Node (N_Character_Literal, Stloc);
      Set_Is_Static_Expression (B_Node);
      Set_Chars                (B_Node, No_Name);
      Set_Char_Literal_Value   (B_Node, 16#FF#);
      Set_Entity               (B_Node,  Empty);
      Set_Etype                (B_Node, Unity_Standard_Char);
      Set_High_Bound (R_Node, B_Node);

      Set_Scalar_Range (Unity_Standard_Char, R_Node);
      Set_Etype (R_Node, Unity_Standard_Char);
      Set_Parent (R_Node, Unity_Standard_Char);

      --  Create type definition node for type String

      Tdef_Node := New_Node (N_Unconstrained_Array_Definition, Stloc);

      declare
         CompDef_Node : Node_Id;
      begin
         CompDef_Node := New_Node (N_Component_Definition, Stloc);
         Set_Aliased_Present    (CompDef_Node, False);
         Set_Subtype_Indication (CompDef_Node, Identifier_For (U_Char));
         Set_Component_Definition (Tdef_Node, CompDef_Node);
      end;

      Set_Subtype_Marks      (Tdef_Node, New_List);
      Append (Identifier_For (U_Int), Subtype_Marks (Tdef_Node));
      Set_Type_Definition (Parent (Unity_Standard_String), Tdef_Node);

      Set_Ekind          (Unity_Standard_String, E_String_Type);
      Set_Etype          (Unity_Standard_String, Unity_Standard_String);
      Set_Component_Type (Unity_Standard_String, Unity_Standard_Char);
      Set_Component_Size (Unity_Standard_String, Uint_8);
      Init_Size_Align    (Unity_Standard_String);

      --  Set index type of String

      E_Id := First
        (Subtype_Marks (Type_Definition (Parent (Unity_Standard_String))));
      Set_First_Index (Unity_Standard_String, E_Id);
      Set_Entity (E_Id, Unity_Standard_Int);
      Set_Etype (E_Id, Unity_Standard_Int);

      --  The type field of packages is set to void

      Set_Etype (Unity_Standard, Standard_Void_Type);

      --  Note on type names. The type names for the following special types
      --  are constructed so that they will look reasonable should they ever
      --  appear in error messages etc, although in practice the use of the
      --  special insertion character } for types results in special handling
      --  of these type names in any case. The blanks in these names would
      --  trouble in Gigi, but that's OK here, since none of these types
      --  should ever get through to Gigi! Attributes of these types are
      --  filled out to minimize problems with cascaded errors (for example,
      --  Any_Unity_Integer is given reasonable and consistent type and size 
      --  values)

      Any_Unity_Type := New_Unity_Entity;
      Decl := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Any_Unity_Type);
      Set_Scope (Any_Unity_Type, Unity_Standard);
      Build_Signed_Integer_Type (Any_Unity_Type, Standard_Short_Integer_Size);
      Make_Name (Any_Unity_Type, "_err_any_type");

      Any_Unity_Id := New_Unity_Entity;
      Set_Ekind             (Any_Unity_Id, E_Variable);
      Set_Scope             (Any_Unity_Id, Unity_Standard);
      Set_Etype             (Any_Unity_Id, Any_Unity_Type);
      Init_Size_Align       (Any_Unity_Id);
      Make_Name             (Any_Unity_Id, "_err_any_id");

      Any_Unity_Access := New_Unity_Entity;
      Set_Ekind             (Any_Unity_Access, E_Access_Type);
      Set_Scope             (Any_Unity_Access, Unity_Standard);
      Set_Etype             (Any_Unity_Access, Any_Unity_Access);
      Init_Size             (Any_Unity_Access, System_Address_Size);
      Set_Prim_Alignment    (Any_Unity_Access);
      Make_Name             (Any_Unity_Access, "_err_access_type");

      Any_Unity_Char := New_Unity_Entity;
      Set_Ekind             (Any_Unity_Char, E_Enumeration_Type);
      Set_Scope             (Any_Unity_Char, Unity_Standard);
      Set_Etype             (Any_Unity_Char, Any_Unity_Char);
      Set_Is_Unsigned_Type  (Any_Unity_Char);
      Set_Is_Character_Type (Any_Unity_Char);
      Init_Esize            (Any_Unity_Char, Standard_Character_Size);
      Init_RM_Size          (Any_Unity_Char, 8);
      Set_Prim_Alignment    (Any_Unity_Char);
      Set_Scalar_Range      (Any_Unity_Char, Scalar_Range (Standard_Character));
      Make_Name             (Any_Unity_Char, "_err_char_type");

      Any_Unity_Array := New_Unity_Entity;
      Set_Ekind             (Any_Unity_Array, E_String_Type);
      Set_Scope             (Any_Unity_Array, Unity_Standard);
      Set_Etype             (Any_Unity_Array, Any_Unity_Array);
      Set_Component_Type    (Any_Unity_Array, Any_Unity_Char);
      Init_Size_Align       (Any_Unity_Array);
      Make_Name             (Any_Unity_Array, "_err_array_type");

      Any_Unity_Bool := New_Unity_Entity;
      Set_Ekind            (Any_Unity_Bool, E_Enumeration_Type);
      Set_Scope            (Any_Unity_Bool, Unity_Standard);
      Set_Etype            (Any_Unity_Bool, Unity_Standard_Bool);
      Init_Esize           (Any_Unity_Bool, Standard_Character_Size);
      Init_RM_Size         (Any_Unity_Bool, 1);
      Set_Prim_Alignment   (Any_Unity_Bool);
      Set_Is_Unsigned_Type (Any_Unity_Bool);
      Set_Scalar_Range     (Any_Unity_Bool, Scalar_Range (Unity_Standard_Bool));
      Make_Name            (Any_Unity_Bool, "_err_bool_type");

      Any_Unity_Composite := New_Unity_Entity;
      Set_Ekind             (Any_Unity_Composite, E_Array_Type);
      Set_Scope             (Any_Unity_Composite, Unity_Standard);
      Set_Etype             (Any_Unity_Composite, Any_Unity_Composite);
      Set_Component_Size    (Any_Unity_Composite, Uint_0);
      Set_Component_Type    (Any_Unity_Composite, Standard_Integer);
      Init_Size_Align       (Any_Unity_Composite);
      Make_Name             (Any_Unity_Composite, "_err_composite_type");

      Any_Unity_Discrete := New_Unity_Entity;
      Set_Ekind             (Any_Unity_Discrete, E_Signed_Integer_Type);
      Set_Scope             (Any_Unity_Discrete, Unity_Standard);
      Set_Etype             (Any_Unity_Discrete, Any_Unity_Discrete);
      Init_Size             (Any_Unity_Discrete, Standard_Short_Integer_Size);
      Set_Prim_Alignment    (Any_Unity_Discrete);
      Make_Name             (Any_Unity_Discrete, "_err_discrete_type");

      Any_Unity_Int := New_Unity_Entity;
      Set_Ekind             (Any_Unity_Int, E_Signed_Integer_Type);
      Set_Scope             (Any_Unity_Int, Unity_Standard);
      Set_Etype             (Any_Unity_Int, Standard_Short_Integer);
      Init_Size             (Any_Unity_Int, Standard_Short_Integer_Size);
      Set_Prim_Alignment    (Any_Unity_Int);

      Set_Integer_Bounds
        (Any_Unity_Int,
         Typ => Base_Type (Standard_Short_Integer),
         Lb  => Uint_0,
         Hb  => Intval (High_Bound (Scalar_Range (Standard_Short_Integer))));
      Make_Name (Any_Unity_Int, "_err_int_type");

      Any_Unity_Modular := New_Unity_Entity;
      Set_Ekind            (Any_Unity_Modular, E_Modular_Integer_Type);
      Set_Scope            (Any_Unity_Modular, Standard_Standard);
      Set_Etype            (Any_Unity_Modular, Standard_Long_Long_Integer);
      Init_Size            (Any_Unity_Modular, Standard_Long_Long_Integer_Size);
      Set_Prim_Alignment   (Any_Unity_Modular);
      Set_Is_Unsigned_Type (Any_Unity_Modular);
      Make_Name            (Any_Unity_Modular, "a modular type");

      Any_Unity_Numeric := New_Unity_Entity;
      Set_Ekind            (Any_Unity_Numeric, E_Signed_Integer_Type);
      Set_Scope            (Any_Unity_Numeric, Standard_Standard);
      Set_Etype            (Any_Unity_Numeric, Standard_Long_Long_Integer);
      Init_Size            (Any_Unity_Numeric, Standard_Long_Long_Integer_Size);
      Set_Prim_Alignment   (Any_Unity_Numeric);
      Make_Name            (Any_Unity_Numeric, "a numeric type");

      Any_Unity_Real := New_Unity_Entity;
      Set_Ekind             (Any_Unity_Real, E_Floating_Point_Type);
      Set_Scope             (Any_Unity_Real, Standard_Standard);
      Set_Etype             (Any_Unity_Real, Standard_Long_Long_Float);
      Init_Size             (Any_Unity_Real, Standard_Long_Long_Float_Size);
      Set_Prim_Alignment    (Any_Unity_Real);
      Make_Name             (Any_Unity_Real, "a real type");

      Any_Unity_Scalar := New_Unity_Entity;
      Set_Ekind             (Any_Unity_Scalar, E_Signed_Integer_Type);
      Set_Scope             (Any_Unity_Scalar, Standard_Standard);
      Set_Etype             (Any_Unity_Scalar, Any_Unity_Scalar);
      Init_Size             (Any_Unity_Scalar, Standard_Integer_Size);
      Set_Prim_Alignment    (Any_Unity_Scalar);
      Make_Name             (Any_Unity_Scalar, "a scalar type");

      Any_Unity_String := New_Unity_Entity;
      Set_Ekind             (Any_Unity_String, E_String_Type);
      Set_Scope             (Any_Unity_String, Standard_Standard);
      Set_Etype             (Any_Unity_String, Any_Unity_String);
      Set_Component_Type    (Any_Unity_String, Any_Unity_Char);
      Init_Size_Align       (Any_Unity_String);
      Make_Name             (Any_Unity_String, "a string type");

      --  Initialize visibility table with entities in Standard

      for E in Unity_Entity_Type loop
	 Set_Name_Entity_Id
	   (Chars (Unity_Entity (E)), Unity_Entity (E));
	 Set_Homonym (Unity_Entity (E), Empty);

	 Set_Scope (Unity_Entity (E), Unity_Standard);
	 Set_Is_Immediately_Visible (Unity_Entity (E));
      end loop;

      --  The predefined package Standard itself does not have a scope;
      --  it is the only entity in the system not to have one, and this
      --  is what identifies the package to Gigi.

      Set_Scope (Unity_Standard, Standard_Standard);

      --  Set global variables indicating last Id values and version

      Last_Unity_Standard_Node_Id := Last_Node_Id;
      Last_Unity_Standard_List_Id := Last_List_Id;

   end Create_Unity_Standard;

   --------------------
   -- Identifier_For --
   --------------------

   function Identifier_For (S : Unity_Entity_Type) return Node_Id is
      Ident_Node : Node_Id;

   begin
      Ident_Node := New_Node (N_Identifier, Stloc);
      Set_Chars (Ident_Node, Chars (Unity_Entity (S)));
      return Ident_Node;
   end Identifier_For;

   --------------------
   -- Make_Component --
   --------------------

   procedure Make_Component
     (Rec : Entity_Id;
      Typ : Entity_Id;
      Nam : String)
   is
      Id : constant Entity_Id := New_Unity_Entity;

   begin
      Set_Ekind                 (Id, E_Component);
      Set_Etype                 (Id, Typ);
      Set_Scope                 (Id, Rec);
      Init_Component_Location   (Id);

      Set_Original_Record_Component (Id, Id);
      Make_Name (Id, Nam);
      Append_Entity (Id, Rec);
   end Make_Component;

   -----------------
   -- Make_Formal --
   -----------------

   function Make_Formal
     (Typ         : Entity_Id;
      Formal_Name : String) return Entity_Id
   is
      Formal : Entity_Id;

   begin
      Formal := New_Unity_Entity;

      Set_Ekind     (Formal, E_In_Parameter);
      Set_Mechanism (Formal, Default_Mechanism);
      Set_Scope     (Formal, Unity_Standard);
      Set_Etype     (Formal, Typ);
      Make_Name     (Formal, Formal_Name);

      return Formal;
   end Make_Formal;

   ------------------
   -- Make_Integer --
   ------------------

   function Make_Integer (V : Uint) return Node_Id is
      N : constant Node_Id := Make_Integer_Literal (Stloc, V);
   begin
      Set_Is_Static_Expression (N);
      return N;
   end Make_Integer;

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
   
   ----------------------
   -- Set_Float_Bounds --
   ----------------------

   procedure Set_Float_Bounds (Id  : Entity_Id) is
      L  : Node_Id;
      --  Low bound of literal value

      H  : Node_Id;
      --  High bound of literal value

      R  : Node_Id;
      --  Range specification

   begin
      --  Note: for the call from Cstand to initially create the types in
      --  Standard, Vax_Float will always be False. Circuitry in Sem_Vfpt
      --  will adjust these types appropriately in the Vax_Float case if
      --  a pragma Float_Representation (VAX_Float) is used.
      
      L := Real_Convert (IEEEX_First'Universal_Literal_String);
      H := Real_Convert	(IEEEX_Last'Universal_Literal_String);

      Set_Etype                (L, Id);
      Set_Is_Static_Expression (L);
      
      Set_Etype                (H, Id);
      Set_Is_Static_Expression (H);
      
      R := New_Node (N_Range, Stloc);
      Set_Low_Bound  (R, L);
      Set_High_Bound (R, H);
      Set_Includes_Infinities (R, True);
      Set_Scalar_Range (Id, R);
      Set_Etype (R, Id);
      Set_Parent (R, Id);
   end Set_Float_Bounds;

   -------------------------
   -- New_Standard_Entity --
   -------------------------

   function New_Unity_Entity
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
   end New_Unity_Entity;
   
   ------------------------
   -- Set_Integer_Bounds --
   ------------------------

   procedure Set_Integer_Bounds
     (Id  : Entity_Id;
      Typ : Entity_Id;
      Lb  : Uint;
      Hb  : Uint)
   is
      L : Node_Id;     -- Low bound of literal value
      H : Node_Id;     -- High bound of literal value
      R : Node_Id;     -- Range specification

   begin
      L := Make_Integer (Lb);
      H := Make_Integer (Hb);

      Set_Etype (L, Typ);
      Set_Etype (H, Typ);

      R := New_Node (N_Range, Stloc);
      Set_Low_Bound  (R, L);
      Set_High_Bound (R, H);
      Set_Scalar_Range (Id, R);
      Set_Etype (R, Typ);
      Set_Parent (R, Id);
      Set_Is_Unsigned_Type (Id, Lb >= 0);
   end Set_Integer_Bounds;
   
   ------------------------------
   -- Corresponding_Unity_Type --
   ------------------------------
   
   function Corresponding_Unity_Type (Typ : Entity_Id) return Entity_Id is
      
      Btype      : Entity_Id;
      Size       : Int;
      Unsigned   : Boolean;
      Target_Plc : Entity_Id;
   begin
      Put_Line
	("Corresponding_Unity_Type Begin " & Get_String (Chars (Typ)));
      pragma Assert (Is_Numeric_Type (Typ));
      
      Btype     := Base_Type (Typ);
      Size      := UI_To_Int (Esize (Btype));
      Unsigned  := Is_Unsigned_Type (Btype);
      
      Put_Line ("   Btype    " & Get_String (Chars (Btype)));
      Put_Line ("   Size     " & Size'Img);
      Put_Line ("   Unsigned " & Unsigned'Img);
      
      if Is_Floating_Point_Type (Btype) then
	 Target_Plc := Unity_Standard_Real;
	 
      elsif Is_Unsigned_Type (Btype) then
	 if Size <= 8 then
	    Target_Plc := Unity_Standard_Byte;
	 elsif Size <= 16 then
	    Target_Plc := Unity_Standard_Uint;
	 elsif Size <= 32 then
	    Target_Plc := Unity_Standard_Udint;
	 else
	    Target_Plc := Unity_Standard_Udint;
	 end if;
	 
      else
	 if Size <= 16 then
	    Target_Plc := Unity_Standard_Int;
	 elsif Size <= 32 then
	    Target_Plc := Unity_Standard_Dint;
	 else
	    Target_Plc := Unity_Standard_Dint;
	 end if;
      end if;	 
      
      Put_Line
	("Corresponding_Unity_Type End " & Get_String (Chars (Target_Plc)));
      return Target_Plc;
   end Corresponding_Unity_Type;
   
   --------------------------------------
   -- Unity_Nuneric_Converion_Function --
   --------------------------------------
   
   function Unity_Nuneric_Converion_Function
     (Source : Entity_Id;
      Target : Entity_Id) return Entity_Id is
      
      Source_Type : Entity_Id;
      Target_Type : Entity_Id;
   begin
      pragma Assert
	(Is_Numeric_Type (Source) and then Is_Numeric_Type (Target));
      
      Source_Type := Corresponding_Unity_Type (Source);
      Target_Type := Corresponding_Unity_Type (Target);
      
      if Source_Type = Unity_Standard_Byte Then
	 if
	   Target_Type = Unity_Standard_Byte  then return Empty; 
	 elsif
	   Target_Type = Unity_Standard_Int   then return Unity_Byte_To_Int;
	 elsif
	   Target_Type = Unity_Standard_Dint  then return Unity_Byte_To_Dint;
	 elsif
	   Target_Type = Unity_Standard_Uint  then return Unity_Byte_To_Uint;
	 elsif
	   Target_Type = Unity_Standard_Udint then return Unity_Byte_To_Udint;
	 elsif
	   Target_Type = Unity_Standard_Real  then return Unity_Byte_To_Real;
	   
	 else
	    return Empty;
	 end if;
	 
      elsif Source_Type = Unity_Standard_Int Then
	 
	 if
	   Target_Type = Unity_Standard_Byte then return Unity_Int_To_Byte;
	 elsif
	   Target_Type = Unity_Standard_Int  then return Empty;
	 elsif 
	   Target_Type = Unity_Standard_Dint  then return Unity_Int_To_Dint;
	 elsif
	   Target_Type = Unity_Standard_Uint  then return Unity_Int_To_Uint;
	 elsif
	   Target_Type = Unity_Standard_Udint then return Unity_Int_To_Udint;
	 elsif
	   Target_Type = Unity_Standard_Real  then return Unity_Int_To_Real;
	   
	 else
	    return Empty;
	 end if;
	 
      elsif Source_Type = Unity_Standard_Uint then
	 
	 if
	   Target_Type = Unity_Standard_Byte  then return Unity_Uint_To_Byte;
	 elsif 
	   Target_Type = Unity_Standard_Int   then return Unity_Uint_To_Int;
	 elsif
	   Target_Type = Unity_Standard_Uint  then return Empty;
	 elsif
	   Target_Type = Unity_Standard_Dint  then return Unity_Uint_To_Dint;
	 elsif
	   Target_Type = Unity_Standard_Udint then return Unity_Uint_To_Udint;
	 elsif
	   Target_Type = Unity_Standard_Real  then return Unity_Uint_To_Real;
	   
	 else
	    return Empty;
	 end if;

      elsif Source_Type = Unity_Standard_Dint then
	 if 
	   Target_Type = Unity_Standard_Byte   then return Unity_Dint_To_Byte;
	 elsif
	   Target_Type = Unity_Standard_Int    then return Unity_Dint_To_Int;
	 elsif
	   Target_Type = Unity_Standard_Uint   then return Unity_Dint_To_Uint;
	 elsif
	   Target_Type = Unity_Standard_Dint   then return Empty;
	 elsif
	   Target_Type = Unity_Standard_Udint  then return Unity_Dint_To_Udint;
	 elsif
	   Target_Type = Unity_Standard_Real   then return Unity_Dint_To_Real;
	   
	 else return Empty;
	 end if;
     
     
      elsif Source_Type = Unity_Standard_Udint then
	 if
	   Target_Type = Unity_Standard_Byte   then return Unity_Udint_To_Byte;
	 elsif
	    Target_Type = Unity_Standard_Int   then return Unity_Udint_To_Int;
	 elsif
	    Target_Type = Unity_Standard_Uint  then return Unity_Udint_To_Uint;
	 elsif 
	    Target_Type = Unity_Standard_Dint  then return Unity_Udint_To_Dint;
	 elsif 
	    Target_Type = Unity_Standard_UDint then return Empty;
	 elsif 
	    Target_Type = Unity_Standard_Real  then return Unity_Udint_To_Real;
	   
	 else
	    return Empty;
	 end if;
     
      elsif Source_Type = Unity_Standard_Real then
	 if
	   Target_Type = Unity_Standard_Byte  then return Unity_Real_To_Byte;
	 elsif
	   Target_Type = Unity_Standard_Int   then return Unity_Real_To_Int;
	 elsif
	   Target_Type = Unity_Standard_Uint  then return Unity_Real_To_Uint;
	 elsif
	   Target_Type = Unity_Standard_Dint  then return Unity_Real_To_Dint;
	 elsif
	   Target_Type = Unity_Standard_Udint then return Unity_Real_To_Udint;
	 elsif
	   Target_Type = Unity_Standard_Real  then return Empty;
	   
	 else 
	    return Empty;
	 end if;
	 
      else
	 null;
      end if;
      
      return Empty;
   end Unity_Nuneric_Converion_Function;
   
end Unity_Standard_Lib;
