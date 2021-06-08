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

with Ada.Text_Io; use Ada.Text_IO;

with Types; use Types;
with Ttypes; use Ttypes;

with Artics.Buffers; use Artics.Buffers;

package Glips.Gen.Fat_Pointers is
   
   In_Fatptr_Constructor_Call : Boolean := False;
   --  True if we are generating code invoking a fatptr constructor
   
   type Bound_Kind is (Low, High);
   --  Used to specify the bound value writen by Write_Array_Bound
   
   function Has_Fat_Pointer (Typ : Entity_Id) return Boolean;
   --  Return True if Typ is an unconstrained array type or an access to an
   --  unconstrained array type.

   function Is_Array_Formal (N : Node_Id) return Boolean;
   function Is_Constrained_Array_Type (E : Entity_Id) return Boolean;
   function Is_Unconstrained_Array_Formal (N : Node_Id) return Boolean;
   function Is_Unconstrained_Array_Type (E : Entity_Id) return Boolean;
   function Is_Unidimensional_Array_Type (E : Entity_Id) return Boolean;

   procedure Write_Attr_Index
     (Ob         : output_buffer;
      Array_Type : Entity_Id; 
      Dimension  : Pos);
   --  Output the reference the Nth attribute of the fat pointer of a
   --  multidimensional array type.

   procedure Write_Fatptr_Bounds
     (This       : access Glips_Generator_Record;
      Array_Node : Node_Id;
      Bound      : Bound_Kind;
      Dimension  : Pos);
   --  Output the Bound of the given Dimension of a fat pointer
   
   procedure Write_Range_Bounds
     (This  : access Glips_Generator_Record;
      Bound : Bound_Kind;
      Rng   : Node_Id);
   --  Output the Bound of the given Dimension of a range expression
   
   procedure Write_Type_Bounds
     (This      : access Glips_Generator_Record;
      Array_Typ : Entity_Id;
      Bound     : Bound_Kind;
      Dimension : Pos);
   --  Output the Bound of the given Dimension of an array type
   
   procedure Write_Bound
     (This       : access Glips_Generator_Record;
      Array_Node : Node_Id;
      Bound      : Bound_Kind;
      Dimension  : Pos);
   --  Output the Bound of the given Dimension of Array_Node
   
   procedure Write_Array_Bound
     (This      : access Glips_Generator_Record;
      Expr      : Node_Id;
      Bound     : Bound_Kind;
      Dimension : Pos);
   --  Output the low bound or high bound of the given dimension of the fat
   --  pointer or array available through Expr.

   procedure Write_Fatptr_Bounds
     (This : access Glips_Generator_Record;
      Expr : Node_Id; 
      Typ  : Entity_Id);
   --  Output the low and high bounds of all the dimensions of the array
   --  Expr separated by commas: {low-bound-N ,high-bound-N}

   procedure Write_Fatptr_Compare
     (This : access Glips_Generator_Record;
      Lhs  : Node_Id; 
      Rhs  : Node_Id);
   --  Output code which compares the fat pointers associated with Lhs and
   --  Rhs expressions. The comparison of fat pointers with constrained
   --  arrays is supported.

   procedure Write_Fatptr_Declare
     (This       : access Glips_Generator_Record;
      Array_Type : Entity_Id);
   --  Output the typedef declaration of a multidimensional unconstrained
   --  array types.

   procedure Write_Fatptr_Dereference (Ob : Output_Buffer);

   --  Output a dereference of the fat pointer contents (i.e. ".all")

   procedure Write_Fatptr_Indexed_Component
     (This : access Glips_Generator_Record;
      N    : Node_Id);
   --  N is an explicit dereference of a multidimensional unconstrained
   --  array type. Output code which displaces the pointer to reference the
   --  array element.

   procedure Write_Fatptr_First
     (Ob         : Output_Buffer;
      Array_Type : Entity_Id; 
      Dimension  : Pos);
   procedure Write_Fatptr_Last
     (Ob         : Output_Buffer;
      Array_Type : Entity_Id; 
      Dimension  : Pos);
   --  Output a reference to the fat pointer field holding the value of the
   --  First/Last Dimension of Array_Type.
   
   procedure Write_Unconstrained_Array_Prefix
     (This : access Glips_Generator_Record;
      N    : Node_Id);
   --  Given an unconstrained array expression N, write a reference to this
   --  array, ready to be used as part of indexing or slicing this array.

   procedure Write_Number_Of_Components
     (This       : access Glips_Generator_Record;
      Fatptr     : Node_Id;
      Array_Type : Entity_Id;
      Dimension  : Nat := 0);
   --  Output code which computes the number of components of Array_Type
   --  in the given Dimension. This routine is commonly used to generate
   --  code which displaces the pointer to the base of an array to point
   --  to a given indexed component. For example, for an array of 3x4x2,
   --  the output generated for dimension 1 computes 4x2=8, for dimension
   --  2 computes 2, and for dimension 3 generates no output. Therefore,
   --  it can be used to compute the total number of elements of an array
   --  passing the value Dimension = 0.

   procedure Write_Fatptr_Init
     (This          : access Glips_Generator_Record;
      Expr          : Node_Id;
      Typ           : Entity_Id;
      Use_Aggregate : Boolean := False);
   --  Output code which initializes the fat pointer associated with Typ
   --  using Expr. For unidimensional unconstrained arrays a call to the
   --  constructor function is generated (unless Use_Aggregate is True);
   --  for multidimensional unconstrained arrays an aggregate is generated.

   procedure Write_Fatptr_Name
     (This       : access Glips_Generator_Record;
      Array_Type : Entity_Id);
   --  Output the name of the fatptr typedef associated with the given
   --  unconstrained array type.
   
   procedure Write_Name_All (Ob : Output_Buffer);
   --  Output "all"

   procedure Write_Name_First (Ob : Output_Buffer);
   --  Output "first"

   procedure Write_Name_Last (Ob : Output_Buffer);
   --  Output "last"
   
   procedure Write_Name_All_Ptr (Ob : Output_Buffer);

end Glips.Gen.Fat_Pointers;
