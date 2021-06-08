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
with Namet; use Namet;
with Einfo; use Einfo;

package Reflex.Expanders.Utils is

   procedure Append_Subprogram_Prefix (Spec : Node_Id);
   --  Append "_rx_" to the name if this is a library-level subprogram,
   --  so it can be invoked as a main subprogram from the bind module.

   function Compound_Statement_Compatible (L : List_Id) return Boolean;
   --  Return True if L contains only expressions or statements compatible
   --  with compound statements.

   function Get_Type_Full_View (Id : Entity_Id) return Entity_Id;
   --  Return the full view of Id, or Id itself

   function Has_Non_Null_Statements (L : List_Id) return Boolean;
   --  Return True if L has non null statements

   function Has_Or_Inherits_Enum_Rep_Clause (E : Entity_Id) return Boolean;
   --  Return True if the enumeration type E or some of its parents has an
   --  enumeration representation clause.

   function Has_Same_Int_Value
     (Val1 : Node_Id;
      Val2 : Node_Id) return Boolean;
   --  Return True if Val1 and Val2 represent the same integer value

   function In_Instantiation (S : Source_Ptr) return Boolean;
   --  Returns True if the source location corresponds with an instantiation

   function Is_Enum_Literal_Of_Enclosing_Subprogram
     (E : Entity_Id) return Boolean;
   --  Returns True if E is an enumeration literal whose enumeration type is
   --  defined in an enclosing subprogram.

   function Is_Out_Mode_Access_Formal (E : Node_Id) return Boolean;
   --  Returns True if E is an OUT or IN-OUT access formal

   function Is_Packed_Array (Typ : Entity_Id) return Boolean;
   --  Returns True if Typ is a packed array

   function Is_Supported_Variable_Size_Record (Typ : Entity_Id) return Boolean;
   --  Returns True if Typ is a record with discriminants whose last field is
   --  an array which depends on its discriminants.

   function Last_Field (Typ : Node_Id) return Node_Id;
   --  Return the last field of a given record type

   function Pass_Pointer (Ent : Entity_Id) return Boolean;
   --  Ent is the entity for a formal parameter. This function returns True if
   --  the corresponding object must be passed by using a pointer in C (i.e. by
   --  adding * in the definition of the formal, and & for calls). This is True
   --  for OUT and IN OUT parameters and for by-ref types.
   --  Note that it is never True for arrays, since in C, arrays are always
   --  passed in pointer form in any case.

   function Requires_Address (Typ : Node_Id) return Boolean;
   --  Return True if an object of type Typ should have its address taken when
   --  referencing it (to e.g. call memcmp() or memcmp()).

   function Ultimate_Expression (N : Node_Id) return Node_Id;
   --  Return the innermost expression of the given qualified expression, type
   --  conversion, or unchecked type conversion N.

   procedure Unimplemented_Attribute
     (N       : Node_Id;
      Attr    : Name_Id;
      Context : String := "");
   --  Called to output error string for given unimplemented attribute Attr,
   --  and post error message on node N. Append Context to the error message.

   function Unqual_Conv (Expr : Node_Id) return Node_Id;

   procedure Register_Entity (E : Entity_Id);

   function Search_Insertion_Node (Node : Node_Id) return Node_Id;

   function Enclosing_Package_Entity (N : Node_Id) return Entity_Id;

   function Enclosing_Subprogram_Body (N : Node_Id) return Node_Id;

   function Enclosing_Subprogram_Body_Declarations
     (N : Node_Id) return List_Id;

   procedure Declare_Enclosing_Subprogram
     (Node : Node_Id;
      Decl : Node_Id);

   function New_Ghost_Entity
     (Kind       : Entity_Kind;
      Sloc_Value : Source_Ptr) return Entity_Id;

   function Build_Ghost_Access_Object_Type
     (DT : Entity_Id;
      N  : Node_Id) return Entity_Id;

   function Create_Expanded_Variable
     (Node : Node_Id;
      Typ  : Entity_Id;
      Name : Name_Id) return Node_Id;

   procedure Declare_Body_Scope
     (Node : Node_Id;
      Decl : Node_Id);

   function Expression_Side_Effect_Free (N : Node_Id) return Boolean;

   procedure Remove_Expression_Side_Effect (N : Node_Id);

--   function Plc_Library_Scope (E : Entity_Id) return Boolean;

end Reflex.Expanders.Utils;
