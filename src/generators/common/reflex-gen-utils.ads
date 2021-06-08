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

with Artics.Buffers; use Artics.Buffers;

package Reflex.Gen.Utils is


   function To_XML (S : in String) return String;

   function Unquote (S : in String) return String;

   function Normalize_Comment (S : in String) return String;

   function To_Plc_Comment (S : in String) return String;

   function To_Plc_Var_Comment (S : in String) return String;

   procedure Append_Subprogram_Prefix (Spec : Node_Id);
   --  Append "_rx_" to the name if this is a library-level subprogram,
   --  so it can be invoked as a main subprogram from the bind module.

   function Check_Sloc (S : Source_Ptr) return Boolean;
   --  Return False if we are not in the current source file (e.g.
   --  instantiation, inlining).

   procedure Col_Check
     (Ob : Output_Buffer;
      N  : Nat);
   --  Check that at least N characters remain on current line, and if not,
   --  then start an extra line with two characters extra indentation for
   --  continuing text on the next line.

   function Compound_Statement_Compatible (L : List_Id) return Boolean;
   --  Return True if L contains only expressions or statements compatible
   --  with compound statements.

   procedure Ensure_New_Line (Ob : Output_Buffer);
   --  Ensure that we are the start of a newline with current indentation

   function First_Line (N : Node_Id) return Physical_Line_Number;
   --  Given a subtree, determines the first physical line number for any node
   --  in the subtree. Returns No_Physical_Line_Number if no value found.

   function Get_Type_Full_View (Id : Entity_Id) return Entity_Id;
   --  Return the full view of Id, or Id itself

   function Last_Line (N : Node_Id) return Physical_Line_Number;
   --  Given a subtree, determines the last physical line number for any node
   --  in the subtree. Returns No_Physical_Line_Number if no value found.

   procedure Get_First_Last_Line (N : Node_Id);
   --  Determines first and last physical line number for subtree N, placing
   --  the result in FLCache. Result is No_Physical_Line_Number if node N does
   --  not come from current source file.

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

   function Parens_Needed (N : Node_Id) return Boolean;
   --  Returns True if N is in a context where it is not known to be safe to
   --  leave an expression unparenthesized. This is conservative. False means
   --  is is definitely safe to leave out parens, True means that parens may
   --  be needed so they will be put in. Right now, the test is limited to
   --  being the right side of an assignment.

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
     (Ob      : Output_Buffer;
      N       : Node_Id;
      Attr    : Name_Id;
      Context : String := "");
   --  Called to output error string for given unimplemented attribute Attr,
   --  and post error message on node N. Append Context to the error message.

   function Unqual_Conv (Expr : Node_Id) return Node_Id;

   function Get_Inner_Loop_Node (Node : Node_Id) return Node_Id;

end Reflex.Gen.Utils;
