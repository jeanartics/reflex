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

--  This package contains the declarations of entities in package Standard,
--  These values are initialized either by calling CStand.Create_Standard,
--  or by calling Stand.Tree_Read.

with Types; use Types;
with Stand; use Stand;

--  Do we really need the with of Namet?

pragma Warnings (Off);
with Namet; use Namet;
pragma Warnings (On);

package Unity_Standard_Lib is

   type Unity_Entity_Type is
     (
      --  This enumeration type contains an entry for each name in Standard

      --  Package names

      U_Unity_Standard,

      --  Types defined in package Standard

      U_Bool,
      U_Ebool,
      U_Char,
      U_Wide_Character,
      U_String,
      U_Wide_String,
      U_Duration,

      U_Byte,
      U_Int,
      U_Dint,
      U_Uint,
      U_Udint,
      U_Real,

      --  Enumeration literals for type Boolean

      U_False,
      U_True,

      --  Exceptions declared in package Standard

      U_Constraint_Error,
      U_Numeric_Error,
      U_Program_Error,
      U_Storage_Error,
      U_Tasking_Error,

      --  Numeric Converion function

      U_Byte_To_Int,
      U_Byte_To_Uint,
      U_Byte_To_Dint,
      U_Byte_To_Udint,
      U_Byte_To_Real,

      U_Int_To_Byte,
      U_Int_To_Uint,
      U_Int_To_Dint,
      U_Int_To_Udint,
      U_Int_To_Real,

      U_Uint_To_Byte,
      U_Uint_To_Int,
      U_Uint_To_Dint,
      U_Uint_To_Udint,
      U_Uint_To_Real,

      U_Dint_To_Byte,
      U_Dint_To_Int,
      U_Dint_To_Uint,
      U_Dint_To_Udint,
      U_Dint_To_Real,

      U_Udint_To_Byte,
      U_Udint_To_Int,
      U_Udint_To_Uint,
      U_Udint_To_Dint,
      U_Udint_To_Real,

      U_Real_To_Byte,
      U_Real_To_Int,
      U_Real_To_Uint,
      U_Real_To_Dint,
      U_Real_To_Udint,

      --  Binary Operators declared in package Standard.

      U_Op_Concat,
      U_Op_Mod,
      U_Op_Rem,
      U_Op_Xor,

      --  Unary operators declared in package Standard

      U_Op_Abs,
      U_Op_Minus,
      U_Op_Not,
      U_Op_Plus,

      --  And one more control character, all on its own

      U_Dummy);

   subtype Unity_Standard_Types is
     Unity_Entity_Type range U_Bool .. U_Real;

   subtype Unity_Standard_Conversion_Functions is
     Unity_Entity_Type range U_Byte_To_Int .. U_Real_To_Udint;

   type Unity_Entity_Array_Type is array (Unity_Entity_Type) of Node_Id;

   Unity_Entity : Unity_Entity_Array_Type;
   --  This array contains pointers to the Defining Identifier nodes
   --  for each of the entities defined in Standard_Entities_Type. It
   --  is initialized by the Create_Standard procedure.

   Unity_Package_Node : Node_Id;
   --  Points to the N_Package_Declaration node for standard. Also
   --  initialized by the Create_Standard procedure.

   --  The following Entities are the pointers to the Defining Identifier
   --  nodes for some visible entities defined in Standard_Entities_Type.

   UE : Unity_Entity_Array_Type renames Unity_Entity;

   Unity_Standard          : Entity_Id renames UE (U_Unity_Standard);

   Unity_Standard_Bool     : Entity_Id renames UE (U_Bool);
   Unity_Standard_False    : Entity_Id renames UE (U_False);
   Unity_Standard_True     : Entity_Id renames UE (U_True);
   Unity_Standard_Ebool    : Entity_Id renames UE (U_Ebool);
   Unity_Standard_Char     : Entity_Id renames UE (U_Char);
   Unity_Standard_String   : Entity_Id renames UE (U_String);
   Unity_Standard_Duration : Entity_Id renames UE (U_Duration);
   Unity_Standard_Byte     : Entity_Id renames UE (U_Byte);
   Unity_Standard_Int      : Entity_Id renames UE (U_Int);
   Unity_Standard_Dint     : Entity_Id renames UE (U_Dint);
   Unity_Standard_Uint     : Entity_Id renames UE (U_Uint);
   Unity_Standard_Udint    : Entity_Id renames UE (U_Udint);
   Unity_Standard_Real     : Entity_Id renames UE (U_Real);
   Unity_Constraint_Error  : Entity_Id renames UE (U_Constraint_Error);
   Unity_Numeric_Error     : Entity_Id renames UE (U_Numeric_Error);
   Unity_Program_Error     : Entity_Id renames UE (U_Program_Error);
   Unity_Tasking_Error     : Entity_Id renames UE (U_Tasking_Error);
   Unity_Byte_To_Int       : Entity_Id renames UE (U_Byte_To_Int);
   Unity_Byte_To_Uint      : Entity_Id renames UE (U_Byte_To_Uint);
   Unity_Byte_To_Dint      : Entity_Id renames UE (U_Byte_To_Dint);
   Unity_Byte_To_Udint     : Entity_Id renames UE (U_Byte_To_Udint);
   Unity_Byte_To_Real      : Entity_Id renames UE (U_Byte_To_Real);
   Unity_Int_To_Byte       : Entity_Id renames UE (U_Int_To_Byte);
   Unity_Int_To_Uint       : Entity_Id renames UE (U_Int_To_Uint);
   Unity_Int_To_Dint       : Entity_Id renames UE (U_Int_To_Dint);
   Unity_Int_To_Udint      : Entity_Id renames UE (U_Int_To_Udint);
   Unity_Int_To_Real       : Entity_Id renames UE (U_Int_To_Real);
   Unity_Uint_To_Byte      : Entity_Id renames UE (U_Uint_To_Byte);
   Unity_Uint_To_Int       : Entity_Id renames UE (U_Uint_To_Int);
   Unity_Uint_To_Dint      : Entity_Id renames UE (U_Uint_To_Dint);
   Unity_Uint_To_Udint     : Entity_Id renames UE (U_Uint_To_Udint);
   Unity_Uint_To_Real      : Entity_Id renames UE (U_Uint_To_Real);
   Unity_Dint_To_Byte      : Entity_Id renames UE (U_Dint_To_Byte);
   Unity_Dint_To_Int       : Entity_Id renames UE (U_Dint_To_Int);
   Unity_Dint_To_Uint      : Entity_Id renames UE (U_Dint_To_Uint);
   Unity_Dint_To_Udint     : Entity_Id renames UE (U_Dint_To_Udint);
   Unity_Dint_To_Real      : Entity_Id renames UE (U_Dint_To_Real);
   Unity_Udint_To_Byte     : Entity_Id renames UE (U_Udint_To_Byte);
   Unity_Udint_To_Int      : Entity_Id renames UE (U_Udint_To_Int);
   Unity_Udint_To_Uint     : Entity_Id renames UE (U_Udint_To_Uint);
   Unity_Udint_To_Dint     : Entity_Id renames UE (U_Udint_To_Dint);
   Unity_Udint_To_Real     : Entity_Id renames UE (U_Udint_To_Real);
   Unity_Real_To_Byte      : Entity_Id renames UE (U_Real_To_Byte);
   Unity_Real_To_Int       : Entity_Id renames UE (U_Real_To_Int);
   Unity_Real_To_Uint      : Entity_Id renames UE (U_Real_To_Uint);
   Unity_Real_To_Dint      : Entity_Id renames UE (U_Real_To_Dint);
   Unity_Real_To_Udint     : Entity_Id renames UE (U_Real_To_Udint);
   Unity_Op_Concat         : Entity_Id renames UE (U_Op_Concat);
   Unity_Op_Mod            : Entity_Id renames UE (U_Op_Mod);
   Unity_Op_Rem            : Entity_Id renames UE (U_Op_Rem);
   Unity_Op_Xor            : Entity_Id renames UE (U_Op_Xor);
   Unity_Op_Abs            : Entity_Id renames UE (U_Op_Abs);
   Unity_Op_Minus          : Entity_Id renames UE (U_Op_Minus);
   Unity_Op_Not            : Entity_Id renames UE (U_Op_Not);
   Unity_Op_Plus           : Entity_Id renames UE (U_Op_Plus);



   Last_Unity_Standard_Node_Id : Node_Id;
   --  Highest Node_Id value used by Standard

   Last_Unity_Standard_List_Id : List_Id;
   --  Highest List_Id value used by Standard (including those used by
   --  normal list headers, element list headers, and list elements)

   -------------------------------------
   -- Semantic Phase Special Entities --
   -------------------------------------

   --  The semantic phase needs a number of entities for internal processing
   --  that are logically at the level of Standard, and hence defined in this
   --  package. However, they are never visible to a program, and are not
   --  chained on to the Decls list of Standard. The names of all these
   --  types are relevant only in certain debugging and error message
   --  situations. They have names that are suitable for use in such
   --  error messages (see body for actual names used).

   --  The entities labeled Any_Unity_xxx are used in situations where the full
   --  characteristics of an entity are not yet known, e.g. Any_Unity_Character
   --  is used to label a character literal before resolution is complete.
   --  These entities are also used to construct appropriate references in
   --  error messages ("expecting an integer type").

   Any_Unity_Id : Entity_Id;
   --  Used to represent some unknown identifier. Used to lable undefined
   --  identifier references to prevent cascaded errors.

   Any_Unity_Type : Entity_Id;
   --  Used to represent some unknown type. Plays an important role in
   --  avoiding cascaded errors, since any node that remains labaled with
   --  this type corresponds to an already issued error message. Any_Unity_Type
   --  is propagated to avoid cascaded errors from a single type error.

   Any_Unity_Access : Entity_Id;
   --  Used to resolve the overloaded literal NULL.

   Any_Unity_Array : Entity_Id;
   --  Used to represent some unknown array type

   Any_Unity_Bool : Entity_Id;
   --  The context type of conditions in IF and WHILE statements.

   Any_Unity_Char : Entity_Id;
   --  Any_Unity_Character is used to label character literals, which in
   --  general will not have an explicit declaration (this is true of the
   --  predefined character types).

   Any_Unity_Composite : Entity_Id;
   --  The type Any_Unity_Composite is used for aggregates before type
   --  resolution. It is compatible with any array or non-limited record type.

   Any_Unity_Discrete : Entity_Id;
   --  Used to represent some unknown discrete type

   Any_Unity_Int : Entity_Id;
   --  Used to represent some unknown integer type.

   Any_Unity_Modular : Entity_Id;
   --  Used to represent the result type of a boolean operation on an
   --  integer literal. The result is not Universal_Integer, because it is
   --  only legal in a modular context.

   Any_Unity_Numeric : Entity_Id;
   --  Used to represent some unknown numeric type.

   Any_Unity_Real : Entity_Id;
   --  Used to represent some unknown real type.

   Any_Unity_Scalar : Entity_Id;
   --  Used to represent some unknown scalar type

   Any_Unity_String : Entity_Id;
   --  The type Any_Unity_String is used for string literals before type
   --  resolution. It corresponds to array (Positive range <>) of character
   --  where the component type is compatible with any character type,
   --  not just Standard_Character.

   procedure Create_Unity_Standard;
   --  This procedure creates the tree for package standard, and initializes
   --  the Standard_Entities array and Standard_Package_Node. First the
   --  syntactic representation is created (as though the parser had parsed
   --  a copy of the source of Standard) and then semantic information is
   --  added as it would be by the semantic phases of the compiler. The
   --  tree is in the standard format defined by Syntax_Info, except that
   --  all Sloc values are set to Standard_Location except for nodes that
   --  are part of package ASCII, which have Sloc = Standard_ASCII_Location.
   --  The semantics info is in the format given by Entity_Info. The global
   --  variables Last_Standard_Node_Id and Last_Standard_List_Id are also set.

   function Corresponding_Unity_Type (Typ : Entity_Id) return Entity_Id;

   function Unity_Nuneric_Converion_Function
     (Source : Entity_Id;
      Target : Entity_Id) return Entity_Id;

end Unity_Standard_Lib;
