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

--  This package contains host independent type definitions which are used
--  in more than one unit in the compiler. They are gathered here for easy
--  reference, although in some cases the full description is found in the
--  relevant module which implements the definition. The main reason that they
--  are not in their "natural" specs is that this would cause a lot of inter-
--  spec dependencies, and in particular some awkward circular dependencies
--  would have to be dealt with.

--  WARNING: There is a C version of this package. Any changes to this source
--  file must be properly reflected in the C header file types.h declarations.

--  Note: the declarations in this package reflect an expectation that the host
--  machine has an efficient integer base type with a range at least 32 bits
--  2s-complement. If there are any machines for which this is not a correct
--  assumption, a significant number of changes will be required!

with System;
with Unchecked_Conversion;
with Unchecked_Deallocation;

package Artics.Types is
  

   -------------------------------
   -- General Use Integer Types --
   -------------------------------

   type Int is range -2 ** 31 .. +2 ** 31 - 1;
   --  Signed 32-bit integer

   subtype Nat is Int range 0 .. Int'Last;
   --  Non-negative Int values

   subtype Pos is Int range 1 .. Int'Last;
   --  Positive Int values

   type Word is mod 2 ** 32;
   --  Unsigned 32-bit integer

   type Short is range -32768 .. +32767;
   for Short'Size use 16;
   --  16-bit signed integer

   type Byte is mod 2 ** 8;
   for Byte'Size use 8;
   --  8-bit unsigned integer

   type size_t is mod 2 ** Standard'Address_Size;
   --  Memory size value, for use in calls to C routines

   --------------------------------------
   -- 8-Bit Character and String Types --
   --------------------------------------

   --  We use Standard.Character and Standard.String freely, since we are
   --  compiling ourselves, and we properly implement the required 8-bit
   --  character code as required in Ada 95. This section defines a few
   --  general use constants and subtypes.

--   EOF : constant Character := ASCII.SUB;
   --  The character SUB (16#1A#) is used in DOS and other systems derived
   --  from DOS (XP, NT etc) to signal the end of a text file. Internally
   --  all source files are ended by an EOF character, even on Unix systems.
   --  An EOF character acts as the end of file only as the last character
   --  of a source buffer, in any other position, it is treated as a blank
   --  if it appears between tokens, and as an illegal character otherwise.
   --  This makes life easier dealing with files that originated from DOS,
   --  including concatenated files with interspersed EOF characters.

--  subtype Graphic_Character is Character range ' ' .. '~';
   --  Graphic characters, as defined in ARM

--   subtype Line_Terminator is Character range ASCII.LF .. ASCII.CR;
   --  Line terminator characters (LF, VT, FF, CR). For further details,
   --  see the extensive discussion of line termination in the Sinput spec.

   --  subtype Upper_Half_Character is
   --    Character range Character'Val (16#80#) .. Character'Val (16#FF#);
   --  Characters with the upper bit set

   type Character_Ptr is access all Character;
   type String_Ptr    is access all String;
   --  Standard character and string pointers

   procedure Free is new Unchecked_Deallocation (String, String_Ptr);
   --  Procedure for freeing dynamically allocated String values

   subtype Big_String is String (Positive);
   type Big_String_Ptr is access all Big_String;
   --  Virtual type for handling imported big strings. Note that we should
   --  never have any allocators for this type, but we don't give a storage
   --  size of zero, since there are legitimate deallocations going on.

   function To_Big_String_Ptr is
     new Unchecked_Conversion (System.Address, Big_String_Ptr);
   --  Used to obtain Big_String_Ptr values from external addresses

   subtype Word_Hex_String is String (1 .. 8);
   --  Type used to represent Word value as 8 hex digits, with lower case
   --  letters for the alphabetic cases.

   function Get_Hex_String (W : Word) return Word_Hex_String;
   --  Convert word value to 8-character hex string

   -----------------------------
   -- Types for Atree Package --
   -----------------------------

   --  Node_Id values are used to identify nodes in the tree. They are
   --  subscripts into the Nodes table declared in package Atree. Note that
   --  the special values Empty and Error are subscripts into this table.
   --  See package Atree for further details.

   type Union_Id is new Int;
   --  The type in the tree for a union of possible ID values

   --  Note: it is also helpful for debugging purposes to make these ranges
   --  distinct. If a bug leads to misidentification of a value, then it will
   --  typically result in an out of range value and a Constraint_Error.

   List_Low_Bound : constant := -100_000_000;
   --  The List_Id values are subscripts into an array of list headers which
   --  has List_Low_Bound as its lower bound. This value is chosen so that all
   --  List_Id values are negative, and the value zero is in the range of both
   --  List_Id and Node_Id values (see further description below).

   List_High_Bound : constant := 0;
   --  Maximum List_Id subscript value. This allows up to 100 million list Id
   --  values, which is in practice infinite, and there is no need to check the
   --  range. The range overlaps the node range by one element (with value
   --  zero), which is used both for the Empty node, and for indicating no
   --  list. The fact that the same value is used is convenient because it
   --  means that the default value of Empty applies to both nodes and lists,
   --  and also is more efficient to test for.
   
   Node_Low_Bound : constant := 0;
   --  The tree Id values start at zero, because we use zero for Empty (to
   --  allow a zero test for Empty). Actual tree node subscripts start at 0
   --  since Empty is a legitimate node value.

   Node_High_Bound : constant := 099_999_999;
   --  Maximum number of nodes that can be allocated is 100 million, which
   --  is in practice infinite, and there is no need to check the range.
   type List_Id is range List_Low_Bound .. List_High_Bound;
   --  Type used to identify a node list

   -- No_List : constant List_Id := List_High_Bound;
   --  Used to indicate absence of a list. Note that the value is zero, which
   --  is the same as Empty, which is helpful in initializing nodes where a
   --  value of zero can represent either an empty node or an empty list.

   Error_List : constant List_Id := List_Low_Bound;
   --  Used to indicate that there was an error in the source program in a
   --  context which would normally require a list. This node appears to be
   --  an empty list to the list operations (a null list is actually allocated
   --  which has this Id value).

   First_List_Id : constant List_Id := Error_List;
   --  Subscript of first allocated list header

   type Node_Id is range Node_Low_Bound .. Node_High_Bound;
   --  Type used to identify nodes in the tree

   subtype Entity_Id is Node_Id;
   --  A synonym for node types, used in the Einfo package to refer to nodes
   --  that are entities (i.e. nodes with an Nkind of N_Defining_xxx). All such
   --  nodes are extended nodes and these are the only extended nodes, so that
   --  in practice entity and extended nodes are synonymous.

   subtype Node_Or_Entity_Id is Node_Id;
   --  A synonym for node types, used in cases where a given value may be used
   --  to represent either a node or an entity. We like to minimize such uses
   --  for obvious reasons of logical type consistency, but where such uses
   --  occur, they should be documented by use of this type.

   Empty   : constant Node_Id := Node_Low_Bound;
   No_Node : constant Node_Id := Empty;
   --  Used to indicate null node. A node is actually allocated with this
   --  Id value, so that Nkind (Empty) = N_Empty. Note that Node_Low_Bound
   --  is zero, so Empty = No_List = zero.

   Empty_List_Or_Node : constant := 0;
   --  This constant is used in situations (e.g. initializing empty fields)
   --  where the value set will be used to represent either an empty node or
   --  a non-existent list, depending on the context.

   Error : constant Node_Id := Node_Low_Bound + 1;
   --  Used to indicate an error in the source program. A node is actually
   --  allocated with this Id value, so that Nkind (Error) = N_Error.

   Empty_Or_Error : constant Node_Id := Error;
   --  Since Empty and Error are the first two Node_Id values, the test for
   --  N <= Empty_Or_Error tests to see if N is Empty or Error. This definition
   --  provides convenient self-documentation for such tests.

   First_Node_Id  : constant Node_Id := Node_Low_Bound;
   --  Subscript of first allocated node. Note that Empty and Error are both
   --  allocated nodes, whose Nkind fields can be accessed without error.

   Names_Low_Bound : constant := 300_000_000;
   --  Low bound for name Id values

   Names_High_Bound : constant := 399_999_999;
   --  Maximum number of names that can be allocated is 100 million, which is
   --  in practice infinite and there is no need to check the range.
   
   type Name_Id is range Names_Low_Bound .. Names_High_Bound;
   for Name_Id'Size use 32;

   type User_Infos is record
       Owner : Node_Id := No_Node;
       -- first entity node named by this name_id
       Back_End_Info : Integer := 0;
       -- used by Pl7.Names
   end record;

   User_Default : constant User_Infos := (No_Node, 0);
   
   -------------------------
   -- Character Code Type --
   -------------------------

   --  The type Char is used for character data internally in the compiler, but
   --  character codes in the source are represented by the Char_Code type.
   --  Each character literal in the source is interpreted as being one of the
   --  16#7FFF_FFFF# possible Wide_Wide_Character codes, and a unique Integer
   --  value is assigned, corresponding to the UTF-32 value, which also
   --  corresponds to the Pos value in the Wide_Wide_Character type, and also
   --  corresponds to the Pos value in the Wide_Character and Character types
   --  for values that are in appropriate range. String literals are similarly
   --  interpreted as a sequence of such codes.

   type Char_Code_Base is mod 2 ** 32;
   for Char_Code_Base'Size use 32;

   subtype Char_Code is Char_Code_Base range 0 .. 16#7FFF_FFFF#;
   for Char_Code'Value_Size use 32;
   for Char_Code'Object_Size use 32;

   function Get_Char_Code (C : Character) return Char_Code;
   pragma Inline (Get_Char_Code);
   --  Function to obtain internal character code from source character. For
   --  the moment, the internal character code is simply the Pos value of the
   --  input source character, but we provide this interface for possible
   --  later support of alternative character sets.

   function In_Character_Range (C : Char_Code) return Boolean;
   pragma Inline (In_Character_Range);
   --  Determines if the given character code is in range of type Character,
   --  and if so, returns True. If not, returns False.

   function In_Wide_Character_Range (C : Char_Code) return Boolean;
   pragma Inline (In_Wide_Character_Range);
   --  Determines if the given character code is in range of the type
   --  Wide_Character, and if so, returns True. If not, returns False.

   function Get_Character (C : Char_Code) return Character;
   pragma Inline (Get_Character);
   --  For a character C that is in Character range (see above function), this
   --  function returns the corresponding Character value. It is an error to
   --  call Get_Character if C is not in Character range.

   function Get_Wide_Character (C : Char_Code) return Wide_Character;
   --  For a character C that is in Wide_Character range (see above function),
   --  this function returns the corresponding Wide_Character value. It is an
   --  error to call Get_Wide_Character if C is not in Wide_Character range.

   -----------------------------------
   -- Global Exception Declarations --
   -----------------------------------

   --  This section contains declarations of exceptions that are used
   --  throughout the compiler or in other GNAT tools.

   Unrecoverable_Error : exception;
   --  This exception is raised to immediately terminate the compilation of the
   --  current source program. Used in situations where things are bad enough
   --  that it doesn't seem worth continuing (e.g. max errors reached, or a
   --  required file is not found). Also raised when the compiler finds itself
   --  in trouble after an error (see Comperr).

   Terminate_Program : exception;
   --  This exception is raised to immediately terminate the tool being
   --  executed. Each tool where this exception may be raised must have a
   --  single exception handler that contains only a null statement and that is
   --  the last statement of the program. If needed, procedure Set_Exit_Status
   --  is called with the appropriate exit status before raising
   --  Terminate_Program.

   Not_Implemented : exception;
   
end Artics.Types;
