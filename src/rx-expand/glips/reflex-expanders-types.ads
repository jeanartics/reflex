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

with Artics.Buffers; use Artics.Buffers;

with Types; use Types;
with Ttypes; use Ttypes;
with Namet; use Namet;
with Table;
with Uintp; use Uintp;
with Urealp; use Urealp;
with Gnat.HTable; use Gnat.HTable;

package Reflex.Expanders.Types is

   FLCache_N  : Node_Id := Empty;
   FLCache_FL : Physical_Line_Number;
   FLCache_LL : Physical_Line_Number;
   --  Cache for First_Line and Last_Line (N records last node for which any
   --  of these subprograms were called, FL and LL record the corresponding
   --  First and Last physical line numbers for this node).

   Freeze_Level : Int := 0;
   --  Keep track of freeze level (incremented on entry to freeze actions and
   --  decremented on exit). Used to know if we are within freeze actions.

   Last_Line_Printed : Physical_Line_Number;
   --  This keeps track of the physical line number of the last source line for
   --  which Write_Source_Lines has processed #line/source output.

   Next_Comment_Line_To_Print : Physical_Line_Number := 1;
   --  This keeps track of the physical line number of the last comment line
   --  for which Write_Comment_Lines has processed output.

   No_Physical_Line_Number : constant Physical_Line_Number :=
     Physical_Line_Number'Last;
   --  Used internally to indicate no line number available

   In_Main_Unit : Boolean := False;
   --  Indicates whether the current unit being processed is part of the
   --  main unit. If this is the case, output all code; otherwise, output
   --  only external declarations and types.

   Library_Level : Boolean := True;
   --  Indicates whether the current node is at library level

   In_Package_Body_Init : Boolean := False;
   --  Indicates whether the current node is located in the initialization of a
   --  package body.

   In_Search_Type_Ref : Boolean := False;
   --  Indicates whether we are unnesting types of nested subprograms

   Special_Elaboration_Code : Boolean := False;
   --  Indicates whether we are generating code for statements part of the
   --  elaboration code (outside an explicit 'begin ... end').

   Current_Elab_Entity : Node_Id := Empty;
   --  Current entity which needs to be elaborated. Only set when
   --  Special_Elaboration_Code is True.

   Current_Subp_Entity : Entity_Id := Empty;
   --  Current subprogram for which Output_One_Body is generating code

   In_Compound_Statement : Boolean := False;
   --  Indicates whether we are processing a compound statement and, if so,
   --  will generate different code if needed. This is used in particular to
   --  emit an if-statement as an if-expression.

   --  The following constants are used by Write_Uint. They are initialized as
   --  shown when Source_Dump is called:

   --  The following constants are used by Write_Uint. They are initialized as
   --  shown when Source_Dump is called:

   Ints  : Nat renames Standard_Integer_Size;
   Longs : Nat renames Standard_Long_Integer_Size;
   Lls   : Nat renames Standard_Long_Long_Integer_Size;
   --  Length in bits of int, long, long long

   LNegInt  : Uint; --  -(Uint_2 ** (ints - 1));
   LPosInt  : Uint; --  abs (LNegInt + 1);
   LNegLong : Uint; --  -(Uint_2 ** (longs - 1));
   LPosLong : Uint; --  abs (LNegLong + 1);
   LNegLL   : Uint; --  -(Uint_2 ** (lls - 1));
   LPosLL   : Uint; --  abs (LNegLL + 1);
   --  Bounds of int, long, long long

   LPosU    : Uint; --  (Uint_2 ** ints) - 1;
   LNegU    : Uint; --  -LPosU;
   LPosUL   : Uint; --  (Uint_2 ** longs) - 1;
   LNegUL   : Uint; --  -LPosUL;
   LPosULL  : Uint; --  (Uint_2 ** lls) - 1;
   LNegULL  : Uint; --  -LPosULL;
   --  Bounds of unsigned, long unsigned, long long unsigned

   type Header_Num is range 1 .. 4096;

   function Hash (N : Node_Id) return Header_Num;
   --  Simple Hash function for Node_Ids

   function Name_Hash (N : Name_Id) return Header_Num;
   --  Simple Hash function for Name_Ids

   package Enclosing_Subp_Table is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Entity_Id,
      No_Element => Empty,
      Key        => Node_Id,
      Hash       => Hash,
      Equal      => "=");
   --  Hash table of entities, to record the enclosing function on which the
   --  backend declares each entity.

   package Entity_Table is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => Node_Id,
      Hash       => Hash,
      Equal      => "=");
   --  Hash table of entities, to record which entity has been dumped already

   package Entity_Names_Table is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Name_Id,
      No_Element => No_Name,
      Key        => Node_Id,
      Hash       => Hash,
      Equal      => "=");
   --  Hash table of entities, to record the external name of the entiy, as
   --  entity could be renamed to avoid target names clash

   package Registered_Names_Table is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Node_Id,
      No_Element => Empty,
      Key        => Name_Id,
      Hash       => Name_Hash,
      Equal      => "=");
   --  Hash table of registered names, to avoid names clash

   package Elaboration_Table is new Table.Table
     (Table_Component_Type => Node_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 1024,
      Table_Increment      => 100,
      Table_Name           => "Elaboration_Table");
   --  Table of statements part of the current elaboration procedure

   package Macro_Table is new Table.Table
     (Table_Component_Type => Node_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 512,
      Table_Increment      => 100,
      Table_Name           => "Macro_Table");
   --  Table of macros part of the current scope

   procedure Initialize;
   --  Do initializations common to all generators.

end Reflex.Expanders.Types;
