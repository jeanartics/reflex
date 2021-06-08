------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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
-- Reflex is a fork from the GNAT compiler. GNAT was originally developed   --
-- by the GNAT team at  New York University. Extensive  contributions to    --
-- GNAT were provided by AdaCore Technologies Inc. Reflex is developed  by --
-- the Artics team at Grenoble.                                             --
--                                                                          --
------------------------------------------------------------------------------

with Types; use Types;
with Namet; use Namet;

package Snames is

--  This package contains definitions of standard names (i.e. entries in the
--  Names table) that are used throughout the GNAT compiler). It also contains
--  the definitions of some enumeration types whose definitions are tied to
--  the order of these preset names.

--  WARNING: There is a C file, a-snames.h which duplicates some of the
--  definitions in this file and must be kept properly synchronized.

   ------------------
   -- Preset Names --
   ------------------

   --  The following are preset entries in the names table, which are
   --  entered at the start of every compilation for easy access. Note
   --  that the order of initialization of these names in the body must
   --  be coordinated with the order of names in this table.

   --  Note: a name may not appear more than once in the following list.
   --  If additional pragmas or attributes are introduced which might
   --  otherwise cause a duplicate, then list it only once in this table,
   --  and adjust the definition of the functions for testing for pragma
   --  names and attribute names, and returning their ID values. Of course
   --  everything is simpler if no such duplications occur!

   --  First we have the one character names used to optimize the lookup
   --  process for one character identifiers (to avoid the hashing in this
   --  case) There are a full 256 of these, but only the entries for lower
   --  case and upper case letters have identifiers

   --  The lower case letter entries are used for one character identifiers
   --  appearing in the source, for example in pragma Interface (C).

   Name_A         : constant Name_Id := First_Name_Id + Character'Pos ('a');
   Name_B         : constant Name_Id := First_Name_Id + Character'Pos ('b');
   Name_C         : constant Name_Id := First_Name_Id + Character'Pos ('c');
   Name_D         : constant Name_Id := First_Name_Id + Character'Pos ('d');
   Name_E         : constant Name_Id := First_Name_Id + Character'Pos ('e');
   Name_F         : constant Name_Id := First_Name_Id + Character'Pos ('f');
   Name_G         : constant Name_Id := First_Name_Id + Character'Pos ('g');
   Name_H         : constant Name_Id := First_Name_Id + Character'Pos ('h');
   Name_I         : constant Name_Id := First_Name_Id + Character'Pos ('i');
   Name_J         : constant Name_Id := First_Name_Id + Character'Pos ('j');
   Name_K         : constant Name_Id := First_Name_Id + Character'Pos ('k');
   Name_L         : constant Name_Id := First_Name_Id + Character'Pos ('l');
   Name_M         : constant Name_Id := First_Name_Id + Character'Pos ('m');
   Name_N         : constant Name_Id := First_Name_Id + Character'Pos ('n');
   Name_O         : constant Name_Id := First_Name_Id + Character'Pos ('o');
   Name_P         : constant Name_Id := First_Name_Id + Character'Pos ('p');
   Name_Q         : constant Name_Id := First_Name_Id + Character'Pos ('q');
   Name_R         : constant Name_Id := First_Name_Id + Character'Pos ('r');
   Name_S         : constant Name_Id := First_Name_Id + Character'Pos ('s');
   Name_T         : constant Name_Id := First_Name_Id + Character'Pos ('t');
   Name_U         : constant Name_Id := First_Name_Id + Character'Pos ('u');
   Name_V         : constant Name_Id := First_Name_Id + Character'Pos ('v');
   Name_W         : constant Name_Id := First_Name_Id + Character'Pos ('w');
   Name_X         : constant Name_Id := First_Name_Id + Character'Pos ('x');
   Name_Y         : constant Name_Id := First_Name_Id + Character'Pos ('y');
   Name_Z         : constant Name_Id := First_Name_Id + Character'Pos ('z');

   --  The upper case letter entries are used by expander code for local
   --  variables that do not require unique names (e.g. formal parameter
   --  names in constructed procedures)

   Name_uA        : constant Name_Id := First_Name_Id + Character'Pos ('A');
   Name_uB        : constant Name_Id := First_Name_Id + Character'Pos ('B');
   Name_uC        : constant Name_Id := First_Name_Id + Character'Pos ('C');
   Name_uD        : constant Name_Id := First_Name_Id + Character'Pos ('D');
   Name_uE        : constant Name_Id := First_Name_Id + Character'Pos ('E');
   Name_uF        : constant Name_Id := First_Name_Id + Character'Pos ('F');
   Name_uG        : constant Name_Id := First_Name_Id + Character'Pos ('G');
   Name_uH        : constant Name_Id := First_Name_Id + Character'Pos ('H');
   Name_uI        : constant Name_Id := First_Name_Id + Character'Pos ('I');
   Name_uJ        : constant Name_Id := First_Name_Id + Character'Pos ('J');
   Name_uK        : constant Name_Id := First_Name_Id + Character'Pos ('K');
   Name_uL        : constant Name_Id := First_Name_Id + Character'Pos ('L');
   Name_uM        : constant Name_Id := First_Name_Id + Character'Pos ('M');
   Name_uN        : constant Name_Id := First_Name_Id + Character'Pos ('N');
   Name_uO        : constant Name_Id := First_Name_Id + Character'Pos ('O');
   Name_uP        : constant Name_Id := First_Name_Id + Character'Pos ('P');
   Name_uQ        : constant Name_Id := First_Name_Id + Character'Pos ('Q');
   Name_uR        : constant Name_Id := First_Name_Id + Character'Pos ('R');
   Name_uS        : constant Name_Id := First_Name_Id + Character'Pos ('S');
   Name_uT        : constant Name_Id := First_Name_Id + Character'Pos ('T');
   Name_uU        : constant Name_Id := First_Name_Id + Character'Pos ('U');
   Name_uV        : constant Name_Id := First_Name_Id + Character'Pos ('V');
   Name_uW        : constant Name_Id := First_Name_Id + Character'Pos ('W');
   Name_uX        : constant Name_Id := First_Name_Id + Character'Pos ('X');
   Name_uY        : constant Name_Id := First_Name_Id + Character'Pos ('Y');
   Name_uZ        : constant Name_Id := First_Name_Id + Character'Pos ('Z');

   --  Note: the following table is read by the utility program XSNAMES and
   --  its format should not be changed without coordinating with this program.

   N : constant Name_Id := First_Name_Id + 256;
   --  Synonym used in standard name definitions

   --  Some names that are used by gigi, and whose definitions are reflected
   --  in the C header file a-snames.h. They are placed at the start so that
   --  the need to modify a-snames.h is minimized.

   Name_uParent                        : constant Name_Id := N + 000;
   Name_uTag                           : constant Name_Id := N + 001;
   Name_Off                            : constant Name_Id := N + 002;
   Name_Space                          : constant Name_Id := N + 003;
   Name_Time                           : constant Name_Id := N + 004;
   
   --  Names of aspects for which there are no matching pragmas or attributes
   --  so that they need to be included for aspect specification use.

   Name_Dynamic_Predicate              : constant Name_Id := N + 005;
   Name_Post                           : constant Name_Id := N + 006;
   Name_Postcondition                  : constant Name_Id := N + 007; -- GNAT
   Name_Pre                            : constant Name_Id := N + 008;
   Name_Precondition                   : constant Name_Id := N + 009; -- GNAT
   Name_Predicate                      : constant Name_Id := N + 010; -- Ada 12
   Name_Predicate_Failure              : constant Name_Id := N + 011; -- Ada 12
   Name_Preelaborable_Initialization   : constant Name_Id := N + 012; -- Ada 05
   Name_Preelaborate                   : constant Name_Id := N + 013;
   Name_Static_Predicate               : constant Name_Id := N + 014;
   Name_Type_Invariant                 : constant Name_Id := N + 015;

   --  Some special names used by the expander. Note that the lower case u's
   --  at the start of these names get translated to extra underscores. These
   --  names are only referenced internally by expander generated code.

--   Name_uAbort_Signal                  : constant Name_Id := N + $;
   Name_uAlignment                     : constant Name_Id := N + 016;
   Name_uAssign                        : constant Name_Id := N + 017;
   Name_uChain                         : constant Name_Id := N + 018;
   Name_uClean                         : constant Name_Id := N + 019;
   Name_uController                    : constant Name_Id := N + 020;
   Name_uEntry_Bodies                  : constant Name_Id := N + 021;
   Name_uExpunge                       : constant Name_Id := N + 022;
   Name_uFinal_List                    : constant Name_Id := N + 023;
   Name_uIdepth                        : constant Name_Id := N + 024;
   Name_uInit                          : constant Name_Id := N + 025;
   Name_uLocal_Final_List              : constant Name_Id := N + 026;
   Name_uMaster                        : constant Name_Id := N + 027;
   Name_uObject                        : constant Name_Id := N + 028;
   Name_uPriority                      : constant Name_Id := N + 029;
   Name_uProcess_ATSD                  : constant Name_Id := N + 030;
   Name_uSecondary_Stack               : constant Name_Id := N + 031;
   Name_uService                       : constant Name_Id := N + 032;
   Name_uSize                          : constant Name_Id := N + 033;
   Name_uTags                          : constant Name_Id := N + 034;
--   Name_uTask                          : constant Name_Id := N + $;
--   Name_uTask_Id                       : constant Name_Id := N + $;
--   Name_uTask_Info                     : constant Name_Id := N + $;
--   Name_uTask_Name                     : constant Name_Id := N + $;
   Name_uTrace_Sp                      : constant Name_Id := N + 035;

   --  Names of routines in Ada.Finalization, needed by expander

   Name_Initialize                     : constant Name_Id := N + 036;
--   Name_Adjust                         : constant Name_Id := N + $;
   Name_Finalize                       : constant Name_Id := N + 037;

   --  Names of fields declared in System.Finalization_Implementation,
   --  needed by the expander when generating code for finalization.

   Name_Next                           : constant Name_Id := N + 038;
   Name_Prev                           : constant Name_Id := N + 039;

   --  Names of allocation routines, also needed by expander

   Name_Allocate                       : constant Name_Id := N + 040;
   Name_Deallocate                     : constant Name_Id := N + 041;
   Name_Dereference                    : constant Name_Id := N + 042;

   --  Names of Text_IO generic subpackages (see Rtsfind.Text_IO_Kludge)

   First_Text_IO_Package               : constant Name_Id := N + 043;
   Name_Decimal_IO                     : constant Name_Id := N + 043;
   Name_Enumeration_IO                 : constant Name_Id := N + 044;
   Name_Fixed_IO                       : constant Name_Id := N + 045;
   Name_Float_IO                       : constant Name_Id := N + 046;
   Name_Integer_IO                     : constant Name_Id := N + 047;
   Name_Modular_IO                     : constant Name_Id := N + 048;
   Last_Text_IO_Package                : constant Name_Id := N + 048;

   subtype Text_IO_Package_Name is Name_Id
     range First_Text_IO_Package .. Last_Text_IO_Package;

   --  Names of files in library for Ada.Text_IO and Ada.Wide_Text_IO

   Name_a_textio                       : constant Name_Id := N + 049;
   Name_a_witeio                       : constant Name_Id := N + 050;

   --  Some miscellaneous names used for error detection/recovery

   Name_Const                          : constant Name_Id := N + 051;
   Name_Error                          : constant Name_Id := N + 052;
   Name_Go                             : constant Name_Id := N + 053;
   Name_Put                            : constant Name_Id := N + 054;
   Name_Put_Line                       : constant Name_Id := N + 055;
   Name_To                             : constant Name_Id := N + 056;

   --  Names for packages that are treated specially by the compiler

   Name_Finalization                   : constant Name_Id := N + 057;
   Name_Finalization_Root              : constant Name_Id := N + 058;
   Name_Interfaces                     : constant Name_Id := N + 059;
   Name_Standard                       : constant Name_Id := N + 060;
   Name_System                         : constant Name_Id := N + 061;
   Name_Text_IO                        : constant Name_Id := N + 062;
   Name_Wide_Text_IO                   : constant Name_Id := N + 063;

   --  Names of identifiers used in expanding distribution stubs

   Name_Addr                           : constant Name_Id := N + 064;
   Name_Async                          : constant Name_Id := N + 065;
--   Name_Get_Active_Partition_ID        : constant Name_Id := N + $;
--   Name_Get_RCI_Package_Receiver       : constant Name_Id := N + $;
   Name_Origin                         : constant Name_Id := N + 066;
   Name_Params                         : constant Name_Id := N + 067;
   Name_Partition                      : constant Name_Id := N + 068;
   Name_Partition_Interface            : constant Name_Id := N + 069;
   Name_Ras                            : constant Name_Id := N + 070;
--   Name_RCI_Name                       : constant Name_Id := N + $;
--   Name_Receiver                       : constant Name_Id := N + $;
   Name_Result                         : constant Name_Id := N + 071;
--   Name_Rpc                            : constant Name_Id := N + $;
   Name_Subp_Id                        : constant Name_Id := N + 072;

   --  Operator Symbol entries. The actual names have an upper case O at
   --  the start in place of the Op_ prefix (e.g. the actual name that
   --  corresponds to Name_Op_Abs is "Oabs".

   First_Operator_Name                 : constant Name_Id := N + 073;
   Name_Op_Abs                         : constant Name_Id := N + 073; -- "abs"
   Name_Op_And                         : constant Name_Id := N + 074; -- "and"
   Name_Op_Mod                         : constant Name_Id := N + 075; -- "mod"
   Name_Op_Not                         : constant Name_Id := N + 076; -- "not"
   Name_Op_Or                          : constant Name_Id := N + 077; -- "or"
   Name_Op_Rem                         : constant Name_Id := N + 078; -- "rem"
   Name_Op_Xor                         : constant Name_Id := N + 079; -- "xor"
   Name_Op_Eq                          : constant Name_Id := N + 080; -- "="
   Name_Op_Ne                          : constant Name_Id := N + 081; -- "/="
   Name_Op_Lt                          : constant Name_Id := N + 082; -- "<"
   Name_Op_Le                          : constant Name_Id := N + 083; -- "<="
   Name_Op_Gt                          : constant Name_Id := N + 084; -- ">"
   Name_Op_Ge                          : constant Name_Id := N + 085; -- ">="
   Name_Op_Add                         : constant Name_Id := N + 086; -- "+"
   Name_Op_Subtract                    : constant Name_Id := N + 087; -- "-"
   Name_Op_Concat                      : constant Name_Id := N + 088; -- "&"
   Name_Op_Multiply                    : constant Name_Id := N + 089; -- "*"
   Name_Op_Divide                      : constant Name_Id := N + 090; -- "/"
   Name_Op_Expon                       : constant Name_Id := N + 091; -- "**"
   Last_Operator_Name                  : constant Name_Id := N + 091;

   --  Names for all pragmas recognized by GNAT. The entries with the comment
   --  "Ada 83" are pragmas that are defined in Ada 83, but not in Ada 95.
   --  These pragmas are fully implemented in both Ada 83 and Ada 95 modes
   --  in GNAT.

   --  The entries marked GNAT are pragmas that are defined by GNAT
   --  and implemented in both Ada 83 and Ada 95 modes. Full descriptions
   --  of these implementation dependent pragmas may be found in the
   --  appropriate section in unit Sem_Prag in file sem-prag.adb.

   --  The entries marked VMS are VMS specific pragmas that are recognized
   --  only in OpenVMS versions of GNAT. They are ignored in other versions
   --  with an appropriate warning.

   --  The entries marked AAMP are AAMP specific pragmas that are recognized
   --  only in GNAT for the AAMP. They are ignored in other versions with
   --  appropriate warnings.

   First_Pragma_Name                   : constant Name_Id := N + 092;

   --  Configuration pragmas are grouped at start

   --  Name_Ada_83                         : constant Name_Id := N + $; -- GNAT
   --  Name_Ada_95                         : constant Name_Id := N + $; -- GNAT
   Name_C_Pass_By_Copy                 : constant Name_Id := N + 092; -- GNAT
   Name_Compile_Time_Warning           : constant Name_Id := N + 093; -- GNAT
   Name_Component_Alignment            : constant Name_Id := N + 094; -- GNAT
   Name_Convention_Identifier          : constant Name_Id := N + 095; -- GNAT
   Name_Discard_Names                  : constant Name_Id := N + 096;
   Name_Elaboration_Checks             : constant Name_Id := N + 097; -- GNAT
   Name_Eliminate                      : constant Name_Id := N + 098; -- GNAT
   Name_Explicit_Overriding            : constant Name_Id := N + 099;
   Name_Extensions_Allowed             : constant Name_Id := N + 100; -- GNAT
   Name_External_Name_Casing           : constant Name_Id := N + 101; -- GNAT
   Name_Initialize_Scalars             : constant Name_Id := N + 102; -- GNAT
   Name_Long_Float                     : constant Name_Id := N + 103; -- VMS
   Name_No_Run_Time                    : constant Name_Id := N + 104; -- GNAT
   Name_Normalize_Scalars              : constant Name_Id := N + 105;
   Name_Persistent_Data                : constant Name_Id := N + 106; -- GNAT
   Name_Persistent_Object              : constant Name_Id := N + 107; -- GNAT
   Name_Ravenscar                      : constant Name_Id := N + 108;
   Name_Restricted_Run_Time            : constant Name_Id := N + 109;
   Name_Restrictions                   : constant Name_Id := N + 110;
   Name_Restriction_Warnings           : constant Name_Id := N + 111; -- GNAT
   Name_Style_Checks                   : constant Name_Id := N + 112; -- GNAT
   Name_Suppress                       : constant Name_Id := N + 113;
   Name_Test_Case                      : constant Name_Id := N + 114; -- GNAT
   Name_Universal_Data                 : constant Name_Id := N + 115; -- AAMP
   Name_Unsuppress                     : constant Name_Id := N + 116; -- GNAT
   Name_Validity_Checks                : constant Name_Id := N + 117; -- GNAT
   Name_Warnings                       : constant Name_Id := N + 118; -- GNAT
   Last_Configuration_Pragma_Name      : constant Name_Id := N + 118;

   --  Remaining pragma names

   Name_Annotate                       : constant Name_Id := N + 119; -- GNAT

   --  Note: AST_Entry is not in this list because its name matches the
   --  name of the corresponding attribute. However, it is included in the
   --  definition of the type Attribute_Id, and the functions Get_Pragma_Id
   --  and Check_Pragma_Id correctly recognize and process Name_AST_Entry.
   --  AST_Entry is a VMS specific pragma.

   Name_Assert                         : constant Name_Id := N + 120; -- GNAT
   Name_Asynchronous                   : constant Name_Id := N + 121;
   Name_Attach_Handler                 : constant Name_Id := N + 122; -- GNAT
   Name_Comment                        : constant Name_Id := N + 123; -- GNAT
   Name_Common_Object                  : constant Name_Id := N + 124; -- GNAT
   Name_Convention                     : constant Name_Id := N + 125;
   Name_Contract_Cases                 : constant Name_Id := N + 126; -- GNAT
   Name_Debug                          : constant Name_Id := N + 127; -- GNAT
   Name_Depends                        : constant Name_Id := N + 128; -- GNAT
   Name_Elaborate                      : constant Name_Id := N + 129; -- Ada 83
   Name_Elaborate_All                  : constant Name_Id := N + 130;
   Name_Elaborate_Body                 : constant Name_Id := N + 131;
   Name_Export                         : constant Name_Id := N + 132;
   Name_Export_Exception               : constant Name_Id := N + 133; -- VMS
   Name_Export_Function                : constant Name_Id := N + 134; -- GNAT
   Name_Export_Object                  : constant Name_Id := N + 135; -- GNAT
   Name_Export_Procedure               : constant Name_Id := N + 136; -- GNAT
   Name_External                       : constant Name_Id := N + 137; -- GNAT
   Name_Finalize_Storage_Only          : constant Name_Id := N + 138; -- GNAT
   Name_Global                         : constant Name_Id := N + 139; -- GNAT
   Name_Ident                          : constant Name_Id := N + 140; -- VMS
   Name_Import                         : constant Name_Id := N + 141;
   Name_Import_Exception               : constant Name_Id := N + 142; -- VMS
   Name_Import_Function                : constant Name_Id := N + 143; -- GNAT
   Name_Import_Object                  : constant Name_Id := N + 144; -- GNAT
   Name_Import_Procedure               : constant Name_Id := N + 145; -- GNAT
   Name_Initial_Condition              : constant Name_Id := N + 146; -- GNAT
   Name_Initializes                    : constant Name_Id := N + 147; -- GNAT
   Name_Inline                         : constant Name_Id := N + 148;
   Name_Inline_Always                  : constant Name_Id := N + 149; -- GNAT
   Name_Inline_Generic                 : constant Name_Id := N + 150; -- GNAT
   Name_Inspection_Point               : constant Name_Id := N + 151;
   Name_Interface                      : constant Name_Id := N + 152; -- Ada 83
   Name_Interface_Name                 : constant Name_Id := N + 153; -- GNAT
   Name_Invariant                      : constant Name_Id := N + 154; -- GNAT
   Name_Keep_Names                     : constant Name_Id := N + 155; -- GNAT
   Name_Link_With                      : constant Name_Id := N + 156; -- GNAT
   Name_Linker_Alias                   : constant Name_Id := N + 157; -- GNAT
   Name_Linker_Options                 : constant Name_Id := N + 158;
   Name_List                           : constant Name_Id := N + 159;
   Name_Memory_Size                    : constant Name_Id := N + 160; -- Ada 83
   Name_No_Return                      : constant Name_Id := N + 161; -- GNAT
   Name_Obsolescent                    : constant Name_Id := N + 162; -- GNAT
   Name_Optimize                       : constant Name_Id := N + 163;
   Name_Optional_Overriding            : constant Name_Id := N + 164;
   Name_Overriding                     : constant Name_Id := N + 165;
   Name_Pack                           : constant Name_Id := N + 166;
   Name_Pure                           : constant Name_Id := N + 167;
   Name_Pure_Function                  : constant Name_Id := N + 168; -- GNAT

   --  Note: Storage_Size is not in this list because its name matches the
   --  name of the corresponding attribute. However, it is included in the
   --  definition of the type Attribute_Id, and the functions Get_Pragma_Id
   --  and Check_Pragma_Id correctly recognize and process Name_Storage_Size.

   --  Note: Storage_Unit is also omitted from the list because of a clash
   --  with an attribute name, and is treated similarly.

   Name_Source_Reference               : constant Name_Id := N + 169; -- GNAT
   Name_Suppress_All                   : constant Name_Id := N + 170; -- GNAT
   Name_Suppress_Debug_Info            : constant Name_Id := N + 171; -- GNAT
   Name_Suppress_Initialization        : constant Name_Id := N + 172; -- GNAT
   Name_System_Name                    : constant Name_Id := N + 173; -- Ada 83
   Name_Unimplemented                  : constant Name_Id := N + 174; -- GNAT
   Name_Unmodified                     : constant Name_Id := N + 175; -- GNAT
   Name_Unreferenced                   : constant Name_Id := N + 176; -- GNAT
   Name_Unreferenced_Objects           : constant Name_Id := N + 177; -- GNAT
   Name_Volatile                       : constant Name_Id := N + 178;
   Name_Volatile_Components            : constant Name_Id := N + 179;
   Last_Pragma_Name                    : constant Name_Id := N + 179;

   --  Language convention names for pragma Convention/Export/Import/Interface
   --  Note that Name_C is not included in this list, since it was already
   --  declared earlier in the context of one-character identifier names
   --  (where the order is critical to the fast look up process).

   --  Note: there are no convention names corresponding to the conventions
   --  Entry and Protected, this is because these conventions cannot be
   --  specified by a pragma.

   First_Convention_Name               : constant Name_Id := N + 180;
   Name_Ada                            : constant Name_Id := N + 180;
   Name_Assembler                      : constant Name_Id := N + 181;
   Name_Intrinsic                      : constant Name_Id := N + 182;
   Name_Stdcall                        : constant Name_Id := N + 183;
   Name_Stubbed                        : constant Name_Id := N + 184;
   Last_Convention_Name                : constant Name_Id := N + 184;

   --  The following names are preset as synonyms for Assembler

   Name_Asm                            : constant Name_Id := N + 185;
   Name_Assembly                       : constant Name_Id := N + 186;

   --  The following names are preset as synonyms for C

   Name_Default                        : constant Name_Id := N + 187;
   --  Name_Exernal (previously defined as pragma)

   --  The following names are present as synonyms for Stdcall

   Name_DLL                            : constant Name_Id := N + 188;
   Name_Win32                          : constant Name_Id := N + 189;

   --  Other special names used in processing pragma arguments

   Name_As_Is                          : constant Name_Id := N + 190;
   Name_Body_File_Name                 : constant Name_Id := N + 191;
   Name_Casing                         : constant Name_Id := N + 192;
   Name_Code                           : constant Name_Id := N + 193;
   Name_Component                      : constant Name_Id := N + 194;
   Name_Component_Size_4               : constant Name_Id := N + 195;
   Name_Copy                           : constant Name_Id := N + 196;
   Name_D_Float                        : constant Name_Id := N + 197;
   Name_Descriptor                     : constant Name_Id := N + 198;
   Name_Dot_Replacement                : constant Name_Id := N + 199;
   Name_Dynamic                        : constant Name_Id := N + 200;
   Name_Entity                         : constant Name_Id := N + 201;
   Name_External_Name                  : constant Name_Id := N + 202;
   Name_First_Optional_Parameter       : constant Name_Id := N + 203;
   Name_Form                           : constant Name_Id := N + 204;
   Name_G_Float                        : constant Name_Id := N + 205;
   Name_Gcc                            : constant Name_Id := N + 206;
   Name_Gnat                           : constant Name_Id := N + 207;
   Name_GPL                            : constant Name_Id := N + 208;
   Name_IEEE_Float                     : constant Name_Id := N + 209;
   Name_Homonym_Number                 : constant Name_Id := N + 210;
   Name_Internal                       : constant Name_Id := N + 211;
   Name_Link_Name                      : constant Name_Id := N + 212;
   Name_Lowercase                      : constant Name_Id := N + 213;
   Name_Max_Size                       : constant Name_Id := N + 214;
   Name_Mechanism                      : constant Name_Id := N + 215;
   Name_Mixedcase                      : constant Name_Id := N + 216;
   Name_Modified_GPL                   : constant Name_Id := N + 217;
   Name_Name                           : constant Name_Id := N + 218;
   Name_NCA                            : constant Name_Id := N + 219;
   Name_No                             : constant Name_Id := N + 220;
   Name_On                             : constant Name_Id := N + 221;
   Name_Parameter_Types                : constant Name_Id := N + 222;
   Name_Reference                      : constant Name_Id := N + 223;
   Name_Restricted                     : constant Name_Id := N + 224;
   Name_Result_Mechanism               : constant Name_Id := N + 225;
   Name_Result_Type                    : constant Name_Id := N + 226;
   Name_Runtime                        : constant Name_Id := N + 227;
   Name_SB                             : constant Name_Id := N + 228;
   Name_Secondary_Stack_Size           : constant Name_Id := N + 229;
   Name_Section                        : constant Name_Id := N + 230;
   Name_Semaphore                      : constant Name_Id := N + 231;
   Name_Spec_File_Name                 : constant Name_Id := N + 232;
   Name_Static                         : constant Name_Id := N + 233;
   Name_Stack_Size                     : constant Name_Id := N + 234;
   Name_Subunit_File_Name              : constant Name_Id := N + 235;
   Name_Task_Stack_Size_Default        : constant Name_Id := N + 236;
   Name_Task_Type                      : constant Name_Id := N + 237;
   Name_Time_Slicing_Enabled           : constant Name_Id := N + 238;
   Name_Top_Guard                      : constant Name_Id := N + 239;
   Name_UBA                            : constant Name_Id := N + 240;
   Name_UBS                            : constant Name_Id := N + 241;
   Name_UBSB                           : constant Name_Id := N + 242;
   Name_Unit_Name                      : constant Name_Id := N + 243;
   Name_Unknown                        : constant Name_Id := N + 244;
   Name_Unrestricted                   : constant Name_Id := N + 245;
   Name_Uppercase                      : constant Name_Id := N + 246;
   Name_User                           : constant Name_Id := N + 247;
   Name_VAX_Float                      : constant Name_Id := N + 248;
   Name_VMS                            : constant Name_Id := N + 249;
   Name_Working_Storage                : constant Name_Id := N + 250;

   --  Names of recognized attributes. The entries with the comment "Ada 83"
   --  are attributes that are defined in Ada 83, but not in Ada 95. These
   --  attributes are implemented in both Ada 83 and Ada 95 modes in GNAT.

   --  The entries marked GNAT are attributes that are defined by GNAT
   --  and implemented in both Ada 83 and Ada 95 modes. Full descriptions
   --  of these implementation dependent attributes may be found in the
   --  appropriate section in package Sem_Attr in file sem-attr.ads.

   --  The entries marked VMS are recognized only in OpenVMS implementations
   --  of GNAT, and are treated as illegal in all other contexts.

   First_Attribute_Name                : constant Name_Id := N + 251;
   Name_Access                         : constant Name_Id := N + 251;
   Name_Address                        : constant Name_Id := N + 252;
   Name_Address_Size                   : constant Name_Id := N + 253;  -- GNAT
   Name_Alignment                      : constant Name_Id := N + 254;
   Name_Bit                            : constant Name_Id := N + 255;  -- GNAT
   Name_Bit_Position                   : constant Name_Id := N + 256;  -- GNAT
   Name_Component_Size                 : constant Name_Id := N + 257;
   Name_Constrained                    : constant Name_Id := N + 258;
   Name_Enum_Rep                       : constant Name_Id := N + 259; -- GNAT
   Name_Exponent                       : constant Name_Id := N + 260;
   Name_First                          : constant Name_Id := N + 261;
   Name_First_Bit                      : constant Name_Id := N + 262;
   Name_Img                            : constant Name_Id := N + 263; -- GNAT
   Name_Integer_Value                  : constant Name_Id := N + 264; -- GNAT
   Name_Large                          : constant Name_Id := N + 265; -- Ada 83
   Name_Last                           : constant Name_Id := N + 266;
   Name_Last_Bit                       : constant Name_Id := N + 267;
   Name_Length                         : constant Name_Id := N + 268;
   Name_Machine_Rounds                 : constant Name_Id := N + 269;
   Name_Machine_Size                   : constant Name_Id := N + 270; -- GNAT
   Name_Mantissa                       : constant Name_Id := N + 271; -- Ada 83
   Name_Max_Size_In_Storage_Elements   : constant Name_Id := N + 272;
   Name_Maximum_Alignment              : constant Name_Id := N + 273; -- GNAT
   Name_Modulus                        : constant Name_Id := N + 274;
   Name_Object_Size                    : constant Name_Id := N + 275; -- GNAT
   Name_Passed_By_Reference            : constant Name_Id := N + 276; -- GNAT
   Name_Pool_Address                   : constant Name_Id := N + 277;
   Name_Pos                            : constant Name_Id := N + 278;
   Name_Position                       : constant Name_Id := N + 279;
   Name_Range                          : constant Name_Id := N + 280;
   Name_Range_Length                   : constant Name_Id := N + 281; -- GNAT
   Name_Round                          : constant Name_Id := N + 282;
   Name_Scale                          : constant Name_Id := N + 283;
   Name_Signed_Zeros                   : constant Name_Id := N + 284;
   Name_Size                           : constant Name_Id := N + 285;
   Name_Storage_Size                   : constant Name_Id := N + 286;
   Name_Storage_Unit                   : constant Name_Id := N + 287; -- GNAT
   Name_Tag                            : constant Name_Id := N + 288;
   Name_Target_Name                    : constant Name_Id := N + 289; -- GNAT
   Name_Terminated                     : constant Name_Id := N + 290;
   Name_To_Address                     : constant Name_Id := N + 291; -- GNAT
   Name_Type_Class                     : constant Name_Id := N + 292; -- GNAT
   Name_Unchecked_Access               : constant Name_Id := N + 293;
   Name_Unconstrained_Array            : constant Name_Id := N + 294;
   Name_Universal_Literal_String       : constant Name_Id := N + 295; -- GNAT
   Name_Unrestricted_Access            : constant Name_Id := N + 296; -- GNAT
   Name_Val                            : constant Name_Id := N + 297;
   Name_Valid                          : constant Name_Id := N + 298;
   Name_Value_Size                     : constant Name_Id := N + 299; -- GNAT
   Name_Wchar_T_Size                   : constant Name_Id := N + 300; -- GNAT
   Name_Wide_Width                     : constant Name_Id := N + 301;
   Name_Width                          : constant Name_Id := N + 302;
   Name_Word_Size                      : constant Name_Id := N + 303; -- GNAT

   --  Attributes that designate attributes returning renamable functions,
   --  i.e. functions that return other than a universal value.

   First_Renamable_Function_Attribute  : constant Name_Id := N + 304;
   Name_Ceiling                        : constant Name_Id := N + 304;
   Name_Floor                          : constant Name_Id := N + 305;
   Name_Fraction                       : constant Name_Id := N + 306;
   Name_Image                          : constant Name_Id := N + 307;
   Name_Input                          : constant Name_Id := N + 308;
   Name_Machine                        : constant Name_Id := N + 309;
   Name_Max                            : constant Name_Id := N + 310;
   Name_Min                            : constant Name_Id := N + 311;
   Name_Model                          : constant Name_Id := N + 312;
   Name_Output                         : constant Name_Id := N + 313;
   Name_Remainder                      : constant Name_Id := N + 314;
   Name_Rounding                       : constant Name_Id := N + 315;
   Name_Succ                           : constant Name_Id := N + 316;
   Name_Truncation                     : constant Name_Id := N + 317;
   Name_Value                          : constant Name_Id := N + 318;
   Name_Wide_Image                     : constant Name_Id := N + 319;
   Name_Wide_Value                     : constant Name_Id := N + 320;
   Last_Renamable_Function_Attribute   : constant Name_Id := N + 320;

   --  Remaining attributes are ones that return entities

   First_Entity_Attribute_Name         : constant Name_Id := N + 321;
   Name_Storage_Pool                   : constant Name_Id := N + 321;

   --  These attributes are the ones that return types

   First_Type_Attribute_Name           : constant Name_Id := N + 322;
   Name_Base                           : constant Name_Id := N + 322;
   Name_Class                          : constant Name_Id := N + 323;
   Last_Type_Attribute_Name            : constant Name_Id := N + 323;
   Last_Entity_Attribute_Name          : constant Name_Id := N + 323;
   Last_Attribute_Name                 : constant Name_Id := N + 323;

   --  Names of recognized task dispatching policy identifiers

   --  Names of recognized checks for pragma Suppress

   First_Check_Name                    : constant Name_Id := N + 324;
   Name_Access_Check                   : constant Name_Id := N + 324;
   Name_Accessibility_Check            : constant Name_Id := N + 325;
   Name_Division_Check                 : constant Name_Id := N + 326;
   Name_Elaboration_Check              : constant Name_Id := N + 327;
   Name_Index_Check                    : constant Name_Id := N + 328;
   Name_Length_Check                   : constant Name_Id := N + 329;
   Name_Overflow_Check                 : constant Name_Id := N + 330;
   Name_Range_Check                    : constant Name_Id := N + 331;
   Name_Storage_Check                  : constant Name_Id := N + 332;
   Name_Tag_Check                      : constant Name_Id := N + 333;
   Name_All_Checks                     : constant Name_Id := N + 334;
   Last_Check_Name                     : constant Name_Id := N + 334;

   --  Names corresponding to reserved keywords, excluding those already
   --  declared in the attribute list (Access, Delta, Digits, Range).

   Name_Abort                          : constant Name_Id := N + 335;
   Name_Abs                            : constant Name_Id := N + 336;
   Name_And                            : constant Name_Id := N + 337;
   Name_All                            : constant Name_Id := N + 338;
   Name_Array                          : constant Name_Id := N + 339;
   Name_At                             : constant Name_Id := N + 340;
   Name_Begin                          : constant Name_Id := N + 341;
   Name_Body                           : constant Name_Id := N + 342;
   Name_Case                           : constant Name_Id := N + 343;
   Name_Constant                       : constant Name_Id := N + 344;
   Name_Declare                        : constant Name_Id := N + 345;
   Name_Delay                          : constant Name_Id := N + 346;
   Name_Do                             : constant Name_Id := N + 347;
   Name_Else                           : constant Name_Id := N + 348;
   Name_Elsif                          : constant Name_Id := N + 349;
   Name_End                            : constant Name_Id := N + 350;
   Name_Exception                      : constant Name_Id := N + 351;
   Name_Exit                           : constant Name_Id := N + 352;
   Name_For                            : constant Name_Id := N + 353;
   Name_Function                       : constant Name_Id := N + 354;
   Name_Generic                        : constant Name_Id := N + 355;
   Name_Goto                           : constant Name_Id := N + 356;
   Name_If                             : constant Name_Id := N + 357;
   Name_In                             : constant Name_Id := N + 358;
   Name_Is                             : constant Name_Id := N + 359;
   Name_Loop                           : constant Name_Id := N + 360;
   Name_Mod                            : constant Name_Id := N + 361;
   Name_New                            : constant Name_Id := N + 362;
   Name_Not                            : constant Name_Id := N + 363;
   Name_Null                           : constant Name_Id := N + 364;
   Name_Of                             : constant Name_Id := N + 365;
   Name_Or                             : constant Name_Id := N + 366;
   Name_Others                         : constant Name_Id := N + 367;
   Name_Out                            : constant Name_Id := N + 368;
   Name_Package                        : constant Name_Id := N + 369;
   Name_Pragma                         : constant Name_Id := N + 370;
   Name_Private                        : constant Name_Id := N + 371;
   Name_Procedure                      : constant Name_Id := N + 372;
   Name_Raise                          : constant Name_Id := N + 373;
   Name_Record                         : constant Name_Id := N + 374;
   Name_Rem                            : constant Name_Id := N + 375;
   Name_Renames                        : constant Name_Id := N + 376;
   Name_Return                         : constant Name_Id := N + 377;
   Name_Reverse                        : constant Name_Id := N + 378;
   Name_Subtype                        : constant Name_Id := N + 379;
   Name_Task                           : constant Name_Id := N + 380;
   Name_Terminate                      : constant Name_Id := N + 381;
   Name_Then                           : constant Name_Id := N + 382;
   Name_Type                           : constant Name_Id := N + 383;
   Name_Use                            : constant Name_Id := N + 384;
   Name_When                           : constant Name_Id := N + 385;
   Name_While                          : constant Name_Id := N + 386;
   Name_With                           : constant Name_Id := N + 387;
   Name_Xor                            : constant Name_Id := N + 388;

   --  Names of intrinsic subprograms

   --  Note: Asm is missing from this list, since Asm is a legitimate
   --  convention name. So is To_Adress, which is a GNAT attribute.

   First_Intrinsic_Name                : constant Name_Id := N + 389;
   Name_Divide                         : constant Name_Id := N + 389;
   Name_Enclosing_Entity               : constant Name_Id := N + 390;
   Name_Exception_Information          : constant Name_Id := N + 391;
   Name_Exception_Message              : constant Name_Id := N + 392;
   Name_Exception_Name                 : constant Name_Id := N + 393;
   Name_File                           : constant Name_Id := N + 394;
   Name_Import_Address                 : constant Name_Id := N + 395;
   Name_Import_Largest_Value           : constant Name_Id := N + 396;
   Name_Import_Value                   : constant Name_Id := N + 397;
   Name_Is_Negative                    : constant Name_Id := N + 398;
   Name_Line                           : constant Name_Id := N + 399;
   Name_Rotate_Left                    : constant Name_Id := N + 400;
   Name_Rotate_Right                   : constant Name_Id := N + 401;
   Name_Shift_Left                     : constant Name_Id := N + 402;
   Name_Shift_Right                    : constant Name_Id := N + 403;
   Name_Shift_Right_Arithmetic         : constant Name_Id := N + 404;
   Name_Source_Location                : constant Name_Id := N + 405;
   Name_Unchecked_Conversion           : constant Name_Id := N + 406;
   Name_Unchecked_Deallocation         : constant Name_Id := N + 407;
   Name_To_Pointer                     : constant Name_Id := N + 408;
   Last_Intrinsic_Name                 : constant Name_Id := N + 408;

   --  Reserved words used only in Ada 95

   First_95_Reserved_Word              : constant Name_Id := N + 409;
   Name_Abstract                       : constant Name_Id := N + 409;
   Name_Aliased                        : constant Name_Id := N + 410;
--   Name_Protected                      : constant Name_Id := N + $;
   Name_Until                          : constant Name_Id := N + 411;
--   Name_Requeue                        : constant Name_Id := N + $;
   Name_Tagged                         : constant Name_Id := N + 412;
   Last_95_Reserved_Word               : constant Name_Id := N + 412;

   subtype Ada_95_Reserved_Words is
     Name_Id range First_95_Reserved_Word .. Last_95_Reserved_Word;

   First_Reactive_Reserved_Word        : constant Name_Id := N + 413;
   Name_Reactive                       : constant Name_Id := N + 413;
   Name_Wait                           : constant Name_Id := N + 414;
   Name_Pause                          : constant Name_Id := N + 415;
   Name_Select                         : constant Name_Id := N + 416;
   Name_Fork                           : constant Name_Id := N + 417;
   Name_Reaction                       : constant Name_Id := N + 418;
   Name_React                          : constant Name_Id := N + 419;
   Name_Flow                           : constant Name_Id := N + 420;
   Name_Activity                       : constant Name_Id := N + 421;
   Last_Reactive_Reserved_Word         : constant Name_Id := N + 421;

   subtype Reactive_Reserved_Words is Name_Id range
     First_Reactive_Reserved_Word .. Last_Reactive_Reserved_Word;

   
   --  Miscellaneous names used in semantic checking

   Name_Raise_Exception                : constant Name_Id := N + 422;

   --  Additional reserved words in GNAT Project Files
   --  Note that Name_External is already previously declared

   Name_Binder                         : constant Name_Id := N + 423;
   Name_Body_Suffix                    : constant Name_Id := N + 424;
   Name_Builder                        : constant Name_Id := N + 425;
   Name_Compiler                       : constant Name_Id := N + 426;
   Name_Cross_Reference                : constant Name_Id := N + 427;
   Name_Default_Switches               : constant Name_Id := N + 428;
   Name_Exec_Dir                       : constant Name_Id := N + 429;
   Name_Executable                     : constant Name_Id := N + 430;
   Name_Executable_Suffix              : constant Name_Id := N + 431;
   Name_Extends                        : constant Name_Id := N + 432;
   Name_Finder                         : constant Name_Id := N + 433;
   Name_Global_Configuration_Pragmas   : constant Name_Id := N + 434;
   Name_Gnatls                         : constant Name_Id := N + 435;
   Name_Gnatstub                       : constant Name_Id := N + 436;
   Name_Implementation                 : constant Name_Id := N + 437;
   Name_Implementation_Exceptions      : constant Name_Id := N + 438;
   Name_Implementation_Suffix          : constant Name_Id := N + 439;
   Name_Languages                      : constant Name_Id := N + 440;
   Name_Library_Dir                    : constant Name_Id := N + 441;
   Name_Library_Auto_Init              : constant Name_Id := N + 442;
   Name_Library_GCC                    : constant Name_Id := N + 443;
   Name_Library_Interface              : constant Name_Id := N + 444;
   Name_Library_Kind                   : constant Name_Id := N + 445;
   Name_Library_Name                   : constant Name_Id := N + 446;
   Name_Library_Options                : constant Name_Id := N + 447;
   Name_Library_Reference_Symbol_File  : constant Name_Id := N + 448;
   Name_Library_Src_Dir                : constant Name_Id := N + 449;
   Name_Library_Symbol_File            : constant Name_Id := N + 450;
   Name_Library_Symbol_Policy          : constant Name_Id := N + 451;
   Name_Library_Version                : constant Name_Id := N + 452;
   Name_Linker                         : constant Name_Id := N + 453;
   Name_Local_Configuration_Pragmas    : constant Name_Id := N + 454;
   Name_Locally_Removed_Files          : constant Name_Id := N + 455;
   Name_Naming                         : constant Name_Id := N + 456;
   Name_Object_Dir                     : constant Name_Id := N + 457;
   Name_Pretty_Printer                 : constant Name_Id := N + 458;
   Name_Project                        : constant Name_Id := N + 459;
   Name_Separate_Suffix                : constant Name_Id := N + 460;
   Name_Source_Dirs                    : constant Name_Id := N + 461;
   Name_Source_Files                   : constant Name_Id := N + 462;
   Name_Source_List_File               : constant Name_Id := N + 463;
   Name_Spec                           : constant Name_Id := N + 464;
   Name_Spec_Suffix                    : constant Name_Id := N + 465;
   Name_Specification                  : constant Name_Id := N + 466;
   Name_Specification_Exceptions       : constant Name_Id := N + 467;
   Name_Specification_Suffix           : constant Name_Id := N + 468;
   Name_Switches                       : constant Name_Id := N + 469;
   --  Other miscellaneous names used in front end

   Name_Unaligned_Valid                : constant Name_Id := N + 470;

   --  Mark last defined name for consistency check in Snames body

   Last_Predefined_Name                : constant Name_Id := N + 470;

   subtype Any_Operator_Name is Name_Id range
     First_Operator_Name .. Last_Operator_Name;

   ------------------------------
   -- Attribute ID Definitions --
   ------------------------------

   type Attribute_Id is (
      Attribute_Access,
      Attribute_Address,
      Attribute_Address_Size,
      Attribute_Alignment,
      Attribute_Bit,
      Attribute_Bit_Position,
      Attribute_Component_Size,
      Attribute_Constrained,
      Attribute_Enum_Rep,
      Attribute_Exponent,
      Attribute_First,
      Attribute_First_Bit,
      Attribute_Fore,
      Attribute_Img,
      Attribute_Integer_Value,
      Attribute_Last,
      Attribute_Last_Bit,
      Attribute_Length,
      Attribute_Machine_Emax,
      Attribute_Machine_Emin,
      Attribute_Machine_Mantissa,
      Attribute_Machine_Overflows,
      Attribute_Machine_Radix,
      Attribute_Machine_Rounds,
      Attribute_Machine_Size,
      Attribute_Mantissa,
      Attribute_Max_Size_In_Storage_Elements,
      Attribute_Modulus,
      Attribute_Object_Size,
      Attribute_Passed_By_Reference,
      Attribute_Pool_Address,
      Attribute_Pos,
      Attribute_Position,
      Attribute_Range,
      Attribute_Range_Length,
      Attribute_Round,
      Attribute_Scale,
      Attribute_Scaling,
      Attribute_Size,
      Attribute_Storage_Size,
      Attribute_Storage_Unit,
      Attribute_Tag,
      Attribute_Target_Name,
      Attribute_Terminated,
      Attribute_To_Address,
      Attribute_Type_Class,
      Attribute_Unbiased_Rounding,
      Attribute_Unchecked_Access,
      Attribute_Unconstrained_Array,
      Attribute_Universal_Literal_String,
      Attribute_Unrestricted_Access,
      Attribute_Val,
      Attribute_Valid,
      Attribute_Value_Size,
      Attribute_Wchar_T_Size,
      Attribute_Word_Size,

      --  Attributes designating renamable functions

      Attribute_Floor,
      Attribute_Fraction,
      Attribute_Image,
      Attribute_Max,
      Attribute_Min,
      Attribute_Model,
      Attribute_Remainder,
      Attribute_Rounding,
      Attribute_Succ,
      Attribute_Truncation,
      Attribute_Value,
      Attribute_Wide_Image,
      Attribute_Wide_Value,

      --  Entity attributes (includes type attributes)

      Attribute_Storage_Pool,

      --  Type attributes

      Attribute_Base,
      Attribute_Class);

   ------------------------------------
   -- Convention Name ID Definitions --
   ------------------------------------

   type Convention_Id is (

      --  The conventions that are defined by the RM come first

      Convention_Ada,
      Convention_Intrinsic,

      --  The remaining conventions are foreign language conventions

      Convention_Assembler,  --  also Asm, Assembly
      Convention_C,          --  also Default, External
--      Convention_CPP,
      Convention_Stdcall,    --  also DLL, Win32
      Convention_Stubbed);

      --  Note: Convention C_Pass_By_Copy is allowed only for record
      --  types (where it is treated like C except that the appropriate
      --  flag is set in the record type). Recognizion of this convention
      --  is specially handled in Sem_Prag.

   for Convention_Id'Size use 8;
   --  Plenty of space for expansion

   subtype Foreign_Convention is
     Convention_Id range Convention_Assembler .. Convention_Stdcall;

   -----------------------------------
   -- Locking Policy ID Definitions --
   -----------------------------------

   type Locking_Policy_Id is (
      Locking_Policy_Inheritance_Locking,
      Locking_Policy_Ceiling_Locking);

   ---------------------------
   -- Pragma ID Definitions --
   ---------------------------

   type Pragma_Id is (

      --  Configuration pragmas

      Pragma_C_Pass_By_Copy,
      Pragma_Compile_Time_Warning,
      Pragma_Component_Alignment,
      Pragma_Convention_Identifier,
      Pragma_Discard_Names,
      Pragma_Elaboration_Checks,
      Pragma_Eliminate,
      Pragma_Explicit_Overriding,
      Pragma_Extensions_Allowed,
      Pragma_External_Name_Casing,
      Pragma_Float_Representation,
      Pragma_Initialize_Scalars,
      Pragma_Long_Float,
      Pragma_No_Run_Time,
      Pragma_Normalize_Scalars,
      Pragma_Polling,
      Pragma_Persistent_Data,
      Pragma_Persistent_Object,
      Pragma_Ravenscar,
      Pragma_Restricted_Run_Time,
      Pragma_Restrictions,
      Pragma_Restriction_Warnings,
      Pragma_Style_Checks,
      Pragma_Suppress,
      Pragma_Suppress_Exception_Locations,
      Pragma_Unsuppress,
      Pragma_Validity_Checks,
      Pragma_Warnings,

      --  Remaining (non-configuration) pragmas

      Pragma_Annotate,
      Pragma_Assert,
      Pragma_Comment,
      Pragma_Common_Object,
      Pragma_Convention,
      Pragma_Debug,
      Pragma_Elaborate,
      Pragma_Elaborate_All,
      Pragma_Elaborate_Body,
      Pragma_Export,
      Pragma_Export_Exception,
      Pragma_Export_Function,
      Pragma_Export_Object,
      Pragma_Export_Procedure,
      Pragma_External,
      Pragma_Finalize_Storage_Only,
      Pragma_Ident,
      Pragma_Import,
      Pragma_Import_Function,
      Pragma_Import_Object,
      Pragma_Import_Procedure,
      Pragma_Inline,
      Pragma_Inline_Always,
      Pragma_Inline_Generic,
      Pragma_Inspection_Point,
      Pragma_Interface,
      Pragma_Interface_Name,
      Pragma_Keep_Names,
      Pragma_Link_With,
      Pragma_Linker_Alias,
      Pragma_Linker_Options,
      Pragma_Memory_Size,
      Pragma_No_Return,
      Pragma_Obsolescent,
      Pragma_Optimize,
      Pragma_Optional_Overriding,
      Pragma_Overriding,
      Pragma_Pack,
      Pragma_Pure,
      Pragma_Pure_Function,
      Pragma_Source_Reference,
      Pragma_Suppress_All,
      Pragma_Suppress_Debug_Info,
      Pragma_Suppress_Initialization,
      Pragma_System_Name,
      Pragma_Unchecked_Union,
      Pragma_Unimplemented_Unit,
      Pragma_Unreferenced,
      Pragma_Volatile,
      Pragma_Volatile_Components,

      --  The following pragmas are on their own, out of order, because of
      --  the special processing required to deal with the fact that their
      --  names match existing attribute names.

      Pragma_Storage_Size,
      Pragma_Storage_Unit,

      --  The value to represent an unknown or unrecognized pragma

      Unknown_Pragma);

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize;
   --  Called to initialize the preset names in the names table.

   function Is_Attribute_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized attribute

   function Is_Entity_Attribute_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized entity attribute,
   --  i.e. an attribute reference that returns an entity.

   function Is_Function_Attribute_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized attribute
   --  that designates a renameable function, and can therefore appear in
   --  a renaming statement. Note that not all attributes designating
   --  functions are renamable, in particular, thos returning a universal
   --  value cannot be renamed.

   function Is_Type_Attribute_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized type attribute,
   --  i.e. an attribute reference that returns a type

   function Is_Check_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized suppress check
   --  as required by pragma Suppress.

   function Is_Convention_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of one of the recognized
   --  language conventions, as required by pragma Convention, Import,
   --  Export, Interface. Returns True if so. Also returns True for a
   --  name that has been specified by a Convention_Identifier pragma.
   --  If neither case holds, returns False.

   function Is_Operator_Symbol_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of an operator symbol

   function Is_Pragma_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized pragma. Note
   --  that pragmas AST_Entry, Storage_Size, and Storage_Unit are recognized
   --  as pragmas by this function even though their names are separate from
   --  the other pragma names.

   function Get_Attribute_Id (N : Name_Id) return Attribute_Id;
   --  Returns Id of attribute corresponding to given name. It is an error to
   --  call this function with a name that is not the name of a attribute.

   function Get_Convention_Id (N : Name_Id) return Convention_Id;
   --  Returns Id of language convention corresponding to given name. It is an
   --  to call this function with a name that is not the name of a convention,
   --  or one previously given in a call to Record_Convention_Identifier.

   function Get_Check_Id (N : Name_Id) return Check_Id;
   --  Returns Id of suppress check corresponding to given name. It is an error
   --  to call this function with a name that is not the name of a check.

   function Get_Pragma_Id (N : Name_Id) return Pragma_Id;
   --  Returns Id of pragma corresponding to given name. Returns Unknown_Pragma
   --  if N is not a name of a known (Ada defined or GNAT-specific) pragma.
   --  Note that the function also works correctly for names of pragmas that
   --  are not in the main list of pragma Names (AST_Entry, Storage_Size, and
   --  Storage_Unit (e.g. Name_Storage_Size returns Pragma_Storage_Size).

   procedure Record_Convention_Identifier
     (Id         : Name_Id;
      Convention : Convention_Id);
   --  A call to this procedure, resulting from an occurrence of a pragma
   --  Convention_Identifier, records that from now on an occurrence of
   --  Id will be recognized as a name for the specified convention.

private
   pragma Inline (Is_Attribute_Name);
   pragma Inline (Is_Entity_Attribute_Name);
   pragma Inline (Is_Type_Attribute_Name);
   pragma Inline (Is_Check_Name);
   pragma Inline (Is_Operator_Symbol_Name);
   pragma Inline (Is_Pragma_Name);

end Snames;
