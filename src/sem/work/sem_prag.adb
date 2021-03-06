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
-- GNAT were provided by Ada Core Technologies Inc. Reflex is developed  by --
-- the Artics team at Grenoble.                                             --
--                                                                          --
------------------------------------------------------------------------------

--  This unit contains the semantic processing for all pragmas, both language
--  and implementation defined. For most pragmas, the parser only does the
--  most basic job of checking the syntax, so Sem_Prag also contains the code
--  to complete the syntax checks. Certain pragmas are handled partially or
--  completely by the parser (see Par.Prag for further details).

with Ada.Text_Io; use Ada.Text_Io;

with Atree;    use Atree;
with Casing;   use Casing;
with Csets;    use Csets;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
--with Expander; use Expander;
--  with Exp_Dist; use Exp_Dist;
with Fname;    use Fname;
with Hostparm; use Hostparm;
with Lib;      use Lib;
with Lib.Writ; use Lib.Writ;
with Lib.Xref; use Lib.Xref;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Restrict; use Restrict;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch13; use Sem_Ch13;
with Sem_Disp; use Sem_Disp;
with Sem_Elim; use Sem_Elim;
with Sem_Eval; use Sem_Eval;
with Sem_Intr; use Sem_Intr;
with Sem_Mech; use Sem_Mech;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
-- with Sem_VFpt; use Sem_VFpt;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Sinfo.CN; use Sinfo.CN;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stringt;  use Stringt;
with Stylesw;  use Stylesw;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Ttypes;
with Uintp;    use Uintp;
with Urealp;   use Urealp;
with Validsw;  use Validsw;
with Namet; use Namet;

with Artics.Strings_Stocks;
with Reflex.Names; use Reflex.Names;
with Reflex.Infos; use Reflex.Infos;

with GNAT.Spelling_Checker; use GNAT.Spelling_Checker;

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Gnat.Case_Util; use Gnat.Case_Util;

package body Sem_Prag is

   ----------------------------------------------
   -- Common Handling of Import-Export Pragmas --
   ----------------------------------------------

   --  In the following section, a number of Import_xxx and Export_xxx
   --  pragmas are defined by GNAT. These are compatible with the DEC
   --  pragmas of the same name, and all have the following common
   --  form and processing:

   --  pragma Export_xxx
   --        [Internal                 =>] LOCAL_NAME,
   --     [, [External                 =>] EXTERNAL_SYMBOL]
   --     [, other optional parameters   ]);

   --  pragma Import_xxx
   --        [Internal                 =>] LOCAL_NAME,
   --     [, [External                 =>] EXTERNAL_SYMBOL]
   --     [, other optional parameters   ]);

   --   EXTERNAL_SYMBOL ::=
   --     IDENTIFIER
   --   | static_string_EXPRESSION

   --  The internal LOCAL_NAME designates the entity that is imported or
   --  exported, and must refer to an entity in the current declarative
   --  part (as required by the rules for LOCAL_NAME).

   --  The external linker name is designated by the External parameter
   --  if given, or the Internal parameter if not (if there is no External
   --  parameter, the External parameter is a copy of the Internal name).

   --  If the External parameter is given as a string, then this string
   --  is treated as an external name (exactly as though it had been given
   --  as an External_Name parameter for a normal Import pragma).

   --  If the External parameter is given as an identifier (or there is no
   --  External parameter, so that the Internal identifier is used), then
   --  the external name is the characters of the identifier, translated
   --  to all upper case letters for OpenVMS versions of GNAT, and to all
   --  lower case letters for all other versions

   --  Note: the external name specified or implied by any of these special
   --  Import_xxx or Export_xxx pragmas override an external or link name
   --  specified in a previous Import or Export pragma.

   --  Note: these and all other DEC-compatible GNAT pragmas allow full
   --  use of named notation, following the standard rules for subprogram
   --  calls, i.e. parameters can be given in any order if named notation
   --  is used, and positional and named notation can be mixed, subject to
   --  the rule that all positional parameters must appear first.

   --  Note: All these pragmas are implemented exactly following the DEC
   --  design and implementation and are intended to be fully compatible
   --  with the use of these pragmas in the DEC Ada compiler.

   -------------------------------------
   -- Local Subprograms and Variables --
   -------------------------------------

   function Adjust_External_Name_Case (N : Node_Id) return Node_Id;
   --  This routine is used for possible casing adjustment of an explicit
   --  external name supplied as a string literal (the node N), according
   --  to the casing requirement of Opt.External_Name_Casing. If this is
   --  set to As_Is, then the string literal is returned unchanged, but if
   --  it is set to Uppercase or Lowercase, then a new string literal with
   --  appropriate casing is constructed.

   function Get_Base_Subprogram (Def_Id : Entity_Id) return Entity_Id;
   --  If Def_Id refers to a renamed subprogram, then the base subprogram
   --  (the original one, following the renaming chain) is returned.
   --  Otherwise the entity is returned unchanged. Should be in Einfo???

   procedure Set_Unit_Name (N : Node_Id; With_Item : Node_Id);
   --  Place semantic information on the argument of an Elaborate or
   --  Elaborate_All pragma. Entity name for unit and its parents is
   --  taken from item in previous with_clause that mentions the unit.
   
   -------------------------------
   -- Adjust_External_Name_Case --
   -------------------------------

   function Adjust_External_Name_Case (N : Node_Id) return Node_Id is
      CC : Char_Code;

   begin
      --  Adjust case of literal if required

      if Opt.External_Name_Exp_Casing = As_Is then
         return N;

      else
         --  Copy existing string

         Start_String;

         --  Set proper casing

         for J in 1 .. String_Length (Strval (N)) loop
            CC := Get_String_Char (Strval (N), J);

            if Opt.External_Name_Exp_Casing = Uppercase
              and then CC >= Get_Char_Code ('a')
              and then CC <= Get_Char_Code ('z')
            then
               Store_String_Char (CC - 32);

            elsif Opt.External_Name_Exp_Casing = Lowercase
              and then CC >= Get_Char_Code ('A')
              and then CC <= Get_Char_Code ('Z')
            then
               Store_String_Char (CC + 32);

            else
               Store_String_Char (CC);
            end if;
         end loop;

         return
           Make_String_Literal (Sloc (N),
             Strval => End_String);
      end if;
   end Adjust_External_Name_Case;

   --------------------
   -- Analyze_Pragma --
   --------------------

   procedure Analyze_Pragma (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Prag_Id : Pragma_Id;

      Pragma_Exit : exception;
      --  This exception is used to exit pragma processing completely. It
      --  is used when an error is detected, and in other situations where
      --  it is known that no further processing is required.

      Arg_Count : Nat;
      --  Number of pragma argument associations

      Arg1 : Node_Id;
      Arg2 : Node_Id;
      Arg3 : Node_Id;
      Arg4 : Node_Id;
      --  First four pragma arguments (pragma argument association nodes,
      --  or Empty if the corresponding argument does not exist).
      
      procedure Process_Reflex_Pragmas;
      
      procedure Check_Arg_Count (Required : Nat);
      --  Check argument count for pragma is equal to given parameter.
      --  If not, then issue an error message and raise Pragma_Exit.

      --  Note: all routines whose name is Check_Arg_Is_xxx take an
      --  argument Arg which can either be a pragma argument association,
      --  in which case the check is applied to the expression of the
      --  association or an expression directly.

      procedure Check_Arg_Is_Identifier (Arg : Node_Id);
      --  Check the specified argument Arg to make sure that it is an
      --  identifier. If not give error and raise Pragma_Exit.

      procedure Check_Arg_Is_Integer_Literal (Arg : Node_Id);
      --  Check the specified argument Arg to make sure that it is an
      --  integer literal. If not give error and raise Pragma_Exit.

      procedure Check_Arg_Is_Library_Level_Local_Name (Arg : Node_Id);
      --  Check the specified argument Arg to make sure that it has the
      --  proper syntactic form for a local name and meets the semantic
      --  requirements for a local name. The local name is analyzed as
      --  part of the processing for this call. In addition, the local
      --  name is required to represent an entity at the library level.

      procedure Check_Arg_Is_Local_Name (Arg : Node_Id);
      --  Check the specified argument Arg to make sure that it has the
      --  proper syntactic form for a local name and meets the semantic
      --  requirements for a local name. The local name is analyzed as
      --  part of the processing for this call.

      procedure Check_Arg_Is_One_Of (Arg : Node_Id; N1, N2 : Name_Id);
      procedure Check_Arg_Is_One_Of (Arg : Node_Id; N1, N2, N3 : Name_Id);
      --  Check the specified argument Arg to make sure that it is an
      --  identifier whose name matches either N1 or N2 (or N3 if present).
      --  If not then give error and raise Pragma_Exit.

      procedure Check_Arg_Is_Static_Expression
        (Arg : Node_Id;
         Typ : Entity_Id);
      --  Check the specified argument Arg to make sure that it is a static
      --  expression of the given type (i.e. it will be analyzed and resolved
      --  using this type, which can be any valid argument to Resolve, e.g.
      --  Any_Integer is OK). If not, given error and raise Pragma_Exit.

      procedure Check_Arg_Is_String_Literal (Arg : Node_Id);
      --  Check the specified argument Arg to make sure that it is a
      --  string literal. If not give error and raise Pragma_Exit

      procedure Check_At_Least_N_Arguments (N : Nat);
      --  Check there are at least N arguments present

      procedure Check_At_Most_N_Arguments (N : Nat);
      --  Check there are no more than N arguments present

      procedure Check_First_Subtype (Arg : Node_Id);
      --  Checks that Arg, whose expression is an entity name referencing
      --  a subtype, does not reference a type that is not a first subtype.

      procedure Check_In_Main_Program;
      --  Common checks for pragmas that appear within a main program
      --  (Priority, Main_Storage, Time_Slice).

      procedure Check_Is_In_Decl_Part_Or_Package_Spec;
      --  Check that pragma appears in a declarative part, or in a package
      --  specification, i.e. that it does not occur in a statement sequence
      --  in a body.

      procedure Check_No_Identifier (Arg : Node_Id);
      --  Checks that the given argument does not have an identifier. If
      --  an identifier is present, then an error message is issued, and
      --  Pragma_Exit is raised.

      procedure Check_No_Identifiers;
      --  Checks that none of the arguments to the pragma has an identifier.
      --  If any argument has an identifier, then an error message is issued,
      --  and Pragma_Exit is raised.

      procedure Check_Optional_Identifier (Arg : Node_Id; Id : Name_Id);
      --  Checks if the given argument has an identifier, and if so, requires
      --  it to match the given identifier name. If there is a non-matching
      --  identifier, then an error message is given and Error_Pragmas raised.

      procedure Check_Optional_Identifier (Arg : Node_Id; Id : String);
      --  Checks if the given argument has an identifier, and if so, requires
      --  it to match the given identifier name. If there is a non-matching
      --  identifier, then an error message is given and Error_Pragmas raised.
      --  In this version of the procedure, the identifier name is given as
      --  a string with lower case letters.

      procedure Check_Static_Constraint (Constr : Node_Id);
      --  Constr is a constraint from an N_Subtype_Indication node from a
      --  component constraint in an Unchecked_Union type. This routine checks
      --  that the constraint is static as required by the restrictions for
      --  Unchecked_Union.

      procedure Check_Valid_Configuration_Pragma;
      --  Legality checks for placement of a configuration pragma

      procedure Check_Valid_Library_Unit_Pragma;
      --  Legality checks for library unit pragmas. A special case arises for
      --  pragmas in generic instances that come from copies of the original
      --  library unit pragmas in the generic templates. In the case of other
      --  than library level instantiations these can appear in contexts which
      --  would normally be invalid (they only apply to the original template
      --  and to library level instantiations), and they are simply ignored,
      --  which is implemented by rewriting them as null statements.

      procedure Error_Pragma (Msg : String);
      pragma No_Return (Error_Pragma);
      --  Outputs error message for current pragma. The message contains an %
      --  that will be replaced with the pragma name, and the flag is placed
      --  on the pragma itself. Pragma_Exit is then raised.

      procedure Error_Pragma_Arg (Msg : String; Arg : Node_Id);
      pragma No_Return (Error_Pragma_Arg);
      --  Outputs error message for current pragma. The message may contain
      --  a % that will be replaced with the pragma name. The parameter Arg
      --  may either be a pragma argument association, in which case the flag
      --  is placed on the expression of this association, or an expression,
      --  in which case the flag is placed directly on the expression. The
      --  message is placed using Error_Msg_N, so the message may also contain
      --  an & insertion character which will reference the given Arg value.
      --  After placing the message, Pragma_Exit is raised.

      procedure Error_Pragma_Arg (Msg1, Msg2 : String; Arg : Node_Id);
      pragma No_Return (Error_Pragma_Arg);
      --  Similar to above form of Error_Pragma_Arg except that two messages
      --  are provided, the second is a continuation comment starting with \.

      procedure Error_Pragma_Arg_Ident (Msg : String; Arg : Node_Id);
      pragma No_Return (Error_Pragma_Arg_Ident);
      --  Outputs error message for current pragma. The message may contain
      --  a % that will be replaced with the pragma name. The parameter Arg
      --  must be a pragma argument association with a non-empty identifier
      --  (i.e. its Chars field must be set), and the error message is placed
      --  on the identifier. The message is placed using Error_Msg_N so
      --  the message may also contain an & insertion character which will
      --  reference the identifier. After placing the message, Pragma_Exit
      --  is raised.

      function Find_Lib_Unit_Name return Entity_Id;
      --  Used for a library unit pragma to find the entity to which the
      --  library unit pragma applies, returns the entity found.

      procedure Find_Program_Unit_Name (Id : Node_Id);
      --  If the pragma is a compilation unit pragma, the id must denote the
      --  compilation unit in the same compilation, and the pragma must appear
      --  in the list of preceding or trailing pragmas. If it is a program
      --  unit pragma that is not a compilation unit pragma, then the
      --  identifier must be visible.

      type Name_List is array (Natural range <>) of Name_Id;
      type Args_List is array (Natural range <>) of Node_Id;
      procedure Gather_Associations
        (Names : Name_List;
         Args  : out Args_List);
      --  This procedure is used to gather the arguments for a pragma that
      --  permits arbitrary ordering of parameters using the normal rules
      --  for named and positional parameters. The Names argument is a list
      --  of Name_Id values that corresponds to the allowed pragma argument
      --  association identifiers in order. The result returned in Args is
      --  a list of corresponding expressions that are the pragma arguments.
      --  Note that this is a list of expressions, not of pragma argument
      --  associations (Gather_Associations has completely checked all the
      --  optional identifiers when it returns). An entry in Args is Empty
      --  on return if the corresponding argument is not present.

      function Get_Pragma_Arg (Arg : Node_Id) return Node_Id;
      --  All the routines that check pragma arguments take either a pragma
      --  argument association (in which case the expression of the argument
      --  association is checked), or the expression directly. The function
      --  Get_Pragma_Arg is a utility used to deal with these two cases. If
      --  Arg is a pragma argument association node, then its expression is
      --  returned, otherwise Arg is returned unchanged.

      procedure GNAT_Pragma;
      --  Called for all GNAT defined pragmas to note the use of the feature,
      --  and also check the relevant restriction (No_Implementation_Pragmas).

      function Is_Before_First_Decl
        (Pragma_Node : Node_Id;
         Decls       : List_Id) return Boolean;
      --  Return True if Pragma_Node is before the first declarative item in
      --  Decls where Decls is the list of declarative items.

      function Is_Configuration_Pragma return Boolean;
      --  Deterermines if the placement of the current pragma is appropriate
      --  for a configuration pragma (precedes the current compilation unit)

      procedure Pragma_Misplaced;
      --  Issue fatal error message for misplaced pragma

      --procedure Process_Atomic_Shared_Volatile;
      --  Common processing for pragmas Atomic, Shared, Volatile. Note that
      --  Shared is an obsolete Ada 83 pragma, treated as being identical
      --  in effect to pragma Atomic.

      procedure Process_Convention (C : out Convention_Id; E : out Entity_Id);
      --  Common procesing for Convention, Interface, Import and Export.
      --  Checks first two arguments of pragma, and sets the appropriate
      --  convention value in the specified entity or entities. On return
      --  C is the convention, E is the referenced entity.

      procedure Process_Extended_Import_Export_Exception_Pragma
        (Arg_Internal : Node_Id;
         Arg_External : Node_Id;
         Arg_Form     : Node_Id;
         Arg_Code     : Node_Id);
      --  Common processing for the pragmas Import/Export_Exception.
      --  The three arguments correspond to the three named parameters of
      --  the pragma. An argument is empty if the corresponding parameter
      --  is not present in the pragma.

      procedure Process_Extended_Import_Export_Object_Pragma
        (Arg_Internal : Node_Id;
         Arg_External : Node_Id;
         Arg_Size     : Node_Id);
      --  Common processing for the pragmass Import/Export_Object.
      --  The three arguments correspond to the three named parameters
      --  of the pragmas. An argument is empty if the corresponding
      --  parameter is not present in the pragma.

      procedure Process_Extended_Import_Export_Internal_Arg
        (Arg_Internal : Node_Id := Empty);
      --  Common processing for all extended Import and Export pragmas. The
      --  argument is the pragma parameter for the Internal argument. If
      --  Arg_Internal is empty or inappropriate, an error message is posted.
      --  Otherwise, on normal return, the Entity_Field of Arg_Internal is
      --  set to identify the referenced entity.

      procedure Process_Extended_Import_Export_Subprogram_Pragma
        (Arg_Internal                 : Node_Id;
         Arg_External                 : Node_Id;
         Arg_Parameter_Types          : Node_Id;
         Arg_Result_Type              : Node_Id := Empty;
         Arg_Mechanism                : Node_Id;
         Arg_Result_Mechanism         : Node_Id := Empty;
         Arg_First_Optional_Parameter : Node_Id := Empty);
      --  Common processing for all extended Import and Export pragmas
      --  applying to subprograms. The caller omits any arguments that do
      --  bnot apply to the pragma in question (for example, Arg_Result_Type
      --  can be non-Empty only in the Import_Function and Export_Function
      --  cases). The argument names correspond to the allowed pragma
      --  association identifiers.

      procedure Process_Generic_List;
      --  Common processing for Share_Generic and Inline_Generic

      procedure Process_Import_Or_Interface;
      --  Common processing for Import of Interface

      procedure Process_Inline (Active : Boolean);
      --  Common processing for Inline and Inline_Always. The parameter
      --  indicates if the inline pragma is active, i.e. if it should
      --  actually cause inlining to occur.

      procedure Process_Interface_Name
        (Subprogram_Def : Entity_Id;
         Ext_Arg        : Node_Id;
         Link_Arg       : Node_Id);
      --  Given the last two arguments of pragma Import, pragma Export, or
      --  pragma Interface_Name, performs validity checks and sets the
      --  Interface_Name field of the given subprogram entity to the
      --  appropriate external or link name, depending on the arguments
      --  given. Ext_Arg is always present, but Link_Arg may be missing.
      --  Note that Ext_Arg may represent the Link_Name if Link_Arg is
      --  missing, and appropriate named notation is used for Ext_Arg.
      --  If neither Ext_Arg nor Link_Arg is present, the interface name
      --  is set to the default from the subprogram name.

      procedure Process_Suppress_Unsuppress (Suppress_Case : Boolean);
      --  Common processing for Suppress and Unsuppress. The boolean parameter
      --  Suppress_Case is True for the Suppress case, and False for the
      --  Unsuppress case.

      procedure Set_Exported (E : Entity_Id; Arg : Node_Id);
      --  This procedure sets the Is_Exported flag for the given entity,
      --  checking that the entity was not previously imported. Arg is
      --  the argument that specified the entity. A check is also made
      --  for exporting inappropriate entities.

      procedure Set_Extended_Import_Export_External_Name
        (Internal_Ent : Entity_Id;
         Arg_External : Node_Id);
      --  Common processing for all extended import export pragmas. The first
      --  argument, Internal_Ent, is the internal entity, which has already
      --  been checked for validity by the caller. Arg_External is from the
      --  Import or Export pragma, and may be null if no External parameter
      --  was present. If Arg_External is present and is a non-null string
      --  (a null string is treated as the default), then the Interface_Name
      --  field of Internal_Ent is set appropriately.

      procedure Set_Imported (E : Entity_Id);
      --  This procedure sets the Is_Imported flag for the given entity,
      --  checking that it is not previously exported or imported.

      procedure Set_Mechanism_Value (Ent : Entity_Id; Mech_Name : Node_Id);
      --  Mech is a parameter passing mechanism (see Import_Function syntax
      --  for MECHANISM_NAME). This routine checks that the mechanism argument
      --  has the right form, and if not issues an error message. If the
      --  argument has the right form then the Mechanism field of Ent is
      --  set appropriately.

      ---------------------
      -- Check_Arg_Count --
      ---------------------

      procedure Check_Arg_Count (Required : Nat) is
      begin
         if Arg_Count /= Required then
            Error_Pragma ("wrong number of arguments for pragma%");
         end if;
      end Check_Arg_Count;

      -----------------------------
      -- Check_Arg_Is_Identifier --
      -----------------------------

      procedure Check_Arg_Is_Identifier (Arg : Node_Id) is
         Argx : constant Node_Id := Get_Pragma_Arg (Arg);

      begin
         if Nkind (Argx) /= N_Identifier then
            Error_Pragma_Arg
              ("argument for pragma% must be identifier", Argx);
         end if;
      end Check_Arg_Is_Identifier;

      ----------------------------------
      -- Check_Arg_Is_Integer_Literal --
      ----------------------------------

      procedure Check_Arg_Is_Integer_Literal (Arg : Node_Id) is
         Argx : constant Node_Id := Get_Pragma_Arg (Arg);

      begin
         if Nkind (Argx) /= N_Integer_Literal then
            Error_Pragma_Arg
              ("argument for pragma% must be integer literal", Argx);
         end if;
      end Check_Arg_Is_Integer_Literal;

      -------------------------------------------
      -- Check_Arg_Is_Library_Level_Local_Name --
      -------------------------------------------

      --  LOCAL_NAME ::=
      --    DIRECT_NAME
      --  | DIRECT_NAME'ATTRIBUTE_DESIGNATOR
      --  | library_unit_NAME

      procedure Check_Arg_Is_Library_Level_Local_Name (Arg : Node_Id) is
      begin
         Check_Arg_Is_Local_Name (Arg);

         if not Is_Library_Level_Entity (Entity (Expression (Arg)))
           and then Comes_From_Source (N)
         then
            Error_Pragma_Arg
              ("argument for pragma% must be library level entity", Arg);
         end if;
      end Check_Arg_Is_Library_Level_Local_Name;

      -----------------------------
      -- Check_Arg_Is_Local_Name --
      -----------------------------

      --  LOCAL_NAME ::=
      --    DIRECT_NAME
      --  | DIRECT_NAME'ATTRIBUTE_DESIGNATOR
      --  | library_unit_NAME

      procedure Check_Arg_Is_Local_Name (Arg : Node_Id) is
         Argx : constant Node_Id := Get_Pragma_Arg (Arg);

      begin
         Analyze (Argx);

         if Nkind (Argx) not in N_Direct_Name
           and then (Nkind (Argx) /= N_Attribute_Reference
                      or else Present (Expressions (Argx))
                      or else Nkind (Prefix (Argx)) /= N_Identifier)
           and then (not Is_Entity_Name (Argx)
                      or else not Is_Compilation_Unit (Entity (Argx)))
         then
            Error_Pragma_Arg ("argument for pragma% must be local name", Argx);
         end if;

         if Is_Entity_Name (Argx)
           and then Scope (Entity (Argx)) /= Current_Scope
         then
            Error_Pragma_Arg
              ("pragma% argument must be in same declarative part", Arg);
         end if;
      end Check_Arg_Is_Local_Name;

      -------------------------
      -- Check_Arg_Is_One_Of --
      -------------------------

      procedure Check_Arg_Is_One_Of (Arg : Node_Id; N1, N2 : Name_Id) is
         Argx : constant Node_Id := Get_Pragma_Arg (Arg);

      begin
         Check_Arg_Is_Identifier (Argx);

         if Chars (Argx) /= N1 and then Chars (Argx) /= N2 then
            Error_Msg_Name_2 := N1;
            Error_Msg_Name_3 := N2;
            Error_Pragma_Arg ("argument for pragma% must be% or%", Argx);
         end if;
      end Check_Arg_Is_One_Of;

      procedure Check_Arg_Is_One_Of
        (Arg        : Node_Id;
         N1, N2, N3 : Name_Id)
      is
         Argx : constant Node_Id := Get_Pragma_Arg (Arg);

      begin
         Check_Arg_Is_Identifier (Argx);

         if Chars (Argx) /= N1
           and then Chars (Argx) /= N2
           and then Chars (Argx) /= N3
         then
            Error_Pragma_Arg ("invalid argument for pragma%", Argx);
         end if;
      end Check_Arg_Is_One_Of;

      ------------------------------------
      -- Check_Arg_Is_Static_Expression --
      ------------------------------------

      procedure Check_Arg_Is_Static_Expression
        (Arg : Node_Id;
         Typ : Entity_Id)
      is
         Argx : constant Node_Id := Get_Pragma_Arg (Arg);

      begin
         Analyze_And_Resolve (Argx, Typ);

         if Is_OK_Static_Expression (Argx) then
            return;

         elsif Etype (Argx) = Any_Type then
            raise Pragma_Exit;

         --  An interesting special case, if we have a string literal and
         --  we are in Ada 83 mode, then we allow it even though it will
         --  not be flagged as static. This allows the use of Ada 95
         --  pragmas like Import in Ada 83 mode. They will of course be
         --  flagged with warnings as usual, but will not cause errors.

         elsif Ada_83 and then Nkind (Argx) = N_String_Literal then
            return;

         --  Static expression that raises Constraint_Error. This has
         --  already been flagged, so just exit from pragma processing.

         elsif Is_Static_Expression (Argx) then
            raise Pragma_Exit;

         --  Finally, we have a real error

         else
            Error_Msg_Name_1 := Chars (N);
            Flag_Non_Static_Expr
              ("argument for pragma% must be a static expression!", Argx);
            raise Pragma_Exit;
         end if;
      end Check_Arg_Is_Static_Expression;

      ---------------------------------
      -- Check_Arg_Is_String_Literal --
      ---------------------------------

      procedure Check_Arg_Is_String_Literal (Arg : Node_Id) is
         Argx : constant Node_Id := Get_Pragma_Arg (Arg);

      begin
         if Nkind (Argx) /= N_String_Literal then
            Error_Pragma_Arg
              ("argument for pragma% must be string literal", Argx);
         end if;

      end Check_Arg_Is_String_Literal;

      --------------------------------
      -- Check_At_Least_N_Arguments --
      --------------------------------

      procedure Check_At_Least_N_Arguments (N : Nat) is
      begin
         if Arg_Count < N then
            Error_Pragma ("too few arguments for pragma%");
         end if;
      end Check_At_Least_N_Arguments;

      -------------------------------
      -- Check_At_Most_N_Arguments --
      -------------------------------

      procedure Check_At_Most_N_Arguments (N : Nat) is
         Arg : Node_Id;

      begin
         if Arg_Count > N then
            Arg := Arg1;

            for J in 1 .. N loop
               Next (Arg);
               Error_Pragma_Arg ("too many arguments for pragma%", Arg);
            end loop;
         end if;
      end Check_At_Most_N_Arguments;

      -------------------------
      -- Check_First_Subtype --
      -------------------------

      procedure Check_First_Subtype (Arg : Node_Id) is
         Argx : constant Node_Id := Get_Pragma_Arg (Arg);

      begin
         if not Is_First_Subtype (Entity (Argx)) then
            Error_Pragma_Arg
              ("pragma% cannot apply to subtype", Argx);
         end if;
      end Check_First_Subtype;

      ---------------------------
      -- Check_In_Main_Program --
      ---------------------------

      procedure Check_In_Main_Program is
         P : constant Node_Id := Parent (N);

      begin
         --  Must be at in subprogram body

         if Nkind (P) /= N_Subprogram_Body then
            Error_Pragma ("% pragma allowed only in subprogram");

         --  Otherwise warn if obviously not main program

         elsif Present (Parameter_Specifications (Specification (P)))
           or else not Is_Compilation_Unit (Defining_Entity (P))
         then
            Error_Msg_Name_1 := Chars (N);
            Error_Msg_N
              ("?pragma% is only effective in main program", N);
         end if;
      end Check_In_Main_Program;

      -------------------------------------------
      -- Check_Is_In_Decl_Part_Or_Package_Spec --
      -------------------------------------------

      procedure Check_Is_In_Decl_Part_Or_Package_Spec is
         P : Node_Id;

      begin
         P := Parent (N);
         loop
            if No (P) then
               exit;

            elsif Nkind (P) = N_Handled_Sequence_Of_Statements then
               exit;

            elsif Nkind (P) = N_Package_Specification then
               return;

            elsif Nkind (P) = N_Block_Statement then
               return;

            --  Note: the following tests seem a little peculiar, because
            --  they test for bodies, but if we were in the statement part
            --  of the body, we would already have hit the handled statement
            --  sequence, so the only way we get here is by being in the
            --  declarative part of the body.

            elsif Nkind (P) = N_Subprogram_Body
              or else Nkind (P) = N_Package_Body
            then
               return;
            end if;

            P := Parent (P);
         end loop;

         Error_Pragma ("pragma% is not in declarative part or package spec");
      end Check_Is_In_Decl_Part_Or_Package_Spec;

      -------------------------
      -- Check_No_Identifier --
      -------------------------

      procedure Check_No_Identifier (Arg : Node_Id) is
      begin
         if Chars (Arg) /= No_Name then
            Error_Pragma_Arg_Ident
              ("pragma% does not permit identifier& here", Arg);
         end if;
      end Check_No_Identifier;

      --------------------------
      -- Check_No_Identifiers --
      --------------------------

      procedure Check_No_Identifiers is
         Arg_Node : Node_Id;
      begin
	 Arg_Node := Arg1;
	 for J in 1 .. Arg_Count loop
	    Check_No_Identifier (Arg_Node);
            Next (Arg_Node);
	 end loop;
      end Check_No_Identifiers;

      -------------------------------
      -- Check_Optional_Identifier --
      -------------------------------

      procedure Check_Optional_Identifier (Arg : Node_Id; Id : Name_Id) is
      begin
         if Present (Arg) and then Chars (Arg) /= No_Name then
            if Chars (Arg) /= Id then
               Error_Msg_Name_1 := Chars (N);
               Error_Msg_Name_2 := Id;
               Error_Msg_N ("pragma% argument expects identifier%", Arg);
               raise Pragma_Exit;
            end if;
         end if;
      end Check_Optional_Identifier;

      procedure Check_Optional_Identifier (Arg : Node_Id; Id : String) is
      begin
         Name_Buffer (1 .. Id'Length) := Id;
         Name_Len := Id'Length;
         Check_Optional_Identifier (Arg, Name_Find);
      end Check_Optional_Identifier;

      -----------------------------
      -- Check_Static_Constraint --
      -----------------------------

      --  Note: for convenience in writing this procedure, in addition to
      --  the officially (i.e. by spec) allowed argument which is always
      --  a constraint, it also allows ranges and discriminant associations.
      --  Above is not clear ???

      procedure Check_Static_Constraint (Constr : Node_Id) is

         --------------------
         -- Require_Static --
         --------------------

         procedure Require_Static (E : Node_Id);
         --  Require given expression to be static expression

         procedure Require_Static (E : Node_Id) is
         begin
            if not Is_OK_Static_Expression (E) then
               Flag_Non_Static_Expr
                 ("non-static constraint not allowed in Unchecked_Union!", E);
               raise Pragma_Exit;
            end if;
         end Require_Static;

      --  Start of processing for Check_Static_Constraint

      begin
         case Nkind (Constr) is
            when N_Discriminant_Association =>
               Require_Static (Expression (Constr));

            when N_Range =>
               Require_Static (Low_Bound (Constr));
               Require_Static (High_Bound (Constr));

            when N_Attribute_Reference =>
               Require_Static (Type_Low_Bound  (Etype (Prefix (Constr))));
               Require_Static (Type_High_Bound (Etype (Prefix (Constr))));

            when N_Range_Constraint =>
               Check_Static_Constraint (Range_Expression (Constr));

            when N_Index_Or_Discriminant_Constraint =>
               declare
                  IDC : Entity_Id := First (Constraints (Constr));
               begin
                  while Present (IDC) loop
                     Check_Static_Constraint (IDC);
                     Next (IDC);
                  end loop;
               end;

            when others =>
               null;
         end case;
      end Check_Static_Constraint;

      --------------------------------------
      -- Check_Valid_Configuration_Pragma --
      --------------------------------------

      --  A configuration pragma must appear in the context clause of
      --  a compilation unit, at the start of the list (i.e. only other
      --  pragmas may precede it).

      procedure Check_Valid_Configuration_Pragma is
      begin
         if not Is_Configuration_Pragma then
            Error_Pragma ("incorrect placement for configuration pragma%");
         end if;
      end Check_Valid_Configuration_Pragma;

      -------------------------------------
      -- Check_Valid_Library_Unit_Pragma --
      -------------------------------------

      procedure Check_Valid_Library_Unit_Pragma is
         Plist       : List_Id;
         Parent_Node : Node_Id;
         Unit_Name   : Entity_Id;
         Unit_Kind   : Node_Kind;
         Unit_Node   : Node_Id;
         Sindex      : Source_File_Index;

      begin
         if not Is_List_Member (N) then
            Pragma_Misplaced;

         else
            Plist := List_Containing (N);
            Parent_Node := Parent (Plist);

            if Parent_Node = Empty then
               Pragma_Misplaced;

            --  Case of pragma appearing after a compilation unit. In this
            --  case it must have an argument with the corresponding name
            --  and must be part of the following pragmas of its parent.

            elsif Nkind (Parent_Node) = N_Compilation_Unit_Aux then
               if Plist /= Pragmas_After (Parent_Node) then
                  Pragma_Misplaced;

               elsif Arg_Count = 0 then
                  Error_Pragma
                    ("argument required if outside compilation unit");

               else
                  Check_No_Identifiers;
                  Check_Arg_Count (1);
                  Unit_Node := Unit (Parent (Parent_Node));
                  Unit_Kind := Nkind (Unit_Node);

                  Analyze (Expression (Arg1));

                  if        Unit_Kind = N_Generic_Subprogram_Declaration
                    or else Unit_Kind = N_Subprogram_Declaration
                  then
                     Unit_Name := Defining_Entity (Unit_Node);

                  elsif     Unit_Kind = N_Function_Instantiation
                    or else Unit_Kind = N_Package_Instantiation
                    or else Unit_Kind = N_Procedure_Instantiation
                  then
                     Unit_Name := Defining_Entity (Unit_Node);

                  else
                     Unit_Name := Cunit_Entity (Current_Sem_Unit);
                  end if;

                  if Chars (Unit_Name) /=
                     Chars (Entity (Expression (Arg1)))
                  then
                     Error_Pragma_Arg
                       ("pragma% argument is not current unit name", Arg1);
                  end if;

                  if Ekind (Unit_Name) = E_Package
                    and then Present (Renamed_Entity (Unit_Name))
                  then
                     Error_Pragma ("pragma% not allowed for renamed package");
                  end if;
               end if;

            --  Pragma appears other than after a compilation unit

            else
               --  Here we check for the generic instantiation case and also
               --  for the case of processing a generic formal package. We
               --  detect these cases by noting that the Sloc on the node
               --  does not belong to the current compilation unit.

               Sindex := Source_Index (Current_Sem_Unit);

               if Loc not in Source_First (Sindex) .. Source_Last (Sindex) then
                  Rewrite (N, Make_Null_Statement (Loc));
                  return;

               --  If before first declaration, the pragma applies to the
               --  enclosing unit, and the name if present must be this name.

               elsif Is_Before_First_Decl (N, Plist) then
                  Unit_Node := Unit_Declaration_Node (Current_Scope);
                  Unit_Kind := Nkind (Unit_Node);

                  if Nkind (Parent (Unit_Node)) /= N_Compilation_Unit then
                     Pragma_Misplaced;

                  elsif Unit_Kind = N_Subprogram_Body
                    and then not Acts_As_Spec (Unit_Node)
                  then
                     Pragma_Misplaced;

                  elsif Nkind (Parent_Node) = N_Package_Body then
                     Pragma_Misplaced;

                  elsif Nkind (Parent_Node) = N_Package_Specification
                    and then Plist = Private_Declarations (Parent_Node)
                  then
                     Pragma_Misplaced;

                  elsif (Nkind (Parent_Node) = N_Generic_Package_Declaration
                          or else Nkind (Parent_Node)
                            = N_Generic_Subprogram_Declaration)
                    and then Plist = Generic_Formal_Declarations (Parent_Node)
                  then
                     Pragma_Misplaced;

                  elsif Arg_Count > 0 then
                     Analyze (Expression (Arg1));

                     if Entity (Expression (Arg1)) /= Current_Scope then
                        Error_Pragma_Arg
                          ("name in pragma% must be enclosing unit", Arg1);
                     end if;

                  --  It is legal to have no argument in this context

                  else
                     return;
                  end if;

               --  Error if not before first declaration. This is because a
               --  library unit pragma argument must be the name of a library
               --  unit (RM 10.1.5(7)), but the only names permitted in this
               --  context are (RM 10.1.5(6)) names of subprogram declarations,
               --  generic subprogram declarations or generic instantiations.

               else
                  Error_Pragma
                    ("pragma% misplaced, must be before first declaration");
               end if;
            end if;
         end if;
      end Check_Valid_Library_Unit_Pragma;

      ------------------
      -- Error_Pragma --
      ------------------

      procedure Error_Pragma (Msg : String) is
      begin
         Error_Msg_Name_1 := Chars (N);
         Error_Msg_N (Msg, N);
         raise Pragma_Exit;
      end Error_Pragma;

      ----------------------
      -- Error_Pragma_Arg --
      ----------------------

      procedure Error_Pragma_Arg (Msg : String; Arg : Node_Id) is
      begin
         Error_Msg_Name_1 := Chars (N);
         Error_Msg_N (Msg, Get_Pragma_Arg (Arg));
         raise Pragma_Exit;
      end Error_Pragma_Arg;

      procedure Error_Pragma_Arg (Msg1, Msg2 : String; Arg : Node_Id) is
      begin
         Error_Msg_Name_1 := Chars (N);
         Error_Msg_N (Msg1, Get_Pragma_Arg (Arg));
         Error_Pragma_Arg (Msg2, Arg);
      end Error_Pragma_Arg;

      ----------------------------
      -- Error_Pragma_Arg_Ident --
      ----------------------------

      procedure Error_Pragma_Arg_Ident (Msg : String; Arg : Node_Id) is
      begin
         Error_Msg_Name_1 := Chars (N);
         Error_Msg_N (Msg, Arg);
         raise Pragma_Exit;
      end Error_Pragma_Arg_Ident;

      ------------------------
      -- Find_Lib_Unit_Name --
      ------------------------

      function Find_Lib_Unit_Name return Entity_Id is
      begin
         --  Return inner compilation unit entity, for case of nested
         --  categorization pragmas. This happens in generic unit.

         if Nkind (Parent (N)) = N_Package_Specification
           and then Defining_Entity (Parent (N)) /= Current_Scope
         then
            return Defining_Entity (Parent (N));
         else
            return Current_Scope;
         end if;
      end Find_Lib_Unit_Name;

      ----------------------------
      -- Find_Program_Unit_Name --
      ----------------------------

      procedure Find_Program_Unit_Name (Id : Node_Id) is
         Unit_Name : Entity_Id;
         Unit_Kind : Node_Kind;
         P         : constant Node_Id := Parent (N);

      begin
         if Nkind (P) = N_Compilation_Unit then
            Unit_Kind := Nkind (Unit (P));

            if Unit_Kind = N_Subprogram_Declaration
              or else Unit_Kind = N_Package_Declaration
              or else Unit_Kind in N_Generic_Declaration
            then
               Unit_Name := Defining_Entity (Unit (P));

               if Chars (Id) = Chars (Unit_Name) then
                  Set_Entity (Id, Unit_Name);
                  Set_Etype (Id, Etype (Unit_Name));
               else
                  Set_Etype (Id, Any_Type);
                  Error_Pragma
                    ("cannot find program unit referenced by pragma%");
               end if;

            else
               Set_Etype (Id, Any_Type);
               Error_Pragma ("pragma% inapplicable to this unit");
            end if;

         else
            Analyze (Id);
         end if;

      end Find_Program_Unit_Name;

      -------------------------
      -- Gather_Associations --
      -------------------------

      procedure Gather_Associations
        (Names : Name_List;
         Args  : out Args_List)
      is
         Arg : Node_Id;

      begin
         --  Initialize all parameters to Empty

         for J in Args'Range loop
            Args (J) := Empty;
         end loop;

         --  That's all we have to do if there are no argument associations

         if No (Pragma_Argument_Associations (N)) then
            return;
         end if;

         --  Otherwise first deal with any positional parameters present

         Arg := First (Pragma_Argument_Associations (N));

         for Index in Args'Range loop
            exit when No (Arg) or else Chars (Arg) /= No_Name;
            Args (Index) := Expression (Arg);
            Next (Arg);
         end loop;

         --  Positional parameters all processed, if any left, then we
         --  have too many positional parameters.

         if Present (Arg) and then Chars (Arg) = No_Name then
            Error_Pragma_Arg
              ("too many positional associations for pragma%", Arg);
         end if;

         --  Process named parameters if any are present

         while Present (Arg) loop
            if Chars (Arg) = No_Name then
               Error_Pragma_Arg
                 ("positional association cannot follow named association",
                  Arg);

            else
               for Index in Names'Range loop
                  if Names (Index) = Chars (Arg) then
                     if Present (Args (Index)) then
                        Error_Pragma_Arg
                          ("duplicate argument association for pragma%", Arg);
                     else
                        Args (Index) := Expression (Arg);
                        exit;
                     end if;
                  end if;

                  if Index = Names'Last then
                     Error_Msg_Name_1 := Chars (N);
                     Error_Msg_N ("pragma% does not allow & argument", Arg);

                     --  Check for possible misspelling

                     for Index1 in Names'Range loop
                        if Is_Bad_Spelling_Of
                             (Get_Name_String (Chars (Arg)),
                              Get_Name_String (Names (Index1)))
                        then
                           Error_Msg_Name_1 := Names (Index1);
                           Error_Msg_N ("\possible misspelling of%", Arg);
                           exit;
                        end if;
                     end loop;

                     raise Pragma_Exit;
                  end if;
               end loop;
            end if;

            Next (Arg);
         end loop;
      end Gather_Associations;

      --------------------
      -- Get_Pragma_Arg --
      --------------------

      function Get_Pragma_Arg (Arg : Node_Id) return Node_Id is
      begin
         if Nkind (Arg) = N_Pragma_Argument_Association then
            return Expression (Arg);
         else
            return Arg;
         end if;
      end Get_Pragma_Arg;

      -----------------
      -- GNAT_Pragma --
      -----------------

      procedure GNAT_Pragma is
      begin
         Check_Restriction (No_Implementation_Pragmas, N);
      end GNAT_Pragma;

      --------------------------
      -- Is_Before_First_Decl --
      --------------------------

      function Is_Before_First_Decl
        (Pragma_Node : Node_Id;
         Decls       : List_Id) return Boolean
      is
         Item : Node_Id := First (Decls);

      begin
         --  Only other pragmas can come before this pragma

         loop
            if No (Item) or else Nkind (Item) /= N_Pragma then
               return False;

            elsif Item = Pragma_Node then
               return True;
            end if;

            Next (Item);
         end loop;
      end Is_Before_First_Decl;

      -----------------------------
      -- Is_Configuration_Pragma --
      -----------------------------

      --  A configuration pragma must appear in the context clause of
      --  a compilation unit, at the start of the list (i.e. only other
      --  pragmas may precede it).

      function Is_Configuration_Pragma return Boolean is
         Lis : constant List_Id := List_Containing (N);
         Par : constant Node_Id := Parent (N);
         Prg : Node_Id;

      begin
         --  If no parent, then we are in the configuration pragma file,
         --  so the placement is definitely appropriate.

         if No (Par) then
            return True;

         --  Otherwise we must be in the context clause of a compilation unit
         --  and the only thing allowed before us in the context list is more
         --  configuration pragmas.

         elsif Nkind (Par) = N_Compilation_Unit
           and then Context_Items (Par) = Lis
         then
            Prg := First (Lis);

            loop
               if Prg = N then
                  return True;
               elsif Nkind (Prg) /= N_Pragma then
                  return False;
               end if;

               Next (Prg);
            end loop;

         else
            return False;
         end if;
      end Is_Configuration_Pragma;

      ----------------------
      -- Pragma_Misplaced --
      ----------------------

      procedure Pragma_Misplaced is
      begin
         Error_Pragma ("incorrect placement of pragma%");
      end Pragma_Misplaced;

      ------------------------------------
      -- Process Atomic_Shared_Volatile --
      ------------------------------------


      ------------------------
      -- Process_Convention --
      ------------------------

      procedure Process_Convention
        (C : out Convention_Id;
         E : out Entity_Id)
      is
         Id        : Node_Id;
         E1        : Entity_Id;
         Comp_Unit : Unit_Number_Type;
         Cname     : Name_Id;

         procedure Set_Convention_From_Pragma (E : Entity_Id);
         --  Set convention in entity E, and also flag that the entity has a
         --  convention pragma. If entity is for a private or incomplete type,
         --  also set convention and flag on underlying type. This procedure
         --  also deals with the special case of C_Pass_By_Copy convention.

         --------------------------------
         -- Set_Convention_From_Pragma --
         --------------------------------

         procedure Set_Convention_From_Pragma (E : Entity_Id) is
         begin
            Set_Convention (E, C);
            Set_Has_Convention_Pragma (E);

            if Is_Incomplete_Or_Private_Type (E) then
               Set_Convention            (Underlying_Type (E), C);
               Set_Has_Convention_Pragma (Underlying_Type (E), True);
            end if;

            --  A class-wide type should inherit the convention of
            --  the specific root type (although this isn't specified
            --  clearly by the RM).

            if Is_Type (E) and then Present (Class_Wide_Type (E)) then
               Set_Convention (Class_Wide_Type (E), C);
            end if;

            --  If the entity is a record type, then check for special case
            --  of C_Pass_By_Copy, which is treated the same as C except that
            --  the special record flag is set. This convention is also only
            --  permitted on record types (see AI95-00131).

            if Cname = Name_C_Pass_By_Copy then
               if Is_Record_Type (E) then
                  Set_C_Pass_By_Copy (Base_Type (E));
               elsif Is_Incomplete_Or_Private_Type (E)
                 and then Is_Record_Type (Underlying_Type (E))
               then
                  Set_C_Pass_By_Copy (Base_Type (Underlying_Type (E)));
               else
                  Error_Pragma_Arg
                    ("C_Pass_By_Copy convention allowed only for record type",
                     Arg2);
               end if;
            end if;

            --  If the entity is a derived boolean type, check for the
            --  special case of convention C, C++, or Fortran, where we
            --  consider any nonzero value to represent true.

            if Is_Discrete_Type (E)
              and then Root_Type (Etype (E)) = Standard_Boolean
              and then
                (C = Convention_C)
            then
               Set_Nonzero_Is_True (Base_Type (E));
            end if;
         end Set_Convention_From_Pragma;

      --  Start of processing for Process_Convention

      begin
         Check_At_Least_N_Arguments (2);
         Check_Arg_Is_Identifier (Arg1);
         Check_Optional_Identifier (Arg1, Name_Convention);
         Cname := Chars (Expression (Arg1));

         --  C_Pass_By_Copy is treated as a synonym for convention C
         --  (this is tested again below to set the critical flag)

         if Cname = Name_C_Pass_By_Copy then
            C := Convention_C;

         --  Otherwise we must have something in the standard convention list

         elsif Is_Convention_Name (Cname) then
            C := Get_Convention_Id (Chars (Expression (Arg1)));

         --  In DEC VMS, it seems that there is an undocumented feature
         --  that any unrecognized convention is treated as the default,
         --  which for us is convention C. It does not seem so terrible
         --  to do this unconditionally, silently in the VMS case, and
         --  with a warning in the non-VMS case.

         else
            if Warn_On_Export_Import and not OpenVMS_On_Target then
               Error_Msg_N
                 ("?unrecognized convention name, C assumed",
                  Expression (Arg1));
            end if;

            C := Convention_C;
         end if;

         Check_Arg_Is_Local_Name (Arg2);
         Check_Optional_Identifier (Arg2, Name_Entity);

         Id := Expression (Arg2);
         Analyze (Id);

         if not Is_Entity_Name (Id) then
            Error_Pragma_Arg ("entity name required", Arg2);
         end if;

         E := Entity (Id);

         --  Go to renamed subprogram if present, since convention applies
         --  to the actual renamed entity, not to the renaming entity.

         if Is_Subprogram (E)
           and then Present (Alias (E))
           and then Nkind (Parent (Declaration_Node (E))) =
                      N_Subprogram_Renaming_Declaration
         then
            E := Alias (E);
         end if;

         --  Check that we not applying this to a specless body

         if Is_Subprogram (E)
           and then Nkind (Parent (Declaration_Node (E))) = N_Subprogram_Body
         then
            Error_Pragma
              ("pragma% requires separate spec and must come before body");
         end if;

         --  Check that we are not applying this to a named constant

         if Ekind (E) = E_Named_Integer
              or else
            Ekind (E) = E_Named_Real
         then
            Error_Msg_Name_1 := Chars (N);
            Error_Msg_N
              ("cannot apply pragma% to named constant!",
               Get_Pragma_Arg (Arg2));
            Error_Pragma_Arg
              ("\supply appropriate type for&!", Arg2);
         end if;

         if Etype (E) = Any_Type
           or else Rep_Item_Too_Early (E, N)
         then
            raise Pragma_Exit;
         else
            E := Underlying_Type (E);
         end if;

         if Rep_Item_Too_Late (E, N) then
            raise Pragma_Exit;
         end if;

         if Has_Convention_Pragma (E) then
            Error_Pragma_Arg
              ("at most one Convention/Export/Import pragma is allowed", Arg2);

         end if;

         --  For Intrinsic, a subprogram is required

         if C = Convention_Intrinsic
           and then not Is_Subprogram (E)
           and then not Is_Generic_Subprogram (E)
         then
            Error_Pragma_Arg
              ("second argument of pragma% must be a subprogram", Arg2);
         end if;

         --  For Stdcall, a subprogram, variable or subprogram type is required

         if C = Convention_Stdcall
           and then not Is_Subprogram (E)
           and then not Is_Generic_Subprogram (E)
           and then Ekind (E) /= E_Variable
           and then not
             (Is_Access_Type (E)
              and then Ekind (Designated_Type (E)) = E_Subprogram_Type)
         then
            Error_Pragma_Arg
              ("second argument of pragma% must be subprogram (type)",
               Arg2);
         end if;

         if not Is_Subprogram (E)
           and then not Is_Generic_Subprogram (E)
         then
            Set_Convention_From_Pragma (E);

            if Is_Type (E) then

               Check_First_Subtype (Arg2);
               Set_Convention_From_Pragma (Base_Type (E));

               --  For subprograms, we must set the convention on the
               --  internally generated directly designated type as well.

               if Ekind (E) = E_Access_Subprogram_Type then
                  Set_Convention_From_Pragma (Directly_Designated_Type (E));
               end if;
            end if;

         --  For the subprogram case, set proper convention for all homonyms
         --  in same compilation unit.
         --  Is the test of compilation unit really necessary ???
         --  What about subprogram renamings here???

         else
            Comp_Unit := Get_Source_Unit (E);
            Set_Convention_From_Pragma (E);

            --  Treat a pragma Import as an implicit body, for GPS use.

            if Prag_Id = Pragma_Import then
                  Generate_Reference (E, Id, 'b');
            end if;

            E1 := E;
            loop
               E1 := Homonym (E1);
               exit when No (E1) or else Scope (E1) /= Current_Scope;

               --  Note: below we are missing a check for Rep_Item_Too_Late.
               --  That is deliberate, we cannot chain the rep item on more
               --  than one Rep_Item chain, to be fixed later ???

               if Comp_Unit = Get_Source_Unit (E1) then
                  Set_Convention_From_Pragma (E1);

                  if Prag_Id = Pragma_Import then
                     Generate_Reference (E, Id, 'b');
                  end if;
               end if;
            end loop;
         end if;
      end Process_Convention;

      -----------------------------------------------------
      -- Process_Extended_Import_Export_Exception_Pragma --
      -----------------------------------------------------

      procedure Process_Extended_Import_Export_Exception_Pragma
        (Arg_Internal : Node_Id;
         Arg_External : Node_Id;
         Arg_Form     : Node_Id;
         Arg_Code     : Node_Id)
      is
         Def_Id   : Entity_Id;
         Code_Val : Uint;

      begin
         GNAT_Pragma;

         if not OpenVMS_On_Target then
            Error_Pragma
              ("?pragma% ignored (applies only to Open'V'M'S)");
         end if;

         Process_Extended_Import_Export_Internal_Arg (Arg_Internal);
         Def_Id := Entity (Arg_Internal);

         Error_Pragma_Arg
           ("pragma% must refer to declared exception", Arg_Internal);

         Set_Extended_Import_Export_External_Name (Def_Id, Arg_External);

         if Present (Arg_Form) then
            Check_Arg_Is_One_Of (Arg_Form, Name_Ada, Name_VMS);
         end if;

         if Present (Arg_Form)
           and then Chars (Arg_Form) = Name_Ada
         then
            null;
         end if;

         if Present (Arg_Code) then
               Error_Pragma_Arg
                 ("Code option for pragma% not allowed for Ada case",
                  Arg_Code);

            Check_Arg_Is_Static_Expression (Arg_Code, Any_Integer);
            Code_Val := Expr_Value (Arg_Code);

            if not UI_Is_In_Int_Range (Code_Val) then
               Error_Pragma_Arg
                 ("Code option for pragma% must be in 32-bit range",
                  Arg_Code);

            end if;
         end if;
      end Process_Extended_Import_Export_Exception_Pragma;

      -------------------------------------------------
      -- Process_Extended_Import_Export_Internal_Arg --
      -------------------------------------------------

      procedure Process_Extended_Import_Export_Internal_Arg
        (Arg_Internal : Node_Id := Empty)
      is
      begin
         GNAT_Pragma;

         if No (Arg_Internal) then
            Error_Pragma ("Internal parameter required for pragma%");
         end if;

         if Nkind (Arg_Internal) = N_Identifier then
            null;

         elsif Nkind (Arg_Internal) = N_Operator_Symbol
           and then (Prag_Id = Pragma_Import_Function
                       or else
                     Prag_Id = Pragma_Export_Function)
         then
            null;

         else
            Error_Pragma_Arg
              ("wrong form for Internal parameter for pragma%", Arg_Internal);
         end if;

         Check_Arg_Is_Local_Name (Arg_Internal);
      end Process_Extended_Import_Export_Internal_Arg;

      --------------------------------------------------
      -- Process_Extended_Import_Export_Object_Pragma --
      --------------------------------------------------

      procedure Process_Extended_Import_Export_Object_Pragma
        (Arg_Internal : Node_Id;
         Arg_External : Node_Id;
         Arg_Size     : Node_Id)
      is
         Def_Id : Entity_Id;

      begin
         Process_Extended_Import_Export_Internal_Arg (Arg_Internal);
         Def_Id := Entity (Arg_Internal);

         if Ekind (Def_Id) /= E_Constant
           and then Ekind (Def_Id) /= E_Variable
         then
            Error_Pragma_Arg
              ("pragma% must designate an object", Arg_Internal);
         end if;

         if Is_Psected (Def_Id) then
            Error_Pragma_Arg
              ("previous Psect_Object applies, pragma % not permitted",
               Arg_Internal);
         end if;

         if Rep_Item_Too_Late (Def_Id, N) then
            raise Pragma_Exit;
         end if;

         Set_Extended_Import_Export_External_Name (Def_Id, Arg_External);

         if Present (Arg_Size)
           and then Nkind (Arg_Size) /= N_Identifier
           and then Nkind (Arg_Size) /= N_String_Literal
         then
            Error_Pragma_Arg
              ("pragma% Size argument must be identifier or string literal",
               Arg_Size);
         end if;

         --  Export_Object case

         if Prag_Id = Pragma_Export_Object then
            if not Is_Library_Level_Entity (Def_Id) then
               Error_Pragma_Arg
                 ("argument for pragma% must be library level entity",
                  Arg_Internal);
            end if;

            if Ekind (Current_Scope) = E_Generic_Package then
               Error_Pragma ("pragma& cannot appear in a generic unit");
            end if;

            if not Size_Known_At_Compile_Time (Etype (Def_Id)) then
               Error_Pragma_Arg
                 ("exported object must have compile time known size",
                  Arg_Internal);
            end if;

            if Warn_On_Export_Import and then Is_Exported (Def_Id) then
               Error_Msg_N
                 ("?duplicate Export_Object pragma", N);
            else
               Set_Exported (Def_Id, Arg_Internal);
            end if;

         --  Import_Object case

         else
            if Ekind (Def_Id) = E_Constant then
               Error_Pragma_Arg
                 ("cannot import a constant", Arg_Internal);
            end if;

            if Warn_On_Export_Import
              and then Is_Access_Type (Etype (Def_Id))
            then
               Error_Pragma_Arg
                 ("cannot import object of an access type?", Arg_Internal);
            end if;

            if Warn_On_Export_Import
              and then Is_Imported (Def_Id)
            then
               Error_Msg_N
                 ("?duplicate Import_Object pragma", N);

            --  Check for explicit initialization present. Note that an
            --  initialization that generated by the code generator, e.g.
            --  for an access type, does not count here.

            elsif Present (Expression (Parent (Def_Id)))
               and then
                 Comes_From_Source
                   (Original_Node (Expression (Parent (Def_Id))))
            then
               Error_Msg_Sloc := Sloc (Def_Id);
               Error_Pragma_Arg
                 ("no initialization allowed for declaration of& #",
                  "\imported entities cannot be initialized ('R'M' 'B.1(24))",
                  Arg1);
            else
               Set_Imported (Def_Id);
               Note_Possible_Modification (Arg_Internal);
            end if;
         end if;
      end Process_Extended_Import_Export_Object_Pragma;

      ------------------------------------------------------
      -- Process_Extended_Import_Export_Subprogram_Pragma --
      ------------------------------------------------------

      procedure Process_Extended_Import_Export_Subprogram_Pragma
        (Arg_Internal                 : Node_Id;
         Arg_External                 : Node_Id;
         Arg_Parameter_Types          : Node_Id;
         Arg_Result_Type              : Node_Id := Empty;
         Arg_Mechanism                : Node_Id;
         Arg_Result_Mechanism         : Node_Id := Empty;
         Arg_First_Optional_Parameter : Node_Id := Empty)
      is
         Ent       : Entity_Id;
         Def_Id    : Entity_Id;
         Hom_Id    : Entity_Id;
         Formal    : Entity_Id;
         Ambiguous : Boolean;
         Match     : Boolean;
         Dval      : Node_Id;

         function Same_Base_Type
          (Ptype  : Node_Id;
           Formal : Entity_Id) return Boolean;
         --  Determines if Ptype references the type of Formal. Note that
         --  only the base types need to match according to the spec. Ptype
         --  here is the argument from the pragma, which is either a type
         --  name, or an access attribute.

         --------------------
         -- Same_Base_Type --
         --------------------

         function Same_Base_Type
           (Ptype  : Node_Id;
            Formal : Entity_Id) return Boolean
         is
            Ftyp : constant Entity_Id := Base_Type (Etype (Formal));
            Pref : Node_Id;

         begin
            --  Case where pragma argument is typ'Access

            if Nkind (Ptype) = N_Attribute_Reference
              and then Attribute_Name (Ptype) = Name_Access
            then
               Pref := Prefix (Ptype);
               Find_Type (Pref);

               if not Is_Entity_Name (Pref)
                 or else Entity (Pref) = Any_Type
               then
                  raise Pragma_Exit;
               end if;

               --  We have a match if the corresponding argument is of an
               --  anonymous access type, and its designicated type matches
               --  the type of the prefix of the access attribute

               return Ekind (Ftyp) = E_Anonymous_Access_Type
                 and then Base_Type (Entity (Pref)) =
                            Base_Type (Etype (Designated_Type (Ftyp)));

            --  Case where pragma argument is a type name

            else
               Find_Type (Ptype);

               if not Is_Entity_Name (Ptype)
                 or else Entity (Ptype) = Any_Type
               then
                  raise Pragma_Exit;
               end if;

               --  We have a match if the corresponding argument is of
               --  the type given in the pragma (comparing base types)

               return Base_Type (Entity (Ptype)) = Ftyp;
            end if;
         end Same_Base_Type;

      --  Start of processing for
      --  Process_Extended_Import_Export_Subprogram_Pragma

      begin
         Process_Extended_Import_Export_Internal_Arg (Arg_Internal);
         Hom_Id := Entity (Arg_Internal);
         Ent := Empty;
         Ambiguous := False;

         --  Loop through homonyms (overloadings) of Hom_Id

         while Present (Hom_Id) loop
            Def_Id := Get_Base_Subprogram (Hom_Id);

            --  We need a subprogram in the current scope

            if not Is_Subprogram (Def_Id)
              or else Scope (Def_Id) /= Current_Scope
            then
               null;

            else
               Match := True;

               --  Pragma cannot apply to subprogram body

               if Is_Subprogram (Def_Id)
                 and then
                   Nkind (Parent
                     (Declaration_Node (Def_Id))) = N_Subprogram_Body
               then
                  Error_Pragma
                    ("pragma% requires separate spec"
                      & " and must come before body");
               end if;

               --  Test result type if given, note that the result type
               --  parameter can only be present for the function cases.

               if Present (Arg_Result_Type)
                 and then not Same_Base_Type (Arg_Result_Type, Def_Id)
               then
                  Match := False;

               elsif Etype (Def_Id) /= Standard_Void_Type
                 and then
                   (Chars (N) = Name_Export_Procedure
                      or else Chars (N) = Name_Import_Procedure)
               then
                  Match := False;

               --  Test parameter types if given. Note that this parameter
               --  has not been analyzed (and must not be, since it is
               --  semantic nonsense), so we get it as the parser left it.

               elsif Present (Arg_Parameter_Types) then
                  Check_Matching_Types : declare
                     Formal : Entity_Id;
                     Ptype  : Node_Id;

                  begin
                     Formal := First_Formal (Def_Id);

                     if Nkind (Arg_Parameter_Types) = N_Null then
                        if Present (Formal) then
                           Match := False;
                        end if;

                     --  A list of one type, e.g. (List) is parsed as
                     --  a parenthesized expression.

                     elsif Nkind (Arg_Parameter_Types) /= N_Aggregate
                       and then Paren_Count (Arg_Parameter_Types) = 1
                     then
                        if No (Formal)
                          or else Present (Next_Formal (Formal))
                        then
                           Match := False;
                        else
                           Match :=
                             Same_Base_Type (Arg_Parameter_Types, Formal);
                        end if;

                     --  A list of more than one type is parsed as a aggregate

                     elsif Nkind (Arg_Parameter_Types) = N_Aggregate
                       and then Paren_Count (Arg_Parameter_Types) = 0
                     then
                        Ptype := First (Expressions (Arg_Parameter_Types));

                        while Present (Ptype) or else Present (Formal) loop
                           if No (Ptype)
                             or else No (Formal)
                             or else not Same_Base_Type (Ptype, Formal)
                           then
                              Match := False;
                              exit;
                           else
                              Next_Formal (Formal);
                              Next (Ptype);
                           end if;
                        end loop;

                     --  Anything else is of the wrong form

                     else
                        Error_Pragma_Arg
                          ("wrong form for Parameter_Types parameter",
                           Arg_Parameter_Types);
                     end if;
                  end Check_Matching_Types;
               end if;

               --  Match is now False if the entry we found did not match
               --  either a supplied Parameter_Types or Result_Types argument

               if Match then
                  if No (Ent) then
                     Ent := Def_Id;

                  --  Ambiguous case, the flag Ambiguous shows if we already
                  --  detected this and output the initial messages.

                  else
                     if not Ambiguous then
                        Ambiguous := True;
                        Error_Msg_Name_1 := Chars (N);
                        Error_Msg_N
                          ("pragma% does not uniquely identify subprogram!",
                           N);
                        Error_Msg_Sloc := Sloc (Ent);
                        Error_Msg_N ("matching subprogram #!", N);
                        Ent := Empty;
                     end if;

                     Error_Msg_Sloc := Sloc (Def_Id);
                     Error_Msg_N ("matching subprogram #!", N);
                  end if;
               end if;
            end if;

            Hom_Id := Homonym (Hom_Id);
         end loop;

         --  See if we found an entry

         if No (Ent) then
            if not Ambiguous then
               if Is_Generic_Subprogram (Entity (Arg_Internal)) then
                  Error_Pragma
                    ("pragma% cannot be given for generic subprogram");

               else
                  Error_Pragma
                    ("pragma% does not identify local subprogram");
               end if;
            end if;

            return;
         end if;

         --  Import pragmas must be be for imported entities

         if Prag_Id = Pragma_Import_Function
              or else
            Prag_Id = Pragma_Import_Procedure
         then
            if not Is_Imported (Ent) then
               Error_Pragma
                 ("pragma Import or Interface must precede pragma%");
            end if;

         --  Here we have the Export case which can set the entity as exported

         --  But does not do so if the specified external name is null,
         --  since that is taken as a signal in DEC Ada 83 (with which
         --  we want to be compatible) to request no external name.

         elsif Nkind (Arg_External) = N_String_Literal
           and then String_Length (Strval (Arg_External)) = 0
         then
            null;

         --  In all other cases, set entit as exported

         else
            Set_Exported (Ent, Arg_Internal);
         end if;

         Set_Extended_Import_Export_External_Name (Ent, Arg_External);

         --  Process Result_Mechanism argument if present. We have already
         --  checked that this is only allowed for the function case.

         if Present (Arg_Result_Mechanism) then
            Set_Mechanism_Value (Ent, Arg_Result_Mechanism);
         end if;

         --  Process Mechanism parameter if present. Note that this parameter
         --  is not analyzed, and must not be analyzed since it is semantic
         --  nonsense, so we get it in exactly as the parser left it.

         if Present (Arg_Mechanism) then
            declare
               Formal : Entity_Id;
               Massoc : Node_Id;
               Mname  : Node_Id;
               Choice : Node_Id;

            begin
               --  A single mechanism association without a formal parameter
               --  name is parsed as a parenthesized expression. All other
               --  cases are parsed as aggregates, so we rewrite the single
               --  parameter case as an aggregate for consistency.

               if Nkind (Arg_Mechanism) /= N_Aggregate
                 and then Paren_Count (Arg_Mechanism) = 1
               then
                  Rewrite (Arg_Mechanism,
                    Make_Aggregate (Sloc (Arg_Mechanism),
                      Expressions => New_List (
                        Relocate_Node (Arg_Mechanism))));
               end if;

               --  Case of only mechanism name given, applies to all formals

               if Nkind (Arg_Mechanism) /= N_Aggregate then
                  Formal := First_Formal (Ent);
                  while Present (Formal) loop
                     Set_Mechanism_Value (Formal, Arg_Mechanism);
                     Next_Formal (Formal);
                  end loop;

               --  Case of list of mechanism associations given

               else
                  if Null_Record_Present (Arg_Mechanism) then
                     Error_Pragma_Arg
                       ("inappropriate form for Mechanism parameter",
                        Arg_Mechanism);
                  end if;

                  --  Deal with positional ones first

                  Formal := First_Formal (Ent);
                  if Present (Expressions (Arg_Mechanism)) then
                     Mname := First (Expressions (Arg_Mechanism));

                     while Present (Mname) loop
                        if No (Formal) then
                           Error_Pragma_Arg
                             ("too many mechanism associations", Mname);
                        end if;

                        Set_Mechanism_Value (Formal, Mname);
                        Next_Formal (Formal);
                        Next (Mname);
                     end loop;
                  end if;

                  --  Deal with named entries

                  if Present (Component_Associations (Arg_Mechanism)) then
                     Massoc := First (Component_Associations (Arg_Mechanism));

                     while Present (Massoc) loop
                        Choice := First (Choices (Massoc));

                        if Nkind (Choice) /= N_Identifier
                          or else Present (Next (Choice))
                        then
                           Error_Pragma_Arg
                             ("incorrect form for mechanism association",
                              Massoc);
                        end if;

                        Formal := First_Formal (Ent);
                        loop
                           if No (Formal) then
                              Error_Pragma_Arg
                                ("parameter name & not present", Choice);
                           end if;

                           if Chars (Choice) = Chars (Formal) then
                              Set_Mechanism_Value
                                (Formal, Expression (Massoc));
                              exit;
                           end if;

                           Next_Formal (Formal);
                        end loop;

                        Next (Massoc);
                     end loop;
                  end if;
               end if;
            end;
         end if;

         --  Process First_Optional_Parameter argument if present. We have
         --  already checked that this is only allowed for the Import case.

         if Present (Arg_First_Optional_Parameter) then
            if Nkind (Arg_First_Optional_Parameter) /= N_Identifier then
               Error_Pragma_Arg
                 ("first optional parameter must be formal parameter name",
                  Arg_First_Optional_Parameter);
            end if;

            Formal := First_Formal (Ent);
            loop
               if No (Formal) then
                  Error_Pragma_Arg
                    ("specified formal parameter& not found",
                     Arg_First_Optional_Parameter);
               end if;

               exit when Chars (Formal) =
                         Chars (Arg_First_Optional_Parameter);

               Next_Formal (Formal);
            end loop;

            Set_First_Optional_Parameter (Ent, Formal);

            --  Check specified and all remaining formals have right form

            while Present (Formal) loop
               if Ekind (Formal) /= E_In_Parameter then
                  Error_Msg_NE
                    ("optional formal& is not of mode in!",
                     Arg_First_Optional_Parameter, Formal);

               else
                  Dval := Default_Value (Formal);

                  if not Present (Dval) then
                     Error_Msg_NE
                       ("optional formal& does not have default value!",
                        Arg_First_Optional_Parameter, Formal);

                  elsif Compile_Time_Known_Value_Or_Aggr (Dval) then
                     null;

                  else
                     Error_Msg_FE
                       ("default value for optional formal& is non-static!",
                        Arg_First_Optional_Parameter, Formal);
                  end if;
               end if;

               Set_Is_Optional_Parameter (Formal);
               Next_Formal (Formal);
            end loop;
         end if;
      end Process_Extended_Import_Export_Subprogram_Pragma;

      --------------------------
      -- Process_Generic_List --
      --------------------------

      procedure Process_Generic_List is
         Arg : Node_Id;
         Exp : Node_Id;

      begin
         GNAT_Pragma;
         Check_No_Identifiers;
         Check_At_Least_N_Arguments (1);

         Arg := Arg1;
         while Present (Arg) loop
            Exp := Expression (Arg);
            Analyze (Exp);

            if not Is_Entity_Name (Exp)
              or else
                (not Is_Generic_Instance (Entity (Exp))
                  and then
                 not Is_Generic_Unit (Entity (Exp)))
            then
               Error_Pragma_Arg
                 ("pragma% argument must be name of generic unit/instance",
                  Arg);
            end if;

            Next (Arg);
         end loop;
      end Process_Generic_List;

      ---------------------------------
      -- Process_Import_Or_Interface --
      ---------------------------------

      procedure Process_Import_Or_Interface is
         C      : Convention_Id;
         Def_Id : Entity_Id;
         Hom_Id : Entity_Id;

      begin
         Process_Convention (C, Def_Id);
         Kill_Size_Check_Code (Def_Id);
         Note_Possible_Modification (Expression (Arg2));

         if Ekind (Def_Id) = E_Variable
              or else
            Ekind (Def_Id) = E_Constant
         then
            --  User initialization is not allowed for imported object, but
            --  the object declaration may contain a default initialization,
            --  that will be discarded. Note that an explicit initialization
            --  only counts if it comes from source, otherwise it is simply
            --  the code generator making an implicit initialization explicit.

            if Present (Expression (Parent (Def_Id)))
               and then Comes_From_Source (Expression (Parent (Def_Id)))
            then
               Error_Msg_Sloc := Sloc (Def_Id);
               Error_Pragma_Arg
                 ("no initialization allowed for declaration of& #",
                  "\imported entities cannot be initialized ('R'M' 'B.1(24))",
                  Arg2);

            else
               Set_Imported (Def_Id);
               Set_Is_Public (Def_Id);
               Process_Interface_Name (Def_Id, Arg3, Arg4);

               --  It is not possible to import a constant of an unconstrained
               --  array type (e.g. string) because there is no simple way to
               --  write a meaningful subtype for it.

               if Is_Array_Type (Etype (Def_Id))
                 and then not Is_Constrained (Etype (Def_Id))
               then
                  Error_Msg_NE
                    ("imported constant& must have a constrained subtype",
                      N, Def_Id);
               end if;
            end if;

         elsif Is_Subprogram (Def_Id)
           or else Is_Generic_Subprogram (Def_Id)
         then
            --  If the name is overloaded, pragma applies to all of the
            --  denoted entities in the same declarative part.

            Hom_Id := Def_Id;

            while Present (Hom_Id) loop
               Def_Id := Get_Base_Subprogram (Hom_Id);

               --  Ignore inherited subprograms because the pragma will
               --  apply to the parent operation, which is the one called.

               if Is_Overloadable (Def_Id)
                 and then Present (Alias (Def_Id))
               then
                  null;

               --  If it is not a subprogram, it must be in an outer
               --  scope and pragma does not apply.

               elsif not Is_Subprogram (Def_Id)
                 and then not Is_Generic_Subprogram (Def_Id)
               then
                  null;

               --  Verify that the homonym is in the same declarative
               --  part (not just the same scope).

               elsif Parent (Unit_Declaration_Node (Def_Id)) /= Parent (N)
                 and then Nkind (Parent (N)) /= N_Compilation_Unit_Aux
               then
                  exit;

               else
                  Set_Imported (Def_Id);

                  --  If Import intrinsic, set intrinsic flag
                  --  and verify that it is known as such.

                  if C = Convention_Intrinsic then
                     Set_Is_Intrinsic_Subprogram (Def_Id);
                     Check_Intrinsic_Subprogram
                       (Def_Id, Expression (Arg2));
                  end if;

                  --  All interfaced procedures need an external
                  --  symbol created for them since they are
                  --  always referenced from another object file.

                  Set_Is_Public (Def_Id);

                  --  Verify that the subprogram does not have a completion
                  --  through a renaming declaration. For other completions
                  --  the pragma appears as a too late representation.

                  declare
                     Decl : constant Node_Id := Unit_Declaration_Node (Def_Id);

                  begin
                     if Present (Decl)
                       and then Nkind (Decl) = N_Subprogram_Declaration
                       and then Present (Corresponding_Body (Decl))
                       and then
                         Nkind
                           (Unit_Declaration_Node
                             (Corresponding_Body (Decl))) =
                                             N_Subprogram_Renaming_Declaration
                     then
                        Error_Msg_Sloc := Sloc (Def_Id);
                        Error_Msg_NE ("cannot import&#," &
                           " already completed by a renaming",
                           N, Def_Id);
                     end if;
                  end;

                  Set_Has_Completion (Def_Id);
                  Process_Interface_Name (Def_Id, Arg3, Arg4);
               end if;

               if Is_Compilation_Unit (Hom_Id) then

                  --  Its possible homonyms are not affected by the pragma.
                  --  Such homonyms might be present in the context of other
                  --  units being compiled.

                  exit;

               else
                  Hom_Id := Homonym (Hom_Id);
               end if;
            end loop;

         else
            Error_Pragma_Arg
              ("second argument of pragma% must be object or subprogram",
               Arg2);
         end if;

         --  If this pragma applies to a compilation unit, then the unit,
         --  which is a subprogram, does not require (or allow) a body.
         --  We also do not need to elaborate imported procedures.

         if Nkind (Parent (N)) = N_Compilation_Unit_Aux then
            declare
               Cunit : constant Node_Id := Parent (Parent (N));
            begin
               Set_Body_Required (Cunit, False);
            end;
         end if;
      end Process_Import_Or_Interface;

      --------------------
      -- Process_Inline --
      --------------------

      procedure Process_Inline (Active : Boolean) is
         Assoc   : Node_Id;
         Decl    : Node_Id;
         Subp_Id : Node_Id;
         Subp    : Entity_Id;
         Applies : Boolean;

         procedure Make_Inline (Subp : Entity_Id);
         --  Subp is the defining unit name of the subprogram
         --  declaration. Set the flag, as well as the flag in the
         --  corresponding body, if there is one present.

         procedure Set_Inline_Flags (Subp : Entity_Id);
         --  Sets Is_Inlined and Has_Pragma_Inline flags for Subp

         function Back_End_Cannot_Inline (Subp : Entity_Id) return Boolean;
         --  Do not set the inline flag if body is available and contains
         --  exception handlers, to prevent undefined symbols at link time.

         ----------------------------
         -- Back_End_Cannot_Inline --
         ----------------------------

         function Back_End_Cannot_Inline (Subp : Entity_Id) return Boolean is
            Decl : constant Node_Id := Unit_Declaration_Node (Subp);

         begin
            if Nkind (Decl) = N_Subprogram_Declaration
              and then Present (Corresponding_Body (Decl))
            then
               --  If the subprogram is a renaming as body, the body is
               --  just a call to the renamed subprogram, and inlining is
               --  trivially possible.

               if Nkind (Unit_Declaration_Node (Corresponding_Body (Decl))) =
                                            N_Subprogram_Renaming_Declaration
               then
                  return False;

               else
                  return False;
               end if;
            else
               --  If body is not available, assume the best, the check is
               --  performed again when compiling enclosing package bodies.

               return False;
            end if;
         end Back_End_Cannot_Inline;

         -----------------
         -- Make_Inline --
         -----------------

         procedure Make_Inline (Subp : Entity_Id) is
            Kind       : constant Entity_Kind := Ekind (Subp);
            Inner_Subp : Entity_Id   := Subp;

         begin
            if Etype (Subp) = Any_Type then
               return;

            elsif Back_End_Cannot_Inline (Subp) then
               Applies := True;    --  Do not treat as an error.
               return;

            --  Here we have a candidate for inlining, but we must exclude
            --  derived operations. Otherwise we will end up trying to
            --  inline a phantom declaration, and the result would be to
            --  drag in a body which has no direct inlining associated with
            --  it. That would not only be inefficient but would also result
            --  in the backend doing cross-unit inlining in cases where it
            --  was definitely inappropriate to do so.

            --  However, a simple Comes_From_Source test is insufficient,
            --  since we do want to allow inlining of generic instances,
            --  which also do not come from source. Predefined operators do
            --  not come from source but are not inlineable either.

            elsif not Comes_From_Source (Subp)
              and then not Is_Generic_Instance (Subp)
              and then Scope (Subp) /= Standard_Standard
            then
               Applies := True;
               return;

            --  The referenced entity must either be the enclosing entity,
            --  or an entity declared within the current open scope.

            elsif Present (Scope (Subp))
              and then Scope (Subp) /= Current_Scope
              and then Subp /= Current_Scope
            then
               Error_Pragma_Arg
                 ("argument of% must be entity in current scope", Assoc);
               return;
            end if;

            --  Processing for procedure, operator or function.
            --  If subprogram is aliased (as for an instance) indicate
            --  that the renamed entity is inlined.

            if Is_Subprogram (Subp) then
               while Present (Alias (Inner_Subp)) loop
                  Inner_Subp := Alias (Inner_Subp);
               end loop;

               Set_Inline_Flags (Inner_Subp);

               Decl := Parent (Parent (Inner_Subp));

               if Nkind (Decl) = N_Subprogram_Declaration
                 and then Present (Corresponding_Body (Decl))
               then
                  Set_Inline_Flags (Corresponding_Body (Decl));
               end if;

               Applies := True;

            --  For a generic subprogram set flag as well, for use at
            --  the point of instantiation, to determine whether the
            --  body should be generated.

            elsif Is_Generic_Subprogram (Subp) then
               Set_Inline_Flags (Subp);
               Applies := True;

            --  Literals are by definition inlined

            elsif Kind = E_Enumeration_Literal then
               null;

            --  Anything else is an error

            else
               Error_Pragma_Arg
                 ("expect subprogram name for pragma%", Assoc);
            end if;
         end Make_Inline;

         ----------------------
         -- Set_Inline_Flags --
         ----------------------

         procedure Set_Inline_Flags (Subp : Entity_Id) is
         begin
            if Active then
               Set_Is_Inlined (Subp, True);
            end if;

            if not Has_Pragma_Inline (Subp) then
               Set_Has_Pragma_Inline (Subp);
               Set_Next_Rep_Item (N, First_Rep_Item (Subp));
               Set_First_Rep_Item (Subp, N);
            end if;
         end Set_Inline_Flags;

      --  Start of processing for Process_Inline

      begin
         Check_No_Identifiers;
         Check_At_Least_N_Arguments (1);

         if Active then
            Inline_Processing_Required := True;
         end if;

         Assoc := Arg1;
         while Present (Assoc) loop
            Subp_Id := Expression (Assoc);
            Analyze (Subp_Id);
            Applies := False;

            if Is_Entity_Name (Subp_Id) then
               Subp := Entity (Subp_Id);

               if Subp = Any_Id then
                  Applies := True;

               else
                  Make_Inline (Subp);

                  while Present (Homonym (Subp))
                    and then Scope (Homonym (Subp)) = Current_Scope
                  loop
                     Make_Inline (Homonym (Subp));
                     Subp := Homonym (Subp);
                  end loop;
               end if;
            end if;

            if not Applies then
               Error_Pragma_Arg
                 ("inappropriate argument for pragma%", Assoc);
            end if;

            Next (Assoc);
         end loop;
      end Process_Inline;

      ----------------------------
      -- Process_Interface_Name --
      ----------------------------

      procedure Process_Interface_Name
        (Subprogram_Def : Entity_Id;
         Ext_Arg        : Node_Id;
         Link_Arg       : Node_Id)
      is
         Ext_Nam    : Node_Id;
         Link_Nam   : Node_Id;
         String_Val : String_Id;

         procedure Check_Form_Of_Interface_Name (SN : Node_Id);
         --  SN is a string literal node for an interface name. This routine
         --  performs some minimal checks that the name is reasonable. In
         --  particular that no spaces or other obviously incorrect characters
         --  appear. This is only a warning, since any characters are allowed.

         procedure Check_Form_Of_Interface_Name (SN : Node_Id) is
            S  : constant String_Id := Strval (Expr_Value_S (SN));
            SL : constant Nat       := String_Length (S);
            C  : Char_Code;

         begin
            if SL = 0 then
               Error_Msg_N ("interface name cannot be null string", SN);
            end if;

            for J in 1 .. SL loop
               C := Get_String_Char (S, J);

               if Warn_On_Export_Import
                 and then (not In_Character_Range (C)
                             or else Get_Character (C) = ' '
                             or else Get_Character (C) = ',')
               then
                  Error_Msg_N
                    ("?interface name contains illegal character", SN);
               end if;
            end loop;
         end Check_Form_Of_Interface_Name;

      --  Start of processing for Process_Interface_Name

      begin
         if No (Link_Arg) then
            if No (Ext_Arg) then
               return;

            elsif Chars (Ext_Arg) = Name_Link_Name then
               Ext_Nam  := Empty;
               Link_Nam := Expression (Ext_Arg);

            else
               Check_Optional_Identifier (Ext_Arg, Name_External_Name);
               Ext_Nam  := Expression (Ext_Arg);
               Link_Nam := Empty;
            end if;

         else
            Check_Optional_Identifier (Ext_Arg,  Name_External_Name);
            Check_Optional_Identifier (Link_Arg, Name_Link_Name);
            Ext_Nam  := Expression (Ext_Arg);
            Link_Nam := Expression (Link_Arg);
         end if;

         --  Check expressions for external name and link name are static

         if Present (Ext_Nam) then
            Check_Arg_Is_Static_Expression (Ext_Nam, Standard_String);
            Check_Form_Of_Interface_Name (Ext_Nam);

            --  Verify that the external name is not the name of a local
            --  entity, which would hide the imported one and lead to
            --  run-time surprises. The problem can only arise for entities
            --  declared in a package body (otherwise the external name is
            --  fully qualified and won't conflict).

            declare
               Nam : Name_Id;
               E   : Entity_Id;
               Par : Node_Id;

            begin
               if Prag_Id = Pragma_Import then
                  String_To_Name_Buffer (Strval (Expr_Value_S (Ext_Nam)));
                  Nam := Name_Find;
                  E   := Entity_Id (Get_Name_Table_Info (Nam));

                  if Nam /= Chars (Subprogram_Def)
                    and then Present (E)
                    and then not Is_Overloadable (E)
                    and then Is_Immediately_Visible (E)
                    and then not Is_Imported (E)
                    and then Ekind (Scope (E)) = E_Package
                  then
                     Par := Parent (E);

                     while Present (Par) loop
                        if Nkind (Par) = N_Package_Body then
                           Error_Msg_Sloc  := Sloc (E);
                           Error_Msg_NE
                             ("imported entity is hidden by & declared#",
                                 Ext_Arg, E);
                           exit;
                        end if;

                        Par := Parent (Par);
                     end loop;
                  end if;
               end if;
            end;
         end if;

         if Present (Link_Nam) then
            Check_Arg_Is_Static_Expression (Link_Nam, Standard_String);
            Check_Form_Of_Interface_Name (Link_Nam);
         end if;

         --  If there is no link name, just set the external name

         if No (Link_Nam) then
            Set_Encoded_Interface_Name
              (Get_Base_Subprogram (Subprogram_Def),
               Adjust_External_Name_Case (Expr_Value_S (Ext_Nam)));

         --  For the Link_Name case, the given literal is preceded by an
         --  asterisk, which indicates to GCC that the given name should
         --  be taken literally, and in particular that no prepending of
         --  underlines should occur, even in systems where this is the
         --  normal default.

         else
            Start_String;
            Store_String_Char (Get_Char_Code ('*'));
            String_Val := Strval (Expr_Value_S (Link_Nam));

            for J in 1 .. String_Length (String_Val) loop
               Store_String_Char (Get_String_Char (String_Val, J));
            end loop;

            Link_Nam :=
              Make_String_Literal (Sloc (Link_Nam), End_String);

            Set_Encoded_Interface_Name
              (Get_Base_Subprogram (Subprogram_Def), Link_Nam);
         end if;
      end Process_Interface_Name;

      ---------------------------------
      -- Process_Suppress_Unsuppress --
      ---------------------------------

      --  Note: this procedure makes entries in the check suppress data
      --  structures managed by Sem. See spec of package Sem for full
      --  details on how we handle recording of check suppression.

      procedure Process_Suppress_Unsuppress (Suppress_Case : Boolean) is
         C    : Check_Id;
         E_Id : Node_Id;
         E    : Entity_Id;

         In_Package_Spec : constant Boolean :=
                             (Ekind (Current_Scope) = E_Package
                                or else
                              Ekind (Current_Scope) = E_Generic_Package)
                               and then not In_Package_Body (Current_Scope);

         procedure Suppress_Unsuppress_Echeck (E : Entity_Id; C : Check_Id);
         --  Used to suppress a single check on the given entity

         --------------------------------
         -- Suppress_Unsuppress_Echeck --
         --------------------------------

         procedure Suppress_Unsuppress_Echeck (E : Entity_Id; C : Check_Id) is
            ESR : constant Entity_Check_Suppress_Record :=
                    (Entity   => E,
                     Check    => C,
                     Suppress => Suppress_Case);

         begin
            Set_Checks_May_Be_Suppressed (E);

            if In_Package_Spec then
               Global_Entity_Suppress.Append (ESR);
            else
               Local_Entity_Suppress.Append (ESR);
            end if;

            --  If this is a first subtype, and the base type is distinct,
            --  then also set the suppress flags on the base type.

            if Is_First_Subtype (E)
              and then Etype (E) /= E
            then
               Suppress_Unsuppress_Echeck (Etype (E), C);
            end if;
         end Suppress_Unsuppress_Echeck;

      --  Start of processing for Process_Suppress_Unsuppress

      begin
         --  Suppress/Unsuppress can appear as a configuration pragma,
         --  or in a declarative part or a package spec (RM 11.5(5))

         if not Is_Configuration_Pragma then
            Check_Is_In_Decl_Part_Or_Package_Spec;
         end if;

         Check_At_Least_N_Arguments (1);
         Check_At_Most_N_Arguments (2);
         Check_No_Identifier (Arg1);
         Check_Arg_Is_Identifier (Arg1);

         if not Is_Check_Name (Chars (Expression (Arg1))) then
            Error_Pragma_Arg
              ("argument of pragma% is not valid check name", Arg1);

         else
            C := Get_Check_Id (Chars (Expression (Arg1)));
         end if;

         if Arg_Count = 1 then

            --  Make an entry in the local scope suppress table. This is the
            --  table that directly shows the current value of the scope
            --  suppress check for any check id value.

            if C = All_Checks then
               for J in Scope_Suppress'Range loop
                  Scope_Suppress (J) := Suppress_Case;
               end loop;
            else
               Scope_Suppress (C) := Suppress_Case;
            end if;

            --  Also make an entry in the Local_Entity_Suppress table. See
            --  extended description in the package spec of Sem for details.

            Local_Entity_Suppress.Append
              ((Entity   => Empty,
                Check    => C,
                Suppress => Suppress_Case));

         --  Case of two arguments present, where the check is
         --  suppressed for a specified entity (given as the second
         --  argument of the pragma)

         else
            Check_Optional_Identifier (Arg2, Name_On);
            E_Id := Expression (Arg2);
            Analyze (E_Id);

            if not Is_Entity_Name (E_Id) then
               Error_Pragma_Arg
                 ("second argument of pragma% must be entity name", Arg2);
            end if;

            E := Entity (E_Id);

            if E = Any_Id then
               return;
            end if;

            --  Enforce RM 11.5(7) which requires that for a pragma that
            --  appears within a package spec, the named entity must be
            --  within the package spec. We allow the package name itself
            --  to be mentioned since that makes sense, although it is not
            --  strictly allowed by 11.5(7).

            if In_Package_Spec
              and then E /= Current_Scope
              and then Scope (E) /= Current_Scope
            then
               Error_Pragma_Arg
                 ("entity in pragma% is not in package spec ('R'M 11.5(7))",
                  Arg2);
            end if;

            --  Loop through homonyms. As noted below, in the case of a package
            --  spec, only homonyms within the package spec are considered.

            loop
               Suppress_Unsuppress_Echeck (E, C);

               if Is_Generic_Instance (E)
                 and then Is_Subprogram (E)
                 and then Present (Alias (E))
               then
                  Suppress_Unsuppress_Echeck (Alias (E), C);
               end if;

               --  Move to next homonym

               E := Homonym (E);
               exit when No (E);

               --  If we are within a package specification, the
               --  pragma only applies to homonyms in the same scope.

               exit when In_Package_Spec
                 and then Scope (E) /= Current_Scope;
            end loop;
         end if;
      end Process_Suppress_Unsuppress;

      ------------------
      -- Set_Exported --
      ------------------

      procedure Set_Exported (E : Entity_Id; Arg : Node_Id) is
      begin
         if Is_Imported (E) then
            Error_Pragma_Arg
              ("cannot export entity& that was previously imported", Arg);

         elsif Present (Address_Clause (E)) then
            Error_Pragma_Arg
              ("cannot export entity& that has an address clause", Arg);
         end if;

         Set_Is_Exported (E);

         --  Generate a reference for entity explicitly, because the
         --  identifier may be overloaded and name resolution will not
         --  generate one.

         Generate_Reference (E, Arg);

         --  Deal with exporting non-library level entity

         if not Is_Library_Level_Entity (E) then

            --  Not allowed at all for subprograms

            if Is_Subprogram (E) then
               Error_Pragma_Arg ("local subprogram& cannot be exported", Arg);

            --  Otherwise set public and statically allocated

            else
               Set_Is_Public (E);
               Set_Is_Statically_Allocated (E);

               if Warn_On_Export_Import then
                  Error_Msg_NE
                    ("?& has been made static as a result of Export", Arg, E);
                  Error_Msg_N
                    ("\this usage is non-standard and non-portable", Arg);
               end if;
            end if;
         end if;

         if Warn_On_Export_Import and then Is_Type (E) then
            Error_Msg_NE
              ("exporting a type has no effect?", Arg, E);
         end if;

         if Warn_On_Export_Import and Inside_A_Generic then
            Error_Msg_NE
              ("all instances of& will have the same external name?", Arg, E);
         end if;
      end Set_Exported;

      ----------------------------------------------
      -- Set_Extended_Import_Export_External_Name --
      ----------------------------------------------

      procedure Set_Extended_Import_Export_External_Name
        (Internal_Ent : Entity_Id;
         Arg_External : Node_Id)
      is
         Old_Name : constant Node_Id := Interface_Name (Internal_Ent);
         New_Name : Node_Id;

      begin
         if No (Arg_External) then
            return;

         elsif Nkind (Arg_External) = N_String_Literal then
            if String_Length (Strval (Arg_External)) = 0 then
               return;
            else
               New_Name := Adjust_External_Name_Case (Arg_External);
            end if;

         elsif Nkind (Arg_External) = N_Identifier then
            New_Name := Get_Default_External_Name (Arg_External);

         else
            Error_Pragma_Arg
              ("incorrect form for External parameter for pragma%",
               Arg_External);
         end if;

         --  If we already have an external name set (by a prior normal
         --  Import or Export pragma), then the external names must match

         if Present (Interface_Name (Internal_Ent)) then
            declare
               S1 : constant String_Id := Strval (Old_Name);
               S2 : constant String_Id := Strval (New_Name);

               procedure Mismatch;
               --  Called if names do not match

               procedure Mismatch is
               begin
                  Error_Msg_Sloc := Sloc (Old_Name);
                  Error_Pragma_Arg
                    ("external name does not match that given #",
                     Arg_External);
               end Mismatch;

            begin
               if String_Length (S1) /= String_Length (S2) then
                  Mismatch;

               else
                  for J in 1 .. String_Length (S1) loop
                     if Get_String_Char (S1, J) /= Get_String_Char (S2, J) then
                        Mismatch;
                     end if;
                  end loop;
               end if;
            end;

         --  Otherwise set the given name

         else
            Set_Encoded_Interface_Name (Internal_Ent, New_Name);
         end if;

      end Set_Extended_Import_Export_External_Name;

      ------------------
      -- Set_Imported --
      ------------------

      procedure Set_Imported (E : Entity_Id) is
      begin
         Error_Msg_Sloc  := Sloc (E);

         if Is_Exported (E) or else Is_Imported (E) then
            Error_Msg_NE ("import of& declared# not allowed", N, E);

            if Is_Exported (E) then
               Error_Msg_N ("\entity was previously exported", N);
            else
               Error_Msg_N ("\entity was previously imported", N);
            end if;

            Error_Pragma ("\(pragma% applies to all previous entities)");

         else
            Set_Is_Imported (E);

            --  If the entity is an object that is not at the library
            --  level, then it is statically allocated. We do not worry
            --  about objects with address clauses in this context since
            --  they are not really imported in the linker sense.

            if Is_Object (E)
              and then not Is_Library_Level_Entity (E)
              and then No (Address_Clause (E))
            then
               Set_Is_Statically_Allocated (E);
            end if;
         end if;
      end Set_Imported;

      -------------------------
      -- Set_Mechanism_Value --
      -------------------------

      --  Note: the mechanism name has not been analyzed (and cannot indeed
      --  be analyzed, since it is semantic nonsense), so we get it in the
      --  exact form created by the parser.

      procedure Set_Mechanism_Value (Ent : Entity_Id; Mech_Name : Node_Id) is
         Class : Node_Id;
         Param : Node_Id;

         procedure Bad_Class;
         --  Signal bad descriptor class name

         procedure Bad_Mechanism;
         --  Signal bad mechanism name

         procedure Bad_Class is
         begin
            Error_Pragma_Arg ("unrecognized descriptor class name", Class);
         end Bad_Class;

         procedure Bad_Mechanism is
         begin
            Error_Pragma_Arg ("unrecognized mechanism name", Mech_Name);
         end Bad_Mechanism;

      --  Start of processing for Set_Mechanism_Value

      begin
         if Mechanism (Ent) /= Default_Mechanism then
            Error_Msg_NE
              ("mechanism for & has already been set", Mech_Name, Ent);
         end if;

         --  MECHANISM_NAME ::= value | reference | descriptor

         if Nkind (Mech_Name) = N_Identifier then
            if Chars (Mech_Name) = Name_Value then
               Set_Mechanism (Ent, By_Copy);
               return;

            elsif Chars (Mech_Name) = Name_Reference then
               Set_Mechanism (Ent, By_Reference);
               return;

            elsif Chars (Mech_Name) = Name_Descriptor then
               Check_VMS (Mech_Name);
               Set_Mechanism (Ent, By_Descriptor);
               return;

            elsif Chars (Mech_Name) = Name_Copy then
               Error_Pragma_Arg
                 ("bad mechanism name, Value assumed", Mech_Name);

            else
               Bad_Mechanism;
            end if;

         --  MECHANISM_NAME ::= descriptor (CLASS_NAME)
         --  CLASS_NAME     ::= ubs | ubsb | uba | s | sb | a | nca

         --  Note: this form is parsed as an indexed component

         elsif Nkind (Mech_Name) = N_Indexed_Component then
            Class := First (Expressions (Mech_Name));

            if Nkind (Prefix (Mech_Name)) /= N_Identifier
              or else Chars (Prefix (Mech_Name)) /= Name_Descriptor
              or else Present (Next (Class))
            then
               Bad_Mechanism;
            end if;

         --  MECHANISM_NAME ::= descriptor (Class => CLASS_NAME)
         --  CLASS_NAME     ::= ubs | ubsb | uba | s | sb | a | nca

         --  Note: this form is parsed as a function call

         elsif Nkind (Mech_Name) = N_Function_Call then

            Param := First (Parameter_Associations (Mech_Name));

            if Nkind (Name (Mech_Name)) /= N_Identifier
              or else Chars (Name (Mech_Name)) /= Name_Descriptor
              or else Present (Next (Param))
              or else No (Selector_Name (Param))
              or else Chars (Selector_Name (Param)) /= Name_Class
            then
               Bad_Mechanism;
            else
               Class := Explicit_Actual_Parameter (Param);
            end if;

         else
            Bad_Mechanism;
         end if;

         --  Fall through here with Class set to descriptor class name

         Check_VMS (Mech_Name);

         if Nkind (Class) /= N_Identifier then
            Bad_Class;

         elsif Chars (Class) = Name_UBS then
            Set_Mechanism (Ent, By_Descriptor_UBS);

         elsif Chars (Class) = Name_UBSB then
            Set_Mechanism (Ent, By_Descriptor_UBSB);

         elsif Chars (Class) = Name_UBA then
            Set_Mechanism (Ent, By_Descriptor_UBA);

         elsif Chars (Class) = Name_S then
            Set_Mechanism (Ent, By_Descriptor_S);

         elsif Chars (Class) = Name_SB then
            Set_Mechanism (Ent, By_Descriptor_SB);

         elsif Chars (Class) = Name_A then
            Set_Mechanism (Ent, By_Descriptor_A);

         elsif Chars (Class) = Name_NCA then
            Set_Mechanism (Ent, By_Descriptor_NCA);

         else
            Bad_Class;
         end if;

      end Set_Mechanism_Value;
      
      ----------------------------
      -- Process_Reflex_Pragmas --
      ----------------------------
      
      procedure Process_Reflex_Pragmas is
	 Arg     : Node_Id;
	 Expr    : Node_Id;
	 Nam_Arg : Node_Id;
	 E       : Entity_Id;
         Rx_Name : Name_Id;
         
	 procedure Analyze_Reflex_Pragmas;
	 
	 ----------------------------
	 -- Analyze_Reflex_Pragmas --
	 ----------------------------
	 
	 procedure Analyze_Reflex_Pragmas is
	 begin
	    --  Arg1 is Reflex
	    --  Arg2 is the Name of the Reflex Pragma
	    --  All others Pragma depend on the Name of the Reflex Pragma
	    --  and are expresson like string
	    
	    Expr := Get_Pragma_Arg (Arg2);
	    
	    pragma Assert (Nkind (Expr) = N_Identifier);
	    
	    Rx_Name := Chars (Expr);
	    
            --  Subprogram generation Type
	       
            if Rx_Name = Name_Generate then
               Check_Arg_Count (3);
		  
               Arg := Next (Arg2);
               if Present (Arg)
		 and then Present (Expression (Arg))
                 and then Nkind (Expression (Arg)) = N_String_Literal 
               then
                  String_To_Name_Buffer (Strval (Expression (Arg)));
                  declare
                     S : String := Name_Buffer (1..Name_Len);
                  begin
                     Trim (S, Both);
                     To_Lower (S);
                     --  if S = "section" then
                     --     Set_Generation_Type (E, Section_Type);
                     --  elsif S = "sr" then
                     --     Set_Generation_Type (E, Sr_Type);
                     --  elsif S = "dfb" then
                     --     Set_Generation_Type (E, Fb_Type);
                     --  else
                     --     Error_Pragma_Arg
                     --       ("not a valid generation name%", Expr);
                     --  end if;
                  end;
               else
                  Error_Pragma_Arg
                    ("missing generation name%", Expr);
               end if;

               --  Generatio nLangage 
		  
            --  elsif Rx_Name = Name_Plc_Lang then
            --     Check_Arg_Count (3);
		  
            --     Arg := Next (Arg2);
            --     if Present (Arg)
	    --  	 and then Present (Expression (Arg)) 
            --       and then Nkind (Expression (Arg)) = N_String_Literal 
            --     then
            --        String_To_Name_Buffer (Strval (Expression (Arg)));
            --        declare
            --           S : String := Name_Buffer (1..Name_Len);
            --        begin
            --           Trim (S, Both);
            --           To_Lower (S);
            --           if S = "literal" then
            --              Set_Language (E, Literal_Language);
            --           elsif S = "ladder" then
            --              Set_Language (E, Ladder_Language);
            --           elsif S = "fdb" then
            --              Set_Language(E, Flow_Language);
            --           elsif S = "chart" then
            --              Set_Language (E, Chart_Language);
            --           else
            --              Error_Pragma_Arg
            --                ("not a valid language name%", Expr);
            --           end if;
            --        end;
		  
--                 else
--                    Error_Pragma_Arg
--                      ("missing language name%", Expr);
--                 end if;
		  
               --  Comment pragma
		  
            elsif Rx_Name = Name_Plc_Comment then
               Check_Arg_Count (3);
		  
               Arg := Next (Arg2);
               if Present (Arg)
		 and then Present (Expression (Arg))
                 and then Nkind (Expression (Arg)) = N_String_Literal 
               then
                  String_To_Name_Buffer (Strval (Expression (Arg)));
                  declare
                     S : String := Name_Buffer (1..Name_Len);
                  begin
		     null; --Set_Entity_Comment 
		       ---(E, Artics.Strings_Stocks.Enter_String (S));
                  end;
               end if;
		  
            else
               Error_Pragma_Arg ("not a valid Reflex pragma name%", Expr);
            end if;
	    
         end Analyze_Reflex_Pragmas;
	 
         --  Start of Process_Reflex_Pragmas
      begin
         Check_At_Least_N_Arguments (1);
	 
         --  Arg1 is the reflex pragma
	 
         --  Arg2 is either an expression or a name
	 
         --  All remaining args are either an expression or a pragma argument
         --  association
         
	 Nam_Arg := Last (Pragma_Argument_Associations (N));
	 
	 --  Determine whether the last argument is "Entity => local_NAME"
	 --  and if it is, perform the required semantic checks. Remove the
	 --  argument from further processing.
	 
	 if Nkind (Nam_Arg) = N_Pragma_Argument_Association
	   and then Chars (Nam_Arg) = Name_Entity
	 then
	    Check_Arg_Is_Local_Name (Nam_Arg);
	    if Present (Entity (Expression (Nam_Arg))) then
	       E := Entity (Expression (Nam_Arg));
	    else
	       return;
	    end if;
	    Arg_Count := Arg_Count - 1;
	 end if;
	 
	 --  The second parameter is optional, it is never analyzed
	 
	 if No (Arg2) then
               null;

	       --  Otherwise there is a second parameter

	 else
	    --  The second parameter must be an identifier
	    
	    Check_Arg_Is_Identifier (Arg2);
	    
	    --  Process the remaining parameters (if any)
	    
	    Arg := Next (Arg2);
	    while Present (Arg) loop
	       Expr := Get_Pragma_Arg (Arg);
	       Analyze (Expr);
	       
	       if Is_Entity_Name (Expr) then
		  null;
		  
                  --  For string literals, we assume Standard_String as the
                  --  type, unless the string contains wide or wide_wide
                  --  characters.
		  
	       elsif Nkind (Expr) = N_String_Literal then
		  Resolve (Expr, Standard_String);
		  
	       elsif Is_Overloaded (Expr) then
		  Error_Pragma_Arg ("ambiguous argument for pragma%", Expr);
		  
	       else
		  Resolve (Expr);
	       end if;
	       
	       Next (Arg);
	    end loop;
	 end if;
	 
	 Analyze_Reflex_Pragmas;
	 
      end Process_Reflex_Pragmas;
      
   --  Start of processing for Analyze_Pragma

   begin
      if not Is_Pragma_Name (Chars (N)) then
         if Warn_On_Unrecognized_Pragma then
            Error_Pragma ("unrecognized pragma%!?");
         else
            raise Pragma_Exit;
         end if;
      else
         Prag_Id := Get_Pragma_Id (Chars (N));
      end if;

      --  Preset arguments

      Arg1 := Empty;
      Arg2 := Empty;
      Arg3 := Empty;
      Arg4 := Empty;

      if Present (Pragma_Argument_Associations (N)) then
         Arg1 := First (Pragma_Argument_Associations (N));

         if Present (Arg1) then
            Arg2 := Next (Arg1);

            if Present (Arg2) then
               Arg3 := Next (Arg2);

               if Present (Arg3) then
                  Arg4 := Next (Arg3);
               end if;
            end if;
         end if;
      end if;

      --  Count number of arguments

      declare
         Arg_Node : Node_Id;
      begin
         Arg_Count := 0;
         Arg_Node := Arg1;
         while Present (Arg_Node) loop
            Arg_Count := Arg_Count + 1;
            Next (Arg_Node);
         end loop;
      end;

      --  An enumeration type defines the pragmas that are supported by the
      --  implementation. Get_Pragma_Id (in package Prag) transorms a name
      --  into the corresponding enumeration value for the following case.

      case Prag_Id is

         ----------------------
         -- All_Calls_ --
         ----------------------

	 --------------
         -- Annotate --
         --------------

         --  pragma Annotate (IDENTIFIER {, ARG});
         --  ARG ::= NAME | EXPRESSION

         when Pragma_Annotate => Annotate : begin
            GNAT_Pragma;
            Check_At_Least_N_Arguments (1);
            Check_Arg_Is_Identifier (Arg1);
	    Arg_Count := Arg_Count - 1;
	    Check_No_Identifiers;
	    Arg_Count := Arg_Count + 1;
	    
	    if Present (Arg1) 
	      and then Nkind (Get_Pragma_Arg (Arg1)) = N_Identifier
	      and then Chars (Get_Pragma_Arg (Arg1)) = Name_Reflex then
	       Process_Reflex_Pragmas;
	    else
	       declare
		  Arg : Node_Id := Arg2;
		  Exp : Node_Id;
		  
	       begin
		  --  The second parameter must be an identifier

		  Check_Arg_Is_Identifier (Arg2);
		  
		  --  Process the remaining parameters (if any)
		  
		  Arg := Next (Arg2);
		  while Present (Arg) loop
		     Exp := Expression (Arg);
		     Analyze (Exp);
		     
		     if Is_Entity_Name (Exp) then
			null;
			
		     elsif Nkind (Exp) = N_String_Literal then
			Resolve (Exp, Standard_String);
			
		     elsif Is_Overloaded (Exp) then
			Error_Pragma_Arg
			  ("ambiguous argument for pragma%", Exp);
			
		     else
			Resolve (Exp);
		     end if;
		     
                  Next (Arg);
               end loop;
	       end;
	    end if;
         end Annotate;

         ------------
         -- Assert --
         ------------

         --  pragma Assert (Boolean_EXPRESSION [, static_string_EXPRESSION]);

         when Pragma_Assert =>
            GNAT_Pragma;
            Check_No_Identifiers;

            if Arg_Count > 1 then
               Check_Arg_Count (2);
               Check_Arg_Is_Static_Expression (Arg2, Standard_String);
            end if;

            --  If expansion is active and assertions are inactive, then
            --  we rewrite the Assertion as:

            --    if False and then condition then
            --       null;
            --    end if;

            --  The reason we do this rewriting during semantic analysis
            --  rather than as part of normal expansion is that we cannot
            --  analyze and expand the code for the boolean expression
            --  directly, or it may cause insertion of actions that would
            --  escape the attempt to suppress the assertion code.

            if Expander_Active and not Assertions_Enabled then
               Rewrite (N,
                 Make_If_Statement (Loc,
                   Condition =>
                     Make_And_Then (Loc,
                       Left_Opnd  => New_Occurrence_Of (Standard_False, Loc),
                       Right_Opnd => Get_Pragma_Arg (Arg1)),
                   Then_Statements => New_List (
                     Make_Null_Statement (Loc))));

               Analyze (N);

            --  Otherwise (if assertions are enabled, or if we are not
            --  operating with expansion active), then we just analyze
            --  and resolve the expression.

            else
               Analyze_And_Resolve (Expression (Arg1), Any_Boolean);
            end if;

         --------------------
         -- C_Pass_By_Copy --
         --------------------

         --  pragma C_Pass_By_Copy ([Max_Size =>] static_integer_EXPRESSION);

         when Pragma_C_Pass_By_Copy => C_Pass_By_Copy : declare
            Arg : Node_Id;
            Val : Uint;

         begin
            GNAT_Pragma;
            Check_Valid_Configuration_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, "max_size");

            Arg := Expression (Arg1);
            Check_Arg_Is_Static_Expression (Arg, Any_Integer);

            Val := Expr_Value (Arg);

            if Val <= 0 then
               Error_Pragma_Arg
                 ("maximum size for pragma% must be positive", Arg1);

            elsif UI_Is_In_Int_Range (Val) then
               Default_C_Record_Mechanism := UI_To_Int (Val);

            --  If a giant value is given, Int'Last will do well enough.
            --  If sometime someone complains that a record larger than
            --  two gigabytes is not copied, we will worry about it then!

            else
               Default_C_Record_Mechanism := Mechanism_Type'Last;
            end if;
         end C_Pass_By_Copy;

         -------------
         -- Comment --
         -------------

         --  pragma Comment (static_string_EXPRESSION)

         --  Processing for pragma Comment shares the circuitry for
         --  pragma Ident. The only differences are that Ident enforces
         --  a limit of 31 characters on its argument, and also enforces
         --  limitations on placement for DEC compatibility. Pragma
         --  Comment shares neither of these restrictions.

         -------------------
         -- Common_Object --
         -------------------

         --  pragma Common_Object (
         --        [Internal =>] LOCAL_NAME,
         --     [, [External =>] EXTERNAL_SYMBOL]
         --     [, [Size     =>] EXTERNAL_SYMBOL]);

         --  Processing for this pragma is shared with Psect_Object

         --------------------------
         -- Compile_Time_Warning --
         --------------------------

         --  pragma Compile_Time_Warning
         --    (boolean_EXPRESSION, static_string_EXPRESSION);

         when Pragma_Compile_Time_Warning => Compile_Time_Warning : declare
            Arg1x : constant Node_Id := Get_Pragma_Arg (Arg1);

         begin
            GNAT_Pragma;
            Check_Arg_Count (2);
            Check_No_Identifiers;
            Check_Arg_Is_Static_Expression (Arg2, Standard_String);
            Analyze_And_Resolve (Arg1x, Standard_Boolean);

            if Compile_Time_Known_Value (Arg1x) then
               if Is_True (Expr_Value (Get_Pragma_Arg (Arg1))) then
                  String_To_Name_Buffer (Strval (Get_Pragma_Arg (Arg2)));
                  Add_Char_To_Name_Buffer ('?');

                  declare
                     Msg : String (1 .. Name_Len) :=
                             Name_Buffer (1 .. Name_Len);

                     B : Natural;

                  begin
                     --  This loop looks for multiple lines separated by
                     --  ASCII.LF and breaks them into continuation error
                     --  messages marked with the usual back slash.

                     B := 1;
                     for S in 2 .. Msg'Length - 1 loop
                        if Msg (S) = ASCII.LF then
                           Msg (S) := '?';
                           Error_Msg_N (Msg (B .. S), Arg1);
                           B := S;
                           Msg (B) := '\';
                        end if;
                     end loop;

                     Error_Msg_N (Msg (B .. Msg'Length), Arg1);
                  end;
               end if;
            end if;
         end Compile_Time_Warning;

         ----------------------------
         -------------------------
         -- Component_Alignment --
         -------------------------

         --  pragma Component_Alignment (
         --        [Form =>] ALIGNMENT_CHOICE
         --     [, [Name =>] type_LOCAL_NAME]);
         --
         --   ALIGNMENT_CHOICE ::=
         --     Component_Size
         --   | Component_Size_4
         --   | Storage_Unit
         --   | Default

         when Pragma_Component_Alignment => Component_AlignmentP : declare
            Args  : Args_List (1 .. 2);
            Names : constant Name_List (1 .. 2) := (
                      Name_Form,
                      Name_Name);

            Form  : Node_Id renames Args (1);
            Name  : Node_Id renames Args (2);

            Atype : Component_Alignment_Kind;
            Typ   : Entity_Id;

         begin
            GNAT_Pragma;
            Gather_Associations (Names, Args);

            if No (Form) then
               Error_Pragma ("missing Form argument for pragma%");
            end if;

            Check_Arg_Is_Identifier (Form);

            --  Get proper alignment, note that Default = Component_Size
            --  on all machines we have so far, and we want to set this
            --  value rather than the default value to indicate that it
            --  has been explicitly set (and thus will not get overridden
            --  by the default component alignment for the current scope)

            if Chars (Form) = Name_Component_Size then
               Atype := Calign_Component_Size;

            elsif Chars (Form) = Name_Component_Size_4 then
               Atype := Calign_Component_Size_4;

            elsif Chars (Form) = Name_Default then
               Atype := Calign_Component_Size;

            elsif Chars (Form) = Name_Storage_Unit then
               Atype := Calign_Storage_Unit;

            else
               Error_Pragma_Arg
                 ("invalid Form parameter for pragma%", Form);
            end if;

            --  Case with no name, supplied, affects scope table entry

            if No (Name) then
               Scope_Stack.Table
                 (Scope_Stack.Last).Component_Alignment_Default := Atype;

            --  Case of name supplied

            else
               Check_Arg_Is_Local_Name (Name);
               Find_Type (Name);
               Typ := Entity (Name);

               if Typ = Any_Type
                 or else Rep_Item_Too_Early (Typ, N)
               then
                  return;
               else
                  Typ := Underlying_Type (Typ);
               end if;

               if not Is_Record_Type (Typ)
                 and then not Is_Array_Type (Typ)
               then
                  Error_Pragma_Arg
                    ("Name parameter of pragma% must identify record or " &
                     "array type", Name);
               end if;

               --  An explicit Component_Alignment pragma overrides an
               --  implicit pragma Pack, but not an explicit one.

               if not Has_Pragma_Pack (Base_Type (Typ)) then
                  Set_Is_Packed (Base_Type (Typ), False);
                  Set_Component_Alignment (Base_Type (Typ), Atype);
               end if;
            end if;
         end Component_AlignmentP;

         ----------------
         -- Convention --
         ----------------

         --  pragma Convention ([Convention =>] convention_IDENTIFIER,
         --    [Entity =>] LOCAL_NAME);

         when Pragma_Convention => Convention : declare
            C : Convention_Id;
            E : Entity_Id;
         begin
            Check_Arg_Count (2);
            Process_Convention (C, E);
         end Convention;

         ---------------------------
         -- Convention_Identifier --
         ---------------------------

         --  pragma Convention_Identifier ([Name =>] IDENTIFIER,
         --    [Convention =>] convention_IDENTIFIER);

         when Pragma_Convention_Identifier => Convention_Identifier : declare
            Idnam : Name_Id;
            Cname : Name_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (2);
            Check_Optional_Identifier (Arg1, Name_Name);
            Check_Optional_Identifier (Arg2, Name_Convention);
            Check_Arg_Is_Identifier (Arg1);
            Check_Arg_Is_Identifier (Arg1);
            Idnam := Chars (Expression (Arg1));
            Cname := Chars (Expression (Arg2));

            if Is_Convention_Name (Cname) then
               Record_Convention_Identifier
                 (Idnam, Get_Convention_Id (Cname));
            else
               Error_Pragma_Arg
                 ("second arg for % pragma must be convention", Arg2);
            end if;
         end Convention_Identifier;

         -----------
         -- Debug --
         -----------

         --  pragma Debug (PROCEDURE_CALL_STATEMENT);

         when Pragma_Debug => null;

         -------------------
         -- Discard_Names --
         -------------------

         --  pragma Discard_Names [([On =>] LOCAL_NAME)];

         when Pragma_Discard_Names => Discard_Names : declare
            E_Id : Entity_Id;
            E    : Entity_Id;

         begin
            --  Deal with configuration pragma case

            if Arg_Count = 0 and then Is_Configuration_Pragma then
               Global_Discard_Names := True;
               return;

            --  Otherwise, check correct appropriate context

            else
               Check_Is_In_Decl_Part_Or_Package_Spec;

               if Arg_Count = 0 then

                  --  If there is no parameter, then from now on this pragma
                  --  applies to any enumeration, exception or tagged type
                  --  defined in the current declarative part.

                  Set_Discard_Names (Current_Scope);
                  return;

               else
                  Check_Arg_Count (1);
                  Check_Optional_Identifier (Arg1, Name_On);
                  Check_Arg_Is_Local_Name (Arg1);
                  E_Id := Expression (Arg1);

                  if Etype (E_Id) = Any_Type then
                     return;
                  else
                     E := Entity (E_Id);
                  end if;

                  if (Is_First_Subtype (E)
                       and then (Is_Enumeration_Type (E)
                                  or else Is_Tagged_Type (E)))
                  then
                     Set_Discard_Names (E);
                  else
                     Error_Pragma_Arg
                       ("inappropriate entity for pragma%", Arg1);
                  end if;
               end if;
            end if;
         end Discard_Names;

         ---------------
         -- Elaborate --
         ---------------

         --  pragma Elaborate (library_unit_NAME {, library_unit_NAME});

         when Pragma_Elaborate => Elaborate : declare
            Plist       : List_Id;
            Parent_Node : Node_Id;
            Arg         : Node_Id;
            Citem       : Node_Id;

         begin
            --  Pragma must be in context items list of a compilation unit

            if not Is_List_Member (N) then
               Pragma_Misplaced;
               return;

            else
               Plist := List_Containing (N);
               Parent_Node := Parent (Plist);

               if Parent_Node = Empty
                 or else Nkind (Parent_Node) /= N_Compilation_Unit
                 or else Context_Items (Parent_Node) /= Plist
               then
                  Pragma_Misplaced;
                  return;
               end if;
            end if;

            --  Must be at least one argument

            if Arg_Count = 0 then
               Error_Pragma ("pragma% requires at least one argument");
            end if;

            --  In Ada 83 mode, there can be no items following it in the
            --  context list except other pragmas and implicit with clauses
            --  (e.g. those added by use of Rtsfind). In Ada 95 mode, this
            --  placement rule does not apply.

            if Ada_83 and then Comes_From_Source (N) then
               Citem := Next (N);

               while Present (Citem) loop
                  if Nkind (Citem) = N_Pragma
                    or else (Nkind (Citem) = N_With_Clause
                              and then Implicit_With (Citem))
                  then
                     null;
                  else
                     Error_Pragma
                       ("(Ada 83) pragma% must be at end of context clause");
                  end if;

                  Next (Citem);
               end loop;
            end if;

            --  Finally, the arguments must all be units mentioned in a with
            --  clause in the same context clause. Note we already checked
            --  (in Par.Prag) that the arguments are either identifiers or

            Arg := Arg1;
            Outer : while Present (Arg) loop
               Citem := First (Plist);

               Inner : while Citem /= N loop
                  if Nkind (Citem) = N_With_Clause
                    and then Same_Name (Name (Citem), Expression (Arg))
                  then
                     Set_Elaborate_Present (Citem, True);
                     Set_Unit_Name (Expression (Arg), Name (Citem));
                     Set_Suppress_Elaboration_Warnings (Entity (Name (Citem)));
                     exit Inner;
                  end if;

                  Next (Citem);
               end loop Inner;

               if Citem = N then
                  Error_Pragma_Arg
                    ("argument of pragma% is not with'ed unit", Arg);
               end if;

               Next (Arg);
            end loop Outer;

            --  Give a warning if operating in static mode with -gnatwl
            --  (elaboration warnings eanbled) switch set.

            if Elab_Warnings and not Dynamic_Elaboration_Checks then
               Error_Msg_N
                 ("?use of pragma Elaborate may not be safe", N);
               Error_Msg_N
                 ("?use pragma Elaborate_All instead if possible", N);
            end if;
         end Elaborate;

         -------------------
         -- Elaborate_All --
         -------------------

         --  pragma Elaborate_All (library_unit_NAME {, library_unit_NAME});

         when Pragma_Elaborate_All => Elaborate_All : declare
            Plist       : List_Id;
            Parent_Node : Node_Id;
            Arg         : Node_Id;
            Citem       : Node_Id;

         begin
            --  Pragma must be in context items list of a compilation unit

            if not Is_List_Member (N) then
               Pragma_Misplaced;
               return;

            else
               Plist := List_Containing (N);
               Parent_Node := Parent (Plist);

               if Parent_Node = Empty
                 or else Nkind (Parent_Node) /= N_Compilation_Unit
                 or else Context_Items (Parent_Node) /= Plist
               then
                  Pragma_Misplaced;
                  return;
               end if;
            end if;

            --  Must be at least one argument

            if Arg_Count = 0 then
               Error_Pragma ("pragma% requires at least one argument");
            end if;

            --  Note: unlike pragma Elaborate, pragma Elaborate_All does not
            --  have to appear at the end of the context clause, but may
            --  appear mixed in with other items, even in Ada 83 mode.

            --  Final check: the arguments must all be units mentioned in
            --  a with clause in the same context clause. Note that we
            --  already checked (in Par.Prag) that all the arguments are
            --  either identifiers or selected components.

            Arg := Arg1;
            Outr : while Present (Arg) loop
               Citem := First (Plist);

               Innr : while Citem /= N loop
                  if Nkind (Citem) = N_With_Clause
                    and then Same_Name (Name (Citem), Expression (Arg))
                  then
                     Set_Elaborate_All_Present (Citem, True);
                     Set_Unit_Name (Expression (Arg), Name (Citem));
                     Set_Suppress_Elaboration_Warnings (Entity (Name (Citem)));
                     exit Innr;
                  end if;

                  Next (Citem);
               end loop Innr;

               if Citem = N then
                  Set_Error_Posted (N);
                  Error_Pragma_Arg
                    ("argument of pragma% is not with'ed unit", Arg);
               end if;

               Next (Arg);
            end loop Outr;
         end Elaborate_All;

         --------------------
         -- Elaborate_Body --
         --------------------

         --  pragma Elaborate_Body [( library_unit_NAME )];

         when Pragma_Elaborate_Body => Elaborate_Body : declare
            Cunit_Node : Node_Id;
            Cunit_Ent  : Entity_Id;

         begin
            Check_Valid_Library_Unit_Pragma;

            if Nkind (N) = N_Null_Statement then
               return;
            end if;

            Cunit_Node := Cunit (Current_Sem_Unit);
            Cunit_Ent  := Cunit_Entity (Current_Sem_Unit);

            if Nkind (Unit (Cunit_Node)) = N_Package_Body
                 or else
               Nkind (Unit (Cunit_Node)) = N_Subprogram_Body
            then
               Error_Pragma ("pragma% must refer to a spec, not a body");
            else
               Set_Body_Required (Cunit_Node, True);
               Set_Has_Pragma_Elaborate_Body     (Cunit_Ent);

               --  If we are in dynamic elaboration mode, then we suppress
               --  elaboration warnings for the unit, since it is definitely
               --  fine NOT to do dynamic checks at the first level (and such
               --  checks will be suppressed because no elaboration boolean
               --  is created for Elaborate_Body packages).

               --  But in the static model of elaboration, Elaborate_Body is
               --  definitely NOT good enough to ensure elaboration safety on
               --  its own, since the body may WITH other units that are not
               --  safe from an elaboration point of view, so a client must
               --  still do an Elaborate_All on such units.

               --  Debug flag -gnatdD restores the old behavior of 3.13,
               --  where Elaborate_Body always suppressed elab warnings.

               if Dynamic_Elaboration_Checks or Debug_Flag_DD then
                  Set_Suppress_Elaboration_Warnings (Cunit_Ent);
               end if;
            end if;
         end Elaborate_Body;

         ------------------------
         -- Elaboration_Checks --
         ------------------------

         --  pragma Elaboration_Checks (Static | Dynamic);

         when Pragma_Elaboration_Checks =>
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Arg_Is_One_Of (Arg1, Name_Static, Name_Dynamic);
            Dynamic_Elaboration_Checks :=
              (Chars (Get_Pragma_Arg (Arg1)) = Name_Dynamic);

         ---------------
         -- Eliminate --
         ---------------

         --  pragma Eliminate (
         --      [Unit_Name       =>]  IDENTIFIER |
         --                            SELECTED_COMPONENT
         --    [,[Entity          =>]  IDENTIFIER |
         --                            SELECTED_COMPONENT |
         --                            STRING_LITERAL]
         --    [,[Parameter_Types =>]  PARAMETER_TYPES]
         --    [,[Result_Type     =>]  result_SUBTYPE_NAME]
         --    [,[Homonym_Number  =>]  INTEGER_LITERAL]);

         --  PARAMETER_TYPES ::= (SUBTYPE_NAME {, SUBTYPE_NAME})
         --  SUBTYPE_NAME    ::= STRING_LITERAL

         when Pragma_Eliminate => Eliminate : declare
            Args  : Args_List (1 .. 5);
            Names : constant Name_List (1 .. 5) := (
                      Name_Unit_Name,
                      Name_Entity,
                      Name_Parameter_Types,
                      Name_Result_Type,
                      Name_Homonym_Number);

            Unit_Name       : Node_Id renames Args (1);
            Entity          : Node_Id renames Args (2);
            Parameter_Types : Node_Id renames Args (3);
            Result_Type     : Node_Id renames Args (4);
            Homonym_Number  : Node_Id renames Args (5);

         begin
            GNAT_Pragma;
            Check_Valid_Configuration_Pragma;
            Gather_Associations (Names, Args);

            if No (Unit_Name) then
               Error_Pragma ("missing Unit_Name argument for pragma%");
            end if;

            if No (Entity)
              and then (Present (Parameter_Types)
                          or else
                        Present (Result_Type)
                          or else
                        Present (Homonym_Number))
            then
               Error_Pragma ("missing Entity argument for pragma%");
            end if;

            Process_Eliminate_Pragma
              (N,
               Unit_Name,
               Entity,
               Parameter_Types,
               Result_Type,
               Homonym_Number);
         end Eliminate;

         --------------------------
         --  Explicit_Overriding --
         --------------------------

         when Pragma_Explicit_Overriding =>
            Check_Valid_Configuration_Pragma;
            Check_Arg_Count (0);
            Explicit_Overriding := True;

         ------------
         -- Export --
         ------------

         --  pragma Export (
         --    [   Convention    =>] convention_IDENTIFIER,
         --    [   Entity        =>] local_NAME
         --    [, [External_Name =>] static_string_EXPRESSION ]
         --    [, [Link_Name     =>] static_string_EXPRESSION ]);

         when Pragma_Export => Export : declare
            C      : Convention_Id;
            Def_Id : Entity_Id;

         begin
            Check_At_Least_N_Arguments (2);
            Check_At_Most_N_Arguments  (4);
            Process_Convention (C, Def_Id);

            if Ekind (Def_Id) /= E_Constant then
               Note_Possible_Modification (Expression (Arg2));
            end if;

            Process_Interface_Name (Def_Id, Arg3, Arg4);
            Set_Exported (Def_Id, Arg2);
         end Export;

         ---------------------
         -- Export_Function --
         ---------------------

         --  pragma Export_Function (
         --        [Internal         =>] LOCAL_NAME,
         --     [, [External         =>] EXTERNAL_SYMBOL,]
         --     [, [Parameter_Types  =>] (PARAMETER_TYPES)]
         --     [, [Result_Type      =>] TYPE_DESIGNATOR]
         --     [, [Mechanism        =>] MECHANISM]
         --     [, [Result_Mechanism =>] MECHANISM_NAME]);

         --  EXTERNAL_SYMBOL ::=
         --    IDENTIFIER
         --  | static_string_EXPRESSION

         --  PARAMETER_TYPES ::=
         --    null
         --  | TYPE_DESIGNATOR @{, TYPE_DESIGNATOR@}

         --  TYPE_DESIGNATOR ::=
         --    subtype_NAME
         --  | subtype_Name ' Access

         --  MECHANISM ::=
         --    MECHANISM_NAME
         --  | (MECHANISM_ASSOCIATION @{, MECHANISM_ASSOCIATION@})

         --  MECHANISM_ASSOCIATION ::=
         --    [formal_parameter_NAME =>] MECHANISM_NAME

         --  MECHANISM_NAME ::=
         --    Value
         --  | Reference
         --  | Descriptor [([Class =>] CLASS_NAME)]

         --  CLASS_NAME ::= ubs | ubsb | uba | s | sb | a | nca

         when Pragma_Export_Function => Export_Function : declare
            Args  : Args_List (1 .. 6);
            Names : constant Name_List (1 .. 6) := (
                      Name_Internal,
                      Name_External,
                      Name_Parameter_Types,
                      Name_Result_Type,
                      Name_Mechanism,
                      Name_Result_Mechanism);

            Internal         : Node_Id renames Args (1);
            External         : Node_Id renames Args (2);
            Parameter_Types  : Node_Id renames Args (3);
            Result_Type      : Node_Id renames Args (4);
            Mechanism        : Node_Id renames Args (5);
            Result_Mechanism : Node_Id renames Args (6);

         begin
            GNAT_Pragma;
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Subprogram_Pragma (
              Arg_Internal         => Internal,
              Arg_External         => External,
              Arg_Parameter_Types  => Parameter_Types,
              Arg_Result_Type      => Result_Type,
              Arg_Mechanism        => Mechanism,
              Arg_Result_Mechanism => Result_Mechanism);
         end Export_Function;

         -------------------
         -- Export_Object --
         -------------------

         --  pragma Export_Object (
         --        [Internal =>] LOCAL_NAME,
         --     [, [External =>] EXTERNAL_SYMBOL]
         --     [, [Size     =>] EXTERNAL_SYMBOL]);

         --  EXTERNAL_SYMBOL ::=
         --    IDENTIFIER
         --  | static_string_EXPRESSION

         --  PARAMETER_TYPES ::=
         --    null
         --  | TYPE_DESIGNATOR @{, TYPE_DESIGNATOR@}

         --  TYPE_DESIGNATOR ::=
         --    subtype_NAME
         --  | subtype_Name ' Access

         --  MECHANISM ::=
         --    MECHANISM_NAME
         --  | (MECHANISM_ASSOCIATION @{, MECHANISM_ASSOCIATION@})

         --  MECHANISM_ASSOCIATION ::=
         --    [formal_parameter_NAME =>] MECHANISM_NAME

         --  MECHANISM_NAME ::=
         --    Value
         --  | Reference
         --  | Descriptor [([Class =>] CLASS_NAME)]

         --  CLASS_NAME ::= ubs | ubsb | uba | s | sb | a | nca

         when Pragma_Export_Object => Export_Object : declare
            Args  : Args_List (1 .. 3);
            Names : constant Name_List (1 .. 3) := (
                      Name_Internal,
                      Name_External,
                      Name_Size);

            Internal : Node_Id renames Args (1);
            External : Node_Id renames Args (2);
            Size     : Node_Id renames Args (3);

         begin
            GNAT_Pragma;
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Object_Pragma (
              Arg_Internal => Internal,
              Arg_External => External,
              Arg_Size     => Size);
         end Export_Object;

         ----------------------
         -- Export_Procedure --
         ----------------------

         --  pragma Export_Procedure (
         --        [Internal         =>] LOCAL_NAME,
         --     [, [External         =>] EXTERNAL_SYMBOL,]
         --     [, [Parameter_Types  =>] (PARAMETER_TYPES)]
         --     [, [Mechanism        =>] MECHANISM]);

         --  EXTERNAL_SYMBOL ::=
         --    IDENTIFIER
         --  | static_string_EXPRESSION

         --  PARAMETER_TYPES ::=
         --    null
         --  | TYPE_DESIGNATOR @{, TYPE_DESIGNATOR@}

         --  TYPE_DESIGNATOR ::=
         --    subtype_NAME
         --  | subtype_Name ' Access

         --  MECHANISM ::=
         --    MECHANISM_NAME
         --  | (MECHANISM_ASSOCIATION @{, MECHANISM_ASSOCIATION@})

         --  MECHANISM_ASSOCIATION ::=
         --    [formal_parameter_NAME =>] MECHANISM_NAME

         --  MECHANISM_NAME ::=
         --    Value
         --  | Reference
         --  | Descriptor [([Class =>] CLASS_NAME)]

         --  CLASS_NAME ::= ubs | ubsb | uba | s | sb | a | nca

         when Pragma_Export_Procedure => Export_Procedure : declare
            Args  : Args_List (1 .. 4);
            Names : constant Name_List (1 .. 4) := (
                      Name_Internal,
                      Name_External,
                      Name_Parameter_Types,
                      Name_Mechanism);

            Internal        : Node_Id renames Args (1);
            External        : Node_Id renames Args (2);
            Parameter_Types : Node_Id renames Args (3);
            Mechanism       : Node_Id renames Args (4);

         begin
            GNAT_Pragma;
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Subprogram_Pragma (
              Arg_Internal        => Internal,
              Arg_External        => External,
              Arg_Parameter_Types => Parameter_Types,
              Arg_Mechanism       => Mechanism);
         end Export_Procedure;

         ------------------------
         -- Extensions_Allowed --
         ------------------------

         --  pragma Extensions_Allowed (ON | OFF);

         when Pragma_Extensions_Allowed =>
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Arg_Is_One_Of (Arg1, Name_On, Name_Off);
            Extensions_Allowed := (Chars (Expression (Arg1)) = Name_On);

         --------------
         -- External --
         --------------

         --  pragma External (
         --    [   Convention    =>] convention_IDENTIFIER,
         --    [   Entity        =>] local_NAME
         --    [, [External_Name =>] static_string_EXPRESSION ]
         --    [, [Link_Name     =>] static_string_EXPRESSION ]);

         when Pragma_External => External : declare
            C      : Convention_Id;
            Def_Id : Entity_Id;

         begin
            GNAT_Pragma;
            Check_At_Least_N_Arguments (2);
            Check_At_Most_N_Arguments  (4);
            Process_Convention (C, Def_Id);
            Note_Possible_Modification (Expression (Arg2));
            Process_Interface_Name (Def_Id, Arg3, Arg4);
            Set_Exported (Def_Id, Arg2);
         end External;

         --------------------------
         -- External_Name_Casing --
         --------------------------

         --  pragma External_Name_Casing (
         --    UPPERCASE | LOWERCASE
         --    [, AS_IS | UPPERCASE | LOWERCASE]);

         when Pragma_External_Name_Casing =>

         External_Name_Casing : declare
         begin
            GNAT_Pragma;
            Check_No_Identifiers;

            if Arg_Count = 2 then
               Check_Arg_Is_One_Of
                 (Arg2, Name_As_Is, Name_Uppercase, Name_Lowercase);

               case Chars (Get_Pragma_Arg (Arg2)) is
                  when Name_As_Is     =>
                     Opt.External_Name_Exp_Casing := As_Is;

                  when Name_Uppercase =>
                     Opt.External_Name_Exp_Casing := Uppercase;

                  when Name_Lowercase =>
                     Opt.External_Name_Exp_Casing := Lowercase;

                  when others =>
                     null;
               end case;

            else
               Check_Arg_Count (1);
            end if;

            Check_Arg_Is_One_Of (Arg1, Name_Uppercase, Name_Lowercase);

            case Chars (Get_Pragma_Arg (Arg1)) is
               when Name_Uppercase =>
                  Opt.External_Name_Imp_Casing := Uppercase;

               when Name_Lowercase =>
                  Opt.External_Name_Imp_Casing := Lowercase;

               when others =>
                  null;
            end case;
         end External_Name_Casing;

         -----------
         -- Ident --
         -----------

         --  pragma Ident (static_string_EXPRESSION)

         --  Note: pragma Comment shares this processing. Pragma Comment
         --  is identical to Ident, except that the restriction of the
         --  argument to 31 characters and the placement restrictions
         --  are not enforced for pragma Comment.

         when Pragma_Ident | Pragma_Comment => Ident : declare
            Str : Node_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Arg_Is_Static_Expression (Arg1, Standard_String);

            --  For pragma Ident, preserve DEC compatibility by requiring
            --  the pragma to appear in a declarative part or package spec.

            if Prag_Id = Pragma_Ident then
               Check_Is_In_Decl_Part_Or_Package_Spec;
            end if;

            Str := Expr_Value_S (Expression (Arg1));

            declare
               CS : Node_Id;
               GP : Node_Id;

            begin
               GP := Parent (Parent (N));

               if Nkind (GP) = N_Package_Declaration
                    or else
                  Nkind (GP) = N_Generic_Package_Declaration
               then
                  GP := Parent (GP);
               end if;

               --  If we have a compilation unit, then record the ident
               --  value, checking for improper duplication.

               if Nkind (GP) = N_Compilation_Unit then
                  CS := Ident_String (Current_Sem_Unit);

                  if Present (CS) then

                     --  For Ident, we do not permit multiple instances

                     if Prag_Id = Pragma_Ident then
                        Error_Pragma ("duplicate% pragma not permitted");

                     --  For Comment, we concatenate the string, unless we
                     --  want to preserve the tree structure for ASIS.

                     elsif not ASIS_Mode then
                        Start_String (Strval (CS));
                        Store_String_Char (' ');
                        Store_String_Chars (Strval (Str));
                        Set_Strval (CS, End_String);
                     end if;

                  else
                     --  In VMS, the effect of IDENT is achieved by passing
                     --  IDENTIFICATION=name as a --for-linker switch.

                     if OpenVMS_On_Target then
                        Start_String;
                        Store_String_Chars
                          ("--for-linker=IDENTIFICATION=");
                        String_To_Name_Buffer (Strval (Str));
                        Store_String_Chars (Name_Buffer (1 .. Name_Len));

                        --  Only the last processed IDENT is saved. The main
                        --  purpose is so an IDENT associated with a main
                        --  procedure will be used in preference to an IDENT
                        --  associated with a with'd package.

                        Replace_Linker_Option_String
                          (End_String, "--for-linker=IDENTIFICATION=");
                     end if;

                     Set_Ident_String (Current_Sem_Unit, Str);
                  end if;

               --  Otherwise we have a misplaced pragma Ident, but we ignore
               --  this if we are in an instantiation, since it comes from
               --  a generic, and has no relevance to the instantiation.

               elsif Prag_Id = Pragma_Ident then
                  if Instantiation_Location (Loc) = No_Location then
                     Error_Pragma ("pragma% only allowed at outer level");
                  end if;
               end if;
            end;
         end Ident;

         ------------
         -- Import --
         ------------

         --  pragma Import (
         --    [   Convention    =>] convention_IDENTIFIER,
         --    [   Entity        =>] local_NAME
         --    [, [External_Name =>] static_string_EXPRESSION ]
         --    [, [Link_Name     =>] static_string_EXPRESSION ]);

         when Pragma_Import =>
            Check_At_Least_N_Arguments (2);
            Check_At_Most_N_Arguments  (4);
            Process_Import_Or_Interface;

         ---------------------
         -- Import_Function --
         ---------------------

         --  pragma Import_Function (
         --        [Internal                 =>] LOCAL_NAME,
         --     [, [External                 =>] EXTERNAL_SYMBOL]
         --     [, [Parameter_Types          =>] (PARAMETER_TYPES)]
         --     [, [Result_Type              =>] SUBTYPE_MARK]
         --     [, [Mechanism                =>] MECHANISM]
         --     [, [Result_Mechanism         =>] MECHANISM_NAME]
         --     [, [First_Optional_Parameter =>] IDENTIFIER]);

         --  EXTERNAL_SYMBOL ::=
         --    IDENTIFIER
         --  | static_string_EXPRESSION

         --  PARAMETER_TYPES ::=
         --    null
         --  | TYPE_DESIGNATOR @{, TYPE_DESIGNATOR@}

         --  TYPE_DESIGNATOR ::=
         --    subtype_NAME
         --  | subtype_Name ' Access

         --  MECHANISM ::=
         --    MECHANISM_NAME
         --  | (MECHANISM_ASSOCIATION @{, MECHANISM_ASSOCIATION@})

         --  MECHANISM_ASSOCIATION ::=
         --    [formal_parameter_NAME =>] MECHANISM_NAME

         --  MECHANISM_NAME ::=
         --    Value
         --  | Reference
         --  | Descriptor [([Class =>] CLASS_NAME)]

         --  CLASS_NAME ::= ubs | ubsb | uba | s | sb | a | nca

         when Pragma_Import_Function => Import_Function : declare
            Args  : Args_List (1 .. 7);
            Names : constant Name_List (1 .. 7) := (
                      Name_Internal,
                      Name_External,
                      Name_Parameter_Types,
                      Name_Result_Type,
                      Name_Mechanism,
                      Name_Result_Mechanism,
                      Name_First_Optional_Parameter);

            Internal                 : Node_Id renames Args (1);
            External                 : Node_Id renames Args (2);
            Parameter_Types          : Node_Id renames Args (3);
            Result_Type              : Node_Id renames Args (4);
            Mechanism                : Node_Id renames Args (5);
            Result_Mechanism         : Node_Id renames Args (6);
            First_Optional_Parameter : Node_Id renames Args (7);

         begin
            GNAT_Pragma;
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Subprogram_Pragma (
              Arg_Internal                 => Internal,
              Arg_External                 => External,
              Arg_Parameter_Types          => Parameter_Types,
              Arg_Result_Type              => Result_Type,
              Arg_Mechanism                => Mechanism,
              Arg_Result_Mechanism         => Result_Mechanism,
              Arg_First_Optional_Parameter => First_Optional_Parameter);
         end Import_Function;

         -------------------
         -- Import_Object --
         -------------------

         --  pragma Import_Object (
         --        [Internal =>] LOCAL_NAME,
         --     [, [External =>] EXTERNAL_SYMBOL]
         --     [, [Size     =>] EXTERNAL_SYMBOL]);

         --  EXTERNAL_SYMBOL ::=
         --    IDENTIFIER
         --  | static_string_EXPRESSION

         when Pragma_Import_Object => Import_Object : declare
            Args  : Args_List (1 .. 3);
            Names : constant Name_List (1 .. 3) := (
                      Name_Internal,
                      Name_External,
                      Name_Size);

            Internal : Node_Id renames Args (1);
            External : Node_Id renames Args (2);
            Size     : Node_Id renames Args (3);

         begin
            GNAT_Pragma;
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Object_Pragma (
              Arg_Internal => Internal,
              Arg_External => External,
              Arg_Size     => Size);
         end Import_Object;

         ----------------------
         -- Import_Procedure --
         ----------------------

         --  pragma Import_Procedure (
         --        [Internal                 =>] LOCAL_NAME,
         --     [, [External                 =>] EXTERNAL_SYMBOL]
         --     [, [Parameter_Types          =>] (PARAMETER_TYPES)]
         --     [, [Mechanism                =>] MECHANISM]
         --     [, [First_Optional_Parameter =>] IDENTIFIER]);

         --  EXTERNAL_SYMBOL ::=
         --    IDENTIFIER
         --  | static_string_EXPRESSION

         --  PARAMETER_TYPES ::=
         --    null
         --  | TYPE_DESIGNATOR @{, TYPE_DESIGNATOR@}

         --  TYPE_DESIGNATOR ::=
         --    subtype_NAME
         --  | subtype_Name ' Access

         --  MECHANISM ::=
         --    MECHANISM_NAME
         --  | (MECHANISM_ASSOCIATION @{, MECHANISM_ASSOCIATION@})

         --  MECHANISM_ASSOCIATION ::=
         --    [formal_parameter_NAME =>] MECHANISM_NAME

         --  MECHANISM_NAME ::=
         --    Value
         --  | Reference
         --  | Descriptor [([Class =>] CLASS_NAME)]

         --  CLASS_NAME ::= ubs | ubsb | uba | s | sb | a | nca

         when Pragma_Import_Procedure => Import_Procedure : declare
            Args  : Args_List (1 .. 5);
            Names : constant Name_List (1 .. 5) := (
                      Name_Internal,
                      Name_External,
                      Name_Parameter_Types,
                      Name_Mechanism,
                      Name_First_Optional_Parameter);

            Internal                 : Node_Id renames Args (1);
            External                 : Node_Id renames Args (2);
            Parameter_Types          : Node_Id renames Args (3);
            Mechanism                : Node_Id renames Args (4);
            First_Optional_Parameter : Node_Id renames Args (5);

         begin
            GNAT_Pragma;
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Subprogram_Pragma (
              Arg_Internal                 => Internal,
              Arg_External                 => External,
              Arg_Parameter_Types          => Parameter_Types,
              Arg_Mechanism                => Mechanism,
              Arg_First_Optional_Parameter => First_Optional_Parameter);
         end Import_Procedure;

         ------------------------
         -- Initialize_Scalars --
         ------------------------

         --  pragma Initialize_Scalars;

         when Pragma_Initialize_Scalars =>
            GNAT_Pragma;
            Check_Arg_Count (0);
            Check_Valid_Configuration_Pragma;
            Check_Restriction (No_Initialize_Scalars, N);

            if not Restrictions (No_Initialize_Scalars) then
               Init_Or_Norm_Scalars := True;
               Initialize_Scalars := True;
            end if;

         ------------
         -- Inline --
         ------------

         --  pragma Inline ( NAME {, NAME} );

         when Pragma_Inline =>

            --  Pragma is active if inlining option is active

            if Inline_Active then
               Process_Inline (True);

            --  Pragma is active in a predefined file in config run time mode

            elsif Configurable_Run_Time_Mode
              and then
                Is_Predefined_File_Name (Unit_File_Name (Current_Sem_Unit))
            then
               Process_Inline (True);

            --  Otherwise inlining is not active

            else
               Process_Inline (False);
            end if;

         -------------------
         -- Inline_Always --
         -------------------

         --  pragma Inline_Always ( NAME {, NAME} );

         when Pragma_Inline_Always =>
            Process_Inline (True);

         --------------------
         -- Inline_Generic --
         --------------------

         --  pragma Inline_Generic (NAME {, NAME});

         when Pragma_Inline_Generic =>
            Process_Generic_List;

         ---------------
         -- Interface --
         ---------------

         --  pragma Interface (
         --    convention_IDENTIFIER,
         --    local_NAME );

         when Pragma_Interface =>
            GNAT_Pragma;
            Check_Arg_Count (2);
            Check_No_Identifiers;
            Process_Import_Or_Interface;

         --------------------
         -- Interface_Name --
         --------------------

         --  pragma Interface_Name (
         --    [  Entity        =>] local_NAME
         --    [,[External_Name =>] static_string_EXPRESSION ]
         --    [,[Link_Name     =>] static_string_EXPRESSION ]);

         when Pragma_Interface_Name => Interface_Name : declare
            Id     : Node_Id;
            Def_Id : Entity_Id;
            Hom_Id : Entity_Id;
            Found  : Boolean;

         begin
            GNAT_Pragma;
            Check_At_Least_N_Arguments (2);
            Check_At_Most_N_Arguments  (3);
            Id := Expression (Arg1);
            Analyze (Id);

            if not Is_Entity_Name (Id) then
               Error_Pragma_Arg
                 ("first argument for pragma% must be entity name", Arg1);
            elsif Etype (Id) = Any_Type then
               return;
            else
               Def_Id := Entity (Id);
            end if;

            --  Special DEC-compatible processing for the object case,
            --  forces object to be imported.

            if Ekind (Def_Id) = E_Variable then
               Kill_Size_Check_Code (Def_Id);
               Note_Possible_Modification (Id);

               --  Initialization is not allowed for imported variable

               if Present (Expression (Parent (Def_Id)))
                 and then Comes_From_Source (Expression (Parent (Def_Id)))
               then
                  Error_Msg_Sloc := Sloc (Def_Id);
                  Error_Pragma_Arg
                    ("no initialization allowed for declaration of& #",
                     Arg2);

               else
                  --  For compatibility, support VADS usage of providing both
                  --  pragmas Interface and Interface_Name to obtain the effect
                  --  of a single Import pragma.

                  if Is_Imported (Def_Id)
                    and then Present (First_Rep_Item (Def_Id))
                    and then Nkind (First_Rep_Item (Def_Id)) = N_Pragma
                    and then Chars (First_Rep_Item (Def_Id)) = Name_Interface
                  then
                     null;
                  else
                     Set_Imported (Def_Id);
                  end if;

                  Set_Is_Public (Def_Id);
                  Process_Interface_Name (Def_Id, Arg2, Arg3);
               end if;

            --  Otherwise must be subprogram

            elsif not Is_Subprogram (Def_Id) then
               Error_Pragma_Arg
                 ("argument of pragma% is not subprogram", Arg1);

            else
               Check_At_Most_N_Arguments (3);
               Hom_Id := Def_Id;
               Found := False;

               --  Loop through homonyms

               loop
                  Def_Id := Get_Base_Subprogram (Hom_Id);

                  if Is_Imported (Def_Id) then
                     Process_Interface_Name (Def_Id, Arg2, Arg3);
                     Found := True;
                  end if;

                  Hom_Id := Homonym (Hom_Id);

                  exit when No (Hom_Id)
                    or else Scope (Hom_Id) /= Current_Scope;
               end loop;

               if not Found then
                  Error_Pragma_Arg
                    ("argument of pragma% is not imported subprogram",
                     Arg1);
               end if;
            end if;
         end Interface_Name;

         ---------------
         -- Link_With --
         ---------------

         --  pragma Link_With (string_EXPRESSION {, string_EXPRESSION});

         when Pragma_Link_With => Link_With : declare
            Arg : Node_Id;

         begin
            GNAT_Pragma;

            if Operating_Mode = Generate_Code
              and then In_Extended_Main_Source_Unit (N)
            then
               Check_At_Least_N_Arguments (1);
               Check_No_Identifiers;
               Check_Is_In_Decl_Part_Or_Package_Spec;
               Check_Arg_Is_Static_Expression (Arg1, Standard_String);
               Start_String;

               Arg := Arg1;
               while Present (Arg) loop
                  Check_Arg_Is_Static_Expression (Arg, Standard_String);

                  --  Store argument, converting sequences of spaces
                  --  to a single null character (this is one of the
                  --  differences in processing between Link_With
                  --  and Linker_Options).

                  declare
                     C : constant Char_Code := Get_Char_Code (' ');
                     S : constant String_Id :=
                           Strval (Expr_Value_S (Expression (Arg)));
                     L : constant Nat := String_Length (S);
                     F : Nat := 1;

                     procedure Skip_Spaces;
                     --  Advance F past any spaces

                     procedure Skip_Spaces is
                     begin
                        while F <= L and then Get_String_Char (S, F) = C loop
                           F := F + 1;
                        end loop;
                     end Skip_Spaces;

                  begin
                     Skip_Spaces; -- skip leading spaces

                     --  Loop through characters, changing any embedded
                     --  sequence of spaces to a single null character
                     --  (this is how Link_With/Linker_Options differ)

                     while F <= L loop
                        if Get_String_Char (S, F) = C then
                           Skip_Spaces;
                           exit when F > L;
                           Store_String_Char (ASCII.NUL);

                        else
                           Store_String_Char (Get_String_Char (S, F));
                           F := F + 1;
                        end if;
                     end loop;
                  end;

                  Arg := Next (Arg);

                  if Present (Arg) then
                     Store_String_Char (ASCII.NUL);
                  end if;
               end loop;

               Store_Linker_Option_String (End_String);
            end if;
         end Link_With;

         ------------------
         -- Linker_Alias --
         ------------------

         --  pragma Linker_Alias (
         --      [Entity =>]  LOCAL_NAME
         --      [Alias  =>]  static_string_EXPRESSION);

         when Pragma_Linker_Alias =>
            GNAT_Pragma;
            Check_Arg_Count (2);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Optional_Identifier (Arg2, "alias");
            Check_Arg_Is_Library_Level_Local_Name (Arg1);
            Check_Arg_Is_Static_Expression (Arg2, Standard_String);

            --  The only processing required is to link this item on to the
            --  list of rep items for the given entity. This is accomplished
            --  by the call to Rep_Item_Too_Late (when no error is detected
            --  and False is returned).

            if Rep_Item_Too_Late (Entity (Expression (Arg1)), N) then
               return;
            else
               Set_Has_Gigi_Rep_Item (Entity (Expression (Arg1)));
            end if;

         --------------------
         -- Linker_Options --
         --------------------

         --  pragma Linker_Options (string_EXPRESSION {, string_EXPRESSION});

         when Pragma_Linker_Options => Linker_Options : declare
            Arg : Node_Id;

         begin
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Is_In_Decl_Part_Or_Package_Spec;

            if Operating_Mode = Generate_Code
              and then In_Extended_Main_Source_Unit (N)
            then
               Check_Arg_Is_Static_Expression (Arg1, Standard_String);
               Start_String (Strval (Expr_Value_S (Expression (Arg1))));

               Arg := Arg2;
               while Present (Arg) loop
                  Check_Arg_Is_Static_Expression (Arg, Standard_String);
                  Store_String_Char (ASCII.NUL);
                  Store_String_Chars
                    (Strval (Expr_Value_S (Expression (Arg))));
                  Arg := Next (Arg);
               end loop;

               Store_Linker_Option_String (End_String);
            end if;
         end Linker_Options;

         -----------------
         -- Memory_Size --
         -----------------

         --  pragma Memory_Size (NUMERIC_LITERAL)

         when Pragma_Memory_Size =>
            GNAT_Pragma;

            --  Memory size is simply ignored

            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_Integer_Literal (Arg1);

         ---------------
         -- No_Return --
         ---------------

         --  pragma No_Return (procedure_LOCAL_NAME);

         when Pragma_No_Return => No_Return : declare
            Id    : Node_Id;
            E     : Entity_Id;
            Found : Boolean;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Arg_Is_Local_Name (Arg1);
            Id := Expression (Arg1);
            Analyze (Id);

            if not Is_Entity_Name (Id) then
               Error_Pragma_Arg ("entity name required", Arg1);
            end if;

            if Etype (Id) = Any_Type then
               raise Pragma_Exit;
            end if;

            E := Entity (Id);

            Found := False;
            while Present (E)
              and then Scope (E) = Current_Scope
            loop
               if Ekind (E) = E_Procedure
                 or else Ekind (E) = E_Generic_Procedure
               then
                  Set_No_Return (E);
                  Found := True;
               end if;

               E := Homonym (E);
            end loop;

            if not Found then
               Error_Pragma ("no procedures found for pragma%");
            end if;
         end No_Return;

         -----------------
         -- Obsolescent --
         -----------------

         --  pragma Obsolescent [(static_string_EXPRESSION)];

         when Pragma_Obsolescent => Obsolescent : declare
         begin
            GNAT_Pragma;
            Check_At_Most_N_Arguments (1);
            Check_No_Identifiers;

            if Arg_Count = 1 then
               Check_Arg_Is_Static_Expression (Arg1, Standard_String);
            end if;

            if No (Prev (N))
              or else (Nkind (Prev (N))) /= N_Subprogram_Declaration
            then
               Error_Pragma
                 ("pragma% misplaced, must immediately " &
                  "follow subprogram spec");
            end if;
         end Obsolescent;

         -----------------
         -- No_Run_Time --
         -----------------

         --  pragma No_Run_Time

         --  Note: this pragma is retained for backwards compatibiltiy.
         --  See body of Rtsfind for full details on its handling.

         when Pragma_No_Run_Time =>
            GNAT_Pragma;
            Check_Valid_Configuration_Pragma;
            Check_Arg_Count (0);

            No_Run_Time_Mode           := True;
            Configurable_Run_Time_Mode := True;

            declare
               Word32 : constant Boolean := Ttypes.System_Word_Size = 32;
            begin
               if Word32 then
                  Duration_32_Bits_On_Target := True;
               end if;
            end;

            Restrictions (No_Finalization)       := True;
            Restrictions (No_Exception_Handlers) := True;

         --------------
         -- Optimize --
         --------------

         --  pragma Optimize (Time | Space);

         --  The actual check for optimize is done in Gigi. Note that this
         --  pragma does not actually change the optimization setting, it
         --  simply checks that it is consistent with the pragma.

         when Pragma_Optimize =>
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_One_Of (Arg1, Name_Time, Name_Space, Name_Off);

         ----------------
         -- Overriding --
         ----------------

         when Pragma_Overriding =>
            Error_Msg_N ("pragma must appear immediately after subprogram", N);

         ----------
         -- Pack --
         ----------

         --  pragma Pack (first_subtype_LOCAL_NAME);

         when Pragma_Pack => Pack : declare
            Assoc   : constant Node_Id := Arg1;
            Type_Id : Node_Id;
            Typ     : Entity_Id;

         begin
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_Local_Name (Arg1);

            Type_Id := Expression (Assoc);
            Find_Type (Type_Id);
            Typ := Entity (Type_Id);

            if Typ = Any_Type
              or else Rep_Item_Too_Early (Typ, N)
            then
               return;
            else
               Typ := Underlying_Type (Typ);
            end if;

            if not Is_Array_Type (Typ) and then not Is_Record_Type (Typ) then
               Error_Pragma ("pragma% must specify array or record type");
            end if;

            Check_First_Subtype (Arg1);

            if Has_Pragma_Pack (Typ) then
               Error_Pragma ("duplicate pragma%, only one allowed");

            --  Array type. We set the Has_Pragma_Pack flag, and Is_Packed,
            --  but not Has_Non_Standard_Rep, because we don't actually know
            --  till freeze time if the array can have packed representation.
            --  That's because in the general case we do not know enough about
            --  the component type until it in turn is frozen, which certainly
            --  happens before the array type is frozen, but not necessarily
            --  till that point (i.e. right now it may be unfrozen).

            elsif Is_Array_Type (Typ) then
               if Has_Aliased_Components (Base_Type (Typ)) then
                  Error_Pragma
                    ("pragma% ignored, cannot pack aliased components?");

               elsif not Rep_Item_Too_Late (Typ, N) then
                  Set_Is_Packed            (Base_Type (Typ));
                  Set_Has_Pragma_Pack      (Base_Type (Typ));
                  Set_Has_Non_Standard_Rep (Base_Type (Typ));
               end if;

            --  Record type. For record types, the pack is always effective

            else pragma Assert (Is_Record_Type (Typ));
               if not Rep_Item_Too_Late (Typ, N) then
                  Set_Has_Pragma_Pack      (Base_Type (Typ));
                  Set_Is_Packed            (Base_Type (Typ));
                  Set_Has_Non_Standard_Rep (Base_Type (Typ));
               end if;
            end if;
         end Pack;

         -------------
         -- Polling --
         -------------

         --  pragma Polling (ON | OFF);

         when Pragma_Polling =>
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Arg_Is_One_Of (Arg1, Name_On, Name_Off);
            Polling_Required := (Chars (Expression (Arg1)) = Name_On);

         ---------------------
         -- Persistent_Data --
         ---------------------

         when Pragma_Persistent_Data => declare
            Ent : Entity_Id;

         begin
            --  Register the pragma as applying to the compilation unit.
            --  Individual Persistent_Object pragmas for relevant objects
            --  are generated the end of the compilation.

            GNAT_Pragma;
            Check_Valid_Configuration_Pragma;
            Check_Arg_Count (0);
            Ent := Find_Lib_Unit_Name;
            Set_Is_Preelaborated (Ent);
         end;

         ------------------------
         --  Persistent_Object --
         ------------------------

         when Pragma_Persistent_Object => null;
         ----------
         -- Pure --
         ----------

         --  pragma Pure [(library_unit_NAME)];

         when Pragma_Pure => Pure : declare
            Ent : Entity_Id;
         begin
            Check_Valid_Library_Unit_Pragma;

            if Nkind (N) = N_Null_Statement then
               return;
            end if;

            Ent := Find_Lib_Unit_Name;
            Set_Is_Pure (Ent);
            Set_Suppress_Elaboration_Warnings (Ent);
         end Pure;

         -------------------
         -- Pure_Function --
         -------------------

         --  pragma Pure_Function ([Entity =>] function_LOCAL_NAME);

         when Pragma_Pure_Function => Pure_Function : declare
            E_Id   : Node_Id;
            E      : Entity_Id;
            Def_Id : Entity_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Arg_Is_Local_Name (Arg1);
            E_Id := Expression (Arg1);

            if Error_Posted (E_Id) then
               return;
            end if;

            --  Loop through homonyms (overloadings) of referenced entity

            E := Entity (E_Id);

            if Present (E) then
               loop
                  Def_Id := Get_Base_Subprogram (E);

                  if Ekind (Def_Id) /= E_Function
                    and then Ekind (Def_Id) /= E_Generic_Function
                    and then Ekind (Def_Id) /= E_Operator
                  then
                     Error_Pragma_Arg
                       ("pragma% requires a function name", Arg1);
                  end if;

                  Set_Is_Pure (Def_Id);
                  Set_Has_Pragma_Pure_Function (Def_Id);

                  E := Homonym (E);
                  exit when No (E) or else Scope (E) /= Current_Scope;
               end loop;
            end if;
         end Pure_Function;

         ---------------
         -- Ravenscar --
         ---------------

         --  pragma Ravenscar;

         when Pragma_Ravenscar =>
            GNAT_Pragma;
            Check_Arg_Count (0);
            Check_Valid_Configuration_Pragma;
            Set_Ravenscar (N);

         -------------------------
         -- Restricted_Run_Time --
         -------------------------

         --  pragma Restricted_Run_Time;

         when Pragma_Restricted_Run_Time =>
            GNAT_Pragma;
            Check_Arg_Count (0);
            Check_Valid_Configuration_Pragma;
            Set_Restricted_Profile (N);

         ------------------
         -- Restrictions --
         ------------------

         --  pragma Restrictions (RESTRICTION {, RESTRICTION});

         --  RESTRICTION ::=
         --    restriction_IDENTIFIER
         --  | restriction_parameter_IDENTIFIER => EXPRESSION

         when Pragma_Restrictions => Restrictions_Pragma : declare
            Arg   : Node_Id;
            R_Id  : Restriction_Id;
            RP_Id : Restriction_Parameter_Id;
            Id    : Name_Id;
            Expr  : Node_Id;
            Val   : Uint;

         begin
            Check_At_Least_N_Arguments (1);
            Check_Valid_Configuration_Pragma;

            Arg := Arg1;
            while Present (Arg) loop
               Id := Chars (Arg);
               Expr := Expression (Arg);

               --  Case of no restriction identifier

               if Id = No_Name then
                  if Nkind (Expr) /= N_Identifier then
                     Error_Pragma_Arg
                       ("invalid form for restriction", Arg);

                  else
                     R_Id := Get_Restriction_Id (Chars (Expr));

                     if R_Id = Not_A_Restriction_Id then
                        Error_Pragma_Arg
                          ("invalid restriction identifier", Arg);

                     --  Restriction is active

                     else
                        if Implementation_Restriction (R_Id) then
                           Check_Restriction
                             (No_Implementation_Restrictions, Arg);
                        end if;

                        Restrictions (R_Id) := True;

                        --  Set location, but preserve location of system
                        --  restriction for nice error msg with run time name

                        if Restrictions_Loc (R_Id) /= System_Location then
                           Restrictions_Loc (R_Id) := Sloc (N);
                        end if;

                        --  Record the restriction if we are in the main unit,
                        --  or in the extended main unit. The reason that we
                        --  test separately for Main_Unit is that gnat.adc is
                        --  processed with Current_Sem_Unit = Main_Unit, but
                        --  nodes in gnat.adc do not appear to be the extended
                        --  main source unit (they probably should do ???)

                        if Current_Sem_Unit = Main_Unit
                          or else In_Extended_Main_Source_Unit (N)
                        then
                           Main_Restrictions (R_Id) := True;
                        end if;

                        --  A very special case that must be processed here:
                        --  pragma Restrictions (No_Exceptions) turns off all
                        --  run-time checking. This is a bit dubious in terms
                        --  of the formal language definition, but it is what
                        --  is intended by the wording of RM H.4(12).

                        if R_Id = No_Exceptions then
                           Scope_Suppress := (others => True);
                        end if;
                     end if;
                  end if;

               --  Case of restriction identifier present

               else
                  RP_Id := Get_Restriction_Parameter_Id (Id);
                  Analyze_And_Resolve (Expr, Any_Integer);

                  if RP_Id = Not_A_Restriction_Parameter_Id then
                     Error_Pragma_Arg
                       ("invalid restriction parameter identifier", Arg);

                  elsif not Is_OK_Static_Expression (Expr) then
                     Flag_Non_Static_Expr
                       ("value must be static expression!", Expr);
                     raise Pragma_Exit;

                  elsif not Is_Integer_Type (Etype (Expr))
                    or else Expr_Value (Expr) < 0
                  then
                     Error_Pragma_Arg
                       ("value must be non-negative integer", Arg);

                  --  Restriction pragma is active

                  else
                     Val := Expr_Value (Expr);

                     --  Record pragma if most restrictive so far

                     if Restriction_Parameters (RP_Id) = No_Uint
                       or else Val < Restriction_Parameters (RP_Id)
                     then
                        Restriction_Parameters (RP_Id) := Val;
                        Restriction_Parameters_Loc (RP_Id) := Sloc (N);
                     end if;
                  end if;
               end if;

               Next (Arg);
            end loop;
         end Restrictions_Pragma;

         --------------------------
         -- Restriction_Warnings --
         --------------------------

         --  pragma Restriction_Warnings (RESTRICTION {, RESTRICTION});

         --  RESTRICTION ::= restriction_IDENTIFIER

         when Pragma_Restriction_Warnings => Restriction_Warn : declare
            Arg   : Node_Id;
            R_Id  : Restriction_Id;
            Expr  : Node_Id;

         begin
            GNAT_Pragma;
            Check_At_Least_N_Arguments (1);
            Check_Valid_Configuration_Pragma;
            Check_No_Identifiers;

            Arg := Arg1;
            while Present (Arg) loop
               Expr := Expression (Arg);

               if Nkind (Expr) /= N_Identifier then
                  Error_Pragma_Arg
                    ("invalid form for restriction", Arg);

               else
                  R_Id := Get_Restriction_Id (Chars (Expr));

                  if R_Id = Not_A_Restriction_Id then
                     Error_Pragma_Arg
                       ("invalid restriction identifier", Arg);

                  --  Restriction is active

                  else
                     if Implementation_Restriction (R_Id) then
                        Check_Restriction
                          (No_Implementation_Restrictions, Arg);
                     end if;

                     Restriction_Warnings (R_Id) := True;
                  end if;
               end if;

               Next (Arg);
            end loop;
         end Restriction_Warn;

         ----------------------
         -- Source_Reference --
         ----------------------

         --  pragma Source_Reference (INTEGER_LITERAL [, STRING_LITERAL]);

         --  Nothing to do, all processing completed in Par.Prag, since we
         --  need the information for possible parser messages that are output

         when Pragma_Source_Reference =>
            GNAT_Pragma;

         ------------------
         -- Storage_Unit --
         ------------------

         --  pragma Storage_Unit (NUMERIC_LITERAL);

         --  Only permitted argument is System'Storage_Unit value

         when Pragma_Storage_Unit =>
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_Integer_Literal (Arg1);

            if Intval (Expression (Arg1)) /=
              UI_From_Int (Ttypes.System_Storage_Unit)
            then
               Error_Msg_Uint_1 := UI_From_Int (Ttypes.System_Storage_Unit);
               Error_Pragma_Arg
                 ("the only allowed argument for pragma% is ^", Arg1);
            end if;

         -------------------------
         -- Style_Checks (GNAT) --
         -------------------------

         --  pragma Style_Checks (On | Off | ALL_CHECKS | STRING_LITERAL);

         --  This is processed by the parser since some of the style
         --  checks take place during source scanning and parsing. This
         --  means that we don't need to issue error messages here.

         when Pragma_Style_Checks => Style_Checks : declare
            A  : constant Node_Id   := Expression (Arg1);
            S  : String_Id;
            C  : Char_Code;

         begin
            GNAT_Pragma;
            Check_No_Identifiers;

            --  Two argument form

            if Arg_Count = 2 then
               Check_Arg_Is_One_Of (Arg1, Name_On, Name_Off);

               declare
                  E_Id : Node_Id;
                  E    : Entity_Id;

               begin
                  E_Id := Expression (Arg2);
                  Analyze (E_Id);

                  if not Is_Entity_Name (E_Id) then
                     Error_Pragma_Arg
                       ("second argument of pragma% must be entity name",
                        Arg2);
                  end if;

                  E := Entity (E_Id);

                  if E = Any_Id then
                     return;
                  else
                     loop
                        Set_Suppress_Style_Checks (E,
                          (Chars (Expression (Arg1)) = Name_Off));
                        exit when No (Homonym (E));
                        E := Homonym (E);
                     end loop;
                  end if;
               end;

            --  One argument form

            else
               Check_Arg_Count (1);

               if Nkind (A) = N_String_Literal then
                  S   := Strval (A);

                  declare
                     Slen    : constant Natural := Natural (String_Length (S));
                     Options : String (1 .. Slen);
                     J       : Natural;

                  begin
                     J := 1;
                     loop
                        C := Get_String_Char (S, Int (J));
                        exit when not In_Character_Range (C);
                        Options (J) := Get_Character (C);

                        if J = Slen then
                           Set_Style_Check_Options (Options);
                           exit;
                        else
                           J := J + 1;
                        end if;
                     end loop;
                  end;

               elsif Nkind (A) = N_Identifier then

                  if Chars (A) = Name_All_Checks then
                     Set_Default_Style_Check_Options;

                  elsif Chars (A) = Name_On then
                     Style_Check := True;

                  elsif Chars (A) = Name_Off then
                     Style_Check := False;

                  end if;
               end if;
            end if;
         end Style_Checks;

         --------------
         -- Suppress --
         --------------

         --  pragma Suppress (IDENTIFIER [, [On =>] NAME]);

         when Pragma_Suppress =>
            Process_Suppress_Unsuppress (True);

         ------------------
         -- Suppress_All --
         ------------------

         --  pragma Suppress_All;

         --  The only check made here is that the pragma appears in the
         --  proper place, i.e. following a compilation unit. If indeed
         --  it appears in this context, then the parser has already
         --  inserted an equivalent pragma Suppress (All_Checks) to get
         --  the required effect.

         when Pragma_Suppress_All =>
            GNAT_Pragma;
            Check_Arg_Count (0);

            if Nkind (Parent (N)) /= N_Compilation_Unit_Aux
              or else not Is_List_Member (N)
              or else List_Containing (N) /= Pragmas_After (Parent (N))
            then
               Error_Pragma
                 ("misplaced pragma%, must follow compilation unit");
            end if;

         -----------------------------
         -- Suppress_Initialization --
         -----------------------------

         --  pragma Suppress_Initialization ([Entity =>] type_Name);

         when Pragma_Suppress_Initialization => Suppress_Init : declare
            E_Id : Node_Id;
            E    : Entity_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Arg_Is_Local_Name (Arg1);

            E_Id := Expression (Arg1);

            if Etype (E_Id) = Any_Type then
               return;
            end if;

            E := Entity (E_Id);

            if Is_Type (E) then
               if Is_Incomplete_Or_Private_Type (E) then
                  if No (Full_View (Base_Type (E))) then
                     Error_Pragma_Arg
                       ("argument of pragma% cannot be an incomplete type",
                         Arg1);
                  else
                     Set_Suppress_Init_Proc (Full_View (Base_Type (E)));
                  end if;
               else
                  Set_Suppress_Init_Proc (Base_Type (E));
               end if;

            else
               Error_Pragma_Arg
                 ("pragma% requires argument that is a type name", Arg1);
            end if;
         end Suppress_Init;

         -----------------
         -- System_Name --
         -----------------

         --  pragma System_Name (DIRECT_NAME);

         --  Syntax check: one argument, which must be the identifier GNAT
         --  or the identifier GCC, no other identifiers are acceptable.

         when Pragma_System_Name =>
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_One_Of (Arg1, Name_Gcc, Name_Gnat);

         ------------------
         -- Unreferenced --
         ------------------

         --  pragma Unreferenced (local_Name {, local_Name});

         when Pragma_Unreferenced => Unreferenced : declare
            Arg_Node : Node_Id;
            Arg_Expr : Node_Id;
            Arg_Ent  : Entity_Id;

         begin
            GNAT_Pragma;
            Check_At_Least_N_Arguments (1);

            Arg_Node := Arg1;

            while Present (Arg_Node) loop
               Check_No_Identifier (Arg_Node);

               --  Note that the analyze call done by Check_Arg_Is_Local_Name
               --  will in fact generate a reference, so that the entity will
               --  have a reference, which will inhibit any warnings about it
               --  not being referenced, and also properly show up in the ali
               --  file as a reference. But this reference is recorded before
               --  the Has_Pragma_Unreferenced flag is set, so that no warning
               --  is generated for this reference.

               Check_Arg_Is_Local_Name (Arg_Node);
               Arg_Expr := Get_Pragma_Arg (Arg_Node);

               if Is_Entity_Name (Arg_Expr) then
                  Arg_Ent := Entity (Arg_Expr);

                  --  If the entity is overloaded, the pragma applies to the
                  --  most recent overloading, as documented. In this case,
                  --  name resolution does not generate a reference, so it
                  --  must be done here explicitly.

                  if Is_Overloaded (Arg_Expr) then
                     Generate_Reference (Arg_Ent, N);
                  end if;

                  Set_Has_Pragma_Unreferenced (Arg_Ent);
               end if;

               Next (Arg_Node);
            end loop;
         end Unreferenced;

         ----------------
         -- Unsuppress --
         ----------------

         --  pragma Unsuppress (IDENTIFIER [, [On =>] NAME]);

         when Pragma_Unsuppress =>
            GNAT_Pragma;
            Process_Suppress_Unsuppress (False);

         ---------------------
         -- Validity_Checks --
         ---------------------

         --  pragma Validity_Checks (On | Off | ALL_CHECKS | STRING_LITERAL);

         when Pragma_Validity_Checks => Validity_Checks : declare
            A  : constant Node_Id   := Expression (Arg1);
            S  : String_Id;
            C  : Char_Code;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_No_Identifiers;

            if Nkind (A) = N_String_Literal then
               S   := Strval (A);

               declare
                  Slen    : constant Natural := Natural (String_Length (S));
                  Options : String (1 .. Slen);
                  J       : Natural;

               begin
                  J := 1;
                  loop
                     C := Get_String_Char (S, Int (J));
                     exit when not In_Character_Range (C);
                     Options (J) := Get_Character (C);

                     if J = Slen then
                        Set_Validity_Check_Options (Options);
                        exit;
                     else
                        J := J + 1;
                     end if;
                  end loop;
               end;

            elsif Nkind (A) = N_Identifier then

               if Chars (A) = Name_All_Checks then
                  Set_Validity_Check_Options ("a");

               elsif Chars (A) = Name_On then
                  Validity_Checks_On := True;

               elsif Chars (A) = Name_Off then
                  Validity_Checks_On := False;

               end if;
            end if;
         end Validity_Checks;

         --------------
         -- Volatile --
         --------------

         --  pragma Volatile (LOCAL_NAME);

         when Pragma_Volatile =>
            null; -- Process_Atomic_Shared_Volatile;

         -------------------------
         -- Volatile_Components --
         -------------------------

         --  pragma Volatile_Components (array_LOCAL_NAME);

         --  Volatile is handled by the same circuit as Atomic_Components

         --------------
         -- Warnings --
         --------------

         --  pragma Warnings (On | Off, [LOCAL_NAME])

         when Pragma_Warnings => Warnings : begin
            GNAT_Pragma;
            Check_At_Least_N_Arguments (1);
            Check_At_Most_N_Arguments (2);
            Check_No_Identifiers;

            --  One argument case was processed by parser in Par.Prag

            if Arg_Count /= 1 then
               Check_Arg_Is_One_Of (Arg1, Name_On, Name_Off);
               Check_Arg_Count (2);

               declare
                  E_Id : Node_Id;
                  E    : Entity_Id;

               begin
                  E_Id := Expression (Arg2);
                  Analyze (E_Id);

                  --  In the expansion of an inlined body, a reference to
                  --  the formal may be wrapped in a conversion if the actual
                  --  is a conversion. Retrieve the real entity name.

                  if In_Instance_Body
                    and then Nkind (E_Id) = N_Unchecked_Type_Conversion
                  then
                     E_Id := Expression (E_Id);
                  end if;

                  if not Is_Entity_Name (E_Id) then
                     Error_Pragma_Arg
                       ("second argument of pragma% must be entity name",
                        Arg2);
                  end if;

                  E := Entity (E_Id);

                  if E = Any_Id then
                     return;
                  else
                     loop
                        Set_Warnings_Off (E,
                          (Chars (Expression (Arg1)) = Name_Off));

                        if Is_Enumeration_Type (E) then
                           declare
                              Lit : Entity_Id := First_Literal (E);

                           begin
                              while Present (Lit) loop
                                 Set_Warnings_Off (Lit);
                                 Next_Literal (Lit);
                              end loop;
                           end;
                        end if;

                        exit when No (Homonym (E));
                        E := Homonym (E);
                     end loop;
                  end if;
               end;
            end if;
         end Warnings;

         --------------------
         -- Unknown_Pragma --
         --------------------

         --  Should be impossible, since the case of an unknown pragma is
         --  separately processed before the case statement is entered.

         when Unknown_Pragma =>
            raise Program_Error;

         when others =>
            null;
      end case;

   exception
      when Pragma_Exit => null;
   end Analyze_Pragma;

   -------------------------
   -- Get_Base_Subprogram --
   -------------------------

   function Get_Base_Subprogram (Def_Id : Entity_Id) return Entity_Id is
      Result : Entity_Id;

   begin
      Result := Def_Id;

      --  Follow subprogram renaming chain

      while Is_Subprogram (Result)
        and then
          (Is_Generic_Instance (Result)
            or else Nkind (Parent (Declaration_Node (Result))) =
              N_Subprogram_Renaming_Declaration)
        and then Present (Alias (Result))
      loop
         Result := Alias (Result);
      end loop;

      return Result;
   end Get_Base_Subprogram;

   -----------------------------------------
   -- Is_Non_Significant_Pragma_Reference --
   -----------------------------------------

   --  This function makes use of the following static table which indicates
   --  whether a given pragma is significant. A value of -1 in this table
   --  indicates that the reference is significant. A value of zero indicates
   --  than appearence as any argument is insignificant, a positive value
   --  indicates that appearence in that parameter position is significant.

   Sig_Flags : constant array (Pragma_Id) of Int :=
     (Pragma_Annotate                     => -1,
      Pragma_Assert                       => -1,
      Pragma_C_Pass_By_Copy               =>  0,
      Pragma_Comment                      =>  0,
      Pragma_Common_Object                => -1,
      Pragma_Compile_Time_Warning         => -1,
      Pragma_Component_Alignment          => -1,
      Pragma_Convention                   =>  0,
      Pragma_Convention_Identifier        =>  0,
      Pragma_Debug                        => -1,
      Pragma_Discard_Names                =>  0,
      Pragma_Elaborate                    => -1,
      Pragma_Elaborate_All                => -1,
      Pragma_Elaborate_Body               => -1,
      Pragma_Elaboration_Checks           => -1,
      Pragma_Eliminate                    => -1,
      Pragma_Explicit_Overriding          => -1,
      Pragma_Export                       => -1,
      Pragma_Export_Exception             => -1,
      Pragma_Export_Function              => -1,
      Pragma_Export_Object                => -1,
      Pragma_Export_Procedure             => -1,
      Pragma_Extensions_Allowed           => -1,
      Pragma_External                     => -1,
      Pragma_External_Name_Casing         => -1,
      Pragma_Finalize_Storage_Only        =>  0,
      Pragma_Float_Representation         =>  0,
      Pragma_Ident                        => -1,
      Pragma_Import                       => +2,
      Pragma_Import_Function              =>  0,
      Pragma_Import_Object                =>  0,
      Pragma_Import_Procedure             =>  0,
      Pragma_Initialize_Scalars           => -1,
      Pragma_Inline                       =>  0,
      Pragma_Inline_Always                =>  0,
      Pragma_Inline_Generic               =>  0,
      Pragma_Inspection_Point             => -1,
      Pragma_Interface                    => +2,
      Pragma_Interface_Name               => +2,
      Pragma_Keep_Names                   =>  0,
      Pragma_Link_With                    => -1,
      Pragma_Linker_Alias                 => -1,
      Pragma_Linker_Options               => -1,
      Pragma_Long_Float                   => -1,
      Pragma_Memory_Size                  => -1,
      Pragma_No_Return                    =>  0,
      Pragma_No_Run_Time                  => -1,
      Pragma_Normalize_Scalars            => -1,
      Pragma_Obsolescent                  =>  0,
      Pragma_Optimize                     => -1,
      Pragma_Optional_Overriding          => -1,
      Pragma_Overriding                   => -1,
      Pragma_Pack                         =>  0,
      Pragma_Polling                      => -1,
      Pragma_Persistent_Data              => -1,
      Pragma_Persistent_Object            => -1,
      Pragma_Pure                         =>  0,
      Pragma_Pure_Function                =>  0,
      Pragma_Ravenscar                    => -1,
      Pragma_Restricted_Run_Time          => -1,
      Pragma_Restriction_Warnings         => -1,
      Pragma_Restrictions                 => -1,
      Pragma_Source_Reference             => -1,
      Pragma_Storage_Size                 => -1,
      Pragma_Storage_Unit                 => -1,
      Pragma_Style_Checks                 => -1,
      Pragma_Suppress                     =>  0,
      Pragma_Suppress_Exception_Locations =>  0,
      Pragma_Suppress_All                 => -1,
      Pragma_Suppress_Debug_Info          =>  0,
      Pragma_Suppress_Initialization      =>  0,
      Pragma_System_Name                  => -1,
      Pragma_Unchecked_Union              => -1,
      Pragma_Unimplemented_Unit           => -1,
      Pragma_Unreferenced                 => -1,
      Pragma_Unsuppress                   =>  0,
      Pragma_Validity_Checks              => -1,
      Pragma_Volatile                     =>  0,
      Pragma_Volatile_Components          =>  0,
      Pragma_Warnings                     => -1,
      Unknown_Pragma                      =>  0,
     others => 0);

   function Is_Non_Significant_Pragma_Reference (N : Node_Id) return Boolean is
      P : Node_Id;
      C : Int;
      A : Node_Id;

   begin
      P := Parent (N);

      if Nkind (P) /= N_Pragma_Argument_Association then
         return False;

      else
         C := Sig_Flags (Get_Pragma_Id (Chars (Parent (P))));

         case C is
            when -1 =>
               return False;

            when 0 =>
               return True;

            when others =>
               A := First (Pragma_Argument_Associations (Parent (P)));
               for J in 1 .. C - 1 loop
                  if No (A) then
                     return False;
                  end if;

                  Next (A);
               end loop;

               return A = P;
         end case;
      end if;
   end Is_Non_Significant_Pragma_Reference;

   ------------------------------
   -- Is_Pragma_String_Literal --
   ------------------------------

   --  This function returns true if the corresponding pragma argument is
   --  a static string expression. These are the only cases in which string
   --  literals can appear as pragma arguments. We also allow a string
   --  literal as the first argument to pragma Assert (although it will
   --  of course always generate a type error).

   function Is_Pragma_String_Literal (Par : Node_Id) return Boolean is
      Pragn : constant Node_Id := Parent (Par);
      Assoc : constant List_Id := Pragma_Argument_Associations (Pragn);
      Pname : constant Name_Id := Chars (Pragn);
      Argn  : Natural;
      N     : Node_Id;

   begin
      Argn := 1;
      N := First (Assoc);
      loop
         exit when N = Par;
         Argn := Argn + 1;
         Next (N);
      end loop;

      if Pname = Name_Assert then
         return True;

      elsif Pname = Name_Export then
         return Argn > 2;

      elsif Pname = Name_Ident then
         return Argn = 1;

      elsif Pname = Name_Import then
         return Argn > 2;

      elsif Pname = Name_Interface_Name then
         return Argn > 1;

      elsif Pname = Name_Linker_Alias then
         return Argn = 2;

      elsif Pname = Name_Source_Reference then
         return Argn = 2;

      else
         return False;
      end if;
   end Is_Pragma_String_Literal;

   --------------------------------------
   -- Process_Compilation_Unit_Pragmas --
   --------------------------------------

   procedure Process_Compilation_Unit_Pragmas (N : Node_Id) is
   begin
      --  A special check for pragma Suppress_All. This is a strange DEC
      --  pragma, strange because it comes at the end of the unit. If we
      --  have a pragma Suppress_All in the Pragmas_After of the current
      --  unit, then we insert a pragma Suppress (All_Checks) at the start
      --  of the context clause to ensure the correct processing.

      declare
         PA : constant List_Id := Pragmas_After (Aux_Decls_Node (N));
         P  : Node_Id;

      begin
         if Present (PA) then
            P := First (PA);
            while Present (P) loop
               if Chars (P) = Name_Suppress_All then
                  Prepend_To (Context_Items (N),
                    Make_Pragma (Sloc (P),
                      Chars => Name_Suppress,
                      Pragma_Argument_Associations => New_List (
                        Make_Pragma_Argument_Association (Sloc (P),
                          Expression =>
                            Make_Identifier (Sloc (P),
                              Chars => Name_All_Checks)))));
                  exit;
               end if;

               Next (P);
            end loop;
         end if;
      end;
   end Process_Compilation_Unit_Pragmas;

   --------------------------------
   -- Set_Encoded_Interface_Name --
   --------------------------------

   procedure Set_Encoded_Interface_Name (E : Entity_Id; S : Node_Id) is
      Str : constant String_Id := Strval (S);
      Len : constant Int       := String_Length (Str);
      CC  : Char_Code;
      C   : Character;
      J   : Int;

      Hex : constant array (0 .. 15) of Character := "0123456789abcdef";

      procedure Encode;
      --  Stores encoded value of character code CC. The encoding we
      --  use an underscore followed by four lower case hex digits.

      procedure Encode is
      begin
         Store_String_Char (Get_Char_Code ('_'));
         Store_String_Char
           (Get_Char_Code (Hex (Integer (CC / 2 ** 12))));
         Store_String_Char
           (Get_Char_Code (Hex (Integer (CC / 2 ** 8 and 16#0F#))));
         Store_String_Char
           (Get_Char_Code (Hex (Integer (CC / 2 ** 4 and 16#0F#))));
         Store_String_Char
           (Get_Char_Code (Hex (Integer (CC and 16#0F#))));
      end Encode;

   --  Start of processing for Set_Encoded_Interface_Name

   begin
      --  If first character is asterisk, this is a link name, and we
      --  leave it completely unmodified. We also ignore null strings
      --  (the latter case happens only in error cases) and no encoding
      --  should occur for Java interface names.

      if Len = 0
        or else Get_String_Char (Str, 1) = Get_Char_Code ('*')
        or else Java_VM
      then
         Set_Interface_Name (E, S);

      else
         J := 1;
         loop
            CC := Get_String_Char (Str, J);

            exit when not In_Character_Range (CC);

            C := Get_Character (CC);

            exit when C /= '_' and then C /= '$'
              and then C not in '0' .. '9'
              and then C not in 'a' .. 'z'
              and then C not in 'A' .. 'Z';

            if J = Len then
               Set_Interface_Name (E, S);
               return;

            else
               J := J + 1;
            end if;
         end loop;

         --  Here we need to encode. The encoding we use as follows:
         --     three underscores  + four hex digits (lower case)

         Start_String;

         for J in 1 .. String_Length (Str) loop
            CC := Get_String_Char (Str, J);

            if not In_Character_Range (CC) then
               Encode;
            else
               C := Get_Character (CC);

               if C = '_' or else C = '$'
                 or else C in '0' .. '9'
                 or else C in 'a' .. 'z'
                 or else C in 'A' .. 'Z'
               then
                  Store_String_Char (CC);
               else
                  Encode;
               end if;
            end if;
         end loop;

         Set_Interface_Name (E,
           Make_String_Literal (Sloc (S),
             Strval => End_String));
      end if;
   end Set_Encoded_Interface_Name;

   -------------------
   -- Set_Unit_Name --
   -------------------

   procedure Set_Unit_Name (N : Node_Id; With_Item : Node_Id) is
      Pref : Node_Id;
      Scop : Entity_Id;

   begin
      if Nkind (N) = N_Identifier
        and then Nkind (With_Item) = N_Identifier
      then
         Set_Entity (N, Entity (With_Item));

      elsif Nkind (N) = N_Selected_Component then
         Change_Selected_Component_To_Expanded_Name (N);
         Set_Entity (N, Entity (With_Item));
         Set_Entity (Selector_Name (N), Entity (N));

         Pref := Prefix (N);
         Scop := Scope (Entity (N));

         while Nkind (Pref) = N_Selected_Component loop
            Change_Selected_Component_To_Expanded_Name (Pref);
            Set_Entity (Selector_Name (Pref), Scop);
            Set_Entity (Pref, Scop);
            Pref := Prefix (Pref);
            Scop := Scope (Scop);
         end loop;

         Set_Entity (Pref, Scop);
      end if;
   end Set_Unit_Name;

end Sem_Prag;
