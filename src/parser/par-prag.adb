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

--  Generally the parser checks the basic syntax of pragmas, but does not
--  do specialized syntax checks for individual pragmas, these are deferred
--  to semantic analysis time (see unit Sem_Prag). There are some pragmas
--  which require recognition and either partial or complete processing
--  during parsing, and this unit performs this required processing.

with Fname.UF; use Fname.UF;
with Osint;    use Osint;
with Stringt;  use Stringt;
--with Stylesw;  use Stylesw;
with Uintp;    use Uintp;
with Uname;    use Uname;

separate (Par)

function Prag (Pragma_Node : Node_Id; Semi : Source_Ptr) return Node_Id is
   Pragma_Name : constant Name_Id    := Chars (Pragma_Node);
   Pragma_Sloc : constant Source_Ptr := Sloc (Pragma_Node);
   Arg_Count   : Nat;
   Arg_Node    : Node_Id;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Arg1 return Node_Id;
   function Arg2 return Node_Id;
   function Arg3 return Node_Id;
   --  Obtain specified Pragma_Argument_Association. It is allowable to call
   --  the routine for the argument one past the last present argument, but
   --  that is the only case in which a non-present argument can be referenced.

   procedure Check_Arg_Count (Required : Int);
   --  Check argument count for pragma = Required.
   --  If not give error and raise Error_Resync.

   procedure Check_Arg_Is_String_Literal (Arg : Node_Id);
   --  Check the expression of the specified argument to make sure that it
   --  is a string literal. If not give error and raise Error_Resync.

   procedure Check_Arg_Is_On_Or_Off (Arg : Node_Id);
   --  Check the expression of the specified argument to make sure that it
   --  is an identifier which is either ON or OFF, and if not, then issue
   --  an error message and raise Error_Resync.

   procedure Check_No_Identifier (Arg : Node_Id);
   --  Checks that the given argument does not have an identifier. If
   --  an identifier is present, then an error message is issued, and
   --  Error_Resync is raised.

   procedure Check_Optional_Identifier (Arg : Node_Id; Id : Name_Id);
   --  Checks if the given argument has an identifier, and if so, requires
   --  it to match the given identifier name. If there is a non-matching
   --  identifier, then an error message is given and Error_Resync raised.

   procedure Check_Required_Identifier (Arg : Node_Id; Id : Name_Id);
   --  Same as Check_Optional_Identifier, except that the name is required
   --  to be present and to match the given Id value.

   ----------
   -- Arg1 --
   ----------

   function Arg1 return Node_Id is
   begin
      return First (Pragma_Argument_Associations (Pragma_Node));
   end Arg1;

   ----------
   -- Arg2 --
   ----------

   function Arg2 return Node_Id is
   begin
      return Next (Arg1);
   end Arg2;

   ----------
   -- Arg3 --
   ----------

   function Arg3 return Node_Id is
   begin
      return Next (Arg2);
   end Arg3;

   ---------------------
   -- Check_Arg_Count --
   ---------------------

   procedure Check_Arg_Count (Required : Int) is
   begin
      if Arg_Count /= Required then
         Error_Msg ("wrong number of arguments for pragma%", Pragma_Sloc);
         raise Error_Resync;
      end if;
   end Check_Arg_Count;

   ----------------------------
   -- Check_Arg_Is_On_Or_Off --
   ----------------------------

   procedure Check_Arg_Is_On_Or_Off (Arg : Node_Id) is
      Argx : constant Node_Id := Expression (Arg);

   begin
      if Nkind (Expression (Arg)) /= N_Identifier
        or else (Chars (Argx) /= Name_On
                   and then
                 Chars (Argx) /= Name_Off)
      then
         Error_Msg_Name_2 := Name_On;
         Error_Msg_Name_3 := Name_Off;

         Error_Msg
           ("argument for pragma% must be% or%", Sloc (Argx));
         raise Error_Resync;
      end if;
   end Check_Arg_Is_On_Or_Off;

   ---------------------------------
   -- Check_Arg_Is_String_Literal --
   ---------------------------------

   procedure Check_Arg_Is_String_Literal (Arg : Node_Id) is
   begin
      if Nkind (Expression (Arg)) /= N_String_Literal then
         Error_Msg
           ("argument for pragma% must be string literal",
             Sloc (Expression (Arg)));
         raise Error_Resync;
      end if;
   end Check_Arg_Is_String_Literal;

   -------------------------
   -- Check_No_Identifier --
   -------------------------

   procedure Check_No_Identifier (Arg : Node_Id) is
   begin
      if Chars (Arg) /= No_Name then
         Error_Msg_N ("pragma% does not permit named arguments", Arg);
         raise Error_Resync;
      end if;
   end Check_No_Identifier;

   -------------------------------
   -- Check_Optional_Identifier --
   -------------------------------

   procedure Check_Optional_Identifier (Arg : Node_Id; Id : Name_Id) is
   begin
      if Present (Arg) and then Chars (Arg) /= No_Name then
         if Chars (Arg) /= Id then
            Error_Msg_Name_2 := Id;
            Error_Msg_N ("pragma% argument expects identifier%", Arg);
         end if;
      end if;
   end Check_Optional_Identifier;

   -------------------------------
   -- Check_Required_Identifier --
   -------------------------------

   procedure Check_Required_Identifier (Arg : Node_Id; Id : Name_Id) is
   begin
      if Chars (Arg) /= Id then
         Error_Msg_Name_2 := Id;
         Error_Msg_N ("pragma% argument must have identifier%", Arg);
      end if;
   end Check_Required_Identifier;

   ----------
   -- Prag --
   ----------

begin
   Error_Msg_Name_1 := Pragma_Name;

   --  Ignore unrecognized pragma. We let Sem post the warning for this, since
   --  it is a semantic error, not a syntactic one (we have already checked
   --  the syntax for the unrecognized pragma as required by (RM 2.8(11)).

   if not Is_Pragma_Name (Chars (Pragma_Node)) then
      return Pragma_Node;
   end if;

   --  Count number of arguments. This loop also checks if any of the arguments
   --  are Error, indicating a syntax error as they were parsed. If so, we
   --  simply return, because we get into trouble with cascaded errors if we
   --  try to perform our error checks on junk arguments.

   Arg_Count := 0;

   if Present (Pragma_Argument_Associations (Pragma_Node)) then
      Arg_Node := Arg1;

      while Arg_Node /= Empty loop
         Arg_Count := Arg_Count + 1;

         if Expression (Arg_Node) = Error then
            return Error;
         end if;

         Next (Arg_Node);
      end loop;
   end if;

   --  Remaining processing is pragma dependent

   case Get_Pragma_Id (Pragma_Name) is

      -----------
      -- Debug --
      -----------

      --  pragma Debug (PROCEDURE_CALL_STATEMENT);

      --  This has to be processed by the parser because of the very peculiar
      --  form of the second parameter, which is syntactically from a formal
      --  point of view a function call (since it must be an expression), but
      --  semantically we treat it as a procedure call (which has exactly the
      --  same syntactic form, so that's why we can get away with this!)

      when Pragma_Debug =>
         Check_Arg_Count (1);
         Check_No_Identifier (Arg1);

         declare
            Expr : constant Node_Id := New_Copy (Expression (Arg1));

         begin
            if Nkind (Expr) /= N_Indexed_Component
              and then Nkind (Expr) /= N_Function_Call
              and then Nkind (Expr) /= N_Identifier
              and then Nkind (Expr) /= N_Selected_Component
            then
               Error_Msg
                 ("argument of pragma% is not procedure call", Sloc (Expr));
               raise Error_Resync;
            else
               Set_Debug_Statement
                 (Pragma_Node, P_Statement_Name (Expr));
            end if;
         end;

      -------------------------------
      -- Extensions_Allowed (GNAT) --
      -------------------------------

      --  pragma Extensions_Allowed (Off | On)

      --  The processing for pragma Extensions_Allowed must be done at
      --  parse time, since extensions mode may affect what is accepted.

      when Pragma_Extensions_Allowed =>
         Check_Arg_Count (1);
         Check_No_Identifier (Arg1);
         Check_Arg_Is_On_Or_Off (Arg1);
         Opt.Extensions_Allowed := (Chars (Expression (Arg1)) = Name_On);

      -----------------------------
      -- Source_Reference (GNAT) --
      -----------------------------

      --  pragma Source_Reference
      --    (INTEGER_LITERAL [, STRING_LITERAL] );

      --  Processing for this pragma must be done at parse time, since error
      --  messages needing the proper line numbers can be generated in parse
      --  only mode with semantic checking turned off, and indeed we usually
      --  turn off semantic checking anyway if any parse errors are found.

      when Pragma_Source_Reference => Source_Reference : declare
         Fname : Name_Id;

      begin
         if Arg_Count /= 1 then
            Check_Arg_Count (2);
            Check_No_Identifier (Arg2);
         end if;

         --  Check that this is first line of file. We skip this test if
         --  we are in syntax check only mode, since we may be dealing with
         --  multiple compilation units.

         if Get_Physical_Line_Number (Pragma_Sloc) /= 1
           and then Num_SRef_Pragmas (Current_Source_File) = 0
           and then Operating_Mode /= Check_Syntax
         then
            Error_Msg
              ("first % pragma must be first line of file", Pragma_Sloc);
            raise Error_Resync;
         end if;

         Check_No_Identifier (Arg1);

         if Arg_Count = 1 then
            if Num_SRef_Pragmas (Current_Source_File) = 0 then
               Error_Msg
                 ("file name required for first % pragma in file",
                  Pragma_Sloc);
               raise Error_Resync;

            else
               Fname := No_Name;
            end if;

         --  File name present

         else
            Check_Arg_Is_String_Literal (Arg2);
            String_To_Name_Buffer (Strval (Expression (Arg2)));
            Fname := Name_Find;

            if Num_SRef_Pragmas (Current_Source_File) > 0 then
               if Fname /= Full_Ref_Name (Current_Source_File) then
                  Error_Msg
                    ("file name must be same in all % pragmas", Pragma_Sloc);
                  raise Error_Resync;
               end if;
            end if;
         end if;

         if Nkind (Expression (Arg1)) /= N_Integer_Literal then
            Error_Msg
              ("argument for pragma% must be integer literal",
                Sloc (Expression (Arg1)));
            raise Error_Resync;

         --  OK, this source reference pragma is effective, however, we
         --  ignore it if it is not in the first unit in the multiple unit
         --  case. This is because the only purpose in this case is to
         --  provide source pragmas for subsequent use by gnatchop.

         else
            if Num_Library_Units = 1 then
               Register_Source_Ref_Pragma
                 (Fname,
                  Strip_Directory (Fname),
                  UI_To_Int (Intval (Expression (Arg1))),
                  Get_Physical_Line_Number (Pragma_Sloc) + 1);
            end if;
         end if;
      end Source_Reference;

      -------------------------
      -- Style_Checks (GNAT) --
      -------------------------

      --  pragma Style_Checks (On | Off | ALL_CHECKS | STRING_LITERAL);

      --  This is processed by the parser since some of the style
      --  checks take place during source scanning and parsing.

      when Pragma_Style_Checks => null;
      ---------------------
      -- Warnings (GNAT) --
      ---------------------

      --  pragma Warnings (On | Off, [LOCAL_NAME])

      --  The one argument case is processed by the parser, since it may
      --  control parser warnings as well as semantic warnings, and in any
      --  case we want to be absolutely sure that the range in the warnings
      --  table is set well before any semantic analysis is performed.

      when Pragma_Warnings =>
         if Arg_Count = 1 then
            Check_No_Identifier (Arg1);
            Check_Arg_Is_On_Or_Off (Arg1);

            if Chars (Expression (Arg1)) = Name_On then
               Set_Warnings_Mode_On (Pragma_Sloc);
            else
               Set_Warnings_Mode_Off (Pragma_Sloc);
            end if;
         end if;

      -----------------------
      -- All Other Pragmas --
      -----------------------

      --  For all other pragmas, checking and processing is handled
      --  entirely in Sem_Prag, and no further checking is done by Par.

      when Pragma_Annotate                     |
           Pragma_Assert                       |
           Pragma_Compile_Time_Warning         |
           Pragma_Convention_Identifier        |
           Pragma_C_Pass_By_Copy               |
           Pragma_Comment                      |
           Pragma_Common_Object                |
           Pragma_Component_Alignment          |
           Pragma_Convention                   |
           Pragma_Discard_Names                |
           Pragma_Eliminate                    |
           Pragma_Elaborate                    |
           Pragma_Elaborate_All                |
           Pragma_Elaborate_Body               |
           Pragma_Elaboration_Checks           |
           Pragma_Explicit_Overriding          |
           Pragma_Export                       |
           Pragma_Export_Exception             |
           Pragma_Export_Function              |
           Pragma_Export_Object                |
           Pragma_Export_Procedure             |
           Pragma_External                     |
           Pragma_External_Name_Casing         |
           Pragma_Finalize_Storage_Only        |
           Pragma_Float_Representation         |
           Pragma_Ident                        |
           Pragma_Import                       |
           Pragma_Import_Function              |
           Pragma_Import_Object                |
           Pragma_Import_Procedure             |
           Pragma_Initialize_Scalars           |
           Pragma_Inline                       |
           Pragma_Inline_Always                |
           Pragma_Inline_Generic               |
           Pragma_Inspection_Point             |
           Pragma_Interface                    |
           Pragma_Interface_Name               |
           Pragma_Keep_Names                   |
           Pragma_Link_With                    |
           Pragma_Linker_Alias                 |
           Pragma_Linker_Options               |
           Pragma_Long_Float                   |
           Pragma_Memory_Size                  |
           Pragma_No_Return                    |
           Pragma_Obsolescent                  |
           Pragma_No_Run_Time                  |
           Pragma_Normalize_Scalars            |
           Pragma_Optimize                     |
           Pragma_Optional_Overriding          |
           Pragma_Overriding                   |
           Pragma_Pack                         |
           Pragma_Polling                      |
           Pragma_Persistent_Data              |
           Pragma_Persistent_Object            |
           Pragma_Pure                         |
           Pragma_Pure_Function                |
           Pragma_Restrictions                 |
           Pragma_Restriction_Warnings         |
           Pragma_Restricted_Run_Time          |
           Pragma_Ravenscar                    |
           Pragma_Storage_Size                 |
           Pragma_Storage_Unit                 |
           Pragma_Suppress                     |
           Pragma_Suppress_All                 |
           Pragma_Suppress_Debug_Info          |
           Pragma_Suppress_Exception_Locations |
           Pragma_Suppress_Initialization      |
           Pragma_System_Name                  |
           Pragma_Unchecked_Union              |
           Pragma_Unimplemented_Unit           |
           Pragma_Unreferenced                 |
           Pragma_Unsuppress                   |
           Pragma_Volatile                     |
           Pragma_Volatile_Components          |
           Pragma_Validity_Checks              =>
         null;

      --------------------
      -- Unknown_Pragma --
      --------------------

      --  Should be impossible, since we excluded this case earlier on

      when Unknown_Pragma =>
         raise Program_Error;

   end case;

   return Pragma_Node;

   --------------------
   -- Error Handling --
   --------------------

exception
   when Error_Resync =>
      return Error;

end Prag;
