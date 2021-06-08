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

--  This package defines the aspects that are recognized by GNAT in aspect
--  specifications. It also contains the subprograms for storing/retrieving
--  aspect specifications from the tree. The semantic processing for aspect
--  specifications is found in Sem_Ch13.Analyze_Aspect_Specifications.

------------------------
-- Adding New Aspects --
------------------------

--  In general, each aspect should have a corresponding pragma, so that the
--  newly developed functionality is available for Ada versions < Ada 2012.
--  When both are defined, it is convenient to first transform the aspect into
--  an equivalent pragma in Sem_Ch13.Analyze_Aspect_Specifications, and then
--  analyze the pragma in Sem_Prag.Analyze_Pragma.

--  To add a new aspect, you need to do the following

--    1. Create a name in snames.ads-tmpl

--    2. Create a value in type Aspect_Id in this unit

--    3. Add a value for the aspect in the global arrays defined in this unit

--    4. Add code for the aspect in Sem_Ch13.Analyze_Aspect_Specifications.
--       This may involve adding some nodes to the tree to perform additional
--       treatments later.

--    5. If the semantic analysis of expressions/names in the aspect should not
--       occur at the point the aspect is defined, add code in the adequate
--       semantic analysis procedure for the aspect. For example, this is the
--       case for aspects Pre and Post on subprograms, which are pre-analyzed
--       at the end of the declaration list to which the subprogram belongs,
--       and fully analyzed (possibly with expansion) during the semantic
--       analysis of subprogram bodies.

with Namet;   use Namet;
with Snames;  use Snames;
with Types;   use Types;

package Aspects is

   --  Type defining recognized aspects

   type Aspect_Id is
     (No_Aspect,                            -- Dummy entry for no aspect
      Aspect_Address,
      Aspect_Annotate,                      -- GNAT
      Aspect_Attach_Handler,
      Aspect_Contract_Cases,                -- GNAT
      Aspect_Convention,
      Aspect_Depends,                       -- GNAT
      Aspect_Dynamic_Predicate,
      Aspect_External_Name,
      Aspect_Global,                        -- GNAT
      Aspect_Initial_Condition,             -- GNAT
      Aspect_Initializes,                   -- GNAT
      Aspect_Input,
      Aspect_Invariant,                     -- GNAT
      Aspect_Obsolescent,                   -- GNAT
      Aspect_Output,
      Aspect_Post,
      Aspect_Postcondition,
      Aspect_Pre,
      Aspect_Precondition,
      Aspect_Predicate,                     -- GNAT
      Aspect_Predicate_Failure,
      Aspect_Static_Predicate,
      Aspect_Suppress,
      Aspect_Test_Case,                     -- GNAT
      Aspect_Type_Invariant,
      Aspect_Unimplemented,                 -- GNAT
      Aspect_Unsuppress,
      Aspect_Warnings,                      -- GNAT

      --  The following aspects correspond to library unit pragmas

      Aspect_Elaborate_Body,
      Aspect_Preelaborate,
      Aspect_Pure,
      Aspect_Pure_Function,

      --  Remaining aspects have a static boolean value that turns the aspect
      --  on or off. They all correspond to pragmas, but are only converted to
      --  the pragmas where the value is True. A value of False normally means
      --  that the aspect is ignored, except in the case of derived types where
      --  the aspect value is inherited from the parent, in which case, we do
      --  not allow False if we inherit a True value from the parent.

      Aspect_Discard_Names,
      Aspect_Suppress_Debug_Info,
      Aspect_Export,
      Aspect_Import,
      Aspect_Inline,
      Aspect_Inline_Always,                 -- GNAT
      Aspect_No_Return,
      Aspect_Preelaborable_Initialization,
      Aspect_Unmodified,                    -- GNAT
      Aspect_Unreferenced,                  -- GNAT
      Aspect_Unreferenced_Objects,          -- GNAT
      Aspect_Volatile);                     -- GNAT

   subtype Aspect_Id_Exclude_No_Aspect is
     Aspect_Id range Aspect_Id'Succ (No_Aspect) .. Aspect_Id'Last;
   --  Aspect_Id's excluding No_Aspect

   --  The following array indicates aspects that accept 'Class

   Class_Aspect_OK : constant array (Aspect_Id) of Boolean :=
     (Aspect_Input          => True,
      Aspect_Invariant      => True,
      Aspect_Output         => True,
      Aspect_Pre            => True,
      Aspect_Predicate      => True,
      Aspect_Post           => True,
      Aspect_Type_Invariant => True,
      others                => False);

   --  The following array identifies all implementation defined aspects

   Implementation_Defined_Aspect : constant array (Aspect_Id) of Boolean :=
     (Aspect_Annotate                   => True,
      Aspect_Contract_Cases             => True,
      Aspect_Depends                    => True,
      Aspect_Global                     => True,
      Aspect_Inline_Always              => True,
      Aspect_Invariant                  => True,
      Aspect_Predicate                  => True,
      Aspect_Pure_Function              => True,
      Aspect_Suppress_Debug_Info        => True,
      Aspect_Test_Case                  => True,
      Aspect_Unmodified                 => True,
      Aspect_Unreferenced               => True,
      Aspect_Unreferenced_Objects       => True,
      Aspect_Warnings                   => True,
      others                            => False);

   --  The following array indicates aspects for which multiple occurrences of
   --  the same aspect attached to the same declaration are allowed.

   No_Duplicates_Allowed : constant array (Aspect_Id) of Boolean :=
     (Aspect_Annotate  => False,
      Aspect_Test_Case => False,
      others           => True);

   --  The following subtype defines aspects corresponding to library unit
   --  pragmas, these can only validly appear as aspects for library units,
   --  and result in a corresponding pragma being inserted immediately after
   --  the occurrence of the aspect.

   subtype Library_Unit_Aspects is
     Aspect_Id range Aspect_Elaborate_Body .. Aspect_Pure_Function;

   --  The following subtype defines aspects accepting an optional static
   --  boolean parameter indicating if the aspect should be active or
   --  cancelling. If the parameter is missing the effective value is True,
   --  enabling the aspect. If the parameter is present it must be a static
   --  expression of type Standard.Boolean. If the value is True, then the
   --  aspect is enabled. If it is False, the aspect is disabled.

   subtype Boolean_Aspects is
     Aspect_Id range Aspect_Discard_Names .. Aspect_Id'Last;

   subtype Pre_Post_Aspects is
     Aspect_Id range Aspect_Post .. Aspect_Precondition;

   --  The following type is used for indicating allowed expression forms

   type Aspect_Expression is
     (Expression,             -- Required expression
      Name,                   -- Required name
      Optional_Expression,    -- Optional boolean expression
      Optional_Name);         -- Optional name

   --  The following array indicates what argument type is required

   Aspect_Argument : constant array (Aspect_Id) of Aspect_Expression :=
     (No_Aspect                         => Optional_Expression,
      Aspect_Address                    => Expression,
      Aspect_Annotate                   => Expression,
      Aspect_Attach_Handler             => Expression,
      Aspect_Contract_Cases             => Expression,
      Aspect_Convention                 => Name,
      Aspect_Depends                    => Expression,
      Aspect_Dynamic_Predicate          => Expression,
      Aspect_External_Name              => Expression,
      Aspect_Global                     => Expression,
      Aspect_Initial_Condition          => Expression,
      Aspect_Initializes                => Expression,
      Aspect_Input                      => Name,
      Aspect_Invariant                  => Expression,
      Aspect_Obsolescent                => Optional_Expression,
      Aspect_Output                     => Name,
      Aspect_Post                       => Expression,
      Aspect_Postcondition              => Expression,
      Aspect_Pre                        => Expression,
      Aspect_Precondition               => Expression,
      Aspect_Predicate                  => Expression,
      Aspect_Predicate_Failure          => Expression,
      Aspect_Static_Predicate           => Expression,
      Aspect_Suppress                   => Name,
      Aspect_Test_Case                  => Expression,
      Aspect_Type_Invariant             => Expression,
      Aspect_Unimplemented              => Optional_Expression,
      Aspect_Unsuppress                 => Name,
      Aspect_Warnings                   => Name,

      Boolean_Aspects                   => Optional_Expression,
      Library_Unit_Aspects              => Optional_Expression);

   -----------------------------------------
   -- Table Linking Names and Aspect_Id's --
   -----------------------------------------

   --  Table linking aspect names and id's

   Aspect_Names : constant array (Aspect_Id) of Name_Id :=
     (No_Aspect                           => No_Name,
      Aspect_Address                      => Name_Address,
      Aspect_Annotate                     => Name_Annotate,
      Aspect_Attach_Handler               => Name_Attach_Handler, --*
      Aspect_Contract_Cases               => Name_Contract_Cases, --*
      Aspect_Convention                   => Name_Convention,
      Aspect_Depends                      => Name_Depends,        --*
      Aspect_Discard_Names                => Name_Discard_Names,
      Aspect_Dynamic_Predicate            => Name_Dynamic_Predicate,
      Aspect_Elaborate_Body               => Name_Elaborate_Body,
      Aspect_Export                       => Name_Export,
      Aspect_External_Name                => Name_External_Name,
      Aspect_Global                       => Name_Global,        -- *
      Aspect_Import                       => Name_Import,
      Aspect_Inline                       => Name_Inline,
      Aspect_Inline_Always                => Name_Inline_Always,
      Aspect_Initial_Condition            => Name_Initial_Condition, --*
      Aspect_Initializes                  => Name_Initializes,       --*
      Aspect_Input                        => Name_Input,             -- *
      Aspect_Invariant                    => Name_Invariant,         --*
      Aspect_No_Return                    => Name_No_Return,
      Aspect_Obsolescent                  => Name_Obsolescent,
      Aspect_Output                       => Name_Output,            --*
      Aspect_Post                         => Name_Post,
      Aspect_Postcondition                => Name_Postcondition,     --*
      Aspect_Pre                          => Name_Pre,
      Aspect_Precondition                 => Name_Precondition,      --*
      Aspect_Predicate                    => Name_Predicate,         --*
      Aspect_Predicate_Failure            => Name_Predicate_Failure, --*
      Aspect_Preelaborable_Initialization => Name_Preelaborable_Initialization, --
      Aspect_Preelaborate                 => Name_Preelaborate, --*
      Aspect_Pure                         => Name_Pure,
      Aspect_Pure_Function                => Name_Pure_Function,
      Aspect_Static_Predicate             => Name_Static_Predicate,
      Aspect_Suppress                     => Name_Suppress,
      Aspect_Suppress_Debug_Info          => Name_Suppress_Debug_Info,
      Aspect_Test_Case                    => Name_Test_Case,            --*
      Aspect_Type_Invariant               => Name_Type_Invariant,
      Aspect_Unimplemented                => Name_Unimplemented,        --*
      Aspect_Unmodified                   => Name_Unmodified,           -- *
      Aspect_Unreferenced                 => Name_Unreferenced,
      Aspect_Unreferenced_Objects         => Name_Unreferenced_Objects, --*
      Aspect_Unsuppress                   => Name_Unsuppress,
      Aspect_Volatile                     => Name_Volatile,
      Aspect_Warnings                     => Name_Warnings);

   function Get_Aspect_Id (Name : Name_Id) return Aspect_Id;
   pragma Inline (Get_Aspect_Id);
   --  Given a name Nam, returns the corresponding aspect id value. If the name
   --  does not match any aspect, then No_Aspect is returned as the result.

   function Get_Aspect_Id (Aspect : Node_Id) return Aspect_Id;
   pragma Inline (Get_Aspect_Id);
   --  Given an aspect specification, return the corresponding aspect_id value.
   --  If the name does not match any aspect, return No_Aspect.

   ------------------------------------
   -- Delaying Evaluation of Aspects --
   ------------------------------------

   --  The RM requires that all language defined aspects taking an expression
   --  delay evaluation of the expression till the freeze point of the entity
   --  to which the aspect applies. This allows forward references, and is of
   --  use for example in connection with preconditions and postconditions
   --  where the requirement of making all references in contracts to local
   --  functions be backwards references would be onerous.

   --  For consistency, even attributes like Size are delayed, so we can do:

   --    type A is range 1 .. 10
   --      with Size => Not_Defined_Yet;
   --    ..
   --    Not_Defined_Yet : constant := 64;

   --  Resulting in A having a size of 64, which gets set when A is frozen.
   --  Furthermore, we can have a situation like

   --    type A is range 1 .. 10
   --      with Size => Not_Defined_Yet;
   --    ..
   --    type B is new A;
   --    ..
   --    Not_Defined_Yet : constant := 64;

   --  where the Size of A is considered to have been previously specified at
   --  the point of derivation, even though the actual value of the size is
   --  not known yet, and in this example B inherits the size value of 64.

   --  Our normal implementation model (prior to Ada 2012) was simply to copy
   --  inheritable attributes at the point of derivation. Then any subsequent
   --  representation items apply either to the parent type, not affecting the
   --  derived type, or to the derived type, not affecting the parent type.

   --  To deal with the delayed aspect case, we use two flags. The first is
   --  set on the parent type if it has delayed representation aspects. This
   --  flag Has_Delayed_Rep_Aspects indicates that if we derive from this type
   --  we have to worry about making sure we inherit any delayed aspects. The
   --  second flag is set on a derived type: May_Have_Inherited_Rep_Aspects
   --  is set if the parent type has Has_Delayed_Rep_Aspects set.

   --  When we freeze a derived type, if the May_Have_Inherited_Rep_Aspects
   --  flag is set, then we call Freeze.Inherit_Delayed_Rep_Aspects when
   --  the derived type is frozen, which deals with the necessary copying of
   --  information from the parent type, which must be frozen at that point
   --  (since freezing the derived type first freezes the parent type).

   --  SPARK 2014 aspects do not follow the general delay mechanism as they
   --  act as annotations and cannot modify the attributes of their related
   --  constructs. To handle forward references in such aspects, the compiler
   --  delays the analysis of their respective pragmas by collecting them in
   --  N_Contract nodes. The pragmas are then analyzed at the end of the
   --  declarative region containing the related construct. For details,
   --  see routines Analyze_xxx_In_Decl_Part.

   --  The following shows which aspects are delayed. There are three cases:

   type Delay_Type is
     (Always_Delay,
      --  This aspect is not a representation aspect that can be inherited and
      --  is always delayed, as required by the language definition.

      Never_Delay,
      --  There are two cases. There are language defined aspects like
      --  Convention where the "expression" is simply an uninterpreted
      --  identifier, and there is no issue of evaluating it and thus no
      --  issue of delaying the evaluation. The second case is implementation
      --  defined aspects where we have decided that we don't want to allow
      --  delays (and for our own aspects we can do what we like).

      Rep_Aspect);
      --  These are the cases of representation aspects that are in general
      --  delayed, and where there is a potential issue of derived types that
      --  inherit delayed representation values.

   --  Note: even if this table indicates that an aspect is delayed, we never
   --  delay Boolean aspects that have a missing expression (taken as True),
   --  or expressions for delayed rep items that consist of an integer literal
   --  (most cases of Size etc. in practice), since in these cases we know we
   --  can get the value of the expression without delay. Note that we still
   --  need to delay Boolean aspects that are specifically set to True:

   --     type R is array (0 .. 31) of Boolean
   --       with Pack => True;
   --     True : constant Boolean := False;

   --  This is nonsense, but we need to make it work and result in R not
   --  being packed, and if we have something like:

   --     type R is array (0 .. 31) of Boolean
   --       with Pack => True;
   --     RR : R;
   --     True : constant Boolean := False;

   --  This is illegal because the visibility of True changes after the freeze
   --  point, which is not allowed, and we need the delay mechanism to properly
   --  diagnose this error.

   Aspect_Delay : constant array (Aspect_Id) of Delay_Type :=
     (No_Aspect                           => Always_Delay,
      Aspect_Address                      => Always_Delay,
      Aspect_Attach_Handler               => Always_Delay,
      Aspect_Discard_Names                => Always_Delay,
      Aspect_Dynamic_Predicate            => Always_Delay,
      Aspect_Elaborate_Body               => Always_Delay,
      Aspect_External_Name                => Always_Delay,
      Aspect_Inline                       => Always_Delay,
      Aspect_Inline_Always                => Always_Delay,
      Aspect_Input                        => Always_Delay,
      Aspect_Invariant                    => Always_Delay,
      Aspect_No_Return                    => Always_Delay,
      Aspect_Output                       => Always_Delay,
      Aspect_Post                         => Always_Delay,
      Aspect_Postcondition                => Always_Delay,
      Aspect_Pre                          => Always_Delay,
      Aspect_Precondition                 => Always_Delay,
      Aspect_Predicate                    => Always_Delay,
      Aspect_Predicate_Failure            => Always_Delay,
      Aspect_Preelaborable_Initialization => Always_Delay,
      Aspect_Preelaborate                 => Always_Delay,
      Aspect_Pure                         => Always_Delay,
      Aspect_Pure_Function                => Always_Delay,
      Aspect_Static_Predicate             => Always_Delay,
      Aspect_Suppress                     => Always_Delay,
      Aspect_Suppress_Debug_Info          => Always_Delay,
      Aspect_Type_Invariant               => Always_Delay,
      Aspect_Unmodified                   => Always_Delay,
      Aspect_Unreferenced                 => Always_Delay,
      Aspect_Unreferenced_Objects         => Always_Delay,
      Aspect_Unsuppress                   => Always_Delay,

      Aspect_Annotate                     => Never_Delay,
      Aspect_Contract_Cases               => Never_Delay,
      Aspect_Convention                   => Never_Delay,
      Aspect_Depends                      => Never_Delay,
      Aspect_Export                       => Never_Delay,
      Aspect_Global                       => Never_Delay,
      Aspect_Import                       => Never_Delay,
      Aspect_Initial_Condition            => Never_Delay,
      Aspect_Initializes                  => Never_Delay,
      Aspect_Obsolescent                  => Never_Delay,
      Aspect_Test_Case                    => Never_Delay,
      Aspect_Unimplemented                => Never_Delay,
      Aspect_Warnings                     => Never_Delay,

      Aspect_Volatile                     => Rep_Aspect);

   ------------------------------------------------
   -- Handling of Aspect Specifications on Stubs --
   ------------------------------------------------

   --  Aspects that appear on the following stub nodes

   --    N_Package_Body_Stub
   --    N_Protected_Body_Stub
   --    N_Subprogram_Body_Stub
   --    N_Task_Body_Stub

   --  are treated as if they apply to the corresponding proper body. Their
   --  analysis is postponed until the analysis of the proper body takes place
   --  (see Analyze_Proper_Body). The delay is required because the analysis
   --  may generate extra code which would be harder to relocate to the body.
   --  If the proper body is present, the aspect specifications are relocated
   --  to the corresponding body node:

   --    N_Package_Body
   --    N_Protected_Body
   --    N_Subprogram_Body
   --    N_Task_Body

   --  The subsequent analysis takes care of the aspect-to-pragma conversions
   --  and verification of pragma legality. In the case where the proper body
   --  is not available, the aspect specifications are analyzed on the spot
   --  (see Analyze_Proper_Body) to catch potential errors.

   --  The following table lists all aspects that can apply to a subprogram
   --  body [stub]. For instance, the following example is legal:

   --    package P with SPARK_Mode ...;
   --    package body P with SPARK_Mode is ...;

   --  The table should be synchronized with Pragma_On_Body_Or_Stub_OK in unit
   --  Sem_Prag.

   Aspect_On_Body_Or_Stub_OK : constant array (Aspect_Id) of Boolean :=
     (Aspect_Warnings                     => True,
      others                              => False);

   -------------------------------------------------------------------
   -- Handling of Aspects Specifications on Single Concurrent Types --
   -------------------------------------------------------------------

   --  Certain aspects that appear on the following nodes

   --    N_Single_Protected_Declaration
   --    N_Single_Task_Declaration

   --  are treated as if they apply to the anonymous object produced by the
   --  analysis of a single concurrent type. The following table lists all
   --  aspects that should apply to the anonymous object. The table should
   --  be synchronized with Pragma_On_Anonymous_Object_OK in unit Sem_Prag.

   Aspect_On_Anonymous_Object_OK : constant array (Aspect_Id) of Boolean :=
     (Aspect_Depends                      => True,
      Aspect_Global                       => True,
      others                              => False);

   ---------------------------------------------------
   -- Handling of Aspect Specifications in the Tree --
   ---------------------------------------------------

   --  Several kinds of declaration node permit aspect specifications in Ada
   --  2012 mode. If there was room in all the corresponding declaration nodes,
   --  we could just have a field Aspect_Specifications pointing to a list of
   --  nodes for the aspects (N_Aspect_Specification nodes). But there isn't
   --  room, so we adopt a different approach.

   --  The following subprograms provide access to a specialized interface
   --  implemented internally with a hash table in the body, that provides
   --  access to aspect specifications.

   function Aspect_Specifications (N : Node_Id) return List_Id;
   --  Given a node N, returns the list of N_Aspect_Specification nodes that
   --  are attached to this declaration node. If the node is in the class of
   --  declaration nodes that permit aspect specifications, as defined by the
   --  predicate above, and if their Has_Aspects flag is set to True, then this
   --  will always be a non-empty list. If this flag is set to False, then
   --  No_List is returned. Normally, the only nodes that have Has_Aspects set
   --  True are the nodes for which Permits_Aspect_Specifications would return
   --  True (i.e. the declaration nodes defined in the RM as permitting the
   --  presence of Aspect_Specifications). However, it is possible for the
   --  flag Has_Aspects to be set on other nodes as a result of Rewrite and
   --  Replace calls, and this function may be used to retrieve the aspect
   --  specifications for the original rewritten node in such cases.

   function Aspects_On_Body_Or_Stub_OK (N : Node_Id) return Boolean;
   --  N denotes a body [stub] with aspects. Determine whether all aspects of N
   --  are allowed to appear on a body [stub].

   procedure Exchange_Aspects (N1 : Node_Id; N2 : Node_Id);
   --  Exchange the aspect specifications of two nodes. If either node lacks an
   --  aspect specification list, the routine has no effect. It is assumed that
   --  both nodes can support aspects.

   function Find_Aspect (Id : Entity_Id; A : Aspect_Id) return Node_Id;
   --  Find the aspect specification of aspect A associated with entity I.
   --  Return Empty if Id does not have the requested aspect.

   function Find_Value_Of_Aspect
     (Id : Entity_Id;
      A  : Aspect_Id) return Node_Id;
   --  Find the value of aspect A associated with entity Id. Return Empty if
   --  Id does not have the requested aspect.

   function Has_Aspect (Id : Entity_Id; A : Aspect_Id) return Boolean;
   --  Determine whether entity Id has aspect A

   procedure Move_Aspects (From : Node_Id; To : Node_Id);
   --  Relocate the aspect specifications of node From to node To. On entry it
   --  is assumed that To does not have aspect specifications. If From has no
   --  aspects, the routine has no effect.

   procedure Move_Or_Merge_Aspects (From : Node_Id; To : Node_Id);
   --  Relocate the aspect specifications of node From to node To. If To has
   --  aspects, the aspects of From are appended to the aspects of To. If From
   --  has no aspects, the routine has no effect. Special behavior:
   --    * When node From denotes a subprogram body stub without a previous
   --      declaration, the only aspects relocated to node To are those found
   --      in table Aspect_On_Body_Or_Stub_OK.
   --    * When node From denotes a single synchronized type declaration, the
   --      only aspects relocated to node To are those found in table
   --      Aspect_On_Anonymous_Object_OK.

   function Permits_Aspect_Specifications (N : Node_Id) return Boolean;
   --  Returns True if the node N is a declaration node that permits aspect
   --  specifications in the grammar. It is possible for other nodes to have
   --  aspect specifications as a result of Rewrite or Replace calls.

   procedure Remove_Aspects (N : Node_Id);
   --  Delete the aspect specifications associated with node N. If the node has
   --  no aspects, the routine has no effect.

   function Same_Aspect (A1 : Aspect_Id; A2 : Aspect_Id) return Boolean;
   --  Returns True if A1 and A2 are (essentially) the same aspect. This is not
   --  a simple equality test because e.g. Post and Postcondition are the same.
   --  This is used for detecting duplicate aspects.

   procedure Set_Aspect_Specifications (N : Node_Id; L : List_Id);
   --  The node N must be in the class of declaration nodes that permit aspect
   --  specifications and the Has_Aspects flag must be False on entry. L must
   --  be a non-empty list of N_Aspect_Specification nodes. This procedure sets
   --  the Has_Aspects flag to True, and makes an entry that can be retrieved
   --  by a subsequent Aspect_Specifications call. It is an error to call this
   --  procedure with a node that does not permit aspect specifications, or a
   --  node that has its Has_Aspects flag set True on entry, or with L being an
   --  empty list or No_List.

   procedure Tree_Read;
   --  Reads contents of Aspect_Specifications hash table from the tree file

   procedure Tree_Write;
   --  Writes contents of Aspect_Specifications hash table to the tree file

end Aspects;
