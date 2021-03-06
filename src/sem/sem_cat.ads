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

--  This unit contains the routines used for checking for conformance with
--  the semantic restrictions required for the categorization pragmas:
--
--    Preelaborate
--    Pure,
--    Remote_Call_Interface
--    Remote_Types
--    Shared_Passive
--
--  Note that we treat Preelaborate as a categorization pragma, even though
--  strictly, according to RM E.2(2,3), the term does not apply in this case.

with Types; use Types;

package Sem_Cat is

   function In_Preelaborated_Unit return Boolean;
   --  Determines if the current scope is within a preelaborated compilation
   --  unit, that is one to which one of the pragmas Preelaborate, Pure,
   --  Shared_Passive, Remote_Types, or inside a unit other than a package
   --  body with pragma Remote_Call_Interface.

   function In_Pure_Unit return Boolean;
   pragma Inline (In_Pure_Unit);
   --  Determines if the current scope is within pure compilation unit,
   --  that is, one to which the pragmas Pure is applied.

   function In_Subprogram_Task_Protected_Unit return Boolean;
   --  Determines if the current scope is within a subprogram, task
   --  or protected unit. Used to validate if the library unit is Pure
   --  (RM 10.2.1(16)).

--   procedure Set_Categorization_From_Pragmas (N : Node_Id);
   --  Since validation of categorization dependency is done during Analyze,
   --  categorization flags from following pragmas should be set before
   --  validation begin. N is the N_Compilation_Unit node.

   procedure Set_Categorization_From_Scope (E : Entity_Id; Scop : Entity_Id);
   --  Set categorization flags Pure, Remote_Call_Interface and Remote_Types
   --  on entity E according to those of Scop.

   procedure Validate_Access_Type_Declaration (T : Entity_Id; N : Node_Id);
   --  Validate all constraints against declaration of access types in
   --  categorized library units. Usually this is a violation in Pure unit,
   --  Shared_Passive unit. N is the declaration node.

   procedure Validate_Ancestor_Part (N : Node_Id);
   --  Checks that a type given as the ancestor in an extension aggregate
   --  satisfies the restriction of 10.2.1(9).

   procedure Validate_Categorization_Dependency (N : Node_Id; E : Entity_Id);
   --  There are restrictions on lib unit that semantically depends on other
   --  units (RM E.2(5), 10.2.1(11). This procedure checks the restrictions
   --  on categorizations. N is the current unit node, and E is the current
   --  library unit entity.

   procedure Validate_Controlled_Object (E : Entity_Id);
   --  Given an entity for a library level controlled object, check that it is
   --  not in a preelaborated unit (prohibited by RM 10.2.1(9)).

   procedure Validate_Null_Statement_Sequence (N : Node_Id);
   --  Given N, a package body node, check that a handled statement sequence
   --  in a preelaborable body contains no statements other than labels or
   --  null statements, as required by RM 10.2.1(6).

   procedure Validate_Object_Declaration (N : Node_Id);
   --  Given N, an object declaration node, validates all the constraints in
   --  a preelaborable library unit, including creation of task objects etc.
   --  Note that this is called when the corresponding object is frozen since
   --  the checks cannot be made before knowing if the object is imported.

   procedure Validate_Static_Object_Name (N : Node_Id);
   --  In the elaboration code of a preelaborated library unit, check
   --  that we do not have the evaluation of a primary that is a name of
   --  an object, unless the name is a static expression (RM 10.2.1(8)).
   --  Non-static constant and variable are the targets, generic parameters
   --  are not included because the generic declaration and body are
   --  preelaborable.

end Sem_Cat;
