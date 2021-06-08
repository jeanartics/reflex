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

with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Elists;   use Elists;
with Exp_Util; use Exp_Util;
with Fname;    use Fname;
with Fname.UF; use Fname.UF;
with Freeze;   use Freeze;
with Impunit;  use Impunit;
with Inline;   use Inline;
with Lib;      use Lib;
with Lib.Load; use Lib.Load;
with Lib.Xref; use Lib.Xref;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Restrict; use Restrict;
with Sem;      use Sem;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
-- with Sem_Dist; use Sem_Dist;
with Sem_Prag; use Sem_Prag;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Sinfo.CN; use Sinfo.CN;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Style;    use Style;
with Stylesw;  use Stylesw;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uname;    use Uname;
with Ada.Text_IO; use Ada.Text_IO;

package body Sem_Ch10 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Analyze_Context (N : Node_Id);
   --  Analyzes items in the context clause of compilation unit

   procedure Check_Body_Needed_For_SAL (Unit_Name : Entity_Id);
   --  Check whether the source for the body of a compilation unit must
   --  be included in a standalone library.

   procedure Expand_With_Clause (Nam : Node_Id; N : Node_Id);
   --  When a child unit appears in a context clause, the implicit withs on
   --  parents are made explicit, and with clauses are inserted in the context
   --  clause before the one for the child. If a parent in the with_clause
   --  is a renaming, the implicit with_clause is on the renaming whose name
   --  is mentioned in the with_clause, and not on the package it renames.
   --  N is the compilation unit whose list of context items receives the
   --  implicit with_clauses.

   function Get_Parent_Entity (Unit : Node_Id) return Entity_Id;
   --  Get defining entity of parent unit of a child unit. In most cases this
   --  is the defining entity of the unit, but for a child instance whose
   --  parent needs a body for inlining, the instantiation node of the parent
   --  has not yet been rewritten as a package declaration, and the entity has
   --  to be retrieved from the Instance_Spec of the unit.

   procedure Implicit_With_On_Parent (Child_Unit : Node_Id; N : Node_Id);
   --  If the main unit is a child unit, implicit withs are also added for
   --  all its ancestors.

   procedure Install_Context_Clauses (N : Node_Id);
   --  Subsidiary to previous one. Process only with_ and use_clauses for
   --  current unit and its library unit if any.

   procedure Install_Withed_Unit (With_Clause : Node_Id);
   --  If the unit is not a child unit, make unit immediately visible.
   --  The caller ensures that the unit is not already currently installed.

   procedure Install_Parents (Lib_Unit : Node_Id; Is_Private : Boolean);
   --  This procedure establishes the context for the compilation of a child
   --  unit. If Lib_Unit is a child library spec then the context of the parent
   --  is installed, and the parent itself made immediately visible, so that
   --  the child unit is processed in the declarative region of the parent.
   --  Install_Parents makes a recursive call to itself to ensure that all
   --  parents are loaded in the nested case. If Lib_Unit is a library body,
   --  the only effect of Install_Parents is to install the private decls of
   --  the parents, because the visible parent declarations will have been
   --  installed as part of the context of the corresponding spec.

   procedure Install_Siblings (U_Name : Entity_Id; N : Node_Id);
   --  In the compilation of a child unit, a child of any of the  ancestor
   --  units is directly visible if it is visible, because the parent is in
   --  an enclosing scope. Iterate over context to find child units of U_Name
   --  or of some ancestor of it.

   function Is_Child_Spec (Lib_Unit : Node_Id) return Boolean;
   --  Lib_Unit is a library unit which may be a spec or a body. Is_Child_Spec
   --  returns True if Lib_Unit is a library spec which is a child spec, i.e.
   --  a library spec that has a parent. If the call to Is_Child_Spec returns
   --  True, then Parent_Spec (Lib_Unit) is non-Empty and points to the
   --  compilation unit for the parent spec.
   --
   --  Lib_Unit can also be a subprogram body that acts as its own spec. If
   --  the Parent_Spec is  non-empty, this is also a child unit.

   procedure Remove_Context_Clauses (N : Node_Id);
   --  Subsidiary of previous one. Remove use_ and with_clauses.

   procedure Remove_Parents (Lib_Unit : Node_Id);
   --  Remove_Parents checks if Lib_Unit is a child spec. If so then the parent
   --  contexts established by the corresponding call to Install_Parents are
   --  removed. Remove_Parents contains a recursive call to itself to ensure
   --  that all parents are removed in the nested case.

   procedure Remove_Unit_From_Visibility (Unit_Name : Entity_Id);
   --  Reset all visibility flags on unit after compiling it, either as a
   --  main unit or as a unit in the context.

   procedure Unchain (E : Entity_Id);
   --  Remove single entity from visibility list

   --------------------------
   -- Limited_With_Clauses --
   --------------------------

   --  Limited_With clauses are the mechanism chosen for Ada05 to support
   --  mutually recursive types declared in different units. A limited_with
   --  clause that names package P in the context of unit U makes the types
   --  declared in the visible part of P available within U, but with the
   --  restriction that these types can only be used as incomplete types.
   --  The limited_with clause does not impose a semantic dependence on P,
   --  and it is possible for two packages to have limited_with_clauses on
   --  each other without creating an elaboration circularity.

   --  To support this feature, the analysis of a limited_with clause must
   --  create an abbreviated view of the package, without performing any
   --  semantic analysis on it. This "package abstract" contains shadow
   --  types that are in one-one correspondence with the real types in the
   --  package, and that have the properties of incomplete types.

   --  The implementation creates two element lists: one to chain the shadow
   --  entities, and one to chain the corresponding type entities in the tree
   --  of the package. Links between corresponding entities in both chains
   --  allow the compiler to select the proper view of a given type, depending
   --  on the context. Note that in contrast with the handling of private
   --  types, the limited view and the non-limited view of a type are treated
   --  as separate entities, and no entity exchange needs to take place, which
   --  makes the implementation must simpler than could be feared.

   ------------------------------
   -- Analyze_Compilation_Unit --
   ------------------------------

   procedure Analyze_Compilation_Unit (N : Node_Id) is
      Unit_Node     : constant Node_Id := Unit (N);
      Lib_Unit      : Node_Id          := Library_Unit (N);
      Spec_Id       : Node_Id;
      Main_Cunit    : constant Node_Id := Cunit (Main_Unit);
      Par_Spec_Name : Unit_Name_Type;
      Unum          : Unit_Number_Type;

      procedure Generate_Parent_References (N : Node_Id; P_Id : Entity_Id);
      --  Generate cross-reference information for the parents of child units.
      --  N is a defining_program_unit_name, and P_Id is the immediate parent.

      --------------------------------
      -- Generate_Parent_References --
      --------------------------------

      procedure Generate_Parent_References (N : Node_Id; P_Id : Entity_Id) is
         Pref   : Node_Id;
         P_Name : Entity_Id := P_Id;

      begin
         Pref   := Name (Parent (Defining_Entity (N)));

         if Nkind (Pref) = N_Expanded_Name then

            --  Done already, if the unit has been compiled indirectly as
            --  part of the closure of its context because of inlining.

            return;
         end if;

         while Nkind (Pref) = N_Selected_Component loop
            Change_Selected_Component_To_Expanded_Name (Pref);
            Set_Entity (Pref, P_Name);
            Set_Etype (Pref, Etype (P_Name));
            Generate_Reference (P_Name, Pref, 'r');
            Pref   := Prefix (Pref);
            P_Name := Scope (P_Name);
         end loop;

         --  The guard here on P_Name is to handle the error condition where
         --  the parent unit is missing because the file was not found.

         if Present (P_Name) then
            Set_Entity (Pref, P_Name);
            Set_Etype (Pref, Etype (P_Name));
            Generate_Reference (P_Name, Pref, 'r');
            Style.Check_Identifier (Pref, P_Name);
         end if;
      end Generate_Parent_References;

   --  Start of processing for Analyze_Compilation_Unit

   begin
      Process_Compilation_Unit_Pragmas (N);

      if Context_Pending (N) then
         declare
            Circularity : Boolean := True;

         begin
            if Is_Predefined_File_Name
                 (Unit_File_Name (Get_Source_Unit (Unit (N))))
            then
               Circularity := False;

            else
               for U in Main_Unit + 1 .. Last_Unit loop
                  if Nkind (Unit (Cunit (U))) = N_Package_Body
                    and then not Analyzed (Cunit (U))
                  then
                     Circularity := False;
                     exit;
                  end if;
               end loop;
            end if;

            if Circularity then
               Error_Msg_N ("circular dependency caused by with_clauses", N);
               Error_Msg_N
                 ("\possibly missing limited_with clause"
                  & " in one of the following", N);

               for U in Main_Unit .. Last_Unit loop
                  if Context_Pending (Cunit (U)) then
                     Error_Msg_Unit_1 := Get_Unit_Name (Unit (Cunit (U)));
                     Error_Msg_N ("\unit$", N);
                  end if;
               end loop;

               raise Unrecoverable_Error;
            end if;
         end;
      else
         Set_Context_Pending (N);
      end if;

      --  Analyze context (this will call Sem recursively for with'ed units)

      Analyze_Context (N);
      
      Set_Context_Pending (N, False);

      --  If the unit is a package body, the spec is already loaded and must
      --  be analyzed first, before we analyze the body.

      if Nkind (Unit_Node) = N_Package_Body then

         --  If no Lib_Unit, then there was a serious previous error, so
         --  just ignore the entire analysis effort

         if No (Lib_Unit) then
            return;

         else
            Semantics (Lib_Unit);
            Check_Unused_Withs (Get_Cunit_Unit_Number (Lib_Unit));

            --  Verify that the library unit is a package declaration.

            if Nkind (Unit (Lib_Unit)) /= N_Package_Declaration
                 and then
               Nkind (Unit (Lib_Unit)) /= N_Generic_Package_Declaration
            then
               Error_Msg_N
                 ("no legal package declaration for package body", N);
               return;

            --  Otherwise, the entity in the declaration is visible. Update
            --  the version to reflect dependence of this body on the spec.

            else
               Spec_Id := Defining_Entity (Unit (Lib_Unit));
               Set_Is_Immediately_Visible (Spec_Id, True);
               Version_Update (N, Lib_Unit);

               if Nkind (Defining_Unit_Name (Unit_Node))
                 = N_Defining_Program_Unit_Name
               then
                  Generate_Parent_References (Unit_Node, Scope (Spec_Id));
               end if;
            end if;
         end if;

      --  If the unit is a subprogram body, then we similarly need to analyze
      --  its spec. However, things are a little simpler in this case, because
      --  here, this analysis is done only for error checking and consistency
      --  purposes, so there's nothing else to be done.

      elsif Nkind (Unit_Node) = N_Subprogram_Body then
         if Acts_As_Spec (N) then

            --  If the subprogram body is a child unit, we must create a
            --  declaration for it, in order to properly load the parent(s).
            --  After this, the original unit does not acts as a spec, because
            --  there is an explicit one. If this  unit appears in a context
            --  clause, then an implicit with on the parent will be added when
            --  installing the context. If this is the main unit, there is no
            --  Unit_Table entry for the declaration, (It has the unit number
            --  of the main unit) and code generation is unaffected.

            Unum := Get_Cunit_Unit_Number (N);
            Par_Spec_Name := Get_Parent_Spec_Name (Unit_Name (Unum));

            if Par_Spec_Name /= No_Name then
               Unum :=
                 Load_Unit
                   (Load_Name  => Par_Spec_Name,
                    Required   => True,
                    Subunit    => False,
                    Error_Node => N);

               if Unum /= No_Unit then

                  --  Build subprogram declaration and attach parent unit to it
                  --  This subprogram declaration does not come from source!

                  declare
                     Loc : constant Source_Ptr := Sloc (N);
                     SCS : constant Boolean :=
                             Get_Comes_From_Source_Default;

                  begin
                     Set_Comes_From_Source_Default (False);
                     Lib_Unit :=
                       Make_Compilation_Unit (Loc,
                         Context_Items => New_Copy_List (Context_Items (N)),
                         Unit =>
                           Make_Subprogram_Declaration (Sloc (N),
                             Specification =>
                               Copy_Separate_Tree
                                 (Specification (Unit_Node))),
                         Aux_Decls_Node =>
                           Make_Compilation_Unit_Aux (Loc));

                     Set_Library_Unit (N, Lib_Unit);
                     Set_Parent_Spec (Unit (Lib_Unit), Cunit (Unum));
                     Semantics (Lib_Unit);
                     Set_Acts_As_Spec (N, False);
                     Set_Comes_From_Source_Default (SCS);
                  end;
               end if;
            end if;

         --  Here for subprogram with separate declaration

         else
            Semantics (Lib_Unit);
            Check_Unused_Withs (Get_Cunit_Unit_Number (Lib_Unit));
            Version_Update (N, Lib_Unit);
         end if;

         if Nkind (Defining_Unit_Name (Specification (Unit_Node))) =
                                             N_Defining_Program_Unit_Name
         then
            Generate_Parent_References (
              Specification (Unit_Node),
                Scope (Defining_Entity (Unit (Lib_Unit))));
         end if;
      end if;

      --  If it is a child unit, the parent must be elaborated first
      --  and we update version, since we are dependent on our parent.

      if Is_Child_Spec (Unit_Node) then

         --  The analysis of the parent is done with style checks off

         declare
            Save_Style_Check : constant Boolean := Style_Check;
            Save_C_Restrict  : constant Save_Compilation_Unit_Restrictions :=
                                 Compilation_Unit_Restrictions_Save;

         begin
            if not GNAT_Mode then
               Style_Check := False;
            end if;

            Semantics (Parent_Spec (Unit_Node));
            Version_Update (N, Parent_Spec (Unit_Node));
            Style_Check := Save_Style_Check;
            Compilation_Unit_Restrictions_Restore (Save_C_Restrict);
         end;
      end if;

      --  With the analysis done, install the context. Note that we can't
      --  install the context from the with clauses as we analyze them,
      --  because each with clause must be analyzed in a clean visibility
      --  context, so we have to wait and install them all at once.

      Install_Context (N);

      if Is_Child_Spec (Unit_Node) then

         --  Set the entities of all parents in the program_unit_name.

         Generate_Parent_References (
           Unit_Node, Get_Parent_Entity (Unit (Parent_Spec (Unit_Node))));
      end if;

      --  All components of the context: with-clauses, library unit, ancestors
      --  if any, (and their context)  are analyzed and installed. Now analyze
      --  the unit itself, which is either a package, subprogram spec or body.

      Analyze (Unit_Node);

      --  The above call might have made Unit_Node an N_Subprogram_Body
      --  from something else, so propagate any Acts_As_Spec flag.

      if Nkind (Unit_Node) = N_Subprogram_Body
        and then Acts_As_Spec (Unit_Node)
      then
         Set_Acts_As_Spec (N);
      end if;

      --  Treat compilation unit pragmas that appear after the library unit

      if Present (Pragmas_After (Aux_Decls_Node (N))) then
         declare
            Prag_Node : Node_Id := First (Pragmas_After (Aux_Decls_Node (N)));

         begin
            while Present (Prag_Node) loop
               Analyze (Prag_Node);
               Next (Prag_Node);
            end loop;
         end;
      end if;

      if Nkind (Unit_Node) = N_Package_Declaration
        or else Nkind (Unit_Node) in N_Generic_Declaration
        or else Nkind (Unit_Node) = N_Package_Renaming_Declaration
        or else Nkind (Unit_Node) = N_Subprogram_Declaration
      then
         Remove_Unit_From_Visibility (Defining_Entity (Unit_Node));

      --  If the unit is an instantiation whose body will be elaborated
      --  for inlining purposes, use the the proper entity of the instance.

      elsif Nkind (Unit_Node) = N_Package_Instantiation
        and then not Error_Posted (Unit_Node)
      then
         Remove_Unit_From_Visibility
           (Defining_Entity (Instance_Spec (Unit_Node)));

      elsif Nkind (Unit_Node) = N_Package_Body
        or else (Nkind (Unit_Node) = N_Subprogram_Body
                  and then not Acts_As_Spec (Unit_Node))
      then
         --  Bodies that are not the main unit are compiled if they
         --  are generic or contain generic or inlined units. Their
         --  analysis brings in the context of the corresponding spec
         --  (unit declaration) which must be removed as well, to
         --  return the compilation environment to its proper state.

         Remove_Context (Lib_Unit);
         Set_Is_Immediately_Visible (Defining_Entity (Unit (Lib_Unit)), False);
      end if;

      --  Last step is to deinstall the context we just installed
      --  as well as the unit just compiled.

      Remove_Context (N);

      --  If this is the main unit and we are generating code, we must
      --  check that all generic units in the context have a body if they
      --  need it, even if they have not been instantiated. In the absence
      --  of .ali files for generic units, we must force the load of the body,
      --  just to produce the proper error if the body is absent. We skip this
      --  verification if the main unit itself is generic.

      if Get_Cunit_Unit_Number (N) = Main_Unit
        and then Operating_Mode = Generate_Code
        and then Expander_Active
      then
         --  Check whether the source for the body of the unit must be
         --  included in a standalone library.

         Check_Body_Needed_For_SAL (Cunit_Entity (Main_Unit));

         --  Indicate that the main unit is now analyzed, to catch possible
         --  circularities between it and generic bodies. Remove main unit
         --  from visibility. This might seem superfluous, but the main unit
         --  must not be visible in the generic body expansions that follow.

         Set_Analyzed (N, True);
         Set_Is_Immediately_Visible (Cunit_Entity (Main_Unit), False);

         declare
            Item  : Node_Id;
            Nam   : Entity_Id;
            Un    : Unit_Number_Type;

            Save_Style_Check : constant Boolean := Style_Check;
            Save_C_Restrict  : constant Save_Compilation_Unit_Restrictions :=
                                 Compilation_Unit_Restrictions_Save;

         begin
            Item := First (Context_Items (N));
            while Present (Item) loop

               if Nkind (Item) = N_With_Clause
                  and then not Implicit_With (Item)
               then
                  Nam := Entity (Name (Item));

                  if (Is_Generic_Subprogram (Nam)
                       and then not Is_Intrinsic_Subprogram (Nam))
                    or else (Ekind (Nam) = E_Generic_Package
                              and then Unit_Requires_Body (Nam))
                  then
                     Style_Check := False;

                     if Present (Renamed_Object (Nam)) then
                        Un :=
                           Load_Unit
                             (Load_Name  => Get_Body_Name
                                              (Get_Unit_Name
                                                (Unit_Declaration_Node
                                                  (Renamed_Object (Nam)))),
                              Required   => False,
                              Subunit    => False,
                              Error_Node => N,
                              Renamings  => True);
                     else
                        Un :=
                          Load_Unit
                            (Load_Name  => Get_Body_Name
                                             (Get_Unit_Name (Item)),
                             Required   => False,
                             Subunit    => False,
                             Error_Node => N,
                             Renamings  => True);
                     end if;

                     if Un = No_Unit then
                        Error_Msg_NE
                          ("body of generic unit& not found", Item, Nam);
                        exit;

                     elsif not Analyzed (Cunit (Un))
                       and then Un /= Main_Unit
                       and then not Fatal_Error (Un)
                     then
                        Style_Check := False;
                        Semantics (Cunit (Un));
                     end if;
                  end if;
               end if;

               Next (Item);
            end loop;

            Style_Check := Save_Style_Check;
            Compilation_Unit_Restrictions_Restore (Save_C_Restrict);
         end;
      end if;

      --  Deal with creating elaboration Boolean if needed. We create an
      --  elaboration boolean only for units that come from source since
      --  units manufactured by the compiler never need elab checks.

      if Comes_From_Source (N)
        and then
          (Nkind (Unit (N)) =  N_Package_Declaration         or else
           Nkind (Unit (N)) =  N_Generic_Package_Declaration or else
           Nkind (Unit (N)) =  N_Subprogram_Declaration      or else
           Nkind (Unit (N)) =  N_Generic_Subprogram_Declaration)
      then
         declare
            Loc  : constant Source_Ptr := Sloc (N);
            Unum : constant Unit_Number_Type := Get_Source_Unit (Loc);
         begin
            Spec_Id := Defining_Entity (Unit (N));
            Generate_Definition (Spec_Id);

            --  See if an elaboration entity is required for possible
            --  access before elaboration checking. Note that we must
            --  allow for this even if -gnatE is not set, since a client
            --  may be compiled in -gnatE mode and reference the entity.

            --  Case of units which do not require elaboration checks

            if
               --  Pure units do not need checks

                 Is_Pure (Spec_Id)

               --  Preelaborated units do not need checks

                 or else Is_Preelaborated (Spec_Id)

               --  No checks needed if pagma Elaborate_Body present

                 or else Has_Pragma_Elaborate_Body (Spec_Id)

               --  No checks needed if unit does not require a body

                 or else not Unit_Requires_Body (Spec_Id)

               --  No checks needed for predefined files

                 or else Is_Predefined_File_Name (Unit_File_Name (Unum))

               --  No checks required if no separate spec

                 or else Acts_As_Spec (N)
            then
               --  This is a case where we only need the entity for
               --  checking to prevent multiple elaboration checks.

               Set_Elaboration_Entity_Required (Spec_Id, False);

            --  Case of elaboration entity is required for access before
            --  elaboration checking (so certainly we must build it!)

            else
               Set_Elaboration_Entity_Required (Spec_Id, True);
            end if;

            Build_Elaboration_Entity (N, Spec_Id);
         end;
      end if;

      --  Finally, freeze the compilation unit entity. This for sure is needed
      --  because of some warnings that can be output (see Freeze_Subprogram),
      --  but may in general be required. If freezing actions result, place
      --  them in the compilation unit actions list, and analyze them.

--        declare
--           Loc : constant Source_Ptr := Sloc (N);
--           L   : constant List_Id :=
--                   Freeze_Entity (Cunit_Entity (Current_Sem_Unit), Loc);
--  
--        begin
--           while Is_Non_Empty_List (L) loop
--              Insert_Library_Level_Action (Remove_Head (L));
--           end loop;
--        end;

      Set_Analyzed (N);

      if Nkind (Unit_Node) = N_Package_Declaration
        and then Get_Cunit_Unit_Number (N) /= Main_Unit
        and then (Expander_Active or else Generate_Plc_Code)
      then
         declare
            Save_Style_Check : constant Boolean := Style_Check;
            Save_Warning     : constant Warning_Mode_Type := Warning_Mode;
            Options : Style_Check_Options;

         begin
            Save_Style_Check_Options (Options);
            Reset_Style_Check_Options;
            Opt.Warning_Mode := Suppress;
	    
            
	    if not Generate_Plc_Code then
	       Check_Body_For_Inlining (N, Defining_Entity (Unit_Node));
	    else
	       declare
		  Bname : Unit_Name_Type;
                  OK    : Boolean;
	       begin
		  if not In_Predefined_Unit (N) then
		     Bname := Get_Body_Name (Get_Unit_Name (Unit (N)));
		     if not Is_Loaded (Bname) then
			Load_Needed_Body (N, OK);
		     end if;
		  end if;
	       end;
	    end if;

            Reset_Style_Check_Options;
            Set_Style_Check_Options (Options);
            Style_Check := Save_Style_Check;
            Warning_Mode := Save_Warning;
         end;
      end if;
   end Analyze_Compilation_Unit;

   ---------------------
   -- Analyze_Context --
   ---------------------

   procedure Analyze_Context (N : Node_Id) is
      Item  : Node_Id;

   begin
      --  Loop through context items. This is done is three passes:
      --  a) The first pass analyze non-limited with-clauses.
      --  b) The second pass add implicit limited_with clauses for
      --     the parents of child units (Ada0Y: AI-50217)
      --  c) The third pass analyzes limited_with clauses (Ada0Y: AI-50217)

      Item := First (Context_Items (N));
      while Present (Item) loop

         --  For with clause, analyze the with clause, and then update
         --  the version, since we are dependent on a unit that we with.

         if Nkind (Item) = N_With_Clause
         then

            --  Skip analyzing with clause if no unit, nothing to do (this
            --  happens for a with that references a non-existant unit)

            if Present (Library_Unit (Item)) then
               Analyze (Item);
            end if;

            if not Implicit_With (Item) then
               Version_Update (N, Library_Unit (Item));
            end if;

         --  But skip use clauses at this stage, since we don't want to do
         --  any installing of potentially use visible entities until we
         --  we actually install the complete context (in Install_Context).
         --  Otherwise things can get installed in the wrong context.
         --  Similarly, pragmas are analyzed in Install_Context, after all
         --  the implicit with's on parent units are generated.

         else
            null;
         end if;

         Next (Item);
      end loop;
   end Analyze_Context;

   -------------------------
   -- Analyze_With_Clause --
   -------------------------

   --  Analyze the declaration of a unit in a with clause. At end,
   --  label the with clause with the defining entity for the unit.

   procedure Analyze_With_Clause (N : Node_Id) is

      --  Retrieve the original kind of the unit node, before analysis.
      --  If it is a subprogram instantiation, its analysis below will
      --  rewrite as the declaration of the wrapper package. If the same
      --  instantiation appears indirectly elsewhere in the context, it
      --  will have been analyzed already.

      Unit_Kind : constant Node_Kind :=
                    Nkind (Original_Node (Unit (Library_Unit (N))));

      E_Name    : Entity_Id;
      Par_Name  : Entity_Id;
      Pref      : Node_Id;
      U         : Node_Id;

      Intunit : Boolean;
      --  Set True if the unit currently being compiled is an internal unit

      Save_Style_Check : constant Boolean := Opt.Style_Check;
      Save_C_Restrict  : constant Save_Compilation_Unit_Restrictions :=
                           Compilation_Unit_Restrictions_Save;

   begin
      Semantics (Library_Unit (N));

      U := Unit (Library_Unit (N));
      Intunit := Is_Internal_File_Name (Unit_File_Name (Current_Sem_Unit));

      --  Following checks are skipped for dummy packages (those supplied
      --  for with's where no matching file could be found). Such packages
      --  are identified by the Sloc value being set to No_Location

      if Sloc (U) /= No_Location then

         --  Check restrictions, except that we skip the check if this
         --  is an internal unit unless we are compiling the internal
         --  unit as the main unit. We also skip this for dummy packages.

         if not Intunit or else Current_Sem_Unit = Main_Unit then
            Check_Restricted_Unit (Unit_Name (Get_Source_Unit (U)), N);
         end if;

         --  Check for inappropriate with of internal implementation unit
         --  if we are currently compiling the main unit and the main unit
         --  is itself not an internal unit. We do not issue this message
         --  for implicit with's generated by the compiler itself.

         if Implementation_Unit_Warnings
           and then Current_Sem_Unit = Main_Unit
           and then Implementation_Unit (Get_Source_Unit (U))
           and then not Intunit
           and then not Implicit_With (N)
         then
            Error_Msg_N ("& is an internal 'G'N'A'T unit?", Name (N));
            Error_Msg_N
              ("\use of this unit is non-portable and version-dependent?",
               Name (N));
         end if;
      end if;

      --  Semantic analysis of a generic unit is performed on a copy of
      --  the original tree. Retrieve the entity on  which semantic info
      --  actually appears.

      if Unit_Kind in N_Generic_Declaration then
         E_Name := Defining_Entity (U);

      --  Note: in the following test, Unit_Kind is the original Nkind, but
      --  in the case of an instantiation, semantic analysis above will
      --  have replaced the unit by its instantiated version. If the instance
      --  body has been generated, the instance now denotes the body entity.
      --  For visibility purposes we need the entity of its spec.

      elsif (Unit_Kind = N_Package_Instantiation
              or else Nkind (Original_Node (Unit (Library_Unit (N)))) =
                N_Package_Instantiation)
        and then Nkind (U) = N_Package_Body
      then
         E_Name := Corresponding_Spec (U);

      elsif Unit_Kind = N_Package_Instantiation
        and then Nkind (U) = N_Package_Instantiation
      then
         --  If the instance has not been rewritten as a package declaration,
         --  then it appeared already in a previous with clause. Retrieve
         --  the entity from the previous instance.

         E_Name := Defining_Entity (Specification (Instance_Spec (U)));

      elsif Unit_Kind = N_Procedure_Instantiation
        or else Unit_Kind = N_Function_Instantiation
      then
         --  Instantiation node is replaced with a package that contains
         --  renaming declarations and instance itself. The subprogram
         --  Instance is declared in the visible part of the wrapper package.

         E_Name := First_Entity (Defining_Entity (U));

         while Present (E_Name) loop
            exit when Is_Subprogram (E_Name)
              and then Is_Generic_Instance (E_Name);
            E_Name := Next_Entity (E_Name);
         end loop;

      elsif Unit_Kind = N_Package_Renaming_Declaration
        or else Unit_Kind in N_Generic_Renaming_Declaration
      then
         E_Name := Defining_Entity (U);

      elsif Unit_Kind = N_Subprogram_Body
        and then Nkind (Name (N)) = N_Selected_Component
        and then not Acts_As_Spec (Library_Unit (N))
      then
         --  For a child unit that has no spec, one has been created and
         --  analyzed. The entity required is that of the spec.

         E_Name := Corresponding_Spec (U);

      else
         E_Name := Defining_Entity (U);
      end if;

      if Nkind (Name (N)) = N_Selected_Component then

         --  Child unit in a with clause

         Change_Selected_Component_To_Expanded_Name (Name (N));
      end if;

      --  Restore style checks and restrictions

      Style_Check := Save_Style_Check;
      Compilation_Unit_Restrictions_Restore (Save_C_Restrict);

      --  Record the reference, but do NOT set the unit as referenced, we
      --  want to consider the unit as unreferenced if this is the only
      --  reference that occurs.

      Set_Entity_With_Style_Check (Name (N), E_Name);
      Generate_Reference (E_Name, Name (N), 'w', Set_Ref => False);

      if Is_Child_Unit (E_Name) then
         Pref     := Prefix (Name (N));
         Par_Name := Scope (E_Name);

         while Nkind (Pref) = N_Selected_Component loop
            Change_Selected_Component_To_Expanded_Name (Pref);
            Set_Entity_With_Style_Check (Pref, Par_Name);

            Generate_Reference (Par_Name, Pref);
            Pref := Prefix (Pref);

            --  If E_Name is the dummy entity for a nonexistent unit,
            --  its scope is set to Standard_Standard, and no attempt
            --  should be made to further unwind scopes.

            if Par_Name /= Standard_Standard then
               Par_Name := Scope (Par_Name);
            end if;
         end loop;

         if Present (Entity (Pref))
           and then not Analyzed (Parent (Parent (Entity (Pref))))
         then
            --  If the entity is set without its unit being compiled,
            --  the original parent is a renaming, and Par_Name is the
            --  renamed entity. For visibility purposes, we need the
            --  original entity, which must be analyzed now, because
            --  Load_Unit retrieves directly the renamed unit, and the
            --  renaming declaration itself has not been analyzed.

            Analyze (Parent (Parent (Entity (Pref))));
            pragma Assert (Renamed_Object (Entity (Pref)) = Par_Name);
            Par_Name := Entity (Pref);
         end if;

         Set_Entity_With_Style_Check (Pref, Par_Name);
         Generate_Reference (Par_Name, Pref);
      end if;

      --  If the withed unit is System, and a system extension pragma is
      --  present, compile the extension now, rather than waiting for
      --  a visibility check on a specific entity.

      if Chars (E_Name) = Name_System
        and then Scope (E_Name) = Standard_Standard
        and then Present (System_Extend_Unit)
        and then Present_System_Aux (N)
      then
         --  If the extension is not present, an error will have been emitted.

         null;
      end if;
   end Analyze_With_Clause;

   ------------------------
   -- Expand_With_Clause --
   ------------------------

   procedure Expand_With_Clause (Nam : Node_Id; N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (Nam);
      Ent   : constant Entity_Id := Entity (Nam);
      Withn : Node_Id;
      P     : Node_Id;

      function Build_Unit_Name (Nam : Node_Id) return Node_Id;

      function Build_Unit_Name (Nam : Node_Id) return Node_Id is
         Result : Node_Id;

      begin
         if Nkind (Nam) = N_Identifier then
            return New_Occurrence_Of (Entity (Nam), Loc);

         else
            Result :=
              Make_Expanded_Name (Loc,
                Chars  => Chars (Entity (Nam)),
                Prefix => Build_Unit_Name (Prefix (Nam)),
                Selector_Name => New_Occurrence_Of (Entity (Nam), Loc));
            Set_Entity (Result, Entity (Nam));
            return Result;
         end if;
      end Build_Unit_Name;

   begin
      New_Nodes_OK := New_Nodes_OK + 1;
      Withn :=
        Make_With_Clause (Loc, Name => Build_Unit_Name (Nam));

      P := Parent (Unit_Declaration_Node (Ent));
      Set_Library_Unit          (Withn, P);
      Set_Corresponding_Spec    (Withn, Ent);
      Set_First_Name            (Withn, True);
      Set_Implicit_With         (Withn, True);

      Prepend (Withn, Context_Items (N));
      Mark_Rewrite_Insertion (Withn);
      Install_Withed_Unit (Withn);

      if Nkind (Nam) = N_Expanded_Name then
         Expand_With_Clause (Prefix (Nam), N);
      end if;

      New_Nodes_OK := New_Nodes_OK - 1;
   end Expand_With_Clause;

   -----------------------
   -- Get_Parent_Entity --
   -----------------------

   function Get_Parent_Entity (Unit : Node_Id) return Entity_Id is
   begin
      if Nkind (Unit) = N_Package_Instantiation then
         return Defining_Entity (Specification (Instance_Spec (Unit)));
      else
         return Defining_Entity (Unit);
      end if;
   end Get_Parent_Entity;

   -----------------------------
   -- Implicit_With_On_Parent --
   -----------------------------

   procedure Implicit_With_On_Parent
     (Child_Unit : Node_Id;
      N          : Node_Id)
   is
      Loc    : constant Source_Ptr := Sloc (N);
      P      : constant Node_Id    := Parent_Spec (Child_Unit);
      P_Unit : constant Node_Id    := Unit (P);
      P_Name : constant Entity_Id  := Get_Parent_Entity (P_Unit);
      Withn  : Node_Id;

      function Build_Ancestor_Name (P : Node_Id)  return Node_Id;
      --  Build prefix of child unit name. Recurse if needed.

      function Build_Unit_Name return Node_Id;
      --  If the unit is a child unit, build qualified name with all
      --  ancestors.

      -------------------------
      -- Build_Ancestor_Name --
      -------------------------

      function Build_Ancestor_Name (P : Node_Id) return Node_Id is
         P_Ref : constant Node_Id :=
                   New_Reference_To (Defining_Entity (P), Loc);

      begin
         if No (Parent_Spec (P)) then
            return P_Ref;
         else
            return
              Make_Selected_Component (Loc,
                Prefix => Build_Ancestor_Name (Unit (Parent_Spec (P))),
                Selector_Name => P_Ref);
         end if;
      end Build_Ancestor_Name;

      ---------------------
      -- Build_Unit_Name --
      ---------------------

      function Build_Unit_Name return Node_Id is
         Result : Node_Id;

      begin
         if No (Parent_Spec (P_Unit)) then
            return New_Reference_To (P_Name, Loc);
         else
            Result :=
              Make_Expanded_Name (Loc,
                Chars  => Chars (P_Name),
                Prefix => Build_Ancestor_Name (Unit (Parent_Spec (P_Unit))),
                Selector_Name => New_Reference_To (P_Name, Loc));
            Set_Entity (Result, P_Name);
            return Result;
         end if;
      end Build_Unit_Name;

   --  Start of processing for Implicit_With_On_Parent

   begin
      New_Nodes_OK := New_Nodes_OK + 1;
      Withn := Make_With_Clause (Loc, Name => Build_Unit_Name);

      Set_Library_Unit          (Withn, P);
      Set_Corresponding_Spec    (Withn, P_Name);
      Set_First_Name            (Withn, True);
      Set_Implicit_With         (Withn, True);

      --  Node is placed at the beginning of the context items, so that
      --  subsequent use clauses on the parent can be validated.

      Prepend (Withn, Context_Items (N));
      Mark_Rewrite_Insertion (Withn);
      Install_Withed_Unit (Withn);

      if Is_Child_Spec (P_Unit) then
         Implicit_With_On_Parent (P_Unit, N);
      end if;
      New_Nodes_OK := New_Nodes_OK - 1;
   end Implicit_With_On_Parent;

   ---------------------
   -- Install_Context --
   ---------------------

   procedure Install_Context (N : Node_Id) is
      Lib_Unit : constant Node_Id := Unit (N);

   begin
      Install_Context_Clauses (N);

      if Is_Child_Spec (Lib_Unit) then
         Install_Parents (Lib_Unit, Private_Present (Parent (Lib_Unit)));
      end if;
   end Install_Context;

   -----------------------------
   -- Install_Context_Clauses --
   -----------------------------

   procedure Install_Context_Clauses (N : Node_Id) is
      Lib_Unit      : constant Node_Id := Unit (N);
      Item          : Node_Id;
      Uname_Node    : Entity_Id;
      Check_Private : Boolean := False;
      Decl_Node     : Node_Id;
      Lib_Parent    : Entity_Id;

   begin
      --  Loop through context clauses to find the with/use clauses.
      --  This is done twice, first for everything except limited_with
      --  clauses, and then for those, if any are present.

      Item := First (Context_Items (N));
      while Present (Item) loop

         --  Case of explicit WITH clause

         if Nkind (Item) = N_With_Clause
           and then not Implicit_With (Item)
         then
            --  If Name (Item) is not an entity name, something is wrong, and
            --  this will be detected in due course, for now ignore the item

            if not Is_Entity_Name (Name (Item)) then
               goto Continue;

            elsif No (Entity (Name (Item))) then
               Set_Entity (Name (Item), Any_Id);
               goto Continue;
            end if;

            Uname_Node := Entity (Name (Item));

            if Is_Private_Descendant (Uname_Node) then
               Check_Private := True;
            end if;

            Install_Withed_Unit (Item);

            Decl_Node := Unit_Declaration_Node (Uname_Node);

            --  If the unit is a subprogram instance, it appears nested
            --  within a package that carries the parent information.

            if Is_Generic_Instance (Uname_Node)
              and then Ekind (Uname_Node) /= E_Package
            then
               Decl_Node := Parent (Parent (Decl_Node));
            end if;

            if Is_Child_Spec (Decl_Node) then
               if Nkind (Name (Item)) = N_Expanded_Name then
                  Expand_With_Clause (Prefix (Name (Item)), N);
               else
                  --  if not an expanded name, the child unit must be a
                  --  renaming, nothing to do.

                  null;
               end if;

            elsif Nkind (Decl_Node) = N_Subprogram_Body
              and then not Acts_As_Spec (Parent (Decl_Node))
              and then Is_Child_Spec (Unit (Library_Unit (Parent (Decl_Node))))
            then
               Implicit_With_On_Parent
                 (Unit (Library_Unit (Parent (Decl_Node))), N);
            end if;

         --  Case of USE PACKAGE clause

         elsif Nkind (Item) = N_Use_Package_Clause then
            Analyze_Use_Package (Item);

         --  case of PRAGMA

         elsif Nkind (Item) = N_Pragma then
            Analyze (Item);
         end if;

      <<Continue>>
         Next (Item);
      end loop;

      if Is_Child_Spec (Lib_Unit) then

         --  The unit also has implicit withs on its own parents

         if No (Context_Items (N)) then
            Set_Context_Items (N, New_List);
         end if;

         Implicit_With_On_Parent (Lib_Unit, N);
      end if;

      --  If the unit is a body, the context of the specification must also
      --  be installed.

      if Nkind (Lib_Unit) = N_Package_Body
        or else (Nkind (Lib_Unit) = N_Subprogram_Body
                  and then not Acts_As_Spec (N))
      then
         Install_Context (Library_Unit (N));

         if Is_Child_Spec (Unit (Library_Unit (N))) then

            --  If the unit is the body of a public child unit, the private
            --  declarations of the parent must be made visible. If the child
            --  unit is private, the private declarations have been installed
            --  already in the call to Install_Parents for the spec. Installing
            --  private declarations must be done for all ancestors of public
            --  child units. In addition, sibling units mentioned in the
            --  context clause of the body are directly visible.

            declare
               Lib_Spec : Node_Id := Unit (Library_Unit (N));
               P        : Node_Id;
               P_Name   : Entity_Id;

            begin
               while Is_Child_Spec (Lib_Spec) loop
                  P := Unit (Parent_Spec (Lib_Spec));

                  if not (Private_Present (Parent (Lib_Spec))) then
                     P_Name := Defining_Entity (P);
                     Install_Private_Declarations (P_Name);
                     Set_Use (Private_Declarations (Specification (P)));
                  end if;

                  Lib_Spec := P;
               end loop;
            end;
         end if;

         --  For a package body, children in context are immediately visible

         Install_Siblings (Defining_Entity (Unit (Library_Unit (N))), N);
      end if;

      if Nkind (Lib_Unit) = N_Generic_Package_Declaration
        or else Nkind (Lib_Unit) = N_Generic_Subprogram_Declaration
        or else Nkind (Lib_Unit) = N_Package_Declaration
        or else Nkind (Lib_Unit) = N_Subprogram_Declaration
      then
         if Is_Child_Spec (Lib_Unit) then
            Lib_Parent := Defining_Entity (Unit (Parent_Spec (Lib_Unit)));
            Set_Is_Private_Descendant
              (Defining_Entity (Lib_Unit),
               Is_Private_Descendant (Lib_Parent)
                 or else Private_Present (Parent (Lib_Unit)));

         else
            Set_Is_Private_Descendant
              (Defining_Entity (Lib_Unit),
               Private_Present (Parent (Lib_Unit)));
         end if;
      end if;

   end Install_Context_Clauses;

   ---------------------
   -- Install_Parents --
   ---------------------

   procedure Install_Parents (Lib_Unit : Node_Id; Is_Private : Boolean) is
      P      : Node_Id;
      E_Name : Entity_Id;
      P_Name : Entity_Id;
      P_Spec : Node_Id;

   begin
      P := Unit (Parent_Spec (Lib_Unit));
      P_Name := Get_Parent_Entity (P);

      if Etype (P_Name) = Any_Type then
         return;
      end if;

      if Ekind (P_Name) = E_Generic_Package
        and then Nkind (Lib_Unit) /= N_Generic_Subprogram_Declaration
        and then Nkind (Lib_Unit) /= N_Generic_Package_Declaration
        and then Nkind (Lib_Unit) not in N_Generic_Renaming_Declaration
      then
         Error_Msg_N
           ("child of a generic package must be a generic unit", Lib_Unit);

      elsif not Is_Package (P_Name) then
         Error_Msg_N
           ("parent unit must be package or generic package", Lib_Unit);
         raise Unrecoverable_Error;

      elsif Present (Renamed_Object (P_Name)) then
         Error_Msg_N ("parent unit cannot be a renaming", Lib_Unit);
         raise Unrecoverable_Error;

      --  Verify that a child of an instance is itself an instance, or
      --  the renaming of one. Given that an instance that is a unit is
      --  replaced with a package declaration, check against the original
      --  node.

      elsif Nkind (Original_Node (P)) = N_Package_Instantiation
        and then Nkind (Lib_Unit)
                   not in N_Renaming_Declaration
        and then Nkind (Original_Node (Lib_Unit))
                   not in N_Generic_Instantiation
      then
         Error_Msg_N
           ("child of an instance must be an instance or renaming", Lib_Unit);
      end if;

      --  This is the recursive call that ensures all parents are loaded

      if Is_Child_Spec (P) then
         Install_Parents (P,
           Is_Private or else Private_Present (Parent (Lib_Unit)));
      end if;

      --  Now we can install the context for this parent

      Install_Context_Clauses (Parent_Spec (Lib_Unit));
      Install_Siblings (P_Name, Parent (Lib_Unit));

      --  The child unit is in the declarative region of the parent. The
      --  parent must therefore appear in the scope stack and be visible,
      --  as when compiling the corresponding body. If the child unit is
      --  private or it is a package body, private declarations must be
      --  accessible as well. Use declarations in the parent must also
      --  be installed. Finally, other child units of the same parent that
      --  are in the context are immediately visible.

      --  Find entity for compilation unit, and set its private descendant
      --  status as needed.

      E_Name := Defining_Entity (Lib_Unit);

      Set_Is_Child_Unit (E_Name);

      Set_Is_Private_Descendant (E_Name,
         Is_Private_Descendant (P_Name)
           or else Private_Present (Parent (Lib_Unit)));

      P_Spec := Specification (Unit_Declaration_Node (P_Name));
      New_Scope (P_Name);

      --  Save current visibility of unit

      Scope_Stack.Table (Scope_Stack.Last).Previous_Visibility :=
        Is_Immediately_Visible (P_Name);
      Set_Is_Immediately_Visible (P_Name);
      Install_Visible_Declarations (P_Name);
      Set_Use (Visible_Declarations (P_Spec));

      --  If the parent is a generic unit, its formal part may contain
      --  formal packages and use clauses for them.

      if Ekind (P_Name) = E_Generic_Package then
         Set_Use (Generic_Formal_Declarations (Parent (P_Spec)));
      end if;

      if Is_Private
        or else Private_Present (Parent (Lib_Unit))
      then
         Install_Private_Declarations (P_Name);
         Set_Use (Private_Declarations (P_Spec));
      end if;
   end Install_Parents;

   ----------------------
   -- Install_Siblings --
   ----------------------

   procedure Install_Siblings (U_Name : Entity_Id; N : Node_Id) is
      Item : Node_Id;
      Id   : Entity_Id;
      Prev : Entity_Id;

      function Is_Ancestor (E : Entity_Id) return Boolean;
      --  Determine whether the scope of a child unit is an ancestor of
      --  the current unit.
      --  Shouldn't this be somewhere more general ???

      -----------------
      -- Is_Ancestor --
      -----------------

      function Is_Ancestor (E : Entity_Id) return Boolean is
         Par : Entity_Id;

      begin
         Par := U_Name;

         while Present (Par)
           and then Par /= Standard_Standard
         loop

            if Par = E then
               return True;
            end if;

            Par := Scope (Par);
         end loop;

         return False;
      end Is_Ancestor;

   --  Start of processing for Install_Siblings

   begin
      --  Iterate over explicit with clauses, and check whether the
      --  scope of each entity is an ancestor of the current unit.

      Item := First (Context_Items (N));

      while Present (Item) loop

         if Nkind (Item) = N_With_Clause
           and then not Implicit_With (Item)
         then
            Id := Entity (Name (Item));

            if Is_Child_Unit (Id)
              and then Is_Ancestor (Scope (Id))
            then
               Set_Is_Immediately_Visible (Id);
               Prev := Current_Entity (Id);

               --  Check for the presence of another unit in the context,
               --  that may be inadvertently hidden by the child.

               if Present (Prev)
                 and then Is_Immediately_Visible (Prev)
                 and then not Is_Child_Unit (Prev)
               then
                  declare
                     Clause : Node_Id;

                  begin
                     Clause := First (Context_Items (N));

                     while Present (Clause) loop
                        if Nkind (Clause) = N_With_Clause
                          and then Entity (Name (Clause)) = Prev
                        then
                           Error_Msg_NE
                              ("child unit& hides compilation unit " &
                               "with the same name?",
                                 Name (Item), Id);
                           exit;
                        end if;

                        Next (Clause);
                     end loop;
                  end;
               end if;

            --  the With_Clause may be on a grand-child, which makes
            --  the child immediately visible.

            elsif Is_Child_Unit (Scope (Id))
              and then Is_Ancestor (Scope (Scope (Id)))
            then
               Set_Is_Immediately_Visible (Scope (Id));
            end if;

         end if;

         Next (Item);
      end loop;
   end Install_Siblings;

   -------------------------
   -- Install_Withed_Unit --
   -------------------------

   procedure Install_Withed_Unit (With_Clause : Node_Id) is
      Uname : constant Entity_Id := Entity (Name (With_Clause));
      P     : constant Entity_Id := Scope (Uname);

   begin

      if Debug_Flag_I then
         Write_Str ("install withed unit ");
         Write_Name (Chars (Uname));
         Write_Eol;
      end if;

      --  We do not apply the restrictions to an internal unit unless
      --  we are compiling the internal unit as a main unit. This check
      --  is also skipped for dummy units (for missing packages).

      if Sloc (Uname) /= No_Location
        and then (not Is_Internal_File_Name (Unit_File_Name (Current_Sem_Unit))
                    or else Current_Sem_Unit = Main_Unit)
      then
         Check_Restricted_Unit
           (Unit_Name (Get_Source_Unit (Uname)), With_Clause);
      end if;

      if P /= Standard_Standard then

         --  If the unit is not analyzed after analysis of the with clause,
         --  and it is an instantiation, then it awaits a body and is the main
         --  unit. Its appearance in the context of some other unit indicates
         --  a circular dependency (DEC suite perversity).

         if not Analyzed (Uname)
           and then Nkind (Parent (Uname)) = N_Package_Instantiation
         then
            Error_Msg_N
              ("instantiation depends on itself", Name (With_Clause));

         elsif not Is_Visible_Child_Unit (Uname) then
            Set_Is_Visible_Child_Unit (Uname);

            if Is_Generic_Instance (Uname)
              and then Ekind (Uname) in Subprogram_Kind
            then
               --  Set flag as well on the visible entity that denotes the
               --  instance, which renames the current one.

               Set_Is_Visible_Child_Unit
                 (Related_Instance
                   (Defining_Entity (Unit (Library_Unit (With_Clause)))));
               null;
            end if;

            --  The parent unit may have been installed already, and
            --  may have appeared in a use clause.

            if In_Use (Scope (Uname)) then
               Set_Is_Potentially_Use_Visible (Uname);
            end if;

            Set_Context_Installed (With_Clause);
         end if;

      elsif not Is_Immediately_Visible (Uname) then
         Set_Is_Immediately_Visible (Uname);
         Set_Context_Installed (With_Clause);
      end if;

   end Install_Withed_Unit;

   -------------------
   -- Is_Child_Spec --
   -------------------

   function Is_Child_Spec (Lib_Unit : Node_Id) return Boolean is
      K : constant Node_Kind := Nkind (Lib_Unit);

   begin
      return (K in N_Generic_Declaration              or else
              K in N_Generic_Instantiation            or else
              K in N_Generic_Renaming_Declaration     or else
              K =  N_Package_Declaration              or else
              K =  N_Package_Renaming_Declaration     or else
              K =  N_Subprogram_Declaration           or else
              K =  N_Subprogram_Renaming_Declaration)
        and then Present (Parent_Spec (Lib_Unit));
   end Is_Child_Spec;

   -----------------------
   -- Load_Needed_Body --
   -----------------------

   --  N is a generic unit named in a with clause, or else it is
   --  a unit that contains a generic unit or an inlined function.
   --  In order to perform an instantiation, the body of the unit
   --  must be present. If the unit itself is generic, we assume
   --  that an instantiation follows, and  load and analyze the body
   --  unconditionally. This forces analysis of the spec as well.

   --  If the unit is not generic, but contains a generic unit, it
   --  is loaded on demand, at the point of instantiation (see ch12).

   procedure Load_Needed_Body (N : Node_Id; OK : out Boolean) is
      Body_Name : Unit_Name_Type;
      Unum      : Unit_Number_Type;

   begin
      Body_Name := Get_Body_Name (Get_Unit_Name (Unit (N)));
      Unum :=
        Load_Unit
          (Load_Name  => Body_Name,
           Required   => False,
           Subunit    => False,
           Error_Node => N,
           Renamings  => True);

      if Unum = No_Unit then
         OK := False;

      else
         Compiler_State := Analyzing; -- reset after load

         if not Fatal_Error (Unum) or else Try_Semantics then
            if Debug_Flag_L then
               Write_Str ("*** Loaded generic body");
               Write_Eol;
            end if;

            Semantics (Cunit (Unum));
         end if;

         OK := True;
      end if;
      
   end Load_Needed_Body;

   -------------------------------
   -- Check_Body_Needed_For_SAL --
   -------------------------------

   procedure Check_Body_Needed_For_SAL (Unit_Name : Entity_Id) is

      function Entity_Needs_Body (E : Entity_Id) return Boolean;
      --  Determine whether use of entity E might require the presence
      --  of its body. For a package this requires a recursive traversal
      --  of all nested declarations.

      ---------------------------
      -- Entity_Needed_For_SAL --
      ---------------------------

      function Entity_Needs_Body (E : Entity_Id) return Boolean is
         Ent : Entity_Id;

      begin
         if Is_Subprogram (E)
           and then Has_Pragma_Inline (E)
         then
            return True;

         elsif Ekind (E) = E_Generic_Function
           or else Ekind (E) = E_Generic_Procedure
         then
            return True;

         elsif Ekind (E) = E_Generic_Package
           and then
             Nkind (Unit_Declaration_Node (E)) = N_Generic_Package_Declaration
           and then Present (Corresponding_Body (Unit_Declaration_Node (E)))
         then
            return True;

         elsif Ekind (E) = E_Package
           and then
             Nkind (Unit_Declaration_Node (E)) = N_Package_Declaration
           and then Present (Corresponding_Body (Unit_Declaration_Node (E)))
         then
            Ent := First_Entity (E);

            while Present (Ent) loop
               if Entity_Needs_Body (Ent) then
                  return True;
               end if;

               Next_Entity (Ent);
            end loop;

            return False;

         else
            return False;
         end if;
      end Entity_Needs_Body;

   --  Start of processing for Check_Body_Needed_For_SAL

   begin
      if Ekind (Unit_Name) = E_Generic_Package
        and then
          Nkind (Unit_Declaration_Node (Unit_Name)) =
                                            N_Generic_Package_Declaration
        and then
          Present (Corresponding_Body (Unit_Declaration_Node (Unit_Name)))
      then
         Set_Body_Needed_For_SAL (Unit_Name);

      elsif Ekind (Unit_Name) = E_Generic_Procedure
        or else Ekind (Unit_Name) = E_Generic_Function
      then
         Set_Body_Needed_For_SAL (Unit_Name);

      elsif Is_Subprogram (Unit_Name)
        and then Nkind (Unit_Declaration_Node (Unit_Name)) =
                                            N_Subprogram_Declaration
        and then Has_Pragma_Inline (Unit_Name)
      then
         Set_Body_Needed_For_SAL (Unit_Name);

      elsif Ekind (Unit_Name) = E_Subprogram_Body then
         Check_Body_Needed_For_SAL
           (Corresponding_Spec (Unit_Declaration_Node (Unit_Name)));

      elsif Ekind (Unit_Name) = E_Package
        and then Entity_Needs_Body (Unit_Name)
      then
         Set_Body_Needed_For_SAL (Unit_Name);

      elsif Ekind (Unit_Name) = E_Package_Body
        and then Nkind (Unit_Declaration_Node (Unit_Name)) = N_Package_Body
      then
         Check_Body_Needed_For_SAL
           (Corresponding_Spec (Unit_Declaration_Node (Unit_Name)));
      end if;
   end Check_Body_Needed_For_SAL;

   --------------------
   -- Remove_Context --
   --------------------

   procedure Remove_Context (N : Node_Id) is
      Lib_Unit : constant Node_Id := Unit (N);

   begin
      --  If this is a child unit, first remove the parent units.

      if Is_Child_Spec (Lib_Unit) then
         Remove_Parents (Lib_Unit);
      end if;

      Remove_Context_Clauses (N);
   end Remove_Context;

   ----------------------------
   -- Remove_Context_Clauses --
   ----------------------------

   procedure Remove_Context_Clauses (N : Node_Id) is
      Item      : Node_Id;
      Unit_Name : Entity_Id;

   begin
      --  Second Phase: Loop through context items and undo regular
      --  with_clauses and use_clauses.

      Item := First (Context_Items (N));
      while Present (Item) loop

	 if Nkind (Item) = N_With_Clause
            and then Context_Installed (Item)
         then
            --  Remove items from one with'ed unit

            Unit_Name := Entity (Name (Item));
            Remove_Unit_From_Visibility (Unit_Name);
            Set_Context_Installed (Item, False);

         elsif Nkind (Item) = N_Use_Package_Clause then
            End_Use_Package (Item);

         end if;

         Next (Item);
      end loop;
   end Remove_Context_Clauses;

   --------------------
   -- Remove_Parents --
   --------------------

   procedure Remove_Parents (Lib_Unit : Node_Id) is
      P      : Node_Id;
      P_Name : Entity_Id;
      E      : Entity_Id;
      Vis    : constant Boolean :=
                 Scope_Stack.Table (Scope_Stack.Last).Previous_Visibility;

   begin
      if Is_Child_Spec (Lib_Unit) then
         P := Unit (Parent_Spec (Lib_Unit));
         P_Name := Get_Parent_Entity (P);

         Remove_Context_Clauses (Parent_Spec (Lib_Unit));
         End_Package_Scope (P_Name);
         Set_Is_Immediately_Visible (P_Name, Vis);

         --  Remove from visibility the siblings as well, which are directly
         --  visible while the parent is in scope.

         E := First_Entity (P_Name);

         while Present (E) loop

            if Is_Child_Unit (E) then
               Set_Is_Immediately_Visible (E, False);
            end if;

            Next_Entity (E);
         end loop;

         Set_In_Package_Body (P_Name, False);

         --  This is the recursive call to remove the context of any
         --  higher level parent. This recursion ensures that all parents
         --  are removed in the reverse order of their installation.

         Remove_Parents (P);
      end if;
   end Remove_Parents;

   ---------------------------------
   -- Remove_Unit_From_Visibility --
   ---------------------------------

   procedure Remove_Unit_From_Visibility (Unit_Name : Entity_Id) is
      P : constant Entity_Id := Scope (Unit_Name);

   begin

      if Debug_Flag_I then
         Write_Str ("remove unit ");
         Write_Name (Chars (Unit_Name));
         Write_Str (" from visibility");
         Write_Eol;
      end if;

      if P /= Standard_Standard then
         Set_Is_Visible_Child_Unit (Unit_Name, False);
      end if;

      Set_Is_Potentially_Use_Visible (Unit_Name, False);
      Set_Is_Immediately_Visible     (Unit_Name, False);

   end Remove_Unit_From_Visibility;

   -------------
   -- Unchain --
   -------------

   procedure Unchain (E : Entity_Id) is
      Prev : Entity_Id;

   begin
      Prev := Current_Entity (E);

      if No (Prev) then
         return;

      elsif Prev = E then
         Set_Name_Entity_Id (Chars (E), Homonym (E));

      else
         while Present (Prev)
           and then Homonym (Prev) /= E
         loop
            Prev := Homonym (Prev);
         end loop;

         if Present (Prev) then
            Set_Homonym (Prev, Homonym (E));
         end if;
      end if;

      if Debug_Flag_I then
         Write_Str ("   (homonym) unchain ");
         Write_Name (Chars (E));
         Write_Eol;
      end if;

   end Unchain;
end Sem_Ch10;
