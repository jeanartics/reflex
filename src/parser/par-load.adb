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

--  The Par.Load procedure loads all units that are definitely required before
--  it makes any sense at all to proceed with semantic analysis, including
--  with'ed units, corresponding specs for bodies, parents of child specs,
--  and parents of subunits. All these units are loaded and pointers installed
--  in the tree as described in the spec of package Lib.

with Fname;    use Fname;
with Fname.UF; use Fname.UF;
with Lib.Load; use Lib.Load;
with Uname;    use Uname;
with Namet;    use Namet;
with Casing;   use Casing;
with Opt;      use Opt;
with Osint;    use Osint;
with Sinput.L; use Sinput.L;
--with Stylesw;  use Stylesw;
with Validsw;  use Validsw;

with GNAT.Spelling_Checker; use GNAT.Spelling_Checker;

separate (Par)
procedure Load is

   File_Name : File_Name_Type;
   --  Name of file for current unit, derived from unit name

   Cur_Unum : constant Unit_Number_Type := Current_Source_Unit;
   --  Unit number of unit that we just finished parsing. Note that we need
   --  to capture this, because Source_Unit will change as we parse new
   --  source files in the multiple main source file case.

   Curunit : constant Node_Id := Cunit (Cur_Unum);
   --  Compilation unit node for current compilation unit

   Loc : Source_Ptr := Sloc (Curunit);
   --  Source location for compilation unit node

   With_Cunit : Node_Id;
   --  Compilation unit node for withed unit

   Context_Node : Node_Id;
   --  Next node in context items list

   With_Node : Node_Id;
   --  N_With_Clause node

   Spec_Name : Unit_Name_Type;
   --  Unit name of required spec

   Body_Name : Unit_Name_Type;
   --  Unit name of corresponding body

   Unum : Unit_Number_Type;
   --  Unit number of loaded unit

   function Same_File_Name_Except_For_Case
     (Expected_File_Name : File_Name_Type;
      Actual_File_Name   : File_Name_Type)
      return               Boolean;
   --  Given an actual file name and an expected file name (the latter being
   --  derived from the unit name), determine if they are the same except for
   --  possibly different casing of letters.

   function Same_File_Name_Except_For_Case
     (Expected_File_Name : File_Name_Type;
      Actual_File_Name   : File_Name_Type)
      return               Boolean
   is
   begin
      Get_Name_String (Actual_File_Name);
      Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));

      declare
         Lower_Case_Actual_File_Name : String (1 .. Name_Len);

      begin
         Lower_Case_Actual_File_Name := Name_Buffer (1 .. Name_Len);
         Get_Name_String (Expected_File_Name);
         Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
         return Lower_Case_Actual_File_Name = Name_Buffer (1 .. Name_Len);
      end;

   end Same_File_Name_Except_For_Case;

--  Start of processing for Load

begin
   --  Don't do any loads if we already had a fatal error

   if Fatal_Error (Cur_Unum) then
      return;
   end if;

   --  If main unit, set Main_Unit_Entity (this will get overwritten if
   --  the main unit has a separate spec, that happens later on in Load)

   if Cur_Unum = Main_Unit then
      Main_Unit_Entity := Cunit_Entity (Main_Unit);
   end if;

   --  If we have no unit name, things are seriously messed up by previous
   --  errors, and we should not try to continue compilation.

   if Unit_Name (Cur_Unum) = No_Name then
      raise Unrecoverable_Error;
   end if;

   --  Next step, make sure that the unit name matches the file name
   --  and issue a warning message if not. We only output this for the
   --  main unit, since for other units it is more serious and is
   --  caught in a separate test below.

   File_Name :=
     Get_File_Name
       (Unit_Name (Cur_Unum), False);

   if Cur_Unum = Main_Unit
     and then File_Name /= Unit_File_Name (Cur_Unum)
     and then (Same_File_Name_Except_For_Case
                 (File_Name, Unit_File_Name (Cur_Unum)))
   then
      Error_Msg_Name_1 := File_Name;
      Error_Msg
        ("?file name does not match unit name, should be{", Sloc (Curunit));
   end if;

   --  For units other than the main unit, the expected unit name is set and
   --  must be the same as the actual unit name, or we are in big trouble, and
   --  abandon the compilation since there are situations where this really
   --  gets us into bad trouble (e.g. some subunit situations).

   if Cur_Unum /= Main_Unit
     and then Expected_Unit (Cur_Unum) /= Unit_Name (Cur_Unum)
   then
      Loc := Error_Location (Cur_Unum);
      Error_Msg_Name_1 := Unit_File_Name (Cur_Unum);
      Get_Name_String (Error_Msg_Name_1);

      --  Check for predefined file case

      if Name_Len > 1
        and then Name_Buffer (2) = '-'
        and then (Name_Buffer (1) = 'a'
                    or else
                  Name_Buffer (1) = 's'
                    or else
                  Name_Buffer (1) = 'i'
                    or else
                  Name_Buffer (1) = 'g')
      then
         declare
            Expect_Name : constant Name_Id := Expected_Unit (Cur_Unum);
            Actual_Name : constant Name_Id := Unit_Name (Cur_Unum);

         begin
            Error_Msg_Name_1 := Expect_Name;
            Error_Msg ("% is not a predefined library unit!", Loc);

            --  In the predefined file case, we know the user did not
            --  construct their own package, but we got the wrong one.
            --  This means that the name supplied by the user crunched
            --  to something we recognized, but then the file did not
            --  contain the unit expected. Most likely this is due to
            --  a misspelling, e.g.

            --    with Ada.Calender;

            --  This crunches to a-calend, which indeed contains the unit
            --  Ada.Calendar, and we can diagnose the misspelling. This
            --  is a simple heuristic, but it catches many common cases
            --  of misspelling of predefined unit names without needing
            --  a full list of them.

            --  Before actually issinying the message, we will check that the
            --  unit name is indeed a plausible misspelling of the one we got.

            if Is_Bad_Spelling_Of
              (Found  => Get_Name_String (Expect_Name),
               Expect => Get_Name_String (Actual_Name))
            then
               Error_Msg_Name_1 := Actual_Name;
               Error_Msg ("possible misspelling of %!", Loc);
            end if;
         end;

      --  Non-predefined file name case. In this case we generate a message
      --  and then we quit, because we are in big trouble, and if we try
      --  to continue compilation, we get into some nasty situations
      --  (for example in some subunit cases).

      else
         Error_Msg ("file { does not contain expected unit!", Loc);
         Error_Msg_Unit_1 := Expected_Unit (Cur_Unum);
         Error_Msg ("expected unit $!", Loc);
         Error_Msg_Unit_1 := Unit_Name (Cur_Unum);
         Error_Msg ("found unit $!", Loc);
      end if;

      --  In both cases, remove the unit if it is the last unit (which it
      --  normally (always?) will be) so that it is out of the way later.

      Remove_Unit (Cur_Unum);
   end if;

   --  If current unit is a body, load its corresponding spec

   if Nkind (Unit (Curunit)) = N_Package_Body
     or else Nkind (Unit (Curunit)) = N_Subprogram_Body
   then
      Spec_Name := Get_Spec_Name (Unit_Name (Cur_Unum));

      Unum :=
        Load_Unit
          (Load_Name  => Spec_Name,
           Required   => False,
           Subunit    => False,
           Error_Node => Curunit,
           Corr_Body  => Cur_Unum);

      --  If we successfully load the unit, then set the spec pointer. Once
      --  again note that if the loaded unit has a fatal error, Load will
      --  have set our Fatal_Error flag to propagate this condition.

      if Unum /= No_Unit then
         Set_Library_Unit (Curunit, Cunit (Unum));
         Set_Library_Unit (Cunit (Unum), Curunit);

         --  If this is a separate spec for the main unit, then we reset
         --  Main_Unit_Entity to point to the entity for this separate spec

         if Cur_Unum = Main_Unit then
            Main_Unit_Entity := Cunit_Entity (Unum);
         end if;

      --  If we don't find the spec, then if we have a subprogram body, we
      --  are still OK, we just have a case of a body acting as its own spec

      elsif Nkind (Unit (Curunit)) = N_Subprogram_Body then
         Set_Acts_As_Spec (Curunit, True);
         Set_Library_Unit (Curunit, Curunit);

      --  Otherwise we do have an error, repeat the load request for the spec
      --  with Required set True to generate an appropriate error message.

      else
         Unum :=
           Load_Unit
             (Load_Name  => Spec_Name,
              Required   => True,
              Subunit    => False,
              Error_Node => Curunit);
         return;
      end if;

   --  If current unit is a child unit spec, load its parent

   elsif Nkind (Unit (Curunit)) = N_Package_Declaration
     or else Nkind (Unit (Curunit)) =  N_Subprogram_Declaration
     or else Nkind (Unit (Curunit)) in N_Generic_Declaration
     or else Nkind (Unit (Curunit)) in N_Generic_Instantiation
     or else Nkind (Unit (Curunit)) in N_Renaming_Declaration
   then
      Spec_Name := Get_Parent_Spec_Name (Unit_Name (Cur_Unum));

      if Spec_Name /= No_Name then
         Unum :=
           Load_Unit
             (Load_Name  => Spec_Name,
              Required   => True,
              Subunit    => False,
              Error_Node => Curunit);

         if Unum /= No_Unit then
            Set_Parent_Spec (Unit (Curunit), Cunit (Unum));
         end if;
      end if;
   end if;

   --  Loop through context items

   Context_Node := First (Context_Items (Curunit));
   while Present (Context_Node) loop

      if Nkind (Context_Node) = N_With_Clause then
         With_Node := Context_Node;
         Spec_Name := Get_Unit_Name (With_Node);

         Unum :=
           Load_Unit
             (Load_Name  => Spec_Name,
              Required   => False,
              Subunit    => False,
              Error_Node => With_Node,
              Renamings  => True);

         --  If we find the unit, then set spec pointer in the N_With_Clause
         --  to point to the compilation unit for the spec. Remember that
         --  the Load routine itself sets our Fatal_Error flag if the loaded
         --  unit gets a fatal error, so we don't need to worry about that.

         if Unum /= No_Unit then
            Set_Library_Unit (With_Node, Cunit (Unum));

         --  If the spec isn't found, then try finding the corresponding
         --  body, since it is possible that we have a subprogram body
         --  that is acting as a spec (since no spec is present).

         else
            Body_Name := Get_Body_Name (Spec_Name);
            Unum :=
              Load_Unit
                (Load_Name  => Body_Name,
                 Required   => False,
                 Subunit    => False,
                 Error_Node => With_Node,
                 Renamings  => True);

            --  If we got a subprogram body, then mark that we are using
            --  the body as a spec in the file table, and set the spec
            --  pointer in the N_With_Clause to point to the body entity.

            if Unum /= No_Unit
              and then Nkind (Unit (Cunit (Unum))) = N_Subprogram_Body
            then
               With_Cunit := Cunit (Unum);
               Set_Library_Unit (With_Node, With_Cunit);
               Set_Acts_As_Spec (With_Cunit, True);
               Set_Library_Unit (With_Cunit, With_Cunit);

            --  If we couldn't find the body, or if it wasn't a body spec
            --  then we are in trouble. We make one more call to Load to
            --  require the spec. We know it will fail of course, the
            --  purpose is to generate the required error message (we prefer
            --  that this message refer to the missing spec, not the body)

            else
               Unum :=
                 Load_Unit
                   (Load_Name  => Spec_Name,
                    Required   => True,
                    Subunit    => False,
                    Error_Node => With_Node,
                    Renamings  => True);

               --  Here we create a dummy package unit for the missing unit

               Unum := Create_Dummy_Package_Unit (With_Node, Spec_Name);
               Set_Library_Unit (With_Node, Cunit (Unum));
            end if;
         end if;
      end if;

      Next (Context_Node);
   end loop;

end Load;
