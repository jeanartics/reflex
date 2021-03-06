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

with Atree;    use Atree;
with Back_End; use Back_End;
with Comperr;
with Csets;    use Csets;
with Debug;    use Debug;
with Elists;
with Errout;   use Errout;
with Fmap;
with Fname;    use Fname;
with Fname.UF; use Fname.UF;
with Frontend;
with Gnatvsn;  use Gnatvsn;
with Hostparm;
with Inline;
with Lib;      use Lib;
--with Lib.Writ; use Lib.Writ;
--with Lib.Xref;
with Namet;    use Namet;
with Nlists;
with Opt;      use Opt;
with Osint;    use Osint;
with Output;   use Output;
--with Prepcomp;
with Repinfo;  use Repinfo;
with Restrict;
with Rident;
with Sem;
with Sem_Ch8;
with Sem_Ch12;
with Sem_Ch13;
--with Sem_Elim;
with Sem_Eval;
with Sem_Type;
with Sinfo;    use Sinfo;
with Sinput.L; use Sinput.L;
with Snames;
with Sprint;   use Sprint;
with Stringt;
with Targparm;
with Tree_Gen;
with Treepr;   use Treepr;
--with Ttypes;
with Types;    use Types;
with Namet; use Namet;
with Uintp;    use Uintp;
with Uname;    use Uname;
with Urealp;
with Usage;

-- with Reflex.Configs;
-- with Reflex.Infos;
with Reflex_Options;
----with Reflex.Expansion;
with Reflex.Gen.Ada_Outputs;
with Rxada.Generator;
with Reflex.Names; use Reflex.Names;
with Reflex.Vertices; use Reflex.Vertices;

with System.Assertions;
with Ada.Text_IO; use Ada.Text_IO;
with Rx.Utils; use Rx.Utils;

procedure Reflex2Ada is
   Main_Unit_Node : Node_Id;
   --  Compilation unit node for main unit

   Main_Kind : Node_Kind;
   --  Kind of main compilation unit node.

   Back_End_Mode : Back_End.Back_End_Mode_Type;
   --  Record back end mode

begin
   --  This inner block is set up to catch assertion errors and constraint
   --  errors. Since the code for handling these errors can cause another
   --  exception to be raised (namely Unrecoverable_Error), we need two
   --  nested blocks, so that the outer one handles unrecoverable error.

   begin
      --  Lib.Initialize need to be called before Scan_Compiler_Arguments,
      --  because it initialize a table that is filled by
      --  Scan_Compiler_Arguments.

      Osint.Initialize;
      Fmap.Reset_Tables;
      Lib.Initialize;
--      Lib.Xref.Initialize;
      Scan_Compiler_Arguments;
      Osint.Add_Default_Search_Dirs;

      Nlists.Initialize;
      Sinput.Initialize;
      Sem.Initialize;
      Csets.Initialize;
      Uintp.Initialize;
      Urealp.Initialize;
      Errout.Initialize;
      Namet.Initialize;
      Snames.Initialize;
      Reflex.Names.Initialize;
      Stringt.Initialize;
      --Inline.Initialize;
      Sem_Ch8.Initialize;
      Sem_Ch12.Initialize;
      Sem_Ch13.Initialize;
      Sem_Eval.Initialize;
      Sem_Type.Init_Interp_Tables;
      Reflex.Vertices.Initialize_Vertices;

      --  Acquire target parameters from system.rxs (source of package System)

      declare
         use Sinput;

         S : Source_File_Index;
         N : Name_Id;
         R : Restrict.Restriction_Id;
         P : Restrict.Restriction_Parameter_Id;
	 Exe : String := Full_Executable_Location;

      begin
	 N := String_Find (Exe & "system.rxs");
         S := Load_Source_File (File_Name_Type (N));

         if S = No_Source_File then
            Write_Line
              ("fatal error, run-time library not installed correctly");
            Write_Line
              ("cannot locate file system.rxs");
            raise Unrecoverable_Error;

         --  Here if system.rxs successfully read. Remember its source index.

         else
            System_Source_File_Index := S;
         end if;

         Targparm.Get_Target_Parameters
           (System_Text  => Source_Text (S),
            Source_First => Source_First (S),
            Source_Last  => Source_Last (S));

         --  Acquire configuration pragma information from Targparm

         for J in Rident.Partition_Restrictions loop
            R := Restrict.Partition_Restrictions (J);

            if Targparm.Restrictions_On_Target (J) then
               Restrict.Restrictions (R)     := True;
               Restrict.Restrictions_Loc (R) := System_Location;
            end if;
         end loop;

         for K in Rident.Restriction_Parameter_Id loop
            P := Restrict.Restriction_Parameter_Id (K);

            if Targparm.Restriction_Parameters_On_Target (K) /= No_Uint then
               Restrict.Restriction_Parameters (P) :=
                 Targparm.Restriction_Parameters_On_Target (K);
               Restrict.Restriction_Parameters_Loc (P) := System_Location;
            end if;
         end loop;
      end;

      --  Set Configurable_Run_Time mode if system.rxs flag set

      if Targparm.Configurable_Run_Time_On_Target or Debug_Flag_YY then
         Configurable_Run_Time_Mode := True;
      end if;

      --  Output copyright notice if full list mode

      if (Verbose_Mode or Full_List)
        and then (not Debug_Flag_7)
      then
         Write_Eol;
         Write_Str ("reflex2ada ");
         Write_Str (Gnat_Version_String);
         Write_Str (" Copyright 2012-2020 Free Software Foundation, Inc.");
         Write_Eol;
      end if;

      --  Set proper status for overflow checks. We turn on overflow checks
      --  if -gnatp was not specified, and either -gnato is set or the back
      --  end takes care of overflow checks. Otherwise we suppress overflow
      --  checks by default (since front end checks are expensive).

      if not Opt.Suppress_Checks
        and then (Opt.Enable_Overflow_Checks
                    or else
                      (Targparm.Backend_Divide_Checks_On_Target
                        and
                       Targparm.Backend_Overflow_Checks_On_Target))
      then
         Suppress_Options (Overflow_Check) := False;
      else
         Suppress_Options (Overflow_Check) := True;
      end if;

      --  Check we have exactly one source file, this happens only in
      --  the case where the driver is called directly, it cannot happen
      --  when gnat1 is invoked from gcc in the normal case.

      if Osint.Number_Of_Files /= 1 then
         Usage;
         Write_Eol;
         Osint.Fail ("you must provide one source file");

      elsif Usage_Requested then
         Usage;
      end if;

      Original_Operating_Mode := Operating_Mode;
      Frontend;
      Main_Unit_Node := Cunit (Main_Unit);
      Main_Kind := Nkind (Unit (Main_Unit_Node));

      --  Check for suspicious or incorrect body present if we are doing
      --  semantic checking. We omit this check in syntax only mode, because
      --  in that case we do not know if we need a body or not.

      if Operating_Mode /= Check_Syntax
        and then
          ((Main_Kind = N_Package_Declaration
             and then not Body_Required (Main_Unit_Node))
           or else (Main_Kind = N_Generic_Package_Declaration
                     and then not Body_Required (Main_Unit_Node))
           or else Main_Kind = N_Package_Renaming_Declaration
           or else Main_Kind = N_Subprogram_Renaming_Declaration
           or else Nkind (Original_Node (Unit (Main_Unit_Node)))
                           in N_Generic_Instantiation)
      then
         declare
            Sname   : Unit_Name_Type := Unit_Name (Main_Unit);
            Src_Ind : Source_File_Index;
            Fname   : File_Name_Type;

            procedure Bad_Body (Msg : String);
            --  Issue message for bad body found

            procedure Bad_Body (Msg : String) is
            begin
               Error_Msg_N (Msg, Main_Unit_Node);
               Error_Msg_Name_1 := Name_Id (Fname);
               Error_Msg_N
                 ("remove incorrect body in file{!", Main_Unit_Node);
            end Bad_Body;

         begin
            Sname := Unit_Name (Main_Unit);

            --  If we do not already have a body name, then get the body
            --  name (but how can we have a body name here ???)

            if not Is_Body_Name (Sname) then
               Sname := Get_Body_Name (Sname);
            end if;

            Fname := Get_File_Name (Sname, Subunit => False);
            Src_Ind := Load_Source_File (Fname);

            --  Case where body is present and it is not a subunit. Exclude
            --  the subunit case, because it has nothing to do with the
            --  package we are compiling. It is illegal for a child unit
            --  and a subunit with the same expanded name (RM 10.2(9)) to
            --  appear together in a partition, but there is nothing to
            --  stop a compilation environment from having both, and the
            --  test here simply allows that. If there is an attempt to
            --  include both in a partition, this is diagnosed at bind time.
            --  In Ada 83 mode this is not a warning case.
	    
            if Src_Ind /= No_Source_File then
               Error_Msg_Name_1 := Name_Id (Sname);
	       
	       --  For generic instantiations, we never allow a body
	       
	       if Nkind (Original_Node (Unit (Main_Unit_Node)))
		 in N_Generic_Instantiation
	       then
		  Bad_Body
		    ("generic instantiation for % does not allow a body");
		  
                  --  A library unit that is a renaming never allows a body
		  
	       elsif Main_Kind in N_Renaming_Declaration then
		  Bad_Body
		    ("renaming declaration for % does not allow a body!");
		  
                  --  Remaining cases are packages and generic packages.
                  --  Here we only do the test if there are no previous
                  --  errors, because if there are errors, they may lead
                  --  us to incorrectly believe that a package does not
                  --  allow a body when in fact it does.
		  
	       elsif not Compilation_Errors then
		  if Main_Kind = N_Package_Declaration then
		     Bad_Body ("package % does not allow a body!");
		     
		  elsif Main_Kind = N_Generic_Package_Declaration then
		     Bad_Body ("generic package % does not allow a body!");
		  end if;
	       end if;
            end if;
         end;
      end if;

      --  Exit if compilation errors detected

      if Compilation_Errors then
         Sem_Ch13.Validate_Unchecked_Conversions;
         Errout.Finalize;
         Namet.Finalize;

         Exit_Program (E_Errors);
      end if;

      --  Set Generate_Code on main unit and its spec. We do this even if
      --  are not generating code, since Lib-Writ uses this to determine
      --  which units get written in the ali file.

      Set_Generate_Code (Main_Unit);

      --  If we have a corresponding spec, then we need object
      --  code for the spec unit as well

      if Nkind (Unit (Main_Unit_Node)) in N_Unit_Body
        and then not Acts_As_Spec (Main_Unit_Node)
      then
         Set_Generate_Code
           (Get_Cunit_Unit_Number (Library_Unit (Main_Unit_Node)));
      end if;

      --  Case of no code required to be generated, exit indicating no error

      if Original_Operating_Mode = Check_Syntax then
         Errout.Finalize;
         Tree_Gen;
         Namet.Finalize;
         Exit_Program (E_Success);

      elsif Original_Operating_Mode = Check_Semantics then
         Back_End_Mode := Declarations_Only;

      --  All remaining cases are cases in which the user requested that code
      --  be generated (i.e. no -gnatc or -gnats switch was used). Check if
      --  we can in fact satisfy this request.

      --  Cannot generate code if someone has turned off code generation
      --  for any reason at all. We will try to figure out a reason below.

      elsif Operating_Mode /= Generate_Code then
         Back_End_Mode := Skip;

      --  We can generate code for a subprogram body unless there were
      --  missing subunits. Note that we always generate code for all
      --  generic units (a change from some previous versions of GNAT).

      elsif Main_Kind = N_Subprogram_Body then
         Back_End_Mode := Generate_Object;

      --  We can generate code for a package body unless there are subunits
      --  missing (note that we always generate code for generic units, which
      --  is a change from some earlier versions of GNAT).

      elsif Main_Kind = N_Package_Body then
         Back_End_Mode := Generate_Object;

      --  We can generate code for a package declaration or a subprogram
      --  declaration only if it does not required a body.

      elsif (Main_Kind = N_Package_Declaration
               or else
             Main_Kind = N_Subprogram_Declaration)
        and then not Body_Required (Main_Unit_Node)
      then
         Back_End_Mode := Generate_Object;

      --  We can generate code for a generic package declaration of a generic
      --  subprogram declaration only if does not require a body.

      elsif (Main_Kind = N_Generic_Package_Declaration
               or else
             Main_Kind = N_Generic_Subprogram_Declaration)
        and then not Body_Required (Main_Unit_Node)
      then
         Back_End_Mode := Generate_Object;

      --  Compilation units that are renamings do not require bodies,
      --  so we can generate code for them.

      elsif Main_Kind = N_Package_Renaming_Declaration
        or else Main_Kind = N_Subprogram_Renaming_Declaration
      then
         Back_End_Mode := Generate_Object;

      --  Compilation units that are generic renamings do not require bodies
      --  so we can generate code for them.

      elsif Main_Kind in N_Generic_Renaming_Declaration then
         Back_End_Mode := Generate_Object;

      --  In all other cases (specs which have bodies, generics, and bodies
      --  where subunits are missing), we cannot generate code and we generate
      --  a warning message. Note that generic instantiations are gone at this
      --  stage since they have been replaced by their instances.

      else
         Back_End_Mode := Skip;
      end if;

      --  At this stage Call_Back_End is set to indicate if the backend
      --  should be called to generate code. If it is not set, then code
      --  generation has been turned off, even though code was requested
      --  by the original command. This is not an error from the user
      --  point of view, but it is an error from the point of view of
      --  the gcc driver, so we must exit with an error status.

      --  We generate an informative message (from the gcc point of view,
      --  it is an error message, but from the users point of view this
      --  is not an error, just a consequence of compiling something that
      --  cannot generate code).

      if Back_End_Mode = Skip then
         Write_Str ("cannot generate code for ");
         Write_Str ("file ");
         Write_Name (Unit_File_Name (Main_Unit));

	 if Main_Kind = N_Subprogram_Declaration then
            Write_Str (" (subprogram spec)");
            Write_Eol;
            Write_Str ("to check subprogram spec");

         --  Only other case is a package spec

         else
            Write_Str (" (package spec)");
            Write_Eol;
            Write_Str ("to check package spec");
         end if;

         Write_Str (" for errors, use ");
	 Write_Str ("-gnatc");

         Write_Eol;

         Sem_Ch13.Validate_Unchecked_Conversions;
         Errout.Finalize;
         Namet.Finalize;

         --  Exit program with error indication, to kill object file

         Exit_Program (E_No_Code);
      end if;

      --  Ensure that we properly register a dependency on system.rxs,
      --  since even if we do not semantically depend on this, Targparm
      --  has read system parameters from the system.rxs file.

      --Lib.Writ.Ensure_System_Dependency;

      --  Add dependencies, if any, on preprocessing data file and on
      --  preprocessing definition file(s).

      --Prepcomp.Add_Dependencies;

      --  Back end needs to explicitly unlock tables it needs to touch

      Atree.Lock;
      Elists.Lock;
      Fname.UF.Lock;
      Inline.Lock;
      Lib.Lock;
      Nlists.Lock;
      Sem.Lock;
      Sinput.Lock;
      Namet.Lock;
      Stringt.Lock;

      --  Here we call the back end to generate the output code
      -- Put_Line ("Going to Generation");
      -- Reflex.Expansion.Do_Expansion;
      --  Populate AREC for Subprogram
      
      Reflex.Gen.Ada_Outputs.Comment_Type := 
	Reflex.Gen.Ada_Outputs.Ada_Comment_Type;
      
      RxAda.Generator.Do_Generation;

      --  Once the backend is complete, we unlock the names table. This
      --  call allows a few extra entries, needed for example for the file
      --  name for the library file output.

      Namet.Unlock;

      --  Validate unchecked conversions (using the values for size
      --  and alignment annotated by the backend where possible).

      -- Sem_Ch13.Validate_Unchecked_Conversions;

      --  Now we complete output of errors, rep info and the tree info.
      --  These are delayed till now, since it is perfectly possible for
      --  gigi to generate errors, modify the tree (in particular by setting
      --  flags indicating that elaboration is required, and also to back
      --  annotate representation information for List_Rep_Info.

      Errout.Finalize;
      List_Rep_Info;

      --  Only write the library if the backend did not generate any error
      --  messages. Otherwise signal errors to the driver program so that
      --  there will be no attempt to generate an object file.

      if Compilation_Errors then
         Exit_Program (E_Errors);
      end if;

      --  Finalize name table and we are all done

      Namet.Finalize;

   exception
      --  Handle fatal internal compiler errors

      when System.Assertions.Assert_Failure =>
         Comperr.Compiler_Abort ("Assert_Failure");

      when Constraint_Error =>
         Comperr.Compiler_Abort ("Constraint_Error");

      when Program_Error =>
         Comperr.Compiler_Abort ("Program_Error");

      when Storage_Error =>

         --  Assume this is a bug. If it is real, the message will in
         --  any case say Storage_Error, giving a strong hint!

         Comperr.Compiler_Abort ("Storage_Error");
   end;

--  The outer exception handles an unrecoverable error

exception
   when Unrecoverable_Error =>
      Errout.Finalize;

      Set_Standard_Error;
      Write_Str ("compilation abandoned");
      Write_Eol;

      Set_Standard_Output;
      Exit_Program (E_Errors);

end Reflex2Ada;
