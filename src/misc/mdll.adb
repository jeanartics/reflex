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

--  This package provides the core high level routines used by GNATDLL
--  to build Windows DLL

with Ada.Text_IO;

with GNAT.Directory_Operations;
with MDLL.Utl;
with MDLL.Fil;

package body MDLL is

   use Ada;
   use GNAT;

   ---------------------------
   -- Build_Dynamic_Library --
   ---------------------------

   procedure Build_Dynamic_Library
     (Ofiles        : Argument_List;
      Afiles        : Argument_List;
      Options       : Argument_List;
      Bargs_Options : Argument_List;
      Largs_Options : Argument_List;
      Lib_Filename  : String;
      Def_Filename  : String;
      Lib_Address   : String  := "";
      Build_Import  : Boolean := False;
      Relocatable   : Boolean := False)
   is

      use type OS_Lib.Argument_List;

      Base_Filename : constant String := MDLL.Fil.Ext_To (Lib_Filename);

      Def_File : aliased constant String := Def_Filename;
      Jnk_File : aliased          String := Base_Filename & ".jnk";
      Bas_File : aliased constant String := Base_Filename & ".base";
      Dll_File : aliased          String := Base_Filename & ".dll";
      Exp_File : aliased          String := Base_Filename & ".exp";
      Lib_File : aliased constant String := "lib" & Base_Filename & ".a";

      Bas_Opt  : aliased String := "-Wl,--base-file," & Bas_File;
      Lib_Opt  : aliased String := "-mdll";
      Out_Opt  : aliased String := "-o";
      Adr_Opt  : aliased String := "-Wl,--image-base=" & Lib_Address;

      L_Afiles : Argument_List := Afiles;
      --  Local afiles list. This list can be reordered to ensure that the
      --  binder ali file is not the first entry in this list.

      All_Options : constant Argument_List := Options & Largs_Options;

      procedure Build_Reloc_DLL;
      --  Build a relocatable DLL with only objects file specified.
      --  this use the well known 5 steps build. (see GNAT User's Guide).

      procedure Ada_Build_Reloc_DLL;
      --  Build a relocatable DLL with Ada code.
      --  this use the well known 5 steps build. (see GNAT User's Guide).

      procedure Build_Non_Reloc_DLL;
      --  Build a non relocatable DLL containing no Ada code.

      procedure Ada_Build_Non_Reloc_DLL;
      --  Build a non relocatable DLL with Ada code.

      ---------------------
      -- Build_Reloc_DLL --
      ---------------------

      procedure Build_Reloc_DLL is
         --  Objects plus the export table (.exp) file

         Objects_Exp_File : constant OS_Lib.Argument_List
           := Exp_File'Unchecked_Access & Ofiles;

         Success : Boolean;

      begin
         if not Quiet then
            Text_IO.Put_Line ("building relocatable DLL...");
            Text_IO.Put ("make " & Dll_File);

            if Build_Import then
               Text_IO.Put_Line (" and " & Lib_File);
            else
               Text_IO.New_Line;
            end if;
         end if;

         --  1) Build base file with objects files.

         Utl.Gcc (Output_File => Jnk_File,
                  Files       => Ofiles,
                  Options     => All_Options,
                  Base_File   => Bas_File,
                  Build_Lib   => True);

         --  2) Build exp from base file.

         Utl.Dlltool (Def_File, Dll_File, Lib_File,
                      Base_File    => Bas_File,
                      Exp_Table    => Exp_File,
                      Build_Import => False);

         --  3) Build base file with exp file and objects files.

         Utl.Gcc (Output_File => Jnk_File,
                  Files       => Objects_Exp_File,
                  Options     => All_Options,
                  Base_File   => Bas_File,
                  Build_Lib   => True);

         --  4) Build new exp from base file and the lib file (.a)

         Utl.Dlltool (Def_File, Dll_File, Lib_File,
                      Base_File    => Bas_File,
                      Exp_Table    => Exp_File,
                      Build_Import => Build_Import);

         --  5) Build the dynamic library

         Utl.Gcc (Output_File => Dll_File,
                  Files       => Objects_Exp_File,
                  Options     => Adr_Opt'Unchecked_Access & All_Options,
                  Build_Lib   => True);

         OS_Lib.Delete_File (Exp_File, Success);
         OS_Lib.Delete_File (Bas_File, Success);
         OS_Lib.Delete_File (Jnk_File, Success);

      exception
         when others =>
            OS_Lib.Delete_File (Exp_File, Success);
            OS_Lib.Delete_File (Bas_File, Success);
            OS_Lib.Delete_File (Jnk_File, Success);
            raise;
      end Build_Reloc_DLL;

      -------------------------
      -- Ada_Build_Reloc_DLL --
      -------------------------

      procedure Ada_Build_Reloc_DLL is
         Success : Boolean;
      begin
         if not Quiet then
            Text_IO.Put_Line ("Building relocatable DLL...");
            Text_IO.Put ("make " & Dll_File);

            if Build_Import then
               Text_IO.Put_Line (" and " & Lib_File);
            else
               Text_IO.New_Line;
            end if;
         end if;

         --  1) Build base file with objects files.

         Utl.Gnatbind (L_Afiles, Options & Bargs_Options);

         declare
            Params : constant OS_Lib.Argument_List :=
                       Out_Opt'Unchecked_Access &
                       Jnk_File'Unchecked_Access &
                       Lib_Opt'Unchecked_Access &
                       Bas_Opt'Unchecked_Access &
                       Ofiles &
                       All_Options;
         begin
            Utl.Gnatlink (L_Afiles (L_Afiles'Last).all, Params);
         end;

         --  2) Build exp from base file.

         Utl.Dlltool (Def_File, Dll_File, Lib_File,
                      Base_File    => Bas_File,
                      Exp_Table    => Exp_File,
                      Build_Import => False);

         --  3) Build base file with exp file and objects files.

         Utl.Gnatbind (L_Afiles, Options & Bargs_Options);

         declare
            Params : constant OS_Lib.Argument_List :=
                       Out_Opt'Unchecked_Access &
                       Jnk_File'Unchecked_Access &
                       Lib_Opt'Unchecked_Access &
                       Bas_Opt'Unchecked_Access &
                       Exp_File'Unchecked_Access &
                       Ofiles &
                       All_Options;
         begin
            Utl.Gnatlink (L_Afiles (L_Afiles'Last).all, Params);
         end;

         --  4) Build new exp from base file and the lib file (.a)

         Utl.Dlltool (Def_File, Dll_File, Lib_File,
                      Base_File    => Bas_File,
                      Exp_Table    => Exp_File,
                      Build_Import => Build_Import);

         --  5) Build the dynamic library

         Utl.Gnatbind (L_Afiles, Options & Bargs_Options);

         declare
            Params : constant OS_Lib.Argument_List :=
                       Out_Opt'Unchecked_Access &
                       Dll_File'Unchecked_Access &
                       Lib_Opt'Unchecked_Access &
                       Exp_File'Unchecked_Access &
                       Adr_Opt'Unchecked_Access &
                       Ofiles &
                       All_Options;
         begin
            Utl.Gnatlink (L_Afiles (L_Afiles'Last).all, Params);
         end;

         OS_Lib.Delete_File (Exp_File, Success);
         OS_Lib.Delete_File (Bas_File, Success);
         OS_Lib.Delete_File (Jnk_File, Success);

      exception
         when others =>
            OS_Lib.Delete_File (Exp_File, Success);
            OS_Lib.Delete_File (Bas_File, Success);
            OS_Lib.Delete_File (Jnk_File, Success);
            raise;
      end Ada_Build_Reloc_DLL;

      -------------------------
      -- Build_Non_Reloc_DLL --
      -------------------------

      procedure Build_Non_Reloc_DLL is
         Success : Boolean;
      begin
         if not Quiet then
            Text_IO.Put_Line ("building non relocatable DLL...");
            Text_IO.Put ("make " & Dll_File &
                         " using address " & Lib_Address);

            if Build_Import then
               Text_IO.Put_Line (" and " & Lib_File);
            else
               Text_IO.New_Line;
            end if;
         end if;

         --  Build exp table and the lib .a file.

         Utl.Dlltool (Def_File, Dll_File, Lib_File,
                      Exp_Table    => Exp_File,
                      Build_Import => Build_Import);

         --  Build the DLL

         Utl.Gcc (Output_File => Dll_File,
                  Files       => Exp_File'Unchecked_Access & Ofiles,
                  Options     => Adr_Opt'Unchecked_Access & All_Options,
                  Build_Lib   => True);

         OS_Lib.Delete_File (Exp_File, Success);

      exception
         when others =>
            OS_Lib.Delete_File (Exp_File, Success);
            raise;
      end Build_Non_Reloc_DLL;

      -----------------------------
      -- Ada_Build_Non_Reloc_DLL --
      -----------------------------

      --  Build a non relocatable DLL with Ada code.

      procedure Ada_Build_Non_Reloc_DLL is
         Success : Boolean;
      begin
         if not Quiet then
            Text_IO.Put_Line ("building non relocatable DLL...");
            Text_IO.Put ("make " & Dll_File &
                         " using address " & Lib_Address);

            if Build_Import then
               Text_IO.Put_Line (" and " & Lib_File);
            else
               Text_IO.New_Line;
            end if;
         end if;

         --  Build exp table and the lib .a file.

         Utl.Dlltool (Def_File, Dll_File, Lib_File,
                      Exp_Table    => Exp_File,
                      Build_Import => Build_Import);

         --  Build the DLL

         Utl.Gnatbind (L_Afiles, Options & Bargs_Options);

         declare
            Params : constant OS_Lib.Argument_List :=
                       Out_Opt'Unchecked_Access &
                       Dll_File'Unchecked_Access &
                       Lib_Opt'Unchecked_Access &
                       Exp_File'Unchecked_Access &
                       Adr_Opt'Unchecked_Access &
                       Ofiles &
                       All_Options;
         begin
            Utl.Gnatlink (L_Afiles (L_Afiles'Last).all, Params);
         end;

         OS_Lib.Delete_File (Exp_File, Success);

      exception
         when others =>
            OS_Lib.Delete_File (Exp_File, Success);
            raise;
      end Ada_Build_Non_Reloc_DLL;

   begin
      --  On Windows the binder file must not be in the first position
      --  in the list. This is due to the way DLL's are built on Windows.
      --  We swap the first ali with the last one if it is the case.

      if L_Afiles'Length > 1 then
         declare
            Filename : constant String :=
                         Directory_Operations.Base_Name (L_Afiles (1).all);
            First    : constant Positive := Filename'First;

         begin
            if Filename (First .. First + 1) = "b~" then
               L_Afiles (L_Afiles'Last) := Afiles (1);
               L_Afiles (1) := Afiles (Afiles'Last);
            end if;
         end;
      end if;

      case Relocatable is

         when True =>
            if L_Afiles'Length = 0 then
               Build_Reloc_DLL;
            else
               Ada_Build_Reloc_DLL;
            end if;

         when False =>
            if L_Afiles'Length = 0 then
               Build_Non_Reloc_DLL;
            else
               Ada_Build_Non_Reloc_DLL;
            end if;

      end case;
   end Build_Dynamic_Library;

   --------------------------
   -- Build_Import_Library --
   --------------------------

   procedure Build_Import_Library
     (Lib_Filename : String;
      Def_Filename : String)
   is

      procedure Build_Import_Library (Def_Base_Filename : String);
      --  Build an import library.
      --  this is to build only a .a library to link against a DLL.

      Base_Filename : constant String := MDLL.Fil.Ext_To (Lib_Filename);

      --------------------------
      -- Build_Import_Library --
      --------------------------

      procedure Build_Import_Library (Def_Base_Filename : String) is

         Def_File : String renames Def_Filename;
         Dll_File : constant String := Def_Base_Filename & ".dll";
         Lib_File : constant String := "lib" & Base_Filename & ".a";

      begin

         if not Quiet then
            Text_IO.Put_Line ("Building import library...");
            Text_IO.Put_Line ("make " & Lib_File &
                              " to use dynamic library " & Dll_File);
         end if;

         Utl.Dlltool (Def_File, Dll_File, Lib_File,
                      Build_Import => True);
      end Build_Import_Library;

   begin
      --  If the library has the form lib<name>.a then the def file should
      --  be <name>.def and the DLL to link against <name>.dll
      --  this is a Windows convention and we try as much as possible to
      --  follow the platform convention.

      if Lib_Filename'Length > 3 and then Lib_Filename (1 .. 3) = "lib" then
         Build_Import_Library (Base_Filename (4 .. Base_Filename'Last));
      else
         Build_Import_Library (Base_Filename);
      end if;
   end Build_Import_Library;

end MDLL;
