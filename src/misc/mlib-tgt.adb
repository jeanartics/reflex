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

--  This is the default version which does not support libraries.
--  All subprograms are dummies, because they are never called,
--  except Support_For_Libraries which returns None.

package body MLib.Tgt is

   ---------------------
   -- Archive_Builder --
   ---------------------

   function Archive_Builder return String is
   begin
      return "ar";
   end Archive_Builder;

   -----------------------------
   -- Archive_Builder_Options --
   -----------------------------

   function Archive_Builder_Options return String_List_Access is
   begin
      return new String_List'(1 => new String'("cr"));
   end Archive_Builder_Options;

   -----------------
   -- Archive_Ext --
   -----------------

   function Archive_Ext return String is
   begin
      return "";
   end Archive_Ext;

   ---------------------
   -- Archive_Indexer --
   ---------------------

   function Archive_Indexer return String is
   begin
      return "ranlib";
   end Archive_Indexer;

   ---------------------------
   -- Build_Dynamic_Library --
   ---------------------------

   procedure Build_Dynamic_Library
     (Ofiles       : Argument_List;
      Foreign      : Argument_List;
      Afiles       : Argument_List;
      Options      : Argument_List;
      Interfaces   : Argument_List;
      Lib_Filename : String;
      Lib_Dir      : String;
      Symbol_Data  : Symbol_Record;
      Driver_Name  : Name_Id := No_Name;
      Lib_Address  : String  := "";
      Lib_Version  : String  := "";
      Relocatable  : Boolean := False;
      Auto_Init    : Boolean := False)
   is
      pragma Unreferenced (Ofiles);
      pragma Unreferenced (Foreign);
      pragma Unreferenced (Afiles);
      pragma Unreferenced (Options);
      pragma Unreferenced (Interfaces);
      pragma Unreferenced (Lib_Filename);
      pragma Unreferenced (Lib_Dir);
      pragma Unreferenced (Symbol_Data);
      pragma Unreferenced (Driver_Name);
      pragma Unreferenced (Lib_Address);
      pragma Unreferenced (Lib_Version);
      pragma Unreferenced (Relocatable);
      pragma Unreferenced (Auto_Init);

   begin
      null;
   end Build_Dynamic_Library;

   -------------------------
   -- Default_DLL_Address --
   -------------------------

   function Default_DLL_Address return String is
   begin
      return "";
   end Default_DLL_Address;

   -------------
   -- DLL_Ext --
   -------------

   function DLL_Ext return String is
   begin
      return "";
   end DLL_Ext;

   --------------------
   -- Dynamic_Option --
   --------------------

   function Dynamic_Option return String is
   begin
      return "";
   end Dynamic_Option;

   -------------------
   -- Is_Object_Ext --
   -------------------

   function Is_Object_Ext (Ext : String) return Boolean is
      pragma Unreferenced (Ext);
   begin
      return False;
   end Is_Object_Ext;

   --------------
   -- Is_C_Ext --
   --------------

   function Is_C_Ext (Ext : String) return Boolean is
      pragma Unreferenced (Ext);
   begin
      return False;
   end Is_C_Ext;

   --------------------
   -- Is_Archive_Ext --
   --------------------

   function Is_Archive_Ext (Ext : String) return Boolean is
      pragma Unreferenced (Ext);
   begin
      return False;
   end Is_Archive_Ext;

   -------------
   -- Libgnat --
   -------------

   function Libgnat return String is
   begin
      return "libgnat.a";
   end Libgnat;

   ------------------------
   -- Library_Exists_For --
   ------------------------

   function Library_Exists_For (Project : Project_Id) return Boolean is
      pragma Unreferenced (Project);
   begin
      return False;
   end Library_Exists_For;

   ---------------------------
   -- Library_File_Name_For --
   ---------------------------

   function Library_File_Name_For (Project : Project_Id) return Name_Id is
      pragma Unreferenced (Project);
   begin
      return No_Name;
   end Library_File_Name_For;

   --------------------------------
   -- Linker_Library_Path_Option --
   --------------------------------

   function Linker_Library_Path_Option return String_Access is
   begin
      return null;
   end Linker_Library_Path_Option;

   ----------------
   -- Object_Ext --
   ----------------

   function Object_Ext return String is
   begin
      return "";
   end Object_Ext;

   ----------------
   -- PIC_Option --
   ----------------

   function PIC_Option return String is
   begin
      return "";
   end PIC_Option;

   -----------------------------------------------
   -- Standalone_Library_Auto_Init_Is_Supported --
   -----------------------------------------------

   function Standalone_Library_Auto_Init_Is_Supported return Boolean is
   begin
      return False;
   end Standalone_Library_Auto_Init_Is_Supported;

   ---------------------------
   -- Support_For_Libraries --
   ---------------------------

   function Support_For_Libraries return Library_Support is
   begin
      return None;
   end Support_For_Libraries;

end MLib.Tgt;
