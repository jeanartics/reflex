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

--  This package contains global flags set by the initialization routine from
--  the command line and referenced throughout the compiler, the binder, or
--  other GNAT tools. The comments indicate which options are used by which
--  programs (GNAT, GNATBIND, GNATLINK, GNATMAKE, etc).

--  Some flags are labelled "PROJECT MANAGER". These are used by tools that
--  use the Project Manager. These tools include gnatmake, gnatname, the gnat
--  driver, gnatclean, gprbuild and gprclean.

with Hostparm; use Hostparm;
with Types;    use Types;

pragma Warnings (Off);
--  This package is used also by gnatcoll
with System.Strings; use System.Strings;
with System.WCh_Con; use System.WCh_Con;
pragma Warnings (On);

package Reflex_Options is

   type Plc_Target_Type is
     (Unknown,
      Unity_Target,
      Tia_Target,
      Codesys_Target,
      Iec131_Target);

   Plc_Target : Plc_Target_Type := Unity_Target;

   type Subprogram_Generation_Mode is
     (Unknown,
      As_Main,
      As_Dfb,
      As_Sr,
      As_Section,
      As_Inline);

   Generate_Enum_As_Constants : Boolean := True; -- False;

   Disable_Enum_Image_Tables : Boolean := True;

   Dfb_Globals_By_Ref : Boolean := True;

   type Case_Statement_Type is
     (Always_As_If,
      Always_As_Case,
      Range_As_If);

   Case_Statement_Generation : Case_Statement_Type := Always_As_Case;

   Max_Unity_Dfb_Parameter_Count : Natural := 32;

   Ladder_Language : Boolean := False; -- True;

   Max_Unity_Ladder_Horizontal : Natural := 11;
   Max_Unity_Ladder_Vertical   : Natural := 17;

   Max_Unity_Fbd_Horizontal : Natural := 360;
   Max_Unity_Fbd_Vertical   : Natural := 240;
   
   type Target_Infos_Record is record
      Plc_Target : Plc_Target_Type := Unity_Target;
      Generate_Enum_As_Constants : Boolean := True; -- False;
      
      Disable_Enum_Image_Tables : Boolean := True;
      
      Dfb_Globals_By_Ref : Boolean := True;
      
      Case_Statement_Generation : Case_Statement_Type := Always_As_Case;
      
      Max_Unity_Dfb_Parameter_Count : Natural := 32;
      
      Max_Unity_Ladder_Horizontal : Natural := 11;
      Max_Unity_Ladder_Vertical   : Natural := 17;
      
      Max_Unity_Fbd_Horizontal : Natural := 360;
      Max_Unity_Fbd_Vertical   : Natural := 240;
   end record;
   
   type Target_Lang_Type is
     (Unknown,
      St_Lang,
      Ladder_Lang,
      Fbd_Lang,
      Graph_Lang);
   
   type Entity_Infos_Record is record
      Dfb_Globals_By_Ref : Boolean := True;
      Lang : Target_Lang_Type;
   end record;
   
end Reflex_Options;
