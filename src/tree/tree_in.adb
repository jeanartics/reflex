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

with Atree;
with Csets;
with Elists;
with Fname;
with Lib;
with Namet;
with Nlists;
with Opt;
with Repinfo;
with Sinput;
with Stand;
with Stringt;
with Tree_IO;
with Uintp;
with Urealp;

procedure Tree_In (Desc : File_Descriptor) is
begin
   Tree_IO.Tree_Read_Initialize (Desc);
   Opt.Tree_Read;
   Atree.Tree_Read;
   Elists.Tree_Read;
   Fname.Tree_Read;
   Lib.Tree_Read;
   Namet.Tree_Read;
   Nlists.Tree_Read;
   Sinput.Tree_Read;
   Stand.Tree_Read;
   Stringt.Tree_Read;
   Uintp.Tree_Read;
   Urealp.Tree_Read;
   Repinfo.Tree_Read;
   Csets.Initialize;
end Tree_In;
