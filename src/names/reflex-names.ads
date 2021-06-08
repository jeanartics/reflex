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

with Types; use Types;
with Namet; use Namet;

package Reflex.Names is

   Name_Reflex       : Name_Id; -- "reflex"
   Name_Generate     : Name_Id; -- "generate"
   Name_Plc_Lang     : Name_Id; -- "lang"
   Name_Plc_Comment  : Name_Id; -- "comment"
   Name_Plc_ST       : Name_Id; -- "st"
   Name_Plc_Ladder   : Name_Id; -- "ladder"
   Name_Plc_Fbd      : Name_Id; -- "fbd"
   Name_Plc_Graph    : Name_Id; -- "graph"
   Name_Plc_Section  : Name_Id; -- "section"
   Name_Plc_SR       : Name_Id; -- "sr"
   Name_Plc_DFB      : Name_Id; -- "dfb"
   Name_Plc_Extern   : Name_Id; -- "plc-extern"
   Name_Plc_Import   : Name_Id; -- "plc-import"
   Name_Plc_Unity    : Name_Id; -- "unity"
   Name_Plc_TIA      : Name_Id; -- "tia"
   Name_Plc_Codesys  : Name_Id; -- "codesys"

   type Plc_Attribute_Type is
     (Map,
      Comment,
      Address,
      Lang,
      Elim);

   procedure Initialize;

end Reflex.Names;
