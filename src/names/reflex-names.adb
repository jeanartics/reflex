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

package body Reflex.Names is
   
   procedure Initialize is
   begin
      Name_Reflex       := String_Find ("reflex");
      Name_Generate     := String_Find ("generate");
      Name_Plc_Lang     := String_Find ("lang");
      Name_Plc_Comment  := String_Find ("comment");
      Name_Plc_ST       := String_Find ("literal");
      Name_Plc_Ladder   := String_Find ("ladder");
      Name_Plc_Fbd      := String_Find ("fbd");
      Name_Plc_Graph    := String_Find ("graph");
      Name_Plc_Section  := String_Find ("section");
      Name_Plc_SR       := String_Find ("sr");
      Name_Plc_DFB      := String_Find ("dfb");
      Name_Plc_Extern   := String_Find ("plc-extern");
      Name_Plc_Import   := String_Find ("plc-import");
      Name_Plc_Unity    := String_Find ("unity");
      Name_Plc_TIA      := String_Find ("tia");
      Name_Plc_Codesys  := String_Find ("codesys");
      
   end Initialize;
   
end Reflex.Names;
