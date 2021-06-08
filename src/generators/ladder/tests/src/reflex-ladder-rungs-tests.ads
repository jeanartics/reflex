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
-- Reflex is originally developed by Artics
------------------------------------------------------------------------------

with AUnit; use AUnit;
with AUnit.Test_Cases;

package Reflex.Ladder.Rungs.Tests is
   
   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   --  Override:

   --  Register routines to be run:
   procedure Register_Tests (T : in out Test_Case);

   --  Provide name identifying the test case:
   function Name (T : Test_Case) return Message_String;

   --  Override if needed. Default empty implementations provided:

   --  Preparation performed before each routine:
   procedure Set_Up (T : in out Test_Case);

   --  Cleanup performed after each routine:
   procedure Tear_Down (T :  in out Test_Case);
   
   procedure Test_Initialize      (R : in out AUnit.Test_Cases.Test_Case'Class);
   
end Reflex.Ladder.Rungs.Tests;
