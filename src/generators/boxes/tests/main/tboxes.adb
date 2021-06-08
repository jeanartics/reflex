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


with AUnit.Reporter.Text;with AUnit.Run;

with Reflex.Boxes_Suite;
with Reflex.Boxes.Duals_Suite;
with Reflex.Boxes.Multis_Suite;
with Reflex.Boxes.Terminals_Suite;
with Reflex.Boxes.Enclosings_Suite;

procedure Tboxes is

   procedure Run0 is new AUnit.Run.Test_Runner (Reflex.Boxes_Suite.Suite);
   Reporter0 : AUnit.Reporter.Text.Text_Reporter;
   --  Reporter : AUnit.Reporter.XML.XML_Reporter;

   procedure Run1 is new AUnit.Run.Test_Runner (Reflex.Boxes.Duals_Suite.Suite);
   Reporter1 : AUnit.Reporter.Text.Text_Reporter;
   --  Reporter : AUnit.Reporter.XML.XML_Reporter;

   procedure Run2 is new AUnit.Run.Test_Runner (Reflex.Boxes.Multis_Suite.Suite);
   Reporter2 : AUnit.Reporter.Text.Text_Reporter;
   --  Reporter : AUnit.Reporter.XML.XML_Reporter;

   procedure Run3 is new AUnit.Run.Test_Runner (Reflex.Boxes.Terminals_Suite.Suite);
   Reporter3 : AUnit.Reporter.Text.Text_Reporter;
   --  Reporter : AUnit.Reporter.XML.XML_Reporter;

   procedure Run4 is new AUnit.Run.Test_Runner (Reflex.Boxes.Enclosings_Suite.Suite);
   Reporter4 : AUnit.Reporter.Text.Text_Reporter;
   --  Reporter : AUnit.Reporter.XML.XML_Reporter;

begin
   Run0 (Reporter0);
   Run1 (Reporter1);
   Run2 (Reporter2);
   Run3 (Reporter3);
   Run4 (Reporter4);

end Tboxes;

