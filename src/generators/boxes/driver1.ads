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

with Artics.Types;
with Artics.Namet;
with Artics.Uintp;
with Artics.Urealp;
with Artics.Stringt;
with Artics.Elmt_Nlists;

with Reflex.Boxes;
with Reflex.Boxes.Multis;
with Reflex.Boxes.Duals;
with Reflex.Boxes.Terminals;
with Reflex.Boxes.Coils;
with Reflex.Boxes.Enclosings;
with Reflex.Boxes.Ch2;
with Reflex.Boxes.Ch4;
with Reflex.Boxes.Ch5;
with Reflex.Boxes.Ch6;
with Reflex.Boxes.Dispatch;
with Reflex.Boxes.Exp_Ch5;
with Reflex.Boxes.Exp_Ch4;
with Reflex.Boxes.Ffb;
with Reflex.Boxes.Gen_Dispatch;

package Driver1 is

   procedure Init;
   procedure Run;

end Driver1;
