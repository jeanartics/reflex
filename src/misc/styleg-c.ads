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

--  This generic package, instantiated in package Style, contains routines
--  used by the compiler for style checking. These routines are in a separate
--  package because they depend on the GNAT tree (Atree, Sinfo, ...).

with Types;  use Types;

generic
   with procedure Error_Msg_N (Msg : String; N : Node_Or_Entity_Id);
   --  Output a message at the Sloc of the given node

package Styleg.C is

   procedure Body_With_No_Spec (N : Node_Id);
   --  Called where N is a subprogram body node for a subprogram body
   --  for which no spec was given, i.e. a body acting as its own spec.

   procedure Check_Identifier
     (Ref : Node_Or_Entity_Id;
      Def : Node_Or_Entity_Id);
   --  Check style of identifier occurrence. Ref is an N_Identifier node whose
   --  spelling is to be checked against the Chars spelling in identifier node
   --  Def (which may be either an N_Identifier, or N_Defining_Identifier node)

   procedure Subprogram_Not_In_Alpha_Order (Name : Node_Id);
   --  Called if Name is the name of a subprogram body in a package body
   --  that is not in alphabetical order.

end Styleg.C;
