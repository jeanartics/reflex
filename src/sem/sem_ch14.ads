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

package Sem_Ch14 is

   function Analyze_Reactive_Specification (N : Node_id) return Entity_Id;
   procedure Analyze_Reactive_Declaration (N : Node_id);
   procedure Analyze_Reactive_Body (N : Node_Id);
   procedure Analyze_Reactive_Body_Description (N : Node_Id);
   procedure Analyze_Reactive_Reaction_Proc (N : Node_Id);
   procedure Analyze_Reactive_Flow_Proc (N : Node_Id);

   procedure Install_Reactive_Formals (Spec : Entity_Id);
   procedure Process_Reactive_Formals (T : List_Id);
   --  Process_Formals.
end Sem_Ch14;
