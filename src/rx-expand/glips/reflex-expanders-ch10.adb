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

with Ada.Text_Io; use Ada.Text_Io;

with Atree; use Atree;
with Einfo; use Einfo;
with Sinfo; use Sinfo;
with Nlists; use Nlists;
with Lib; use Lib;
with Sem_Util; use Sem_Util;
with Sem_Aux; use Sem_Aux;
with Types; use Types;

with Reflex.Expansion; use Reflex.Expansion;
with Reflex.Expanders.Supports; use Reflex.Expanders.Supports;
with Reflex.Expanders.Utils; use Reflex.Expanders.Utils;
with Reflex.Expanders.Dispatch; use Reflex.Expanders.Dispatch;

package body Reflex.Expanders.Ch10 is
   
   -------------------------------
   -- Expand_Compilation_Unit --
   -------------------------------
   
   procedure Expand_Compilation_Unit 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
   begin
      Expand_Node_List (This, Context_Items (Node));
      
      Expand_Opt_Node_List (This, Declarations (Aux_Decls_Node (Node)));
      
      Expand_Node (This, Unit (Node));
      
      if Present (Actions (Aux_Decls_Node (Node)))
	or else Present (Pragmas_After (Aux_Decls_Node (Node)))
      then
	 null;
      end if;
      
      Expand_Opt_Node_List (This, Actions (Aux_Decls_Node (Node)));
      Expand_Opt_Node_List (This, Pragmas_After (Aux_Decls_Node (Node)));
      
   end Expand_Compilation_Unit;
   
   -----------------------------------
   -- Expand_Compilation_Unit_Aux --
   -----------------------------------
   
   procedure Expand_Compilation_Unit_Aux 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Compilation_Unit_Aux;
   
   ---------------------------------
   -- Expand_Use_Package_Clause --
   ---------------------------------
   
   procedure Expand_Use_Package_Clause 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Use_Package_Clause;
   
   ------------------------------
   -- Expand_Use_Type_Clause --
   ------------------------------
   
   procedure Expand_Use_Type_Clause 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Use_Type_Clause;
   
   --------------------------
   -- Expand_With_Clause --
   --------------------------
   
   procedure Expand_With_Clause 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      U         : Unit_Number_Type;
      Comp_Unit : Node_Id;
      With_Unit : Node_Id;
      Spec      : Node_Id;
      Def_Id    : Entity_Id;
   begin
      if not Implicit_With (Node) 
---	and not Implicit_With_From_Instantiation (Node)
      then
	 Comp_Unit := Library_Unit (Node);
	 if not In_Predefined_Unit (Comp_Unit) then
	    U := Get_Cunit_Unit_Number (Comp_Unit);
	    Do_Unit_Expansion (U);
	 end if;
	 
	 With_Unit := Unit (Comp_Unit);
	 
	 if Nkind (With_Unit) = N_Package_Declaration then
	    Spec := Specification (With_Unit);
	    Def_Id := Defining_Entity (With_Unit);
	 else
	    null; --  raise Program_Error;
	 end if;
      end if;      
   end Expand_With_Clause;

end Reflex.Expanders.Ch10;
