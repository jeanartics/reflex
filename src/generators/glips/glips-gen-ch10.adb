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

with artics.Buffers; use Artics.Buffers;
with Reflex.Gen.Outputs; use Reflex.Gen.Outputs;
with Reflex.Gen.Utils; use Reflex.Gen.Utils;
with Glips.Generator; use Glips.Generator;

package body Glips.Gen.Ch10 is
   
   -------------------------------
   -- Generate_Compilation_Unit --
   -------------------------------
   
   procedure Generate_Compilation_Unit 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob        : Output_Buffer := This.get_Output_Buffer;
      Unit_Node : Node_Id;
      Unit_Body : Node_Id;
      Spec      : Node_Id;
      Body_Node : Node_Id;
   begin
      --  Generate_Opt_Node_List (This, Declarations (Aux_Decls_Node (Node)));
      
      Unit_Node := Unit (Node);
      Put_Line ("*** Unit ===> " & Nkind (Unit_Node)'Img);
      
      if Nkind (Unit_Node) = N_Package_Declaration then
	 Generate_Opt_Node_List (This, Context_Items (Node));
	 Body_Node := Corresponding_Body (Unit_Node);
	 Put_Line ("*** Body_Node ===> " & Nkind (Body_Node)'Img);
	 Unit_Body := Parent (Corresponding_Body (Unit_Node));
	 Put_Line ("*** Unit_Body_Node ===> " & Nkind (Unit_Body)'Img);
	 
	 if Present (Unit_Body) then
	    Generate_Opt_Node_List (This, Context_Items (Parent (Parent (Unit_Body))));
	 end if;
	 
      elsif Nkind (Unit_Node) = N_Package_Body then
	 Spec := Corresponding_Spec (Unit_Node);
	 Put_Line ("*** Spec_Node ===> " & Nkind (Spec)'Img);
	 if No (Spec) then
	    Generate_Opt_Node_List (This, Context_Items (Node));
	 end if;
      else
	 Generate_Opt_Node_List (This, Context_Items (Node));
      end if;
      
      Generate_Node (This, Unit_Node);
      
      --  if Present (Actions (Aux_Decls_Node (Node))) then
      --  	 Generate_Opt_Node_List (This, Actions (Aux_Decls_Node (Node)));
      --  end if;
   end Generate_Compilation_Unit;
   
   -----------------------------------
   -- Generate_Compilation_Unit_Aux --
   -----------------------------------
   
   procedure Generate_Compilation_Unit_Aux 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Compilation_Unit_Aux;
   
   ---------------------------------
   -- Generate_Use_Package_Clause --
   ---------------------------------
   
   procedure Generate_Use_Package_Clause 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Use_Package_Clause;
   
   ------------------------------
   -- Generate_Use_Type_Clause --
   ------------------------------
   
   procedure Generate_Use_Type_Clause 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Use_Type_Clause;
   
   --------------------------
   -- Generate_With_Clause --
   --------------------------
   
   procedure Generate_With_Clause 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob        : Output_Buffer := This.Get_Output_Buffer;
      U         : Unit_Number_Type;
      Comp_Unit : Node_Id;
      With_Unit : Node_Id;
      Spec      : Node_Id;
      Def_Id    : Entity_Id;
   begin
      if not Implicit_With (Node) 
	--and not Implicit_With_From_Instantiation (Node)
      then
	 Comp_Unit := Library_Unit (Node);
	 if not In_Predefined_Unit (Comp_Unit) then
	    U := Get_Cunit_Unit_Number (Comp_Unit);
	    Do_Unit_Generation (U);
	 end if;
	 
	 Write_Comment_Line_To_Node (This, Node);
	 With_Unit := Unit (Comp_Unit);
	 
	 if Nkind (With_Unit) = N_Package_Declaration then
	    Spec := Specification (With_Unit);
	    Def_Id := Defining_Entity (With_Unit);
	    Write_Str (Ob, "with ");
	    Write_Id (Ob, Def_Id);
	    Write_Str (Ob, ";");
	    Write_Eol (Ob);
	 else
	    null; --  raise Program_Error;
	 end if;
      end if;
   end Generate_With_Clause;

end Glips.Gen.Ch10;
