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

with Ada.Text_Io; use Ada.Text_IO;

with Gnat.Os_Lib; use Gnat.Os_Lib;

with Atree; use Atree;
with Einfo; use Einfo;
with Fname; use Fname;
with Lib; use Lib;
with Opt; use Opt;
with Namet; use Namet;
with Sinfo; use Sinfo;
with Types; use Types;
with Uname; use Uname;
with Sem; use Sem;
with Errout;   use Errout;
with Sem_Ch8;  use Sem_Ch8;
with Stand;    use Stand;

with Artics.Strings_Stocks; use Artics.Strings_Stocks;

with Reflex_Options;

with Reflex.Infos; use Reflex.Infos;
with Reflex.Configs.Entities_Renames; use Reflex.Configs.Entities_Renames;

with Reflex.Expanders; use Reflex.Expanders;
with Reflex.Expanders.Types; use Reflex.Expanders.Types;
with Reflex.Expanders.Dispatch;
with Reflex.Expanders.Utils;
with Reflex.Expanders.Expressions;

with Reflex.Global_Arecs; use Reflex.Global_Arecs;
with Reflex.Configs.Vars; use Reflex.Configs.Vars;
with Reflex.Configs.Subps; use Reflex.Configs.Subps;
with Reflex.Entities_Lists;

package body Reflex.Expansion is
   
   Outer_Generic_Scope : Entity_Id := Empty;
   --  Global reference to the outer scope that is generic. In a non
   --  generic context, it is empty. At the moment, it is only used
   --  for avoiding freezing of external references in generics.
   
   function To_Unit_Name (S : String) return String;
   procedure Dump_Entities (U : Unit_Number_Type);
   
   ----------------------------
   -- Expand_Specification --
   ----------------------------
   
   procedure Expand_Specification
     (This           : access Reflex_Expander_Record;
      Spec_Comp_Unit : Node_Id) is
      
      Current_Unit : Unit_Number_Type;
   begin
      --  Expand Specs.
      
      Current_Unit := Get_Cunit_Unit_Number (Spec_Comp_Unit);
      This.Set_Main_Node (Spec_Comp_Unit);
      This.Set_Current_Source_File (Source_Index (Current_Unit));
      
      Reflex.Expanders.Dispatch.Expand_Node (This, Spec_Comp_Unit);
      
   end Expand_Specification;
   
   -------------------
   -- Expand_Body --
   -------------------
   
   procedure Expand_Body
     (This           : access Reflex_Expander_Record;
      Body_Comp_Unit : Node_Id) is
      
      Current_Unit : Unit_Number_Type;
   begin
      --  Expand Body.
      
      Current_Unit := Get_Cunit_Unit_Number (Body_Comp_Unit);
      This.Set_Main_Node (Body_Comp_Unit);
      This.Set_Current_Source_File (Source_Index (Current_Unit));
      
      Reflex.Expanders.Dispatch.Expand_Node (This, Body_Comp_Unit);
      
   end Expand_Body;
   
   ------------------
   -- Do_Expansion --
   ------------------
   
   procedure Do_Expansion is
      
      Comp_Unit        : Node_Id;
      Comp_Node        : Node_Id;
      Body_Comp_Unit   : Node_Id;
      Body_Node        : Node_Id;
      Spec_Comp_Unit   : Node_Id;
      Spec_Node        : Node_Id;
      Parent_Comp_Unit : Node_Id;
      Parent_Unit      : Unit_Number_Type;
      Reflex_Expander  : access Reflex_Expander_Record;
   begin
      Put_Line ("EXPANSION");
      Reflex.Expanders.Types.Initialize;
      Artics.Strings_Stocks.Initialize;
      Reflex.Infos.Initialize_Reflex_Infos;
      
      if Is_Regular_File ("vars.csv") then
	 Reflex.Configs.Vars.Parse_Vars ("vars.csv");      
      end if;
      
      if Is_Regular_File ("subps.csv") then
	 Reflex.Configs.Subps.Parse_Subps ("subps.csv");
      end if;
      
      Reflex_Options.Generate_Enum_As_Constants := True;
      
      --  for U in 1..Last_Unit loop
      --  	 if not Is_Predefined_File_Name (Unit_File_Name (U)) then
      --  	    Dump_Entities (U);
      --  	 end if;
      --  end loop;
      
      --  Dump file

      --  Expand Glips code for this unit. First Expand the specs and then
      --  Expand the body in one file named "file_name.glips"
      
      Comp_Unit := Cunit (Main_Unit); 
      Comp_Node := Unit (Comp_Unit);
      
      if Is_Expanded (Comp_Unit) 
	or else Is_Expansion_Pending (Comp_Unit)
      then
	 return;
      end if;
      
      Set_Expansion_Pending (Comp_Unit, True);
      
      --  If the main compilation unit is a package or subprogram declaration
      --  then no body to Expand.
      
      if Nkind (Comp_Node) = N_Package_Declaration 
	or else Nkind (Comp_Node) = N_Subprogram_Declaration 
      then
	 if Present (Parent_Spec (Comp_Node)) then
	    Parent_Comp_Unit := Parent_Spec (Comp_Node);
	    Parent_Unit := Get_Cunit_Unit_Number (Parent_Comp_Unit);
	    Do_Unit_Expansion (Parent_Unit);
	 end if;
	    
	 Body_Comp_Unit := Empty;
	 Body_Node      := Empty;
	 Spec_Comp_Unit := Comp_Unit;
	 Spec_Node      := Comp_Node;
      else
	 Body_Comp_Unit := Comp_Unit;
	 Body_Node      := Comp_Node;
	 Spec_Comp_Unit := Library_Unit (Body_comp_Unit);
	 Spec_Node      := Unit (Spec_Comp_Unit);
      end if;
      
      --  Put_Line ("Unit Name      => " & Name_Buffer (1..Name_Len));
      --  Put_Line ("Body_Comp_Unit => " & Nkind (Body_Comp_Unit)'Img);
      --  Put_Line ("Body_Node      => " & Nkind (Body_Node)'Img);
      --  Put_Line ("Spec_Comp_Unit => " & Nkind (Spec_Comp_Unit)'Img);
      --  Put_Line ("Sepc_Node      => " & Nkind (Spec_Node)'Img);
      
      -- Create Expander 
      
      Reflex_Expander := New_Reflex_Expander;
      
      --  Set_Stack
      
      --  Expand Sepcification.
	 
      if Present (Spec_Comp_Unit) then
	 Expand_Specification (Reflex_Expander, Spec_Comp_Unit);	 
      end if;
      
      --  Expand Body.
	 
      if Present (Body_Comp_Unit) then
	 Expand_Body (Reflex_Expander, Body_Comp_Unit);	 
      end if;
      
      --  AREC Post exansion 
      declare
	 Entities : Reflex.Entities_Lists.List;
      begin
	 Entities := Reflex_Expander.Get_Arec_Subprogram_List;
	 for E of Entities loop
	   Populate_Arec_Entities (E);
	 end loop;
      end;	 
      
      Set_Expanded (Comp_Unit, True);
      
   end Do_Expansion;
   
   -------------------
   -- Dump_Entities --
   -------------------
   
   procedure Dump_Entities (U : Unit_Number_Type) is
      Eunit : Entity_Id;
      E     : Entity_Id;
      Ebody : Entity_Id;
   begin
      Eunit := Cunit_Entity (U);
      
      E := First_Entity (Eunit);
      while Present (E) loop
	 Next_Entity (E);
      end loop;
      
      if Ekind (Eunit) = E_Package then
	 Ebody := Body_Entity (Eunit);
	 
	 E := First_Entity (Ebody);
	 while Present (E) loop
	 Next_Entity (E);
	 end loop;
      end if;
   end Dump_Entities;
   
   ------------------
   -- To_Unit_Name --
   ------------------
   
   function To_Unit_Name (S : String) return String is
   begin
      if S'Length > 2 then
	 return S(S'First..S'Last-2);
      else
	 return "";
      end if;
   end To_Unit_Name;
   
   -----------------------
   -- Do_Unit_Expansion --
   -----------------------
   
   procedure Do_Unit_Expansion (U : Unit_Number_Type) is
      
      Comp_Unit      : Node_Id;
      Comp_Node      : Node_Id;
      Body_Comp_Unit : Node_Id;
      Body_Node      : Node_Id;
      Spec_Comp_Unit : Node_Id;
      Spec_Node      : Node_Id;
      
      Parent_Comp_Unit : Node_Id;
      Parent_Unit      : Unit_Number_Type;
      Reflex_Expander  : access Reflex_Expander_Record;
   begin
      Put_Line ("UNIT EXPANSION");
      --  Dump file
      
      --  Put_Line
      --  	("---> Unit Name ==> " & Get_Name_String (Unit_Name (U)));

      --  Expand Glips code for this unit. First Expand the specs and then
      --  Expand the body in one file named "file_name.glips"
      
      Comp_Unit := Cunit (U); 
      Comp_Node := Unit (Comp_Unit);
      
      if Is_Expanded (Comp_Unit) 
	or else Is_Expansion_Pending (Comp_Unit)
      then
	 return;
      end if;
      
      Set_Expansion_Pending (Comp_Unit, True);
      
      --  If the main compilation unit is a package or subprogram declaration
      --  then no body to Expand.
      
      if Nkind (Comp_Node) = N_Package_Declaration 
	or else Nkind (Comp_Node) = N_Subprogram_Declaration 
      then
	 if Present (Parent_Spec (Comp_Node)) then
	    if not In_Predefined_Unit (Comp_Node) 
	      and then not Is_Expanded (Comp_Node) 
	    then
	       Parent_Comp_Unit := Parent_Spec (Comp_Node);
	       Parent_Unit := Get_Cunit_Unit_Number (Parent_Comp_Unit);
	       Do_Unit_Expansion (Parent_Unit);
	    end if;
	 end if;
	    
	 Spec_Comp_Unit := Comp_Unit;
	 Spec_Node      := Comp_Node;
	 
	 if Present (Library_Unit (Spec_Comp_Unit)) then
	    Body_Comp_Unit := Library_Unit (Spec_Comp_Unit);
	    Body_Node      := Unit (Body_Comp_Unit);
	 else
	    Body_Comp_Unit := Empty;
	    Body_Node      := Empty;
	 end if;
	 
      else
	 Body_Comp_Unit := Comp_Unit;
	 Body_Node      := Comp_Node;
	 Spec_Comp_Unit := Library_Unit (Body_comp_Unit);
	 Spec_Node      := Unit (Spec_Comp_Unit);
      end if;
      
      Get_External_Unit_Name_String (Unit_Name (U));
      
      --  Put_Line
      --    ("Unit Expand: Unit Name      => " & Name_Buffer (1..Name_Len));
      --  Put_Line
      --    ("Unit Expand: Body_Comp_Unit => " & Nkind (Body_Comp_Unit)'Img);
      --  Put_Line
      --    ("Unit Expand: Body_Node      => " & Nkind (Body_Node)'Img);
      --  Put_Line
      --    ("Unit Expand: Spec_Comp_Unit => " & Nkind (Spec_Comp_Unit)'Img);
      --  Put_Line
      --    ("Unit Expand: Sepc_Node      => " & Nkind (Spec_Node)'Img);
      
      -- Create Expander
      
      declare
	 --  The following locations save the corresponding global flags and
	 --  variables so that they can be restored on completion. This is
	 --  needed so that calls to Rtsfind start with the proper default
	 --  values for these variables, and also that such calls do not
	 --  disturb the settings for units being analyzed at a higher level.
	 
	 S_Full_Analysis    : constant Boolean         := Full_Analysis;
	 S_In_Default_Expr  : constant Boolean         := In_Default_Expression;
	 S_Inside_A_Generic : constant Boolean         := Inside_A_Generic;
	 S_New_Nodes_OK     : constant Int             := New_Nodes_OK;
	 S_Outer_Gen_Scope  : constant Entity_Id       := Outer_Generic_Scope;
	 S_Sem_Unit         : constant Unit_Number_Type := Current_Sem_Unit;
	 
	 Generic_Main       : constant Boolean :=
	   Nkind (Unit (Cunit (Main_Unit)))
	   in N_Generic_Declaration;
	 
	 --  If the main unit is generic, every compiled unit, including its
	 --  context, is compiled with expansion disabled.

	 Save_Config_Switches : Config_Switches_Type;
	 --  Variable used to save values of config switches while we analyze
	 --  the new unit, to be restored on exit for proper recursive behavior.
	 
      begin
	 Compiler_State        := Analyzing;
	 Current_Sem_Unit      := Get_Cunit_Unit_Number (Comp_Unit);
	 
	 --        if Generic_Main then
	 --           Expander_Mode_Save_And_Set (False);
	 --        else
	 --           Expander_Mode_Save_And_Set
	 --             (Operating_Mode = Generate_Code or Debug_Flag_X);
	 --        end if;
	 
	 Full_Analysis         := True;
	 Inside_A_Generic      := False;
	 In_Default_Expression := False;
	 
	 Set_Comes_From_Source_Default (False);
	 Save_Opt_Config_Switches (Save_Config_Switches);
	 Set_Opt_Config_Switches
	   (Is_Internal_File_Name (Unit_File_Name (Current_Sem_Unit)));
	 
	 
	 Reflex_Expander := New_Reflex_Expander;
	 
	 --  Save Stack
	 --  Set New Stack
	 
	 --  Expand Specification
	 
	 if Present (Spec_Comp_Unit) then
	    Expand_Specification (Reflex_Expander, Spec_Comp_Unit);	 
	 end if;
	 
	 --  Expand Body.
	 
	 if Present (Body_Comp_Unit) then
	    Expand_Body (Reflex_Expander, Body_Comp_Unit);	 
	 end if;
	 
	 --  AREC Post exansion 
	 
	 --  Checks for circular dependency and set Need_Arec flags
	 
	 declare
	    Entities : Reflex.Entities_Lists.List;
	 begin
	    Entities := Reflex_Expander.Get_Arec_Subprogram_List;
	    for E of Entities loop
	       null; -- Populate_Arec_Entities (E);
	    end loop;
	 end;	 
	 
	 declare
	    Entities : Reflex.Entities_Lists.List;
	 begin
	    Entities := Reflex_Expander.Get_Arec_Subprogram_List;
	    for E of Entities loop
	       Populate_Arec_Entities (E);
	    end loop;
	 end;	 
	 
	 --  Save indication of dynamic elaboration checks for ALI file
	 
	 Set_Dynamic_Elab (Current_Sem_Unit, Dynamic_Elaboration_Checks);
	 
	 --  Restore settings of saved switches to entry values
	 
	 Current_Sem_Unit       := S_Sem_Unit;
	 Full_Analysis          := S_Full_Analysis;
	 In_Default_Expression  := S_In_Default_Expr;
	 Inside_A_Generic       := S_Inside_A_Generic;
	 New_Nodes_OK           := S_New_Nodes_OK;
	 Outer_Generic_Scope    := S_Outer_Gen_Scope;
	 
	 Restore_Opt_Config_Switches (Save_Config_Switches);
      end;
      
      Free_Reflex_Expander (Reflex_Expander_Ptr (Reflex_Expander));
      
      Set_Expanded (Comp_Unit, True);
      
   end Do_Unit_Expansion;
   
end Reflex.Expansion;
