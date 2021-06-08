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

package body Reflex.Coverings is
   
   Outer_Generic_Scope : Entity_Id := Empty;
   --  Global reference to the outer scope that is generic. In a non
   --  generic context, it is empty. At the moment, it is only used
   --  for avoiding freezing of external references in generics.
   
   function To_Unit_Name (S : String) return String;
   procedure Dump_Entities (U : Unit_Number_Type);
   
   ----------------------------
   -- Expand_Specification --
   ----------------------------
   
   procedure Cover_Specification
     (This           : access Reflex_Expander_Record;
      Spec_Comp_Unit : Node_Id) is
      
      Current_Unit : Unit_Number_Type;
   begin
      --  Expand Specs.
      
      Current_Unit := Get_Cunit_Unit_Number (Spec_Comp_Unit);
      This.Set_Main_Node (Spec_Comp_Unit);
      This.Set_Current_Source_File (Source_Index (Current_Unit));
      
      Reflex.Coverage.Cover_Node (This, Spec_Comp_Unit);
      
   end Cover_Specification;
   
   ----------------
   -- Cover_Body --
   ----------------
   
   procedure Cover_Body
     (This           : access Reflex_Expander_Record;
      Body_Comp_Unit : Node_Id) is
      
      Current_Unit : Unit_Number_Type;
   begin
      --  Expand Body.
      
      Current_Unit := Get_Cunit_Unit_Number (Body_Comp_Unit);
      This.Set_Main_Node (Body_Comp_Unit);
      This.Set_Current_Source_File (Source_Index (Current_Unit));
      
      Reflex.Coverage.Dispatch.Coverage_Node (This, Body_Comp_Unit);
      
   end Cover_Body;
   
   -----------------
   -- Do_Coverage --
   -----------------
   
   procedure Do_Coverage is
      
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
      Reflex.Coverage.Types.Initialize;
      
      --  Cover reflex code for this unit. First Cover the specs and then
      --  Cover the body.
      
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
      
   end Do_Coverage;
   
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
   
   procedure Do_Unit_Coverage (U : Unit_Number_Type) is
      
      Comp_Unit      : Node_Id;
      Comp_Node      : Node_Id;
      Body_Comp_Unit : Node_Id;
      Body_Node      : Node_Id;
      Spec_Comp_Unit : Node_Id;
      Spec_Node      : Node_Id;
      
      Parent_Comp_Unit : Node_Id;
      Parent_Unit      : Unit_Number_Type;
      Reflex_Coverage  : access Reflex_Coverage_Record;
   begin
      --  Dump file
      
      --  Put_Line
      --  	("---> Unit Name ==> " & Get_Name_String (Unit_Name (U)));

      --  Expand Glips code for this unit. First Expand the specs and then
      --  Expand the body in one file named "file_name.glips"
      
      Comp_Unit := Cunit (U); 
      Comp_Node := Unit (Comp_Unit);
      
      if Is_Covered (Comp_Unit) 
	or else Is_Coverage_Pending (Comp_Unit)
      then
	 return;
      end if;
      
      Set_Coverage_Pending (Comp_Unit, True);
      
      --  If the main compilation unit is a package or subprogram declaration
      --  then no body to Expand.
      
      if Nkind (Comp_Node) = N_Package_Declaration 
	or else Nkind (Comp_Node) = N_Subprogram_Declaration 
      then
	 if Present (Parent_Spec (Comp_Node)) then
	    if not In_Predefined_Unit (Comp_Node) 
	      and then not Is_Covered (Comp_Node) 
	    then
	       Parent_Comp_Unit := Parent_Spec (Comp_Node);
	       Parent_Unit := Get_Cunit_Unit_Number (Parent_Comp_Unit);
	       Do_Unit_Coverage (Parent_Unit);
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
      
	 
      Reflex_Coverage := New_Reflex_Coverage;
      
      --  Expand Specification
	 
      if Present (Spec_Comp_Unit) then
	 Cover_Specification (Reflex_Coverage, Spec_Comp_Unit);	 
      end if;
      
      --  Expand Body.
      
      if Present (Body_Comp_Unit) then
	 Cover_Body (Reflex_Coverage, Body_Comp_Unit);	 
      end if;
      
      Free_Reflex_Coverage (Reflex_Coverage_Ptr (Reflex_Coverage));
      
      Set_Covered (Comp_Unit, True);
      
   end Do_Unit_Coverage;
   
end Reflex.Coverings;
