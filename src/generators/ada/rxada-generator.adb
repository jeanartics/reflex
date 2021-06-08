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

with Atree; use Atree;
with Einfo; use Einfo;
with Lib; use Lib;
with Opt; use Opt;
with Namet; use Namet;
with Sinfo; use Sinfo;
with Sinput; use Sinput;
with Types; use Types;
with Uname; use Uname;
with Osint; use Osint;

with Artics.Buffers; use Artics.Buffers;

with Reflex.Generators; use Reflex.Generators;
with Rxada.Gen; use Rxada.Gen;
with Reflex.Gen.Types; use Reflex.Gen.Types;
with Rxada.Gen.Dispatch;
with Reflex.Gen.Ada_Outputs; use Reflex.Gen.Ada_Outputs;
with Reflex.Gen.Utils;
with Reflex.Infos; use Reflex.Infos;
--  with Reflex.Gen.Scopes_Stacks;
--  with Reflex.Gen.Supports;

package body Rxada.Generator is
   
   ----------------------------
   -- Generate_Specification --
   ----------------------------
   
   procedure Generate_Specification
     (This           : access Ada_Generator_Record;
      Spec_Comp_Unit : Node_Id) is
      
      Ob           : Output_Buffer := This.Get_Output_Buffer;
      Current_Unit : Unit_Number_Type;
      Src_First    : Source_Ptr;
   begin
      --  Generate Specs.
      
      Current_Unit := Get_Cunit_Unit_Number (Spec_Comp_Unit);
      This.Set_Main_Node (Spec_Comp_Unit);
      This.Set_Current_Source_File (Source_Index (Current_Unit));
      
      Src_First := Source_First (This.Get_Current_Source_File);
      This.Set_Last_Line_Printed (Get_Physical_Line_Number (Src_First));
				       
      RxAda.Gen.Dispatch.Generate_Node (This, Spec_Comp_Unit);
     
   end Generate_Specification;
   
   -------------------
   -- Generate_Body --
   -------------------
   
   procedure Generate_Body
     (This           : access Ada_Generator_Record;
      Body_Comp_Unit : Node_Id) is
      
      Ob           : Output_Buffer := This.Get_Output_Buffer;
      Current_Unit : Unit_Number_Type;
      Src_First    : Source_Ptr;
   begin
      --  Generate Body.
      
      Current_Unit := Get_Cunit_Unit_Number (Body_Comp_Unit);
      This.Set_Main_Node (Body_Comp_Unit);
      This.Set_Current_Source_File (Source_Index (Current_Unit));
      
      Src_First := Source_First (This.Get_Current_Source_File);
      This.Set_Last_Line_Printed (Get_Physical_Line_Number (Src_First));
				       
      RxAda.Gen.Dispatch.Generate_Node (This, Body_Comp_Unit);
      
   end Generate_Body;
   
   -------------------
   -- Do_Generation --
   -------------------
   
   procedure Do_Generation is
      
      Comp_Unit        : Node_Id;
      Comp_Node        : Node_Id;
      Body_Comp_Unit   : Node_Id;
      Body_Node        : Node_Id;
      Spec_Comp_Unit   : Node_Id;
      Spec_Node        : Node_Id;
      Ob_Spec          : Output_Buffer;
      Ob_Body          : Output_Buffer;
      Parent_Comp_Unit : Node_Id;
      Parent_Comp_Node : Node_Id;
      Parent_Unit      : Unit_Number_Type; 
      Ada_Gen : access Ada_Generator_Record;
  begin
      Reflex.Gen.Types.Initialize;
      
      --  Dump file

      --  Generate Ada code for this unit. First Generate the specs and then
      --  generate the body in one file named "file_name.Ada"
      
      Comp_Unit := Cunit (Main_Unit); 
      Comp_Node := Unit (Comp_Unit);
      
      if Is_Generated (Comp_Unit) 
	or else Is_Generation_Pending (Comp_Unit) 
      then
	 return;
      end if;
      
      Set_Generation_Pending (Comp_Unit, True);
      
      --  If the main compilation unit is a package or subprogram declaration
      --  then no body to generate.
      
      if Nkind (Comp_Node) = N_Package_Declaration 
	or else Nkind (Comp_Node) = N_Subprogram_Declaration 
      then
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
      
      Get_External_Unit_Name_String (Unit_Name (Main_Unit));
      
      Put_Line ("Unit Name      => " & Name_Buffer (1..Name_Len));
      Put_Line ("Body_Comp_Unit => " & Nkind (Body_Comp_Unit)'Img);
      Put_Line ("Body_Node      => " & Nkind (Body_Node)'Img);
      Put_Line ("Spec_Comp_Unit => " & Nkind (Spec_Comp_Unit)'Img);
      Put_Line ("Sepc_Node      => " & Nkind (Spec_Node)'Img);
      
      if Present (Spec_Node) 
	and then Nkind (Spec_Node) = N_Package_Declaration 
      then
	 Parent_Comp_Unit := Parent_Spec (Spec_Node);
	 
	 if Present (Parent_Comp_Unit) then
	    Parent_Comp_Node := Unit (Parent_Comp_Unit);
	    
	    if not In_Predefined_Unit (Parent_Comp_Unit) then
	       Parent_Comp_Unit := Parent_Spec (Spec_Node);
	       Parent_Unit      := Get_Cunit_Unit_Number (Parent_Comp_Unit);
	       Do_Unit_Generation (Parent_Unit);
	    end if;
	 end if;
      end if;
      
      Ada_Gen := New_Ada_Generator;
      
      Ob_Spec := null;
      Ob_Body := null;
      
      --  Generate Specs.
	 
      if Present (Spec_Comp_Unit) 
	and then (Nkind (Body_Node) /= N_Subprogram_Body 
		    or else not Acts_As_Spec (Body_Node))
      then
	 Ob_Spec := New_Output_Buffer;
	 Ada_Gen.Set_Output_Buffer (Ob_Spec);
      
	 Next_Comment_Line_To_Print := 1;
	 Generate_Specification (Ada_Gen, Spec_Comp_Unit);	 
      end if;
      
      --  Generate Body.
	 
      if Present (Body_Comp_Unit) then
	 Ob_Body := New_Output_Buffer;
	 Ada_Gen.Set_Output_Buffer (Ob_Body);
      
	 Next_Comment_Line_To_Print := 1;
	 Generate_Body (Ada_Gen, Body_Comp_Unit);	 
      end if;
      
      Set_Generated (Comp_Unit, True);
      
      --  And that's all folk.
      
      Get_Unit_File_Name_String (Unit_Name (Main_Unit));
      
      if Ob_Spec /= null then
	 declare
	    Object_Path : String := Object_Dir_Default_Prefix;
	    S           : String := Name_Buffer (1..Name_Len) & ".ads";
	 begin
	    if Object_Path /= "" then
	       -- Write_To_Text_File (Ob_Spec, Object_Path & "/" & S);
	       Write_To_Text_File (Ob_Spec, "./obj/" & S);
	    else
	       Write_To_Text_File (Ob_Spec, S);
	    end if;
	 end;
      end if;
      
      if Ob_Body /= null then
	 declare
	    Object_Path : String := Object_Dir_Default_Prefix;
	    S           : String := Name_Buffer (1..Name_Len) & ".adb";
	 begin
	    if Object_Path /= "" then
	       -- Write_To_Text_File (Ob_Body, Object_Path & "/" & S);
	       Write_To_Text_File (Ob_Body, "./obj/" & S);
	    else
	       Write_To_Text_File (Ob_Body, S);
	    end if;
	 end;
      end if;
      
   end Do_Generation;
   
   ------------------------
   -- Do_Unit_Generation --
   ------------------------
   
   procedure Do_Unit_Generation (U : Unit_Number_Type) is
      
      Comp_Unit        : Node_Id;
      Comp_Node        : Node_Id;
      Body_Comp_Unit   : Node_Id;
      Body_Node        : Node_Id;
      Spec_Comp_Unit   : Node_Id;
      Spec_Node        : Node_Id;
      Ob_Spec          : Output_Buffer;
      Ob_Body          : Output_Buffer;
      Parent_Comp_Unit : Node_Id;
      Parent_Comp_Node : Node_Id;
      Parent_Unit      : Unit_Number_Type;
      Ada_Gen        : access Ada_Generator_Record;
   begin
      --  Dump file

      --  Generate Ada code for this unit. First Generate the specs and then
      --  generate the body in one file named "file_name.Ada"
      
      Comp_Unit := Cunit (U); 
      Comp_Node := Unit (Comp_Unit);
      
      if Is_Generated (Comp_Unit) 
	or else Is_Generation_Pending (Comp_Unit) 
      then
	 return;
      end if;
      
      Set_Generation_Pending (Comp_Unit, True);
      
      --  If the main compilation unit is a package or subprogram declaration
      --  then no body to generate.
      
      if Nkind (Comp_Node) = N_Package_Declaration 
	or else Nkind (Comp_Node) = N_Subprogram_Declaration 
      then
	 Parent_Comp_Unit := Parent_Spec (Comp_Node);
	 
	 if Present (Parent_Comp_Unit) then
	    Parent_Comp_Node := Unit (Parent_Comp_Unit);
	    if not In_Predefined_Unit (Parent_Comp_Unit)
	    then
	       Parent_Comp_Unit := Parent_Spec (Comp_Node);
	       Parent_Unit      := Get_Cunit_Unit_Number (Parent_Comp_Unit);
	       Do_Unit_Generation (Parent_Unit);
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
      
      -- Put_Line ("Unit Gen: Unit Name      => " & Name_Buffer (1..Name_Len));
      -- Put_Line ("Unit Gen: Body_Comp_Unit => " & Nkind (Body_Comp_Unit)'Img);
      -- Put_Line ("Unit Gen: Body_Node      => " & Nkind (Body_Node)'Img);
      -- Put_Line ("Unit Gen: Spec_Comp_Unit => " & Nkind (Spec_Comp_Unit)'Img);
      -- Put_Line ("Unit Gen: Sepc_Node      => " & Nkind (Spec_Node)'Img);
      
      -- Create Generator 
      
      Ada_Gen := New_Ada_Generator;
      
      Ob_Spec := null;
      Ob_Body := null;
      
      --  Generate Specs.
      
      if Present (Spec_Comp_Unit) then
	 Ob_Spec := New_Output_Buffer;
	 Ada_Gen.Set_Output_Buffer (Ob_Spec);
      
	 Next_Comment_Line_To_Print := 1;
	 Generate_Specification (Ada_Gen, Spec_Comp_Unit);	 
      end if;
      
      --  Generate Body.
      
      if Present (Body_Comp_Unit) then
	 Ob_Body := New_Output_Buffer;
	 Ada_Gen.Set_Output_Buffer (Ob_Body);
      
	 Next_Comment_Line_To_Print := 1;
	 Generate_Body (Ada_Gen, Body_Comp_Unit);	 
      end if;
      
      Set_Generated (Comp_Unit, True);
      
      --  And that's all folk.
      
      Get_Unit_File_Name_String (Unit_Name (U));
      
      if Ob_Spec /= null then
	 declare
	    Object_Path : String := Object_Dir_Default_Prefix;
	    S           : String := Name_Buffer (1..Name_Len) & ".ads";
	 begin
	    if Object_Path /= "" then
	       -- Write_To_Text_File (Ob_Spec, Object_Path & "/" & S);
	       Write_To_Text_File (Ob_Spec, "./obj/" & S);
	    else
	       Write_To_Text_File (Ob_Spec, S);
	    end if;
	 end;
      end if;
      
      if Ob_Body /= null then
	 declare
	    Object_Path : String := Object_Dir_Default_Prefix;
	    S           : String := Name_Buffer (1..Name_Len) & ".adb";
	 begin
	    if Object_Path /= "" then
	       -- Write_To_Text_File (Ob_Body, Object_Path & "/" & S);
	       Write_To_Text_File (Ob_Body, "./obj/" & S);
	    else
	       Write_To_Text_File (Ob_Body, S);
	    end if;
	 end;
      end if;
      
      Free_Ada_Generator (Ada_Generator_Ptr (Ada_Gen));      
      
   end Do_Unit_Generation;
   
end Rxada.Generator;
