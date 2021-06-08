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
 
with Ada.Directories;
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

with Artics.Buffers; use Artics.Buffers;
with Unity.Gen; use Unity.Gen;
with Reflex.Gen.Types; use Reflex.Gen.Types;
with Unity.Gen.Dispatch;
with Reflex.Gen.Outputs; use Reflex.Gen.Outputs;
with Reflex.Infos; use Reflex.Infos;

--  with Unity.Gen.Utils;
--  with Unity.Gen.Scopes_Stacks;
--  with Unity.Gen.Supports;
--  with Unity.Names;
--  with Unity.Tokens;
--  with Unity.Dom_Tokens;
with Artics.Input_Buffers;
with Unity.Xef_Generators; use Unity.Xef_Generators;

package body Unity.Generator is
   
   Unity_Gen : access Unity_Generator_Record;
   
   ----------------------------
   -- Generate_Specification --
   ----------------------------
   
   procedure Generate_Specification
     (This           : access Unity_Generator_Record;
      Spec_Comp_Unit : Node_Id) is
      
      Current_Unit : Unit_Number_Type;
      Src_First    : Source_Ptr;
   begin
      --  Generate Specs.
      
      Current_Unit := Get_Cunit_Unit_Number (Spec_Comp_Unit);
      Put_Line ("Specification File : ====>");
      Get_External_Unit_Name_String (Unit_Name (Current_Unit));
      declare
	 S : String := Name_Buffer (1..Name_Len);
      begin
	 Put_Line (S); 
      end;

      This.Set_Main_Node (Spec_Comp_Unit);
      This.Set_Current_Source_File (Source_Index (Current_Unit));
      Put_Line ("Source_Index " & Source_Index (Current_Unit)'Img);
      Put_Line ("Sloc " & Sloc (Spec_Comp_Unit)'Img);
      Put_Line
	("Sloc Physical " & 
	   Get_Physical_Line_Number (Sloc (Spec_Comp_Unit))'Img);
      
      Src_First := Source_First (This.Get_Current_Source_File);
      This.Set_Last_Line_Printed (Get_Physical_Line_Number (Src_First));
				       
      Unity.Gen.Dispatch.Generate_Node (This, Spec_Comp_Unit);
     
   end Generate_Specification;
   
   -------------------
   -- Generate_Body --
   -------------------
   
   procedure Generate_Body
     (This           : access Unity_Generator_Record;
      Body_Comp_Unit : Node_Id) is
      
      Current_Unit : Unit_Number_Type;
      Src_First    : Source_Ptr;
   begin
      --  Generate Body.
      
      Current_Unit := Get_Cunit_Unit_Number (Body_Comp_Unit);
      Put_Line ("Body File : ====>");
      Get_External_Unit_Name_String (Unit_Name (Current_Unit));
      declare
	 S : String := Name_Buffer (1..Name_Len);
      begin
	 Put_Line (S); 
      end;
      
      This.Set_Main_Node (Body_Comp_Unit);
      This.Set_Current_Source_File (Source_Index (Current_Unit));
      Put_Line ("Source_Index " & Source_Index (Current_Unit)'Img);
      Put_Line ("Sloc " & Sloc (Body_Comp_Unit)'Img);
      Put_Line
	("Sloc Physical " & 
	   Get_Physical_Line_Number (Sloc (Body_Comp_Unit))'Img);
      
      Src_First := Source_First (This.Get_Current_Source_File);
      This.Set_Last_Line_Printed (Get_Physical_Line_Number (Src_First));
      
      Unity.Gen.Dispatch.Generate_Node (This, Body_Comp_Unit);
      
   end Generate_Body;
   
   -------------------
   -- Do_Generation --
   -------------------
   
   procedure Do_Generation is
      
      Comp_Unit      : Node_Id;
      Comp_Node      : Node_Id;
      Body_Comp_Unit : Node_Id;
      Body_Node      : Node_Id;
      Spec_Comp_Unit : Node_Id;
      Spec_Node      : Node_Id;
      Parent_Comp_Unit : Node_Id;
      Parent_Comp_Node : Node_Id;
      Parent_Unit      : Unit_Number_Type;
   begin
      Reflex.Gen.Types.Initialize;
      
      --  Generate Unity code for this unit. First Generate the specs and then
      --  generate the body in one file named "file_name.Unity"
      
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
      
      -- Create Generator 
      
      Unity_Gen := New_Unity_Generator;
      
      
      if Present (Spec_Node) 
	and then Nkind (Spec_Node) = N_Package_Declaration 
      then
	 Parent_Comp_Unit := Parent_Spec (Spec_Node);
	 
	 if Present (Parent_Comp_Unit) then
	    Parent_Comp_Node := Unit (Parent_Comp_Unit);
	    if not In_Predefined_Unit (Parent_Comp_Unit)
	    then
	       Parent_Comp_Unit := Parent_Spec (Spec_Node);
	       Parent_Unit := Get_Cunit_Unit_Number (Parent_Comp_Unit);
	       Do_Unit_Generation (Parent_Unit);
	    end if;
	 end if;
      end if;
      
      --  Generate Specs.
      
      if Present (Spec_Comp_Unit) then
	 Next_Comment_Line_To_Print := 1;
	 Generate_Specification (Unity_Gen, Spec_Comp_Unit);	 
      end if;
      
      --  Generate Body.
      
      if Present (Body_Comp_Unit) then
	 Next_Comment_Line_To_Print := 1;
	 Generate_Body (Unity_Gen, Body_Comp_Unit);	 
      end if;
      
      Set_Generated (Comp_Unit, True);
      
      Get_External_Unit_Name_String (Unit_Name (Main_Unit));
      declare
         Fname   : String := Name_Buffer (1 .. Name_Len) & ".xef";
         Xef_Gen : access Xef_Generator_Record := 
                     New_Xef_Generator (Unity_Gen, Fname);
      begin
         Xef_Gen.Set_Xef_File_Name (Fname);
	 
         -- Template to use :
         -- -----------------
         -- priorite au fichier tmpl.xef du repertoire courant 
         -- ou option -T de la ligne de commande
         -- ou le template intalle dans gnat (cf gprconfig)
         
         -- En priorite : tmpl.xefdu current dir
         if Ada.Directories.Exists
	   (Ada.Directories.Current_Directory & "/tmpl.xef") then 
            Xef_Gen.Set_Template_Xef_File_Name ("tmpl.xef");         
         else
            -- Par defaut : celui de l'option -T{path_to_file} 
            -- Put_Line ("######## GET Template to use, switch -T ");
             
            begin
               for I in 1 .. Lib.Compilation_Switches_Last loop
                  declare
                     Sw  : String  := Get_Compilation_Switch (I).all;
                     F   : Natural := Sw'First;
                     L   : Natural := Sw'Last;
                     Len : Natural := Sw'Length;
                  begin
                     if Len >= 3 and then
                       Sw (F .. F + 1) = "-T" then
                        -- minimum check on file
                        if Ada.Directories.Exists (Sw (F + 2 .. L)) then
                           Xef_Gen.Set_Template_Xef_File_Name (Sw (F + 2 .. L));
                        end if;
                     end if;
                  end;               
               end loop;
            exception 
               when others =>
                  Put_Line ("############## EXCEPTION IN Resolve_Filename ");
            end;
         end if;
         --  Put_Line
	 --    ("# Template used is : " & Xef_Gen.Get_Template_Xef_File_Name);
         
         Xef_Gen.Generate_Xef;
      end;
      
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
      Parent_Comp_Unit : Node_Id;
      Parent_Comp_Node : Node_Id;
      Parent_Unit      : Unit_Number_Type;
      -- Glips_Gen        : access Unity_Generator_Record;
   begin
      --  Dump file

      --  Generate Glips code for this unit. First Generate the specs and then
      --  generate the body in one file named "file_name.glips"
      
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
	       Parent_Unit := Get_Cunit_Unit_Number (Parent_Comp_Unit);
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
      
      Get_External_Unit_Name_String (Unit_Name (U));
      
      --Put_Line ("Unit Gen: Unit Name      => " & Name_Buffer (1..Name_Len));
      --Put_Line ("Unit Gen: Body_Comp_Unit => " & Nkind (Body_Comp_Unit)'Img);
      --Put_Line ("Unit Gen: Body_Node      => " & Nkind (Body_Node)'Img);
      --Put_Line ("Unit Gen: Spec_Comp_Unit => " & Nkind (Spec_Comp_Unit)'Img);
      --Put_Line ("Unit Gen: Sepc_Node      => " & Nkind (Spec_Node)'Img);
      
      --  -- Create Generator 
      
      --  Unity_Gen := New_Unity_Generator;
      
      --  Ob := New_Output_Buffer;
      --  Unity_Gen.Set_Output_Buffer (Ob);
      
      --  Generate Specification
	 
      if Present (Spec_Comp_Unit) then
	 Next_Comment_Line_To_Print := 1;
	 Generate_Specification (Unity_Gen, Spec_Comp_Unit);	 
      end if;
      
      --  Generate Body.
	 
      if Present (Body_Comp_Unit) then
	 Next_Comment_Line_To_Print := 1;
	 Generate_Body (Unity_Gen, Body_Comp_Unit);	 
      end if;
      
      --  And that's all folk.
      
      Set_Generated (Comp_Unit, True);
      
      -- Free_Unity_Generator (Unity_Generator_Ptr (Unity_Gen));      
      
   end Do_Unit_Generation;
   
end Unity.Generator;
