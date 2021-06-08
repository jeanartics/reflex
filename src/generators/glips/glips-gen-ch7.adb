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
-----------------------------!-------------------------------------------------

with Ada.Text_Io; use Ada.Text_IO;

with Atree; use Atree;
with Einfo; use Einfo;
with Sinfo; use Sinfo;
with Nlists; use Nlists;
with Sem_Util; use Sem_Util;
with Types; use Types;
with Namet; use Namet;
with Lib; use Lib;
with Uname; use Uname;

with Artics.Buffers; use Artics.Buffers;
with Reflex.Gen.Types; use Reflex.Gen.Types;
with Reflex.Gen.Outputs; use Reflex.Gen.Outputs;
with Reflex.Gen.Utils; use Reflex.Gen.Utils;

package body Glips.Gen.Ch7 is
   
   -----------------------------------------
   -- Generate_Defining_Program_Unit_Name --
   -----------------------------------------
   
   procedure Generate_Defining_Program_Unit_Name 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      Generate_Node (This, Defining_Unit_Name (Node));
   end Generate_Defining_Program_Unit_Name;
   
   -------------------------
   -- Generate_Designator --
   -------------------------
   
   procedure Generate_Designator 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Designator;
   
   ---------------------------
   -- Generate_Package_Body --
   ---------------------------
   
   procedure Generate_Package_Body 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
      U  : Unit_Number_Type;
      E  : Entity_Id;
   begin
      Put_Line ("Generate_Package_Body Begin");
      if Ekind (Corresponding_Spec (Node)) = E_Generic_Package then
	 return;
      end if;
      
      Write_Comment_Line_To_Node (This, Node);
      
      E := Defining_Entity (Node);
      U := Get_Cunit_Entity_Unit_Number (E);
      
      if not Present (Corresponding_Spec (Node)) then
	 Write_Indent_Str (Ob, "package ");
	 Get_External_Unit_Name_String (Unit_Name (U));
	 declare
	    S : String := Name_Buffer (1..Name_Len);
	 begin
	 Put_Line ("=========> body name is " & S);
	    Write_Str (Ob, S);
	 end;
      -- Generate_Defining_Program_Unit_Name (This, Node);
	 Write_Str (Ob, " is");
	 Write_Eol (Ob);
	 
	 Indent_Begin (Ob);
      end if;
      
      Skip_Comment_Line_To_Node (Generator_Ptr (This), Node);
      
      This.Open_Scope (Unique_Defining_Entity (Node));
      
      Generate_Node_List (This, Declarations (Node));
      
      Indent_End (Ob);
      
      Get_External_Unit_Name_String (Unit_Name (U));
      Write_Indent_Str (Ob, "end ");
      declare
	 S : String := Name_Buffer (1..Name_Len);
      begin
	 Write_Str (Ob, S);
      end;
      --  Generate_Defining_Program_Unit_Name (This, Node);
      Write_Str (Ob, ";");
      Write_Eol (Ob);
      
      This.Close_Scope;
      
      Put_Line ("Generate_Package_Body End");
   end Generate_Package_Body;
   
   ----------------------------------
   -- Generate_Package_Declaration --
   ----------------------------------
   
   procedure Generate_Package_Declaration 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
      
      Ob   : Output_Buffer := This.Get_Output_Buffer;
      Spec : Node_Id;
      U    : Unit_Number_Type;
      E    : Entity_Id;
   begin
      Write_Comment_Line_To_Node (This, Node);
      
      Write_Indent_Str (Ob, "package ");
      
      Spec := Specification (Node);
      
      E := Defining_Entity (Node);
      U := Get_Cunit_Entity_Unit_Number (E);
      
      Get_External_Unit_Name_String (Unit_Name (U));
      declare
	 S : String := Name_Buffer (1..Name_Len);
      begin
	 Put_Line ("=========> declaration name is " & S);
	 Write_Str (Ob, S);
      end;

      -- Generate_Defining_Program_Unit_Name (This, Specification (Node));
      Write_Str (Ob, " is");
      Write_Eol (Ob);
      
      Indent_Begin (Ob);
      
      Generate_Node (This, Specification (Node), True); 
      
      if not Present (Corresponding_Body (Node)) then
	 Indent_End (Ob);
	 Write_Indent_Str (Ob, "end ");
	 Get_External_Unit_Name_String (Unit_Name (U));
	 declare
	    S : String := Name_Buffer (1..Name_Len);
	 begin
	    Write_Str (Ob, S);
	 end;
	 -- Generate_Defining_Program_Unit_Name (This, Node);
	 Write_Str (Ob, ";");
	 Write_Eol (Ob);
      end if;
  end Generate_Package_Declaration;
   
   ------------------------------------
   -- Generate_Package_Specification --
   ------------------------------------
   
   procedure Generate_Package_Specification 
     (This : access Glips_Generator_Record;
      Node : Node_Id) is
   begin
      This.Open_Scope (Unique_Defining_Entity (Node));
      
      Generate_Node_List (This, Visible_Declarations (Node));
      
      if Present (Private_Declarations (Node)) then
	 Generate_Node_List (This, Private_Declarations (Node));
      end if;
      
      This.Close_Scope;
   end Generate_Package_Specification;

end Glips.Gen.Ch7;
