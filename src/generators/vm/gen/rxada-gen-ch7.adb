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

with Ada.Text_Io; use Ada.Text_Io;

with Atree; use Atree;
with Einfo; use Einfo;
with Sinfo; use Sinfo;
with Nlists; use Nlists;
with Sem_Util; use Sem_Util;
with Types; use Types;
with Namet; use Namet;

with Artics.Buffers; use Artics.Buffers;
with Reflex.Gen.Types; use Reflex.Gen.Types;
with Reflex.Gen.Ada_Outputs; use Reflex.Gen.Ada_Outputs;
with Reflex.Gen.Utils; use Reflex.Gen.Utils;

package body Rxada.Gen.Ch7 is
   
   -----------------------------------------
   -- Generate_Defining_Program_Unit_Name --
   -----------------------------------------
   
   procedure Generate_Defining_Program_Unit_Name 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob       : Output_Buffer := This.Get_Output_Buffer;
      Def_Name : Node_Id := Defining_Unit_Name (Node);
   begin
      if Nkind (Def_Name) = N_Defining_Program_Unit_Name then
	 Write_Id (Ob, Name (Def_Name));
	 Write_Str (Ob, ".");
	 Write_Id (Ob, (Defining_Identifier (Def_Name)));
      else
	 Write_Id (Ob, Def_Name);
      end if;
   end Generate_Defining_Program_Unit_Name;
   
   -------------------------
   -- Generate_Designator --
   -------------------------
   
   procedure Generate_Designator 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Designator;
   
   ---------------------------
   -- Generate_Package_Body --
   ---------------------------
   
   procedure Generate_Package_Body 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      if Ekind (Corresponding_Spec (Node)) = E_Generic_Package then
	 return;
      end if;
      
      Write_Comment_Line_To_Node (This, Node);
      
      Write_Indent_Str (Ob, "package body ");
      Generate_Defining_Program_Unit_Name (This, Node);
      Write_Str (Ob, " is");
      Write_Eol (Ob);
      
      Indent_Begin (Ob);
      
      Skip_Comment_Line_To_Node (Generator_Ptr (This), Node);
      
      This.Open_Scope (Unique_Defining_Entity (Node));
      
      Generate_Node_List (This, Declarations (Node));
      
      Indent_End (Ob);
      Write_Indent_Str (Ob, "end ");
      Generate_Defining_Program_Unit_Name (This, Node);
      Write_Str (Ob, ";");
      Write_Eol (Ob);
      
      This.Close_Scope;
   end Generate_Package_Body;
   
   ----------------------------------
   -- Generate_Package_Declaration --
   ----------------------------------
   
   procedure Generate_Package_Declaration 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Write_Comment_Line_To_Node (This, Node);
      
      Write_Indent_Str (Ob, "package ");
      Generate_Defining_Program_Unit_Name (This, Specification (Node));
      Write_Str (Ob, " is");
      Write_Eol (Ob);
      
      Indent_Begin (Ob);
      
      Generate_Node (This, Specification (Node), True); 
      
      Indent_End (Ob);
      Write_Indent_Str (Ob, "end ");
      Generate_Defining_Program_Unit_Name (This, Specification (Node));
      Write_Str (Ob, ";");
      Write_Eol (Ob);
  end Generate_Package_Declaration;
   
   ------------------------------------
   -- Generate_Package_Specification --
   ------------------------------------
   
   procedure Generate_Package_Specification 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      This.Open_Scope (Unique_Defining_Entity (Node));
      
      Generate_Node_List (This, Visible_Declarations (Node));
      
      if Present (Private_Declarations (Node)) then
	 Indent_End (Ob);
	 Write_Eol (Ob);
	 Write_Indent_Str (Ob, "private");
	 Write_Eol (Ob);
	 Indent_Begin (Ob);
	 
	 Generate_Node_List (This, Private_Declarations (Node));
      end if;
      
      This.Close_Scope;
   end Generate_Package_Specification;

end Rxada.Gen.Ch7;
