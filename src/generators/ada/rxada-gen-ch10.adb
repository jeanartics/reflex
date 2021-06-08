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

with Artics.Buffers; use Artics.Buffers;
with Reflex.Gen.Ada_Outputs; use Reflex.Gen.Ada_Outputs;
with Reflex.Gen.Utils; use Reflex.Gen.Utils;
with Rxada.Generator; use Rxada.Generator;

package body Rxada.Gen.Ch10 is
   
   -------------------------------
   -- Generate_Compilation_Unit --
   -------------------------------
   
   procedure Generate_Compilation_Unit 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob    : Output_Buffer := This.get_Output_Buffer;
      Items : List_Id;
      Item  : Node_Id;
   begin
      Items := Context_Items (Node);
      Item := First (Items);
      while Present (Item) loop
	 
	 if Nkind (Item) = N_With_Clause then
	    Generate_With_Clause (This, Item);
	 elsif Nkind (Item) = N_Use_Package_Clause then
	    Generate_Use_Package_Clause (This, Item);
	 end if;
	 Next (Item);
      end loop;
      
      Generate_Node (This, Unit (Node));
   end Generate_Compilation_Unit;
   
   -----------------------------------
   -- Generate_Compilation_Unit_Aux --
   -----------------------------------
   
   procedure Generate_Compilation_Unit_Aux 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Compilation_Unit_Aux;
   
   ---------------------------------
   -- Generate_Use_Package_Clause --
   ---------------------------------
   
   procedure Generate_Use_Package_Clause 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.get_Output_Buffer;
      N  : Node_Id;
   begin
      if Present (Names (Node))
	and then not Is_Empty_List (Names (Node)) then
	 N := First (Names (Node));
	 while Present (N) loop
	    Write_Indent_Str (Ob, "use ");
	    Write_Id (Ob, N);
	    Write_Str (Ob, ";");
	    Write_Eol (Ob);
	    Next (N);
	 end loop;
      end if;
   end Generate_Use_Package_Clause;
   
   ------------------------------
   -- Generate_Use_Type_Clause --
   ------------------------------
   
   procedure Generate_Use_Type_Clause 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Use_Type_Clause;
   
   --------------------------
   -- Generate_With_Clause --
   --------------------------
   
   procedure Generate_With_Clause 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob        : Output_Buffer := This.Get_Output_Buffer;
      U         : Unit_Number_Type;
      Comp_Unit : Node_Id;
      With_Unit : Node_Id;
--      Spec      : Node_Id;
--      Def_Id    : Entity_Id;
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
	    --  Spec := Specification (With_Unit);
	    --  Def_Id := Defining_Entity (With_Unit);
	    Write_Str (Ob, "with ");

	    Write_Id (Ob, Name (Node)); -- Def_Id);
	    Write_Str (Ob, ";");
	    Write_Eol (Ob);
	 else
	    null; --  raise Program_Error;
	 end if;
      end if;
   end Generate_With_Clause;

end Rxada.Gen.Ch10;
