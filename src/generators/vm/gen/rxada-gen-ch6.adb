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

with ada.text_io; use ada.text_io;

with Atree; use Atree;
with Errout; use Errout;
with Einfo; use Einfo;
with Sinfo; use Sinfo;
with Namet; use Namet;
with Nlists; use Nlists;
with Sem_Util; use Sem_Util;
with Types; use Types;

with artics.Buffers; use Artics.Buffers;

with Reflex.Generators; use Reflex.Generators;
with Reflex.Gen.Types; use Reflex.Gen.Types;
with Reflex.Gen.Ada_Outputs; use Reflex.Gen.Ada_Outputs;
with Reflex.Gen.Utils; use Reflex.Gen.Utils;
with Rxada.Gen.Dispatch; use Rxada.Gen.Dispatch;
with Rxada.Gen.Ch3; use Rxada.Gen.Ch3;

package body Rxada.Gen.Ch6 is
   
   ----------------------------------------------
   -- Generate_Abstract_Subprogram_Declaration --
   ----------------------------------------------
   
   procedure Generate_Abstract_Subprogram_Declaration 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Abstract_Subprogram_Declaration;
   
   ------------------------
   -- Generate_Body_Stub --
   ------------------------
   
   procedure Generate_Body_Stub 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Body_Stub;
   
   ---------------------------------------
   -- Generate_Defining_Operator_Symbol --
   ---------------------------------------
   
   procedure Generate_Defining_Operator_Symbol 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Defining_Operator_Symbol;
   
   -----------------------------------------
   -- Generate_Defining_Program_Unit_Name --
   -----------------------------------------
   
   procedure Generate_Defining_Program_Unit_Name 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
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
   
   ----------------------------------------
   -- Generate_Extended_Return_Statement --
   ----------------------------------------
   
   procedure Generate_Extended_Return_Statement 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Extended_Return_Statement;
   
   -------------------------------------
   -- Generate_Function_Instantiation --
   -------------------------------------
   
   procedure Generate_Function_Instantiation 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Function_Instantiation;
   
   -------------------------------------
   -- Generate_Function_Specification --
   -------------------------------------
   
   procedure Generate_Function_Specification 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Function_Specification;
   
   ------------------------------
   -- Generate_Operator_Symbol --
   ------------------------------
   
   procedure Generate_Operator_Symbol 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Operator_Symbol;
   
   ------------------------------------
   -- Generate_Parameter_Association --
   ------------------------------------
   
   procedure Generate_Parameter_Association 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Parameter_Association;
   
   ------------------------------------
   -- Generate_Parameter_Declaration --
   ------------------------------------

   procedure Generate_Parameter_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob         : Output_Buffer := This.Get_Output_Buffer;
      Formal     : Entity_Id := Node; --  Defining_Identifier (Node);
      Param_Type : Entity_Id;
   begin
      ----------------------------------
      -- 6.1  Parameter Specification --
      ----------------------------------

      --  PARAMETER_SPECIFICATION ::=
      --    DEFINING_IDENTIFIER_LIST : [ALIASED] MODE [NULL_EXCLUSION]
      --      SUBTYPE_MARK [:= DEFAULT_EXPRESSION]
      --  | DEFINING_IDENTIFIER_LIST : ACCESS_DEFINITION
      --      [:= DEFAULT_EXPRESSION]

      
      Write_Indent (Ob);
      Write_Id (Ob, Formal);
      Write_Str (Ob, " : ");
      
      if Ekind (Formal) = E_In_Parameter then
	 Write_Str (Ob, "in ");
	 
      elsif Ekind (Formal) = E_In_Out_Parameter then
	 Write_Str (Ob, "in out ");
	 
      elsif Ekind (Formal) = E_Out_Parameter then
	 Write_Str (Ob, "out ");
	 
	 -- Never arise
      else
	 raise Program_Error;
      end if;
      
      --  Param_Type := Parameter_Type (Node);
      Param_Type := Etype (Formal);
      
      if Is_Access_Type (Param_Type) then
	 Write_Str (Ob, "access ");
      else
	 null;
      end if;
      Write_Id (Ob, Param_Type);
      
      --  if NKind_In (Param_Type, N_Expanded_Name, N_Identifier) then
      --  	 Generate_Subtype_Mark (Param_Type);
	 
      --  	 Expr := Expression (Node);
      --  	 if Present (Expr) then
      --  	    Write_Str (" := ");
      --  	    if Compile_Time_Known_Value (Expr) then
      --  	       Write_Uint (Expr_Value (Expr));
	       
      --  	    else
      --  	       null;
      --  	    end if;
      --  	 end if;
      --  elsif Nkind (Param_Type) = N_Access_Definition then
	 
      --  	 Subtyp := Subtype_Mark (Param_Type);
      --  	 pragma Assert (Present (Subtyp));
      --  	 pragma Assert (Nkind_In (Subtyp, N_Expanded_Name, N_Identifier));
	 
      --  	 Write_Str (" access ");
      --  	 Generate_Subtype_Mark (Subtyp);
	 
      --  else
      --  	 Error_Msg_N
      --  	   ("reflex does not supported this definition type ", Node);
      --  end if;
      
      --  Generate Expression if it is static.
   end Generate_Parameter_Declaration;
   
   --------------------------------------
   -- Generate_Parameter_Specification --
   --------------------------------------
   
   procedure Generate_Parameter_Specification 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob          : Output_Buffer := This.Get_Output_Buffer;
      Subp_Id     : Entity_Id     := Unique_Defining_Entity (Node);
      Formals     : List_Id;
      Formal      : Node_Id;
      Formal_Id   : Entity_Id;
      Formal_Type : Node_Id;
      Frst_Formal : Node_Id;
      Expr        : Node_Id;
      Len         : Natural;
      Count       : Natural;
   begin
      Count  := 0;
      Len    := 0;
      
      Formals := Parameter_Specifications (Node);
      
      Formal := First (Formals);
      while Present (Formal) loop
	 Formal_Id := Defining_Identifier (Formal);
	 declare
	    S : String := Get_Name_String (Chars (Formal_Id));
	 begin
	    if S'Length > Len then
	       Len := S'Length;
	    end if;
	 end;
	 Count := Count + 1;
	 Next (Formal);
      end loop;
      Len := Len + 1;
      
      if Count = 0 then
	 return;
      end if;
      
      if Count = 1 then
	 Write_Str (Ob, " (");
      else
	 Write_Eol (Ob);
	 Indent_Begin (Ob);
	 Write_Indent_Str (Ob, "(");
      end if;
      
      Formal := First (Formals);
      Frst_Formal := Formal;
      while Present (Formal) loop
      
	 Write_Comment_Line_To_Node (This, Formal);
	 
	 --  Emit Format "Formal : "
	 
	 Formal_Id := Defining_Identifier (Formal);
	 
	 declare
	    S      : String := Get_Name_String (Chars (Formal_Id));
	    Spaces : Natural;
	 begin
	    Spaces := Len - S'Length;
	    if Frst_Formal /= Formal then
	       Write_Indent_Str (Ob, " ");
	    end if;
	    
	    Write_Id (Ob, Formal_Id);
	    
	    for I in 2..Spaces loop
	       Write_Char (Ob, ' ');
	    end loop;
	    
	    Write_Str (Ob, " : ");
	 end;
	 
	 if In_Present (Formal) then
	    Write_Str (Ob, "in ");
	 end if;
	    
	 if Out_Present (Formal) then
	    Write_Str (Ob, "out ");
	 end if;
	    
	 Formal_Type := Parameter_Type (Formal);
	 
	 if Nkind (Formal_Type) = N_Access_Definition then
	    Write_Str (Ob, "access ");
	    Generate_Subtype_Mark (This, Subtype_Mark (Formal_Type));
				   
	 else
	    Generate_Subtype_Mark (This, Formal_Type);
	 end if;
	 
	 Expr := Expression (Formal);
	 if Present (Expr) then
	    Write_Str (Ob, " := ");
	    Generate_Node (This, Expr);
	 end if;
	 
	 Next (Formal);
	 exit when No (Formal);
	 Write_Str (Ob, ";");
	 Write_Eol (Ob);
      end loop;
      
      if Count = 1 then
	 Write_Str (Ob, ")");
      else
	 Write_Str (Ob, ")");
	 Indent_End (Ob);
      end if;
      
   end Generate_Parameter_Specification;
   
   --------------------------------------
   -- Generate_Procedure_Instantiation --
   --------------------------------------
   
   procedure Generate_Procedure_Instantiation 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Procedure_Instantiation;
   
   --------------------------------------
   -- Generate_Procedure_Specification --
   --------------------------------------
   
   procedure Generate_Procedure_Specification 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Procedure_Specification;
   
   --------------------------------------
   -- Generate_Simple_Return_Statement --
   --------------------------------------
   
   procedure Generate_Simple_Return_Statement 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Simple_Return_Statement;
   
   ---------------------------------------
   -- Generate_Subprogram_Specification --
   ---------------------------------------
   
   procedure Generate_Subprogram_Specification
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob     : Output_Buffer := This.Get_Output_Buffer;
      Def_Id : Entity_Id := Defining_Unit_Name (Node);
      Kind   : Node_Kind := Nkind (Node);
      Result : Node_Id;
   begin
      Write_Comment_Line_To_Node (This, Node);
      
      Write_Indent (Ob);
      if Kind = N_Function_Specification then
	 Write_Str (Ob, "function ");
      elsif Kind = N_Procedure_Specification then
	 Write_Str (Ob, "procedure ");
      else
	 Error_Msg_N
	   ("unknown subprogram type for reflex", Node);
	 return;
      end if;
      
      --  Generate_Defining_Program_Unit_Name (This, Node);
      
      pragma Assert (Nkind (Def_Id) = N_Defining_Identifier);
      Write_Id (Ob, Def_Id);
      
      if Present (Parameter_Specifications (Node)) then
	 Generate_Parameter_Specification (This, Node);	 
      end if;
      
      if Kind = N_Function_Specification then
	 Write_Str (Ob, " return ");
	 Result := Subtype_Mark (Node);
	 Generate_Subtype_Mark (This, Result);
      end if;
   end Generate_Subprogram_Specification;
   
   ------------------------------
   -- Generate_Subprogram_Body --
   ------------------------------
   
   procedure Generate_Subprogram_Body 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob    : Output_Buffer := This.Get_Output_Buffer;
      Spec  : Node_Id;
      Decls : List_Id;
      N     : Node_Id;
   begin
      Write_Comment_Line_To_Node (This, Node);
      
      Spec := Specification (Node);
      Generate_Subprogram_Specification (This, Spec);
      
      Write_Str (Ob, " is");
      Write_Eol (Ob);
      
      Decls := Declarations (Node);
      if Is_Non_Empty_List (Decls) then
	 Indent_Begin (Ob);
         N := First (Decls);

         while Present (N) loop
	    Write_Comment_Line_To_Node (This, N);
	    Generate_Node (This, N);
            Next (N);
         end loop;
	 
	 Indent_End (Ob);
      end if;
      
      Write_Indent_Str (Ob, "begin");
      Write_Eol (Ob);
	
      Indent_Begin (Ob);
      Generate_Node (This, Handled_Statement_Sequence (Node));
      Indent_End (Ob);
      
      Write_Indent_Str (Ob, "end ");
      Write_Id (Ob, Unique_Defining_Entity (Node));
      Write_Str (Ob, ";");
      Write_Eol (Ob);
      Write_Eol (Ob);
   end Generate_Subprogram_Body;
   
   -------------------------------------
   -- Generate_Subprogram_Declaration --
   -------------------------------------
   
   procedure Generate_Subprogram_Declaration 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob        : Output_Buffer := This.Get_Output_Buffer;
      Spec      : Node_Id;
      Body_Id   : Node_Id;
   begin
      Write_Comment_Line_To_Node (This, Node);
      Spec    := Specification (Node);
      Body_Id := Corresponding_Body (Node);

      Generate_Subprogram_Specification (This, Spec);
      
      Write_Str (Ob, ";");
      Write_Eol (Ob);
   end Generate_Subprogram_Declaration;
   
   ----------------------------------------------
   -- Generate_Subprogram_Renaming_Declaration --
   ----------------------------------------------
   
   procedure Generate_Subprogram_Renaming_Declaration 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Subprogram_Renaming_Declaration;
   
   ----------------------
   -- Generate_Subunit --
   ----------------------
   
   procedure Generate_Subunit 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Subunit;

end Rxada.Gen.Ch6;
