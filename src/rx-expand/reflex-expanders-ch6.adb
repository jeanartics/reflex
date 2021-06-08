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
with Nmake; use Nmake;
with Nlists; use Nlists;
with Sem_Util; use Sem_Util;
with Sem_Ch12; use Sem_Ch12;
with Types; use Types;
with Tbuild; use Tbuild;
with Snames; use Snames;
with Exp_Util; use Exp_Util;
with Sem; use Sem;
with Stand; use Stand;

with Reflex_Options; use Reflex_Options;
with Reflex.Infos; use Reflex.Infos;
with Reflex.Entities_Lists; use Reflex.Entities_Lists;
with Reflex.Expanders.Types; use Reflex.Expanders.Types;
with Reflex.Expanders.Utils; use Reflex.Expanders.Utils;
with Reflex.Expanders.Dispatch; use Reflex.Expanders.Dispatch;
with Reflex.External_Names; use Reflex.External_Names;
with Reflex.Global_Arecs; use Reflex.Global_Arecs;

package body Reflex.Expanders.Ch6 is
   
   procedure Expand_Subprogram_Body_For_Unity
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Subprogram_Body_For_Tia
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   procedure Expand_Subprogram_Body_For_Iec1131
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   
   
   
   ----------------------------------------------
   -- Expand_Abstract_Subprogram_Declaration --
   ----------------------------------------------
   
   procedure Expand_Abstract_Subprogram_Declaration 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Abstract_Subprogram_Declaration;
   
   ------------------------
   -- Expand_Body_Stub --
   ------------------------
   
   procedure Expand_Body_Stub 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Body_Stub;
   
   ---------------------------------------
   -- Expand_Defining_Operator_Symbol --
   ---------------------------------------
   
   procedure Expand_Defining_Operator_Symbol 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Defining_Operator_Symbol;
   
   -----------------------------------------
   -- Expand_Defining_Program_Unit_Name --
   -----------------------------------------
   
   procedure Expand_Defining_Program_Unit_Name 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Defining_Program_Unit_Name;
   
   -------------------------
   -- Expand_Designator --
   -------------------------
   
   procedure Expand_Designator 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Designator;
   
   ----------------------------------------
   -- Expand_Extended_Return_Statement --
   ----------------------------------------
   
   procedure Expand_Extended_Return_Statement 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Extended_Return_Statement;
   
   -------------------------------------
   -- Expand_Function_Instantiation --
   -------------------------------------
   
   procedure Expand_Function_Instantiation 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Function_Instantiation;
   
   -------------------------------------
   -- Expand_Function_Specification --
   -------------------------------------
   
   procedure Expand_Function_Specification 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Function_Specification;
   
   ------------------------------
   -- Expand_Operator_Symbol --
   ------------------------------
   
   procedure Expand_Operator_Symbol 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Operator_Symbol;
   
   ------------------------------------
   -- Expand_Parameter_Association --
   ------------------------------------
   
   procedure Expand_Parameter_Association 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Parameter_Association;
   
   ------------------------------------
   -- Expand_Parameter_Declaration --
   ------------------------------------

   procedure Expand_Parameter_Declaration
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Formal       : Entity_Id := Node; --  Defining_Identifier (Node);
      Param_Type   : Entity_Id;
   begin
      ----------------------------------
      -- 6.1  Parameter Specification --
      ----------------------------------

      --  PARAMETER_SPECIFICATION ::=
      --    DEFINING_IDENTIFIER_LIST : [ALIASED] MODE [NULL_EXCLUSION]
      --      SUBTYPE_MARK [:= DEFAULT_EXPRESSION]
      --  | DEFINING_IDENTIFIER_LIST : ACCESS_DEFINITION
      --      [:= DEFAULT_EXPRESSION]

      
      if Ekind (Formal) = E_In_Parameter then
	 null;
	 
      elsif Ekind (Formal) = E_In_Out_Parameter then
	 null;
	 
      elsif Ekind (Formal) = E_Out_Parameter then
	 null;
	 
	 -- Never arise
      else
	 raise Program_Error;
      end if;
      
      --  Param_Type := Parameter_Type (Node);
      Param_Type := Etype (Formal);
      
      if Is_Access_Type (Param_Type) then
	 null;
      else
	 null;
      end if;
      
      --  if NKind_In (Param_Type, N_Expanded_Name, N_Identifier) then
      --  	 Expand_Subtype_Mark (Param_Type);
	 
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
      --  	 Expand_Subtype_Mark (Subtyp);
	 
      --  else
      --  	 Error_Msg_N
      --  	   ("reflex does not supported this definition type ", Node);
      --  end if;
      
      --  Expand Expression if it is static.
   end Expand_Parameter_Declaration;
   
   --------------------------------------
   -- Expand_Parameter_Specification --
   --------------------------------------
   
   procedure Expand_Parameter_Specification 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Subp_Id : Entity_Id      := Unique_Defining_Entity (Node);
      Formal  : Node_Id;
   begin
      Formal := First_Formal_With_Extras (Subp_Id);
      while Present (Formal) loop
	 
	 Expand_Parameter_Declaration (This, Formal);
	 
	 Next_Formal_With_Extras (Formal);
	 exit when No (Formal);
      end loop;
   end Expand_Parameter_Specification;
   
   --------------------------------------
   -- Expand_Procedure_Instantiation --
   --------------------------------------
   
   procedure Expand_Procedure_Instantiation 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Procedure_Instantiation;
   
   --------------------------------------
   -- Expand_Procedure_Specification --
   --------------------------------------
   
   procedure Expand_Procedure_Specification 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Procedure_Specification;
   
   --------------------------------------
   -- Expand_Simple_Return_Statement --
   --------------------------------------
   
   procedure Expand_Simple_Return_Statement 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Simple_Return_Statement;
   
   ---------------------------------------
   -- Expand_Subprogram_Specification --
   ---------------------------------------
   
   procedure Expand_Subprogram_Specification
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Def_Id      : Node_Id;
      Params_Spec : List_Id;
      Kind        : Node_Kind := Nkind (Node);
      Result      : Node_Id;
   begin
      Def_Id := Unique_Defining_Entity (Node);
      
      This.Open_Scope (Def_Id);
      This.Set_In_Declarations (True);
      This.Set_Scope_Node (Node);
      
      Params_Spec := Parameter_Specifications (Node);
      
      if Kind = N_Function_Specification then
	 null;
      elsif Kind = N_Procedure_Specification then
	 null;
      else
	 Error_Msg_N
	   ("unknown subprogram type for reflex", Node);
	 return;
      end if;
      
      if Present (Params_Spec) then
	 Expand_Parameter_Specification (This, Node);	 
      end if;
      
      if Kind = N_Function_Specification then
	 Result := Subtype_Mark (Node);
	 
      else
	 null;
      end if;
      
      This.Close_Scope;
   end Expand_Subprogram_Specification;
   
   ------------------------------
   -- Expand_Subprogram_Body --
   ------------------------------
   
   procedure Expand_Subprogram_Body 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
   begin
      -- Push Scope
      
      if Reflex_Options.Plc_Target = Unity_Target then
	 Expand_Subprogram_Body_For_Unity (This, Node);
	 
      elsif Reflex_Options.Plc_Target = Tia_Target then
	 Expand_Subprogram_Body_For_Tia (This, Node);
	 
      else
	 Expand_Subprogram_Body_For_Iec1131 (This, Node);
      end if;
   end Expand_Subprogram_Body;
   
   -----------------------------------
   -- Expand_Subprogram_Body_Phase2 --
   -----------------------------------
   
   procedure Expand_Subprogram_Body_Phase2
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
   begin
      if Reflex_Options.Plc_Target = Unity_Target then
	 null; -- Expand_Subprogram_Body_For_Unity_Phase2 (This, Node);
	 
      elsif Reflex_Options.Plc_Target = Tia_Target then
	 null;
	 
      else
	 null;
      end if;
   end Expand_Subprogram_Body_Phase2;
   
   --------------------------------------
   -- Expand_Subprogram_Body_For_Unity --
   --------------------------------------
   
   procedure Expand_Subprogram_Body_For_Unity
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Spec        : Node_Id := Specification (Node);
      Decls       : List_Id;
      N           : Node_Id;
      Def_Id      : Entity_id;
      Global_Arec : access Global_Arec_Record;
      Globals     : Globals_Assoc_Lists.List;
      Lang        : Language_Type;
      Gen         : Generation_Type;
      
      Last_Real_Spec_Entity : Entity_Id := Empty;
      Body_Id               : Entity_Id := Defining_Entity (Spec);
      
      Lib_Entity : Entity_Id;
      New_Name : Name_Id;
      Nxt : Node_Id;
   begin
      Def_Id := Unique_Defining_Entity (Node);
      
      Put_Line
	("Expand_Subprogram_Body_For_Unity Begin " & 
	   Get_Name_String (Chars (Def_Id)));
      
      This.Open_Scope (Def_Id);
      This.Set_In_Declarations (True);
      This.Set_Scope_Node (Node);
      
      --  Spec := Specification (Node);
      Expand_Subprogram_Specification (This, Spec);
      
      --  Chain Body entities to Spec.
      
      --  Set_First_Entity (Def_Id, First_Entity (Body_Id));
      --  Set_Last_Entity  (Def_Id, Last_Entity (Body_Id));
      
      --  First Put Type declaration at the libray Level Declarations
      --  Here we can found full type declaration or subtype declaration
      
      Lib_Entity := Enclosing_Lib_Unit_Entity (Def_Id);
      
      Decls := Declarations (Node);
      if Is_Non_Empty_List (Decls) then
         N := First (Decls);

         while Present (N) loop
	    Nxt := Next (N);
	    if Nkind_In (N, N_Full_Type_Declaration, N_Subtype_Declaration) 
	    then
	       declare
	    	  Id : Entity_Id;
	       begin
	    	  Id := Defining_Identifier (N);
	    	  New_Name := Make_Unique_Name_In_Scope
	    	    (Chars (Id), Lib_Entity);
	    	  Set_Chars (Id, New_Name);
		  
	    	  Remove (N);
	    	  Insert_Before (Node, N);
	    	  Append_Entity (Id, Lib_Entity);
		  
	    	  Expand_Node (This, N);
	       end;
	    else
	       null;
	    end if;
            N := Nxt;
         end loop;
      end if;
      
      Decls := Declarations (Node);
      if Is_Non_Empty_List (Decls) then
         N := First (Decls);

         while Present (N) loop
	    Expand_Node (This, N);
            Next (N);
         end loop;
      end if;
      
      This.Set_In_Statements (True);
      
      Expand_Node (This, Handled_Statement_Sequence (Node));
      
      Put_Line ("After Expand Seq of Stmts");
      --  Transform function to procedure
      
      --  Create AREC entities and collect Globals.
      
      Global_Arec := New_Global_Arec;
      Set_Subprogram_Global_Arec (Def_Id, Global_Arec);
      This.Append_Arec_List (Def_Id);
      
      Put_Line ("0");
      Collect_Globals (Def_Id);
      Put_Line ("1");
      Collect_Called (Def_Id);
      Put_Line ("2");
      
      if Need_Globals_Arec (Def_Id) then
	 Put_Line ("3");
	 Create_Global_Arec_Entities (Def_Id);
      end if;
      
      Put_Line ("Before Change_Function_To_Procedure");
      if Nkind (Spec) = N_Function_Specification then
	 Change_Function_To_Procedure (Subp => Node, Goto_Form => True);
      else
	 Replace_Returns (Spec, Empty, True);
      end if;
      
      Put_Line ("After Change_Function_To_Procedure");
      
      
      --  Add AREC_List, to populate the AREC entities at the ned of expansion
      
      
      Gen := Get_Generation_Type (Def_Id);
      -- Gen := Sr_Type;
      Gen := Fb_Type;
Put_Line ("Gen = " & Gen'Img);      
      --  For Section and Sr, create for each formal, its corresponding global
      --  variable to hold the parameter on call. Then replace all reference
      --  to formal to reference to corresponding global.
      
      if Gen = Section_Type then
	 null;
	 
      elsif Gen = Sr_Type then
	 --  Move local declarations to globals
	 
	 Expand_Subprogram_As_Sr (This, Node);
	 
      --  If language is Fbd create the instance. The default generation is Fbd
      
      elsif Gen = Fb_Type or else Gen = Unknown_Generation then
	 
	 Put_Line ("Gen = Fb_Type");
	 
	 
	 --  Now create an instance for each subprogram called in the body. 
	 --  The subprogram called have been collect in Called_Entity of the
	 --  arec of the subprogram currently expanding.
	 
	 declare
	    Called_List : Called_Assoc_Lists.List;
	    Called_Gen  : Generation_Type; 
	 begin
	    Called_List := Get_Called_List (Def_Id);
            for Call of Called_List loop
	       Called_Gen := Get_Generation_Type (Call.Entity);
	       if Called_Gen = Section_Type then
		  Error_Msg_N
		    ("a section cannot be called in a dfb", Call.Entity);
	       elsif Called_Gen = Sr_Type then
		  Error_Msg_N
		    ("a sr cannot be call in a dfb", Call.Entity);
	       else		    
		  declare
		     S : String := 
		       Get_Name_String (Chars (Call.Entity)) & "_inst";
		  begin
		     Call.Instance_Name := 
		       Make_Unique_Name (This, String_Find (S));
		  end;
	       end if;
	    end loop;
	 end;
	 
      else
	 null;
      end if;
      
      
      --  If generation is Ladder then rewrite statements to remove all 
      --  constructions not supported by ladder
      
      Lang := Get_Language (Def_Id);
      if Lang = Reflex.Infos.Ladder_Language then
	 null;
      elsif Lang = Flow_Language then
	 null;
      elsif Lang = Chart_Language then
	 null;
      else
	 null;
      end if;
      
      This.Close_Scope;
      
      Put_Line ("Expand_Subprogram_Body_For_Unity End");
   end Expand_Subprogram_Body_For_Unity;
   
   ---------------------------------------------
   -- Expand_Subprogram_Body_For_Unity_Phase2 --
   ---------------------------------------------
   
   procedure Expand_Subprogram_Body_For_Unity_Phase2
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Spec        : Node_Id := Specification (Node);
      Decls       : List_Id;
      Def_Id      : Entity_id;
      Globals     : Globals_Assoc_Lists.List;
      Gen         : Generation_Type;
      
      Last_Real_Spec_Entity : Entity_Id := Empty;
      Body_Id               : Entity_Id := Defining_Entity (Spec);
      
      Lib_Entity : Entity_Id;
   begin
      Def_Id := Unique_Defining_Entity (Node);
		
      This.Open_Scope (Def_Id);
      This.Set_In_Declarations (True);
      This.Set_Scope_Node (Node);
      
      Lib_Entity := Enclosing_Lib_Unit_Entity (Def_Id);
      
      Decls := Declarations (Node);
      
      Globals := Get_Globals_List (Def_Id);
      
      Gen := Get_Generation_Type (Def_Id);
      -- Gen := Sr_Type;
      Gen := Fb_Type;
      
      --  For Section and Sr, create for each formal, its corresponding global
      --  variable to hold the parameter on call. Then replace all reference
      --  to formal to reference to corresponding global.
      
      if Gen = Section_Type then
	 null;
	 
      elsif Gen = Sr_Type then
	 --  Move local declarations to globals
	 
	 null; -- Expand_Subprogram_As_Sr (This, Node);
	 
      --  If language is Fbd create the instance. The default generation is Fbd
      
      elsif Gen = Fb_Type or else Gen = Unknown_Generation then
	 Replace_Globals (Def_Id);
      else
	 null;
      end if;
      
      This.Close_Scope;
   end Expand_Subprogram_Body_For_Unity_Phase2;
   
   ------------------------------------
   -- Expand_Subprogram_Body_For_Tia --
   ------------------------------------
   
   procedure Expand_Subprogram_Body_For_Tia
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Spec        : Node_Id := Specification (Node);
      Decls       : List_Id;
      N           : Node_Id;
      Def_Id      : Entity_id;
      Globals     : Globals_Assoc_Lists.List;
      Lang        : Language_Type;
      Gen         : Generation_Type;
      
      Last_Real_Spec_Entity : Entity_Id := Empty;
      Body_Id               : Entity_Id := Defining_Entity (Spec);
      
      Lib_Entity : Entity_Id;
      New_Name : Name_Id;
      Nxt : Node_Id;
   begin
      Def_Id := Unique_Defining_Entity (Node);
      
      This.Open_Scope (Def_Id);
      This.Set_In_Declarations (True);
      This.Set_Scope_Node (Node);
      
      --  Spec := Specification (Node);
      Expand_Subprogram_Specification (This, Spec);
      
      --  Chain Body entities to Spec.
      
      Set_First_Entity (Def_Id, First_Entity (Body_Id));
      Set_Last_Entity  (Def_Id, Last_Entity (Body_Id));
      
      --  First Put Type declaration at the libray Level Declarations
      --  Here we can found full type declaration or subtype declaration
      
      Lib_Entity := Enclosing_Lib_Unit_Entity (Def_Id);
      Decls := Declarations (Node);
      if Is_Non_Empty_List (Decls) then
         N := First (Decls);

         while Present (N) loop
	    Nxt := Next (N);
	    if Nkind_In (N, N_Full_Type_Declaration, N_Subtype_Declaration) 
	    then
	       declare
	    	  Id : Entity_Id;
	       begin
	    	  Id := Defining_Identifier (N);
	    	  New_Name := Make_Unique_Name_In_Scope
	    	    (Chars (Id), Lib_Entity);
	    	  Set_Chars (Id, New_Name);
		  
	    	  Remove (N);
	    	  Insert_Before (Node, N);
		  
	    	  Append_Entity (Id, Lib_Entity);
		  
	    	  Expand_Node (This, N);
	       end;
	    else
	       null;
	    end if;
            N := Nxt;
         end loop;
      end if;
      
      Decls := Declarations (Node);
      if Is_Non_Empty_List (Decls) then
         N := First (Decls);

         while Present (N) loop
	    Expand_Node (This, N);
            Next (N);
         end loop;
      end if;
      
      This.Set_In_Statements (True);
      
      Expand_Node (This, Handled_Statement_Sequence (Node));
      
      Gen := Get_Generation_Type (Def_Id);
      Gen := Sr_Type;
      Gen := Fb_Type;
      
      --  For Section and Sr, create for each formal, its corresponding global
      --  variable to hold the parameter on call. Then replace all reference
      --  to formal to reference to corresponding global.
      
      if Gen = Section_Type then
	 null;
	 
      elsif Gen = Sr_Type then
	 --  Move local declarations to globals
	 
	 null;
	 
      --  If language is Fbd create the instance. The default generation is Fbd
      
      elsif Gen = Fb_Type or else Gen = Unknown_Generation then
	 
	 --  Now create an instance for each subprogram called in the body. 
	 --  The subprogram called have been collect in Called_Entity of the
	 --  arec of the subprogram currently expanding.
	 
	 null;
      else
	 null;
      end if;
      
      
      --  If generation is Ladder then rewrite statements to remove all 
      --  constructions not supported by ladder
      
      Lang := Get_Language (Def_Id);
      if Lang = Reflex.Infos.Ladder_Language then
	 null;
      elsif Lang = Flow_Language then
	 null;
      elsif Lang = Chart_Language then
	 null;
      else
	 null;
      end if;
      
      This.Close_Scope;
   end Expand_Subprogram_Body_For_Tia;
   
   ----------------------------------------
   -- Expand_Subprogram_Body_For_Iec1131 --
   ----------------------------------------
   
   procedure Expand_Subprogram_Body_For_Iec1131
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Subprogram_Body_For_Iec1131;
   
   -------------------------------------
   -- Expand_Subprogram_Declaration --
   -------------------------------------
   
   procedure Expand_Subprogram_Declaration 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Spec      : Node_Id;
      Body_Id   : Node_Id;
      --  Subp_Body : Node_Id;
   begin
      Spec    := Specification (Node);
      Body_Id := Corresponding_Body (Node);
      
      -- Expand_Subprogram_Specification (Spec);
      
      --  if Present (Body_Id) then
      --  	 Put_Line (" Corresponding Body Id " & Nkind (Body_Id)'Img);
      --  	 Subp_Body := Parent (Body_Id);
      --  	 if Nkind (Subp_Body) = N_Defining_Program_Unit_Name then
      --  	    Subp_Body := Parent (Subp_Body);
      --  	 end if;
      --  	 Subp_Body := Parent (Subp_Body);
      --  	 Put_Line (" Corresponding Body " & Nkind (Subp_Body)'Img);
      --  	 Expand_Subprogram_Body (Subp_Body);
      --  else
      --  	 Write_Str (";");
      --  	 Write_Eol;
      --  end if;
      --  Write_Eol;
   end Expand_Subprogram_Declaration;
   
   -----------------------------
   -- Expand_Subprogram_As_Sr --
   -----------------------------
   
   procedure Expand_Subprogram_As_Sr
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      E            : Entity_Id;
      Formal       : Node_Id;
      Decl         : Node_Id;
      Type_Def     : Node_Id;
      Scp          : Entity_Id;
      Edecl        : Entity_Id;
      Name         : Name_Id;
      Par          : Node_Id;
      C            : Entity_Id;
      Subp_Spec    : Entity_Id;
      Subp_Dec     : Node_Id;
   begin
      E := Unique_Defining_Entity (Node);
      
      if Nkind (Node) = N_Subprogram_Body then
	 Subp_Spec := Corresponding_Spec (Node);
	 
	 if Present (Subp_Spec) then
	    Subp_Dec := Parent (Parent (Subp_Spec));
	 else
	    Subp_Dec := Node;
	 end if;
      else
	 pragma Assert (Nkind (Node) = N_Subprogram_Declaration);
	 Subp_Dec := Node;
      end if;
      
      Scp := Enclosing_Package_Entity (E);
      
      --  Formals as global variables
      
      Formal := First_Formal_With_Extras (E);
      while Present (Formal) loop
	 Par := Parent (Formal);
	 Type_Def := New_Copy_Tree (Parameter_Type (Par));
	 
	 Name := Chars (Formal);
	 Edecl := Make_Unique_Entity_In_Scope (Sloc (Formal), Name, Scp);
	 
	 Set_Formal_Global (Formal, Edecl);
	 Set_Formal_Global (Spec_Entity (Formal), Edecl);
	 
	 Decl := Make_Object_Declaration
	   (Sloc => Sloc (Formal),
	    Defining_Identifier  => Edecl,
	    Object_Definition    => Type_Def,
	    Expression           => New_Copy_Tree (Expression (Par)));
--  	    Has_Init_Expression  => Present (Expression (Par)));
	 
	 Insert_Before (Subp_Dec, Decl);
	 
	 Set_Scope (Edecl, E);
	 C := Get_Name_Entity_Id (Chars (Edecl));
	 if Present (C) then
	    Set_Homonym (Edecl, C);
	 end if;
	 Set_Name_Entity_Id (Chars (Edecl), Edecl);
	 Append_Entity (Edecl, E);
	 
	 Next_Formal_With_Extras (Formal);
      end loop;
      
      Replace_Formals_By_Globals (E);
      
   end Expand_Subprogram_As_Sr;
      
   ----------------------------------------------
   -- Expand_Subprogram_Renaming_Declaration --
   ----------------------------------------------
   
   procedure Expand_Subprogram_Renaming_Declaration 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Subprogram_Renaming_Declaration;
   
   ----------------------------------
   -- Expand_Subtype_Declaration --
   ----------------------------------
   
   procedure Expand_Subtype_Declaration 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Subtype_Declaration;
   
   ----------------------
   -- Expand_Subunit --
   ----------------------
   
   procedure Expand_Subunit 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Subunit;
   
   -----------------
   -- Expand_Call --
   -----------------

   --  This procedure handles expansion of function calls and procedure call
   --  statements (i.e. it serves as the body for Expand_N_Function_Call and
   --  Expand_N_Procedure_Call_Statement. Processing for calls includes:

   --    Replace call to Raise_Exception by Raise_Exception always if possible
   --    Provide values of actuals for all formals in Extra_Formals list
   --    Replace "call" to enumeration literal function by literal itself
   --    Rewrite call to predefined operator as operator
   --    Replace actuals to in-out parameters that are numeric conversions,
   --     with explicit assignment to temporaries before and after the call.
   --    Remove optional actuals if First_Optional_Parameter specified.

   --   Note that the list of actuals has been filled with default expressions
   --   during semantic analysis of the call. Only the extra actuals required
   --   for the 'Constrained attribute and for accessibility checks are added
   --   at this point.

   procedure Expand_Call (N : Node_Id) is
      Loc           : constant Source_Ptr := Sloc (N);
      Subp          : Entity_Id;
      Orig_Subp     : Entity_Id := Empty;
      Parent_Subp   : Entity_Id;
      Actual        : Node_Id;
      Formal        : Entity_Id;
      Prev          : Node_Id := Empty;
      Prev_Orig     : Node_Id;
      Extra_Actuals : List_Id := No_List;
   begin
      --  Ignore if previous error

      if Nkind (N) in N_Has_Etype and then Etype (N) = Any_Type then
         return;
      end if;

      Subp        := Entity (Name (N));
      Parent_Subp := Alias (Subp);
      
      --  First step, compute extra actuals, corresponding to any
      --  Extra_Formals present. Note that we do not access Extra_Formals
      --  directly, instead we simply note the presence of the extra
      --  formals as we process the regular formals and collect the
      --  corresponding actuals in Extra_Actuals.

      --  We also generate any required range checks for actuals as we go
      --  through the loop, since this is a convenient place to do this.

      Formal := First_Formal (Subp);
      Actual := First_Actual (N);
      while Present (Formal) loop

         --  Prepare to examine current entry

         Prev := Actual;
         Prev_Orig := Original_Node (Prev);

         --  If the formal is class wide and the actual is an aggregate, force
         --  evaluation so that the back end who does not know about class-wide
         --  type, does not generate a temporary of the wrong size.

         if not Is_Class_Wide_Type (Etype (Formal)) then
            null;

         elsif Nkind (Actual) = N_Aggregate
           or else (Nkind (Actual) = N_Qualified_Expression
                     and then Nkind (Expression (Actual)) = N_Aggregate)
         then
            Force_Evaluation (Actual);
         end if;

         Next_Actual (Actual);
         Next_Formal (Formal);
      end loop;
      
      --  If this is a call to an intrinsic subprogram, then perform the
      --  appropriate expansion to the corresponding tree node and we
      --  are all done (since after that the call is gone!)

      --  if Is_Intrinsic_Subprogram (Subp) then
      --     Expand_Intrinsic_Call (N, Subp);
      --     return;
      --  end if;

      if Ekind (Subp) = E_Function
        or else Ekind (Subp) = E_Procedure
      then
         if Is_Inlined (Subp) then

            declare
               Bod         : Node_Id;
               Must_Inline : Boolean := False;
               Spec        : constant Node_Id := Unit_Declaration_Node (Subp);
               Scop        : constant Entity_Id := Scope (Subp);

            begin
               --  Verify that the body to inline has already been seen,
               --  and that if the body is in the current unit the inlining
               --  does not occur earlier. This avoids order-of-elaboration
               --  problems in gigi.

               if No (Spec)
                 or else Nkind (Spec) /= N_Subprogram_Declaration
                 or else No (Body_To_Inline (Spec))
               then
                  Must_Inline := False;

               --  If this an inherited function that returns a private
               --  type, do not inline if the full view is an unconstrained
               --  array, because such calls cannot be inlined.

               elsif Present (Orig_Subp)
                 and then Is_Array_Type (Etype (Orig_Subp))
                 and then not Is_Constrained (Etype (Orig_Subp))
               then
                  Must_Inline := False;

               --  If the subprogram comes from an instance in the same
               --  unit, and the instance is not yet frozen, inlining might
               --  trigger order-of-elaboration problems in gigi.

               elsif Is_Generic_Instance (Scop)
                 and then Present (Freeze_Node (Scop))
                 and then not Analyzed (Freeze_Node (Scop))
               then
                  Must_Inline := False;

               else
                  Bod := Body_To_Inline (Spec);

		  Must_Inline := True;

               end if;

               if Must_Inline then
                  Expand_Inlined_Call (N, Subp, Orig_Subp);

               else
		  null;
               end if;
            end;
         end if;
      end if;
   end Expand_Call;

   --------------------------
   -- Expand_Inlined_Call --
   --------------------------

   procedure Expand_Inlined_Call
    (N         : Node_Id;
     Subp      : Entity_Id;
     Orig_Subp : Entity_Id)
   is
      Loc       : constant Source_Ptr := Sloc (N);
      Orig_Bod  : constant Node_Id :=
                    Body_To_Inline (Unit_Declaration_Node (Subp));

      Blk      : Node_Id;
      Bod      : Node_Id;
      Decl     : Node_Id;
      Exit_Lab : Entity_Id := Empty;
      F        : Entity_Id;
      A        : Node_Id;
      Lab_Decl : Node_Id;
      Lab_Id   : Node_Id;
      New_A    : Node_Id;
      Num_Ret  : Int := 0;
      Ret_Type : Entity_Id;
      Targ     : Node_Id;
      Temp     : Entity_Id;
      Temp_Typ : Entity_Id;

      procedure Make_Exit_Label;
      --  Build declaration for exit label to be used in Return statements.

      function Process_Formals (N : Node_Id) return Traverse_Result;
      --  Replace occurrence of a formal with the corresponding actual, or
      --  the thunk generated for it.

      function Process_Sloc (Nod : Node_Id) return Traverse_Result;
      --  If the call being expanded is that of an internal subprogram,
      --  set the sloc of the generated block to that of the call itself,
      --  so that the expansion is skipped by the -next- command in gdb.
      --  Same processing for a subprogram in a predefined file, e.g.
      --  Ada.Tags. If Debug_Generated_Code is true, suppress this change
      --  to simplify our own development.

      procedure Rewrite_Function_Call (N : Node_Id; Blk : Node_Id);
      --  If the function body is a single expression, replace call with
      --  expression, else insert block appropriately.

      procedure Rewrite_Procedure_Call (N : Node_Id; Blk : Node_Id);
      --  If procedure body has no local variables, inline body without
      --  creating block,  otherwise rewrite call with block.

      ---------------------
      -- Make_Exit_Label --
      ---------------------

      procedure Make_Exit_Label is
      begin
         --  Create exit label for subprogram, if one doesn't exist yet.

         if No (Exit_Lab) then
            Lab_Id := Make_Identifier (Loc, New_Label_Name ("Labl"));
            Set_Entity (Lab_Id,
              Make_Defining_Identifier (Loc, Chars (Lab_Id)));
            Exit_Lab := Make_Label (Loc, Lab_Id);

            Lab_Decl :=
              Make_Implicit_Label_Declaration (Loc,
                Defining_Identifier  => Entity (Lab_Id),
                Label_Construct      => Exit_Lab);
         end if;
      end Make_Exit_Label;

      ---------------------
      -- Process_Formals --
      ---------------------

      function Process_Formals (N : Node_Id) return Traverse_Result is
         A   : Entity_Id;
         E   : Entity_Id;
         Ret : Node_Id;

      begin
         if Is_Entity_Name (N)
           and then Present (Entity (N))
         then
            E := Entity (N);

            if Is_Formal (E)
              and then Scope (E) = Subp
            then
               A := Renamed_Object (E);

               if Is_Entity_Name (A) then
                  Rewrite (N, New_Occurrence_Of (Entity (A), Loc));

               elsif Nkind (A) = N_Defining_Identifier then
                  Rewrite (N, New_Occurrence_Of (A, Loc));

               else   --  numeric literal
                  Rewrite (N, New_Copy (A));
               end if;
            end if;

            return Skip;

         elsif Nkind (N) = N_Return_Statement then

            if No (Expression (N)) then
               Make_Exit_Label;
               Rewrite (N, Make_Goto_Statement (Loc,
                 Name => New_Copy (Lab_Id)));

            else
               if Nkind (Parent (N)) = N_Handled_Sequence_Of_Statements
                 and then Nkind (Parent (Parent (N))) = N_Subprogram_Body
               then
                  --  Function body is a single expression. No need for
                  --  exit label.

                  null;

               else
                  Num_Ret := Num_Ret + 1;
                  Make_Exit_Label;
               end if;

               --  Because of the presence of private types, the views of the
               --  expression and the context may be different, so place an
               --  unchecked conversion to the context type to avoid spurious
               --  errors, eg. when the expression is a numeric literal and
               --  the context is private. If the expression is an aggregate,
               --  use a qualified expression, because an aggregate is not a
               --  legal argument of a conversion.

               if Nkind (Expression (N)) = N_Aggregate
                 or else Nkind (Expression (N)) = N_Null
               then
                  Ret :=
                    Make_Qualified_Expression (Sloc (N),
                       Subtype_Mark => New_Occurrence_Of (Ret_Type, Sloc (N)),
                       Expression => Relocate_Node (Expression (N)));
               else
                  Ret :=
                    Unchecked_Convert_To
                      (Ret_Type, Relocate_Node (Expression (N)));
               end if;

               if Nkind (Targ) = N_Defining_Identifier then
                  Rewrite (N,
                    Make_Assignment_Statement (Loc,
                      Name => New_Occurrence_Of (Targ, Loc),
                      Expression => Ret));
               else
                  Rewrite (N,
                    Make_Assignment_Statement (Loc,
                      Name => New_Copy (Targ),
                      Expression => Ret));
               end if;

               Set_Assignment_OK (Name (N));

               if Present (Exit_Lab) then
                  Insert_After (N,
                    Make_Goto_Statement (Loc,
                      Name => New_Copy (Lab_Id)));
               end if;
            end if;

            return OK;

         --  Remove pragma Unreferenced since it may refer to formals that
         --  are not visible in the inlined body, and in any case we will
         --  not be posting warnings on the inlined body so it is unneeded.

         elsif Nkind (N) = N_Pragma
           and then Chars (N) = Name_Unreferenced
         then
            Rewrite (N, Make_Null_Statement (Sloc (N)));
            return OK;

         else
            return OK;
         end if;
      end Process_Formals;

      procedure Replace_Formals is new Traverse_Proc (Process_Formals);

      ------------------
      -- Process_Sloc --
      ------------------

      function Process_Sloc (Nod : Node_Id) return Traverse_Result is
      begin
         Set_Sloc (Nod, Sloc (N));
         Set_Comes_From_Source (Nod, False);

         return OK;
      end Process_Sloc;

      procedure Reset_Slocs is new Traverse_Proc (Process_Sloc);

      ---------------------------
      -- Rewrite_Function_Call --
      ---------------------------

      procedure Rewrite_Function_Call (N : Node_Id; Blk : Node_Id) is
         HSS : constant Node_Id := Handled_Statement_Sequence (Blk);
         Fst : constant Node_Id := First (Statements (HSS));

      begin
         --  Optimize simple case: function body is a single return statement,
         --  which has been expanded into an assignment.

         if Is_Empty_List (Declarations (Blk))
           and then Nkind (Fst) = N_Assignment_Statement
           and then No (Next (Fst))
         then

            --  The function call may have been rewritten as the temporary
            --  that holds the result of the call, in which case remove the
            --  now useless declaration.

            if Nkind (N) = N_Identifier
              and then Nkind (Parent (Entity (N))) = N_Object_Declaration
            then
               Rewrite (Parent (Entity (N)), Make_Null_Statement (Loc));
            end if;

            Rewrite (N, Expression (Fst));

         elsif Nkind (N) = N_Identifier
           and then Nkind (Parent (Entity (N))) = N_Object_Declaration
         then

            --  The block assigns the result of the call to the temporary.

            Insert_After (Parent (Entity (N)), Blk);

         elsif Nkind (Parent (N)) = N_Assignment_Statement
           and then Is_Entity_Name (Name (Parent (N)))
         then

            --  Replace assignment with the block

            Rewrite (Parent (N), Blk);

         elsif Nkind (Parent (N)) = N_Object_Declaration then
            Set_Expression (Parent (N), Empty);
            Insert_After (Parent (N), Blk);
         end if;
      end Rewrite_Function_Call;

      ----------------------------
      -- Rewrite_Procedure_Call --
      ----------------------------

      procedure Rewrite_Procedure_Call
	(N   : Node_Id; 
	 Blk : Node_Id) is
	 
         HSS  : constant Node_Id := Handled_Statement_Sequence (Blk);
      begin
         if Is_Empty_List (Declarations (Blk)) then
            Insert_List_After (N, Statements (HSS));
            Rewrite (N, Make_Null_Statement (Loc));
         else
            Rewrite (N, Blk);
         end if;
      end Rewrite_Procedure_Call;

   --  Start of processing for Expand_Inlined_Call

   begin
      if Nkind (Orig_Bod) = N_Defining_Identifier then

         --  Subprogram is a renaming_as_body. Calls appearing after the
         --  renaming can be replaced with calls to the renamed entity
         --  directly, because the subprograms are subtype conformant.

         Set_Name (N, New_Occurrence_Of (Orig_Bod, Loc));
         return;
      end if;

      --  Use generic machinery to copy body of inlined subprogram, as if it
      --  were an instantiation, resetting source locations appropriately, so
      --  that nested inlined calls appear in the main unit.

      Save_Env (Subp, Empty);
      Set_Copied_Sloc_For_Inlined_Body (N, Defining_Entity (Orig_Bod));

      Bod := Copy_Generic_Node (Orig_Bod, Empty, Instantiating => True);
      Blk := Make_Block_Statement 
	(Loc,
	 Declarations               => Declarations (Bod),
	 Handled_Statement_Sequence => Handled_Statement_Sequence (Bod));

      if No (Declarations (Bod)) then
         Set_Declarations (Blk, New_List);
      end if;

      --  If this is a derived function, establish the proper return type.

      if Present (Orig_Subp)
        and then Orig_Subp /= Subp
      then
         Ret_Type := Etype (Orig_Subp);
      else
         Ret_Type := Etype (Subp);
      end if;

      F := First_Formal (Subp);
      A := First_Actual (N);

      --  Create temporaries for the actuals that are expressions, or that
      --  are scalars and require copying to preserve semantics.

      while Present (F) loop
         if Present (Renamed_Object (F)) then
            Error_Msg_N (" cannot inline call to recursive subprogram", N);
            return;
         end if;

         --  If the argument may be a controlling argument in a call within
         --  the inlined body, we must preserve its classwide nature to
         --  insure that dynamic dispatching take place subsequently.
         --  If the formal has a constraint it must be preserved to retain
         --  the semantics of the body.

         if Is_Class_Wide_Type (Etype (F))
           or else (Is_Access_Type (Etype (F))
                      and then
                    Is_Class_Wide_Type (Designated_Type (Etype (F))))
         then
            Temp_Typ := Etype (F);

         elsif Base_Type (Etype (F)) = Base_Type (Etype (A))
           and then Etype (F) /= Base_Type (Etype (F))
         then
            Temp_Typ := Etype (F);

         else
            Temp_Typ := Etype (A);
         end if;

         --  If the actual is a simple name or a literal, no need to
         --  create a temporary, object can be used directly.

         if (Is_Entity_Name (A)
              and then
               (not Is_Scalar_Type (Etype (A))
                 or else Ekind (Entity (A)) = E_Enumeration_Literal))

           or else Nkind (A) = N_Real_Literal
           or else Nkind (A) = N_Integer_Literal
           or else Nkind (A) = N_Character_Literal
         then
            if Etype (F) /= Etype (A) then
               Set_Renamed_Object
                (F, Unchecked_Convert_To (Etype (F), Relocate_Node (A)));
            else
               Set_Renamed_Object (F, A);
            end if;

         else
            Temp :=
              Make_Defining_Identifier (Loc,
                Chars => New_Internal_Name ('C'));

            --  If the actual for an in/in-out parameter is a view conversion,
            --  make it into an unchecked conversion, given that an untagged
            --  type conversion is not a proper object for a renaming.

            --  In-out conversions that involve real conversions have already
            --  been transformed in Expand_Actuals.

            if Nkind (A) = N_Type_Conversion
              and then Ekind (F) /= E_In_Parameter
            then
               New_A := Make_Unchecked_Type_Conversion (Loc,
                 Subtype_Mark => New_Occurrence_Of (Etype (F), Loc),
                 Expression   => Relocate_Node (Expression (A)));

            elsif Etype (F) /= Etype (A) then
               New_A := Unchecked_Convert_To (Etype (F), Relocate_Node (A));
               Temp_Typ := Etype (F);

            else
               New_A := Relocate_Node (A);
            end if;

            Set_Sloc (New_A, Sloc (N));

            if Ekind (F) = E_In_Parameter
            then
               Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Constant_Present    => True,
                   Object_Definition   => New_Occurrence_Of (Temp_Typ, Loc),
                   Expression          => New_A);
            else
               Decl :=
                 Make_Object_Renaming_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Subtype_Mark        => New_Occurrence_Of (Temp_Typ, Loc),
                   Name                => New_A);
            end if;

            Prepend (Decl, Declarations (Blk));
            Set_Renamed_Object (F, Temp);
         end if;

         Next_Formal (F);
         Next_Actual (A);
      end loop;

      --  Establish target of function call. If context is not assignment or
      --  declaration, create a temporary as a target. The declaration for
      --  the temporary may be subsequently optimized away if the body is a
      --  single expression, or if the left-hand side of the assignment is
      --  simple enough.

      if Ekind (Subp) = E_Function then
         if Nkind (Parent (N)) = N_Assignment_Statement
           and then Is_Entity_Name (Name (Parent (N)))
         then
            Targ := Name (Parent (N));

         else
            --  Replace call with temporary, and create its declaration.

            Temp :=
              Make_Defining_Identifier (Loc, New_Variable_Name ("C"));

            Decl :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Temp,
                Object_Definition =>
                  New_Occurrence_Of (Ret_Type, Loc));

            Set_No_Initialization (Decl);
            Insert_Action (N, Decl);
            Rewrite (N, New_Occurrence_Of (Temp, Loc));
            Targ := Temp;
         end if;
      end if;

      --  Traverse the tree and replace  formals with actuals or their thunks.
      --  Attach block to tree before analysis and rewriting.

      Replace_Formals (Blk);
      Set_Parent (Blk, N);

      if not Comes_From_Source (Subp)
      then
         Reset_Slocs (Blk);
      end if;

      if Present (Exit_Lab) then

         --  If the body was a single expression, the single return statement
         --  and the corresponding label are useless.

         if Num_Ret = 1
           and then
             Nkind (Last (Statements (Handled_Statement_Sequence (Blk)))) =
               N_Goto_Statement
         then
            Remove (Last (Statements (Handled_Statement_Sequence (Blk))));
         else
            Append (Lab_Decl, (Declarations (Blk)));
            Append (Exit_Lab, Statements (Handled_Statement_Sequence (Blk)));
         end if;
      end if;

      --  Analyze Blk with In_Inlined_Body set, to avoid spurious errors on
      --  conflicting private views that Gigi would ignore. If this is a
      --  predefined unit, analyze with checks off, as is done in the non-
      --  inlined run-time units.

      declare
         I_Flag : constant Boolean := In_Inlined_Body;

      begin
         In_Inlined_Body := True;
	 Analyze (Blk);
         In_Inlined_Body := I_Flag;
      end;
      
      if Ekind (Subp) = E_Procedure then
         Rewrite_Procedure_Call (N, Blk);
      else
         Rewrite_Function_Call (N, Blk);
      end if;

      Restore_Env;

      --  Cleanup mapping between formals and actuals, for other expansions.

      F := First_Formal (Subp);

      while Present (F) loop
         Set_Renamed_Object (F, Empty);
         Next_Formal (F);
      end loop;
   end Expand_Inlined_Call;
   
   -------------------------------
   -- Change_Return_To_Goto_End --
   -------------------------------
   
   procedure Replace_Returns
     (Subp      : Node_Id;
      Param_Id  : Entity_Id;
      Goto_Form : Boolean := False) is
      
      Loc      : constant Source_Ptr := Sloc (Subp);
      Exit_Lab : Entity_Id;
      Lab_Id   : Node_Id;
      Lab_Decl : Node_Id;
      Num_Ret  : Natural := 0;
      
      function Need_Label (Stmts : List_Id) return Boolean;
      --  Return True, if the return replacement need to create a label and is
      --  translate into a goto form. Its is true when one of the following
      --  rule is verified :
      --  * There is only one return statement and this statement is le last 
      --    statement.
      --  * There is either only one coumpond statement N_Declare, 
      --    N_If_Statement or N_Case_Statement and all alternatives not 
      --    Need_label. For N_If_Statemnt and N_Case_Statment, the default 
      --    alternative (else or others) The Need_Label is false if either
      --    there is only one return statement and is the last statement of
      --    the alternative, or there is no return statement.
      
      procedure Make_Exit_Label;
      --  Build declaration for exit label to be used in Return statements.

      procedure Change_Returns (Stmts : List_Id);
      --  Replace each return statement found in the list Stmts with an
      --  assignment of the return expression to parameter Param_Id.
      
      ----------------
      -- Need_Label --
      ----------------
      
      function Need_Label (Stmts : List_Id) return Boolean is
      begin
	 return True;
      end Need_Label;
      
      ---------------------
      -- Make_Exit_Label --
      ---------------------

      procedure Make_Exit_Label is
      begin
         --  Create exit label for subprogram, if one doesn't exist yet.

         if No (Exit_Lab) then
	    Put_Line ("Create Label Begin");
	    
            Lab_Id := Make_Identifier (Loc, New_Label_Name ("End"));
            Set_Entity (Lab_Id,
              Make_Defining_Identifier (Loc, Chars (Lab_Id)));
            Exit_Lab := Make_Label (Loc, Lab_Id);

            Lab_Decl :=
              Make_Implicit_Label_Declaration
	      (Loc, Defining_Identifier  => Entity (Lab_Id));
	    
	    Put_Line ("Create Label End");
         end if;
      end Make_Exit_Label;

      --------------------
      -- Change_Returns --
      --------------------

      procedure Change_Returns (Stmts : List_Id) is
	 
         Stmt : Node_Id;
      begin
         Stmt := First (Stmts);
         while Present (Stmt) loop
            if Nkind (Stmt) = N_Block_Statement then
               Change_Returns (Statements (Stmt));

            elsif Nkind (Stmt) = N_Case_Statement then
               declare
                  Alt : Node_Id;
               begin
                  Alt := First (Alternatives (Stmt));
                  while Present (Alt) loop
                     Change_Returns (Statements (Alt));
                     Next (Alt);
                  end loop;
               end;

            elsif Nkind (Stmt) = N_If_Statement then
               Change_Returns (Then_Statements (Stmt));
               Change_Returns (Else_Statements (Stmt));

               declare
                  Part : Node_Id;
               begin
                  Part := First (Elsif_Parts (Stmt));
                  while Present (Part) loop
                     Change_Returns (Then_Statements (Part));
                     Next (Part);
                  end loop;
               end;

            elsif Nkind (Stmt) = N_Loop_Statement then
               Change_Returns (Statements (Stmt));

            elsif Nkind (Stmt) = N_Return_Statement then
	       
	       Num_Ret := Num_Ret + 1;
	       
	       if Present (Param_Id) then
		  --  Generate:
		  --    Param := Expr;
		  --    return;
		  
		  Insert_Before 
		    (Stmt,
		     Make_Assignment_Statement 
		       (Sloc (Stmt),
			Name       => New_Occurrence_Of (Param_Id, Loc),
			Expression => Relocate_Node (Expression (Stmt))));
	       end if;
	       
	       if Goto_Form then
		  --  Replace Return by goto _lend
		  
		  Make_Exit_Label;
	       
		  Rewrite 
		    (Stmt, 
		     Make_Goto_Statement (Loc, Name => New_Copy (Lab_Id)));
	       else
		  Rewrite (Stmt, Make_Return_Statement (Loc));
	       end if;
            end if;

            Next (Stmt);
         end loop;
      end Change_Returns;
      
      Stmts : List_Id;
      
      --  Start of Change_Return_To_Goto_End
   begin
      Put_Line ("Replace_Returns Begin");
      pragma Assert (Nkind (Subp) = N_Subprogram_Body);
      
      Stmts := Statements (Handled_Statement_Sequence (Subp));
      
      Exit_Lab := Empty;
      
      Change_Returns (Stmts);
      
      if Present (Exit_Lab) then
      
	 --  If the body was a single expression, the single return statement
	 --  and the corresponding label are useless.
	 
	 if Num_Ret = 1
	   and then Nkind (Last (Stmts)) =  N_Goto_Statement
	 then
	    Remove (Last (Stmts));
	 else
	    Append (Lab_Decl, (Declarations (Subp)));
	    Append (Exit_Lab, Stmts);
	 end if;
      end if;
      Put_Line ("Replace_Returns End");
   end Replace_Returns;
   
   ----------------------------------
   -- Change_Function_To_Procedure --
   ----------------------------------
   
   procedure Change_Function_To_Procedure
     (Subp      : Node_Id;
      Goto_Form : Boolean := False) is
      
      Spec     : Node_Id;
      Subp_Id  : Entity_Id;
      Ret_Type : Entity_Id;
      Param_Id : Entity_Id;
   begin
      Put_Line ("Change_Function_To_Procedure Begin " & Nkind (Subp)'Img);
      Spec    := Specification (Subp);
      Subp_Id := Unique_Defining_Entity (Subp);
      
      pragma Assert (Nkind (Spec) = N_Function_Specification);
      
      --  Create a formal out parameter to hold the result of the function.
      
      Ret_Type := Base_Type (Entity (Subtype_Mark (Spec)));
      
      Param_Id := Make_Defining_Identifier
	(Sloc  => Sloc (Subp_Id),
	 Chars => String_Find (Parameter_Prefix));
      
      --  Formal := Make_Parameter_Specification
      --  	(Sloc                => Sloc (Subp_Id),
      --  	 Defining_Identifier => Param_Id,
      --  	 Out_Present         => True,
      --  	 Parameter_Type      => 
      --  	   New_Occurrence_Of (Ret_Type, Sloc (Subp_Id)));
      --  Put_Line ("4");
      
      --  Set the entities attributes 
      
      Set_Ekind (Param_Id, E_Out_Parameter);
      Set_Etype (Param_Id, Ret_Type);
      Set_Scope (Param_Id, Subp_Id);
      
      Append_Entity (Param_Id, Subp_Id);
      
      --  Add an AREC extra formal to the Subprogram
      
      Add_Extra_Formal (Subp_Id, Param_Id);
      
      --  Asosociate the estra formal with the function to retreive it
      --  when expanding call.
      
      Set_Function_Result_Formal (Subp_Id, Param_Id);
      
      Replace_Returns (Subp, Param_Id, Goto_Form);
      
      Put_Line ("Change_Function_To_Procedure End");
   end Change_Function_To_Procedure;
   
end Reflex.Expanders.Ch6;
