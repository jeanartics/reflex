------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
--                                                                          --
-- Reflex is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware Foundation; either version 3, or (at your option) any later version --
-- Reflex is dstributed in the hope that it will be useful, but WITH-      --
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
with Sinfo; use Sinfo;
with Einfo; use Einfo;
with Elists; use Elists;
with Errout; use Errout;
with Namet; use Namet;
with Nlists; use Nlists;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Uintp; use Uintp;
with Urealp; use Urealp;
with Stand; use Stand;
with Snames; use Snames;

with Artics.Buffers; use Artics.Buffers;
with Artics.Strings_Stocks; use Artics.Strings_Stocks;

with Reflex.Gen.Outputs; use Reflex.Gen.Outputs;
with Reflex.Gen.Utils; use Reflex.Gen.Utils;
with Unity.Gen.Ch2; use Unity.Gen.Ch2;
with Unity.Gen.Ch4; use Unity.Gen.Ch4;
with Reflex.Infos; use Reflex.Infos;
with Reflex.Predicates; use Reflex.Predicates;

package body Unity.Gen.Ch3 is

   Option_Generate_Enum_Literal : boolean := False;
   
   procedure Check_Components
     (Node             : Node_Id;
      Clist            : Node_Id;
      Allow_Last_Field : Boolean);
   --  Check validity of components in Clist. Emit an error if a
   --  type whose size depends on a discriminant is found, unless
   --  Allow_Last_Field is True and this is the type of the last
   --  field in a record.
   
   ----------------------
   -- Check_Components --
   ----------------------

   procedure Check_Components
     (Node             : Node_Id;
      Clist            : Node_Id;
      Allow_Last_Field : Boolean) is
   begin
     null;
   end Check_Components;
   
   -------------------------------
   -- Generate_Type_Declaration --
   -------------------------------
   
   procedure Generate_Type_Declaration
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Previous_Ob : Output_Buffer;
   begin
      -----------------------------
      -- 3.2.1  Type Declaration --
      -----------------------------

      --  TYPE_DECLARATION ::=
      --    FULL_TYPE_DECLARATION
      --  | INCOMPLETE_TYPE_DECLARATION
      --  | PRIVATE_TYPE_DECLARATION
      --  | PRIVATE_EXTENSION_DECLARATION
      
      Previous_Ob := This.Get_Output_Buffer;
      This.Set_Output_Buffer (This.Get_Current_Types_Buffer);
      
      if Nkind (Node) = N_Full_Type_Declaration then
	 Generate_Full_Type_Declaration (This, Node);
	 
      elsif Nkind (Node) = N_Incomplete_Type_Declaration then
	 Generate_Incomplete_Type_Declaration (This, Node);
	 
      elsif Nkind (Node) = N_Private_Type_Declaration then
	 Generate_Private_Type_Declaration (This, Node);
	 
      elsif Nkind (Node) = N_Private_Extension_Declaration then
	 Generate_Private_Extension_Declaration (This, Node);
	 
      else
	 This.Set_Output_Buffer (Previous_Ob);
	 raise Program_Error;
      end if;
      
      This.Set_Output_Buffer (Previous_Ob);
   end Generate_Type_Declaration;
   
   ------------------------------------
   -- Generate_Full_Type_Declaration --
   ------------------------------------
   
   procedure Generate_Full_Type_Declaration 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob       : Output_Buffer := This.Get_Output_Buffer;
      Def_Id   : Entity_Id := Defining_Entity (Node);
      Type_Def : Node_Id;
   begin
      Put_Line 
        ("Generate_Type_Declaration Begin " & Get_String (Chars (Def_Id)));
      
      ----------------------------------
      -- 3.2.1  Full Type Declaration --
      ----------------------------------

      --  FULL_TYPE_DECLARATION ::=
      --    type DEFINING_IDENTIFIER [KNOWN_DISCRIMINANT_PART]
      --      is TYPE_DEFINITION
      --        [ASPECT_SPECIFICATIONS];
      --  | TASK_TYPE_DECLARATION
      --  | PROTECTED_TYPE_DECLARATION
      
      --  Discrinats are not alowed in Reflex. 
      
      ----------------------------
      -- 3.2.1  Type Definition --
      ----------------------------

      --  TYPE_DEFINITION ::=
      --    ENUMERATION_TYPE_DEFINITION  | INTEGER_TYPE_DEFINITION
      --  | REAL_TYPE_DEFINITION         | ARRAY_TYPE_DEFINITION
      --  | RECORD_TYPE_DEFINITION       | ACCESS_TYPE_DEFINITION
      --  | DERIVED_TYPE_DEFINITION      | INTERFACE_TYPE_DEFINITION
      
      --  Discrinats are not alowed in Reflex. 
      
      --  Do not generate Anonymous types.
      
      if Is_Anonym (Def_Id) then
	 return;
      end if;
      
      --  Write_Itypes_In_Subtree (This, Node);
	 
      Type_Def := Type_Definition (Node);
      
      pragma Assert (Nkind (Type_Def) /= N_Access_Function_Definition);
      pragma Assert (Nkind (Type_Def) /= N_Access_Procedure_Definition);
      
      --  ENUMERATION_TYPE_DEFINITION :
      --  The enumeration type is translate as integer in Unity. The
      --  literals are replaced by their values, unless the option 
      --  Generate_Enumeration_Literal is set and then the literals are
      --  translate as integer constant wich value is the representation
      --  value of the literal
      
      Put_Line ("Type_Def " & Nkind (Type_Def)'Img);

      if NKind (Type_Def) = N_Enumeration_Type_Definition then
	 null;
	 
	 --  INTEGER_TYPE_DEFINITION : 
	 --  They are emit as integer or unsigned if the type is a modular
	 --  type. So nothing to do with discrete type as there are declared
	 --  as integer_x or unisgned_x for modular types, with x the size
	 --  in bits of the type (8, 16, 32, 64)
	 
      elsif Nkind (Type_Def) = N_Signed_Integer_Type_Definition then
	 Generate_Signed_Integer_Type_Definition (This, Node);
	 
      elsif Nkind (Type_Def) = N_Modular_Type_Definition then
	 Generate_Modular_Type_Definition (This, Node);
	 
	 --  REAL_TYPE_DEFINITION 
	 --  Emits as real types, so no need to declared it
	    
      elsif Nkind (Type_Def) = N_Floating_Point_Definition then
	 Generate_Floating_Point_Definition (This, Node);
	 
	 --  RECORD_TYPE_DEFINITION
	 
      elsif Nkind (Type_Def) = N_Record_Definition then
	 Generate_Record_Declaration (This, Node);
	 
         --  ARRAY_TYPE_DEFINITION 
         
      elsif Nkind (Type_Def) = N_Unconstrained_Array_Definition then
         Generate_Unconstrained_Array_Declaration (This, Node);
	    
      elsif Nkind (Type_Def) = N_Constrained_Array_Definition then
         Generate_Constrained_Array_Declaration (This, Node);
	    
         --  ACCESS_TYPE_DEFINITION
	    
      elsif Nkind (Type_Def) = N_Access_To_Object_Definition then
         Generate_Access_To_Object_Definition (This, Node);
	    
         --  DERIVED_TYPE_DEFINITION
	    
      elsif Nkind (Type_Def) = N_Derived_Type_Definition then
         Generate_Derived_Type_Definition (This, Node);
	    
      elsif Nkind (Type_Def) = N_Subtype_Indication then
	 null; -- Generate_Subtype_Mark (This, Subtype_Mark (Type_Def));
	 
      elsif Nkind_In (Type_Def, N_Identifier, N_Expanded_Name) then
	 null; -- Generate_Subtype_Mark (This, Type_Def);
	 
      else
         Error_Msg_N
           ("unknown reflex type definition for type ", Def_Id);
      end if;
      
      --  Set Output to null as the entry point of each declarations
      --  mmust set it to their owns buffer
      
      Set_Output_Buffer (This, null);
   end Generate_Full_Type_Declaration;
   
   ----------------------------------------------
   -- Generate_Abstract_Subprogram_Declaration --
   ----------------------------------------------
   
   procedure Generate_Abstract_Subprogram_Declaration 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Abstract_Subprogram_Declaration;
   
   -----------------------------------------
   -- Generate_Access_Function_Definition --
   -----------------------------------------
   
   procedure Generate_Access_Function_Definition 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Access_Function_Definition;
   
   ------------------------------------------
   -- Generate_Access_Procedure_Definition --
   ------------------------------------------
   
   procedure Generate_Access_Procedure_Definition 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Access_Procedure_Definition;
   
   ------------------------------------
   -- Generate_Component_Association --
   ------------------------------------
   
   procedure Generate_Component_Association 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Component_Association;
   
   -----------------------------------
   -- Generate_Component_Definition --
   -----------------------------------
   
   procedure Generate_Component_Definition 
     (This       : access Unity_Generator_Record;
      Node       : Node_Id;
      From_Array : Boolean := False) is
      
      Ob         : Output_Buffer := This.Get_Output_Buffer;
      Subtyp     : Node_Id;
      Access_Def : Node_Id;
--        Mark       : Node_Id;
--        Constr     : Node_Id;
   begin
      -------------------------------
      -- 3.6  Component Definition --
      -------------------------------

      --  COMPONENT_DEFINITION ::=
      --    [aliased] [NULL_EXCLUSION] SUBTYPE_INDICATION | ACCESS_DEFINITION

      --  In Reflex mode, the component definition is a subtype mark. But in 
      --  relax Reflex access and anonymous  array with constraints are
      --  alloxed. 
      
      Subtyp := Subtype_Indication (Node);
      if Present (Subtyp) then
	 if Nkind (Subtyp) = N_Subtype_Indication then
	    
	    --  Mark := Subtype_Mark (Subtyp);
	    --  Generate_Subtype_Mark (This, Mark);
	    
	    --  Constr := Constraint (Subtyp);
	    --  if Present (Constr) then
	    --     Generate_Node (This, Constr);
	    --  end if;
	    
	    Generate_Subtype_Indication (This, Subtyp);
	    
	 elsif Nkind_In (Nkind (Subtyp), N_Expanded_Name, N_Identifier) then
	    
	    --  For recomponent if the refrenced Type is anonymous, we generate
	    --  the Type Definition of the base Type of the refrenced Type.
	    
	    Handle_Anonymous_Type (This, Entity (Subtyp), From_Array);
	    
	 elsif Nkind (Subtyp) = N_Constrained_Array_Definition then    
	    Generate_Constrained_Array_Definition (This, Subtyp, True);
	 else
	    Error_Msg_N
	      ("reflex does not supported this definition type ", Node);
	 end if;
	    
      else
	 Access_Def := Access_Definition (Node);
	 if Present (Access_Def) then
	    Generate_Access_Definition (This, Access_Def);
	 else
	    Error_Msg_N
	      ("reflex does not supported this definition type ", Node);
	 end if;
      end if;
      
   end Generate_Component_Definition;
   
   ------------------------------------
   -- Generate_Component_Declaration --
   ------------------------------------
   
   procedure Generate_Component_Declaration 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob         : Output_Buffer := This.Get_Output_Buffer;
      Def_Id     : Entity_Id;
      Comp_Def   : Node_Id;
      Com        : Str_Id;
      Need_Eol   : Boolean;
      -- Arec_Id  : Entity_Id;
   begin
      pragma Assert (Nkind (Node) = N_Component_Declaration);
      
      --------------------------------
      -- 3.8  Component Declaration --
      --------------------------------

      --  COMPONENT_DECLARATION ::=
      --    DEFINING_IDENTIFIER_LIST : COMPONENT_DEFINITION
      --      [:= DEFAULT_EXPRESSION]
      --        [ASPECT_SPECIFICATIONS];

      Def_Id := Defining_Identifier (Node);
      
      Write_Indent_Str (Ob, "<variables name=""");
      Write_Id (Ob, Def_Id);
      Write_Str (Ob, """ typeName=""");
      
      Comp_Def := Component_Definition (Node);
      Generate_Component_Definition (This, Comp_Def);
      
      Write_Str (Ob, """>");
      
      Need_Eol := False;
      
      --  Gen_Var_Comment (Ob, N);
      
      Com := Get_Entity_Comment (Def_Id);
      if Com /= No_Str_Id then
	 Need_Eol := True;
	 Write_Eol (Ob);
	 Write_Indent_Str
           (Ob, "<comment>" & 
              Normalize_Comment (Get_String (Com)) & "</comment>");
      end if;
      
      if Need_Eol then
	 Write_Eol (Ob);
	 Write_Indent_Str (Ob, "</variables>");
      else
	 Write_Str (Ob, "</variables>");
      end if;
      
      Write_Eol (Ob);
   end Generate_Component_Declaration;
   
   -----------------------------
   -- Generate_Null_Component --
   -----------------------------
   
   procedure Generate_Null_Component (Ob : Output_Buffer) is
   begin
      Write_Indent_Str
	(Ob, "<variables name=""Ignore"" typeName=""INT"">");
      
      --  Gen_Var_Comment (Ob, N);
      
      Write_Eol (Ob);
      Indent_Begin (Ob);
      Write_Indent_Str
	(Ob, "<comment>Generate by reflex for null record</comment>");
      Indent_End (Ob);
      
      Write_Eol (Ob);
      Write_Indent_Str (Ob, "</variables>");
      Write_Eol (Ob);
   end Generate_Null_Component;
      
   -----------------------------
   -- Generate_Component_List --
   -----------------------------
   
   procedure Generate_Component_List 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob        : Output_Buffer := This.Get_Output_Buffer;
      Comp_List : Node_Id;
      Items     : List_Id;
      N         : Node_Id;
   begin
      pragma Assert (Nkind (Node) = N_Record_Definition);
      
      Indent_Begin (Ob);
      
      if Null_Present (Node) then
	 Generate_Null_Component (Ob);
	 
      else	
	 Comp_List := Component_List (Node);
	 Items     := Component_Items (Comp_List);
	 if Is_Non_Empty_List (Items) then
	    N := First (Items);
	    
	    while Present (N) loop
	       if Nkind (N) = N_Component_Declaration then
		  Generate_Component_Declaration (This, N);
	       end if;
	       Next (N);
	    end loop;
	    
	 else
	    Generate_Null_Component (Ob);
	 end if;
      end if;
      
      Indent_End (Ob);
   end Generate_Component_List;
   
   -------------------------------
   -- Generate_Delta_Constraint --
   -------------------------------
   
   procedure Generate_Delta_Constraint 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Delta_Constraint;
   
   --------------------------------------
   -- Generate_Derived_Type_Definition --
   --------------------------------------
   
   procedure Generate_Derived_Type_Definition 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob               : Output_Buffer := This.Get_Output_Buffer;
      Def_Id           : Entity_Id := Defining_Identifier (Node);
      Type_Def         : Node_Id;
      Ancestror_Type   : Node_Id;
      Record_Extension : Node_Id;
   begin
      ----------------------------------
      -- 3.4  Derived Type Definition --
      ----------------------------------

      --  DERIVED_TYPE_DEFINITION ::=
      --    [abstract] [limited] new [NULL_EXCLUSION] parent_SUBTYPE_INDICATION
      --    [[and INTERFACE_LIST] RECORD_EXTENSION_PART]
      
      Type_Def         := Type_Definition (Node);
      Record_Extension := Record_Extension_Part (Type_Def);
      if Present (Record_Extension) then
	 
	 Set_Output_Buffer (This, Get_Types_Output_Buffer (This));
	 Ob := This.Get_Output_Buffer;
	 
	 --  RECORD_DEFINITION ::=
	 --    record
	 --      COMPONENT_LIST
	 --    end record
	 --  | null record
	 
	 Write_Indent_Str (Ob, "<DDTSource DDTName=""");
	 Write_Id (Ob, Def_Id);
	 Write_Str
	   (Ob, """ version=""0.02"" dateTime=""dt#2017-01-23-18:31:26"">");
	 Write_Eol (Ob);
	 
	 Indent_Begin (Ob);
	 Write_Indent_Str (Ob, "<structure>");
	 Write_Eol (Ob);
	 
	 Indent_Begin (Ob);
	 
	 --  Write_Indent_Str (Ob, "<variables name=""");
	 --  Write_Str (Ob, "_parent");
	 --  Write_Str (Ob, """ typeName=""");
	 
	 --  pragma Assert
	 --    (Nkind_In 
	 --       (Subtype_Indication (Type_Def), N_Identifier, N_Expanded_Name));
	 
	 --  Write_Id (Ob, Base_Type (Entity (Subtype_Indication (Type_Def))));
	 --  Write_Str (Ob, """>");
	 
	 --  Write_Eol (Ob);
	 --  Write_Indent_Str(Ob, "<comment>" & "parent type" & "</comment>");
	 --  Write_Eol (Ob);
	 --  Write_Indent_Str (Ob, "</variables>");
      
	 --  Write_Eol (Ob);
	 
	 --  Now Generate the components of extension part
	 
	 Generate_Component_List (This, Record_Extension);
	 
	 Indent_End (Ob);
	 Write_Indent_Str (Ob, "</structure>");
	 Write_Eol (Ob);
	 
	 Indent_End (Ob);
	 Write_Indent_Str (Ob, "</DDTSource>");
	 Write_Eol (Ob);
	 
      else 
	 Ancestror_Type := Subtype_Indication (Type_Def);
	 pragma Assert
	   (Nkind_In (Ancestror_Type, N_Expanded_Name, N_Identifier));
	 
	 --  Nothing to do as the derived type is replaced by its base type,
	 --  and the base type is declared when encountering iths declaration
	 
	 null;
      end if;
   end Generate_Derived_Type_Definition;
   
   --------------------
   -- Generate_Range --
   --------------------
   
   procedure Generate_Range_Constraint 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Rng : Node_Id;
   begin
      Rng := Range_Expression (Node);
      Generate_Node (This, Rng);
   end Generate_Range_Constraint;
   
   --------------------
   -- Generate_Range --
   --------------------
   
   procedure Generate_Range 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob       : Output_Buffer := This.Get_Output_Buffer;
      Low      : Node_Id;
      High     : Node_Id;
      Low_Val  : Uint;
      High_Val : Uint;
   begin
      Low := Low_Bound (Node);
      if Compile_Time_Known_Value (Low) then
         if Is_Real_Type (Etype (Low)) then
	    Generate_Real_Literal (This, Low);
	 else
            Low_Val := Expr_Value (Low);
	    Write_Uint (Ob, Low_Val);
	 end if;
      else
	 Generate_Node (This, Low_Bound (Node));
      end if;
      
      Write_Str (Ob, "..");
			
      High := High_Bound (Node);
      if Compile_Time_Known_Value (High) then
         if Is_Real_Type (Etype (High)) then
	    Generate_Real_Literal (This, High);
	 else
            High_Val := Expr_Value (High);
	    Write_Uint (Ob, High_Val);
	 end if;
      else
	 Generate_Node (This, High_Bound (Node));
      end if;
   end Generate_Range;
   
   -----------------------------------------------
   -- Generate_Index_Or_Discriminant_Constraint --
   -----------------------------------------------
   
   procedure Generate_Index_Or_Discriminant_Constraint 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Index_Or_Discriminant_Constraint;

   ---------------------------------------
   -- Generate_Discriminant_Association --
   ---------------------------------------
   
   procedure Generate_Discriminant_Association 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Discriminant_Association;
   
   -----------------------------------------
   -- Generate_Discriminant_Specification --
   -----------------------------------------
   
   procedure Generate_Discriminant_Specification 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Discriminant_Specification;
   
   ------------------------------------------
   -- Generate_Enumeration_Type_Definition --
   ------------------------------------------
   
   procedure Generate_Enumeration_Type_Definition  
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Enumeration_Type_Definition;
   
   ----------------------------------
   -- Generate_Extension_Aggregate --
   ----------------------------------
   
   procedure Generate_Extension_Aggregate 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Extension_Aggregate;
   
   -------------------------------------
   -- Generate_Function_Instantiation --
   -------------------------------------
   
   procedure Generate_Function_Instantiation 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Function_Instantiation;
   
   -------------------------------------
   -- Generate_Function_Specification --
   -------------------------------------
   
   procedure Generate_Function_Specification 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Function_Specification;
   
   -----------------------------------------
   -- Generate_Implicit_Label_Declaration --
   -----------------------------------------
   
   procedure Generate_Implicit_Label_Declaration 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Implicit_Label_Declaration;
   
   ------------------------------------------
   -- Generate_Incomplete_Type_Declaration --
   ------------------------------------------
   
   procedure Generate_Incomplete_Type_Declaration 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      --  Set Output to Types buffer
      
      -- Set_Output_Buffer (This, Get_Types_Output_Buffer (This));
      
      null;
   end Generate_Incomplete_Type_Declaration;
   
   --------------------------------------
   -- Generate_Modular_Type_Definition --
   --------------------------------------
   
   procedure Generate_Modular_Type_Definition 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      --  Nothing to do as the modular types are generated as unsigned_integer. 
      --  The references to the types are replaced by unsigned_x where x is 
      --  the number of bits needed to holds a value of the type
      null;
   end Generate_Modular_Type_Definition;
   
   ----------------------------------------
   -- Generate_Floating_Point_Definition --
   ----------------------------------------
   
   procedure Generate_Floating_Point_Definition 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      --  Nothing to do as the floating point are  generated as float. 
      --  The references to the types are replaced by float. 
      null;
   end Generate_Floating_Point_Definition;
   
   ---------------------------
   -- Generate_Subtype_Mark --
   ---------------------------
   
   procedure Generate_Subtype_Mark
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      -------------------------
      -- 3.2.2  Subtype Mark --
      -------------------------

      --  SUBTYPE_MARK ::= subtype_NAME
      
      pragma Assert (Nkind_In (Node, N_Expanded_Name, N_Identifier));
      
      Generate_Type_Name (This, Entity (Node));
   end Generate_Subtype_Mark;
   
   ---------------------------------
   -- Generate_Number_Declaration --
   ---------------------------------
   
   procedure Generate_Number_Declaration 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob     : Output_Buffer;
      Def_Id : Entity_Id;
      Expr   : Node_Id;
      Typ    : Node_Id;
      Com    : Str_Id;
   begin
      --  Set Output to Types buffer
      
      Set_Output_Buffer (This, Get_Current_Vars_Buffer (This));
      Ob := This.Get_Output_Buffer;
      
      Def_Id := Defining_Identifier (Node);
      
      Write_Indent_Str (Ob, "<variables ");
      Write_Str (Ob, "name=""");
      Write_Id (Ob,  Def_Id);
      Write_Str (Ob,""" typeName=""");
      Typ := Get_Type_Full_View (Etype (Def_Id));
      
      Expr := Expression (Node);
      if Present (Expr) then
         
         if Is_Real_Type (Etype (Expr)) then
            Write_Str (ob, "Real");
         else
            Generate_Type_Name (This, Typ);
         end if;

	 Write_Str (Ob, """>");
	 Write_Eol (Ob);
	 Indent_Begin (Ob);
	 
	 Write_Indent_Str (Ob, "<variableInit value=""");
	 
	 if Compile_Time_Known_Value (Expr) then
	    if Is_Boolean_Type (Etype (Expr)) then
	       if Expr_Value (Expr) = Uint_1 then
		  Write_Str (Ob, "True");
	       else
		  Write_Str (Ob, "False");
	       end if;
	       
	    elsif Is_Real_Type (Etype (Expr)) then
	       Generate_Real_Literal (This, Expr);
	    else
	       Write_Uint (Ob, Expr_Value (Expr));
	    end if;
	 else
	    Generate_Node (This, Expr);
	 end if;
	 
	 Write_Str (Ob, """></variableInit>");
	 Indent_End (Ob);
	 Write_Eol (Ob);
      else
	 Write_Str (Ob, ">");
	 Write_Eol (Ob);
      end if;
      
      -- Gen_Var_Comment (Def_Id);
      
      Com := Get_Entity_Comment (Def_Id);
      if Com /= No_Str_Id then
	 Write_Indent_Str
           (Ob, "<comment>" & 
              Normalize_Comment (Get_String (Com)) & "</comment>");
      end if;
      
      Write_Indent_Str (Ob, "</variables>");
      Write_Eol (Ob);
   end Generate_Number_Declaration;
   
   ---------------------------------
   -- Generate_Object_Declaration --
   ---------------------------------
   
   procedure Generate_Object_Declaration 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Previous_Ob : Output_Buffer;
      Ob          : Output_Buffer;
      Def_Id      : Entity_Id; --  := Defining_Identifier (Node);
      Type_Def    : Node_Id;
      Expr        : Node_Id;
      Aggr        : Node_Id;
      Init        : Node_Id;
--        Mark     : Node_Id;
--        Constr   : Node_Id;
   begin
      Put_Line ("Generate_Object_Declaration Begin");
      Def_Id := Defining_Identifier (Node);
      
      Put_Line
	("Generate_Object_Declaration Begin " & Get_String (Chars (Def_Id)));
      
      Previous_Ob := This.Get_Output_Buffer;
      This.Set_Output_Buffer (This.Get_Current_Vars_Buffer);
      Ob := This.Get_Output_Buffer;
      
      -------------------------------
      -- 3.3.1  Object Declaration --
      -------------------------------

      --  OBJECT_DECLARATION ::=
      --    DEFINING_IDENTIFIER_LIST : [aliased] [constant]
      --      [NULL_EXCLUSION] SUBTYPE_INDICATION [:= EXPRESSION]
      --        [ASPECT_SPECIFICATIONS];
      --  | DEFINING_IDENTIFIER_LIST : [aliased] [constant]
      --      ACCESS_DEFINITION [:= EXPRESSION]
      --        [ASPECT_SPECIFICATIONS];
      --  | DEFINING_IDENTIFIER_LIST : [aliased] [constant]
      --      ARRAY_TYPE_DEFINITION [:= EXPRESSION]
      --        [ASPECT_SPECIFICATIONS];
      --  | SINGLE_TASK_DECLARATION
      --  | SINGLE_PROTECTED_DECLARATION
      
      --  Here we skip access to others thans those generated by reflex.
      
      Type_Def := Object_Definition (Node);
      
      if NKind (Type_Def) /= N_Expanded_Name
	and then Nkind (Type_Def) /= N_Identifier
	and then Nkind (Type_Def) /= N_Subtype_Indication
	and then Nkind (Type_Def) /= N_Access_Definition
	and then Nkind (Type_Def) /= N_Constrained_Array_Definition
      then
	 Error_Msg_N
	   ("unsupported type for object declaration " & 
	      Nkind (Type_Def)'Img, Node);
	 
	 This.Set_Output_Buffer (Previous_Ob);
	 return;
      end if;
      
      if Nkind (Type_Def) = N_Access_Definition then
	 if Present (Access_To_Subprogram_Definition (Type_Def)) then
	    if Comes_From_Source (Node) then
	       Error_Msg_N
		 ("reflex does not supported access to subprogram ", Node);
	       
	       This.Set_Output_Buffer (Previous_Ob);
	       return;
	       
	       --  The expander has generated an access to subprogram. No need
	       --  to generate it as Unity handles them.
	    else
	       This.Set_Output_Buffer (Previous_Ob);
	       return;
	    end if;
	 end if;
      end if;
      
      Write_Indent_Str (Ob, "<variables ");
      Write_Str (Ob, "name=""");
      Write_Id (Ob,  Def_Id);
      Write_Str (Ob,""" typeName=""");
      
      if NKind_In (Type_Def, N_Expanded_Name, N_Identifier) then
	 
	 --  If the base type of Type_def is tagged anonym and the base type
	 --  is either an array or an access. Replace the identifier by the
	 --  Type Definition of te base type.
	 
	 Handle_Anonymous_Type (This, Entity (Type_Def));
	 
	 -- Add Address
	 
	 declare
	    Addr : Str_Id;
	 begin
	    Addr := Get_Entity_Address (Def_Id);
	    if Addr /= No_Str_Id then
	       Write_Str (Ob, """ ");
	       
	       Write_Str
		 (Ob, "topologicalAddress=""" & Get_String (Addr) & """>");
	    else
	       Write_Str (Ob, """>");
	    end if;
	 end;
	 
	 -- Add Comment
	 
	 declare
	    Com : Str_Id;
	 begin
	    Com := Get_Entity_Comment (Def_Id);
	    if Com /= No_Str_Id then
	       Write_Eol (Ob);
	       Write_Indent_Str (Ob, "<comment>");
	       Write_Str (Ob, Normalize_Comment (Get_String (Com)));
	       Write_Indent_Str (Ob, "</comment>");
	       Write_Eol (Ob);
	    end if;
	 end;
	 
	 --  Initialize Object if initialization presebt
	 
	 Expr := Expression (Node);
	 if Present (Expr) and then Nkind (Expr) /= N_Null then
	    Aggr := Designate_Aggregate (Expr);
	    if Present (Aggr) then
	       
	       if Is_Record_Type (Etype (Type_Def)) then
		  Write_Eol (Ob);
		  Indent_Begin (Ob);
		  Generate_Record_Aggregate_Values (This, Aggr);	       
		  Indent_End (Ob);
		  Write_Eol (Ob);
		  Write_Indent_Str (Ob, "</variables>");
		  Write_Eol (Ob);
	       else
		  pragma Assert (Is_Array_Type (Etype (Type_Def)));
		  null;
		  Write_Indent_Str (Ob, "</variables>");
		  Write_Eol (Ob);
	       end if;
	       
	    else
	       Write_Eol (Ob);
	       Indent_Begin (Ob);
	       Write_Indent_Str (Ob, "<variableInit value=""");
	       if Compile_Time_Known_Value (Expr) then
		  declare
		     Val : Integer;
		  begin
		     Val := Integer (Ui_To_Int (Expr_Value (Expr)));
		     Write_Int (Ob , Val);
		  end;
	       else
		  Generate_Node (This, Expr);
	       end if;
	       Write_Str (Ob, """>");
	       Write_Str (Ob, "</variableInit>");
	       
	       Indent_End (Ob);
	       Write_Eol (Ob);
	       Write_Indent_Str (Ob, "</variables>");
	       Write_Eol (Ob);
	    end if;
	    
	 elsif Is_Record_Type (Etype (Type_Def)) then
	    Indent_Begin (Ob);
	    Init := Get_Init_Record (Base_Type (Etype (Type_Def)));
	    
	    if Present (Init) then
	       Aggr := Expression (Init);
	       
	       if Present (Aggr) then
		  Write_Eol (Ob);
		  Indent_Begin (Ob);
		  Generate_Record_Aggregate_Values (This, Aggr);	       
		  Indent_End (Ob);
		  Write_Eol (Ob);
	       end if;	       
	    end if;
	    Write_Indent_Str (Ob, "</variables>");
	    Write_Eol (Ob);
	    
	 else
	    Write_Str (Ob, "</variables>");
	    Write_Eol (Ob);
	 end if;
	 
      elsif Nkind (Type_Def) = N_Subtype_Indication then
	 
	 --  Mark := Subtype_Mark (Type_Def);
	 --  Generate_Subtype_Mark (This, Mark);
	 
	 --  Constr := Constraint (Type_Def);
	 --  if Present (Constr) then
	 --     Generate_Node (This, Constr);
	 --  end if;
      
	 Generate_Subtype_Indication (This, Type_Def);
	 Write_Str (Ob, """>");
	 Write_Str (Ob, "</variables>");
	 Write_Eol (Ob);
	 
      elsif Nkind (Type_Def) = N_Constrained_Array_Definition then
	 Generate_Constrained_Array_Definition (This, Type_Def, True);
	 Write_Str (Ob, """>");
	 Write_Str (Ob, "</variables>");
	 Write_Eol (Ob);
	 
      elsif NKind (Type_Def) = N_Access_Definition then
	 Generate_Access_Definition (This, Type_Def);
	 Write_Str (Ob, """>");
	 Write_Str (Ob, "</variables>");
	 Write_Eol (Ob);
	 
	 --  Never go here
      else
	 raise Program_Error;
      end if;
      
      This.Set_Output_Buffer (Previous_Ob);
      
      Put_Line ("Generate_Object_Declaration ENd");
   exception
      when others =>
	 This.Set_Output_Buffer (Previous_Ob);
	 Put_Line ("Exception ===============> Generate_Object_Declaration");
   end Generate_Object_Declaration;
   
   -----------------------------------------
   -- Initialze_Record_Object_Declaration --
   -----------------------------------------
   
   procedure Initialze_Record_Object_Declaration
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob       : Output_Buffer := This.Get_Output_Buffer;
      Type_Def : Node_Id;
      E        : Entity_Id;
      Expr     : Node_Id;
      Comp_Id  : Entity_Id;
      Comp_Dec : Node_Id;
   begin
      Type_Def := Object_Definition (Node);
      
      pragma Assert (NKind_In (Type_Def, N_Expanded_Name, N_Identifier));
      pragma Assert (Is_Record_Type (Etype (Type_Def)));
      Expr := Expression (Node);
      
      if Present (Expr) and then Nkind (Expr) /= N_Null then
	 if Nkind (Expr) = N_Aggregate
	   or else Nkind (Expr) = N_Extension_Aggregate 
	 then
	    Generate_Record_Aggregate_In_Declaration (This, Expr);
	    
	 else
	    Error_Msg_N
	      ("initialization expression for record must be an aggregate", 
	       Node);
	 end if;
	 
	 --  Initialize the component that have default initialization (set
	 --  at the definition of the type).
	 
      else
	 E := Entity (Type_Def);
	 Comp_Id := First_Component (E);
	 while Present (Comp_Id) loop
	    Comp_Dec := Parent (Comp_Id);
	    Expr     := Expression (Comp_Dec);
	    
	    if Present (Expr) then
	       if Nkind (Expr) /= N_Attribute_Reference then
		  Write_Indent_Str 
		 (Ob, "<instanceElementDesc name=""");
		  Write_Id (Ob, Comp_Id);
		  Write_Str (Ob, """>");
		  Write_Eol (Ob);
		  
		  Indent_Begin (Ob);
		  Write_Indent_Str (Ob, "<value>REF(");
		  Generate_Node (This, Expr);
		  Write_Str (Ob, ")</value>");
		  Write_Eol (Ob);
		  Indent_End (Ob);
		  
		  Write_Indent_Str (Ob, "</instanceElementDesc>");
		  Write_Eol (Ob);
		  
	       elsif Nkind (Expr) = N_Attribute_Reference then
		  
		  declare
		     Aname   : constant Name_Id := Attribute_Name (Expr);
		     Attr_Id : constant Attribute_Id := 
		       Get_Attribute_Id (Aname);
		     P     : constant Node_Id := Prefix (Expr);
		  begin
		     if Attr_Id = Attribute_Access
		       or else Attr_Id = Attribute_Unchecked_Access
		       or else Attr_Id = Attribute_Unrestricted_Access
		     then
			Write_Indent_Str 
			  (Ob, "<instanceElementDesc name=""");
			Write_Id (Ob, Comp_Id);
			Write_Str (Ob, """>");
			Write_Eol (Ob);
			
			Indent_Begin (Ob);
			Write_Indent_Str (Ob, "<value>REF(");
			Write_Id (Ob, P);
			Write_Str (Ob, ")</value>");
			Write_Eol (Ob);
			Indent_End (Ob);
			
			Write_Indent_Str (Ob, "</instanceElementDesc>");
			Write_Eol (Ob);
			
		     end if;
		  end;
	       end if;
	    end if;
	    
	    Next_Component (Comp_Id);
	 end loop;
      end if;
      
   end Initialze_Record_Object_Declaration;
   
   ------------------------------------------
   -- Generate_Object_Renaming_Declaration --
   ------------------------------------------
   
   procedure Generate_Object_Renaming_Declaration 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob         : Output_Buffer := This.Get_Output_Buffer;
      Def_Id     : Entity_Id := Defining_Identifier (Node);
      Type_Def   : Node_Id;
      Access_Def : Node_Id;
      Subtyp     : Node_Id;
      Expr       : Node_Id;
--        Mark       : Node_Id;
--        Constr     : Node_Id;
   begin
      -------------------------------
      -- 3.3.1  Object Declaration --
      -------------------------------

      --  OBJECT_DECLARATION ::=
      --    DEFINING_IDENTIFIER_LIST : [aliased] [constant]
      --      [NULL_EXCLUSION] SUBTYPE_INDICATION [:= EXPRESSION]
      --        [ASPECT_SPECIFICATIONS];
      --  | DEFINING_IDENTIFIER_LIST : [aliased] [constant]
      --      ACCESS_DEFINITION [:= EXPRESSION]
      --        [ASPECT_SPECIFICATIONS];
      --  | DEFINING_IDENTIFIER_LIST : [aliased] [constant]
      --      ARRAY_TYPE_DEFINITION [:= EXPRESSION]
      --        [ASPECT_SPECIFICATIONS];
      --  | SINGLE_TASK_DECLARATION
      --  | SINGLE_PROTECTED_DECLARATION
      
      Write_Indent (Ob);
      Write_Id (Ob, Def_Id);
      Write_Str (Ob, " : ");
      
      if Constant_Present (Node) then
	 Write_Str (Ob, "constant ");
      end if;
      
      Type_Def := Object_Definition (Node);
      
      if NKind_In (Type_Def, N_Expanded_Name, N_Identifier) then
	 Generate_Subtype_Mark (This, Type_Def);
	 
      elsif Nkind (Type_Def) = N_Subtype_Indication then
	 
	 --  Mark := Subtype_Mark (Type_Def);
	 --  Generate_Subtype_Mark (This, Mark);
	 
	 --  Constr := Constraint (Type_Def);
	 --  if Present (Constr) then
	 --     Generate_Node (This, Constr);
	 --  end if;
	 
	 Generate_Subtype_Indication (This, Type_Def);
	 
      else
	 Access_Def := Access_Definition (Node);
	 
	 if Present (Access_Def) then
	    pragma Assert (Nkind (Access_Def) = N_Access_Definition);
	    pragma Assert (No (Access_To_Subprogram_Definition (Access_Def)));
	    
	    Subtyp := Subtype_Mark (Access_Def);
	    pragma Assert (Present (Subtyp));
	    pragma Assert (Nkind_In (Subtyp, N_Expanded_Name, N_Identifier));
	    
	    Write_Str (Ob, " access ");
	    Generate_Subtype_Mark (This, Subtyp);
	    
	 else
	    Error_Msg_N
	      ("reflex does not supported this definition type ", Node);
	    return;
	 end if;
      end if;
      
      --  Here if we have an expression it must be static
      
      Expr := Expression (Node);
      if Present (Expr) then
	 Write_Str (Ob, " := ");
	 if Compile_Time_Known_Value (Expr) then
	   Generate_Node (This, Expr);  --     Write_Uint (Expr_Value (Expr));
	 else
	    Generate_Node (This, Expr);
	 end if;
      end if;
      
      Write_Str (Ob, ";");
      Write_Eol (Ob);
      Write_Eol (Ob);
   end Generate_Object_Renaming_Declaration;
   
   ----------------------------------------------
   -- Generate_Ordinary_Fixed_Point_Definition --
   ----------------------------------------------
   
   procedure Generate_Ordinary_Fixed_Point_Definition 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Ordinary_Fixed_Point_Definition;
   
   ------------------------------------
   -- Generate_Package_Instantiation --
   ------------------------------------
   
   procedure Generate_Package_Instantiation 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Package_Instantiation;
   
   -------------------------------------------
   -- Generate_Package_Renaming_Declaration --
   -------------------------------------------
   
   procedure Generate_Package_Renaming_Declaration 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Package_Renaming_Declaration;
   
   --------------------------------------------
   -- Generate_Private_Extension_Declaration --
   --------------------------------------------
   
   procedure Generate_Private_Extension_Declaration 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer;
   begin
      --  Set Output to Types buffer
      
      --  Set_Output_Buffer (This, Get_Types_Output_Buffer (This));
      --  Ob := This.Get_Output_Buffer;
      
      null;
   end Generate_Private_Extension_Declaration;
   
   ---------------------------------------
   -- Generate_Private_Type_Declaration --
   ---------------------------------------
   
   procedure Generate_Private_Type_Declaration 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer;
   begin
      --  Set Output to Types buffer
      
      --  Set_Output_Buffer (This, Get_Types_Output_Buffer (This));
      --  Ob := This.Get_Output_Buffer;
      
      null;
   end Generate_Private_Type_Declaration;
   
   --------------------------------------
   -- Generate_Procedure_Instantiation --
   --------------------------------------
   
   procedure Generate_Procedure_Instantiation 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Procedure_Instantiation;
   
   --------------------------------------
   -- Generate_Procedure_Specification --
   --------------------------------------
   
   procedure Generate_Procedure_Specification 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Procedure_Specification;
   
   ---------------------------------------
   -- Generate_Real_Range_Specification --
   ---------------------------------------
   
   procedure Generate_Real_Range_Specification 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Real_Range_Specification;
   
   ----------------------------------------------
   -- Generate_Unconstrained_Array_Declaration --
   ----------------------------------------------
   
   procedure Generate_Unconstrained_Array_Declaration
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer;
   begin
      --  Set Output to Types buffer
      
      --  Set_Output_Buffer (This, Get_Types_Output_Buffer (This));
      --  Ob := This.Get_Output_Buffer;
      
      --  Unconstrained Array Type are not emited
      null;
   end Generate_Unconstrained_Array_Declaration;
   
   --------------------------------------------
   -- Generate_Constrained_Array_Declaration --
   --------------------------------------------
   
   procedure Generate_Constrained_Array_Declaration
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob         : Output_Buffer;
      Def_Id     : Entity_Id     := Defining_Identifier (Node);
      Type_Def   : Node_Id       := Type_Definition (Node);
   begin
      --  Set Output to Types buffer
      
      Set_Output_Buffer (This, Get_Types_Output_Buffer (This));
      Ob := This.Get_Output_Buffer;
      
      ---------------------------------------
      -- 3.6  Constrained Array Definition --
      ---------------------------------------

      --  CONSTRAINED_ARRAY_DEFINITION ::=
      --    array (DISCRETE_SUBTYPE_DEFINITION
      --      {, DISCRETE_SUBTYPE_DEFINITION})
      --        of COMPONENT_DEFINITION
      
      
      
      Write_Indent_Str (Ob, "<DDTSource DDTName=""");
      Write_Id (Ob, Def_Id);
      Write_Str
	(Ob, """ version=""0.00"" dateTime=""dt#2006-07-25-16:03:09"">");
      
      Indent_Begin (Ob);
      
      Generate_Constrained_Array_Definition (This, Type_Def);
      
      Indent_End (Ob);
      
      Write_Indent_Str (Ob, "</DDTSource>");
      Write_Eol (Ob);
   end Generate_Constrained_Array_Declaration;
   
   ---------------------------------------------
   -- Generate_Unconstrained_Array_Definition --
   ---------------------------------------------
   
   procedure Generate_Unconstrained_Array_Definition
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob    : Output_Buffer := This.Get_Output_Buffer;
   begin
      pragma Assert (Nkind (Node) = N_Unconstrained_Array_Definition);
      
      null;
   end Generate_Unconstrained_Array_Definition;
   
   -------------------------------------------
   -- Generate_Constrained_Array_Definition --
   -------------------------------------------
   
   procedure Generate_Constrained_Array_Definition
     (This   : access Unity_Generator_Record;
      Node   : Node_Id;
      Anomym : Boolean := False) is
      
      Ob      : Output_Buffer := This.Get_Output_Buffer;
      Indexes : List_Id;
      Dim     : Node_Id;
      Low     : Node_Id;
      High    : Node_Id;
   begin
      pragma Assert (Nkind (Node) = N_Constrained_Array_Definition);
	
      ---------------------------------------
      -- 3.6  Constrained Array Definition --
      ---------------------------------------

      --  CONSTRAINED_ARRAY_DEFINITION ::=
      --    array (DISCRETE_SUBTYPE_DEFINITION
      --      {, DISCRETE_SUBTYPE_DEFINITION})
      --        of COMPONENT_DEFINITION
      
      if not Anomym then
	 Write_Eol (Ob);
	 Write_Indent_Str (Ob, "<array>");
      end if;
      Write_Str (Ob, "ARRAY[");
      Indexes := Discrete_Subtype_Definitions (Node);
      Dim := First (Indexes);
      loop
	 if Nkind (Dim) = N_Range then
	    Low  := Low_Bound (Dim);
	    High := High_Bound (Dim);
	    Generate_Node (This, Low);
	    Write_Str (Ob, "..");
	    Generate_Node (This, High);
	 else
	    null;
	 end if;
	 
	 Next (Dim);
	 exit when No (Dim);
	 Write_Str (Ob, ",");
      end loop;
      
      Write_Str (Ob, "] of ");
      
      Generate_Component_Definition (This, Component_Definition (Node), True);
      
      if not Anomym then
	 Write_Str (Ob, "</array>");
	 Write_Eol (Ob);
      end if;
   end Generate_Constrained_Array_Definition;
   
   --------------------------------
   -- Generate_Acces_Definition --
   --------------------------------
   
   procedure Generate_Access_Definition
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob     : Output_Buffer := This.Get_Output_Buffer;
      Subtyp : Node_Id;
      Mark   : Node_Id;
   begin
      pragma Assert (Nkind (Node) /= N_Access_Function_Definition);
      pragma Assert (Nkind (Node) /= N_Access_Procedure_Definition);
	 
      if Nkind (Node) = N_Access_To_Object_Definition then
	 
	 Subtyp := Subtype_Indication (Node);
	 
	 pragma Assert (Nkind_In (Subtyp, N_Expanded_Name, N_Identifier));
	 
	 Write_Str (Ob, "REF_TO ");
	 --  Generate_Type_Name (This, Base_Type (Entity (Subtyp)));
	 Generate_Type_Name (This, Etype (Subtyp));
	 
      elsif Nkind (Node) = N_Access_Definition then
	 
	 pragma Assert (No (Access_To_Subprogram_Definition (Node)));
	    
	 Mark := Subtype_Mark (Node);	 
	 if Present (Mark) then
	    pragma Assert (Nkind_In (Mark, N_Expanded_Name, N_Identifier));
	    
	    Write_Str (Ob, "REF_TO ");
	    Generate_Type_Name (This, Etype (Mark));
	 else
	    raise Program_Error;
	 end if;
	 
      else
	 raise Program_Error;
      end if;
   exception
      when others =>
	 Put_Line ("Generate_Access_Definition exception");
   end Generate_Access_Definition;
   
   ------------------------------------------
   -- Generate_Access_To_Object_Definition --
   ------------------------------------------
   
   procedure Generate_Access_To_Object_Definition
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob       : Output_Buffer := This.Get_Output_Buffer;
      Def_Id   : Node_id := Defining_Identifier (Node);
      Type_Def : Node_Id;
   begin
      ---------------------------------------
      -- 3.10  Access To Object Definition --
      ---------------------------------------

      --  ACCESS_TO_OBJECT_DEFINITION ::=
      --    [NULL_EXCLUSION] access [GENERAL_ACCESS_MODIFIER]
      --    SUBTYPE_INDICATION
      
      Write_Indent_Str (Ob, "<DDTSource DDTName=""");
      Write_Id (Ob, Def_Id);
      Write_Str
	(Ob, """ version=""0.02"" dateTime=""dt#2017-01-23-18:31:26"">");
      Write_Eol (Ob);
      
      Indent_Begin (Ob);
      --  Write_Indent_Str
      --  	(Ob, 
      --  	 "<attribute name=""TypeRWReferencedTypeRight""" & 
      --  	   " value=""2#0000_0001""></attribute>");
      --  Write_Eol (Ob);

      Write_Indent_Str (Ob, "<reference>");
      Type_Def := Type_Definition (Node);
      Generate_Access_Definition (This, Type_Def);
      Write_Str (Ob, "</reference>");
      Write_Eol (Ob);
      
      Indent_End (Ob);
      Write_Indent_Str (Ob, "</DDTSource>");
      Write_Eol (Ob);
   exception
      when others =>
	 Put_Line ("Generate_Access_To_Object_Definition exception");
   end Generate_Access_To_Object_Definition;
   
   ---------------------------------
   -- Generate_Record_Declaration --
   ---------------------------------
   
   procedure Generate_Record_Declaration
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob     : Output_Buffer;
      Def_Id : Entity_Id := Defining_Identifier (Node);
   begin
      --  Set Output to Types buffer
      
      Set_Output_Buffer (This, Get_Types_Output_Buffer (This));
      Ob := This.Get_Output_Buffer;
      
      --  RECORD_DEFINITION ::=
      --    record
      --      COMPONENT_LIST
      --    end record
      --  | null record
      
      Write_Indent_Str (Ob, "<DDTSource DDTName=""");
      Write_Id (Ob, Def_Id);
      Write_Str
	(Ob, """ version=""0.02"" dateTime=""dt#2017-01-23-18:31:26"">");
      Write_Eol (Ob);
      
      Indent_Begin (Ob);
      Write_Indent_Str (Ob, "<structure>");
      Write_Eol (Ob);
      
      Indent_Begin (Ob);
      Generate_Component_List (This, Type_Definition (Node));
      
      Indent_End (Ob);
      Write_Indent_Str (Ob, "</structure>");
      Write_Eol (Ob);
      
      Indent_End (Ob);
      Write_Indent_Str (Ob, "</DDTSource>");
      Write_Eol (Ob);
   end Generate_Record_Declaration;
   
   --------------------------------------------
   -- Generate_Enumeration_Literal_Constants --
   --------------------------------------------
   
   procedure Generate_Enumeration_Literal_Constants
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob     : Output_Buffer := This.Get_Output_Buffer;
      Def_Id : Node_Id;
      Lit    : Node_Id;
   begin
      Def_Id := Defining_Identifier (Node);
      
      --  For each litraral emit an integer constant wihch value is the 
      --  Representation value of the literal
      
      Write_Eol (Ob);
      Lit := First_Literal (Def_Id);
      while Present (Lit) loop
	 Write_Indent (Ob);
	 Write_Id (Ob, Lit);
	 Write_Str (Ob, " : constant Integer := ");
	 Write_Uint (Ob, Enumeration_Rep (Lit));
	 Write_Str (Ob, ";");
	 Write_Eol (Ob);
	 
	 Lit := Next_Literal (Lit);
      end loop;
      
      Write_Eol (Ob);
   end Generate_Enumeration_Literal_Constants;
   
   ---------------------------------------------
   -- Generate_Signed_Integer_Type_Definition --
   ---------------------------------------------
   
   procedure Generate_Signed_Integer_Type_Definition 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      --  Nothing to do as the signed integer types are generated as integer. 
      --  The references to the types are replaced by integer_x where x is 
      --  the number of bits needed to holds a value of the type
      null;
   end Generate_Signed_Integer_Type_Definition;
   
   ----------------------------------------------
   -- Generate_Subprogram_Renaming_Declaration --
   ----------------------------------------------
   
   procedure Generate_Subprogram_Renaming_Declaration 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Subprogram_Renaming_Declaration;
   
   ----------------------------------
   -- Generate_Subtype_Declaration --
   ----------------------------------
   
   procedure Generate_Subtype_Declaration 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob          : Output_Buffer;
      Subtyp      : Node_id;
      Mark        : Node_Id;
      Typ         : Entity_Id;
      Previous_Ob : Output_Buffer;
   begin
      --  Set Output to Types buffer
      
      Previous_Ob := This.Get_Output_Buffer;
      This.Set_Output_Buffer (This.Get_Current_Types_Buffer);
      
      Ob := This.Get_Output_Buffer;
      
      --------------------------------
      -- 3.2.2  Subtype Declaration --
      --------------------------------

      --  SUBTYPE_DECLARATION ::=
      --    subtype DEFINING_IDENTIFIER is [NULL_EXCLUSION] SUBTYPE_INDICATION
      --      [ASPECT_SPECIFICATIONS];

      --  The subtype indication field is set to Empty for subtypes
      --  declared in package Standard (Positive, Natural).

      --  N_Subtype_Declaration
      --  Sloc points to SUBTYPE
      --  Defining_Identifier (Node1)
      --  Null_Exclusion_Present (Flag11)
      --  Subtype_Indication (Node5)
      --  Generic_Parent_Type (Node4-Sem) (set for an actual derived type).
      --  Exception_Junk (Flag8-Sem)
      --  Has_Dynamic_Range_Check (Flag12-Sem)
      
      Subtyp := Subtype_Indication (Node);
      if Nkind (Subtyp) = N_Subtype_Indication then
	 
	 Mark := Subtype_Mark (Subtyp);
	 Typ  := Etype (Entity (Mark));
	 
	 --  If the subtype is constraining an array type, repalce the subtype
	 --  indication by an anonymous array
	 
	 if Ekind (Typ) = E_String_Literal_Subtype then
	    Write_Indent_Str (Ob, "type ");
	    Write_Id (Ob, Defining_Identifier (Node));
	    Write_Str (Ob, " is ");
		      
	    Subtype_Indication_As_String (This, Typ);
	    
	    Write_Char (Ob, ';');
	    Write_Eol (Ob);
	    
	    --  If the subtype is constraining an array type, repalce the
	    --  subtype indication by an anonymous array
	    
	 elsif Is_Array_Type (Typ) then
	    Write_Indent_Str (Ob, "type ");
	    Write_Id (Ob, Defining_Identifier (Node));
	    Write_Str (Ob, " is ");
	    
	    Subtype_Indication_As_Anonymous_Array (This, Subtyp);
	    
	    Write_Char (Ob, ';');
	    Write_Eol (Ob);
	 end if;
	 
	 --  Nothing to do, as the generator replaces the subtype by its
	 --  base type
      else
	 null;
      end if;
      
      This.Set_Output_Buffer (Previous_Ob);
   end Generate_Subtype_Declaration;
   
   ---------------------------------
   -- Generate_Subtype_Indication --
   ---------------------------------
   
   procedure Generate_Subtype_Indication 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
      
      Ob   : Output_Buffer := This.Get_Output_Buffer;
      Mark : Node_Id;
      Typ  : Entity_Id;
   begin
      -------------------------------
      -- 3.2.2  Subtype Indication --
      -------------------------------

      --  SUBTYPE_INDICATION ::= SUBTYPE_MARK [CONSTRAINT]

      --  Note: if no constraint is present, the subtype indication appears
      --  directly in the tree as a subtype mark. The N_Subtype_Indication
      --  node is used only if a constraint is present.

      --  Note: [For Ada 2005 (AI-231)]: Because Ada 2005 extends this rule
      --  with the null-exclusion part (see AI-231), we had to introduce a new
      --  attribute in all the parents of subtype_indication nodes to indicate
      --  if the null-exclusion is present.

      --  Note: the reason that this node has expression fields is that a
      --  subtype indication can appear as an operand of a membership test.

      --  N_Subtype_Indication
      --  Sloc points to first token of subtype mark
      --  Subtype_Mark (Node4)
      --  Constraint (Node3)
      --  Etype (Node5-Sem)
      --  Must_Not_Freeze (Flag8-Sem)

      --  Note: Depending on context, the Etype is either the entity of the
      --  Subtype_Mark field, or it is an itype constructed to reify the
      --  subtype indication. In particular, such itypes are created for a
      --  subtype indication that appears in an array type declaration. This
      --  simplifies constraint checking in indexed components.

      --  For subtype indications that appear in scalar type and subtype
      --  declarations, the Etype is the entity of the subtype mark.
      
      
      --  Special case when the subtype mark is an array. The array must be
      --  unconstrainted and the constraint provided in subtype indication is
      --  a range or an attribute range. We replace the subtype indication with
      --  an anonymous array of range bounded by the constraint provided in 
      --  subtype indication.
      
      Mark := Subtype_Mark (Node);
      Typ  := Etype (Mark);
      
      if Ekind (Typ) = E_String_Literal_Subtype then
	 Subtype_Indication_As_String (This, Typ);
	 
      elsif Is_Array_Type (Typ) then
	 Subtype_Indication_As_Anonymous_Array (This, Node);
	 
      else
	 Generate_Subtype_Mark (This, Mark);
      end if;
      
      --  Constr := Constraint (Node);
      --  if Present (Constr) then
      --  	 Write_Char (Ob, '(');
      --  	 Generate_Node (This, Constr);
      --  	 Write_Char (Ob, ')');
      --  end if;
      
   end Generate_Subtype_Indication;
   
   ----------------------
   -- Generate_Variant --
   ----------------------
   
   procedure Generate_Variant 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Variant;
   
   ---------------------------
   -- Generate_Variant_Part --
   ---------------------------
   
   procedure Generate_Variant_Part 
     (This : access Unity_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Variant_Part;
   
   -------------------------------------------
   -- Subtype_Indication_As_Anonymous_Array --
   -------------------------------------------
   
   procedure Subtype_Indication_As_Anonymous_Array 
     (This   : access Unity_Generator_Record;
      Subtyp : Node_Id) is
      
      Ob          : Output_Buffer := This.Get_Output_Buffer;
      Mark        : Node_Id;
      Constr      : Node_Id;
      Typ         : Entity_Id;
      Constr_List : List_Id;
      Index       : Node_Id;
      Base_Mark   : Entity_Id;
      Comp_Type   : Entity_Id;
      Comp_Base   : Entity_Id;
   begin
      --  Special case when the subtype mark is an array. The array must be
      --  unconstrainted and the constraint provided in subtype indication is
      --  a range or an attribute range. We replace the subtype indication with
      --  an anonymous array of range bounded by the constraint provided in 
      --  subtype indication.
      
      Mark := Subtype_Mark (Subtyp);
      Typ  := Etype (Mark);
      
      Write_Str (Ob, "array [");
      
      -- pragma Assert (not Is_Constrained (Typ));
      
      Constr := Constraint (Subtyp);
      if Nkind (Constr) = N_Index_Or_Discriminant_Constraint then
	 
	 Constr_List := Constraints (Constr);
	 Index := First (Constr_List);
	 
	 while Present (Index) loop
	    
	    case Nkind (Index) is
	       when N_Range_Constraint =>
		  Generate_Range_Constraint (This, Index);
		  
	       when N_Range =>
		  Generate_Range (This, Index);
		  
	       when N_Slice =>
		  --  ?? implicit subtype is created to describe the 
		  --  slice type, so that the bounds of this type are 
		  --  the bounds of the slice. Better use the implicit type
		  
		  Generate_Node (This, Discrete_Range (Index));
		  
	       when others =>
		  if Is_Access_Type (Index) then
		     Typ := Get_Type_Full_View (Designated_Type (Index));
		  else
		     Typ := Get_Type_Full_View (Index);
		  end if;
		  
		  Generate_Node (This, Type_Low_Bound (Etype (Typ)));
		  Write_Str (Ob, " .. ");
		  Generate_Node (This, Type_High_Bound (Etype (Typ)));
		  
	    end case;
	    
	    Next (Index);
	    
	    if Present (Index) then
	       Write_Str (Ob, ", ");
	    end if;
	 end loop;
      end if;
	 
      Write_Str (Ob, "] of ");
      
      Base_Mark := Base_Type (Mark);
      Comp_Type := Component_Type (Base_Mark);
      
      Comp_Base := Base_Type (Etype (Comp_Type));
      
      if Present (Comp_Base) 
	and then Nkind (Comp_Base) = N_Defining_Identifier 
      then
	 Write_Id (Ob, Comp_Base);
      end if;
      
      -- Comp := Component_Definition (Base_Type (Mark));
      
      -- Generate_Component_Definition (This, Comp);
	 
   end Subtype_Indication_As_Anonymous_Array;

   ----------------------------------
   -- Subtype_Indication_As_String --
   ----------------------------------
   
   procedure Subtype_Indication_As_String 
     (This : access Unity_Generator_Record;
      Typ  : Entity_Id) is
      
      Ob  : Output_Buffer := This.Get_Output_Buffer;       
      Len : Integer;
   begin
      pragma Assert (Ekind (Typ) = E_String_Literal_Subtype);
      
      Write_Str (Ob, "string[");
      Len := Integer (UI_To_Int (String_Literal_Length (Typ)));
      Write_Str (Ob, "]");
   end Subtype_Indication_As_String;
   
   -----------------------------
   -- Initialize_Simple_Value --
   -----------------------------
   
   procedure Initialize_Simple_Value 
     (This : access Unity_Generator_Record;
      Expr : Node_Id) is
      
      Ob   : Output_Buffer := This.Get_Output_Buffer;
   begin
      --  Here if we have an expression it must be static
      
      if Present (Expr) then
	 Write_Indent_Str (Ob, "<variableInit value=""");
	 
	 if Nkind (Expr) = N_Null then
	    Write_Str (Ob, "null");
	    
	 elsif Compile_Time_Known_Value (Expr) then
	    Generate_Node (This, Expr);
	    
	 else
	    Error_Msg_N ("unity does supported only static expression", Expr);
	    --  Generate_Node (This, Expr);
	 end if;
	 
	 Write_Str (Ob, """></variableInit>");
	 Indent_End (Ob);
	 Write_Eol (Ob);
      end if;
      
   exception
      when others =>
	 Put_Line ("Initialize_Simple_Value exception");
   end Initialize_Simple_Value;
   
   ----------------------------
   -- Initialize_Array_Value --
   ----------------------------
   
   procedure Initialize_Array_Value 
     (This : access Unity_Generator_Record;
      Expr : Node_Id) is
      
      Ob   : Output_Buffer := This.Get_Output_Buffer;
   begin
      --  Here if we have an expression it must be static
      
      Write_Indent_Str (Ob, "<instanceElementDesc name=""[");
      Write_Str (Ob, "]"">");
      Write_Eol (Ob);
      Indent_Begin (Ob);
      
      Write_Indent_Str (Ob, "<value>");
      Indent_End (Ob);
      
      --  <instanceElementDesc name="[1]">
      --  	<value>19</value>
      --  </instanceElementDesc>
      --  <instanceElementDesc name="[2]">
      --  	<value>20</value>
      --  </instanceElementDesc>
      --  <instanceElementDesc name="[3]">
      --  	<value>21</value>
      --  </instanceElementDesc>
      --  <instanceElementDesc name="[4]">
      --  	<value>22</value>
      --  </instanceElementDesc>
      --  <instanceElementDesc name="[5]">
      --  	<value>23</value>
      --  </instanceElementDesc>
      
      ----------------------
      -- Compound Element --
      ----------------------
      
      --  <instanceElementDesc name="[1]">
      --  	<instanceElementDesc name="Kp">
      --  		<value>0.1</value>
      --  	</instanceElementDesc>
      --  	<instanceElementDesc name="Ki">
      --  		<value>0.2</value>
      --  	</instanceElementDesc>
      --  	<instanceElementDesc name="Period">
      --  		<value>0.3</value>
      --  	</instanceElementDesc>
      --  	<instanceElementDesc name="Prev_Ki">
      --  		<value>0.4</value>
      --  	</instanceElementDesc>
      --  	<instanceElementDesc name="Prev_Clck">
      --  		<value>0.5</value>
      --  	</instanceElementDesc>
      --  	<instanceElementDesc name="Ki_High">
      --  		<value>0.6</value>
      --  	</instanceElementDesc>
      --  	<instanceElementDesc name="Ki_Low">
      --  		<value>0.7</value>
      --  	</instanceElementDesc>
      --  	<instanceElementDesc name="Init">
      --  		<value>False</value>
      --  	</instanceElementDesc>
      --  </instanceElementDesc>
      --  <instanceElementDesc name="[2]">
      --  	<instanceElementDesc name="Kp">
      --  		<value>0.10</value>
      --  	</instanceElementDesc>
      --  	<instanceElementDesc name="Ki">
      --  		<value>0.20</value>
      --  	</instanceElementDesc>
      --  	<instanceElementDesc name="Period">
      --  		<value>0.30</value>
      --  	</instanceElementDesc>
      --  	<instanceElementDesc name="Prev_Ki">
      --  		<value>0.40</value>
      --  	</instanceElementDesc>
      --  	<instanceElementDesc name="Prev_Clck">
      --  		<value>0.50</value>
      --  	</instanceElementDesc>
      --  	<instanceElementDesc name="Ki_High">
      --  		<value>0.60</value>
      --  	</instanceElementDesc>
      --  	<instanceElementDesc name="Ki_Low">
      --  		<value>0.70</value>
      --  	</instanceElementDesc>
      --  	<instanceElementDesc name="Init">
      --  		<value>True</value>
      --  	</instanceElementDesc>
      --  </instanceElementDesc>
      
			  
      null;
   exception
      when others =>
	 Put_Line ("Initialize_Array_Value exception");
   end Initialize_Array_Value;
   
   -------------------------------
   -- Has_Initialized_Component --
   -------------------------------
   
   function Has_Initialized_Component (E : Entity_Id) return Boolean is
      
      Comp   : Entity_Id;
      Dec    : Node_Id;
      Def    : Node_Id;
      Expr   : Node_Id;
      Subtyp : Node_Id;
      T      : Entity_Id;
   begin
      Comp := First_Component (E);
      while Present (Comp) loop
	 Dec    := Parent (Comp);
	 Def    := Component_Definition (Dec);
	 Subtyp := Subtype_Indication (Def);
	 Expr   := Expression (Dec);
	 
	 if Present (Expr) then
	    return True;
	    
	 elsif Present (Subtyp) 
	   and then Nkind_In (Nkind (Subtyp), N_Expanded_Name, N_Identifier) 
	 then
	    T := Base_Type (Etype (Entity (Subtyp)));
	    if Ekind (T) = E_Record_Type 
	      and then Has_Initialized_Component (T) 
	    then 
	       return True;
	    end if;
	 end if;

	 Comp := Next_Component (Comp);
      end loop;
      
      return False;
   end Has_Initialized_Component;
   
   -----------------------------
   -- Initialize_Record_Value --
   -----------------------------
   
   procedure Initialize_Record_Value_Old
     (This : access Unity_Generator_Record;
      E    : Node_Id) is
      
      Ob     : Output_Buffer := This.Get_Output_Buffer;
      Comp   : Entity_Id;
      Dec    : Node_Id;
      Def    : Node_Id;
      Expr   : Node_Id; 
      Subtyp : Node_Id;
      T      : Entity_Id;
  begin
      --  There two exclusives cases, the first one is when the record variable
      --  is initialzed by the mean of an aggregate expression and the second
      --  one is when some compoment of the record are initialized on the
      --  type declaration. In the second case, we must recurse on component
      --  which are record. All initializations are staic in reflex, so all of
      --  them can be initialized at the declaration and there is no need 
      --  to create an init procedure.
      
      --  First case : the record is initialized by an aggregate expression. 
      --  Walk througth aggregate and generate initialization for each field
      
      --  Second case : Here there is no aggregate expression to initialize the
      --  record. We must look to the type definition to see if component are
      --  directly initialized in the type. The walk recurse on compoment whose
      --  type is a record.
      
      Comp := First_Component (E);
      while Present (Comp) loop
	 
	 Dec    := Parent (Comp);
	 Def    := Component_Definition (Dec);
	 Subtyp := Subtype_Indication (Def);
	 Expr   := Expression (Dec);
	 
	 if Present (Expr) then
	    Write_Indent_Str (Ob, "<instanceElementDesc name=""");
	    Write_Id (Ob, Comp);
	    Write_Str (Ob, """>");
	    Write_Eol (Ob);
	    Indent_Begin (Ob);
	    Write_Indent_Str (Ob, "<value>");
	    Generate_Node (This,Expr);
	    Write_Indent_Str (Ob, "</value>");
	    Write_Eol (Ob);
	    Indent_End (Ob);
	    Write_Indent_Str (Ob, "</instanceElementDesc>");
	    Write_Eol (Ob);
	    
	 elsif Present (Subtyp) then
	    
	    if Nkind_In (Nkind (Subtyp), N_Expanded_Name, N_Identifier) then
	       
	       --  In case of record or record subtype we must recurse to see
	       --  if the indicated record type has initializaton
	       
	       T := Base_Type (Etype (Entity (Subtyp)));
	       
	       if Ekind (T) = E_Record_Type then
		  if Has_Initialized_Component (T) then
		     Write_Indent_Str (Ob, "<instanceElementDesc name=""");
		     Write_Id (Ob, Comp);
		     Write_Str (Ob, """>");
		     Write_Eol (Ob);
		     Indent_Begin (Ob);
		     
		     --  Initialize_Record_Value (This, T);
		     
		     Write_Indent_Str (Ob, "</instanceElementDesc>");
		     Write_Eol (Ob);
		  end if;
		  
		  --  Nothing to do for all cases, as there are initialized 
		  --  only by an expression in the declaration
		  
	       else
		  null;
	       end if;
	       
	    else
	       null;
	    end if;
	    
	 else
	    null;
	 end if;
	 
	 Comp := Next_Component (Comp);
      end loop;
      
	 
      --  Here if we have an expression it must be static
      
			--  <instanceElementDesc name="Kp">
			--  	<value>50.0</value>
			--  </instanceElementDesc>
			--  <instanceElementDesc name="Ki">
			--  	<value>51.0</value>
			--  </instanceElementDesc>
			--  <instanceElementDesc name="Period">
			--  	<value>52.0</value>
			--  </instanceElementDesc>
			--  <instanceElementDesc name="Prev_Ki">
			--  	<value>53.0</value>
			--  </instanceElementDesc>
			--  <instanceElementDesc name="Prev_Clck">
			--  	<value>54.0</value>
			--  </instanceElementDesc>
			--  <instanceElementDesc name="Ki_High">
			--  	<value>55.0</value>
			--  </instanceElementDesc>
			--  <instanceElementDesc name="Ki_Low">
			--  	<value>56.0</value>
			--  </instanceElementDesc>
			--  <instanceElementDesc name="Init">
			--  	<value>False</value>
			--  </instanceElementDesc>
      
      null;
   exception
      when others =>
	 Put_Line ("Initialize_Record_Value exception");
   end Initialize_Record_Value_Old;
   
   -----------------------
   -- Base_Itype_Entity --
   -----------------------
   
   function Base_Itype_Entity (E : Entity_Id) return Entity_Id is
   begin
      Put_Line ("Base_Itype_Entity Begin " & Get_String (Chars (E)));
      
      if Is_Itype (E) then
	 if E /= Etype (E) then
	    return Base_Itype_Entity (Etype (E));
	 end if;
      end if;
      
      return E;
   end Base_Itype_Entity;
   
   ---------------------------
   -- Handle_Anonymous_Type --
   ---------------------------
   
   procedure Handle_Anonymous_Type
     (This       : access Unity_Generator_Record;
      E          : Entity_Id;
      From_Array : Boolean := False) is
      
      Btype    : Entity_Id;
      Type_Def : Node_Id;
      Related_Nod : Node_Id;
   begin
      Put_Line ("Handle_Anonymous_Type Begin " & Get_String (Chars (E)));
      
      Btype := Base_Type (E);
      Put_Line ("Btype " & Get_String (Chars (Btype)));
      
      if Is_Anonym (Btype) then
	 Type_Def := Type_Definition (Parent (Btype));
	 
	 if Is_Array_Type (Btype) and not From_Array Then
	    Generate_Constrained_Array_Definition (This, Type_Def, True);
	    return;
	    
	 elsif Is_Access_Type (Btype) then
	    Generate_Access_Definition (This, Type_Def);
	    return;
	 else
	    Generate_Type_Name (This, E);
	 end if;
      else
	 Put_Line ("Handle_Anonymous_Type not is anomym " & Ekind (E)'Img);
	 
	 if Has_Type_For_Generation (E) then
	    Btype := Get_Type_For_Generation (E);
	 else
	    Btype := E;
	 end if;
	 Generate_Type_Name (This, Btype);
      end if;
      
      Put_Line ("Handle_Anonymous_Type End");
   end Handle_Anonymous_Type;

end Unity.Gen.Ch3;
