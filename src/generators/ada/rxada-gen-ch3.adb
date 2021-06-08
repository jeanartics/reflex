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

with Reflex.Gen.Ada_Outputs; use Reflex.Gen.Ada_Outputs;
with Reflex.Gen.Utils; use Reflex.Gen.Utils;
with Rxada.Gen.Ch2; use Rxada.Gen.Ch2;
with Rxada.Gen.Ch4; use Rxada.Gen.Ch4;
--with Reflex.Infos; use Reflex.Infos;

package body Rxada.Gen.Ch3 is

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
   
   function Is_Empty_Record (Node : Node_Id) return Boolean;
   
   ---------------------
   -- Is_Empty_Record --
   ---------------------
   
   function Is_Empty_Record (Node : Node_Id) return Boolean is
      
      Comp_List : Node_Id := Empty;
      Items     : List_Id;
      Item      : Node_Id;
   begin
      if Nkind (Node) = N_Record_Definition then
	 if Null_Present (Node) then
	    return True;
	 end if;
	 
	 Comp_List := Component_List (Node);
	 
      elsif Nkind (Node) = N_Component_List then
	 Comp_List := Node;
      end if;
      
      if Present (Comp_List) then
	 if Null_Present (Comp_List) then
	    return True;
	 end if;
	 
	 Items := Component_Items (Comp_List);
	 if Is_Empty_List (Items) then
	    return True;
	 end if;
	 
	 Item := First (Items);
	 while Present (Item) loop
	    if Chars (Defining_Identifier (Item)) /= Name_Uparent then
	       return False;
	    end if;
	    
	    Next (Item);
	 end loop;
      end if;
      
      return True;
   end Is_Empty_Record;
   
   -------------------------------
   -- Generate_Type_Declaration --
   -------------------------------
   
   procedure Generate_Type_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Write_Comment_Line_To_Node (This, Node);
      
      -----------------------------
      -- 3.2.1  Type Declaration --
      -----------------------------

      --  TYPE_DECLARATION ::=
      --    FULL_TYPE_DECLARATION
      --  | INCOMPLETE_TYPE_DECLARATION
      --  | PRIVATE_TYPE_DECLARATION
      --  | PRIVATE_EXTENSION_DECLARATION
      
      if Nkind (Node) = N_Full_Type_Declaration then
	 Generate_Full_Type_Declaration (This, Node);
	 
      elsif Nkind (Node) = N_Incomplete_Type_Declaration then
	 Generate_Incomplete_Type_Declaration (This, Node);
	 
      elsif Nkind (Node) = N_Private_Type_Declaration then
	 Generate_Private_Type_Declaration (This, Node);
	 
      elsif Nkind (Node) = N_Private_Extension_Declaration then
	 Generate_Private_Extension_Declaration (This, Node);
	 
      else
	 raise Program_Error;
      end if;
      
   end Generate_Type_Declaration;
   
   ------------------------------------
   -- Generate_Full_Type_Declaration --
   ------------------------------------
   
   procedure Generate_Full_Type_Declaration 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob       : Output_Buffer := This.Get_Output_Buffer;
      Def_Id   : Entity_Id := Defining_Entity (Node);
      Type_Def : Node_Id;
   begin
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
      
      --  Write_Itypes_In_Subtree (This, Node);
	 
      Type_Def := Type_Definition (Node);
      
      pragma Assert (Nkind (Type_Def) /= N_Access_Function_Definition);
      pragma Assert (Nkind (Type_Def) /= N_Access_Procedure_Definition);
      
      --  ENUMERATION_TYPE_DEFINITION :
      --  The enumeration type is translate as integer in Ada. The
      --  literals are replaced by their values, unless the option 
      --  Generate_Enumeration_Literal is set and then the literals are
      --  translate as integer constant wich value is the representation
      --  value of the literal
      
      if NKind (Type_Def) = N_Enumeration_Type_Definition then
	 Generate_Enumeration_Type_Definition (This, Node);
	 
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
	 Generate_Record_Definition (This, Node);
	 
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
	    
         --  REACTIVE_TYPE_DEFINITION
	    
      elsif Nkind (Type_Def) = N_Reactive_Type then
         Generate_Reactive_Type_Definition (This, Node);
	    
      else
         Error_Msg_N
           ("unknown reflex type definition for type ", Def_Id);
      end if;
      
      --  Set Output to null as the entry point of each declarations
      --  mmust set it to their owns buffer
      
   end Generate_Full_Type_Declaration;
   
   ----------------------------------------------
   -- Generate_Abstract_Subprogram_Declaration --
   ----------------------------------------------
   
   procedure Generate_Abstract_Subprogram_Declaration 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Abstract_Subprogram_Declaration;
   
   -----------------------------------------
   -- Generate_Access_Function_Definition --
   -----------------------------------------
   
   procedure Generate_Access_Function_Definition 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Access_Function_Definition;
   
   ------------------------------------------
   -- Generate_Access_Procedure_Definition --
   ------------------------------------------
   
   procedure Generate_Access_Procedure_Definition 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Access_Procedure_Definition;
   
   ------------------------------------
   -- Generate_Component_Association --
   ------------------------------------
   
   procedure Generate_Component_Association 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Component_Association;
   
   -----------------------------------
   -- Generate_Component_Definition --
   -----------------------------------
   
   procedure Generate_Component_Definition 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
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
	    
	    Generate_Subtype_Indication (This, Subtyp);
	    
	 elsif Nkind_In (Nkind (Subtyp), N_Expanded_Name, N_Identifier) then
	    Generate_Subtype_Mark (This, Subtyp);
	    
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
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob         : Output_Buffer := This.Get_Output_Buffer;
      Def_Id     : Entity_Id;
      Comp_Def   : Node_Id;
      Expr       : Node_Id;
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
      
      --  Does not generate internal names.
      
      if Chars (Def_Id) = Name_UParent then
      	 return;
      end if;
      
      Write_Indent (Ob);
      Write_Id (Ob, Def_Id);
      Write_Str (Ob, " : ");
      
      Comp_Def := Component_Definition (Node);
      Generate_Component_Definition (This, Comp_Def);
      
      Expr := Expression (Node);
      
      if Present (Expr) then
	 Write_Str (Ob, " := ");
	 Generate_Node (This, Expr);
      end if;
      
      Write_Char (Ob, ';');
      Write_Eol (Ob);
      
   end Generate_Component_Declaration;
   
   -----------------------------
   -- Generate_Component_List --
   -----------------------------
   
   procedure Generate_Component_List 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob        : Output_Buffer := This.Get_Output_Buffer;
      Comp_List : Node_Id;
      Items     : List_Id;
      N         : Node_Id;
   begin
      pragma Assert (Nkind (Node) = N_Record_Definition);
      
      Indent_Begin (Ob);
      
      Comp_List := Component_List (Node);
      Items     := Component_Items (Comp_List);
      
      if Null_Present (Node) or else Is_Empty_List (Items) then
	 return;
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
	 end if;
      end if;
      
      Indent_End (Ob);
   end Generate_Component_List;
   
   -------------------------------
   -- Generate_Delta_Constraint --
   -------------------------------
   
   procedure Generate_Delta_Constraint 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Delta_Constraint;
   
   --------------------------------------
   -- Generate_Derived_Type_Definition --
   --------------------------------------
   
   procedure Generate_Derived_Type_Definition 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob               : Output_Buffer := This.Get_Output_Buffer;
      Def_Id           : Entity_Id := Defining_Identifier (Node);
      Type_Def         : Node_Id;
      Record_Extension : Node_Id;
      Subtyp           : Node_Id;
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
	 
	 Write_Indent_Str (Ob, "type ");
	 Write_Id (Ob, Def_Id);
	 
	 Write_Str (Ob, " is new ");
	 Subtyp := Subtype_Indication (Type_Def);
	 
	 pragma Assert (Nkind_In (Subtyp, N_Expanded_Name, N_Identifier));
	 
	 Generate_Subtype_Mark (This, Subtyp);
	 
	 if Is_Empty_record (Record_Extension) then
	    Write_Str (Ob, " with null record;");
	    Write_Eol (Ob);
	    Write_Eol (Ob);
	    
	    --  Here we know that the record has compoenent others that
	    --  generated internally by compiler
	    
	 else
	    Write_Str (Ob, " with record");
	    Write_Eol (Ob);
	    
	    Indent_Begin (Ob);
	    Generate_Component_List (This, Record_Extension);
	    Indent_End (Ob);
	    
	    Write_Indent_Str (Ob, "end record;");
	    Write_Eol (Ob);
	    Write_Eol (Ob);
	 end if;
	 
      else 
	 Write_Indent_Str (Ob, "type ");
	 Write_Id (Ob, Def_Id);
	 Write_Str (Ob, " is new ");
	 
	 Generate_Subtype_Indication (This, Subtype_Indication (Type_Def));
	 
	 Write_Str (Ob, ";");
	 Write_Eol (Ob);
      end if;
   end Generate_Derived_Type_Definition;
   
   ---------------------------------------
   -- Generate_Reactive_Type_Definition --
   ---------------------------------------
   
   procedure Generate_Reactive_Type_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob       : Output_Buffer := This.Get_Output_Buffer;
      Def_Id   : Entity_Id := Defining_Identifier (Node);
      Type_Def : Node_Id;
   begin
      Put_Line
	("Generate_Reactive_Type_Definition " & Get_String (Chars (Def_Id)));
      Type_Def := Type_Definition (Node);
      Put_Line
	("   Type_def " & Nkind (Type_Def)'Img);
      

      pragma Assert (Nkind (Type_Def) = N_Reactive_Type);
      
      Write_Indent_Str (Ob, "type ");
      Write_Id (Ob, Def_Id);
      Write_Str (Ob, " is ");
      Write_Str (Ob, "reactive;");
      Write_Eol (Ob);
   end Generate_Reactive_Type_Definition;
   
   --------------------
   -- Generate_Range --
   --------------------
   
   procedure Generate_Range_Constraint 
     (This : access Ada_Generator_Record;
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
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob   : Output_Buffer := This.Get_Output_Buffer;
      Low  : Node_Id;
      High : Node_Id;
   begin
      Low := Low_Bound (Node);
      Generate_Node (This, Low_Bound (Node));
      
      Write_Str (Ob, "..");
			
      High := High_Bound (Node);
      Generate_Node (This, High_Bound (Node));
   end Generate_Range;
   
   -----------------------------------------------
   -- Generate_Index_Or_Discriminant_Constraint --
   -----------------------------------------------
   
   procedure Generate_Index_Or_Discriminant_Constraint 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob    : Output_Buffer := This.Get_Output_Buffer;
      Cons  : List_Id := Constraints (Node);
      Index : Node_Id;
   begin
      Write_Str (Ob, "(");
      Index := First (Cons);
      while Present (Index) loop
	 Generate_Node (This, Index);
	 Next (Index);
	 exit when No (Index);
	 Write_Str (Ob, ", ");
      end loop;
      
      Write_Str (Ob, ")");
   end Generate_Index_Or_Discriminant_Constraint;

   ---------------------------------------
   -- Generate_Discriminant_Association --
   ---------------------------------------
   
   procedure Generate_Discriminant_Association 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Discriminant_Association;
   
   -----------------------------------------
   -- Generate_Discriminant_Specification --
   -----------------------------------------
   
   procedure Generate_Discriminant_Specification 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Discriminant_Specification;
   
   ------------------------------------------
   -- Generate_Enumeration_Type_Definition --
   ------------------------------------------
   
   procedure Generate_Enumeration_Type_Definition  
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob       : Output_Buffer := This.Get_Output_Buffer;
      Type_Def : Node_Id;
      E        : Entity_Id := Defining_Identifier (Node);
      Lit      : Entity_Id;
      Line     : Boolean;
   begin
      Write_Indent_Str (Ob, "type ");
      Write_Id (Ob, E);
      Write_Str (Ob, " is");
      
      Type_Def := Type_Definition (Node);
      
      Line := List_Length (Literals (Type_Def)) > 4;
      
      Lit := First_Literal (E);
      -- First Literal
      
      if Present (Lit) then
	 if Line then
	    Write_Eol (Ob);
	    Indent_Begin (Ob);
	    Write_Indent_Str (Ob, "(");
	 else
	    Write_Str (Ob, " (");
	 end if;
	 
	 Write_Id (Ob, Lit);
	 
	 Lit := Next_Literal (Lit);
	 if Present (Lit) then
	    Write_Str (Ob, ", ");
	    if Line then
	       Write_Eol (Ob);
	    end if;
	 end if;
      end if;
      
      --  The folowwing literals
      
      while Present (Lit) loop
	 if Line then
	    Write_Indent_Str (Ob, " ");
	    Write_Id (Ob, Lit);
	 else
	    Write_Str (Ob, " ");
	    Write_Id (Ob, Lit);
	 end if;
	 
	 Lit := Next_Literal (Lit);
	 
	 if Present (Lit) then
	    if Line then
	       Write_Char (Ob, ',');
	       Write_Eol (Ob);
	    else
	       Write_Str (Ob, ", ");
	    end if;
	 end if;
      end loop;
      
      Write_Str (Ob, ");");
      
      if Line then
	 Indent_End (Ob);
      end if;
      
      Write_Eol (Ob);
   end Generate_Enumeration_Type_Definition;
   
   ----------------------------------
   -- Generate_Extension_Aggregate --
   ----------------------------------
   
   procedure Generate_Extension_Aggregate 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Extension_Aggregate;
   
   -------------------------------------
   -- Generate_Function_Specification --
   -------------------------------------
   
   procedure Generate_Function_Specification 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Function_Specification;
   
   -----------------------------------------
   -- Generate_Implicit_Label_Declaration --
   -----------------------------------------
   
   procedure Generate_Implicit_Label_Declaration 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Implicit_Label_Declaration;
   
   ------------------------------------------
   -- Generate_Incomplete_Type_Declaration --
   ------------------------------------------
   
   procedure Generate_Incomplete_Type_Declaration 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is

   begin
      null;
   end Generate_Incomplete_Type_Declaration;
   
   --------------------------------------
   -- Generate_Modular_Type_Definition --
   --------------------------------------
   
   procedure Generate_Modular_Type_Definition 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob       : Output_Buffer := This.Get_Output_Buffer;
      Type_Def : Node_Id;
      Expr     : Node_Id;
      E        : Entity_Id := Defining_Identifier (Node);
   begin
      Write_Indent_Str (Ob, "type ");
      Write_Id (Ob, E);
      Write_Str (Ob, " is mod ");
      
      Type_Def := Type_Definition (Node);
      Expr := Expression (Type_Def);
      Generate_Node (This, Expr);
      
      Write_Str (Ob, ";");
      Write_Eol (Ob);
   end Generate_Modular_Type_Definition;
   
   ----------------------------------------
   -- Generate_Floating_Point_Definition --
   ----------------------------------------
   
   procedure Generate_Floating_Point_Definition 
     (This : access Ada_Generator_Record;
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
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      -------------------------
      -- 3.2.2  Subtype Mark --
      -------------------------

      --  SUBTYPE_MARK ::= subtype_NAME
      
      pragma Assert (Nkind_In (Node, N_Expanded_Name, N_Identifier));
      
      Write_Id (Ob, Node);
      
   end Generate_Subtype_Mark;
   
   ---------------------------------
   -- Generate_Number_Declaration --
   ---------------------------------
   
   procedure Generate_Number_Declaration 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob     : Output_Buffer := This.Get_Output_Buffer;
      Def_Id : Entity_Id;
      Expr   : Node_Id;
   begin
      Def_Id := Defining_Identifier (Node);
      
      Write_Indent (Ob);
      Write_Id (Ob, Def_Id);
      Write_Str (Ob, " : constant := ");
      
      Expr := Expression (Node);
      if Present (Expr) then
	 Generate_Node (This, Expr);
      else
	 raise Program_Error;
      end if;
      
      Write_Str (Ob, ";");
      Write_Eol (Ob);
   end Generate_Number_Declaration;
   
   ---------------------------------
   -- Generate_Object_Declaration --
   ---------------------------------
   
   procedure Generate_Object_Declaration 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob       : Output_Buffer := This.Get_Output_Buffer;
      Def_Id   : Entity_Id := Defining_Identifier (Node);
      Type_Def : Node_Id;
      Expr     : Node_Id;
--        Mark     : Node_Id;
--        Constr   : Node_Id;
   begin
      Write_Comment_Line_To_Node (This, Node);
      
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
	 return;
      end if;
      
      if Nkind (Type_Def) = N_Access_Definition then
	 if Present (Access_To_Subprogram_Definition (Type_Def)) then
	    if Comes_From_Source (Node) then
	       Error_Msg_N
		 ("reflex does not supported access to subprogram ", Node);
	       return;
	       
	       --  The expander has generated an access to subprogram. No need
	       --  to generate it as Ada handles them.
	    else
	       return;
	    end if;
	 end if;
      end if;
	       
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
	 
      elsif Nkind (Type_Def) = N_Constrained_Array_Definition then
	 Generate_Constrained_Array_Definition (This, Type_Def);
	 
      elsif NKind (Type_Def) = N_Access_Definition then
	 Generate_Access_Definition (This, Type_Def);
	 
	 --  Never go here
      else
	 raise Program_Error;
      end if;
      
      --  Here if we have an expression it must be static
      
      Expr := Expression (Node);
      if Present (Expr) then
	 Write_Str (Ob, " := ");
	 if Nkind (Expr) = N_Null then
	    Write_Str (Ob, "null");
	    
	 elsif Compile_Time_Known_Value (Expr) then
	    Generate_Node (This, Expr);
	 else
	    Generate_Node (This, Expr);
	 end if;
      end if;
      
      Write_Str (Ob, ";");
      Write_Eol (Ob);
      
   exception
      when others =>
	 Put_Line ("Generate_Object_Declaration exception");
   end Generate_Object_Declaration;
   
   ------------------------------------------
   -- Generate_Object_Renaming_Declaration --
   ------------------------------------------
   
   procedure Generate_Object_Renaming_Declaration 
     (This : access Ada_Generator_Record;
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
	      ("reflex does not supported this definition type 12 ", Node);
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
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Ordinary_Fixed_Point_Definition;
   
   ------------------------------------
   -- Generate_Package_Instantiation --
   ------------------------------------
   
   procedure Generate_Package_Instantiation 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Package_Instantiation;
   
   -------------------------------------------
   -- Generate_Package_Renaming_Declaration --
   -------------------------------------------
   
   procedure Generate_Package_Renaming_Declaration 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Package_Renaming_Declaration;
   
   --------------------------------------------
   -- Generate_Private_Extension_Declaration --
   --------------------------------------------
   
   procedure Generate_Private_Extension_Declaration 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob     : Output_Buffer := This.Get_Output_Buffer;
      Def_Id : Entity_Id     := Defining_Identifier (Node);
      Mark   : Node_Id;
   begin
      Write_Indent_Str (Ob, "type ");
      Write_Id (Ob, Def_Id);
      Write_Str (Ob, " is new ");
      
      Mark := Subtype_Indication (Node);
      pragma Assert (Nkind_In (Mark, N_Expanded_Name, N_Identifier));
      
      Generate_Subtype_Mark (This, Mark);
      
      Write_Str (Ob, " with private;");
      Write_Eol (Ob);
   end Generate_Private_Extension_Declaration;
   
   ---------------------------------------
   -- Generate_Private_Type_Declaration --
   ---------------------------------------
   
   procedure Generate_Private_Type_Declaration 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob     : Output_Buffer := This.Get_Output_Buffer;
      Def_Id : Entity_Id     := Defining_Identifier (Node);
   begin
      Write_Indent_Str (Ob, "type ");
      Write_Id (Ob, Def_Id);
      
      if Tagged_Present (Node) then
	 Write_Str (Ob, " is tagged private;");
      else
	 Write_Str (Ob, " is private;");
      end if;
      
      Write_Eol (Ob);
   end Generate_Private_Type_Declaration;
   
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
   
   ---------------------------------------
   -- Generate_Real_Range_Specification --
   ---------------------------------------
   
   procedure Generate_Real_Range_Specification 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Real_Range_Specification;
   
   ----------------------------------------------
   -- Generate_Unconstrained_Array_Declaration --
   ----------------------------------------------
   
   procedure Generate_Unconstrained_Array_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob       : Output_Buffer := This.Get_Output_Buffer;
      Def_Id   : Entity_Id     := Defining_Identifier (Node);
      Type_Def : Node_Id       := Type_Definition (Node);
   begin
      pragma Assert (Nkind (Type_Def) = N_Unconstrained_Array_Definition);
      
      -- 3.6  Unconstrained Array Definition --
      -----------------------------------------

      --  UNCONSTRAINED_ARRAY_DEFINITION ::=
      --    array (INDEX_SUBTYPE_DEFINITION {, INDEX_SUBTYPE_DEFINITION}) of
      --      COMPONENT_DEFINITION

      --  Here we does not borrow about Subtype mark of index, as the discrete
      --  types are generated as integer in Ada. 
      
      Write_Eol (Ob);
      Write_Indent_Str (Ob, "type ");
      Write_Id (Ob, Def_Id);
      Write_Str (Ob, " is ");
      
      Generate_Unconstrained_Array_Definition (This, Type_Def);    
      
      Write_Char (Ob, ';');
      Write_Eol (Ob);
   end Generate_Unconstrained_Array_Declaration;
   
   --------------------------------------------
   -- Generate_Constrained_Array_Declaration --
   --------------------------------------------
   
   procedure Generate_Constrained_Array_Declaration
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob         : Output_Buffer := This.Get_Output_Buffer;
      Def_Id     : Entity_Id     := Defining_Identifier (Node);
      Type_Def   : Node_Id       := Type_Definition (Node);
   begin
      ---------------------------------------
      -- 3.6  Constrained Array Definition --
      ---------------------------------------

      --  CONSTRAINED_ARRAY_DEFINITION ::=
      --    array (DISCRETE_SUBTYPE_DEFINITION
      --      {, DISCRETE_SUBTYPE_DEFINITION})
      --        of COMPONENT_DEFINITION
      
      Write_Eol (Ob);
      Write_Indent_Str (Ob, "type ");
      Write_Id (Ob, Def_Id);
      Write_Str (Ob, " is ");
      
      Generate_Constrained_Array_Definition (This, Type_Def);
      
      Write_Char (Ob, ';');
      Write_Eol (Ob);
   end Generate_Constrained_Array_Declaration;
   
   ---------------------------------------------
   -- Generate_Unconstrained_Array_Definition --
   ---------------------------------------------
   
   procedure Generate_Unconstrained_Array_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob    : Output_Buffer := This.Get_Output_Buffer;
      Marks : List_Id;
      Dim   : Node_Id;
      Orig  : Node_Id;
   begin
      pragma Assert (Nkind (Node) = N_Unconstrained_Array_Definition);
      
      -- 3.6  Unconstrained Array Definition --
      -----------------------------------------

      --  UNCONSTRAINED_ARRAY_DEFINITION ::=
      --    array (INDEX_SUBTYPE_DEFINITION {, INDEX_SUBTYPE_DEFINITION}) of
      --      COMPONENT_DEFINITION

      --  Here we does not borrow about Subtype mark of index, as the discrete
      --  types are generated as integer in Ada. 
      
      Write_Str (Ob, " array (");
      
      --  For each dimension generate "<>"
      
      Marks := Subtype_Marks (Node);
      pragma Assert (Present (Marks));
      
      Dim := First (Marks);
      loop
	 Orig := Original_Node (Dim);
	 Generate_Node (This, Orig);
	 
	 Write_Str (Ob, " range <>");
	 Next (Dim);
	 exit when No (Dim);
	 Write_Str (Ob, ", ");
      end loop;
      
      Write_Str (Ob, ") of ");
      
      Generate_Component_Definition (This, Component_Definition (Node));
   end Generate_Unconstrained_Array_Definition;
   
   -------------------------------------------
   -- Generate_Constrained_Array_Definition --
   -------------------------------------------
   
   procedure Generate_Constrained_Array_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob      : Output_Buffer := This.Get_Output_Buffer;
      Indexes : List_Id;
      Dim     : Node_Id;
      Orig    : Node_Id;
   begin
      pragma Assert (Nkind (Node) = N_Constrained_Array_Definition);
	
      ---------------------------------------
      -- 3.6  Constrained Array Definition --
      ---------------------------------------

      --  CONSTRAINED_ARRAY_DEFINITION ::=
      --    array (DISCRETE_SUBTYPE_DEFINITION
      --      {, DISCRETE_SUBTYPE_DEFINITION})
      --        of COMPONENT_DEFINITION
      
      Write_Str (Ob, "array (");
      
      --  Loop through subscripts
      
      Indexes := Discrete_Subtype_Definitions (Node);
      Dim := First (Indexes);
      while Present (Dim) loop
	 Orig := Original_Node (Dim);
	 
	 -- N_Identifier, N_Subype_Indication, N_Range, Attrubute Range
	 -- N_Expanded_Name
	 Generate_Node (This, Orig);
	 
	 Next (Dim);
	 exit when No (Dim);
	 Write_Str (Ob, ", ");
      end loop;
      
      Write_Str (Ob, ") of ");
      
      Generate_Component_Definition (This, Component_Definition (Node));
   end Generate_Constrained_Array_Definition;
   
   --------------------------------
   -- Generate_Access_Definition --
   --------------------------------
   
   procedure Generate_Access_Definition
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob     : Output_Buffer := This.Get_Output_Buffer;
      Subtyp : Node_Id;
      Mark   : Node_Id;
      Const  : Node_Id;
   begin
      if Nkind (Node) = N_Access_Function_Definition then
	 Error_Msg_N
	   ("reflex does not supported access to function ", Node);
	 
      elsif Nkind (Node) = N_Access_Procedure_Definition then
	 Error_Msg_N
	   ("reflex does not supported access to procedure ", Node);
	 
      elsif Nkind (Node) = N_Access_To_Object_Definition then
	 
	 Subtyp := Subtype_Indication (Node);
	 
	 if Nkind (Subtyp) = N_Subtype_Indication then
	    Mark  := Subtype_Mark (Subtyp);
	    Const := Constraint (Subtyp);
	    
	    pragma Assert (Nkind_In (Mark, N_Expanded_Name, N_Identifier));
	    
	    Write_Str (Ob, " access ");
	    Generate_Node (This, Mark);
	    Write_Char (Ob, '(');
	    Generate_Node (This, Const);
	    Write_Char (Ob, ')');
	    
	 else
	    pragma Assert (Nkind_In (Subtyp, N_Expanded_Name, N_Identifier));
	 
            Generate_Node (This, Subtyp);
         end if;
	 
      elsif Nkind (Node) = N_Access_Definition then
	 
	 if Present (Access_To_Subprogram_Definition (Node)) then
	    Error_Msg_N
	      ("reflex does not supported access to subprogram ", Node);
	 else
	    
	    Mark := Subtype_Mark (Node);	 
	    if Present (Mark) then
	       pragma Assert (Nkind_In (Mark, N_Expanded_Name, N_Identifier));
	       
	       Write_Str (Ob, " access ");
	       Generate_Node (This, Mark);
	    else
	       raise Program_Error;
	    end if;
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
     (This : access Ada_Generator_Record;
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
      
      Write_Indent_Str (Ob, "type ");
      Write_Id (Ob, Def_Id);
      Write_Str (Ob, " is access ");
      
      Type_Def := Type_Definition (Node);
      Generate_Access_Definition (This, Type_Def);
      
      Write_Str (Ob, ";");
      Write_Eol (Ob);
   exception
      when others =>
	 Put_Line ("Generate_Access_To_Object_Definition exception");
   end Generate_Access_To_Object_Definition;
   
   --------------------------------
   -- Generate_Record_Definition --
   --------------------------------
   
   procedure Generate_Record_Definition 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob        : Output_Buffer := This.get_Output_Buffer;
      Def_Id    : Entity_Id := Defining_Identifier (Node);
      Type_Def  : Node_Id := Type_Definition (Node);
      Comp_List : Node_Id;
      Items     : List_Id;
   begin
      --  RECORD_DEFINITION ::=
      --    record
      --      COMPONENT_LIST
      --    end record
      --  | null record
      
      Write_Eol (Ob);
      Write_Indent (Ob);
      Write_Str (Ob, "type ");
      Write_Id (Ob, Def_Id);
      
      Comp_List := Component_List (Type_Def);
      Items     := Component_Items (Comp_List);
      
      if Null_Present (Type_Def) or else Is_Empty_List (Items) then
	 if Tagged_Present (Type_Def) then
	    Write_Str (Ob, " is tagged null record;");
	    Write_Eol (Ob);
	 else
	    Write_Str (Ob, " is null record;");
	    Write_Eol (Ob);
	 end if;
	 
      else	
	 if Tagged_Present (Type_Def) then
	    Write_Str (Ob, " is tagged record");
	 else
	    Write_Str (Ob, " is record");
	 end if;
	 Write_Eol (Ob);
      
	 Generate_Component_List (This, Type_Def);
      
	 Write_Indent_Str (Ob, "end record;");
	 Write_Eol (Ob);
      end if;
   end Generate_Record_Definition;
   
   --------------------------------------------
   -- Generate_Enumeration_Literal_Constants --
   --------------------------------------------
   
   procedure Generate_Enumeration_Literal_Constants
     (This : access Ada_Generator_Record;
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
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob       : Output_Buffer := This.Get_Output_Buffer;
      Def_Id   : Entity_Id     := Defining_Identifier (Node);
      Type_Def : Node_Id       := Type_Definition (Node);
      Low      : Node_Id;
      High     : Node_Id;
   begin
      Write_Indent_Str (Ob, "type ");
      Write_Id (Ob, Def_Id);
      Write_Str (Ob, " is range ");
      
      Low  := Low_Bound (Type_Def);
      Generate_Node (This, Low);
      
      Write_Str (Ob, " .. ");
      
      High := High_Bound (Type_Def);
      Generate_Node (This, High);
      
      Write_Char (Ob, ';');
      Write_Eol (Ob);
   end Generate_Signed_Integer_Type_Definition;
   
   ----------------------------------------------
   -- Generate_Subprogram_Renaming_Declaration --
   ----------------------------------------------
   
   procedure Generate_Subprogram_Renaming_Declaration 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Subprogram_Renaming_Declaration;
   
   ----------------------------------
   -- Generate_Subtype_Declaration --
   ----------------------------------
   
   procedure Generate_Subtype_Declaration 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob     : Output_Buffer := This.Get_Output_Buffer;
      Subtyp : Node_id;
      E      : Entity_Id := Defining_Identifier (Node);
   begin
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

      Write_Indent_Str (Ob, "subtype ");
      Write_Id (Ob, E);
      Write_Str (Ob, " is ");
      
      Subtyp := Subtype_Indication (Node);
      if Nkind (Subtyp) = N_Subtype_Indication then
	 Generate_Subtype_Indication (This, Subtyp);
	 
      else
	 pragma Assert
	   (Nkind_In (Subtyp, N_Expanded_Name, N_Identifier));
	 Generate_Subtype_Mark (This, Subtyp);
      end if;
      
      Write_Str (Ob,";");
      Write_Eol (Ob);
      
   end Generate_Subtype_Declaration;
   
   ---------------------------------
   -- Generate_Subtype_Indication --
   ---------------------------------
   
   procedure Generate_Subtype_Indication 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
      
      Ob     : Output_Buffer := This.Get_Output_Buffer;
      Mark   : Node_Id;
      Constr : Node_Id;
      Rng    : Node_Id;
      Low    : Node_Id;
      High   : Node_Id;
      Origin : Node_Id;
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
      
      
      if Nkind (Node) = N_Subtype_Indication then
	 Mark := Subtype_Mark (Node);
	 Generate_Subtype_Mark (This, Mark);
	 
	 Constr := Constraint (Node);
	 Origin := Original_Node (Constr);	 
	 
	 if Nkind (Constr) = N_Index_Or_Discriminant_Constraint then
	    Write_Str (Ob, " ");
	    Generate_Index_Or_Discriminant_Constraint (This, Constr);
	    
	 else   
	    Write_Str (Ob, " range ");
	 
	    if Nkind (Constr) = N_Range_Constraint then
	       Rng := Range_Expression (Constr);
	    else
	       Rng := Constr;
	    end if;
	    
	    Low := Low_Bound (Rng);
	    Generate_Node (This, Low);
	    
	    Write_Str (Ob, " .. ");
	    
	    High := High_Bound (Rng);
	    Generate_Node (This, High);
	 end if;
      else
	 pragma Assert (Nkind_In (Nkind (Node), N_Expanded_Name, N_Identifier));
	 Generate_Subtype_Mark (This, Node);
      end if;
   end Generate_Subtype_Indication;
   
   ----------------------
   -- Generate_Variant --
   ----------------------
   
   procedure Generate_Variant 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Variant;
   
   ---------------------------
   -- Generate_Variant_Part --
   ---------------------------
   
   procedure Generate_Variant_Part 
     (This : access Ada_Generator_Record;
      Node : Node_Id) is
   begin
      null;
   end Generate_Variant_Part;
   
   -------------------------------------------
   -- Subtype_Indication_As_Anonymous_Array --
   -------------------------------------------
   
   procedure Subtype_Indication_As_Anonymous_Array 
     (This   : access Ada_Generator_Record;
      Subtyp : Node_Id) is
      
      Ob          : Output_Buffer := This.Get_Output_Buffer;
      Mark        : Node_Id;
      Comp        : Node_Id;
      Constr      : Node_Id;
      Typ         : Entity_Id;
      Constr_List : List_Id;
      Index       : Node_Id;
      Base_Mark   : Entity_Id;
      Comp_Type   : Entity_Id;
      Comp_Base   : Entity_Id;
   begin
      --  Create a full type array declaration for subtype indication, with 
      --  component definition a a copy of the component definition of the
      --  suntype mark of the subtype indication
      
      Mark := Subtype_Mark (Subtyp);
      Typ  := Etype (Mark);
      
      Write_Indent_Str (Ob, "type ");
      Write_Id (Ob, Typ);
      Write_Str (Ob, "_rx");
      Write_Str (Ob, " is array(");
      
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
	 
      Write_Str (Ob, ") of ");
      
      Comp := Component_Definition (Parent (Mark));
      
      Generate_Component_Definition (This, Comp);
      
      Write_Str (Ob, ";");
      Write_Eol (Ob);
      
      --  Emit the subtype indication as subtype mark to the newly created 
      --  array
      
      Write_Indent_Str (Ob, "type ");
      Write_Str (Ob, "Id_A_Mettre");
      Write_Str (Ob, "is arra");
      
      --  Special case when the subtype mark is an array. The array must be
      --  unconstrainted and the constraint provided in subtype indication is
      --  a range or an attribute range. We replace the subtype indication with
      --  an anonymous array of range bounded by the constraint provided in 
      --  subtype indication.
      
      Mark := Subtype_Mark (Subtyp);
      Typ  := Etype (Mark);
      
      Write_Str (Ob, "array (");
      
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
	 
      Write_Str (Ob, ") of ");
      
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
     (This : access Ada_Generator_Record;
      Typ  : Entity_Id) is
      
      Ob      : Output_Buffer := This.Get_Output_Buffer;       
      Low     : Node_Id;
      Low_Val : Uint;
      Len     : Uint;
   begin
      pragma Assert (Ekind (Typ) = E_String_Literal_Subtype);
      
      Low := String_Literal_Low_Bound (Typ);
      Len := String_Literal_Length (Typ);
      Write_Str (Ob, "string (");
      
      if Compile_Time_Known_Value (Low) then
	 Low_Val := Expr_Value (Low);
	 Write_Uint (Ob, Low_Val);
	 Write_Str (Ob, " .. ");
	 Write_Uint (Ob, UI_Add (Low_Val, Len));
      else
	 Generate_Node (This, Low);
	 Write_Str (Ob, " .. (");
	 Generate_Node (This, Low);
	 Write_Str (Ob, " + ");
	 Write_Uint (Ob, Len);
	 Write_Str (Ob, " - 1)");
      end if;
   end Subtype_Indication_As_String;
   
end Rxada.Gen.Ch3;
