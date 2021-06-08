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
with Nmake; use Nmake;
with Stand; use Stand;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Tbuild; use Tbuild;
with Uintp; use Uintp;
with Urealp;   use Urealp;
with Stand;    use Stand;
with Snames; use Snames;

with Reflex_Options;
with Reflex.External_Names; use Reflex.External_Names;
with Reflex.Expanders.Utils; use Reflex.Expanders.Utils;
with Reflex.Expanders.Dispatch; use Reflex.Expanders.Dispatch;
with Reflex.Expanders.Ch2; use Reflex.Expanders.Ch2;
with Reflex.Expanders.Ch4; use Reflex.Expanders.Ch4;
with Reflex.Expanders.Itypes; use Reflex.Expanders.Itypes;

--with Glips_Options;

package body Reflex.Expanders.Ch3 is
   
   Derived_As_Alias : Boolean := False;   
   Option_Generate_Enum_Literal : boolean := False;
   
   procedure Check_Components
     (Node             : Node_Id;
      Clist            : Node_Id;
      Allow_Last_Field : Boolean);
   --  Check validity of components in Clist. Emit an error if a
   --  type whose size depends on a discriminant is found, unless
   --  Allow_Last_Field is True and this is the type of the last
   --  field in a record.
   
   procedure Expand_Object_Subtype_Indication 
     (This    : access Reflex_Expander_Record;
      Obj_Def : Node_Id);
   
   function Change_Index_To_Range
     (This : access Reflex_Expander_Record;
      C    : Node_Id) return Node_Id;
   
   function Change_Array_Indexes_To_Range
     (This    : access Reflex_Expander_Record;
      Indexes : List_Id) return List_Id;
   
   function Change_Array_Constaint_To_Range
     (This : access Reflex_Expander_Record;
      Node : Node_Id) return List_Id;
   
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
   -- Expand_Type_Declaration --
   -------------------------------
   
   procedure Expand_Type_Declaration
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      --  if not Comes_From_Source (Node) then
      --  	 return;
      --  end if;
      
      -----------------------------
      -- 3.2.1  Type Declaration --
      -----------------------------

      --  TYPE_DECLARATION ::=
      --    FULL_TYPE_DECLARATION
      --  | INCOMPLETE_TYPE_DECLARATION
      --  | PRIVATE_TYPE_DECLARATION
      --  | PRIVATE_EXTENSION_DECLARATION

      if Nkind (Node) = N_Full_Type_Declaration then
	 Expand_Full_Type_Declaration (This, Node);
	 
      elsif Nkind (Node) = N_Incomplete_Type_Declaration then
	 null;
	 
      elsif Nkind (Node) = N_Private_Type_Declaration then
	 null;
	 
      elsif Nkind (Node) = N_Private_Extension_Declaration then
	 null;
	 
      else
	 raise Program_Error;
      end if;
   end Expand_Type_Declaration;
   
   ------------------------------------
   -- Expand_Full_Type_Declaration --
   ------------------------------------
   
   procedure Expand_Full_Type_Declaration 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
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
      
--        if Nkind (Node) = N_Task_Type_Declaration then
--  	 Error_Msg_N ("reflex does not supported task type", Def_Id);
--  	 
--        elsif Nkind (Node) = N_Protected_Type_Declaration then
--  	 Error_Msg_N ("reflex does not supported protected type for ", Def_Id);
--  	 
--        else
	 --  Write_Itypes_In_Subtree (This, Node);
	 
	 Type_Def := Type_Definition (Node);
	 
	 --  ENUMERATION_TYPE_DEFINITION :
	 --  The enumeration type is translate as integer in Glips. The
	 --  literals are replaced by their values, unless the option 
	 --  Expand_Enumeration_Literal is set and then the literals are
	 --  translate as integer constant which value is the representation
	 --  value of the literal
	 
	 if NKind (Type_Def) = N_Enumeration_Type_Definition then
	    Expand_Enumeration_Type_Definition (This, Node);
	    
	    --  if Reflex_Options.Generate_Enum_As_Constants then
	    --    null; -- Expand_Enumeration_Literal_Constants (This, Node);
	    --  end if;
	    
	    --  INTEGER_TYPE_DEFINITION : 
	    --  They are emit as integer or unsigned if the type is a modular
	    --  type. So nothing to do with discrete type as there are declared
	    --  as integer_x or unisgned_x for modular types, with x the size
	    --  in bits of the type (8, 16, 32, 64)
	    
	 elsif Nkind (Type_Def) = N_Signed_Integer_Type_Definition then
	    Expand_Signed_Integer_Type_Definition (This, Node);
	    
	 elsif Nkind (Type_Def) = N_Modular_Type_Definition then
	    Expand_Modular_Type_Definition (This, Node);
	    
	    --  REAL_TYPE_DEFINITION 
	    --  Emits as real types, so no need to declared it
	    
	 elsif Nkind (Type_Def) = N_Floating_Point_Definition then
	    Expand_Floating_Point_Definition (This, Node);
	    
	    --  RECORD_TYPE_DEFINITION
	    
	 elsif Nkind (Type_Def) = N_Record_Definition then
	    Expand_Record_Definition (This, Node);
      
	    --  ARRAY_TYPE_DEFINITION 
	    
	 elsif Nkind (Type_Def) = N_Unconstrained_Array_Definition then
	    Expand_Unconstrained_Array_Declaration (This, Node);
	    
	 elsif Nkind (Type_Def) = N_Constrained_Array_Definition then
	    Expand_Constrained_Array_Declaration (This, Node);
	    
	    --  ACCESS_TYPE_DEFINITION
	    
	 elsif Nkind (Type_Def) = N_Access_To_Object_Definition then
	    Expand_Access_To_Object_Definition (This, Node, Node);
	    
	 elsif Nkind (Type_Def) = N_Access_Function_Definition then
	    Error_Msg_N
	      ("reflex does not supported access to function", Def_Id);
	    
	 elsif Nkind (Type_Def) = N_Access_Procedure_Definition then
	    Error_Msg_N
	      ("reflex does not supported access to procedure", Def_Id);
	    
	    --  DERIVED_TYPE_DEFINITION
	    
	 elsif Nkind (Type_Def) = N_Derived_Type_Definition then
	    Expand_Derived_Type_Definition (This, Node);
	    
         elsif Nkind (Type_Def) = N_Subtype_Indication then
            --  ???
	    Expand_Subtype_Indication (This, Node, Node);
	    
	 else
	    Error_Msg_N
	      ("unknown reflex type definition for type ", Def_Id);
	 end if;
    end Expand_Full_Type_Declaration;
   
   --------------------------------------------
   -- Expand_Abstract_Subprogram_Declaration --
   --------------------------------------------
   
   procedure Expand_Abstract_Subprogram_Declaration 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Abstract_Subprogram_Declaration;
   
   -----------------------------------------
   -- Expand_Access_Function_Definition --
   -----------------------------------------
   
   procedure Expand_Access_Function_Definition 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Access_Function_Definition;
   
   ------------------------------------------
   -- Expand_Access_Procedure_Definition --
   ------------------------------------------
   
   procedure Expand_Access_Procedure_Definition 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Access_Procedure_Definition;
   
   ------------------------------------
   -- Expand_Component_Association --
   ------------------------------------
   
   procedure Expand_Component_Association 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Component_Association;
   
   -----------------------------------
   -- Expand_Component_Definition --
   -----------------------------------
   
   procedure Expand_Component_Definition 
     (This      : access Reflex_Expander_Record;
      Node      : Node_Id;
      Decl_Node : Node_Id) is
      
      Subtyp     : Node_Id;
      Access_Def : Node_Id;
      Mark       : Node_Id;
      Comp_Def   : Node_Id;
      Const      : Node_Id;
      Id         : Entity_Id;
      Par        : Node_Id;
      Decl       : Node_Id;
      Indexes    : List_Id;
      Full_Type  : Node_Id;
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
	 
	 --  Subtype Indication is a constraint for an unconstrainted array.
	 --  For a component declaration of a record we generate an anonymous
	 --  array type definition, as for Object Declaration. For an array
	 --  we create an array type with Indexes given by the constrains and
	 --  Component of the array is a reference to the Component Type of the
	 --  Subtype Mark which must an unconstrainted array

	 if Nkind (Subtyp) = N_Subtype_Indication then
	    
	    --  Case Record Component
	    
	    if Nkind (Parent (Node)) = N_Component_Declaration then
	       Expand_Object_Subtype_Indication (This, Subtyp);
	       
	       --  Case Component of an Array 
	    else
	       Mark  := Subtype_Mark (Subtyp);
	       Const := Constraint (Subtyp);
	       
	       pragma Assert
		 (Is_Array_Type (Base_Type (Entity (Mark))));
	       pragma Assert
		 (Nkind (Const) = N_Index_Or_Discriminant_Constraint);
	       
	       Par := Parent (Node);
	       pragma Assert (Nkind (Par) = N_Constrained_Array_Definition);
	       
	       Decl := Parent (Par);
	       pragma Assert (Nkind (Decl) = N_Full_Type_Declaration);
	       
	       Comp_Def := Component_Type (Base_Type (Entity (Mark)));
	       Indexes  := Change_Array_Constaint_To_Range (This, Const);
	       
	       Id := Make_Defining_Identifier 
		 (Sloc    => Sloc (Node),
		  Chars   => New_Type_Name (Chars (Mark)));
	       
	       Full_Type := Make_Full_Type_Declaration
		 (Sloc                => Sloc (Node),
		  Defining_Identifier => Id,
		  Type_Definition  => 
		    Make_Constrained_Array_Definition
		    (Sloc                         => Sloc (Subtyp),
		     Discrete_Subtype_Definitions => Indexes,
		     Component_Definition         => 
		       Make_Component_Definition
		       (Sloc               => Sloc (Subtyp),
			Subtype_Indication => 
			  New_Occurrence_Of (Comp_Def, Sloc (Subtyp)))));
	       
	       Set_Ekind (Id, E_Array_Type);
	       Set_Etype (Id, Id);
	       Set_First_Index    (Id, First (Indexes));
	       Set_Component_Type (Id, Comp_Def);
	       Set_Is_Constrained (Id, True);
	       Set_Scope (Id, Scope (Defining_Identifier (Decl)));
	       Append_Entity (Id, Scope (Defining_Identifier (Decl)));
	       
	       Insert_Before (Decl, Full_Type);
	       Rewrite (Subtyp, New_Occurrence_Of (Id, Sloc (Subtyp)));
	    end if;
	    
	 elsif Nkind_In (Nkind (Subtyp), N_Expanded_Name, N_Identifier) then
	    Expand_Subtype_Mark (This, Subtyp);
	    
	 else
	    Error_Msg_N
	      ("reflex does not supported this definition type 1 ", Node);
	 end if;
	    
      else
	 Access_Def := Access_Definition (Node);
	 if Present (Access_Def) then
	    Expand_Access_Definition (This, Access_Def, Decl_Node);
	 else
	    Error_Msg_N
	      ("reflex does not supported this definition type 2 ", Node);
	 end if;
      end if;
   end Expand_Component_Definition;
   
   ------------------------------------
   -- Expand_Component_Declaration --
   ------------------------------------
   
   procedure Expand_Component_Declaration 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Def_Id   : Entity_Id;
      Comp_Def : Node_Id;
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
      
      Comp_Def := Component_Definition (Node);
      Expand_Component_Definition (This, Comp_Def, Node);
      
      if Present (Expression (Node)) then
	 Expand_Node (This, Expression (Node));
      end if;
	 
      --  If an expression is present create the Record Initialization 
      --  aggregate, if is not already created and add it to the record 
      --  definition
      
      --  If exprssion is not created and the type of component is a Record 
      --  Type or an Array Type and either or one type need initialization
      --  set an expression to this type 
      
      --  if Present (Expression (Node)) then
	 
      --  	 --  Get corresponding N_Record_Declartion node
	 
      --  	 --  Get Initialisation Aggregate 
	 
      --  	 Init := Init_Record (Rec_Type);
	 
      --  	 --  If not ralready created, cretae it
	 
      --  	 if No (Init) then
      --  	    Create_Init_Record;
      --  	    Init := Init_Record (Rec_Dec);
      --  	 end if;
	 
      --  	 pragma Assert (Present (Init_Record));
	 
      --  	 --  Create component Association with Exprssion set to compoenent
      --  	 --  exprssion.
	 
      --  	 --  Add Component Association to the init aggragte
	 
      --  	 --  And we are on
	 
      --  elsif Is_Record_Type (Etype (Defining_Identifier (Rec_Dec))) then
	 
      --  	 Init := Init_Record (Etype (Defining_Identifier (Node)));
	 
      --  	 if Present 
	 
      --  elsif Is_Array_Type (Etype (Defining_Identifier (Rec_Dec))) then
      --  	 null;
      --  end if;
      
      --  The type of the aggregate is the type of the record, and the type
      --  of the expression in the newly component association is the type
      --  of the component
      
      -- Comp_Def := Etype (Def_Id);
      
      --  Arec_Id := Activation_Record_Component (Def_Id);
      --  if Present (Arec_Id) then
      --  	 Comp_Def := Etype (Arec_Id);
      --  end if;
      
      --  if Is_Access_Type (Comp_Def) then
      --  	 Write_Id (This, Etype (Comp_Def));
      --  else
      --  	 Write_Id (This, Etype (Comp_Def));
      --  end if;
      
	 --  Comp_Def := Component_Definition (Node);
      --  Expand_Component_Definintion (Comp_Def);
      
   end Expand_Component_Declaration;
   
   -----------------------------
   -- Expand_Component_List --
   -----------------------------
   
   procedure Expand_Component_List 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Comp_List : Node_Id;
      Items     : List_Id;
      N         : Node_Id;
   begin
      pragma Assert (Nkind (Node) = N_Record_Definition);
      
      if Null_Present (Node) then
	 null;
	 
      else	
	 Comp_List := Component_List (Node);
	 Items     := Component_Items (Comp_List);
	 if Is_Non_Empty_List (Items) then
	    N := Nlists.First (Items);
	    
	    while Present (N) loop
	       if Nkind (N) = N_Component_Declaration then
		  Expand_Component_Declaration (This, N);
	       end if;
	       Next (N);
	    end loop;
	    
	 else
	    null; 
	 end if;
      end if;
   end Expand_Component_List;
   
   -------------------------------
   -- Expand_Delta_Constraint --
   -------------------------------
   
   procedure Expand_Delta_Constraint 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Delta_Constraint;
   
   --------------------------------------
   -- Expand_Derived_Type_Definition --
   --------------------------------------
   
   procedure Expand_Derived_Type_Definition 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      -------------------------------
      -- Ulimate_Entity_From_Itype --
      -------------------------------
      
      function Ulimate_Entity_From_Itype (E : Entity_Id) return Entity_Id is
	 
	 Ent  : Entity_Id := E;
	 Asso : Node_Id;
      begin
	 Put_Line
	   ("Ulimate_Entity_From_Itype Begin " & Get_String (Chars (Ent)));
	 
	 while Is_Itype (Ent) loop
	    if Ent = Etype (Ent) then
	       Asso := Associated_Node_For_Itype (Ent);
	       pragma Assert (Nkind (Parent (Ent)) = N_Full_Type_Declaration);
               Ent := Defining_Identifier (Asso);
	       exit;
            else
               Ent := Etype (Ent);
	    end if;
	 end loop;
	 
	 Put_Line ("Ulimate_Entity_From_Itype End " & Get_String (Chars (Ent)));
	 return Ent;
      end Ulimate_Entity_From_Itype;
      
      Def_Id           : Entity_Id := Defining_Identifier (Node);
      Type_Def         : Node_Id;
      Subtyp           : Node_Id;
      Record_Extension : Node_Id;
      Array_Def        : Node_Id;
      Mark             : Node_Id;
      Mark_Type        : Entity_Id;
      Const            : Node_Id;
      Comp_Def         : Entity_Id;
      Full_Type        : Node_Id;
      Indexes          : List_Id;
      Par              : Node_Id;
      Par_Def          : Node_Id;
      New_Def          : Node_Id;
   begin
      ----------------------------------
      -- 3.4  Derived Type Definition --
      ----------------------------------

      --  DERIVED_TYPE_DEFINITION ::=
      --    [abstract] [limited] new [NULL_EXCLUSION] parent_SUBTYPE_INDICATION
      --    [[and INTERFACE_LIST] RECORD_EXTENSION_PART]
      
      Type_Def         := Type_Definition (Node);
      Subtyp           := Subtype_Indication (Type_Def);
      Record_Extension := Record_Extension_Part (Type_Def);
      
      if Present (Record_Extension) then
	 Expand_Component_List (This, Record_Extension_Part (Type_Def));
	 
	 if Need_Init_Record (Def_Id) then
	    Create_Initialization_Record (Node, Node);
	 end if;
	 
      elsif Nkind (Subtyp) = N_Subtype_Indication then
	 --  Change the Derived Type by an Array Full Type declaration
	 
	 Mark      := Subtype_Mark (Subtyp);
	 Mark_Type := Base_Type (Entity (Mark));
	 Const     := Constraint (Subtyp);
	 
	 if Is_Array_Type (Mark_Type) then
	    
	    pragma Assert
	      (Nkind (Const) = N_Index_Or_Discriminant_Constraint);
	    
	    Comp_Def := Component_Type (Mark_Type);
	    Indexes  := Change_Array_Constaint_To_Range (This, Const);
	    
	    Array_Def := Make_Constrained_Array_Definition
	      (Sloc                         => Sloc (Type_Def),
	       Discrete_Subtype_Definitions => Indexes,
	       Component_Definition         => 
		 Make_Component_Definition
		 (Sloc               => Sloc (Type_Def),
		  Subtype_Indication => 
		    New_Occurrence_Of (Comp_Def, Sloc (Type_Def))));
	    
	    Full_Type := Make_Full_Type_Declaration
	      (Sloc                => Sloc (Node),
	       Defining_Identifier => Def_Id,
	       Type_Definition     => Array_Def);
	    
	    Set_Ekind          (Def_Id, E_Array_Type);
	    Set_Etype          (Def_Id, Def_Id);
	    Set_First_Index    (Def_Id, First (Indexes));
	    Set_Component_Type (Def_Id, Comp_Def);
	    Set_Is_Constrained (Def_Id, True);
	    
	    Rewrite (Node, Full_Type);
	    
	    --  Here we know that is not an array so the subtype indication 
	    --  must be a range exprsssion. So we change the Drerived Type 
	    --  to A Full Type with type definition is the Subtype_Indication 
	    --  of the Derived Type

	 else	 
	    Set_Ekind (Def_Id, Ekind (Mark_Type));
	    Set_Etype (Def_Id, Def_Id);
	 end if;
	 
	 --  Here we known that the subtype indication is neither an array
	 --  not a Tagged Type as there is no record extension.
	 
      else
	 Put_Line ("Derived is not subtype nor tagged");
	 Mark := Subtyp;
	 Mark_Type := Entity (Mark);
	 
	 Put_Line ("Mark " & Get_String (Chars (Mark)));
	 Put_Line ("Mark Type " & Get_String (Chars (Mark_Type)));
	 
	 Put_Line ("Is_array_type " & Is_Array_Type (Mark_Type)'Img);
	 Put_Line ("Is_Constrained " & Is_Constrained (Mark_Type)'Img);
	 if not Is_Array_Type (Mark_Type) 
	   or else Is_Constrained (Mark_Type) 
	 then
	    Put_Line ("Etype " & Get_String (Chars (Etype (Mark_Type))));
	    if Is_Itype (Etype (Mark_Type)) then
	       Mark_Type := 
		 Ulimate_Entity_From_Itype (Etype (Mark_Type));
	    end if;
	    
	    if Derived_As_Alias then
	       Put_Line ("last Mark Type " & Get_String (Chars (Mark_Type)));
	       
	       Set_Type_For_Generation (Def_Id, Mark_Type);
	       Rewrite
		 (Type_Def, New_Occurrence_Of (Mark_Type, Sloc (Type_Def)));
	    else
	       Par := Parent (Mark_Type);
	       pragma Assert (Nkind (Par) = N_Full_Type_Declaration);
	       
	       Par_Def := Type_Definition (Par);
	       New_Def := New_Copy_Tree (Par_Def);
	       Rewrite (Type_Def, New_Def);
	    end if;
	 end if;
      end if;
   end Expand_Derived_Type_Definition;
   
   --------------------
   -- Expand_Range --
   --------------------
   
   procedure Expand_Range_Constraint 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Rng : Node_Id;
   begin
      Rng := Range_Expression (Node);
      Expand_Node (This, Rng);
   end Expand_Range_Constraint;
   
   --------------------
   -- Expand_Range --
   --------------------
   
   procedure Expand_Range 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Low      : Node_Id;
      High     : Node_Id;
      Low_Val  : Uint;
      High_Val : Uint;
   begin
      Low := Low_Bound (Node);
      if Compile_Time_Known_Value (Low) then
         if Is_Real_Type (Etype (Low)) then
	    Expand_Real_Literal (This, Low);
	 else
            Low_Val := Expr_Value (Low);
	 end if;
      else
	 Expand_Node (This, Low_Bound (Node));
      end if;
      
      High := High_Bound (Node);
      if Compile_Time_Known_Value (High) then
         if Is_Real_Type (Etype (High)) then
	    Expand_Real_Literal (This, High);
	 else
            High_Val := Expr_Value (High);
	 end if;
      else
	 Expand_Node (This, High_Bound (Node));
      end if;
   end Expand_Range;
   
   -----------------------------------------------
   -- Expand_Index_Or_Discriminant_Constraint --
   -----------------------------------------------
   
   procedure Expand_Index_Or_Discriminant_Constraint 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Index_Or_Discriminant_Constraint;

   ---------------------------------------
   -- Expand_Discriminant_Association --
   ---------------------------------------
   
   procedure Expand_Discriminant_Association 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Discriminant_Association;
   
   -----------------------------------------
   -- Expand_Discriminant_Specification --
   -----------------------------------------
   
   procedure Expand_Discriminant_Specification 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Discriminant_Specification;
   
   ------------------------------------------
   -- Expand_Enumeration_Type_Definition --
   ------------------------------------------
   
   procedure Expand_Enumeration_Type_Definition  
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
--        case Glips_Options.Literal_Gen is
--  	 when Glips_Options.Literal_Values   =>
--  	    --  Nothing to do
--  	    
--  	    null;
--  	 when Glips_Options.Literal_Constnts =>
--  	    --  Declare constants for each literal
--  	    
--  	    Expand_Enumeration_Literal_Constants (This, Node);
--  	    
--  	 when Glips_Options.Literal_Enum     =>
--  	    --  No thing to do as the Glips generator emits a Glips enum
--  	    
--  	    null;
--        end case;
null;   end Expand_Enumeration_Type_Definition;
   
   ----------------------------------
   -- Expand_Extension_Aggregate --
   ----------------------------------
   
   procedure Expand_Extension_Aggregate 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Extension_Aggregate;
   
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
   
   -----------------------------------------
   -- Expand_Implicit_Label_Declaration --
   -----------------------------------------
   
   procedure Expand_Implicit_Label_Declaration 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Implicit_Label_Declaration;
   
   ------------------------------------------
   -- Expand_Incomplete_Type_Declaration --
   ------------------------------------------
   
   procedure Expand_Incomplete_Type_Declaration 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Incomplete_Type_Declaration;
   
   --------------------------------------
   -- Expand_Modular_Type_Definition --
   --------------------------------------
   
   procedure Expand_Modular_Type_Definition 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      --  Nothing to do as the modular types are Expandd as unsigned_integer. 
      --  The references to the types are replaced by unsigned_x where x is 
      --  the number of bits needed to holds a value of the type
      null;
   end Expand_Modular_Type_Definition;
   
   ----------------------------------------
   -- Expand_Floating_Point_Definition --
   ----------------------------------------
   
   procedure Expand_Floating_Point_Definition 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      --  Nothing to do as the floating point are  Expandd as float. 
      --  The references to the types are replaced by float. 
      null;
   end Expand_Floating_Point_Definition;
   
   ---------------------------
   -- Expand_Subtype_Mark --
   ---------------------------
   
   procedure Expand_Subtype_Mark
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Typ : Entity_Id;
   begin
      -------------------------
      -- 3.2.2  Subtype Mark --
      -------------------------

      --  SUBTYPE_MARK ::= subtype_NAME
      
      pragma Assert (Nkind_In (Node, N_Expanded_Name, N_Identifier));
      
      Typ := Etype (Entity (Node));
      
      if Is_String_Type (Typ) then
	 Subtype_Indication_As_String (This, Typ);
	 
      elsif Is_Array_Type (Typ) then
         Expand_Type_Name (This, Entity (Node));
	
      else
         Expand_Type_Name (This, Base_Type (Entity (Node)));
      end if;
      
   end Expand_Subtype_Mark;
   
   ---------------------------------
   -- Expand_Number_Declaration --
   ---------------------------------
   
   procedure Expand_Number_Declaration 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Def_Id : Entity_Id;
      Expr   : Node_Id;
      Typ    : Node_Id;
   begin
      Def_Id := Defining_Identifier (Node);
      
      Typ := Get_Type_Full_View (Etype (Def_Id));
      
      Expr := Expression (Node);
      if Present (Expr) then
         
         if Is_Real_Type (Etype (Expr)) then
            null; -- write_Str (ob, "float ");
         else
            Expand_Type_Name (This, Typ);
         end if;

	 if Compile_Time_Known_Value (Expr) then
            if Is_Real_Type (Etype (Expr)) then
               Expand_Real_Literal (This, Expr);
            else
	       null; -- Write_Uint (Ob, Expr_Value (Expr));
            end if;
	    
	 else
	    Expand_Node (This, Expr);
	 end if;
      else
	 raise Program_Error;
      end if;
   end Expand_Number_Declaration;
   
   ---------------------------------
   -- Expand_Object_Declaration --
   ---------------------------------
   
   procedure Expand_Object_Declaration 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Def_Id   : Entity_Id := Defining_Identifier (Node);
      Type_Def : Node_Id;
      Expr     : Node_Id;
--        Mark     : Node_Id;
--        Constr   : Node_Id;
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
      
      --  Here we skip access to others thans those Expandd by reflex.
      
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
	       
	       --  The expander has Expandd an access to subprogram. No need
	       --  to Expand it as glips handles them.
	    else
	       return;
	    end if;
	 end if;
      end if;
	       
      if Constant_Present (Node) then
	 null; 
      end if;
      
      Type_Def := Object_Definition (Node);
      
      if NKind_In (Type_Def, N_Expanded_Name, N_Identifier) then
	 Expand_Subtype_Mark (This, Type_Def);
	 
      elsif Nkind (Type_Def) = N_Subtype_Indication then
	 
	 Expand_Object_Subtype_Indication (This, Type_Def);
	 
      elsif Nkind (Type_Def) = N_Constrained_Array_Definition then
	 Expand_Constrained_Array_Definition (This, Type_Def, Node);
	 
      elsif NKind (Type_Def) = N_Access_Definition then
	 Expand_Access_Definition (This, Type_Def, Node);
	 
	 --  Never go here
      else
	 raise Program_Error;
      end if;
      
      --  Here if we have an expression it must be static
      
      Expr := Expression (Node);
      if Present (Expr) then
	 if Nkind (Expr) = N_Null then
	    null;
	    
	 elsif Compile_Time_Known_Value (Expr) then
	    Expand_Node (This, Expr, True);
	 else
	    Expand_Node (This, Expr, True);
	 end if;
      end if;
      
      --  if This.In_Statements then
      --  Put_Line ("10");
      --  	 Remove (Node);
      --  	 Declare_Current_Scope (This, Node);
      --  end if;
      
   exception
      when others =>
	 Put_Line ("Expand_Object_Declaration exception");
   end Expand_Object_Declaration;
   
   ------------------------------------------
   -- Expand_Object_Renaming_Declaration --
   ------------------------------------------
   
   procedure Expand_Object_Renaming_Declaration 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
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
      
      if Constant_Present (Node) then
	 null;
      end if;
      
      Type_Def := Object_Definition (Node);
      
      if NKind_In (Type_Def, N_Expanded_Name, N_Identifier) then
	 Expand_Subtype_Mark (This, Type_Def);
	 
      elsif Nkind (Type_Def) = N_Subtype_Indication then
	 
	 --  Mark := Subtype_Mark (Type_Def);
	 --  Expand_Subtype_Mark (This, Mark);
	 
	 --  Constr := Constraint (Type_Def);
	 --  if Present (Constr) then
	 --     Expand_Node (This, Constr);
	 --  end if;
	 
	 Expand_Subtype_Indication (This, Type_Def, Node);
	 
      else
	 Access_Def := Access_Definition (Node);
	 
	 if Present (Access_Def) then
	    pragma Assert (Nkind (Access_Def) = N_Access_Definition);
	    pragma Assert (No (Access_To_Subprogram_Definition (Access_Def)));
	    
	    Subtyp := Subtype_Mark (Access_Def);
	    pragma Assert (Present (Subtyp));
	    pragma Assert (Nkind_In (Subtyp, N_Expanded_Name, N_Identifier));
	    
	    Expand_Subtype_Mark (This, Subtyp);
	    
	 else
	    Error_Msg_N
	      ("reflex does not supported this definition type 3 ", Node);
	    return;
	 end if;
      end if;
      
      --  Here if we have an expression it must be static
      
      Expr := Expression (Node);
      if Present (Expr) then
	 if Compile_Time_Known_Value (Expr) then
	   Expand_Node (This, Expr);  --     Write_Uint (Expr_Value (Expr));
	 else
	    Expand_Node (This, Expr);
	 end if;
      end if;
   end Expand_Object_Renaming_Declaration;
   
   ----------------------------------------------
   -- Expand_Ordinary_Fixed_Point_Definition --
   ----------------------------------------------
   
   procedure Expand_Ordinary_Fixed_Point_Definition 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Ordinary_Fixed_Point_Definition;
   
   ------------------------------------
   -- Expand_Package_Instantiation --
   ------------------------------------
   
   procedure Expand_Package_Instantiation 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Package_Instantiation;
   
   -------------------------------------------
   -- Expand_Package_Renaming_Declaration --
   -------------------------------------------
   
   procedure Expand_Package_Renaming_Declaration 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Package_Renaming_Declaration;
   
   --------------------------------------------
   -- Expand_Private_Extension_Declaration --
   --------------------------------------------
   
   procedure Expand_Private_Extension_Declaration 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Private_Extension_Declaration;
   
   ---------------------------------------
   -- Expand_Private_Type_Declaration --
   ---------------------------------------
   
   procedure Expand_Private_Type_Declaration 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Private_Type_Declaration;
   
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
   
   ---------------------------------------
   -- Expand_Real_Range_Specification --
   ---------------------------------------
   
   procedure Expand_Real_Range_Specification 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Real_Range_Specification;
   
   ----------------------------------------------
   -- Expand_Unconstrained_Array_Declaration --
   ----------------------------------------------
   
   procedure Expand_Unconstrained_Array_Declaration
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
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
      --  types are Expandd as integer in Glips. 
      
      Expand_Unconstrained_Array_Definition (This, Type_Def, Node);    
      
   end Expand_Unconstrained_Array_Declaration;
   
   --------------------------------------------
   -- Expand_Constrained_Array_Declaration --
   --------------------------------------------
   
   procedure Expand_Constrained_Array_Declaration
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
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
      
      Expand_Constrained_Array_Definition (This, Type_Def, Node);
      
   end Expand_Constrained_Array_Declaration;
   
   ---------------------------------------------
   -- Expand_Unconstrained_Array_Definition --
   ---------------------------------------------
   
   procedure Expand_Unconstrained_Array_Definition
     (This      : access Reflex_Expander_Record;
      Node      : Node_Id;
      Decl_Node : Node_Id) is
      
      Marks : List_Id;
      Dim   : Node_Id;
      Typ   : Entity_Id;
   begin
      pragma Assert (Nkind (Node) = N_Unconstrained_Array_Definition);
      
      -- 3.6  Unconstrained Array Definition --
      -----------------------------------------

      --  UNCONSTRAINED_ARRAY_DEFINITION ::=
      --    array (INDEX_SUBTYPE_DEFINITION {, INDEX_SUBTYPE_DEFINITION}) of
      --      COMPONENT_DEFINITION

      --  Here we does not borrow about Subtype mark of index, as the discrete
      --  types are Expandd as integer in Glips. 
      
      --  For each dimension Expand "<>"
      
      Marks := Subtype_Marks (Node);
      pragma Assert (Present (Marks));
      
      Dim := First (Marks);
      loop
	 Typ := Etype (Get_Type_Full_View (Entity (Dim)));
         --  Write_Integer_Type
         --    (Ob,
	 --     UI_To_Int (Esize (Typ)),
         --     Signed => not Is_Modular_Integer_Type (Typ));
	 
	 Next (Dim);
	 exit when No (Dim);
      end loop;
      
      Expand_Component_Definition
	(This, Component_Definition (Node), Decl_Node);
   end Expand_Unconstrained_Array_Definition;
   
   -------------------------------------------
   -- Expand_Constrained_Array_Definition --
   -------------------------------------------
   
   procedure Expand_Constrained_Array_Definition
     (This      : access Reflex_Expander_Record;
      Node      : Node_Id;
      Decl_Node : Node_Id) is
      
      Indexes     : List_Id;
      Dim         : Node_Id;
      New_Indexes : List_Id;
   begin
      pragma Assert (Nkind (Node) = N_Constrained_Array_Definition);
	
      ---------------------------------------
      -- 3.6  Constrained Array Definition --
      ---------------------------------------

      --  CONSTRAINED_ARRAY_DEFINITION ::=
      --    array (DISCRETE_SUBTYPE_DEFINITION
      --      {, DISCRETE_SUBTYPE_DEFINITION})
      --        of COMPONENT_DEFINITION
      
      --  For each dimension Expand "<>"
      
      --  Loop through subscripts
      
      Indexes := Discrete_Subtype_Definitions (Node);
      Dim := First (Indexes);
      loop
	 Expand_Node (This, Dim);
	 
	 Next (Dim);
	 exit when No (Dim);
      end loop;
      
      Expand_Component_Definition
	(This, Component_Definition (Node), Decl_Node);
      
      Indexes := Discrete_Subtype_Definitions (Node);
      
      New_Indexes := Change_Array_Indexes_To_Range (This, Indexes);
      
      Set_Discrete_Subtype_Definitions (Node, New_Indexes);
      
   end Expand_Constrained_Array_Definition;
   
   --------------------------------
   -- Expand_Access_Definition --
   --------------------------------
   
   procedure Expand_Access_Definition
     (This      : access Reflex_Expander_Record;
      Node      : Node_Id; 
      Decl_Node : Node_Id) is
      
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
	    
	    Expand_Type_Name (This, Mark);
	    Expand_Node (This, Const);
	    
	    --  Expand_Subtype_Indication (This, Subtyp);
	    
	    
	 else
	    pragma Assert (Nkind_In (Subtyp, N_Expanded_Name, N_Identifier));
	 
            Expand_Type_Name (This, Base_Type (Entity (Subtyp)));
         end if;
	 
      elsif Nkind (Node) = N_Access_Definition then
	 
	 if Present (Access_To_Subprogram_Definition (Node)) then
	    Error_Msg_N
	      ("reflex does not supported access to subprogram ", Node);
	 else
	    
	    Mark := Subtype_Mark (Node);	 
	    if Present (Mark) then
	       pragma Assert (Nkind_In (Mark, N_Expanded_Name, N_Identifier));
	       
	       Expand_Type_Name (This, Mark);
	    else
	       raise Program_Error;
	    end if;
	 end if;
	    
      else
	 raise Program_Error;
      end if;
   exception
      when others =>
	 Put_Line ("Expand_Access_Definition exception");
   end Expand_Access_Definition;
   
   ------------------------------------------
   -- Expand_Access_To_Object_Definition --
   ------------------------------------------
   
   procedure Expand_Access_To_Object_Definition
     (This      : access Reflex_Expander_Record;
      Node      : Node_Id;
      Decl_Node : Node_Id) is
      
      Def_Id   : Node_id := Defining_Identifier (Node);
      Type_Def : Node_Id;
   begin
      ---------------------------------------
      -- 3.10  Access To Object Definition --
      ---------------------------------------

      --  ACCESS_TO_OBJECT_DEFINITION ::=
      --    [NULL_EXCLUSION] access [GENERAL_ACCESS_MODIFIER]
      --    SUBTYPE_INDICATION
      
      Type_Def := Type_Definition (Node);
      Expand_Access_Definition (This, Type_Def, Decl_Node);
      
   exception
      when others =>
	 Put_Line ("Expand_Access_To_Object_Definition exception");
   end Expand_Access_To_Object_Definition;
   
   --------------------------------
   -- Expand_Record_Definition --
   --------------------------------
   
   procedure Expand_Record_Definition 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Def_Id    : Entity_Id := Defining_Identifier (Node);
      Type_Def  : Node_Id;
      Comp_List : Node_Id;
      Items     : List_Id;
      Item      : Node_Id;
   begin
      --  RECORD_DEFINITION ::= 
      --    record
      --      COMPONENT_LIST
      --    end record
      --  | null record
      
      
      Type_Def := Type_Definition (Node);
      
      Expand_Component_List (This, Type_Def);
      
      if Need_Init_Record (Def_Id) then
	 Create_Initialization_Record (Node, Node);
	 
	 if Null_Present (Node) then
	    null;
	    
	 else	
	    Comp_List := Component_List (Type_Def);
	    Items     := Component_Items (Comp_List);
	    if Is_Non_Empty_List (Items) then
	       Item := Nlists.First (Items);
	       
	       while Present (Item) loop
		  if Nkind (Item) = N_Component_Declaration then
		     Expand_Node (This, Item, True);
		     Append_Component_Initialization (Item, Node);
		  end if;
		  Next (Item);
	       end loop;
	    end if;
	 end if;
      end if;
   end Expand_Record_Definition;
   
   --------------------------------------------
   -- Expand_Enumeration_Literal_Constants --
   --------------------------------------------
   
   procedure Expand_Enumeration_Literal_Constants
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Def_Id  : Node_Id;
      Lit     : Node_Id;
      Loc     : constant Source_Ptr := Sloc (Node);
      E       : Entity_Id;
      New_Lit : Node_Id;
   begin
      Def_Id := Defining_Identifier (Node);
      
      --  For each litraral emit an integer constant wihch value is the 
      --  Representation value of the literal
      
      Lit := First_Literal (Def_Id);
      while Present (Lit) loop
	 E := Make_Unique_Entity (This, Loc, Chars (Lit));
	 
	 New_Lit := Make_Object_Declaration 
	   (Sloc                => Loc,
	    Defining_Identifier => E,
	    Constant_Present    => True,
	    Object_Definition   => New_Occurrence_Of (Standard_Integer, Loc),
	    Expression          => 
	      Make_Integer_Literal (Loc, Enumeration_Rep (Lit)));
	 
	--JMA Nodes_Lists.Insert_List_Before (Node, New_Lit);
	 
         Set_Ekind (E, E_Variable);
         Set_Etype (E, Standard_Integer);
	 Set_Enumeration_Literal_Constant (Lit, E);
	 
	 Lit := Next_Literal (Lit);
      end loop;
   end Expand_Enumeration_Literal_Constants;
   
   ---------------------------------------------
   -- Expand_Signed_Integer_Type_Definition --
   ---------------------------------------------
   
   procedure Expand_Signed_Integer_Type_Definition 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      --  Nothing to do as the signed integer types are Expandd as integer. 
      --  The references to the types are replaced by integer_x where x is 
      --  the number of bits needed to holds a value of the type
      null;
   end Expand_Signed_Integer_Type_Definition;
   
   ----------------------------------------------
   -- Expand_Subprogram_Renaming_Declaration --
   ----------------------------------------------
   
   procedure Expand_Subprogram_Renaming_Declaration 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Subprogram_Renaming_Declaration;
   
   ---------------------------------------------
   -- Create_Full_Type_For_Subtype_Indication --
   ---------------------------------------------
   
   procedure Create_Full_Type_For_Subtype_Indication
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Create_Full_Type_For_Subtype_Indication;
     
   ----------------------------------
   -- Expand_Subtype_Declaration --
   ----------------------------------
   
   procedure Expand_Subtype_Declaration 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Subtyp     : Node_id;
      Indexes    : List_Id;
      Mark       : Node_Id;
      Const      : Node_Id;
      Comp_Def   : Entity_Id;
      Id         : Entity_Id := Defining_Identifier (Node);
      Full_Type  : Node_Id;
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
      
      Subtyp := Subtype_Indication (Node);
      if Nkind (Subtyp) = N_Subtype_Indication then
	 
	 Mark  := Subtype_Mark (Subtyp);
	 Const := Constraint (Subtyp);
	 
	 --  If the parent Type is an array, create an anoynous array, with
	 --  indexes taken in the constraint.
	 
	 if Is_Array_Type (Base_Type (Entity (Mark))) then
	    
	    Comp_Def := Component_Type (Base_Type (Entity (Mark)));
	    Indexes  := Change_Array_Constaint_To_Range (This, Const);
	    
	    pragma Assert (Is_Array_Type (Base_Type (Entity (Mark))));
	    pragma Assert (Nkind (Const) = N_Index_Or_Discriminant_Constraint);
	    
	    Full_Type := Make_Full_Type_Declaration
	      (Sloc                => Sloc (Node),
	       Defining_Identifier => Id,
	       Type_Definition  => 
		 Make_Constrained_Array_Definition
		 (Sloc                         => Sloc (Subtyp),
		  Discrete_Subtype_Definitions => Indexes,
		  Component_Definition         => 
		    Make_Component_Definition
		    (Sloc               => Sloc (Subtyp),
		     Subtype_Indication => 
		       New_Occurrence_Of (Comp_Def, Sloc (Subtyp)))));
	    
	    Set_Ekind (Id, E_Array_Type);
	    Set_Etype (Id, Id);
	    Set_First_Index    (Id, First (Indexes));
	    Set_Component_Type (Id, Comp_Def);
	    Set_Is_Constrained (Id, True);
	      
	    Rewrite (Node, Full_Type);
	 else
	    pragma Assert (Nkind_In (Const, N_Range, N_Range_Constraint));
	 end if;
	    
	 --  Nothing to do, as the generator replaces the subtype by its
	 --  base type
      else
	 null;
      end if;
   end Expand_Subtype_Declaration;
   
   ---------------------------------
   -- Expand_Subtype_Indication --
   ---------------------------------
   
   procedure Expand_Subtype_Indication 
     (This      : access Reflex_Expander_Record;
      Node      : Node_Id;
      Decl_Node : Node_Id) is
      
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
      
      if Is_String_Type (Base_Type (Typ)) then -- E_String_Literal_Subtype then
	 Subtype_Indication_As_String (This, Typ);
	 
      elsif Is_Array_Type (Typ) then
	 --  Subtype_Indication_As_Anonymous_Array
	 --    (This, Node, Decl_Node);
null;	 
      else
	 Expand_Subtype_Mark (This, Mark);
      end if;
      
      --  Constr := Constraint (Node);
      --  if Present (Constr) then
      --  	 Write_Char (Ob, '(');
      --  	 Expand_Node (This, Constr);
      --  	 Write_Char (Ob, ')');
      --  end if;
   end Expand_Subtype_Indication;
   
   ---------------------------
   -- Change_Index_To_Range --
   ---------------------------
   
   function Change_Index_To_Range
     (This : access Reflex_Expander_Record;
      C    : Node_Id) return Node_Id is
      
      Rng      : Node_Id;
      Rng_Low  : Node_Id;
      Rng_High : Node_Id;
      Low      : Node_Id;
      High     : Node_Id;
   begin
      Rng_Low  := Empty;
      Rng_High := Empty;
	 
      --  The constraint is done by a Subtype indication. The 
      --  constraint is a N_Range or a N_Range_Constraint and the type
      --  of the index is the Subtype_Mark of the Subtype Indication 
      --  and the bounds are given by the Low and High bounds of the
      --  range. The bound smust be static so known at compile time
      
      if Nkind_In (C, N_Identifier, N_Expanded_Name) then
	 Rng_Low  := Type_Low_Bound  (Etype (C));
	 Rng_High := Type_High_Bound (Etype (C));
	 
      elsif Nkind (C) = N_Subtype_Indication then
	    
	 Rng := Constraint (C);
	 
	 if Nkind (C) = N_Range_Constraint then
	    Rng := Range_Expression (Rng);
	    
	 elsif Nkind (C) = N_Range then
	    null;
	 else
	    raise Program_Error;
	 end if;
	 
	 Rng_Low  := Low_Bound (Rng);
	 Rng_High := High_Bound (Rng);
	 
	 --  Attribute reference, the Prefix is either a type or a 
	 --  variable. The type is the prefix if the Prefix is a Type
	 --  or the Type of the Prefix is the Prefix is not a type
	 --  The low and High bounds are the Low and High bounds of
	 --  the Type
	 
      elsif Nkind (C) = N_Attribute_Reference then
	 
	 pragma Assert (Attribute_Name (C) = Name_Range);
	 declare
	    Nam     : constant Name_Id      := Attribute_Name (C);
	    Attr_Id : constant Attribute_Id := Get_Attribute_Id (Nam);
	    Pref    : Node_Id               := Prefix ( C);
	    Epref   : Entity_Id;
	 begin
	    pragma Assert
	      (Is_Entity_Name (Pref) or else Is_Type (Entity (Pref)));
	    
	    Epref := Entity (Pref);
	    if not Is_Type (Epref) then
	       Epref := Etype (Epref);
	    end if;
	    
	    Rng_Low  := Type_Low_Bound  (Epref);
	    Rng_High := Type_High_Bound (Epref);
	 end;
	 
      elsif Nkind (C) = N_Range_Constraint then
	 Rng      := Range_Expression (C);
	 Rng_Low  := Low_Bound  (Rng);
	 Rng_High := High_Bound (Rng);
	 
      elsif Nkind (C) = N_Range then
	 Rng_Low  := Low_Bound  (C);
	 Rng_High := High_Bound (C);
	 
      else
	 null;
      end if;
      
      --  If we fall here, low and high bounds of the index must be
      --  presents
      
      pragma Assert (Present (Rng_Low) and then Present (Rng_High));
      
      --  pragma Assert (Present (Rng_Low) and then Present (Rng_High));
      
      --  The bounds must be static in order to generate them
      
      Rng := Empty;
      
      if No (Rng_Low) 
	or else not Compile_Time_Known_Value (Rng_Low) 
      then
	 Error_Msg_N
	   ("low bound of an array index must be static", C);
	 
      elsif No (Rng_High) 
	or else not Compile_Time_Known_Value (Rng_High) 
      then
	 Error_Msg_N
	   ("high bound of an array index must be static", C);
	 
	 --  Low and High bounds are known at compile time.
	 
      else
	 Low  := Make_Integer_Literal
	   (Sloc (Rng_Low), Expr_Value (Rng_Low));
	 High := Make_Integer_Literal
	   (Sloc (Rng_High), Expr_Value (Rng_High));
	 
	 Rng := Make_Range
	   (Sloc       => Sloc (C),
	    Low_Bound  => Low,
	    High_Bound => High);
      end if;
	 
      return Rng;
   end Change_Index_To_Range;
   
   -----------------------------------
   -- Change_Array_Indexes_To_Range --
   -----------------------------------
   
   function Change_Array_Indexes_To_Range
     (This    : access Reflex_Expander_Record;
      Indexes : List_Id) return List_Id is
      
      C           : Node_Id;
      Rng         : Node_Id;
      New_Indexes : List_Id;
   begin
      New_Indexes := New_List;
      C           := First (Indexes);
      while Present (C) loop
	 Rng := Change_Index_To_Range (This, C);
	 
	 if Present (Rng) then
	    Append (Rng, New_Indexes);
	 else
	    null;
	 end if;
	 
	 Next (C);
      end loop;
      
      return New_Indexes;
   end Change_Array_Indexes_To_Range;
   
   -------------------------------------
   -- Change_Array_Constaint_To_Range --
   -------------------------------------
   
   function Change_Array_Constaint_To_Range
     (This : access Reflex_Expander_Record;
      Node : Node_Id) return List_Id is
      
      C       : Node_Id;
      L       : List_Id;
      Rng     : Node_Id;
      Indexes : List_Id;
   begin
      pragma Assert (Nkind (Node) = N_Index_Or_Discriminant_Constraint);
      
      Indexes := New_List;
      L       := Constraints (Node);
      C       := First (L);
      
      while Present (C) loop
	 Rng := Change_Index_To_Range (This, C);
	 
	 if Present (Rng) then
	    Append (Rng, Indexes);
	 else
	    null;
	 end if;
	    
	 Next (C);
      end loop;
      
      return Indexes;
   end Change_Array_Constaint_To_Range;
   
   --------------------------------------
   -- Expand_Object_Subtype_Indication --
   --------------------------------------
   
   procedure Expand_Object_Subtype_Indication 
     (This    : access Reflex_Expander_Record;
      Obj_Def : Node_Id) is
      
      Comp_Def  : Entity_Id;
      Const     : Node_Id;
      Array_Def : Node_Id;
      Indexes   : List_Id;
      Mark      : Node_Id;
   begin
      Comp_Def := Component_Type (Entity (Subtype_Mark (Obj_Def)));
      Const := Constraint (Obj_Def);
      
      --  Full_Type := Make_Full_Type_Declaration
      --    (Sloc                => Sloc (Related_Nod),
      --     Defining_Identifier => 
      --       Make_Defining_Identifier
      --       (Sloc  => Sloc (Related_Nod),
      --        Chars => Chars (P)),
      --     Type_Definition  => 
      --       Make_Constrained_Array_Definition
      --       (Sloc                         => Sloc (Obj_Def),
      --        Discrete_Subtype_Definitions => New_List (Const),
      --        Component_Definition         => 
      --  	 New_Occurrence_Of (Comp_Def, Sloc (Obj_Def))));
      
      Mark  := Subtype_Mark (Obj_Def);
      Const := Constraint (Obj_Def);
      
      --  If the parent Type is an array, create an anoynous array, with
      --  indexes taken in the constraint.
      
      pragma Assert (Is_Array_Type (Base_Type (Mark)));
      pragma Assert (Nkind (Const) = N_Index_Or_Discriminant_Constraint);
      
      Indexes := Change_Array_Constaint_To_Range (This, Const);
      
      Array_Def := Make_Constrained_Array_Definition
	(Sloc                         => Sloc (Obj_Def),
	 Discrete_Subtype_Definitions => Indexes,
	 Component_Definition         => 
	   Make_Component_Definition
	   (Sloc  => Sloc (Obj_Def),
	    Subtype_Indication => 
	      New_Occurrence_Of (Comp_Def, Sloc (Obj_Def))));
      
      Rewrite (Obj_Def, Array_Def);
   end Expand_Object_Subtype_Indication;
   
   ----------------------
   -- Expand_Variant --
   ----------------------
   
   procedure Expand_Variant 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Variant;
   
   ---------------------------
   -- Expand_Variant_Part --
   ---------------------------
   
   procedure Expand_Variant_Part 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Variant_Part;
   
   -------------------------------------------
   -- Subtype_Indication_As_Anonymous_Array --
   -------------------------------------------
   
   procedure Subtype_Indication_As_Anonymous_Array_Old
     (This      : access Reflex_Expander_Record;
      Subtyp    : Node_Id;
      Decl_Node : Node_Id) is
      
      Loc          : constant Source_Ptr := Sloc (Decl_Node);
      Mark         : Entity_Id;
      Par          : Node_Id;
      Comp_Type    : Entity_Id;
      Def_Id       : Entity_Id;
      Constr_List  : List_Id;
      Constr       : Node_id;
      Type_Def     : Node_Id;
      Comp_Def     : Node_Id;
      New_Comp_Def : Node_Id;
      Scomp        : Node_Id;
      Base_Par     : Node_Id;
      Arr_Def      : Node_Id;
      Full_Type    : Node_Id;
   begin
      --  Here we have to deal with two cases. The first one is when subtype
      --  indication appears in a Full Type or Subtype declaration, and in the 
      --  case od the subtype indcation appears in a component declaration or
      --  in an object declaration.
      
      --  First case (Full Type Declaration or Subtype Declaration):
      --------------------------------------------------------------
      
      --  Create a Full Type Declaration for the subtype indication. The 
      --  Decl_Node is either a Full Type declaration which constrains an
      --  unconstrained array, or a type Subtype Declaration which constrains 
      --  an Unconstrainted array.
      
      --  In the case of a Subtype Declaration, the subtype is replaced by a
      --  Full Type Declaration of an Constrained Array definition with the
      --  Discrete Range build from the contraints of the subtype indication
      --  contraints of the older Subtype. The Component Definition is the
      --  duplicate of the Component Definition of the Unconstrained Array
      --  pointing by the subtype mark of the subtype indication of the 
      --  Decl_Node
      
      --  In the case of a Full Type Declaration, we must insert a Full Type 
      --  Declaration for the subtype indication present in the type definition
      --  of the Decl_Node. The Type_Definition of the Decl_Node is changed in
      --  an Array Type definition with its Discrete_Range build from the
      --  constraint of the subtype indication of the Decl_Node, and the 
      --  component definition is a an identifier pointing on the newly created
      --  Full Type.
      
      -- Second case (Component or Object Declaration):
      -------------------------------------------------
      
      --  Here we create an anonymous array definition. The object must be 
      --  constrained, so we can safey replace the type definition by an
      --  anonymous array
      
      Mark := Entity (Subtype_Mark (Subtyp));
      pragma Assert 
        (Ekind (Etype (Mark)) = E_Array_Type 
         or else Ekind (Etype (Mark)) = E_Array_Subtype);
      
      Par := Parent (Mark);
      
      Comp_Type := Component_Type (Base_Type (Mark));
      
      --  Duplicate Constrain
      
      Constr := Constraint (Subtyp);
      pragma Assert(Nkind (Constr) = N_Index_Or_Discriminant_Constraint);
      Constr_List := Constraints (Constr);
	 
      --  Decl_Node is a Full Type Declaration, so we have to create a new Id
      --  For the newly Full Type Declaration, insert this newly Full Type,
      --  before the Decl_Node.
      
      if Nkind (Decl_Node) = N_Full_Type_Declaration 
	or else Nkind (Decl_Node) = N_Subtype_Declaration 
      then
	 if Nkind (Decl_Node) = N_Full_Type_Declaration 
	   and then Is_Array_Type (Etype (Defining_Entity (Decl_Node)))
	 then
	    
	    Def_Id := New_Copy (Defining_Identifier (Decl_Node));
	    Set_Chars
	      (Def_Id, Name_Find (Get_Name_String (Chars (Def_Id)) & "_C"));
	    Set_Etype (Def_Id, Def_Id);
	    Set_Component_Type (Def_Id, Comp_Type);
	    Set_First_Index (Def_Id, First (Constr_List));
	 
	    --  Here is when Decl_Node is Subtype Declaration, we replace 
	    --  Decl_Node by the newly created Full Type.
	 
	 else
	 
	    --  Reuse the entity of the subtype, and change it to an 
	    --  E_Array_Type
	    
	    Def_Id := Defining_Identifier (Decl_Node);
	    Set_Ekind (Def_Id, E_Array_Type);
	    Set_Etype (Def_Id, Comp_Type);
	    Set_First_Index (Def_Id, First (Constr_List));
	    
	    --  No Entity to create or change
	 end if;
	 
      elsif Nkind (Par) = N_Component_Definition then
	 null;
      end if;
      
      Base_Par := Parent (Base_Type (Mark));
      pragma Assert (Nkind (Base_Par) = N_Full_Type_Declaration);
      
      Type_Def := Type_Definition (Base_Par);
      
      --  Here we know that Comp_Def is Subtype_Indication
      
      Comp_Def := Component_Definition (Type_Def);
      
      Scomp := Subtype_Indication (Comp_Def);
      if Present (Scomp) and then Nkind (Scomp) = N_Subtype_Indication then
	 New_Comp_Def := Subtype_Mark (Scomp);
      else
	 New_Comp_Def := New_Copy_Tree (Comp_Def);
      end if;
      Arr_Def := Make_Constrained_Array_Definition
	(Sloc => Loc,
	 Discrete_Subtype_Definitions => Constr_List,
	 Component_Definition         => New_Comp_Def);
      
      if Nkind (Par) = N_Full_Type_Declaration 
	or else Nkind (Par) = N_Subtype_Declaration
      then
	 Full_Type := Make_Full_Type_Declaration
	   (Loc,
	    Defining_Identifier => Def_Id,
	    Type_Definition     => Arr_Def);
	 
	 --  If parent is a Full Type declaration insert the new node befor the 
	 -- Node_DeclI
	 
	 if Nkind (Par) = N_Full_Type_Declaration then
	    
	    Insert_Before (Decl_Node, Full_Type);      
	    
	    --  Type of compoennt the Decl_Node becomes Full_Type
	    
	    Type_Def := Component_Definition (Decl_Node);
	    Set_Subtype_Indication (Type_Def, New_Occurrence_Of (Def_Id, Loc));
	    
	    --  If the Decl_Node is a subtype replace it with 
	    
	 else
	    Insert_Before (Decl_Node, Full_Type);      
	    Remove (Decl_Node);
	    --  Replace (Decl_Node, Full_Type);
	 end if;
	 
	 --  Component Definition case
	 
      else
	 Replace (Subtyp, Arr_Def);
      end if;
      
   exception 
      when others =>
	 Put_Line ("Subtype_Indication_As_Anonymous_Array ----> Eception");
   end Subtype_Indication_As_Anonymous_Array_Old;

   ----------------------------------
   -- Subtype_Indication_As_String --
   ----------------------------------
   
   procedure Subtype_Indication_As_String 
     (This : access Reflex_Expander_Record;
      Typ  : Entity_Id) is
      
      Low     : Node_Id;
      Low_Val : Uint;
      Len     : Uint;
   begin
      pragma Assert (Ekind (Typ) = E_String_Literal_Subtype);
      
      Low := String_Literal_Low_Bound (Typ);
      Len := String_Literal_Length (Typ);
      
      if Compile_Time_Known_Value (Low) then
	 Low_Val := Expr_Value (Low);
	 --  Write_Uint (Ob, Low_Val);
	 --  Write_Str (Ob, " .. ");
	 --  Write_Uint (Ob, UI_Add (Low_Val, Len));
      else
	 Expand_Node (This, Low);
	 Expand_Node (This, Low);
	 --  Write_Str (Ob, " + ");
	 --  Write_Uint (Ob, Len);
	 --  Write_Str (Ob, " - 1)");
      end if;
   end Subtype_Indication_As_String;
   
   -------------------------------------------
   -- Subtype_Indication_As_Anonymous_Array --
   -------------------------------------------
   
   procedure Subtype_Indication_As_Anonymous_Array 
     (This      : access Reflex_Expander_Record;
      Subtyp    : Node_Id;
      Decl_Node : Node_Id) is
      
      Loc          : constant Source_Ptr := Sloc (Decl_Node);
      Mark         : Entity_Id;
      Comp_Type    : Entity_Id;
      Def_Id       : Entity_Id;
      Constr_List  : List_Id;
      Constr       : Node_id;
      Type_Def     : Node_Id;
      Comp_Def     : Node_Id;
      New_Comp_Def : Node_Id;
      Scomp        : Node_Id;
      Btype        : Entity_Id;
      Base_Par     : Node_Id;
      Arr_Def      : Node_Id;
      Full_Type    : Node_Id;
   begin
      --  Here we have to deal with two cases. The first one is when subtype
      --  indication appears in a Full Type or Subtype declaration, and in the 
      --  case od the subtype indcation appears in a component declaration or
      --  in an object declaration.
      
      --  First case (Full Type Declaration or Subtype Declaration):
      --------------------------------------------------------------
      
      --  Create a Full Type Declaration for the subtype indication. The 
      --  Decl_Node is either a Full Type declaration which constrains an
      --  unconstrained array, or a type Subtype Declaration which constrains 
      --  an Unconstrainted array.
      
      --  In the case of a Subtype Declaration, the subtype is replaced by a
      --  Full Type Declaration of an Constrained Array definition with the
      --  Discrete Range build from the contraints of the subtype indication
      --  contraints of the older Subtype. The Component Definition is the
      --  duplicate of the Component Definition of the Unconstrained Array
      --  pointing by the subtype mark of the subtype indication of the 
      --  Decl_Node
      
      --  In the case of a Full Type Declaration, we must insert a Full Type 
      --  Declaration for the subtype indication present in the type definition
      --  of the Decl_Node. The Type_Definition of the Decl_Node is changed in
      --  an Array Type definition with its Discrete_Range build from the
      --  constraint of the subtype indication of the Decl_Node, and the 
      --  component definition is a an identifier pointing on the newly created
      --  Full Type.
      
      -- Second case (Component or Object Declaration):
      -------------------------------------------------
      
      --  Here we create an anonymous array definition. The object must be 
      --  constrained, so we can safey replace the type definition by an
      --  anonymous array
      
      Mark := Entity (Subtype_Mark (Subtyp));
      pragma Assert 
        (Ekind (Etype (Mark)) = E_Array_Type 
         or else Ekind (Etype (Mark)) = E_Array_Subtype);
      
      --  Constraints
      
      Constr := Constraint (Subtyp);
      pragma Assert(Nkind (Constr) = N_Index_Or_Discriminant_Constraint);
      Constr_List := Constraints (Constr);
      
      Btype := Base_Type (Mark);
      pragma Assert (Is_Array_Type (Btype));
      
      Base_Par := Parent (Btype);
      pragma Assert (Nkind (Base_Par) = N_Full_Type_Declaration);
      
      Type_Def := Type_Definition (Base_Par);
      
      Comp_Def := Component_Definition (Type_Def);
      
      Comp_Type := Component_Type (Btype);
      
      New_Comp_Def := New_Copy_Tree (Comp_Def, New_Sloc => Loc);
      
      Scomp := Subtype_Indication (Comp_Def);
      pragma Assert (Nkind_In (Scomp, N_Expanded_Name, N_Identifier));
	 
      Arr_Def := Make_Constrained_Array_Definition
	(Sloc => Loc,
	 Discrete_Subtype_Definitions => Constr_List,
	 Component_Definition         => New_Comp_Def);
      
      if Nkind (Decl_Node) = N_Full_Type_Declaration 
	and then Is_Array_Type (Etype (Defining_Entity (Decl_Node)))
      then
	 Def_Id := New_Copy (Defining_Identifier (Decl_Node));
	 Set_Chars
	   (Def_Id, Name_Find (Get_Name_String (Chars (Def_Id)) & "_C"));
	 Set_Ekind (Def_Id, E_Array_Type);
	 Set_Etype (Def_Id, Def_Id);
	 Set_Component_Type (Def_Id, Comp_Type);
	 Set_First_Index (Def_Id, First (Constr_List));
	 
	 Full_Type := Make_Full_Type_Declaration
	   (Loc,
	    Defining_Identifier => Def_Id,
	    Type_Definition     => Arr_Def);
	 
	 --  Insert the new node before the decl_Node
	 
	   Insert_Before (Decl_Node, Full_Type);      
	   
	   --  Type of component of Decl_Node becomes Full_Type
	   
	   Set_Component_Type (Defining_Identifier (Decl_Node), Def_Id);
	   Type_Def := Type_Definition (Decl_Node);
	   Comp_Def := Component_Definition (Type_Def);
	   Set_Subtype_Indication (Comp_Def, New_Occurrence_Of (Def_Id, Loc));
	   Set_Access_Definition (Comp_Def, Empty);
	 
      elsif  Nkind (Decl_Node) = N_Full_Type_Declaration then
	 Set_Component_Type (Defining_Identifier (Decl_Node), Comp_Type);
	 Set_Type_Definition (Decl_Node, Arr_Def);
	
      elsif Nkind (Decl_Node) = N_Subtype_Declaration then
	 --  Reuse the entity of the subtype, and change it to an 
	    --  E_Array_Type
	    
	    Def_Id := Defining_Identifier (Decl_Node);
	    Set_Ekind (Def_Id, E_Array_Type);
	    Set_Etype (Def_Id, Def_Id);
	    Set_Component_Type (Def_Id, Comp_Type);
	    Set_First_Index (Def_Id, First (Constr_List));
	    
	    Full_Type := Make_Full_Type_Declaration
	      (Loc,
	       Defining_Identifier => Def_Id,
	       Type_Definition     => Arr_Def);
	    
	    --  Insert_Before (Decl_Node, Full_Type);      
	    --  Remove (Decl_Node);
	 Replace (Decl_Node, Full_Type);
	    
      else
	 Replace (Subtyp, Arr_Def);
      end if;
      
   exception 
      when others =>
	 Put_Line ("Subtype_Indication_As_Anonymous_Array ----> Eception");
   end Subtype_Indication_As_Anonymous_Array;
   
end Reflex.Expanders.Ch3;
