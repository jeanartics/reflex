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

with Types; use Types;
with Atree; use Atree;
with Namet; use Namet;
with Stand; use Stand;
with Sinfo; use Sinfo;
with Snames; use Snames;
with Einfo; use Einfo;
with Sem_Util; use Sem_Util;
with Sem_Aux; use Sem_Aux;
with Sem_Eval; use Sem_Eval;
with Uintp; use Uintp;
with Urealp; use Urealp;

with Reflex.Infos; use Reflex.Infos;
with Reflex.Gen.Outputs; use Reflex.Gen.Outputs;
with Reflex.Gen.Utils; use Reflex.Gen.Utils;

package body Unity.Gen.Ch2 is
   
   -----------------------------------------
   -- Generate_Defining_Character_Literal --
   -----------------------------------------
   
   procedure Generate_Defining_Character_Literal
     (This : access Unity_Generator_Record;
      Node : Node_id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      --  For enumeration literals of enumeration types that have a
      --  representation clause use directly their value.

      if Ekind (Node) = E_Enumeration_Literal
	and then Has_Enumeration_Rep_Clause (Get_Full_View (Etype (Node)))
      then
	 Write_Uint (Ob, Enumeration_Rep (Node));
      else
	 Write_Name (Ob, Chars (Ultimate_Alias (Node)));
      end if;
   end Generate_Defining_Character_Literal;
   
   ----------------------------------
   -- Generate_Defining_Identifier --
   ----------------------------------
   
   procedure Generate_Defining_Identifier 
     (This        : access Unity_Generator_Record;
      Node        : Node_Id;
      Declaration : Boolean := False) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      --  Replace constant references by the direct values, to avoid
      --  a level of indirection for e.g. private values, and since
      --  we are not trying to generate human readable code, losing
      --  the reference to the constant object is not a problem. In
      --  addition, this allows generation of static values and static
      --  aggregates.

      if Ekind (Node) = E_Constant
	and then not Declaration
	and then Is_Scalar_Type (Get_Full_View (Etype (Node)))
      then
	 declare
	    N    : constant Node_Id := Get_Type_Full_View (Node);
	    Decl : constant Node_Id := Declaration_Node (N);
	    Expr : Node_Id := Empty;

	 begin
	    if Nkind (Decl) /= N_Object_Renaming_Declaration then
	       Expr := Expression (Decl);
	    end if;

	    if Present (Expr)
	      and then Nkind_In (Expr, N_Character_Literal,
				 N_Expanded_Name,
				 N_Integer_Literal,
				 N_Real_Literal)
	    then

	       --  Add a cast to System.Address to avoid mismatch between
	       --  integer and pointer.

--  	       if Is_Descendant_Of_Address (Etype (N)) then
--  		  Write_Str (Ob, "(system__address)");
--  	       end if;

	       Generate_Node (This, Expr);

	    elsif Present (Expr)
	      and then Nkind (Expr) = N_Identifier
	      and then Ekind (Entity (Expr)) = E_Enumeration_Literal
	    then
	       Write_Uint (Ob, Enumeration_Rep (Entity (Expr)));
	    else
	       Write_Id (Ob, N);
	    end if;
	 end;

      --  elsif Is_Formal (Node)
      --  	and then Is_Unconstrained_Array_Type (Etype (Node))
      --  	and then Present (Activation_Record_Component (Node))
      --  	and then Present (Current_Subp_Entity)
      --  	and then not Within_Scope (Node, Current_Subp_Entity)
      --  then
      --  	 Write_Up_Level_Formal_Reference
      --  	   (This,
      --  	    Subp   => Current_Subp_Entity,
      --  	    Formal => Node);

	 --  For enumeration literals defined in the enclosing scope of a
	 --  nested subprogram we directly generate their values. Thus, we
	 --  avoid the need to duplicate the declaration of the enum in the
	 --  enclosing subprograms.

      elsif Is_Enum_Literal_Of_Enclosing_Subprogram (Node) then
	 Write_Uint (Ob, Enumeration_Rep (Node));

	 --  For enumeration literals of enumeration types that have a
	 --  representation clause use directly their value.

      elsif Ekind (Node) = E_Enumeration_Literal
	and then Has_Or_Inherits_Enum_Rep_Clause (Etype (Node))
      then
	 Write_Uint (Ob, Enumeration_Rep (Node));

      else
	 Write_Id (Ob, Node);
      end if;
   end Generate_Defining_Identifier;
   
   ---------------------------------------
   -- Generate_Defining_Operator_Symbol --
   ---------------------------------------
   
   procedure Generate_Defining_Operator_Symbol
     (This : access Unity_Generator_Record;
      Node : Node_id) is
   begin
      null;
   end Generate_Defining_Operator_Symbol;
   
   ----------------------------
   -- Generate_Expanded_Name --
   ----------------------------
   
   procedure Generate_Expanded_Name
     (This : access Unity_Generator_Record;
      Node : Node_id) is
   begin
      Write_Id (This.Get_Output_Buffer, Node);
   end Generate_Expanded_Name;
   
   -------------------------
   -- Generate_Identifier --
   -------------------------
   
   procedure Generate_Identifier
     (This : access Unity_Generator_Record;
      Node : Node_id) is
      
      Ob      : Output_Buffer := This.Get_Output_Buffer;
      E       : Entity_Id;
      Te      : Entity_Id;
      Orig_E  : Entity_Id;
      Orig_Te : Entity_Id;
   begin
      E  := Entity (Node);
      
      Te := Get_Type_Full_View (Scope (E));
      
      if Ekind (E) = E_Enumeration_Literal then
	 if Present (Get_Enumeration_Literal_Constant (E)) then
	    Write_Id (Ob, Get_Enumeration_Literal_Constant (E));
	    
	 elsif Has_Or_Inherits_Enum_Rep_Clause (Etype (Node)) then
	    Write_Uint (Ob, Enumeration_Rep (Node));

	 else
	    Write_Id (Ob, Node);
	 end if;
	 
	 return;
	 
      elsif Is_Numeric_Type (Etype (E)) then
	 if Compile_Time_Known_Value (E) then
	    Write_Uint (Ob, Expr_Value (E));
	    return;
	 end if;
      end if;
      
      if Ekind (E) = E_Component and then Is_Tagged_Type (Te) then
	 
	 Orig_E  := Original_Record_Component (E);
	 Orig_Te := Get_Type_Full_View (Scope (Orig_E));
	 
	 while Present (Te) and then Te /= Orig_Te loop
	    Write_Str (Ob, "_parent.");
	    Te := Get_Type_Full_View (Etype (Base_Type (Te)));
	 end loop;
      end if;
      
      Write_Id (Ob, Node);
      --  if Is_Extra_Access_Type (E) 
      --  	or else Is_Access_Type (Etype (E)) 
      --  then
      --  	 Write_Char (Ob, '^');
      --  end if;
      
   end Generate_Identifier;
   
   ---------------------------------
   -- Generate_Selected_Component --
   ---------------------------------
   
   procedure Generate_Selected_Component
     (This : access Unity_Generator_Record;
      Node : Node_id) is
      
      Ob    : Output_Buffer := This.Get_Output_Buffer;
      Tpref : Node_Id;
   begin
      Generate_Node (This, Prefix (Node));
	 
      Tpref := Etype (Prefix (Node));
      if Is_Access_Type (Tpref) then
      	 Write_Char (Ob, '^');
      end if;
      
      Write_Char (Ob, '.');
      
      Generate_Node (This, Selector_Name (Node));
   end Generate_Selected_Component;
   
   ------------------------
   -- Generate_Type_Name --
   ------------------------

   procedure Generate_Type_Name
     (This : access Unity_Generator_Record;
      Typ  : Entity_Id) is
      
      Ob       : Output_Buffer := This.Get_Output_Buffer;
      Subtyp   : Node_Id;
      Par      : Node_Id;
      Type_Def : Node_Id;
   begin
      Put_Line ("Generate_Type_Name Begin " & Get_String (Chars (Typ)));
      --  Discrete types
      
      if Chars (Typ) = Name_Tag then
	 Write_Str (Ob, "Int");
	 
      elsif Typ = Standard_Boolean then
	 Write_Str (Ob, "Bool");
	 
      elsif Typ = Standard_Character then
	 Write_Str (Ob, "Char");
	 
      elsif Typ = Standard_Short_Float
	or else Typ = Standard_Float
	or else Typ = Standard_Long_Float
	or else Typ = Standard_Long_Long_Float
      then
	 Write_Str (Ob, "Real");
	 
      elsif Is_Discrete_Type (Typ) then
	 if Ekind (Typ) = E_Enumeration_Type
	   or else EKind (Typ) = E_Enumeration_Subtype
	 then
	    Write_Str (Ob, "Int");
	 else
	    Write_Integer_Type
	      (Ob, 
	       UI_To_Int (Esize (Typ)),
	       Signed => not Is_Modular_Integer_Type (Typ));
	 end if;
	 --  Print the type name
	 
      else
	 Put_Line ("Type_Name " & Get_String (Chars (Typ)));
         Write_Name (Ob, Chars (Typ));
      end if;
      
      Put_Line ("Generate_Type_Name End");
   end Generate_Type_Name;
   
end Unity.Gen.Ch2;
