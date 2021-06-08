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

with Ada.Text_IO; use Ada.Text_IO;

with Atree;    use Atree;
--with Checks;   use Checks;
with Csets;    use Csets;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
--with Exp_Tss;  use Exp_Tss;
--with Exp_Unst; use Exp_Unst;
--with Exp_Util; use Exp_Util;
with Lib;      use Lib;
with Nmake;    use Nmake;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Osint;    use Osint;
with Osint.C;  use Osint.C;
with Output;   use Output;
with Restrict; use Restrict;
with Rident;   use Rident;
with Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Eval; use Sem_Eval;
with Sem_Mech; use Sem_Mech;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Table;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Types;    use Types;
with Uintp;    use Uintp;
with Urealp;   use Urealp;
with Gnat.HTable; use Gnat.HTable;

with Reflex.Expanders.Types; use Reflex.Expanders.Types;
--with Reflex.Expanders.Dispatch; use Reflex.Expanders.Dispatch;
with Reflex.Formats; use Reflex.Formats;
with Reflex.External_Names; use Reflex.External_Names;
with Reflex.Expanders.Ch5;

with Unity_Standard_Lib; use Unity_Standard_Lib;


package body Reflex.Expanders.Utils is
   
   FLCache_N  : Node_Id := Empty;
   --  Cache for First_Line and Last_Line (N records last node for which any
   --  of these subprograms were called, FL and LL record the corresponding
   --  First and Last physical line numbers for this node).
   
   Ghost_Count : Natural := 0;
   
   -----------------------------
   -- Append_Subpogram_Prefix --
   -----------------------------

   procedure Append_Subprogram_Prefix (Spec : Node_Id) is
      function Name_String (Name : Name_Id) return String;
      --  Returns the name string associated with Name

      function New_Name_Id (Name : String) return Name_Id;
      --  Returns a Name_Id corresponding to the given name string

      -----------------
      -- Name_String --
      -----------------

      function Name_String (Name : Name_Id) return String is
      begin
         pragma Assert (Name /= No_Name);
         return Get_Name_String (Name);
      end Name_String;

      -----------------
      -- New_Name_Id --
      -----------------

      function New_Name_Id (Name : String) return Name_Id is
      begin
         for J in 1 .. Name'Length loop
            Name_Buffer (J) := Name (Name'First + (J - 1));
         end loop;

         Name_Len := Name'Length;
         return Name_Find;
      end New_Name_Id;

      --  Local variables

      Subp : constant Entity_Id := Unique_Defining_Entity (Spec);

   --  Start of processing for Append_Subprogram_Prefix

   begin
      if Is_Compilation_Unit (Subp) then
         declare
            Prefix    : constant String := "_ada_";
            Subp_Name : Name_Id := Chars (Subp);
            Subp_Str  : constant String := Name_String (Subp_Name);

         begin
            --  Do not append the prefix if already done as part of processing
            --  its declaration.

            if Subp_Str'Length <= Prefix'Length
              or else
                Subp_Str (Subp_Str'First ..
                          Subp_Str'First + Prefix'Length - 1) /= Prefix
            then
               Subp_Name := New_Name_Id ("_ada_" & Name_String (Subp_Name));
               Set_Chars (Subp, Subp_Name);
            end if;
         end;
      end if;
   end Append_Subprogram_Prefix;

   -----------------------------------
   -- Compound_Statement_Compatible --
   -----------------------------------

   function Compound_Statement_Compatible (L : List_Id) return Boolean is
      Result : Boolean := True;

      function Search_Complex_Node (Node : Node_Id) return Traverse_Result;
      --  Subtree visitor that looks for nodes incompatible with compound
      --  statements.

      -------------------------
      -- Search_Complex_Node --
      -------------------------

      function Search_Complex_Node (Node : Node_Id) return Traverse_Result is
      begin
         case Nkind (Node) is
            when N_Declaration | N_Statement_Other_Than_Procedure_Call =>
               if not Nkind_In (Node, N_Null_Statement, N_If_Statement) then
                  Result := False;
                  return Abandon;
               end if;

            when others =>
               return OK;
         end case;

         return OK;
      end Search_Complex_Node;

      procedure Search is new Traverse_Proc (Search_Complex_Node);
      --  Subtree visitor instantiation

      --  Local variables

      N : Node_Id;

   --  Start of processing for Compound_Statement_Compatible

   begin
      if Is_Non_Empty_List (L) then
         N := First (L);

         loop
            Search (N);
            Next (N);
            exit when N = Empty;
         end loop;
      end if;

      return Result;
   end Compound_Statement_Compatible;

   ------------------------
   -- Get_Type_Full_View --
   ------------------------

   function Get_Type_Full_View (Id : Entity_Id) return Entity_Id is
   begin
      if Id /= Standard_Void_Type
        and then (Is_Type (Id) or else Ekind (Id) = E_Constant)
        and then Present (Full_View (Id))
      then
         return Full_View (Id);
      else
         return Id;
      end if;
   end Get_Type_Full_View;

   -----------------------------
   -- Has_Non_Null_Statements --
   -----------------------------

   function Has_Non_Null_Statements (L : List_Id) return Boolean is
      
      Node : Node_Id;
   begin
      if Is_Non_Empty_List (L) then
         Node := First (L);

         loop
            if Nkind (Node) /= N_Null_Statement then
               return True;
            end if;

            Next (Node);
            exit when Node = Empty;
         end loop;
      end if;

      return False;
   end Has_Non_Null_Statements;

   -------------------------------------
   -- Has_Or_Inherits_Enum_Rep_Clause --
   -------------------------------------

   function Has_Or_Inherits_Enum_Rep_Clause (E : Entity_Id) return Boolean is
      
      Typ    : Entity_Id := Get_Type_Full_View (E);
      Result : Boolean   := Has_Enumeration_Rep_Clause (Typ);
   begin
      while Get_Type_Full_View (Etype (Typ)) /= Typ loop
         Typ    := Get_Type_Full_View (Etype (Typ));
         Result := Result or Has_Enumeration_Rep_Clause (Typ);
      end loop;

      return Result;
   end Has_Or_Inherits_Enum_Rep_Clause;

   ------------------------
   -- Has_Same_Int_Value --
   ------------------------

   function Has_Same_Int_Value
     (Val1 : Node_Id;
      Val2 : Node_Id) return Boolean
   is
   begin
      return Compile_Time_Known_Value (Val1)
        and then Compile_Time_Known_Value (Val2)
        and then Expr_Value (Val1) = Expr_Value (Val2);
   end Has_Same_Int_Value;

   ----------
   -- Hash --
   ----------

   function Hash (N : Node_Id) return Header_Num is
   begin
      return Header_Num (1 + N mod Node_Id (Header_Num'Last));
   end Hash;

   ----------------------
   -- In_Instantiation --
   ----------------------

   function In_Instantiation (S : Source_Ptr) return Boolean is
      SI : constant Source_File_Index := Get_Source_File_Index (S);
   begin
      return Instantiation (SI) /= No_Location;
   end In_Instantiation;

   ---------------------------------------------
   -- Is_Enum_Literal_Of_Enclosing_Subprogram --
   ---------------------------------------------

   function Is_Enum_Literal_Of_Enclosing_Subprogram
     (E : Entity_Id) return Boolean
   is
   begin
      return Ekind (E) = E_Enumeration_Literal
        and then not Is_Library_Level_Entity (E)
        and then Present (Current_Subp_Entity)
        and then not Within_Scope (E, Current_Subp_Entity);
   end Is_Enum_Literal_Of_Enclosing_Subprogram;

   -------------------------------
   -- Is_Out_Mode_Access_Formal --
   -------------------------------

   function Is_Out_Mode_Access_Formal (E : Node_Id) return Boolean is
   begin
      return Is_Formal (E)
        and then Is_Access_Type (Etype (E))
        and then Ekind_In (E, E_In_Out_Parameter, E_Out_Parameter);
   end Is_Out_Mode_Access_Formal;

   ---------------------
   -- Is_Packed_Array --
   ---------------------

   function Is_Packed_Array (Typ : Entity_Id) return Boolean is
   begin
      return False;
---      Is_Array_Type (Typ)
--          and then Present (Packed_Array_Impl_Type (Typ));
   end Is_Packed_Array;

   ---------------------------------------
   -- Is_Supported_Variable_Size_Record --
   ---------------------------------------

   function Is_Supported_Variable_Size_Record
     (Typ : Entity_Id) return Boolean
   is
   begin
      return False;
   end Is_Supported_Variable_Size_Record;

   ----------------
   -- Last_Field --
   ----------------

   function Last_Field (Typ : Node_Id) return Node_Id is
      Field  : Node_Id := First_Entity (Typ);
      Result : Node_Id := Empty;

   begin
      while Present (Field) loop
         if Ekind (Field) in Object_Kind then
            Result := Field;
         end if;

         Next_Entity (Field);
      end loop;

      return Result;
   end Last_Field;

   ------------------
   -- Pass_Pointer --
   ------------------

   function Pass_Pointer (Ent : Entity_Id) return Boolean is
      Typ : constant Entity_Id := Get_Type_Full_View (Etype (Ent));
   begin
      if Is_Array_Type (Typ) then
         return False;

      --  elsif Ekind_In (Ent, E_In_Out_Parameter, E_Out_Parameter) then
      --     return True;

      --  Pass "flexible arrays" (arrays whose size is determined by a
      --  discriminant) by reference.

      else
         return Mechanism (Ent) = By_Reference;
      end if;
   end Pass_Pointer;

   -------------------------
   -- Ultimate_Expression --
   -------------------------

   function Ultimate_Expression (N : Node_Id) return Node_Id is
      Expr : Node_Id := N;

   begin
      while Nkind_In (Expr, N_Qualified_Expression,
                            N_Type_Conversion,
                            N_Unchecked_Type_Conversion)
      loop
         Expr := Expression (Expr);
      end loop;

      return Expr;
   end Ultimate_Expression;

   ---------------------
   -- Register_Entity --
   ---------------------

   procedure Register_Entity (E : Entity_Id) is
   begin
      Entity_Table.Set (E, True);
      Enclosing_Subp_Table.Set (E, Current_Subp_Entity);
   end Register_Entity;

   -------------------------------
   -- Register_Entity_With_Name --
   -------------------------------

   procedure Register_Entity_With_Name (E : Entity_Id) is
      
      Original_Name : Name_Id;
      Name          : Name_Id;
      Count         : Nat := 0;
   begin
      if Is_Library_Level_Entity (E) then
	 
	 Original_Name := Chars (E);
	 Name          := Original_Name;
	 
	 --  Verify that name is not already used
	 
	 while Registered_Names_Table.Get (Name) /= Empty loop
	    Name_Len := 0;
	    Append (Global_Name_Buffer, Original_Name);
	    Append (Global_Name_Buffer, '_');
	    Append (Global_Name_Buffer, Count);
	    Count := Count + 1;
	    
	    Name := Name_Find;
	 end loop;
      
	 --  Register Enity name
	 
	 Registered_Names_Table.Set (Name, E);
	 Entity_Names_Table.Set (E, Name);
      end if;
      
      Entity_Table.Set (E, True);
      Enclosing_Subp_Table.Set (E, Current_Subp_Entity);
   end Register_Entity_With_Name;

   ----------------------
   -- Requires_Address --
   ----------------------

   function Requires_Address (Typ : Node_Id) return Boolean is
   begin
      return False;
 --       not Is_Array_Type (Typ)
--            or else (Is_Packed_Array (Typ)
--                      and then Is_Integer_Type (Packed_Array_Impl_Type (Typ)));
   end Requires_Address;

   -----------------------------
   -- Unimplemented_Attribute --
   -----------------------------

   procedure Unimplemented_Attribute
     (N       : Node_Id;
      Attr    : Name_Id;
      Context : String := "")
   is
      Name : constant String := Get_Name_String (Attr);
   begin
      Error_Msg_Name_1 := Attr;

      if Context = "" then
         Error_Msg_N ("unsupported attribute%", N);
      else
--           Error_Msg_Strlen := Context'Length;
--           Error_Msg_String (1 .. Error_Msg_Strlen) := Context;
         Error_Msg_N ("unsupported attribute% in this context (~)", N);
      end if;

   end Unimplemented_Attribute;
   
   -----------------
   -- Unqual_Conv --
   -----------------

   function Unqual_Conv (Expr : Node_Id) return Node_Id is
   begin
      --  Recurse to handle unlikely case of multiple levels of qualification
      --  and/or conversion.

      if Nkind_In (Expr, N_Qualified_Expression,
                         N_Type_Conversion,
                         N_Unchecked_Type_Conversion)
      then
         return Unqual_Conv (Expression (Expr));

      --  Normal case, not a qualified expression

      else
         return Expr;
      end if;
   end Unqual_Conv;
   
   ---------------------------
   -- Search_Insertion_Node --
   ---------------------------
   
   function Search_Insertion_Node (Node : Node_Id) return Node_Id is
   begin
      if No (Node) then
	 return Empty;
      end if;
      
      if Nkind (Node) in N_Statement_Other_Than_Procedure_Call
	or else Nkind (Node) = N_Procedure_Call_Statement
      then
	 return Node;
	 
      else
	 return Search_Insertion_Node (Parent (Node));
      end if;
   end Search_Insertion_Node;
   
   ------------------------------
   -- Enclosing_Package_Entity --
   ------------------------------

   function Enclosing_Package_Entity (N : Node_Id) return Entity_Id is
      
      Par : Node_Id;
   begin
      if No (N) then
	 return Empty;
      else
	 if Nkind (N) = N_Package_Body
	   or else Nkind (N) = N_Package_Declaration
	 then
	    return Unique_Defining_Entity (N);
	 end if;
      end if;
      
      Par := Parent (N);
      
      if Present (Par) then
	 if Nkind (Par) = N_Package_Body
	   or else Nkind (Par) = N_Package_Declaration
	 then
	    return Unique_Defining_Entity (Par);
	 else
	    return Enclosing_Package_Entity (Par);
	 end if;
      else
	 return Empty;
      end if;
   end Enclosing_Package_Entity;
   
   -------------------------------
   -- Enclosing_Subprogram_Body --
   -------------------------------

   function Enclosing_Subprogram_Body (N : Node_Id) return Node_Id is
      
      Par : Node_Id;
   begin
      if No (N) then
	 return Empty;
      else
	 if Nkind (N) = N_Subprogram_Body then
	    return N;
	 end if;
      end if;
      
      Par := Parent (N);
      
      if Present (Par) then
	 if Nkind (Par) = N_Subprogram_Body then
	    return Par;
	 else
	    return Enclosing_Subprogram_Body (Par);
	 end if;
      else
	 return Empty;
      end if;
   end Enclosing_Subprogram_Body;
   
   --------------------------------------------
   -- Enclosing_Subprogram_Body_Declarations --
   --------------------------------------------

   function Enclosing_Subprogram_Body_Declarations
     (N : Node_Id) return List_Id is
      
      Subp_Body : Node_Id;
   begin
      Subp_Body := Enclosing_Subprogram_Body (N);
      
      if Present (Subp_Body) then
	return Declarations (Subp_Body);
      else
	 return No_List;
      end if;
   end Enclosing_Subprogram_Body_Declarations;
   
   ----------------------------------
   -- Declare_Enclosing_Subprogram --
   ----------------------------------
   
   procedure Declare_Enclosing_Subprogram 
     (Node : Node_Id;
      Decl : Node_Id)
   is
      Subp       : Node_Id;
      Subp_Decls : List_Id;
   begin
      Subp := Enclosing_Subprogram_Body (Node);
      
      if Present (Subp) then
	 Subp_Decls := Declarations (Subp);
	 
	 if Is_Empty_List (Subp_Decls) then
	    Subp_Decls := New_List;
	    Set_Declarations (Subp, Subp_Decls);
	 end if;
	 
	 Append (Decl, Subp_Decls);
      else
	 raise Program_Error;
      end if;
   end Declare_Enclosing_Subprogram;
   
   ----------------------
   -- New_Ghost_Entity --
   ----------------------

   function New_Ghost_Entity
     (Kind       : Entity_Kind;
      Sloc_Value : Source_Ptr) return Entity_Id
   is
      E : Entity_Id;
   begin
      Ghost_Count := Ghost_Count + 1;
      
      E := Make_Defining_Identifier
	(Sloc  => Sloc_Value,
	 Chars => String_Find
	   ("_ghost_entity_" & Integer_To_String (Ghost_Count)));
      
      Set_Ekind       (E, Kind);
      Set_Is_Internal (E, True);
      
      --  Scope is irelevant as we want Entity to be ghost
      
      --  Do not append entity to scope, as this entity must not be generated
      --  Append_Entity      (N, Scope_Id);
      
      return E;
   end New_Ghost_Entity;
   
   ------------------------------------
   -- Build_Ghost_Access_Object_Type --
   ------------------------------------
   
   function Build_Ghost_Access_Object_Type
     (DT : Entity_Id;
      N  : Node_Id) return Entity_Id is
      
      Typ : constant Entity_Id :=
	New_Ghost_Entity (E_Access_Attribute_Type, Sloc (DT));
   begin
      Set_Etype                     (Typ, Typ);
      Set_Is_Itype                  (Typ);
      Set_Associated_Node_For_Itype (Typ, N);
      Set_Directly_Designated_Type  (Typ, DT);
      return Typ;
   end Build_Ghost_Access_Object_Type;
   
   ------------------------------
   -- Create_Expanded_Variable --
   ------------------------------
   
   function Create_Expanded_Variable
     (Node : Node_Id;
      Typ  : Entity_Id;
      Name : Name_Id) return Node_Id is
      
      Obj : Node_Id;
      Id  : Entity_Id;
   begin
      Put_Line ("Create_Expanded_Variable Begin");
      Id := Make_Defining_Identifier
	(Sloc  => Sloc (Node),
	 Chars => New_Variable_Name (Name));
      
      Set_Ekind (Id, E_Variable);
      Set_Etype (Id, Typ);
      
      Put_Line (" Id = " & Get_Name_String (Chars (Id)));
      
      Obj := Make_Object_Declaration
	(Sloc                 => Sloc (Node),
	 Defining_Identifier  => Id,
	 Object_Definition    =>
	   New_Occurrence_Of (Typ, Sloc (Node)));
      
      Put_Line ("Create_Expanded_Variable End");
      return Obj;
   end Create_Expanded_Variable;
   
   ----------------------------------
   -- Declare_Enclosing_Subprogram --
   ----------------------------------
   
   procedure Declare_Body_Scope
     (Node : Node_Id;
      Decl : Node_Id)
   is
      Scp   : Node_Id := Node;
      Decls : List_Id;
      E     : Entity_Id;
   begin
      Put_Line ("Declare_Body_Scope Begin");
      Scp := Node;
      while Present (Scp) loop
	 if Nkind (Scp) = N_Block_Statement then
	    E := Entity (Identifier (Scp));
	    Decls := Declarations (Scp);
	    exit;
	    
	 elsif Nkind (Scp) = N_Subprogram_Body then
	    E := Unique_Defining_Entity (Specification (Scp));
	    exit;
	 end if;
	 Scp := Parent (Scp);
      end loop;
      
      Decls := Declarations (Scp);
      if Is_Empty_List (Decls) then
	 Decls := New_List;
	 Set_Declarations (Scp, Decls);
      end if;
	 
      Append (Decl, Decls);
      
      Put_Line ("Id " & Get_String (Chars (Defining_Identifier (Decl))));
      Put_Line ("Scp Entity " & Get_String (Chars (E)));
      Set_Scope (Defining_Identifier (Decl), E);
      Append_Entity (Defining_Identifier (Decl), E);
      
      Put_Line ("Declare_Body_Scope End");
   end Declare_Body_Scope;
   
   ---------------------------------
   -- Expression_Side_Effect_Free --
   ---------------------------------

   function Expression_Side_Effect_Free (N : Node_Id) return Boolean is
      
      function List_Side_Effect_Free (L : List_Id) return Boolean;
      
      ---------------------------
      -- List_Side_Effect_Free --
      ---------------------------
      
      function List_Side_Effect_Free (L : List_Id) return Boolean is
         Expr : Node_Id;
      begin
         if Is_Non_Empty_List (L) then
            Expr := First (L);
            while Present (Expr) loop
               if not Expression_Side_Effect_Free (Expr) then
                  return False;
               end if;
               Next (Expr);
            end loop;
         end if;
         
         return True;
      end List_Side_Effect_Free;
      
   begin
      --  Entity names are Side Effect free

      if Is_Entity_Name (N) then
	 return True;
	 
	 --  A value known at compile time is always side effect free

      elsif Compile_Time_Known_Value (N) then
	 return True;
      end if;

      --  For other than entity names and compile time known values,
      --  check the node kind for special processing.

      case Nkind (N) is

	 --  An attribute reference is side effect free if its expressions
	 --  are side effect free and its prefix is side effect free or
	 --  is an entity reference.

	 --  Is this right? what about x'first where x is a variable???

	 when N_Attribute_Reference =>
	    return List_Side_Effect_Free (Expressions (N))
	      and then (Is_Entity_Name (Prefix (N))
			  or else Expression_Side_Effect_Free (Prefix (N)));

            --  A binary operator is side effect free if and both operands
            --  are side effect free. For this purpose binary operators
            --  include membership tests and short circuit forms

	 when N_Binary_Op |
	   N_In        |
	   N_Not_In    |
	   N_And_Then  |
	   N_Or_Else
	   =>
	    return Expression_Side_Effect_Free (Left_Opnd  (N))
	      and then Expression_Side_Effect_Free (Right_Opnd (N));

            --  An explicit dereference is side effect free only if it is
            --  a side effect free prefixed reference.

	 when N_Explicit_Dereference =>
	    return True;

            --  A call to _rep_to_pos is side effect free, since we generate
            --  this pure function call ourselves. Moreover it is critically
            --  important to make this exception, since otherwise we can
            --  have discriminants in array components which don't look
            --  side effect free in the case of an array whose index type
            --  is an enumeration type with an enumeration rep clause.

            --  All other function calls are not side effect free

	 when N_Function_Call =>
	    return False;

            --  An indexed component is side effect free if it is a side
            --  effect free prefixed reference and all the indexing
            --  expressions are side effect free.

	 when N_Indexed_Component =>
	    return List_Side_Effect_Free (Expressions (N));

            --  A type qualification is side effect free if the expression
            --  is side effect free.

	 when N_Qualified_Expression =>
	    return Expression_Side_Effect_Free (Expression (N));

            --  A selected component is side effect free only if it is a
            --  side effect free prefixed reference.

	 when N_Selected_Component =>
	    return True;

            --  A range is side effect free if the bounds are side effect free

	 when N_Range =>
	    return Expression_Side_Effect_Free (Low_Bound (N))
	      and then Expression_Side_Effect_Free (High_Bound (N));

            --  A slice is side effect free if it is a side effect free
            --  prefixed reference and the bounds are side effect free.

	 when N_Slice =>
	    return Expression_Side_Effect_Free (Discrete_Range (N));

            --  A type conversion is side effect free if the expression
            --  to be converted is side effect free.

	 when N_Type_Conversion =>
	    return Expression_Side_Effect_Free (Expression (N));

            --  A unary operator is side effect free if the operand
            --  is side effect free.

	 when N_Unary_Op =>
	    return Expression_Side_Effect_Free (Right_Opnd (N));

            --  An unchecked type conversion is side effect free only if it
            --  is safe and its argument is side effect free.

	 when N_Unchecked_Type_Conversion =>
	    return Expression_Side_Effect_Free (Expression (N));

            --  An unchecked expression is side effect free if its expression
            --  is side effect free.

	 when N_Unchecked_Expression =>
	    return Expression_Side_Effect_Free (Expression (N));

            --  We consider that anything else has side effects. This is a bit
            --  crude, but we are pretty close for most common cases, and we
            --  are certainly correct (i.e. we never return True when the
            --  answer should be False).

	 when others =>
	    return False;
      end case;
   end Expression_Side_Effect_Free;

   -----------------------------------
   -- Remove_Expression_Side_Effect --
   -----------------------------------

   procedure Remove_Expression_Side_Effect (N : Node_Id) is
      
      procedure Remove_List_Side_Effect (L : List_Id);
      
      -----------------------------
      -- Remove_List_Side_Effect --
      -----------------------------
      
      procedure Remove_List_Side_Effect (L : List_Id) is
         Expr : Node_Id;
	 Nxt  : Node_Id;
      begin
         if Is_Non_Empty_List (L) then
            Expr := First (L);
            while Present (Expr) loop
	       Nxt := Next (Expr);
               Remove_Expression_Side_Effect (Expr);
               Expr := Nxt;
            end loop;
         end if;
      end Remove_List_Side_Effect;
      
   begin
      --  Entity names are Side Effect free

      if Is_Entity_Name (N) then
	 null;
	 
	 --  A value known at compile time is always side effect free

      elsif Compile_Time_Known_Value (N) then
	 null;
      end if;

      --  For other than entity names and compile time known values,
      --  check the node kind for special processing.

      case Nkind (N) is

	 --  An attribute reference is side effect free if its expressions
	 --  are side effect free and its prefix is side effect free or
	 --  is an entity reference.

	 --  Is this right? what about x'first where x is a variable???

	 when N_Attribute_Reference =>
	    Remove_List_Side_Effect (Expressions (N));
	    Remove_Expression_Side_Effect (Prefix (N));

            --  A binary operator is side effect free if and both operands
            --  are side effect free. For this purpose binary operators
            --  include membership tests and short circuit forms

	 when N_Binary_Op |
	   N_In        |
	   N_Not_In    |
	   N_And_Then  |
	   N_Or_Else
	   =>
	    Remove_Expression_Side_Effect (Left_Opnd  (N));
	    Remove_Expression_Side_Effect (Right_Opnd (N));

            --  An explicit dereference is side effect free only if it is
            --  a side effect free prefixed reference.

	 when N_Explicit_Dereference =>
	    null;

            --  A call to _rep_to_pos is side effect free, since we generate
            --  this pure function call ourselves. Moreover it is critically
            --  important to make this exception, since otherwise we can
            --  have discriminants in array components which don't look
            --  side effect free in the case of an array whose index type
            --  is an enumeration type with an enumeration rep clause.

            --  All other function calls are not side effect free

	 when N_Function_Call =>
	    
	    null;

            --  An indexed component is side effect free if it is a side
            --  effect free prefixed reference and all the indexing
            --  expressions are side effect free.

	 when N_Indexed_Component =>
	    Remove_List_Side_Effect (Expressions (N));

            --  A type qualification is side effect free if the expression
            --  is side effect free.

	 when N_Qualified_Expression =>
	    Remove_Expression_Side_Effect (Expression (N));

            --  A selected component is side effect free only if it is a
            --  side effect free prefixed reference.

	 when N_Selected_Component =>
	    null;

            --  A range is side effect free if the bounds are side effect free

	 when N_Range =>
	    Remove_Expression_Side_Effect (Low_Bound  (N));
	    Remove_Expression_Side_Effect (High_Bound (N));

            --  A slice is side effect free if it is a side effect free
            --  prefixed reference and the bounds are side effect free.

	 when N_Slice =>
	    Remove_Expression_Side_Effect (Discrete_Range (N));

            --  A type conversion is side effect free if the expression
            --  to be converted is side effect free.

	 when N_Type_Conversion =>
	    Remove_Expression_Side_Effect (Expression (N));

            --  A unary operator is side effect free if the operand
            --  is side effect free.

	 when N_Unary_Op =>
	    Remove_Expression_Side_Effect (Right_Opnd (N));

            --  An unchecked type conversion is side effect free only if it
            --  is safe and its argument is side effect free.

	 when N_Unchecked_Type_Conversion =>
	    Remove_Expression_Side_Effect (Expression (N));

            --  An unchecked expression is side effect free if its expression
            --  is side effect free.

	 when N_Unchecked_Expression =>
	    Remove_Expression_Side_Effect (Expression (N));
	    
            --  We consider that anything else has side effects. This is a bit
            --  crude, but we are pretty close for most common cases, and we
            --  are certainly correct (i.e. we never return True when the
            --  answer should be False).

	 when others =>
	    null;
      end case;
   end Remove_Expression_Side_Effect;
   
   -----------------------------------
   -- Remove_Statements_Side_Effect --
   -----------------------------------

   procedure Remove_Statements_Side_Effect (Stmts : List_Id) is
	 
      Stmt : Node_Id;
      Nxt  : Node_Id;
   begin
      Stmt := First (Stmts);
      while Present (Stmt) loop
	 Nxt := Next (Stmt);

	 if Nkind (Stmt) = N_Block_Statement then
	    Remove_Statements_Side_Effect (Statements (Stmt));
	    
	 elsif Nkind (Stmt) = N_Case_Statement then
	    declare
	       Alt : Node_Id;
	    begin
	       Alt := First (Alternatives (Stmt));
	       while Present (Alt) loop
		  Remove_Statements_Side_Effect (Statements (Alt));
		  Next (Alt);
	       end loop;
	    end;
	    
	 elsif Nkind (Stmt) = N_If_Statement then
	    
	    --  If some elsif has side effect, the elsif is rewritten as
	    --  an else which its first statements is an if with elsif condition
	    
	    Reflex.Expanders.Ch5.Rewrite_Elsif_Side_Effect (Stmt);
	    
	    Remove_Expression_Side_Effect (Condition (Stmt));
	    Remove_Statements_Side_Effect (Then_Statements (Stmt));
	    
	    --  Here we known that that the remaining eslif have not side
	    --  effects, as the Rewrite_Elsif_Side_Effect has rewriten the
	    --  elsif as if in case of side effects on the elsif condition, so 
	    --  no need to call Remove_Side_Effect on the condition
	    
	    declare
	       Part : Node_Id;
	    begin
	       Part := First (Elsif_Parts (Stmt));
	       while Present (Part) loop
		  Remove_Expression_Side_Effect (Condition (Part));
		  Remove_Statements_Side_Effect (Then_Statements (Part));
		  Next (Part);
	       end loop;
	    end;
	    
	    Remove_Statements_Side_Effect (Else_Statements (Stmt));
	       
	 elsif Nkind (Stmt) = N_Loop_Statement then
	    Remove_Statements_Side_Effect (Statements (Stmt));
	    
	 elsif Nkind (Stmt) = N_Assignment_Statement then
	    Remove_Expression_Side_Effect (Name (Stmt));
	    Remove_Expression_Side_Effect (Expression (Stmt));
					   
	 elsif Nkind (Stmt) = N_Block_Statement then
	    null;
	 elsif Nkind (Stmt) = N_Case_Statement then
	    null;
	 elsif Nkind (Stmt) = N_Code_Statement then
	    null;
	 elsif Nkind (Stmt) = N_Free_Statement then
	    null;
	 elsif Nkind (Stmt) = N_Goto_Statement then
	    null;
	 elsif Nkind (Stmt) = N_Loop_Statement then
	    null;
	 elsif Nkind (Stmt) = N_Null_Statement then
	    null;
	 elsif Nkind (Stmt) = N_Raise_Statement then
	    null;
	 elsif Nkind (Stmt) = N_Return_Statement then
	    null;
	 elsif Nkind (Stmt) = N_Exit_Statement then
	    null;
	 elsif Nkind (Stmt) = N_If_Statement then
	    null;
	 elsif Nkind (Stmt) = N_Procedure_Call_Statement then
	    null;
	 else
	    null;
	 end if;
	 
      end loop;
   end Remove_Statements_Side_Effect;
      
   
   -----------------------
   -- Expand_Expression --
   -----------------------

   procedure Expand_Expression (N : Node_Id) is
      
      procedure Expand_List (L : List_Id);
      
      -----------------
      -- Expand_List --
      -----------------
      
      procedure Expand_List (L : List_Id) is
         Expr : Node_Id;
	 Nxt  : Node_Id;
      begin
         if Is_Non_Empty_List (L) then
            Expr := First (L);
            while Present (Expr) loop
	       Nxt := Next (Expr);
               Expand_Expression (Expr);
               Expr := Nxt;
            end loop;
         end if;
      end Expand_List;
      
   begin
      --  Entity names are Side Effect free

      if Is_Entity_Name (N) then
	 null;
	 
	 --  A value known at compile time is always side effect free

      elsif Compile_Time_Known_Value (N) then
	 null;
      end if;

      --  For other than entity names and compile time known values,
      --  check the node kind for special processing.

      case Nkind (N) is

	 --  An attribute reference is side effect free if its expressions
	 --  are side effect free and its prefix is side effect free or
	 --  is an entity reference.

	 --  Is this right? what about x'first where x is a variable???

	 when N_Attribute_Reference =>
	    Expand_List (Expressions (N));
	    Expand_Expression (Prefix (N));

            --  A binary operator is side effect free if and both operands
            --  are side effect free. For this purpose binary operators
            --  include membership tests and short circuit forms

	 when N_Binary_Op |
	   N_In        |
	   N_Not_In    |
	   N_And_Then  |
	   N_Or_Else
	   =>
	    Expand_Expression (Left_Opnd  (N));
	    Expand_Expression (Right_Opnd (N));

            --  An explicit dereference is side effect free only if it is
            --  a side effect free prefixed reference.

	 when N_Explicit_Dereference =>
	    null;

            --  A call to _rep_to_pos is side effect free, since we generate
            --  this pure function call ourselves. Moreover it is critically
            --  important to make this exception, since otherwise we can
            --  have discriminants in array components which don't look
            --  side effect free in the case of an array whose index type
            --  is an enumeration type with an enumeration rep clause.

            --  All other function calls are not side effect free

	 when N_Function_Call =>
	    
	    null;

            --  An indexed component is side effect free if it is a side
            --  effect free prefixed reference and all the indexing
            --  expressions are side effect free.

	 when N_Indexed_Component =>
	    Expand_List (Expressions (N));

            --  A type qualification is side effect free if the expression
            --  is side effect free.

	 when N_Qualified_Expression =>
	    Expand_Expression (Expression (N));

            --  A selected component is side effect free only if it is a
            --  side effect free prefixed reference.

	 when N_Selected_Component =>
	    null;

            --  A range is side effect free if the bounds are side effect free

	 when N_Range =>
	    Expand_Expression (Low_Bound  (N));
	    Expand_Expression (High_Bound (N));

            --  A slice is side effect free if it is a side effect free
            --  prefixed reference and the bounds are side effect free.

	 when N_Slice =>
	    Expand_Expression (Discrete_Range (N));

            --  A type conversion is side effect free if the expression
            --  to be converted is side effect free.

	 when N_Type_Conversion =>
	    Expand_Expression (Expression (N));

            --  A unary operator is side effect free if the operand
            --  is side effect free.

	 when N_Unary_Op =>
	    Expand_Expression (Right_Opnd (N));

            --  An unchecked type conversion is side effect free only if it
            --  is safe and its argument is side effect free.

	 when N_Unchecked_Type_Conversion =>
	    Expand_Expression (Expression (N));

            --  An unchecked expression is side effect free if its expression
            --  is side effect free.

	 when N_Unchecked_Expression =>
	    Expand_Expression (Expression (N));
	    
            --  We consider that anything else has side effects. This is a bit
            --  crude, but we are pretty close for most common cases, and we
            --  are certainly correct (i.e. we never return True when the
            --  answer should be False).

	 when others =>
	    raise Program_Error;
      end case;
   end Expand_Expression;
   
   -----------------------
   -- Expand_Statements --
   -----------------------

   procedure Expand_Statements (Stmts : List_Id) is
	 
      Stmt : Node_Id;
      Nxt  : Node_Id;
   begin
      Stmt := First (Stmts);
      while Present (Stmt) loop
	 Nxt := Next (Stmt);

	 if Nkind (Stmt) = N_Block_Statement then
	    Expand_Statements (Statements (Stmt));
	    
	 elsif Nkind (Stmt) = N_Case_Statement then
	    declare
	       Alt : Node_Id;
	    begin
	       Alt := First (Alternatives (Stmt));
	       while Present (Alt) loop
		  Expand_Statements (Statements (Alt));
		  Next (Alt);
	       end loop;
	    end;
	    
	 elsif Nkind (Stmt) = N_If_Statement then
	    
	    --  If some elsif has side effect, the elsif is rewritten as
	    --  an else which its first statements is an if with elsif condition
	    
	    Reflex.Expanders.Ch5.Rewrite_Elsif_Side_Effect (Stmt);
	    
	    Expand_Expression (Condition (Stmt));
	    Expand_Statements (Then_Statements (Stmt));
	    
	    --  Here we known that that the remaining eslif have not side
	    --  effects, as the Rewrite_Elsif has rewriten the
	    --  elsif as if in case of side effects on the elsif condition, so 
	    --  no need to call Expand on the condition
	    
	    declare
	       Part : Node_Id;
	    begin
	       Part := First (Elsif_Parts (Stmt));
	       while Present (Part) loop
		  Expand_Expression (Condition (Part));
		  Expand_Statements (Then_Statements (Part));
		  Next (Part);
	       end loop;
	    end;
	    
	    Expand_Statements (Else_Statements (Stmt));
	       
	 elsif Nkind (Stmt) = N_Loop_Statement then
	    Expand_Statements (Statements (Stmt));
	    
	 elsif Nkind (Stmt) = N_Assignment_Statement then
	    Expand_Expression (Name (Stmt));
	    Expand_Expression (Expression (Stmt));
					   
	 elsif Nkind (Stmt) = N_Block_Statement then
	    null;
	 elsif Nkind (Stmt) = N_Case_Statement then
	    null;
	 elsif Nkind (Stmt) = N_Code_Statement then
	    null;
	 elsif Nkind (Stmt) = N_Free_Statement then
	    null;
	 elsif Nkind (Stmt) = N_Goto_Statement then
	    null;
	 elsif Nkind (Stmt) = N_Loop_Statement then
	    null;
	 elsif Nkind (Stmt) = N_Null_Statement then
	    null;
	 elsif Nkind (Stmt) = N_Raise_Statement then
	    null;
	 elsif Nkind (Stmt) = N_Return_Statement then
	    null;
	 elsif Nkind (Stmt) = N_Exit_Statement then
	    null;
	 elsif Nkind (Stmt) = N_If_Statement then
	    null;
	 elsif Nkind (Stmt) = N_Procedure_Call_Statement then
	    null;
	 else
	    null;
	 end if;
	 
      end loop;
   end Expand_Statements;
   
   -----------------------
   -- Plc_Library_Scope --
   -----------------------
   
   function Plc_Library_Scope (E : Entity_Id) return Boolean is
      
      S   : Entity_Id;
      Res : Boolean;
   begin
      Put_Line ("Plc_Library_Scope Begin " & Get_String (Chars (E)));
      S := Scope (E);
      
      Put_Line ("  Scope " & Get_String (Chars (S)));
      Put_Line ("  Scope Id " & S'Img);
      Put_Line ("  Scope Unity_Standard " & Unity_Standard'Img);
      
      while S /= Standard_Standard 
	and then S /= Unity_Standard
        and then not Is_Dynamic_Scope (S)
      loop
         S := Scope (S);
	 Put_Line (" loop Scope S " & S'Img);
      end loop;
      
      Put_Line ("  2 Scope Id " & S'Img);
      Put_Line ("  2 Scope Unity_Standard " & Unity_Standard'Img);
      Res := (S = Unity_Standard);
      Put_Line ("Plc_Library_Scope End " & Res'Img);
      return Res;
   end Plc_Library_Scope;
   
end Reflex.Expanders.Utils;
