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

with Ada.Text_Io; use Ada.Text_IO;

with Atree;    use Atree;
with Checks;   use Checks;
with Csets;    use Csets;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Tss;  use Exp_Tss;
with Exp_Unst; use Exp_Unst;
with Exp_Util; use Exp_Util;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Osint;    use Osint;
with Osint.C;  use Osint.C;
-- with Output;   use Output;
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
with Ttypes;   use Ttypes;
with Types;    use Types;
with Uintp;    use Uintp;
with Urealp;   use Urealp;
with System.HTable; use System.HTable;

with Glips.Gen.Utils; use Glips.Gen.Utils;
with Glips.Gen.Outputs; use Glips.Gen.Outputs;
with Glips.Gen.Dispatch; use Glips.Gen.Dispatch;
with Glips.Gen.Supports; use Glips.Gen.Supports;

package body Glips.Gen.Fat_Pointers is
   
   -------------------------
   -- Write_Fatptr_Bounds --
   -------------------------
   
   procedure Write_Fatptr_Bounds 
     (This       : access Glips_Generator_Record;
      Array_Node : Node_Id;
      Bound      : Bound_Kind;
      Dimension  : Pos) is
      
      Typ : Entity_Id     := Etype (Array_Node);
      Ob  : Output_Buffer := This.Get_Output_Buffer;
   begin
      if Is_Access_Type (Typ) then
	 Typ := Get_Type_Full_View (Designated_Type (Typ));
      end if;
      
      Generate_Node (This, Array_Node);
      Write_Char (Ob, '.');
      
      if Bound = Low then
	 Write_Fatptr_First (Ob, Typ, Dimension);
      else
	 Write_Fatptr_Last (Ob, Typ, Dimension);
      end if;
   end Write_Fatptr_Bounds;

   ------------------------
   -- Write_Range_Bounds --
   ------------------------
   
   procedure Write_Range_Bounds
     (This  : access Glips_Generator_Record;
      Bound : Bound_Kind;
      Rng   : Node_Id) is
      
      pragma Assert (Nkind (Rng) = N_Range);
   begin
      if Bound = Low then
	 Generate_Node (This, Low_Bound (Rng));
      else
	 Generate_Node (THis, High_Bound (Rng));
      end if;
   end Write_Range_Bounds;
   
   -----------------------
   -- Write_Type_Bounds --
   -----------------------
   
   procedure Write_Type_Bounds
     (This  : access Glips_Generator_Record;
      Typ   : Entity_Id;
      Bound : Bound_Kind) is
   begin
      if Bound = Low then
	 Generate_Node (This, Type_Low_Bound (Etype (Typ)));
      else
	 Generate_Node (This, Type_High_Bound (Etype (Typ)));
      end if;
   end Write_Type_Bounds;
   
   -----------------------
   -- Write_Type_Bounds --
   -----------------------
   
   procedure Write_Type_Bounds
     (This      : access Glips_Generator_Record;
      Array_Typ : Entity_Id;
      Bound     : Bound_Kind;
      Dimension : Pos) is
      
      Ind : Node_Id := First_Index (Array_Typ);
   begin
      for J in 2 .. Dimension loop
	 Next_Index (Ind);
      end loop;
      
      Write_Type_Bounds (This, Ind, Bound);
   end Write_Type_Bounds;
   
   -----------------
   -- Write_Bound --
   -----------------
   
   procedure Write_Bound
     (This       : access Glips_Generator_Record;
      Array_Node : Node_Id;
      Bound      : Bound_Kind;
      Dimension  : Pos) is
      
      Ob        : Output_Buffer := This.Get_Output_Buffer;
      Expr_Type : Entity_Id     := Get_Type_Full_View (Etype (Array_Node));
   begin
      if Is_Access_Type (Expr_Type) then
	 Expr_Type := Get_Type_Full_View (Designated_Type (Expr_Type));
      end if;
      
      --  Annoying special case of string literal
      
      if Ekind (Expr_Type) = E_String_Literal_Subtype then
	 if Bound = Low then
	    Write_Uint (Ob, Intval (String_Literal_Low_Bound (Expr_Type)));
	 else
	    Write_Uint
	      (Ob, String_Literal_Length (Expr_Type) -
		 Intval (String_Literal_Low_Bound (Expr_Type)) + 1);
	 end if;
	 
	 return;
      end if;
      
      if Nkind (Array_Node) in N_Has_Entity
	and then Present (Entity (Array_Node))
      then
	 declare
	    E   : constant Entity_Id := Entity (Array_Node);
	    Typ : constant Entity_Id := Get_Type_Full_View (Etype (E));
	 begin
	    if Ekind (E) = E_Variable then

	       if Is_Access_Type (Typ) then

		  --  Retrieve the bounds from the fat pointer

		  if not Is_Constrained (Designated_Type (Typ)) then
		     Write_Fatptr_Bounds
		       (This, Array_Node, Bound, Dimension);
		  else
		     Write_Type_Bounds 
		       (This, Designated_Type (Typ), Bound, Dimension);
		  end if;

	       else
		  Write_Type_Bounds (This, Typ, Bound, Dimension);
	       end if;

	    elsif Ekind (E) in Formal_Kind
	      and then not Is_Constrained (Typ)
	    then
	       Write_Fatptr_Bounds (This, Array_Node, Bound, Dimension);

	    else
	       Write_Type_Bounds (This, Expr_Type, Bound, Dimension);
	    end if;
	 end;
	 
      else
	 case Nkind (Array_Node) is
	    when N_Slice =>
	       declare
		  Rng : constant Node_Id := Discrete_Range (Array_Node);
		  
	       begin
		  if Nkind (Rng) = N_Range then
		     Write_Range_Bounds (This, Bound, Rng);
		  else
		     Write_Type_Bounds (This, Etype (Rng), Bound, Dimension);
		  end if;
	       end;

	    when N_Null =>

	       --  The bounds of null are 0 when initializing fat pointers

	       Write_Char (Ob, '0');

	    when N_Selected_Component |
	      N_Qualified_Expression =>
	       Write_Type_Bounds (This, Expr_Type, Bound, Dimension);

	    when others =>

	       --  Get index subtype bounds

	       Write_Type_Bounds (This, Expr_Type, Bound, Dimension);
	 end case;
      end if;
   end Write_Bound;

   -----------------------
   -- Write_Array_Bound --
   -----------------------

   procedure Write_Array_Bound
     (This      : access Glips_Generator_Record;
      Expr      : Node_Id;
      Bound     : Bound_Kind;
      Dimension : Pos)
   is
      Expr_Type  : constant Entity_Id := Get_Type_Full_View (Etype (Expr));
      Array_Node : Node_Id := Expr;
      Array_Type : Entity_Id;
   begin
      if Is_Access_Type (Expr_Type) then
         Array_Type := Get_Type_Full_View (Designated_Type (Expr_Type));
      else
         Array_Type := Expr_Type;
      end if;

      pragma Assert (Is_Array_Type (Array_Type));

      if not Is_Constrained (Array_Type) then
	 
         case Nkind (Array_Node) is
            when N_Attribute_Reference =>
               declare
                  Attr_Name   : constant Name_Id := Attribute_Name (Expr);
                  Attr_Id     : constant Attribute_Id :=
                                  Get_Attribute_Id (Attr_Name);
                  Attr_Prefix : constant Node_Id := Prefix (Expr);
                  Prefix_Type : constant Entity_Id :=
                                  Get_Type_Full_View (Etype (Attr_Prefix));
               begin
                  pragma Assert
                    (Attr_Id = Attribute_Access
                      or else Attr_Id = Attribute_Unchecked_Access
                      or else Attr_Id = Attribute_Unrestricted_Access);
                  pragma Assert (Is_Array_Type (Prefix_Type));

                  Array_Node := Attr_Prefix;
               end;

            when N_Type_Conversion =>
               Array_Node := Expression (Array_Node);

            when N_Null       |
                 N_Identifier =>
               null;

            when N_Allocator =>
               Array_Node := Expression (Array_Node);

               if Nkind (Array_Node) = N_Qualified_Expression then
                  Array_Node := Expression (Array_Node);
               end if;

            --  Play it safe and generate an error for other cases we haven't
            --  tested.
            --  ??? in particular we need to handle N_Allocator, see c34007d

            when others =>
               declare
                  S : constant String := Node_Kind'Image (Nkind (Array_Node));
               begin
                  Error_Msg_Strlen := S'Length;
                  Error_Msg_String (1 .. Error_Msg_Strlen) := S;
                  Error_Msg_N
                    ("unsupported access to unconstrained array (~)",
                     Array_Node);
               end;
         end case;
      end if;

      Write_Bound (This, Array_Node, Bound, Dimension);
   end Write_Array_Bound;
   
   --------------------------------------
   -- Write_Unconstrained_Array_Prefix --
   --------------------------------------

   procedure Write_Unconstrained_Array_Prefix
     (This : access Glips_Generator_Record;
      N    : Node_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      if Is_Unidimensional_Array_Type (Etype (N)) then
         Write_Str (Ob, "((");
         Generate_Node (This, Component_Type (Etype (N)));
         Write_Str (Ob, "*)");

         Write_Char (Ob, '(');

         if Nkind (N) = N_Explicit_Dereference then
            Generate_Node (This, Prefix (N));
         else
            Generate_Node (This, N);
         end if;

         Write_Fatptr_Dereference (Ob);
         Write_Str (Ob, "))");

      elsif Nkind (N) in N_Has_Entity
        and then Present (Actual_Subtype (Entity (N)))
      then
         Write_Str (Ob, "(*(");
         Write_Id (This, Actual_Subtype (Entity (N)));
         Write_Str (Ob, "*) ");
         Generate_Node (This, N);
         Write_Fatptr_Dereference (Ob);
         Write_Str (Ob, ")");

--  --        elsif Is_Array_Formal (N)
--  --          and then Nkind (N) = N_Explicit_Dereference
--  --          and then Has_Back_End_Itype (Entity (Prefix (N)))
--  --        then
--  --           Write_Str (Ob, "(*(");
--  --           null; --  JMA write_Back_End_Itype_Id (Entity (Prefix (N)));
--  --           Write_Str (Ob, "*) ");
--  --           Write_Id (This, Entity (Prefix (N)));
--  --           null; -- JMA write_Fatptr_Dereference;
--  --           Write_Str (Ob, ")");

      else
         declare
            S : constant String := Node_Kind'Image (Nkind (N));
         begin
            Error_Msg_Strlen := S'Length;
            Error_Msg_String (1 .. Error_Msg_Strlen) := S;
            Error_Msg_N ("unsupported unconstrained array access (~)", N);
         end;
      end if;
   end Write_Unconstrained_Array_Prefix;

   ---------------------
   -- Has_Fat_Pointer --
   ---------------------

   function Has_Fat_Pointer (Typ : Entity_Id) return Boolean is
      
      E : constant Entity_Id := Get_Type_Full_View (Typ);
   begin
      return Is_Unconstrained_Array_Type (E)
	or else
	(Is_Access_Type (E)
	   and then Is_Array_Type (Get_Type_Full_View (Designated_Type (E)))
	   and then not
	   Is_Constrained (Get_Type_Full_View (Designated_Type (E))));
   end Has_Fat_Pointer;

   ---------------------
   -- Is_Array_Formal --
   ---------------------

   function Is_Array_Formal (N : Node_Id) return Boolean is
      
      Nod : Node_Id := N;
   begin
      loop
	 while Nkind_In (Nod, N_Attribute_Reference,
			 N_Explicit_Dereference)
	 loop
	    Nod := Prefix (Nod);
	 end loop;

	 if Nkind (Nod) in N_Has_Entity
	   and then Present (Entity (Nod))
	   and then Present (Renamed_Object (Get_Type_Full_View (Entity (Nod))))
	 then
	    Nod := Renamed_Object (Get_Type_Full_View (Entity (Nod)));
	 end if;

	 exit when not Nkind_In (Nod, N_Attribute_Reference,
				 N_Explicit_Dereference);
      end loop;

      if Nkind (Nod) in N_Has_Entity
	and then Present (Entity (Nod))
	and then Is_Formal (Entity (Nod))
      then
	 declare
	    Typ : Entity_Id;
	 begin
	    Typ := Get_Type_Full_View (Etype (Entity (Nod)));

	    if Is_Access_Type (Typ) then
	       Typ := Get_Type_Full_View (Designated_Type (Typ));
	    end if;

	    return Is_Array_Type (Typ);
	 end;
      else
	 return False;
      end if;
   end Is_Array_Formal;

   -------------------------------
   -- Is_Constrained_Array_Type --
   -------------------------------

   function Is_Constrained_Array_Type (E : Entity_Id) return Boolean is
   begin
      return Is_Array_Type (E) and then Is_Constrained (E);
   end Is_Constrained_Array_Type;

   -----------------------------------
   -- Is_Unconstrained_Array_Formal --
   -----------------------------------

   function Is_Unconstrained_Array_Formal (N : Node_Id) return Boolean is
   begin
      return Is_Array_Formal (N) and then not Is_Constrained (Etype (N));
   end Is_Unconstrained_Array_Formal;

   ---------------------------------
   -- Is_Unconstrained_Array_Type --
   ---------------------------------

   function Is_Unconstrained_Array_Type (E : Entity_Id) return Boolean is
   begin
      return Is_Array_Type (E) and then not Is_Constrained (E);
   end Is_Unconstrained_Array_Type;

   ----------------------------------
   -- Is_Unidimensional_Array_Type --
   ----------------------------------

   function Is_Unidimensional_Array_Type (E : Entity_Id) return Boolean is
      
      Full_E : constant Entity_Id := Get_Type_Full_View (E);
   begin
      return
	Is_Array_Type (Full_E)
	and then (No (First_Index (Full_E))
		    or else No (Next_Index (First_Index (Full_E))));
   end Is_Unidimensional_Array_Type;

   ----------------------
   -- Write_Attr_Index --
   ----------------------

   procedure Write_Attr_Index
     (Ob         : Output_Buffer;
      Array_Type : Entity_Id; 
      Dimension  : Pos) is
   begin
      if not Is_Unidimensional_Array_Type (Array_Type) then
	 Write_Char (Ob, '(');
	 Write_Int (Ob, Integer (Dimension));
	 Write_Char (Ob, ')');
      end if;
   end Write_Attr_Index;

   -------------------------
   -- Write_Fatptr_Bounds --
   -------------------------

   procedure Write_Fatptr_Bounds
     (This : access Glips_Generator_Record;
      Expr : Node_Id; 
      Typ  : Entity_Id) is
      
      Ob : output_Buffer := This.Get_Output_Buffer;
   begin
      if Ekind (Typ) = E_String_Literal_Subtype then
	 Write_Array_Bound (This, Expr, Low, 1);
	 Write_Str (Ob, ", ");
	 Write_Array_Bound (This, Expr, High, 1);

      else
	 declare
	    Idx : Nat     := 1;
	    Ind : Node_Id := First_Index (Typ);

	 begin
	    while Present (Ind) loop
	       Write_Array_Bound (This, Expr, Low, Idx);
	       Write_Str (Ob, ", ");
	       Write_Array_Bound (This, Expr, High, Idx);

	       Idx := Idx + 1;
	       Next_Index (Ind);

	       if Present (Ind) then
		  Write_Str (Ob, ", ");
	       end if;
	    end loop;
	 end;
      end if;
   end Write_Fatptr_Bounds;

   --------------------------
   -- Write_Fatptr_Compare --
   --------------------------

   procedure Write_Fatptr_Compare
     (This : access Glips_Generator_Record;
      Lhs  : Node_Id; 
      Rhs  : Node_Id) is
      
      Ob        : Output_Buffer := This.Get_Output_Buffer;
      Is_Access : Boolean := False;

      procedure Write_Reference (N : Node_Id; Typ : Node_Id);
      --  Output a reference to N plus a dereference for fat pointers

      ---------------------
      -- Write_Reference --
      ---------------------

      procedure Write_Reference 
        (N    : Node_Id;
         Typ  : Node_Id) is
      begin
	 if Has_Fat_Pointer (Typ) then
	    if Is_Access then
	       Generate_Node_Paren (This, N);
	    else
	       Generate_Node (This, N);
	    end if;

	    Write_Fatptr_Dereference (Ob);
	 else
	    Generate_Node (This, N);
	 end if;
      end Write_Reference;

      --  Local variables

      Lhs_Typ : Node_Id := Get_Type_Full_View (Etype (Lhs));
      Rhs_Typ : Node_Id := Get_Type_Full_View (Etype (Rhs));

      --  Start of processing for Write_Fatptr_Compare

   begin
      if Is_Access_Type (Lhs_Typ) then
	 Lhs_Typ   := Get_Type_Full_View (Designated_Type (Lhs_Typ));
	 Is_Access := True;
      end if;

      if Is_Access_Type (Rhs_Typ) then
	 Rhs_Typ   := Get_Type_Full_View (Designated_Type (Rhs_Typ));
	 Is_Access := True;
      end if;

      Write_Str_Col_Check (Ob, "(");

      if Nkind (Rhs) = N_Null then
	 Write_Reference (Lhs, Lhs_Typ);
	 Write_Str (Ob, " = ");
	 Write_Str (Ob, "null");

      else
	 --  Generate for access types:
	 --    Lhs.all = Rhs.all
	 --    and Lhs.first = Rhs.first
	 --    and Lhs.last = Rhs.last
	 --
	 --  and for arrays:
	 --    sizeof (Lhs) == sizeof(Rhs)
	 --    and !memcmp(Lhs.all, Rhs.all, sizeof(...))

	 if Is_Access then
	    Write_Reference (Lhs, Lhs_Typ);
	    Write_Str (Ob, " = ");
	    Write_Reference (Rhs, Rhs_Typ);

	    for Idx in 1 .. Number_Dimensions (Lhs_Typ) loop
	       Write_Str_Col_Check (Ob, " and ");
	       Generate_Node (This, Lhs);
	       Write_Str (Ob, ".");
	       Write_Fatptr_First (Ob, Lhs_Typ, Idx);
	       Write_Str (Ob, " = ");
	       Generate_Node (This, Rhs);
	       Write_Str (Ob, ".");
	       Write_Fatptr_First (Ob, Rhs_Typ, Idx);
	       Write_Str_Col_Check (Ob, " and ");
	       Generate_Node (This, Lhs);
	       Write_Str (Ob, ".");
	       Write_Fatptr_Last (Ob, Lhs_Typ, Idx);
	       Write_Str (Ob, " = ");
	       Generate_Node (This, Rhs);
	       Write_Str (Ob, ".");
	       Write_Fatptr_Last (Ob, Rhs_Typ, Idx);
	    end loop;

	 else
-- --	    Output_Sizeof (Lhs);
	    Write_Str_Col_Check (Ob, " = ");
-- --	    Output_Sizeof (Rhs);
	    Write_Str_Col_Check (Ob, " and ");

	    Write_Str (Ob, "!memcmp(");
	    Write_Reference (Lhs, Lhs_Typ);
	    Write_Str (Ob, ", ");
	    Write_Reference (Rhs, Rhs_Typ);
	    Write_Str (Ob, ", ");
-- --	    Output_Sizeof (Lhs, Rhs);
	    Write_Char (Ob, ')');
	 end if;
      end if;

      Write_Char (Ob, ')');
   end Write_Fatptr_Compare;

   --------------------------
   -- Write_Fatptr_Declare --
   --------------------------

   procedure Write_Fatptr_Declare
     (This       : access Glips_Generator_Record;
      Array_Type : Entity_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
      
      procedure Write_Array_Length (Length : Pos);
      --  Output the length of the array declaration

      ------------------------
      -- Write_Array_Length --
      ------------------------

      procedure Write_Array_Length (Length : Pos) is
      begin
	 Write_Str (Ob, " : array (1..");
	 Write_Int (Ob, Integer (Length));
	 Write_Str (Ob, ") of Integer;");
      end Write_Array_Length;

      --  Start of processing for Write_Fatptr_Declare

   begin
      pragma Assert
	(Is_Array_Type (Array_Type)
	   and then not Is_Unidimensional_Array_Type (Array_Type));

      Write_Indent (Ob);

      --  Generate:

      --    type _fatptr__<typeName> is record
      --      All_Ptr : access <typeName>;
      --      First   : array (1..n) of integer_ptr_t;
      --      Last    : array (1..n) of integer_ptr_t;
      --    end record;

      Write_Str (Ob, "type _faptr__");
      Write_Id (This, Array_Type);
      Write_Str (Ob, " is record");

      Indent_Begin (Ob);
      Write_Indent (Ob);

      Write_Name_All_Ptr (Ob);
      Write_Str (Ob, " : access ");
      Write_Id (This, Component_Type (Array_Type));
      Write_Str (Ob, ";");

      Write_Indent (Ob);
      Write_Name_First (Ob);
      Write_Array_Length (Number_Dimensions (Array_Type));

      Write_Indent (Ob);
      Write_Name_Last (Ob);
      Write_Array_Length (Number_Dimensions (Array_Type));

      Indent_End (Ob);
      Write_Indent (Ob);

      Write_Str (Ob, "end record;");
      Write_Indent (Ob);
   end Write_Fatptr_Declare;

   ------------------------------
   -- Write_Fatptr_Dereference --
   ------------------------------

   procedure Write_Fatptr_Dereference (Ob : output_Buffer) is
   begin
      Write_Char (Ob, '.');
      Write_Name_All (Ob);
   end Write_Fatptr_Dereference;

   ------------------------------------
   -- Write_Fatptr_Indexed_Component --
   ------------------------------------

   procedure Write_Fatptr_Indexed_Component 
     (This : access Glips_Generator_Record;
      N    : Node_Id) is
      
      Ob        : Output_Buffer := This.get_Output_Buffer;
      Pref      : constant Node_Id   := Ultimate_Expression (Prefix (N));
      Pref_Type : constant Entity_Id := Get_Type_Full_View (Etype (Pref));
      Fatptr    : constant Node_Id   := Prefix (Pref);
   begin
      pragma Assert
	(Nkind (N) = N_Indexed_Component
	   and then Nkind (Pref) = N_Explicit_Dereference
	   and then Is_Unconstrained_Array_Type (Pref_Type)
	   and then not Is_Unidimensional_Array_Type (Pref_Type));

      --  Generate code to dereference the resulting computed address

      Write_Str (Ob, "(*("); --  Open parenthesis 1 & 2

      --  In practice the following cast is currently not needed since the
      --  type of the pointer defined in the fat pointer struct associated
      --  with multidimensional arrays is a pointer to the component type,
      --  and the first component of the expression generated to compute
      --  the address of the indexed array component is precisely such fat
      --  pointer component (implicitly meaning in C that the arithmetic of
      --  C pointers will use such size to displace the pointer). However,
      --  we generate it to leave the code clear but also to facilitate the
      --  early detection of problems in case of changes in this area since
      --  the correct type of the pointer is essential to ensure that the
      --  resulting values computed by this routine are correct.

      Write_Char (Ob, '(');
      Generate_Node (This, Component_Type (Pref_Type));
      Write_Str (Ob, "*)");

      --  The needed computation is simple: for each dimension generate code
      --  which displaces the pointer as many components as the number of
      --  components of each dimension multiplied by the index. As usual,
      --  given that in C arrays start at 0, the actual value of the index
      --  requires computing its distance to 'first.

      Write_Char (Ob, '(');  --  Open parenthesis 3

      Generate_Node (This, Fatptr);
      Write_Fatptr_Dereference (Ob);

      declare
	 Expr : Node_Id := First (Expressions (N));
         -- Idx  : Pos;
      begin
	 Write_Str (Ob, "("); 
	 
	 for Idx in 1..Number_Dimensions (Pref_Type) loop
	    Generate_Node (This, Expr);
	    
	    Next (Expr);
	    if Present (Expr) then
	       Write_Str (Ob, ", ");
	    end if;
	    
	 end loop;
      end;

      Write_Str (Ob, ")"); 
   end Write_Fatptr_Indexed_Component;

   ------------------------
   -- Write_Fatptr_First --
   ------------------------

   procedure Write_Fatptr_First
     (Ob         : Output_Buffer;
      Array_Type : Entity_Id; 
      Dimension  : Pos) is
      
      pragma Assert (Is_Unconstrained_Array_Type (Array_Type));
   begin
      Write_Name_First (Ob);
      Write_Attr_Index (Ob, Array_Type, Dimension);
   end Write_Fatptr_First;

   -----------------------
   -- Write_Fatptr_Last --
   -----------------------

   procedure Write_Fatptr_Last
     (Ob         : Output_Buffer;
      Array_Type : Entity_Id; 
      Dimension  : Pos) is
      pragma Assert (Is_Unconstrained_Array_Type (Array_Type));
   begin
      Write_Name_Last (Ob);
      Write_Attr_Index (Ob, Array_Type, Dimension);
   end Write_Fatptr_Last;

   --------------------------------
   -- Write_Number_of_Components --
   --------------------------------

   procedure Write_Number_Of_Components
     (This       : access Glips_Generator_Record;
      Fatptr     : Node_Id;
      Array_Type : Entity_Id;
      Dimension  : Nat := 0)
   is
      Ob : output_Buffer := This.Get_Output_Buffer;
      
      procedure Write_Fatptr_Length
	(Fatptr     : Node_Id;
	 Array_Type : Entity_Id;
	 Dimension  : Pos);
      --  Output code which computes the length of the array in the given
      --  dimension: Fatptr.last[dimension] - Fatptr.first[dimension] + 1

      -------------------------
      -- Write_Fatptr_Length --
      -------------------------

      procedure Write_Fatptr_Length
	(Fatptr     : Node_Id;
	 Array_Type : Entity_Id;
	 Dimension  : Pos)
      is
      begin
	 Generate_Node (This, Fatptr);
	 Write_Str (Ob, ".");
	 Write_Fatptr_Last (Ob, Array_Type, Dimension);

	 Write_Str_Col_Check (Ob, " - ");

	 Generate_Node (This, Fatptr);
	 Write_Str (Ob, ".");
	 Write_Fatptr_First (Ob, Array_Type, Dimension);

	 Write_Str_Col_Check (Ob, " + 1");
      end Write_Fatptr_Length;

      --  Local variables

      Idx : Nat     := 1;
      Ind : Node_Id := First_Index (Array_Type);

      --  Start of processing for Write_Number_Of_Components

   begin
      --  Locate the index of the given Dimension

      while Idx <= Dimension loop
	 Next_Index (Ind);
	 Idx := Idx + 1;
      end loop;

      --  Generate code which computes its number of components

      while Idx <= Number_Dimensions (Array_Type) loop
	 Write_Char (Ob, '(');
	 Write_Fatptr_Length (Fatptr, Array_Type, Idx);
	 Write_Char (Ob, ')');

	 if Idx < Number_Dimensions (Array_Type) then
	    Write_Str_Col_Check (Ob, " * ");
	 end if;

	 Next_Index (Ind);
	 Idx := Idx + 1;
      end loop;
   end Write_Number_Of_Components;

   -----------------------
   -- Write_Fatptr_Init --
   -----------------------

   procedure Write_Fatptr_Init
     (This          : access Glips_Generator_Record;
      Expr          : Node_Id;
      Typ           : Entity_Id;
      Use_Aggregate : Boolean := False)
   is
      Ob : Output_Buffer := This.Get_Output_Buffer;
      
      procedure Write_Array_Aggregate_Bounds (Expr : Node_Id);
      --  Output the low bound and high bound of all the dimensions of the
      --  type of Expr separated by commas:
      --    low-bound-1 {,low-bound-N} high-bound-1 {,high-bound-N}

      procedure Write_Call_Fatptr_Constructor
	(Expr       : Node_Id;
	 Array_Type : Entity_Id);
      --  Generate a call to the constructor of Typ to initialize Expr

      procedure Write_Fatptr_Aggregate
	(Expr       : Node_Id;
	 Array_Type : Entity_Id);
      --  Generate an aggregate of Typ to initialize Expr

      ----------------------------------
      -- Write_Array_Aggregate_Bounds --
      ----------------------------------

      procedure Write_Array_Aggregate_Bounds (Expr : Node_Id) is
	 Typ : Node_Id;

      begin
	 Typ := Get_Type_Full_View (Etype (Expr));

	 if Is_Access_Type (Typ) then
	    Typ := Get_Type_Full_View (Designated_Type (Typ));
	 end if;

	 --  Initialize all the components of first[]

	 declare
	    Idx : Nat := 1;
	    Ind : Node_Id := First_Index (Typ);

	 begin
	    while Present (Ind) loop
	       Write_Array_Bound (This, Expr, Low, Idx);
	       Write_Str (Ob, ", ");

	       Idx := Idx + 1;
	       Next_Index (Ind);
	    end loop;
	 end;

	 --  Initialize all the components of last[]

	 declare
	    Idx : Nat := 1;
	    Ind : Node_Id := First_Index (Typ);

	 begin
	    while Present (Ind) loop
	       Write_Array_Bound (This, Expr, High, Idx);

	       Idx := Idx + 1;
	       Next_Index (Ind);

	       if Present (Ind) then
		  Write_Str (Ob, ", ");
	       end if;
	    end loop;
	 end;
      end Write_Array_Aggregate_Bounds;

      -----------------------------------
      -- Write_Call_Fatptr_Constructor --
      -----------------------------------

      procedure Write_Call_Fatptr_Constructor
	(Expr       : Node_Id;
	 Array_Type : Entity_Id)
      is
	 Close_Paren : Boolean := True;
	 Expr_Typ    : Entity_Id := Get_Type_Full_View (Etype (Expr));
	 Saved_Value : constant Boolean := In_Fatptr_Constructor_Call;
	 U_Expr      : constant Node_Id := Ultimate_Expression (Expr);
	 U_Etyp      : constant Entity_Id := Get_Type_Full_View (Etype (U_Expr));

      begin
	 if Is_Access_Type (Expr_Typ) then
	    Expr_Typ := Get_Type_Full_View (Designated_Type (Expr_Typ));
	 end if;

	 In_Fatptr_Constructor_Call := True;

	 Write_Str (Ob, "_fatptr_UNCarray_CONS ");
	 Write_Str (Ob, "((void*)");

	 --  Null fat pointers are initialized with .all = NULL and all its
	 --  bounds set to 0.

	 if Nkind (U_Expr) = N_Null then
	    Write_Str (Ob, "NULL, ");

	    declare
	       Ind : Node_Id := First_Index (Array_Type);

	    begin
	       while Present (Ind) loop
		  Write_Str (Ob, "0, 0");
		  Next_Index (Ind);

		  if Present (Ind) then
		     Write_Str (Ob, ", ");
		  end if;
	       end loop;
	    end;

	 elsif Nkind (U_Expr) = N_Allocator then
	    Generate_Node (This, U_Expr);
	    Close_Paren := False;

	 elsif Nkind_In (Expr, N_Type_Conversion,
			 N_Unchecked_Type_Conversion)
	 then
	    Generate_Node (This, U_Expr);

	    if Has_Fat_Pointer (U_Etyp) then
	       Write_Fatptr_Dereference (Ob);
	    end if;

	    --  The bounds must be computed using the target type of the
	    --  type conversion.

	    Write_Str (Ob, ", ");
	    Write_Fatptr_Bounds (This, Expr, Expr_Typ);

            --  Common case

	 else
	    Generate_Node (This, Expr);

	    if Has_Fat_Pointer (Expr_Typ) then
	       Write_Fatptr_Dereference (Ob);
	    end if;

	    Write_Str (Ob, ", ");
	    Write_Fatptr_Bounds (This, Expr, Array_Type);
	 end if;

	 if Close_Paren then
	    Write_Str (Ob, ")");
	 end if;

	 In_Fatptr_Constructor_Call := Saved_Value;
      end Write_Call_Fatptr_Constructor;

      ----------------------------
      -- Write_Fatptr_Aggregate --
      ----------------------------

      procedure Write_Fatptr_Aggregate
	(Expr       : Node_Id;
	 Array_Type : Entity_Id)
      is
	 U_Expr : constant Node_Id := Ultimate_Expression (Expr);
	 U_Etyp : constant Entity_Id := Get_Type_Full_View (Etype (U_Expr));

      begin
	 Write_Char (Ob, '(');
	 Write_Fatptr_Name (This, Array_Type);
	 Write_Char (Ob, ')');

	 Write_Char (Ob, '{');

	 Write_Str (Ob, "(");
	 Write_Id (This, Component_Type (Array_Type));
	 Write_Str (Ob, "*) ");

	 if Nkind (U_Expr) = N_Null then
	    Write_Str (Ob, "NULL");

	 elsif Nkind (U_Expr) = N_Allocator then
	    Generate_Node (This, U_Expr);

	 else
	    Write_Str (Ob, "&");
	    Generate_Node (This, U_Expr);

	    if Has_Fat_Pointer (U_Etyp) then
	       Write_Fatptr_Dereference (Ob);
	    end if;
	 end if;

	 Write_Str (Ob, ", ");

	 --  The bounds must be computed using the type of the original
	 --  expression.

	 Write_Array_Aggregate_Bounds (Expr);
	 Write_Char (Ob, '}');
      end Write_Fatptr_Aggregate;

      --  Local variable

      Array_Type : Entity_Id;

      --  Start of processing for Write_Fatptr_Init

   begin
      if Is_Access_Type (Typ) then
	 Array_Type := Designated_Type (Typ);
      else
	 Array_Type := Typ;
      end if;

      --  This routine must not be invoked with an attribute reference.
      --  Attribute_Reference() must be invoked by the caller (routine
      --  that takes care of invoking this one). The exception of this
      --  rule is attribute 'Deref since the use of this attribute in
      --  constrained array actuals may involve building a fat pointer
      --  using the type of the formal (cf. Generate_Call).

      pragma Assert (Nkind (Expr) /= N_Attribute_Reference
		       or else
		       Get_Attribute_Id (Attribute_Name (Expr)) = Attribute_Deref);

      --  Ensure that it is correct to generate the code initializing a fat
      --  pointer.

      pragma Assert (Is_Unconstrained_Array_Type (Array_Type));

      --  Fat pointers of unidimensional arrays are initialized by means of
      --  the constructor to generate code compliant with C90.

      if Is_Unidimensional_Array_Type (Array_Type)
	and then not Use_Aggregate
      then
	 Write_Call_Fatptr_Constructor (Expr, Array_Type);

         --  Fat pointers of multidimensional arrays are initialized by means
         --  of an aggregate.

      else
	 Write_Fatptr_Aggregate (Expr, Array_Type);
      end if;
   end Write_Fatptr_Init;

   -----------------------
   -- Write_Fatptr_Name --
   -----------------------

   procedure Write_Fatptr_Name
     (This       : access Glips_Generator_Record;
      Array_Type : Entity_Id) is
      
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      pragma Assert (Is_Unconstrained_Array_Type (Array_Type));

      Write_Str (Ob, "_fatptr_");
      
      if Is_Unidimensional_Array_Type (Array_Type) then
	 null;
      else
	 Generate_Node (This, Array_Type, Declaration => True);
      end if;
   end Write_Fatptr_Name;

   --------------------
   -- Write_Name_All --
   --------------------

   procedure Write_Name_All (Ob : Output_Buffer) is
   begin
      Write_Str (Ob, "all");
   end Write_Name_All;

   ------------------------
   -- Write_Name_All_Ptr --
   ------------------------

   procedure Write_Name_All_Ptr (Ob : Output_Buffer) is
   begin
      Write_Str (Ob, "All_Ptr");
   end Write_Name_All_Ptr;

   ----------------------
   -- Write_Name_First --
   ----------------------

   procedure Write_Name_First (Ob : Output_Buffer) is
   begin
      Write_Str (Ob, "first");
   end Write_Name_First;

   ---------------------
   -- Write_Name_Last --
   ---------------------

   procedure Write_Name_Last (Ob : Output_Buffer) is
   begin
      Write_Str (Ob, "last");
   end Write_Name_Last;

end Glips.Gen.Fat_Pointers;
