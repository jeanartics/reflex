------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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
-- Reflex is a fork from the GNAT compiler. GNAT was originally developed   --
-- by the GNAT team at  New York University. Extensive  contributions to    --
-- GNAT were provided by Ada Core Technologies Inc. Reflex is developed  by --
-- the Artics team at Grenoble.                                             --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_Io; use Ada.Text_Io;

with Atree;    use Atree;
with Casing;   use Casing;
--with Checks;   use Checks;
with Debug;    use Debug;
with Errout;   use Errout;
with Elists;   use Elists;
--  with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Fname;    use Fname;
with Freeze;   use Freeze;
with Lib;      use Lib;
with Lib.Xref; use Lib.Xref;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Output;   use Output;
with Opt;      use Opt;
with Restrict; use Restrict;
with Scans;    use Scans;
with Scn;      use Scn;
with Sem;      use Sem;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stand;    use Stand;
with Style;
with Stringt;  use Stringt;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;

with GNAT.HTable; use GNAT.HTable;

package body Sem_Util is

   ----------------------------------------
   -- Global Variables for New_Copy_Tree --
   ----------------------------------------

   --  These global variables are used by New_Copy_Tree. See description of the
   --  body of this subprogram for details. Global variables can be safely used
   --  by New_Copy_Tree, since there is no case of a recursive call from the
   --  processing inside New_Copy_Tree.

   NCT_Hash_Threshold : constant := 20;
   --  If there are more than this number of pairs of entries in the map, then
   --  Hash_Tables_Used will be set, and the hash tables will be initialized
   --  and used for the searches.

   NCT_Hash_Tables_Used : Boolean := False;
   --  Set to True if hash tables are in use

   NCT_Table_Entries : Nat := 0;
   --  Count entries in table to see if threshold is reached

   NCT_Hash_Table_Setup : Boolean := False;
   --  Set to True if hash table contains data. We set this True if we setup
   --  the hash table with data, and leave it set permanently from then on,
   --  this is a signal that second and subsequent users of the hash table
   --  must clear the old entries before reuse.

   subtype NCT_Header_Num is Int range 0 .. 511;
   --  Defines range of headers in hash tables (512 headers)

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Build_Component_Subtype
     (C   : List_Id;
      Loc : Source_Ptr;
      T   : Entity_Id) return Node_Id;
   --  This function builds the subtype for Build_Actual_Subtype_Of_Component
   --  and Build_Discriminal_Subtype_Of_Component. C is a list of constraints,
   --  Loc is the source location, T is the original subtype.

   function Has_Null_Extension (T : Entity_Id) return Boolean;
   --  T is a derived tagged type. Check whether the type extension is null.
   --  If the parent type is fully initialized, T can be treated as such.

   --------------------------------
   -- Add_Access_Type_To_Process --
   --------------------------------

   procedure Add_Access_Type_To_Process (E : Entity_Id; A : Entity_Id) is
      L : Elist_Id;

   begin
      Ensure_Freeze_Node (E);
      L := Access_Types_To_Process (Freeze_Node (E));

      if No (L) then
         L := New_Elmt_List;
         Set_Access_Types_To_Process (Freeze_Node (E), L);
      end if;

      Append_Elmt (A, L);
   end Add_Access_Type_To_Process;

   -----------------------
   -- Alignment_In_Bits --
   -----------------------

   function Alignment_In_Bits (E : Entity_Id) return Uint is
   begin
      return Alignment (E) * System_Storage_Unit;
   end Alignment_In_Bits;

   -----------------------------------------
   -- Apply_Compile_Time_Constraint_Error --
   -----------------------------------------

   procedure Apply_Compile_Time_Constraint_Error
     (N      : Node_Id;
      Msg    : String;
      Reason : RT_Exception_Code;
      Ent    : Entity_Id  := Empty;
      Typ    : Entity_Id  := Empty;
      Loc    : Source_Ptr := No_Location;
      Rep    : Boolean    := True;
      Warn   : Boolean    := False)
   is
      Stat : constant Boolean := Is_Static_Expression (N);
      Rtyp : Entity_Id;

   begin
      if No (Typ) then
         Rtyp := Etype (N);
      else
         Rtyp := Typ;
      end if;

      if No (Compile_Time_Constraint_Error (N, Msg, Ent, Loc, Warn => Warn))
        or else not Rep
      then
         return;
      end if;

      --  Now we replace the node by an N_Raise_Constraint_Error node
      --  This does not need reanalyzing, so set it as analyzed now.

      Set_Analyzed (N, True);
      Set_Etype (N, Rtyp);
      Set_Raises_Constraint_Error (N);

      --  If the original expression was marked as static, the result is
      --  still marked as static, but the Raises_Constraint_Error flag is
      --  always set so that further static evaluation is not attempted.

      if Stat then
         Set_Is_Static_Expression (N);
      end if;
   end Apply_Compile_Time_Constraint_Error;

   --------------------------
   -- Build_Actual_Subtype --
   --------------------------

   function Build_Actual_Subtype
     (T : Entity_Id;
      N : Node_Or_Entity_Id) return Node_Id
   is
      Obj : Node_Id;

      Loc         : constant Source_Ptr := Sloc (N);
      Constraints : List_Id;
      Decl        : Node_Id;
      Hi          : Node_Id;
      Lo          : Node_Id;
      Subt        : Entity_Id;

   begin
      if Nkind (N) = N_Defining_Identifier then
         Obj := New_Reference_To (N, Loc);
      else
         Obj := N;
      end if;

      if Is_Array_Type (T) then
         Constraints := New_List;

         for J in 1 .. Number_Dimensions (T) loop

            --  Build an array subtype declaration with the nominal
            --  subtype and the bounds of the actual. Add the declaration
            --  in front of the local declarations for the subprogram, for
            --  analysis before any reference to the formal in the body.

            Lo :=
              Make_Attribute_Reference (Loc,
                Prefix         =>
                  Duplicate_Subexpr (Obj),
                Attribute_Name => Name_First,
                Expressions    => New_List (
                  Make_Integer_Literal (Loc, J)));

            Hi :=
              Make_Attribute_Reference (Loc,
                Prefix         =>
                  Duplicate_Subexpr (Obj),
                Attribute_Name => Name_Last,
                Expressions    => New_List (
                  Make_Integer_Literal (Loc, J)));

            Append (Make_Range (Loc, Lo, Hi), Constraints);
         end loop;

      else
         Constraints := New_List;
      end if;

      Subt :=
        Make_Defining_Identifier (Loc,
          Chars => New_Internal_Name ('S'));
      Set_Is_Internal (Subt);

      Decl :=
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Subt,
          Subtype_Indication =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (T,  Loc),
              Constraint  =>
                Make_Index_Or_Discriminant_Constraint (Loc,
                  Constraints => Constraints)));

      Mark_Rewrite_Insertion (Decl);
      return Decl;
   end Build_Actual_Subtype;

   ---------------------------------------
   -- Build_Actual_Subtype_Of_Component --
   ---------------------------------------

   function Build_Actual_Subtype_Of_Component
     (T : Entity_Id;
      N : Node_Id) return Node_Id
   is
      Loc       : constant Source_Ptr := Sloc (N);
      P         : constant Node_Id    := Prefix (N);
      Id        : Node_Id;
      Indx_Type : Entity_Id;

      Deaccessed_T : Entity_Id;
      --  This is either a copy of T, or if T is an access type, then it is
      --  the directly designated type of this access type.

      function Build_Actual_Array_Constraint return List_Id;
      --  If one or more of the bounds of the component depends on
      --  discriminants, build  actual constraint using the discriminants
      --  of the prefix.

      -----------------------------------
      -- Build_Actual_Array_Constraint --
      -----------------------------------

      function Build_Actual_Array_Constraint return List_Id is
         Constraints : constant List_Id := New_List;
         Indx        : Node_Id;
         Hi          : Node_Id;
         Lo          : Node_Id;
         Old_Hi      : Node_Id;
         Old_Lo      : Node_Id;

      begin
         Indx := First_Index (Deaccessed_T);
         while Present (Indx) loop
            Old_Lo := Type_Low_Bound  (Etype (Indx));
            Old_Hi := Type_High_Bound (Etype (Indx));

               Lo := New_Copy_Tree (Old_Lo);

               --  The new bound will be reanalyzed in the enclosing
               --  declaration. For literal bounds that come from a type
               --  declaration, the type of the context must be imposed, so
               --  insure that analysis will take place. For non-universal
               --  types this is not strictly necessary.

               Set_Analyzed (Lo, False);

               Hi := New_Copy_Tree (Old_Hi);
               Set_Analyzed (Hi, False);

            Append (Make_Range (Loc, Lo, Hi), Constraints);
            Next_Index (Indx);
         end loop;

         return Constraints;
      end Build_Actual_Array_Constraint;

   --  Start of processing for Build_Actual_Subtype_Of_Component

   begin
      if In_Default_Expression then
         return Empty;

      elsif Nkind (N) = N_Explicit_Dereference then
         if Is_Composite_Type (T)
           and then not Is_Constrained (T)
           and then not (Is_Class_Wide_Type (T)
                          and then Is_Constrained (Root_Type (T)))
         then
            --  If the type of the dereference is already constrained, it
            --  is an actual subtype.

            if Is_Array_Type (Etype (N))
              and then Is_Constrained (Etype (N))
            then
               return Empty;
            else
               -- Remove_Side_Effects (P);
               return Build_Actual_Subtype (T, N);
            end if;
         else
            return Empty;
         end if;
      end if;

      if Ekind (T) = E_Access_Subtype then
         Deaccessed_T := Designated_Type (T);
      else
         Deaccessed_T := T;
      end if;

      if Ekind (Deaccessed_T) = E_Array_Subtype then
         Id := First_Index (Deaccessed_T);
         Indx_Type := Underlying_Type (Etype (Id));

      end if;

      --  If none of the above, the actual and nominal subtypes are the same.

      return Empty;
   end Build_Actual_Subtype_Of_Component;

   -----------------------------
   -- Build_Component_Subtype --
   -----------------------------

   function Build_Component_Subtype
     (C   : List_Id;
      Loc : Source_Ptr;
      T   : Entity_Id) return Node_Id
   is
      Subt : Entity_Id;
      Decl : Node_Id;

   begin
      Subt :=
        Make_Defining_Identifier (Loc,
          Chars => New_Internal_Name ('S'));
      Set_Is_Internal (Subt);

      Decl :=
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Subt,
          Subtype_Indication =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (Base_Type (T),  Loc),
              Constraint  =>
                Make_Index_Or_Discriminant_Constraint (Loc,
                  Constraints => C)));

      Mark_Rewrite_Insertion (Decl);
      return Decl;
   end Build_Component_Subtype;

   ------------------------------
   -- Build_Elaboration_Entity --
   ------------------------------

   procedure Build_Elaboration_Entity (N : Node_Id; Spec_Id : Entity_Id) is
      Loc       : constant Source_Ptr       := Sloc (N);
      Unum      : constant Unit_Number_Type := Get_Source_Unit (Loc);
      Decl      : Node_Id;
      P         : Natural;
      Elab_Ent  : Entity_Id;

   begin
      --  Ignore if already constructed

      if Present (Elaboration_Entity (Spec_Id)) then
         return;
      end if;

      --  Construct name of elaboration entity as xxx_E, where xxx
      --  is the unit name with dots replaced by double underscore.
      --  We have to manually construct this name, since it will
      --  be elaborated in the outer scope, and thus will not have
      --  the unit name automatically prepended.

      Get_Name_String (Unit_Name (Unum));

      --  Replace the %s by _E

      Name_Buffer (Name_Len - 1 .. Name_Len) := "_E";

      --  Replace dots by double underscore

      P := 2;
      while P < Name_Len - 2 loop
         if Name_Buffer (P) = '.' then
            Name_Buffer (P + 2 .. Name_Len + 1) :=
              Name_Buffer (P + 1 .. Name_Len);
            Name_Len := Name_Len + 1;
            Name_Buffer (P) := '_';
            Name_Buffer (P + 1) := '_';
            P := P + 3;
         else
            P := P + 1;
         end if;
      end loop;

      --  Create elaboration flag

      Elab_Ent :=
        Make_Defining_Identifier (Loc, Chars => Name_Find);
      Set_Elaboration_Entity (Spec_Id, Elab_Ent);

      if No (Declarations (Aux_Decls_Node (N))) then
         Set_Declarations (Aux_Decls_Node (N), New_List);
      end if;

      Decl :=
         Make_Object_Declaration (Loc,
           Defining_Identifier => Elab_Ent,
           Object_Definition   =>
             New_Occurrence_Of (Standard_Boolean, Loc),
           Expression          =>
             New_Occurrence_Of (Standard_False, Loc));

      Append_To (Declarations (Aux_Decls_Node (N)), Decl);
      Analyze (Decl);

      --  Reset True_Constant indication, since we will indeed
      --  assign a value to the variable in the binder main.

      Set_Is_True_Constant (Elab_Ent, False);
      Set_Current_Value    (Elab_Ent, Empty);

      --  We do not want any further qualification of the name (if we did
      --  not do this, we would pick up the name of the generic package
      --  in the case of a library level generic instantiation).

      Set_Has_Qualified_Name       (Elab_Ent);
      Set_Has_Fully_Qualified_Name (Elab_Ent);
   end Build_Elaboration_Entity;

   -----------------------------------
   -- Cannot_Raise_Constraint_Error --
   -----------------------------------

   function Cannot_Raise_Constraint_Error (Expr : Node_Id) return Boolean is
   begin
      if Compile_Time_Known_Value (Expr) then
         return True;

      elsif Do_Range_Check (Expr) then
         return False;

      elsif Raises_Constraint_Error (Expr) then
         return False;

      else
         case Nkind (Expr) is
            when N_Identifier =>
               return True;

            when N_Expanded_Name =>
               return True;

            when N_Attribute_Reference =>
               if Do_Overflow_Check (Expr) then
                  return False;

               elsif No (Expressions (Expr)) then
                  return True;

               else
                  declare
                     N : Node_Id := First (Expressions (Expr));

                  begin
                     while Present (N) loop
                        if Cannot_Raise_Constraint_Error (N) then
                           Next (N);
                        else
                           return False;
                        end if;
                     end loop;

                     return True;
                  end;
               end if;

            when N_Type_Conversion =>
               if Do_Overflow_Check (Expr)
                 or else Do_Length_Check (Expr)
                 or else Do_Tag_Check (Expr)
               then
                  return False;
               else
                  return
                    Cannot_Raise_Constraint_Error (Expression (Expr));
               end if;

            when N_Unchecked_Type_Conversion =>
               return Cannot_Raise_Constraint_Error (Expression (Expr));

            when N_Unary_Op =>
               if Do_Overflow_Check (Expr) then
                  return False;
               else
                  return
                    Cannot_Raise_Constraint_Error (Right_Opnd (Expr));
               end if;

            when N_Op_Divide |
                 N_Op_Mod    |
                 N_Op_Rem
            =>
               if Do_Division_Check (Expr)
                 or else Do_Overflow_Check (Expr)
               then
                  return False;
               else
                  return
                    Cannot_Raise_Constraint_Error (Left_Opnd (Expr))
                      and then
                    Cannot_Raise_Constraint_Error (Right_Opnd (Expr));
               end if;

            when N_Op_Add                    |
                 N_Op_And                    |
                 N_Op_Concat                 |
                 N_Op_Eq                     |
                 N_Op_Expon                  |
                 N_Op_Ge                     |
                 N_Op_Gt                     |
                 N_Op_Le                     |
                 N_Op_Lt                     |
                 N_Op_Multiply               |
                 N_Op_Ne                     |
                 N_Op_Or                     |
                 N_Op_Rotate_Left            |
                 N_Op_Rotate_Right           |
                 N_Op_Shift_Left             |
                 N_Op_Shift_Right            |
                 N_Op_Shift_Right_Arithmetic |
                 N_Op_Subtract               |
                 N_Op_Xor
            =>
               if Do_Overflow_Check (Expr) then
                  return False;
               else
                  return
                    Cannot_Raise_Constraint_Error (Left_Opnd (Expr))
                      and then
                    Cannot_Raise_Constraint_Error (Right_Opnd (Expr));
               end if;

            when others =>
               return False;
         end case;
      end if;
   end Cannot_Raise_Constraint_Error;

   --------------------------
   -- Check_Fully_Declared --
   --------------------------

   procedure Check_Fully_Declared (T : Entity_Id; N : Node_Id) is
   begin
      if Ekind (T) = E_Incomplete_Type then

         --  Ada0Y (AI-50217): If the type is available through a limited
         --  with_clause, verify that its full view has been analyzed.

         if From_With_Type (T)
           and then Present (Non_Limited_View (T))
           and then Ekind (Non_Limited_View (T)) /= E_Incomplete_Type
         then
            --  The non-limited view is fully declared
            null;

         else
            Error_Msg_NE
              ("premature usage of incomplete}", N, First_Subtype (T));
         end if;

      elsif Has_Private_Component (T)
        and then not Is_Generic_Type (Root_Type (T))
        and then not In_Default_Expression
      then

	 Error_Msg_NE
	   ("premature usage of incomplete}", N, First_Subtype (T));
      end if;
   end Check_Fully_Declared;

   ---------------
   -- Check_VMS --
   ---------------

   procedure Check_VMS (Construct : Node_Id) is
   begin
      if not OpenVMS_On_Target then
         Error_Msg_N
           ("this construct is allowed only in Open'V'M'S", Construct);
      end if;
   end Check_VMS;

   ----------------------------------
   -- Collect_Primitive_Operations --
   ----------------------------------

   function Collect_Primitive_Operations (T : Entity_Id) return Elist_Id is
      B_Type         : constant Entity_Id := Base_Type (T);
      B_Decl         : constant Node_Id   := Original_Node (Parent (B_Type));
      B_Scope        : Entity_Id          := Scope (B_Type);
      Op_List        : Elist_Id;
      Formal         : Entity_Id;
      Is_Prim        : Boolean;
      Formal_Derived : Boolean := False;
      Id             : Entity_Id;

   begin
      --  For tagged types, the primitive operations are collected as they
      --  are declared, and held in an explicit list which is simply returned.

      if Is_Tagged_Type (B_Type) then
         return Primitive_Operations (B_Type);

      --  An untagged generic type that is a derived type inherits the
      --  primitive operations of its parent type. Other formal types only
      --  have predefined operators, which are not explicitly represented.

      elsif Is_Generic_Type (B_Type) then
         if Nkind (B_Decl) = N_Formal_Type_Declaration
           and then Nkind (Formal_Type_Definition (B_Decl))
             = N_Formal_Derived_Type_Definition
         then
            Formal_Derived := True;
         else
            return New_Elmt_List;
         end if;
      end if;

      Op_List := New_Elmt_List;

      if B_Scope = Standard_Standard then
         if B_Type = Standard_String then
            Append_Elmt (Standard_Op_Concat, Op_List);

         elsif B_Type = Standard_Wide_String then
            Append_Elmt (Standard_Op_Concatw, Op_List);

         else
            null;
         end if;

      elsif (Is_Package (B_Scope)
               and then Nkind (
                 Parent (Declaration_Node (First_Subtype (T))))
                   /=  N_Package_Body)

        or else Is_Derived_Type (B_Type)
      then
         --  The primitive operations appear after the base type, except
         --  if the derivation happens within the private part of B_Scope
         --  and the type is a private type, in which case both the type
         --  and some primitive operations may appear before the base
         --  type, and the list of candidates starts after the type.

         if In_Open_Scopes (B_Scope)
           and then Scope (T) = B_Scope
           and then In_Private_Part (B_Scope)
         then
            Id := Next_Entity (T);
         else
            Id := Next_Entity (B_Type);
         end if;

         while Present (Id) loop

            --  Note that generic formal subprograms are not
            --  considered to be primitive operations and thus
            --  are never inherited.

            if Is_Overloadable (Id)
              and then Nkind (Parent (Parent (Id)))
                         /= N_Formal_Subprogram_Declaration
            then
               Is_Prim := False;

               if Base_Type (Etype (Id)) = B_Type then
                  Is_Prim := True;
               else
                  Formal := First_Formal (Id);
                  while Present (Formal) loop
                     if Base_Type (Etype (Formal)) = B_Type then
                        Is_Prim := True;
                        exit;

                     elsif Ekind (Etype (Formal)) = E_Anonymous_Access_Type
                       and then Base_Type
                         (Designated_Type (Etype (Formal))) = B_Type
                     then
                        Is_Prim := True;
                        exit;
                     end if;

                     Next_Formal (Formal);
                  end loop;
               end if;

               --  For a formal derived type, the only primitives are the
               --  ones inherited from the parent type. Operations appearing
               --  in the package declaration are not primitive for it.

               if Is_Prim
                 and then (not Formal_Derived
                            or else Present (Alias (Id)))
               then
                  Append_Elmt (Id, Op_List);
               end if;
            end if;

            Next_Entity (Id);

            --  For a type declared in System, some of its operations
            --  may appear in  the target-specific extension to System.

            if No (Id)
              and then Chars (B_Scope) = Name_System
              and then Scope (B_Scope) = Standard_Standard
              and then Present_System_Aux
            then
               B_Scope := System_Aux_Id;
               Id := First_Entity (System_Aux_Id);
            end if;
         end loop;
      end if;

      return Op_List;
   end Collect_Primitive_Operations;

   -----------------------------------
   -- Compile_Time_Constraint_Error --
   -----------------------------------

   function Compile_Time_Constraint_Error
     (N    : Node_Id;
      Msg  : String;
      Ent  : Entity_Id  := Empty;
      Loc  : Source_Ptr := No_Location;
      Warn : Boolean  := False) return Node_Id
   is
      Msgc : String (1 .. Msg'Length + 2);
      Msgl : Natural;
      Wmsg : Boolean;
      P    : Node_Id;
      Msgs : Boolean;
      Eloc : Source_Ptr;

   begin
      --  A static constraint error in an instance body is not a fatal error.
      --  we choose to inhibit the message altogether, because there is no
      --  obvious node (for now) on which to post it. On the other hand the
      --  offending node must be replaced with a constraint_error in any case.

      --  No messages are generated if we already posted an error on this node

      if not Error_Posted (N) then
         if Loc /= No_Location then
            Eloc := Loc;
         else
            Eloc := Sloc (N);
         end if;

         --  Make all such messages unconditional

         Msgc (1 .. Msg'Length) := Msg;
         Msgc (Msg'Length + 1) := '!';
         Msgl := Msg'Length + 1;

         --  Message is a warning, even in Ada 95 case

         if Msg (Msg'Last) = '?' then
            Wmsg := True;

         --  In Ada 83, all messages are warnings. In the private part and
         --  the body of an instance, constraint_checks are only warnings.
         --  We also make this a warning if the Warn parameter is set.

         elsif Warn or else (Ada_83 and then Comes_From_Source (N)) then
            Msgl := Msgl + 1;
            Msgc (Msgl) := '?';
            Wmsg := True;

         elsif In_Instance_Not_Visible then
            Msgl := Msgl + 1;
            Msgc (Msgl) := '?';
            Wmsg := True;

         --  Otherwise we have a real error message (Ada 95 static case)

         else
            Wmsg := False;
         end if;

         --  Should we generate a warning? The answer is not quite yes. The
         --  very annoying exception occurs in the case of a short circuit
         --  operator where the left operand is static and decisive. Climb
         --  parents to see if that is the case we have here.

         Msgs := True;
         P := N;

         loop
            P := Parent (P);

            if (Nkind (P) = N_And_Then
                and then Compile_Time_Known_Value (Left_Opnd (P))
                and then Is_False (Expr_Value (Left_Opnd (P))))
              or else (Nkind (P) = N_Or_Else
                and then Compile_Time_Known_Value (Left_Opnd (P))
                and then Is_True (Expr_Value (Left_Opnd (P))))
            then
               Msgs := False;
               exit;

            elsif Nkind (P) = N_Component_Association
              and then Nkind (Parent (P)) = N_Aggregate
            then
               null;  --   Keep going.

            else
               exit when Nkind (P) not in N_Subexpr;
            end if;
         end loop;

         if Msgs then
            if Present (Ent) then
               Error_Msg_NEL (Msgc (1 .. Msgl), N, Ent, Eloc);
            else
               Error_Msg_NEL (Msgc (1 .. Msgl), N, Etype (N), Eloc);
            end if;

            if Wmsg then
--                 if Inside_Init_Proc then
--                    Error_Msg_NEL
--                      ("\& will be raised for objects of this type!?",
--                       N, Standard_Constraint_Error, Eloc);
--                 else
                  Error_Msg_NEL
                    ("\& will be raised at run time!?",
                     N, Standard_Constraint_Error, Eloc);
--               end if;
            else
               Error_Msg_NEL
                 ("\static expression raises&!",
                  N, Standard_Constraint_Error, Eloc);
            end if;
         end if;
      end if;

      return N;
   end Compile_Time_Constraint_Error;

   -----------------------
   -- Conditional_Delay --
   -----------------------

   procedure Conditional_Delay (New_Ent, Old_Ent : Entity_Id) is
   begin
      if Has_Delayed_Freeze (Old_Ent) and then not Is_Frozen (Old_Ent) then
         Set_Has_Delayed_Freeze (New_Ent);
      end if;
   end Conditional_Delay;

   --------------------
   -- Current_Entity --
   --------------------

   --  The currently visible definition for a given identifier is the
   --  one most chained at the start of the visibility chain, i.e. the
   --  one that is referenced by the Node_Id value of the name of the
   --  given identifier.

   function Current_Entity (N : Node_Id) return Entity_Id is
   begin
      return Get_Name_Entity_Id (Chars (N));
   end Current_Entity;

   -----------------------------
   -- Current_Entity_In_Scope --
   -----------------------------

   function Current_Entity_In_Scope (N : Node_Id) return Entity_Id is
      E  : Entity_Id;
      CS : constant Entity_Id := Current_Scope;

      Transient_Case : constant Boolean := Scope_Is_Transient;

   begin
      E := Get_Name_Entity_Id (Chars (N));

      while Present (E)
        and then Scope (E) /= CS
        and then (not Transient_Case or else Scope (E) /= Scope (CS))
      loop
         E := Homonym (E);
      end loop;

      return E;
   end Current_Entity_In_Scope;

   -------------------
   -- Current_Scope --
   -------------------

   function Current_Scope return Entity_Id is
   begin
      if Scope_Stack.Last = -1 then
         return Standard_Standard;
      else
         declare
            C : constant Entity_Id :=
                  Scope_Stack.Table (Scope_Stack.Last).Entity;
         begin
            if Present (C) then
               return C;
            else
               return Standard_Standard;
            end if;
         end;
      end if;
   end Current_Scope;

   ------------------------
   -- Current_Subprogram --
   ------------------------

   function Current_Subprogram return Entity_Id is
      Scop : constant Entity_Id := Current_Scope;

   begin
      if Is_Subprogram (Scop) or else Is_Generic_Subprogram (Scop) then
         return Scop;
      else
         return Enclosing_Subprogram (Scop);
      end if;
   end Current_Subprogram;

   ---------------------
   -- Defining_Entity --
   ---------------------

   function Defining_Entity (N : Node_Id) return Entity_Id is
      K   : constant Node_Kind := Nkind (N);
      Err : Entity_Id := Empty;

   begin
      case K is
         when
           N_Subprogram_Declaration                 |
           N_Abstract_Subprogram_Declaration        |
           N_Subprogram_Body                        |
           N_Package_Declaration                    |
           N_Subprogram_Renaming_Declaration        |
           N_Generic_Subprogram_Declaration         |
           N_Generic_Package_Declaration            |
           N_Formal_Subprogram_Declaration
         =>
            return Defining_Entity (Specification (N));

         when
           N_Component_Declaration                  |
           N_Defining_Program_Unit_Name             |
           N_Formal_Object_Declaration              |
           N_Formal_Package_Declaration             |
           N_Formal_Type_Declaration                |
           N_Full_Type_Declaration                  |
           N_Implicit_Label_Declaration             |
           N_Incomplete_Type_Declaration            |
           N_Loop_Parameter_Specification           |
           N_Number_Declaration                     |
           N_Object_Declaration                     |
           N_Object_Renaming_Declaration            |
           N_Parameter_Specification                |
           N_Private_Extension_Declaration          |
           N_Private_Type_Declaration               |
           N_Subtype_Declaration
         =>
            return Defining_Identifier (N);

         when
           N_Function_Instantiation                 |
           N_Function_Specification                 |
           N_Generic_Function_Renaming_Declaration  |
           N_Generic_Package_Renaming_Declaration   |
           N_Generic_Procedure_Renaming_Declaration |
           N_Package_Body                           |
           N_Package_Instantiation                  |
           N_Package_Renaming_Declaration           |
           N_Package_Specification                  |
           N_Procedure_Instantiation                |
           N_Procedure_Specification
         =>
            declare
               Nam : constant Node_Id := Defining_Unit_Name (N);

            begin
               if Nkind (Nam) in N_Entity then
                  return Nam;

               --  For Error, make up a name and attach to declaration
               --  so we can continue semantic analysis

               elsif Nam = Error then
                  Err :=
                    Make_Defining_Identifier (Sloc (N),
                      Chars => New_Internal_Name ('T'));
                  Set_Defining_Unit_Name (N, Err);

                  return Err;
               --  If not an entity, get defining identifier

               else
                  return Defining_Identifier (Nam);
               end if;
            end;

         when N_Block_Statement =>
            return Entity (Identifier (N));

         when others =>
            raise Program_Error;

      end case;
   end Defining_Entity;

   -------------------------
   -- Designate_Same_Unit --
   -------------------------

   function Designate_Same_Unit
     (Name1 : Node_Id;
      Name2 : Node_Id) return Boolean
   is
      K1 : constant Node_Kind := Nkind (Name1);
      K2 : constant Node_Kind := Nkind (Name2);

      function Prefix_Node (N : Node_Id) return Node_Id;
      --  Returns the parent unit name node of a defining program unit name
      --  or the prefix if N is a selected component or an expanded name.

      function Select_Node (N : Node_Id) return Node_Id;
      --  Returns the defining identifier node of a defining program unit
      --  name or  the selector node if N is a selected component or an
      --  expanded name.

      -----------------
      -- Prefix_Node --
      -----------------

      function Prefix_Node (N : Node_Id) return Node_Id is
      begin
         if Nkind (N) = N_Defining_Program_Unit_Name then
            return Name (N);

         else
            return Prefix (N);
         end if;
      end Prefix_Node;

      -----------------
      -- Select_Node --
      -----------------

      function Select_Node (N : Node_Id) return Node_Id is
      begin
         if Nkind (N) = N_Defining_Program_Unit_Name then
            return Defining_Identifier (N);

         else
            return Selector_Name (N);
         end if;
      end Select_Node;

   --  Start of processing for Designate_Next_Unit

   begin
      if (K1 = N_Identifier or else
          K1 = N_Defining_Identifier)
        and then
         (K2 = N_Identifier or else
          K2 = N_Defining_Identifier)
      then
         return Chars (Name1) = Chars (Name2);

      elsif
         (K1 = N_Expanded_Name      or else
          K1 = N_Selected_Component or else
          K1 = N_Defining_Program_Unit_Name)
        and then
         (K2 = N_Expanded_Name      or else
          K2 = N_Selected_Component or else
          K2 = N_Defining_Program_Unit_Name)
      then
         return
           (Chars (Select_Node (Name1)) = Chars (Select_Node (Name2)))
             and then
               Designate_Same_Unit (Prefix_Node (Name1), Prefix_Node (Name2));

      else
         return False;
      end if;
   end Designate_Same_Unit;

   ------------------------------
   -- Enclosing_Comp_Unit_Node --
   ------------------------------

   function Enclosing_Comp_Unit_Node (N : Node_Id) return Node_Id is
      Current_Node : Node_Id;

   begin
      Current_Node := N;
      while Present (Current_Node)
        and then Nkind (Current_Node) /= N_Compilation_Unit
      loop
         Current_Node := Parent (Current_Node);
      end loop;

      if Nkind (Current_Node) /= N_Compilation_Unit then
         return Empty;
      else
         return Current_Node;
      end if;
   end Enclosing_Comp_Unit_Node;

   ---------------------------
   -- Enclosing_Declaration --
   ---------------------------

   function Enclosing_Declaration (N : Node_Id) return Node_Id is
      Decl : Node_Id := N;

   begin
      while Present (Decl)
        and then not (Nkind (Decl) in N_Declaration
                        or else
                      Nkind (Decl) in N_Later_Decl_Item)
      loop
         Decl := Parent (Decl);
      end loop;

      return Decl;
   end Enclosing_Declaration;

   -----------------------
   -- Enclosing_Package --
   -----------------------

   function Enclosing_Package (E : Entity_Id) return Entity_Id is
      Dynamic_Scope : constant Entity_Id := Enclosing_Dynamic_Scope (E);

   begin
      if Dynamic_Scope = Standard_Standard then
         return Standard_Standard;

      elsif Dynamic_Scope = Empty then
         return Empty;

      elsif Ekind_In (Dynamic_Scope, E_Package, E_Package_Body,
                      E_Generic_Package)
      then
         return Dynamic_Scope;

      else
         return Enclosing_Package (Dynamic_Scope);
      end if;
   end Enclosing_Package;

   -------------------------------------
   -- Enclosing_Package_Or_Subprogram --
   -------------------------------------

   function Enclosing_Package_Or_Subprogram (E : Entity_Id) return Entity_Id is
      S : Entity_Id;

   begin
      S := Scope (E);
      while Present (S) loop
         if Is_Package_Or_Generic_Package (S)
           or else Ekind (S) = E_Package_Body
         then
            return S;

         elsif Is_Subprogram_Or_Generic_Subprogram (S)
           or else Ekind (S) = E_Subprogram_Body
         then
            return S;

         else
            S := Scope (S);
         end if;
      end loop;

      return Empty;
   end Enclosing_Package_Or_Subprogram;

   ----------------------------
   -- Enclosing_Generic_Unit --
   ----------------------------

   function Enclosing_Generic_Unit
     (N : Node_Id) return Node_Id
   is
      P    : Node_Id;
      Decl : Node_Id;
      Spec : Node_Id;

   begin
      P := Parent (N);
      while Present (P) loop
         if Nkind (P) = N_Generic_Package_Declaration
           or else Nkind (P) = N_Generic_Subprogram_Declaration
         then
            return P;

         elsif Nkind (P) = N_Package_Body
           or else Nkind (P) = N_Subprogram_Body
         then
            Spec := Corresponding_Spec (P);

            if Present (Spec) then
               Decl := Unit_Declaration_Node (Spec);

               if Nkind (Decl) = N_Generic_Package_Declaration
                 or else Nkind (Decl) = N_Generic_Subprogram_Declaration
               then
                  return Decl;
               end if;
            end if;
         end if;

         P := Parent (P);
      end loop;

      return Empty;
   end Enclosing_Generic_Unit;

   ----------------------------
   -- Enclosing_Generic_Body --
   ----------------------------

   function Enclosing_Generic_Body
     (E : Entity_Id) return Node_Id
   is
      P    : Node_Id;
      Decl : Node_Id;
      Spec : Node_Id;

   begin
      P := Parent (E);

      while Present (P) loop
         if Nkind (P) = N_Package_Body
           or else Nkind (P) = N_Subprogram_Body
         then
            Spec := Corresponding_Spec (P);

            if Present (Spec) then
               Decl := Unit_Declaration_Node (Spec);

               if Nkind (Decl) = N_Generic_Package_Declaration
                 or else Nkind (Decl) = N_Generic_Subprogram_Declaration
               then
                  return P;
               end if;
            end if;
         end if;

         P := Parent (P);
      end loop;

      return Empty;
   end Enclosing_Generic_Body;

   -------------------------------
   -- Enclosing_Lib_Unit_Entity --
   -------------------------------

   function Enclosing_Lib_Unit_Entity
      (E : Entity_Id := Current_Scope) return Entity_Id is
      Unit_Entity : Entity_Id := Current_Scope;

   begin
      --  Look for enclosing library unit entity by following scope links.
      --  Equivalent to, but faster than indexing through the scope stack.

      Unit_Entity := E;
      while (Present (Scope (Unit_Entity))
        and then Scope (Unit_Entity) /= Standard_Standard)
        and not Is_Child_Unit (Unit_Entity)
      loop
         Unit_Entity := Scope (Unit_Entity);
      end loop;

      return Unit_Entity;
   end Enclosing_Lib_Unit_Entity;

   -----------------------------
   -- Enclosing_Lib_Unit_Node --
   -----------------------------

   function Enclosing_Lib_Unit_Node (N : Node_Id) return Node_Id is
      Current_Node : Node_Id := N;

   begin
      while Present (Current_Node)
        and then Nkind (Current_Node) /= N_Compilation_Unit
      loop
         Current_Node := Parent (Current_Node);
      end loop;

      if Nkind (Current_Node) /= N_Compilation_Unit then
         return Empty;
      end if;

      return Current_Node;
   end Enclosing_Lib_Unit_Node;

   --------------------------
   -- Enclosing_Subprogram --
   --------------------------

   function Enclosing_Subprogram (E : Entity_Id) return Entity_Id is
      Dynamic_Scope : constant Entity_Id := Enclosing_Dynamic_Scope (E);

   begin
      if Dynamic_Scope = Standard_Standard then
         return Empty;

      elsif Ekind (Dynamic_Scope) = E_Subprogram_Body then
         return Corresponding_Spec (Parent (Parent (Dynamic_Scope)));

      elsif Ekind (Dynamic_Scope) = E_Block then
         return Enclosing_Subprogram (Dynamic_Scope);

      else
         return Dynamic_Scope;
      end if;
   end Enclosing_Subprogram;

   ------------------------
   -- Ensure_Freeze_Node --
   ------------------------

   procedure Ensure_Freeze_Node (E : Entity_Id) is
      FN : Node_Id;

   begin
      if No (Freeze_Node (E)) then
         FN := Make_Freeze_Entity (Sloc (E));
         Set_Has_Delayed_Freeze (E);
         Set_Freeze_Node (E, FN);
         Set_Access_Types_To_Process (FN, No_Elist);
         Set_TSS_Elist (FN, No_Elist);
         Set_Entity (FN, E);
      end if;
   end Ensure_Freeze_Node;

   ----------------
   -- Enter_Name --
   ----------------

   procedure Enter_Name (Def_Id : Node_Id) is
      C : constant Entity_Id := Current_Entity (Def_Id);
      E : constant Entity_Id := Current_Entity_In_Scope (Def_Id);
      S : constant Entity_Id := Current_Scope;

   begin
      Put_Line ("ENter Name Def_Id " & Nkind (Def_Id)'Img);
      Put_Line ("ENter Name Def_Id Chars " & Get_String (Chars (Def_Id)));

      Generate_Definition (Def_Id);

      --  Add new name to current scope declarations. Check for duplicate
      --  declaration, which may or may not be a genuine error.

      if Present (E) then

         --  Case of previous entity entered because of a missing declaration
         --  or else a bad subtype indication. Best is to use the new entity,
         --  and make the previous one invisible.

         if Etype (E) = Any_Type then
            Set_Is_Immediately_Visible (E, False);

         --  Case of renaming declaration constructed for package instances.
         --  if there is an explicit declaration with the same identifier,
         --  the renaming is not immediately visible any longer, but remains
         --  visible through selected component notation.

         elsif Nkind (Parent (E)) = N_Package_Renaming_Declaration
           and then not Comes_From_Source (E)
         then
            Set_Is_Immediately_Visible (E, False);

         --  The new entity may be the package renaming, which has the same
         --  same name as a generic formal which has been seen already.

         elsif Nkind (Parent (Def_Id)) = N_Package_Renaming_Declaration
            and then not Comes_From_Source (Def_Id)
         then
            Set_Is_Immediately_Visible (E, False);

         --  A controller component for a type extension overrides the
         --  inherited component.

         elsif Chars (E) = Name_uController then
            null;

         --  Case of an implicit operation or derived literal. The new entity
         --  hides the implicit one,  which is removed from all visibility,
         --  i.e. the entity list of its scope, and homonym chain of its name.

         elsif (Is_Overloadable (E) and then Present (Alias (E)))
           or else Is_Internal (E)
           or else (Ekind (E) = E_Enumeration_Literal
                     and then Is_Derived_Type (Etype (E)))
         then
            declare
               Prev     : Entity_Id;
               Prev_Vis : Entity_Id;
               Decl     : constant Node_Id := Parent (E);

            begin
               --  If E is an implicit declaration, it cannot be the first
               --  entity in the scope.

               Prev := First_Entity (Current_Scope);

               while Present (Prev)
                 and then Next_Entity (Prev) /= E
               loop
                  Next_Entity (Prev);
               end loop;

               if No (Prev) then

                  --  If E is not on the entity chain of the current scope,
                  --  it is an implicit declaration in the generic formal
                  --  part of a generic subprogram. When analyzing the body,
                  --  the generic formals are visible but not on the entity
                  --  chain of the subprogram. The new entity will become
                  --  the visible one in the body.

                  pragma Assert
                    (Nkind (Parent (Decl)) = N_Generic_Subprogram_Declaration);
                  null;

               else
                  Set_Next_Entity (Prev, Next_Entity (E));

                  if No (Next_Entity (Prev)) then
                     Set_Last_Entity (Current_Scope, Prev);
                  end if;

                  if E = Current_Entity (E) then
                     Prev_Vis := Empty;

                  else
                     Prev_Vis := Current_Entity (E);
                     while Homonym (Prev_Vis) /= E loop
                        Prev_Vis := Homonym (Prev_Vis);
                     end loop;
                  end if;

                  if Present (Prev_Vis)  then

                     --  Skip E in the visibility chain

                     Set_Homonym (Prev_Vis, Homonym (E));

                  else
                     Set_Name_Entity_Id (Chars (E), Homonym (E));
                  end if;
               end if;
            end;

         --  In the body or private part of an instance, a type extension
         --  may introduce a component with the same name as that of an
         --  actual. The legality rule is not enforced, but the semantics
         --  of the full type with two components of the same name are not
         --  clear at this point ???

         elsif In_Instance_Not_Visible  then
            null;

         --  When compiling a package body, some child units may have become
         --  visible. They cannot conflict with local entities that hide them.

         elsif Is_Child_Unit (E)
           and then In_Open_Scopes (Scope (E))
           and then not Is_Immediately_Visible (E)
         then
            null;

         --  Conversely, with front-end inlining we may compile the parent
         --  body first, and a child unit subsequently. The context is now
         --  the parent spec, and body entities are not visible.

         elsif Is_Child_Unit (Def_Id)
           and then Is_Package_Body_Entity (E)
           and then not In_Package_Body (Current_Scope)
         then
            null;

         --  Case of genuine duplicate declaration

         else
            Error_Msg_Sloc := Sloc (E);

            --  If the previous declaration is an incomplete type declaration
            --  this may be an attempt to complete it with a private type.
            --  The following avoids confusing cascaded errors.

            if Nkind (Parent (E)) = N_Incomplete_Type_Declaration
              and then Nkind (Parent (Def_Id)) = N_Private_Type_Declaration
            then
               Error_Msg_N
                 ("incomplete type cannot be completed" &
                        " with a private declaration",
                    Parent (Def_Id));
               Set_Is_Immediately_Visible (E, False);
               Set_Full_View (E, Def_Id);

            --  If the name of the unit appears in its own context clause,
            --  a dummy package with the name has already been created, and
            --  the error emitted. Try to continue quietly.

            elsif Error_Posted (E)
              and then Sloc (E) = No_Location
              and then Nkind (Parent (E)) = N_Package_Specification
              and then Current_Scope = Standard_Standard
            then
               Set_Scope (Def_Id, Current_Scope);
               return;

            else
               Error_Msg_N ("& conflicts with declaration#", Def_Id);

               --  Avoid cascaded messages with duplicate components in
               --  derived types.

               if Ekind (E) = E_Component
               then
                  return;
               end if;
            end if;

            if Nkind (Parent (Parent (Def_Id)))
                 = N_Generic_Subprogram_Declaration
              and then Def_Id =
                Defining_Entity (Specification (Parent (Parent (Def_Id))))
            then
               Error_Msg_N ("\generic units cannot be overloaded", Def_Id);
            end if;

            --  If entity is in standard, then we are in trouble, because
            --  it means that we have a library package with a duplicated
            --  name. That's hard to recover from, so abort!

            if S = Standard_Standard then
               raise Unrecoverable_Error;

            --  Otherwise we continue with the declaration. Having two
            --  identical declarations should not cause us too much trouble!

            else
               null;
            end if;
         end if;
      end if;

      --  If we fall through, declaration is OK , or OK enough to continue

      --  If Def_Id is a discriminant or a record component we are in the
      --  midst of inheriting components in a derived record definition.
      --  Preserve their Ekind and Etype.

      if Ekind (Def_Id) = E_Component
      then
         null;

      --  If a type is already set, leave it alone (happens whey a type
      --  declaration is reanalyzed following a call to the optimizer)

      elsif Present (Etype (Def_Id)) then
         null;

      --  Otherwise, the kind E_Void insures that premature uses of the entity
      --  will be detected. Any_Type insures that no cascaded errors will occur

      else
         Set_Ekind (Def_Id, E_Void);
         Set_Etype (Def_Id, Any_Type);
      end if;

      --  Inherited discriminants and components in derived record types are
      --  immediately visible. Itypes are not.

      if Ekind (Def_Id) = E_Component
        or else not Is_Itype (Def_Id)
      then
         Set_Is_Immediately_Visible (Def_Id);
         Set_Current_Entity         (Def_Id);
      end if;
Put_Line ("Enter Name Ok");
      Set_Homonym       (Def_Id, C);
      Append_Entity     (Def_Id, S);
      Set_Public_Status (Def_Id);

      --  Warn if new entity hides an old one

      if Warn_On_Hiding
        and then Present (C)
        and then Length_Of_Name (Chars (C)) /= 1
        and then Comes_From_Source (C)
        and then Comes_From_Source (Def_Id)
        and then In_Extended_Main_Source_Unit (Def_Id)
      then
         Error_Msg_Sloc := Sloc (C);
         Error_Msg_N ("declaration hides &#?", Def_Id);
      end if;
   end Enter_Name;

   -----------------------------
   -- Find_Static_Alternative --
   -----------------------------

   function Find_Static_Alternative (N : Node_Id) return Node_Id is
      Expr   : constant Node_Id := Expression (N);
      Val    : constant Uint    := Expr_Value (Expr);
      Alt    : Node_Id;
      Choice : Node_Id;

   begin
      Alt := First (Alternatives (N));

      Search : loop
         if Nkind (Alt) /= N_Pragma then
            Choice := First (Discrete_Choices (Alt));

            while Present (Choice) loop

               --  Others choice, always matches

               if Nkind (Choice) = N_Others_Choice then
                  exit Search;

               --  Range, check if value is in the range

               elsif Nkind (Choice) = N_Range then
                  exit Search when
                    Val >= Expr_Value (Low_Bound (Choice))
                      and then
                    Val <= Expr_Value (High_Bound (Choice));

               --  Choice is a subtype name. Note that we know it must
               --  be a static subtype, since otherwise it would have
               --  been diagnosed as illegal.

               elsif Is_Entity_Name (Choice)
                 and then Is_Type (Entity (Choice))
               then
                  exit Search when Is_In_Range (Expr, Etype (Choice));

               --  Choice is a subtype indication

               elsif Nkind (Choice) = N_Subtype_Indication then
                  declare
                     C : constant Node_Id := Constraint (Choice);
                     R : constant Node_Id := Range_Expression (C);

                  begin
                     exit Search when
                       Val >= Expr_Value (Low_Bound (R))
                         and then
                       Val <= Expr_Value (High_Bound (R));
                  end;

               --  Choice is a simple expression

               else
                  exit Search when Val = Expr_Value (Choice);
               end if;

               Next (Choice);
            end loop;
         end if;

         Next (Alt);
         pragma Assert (Present (Alt));
      end loop Search;

      --  The above loop *must* terminate by finding a match, since
      --  we know the case statement is valid, and the value of the
      --  expression is known at compile time. When we fall out of
      --  the loop, Alt points to the alternative that we know will
      --  be selected at run time.

      return Alt;
   end Find_Static_Alternative;

   ------------------
   -- First_Actual --
   ------------------

   function First_Actual (Node : Node_Id) return Node_Id is
      N : Node_Id;

   begin
      if No (Parameter_Associations (Node)) then
         return Empty;
      end if;

      N := First (Parameter_Associations (Node));

      if Nkind (N) = N_Parameter_Association then
         return First_Named_Actual (Node);
      else
         return N;
      end if;
   end First_Actual;

   -------------------------
   -- Full_Qualified_Name --
   -------------------------

   function Full_Qualified_Name (E : Entity_Id) return String_Id is
      Res : String_Id;
      pragma Warnings (Off, Res);

      function Internal_Full_Qualified_Name (E : Entity_Id) return String_Id;
      --  Compute recursively the qualified name without NUL at the end.

      ----------------------------------
      -- Internal_Full_Qualified_Name --
      ----------------------------------

      function Internal_Full_Qualified_Name (E : Entity_Id) return String_Id is
         Ent         : Entity_Id := E;
         Parent_Name : String_Id := No_String;

      begin
         --  Deals properly with child units

         if Nkind (Ent) = N_Defining_Program_Unit_Name then
            Ent := Defining_Identifier (Ent);
         end if;

         --  Compute recursively the qualification. Only "Standard" has no
         --  scope.

         if Present (Scope (Scope (Ent))) then
            Parent_Name := Internal_Full_Qualified_Name (Scope (Ent));
         end if;

         --  Every entity should have a name except some expanded blocks
         --  don't bother about those.

         if Chars (Ent) = No_Name then
            return Parent_Name;
         end if;

         --  Add a period between Name and qualification

         if Parent_Name /= No_String then
            Start_String (Parent_Name);
            Store_String_Char (Get_Char_Code ('.'));

         else
            Start_String;
         end if;

         --  Generates the entity name in upper case

         Get_Name_String (Chars (Ent));
         Set_All_Upper_Case;
         Store_String_Chars (Name_Buffer (1 .. Name_Len));
         return End_String;
      end Internal_Full_Qualified_Name;

   --  Start of processing for Full_Qualified_Name

   begin
      Res := Internal_Full_Qualified_Name (E);
      Store_String_Char (Get_Char_Code (ASCII.nul));
      return End_String;
   end Full_Qualified_Name;

   -----------------------
   -- Gather_Components --
   -----------------------

   procedure Gather_Components
     (Comp_List : Node_Id;
      Into      : Elist_Id)
   is
      Comp_Item : Node_Id;

   begin
      if No (Comp_List) or else Null_Present (Comp_List) then
         return;

      elsif Present (Component_Items (Comp_List)) then
         Comp_Item := First (Component_Items (Comp_List));

      else
         Comp_Item := Empty;
      end if;

      while Present (Comp_Item) loop

         --  Skip the tag of a tagged record, as well as all items
         --  that are not user components (anonymous types, rep clauses,
         --  Parent field, controller field).

         if Nkind (Comp_Item) = N_Component_Declaration
           and then Chars (Defining_Identifier (Comp_Item)) /= Name_uTag
           and then Chars (Defining_Identifier (Comp_Item)) /= Name_uParent
           and then Chars (Defining_Identifier (Comp_Item)) /= Name_uController
         then
            Append_Elmt (Defining_Identifier (Comp_Item), Into);
         end if;

         Next (Comp_Item);
      end loop;

      return;
   end Gather_Components;

   ------------------------
   -- Get_Actual_Subtype --
   ------------------------

   function Get_Actual_Subtype (N : Node_Id) return Entity_Id is
      Typ  : constant Entity_Id := Etype (N);
      Utyp : Entity_Id := Underlying_Type (Typ);

   begin
      if not Present (Utyp) then
         Utyp := Typ;
      end if;

      --  If what we have is an identifier that references a subprogram
      --  formal, or a variable or constant object, then we get the actual
      --  subtype from the referenced entity if one has been built.

      if Nkind (N) = N_Identifier
        and then
          (Is_Formal (Entity (N))
            or else Ekind (Entity (N)) = E_Constant
            or else Ekind (Entity (N)) = E_Variable)
        and then Present (Actual_Subtype (Entity (N)))
      then
         return Actual_Subtype (Entity (N));

      --  Checking the type, not the underlying type, for constrainedness
      --  seems to be necessary. Maybe all the tests should be on the type???
      else
	 return Typ;
      end if;
   end Get_Actual_Subtype;

   -------------------------------------
   -- Get_Actual_Subtype_If_Available --
   -------------------------------------

   function Get_Actual_Subtype_If_Available (N : Node_Id) return Entity_Id is
      Typ  : constant Entity_Id := Etype (N);

   begin
      --  If what we have is an identifier that references a subprogram
      --  formal, or a variable or constant object, then we get the actual
      --  subtype from the referenced entity if one has been built.

      if Nkind (N) = N_Identifier
        and then
          (Is_Formal (Entity (N))
            or else Ekind (Entity (N)) = E_Constant
            or else Ekind (Entity (N)) = E_Variable)
        and then Present (Actual_Subtype (Entity (N)))
      then
         return Actual_Subtype (Entity (N));

      --  Otherwise the Etype of N is returned unchanged

      else
         return Typ;
      end if;
   end Get_Actual_Subtype_If_Available;

   -------------------------------
   -- Get_Default_External_Name --
   -------------------------------

   function Get_Default_External_Name (E : Node_Or_Entity_Id) return Node_Id is
   begin
      Get_Decoded_Name_String (Chars (E));

      if Opt.External_Name_Imp_Casing = Uppercase then
         Set_Casing (All_Upper_Case);
      else
         Set_Casing (All_Lower_Case);
      end if;

      return
        Make_String_Literal (Sloc (E),
          Strval => String_From_Name_Buffer);
   end Get_Default_External_Name;

   ---------------------------
   -- Get_Enum_Lit_From_Pos --
   ---------------------------

   function Get_Enum_Lit_From_Pos
     (T   : Entity_Id;
      Pos : Uint;
      Loc : Source_Ptr) return Node_Id
   is
      Lit : Node_Id;
      P   : constant Nat := UI_To_Int (Pos);

   begin
      --  In the case where the literal is either of type Wide_Character
      --  or Character or of a type derived from them, there needs to be
      --  some special handling since there is no explicit chain of
      --  literals to search. Instead, an N_Character_Literal node is
      --  created with the appropriate Char_Code and Chars fields.

      if Root_Type (T) = Standard_Character
        or else Root_Type (T) = Standard_Wide_Character
      then
         Set_Character_Literal_Name (Char_Code (P));
         return
           Make_Character_Literal (Loc,
             Chars => Name_Find,
             Char_Literal_Value => Char_Code (P));

      --  For all other cases, we have a complete table of literals, and
      --  we simply iterate through the chain of literal until the one
      --  with the desired position value is found.
      --

      else
         Lit := First_Literal (Base_Type (T));
         for J in 1 .. P loop
            Next_Literal (Lit);
         end loop;

         return New_Occurrence_Of (Lit, Loc);
      end if;
   end Get_Enum_Lit_From_Pos;

   ------------------------
   -- Get_Generic_Entity --
   ------------------------

   function Get_Generic_Entity (N : Node_Id) return Entity_Id is
      Ent : constant Entity_Id := Entity (Name (N));

   begin
      if Present (Renamed_Object (Ent)) then
         return Renamed_Object (Ent);
      else
         return Ent;
      end if;
   end Get_Generic_Entity;

   ----------------------
   -- Get_Index_Bounds --
   ----------------------

   procedure Get_Index_Bounds (N : Node_Id; L, H : out Node_Id) is
      Kind : constant Node_Kind := Nkind (N);
      R    : Node_Id;

   begin
      if Kind = N_Range then
         L := Low_Bound (N);
         H := High_Bound (N);

      elsif Kind = N_Subtype_Indication then
         R := Range_Expression (Constraint (N));

         if R = Error then
            L := Error;
            H := Error;
            return;

         else
            L := Low_Bound  (Range_Expression (Constraint (N)));
            H := High_Bound (Range_Expression (Constraint (N)));
         end if;

      elsif Is_Entity_Name (N) and then Is_Type (Entity (N)) then
         if Error_Posted (Scalar_Range (Entity (N))) then
            L := Error;
            H := Error;

         elsif Nkind (Scalar_Range (Entity (N))) = N_Subtype_Indication then
            Get_Index_Bounds (Scalar_Range (Entity (N)), L, H);

         else
            L := Low_Bound  (Scalar_Range (Entity (N)));
            H := High_Bound (Scalar_Range (Entity (N)));
         end if;

      else
         --  N is an expression, indicating a range with one value.

         L := N;
         H := N;
      end if;
   end Get_Index_Bounds;

   ------------------------
   -- Get_Name_Entity_Id --
   ------------------------

   function Get_Name_Entity_Id (Id : Name_Id) return Entity_Id is
   begin
      return Entity_Id (Get_Name_Table_Info (Id));
   end Get_Name_Entity_Id;

   ---------------------------
   -- Get_Referenced_Object --
   ---------------------------

   function Get_Referenced_Object (N : Node_Id) return Node_Id is
      R   : Node_Id := N;

   begin
      while Is_Entity_Name (R)
        and then Present (Renamed_Object (Entity (R)))
      loop
         R := Renamed_Object (Entity (R));
      end loop;

      return R;
   end Get_Referenced_Object;

   -------------------------
   -- Get_Subprogram_Body --
   -------------------------

   function Get_Subprogram_Body (E : Entity_Id) return Node_Id is
      Decl : Node_Id;

   begin
      Decl := Unit_Declaration_Node (E);

      if Nkind (Decl) = N_Subprogram_Body then
         return Decl;

      else           --  Nkind (Decl) = N_Subprogram_Declaration

         if Present (Corresponding_Body (Decl)) then
            return Unit_Declaration_Node (Corresponding_Body (Decl));

         else        --  imported subprogram.
            return Empty;
         end if;
      end if;
   end Get_Subprogram_Body;

   --------------------
   -- Has_Infinities --
   --------------------

   function Has_Infinities (E : Entity_Id) return Boolean is
   begin
      return
        Is_Floating_Point_Type (E)
          and then Nkind (Scalar_Range (E)) = N_Range
          and then Includes_Infinities (Scalar_Range (E));
   end Has_Infinities;

   ------------------------
   -- Has_Null_Extension --
   ------------------------

   function Has_Null_Extension (T : Entity_Id) return Boolean is
      B     : constant Entity_Id := Base_Type (T);
      Comps : Node_Id;
      Ext   : Node_Id;

   begin
      if Nkind (Parent (B)) = N_Full_Type_Declaration
        and then Present (Record_Extension_Part (Type_Definition (Parent (B))))
      then
         Ext := Record_Extension_Part (Type_Definition (Parent (B)));

         if Present (Ext) then
            if Null_Present (Ext) then
               return True;
            else
               Comps := Component_List (Ext);

               --  The null component list is rewritten during analysis to
               --  include the parent component. Any other component indicates
               --  that the extension was not originally null.

               return Null_Present (Comps)
                 or else No (Next (First (Component_Items (Comps))));
            end if;
         else
            return False;
         end if;

      else
         return False;
      end if;
   end Has_Null_Extension;

   ---------------------------
   -- Has_Private_Component --
   ---------------------------

   function Has_Private_Component (Type_Id : Entity_Id) return Boolean is
      Btype     : Entity_Id := Base_Type (Type_Id);
      Component : Entity_Id;

   begin
      if Error_Posted (Type_Id)
        or else Error_Posted (Btype)
      then
         return False;
      end if;

      if Is_Class_Wide_Type (Btype) then
         Btype := Root_Type (Btype);
      end if;

      if Is_Private_Type (Btype) then
         declare
            UT : constant Entity_Id := Underlying_Type (Btype);
         begin
            if No (UT) then

               if No (Full_View (Btype)) then
                  return not Is_Generic_Type (Btype)
                    and then not Is_Generic_Type (Root_Type (Btype));

               else
                  return not Is_Generic_Type (Root_Type (Full_View (Btype)));
               end if;

            else
               return not Is_Frozen (UT) and then Has_Private_Component (UT);
            end if;
         end;
      elsif Is_Array_Type (Btype) then
         return Has_Private_Component (Component_Type (Btype));

      elsif Is_Record_Type (Btype) then

         Component := First_Component (Btype);
         while Present (Component) loop

            if Has_Private_Component (Etype (Component)) then
               return True;
            end if;

            Next_Component (Component);
         end loop;

         return False;

      else
         return False;
      end if;
   end Has_Private_Component;

   --------------------------
   -- Has_Tagged_Component --
   --------------------------

   function Has_Tagged_Component (Typ : Entity_Id) return Boolean is
      Comp : Entity_Id;

   begin
      if Is_Private_Type (Typ)
        and then Present (Underlying_Type (Typ))
      then
         return Has_Tagged_Component (Underlying_Type (Typ));

      elsif Is_Array_Type (Typ) then
         return Has_Tagged_Component (Component_Type (Typ));

      elsif Is_Tagged_Type (Typ) then
         return True;

      elsif Is_Record_Type (Typ) then
         Comp := First_Component (Typ);

         while Present (Comp) loop
            if Has_Tagged_Component (Etype (Comp)) then
               return True;
            end if;

            Comp := Next_Component (Typ);
         end loop;

         return False;

      else
         return False;
      end if;
   end Has_Tagged_Component;

   -----------------
   -- In_Instance --
   -----------------

   function In_Instance return Boolean is
      S : Entity_Id := Current_Scope;

   begin
      while Present (S)
        and then S /= Standard_Standard
      loop
         if (Ekind (S) = E_Function
              or else Ekind (S) = E_Package
              or else Ekind (S) = E_Procedure)
           and then Is_Generic_Instance (S)
         then
            return True;
         end if;

         S := Scope (S);
      end loop;

      return False;
   end In_Instance;

   ----------------------
   -- In_Instance_Body --
   ----------------------

   function In_Instance_Body return Boolean is
      S : Entity_Id := Current_Scope;

   begin
      while Present (S)
        and then S /= Standard_Standard
      loop
         if (Ekind (S) = E_Function
              or else Ekind (S) = E_Procedure)
           and then Is_Generic_Instance (S)
         then
            return True;

         elsif Ekind (S) = E_Package
           and then In_Package_Body (S)
           and then Is_Generic_Instance (S)
         then
            return True;
         end if;

         S := Scope (S);
      end loop;

      return False;
   end In_Instance_Body;

   -----------------------------
   -- In_Instance_Not_Visible --
   -----------------------------

   function In_Instance_Not_Visible return Boolean is
      S : Entity_Id := Current_Scope;

   begin
      while Present (S)
        and then S /= Standard_Standard
      loop
         if (Ekind (S) = E_Function
              or else Ekind (S) = E_Procedure)
           and then Is_Generic_Instance (S)
         then
            return True;

         elsif Ekind (S) = E_Package
           and then (In_Package_Body (S) or else In_Private_Part (S))
           and then Is_Generic_Instance (S)
         then
            return True;
         end if;

         S := Scope (S);
      end loop;

      return False;
   end In_Instance_Not_Visible;

   ------------------------------
   -- In_Instance_Visible_Part --
   ------------------------------

   function In_Instance_Visible_Part return Boolean is
      S : Entity_Id := Current_Scope;

   begin
      while Present (S)
        and then S /= Standard_Standard
      loop
         if Ekind (S) = E_Package
           and then Is_Generic_Instance (S)
           and then not In_Package_Body (S)
           and then not In_Private_Part (S)
         then
            return True;
         end if;

         S := Scope (S);
      end loop;

      return False;
   end In_Instance_Visible_Part;

   ----------------------
   -- In_Packiage_Body --
   ----------------------

   function In_Package_Body return Boolean is
      S : Entity_Id := Current_Scope;

   begin
      while Present (S)
        and then S /= Standard_Standard
      loop
         if Ekind (S) = E_Package
           and then In_Package_Body (S)
         then
            return True;
         else
            S := Scope (S);
         end if;
      end loop;

      return False;
   end In_Package_Body;

   --------------------------------------
   -- In_Subprogram_Or_Concurrent_Unit --
   --------------------------------------

   function In_Subprogram_Or_Concurrent_Unit return Boolean is
      E : Entity_Id;
      K : Entity_Kind;

   begin
      --  Use scope chain to check successively outer scopes

      E := Current_Scope;
      loop
         K := Ekind (E);

         if K in Subprogram_Kind
           or else K in Generic_Subprogram_Kind
         then
            return True;

         elsif E = Standard_Standard then
            return False;
         end if;

         E := Scope (E);
      end loop;
   end In_Subprogram_Or_Concurrent_Unit;

   ---------------------
   -- In_Visible_Part --
   ---------------------

   function In_Visible_Part (Scope_Id : Entity_Id) return Boolean is
   begin
      return
        Is_Package (Scope_Id)
          and then In_Open_Scopes (Scope_Id)
          and then not In_Package_Body (Scope_Id)
          and then not In_Private_Part (Scope_Id);
   end In_Visible_Part;

   ---------------------------------
   -- Insert_Explicit_Dereference --
   ---------------------------------

   procedure Insert_Explicit_Dereference (N : Node_Id) is
      New_Prefix : constant Node_Id := Relocate_Node (N);
      I          : Interp_Index;
      It         : Interp;
      T          : Entity_Id;

   begin
      Save_Interps (N, New_Prefix);
      Rewrite (N,
        Make_Explicit_Dereference (Sloc (N), Prefix => New_Prefix));

      Set_Etype (N, Designated_Type (Etype (New_Prefix)));

      if Is_Overloaded (New_Prefix) then

         --  The deference is also overloaded, and its interpretations are the
         --  designated types of the interpretations of the original node.

         Set_Etype (N, Any_Type);
         Get_First_Interp (New_Prefix, I, It);

         while Present (It.Nam) loop
            T := It.Typ;

            if Is_Access_Type (T) then
               Add_One_Interp (N, Designated_Type (T), Designated_Type (T));
            end if;

            Get_Next_Interp (I, It);
         end loop;

         End_Interp_List;
      end if;
   end Insert_Explicit_Dereference;

   -------------------
   -- Is_AAMP_Float --
   -------------------

   function Is_AAMP_Float (E : Entity_Id) return Boolean is
   begin
      pragma Assert (Is_Type (E));

      return AAMP_On_Target
         and then Is_Floating_Point_Type (E)
         and then E = Base_Type (E);
   end Is_AAMP_Float;

   -------------------------
   -- Is_Actual_Parameter --
   -------------------------

   function Is_Actual_Parameter (N : Node_Id) return Boolean is
      PK : constant Node_Kind := Nkind (Parent (N));

   begin
      case PK is
         when N_Parameter_Association =>
            return N = Explicit_Actual_Parameter (Parent (N));

         when N_Function_Call | N_Procedure_Call_Statement =>
            return Is_List_Member (N)
              and then
                List_Containing (N) = Parameter_Associations (Parent (N));

         when others =>
            return False;
      end case;
   end Is_Actual_Parameter;

   ---------------------
   -- Is_Aliased_View --
   ---------------------

   function Is_Aliased_View (Obj : Node_Id) return Boolean is
      E : Entity_Id;

   begin
      if Is_Entity_Name (Obj) then

         --  Shouldn't we check that we really have an object here?
         --  If we do, then a-caldel.adb blows up mysteriously ???

         E := Entity (Obj);

         return Is_Aliased (E)
           or else (Present (Renamed_Object (E))
                     and then Is_Aliased_View (Renamed_Object (E)))

           or else ((Is_Formal (E)
                      or else Ekind (E) = E_Generic_In_Out_Parameter
                      or else Ekind (E) = E_Generic_In_Parameter)
                    and then Is_Tagged_Type (Etype (E)))

            --  Current instance of type

           or else (Is_Type (E) and then E = Current_Scope)
           or else (Is_Incomplete_Or_Private_Type (E)
                     and then Full_View (E) = Current_Scope);

      elsif Nkind (Obj) = N_Selected_Component then
         return Is_Aliased (Entity (Selector_Name (Obj)));

      elsif Nkind (Obj) = N_Indexed_Component then
         return Has_Aliased_Components (Etype (Prefix (Obj)))
           or else
             (Is_Access_Type (Etype (Prefix (Obj)))
               and then
              Has_Aliased_Components
                (Designated_Type (Etype (Prefix (Obj)))));

      elsif Nkind (Obj) = N_Unchecked_Type_Conversion
        or else Nkind (Obj) = N_Type_Conversion
      then
         return Is_Tagged_Type (Etype (Obj))
           and then Is_Aliased_View (Expression (Obj));

      elsif Nkind (Obj) = N_Explicit_Dereference then
         return Nkind (Original_Node (Obj)) /= N_Function_Call;

      else
         return False;
      end if;
   end Is_Aliased_View;

   ----------------------
   -- Is_Atomic_Object --
   ----------------------

--     function Is_Atomic_Object (N : Node_Id) return Boolean is
--
--        function Object_Has_Atomic_Components (N : Node_Id) return Boolean;
--        --  Determines if given object has atomic components
--
--        function Is_Atomic_Prefix (N : Node_Id) return Boolean;
--        --  If prefix is an implicit dereference, examine designated type.
--
--        function Is_Atomic_Prefix (N : Node_Id) return Boolean is
--        begin
--           if Is_Access_Type (Etype (N)) then
--              return
--                Has_Atomic_Components (Designated_Type (Etype (N)));
--           else
--              return Object_Has_Atomic_Components (N);
--           end if;
--        end Is_Atomic_Prefix;

--        function Object_Has_Atomic_Components (N : Node_Id) return Boolean is
--        begin
--           if Has_Atomic_Components (Etype (N))
--             or else Is_Atomic (Etype (N))
--           then
--              return True;
--
--           elsif Is_Entity_Name (N)
--             and then (Has_Atomic_Components (Entity (N))
--                        or else Is_Atomic (Entity (N)))
--           then
--              return True;
--
--           elsif Nkind (N) = N_Indexed_Component
--             or else Nkind (N) = N_Selected_Component
--           then
--              return Is_Atomic_Prefix (Prefix (N));
--
--           else
--              return False;
--           end if;
--        end Object_Has_Atomic_Components;

   --  Start of processing for Is_Atomic_Object

--     begin
--        if Is_Atomic (Etype (N))
--          or else (Is_Entity_Name (N) and then Is_Atomic (Entity (N)))
--        then
--           return True;
--
--        elsif Nkind (N) = N_Indexed_Component
--          or else Nkind (N) = N_Selected_Component
--        then
--           return Is_Atomic_Prefix (Prefix (N));
--
--        else
--           return False;
--        end if;
--     end Is_Atomic_Object;

   ----------------------------------------------
   -- Is_Dependent_Component_Of_Mutable_Object --
   ----------------------------------------------

   function Is_Dependent_Component_Of_Mutable_Object
     (Object : Node_Id) return   Boolean
   is
      P           : Node_Id;
      Prefix_Type : Entity_Id;
      P_Aliased   : Boolean := False;
      Comp        : Entity_Id;

      function Has_Dependent_Constraint (Comp : Entity_Id) return Boolean;
      --  Returns True if and only if Comp has a constrained subtype
      --  that depends on a discriminant.

      ------------------------------
      -- Has_Dependent_Constraint --
      ------------------------------

      function Has_Dependent_Constraint (Comp : Entity_Id) return Boolean is
         Comp_Decl  : constant Node_Id := Parent (Comp);
         Subt_Indic : constant Node_Id :=
                        Subtype_Indication (Component_Definition (Comp_Decl));

      begin
         return False;
      end Has_Dependent_Constraint;

   --  Start of processing for Is_Dependent_Component_Of_Mutable_Object

   begin
      if Is_Variable (Object) then

         if Nkind (Object) = N_Selected_Component then
            P := Prefix (Object);
            Prefix_Type := Etype (P);

            if Is_Entity_Name (P) then

               if Ekind (Entity (P)) = E_Generic_In_Out_Parameter then
                  Prefix_Type := Base_Type (Prefix_Type);
               end if;

               if Is_Aliased (Entity (P)) then
                  P_Aliased := True;
               end if;

            else
               --  Check for prefix being an aliased component ???
               null;
            end if;

            if Is_Access_Type (Prefix_Type)
              or else Nkind (P) = N_Explicit_Dereference
            then
               return False;
            end if;

            Comp :=
              Original_Record_Component (Entity (Selector_Name (Object)));

            --  As per AI-0017, the renaming is illegal in a generic body,
            --  even if the subtype is indefinite.

            if not Is_Constrained (Prefix_Type)
              and then (not Is_Indefinite_Subtype (Prefix_Type)
                         or else
                          (Is_Generic_Type (Prefix_Type)
                            and then Ekind (Current_Scope) = E_Generic_Package
                            and then In_Package_Body (Current_Scope)))

              and then (Has_Dependent_Constraint (Comp))
              and then not P_Aliased
            then
               return True;

            else
               return
                 Is_Dependent_Component_Of_Mutable_Object (Prefix (Object));

            end if;

         elsif Nkind (Object) = N_Indexed_Component
           or else Nkind (Object) = N_Slice
         then
            return Is_Dependent_Component_Of_Mutable_Object (Prefix (Object));
         end if;
      end if;

      return False;
   end Is_Dependent_Component_Of_Mutable_Object;

   ---------------------
   -- Is_Dereferenced --
   ---------------------

   function Is_Dereferenced (N : Node_Id) return Boolean is
      P : constant Node_Id := Parent (N);

   begin
      return
         (Nkind (P) = N_Selected_Component
            or else
          Nkind (P) = N_Explicit_Dereference
            or else
          Nkind (P) = N_Indexed_Component
            or else
          Nkind (P) = N_Slice)
        and then Prefix (P) = N;
   end Is_Dereferenced;

   --------------
   -- Is_False --
   --------------

   function Is_False (U : Uint) return Boolean is
   begin
      return (U = 0);
   end Is_False;

   -------------------------------
   -- Is_Fully_Initialized_Type --
   -------------------------------

   function Is_Fully_Initialized_Type (Typ : Entity_Id) return Boolean is
   begin
      if Is_Scalar_Type (Typ) then
         return False;

      elsif Is_Access_Type (Typ) then
         return True;

      elsif Is_Array_Type (Typ) then
         if Is_Fully_Initialized_Type (Component_Type (Typ)) then
            return True;
         end if;

         --  An interesting case, if we have a constrained type one of whose
         --  bounds is known to be null, then there are no elements to be
         --  initialized, so all the elements are initialized!

         if Is_Constrained (Typ) then
            declare
               Indx     : Node_Id;
               Indx_Typ : Entity_Id;
               Lbd, Hbd : Node_Id;

            begin
               Indx := First_Index (Typ);
               while Present (Indx) loop

                  if Etype (Indx) = Any_Type then
                     return False;

                  --  If index is a range, use directly.

                  elsif Nkind (Indx) = N_Range then
                     Lbd := Low_Bound  (Indx);
                     Hbd := High_Bound (Indx);

                  else
                     Indx_Typ := Etype (Indx);

                     if Is_Private_Type (Indx_Typ)  then
                        Indx_Typ := Full_View (Indx_Typ);
                     end if;

                     if No (Indx_Typ) then
                        return False;
                     else
                        Lbd := Type_Low_Bound  (Indx_Typ);
                        Hbd := Type_High_Bound (Indx_Typ);
                     end if;
                  end if;

                  if Compile_Time_Known_Value (Lbd)
                    and then Compile_Time_Known_Value (Hbd)
                  then
                     if Expr_Value (Hbd) < Expr_Value (Lbd) then
                        return True;
                     end if;
                  end if;

                  Next_Index (Indx);
               end loop;
            end;
         end if;

         --  If no null indexes, then type is not fully initialized

         return False;

      --  Record types

      elsif Is_Record_Type (Typ) then
         --  Controlled records are considered to be fully initialized if
         --  there is a user defined Initialize routine. This may not be
         --  entirely correct, but as the spec notes, we are guessing here
         --  what is best from the point of view of issuing warnings.

         if Is_Controlled (Typ) then
            declare
               Utyp : constant Entity_Id := Underlying_Type (Typ);

            begin
               if Present (Utyp) then
                  declare
                     Init : constant Entity_Id :=
                              (Find_Prim_Op
                                 (Underlying_Type (Typ), Name_Initialize));

                  begin
                     if Present (Init)
                       and then Comes_From_Source (Init)
                       and then not
                         Is_Predefined_File_Name
                           (File_Name (Get_Source_File_Index (Sloc (Init))))
                     then
                        return True;

                     elsif Has_Null_Extension (Typ)
                        and then
                          Is_Fully_Initialized_Type
                            (Etype (Base_Type (Typ)))
                     then
                        return True;
                     end if;
                  end;
               end if;
            end;
         end if;

         --  Otherwise see if all record components are initialized

         declare
            Ent : Entity_Id;

         begin
            Ent := First_Entity (Typ);

            while Present (Ent) loop
               if Chars (Ent) = Name_uController then
                  null;

               elsif Ekind (Ent) = E_Component
                 and then (No (Parent (Ent))
                             or else No (Expression (Parent (Ent))))
                 and then not Is_Fully_Initialized_Type (Etype (Ent))
               then
                  return False;
               end if;

               Next_Entity (Ent);
            end loop;
         end;

         --  No uninitialized components, so type is fully initialized.
         --  Note that this catches the case of no components as well.

         return True;

      elsif Is_Private_Type (Typ) then
         declare
            U : constant Entity_Id := Underlying_Type (Typ);

         begin
            if No (U) then
               return False;
            else
               return Is_Fully_Initialized_Type (U);
            end if;
         end;

      else
         return False;
      end if;
   end Is_Fully_Initialized_Type;

   ----------------------------
   -- Is_Inherited_Operation --
   ----------------------------

   function Is_Inherited_Operation (E : Entity_Id) return Boolean is
      Kind : constant Node_Kind := Nkind (Parent (E));

   begin
      pragma Assert (Is_Overloadable (E));
      return Kind = N_Full_Type_Declaration
        or else Kind = N_Private_Extension_Declaration
        or else Kind = N_Subtype_Declaration
        or else (Ekind (E) = E_Enumeration_Literal
                  and then Is_Derived_Type (Etype (E)));
   end Is_Inherited_Operation;

   -----------------------------
   -- Is_Library_Level_Entity --
   -----------------------------

   function Is_Library_Level_Entity (E : Entity_Id) return Boolean is
   begin
      --  The following is a small optimization, and it also handles
      --  properly discriminals, which in task bodies might appear in
      --  expressions before the corresponding procedure has been
      --  created, and which therefore do not have an assigned scope.

      if Ekind (E) in Formal_Kind then
         return False;
      end if;

      --  Normal test is simply that the enclosing dynamic scope is Standard

      return Enclosing_Dynamic_Scope (E) = Standard_Standard;
   end Is_Library_Level_Entity;

   ---------------------------------
   -- Is_Local_Variable_Reference --
   ---------------------------------

   function Is_Local_Variable_Reference (Expr : Node_Id) return Boolean is
   begin
      if not Is_Entity_Name (Expr) then
         return False;

      else
         declare
            Ent : constant Entity_Id := Entity (Expr);
            Sub : constant Entity_Id := Enclosing_Subprogram (Ent);

         begin
            if Ekind (Ent) /= E_Variable
                 and then
               Ekind (Ent) /= E_In_Out_Parameter
            then
               return False;

            else
               return Present (Sub) and then Sub = Current_Subprogram;
            end if;
         end;
      end if;
   end Is_Local_Variable_Reference;

   ---------------
   -- Is_Lvalue --
   ---------------

   function Is_Lvalue (N : Node_Id) return Boolean is
      P : constant Node_Id := Parent (N);

   begin
      case Nkind (P) is

         --  Test left side of assignment

         when N_Assignment_Statement =>
            return N = Name (P);

         --  Test prefix of component or attribute

         when N_Attribute_Reference  |
              N_Expanded_Name        |
              N_Explicit_Dereference |
              N_Indexed_Component    |
              N_Reference            |
              N_Selected_Component   |
              N_Slice                =>
            return N = Prefix (P);

         --  Test subprogram parameter (we really should check the
         --  parameter mode, but it is not worth the trouble)

         when N_Function_Call            |
              N_Procedure_Call_Statement |
              N_Parameter_Association    =>
            return True;

         --  Test for appearing in a conversion that itself appears
         --  in an lvalue context, since this should be an lvalue.

         when N_Type_Conversion =>
            return Is_Lvalue (P);

         --  Test for appearence in object renaming declaration

         when N_Object_Renaming_Declaration =>
            return True;

         --  All other references are definitely not Lvalues

         when others =>
            return False;

      end case;
   end Is_Lvalue;

   -------------------------
   -- Is_Object_Reference --
   -------------------------

   function Is_Object_Reference (N : Node_Id) return Boolean is
   begin
      if Is_Entity_Name (N) then
         return Is_Object (Entity (N));

      else
         case Nkind (N) is
            when N_Indexed_Component | N_Slice =>
               return Is_Object_Reference (Prefix (N));

            --  In Ada95, a function call is a constant object

            when N_Function_Call =>
               return True;

            when N_Selected_Component =>
               return Is_Object_Reference (Selector_Name (N));

            when N_Explicit_Dereference =>
               return True;

            --  An unchecked type conversion is considered to be an object if
            --  the operand is an object (this construction arises only as a
            --  result of expansion activities).

            when N_Unchecked_Type_Conversion =>
               return True;

            when others =>
               return False;
         end case;
      end if;
   end Is_Object_Reference;

   -----------------------------------
   -- Is_OK_Variable_For_Out_Formal --
   -----------------------------------

   function Is_OK_Variable_For_Out_Formal (AV : Node_Id) return Boolean is
   begin
      Note_Possible_Modification (AV);

      --  We must reject parenthesized variable names. The check for
      --  Comes_From_Source is present because there are currently
      --  cases where the compiler violates this rule (e.g. passing
      --  a task object to its controlled Initialize routine).

      if Paren_Count (AV) > 0 and then Comes_From_Source (AV) then
         return False;

      --  A variable is always allowed

      elsif Is_Variable (AV) then
         return True;

      --  Unchecked conversions are allowed only if they come from the
      --  generated code, which sometimes uses unchecked conversions for
      --  out parameters in cases where code generation is unaffected.
      --  We tell source unchecked conversions by seeing if they are
      --  rewrites of an original UC function call, or of an explicit
      --  conversion of a function call.

      elsif Nkind (AV) = N_Unchecked_Type_Conversion then
         if Nkind (Original_Node (AV)) = N_Function_Call then
            return False;

         elsif Comes_From_Source (AV)
           and then Nkind (Original_Node (Expression (AV))) = N_Function_Call
         then
            return False;

         else
            return True;
         end if;

      --  Normal type conversions are allowed if argument is a variable

      elsif Nkind (AV) = N_Type_Conversion then
         if Is_Variable (Expression (AV))
           and then Paren_Count (Expression (AV)) = 0
         then
            Note_Possible_Modification (Expression (AV));
            return True;

         --  We also allow a non-parenthesized expression that raises
         --  constraint error if it rewrites what used to be a variable

         elsif Raises_Constraint_Error (Expression (AV))
            and then Paren_Count (Expression (AV)) = 0
            and then Is_Variable (Original_Node (Expression (AV)))
         then
            return True;

         --  Type conversion of something other than a variable

         else
            return False;
         end if;

      --  If this node is rewritten, then test the original form, if that is
      --  OK, then we consider the rewritten node OK (for example, if the
      --  original node is a conversion, then Is_Variable will not be true
      --  but we still want to allow the conversion if it converts a variable).

      elsif Original_Node (AV) /= AV then
         return Is_OK_Variable_For_Out_Formal (Original_Node (AV));

      --  All other non-variables are rejected

      else
         return False;
      end if;
   end Is_OK_Variable_For_Out_Formal;

   -----------------------------------
   -- Is_Partially_Initialized_Type --
   -----------------------------------

   function Is_Partially_Initialized_Type (Typ : Entity_Id) return Boolean is
   begin
      if Is_Scalar_Type (Typ) then
         return False;

      elsif Is_Access_Type (Typ) then
         return True;

      elsif Is_Array_Type (Typ) then

         --  If component type is partially initialized, so is array type

         if Is_Partially_Initialized_Type (Component_Type (Typ)) then
            return True;

         --  Otherwise we are only partially initialized if we are fully
         --  initialized (this is the empty array case, no point in us
         --  duplicating that code here).

         else
            return Is_Fully_Initialized_Type (Typ);
         end if;

      elsif Is_Record_Type (Typ) then

         --  A tagged type is always partially initialized

         if Is_Tagged_Type (Typ) then
            return True;

         --  Case of non-discriminated record

         else
            declare
               Ent : Entity_Id;

               Component_Present : Boolean := False;
               --  Set True if at least one component is present. If no
               --  components are present, then record type is fully
               --  initialized (another odd case, like the null array).

            begin
               --  Loop through components

               Ent := First_Entity (Typ);
               while Present (Ent) loop
                  if Ekind (Ent) = E_Component then
                     Component_Present := True;

                     --  If a component has an initialization expression then
                     --  the enclosing record type is partially initialized

                     if Present (Parent (Ent))
                       and then Present (Expression (Parent (Ent)))
                     then
                        return True;

                     --  If a component is of a type which is itself partially
                     --  initialized, then the enclosing record type is also.

                     elsif Is_Partially_Initialized_Type (Etype (Ent)) then
                        return True;
                     end if;
                  end if;

                  Next_Entity (Ent);
               end loop;

               --  No initialized components found. If we found any components
               --  they were all uninitialized so the result is false.

               if Component_Present then
                  return False;

               --  But if we found no components, then all the components are
               --  initialized so we consider the type to be initialized.

               else
                  return True;
               end if;
            end;
         end if;

      --  For a private type, go to underlying type. If there is no underlying
      --  type then just assume this partially initialized. Not clear if this
      --  can happen in a non-error case, but no harm in testing for this.

      elsif Is_Private_Type (Typ) then
         declare
            U : constant Entity_Id := Underlying_Type (Typ);

         begin
            if No (U) then
               return True;
            else
               return Is_Partially_Initialized_Type (U);
            end if;
         end;

      --  For any other type (are there any?) assume partially initialized

      else
         return True;
      end if;
   end Is_Partially_Initialized_Type;

   -----------------------------
   -- Is_RCI_Pkg_Spec_Or_Body --
   -----------------------------

   function Is_RCI_Pkg_Spec_Or_Body (Cunit : Node_Id) return Boolean is

      function Is_RCI_Pkg_Decl_Cunit (Cunit : Node_Id) return Boolean;
      --  Return True if the unit of Cunit is an RCI package declaration

      ---------------------------
      -- Is_RCI_Pkg_Decl_Cunit --
      ---------------------------

      function Is_RCI_Pkg_Decl_Cunit (Cunit : Node_Id) return Boolean is
         The_Unit : constant Node_Id := Unit (Cunit);

      begin
         return False;
      end Is_RCI_Pkg_Decl_Cunit;

   --  Start of processing for Is_RCI_Pkg_Spec_Or_Body

   begin
      return Is_RCI_Pkg_Decl_Cunit (Cunit)
        or else
         (Nkind (Unit (Cunit)) = N_Package_Body
           and then Is_RCI_Pkg_Decl_Cunit (Library_Unit (Cunit)));
   end Is_RCI_Pkg_Spec_Or_Body;

   ----------------------
   -- Is_Selector_Name --
   ----------------------

   function Is_Selector_Name (N : Node_Id) return Boolean is

   begin
      if not Is_List_Member (N) then
         declare
            P : constant Node_Id   := Parent (N);
            K : constant Node_Kind := Nkind (P);

         begin
            return
              (K = N_Expanded_Name          or else
               K = N_Generic_Association    or else
               K = N_Parameter_Association  or else
               K = N_Selected_Component)
              and then Selector_Name (P) = N;
         end;

      else
         declare
            L : constant List_Id := List_Containing (N);
            P : constant Node_Id := Parent (L);

         begin
            return (Nkind (P) = N_Discriminant_Association
                     and then Selector_Names (P) = L)
              or else
                   (Nkind (P) = N_Component_Association
                     and then Choices (P) = L);
         end;
      end if;
   end Is_Selector_Name;

   ------------------
   -- Is_Statement --
   ------------------

   function Is_Statement (N : Node_Id) return Boolean is
   begin
      return
        Nkind (N) in N_Statement_Other_Than_Procedure_Call
          or else Nkind (N) = N_Procedure_Call_Statement;
   end Is_Statement;

   -----------------
   -- Is_Transfer --
   -----------------

   function Is_Transfer (N : Node_Id) return Boolean is
      Kind : constant Node_Kind := Nkind (N);

   begin
      if Kind = N_Return_Statement
           or else
         Kind = N_Goto_Statement
      then
         return True;

      elsif Kind = N_Exit_Statement
        and then No (Condition (N))
      then
         return True;

      elsif Kind = N_Procedure_Call_Statement
        and then Is_Entity_Name (Name (N))
        and then Present (Entity (Name (N)))
        and then No_Return (Entity (Name (N)))
      then
         return True;

      else
         return False;
      end if;
   end Is_Transfer;

   -------------
   -- Is_True --
   -------------

   function Is_True (U : Uint) return Boolean is
   begin
      return (U /= 0);
   end Is_True;

   -----------------
   -- Is_Variable --
   -----------------

   function Is_Variable (N : Node_Id) return Boolean is

      Orig_Node : constant Node_Id := Original_Node (N);
      --  We do the test on the original node, since this is basically a
      --  test of syntactic categories, so it must not be disturbed by
      --  whatever rewriting might have occurred. For example, an aggregate,
      --  which is certainly NOT a variable, could be turned into a variable
      --  by expansion.

      function Is_Variable_Prefix (P : Node_Id) return Boolean;
      --  Prefixes can involve implicit dereferences, in which case we
      --  must test for the case of a reference of a constant access
      --  type, which can never be a variable.

      ------------------------
      -- Is_Variable_Prefix --
      ------------------------

      function Is_Variable_Prefix (P : Node_Id) return Boolean is
      begin
         if Is_Access_Type (Etype (P)) then
            return not Is_Access_Constant (Root_Type (Etype (P)));
         else
            return Is_Variable (P);
         end if;
      end Is_Variable_Prefix;

   --  Start of processing for Is_Variable

   begin
      --  Definitely OK if Assignment_OK is set. Since this is something that
      --  only gets set for expanded nodes, the test is on N, not Orig_Node.

      if Nkind (N) in N_Subexpr and then Assignment_OK (N) then
         return True;

      --  Normally we go to the original node, but there is one exception
      --  where we use the rewritten node, namely when it is an explicit
      --  dereference. The generated code may rewrite a prefix which is an
      --  access type with an explicit dereference. The dereference is a
      --  variable, even though the original node may not be (since it could
      --  be a constant of the access type).

      elsif Nkind (N) = N_Explicit_Dereference
        and then Nkind (Orig_Node) /= N_Explicit_Dereference
        and then Is_Access_Type (Etype (Orig_Node))
      then
         return Is_Variable_Prefix (Original_Node (Prefix (N)));

      --  All remaining checks use the original node

      elsif Is_Entity_Name (Orig_Node) then
         declare
            E : constant Entity_Id := Entity (Orig_Node);
            K : constant Entity_Kind := Ekind (E);

         begin
            return K = E_Variable
              or else  K = E_Component
              or else  K = E_Out_Parameter
              or else  K = E_In_Out_Parameter
              or else  K = E_Generic_In_Out_Parameter

               --  Current instance of type:

              or else (Is_Type (E) and then In_Open_Scopes (E))
              or else (Is_Incomplete_Or_Private_Type (E)
                        and then In_Open_Scopes (Full_View (E)));
         end;

      else
         case Nkind (Orig_Node) is
            when N_Indexed_Component | N_Slice =>
               return Is_Variable_Prefix (Prefix (Orig_Node));

            when N_Selected_Component =>
               return Is_Variable_Prefix (Prefix (Orig_Node))
                 and then Is_Variable (Selector_Name (Orig_Node));

            --  For an explicit dereference, the type of the prefix cannot
            --  be an access to constant or an access to subprogram.

            when N_Explicit_Dereference =>
               declare
                  Typ : constant Entity_Id := Etype (Prefix (Orig_Node));

               begin
                  return Is_Access_Type (Typ)
                    and then not Is_Access_Constant (Root_Type (Typ))
                    and then Ekind (Typ) /= E_Access_Subprogram_Type;
               end;

            --  The type conversion is the case where we do not deal with the
            --  context dependent special case of an actual parameter. Thus
            --  the type conversion is only considered a variable for the
            --  purposes of this routine if the target type is tagged. However,
            --  a type conversion is considered to be a variable if it does not
            --  come from source (this deals for example with the conversions
            --  of expressions to their actual subtypes).

            when N_Type_Conversion =>
               return Is_Variable (Expression (Orig_Node))
                 and then
                   (not Comes_From_Source (Orig_Node)
                      or else
                        (Is_Tagged_Type (Etype (Subtype_Mark (Orig_Node)))
                          and then
                         Is_Tagged_Type (Etype (Expression (Orig_Node)))));

            --  GNAT allows an unchecked type conversion as a variable. This
            --  only affects the generation of internal expanded code, since
            --  calls to instantiations of Unchecked_Conversion are never
            --  considered variables (since they are function calls).
            --  This is also true for expression actions.

            when N_Unchecked_Type_Conversion =>
               return Is_Variable (Expression (Orig_Node));

            when others =>
               return False;
         end case;
      end if;
   end Is_Variable;

   ------------------------
   -- Is_Volatile_Object --
   ------------------------

   function Is_Volatile_Object (N : Node_Id) return Boolean is

      function Object_Has_Volatile_Components (N : Node_Id) return Boolean;
      --  Determines if given object has volatile components

      function Is_Volatile_Prefix (N : Node_Id) return Boolean;
      --  If prefix is an implicit dereference, examine designated type.

      ------------------------
      -- Is_Volatile_Prefix --
      ------------------------

      function Is_Volatile_Prefix (N : Node_Id) return Boolean is
         Typ  : constant Entity_Id := Etype (N);

      begin
         if Is_Access_Type (Typ) then
            declare
               Dtyp : constant Entity_Id := Designated_Type (Typ);

            begin
               return Is_Volatile (Dtyp)
                 or else Has_Volatile_Components (Dtyp);
            end;

         else
            return Object_Has_Volatile_Components (N);
         end if;
      end Is_Volatile_Prefix;

      ------------------------------------
      -- Object_Has_Volatile_Components --
      ------------------------------------

      function Object_Has_Volatile_Components (N : Node_Id) return Boolean is
         Typ : constant Entity_Id := Etype (N);

      begin
         if Is_Volatile (Typ)
           or else Has_Volatile_Components (Typ)
         then
            return True;

         elsif Is_Entity_Name (N)
           and then (Has_Volatile_Components (Entity (N))
                      or else Is_Volatile (Entity (N)))
         then
            return True;

         elsif Nkind (N) = N_Indexed_Component
           or else Nkind (N) = N_Selected_Component
         then
            return Is_Volatile_Prefix (Prefix (N));

         else
            return False;
         end if;
      end Object_Has_Volatile_Components;

   --  Start of processing for Is_Volatile_Object

   begin
      if Is_Volatile (Etype (N))
        or else (Is_Entity_Name (N) and then Is_Volatile (Entity (N)))
      then
         return True;

      elsif Nkind (N) = N_Indexed_Component
        or else Nkind (N) = N_Selected_Component
      then
         return Is_Volatile_Prefix (Prefix (N));

      else
         return False;
      end if;
   end Is_Volatile_Object;

   -------------------------
   -- Kill_Current_Values --
   -------------------------

   procedure Kill_Current_Values is
      S : Entity_Id;

      procedure Kill_Current_Values_For_Entity_Chain (E : Entity_Id);
      --  Clear current value for entity E and all entities chained to E

      -------------------------------------------
      --  Kill_Current_Values_For_Entity_Chain --
      -------------------------------------------

      procedure Kill_Current_Values_For_Entity_Chain (E : Entity_Id) is
         Ent : Entity_Id;

      begin
         Ent := E;
         while Present (Ent) loop
            if Is_Object (Ent) then
               Set_Current_Value (Ent, Empty);

               if not Can_Never_Be_Null (Ent) then
                  Set_Is_Known_Non_Null (Ent, False);
               end if;
            end if;

            Next_Entity (Ent);
         end loop;
      end Kill_Current_Values_For_Entity_Chain;

   --  Start of processing for Kill_Current_Values

   begin
      --  Kill all saved checks, a special case of killing saved values

      --Kill_All_Checks;

      --  Loop through relevant scopes, which includes the current scope and
      --  any parent scopes if the current scope is a block or a package.

      S := Current_Scope;
      Scope_Loop : loop

         --  Clear current values of all entities in current scope

         Kill_Current_Values_For_Entity_Chain (First_Entity (S));

         --  If scope is a package, also clear current values of all
         --  private entities in the scope.

         if Ekind (S) = E_Package
              or else
            Ekind (S) = E_Generic_Package
         then
            Kill_Current_Values_For_Entity_Chain (First_Private_Entity (S));
         end if;

         --  If this is a block or nested package, deal with parent

         if Ekind (S) = E_Block
           or else (Ekind (S) = E_Package
                      and then not Is_Library_Level_Entity (S))
         then
            S := Scope (S);
         else
            exit Scope_Loop;
         end if;
      end loop Scope_Loop;
   end Kill_Current_Values;

   --------------------------
   -- Kill_Size_Check_Code --
   --------------------------

   procedure Kill_Size_Check_Code (E : Entity_Id) is
   begin
      if (Ekind (E) = E_Constant or else Ekind (E) = E_Variable)
        and then Present (Size_Check_Code (E))
      then
         Remove (Size_Check_Code (E));
         Set_Size_Check_Code (E, Empty);
      end if;
   end Kill_Size_Check_Code;

   ------------------------
   -- New_Copy_List_Tree --
   ------------------------

   function New_Copy_List_Tree (List : List_Id) return List_Id is
      NL : List_Id;
      E  : Node_Id;

   begin
      if List = No_List then
         return No_List;

      else
         NL := New_List;
         E := First (List);

         while Present (E) loop
            Append (New_Copy_Tree (E), NL);
            E := Next (E);
         end loop;

         return NL;
      end if;
   end New_Copy_List_Tree;

   --------------------------------------------------
   -- New_Copy_Tree Auxiliary Data and Subprograms --
   --------------------------------------------------

   use Atree.Unchecked_Access;
   use Atree_Private_Part;

   --  Our approach here requires a two pass traversal of the tree. The
   --  first pass visits all nodes that eventually will be copied looking
   --  for defining Itypes. If any defining Itypes are found, then they are
   --  copied, and an entry is added to the replacement map. In the second
   --  phase, the tree is copied, using the replacement map to replace any
   --  Itype references within the copied tree.

   --  The following hash tables are used if the Map supplied has more
   --  than hash threshold entries to speed up access to the map. If
   --  there are fewer entries, then the map is searched sequentially
   --  (because setting up a hash table for only a few entries takes
   --  more time than it saves.

   function New_Copy_Hash (E : Entity_Id) return NCT_Header_Num;
   --  Hash function used for hash operations

   -------------------
   -- New_Copy_Hash --
   -------------------

   function New_Copy_Hash (E : Entity_Id) return NCT_Header_Num is
   begin
      return Nat (E) mod (NCT_Header_Num'Last + 1);
   end New_Copy_Hash;

   ---------------
   -- NCT_Assoc --
   ---------------

   --  The hash table NCT_Assoc associates old entities in the table
   --  with their corresponding new entities (i.e. the pairs of entries
   --  presented in the original Map argument are Key-Element pairs).

   package NCT_Assoc is new Simple_HTable (
     Header_Num => NCT_Header_Num,
     Element    => Entity_Id,
     No_Element => Empty,
     Key        => Entity_Id,
     Hash       => New_Copy_Hash,
     Equal      => Types."=");

   ---------------------
   -- NCT_Itype_Assoc --
   ---------------------

   --  The hash table NCT_Itype_Assoc contains entries only for those
   --  old nodes which have a non-empty Associated_Node_For_Itype set.
   --  The key is the associated node, and the element is the new node
   --  itself (NOT the associated node for the new node).

   package NCT_Itype_Assoc is new Simple_HTable (
     Header_Num => NCT_Header_Num,
     Element    => Entity_Id,
     No_Element => Empty,
     Key        => Entity_Id,
     Hash       => New_Copy_Hash,
     Equal      => Types."=");

   -------------------
   -- New_Copy_Tree --
   -------------------

   function New_Copy_Tree
     (Source    : Node_Id;
      Map       : Elist_Id   := No_Elist;
      New_Sloc  : Source_Ptr := No_Location;
      New_Scope : Entity_Id  := Empty) return Node_Id
   is
      Actual_Map : Elist_Id := Map;
      --  This is the actual map for the copy. It is initialized with the
      --  given elements, and then enlarged as required for Itypes that are
      --  copied during the first phase of the copy operation. The visit
      --  procedures add elements to this map as Itypes are encountered.
      --  The reason we cannot use Map directly, is that it may well be
      --  (and normally is) initialized to No_Elist, and if we have mapped
      --  entities, we have to reset it to point to a real Elist.

      function Assoc (N : Node_Or_Entity_Id) return Node_Id;
      --  Called during second phase to map entities into their corresponding
      --  copies using Actual_Map. If the argument is not an entity, or is not
      --  in Actual_Map, then it is returned unchanged.

      procedure Build_NCT_Hash_Tables;
      --  Builds hash tables (number of elements >= threshold value)

      function Copy_Elist_With_Replacement
        (Old_Elist : Elist_Id) return Elist_Id;
      --  Called during second phase to copy element list doing replacements

      procedure Copy_Itype_With_Replacement (New_Itype : Entity_Id);
      --  Called during the second phase to process a copied Itype. The actual
      --  copy happened during the first phase (so that we could make the entry
      --  in the mapping), but we still have to deal with the descendants of
      --  the copied Itype and copy them where necessary.

      function Copy_List_With_Replacement (Old_List : List_Id) return List_Id;
      --  Called during second phase to copy list doing replacements

      function Copy_Node_With_Replacement (Old_Node : Node_Id) return Node_Id;
      --  Called during second phase to copy node doing replacements

      procedure Visit_Elist (E : Elist_Id);
      --  Called during first phase to visit all elements of an Elist

      procedure Visit_Field (F : Union_Id; N : Node_Id);
      --  Visit a single field, recursing to call Visit_Node or Visit_List
      --  if the field is a syntactic descendant of the current node (i.e.
      --  its parent is Node N).

      procedure Visit_Itype (Old_Itype : Entity_Id);
      --  Called during first phase to visit subsidiary fields of a defining
      --  Itype, and also create a copy and make an entry in the replacement
      --  map for the new copy.

      procedure Visit_List (L : List_Id);
      --  Called during first phase to visit all elements of a List

      procedure Visit_Node (N : Node_Or_Entity_Id);
      --  Called during first phase to visit a node and all its subtrees

      -----------
      -- Assoc --
      -----------

      function Assoc (N : Node_Or_Entity_Id) return Node_Id is
         E   : Elmt_Id;
         Ent : Entity_Id;

      begin
         if not Has_Extension (N) or else No (Actual_Map) then
            return N;

         elsif NCT_Hash_Tables_Used then
            Ent := NCT_Assoc.Get (Entity_Id (N));

            if Present (Ent) then
               return Ent;
            else
               return N;
            end if;

         --  No hash table used, do serial search

         else
            E := First_Elmt (Actual_Map);
            while Present (E) loop
               if Node (E) = N then
                  return Node (Next_Elmt (E));
               else
                  E := Next_Elmt (Next_Elmt (E));
               end if;
            end loop;
         end if;

         return N;
      end Assoc;

      ---------------------------
      -- Build_NCT_Hash_Tables --
      ---------------------------

      procedure Build_NCT_Hash_Tables is
         Elmt : Elmt_Id;
         Ent  : Entity_Id;
      begin
         if NCT_Hash_Table_Setup then
            NCT_Assoc.Reset;
            NCT_Itype_Assoc.Reset;
         end if;

         Elmt := First_Elmt (Actual_Map);
         while Present (Elmt) loop
            Ent := Node (Elmt);

            --  Get new entity, and associate old and new

            Next_Elmt (Elmt);
            NCT_Assoc.Set (Ent, Node (Elmt));

            if Is_Type (Ent) then
               declare
                  Anode : constant Entity_Id :=
                            Associated_Node_For_Itype (Ent);

               begin
                  if Present (Anode) then

                     --  Enter a link between the associated node of the
                     --  old Itype and the new Itype, for updating later
                     --  when node is copied.

                     NCT_Itype_Assoc.Set (Anode, Node (Elmt));
                  end if;
               end;
            end if;

            Next_Elmt (Elmt);
         end loop;

         NCT_Hash_Tables_Used := True;
         NCT_Hash_Table_Setup := True;
      end Build_NCT_Hash_Tables;

      ---------------------------------
      -- Copy_Elist_With_Replacement --
      ---------------------------------

      function Copy_Elist_With_Replacement
        (Old_Elist : Elist_Id) return Elist_Id
      is
         M         : Elmt_Id;
         New_Elist : Elist_Id;

      begin
         if No (Old_Elist) then
            return No_Elist;

         else
            New_Elist := New_Elmt_List;

            M := First_Elmt (Old_Elist);
            while Present (M) loop
               Append_Elmt (Copy_Node_With_Replacement (Node (M)), New_Elist);
               Next_Elmt (M);
            end loop;
         end if;

         return New_Elist;
      end Copy_Elist_With_Replacement;

      ---------------------------------
      -- Copy_Itype_With_Replacement --
      ---------------------------------

      --  This routine exactly parallels its phase one analog Visit_Itype,

      procedure Copy_Itype_With_Replacement (New_Itype : Entity_Id) is
      begin
         --  Translate Next_Entity, Scope, and Etype fields, in case they
         --  reference entities that have been mapped into copies.

         Set_Next_Entity (New_Itype, Assoc (Next_Entity (New_Itype)));
         Set_Etype       (New_Itype, Assoc (Etype       (New_Itype)));

         if Present (New_Scope) then
            Set_Scope    (New_Itype, New_Scope);
         else
            Set_Scope    (New_Itype, Assoc (Scope       (New_Itype)));
         end if;

         --  Copy referenced fields

         if Is_Discrete_Type (New_Itype) then
            Set_Scalar_Range (New_Itype,
              Copy_Node_With_Replacement (Scalar_Range (New_Itype)));

         elsif Is_Array_Type (New_Itype) then
            if Present (First_Index (New_Itype)) then
               Set_First_Index (New_Itype,
                 First (Copy_List_With_Replacement
                         (List_Containing (First_Index (New_Itype)))));
            end if;

            if Is_Packed (New_Itype) then
--                 Set_Packed_Array_Impl_Type (New_Itype,
--                   Copy_Node_With_Replacement
--                     (Packed_Array_Impl_Type (New_Itype)));
null;            end if;
         end if;
      end Copy_Itype_With_Replacement;

      --------------------------------
      -- Copy_List_With_Replacement --
      --------------------------------

      function Copy_List_With_Replacement
        (Old_List : List_Id) return List_Id
      is
         New_List : List_Id;
         E        : Node_Id;

      begin
         if Old_List = No_List then
            return No_List;

         else
            New_List := Empty_List;

            E := First (Old_List);
            while Present (E) loop
               Append (Copy_Node_With_Replacement (E), New_List);
               Next (E);
            end loop;

            return New_List;
         end if;
      end Copy_List_With_Replacement;

      --------------------------------
      -- Copy_Node_With_Replacement --
      --------------------------------

      function Copy_Node_With_Replacement
        (Old_Node : Node_Id) return Node_Id
      is
         New_Node : Node_Id;

         procedure Adjust_Named_Associations
           (Old_Node : Node_Id;
            New_Node : Node_Id);
         --  If a call node has named associations, these are chained through
         --  the First_Named_Actual, Next_Named_Actual links. These must be
         --  propagated separately to the new parameter list, because these
         --  are not syntactic fields.

         function Copy_Field_With_Replacement
           (Field : Union_Id) return Union_Id;
         --  Given Field, which is a field of Old_Node, return a copy of it
         --  if it is a syntactic field (i.e. its parent is Node), setting
         --  the parent of the copy to poit to New_Node. Otherwise returns
         --  the field (possibly mapped if it is an entity).

         -------------------------------
         -- Adjust_Named_Associations --
         -------------------------------

         procedure Adjust_Named_Associations
           (Old_Node : Node_Id;
            New_Node : Node_Id)
         is
            Old_E : Node_Id;
            New_E : Node_Id;

            Old_Next : Node_Id;
            New_Next : Node_Id;

         begin
            Old_E := First (Parameter_Associations (Old_Node));
            New_E := First (Parameter_Associations (New_Node));
            while Present (Old_E) loop
               if Nkind (Old_E) = N_Parameter_Association
                 and then Present (Next_Named_Actual (Old_E))
               then
                  if First_Named_Actual (Old_Node)
                    = Explicit_Actual_Parameter (Old_E)
                  then
                     Set_First_Named_Actual
                       (New_Node, Explicit_Actual_Parameter (New_E));
                  end if;

                  --  Now scan parameter list from the beginning,to locate
                  --  next named actual, which can be out of order.

                  Old_Next := First (Parameter_Associations (Old_Node));
                  New_Next := First (Parameter_Associations (New_Node));

                  while Nkind (Old_Next) /= N_Parameter_Association
                    or else Explicit_Actual_Parameter (Old_Next) /=
                                              Next_Named_Actual (Old_E)
                  loop
                     Next (Old_Next);
                     Next (New_Next);
                  end loop;

                  Set_Next_Named_Actual
                    (New_E, Explicit_Actual_Parameter (New_Next));
               end if;

               Next (Old_E);
               Next (New_E);
            end loop;
         end Adjust_Named_Associations;

         ---------------------------------
         -- Copy_Field_With_Replacement --
         ---------------------------------

         function Copy_Field_With_Replacement
           (Field : Union_Id) return Union_Id
         is
         begin
            if Field = Union_Id (Empty) then
               return Field;

            elsif Field in Node_Range then
               declare
                  Old_N : constant Node_Id := Node_Id (Field);
                  New_N : Node_Id;

               begin
                  --  If syntactic field, as indicated by the parent pointer
                  --  being set, then copy the referenced node recursively.

                  if Parent (Old_N) = Old_Node then
                     New_N := Copy_Node_With_Replacement (Old_N);

                     if New_N /= Old_N then
                        Set_Parent (New_N, New_Node);
                     end if;

                  --  For semantic fields, update possible entity reference
                  --  from the replacement map.

                  else
                     New_N := Assoc (Old_N);
                  end if;

                  return Union_Id (New_N);
               end;

            elsif Field in List_Range then
               declare
                  Old_L : constant List_Id := List_Id (Field);
                  New_L : List_Id;

               begin
                  --  If syntactic field, as indicated by the parent pointer,
                  --  then recursively copy the entire referenced list.

                  if Parent (Old_L) = Old_Node then
                     New_L := Copy_List_With_Replacement (Old_L);
                     Set_Parent (New_L, New_Node);

                  --  For semantic list, just returned unchanged

                  else
                     New_L := Old_L;
                  end if;

                  return Union_Id (New_L);
               end;

            --  Anything other than a list or a node is returned unchanged

            else
               return Field;
            end if;
         end Copy_Field_With_Replacement;

      --  Start of processing for Copy_Node_With_Replacement

      begin
         if Old_Node <= Empty_Or_Error then
            return Old_Node;

         elsif Has_Extension (Old_Node) then
            return Assoc (Old_Node);

         else
            New_Node := New_Copy (Old_Node);

            --  If the node we are copying is the associated node of a
            --  previously copied Itype, then adjust the associated node
            --  of the copy of that Itype accordingly.

            if Present (Actual_Map) then
               declare
                  E   : Elmt_Id;
                  Ent : Entity_Id;

               begin
                  --  Case of hash table used

                  if NCT_Hash_Tables_Used then
                     Ent := NCT_Itype_Assoc.Get (Old_Node);

                     if Present (Ent) then
                        Set_Associated_Node_For_Itype (Ent, New_Node);
                     end if;

                  --  Case of no hash table used

                  else
                     E := First_Elmt (Actual_Map);
                     while Present (E) loop
                        if Is_Itype (Node (E))
                          and then
                            Old_Node = Associated_Node_For_Itype (Node (E))
                        then
                           Set_Associated_Node_For_Itype
                             (Node (Next_Elmt (E)), New_Node);
                        end if;

                        E := Next_Elmt (Next_Elmt (E));
                     end loop;
                  end if;
               end;
            end if;

            --  Recursively copy descendants

            Set_Field1
              (New_Node, Copy_Field_With_Replacement (Field1 (New_Node)));
            Set_Field2
              (New_Node, Copy_Field_With_Replacement (Field2 (New_Node)));
            Set_Field3
              (New_Node, Copy_Field_With_Replacement (Field3 (New_Node)));
            Set_Field4
              (New_Node, Copy_Field_With_Replacement (Field4 (New_Node)));
            Set_Field5
              (New_Node, Copy_Field_With_Replacement (Field5 (New_Node)));

            --  Adjust Sloc of new node if necessary

            if New_Sloc /= No_Location then
               Set_Sloc (New_Node, New_Sloc);

               --  If we adjust the Sloc, then we are essentially making a
               --  completely new node, so the Comes_From_Source flag should
               --  be reset to the proper default value.

               Set_Comes_From_Source
                 (New_Node, Default_Node.Comes_From_Source);
            end if;

            --  If the node is a call and has named associations, set the
            --  corresponding links in the copy.

            if Nkind_In (Old_Node, N_Function_Call,
                                   N_Procedure_Call_Statement)
              and then Present (First_Named_Actual (Old_Node))
            then
               Adjust_Named_Associations (Old_Node, New_Node);
            end if;

            --  Reset First_Real_Statement for Handled_Sequence_Of_Statements.
            --  The replacement mechanism applies to entities, and is not used
            --  here. Eventually we may need a more general graph-copying
            --  routine. For now, do a sequential search to find desired node.

            if Nkind (Old_Node) = N_Handled_Sequence_Of_Statements
              and then Present (First_Real_Statement (Old_Node))
            then
               declare
                  Old_F  : constant Node_Id := First_Real_Statement (Old_Node);
                  N1, N2 : Node_Id;

               begin
                  N1 := First (Statements (Old_Node));
                  N2 := First (Statements (New_Node));

                  while N1 /= Old_F loop
                     Next (N1);
                     Next (N2);
                  end loop;

                  Set_First_Real_Statement (New_Node, N2);
               end;
            end if;
         end if;

         --  All done, return copied node

         return New_Node;
      end Copy_Node_With_Replacement;

      -----------------
      -- Visit_Elist --
      -----------------

      procedure Visit_Elist (E : Elist_Id) is
         Elmt : Elmt_Id;
      begin
         if Present (E) then
            Elmt := First_Elmt (E);

            while Elmt /= No_Elmt loop
               Visit_Node (Node (Elmt));
               Next_Elmt (Elmt);
            end loop;
         end if;
      end Visit_Elist;

      -----------------
      -- Visit_Field --
      -----------------

      procedure Visit_Field (F : Union_Id; N : Node_Id) is
      begin
         if F = Union_Id (Empty) then
            return;

         elsif F in Node_Range then

            --  Copy node if it is syntactic, i.e. its parent pointer is
            --  set to point to the field that referenced it (certain
            --  Itypes will also meet this criterion, which is fine, since
            --  these are clearly Itypes that do need to be copied, since
            --  we are copying their parent.)

            if Parent (Node_Id (F)) = N then
               Visit_Node (Node_Id (F));
               return;

            --  Another case, if we are pointing to an Itype, then we want
            --  to copy it if its associated node is somewhere in the tree
            --  being copied.

            --  Note: the exclusion of self-referential copies is just an
            --  optimization, since the search of the already copied list
            --  would catch it, but it is a common case (Etype pointing
            --  to itself for an Itype that is a base type).

            elsif Has_Extension (Node_Id (F))
              and then Is_Itype (Entity_Id (F))
              and then Node_Id (F) /= N
            then
               declare
                  P : Node_Id;

               begin
                  P := Associated_Node_For_Itype (Node_Id (F));
                  while Present (P) loop
                     if P = Source then
                        Visit_Node (Node_Id (F));
                        return;
                     else
                        P := Parent (P);
                     end if;
                  end loop;

                  --  An Itype whose parent is not being copied definitely
                  --  should NOT be copied, since it does not belong in any
                  --  sense to the copied subtree.

                  return;
               end;
            end if;

         elsif F in List_Range and then Parent (List_Id (F)) = N then
            Visit_List (List_Id (F));
            return;
         end if;
      end Visit_Field;

      -----------------
      -- Visit_Itype --
      -----------------

      procedure Visit_Itype (Old_Itype : Entity_Id) is
         New_Itype : Entity_Id;
         E         : Elmt_Id;
         Ent       : Entity_Id;

      begin
         --  Itypes that describe the designated type of access to subprograms
         --  have the structure of subprogram declarations, with signatures,
         --  etc. Either we duplicate the signatures completely, or choose to
         --  share such itypes, which is fine because their elaboration will
         --  have no side effects.

         if Ekind (Old_Itype) = E_Subprogram_Type then
            return;
         end if;

         New_Itype := New_Copy (Old_Itype);

         --  The new Itype has all the attributes of the old one, and
         --  we just copy the contents of the entity. However, the back-end
         --  needs different names for debugging purposes, so we create a
         --  new internal name for it in all cases.

         Set_Chars (New_Itype, New_Internal_Name ('T'));

         --  If our associated node is an entity that has already been copied,
         --  then set the associated node of the copy to point to the right
         --  copy. If we have copied an Itype that is itself the associated
         --  node of some previously copied Itype, then we set the right
         --  pointer in the other direction.

         if Present (Actual_Map) then

            --  Case of hash tables used

            if NCT_Hash_Tables_Used then

               Ent := NCT_Assoc.Get (Associated_Node_For_Itype (Old_Itype));

               if Present (Ent) then
                  Set_Associated_Node_For_Itype (New_Itype, Ent);
               end if;

               Ent := NCT_Itype_Assoc.Get (Old_Itype);
               if Present (Ent) then
                  Set_Associated_Node_For_Itype (Ent, New_Itype);

               --  If the hash table has no association for this Itype and
               --  its associated node, enter one now.

               else
                  NCT_Itype_Assoc.Set
                    (Associated_Node_For_Itype (Old_Itype), New_Itype);
               end if;

            --  Case of hash tables not used

            else
               E := First_Elmt (Actual_Map);
               while Present (E) loop
                  if Associated_Node_For_Itype (Old_Itype) = Node (E) then
                     Set_Associated_Node_For_Itype
                       (New_Itype, Node (Next_Elmt (E)));
                  end if;

                  if Is_Type (Node (E))
                    and then Old_Itype = Associated_Node_For_Itype (Node (E))
                  then
                     Set_Associated_Node_For_Itype
                       (Node (Next_Elmt (E)), New_Itype);
                  end if;

                  E := Next_Elmt (Next_Elmt (E));
               end loop;
            end if;
         end if;

         if Present (Freeze_Node (New_Itype)) then
            Set_Is_Frozen (New_Itype, False);
            Set_Freeze_Node (New_Itype, Empty);
         end if;

         --  Add new association to map

         if No (Actual_Map) then
            Actual_Map := New_Elmt_List;
         end if;

         Append_Elmt (Old_Itype, Actual_Map);
         Append_Elmt (New_Itype, Actual_Map);

         if NCT_Hash_Tables_Used then
            NCT_Assoc.Set (Old_Itype, New_Itype);

         else
            NCT_Table_Entries := NCT_Table_Entries + 1;

            if NCT_Table_Entries > NCT_Hash_Threshold then
               Build_NCT_Hash_Tables;
            end if;
         end if;

         --  If a record subtype is simply copied, the entity list will be
         --  shared. Thus cloned_Subtype must be set to indicate the sharing.

         if Ekind_In (Old_Itype, E_Record_Subtype, E_Class_Wide_Subtype) then
            Set_Cloned_Subtype (New_Itype, Old_Itype);
         end if;

         --  Visit descendants that eventually get copied

         Visit_Field (Union_Id (Etype (Old_Itype)), Old_Itype);

         if Is_Discrete_Type (Old_Itype) then
            Visit_Field (Union_Id (Scalar_Range (Old_Itype)), Old_Itype);

         elsif Is_Array_Type (Old_Itype) then
            if Present (First_Index (Old_Itype)) then
               Visit_Field (Union_Id (List_Containing
                                (First_Index (Old_Itype))),
                            Old_Itype);
            end if;

            if Is_Packed (Old_Itype) then
--                 Visit_Field (Union_Id (Packed_Array_Impl_Type (Old_Itype)),
--                              Old_Itype);
null;            end if;
         end if;
      end Visit_Itype;

      ----------------
      -- Visit_List --
      ----------------

      procedure Visit_List (L : List_Id) is
         N : Node_Id;
      begin
         if L /= No_List then
            N := First (L);

            while Present (N) loop
               Visit_Node (N);
               Next (N);
            end loop;
         end if;
      end Visit_List;

      ----------------
      -- Visit_Node --
      ----------------

      procedure Visit_Node (N : Node_Or_Entity_Id) is

      --  Start of processing for Visit_Node

      begin
         --  Handle case of an Itype, which must be copied

         if Has_Extension (N) and then Is_Itype (N) then

            --  Nothing to do if already in the list. This can happen with an
            --  Itype entity that appears more than once in the tree.
            --  Note that we do not want to visit descendants in this case.

            --  Test for already in list when hash table is used

            if NCT_Hash_Tables_Used then
               if Present (NCT_Assoc.Get (Entity_Id (N))) then
                  return;
               end if;

            --  Test for already in list when hash table not used

            else
               declare
                  E : Elmt_Id;
               begin
                  if Present (Actual_Map) then
                     E := First_Elmt (Actual_Map);
                     while Present (E) loop
                        if Node (E) = N then
                           return;
                        else
                           E := Next_Elmt (Next_Elmt (E));
                        end if;
                     end loop;
                  end if;
               end;
            end if;

            Visit_Itype (N);
         end if;

         --  Visit descendants

         Visit_Field (Field1 (N), N);
         Visit_Field (Field2 (N), N);
         Visit_Field (Field3 (N), N);
         Visit_Field (Field4 (N), N);
         Visit_Field (Field5 (N), N);
      end Visit_Node;

   --  Start of processing for New_Copy_Tree

   begin
      Actual_Map := Map;

      --  See if we should use hash table

      if No (Actual_Map) then
         NCT_Hash_Tables_Used := False;

      else
         declare
            Elmt : Elmt_Id;

         begin
            NCT_Table_Entries := 0;

            Elmt := First_Elmt (Actual_Map);
            while Present (Elmt) loop
               NCT_Table_Entries := NCT_Table_Entries + 1;
               Next_Elmt (Elmt);
               Next_Elmt (Elmt);
            end loop;

            if NCT_Table_Entries > NCT_Hash_Threshold then
               Build_NCT_Hash_Tables;
            else
               NCT_Hash_Tables_Used := False;
            end if;
         end;
      end if;

      --  Hash table set up if required, now start phase one by visiting
      --  top node (we will recursively visit the descendants).

      Visit_Node (Source);

      --  Now the second phase of the copy can start. First we process
      --  all the mapped entities, copying their descendants.

      if Present (Actual_Map) then
         declare
            Elmt      : Elmt_Id;
            New_Itype : Entity_Id;
         begin
            Elmt := First_Elmt (Actual_Map);
            while Present (Elmt) loop
               Next_Elmt (Elmt);
               New_Itype := Node (Elmt);

               if Is_Itype (New_Itype) then
                  Copy_Itype_With_Replacement (New_Itype);
               end if;
               Next_Elmt (Elmt);
            end loop;
         end;
      end if;

      --  Now we can copy the actual tree

      return Copy_Node_With_Replacement (Source);
   end New_Copy_Tree;

   -------------------------
   -- New_External_Entity --
   -------------------------

   function New_External_Entity
     (Kind         : Entity_Kind;
      Scope_Id     : Entity_Id;
      Sloc_Value   : Source_Ptr;
      Related_Id   : Entity_Id;
      Suffix       : Character;
      Suffix_Index : Nat := 0;
      Prefix       : Character := ' ') return Entity_Id
   is
      N : constant Entity_Id :=
            Make_Defining_Identifier (Sloc_Value,
              New_External_Name
                (Chars (Related_Id), Suffix, Suffix_Index, Prefix));

   begin
      Set_Ekind          (N, Kind);
      Set_Is_Internal    (N, True);
      Append_Entity      (N, Scope_Id);
      Set_Public_Status  (N);

      if Kind in Type_Kind then
         Init_Size_Align (N);
      end if;

      return N;
   end New_External_Entity;

   -------------------------
   -- New_Internal_Entity --
   -------------------------

   function New_Internal_Entity
     (Kind       : Entity_Kind;
      Scope_Id   : Entity_Id;
      Sloc_Value : Source_Ptr;
      Id_Char    : Character) return Entity_Id
   is
      N : constant Entity_Id :=
            Make_Defining_Identifier (Sloc_Value, New_Internal_Name (Id_Char));

   begin
      Set_Ekind          (N, Kind);
      Set_Is_Internal    (N, True);
      Append_Entity      (N, Scope_Id);

      if Kind in Type_Kind then
         Init_Size_Align (N);
      end if;

      return N;
   end New_Internal_Entity;

   -----------------
   -- Next_Actual --
   -----------------

   function Next_Actual (Actual_Id : Node_Id) return Node_Id is
      N  : Node_Id;

   begin
      --  If we are pointing at a positional parameter, it is a member of
      --  a node list (the list of parameters), and the next parameter
      --  is the next node on the list, unless we hit a parameter
      --  association, in which case we shift to using the chain whose
      --  head is the First_Named_Actual in the parent, and then is
      --  threaded using the Next_Named_Actual of the Parameter_Association.
      --  All this fiddling is because the original node list is in the
      --  textual call order, and what we need is the declaration order.

      if Is_List_Member (Actual_Id) then
         N := Next (Actual_Id);

         if Nkind (N) = N_Parameter_Association then
            return First_Named_Actual (Parent (Actual_Id));
         else
            return N;
         end if;

      else
         return Next_Named_Actual (Parent (Actual_Id));
      end if;
   end Next_Actual;

   procedure Next_Actual (Actual_Id : in out Node_Id) is
   begin
      Actual_Id := Next_Actual (Actual_Id);
   end Next_Actual;

   -----------------------
   -- Normalize_Actuals --
   -----------------------

   --  Chain actuals according to formals of subprogram. If there are
   --  no named associations, the chain is simply the list of Parameter
   --  Associations, since the order is the same as the declaration order.
   --  If there are named associations, then the First_Named_Actual field
   --  in the N_Procedure_Call_Statement node or N_Function_Call node
   --  points to the Parameter_Association node for the parameter that
   --  comes first in declaration order. The remaining named parameters
   --  are then chained in declaration order using Next_Named_Actual.

   --  This routine also verifies that the number of actuals is compatible
   --  with the number and default values of formals, but performs no type
   --  checking (type checking is done by the caller).

   --  If the matching succeeds, Success is set to True, and the caller
   --  proceeds with type-checking. If the match is unsuccessful, then
   --  Success is set to False, and the caller attempts a different
   --  interpretation, if there is one.

   --  If the flag Report is on, the call is not overloaded, and a failure
   --  to match can be reported here, rather than in the caller.

   procedure Normalize_Actuals
     (N       : Node_Id;
      S       : Entity_Id;
      Report  : Boolean;
      Success : out Boolean)
   is
      Actuals     : constant List_Id := Parameter_Associations (N);
      Actual      : Node_Id   := Empty;
      Formal      : Entity_Id;
      Last        : Node_Id := Empty;
      First_Named : Node_Id := Empty;
      Found       : Boolean;

      Formals_To_Match : Integer := 0;
      Actuals_To_Match : Integer := 0;

      procedure Chain (A : Node_Id);
      --  Add named actual at the proper place in the list, using the
      --  Next_Named_Actual link.

      function Reporting return Boolean;
      --  Determines if an error is to be reported. To report an error, we
      --  need Report to be True, and also we do not report errors caused
      --  by calls to init procs that occur within other init procs. Such
      --  errors must always be cascaded errors, since if all the types are
      --  declared correctly, the compiler will certainly build decent calls!

      -----------
      -- Chain --
      -----------

      procedure Chain (A : Node_Id) is
      begin
         if No (Last) then

            --  Call node points to first actual in list.

            Set_First_Named_Actual (N, Explicit_Actual_Parameter (A));

         else
            Set_Next_Named_Actual (Last, Explicit_Actual_Parameter (A));
         end if;

         Last := A;
         Set_Next_Named_Actual (Last, Empty);
      end Chain;

      ---------------
      -- Reporting --
      ---------------

      function Reporting return Boolean is
      begin
         if not Report then
            return False;

         elsif not Within_Init_Proc then
            return True;

--           elsif Is_Init_Proc (Entity (Name (N))) then
--              return False;

         else
            return True;
         end if;
      end Reporting;

   --  Start of processing for Normalize_Actuals

   begin
      if Is_Access_Type (S) then

         --  The name in the call is a function call that returns an access
         --  to subprogram. The designated type has the list of formals.

         Formal := First_Formal (Designated_Type (S));
      else
         Formal := First_Formal (S);
      end if;

      while Present (Formal) loop
         Formals_To_Match := Formals_To_Match + 1;
         Next_Formal (Formal);
      end loop;

      --  Find if there is a named association, and verify that no positional
      --  associations appear after named ones.

      if Present (Actuals) then
         Actual := First (Actuals);
      end if;

      while Present (Actual)
        and then Nkind (Actual) /= N_Parameter_Association
      loop
         Actuals_To_Match := Actuals_To_Match + 1;
         Next (Actual);
      end loop;

      if No (Actual) and Actuals_To_Match = Formals_To_Match then

         --  Most common case: positional notation, no defaults

         Success := True;
         return;

      elsif Actuals_To_Match > Formals_To_Match then

         --  Too many actuals: will not work.

         if Reporting then
            if Is_Entity_Name (Name (N)) then
               Error_Msg_N ("too many arguments in call to&", Name (N));
            else
               Error_Msg_N ("too many arguments in call", N);
            end if;
         end if;

         Success := False;
         return;
      end if;

      First_Named := Actual;

      while Present (Actual) loop
         if Nkind (Actual) /= N_Parameter_Association then
            Error_Msg_N
              ("positional parameters not allowed after named ones", Actual);
            Success := False;
            return;

         else
            Actuals_To_Match := Actuals_To_Match + 1;
         end if;

         Next (Actual);
      end loop;

      if Present (Actuals) then
         Actual := First (Actuals);
      end if;

      Formal := First_Formal (S);

      while Present (Formal) loop

         --  Match the formals in order. If the corresponding actual
         --  is positional,  nothing to do. Else scan the list of named
         --  actuals to find the one with the right name.

         if Present (Actual)
           and then Nkind (Actual) /= N_Parameter_Association
         then
            Next (Actual);
            Actuals_To_Match := Actuals_To_Match - 1;
            Formals_To_Match := Formals_To_Match - 1;

         else
            --  For named parameters, search the list of actuals to find
            --  one that matches the next formal name.

            Actual := First_Named;
            Found  := False;

            while Present (Actual) loop
               if Chars (Selector_Name (Actual)) = Chars (Formal) then
                  Found := True;
                  Chain (Actual);
                  Actuals_To_Match := Actuals_To_Match - 1;
                  Formals_To_Match := Formals_To_Match - 1;
                  exit;
               end if;

               Next (Actual);
            end loop;

            if not Found then
               if Ekind (Formal) /= E_In_Parameter
                 or else No (Default_Value (Formal))
               then
                  if Reporting then
                     if (Comes_From_Source (S)
                          or else Sloc (S) = Standard_Location)
                       and then Is_Overloadable (S)
                     then
                        Error_Msg_Name_1 := Chars (S);
                        Error_Msg_Sloc := Sloc (S);
                        Error_Msg_NE
                          ("missing argument for parameter & " &
                             "in call to % declared #", N, Formal);

                     elsif Is_Overloadable (S) then
                        Error_Msg_Name_1 := Chars (S);

                        --  Point to type derivation that
                        --  generated the operation.

                        Error_Msg_Sloc := Sloc (Parent (S));

                        Error_Msg_NE
                          ("missing argument for parameter & " &
                             "in call to % (inherited) #", N, Formal);

                     else
                        Error_Msg_NE
                          ("missing argument for parameter &", N, Formal);
                     end if;
                  end if;

                  Success := False;
                  return;

               else
                  Formals_To_Match := Formals_To_Match - 1;
               end if;
            end if;
         end if;

         Next_Formal (Formal);
      end loop;

      if  Formals_To_Match = 0 and then Actuals_To_Match = 0 then
         Success := True;
         return;

      else
         if Reporting then

            --  Find some superfluous named actual that did not get
            --  attached to the list of associations.

            Actual := First (Actuals);

            while Present (Actual) loop

               if Nkind (Actual) = N_Parameter_Association
                 and then Actual /= Last
                 and then No (Next_Named_Actual (Actual))
               then
                  Error_Msg_N ("unmatched actual & in call",
                    Selector_Name (Actual));
                  exit;
               end if;

               Next (Actual);
            end loop;
         end if;

         Success := False;
         return;
      end if;
   end Normalize_Actuals;

   --------------------------------
   -- Note_Possible_Modification --
   --------------------------------

   procedure Note_Possible_Modification (N : Node_Id) is
      Ent : Entity_Id;
      Exp : Node_Id;

      procedure Set_Ref (E : Entity_Id; N : Node_Id);
      --  Internal routine to note modification on entity E by node N
      --  Has no effect if entity E does not represent an object.

      -------------
      -- Set_Ref --
      -------------

      procedure Set_Ref (E : Entity_Id; N : Node_Id) is
      begin
         if Is_Object (E) then
            if Comes_From_Source (N) then
               Set_Never_Set_In_Source (E, False);
            end if;

            Set_Is_True_Constant    (E, False);
            Set_Current_Value       (E, Empty);
            Generate_Reference      (E, N, 'm');
            -- Kill_Checks             (E);

            if not Can_Never_Be_Null (E) then
               Set_Is_Known_Non_Null (E, False);
            end if;
         end if;
      end Set_Ref;

   --  Start of processing for Note_Possible_Modification

   begin
      --  Loop to find referenced entity, if there is one

      Exp := N;
      loop
         --  Test for node rewritten as dereference (e.g. accept parameter)

         if Nkind (Exp) = N_Explicit_Dereference
           and then not Comes_From_Source (Exp)
         then
            Exp := Original_Node (Exp);
         end if;

         --  Now look for entity being referenced

         if Is_Entity_Name (Exp) then
            Ent := Entity (Exp);

            if (Ekind (Ent) = E_Variable or else Ekind (Ent) = E_Constant)
              and then Present (Renamed_Object (Ent))
            then
               Set_Never_Set_In_Source (Ent, False);
               Set_Is_True_Constant    (Ent, False);
               Set_Current_Value       (Ent, Empty);

               if not Can_Never_Be_Null (Ent) then
                  Set_Is_Known_Non_Null (Ent, False);
               end if;

               Exp := Renamed_Object (Ent);

            else
               Set_Ref (Ent, Exp);
               --Kill_Checks (Ent);
               return;
            end if;

         elsif     Nkind (Exp) = N_Type_Conversion
           or else Nkind (Exp) = N_Unchecked_Type_Conversion
         then
            Exp := Expression (Exp);

         elsif     Nkind (Exp) = N_Slice
           or else Nkind (Exp) = N_Indexed_Component
           or else Nkind (Exp) = N_Selected_Component
         then
            Exp := Prefix (Exp);

         else
            return;
         end if;
      end loop;
   end Note_Possible_Modification;

   -------------------------
   -- Object_Access_Level --
   -------------------------

   function Object_Access_Level (Obj : Node_Id) return Uint is
      E : Entity_Id;

   --  Returns the static accessibility level of the view denoted
   --  by Obj.  Note that the value returned is the result of a
   --  call to Scope_Depth.  Only scope depths associated with
   --  dynamic scopes can actually be returned.  Since only
   --  relative levels matter for accessibility checking, the fact
   --  that the distance between successive levels of accessibility
   --  is not always one is immaterial (invariant: if level(E2) is
   --  deeper than level(E1), then Scope_Depth(E1) < Scope_Depth(E2)).

   begin
      if Is_Entity_Name (Obj) then
         E := Entity (Obj);

         --  If E is a type then it denotes a current instance.
         --  For this case we add one to the normal accessibility
         --  level of the type to ensure that current instances
         --  are treated as always being deeper than than the level
         --  of any visible named access type (see 3.10.2(21)).

         if Is_Type (E) then
            return Type_Access_Level (E) +  1;

         elsif Present (Renamed_Object (E)) then
            return Object_Access_Level (Renamed_Object (E));

         else
            return Scope_Depth (Enclosing_Dynamic_Scope (E));
         end if;

      elsif Nkind (Obj) = N_Selected_Component then
         if Is_Access_Type (Etype (Prefix (Obj))) then
            return Type_Access_Level (Etype (Prefix (Obj)));
         else
            return Object_Access_Level (Prefix (Obj));
         end if;

      elsif Nkind (Obj) = N_Indexed_Component then
         if Is_Access_Type (Etype (Prefix (Obj))) then
            return Type_Access_Level (Etype (Prefix (Obj)));
         else
            return Object_Access_Level (Prefix (Obj));
         end if;

      elsif Nkind (Obj) = N_Explicit_Dereference then

            return Type_Access_Level (Etype (Prefix (Obj)));

      elsif Nkind (Obj) = N_Type_Conversion
        or else Nkind (Obj) = N_Unchecked_Type_Conversion
      then
         return Object_Access_Level (Expression (Obj));

      --  Function results are objects, so we get either the access level
      --  of the function or, in the case of an indirect call, the level of
      --  of the access-to-subprogram type.

      elsif Nkind (Obj) = N_Function_Call then
         if Is_Entity_Name (Name (Obj)) then
            return Subprogram_Access_Level (Entity (Name (Obj)));
         else
            return Type_Access_Level (Etype (Prefix (Name (Obj))));
         end if;

      --  For convenience we handle qualified expressions, even though
      --  they aren't technically object names.

      elsif Nkind (Obj) = N_Qualified_Expression then
         return Object_Access_Level (Expression (Obj));

      --  Otherwise return the scope level of Standard.
      --  (If there are cases that fall through
      --  to this point they will be treated as
      --  having global accessibility for now. ???)

      else
         return Scope_Depth (Standard_Standard);
      end if;
   end Object_Access_Level;

   -----------------------
   -- Private_Component --
   -----------------------

   function Private_Component (Type_Id : Entity_Id) return Entity_Id is
      Ancestor  : constant Entity_Id := Base_Type (Type_Id);

      function Trace_Components
        (T     : Entity_Id;
         Check : Boolean) return Entity_Id;
      --  Recursive function that does the work, and checks against circular
      --  definition for each subcomponent type.

      ----------------------
      -- Trace_Components --
      ----------------------

      function Trace_Components
         (T     : Entity_Id;
          Check : Boolean) return Entity_Id
       is
         Btype     : constant Entity_Id := Base_Type (T);
         Component : Entity_Id;
         P         : Entity_Id;
         Candidate : Entity_Id := Empty;

      begin
         if Check and then Btype = Ancestor then
            Error_Msg_N ("circular type definition", Type_Id);
            return Any_Type;
         end if;

         if Is_Private_Type (Btype)
           and then not Is_Generic_Type (Btype)
         then
            return Btype;

         elsif Is_Array_Type (Btype) then
            return Trace_Components (Component_Type (Btype), True);

         elsif Is_Record_Type (Btype) then
            Component := First_Entity (Btype);
            while Present (Component) loop

               --  skip anonymous types generated by constrained components.

               if not Is_Type (Component) then
                  P := Trace_Components (Etype (Component), True);

                  if Present (P) then
                     if P = Any_Type then
                        return P;
                     else
                        Candidate := P;
                     end if;
                  end if;
               end if;

               Next_Entity (Component);
            end loop;

            return Candidate;

         else
            return Empty;
         end if;
      end Trace_Components;

   --  Start of processing for Private_Component

   begin
      return Trace_Components (Type_Id, False);
   end Private_Component;

   -----------------------
   -- Process_End_Label --
   -----------------------

   procedure Process_End_Label
     (N   : Node_Id;
      Typ : Character;
      Ent  : Entity_Id)
   is
      Loc  : Source_Ptr;
      Nam  : Node_Id;

      Label_Ref : Boolean;
      --  Set True if reference to end label itself is required

      Endl : Node_Id;
      --  Gets set to the operator symbol or identifier that references
      --  the entity Ent. For the child unit case, this is the identifier
      --  from the designator. For other cases, this is simply Endl.

      procedure Generate_Parent_Ref (N : Node_Id);
      --  N is an identifier node that appears as a parent unit reference
      --  in the case where Ent is a child unit. This procedure generates
      --  an appropriate cross-reference entry.

      -------------------------
      -- Generate_Parent_Ref --
      -------------------------

      procedure Generate_Parent_Ref (N : Node_Id) is
         Parent_Ent : Entity_Id;

      begin
         --  Search up scope stack. The reason we do this is that normal
         --  visibility analysis would not work for two reasons. First in
         --  some subunit cases, the entry for the parent unit may not be
         --  visible, and in any case there can be a local entity that
         --  hides the scope entity.

         Parent_Ent := Current_Scope;
         while Present (Parent_Ent) loop
            if Chars (Parent_Ent) = Chars (N) then

               --  Generate the reference. We do NOT consider this as a
               --  reference for unreferenced symbol purposes, but we do
               --  force a cross-reference even if the end line does not
               --  come from source (the caller already generated the
               --  appropriate Typ for this situation).

               Generate_Reference
                 (Parent_Ent, N, 'r', Set_Ref => False, Force => True);
               Style.Check_Identifier (N, Parent_Ent);
               return;
            end if;

            Parent_Ent := Scope (Parent_Ent);
         end loop;

         --  Fall through means entity was not found -- that's odd, but
         --  the appropriate thing is simply to ignore and not generate
         --  any cross-reference for this entry.

         return;
      end Generate_Parent_Ref;

   --  Start of processing for Process_End_Label

   begin
      --  If no node, ignore. This happens in some error situations,
      --  and also for some internally generated structures where no
      --  end label references are required in any case.

      if No (N) then
         return;
      end if;

      --  Nothing to do if no End_Label, happens for internally generated
      --  constructs where we don't want an end label reference anyway.
      --  Also nothing to do if Endl is a string literal, which means
      --  there was some prior error (bad operator symbol)

      Endl := End_Label (N);

      if No (Endl) or else Nkind (Endl) = N_String_Literal then
         return;
      end if;

      --  Reference node is not in extended main source unit

      if not In_Extended_Main_Source_Unit (N) then

         --  Generally we do not collect references except for the
         --  extended main source unit. The one exception is the 'e'
         --  entry for a package spec, where it is useful for a client
         --  to have the ending information to define scopes.

         if Typ /= 'e' then
            return;

         else
            Label_Ref := False;

            --  For this case, we can ignore any parent references,
            --  but we need the package name itself for the 'e' entry.

            if Nkind (Endl) = N_Designator then
               Endl := Identifier (Endl);
            end if;
         end if;

      --  Reference is in extended main source unit

      else
         Label_Ref := True;

         --  For designator, generate references for the parent entries

         if Nkind (Endl) = N_Designator then

            --  Generate references for the prefix if the END line comes
            --  from source (otherwise we do not need these references)

            if Comes_From_Source (Endl) then
               Nam := Name (Endl);
               while Nkind (Nam) = N_Selected_Component loop
                  Generate_Parent_Ref (Selector_Name (Nam));
                  Nam := Prefix (Nam);
               end loop;

               Generate_Parent_Ref (Nam);
            end if;

            Endl := Identifier (Endl);
         end if;
      end if;

      --  If the end label is not for the given entity, then either we have
      --  some previous error, or this is a generic instantiation for which
      --  we do not need to make a cross-reference in this case anyway. In
      --  either case we simply ignore the call.

      if Chars (Ent) /= Chars (Endl) then
         return;
      end if;

      --  If label was really there, then generate a normal reference
      --  and then adjust the location in the end label to point past
      --  the name (which should almost always be the semicolon).

      Loc := Sloc (Endl);

      if Comes_From_Source (Endl) then

         --  If a label reference is required, then do the style check
         --  and generate an l-type cross-reference entry for the label

         if Label_Ref then
            if Style_Check then
               Style.Check_Identifier (Endl, Ent);
            end if;
            Generate_Reference (Ent, Endl, 'l', Set_Ref => False);
         end if;

         --  Set the location to point past the label (normally this will
         --  mean the semicolon immediately following the label). This is
         --  done for the sake of the 'e' or 't' entry generated below.

         Get_Decoded_Name_String (Chars (Endl));
         Set_Sloc (Endl, Sloc (Endl) + Source_Ptr (Name_Len));
      end if;

      --  Now generate the e/t reference

      Generate_Reference (Ent, Endl, Typ, Set_Ref => False, Force => True);

      --  Restore Sloc, in case modified above, since we have an identifier
      --  and the normal Sloc should be left set in the tree.

      Set_Sloc (Endl, Loc);
   end Process_End_Label;

   ------------------
   -- Real_Convert --
   ------------------

   --  We do the conversion to get the value of the real string by using
   --  the scanner, see Sinput for details on use of the internal source
   --  buffer for scanning internal strings.

   function Real_Convert (S : String) return Node_Id is
      Save_Src : constant Source_Buffer_Ptr := Source;
      Negative : Boolean;

   begin
      Source := Internal_Source_Ptr;
      Scan_Ptr := 1;

      for J in S'Range loop
         Source (Source_Ptr (J)) := S (J);
      end loop;

      Source (S'Length + 1) := EOF;

      if Source (Scan_Ptr) = '-' then
         Negative := True;
         Scan_Ptr := Scan_Ptr + 1;
      else
         Negative := False;
      end if;

      Scan;

      if Negative then
         Set_Realval (Token_Node, UR_Negate (Realval (Token_Node)));
      end if;

      Source := Save_Src;
      return Token_Node;
   end Real_Convert;

   ---------------------
   -- Rep_To_Pos_Flag --
   ---------------------

   function Rep_To_Pos_Flag (E : Entity_Id; Loc : Source_Ptr) return Node_Id is
   begin
--        if Range_Checks_Suppressed (E) then
         return New_Occurrence_Of (Standard_False, Loc);
--        else
--           return New_Occurrence_Of (Standard_True, Loc);
--        end if;
   end Rep_To_Pos_Flag;

   --------------------
   -- Require_Entity --
   --------------------

   procedure Require_Entity (N : Node_Id) is
   begin
      if Is_Entity_Name (N) and then No (Entity (N)) then
         if Total_Errors_Detected /= 0 then
            Set_Entity (N, Any_Id);
         else
            raise Program_Error;
         end if;
      end if;
   end Require_Entity;

   ------------------------------
   -- Requires_Transient_Scope --
   ------------------------------

   --  A transient scope is required when variable-sized temporaries are
   --  allocated in the primary or secondary stack, or when finalization
   --  actions must be generated before the next instruction

   function Requires_Transient_Scope (Id : Entity_Id) return Boolean is
      Typ : constant Entity_Id := Underlying_Type (Id);

   begin
      --  This is a private type which is not completed yet. This can only
      --  happen in a default expression (of a formal parameter or of a
      --  record component). Do not expand transient scope in this case

      if No (Typ) then
         return False;

      elsif Typ = Standard_Void_Type then
         return False;

      --  The back-end has trouble allocating variable-size temporaries so
      --  we generate them in the front-end and need a transient scope to
      --  reclaim them properly

      elsif not Size_Known_At_Compile_Time (Typ) then
         return True;

      --  Functions returning tagged types may dispatch on result so their
      --  returned value is allocated on the secondary stack. Controlled
      --  type temporaries need finalization.

      elsif Is_Tagged_Type (Typ)
        or else Has_Controlled_Component (Typ)
      then
         return True;

      --  Unconstrained array types are returned on the secondary stack

      elsif Is_Array_Type (Typ) then
         return not Is_Constrained (Typ);
      end if;

      return False;
   end Requires_Transient_Scope;

   --------------------------
   -- Reset_Analyzed_Flags --
   --------------------------

   procedure Reset_Analyzed_Flags (N : Node_Id) is

      function Clear_Analyzed
        (N : Node_Id) return Traverse_Result;
      --  Function used to reset Analyzed flags in tree. Note that we do
      --  not reset Analyzed flags in entities, since there is no need to
      --  renalalyze entities, and indeed, it is wrong to do so, since it
      --  can result in generating auxiliary stuff more than once.

      --------------------
      -- Clear_Analyzed --
      --------------------

      function Clear_Analyzed
        (N : Node_Id) return Traverse_Result
      is
      begin
         if not Has_Extension (N) then
            Set_Analyzed (N, False);
         end if;

         return OK;
      end Clear_Analyzed;

      function Reset_Analyzed is
        new Traverse_Func (Clear_Analyzed);

      Discard : Traverse_Result;
      pragma Warnings (Off, Discard);

   --  Start of processing for Reset_Analyzed_Flags

   begin
      Discard := Reset_Analyzed (N);
   end Reset_Analyzed_Flags;

   ---------------------------
   -- Safe_To_Capture_Value --
   ---------------------------

   function Safe_To_Capture_Value
     (N   : Node_Id;
      Ent : Entity_Id) return Boolean
   is
   begin
      --  The only entities for which we track constant values are variables,
      --  out parameters and in out parameters, so check if we have this case.

      if Ekind (Ent) /= E_Variable
           and then
         Ekind (Ent) /= E_Out_Parameter
           and then
         Ekind (Ent) /= E_In_Out_Parameter
      then
         return False;
      end if;

      --  Skip volatile and aliased variables, since funny things might
      --  be going on in these cases which we cannot necessarily track.

      if Treat_As_Volatile (Ent) or else Is_Aliased (Ent) then
         return False;
      end if;

      --  OK, all above conditions are met. We also require that the scope
      --  of the reference be the same as the scope of the entity, not
      --  counting packages and blocks.

      declare
         E_Scope : constant Entity_Id := Scope (Ent);
         R_Scope : Entity_Id;

      begin
         R_Scope := Current_Scope;
         while R_Scope /= Standard_Standard loop
            exit when R_Scope = E_Scope;

            if Ekind (R_Scope) /= E_Package
                 and then
               Ekind (R_Scope) /= E_Block
            then
               return False;
            else
               R_Scope := Scope (R_Scope);
            end if;
         end loop;
      end;

      --  We also require that the reference does not appear in a context
      --  where it is not sure to be executed (i.e. a conditional context
      --  or an exception handler).

      declare
         P : Node_Id;

      begin
         P := Parent (N);
         while Present (P) loop
            if Nkind (P) = N_If_Statement
                 or else
               Nkind (P) = N_Case_Statement
            then
               return False;
            else
               P := Parent (P);
            end if;
         end loop;
      end;

      --  OK, looks safe to set value

      return True;
   end Safe_To_Capture_Value;

   ---------------
   -- Same_Name --
   ---------------

   function Same_Name (N1, N2 : Node_Id) return Boolean is
      K1 : constant Node_Kind := Nkind (N1);
      K2 : constant Node_Kind := Nkind (N2);

   begin
      if (K1 = N_Identifier or else K1 = N_Defining_Identifier)
        and then (K2 = N_Identifier or else K2 = N_Defining_Identifier)
      then
         return Chars (N1) = Chars (N2);

      elsif (K1 = N_Selected_Component or else K1 = N_Expanded_Name)
        and then (K2 = N_Selected_Component or else K2 = N_Expanded_Name)
      then
         return Same_Name (Selector_Name (N1), Selector_Name (N2))
           and then Same_Name (Prefix (N1), Prefix (N2));

      else
         return False;
      end if;
   end Same_Name;

   ---------------
   -- Same_Type --
   ---------------

   function Same_Type (T1, T2 : Entity_Id) return Boolean is
   begin
      if T1 = T2 then
         return True;

      elsif not Is_Constrained (T1)
        and then not Is_Constrained (T2)
        and then Base_Type (T1) = Base_Type (T2)
      then
         return True;

      --  For now don't bother with case of identical constraints, to be
      --  fiddled with later on perhaps (this is only used for optimization
      --  purposes, so it is not critical to do a best possible job)

      else
         return False;
      end if;
   end Same_Type;

   ------------------------
   -- Scope_Is_Transient --
   ------------------------

   function Scope_Is_Transient  return Boolean is
   begin
      return Scope_Stack.Table (Scope_Stack.Last).Is_Transient;
   end Scope_Is_Transient;

   ------------------
   -- Scope_Within --
   ------------------

   function Scope_Within (Scope1, Scope2 : Entity_Id) return Boolean is
      Scop : Entity_Id;

   begin
      Scop := Scope1;
      while Scop /= Standard_Standard loop
         Scop := Scope (Scop);

         if Scop = Scope2 then
            return True;
         end if;
      end loop;

      return False;
   end Scope_Within;

   --------------------------
   -- Scope_Within_Or_Same --
   --------------------------

   function Scope_Within_Or_Same (Scope1, Scope2 : Entity_Id) return Boolean is
      Scop : Entity_Id;

   begin
      Scop := Scope1;
      while Scop /= Standard_Standard loop
         if Scop = Scope2 then
            return True;
         else
            Scop := Scope (Scop);
         end if;
      end loop;

      return False;
   end Scope_Within_Or_Same;

   --------------------
   -- Set_Convention --
   --------------------

   procedure Set_Convention (E : Entity_Id; Val : Snames.Convention_Id) is
   begin
      Basic_Set_Convention (E, Val);

      if Is_Type (E)
        and then Is_Access_Subprogram_Type (Base_Type (E))
        and then Has_Foreign_Convention (E)
      then

         --  A pragma Convention in an instance may apply to the subtype
         --  created for a formal, in which case we have already verified
         --  that conventions of actual and formal match and there is nothing
         --  to flag on the subtype.

         if In_Instance then
            null;
         else
            null; -- Set_Can_Use_Internal_Rep (E, False);
         end if;
      end if;

      --  If E is an object or component, and the type of E is an anonymous
      --  access type with no convention set, then also set the convention of
      --  the anonymous access type. We do not do this for anonymous protected
      --  types, since protected types always have the default convention.

      if Present (Etype (E))
        and then (Is_Object (E)
                   or else Ekind (E) = E_Component

                   --  Allow E_Void (happens for pragma Convention appearing
                   --  in the middle of a record applying to a component)

                   or else Ekind (E) = E_Void)
      then
         declare
            Typ : constant Entity_Id := Etype (E);

         begin
            if Ekind_In (Typ, E_Anonymous_Access_Type,
                              E_Anonymous_Access_Subprogram_Type)
              and then not Has_Convention_Pragma (Typ)
            then
               Basic_Set_Convention (Typ, Val);
               Set_Has_Convention_Pragma (Typ);

               --  And for the access subprogram type, deal similarly with the
               --  designated E_Subprogram_Type if it is also internal (which
               --  it always is?)

               if Ekind (Typ) = E_Anonymous_Access_Subprogram_Type then
                  declare
                     Dtype : constant Entity_Id := Designated_Type (Typ);
                  begin
                     if Ekind (Dtype) = E_Subprogram_Type
                       and then Is_Itype (Dtype)
                       and then not Has_Convention_Pragma (Dtype)
                     then
                        Basic_Set_Convention (Dtype, Val);
                        Set_Has_Convention_Pragma (Dtype);
                     end if;
                  end;
               end if;
            end if;
         end;
      end if;
   end Set_Convention;

   ------------------------
   -- Set_Current_Entity --
   ------------------------

   --  The given entity is to be set as the currently visible definition
   --  of its associated name (i.e. the Node_Id associated with its name).
   --  All we have to do is to get the name from the identifier, and
   --  then set the associated Node_Id to point to the given entity.

   procedure Set_Current_Entity (E : Entity_Id) is
   begin
      Set_Name_Entity_Id (Chars (E), E);
   end Set_Current_Entity;

   ---------------------------------
   -- Set_Entity_With_Style_Check --
   ---------------------------------

   procedure Set_Entity_With_Style_Check (N : Node_Id; Val : Entity_Id) is
      Val_Actual : Entity_Id;
      Nod        : Node_Id;

   begin
      Set_Entity (N, Val);

      if Style_Check
        and then not Suppress_Style_Checks (Val)
        and then not In_Instance
      then
         if Nkind (N) = N_Identifier then
            Nod := N;

         elsif Nkind (N) = N_Expanded_Name then
            Nod := Selector_Name (N);

         else
            return;
         end if;

         Val_Actual := Val;

         --  A special situation arises for derived operations, where we want
         --  to do the check against the parent (since the Sloc of the derived
         --  operation points to the derived type declaration itself).

         while not Comes_From_Source (Val_Actual)
           and then Nkind (Val_Actual) in N_Entity
           and then (Ekind (Val_Actual) = E_Enumeration_Literal
                      or else Is_Subprogram (Val_Actual)
                      or else Is_Generic_Subprogram (Val_Actual))
           and then Present (Alias (Val_Actual))
         loop
            Val_Actual := Alias (Val_Actual);
         end loop;

         --  Renaming declarations for generic actuals do not come from source,
         --  and have a different name from that of the entity they rename, so
         --  there is no style check to perform here.

         if Chars (Nod) = Chars (Val_Actual) then
            Style.Check_Identifier (Nod, Val_Actual);
         end if;
      end if;

      Set_Entity (N, Val);
   end Set_Entity_With_Style_Check;

   ------------------------
   -- Set_Name_Entity_Id --
   ------------------------

   procedure Set_Name_Entity_Id (Id : Name_Id; Val : Entity_Id) is
   begin
      Set_Name_Table_Info (Id, Int (Val));
   end Set_Name_Entity_Id;

   ---------------------
   -- Set_Next_Actual --
   ---------------------

   procedure Set_Next_Actual (Ass1_Id : Node_Id; Ass2_Id : Node_Id) is
   begin
      if Nkind (Parent (Ass1_Id)) = N_Parameter_Association then
         Set_First_Named_Actual (Parent (Ass1_Id), Ass2_Id);
      end if;
   end Set_Next_Actual;

   -----------------------
   -- Set_Public_Status --
   -----------------------

   procedure Set_Public_Status (Id : Entity_Id) is
      S : constant Entity_Id := Current_Scope;

   begin
      if S = Standard_Standard
        or else (Is_Public (S)
                  and then (Ekind (S) = E_Package
                             or else Is_Record_Type (S)
                             or else Ekind (S) = E_Void))
      then
         Set_Is_Public (Id);
      end if;
   end Set_Public_Status;

   ----------------------------
   -- Set_Scope_Is_Transient --
   ----------------------------

   procedure Set_Scope_Is_Transient (V : Boolean := True) is
   begin
      Scope_Stack.Table (Scope_Stack.Last).Is_Transient := V;
   end Set_Scope_Is_Transient;

   -------------------
   -- Set_Size_Info --
   -------------------

   procedure Set_Size_Info (T1, T2 : Entity_Id) is
   begin
      --  We copy Esize, but not RM_Size, since in general RM_Size is
      --  subtype specific and does not get inherited by all subtypes.

      Set_Esize                     (T1, Esize                     (T2));
      Set_Has_Biased_Representation (T1, Has_Biased_Representation (T2));

      if Is_Discrete_Or_Fixed_Point_Type (T1)
           and then
         Is_Discrete_Or_Fixed_Point_Type (T2)
      then
         Set_Is_Unsigned_Type       (T1, Is_Unsigned_Type          (T2));
      end if;
      Set_Alignment                 (T1, Alignment                 (T2));
   end Set_Size_Info;

   --------------------
   -- Static_Integer --
   --------------------

   function Static_Integer (N : Node_Id) return Uint is
   begin
      Analyze_And_Resolve (N, Any_Integer);

      if N = Error
        or else Error_Posted (N)
        or else Etype (N) = Any_Type
      then
         return No_Uint;
      end if;

      if Is_Static_Expression (N) then
         if not Raises_Constraint_Error (N) then
            return Expr_Value (N);
         else
            return No_Uint;
         end if;

      elsif Etype (N) = Any_Type then
         return No_Uint;

      else
         Flag_Non_Static_Expr
           ("static integer expression required here", N);
         return No_Uint;
      end if;
   end Static_Integer;

   --------------------------
   -- Statically_Different --
   --------------------------

   function Statically_Different (E1, E2 : Node_Id) return Boolean is
      R1 : constant Node_Id := Get_Referenced_Object (E1);
      R2 : constant Node_Id := Get_Referenced_Object (E2);

   begin
      return     Is_Entity_Name (R1)
        and then Is_Entity_Name (R2)
        and then Entity (R1) /= Entity (R2)
        and then not Is_Formal (Entity (R1))
        and then not Is_Formal (Entity (R2));
   end Statically_Different;

   -----------------------------
   -- Subprogram_Access_Level --
   -----------------------------

   function Subprogram_Access_Level (Subp : Entity_Id) return Uint is
   begin
      if Present (Alias (Subp)) then
         return Subprogram_Access_Level (Alias (Subp));
      else
         return Scope_Depth (Enclosing_Dynamic_Scope (Subp));
      end if;
   end Subprogram_Access_Level;

   -----------------
   -- Trace_Scope --
   -----------------

   procedure Trace_Scope (N : Node_Id; E : Entity_Id; Msg : String) is
   begin
      if Debug_Flag_W then
         for J in 0 .. Scope_Stack.Last loop
            Write_Str ("  ");
         end loop;

         Write_Str (Msg);
         Write_Name (Chars (E));
         Write_Str ("   line ");
         Write_Int (Int (Get_Logical_Line_Number (Sloc (N))));
         Write_Eol;
      end if;
   end Trace_Scope;

   -----------------------
   -- Transfer_Entities --
   -----------------------

   procedure Transfer_Entities (From : Entity_Id; To : Entity_Id) is
      Ent      : Entity_Id := First_Entity (From);

   begin
      if No (Ent) then
         return;
      end if;

      if (Last_Entity (To)) = Empty then
         Set_First_Entity (To, Ent);
      else
         Set_Next_Entity (Last_Entity (To), Ent);
      end if;

      Set_Last_Entity (To, Last_Entity (From));

      while Present (Ent) loop
         Set_Scope (Ent, To);

         if not Is_Public (Ent) then
            Set_Public_Status (Ent);

            if Is_Public (Ent)
              and then Ekind (Ent) = E_Record_Subtype

            then
               --  The components of the propagated Itype must be public
               --  as well.

               declare
                  Comp : Entity_Id;

               begin
                  Comp := First_Entity (Ent);

                  while Present (Comp) loop
                     Set_Is_Public (Comp);
                     Next_Entity (Comp);
                  end loop;
               end;
            end if;
         end if;

         Next_Entity (Ent);
      end loop;

      Set_First_Entity (From, Empty);
      Set_Last_Entity (From, Empty);
   end Transfer_Entities;

   -----------------------
   -- Type_Access_Level --
   -----------------------

   function Type_Access_Level (Typ : Entity_Id) return Uint is
      Btyp : Entity_Id;

   begin
      --  If the type is an anonymous access type we treat it as being
      --  declared at the library level to ensure that names such as
      --  X.all'access don't fail static accessibility checks.

      Btyp := Base_Type (Typ);
      if Ekind (Btyp) in Access_Kind then
         if Ekind (Btyp) = E_Anonymous_Access_Type then
            return Scope_Depth (Standard_Standard);
         end if;

         Btyp := Root_Type (Btyp);
      end if;

      return Scope_Depth (Enclosing_Dynamic_Scope (Btyp));
   end Type_Access_Level;

   ----------------------------
   -- Unique_Defining_Entity --
   ----------------------------

   function Unique_Defining_Entity (N : Node_Id) return Entity_Id is
   begin
      return Unique_Entity (Defining_Entity (N));
   end Unique_Defining_Entity;

   -------------------
   -- Unique_Entity --
   -------------------

   function Unique_Entity (E : Entity_Id) return Entity_Id is
      U : Entity_Id := E;
      P : Node_Id;

   begin
      case Ekind (E) is
         when E_Constant =>
            if Present (Full_View (E)) then
               U := Full_View (E);
            end if;

         when Formal_Kind =>
            if Present (Spec_Entity (E)) then
               U := Spec_Entity (E);
            end if;

         when E_Package_Body =>
            P := Parent (E);

            if Nkind (P) = N_Defining_Program_Unit_Name then
               P := Parent (P);
            end if;

            if Nkind (P) = N_Package_Body
              and then Present (Corresponding_Spec (P))
            then
               U := Corresponding_Spec (P);

            end if;

         when E_Subprogram_Body =>
            P := Parent (E);

            if Nkind (P) = N_Defining_Program_Unit_Name then
               P := Parent (P);
            end if;

            P := Parent (P);

            if Nkind (P) = N_Subprogram_Body
              and then Present (Corresponding_Spec (P))
            then
               U := Corresponding_Spec (P);

            elsif Nkind (P) = N_Subprogram_Renaming_Declaration then
               U := Corresponding_Spec (P);
            end if;

         when Type_Kind =>
            if Present (Full_View (E)) then
               U := Full_View (E);
            end if;

         when others =>
            null;
      end case;

      return U;
   end Unique_Entity;

   -----------------
   -- Unique_Name --
   -----------------

   function Unique_Name (E : Entity_Id) return String is

      --  Names of E_Subprogram_Body or E_Package_Body entities are not
      --  reliable, as they may not include the overloading suffix. Instead,
      --  when looking for the name of E or one of its enclosing scope, we get
      --  the name of the corresponding Unique_Entity.

      function Get_Scoped_Name (E : Entity_Id) return String;
      --  Return the name of E prefixed by all the names of the scopes to which
      --  E belongs, except for Standard.

      ---------------------
      -- Get_Scoped_Name --
      ---------------------

      function Get_Scoped_Name (E : Entity_Id) return String is
         Name : constant String := Get_Name_String (Chars (E));
      begin
         if Has_Fully_Qualified_Name (E)
           or else Scope (E) = Standard_Standard
         then
            return Name;
         else
            return Get_Scoped_Name (Unique_Entity (Scope (E))) & "__" & Name;
         end if;
      end Get_Scoped_Name;

   --  Start of processing for Unique_Name

   begin
      if E = Standard_Standard then
         return Get_Name_String (Name_Standard);

      elsif Scope (E) = Standard_Standard
        and then not (Ekind (E) = E_Package or else Is_Subprogram (E))
      then
         return Get_Name_String (Name_Standard) & "__" &
           Get_Name_String (Chars (E));

      elsif Ekind (E) = E_Enumeration_Literal then
         return Unique_Name (Etype (E)) & "__" & Get_Name_String (Chars (E));

      else
         return Get_Scoped_Name (Unique_Entity (E));
      end if;
   end Unique_Name;

   --------------------------
   -- Unit_Declaration_Node --
   --------------------------

   function Unit_Declaration_Node (Unit_Id : Entity_Id) return Node_Id is
      N : Node_Id := Parent (Unit_Id);

   begin
      --  Predefined operators do not have a full function declaration.

      if Ekind (Unit_Id) = E_Operator then
         return N;
      end if;

      while Nkind (N) /= N_Abstract_Subprogram_Declaration
        and then Nkind (N) /= N_Formal_Package_Declaration
        and then Nkind (N) /= N_Formal_Subprogram_Declaration
        and then Nkind (N) /= N_Function_Instantiation
        and then Nkind (N) /= N_Generic_Package_Declaration
        and then Nkind (N) /= N_Generic_Subprogram_Declaration
        and then Nkind (N) /= N_Package_Declaration
        and then Nkind (N) /= N_Package_Body
        and then Nkind (N) /= N_Package_Instantiation
        and then Nkind (N) /= N_Package_Renaming_Declaration
        and then Nkind (N) /= N_Procedure_Instantiation
        and then Nkind (N) /= N_Subprogram_Declaration
        and then Nkind (N) /= N_Subprogram_Body
        and then Nkind (N) /= N_Subprogram_Renaming_Declaration
        and then Nkind (N) not in N_Generic_Renaming_Declaration
      loop
         N := Parent (N);
         pragma Assert (Present (N));
      end loop;

      return N;
   end Unit_Declaration_Node;

   ------------------------------
   -- Universal_Interpretation --
   ------------------------------

   function Universal_Interpretation (Opnd : Node_Id) return Entity_Id is
      Index : Interp_Index;
      It    : Interp;

   begin
      --  The argument may be a formal parameter of an operator or subprogram
      --  with multiple interpretations, or else an expression for an actual.

      if Nkind (Opnd) = N_Defining_Identifier
        or else not Is_Overloaded (Opnd)
      then
         if Etype (Opnd) = Universal_Integer
           or else Etype (Opnd) = Universal_Real
         then
            return Etype (Opnd);
         else
            return Empty;
         end if;

      else
         Get_First_Interp (Opnd, Index, It);

         while Present (It.Typ) loop

            if It.Typ = Universal_Integer
              or else It.Typ = Universal_Real
            then
               return It.Typ;
            end if;

            Get_Next_Interp (Index, It);
         end loop;

         return Empty;
      end if;
   end Universal_Interpretation;

   ----------------------
   -- Within_Init_Proc --
   ----------------------

   function Within_Init_Proc return Boolean is

   begin
--        S := Current_Scope;
--        while not Is_Overloadable (S) loop
--           if S = Standard_Standard then
--              return False;
--           else
--              S := Scope (S);
--           end if;
--        end loop;

      return False; -- Is_Init_Proc (S);
   end Within_Init_Proc;

   ------------------
   -- Within_Scope --
   ------------------

   function Within_Scope (E : Entity_Id; S : Entity_Id) return Boolean is
   begin
      return Scope_Within_Or_Same (Scope (E), S);
   end Within_Scope;

   ----------------
   -- Wrong_Type --
   ----------------

   procedure Wrong_Type (Expr : Node_Id; Expected_Type : Entity_Id) is
      Found_Type : constant Entity_Id := First_Subtype (Etype (Expr));
      Expec_Type : constant Entity_Id := First_Subtype (Expected_Type);

      function Has_One_Matching_Field return Boolean;
      --  Determines whether Expec_Type is a record type with a single
      --  component or discriminant whose type matches the found type or
      --  is a one dimensional array whose component type matches the
      --  found type.

      function Has_One_Matching_Field return Boolean is
         E : Entity_Id;

      begin
         if Is_Array_Type (Expec_Type)
           and then Number_Dimensions (Expec_Type) = 1
           and then
             Covers (Etype (Component_Type (Expec_Type)), Found_Type)
         then
            return True;

         elsif not Is_Record_Type (Expec_Type) then
            return False;

         else
            E := First_Entity (Expec_Type);

            loop
               if No (E) then
                  return False;

               elsif (Chars (E) = Name_uTag
                           or else Chars (E) = Name_uParent)
               then
                  Next_Entity (E);

               else
                  exit;
               end if;
            end loop;

            if not Covers (Etype (E), Found_Type) then
               return False;

            elsif Present (Next_Entity (E)) then
               return False;

            else
               return True;
            end if;
         end if;
      end Has_One_Matching_Field;

   --  Start of processing for Wrong_Type

   begin
      --  Don't output message if either type is Any_Type, or if a message
      --  has already been posted for this node. We need to do the latter
      --  check explicitly (it is ordinarily done in Errout), because we
      --  are using ! to force the output of the error messages.

      if Expec_Type = Any_Type
        or else Found_Type = Any_Type
        or else Error_Posted (Expr)
      then
         return;

      --  In  an instance, there is an ongoing problem with completion of
      --  type derived from private types. Their structure is what Gigi
      --  expects, but the  Etype is the parent type rather than the
      --  derived private type itself. Do not flag error in this case. The
      --  private completion is an entity without a parent, like an Itype.
      --  Similarly, full and partial views may be incorrect in the instance.
      --  There is no simple way to insure that it is consistent ???

      elsif In_Instance then

         if Etype (Etype (Expr)) = Etype (Expected_Type)
           and then
             (Has_Private_Declaration (Expected_Type)
               or else Has_Private_Declaration (Etype (Expr)))
           and then No (Parent (Expected_Type))
         then
            return;
         end if;
      end if;

      --  An interesting special check. If the expression is parenthesized
      --  and its type corresponds to the type of the sole component of the
      --  expected record type, or to the component type of the expected one
      --  dimensional array type, then assume we have a bad aggregate attempt.

      if Nkind (Expr) in N_Subexpr
        and then Paren_Count (Expr) /= 0
        and then Has_One_Matching_Field
      then
         Error_Msg_N ("positional aggregate cannot have one component", Expr);

      --  Another special check, if we are looking for a pool-specific access
      --  type and we found an E_Access_Attribute_Type, then we have the case
      --  of an Access attribute being used in a context which needs a pool-
      --  specific type, which is never allowed. The one extra check we make
      --  is that the expected designated type covers the Found_Type.

      elsif Is_Access_Type (Expec_Type)
        and then Ekind (Found_Type) = E_Access_Attribute_Type
        and then Ekind (Base_Type (Expec_Type)) /= E_General_Access_Type
        and then Ekind (Base_Type (Expec_Type)) /= E_Anonymous_Access_Type
        and then Covers
          (Designated_Type (Expec_Type), Designated_Type (Found_Type))
      then
         Error_Msg_N ("result must be general access type!", Expr);
         Error_Msg_NE ("add ALL to }!", Expr, Expec_Type);

      --  If the expected type is an anonymous access type, as for access
      --  parameters and discriminants, the error is on the designated types.

      elsif Ekind (Expec_Type) = E_Anonymous_Access_Type then
         if Comes_From_Source (Expec_Type) then
            Error_Msg_NE ("expected}!", Expr, Expec_Type);
         else
            Error_Msg_NE
              ("expected an access type with designated}",
                 Expr, Designated_Type (Expec_Type));
         end if;

         if Is_Access_Type (Found_Type)
           and then not Comes_From_Source (Found_Type)
         then
            Error_Msg_NE
              ("found an access type with designated}!",
                Expr, Designated_Type (Found_Type));
         else
            if From_With_Type (Found_Type) then
               Error_Msg_NE ("found incomplete}!", Expr, Found_Type);
               Error_Msg_NE
                 ("\possibly missing with_clause on&", Expr,
                   Scope (Found_Type));
            else
               Error_Msg_NE ("found}!", Expr, Found_Type);
            end if;
         end if;

      --  Normal case of one type found, some other type expected

      else
         --  If the names of the two types are the same, see if some
         --  number of levels of qualification will help. Don't try
         --  more than three levels, and if we get to standard, it's
         --  no use (and probably represents an error in the compiler)
         --  Also do not bother with internal scope names.

         declare
            Expec_Scope : Entity_Id;
            Found_Scope : Entity_Id;

         begin
            Expec_Scope := Expec_Type;
            Found_Scope := Found_Type;

            for Levels in Int range 0 .. 3 loop
               if Chars (Expec_Scope) /= Chars (Found_Scope) then
                  Error_Msg_Qual_Level := Levels;
                  exit;
               end if;

               Expec_Scope := Scope (Expec_Scope);
               Found_Scope := Scope (Found_Scope);

               exit when Expec_Scope = Standard_Standard
                           or else
                         Found_Scope = Standard_Standard
                           or else
                         not Comes_From_Source (Expec_Scope)
                           or else
                         not Comes_From_Source (Found_Scope);
            end loop;
         end;

         Error_Msg_NE ("expected}!", Expr, Expec_Type);

         if Is_Entity_Name (Expr)
           and then Is_Package (Entity (Expr))
         then
            Error_Msg_N ("found package name!", Expr);

         elsif Is_Entity_Name (Expr)
           and then
             (Ekind (Entity (Expr)) = E_Procedure
                or else
              Ekind (Entity (Expr)) = E_Generic_Procedure)
         then
            Error_Msg_N ("found procedure name instead of function!", Expr);

         --  catch common error: a prefix or infix operator which is not
         --  directly visible because the type isn't.

         elsif Nkind (Expr) in N_Op
            and then Is_Overloaded (Expr)
            and then not Is_Immediately_Visible (Expec_Type)
            and then not Is_Potentially_Use_Visible (Expec_Type)
            and then not In_Use (Expec_Type)
            and then Has_Compatible_Type (Right_Opnd (Expr), Expec_Type)
         then
            Error_Msg_N (
              "operator of the type is not directly visible!", Expr);

         elsif Ekind (Found_Type) = E_Void
           and then Present (Parent (Found_Type))
           and then Nkind (Parent (Found_Type)) = N_Full_Type_Declaration
         then
            Error_Msg_NE ("found premature usage of}!", Expr, Found_Type);

         else
            Error_Msg_NE ("found}!", Expr, Found_Type);
         end if;

         Error_Msg_Qual_Level := 0;
      end if;
   end Wrong_Type;

   --------------------------
   -- Copy_Subprogram_Spec --
   --------------------------

   function Copy_Subprogram_Spec (Spec : Node_Id) return Node_Id is
      Def_Id      : Node_Id;
      Formal_Spec : Node_Id;
      Result      : Node_Id;

   begin
      --  The structure of the original tree must be replicated without any
      --  alterations. Use New_Copy_Tree for this purpose.

      Result := New_Copy_Tree (Spec);

      --  Create a new entity for the defining unit name

      Def_Id := Defining_Unit_Name (Result);
      Set_Defining_Unit_Name (Result,
        Make_Defining_Identifier (Sloc (Def_Id), Chars (Def_Id)));

      --  Create new entities for the formal parameters

      if Present (Parameter_Specifications (Result)) then
         Formal_Spec := First (Parameter_Specifications (Result));
         while Present (Formal_Spec) loop
            Def_Id := Defining_Identifier (Formal_Spec);
            Set_Defining_Identifier (Formal_Spec,
              Make_Defining_Identifier (Sloc (Def_Id), Chars (Def_Id)));

            Next (Formal_Spec);
         end loop;
      end if;

      return Result;
   end Copy_Subprogram_Spec;

end Sem_Util;
