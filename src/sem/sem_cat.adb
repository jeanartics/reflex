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

with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
--with Exp_Tss;  use Exp_Tss;
with Fname;    use Fname;
with Lib;      use Lib;
with Nlists;   use Nlists;
with Sem;      use Sem;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Namet; use Namet;

package body Sem_Cat is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Check_Categorization_Dependencies
     (Unit_Entity     : Entity_Id;
      Depended_Entity : Entity_Id;
      Info_Node       : Node_Id;
      Is_Subunit      : Boolean);
   --  This procedure checks that the categorization of a lib unit and that
   --  of the depended unit satisfy dependency restrictions.
   --  The depended_entity can be the entity in a with_clause item, in which
   --  case Info_Node denotes that item. The depended_entity can also be the
   --  parent unit of a child unit, in which case Info_Node is the declaration
   --  of the child unit.  The error message is posted on Info_Node, and is
   --  specialized if Is_Subunit is true.

   procedure Check_Non_Static_Default_Expr
     (Type_Def : Node_Id;
      Obj_Decl : Node_Id);
   --  Iterate through the component list of a record definition, check
   --  that no component is declared with a nonstatic default value.
   --  If a nonstatic default exists, report an error on Obj_Decl.

   --  Iterate through the component list of a record definition, check
   --  that no component is declared with a non-static default value.

   ---------------------------------------
   -- Check_Categorization_Dependencies --
   ---------------------------------------

   procedure Check_Categorization_Dependencies
     (Unit_Entity     : Entity_Id;
      Depended_Entity : Entity_Id;
      Info_Node       : Node_Id;
      Is_Subunit      : Boolean)
   is
      N : constant Node_Id := Info_Node;

      type Categorization is
         (Pure, Shared_Passive, Remote_Types,
           Remote_Call_Interface, Pre_Elaborated, Normal);

      Unit_Category : Categorization;
      With_Category : Categorization;

      function Get_Categorization (E : Entity_Id) return Categorization;
      --  Check categorization flags from entity, and return in the form
      --  of a corresponding enumeration value.

      ------------------------
      -- Get_Categorization --
      ------------------------

      function Get_Categorization (E : Entity_Id) return Categorization is
      begin
         if Is_Preelaborated (E) then
            return Pre_Elaborated;
         elsif Is_Pure (E) then
            return Pure;
         else
            return Normal;
         end if;
      end Get_Categorization;

   --  Start of processing for Check_Categorization_Dependencies

   begin
      --  Intrinsic subprograms are preelaborated, so do not impose any
      --  categorization dependencies.

      if Is_Intrinsic_Subprogram (Depended_Entity) then
         return;
      end if;

      Unit_Category := Get_Categorization (Unit_Entity);
      With_Category := Get_Categorization (Depended_Entity);

      if With_Category > Unit_Category then

            Error_Msg_NE ("current unit cannot depend on&"
              & " (wrong categorization)", N, Depended_Entity);
      end if;

   end Check_Categorization_Dependencies;

   -----------------------------------
   -- Check_Non_Static_Default_Expr --
   -----------------------------------

   procedure Check_Non_Static_Default_Expr
     (Type_Def : Node_Id;
      Obj_Decl : Node_Id)
   is
      Recdef         : Node_Id;
      Component_Decl : Node_Id;

   begin
      if Nkind (Type_Def) = N_Derived_Type_Definition then
         Recdef := Record_Extension_Part (Type_Def);

         if No (Recdef) then
            return;
         end if;

      else
         Recdef := Type_Def;
      end if;

      --  Check that component declarations do not involve:

      --    a. a non-static default expression, where the object is
      --       declared to be default initialized.

      --    b. a dynamic Itype (discriminants and constraints)

      if Null_Present (Recdef) then
         return;
      else
         Component_Decl := First (Component_Items (Component_List (Recdef)));
      end if;

      while Present (Component_Decl)
        and then Nkind (Component_Decl) = N_Component_Declaration
      loop
         if Present (Expression (Component_Decl))
           and then Nkind (Expression (Component_Decl)) /= N_Null
           and then not Is_Static_Expression (Expression (Component_Decl))
         then
            Error_Msg_Sloc := Sloc (Component_Decl);
            Error_Msg_F
              ("object in preelaborated unit has non-static default#",
               Obj_Decl);

         --  Fix this later ???

         --  elsif Has_Dynamic_Itype (Component_Decl) then
         --     Error_Msg_N
         --       ("dynamic type discriminant," &
         --        " constraint in preelaborated unit",
         --        Component_Decl);
         end if;

         Next (Component_Decl);
      end loop;
   end Check_Non_Static_Default_Expr;

   ---------------------------
   -- In_Preelaborated_Unit --
   ---------------------------

   function In_Preelaborated_Unit return Boolean is
      Unit_Entity : constant Entity_Id := Current_Scope;
   begin
      return (Unit_Entity /= Standard_Standard)
        and then (Is_Preelaborated (Unit_Entity)
                    or else Is_Pure (Unit_Entity));
   end In_Preelaborated_Unit;

   ------------------
   -- In_Pure_Unit --
   ------------------

   function In_Pure_Unit return Boolean is
   begin
      return Is_Pure (Current_Scope);
   end In_Pure_Unit;

   ------------------------
   -- In_RCI_Declaration --
   ------------------------

   function In_RCI_Declaration (N : Node_Id) return Boolean is
      Unit_Entity : constant Entity_Id := Current_Scope;
      Unit_Kind   : constant Node_Kind :=
                      Nkind (Unit (Cunit (Current_Sem_Unit)));

   begin
      --  There are no restrictions on the private part or body
      --  of an RCI unit.

      return False;
   end In_RCI_Declaration;

   -----------------------
   -- In_RT_Declaration --
   -----------------------

   function In_RT_Declaration return Boolean is
      Unit_Entity : constant Entity_Id := Current_Scope;
      Unit_Kind   : constant Node_Kind :=
                      Nkind (Unit (Cunit (Current_Sem_Unit)));

   begin
      --  There are no restrictions on the body of a Remote Types unit.

      return False;
   end In_RT_Declaration;

   ----------------------------
   ---------------------------------------
   -- In_Subprogram_Task_Protected_Unit --
   ---------------------------------------

   function In_Subprogram_Task_Protected_Unit return Boolean is
      E : Entity_Id;

   begin
      --  The following is to verify that a declaration is inside
      --  subprogram, generic subprogram, task unit, protected unit.
      --  Used to validate if a lib. unit is Pure. RM 10.2.1(16).

      --  Use scope chain to check successively outer scopes

      E := Current_Scope;
      loop
         if Is_Subprogram (E)
              or else
            Is_Generic_Subprogram (E)
         then
            return True;

         elsif E = Standard_Standard then
            return False;
         end if;

         E := Scope (E);
      end loop;
   end In_Subprogram_Task_Protected_Unit;

   -------------------------------
   -- Is_Non_Remote_Access_Type --
   -------------------------------

   function Is_Non_Remote_Access_Type (E : Entity_Id) return Boolean is
   begin
      return Is_Access_Type (E);
   end Is_Non_Remote_Access_Type;

   -----------------------------------
   -- Set_Categorization_From_Scope --
   -----------------------------------

   procedure Set_Categorization_From_Scope (E : Entity_Id; Scop : Entity_Id) is
   begin
      Set_Is_Pure (E,
        Is_Pure (Scop) and then Is_Library_Level_Entity (E));
   end Set_Categorization_From_Scope;

   --------------------------------------
   -- Validate_Access_Type_Declaration --
   --------------------------------------

   procedure Validate_Access_Type_Declaration (T : Entity_Id; N : Node_Id) is
      Def : constant Node_Id := Type_Definition (N);

   begin
      case Nkind (Def) is
         when N_Access_To_Subprogram_Definition =>

            --  A pure library_item must not contain the declaration of a
            --  named access type, except within a subprogram, generic
            --  subprogram, task unit, or protected unit (RM 10.2.1(16)).

            if Comes_From_Source (T)
               and then In_Pure_Unit
               and then not In_Subprogram_Task_Protected_Unit
            then
               Error_Msg_N ("named access type not allowed in pure unit", T);
            end if;

         when N_Access_To_Object_Definition =>

            if Comes_From_Source (T)
              and then In_Pure_Unit
              and then not In_Subprogram_Task_Protected_Unit
            then
               Error_Msg_N
                 ("named access type not allowed in pure unit", T);
            end if;

            --  Check for shared passive unit type declaration. It should
            --  not contain the declaration of access to class wide type,
            --  access to task type and access to protected type with entry.

            --Validate_SP_Access_Object_Type_Decl (T);

         when others => null;
      end case;

   end Validate_Access_Type_Declaration;

   ----------------------------
   -- Validate_Ancestor_Part --
   ----------------------------

   procedure Validate_Ancestor_Part (N : Node_Id) is
      A : constant Node_Id   := Ancestor_Part (N);
      T : constant Entity_Id := Entity (A);

   begin
      if In_Preelaborated_Unit
        and then not In_Subprogram_Or_Concurrent_Unit
        and then (not Inside_A_Generic
                   or else Present (Enclosing_Generic_Body (N)))
      then
         --  We relax the restriction of 10.2.1(9) within GNAT
         --  units to allow packages such as Ada.Strings.Unbounded
         --  to be implemented (i.p., Null_Unbounded_String).
         --  (There are ACVC tests that check that the restriction
         --  is enforced, but note that AI-161, once approved,
         --  will relax the restriction prohibiting default-
         --  initialized objects of private and controlled
         --  types.)

         if Is_Private_Type (T)
           and then not Is_Internal_File_Name
                          (Unit_File_Name (Get_Source_Unit (N)))
         then
            Error_Msg_N
              ("private ancestor type not allowed in preelaborated unit", A);

         elsif Is_Record_Type (T) then
            if Nkind (Parent (T)) = N_Full_Type_Declaration then
               Check_Non_Static_Default_Expr
                 (Type_Definition (Parent (T)), A);
            end if;
         end if;
      end if;
   end Validate_Ancestor_Part;

   ----------------------------------------
   -- Validate_Categorization_Dependency --
   ----------------------------------------

   procedure Validate_Categorization_Dependency
     (N : Node_Id;
      E : Entity_Id)
   is
      K          : constant Node_Kind := Nkind (N);
      P          : Node_Id            := Parent (N);
      U          : Entity_Id := E;
   begin
      --  Only validate library units and subunits. For subunits, checks
      --  concerning withed units apply to the parent compilation unit.


      if Nkind (P) /= N_Compilation_Unit then
         return;
      end if;

      --  Ada0Y (AI-50217): Process explicit with_clauses that are not limited

      declare
         Item             : Node_Id;
         Entity_Of_Withed : Entity_Id;

      begin
         Item := First (Context_Items (P));

         while Present (Item) loop
            if Nkind (Item) = N_With_Clause
              and then not Implicit_With (Item)
            then
               Entity_Of_Withed := Entity (Name (Item));
               Check_Categorization_Dependencies
                (U, Entity_Of_Withed, Item, False);
            end if;

            Next (Item);
         end loop;
      end;

      --  Child depends on parent; therefore parent should also
      --  be categorized and satify the dependency hierarchy.

      --  Check if N is a child spec.

      if (K in N_Generic_Declaration              or else
          K in N_Generic_Instantiation            or else
          K in N_Generic_Renaming_Declaration     or else
          K =  N_Package_Declaration              or else
          K =  N_Package_Renaming_Declaration     or else
          K =  N_Subprogram_Declaration           or else
          K =  N_Subprogram_Renaming_Declaration)
        and then Present (Parent_Spec (N))
      then
         declare
            Parent_Lib_U  : constant Node_Id   := Parent_Spec (N);
            Parent_Kind   : constant Node_Kind :=
                              Nkind (Unit (Parent_Lib_U));
            Parent_Entity : Entity_Id;

         begin
            if        Parent_Kind =  N_Package_Instantiation
              or else Parent_Kind =  N_Procedure_Instantiation
              or else Parent_Kind =  N_Function_Instantiation
              or else Parent_Kind =  N_Package_Renaming_Declaration
              or else Parent_Kind in N_Generic_Renaming_Declaration
            then
               Parent_Entity := Defining_Entity (Unit (Parent_Lib_U));

            else
               Parent_Entity :=
                 Defining_Entity (Specification (Unit (Parent_Lib_U)));
            end if;

            Check_Categorization_Dependencies (E, Parent_Entity, N, False);

            --  Verify that public child of an RCI library unit
            --  must also be an RCI library unit (RM E.2.3(15)).

         end;
      end if;

   end Validate_Categorization_Dependency;

   --------------------------------
   -- Validate_Controlled_Object --
   --------------------------------

   procedure Validate_Controlled_Object (E : Entity_Id) is
   begin
      --  For now, never apply this check for internal GNAT units, since we
      --  have a number of cases in the library where we are stuck with objects
      --  of this type, and the RM requires Preelaborate.

      --  For similar reasons, we only do this check for source entities, since
      --  we generate entities of this type in some situations.

      --  Note that the 10.2.1(9) restrictions are not relevant to us anyway.
      --  We have to enforce them for RM compatibility, but we have no trouble
      --  accepting these objects and doing the right thing. Note that there is
      --  no requirement that Preelaborate not actually generate any code!

      if In_Preelaborated_Unit
        and then not Debug_Flag_PP
        and then Comes_From_Source (E)
        and then not
          Is_Internal_File_Name (Unit_File_Name (Get_Source_Unit (E)))
        and then (not Inside_A_Generic
                   or else Present (Enclosing_Generic_Body (E)))
      then
         Error_Msg_N
           ("library level controlled object not allowed in " &
            "preelaborated unit", E);
      end if;
   end Validate_Controlled_Object;

   --------------------------------------
   -- Validate_Null_Statement_Sequence --
   --------------------------------------

   procedure Validate_Null_Statement_Sequence (N : Node_Id) is
      Item : Node_Id;

   begin
      if In_Preelaborated_Unit then
         Item := First (Statements (Handled_Statement_Sequence (N)));

         while Present (Item) loop
            if Nkind (Item) /= N_Label
              and then Nkind (Item) /= N_Null_Statement
            then
               Error_Msg_N
                 ("statements not allowed in preelaborated unit", Item);
               exit;
            end if;

            Next (Item);
         end loop;
      end if;
   end Validate_Null_Statement_Sequence;

   ---------------------------------
   -- Validate_Object_Declaration --
   ---------------------------------

   procedure Validate_Object_Declaration (N : Node_Id) is
      Id  : constant Entity_Id  := Defining_Identifier (N);
      E   : constant Node_Id    := Expression (N);
      Odf : constant Node_Id    := Object_Definition (N);
      T   : constant Entity_Id  := Etype (Id);

   begin
      --  Check that if we are in preelaborated elaboration code, then we
      --  do not have an instance of a default initialized private, task or
      --  protected object declaration which would violate (RM 10.2.1(9)).
      --  Note that constants are never default initialized (and the test
      --  below also filters out deferred constants). A variable is default
      --  initialized if it does *not* have an initialization expression.

      --  Filter out cases that are not declaration of a variable from source

      if Nkind (N) /= N_Object_Declaration
        or else Constant_Present (N)
        or else not Comes_From_Source (Id)
      then
         return;
      end if;

      --  Exclude generic specs from the checks (this will get rechecked
      --  on instantiations).

      if Inside_A_Generic
        and then not Present (Enclosing_Generic_Body (Id))
      then
         return;
      end if;

      --  Required checks for declaration that is in a preelaborated
      --  package and is not within some subprogram.

      if In_Preelaborated_Unit
        and then not In_Subprogram_Or_Concurrent_Unit
      then
         --  Check for default initialized variable case. Note that in
         --  accordance with (RM B.1(24)) imported objects are not
         --  subject to default initialization.

         if No (E) and then not Is_Imported (Id) then
            declare
               Ent : Entity_Id := T;

            begin
               --  An array whose component type is a record with nonstatic
               --  default expressions is a violation, so we get the array's
               --  component type.

               if Is_Array_Type (Ent) then
                  declare
                     Comp_Type : Entity_Id := Component_Type (Ent);

                  begin
                     while Is_Array_Type (Comp_Type) loop
                        Comp_Type := Component_Type (Comp_Type);
                     end loop;

                     Ent := Comp_Type;
                  end;
               end if;

               --  Object decl. that is of record type and has no default expr.
               --  should check if there is any non-static default expression
               --  in component decl. of the record type decl.

               if Is_Record_Type (Ent) then
                  if Nkind (Parent (Ent)) = N_Full_Type_Declaration then
                     Check_Non_Static_Default_Expr
                       (Type_Definition (Parent (Ent)), N);

                  elsif Nkind (Odf) = N_Subtype_Indication
                    and then not Is_Array_Type (T)
                    and then not Is_Private_Type (T)
                  then
                     Check_Non_Static_Default_Expr (Type_Definition
                       (Parent (Entity (Subtype_Mark (Odf)))), N);
                  end if;
               end if;

               --  We relax the restriction of 10.2.1(9) within GNAT
               --  units. (There are ACVC tests that check that the
               --  restriction is enforced, but note that AI-161,
               --  once approved, will relax the restriction prohibiting
               --  default-initialized objects of private types, and
               --  will recommend a pragma for marking private types.)

               if (Is_Private_Type (Ent)
                    or else Depends_On_Private (Ent))
                 and then not Is_Internal_File_Name
                                (Unit_File_Name (Get_Source_Unit (N)))
               then
                  Error_Msg_N
                    ("private object not allowed in preelaborated unit", N);
                  return;

               --  Access to Task or Protected type

               elsif Is_Entity_Name (Odf)
                 and then Present (Etype (Odf))
                 and then Is_Access_Type (Etype (Odf))
               then
                  Ent := Designated_Type (Etype (Odf));

               elsif Is_Entity_Name (Odf) then
                  Ent := Entity (Odf);

               elsif Nkind (Odf) = N_Subtype_Indication then
                  Ent := Etype (Subtype_Mark (Odf));

               elsif
                  Nkind (Odf) = N_Constrained_Array_Definition
               then
                  Ent := Component_Type (T);

               --  else
               --     return;
               end if;
            end;
         end if;
      end if;

      --  A pure library_item must not contain the declaration of any
      --  variable except within  a subprogram, generic subprogram, task
      --  unit or protected unit (RM 10.2.1(16)).

      if In_Pure_Unit
      then
         Error_Msg_N ("declaration of variable not allowed in pure unit", N);
      end if;

   end Validate_Object_Declaration;

   ---------------------------------
   -- Validate_Static_Object_Name --
   ---------------------------------

   procedure Validate_Static_Object_Name (N : Node_Id) is
      E : Entity_Id;

      function Is_Primary (N : Node_Id) return Boolean;
      --  Determine whether node is syntactically a primary in an expression.

      function Is_Primary (N : Node_Id) return Boolean is
         K : constant Node_Kind := Nkind (Parent (N));

      begin
         case K is

            when N_Op | N_In | N_Not_In =>
               return True;

            when N_Aggregate
               | N_Component_Association
               | N_Index_Or_Discriminant_Constraint =>
               return True;

            when N_Attribute_Reference =>
               return Attribute_Name (Parent (N)) /= Name_Address
                 and then Attribute_Name (Parent (N)) /= Name_Access
                 and then Attribute_Name (Parent (N)) /= Name_Unchecked_Access
                 and then
                   Attribute_Name (Parent (N)) /= Name_Unrestricted_Access;

            when N_Indexed_Component =>
               return (N /= Prefix (Parent (N))
                 or else Is_Primary (Parent (N)));

            when N_Qualified_Expression | N_Type_Conversion =>
               return Is_Primary (Parent (N));

            when N_Assignment_Statement | N_Object_Declaration =>
               return (N = Expression (Parent (N)));

            when N_Selected_Component =>
               return Is_Primary (Parent (N));

            when others =>
               return False;
         end case;
      end Is_Primary;

   --  Start of processing for Validate_Static_Object_Name

   begin
      if not In_Preelaborated_Unit
        or else not Comes_From_Source (N)
        or else In_Subprogram_Or_Concurrent_Unit
        or else Ekind (Current_Scope) = E_Block
      then
         return;

      --  Filter out cases where primary is default in a component
      --  declaration, discriminant specification, or actual in a record
      --  type initialization call.

      --  Initialization call of internal types.

      elsif Nkind (Parent (N)) = N_Procedure_Call_Statement then

         if Present (Parent (Parent (N)))
           and then Nkind (Parent (Parent (N))) = N_Freeze_Entity
         then
            return;
         end if;

         if Nkind (Name (Parent (N))) = N_Identifier
           and then not Comes_From_Source (Entity (Name (Parent (N))))
         then
            return;
         end if;
      end if;

      --  Error if the name is a primary in an expression. The parent must not
      --  be an operator, or a selected component or an indexed component that
      --  is itself a primary. Entities that are actuals do not need to be
      --  checked, because the call itself will be diagnosed.

      if Is_Primary (N)
        and then (not Inside_A_Generic
                   or else Present (Enclosing_Generic_Body (N)))
      then
         if Ekind (Entity (N)) = E_Variable then
            Flag_Non_Static_Expr
              ("non-static object name in preelaborated unit", N);

         --  We take the view that a constant defined in another preelaborated
         --  unit is preelaborable, even though it may have a private type and
         --  thus appear non-static in a client. This must be the intent of
         --  the language, but currently is an RM gap.

         elsif Ekind (Entity (N)) = E_Constant
           and then not Is_Static_Expression (N)
         then
            E := Entity (N);

            if Is_Internal_File_Name (Unit_File_Name (Get_Source_Unit (N)))
              and then
                Enclosing_Lib_Unit_Node (N) /= Enclosing_Lib_Unit_Node (E)
              and then (Is_Preelaborated (Scope (E))
                          or else Is_Pure (Scope (E))
                          or else (Present (Renamed_Object (E))
                                     and then
                                       Is_Entity_Name (Renamed_Object (E))
                                     and then
                                       (Is_Preelaborated
                                         (Scope (Renamed_Object (E)))
                                            or else
                                              Is_Pure (Scope
                                                (Renamed_Object (E))))))
            then
               null;
            else
               Flag_Non_Static_Expr
                 ("non-static constant in preelaborated unit", N);
            end if;
         end if;
      end if;
   end Validate_Static_Object_Name;

end Sem_Cat;
