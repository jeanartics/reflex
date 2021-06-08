------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ A T T R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2016, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with Atree;    use Atree;
with Casing;   use Casing;
with Checks;   use Checks;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Eval_Fat;
with Exp_Dist; use Exp_Dist;
with Exp_Util; use Exp_Util;
with Expander; use Expander;
with Freeze;   use Freeze;
with Gnatvsn;  use Gnatvsn;
with Itypes;   use Itypes;
with Lib;      use Lib;
with Lib.Xref; use Lib.Xref;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sdefault; use Sdefault;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Cat;  use Sem_Cat;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch10; use Sem_Ch10;
with Sem_Dim;  use Sem_Dim;
with Sem_Dist; use Sem_Dist;
with Sem_Elab; use Sem_Elab;
with Sem_Elim; use Sem_Elim;
with Sem_Eval; use Sem_Eval;
with Sem_Prag; use Sem_Prag;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Sem_Warn;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with System;
with Stringt;  use Stringt;
with Style;
with Stylesw;  use Stylesw;
with Targparm; use Targparm;
with Ttypes;   use Ttypes;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Uname;    use Uname;
with Urealp;   use Urealp;

package body Sem_Attr is

   True_Value  : constant Uint := Uint_1;
   False_Value : constant Uint := Uint_0;
   --  Synonyms to be used when these constants are used as Boolean values

   Bad_Attribute : exception;
   --  Exception raised if an error is detected during attribute processing,
   --  used so that we can abandon the processing so we don't run into
   --  trouble with cascaded errors.

   --  The following array is the list of attributes defined in the Ada 83 RM.
   --  In Ada 83 mode, these are the only recognized attributes. In other Ada
   --  modes all these attributes are recognized, even if removed in Ada 95.

   -----------------------
   -- Analyze_Attribute --
   -----------------------

   procedure Analyze_Attribute (N : Node_Id) is
      Loc     : constant Source_Ptr   := Sloc (N);
      Aname   : constant Name_Id      := Attribute_Name (N);
      P       : constant Node_Id      := Prefix (N);
      Exprs   : constant List_Id      := Expressions (N);
      Attr_Id : constant Attribute_Id := Get_Attribute_Id (Aname);
      E1      : Node_Id;
      E2      : Node_Id;

      P_Type : Entity_Id;
      --  Type of prefix after analysis

      P_Base_Type : Entity_Id;
      --  Base type of prefix after analysis

   begin
      --  Remaining processing depends on attribute

      case Attr_Id is

      --  Attributes related to Ada 2012 iterators. Attribute specifications
      --  exist for these, but they cannot be queried.

      when Attribute_Constant_Indexing    |
           Attribute_Default_Iterator     |
           Attribute_Implicit_Dereference |
           Attribute_Iterator_Element     |
           Attribute_Iterable             |
           Attribute_Variable_Indexing    =>
         Error_Msg_N ("illegal attribute", N);

      --  Internal attributes used to deal with Ada 2012 delayed aspects. These
      --  were already rejected by the parser. Thus they shouldn't appear here.

      when Internal_Attribute_Id =>
         raise Program_Error;

      ------------------
      -- Abort_Signal --
      ------------------

      when Attribute_Abort_Signal =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ------------
      -- Access --
      ------------

      when Attribute_Access =>
         Analyze_Access_Attribute;
         Check_Not_Incomplete_Type;

      -------------
      -- Address --
      -------------

      when Attribute_Address =>
         Check_E0;
         Address_Checks;
         Check_Not_Incomplete_Type;
         Set_Etype (N, RTE (RE_Address));

      ------------------
      -- Address_Size --
      ------------------

      when Attribute_Address_Size =>
         Standard_Attribute (System_Address_Size);

      --------------
      -- Adjacent --
      --------------

      when Attribute_Adjacent =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ---------
      -- Aft --
      ---------

      when Attribute_Aft =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ---------------
      -- Alignment --
      ---------------

      when Attribute_Alignment =>

         --  Don't we need more checking here, cf Size ???

         Check_E0;
         Check_Not_Incomplete_Type;
         Check_Not_CPP_Type;
         Set_Etype (N, Universal_Integer);

      ---------------
      -- Asm_Input --
      ---------------

      when Attribute_Asm_Input =>
         Check_Asm_Attribute;

         --  The back-end may need to take the address of E2

         if Is_Entity_Name (E2) then
            Set_Address_Taken (Entity (E2));
         end if;

         Set_Etype (N, RTE (RE_Asm_Input_Operand));

      ----------------
      -- Asm_Output --
      ----------------

      when Attribute_Asm_Output =>
         Check_Asm_Attribute;

         if Etype (E2) = Any_Type then
            return;

         elsif Aname = Name_Asm_Output then
            if not Is_Variable (E2) then
               Error_Attr
                 ("second argument for Asm_Output is not variable", E2);
            end if;
         end if;

         Note_Possible_Modification (E2, Sure => True);

         --  The back-end may need to take the address of E2

         if Is_Entity_Name (E2) then
            Set_Address_Taken (Entity (E2));
         end if;

         Set_Etype (N, RTE (RE_Asm_Output_Operand));

      -----------------------------
      -- Atomic_Always_Lock_Free --
      -----------------------------

      when Attribute_Atomic_Always_Lock_Free =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ----------
      -- Base --
      ----------

      --  Note: when the base attribute appears in the context of a subtype
      --  mark, the analysis is done by Sem_Ch8.Find_Type, rather than by
      --  the following circuit.

      when Attribute_Base => Base : declare
         Typ : Entity_Id;

      begin
         Check_E0;
         Find_Type (P);
         Typ := Entity (P);

         if Ada_Version >= Ada_95
           and then not Is_Scalar_Type (Typ)
           and then not Is_Generic_Type (Typ)
         then
            Error_Attr_P ("prefix of Base attribute must be scalar type");

         elsif Sloc (Typ) = Standard_Location
           and then Base_Type (Typ) = Typ
           and then Warn_On_Redundant_Constructs
         then
            Error_Msg_NE -- CODEFIX
              ("?r?redundant attribute, & is its own base type", N, Typ);
         end if;

         if Nkind (Parent (N)) /= N_Attribute_Reference then
            Error_Msg_Name_1 := Aname;
            Check_SPARK_05_Restriction
              ("attribute% is only allowed as prefix of another attribute", P);
         end if;

         Set_Etype (N, Base_Type (Entity (P)));
         Set_Entity (N, Base_Type (Entity (P)));
         Rewrite (N, New_Occurrence_Of (Entity (N), Loc));
         Analyze (N);
      end Base;

      ---------
      -- Bit --
      ---------

      when Attribute_Bit => Bit :
      begin
         Check_E0;

         if not Is_Object_Reference (P) then
            Error_Attr_P ("prefix for % attribute must be object");

         --  What about the access object cases ???

         else
            null;
         end if;

         Set_Etype (N, Universal_Integer);
      end Bit;

      ---------------
      -- Bit_Order --
      ---------------

      when Attribute_Bit_Order => Bit_Order :
      begin
         Check_E0;
         Check_Type;

         if not Is_Record_Type (P_Type) then
            Error_Attr_P ("prefix of % attribute must be record type");
         end if;

         if Bytes_Big_Endian xor Reverse_Bit_Order (P_Type) then
            Rewrite (N,
              New_Occurrence_Of (RTE (RE_High_Order_First), Loc));
         else
            Rewrite (N,
              New_Occurrence_Of (RTE (RE_Low_Order_First), Loc));
         end if;

         Set_Etype (N, RTE (RE_Bit_Order));
         Resolve (N);

         --  Reset incorrect indication of staticness

         Set_Is_Static_Expression (N, False);
      end Bit_Order;

      ------------------
      -- Bit_Position --
      ------------------

      --  Note: in generated code, we can have a Bit_Position attribute
      --  applied to a (naked) record component (i.e. the prefix is an
      --  identifier that references an E_Component or E_Discriminant
      --  entity directly, and this is interpreted as expected by Gigi.
      --  The following code will not tolerate such usage, but when the
      --  expander creates this special case, it marks it as analyzed
      --  immediately and sets an appropriate type.

      when Attribute_Bit_Position =>
         if Comes_From_Source (N) then
            Check_Component;
         end if;

         Set_Etype (N, Universal_Integer);

      ------------------
      -- Body_Version --
      ------------------

      when Attribute_Body_Version =>
         Check_E0;
         Check_Program_Unit;
         Set_Etype (N, RTE (RE_Version_String));

      --------------
      -- Callable --
      --------------

      when Attribute_Callable =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ------------
      -- Caller --
      ------------

      when Attribute_Caller => 
         Error_Msg_N ("attribute not supported by reflex", N);

      -------------
      -- Ceiling --
      -------------

      when Attribute_Ceiling =>
         Error_Msg_N ("attribute not supported by reflex", N);

      -----------
      -- Class --
      -----------

      when Attribute_Class =>
         Check_Restriction (No_Dispatch, N);
         Check_E0;
         Find_Type (N);

         --  Applying Class to untagged incomplete type is obsolescent in Ada
         --  2005. Note that we can't test Is_Tagged_Type here on P_Type, since
         --  this flag gets set by Find_Type in this situation.

         if Restriction_Check_Required (No_Obsolescent_Features)
           and then Ada_Version >= Ada_2005
           and then Ekind (P_Type) = E_Incomplete_Type
         then
            declare
               DN : constant Node_Id := Declaration_Node (P_Type);
            begin
               if Nkind (DN) = N_Incomplete_Type_Declaration
                 and then not Tagged_Present (DN)
               then
                  Check_Restriction (No_Obsolescent_Features, P);
               end if;
            end;
         end if;

      ------------------
      -- Code_Address --
      ------------------

      when Attribute_Code_Address =>
         Check_E0;

         if Nkind (P) = N_Attribute_Reference
           and then Nam_In (Attribute_Name (P), Name_Elab_Body, Name_Elab_Spec)
         then
            null;

         elsif not Is_Entity_Name (P)
           or else (Ekind (Entity (P)) /= E_Function
                      and then
                    Ekind (Entity (P)) /= E_Procedure)
         then
            Error_Attr ("invalid prefix for % attribute", P);
            Set_Address_Taken (Entity (P));

         --  Issue an error if the prefix denotes an eliminated subprogram

         else
            Check_For_Eliminated_Subprogram (P, Entity (P));
         end if;

         Set_Etype (N, RTE (RE_Address));

      ----------------------
      -- Compiler_Version --
      ----------------------

      when Attribute_Compiler_Version =>
         Check_E0;
         Check_Standard_Prefix;
         Rewrite (N, Make_String_Literal (Loc, "GNAT " & Gnat_Version_String));
         Analyze_And_Resolve (N, Standard_String);
         Set_Is_Static_Expression (N, True);

      --------------------
      -- Component_Size --
      --------------------

      when Attribute_Component_Size =>
         Check_E0;
         Set_Etype (N, Universal_Integer);

         --  Note: unlike other array attributes, unconstrained arrays are OK

         if Is_Array_Type (P_Type) and then not Is_Constrained (P_Type) then
            null;
         else
            Check_Array_Type;
         end if;

      -------------
      -- Compose --
      -------------

      when Attribute_Compose =>
         Error_Msg_N ("attribute not supported by reflex", N);

      -----------------
      -- Constrained --
      -----------------

      when Attribute_Constrained =>
         Check_E0;
         Set_Etype (N, Standard_Boolean);

         --  Case from RM J.4(2) of constrained applied to private type

         if Is_Entity_Name (P) and then Is_Type (Entity (P)) then
            Check_Restriction (No_Obsolescent_Features, P);

            if Warn_On_Obsolescent_Feature then
               Error_Msg_N
                 ("constrained for private type is an " &
                  "obsolescent feature (RM J.4)?j?", N);
            end if;

            --  If we are within an instance, the attribute must be legal
            --  because it was valid in the generic unit. Ditto if this is
            --  an inlining of a function declared in an instance.

            if In_Instance or else In_Inlined_Body then
               return;

            --  For sure OK if we have a real private type itself, but must
            --  be completed, cannot apply Constrained to incomplete type.

            elsif Is_Private_Type (Entity (P)) then

               --  Note: this is one of the Annex J features that does not
               --  generate a warning from -gnatwj, since in fact it seems
               --  very useful, and is used in the GNAT runtime.

               Check_Not_Incomplete_Type;
               return;
            end if;

         --  Normal (non-obsolescent case) of application to object of
         --  a discriminated type.

         else
            Check_Object_Reference (P);

            --  If N does not come from source, then we allow the
            --  the attribute prefix to be of a private type whose
            --  full type has discriminants. This occurs in cases
            --  involving expanded calls to stream attributes.

            if not Comes_From_Source (N) then
               P_Type := Underlying_Type (P_Type);
            end if;

            --  Must have discriminants or be an access type designating
            --  a type with discriminants. If it is a classwide type it
            --  has unknown discriminants.

            if Has_Discriminants (P_Type)
              or else Has_Unknown_Discriminants (P_Type)
              or else
                (Is_Access_Type (P_Type)
                  and then Has_Discriminants (Designated_Type (P_Type)))
            then
               return;

            --  The rule given in 3.7.2 is part of static semantics, but the
            --  intent is clearly that it be treated as a legality rule, and
            --  rechecked in the visible part of an instance. Nevertheless
            --  the intent also seems to be it should legally apply to the
            --  actual of a formal with unknown discriminants, regardless of
            --  whether the actual has discriminants, in which case the value
            --  of the attribute is determined using the J.4 rules. This choice
            --  seems the most useful, and is compatible with existing tests.

            elsif In_Instance then
               return;

            --  Also allow an object of a generic type if extensions allowed
            --  and allow this for any type at all. (this may be obsolete ???)

            elsif (Is_Generic_Type (P_Type)
                    or else Is_Generic_Actual_Type (P_Type))
              and then Extensions_Allowed
            then
               return;
            end if;
         end if;

         --  Fall through if bad prefix

         Error_Attr_P
           ("prefix of % attribute must be object of discriminated type");

      ---------------
      -- Copy_Sign --
      ---------------

      when Attribute_Copy_Sign =>
         Check_Floating_Point_Type_2;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);
         Resolve (E2, P_Base_Type);

      -----------
      -- Count --
      -----------

      when Attribute_Count => 
         Error_Msg_N ("attribute not supported by reflex", N);
	 
      -----------------------
      -- Default_Bit_Order --
      -----------------------

      when Attribute_Default_Bit_Order => Default_Bit_Order : declare
         Target_Default_Bit_Order : System.Bit_Order;

      begin
         Check_Standard_Prefix;

         if Bytes_Big_Endian then
            Target_Default_Bit_Order := System.High_Order_First;
         else
            Target_Default_Bit_Order := System.Low_Order_First;
         end if;

         Rewrite (N,
           Make_Integer_Literal (Loc,
             UI_From_Int (System.Bit_Order'Pos (Target_Default_Bit_Order))));

         Set_Etype (N, Universal_Integer);
         Set_Is_Static_Expression (N);
      end Default_Bit_Order;

      ----------------------------------
      -- Default_Scalar_Storage_Order --
      ----------------------------------

      when Attribute_Default_Scalar_Storage_Order => Default_SSO : declare
         RE_Default_SSO : RE_Id;

      begin
         Check_Standard_Prefix;

         case Opt.Default_SSO is
            when ' ' =>
               if Bytes_Big_Endian then
                  RE_Default_SSO := RE_High_Order_First;
               else
                  RE_Default_SSO := RE_Low_Order_First;
               end if;

            when 'H' =>
               RE_Default_SSO := RE_High_Order_First;

            when 'L' =>
               RE_Default_SSO := RE_Low_Order_First;

            when others =>
               raise Program_Error;
         end case;

         Rewrite (N, New_Occurrence_Of (RTE (RE_Default_SSO), Loc));
      end Default_SSO;

      --------------
      -- Definite --
      --------------

      when Attribute_Definite =>
         Error_Msg_N ("attribute not supported by reflex", N);

      -----------
      -- Delta --
      -----------

      when Attribute_Delta =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ------------
      -- Denorm --
      ------------

      when Attribute_Denorm =>
         Error_Msg_N ("attribute not supported by reflex", N);

      -----------
      -- Deref --
      -----------

      when Attribute_Deref =>
         Check_Type;
         Check_E1;
         Resolve (E1, RTE (RE_Address));
         Set_Etype (N, P_Type);

      ---------------------
      -- Descriptor_Size --
      ---------------------

      when Attribute_Descriptor_Size =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ------------
      -- Digits --
      ------------

      when Attribute_Digits =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ---------------
      -- Elab_Body --
      ---------------

      --  Also handles processing for Elab_Spec and Elab_Subp_Body

      when Attribute_Elab_Body      |
           Attribute_Elab_Spec      |
           Attribute_Elab_Subp_Body =>

         Check_E0;
         Check_Unit_Name (P);
         Set_Etype (N, Standard_Void_Type);

         --  We have to manually call the expander in this case to get
         --  the necessary expansion (normally attributes that return
         --  entities are not expanded).

         Expand (N);

      ---------------
      -- Elab_Spec --
      ---------------

      --  Shares processing with Elab_Body

      ----------------
      -- Elaborated --
      ----------------

      when Attribute_Elaborated =>
         Check_E0;
         Check_Unit_Name (P);
         Set_Etype (N, Standard_Boolean);

      ----------
      -- Emax --
      ----------

      when Attribute_Emax =>
         Error_Msg_N ("attribute not supported by reflex", N);

      -------------
      -- Enabled --
      -------------

      when Attribute_Enabled =>
         Check_Either_E0_Or_E1;

         if Present (E1) then
            if not Is_Entity_Name (E1) or else No (Entity (E1)) then
               Error_Msg_N ("entity name expected for Enabled attribute", E1);
               E1 := Empty;
            end if;
         end if;

         if Nkind (P) /= N_Identifier then
            Error_Msg_N ("identifier expected (check name)", P);
         elsif Get_Check_Id (Chars (P)) = No_Check_Id then
            Error_Msg_N ("& is not a recognized check name", P);
         end if;

         Set_Etype (N, Standard_Boolean);

      --------------
      -- Enum_Rep --
      --------------

      when Attribute_Enum_Rep => Enum_Rep : declare
      begin
         if Present (E1) then
            Check_E1;
            Check_Discrete_Type;
            Resolve (E1, P_Base_Type);

         else
            if not Is_Entity_Name (P)
              or else (not Is_Object (Entity (P))
                        and then Ekind (Entity (P)) /= E_Enumeration_Literal)
            then
               Error_Attr_P
                 ("prefix of % attribute must be " &
                  "discrete type/object or enum literal");
            end if;
         end if;

         Set_Etype (N, Universal_Integer);
      end Enum_Rep;

      --------------
      -- Enum_Val --
      --------------

      when Attribute_Enum_Val => Enum_Val : begin
         Check_E1;
         Check_Type;

         if not Is_Enumeration_Type (P_Type) then
            Error_Attr_P ("prefix of % attribute must be enumeration type");
         end if;

         --  If the enumeration type has a standard representation, the effect
         --  is the same as 'Val, so rewrite the attribute as a 'Val.

         if not Has_Non_Standard_Rep (P_Base_Type) then
            Rewrite (N,
              Make_Attribute_Reference (Loc,
                Prefix         => Relocate_Node (Prefix (N)),
                Attribute_Name => Name_Val,
                Expressions    => New_List (Relocate_Node (E1))));
            Analyze_And_Resolve (N, P_Base_Type);

         --  Non-standard representation case (enumeration with holes)

         else
            Check_Enum_Image;
            Resolve (E1, Any_Integer);
            Set_Etype (N, P_Base_Type);
         end if;
      end Enum_Val;

      -------------
      -- Epsilon --
      -------------

      when Attribute_Epsilon =>
         Error_Msg_N ("attribute not supported by reflex", N);

      --------------
      -- Exponent --
      --------------

      when Attribute_Exponent =>
         Check_Floating_Point_Type_1;
         Set_Etype (N, Universal_Integer);
         Resolve (E1, P_Base_Type);

      ------------------
      -- External_Tag --
      ------------------

      when Attribute_External_Tag =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ---------------
      -- Fast_Math --
      ---------------

      when Attribute_Fast_Math =>
         Error_Msg_N ("attribute not supported by reflex", N);

      -----------
      -- First --
      -----------

      when Attribute_First =>
         Check_Array_Or_Scalar_Type;
         Bad_Attribute_For_Predicate;

      ---------------
      -- First_Bit --
      ---------------

      when Attribute_First_Bit =>
         Check_Component;
         Set_Etype (N, Universal_Integer);

      -----------------
      -- First_Valid --
      -----------------

      when Attribute_First_Valid =>
         Check_First_Last_Valid;
         Set_Etype (N, P_Type);

      -----------------
      -- Fixed_Value --
      -----------------

      when Attribute_Fixed_Value =>
         Error_Msg_N ("attribute not supported by reflex", N);

      -----------
      -- Floor --
      -----------

      when Attribute_Floor =>
         Check_Floating_Point_Type_1;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);

      ----------
      -- Fore --
      ----------

      when Attribute_Fore =>
         Error_Msg_N ("attribute not supported by reflex", N);

      --------------
      -- Fraction --
      --------------

      when Attribute_Fraction =>
         Check_Floating_Point_Type_1;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);

      --------------
      -- From_Any --
      --------------

      when Attribute_From_Any =>
         Error_Msg_N ("attribute not supported by reflex", N);

      -----------------------
      -- Has_Access_Values --
      -----------------------

      when Attribute_Has_Access_Values =>
         Check_Type;
         Check_E0;
         Set_Etype (N, Standard_Boolean);

      ----------------------
      -- Has_Same_Storage --
      ----------------------

      when Attribute_Has_Same_Storage =>
         Check_E1;

         --  The arguments must be objects of any type

         Analyze_And_Resolve (P);
         Analyze_And_Resolve (E1);
         Check_Object_Reference (P);
         Check_Object_Reference (E1);
         Set_Etype (N, Standard_Boolean);

      -----------------------
      -- Has_Tagged_Values --
      -----------------------

      when Attribute_Has_Tagged_Values =>
         Check_Type;
         Check_E0;
         Set_Etype (N, Standard_Boolean);

      -----------------------
      -- Has_Discriminants --
      -----------------------

      when Attribute_Has_Discriminants =>
         Error_Msg_N ("attribute not supported by reflex", N);

      --------------
      -- Identity --
      --------------

      when Attribute_Identity =>
         Error_Msg_N ("attribute not supported by reflex", N);

      -----------
      -- Image --
      -----------

      when Attribute_Image => Image : begin
         Check_SPARK_05_Restriction_On_Attribute;

         --  AI12-00124-1 : The ARG has adopted the GNAT semantics of 'Img
         --  for scalar types, so that the prefix can be an object and not
         --  a type, and there is no need for an argument. Given this vote
         --  of confidence from the ARG, simplest is to transform this new
         --  usage of 'Image into a reference to 'Img.

         if Ada_Version > Ada_2005
           and then Is_Object_Reference (P)
           and then Is_Scalar_Type (P_Type)
         then
            Rewrite (N,
              Make_Attribute_Reference (Loc,
                Prefix         => Relocate_Node (P),
                Attribute_Name => Name_Img));
            Analyze (N);
            return;

         else
            Check_Scalar_Type;
         end if;

         Set_Etype (N, Standard_String);

         if Is_Real_Type (P_Type) then
            if Ada_Version = Ada_83 and then Comes_From_Source (N) then
               Error_Msg_Name_1 := Aname;
               Error_Msg_N
                 ("(Ada 83) % attribute not allowed for real types", N);
            end if;
         end if;

         if Is_Enumeration_Type (P_Type) then
            Check_Restriction (No_Enumeration_Maps, N);
         end if;

         Check_E1;
         Resolve (E1, P_Base_Type);
         Check_Enum_Image;
         Validate_Non_Static_Attribute_Function_Call;

         --  Check restriction No_Fixed_IO. Note the check of Comes_From_Source
         --  to avoid giving a duplicate message for Img expanded into Image.

         if Restriction_Check_Required (No_Fixed_IO)
           and then Comes_From_Source (N)
           and then Is_Fixed_Point_Type (P_Type)
         then
            Check_Restriction (No_Fixed_IO, P);
         end if;
      end Image;

      ---------
      -- Img --
      ---------

      when Attribute_Img => Img :
      begin
         Check_E0;
         Set_Etype (N, Standard_String);

         if not Is_Scalar_Type (P_Type)
           or else (Is_Entity_Name (P) and then Is_Type (Entity (P)))
         then
            Error_Attr_P
              ("prefix of % attribute must be scalar object name");
         end if;

         Check_Enum_Image;

         --  Check restriction No_Fixed_IO

         if Restriction_Check_Required (No_Fixed_IO)
           and then Is_Fixed_Point_Type (P_Type)
         then
            Check_Restriction (No_Fixed_IO, P);
         end if;
      end Img;

      -----------
      -- Input --
      -----------

      when Attribute_Input =>
         Error_Msg_N ("attribute not supported by reflex", N);

      -------------------
      -- Integer_Value --
      -------------------

      when Attribute_Integer_Value =>
         Error_Msg_N ("attribute not supported by reflex", N);

      -------------------
      -- Invalid_Value --
      -------------------

      when Attribute_Invalid_Value =>
         Check_E0;
         Check_Scalar_Type;
         Set_Etype (N, P_Base_Type);
         Invalid_Value_Used := True;

      -----------
      -- Large --
      -----------

      when Attribute_Large =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ----------
      -- Last --
      ----------

      when Attribute_Last =>
         Check_Array_Or_Scalar_Type;
         Bad_Attribute_For_Predicate;

      --------------
      -- Last_Bit --
      --------------

      when Attribute_Last_Bit =>
         Check_Component;
         Set_Etype (N, Universal_Integer);

      ----------------
      -- Last_Valid --
      ----------------

      when Attribute_Last_Valid =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ------------------
      -- Leading_Part --
      ------------------

      when Attribute_Leading_Part =>
         Check_Floating_Point_Type_2;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);
         Resolve (E2, Any_Integer);

      ------------
      -- Length --
      ------------

      when Attribute_Length =>
         Check_Array_Type;
         Set_Etype (N, Universal_Integer);

      -------------------
      -- Library_Level --
      -------------------

      when Attribute_Library_Level =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ---------------
      -- Lock_Free --
      ---------------

      when Attribute_Lock_Free =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ----------------
      -- Loop_Entry --
      ----------------

      when Attribute_Loop_Entry => 
         Error_Msg_N ("attribute not supported by reflex", N);

      -------------
      -- Machine --
      -------------

      when Attribute_Machine =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ------------------
      -- Machine_Emax --
      ------------------

      when Attribute_Machine_Emax =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ------------------
      -- Machine_Emin --
      ------------------

      when Attribute_Machine_Emin =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ----------------------
      -- Machine_Mantissa --
      ----------------------

      when Attribute_Machine_Mantissa =>
         Error_Msg_N ("attribute not supported by reflex", N);

      -----------------------
      -- Machine_Overflows --
      -----------------------

      when Attribute_Machine_Overflows =>
         Error_Msg_N ("attribute not supported by reflex", N);

      -------------------
      -- Machine_Radix --
      -------------------

      when Attribute_Machine_Radix =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ----------------------
      -- Machine_Rounding --
      ----------------------

      when Attribute_Machine_Rounding =>
         Error_Msg_N ("attribute not supported by reflex", N);

      --------------------
      -- Machine_Rounds --
      --------------------

      when Attribute_Machine_Rounds =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ------------------
      -- Machine_Size --
      ------------------

      when Attribute_Machine_Size =>
         Check_E0;
         Check_Type;
         Check_Not_Incomplete_Type;
         Set_Etype (N, Universal_Integer);

      --------------
      -- Mantissa --
      --------------

      when Attribute_Mantissa =>
         Check_E0;
         Check_Real_Type;
         Set_Etype (N, Universal_Integer);

      ---------
      -- Max --
      ---------

      when Attribute_Max =>
         Min_Max;

      ----------------------------------
      -- Max_Alignment_For_Allocation --
      ----------------------------------

      when Attribute_Max_Size_In_Storage_Elements =>
         Max_Alignment_For_Allocation_Max_Size_In_Storage_Elements;

      ----------------------------------
      -- Max_Size_In_Storage_Elements --
      ----------------------------------

      when Attribute_Max_Alignment_For_Allocation =>
         Max_Alignment_For_Allocation_Max_Size_In_Storage_Elements;

      -----------------------
      -- Maximum_Alignment --
      -----------------------

      when Attribute_Maximum_Alignment =>
         Standard_Attribute (Ttypes.Maximum_Alignment);

      --------------------
      -- Mechanism_Code --
      --------------------

      when Attribute_Mechanism_Code =>
         if not Is_Entity_Name (P)
           or else not Is_Subprogram (Entity (P))
         then
            Error_Attr_P ("prefix of % attribute must be subprogram");
         end if;

         Check_Either_E0_Or_E1;

         if Present (E1) then
            Resolve (E1, Any_Integer);
            Set_Etype (E1, Standard_Integer);

            if not Is_OK_Static_Expression (E1) then
               Flag_Non_Static_Expr
                 ("expression for parameter number must be static!", E1);
               Error_Attr;

            elsif UI_To_Int (Intval (E1)) > Number_Formals (Entity (P))
              or else UI_To_Int (Intval (E1)) < 0
            then
               Error_Attr ("invalid parameter number for % attribute", E1);
            end if;
         end if;

         Set_Etype (N, Universal_Integer);

      ---------
      -- Min --
      ---------

      when Attribute_Min =>
         Min_Max;

      ---------
      -- Mod --
      ---------

      when Attribute_Mod =>

         --  Note: this attribute is only allowed in Ada 2005 mode, but
         --  we do not need to test that here, since Mod is only recognized
         --  as an attribute name in Ada 2005 mode during the parse.

         Check_E1;
         Check_Modular_Integer_Type;
         Resolve (E1, Any_Integer);
         Set_Etype (N, P_Base_Type);

      -----------
      -- Model --
      -----------

      when Attribute_Model =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ----------------
      -- Model_Emin --
      ----------------

      when Attribute_Model_Emin =>
         Error_Msg_N ("attribute not supported by reflex", N);

      -------------------
      -- Model_Epsilon --
      -------------------

      when Attribute_Model_Epsilon =>
         Error_Msg_N ("attribute not supported by reflex", N);

      --------------------
      -- Model_Mantissa --
      --------------------

      when Attribute_Model_Mantissa =>
         Error_Msg_N ("attribute not supported by reflex", N);

      -----------------
      -- Model_Small --
      -----------------

      when Attribute_Model_Small =>
         Error_Msg_N ("attribute not supported by reflex", N);

      -------------
      -- Modulus --
      -------------

      when Attribute_Modulus =>
         Check_E0;
         Check_Modular_Integer_Type;
         Set_Etype (N, Universal_Integer);

      --------------------
      -- Null_Parameter --
      --------------------

      when Attribute_Null_Parameter => Null_Parameter : declare
         Parnt  : constant Node_Id := Parent (N);
         GParnt : constant Node_Id := Parent (Parnt);

         procedure Bad_Null_Parameter (Msg : String);
         --  Used if bad Null parameter attribute node is found. Issues
         --  given error message, and also sets the type to Any_Type to
         --  avoid blowups later on from dealing with a junk node.

         procedure Must_Be_Imported (Proc_Ent : Entity_Id);
         --  Called to check that Proc_Ent is imported subprogram

         ------------------------
         -- Bad_Null_Parameter --
         ------------------------

         procedure Bad_Null_Parameter (Msg : String) is
         begin
            Error_Msg_N (Msg, N);
            Set_Etype (N, Any_Type);
         end Bad_Null_Parameter;

         ----------------------
         -- Must_Be_Imported --
         ----------------------

         procedure Must_Be_Imported (Proc_Ent : Entity_Id) is
            Pent : constant Entity_Id := Ultimate_Alias (Proc_Ent);

         begin
            --  Ignore check if procedure not frozen yet (we will get
            --  another chance when the default parameter is reanalyzed)

            if not Is_Frozen (Pent) then
               return;

            elsif not Is_Imported (Pent) then
               Bad_Null_Parameter
                 ("Null_Parameter can only be used with imported subprogram");

            else
               return;
            end if;
         end Must_Be_Imported;

      --  Start of processing for Null_Parameter

      begin
         Check_Type;
         Check_E0;
         Set_Etype (N, P_Type);

         --  Case of attribute used as default expression

         if Nkind (Parnt) = N_Parameter_Specification then
            Must_Be_Imported (Defining_Entity (GParnt));

         --  Case of attribute used as actual for subprogram (positional)

         elsif Nkind (Parnt) in N_Subprogram_Call
            and then Is_Entity_Name (Name (Parnt))
         then
            Must_Be_Imported (Entity (Name (Parnt)));

         --  Case of attribute used as actual for subprogram (named)

         elsif Nkind (Parnt) = N_Parameter_Association
           and then Nkind (GParnt) in N_Subprogram_Call
           and then Is_Entity_Name (Name (GParnt))
         then
            Must_Be_Imported (Entity (Name (GParnt)));

         --  Not an allowed case

         else
            Bad_Null_Parameter
              ("Null_Parameter must be actual or default parameter");
         end if;
      end Null_Parameter;

      -----------------
      -- Object_Size --
      -----------------

      when Attribute_Object_Size =>
         Check_E0;
         Check_Type;
         Check_Not_Incomplete_Type;
         Set_Etype (N, Universal_Integer);

      ---------
      -- Old --
      ---------

      when Attribute_Old => Old : declare
         procedure Check_References_In_Prefix (Subp_Id : Entity_Id);
         --  Inspect the contents of the prefix and detect illegal uses of a
         --  nested 'Old, attribute 'Result or a use of an entity declared in
         --  the related postcondition expression. Subp_Id is the subprogram to
         --  which the related postcondition applies.

         --------------------------------
         -- Check_References_In_Prefix --
         --------------------------------

         procedure Check_References_In_Prefix (Subp_Id : Entity_Id) is
            function Check_Reference (Nod : Node_Id) return Traverse_Result;
            --  Detect attribute 'Old, attribute 'Result of a use of an entity
            --  and perform the appropriate semantic check.

            ---------------------
            -- Check_Reference --
            ---------------------

            function Check_Reference (Nod : Node_Id) return Traverse_Result is
            begin
               --  Attributes 'Old and 'Result cannot appear in the prefix of
               --  another attribute 'Old.

               if Nkind (Nod) = N_Attribute_Reference
                 and then Nam_In (Attribute_Name (Nod), Name_Old,
                                                        Name_Result)
               then
                  Error_Msg_Name_1 := Attribute_Name (Nod);
                  Error_Msg_Name_2 := Name_Old;
                  Error_Msg_N
                    ("attribute % cannot appear in the prefix of attribute %",
                     Nod);
                  return Abandon;

               --  Entities mentioned within the prefix of attribute 'Old must
               --  be global to the related postcondition. If this is not the
               --  case, then the scope of the local entity is nested within
               --  that of the subprogram.

               elsif Is_Entity_Name (Nod)
                 and then Present (Entity (Nod))
                 and then Scope_Within (Scope (Entity (Nod)), Subp_Id)
               then
                  Error_Attr
                    ("prefix of attribute % cannot reference local entities",
                     Nod);
                  return Abandon;

               --  Otherwise keep inspecting the prefix

               else
                  return OK;
               end if;
            end Check_Reference;

            procedure Check_References is new Traverse_Proc (Check_Reference);

         --  Start of processing for Check_References_In_Prefix

         begin
            Check_References (P);
         end Check_References_In_Prefix;

         --  Local variables

         Legal    : Boolean;
         Pref_Id  : Entity_Id;
         Pref_Typ : Entity_Id;
         Spec_Id  : Entity_Id;

      --  Start of processing for Old

      begin
         --  The attribute reference is a primary. If any expressions follow,
         --  then the attribute reference is an indexable object. Transform the
         --  attribute into an indexed component and analyze it.

         if Present (E1) then
            Rewrite (N,
              Make_Indexed_Component (Loc,
                Prefix      =>
                  Make_Attribute_Reference (Loc,
                    Prefix         => Relocate_Node (P),
                    Attribute_Name => Name_Old),
                Expressions => Expressions (N)));
            Analyze (N);
            return;
         end if;

         Analyze_Attribute_Old_Result (Legal, Spec_Id);

         --  The aspect or pragma where attribute 'Old resides should be
         --  associated with a subprogram declaration or a body. If this is not
         --  the case, then the aspect or pragma is illegal. Return as analysis
         --  cannot be carried out.

         --  The exception to this rule is when generating C since in this case
         --  postconditions are inlined.

         if No (Spec_Id)
           and then Modify_Tree_For_C
           and then In_Inlined_Body
         then
            Spec_Id := Entity (P);

         elsif not Legal then
            return;
         end if;

         --  The prefix must be preanalyzed as the full analysis will take
         --  place during expansion.

         Preanalyze_And_Resolve (P);

         --  Ensure that the prefix does not contain attributes 'Old or 'Result

         Check_References_In_Prefix (Spec_Id);

         --  Set the type of the attribute now to prevent cascaded errors

         Pref_Typ := Etype (P);
         Set_Etype (N, Pref_Typ);

         --  Legality checks

         if Is_Limited_Type (Pref_Typ) then
            Error_Attr ("attribute % cannot apply to limited objects", P);
         end if;

         --  The prefix is a simple name

         if Is_Entity_Name (P) and then Present (Entity (P)) then
            Pref_Id := Entity (P);

            --  Emit a warning when the prefix is a constant. Note that the use
            --  of Error_Attr would reset the type of N to Any_Type even though
            --  this is a warning. Use Error_Msg_XXX instead.

            if Is_Constant_Object (Pref_Id) then
               Error_Msg_Name_1 := Name_Old;
               Error_Msg_N
                 ("??attribute % applied to constant has no effect", P);
            end if;

         --  Otherwise the prefix is not a simple name

         else
            --  Ensure that the prefix of attribute 'Old is an entity when it
            --  is potentially unevaluated (6.1.1 (27/3)).

            if Is_Potentially_Unevaluated (N) then
               Uneval_Old_Msg;

            --  Detect a possible infinite recursion when the prefix denotes
            --  the related function.

            --    function Func (...) return ...
            --      with Post => Func'Old ...;

            --  The function may be specified in qualified form X.Y where X is
            --  a protected object and Y is a protected function. In that case
            --  ensure that the qualified form has an entity.

            elsif Nkind (P) = N_Function_Call
              and then Nkind (Name (P)) in N_Has_Entity
            then
               Pref_Id := Entity (Name (P));

               if Ekind_In (Spec_Id, E_Function, E_Generic_Function)
                 and then Pref_Id = Spec_Id
               then
                  Error_Msg_Warn := SPARK_Mode /= On;
                  Error_Msg_N ("!possible infinite recursion<<", P);
                  Error_Msg_N ("\!??Storage_Error ]<<", P);
               end if;
            end if;

            --  The prefix of attribute 'Old may refer to a component of a
            --  formal parameter. In this case its expansion may generate
            --  actual subtypes that are referenced in an inner context and
            --  that must be elaborated within the subprogram itself. If the
            --  prefix includes a function call, it may involve finalization
            --  actions that should be inserted when the attribute has been
            --  rewritten as a declaration. Create a declaration for the prefix
            --  and insert it at the start of the enclosing subprogram. This is
            --  an expansion activity that has to be performed now to prevent
            --  out-of-order issues.

            --  This expansion is both harmful and not needed in SPARK mode,
            --  since the formal verification backend relies on the types of
            --  nodes (hence is not robust w.r.t. a change to base type here),
            --  and does not suffer from the out-of-order issue described
            --  above. Thus, this expansion is skipped in SPARK mode.

            --  The expansion is not relevant for discrete types, which will
            --  not generate extra declarations, and where use of the base type
            --  may lead to spurious errors if context is a case.

            if not GNATprove_Mode then
               if not Is_Discrete_Type (Pref_Typ) then
                  Pref_Typ := Base_Type (Pref_Typ);
               end if;

               Set_Etype (N, Pref_Typ);
               Set_Etype (P, Pref_Typ);

               Analyze_Dimension (N);
               Expand (N);
            end if;
         end if;
      end Old;

      ----------------------
      -- Overlaps_Storage --
      ----------------------

      when Attribute_Overlaps_Storage =>
         Check_E1;

         --  Both arguments must be objects of any type

         Analyze_And_Resolve (P);
         Analyze_And_Resolve (E1);
         Check_Object_Reference (P);
         Check_Object_Reference (E1);
         Set_Etype (N, Standard_Boolean);

      ------------
      -- Output --
      ------------

      when Attribute_Output =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ------------------
      -- Partition_ID --
      ------------------

      when Attribute_Partition_ID => 
         Error_Msg_N ("attribute not supported by reflex", N);
	 
      -------------------------
      -- Passed_By_Reference --
      -------------------------

      when Attribute_Passed_By_Reference =>
         Check_E0;
         Check_Type;
         Set_Etype (N, Standard_Boolean);

      ------------------
      -- Pool_Address --
      ------------------

      when Attribute_Pool_Address =>
         Check_E0;
         Set_Etype (N, RTE (RE_Address));

      ---------
      -- Pos --
      ---------

      when Attribute_Pos =>
         Check_Discrete_Type;
         Check_E1;

         if Is_Boolean_Type (P_Type) then
            Error_Msg_Name_1 := Aname;
            Error_Msg_Name_2 := Chars (P_Type);
            Check_SPARK_05_Restriction
              ("attribute% is not allowed for type%", P);
         end if;

         Resolve (E1, P_Base_Type);
         Set_Etype (N, Universal_Integer);

      --------------
      -- Position --
      --------------

      when Attribute_Position =>
         Check_Component;
         Set_Etype (N, Universal_Integer);

      ----------
      -- Pred --
      ----------

      when Attribute_Pred =>
         Check_Scalar_Type;
         Check_E1;

         if Is_Real_Type (P_Type) or else Is_Boolean_Type (P_Type) then
            Error_Msg_Name_1 := Aname;
            Error_Msg_Name_2 := Chars (P_Type);
            Check_SPARK_05_Restriction
              ("attribute% is not allowed for type%", P);
         end if;

         Resolve (E1, P_Base_Type);
         Set_Etype (N, P_Base_Type);

         --  Since Pred works on the base type, we normally do no check for the
         --  floating-point case, since the base type is unconstrained. But we
         --  make an exception in Check_Float_Overflow mode.

         if Is_Floating_Point_Type (P_Type) then
            if not Range_Checks_Suppressed (P_Base_Type) then
               Set_Do_Range_Check (E1);
            end if;

         --  If not modular type, test for overflow check required

         else
            if not Is_Modular_Integer_Type (P_Type)
              and then not Range_Checks_Suppressed (P_Base_Type)
            then
               Enable_Range_Check (E1);
            end if;
         end if;

      --------------
      -- Priority --
      --------------

      --  Ada 2005 (AI-327): Dynamic ceiling priorities

      when Attribute_Priority =>
         Error_Msg_N ("attribute not supported by reflex", N);

      -----------
      -- Range --
      -----------

      when Attribute_Range =>
         Check_Array_Or_Scalar_Type;
         Bad_Attribute_For_Predicate;

         if Ada_Version = Ada_83
           and then Is_Scalar_Type (P_Type)
           and then Comes_From_Source (N)
         then
            Error_Attr
              ("(Ada 83) % attribute not allowed for scalar type", P);
         end if;

      ------------
      -- Result --
      ------------

      when Attribute_Result => 
         Error_Msg_N ("attribute not supported by reflex", N);

      ------------------
      -- Range_Length --
      ------------------

      when Attribute_Range_Length =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ----------
      -- Read --
      ----------

      when Attribute_Read =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ---------
      -- Ref --
      ---------

      when Attribute_Ref =>
         Check_E1;
         Analyze (P);

         if Nkind (P) /= N_Expanded_Name
           or else not Is_RTE (P_Type, RE_Address)
         then
            Error_Attr_P ("prefix of % attribute must be System.Address");
         end if;

         Analyze_And_Resolve (E1, Any_Integer);
         Set_Etype (N, RTE (RE_Address));

      ---------------
      -- Remainder --
      ---------------

      when Attribute_Remainder =>
         Check_Floating_Point_Type_2;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);
         Resolve (E2, P_Base_Type);

      ---------------------
      -- Restriction_Set --
      ---------------------

      when Attribute_Restriction_Set =>
         Error_Msg_N ("attribute not supported by reflex", N);

      -----------
      -- Round --
      -----------

      when Attribute_Round =>
         Check_E1;
         Check_Decimal_Fixed_Point_Type;
         Set_Etype (N, P_Base_Type);

         --  Because the context is universal_real (3.5.10(12)) it is a
         --  legal context for a universal fixed expression. This is the
         --  only attribute whose functional description involves U_R.

         if Etype (E1) = Universal_Fixed then
            declare
               Conv : constant Node_Id := Make_Type_Conversion (Loc,
                  Subtype_Mark => New_Occurrence_Of (Universal_Real, Loc),
                  Expression   => Relocate_Node (E1));

            begin
               Rewrite (E1, Conv);
               Analyze (E1);
            end;
         end if;

         Resolve (E1, Any_Real);

      --------------
      -- Rounding --
      --------------

      when Attribute_Rounding =>
         Check_Floating_Point_Type_1;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);

      ---------------
      -- Safe_Emax --
      ---------------

      when Attribute_Safe_Emax =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ----------------
      -- Safe_First --
      ----------------

      when Attribute_Safe_First =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ----------------
      -- Safe_Large --
      ----------------

      when Attribute_Safe_Large =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ---------------
      -- Safe_Last --
      ---------------

      when Attribute_Safe_Last =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ----------------
      -- Safe_Small --
      ----------------

      when Attribute_Safe_Small =>
         Error_Msg_N ("attribute not supported by reflex", N);

      --------------------------
      -- Scalar_Storage_Order --
      --------------------------

      when Attribute_Scalar_Storage_Order => Scalar_Storage_Order :
      declare
         Ent : Entity_Id := Empty;

      begin
         Check_E0;
         Check_Type;

         if not (Is_Record_Type (P_Type) or else Is_Array_Type (P_Type)) then

            --  In GNAT mode, the attribute applies to generic types as well
            --  as composite types, and for non-composite types always returns
            --  the default bit order for the target.

            if not (GNAT_Mode and then Is_Generic_Type (P_Type))
              and then not In_Instance
            then
               Error_Attr_P
                 ("prefix of % attribute must be record or array type");

            elsif not Is_Generic_Type (P_Type) then
               if Bytes_Big_Endian then
                  Ent := RTE (RE_High_Order_First);
               else
                  Ent := RTE (RE_Low_Order_First);
               end if;
            end if;

         elsif Bytes_Big_Endian xor Reverse_Storage_Order (P_Type) then
            Ent := RTE (RE_High_Order_First);

         else
            Ent := RTE (RE_Low_Order_First);
         end if;

         if Present (Ent) then
            Rewrite (N, New_Occurrence_Of (Ent, Loc));
         end if;

         Set_Etype (N, RTE (RE_Bit_Order));
         Resolve (N);

         --  Reset incorrect indication of staticness

         Set_Is_Static_Expression (N, False);
      end Scalar_Storage_Order;

      -----------
      -- Scale --
      -----------

      when Attribute_Scale =>
         Error_Msg_N ("attribute not supported by reflex", N);

      -------------
      -- Scaling --
      -------------

      when Attribute_Scaling =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ------------------
      -- Signed_Zeros --
      ------------------

      when Attribute_Signed_Zeros =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ----------
      -- Size --
      ----------

      when Attribute_Size | Attribute_VADS_Size => Size :
      begin
         Check_E0;

         --  If prefix is parameterless function call, rewrite and resolve
         --  as such.

         if Is_Entity_Name (P)
           and then Ekind (Entity (P)) = E_Function
         then
            Resolve (P);

         --  Similar processing for a protected function call

         elsif Nkind (P) = N_Selected_Component
           and then Ekind (Entity (Selector_Name (P))) = E_Function
         then
            Resolve (P);
         end if;

         if Is_Object_Reference (P) then
            Check_Object_Reference (P);

         elsif Is_Entity_Name (P)
           and then (Is_Type (Entity (P))
                       or else Ekind (Entity (P)) = E_Enumeration_Literal)
         then
            null;

         elsif Nkind (P) = N_Type_Conversion
           and then not Comes_From_Source (P)
         then
            null;

         --  Some other compilers allow dubious use of X'???'Size

         elsif Relaxed_RM_Semantics
           and then Nkind (P) = N_Attribute_Reference
         then
            null;

         else
            Error_Attr_P ("invalid prefix for % attribute");
         end if;

         Check_Not_Incomplete_Type;
         Check_Not_CPP_Type;
         Set_Etype (N, Universal_Integer);
      end Size;

      -----------
      -- Small --
      -----------

      when Attribute_Small =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ------------------
      -- Storage_Pool --
      ------------------

      when Attribute_Storage_Pool        |
           Attribute_Simple_Storage_Pool => Storage_Pool :
      begin
         Check_E0;

         if Is_Access_Type (P_Type) then
            if Ekind (P_Type) = E_Access_Subprogram_Type then
               Error_Attr_P
                 ("cannot use % attribute for access-to-subprogram type");
            end if;

            --  Set appropriate entity

            if Present (Associated_Storage_Pool (Root_Type (P_Type))) then
               Set_Entity (N, Associated_Storage_Pool (Root_Type (P_Type)));
            else
               Set_Entity (N, RTE (RE_Global_Pool_Object));
            end if;

            if Attr_Id = Attribute_Storage_Pool then
               if Present (Get_Rep_Pragma (Etype (Entity (N)),
                                           Name_Simple_Storage_Pool_Type))
               then
                  Error_Msg_Name_1 := Aname;
                     Error_Msg_Warn := SPARK_Mode /= On;
                  Error_Msg_N ("cannot use % attribute for type with simple "
                               & "storage pool<<", N);
                  Error_Msg_N ("\Program_Error [<<", N);

                  Rewrite
                    (N, Make_Raise_Program_Error
                          (Sloc (N), Reason => PE_Explicit_Raise));
               end if;

               Set_Etype (N, Class_Wide_Type (RTE (RE_Root_Storage_Pool)));

            --  In the Simple_Storage_Pool case, verify that the pool entity is
            --  actually of a simple storage pool type, and set the attribute's
            --  type to the pool object's type.

            else
               if not Present (Get_Rep_Pragma (Etype (Entity (N)),
                                               Name_Simple_Storage_Pool_Type))
               then
                  Error_Attr_P
                    ("cannot use % attribute for type without simple " &
                     "storage pool");
               end if;

               Set_Etype (N, Etype (Entity (N)));
            end if;

            --  Validate_Remote_Access_To_Class_Wide_Type for attribute
            --  Storage_Pool since this attribute is not defined for such
            --  types (RM E.2.3(22)).

            Validate_Remote_Access_To_Class_Wide_Type (N);

         else
            Error_Attr_P ("prefix of % attribute must be access type");
         end if;
      end Storage_Pool;

      ------------------
      -- Storage_Size --
      ------------------

      when Attribute_Storage_Size => Storage_Size :
      begin
         Check_E0;

         if Is_Task_Type (P_Type) then
            Set_Etype (N, Universal_Integer);

            --  Use with tasks is an obsolescent feature

            Check_Restriction (No_Obsolescent_Features, P);

         elsif Is_Access_Type (P_Type) then
            if Ekind (P_Type) = E_Access_Subprogram_Type then
               Error_Attr_P
                 ("cannot use % attribute for access-to-subprogram type");
            end if;

            if Is_Entity_Name (P)
              and then Is_Type (Entity (P))
            then
               Check_Type;
               Set_Etype (N, Universal_Integer);

               --   Validate_Remote_Access_To_Class_Wide_Type for attribute
               --   Storage_Size since this attribute is not defined for
               --   such types (RM E.2.3(22)).

               Validate_Remote_Access_To_Class_Wide_Type (N);

            --  The prefix is allowed to be an implicit dereference of an
            --  access value designating a task.

            else
               Check_Task_Prefix;
               Set_Etype (N, Universal_Integer);
            end if;

         else
            Error_Attr_P ("prefix of % attribute must be access or task type");
         end if;
      end Storage_Size;

      ------------------
      -- Storage_Unit --
      ------------------

      when Attribute_Storage_Unit =>
         Standard_Attribute (Ttypes.System_Storage_Unit);

      -----------------
      -- Stream_Size --
      -----------------

      when Attribute_Stream_Size =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ---------------
      -- Stub_Type --
      ---------------

      when Attribute_Stub_Type =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ----------
      -- Succ --
      ----------

      when Attribute_Succ =>
         Error_Msg_N ("attribute not supported by reflex", N);

      --------------------------------
      -- System_Allocator_Alignment --
      --------------------------------

      when Attribute_System_Allocator_Alignment =>
         Standard_Attribute (Ttypes.System_Allocator_Alignment);

      ---------
      -- Tag --
      ---------

      when Attribute_Tag => Tag :
      begin
         Check_E0;
         Check_Dereference;

         if not Is_Tagged_Type (P_Type) then
            Error_Attr_P ("prefix of % attribute must be tagged");

         --  Next test does not apply to generated code why not, and what does
         --  the illegal reference mean???

         elsif Is_Object_Reference (P)
           and then not Is_Class_Wide_Type (P_Type)
           and then Comes_From_Source (N)
         then
            Error_Attr_P
              ("% attribute can only be applied to objects " &
               "of class - wide type");
         end if;

         --  The prefix cannot be an incomplete type. However, references to
         --  'Tag can be generated when expanding interface conversions, and
         --  this is legal.

         if Comes_From_Source (N) then
            Check_Not_Incomplete_Type;
         end if;

         --  Set appropriate type

         Set_Etype (N, RTE (RE_Tag));
      end Tag;

      -----------------
      -- Target_Name --
      -----------------

      when Attribute_Target_Name => Target_Name : declare
         TN : constant String := Sdefault.Target_Name.all;
         TL : Natural;

      begin
         Check_Standard_Prefix;

         TL := TN'Last;

         if TN (TL) = '/' or else TN (TL) = '\' then
            TL := TL - 1;
         end if;

         Rewrite (N,
           Make_String_Literal (Loc,
             Strval => TN (TN'First .. TL)));
         Analyze_And_Resolve (N, Standard_String);
         Set_Is_Static_Expression (N, True);
      end Target_Name;

      ----------------
      -- Terminated --
      ----------------

      when Attribute_Terminated =>
         Check_E0;
         Set_Etype (N, Standard_Boolean);
         Check_Task_Prefix;

      ----------------
      -- To_Address --
      ----------------

      when Attribute_To_Address => To_Address : declare
         Val : Uint;

      begin
         Check_E1;
         Analyze (P);
         Check_System_Prefix;

         Generate_Reference (RTE (RE_Address), P);
         Analyze_And_Resolve (E1, Any_Integer);
         Set_Etype (N, RTE (RE_Address));

         if Is_Static_Expression (E1) then
            Set_Is_Static_Expression (N, True);
         end if;

         --  OK static expression case, check range and set appropriate type

         if Is_OK_Static_Expression (E1) then
            Val := Expr_Value (E1);

            if Val < -(2 ** UI_From_Int (Standard'Address_Size - 1))
                 or else
               Val > 2 ** UI_From_Int (Standard'Address_Size) - 1
            then
               Error_Attr ("address value out of range for % attribute", E1);
            end if;

            --  In most cases the expression is a numeric literal or some other
            --  address expression, but if it is a declared constant it may be
            --  of a compatible type that must be left on the node.

            if Is_Entity_Name (E1) then
               null;

            --  Set type to universal integer if negative

            elsif Val < 0 then
               Set_Etype (E1, Universal_Integer);

            --  Otherwise set type to Unsigned_64 to accomodate max values

            else
               Set_Etype (E1, Standard_Unsigned_64);
            end if;
         end if;

         Set_Is_Static_Expression (N, True);
      end To_Address;

      ------------
      -- To_Any --
      ------------

      when Attribute_To_Any =>
         Check_E1;
         Check_PolyORB_Attribute;
         Set_Etype (N, RTE (RE_Any));

      ----------------
      -- Truncation --
      ----------------

      when Attribute_Truncation =>
         Check_Floating_Point_Type_1;
         Resolve (E1, P_Base_Type);
         Set_Etype (N, P_Base_Type);

      ----------------
      -- Type_Class --
      ----------------

      when Attribute_Type_Class =>
         Check_E0;
         Check_Type;
         Check_Not_Incomplete_Type;
         Set_Etype (N, RTE (RE_Type_Class));

      --------------
      -- TypeCode --
      --------------

      when Attribute_TypeCode =>
         Check_E0;
         Check_PolyORB_Attribute;
         Set_Etype (N, RTE (RE_TypeCode));

      --------------
      -- Type_Key --
      --------------

      when Attribute_Type_Key =>
         Check_E0;
         Check_Type;

         --  This processing belongs in Eval_Attribute ???

         declare
            function Type_Key return String_Id;
            --  A very preliminary implementation. For now, a signature
            --  consists of only the type name. This is clearly incomplete
            --  (e.g., adding a new field to a record type should change the
            --  type's Type_Key attribute).

            --------------
            -- Type_Key --
            --------------

            function Type_Key return String_Id is
               Full_Name : constant String_Id :=
                             Fully_Qualified_Name_String (Entity (P));

            begin
               --  Copy all characters in Full_Name but the trailing NUL

               Start_String;
               for J in 1 .. String_Length (Full_Name) - 1 loop
                  Store_String_Char (Get_String_Char (Full_Name, Pos (J)));
               end loop;

               Store_String_Chars ("'Type_Key");
               return End_String;
            end Type_Key;

         begin
            Rewrite (N, Make_String_Literal (Loc, Type_Key));
         end;

         Analyze_And_Resolve (N, Standard_String);

      -----------------------
      -- Unbiased_Rounding --
      -----------------------

      when Attribute_Unbiased_Rounding =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ----------------------
      -- Unchecked_Access --
      ----------------------

      when Attribute_Unchecked_Access =>
         if Comes_From_Source (N) then
            Check_Restriction (No_Unchecked_Access, N);
         end if;

         Analyze_Access_Attribute;
         Check_Not_Incomplete_Type;

      -------------------------
      -- Unconstrained_Array --
      -------------------------

      when Attribute_Unconstrained_Array =>
         Check_E0;
         Check_Type;
         Check_Not_Incomplete_Type;
         Set_Etype (N, Standard_Boolean);
         Set_Is_Static_Expression (N, True);

      ------------------------------
      -- Universal_Literal_String --
      ------------------------------

      --  This is a GNAT specific attribute whose prefix must be a named
      --  number where the expression is either a single numeric literal,
      --  or a numeric literal immediately preceded by a minus sign. The
      --  result is equivalent to a string literal containing the text of
      --  the literal as it appeared in the source program with a possible
      --  leading minus sign.

      when Attribute_Universal_Literal_String => Universal_Literal_String :
      begin
         Check_E0;

         if not Is_Entity_Name (P)
           or else Ekind (Entity (P)) not in Named_Kind
         then
            Error_Attr_P ("prefix for % attribute must be named number");

         else
            declare
               Expr     : Node_Id;
               Negative : Boolean;
               S        : Source_Ptr;
               Src      : Source_Buffer_Ptr;

            begin
               Expr := Original_Node (Expression (Parent (Entity (P))));

               if Nkind (Expr) = N_Op_Minus then
                  Negative := True;
                  Expr := Original_Node (Right_Opnd (Expr));
               else
                  Negative := False;
               end if;

               if not Nkind_In (Expr, N_Integer_Literal, N_Real_Literal) then
                  Error_Attr
                    ("named number for % attribute must be simple literal", N);
               end if;

               --  Build string literal corresponding to source literal text

               Start_String;

               if Negative then
                  Store_String_Char (Get_Char_Code ('-'));
               end if;

               S := Sloc (Expr);
               Src := Source_Text (Get_Source_File_Index (S));

               while Src (S) /= ';' and then Src (S) /= ' ' loop
                  Store_String_Char (Get_Char_Code (Src (S)));
                  S := S + 1;
               end loop;

               --  Now we rewrite the attribute with the string literal

               Rewrite (N,
                 Make_String_Literal (Loc, End_String));
               Analyze (N);
               Set_Is_Static_Expression (N, True);
            end;
         end if;
      end Universal_Literal_String;

      -------------------------
      -- Unrestricted_Access --
      -------------------------

      --  This is a GNAT specific attribute which is like Access except that
      --  all scope checks and checks for aliased views are omitted. It is
      --  documented as being equivalent to the use of the Address attribute
      --  followed by an unchecked conversion to the target access type.

      when Attribute_Unrestricted_Access =>

         --  If from source, deal with relevant restrictions

         if Comes_From_Source (N) then
            Check_Restriction (No_Unchecked_Access, N);

            if Nkind (P) in N_Has_Entity
              and then Present (Entity (P))
              and then Is_Object (Entity (P))
            then
               Check_Restriction (No_Implicit_Aliasing, N);
            end if;
         end if;

         if Is_Entity_Name (P) then
            Set_Address_Taken (Entity (P));
         end if;

         --  It might seem reasonable to call Address_Checks here to apply the
         --  same set of semantic checks that we enforce for 'Address (after
         --  all we document Unrestricted_Access as being equivalent to the
         --  use of Address followed by an Unchecked_Conversion). However, if
         --  we do enable these checks, we get multiple failures in both the
         --  compiler run-time and in our regression test suite, so we leave
         --  out these checks for now. To be investigated further some time???

         --  Address_Checks;

         --  Now complete analysis using common access processing

         Analyze_Access_Attribute;

      ------------
      -- Update --
      ------------

      when Attribute_Update => 
         Error_Msg_N ("attribute not supported by reflex", N);

      ---------
      -- Val --
      ---------

      when Attribute_Val => Val : declare
      begin
         Check_E1;
         Check_Discrete_Type;

         if Is_Boolean_Type (P_Type) then
            Error_Msg_Name_1 := Aname;
            Error_Msg_Name_2 := Chars (P_Type);
            Check_SPARK_05_Restriction
              ("attribute% is not allowed for type%", P);
         end if;

         Resolve (E1, Any_Integer);
         Set_Etype (N, P_Base_Type);

         --  Note, we need a range check in general, but we wait for the
         --  Resolve call to do this, since we want to let Eval_Attribute
         --  have a chance to find an static illegality first.
      end Val;

      -----------
      -- Valid --
      -----------

      when Attribute_Valid =>
         Error_Msg_N ("attribute not supported by reflex", N);

      -------------------
      -- Valid_Scalars --
      -------------------

      when Attribute_Valid_Scalars =>
         Error_Msg_N ("attribute not supported by reflex", N);

      -----------
      -- Value --
      -----------

      when Attribute_Value => Value :
      begin
         Check_SPARK_05_Restriction_On_Attribute;
         Check_E1;
         Check_Scalar_Type;

         --  Case of enumeration type

         --  When an enumeration type appears in an attribute reference, all
         --  literals of the type are marked as referenced. This must only be
         --  done if the attribute reference appears in the current source.
         --  Otherwise the information on references may differ between a
         --  normal compilation and one that performs inlining.

         if Is_Enumeration_Type (P_Type)
           and then In_Extended_Main_Code_Unit (N)
         then
            Check_Restriction (No_Enumeration_Maps, N);

            --  Mark all enumeration literals as referenced, since the use of
            --  the Value attribute can implicitly reference any of the
            --  literals of the enumeration base type.

            declare
               Ent : Entity_Id := First_Literal (P_Base_Type);
            begin
               while Present (Ent) loop
                  Set_Referenced (Ent);
                  Next_Literal (Ent);
               end loop;
            end;
         end if;

         --  Set Etype before resolving expression because expansion of
         --  expression may require enclosing type. Note that the type
         --  returned by 'Value is the base type of the prefix type.

         Set_Etype (N, P_Base_Type);
         Validate_Non_Static_Attribute_Function_Call;

         --  Check restriction No_Fixed_IO

         if Restriction_Check_Required (No_Fixed_IO)
           and then Is_Fixed_Point_Type (P_Type)
         then
            Check_Restriction (No_Fixed_IO, P);
         end if;
      end Value;

      ----------------
      -- Value_Size --
      ----------------

      when Attribute_Value_Size =>
         Check_E0;
         Check_Type;
         Check_Not_Incomplete_Type;
         Set_Etype (N, Universal_Integer);

      -------------
      -- Version --
      -------------

      when Attribute_Version =>
         Check_E0;
         Check_Program_Unit;
         Set_Etype (N, RTE (RE_Version_String));

      ------------------
      -- Wchar_T_Size --
      ------------------

      when Attribute_Wchar_T_Size =>
         Standard_Attribute (Interfaces_Wchar_T_Size);

      ----------------
      -- Wide_Image --
      ----------------

      when Attribute_Wide_Image => Wide_Image :
      begin
         Check_SPARK_05_Restriction_On_Attribute;
         Check_Scalar_Type;
         Set_Etype (N, Standard_Wide_String);
         Check_E1;
         Resolve (E1, P_Base_Type);
         Validate_Non_Static_Attribute_Function_Call;

         --  Check restriction No_Fixed_IO

         if Restriction_Check_Required (No_Fixed_IO)
           and then Is_Fixed_Point_Type (P_Type)
         then
            Check_Restriction (No_Fixed_IO, P);
         end if;
      end Wide_Image;

      ---------------------
      -- Wide_Wide_Image --
      ---------------------

      when Attribute_Wide_Wide_Image => Wide_Wide_Image :
      begin
         Check_Scalar_Type;
         Set_Etype (N, Standard_Wide_Wide_String);
         Check_E1;
         Resolve (E1, P_Base_Type);
         Validate_Non_Static_Attribute_Function_Call;

         --  Check restriction No_Fixed_IO

         if Restriction_Check_Required (No_Fixed_IO)
           and then Is_Fixed_Point_Type (P_Type)
         then
            Check_Restriction (No_Fixed_IO, P);
         end if;
      end Wide_Wide_Image;

      ----------------
      -- Wide_Value --
      ----------------

      when Attribute_Wide_Value => Wide_Value :
      begin
         Check_SPARK_05_Restriction_On_Attribute;
         Check_E1;
         Check_Scalar_Type;

         --  Set Etype before resolving expression because expansion
         --  of expression may require enclosing type.

         Set_Etype (N, P_Type);
         Validate_Non_Static_Attribute_Function_Call;

         --  Check restriction No_Fixed_IO

         if Restriction_Check_Required (No_Fixed_IO)
           and then Is_Fixed_Point_Type (P_Type)
         then
            Check_Restriction (No_Fixed_IO, P);
         end if;
      end Wide_Value;

      ---------------------
      -- Wide_Wide_Value --
      ---------------------

      when Attribute_Wide_Wide_Value => Wide_Wide_Value :
      begin
         Check_E1;
         Check_Scalar_Type;

         --  Set Etype before resolving expression because expansion
         --  of expression may require enclosing type.

         Set_Etype (N, P_Type);
         Validate_Non_Static_Attribute_Function_Call;

         --  Check restriction No_Fixed_IO

         if Restriction_Check_Required (No_Fixed_IO)
           and then Is_Fixed_Point_Type (P_Type)
         then
            Check_Restriction (No_Fixed_IO, P);
         end if;
      end Wide_Wide_Value;

      ---------------------
      -- Wide_Wide_Width --
      ---------------------

      when Attribute_Wide_Wide_Width =>
         Check_E0;
         Check_Scalar_Type;
         Set_Etype (N, Universal_Integer);

      ----------------
      -- Wide_Width --
      ----------------

      when Attribute_Wide_Width =>
         Check_SPARK_05_Restriction_On_Attribute;
         Check_E0;
         Check_Scalar_Type;
         Set_Etype (N, Universal_Integer);

      -----------
      -- Width --
      -----------

      when Attribute_Width =>
         Error_Msg_N ("attribute not supported by reflex", N);

      ---------------
      -- Word_Size --
      ---------------

      when Attribute_Word_Size =>
         Standard_Attribute (System_Word_Size);

      -----------
      -- Write --
      -----------

      when Attribute_Write =>
         Error_Msg_N ("attribute not supported by reflex", N);

      end case;

   --  All errors raise Bad_Attribute, so that we get out before any further
   --  damage occurs when an error is detected (for example, if we check for
   --  one attribute expression, and the check succeeds, we want to be able
   --  to proceed securely assuming that an expression is in fact present.

   --  Note: we set the attribute analyzed in this case to prevent any
   --  attempt at reanalysis which could generate spurious error msgs.

   exception
      when Bad_Attribute =>
         Set_Analyzed (N);
         Set_Etype (N, Any_Type);
         return;
   end Analyze_Attribute;


end Sem_Attr;
