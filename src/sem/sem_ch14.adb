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
with Checks;   use Checks;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Lib;      use Lib;
with Lib.Xref; use Lib.Xref;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Case; use Sem_Case;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch5;  use Sem_Ch5;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Exp_Ch14; use Exp_Ch14;
--with Sem_Disp; use Sem_Disp;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Sem_Aux;  use Sem_Aux;
with Snames;   use Snames;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
--  with Style;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

with Ada.Text_Io; use Ada.Text_Io;
package body Sem_Ch14 is

   procedure Dump_Reactive_Formals (Spec : Entity_Id);

   procedure Check_Reactive_Parameters_Conformance
     (List1     : List_Id;
      List2     : List_Id;
      Error_Out : Boolean;
      Conform   : out Boolean);

   procedure Check_Reactive_Specification_Conformance
     (Spec_Decl : Entity_Id;
      Spec_Body : Entity_Id;
      Error_Out : Boolean;
      Conform   : out Boolean);

   function Reactive_Type_Conformant
     (E1 : Node_Id;
      E2 : Node_Id) return Boolean;


   function Find_Corresponding_Reactive_Spec
     (N          : Node_Id;
      Post_Error : Boolean := True) return Entity_Id;
   --  Find the spec corresponding to the reactive body N.

   procedure Analyze_Reactive_Statements (L : List_Id);

   ---------------------------------------------
   --  Check_Reactive_Parameters_Conformance  --
   ---------------------------------------------

   procedure Check_Reactive_Parameters_Conformance
     (List1     : List_Id;
      List2     : List_Id;
      Error_Out : Boolean;
      Conform   : out Boolean) is

      P1 : Node_id;
      P2 : Node_id;
      F1 : Entity_Id;
      F2 : Entity_Id;
      T1 : Node_id;
      T2 : Node_id;

   begin
      Put_Line ("Check_Reactive_Parameters_Conformance Begin");
      Conform := True;

      P1 := First (List1);
      P2 := First (List2);
      while Present (P1) and then Present (P2) loop
         F1 := Defining_Identifier (P1);
         T1 := Parameter_Type (P1);

         F2 := Defining_Identifier (P2);
         T2 := Parameter_Type (P2);

	 if not Present (F1)
	   or else not Present (T1)
	   or else not Present (F2)
	   or else not Present (T2)
	 then
	    null;

         elsif Chars (F1) /= Chars (F2) then
	    Conform := False;

	    if Error_Out then
	       Error_Msg_N ("\name & does not match!", F2);
	    else
	       return;
	    end if;

         elsif not Same_Name (T1, T2) then
	    Conform := False;
	    if Error_Out then
	       if Denotes_Same_Object (T1, T2) then
		  Error_Msg_N
		    ("not fully conformant with declaration#!", T2);

	       else
		  Error_Msg_N
		    ("not type conformant with declaration#!", T2);
	       end if;
	    else
	       return;
	    end if;

	 elsif (In_Present (P1) and then not In_Present (P2))
	   or else (not In_Present (P1) and then In_Present (P2))
	   or else (Out_Present (P1) and then not Out_Present (P2))
	   or else (not Out_Present (P2) and then Out_Present (P2))
	 then
	    Error_Msg_N
	      ("not fully conformant with declaration#!", F1);
	    Error_Msg_N
	      ("mode differs for parameter", F1);
	 end if;

	 if not Conform then
	    return;
	 end if;

        Next (P1);
        Next (P2);
      end loop;

      if Present (P1) or else Present (P2) then
	 Conform := False;
	 if Error_Out then
	    if Present (P1) then
	       F1 := Defining_Identifier (P1);
	       Error_Msg_N
		 ("missing argument for parameter", F1);

	    elsif Present (P2) then
	       F2 := Defining_Identifier (P2);
	       Error_Msg_N
		 ("missing argument for parameter", F2);
	    else
	       raise Program_Error;
	    end if;
	 end if;
      end if;
      Put_Line ("Check_Reactive_Parameters_Conformance End");
  end Check_Reactive_Parameters_Conformance;

   ------------------------------------------------
   --  Check_Reactive_Specification_Conformance  --
   ------------------------------------------------

   procedure Check_Reactive_Specification_Conformance
     (Spec_Decl : Node_Id;
      Spec_Body : Node_Id;
      Error_Out : Boolean;
      Conform   : out Boolean) is

      Name_Decl : Entity_Id := Defining_Identifier (Spec_Decl);
      Name_Body : Entity_Id := Defining_Identifier (Spec_Body);

      Init_Decl   : constant List_Id :=
        Initialize_Parameters_List (Spec_Decl);
      Interf_Decl : constant List_Id :=
        Interface_Parameters_List (Spec_Decl);

      Init_Body   : constant List_Id :=
        Initialize_Parameters_List (Spec_Body);
      Interf_Body : constant List_Id :=
        Interface_Parameters_List (Spec_Body);

   begin
      Conform := True;

      --  The two Id must have the same name.
      if Chars (Name_Decl) /= Chars (Name_Body) then
	 Conform := False;
	 if Error_Out then
	    Error_Msg_N ("\name & does not match!", Name_Body);
	 end if;

	 return;
      end if;

      --  The Initialization Parameters must conform
      Check_Reactive_Parameters_Conformance
        (Init_Decl, Init_Body, Error_Out, Conform);

      if not Error_Out and then not Conform then
	 return;
      end if;

      --  The Interface Parameters must conform
      Check_Reactive_Parameters_Conformance
	(Interf_Decl, Interf_Body, Error_Out, Conform);
   end Check_Reactive_Specification_Conformance;

   --------------------------------
   --  Reactive_Type_Conformant  --
   --------------------------------

   function Reactive_Type_Conformant
     (E1 : Node_Id;
      E2 : Node_Id) return Boolean is

      Conform : Boolean;
   begin
      Check_Reactive_Specification_Conformance (E1, E2, False, Conform);

      return Conform;
   end Reactive_Type_Conformant;

   --------------------------------------
   -- Find_Corresponding_Reactive_Spec --
   --------------------------------------

   function Find_Corresponding_Reactive_Spec
     (N          : Node_Id;
      Post_Error : Boolean := True) return Entity_Id
   is
      Spec       : constant Node_Id   := Specification (N);
      Designator : constant Entity_Id := Defining_Entity (Spec);
      Eparent    : Node_Id;
      E          : Entity_Id;
   begin
      E := Current_Entity_In_Scope (Designator);
      --while Present (E) loop

      --  We are looking for a matching spec. It must have the same scope,
      --  and the same name, and either be type conformant, or be the case
      --  of a library procedure spec and its body (which belong to one
      --  another regardless of whether they are type conformant or not).

      --if Scope (E) = Current_Scope then
      if Present (E) and then Ekind (E) = Ekind (Designator) then
	 Eparent := Parent (E);
	 if Present (Eparent)
	   and then Nkind (Eparent) = N_Reactive_Specification
	 then
	    --  if not Has_Completion (E) then
	    --     Set_Corresponding_Spec (N, E);
	    --  end if;

	    --  Set_Has_Completion (E);
	    return E;
	 end if;
      end if;
      --end if;

      --E := Homonym (E);
      --end loop;

      --  On exit, we know that no previous declaration of subprogram exists

      return Empty;
   end Find_Corresponding_Reactive_Spec;

   ------------------------------
   -- Process_Reactive_Formals --
   ------------------------------

   procedure Process_Reactive_Formals (T : List_Id) is
      Param_Spec  : Node_Id;
      Formal      : Entity_Id;
      Formal_Type : Entity_Id;
      Ptype       : Entity_Id;

      Num_Out_Params  : Nat       := 0;
      First_Out_Param : Entity_Id := Empty;
      --  Used for setting Is_Only_Out_Parameter

   begin
      Put_Line ("Process_Reactive_Formals Begin");
      --  In order to prevent premature use of the formals in the same formal
      --  part, the Ekind is left undefined until all default expressions are
      --  analyzed. The Ekind is established in a separate loop at the end.

      Param_Spec := First (T);
      while Present (Param_Spec) loop
         Formal := Defining_Identifier (Param_Spec);
         Set_Never_Set_In_Source (Formal, True);
         Enter_Name (Formal);

	 Find_Type (Parameter_Type (Param_Spec));
	 Ptype := Parameter_Type (Param_Spec);

	 if Ptype /= Error then
	    Formal_Type := Entity (Ptype);
	    Set_Etype (Formal, Formal_Type);
	 end if;

         Next (Param_Spec);
      end loop;

      Put_Line (" React Spec 1");
      --  Now set the kind (mode) of each formal

      Param_Spec := First (T);
      while Present (Param_Spec) loop
         Formal := Defining_Identifier (Param_Spec);
         Set_Formal_Mode (Formal);

         if Ekind (Formal) = E_In_Parameter then
            Set_Default_Value (Formal, Expression (Param_Spec));

         elsif Ekind (Formal) = E_Out_Parameter then
            Num_Out_Params := Num_Out_Params + 1;

            if Num_Out_Params = 1 then
               First_Out_Param := Formal;
            end if;
         end if;

         --  Skip remaining processing if formal type was in error

         if Etype (Formal) = Any_Type or else Error_Posted (Formal) then
            null;
	 else
	    --  if Convention (Formal_Type) = Convention_Ada_Pass_By_Copy then
	    --   Set_Mechanism (Formal, By_Copy);

	    --   elsif Convention (Formal_Type) = Convention_Ada_Pass_By_Reference then
	    --    Set_Mechanism (Formal, By_Reference);
            --  end if;
            null;
         end if;

         Next (Param_Spec);
      end loop;

      if Present (First_Out_Param) and then Num_Out_Params = 1 then
         Set_Is_Only_Out_Parameter (First_Out_Param);
      end if;
      Put_Line ("Process_Reactive_Formals End");
   end Process_Reactive_Formals;

   -------------------------------------
   --  Analyze_Reactive_Specification --
   -------------------------------------

   function Analyze_Reactive_Specification (N : Node_id) return Entity_Id is
      Designator         : constant Entity_Id := Defining_Entity (N);
      Initialize_Formals : constant List_Id := Initialize_Parameters_List (N);
      Interface_Formals  : constant List_Id := Interface_Parameters_List (N);
   begin
      --  Set_Contract (Designator, Make_Contract (Sloc (Designator)));

      Set_Ekind (Designator, E_Reactive_Type);
      Set_Etype (Designator, Standard_Void_Type);

      --  Introduce new scope for analysis of the formals and the return type

      Set_Scope (Designator, Current_Scope);

      if Present (Initialize_Formals) then
         Push_Scope (Designator);
         Process_Reactive_Formals (Initialize_Formals);
         End_Scope;
      end if;

      if Present (Interface_Formals) then
         Push_Scope (Designator);
         Process_Reactive_Formals (Interface_Formals);
         End_Scope;
      end if;

      return Designator;
   end Analyze_Reactive_Specification;

   -----------------------------------
   --  Analyze_Reactive_Declaration --
   -----------------------------------

   procedure Analyze_Reactive_Declaration (N : Node_id) is
      Loc        : constant Source_Ptr := Sloc (N);
      Scop       : constant Entity_Id  := Current_Scope;
      Designator : Entity_Id;
      Null_Body  : Node_Id := Empty;
      Spec       : Node_Id;
      T          : Entity_Id;
   begin
      Spec := Specification (N);
      Designator := Analyze_Reactive_Specification (Spec);

      --  A reference may already have been generated for the unit name, in
      --  which case the following call is redundant. However it is needed for
      --  declarations that are the rewriting of an expression function.

      T := Find_Type_Name (Spec);

      Set_Ekind (Designator, E_Reactive_Type);
      Set_Etype (Designator, Designator);
      --  Set_Body_Required (Parent (N), True);

      Set_Analyzed (N);

      --  if Reactive_Operating_Mode = Expand_Reactive_Mode then
      --     Expand_Reactive_Declaration (N);
      --  end if;

      --  elsif Reactive_Operating_Mode = Replace_Reactive_Mode then
      --  	 null; --  Add_Expanded_Reactive_Declaration (N);
   end Analyze_Reactive_Declaration;

   ----------------------------
   --  Analyze_Reactive_Body --
   ----------------------------

   procedure Analyze_Reactive_Body (N : Node_Id) is
      Loc         : constant Source_Ptr := Sloc (N);
      Body_Spec   : constant Node_Id    := Specification (N);
      Body_Id     : Entity_Id
	:= Defining_Entity (Body_Spec);
      Body_Desc   : Node_Id := Body_Description (N);
      Prev_Id     : Entity_Id; --   :=
      --  Current_Entity_In_Scope (Body_Id);
      Spec_Spec   : Node_Id;
      Spec_Id     : Entity_Id;
      Conform     : Boolean;
      React       : Node_Id;
      Flow        : Node_Id;
      init        : Node_Id;
      Final       : Node_Id;
      --  Stmts       : List_Id;
   begin
      Put_Line ("Analyze_Reactive_Body started for " & N'Img);
      Put_Line ("    Body_Desc = " & Body_Desc'Img);
      Put_Line ("    Parent of Body_Desc = " & Parent (Body_Desc)'Img);

      Prev_Id := Current_Entity_In_Scope (Body_Id);

      if Prev_Id = Body_Id
	and then Has_Completion (Body_Id)
      then
	 return;
      end if;

      --  if Present (Prev_Id) then
      --   Enter_Name (Body_Id);
      --   return;
      --  end if;

      Body_Id := Analyze_Reactive_Specification (Body_Spec);
      Put_Line (" reactive Body => Find_Corresponding_Reactive_Spec");
      Spec_Id := Find_Corresponding_Reactive_Spec (N);
      Put_Line (" reactive Body => Find_Corresponding_Reactive_Spec End");

      --  Place subprogram on scope stack, and make formals visible. If there
      --  is a spec, the visible entity remains that of the spec.

      if Present (Spec_Id) then
	 Dump_Reactive_Formals (Spec_Id);

	 if Has_Completion (Spec_Id)
	   and then Present (Corresponding_Body
			       (Unit_Declaration_Node (Spec_Id)))
	 then
	    Error_Msg_NE ("duplicate body for reactive&", N, Spec_Id);
	 end if;

	 Set_Convention (Body_Id, Convention (Spec_Id));
	 Set_Has_Completion (Spec_Id);

	 --  If this is a body generated for a renaming, do not check for
	 --  full conformance. The check is redundant, because the spec of
	 --  the body is a copy of the spec in the renaming declaration,
	 --  and the test can lead to spurious errors on nested defaults.

	 Spec_Spec := Parent (Spec_Id);
	 Check_Reactive_Specification_Conformance
	   (Body_Spec,
	    Spec_Spec,
	    True,
	    Conform);
	 Put_Line (" reactive Body => End conformanace End");

	 --  If the body is not fully conformant, we have to decide if we
	 --  should analyze it or not. If it has a really messed up profile
	 --  then we probably should not analyze it, since we will get too
	 --  many bogus messages.

	 --  Our decision is to go ahead in the non-fully conformant case
	 --  only if it is at least mode conformant with the spec. Note
	 --  that the call to Check_Fully_Conformant has issued the proper
	 --  error messages to complain about the lack of conformance.

	 --   if not Conformant
	 --     and then not Mode_Conformant (Body_Id, Spec_Id)
	 --   then
	 --      return;
	 --   end if;
	    --  end if;
      end if;

	 --  Initailze expander body list
      Set_Expand_Declarations (N, New_List);

      Dump_Reactive_Formals (Spec_Id);

      Install_Reactive_Formals (Spec_Id);
      Push_Scope (Spec_Id);
      Set_Corresponding_Spec (N, Spec_Id);
      Set_Corresponding_Body (Unit_Declaration_Node (Spec_Id), Body_Id);
      Set_Has_Completion (Spec_Id);

      Set_Ekind (Body_Id, E_Reactive_Body);
      Set_Etype (Body_Id, Etype (Spec_Id));

      --  Install_Visible_Declarations (Spec_Id);
      --  Install_Private_Declarations (Spec_Id);

      Put_Line (" React Body Before Declarations");

      if not Is_Empty_List (Declarations (Body_Desc)) then
	 Analyze_Declarations (Declarations (Body_Desc));
      end if;

      Put_Line (" React Body After Declarations");
      --  Set_State_Count (react, 0);
      React := Reaction_Proc (Body_Desc);
      if Present (React) then
	 --  Stmts := Statements (Handled_Statement_Sequence  (React));
	 --  Analyze_Reactive_Statements (Stmts);
	 Analyze (React);
      else
	 --  Error_Msg_N ("no reaction statements#!", T2);
	 null;
      end if;

      Flow := Flow_Proc (Body_Desc);
      if Present (Flow) then
	 Analyze (Flow);
      else
	 --  Error_Msg_N ("no flow statements#!", T2);
	 null;
      end if;

      Init := Initialize_Proc (Body_Desc);
      if Present (Init) then
	 --  Stmts := Statements (Handled_Statement_Sequence  (Init));
	 --  Analyze_Reactive_Statements (Stmts);
	 Analyze (Init);
      else
	 --  Error_Msg_N ("no flow statements#!", T2);
	 null;
      end if;

      Final := Finalize_Proc (Body_Desc);
      if Present (Final) then
	 Analyze (Final);
      else
	 --  Error_Msg_N ("no flow statements#!", T2);
	 null;
      end if;

      --  if Reactive_Operating_Mode = Expand_Reactive_Mode then
      --     Rn := Make_Reactive_Graph (N);
      --     Expand_Reactive_Body (N);
      --  end if;

      End_Scope;

      Set_Analyzed (N);

      Put_Line ("Analyze_Reactive_Body ended for " & N'Img);
      Put_Line ("    Body_Desc = " & Body_Desc'Img);
      Put_Line ("    Parent of Body_Desc = " & Parent (Body_Desc)'Img);
      Put_Line ("*******************************************************");

   end Analyze_Reactive_Body;

   ----------------------------------------
   --  Analyze_Reactive_Body_Description --
   ----------------------------------------

   procedure Analyze_Reactive_Body_Description (N : Node_Id) is
   begin
      null;
   end Analyze_Reactive_Body_Description;

   ------------------------------------------
   -- Analyze_Reactive_Entry_Body_Reaction --
   ------------------------------------------

   procedure Analyze_Reactive_Reaction_Proc (N : Node_Id) is
   begin
      --  Analyze Reaction Statements.

      Analyze_Statements (Statements (Handled_Statement_Sequence (N)));

      --  Create the associated procedure

      --  Expand the reactive code

      -- Analyze the procedure

   end Analyze_Reactive_Reaction_Proc;

   --------------------------------
   -- Analyze_Reactive_Flow_Proc --
   --------------------------------

   procedure Analyze_Reactive_Flow_Proc (N : Node_Id) is
   begin
      --  Analyze Reaction Statements.

      Analyze_Statements (Statements (Handled_Statement_Sequence (N)));
   end Analyze_Reactive_Flow_Proc;

   ---------------------------------
   -- Analyze_Reactive_Statements --
   ---------------------------------

   procedure Analyze_Reactive_Statements (L : List_Id) is
   begin
      Analyze_Statements (L);
   end Analyze_Reactive_Statements;

   ------------------------------
   -- Install_Reactive_Formals --
   ------------------------------

   procedure Install_Reactive_Formals (Spec : Entity_Id) is
      E    : Entity_Id;
      Prev : Entity_Id;
   begin
      E := First_Entity (Spec);
      while Present (E) loop
	 Put_Line (" Install Fomals E => " & Get_String (Chars (E)));
         Prev := Current_Entity (E);
         Set_Current_Entity (E);
         Set_Is_Immediately_Visible (E);
         Set_Homonym (E, Prev);
         Next_Entity (E);
      end loop;
   end Install_Reactive_Formals;

   ---------------------------
   -- Dump_Reactive_Formals --
   ---------------------------

   procedure Dump_Reactive_Formals (Spec : Entity_Id) is
      E    : Entity_Id;
   begin
      Put_Line ("Dump for " & Get_String (Chars (Spec)));
      E := First_Entity (Spec);
      while Present (E) loop
	 Put_Line (" Install Fomals E => " & Get_String (Chars (E)));
         Next_Entity (E);
      end loop;
   end Dump_Reactive_Formals;


   ------------------------------------
   -- Analyze_Subprogram_Body_Helper --
   ------------------------------------

   --  This procedure is called for regular subprogram bodies, generic bodies,
   --  and for subprogram stubs of both kinds. In the case of stubs, only the
   --  specification matters, and is used to create a proper declaration for
   --  the subprogram, or to perform conformance checks.

   procedure Analyze_Subprogram_Body_Helper (N : Node_Id) is
      Loc          : constant Source_Ptr := Sloc (N);
      Body_Spec    : constant Node_Id    := Specification (N);
      Body_Id      : Entity_Id           := Defining_Entity (Body_Spec);
      Prev_Id      : constant Entity_Id  := Current_Entity_In_Scope (Body_Id);
      HSS          : Node_Id;


   --  Start of processing for Analyze_Subprogram_Body_Helper

   begin
      if Present (Prev_Id) then
	 --  Previous entity conflicts with subprogram name. Attempting to
	 --  enter name will post error.

	 Error_Msg_Sloc := Sloc (Prev_Id);
	 Error_Msg_N ("& conflicts with declaration#", Body_Id);
      end if;

      Body_Id := Analyze_Subprogram_Specification (Body_Spec);

      Set_Ekind (Body_Id, E_Subprogram_Body);
      Set_Scope (Body_Id, Defining_Entity (Parent (N)));
      Set_Acts_As_Spec (N);
      Generate_Definition (Body_Id);

      Generate_Reference
	(Body_Id, Body_Id, 'b', Set_Ref => False, Force => True);

      --  There is no formals for a reactive procedure.
      Set_Has_Completion (Body_Id);

      --  Now we can go on to analyze the body

      HSS := Handled_Statement_Sequence (N);


      --  Analyze the declarations in the scope of the ractive, as
      --  the variable are persistent.

      Analyze_Declarations (Declarations (N));

      --  Check completion, and analyze the statements

      Check_Completion;
      Inspect_Deferred_Constant_Completion (Declarations (N));

      Push_Scope (Body_Id);
      Analyze (HSS);

      --  Deal with end of scope processing for the body

      Process_End_Label (HSS, 't', Current_Scope);
      End_Scope;

      --  Check_Subprogram_Order (N);

      Set_Analyzed (Body_Id);
   end Analyze_Subprogram_Body_Helper;

end Sem_Ch14;
