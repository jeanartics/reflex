------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
--                                                                          --
-- Reflex is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public Licensea as published  by the Free Soft- --
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

with Sinfo; use Sinfo;
with Nlists; use Nlists;
with Atree; use Atree;
with Sem_Util; use Sem_Util;
with Nmake; use Nmake;
with Tbuild; use Tbuild;
with Stand; use Stand;

with Reflex.Infos; use Reflex.Infos;

package body Reflex.Vertices.Interp is
   
   Reactive_Type : Entity_Id;
   --  Current Reactive type
   
   Reactive_Type_Decl : Node_Id;
   --  The full declaration of current reactive type
   
   Reactive_Formal : Entity_Id;
   --  The formal of the reactive procedure for the reactive type
   
   Reactive_Procedure_Decl : Node_Id;
   --  Declaration of reactive procedure 
   
   Reactive_Procedure_Body : Node_Id;
   --  Body of reactive procedure 
   
   procedure Create_Waiting_Raz_Code (V : Rnode_Id);
   procedure Create_Transient_Raz_Code (V : Rnode_Id);
   procedure Create_Update_Code (V : Rnode_Id);
   
   procedure Interp_Graph_Vertex  (G : Rnode_Id);
   procedure Interp_Pause_Vertex  (V : Rnode_Id);
   procedure Interp_Sync_Vertex   (V : Rnode_Id);
   procedure Interp_Wait_Vertex   (V : Rnode_Id);

   procedure Interp_Select_Vertex (V : Rnode_Id);
   procedure Interp_Fork_Vertex   (V : Rnode_Id);
   procedure Interp_If_Vertex     (V : Rnode_Id);
   procedure Interp_Loop_Vertex   (V : Rnode_Id);
   procedure Interp_Exit_Vertex   (V : Rnode_Id);
   procedure Interp_Abort_Vertex  (V : Rnode_Id);
   
   procedure Add_Transitionnal_Code
     (Stmts : List_Id;
      V     : Rnode_Id;
      Nxt   : Rnode_Id);
   
   ---------------------------
   -- Add_Transitonnal_Code --
   ---------------------------

   procedure Add_Transitionnal_Code
     (Stmts : List_Id;
      V     : Vertex_Id;
      Nxt   : Vertex_Id) is
   begin
      -- Get Eit code form V

      Append_List_To (Stmts, Exit_Code (V));

      -- Get Enter code from Nxt.

      Append_List_To (Stmts, Enter_Code (Nxt));
   end Add_Transitionnal_Code;

   --------------------------------------------
   -- Make_If_Trans_Activation_Desactivation --
   --------------------------------------------

   function Make_If_Trans_Activation_Desactivation
     (V    : Vertex_Id;
      Nxt  : Vertex_Id;
      Cond : Node_Id) return Node_Id is

      If_Node    : Node_Id;
      Expr       : Node_Id;
      N          : Node_Id := Corresponding_Node (V);
      Loc        : Source_Ptr := Sloc (N);
      Then_Stmts : List_Id;
   begin
      Then_Stmts := New_List;
      Append_To (Then_Stmts, Make_Desactivation (V));
      Append_To (Then_Stmts, Make_Activation (Nxt));

      Add_Transitionnal_Code (Then_Stmts, V, Nxt);

      if Present (Cond) then
	 Expr := Make_Op_And
	   (Loc,
	    Left_Opnd  => New_Occurrence_Of (Xc (V), Loc),
	    Right_Opnd => New_Copy_Tree (Cond));

	 If_Node := Make_If_Statement
	   (Loc,
	    Condition       => Expr,
	    Then_Statements => Then_Stmts);

      else
	 If_Node := Make_If_Statement
	   (Loc,
	    Condition       => New_Occurrence_Of (Xc (V), Loc),
	    Then_Statements => Then_Stmts);
      end if;

      return If_Node;
   end Make_If_Trans_Activation_Desactivation;

   ---------------------
   -- Make_Activation --
   ---------------------

   function Make_Activation (V : Vertex_Id) return Node_Id is

      N   : Node_Id := Corresponding_Node (V);
      Loc : Source_Ptr := Sloc (N);
      
      Xa_Sel : Node_Id;
      Xa     : Node_Id;
   begin
      --  The activation code for a waiting vertex is X_a := True;

      if Vertex_Kind (V) in Waiting_Vertex then
	 
	 Xa_Sel := Make_Selected_Component
	   (Sloc         => Loc,
	    Prefix       => New_Occurrence_Of (Reactive_Type, Loc);
	    Selector_Name => New_Occurrence_Of (Xa_Comp, Loc));
	 
	 Xa :=  Make_Indexed_Component
	   (Sloc        => Loc,
	    Prefix      => Xa_Sel,
	    Expressions => New_List (It));
	 
	 return
	   Make_Assignment_Statement
	   (Loc,
	    Name       => New_Occurrence_Of (Xa, Loc),
	    Expression => New_Occurrence_Of (Standard_True, Loc));

	 --  The activation code for a transient vertex is X_c := True;
      else
	 Xc_Sel := Make_Selected_Component
	   (Sloc         => Loc,
	    Prefix       => New_Occurrence_Of (This, Loc);
	    Selector_Name => New_Occurrence_Of (Xc_Comp, Loc));
	 
	 Xc :=  Make_Indexed_Component
	   (Sloc        => Loc,
	    Prefix      => Xa_Sel,
	    Expressions => New_List (It));
	 
	 return
	   Make_Assignment_Statement
	   (Loc,
	    Name       => New_Occurrence_Of (Xc, Loc),
	    Expression => New_Occurrence_Of (Standard_True, Loc));
      end if;
   end Make_Activation;

   ------------------------
   -- Make_Desactivation --
   ------------------------

   function Make_Desactivation (V : Vertex_Id) return Node_Id is

      N   : Node_Id := Corresponding_Node (V);
      Loc : Source_Ptr := Sloc (N);
   begin
      --  The desactivation code for a waiting vertex is X_d := True;
      if Vertex_Kind (V) in Waiting_Vertex then
	 return
	   Make_Assignment_Statement
	   (Loc,
	    Name       => New_Occurrence_Of (Xd (V), Loc),
	    Expression => New_Occurrence_Of (Standard_True, Loc));

	 --  The desactivation code for a transient vertex is X_c := False;
      else
	 return
	   Make_Assignment_Statement
	   (Loc,
	    Name       => New_Occurrence_Of (Xc (V), Loc),
	    Expression => New_Occurrence_Of (Standard_False, Loc));
      end if;
   end Make_Desactivation;

   ---------------------
   -- Create_Raz_Code --
   ---------------------

   procedure Create_Pre_Code (N : Node_Id) is

      N   : Node_Id := Get_Corresponding_Node (V);
      Loc : Source_Ptr := Sloc (N);
      This : Entity_Id;
      Xa_Comp : Node_Id;
   begin
      -- The interp RAZ code is
      -- for I in Emun_Type'Range loop
      --   This.Xa (I) := False;
      --   This.Xd (I) := False;
      --   This.Xp (I) := This.Xc (I);
      -- end loop;
      
      Stmts := New_List;
      
      
      Iterator := Make_Temporary (Loc, 'C');
      
      
      Xa_Sel := Make_Selected_Component
	(Sloc         => Loc,
	 Prefix       => New_Occurrence_Of (This, Loc);
	 Selector_Name => New_Occurrence_Of (Xa_Comp, Loc));
      
      Xa :=  Make_Indexed_Component
	(Sloc        => Loc,
	 Prefix      => Xa_Sel,
	 Expressions => New_List (It));
      
      --  This.Xa (I) := False;
      
      Append
	(Make_Assignment_Statement
	   (Loc,
	    
	    Name => 
	      Make_Indexed_Component
	      (Sloc        => Loc,
	       Make_Selected_Component
		 (Sloc         => Loc,
		  Prefix       => New_Occurrence_Of (This, Loc),
		  Selector_Name => New_Occurrence_Of (Xa_Comp, Loc))),
	    
	    Expression => New_Occurrence_Of (Standard_False, Loc)),
	Stmts);
      
      --  This.Xd (I) := False;
      
      Append
	(Make_Assignment_Statement
	   (Loc,
	    Name       => 
	      Make_Indexed_Component
	      (Sloc        => Loc,
	       Make_Selected_Component
		 (Sloc         => Loc,
		  Prefix       => New_Occurrence_Of (This, Loc),
		  Selector_Name => New_Occurrence_Of (Xd_Comp, Loc))),
	    
	    Expression => New_Occurrence_Of (Standard_False, Loc)),
	 Stmts);
      
      --  This.Xp (I) := This.Xc (I);
      
      Append
	(Make_Assignment_Statement
	   (Loc,
	    Name       => 
	      Make_Indexed_Component
	      (Sloc        => Loc,
	       Make_Selected_Component
		 (Sloc         => Loc,
	       Prefix       => New_Occurrence_Of (This, Loc),
	       Selector_Name => New_Occurrence_Of (Xp_Comp, Loc))),
	    
	 Expression => 
	   Make_Indexed_Component
	   (Sloc        => Loc,
	    Make_Selected_Component
	      (Sloc         => Loc,
	       Prefix       => New_Occurrence_Of (This, Loc),
	       Selector_Name => New_Occurrence_Of (Xp_Comp, Loc)))),
	 Stmts);
	   
      Loop_Node :=
        Make_Loop_Statement
	(Loc,
	 
	 Iteration_Scheme =>
	   Make_Iteration_Scheme
	   (Loc,
	    Loop_Parameter_Specification =>
	      Make_Loop_Parameter_Specification
	      (Loc,
	       Defining_Identifier         => New_Occurrence_Of (It, Loc),
	       Reverse_Present             => False,
	       Discrete_Subtype_Definition =>
		 Make_Attribute_Reference
		    (Loc,
		     Prefix         => New_Occurrence_Of (React_Type, Loc),
		     Attribute_Name => Name_Range))),
		  
	 Statements => Stmts,
	 End_Label  => Empty);
      
   end Create_Pre_Code;

   
   -----------------------------
   -- Create_Waiting_Raz_Code --
   -----------------------------

   procedure Create_Waiting_Raz_Code (V : Vertex_Id) is

      N   : Node_Id := Get_Corresponding_Node (V);
      Loc : Source_Ptr := Sloc (N);
   begin
      if not Present (Update_Raz_Code (V)) then
	 Set_Update_Raz_Code (V, New_List);
      end if;

      -- Xa := False; Xd := False;

      Append_To
	(Update_Raz_Code (V),
	  Make_Assignment_Statement
	    (Loc,
	     Name       => New_Occurrence_Of (Xa (V), Loc),
	     Expression => New_Occurrence_Of (Standard_False, Loc)));

      Append_To
	(Update_Raz_Code (V),
	  Make_Assignment_Statement
	    (Loc,
	     Name => New_Occurrence_Of (Xc (V), Loc),
	     Expression => New_Occurrence_Of (Xd (V), Loc)));

   end Create_Waiting_Raz_Code;

   -------------------------------
   -- Create_Transient_Raz_Code --
   -------------------------------

   procedure Create_Transient_Raz_Code (V : Rnode_Id) is

      N   : Node_Id := Item_Node (V);
      Loc : Source_Ptr := Sloc (N);
   begin
      if not Present (Update_Raz_Code (V)) then
	 Set_Update_Raz_Code (V, New_List);
      end if;

      -- Xc := False

      Append_To
	(Update_Raz_Code (V),
	  Make_Assignment_Statement
	    (Loc,
	     Name => New_Occurrence_Of (Xc (V), Loc),
	     Expression => New_Occurrence_Of (Standard_False, Loc)));

   end Create_Transient_Raz_Code;

   ------------------------
   -- Create_Update_Code --
   ------------------------

   procedure Create_Update_Code (V : Rnode_Id) is

      N   : Node_Id := Item_Node (V);
      Loc : Source_Ptr := Sloc (N);
   begin
      Set_Update_Code (V, New_List);

      -- Update State
      
      
      Append_To
	(Update_Code (V),
	 Make_Assignment_Statement
	   (Loc,
	    Name       => New_Occurrence_Of (Xd (V), Loc),
	    Expression =>
	      Make_Op_Or
	      (Loc,
	       Left_Opnd  => New_Occurrence_Of (Xa (V), Loc),
	       Right_Opnd => New_Occurrence_Of (Xc (V), Loc))));

   end Create_Update_Code;

   -------------------------
   -- Interp_Graph_Vertex --
   -------------------------

   procedure Interp_Graph_Vertex (G : Vertex_Id) is

      V : Rnode_Id;
   begin
      Set_Trans_Code (G, New_List);

      if Is_Empty_List (Vertices_List (G)) then
	 return;
      end if;

      V := First (Vertices_List (G));
      while V /= No_Vertex loop

	 case Vertex_Kind (V) is

	    when R_Unused_At_Start =>
	       null;

	    when V_Graph_Vertex =>
	       null;

	    when R_Contionnal_Graph_Vertex =>
	       null;

	    when R_Reactive_Begin_Vertex =>
	       null;

	    when R_Reactive_End_Vertex =>
	       null;

	    when R_End_Vertex =>
	       null;
	    when R_Begin_Vertex =>
	       null;
	    when R_Repeat_Loop_Vertex =>
	       null;

	    when R_Handler_Vertex =>
	       null;

	    when R_If_Vertex =>
	       Interp_If_Vertex (V);

	    when R_Loop_Vertex =>
	       Interp_Loop_Vertex (V);

	    when R_Exit_Vertex =>
	       Interp_Exit_Vertex (V);

	    when R_Fork_Vertex =>
	       Interp_Fork_Vertex (V);

	    when R_Abort_Vertex =>
               Interp_Abort_Vertex (V);

	    when R_Weak_Abort_Vertex =>
	       null;

	    when R_Pause_Vertex =>
	       Interp_Pause_Vertex (V);

	    when R_Wait_Vertex =>
	       Interp_Wait_Vertex (V);

	    when R_Sync_Vertex =>
	       Interp_Sync_Vertex (V);

	    when R_Select_Vertex =>
	       Interp_Select_Vertex (V);

	    when R_Unused_At_End =>
	       null;
	 end case;

         Next (V);
      end loop;
      Put_Line ("Interp_Graph_Vertex End");
   end Interp_Graph_Vertex;

   ----------------------
   -- Attach_End_Graph --
   ----------------------

   procedure Attach_End_Graph
     (G   : Rnode_Id;
      Nxt : Rnode_id) is

      V   : Rnode_Id;
      Ifn : Node_Id;
   begin
      if Is_Empty_List (Vertices_List (G)) then
	 return;
      end if;

      V := Last (Vertices_List (G));
      case Rkind (V) is
	 when R_Unused_At_Start         =>
	    null;

	 when R_Graph_Vertex            =>
	    null;

	 when R_Contionnal_Graph_Vertex =>
	    null;

	 when R_Reactive_Begin_Vertex   =>
	    null;

	 when R_Reactive_End_Vertex     =>
	    null;

	 when R_End_Vertex              =>
	    null;

	 when R_Begin_Vertex            =>
	    null;

	 when R_Repeat_Loop_Vertex      =>
	    null;

	 when R_Handler_Vertex          =>
	    Put_Line ("Attach R_Handler_Vertex Begin");
	    Append_To
	      (Trans_Code (V),
	       Make_If_Trans_Activation_Desactivation
		 (V    => End_Vertex (V),
		  Nxt  => Nxt,
		  Cond => Empty));
--	       Make_If_Activation_Desactivation
--		 (End_Vertex (V), Nxt, Empty));
	    Put_Line ("Attach R_Handler_Vertex End");

	 when R_If_Vertex               =>
	    Put_Line ("Attach R_If_Vertex Begin");
	    --Ifn := Make_If_Activation_Desactivation
	    Ifn := Make_If_Trans_Activation_Desactivation
	      (V    => End_Vertex (V),
	       Nxt  => Nxt,
	       Cond => Empty);
	    -- (End_Vertex (V), Nxt, Empty);

	    Append_To (Trans_Code (End_Vertex (V)), Ifn);
	       --  Make_If_Activation_Desactivation
	       --  	 (End_Vertex (V), Nxt, Empty));
	    if Is_Non_Empty_List (Exit_Code (V)) then
	       Append_List_To (Then_Statements (Ifn), Exit_Code (V));
	    end if;
	    Put_Line ("Attach R_If_Vertex End");


	 when R_Loop_Vertex             =>
	    Put_Line ("Attach R_Loop_Vertex Begin");
	    Append_To
	      (Trans_Code (End_Vertex (V)),
	       Make_If_Activation_Desactivation
		 (End_Vertex (V), Nxt, Empty));
	    Put_Line ("Attach R_Loop_Vertex End");

	 when R_Exit_Vertex             =>
	    null;

	 when R_Fork_Vertex             =>
	    Put_Line ("Attach R_Fork_Vertex Begin");
	    if Is_Empty_List (Activ_Statements (V)) then
	       Ifn := Make_If_Trans_Activation_Desactivation
		 (V    => End_Vertex (V),
		  Nxt  => Nxt,
		  Cond => Empty);
	       --  Ifn := Make_If_Activation_Desactivation
	       --  	    (End_Vertex (V), Nxt, Empty);

	       -- Exit Code of fork statement

	       if Is_Non_Empty_List (Exit_Code (V)) then
		  Append_List_To (Then_Statements (Ifn), Exit_Code (V));
	       end if;

	       Append_To
		 (Trans_Code (End_Vertex (V)), Ifn);
	    else
	       Append_To
		 (Activ_Statements (V), Make_Activation (Nxt));

	       -- Exit Code of fork statement

	       if Is_Non_Empty_List (Exit_Code (V)) then
		  Append_List_To (Activ_Statements (V), Exit_Code (V));
	       end if;
	    end if;
	    Put_Line ("Attach R_Fork_Vertex End");

	 when R_Abort_Vertex            =>
	       Append_To
		 (Trans_Code (End_Vertex (V)),
		  Make_If_Trans_Activation_Desactivation
		    (V    => End_Vertex (V),
		     Nxt  => Nxt,
		     Cond => Empty));
		  --  Make_If_Activation_Desactivation
		  --    (End_Vertex (V), Nxt, Empty));
	    Put_Line ("Attach R_Abort_Vertex End");

	 when R_Weak_Abort_Vertex       =>
	    Put_Line ("Attach R_Weak_Abort_Vertex Begin");
	    Append_To
	      (Trans_Code (End_Vertex (V)),
	       Make_If_Trans_Activation_Desactivation
		 (V    => End_Vertex (V),
		  Nxt  => Nxt,
		  Cond => Empty));
	       --  Make_If_Activation_Desactivation
	       --  	 (End_Vertex (V), Nxt, Empty));
	    Put_Line ("Attach R_Weak_Abort_Vertex End");

	 when R_Pause_Vertex            =>
	    null;

	 when R_Wait_Vertex             =>
	    Put_Line ("Attach R_Wait_Vertex Begin");
	    Ifn := Make_If_Trans_Activation_Desactivation
	      (V    => V,
	       Nxt  => Nxt,
	       Cond => Condition (V));

	    Append_To
	      (Trans_Code (V), Ifn);

	    -- Enter Code of wait

	    if Is_Non_Empty_List (Enter_Code (V)) then
	       Prepend_List_To (Then_Statements (Ifn), Enter_Code (V));
	    end if;

	    -- Exit Code of wait

	    if Is_Non_Empty_List (Exit_Code (V)) then
	       Append_List_To (Then_Statements (Ifn), Exit_Code (V));
	    end if;

	    -- Exit Code of Graph

	    if Is_Non_Empty_List (Exit_Code (G)) then
	       Append_List_To (Then_Statements (Ifn), Exit_Code (G));
	    end if;

	    Put_Line ("Attach R_Wait_Vertex End");

	 when R_Sync_Vertex             =>
	    Put_Line ("Attach R_Sync_Vertex Begin");
	    Append_To
	      (Trans_Code (V),
	       Make_If_Trans_Activation_Desactivation
		 (V    => V,
		  Nxt  => Nxt,
		  Cond => Condition (V)));
	       --  Make_If_Activation_Desactivation
	       --  	 (V, Nxt, Condition (V)));
	    Put_Line ("Attach R_Sync_Vertex End");

	 when R_Select_Vertex           =>
	    Put_Line ("Attach R_Select_Vertex Begin");
	    Ifn := Make_If_Trans_Activation_Desactivation
		 (V    => End_Vertex (V),
		  Nxt  => Nxt,
		  Cond => Empty);

	    if Is_Non_Empty_List (Exit_Code (V)) then
	       Append_List_To (Then_Statements (Ifn), Exit_Code (V));
	    end if;

	    Append_To
	      (Trans_Code (End_Vertex (V)), Ifn);
	       --  Make_If_Trans_Activation_Desactivation
	       --  	 (V    => End_Vertex (V),
	       --  	  Nxt  => Nxt,
	       --  	  Cond => Empty));
	       --  Make_If_Activation_Desactivation
	       --  	 (End_Vertex (V), Nxt, Empty));
	    Put_Line ("Attach R_Select_Vertex End");

	 when R_Unused_At_End           =>
	    null;
      end case;
   end Attach_End_Graph;

   -----------------
   -- Interp_Step --
   -----------------

   procedure Interp_Step (N : Node_Id) is
   begin
      --  1a. Translate the type and subtype definitions.


      --  1b. Translate the local declarations of step to
      --      to the englobing procedure declarations.

      --  1c. Translate procedures

      --  2. Change all reference to names of the just tranfered objet
      --     with the new name

      --  3. For all object which have an initial value, create the code
      --     to initialize it.
      --     initialize code is : if Xre then init_code end if;
      --     finalize code is   : if Xfe then finalize_code end if;

      --  4. Create the activity step : if Xc then step_code end if;

      --  5. Place init code + step_code in the then statments of the
      --     if Xc then branch
      null;
   end Interp_Step;

   -------------------------
   -- Interp_Pause_Vertex --
   -------------------------

   procedure Interp_Pause_Vertex (V : Rnode_Id) is
   begin
      -- RAZ Code.

      Create_Waiting_Raz_Code (V);

      -- Update Code.

      Create_Update_Code (V);
   end Interp_Pause_Vertex;

   ------------------------
   -- Interp_Sync_Vertex --
   ------------------------

   procedure Interp_Sync_Vertex (V : Rnode_Id) is
   begin
      -- RAZ Code.

      Create_Waiting_Raz_Code (V);

      -- Update Code.

      Create_Update_Code (V);
   end Interp_Sync_Vertex;

   ------------------------
   -- Interp_Wait_Vertex --
   ------------------------

   procedure Interp_Wait_Vertex (V : Rnode_Id) is
      Ifn : Node_Id;
   begin
      -- Update Code.

      Create_Update_Code (V);

      if Next (V) = No_Rnode then
	 Set_Trans_Code (V, New_List);
      else
	 --  Ifn := Make_If_Activation_Desactivation
	 --    (V, Next (V), Condition (V));
	 Ifn := Make_If_Trans_Activation_Desactivation
	   (V, Next (V), Condition (V));

	 Set_Trans_Code
	   (V, New_List (Ifn));
	    --  New_List (Make_If_Activation_Desactivation
	    --  		(V, Next (V), Condition (V))));
      end if;

      if Is_Non_Empty_List (Exit_Code (V)) then
	 Append_List_To (Then_Statements (Ifn), Exit_Code (V));
      end if;

      --  If a step is associated to the wait, create the code
      --  step activity.

   end Interp_Wait_Vertex;

   --------------------------
   -- Interp_Select_Vertex --
   --------------------------

   procedure Interp_Select_Vertex (V : Rnode_Id) is

      Alt      : Rnode_Id;
      Then_Stmts : List_Id;
      Vend     : Rnode_Id;
      Nxt      : Rnode_Id;
      Cond     : Node_Id;
      --  End_Expr : Node_Id;
      If_Node  : Node_Id;
      Ifn      : Node_Id;
      N        : Node_Id := Item_Node (V);
      Loc      : Source_Ptr := Sloc (N);
   begin
      Put_Line ("Interp_Select_Vertex Begin");

      Vend := End_Vertex (V);

      Set_Trans_Code (V,    New_List);
      Set_Trans_Code (Vend, New_List);

      -- Raz Code
      Create_Waiting_Raz_Code (V);

      -- Update Code.

      Create_Update_Code (V);

      -- Branch Select to alternatives.

--      Create_Transient_Raz_Code (Vend);

      -- Interpret alternatives.

      if Present (Alternatives (V)) then
	 Alt := First (Alternatives (V));
         while Alt /= No_Rnode loop
	    Interp_Graph_Vertex (Alt);
	    Attach_End_Graph (Alt, Vend);
	    Next (Alt);
	 end loop;
      end if;

      If_Node := Empty;

      if Present (Alternatives (V)) then
	 Alt := First (Alternatives (V));
	 while Alt /= No_Rnode loop
	    Cond := Condition (Alt);
	    pragma Assert (Present (Cond));

	    if Is_Non_Empty_List (Vertices_List (Alt)) then
	       Nxt := First (Vertices_List (Alt));
	       if Nxt = No_Rnode then
		  Nxt := Vend;
	       end if;
	    else
	       Nxt := Vend;
	    end if;

	    if If_Node = Empty then
	       Then_Stmts := New_List;

	       if Is_Non_Empty_List (Enter_Code (Alt)) then
		  Append_List_To (Then_Stmts, Enter_Code (Alt));
	       end if;

	       Append_To (Then_Stmts, Make_Desactivation (V));
	       Append_To (Then_Stmts, Make_Activation (Nxt));

	       If_Node := Make_If_Statement
		 (Loc,
		  Condition => New_Copy_Tree (Cond),
		  Then_Statements => Then_Stmts);

	    else
	       if Elsif_Parts (If_Node) = No_List then
		  Set_Elsif_Parts (If_Node, New_List);
	       end if;

	       Then_Stmts := New_List;

	       if Is_Non_Empty_List (Enter_Code (Alt)) then
		  Append_List_To (Then_Stmts, Enter_Code (Alt));
	       end if;

	       Append_To (Then_Stmts, Make_Desactivation (V));
	       Append_To (Then_Stmts, Make_Activation (Nxt));

	       Append_to
		 (Elsif_Parts (If_Node),
		  Make_Elsif_Part
		    (Loc,
		     Condition => New_Copy_Tree (Cond),
		     Then_Statements => Then_Stmts));
		       --  (Make_Desactivation (V),
		       --  	Make_Activation (Nxt))));
	    end if;
	    Next (Alt);
	 end loop;
      end if;

      Ifn := Make_If_Statement
	   (Loc,
	    Condition       => New_Occurrence_Of (Xc (V), Loc),
	    Then_Statements => New_List (If_Node));

      Append_To (Trans_Code (V), Ifn);
	 --  Make_If_Statement
	 --    (Loc,
	 --     Condition       => New_Occurrence_Of (Xc (V), Loc),
	 --     Then_Statements => New_List (If_Node)));

      if Is_Non_Empty_List (Enter_Code (V)) then
	 Prepend_List_To (Then_Statements (Ifn), Enter_Code (V));
      end if;

      -- Vend Vertex.

      if Next (V) /= No_Rnode then
	 Ifn := Make_If_Trans_Activation_Desactivation
	   (End_Vertex (V), Next (V), Empty);

	 if Is_Non_Empty_List (Exit_Code (V)) then
	    Prepend_List_To (Then_Statements (Ifn), Exit_Code (V));
	 end if;

	 Append_To (Trans_Code (Vend), Ifn);
	      --  (Make_If_Trans_Activation_Desactivation
	      --  	 (V, Next (V), Empty)));
	 --  New_List (Make_If_Activation_Desactivation
	 --  	(Vend, Next (V), Empty)));
      end if;

      Put_Line ("Interp_Select_Vertex End");
   end Interp_Select_Vertex;

   -------------------------
   -- Interp_Abort_Vertex --
   -------------------------

   procedure Interp_Abort_Vertex (V : Rnode_Id)
   is
      pragma Assert (Rkind (V) = R_Abort_Vertex);

      N         : Node_Id    := Item_Node (V);
      Loc       : Source_Ptr := Sloc (N);

      Vend      : Rnode_Id;
      Stmts     : List_Id;
      Nxt       : Rnode_Id;
      If_Stmt   : Node_Id;
      If_Xabort : Node_Id;

      procedure Attach_Handler_Enter_Exit_Code
        (Gr : in Rnode_Id;
         L  : in List_Id)
      is
      begin
         if Present (Gr) then
            if Is_Non_Empty_List (Enter_Code (Gr)) then
               Prepend_List_To (L, Enter_Code (Gr));
            end if;

            --  ????? maybe is not the right place for this
            if Is_Non_Empty_List (Exit_Code (Gr)) then
               Append_List_To (L, Exit_Code (Gr));
            end if;
         end if;
      end Attach_Handler_Enter_Exit_Code;

   begin
      Put_Line ("Interp_Abort_Vertex started");

      --  1. Abort end vertex
      Vend := End_Vertex (V);

      --  2. Create interpreted code using GrBody and GrHandler
      Set_Trans_Code (V, New_List);
      Create_Transient_Raz_Code (V);

      Set_Trans_Code (Vend, New_List);
      Create_Transient_Raz_Code (Vend);

      --  2.1 Interpret Handler part
      if Present (Handler_Graph (V)) then
         Interp_Graph_Vertex (Handler_Graph (V));
         Attach_End_Graph (Handler_Graph (V), Vend);
      end if;

      --  2.2 Interpret body part
      Interp_Graph_Vertex (Body_Graph (V));
      Attach_End_Graph (Body_Graph (V), Vend);

      --  2.3 Build statements list
      Stmts := New_List;

      --  2.3.1 Append abort statement enter_code if any
      if Is_Non_Empty_List (Enter_Code (V)) then
         Append_List (Stmts, Enter_Code (V));
      end if;

      --  2.3.2 Append statements with desactivate abort transient state
      Append_To (Stmts, Make_Desactivation (V));

      --  2.3.3 Abort handler part
      --  Compute next vertice :
      --  Nxt = Vend_abort or First step of handler part, if any !
      Nxt := Vend;
      if Present (Handler_Graph (V)) and then
        Present (Vertices_List (Handler_Graph (V)))
        and then First (Vertices_List (Handler_Graph (V))) /= No_Rnode
      then
         Nxt := First (Vertices_List (Handler_Graph (V)));
      end if;

      --  Create if statement for handler part
      --    ** if Handler part present then its code is
      --    if c3 then
      --       << handler enter code if any >>
      --       X4a := true;
      --       << handler exit code if any >>
      --    ** if Handler part not present then its code is
      --    if c3 then
      --       << handler enter code if any >>
      --       xend_abort := true;
      --       << handler exit code if any >>

      If_Stmt := Make_If_Statement
        (Sloc            => Loc,
         Condition       => New_Copy_Tree (Condition (V)),
         Then_Statements => New_List (Make_Activation (Nxt)));

      --  if abort handler part has enter or exit code
      Attach_Handler_Enter_Exit_Code
        (Gr => Handler_Graph (V),
         L  => Then_Statements (If_Stmt));
      Append_To (Stmts, If_Stmt);

      --  2.3.4 Abort body part
      --  Compute next vertice :
      --  Nxt = Vend_abort or First step of body part (always present)

      --  Abort body part
      --    ** else
      --    else
      --       << body enter code if any >>
      --       X1a := true;
      --       << body exit code if any >>
      --    OR
      --       xend_abort := true;
      --    end if;

      Nxt := Vend;
      if Present (Body_Graph (V))
        and then Present (Vertices_List (Body_Graph (V)))
        and then First (Vertices_List (Body_Graph (V))) /= No_Rnode
      then
         Nxt := First (Vertices_List (Body_Graph (V)));
      end if;
      Set_Else_Statements (If_Stmt, New_List (Make_Activation (Nxt)));
      Attach_Handler_Enter_Exit_Code
        (Gr => Body_Graph (V),
         L  => Else_Statements (If_Stmt));

      --  2.4 Build abort instruction if structure
      --  if Xabort then
      --     Stmts
      --     Xabort := False;
      --  end if;
      If_Xabort := Make_If_Statement
        (Sloc            => Loc,
         Condition       => New_Occurrence_Of (Xc (V), Loc),
         Then_Statements => Stmts);
      Append_To (Trans_Code (V), If_Xabort);

      --  2.5 End abort interpretation
      --  if Xend_abort then
      --     << Abort Exit_Code >>
      --     Xend_abort := False;
      --     Xnext := True;
      --  end if;
      Stmts := New_List;
      Append_To (Stmts, Make_Desactivation (Vend));

      --  2.5.1 Compute next vertex after end abort and do activation code
      Nxt := Next (V);
      if Present (Nxt) then
         Append_To (Stmts, Make_Activation (Nxt));
      end if;

      --  2.5.2 Append abort exit_code if any
      if Is_Non_Empty_List (Exit_Code (V)) then
         Append_List_To (Stmts, Exit_Code (V));
      end if;

      --  2.5.3 Add to trans_code the corrsponding IF statement
      Append_To (Trans_Code (Vend),
                 Make_If_Statement
                   (Sloc            => Loc,
                    Condition       => New_Occurrence_Of (Xc (Vend), Loc),
                    Then_Statements => Stmts));

      Put_Line ("Interp_Abort_Vertex ended");

   end Interp_Abort_Vertex;

   ------------------------
   -- Interp_Fork_Vertex --
   ------------------------

   procedure Interp_Fork_Vertex (V : Rnode_Id) is

      Alt         : Rnode_Id;
      Stmts       : List_Id;
      Activ_Stmts : List_Id;
      --  Then_Stmts  : List_Id;
      Vend        : Rnode_Id;
      Nxt         : Rnode_Id;
      Sync        : Rnode_Id;
      Expr        : Node_Id;
      Ifn         : Node_Id;
      N           : Node_Id := Item_Node (V);
      Loc         : Source_Ptr := Sloc (N);
   begin
      Put_Line ("Interp_Fork_Vertex Begin");
      Vend := End_Vertex (V);
      Set_Trans_Code (V, New_List);
      Set_Trans_Code (Vend, New_List);

      Create_Transient_Raz_Code (V);
      Create_Transient_Raz_Code (Vend);

      -- Interprete sub graphs.

      if Present (Alternatives (V)) then
	 Alt := First (Alternatives (V));
	 while Alt /= No_Rnode loop
	    Interp_Graph_Vertex (Alt);
	    Next (Alt);
	 end loop;
      end if;

      -- Validate all first states of all sub graphs of fork.

      if Present (Alternatives (V)) then
	 Stmts := New_List;

	 -- Enter Code of fork statement

	 if Is_Non_Empty_List (Enter_Code (V)) then
	    Append_List_To (Stmts, Enter_Code (V));
	 end if;

	 Append_To (Stmts, Make_Desactivation (V));

	 Alt := First (Alternatives (V));
	 while Alt /= No_Rnode loop
	    if Present (Vertices_List (Alt)) then
	       Nxt := First (Vertices_List (Alt));
	       Append_To (Stmts, Make_Activation (Nxt));
	    end if;

	    --  Enter Code of fork alternative, there is no exit as the
	    --  last statment of a fork statment is a pause statement

	    if Is_Non_Empty_List (Enter_Code (Alt)) then
	       Append_List_To (Stmts, Enter_Code (Alt));
	    end if;

	    Next (Alt);
	 end loop;

	 Append_To
	   (Trans_Code (V),
	    Make_If_Statement
	      (Loc,
	       Condition => New_Occurrence_Of (Xc (V), Loc),
	       Then_Statements => Stmts));
      end if;

      --  Interpret End Vertex of fork

      Activ_Stmts := New_List;
      Append_To	(Activ_Stmts, Make_Desactivation (Vend));

      if Present (Alternatives (V)) then
	 Expr := Empty;
	 Alt := First (Alternatives (V));
	 while Alt /= No_Rnode loop
	    Sync := Last (Vertices_List (Alt));

	    Put_Line ("Sync => " & Rkind (Sync)'Img);
	    if Expr = Empty then
	       Expr := New_Occurrence_Of (Xc (Sync), Loc);
	    else
	       Expr := Make_Op_And
		 (Loc,
		  Left_Opnd => Expr,
		  Right_Opnd => New_Occurrence_Of (Xc (Sync), Loc));
	    end if;

	    Append_To (Activ_Stmts, Make_Desactivation (Sync));

	    Next (Alt);
	 end loop;

	 Append_To
	   (Trans_Code (Vend),
	    Make_Assignment_Statement
	      (Loc,
	       Name => New_Occurrence_Of (Xc (Vend), Loc),
	       Expression => Expr));
      end if;

      Nxt := Next (V);
      if Nxt /= No_Rnode then
	 Append_To (Activ_Stmts, Make_Activation (Nxt));
      end if;

      -- If Xend_c then
      --   if Tend_fork then
      --     Xend_c := False;
      --     Activate next (V)

      Set_Activ_Statements (V, Activ_Stmts);

      if Condition (V) = Empty then
	 --  Then_Stmts := Activ_Stmts;
	 Ifn := Make_If_Statement
	   (Loc,
	    Condition => New_Occurrence_Of (Xc (Vend), Loc),
	    Then_Statements => Activ_Stmts);
      else
	 Expr := Make_Op_And
	   (Loc,
	    Left_Opnd  => New_Occurrence_Of (Xc (V), Loc),
	    Right_Opnd => New_Copy_Tree (Condition (V)));

	 Ifn := Make_If_Statement
	   (Loc,
	    Condition => Expr,
	    Then_Statements => Activ_Stmts);

	 --  Then_Stmts := New_List (Ifn);
      end if;

      Append_To
	(Trans_Code (Vend), Ifn);
	 --  Make_If_Statement
	 --    (Loc,
	 --     Condition => New_Occurrence_Of (Xc (Vend), Loc),
	 --     Then_Statements => Then_Stmts));

      Put_Line ("Interp_Fork_Vertex End");
   end Interp_Fork_Vertex;

   ------------------------
   -- Interp_If_Vertex --
   ------------------------

   procedure Interp_If_Vertex (V : Rnode_Id) is
      Alt        : Rnode_Id;
      Stmts      : List_Id;
      If_Node    : Node_Id;
      Elsif_Node : Node_Id;
      Vend       : Rnode_Id;
      Nxt        : Rnode_Id;
      N          : Node_Id := Item_Node (V);
      Loc        : Source_Ptr := Sloc (N);
   begin
      Vend := End_Vertex (V);

      Set_Trans_Code (V, New_List);
      Set_Trans_Code (Vend, New_List);

      Create_Transient_Raz_Code (V);
      Create_Transient_Raz_Code (Vend);

      -- Then Graph

      Interp_Graph_Vertex (Then_Graph (V));
      Attach_End_Graph (Then_Graph (V), Vend);

      -- Build graf of all alternatives.

     if Present (Alternatives (V)) then
	 Alt := First (Alternatives (V));
	 while Alt /= No_Rnode loop
	    Interp_Graph_Vertex (Alt);
	    Attach_End_Graph (Alt, Vend);
	    Next (Alt);
	 end loop;
     end if;

     -- Else Graph

      if Else_Graph (V) /= No_Rnode then
	 Interp_Graph_Vertex (Else_Graph (V));
	 Attach_End_Graph (Else_Graph (V), Vend);
      end if;

      -- If Xif_c Then
      --      Enter_Code_If
      --   Xif_c := False;
      --   if C1 then
      --      Enter_Code_Then
      --      Xthen_c := True;
      --      Exit_Code_Then
      --   elsif C2 then
      --      Enter_Code_Elsif for C2
      --      Xelsif1_c := True;
      --      Exit_Code_Elsif for C2
      --   ..
      --   else
      --      Enter_Code_Else
      --      Xelse_c := True;
      --      Exit_Code_Else
      --   end if;

      Stmts := New_List;

      --  Append Enter_Code for If

      if Is_Non_Empty_List (Enter_Code (V)) then
	 Append_List_To (Stmts, Enter_Code (V));
      end if;

      --  Desactivate the If transient state

      Append_To (Stmts, Make_Desactivation (V));

      -- Then part.

      Nxt := Vend;
      if Present (Vertices_List (Then_Graph (V)))
      and then First (Vertices_List (Then_Graph (V))) /= No_Rnode then
	 Nxt := First (Vertices_List (Then_Graph (V)));
      end if;

      If_Node := Make_If_Statement
	(Loc,
	 Condition => New_Copy_Tree (Condition (V)),
	 Then_Statements => New_List
	   (Make_Activation (Nxt)));

      -- Enter Code of Then part

      if Is_Non_Empty_List (Enter_Code (Then_Graph (V))) then
	 Prepend_List_To (Then_Statements (If_Node), Enter_Code (Then_Graph (V)));
      end if;

      -- Exit Code of Then part

      if Is_Non_Empty_List (Exit_Code (Then_Graph (V))) then
	 Append_List_To (Then_Statements (If_Node), Exit_Code (Then_Graph (V)));
      end if;

      Append_To (Stmts, If_Node);

      -- Elsif parts.

      if Present (Alternatives (V)) then
	 Alt := First (Alternatives (V));
	 while Alt /= No_Rnode loop
	    Nxt := Vend;
	    if Present (Vertices_List (Alt))
	      and then First (Vertices_List (Alt)) /= No_Rnode then
	       Nxt := First (Vertices_List (Alt));
	    end if;

	    if Elsif_Parts (If_Node) = No_List then
	       Set_Elsif_Parts (If_Node, New_List);
	    end if;

	    Elsif_Node := Make_Elsif_Part
	      (Loc,
	       Condition => New_Copy_Tree (Condition (Alt)),
	       Then_Statements => New_List
		 (Make_Activation (Nxt)));

	    Append_to (Elsif_Parts (If_Node), Elsif_Node);

	    -- Enter Code of Elsif part

	    if Is_Non_Empty_List (Enter_Code (Alt)) then
	       Prepend_List_To (Then_Statements (Elsif_Node), Enter_Code (Alt));
	    end if;

	    -- Exit Code of Elsif part

	    if Is_Non_Empty_List (Exit_Code (Alt)) then
	       Append_List_To (Then_Statements (Elsif_Node), Exit_Code (Alt));
	    end if;

	    Next (Alt);
	 end loop;
     end if;

     -- Else Part.

     Nxt := Vend;
     if Else_Graph (V) /= No_Rnode
       and then First (Vertices_List (Else_Graph (V))) /= No_Rnode then
	Nxt := First (Vertices_List (Else_Graph (V)));
     end if;

     Set_Else_Statements (If_Node, New_List (Make_Activation (Nxt)));

     -- Enter Code of Elsif part

     if Is_Non_Empty_List (Enter_Code (Else_Graph (V))) then
	Prepend_List_To (Else_Statements (If_Node), Enter_Code (Else_Graph (V)));
     end if;

     -- Exit Code of Elsif part

     if Is_Non_Empty_List (Exit_Code (Else_Graph (V))) then
	Append_List_To (Else_Statements (If_Node), Exit_Code (Else_Graph (V)));
     end if;

     -- If Xif_c then
     --    Stmts..

     Append_To
       (Trans_Code (V),
	Make_If_Statement
	  (Loc,
	   Condition => New_Occurrence_Of (Xc (V), Loc),
	   Then_Statements => Stmts));

     -- End if interpretation
     --  If Xendif_c then
     --     Exit_Code_If
     --     Xendif_c := False;
     --     Xnext_c := True;
     --  end if;


     --  Nxt := Next (V);
     --  Then_Stmts := New_List;
     --  Append_To (Then_Stmts, Make_Desactivation (Vend));

     --  if Nxt /= No_Rnode then
     --  	Append_To (Then_Stmts, Make_Activation (Nxt));
     --  end if;

     --  -- Exit Code of if part

     --  if Is_Non_Empty_List (Exit_Code (V)) then
     --  	Append_List_To (Then_Stmts, Exit_Code (V));
     --  end if;

     --  Append_To
     --    (Trans_Code (Vend),
     --  	Make_If_Statement
     --  	  (Loc,
     --  	   Condition => New_Occurrence_Of (Xc (Vend), Loc),
     --  	   Then_Statements => Then_Stmts));

   end Interp_If_Vertex;

   ------------------------
   -- Interp_Loop_Vertex --
   ------------------------

   procedure Interp_Loop_Vertex (V : Rnode_Id) is

      If_Rep  : Node_Id;
      Vend    : Rnode_Id;
      Vrepeat : Rnode_Id;
      Vexit   : Rnode_Id;
      Nxt     : Rnode_Id;
      N       : Node_Id := Item_Node (V);
      Loc     : Source_Ptr := Sloc (N);
      Ifn : Node_Id;
      Loop_Node : Node_Id;
   begin
      Put_Line ("Interp_Loop_Vertex Begin");
      Put_Line ("      V => " & V'Img);

      Vend    := End_Vertex (V);
      Vrepeat := Repeat_Vertex (V);
      Vexit   := Exit_Vertex (V);

      Set_Trans_Code (V,       New_List);
      Set_Trans_Code (Vend,    New_List);
      Set_Trans_Code (Vrepeat, New_List);
      Set_Trans_Code (Vexit,   New_List);

      Create_Transient_Raz_Code (V);
      Create_Transient_Raz_Code (Vend);
      Create_Transient_Raz_Code (Vrepeat);

      -- Make loop interp
      --  loop
      --    Exit_Loop := True;
      --    If Xloop_c then
      --       Xloop_c := False
      --       Xfirst_Body_c := True
      --    end if;
      --    ....

      Nxt := Vend;
      if Is_Non_Empty_List (Vertices_List (Body_Graph (V))) then
	 Nxt := First (Vertices_List (Body_Graph (V)));
      end if;

      Ifn := Make_If_Trans_Activation_Desactivation (V, Nxt, Empty);

      Loop_Node := Make_Loop_Statement
	(Loc,
	 Statements =>
	   New_List
	   (Make_Assignment_Statement
	      (Loc,
	       Name       => New_Occurrence_Of (Exit_Loop (V), Loc),
	       Expression => New_Occurrence_Of (Standard_True, Loc)),
	    Ifn),
	 End_Label => Empty);

      Append_To (Trans_Code (V), Loop_Node);

      -- Body Graph

      Interp_Graph_Vertex (Body_Graph (V));


      -- Repeat code.
      --  1. Attach end body to repeat.
      Attach_End_Graph (Body_Graph (V), Vrepeat);

      --  Exit Code of body graph is attach to the last vertex is attached to
      --  the desactivation of last vertex of body graph in the procedure
      --  Attach_End_Graph

      --  Here we have just to attach the enter code to the deasctivation
      --  of the begin loop vertex

     if Is_Non_Empty_List (Enter_Code (Body_Graph (V))) then
	Prepend_List_To
	  (Then_Statements (Ifn),
	   Enter_Code (Body_Graph (V)));
     end if;



      --  2. If Xrepeat_c then
      --        Xrepeat_c := False;
      --        Xloop_c := True;
      --        Exit_Loop := False;

      --  If_Rep := Make_If_Activation_Desactivation (Vrepeat, V, Empty);
      If_Rep := Make_If_Trans_Activation_Desactivation (Vrepeat, V, Empty);
      Append_To
	(Then_Statements (If_Rep),
	 Make_Assignment_Statement
	   (Loc,
	    Name       => New_Occurrence_Of (Exit_Loop (V), Loc),
	    Expression => New_Occurrence_Of (Standard_False, Loc)));

      Append_To (Trans_Code (Vrepeat), If_Rep);

      -- Exit of loop when Exit_Loop = True.

      Append_To
	(Trans_Code (Vexit),
	 Make_Exit_Statement
	   (Loc,
	    Condition => New_Occurrence_Of (Exit_Loop (V), Loc)));

      Put_Line ("Interp_Loop_Vertex End");
   end Interp_Loop_Vertex;

   ------------------------
   -- Interp_Exit_Vertex --
   ------------------------

   procedure Interp_Exit_Vertex (V : Rnode_Id) is

      Vend       : Rnode_Id;
      Vloop      : Rnode_Id;
      Then_Stmts : List_Id;
      N          : Node_Id := Item_Node (V);
      Loc        : Source_Ptr := Sloc (N);
   begin
      Put_Line ("Interp_Exit_Vertex Begin");

      Set_Trans_Code (V, New_List);
      Create_Transient_Raz_Code (V);

      Vloop := Parent_Vertex (V);

      -- Add after the loop statement the validation of the state
      -- following loop.
      --   if Vend_loop_c then
      --      Vend_Loop_c := False;
      --      Vloop_next_A := True;
      --   end if;

      Vend := End_Vertex (Vloop);

      if Next (Vloop) /= No_Rnode then
	 Append_To
	   (Trans_Code (Vend),
	    Make_If_Trans_Activation_Desactivation (Vend, Next (Vloop), Empty));
	    -- Make_If_Activation_Desactivation (Vend, Next (Vloop), Empty));
      end if;

      -- Now exit code is :
      --    if Xexit_c then
      --       Xexit_c := False;
      --       if Cond then
      --           Vend_loop_c := True;
      --        end if;
      --    end if;

      Then_Stmts := New_List;
      Append_To (Then_Stmts, Make_Desactivation (V));

      if Present (Condition (V)) then
	 Append_To
	   (Then_Stmts,
	    Make_If_Statement
	      (Loc,
	       Condition       => New_Copy_Tree (Condition (V)),
	       Then_Statements => New_List (Make_Activation (Vend))));
      else
	 Append_To (Then_Stmts, Make_Activation (Vend));
      end if;

      Append_To
	(Trans_Code (V),
	 Make_If_Statement
	   (Loc,
	    Condition       => New_Occurrence_of (Xc (V), loc),
	    Then_Statements => Then_Stmts));

      Put_Line ("Interp_Exit_Vertex End");
   end Interp_Exit_Vertex;

end Reflex.Vertices.Interp;
