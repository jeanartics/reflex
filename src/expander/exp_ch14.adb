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

with Atree;    use Atree;
with Nmake;    use Nmake;
with Tbuild;   use Tbuild;
with Debug;    use Debug;
with Nlists;   use Nlists;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Einfo;    use Einfo;
with Sem;      use Sem;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Aux;  use Sem_Aux;
with Sem_Util; use Sem_Util;

with Rtree;    use Rtree;
with Rinfo;    use Rinfo;
with Nconv;    use Nconv;
with Rlists;   use Rlists;
with Nassoc;
with Opt;      use Opt;

with Graphs_Svg;

with Ada.Text_Io; use Ada.Text_Io;
with Transformer.Scopes;
with Sinfo.Transform;
package body Exp_Ch14 is


   package Accessors_Assoc is new Nassoc;
   use Accessors_Assoc;

   package Vars_Assoc is new Nassoc;
   use Vars_Assoc;

   type Parameter_Mode is (Mode_In, Mode_Out);

   Generate_Edge_Graph : Boolean := False;

   function Parent_Loop (N : Node_Id) return Node_Id;

   function Build_Pause_State_Names
     (Reactive_Scope : Entity_Id;
      Reaction_Scope : Entity_Id;
      Globals        : List_Id;
      Locals         : List_Id) return Name_Id;

   function Build_Wait_State_Names
     (Reactive_Scope : Entity_Id;
      Reaction_Scope : Entity_Id;
      Globals        : List_Id;
      Locals         : List_Id) return Name_Id;

   procedure Build_Fork_State_Names
     (Reaction_Scope : Entity_Id;
      Locals         : List_Id;
      Name_Fork      : out Name_Id;
      Name_End_Fork  : out Name_id);

   procedure Build_Select_State_Names
     (Prefix          : String;
      Reactive_Scope  : Entity_Id;
      Reaction_Scope  : Entity_Id;
      Globals         : List_Id;
      Locals          : List_Id;
      Name_Select     : out Name_Id;
      Name_End_Select : out Name_Id);

   procedure Build_If_State_Names
     (Reaction_Scope : Entity_Id;
      Locals         : List_Id;
      Name_If        : out Name_Id;
      Name_End_If    : out Name_Id);

   procedure Build_Case_State_Names
     (Reaction_Scope : Entity_Id;
      Locals         : List_Id;
      Name_Case      : out Name_Id;
      Name_End_Case  : out Name_Id);

   procedure Build_Loop_State_Names
      (Reaction_Scope : Entity_Id;
       Locals         : List_Id;
       Name_Loop      : out Name_Id;
       Name_Exit      : out Name_Id;
       Name_Repeat    : out Name_Id;
       Name_End_Loop  : out Name_Id);

   procedure Build_Abort_State_Names
     (Reaction_Scope : Entity_Id;
      Locals         : List_Id;
      Name_Abort     : out Name_Id;
      Name_End_Abort : out Name_Id);


   function Current_Reaction_Procedure_Entity (S : Node_Id) return Entity_Id;
   function Current_Reactive_Body_Entity (S : Node_Id) return Entity_Id;

   function Current_Reactive_Body (S : Node_Id) return Node_Id;

   -- Vertices --
   --------------

   --  A vertex represents a waiting statement. A waiting statement is
   --  a statement that has at least a wait state. So the reactive statements
   --  like wait, pause, select and fork are waiting states, but composed
   --  statements like if, loop, case are waiting if at least one of the
   --  alternative or body of this coumpound statement a reactive wait
   --  statement. The vertices are linked together in a Rlist and is calles
   --  a graph. Each waiting statement has a vertex assciated with it. The
   --  graph is a structured graph, in the sense that a vertex has on and
   --  only one previous vertex and next vertex in the list. The composed
   --  statement have one graph for each alternative composing the statement.
   --  For example a waiting if statement has a graph representing the then
   --  part, a graph for each eslif alternative and one mre for the else
   --  part. A compound vertex is composed by a entry state and an exit state
   --  and represents the entry and exit states of the sub graphs.

   --  For exaple for a if vertex :
   --    Vetex                        --------->    if expr then
   --       Then_graph (sub graph)    --------->         then_statements
   --       Alternatives (sub graphs) --------->       elsif expr ....
   --       else part (sub graph)     --------->       else part .....
   --       end vertex                --------->    end if


   function Make_Graph_Vertex
     (N : Node_Id;
      L : List_Id;
      K : Rnode_Kind := R_Graph_Vertex) return Rnode_Id;
   --  A graph vertex represents a waiting statement list. The statements are
   --  all represented by a vertex in the list. The K parameter is to choice
   --  the building of a conditional graph or not. The difference between a
   --  conditional graph a graph is that the conditonal graph holds a expression
   --  that condition the graph.L

   function Analyze_Statements
     (Gn : Rnode_Id;
      L  : List_Id) return RList_Id;
   --  Used to walk throught the statement list, and build the corresponding
   --  vertices of the waiting statements. The vertices are link together in
   --  a Vertices list.

   function Make_Pause_Vertex  (N : Node_Id) return Rnode_Id;
   --  Create the pause vertex. This vertex is simple, no information is
   --  associated with it, other than the correspoding node N.

   function Make_Wait_Vertex   (N : Node_Id) return Rnode_Id;
   --  Create the vertex associated with the wait statement N.

   function Make_Select_Vertex (N : Node_Id) return Rnode_Id;
   --  Create the vertex associated with the select statement N. The vertex holds
   --  an End_Vertex and a conditional graph for each alternative of the select
   --  statement. The condition of the conditional graph is the condition of the
   --  the correspondign alternative of the select statement

   function Make_Fork_Vertex   (N : Node_Id) return Rnode_Id;
   --  Create the vertex associated with the fork statement N. The vertex holds th
   --  an End_Vertex and a graph for each alternative of the fork statement. The
   --  condition in the vertex is the exit condition of the fork.

   function Make_Abort_Vertex  (N : Node_Id) return Rnode_Id;
   --  Create the vertex associated with the abort statement N. The vertex has
   --  a condition which is the consition of the abort statement, a graph for
   --  its body and another graph for the handler if present in the abort
   --  statement

   function Make_If_Vertex     (N : Node_Id) return Rnode_Id;
   --  Create the vertex associated with the if statement N. the vertex is
   --  composed of a condition which is the conditon of the then part of the
   --  if statement, a conditional graph for each elsif alternatives, and
   --  a graph for the else part of the if statement

   function Make_Loop_Vertex   (N : Node_Id) return Rnode_Id;
   --  Create the vertex asscoiated with the waiting loop statement N.
   --  This vertex holds a end vertex, a repeat vertex and an exit vertex
   --  the body of the loop is the graph. The interpretation of the waiting
   --  loop statment is rather complex, see presentation of the interpretation
   --  for a complete description of the functions of all the vertex composing
   --  this statement.

   function Make_Exit_Vertex   (N : Node_Id) return Rnode_Id;
   --  Create the vertex asscociated with the exit statement N. This vertex
   --  is a child of the loop vertex. here to, see the descrtiption of the
   --  loop statement for a complete description.

   ---------------------
   -- States Entities --
   ---------------------

   --  The declaration of the entities asscoitaed with vertex is done in two 
   --  passes. The first one creates all entities corresponding to the states
   --  thet are not anonymous, it is to said that have a name given by the user
   --  in the source. The second pass creates the entities for all others
   --  states. This mechanism is used because we want to enter in declarative 
   --  scope the entities corresponding to the user defined name states, in
   --  order to avoid names clasches with the interpretor created names. The
   --  entities associated with the states are boolean variables used by the
   --  interpretor is update the new state of the chart. The chart is composed
   --  of two type of states transient states and waiting states. The chart
   --  abandones the execution when it reaches a waitibng states, or the 
   --  transition associated to the waiting states is false. The transient
   --  states have no transition and are created to simplify the interpretor.
   --  The transient states are activated and desactived in the same cyle of
   --  exceution.

   procedure Pre_Declar_Graph_Vertex
     (G       : Rnode_Id;
      Globals : List_Id;
      Locals  : List_Id);
   --  Walk recursevly throught the vertices list of the graph and declare the state entities
   --  for each vertex associated wiht a waiting statement that has an user states name

   procedure Declar_Graph_Vertex
     (G       : Rnode_Id;
      Globals : List_Id;
      Locals  : List_Id);
   --  Walk recursevly throught the vertices list of the graph and declare all state entities
   --  that are not been pre declared.

   procedure Declar_Waiting_Vertex
     (V       : Rnode_Id;
      Globals : List_Id;
      Locals  : List_Id);
   --  A wait statement created five boolean variables :
   --   . Xc  : a boolean variable witch represents the current state activity
   --   . Xa  : a boolean variable the state must be activated
   --   . Xd  : a boolean variable the state must be desactivated
   --   . Xre : a boolean variable the raising edge of the activation state
   --   . Xfe : a boolean variable the falling edge of the activation state
   --  The two variables Xa, Xd must be activated and must be desasctivated are declared
   --  in the local part of the reaction procedure, and the tree others are declared
   --  in the declartion scope of the reaction procédure, as the value is retained between
   --  two execution cyles.

   procedure Declar_Transient_Vertex
     (V      : Rnode_Id;
      Locals : List_Id);

   procedure Declar_Fork_Vertex
     (V       : Rnode_Id;
      Locals  : List_Id);

   procedure Declar_Select_Vertex
     (V       : Rnode_Id;
      Globals : List_Id;
      Locals  : List_Id);

   procedure Declar_If_Vertex
     (V       : Rnode_Id;
      Globals : List_Id;
      Locals  : List_Id);

   procedure Declar_Loop_Vertex
     (V       : Rnode_Id;
      Globals : List_Id;
      Locals  : List_Id);

   procedure Declar_Exit_Vertex
     (V      : Rnode_Id;
      Locals : List_Id);

   procedure Declar_Abort_Vertex
     (V       : Rnode_Id;
      Globals : List_Id;
      Locals  : List_Id);

   procedure Create_Initialize_Code (Gn : Rnode_Id);

   function Make_If_Trans_Activation_Desactivation
     (V    : Rnode_Id;
      Nxt  : Rnode_Id;
      Cond : Node_Id) return Node_Id;

   function Make_If_Activation_Desactivation
     (V    : Rnode_Id;
      Nxt  : Rnode_Id;
      Cond : Node_Id) return Node_Id;

   function Make_Activation (V : Rnode_Id) return Node_Id;
   function Make_Desactivation (V : Rnode_Id) return Node_Id;

   procedure Make_Actication_Desactivation
     (Stmts     : List_Id;
      V_Current : Rnode_Id;
      V_Next    : Rnode_Id);

   procedure Add_Transitionnal_Code
     (Stmts : List_Id;
      V     : Rnode_Id;
      Nxt   : Rnode_Id);

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


   procedure Collect_Code_Graph
     (Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      G                : Rnode_Id);

   procedure Collect_Pause_Vertex
     (Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      V                : Rnode_Id);

   procedure Collect_Wait_Vertex
     (Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      V                : Rnode_Id);

   procedure Collect_Sync_Vertex
     (Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      V                : Rnode_Id);

   procedure Collect_Abort_Vertex
     (Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      V                : Rnode_Id);

   procedure Collect_Select_Vertex
     (Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      V                : Rnode_Id);

   procedure Collect_Fork_Vertex
     (Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      V                : Rnode_Id);
   procedure Collect_If_Vertex
     (Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      V                : Rnode_Id);

   procedure Collect_Loop_Vertex
     (Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      V                : Rnode_Id);

   procedure Collect_Exit_Vertex
     (Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      V                : Rnode_Id);

   procedure Collect_Handler_Vertex
     (Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      V                : Rnode_Id);


   function Has_Waiting_Statement      (N : Node_Id) return Boolean;
   function If_Has_Waiting_Statement   (N : Node_Id) return Boolean;
   function Loop_Has_Waiting_Statement (N : Node_Id) return Boolean;

   procedure Reset_Name_Statements (N : Node_Id);


   procedure Expand_Loop_Statements (N : in Node_Id);

   procedure Prefix_Name_Statements
     (N    : Node_Id;
      This : Node_Id);


   State_Count        : Natural := 0;
   If_State_Count     : Natural := 0;
   Abort_State_Count  : Natural := 0;
   Case_State_Count   : Natural := 0;
   Loop_State_Count   : Natural := 0;
   Fork_State_Count   : Natural := 0;
   Select_State_Count : Natural := 0;
   Wait_State_Count   : Natural := 0;
   Pause_State_Count  : Natural := 0;

   Globals : List_Id;
   Locals  : List_Id;
   --  Globals is the list of declarations  of Reactive and Locals
   --  is the locals declaration of the procedure corresponding to
   --  React..

   -----------------
   --  Initialize --
   -----------------

   procedure Initialize is
   begin
      Rtree.Initialize;
      Rlists.Initialize;
      Nconv.Init_Nodes_Association_Tables;
      Accessors_Assoc.Init_Nodes_Association_Tables;
      Vars_Assoc.Init_Nodes_Association_Tables;
   end Initialize;

   -----------------------
   -- Reset_State_Count --
   -----------------------

   procedure Reset_State_Count is
   begin
      State_Count        := 0;
      If_State_Count     := 0;
      Case_State_Count   := 0;
      Loop_State_Count   := 0;
      Fork_State_Count   := 0;
      Select_State_Count := 0;
      Wait_State_Count   := 0;
      Pause_State_Count  := 0;
   end Reset_State_Count;

   -----------------
   -- Parent_Loop --
   -----------------

   function Parent_Loop (N : Node_Id) return Node_Id is
      P : Node_Id;
   begin
      P := Parent (N);
      while Present (P) loop
	 if Nkind (P) = N_Loop_Statement then
	    return P;
	 end if;
	 P := Parent (P);
      end loop;
      return Empty;
   end Parent_Loop;


   ---------------------------------------
   -- Current_Reaction_Procedure_Entity --
   ---------------------------------------

   function Current_Reaction_Procedure_Entity (S : Node_Id) return Entity_Id is
      React : Node_Id;
      Ereact : Entity_Id;
   begin
      if No (S) then
	 return Empty;
      end if;

      case Nkind (S) is
	 when N_Reactive_Body_Description =>
	    React := Reaction_Proc (S);
	    if Present (React) then
	       if Nkind (React) = N_Subprogram_Body then
		  if Nkind (Defining_Unit_Name (Specification (React))) =
		    N_Defining_Identifier then
		     Ereact := Defining_Unit_Name (Specification (React));
		  else
		     Ereact := Defining_Identifier
		       (Defining_Unit_Name (Specification (React)));
		  end if;
	       else
		  Ereact := Empty;
	       end if;
	    end if;

	 when others =>
	    Ereact := Current_Reaction_Procedure_Entity (Parent (S));
      end case;

      return Ereact;
   end Current_Reaction_Procedure_Entity;

   ---------------------------
   -- Current_Reactive_Body --
   ---------------------------

   function Current_Reactive_Body (S : Node_Id) return Node_Id is
      NReact : Entity_Id;
   begin
      if No (S) then
	 return Empty;
      end if;

      case Nkind (S) is
	 when N_Reactive_Body =>
	    NReact := Defining_Identifier (Specification (S));
	 when others =>
	    NReact := Current_Reactive_Body (Parent (S));
      end case;

      return NReact;
   end Current_Reactive_Body;



   ----------------------------------
   -- Current_Reactive_Body_Entity --
   ----------------------------------

   function Current_Reactive_Body_Entity (S : Node_Id) return Entity_Id is
      Ereact : Entity_Id;
   begin
      if No (S) then
	 return Empty;
      end if;

      case Nkind (S) is
	 when N_Reactive_Body =>
	    Ereact := Defining_Identifier (Specification (S));
	 when others =>
	    Ereact := Current_Reactive_Body_Entity (Parent (S));
      end case;

      return Ereact;
   end Current_Reactive_Body_Entity;

   ----------------------------------
   -- Current_Name_Entity_In_Scope --
   ----------------------------------

   function Exist_Name_Entity_In_Scope
     (Name : Name_Id;
      Scop : Entity_Id) return Boolean is

      E  : Entity_Id;
   begin
      E := Get_Name_Entity_Id (Name);
      while Present (E)
        and then Scope (E) /= Scop
      loop
         E := Homonym (E);
      end loop;

      return Present (E);
   end Exist_Name_Entity_In_Scope;

   --------------------------------
   -- Exist_Name_Entity_In_Scope --
   --------------------------------

   function Exist_Name_Entity_In_Scope
     (S    : String;
      Scop : Entity_Id) return Boolean is
   begin
      return Exist_Name_Entity_In_Scope (String_Find (S), Scop);
   end Exist_Name_Entity_In_Scope;

   -------------------------------
   -- Prefix_Name_By_Scope_Name --
   -------------------------------

   function Prefix_Name_By_Scope_Name
     (Scop : Entity_Id;
      Obj  : Entity_Id) return Name_Id is

      Scope_Name  : String := Get_String (Chars (Scop));
      Object_Name : String := Get_String (Chars (Obj));
   begin
      Add_New_String (Scope_Name);
      Add_Char_To_Name_Buffer ('_');
      Add_Str_To_Name_Buffer (Object_Name);
      return Name_Find;
   end Prefix_Name_By_Scope_Name;

   -----------------------
   -- Enter_Indice_Name --
   -----------------------

   function Enter_Indice_Name
     (Nam   : Name_Id;
      Count : Nat := 0) return Name_Id is

      Str : String := Get_String (Nam);
   begin
      Add_New_String (Str);
      Add_Char_To_Name_Buffer ('_');
      Add_Nat_To_Name_Buffer (Count);
      return Name_Find;
   end Enter_Indice_Name;

   --------------------
   -- New_State_Name --
   --------------------

   function New_Prefix_Scope_Name
     (Scop : Entity_Id;
      Obj  : Entity_Id) return Name_Id is

      Name  : Name_Id;
      Count : Nat := 0;
   begin
      Name := Prefix_Name_By_Scope_Name (Scop, Obj);
      while Exist_Name_Entity_In_Scope (Name, Scop) loop
	 Count := Count + 1;
	 Name := Enter_Indice_Name (Name, Count);
      end loop;

      return Name;
   end New_Prefix_Scope_Name;

   ----------------------
   -- Enter_State_Name --
   ----------------------

   function Enter_State_Name
     (Prefix : String;
      Suffix : String) return Name_Id is
   begin
      Add_New_String (Prefix);
      Add_Nat_To_Name_Buffer (Nat (State_Count));
      Add_Str_To_Name_Buffer (Suffix);
      State_Count := State_Count + 1;
      return Name_Find;
   end Enter_State_Name;

   --------------------
   -- New_State_Name --
   --------------------

   function New_State_Name
     (Prefix : String;
      Suffix : String;
      Scop   : Entity_Id) return Name_Id is

      Name : Name_Id;
   begin
      Name := Enter_State_Name (Prefix, Suffix);
      while Exist_Name_Entity_In_Scope (Name, Scop) loop
	 Name := Enter_State_Name (Prefix, Suffix);
      end loop;

      return Name;
   end New_State_Name;

   ------------------------
   -- Exist_Name_In_List --
   -------------------------

   function Exist_Name_Entity_In_List
     (Name : Name_Id;
      Lst  : List_Id) return Boolean is

      E   : Node_Id;
      Res : Boolean;
   begin
      Res := False;
      if Is_Non_Empty_List (Lst) then
	 E := First (Lst);
	 while Present (E) loop
	    --  if Nkind (E) = N_Object_Declaration then
	       if Name = Chars (Defining_Identifier (E)) then
		  Res := True;
		  exit;
	       end if;
	    --  end if;
	    Next (E);
	 end loop;
      end if;

      return Res;
   end Exist_Name_Entity_In_List;

   -----------------------------
   --  Build_Fork_State_Names --
   -----------------------------

   procedure Build_Fork_State_Names
     (Reaction_Scope : Entity_Id;
      Locals         : List_Id;
      Name_Fork      : out Name_Id;
      Name_End_Fork  : out Name_id) is

      function Enter_Fork_State_Name
	(Prefix : String) return Name_Id;

      ---------------------------
      -- Enter_Fork_State_Name --
      ---------------------------

      function Enter_Fork_State_Name
	(Prefix : String) return Name_Id is
      begin
	 Add_New_String (Prefix);
	 Add_Nat_To_Name_Buffer (Nat (Fork_State_Count));
	 return Name_Find;
      end Enter_Fork_State_Name;

   begin
      Put_Line ("Build_Fork_State_Names Begin");

      Name_Fork     := String_Find ("xfork");
      Name_End_Fork := String_Find ("xend_fork");

      loop
	   --  Verify that the activation and desactivation are not
	   --  in reaction procedure

	   if not Exist_Name_Entity_In_Scope (Name_Fork, Reaction_Scope)
	   and then not Exist_Name_Entity_In_Scope (Name_End_Fork, Reaction_Scope)

	   --  Verify that the activation and desactivation are not
	   --  in locals

	     and then not Exist_Name_Entity_In_List (Name_Fork, Locals)
	   and then not Exist_Name_Entity_In_List (Name_End_Fork, Locals)
	 then
	    exit;
	 end if;

	 Fork_State_Count := Fork_State_Count + 1;
	 Name_Fork     := Enter_Fork_State_Name ("xfork");
	 Name_End_Fork := Enter_Fork_State_Name ("xend_fork");

	 Put_Line ("  Name_Fork     => " & Get_String (Name_Fork));
	 Put_Line ("  Name_End_Fork => " & Get_String (Name_End_Fork));
      end loop;

      Put_Line ("Build_Fork_State_Names End");
   end Build_Fork_State_Names;

   -------------------------------
   --  Build_Select_State_Names --
   -------------------------------

   procedure Build_Select_State_Names
     (Prefix          : String;
      Reactive_Scope  : Entity_Id;
      Reaction_Scope  : Entity_Id;
      Globals         : List_Id;
      Locals          : List_Id;
      Name_Select     : out Name_Id;
      Name_End_Select : out Name_Id) is

      Name  : Name_Id := String_Find (Prefix) ;
      Namc  : Name_Id;
      Nama  : Name_Id;
      Namd  : Name_Id;
      Namre : Name_Id;
      Namfe : Name_Id;
      Nend  : Name_Id;

      function Enter_Select_State_Name
	(Prefix : String) return Name_Id;

      -----------------------------
      -- Enter_Select_State_Name --
      -----------------------------

      function Enter_Select_State_Name
	(Prefix : String) return Name_Id is
      begin
	 Add_New_String (Prefix);
	 Add_Nat_To_Name_Buffer (Nat (Select_State_Count));
	 return Name_Find;
      end Enter_Select_State_Name;

   begin
      Put_Line ("Build_Select_State_Names Begin");
      loop
	 Namc  := String_Find (Get_String (Name) & "c");
	 Nama  := String_Find (Get_String (Name) & "a");
	 Namd  := String_Find (Get_String (Name) & "d");
	 Namre := String_Find (Get_String (Name) & "re");
	 Namfe := String_Find (Get_String (Name) & "fe");
	 Nend  := String_Find (Get_String (Name) & "end");

	 --  Verify that names Activity, raisinget falling edge
	 --  are not in reactive scope

	 if not Exist_Name_Entity_In_Scope (Namd, Reactive_Scope)
	   and then not Exist_Name_Entity_In_Scope (Namre, Reactive_Scope)
	   and then not Exist_Name_Entity_In_Scope (Namfe, Reactive_Scope)

	   --  Verify that names Activity, raisinget falling edge
	   --  are not in globals

	   and then not Exist_Name_Entity_In_List (Namd,  Globals)
	   and then not Exist_Name_Entity_In_List (Namre, Globals)
	   and then not Exist_Name_Entity_In_List (Namfe, Globals)

	   --  Verify that the activation and desactivation are not
	   --  in reaction procedure

	   and then not Exist_Name_Entity_In_Scope (Nama,  Reaction_Scope)
	   and then not Exist_Name_Entity_In_Scope (Namc,  Reaction_Scope)
	   and then not Exist_Name_Entity_In_Scope (Nend,  Reaction_Scope)

	   --  Verify that the activation and desactivation are not
	   --  in locals

	   and then not Exist_Name_Entity_In_List (Nama,  Locals)
	   and then not Exist_Name_Entity_In_List (Namc,  Locals)
	   and then not Exist_Name_Entity_In_List (Nend,  Locals)
	 then
	    exit;
	 end if;

	 Select_State_Count := Select_State_Count + 1;
	 Name := Enter_Select_State_Name (Get_String (Name));

	 Put_Line ("  Name => " & Get_String (Name));
      end loop;

      Name_Select := Name;
      Name_End_Select := Nend;
      Put_Line ("Build_Select_State_Names End");
   end Build_Select_State_Names;

   --------------------------------
   --  Build_Waiting_State_Names --
   --------------------------------

   function Build_Waiting_State_Names
     (Prefix         : String;
      Suffix         : String;
      Reactive_Scope : Entity_Id;
      Reaction_Scope : Entity_Id;
      Globals        : List_Id;
      Locals         : List_Id) return Name_Id is

      Name  : Name_Id := String_Find (Prefix & Suffix) ;
      Namc  : Name_Id;
      Nama  : Name_Id;
      Namd  : Name_Id;
      Namre : Name_Id;
      Namfe : Name_Id;
      Name_Terminate  : Name_Id;
      Name_Init_Macro : Name_Id;
   begin
      Put_Line ("Build_Waiting_State_Names Begin");
      loop
	 --  Enter Xc for activity
	 --  Enter Xa to activate the step
	 --  Enter Xd to desactivate the state
	 --  Enter Xre for raising edge
	 --  Enter Xfe for falling edge

	 Namc  := String_Find (Get_String (Name) & "c");
	 Nama  := String_Find (Get_String (Name) & "a");
	 Namd  := String_Find (Get_String (Name) & "d");
	 Namre := String_Find (Get_String (Name) & "re");
	 Namfe := String_Find (Get_String (Name) & "fe");
	 Name_Terminate  := String_Find (Get_String (Name) & "_terminate");
	 Name_Init_Macro := String_Find (Get_String (Name) & "_init_macro");

	 --  Verify that names Activity, raisinget falling edge
	 --  are not in reactive scope

	 if not Exist_Name_Entity_In_Scope (Namd, Reactive_Scope)
	   and then not Exist_Name_Entity_In_Scope (Namre, Reactive_Scope)
	   and then not Exist_Name_Entity_In_Scope (Namfe, Reactive_Scope)

	   --  Verify that names Activity, raisinget falling edge
	   --  are not in globals

	   and then not Exist_Name_Entity_In_List (Namd,  Globals)
	   and then not Exist_Name_Entity_In_List (Namre, Globals)
	   and then not Exist_Name_Entity_In_List (Namfe, Globals)

	   --  Verify that the activation and desactivation are not
	   --  in reaction procedure

	   and then not Exist_Name_Entity_In_Scope (Nama,  Reaction_Scope)
	   and then not Exist_Name_Entity_In_Scope (Namc,  Reaction_Scope)
	   and then not Exist_Name_Entity_In_Scope (Name_Terminate,  Reaction_Scope)
	   and then not Exist_Name_Entity_In_Scope (Name_Init_Macro, Reaction_Scope)

	   --  Verify that the activation and desactivation are not
	   --  in locals

	   and then not Exist_Name_Entity_In_List (Nama,  Locals)
	   and then not Exist_Name_Entity_In_List (Namc,  Locals)
	   and then not Exist_Name_Entity_In_List (Name_Terminate,  Locals)
	   and then not Exist_Name_Entity_In_List (Name_Init_Macro, Locals)
	 then
	    exit;
	 end if;

	 Name := Enter_State_Name (Prefix, Suffix);
	 Put_Line ("  new Name => " & Get_String (Name));
      end loop;

      Put_Line ("Build_Waiting_State_Names End");
      return Name;
   end Build_Waiting_State_Names;

   ------------------------------
   --  Build_Pause_State_Names --
   ------------------------------

   function Build_Pause_State_Names
     (Reactive_Scope : Entity_Id;
      Reaction_Scope : Entity_Id;
      Globals        : List_Id;
      Locals         : List_Id) return Name_Id is

      Name  : Name_Id := String_Find ("xpause");
      Namc  : Name_Id;
      Nama  : Name_Id;
      Namd  : Name_Id;
      Namre : Name_Id;
      Namfe : Name_Id;

      function Enter_Pause_State_Name
	(Prefix : String) return Name_Id;

      ----------------------------
      -- Enter_Pause_State_Name --
      ----------------------------

      function Enter_Pause_State_Name
	(Prefix : String) return Name_Id is
      begin
	 Add_New_String (Prefix);
	 Add_Nat_To_Name_Buffer (Nat (Pause_State_Count));
	 return Name_Find;
      end Enter_Pause_State_Name;

   begin
      Put_Line ("Build_Pause_State_Names Begin");
      loop
	 --  Enter Xc for activity
	 --  Enter Xa to activate the step
	 --  Enter Xd to desactivate the state
	 --  Enter Xre for raising edge
	 --  Enter Xfe for falling edge

	 Namc  := String_Find (Get_String (Name) & "c");
	 Nama  := String_Find (Get_String (Name) & "a");
	 Namd  := String_Find (Get_String (Name) & "d");
	 Namre := String_Find (Get_String (Name) & "re");
	 Namfe := String_Find (Get_String (Name) & "fe");

	 --  Verify that names Activity, raisinget falling edge
	 --  are not in reactive scope

	 if not Exist_Name_Entity_In_Scope (Namd, Reactive_Scope)
	   and then not Exist_Name_Entity_In_Scope (Namre, Reactive_Scope)
	   and then not Exist_Name_Entity_In_Scope (Namfe, Reactive_Scope)

	   --  Verify that names Activity, raisinget falling edge
	   --  are not in globals

	   and then not Exist_Name_Entity_In_List (Namd,  Globals)
	   and then not Exist_Name_Entity_In_List (Namre, Globals)
	   and then not Exist_Name_Entity_In_List (Namfe, Globals)

	   --  Verify that the activation and desactivation are not
	   --  in reaction procedure

	   and then not Exist_Name_Entity_In_Scope (Nama,  Reaction_Scope)
	   and then not Exist_Name_Entity_In_Scope (Namc,  Reaction_Scope)

	   --  Verify that the activation and desactivation are not
	   --  in locals

	   and then not Exist_Name_Entity_In_List (Nama,  Locals)
	   and then not Exist_Name_Entity_In_List (Namc,  Locals)
	 then
	    exit;
	 end if;

	 Pause_State_Count := Pause_State_Count + 1;
	 Name := Enter_Pause_State_Name ("xpause");
	 Put_Line ("  new Name => " & Get_String (Name));
      end loop;

      Put_Line ("Build_Pause_State_Names End");
      return Name;
   end Build_Pause_State_Names;

   -----------------------------
   --  Build_Wait_State_Names --
   -----------------------------

   function Build_Wait_State_Names
     (Reactive_Scope : Entity_Id;
      Reaction_Scope : Entity_Id;
      Globals        : List_Id;
      Locals         : List_Id) return Name_Id is

      Name  : Name_Id := String_Find ("xwait");
      Namc  : Name_Id;
      Nama  : Name_Id;
      Namd  : Name_Id;
      Namre : Name_Id;
      Namfe : Name_Id;

      function Enter_Wait_State_Name
	(Prefix : String) return Name_Id;

      ----------------------------
      -- Enter_Wait_State_Name --
      ----------------------------

      function Enter_Wait_State_Name
	(Prefix : String) return Name_Id is
      begin
	 Add_New_String (Prefix);
	 Add_Nat_To_Name_Buffer (Nat (Wait_State_Count));
	 return Name_Find;
      end Enter_Wait_State_Name;

   begin
      Put_Line ("Build_Wait_State_Names Begin");
      loop
	 --  Enter Xc for activity
	 --  Enter Xa to activate the step
	 --  Enter Xd to desactivate the state
	 --  Enter Xre for raising edge
	 --  Enter Xfe for falling edge

	 Namc  := String_Find (Get_String (Name) & "c");
	 Nama  := String_Find (Get_String (Name) & "a");
	 Namd  := String_Find (Get_String (Name) & "d");
	 Namre := String_Find (Get_String (Name) & "re");
	 Namfe := String_Find (Get_String (Name) & "fe");

	 --  Verify that names Activity, raisinget falling edge
	 --  are not in reactive scope

	 if not Exist_Name_Entity_In_Scope (Namd, Reactive_Scope)
	   and then not Exist_Name_Entity_In_Scope (Namre, Reactive_Scope)
	   and then not Exist_Name_Entity_In_Scope (Namfe, Reactive_Scope)

	   --  Verify that names Activity, raisinget falling edge
	   --  are not in globals

	   and then not Exist_Name_Entity_In_List (Namd,  Globals)
	   and then not Exist_Name_Entity_In_List (Namre, Globals)
	   and then not Exist_Name_Entity_In_List (Namfe, Globals)

	   --  Verify that the activation and desactivation are not
	   --  in reaction procedure

	   and then not Exist_Name_Entity_In_Scope (Nama,  Reaction_Scope)
	   and then not Exist_Name_Entity_In_Scope (Namc,  Reaction_Scope)

	   --  Verify that the activation and desactivation are not
	   --  in locals

	   and then not Exist_Name_Entity_In_List (Nama,  Locals)
	   and then not Exist_Name_Entity_In_List (Namc,  Locals)
	 then
	    exit;
	 end if;

	 Wait_State_Count := Wait_State_Count + 1;
	 Name := Enter_Wait_State_Name ("xwait");
	 Put_Line ("  new Name => " & Get_String (Name));
      end loop;

      Put_Line ("Build_Wait_State_Names End");
      return Name;
   end Build_Wait_State_Names;

   --------------------------------
   --  Build_For_Loop_Count_Name --
   --------------------------------

   function Build_For_Loop_Count_Name
     (Name           : Name_Id;
      Reactive_Scope : Entity_Id;
      Reaction_Scope : Entity_Id;
      Globals        : List_Id;
      Locals         : List_Id) return Name_Id is

      ForCountName : Name_Id := Name;

      function Enter_For_Loop_Count_Name (Prefix : String) return Name_Id;

      -------------------------------
      -- Enter_For_Loop_Count_Name --
      -------------------------------

      function Enter_For_Loop_Count_Name (Prefix : String) return Name_Id is
      begin
	 Add_New_String (Prefix);
	 Add_Nat_To_Name_Buffer (Nat (Wait_State_Count));
	 return Name_Find;
      end Enter_For_Loop_Count_Name;

   begin
      Put_Line ("Build_For_Loop_Count_Name started for : ");

      --  Verify that name is not in reactive scope, reaction
      --  scope, global or local variables list
      if not Exist_Name_Entity_In_Scope (ForCountName, Reactive_Scope)
        and then not Exist_Name_Entity_In_List (ForCountName,  Globals)
        and then not Exist_Name_Entity_In_Scope (ForCountName,  Reaction_Scope)
        and then not Exist_Name_Entity_In_List (ForCountName,  Locals)
        and then not Exist_Name_Entity_In_List
          (Name => ForCountName,
           Lst  => Transformer.Scopes.Transformed_Declarations
             (Transformer.Scopes.Top_Transform_Scope))
      then
         return ForCountName;
      end if;

      ForCountName := Enter_For_Loop_Count_Name (Get_String (ForCountName));
      Put_Line ("  new ForCountName => " & Get_String (ForCountName));

      return ForCountName;
   end Build_For_Loop_Count_Name;

   -----------------------------
   --  Build_Loop_State_Names --
   -----------------------------

   procedure Build_Loop_State_Names
     (Reaction_Scope : Entity_Id;
      Locals         : List_Id;
      Name_Loop      : out Name_Id;
      Name_Exit      : out Name_Id;
      Name_Repeat    : out Name_Id;
      Name_End_Loop  : out Name_Id) is

      function Enter_Loop_State_Name
	(Prefix : String) return Name_Id;

      ---------------------------
      -- Enter_Loop_State_Name --
      ---------------------------

      function Enter_Loop_State_Name
	(Prefix : String) return Name_Id is
      begin
	 Add_New_String (Prefix);
	 Add_Nat_To_Name_Buffer (Nat (Loop_State_Count));
	 return Name_Find;
      end Enter_Loop_State_Name;

   begin
      Put_Line ("Build_Loop_State_Names Begin");
      Name_Loop     := String_Find ("xloop");
      Name_Exit     := String_Find ("xexit_loop");
      Name_Repeat   := String_Find ("xrepeat_loop");
      Name_End_Loop := String_Find ("xend_loop");

      loop
	   --  Verify that the state names used for the loop states are not
	   --  in reaction procedure

	 if not Exist_Name_Entity_In_Scope (Name_Loop, Reaction_Scope)
	   and then not Exist_Name_Entity_In_Scope (Name_Exit, Reaction_Scope)
	   and then not Exist_Name_Entity_In_Scope (Name_Repeat, Reaction_Scope)
	   and then not Exist_Name_Entity_In_Scope (Name_End_Loop, Reaction_Scope)

	   --  Verify that the state names used for the loop states are not
	   --  in locals

	   and then not Exist_Name_Entity_In_List (Name_Loop, Locals)
	   and then not Exist_Name_Entity_In_List (Name_Exit, Locals)
	   and then not Exist_Name_Entity_In_List (Name_Repeat, Locals)
	   and then not Exist_Name_Entity_In_List (Name_End_Loop, Locals)
	 then
	    exit;
	 end if;

	 Loop_State_Count := Loop_State_Count + 1;
	 Name_Loop        := Enter_Loop_State_Name ("xloop");
	 Name_Exit        := Enter_Loop_State_Name ("xexit_loop" );
	 Name_Repeat      := Enter_Loop_State_Name ("xrepeat_loop");
	 Name_End_Loop    := Enter_Loop_State_Name ("xend_loop");

	 Put_Line ("  xloop        => " & Get_String (Name_Loop));
	 Put_Line ("  xexit_loop   => " & Get_String (Name_Exit));
	 Put_Line ("  xrepeat_loop => " & Get_String (Name_Repeat));
	 Put_Line ("  xend_loop    => " & Get_String (Name_End_Loop));
      end loop;

      Put_Line ("Build_Loop_State_Names End");
   end Build_Loop_State_Names;

   ---------------------------
   --  Build_If_State_Names --
   ---------------------------

   procedure Build_If_State_Names
     (Reaction_Scope : Entity_Id;
      Locals         : List_Id;
      Name_If        : out Name_Id;
      Name_End_If    : out Name_Id) is

      function Enter_if_State_Name
	(Prefix : String) return Name_Id;

      -------------------------
      -- Enter_If_State_Name --
      -------------------------

      function Enter_if_State_Name
	(Prefix : String) return Name_Id is
      begin
	 Add_New_String (Prefix);
	 Add_Nat_To_Name_Buffer (Nat (If_State_Count));
	 return Name_Find;
      end Enter_If_State_Name;

   begin
      Put_Line ("Build_If_State_Names Begin");
      Name_If     := String_Find ("xif");
      Name_End_If := String_Find ("xend_if");

      loop
	   --  Verify that the state names used for the loop states are not
	   --  in reaction procedure

	 if not Exist_Name_Entity_In_Scope (Name_If, Reaction_Scope)
	   and then not Exist_Name_Entity_In_Scope (Name_End_If, Reaction_Scope)

	   --  Verify that the state names used for the loop states are not
	   --  in locals

	   and then not Exist_Name_Entity_In_List (Name_If, Locals)
	   and then not Exist_Name_Entity_In_List (Name_End_If, Locals)
	 then
	    exit;
	 end if;

	 If_State_Count := If_State_Count + 1;
	 Name_If        := Enter_If_State_Name ("xif");
	 Name_End_If    := Enter_If_State_Name ("xend_if");

	 Put_Line ("  xif     => " & Get_String (Name_If));
	 Put_Line ("  xend_if => " & Get_String (Name_End_If));
      end loop;

      Put_Line ("Build_If_State_Names End");
   end Build_If_State_Names;

      ------------------------------
   --  Build_Abort_State_Names --
   ------------------------------

   procedure Build_Abort_State_Names
     (Reaction_Scope : Entity_Id;
      Locals         : List_Id;
      Name_Abort     : out Name_Id;
      Name_End_Abort : out Name_Id) is

      function Enter_Abort_State_Name
	(Prefix : String) return Name_Id;

      ----------------------------
      -- Enter_Abort_State_Name --
      ----------------------------
      function Enter_Abort_State_Name
	(Prefix : String) return Name_Id is
      begin
	 Add_New_String (Prefix);
	 Add_Nat_To_Name_Buffer (Nat (If_State_Count));
	 return Name_Find;
      end Enter_Abort_State_Name;

   begin
      Name_Abort     := String_Find ("xabort");
      Name_End_Abort := String_Find ("xend_abort");

      loop
	   --  Verify that the state names used for the loop states are not
	   --  in reaction procedure
         if not Exist_Name_Entity_In_Scope (Name_Abort, Reaction_Scope)
           and then
             not Exist_Name_Entity_In_Scope (Name_End_Abort, Reaction_Scope)

	   --  Verify that the state names used for the loop states are not
	   --  in locals

	   and then not Exist_Name_Entity_In_List (Name_Abort, Locals)
	   and then not Exist_Name_Entity_In_List (Name_End_Abort, Locals)
	 then
	    exit;
	 end if;

	 Abort_State_Count := Abort_State_Count + 1;
	 Name_Abort        := Enter_Abort_State_Name ("xabort");
	 Name_End_Abort    := Enter_Abort_State_Name ("xend_abort");
      end loop;

   end Build_Abort_State_Names;




   -----------------------------
   --  Build_Case_State_Names --
   -----------------------------

   procedure Build_Case_State_Names
     (Reaction_Scope : Entity_Id;
      Locals         : List_Id;
      Name_Case      : out Name_Id;
      Name_End_Case  : out Name_Id) is

      function Enter_Case_State_Name
	(Prefix : String) return Name_Id;

      ---------------------------
      -- Enter_Case_State_Name --
      ---------------------------

      function Enter_Case_State_Name
	(Prefix : String) return Name_Id is
      begin
	 Add_New_String (Prefix);
	 Add_Nat_To_Name_Buffer (Nat (Case_State_Count));
	 return Name_Find;
      end Enter_Case_State_Name;

   begin
      Put_Line ("Build_Case_State_Names Begin");
      Name_Case     := String_Find ("xcase");
      Name_End_Case := String_Find ("xend_case");

      loop
	   --  Verify that the state names used for the loop states are not
	   --  in reaction procedure

	 if not Exist_Name_Entity_In_Scope (Name_Case, Reaction_Scope)
	   and then not Exist_Name_Entity_In_Scope (Name_End_Case, Reaction_Scope)

	   --  Verify that the state names used for the loop states are not
	   --  in locals

	   and then not Exist_Name_Entity_In_List (Name_Case, Locals)
	   and then not Exist_Name_Entity_In_List (Name_End_Case, Locals)
	 then
	    exit;
	 end if;

	 Case_State_Count := Case_State_Count + 1;
	 Name_Case        := Enter_Case_State_Name ("xcase");
	 Name_End_Case    := Enter_Case_State_Name ("xend_case");

	 Put_Line ("  xcase     => " & Get_String (Name_Case));
	 Put_Line ("  xend_case => " & Get_String (Name_End_Case));
      end loop;

      Put_Line ("Build_Case_State_Names End");
   end Build_Case_State_Names;

   ----------------------------------
   --  Build_Transient_State_Names --
   ----------------------------------

   function Build_Transient_State_Names
     (Prefix         : String;
      Suffix         : String;
      Reaction_Scope : Entity_Id;
      Locals         : List_Id) return Name_Id is

      Name : Name_Id := String_Find (Prefix & Suffix) ;
      Namc : Name_Id;
   begin
      Put_Line ("Build_Transient_State_Names Begin");
      loop
	 Name := Enter_State_Name (Prefix, Suffix);

	 --  Enter Xc for activity
	 --  Enter Xa to activate the step
	 --  Enter Xd to desactivate the state
	 --  Enter Xre for raising edge
	 --  Enter Xfe for falling edge

	 Namc  := String_Find (Get_String (Name) & "c");

	 --  Verify that names Activity, raisinget falling edge
	 --  are not in reactive scope

	 if not Exist_Name_Entity_In_Scope (Namc, Reaction_Scope)
	   and then not Exist_Name_Entity_In_List (Namc,  Locals)
	 then
	    exit;
	 end if;

      end loop;

      Put_Line ("Build_Transient_State_Names End");
      return Name;
   end Build_Transient_State_Names;

   ------------------------------------
   --  Make_New_Defining_Identifier  --
   ------------------------------------

   function Make_New_Defining_Identifier
     (Prefix : String;
      Suffix : String;
      Scop   : Entity_Id;
      Sloc   : Source_Ptr) return Node_Id is

      Name : Name_Id := String_Find (Prefix & Suffix);
   begin
      while Exist_Name_Entity_In_Scope (Name, Scop) loop
	 Name := Enter_State_Name (Prefix, Suffix);
      end loop;

      return Make_Defining_Identifier (Sloc, Name);
   end Make_New_Defining_Identifier;

   -------------------------
   -- Make_Reactive_Graph --
   -------------------------

   function Make_Reactive_Graph (N : in Node_Id) return Rnode_Id
   is
      pragma Assert (Present (N)
                     and then Nkind (N) = N_Reactive_Body);
      Body_Spec        : Node_Id;
      Body_Desc        : Node_Id;
      React_Proc       : Node_Id;
      Rn               : Rnode_Id;
      Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      Stmts            : List_Id;
   begin
      Put_Line ("Make_Reactive_Graph started for "
                & N'Img & " : " & Nkind (N)'Img);

      Body_Spec := Specification (N);
      Body_Desc := Body_Description (N);
      React_Proc := Reaction_Proc (Body_Desc);

      Globals := Declarations (Body_Desc);
      Locals  := Declarations (React_Proc);

      Reset_State_Count;
      Stmts := Statements (Handled_Statement_Sequence  (React_Proc));
      if Is_Empty_List (Stmts) then
         return No_Rnode;
      end if;

      --  Add a pause as the last statement in chart
      
      Pause_Vertex := New_Pause_Vertex;
      
      Append_To
	(Stmts, Pause_Vertex_Class_Ptr (Pause_Vertex));

      --  Create the reactive graph of the reaction procedure
      Rn := Make_Graph_Vertex
	(React_Proc, Statements (Handled_Statement_Sequence  (React_Proc)));

      --  Initialize Globals and Locals declarations generated by the
      --  interpreter
      
      Set_Global_States_Declarations (Rn, New_List);
      Set_Local_States_Declarations (Rn, New_List);

      --  Declar Step variables.
      Pre_Declar_Graph_Vertex
	(Rn,
	 Global_States_Declarations (Rn),
	 Local_States_Declarations (Rn));

      Declar_Graph_Vertex
	(Rn,
	 Global_States_Declarations (Rn),
	 Local_States_Declarations (Rn));

      --  Interpret reactive code
      Put_Line (" Going to Interp_Graph");
      Interp_Graph_Vertex (Rn);
      Put_Line (" From Interp_Graph");

      --  Collect interpretation code.
      Update_Raz_Stmts := New_List;
      Update_Stmts     := New_List;
      Trans_Stmts      := New_List;

      Collect_Code_Graph
	(Update_Raz_Stmts,
	 Update_Stmts,
	 Trans_Stmts,
	 Rn);

      Set_Update_Raz_Code (Rn, Update_Raz_Stmts);
      Set_Trans_Code (Rn, Trans_Stmts);
      Set_Update_Code (Rn, Update_Stmts);

      --  Create Initialize code of graph
      Create_Initialize_Code (Rn);

      --  Generate SVG file
      declare
	 S : String := Get_String (Chars (Defining_Identifier (Body_Spec)));
      begin
	 Graphs_Svg.Dump_Svg_Graph
	   (G         => Rn,
	    File_Name => S);
      end;

      Put_Line ("Make_Reactive_Graph End");
      return Rn;
   end Make_Reactive_Graph;

   ------------------------------
   -- If_Has_Waiting_Statement --
   ------------------------------

   function If_Has_Waiting_Statement (N : Node_Id) return Boolean is
      E : Node_Id;
   begin
      if Is_Non_Empty_List (Then_Statements (N)) then
	 if Has_Waiting_Statement (First (Then_Statements (N))) then
	    return True;
	 end if;
      end if;

      if Present (Elsif_Parts (N)) then
         E := First (Elsif_Parts (N));
         while Present (E) loop
	    if Is_Non_Empty_List (Then_Statements (E)) then
	       if Has_Waiting_Statement (First (Then_Statements (E))) then
		  return True;
	       end if;
	    end if;

            Next (E);
         end loop;
      end if;

      if Present (Else_Statements (N)) then
	 if Is_Non_Empty_List (Else_Statements (N)) then
	    if Has_Waiting_Statement (First (Else_Statements (N))) then
	       return True;
	    end if;
	 end if;
      end if;

      return False;
   end If_Has_Waiting_Statement;

   --------------------------------
   -- Loop_Has_Waiting_Statement --
   --------------------------------

   function Loop_Has_Waiting_Statement (N : Node_Id) return Boolean is
   begin
      if Is_Non_Empty_List (Statements (N)) then
	 if Has_Waiting_Statement (First (Statements (N))) then
	    return True;
	 end if;
      end if;

      return False;
   end Loop_Has_Waiting_Statement;

   --------------------------------
   -- Case_Has_Waiting_Statement --
   --------------------------------

   function Case_Has_Waiting_Statement (N : Node_Id) return Boolean is
      Alt : Node_Id;
   begin
      if Is_Non_Empty_List (Alternatives (N)) then
	 Alt := First (Alternatives (N));
	 while Present (Alt) loop
	    if Has_Waiting_Statement (First (Statements (Alt))) then
	       return True;
	    end if;
	    Next (Alt);
	 end loop;
      end if;

      return False;
   end Case_Has_Waiting_Statement;

   ---------------------------
   -- Has_Waiting_Statement --
   ---------------------------

   function Has_Waiting_Statement (N : Node_Id) return Boolean is

      Node : Node_Id;
      Par  : Node_Id;
   begin
      Node := N;
      while Present (Node) loop
	 case Nkind (Node) is
	    when N_If_Statement =>
	       if If_Has_Waiting_Statement (Node) then
		  return True;
	       end if;

	    when N_Loop_Statement =>
	       if Loop_Has_Waiting_Statement (Node) then
		  return True;
	       end if;

	    when N_Case_Statement =>
	       if Case_Has_Waiting_Statement (Node) then
		  return True;
	       end if;

	    when N_Exit_Statement =>
	       Par := Parent_Loop (Node);
	       if Is_Waiting_Statement (Par) then
		  return True;
	       end if;

	    when N_Reactive_Pause_Statement
	      | N_Reactive_Wait_Statement
	      | N_Reactive_Select_Statement
	      | N_Reactive_Fork_Statement
	      | N_Reactive_Abort_Statement =>
	       return True;

	    when others =>
	      null;
	 end case;

	 Next (Node);
      end loop;

      return False;
   end Has_Waiting_Statement;

   -----------------------------
   -- Pre_Declar_Graph_Vertex --
   -----------------------------

   procedure Pre_Declar_Graph_Vertex
     (G       : Rnode_Id;
      Globals : List_Id;
      Locals  : List_Id) is

      V   : Rnode_Id;
      Alt : Rnode_Id;
   begin
      Put_Line ("Pre_Declar_Graph_Vertex Begin");
      if Is_Empty_List (Vertices_List (G)) then
	 return;
      end if;

      V := First (Vertices_List (G));
      while V /= No_Rnode loop

	 case Rkind (V) is
	    when R_Unused_At_Start =>
	       null;

	    when R_Graph_Vertex =>
	       Pre_Declar_Graph_Vertex (V, Globals, Locals);

	    when R_Contionnal_Graph_Vertex =>
	       Pre_Declar_Graph_Vertex (V, Globals, Locals);

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
	       Pre_Declar_Graph_Vertex (Then_Graph (V), Globals, Locals);

	       if Present (Alternatives (V)) then
		  Alt := First (Alternatives (V));
		  while Alt /= No_Rnode loop
		     Pre_Declar_Graph_Vertex (Alt, Globals, Locals);
		     Next (Alt);
		  end loop;
	       end if;

	       if Else_Graph (V) /= No_Rnode then
		  Pre_Declar_Graph_Vertex (Else_Graph (V), Globals, Locals);
	       end if;

	    when R_Loop_Vertex =>
	       Pre_Declar_Graph_Vertex (Body_Graph (V), Globals, Locals);

	    when R_Exit_Vertex =>
	       null;

	    when R_Fork_Vertex =>
	       Alt := First (Alternatives (V));
	       while Alt /= No_Rnode loop
		  Pre_Declar_Graph_Vertex (Alt, Globals, Locals);
		  Next (Alt);
	       end loop;

	    when R_Abort_Vertex =>
	       Pre_Declar_Graph_Vertex (Body_Graph (V), Globals, Locals);

	       if Handler_Graph (V) /= No_Rnode then
		  Pre_Declar_Graph_Vertex (Handler_Graph (V), Globals, Locals);
	       end if;

	    when R_Pause_Vertex =>
	       if Present (State_Identifier (Item_Node (V))) then
		  Declar_Waiting_Vertex (V, Globals, Locals);
	       end if;

	    when R_Wait_Vertex =>
	       if Present (State_Identifier (Item_Node (V))) then
		  Declar_Waiting_Vertex (V, Globals, Locals);
	       end if;

	    when R_Sync_Vertex =>
	       if Present (State_Identifier (Item_Node (V))) then
		  Declar_Waiting_Vertex (V, Globals, Locals);
	       end if;

	    when R_Select_Vertex =>
	       if Present (State_Identifier (Item_Node (V))) then
		  Declar_Select_Vertex (V, Globals, Locals);
	       end if;

	       Alt := First (Alternatives (V));
	       while Alt /= No_Rnode loop
		  Pre_Declar_Graph_Vertex (Alt, Globals, Locals);
		  Next (Alt);
	       end loop;

	    when R_Unused_At_End =>
	       null;
	 end case;

         Next (V);
      end loop;
      Put_Line ("Pre_Declar_Graph_Vertex End");
   end Pre_Declar_Graph_Vertex;

   -------------------------
   -- Declar_Graph_Vertex --
   -------------------------

   procedure Declar_Graph_Vertex
     (G       : Rnode_Id;
      Globals : List_Id;
      Locals  : List_Id) is

      V   : Rnode_Id;
      Alt : Rnode_Id;
   begin
      Put_Line ("Declar_Graph_Vertex Begin");
      if Is_Empty_List (Vertices_List (G)) then
	 return;
      end if;

      V := First (Vertices_List (G));
      while V /= No_Rnode loop

	 case Rkind (V) is

	    when R_Unused_At_Start =>
	       null;

	    when R_Graph_Vertex =>
	       Declar_Graph_Vertex (V, Globals, Locals);

	    when R_Contionnal_Graph_Vertex =>
	       Declar_Graph_Vertex (V, Globals, Locals);

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
	       Declar_If_Vertex (V, Globals, Locals);

	       --  Declar_Transient_Vertex (V, Locals);
	       --  Declar_Transient_Vertex (End_Vertex (V), Locals);
	       --  Declar_Graph_Vertex (Then_Graph (V), Globals, Locals);

	       --  if Present (Alternatives (V)) then
	       --  	  Alt := First (Alternatives (V));
	       --  	  while Alt /= No_Rnode loop
	       --  	     Declar_Graph_Vertex (Alt, Globals, Locals);
	       --  	     Next (Alt);
	       --  	  end loop;
	       --  end if;

	       --  if Else_Graph (V) /= No_Rnode then
	       --  	  Declar_Graph_Vertex (Else_Graph (V), Globals, Locals);
	       --  end if;

	    when R_Loop_Vertex =>
	       Declar_Loop_Vertex (V, Globals, Locals);

	       --  Declar_Transient_Vertex (V, Locals);
	       --  Declar_Exit_Loop (V, Locals);
	       --  Declar_Repeat_Loop (Repeat_Vertex (V), Locals);
	       --  Declar_Transient_Vertex (End_Vertex (V), Locals);
	       --  Declar_Graph_Vertex (Body_Graph (V), Globals, Locals);

	    when R_Exit_Vertex =>
	       Declar_Exit_Vertex (V, Locals);

	    when R_Fork_Vertex =>
	       Declar_Fork_Vertex (V, Locals);

	       Alt := First (Alternatives (V));
	       while Alt /= No_Rnode loop
		  Declar_Graph_Vertex (Alt, Globals, Locals);
		  Next (Alt);
	       end loop;

            when R_Abort_Vertex | R_Weak_Abort_Vertex =>
               Declar_Abort_Vertex (V, Globals, Locals);

	    when R_Pause_Vertex =>
	       Declar_Waiting_Vertex (V, Globals, Locals);

	    when R_Wait_Vertex =>
	       Declar_Waiting_Vertex (V, Globals, Locals);

	    when R_Sync_Vertex =>
	       Declar_Waiting_Vertex (V, Globals, Locals);

	    when R_Select_Vertex =>
	       Declar_Select_Vertex (V, Globals, Locals);

	       Alt := First (Alternatives (V));
	       while Alt /= No_Rnode loop
		  Declar_Graph_Vertex (Alt, Globals, Locals);
		  Next (Alt);
	       end loop;

	    when R_Unused_At_End =>
	       null;
	 end case;

         Next (V);
      end loop;
      Put_Line ("Declar_Graph_Vertex End");
   end Declar_Graph_Vertex;

   ---------------------------
   -- Declar_Waiting_Vertex --
   ---------------------------

   procedure Declar_Waiting_Vertex
     (V       : Rnode_Id;
      Globals : List_Id;
      Locals  : List_Id) is

      N         : Node_Id := Item_Node (V);
      Sident    : Node_Id := Step_Identifier (N);
      Eident    : Entity_Id;
      Eproc     : Entity_Id  := Current_Reaction_Procedure_Entity (N);
      Ebody     : Entity_Id; --  := Scope (Eproc);
      Loc       : Source_Ptr := Sloc (N);

      Xc_Id      : Entity_Id;
      Xa_Id      : Entity_Id;
      Xd_Id      : Entity_Id;
      Xre_Id     : Entity_Id;
      Xfe_Id     : Entity_Id;
      Xter_Id    : Entity_Id;
      Xinimac_Id : Entity_Id;

      Xc_Name      : Name_Id;
      Xa_Name      : Name_Id;
      Xd_Name      : Name_Id;
      Xre_Name     : Name_Id;
      Xfe_Name     : Name_Id;
      Xter_Name    : Name_Id;
      Xinimac_Name : Name_Id;

      Vxc       : Node_Id;
      Vxa       : Node_Id;
      Vxd       : Node_Id;
      Vxre      : Node_Id;
      Vxfe      : Node_Id;
      Vxter     : Node_Id;
      Vxinimac  : Node_Id;

      Base_Name : Name_Id;
   begin
      Put_Line ("Declar_Waiting_Vertex Begin V => " & Rkind (V)'Img);

      N := Item_Node (V);
      Put_Line (" N Kind => " & Nkind (N)'Img);
      Sident := State_Identifier (N);
      Put_Line (" Sident Kind => " & Nkind (Sident)'Img);

      if Present (Sident) then
	 Put_Line
	   (" Sident Name => " &
	      Get_String (Chars (Defining_Identifier (Sident))));
      end if;

      Eproc := Current_Reaction_Procedure_Entity (N);
      Put_Line (" Eproc Kind   => " & Nkind (Eproc)'Img);
      Put_Line (" Eproc Ekind  => " & Ekind (Eproc)'Img);

      Ebody := Scope (Eproc);

      --  To handle a waiting vertex, we need 4 boolean variables.
      --    - Xc  when True, the step is in activity.
      --    - Xa  when True, the step must be activated.
      --    - Xd  when True, the step must be desactivated.
      --    - Xre when True  is the reasing edge of the activation of Step.
      --    - Xfe when True  is the falling edge of the activation of Step.

      --  Here we created these 4 variables and add Xc, Xre and Xfe to the 
      --  globals declaration of the Reactive.  Xa and Xd are not persistent, 
      --  so they are added to the locals declarations of the procedure 
      --  corresponding to the section React of a reactive.

      if Present (Xc (V)) then
	 return;
      end if;

      --  If a state name is defined, use it to  build the step associated
      --  boolean variables Xc, Xa, Xd, Xre, Xfe.

      if Present (Sident) then
	 --  and then not Is_From_Step (N) then
	 Eident := Defining_Identifier (Sident);
	 Set_Name (V, Chars (Eident));
	 Base_Name := Build_Waiting_State_Names
	   (Prefix         => Get_String (Chars (Eident)),
	    Suffix         => "",
	    Reactive_Scope => Ebody,
	    Reaction_Scope => Eproc,
	    Globals        => Globals,
	    Locals         => Locals);

	 --  if step name is not present, created one Xxx with xx
	 --  a natural
      else
	 if Rkind (V) = R_Wait_Vertex then
	    Base_Name := Build_Waiting_State_Names
	      (Prefix         => "xwait",
	       Suffix         => "",
	       Reactive_Scope => Ebody,
	       Reaction_Scope => Eproc,
	       Globals        => Globals,
	       Locals         => Locals);
	 elsif Rkind (V) = R_Pause_Vertex then
	    Base_Name := Build_Waiting_State_Names
	      (Prefix         => "xpause",
	       Suffix         => "",
	       Reactive_Scope => Ebody,
	       Reaction_Scope => Eproc,
	       Globals        => Globals,
	       Locals         => Locals);

	 elsif Rkind (V) = R_Sync_Vertex then
	    Base_Name := Build_Waiting_State_Names
	      (Prefix         => "xsync",
	       Suffix         => "",
	       Reactive_Scope => Ebody,
	       Reaction_Scope => Eproc,
	       Globals        => Globals,
	       Locals         => Locals);
	 else
	    Base_Name := Build_Waiting_State_Names
	      (Prefix         => "x",
	       Suffix         => "",
	       Reactive_Scope => Ebody,
	       Reaction_Scope => Eproc,
	       Globals        => Globals,
	       Locals         => Locals);
	 end if;
      end if;

      --  Now we can generate the step entities

      --  Activity bit

      Xc_Name := String_Find (Get_String (Base_Name) & "c");
      Xc_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Xc_Name);
      Vxc     := Make_Object_Declaration
	(Loc,
	 Defining_Identifier => Xc_Id,
	 Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      Append (Vxc, Locals);

      --  Activation bit

      Xa_Name := String_Find (Get_String (Base_Name) & "a");
      Xa_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Xa_Name);
      Vxa     := Make_Object_Declaration
	(Loc,
	 Defining_Identifier => Xa_Id,
	 Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      Append (Vxa, Locals);

      --  Desactivation bit

      Xd_Name := String_Find (Get_String (Base_Name) & "d");
      Xd_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Xd_Name);
      Vxd     := Make_Object_Declaration
	(Loc,
	 Defining_Identifier => Xd_Id,
	 Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      Append (Vxd, Globals);

      if Rkind (V) = R_Wait_Vertex then
	 Xter_Name := String_Find (Get_String (Base_Name) & "_terminate");
	 Xter_Id   := Make_Defining_Identifier
	   (Sloc  => Loc, Chars => Xter_Name);
	 Vxter     := Make_Object_Declaration
	   (Loc,
	    Defining_Identifier => Xter_Id,
	    Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
	 Append (Vxter, Locals);

	 Xinimac_Name := String_Find (Get_String (Base_Name) & "_init_macro");
	 Xinimac_Id   := Make_Defining_Identifier
	   (Sloc  => Loc, Chars => Xinimac_Name);
	 Vxinimac     := Make_Object_Declaration
	   (Loc,
	    Defining_Identifier => Xinimac_Id,
	    Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
	 Append (Vxinimac, Locals);
      else
	 Xter_Id    := Empty;
	 Xinimac_Id := Empty;
      end if;

      --  Raising and falling edge bits

      if Generate_Edge_Graph then
	 Xre_Name := String_Find (Get_String (Base_Name) & "re");
	 Xre_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Xre_Name);
	 Vxre     := Make_Object_Declaration
	   (Loc,
	    Defining_Identifier => Xre_Id,
	    Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
	 Append (Vxre, Globals);

	 Xfe_Name := String_Find (Get_String (Base_Name) & "fe");
	 Xfe_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Xfe_Name);
	 Vxfe     := Make_Object_Declaration
	   (Loc,
	    Defining_Identifier => Xfe_Id,
	    Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
	 Append (Vxfe, Globals);

      else
	 Xre_Id := Empty;
	 Xfe_Id := Empty;
      end if;

      Set_Xc  (V, Xc_Id);
      Set_Xa  (V, Xa_Id);
      Set_Xd  (V, Xd_Id);
      Set_Xre (V, Xre_Id);
      Set_Xfe (V, Xfe_Id);
      Set_Xterminate (V, Xter_Id);
      Set_Xinit_Macro (V, Xinimac_Id);

      Put_Line ("Declar_Waiting_Vertex End V => " & Rkind (V)'Img);
   end Declar_Waiting_Vertex;

   --------------------------
   -- Declar_Select_Vertex --
   --------------------------

   procedure Declar_Select_Vertex
     (V       : Rnode_Id;
      Globals : List_Id;
      Locals  : List_Id) is

      N         : Node_Id := Item_Node (V);
      Sident    : Node_Id := Step_Identifier (N);
      Eident    : Entity_Id;
      Eproc     : Entity_Id  := Current_Reaction_Procedure_Entity (N);
      Ebody     : Entity_Id  := Scope (Eproc);
      Loc       : Source_Ptr := Sloc (N);

      Xc_Id     : Entity_Id;
      Xa_Id     : Entity_Id;
      Xd_Id     : Entity_Id;
      Xre_Id    : Entity_Id;
      Xfe_Id    : Entity_Id;

      Name_Select     : Name_Id;
      Name_End_Select : Name_Id;

      Xc_Name   : Name_Id;
      Xa_Name   : Name_Id;
      Xd_Name   : Name_Id;
      Xre_Name  : Name_Id;
      Xfe_Name  : Name_Id;

      Vxc       : Node_Id;
      Vxa       : Node_Id;
      Vxd       : Node_Id;
      Vxre      : Node_Id;
      Vxfe      : Node_Id;

   begin
      Put_Line ("Declar_Select_Vertex Begin V => " & Rkind (V)'Img);
      --  To handle a waiting vertex, we need 4 boolean variables.
      --    - Xc  when True, the step is in activity.
      --    - Xa  when True, the step must be activated.
      --    - Xd  when True, the step must be desactivated.
      --    - Xre when True  is the reasing edge of the activation of Step.
      --    - Xfe when True  is the falling edge of the activation of Step.

      --  Here we created these 4 variables and add Xc, Xre and Xfe to the globals
      --  declaration of the Reactive.  Xa and Xd are not persistent, so they
      --  are added to the locals declarations of the procedure corresponding
      --  to the section React of a reactive.

      if Present (Xc (V)) then
	 return;
      end if;

      --  If a state name is defined, use it to  build the step associated
      --  boolean variables Xc, Xa, Xd, Xre, Xfe.

      if Present (Sident) then
	 --  and then not Is_From_Step (N) then
	 Eident := Defining_Identifier (Sident);
	 Set_Name (V, Chars (Eident));
	 Build_Select_State_Names
	   (Prefix          => Get_String (Chars (Eident)),
	    Reactive_Scope  => Ebody,
	    Reaction_Scope  => Eproc,
	    Globals         => Globals,
	    Locals          => Locals,
	    Name_Select     => Name_Select,
	    Name_End_Select => Name_End_Select);

	 --  if step name is not present, created one Xxx with xx
	 --  a natural
      else
	 Build_Select_State_Names
	   (Prefix          => "xselect",
	    Reactive_Scope  => Ebody,
	    Reaction_Scope  => Eproc,
	    Globals         => Globals,
	    Locals          => Locals,
	    Name_Select     => Name_Select,
	    Name_End_Select => Name_End_Select);
      end if;

      --  Now we can generate the step entities

      --  Activity bit

      Xc_Name := String_Find (Get_String (Name_Select) & "c");
      Xc_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Xc_Name);
      Vxc     := Make_Object_Declaration
	(Loc,
	 Defining_Identifier => Xc_Id,
	 Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      Append (Vxc, Locals);

      --  Activation bit

      Xa_Name := String_Find (Get_String (Name_Select) & "a");
      Xa_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Xa_Name);
      Vxa     := Make_Object_Declaration
	(Loc,
	 Defining_Identifier => Xa_Id,
	 Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      Append (Vxa, Locals);

      --  Desactivation bit

      Xd_Name := String_Find (Get_String (Name_Select) & "d");
      Xd_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Xd_Name);
      Vxd     := Make_Object_Declaration
	(Loc,
	 Defining_Identifier => Xd_Id,
	 Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      Append (Vxd, Globals);

      --  Raising and falling edge bits

      if Generate_Edge_Graph then
	 Xre_Name := String_Find (Get_String (Name_Select) & "re");
	 Xre_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Xre_Name);
	 Vxre     := Make_Object_Declaration
	   (Loc,
	    Defining_Identifier => Xre_Id,
	    Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
	 Append (Vxre, Globals);

	 Xfe_Name := String_Find (Get_String (Name_Select) & "fe");
	 Xfe_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Xfe_Name);
	 Vxfe     := Make_Object_Declaration
	   (Loc,
	    Defining_Identifier => Xfe_Id,
	    Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
	 Append (Vxfe, Globals);

      else
	 Xre_Id := Empty;
	 Xfe_Id := Empty;
      end if;

      Set_Xc  (V, Xc_Id);
      Set_Xa  (V, Xa_Id);
      Set_Xd  (V, Xd_Id);
      Set_Xre (V, Xre_Id);
      Set_Xfe (V, Xfe_Id);

      -- Declar the end vertex entity

      Xc_Name := String_Find (Get_String (Name_End_Select));
      Xc_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Xc_Name);
      Vxc     := Make_Object_Declaration
	(Loc,
	 Defining_Identifier => Xc_Id,
	 Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      Append (Vxc, Locals);

      Set_Xc  (End_Vertex (V), Xc_Id);
      Set_Xa  (End_Vertex (V), Empty);
      Set_Xd  (End_Vertex (V), Empty);
      Set_Xre (End_Vertex (V), Empty);
      Set_Xfe (End_Vertex (V), Empty);

      Put_Line ("Declar_Select_Vertex End V => " & Rkind (V)'Img);
   end Declar_Select_Vertex;

   -----------------------------
   -- Declar_Transient_Vertex --
   -----------------------------

   procedure Declar_Transient_Vertex
     (V      : Rnode_Id;
      Locals : List_Id) is

      N         : Node_Id := Item_Node (V);
      Eproc     : Entity_Id := Current_Reaction_Procedure_Entity (N);
      Loc       : Source_Ptr :=  Sloc (N);
      Xc_Id     : Entity_Id;
      Vxc       : Node_Id;
      Xc_Name   : Name_Id;
      Base_Name : Name_Id;
   begin
      Put_Line ("Declar_Transient_Vertex Begin V => " & Rkind (V)'Img);

      Base_Name := Build_Transient_State_Names
	(Prefix         => "X",
	 Suffix         => "",
	 Reaction_Scope => Eproc,
	 Locals         => Locals);

      --  Activity bit

      Xc_Name := String_Find (Get_String (Base_Name) & "c");
      Xc_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Xc_Name);
      Vxc     := Make_Object_Declaration
	(Loc,
	 Defining_Identifier => Xc_Id,
	 Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      Append (Vxc, Locals);

     Set_Xc  (V, Xc_Id);
     Set_Xa  (V, Empty);
     Set_Xd  (V, Empty);
     Set_Xre (V, Empty);
     Set_Xfe (V, Empty);

     Put_Line ("Declar_Transient_Vertex End V => " & Rkind (V)'Img);
   end Declar_Transient_Vertex;

   ------------------------
   -- Declar_Fork_Vertex --
   ------------------------

   procedure Declar_Fork_Vertex
     (V       : Rnode_Id;
      Locals  : List_Id) is

      N             : Node_Id := Item_Node (V);
      Eproc         : Entity_Id := Current_Reaction_Procedure_Entity (N);
      Loc           : Source_Ptr :=  Sloc (N);
      Xc_Id         : Entity_Id;
      Vxc           : Node_Id;
      Name_Fork     : Name_Id;
      Name_End_Fork : Name_Id;
   begin
      Put_Line ("Declar_Fork_Vertex Begin V => " & Rkind (V)'Img);

     --  Declar fork vertex

      Build_Fork_State_Names
	(Reaction_Scope => Eproc,
	 Locals         => Locals,
	 Name_Fork      => Name_Fork,
	 Name_End_Fork  => Name_End_Fork);

      Xc_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Name_Fork);
      Vxc     := Make_Object_Declaration
	(Loc,
	 Defining_Identifier => Xc_Id,
	 Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      Append (Vxc, Locals);

     Set_Xc (V, Xc_Id);

     --  Declar end fork vertex

      Xc_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Name_End_Fork);
      Vxc     := Make_Object_Declaration
	(Loc,
	 Defining_Identifier => Xc_Id,
	 Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      Append (Vxc, Locals);

     Set_Xc (End_Vertex (V), Xc_Id);

     Put_Line ("Declar_Fork_Vertex End V => " & Rkind (V)'Img);
   end Declar_Fork_Vertex;

   ------------------------
   -- Declar_Exit_Vertex --
   ------------------------

   procedure Declar_Exit_Vertex
     (V      : Rnode_Id;
      Locals : List_Id) is

      N         : Node_Id := Item_Node (V);
      Eproc     : Entity_Id := Current_Reaction_Procedure_Entity (N);
      Loc       : Source_Ptr :=  Sloc (N);
      Xc_Id     : Entity_Id;
      Vxc       : Node_Id;
      Xc_Name   : Name_Id;
      Base_Name : Name_Id;
   begin
      Put_Line ("Declar_Exit_Vertex Begin V => " & Rkind (V)'Img);

      Base_Name := Build_Transient_State_Names
	(Prefix         => "xexit",
	 Suffix         => "",
	 Reaction_Scope => Eproc,
	 Locals         => Locals);

      --  Activity bit

      Xc_Name := String_Find (Get_String (Base_Name) & "c");
      Xc_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Xc_Name);
      Vxc     := Make_Object_Declaration
	(Loc,
	 Defining_Identifier => Xc_Id,
	 Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      Append (Vxc, Locals);

     Set_Xc (V, Xc_Id);

     Put_Line ("Declar_Exit_Vertex End V => " & Rkind (V)'Img);
   end Declar_Exit_Vertex;

   ----------------------
   -- Declar_If_Vertex --
   ----------------------

   procedure Declar_If_Vertex
     (V       : Rnode_Id;
      Globals : List_Id;
      Locals  : List_Id) is

      N           : Node_Id := Item_Node (V);
      Eproc       : Entity_Id := Current_Reaction_Procedure_Entity (N);
      Loc         : Source_Ptr :=  Sloc (N);
      Xc_Id       : Entity_Id;
      Vxc         : Node_Id;
      Name_If     : Name_Id;
      Name_End_If : Name_Id;
      Alt         : Rnode_Id;
   begin
      Put_Line ("Declar_If_Vertex Begin V => " & Rkind (V)'Img);

     --  Declar if vertex

      Build_If_State_Names
	(Reaction_Scope => Eproc,
	 Locals         => Locals,
	 Name_If        => Name_If,
	 Name_End_If    => Name_End_If);

      Xc_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Name_If);
      Vxc     := Make_Object_Declaration
	(Loc,
	 Defining_Identifier => Xc_Id,
	 Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      Append (Vxc, Locals);

     Set_Xc (V, Xc_Id);

     --  Declar end if vertex

      Xc_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Name_End_If);
      Vxc     := Make_Object_Declaration
	(Loc,
	 Defining_Identifier => Xc_Id,
	 Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      Append (Vxc, Locals);

     Set_Xc (End_Vertex (V), Xc_Id);

     --  Declar the entites for the then part

     Declar_Graph_Vertex (Then_Graph (V), Globals, Locals);

     --  Declar the entites for the elsif parts if any

     if Present (Alternatives (V)) then
	Alt := First (Alternatives (V));
	while Alt /= No_Rnode loop
	   Declar_Graph_Vertex (Alt, Globals, Locals);
	   Next (Alt);
	end loop;
     end if;

     --  Declar the entites for the else part

     if Else_Graph (V) /= No_Rnode then
	Declar_Graph_Vertex (Else_Graph (V), Globals, Locals);
     end if;

     Put_Line ("Declar_If_Vertex End V => " & Rkind (V)'Img);
   end Declar_If_Vertex;

   -------------------------
   -- Declar_Abort_Vertex --
   -------------------------

   procedure Declar_Abort_Vertex
     (V       : Rnode_Id;
      Globals : List_Id;
      Locals  : List_Id)
   is
      N              : Node_Id    := Item_Node (V);
      Eproc          : Entity_Id  := Current_Reaction_Procedure_Entity (N);
      Loc            : Source_Ptr :=  Sloc (N);
      Xc_Id          : Entity_Id;
      Vxc            : Node_Id;
      Name_Abort     : Name_Id;
      Name_End_Abort : Name_Id;
   begin
      Put_Line ("Declar_Abort_Vertex Begin V => " & Rkind (V)'Img);

     --  Declar abort vertex
      Build_Abort_State_Names
        (Reaction_Scope => Eproc,
         Locals         => Locals,
         Name_Abort     => Name_Abort,
         Name_End_Abort => Name_End_Abort);

      Xc_Id := Make_Defining_Identifier
        (Sloc  => Loc,
         Chars => Name_Abort);
      Vxc   := Make_Object_Declaration
	(Sloc                => Loc,
	 Defining_Identifier => Xc_Id,
	 Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      Append (Vxc, Locals);
      Set_Xc (V, Xc_Id);

     --  Declar end abort vertex
      Xc_Id := Make_Defining_Identifier
        (Sloc  => Loc,
         Chars => Name_End_Abort);
      Vxc   := Make_Object_Declaration
	(Sloc                => Loc,
	 Defining_Identifier => Xc_Id,
	 Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      Append (Vxc, Locals);
      Set_Xc (End_Vertex (V), Xc_Id);

     --  Declar the entites for the body part
     Declar_Graph_Vertex (Body_Graph (V), Globals, Locals);

      --  Declar the entites for the hanler part if any
      if Present (Handler_Graph (V)) then
         Declar_Graph_Vertex (Handler_Graph (V), Globals, Locals);
      end if;

      Put_Line ("Declar_Abort_Vertex End V => " & Rkind (V)'Img);
   end Declar_Abort_Vertex;


   ------------------------
   -- Declar_Case_Vertex --
   ------------------------

   procedure Declar_Case_Vertex
     (V       : Rnode_Id;
      Globals : List_Id;
      Locals  : List_Id) is

      N             : Node_Id := Item_Node (V);
      Eproc         : Entity_Id := Current_Reaction_Procedure_Entity (N);
      Loc           : Source_Ptr :=  Sloc (N);
      Xc_Id         : Entity_Id;
      Vxc           : Node_Id;
      Name_Case     : Name_Id;
      Name_End_Case : Name_Id;
   begin
      Put_Line ("Declar_Case_Vertex Begin V => " & Rkind (V)'Img);

     --  Declar if vertex

      Build_Case_State_Names
	(Reaction_Scope => Eproc,
	 Locals         => Locals,
	 Name_Case      => Name_Case,
	 Name_End_Case  => Name_End_Case);

      Xc_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Name_Case);
      Vxc     := Make_Object_Declaration
	(Loc,
	 Defining_Identifier => Xc_Id,
	 Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      Append (Vxc, Locals);

     Set_Xc (V, Xc_Id);

     --  Declar end if vertex

      Xc_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Name_End_Case);
      Vxc     := Make_Object_Declaration
	(Loc,
	 Defining_Identifier => Xc_Id,
	 Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      Append (Vxc, Locals);

     Set_Xc (End_Vertex (V), Xc_Id);

     --  Declar the entites for the then part

     --   Declar_Graph_Vertex (Then_Graph (V), Globals, Locals);

     --  Declar the entites for the elsif parts if any

     --  if Present (Alternatives (V)) then
     --  	Alt := First (Alternatives (V));
     --  	while Alt /= No_Rnode loop
     --  	   Declar_Graph_Vertex (Alt, Globals, Locals);
     --  	   Next (Alt);
     --  	end loop;
     --  end if;

     --  --  Declar the entites for the else part

     --  if Else_Graph (V) /= No_Rnode then
     --  	Declar_Graph_Vertex (Else_Graph (V), Globals, Locals);
     --  end if;

     Put_Line ("Declar_Case_Vertex End V => " & Rkind (V)'Img);
   end Declar_Case_Vertex;

   ------------------------
   -- Declar_Loop_Vertex --
   ------------------------

   procedure Declar_Loop_Vertex
     (V       : Rnode_Id;
      Globals : List_Id;
      Locals  : List_Id) is

      N             : Node_Id := Item_Node (V);
      Eproc         : Entity_Id := Current_Reaction_Procedure_Entity (N);
      Loc           : Source_Ptr :=  Sloc (N);
      Xc_Id         : Entity_Id;
      Vxc           : Node_Id;
      Name_Loop     : Name_Id;
      Name_Exit     : Name_Id;
      Name_Repeat   : Name_Id;
      Name_End_Loop : Name_Id;
   begin
      Put_Line ("Declar_Loop_Vertex Begin V => " & Rkind (V)'Img);

     --  Declar loop vertex

      Build_Loop_State_Names
	(Reaction_Scope => Eproc,
	 Locals         => Locals,
	 Name_Loop      => Name_Loop,
	 Name_Exit      => Name_Exit,
	 Name_Repeat    => Name_Repeat,
	 Name_End_Loop  => Name_End_Loop);

      Xc_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Name_Loop);
      Vxc     := Make_Object_Declaration
	(Loc,
	 Defining_Identifier => Xc_Id,
	 Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      Append (Vxc, Locals);

     Set_Xc (V, Xc_Id);

     --  Declar repeat loop vertex

      Xc_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Name_Repeat);
      Vxc     := Make_Object_Declaration
	(Loc,
	 Defining_Identifier => Xc_Id,
	 Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      Append (Vxc, Locals);

     Set_Xc (Repeat_Vertex (V), Xc_Id);

     --  Declar end loop vertex

      Xc_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Name_End_Loop);
      Vxc     := Make_Object_Declaration
	(Loc,
	 Defining_Identifier => Xc_Id,
	 Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      Append (Vxc, Locals);

     Set_Xc (End_Vertex (V), Xc_Id);

     --  Declar exit loop vertex

      Xc_Id   := Make_Defining_Identifier (Sloc  => Loc, Chars => Name_Exit);
      Vxc     := Make_Object_Declaration
	(Loc,
	 Defining_Identifier => Xc_Id,
	 Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      Append (Vxc, Locals);

     Set_Exit_Loop (V, Xc_Id);

     --  Declar the entites for the then part

     Declar_Graph_Vertex (Body_Graph (V), Globals, Locals);

     Put_Line ("Declar_Loop_Vertex End V => " & Rkind (V)'Img);
   end Declar_Loop_Vertex;

   ----------------------------
   -- Create_Initialize_Code --
   ----------------------------

   procedure Create_Initialize_Code (Gn : Rnode_Id) is
      Vbegin     : Rnode_Id := Begin_Vertex (Gn);
      Vfirst     : Rnode_Id := First (Vertices_List (Gn));
      Then_Stmts : List_Id;
      If_Stmt    : Node_Id;
      N          : Node_Id := Item_Node (Gn);
      Loc        : Source_Ptr := Sloc (N);
      Xinit      : Entity_Id;
      Vxinit     : Node_Id;
      Ebody      : Entity_Id := Current_Reactive_Body_Entity (N);

      NL          : List_Id := New_List;
   begin
      Put_Line ("Create_Initialize_Code Begin");

      Xinit := Make_Defining_Identifier (Loc, New_State_Name ("xinit", "", Ebody));
      Set_Xinit (Gn, Xinit);

      VXinit := Make_Object_Declaration
	(Loc,
	 Defining_Identifier => Xinit,
	 Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));

      Set_Has_Init_Expression (Vxinit, False);
--        Set_Expression (Vxinit, New_Occurrence_Of (Standard_True, Sloc (N)));
      Append (Vxinit, Global_States_Declarations (Gn));

      --  init Xinit := True;
      Append
        (Make_Assignment_Statement
                (Sloc       => Loc,
                 Name       => New_Occurrence_Of (Xinit, Loc),
                 Expression => New_Occurrence_Of (Standard_True, Sloc (N))),
         NL);

      --  create xinit code
      Then_Stmts := New_List;
      if Is_Non_Empty_List (Enter_Code (Gn)) then
	 Append_List (Enter_Code (Gn), Then_Stmts);
      end if;

      Append
	(Make_Assignment_Statement
	   (Loc,
	    Name       => New_Occurrence_Of (Xc (Vfirst), Loc),
	    Expression => New_Occurrence_Of (Standard_True, Loc)),
	Then_Stmts);
      Append
	(Make_Assignment_Statement
	   (Loc,
	    Name       => New_Occurrence_Of (Xinit, Loc),
	    Expression => New_Occurrence_Of (Standard_False, Loc)),
	Then_Stmts);

      if Is_Non_Empty_List (Exit_Code (Gn)) then
	 Append_List (Exit_Code (Gn), Then_Stmts);
      end if;

      If_Stmt := Make_If_Statement
	(Loc,
	 Condition       => New_Occurrence_Of (Xinit, Loc),
	 Then_Statements => Then_Stmts);

      Append (If_Stmt, NL);
      Set_Initialize_Code (Gn, NL);

--        Set_Initialize_Code (Gn, New_List (If_Stmt));

      Put_Line ("Create_Initialize_Code End");
   end Create_Initialize_Code;


   -----------------------
   -- Make_Graph_Vertex --
   -----------------------

   function Make_Graph_Vertex
     (N : Node_Id;
      L : List_Id;
      K : Rnode_Kind := R_Graph_Vertex) return Rnode_Id is

      Gn : Rnode_Id;
   begin
      Put_Line ("Make_Graph_Vertex Begin => " & Nkind (N)'Img);

      --  expand while or for loop statements in simple loop
      Expand_Loop_Statements (N);

      Gn := New_Node (K);

      if Present (N) then
	 New_Nodes_Association (N, Gn);
      end if;

      --  Create Begin and End Vertex of the graph

      Set_Begin_Vertex (Gn, New_Node (R_Begin_Vertex));
      Set_End_Vertex (Gn, New_Node (R_End_Vertex));

      -- Now create the vertices corresponding to the statements in
      -- the list L.

      if Present (L) and then Is_Non_Empty_List (L) then
	 Set_Vertices_List (Gn, Analyze_Statements (Gn, L));
      end if;

      Put_Line ("Make_Graph_Vertex End => " & Nkind (N)'Img );
      Put_Line ("Make_Graph_Vertex End Gn => " & Gn'Img );
      return Gn;
   end Make_Graph_Vertex;

   ------------------------
   -- Analyze_Statements --
   ------------------------

   function Analyze_Statements
     (Gn : Rnode_Id;
      L  : List_Id) return RList_Id is

      RL   : Rlist_Id;
      Vx   : Rnode_Id := No_Rnode;
      Pred : Rnode_Id;
      Code : List_Id := No_List;
      S    : Node_Id;

      procedure Append_Trans_Code (S : Node_Id);
      -- Append a copy of the statement S to the list of non reactive
      -- statements executed when the current actif step is
      -- desactivated (Exit_Code).

      procedure Append_Pred_Exit_Code;
      -- helper function for attached the Exit_Code to the preceding vertex
      -- and the current vertex Vx is now the preceding vertex. The Exit_Code
      -- is reset to accumulate the statements to be excetuted by the current
      -- step which become preceding step.

      -----------------------
      -- Append_Trans_Code --
      -----------------------

      procedure Append_Trans_Code (S : Node_Id) is
         New_S : Node_id := New_Copy_Tree (S);
      begin
	 if No (Code) then
	    Code := New_List;
	 end if;
	 Nlists.Append_To (Code, New_S);
      end Append_Trans_Code;

      ---------------------------
      -- Append_Pred_Exit_Code --
      ---------------------------

      procedure Append_Pred_Exit_Code is
      begin
	 --  if Present (Pred) then

	    -- The exit is rattached to the end vertex of a composed vertex
	    -- and is attached to the vertex for non composed vertex.

	    --  if Rkind (Pred) in Composed_Vertex then
	    --     Append_Exit_Code (End_Vertex (Pred), Code);
	    --  else
	    if Pred = No_Rnode then
	       Append_Enter_Code (Gn, Code);
	    else
	       Append_Exit_Code (Pred, Code);
	    end if;
	    --  end if;

	    Code := No_List;
	    Pred := Vx;
	 --  end if;

	 Append_To (RL, Vx);
      end Append_Pred_Exit_Code;

   begin
      RL := New_List;
      Pred := No_Rnode;   --  Begin_Vertex (Gn);

      S := First (L);
      while Present (S) loop
	 Vx := No_Rnode;
	 case Nkind (S) is
	    when N_Reactive_Connection         =>
	       null;

	       
	    when N_Reactive_Pause_Statement    =>
	       Vx := Make_Pause_Vertex (S);
	       Append_Pred_Exit_Code;

	    when N_Reactive_Wait_Statement     =>
	       Vx := Make_Wait_Vertex (S);
	       Append_Pred_Exit_Code;

	    when N_Reactive_Select_Statement   =>
	       Vx := Make_Select_Vertex (S);
	       Append_Pred_Exit_Code;

	    when N_Reactive_Fork_Statement     =>
	       Vx := Make_Fork_Vertex (S);
	       Append_Pred_Exit_Code;

	    when N_Reactive_Abort_Statement    =>
	       Append_To (RL, Make_Abort_Vertex (S));

	    when N_Reactive_Step_Definition    =>
	       null; --  Append_To (RL, Make_Step_Vertex (S));

	    when N_If_Statement =>
	       if If_Has_Waiting_Statement (S) then
		  Vx := Make_If_Vertex (S);
		  Append_Pred_Exit_Code;
	       else
		  Append_Trans_Code (S);
	       end if;

	    when N_Loop_Statement =>
	       if Loop_Has_Waiting_Statement (S) then
		  Vx := Make_Loop_Vertex (S);
		  Append_Pred_Exit_Code;
	       else
		  Append_Trans_Code (S);
	       end if;

	    when N_Exit_Statement =>
	       Vx := Make_Exit_Vertex (S);
	       Append_Pred_Exit_Code;

	    when others =>
	       Append_Trans_Code (S);
	 end case;

         Next (S);
      end loop;

      if Present (Vx) then
	 --  if Rkind (Vx) in Composed_Vertex then
	 --     Append_Exit_Code (End_Vertex (Vx), Code);
	 --  else
	 Append_Exit_Code (Vx, Code);
	 --  end if;
      else
	 Append_Exit_Code (Gn, Code);
      end if;

      Put_Line ("Analyze_Statements End");
      return Rl;
   end Analyze_Statements;

   ------------------------
   --  Make_Pause_Vertex --
   ------------------------

   function Make_Pause_Vertex (N : Node_Id) return Rnode_Id is
      Rn : Rnode_Id;
   begin
      Rn := New_Node (R_Pause_Vertex);
      New_Nodes_Association (N, Rn);

      return Rn;
   end Make_Pause_Vertex;

   -----------------------
   --  Make_Wait_Vertex --
   -----------------------

   function Make_Wait_Vertex (N : Node_Id) return Rnode_Id is
      Rn : Rnode_Id;
   begin
      Put_Line ("Make_Wait_Vertex Begin");
      --  Create the associated Rnode

      Rn := New_Node (R_Wait_Vertex);
      New_Nodes_Association (N, Rn);

      Set_Condition
	(Rn,
	 New_Copy_Tree (Condition (N)));

      Put_Line ("Make_Wait_Vertex End Rn => " & Rn'Img);
      return Rn;
   end Make_Wait_Vertex;

   ------------------------
   -- Make_Select_Vertex --
   ------------------------

   function Make_Select_Vertex (N : Node_Id) return Rnode_Id is

      Rn   : Rnode_Id;
      Vend : Rnode_Id;
      Alt  : Node_Id;

      function Make_Select_Alternative (Anode : Node_Id) return Rnode_Id;
      --  Create the graph for a select alternative. Create a R_Conditional_Graph

      -----------------------------
      -- Make_Select_Alternative --
      -----------------------------

      function Make_Select_Alternative (Anode : Node_Id) return Rnode_id is
	 Ag : Rnode_Id;
      begin
	 Ag := Make_Graph_Vertex
	   (Anode, Statements (Anode), R_Contionnal_Graph_Vertex);

	 Set_Condition
	   (Ag,
	    New_Copy_Tree (Condition (Anode)));
	 return Ag;
      end Make_Select_Alternative;

   begin
      Rn := New_Node (R_Select_Vertex);
      New_Nodes_Association (N, Rn);

      -- Create the end select vertex.

      Vend := New_Node (R_End_Vertex);
      Set_Item_Node (Vend, N);
      Set_End_Vertex (Rn, Vend);

      -- Build graf of all alternatives.

      if Present (Alternatives (N)) then
	 Set_Alternatives (Rn, New_List);

         Alt := First (Alternatives (N));
         while Present (Alt) loop
            Append_To
	      (Alternatives (Rn),
	       Make_Select_Alternative (Alt));

            Next (Alt);
         end loop;
      end if;

      return Rn;
   end Make_Select_Vertex;

   ----------------------
   -- Make_Fork_Vertex --
   ----------------------

   function Make_Fork_Vertex (N : Node_Id) return Rnode_Id is

      Rn   : Rnode_Id;
      Vend : Rnode_Id;
      Alt  : Node_Id;

      function Make_Fork_Alternative (Anode : Node_Id) return Rnode_Id;
      --  This is applied to either the N_If_Statement node itself or
      --  to an N_Elsif_Part node. It deals with analyzing the condition
      --  and the THEN statements associated with it.

      ---------------------------
      -- Make_Fork_Alternative --
      ---------------------------

      function Make_Fork_Alternative (Anode : Node_Id) return Rnode_id is
	 Ag   : Rnode_Id;
      begin
	 Ag := Make_Graph_Vertex
	   (Anode, Statements (Anode), R_Graph_Vertex);

	 return Ag;
      end Make_Fork_Alternative;

   begin
      Rn := New_Node (R_Fork_Vertex);
      New_Nodes_Association (N, Rn);

      -- Create the end select vertex.

      Vend := New_Node (R_End_Vertex);
      Set_Item_Node (Vend, N);
      Set_End_Vertex (Rn, Vend);

      Set_Condition
	(Rn,
	 New_Copy_Tree (Condition (N)));

      -- Build graf of all alternatives.

      if Present (Alternatives (N)) then
	 Set_Alternatives (Rn, New_List);

	 Alt := First (Alternatives (N));
	 while Present (Alt) loop
	    Append_To
	      (Alternatives (Rn),
	       Make_Fork_Alternative (Alt));
	    Next (Alt);
	 end loop;
      end if;

      return Rn;
   end Make_Fork_Vertex;

   -----------------------
   -- Make_Abort_Vertex --
   -----------------------

   function Make_Abort_Vertex (N : Node_Id) return Rnode_Id
   is
      Rn      : Rnode_Id;
      Vend    : Rnode_Id;
      Handler : Node_Id;
   begin
      Rn := New_Node (R_Abort_Vertex);
      New_Nodes_Association (N, Rn);

      -- Create the end select vertex.
      Vend := New_Transient_Vertex (Node);
      Set_Item_Node (Vend, N);
      Set_End_Vertex (Rn, Vend);

      --  The abort condition
      Set_Condition
        (R => Rn,
         C => New_Copy_Tree (Condition (N)));

      -- Body of abort
      Set_Body_Graph
        (R => Rn,
         G => Make_Graph_Vertex (Empty, Statements (N)));

      -- Analyze Handler
      if Is_Non_Empty_List (Abort_Handlers (N)) then
	 -- Create The Handler Abort State.
	 Handler := First (Abort_Handlers (N));
         Set_Handler_Graph
           (R => Rn,
            G => Make_Graph_Vertex (Handler, Statements (Handler)));
      else
	 Set_Handler_Graph (Rn, No_Rnode);
      end if;

      return Rn;
   end Make_Abort_Vertex;

   --------------------
   -- Make_If_Vertex --
   --------------------

   function Make_If_Vertex (N : Node_Id) return Rnode_Id is

      Rn   : Rnode_Id;
      Vend : Rnode_Id;
      E    : Node_Id;

      function Make_If_Alternative (Anode : Node_Id) return Rnode_Id;
      --  This is applied to either the N_If_Statement node itself or
      --  to an N_Elsif_Part node. It deals with analyzing the condition
      --  and the THEN statements associated with it.

      -------------------------
      -- Make_If_Alternative --
      -------------------------

      function Make_If_Alternative (Anode : Node_Id) return Rnode_id is
	 Ag : Rnode_Id;
      begin
	 Ag := Make_Graph_Vertex
	   (Anode, Then_Statements (Anode), R_Contionnal_Graph_Vertex);

	 Set_Condition
	   (Ag,
	    New_Copy_Tree (Condition (Anode)));
	 return Ag;
      end Make_If_Alternative;

   begin
      Vif := New_If_Vertex (N);

      -- Create the end select vertex.

      Vend := New_Transient_Vertex (Node);
      Set_End_Vertex (Vif, Vend);
      
      Set_Condition
	(Vif,
	 New_Copy_Tree (Condition (N)));

      Set_Then_Graph
	(Vif, Make_Graph_Vertex (Empty, Then_Statements (N)));

      -- Build graf of all alternatives.

      if Present (Elsif_Parts (N)) then
	 Set_Alternatives (Rn, New_List);

         E := First (Elsif_Parts (N));
         while Present (E) loop
            Append_To
	      (Alternatives (Rn),
	       Make_If_Alternative (E));
            Next (E);
         end loop;
      end if;

      if Present (Else_Statements (N)) then
	 Set_Else_Graph
	   (Vif, Make_Graph_Vertex (Empty, Else_Statements (N)));
      end if;

      return Vif;
   end Make_If_Vertex;

   -------------------------
   --  Expand_While2Loop  --
   -------------------------

   procedure Expand_While2Loop (N : Node_Id) is
      pragma Assert
        (Nkind (N) = N_Loop_Statement and then Present (Iteration_Scheme (N)));
      Loop_Stmt : Node_Id;
      Stmts     : List_Id := No_List;
   begin
      Put_Line ("Expand_While2Loop started");
      --  while Cond loop
      --     <<loop statements>>
      --  end loop;
      --  is transformed in
      --  loop
      --      exit when Cond;
      --      <<loop statements>>
      --  end loop;

      Loop_Stmt := Make_Loop_Statement
        (Sloc        => Sloc (N),
         Statements  => No_List,
         End_Label   => Empty);

      Stmts := New_List;
      Append
        (Make_Exit_Statement
              (Sloc      => Sloc (N),
               Condition => New_Copy_Tree (Condition (Iteration_Scheme (N)))),
         Stmts);

      Append_List_To (To   => Stmts,
                      List => New_Copy_List_Tree (Statements (N)));

      Set_Statements (Loop_Stmt, Stmts);

      Replace (Old_Node => N,
               New_Node => Loop_Stmt);

--        Analyze (N);

      Put_Line ("Expand_While2Loop ended");
   end Expand_While2Loop;

   --------------------------
   --  Expand_For2Loop  --
   --------------------------

   procedure Expand_For2Loop (N : Node_Id)
   is
      pragma Assert
        (Nkind (N) = N_Loop_Statement and then Present (Iteration_Scheme (N)));

      ItSpec       : Node_Id;
      ParamSpec    : Node_Id;
      Def_Ident    : Node_Id;

      ForCount     : Node_Id;
      InitExpr     : Node_Id;
      IncrStmt     : Node_Id;

      LoopStmts    : List_Id;
      LoopStmt     : Node_Id;

      --------------------------------------------
      --  Create_And_Add_For_Count_Declaration  --
      --------------------------------------------

      function Create_And_Add_For_Count_Declaration return Node_Id is
         ForCountName : Name_Id;
         ForCountDecl : Node_Id;
         SubTypIndic  : Node_Id;
         SubTypMark   : Node_Id;

         S            : Transform_Scope_Id := Transformer.Scopes.Top_Transform_Scope;
         ReactionBody : Node_Id :=
           Sinfo.Transform.Transformed_Node (Transformer.Scopes.Scope_Entity (S));
      begin
         Put_Line ("Create_And_Add_For_Count_Declaration started ");
         Put_Line ("       Transformation scope = " & S'Img);
         Put_Line ("       Transformation scope node = "
                   & ReactionBody'Img & " - "
                   & " - " & Nkind (ReactionBody)'Img);

         ForCountName := Build_For_Loop_Count_Name
           (Name           => Chars (Def_Ident),
            Reactive_Scope => Current_Reactive_Body_Entity (N),
            Reaction_Scope => Current_Reaction_Procedure_Entity (N),
            Globals        => Globals,
            Locals         => Locals);
         Put_Line (" ===> 1. ForCountName : " & Get_String (ForCountName));

         SubTypIndic := Discrete_Subtype_Definition (ParamSpec);
         SubTypMark  := Subtype_Mark (SubTypIndic);
         Put_Line (" ===> 2. Subtype_Mark : " & Nkind (SubTypMark)'Img & " - "
                   & Get_String (Chars (SubTypMark)));

         --  ForCount object declaration and add to reactive globals
         ForCountDecl := Make_Object_Declaration
           (Sloc                 => Sloc (N),
            Defining_Identifier  => Make_Defining_Identifier
              (Sloc              => Sloc (N),
               Chars             => ForCountName),
            Object_Definition    =>
              New_Occurrence_Of (Entity (SubTypMark), Sloc (N)));

         Transformer.Scopes.Append_Transform_Declaration
           (Tscp => Transformer.Scopes.Top_Transform_Scope,
            Decl => ForCountDecl);

         Put_Line ("Create_And_Add_For_Count_Declaration ended ");

         return ForCountDecl;
      end Create_And_Add_For_Count_Declaration;

      ----------------------------------------------------
      --  Initialize_For_Count_Variable_With_Low_Bound  --
      ----------------------------------------------------

      function Initialize_For_Count_Variable_With_Low_Bound return Node_Id
      is
         ForConstr   : Node_Id;
         SubTypIndic : Node_Id;
         SubTypMark  : Node_Id;

         RangeExpr   : Node_Id;
         LowBound    : Node_Id;

         InitAssign  : Node_Id;
      begin
         Put_Line ("Initialize_For_Count_Variable_With_Low_Bound started");
         SubTypIndic := Discrete_Subtype_Definition (ParamSpec);
         SubTypMark  := Subtype_Mark (SubTypIndic);
         ForConstr   := Constraint (SubTypIndic);

         Put_Line (" ===> 1. For constraint : " & Nkind (ForConstr)'Img);
         pragma Assert
           (Present (ForConstr)
            and then Nkind (ForConstr) = N_Range_Constraint);
         RangeExpr := Range_Expression (ForConstr);

         Put_Line (" ===> 2. Range expression : " & Nkind (RangeExpr)'Img);
         pragma Assert
           (Present (RangeExpr) and then Nkind (RangeExpr) = N_Range);
         LowBound := Low_Bound (RangeExpr);
         Put_Line ("Range : LB = " & Nkind (LowBound)'Img );

         Put_Line (" ===> 3. Create ForCount init statement : "
                   & "ForCount := LowBound; ");
         InitAssign := Make_Assignment_Statement
           (Sloc       => Sloc (N),
            Name       => New_Occurrence_Of
              (Def_Id => Defining_Identifier (ForCount),
               Loc    => Sloc (N)),
            Expression => New_Copy_Tree (LowBound));
         Put_Line ("Initialize_For_Count_Variable_With_Low_Bound ended");
         return InitAssign;
      end Initialize_For_Count_Variable_With_Low_Bound;

      ------------------------------------------
      --  Create_And_Add_Exit_Loop_Statement  --
      ------------------------------------------

      procedure Create_And_Add_Exit_Loop_Statement (Stmts : in out List_Id) is
         ExitCond     : Node_Id;
         ExitStmt     : Node_Id;

         SubTypIndic : Node_Id;
         SubTypMark  : Node_Id;

         ForConstr    : Node_Id;
         RangeExpr    : Node_Id;
         HighBound    : Node_Id;

      begin
         Put_Line ("Create_And_Add_Exit_Loop_Statement started");

         SubTypIndic := Discrete_Subtype_Definition (ParamSpec);
         SubTypMark  := Subtype_Mark (SubTypIndic);
         ForConstr   := Constraint (SubTypIndic);
         RangeExpr := Range_Expression (ForConstr);
         pragma Assert
           (Present (RangeExpr)
            and then Nkind (RangeExpr) = N_Range);
         HighBound := High_Bound (RangeExpr);
         Put_Line ("Range : HB = " & Nkind (HighBound)'Img);

         if Reverse_Present (ParamSpec) then
            ExitCond := Make_Op_Le
              (Sloc       => Sloc (N),
               Left_Opnd  =>
                 New_Occurrence_Of (Defining_Identifier (ForCount), Sloc (N)),
               Right_Opnd => New_Copy_Tree (HighBound));
         else
            ExitCond := Make_Op_Ge
              (Sloc       => Sloc (N),
               Left_Opnd  =>
                 New_Occurrence_Of (Defining_Identifier (ForCount), Sloc (N)),
               Right_Opnd => New_Copy_Tree (HighBound));
         end if;

         --  exit statement
         ExitStmt := Make_Exit_Statement (Sloc (N), ExitCond);
         Append_To (To   => LoopStmts,
                    Node => ExitStmt);

         Put_Line ("Create_And_Add_Exit_Loop_Statement ended");
      end Create_And_Add_Exit_Loop_Statement;

   begin
      Put_Line ("Expand_For2Loop started for " & Nkind (N)'Img);

      --  For I in Integer range Low..High loop
      --     <<loop statements>>
      --  end loop;
      --  is transformed in
      --  declare
      --        I : Integer;
      --  begin
      --    I := Low;
      --    loop
      --      exit when I <= High;
      --      <<loop statements>>
      --      I := I + 1;
      --    end loop;

      LoopStmts := New_List;

      ItSpec    := Iterator_Specification (Iteration_Scheme (N));
      ParamSpec := Loop_Parameter_Specification (Iteration_Scheme (N));

      if Present (ItSpec)
        and then Nkind (ItSpec) = N_Iterator_Specification
      then
         Put_Line ("For2Loop : N_Iterator_Specification present");
         Put_Line ("For2Loop : not yet implemented !");
         raise Program_Error;

      elsif Present (ParamSpec)
        and then Nkind (ParamSpec) = N_Loop_Parameter_Specification
      then
         Put_Line ("N_Loop_Parameter_Specification present");
         Def_Ident := Defining_Identifier (ParamSpec);

         --  1. FOR implicit variable declaration in reactive scop
         ForCount := Create_And_Add_For_Count_Declaration;

         --  2. Initialize declared ForCount var to range'first value
         InitExpr := Initialize_For_Count_Variable_With_Low_Bound;

         --  3. Create loop exit condition : ForCount {>= | <=} range'last
         Create_And_Add_Exit_Loop_Statement (LoopStmts);

         --  4. Append old loop statements
         Append_List_To (LoopStmts, New_Copy_List_Tree (Statements (N)));

         --  5. Create assigment to increment loop counter
         IncrStmt := Make_Assignment_Statement
           (Sloc             => Sloc (N),
            Name             =>
              New_Occurrence_Of (Defining_Identifier (ForCount), Sloc (N)),
            Expression       =>
              Make_Op_Add
                (Sloc        => Sloc (N),
                 Left_Opnd   =>
                   New_Occurrence_Of (Defining_Identifier (ForCount), Sloc (N)),
                 Right_Opnd  => Make_Integer_Literal (Loc    => Sloc (N),
                                                      Intval => 1)));
         Append_To (LoopStmts, IncrStmt);

         --  6. Create new simple loop
         LoopStmt := Make_Loop_Statement
           (Sloc        => Sloc (N),
            Statements  => LoopStmts,
            End_Label   => Empty);

--           Set_Handled_Statement_Sequence
--             (N   => BlockDecl,
--              Val => Make_Handled_Sequence_Of_Statements
--                (Sloc       => Sloc (LoopStmt),
--                 Statements => Stmts));

         --  7. Replace old FOR loop by the simple loop statements
         Replace (Old_Node => N,
                  New_Node => LoopStmt);
         Insert_Before (N, InitExpr);
      end if;

      Put_Line ("Expand_For2Loop ended");
   end Expand_For2Loop;

   -----------------------------
   --  Expand_To_Simple_Loop  --
   -----------------------------

   procedure Expand_To_Simple_Loop (N : in Node_Id) is
      pragma Assert (Present (Iteration_Scheme (N)));
   begin
      Put_Line ("Expand_To_Simple_Loop started");

      if Present (Condition (Iteration_Scheme (N))) then
         Put_Line ("  WHILE loop ");
         Expand_While2Loop (N);

      elsif Present (Iterator_Specification (N))
        or Present (Loop_Parameter_Specification (N))
      then
         Put_Line ("  FOR loop ");
         Expand_For2Loop (N);
      end if;

      Put_Line ("Expand_To_Simple_Loop ended");
   end Expand_To_Simple_Loop;


   ------------------------------
   --  Expand_Loop_Statements  --
   ------------------------------

   procedure Expand_Loop_Statements (N : in Node_Id)
   is
      L : List_Id := Statements (Handled_Statement_Sequence (N));
      S : Node_Id := Empty;

      -------------------------
      --  Expand_Statements  --
      -------------------------

      procedure Expand_Statements (S : Node_Id)
      is
      begin
         case Nkind (S) is

         when N_Reactive_Select_Statement   =>
            declare
               CrtAlt : Node_Id := Empty;
               CrtS   : Node_Id := Empty;
            begin
               if Is_Non_Empty_List (Alternatives (S)) then
                  CrtAlt := First (Alternatives (S));
                  while Present (CrtAlt) loop
                     CrtS := First (Statements (CrtAlt));
                     while Present (CrtS) loop
                        Expand_Statements (CrtS);
                        Next (CrtS);
                     end loop;
                     Next (CrtAlt);
                  end loop;
               end if;
            end;

            when N_Reactive_Fork_Statement     =>
               Put_Line ("   Fork statement");
               declare
                  CrtAlt : Node_Id := Empty;
                  CrtS   : Node_Id := Empty;
               begin
                  if Is_Non_Empty_List (Alternatives (S)) then
                     CrtAlt := First (Alternatives (S));
                     while Present (CrtAlt) loop
                        CrtS := First (Statements (CrtAlt));
                        while Present (CrtS) loop
                           Expand_Statements (CrtS);
                           Next (CrtS);
                        end loop;
                        Next (CrtAlt);
                     end loop;
                  end if;
               end;

            when N_Reactive_Abort_Statement    =>
               Put_Line ("   Abort statement");
               declare
                  Handlers : List_Id := Abort_Handlers (S);
                  CrtH     : Node_Id := Empty;
                  CrtS   : Node_Id := Empty;
               begin
                  --  abort statements
                  if Is_Non_Empty_List (Statements (S)) then
                     CrtS := First (Statements (S));
                     while Present (CrtS) loop
                        Expand_Statements (CrtS);
                        Next (CrtS);
                     end loop;
                  end if;

                  --  abort handlers
                  if Is_Non_Empty_List (Handlers) then
                     CrtH := First (Handlers);
                     while Present (CrtH) loop
                        if Is_Non_Empty_List (Statements (CrtH)) then
                           CrtS := First (Statements (CrtH));
                           while Present (CrtS) loop
                              Expand_Statements (CrtS);
                              Next (CrtS);
                           end loop;
                        end if;
                        Next (CrtH);
                     end loop;
                  end if;
               end;

            when N_Case_Statement =>
               Put_Line ("   Case statement");
               if Case_Has_Waiting_Statement (S) then
                  declare
                     CrtAlt : Node_Id := Empty;
                     CrtS   : Node_Id := Empty;
                  begin
                     if Is_Non_Empty_List (Alternatives (S)) then
                        CrtAlt := First (Alternatives (S));
                        while Present (CrtAlt) loop
                           CrtS := First (Statements (CrtAlt));
                           while Present (CrtS) loop
                              Expand_Statements (CrtS);
                              Next (CrtS);
                           end loop;
                           Next (CrtAlt);
                        end loop;
                     end if;
                  end;
               end if;

            when N_If_Statement =>
               Put_Line ("   IF statement");
               if If_Has_Waiting_Statement (S) then
                  declare
                     CrtS : Node_Id := Empty;
                     CrtE : Node_Id := Empty;
                  begin
                     --  then part
                     if Is_Non_Empty_List (Then_Statements (S)) then
                        CrtS := First (Then_Statements (S));
                        while Present (CrtS) loop
                           Expand_Statements (CrtS);
                           Next (CrtS);
                        end loop;
                     end if;

                     --  elsif part
                     if Present (Elsif_Parts (S)) then
                        CrtS := First (Elsif_Parts (S));
                        while Present (CrtS) loop
                           if Is_Non_Empty_List (Then_Statements (CrtS)) then
                              CrtE := First (Then_Statements (CrtS));
                              while Present (CrtE) loop
                                 Expand_Statements (CrtE);
                                 Next (CrtE);
                              end loop;
                           end if;
                           Next (CrtS);
                        end loop;
                     end if;

                     --  else part
                     if Present (Else_Statements (S)) then
                        if Is_Non_Empty_List (Else_Statements (S)) then
                           CrtS := First (Else_Statements (S));
                           while Present (CrtS) loop
                              Expand_Statements (CrtS);
                              Next (CrtS);
                           end loop;
                        end if;
                     end if;
                  end;
               end if;

            when N_Loop_Statement =>
               Put_Line ("   LOOP statement");
               if Loop_Has_Waiting_Statement (S) then
                  if Present (Iteration_Scheme (S)) then
                     Expand_To_Simple_Loop (S);
                  end if;
               end if;

            when others =>
               Put_Line ("   Not concerned statements");
               null;
         end case;
      end Expand_Statements;

   begin
      --  expands all loop statements having wait instructions
      S := First (L);
      while Present (S) loop
         Expand_Statements (S);
         Next (S);
      end loop;

   end Expand_Loop_Statements;


   ----------------------
   -- Make_Loop_Vertex --
   ----------------------

   function Make_Loop_Vertex (N : Node_Id) return Rnode_Id is
      Rn      : Rnode_Id;
      Vrepeat : Rnode_Id;
      Vexit   : Rnode_Id;
      Vend    : Rnode_Id;
   begin
      Put_Line ("Make_Loop_Vertex Begin");

      --  if WHILE or FOR loop then transform in simple loop before creating
      --  loop vertex
--        if Present (Iteration_Scheme (N)) then
--           Expand_To_Simple_Loop (N);
--        end if;

      Rn := New_Node (R_Loop_Vertex);
      New_Nodes_Association (N, Rn);

      -- Create the end loop vertex.
      Vend := New_Node (R_End_Vertex);
      Set_Item_Node (Vend, N);
      Set_End_Vertex (Rn, Vend);

      Vrepeat := New_Node (R_Repeat_Loop_Vertex);
      Set_Repeat_Vertex (Rn, Vrepeat);

      Vexit := New_Node (R_Exit_Vertex);
      Set_Exit_Vertex (Rn, Vexit);

      Set_Body_Graph
	(Rn, Make_Graph_Vertex (Empty, Statements (N)));

      Put_Line ("Make_Loop_Vertex End");
      return Rn;
   end Make_Loop_Vertex;

   ----------------------
   -- Make_Exit_Vertex --
   ----------------------

   function Make_Exit_Vertex (N : Node_Id) return Rnode_Id is

      Rn  : Rnode_Id;
      Loc : Source_Ptr :=  Sloc (N);

   begin
      Put_Line ("Make_Exit_Vertex Begin");
      Rn := New_Node (R_Exit_Vertex);
      New_Nodes_Association (N, Rn);

      Set_Condition
	(Rn,
	 New_Copy_Tree (Condition (N)));

      Set_Parent_Vertex
	(Rn, Node_To_Rnode (Parent_Loop (N)));

      Put_Line ("Make_Exit_Vertex End Rn => " & Rn'Img);
      return Rn;
   end Make_Exit_Vertex;

   --------------------------------------------
   -- Make_If_Trans_Activation_Desactivation --
   --------------------------------------------

   function Make_If_Trans_Activation_Desactivation
     (V    : Rnode_Id;
      Nxt  : Rnode_Id;
      Cond : Node_Id) return Node_Id is

      If_Node    : Node_Id;
      Expr       : Node_Id;
      N          : Node_Id := Item_Node (V);
      Loc        : Source_Ptr := Sloc (N);
      Then_Stmts : List_Id;
   begin
      Put_Line ("Make_If_Trans_Activation_Desactivation Begin");
      --  The Activation/Desactivation code
      --   X_vc_desact := False;
      --   X_nxt_act   := True;
      --   following by transitionnal user code

      Then_Stmts := New_List;
      Append_To (Then_Stmts, Make_Desactivation (V));
      Append_To (Then_Stmts, Make_Activation (Nxt));

      Add_Transitionnal_Code (Then_Stmts, V, Nxt);

      --  When there no condition to activate next and desactivate current.
      --  for example for transient vertex, the code is :

	 --  When the condition is present, for example for a waiting vertex
	 --  the activation/desactivation code is


	 --    if X_vc and Cnod then
	 --       X_vc_desact := False;
	 --       X_nxt_act   := True;
	 --    end if;

      if Present (Cond) then
	 Expr := Make_Op_And
	   (Loc,
	    Left_Opnd  => New_Occurrence_Of (Xc (V), Loc),
	    Right_Opnd => New_Copy_Tree (Cond));

	 If_Node := Make_If_Statement
	   (Loc,
	    Condition       => Expr,
	    Then_Statements => Then_Stmts);

      --    if X_vc then
      --       X_vc_desact := False;
      --       X_nxt_act   := True;
      --    end if;

      else
	 If_Node := Make_If_Statement
	   (Loc,
	    Condition       => New_Occurrence_Of (Xc (V), Loc),
	    Then_Statements => Then_Stmts);
      end if;

      Put_Line ("Make_If_Trans_Activation_Desactivation End");
      return If_Node;
   end Make_If_Trans_Activation_Desactivation;

   --------------------------------------
   -- Make_If_Activation_Desactivation --
   --------------------------------------

   function Make_If_Activation_Desactivation
     (V    : Rnode_Id;
      Nxt  : Rnode_Id;
      Cond : Node_Id) return Node_Id is

      If_Node    : Node_Id;
      N          : Node_Id := Item_Node (V);
      Loc        : Source_Ptr := Sloc (N);
      Then_Stmts : List_Id;
   begin
      --  The Activation/Desactivation code
      --   X_vc_desact := False;
      --   X_nxt_act   := True;
      --   following by transitionnal user code

      Then_Stmts := New_List;
      Append_To (Then_Stmts, Make_Desactivation (V));
      Append_To (Then_Stmts, Make_Activation (Nxt));

      Add_Transitionnal_Code (Then_Stmts, V, Nxt);

      --  When there no condition to activate next and desactivate current.
      --  for example for transient vertex, the code is :

	 --  When the condition is present, for example for a waiting vertex
	 --  the activation/desactivation code is


	 --    if X_vc then
	 --       if Cnod then
	 --          X_vc_desact := False;
	 --          X_nxt_act   := True;
	 --       end if;
	 --    end if;

      if Present (Cond) then
	 If_Node := Make_If_Statement
	   (Loc,
	    Condition => New_Occurrence_Of (Xc (V), Loc),
	    Then_Statements =>
	      New_List
	      (Make_If_Statement
		 (Loc,
		  Condition       => New_Copy_Tree (Cond),
		  Then_Statements => Then_Stmts)));

      --    if X_vc then
      --       X_vc_desact := False;
      --       X_nxt_act   := True;
      --    end if;

      else
	 If_Node := Make_If_Statement
	   (Loc,
	    Condition       => New_Occurrence_Of (Xc (V), Loc),
	    Then_Statements => Then_Stmts);
      end if;

      return If_Node;
   end Make_If_Activation_Desactivation;

   ---------------------
   -- Make_Activation --
   ---------------------

   function Make_Activation (V : Rnode_Id) return Node_Id is

      N   : Node_Id := Item_Node (V);
      Loc : Source_Ptr := Sloc (N);
   begin
      --  The activation code for a waiting vertex is X_a := True;

      if Rkind (V) in Waiting_Vertex
	--  or else Rkind (V) in Activation_Transient_Vertex
      then
	 return
	   Make_Assignment_Statement
	   (Loc,
	    Name       => New_Occurrence_Of (Xa (V), Loc),
	    Expression => New_Occurrence_Of (Standard_True, Loc));

	 --  The activation code for a transient vertex is X_c := True;
      else
	 return
	   Make_Assignment_Statement
	   (Loc,
	    Name       => New_Occurrence_Of (Xc (V), Loc),
	    Expression => New_Occurrence_Of (Standard_True, Loc));
      end if;
   end Make_Activation;

   ------------------------
   -- Make_Desactivation --
   ------------------------

   function Make_Desactivation (V : Rnode_Id) return Node_Id is

      N   : Node_Id := Item_Node (V);
      Loc : Source_Ptr := Sloc (N);
   begin
      --  The desactivation code for a waiting vertex is X_d := False;
      if Rkind (V) in Waiting_Vertex
	--  or else Rkind (V) in Activation_Transient_Vertex
      then
	 return
	   Make_Assignment_Statement
	   (Loc,
	    Name       => New_Occurrence_Of (Xc (V), Loc),
	    Expression => New_Occurrence_Of (Standard_False, Loc));

	 --  The desactivation code for a transient vertex is X_c := True;
      else
	 return
	   Make_Assignment_Statement
	   (Loc,
	    Name       => New_Occurrence_Of (Xc (V), Loc),
	    Expression => New_Occurrence_Of (Standard_False, Loc));
      end if;
   end Make_Desactivation;

   -----------------------------------
   -- Make_Actication_Desactivation --
   -----------------------------------

   procedure Make_Actication_Desactivation
     (Stmts     : List_Id;
      V_Current : Rnode_Id;
      V_Next    : Rnode_Id) is

   begin
      Append_To (Stmts, Make_Desactivation (V_Current));
      Append_To (Stmts, Make_Activation (V_Next));
   end Make_Actication_Desactivation;

   ---------------------------
   -- Add_Transitonnal_Code --
   ---------------------------

   procedure Add_Transitionnal_Code
     (Stmts : List_Id;
      V     : Rnode_Id;
      Nxt   : Rnode_Id) is
   begin
      -- Get Eit code form V

      Append_List_To (Stmts, Exit_Code (V));

      -- Get Enter code from Nxt.

      Append_List_To (Stmts, Enter_Code (V));
   end Add_Transitionnal_Code;

   -----------------------------
   -- Create_Waiting_Raz_Code --
   -----------------------------

   procedure Create_Waiting_Raz_Code (V : Rnode_Id) is

      N   : Node_Id := Item_Node (V);
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
	     Name => New_Occurrence_Of (Xa (V), Loc),
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

      if Generate_Edge_Graph then

	 -- Falling Edge.

	 Append_To
	   (Update_Code (V),
	    Make_Assignment_Statement
	      (Loc,
	    Name       => New_Occurrence_Of (Xfe (V), Loc),
	       Expression =>
		 Make_Op_And
		 (Loc,
		  Left_Opnd  => New_Occurrence_Of (Xc (V), Loc),
		  Right_Opnd => Make_Op_And
		    (Loc,
		     Left_Opnd  => New_Occurrence_Of (Xd (V), Loc),
		     Right_Opnd => Make_Op_Not
		       (Loc,
			Right_Opnd => New_Occurrence_Of (Xa (V), Loc))))));


	 -- Raising Edge.

	 Append_To
	   (Update_Code (V),
	    Make_Assignment_Statement
	      (Loc,
	       Name       => New_Occurrence_Of (Xre (V), Loc),
	       Expression =>
		 Make_Op_And
		 (Loc,
		  Left_Opnd  => New_Occurrence_Of (Xa (V), Loc),
		  Right_Opnd => Make_Op_Not
		    (Loc,
		     Right_Opnd => New_Occurrence_Of (Xc (V), Loc)))));
      end if;

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

   procedure Interp_Graph_Vertex (G : Rnode_Id) is

      V : Rnode_Id;
   begin
      Put_Line ("Interp_Graph_Vertex Begin");
      Set_Trans_Code (G, New_List);

      if Is_Empty_List (Vertices_List (G)) then
	 return;
      end if;

      V := First (Vertices_List (G));
      while V /= No_Rnode loop

	 case Rkind (V) is

	    when R_Unused_At_Start =>
	       null;

	    when R_Graph_Vertex =>
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
	       Put_Line ("Goto Interp_If_Vertex");
	       Interp_If_Vertex (V);
	       Put_Line ("From Interp_If_Vertex");

	    when R_Loop_Vertex =>
	       Put_Line ("Goto Interp_Loop_Vertex");
	       Interp_Loop_Vertex (V);
	       Put_Line ("From Interp_Loop_Vertex");

	    when R_Exit_Vertex =>
	       Put_Line ("Goto Interp_Exit_Vertex");
	       Interp_Exit_Vertex (V);
	       Put_Line ("From Interp_Exit_Vertex");

	    when R_Fork_Vertex =>
	       Put_Line ("Goto Interp_Fork_Vertex");
	       Interp_Fork_Vertex (V);
	       Put_Line ("From Interp_Fork_Vertex");

	    when R_Abort_Vertex =>
	       Put_Line ("Goto Interp_Abort_Vertex");
               Interp_Abort_Vertex (V);
               Put_Line ("From Interp_Abort_Vertex");

	    when R_Weak_Abort_Vertex =>
	       null;

	    when R_Pause_Vertex =>
	       Put_Line ("Goto Interp_Pause_Vertex");
	       Interp_Pause_Vertex (V);
	       Put_Line ("From Interp_Pause_Vertex");

	    when R_Wait_Vertex =>
	       Put_Line ("Goto Interp_Wait_Vertex");
	       Interp_Wait_Vertex (V);
	       Put_Line ("From Interp_Wait_Vertex");

	    when R_Sync_Vertex =>
	       Put_Line ("Goto Interp_Sync_Vertex");
	       Interp_Sync_Vertex (V);
	       Put_Line ("From Interp_Sync_Vertex");

	    when R_Select_Vertex =>
	       Put_Line ("Goto Interp_Select_Vertex");
	       Interp_Select_Vertex (V);
	       Put_Line ("From Interp_Select_Vertex");

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
      -- Raz Code
      Create_Waiting_Raz_Code (V);

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

   ------------------------
   -- Collect_Code_Graph --
   ------------------------

   procedure Collect_Code_Graph
     (Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      G                : Rnode_Id) is

      V : Rnode_Id;
   begin
      if Is_Empty_List (Vertices_List (G)) then
	 return;
      end if;

      V := First (Vertices_List (G));
      while V /= No_Rnode loop

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
	       Collect_Exit_Vertex
		 (Update_Raz_Stmts,
		  Update_Stmts,
		  Trans_Stmts,
		   V);

	    when R_If_Vertex               =>
	       Collect_If_Vertex
		 (Update_Raz_Stmts,
		  Update_Stmts,
		  Trans_Stmts,
		   V);

	    when R_Loop_Vertex             =>
	       Collect_Loop_Vertex
		 (Update_Raz_Stmts,
		  Update_Stmts,
		  Trans_Stmts,
		   V);

	    when R_Exit_Vertex             =>
	       Collect_Exit_Vertex
		 (Update_Raz_Stmts,
		  Update_Stmts,
		  Trans_Stmts,
		   V);

	    when R_Fork_Vertex             =>
	       Collect_Fork_Vertex
		 (Update_Raz_Stmts,
		  Update_Stmts,
		  Trans_Stmts,
		   V);

	    when R_Abort_Vertex            =>
	       Collect_Abort_Vertex
		 (Update_Raz_Stmts,
		  Update_Stmts,
		  Trans_Stmts,
		   V);

	    when R_Weak_Abort_Vertex       =>
	       Collect_Abort_Vertex
		 (Update_Raz_Stmts,
		  Update_Stmts,
		  Trans_Stmts,
		   V);

	    when R_Pause_Vertex            =>
	       Collect_Pause_Vertex
		 (Update_Raz_Stmts,
		  Update_Stmts,
		  Trans_Stmts,
		   V);

	    when R_Wait_Vertex             =>
	       Collect_Wait_Vertex
		 (Update_Raz_Stmts,
		  Update_Stmts,
		  Trans_Stmts,
		   V);

	    when R_Sync_Vertex             =>
	       Collect_Sync_Vertex
		 (Update_Raz_Stmts,
		  Update_Stmts,
		  Trans_Stmts,
		   V);

	    when R_Select_Vertex           =>
	       Collect_Select_Vertex
		 (Update_Raz_Stmts,
		  Update_Stmts,
		  Trans_Stmts,
		   V);

	    when R_Unused_At_End           =>
	       null;
	 end case;

         Next (V);
      end loop;

      --  if Is_Non_Empty_List (Xe_Code (G)) then
      --     Append_List_To (Trans_Stmts, Xe_Code (G));
      --  end if;

      if Is_Non_Empty_List (Trans_Code (G)) then
	 Append_List_To (Trans_Stmts, Trans_Code (G));
      end if;
   end Collect_Code_Graph;

   --------------------------
   -- Collect_Pause_Vertex --
   --------------------------

   procedure Collect_Pause_Vertex
     (Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      V                : Rnode_Id) is
   begin
      Append_List_To (Update_Raz_Stmts, Update_Raz_Code (V));
      Append_List_To (Update_Stmts, Update_Code (V));
   end Collect_Pause_Vertex;

   -------------------------
   -- Collect_Wait_Vertex --
   -------------------------

   procedure Collect_Wait_Vertex
     (Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      V                : Rnode_Id) is
   begin
      Append_List_To (Update_Raz_Stmts, Update_Raz_Code (V));
      Append_List_To (Update_Stmts, Update_Code (V));

      if Is_Non_Empty_List (Trans_Code (V)) then
	 Append_List_To (Trans_Stmts, Trans_Code (V));
      end if;
   end Collect_Wait_Vertex;

   -------------------------
   -- Collect_Sync_Vertex --
   -------------------------

   procedure Collect_Sync_Vertex
     (Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      V                : Rnode_Id) is
   begin
      Append_List_To (Update_Raz_Stmts, Update_Raz_Code (V));
      Append_List_To (Update_Stmts, Update_Code (V));

      if Is_Non_Empty_List (Trans_Code (V)) then
	 Append_List_To (Trans_Stmts, Trans_Code (V));
      end if;
   end Collect_Sync_Vertex;

      --------------------------
   -- Collect_Abort_Vertex --
   --------------------------

   procedure Collect_Abort_Vertex
     (Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      V                : Rnode_Id) is
   begin
      Append_List_To (Update_Raz_Stmts, Update_Raz_Code (V));

      -- Abort.
      if Is_Non_Empty_List (Trans_Code (V)) then
	 Append_List_To (Trans_Stmts, Trans_Code (V));
      end if;

      -- Body
      Collect_Code_Graph
	(Update_Raz_Stmts, Update_Stmts, Trans_Stmts, Body_Graph (V));

      -- Handler
      if Handler_Graph (V) /= No_Rnode then
	 Collect_Code_Graph
	   (Update_Raz_Stmts, Update_Stmts, Trans_Stmts, Handler_Graph (V));
      end if;

      -- Vend
      if Is_Non_Empty_List (Trans_Code (End_Vertex (V))) then
	 Append_List_To (Trans_Stmts, Trans_Code (End_Vertex (V)));
      end if;
   end Collect_Abort_Vertex;

   ---------------------------
   -- Collect_Select_Vertex --
   ---------------------------

   procedure Collect_Select_Vertex
     (Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      V                : Rnode_Id) is

      Alt : Rnode_Id;
   begin
      Append_List_To (Update_Raz_Stmts, Update_Raz_Code (V));
      Append_List_To (Update_Stmts, Update_Code (V));

      if Is_Non_Empty_List (Trans_Code (V)) then
	 Append_List_To (Trans_Stmts, Trans_Code (V));
      end if;

      if Present (Alternatives (V)) then
	 Alt := First (Alternatives (V));
	 while Alt /= No_Rnode loop
	    Collect_Code_Graph
	      (Update_Raz_Stmts, Update_Stmts, Trans_Stmts, Alt);
	    Next (Alt);
	 end loop;
      end if;

      if Is_Non_Empty_List (Trans_Code (End_Vertex (V))) then
	 Append_List_To (Trans_Stmts, Trans_Code (End_Vertex (V)));
      end if;
   end Collect_Select_Vertex;

   -------------------------
   -- Collect_Fork_Vertex --
   -------------------------

   procedure Collect_Fork_Vertex
     (Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      V                : Rnode_Id) is

      Alt : Rnode_Id;
   begin
      Append_List_To (Update_Raz_Stmts, Update_Raz_Code (V));

      if Is_Non_Empty_List (Trans_Code (V)) then
	 Append_List_To (Trans_Stmts, Trans_Code (V));
      end if;

      if Present (Alternatives (V)) then
	 Alt := First (Alternatives (V));
	 while Alt /= No_Rnode loop
	    Collect_Code_Graph
	      (Update_Raz_Stmts, Update_Stmts, Trans_Stmts, Alt);
	    Next (Alt);
	 end loop;
      end if;

      if Is_Non_Empty_List (Trans_Code (End_Vertex (V))) then
	 Append_List_To (Trans_Stmts, Trans_Code (End_Vertex (V)));
      end if;
   end Collect_Fork_Vertex;

   -----------------------
   -- Collect_If_Vertex --
   -----------------------

   procedure Collect_If_Vertex
     (Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      V                : Rnode_Id) is

      Alt : Rnode_Id;
   begin
      Append_List_To (Update_Raz_Stmts, Update_Raz_Code (V));

      if Is_Non_Empty_List (Trans_Code (V)) then
	 Append_List_To (Trans_Stmts, Trans_Code (V));
      end if;

      -- Then Graph

      Collect_Code_Graph
	(Update_Raz_Stmts, Update_Stmts, Trans_Stmts, Then_Graph (V));

      -- Build graf of all alternatives.

     if Present (Alternatives (V)) then
	 Alt := First (Alternatives (V));
	 while Alt /= No_Rnode loop
	    Collect_Code_Graph
	      (Update_Raz_Stmts, Update_Stmts, Trans_Stmts, Alt);
	    Next (Alt);
	 end loop;
     end if;

     -- Else Graph

      if Else_Graph (V) /= No_Rnode then
	 Collect_Code_Graph
	   (Update_Raz_Stmts, Update_Stmts, Trans_Stmts, Else_Graph (V));
      end if;

      if Is_Non_Empty_List (Trans_Code (End_Vertex (V))) then
	 Append_List_To (Trans_Stmts, Trans_Code (End_Vertex (V)));
      end if;
   end Collect_If_Vertex;

   -------------------------
   -- Collect_Loop_Vertex --
   -------------------------

   procedure Collect_Loop_Vertex
     (Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      V                : Rnode_Id) is

      Loop_Node  : Node_Id;
      Loop_Stmts : List_Id;
   begin
      Append_List_To (Update_Raz_Stmts, Update_Raz_Code (V));
      Append_List_To (Update_Raz_Stmts, Update_Raz_Code (Repeat_Vertex (V)));
      Append_List_To (Update_Raz_Stmts, Update_Raz_Code (End_Vertex (V)));

      -- Body Graph

      if Is_Non_Empty_List (Trans_Code (V)) then
	 Loop_Node := First (Trans_Code (V));
	 if Present (Loop_Node) then
	    Loop_Stmts := Statements (Loop_Node);
	 else
	    raise Program_Error;
	 end if;
      else
	 raise Program_Error;
      end if;

      Collect_Code_Graph
	(Update_Raz_Stmts, Update_Stmts, Loop_Stmts, Body_Graph (V));

     -- Repeat vertex

      Append_List_To (Loop_Stmts, Trans_Code (Repeat_Vertex (V)));

      --  Exit vertex

      if Is_Non_Empty_List (Trans_Code (Exit_Vertex (V))) then
	 Append_List_To (Loop_Stmts, Trans_Code (Exit_Vertex (V)));
      end if;

      if Is_Non_Empty_List (Trans_Code (V)) then
	 Append_List_To (Trans_Stmts, Trans_Code (V));
      end if;

      -- End Vertex

      if Is_Non_Empty_List (Trans_Code (End_Vertex (V))) then
	 Append_List_To (Trans_Stmts, Trans_Code (End_Vertex (V)));
      end if;

   end Collect_Loop_Vertex;

   -------------------------
   -- Collect_Exit_Vertex --
   -------------------------

   procedure Collect_Exit_Vertex
     (Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      V                : Rnode_Id) is
   begin
      Append_List_To (Update_Raz_Stmts, Update_Raz_Code (V));

      if Is_Non_Empty_List (Trans_Code (V)) then
	 Append_List_To (Trans_Stmts, Trans_Code (V));
      end if;
   end Collect_Exit_Vertex;

   ----------------------------
   -- Collect_Handler_Vertex --
   ----------------------------

   procedure Collect_Handler_Vertex
     (Update_Raz_Stmts : List_Id;
      Update_Stmts     : List_Id;
      Trans_Stmts      : List_Id;
      V                : Rnode_Id) is
   begin
      Append_List_To (Update_Raz_Stmts, Update_Raz_Code (V));

      if Is_Non_Empty_List (Trans_Code (V)) then
	 Append_List_To (Trans_Stmts, Trans_Code (V));
      end if;
   end Collect_Handler_Vertex;


--     ----------------------
--     -- Need_To_Prefixed --
--     ----------------------
--
--     function Need_To_Prefixed (N : Node_Id) return Boolean is
--     begin
--        pragma Assert (Nkind (N) = N_Identifier);
--        return Vars_Assoc.Has_Anode (Entity (N));
--     end Need_To_Prefixed;

   ----------------------
   -- Prefix_Name_By_T --
   ----------------------

   function Prefix_Name_By_T
     (N    : Node_Id;
      This : Node_Id) return Node_id is

      Selected_Current  : Node_Id;
      Selected_Previous : Node_Id;
      Selector          : Node_Id;
      Prefix_Name       : Node_Id;
      New_Selected      : Node_Id;
      Ident             : Node_Id;
      Root_Node         : Node_Id;
   begin
      Root_Node := N;

      if Nkind (N) = N_Identifier then
	 if Vars_Assoc.Has_Anode (Entity (N)) then
	    Ident := New_Copy (N);
	    Set_Entity (N, Empty);

	    Root_Node := Make_Selected_Component
	      (Sloc           => Sloc (Ident),
	       Prefix         =>
		 Make_Identifier
		 (Sloc   => Sloc (Ident),
		  Chars  => Chars (Defining_Identifier (This))),
	       Selector_Name  => Ident);
	 end if;

      else
	 Selected_Current := N;
	 Selected_Previous := Empty;

	 while Present (Selected_Current) loop
	    Selector := Selector_Name (Selected_Current);
	    Prefix_Name := Prefix (Selected_Current);

	    if Vars_Assoc.Has_Anode (Entity (Selector)) then
	       Set_Selector_Name
		 (Selected_Current,
		  Make_Identifier
		    (Sloc   => Sloc (Selector),
		     Chars  => Chars (Defining_Identifier (This))));

	       New_Selected := Make_Selected_Component
		 (Sloc           => Sloc (Selector),
		  Prefix         => Selected_Current,
		  Selector_Name  => Selector);

	       if Present (Selected_Previous) then
		  Set_Prefix
		    (Selected_Previous, New_Selected);
	       else
		  Root_Node := New_Selected;
	       end if;

	       exit;

	    elsif Nkind (Prefix_Name) = N_Identifier then
	       if Vars_Assoc.Has_Anode (Entity (Prefix_Name)) then
		  New_Selected := Make_Selected_Component
		    (Sloc           => Sloc (Prefix_Name),
		     Prefix         =>
		       Make_Identifier
		       (Sloc   => Sloc (Ident),
			Chars  => Chars (Defining_Identifier (This))),
		     Selector_Name  => Prefix_Name);

		  Set_Prefix (Selected_Current, New_Selected);
		  exit;
	       end if;
	    end if;

	    Set_Entity (Selector, Empty);
	    Set_Entity (Prefix_Name, Empty);

	    Selected_Previous := Selected_Current;
	    Selected_Current := Prefix (Selected_Current);
	 end loop;
      end if;

      return Root_Node;
   end Prefix_Name_By_T;

   -----------------------------
   --  Prefix_Name_Statements --
   -----------------------------

   procedure Prefix_Name_Statements
     (N    : Node_Id;
      This : Node_Id) is

      Stmts : List_Id;
      Item  : Node_Id;

      function Process_Reactive_Statement_Name (N : Node_Id) return Traverse_Result;

      procedure Prefix_Reactive_Name is
	 new Traverse_Proc (Process_Reactive_Statement_Name);

      function Process_Reactive_Statement_Name (N : Node_Id) return Traverse_Result is
	 Replace_Node : Node_Id;
      begin
	 if Nkind_In (N, N_Identifier, N_Selected_Component) then
	    Replace_Node := Prefix_Name_By_T (N, This);
	    Replace (N, Replace_Node);
	    return Skip;
	 else
	    return Ok;
	 end if;
      end Process_Reactive_Statement_Name;
   begin
      Stmts := Statements (Handled_Statement_Sequence (N));
      if Is_Non_Empty_List (Stmts) then
         Item := First (Stmts);
         while Present (Item) loop
	    Prefix_Reactive_Name (Item);
	    Next (item);
	 end loop;
      end if;

   end Prefix_Name_Statements;

   ----------------------------
   --  Reset_Name_Statements --
   ----------------------------

   procedure Reset_Name_Statements (N : Node_Id) is

      Stmts : List_Id;
      Item  : Node_Id;

      function Process_Reactive_Statement_Name (N : Node_Id) return Traverse_Result;

      procedure Prefix_Reactive_Name is
	 new Traverse_Proc (Process_Reactive_Statement_Name);

      function Process_Reactive_Statement_Name (N : Node_Id) return Traverse_Result is
      begin
	 if Nkind (N) in N_Has_Entity then
	    Set_Entity (N, Empty);
	 end if;

	 if Nkind (N) in N_Has_Etype then
	    Set_Etype (N, Empty);
	 end if;

	 return Ok;
      end Process_Reactive_Statement_Name;
   begin
      Stmts := Statements (Handled_Statement_Sequence (N));

      if Is_Non_Empty_List (Stmts) then
         Item := First (Stmts);
         while Present (Item) loop
	    Prefix_Reactive_Name (Item);
	    Next (item);
	 end loop;
      end if;
   end Reset_Name_Statements;

end Exp_Ch14;
