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

with Ada.Text_Io; use Ada.Text_Io;

with Ada.Unchecked_Deallocation;

with Atree; use Atree;
with Stand; use Stand;
with Namet; use Namet;
with Nlists; use Nlists;
with Nmake; use Nmake;
with Sem_Util; use Sem_Util;
with Sinfo; use Sinfo;
with Einfo; use Einfo;

with Artics.Chars_Codes;
with Artics.Strings_Stocks;
with Reflex.Formats; use Reflex.Formats;
--  with Unity.Gen.Utils; use Unity.Gen.Utils;

package body Reflex.Expanders is
   
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

   
   -------------------------
   -- New_Reflex_Expander --
   -------------------------
   
   function New_Reflex_Expander return Reflex_Expander_Ptr is
      This : Reflex_Expander_Ptr := 
	new Reflex_Expander_Record'(No_Reflex_Expander_Record);
   begin
      Scope_Stack.Init (This.Scopes);
      return new Reflex_Expander_Record'(No_Reflex_Expander_Record);
   end New_Reflex_Expander;
   
   --------------------------
   -- Free_Reflex_Expander --
   --------------------------
   
   procedure Free_Reflex_Expander (This : in out Reflex_Expander_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Reflex_Expander_Record, Reflex_Expander_Ptr);
      use Scope_Stack;
   begin
      if This.Scopes /= Scope_Stack.No_Instance then
	 Scope_Stack.Free (This.Scopes);
      end if;
      Free (This);
   end Free_Reflex_Expander;
   
   -------------------
   -- Get_Main_Node --
   -------------------
   
   function Get_Main_Node
     (This : access Reflex_Expander_Record) return Node_Id is
   begin
      return This.Main_Node;
   end Get_Main_Node;
   
   -------------------
   -- Set_Main_Node --
   -------------------
   
   procedure Set_Main_Node
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      This.Main_Node := Node;
   end Set_Main_Node;
   
   -----------------------------
   -- Get_Current_Source_File --
   -----------------------------
   
   function Get_Current_Source_File
     (This : access Reflex_Expander_Record) return Source_File_Index is
   begin
      return This.Current_Source_File;
   end Get_Current_Source_File;
   
   -----------------------------
   -- Set_Current_Source_File --
   -----------------------------
   
   procedure Set_Current_Source_File
     (This        : access Reflex_Expander_Record;
      Source_File : Source_File_Index) is
   begin
      This.Current_Source_File := Source_File;
   end Set_Current_Source_File;
   
   ------------------------------
   -- Get_Full_Code_Generation --
   ------------------------------
   
   function Get_Full_Code_Generation
     (This : access Reflex_Expander_Record) return Boolean is
   begin
      return This.Full_Code_Generation;
   end Get_Full_Code_Generation;
   
   ------------------------------
   -- Set_Full_Code_Generation --
   ------------------------------
   
   procedure Set_Full_Code_Generation
     (This : access Reflex_Expander_Record;
      Gen  : Boolean) is
   begin
      This.Full_Code_Generation := Gen;
   end Set_Full_Code_Generation;
   
   ------------------------------
   -- Get_In_Package_Body_Init --
   ------------------------------
   
   function Get_In_Package_Body_Init
     (This : access Reflex_Expander_Record) return Boolean is
   begin
      return This.In_Package_Body_Init;
   end Get_In_Package_Body_Init;
   
   ------------------------------
   -- Set_In_Package_Body_Init --
   ------------------------------
   
   procedure Set_In_Package_Body_Init
     (This : access Reflex_Expander_Record;
      Init : Boolean) is
   begin
      This.In_Package_Body_Init := Init;
   end Set_In_Package_Body_Init;
   
   ------------------
   -- Do_Expansion --
   ------------------
   
   procedure Do_Expansion (This : access Reflex_Expander_Record) is
   begin
      null;
   end Do_Expansion;
   
   -----------------
   -- Close_Scope --
   -----------------

   procedure Close_Scope
     (This : access Reflex_Expander_Record) is
      use Scope_Stack;
   begin
      Decrement_Last (This.Scopes);
   end Close_Scope;

   -----------------
   -- Close_Scope --
   -----------------

   procedure Close_Scope
     (This    : access Reflex_Expander_Record;
      Scop_Id : Natural) is
      use Scope_Stack;
   begin
      loop
	 Close_Scope (This);
	 exit when Last (This.Scopes) < Scop_Id;
      end loop;
   end Close_Scope;

   ----------------------
   -- Current_Scope_Id --
   ----------------------

   function Current_Scope_Id
     (This : access Reflex_Expander_Record) return Natural is
      use Scope_Stack;
   begin
      return Last (This.Scopes);
   end Current_Scope_Id;

   ---------------------
   -- In_Declarations --
   ---------------------

   function In_Declarations
     (This : access Reflex_Expander_Record) return Boolean is
      use Scope_Stack;
   begin
      return This.Scopes.Table (Last (This.Scopes)).In_Declarations;
   end In_Declarations;

   -------------------------
   -- Set_In_Declarations --
   -------------------------

   procedure Set_In_Declarations
     (This : access Reflex_Expander_Record;
      V    : Boolean) is
      use Scope_Stack;
   begin
      This.Scopes.Table (Last (This.Scopes)).In_Declarations := V;
      This.Scopes.Table (Last (This.Scopes)).In_Statements   := not V;
   end Set_In_Declarations;

   ----------------
   -- Open_Scope --
   ----------------

   procedure Open_Scope
     (This : access Reflex_Expander_Record;
      E    : Entity_Id := Empty) is
      use Scope_Stack;
   begin
      Increment_Last (This.Scopes);
      
      This.Scopes.Table (Last (This.Scopes)) := No_Scope_Stack_Entry;
      
      This.Scopes.Table (Last (This.Scopes)).Scope_Entity := E;
   end Open_Scope;

   -------------------
   -- In_Statements --
   -------------------
   
   function In_Statements
     (This : access Reflex_Expander_Record) return Boolean is
      use Scope_Stack;
   begin
      return This.Scopes.Table (Last (This.Scopes)).In_Statements;
   end In_Statements;

   -----------------------
   -- Set_In_Statements --
   ------------------------

   procedure Set_In_Statements 
     (This : access Reflex_Expander_Record;
      V    : Boolean) is
      use Scope_Stack;
   begin
      This.Scopes.Table (Last (This.Scopes)).In_Statements := V;
      This.Scopes.Table (Last (This.Scopes)).In_Declarations := not V;
   end Set_In_Statements;

   ----------------------
   -- Get_Scope_Entity --
   ----------------------

   function Get_Scope_Entity
     (This : access Reflex_Expander_Record) return Entity_Id is
      use Scope_Stack;
   begin
      return This.Scopes.Table (Last (This.Scopes)).Scope_Entity;
   end Get_Scope_Entity;

   ----------------------
   -- Set_Scope_Entity --
   ----------------------

   procedure Set_Scope_Entity
     (This : access Reflex_Expander_Record;
      E    : Entity_Id) is
      use Scope_Stack;
   begin
      This.Scopes.Table (Last (This.Scopes)).Scope_Entity := E;
   end Set_Scope_Entity;
   
   --------------------------
   -- Get_Declarative_List --
   --------------------------
   
   function Get_Declarative_List
     (This : access Reflex_Expander_Record) return List_Id is
      use Scope_Stack;
   begin
      return This.Scopes.Table (Last (This.Scopes)).Declarative_List;
   end Get_Declarative_List;
   
   --------------------------
   -- Set_Declarative_List --
   --------------------------
   
   procedure Set_Declarative_List
     (This : access Reflex_Expander_Record;
      L    : List_Id) is
      use Scope_Stack;
   begin
      This.Scopes.Table (Last (This.Scopes)).Declarative_List := L;
   end Set_Declarative_List;
   
   --------------------
   -- Get_Scope_Node --
   --------------------
   
   function Get_Scope_Node
     (This : access Reflex_Expander_Record) return Node_Id is
      use Scope_Stack;
   begin
      return This.Scopes.Table (Last (This.Scopes)).Scope_Node;
   end Get_Scope_Node;
   
   --------------------
   -- Set_Scope_Node --
   --------------------
   
   procedure Set_Scope_Node
     (This : access Reflex_Expander_Record;
      N    : Node_Id) is
      use Scope_Stack;
   begin
      This.Scopes.Table (Last (This.Scopes)).Scope_Node := N;
   end Set_Scope_Node;
   
   ---------------------
   -- In_Private_Part --
   ---------------------
   
   function In_Private_Part
     (This : access Reflex_Expander_Record) return Boolean is
      use Scope_Stack;
   begin
      return This.Scopes.Table (Last (This.Scopes)).Private_Part;
   end In_Private_Part;
   
   ---------------------
   -- In_Private_Part --
   ---------------------
   
   procedure Set_In_Private_Part
     (This : access Reflex_Expander_Record;
      V    : Boolean) is
      use Scope_Stack;
   begin
      This.Scopes.Table (Last (This.Scopes)).Private_Part := V;
   end Set_In_Private_Part;
   
   function Make_Unique_Label
     (This  : access Reflex_Expander_Record;
      Name  : Name_Id) return Name_Id is
   begin
      return Name;
   end Make_Unique_Label;
   
   --------------------------------
   -- Make_Unique_Label_In_Scope --
   --------------------------------
   
   function Make_Unique_Label_In_Scope
     (This  : access Reflex_Expander_Record;
      Name  : Name_Id;
      Count : Natural := 1) return Name_Id is
      
      New_Name : Name_Id;
      Scp      : Entity_Id;
      E        : Entity_Id;
   begin
      New_Name := Name;
      
      if Count /= 0 then
	 declare 
	    S : String := Get_Name_String (Name) & integer_To_String (Count);
	 begin
	    New_Name := Name_Find (S);
	 end;
      end if;
      
      Scp := Unique_Defining_Entity (This.Get_Scope_Node);
      E := First_Entity (Scp);
      while Present (E) loop
	 if Ekind (E) = E_Label
	   and then Chars (E) = New_Name 
	 then
	    return Make_Unique_Label_In_Scope (This, Name, Count + 1);
	 end if;
	 E := Next_Entity (E);
      end loop;
      
      return New_Name;
   end Make_Unique_Label_In_Scope;
   
   ------------------------------
   -- Make_Unique_label_Entity --
   ------------------------------
   
   function Make_Unique_Label_Entity
     (This  : access Reflex_Expander_Record;
      Loc   : Source_Ptr;
      Name  : Name_Id;
      Count : Natural := 0) return Node_Id is
      
      Entity_Name : Name_Id;
      Lab         : Node_Id;
      Scp         : Entity_Id;
   begin
      Entity_Name := Make_Unique_Label_In_Scope (This, Name);
      
      Lab := Make_Defining_Identifier (Loc, Entity_Name);
      
      Scp := Unique_Defining_Entity (This.Get_Scope_Node);
      
      Set_Ekind           (Lab, E_Label);
      Set_Etype           (Lab, Standard_Void_Type);
      Set_Enclosing_Scope (Lab, Scp);
      Set_Reachable       (Lab, True);
      
      Append_Entity (Lab, Scp);
      
      return Lab;
   end Make_Unique_Label_Entity;
   
   ----------------------
   -- Make_Unique_Name --
   ----------------------
   
   function Make_Unique_Name 
     (This  : access Reflex_Expander_Record;
      Name  : Name_Id;
      Count : Natural := 0) return Name_Id is
      
      Scp      : Entity_Id;
      New_Name : Name_Id := Name;
   begin
      Scp := Unique_Defining_Entity (This.Get_Scope_Node);
      New_Name := Make_Unique_Name_In_Scope (Name, Scp);
      
      return New_Name;
   end Make_Unique_Name;
   
   ------------------------
   -- Make_Unique_Entity --
   ------------------------
   
   function Make_Unique_Entity
     (This : access Reflex_Expander_Record;
      Loc  : Source_Ptr;
      Name : Name_Id) return Entity_Id is
      
      Scp : Entity_Id;
   begin
      Scp := Unique_Defining_Entity (This.Get_Scope_Node);
      
      return Make_Unique_Entity_In_Scope (Loc, Name, Scp);
   end Make_Unique_Entity;
   
   -------------------------------
   -- Make_Unique_Name_In_Scope --
   -------------------------------
   
   function Make_Unique_Name_In_Scope
     (Name  : Name_Id;
      Scp   : Entity_Id;
      Count : Natural := 0) return Name_Id is
      
      E        : Entity_Id;
      New_Name : Name_Id := Name;
   begin
      pragma Assert (Present (Scp));
      
      if Count /= 0 then
	 declare 
	    S : String := Get_Name_String (Name) & Integer_To_String (Count);
	 begin
	    New_Name := Name_Find (S);
	 end;
      end if;
      
      E := First_Entity (Scp);
      while Present (E) loop
	 if Chars (E) = New_Name then
	    New_Name := Make_Unique_Name_In_Scope (Name, Scp, Count + 1);
	    exit;
	 end if;
	 E := Next_Entity (E);
      end loop;
      
      return New_Name;
   end Make_Unique_Name_In_Scope;
   
   ---------------------------------
   -- Make_Unique_Entity_In_Scope --
   ---------------------------------
   
   function Make_Unique_Entity_In_Scope
     (Loc  : Source_Ptr;
      Name : Name_Id;
      Scp  : Entity_Id) return Entity_Id is
      
      Def : Node_Id;
   begin
      Def:= Make_Defining_Identifier
	(Loc, Make_Unique_Name_In_Scope (Name, Scp));
      return Def;
   end Make_Unique_Entity_In_Scope;
   
   -------------------------
   -- Search_Reuse_Entity --
   -------------------------
   
   function Search_Reuse_Entity_Old
     (This      : access Reflex_Expander_Record;
      Name      : Name_Id;
      Name_Type : Entity_Id) return Entity_Id is
      
      Scp : Entity_Id;
      E   : Entity_Id;
   begin
      Scp := Unique_Defining_Entity (This.Get_Scope_Node);
      
      E := First_Entity (Scp);
      while Present (E) loop
	 if Scp = Scope (E) 
	   and then Chars (E) = Name 
	 then
	    if Is_Entity_Reusable (E)
	      and then not Entity_In_Use (E)
	      and then Base_Type (Get_Type_Full_View (Etype (E)))
	      = Base_Type (Get_Type_Full_View (Name_Type)) 
	    then
	       return E;
	    end if;
	    
	    return Empty;
	 end if;
	 
	 E := Next_Entity (E);
      end loop;
      
      return Empty;
   end Search_Reuse_Entity_Old;
   
   -------------------------
   -- Search_Reuse_Entity --
   -------------------------
   
   function Search_Reuse_Entity
     (This      : access Reflex_Expander_Record;
      Name      : Name_Id;
      Name_Type : Entity_Id;
      Count     : Natural := 0) return Entity_Id is
      
      Scp : Entity_Id;
      E   : Entity_Id;
   begin
      Scp := Unique_Defining_Entity (This.Get_Scope_Node);
      E := First_Entity (Scp);
      while Present (E) loop
	 if Scp = Scope (E) 
	   and then Chars (E) = Name 
	 then
	    if Is_Entity_Reusable (E) 
	      and then not Entity_In_Use (E)
	      and then Base_Type (Get_Type_Full_View (Etype (E)))
	      = Base_Type (Get_Type_Full_View (Name_Type)) 
	    then
	       return E;
	    else
	       declare
		  S : String := 
		    Get_Name_String (Name) & Integer_To_String (Count + 1);
		  New_Name : Name_Id := Name_Find (S);
	       begin
		  return Search_Reuse_Entity
		    (This, New_Name, Name_Type, Count + 1);
	       end;
	    end if;
	 end if;
	 
	 E := Next_Entity (E);
      end loop;
      
      return Empty;
   end Search_Reuse_Entity;
   
   -----------------------
   -- Make_Reuse_Entity --
   -----------------------
   
   function Make_Reuse_Entity
     (This      : access Reflex_Expander_Record;
      Loc       : Source_Ptr;
      Name      : Name_Id;
      Name_Type : Entity_Id) return Entity_Id is
      
      E   : Entity_Id;
   begin
      E := Make_Unique_Entity (This, Loc, Name);
      Set_Etype (E, Base_Type  (Get_Type_Full_View (Name_Type)));
      Set_Entity_Reusable (E, True);
      
      return E;
   end Make_Reuse_Entity;
   
   ---------------------------
   -- Declare_Current_Scope --
   ---------------------------
   
   procedure Declare_Current_Scope 
     (This : access Reflex_Expander_Record;
      Decl : Node_Id) is
      
      use Scope_Stack;
      Decls  : List_Id;
      Node   : Node_id;
      E      : Entity_Id;
      C      : Entity_Id;
      Def_Id : Entity_Id;
   begin
      Node := This.Get_Scope_Node;
      E := Unique_Defining_Entity (Node);
      
      Decls := No_List;
      
      if Nkind (Node) = N_Package_Declaration then
	 if This.Scopes.Table (Last (This.Scopes)).Private_Part then
	    Decls := Private_Declarations (Specification (Node));
	    if Is_Empty_List (Decls) then
	       Decls := New_List;
	       Set_Private_Declarations (Node, Decls);
	    end if;
	 else
	    Decls := Visible_Declarations (Specification (Node));
	    if Is_Empty_List (Decls) then
	       Decls := New_List;
	       Set_Visible_Declarations (Node, Decls);
	    end if;
	 end if;
	 
	 Append (Decl, Decls);
	 Def_Id := Defining_Identifier (Decl);
	 Set_Scope (Def_Id, E);
	 C := Get_Name_Entity_Id (Chars (Def_Id));
	 if Present (C) then
	    Set_Homonym (Def_Id, C);
	 else
	    Set_Name_Entity_Id (Chars (Def_Id), Def_Id);
	 end if;
	 Append_Entity (Def_Id, E);

      elsif Nkind (Node) = N_Package_Body 
	or else Nkind (Node) = N_Subprogram_Body
      then
	 Decls := Declarations (Node);
	 if Is_Empty_List (Decls) then
	    Decls := New_List;
	    Set_Declarations (Node, Decls);
	 end if;
	 
	 Append (Decl, Decls);
	 Def_Id := Defining_Identifier (Decl);
	 Set_Scope (Def_Id, E);
	 if Nkind (Node) = N_Package_Body then
	    C := Get_Name_Entity_Id (Chars (Def_Id));
	    if Present (C) then
	       Set_Homonym (Def_Id, C);
	    else
	       Set_Name_Entity_Id (Chars (Def_Id), Def_Id);
	    end if;
	 end if;
	 Append_Entity (Def_Id, E);
	 
	 -- ??? TO DO
      elsif Nkind (Node) = N_Package_Body_Stub then
	 Decls := No_List;
      elsif Nkind (Node) = N_Subprogram_Body_Stub then
	 Decls := No_List;
      else
	 null;
      end if;
   end Declare_Current_Scope;
   
   ---------------------------------
   -- Declare_Label_Current_Scope --
   ---------------------------------
   
   procedure Declare_Label_Current_Scope 
     (This  : access Reflex_Expander_Record;
      Label : Node_Id) is
   begin
      null;
   end Declare_Label_Current_Scope;
   
   ------------------------------
   -- Get_Arec_Subprogram_List --
   ------------------------------
   
   function Get_Arec_Subprogram_List
     (This : access Reflex_Expander_Record) return Reflex.Entities_Lists.List is
   begin
      return This.Subprogram_Arec_List;
   end Get_Arec_Subprogram_List;
   
   ------------------------------
   -- Get_Arec_Subprogram_List --
   ------------------------------
   
   procedure Set_Arec_Subprogram_List
     (This : access Reflex_Expander_Record;
      L    :  Reflex.Entities_Lists.List) is
   begin
      This.Subprogram_Arec_List := L;
   end Set_Arec_Subprogram_List;
   
   ----------------------
   -- Append_Arec_List --
   ----------------------
   
   procedure Append_Arec_List
     (This : access Reflex_Expander_Record;
      E    : Entity_Id) is
   begin
      Reflex.Entities_Lists.Append (This.Subprogram_Arec_List, E);
   end Append_Arec_List;
   
end Reflex.Expanders;
