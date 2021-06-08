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

with Ada.Unchecked_Deallocation;

package body Reflex.Scopes_Stacks is
   
   ----------------
   -- Initialize --
   ----------------
   
   procedure Initialize is
   begin
      null;
   end Initialize;
   
   ---------------------
   -- New_Scope_Stack --
   ---------------------
   
   function New_Scope_Stack return Scope_Stack_Id is
      Inst : Scope_Stack.Instance;
   begin
      Scope_Stack.Init (Inst);
      return Scope_Stack_Id (Inst);
   end New_Scope_Stack;
   
   ----------------
   -- Open_Scope --
   ----------------

   procedure Open_Scope
     (Stack : in out Scope_Stack_Id;
      N     : Node_Or_Entity_Id := Empty) is
   begin
      Increment_Last (Stack);
      
      Stack.Table (Last (Stack)) := No_Scope_Stack_Entry;
      
      Stack.Table (Last (Stack)).Scope_Node := N;
      Stack.Table (Last (Stack)).Declarative_List := new Nodes_Lists.List;
   end Open_Scope;

   -----------------
   -- Close_Scope --
   -----------------

   procedure Close_Scope
     (Stack : in out Scope_Stack_Id) is
      
      L : Nodes_Lists.List;
   begin
      if Stack.Table (Last (Stack)).Declarative_List /= null then
	 L := Stack.Table (Last (Stack)).Declarative_List.all;
	 Nodes_Lists.Clear (L);
	 Free_List (Stack.Table (Last (Stack)).Declarative_List);
      end if;
      Decrement_Last (Stack);
   end Close_Scope;

   ----------------------
   -- Open_Extra_Scope --
   ----------------------

   procedure Open_Extra_Scope (Stack : in out Scope_Stack_Id) is
   begin
      Increment_Last (Stack);
      
      Stack.Table (Last (Stack)) := No_Scope_Stack_Entry;
      Stack.Table (Last (Stack)).Extra_Scope := True;
      Stack.Table (Last (Stack)).Declarative_List := new Nodes_Lists.List;
   end Open_Extra_Scope;
   
   -----------------------
   -- Close_Extra_Scope --
   -----------------------

   procedure Close_Extra_Scope (Stack : in out Scope_Stack_Id) is
   begin
      if Stack.Table (Last (Stack)).Declarative_List /= null then
	 Free_List (Stack.Table (Last (Stack)).Declarative_List);
      end if;
      Decrement_Last (Stack);
   end Close_Extra_Scope;
 
   ---------------------------
   -- Close_All_Extra_Scope --
   ---------------------------

   procedure Close_All_Extra_Scope (Stack : in out Scope_Stack_Id) is
      
      Lst : Scope_Entry_Id;
   begin
      loop
	 Lst := Last (Stack);
	 exit when not Stack.Table (Last (Stack)).Extra_Scope;
	 Close_Extra_Scope (Stack);
      end loop;
   end Close_All_Extra_Scope;
   
   ----------------------
   -- Get_Scope_Entity --
   ----------------------

   function Get_Scope_Entity
     (Stack : Scope_Stack_Id) return Entity_Id is
   begin
      return Stack.Table (Last (Stack)).Scope_Node;
   end Get_Scope_Entity;

   ----------------------
   -- Set_Scope_Entity --
   ----------------------

   procedure Set_Scope_Entity
     (Stack : Scope_Stack_Id;
      E     : Entity_Id) is
   begin
      Stack.Table (Last (Stack)).Scope_Node := E;
   end Set_Scope_Entity;
   
   --------------------------
   -- Get_Declarative_List --
   --------------------------
   
   function Get_Declarative_List
     (Stack : Scope_Stack_Id) return Nodes_Lists.List is
   begin
      return Stack.Table (Last (Stack)).Declarative_List.all;
   end Get_Declarative_List;
   
   --------------------------
   -- Set_Declarative_List --
   --------------------------
   
   procedure Set_Declarative_List
     (Stack : Scope_Stack_Id;
      L     : Nodes_Lists.list) is
   begin
      Stack.Table (Last (Stack)).Declarative_List.all := L;
   end Set_Declarative_List;
   
   ---------------------
   -- Get_Scope_Infos --
   ---------------------
   
   function Get_Scope_Infos
     (Stack : Scope_Stack_Id) return Scope_Infos_Type is
   begin
      return Stack.Table (Last (Stack)).Scope_Infos;
   end Get_Scope_Infos;
   
   ---------------------
   -- Set_Scope_Infos --
   ---------------------
   
   procedure Set_Scope_Infos
     (Stack  : Scope_Stack_Id;
      Infos  : Scope_Infos_Type) is
   begin
      Stack.Table (Last (Stack)).Scope_Infos := Infos;
   end Set_Scope_Infos;
   
   ----------------------------
   -- Get_Entity_Scope_Infos --
   ----------------------------
   
   function Get_Entity_Scope_Infos
     (Stack : Scope_Stack_Id) return Scope_Infos_Type is
      
      Lst : Scope_Entry_Id;
   begin
      loop
	 Lst := Last (Stack);
	 exit when not Stack.Table (Lst).Extra_Scope;
      end loop;
      return Stack.Table (Lst).Scope_Infos;
   end Get_Entity_Scope_Infos;
   
   ----------------------------
   -- Set_Entity_Scope_Infos --
   ----------------------------
   
   procedure Set_Entity_Scope_Infos
     (Stack  : Scope_Stack_Id;
      Infos  : Scope_Infos_Type) is
      
      lst : Scope_Entry_Id;
   begin
      loop
	 Lst := Last (Stack);
	 exit when not Stack.Table (Lst).Extra_Scope;
      end loop;
      Stack.Table (lst).Scope_Infos := Infos;
   end Set_Entity_Scope_Infos;
   
   ---------------------------
   -- Declare_Current_Scope --
   ---------------------------
   
   procedure Declare_Current_Scope 
     (Stack : Scope_Stack_Id;
      Decl  : Node_Id) is
      
      S   : Scope_Stack_Id := Stack;
      Lst : Scope_Entry_Id;
   begin
      loop
	 Lst := Last (S);
	 exit when not Stack.Table (Lst).Extra_Scope;
	 Decrement_Last (S);
      end loop;
      
      Nodes_Lists.Append (Stack.Table (Lst).Declarative_List.all, Decl);
   end Declare_Current_Scope;
   
   ---------------------------
   -- Declare_Current_Scope --
   ---------------------------
   
   procedure Declare_Before_Current_Scope 
     (Stack : Scope_Stack_Id;
      Decl  : Node_Id) is
      
      S   : Scope_Stack_Id := Stack;
      Lst : Scope_Entry_Id;
   begin
      loop
	 Lst := Last (S);
	 exit when not Stack.Table (Lst).Extra_Scope;
	 Decrement_Last (S);
      end loop;
      
	Nodes_Lists.Append (Stack.Table (Lst).Declarative_List.all, Decl);
   end Declare_Before_Current_Scope;
   
end Reflex.Scopes_Stacks;
