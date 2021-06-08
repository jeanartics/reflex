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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation; 

with Artics.Buffers; use Artics.Buffers;

with Types; use Types;
with Artics.Dynamic_Tables;

generic
   type Scope_Infos_Type is private;
   No_Scope_Infos : Scope_Infos_Type;
   
package Reflex.Scopes_Stacks is
   
   type Scope_Stack_Id is private;
   
   No_Scope_Stack_Id : constant Scope_Stack_Id;
   
   subtype Scope_Entry_Id is Natural;
   
   procedure Initialize;
   
   function New_Scope_Stack return Scope_Stack_Id;
   --  Create a new scope stack.
   
   procedure Open_Scope
     (Stack : in out Scope_Stack_Id;
      N     : Node_Or_Entity_Id := Empty);
   --  Make new scope stack entry in the top of the scopes stack and output
   --  character '{' if With_Block is True. The new scope is enabled to
   --  start processing declarations; it must be disabled by the caller
   --  invoking the routine Set_In_Statements when it starts generating
   --  code for the statements of Stack scope.

   procedure Close_Scope (Stack : in out Scope_Stack_Id);
   --  Remove from the top of the stack all the entries of inner extra
   --  scopes (if any) and the first non-extra scope. Output '}' for
   --  each closed scope that was opened with With_Block set to True.
   
   procedure Open_Extra_Scope (Stack : in out Scope_Stack_Id);
   procedure Close_Extra_Scope (Stack : in out Scope_Stack_Id);
   procedure Close_All_Extra_Scope (Stack : in out Scope_Stack_Id);
   
   function Get_Scope_Entity
     (Stack : Scope_Stack_Id) return Entity_Id;
   procedure Set_Scope_Entity
     (Stack : Scope_Stack_Id;
      E     : Entity_Id);
   --  The entity to which Stack scope belongs
   
   function Get_Declarative_List
     (Stack : Scope_Stack_Id) return Nodes_Lists.List;
   
   procedure Set_Declarative_List
     (Stack : Scope_Stack_Id;
      L     : Nodes_Lists.list);
   
   function Get_Scope_Infos (Stack : Scope_Stack_Id) return Scope_Infos_Type;
   procedure Set_Scope_Infos
     (Stack : Scope_Stack_Id;
      Infos : Scope_Infos_Type);
   
   procedure Declare_Current_Scope 
     (Stack : Scope_Stack_Id;
      Decl  : Node_Id);
   
private
   
   type List_Ptr is access Nodes_Lists.List;
   
   procedure Free_List is new Ada.Unchecked_Deallocation
     (Nodes_Lists.List, List_Ptr);
   
   type Scope_Stack_Entry is record
      
      Extra_Scope : Boolean;
      
      Scope_Node : Node_Or_Entity_Id;
      --  The entity to witch Stack scope belongs
      
      Declarative_List : List_Ptr; -- Access Nodes_Lists.List;
      
      Scope_Infos : Scope_Infos_Type;
   end record;
   
   No_Scope_Stack_Entry : constant Scope_Stack_Entry :=
     (Extra_Scope      => False,
      Scope_Node       => Empty,
      Declarative_List => null, -- Nodes_Lists.Empty_List,
      Scope_Infos      => No_Scope_Infos);
   
   package Scope_Stack is new Artics.Dynamic_Tables
     (Table_Component_Type => Scope_Stack_Entry,
      Table_Index_Type     => Scope_Entry_Id,
      Table_Low_Bound      => 1,
      Table_Initial        => 128,
      Table_Increment      => 100);
   use Scope_Stack;
   
   type Scope_Stack_Id is new Scope_Stack.Instance;
   No_Scope_Stack_Id : constant Scope_Stack_Id 
     := Scope_Stack_Id (Scope_Stack.No_Instance);
   
end Reflex.Scopes_Stacks;
