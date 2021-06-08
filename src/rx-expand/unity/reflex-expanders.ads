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

with Artics.Buffers; use Artics.Buffers;
with Artics.Dynamic_Tables;

with Types; use Types;
with Namet; use Namet;

with Reflex.Entities_Lists;
with Reflex.Infos; use Reflex.Infos;

package Reflex.Expanders is

   type Reflex_Expander_Record is tagged private;
   type Reflex_Expander_Ptr is access all Reflex_Expander_Record;
   type Reflex_Expander_Class_Ptr is access all Reflex_Expander_Record'Class;

   No_Reflex_Expander_Record : constant Reflex_Expander_Record;

   function New_Reflex_Expander return Reflex_Expander_Ptr;

   procedure Free_Reflex_Expander (This : in out Reflex_Expander_Ptr);

   function Get_Main_Node
     (This : access Reflex_Expander_Record) return Node_Id;
   procedure Set_Main_Node
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   function Get_Current_Source_File
     (This : access Reflex_Expander_Record) return Source_File_Index;
   procedure Set_Current_Source_File
     (This        : access Reflex_Expander_Record;
      Source_File : Source_File_Index);
   --  Index of source file whose generated code is being dumped

   function Get_Full_Code_Generation
     (This : access Reflex_Expander_Record) return Boolean;
   procedure Set_Full_Code_Generation
     (This : access Reflex_Expander_Record;
      Gen  : Boolean);
   --  True if we should generate C code for all constructs. If False, only
   --  generate a C header for Ada specs.

   function Get_In_Package_Body_Init
     (This : access Reflex_Expander_Record) return Boolean;
   procedure Set_In_Package_Body_Init
     (This : access Reflex_Expander_Record;
      Init : Boolean);
   --  Indicates whether the current node is located in the initialization
   --  of a package body.

   procedure Do_Expansion (This : access Reflex_Expander_Record);

   procedure Open_Scope
     (This : access Reflex_Expander_Record;
      E    : Entity_Id := Empty);
   --  Make new scope stack entry in the top of the scopes stack and output
   --  character '{' if With_Block is True. The new scope is enabled to
   --  start processing declarations; it must be disabled by the caller
   --  invoking the routine Set_In_Statements when it starts generating
   --  code for the statements of this scope.

   procedure Close_Scope (This : access Reflex_Expander_Record);
   --  Remove from the top of the stack all the entries of inner extra
   --  scopes (if any) and the first non-extra scope. Output '}' for
   --  each closed scope that was opened with With_Block set to True.

   procedure Close_Scope
     (This    : access Reflex_Expander_Record;
      Scop_Id : Natural);
   --  Remove from the top of the stack all the entries of inner extra
   --  scopes (if any) until the scope Scop_Id is removed from the stack.
   --  Output '}' for each closed scope that was opened with With_Blocks
   --  set to True.

   function Current_Scope_Id
     (This : access Reflex_Expander_Record) return Natural;
   --  Return the id of the current scope

   function In_Declarations
     (This : access Reflex_Expander_Record) return Boolean;
   procedure Set_In_Declarations
     (This : access Reflex_Expander_Record;
      V    : Boolean);
   --  Return True if we are processing the declarations of the scope in
   --  the top of the stack.

   function In_Statements
     (This : access Reflex_Expander_Record) return Boolean;

   procedure Set_In_Statements
     (This : access Reflex_Expander_Record;
      V    : Boolean);
   --  Remember in the top of the stack entry that we are processing its
   --  declarations.

   function Get_Declarative_List
     (This : access Reflex_Expander_Record) return List_Id;

   procedure Set_Declarative_List
     (This : access Reflex_Expander_Record;
      L    : List_Id);

   function Get_Scope_Entity
     (This : access Reflex_Expander_Record) return Entity_Id;

   procedure Set_Scope_Entity
     (This : access Reflex_Expander_Record;
      E    : Node_Id);

   function Get_Scope_Node
     (This : access Reflex_Expander_Record) return Node_Id;

   procedure Set_Scope_Node
     (This : access Reflex_Expander_Record;
      N    : Node_Id);

   procedure Declare_Current_Scope
     (This : access Reflex_Expander_Record;
      Decl : Node_Id);

   procedure Declare_Label_Current_Scope
     (This  : access Reflex_Expander_Record;
      Label : Node_Id);

   function In_Private_Part
     (This : access Reflex_Expander_Record) return Boolean;

   procedure Set_In_Private_Part
     (This : access Reflex_Expander_Record;
      V    : Boolean);

   function Make_Unique_Label
     (This  : access Reflex_Expander_Record;
      Name  : Name_Id) return Name_Id;
   function Make_Unique_Label_In_Scope
     (This  : access Reflex_Expander_Record;
      Name  : Name_Id;
      Count : Natural := 1) return Name_Id;

   function Make_Unique_Label_Entity
     (This  : access Reflex_Expander_Record;
      Loc   : Source_Ptr;
      Name  : Name_Id;
      Count : Natural := 0) return Node_Id;

   function Make_Unique_Name
     (This  : access Reflex_Expander_Record;
      Name  : Name_Id;
      Count : Natural := 0) return Name_Id;

   function Make_Unique_Entity
     (This : access Reflex_Expander_Record;
      Loc  : Source_Ptr;
      Name : Name_Id) return Entity_Id;

   function Make_Unique_Name_In_Scope
     (Name  : Name_Id;
      Scp   : Entity_Id;
      Count : Natural := 0) return Name_Id;

   function Make_Unique_Entity_In_Scope
     (Loc  : Source_Ptr;
      Name : Name_Id;
      Scp  : Entity_Id) return Entity_Id;

   function Search_Reuse_Entity_Old
     (This      : access Reflex_Expander_Record;
      Name      : Name_Id;
      Name_Type : Entity_Id) return Entity_Id;

   function Search_Reuse_Entity
     (This      : access Reflex_Expander_Record;
      Name      : Name_Id;
      Name_Type : Entity_Id;
      Count     : Natural := 0) return Entity_Id;

   function Make_Reuse_Entity
     (This      : access Reflex_Expander_Record;
      Loc       : Source_Ptr;
      Name      : Name_Id;
      Name_Type : Entity_Id) return Entity_Id;

   function Get_Arec_Subprogram_List
     (This : access Reflex_Expander_Record) return Reflex.Entities_Lists.List;

   procedure Set_Arec_Subprogram_List
     (This : access Reflex_Expander_Record;
      L    :  Reflex.Entities_Lists.List);

   procedure Append_Arec_List
     (This : access Reflex_Expander_Record;
      E    : Entity_Id);

private
   type Scope_Stack_Entry is record

      Scope_Entity : Entity_Id;
      --  The entity to witch this scope belongs

      Scope_Node : Node_Id;
      --  The entity to witch this scope belongs

      In_Declarations : Boolean;
      --  True when we are processing declarations of this scope

      In_Statements : Boolean;
      --  True when we are processing statements of this scope

      Private_Part : Boolean;

      Declarative_List : List_Id;
   end record;

   No_Scope_Stack_Entry : constant Scope_Stack_Entry :=
     (Scope_Entity     => Empty,
      Scope_Node       => Empty,
      In_Declarations  => False,
      In_Statements    => False,
      Private_Part     => False,
      Declarative_List => No_List);

   package Scope_Stack is new Artics.Dynamic_Tables
     (Table_Component_Type => Scope_Stack_Entry,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 128,
      Table_Increment      => 100);

   type Reflex_Expander_Record is tagged record
      Main_Node : Node_Id;
      --  The root node of the tree to generate

      Current_Source_File : Source_File_Index;
      --  Index of source file whose generated code is being dumped

      Full_Code_Generation : Boolean;
      --  True if we should generate C code for all constructs. If False, only
      --  generate a C header for Ada specs.

      In_Package_Body_Init : Boolean;
      --  Indicates whether the current node is located in the initialization
      --  of a package body.

      Scopes : Scope_Stack.Instance;

      Scope_Count : Natural;

      Subprogram_Arec_List : Reflex.Entities_Lists.List;
   end record;


   No_Reflex_Expander_Record : constant Reflex_Expander_Record :=
     Reflex_Expander_Record'
       (Main_Node            => Empty,
        Current_Source_File  => No_Source_File,
        Full_Code_Generation => False,
        In_Package_Body_Init => False,
        Scopes               => Scope_Stack.No_Instance,
        Scope_Count          => 0,
        Subprogram_Arec_List => Reflex.Entities_Lists.Empty_List);

end Reflex.Expanders;
