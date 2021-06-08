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

with Types; use Types;
with Artics.Dynamic_Tables;
with Nlists; use Nlists;

with Reflex.Gen.Dispatch_Interface; use Reflex.Gen.Dispatch_Interface;

package Reflex.Generators is
   
   type Generator_Record is new Generator_Dispatch_Interface with private; -- tagged private;
   type Generator_Ptr is access all Generator_Record;
   type Generator_Class_Ptr is access all Generator_Record'Class;
   
   No_Generator_Record : constant Generator_Record;
   
   function New_Generator return Generator_Ptr;
   
   procedure Initialize_Generator (This : in out Generator_Record);
   
   procedure Free_Generator (This : in out Generator_Ptr);
   
   function Get_Main_Node
     (This : access Generator_Record) return Node_Id;
   procedure Set_Main_Node
     (This : access Generator_Record;
      Node : Node_Id);
   
   procedure Create_Output_Buffer (This : access Generator_Record);
   
   procedure Delete_Output_Buffer (This : access Generator_Record);
   
   function Get_Output_Buffer
     (This : access Generator_Record) return Output_Buffer;
   procedure Set_Output_Buffer
     (This : access Generator_Record;
      Ob   : Output_Buffer);
   
   function Get_Current_Source_File
     (This : access Generator_Record) return Source_File_Index;
   procedure Set_Current_Source_File
     (This        : access Generator_Record;
      Source_File : Source_File_Index);
   --  Index of source file whose generated code is being dumped
   
   function Get_Full_Code_Generation
     (This : access Generator_Record) return Boolean;
   procedure Set_Full_Code_Generation
     (This : access Generator_Record;
      Gen  : Boolean);
   --  True if we should generate C code for all constructs. If False, only
   --  generate a C header for Ada specs.
   
   function Get_Last_Line_Printed
     (This : access Generator_Record) return Physical_Line_Number;
   procedure Set_Last_Line_Printed
     (This : access Generator_Record;
      Line : Physical_Line_Number);
   -- Last Comment Line output on buffer.
   
   function Get_Dump_Source_Comment
     (This : access Generator_Record) return Boolean;
   procedure Set_Dump_Source_Comment
     (This    : access Generator_Record;
      Comment : Boolean);
   --  Ouputs comments in generated file.
   
   function Get_Dump_Node
     (This : access Generator_Record) return Node_Id;
   procedure Set_Dump_Node
     (This : access Generator_Record;
      Node : Node_Id);
   --  This is set to the current node, used for printing line numbers
   
   function Get_In_Package_Body_Init
     (This : access Generator_Record) return Boolean;
   procedure Set_In_Package_Body_Init
     (This : access Generator_Record;
      Init : Boolean);
   --  Indicates whether the current node is located in the initialization 
   --  of a package body.
   
   procedure Open_Scope
     (This : access Generator_Record;
      E    : Entity_Id := Empty);
   --  Make new scope stack entry in the top of the scopes stack and output
   --  character '{' if With_Block is True. The new scope is enabled to
   --  start processing declarations; it must be disabled by the caller
   --  invoking the routine Set_In_Statements when it starts generating
   --  code for the statements of this scope.

   procedure Close_Scope (This : access Generator_Record);
   --  Remove from the top of the stack all the entries of inner extra
   --  scopes (if any) and the first non-extra scope. Output '}' for
   --  each closed scope that was opened with With_Block set to True.

   procedure Close_Scope
     (This    : access Generator_Record;
      Scop_Id : Natural);
   --  Remove from the top of the stack all the entries of inner extra
   --  scopes (if any) until the scope Scop_Id is removed from the stack.
   --  Output '}' for each closed scope that was opened with With_Blocks
   --  set to True.
   
   function Current_Scope_Id
     (This : access Generator_Record) return Natural;
   --  Return the id of the current scope
   
   function Get_Scope_Entity
     (This : access Generator_Record) return Entity_Id;
   procedure Set_Scope_Entity
     (This : access Generator_Record;
      E    : Entity_Id);
   --  The entity to which this scope belongs
   
   function Get_Declarative_List
     (This : access Generator_Record) return List_Id;
   
   procedure Set_Declarative_List
     (This : access Generator_Record;
      L    : List_Id);
   
   procedure Declare_Current_Scope 
     (This : access Generator_Record;
      Decl : Node_Id);
   
   procedure Do_Generation (This : access Generator_Record) is null;
   
   procedure Generate_And_List
     (This : access Generator_Record;
      List : List_Id);
   --  Print the given list with items separated by vertical "and"

   procedure Generate_Bar_List
     (This : access Generator_Record;
      List : List_Id);
   --  Print the given list with items separated by vertical bars

   function Generate_Comma_List
     (This : access Generator_Record;
      List : List_Id) return Integer;
   procedure Generate_Comma_List
     (This : access Generator_Record;
      List : List_Id);
   --  Prints the nodes in a list, with separating commas. If the list is empty
   --  then no output is generated.
   --  The function version returns the number of nodes printed.

   procedure Generate_Indented_List
     (This : access Generator_Record;
      List : List_Id);
   --  Like Generate_Line_List, except that the indentation level is increased
   --  before outputting the list of items, and then decremented (back to its
   --  original level) before returning to the caller.

   procedure Generate_Left_Opnd
     (This : access Generator_Record;
      N    : Node_Id);
   --  Print left operand of operator, parenthesizing if necessary. Note that
   --  we fully parenthesize operator trees in the C output.

   procedure Generate_Node_List
     (This      : access Generator_Record;
      List      : List_Id; 
      New_Lines : Boolean := False);
   --  Prints the nodes in a list with no separating characters. This is used
   --  in the case of lists of items which are printed on separate lines using
   --  the current indentation amount. New_Lines controls the generation of
   --  New_Line calls. If False, no New_Line calls are generated. If True,
   --  then New_Line calls are generated as needed to ensure that each list
   --  item starts at the beginning of a line.

   procedure Generate_Node_Paren
     (This : access Generator_Record;
      N    : Node_Id);
   --  Prints node, adding parentheses if N is an operator, or short circuit
   --  operation or other subexpression which needs parenthesizing as an
   --  operand (we always fully parenthesize expression trees in the C output).

   procedure Generate_Opt_Node
     (This : access Generator_Record;
      Node : Node_Id);
   --  Same as normal Generate_Node procedure, except that one leading blank is
   --  output before the node if it is non-empty.

   procedure Generate_Opt_Node_List
     (This : access Generator_Record;
      List : List_Id);
   --  Like Generate_Node_List, but prints nothing if List = No_List

   procedure Generate_Right_Opnd
     (This : access Generator_Record;
      N    : Node_Id);
   --  Print right operand of operator, parenthesizing if necessary. Note that
   --  we fully parenthesize operator trees in the C output.
   
   
private
   
   No_Scope_Id : Natural := 0;
   
   type Scope_Stack_Entry is record
      
      Extra_Scope : Boolean;
      
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
     (Extra_Scope      => False,
      Scope_Entity     => Empty,
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
   
   
   type Generator_Record is new Generator_Dispatch_Interface with record 
      Main_Node : Node_Id;
      -- The root node of the tree to generate
      
      Ob : Output_Buffer;
      -- Output buffer containing the whole program
      
      Current_Source_File : Source_File_Index;
      --  Index of source file whose generated code is being dumped
      
      Full_Code_Generation : Boolean;
      --  True if we should generate C code for all constructs. If False, only
      --  generate a C header for Ada specs.
      
      Dump_Source_Comment : Boolean;
      --  Ouputs comments in generated file.
      
      Dump_Node : Node_Id;
      --  This is set to the current node, used for printing line numbers
      
      In_Package_Body_Init : Boolean;
      --  Indicates whether the current node is located in the initialization 
      --  of a package body.
      
      Last_Line_Printed : Physical_Line_Number;
      -- Last Comment Line output on buffer.
      
      Scopes : Scope_Stack.Instance;
      
      Scope_Count : Natural;
   end record;
   
   No_Generator_Record : constant Generator_Record :=
     Generator_Record'
     (Main_Node            => Empty,
      Ob                   => null,
      Current_Source_File  => No_Source_File,
      Full_Code_Generation => False,
      Dump_Source_Comment  => True,
      Dump_Node            => Empty,
      In_Package_Body_Init => False,
      Last_Line_Printed    => Physical_Line_Number'First,
      Scopes               => Scope_Stack.No_Instance,
      Scope_Count          => 0);
   
end Reflex.Generators;
