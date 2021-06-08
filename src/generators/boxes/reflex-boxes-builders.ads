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

with Artics.Dynamic_Tables;
with Artics.Buffers; use Artics.Buffers;

with Unity.Gen;      use Unity.Gen;
with Reflex.Rungs;               use Reflex.Rungs;
with Reflex_Options;             use Reflex_Options;
with Reflex.Boxes.Ladder_Emitor; use Reflex.Boxes.Ladder_Emitor;

package Reflex.Boxes.Builders is
   
   type Builder_Record is tagged private;
   type Builder_Ptr is access all Builder_Record;
   type Builder_Class_Ptr is access all Builder_Record'Class;
   
   No_Builder_Record : constant Builder_Record;
   
   function New_Builder return Builder_Ptr;
   
   procedure Free_Builder (This : in out Builder_Ptr);
   
   function Get_Subp 
     (This : access Builder_Record) return Node_Id;
   procedure Set_Subp
     (This : access Builder_Record;
      Subp : Node_Id);
   
   function Get_Subp_Boxes
     (This : access Builder_Record) return Boxes_Lists.List;
   procedure Set_Subp_Boxes
     (This : access Builder_Record;
      L    : Boxes_Lists.List);
      
   function In_If_Statement 
     (This : access Builder_Record) return Boolean;
   procedure Set_In_If_Statement
     (This : access Builder_Record;
      V    : Boolean);
      
   function Get_Literal_Generator 
     (This : access Builder_Record) return access Unity_Generator_Record;
   procedure Set_Literal_Generator 
     (This : access Builder_Record;
      Gen  : access Unity_Generator_Record);
   
   function Get_Literal_Buffer
     (This : access Builder_Record) return Output_Buffer;
   procedure Set_Literal_Buffer
     (This : access Builder_Record;
      Ob   : Output_Buffer);
   
   function Get_Ladder_Emitor
     (This : access Builder_Record) return access Ladder_Emitor_Record;
   procedure Set_Ladder_Emitor
     (This : access Builder_Record;
      Ld   : access Ladder_Emitor_Record);
   
   procedure Open_Scope
     (This : access Builder_Record;
      N    : Node_Id := Empty);
   procedure Close_Scope 
     (This : access Builder_Record);
   
   procedure Append_Rung
     (This : access Builder_Record;
      R    : access Rung_Record);
      
   procedure Append_Box
     (This : access Builder_Record;
      B    : access Box_Record'Class);
   
   procedure Build_Rungs (This : access Builder_Record);

   procedure Build_Boxes 
     (This : access Builder_Record;
      Node : Node_Id);
     
   procedure Generate_Literal_Expression
     (This : access Builder_Record;
      Node : Node_Id);
   
   procedure Generate_Rungs (This : access Builder_Record);

   procedure Dump_Box
     (This : access Builder_Record;
      B    : access Box_Record'Class);
   
   procedure Dump_Matrix (This : access Builder_Record);
   procedure Dump_A_Matrix (Matrix : access Matrix_Record);
   
private
   
   type Scope_Stack_Entry is record
      Scope_Node : Node_Id;
      In_If_Statement : Boolean;
   end record;
   
   No_Scope_Stack_Entry : constant Scope_Stack_Entry :=
     (Scope_Node      => Empty,
      In_If_Statement => False);
   
   package Scope_Stack is new Artics.Dynamic_Tables
     (Table_Component_Type => Scope_Stack_Entry,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 128,
      Table_Increment      => 100);
   use Scope_Stack;
   
   type Builder_Record is tagged record
      Subp : Node_Id;
      Subp_Boxes : Boxes_Lists.List;
      
      Rungs : Rungs_Lists.List;
      
      Scopes : Scope_Stack.Instance;
      
      Ladder_Emitor : access Ladder_Emitor_Record;
      
      Literal_Generator : access Unity_Generator_Record;
      Literal_Buffer : Output_Buffer;
   end record;
   
   No_Builder_Record : constant Builder_Record :=
     (Subp              => Empty,
      Subp_Boxes        => Boxes_Lists.Empty_List,
      Rungs             => Rungs_Lists.Empty_List,
      Scopes            => Scope_Stack.No_Instance,
      Ladder_Emitor     => null,
      Literal_Generator => null,
      Literal_Buffer    => null);
   
end Reflex.Boxes.Builders;
