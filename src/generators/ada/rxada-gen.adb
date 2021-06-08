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

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Unchecked_Deallocation;

with Atree; use Atree;
with Sinfo; use Sinfo;
with Einfo; use Einfo;
with Namet; use Namet;

with RxAda.Gen.Dispatch;

with Reflex.Infos; use Reflex.Infos;
with Reflex.Visitor;
with Reflex.Simple_Accept;

package body Rxada.Gen is
   
   -------------------------
   -- New_Ada_Generator --
   -------------------------
   
   function New_Ada_Generator return Ada_Generator_Ptr is
      This : Ada_Generator_Ptr :=
        new Ada_Generator_Record'(No_Ada_Generator_Record);
   begin
      Create_Output_Buffer (This);
      This.Open_Scope (Empty);
      
      return This;
   end New_Ada_Generator;
   
   --------------------------
   -- Free_Ada_Generator --
   --------------------------
   
   procedure Free_Ada_Generator (This : in out Ada_Generator_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Ada_Generator_Record, Ada_Generator_Ptr);
   begin
      Delete_Output_Buffer (This);
      
      Free (This);
   end Free_Ada_Generator;
   
   -------------------
   -- Do_Generation --
   -------------------
   
   procedure Do_Generation (This : access Ada_Generator_Record) is
   begin
      null;
   end Do_Generation;
   
   -----------------------
   -- Get_Output_Buffer --
   -----------------------
   
   function Get_Output_Buffer
     (This : access Ada_Generator_Record) return Output_Buffer is
   begin
      return This.Ob;
   end Get_Output_Buffer;
   
   -----------------------
   -- Set_Output_Buffer --
   -----------------------
   
   procedure Set_Output_Buffer
     (This : access Ada_Generator_Record;
      Ob   : Output_Buffer) is
   begin
      This.Ob := Ob;
   end Set_Output_Buffer;
   
   ------------------------
   -- Get_Current_Entity --
   ------------------------
   
   function Get_Current_Entity
     (This : access Ada_Generator_Record) return Entity_Id is
   begin
      return This.Current_Entity;
   end Get_Current_Entity;
   
   ------------------------
   -- Set_Current_Entity --
   ------------------------
   
   procedure Set_Current_Entity
     (This : access Ada_Generator_Record;
      E    : Entity_Id) is
   begin
      This.Current_Entity := E;
   end Set_Current_Entity;
   
   -------------------
   -- Generate_Node --
   -------------------
   
   procedure Generate_Node
     (This        : access Ada_Generator_Record;
      Node        : Node_Id; 
      Declaration : Boolean := False) is
   begin
      RxAda.Gen.Dispatch.Generate_Node (This, Node, Declaration);
   end Generate_Node;
   
   ---------------------------------
   -- Generate_Literal_Expression --
   ---------------------------------
   
   procedure Generate_Literal_Expression
     (This : access Ada_Generator_Record;
      Node : Node_Id;
      Ob   : Output_Buffer) is
   begin
      RxAda.Gen.Dispatch.Generate_Literal_Expression (This, Node, Ob);
   end Generate_Literal_Expression;
   
end Rxada.Gen;
