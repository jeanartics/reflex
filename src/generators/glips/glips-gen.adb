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

with Glips.Gen.Dispatch;

with Reflex.Infos; use Reflex.Infos;

package body Glips.Gen is
   
   -------------------------
   -- New_Glips_Generator --
   -------------------------
   
   function New_Glips_Generator return Glips_Generator_Ptr is
      This : Glips_Generator_Ptr :=
        new Glips_Generator_Record'(No_Glips_Generator_Record);
   begin
      Create_Output_Buffer (This);
      This.Open_Scope (Empty);
      
      return This;
   end New_Glips_Generator;
   
   --------------------------
   -- Free_Glips_Generator --
   --------------------------
   
   procedure Free_Glips_Generator (This : in out Glips_Generator_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Glips_Generator_Record, Glips_Generator_Ptr);
   begin
      Delete_Output_Buffer (This);
      
      Free (This);
   end Free_Glips_Generator;
   
   -------------------
   -- Do_Generation --
   -------------------
   
   procedure Do_Generation (This : access Glips_Generator_Record) is
   begin
      null;
   end Do_Generation;
   
   -----------------------
   -- Get_Output_Buffer --
   -----------------------
   
   function Get_Output_Buffer
     (This : access Glips_Generator_Record) return Output_Buffer is
   begin
      return This.Ob;
   end Get_Output_Buffer;
   
   -----------------------
   -- Set_Output_Buffer --
   -----------------------
   
   procedure Set_Output_Buffer
     (This : access Glips_Generator_Record;
      Ob   : Output_Buffer) is
   begin
      This.Ob := Ob;
   end Set_Output_Buffer;
   
   ------------------------
   -- Get_Current_Entity --
   ------------------------
   
   function Get_Current_Entity
     (This : access Glips_Generator_Record) return Entity_Id is
   begin
      return This.Current_Entity;
   end Get_Current_Entity;
   
   ------------------------
   -- Set_Current_Entity --
   ------------------------
   
   procedure Set_Current_Entity
     (This : access Glips_Generator_Record;
      E    : Entity_Id) is
   begin
      This.Current_Entity := E;
   end Set_Current_Entity;
   
   -------------------
   -- Generate_Node --
   -------------------
   
   procedure Generate_Node
     (This        : access Glips_Generator_Record;
      Node        : Node_Id; 
      Declaration : Boolean := False) is
   begin
      Glips.Gen.Dispatch.Generate_Node (This, Node, Declaration);
   end Generate_Node;
   
   ---------------------------------
   -- Generate_Literal_Expression --
   ---------------------------------
   
   procedure Generate_Literal_Expression
     (This : access Glips_Generator_Record;
      Node : Node_Id;
      Ob   : Output_Buffer) is
   begin
      Glips.Gen.Dispatch.Generate_Literal_Expression (This, Node, Ob);
   end Generate_Literal_Expression;
   
end Glips.Gen;
