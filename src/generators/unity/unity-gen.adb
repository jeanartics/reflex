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

with Unity.Gen.Dispatch;

with Reflex.Infos; use Reflex.Infos;
--  with Artics.Geometry;
--  with Artics.Geometry.Dimensions;
--  with Artics.Geometry.Points;
--  with Artics.Geometry.Lines;
--  with Artics.Geometry.Rectangles;
--  wIth Artics.Graph.Cells;
--  with Artics.Graph.Cells_Geometry;
--  with Artics.Graph.Cells_Paths;
--  with Artics.Graph.Models;
--  with Artics.Graph.Graphs;
--  with Artics.Graph.Svg_Canvas;

package body Unity.Gen is
   
   -------------------------
   -- New_Unity_Generator --
   -------------------------
   
   function New_Unity_Generator return Unity_Generator_Ptr is
      This : Unity_Generator_Ptr :=
        new Unity_Generator_Record'(No_Unity_Generator_Record);
   begin
      Create_Output_Buffer (This);
      
      This.Ob_Dummy      := New_Output_Buffer;
      This.Ob_Tasks      := New_Output_Buffer;
      This.Ob_Types      := New_Output_Buffer;
      This.Ob_Dfbs       := New_Output_Buffer;
      This.Ob_Vars       := New_Output_Buffer;
      This.Ob_Functional := New_Output_Buffer;
      This.Ob_Program    := New_Output_Buffer;
      This.Ob_Sections   := New_Output_Buffer;
      This.Ob_Sr         := New_Output_Buffer;
      This.Ob_Tmp_Prog   := New_Output_Buffer;
      
      This.Open_Scope (Empty);
      return This;
   end New_Unity_Generator;
   
   --------------------------
   -- Free_Unity_Generator --
   --------------------------
   
   procedure Free_Unity_Generator (This : in out Unity_Generator_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Unity_Generator_Record, Unity_Generator_Ptr);
   begin
      Delete_Output_Buffer (This);
      if This.Ob_Tasks /= null then
         Free_Buffer (This.Ob_Tasks);
      end if;
      if This.Ob_Types /= null then
         Free_Buffer (This.Ob_Types);
      end if;
      if This.Ob_Dfbs /= null then
         Free_Buffer (This.Ob_Dfbs);
      end if;
      if This.Ob_Vars /= null then
         Free_Buffer (This.Ob_Vars);
      end if;
      if This.Ob_Functional /= null then
         Free_Buffer (This.Ob_Functional);
      end if;
      if This.Ob_Program /= null then
         Free_Buffer (This.Ob_Program);
      end if;
      if This.Ob_Sections /= null then
         Free_Buffer (This.Ob_Sections);
      end if;
      if This.Ob_Sr /= null then
         Free_Buffer (This.Ob_Sr);
      end if;
      if This.Ob_Tmp_Prog /= null then
         Free_Buffer (This.Ob_Tmp_Prog);
      end if;
      
      Free (This);
   end Free_Unity_Generator;
   
   -----------------------------
   -- Get_Tasks_Output_Buffer --
   -----------------------------
   
   function Get_Tasks_Output_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer is
   begin
      return This.Ob_Tasks;
   end Get_Tasks_Output_Buffer;
   
   -----------------------------
   -- Set_Tasks_Output_Buffer --
   -----------------------------
   
   procedure Set_Tasks_Output_Buffer
     (This : access Unity_Generator_Record;
      Ob   : Output_Buffer) is
   begin
      This.Ob_Tasks := Ob;
   end Set_Tasks_Output_Buffer;
   
   -----------------------------
   -- Get_Types_Output_Buffer --
   -----------------------------
   
   function Get_Types_Output_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer is
   begin
      return This.Ob_Types;
   end Get_Types_Output_Buffer;
   
   -----------------------------
   -- Set_Types_Output_Buffer --
   -----------------------------
   
   procedure Set_Types_Output_Buffer
     (This : access Unity_Generator_Record;
      Ob   : Output_Buffer) is
   begin
      This.Ob_Types := Ob;
   end Set_Types_Output_Buffer;
   
   ----------------------------
   -- Get_Dfbs_Output_Buffer --
   ----------------------------
   
   function Get_Dfbs_Output_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer is
   begin
      return This.Ob_Dfbs;
   end Get_Dfbs_Output_Buffer;
   
   ----------------------------
   -- Set_Dfbs_Output_Buffer --
   ----------------------------
   
   procedure Set_Dfbs_Output_Buffer
     (This : access Unity_Generator_Record;
      Ob   : Output_Buffer) is
   begin
      This.Ob_Dfbs := Ob;
   end Set_Dfbs_Output_Buffer;
   
   ----------------------------
   -- Get_Vars_Output_Buffer --
   ----------------------------
   
   function Get_Vars_Output_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer is
   begin
      return This.Ob_Vars;
   end Get_Vars_Output_Buffer;
   
   ----------------------------
   -- Set_Vars_Output_Buffer --
   ----------------------------
   
   procedure Set_Vars_Output_Buffer
     (This : access Unity_Generator_Record;
      Ob   : Output_Buffer) is
   begin
      This.Ob_Vars := Ob;
   end Set_Vars_Output_Buffer;
   
   ----------------------------------
   -- Get_Functional_Output_Buffer --
   ----------------------------------
   
   function Get_Functional_Output_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer is
   begin
      return This.Ob_Functional;
   end Get_Functional_Output_Buffer;
   
   ----------------------------------
   -- Set_Functional_Output_Buffer --
   ----------------------------------
   
   procedure Set_Functional_Output_Buffer
     (This : access Unity_Generator_Record;
      Ob   : Output_Buffer) is
   begin
      This.Ob_Functional := Ob;
   end Set_Functional_Output_Buffer;
   
   -------------------------------
   -- Get_Program_Output_Buffer --
   -------------------------------
   
   function Get_Program_Output_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer is
   begin
      return This.Ob_Program;
   end Get_Program_Output_Buffer;
   
   -------------------------------
   -- Set_Program_Output_Buffer --
   -------------------------------
   
   procedure Set_Program_Output_Buffer
     (This : access Unity_Generator_Record;
      Ob   : Output_Buffer) is
   begin
      This.Ob_Program := Ob;
   end Set_Program_Output_Buffer;
      
   --------------------------------
   -- Get_Sections_Output_Buffer --
   --------------------------------
   
   function Get_Sections_Output_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer is
   begin
      return This.Ob_Sections;
   end Get_Sections_Output_Buffer;
   
   --------------------------------
   -- Set_Sections_Output_Buffer --
   --------------------------------
   
   procedure Set_Sections_Output_Buffer
     (This : access Unity_Generator_Record;
      Ob   : Output_Buffer) is
   begin
      This.Ob_Sections := Ob;
   end Set_Sections_Output_Buffer;
      
   --------------------------
   -- Get_Sr_Output_Buffer --
   --------------------------
   
   function Get_Sr_Output_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer is
   begin
      return This.Ob_Sr;
   end Get_Sr_Output_Buffer;
   
   --------------------------
   -- Set_Sr_Output_Buffer --
   --------------------------
   
   procedure Set_Sr_Output_Buffer
     (This : access Unity_Generator_Record;
      Ob   : Output_Buffer) is
   begin
      This.Ob_Sr := Ob;
   end Set_Sr_Output_Buffer;
   
   --------------------------------
   -- Get_Tmp_Prog_Output_Buffer --
   --------------------------------
   
   function Get_Tmp_Prog_Output_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer is
   begin
      return This.Ob_Sr;
   end Get_Tmp_Prog_Output_Buffer;
   
   --------------------------------
   -- Set_Tmp_Prog_Output_Buffer --
   --------------------------------
   
   procedure Set_Tmp_Prog_Output_Buffer
     (This : access Unity_Generator_Record;
      Ob   : Output_Buffer) is
   begin
      This.Ob_Sr := Ob;
   end Set_Tmp_Prog_Output_Buffer;
   
   -----------------------
   -- In_Fbd_Generation --
   -----------------------
   
   function In_Fbd_Generation
     (This : access Unity_Generator_Record) return Boolean is
      
      E : Entity_Id := This.Get_Scope_Entity;
   begin
      --  if Present (E) then
      --     Put_Line ("In_Fbd_Generation E = " & Get_Name_String (Chars (E)));
      --     Put_Line ("   Gen Type = " & Get_Generation_Type (E)'Img);
      --  else
      --     Put_Line ("In_Fbd_Generation no E");
      --  end if;
      return Present (E) 
        and then Ekind (E) in Subprogram_Kind
        and then (Get_Generation_Type (E) = Fb_Type
                  or else Get_Generation_Type (E) = Unknown_Generation);
   end In_Fbd_Generation;
   
   -------------------
   -- Do_Generation --
   -------------------
   
   procedure Do_Generation (This : access Unity_Generator_Record) is
   begin
      null;
   end Do_Generation;
   
   ------------------------
   -- Get_Current_Entity --
   ------------------------
   
   function Get_Current_Entity
     (This : access Unity_Generator_Record) return Entity_Id is
   begin
      return This.Current_Entity;
   end Get_Current_Entity;
   
   ------------------------
   -- Set_Current_Entity --
   ------------------------
   
   procedure Set_Current_Entity
     (This : access Unity_Generator_Record;
      E    : Entity_Id) is
   begin
      This.Current_Entity := E;
   end Set_Current_Entity;
   
   ------------------------------
   -- Get_Current_Types_Buffer --
   ------------------------------
   
   function Get_Current_Types_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer is
   begin
      return This.Get_Types_Output_Buffer;
   end Get_Current_Types_Buffer;
   
   -----------------------------
   -- Get_Current_Vars_Buffer --
   -----------------------------
   
   function Get_Current_Vars_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer is
   begin
      if This.In_Fbd_Generation then
         return This.Get_Dfbs_Output_Buffer;
      else
         return This.Get_Vars_Output_Buffer;
      end if;
   end Get_Current_Vars_Buffer;
   
   ----------------------------------
   -- Get_Current_Statments_Buffer --
   ----------------------------------
   
   function Get_Current_Statments_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer is
      
      E : Entity_Id := This.Current_Entity;
   begin
      if Present (E) and then Ekind (E) in Subprogram_Kind then
         case Get_Generation_Type (E) is
         when Section_Type =>
            return This.Get_Sections_Output_Buffer;
         when Sr_Type =>
            return This.Get_Sr_Output_Buffer;
         when Fb_Type
            | Unknown_Generation =>
            return This.Get_Dfbs_Output_Buffer;
         end case;
      else
         return null;
      end if;
   end Get_Current_Statments_Buffer;
   
   -------------------
   -- Generate_Node --
   -------------------
   
   procedure Generate_Node
     (This        : access Unity_Generator_Record;
      Node        : Node_Id; 
      Declaration : Boolean := False) is
   begin
      Unity.Gen.Dispatch.Generate_Node (This, Node, Declaration);
   end Generate_Node;
   
   ---------------------------------
   -- Generate_Literal_Expression --
   ---------------------------------
   
   procedure Generate_Literal_Expression
     (This : access Unity_Generator_Record;
      Node : Node_Id;
      Ob   : Output_Buffer) is
   begin
      Unity.Gen.Dispatch.Generate_Literal_Expression (This, Node, Ob);
   end Generate_Literal_Expression;
   
end Unity.Gen;
