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
-- Reflex is originally developed by Artics
------------------------------------------------------------------------------

with Artics.Graph.Graphs;       use Artics.Graph.Graphs;
with Artics.Graph.Cells_States; use Artics.Graph.Cells_States;
with Artics.Geometry;           use Artics.Geometry;

with Sinfo; use Sinfo;

with Reflex.Vertex_Value; use Reflex.Vertex_Value;
with Reflex.Fbd_Dispatch; use Reflex.Fbd_Dispatch;
with Reflex.Infos;        use Reflex.Infos;
with Reflex.Fbd_Util;     use Reflex.Fbd_Util;

package body Reflex.Vertex_Value is
   
   ----------------------
   -- New_Vertex_Value --
   ----------------------
   
   function New_Vertex_Value return Vertex_Value_Ptr is
      This : Vertex_Value_Ptr := 
        new Vertex_Value_Record'(No_Vertex_Value_Record);
   begin
      return This;
   end New_Vertex_Value;
   
   -----------------------
   -- Free_Vertex_Value --
   -----------------------
   
   procedure Free_Vertex_Value (This : in out Vertex_Value_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Vertex_Value_Record, Vertex_Value_Ptr);
   begin
      if This /= null then
         Free (This);
      end if;
   end Free_Vertex_Value;
   
   --------------
   -- Get_Node --
   --------------
   
   function Get_Node (This : access Vertex_Value_Record) return Node_Id is
   begin
      return This.Node;
   end Get_Node;

   --------------
   -- Set_Node --
   --------------
   
   procedure Set_Node (This : access Vertex_Value_Record; Node : Node_Id) is
   begin
      This.Node := Node;
   end Set_Node;
   
   -------------------
   -- Get_Vertex_Kind --
   -------------------
   
   function Get_Vertex_Kind
     (This : access Vertex_Value_Record) return Vertex_Kind is
   begin
      return This.Kind;
   end Get_Vertex_Kind;
   
   -------------------
   -- Set_Vertex_Kind --
   -------------------
   
   procedure Set_Vertex_Kind
     (This : access Vertex_Value_Record; 
      Vertex : Vertex_Kind) is
   begin
      This.Kind := Vertex;
   end Set_Vertex_Kind;
   
   --------------------------
   -- Get_Has_Multiple_Out --
   --------------------------
    
   function  Get_Has_Multiple_Out 
     (This : access Vertex_Value_Record) return Boolean is
   begin
      return This.Has_Multiple_Out;
   end Get_Has_Multiple_Out;
   
   --------------------------
   -- Set_Has_Multiple_Out --
   --------------------------
    
   procedure Set_Has_Multiple_Out 
     (This : access Vertex_Value_Record; 
      Bool : Boolean) is
   begin
      This.Has_Multiple_Out := Bool;
   end Set_Has_Multiple_Out;
   
   ------------------------
   -- Get_Is_Negate_Vertex --
   ------------------------
   
   function Get_Is_Negate_Vertex
     (This : access Vertex_Value_Record) return Boolean is
   begin
      return This.Is_Negate_Vertex;
   end Get_Is_Negate_Vertex;
      
   ------------------------
   -- Set_Is_Negate_Vertex --
   ------------------------
   
   procedure Set_Is_Negate_Vertex 
     (This : access Vertex_Value_Record; 
      Bool : Boolean) is
   begin
      This.Is_Negate_Vertex := Bool;
   end Set_Is_Negate_Vertex;
   
   ------------------
   -- Get_Vertexs_In --
   ------------------
   
   function Get_Vertexs_In 
     (This : access Vertex_Value_Record) return Cells_Lists.List is
   begin
      return This.Vertexs_In;
   end Get_Vertexs_In;
   
   ------------------
   -- Set_Vertexs_In --
   ------------------
   
   procedure Set_Vertexs_In
     (This   : access Vertex_Value_Record;
      Vertexs : Cells_Lists.List) is
   begin
      This.Vertexs_Out := Vertexs;
   end Set_Vertexs_In;
   
   -------------------
   -- Get_Vertexs_Out --
   -------------------
   
   function Get_Vertexs_Out
     (This : access Vertex_Value_Record) return Cells_Lists.List is
   begin
      return This.Vertexs_Out;
   end Get_Vertexs_Out;
   
   -------------------
   -- Set_Vertexs_out --
   -------------------
   
   procedure Set_Vertexs_Out
     (This   : access Vertex_Value_Record;
      Vertexs : Cells_Lists.List) is
   begin
      This.Vertexs_Out := Vertexs;
   end Set_Vertexs_Out;
  
   ---------------------
   -- Append_Vertex_In --
   ---------------------
      
   procedure Append_Vertex_In
     (This : access Vertex_Value_Record;
      Vertex : access Cell_Record) is
   begin
      Cells_Lists.Append (This.Vertexs_In, Cell_Class_Ptr (Vertex));
   end Append_Vertex_In;
   
   -----------------------
   -- Append_Vertex_Out --
   -----------------------
      
   procedure Append_Vertex_Out
     (This : access Vertex_Value_Record;
      Vertex : access Cell_Record) is
   begin
      Cells_Lists.Append (This.Vertexs_Out, Cell_Class_Ptr (Vertex));
   end Append_Vertex_Out;
   
   ----------------------
   -- Get_Cell_To_Link --
   ----------------------
   
   function Get_Cell_To_Link 
     (This : access Vertex_Value_Record) return access Cell_Record is
   begin
      return This.Cell_To_Link;
   end Get_Cell_To_Link;
   
   ----------------------
   -- Set_Cell_To_Link --
   ----------------------
   
   procedure Set_Cell_To_Link
     (This : access Vertex_Value_Record;
      Cell : access Cell_Record) is
   begin
      This.Cell_To_Link := Cell;
   end Set_Cell_To_Link;
   
   -------------------------
   -- Get_Effective_Param --
   -------------------------
   
   function Get_Effective_Param
     (This : access Vertex_Value_Record) return Node_Id is
   begin
      return This.Effective_Param;
   end Get_Effective_Param;
   
   -------------------------
   -- Set_Effective_Param --
   -------------------------
   
   procedure Set_Effective_Param
     (This : access Vertex_Value_Record;
      Node : Node_Id) is
   begin
      This.Effective_Param := Node;
   end Set_Effective_Param;
  
   ---------------------
   -- Get_Formal_Name --
   ---------------------
    
   function Get_Formal_Name
     (This : access Vertex_Value_Record) return Name_Id is 
   begin
      return This.Formal_Name;
   end Get_Formal_Name;
   
   --------------------
   -- Set_Formal_Name --
   --------------------
    
   procedure Set_Formal_Name
     (This   : access Vertex_Value_Record;
      String : Name_Id) is 
   begin
      This.Formal_Name := String;
   end Set_Formal_Name;
   
end Reflex.Vertex_Value;
