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

with Sinfo;  use Sinfo;
with Atree;  use Atree;
with Nlists; use Nlists;

with Reflex_Options;      use Reflex_Options;
with Reflex.Vertex_Value; use Reflex.Vertex_Value;
with Reflex.Fbd_Dispatch; use Reflex.Fbd_Dispatch;

package body Reflex.Fbd_Builders is
      
   -----------------
   -- New_Builder --
   -----------------
   
   function New_Builder return Fbd_Builder_Ptr is
      This : Fbd_Builder_Ptr := new Fbd_Builder_Record'(No_Fbd_Builder_Record);
      Vertex_Value : Vertex_Value_Ptr := New_Vertex_Value;
   begin
      This.Graph := New_Graph;
      This.Fbd_Emitor := New_Fbd_Emitor;

      Vertex_Value.Set_Node (This.Subp);
      Vertex_Value.Set_Vertex_Kind (Enclose_All_Vertex);
      This.Cell_Parent := 
        Create_Vertex (G        => This.Graph,
                       Parent   => null,
                       Id       => "enclose_all",
                       Value    => Vertex_Value,
                       X        => 0.0,
                       Y        => 0.0,
                       Width    => Float (Max_Unity_Fbd_Horizontal),
                       Height   => Float (Max_Unity_Fbd_Vertical));
      This.Graph.Set_Default_Parent (This.Cell_Parent);
      return This;
   end New_Builder;
   
   ------------------
   -- Free_Builder --
   ------------------
   
   procedure Free_Builder (This : in out Fbd_Builder_Ptr) is
   begin
      null;
   end Free_Builder;
   
   ---------------------
   -- Get_Cell_Parent --
   ---------------------
   
   function Get_Cell_Parent
     (This : access Fbd_Builder_Record) return access Cell_Record is
   begin
      return This.Cell_Parent;
   end Get_Cell_Parent;
   
   ---------------------
   -- Set_Cell_Parent --
   ---------------------
    
   procedure Set_Cell_Parent 
     (This : access Fbd_Builder_Record; Cell : access Cell_Record) is
   begin
      This.Cell_Parent := Cell;
   end Set_Cell_Parent;
  
   ---------------
   -- Get_Graph --
   ---------------
   
   function Get_Graph 
     (This : access Fbd_Builder_Record) return access Graph_Record is
   begin
      return This.Graph;
   end Get_Graph;
   
   ---------------
   -- Set_Graph --
   ---------------
   
   procedure Set_Graph 
     (This  : access Fbd_Builder_Record; 
      Graph : access Graph_Record) is
   begin
      This.Graph := Graph;
   end Set_Graph;
   
   --------------
   -- Get_Subp --
   --------------
   
   function Get_Subp (This : access Fbd_Builder_Record) return Node_Id is
   begin
      return This.Subp;
   end Get_Subp;
   
   --------------
   -- Set_Subp --
   --------------
   
   procedure Set_Subp
     (This : access Fbd_Builder_Record;
      Subp : Node_Id) is
   begin
      This.Subp := Subp;
   end Set_Subp;
   
   --------------------
   -- Get_Fbd_Emitor --
   --------------------
   
   function Get_Fbd_Emitor 
     (This : access Fbd_Builder_Record) return access Fbd_Emitor_Record is
   begin
      return This.Fbd_Emitor;
   end Get_Fbd_Emitor;
   
   --------------------
   -- Set_Fbd_Emitor --
   --------------------
    
   procedure Set_Fbd_Emitor 
     (This   : access Fbd_Builder_Record; 
      Emitor : access Fbd_Emitor_Record) is
   begin
      This.Fbd_Emitor := Emitor;
   end Set_Fbd_Emitor;
   
   -----------------
   -- Append_Node --
   -----------------
      
   procedure Append_Node 
     (This : access Fbd_Builder_Record; 
      Node  : Node_Id) is 
   begin
      Nodes_Lists.Append (This.Fbd_Node_List, Node);
   end Append_Node;
   
   ---------------
   -- Find_Node --
   ---------------
      
   function Find_Node  
     (This : access Fbd_Builder_Record; 
      Node  : Node_Id) return Node_Id is 
      
      Curr_List : Nodes_Lists.List := This.Fbd_Node_List;
   begin
      if Nkind (Node) /= N_Identifier then
         return Empty;
      end if;
      
      for N of This.Fbd_Node_List loop
         if Entity (Node) = Entity (N) then
            return N;
         end if;
      end loop;
      
      return Empty;
   end Find_Node;
      
   ---------------
   -- Build_Fbd --
   ---------------
   
   procedure Build_Fbd 
     (This : access Fbd_Builder_Record; 
      Node : Node_Id) is 
      
      Stmts : List_Id;
      Stmt  : Node_Id;
   begin      
      Stmts := Statements (Handled_Statement_Sequence (Node));
      if Is_Non_Empty_List (Stmts) then
         Stmt := First (Stmts);        
         while Present (Stmt) loop
            Fbd_Node_Dispatch (This, Stmt);
            Next (Stmt);
         end loop;
      end if;
   end Build_Fbd;
   
end Reflex.Fbd_Builders;
