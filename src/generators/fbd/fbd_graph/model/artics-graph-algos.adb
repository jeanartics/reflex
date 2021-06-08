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

with Ada.Text_IO; 

with Artics.Utils; use Artics.Utils;
with Reflex.Fbd_Util; use Reflex.Fbd_Util;

package body Artics.Graph.Algos is
   procedure Put_Line (S : String) is null; --  renames Ada.Text_IO.Put_Line;

   -------------------
   --  Unmark_Cells --
   -------------------
      
   procedure Unmark_Cells (Cells : Cells_Lists.List) is
   begin
      for Cell of Cells loop
         Cell.Set_Visited (False);
      end loop;
   end Unmark_Cells; 
   
   -----------------
   --  Top_Parent -- 
   -----------------
      
   function Top_Parent
     (Root : access Cell_Record'Class;
      Item : access Cell_Record'Class) return access Cell_Record'Class
   is
   begin
      if Item /= null then
         if Item.Get_Parent = null then
            return null;
         elsif Item.Get_Parent = Root then
            return Item;
         else
            return Top_Parent (Root, Item.Get_Parent);
         end if;   
      else
         return null;
      end if;
   end Top_Parent;
   
   -----------------
   --  Top_Parent -- 
   -----------------
      
   function Get_Englob
     (Root : access Cell_Record'Class;
      Item : access Cell_Record'Class) return access Cell_Record'Class
   is
      Parent : access Cell_Record;
   begin
      if Item /= null then
         Parent := Item.Get_Parent;
	 
         if Parent = null then
            return null;
	    
         elsif Parent.Get_Parent = Root then
            return Item;
         else
            return Get_Englob (Root, Item.Get_Parent);
         end if;   
      else
         return null;
      end if;
   end Get_Englob;
   
   --------------------------------
   --  Build_vertex_Dependencies --
   --------------------------------
   
   procedure Build_Vertex_Dependencies (Root : access Cell_Record'Class) is
      
      Childs  : Cells_Lists.List := Root.Get_Children_List;
      Top_Src : access Cell_Record'Class;
      Top_Trg : access Cell_Record'Class;
      Src     : access Cell_Record'Class;
      Trg     : access Cell_Record'Class;
   begin
      Reset_Vertex_Dependencies (Root);
      
      for Cell of Childs loop
	 
         if Cell.Is_Edge then
            Src := Cell.Get_Terminal (True);
            Trg := Cell.Get_Terminal (False);	    
	    
            Top_Src := Top_Parent (Root, Src);
            Top_Trg := Top_Parent (Root, Trg);
	    
            Append_Vertices_Forward  (Top_Src, Top_Trg);
            Append_Vertices_Backward (Top_Trg, Top_Src);
         end if;
      end loop;
   end Build_Vertex_Dependencies;
   
   -------------------------------
   -- Reset_Vertex_Dependencies --
   -------------------------------
   
   procedure Reset_Vertex_Dependencies (Root : access Cell_Record'Class) is
      
      Childs  : Cells_Lists.List := Root.Get_Children_List;
   begin
      for Cell of Childs loop
         if Cell.Is_Vertex then
            Cell.Set_Vertices_Forward (Cells_Lists.Empty_List);
            Cell.Set_Vertices_Backward (Cells_Lists.Empty_List);
         end if;
      end loop;
      
   end Reset_Vertex_Dependencies;
   
   -----------------------------
   -- Build_Terminal_Vertices --
   -----------------------------
   
   function Build_Terminal_Vertices
     (Cells : Cells_Lists.List) return Cells_Lists.List is
      
      Terminals : Cells_Lists.List;
   begin
      for Cell of Cells loop
         if Cell.Is_Vertex then
            if Cells_Lists.Is_Empty (Cell.Get_Vertices_Forward) then
               if not Terminals.Contains (Cell) then
                  Terminals.Append (Cell);
               end if;
            end if;
         end if;
      end loop;
      
      return Terminals;
   end Build_Terminal_Vertices;
   
   
   -----------------------------
   -- Build_Starting_Vertices --
   -----------------------------
   
   function Build_Starting_Vertices
     (Cells : Cells_Lists.List) return Cells_Lists.List is
      
      Startings : Cells_Lists.List;
   begin
      for Cell of Cells loop
         if Cell.Is_Vertex then
            if Cells_Lists.Is_Empty (Cell.Get_Vertices_Backward) then
               if not Startings.Contains (Cell) then
                  Startings.Append (Cell);
               end if;
            end if;
         end if;
      end loop;
      
      return Startings;
   end Build_Starting_Vertices;
   
   -----------------
   -- Remve_Cycle --
   -----------------
   
   procedure Remove_Cycle 
     (Cell  : access Cell_Record'Class;
      Stack : in out Cells_Lists.List)
   is
      Forwards : Cells_Lists.List;
   begin
      Cells_Lists.Append (Stack, Cell);
      
      Cell.Set_Edge_Mark (True);
      
      Forwards := Cell.Get_Vertices_Forward;
      for C of Forwards loop      
         if Cells_Lists.Contains (Stack, C) then
            Remove_Forward (Cell, C);
         else
            if Is_Vertex (C) and then not C.Get_Edge_Mark then
               Remove_Cycle (C, Stack);
            else
               null;
            end if;
         end if;
      end loop;      
      
      Cells_Lists.Delete_Last (Stack);
   end Remove_Cycle;
   
   ------------------
   -- Remove_Cycle --
   ------------------
   
   procedure Remove_Cycle (Cell : access Cell_Record'Class) is
      Stack  : Cells_Lists.List;
      Childs : Cells_Lists.List;
   begin
      Childs := Cell.Get_Children_List;
      
      -- Unmark all children cell
      
      for C of Childs loop
         C.Set_Edge_Mark (False);
      end loop;
      
      -- Remove cycles
      
      for C of Childs loop
         if Is_Vertex (C) and then not C.Get_Edge_Mark then
            Remove_Cycle (C, Stack);
         end if;
      end loop;
   end Remove_Cycle;
   
   ------------------------
   -- Inc_Forward_Layer --
   ------------------------
   
   procedure Inc_Forward_Layer 
     (Cell  : access Cell_Record'Class;
      Layer : Integer) is
      
      Cell_Layer : Integer;
      New_Layer  : Integer;
      Forwards   : Cells_Lists.List;
   begin
      
      Cell_Layer := Cell.Get_Layer;
	
      if Cell_Layer <= Layer then
         New_Layer := Layer + 1;
         Cell.Set_Layer (New_Layer);
	 
         Forwards := Cell.Get_Vertices_Forward;
         for F of Forwards loop
            if F /= Cell then
               Inc_Forward_Layer (F, New_Layer);
            end if;
         end loop;
      end if;
   end Inc_Forward_Layer;
   
   ---------------------------
   -- Compute_Layer_Forward --
   ---------------------------
   
   procedure Compute_Layer_Forward (Root : access Cell_Record'Class) is
      
      Childs   : Cells_Lists.List;
      Forwards : Cells_Lists.List;
      Layer    : Integer;
   begin
      Childs := Root.Get_Children_List;
      
      -- Reset layer of all childs
      
      for Child of Childs loop
         Child.Set_Layer (1);
      end loop;
      
      -- Compute layer of all childs
      
      for Child of Childs loop
         if Child.Is_Vertex and then Child.Get_Parent = Root then
            Forwards := Child.Get_Vertices_Forward;
            Layer := Child.Get_Layer;
            for F of Forwards loop
               Inc_Forward_Layer (F, Layer);
            end loop;
         end if;
      end loop;

   end Compute_Layer_Forward;
   
   -----------------------
   -- Build_Connex_Cell --
   -----------------------
   
   procedure Build_Connex_Cell
     (Cell   : access Cell_Record'Class;
      Connex : in out Cells_Lists.List) is
      
      Forwards  : Cells_Lists.List;
      Backwards : Cells_Lists.List;
   begin
      if Cell.Is_Visited  or not Cell.Is_Vertex then
         return;
      end if;
      
      Cell.Set_Visited (True);
      Connex.Append (Cell);
      
      Forwards := Cell.Get_Vertices_Forward;
      for F of Forwards loop
         Build_Connex_Cell (F, Connex);
      end loop;
      
      Backwards := Cell.Get_Vertices_Backward;
      for B of Backwards loop
         Build_Connex_Cell (B, Connex);
      end loop;
      
   end Build_Connex_Cell;
   
   -------------------------
   -- Build_Connex_Graphs --
   -------------------------
   
   function Build_Connex_Graphs
     (Cells : Cells_Lists.List) return Cells_Lists_To_Lists.List is
      
      Connex    : Cells_Lists_To_Lists.List;
      Component : Cells_Lists_Access;
   begin
      Unmark_Cells (Cells);
      
      for Cell of Cells loop
         if not Cell.Is_Visited and Cell.Is_Vertex then
            Component := new Cells_Lists.List;
            Build_Connex_Cell (Cell, Component.all);
            if not Component.all.Is_Empty then
               Connex.Append (Component);
            end if;
         end if;
      end loop;
      
      return Connex;
   end Build_Connex_Graphs;
   
end Artics.Graph.Algos;
