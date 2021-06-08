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

with Artics.Utils;
with Artics.Named_Object;
with Artics.Graph.Writers.Dumps_Graphs;
use Artics.Graph.Writers.Dumps_Graphs;

package body Artics.Graph.Cells.Analysis is   
   
   procedure Put_Line (S : String) is null; -- renames Ada.Text_IO.Put_Line;

           
   ---------------------
   --   Is_In_Vertex  --
   ---------------------
   
   function Is_In_Vertex (Cell : access Cell_Record'Class;
                          L    : Cells_Lists.List) return Boolean
   is
      pragma Assert (Cell /= null and then Cell.Is_Vertex);
      
      use Cells_Lists;
      
      Cur : Cells_Lists.Cursor;
      Crt : access Cell_Record'Class;      
      Crt_Trg : access Cell_Record'Class;
   begin
      if not Cells_Lists.Is_Empty (L) then
         
         Cur := Cells_Lists.First (L);
         while Cells_Lists.Has_Element (Cur) loop            
            Crt := Cells_Lists.Element (Cur);
            
            if Crt /= null and then Crt.Is_Edge then
               Crt_Trg := Crt.Get_Terminal (False);
               
               if Crt_Trg = Cell then
                  return False;
               end if;   
            end if;

            Cells_Lists.Next (Cur);
         end loop;
      end if;
      
      return True;
   end Is_In_Vertex;

   ----------------------
   --   Is_Out_Vertex  --
   ----------------------
   
   function Is_Out_Vertex (Cell : access Cell_Record'Class;
                           L    : Cells_Lists.List) return Boolean
   is
      pragma Assert (Cell /= null and then Cell.Is_Vertex);
      
      use Cells_Lists;
      
      Cur : Cells_Lists.Cursor;
      Crt : access Cell_Record'Class;      
      Crt_Src : access Cell_Record'Class;
   begin
      if not Cells_Lists.Is_Empty (L) then
         
         Cur := Cells_Lists.First (L);
         while Cells_Lists.Has_Element (Cur) loop            
            Crt := Cells_Lists.Element (Cur);
            
            if Crt /= null and then Crt.Is_Edge then
               Crt_Src := Crt.Get_Terminal (True);
               
               if Crt_Src = Cell then
                  return False;
               end if;   
            end if;

            Cells_Lists.Next (Cur);
         end loop;
      end if;
      
      return True;
   end Is_Out_Vertex;

   
   ----------------------------------------
   --  First_Unvisited_In_Terminal_Cell  --
   ----------------------------------------
      
   function First_Unvisited_In_Terminal_Cell 
     (CL : Cells_Lists.List) return access Cell_Record'Class
   is
      Cur  : Cells_Lists.Cursor;
      Cell : access Cell_Record'Class;
   begin
      Cur := Cells_Lists.First (CL);
      while Cells_Lists.Has_Element (Cur) loop
         Cell := Cells_Lists.Element (Cur);
         if Cell.Is_Vertex and then not Cell.Is_Visited 
           and then Is_In_Vertex (Cell, CL) 
         then
            return Cell;
         end if;
            
         Cells_Lists.Next (Cur);
      end loop;
         
      return null;
   end First_Unvisited_In_Terminal_Cell;   
   
   
   ---------------------------------------
   --  Next_Unvisited_In_Terminal_Cell  --
   ---------------------------------------
   
   function Next_Unvisited_In_Terminal_Cell 
     (CL : Cells_Lists.List; 
      Cell : access Cell_Record'Class) return access Cell_Record'Class
   is
      pragma Assert (Cell /= null);
      
      Cur  : Cells_Lists.Cursor;      
      Crt  : access Cell_Record'Class;
   begin
      if not Cells_Lists.Is_Empty (CL) 
        and then Cells_Lists.Contains (CL, Cell) 
      then         
         --  position cursor on Cell 
         Cur := Cells_Lists.First (CL);
         while Cells_Lists.Has_Element (Cur) loop
            Crt := Cells_Lists.Element (Cur);
            
            if Crt.Is_Vertex and then Crt = Cell then  
               exit;
            end if;
            
            Cells_Lists.Next (Cur);
         end loop;

         --  step up to first cell after Cell and find the next unvisited in
         --  terminal
         Cells_Lists.Next (Cur);         
         while Cells_Lists.Has_Element (Cur) loop
            Crt := Cells_Lists.Element (Cur);
            if Crt.Is_Vertex and then not Crt.Is_Visited 
              and then Is_In_Vertex (Crt, CL) 
            then
               return Crt;
            end if;
            
            Cells_Lists.Next (Cur);
         end loop;
      end if;

      return null;
   end Next_Unvisited_In_Terminal_Cell;  
   
   
   ----------------------------
   --  First_Unvisited_Cell  --
   ----------------------------
      
   function First_Unvisited_Cell 
     (CL : Cells_Lists.List) return access Cell_Record'Class
   is
      Cur  : Cells_Lists.Cursor;
      Cell : access Cell_Record'Class;
   begin
      Cur := Cells_Lists.First (CL);
      while Cells_Lists.Has_Element (Cur) loop
         Cell := Cells_Lists.Element (Cur);
            
         if Cell.Is_Vertex and then not Cell.Is_Visited then
            return Cell;
         end if;
            
         Cells_Lists.Next (Cur);
      end loop;
         
      return null;
   end First_Unvisited_Cell;   
   
   
   ---------------------------
   --  Next_Unvisited_Cell  --
   ---------------------------
   
   function Next_Unvisited_Cell 
     (CL   : Cells_Lists.List;
      Cell : access Cell_Record'Class) return access Cell_Record'Class
   is
      pragma Assert (Cell /= null);
      
      Cur  : Cells_Lists.Cursor;      
      Crt  : access Cell_Record'Class;
   begin
      if not Cells_Lists.Is_Empty (CL) 
        and then Cells_Lists.Contains (CL, Cell) 
      then         
         --  position cursor on Cell 
         Cur := Cells_Lists.First (CL);
         while Cells_Lists.Has_Element (Cur) loop
            Crt := Cells_Lists.Element (Cur);
            
            if Crt.Is_Vertex and then Crt = Cell then  
               exit;
            end if;
            
            Cells_Lists.Next (Cur);
         end loop;

         --  step up to first cell after Cell and find the next unvisited in
         --  terminal
         Cells_Lists.Next (Cur);         
         while Cells_Lists.Has_Element (Cur) loop
            Crt := Cells_Lists.Element (Cur);
            if Crt.Is_Vertex and then not Crt.Is_Visited then
               return Crt;
            end if;
            
            Cells_Lists.Next (Cur);
         end loop;
      end if;

      return null;
   end Next_Unvisited_Cell;  
      
      
   -------------------------
   --  Add_Parent_Vertex  --
   -------------------------
      
   procedure Add_Parent_Vertex 
     (L     : in out Cells_Lists.List;
      P     : access Cell_Record'Class;
      Added : out Boolean)
   is
   begin
      Added := False;
      if P /= null then                        
         if Cells_Lists.Is_Empty (L) then
            Cells_Lists.Append (L, P);
            Added := True;
         else
            if not Cells_Lists.Contains (L, P) then
               Cells_Lists.Append (L, P);
               Added := True;
            end if;            
         end if;
      end if;
   end Add_Parent_Vertex;      
      
      
   ----------------------------------
   --  Get_Parent_To_Add_As_Vertex -- 
   ----------------------------------
      
   function Get_Parent_To_Add_As_Vertex 
     (Root : access Cell_Record'Class;
      Item : access Cell_Record'Class) return access Cell_Record'Class
   is
   begin
      if Item.Get_Parent = null then
         return null;
      end if;
      
      if Item.Get_Parent = Root then
         return Item;
      else
         return Get_Parent_To_Add_As_Vertex (Root, Item.Get_Parent);
      end if;         
      
   end Get_Parent_To_Add_As_Vertex;
      
      
   ----------------
   --  Get_Edge  --
   ----------------
      
   function Get_Edge 
     (L    : Cells_Lists.List;
      From : access Cell_Record'Class;
      To : access Cell_Record'Class) return access Cell_Record'Class
   is
      Arc : access Cell_Record'Class := null;
      Cur : Cells_Lists.Cursor;
      Crt : access Cell_Record'Class;
      
      Crt_Src : access Cell_Record'Class;
      Crt_Trg : access Cell_Record'Class;
   begin
      if not Cells_Lists.Is_Empty (L) then
         
         Cur := Cells_Lists.First (L);
         while Cells_Lists.Has_Element (Cur) loop
            
            Crt := Cells_Lists.Element (Cur);
            if Crt /= null and then Crt.Is_Edge then
               Crt_Src := Crt.Get_Terminal (True);
               Crt_Trg := Crt.Get_Terminal (False);

--                 Put_Line ("From : " & From.Get_Id & " ===> To : " & to.Get_Id);
--                 Put_Line ("Crt From : " & Crt_Src.Get_Id 
--                           & " ===> Crt To : " & Crt_Trg.Get_Id);

               if Crt_Src = From and then Crt_Trg = To then
                  Arc := Crt;
                  exit;
               end if;   
            end if;

            Cells_Lists.Next (Cur);
         end loop;
      end if;

      return Arc;
   end Get_Edge;
      
      
   ------------------
   --  Unmark_All  --
   ------------------
      
   procedure Unmark_All (VL : Cells_Lists.List)
   is
      Cur      : Cells_Lists.Cursor;      
      Crt         : access Cell_Record'Class;         
   begin
      Cur := Cells_Lists.First (VL);
      while Cells_Lists.Has_Element (Cur) loop
         Crt := Cells_Lists.Element (Cur);
         Crt.Set_Visited (False);
         Cells_Lists.Next (Cur);
      end loop;
   end Unmark_All; 
   
   
   -------------------------
   --  Get_Outgoing_Edges --
   -------------------------
   
   function Get_Outgoing_Edges 
     (V     : access Cell_Record'Class;
      Edges : Cells_Lists.List) return Cells_Lists.List
   is 
      Cur : Cells_Lists.Cursor;
      Crt : access Cell_Record'Class;
      CL  : Cells_Lists.List := Cells_Lists.Empty_List;
      Src : access Cell_Record'Class;
   begin
      Cur := Cells_Lists.First (Edges);
      while Cells_Lists.Has_Element (Cur) loop
         Crt := Cells_Lists.Element (Cur);
      
         if Crt.Is_Edge then             
            Src := Crt.Get_Terminal (True);
            
            if Src /= null and then Src.all = V.all then               
--             Put_Line (" MKU : V = " & V.Get_Id & " outEdge = " & Crt.Get_Id);
               Cells_Lists.Append (CL, Crt);
            end if;
         
         end if;
         
         Cells_Lists.Next (Cur);
      end loop;
      
      return CL;
   end Get_Outgoing_Edges;
   
   
   ---------------
   --  Explore  --
   ---------------
   
   procedure Explore (V     : access Cell_Record'Class;
                      Edges : Cells_Lists.List;
                      CCL   : in out Cells_Lists.List;
                      Union : out Cell_Ptr)
   is 
      Outgoing : Cells_Lists.List := Get_Outgoing_Edges (V, Edges);
      
      Cur      : Cells_Lists.Cursor;
      Crt      : access Cell_Record'Class;
      Target_V : access Cell_Record'Class;
      
      --  Explore (Crt) :
      --      if not Visited (Crt) then
      --        Mark as Visited Crt
      --        Add to list of connected components CCL
      --        For each outgoing edge from Crt
      --          Add edge to connected components CCL
      --          Get target vertex Target_V of crt edge 
      --          Explore (Target_V)
      --      end if 
   begin 
      Union := null;
      
      if not Is_Visited (V) then
         --  mark current vertex as visited and append to connected components 
         V.Set_Visited (True);
         Cells_Lists.Append (CCL, V);
         
         if not Cells_Lists.Is_Empty (Outgoing) then
            Cur := Cells_Lists.First (Outgoing);
            while Cells_Lists.Has_Element (Cur) loop
               Crt := Cells_Lists.Element (Cur);               
               --  mark current edge as visited and append to connected  
               --  components list
               Crt.Set_Visited (True);
               Cells_Lists.Append (CCL, Crt);
               
               --  explore target vertex if any
               Target_V := Crt.Get_Terminal (False);
               if Target_V /= null then
                  Explore (V     => Target_V,
                           Edges => Edges,
                           CCL   => CCL,
                           Union => Union);
               end if;
               
               --  get next edge to handle
               Cells_Lists.Next (Cur);
            end loop;
         end if;
      else
         if not Cells_Lists.Contains (Container => CCL,
                                      Item      => V) 
         then
            Union := Cell_Ptr (V);
         end if;         
      end if;      
   end Explore;
   
   --------------------------------------------------------------------------
   Graph_Not_Connected_VL : Cells_Lists.List := Cells_Lists.Empty_List;
   
   -------------------------------------------
   --  Build_Simplified_Dependencies_Graph  --
   -------------------------------------------
   
   procedure Build_Simplified_Dependencies_Graph 
     (Root     : access Cell_Record'Class;
      Graph_VL : out Cells_Lists.List;
      Graph_EL : out Cells_Lists.List)
   is
     
      Childs      : Cells_Lists.List := Root.Get_Children_List;

      Cur         : Cells_Lists.Cursor;
      Crt         : access Cell_Record'Class;
      Src, Trg    : access Cell_Record'Class;
      
      Src_Parent  : access Cell_Record'Class;
      Trg_Parent  : access Cell_Record'Class;
      
      Added       : Boolean := False;
      Edge        : access Cell_Record'Class;
      New_Edge    : access Cell_Record'Class;
      
      --  Starting from a root cell having childrens and edges associated with 
      --  it, find out a simplified dependency graph given as a list of 
      --  vertices (each vertex represents a main visible element of graph) and 
      --  a list of edges (each edge is a new invisible one between two vertices
      --  A and B and has associated with it a list of edges in "depends" which 
      --  contains the list of all initial edges between vertices A and B).
   begin
      Graph_VL := Cells_Lists.Empty_List;
      Graph_EL := Cells_Lists.Empty_List;
      Graph_Not_Connected_VL := Cells_Lists.Empty_List;
      
      Cur := Cells_Lists.First (Childs);
      while Cells_Lists.Has_Element (Cur) loop
         Crt := Cells_Lists.Element (Cur);

         if Crt /= null and then Crt.Is_Edge then
            
            --  1. get terminals and add to the vertex list if not already added
            --  1.1 handle source terminal
            Src := Crt.Get_Terminal (True);
            Src_Parent := Get_Parent_To_Add_As_Vertex (Root => Root,
                                                       Item => Src);            
            Add_Parent_Vertex (Graph_VL, Src_Parent, Added); 
            
            --  1.2 handle target terminal            
            Trg := Crt.Get_Terminal (False);
            Trg_Parent := Get_Parent_To_Add_As_Vertex (Root => Root,
                                                       Item => Trg);
            Add_Parent_Vertex (Graph_VL, Trg_Parent, Added);              
            
            Append_Vertices_Forward (This => Src_Parent,
                                     Cell => Trg_Parent);
            Append_Vertices_Backward (This => Trg_Parent,
                                     Cell  => Src_Parent);
            
            --  2. create new edge and add to edge list and store dependencies 
            --     between
            
            Edge := Get_Edge (Graph_EL, Src_Parent, Trg_Parent);
            if Edge = null then
               New_Edge := New_Cell;
               New_Edge.Set_Id (Src_Parent.Get_Id & "_" & Trg_Parent.Get_Id);
               New_Edge.Set_Edge (True);
               New_Edge.Set_Visible (True);                 
               
               New_Edge.Set_Terminal (Src_Parent, True);
               New_Edge.Set_Terminal (Trg_Parent, False);                
               
               Cells_Lists.Append (Graph_EL, New_Edge);

               --  update arc dependencies with crt edge
               Cells_Lists.Append (New_Edge.Depends, Crt);
               
            else
               --  update edge dependencies with crt edge
               Cells_Lists.Append (Edge.Depends, Crt);
            end if;            
         end if;
         
         Cells_Lists.Next (Cur);
      end loop;
      
      --  build not connected vertices list
      Cur := Cells_Lists.First (Childs);
      while Cells_Lists.Has_Element (Cur) loop
         Crt := Cells_Lists.Element (Cur);
      
         if Crt /= null and then Crt.Is_Vertex 
           and then not Cells_Lists.Contains (Container => Graph_VL,
                                              Item      => Crt) 
         then
            Cells_Lists.Append (Container => Graph_Not_Connected_VL,
                                New_Item  => Crt);
         end if;
         
         Cells_Lists.Next (Cur);
      end loop;
      
   end Build_Simplified_Dependencies_Graph;

   
   ------------------------------
   --  Build_Connected_Graphs  --
   ------------------------------
   
   procedure Build_Connected_Graphs 
     (VL       : Cells_Lists.List;
      EL       : Cells_Lists.List;
      CL_Lists : out Cells_Lists_To_Lists.List)
   is
      ---------------------------
      --  Get_Connected_List  --
      ---------------------------
      
      function Get_Connected_List 
        (CL_Lists : Cells_Lists_To_Lists.List;
         Connex   : access Cell_Record'Class) return Cells_Lists_Access
      is
         pragma Assert (Connex /= null);
         
         Cur : Cells_Lists_To_Lists.Cursor;
         Crt : Cells_Lists_Access;
      begin
         if not Cells_Lists_To_Lists.Is_Empty (CL_Lists) then                  
            Cur := Cells_Lists_To_Lists.First (CL_Lists);
            while Cells_Lists_To_Lists.Has_Element (Cur) loop
               Crt := Cells_Lists_To_Lists.Element (Cur);
                        
               if Cells_Lists.Contains (Container => Crt.all,
                                        Item      => Connex)
               then
                  return Crt;
               end if;
                        
               Cells_Lists_To_Lists.Next (Cur);
            end loop;
         end if;
                  
         return null;
      end Get_Connected_List;
      
      
      --------------------------
      --  Append_Cells_Lists  --
      --------------------------
      
      procedure Append_Cells_Lists (To : Cells_Lists_Access;
                                    L  : Cells_Lists_Access)
      is       
         pragma Assert (To /= null and then L /= null);
         
         Cur : Cells_Lists.Cursor;
      begin
         if not Cells_Lists.Is_Empty (To.all) 
           and then not Cells_Lists.Is_Empty (L.all) 
         then
            Cur := Cells_Lists.First (L.all);
            while Cells_Lists.Has_Element (Cur) loop
               Cells_Lists.Append (Container => To.all,
                                   New_Item  => Cells_Lists.Element (Cur));
               Cells_Lists.Next (Cur);
            end loop;            
         end if;         
      end Append_Cells_Lists;
      
               
      Crt   : access Cell_Record'Class;
      CL    : Cells_Lists_Access;
      U     : Cell_Ptr;
      
   begin   
      Unmark_All (VL);
      Unmark_All (EL);
      CL_Lists := Cells_Lists_To_Lists.Empty_List;
      
      --  build connected graphs and append to graphs list
      Crt := First_Unvisited_In_Terminal_Cell (VL);
      while Crt /= null loop

         CL := new Cells_Lists.List;
         CL.all := Cells_Lists.Empty_List;

         --  1. Explore Crt vertex in order to find all connected components
         Explore (V     => Crt,
                  Edges => EL,
                  CCL   => CL.all,
                  Union => U);
         
         --  2. Add CL at connected graphs list
         if U /= null then
            declare 
               Prev_CL : Cells_Lists_Access := null;
            begin
               --  find already added list which contains cell visited previously
               Prev_CL := Get_Connected_List (CL_Lists, U);
               
               --  append crt CCL to previously built one 
               Append_Cells_Lists (Prev_CL, CL);
            end;
            
         else            
            Cells_Lists_To_Lists.Append (CL_Lists, CL);
         end if;
         
         --  3. get the next unvisited vertex after crt connected graph build         
         Crt := Next_Unvisited_In_Terminal_Cell (VL, Crt);         
      end loop;
      
   end Build_Connected_Graphs;
      
   
   -------------------------------------
   --  Build_Cells_And_Update_Childs  --
   -------------------------------------
   
   procedure Build_Cells_And_Update_Childs 
     (Root     : access Cell_Record'Class;
      CL_Lists : Cells_Lists_To_Lists.List) 
   is
      --------------------------
      --  Append_Cells_Lists  --
      --------------------------
      
      procedure Append_Cells_Lists (To : access Cell_Record'Class;
                                    L  : Cells_Lists.List)
      is       
         pragma Assert (not Cells_Lists.Is_Empty (To.Get_Children_List) 
                        and then not Cells_Lists.Is_Empty (L));
         
         Cur : Cells_Lists.Cursor;
      begin
         Cur := Cells_Lists.First (L);
         while Cells_Lists.Has_Element (Cur) loop
            To.Insert (Cells_Lists.Element (Cur));
            Cells_Lists.Next (Cur);
         end loop;            
      end Append_Cells_Lists;
      
      
      CurL     : Cells_Lists_To_Lists.Cursor;
      CrtL     : Cells_Lists.List := Cells_Lists.Empty_List;
      
      Children : Cells_Lists.List := Cells_Lists.Empty_List;
      Root_Ch  : Cells_Lists.List := Cells_Lists.Empty_List;
      
      N_Cell   : access Cell_Record'Class;
      Group_Nr : Integer := 0;

      Src_V    : access Cell_Record'Class;      
      Trgt_V   : access Cell_Record'Class;
      
   begin
      CurL := Cells_Lists_To_Lists.First (CL_Lists);
      
      Root_Ch := Root.Get_Children_List;         
      
      while Cells_Lists_To_Lists.Has_Element (CurL) loop
         CrtL := Cells_Lists_To_Lists.Element (CurL).all;
         
         Group_Nr := Group_Nr + 1;
         
         --  create new englobing not visible cell 
         N_Cell := New_Cell;
         N_Cell.Set_Id ("group_" & Artics.Utils.Integer_To_String (Group_Nr));
         N_Cell.Set_Visible (True);
         N_Cell.Set_Vertex (True);
	 
         --  add to childrens list  
         Root.Insert (N_Cell);
         
         Put_Line ("group created : " & N_Cell.Get_Id & " ok.");
         
         --  set childrens 
         for C of CrtL loop         
            
            if C.Is_Edge then
               Append_Depend (This => N_Cell,
                              Edge => C);
            else
               N_Cell.Insert (C);
            end if;
            
         end loop;
         
         --  update childrens list of englobing cell with old edges 
         for C of Root_Ch loop
            if C.Is_Edge then
               Src_V := C.Get_Terminal (True);
               Trgt_V := C.Get_Terminal (False);
              
               if Is_Parent_Ancestor (N_Cell, Src_V) 
                 and then Is_Parent_Ancestor (N_Cell, Trgt_V) 
               then 
                  N_Cell.Insert (C);
               end if;               
            end if;            
         end loop;
         
         Put_Line ("group : " & N_Cell.Get_Id 
                   & " children count = " & N_Cell.Get_Child_Count'Img
                   & " depends length = " 
                   & Cells_Lists.Length (N_Cell.Get_Depends)'Img);
         
         
         --  next connected graph list
         Cells_Lists_To_Lists.Next (CurL);
      end loop;
      
      if not Cells_Lists.Is_Empty (Graph_Not_Connected_VL) then
         Append_Cells_Lists (To => Root,
                             L  => Graph_Not_Connected_VL);
      end if;
      
      Root.Edges    := Cells_Lists.Empty_List;
      
   end Build_Cells_And_Update_Childs;
      
   
   -------------------------------------
   --  Build_Cell_Dependencies_Graph  --
   -------------------------------------
   
   procedure Build_Cell_Dependencies_Graph 
     (Root  : access Cell_Record'Class)
   is 
      Ch_List : Cells_Lists.List := Root.Get_Children_List;

      --  temporary lists corresponding to dependency graph vertices and edges
      Vertices_List : Cells_Lists.List := Cells_Lists.Empty_List;
      Edges_List    : Cells_Lists.List := Cells_Lists.Empty_List;

      --  temporary lists of connected graphs 
      CL_Lists : Cells_Lists_To_Lists.List := Cells_Lists_To_Lists.Empty_List;
   begin
      if Cells_Lists.Is_Empty (Ch_List) then
         return;
      end if;
      
      for C of Ch_List loop
	 Set_Depends           (C, Cells_Lists.Empty_List);   
	 Set_Vertices_Forward  (C, Cells_Lists.Empty_List);   
	 Set_Vertices_Backward (C, Cells_Lists.Empty_List);   
	 Set_Edge_Mark (C, False);
	 Set_Visited (C, False);
      end loop;
      
      --  A. build simplified dependency graph
      Build_Simplified_Dependencies_Graph 
        (Root     => Root,
         Graph_VL => Vertices_List,
         Graph_EL => Edges_List);
      
--      Write_Simple_Xml_Cell (Root, "collision-simple.xml");
      
      --  B. visit dependency graph and find out connected graphs list
      --     Unmark all vertex and edges of dependency graph
      Build_Connected_Graphs (VL       => Vertices_List,
                              EL       => Edges_List,
                              CL_Lists => CL_Lists);
      
      --  C. build cell for each cell list and add as children list of root cell
      Build_Cells_And_Update_Childs (Root, CL_Lists);
      
--      Write_Simple_Xml_Cell (Root, "Collision-update.xml");
   end Build_Cell_Dependencies_Graph;


   ----------------------------------------------------------------------------
   --                          L A Y E R S                                   --
   ----------------------------------------------------------------------------

   ---------------------------------
   --  Get_Graph_Vertices_Number  --
   ---------------------------------

   function Get_Graph_Vertices_Number 
     (V : access Cell_Record'Class) return Integer  
   is
      Vx_Nb : Integer := 0;
      
      L     : Cells_Lists.List := V.Get_Children_List;
      Cur   : Cells_Lists.Cursor;
      Crt   : access Cell_Record'Class;
   begin
      
      if not Cells_Lists.Is_Empty (L) then
         Cur := Cells_Lists.First (L);
         
         while Cells_Lists.Has_Element (Cur) loop
            Crt := Cells_Lists.Element (Cur);
            
            if Crt /= null and then Crt.Is_Vertex then
               Vx_Nb := Vx_Nb + 1;
            end if;
            
            Cells_Lists.Next (Cur);
         end loop;
      end if;
      
      return Vx_Nb;
   end Get_Graph_Vertices_Number;
   
   
   ---------------------------
   --  Get_Incomming_Edges  --
   ---------------------------

   function Get_Incomming_Edges 
     (V : access Cell_Record'Class;
      L : Cells_Lists.List) return Cells_Lists.List
   is
      Cur  : Cells_Lists.Cursor;
      Crt  : access Cell_Record'Class;
      CL   : Cells_Lists.List := Cells_Lists.Empty_List;
      Trgt : access Cell_Record'Class;
   begin
      Cur := Cells_Lists.First (L);
      while Cells_Lists.Has_Element (Cur) loop
         Crt := Cells_Lists.Element (Cur);
         if Crt.Is_Edge then             
--              Put_Line ("    incomming for V = " & V.Get_Id 
--                     & " Crt arc = " & Crt.Get_Id);
            Trgt := Crt.Get_Terminal (False);
--              Put_Line ("    **** found Trgt = " & Trgt.Get_Id);
            
            if Trgt /= null and then Trgt.all = V.all then               
--                 Put_Line ("    ===> append Crt = " & Crt.Get_Id);
               Cells_Lists.Append (CL, Crt);
            end if;         
         end if;
         
         Cells_Lists.Next (Cur);
      end loop;
      
      return CL;
   end Get_Incomming_Edges;
   
   
   ----------------------
   --  Compute_Layers  --
   ----------------------
   
   procedure Compute_Layers (V : access Cell_Record'Class) 
   is 
      -----------------------------------
      --  Has_Not_Classified_Vertices  --
      -----------------------------------
   
      function Has_Not_Classified_Vertices (L_Arr : Graph_Vertex_Layers_Array) 
                                            return Boolean 
      is
      begin
         for I in L_Arr'Range loop            
            if L_Arr (I).Vx.Get_Layer = -1 then
               return True;
            end if;            
         end loop;
         
         return False;
      end Has_Not_Classified_Vertices;
      
      
      --------------------------------
      --  Build_Graph_Layers_Array  --
      --------------------------------
      
      procedure Build_Graph_Layers_Array 
        (L      : in Cells_Lists.List;
         D      : in Cells_Lists.List;
         GR_Arr : out Graph_Vertex_Layers_Array)
      is
         pragma Assert (not Cells_Lists.Is_Empty (L));
         pragma Assert (not Cells_Lists.Is_Empty (D));
         
         Idx    : Integer := GR_Arr'First;
         
         Cur    : Cells_Lists.Cursor;
         Crt    : access Cell_Record'Class;
         
         Vx     : access Cell_Record'Class;
         Pre_E  : Cells_Lists.List := Cells_Lists.Empty_List;
         Pre_Vx : Cells_Lists.List := Cells_Lists.Empty_List;         
      begin
         Cur := Cells_Lists.First (L);
         while Cells_Lists.Has_Element (Cur) loop
            Crt := Cells_Lists.Element (Cur);         
            
           -- Put_Line ("Crt = " & Crt.Get_Id
            --          & " is_vertex=" & Crt.Is_Vertex'Img);
            
            if Crt /= null and then Crt.Is_Vertex then
               Vx := Crt;
               
               Pre_E := Get_Incomming_Edges (Vx, D); 

               --  using incomming edges, build from vertices list in order to 
               --  get predecessors
               if not Cells_Lists.Is_Empty (Pre_E) then
                  declare
                     Cur_E  : Cells_Lists.Cursor := Cells_Lists.First (Pre_E);
                     Crt_E  : access Cell_Record'Class;
                     Src_Vx : access Cell_Record'Class;
                  begin
                     Pre_Vx := Cells_Lists.Empty_List;
                     while Cells_Lists.Has_Element (Cur_E) loop
                        Crt_E := Cells_Lists.Element (Cur_E);
                        --        Put_Line (" Crt_E = " & Crt_E.Get_Id);
                        
                        if Crt_E /= null then
                           Src_Vx := Crt_E.Get_Terminal (True);                           
                           --       Put_Line (" Src_Vx = " & Src_Vx.Get_Id);
                           
                           if Src_Vx  /= null  
                             and then not Cells_Lists.Contains 
                               (Container => Pre_Vx,
                                Item      => Src_Vx)
                           then
                                                    
                              --      Put_Line (" ====> append Src_Vx = " & Src_Vx.Get_Id);
                              Cells_Lists.Append (Container => Pre_Vx,
                                                  New_Item  => Src_Vx);
                           end if;
                        end if;
                        
                        Cells_Lists.Next (Cur_E);
                     end loop;                  
                  end;
               else
                  Pre_Vx := Pre_E;
               end if;
               
               --  add elements in array
               GR_Arr (Idx).Vx := Vx;
               GR_Arr (Idx).Pre_Vx := Pre_Vx;
               
               Idx := Idx + 1;         
            end if;
            
            Cells_Lists.Next (Cur);         
         end loop;
      end Build_Graph_Layers_Array;
      
      
      pragma Assert (V /= null 
                     and then not Cells_Lists.Is_Empty (V.Get_Children_List));
      
      Vx_Number    : Integer := Get_Graph_Vertices_Number (V); 
      pragma Assert (Vx_Number >= 1);
            
      
      Gr_Layer_Arr : Graph_Vertex_Layers_Array (1..Vx_Number);
      
      ---------------
      --  Dump_GR  --
      ---------------
      
      procedure Dump_GR is
         L      : Cells_Lists.List := Cells_Lists.Empty_List;
         Cur    : Cells_Lists.Cursor;
         Crt    : access Cell_Record'Class;
         
         V_List : Name_Id := No_Name;
      begin

         Put_Line ("***********  Dump Gr_Layer_Arr ********");
         
         for I in Gr_Layer_Arr'Range loop
            if not Cells_Lists.Is_Empty (Gr_Layer_Arr (I).Pre_Vx) then
               V_List := No_Name;
               L := Gr_Layer_Arr (I).Pre_Vx;
               Cur := Cells_Lists.First (L);
               while Cells_Lists.Has_Element (Cur) loop
                  Crt := Cells_Lists.Element (Cur);
                  if V_List = No_Name then
                     V_List := String_Find (Crt.Get_Id);
                  else                     
                     V_List := String_Find 
                       (Get_String (V_List) & ", " & Crt.Get_Id);
                  end if;
                  Cells_Lists.Next (Cur);
               end loop;               
               
               Put_Line (I'img & " : " & Gr_Layer_Arr (I).Vx.Get_Id 
                         & " - Vertex = " & Gr_Layer_Arr (I).Vx.Is_Vertex'Img 
                         & " - Layer = " & Gr_Layer_Arr (I).Vx.Get_Layer'Img 
                         & " - V_List = { "  & Get_String (V_List) &  " }"); 
            else
               Put_Line (I'img & " : " & Gr_Layer_Arr (I).Vx.Get_Id 
                         & " - Vertex = " & Gr_Layer_Arr (I).Vx.Is_Vertex'Img 
                         & " - Layer = " & Gr_Layer_Arr (I).Vx.Get_Layer'Img 
                         & " - V_List = {}");
            end if;
         end loop;
      
         Put_Line ("*******************");
      end Dump_GR;

      Cur_Crt : Cells_Lists.Cursor;      
      
      N_Layer : Integer := 0;
      L : cells_lists.list;
      
      Idx_Arr : array (1..Vx_Number) of Integer := (others => -1);
      Crt_Idx : Integer                         := Idx_Arr'First;
   begin  
      
      --  ALGO to do this on V which is a given as a connected graph
      --  1. build a layers array containing the association (Vx -> Pre (Vx)) 
      --  2. Init NLayer := 0
      --  3. WHILE there are not classified vertices in array
      --      NLayer := NLayer + 1
      --      FOR each X vertice without unclassified predecessors 
      --          Layer (X) := NLayer; 
      --      END FOR
      --      remove from graph layers array the classified vertex X
      --  END WHILE
      
      --  1. build and fill working array for layers computing 
      Build_Graph_Layers_Array 
        (V.Get_Children_List, V.Get_Depends, Gr_Layer_Arr);
      Dump_Gr;
      
      --  2. compute layers          
      while Has_Not_Classified_Vertices (Gr_Layer_Arr) loop
         
         N_Layer := N_Layer + 1;
--           Put_Line ("  For N_Layer = " & N_Layer'Img & " : ");

         --  2.1. find all not classified vertices at N_Layer
         Idx_Arr := (others => -1);
         Crt_Idx := Idx_Arr'First;
         
         for I in Gr_Layer_Arr'Range loop
            L := Gr_Layer_Arr (I).Pre_Vx;
            if Cells_Lists.Is_Empty (L) 
              and then Gr_Layer_Arr (I).Vx.Get_Layer = -1
            then
               Idx_Arr (Crt_Idx) := I;
               Crt_Idx := Crt_Idx + 1;
            end if;
         end loop;
         
         --  2.2 for all not classified vertices at N_Layer, set level and 
         --  remove all dependencies from graph layer array
         for I in Idx_Arr'Range loop
            if Idx_Arr (I) = -1 then
               exit;
            end if;
            
--              Put_Line ("    MKU : set_layer for = " & Idx_Arr (I)'Img);
--              Put_Line ("                  Vx_Id = " 
--                        & Gr_Layer_Arr (Idx_Arr (I)).Vx.Get_Id);         
              
            --  set layer for graph array vertex found at Idx_Arr (I) location
            Gr_Layer_Arr (Idx_Arr (I)).Vx.Set_Layer (N_Layer);
            
            --  remove classified vertex from all Pre_Vx contained in array
            for J in Gr_Layer_Arr'Range loop
               L := Gr_Layer_Arr (J).Pre_Vx;
               if not Cells_Lists.Is_Empty (L) then
                  if Cells_Lists.Contains
                    (Container => L,
                     Item      => Gr_Layer_Arr (Idx_Arr (I)).Vx)
                  then                     
                     Cur_Crt := Cells_Lists.Find 
                       (Container => L,
                        Item      => Gr_Layer_Arr (Idx_Arr (I)).Vx);
                     Cells_Lists.Delete 
                       (Container => L,
                        Position  => Cur_Crt); 
                     Gr_Layer_Arr (J).Pre_Vx := L;
                  end if;
               end if;
            end loop;               

         end loop; 
            
      end loop;
      Dump_GR;
      
   end Compute_Layers;
      
   
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
	       Put_Line ("    C " & Get_Id (C) & " not in Stack ");
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
   
   ---------------------------
   -- Compute_Layer_Forward --
   ---------------------------
   
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
	 Cell.Set_Layer (Layer + 1);
	 
	 Forwards := Cell.Get_Vertices_Forward;
	 for F of Forwards loop
	    Inc_Forward_Layer (F, New_Layer);
	 end loop;
      end if;
   end Inc_Forward_Layer;
   
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
	 Forwards := Child.Get_Vertices_Forward;
	 Layer := Child.Get_Layer;
	 for F of Forwards loop
	    Inc_Forward_Layer (F, Layer);
	 end loop;
      end loop;
   end Compute_Layer_Forward;
   
   
end Artics.Graph.Cells.Analysis;
