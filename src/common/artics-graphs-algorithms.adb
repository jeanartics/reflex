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

with Artics.Types; use Artics.Types;
with Artics.Containers.Lists;
with Artics.Dynamic_Stack;

separate (Artics.Graphs)

package body Algorithms is

   ------------------------
   -- New_Branch_Element --
   ------------------------

   function New_Branch_Element
     (V : in Vertex := Null_Vertex;
      A : in Arc    := Null_Arc)
     return Branch_Elmt
   is
      Elmt : Branch_Elmt := new Branch_Elmt_Record;
   begin
      Elmt.V := V;
      Elmt.A := A;
      return Elmt;
   end New_Branch_Element;

   ----------------------------
   -- Set_Branch_Elmt_Vertex --
   ----------------------------

   procedure Set_Branch_Elmt_Vertex
     (Elmt : in out Branch_Elmt;
      V    : in Vertex)
   is
   begin
      Elmt.V := V;
   end Set_Branch_Elmt_Vertex;

   ----------------------------
   -- Get_Branch_Elmt_Vertex --
   ----------------------------

   function Get_Branch_Elmt_Vertex
     (Elmt : in Branch_Elmt) return Vertex
   is
   begin
      return Elmt.V;
   end Get_Branch_Elmt_Vertex;

   -------------------------
   -- Set_Branch_Elmt_Arc --
   -------------------------

   procedure Set_Branch_Elmt_Arc
     (Elmt : in out Branch_Elmt;
      A    : in Arc)
   is
   begin
      Elmt.A := A;
   end Set_Branch_Elmt_Arc;

   -------------------------
   -- Get_Branch_Elmt_Arc --
   -------------------------

   function Get_Branch_Elmt_Arc
     (Elmt : in Branch_Elmt) return Arc
   is
   begin
      return Elmt.A;
   end Get_Branch_Elmt_Arc;

   ----------------------
   -- Distance_To_Deep --
   ----------------------

   type Deep_Visitor_Record is new Graph_Visitor_Record with null record;

   procedure Visit_Vertex
     (V : in out Deep_Visitor_Record;
      S : in Visit_Step);
   -- Visit a vertex to compute distance to root and to deep.

   ------------------
   -- Visit_Vertex --
   ------------------

   procedure Visit_Vertex
     (V : in out Deep_Visitor_Record;
      S : in Visit_Step) is
   begin
      if S = Before then
         declare
            Vx   : Vertex := Get_Vertex (V);
            It   : Arc_Id := First_In_Arc (Vx);
            Cur  : Arc;
            Dist : Natural := 0;
            D    : Natural := 0;
         begin
            while It /= No_Arc_Id loop
               Cur := Current_Item (It);
               D := Get_Distance_To_Root (Get_Source_Vertex (Cur));
               if D > Dist then
                  Dist := D;
               end if;
               Next (It);
            end loop;
            Set_Distance_To_Root (Vx, Dist + 1);
         end;
      end if;

      if S = After then
         declare
            Vx   : Vertex := Get_Vertex (V);
            It   : Arc_Id := First_Out_Arc (Vx);
            Cur  : Arc;
            Dist : Natural := 0;
            D    : Natural := 0;
         begin
            while It /= No_Arc_Id loop
               Cur := Current_Item (It);
               D := Get_Distance_To_Deep (Get_Target_Vertex (Cur));
               if D > Dist then
                  Dist := D;
               end if;
               Next (It);
            end loop;
            Set_Distance_To_Deep (Vx, Dist + 1);
         end;
      end if;
   end Visit_Vertex;

   -- Start Processing of Distance_To_Deep --
   ------------------------------------------

   procedure Distance_To_Deep (G : in Graph) is

      Dv : Deep_Visitor_Record;
   begin
      Deep_First_Walk (Dv, G);
   end Distance_To_Deep;

   ----------------------
   -- Distance_To_Root --
   ----------------------

   -- This procedure is a renames from Distance_To_Deep, as this computes the
   -- ditance to deep and the distance to root at the same time.

   procedure Distance_To_Root
     (G : in Graph) renames Distance_To_Deep;

--     -------------------
--     -- Tree_Covering --
--     -------------------

--     function Tree_Covering (G : in Graph) return Tree_List_Id is
--     begin
--        raise Not_Implemented;
--     end Tree_Covering;

   ----------------------
   -- Get_Longest_Path --
   ----------------------

   function Get_Longest_Path
     (V         : in Vertex;
      Visited_V : in Vertex_Lists.List_Id) return Vertex_Lists.List_Id is

      Tmp_Visited : Vertex_Lists.List_Id :=
        Vertex_Lists.New_List_Copy (Visited_V);

      Longest_Path : Vertex_Lists.List_Id :=
        Vertex_Lists.New_List;

      AL : Arc_Lists.List_Id := Get_Outgoing_Arcs (V);
      AIt : Arc_Lists.List_Iterator :=
        Arc_Lists.New_Iterator (AL);

      Crt_V : Vertex := Null_Vertex;

      I : Integer := 0;
   begin
      Vertex_Lists.Append (V, Tmp_Visited);

      while not Arc_Lists.Is_End (AIt) loop
         Crt_V := Get_Target_Vertex
           (Arc_Lists.Current_Item (AIt));
         if not Vertex_Lists.In_List (Crt_V, Tmp_Visited)
         then
            declare
               Tmp_Path : Vertex_Lists.List_Id :=
                 Vertex_Lists.New_List;
            begin
               Tmp_Path := Get_Longest_Path
                 (Crt_V, Tmp_Visited);
               if Vertex_Lists.Length (Tmp_Path)>
                 Vertex_Lists.Length (Longest_Path)
               then
                  Longest_Path := Tmp_Path;
               end if;
            end;
         end if;
         Arc_Lists.Next (AIt);
      end loop;
      Vertex_Lists.Push (V, Longest_Path);

      return Longest_Path;
   end Get_Longest_Path ;

   ----------------------
   -- Max_Depth_Search --
   ----------------------

   function Max_Depth_Search
     (G : in Graph) return Vertex_Lists.List_Id is

      VL : Vertex_Lists.List_Id := Get_Vertex_List (G);
      VIt : Vertex_Lists.List_Iterator :=
        Vertex_Lists.New_Iterator (VL);

      V  : Vertex := Vertex_Lists.Current_Item (VIt);
      St : Vertex_Lists.List_Id := Vertex_Lists.New_List;
   begin
      return Get_Longest_Path (V, St);
   end Max_Depth_Search;

   ----------------------------
   -- Distance_To_Deep_Order --
   ----------------------------

   procedure Distance_To_Deep_Order (G : in Graph) is
   begin
      null;
   end Distance_To_Deep_Order;

   ------------------
   -- Exchange_Arc --
   ------------------

   procedure Exchange_Consecutif_Arc
     (Frst : in Arc_Id;
      Lst  : in Arc_Id) is
   begin
      null;
   end Exchange_Consecutif_Arc;
   
   --------------------------------------
   -- Vertex_Order_By_Distance_To_Deep --
   --------------------------------------
   
   procedure Vertex_Order_By_Distance_To_Deep (V : in Vertex) is

      Forw  : Arc_Id := First_Out_Arc (V);
      Back  : Arc_Id;
      Prv   : Arc_Id;
      Done  : Boolean;
      Aback : Arc;
      Aprv  : Arc;
      Dback : Natural;
      Dprv  : Natural;
   begin
      while Forw /= No_Arc_Id loop
         Back := Last_Out_Arc (V);
         Done := False;

         while not Done loop
            Prv := Back;
            Previous (Prv);
            Aback := Current_Item (Back);
            Aprv := Current_Item (Prv);
            Done := Aprv = Current_Item (Forw);
            Dprv  := Target_Distance_To_Deep (Aprv);
            Dback := Target_Distance_To_Deep (Aback);

            if Dback > Dprv then
               Exchange_Consecutif_Arc (Prv, Back);
               if Done then
                  Forw := Back;
               end if;
            end if;
         end loop;

         Next (Forw);
      end loop;
   end Vertex_Order_By_Distance_To_Deep;

   ----------------------------
   -- Distance_To_Root_Order --
   ----------------------------

   procedure Distance_To_Root_Order (G : in Graph) is
   begin
      null;
   end Distance_To_Root_Order;

   -----------------
   -- Cover_Order --
   -----------------

   procedure Cover_Order (G : in Graph) is
   begin
      null;
   end Cover_Order;

   ------------
   -- Parent --
   ------------

   function Parent
     (Par   : in Vertex;
      Child : in Vertex) return Boolean is

   begin
      return False;
   end Parent;

   ---------------------
   -- Is_Backward_Arc --
   ---------------------

   function Is_Backward_Arc (A : in Arc) return Boolean is
   begin
      return Target_Tree_Cover (A) > Source_Tree_Cover (A);
   end Is_Backward_Arc;

   --------------------
   -- Is_Forward_Arc --
   --------------------

   function Is_Forward_Arc (A : in Arc) return Boolean is
   begin
      return Target_Tree_Cover (A) > Source_Tree_Cover (A) and then
        Target_Distance_To_Root (A) > Source_Distance_To_Root (A) + 1;
   end Is_Forward_Arc;

   ------------------
   -- Is_Cover_Arc --
   ------------------

   function Is_Cover_Arc (A : in Arc) return Boolean is
   begin
      return Target_Tree_Cover (A) > Source_Tree_Cover (A) and then
        Target_Distance_To_Root (A) = Source_Distance_To_Root (A) + 1;
   end Is_Cover_Arc;

   -------------------
   -- Is_Crossed_Arc --
   -------------------

   function Is_Crossed_Arc (A : in Arc) return Boolean is
   begin
      return Target_Tree_Cover (A) > Source_Tree_Cover (A);
   end Is_Crossed_Arc;


   -------------------
   -- REDUCE GRAPHS --
   -------------------

   ---------------------
   -- Has_Divergences --
   ---------------------
   function Has_Divergences (G : in Graph) return Boolean
   is
      VL    : Vertex_Lists.List_Id := Get_Vertex_List (G);
      It    : Vertex_Lists.List_Iterator :=
        Vertex_Lists.New_Iterator (VL);
      Crt_V : Vertex  := Vertex_Lists.Current_Item (It);
      use Vertex_Lists;
   begin
      while not Is_End (It) loop
         if Vertex_External_Degre (Crt_V) > 1 then
            return True;
         end if;
         Next (It);
         Crt_V := Current_Item (It);
      end loop;
      Delete_Iterator (It);
      return False;
   end Has_Divergences;

   -----------------------
   -- Reduce_Divergence --
   -----------------------

   procedure Reduce_Divergence
     (G : in out Graph;
      V : in out Vertex)
   is
      VL    : Vertex_Lists.List_Id := Get_Vertex_List (G);
      Max_D : Vertex_Lists.List_Id := Max_Depth_Search (G);
      AL    : Arc_Lists.List_Id    := Get_Outgoing_Arcs (V);
      Old_AL: Arc_Lists.List_Id    := New_List_Copy (AL);
      AIt   : Arc_Lists.List_Iterator := New_Iterator (Old_AL);
      Crt_A : Arc := Current_Item (AIt);
      Target: Vertex := Get_Target_Vertex (Crt_A);
   begin
      raise Not_Implemented;
   end Reduce_Divergence;

   ------------------------------
   -- Reduce_Graph_Divergences --
   ------------------------------

   procedure Reduce_Graph_With_Divergences (G : in out Graph)
   is
      VL : Vertex_Lists.List_Id := Get_Vertex_List (G);
      Old_VL : Vertex_Lists.List_Id :=
        Vertex_Lists.New_List_Copy (VL);
      It : Vertex_Lists.List_Iterator :=
        Vertex_Lists.New_Iterator (Old_VL);
      Crt_V : Vertex := Vertex_Lists.Current_Item (It);
      use Vertex_Lists;
   begin
      while not Is_End (It) loop
         if Vertex_External_Degre (Crt_V) > 1 then
            Reduce_Divergence (G, Crt_V);
         end if;
         Next (It);
         Crt_V := Current_Item (It);
      end loop;
      Delete_Iterator(It);
   end Reduce_Graph_With_Divergences;

   --------------------
   -- Simplify_Graph --
   --------------------

   procedure Simplify_Graph
     (G     : in out Graph;
      V     : in out Vertex;
      Out_A : in Arc)
   is
      Target    : Vertex := Get_Target_Vertex (Out_A);
      Target_AL : Arc_Lists.List_Id := Get_Outgoing_Arcs (Target);

      Old_AL : Arc_Lists.List_Id := New_List_Copy (Target_AL);
      AIt    : Arc_Lists.List_Iterator := New_Iterator(Old_AL);

      Crt_A  : Arc := Null_Arc ;
      Crt_It : Arc_Item := No_Arc_Item ;
   begin
      Remove_Arc (G, Out_A);
      if not Is_Empty_List (Old_AL) then
         Crt_A := Current_Item (AIt);
         Crt_It := Get_Item (Crt_A);
         while not Is_End (AIt) loop
            Add_Arc
              (G, New_Arc (V,
                           Get_Target_Vertex(Crt_A),
                           Crt_It));
            Remove_Arc (G, Crt_A);
            Next (AIt);
            Crt_A := Current_Item (AIt);
            Crt_It := Get_Item (Crt_A);
         end loop;
      end if;
      Delete_Iterator (AIt);
      Remove_Vertex (G, Target);
   end Simplify_Graph;

   -----------
   -- Merge --
   -----------

   procedure Merge
     ( V     : in out Vertex;
       Out_A : in Arc)
   is
      Target : Vertex    := Get_Target_Vertex (Out_A);
      Elmt   : Branch_Elmt := No_Branch_Elmt;
      BR     : Branch_Elmt_Lists.List_Id :=
        Branch_Elmt_Lists.No_List;
      use  Branch_Elmt_Lists;
   begin

      case Branches_Number (V) is
         when 0 =>
            if Vertex_External_Degre (V) = 1 and
              Vertex_Internal_Degre (Target) = 1
            then
               BR := New_List;
               Elmt := New_Branch_Element (V, Null_Arc);
               Append (Elmt, BR);
               Elmt := New_Branch_Element (Null_Vertex, Out_A);
               Append (Elmt, BR);
               Elmt := New_Branch_Element (Target, Null_Arc);
               Append (Elmt, BR);
               Add_Branch_To_Branches_Record (V, BR);
            end if;

         when 1 =>
            if Branches_Number (Target) = 0 and then
              Vertex_External_Degre (Target) <= 1
            then
               declare
                  Br_Rec : Branches_Record := Get_Branches_Record (V);
                  Arr    : Branch_Array renames Br_Rec.Arr;
                  Idx    : Integer := Br_Rec.Free_Idx -1;
               begin
                  BR   := Arr (Idx);
                  Elmt := New_Branch_Element (Null_Vertex, Out_A);
                  Append (Elmt, BR);
                  Elmt := New_Branch_Element (Target, Null_Arc);
                  Append (Elmt, BR);
               end;
            end if;

         when others =>
            raise Not_Implemented;
      end case;
   end Merge;

   --------------------
   -- Merge_Vertices --
   --------------------

   procedure Merge_Vertices
     (G  : in out Graph;
      VL : in Vertex_Lists.List_Id;
      V  : in out Vertex)
   is
      AL     : Arc_Lists.List_Id := Get_Outgoing_Arcs (V);
      Out_A  : Arc               := First_Item (AL);
      Target : Vertex            := Get_Target_Vertex (Out_A);
   begin
      Merge (V, Out_A);

      if In_List (Target, VL) then
         Mark (Target, True);
      end if;

      if Vertex_Internal_Degre (Target) = 1 then
         case Branches_Number (Target) is
            when 0 =>
               if Vertex_External_Degre (Target) = 0 then
                  -- terminal vertex
                  Simplify_Graph (G, V, Out_A);
               elsif Vertex_External_Degre (Target) = 1 then
                  Simplify_Graph (G, V, Out_A);
                  if Vertex_External_Degre (V) = 1 then
                     Merge_Vertices (G, VL, V);
                  end if;
               else -- divergence
                  Mark (Target, False);
                  Reduce_Simple_Graph (G, VL, Target);
               end if;

            when others =>
               raise Not_Implemented;
         end case;
      end if;
   end Merge_Vertices;

   -------------------------
   -- Reduce_Simple_Graph --
   -------------------------

   procedure Reduce_Simple_Graph
     (G  : in out Graph;
      VL : in Vertex_Lists.List_Id;
      V  : in out Vertex)
   is
      Ext_Deg : Natural := Vertex_External_Degre (V);
   begin
      Mark (V, True);
      if Ext_Deg > 1 then
         Reduce_Divergence (G, V);

      elsif Ext_Deg = 1 and Branches_Number (V) = 0 then
         Merge_Vertices (G, VL, V);

      else
         null;
      end if;
   end Reduce_Simple_Graph;

   ------------------
   -- Reduce_Graph --
   ------------------

   procedure Reduce_Graph (G  : in out Graph)
   is
      VL     : Vertex_Lists.List_Id := Get_Vertex_List (G);
      Old_VL : Vertex_Lists.List_Id :=
        Vertex_Lists.New_List_Copy (VL);
      It  : Vertex_Lists.List_Iterator :=
        Vertex_Lists.New_Iterator (Old_VL);
      Crt_V   : Vertex  := Vertex_Lists.Current_Item (It);
      use Vertex_Lists;
   begin
      Reinitialize_Graph (G);
      if Has_Divergences (G) then
         while not Is_End (It) loop
            if not Marked (Crt_V) then
               Reduce_Graph_With_Divergences (G);
            end if;
            Next (It);
            Crt_V := Current_Item (It);
         end loop;
      else
         Reduce_Simple_Graph (G, VL, Crt_V);
      end if;
   end Reduce_Graph;

   ---------------
   -- Less_Than --
   ---------------

   function Less_Than
     (A1 : in Arc;
      A2 : in Arc) return Boolean is

      V1 : Vertex;
      V2 : Vertex;
   begin
      V1 := Get_Target_Vertex (A1);
      V2 := Get_Target_Vertex (A2);
      return Get_Distance_To_Deep (V1) < Get_Distance_To_Deep (V2);
   end Less_Than;

   procedure Sort_By_Deep_Order is new Arc_Lists.Bubble_Sort_List (Less_Than);

   ----------------------
   -- Distance_To_Deep --
   ----------------------

   type Deep_Order_Visitor_Record is new Graph_Visitor_Record with null record;

   procedure Visit_Vertex
     (V : in out Deep_Order_Visitor_Record;
      S : in Visit_Step);
   -- Visit a vertex to compute distance to root and to deep.

   ------------------
   -- Visit_Vertex --
   ------------------

   procedure Visit_Vertex
     (V : in out Deep_Order_Visitor_Record;
      S : in Visit_Step) is

      L  : Arc_Lists.List_Id;
      Vx : Vertex;
   begin
      if S = Before then
         Vx := Get_Vertex (V);
         L := Get_Outgoing_Arcs (Vx);
         Sort_By_Deep_Order (L);
      end if;
   end Visit_Vertex;

   ------------------------
   -- Sort_By_Deep_Order --
   ------------------------

   procedure Sort_By_Deep_Order (G : in Graph) is
      V : Deep_Order_Visitor_Record;
   begin
      Visit_Initialize (V, G);
      Do_Visit (V, G);
      Visit_Finalize (V);
   end Sort_By_Deep_Order;

end Algorithms;
