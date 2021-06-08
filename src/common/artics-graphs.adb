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

with Ada.Unchecked_Deallocation;
with Ada.Text_Io;

with Artics.Namet; use Artics.Namet;

package body Artics.Graphs is
   procedure Put_Line (S : String) is null; -- renames Ada.Text_IO.Put_Line;

   package body Visitor is separate;
   package body Walkers is separate;
   package body Algorithms is separate;
   package body Dump is separate;

   procedure Free is new Ada.Unchecked_Deallocation
     (Graph_Node_Record, Gnode);

   procedure Free is new Ada.Unchecked_Deallocation
     (Graph_Record, Graph);
   procedure Free is new Ada.Unchecked_Deallocation
     (Vertex_Record, Vertex);
   procedure Free is new Ada.Unchecked_Deallocation
     (Arc_Record, Arc);

   procedure Free is new Ada.Unchecked_Deallocation
     (String, String_Ptr);

   procedure Unlink_In_Arc (A : Arc);
   procedure Unlink_Out_Arc (A : Arc);

   Current_Vertex_Nb : Natural := 0;
   Current_Arc_Nb : Natural := 0;

   --------------------
   -- Get_Properties --
   --------------------

   function Get_Properties (Gn : in Gnode) return Properties_List is
   begin
     if Gn /= null then
         return Gn.Properties;
      else
         return No_Properties;
      end if;
   end Get_Properties;
   pragma Inline(Get_Properties);

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Gn   : in Gnode;
      Name : in Name_Id;
      Data : in Property_Data) is
   begin
      if Gn /= null then
         if Gn.Properties = No_Properties then
            Gn.Properties := Properties_Lists.Empty;
         end if;

         Insert_Property (Gn.Properties, Name, Data);
      end if;
   end Set_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Gn   : in Gnode;
      Name : in String;
      Data : in Property_Data) is
   begin
      Set_Property (Gn, String_Find (Name), Data);
   end Set_Property;
   pragma Inline (Set_Property);

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Gn   : in Gnode;
      Name : in Name_Id) return Property_Data is
   begin
      if Gn /= null and then Gn.Properties /= No_Properties then
         return Read_Property (Gn.Properties, Name);
      else
         return Null_Property_Data;
      end if;
   end Get_Property;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Gn   : in Gnode;
      Name : in String) return Property_Data is
   begin
      return Get_Property (Gn, String_Find (Name));
   end Get_Property;
   pragma Inline(Get_Property);

   --------------------
   -- Get_Gnode_Name --
   --------------------

   function Get_Gnode_Name (G : in Gnode) return String is
   begin
      if G.Name /= null then
         return G.Name.all;
      else
         return "";
      end if;
   end Get_Gnode_Name;

   --------------------
   -- Set_Gnode_Name --
   --------------------

   procedure Set_Gnode_Name
     (G : in Gnode;
      N : in String) is
   begin
      G.Name := new String'(N);
   end Set_Gnode_Name;

   ----------------
   -- Dump_Graph --
   ----------------

   procedure Dump_Graph (G : in Graph) is
   begin
      declare
         It : Vertex_Lists.List_Iterator :=
           Vertex_Lists.New_Iterator (G.Vertex_List);
         V  : Vertex;
      begin
         while not Vertex_Lists.Is_End (It) loop
            V := Vertex_Lists.Current_Item (It);
            if V /= Null_Vertex then
               Dump_Vertex (V);
            else
	       null;
	    end if;

            Vertex_Lists.Next (It);
         end loop;
      end;

   end Dump_Graph;

   -----------------
   -- Dump_Vertex --
   -----------------

   procedure Dump_Vertex
     (V        : in Vertex;
      Arc_Dump : in Boolean := False) is
   begin
      -- Incoming Arcs.

      declare
         It : Arc_Lists.List_Iterator := Arc_Lists.New_Iterator (V.Incoming);
         A  : Arc;
      begin
         while not Arc_Lists.Is_End (It) loop
            A := Arc_Lists.Current_Item (It);
            if A /= Null_Arc then
               if Arc_Dump then
                  Dump_Arc (A);
               else
                  null;
               end if;
            else
               null;
            end if;

            Arc_Lists.Next (It);
         end loop;
      end;

      -- Outgoing Arcs.

      declare
         It : Arc_Lists.List_Iterator := Arc_Lists.New_Iterator (V.Outgoing);
         A  : Arc;
      begin
         while not Arc_Lists.Is_End (It) loop
            A := Arc_Lists.Current_Item (It);
            if A /= Null_Arc then
               if Arc_Dump then
                  Dump_Arc (A);
               else
                  null;
               end if;
            else
               null;
            end if;

            Arc_Lists.Next (It);
         end loop;
      end;

   end Dump_Vertex;

   --------------
   -- Dump_Arc --
   --------------

   procedure Dump_Arc (A : in Arc) is
   begin
      null;
   end Dump_Arc;

   ---------------
   -- New_Graph --
   ---------------

   function New_Graph (I : in Graph_Item) Return Graph is

      G : Graph := new Graph_Record;
   begin
      G.Dump_Name := null;
      G.Gitem := I;
      G.Vertex_List := Vertex_Lists.New_List;
      return G;
   end New_Graph;

   ---------------
   -- New_Graph --
   ---------------

   function New_Graph
     (Name : in String;
      I    : in Graph_Item) return Graph is

      G  : Graph := New_Graph (I);
      Gn : Gnode := Gnode (G);
   begin
      Gn.Name := new String'(Name);

      return G;
   end New_Graph;

   --------------------
   -- Get_Graph_Name --
   --------------------

   function Get_Graph_Name (G : in Graph) return String is
   begin
      return Get_Gnode_Name (Gnode (G));
   end Get_Graph_Name;

   --------------------
   -- Set_Graph_Name --
   --------------------

   procedure Set_Graph_Name
     (G : in Graph;
      N : in String) is
   begin
      Set_Gnode_Name (Gnode (G), N);
   end Set_Graph_Name;

   --------------
   -- Set_Item --
   --------------

   procedure Set_Item
     (G : in Graph;
      I : in Graph_Item) is
   begin
      G.GItem := I;
   end Set_Item;

   --------------
   -- Get_Item --
   --------------

   function Get_Item (G : in Graph) return Graph_Item is
   begin
      return G.GItem;
   end Get_Item;

   ------------------------
   -- Reinitialize_Graph --
   ------------------------

   procedure Reinitialize_Graph
     (G : in out Graph )
   is
      VL : Vertex_Lists.List_Id := Get_Vertex_List (G);
      It : Vertex_Lists.List_Iterator :=
        Vertex_Lists.New_Iterator (VL);
      Crt_V : Vertex := Vertex_Lists.Current_Item (It);
      use Vertex_Lists ;
   begin
      while not Is_End (It) loop
         Mark (Crt_V, False);
         Next (It);
         Crt_V := Current_Item (It);
      end loop;
      Delete_Iterator (It);
   end Reinitialize_Graph;

   -------------------
   -- Destroy_Graph --
   -------------------

   procedure Destroy_Graph (G : in out Graph) is

      Git     : Vertex_Lists.List_Iterator := New_Iterator (G.Vertex_List);
      Current : Vertex;
   begin
      Reset (Git);
      while not Is_End (Git) loop
         Current := Current_Item (Git);
         Next (Git);
         Destroy_Vertex (Current);
      end loop;

      Delete_Iterator (Git);
      Free (G);
   end Destroy_Graph;

   ----------------
   -- Add_Vertex --
   ----------------

   procedure Add_Vertex
     (G : in Graph;
      V : in Vertex) is
   begin
      -- PGI: Commented to try to solve bug#409 must be checked by jma
      -- PFE: Uncommented to try to reverse to a not so old version which was
      -- able to generate graphe... and all the rest of S7.scl
      -- DCH: Commented to solve some graphcet building issues in tsx2glips?
      -- We should have a discussion about this code line...
      --V.Enclosing  := G;

      Append (V, G.Vertex_List);

      Set_Enclosing_Graph (V, G);
   end Add_Vertex;

   -------------
   -- Add_Arc --
   -------------

   procedure Add_Arc
     (G : in Graph;
      A : in Arc) is

      From : Vertex := Get_Source_Vertex (A);
      To   : Vertex := Get_Target_Vertex (A);
   begin
      if From = Null_Vertex then
         raise Program_Error;
      end if;
      if To = Null_Vertex then
         raise Program_Error;
      end if;

      Append (A, From.Outgoing);
      Append (A, To.Incoming);

      Set_Enclosing_Graph (A, G);
      --Inc_In_Count (To);
      --Inc_Out_Count (From);
   end Add_Arc;

   -------------------
   -- Remove_Vertex --
   -------------------

   procedure Remove_Vertex
     (G : in Graph;
      V : in Vertex) is

   begin
      -- Remove the link of all Vertex that have an outgoing
      -- link to the vertex V.

      if V.Incoming /= Arc_Lists.No_List then
         declare
            Init  : Arc_Lists.List_Iterator := New_Iterator (V.Incoming);
         begin
            Reset (Init);
            while not Is_End (Init) loop
               Unlink_In_Arc (Current_Item (Init));
               Next (Init);
            end loop;

            Delete_Iterator (Init);
            Clear_List (V.Incoming);
         end;
      end if;

      -- Remove the link of all Vertex that have an ingoing
      -- link to the vertex V.

      if V.Outgoing /= Arc_Lists.No_List then
         declare
            Outit : Arc_Lists.List_Iterator := New_Iterator (V.Outgoing);
         begin
            Reset (Outit);
            while not Is_End (Outit) loop
               Unlink_Out_Arc (Current_Item (Outit));
               Next (Outit);
            end loop;

            Delete_Iterator (Outit);
            Clear_List (V.Outgoing);
         end;
      end if;

      -- Remove the vertex from the vertex list of the graph.

      if G.Vertex_List /= Vertex_Lists.No_List then
         declare
            Git : Vertex_Lists.List_Iterator  := New_Iterator (G.Vertex_List);
         begin
            Reset (Git);
            while not Is_End (Git) loop
               if Current_Item (Git) = V then
                  Remove_Current_Item (Git);
                  exit;
               end if;
               Next (Git);
            end loop;

            Delete_Iterator (Git);
         end;
      end if;
   end Remove_Vertex;

   -------------------
   -- Unlink_In_Arc --
   -------------------

   procedure Unlink_In_Arc (A : Arc) is

      V : Vertex := Get_Source_Vertex (A);
   begin
      if V /= Null_Vertex then
         if V.Outgoing /= Arc_Lists.No_List then
            declare
               Outit : Arc_Lists.List_Iterator := New_Iterator (V.Outgoing);
               Current : Arc;
            begin
               Reset (Outit);
               while not Is_End (Outit) loop
                  Current := Current_Item(Outit);
                  if Current_Item (Outit) = A then
                     Remove_Current_Item (Outit);
                     exit;
                  end if;
                  Next (Outit);
               end loop;
            exception
               when E: others =>
                  ----Debug.Unknown_Exception (E, "Graph.Unlink_In_Arc-begin()");
                  raise;
            end;
         end if;
      end if;
   exception
      when E: others =>
         ------Debug.Unknown_Exception (E, "Graph.Unlink_In_Arc()");
         raise;
   end Unlink_In_Arc;

   --------------------
   -- Unlink_Out_Arc --
   --------------------

   procedure Unlink_Out_Arc (A : Arc) is

      V : Vertex := Get_Target_Vertex (A);
   begin
      if V /= Null_Vertex then
         if V.Incoming /= Arc_Lists.No_List then
            declare
               Init : Arc_Lists.List_Iterator := New_Iterator (V.Incoming);
            begin
               Reset (Init);
               while not Is_End (Init) loop
                  if Current_Item (Init) = A then
                     Remove_Current_Item (Init);
                     exit;
                  end if;
                  Next (Init);
               end loop;
            end;
         end if;
      end if;
   exception
      when E: others =>
         --Debug.Unknown_Exception (E, "Graph.Unlink_Out_Arc()");
         raise;
   end Unlink_Out_Arc;

   ----------------
   -- Remove_Arc --
   ----------------

   procedure Remove_Arc
     (G : in Graph;
      A : in Arc) is

   begin
      if A /= Null_Arc then
         Unlink_In_Arc (A);
         Unlink_Out_Arc (A);
      end if;
   exception
      when E: others =>
         --Debug.Unknown_Exception (E, "Graph.Remove_Arc()");
         raise;
   end Remove_Arc;

   ------------------------
   -- Number_Of_Vertices --
   ------------------------

   function Number_Of_Vertices (G : in Graph) return Natural is
   begin
      return Length (G.Vertex_List);
   end Number_Of_Vertices;

   --------------------
   -- Number_Of_Arcs --
   --------------------

   function Number_Of_Arcs (G : in Graph) return Natural is

      Count : Natural := 0;
   begin
      return Count;
   end Number_Of_Arcs;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (G : in Graph) return Boolean is
   begin
      return G = Null_Graph;
   end Is_Empty;

   ---------------------
   -- Get_Vertex_List --
   ---------------------

   function Get_Vertex_List (G : in Graph) return Vertex_Lists.List_Id is
   begin
      return G.Vertex_List;
   end Get_Vertex_List;

   --------------------
   -- Is_Root_Vertex --
   --------------------

   function Is_Root_Vertex
     (G : in Graph;
      V : in Vertex) return Boolean is
   begin
      if G.Vertex_List /= Vertex_Lists.No_List then
         declare
            It : Vertex_Lists.List_Iterator :=
              Vertex_Lists.New_Iterator (G.Vertex_List);
         begin
            if not Vertex_Lists.Is_End (It)
              and then V /= Null_Vertex
              and then Current_Item (It) = V then
               return True;
            end if;
         end;
      end if;

      return False;
   end Is_Root_Vertex;

   ---------------------
   -- Get_Root_Vertex --
   ---------------------

   function Get_Root_Vertex (G : in Graph) return Vertex is
   begin
      if G.Vertex_List /= Vertex_Lists.No_List then
         declare
            It : Vertex_Lists.List_Iterator :=
              Vertex_Lists.New_Iterator (G.Vertex_List);
         begin
            if not Vertex_Lists.Is_End (It) then
               return Current_Item (It);
            end if;
         end;
      end if;

      return Null_Vertex;
   end Get_Root_Vertex;

   ---------------------
   -- Set_Root_Vertex --
   ---------------------

   procedure Set_Root_Vertex
     (G    : in Graph;
      Root : in Vertex) is

      L1    : Vertex_Lists.List_Id := Vertex_Lists.New_List;
      L2    : Vertex_Lists.List_Id := Vertex_Lists.New_List;
      L     : Vertex_Lists.List_Id;
      It    : Vertex_Lists.List_Iterator :=
        Vertex_Lists.New_Iterator (G.Vertex_List);
      Vc    : Vertex;
      Split : Boolean;
   begin
      Split := False;
      while not Vertex_Lists.Is_End (It) loop
         Vc := Vertex_Lists.Current_Item (It);
         if Vc = Root then
            Split := True;
         end if;

         if Split then
            Vertex_Lists.Append (Vc, L1);
         else
            Vertex_Lists.Append (Vc, L2);
         end if;

         Vertex_Lists.Next (It);
      end loop;

      if L1 = Vertex_Lists.No_List then
         L := L2;
         Vertex_Lists.Delete_List (L1);
      elsif L2 = Vertex_Lists.No_List then
         L := L1;
         Vertex_Lists.Delete_List (L2);

      else
         L := L1;
         Vertex_Lists.Append_List (L2, L);
         Vertex_Lists.Delete_List (L2);
      end if;

      Vertex_Lists.Delete_List (G.Vertex_List);
      G.Vertex_List := L;
   end Set_Root_Vertex;

   ------------------
   -- First_Vertex --
   ------------------

   function First_Vertex (G : in Graph) return Vertex_Id is
   begin
      return Vertex_Id (Vertex_Lists.First (G.Vertex_List));
   end First_Vertex;

   -----------------
   -- Last_Vertex --
   -----------------

   function Last_Vertex (G : in Graph) return Vertex_Id is
   begin
      return Vertex_Id (Vertex_Lists.Last (G.Vertex_List));
   end Last_Vertex;

   -----------------
   -- Next_Vertex --
   -----------------

   procedure Next_Vertex (Vid : in out Vertex_Id) is
   begin
      Vertex_Lists.Next (Vertex_Lists.Item_Id (Vid));
   end Next_Vertex;

   ------------------
   -- Previous_Arc --
   ------------------

   procedure Previous_Vertex (Vid : in out Vertex_Id) is
   begin
      Vertex_Lists.Previous (Vertex_Lists.Item_Id (Vid));
   end Previous_Vertex;

   ----------------
   -- Get_Vertex --
   ----------------

   function Get_Vertex (Vid : in Vertex_Id) return Vertex is
   begin
      return Vertex_Lists.Current_Item (Vertex_Lists.Item_Id (Vid));
   end Get_Vertex;

   ---------------
   -- Is_Member --
   ---------------

   function Is_Member
     (G : in Graph;
      V : in Vertex) return Boolean is

   begin
      return V.Enclosing = G;
   end Is_Member;

   ---------------
   -- Is_Member --
   ---------------

   function Is_Member
     (G : in Graph;
      V : in Vertex_Item) return Boolean
   is
      VL  : Vertex_Lists.List_Id       := Get_Vertex_List (G);
      It  : Vertex_Lists.List_Iterator := New_Iterator (VL);
      Crt : Vertex                     := Null_Vertex;
   begin
      Vertex_Lists.Reset(It);
      while not Vertex_Lists.Is_End (It)  loop
         Crt := Vertex_Lists.Current_Item (It);
         if Get_Item (Crt) = V then
            return True;
         end if;
         Vertex_Lists.Next (It);
      end loop;
      return False;
   end Is_Member;

   -----------------------
   -- Get_Member_Vertex --
   -----------------------

   function Get_Member_Vertex
     (G : in Graph;
      V : in Vertex_Item ) return Vertex
   is
      VL  : Vertex_Lists.List_Id       := Get_Vertex_List (G);
      It  : Vertex_Lists.List_Iterator := New_Iterator (VL);
      Crt : Vertex                     := Null_Vertex;
   begin
      Vertex_Lists.Reset(It);
      while not Vertex_Lists.Is_End (It)  loop
         Crt := Vertex_Lists.Current_Item (It);
         if Get_Item (Crt) = V then
            return Crt;
         end if;
         Vertex_Lists.Next (It);
      end loop;

      return Null_Vertex;
   end Get_Member_Vertex;

   ---------------
   -- Is_Member --
   ---------------

   function Is_Member
     (G : in Graph;
      A : in Arc) return Boolean is

   begin
      return A.Enclosing = G;
   end Is_Member;

   ---------------
   -- Is_Member --
   ---------------

   function Is_Member
     (G    : in Graph;
      Src  : in Vertex;
      Dest : in Vertex)
     return Boolean
   is
      Outit   : Arc_Lists.List_Iterator := New_Iterator (Src.Outgoing);
      Crt_Arc : Arc                     := Null_Arc;
   begin
      while not Arc_Lists.Is_End (OutIt) loop
         Crt_Arc := Arc_Lists.Current_Item (OutIt);

         if Get_Target_Vertex (Crt_Arc) = Dest then
            return True;
         end if;

         Arc_Lists.Next (OutIt);
      end loop;

      return False;
   end Is_Member;

   ---------------
   -- Is_Member --
   ---------------

   function Is_Member
     (G    : in Graph;
      Src  : in Vertex;
      Dest : in Vertex;
      A    : in Arc_Item)
     return Boolean
   is
      Outit   : Arc_Lists.List_Iterator := New_Iterator (Src.Outgoing);
      Crt_Arc : Arc                     := Null_Arc;
   begin
      while not Arc_Lists.Is_End (OutIt) loop
         Crt_Arc := Arc_Lists.Current_Item (OutIt);

         if Get_Target_Vertex (Arc (Crt_Arc)) = Dest
           and Get_Item (Crt_Arc) = A
         then
            return True;
         end if;

         Arc_Lists.Next (OutIt);
      end loop;

      return False;
   end Is_Member;


   -----------------------
   -- Vertex operations --
   -----------------------

   function New_Vertex (I : in Vertex_Item) return Vertex is

      V : Vertex := new Vertex_Record;
   begin
      Current_Vertex_Nb := Current_Vertex_Nb + 1;
      V.Vertex_Nb := Current_Vertex_Nb;
      V.Item       := I;
      V.Kind       := Normal_Vertex;
      V.Enclosing  := Null_Graph;
      V.Incoming   := Arc_Lists.New_List;
      V.Outgoing   := Arc_Lists.New_List;
      V.Mark       := False;
      V.To_Root    := 0;
      V.To_Deep    := 0;
      V.Tree_Cover := 0;

      return V;
   end New_Vertex;

   --------------------
   -- Destroy_Vertex --
   --------------------

   procedure Destroy_Vertex (V : in out Vertex) is
   begin
      Remove_Vertex (V.Enclosing, V);
      Free (V);
   exception
      when E: others =>
         --Debug.Unknown_Exception(E, "Graph.Destroy_Vertex()");
         raise;
   end Destroy_Vertex;

   -------------------
   -- Get_Vertex_Nb --
   -------------------

   function Get_Vertex_Nb (V : in Vertex) return Natural is
   begin
      return V.Vertex_Nb;
   end Get_Vertex_Nb;

   -------------------
   -- Set_Vertex_Nb --
   -------------------

   procedure Set_Vertex_Nb
     (V : in Vertex;
      N : in Natural) is
   begin
      V.Vertex_Nb := N;
   end Set_Vertex_Nb;

   ---------------------
   -- Get_Vertex_Name --
   ---------------------

   function Get_Vertex_Name (V : in Vertex) return String is
   begin
      return Get_Gnode_Name (Gnode (V));
   end Get_Vertex_Name;

   ---------------------
   -- Set_Vertex_Name --
   ---------------------

   procedure Set_Vertex_Name
     (V : in Vertex;
      N : in String) is
   begin
      Set_Gnode_Name (Gnode (V), N);
   end Set_Vertex_Name;

   --------------
   -- Set_Item --
   --------------

   procedure Set_Item
     (V : in Vertex;
      I : in Vertex_Item) is
   begin
      V.Item := I;
   end Set_Item;

   --------------
   -- Get_Item --
   --------------

   function Get_Item (V : in Vertex) return Vertex_Item is
   begin
      return V.Item;
   end Get_Item;

   --------------
   -- Get_Kind --
   --------------

   function Get_Kind (V : in Vertex)  return Vertex_Kind is
   begin
      return V.Kind;
   end Get_Kind;

   --------------
   -- Set_Kind --
   --------------

   procedure Set_Kind
     (V    : in out Vertex;
      Kind : in Vertex_Kind)
   is
   begin
      V.Kind := Kind;
   end Set_Kind;

   ---------------------
   -- Enclosing_Graph --
   ---------------------

   function Enclosing_Graph (V : in Vertex) return Graph is
   begin
      return V.Enclosing;
   end Enclosing_Graph;

   -------------------------
   -- Set_Enclosing_Graph --
   -------------------------

   procedure Set_Enclosing_Graph
     (V : in Vertex;
      G : in Graph) is
   begin
      V.Enclosing := G;
   end Set_Enclosing_Graph;

   -------------------------------
   -- Get_Source_List_Of_Vertex --
   -------------------------------

   function Get_Source_List_Of_Vertex
     (V : in Vertex) return Vertex_Lists.List_Id is

      L  : Arc_Lists.List_Id;
      Sl : Vertex_Lists.List_Id;
   begin
      Sl := Vertex_Lists.No_List;

      L := Get_Incoming_Arcs (V);

      if  not Is_Empty_List (L) then
         declare
            It   : Arc_Lists.List_Iterator := Arc_Lists.New_Iterator (L);
            A    : Arc;
            From : Vertex;
         begin
            while not Arc_Lists.Is_End (It) loop
               A := Arc_Lists.Current_Item (It);

               From := Get_Source_Vertex (A);
               if From /= Null_Vertex then
                  if Sl = Vertex_Lists.No_List then
                     Sl := Vertex_Lists.New_List;
                  end if;

                  Vertex_Lists.Append (From, Sl);
               end if;

               Arc_Lists.Next (It);
            end loop;
         end;
      end if;

      return Sl;
   end Get_Source_List_Of_Vertex;

   -------------------------------
   -- Get_Target_List_Of_Vertex --
   -------------------------------

   function Get_Target_List_Of_Vertex
     (V : in Vertex) return Vertex_Lists.List_Id is

      L  : Arc_Lists.List_Id;
      Tl : Vertex_Lists.List_Id;
   begin
      Tl := Vertex_Lists.No_List;

      L := Get_Outgoing_Arcs (V);

      if  not Is_Empty_List (L) then
         declare
            It : Arc_Lists.List_Iterator := Arc_Lists.New_Iterator (L);
            A  : Arc;
            To : Vertex;
         begin
            while not Arc_Lists.Is_End (It) loop
               A := Arc_Lists.Current_Item (It);

               To := Get_Target_Vertex (A);
               if To /= Null_Vertex then
                  if Tl = Vertex_Lists.No_List then
                     Tl := Vertex_Lists.New_List;
                  end if;

                  Vertex_Lists.Append (To, Tl);
               end if;

               Arc_Lists.Next (It);
            end loop;
         end;
      end if;

      return Tl;
   end Get_Target_List_Of_Vertex;

   -----------------------
   -- Get_Incoming_Arcs --
   -----------------------

   function Get_Incoming_Arcs (V : in Vertex ) return Arc_Lists.List_Id is
   begin
      return V.Incoming;
   end Get_Incoming_Arcs;

   -----------------------
   -- Get_Outgoing_Arcs --
   -----------------------

   function Get_Outgoing_Arcs (V : in Vertex ) return Arc_Lists.List_Id is
   begin
      return V.Outgoing;
   end Get_Outgoing_Arcs;

   ----------------------------
   -- Get_First_Outgoing_Arc --
   ----------------------------

   function Get_First_Outgoing_Arc (V : in Vertex) return Arc is
   begin
      if not Arc_Lists.Is_Empty_List (V.Outgoing) then
         return Arc_Lists.First_Item (V.Outgoing);
      end if;
      return Null_Arc;
   end Get_First_Outgoing_Arc;

   -------------------------
   -- Set_Branches_Record --
   -------------------------
   procedure Set_Branches_Record
     (V  : in out Vertex;
      Br : in Branches_Record )
   is
   begin
      V.BR := Br;
   end Set_Branches_Record;

   -------------------------
   -- Get_Branches_Record --
   -------------------------
   function Get_Branches_Record
     (V  : in Vertex) return Branches_Record
   is
   begin
      return V.BR ;
   end Get_Branches_Record;
   ------------------
   -- Has_Branches --
   ------------------

   function Has_Branches
     (V : in Vertex) return Boolean
   is
      BR : Branches_Record := Get_Branches_Record (V);
   begin
      return (BR.Free_Idx /= BR.Arr'First);
   end Has_Branches;

   ---------------------
   -- Branches_Number --
   ---------------------

   function Branches_Number (V : in Vertex) return Integer
   is
      BR : Branches_Record := Get_Branches_Record (V);
   begin
      return (Br.Free_Idx - 1);
   end Branches_Number;

   -----------------------------------
   -- Add_Branch_To_Branches_Record --
   -----------------------------------

   procedure Add_Branch_To_Branches_Record
     (V  : in out Vertex;
      BR : in Branch_Elmt_Lists.List_Id)
   is
      Br_Rec : Branches_Record := Get_Branches_Record (V);
   begin
      Br_Rec.Arr (Br_Rec.Free_Idx) := BR;
      Br_Rec.Free_Idx := Br_Rec.Free_Idx + 1;
   end Add_Branch_To_Branches_Record;

   ------------------
   -- First_In_Arc --
   ------------------

   function First_In_Arc (V : in Vertex) return Arc is
   begin
      return Arc_Lists.First_Item (V.Incoming);
   end First_In_Arc;

   -----------------
   -- Last_In_Arc --
   -----------------

   function Last_In_Arc (V : in vertex) return Arc is
   begin
      return Arc_Lists.Last_Item (V.Incoming);
   end Last_In_Arc;

   -------------------
   -- First_Out_Arc --
   -------------------

   function First_Out_Arc (V : in Vertex) return Arc is
   begin
      return Arc_Lists.First_Item (V.Outgoing);
   end First_Out_Arc;

   ------------------
   -- Last_Out_Arc --
   ------------------

   function Last_Out_Arc (V : in vertex) return Arc is
   begin
      return Arc_Lists.Last_Item (V.Outgoing);
   end Last_Out_Arc;

   ------------------
   -- First_In_Arc --
   ------------------

   function First_In_Arc (V : in Vertex) return Arc_Id is
   begin
      return Arc_Id (Arc_Lists.First (V.Incoming));
   end First_In_Arc;

   -----------------
   -- Last_In_Arc --
   -----------------

   function Last_In_Arc (V : in vertex) return Arc_Id is
   begin
      return Arc_Id (Arc_Lists.Last (V.Incoming));
   end Last_In_Arc;

   -------------------
   -- First_Out_Arc --
   -------------------

   function First_Out_Arc (V : in Vertex) return Arc_Id is
   begin
      return Arc_Id (Arc_Lists.First (V.Outgoing));
   end First_Out_Arc;

   ------------------
   -- Last_Out_Arc --
   ------------------

   function Last_Out_Arc (V : in vertex) return Arc_Id is
   begin
      return Arc_Id (Arc_Lists.Last (V.Outgoing));
   end Last_Out_Arc;

   --------------
   -- Next_Arc --
   --------------

   procedure Next_Arc (Aid : in out Arc_Id) is
   begin
      Arc_Lists.Next (Arc_Lists.Item_Id (Aid));
   end Next_Arc;

   ------------------
   -- Previous_Arc --
   ------------------

   procedure Previous_Arc (Aid : in out Arc_Id) is
   begin
      Arc_Lists.Previous (Arc_Lists.Item_Id (Aid));
   end Previous_Arc;

   -------------
   -- Get_Arc --
   -------------

   function Get_Arc (Aid : in Arc_Id) return Arc is
   begin
      return Arc_Lists.Current_Item ( Arc_Lists.Item_Id (Aid));
   end Get_Arc;

   --------------------------
   -- Get_Distance_To_Deep --
   --------------------------

   function Get_Distance_To_Deep (V : in Vertex) return Natural is
   begin
      return V.To_Deep;
   end Get_Distance_To_Deep;

   --------------------------
   -- Set_Distance_To_Deep --
   --------------------------

   procedure Set_Distance_To_Deep
     (V : in Vertex;
      D : in Natural) is
   begin
      V.To_Deep := D;
   end Set_Distance_To_Deep;

   --------------------------
   -- Get_Distance_To_Root --
   --------------------------

   function Get_Distance_To_Root (V : in Vertex) return Natural is
   begin
      return V.To_Root;
   end Get_Distance_To_Root;

   --------------------------
   -- Set_Distance_To_Root --
   --------------------------

   procedure Set_Distance_To_Root
     (V : in Vertex;
      D : in Natural) is
   begin
      V.To_Root := D;
   end Set_Distance_To_Root;

   --------------------
   -- Get_Tree_Cover --
   --------------------

   function Get_Tree_Cover (V : in Vertex) return Natural is
   begin
      return V.Tree_Cover;
   end Get_Tree_Cover;

   --------------------
   -- Set_Tree_Cover --
   --------------------

   procedure Set_Tree_Cover
     (V : in Vertex;
      C : in Natural) is
   begin
      V.Tree_Cover := C;
   end Set_Tree_Cover;

   ----------
   -- Mark --
   ----------

   procedure Mark
     (V : in Vertex;
      B : in Boolean) is
   begin
      V.Mark := B;
   end Mark;

   ------------
   -- Marked --
   ------------

   function Marked (V : in Vertex) return Boolean is
   begin
      return V.Mark;
   end Marked;

   ----------------
   -- Unmark_All --
   ----------------

   procedure Unmark_All (G : in Graph) is
      Vit : Vertex_Lists.List_Iterator := New_Iterator (G.Vertex_List);
   begin
      while not Is_End (Vit) loop
         Mark (Current_Item (Vit), False);
         Next (Vit);
      end loop;
   end Unmark_All;

   -------------
   -- New_Arc --
   -------------

   function New_Arc
     (From : in Vertex;
      To   : in Vertex;
      I    : in Arc_Item) return Arc is

      A : Arc := new Arc_Record;
   begin
      Current_Arc_Nb := Current_Arc_Nb + 1;
      A.Arc_Nb := Current_Arc_Nb;

      A.Item := I;
      A.Enclosing := Null_Graph;
      A.From := From;
      A.To := To;

      if To = Null_Vertex and From = Null_Vertex then
         A.Enclosing := Null_Graph;
      elsif To = Null_Vertex then
         A.Enclosing := From.Enclosing;
      else
         A.Enclosing := To.Enclosing;
      end if;

      A.Mark := False;

      return A;
   end New_Arc;

   -----------------
   -- Destroy_Arc --
   -----------------

   procedure Destroy_Arc (A : in out Arc) is
   begin
      Remove_Arc (A.Enclosing, A);
      --Debug.Output(true, "Graph.Destroy_Arc() 1");
      Free (A);
   exception
      when E: others =>
         --Debug.Unknown_Exception(E, "Graph.Destroy_Arc()");
         raise;
   end Destroy_Arc;

   -----------------------------------
   -- Make_Arcs_From_To_Vertex_List --
   -----------------------------------

   procedure Make_Arcs_From_To_Vertex_List
     (G    : in Graph;
      From : in Vertex;
      To_L : in Vertex_Lists.List_Id) is

   begin
      if Vertex_Lists.Is_Empty_List (To_L) then
         return;
      end if;


      declare
         It : Vertex_Lists.List_Iterator := Vertex_Lists.New_Iterator (To_L);
         To : Vertex;
         A  : Arc;
      begin
         while not Vertex_Lists.Is_End (It) loop
            To := Vertex_Lists.Current_Item (It);

            A := New_Arc (From, To, No_Arc_Item);
            Add_Arc (G, A);

            Vertex_Lists.Next (It);
         end loop;
      end;
   end Make_Arcs_From_To_Vertex_List;

   -----------------------------------
   -- Make_Arcs_To_From_Vertex_List --
   -----------------------------------

   procedure Make_Arcs_To_From_Vertex_List
     (G      : in Graph;
      To     : in Vertex;
      From_L : in Vertex_Lists.List_Id) is

   begin
      if Vertex_Lists.Is_Empty_List (From_L) then
         return;
      end if;


      declare
         It : Vertex_Lists.List_Iterator := Vertex_Lists.New_Iterator (From_L);
         From : Vertex;
         A    : Arc;
      begin
         while not Vertex_Lists.Is_End (It) loop
            From := Vertex_Lists.Current_Item (It);

            A := New_Arc (From, To, No_Arc_Item);
            Add_Arc (G, A);

            Vertex_Lists.Next (It);
         end loop;
      end;
   end Make_Arcs_To_From_Vertex_List;

   ----------------
   -- Get_Arc_Nb --
   ----------------

   function Get_Arc_Nb (A : in Arc) return Natural is
   begin
      return A.Arc_Nb;
   end Get_Arc_Nb;

   ----------------
   -- Set_Arc_Nb --
   ----------------

   procedure Set_Arc_Nb
     (A : in Arc;
      N : in Natural) is
   begin
      A.Arc_Nb := N;
   end Set_Arc_Nb;

   ------------------
   -- Get_Arc_Name --
   ------------------

   function Get_Arc_Name (A : in Arc) return String is
   begin
      return Get_Gnode_Name (Gnode (A));
   end Get_Arc_Name;

   ------------------
   -- Set_Arc_Name --
   ------------------

   procedure Set_Arc_Name
     (A : in Arc;
      N : in String) is
   begin
      Set_Gnode_Name (Gnode (A), N);
   end Set_Arc_Name;

   --------------
   -- Set_Item --
   --------------

   procedure Set_Item
     (A : in Arc;
      I : in Arc_Item) is
   begin
      A.Item := I;
   end Set_Item;

   --------------
   -- Get_Item --
   --------------

   function Get_Item (A : in Arc) return Arc_Item is
   begin
      return A.Item;
   end Get_Item;

   -----------------------
   -- Get_Source_Vertex --
   -----------------------

   function Get_Source_Vertex (A : in Arc) return Vertex is
   begin
      return A.From;
   end Get_Source_Vertex;

   -----------------------
   -- Set_Source_Vertex --
   -----------------------

   procedure Set_Source_Vertex
     (A : in Arc;
      V : in Vertex) is
   begin
      A.From := V;
   end Set_Source_Vertex;

   -----------------------
   -- Get_Target_Vertex --
   -----------------------

   function Get_Target_Vertex (A : in Arc) return Vertex is
   begin
      if A = Null_Arc then
         return Null_Vertex;
      else
         return A.To;
      end if;
   end Get_Target_Vertex;

   -----------------------
   -- Set_Target_Vertex --
   -----------------------

   procedure Set_Target_Vertex
     (A : in Arc;
      V : in Vertex) is
   begin
      A.To := V;
   end Set_Target_Vertex;

   ----------------------------
   -- Redirect_Source_Vertex --
   ----------------------------

   procedure Redirect_Source_Vertex
     (A : in Arc;
      V : in Vertex) is
   begin
      if V = null then
         raise Program_Error;
      end if;

      Unlink_In_Arc (A);
      Set_Source_Vertex (A, V);
      Append (A, V.Outgoing);
   end Redirect_Source_Vertex;

   ----------------------------
   -- Redirect_Target_Vertex --
   ----------------------------

   procedure Redirect_Target_Vertex
     (A : in Arc;
      V : in Vertex) is
   begin
      if V = null then
         raise Program_Error;
      end if;

      Unlink_Out_Arc (A);
      Set_Target_Vertex (A, V);
      Append (A, V.Incoming);
   end Redirect_Target_Vertex;

   -----------------------------
   -- Target_Distance_To_Deep --
   -----------------------------

   function Target_Distance_To_Deep (A : in Arc) return Natural is
   begin
      return Get_Distance_To_Deep (Get_Target_Vertex (A));
   end Target_Distance_To_Deep;

   -----------------------------
   -- Source_Distance_To_Deep --
   -----------------------------

   function Source_Distance_To_Deep (A : in Arc) return Natural is
   begin
      return Get_Distance_To_Deep (Get_Source_Vertex (A));
   end Source_Distance_To_Deep;

   -----------------------------
   -- Target_Distance_To_Root --
   -----------------------------

   function Target_Distance_To_Root (A : in Arc) return Natural is
   begin
      return Get_Distance_To_Root (Get_Target_Vertex (A));
   end Target_Distance_To_Root;

   -----------------------------
   -- Source_Distance_To_Root --
   -----------------------------

   function Source_Distance_To_Root (A : in Arc) return Natural is
   begin
      return Get_Distance_To_Root (Get_Source_Vertex (A));
   end Source_Distance_To_Root;

   -----------------------
   -- Target_Tree_Cover --
   -----------------------

   function Target_Tree_Cover (A : in Arc) return Natural is
   begin
      return Get_Tree_Cover (Get_Target_Vertex (A));
   end Target_Tree_Cover;

   -----------------------
   -- Source_Tree_Cover --
   -----------------------

   function Source_Tree_Cover (A : in Arc) return Natural is
   begin
      return Get_Tree_Cover (Get_Source_Vertex (A));
   end Source_Tree_Cover;

   ----------
   -- Mark --
   ----------

   procedure Mark
     (A : in Arc;
      B : in Boolean) is
   begin
      A.Mark := B;
   end Mark;

   ------------
   -- Marked --
   ------------

   function Marked (A : in Arc) return Boolean is
   begin
      return A.Mark;
   end Marked;

   ------------------
   -- Vertex_Degre --
   ------------------

   function Vertex_Degre (V : in Vertex) return Natural is
   begin
      return Vertex_Internal_Degre (V) + Vertex_External_Degre (V);
   end Vertex_Degre;

   ---------------------------
   -- Vertex_Internal_Degre --
   ---------------------------

   function Vertex_Internal_Degre (V : in Vertex) return Natural is
   begin
      return Length (V.Incoming);
   end Vertex_Internal_Degre;

   ---------------------------
   -- Vertex_External_Degre --
   ---------------------------

   function Vertex_External_Degre (V : in Vertex) return Natural is
   begin
      return Length (V.Outgoing);
   end Vertex_External_Degre;

   ---------------------
   -- Enclosing_Graph --
   ---------------------

   function Enclosing_Graph (A : in Arc) return Graph is
   begin
      return A.Enclosing;
   end Enclosing_Graph;

   -------------------------
   -- Set_Enclosing_Graph --
   -------------------------

   procedure Set_Enclosing_Graph
     (A : in Arc;
      G : in Graph) is
   begin
      A.Enclosing := G;
   end Set_Enclosing_Graph;

end Artics.Graphs;
