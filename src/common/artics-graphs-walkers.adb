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

separate (Artics.Graphs)

package body Walkers is

   procedure Deep_Walk
     (Visit : in out Graph_Visitor_Record'Class;
      V     : in Vertex);

   procedure Sibling_Walk
     (G  : in Graph;
      V  : in Vertex;
      Sl : in Vertex_Lists.List_Id;
      Tl : in out Vertex_Lists.List_Id);

   ----------------
   -- Unmark_All --
   ----------------

   procedure Unmark_All (G : in Graph) is
      Vit  : Vertex_Lists.List_Iterator := New_Iterator (G.Vertex_List);
   begin
      while not Is_End (Vit) loop
         Mark (Current_Item (Vit), False);
         Next (Vit);
      end loop;
   end Unmark_All;

   --------------
   -- Do_Visit --
   --------------

   procedure Do_Visit
     (V : in out Graph_Visitor_Record'Class;
      G : in Graph) is
   begin
      Visit_Initialize (V, G);

      Deep_First_Walk (V, G);

      Visit_Finalize (V);
   end Do_Visit;

   ---------------------
   -- Deep_First_Walk --
   ---------------------

   procedure Deep_First_Walk
     (Visit : in out Graph_Visitor_Record'Class;
      G     : in Graph) is

      Vit  : Vertex_Lists.List_Iterator := New_Iterator (G.Vertex_List);
      Vcur : Vertex;
   begin
      Unmark_All (G);

      Visit_Graph (Visit, Before);

      while not Is_End (Vit) loop
         Vcur := Current_Item (Vit);

         if not Marked (Vcur) then
            Deep_Walk (Visit, Vcur);
         end if;

         Next (Vit);
      end loop;

      Visit_Graph (Visit, After);
   end Deep_First_Walk;

   ---------------
   -- Deep_Walk --
   ---------------

   procedure Deep_Walk
     (Visit : in out Graph_Visitor_Record'Class;
      V     : in Vertex) is

      Ait  : Arc_Lists.List_Iterator := New_Iterator (V.Outgoing);
      Vout : Vertex;
      Acur : Arc;
   begin
      Mark (V, True);
      Set_Vertex (Visit, V);
      Visit_Vertex (Visit, Before);

      while not Is_End (Ait) loop
         Acur := Current_Item (Ait);

         Set_Arc (Visit, Acur);
         Visit_Arc (Visit, Before);

         Vout := Get_Target_Vertex (Acur);

         if not Marked (Vout) then
            Deep_Walk (Visit, Vout);
         end if;

         Set_Arc (Visit, Acur);
         Visit_Arc (Visit, After);

         Next (Ait);
      end loop;

      Set_Vertex (Visit, V);
      Visit_Vertex (Visit, After);
   end Deep_Walk;

   ------------------------
   -- Sibling_First_Walk --
   ------------------------

   procedure Sibling_First_Walk
     (Visit : in out Graph_Visitor_Record'Class;
      G     : in Graph) is

      Vit  : Vertex_Lists.List_Iterator := New_Iterator (G.Vertex_List);
      Vcur : Vertex;
      Frst : Vertex;
      Tl   : Vertex_Lists.List_Id := Vertex_Lists.New_List;
      Sl   : Vertex_Lists.List_Id := Vertex_Lists.New_List;
   begin

      -- First set the vertex mark to false.

      Unmark_All (G);


      -- Then Build The Sibling First List.

      Visit_Graph (Visit, Before);

      Frst := Current_Item (Vit);
      if Frst /= Null_Vertex then
         Append (Frst, Sl);
         Next (Vit);
         while not Is_End (Vit) loop
            Vcur := Current_Item (Vit);
            if not Marked (Vcur) then
               Sibling_Walk (G, Vcur, Tl, Sl);
            end if;
            Next (Vit);
         end loop;


         -- And last Visit the list where vertex in sl are ordered in a sibling
         -- first order.

         while not Is_Empty_List (Sl) loop
            Vcur := First_Item (Sl);
            Remove_First (Sl);
            Set_Vertex (Visit, Vcur);
            Visit_Vertex (Visit, Before);
            declare
               Al  : Arc_Lists.List_Id := Get_Outgoing_Arcs (Vcur);
               Ait : Arc_Lists.List_Iterator := New_Iterator (Al);
            begin
               while not Is_End (Ait) loop
                  Set_Arc (Visit, Current_Item (Ait));
                  Visit_Arc (Visit, Before);
                  Visit_Arc (Visit, After);
                  Next (Ait);
               end loop;
            end;
         end loop;
      end if;

      Visit_Graph (Visit, After);
      Delete_List (Sl);
      Delete_List (Tl);
   end Sibling_First_Walk;

   ------------------
   -- Sibling_Walk --
   ------------------

   procedure Sibling_Walk
     (G  : in Graph;
      V  : in Vertex;
      Sl : in Vertex_Lists.List_Id;
      Tl : in out Vertex_Lists.List_Id) is

      Vcur : Vertex;
   begin
      Mark (V, True);
      Append (V, Tl);
      while not Is_Empty_List (Tl) loop
         Vcur := First_Item (Tl);
         Remove_First (Tl);
         Append (Vcur, Sl);
         declare
            It : Arc_Lists.Item_Id := First (Get_Outgoing_Arcs (Vcur));
            V  : Vertex;
         begin
            while It /= Arc_Lists.No_Item_Id loop
               V := Get_Target_Vertex (Current_Item (It));
               if not Marked (V) then
                  Mark (V, True);
                  Append (V, Tl);
               end if;
               Next (It);
            end loop;
         end;
      end loop;
   end Sibling_Walk;

   -------------------------
   -- Sibling_Vertex_List --
   -------------------------

   function Sibling_Vertex_List
     (Vrtx : in Vertex) return Vertex_Lists.List_Id is

      L  : Arc_Lists.List_Id;
      Vl : Vertex_Lists.List_Id;
   begin

      Vl := Vertex_Lists.No_List;

      L := Get_Outgoing_Arcs (Vrtx);

      -- Return if there is no outgoing arc.

      if L = Arc_Lists.No_List then
         return Vl;
      end if;

      -- Walk throught all outgoing arc, and reduce the target vertex.

      declare
         It   : Arc_Lists.List_Iterator := Arc_Lists.New_Iterator (L);
         Aout : Arc;
         Vout : Vertex;
      begin
         while not Arc_Lists.Is_End (It) loop
            Aout := Arc_Lists.Current_Item (It);
            Vout := Get_Target_Vertex (Aout);

            if not Marked (Vout) then
               if Vl = Vertex_Lists.No_List then
                  Vl := Vertex_Lists.New_List;
               end if;

               Mark (Vout, True);
               Vertex_Lists.Append (Vout, VL);
            end if;

            Arc_Lists.Next (It);
         end loop;
      end;

      return Vl;
   end Sibling_Vertex_List;

   ------------
   -- T_List --
   ------------

   procedure T_List
     (SL : in Vertex_Lists.List_Id;
      TL : in Sibling_Lists.List_Id) is

      L : Vertex_Lists.List_Id;
   begin
      if Sl = Vertex_Lists.No_List then
         return;
      end if;

      Sibling_Lists.Append (Sl, Tl);

      L := Vertex_Lists.No_List;

      declare
         It : Vertex_Lists.List_Iterator := Vertex_Lists.New_Iterator (SL);
         V  : Vertex;
         VL : Vertex_Lists.List_Id;
      begin
         while not Is_End (It) loop
            V := Vertex_Lists.Current_Item (It);

            Vl := Sibling_Vertex_List (V);

            if Vl /= Vertex_Lists.No_List then
               if L = Vertex_Lists.No_List then
                  L := Vertex_Lists.New_List;
               end if;

               Vertex_Lists.Append_List (Vl, L);
            end if;

            Vertex_Lists.Next (It);
         end loop;
      end;

      if L /= Vertex_Lists.No_List then
         T_List (L, Tl);
      end if;
   end T_List;

   ------------------------
   -- Build_Sibling_List --
   ------------------------

   function Build_Sibling_List (G : in Graph) return Sibling_Lists.List_Id is

      Root : Vertex;
      Tl   : Sibling_Lists.List_Id;
      Sl   : Vertex_Lists.List_Id;
   begin

      Unmark_All (G);

      Tl := Sibling_Lists.No_List;

      Root := Get_Root_Vertex (G);

      if Root /= Null_Vertex then
         Mark (Root, True);
         Tl := Sibling_Lists.New_List;
         Sl := Vertex_Lists.New_List;

         Vertex_Lists.Append (Root, Sl);

         T_List (Sl, Tl);

      else
         TL := Sibling_Lists.No_List;
      end if;

      return Tl;
   end Build_Sibling_List;

end Walkers;
