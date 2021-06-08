-----------------------------------------------------------------------------
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

with Artics.Graph.Graphs; use Artics.Graph.Graphs;
with Artics.Graph.Cells;  use Artics.Graph.Cells;

with Sinfo; use Sinfo;
with Atree; use Atree;
with Namet; use Namet;

with Reflex.Fbd_Placements; use Reflex.Fbd_Placements;
with Reflex.Vertex_Value;   use Reflex.Vertex_Value;
with Reflex.Fbd_Util;       use Reflex.Fbd_Util;
with Reflex.Infos;          use Reflex.Infos;
with Ada.Text_IO; use Ada.Text_IO;

package body Reflex.Fbd_Gen_Ch2 is

   -----------------------------
   -- Fbd_Generate_Identifier --
   -----------------------------

   procedure Fbd_Generate_Identifier
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is

      Vertex_Enclose : access Cell_Record;
      Vertex_Formal  : access Cell_Record;
      Vertex_Ident   : access Cell_Record;

      Value_Formal  : Vertex_Value_Ptr := New_Vertex_Value;
      Value_Ident   : Vertex_Value_Ptr := New_Vertex_Value;
      Value_Enclose : Vertex_Value_Ptr := New_Vertex_Value;

      Graph : Graph_Ptr := This.Get_Graph;
   begin

      --  Create enclosing vertex for identifier

      Value_Enclose.Set_Node (Node);
      Value_Enclose.Set_Vertex_Kind (Enclose_Vertex);
      Vertex_Enclose := Insert_Vertex (G        => Graph,
                                       Parent   => This.Get_Cell_Parent,
                                       Id       => "enclose_identifier",
                                       Value    => Value_Enclose,
                                       X        => 0.0,
                                       Y        => 0.0,
                                       Width    => 0.0,
                                       Height   => 0.0);

      --  Create Vertex for identifier

      Value_Ident.Set_Node (Node);
      Value_Ident.Set_Vertex_Kind (Identifier_Vertex);
      Vertex_Ident := Insert_Vertex (G        => Graph,
                                     Parent   => Vertex_Enclose,
                                     Id       => "identifier",
                                     Value    => Value_Ident,
                                     X        => 0.0,
                                     Y        => 0.0,
                                     Width    => Identifier_Width,
                                     Height   => Identifier_Height);
      Set_Cell (Node, Vertex_Ident);
      Value_Enclose.Set_Cell_To_Link (Vertex_Ident);

      Value_Formal.Set_Vertex_Kind (Determinate_Vertex_Kind (This, Node));

      --  Create formal vertex for link

      Vertex_Formal := Insert_Vertex (G        => Graph,
                                      Parent   => Vertex_Enclose,
                                      Id       => "formal",
                                      Value    => Value_Formal,
                                      X        => 0.0,
                                      Y        => 0.0,
                                      Width    => Formal_In_Width,
                                      Height   => Formal_In_Height);

      if Value_Formal.Get_Vertex_Kind = Formal_In_Vertex then
         Vertex_Value_Ptr
           (Vertex_Ident.Get_Value).Append_Vertex_In (Vertex_Formal);
         Vertex_Formal.Set_Id ("formal_id_in");

         if Nkind (Node) = N_Identifier
           and then Is_Local_Variable_Reference (Node) then
            Append_Node (This, Node);
         end if;

      elsif Value_Formal.Get_Vertex_Kind = Formal_Out_Vertex then
         Vertex_Value_Ptr
           (Vertex_Ident.Get_Value).Append_Vertex_Out (Vertex_Formal);
         Vertex_Formal.Set_Id ("formal_id_out");

      end if;
      Value_Formal.Set_Node (Node);

      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Width (Node));
      Vertex_Enclose.Get_Geometry.Set_Height (Calculate_Enclose_Height (Node));

      Place_Vertices (This, Vertex_Ident);
   end Fbd_Generate_Identifier;

end Reflex.Fbd_Gen_Ch2;
