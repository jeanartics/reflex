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

with Artics.Graph.Cells;  use Artics.Graph.Cells;
with Artics.Graph.Graphs; use Artics.Graph.Graphs;

with Sem_Util; use Sem_Util;
with Nlists;   use Nlists;
with Types;    use Types;
with Sinfo;    use Sinfo;
with Atree;    use Atree;
with Namet;    use Namet;
with Einfo;    use Einfo;

with Reflex.Fbd_Placements; use Reflex.Fbd_Placements;
with Reflex.Fbd_Dispatch;   use Reflex.Fbd_Dispatch;
with Reflex.Vertex_Value;   use Reflex.Vertex_Value;
with Reflex.Edge_Value;     use Reflex.Edge_Value;
with Reflex.Fbd_Util;       use Reflex.Fbd_Util;
with Reflex.Infos;          use Reflex.Infos;

package body Reflex.Fbd_Gen_Ch5 is
   
   -----------------------------
   -- Fbd_Generate_Assignment --
   -----------------------------
   
   procedure Fbd_Generate_Assignment
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is
            
      Lhs_Node : Node_Id := Left_Opnd (Node);
      Rhs_Node : Node_Id := Right_Opnd (Node);
            
      Edge_Rhs   : access Cell_Record;
      Edge_Value : Edge_Value_Ptr := New_Edge_Value;
      
      Formal_Out : access Cell_Record;
      Formal_In  : access Cell_Record;
      
      Graph : access Graph_Record := This.Get_Graph;
      use Cells_Lists;
   begin
      if Nkind (Rhs_Node) = N_Identifier
--          or Nkind (Rhs_Node) in N_Numeric_Or_String_Literal 
        or Nkind (Rhs_Node) = N_Op_Not then

         --  Boite spéciale pour les assignement en FBD ???
         --  A := B ??? ou Nb := 3 ??
         null;
         
      else
         Fbd_Node_Dispatch (This, Rhs_Node);
         Fbd_Node_Dispatch (This, Lhs_Node);
         
         Formal_Out := First_Element (Vertex_Value_Ptr (Get_Cell 
                                      (Rhs_Node).Get_Value).Get_Vertexs_Out);
         Formal_In := First_Element (Vertex_Value_Ptr (Get_Cell
                                     (Lhs_Node).Get_Value).Get_Vertexs_In);
         
         Vertex_Value_Ptr(Formal_Out.Get_Value).Set_Effective_Param (Lhs_Node);

         Edge_Rhs := Insert_Edge (G      => Graph,
                                  Parent => This.Get_Cell_Parent,
                                  Id     => "edge_assign",
                                  Value  => Edge_Value,
                                  Source => Formal_Out,
                                  Target => Formal_In);
      end if;
   end Fbd_Generate_Assignment;
   
   -------------------------------------------
   -- Fbd_Generate_Procedure_Call_Statement --
   -------------------------------------------
   
   procedure Fbd_Generate_Procedure_Call_Statement
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is
      
      Vertex_Enclose : access Cell_Record;
      Value_Enclose  : Vertex_Value_Ptr := New_Vertex_Value;
      
      Vertex_Proc    : access Cell_Record;
      Proc_Name      : String := Get_Name_String (Chars (Name (Node)));
      Value_Proc     : Vertex_Value_Ptr := New_Vertex_Value;
      
      Param_Node : Node_Id := First_Actual (Node);
      Param      : Entity_Id := First_Formal_With_Extras (Entity (Name (Node)));
      Param_Kind : Formal_Kind;
      
      Is_In_Param  : Boolean := False;
      Is_Out_Param : Boolean := False;
      
      Graph : access Graph_Record := This.Get_Graph;
      use Cells_Lists;
   begin      
      Value_Enclose.Set_Node (Node);
      Value_Enclose.Set_Vertex_Kind (Enclose_Vertex);
      Vertex_Enclose := Insert_Vertex (G        => Graph,
                                       Parent   => This.Get_Cell_Parent,
                                       Id       => "enclose_proc_call",
                                       Value    => Value_Enclose,
                                       X        => 0.0,
                                       Y        => 0.0,
                                       Width    => 0.0,
                                       Height   => 0.0);

      Value_Proc.Set_Node (Node);
      Value_Proc.Set_Vertex_Kind (Procedure_Call_Vertex);
      Vertex_Proc := Insert_Vertex (G        => Graph,
                                    Parent   => Vertex_Enclose,
                                    Id       => Proc_Name,
                                    Value    => Value_Proc,
                                    X        => 0.0,
                                    Y        => 0.0,
                                    Width    => 0.0,
                                    Height   => 0.0);
      Set_Cell (Node, Vertex_Proc);
      Value_Enclose.Set_Cell_To_Link (Vertex_Proc);

      while Present (Param) loop
         Param_Kind := Parameter_Mode (Param);
         if Param_Kind = E_In_Parameter then
            Is_In_Param := True;
            
         elsif Param_Kind = E_Out_Parameter then
            Is_Out_Param := True;
            
         elsif Param_Kind = E_In_Out_Parameter then
            Is_In_Param := True;
            Is_Out_Param := True;
         end if;
         
         Create_Formal_Vertex
           (This,
            Node,
            Param_Node,
            Is_In_Param,
            Is_Out_Param,
            String_Find (Get_Name_String (Chars (Param))));

         Is_In_Param := False;
         Is_Out_Param := False;
                  
         Next_Actual (Param_Node);
         Next_Formal_With_Extras (Param);
      end loop;

      Vertex_Proc.Get_Geometry.Set_Width (Calculate_Proc_Width (Node));
      Vertex_Proc.Get_Geometry.Set_Height (Calculate_Proc_Height (Node));

      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Width (Node));
      Vertex_Enclose.Get_Geometry.Set_Height (Calculate_Enclose_Height (Node));

      Place_Vertices (This, Vertex_Proc);
   end Fbd_Generate_Procedure_Call_Statement;

   ---------------------------------
   -- Fbd_Generate_Null_Statement --
   ---------------------------------
   
   procedure Fbd_Generate_Null_Statement
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is
   begin
      null;
   end Fbd_Generate_Null_Statement;
end  Reflex.Fbd_Gen_Ch5;
