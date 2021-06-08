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

with Artics.Graph.Cells_Geometry; use Artics.Graph.Cells_Geometry;
with Artics.Geometry.Rectangles;  use Artics.Geometry.Rectangles;
with Artics.Graph.Graphs;         use Artics.Graph.Graphs;
with Artics.Graph.Cells;          use Artics.Graph.Cells;
with Artics.Graph.Algos;          use Artics.Graph.Algos;
with Artics.Geometry;             use Artics.Geometry;
with Artics.Objects;              use Artics.Objects;
with Artics.Maths;                use Artics.Maths;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Atree;    use Atree;
with Einfo;    use Einfo;
with Nlists;   use Nlists;
with Namet;    use Namet;

with Reflex.Fbd_Placements; use Reflex.Fbd_Placements;
with Reflex.Fbd_Dispatch;   use Reflex.Fbd_Dispatch;
with Reflex.Edge_Value;     use Reflex.Edge_Value;
with Reflex.Infos;          use Reflex.Infos;

with Ada.Text_IO; use Ada.Text_IO;

package body Reflex.Fbd_Util is
   
   --------------------------
   -- Create_Formal_Vertex --
   --------------------------
   
   procedure Create_Formal_Vertex 
     (This             : access Fbd_Builder_Record;
      Node             : Node_Id;
      Param_Node       : Node_Id;
      Is_In_Param      : Boolean;
      Is_Out_Param     : Boolean;
      Formal_Name      : Name_Id) is
    
      Cell_Current    : access Cell_Record := Get_Cell (Node);
      Cell_Enclose    : access Cell_Record := Cell_Current.Get_Parent;
      Cell_Formal_In  : access Cell_Record;
      Cell_Formal_Out : access Cell_Record;
      Value_Formal    : Vertex_Value_Ptr := New_Vertex_Value;
      Value_Formal_Out: Vertex_Value_Ptr := New_Vertex_Value;
      
      Node_To_Dispatch  : Node_Id := Param_Node;
      
      Edge       : access Cell_Record;
      Edge_Value : Edge_Value_Ptr := New_Edge_Value;

      Graph : access Graph_Record := This.Get_Graph;
      use Cells_Lists;
   begin
      
      if Is_In_Param then
         Value_Formal.Set_Vertex_Kind (Formal_In_Vertex);
         Value_Formal.Set_Formal_Name (Formal_Name);
         Cell_Formal_In := Insert_Vertex (G        => Graph,
                                          Parent   => Cell_Enclose,
                                          Id       => "formal_in",
                                          Value    => Value_Formal,
                                          X        => 0.0,
                                          Y        => 0.0,
                                          Width    => Formal_In_Width,
                                          Height   => Formal_In_Height
                                         );
         Vertex_Value_Ptr
           (Cell_Current.Get_Value).Append_Vertex_In (Cell_Formal_In);
         
         if Nkind (Node_To_Dispatch) = N_Identifier then
            Value_Formal.Set_Effective_Param (Node_To_Dispatch);

         elsif Nkind (Node_To_Dispatch) = N_Op_Not then
            Node_To_Dispatch := Right_Opnd (Node_To_Dispatch);
            Value_Formal.Set_Is_Negate_Vertex (True);
            Value_Formal.Set_Effective_Param (Node_To_Dispatch);
            
--           elsif Nkind (Node_To_Dispatch) in N_Numeric_Or_String_Literal then
--              Value_Formal.Set_Effective_Param (Node_To_Dispatch);
         end if;

         Edge := Create_Edge (G     => Graph,
                              Id    => "edge",
                              Value => Edge_Value);
         
         if Nkind (Node_To_Dispatch) = N_Identifier 
           and then Is_Local_Variable_Reference (Node_To_Dispatch) 
           and then Find_Node (This, Node_To_Dispatch) /= Empty 
           and then not Is_Out_Param 
         then
            
            declare 
               Local_Var     : Node_Id 
                 := Find_Node (This, Node_To_Dispatch);
               Formal_Source : access Cell_Record;
               Old_Cell      : access Cell_Record := Get_Cell (Local_Var);
               Old_Cell_Val  : Vertex_Value_Ptr := Old_Cell.Get_Value;
               Cells_To_Rm   : Cells_Lists.List;

            begin               
               Formal_Source
                 := First_Element (First_Element (Old_Cell_Val.Get_Vertexs_In)
                                   .Get_Edges_List).Get_Source;
              
               Append (Cells_To_Rm, Old_Cell);
               Remove_Cells (G             => Graph,
                             Cells         => Cells_To_Rm,
                             Include_Edges => True);

               Vertex_Value_Ptr 
                 (Formal_Source.Get_Value).Set_Effective_Param (Empty);
               Vertex_Value_Ptr 
                 (Cell_Formal_In.Get_Value).Set_Effective_Param (Empty);

               Add_Edge (G      => Graph,
                         Edge   => Edge,
                         Parent => This.Get_Graph.Get_Default_Parent,
                         Source => Formal_Source,
                         Target => Cell_Formal_In,
                         Index  => 1);
            end;
         else
            Fbd_Node_Dispatch (This, Node_To_Dispatch);
            declare
               Cell_To_Link : access Cell_Record;
            begin
               Cell_To_Link := First_Element 
                 (Vertex_Value_Ptr 
                    (Get_Cell (Node_To_Dispatch).Get_Value).Get_Vertexs_Out);

               Add_Edge (G      => Graph,
                         Edge   => Edge,
                         Parent => This.Get_Graph.Get_Default_Parent,
                         Source => Cell_To_Link,
                         Target => Cell_Formal_In,
                         Index  => 1);
            end;
         end if;
      end if;
      
      if Is_Out_Param then
         
         Value_Formal_Out.Set_Vertex_Kind (Formal_Out_Vertex);
         Value_Formal_Out.Set_Formal_Name (Formal_Name);
         Cell_Formal_Out := Insert_Vertex (G        => Graph,
                                           Parent   => Cell_Enclose,
                                           Id       => "formal_out",
                                           Value    => Value_Formal_Out,
                                           X        => 0.0,
                                           Y        => 0.0,
                                           Width    => Formal_Out_Width,
                                           Height   => Formal_Out_Height
                                          );
         Cell_Formal_Out.Set_Parent (Cell_Enclose);
         
         Vertex_Value_Ptr
           (Cell_Current.Get_Value).Append_Vertex_Out
           (Cell_Formal_Out);
         
         if Nkind (Node_To_Dispatch) = N_Identifier then
            Value_Formal_Out.Set_Effective_Param (Node_To_Dispatch);
         end if;
      end if;
   end Create_Formal_Vertex;

   -----------------------------
   -- Determinate_Vertex_Kind --
   -----------------------------
   
   function Determinate_Vertex_Kind 
     (This : access Fbd_Builder_Record; 
      Node : Node_Id) return Vertex_Kind is
   begin
      if Nkind (Parent (Node)) = N_Assignment_Statement then
         if Node = Right_Opnd (Parent (Node)) then
            return Formal_Out_Vertex;
         else
            return Formal_In_Vertex;
         end if;
      elsif Nkind (Parent (Node)) = N_Procedure_Call_Statement then

         if In_Present (Selector_Name (Node))
           and then not Out_Present (Selector_Name (Node)) then

            return Formal_In_Vertex;

         elsif Out_Present (Selector_Name (Node))
           and then not In_Present (Selector_Name (Node)) then

            return Formal_Out_Vertex;

         elsif In_Present (Selector_Name (Node))
           and Out_Present (Selector_Name (Node)) then

            if Find_Node (This, Node) /= Empty then
               return Formal_In_Vertex;
            else
               return Formal_Out_Vertex;
            end if;

         else
            return Formal_Out_Vertex;
         end if;

      else
         return Formal_Out_Vertex;
      end if;
   end Determinate_Vertex_Kind;
      
   ---------------------------------
   -- Is_Local_Variable_Reference --
   ---------------------------------

   function Is_Local_Variable_Reference (Expr : Node_Id) return Boolean is
   begin
      if Nkind (Expr) /= N_Identifier then
         return False;
      else
         declare
            Ent : constant Entity_Id := Entity (Expr);
            Sub : constant Entity_Id := Enclosing_Subprogram (Ent);
         begin
            if Present (Sub) then 
               return True ;
            else
               return False;
            end if;
         end;
      end if;
   end Is_Local_Variable_Reference;
   
   --------------------------
   -- Calculate_Proc_Width --
   --------------------------
   
   function Calculate_Proc_Width  (Node : Node_Id) return Float is
      Proc_Name : Node_Id := Name (Node);
      Cell         : access Cell_Record;
      Cell_Value   : Vertex_Value_Ptr;
      Formal_Value : Vertex_Value_Ptr;
      Formal_Name  : Name_Id;
        
      Formal_Length : Integer := 0;
      Name_Length   : Integer := 0;
      Max_In_Param  : Integer := 0;
      Max_Out_Param : Integer := 0;
      
      use Cells_Lists;
   begin
      Cell := Get_Cell (Node);
      Cell_Value := Cell.Get_Value;
      
      for C of Cell_Value.Get_Vertexs_In loop
         Formal_Value  := C.Get_Value;
         Formal_Name   := Formal_Value.Get_Formal_Name;
         Formal_Length := Length (To_Unbounded_String 
                                  (Get_Name_String (Formal_Name)));
         
         Max_In_Param := Max (Max_In_Param, Formal_Length);
      end loop;
      
      for C of Cell_Value.Get_Vertexs_Out loop
         Formal_Value  := C.Get_Value;
         Formal_Name   := Formal_Value.Get_Formal_Name;
         Formal_Length := Length (To_Unbounded_String 
                                  (Get_Name_String (Formal_Name)));
         
         Max_Out_Param := Max (Max_Out_Param, Formal_Length);
      end loop;
      
      return Float (Max (Name_Length, (Max_In_Param + Max_Out_Param))) + 3.0;
   end Calculate_Proc_Width;
   
   ---------------------------
   -- Calculate_Proc_Height --
   ---------------------------
    
   function Calculate_Proc_Height (Node : Node_Id) return Float is
      Cell       : access Cell_Record;
      Cell_Value : Vertex_Value_Ptr;
      
      Height_In  : Float := 0.0;
      Height_Out : Float := 0.0;
      
      use Cells_Lists;
   begin
      Cell := Get_Cell (Node);
      Cell_Value := Cell.Get_Value;
      
      for C of Cell_Value.Get_Vertexs_In loop
         Height_In := Height_In + 1.0;
      end loop;
      
      for C of Cell_Value.Get_Vertexs_Out loop
         Height_In := Height_In + 1.0;
      end loop;
      
      return Float (Max (Height_Out, Height_In) + 3.0); 
   end Calculate_Proc_Height;
   
   -----------------------------
   -- Calculate_Enclose_Width --
   -----------------------------
    
   function Calculate_Enclose_Width (Node : Node_Id) return Float is
      Cell_In_Enclose : access Cell_Record;
      Cell_Value      : Vertex_Value_Ptr;
      Width           : Float := 0.0;
      
      use Cells_Lists;
   begin
      Cell_In_Enclose := Get_Cell (Node);
      Cell_Value := Cell_In_Enclose.Get_Value;
      
      if Cell_Value.Get_Vertexs_In /= Cells_Lists.Empty_List then
         Width := Width + Formal_In_Width / 2.0;
      end if;
      
      if Cell_Value.Get_Vertexs_Out /= Cells_Lists.Empty_List then
         Width := Width + Formal_Out_Width / 2.0;
      end if;
      
      Width := Width + Cell_In_Enclose.Get_Geometry.Get_Width;
      return Width;
   end Calculate_Enclose_Width;
   
   ------------------------------
   -- Calculate_Enclose_Height --
   ------------------------------
    
   function Calculate_Enclose_Height 
     (Node : Node_Id) return Float is
      
      Cell_In_Enclose : access Cell_Record;      
      Height          : Float := 0.0;
      use Cells_Lists;
   begin
      Cell_In_Enclose := Get_Cell (Node);

      Height := Height + Cell_In_Enclose.Get_Geometry.Get_Height;
      
      return Height;
   end Calculate_Enclose_Height;
         
   -----------------
   -- Dump_Vertex --
   -----------------
   
   procedure Dump_Vertex 
     (This   : access Fbd_Builder_Record;
      Vertex : access Cell_Record'Class) is
      
      Formals_In_List  : Cells_Lists.List;
      Formals_Out_List : Cells_Lists.List;
      Value : Vertex_Value_Ptr := Vertex.Get_Value;
      Vertex_Enclose : access Cell_Record := Vertex.Get_Parent;
      
      Nb : Integer := 1;
   begin
      if Vertex.Is_Vertex then
         Put_Line ("=======================================================");
         Put_Line (Vertex.Get_Id);
         Put_Line ("Cell " & Value.Get_Vertex_Kind'Img & " X : " & 
                     Vertex.Get_Geometry.Get_X'Img);
         Put_Line ("Cell " & Value.Get_Vertex_Kind'Img & " Y : " & 
                     Vertex.Get_Geometry.Get_Y'Img);
         Put_Line ("   ==> Cell " & Value.Get_Vertex_Kind'Img & " W : " & 
                     Vertex.Get_Geometry.Get_Width'Img);
         Put_Line ("   ==> Cell " & Value.Get_Vertex_Kind'Img & " H : " & 
                     Vertex.Get_Geometry.Get_Height'Img);
         Put_Line ("");

         Formals_In_List := Vertex_Value_Ptr (Vertex.Get_Value).Get_Vertexs_In;
         for Formal of Formals_In_List loop
            Put_Line ("formal in " & Nb'Img & " X : " & 
                        Formal.Get_Geometry.Get_X'Img);
            Put_Line ("formal in " & Nb'Img & " Y : " & 
                        Formal.Get_Geometry.Get_Y'Img);
            Put_Line ("");
            Nb := Nb + 1;
         end loop;
      
         Nb := 1;
         Formals_Out_List := 
           Vertex_Value_Ptr (Vertex.Get_Value).Get_Vertexs_Out;
         
         for Formal of Formals_Out_List loop
            Put_Line ("formal out" & Nb'Img & " X : " & 
                        Formal.Get_Geometry.Get_X'Img);
            Put_Line ("formal out" & Nb'Img & " Y : " &
                        Formal.Get_Geometry.Get_Y'Img);
            Put_Line ("");
            Nb := Nb + 1;
         end loop;
        
         Put_Line ("=======================================================");
         Put_Line ("");
      else
         Put_Line ("");
      end if;
   end Dump_Vertex;
      
   ----------------------
   -- Is_Global_Vertex --
   ----------------------
   
   function Is_Global_Vertex
     (Cell : access Cell_Record'Class) return Boolean is

      Vertex_Value : Vertex_Value_Ptr := Cell.Get_Value;
   begin
      
      if Vertex_Value.Get_Vertex_Kind = Formal_In_Vertex 
        or Vertex_Value.Get_Vertex_Kind = Formal_Out_Vertex then
         return Is_Local_Variable_Reference (Vertex_Value.Get_Node);
      end if;
      
      return False;
   end Is_Global_Vertex;
   
   ---------------------
   -- Is_Local_Vertex --
   ---------------------
   
   function Is_Local_Vertex
     (Cell : access Cell_Record'Class) return Boolean is
      
      Vertex_Value : Vertex_Value_Ptr := Cell.Get_Value;
   begin
      
      if Vertex_Value.Get_Vertex_Kind = Formal_In_Vertex 
        or Vertex_Value.Get_Vertex_Kind = Formal_Out_Vertex then
         return Is_Local_Variable_Reference (Vertex_Value.Get_Node);
      end if;
      return False;
   end Is_Local_Vertex;
   
   ----------------------------
   -- Is_Local_Global_Vertex --
   ----------------------------
   
   function Is_Local_Global_Vertex
     (Cell : access Cell_Record'Class) return Boolean is
   begin
      return Is_Global_Vertex (Cell) or Is_Local_Vertex (Cell);
   end Is_Local_Global_Vertex;
   
   ------------------
   -- Is_In_Vertex --
   ------------------
   
   function Is_In_Vertex
     (Cell : access Cell_Record'Class) return Boolean is
      
      Vertex_Value : Vertex_Value_Ptr := Cell.Get_Value;
   begin
      return Vertex_Value.Get_Vertex_Kind = Formal_In_Vertex;
   end Is_In_Vertex;
   
   -------------------
   -- Is_Out_Vertex --
   -------------------
   
   function Is_Out_Vertex
     (Cell : access Cell_Record'Class) return Boolean is  
      
      Vertex_Value : Vertex_Value_Ptr := Cell.Get_Value;
   begin
      return Vertex_Value.Get_Vertex_Kind = Formal_Out_Vertex;
   end Is_Out_Vertex;
  
   ------------------
   -- Is_Id_Vertex --
   ------------------
   
   function Is_Id_Vertex 
     (Cell : access Cell_Record'Class) return Boolean is
      Vertex_Value : Vertex_Value_Ptr := Cell.Get_Value;
   begin
      return Vertex_Value.Get_Vertex_Kind = Identifier_Vertex;
   end Is_Id_Vertex;
   
   ---------------------------------
   -- Compute_Absolute_Coordinate --
   ---------------------------------
   
   function Compute_Absolute_Coordinate 
     (Root : access Cell_Record'Class;
      Cell : access Cell_Record'Class) return Point_Record is
      
      Geo    : access Cell_Geometry_Record;
      Parent : access Cell_Record'Class;
      X      : Float;
      Y      : Float;
   begin
      Geo := Cell.Get_Geometry;
      X := Geo.Get_X;
      Y := Geo.Get_Y;
      
      Parent := Cell.Get_Parent;
      while Parent /= null and then Parent /= Root loop
         Geo := Parent.Get_Geometry;
         X := X + Geo.Get_X;
         Y := Y + Geo.Get_Y;
         Parent := Parent.Get_Parent;
      end loop;
      
      return Point_Record'(X, Y);
   end Compute_Absolute_Coordinate;
   
   -----------------------
   -- Edge_Intersection --
   -----------------------
   
   function Edge_Intersection
     (Root : access Cell_Record'Class;
      P1   : Point_Record;
      P2   : Point_Record) return Float is
      
      Childs : Cells_Lists.List;
      Geo    : access Cell_Geometry_Record;
      Rect   : Rectangle_Record;
      Y      : Float;
      H      : Float;
   begin
      Childs := Root.Get_Children_List;
      for Cell of Childs loop
         if Cell.Is_Vertex then
            if not Is_Local_Global_Vertex (Cell)
              and not Is_In_Vertex (Cell)
              and not Is_Out_Vertex (Cell)
            then
	       
               Geo := Cell.Get_Geometry;
               Rect := Geo.Geometry_Rectangle;
               if Intersect_Line (Rect, P1, P2) then
                  Y := Geo.Get_Y;
                  if Get_Y (P1) > Get_Y (P2) then
                     H := Geo.Get_Height;
                     return Y + H;
                  else
                     return Y;
                  end if;
               end if;
            end if;
         end if;
      end loop;
      
      return 0.0;
   end Edge_Intersection;
   
end Reflex.Fbd_Util;
