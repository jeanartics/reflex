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

with Sinfo; use Sinfo;
with Namet; use Namet;

with Reflex.Fbd_Placements; use Reflex.Fbd_Placements;
with Reflex.Vertex_Value;   use Reflex.Vertex_Value;
with Reflex.Fbd_Dispatch;   use Reflex.Fbd_Dispatch;
with Reflex.Fbd_Util;       use Reflex.Fbd_Util;
with Reflex.Infos;          use Reflex.Infos;

package body Reflex.Fbd_Gen_Ch4 is
   
   -------------------------
   -- Fbd_Generate_Op_And --
   -------------------------
   
   procedure Fbd_Generate_Op_And
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is 
      
      Vertex_Enclose : access Cell_Record;
      Vertex_Op_And  : access Cell_Record;

      Lhs_Node : Node_Id := Left_Opnd (Node);
      Rhs_Node : Node_Id := Right_Opnd (Node);
      
      Value_Op_And    : Vertex_Value_Ptr := New_Vertex_Value;
      Value_Enclosing : Vertex_Value_Ptr := New_Vertex_Value;
      
      Graph : access Graph_Record := This.Get_Graph;
      use Cells_Lists;
   begin
      -- Create enclosing vertex
      
      Value_Enclosing.Set_Node (Node);
      Value_Enclosing.Set_Vertex_Kind (Enclose_Vertex);
      Vertex_Enclose := Insert_Vertex (G        => Graph,
                                       Parent   => This.Get_Cell_Parent,
                                       Id       => "enclose_and",
                                       Value    => Value_Enclosing,
                                       X        => 0.0,
                                       Y        => 0.0,
                                       Width    => 0.0,
                                       Height   => 0.0 );
      
      Value_Op_And.Set_Node (Node);
      Value_Op_And.Set_Vertex_Kind (Operator_Vertex);
      Vertex_Op_And := Insert_Vertex (G        => Graph,
                                      Parent   => Vertex_Enclose,
                                      Id       => "op_and",
                                      Value    => Value_Op_And,
                                      X        => 0.0,
                                      Y        => 0.0,
                                      Width    => Operator_Width,
                                      Height   => Operator_Height);
      Set_Cell (Node, Vertex_Op_And);
      Value_Enclosing.Set_Cell_To_Link (Vertex_Op_And);

      --  Create all formals cells
      
      Create_Formal_Vertex (This,
                            Node,
                            Rhs_Node,
                            True,
                            False,
                            String_Find ("IN2"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Lhs_Node,
                            True,
                            False,
                            String_Find ("IN1"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Empty,
                            False,
                            True,
                            String_Find ("OUT"));

      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Width (Node));
      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Height (Node));
      
      Place_Vertices (This, Vertex_Op_And);
       
      Dump_Vertex (This, Vertex_Op_And);
   end Fbd_Generate_Op_And;
      
   -------------------------
   -- Fbd_Generate_Op_Not --
   -------------------------
   
   procedure Fbd_Generate_Op_Not
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is 
   begin
      Fbd_Node_Dispatch (This,Right_Opnd (Node));
   end Fbd_Generate_Op_Not;
   
   ------------------------
   -- Fbd_Generate_Op_Or --
   ------------------------
   
   procedure Fbd_Generate_Op_Or
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is 
      
      Vertex_Enclose : access Cell_Record;
      Vertex_Op_Or   : access Cell_Record;

      Lhs_Node : Node_Id := Left_Opnd (Node);
      Rhs_Node : Node_Id := Right_Opnd (Node);
      
      Value_Op_Or     : Vertex_Value_Ptr := New_Vertex_Value;
      Value_Enclosing : Vertex_Value_Ptr := New_Vertex_Value;
      
      Graph : access Graph_Record := This.Get_Graph;
      use Cells_Lists;
   begin
      -- Create enclosing vertex
      
      Value_Enclosing.Set_Node (Node);
      Value_Enclosing.Set_Vertex_Kind (Enclose_Vertex);
      Vertex_Enclose := Insert_Vertex (G        => Graph,
                                       Parent   => This.Get_Cell_Parent,
                                       Id       => "enclose_or",
                                       Value    => Value_Enclosing,
                                       X        => 0.0,
                                       Y        => 0.0,
                                       Width    => 0.0,
                                       Height   => 0.0 );
      
      Value_Op_Or.Set_Node (Node);
      Value_Op_Or.Set_Vertex_Kind (Operator_Vertex);
      Vertex_Op_Or := Insert_Vertex (G        => Graph,
                                     Parent   => Vertex_Enclose,
                                     Id       => "op_or",
                                     Value    => Value_Op_Or,
                                     X        => 0.0,
                                     Y        => 0.0,
                                     Width    => Operator_Width,
                                     Height   => Operator_Height);
      Set_Cell (Node, Vertex_Op_Or);
      Value_Enclosing.Set_Cell_To_Link (Vertex_Op_Or);

      --  Create all formals cells
      
      Create_Formal_Vertex (This,
                            Node,
                            Rhs_Node,
                            True,
                            False,
                            String_Find ("IN2"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Lhs_Node,
                            True,
                            False,
                            String_Find ("IN1"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Empty,
                            False,
                            True,
                            String_Find ("OUT"));

      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Width (Node));
      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Height (Node));
      
      Place_Vertices (This, Vertex_Op_Or);
       
      Dump_Vertex (This, Vertex_Op_Or);
      
   end Fbd_Generate_Op_Or;
 
   ------------------------
   -- Fbd_Generate_Op_Eq --
   ------------------------
   
   procedure Fbd_Generate_Op_Eq 
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is 
      
      Vertex_Enclose : access Cell_Record;
      Vertex_Op_Eq   : access Cell_Record;

      Lhs_Node : Node_Id := Left_Opnd (Node);
      Rhs_Node : Node_Id := Right_Opnd (Node);
      
      Value_Op_Eq     : Vertex_Value_Ptr := New_Vertex_Value;
      Value_Enclosing : Vertex_Value_Ptr := New_Vertex_Value;
      
      Graph : access Graph_Record := This.Get_Graph;
      use Cells_Lists;
   begin
      -- Create enclosing vertex
      
      Value_Enclosing.Set_Node (Node);
      Value_Enclosing.Set_Vertex_Kind (Enclose_Vertex);
      Vertex_Enclose := Insert_Vertex (G        => Graph,
                                       Parent   => This.Get_Cell_Parent,
                                       Id       => "enclose_eq",
                                       Value    => Value_Enclosing,
                                       X        => 0.0,
                                       Y        => 0.0,
                                       Width    => 0.0,
                                       Height   => 0.0 );
      
      Value_Op_Eq.Set_Node (Node);
      Value_Op_Eq.Set_Vertex_Kind (Operator_Vertex);
      Vertex_Op_Eq := Insert_Vertex (G        => Graph,
                                     Parent   => Vertex_Enclose,
                                     Id       => "op_eq",
                                     Value    => Value_Op_Eq,
                                     X        => 0.0,
                                     Y        => 0.0,
                                     Width    => Operator_Width,
                                     Height   => Operator_Height);
      Set_Cell (Node, Vertex_Op_Eq);
      Value_Enclosing.Set_Cell_To_Link (Vertex_Op_Eq);

      --  Create all formals cells
      
      Create_Formal_Vertex (This,
                            Node,
                            Rhs_Node,
                            True,
                            False,
                            String_Find ("IN2"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Lhs_Node,
                            True,
                            False,
                            String_Find ("IN1"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Empty,
                            False,
                            True,
                            String_Find ("OUT"));

      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Width (Node));
      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Height (Node));
      
      Place_Vertices (This, Vertex_Op_Eq);
       
      Dump_Vertex (This, Vertex_Op_Eq);
      
   end Fbd_Generate_Op_Eq;

   ------------------------
   -- Fbd_Generate_Op_Ne --
   ------------------------
   
   procedure Fbd_Generate_Op_Ne
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is 
      
      Vertex_Enclose : access Cell_Record;
      Vertex_Op_Ne   : access Cell_Record;

      Lhs_Node : Node_Id := Left_Opnd (Node);
      Rhs_Node : Node_Id := Right_Opnd (Node);
      
      Value_Op_Ne     : Vertex_Value_Ptr := New_Vertex_Value;
      Value_Enclosing : Vertex_Value_Ptr := New_Vertex_Value;
      
      Graph : access Graph_Record := This.Get_Graph;
      use Cells_Lists;
   begin
      
      -- Create enclosing vertex
      
      Value_Enclosing.Set_Node (Node);
      Value_Enclosing.Set_Vertex_Kind (Enclose_Vertex);
      Vertex_Enclose := Insert_Vertex (G        => Graph,
                                       Parent   => This.Get_Cell_Parent,
                                       Id       => "enclose_ne",
                                       Value    => Value_Enclosing,
                                       X        => 0.0,
                                       Y        => 0.0,
                                       Width    => 0.0,
                                       Height   => 0.0 );
      
      Value_Op_Ne.Set_Node (Node);
      Value_Op_Ne.Set_Vertex_Kind (Operator_Vertex);
      Vertex_Op_Ne := Insert_Vertex (G        => Graph,
                                     Parent   => Vertex_Enclose,
                                     Id       => "op_ne",
                                     Value    => Value_Op_Ne,
                                     X        => 0.0,
                                     Y        => 0.0,
                                     Width    => Operator_Width,
                                     Height   => Operator_Height);
      Set_Cell (Node, Vertex_Op_Ne);
      Value_Enclosing.Set_Cell_To_Link (Vertex_Op_Ne);

      --  Create all formals cells
      
      Create_Formal_Vertex (This,
                            Node,
                            Rhs_Node,
                            True,
                            False,
                            String_Find ("IN2"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Lhs_Node,
                            True,
                            False,
                            String_Find ("IN1"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Empty,
                            False,
                            True,
                            String_Find ("OUT"));

      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Width (Node));
      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Height (Node));
      
      Place_Vertices (This, Vertex_Op_Ne);
       
      Dump_Vertex (This, Vertex_Op_Ne);
   end Fbd_Generate_Op_Ne;
   
   ------------------------
   -- Fbd_Generate_Op_Ge --
   ------------------------
   
   procedure Fbd_Generate_Op_Ge 
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is 
      
      Vertex_Enclose : access Cell_Record;
      Vertex_Op_Ge   : access Cell_Record;

      Lhs_Node : Node_Id := Left_Opnd (Node);
      Rhs_Node : Node_Id := Right_Opnd (Node);
      
      Value_Op_Ge     : Vertex_Value_Ptr := New_Vertex_Value;
      Value_Enclosing : Vertex_Value_Ptr := New_Vertex_Value;
      
      Graph : access Graph_Record := This.Get_Graph;
      use Cells_Lists;
   begin
      
      -- Create enclosing vertex
      
      Value_Enclosing.Set_Node (Node);
      Value_Enclosing.Set_Vertex_Kind (Enclose_Vertex);
      Vertex_Enclose := Insert_Vertex (G        => Graph,
                                       Parent   => This.Get_Cell_Parent,
                                       Id       => "enclose_ge",
                                       Value    => Value_Enclosing,
                                       X        => 0.0,
                                       Y        => 0.0,
                                       Width    => 0.0,
                                       Height   => 0.0 );
      
      Value_Op_Ge.Set_Node (Node);
      Value_Op_Ge.Set_Vertex_Kind (Operator_Vertex);
      Vertex_Op_Ge := Insert_Vertex (G        => Graph,
                                     Parent   => Vertex_Enclose,
                                     Id       => "op_ge",
                                     Value    => Value_Op_Ge,
                                     X        => 0.0,
                                     Y        => 0.0,
                                     Width    => Operator_Width,
                                     Height   => Operator_Height);
      Set_Cell (Node, Vertex_Op_Ge);
      Value_Enclosing.Set_Cell_To_Link (Vertex_Op_Ge);

      --  Create all formals cells
      
      Create_Formal_Vertex (This,
                            Node,
                            Rhs_Node,
                            True,
                            False,
                            String_Find ("IN2"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Lhs_Node,
                            True,
                            False,
                            String_Find ("IN1"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Empty,
                            False,
                            True,
                            String_Find ("OUT"));

      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Width (Node));
      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Height (Node));
      
      Place_Vertices (This, Vertex_Op_Ge);
       
      Dump_Vertex (This, Vertex_Op_Ge);
   end Fbd_Generate_Op_Ge;
            
   ------------------------
   -- Fbd_Generate_Op_Gt --
   ------------------------
  
   procedure Fbd_Generate_Op_Gt 
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is 
      
      Vertex_Enclose : access Cell_Record;
      Vertex_Op_Gt   : access Cell_Record;

      Lhs_Node : Node_Id := Left_Opnd (Node);
      Rhs_Node : Node_Id := Right_Opnd (Node);
      
      Value_Op_Gt     : Vertex_Value_Ptr := New_Vertex_Value;
      Value_Enclosing : Vertex_Value_Ptr := New_Vertex_Value;
      
      Graph : access Graph_Record := This.Get_Graph;
      use Cells_Lists;
   begin
      
      -- Create enclosing vertex
      
      Value_Enclosing.Set_Node (Node);
      Value_Enclosing.Set_Vertex_Kind (Enclose_Vertex);
      Vertex_Enclose := Insert_Vertex (G        => Graph,
                                       Parent   => This.Get_Cell_Parent,
                                       Id       => "enclose_gt",
                                       Value    => Value_Enclosing,
                                       X        => 0.0,
                                       Y        => 0.0,
                                       Width    => 0.0,
                                       Height   => 0.0 );
      
      Value_Op_Gt.Set_Node (Node);
      Value_Op_Gt.Set_Vertex_Kind (Operator_Vertex);
      Vertex_Op_Gt := Insert_Vertex (G        => Graph,
                                     Parent   => Vertex_Enclose,
                                     Id       => "op_gt",
                                     Value    => Value_Op_Gt,
                                     X        => 0.0,
                                     Y        => 0.0,
                                     Width    => Operator_Width,
                                     Height   => Operator_Height);
      Set_Cell (Node, Vertex_Op_Gt);
      Value_Enclosing.Set_Cell_To_Link (Vertex_Op_Gt);

      --  Create all formals cells
     
      Create_Formal_Vertex (This,
                            Node,
                            Rhs_Node,
                            True,
                            False,
                            String_Find ("IN2"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Lhs_Node,
                            True,
                            False,
                            String_Find ("IN1"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Empty,
                            False,
                            True,
                            String_Find ("OUT"));

      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Width (Node));
      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Height (Node));
      
      Place_Vertices (This, Vertex_Op_Gt);
       
      Dump_Vertex (This, Vertex_Op_Gt);
   end Fbd_Generate_Op_Gt;
            
   ------------------------
   -- Fbd_Generate_Op_Le --
   ------------------------
   
   procedure Fbd_Generate_Op_Le 
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is 
      
      Vertex_Enclose : access Cell_Record;
      Vertex_Op_Le   : access Cell_Record;

      Lhs_Node : Node_Id := Left_Opnd (Node);
      Rhs_Node : Node_Id := Right_Opnd (Node);
      
      Value_Op_Le     : Vertex_Value_Ptr := New_Vertex_Value;
      Value_Enclosing : Vertex_Value_Ptr := New_Vertex_Value;
      
      Graph : access Graph_Record := This.Get_Graph;
      use Cells_Lists;
   begin
      
      -- Create enclosing vertex
      
      Value_Enclosing.Set_Node (Node);
      Value_Enclosing.Set_Vertex_Kind (Enclose_Vertex);
      Vertex_Enclose := Insert_Vertex (G        => Graph,
                                       Parent   => This.Get_Cell_Parent,
                                       Id       => "enclose_le",
                                       Value    => Value_Enclosing,
                                       X        => 0.0,
                                       Y        => 0.0,
                                       Width    => 0.0,
                                       Height   => 0.0 );
      
      Value_Op_Le.Set_Node (Node);
      Value_Op_Le.Set_Vertex_Kind (Operator_Vertex);
      Vertex_Op_Le := Insert_Vertex (G        => Graph,
                                     Parent   => Vertex_Enclose,
                                     Id       => "op_le",
                                     Value    => Value_Op_Le,
                                     X        => 0.0,
                                     Y        => 0.0,
                                     Width    => Operator_Width,
                                     Height   => Operator_Height);
      Set_Cell (Node, Vertex_Op_Le);
      Value_Enclosing.Set_Cell_To_Link (Vertex_Op_Le);

      --  Create all formals cells
      
      Create_Formal_Vertex (This,
                            Node,
                            Rhs_Node,
                            True,
                            False,
                            String_Find ("IN2"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Lhs_Node,
                            True,
                            False,
                            String_Find ("IN1"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Empty,
                            False,
                            True,
                            String_Find ("OUT"));

      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Width (Node));
      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Height (Node));
      
      Place_Vertices (This, Vertex_Op_Le);
       
      Dump_Vertex (This, Vertex_Op_Le);
   end Fbd_Generate_Op_Le;
   
   ------------------------
   -- Fbd_Generate_Op_Lt --
   ------------------------
   
   procedure Fbd_Generate_Op_Lt 
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is 
      
      Vertex_Enclose : access Cell_Record;
      Vertex_Op_Lt   : access Cell_Record;

      Lhs_Node : Node_Id := Left_Opnd (Node);
      Rhs_Node : Node_Id := Right_Opnd (Node);
      
      Value_Op_Lt     : Vertex_Value_Ptr := New_Vertex_Value;
      Value_Enclosing : Vertex_Value_Ptr := New_Vertex_Value;
      
      Graph : access Graph_Record := This.Get_Graph;
      use Cells_Lists;
   begin
      
      -- Create enclosing vertex
      
      Value_Enclosing.Set_Node (Node);
      Value_Enclosing.Set_Vertex_Kind (Enclose_Vertex);
      Vertex_Enclose := Insert_Vertex (G        => Graph,
                                       Parent   => This.Get_Cell_Parent,
                                       Id       => "enclose_lt",
                                       Value    => Value_Enclosing,
                                       X        => 0.0,
                                       Y        => 0.0,
                                       Width    => 0.0,
                                       Height   => 0.0 );
      
      Value_Op_Lt.Set_Node (Node);
      Value_Op_Lt.Set_Vertex_Kind (Operator_Vertex);
      Vertex_Op_Lt := Insert_Vertex (G        => Graph,
                                     Parent   => Vertex_Enclose,
                                     Id       => "op_lt",
                                     Value    => Value_Op_Lt,
                                     X        => 0.0,
                                     Y        => 0.0,
                                     Width    => Operator_Width,
                                     Height   => Operator_Height);
      Set_Cell (Node, Vertex_Op_Lt);
      Value_Enclosing.Set_Cell_To_Link (Vertex_Op_Lt);

      --  Create all formals cells
      
      Create_Formal_Vertex (This,
                            Node,
                            Rhs_Node,
                            True,
                            False,
                            String_Find ("IN2"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Lhs_Node,
                            True,
                            False,
                            String_Find ("IN1"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Empty,
                            False,
                            True,
                            String_Find ("OUT"));

      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Width (Node));
      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Height (Node));
      
      Place_Vertices (This, Vertex_Op_Lt);
       
      Dump_Vertex (This, Vertex_Op_Lt);
   end Fbd_Generate_Op_Lt;
   
   ---------------------------------
   -- Fbd_Generate_Op_Shift_Right --
   ---------------------------------
   
   procedure Fbd_Generate_Op_Shift_Right
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is 
   begin
      null;
   end Fbd_Generate_Op_Shift_Right;
   
   --------------------------------
   -- Fbd_Generate_Op_Shift_Left --
   --------------------------------
   
   procedure Fbd_Generate_Op_Shift_Left
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is 
   begin
      null;
   end Fbd_Generate_Op_Shift_Left;
   
   -------------------------
   -- Fbd_Generate_Op_Add --
   -------------------------
   
   procedure Fbd_Generate_Op_Add
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is 
      
      Vertex_Enclose : access Cell_Record;
      Vertex_Op_Add  : access Cell_Record;

      Lhs_Node : Node_Id := Left_Opnd (Node);
      Rhs_Node : Node_Id := Right_Opnd (Node);
      
      Value_Op_Add    : Vertex_Value_Ptr := New_Vertex_Value;
      Value_Enclosing : Vertex_Value_Ptr := New_Vertex_Value;
      
      Graph : access Graph_Record := This.Get_Graph;
      use Cells_Lists;
   begin
      
      -- Create enclosing vertex
      
      Value_Enclosing.Set_Node (Node);
      Value_Enclosing.Set_Vertex_Kind (Enclose_Vertex);
      Vertex_Enclose := Insert_Vertex (G        => Graph,
                                       Parent   => This.Get_Cell_Parent,
                                       Id       => "enclose_add",
                                       Value    => Value_Enclosing,
                                       X        => 0.0,
                                       Y        => 0.0,
                                       Width    => 0.0,
                                       Height   => 0.0 );
      Value_Op_Add.Set_Node (Node);
      Value_Op_Add.Set_Vertex_Kind (Operator_Vertex);
      Vertex_Op_Add := Insert_Vertex (G        => Graph,
                                      Parent   => Vertex_Enclose,
                                      Id       => "op_add",
                                      Value    => Value_Op_Add,
                                      X        => 0.0,
                                      Y        => 0.0,
                                      Width    => Operator_Width,
                                      Height   => Operator_Height);
      Set_Cell (Node, Vertex_Op_Add);
      Value_Enclosing.Set_Cell_To_Link (Vertex_Op_Add);

      --  Create all formals cells

      Create_Formal_Vertex (This,
                            Node,
                            Rhs_Node,
                            True,
                            False,
                            String_Find ("IN2"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Lhs_Node,
                            True,
                            False,
                            String_Find ("IN1"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Empty,
                            False,
                            True,
                            String_Find ("OUT"));

      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Width (Node));
      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Height (Node));
      
      Place_Vertices (This, Vertex_Op_Add);
       
   end Fbd_Generate_Op_Add;
   
   -------------------------------
   -- Fbd_Generate_Op_Substract --
   -------------------------------
   
   procedure Fbd_Generate_Op_Substract
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is 
      
      Vertex_Enclose      : access Cell_Record;
      Vertex_Op_Substract : access Cell_Record;

      Lhs_Node : Node_Id := Left_Opnd (Node);
      Rhs_Node : Node_Id := Right_Opnd (Node);
      
      Value_Op_Substract : Vertex_Value_Ptr := New_Vertex_Value;
      Value_Enclosing : Vertex_Value_Ptr := New_Vertex_Value;
      
      Graph : access Graph_Record := This.Get_Graph;
      use Cells_Lists;
   begin

      -- Create enclosing vertex
      
      Value_Enclosing.Set_Node (Node);
      Value_Enclosing.Set_Vertex_Kind (Enclose_Vertex);
      Vertex_Enclose := Insert_Vertex (G        => Graph,
                                       Parent   => This.Get_Cell_Parent,
                                       Id       => "enclose_sub",
                                       Value    => Value_Enclosing,
                                       X        => 0.0,
                                       Y        => 0.0,
                                       Width    => 0.0,
                                       Height   => 0.0 );
      
      Value_Op_Substract.Set_Node (Node);
      Value_Op_Substract.Set_Vertex_Kind (Operator_Vertex);
      Vertex_Op_Substract := Insert_Vertex (G        => Graph,
                                            Parent   => Vertex_Enclose,
                                            Id       => "op_sub",
                                            Value    => Value_Op_Substract,
                                            X        => 0.0,
                                            Y        => 0.0,
                                            Width    => Operator_Width,
                                            Height   => Operator_Height);
      Set_Cell (Node, Vertex_Op_Substract);
      Value_Enclosing.Set_Cell_To_Link (Vertex_Op_Substract);

      --  Create all formals cells
      
      Create_Formal_Vertex (This,
                            Node,
                            Rhs_Node,
                            True,
                            False,
                            String_Find ("IN2"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Lhs_Node,
                            True,
                            False,
                            String_Find ("IN1"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Empty,
                            False,
                            True,
                            String_Find ("OUT"));

      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Width (Node));
      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Height (Node));
      
      Place_Vertices (This, Vertex_Op_Substract);
       
      Dump_Vertex (This, Vertex_Op_Substract);
   end Fbd_Generate_Op_Substract;
   
   ----------------------------
   -- Fbd_Generate_Op_Divide --
   ----------------------------
   
   procedure Fbd_Generate_Op_Divide
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is 
      
      Vertex_Enclose   : access Cell_Record;
      Vertex_Op_Divide : access Cell_Record;

      Lhs_Node : Node_Id := Left_Opnd (Node);
      Rhs_Node : Node_Id := Right_Opnd (Node);
      
      Value_Op_Divide : Vertex_Value_Ptr := New_Vertex_Value;
      Value_Enclosing : Vertex_Value_Ptr := New_Vertex_Value;
      
      Graph : access Graph_Record := This.Get_Graph;
      use Cells_Lists;
   begin
      
      -- Create enclosing vertex
      
      Value_Enclosing.Set_Node (Node);
      Value_Enclosing.Set_Vertex_Kind (Enclose_Vertex);
      Vertex_Enclose := Insert_Vertex (G        => Graph,
                                       Parent   => This.Get_Cell_Parent,
                                       Id       => "enclose_div",
                                       Value    => Value_Enclosing,
                                       X        => 0.0,
                                       Y        => 0.0,
                                       Width    => 0.0,
                                       Height   => 0.0 );
      
      Value_Op_Divide.Set_Node (Node);
      Value_Op_Divide.Set_Vertex_Kind (Operator_Vertex);
      Vertex_Op_Divide := Insert_Vertex (G        => Graph,
                                         Parent   => Vertex_Enclose,
                                         Id       => "op_div",
                                         Value    => Value_Op_Divide,
                                         X        => 0.0,
                                         Y        => 0.0,
                                         Width    => Operator_Width,
                                         Height   => Operator_Height);
      Set_Cell (Node, Vertex_Op_Divide);
      Value_Enclosing.Set_Cell_To_Link (Vertex_Op_Divide);

      --  Create all formals cells
      
      Create_Formal_Vertex (This,
                            Node,
                            Rhs_Node,
                            True,
                            False,
                            String_Find ("IN2"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Lhs_Node,
                            True,
                            False,
                            String_Find ("IN1"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Empty,
                            False,
                            True,
                            String_Find ("OUT"));

      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Width (Node));
      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Height (Node));
      
      Place_Vertices (This, Vertex_Op_Divide);
       
      Dump_Vertex (This, Vertex_Op_Divide);
   end Fbd_Generate_Op_Divide;
   
   ------------------------------
   -- Fbd_Generate_Op_Multiply --
   ------------------------------
   
   procedure Fbd_Generate_Op_Multiply
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is 
      
      Vertex_Enclose     : access Cell_Record;
      Vertex_Op_Multiply : access Cell_Record;

      Lhs_Node : Node_Id := Left_Opnd (Node);
      Rhs_Node : Node_Id := Right_Opnd (Node);
      
      Value_Op_Multiply : Vertex_Value_Ptr := New_Vertex_Value;
      Value_Enclosing : Vertex_Value_Ptr := New_Vertex_Value;
      
      Graph : access Graph_Record := This.Get_Graph;
      use Cells_Lists;
   begin
      
      -- Create enclosing vertex
      
      Value_Enclosing.Set_Node (Node);
      Value_Enclosing.Set_Vertex_Kind (Enclose_Vertex);
      Vertex_Enclose := Insert_Vertex (G        => Graph,
                                       Parent   => This.Get_Cell_Parent,
                                       Id       => "enclose_mul",
                                       Value    => Value_Enclosing,
                                       X        => 0.0,
                                       Y        => 0.0,
                                       Width    => 0.0,
                                       Height   => 0.0 );
      
      Value_Op_Multiply.Set_Node (Node);
      Value_Op_Multiply.Set_Vertex_Kind (Operator_Vertex);
      Vertex_Op_Multiply := Insert_Vertex (G        => Graph,
                                           Parent   => Vertex_Enclose,
                                           Id       => "op_mul",
                                           Value    => Value_Op_Multiply,
                                           X        => 0.0,
                                           Y        => 0.0,
                                           Width    => Operator_Width,
                                           Height   => Operator_Height);
      Set_Cell (Node, Vertex_Op_Multiply);
      Value_Enclosing.Set_Cell_To_Link (Vertex_Op_Multiply);

      --  Create all formals cells
      
      Create_Formal_Vertex (This,
                            Node,
                            Rhs_Node,
                            True,
                            False,
                            String_Find ("IN2"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Lhs_Node,
                            True,
                            False,
                            String_Find ("IN1"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Empty,
                            False,
                            True,
                            String_Find ("OUT"));

      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Width (Node));
      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Height (Node));
      
      Place_Vertices (This, Vertex_Op_Multiply);
       
      Dump_Vertex (This, Vertex_Op_Multiply);
   end Fbd_Generate_Op_Multiply;
   
   -------------------------
   -- Fbd_Generate_Op_Xor --
   -------------------------
   
   procedure Fbd_Generate_Op_Xor
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is 
      
      Vertex_Enclose : access Cell_Record;
      Vertex_Op_Xor  : access Cell_Record;

      Lhs_Node : Node_Id := Left_Opnd (Node);
      Rhs_Node : Node_Id := Right_Opnd (Node);
      
      Value_Op_Xor    : Vertex_Value_Ptr := New_Vertex_Value;
      Value_Enclosing : Vertex_Value_Ptr := New_Vertex_Value;
      
      Graph : access Graph_Record := This.Get_Graph;
      use Cells_Lists;
   begin
      
      -- Create enclosing vertex
      
      Value_Enclosing.Set_Node (Node);
      Value_Enclosing.Set_Vertex_Kind (Enclose_Vertex);
      Vertex_Enclose := Insert_Vertex (G        => Graph,
                                       Parent   => This.Get_Cell_Parent,
                                       Id       => "enclose_xor",
                                       Value    => Value_Enclosing,
                                       X        => 0.0,
                                       Y        => 0.0,
                                       Width    => 0.0,
                                       Height   => 0.0 );
      
      Value_Op_Xor.Set_Node (Node);
      Value_Op_Xor.Set_Vertex_Kind (Operator_Vertex);
      Vertex_Op_Xor := Insert_Vertex (G        => Graph,
                                      Parent   => Vertex_Enclose,
                                      Id       => "op_xor",
                                      Value    => Value_Op_Xor,
                                      X        => 0.0,
                                      Y        => 0.0,
                                      Width    => Operator_Width,
                                      Height   => Operator_Height);
      Set_Cell (Node, Vertex_Op_Xor);
      Value_Enclosing.Set_Cell_To_Link (Vertex_Op_Xor);

      --  Create all formals cells
      
      Create_Formal_Vertex (This,
                            Node,
                            Rhs_Node,
                            True,
                            False,
                            String_Find ("IN2"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Lhs_Node,
                            True,
                            False,
                            String_Find ("IN1"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Empty,
                            False,
                            True,
                            String_Find ("OUT"));

      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Width (Node));
      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Height (Node));
      
      Place_Vertices (This, Vertex_Op_Xor);
       
      Dump_Vertex (This, Vertex_Op_Xor);
   end Fbd_Generate_Op_Xor;
   
   -------------------------
   -- Fbd_Generate_Op_Mod --
   -------------------------
   
   procedure Fbd_Generate_Op_Mod 
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is 
      
      Vertex_Enclose : access Cell_Record;
      Vertex_Op_Mod  : access Cell_Record;

      Lhs_Node : Node_Id := Left_Opnd (Node);
      Rhs_Node : Node_Id := Right_Opnd (Node);
      
      Value_Op_Mod    : Vertex_Value_Ptr := New_Vertex_Value;
      Value_Enclosing : Vertex_Value_Ptr := New_Vertex_Value;
      
      Graph : access Graph_Record := This.Get_Graph;
      use Cells_Lists;
   begin
      
      -- Create enclosing vertex
      
      Value_Enclosing.Set_Node (Node);
      Value_Enclosing.Set_Vertex_Kind (Enclose_Vertex);
      Vertex_Enclose := Insert_Vertex (G        => Graph,
                                       Parent   => This.Get_Cell_Parent,
                                       Id       => "enclose_mod",
                                       Value    => Value_Enclosing,
                                       X        => 0.0,
                                       Y        => 0.0,
                                       Width    => 0.0,
                                       Height   => 0.0 );
      
      Value_Op_Mod.Set_Node (Node);
      Value_Op_Mod.Set_Vertex_Kind (Operator_Vertex);
      Vertex_Op_Mod := Insert_Vertex (G        => Graph,
                                      Parent   => Vertex_Enclose,
                                      Id       => "op_mod",
                                      Value    => Value_Op_Mod,
                                      X        => 0.0,
                                      Y        => 0.0,
                                      Width    => Operator_Width,
                                      Height   => Operator_Height);
      Set_Cell (Node, Vertex_Op_Mod);
      Value_Enclosing.Set_Cell_To_Link (Vertex_Op_Mod);

      --  Create all formals cells
      
      Create_Formal_Vertex (This,
                            Node,
                            Rhs_Node,
                            True,
                            False,
                            String_Find ("IN2"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Lhs_Node,
                            True,
                            False,
                            String_Find ("IN1"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Empty,
                            False,
                            True,
                            String_Find ("OUT"));

      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Width (Node));
      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Height (Node));
      
      Place_Vertices (This, Vertex_Op_Mod);
       
      Dump_Vertex (This, Vertex_Op_Mod);
   end Fbd_Generate_Op_Mod;
   
   ---------------------------
   -- Fbd_Generate_Op_Expon --
   ---------------------------
   
   procedure Fbd_Generate_Op_Expon 
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is 
      
      Vertex_Enclose  : access Cell_Record;
      Vertex_Op_Expon : access Cell_Record;

      Lhs_Node : Node_Id := Left_Opnd (Node);
      Rhs_Node : Node_Id := Right_Opnd (Node);
      
      Value_Op_Expon  : Vertex_Value_Ptr := New_Vertex_Value;
      Value_Enclosing : Vertex_Value_Ptr := New_Vertex_Value;
      
      Graph : access Graph_Record := This.Get_Graph;
      use Cells_Lists;
   begin
      
      -- Create enclosing vertex
      
      Value_Enclosing.Set_Node (Node);
      Value_Enclosing.Set_Vertex_Kind (Enclose_Vertex);
      Vertex_Enclose := Insert_Vertex (G        => Graph,
                                       Parent   => This.Get_Cell_Parent,
                                       Id       => "enclose_exp",
                                       Value    => Value_Enclosing,
                                       X        => 0.0,
                                       Y        => 0.0,
                                       Width    => 0.0,
                                       Height   => 0.0 );
      
      Value_Op_Expon.Set_Node (Node);
      Value_Op_Expon.Set_Vertex_Kind (Operator_Vertex);
      Vertex_Op_Expon := Insert_Vertex (G        => Graph,
                                        Parent   => Vertex_Enclose,
                                        Id       => "op_exp",
                                        Value    => Value_Op_Expon,
                                        X        => 0.0,
                                        Y        => 0.0,
                                        Width    => Operator_Width,
                                        Height   => Operator_Height);
      Set_Cell (Node, Vertex_Op_Expon);
      Value_Enclosing.Set_Cell_To_Link (Vertex_Op_Expon);

      --  Create all formals cells
      
      Create_Formal_Vertex (This,
                            Node,
                            Rhs_Node,
                            True,
                            False,
                            String_Find ("IN2"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Lhs_Node,
                            True,
                            False,
                            String_Find ("IN1"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Empty,
                            False,
                            True,
                            String_Find ("OUT"));

      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Width (Node));
      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Height (Node));
      
      Place_Vertices (This, Vertex_Op_Expon);
       
      Dump_Vertex (This, Vertex_Op_Expon);
   end Fbd_Generate_Op_Expon;
   
   -------------------------
   -- Fbd_Generate_Op_Rem --
   -------------------------
   
   procedure Fbd_Generate_Op_Rem 
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is 
      
      Vertex_Enclose  : access Cell_Record;
      Vertex_Op_Rem   : access Cell_Record;

      Lhs_Node : Node_Id := Left_Opnd (Node);
      Rhs_Node : Node_Id := Right_Opnd (Node);
      
      Value_Op_Rem    : Vertex_Value_Ptr := New_Vertex_Value;
      Value_Enclosing : Vertex_Value_Ptr := New_Vertex_Value;
      
      Graph : access Graph_Record := This.Get_Graph;
      use Cells_Lists;
   begin
      
      -- Create enclosing vertex
      
      Value_Enclosing.Set_Node (Node);
      Value_Enclosing.Set_Vertex_Kind (Enclose_Vertex);
      Vertex_Enclose := Insert_Vertex (G        => Graph,
                                       Parent   => This.Get_Cell_Parent,
                                       Id       => "enclose_rem",
                                       Value    => Value_Enclosing,
                                       X        => 0.0,
                                       Y        => 0.0,
                                       Width    => 0.0,
                                       Height   => 0.0 );
      
      Value_Op_Rem.Set_Node (Node);
      Value_Op_Rem.Set_Vertex_Kind (Operator_Vertex);
      Vertex_Op_Rem := Insert_Vertex (G        => Graph,
                                      Parent   => Vertex_Enclose,
                                      Id       => "op_rem",
                                      Value    => Value_Op_Rem,
                                      X        => 0.0,
                                      Y        => 0.0,
                                      Width    => Operator_Width,
                                      Height   => Operator_Height);
      Set_Cell (Node, Vertex_Op_Rem);
      Value_Enclosing.Set_Cell_To_Link (Vertex_Op_Rem);

      --  Create all formals cells
      
      Create_Formal_Vertex (This,
                            Node,
                            Rhs_Node,
                            True,
                            False,
                            String_Find ("IN2"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Lhs_Node,
                            True,
                            False,
                            String_Find ("IN1"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Empty,
                            False,
                            True,
                            String_Find ("OUT"));

      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Width (Node));
      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Height (Node));
      
      Place_Vertices (This, Vertex_Op_Rem);
       
      Dump_Vertex (This, Vertex_Op_Rem);
   end Fbd_Generate_Op_Rem;
   
   --------------------------
   -- Fbd_Generate_Op_Plus --
   --------------------------
   
   procedure Fbd_Generate_Op_Plus
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is
      
      Vertex_Enclose  : access Cell_Record;
      Vertex_Op_Plus  : access Cell_Record;

      Rhs_Node : Node_Id := Right_Opnd (Node);
      
      Value_Op_Plus   : Vertex_Value_Ptr := New_Vertex_Value;
      Value_Enclosing : Vertex_Value_Ptr := New_Vertex_Value;
      
      Graph : access Graph_Record := This.Get_Graph;
      use Cells_Lists;
   begin
           
      Value_Enclosing.Set_Node (Node);
      Value_Enclosing.Set_Vertex_Kind (Enclose_Vertex);
      Vertex_Enclose := Create_Vertex (G        => Graph,
                                       Parent   => This.Get_Cell_Parent,
                                       Id       => "enclose_plus",
                                       Value    => Value_Enclosing,
                                       X        => 0.0,
                                       Y        => 0.0,
                                       Width    => Calculate_Enclose_Width (Node),
                                       Height   => Calculate_Enclose_Height (Node));
      
      Value_Op_Plus.Set_Node (Node);
      Value_Op_Plus.Set_Vertex_Kind (Operator_Vertex);
      Vertex_Op_Plus := Create_Vertex (G        => Graph,
                                       Parent   => null,
                                       Id       => "op_plus",
                                       Value    => Value_Op_Plus,
                                       X        => 0.0,
                                       Y        => 0.0,
                                       Width    => Operator_Width,
                                       Height   => Operator_Height);
      Set_Cell (Node, Vertex_Op_Plus);
      Value_Enclosing.Set_Cell_To_Link (Vertex_Op_Plus);

      --  Create all formals cells
      
      Create_Formal_Vertex (This,
                            Node,
                            Rhs_Node,
                            True,
                            False,
                            String_Find ("IN"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Empty,
                            False,
                            True,
                            String_Find ("OUT"));
      
      
      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Width (Node));
      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Height (Node));
      
      Place_Vertices (This, Vertex_Op_Plus);
       
      Dump_Vertex (This, Vertex_Op_Plus);
   end Fbd_Generate_Op_Plus;
   
   ---------------------------
   -- Fbd_Generate_Op_Minus --
   ---------------------------
   
   procedure Fbd_Generate_Op_Minus 
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is
      
      Vertex_Enclose    : access Cell_Record;
      Vertex_Op_Minus   : access Cell_Record;

      Rhs_Node : Node_Id := Right_Opnd (Node);
      
      Value_Op_Minus  : Vertex_Value_Ptr := New_Vertex_Value;
      Value_Enclosing : Vertex_Value_Ptr := New_Vertex_Value;
      
      Graph : access Graph_Record := This.Get_Graph;
      use Cells_Lists;
   begin
      
      -- Create enclosing vertex
      
      Value_Enclosing.Set_Node (Node);
      Value_Enclosing.Set_Vertex_Kind (Enclose_Vertex);
      Vertex_Enclose := Create_Vertex (G        => Graph,
                                       Parent   => This.Get_Cell_Parent,
                                       Id       => "enclose_minus",
                                       Value    => Value_Enclosing,
                                       X        => 0.0,
                                       Y        => 0.0,
                                       Width    => 0.0,
                                       Height   => 0.0 );
      
      Value_Op_Minus.Set_Node (Node);
      Value_Op_Minus.Set_Vertex_Kind (Operator_Vertex);
      Vertex_Op_Minus := Insert_Vertex (G        => Graph,
                                        Parent   => Vertex_Enclose,
                                        Id       => "op_minus",
                                        Value    => Value_Op_Minus,
                                        X        => 0.0,
                                        Y        => 0.0,
                                        Width    => Operator_Width,
                                        Height   => Operator_Height);
      Set_Cell (Node, Vertex_Op_Minus);
      Value_Enclosing.Set_Cell_To_Link (Vertex_Op_Minus);

      --  Create all formals cells
      
      Create_Formal_Vertex (This,
                            Node,
                            Rhs_Node,
                            True,
                            False,
                            String_Find ("IN"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Empty,
                            False,
                            True,
                            String_Find ("OUT"));

      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Width (Node));
      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Height (Node));
      
      Place_Vertices (This, Vertex_Op_Minus);
       
      Dump_Vertex (This, Vertex_Op_Minus);
   end Fbd_Generate_Op_Minus;
   
   -------------------------
   -- Fbd_Generate_Op_Abs --
   -------------------------
   
   procedure Fbd_Generate_Op_Abs
     (This : access Fbd_Builder_Record;
      Node : Node_Id) is
      
      Vertex_Enclose    : access Cell_Record;
      Vertex_Op_Abs     : access Cell_Record;

      Rhs_Node : Node_Id := Right_Opnd (Node);
      
      Value_Op_Abs    : Vertex_Value_Ptr := New_Vertex_Value;
      Value_Enclosing : Vertex_Value_Ptr := New_Vertex_Value;
      
      Graph : access Graph_Record := This.Get_Graph;
      use Cells_Lists;
   begin
      -- Create enclosing vertex
      
      Value_Enclosing.Set_Node (Node);
      Value_Enclosing.Set_Vertex_Kind (Enclose_Vertex);
      Vertex_Enclose := Create_Vertex (G        => Graph,
                                       Parent   => This.Get_Cell_Parent,
                                       Id       => "enclose_abs",
                                       Value    => Value_Enclosing,
                                       X        => 0.0,
                                       Y        => 0.0,
                                       Width    => 0.0,
                                       Height   => 0.0 );
      
      Value_Op_Abs.Set_Node (Node);
      Value_Op_Abs.Set_Vertex_Kind (Operator_Vertex);
      Vertex_Op_Abs := Insert_Vertex (G        => Graph,
                                      Parent   => Vertex_Enclose,
                                      Id       => "op_abs",
                                      Value    => Value_Op_Abs,
                                      X        => 0.0,
                                      Y        => 0.0,
                                      Width    => Operator_Width,
                                      Height   => Operator_Height);
      Set_Cell (Node, Vertex_Op_Abs);
      Value_Enclosing.Set_Cell_To_Link (Vertex_Op_Abs);

      --  Create all formals cells
      
      Create_Formal_Vertex (This,
                            Node,
                            Rhs_Node,
                            True,
                            False,
                            String_Find ("IN"));
      
      Create_Formal_Vertex (This,
                            Node,
                            Empty,
                            False,
                            True,
                            String_Find ("OUT"));

      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Width (Node));
      Vertex_Enclose.Get_Geometry.Set_Width (Calculate_Enclose_Height (Node));
      
      Place_Vertices (This, Vertex_Op_Abs);
       
      Dump_Vertex (This, Vertex_Op_Abs);
   end Fbd_Generate_Op_Abs;
   
end Reflex.Fbd_Gen_Ch4;
