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

with Einfo;    use Einfo;
with Atree;    use Atree;
with Nlists;   use Nlists;
with Stand;    use Stand;
with Namet;    use Namet;
with Uintp;    use Uintp;
with Sinfo;    use Sinfo;
with Sem_Eval; use Sem_Eval;

with Reflex.Boxes;            use Reflex.Boxes;
with Reflex.Boxes.Duals;      use Reflex.Boxes.Duals;
with Reflex.Boxes.Dispatch;   use Reflex.Boxes.Dispatch;
with Reflex.Boxes.Terminals ; use Reflex.Boxes.Terminals;
with Reflex.Boxes.Multis;     use Reflex.Boxes.Multis;
with Reflex.Infos;            use Reflex.Infos;
with Reflex.Boxes.Ch2;        use Reflex.Boxes.Ch2;

package body Reflex.Boxes.Ch5 is
   
   ------------------------------------------------
   -- Boxes_Build_Handled_Sequence_Of_Statements --
   ------------------------------------------------
   
   procedure Boxes_Build_Handled_Sequence_Of_Statements 
     (This : access Builder_Record;
      Node : Node_Id) is
      
      Stmts       : List_Id;
      Stmt        : Node_Id;
   begin
      Stmts := Statements (Node);
      if Is_Non_Empty_List (Stmts) then
         Stmt := First (Stmts);

         while Present (Stmt) loop
            -- Write_Comment_Line_To_Node (This, Stmt);
            Boxes_Node_Dispatch (This, Stmt);
            Next (Stmt);
         end loop;
      end if;
   end Boxes_Build_Handled_Sequence_Of_Statements;
   
   --------------------------------------
   -- Boxes_Build_Assignment_Statement --
   --------------------------------------
     
   procedure Boxes_Build_Assignment_Statement
     (This : access Builder_Record;
      Node : Node_Id) is
      
      Dual_Assign : access Dual_Box_Record;
      Term_Assign : access Terminal_Box_Record;
      B1          : access Box_Record'Class;
      B2          : access Box_Record'Class;
      
      Lhs     : Node_Id := Name (Node);
      Rhs     : Node_Id := Expression (Node);
   begin
      
      --  If Etype of current assignment is Standard_Boolean, we have to create
      --  a dual_box which contain variable which will be affect on his Rhs
      --  and expression of assignment on his Lhs. We volontary reverse it
      --  because of placement.
      
      if not This.In_If_Statement 
        and then Is_Boolean_Type (Etype (Lhs)) 
      then
         if Compile_Time_Known_Value (Rhs) then
	    
            Term_Assign := New_Terminal_Box;
            Term_Assign.Set_Width (1);
            Term_Assign.Set_Height (1);
            Term_Assign.Set_Is_Action_Box (True);
            Term_Assign.Set_Node (Lhs);
            Set_Box (Lhs, Term_Assign);
	    
            if Expr_Value (Rhs) = Uint_1 then
               Term_Assign.Set_Typ (Coil_Box);
            else
               Term_Assign.Set_Typ (Not_Coil_Box);
            end if;
            
            if not This.In_If_Statement then
               Dual_Assign := New_Dual_Box;
            
               B1 := The_True_Box;
               Dual_Assign.Set_Box1 (B1);
               Dual_Assign.Set_Box2 (Term_Assign);
            
               This.Append_Box (Dual_Assign);
            else
               This.Append_Box (Term_Assign);
            end if;
         else
            Dual_Assign := New_Dual_Box;
            Dual_Assign.Set_Node (Node);
            Set_Box (Node, Dual_Assign);
            
            Boxes_Node_Dispatch (This, Rhs);
            B1 := Get_Box (Rhs);
            B1.Set_Parent_Box (Dual_Assign);
	    
            Boxes_Create_Coil (This, Lhs);
            B2 := Get_Box (Lhs);
            B2.Set_Parent_Box (Dual_Assign);
	    
            Dual_Assign.Set_Box1 (B1);
            Dual_Assign.Set_Box2 (B2);
            
            This.Append_Box (Dual_Assign);
         end if;
      
      else
         Term_Assign := New_Terminal_Box;
         Term_Assign.Set_Width (4);
         Term_Assign.Set_Height (1);
         Term_Assign.Set_Is_Action_Box (True);
         Set_Box (Node, Term_Assign);
         Term_Assign.Set_Node (Node);
         Term_Assign.Set_Typ (Operate_Block_Box);
         
         if not This.In_If_Statement then
            Dual_Assign := New_Dual_Box;
            
            B1 := The_True_Box;
            Dual_Assign.Set_Box1 (B1);
            Dual_Assign.Set_Box2 (Term_Assign);
            
            This.Append_Box (Dual_Assign);
         end if;
      end if;
   end Boxes_Build_Assignment_Statement;
   
   ------------------
   -- Build_If_Box --
   ------------------
   
   procedure Build_If_Box 
     (This : access Builder_Record;
      Node : Node_Id) is
      
      Cond      : Node_Id := Condition (Node);
      Stmts     : List_Id := Then_Statements (Node);
      Stmt      : Node_Id;
      If_Box    : access Dual_Box_Record;
      Cond_Box  : access Box_Record'Class;
      Stmts_Box : access Multi_Box_Record;
   begin
      --  Node is either the If part or an elsif part of an if statement.
      
      --  An if is generated as a dual box with the expression in the box1 and
      --  the then statements as a multi box.
      
      --  The If statement is associated with the dual box. The expression
      --  is put in box1 of the dual box and the statements are put the multi
      --  box.      
      This.Set_In_If_Statement (True);
      
      If_Box := New_Dual_Box;
      If_Box.Set_Node (Node);
      Set_Box (Node, If_Box);
      
      If_Box.Set_Orientation (Horizontal);
      
      --  Build the condition or expression box
      
      Boxes_Node_Dispatch (This, Cond);
      Cond_Box := Get_Box (Cond);
      Cond_Box.Set_Parent_Box (If_Box);
      If_Box.Set_Box1 (Cond_Box);
      
      --  Build the Statements List

      Stmts_Box := New_Multi_Box;
      Stmts_Box.Set_Parent_Box (If_Box);
      Stmts_Box.Set_Orientation (Vertical);
      If_Box.Set_Box2 (Stmts_Box);
      
      Stmt := First (Then_Statements (Node));
      while Present (Stmt) loop
         Boxes_Node_Dispatch (This, Stmt);
         Get_Box (Stmt).Set_Parent_Box (Stmts_Box);
         Append_Child_Box (Stmts_Box, Get_Box (Stmt));
         
         Next (Stmt);
      end loop;
      
      This.Append_Box (If_Box);
   end Build_If_Box;
   
   ------------------------------
   -- Boxes_Build_If_Statement --
   ------------------------------
   
   procedure Boxes_Build_If_Statement
     (This : access Builder_Record;
      Node : Node_Id) is
      
      Stmt     : Node_Id;
      Elif     : Node_Id;
      Stmts    : List_Id;
      
      use Boxes_Lists;
   begin
      This.Open_Scope;

      --  The If/Then part
      
      Build_If_Box (This, Node);
      
      --  The Elsif(s) part(s)
      
      if Present (Elsif_Parts (Node)) then
         Elif := First (Elsif_Parts (Node));
         while Present (Elif) loop
            Build_If_Box (This, Elif);
            Next (Elif);
         end loop;
      end if;
      
      --  The Else part
      
      Stmts := Else_Statements (Node);
      if Present (Stmts) then
         Stmt := First (Stmts);
         while Present (Stmt) loop
            Boxes_Node_Dispatch (This, Stmt);
            This.Append_Box (Get_Box (Stmt));
            Next (Stmt);
         end loop;
      end if;	 
      
      This.Close_Scope;
   end Boxes_Build_If_Statement;
   
   --------------------------------
   -- Boxes_Build_Goto_Statement --
   --------------------------------
   
   procedure Boxes_Build_Goto_Statement
     (This : access Builder_Record;
      Node : Node_Id) is
      
      Goto_Box : access Terminal_Box_Record;
   begin
      
      --  We does not dispatch other node(s) because a N_Goto_Statement means
      --  that the current box is a Terminal_Box and his typ is Jump_Box. 
      
      Goto_Box := New_Terminal_Box;
      Goto_Box.Set_Node (Node);
      Goto_Box.Set_Typ (Jump_Box);
      Goto_Box.Set_Is_Action_Box (True);
      Goto_Box.Set_Height (1);
      Goto_Box.Set_Width (1);
      
      Set_Box (Node, Goto_Box);     
      
      --  If goto is not in If_Statement, we have to create a Dual_Box with
      --  The_True_Box as first child box to create Vlink
      
      if not This.In_If_Statement then
         declare
            Dual_Goto : access Dual_Box_Record;
            B1        : access Box_Record'Class;
         begin
            Dual_Goto := New_Dual_Box;
            
            B1 := The_True_Box;
            Dual_Goto.Set_Box1 (B1);
            Dual_Goto.Set_Box2 (Goto_Box);
            
            This.Append_Box (Dual_Goto);
         end;
      end if;      
   end Boxes_Build_Goto_Statement;
     
   -----------------------
   -- Boxes_Build_Label --
   -----------------------
   
   procedure Boxes_Build_Label
     (This : access Builder_Record;
      Node : Node_Id) is
      
      B : access Terminal_Box_Record;
   begin
      
      --  We does not dispatch other node(s) because a N_Label means
      --  that the current box is a Terminal_Box and his typ is Label_Box. 
      B := New_Terminal_Box;
      B.Set_Node (Node);
      B.Set_Typ (Label_Box);
      B.Set_Height (1);
      B.Set_Width (1);
      
      Set_Box (Node, B);
      
      --  If label is in If_Statement, it is already in builder's 
      --  Subp_Boxes list
      
      if not This.In_If_Statement then
         This.Append_Box (B);
      end if;    
   end Boxes_Build_Label;
   
   ------------------------------------------
   -- Boxes_Build_Procedure_Call_Statement --
   ------------------------------------------
    
   procedure Boxes_Build_Procedure_Call_Statement
     (This : access Builder_Record;
      Node : Node_Id) is
   begin
      null;
   end Boxes_Build_Procedure_Call_Statement;
   
   --------------------------------
   -- Boxes_Build_Null_Statement --
   --------------------------------
    
   procedure Boxes_Build_Null_Statement
     (This : access Builder_Record;
      Node : Node_Id) is
   begin
      null;
   end Boxes_Build_Null_Statement;
      
end Reflex.Boxes.Ch5;
