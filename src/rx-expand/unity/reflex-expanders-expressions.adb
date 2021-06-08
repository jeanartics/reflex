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

with Atree;    use Atree;
with Nmake;    use Nmake;
with Sem_Util; use Sem_Util;

package body Reflex.Expanders.Expressions is
   
   ---------------------------
   -- Change_Binary_Op_Kind --
   ---------------------------

   procedure Change_Binary_Op_Kind
     (N             : Node_Id; 
      New_Node_Kind : Node_Kind) is
      
      use Atree_Private_Part;
   begin
      Nodes.Table (N).Nkind := New_Node_Kind;
   end Change_Binary_Op_Kind;
   
   -----------------
   -- Negate_Expr --
   -----------------
   
   procedure Negate_Expr (N : Node_Id) is
   begin
      case Nkind (N) is
	 when N_Op_And =>
	    Negate_Op_And (N);
	    
	 when N_Op_Or =>
	    Negate_Op_Or (N);
	    
	 when N_Op_Xor =>
	    Negate_Op_Xor (N);
	    
	 when N_Op_Eq =>
	    Negate_Op_Eq (N);
	    
	 when N_Op_Ge =>
	    Negate_Op_Ge (N);
	    
	 when N_Op_Gt =>
	    Negate_Op_Gt (N);
	    
	 when N_Op_Le => 
	    Negate_Op_Le (N);
	    
	 when N_Op_Lt =>
	    Negate_Op_Lt (N);
	    
	 when N_Op_Ne =>
	    Negate_Op_Ne (N);
	    
	 when N_In =>
	    Negate_Op_In (N);
	    
	 when N_Not_In =>
	    Negate_Op_Not_In (N);
	    
	    --        when N_Expression_With_Actions =>
	    --           Negate_Expression_With_Actions (N);
	    --  	   
	 when others =>   
	    Negate_Simple_Operand (N);
      end case;
   end Negate_Expr;
   
   -------------------
   -- Negate_Op_And --
   -------------------
   
   procedure Negate_Op_And (N : Node_Id) is
      
      Lhs : Node_Id;
      Rhs : Node_Id;
   begin
      --  Change recursclye expression exp1 and exp2 To not exp1 or not exp2. 
      
      Lhs := Left_Opnd (N);
      Negate_Expr (Lhs);
      
      Rhs := Right_Opnd (N);
      Negate_Expr (Rhs);
      
      Change_Binary_Op_Kind (N, N_Op_Or);
   end Negate_Op_And;
   
   ------------------
   -- Negate_Op_Or --
   ------------------
   
   procedure Negate_Op_Or (N : Node_Id) is
      
      Lhs : Node_Id;
      Rhs : Node_Id;
   begin
      --  Change recursclye expression exp1 ord exp2 To not exp1 and not exp2. 
      
      Lhs := Left_Opnd (N);
      Negate_Expr (Lhs);
      
      Rhs := Right_Opnd (N);
      Negate_Expr (Rhs);
      
      Change_Binary_Op_Kind (N, N_Op_And);
   end Negate_Op_Or;
     
   ------------------
   -- Negate_Op_Xor --
   ------------------
   
   procedure Negate_Op_Xor (N : Node_Id) is
      
      Lhs      : Node_Id;
      Rhs      : Node_Id;
      Not_Lhs  : Node_Id;
      Not_Rhs  : Node_Id;
      New_Expr : Node_Id;
   begin
      --  Rewrite Lhs xor Rhs into (lhs and Rhs) or (not lhs and not Rhs)
      
      Lhs := Left_Opnd (N);
      Rhs := Right_Opnd (N);
      
      Not_Lhs := New_Copy_Tree (Lhs);
      Negate_Expr (Not_Lhs);
      
      Not_Rhs := New_Copy_Tree (Rhs);
      Negate_Expr (Not_Rhs);
	
      New_Expr := Make_Op_Or
        (Sloc        => Sloc (N),
	 
         Left_Opnd   =>  
           Make_Op_And
             (Sloc        => Sloc (Lhs),
              Left_Opnd   => Lhs, 
              Right_Opnd  => Rhs),
		 
         Right_Opnd  => 
           Make_Op_And
             (Sloc        => Sloc (Rhs),
              Left_Opnd   => Not_Lhs,
              Right_Opnd  => Not_Rhs)
        );
      
      Set_Etype (New_Expr, Etype (N));
      
      Replace (N, New_Expr);
   end Negate_Op_Xor;
   
   -----------------
   -- Neage_Op_Eq --
   -----------------
   
   procedure Negate_Op_Eq (N : Node_Id) is
   begin
      Change_Binary_Op_Kind (N, N_Op_Ne);
   end Negate_Op_Eq;
   
   ------------------
   -- Negate_Op_Ge --
   ------------------
   
   procedure Negate_Op_Ge (N : Node_Id) is
   begin
      Change_Binary_Op_Kind (N, N_Op_Lt);
   end Negate_Op_Ge;
     
   ------------------
   -- Negate_Op_Gt --
   ------------------
   
   procedure Negate_Op_Gt (N : Node_Id) is
   begin
      Change_Binary_Op_Kind (N, N_Op_Le);
   end Negate_Op_Gt;
     
   ------------------
   -- Negate_Op_Le --
   ------------------
   
   procedure Negate_Op_Le (N : Node_Id) is
   begin
      Change_Binary_Op_Kind (N, N_Op_Gt);
   end Negate_Op_Le;
     
   ------------------
   -- Negate_Op_Lt --
   ------------------
   
   procedure Negate_Op_Lt (N : Node_Id) is
   begin
      Change_Binary_Op_Kind (N, N_Op_Ge);
   end Negate_Op_Lt;
     
   ------------------
   -- Negate_Op_Ne --
   ------------------
   
   procedure Negate_Op_Ne (N : Node_Id) is
   begin
      Change_Binary_Op_Kind (N, N_Op_Eq);
   end Negate_Op_Ne;
   
   ------------------
   -- Negate_Op_In --
   ------------------
   
   procedure Negate_Op_In (N : Node_Id) is
   begin
      Change_Binary_Op_Kind (N, N_Not_In);
   end Negate_Op_In;
   
   ----------------------
   -- Negate_Op_Not_In --
   ----------------------
   
   procedure Negate_Op_Not_In (N : Node_Id) is
   begin
      Change_Binary_Op_Kind (N, N_In);
   end Negate_Op_Not_In;
   
   ------------------------------------
   -- Negate_Expression_With_Actions --
   ------------------------------------
   
   procedure Negate_Expression_With_Actions (N : Node_Id) is
   
      Expr     : Node_Id;
      Not_Expr : Node_Id;
   begin
      Expr := Expression (N);
      if Present (Expr) then
         Not_Expr := Make_Op_Not
           (Sloc       => Sloc (N),
            Right_Opnd => New_Copy_Tree (Expr));
         Set_Etype (Not_Expr, Etype (N));
	 
         Replace (Expr, Not_Expr);
      end if;
   end Negate_Expression_With_Actions;
   
   ---------------------------
   -- Negate_Simple_Operand --
   ---------------------------
   
   procedure Negate_Simple_Operand (N : Node_Id) is 
      Not_Expr : Node_Id;
   begin
      if Nkind (N) /= N_Op_Not then
         Not_Expr := Make_Op_Not
           (Sloc       => Sloc (N),
            Right_Opnd => New_Copy_Tree (N));
         Set_Etype (Not_Expr, Etype (N));
         
         Replace (N, Not_Expr);
      else
         Replace (N, Right_Opnd (N));
      end if;
   end Negate_Simple_Operand;
   
end Reflex.Expanders.Expressions;
