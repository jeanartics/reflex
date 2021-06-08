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

with Reflex.Fbd_Builders; use Reflex.Fbd_Builders;

package Reflex.Fbd_Gen_Ch4 is
   
   --  Same code for all binaries operands and same code for all unaries
   --  operands.
   
   --  A binary operand is a vertex wich has 2 formal_in vertices and only 1
   --  formal_out vertex. A binary operand vertex and his formals are contain 
   --  in an enclosing vertex.
   
   --  An unary operand is a vertex wich has only 1 formal_in vertex and 1
   --  formal_out vertex. A unary operand and his formals are contain in an 
   --  enclosing vertex.
   
   procedure Fbd_Generate_Op_And
     (This : access Fbd_Builder_Record;
      Node : Node_Id);   
   
   procedure Fbd_Generate_Op_Not
     (This : access Fbd_Builder_Record;
      Node : Node_Id);

   procedure Fbd_Generate_Op_Or
     (This : access Fbd_Builder_Record;
      Node : Node_Id);
 
   procedure Fbd_Generate_Op_Eq 
     (This : access Fbd_Builder_Record;
      Node : Node_Id);

   procedure Fbd_Generate_Op_Ne
     (This : access Fbd_Builder_Record;
      Node : Node_Id);
   
   procedure Fbd_Generate_Op_Ge 
     (This : access Fbd_Builder_Record;
      Node : Node_Id);
            
   procedure Fbd_Generate_Op_Gt 
     (This : access Fbd_Builder_Record;
      Node : Node_Id);
            
   procedure Fbd_Generate_Op_Le 
     (This : access Fbd_Builder_Record;
      Node : Node_Id);
   
   procedure Fbd_Generate_Op_Lt 
     (This : access Fbd_Builder_Record;
      Node : Node_Id);
   
   procedure Fbd_Generate_Op_Shift_Right
     (This : access Fbd_Builder_Record;
      Node : Node_Id);
   
   procedure Fbd_Generate_Op_Shift_Left
     (This : access Fbd_Builder_Record;
      Node : Node_Id);
               
   procedure Fbd_Generate_Op_Add
     (This : access Fbd_Builder_Record;
      Node : Node_Id);
   
   procedure Fbd_Generate_Op_Substract
     (This : access Fbd_Builder_Record;
      Node : Node_Id);
   
   procedure Fbd_Generate_Op_Divide
     (This : access Fbd_Builder_Record;
      Node : Node_Id);
   
   procedure Fbd_Generate_Op_Multiply
     (This : access Fbd_Builder_Record;
      Node : Node_Id);
   
   procedure Fbd_Generate_Op_Xor
     (This : access Fbd_Builder_Record;
      Node : Node_Id);
   
   procedure Fbd_Generate_Op_Plus
     (This : access Fbd_Builder_Record;
      Node : Node_Id);
   
   procedure Fbd_Generate_Op_Minus 
     (This : access Fbd_Builder_Record;
      Node : Node_Id);
   
   procedure Fbd_Generate_Op_Abs
     (This : access Fbd_Builder_Record;
      Node : Node_Id);
   
   procedure Fbd_Generate_Op_Mod 
     (This : access Fbd_Builder_Record;
      Node : Node_Id);
   
   procedure Fbd_Generate_Op_Expon 
     (This : access Fbd_Builder_Record;
      Node : Node_Id);
   
   procedure Fbd_Generate_Op_Rem 
     (This : access Fbd_Builder_Record;
      Node : Node_Id);
   
end Reflex.Fbd_Gen_Ch4;
