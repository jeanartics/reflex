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

with Sinfo; use Sinfo;

package Reflex.Expanders.Expressions is

   procedure Change_Binary_Op_Kind
     (N             : Node_Id;
      New_Node_Kind : Node_Kind);

   procedure Negate_Expr (N : Node_Id);
   --  replace node N by its negation.

   procedure Negate_Op_And (N : Node_Id);
   --  Change recursvly exp1 and exp2 To not exp1 or not exp2

   procedure Negate_Op_Or (N : Node_Id);
   --  Change recursvly exp1 or exp2 To not exp1 and not exp2

   procedure Negate_Op_Xor (N : Node_Id);
   --  Rewrite Lhs xor Rhs into (lhs and Rhs) or (not lhs and not Rhs)

   procedure Negate_Op_Eq (N : Node_Id);
   --  Change exp1 = exp2 To not exp1 /= exp2

   procedure Negate_Op_Ge (N : Node_Id);
   --  Change exp1 >= exp2 To not exp1 < exp2

   procedure Negate_Op_Gt (N : Node_Id);
   --  Change exp1 > exp2 To not exp1 <= exp2

   procedure Negate_Op_Le (N : Node_Id);
   --  Change exp1 <= exp2 To not exp1 > exp2

   procedure Negate_Op_Lt (N : Node_Id);
   --  Change exp1 < exp2 To not exp1 >= exp2

   procedure Negate_Op_Ne (N : Node_Id);
   --  Change exp1 To not exp1

   procedure Negate_Op_In (N : Node_Id);

   procedure Negate_Op_Not_In (N : Node_Id);

   procedure Negate_Expression_With_Actions (N : Node_Id);

   procedure Negate_Simple_Operand (N : Node_Id);

end Reflex.Expanders.Expressions;
