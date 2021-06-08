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

with Sinfo;     use Sinfo;
with Atree;     use Atree;
with Nlists;    use Nlists;
with Nmake;     use Nmake;

with Reflex.Expanders.Dispatch;     use Reflex.Expanders.Dispatch;
with Reflex.Expanders.Expressions;  use Reflex.Expanders.Expressions;
with Reflex.Infos;                  use Reflex.Infos;

package body Reflex.Boxes.Exp_Ch4 is

   ------------------------------
   -- Expand_Op_Xor_For_Ladder --
   ------------------------------

   procedure Expand_Op_Xor_For_Ladder
     (This : access Reflex_Expander_Record;
      N    : Node_Id) is

      Lhs     : Node_Id := Left_Opnd (N);
      Rhs     : Node_Id := Right_Opnd (N);
      Not_Lhs : Node_Id;
      Not_Rhs : Node_Id;
      And1    : Node_Id;
      And2    : Node_Id;
      Or1     : Node_Id;
   begin
      --  Expand :
      --  A xor B;
      --  To
      --  (A and not B) or (not A and B);

      Expand_Node (This, Lhs);
      Expand_Node (This, Rhs);

      Not_Lhs := Make_Op_Not (Sloc (N), Lhs);
      Not_Rhs := Make_Op_Not (Sloc (N), Rhs);

      And1 := Make_Op_And (Sloc (N), Lhs, Not_Rhs);
      And2 := Make_Op_And (Sloc (N), Rhs, Not_Lhs);

      Or1 := Make_Op_Or (Sloc (N), And1, And2);

      Insert_Before (N, Or1);
      Remove (N);

   end Expand_Op_Xor_For_Ladder;

   ------------------------------
   -- Expand_Op_Not_For_Ladder --
   ------------------------------

   procedure Expand_Op_Not_For_Ladder
     (This : access Reflex_Expander_Record;
      N    : Node_Id) is

      Rhs : Node_Id := Right_Opnd (N);
   begin

      --  here we are in case where Not's Right_Opnd contain a parenthesized
      --  expression

      if Nkind (Rhs) in N_Op_Boolean then

         --  here we are in case where this N_Op_Not is in a bigger expression

         if Nkind (Parent (N)) = N_Op_Or
           or Nkind (Parent (N)) = N_Op_And then

            --  Expand :
            --  A and not (C And D)
            --  To
            --  A and not C and not D

            --  Expand :
            --  A and not (C > D)
            --  To
            --  A and not < D

            Negate_Expr (Rhs);
            Rewrite (N, Rhs);
         else

            --  here we are in case where this N_Op_Not is a full expression

            if Nkind (Parent (N)) = N_Assignment_Statement then

               --  Expand :
               --  A := not (A and B)
               --  To
               --  not A := A and B

               Negate_Expr (Rhs);
               Rewrite (N, Rhs);

               --  This flag allow us to use not coil in Ladder
               --  But it is not use currently

               Set_Negated_Assignment (Parent (N), True);

            else

               --  Expand :
               --  not (A and C And D)
               --  To
               --  not A and not C and not D

               --  Expand :
               --  not (A > B);
               --  To
               --  A < B;

               Negate_Expr (Rhs);
               Rewrite (N, Rhs);
            end if;
         end if;
      end if;
   end Expand_Op_Not_For_Ladder;

end Reflex.Boxes.Exp_Ch4;
