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

with Nlists;   use Nlists;
with Sinfo;    use Sinfo;
with Atree;    use Atree;
with Nmake;    use Nmake;

with Reflex.Boxes.Duals; use Reflex.Boxes.Duals;
with Ada.Text_IO; use Ada.Text_IO;

package body Reflex.Boxes.Utils is

   -----------------------------
   -- Has_Non_Null_Statements --
   -----------------------------

   function Has_Non_Null_Statements (L : List_Id) return Boolean is
      Node : Node_Id;
   begin
      if Is_Non_Empty_List (L) then
         Node := First (L);

         loop
            if Nkind (Node) /= N_Null_Statement then
               return True;
            end if;

            Next (Node);
            exit when Node = Empty;
         end loop;
      end if;

      return False;
   end Has_Non_Null_Statements;

   ---------
   -- Max --
   ---------

   function Max (X : Natural; Y : Natural) return Natural is
   begin
      if X < Y then
         return Y;
      else
         return X;
      end if;
   end Max;

   ---------
   -- Min --
   ---------

   function Min (X : Natural; Y : Natural) return Natural is
   begin
      if X > Y then
         return Y;
      else
         return X;
      end if;
   end Min;

   ----------------------
   -- Check_Exit_In_If --
   ----------------------

   procedure Check_Exit_In_If
     (This        : access Reflex_Expander_Record;
      Node_If     : Node_Id;
      Goto_End_Id : Node_Id) is

      Stmts : List_Id;
      Stmt  : Node_Id;
   begin

      --  this procedure will check if IF_STATEMENT are presents in loop and if
      --  in this IF_STATEMENT, an other IF_STATEMENT is present. In each
      --  IF_STATEMENT found, we check if an EXIT_STATEMENT is present and if
      --  it is, expand it.

      if Nkind (Node_If) /= N_If_Statement then
         return;
      end if;

      Stmts := Then_Statements (Node_If);
      Stmt := First (Stmts);

      Check_Exit (This, Stmts, Goto_End_Id);

      Next (Stmt);

   end Check_Exit_In_If;

   ----------------
   -- Check_Exit --
   ----------------

   procedure Check_Exit
     (This        : access Reflex_Expander_Record;
      Stmts       : List_Id;
      Goto_End_Id : Node_Id) is

      Stmt     : Node_Id;
      Goto_End : Node_Id;
      New_If   : Node_Id;
      Elif     : Node_Id;

   begin
      Put_Line (" Check_Exit begin");
      Stmt := First (Stmts);
      while Present (Stmt) loop
         Put_Line ("Le noeud est" & Nkind (Stmt)'Img);

         if Nkind (Stmt) = N_If_Statement then
            Check_Exit (This, Then_Statements (Stmt), Goto_End_Id);

            if Present (Elsif_Parts (Stmt)) then
               Elif := First (Elsif_Parts (Stmt));
               while Present (Elif) loop
                  Check_Exit (This, Then_Statements (Elif), Goto_End_Id);
                  Next (Elif);
               end loop;
               if Present (Else_Statements (Stmt)) then
                  Check_Exit (This, Else_Statements (Stmt), Goto_End_Id);
               end if;
            end if;
         end if;

            if Nkind (Stmt) /= N_Exit_Statement then
               Put_Line ("2");
               Next (Stmt);

            else
               if not Present (Condition (Stmt)) then
                  Put_Line ("3");

                  --  Case of simple exit whithout condition and name

                  if not Present (Name (Stmt)) then
                     Put_Line ("4");

                     --  We replace exit statement by a Goto to jump out of loop

                     --  expand :
                     --  Exit;

                     --  To

                     --  goto Lend;
                     --  ...
                     --  <<Lend>>

                     Goto_End :=
                       Make_Goto_Statement
                         (Sloc (Stmt),
                          Goto_End_Id);

                     Replace (Stmt, Goto_End);
                  else
                     --  expand :
                     --  ...
                     --  Exit NAME;
                     --  ...

                     --  To

                     --  ...
                     --  Goto NAME;
                     --  ...
                     --  <<NAME>> (used to jump out of loop1)

                     ---------------------- /!\ TO DO : exit NAME; ----------------------
                     -- ouvrir nouveau scope pour chaque boucle ayant un identifier
                     -- (Outer1 : Loop ...) pour pouvoir récuperer le nom du lend et
                     -- jump dessus

                     null;

                  end if;
               else
                  Put_Line ("5");

                  if not Present (Name (Stmt)) then
                     Put_Line ("6");

                     --  expand :
                     --  ...
                     --  Exit when CONDITION;
                     --  ...

                     --  To

                     --  If CONDITION then
                     --      goto Lend;
                     --  end if;
                     --  ...
                     --  <<Lend>> (used to jump out of loop)

                     Goto_End :=
                       Make_Goto_Statement
                         (Sloc (Stmt),
                          Goto_End_Id);

                     New_If := Make_If_Statement
                       (Sloc            => Sloc (Stmt),
                        Condition       => Condition (Stmt),
                        Then_Statements => New_List);

                     Append (Goto_End, Then_Statements (New_If));
                     Replace (Stmt, New_If);
                  end if;
               end if;
            end if;
            Next (Stmt);
         end loop;
         Put_Line (" Check_Exit end");
      end Check_Exit;

      -------------------
      -- Add_Has_Vlink --
      -------------------

      procedure Add_Has_Vlink (B : access Box_Record'Class) is
      begin
         case B.Get_Box_Kind is

         when Dual_Box =>
            declare
               Dual : access Dual_Box_Record := Dual_Box_Ptr (B);
            begin
               Add_Has_Vlink (Dual.Get_Box2);
            end;

         when Terminal_Box =>
            B.Set_Has_Vlink (True);

         when others =>
            pragma Assert (False);
         end case;
      end Add_Has_Vlink;

      ----------------------
      -- Add_Matrix_Vlink --
      ----------------------

      procedure Add_Matrix_Vlink
        (Matrix : access Matrix_Record'Class;
         X      : Natural;
         Y      : Natural) is

         B : access Box_Record'Class;
      begin
         B := Get_Item (Matrix, X, Y);

         if B = null then
            Set_Item (Matrix, X, Y, The_Vlink_Box);

         elsif B = The_Hlink_Box then
            Set_Item (Matrix, X, Y, The_Hlink_Vlink_Box);

         elsif B = The_Busy_Box then
            declare
               Count : Integer := X;
            begin
               while not Is_Valid_Box( Get_Item (Matrix, Count, Y)) loop
                  Count := Count -1;
               end loop;
               Get_Item (Matrix, Count, Y).Set_Has_Vlink (True);
            end;

         else
            B.Set_Has_Vlink (True);
         end if;
      end Add_Matrix_Vlink;

   end Reflex.Boxes.Utils;
