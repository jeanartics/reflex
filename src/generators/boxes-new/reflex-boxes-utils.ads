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

with Reflex.Expanders; use Reflex.Expanders;

package Reflex.Boxes.Utils is

   function Has_Non_Null_Statements
     (L : List_Id) return Boolean;

   function Max (X : Natural; Y : Natural) return Natural;
   function Min (X : Natural; Y : Natural) return Natural;

   procedure Check_Exit_In_If
     (This        : access Reflex_Expander_Record;
      Node_If     : Node_Id;
      Goto_End_Id : Node_Id);

   procedure Check_Exit
     (This        : access Reflex_Expander_Record;
      Stmts       : List_Id;
      Goto_End_Id : Node_Id);

   procedure Add_Has_Vlink (B : access Box_Record'Class);

   procedure Add_Matrix_Vlink
     (Matrix : access Matrix_Record'Class;
      X      : Natural;
      Y      : Natural);

end Reflex.Boxes.Utils;
