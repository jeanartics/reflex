------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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
-- Reflex is a fork from the GNAT compiler. GNAT was originally developed   --
-- by the GNAT team at  New York University. Extensive  contributions to    --
-- GNAT were provided by Ada Core Technologies Inc. Reflex is developed  by --
-- the Artics team at Grenoble.                                             --
--                                                                          --
------------------------------------------------------------------------------

with Sinfo.CN; use Sinfo.CN;

separate (Par)
package body Ch11 is

   ------------------------------------------
   -- 11.2  Handled Sequence Of Statements --
   ------------------------------------------

   --  HANDLED_SEQUENCE_OF_STATEMENTS ::=
   --      SEQUENCE_OF_STATEMENTS
   --    [exception
   --      EXCEPTION_HANDLER
   --      {EXCEPTION_HANDLER}]

   --  Error_Recovery : Cannot raise Error_Resync

   function P_Handled_Sequence_Of_Statements return Node_Id is
      Handled_Stmt_Seq_Node : Node_Id;

   begin
      Handled_Stmt_Seq_Node :=
        New_Node (N_Handled_Sequence_Of_Statements, Token_Ptr);
      Set_Statements
        (Handled_Stmt_Seq_Node, P_Sequence_Of_Statements (SS_Extm_Sreq));

      return Handled_Stmt_Seq_Node;
   end P_Handled_Sequence_Of_Statements;

end Ch11;
