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

package Rxada.Gen.Ch5 is

   procedure Generate_Sequence_Of_Statements
     (This  : access Ada_Generator_Record;
      Stmts : List_Id);
   
   procedure Generate_Assignment_Statement 
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Block_Statement 
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   procedure Generate_Case_Statement 
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   procedure Generate_Case_Statement_Alternative 
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   procedure Generate_Code_Statement 
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Exit_Statement 
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Free_Statement 
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   procedure Generate_Goto_Statement 
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   procedure Generate_Handled_Sequence_Of_Statements 
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   procedure Generate_If_Statement 
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   procedure Generate_Label 
     (This : access Ada_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Loop_Statement 
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   procedure Generate_Null_Statement 
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   procedure Generate_Procedure_Call_Statement 
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   procedure Generate_Raise_Expression 
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   procedure Generate_Raise_Error 
     (This : access Ada_Generator_Record;
      Node : Node_Id);

   procedure Generate_Simple_Return_Statement 
     (This : access Ada_Generator_Record;
      Node : Node_Id);

end Rxada.Gen.Ch5;
