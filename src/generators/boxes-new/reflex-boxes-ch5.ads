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

with Reflex.Boxes.Builders; use Reflex.Boxes.Builders;

package Reflex.Boxes.Ch5 is

   procedure Boxes_Build_Handled_Sequence_Of_Statements 
     (This : access Builder_Record;
      Node : Node_Id);
   
   procedure Boxes_Build_Assignment_Statement
     (This : access Builder_Record;
      Node : Node_Id);
   
   procedure Build_If_Box 
     (This : access Builder_Record;
      Node : Node_Id);
      
   procedure Boxes_Build_If_Statement
     (This : access Builder_Record;
      Node : Node_Id);
   
   procedure Boxes_Build_Goto_Statement
     (This : access Builder_Record;
      Node : Node_Id);
   
   procedure Boxes_Build_Label
     (This : access Builder_Record;
      Node : Node_Id);
           
   procedure Boxes_Build_Procedure_Call_Statement
     (This : access Builder_Record;
      Node : Node_Id);
      
   procedure Boxes_Build_Null_Statement
     (This : access Builder_Record;
      Node : Node_Id);
      
end  Reflex.Boxes.Ch5;
