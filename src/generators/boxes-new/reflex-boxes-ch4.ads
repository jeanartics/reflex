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

package Reflex.Boxes.Ch4 is
   
   procedure Boxes_Build_Op_And
     (This : access Builder_Record;
      Node : Node_Id);
      
   procedure Boxes_Build_Op_Not
     (This : access Builder_Record;
      Node : Node_Id);
               
   procedure Boxes_Build_Op_Or
     (This : access Builder_Record;
      Node : Node_Id);
                  
   procedure Boxes_Build_Op_Eq 
     (This : access Builder_Record;
      Node : Node_Id);
   
   procedure Boxes_Build_Op_Ne
     (This : access Builder_Record;
      Node : Node_Id);
   
   procedure Boxes_Build_Op_Ge 
     (This : access Builder_Record;
      Node : Node_Id);
            
   procedure Boxes_Build_Op_Gt 
     (This : access Builder_Record;
      Node : Node_Id);
            
   procedure Boxes_Build_Op_Le 
     (This : access Builder_Record;
      Node : Node_Id);
   
   procedure Boxes_Build_Op_Lt 
     (This : access Builder_Record;
      Node : Node_Id);
   
   procedure Boxes_Build_Op_Shift_Right
     (This : access Builder_Record;
      Node : Node_Id;
      Name : Node_Id);
   
   procedure Boxes_Build_Op_Shift_Left
     (This : access Builder_Record;
      Node : Node_Id;
      Name : Node_Id);
               
   procedure Boxes_Build_Op_Plus
     (This : access Builder_Record;
      Node : Node_Id);
   
   procedure Boxes_Build_Op_Minus 
     (This : access Builder_Record;
      Node : Node_Id);
   
   procedure Boxes_Build_Op_Abs
     (This : access Builder_Record;
      Node : Node_Id;
      Name : Node_Id);
   
   procedure Boxes_Build_Op_Mod 
     (This : access Builder_Record;
      Node : Node_Id;
      Name : Node_Id);
   
   procedure Boxes_Build_Op_Expon 
     (This : access Builder_Record;
      Node : Node_Id;
      Name : Node_Id);
   
   procedure Boxes_Build_Op_Rem 
     (This : access Builder_Record;
      Node : Node_Id);
   
end Reflex.Boxes.Ch4;
