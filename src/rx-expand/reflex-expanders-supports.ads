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

with Types; use Types;

package Reflex.Expanders.Supports is

   procedure Expand_Left_Opnd
     (This : access Reflex_Expander_Record;
      N    : Node_Id);
   --  Print left operand of operator, parenthesizing if necessary. Note that
   --  we fully parenthesize operator trees in the C output.

   procedure Expand_Node_List
     (This      : access Reflex_Expander_Record;
      List      : List_Id;
      New_Lines : Boolean := False);
   --  Prints the nodes in a list with no separating characters. This is used
   --  in the case of lists of items which are printed on separate lines using
   --  the current indentation amount. New_Lines controls the generation of
   --  New_Line calls. If False, no New_Line calls are Expandd. If True,
   --  then New_Line calls are Expandd as needed to ensure that each list
   --  item starts at the beginning of a line.

   procedure Expand_Opt_Node
     (This : access Reflex_Expander_Record;
      Node : Node_Id);
   --  Same as normal Expand_Node procedure, except that one leading blank is
   --  output before the node if it is non-empty.

   procedure Expand_Opt_Node_List
     (This : access Reflex_Expander_Record;
      List : List_Id);
   --  Like Expand_Node_List, but prints nothing if List = No_List

   procedure Expand_Right_Opnd
     (This : access Reflex_Expander_Record;
      N    : Node_Id);
   --  Print right operand of operator, parenthesizing if necessary. Note that
   --  we fully parenthesize operator trees in the C output.

end Reflex.Expanders.Supports;
