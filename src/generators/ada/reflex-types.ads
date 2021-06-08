------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
--                                                                          --
-- Reflex is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public Licensea as published  by the Free Soft- --
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

package Reflex.Types is
   
   Vertex_Low_Bound : constant := 0;
   --  The tree Id values start at zero, because we use zero for Empty (to
   --  allow a zero test for Empty). Actual tree node subscripts start at 0
   --  since Empty is a legitimate node value.

   Vertex_High_Bound : constant := 099_999_999;
   --  Maximum number of nodes that can be allocated is 100 million, which
   --  is in practice infinite, and there is no need to check the range.
   
   type Vertex_Id is range Vertex_Low_Bound .. Vertex_High_Bound;
   
   First_Vertex_Id  : constant Vertex_Id := Vertex_Low_Bound;
   --  Subscript of first allocated node. Note that Empty and Error are both
   --  allocated nodes, whose Nkind fields can be accessed without error.
   
   No_Vertex : constant Vertex_Id := Vertex_Low_Bound;
   
   Vertex_List_Low_Bound : constant := -100_000_000;
   --  The List_Id values are subscripts into an array of list headers which
   --  has List_Low_Bound as its lower bound. This value is chosen so that all
   --  List_Id values are negative, and the value zero is in the range of both
   --  List_Id and Node_Id values (see further description below).

   Vertex_List_High_Bound : constant := 0;
   --  Maximum List_Id subscript value. This allows up to 100 million list
   --  Id values, which is in practice infinite, and there is no need to
   --  check the range. The range overlaps the node range by one element
   --  (with value zero), which is used both for the Empty node, and for
   --  indicating no list. The fact that the same value is used is convenient
   --  because it means that the default value of Empty applies to both nodes
   --  and lists, and also is more efficient to test for.
   
   type Vertex_List_Id is range Vertex_List_Low_Bound .. Vertex_List_High_Bound;
   --  Type used to identify a node list

   No_Vertex_List : constant Vertex_List_Id := Vertex_List_High_Bound;
   --  Used to indicate absence of a list. Note that the value is zero, which
   --  is the same as Empty, which is helpful in intializing nodes where a
   --  value of zero can represent either an empty node or an empty list.

   Vertex_Error_List : constant Vertex_List_Id := Vertex_List_Low_Bound;
   --  Used to indicate that there was an error in the source program in a
   --  context which would normally require a list. This node appears to be
   --  an empty list to the list operations (a null list is actually allocated
   --  which has this Id value).
   
   First_Vertex_List_Id : constant Vertex_List_Id := Vertex_Error_List;
   --  Subscript of first allocated list header
   
   
end Reflex.Types;
