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
-- Reflex is originally developed by Artics
------------------------------------------------------------------------------

--  A generic double linked list

with Unchecked_Deallocation;

generic
   type Data_Type is private;

package Artics.Generic_Lists is

   type List is private;
   type List_Node is private;

   Null_List : constant List;
   Null_Node : constant List_Node;

   List_Empty : exception;

   function New_List return List;
   
   procedure Delete_List (L : in out List);
   
   procedure Prepend
     (L    : List;
      Item : Data_Type);
   --  Add an item at the beginning of a list. The cost is O(1)

  procedure Insert_Before
     (Node : List_Node;
      Item : Data_Type);
  -- Insert Item before node
  
  procedure Insert_After
     (Node : List_Node;
      Item : Data_Type);
  -- Insert Item before node
  
   procedure Append
     (L    : List;
      Item : Data_Type);
   --  Add an item at the end of a list. The cost is O(1)

   function Is_Empty (L : List) return Boolean;
   --  True if L does not contain any element.

   function Length (L : List) return Natural;
   --  Return the number of elements in L. Cost is O(n)

   procedure Concat
     (L1 : List;
      L2 : List);
   --  Append L2 at the end of L1. Cost is O(1).
   --  Note that no deep copy of L2 is done, which means that L1 and L2
   --  will share the same nodes.

   procedure Remove (Node : in out List_Node);
   -- Remove node 
   
   procedure Remove_Element 
     (L    : List;
      Elmt : Data_Type);
   -- Remove the first node of value Elmt from list L.
   
   function First (L : List) return List_Node;
   --  Return the first node contained in L

   function Last (L : List) return List_Node;
   --  Return the last node contained in L

   function Next (Node : List_Node) return List_Node;
   pragma Inline (Next);
   procedure Next (Iterate : in out List_Node);
   pragma Inline (Next);
   --  Return the node following Node. The cost is O(1)
    
   function Previous (Node : List_Node) return List_Node;
   pragma Inline (Previous);
   procedure Previous (Iterate : in out List_Node);
   pragma Inline (Previous);
   --  Return the node following Node. The cost is O(1)
    
   function Head (L : List) return Data_Type;
   --  Return the first data associated with L.
   --  Raise List_Empty if L is null.

   function Data (Node : List_Node) return Data_Type;
   --  Return the data associated with L.
   --  Raise List_Empty if L is null.

   procedure Set_Data
     (Node : List_Node;
      D    : Data_Type);
   --  Free the data associated with Node and replace it by D
   
   function Get_Size (L : List) return Natural;
   -- Size of the list.
   
private

   type List_Node_Record;
   type List_Node is access List_Node_Record;

   Null_Node : constant List_Node := null;
   type List_Record is record
      Count : Natural;
      First : List_Node;
      Last  : List_Node;
   end record;
   type List is access List_Record;
   
   Null_List_Record : constant List_Record := List_Record'(0, null, null);
   Null_List : constant List := null;

   type List_Node_Record is record
      List_Link : List;
      Element   : Data_Type;
      Prev      : List_Node;
      Next      : List_Node;
   end record;

   procedure Free_Node is new 
     Unchecked_Deallocation (List_Node_Record, List_Node);
   procedure Free_List is new 
     Unchecked_Deallocation (List_Record, List);

   pragma Inline (First);
   pragma Inline (Last);
   pragma Inline (Prepend);
   pragma Inline (Insert_Before);
   pragma Inline (Insert_After);
   pragma Inline (Is_Empty);
   pragma Inline (Data);
   pragma Inline (Head);

end Artics.Generic_Lists;
