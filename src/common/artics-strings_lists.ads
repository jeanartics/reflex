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

with Artics.Types; use Artics.Types;
with Artics.Generic_Lists;
with Artics.Namet; use Artics.Namet;

package Artics.Strings_Lists is
   
   type List is private;
   type List_Node is private;

   Null_List : constant List;
   Null_Node : constant List_Node;

   List_Empty : exception;

   function New_List return List;
   
   procedure Delete_List (L : in out List);
   
   procedure Prepend
     (L    : List;
      Item : String);
   procedure Prepend
     (L    : List;
      Item : Name_Id);
   --  Add an item at the beginning of a list. The cost is O(1)

  procedure Insert_Before
     (Node : List_Node;
      Item : String);
  procedure Insert_Before
     (Node : List_Node;
      Item : Name_Id);
  -- Insert Item before node
  
  procedure Insert_After
     (Node : List_Node;
      Item : Name_Id);
  procedure Insert_After
     (Node : List_Node;
      Item : String);
  -- Insert Item before node
  
   procedure Append
     (L    : List;
      Item : String);
   procedure Append
     (L    : List;
      Item : Name_Id);
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
    
   function Head (L : List) return String;
   function Head (L : List) return Name_Id;
   --  Return the first data associated with L.
   --  Raise List_Empty if L is null.

   function Data (Node : List_Node) return String;
   function Data (Node : List_Node) return Name_Id;
   --  Return the data associated with L.
   --  Raise List_Empty if L is null.
  
   procedure Set_Data
     (Node : List_Node;
      D    : String);
   procedure Set_Data
     (Node : List_Node;
      D    : Name_Id);
   --  Free the data associated with Node and replace it by D
   
private   
   
   package Str_Lists is new Artics.Generic_Lists (Name_Id);
   
   type List is tagged record
      Lst : Str_Lists.List;
   end record;
   
   type List_Node is new Str_Lists.List_Node;
   
   Null_List : constant List := (Lst => Str_Lists.Null_List);
   
   Null_Node : constant List_Node := List_Node (Str_Lists.Null_Node);

   
end Artics.Strings_Lists;

