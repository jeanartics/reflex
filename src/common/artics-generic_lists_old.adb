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

package body Artics.Generic_Lists is

   --------------
   -- New_List --
   --------------
   
   function New_List return List_Access is
   begin
      return new List'(Null_List);
   end New_List;
   
   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (L    : in out List;
      Item : Data_Type)
   is
      N : List_Node;
   begin
      N :=  new List_Node_Record'
        (Element   => Item,
	 Prev      => Null_Node,
         Next      => Null_Node);
      
      if L.Last = Null_Node then
	 L.First := N;
      else
	 L.Last.Next := N;
      end if;
      
      L.Last := N;
      
      N.Next := Null_Node;
      N.Prev := L.last;
      
      L.Count := L.Count + 1;
   end Prepend;

   -------------------
   -- Insert_Before --
   -------------------
   
   procedure Insert_Before
     (L    : in out List;
      Node : List_Node;
      Item : Data_Type)
   is
      New_Node : List_Node;
   begin
      if Node = Null_Node then
	 Prepend
	   (L    => L,
	    Item => Item);
	 return;
      end if;
      
      New_Node := new List_Node_Record'
        (Element   => Item,
	 Prev      => Null_Node,
         Next      => Null_Node);
      
      New_Node.Next := Node;
      New_Node.Prev := Node.Prev;
      
      Node.Prev := New_Node;
      
      if L.First = Node then
	 L.First := New_Node;
      end if;
      
      L.Count := L.Count + 1;
   end Insert_Before;

   ------------------
   -- Insert_After --
   ------------------
   
   procedure Insert_After
     (L    : in out List;
      Node : List_Node;
      Item : Data_Type)
   is
      New_Node : List_Node;
   begin
      if Node = Null_Node then
	 Append
	   (L    => L,
	    Item => Item);
	 return;
      end if;
      
      New_Node := new List_Node_Record'
        (Element   => Item,
	 Prev      => Null_Node,
         Next      => Null_Node);
      
      New_Node.Prev := Node;
      New_Node.Next := Node.Next;
      
      Node.Next := New_Node;
      
      if L.Last = Node then
	 L.Last := New_Node;
      end if;
      
      L.Count := L.Count + 1;
   end Insert_After;

   ------------
   -- Append --
   ------------

   procedure Append
     (L    : in out List;
      Item : Data_Type) is
      
      New_Node : List_Node;
   begin
      New_Node := new List_Node_Record'
        (Element   => Item,
	 Prev      => Null_Node,
         Next      => Null_Node);
      
      if L.First = Null_Node then
	 L.First := New_Node;
	 L.Last  := New_Node;
	 
      else
	 New_Node.Prev := L.Last;
	 L.Last.Next   := New_Node;
	 L.Last        := New_Node;
      end if;
      
      L.Count := L.Count + 1;
   end Append;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (L : List) return Boolean is
   begin
      return L.First = Null_Node or else L.Count = 0;
   end Is_Empty;

   ------------
   -- Length --
   ------------

   function Length (L : List) return Natural is
   begin
      return L.Count;
   end Length;

   ------------
   -- Concat --
   ------------

   procedure Concat
     (L1 : in out List;
      L2 : List)
   is
      F1 : List_Node := L2.First;
      F2 : List_Node := L2.Last;
   begin
      if Is_Empty (L2) then
         return;
      end if;
      
      if Is_Empty (L1) then
         L1.First := L2.First;
         L1.Last  := L2.Last;
      else
         L1.Last.Next  := L2.First;
         L2.First.Prev := L1.Last;
         L1.Count      := L1.Count + L2.Count;
         L1.Last       := L2.Last;
      end if;
   end Concat;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (L    : in out List; 
      Node : in out List_Node) is
   begin
      if Node = Null_Node then
         raise List_Empty;
      end if;
      
      if L.Last = Node 
        and then L.First = Node
      then
	 L.First := Null_Node;
	 L.Last  := Null_Node;
	 
      elsif L.First = Node then
         L.First := Node.Next;
         Node.Next.Prev := Null_Node;
	 
      elsif L.Last = Node then
	 L.Last := Node.Prev;
         Node.Prev.Next := Null_Node;
	 
      else
	 Node.Prev.Next := Node.Next;
	 Node.Next.Prev := Node.Prev;
      end if;
      
      Free_Node (Node);
   end Remove;

   -----------
   -- First --
   -----------

   function First (L : List) return List_Node is
   begin
      return L.First;
   end First;

   ----------
   -- Last --
   ----------

   function Last (L : List) return List_Node is
   begin
      return L.Last;
   end Last;

   ----------
   -- Prev --
   ----------

   function Previous (Node : List_Node) return List_Node is
   begin
      if Node = Null_Node then
         raise List_Empty;
      else
         return Node.Prev;
      end if;
   end Previous;

   ----------
   -- Prev --
   ----------

   procedure Previous (Iterate : in out List_Node) is
   begin
      if Iterate = Null_Node then
         raise List_Empty;
      end if;
      
      Iterate := Iterate.Prev;
   end Previous;

   ----------
   -- Next --
   ----------

   procedure Next (Iterate : in out List_Node) is
   begin
      if Iterate = Null_Node then
         raise List_Empty;
      else
         Iterate := Iterate.Next;
      end if;
   end Next;

   ----------
   -- Next --
   ----------

   function Next (Node : List_Node) return List_Node is
   begin
      if Node = Null_Node then
         raise List_Empty;
      else
         return Node.Next;
      end if;
   end Next;

   ----------
   -- Head --
   ----------

   function Head (L : List) return Data_Type is
   begin
      if L.First = Null_Node then
         raise List_Empty;
      else
         return L.First.Element;
      end if;
   end Head;

   ----------
   -- Data --
   ----------

   function Data (Node : List_Node) return Data_Type is
   begin
      if Node = Null_Node then
         raise List_Empty;
      else
         return Node.Element;
      end if;
   end Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Node : in out List_Node;
      D    : Data_Type) is
   begin
      if Node = Null_Node then
         raise List_Empty;
      else
         Node.Element := D;
      end if;
   end Set_Data;

end Artics.Generic_Lists;
