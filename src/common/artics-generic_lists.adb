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
   
   function New_List return List is
      L : List := new List_Record;
   begin
      L.Count := 0;
      L.First := Null_Node;
      L.Last  := Null_Node;
      
      return L;
   end New_List;

   -----------------
   -- Delete_List --
   -----------------
   
   procedure Delete_List (L : in out List) is
      N : List_Node;
   begin
      pragma Assert (L /= Null_List);

      N := First (L);
      while N /= Null_Node loop
         Next (N);
         Remove (N);
      end loop;
      
      Free_List (L);
   end Delete_List;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (L    : List;
      Item : Data_Type)
   is
      New_Node : List_Node;
   begin
      pragma Assert (L /= Null_List);
      
      New_Node :=  new List_Node_Record'
        (List_Link => L,
         Element   => Item,
         Prev      => Null_Node,
         Next      => Null_Node);
      
      if L.First = Null_Node then
	 L.First := New_Node;
	 L.Last  := New_Node;
	 
      else
	 New_Node.Next := L.First;
	 L.First.Prev  := New_Node;
	 L.First       := New_Node;
      end if;
            
      L.Count := L.Count + 1;
   end Prepend;

   -------------------
   -- Insert_Before --
   -------------------
   
   procedure Insert_Before
     (Node : List_Node;
      Item : Data_Type)
   is
      New_Node : List_Node;
      L        : List;
   begin
      pragma Assert (Node /= Null_Node);
      L := Node.List_Link;
      
      pragma Assert (L /= Null_List);

      New_Node := new List_Node_Record'
        (List_Link => L,
         Element   => Item,
	 Prev      => Null_Node,
         Next      => Null_Node);
      
      New_Node.Next := Node;
      New_Node.Prev := Node.Prev;
      
      Node.Prev := New_Node;
      
      if L.First = Node then
         L.First := New_Node;
      else
         New_Node.Prev.Next := New_Node;
      end if;
      
      L.Count := L.Count + 1;
   end Insert_Before;

   ------------------
   -- Insert_After --
   ------------------
   
   procedure Insert_After
     (Node : List_Node;
      Item : Data_Type)
   is
      New_Node : List_Node;
      L        : List;
   begin
      pragma Assert (Node /= Null_Node);
      L := Node.List_Link;
      
      pragma Assert (L /= Null_List);

      New_Node := new List_Node_Record'
        (List_Link => L,
         Element   => Item,
	 Prev      => Null_Node,
         Next      => Null_Node);
      
      New_Node.Prev := Node;
      New_Node.Next := Node.Next;
      
      Node.Next := New_Node;
      
      if L.Last = Node then
         L.Last := New_Node;
      else
         New_Node.Next.Prev := New_Node;
      end if;
      
      L.Count := L.Count + 1;
   end Insert_After;

   ------------
   -- Append --
   ------------

   procedure Append
     (L    : List;
      Item : Data_Type) is
      
      New_Node : List_Node;
   begin
      pragma Assert (L /= Null_List);

      New_Node := new List_Node_Record'
        (List_Link => L,
         Element   => Item,
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
      pragma Assert (L /= Null_List);
      return L.First = Null_Node or else L.Count = 0;
   end Is_Empty;

   ------------
   -- Length --
   ------------

   function Length (L : List) return Natural is
   begin
      pragma Assert (L /= Null_List);
      return L.Count;
   end Length;

   ------------
   -- Concat --
   ------------

   procedure Concat
     (L1 : List;
      L2 : List)
   is
      F1 : List_Node := L2.First;
      F2 : List_Node := L2.Last;
      N  : List_Node;
   begin
      if Is_Empty (L2) then
         return;
      end if;

      N := First (L2);
      while N /= Null_Node loop
         N.List_Link := L1;
         Next (N);
      end loop;
      
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

   procedure Remove (Node : in out List_Node) is
      L : List; 
   begin
      pragma Assert (Node /= Null_Node);
      L := Node.List_Link;
      
      pragma Assert (L /= Null_List);

      L := Node.List_Link;
      
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
   
   --------------------
   -- Remove_Element --
   --------------------
   
   procedure Remove_Element 
     (L    : List;
      Elmt : Data_Type) is
      
      N : List_Node;
   begin
      N := First (L);
      while N /= Null_Node loop
	 if Data (N) = Elmt then
	    Remove (N);
	 end if;
	 Next (N);
      end loop;
   end Remove_Element;
   
   -----------
   -- First --
   -----------

   function First (L : List) return List_Node is
   begin
      pragma Assert (L /= Null_List);
      return L.First;
   end First;

   ----------
   -- Last --
   ----------

   function Last (L : List) return List_Node is
   begin
      pragma Assert (L /= Null_List);
      return L.Last;
   end Last;

   ----------
   -- Prev --
   ----------

   function Previous (Node : List_Node) return List_Node is
   begin
      pragma Assert (Node /= Null_Node);
      return Node.Prev;
   end Previous;

   ----------
   -- Prev --
   ----------

   procedure Previous (Iterate : in out List_Node) is
   begin
      pragma Assert (Iterate /= Null_Node);
      Iterate := Iterate.Prev;
   end Previous;

   ----------
   -- Next --
   ----------

   procedure Next (Iterate : in out List_Node) is
   begin
      pragma Assert (Iterate /= Null_Node);
      Iterate := Iterate.Next;
   end Next;

   ----------
   -- Next --
   ----------

   function Next (Node : List_Node) return List_Node is
   begin
      pragma Assert (Node /= Null_Node);
      return Node.Next;
   end Next;

   ----------
   -- Head --
   ----------

   function Head (L : List) return Data_Type is
   begin
      pragma Assert (L /= Null_List);
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
      pragma Assert (Node /= Null_Node);
      return Node.Element;
   end Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Node : List_Node;
      D    : Data_Type) is
   begin
      pragma Assert (Node /= Null_Node);
      Node.Element := D;
   end Set_Data;
   
   --------------
   -- Get_Size --
   --------------
   
   function Get_Size (L : List) return Natural is
   begin
      return L.Count;
   end Get_Size;
   

end Artics.Generic_Lists;
