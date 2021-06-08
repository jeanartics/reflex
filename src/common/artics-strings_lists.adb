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

package body Artics.Strings_Lists is
   
   ---------------------
   -- New_List --
   ---------------------
   
   function New_List return List is
      L : List;
   begin
      L.Lst := Str_Lists.New_List;
      return L;
   end New_List;
   
   ------------------------
   -- Delete_List --
   ------------------------
   
   procedure Delete_List (L : in out List) is
   begin
      Str_Lists.Delete_List (L.Lst);
   end Delete_List;
   
   -------------
   -- Prepend --
   -------------
   
   procedure Prepend
     (L    : List;
      Item : String) is
   begin
      Str_Lists.Prepend (L.Lst, String_Find (Item));
   end Prepend;
   
   -------------
   -- Prepend --
   -------------
   
   procedure Prepend
     (L    : List;
      Item : Name_Id) is
   begin
      Str_Lists.Prepend (L.Lst, Item);
   end Prepend;
   
   -------------------
   -- Insert_Before --
   -------------------
   
   procedure Insert_Before
     (Node : List_Node;
      Item : String) is
   begin
      Str_Lists.Insert_Before (Str_Lists.List_Node (Node), String_Find (Item));
   end Insert_Before;
   
   -------------------
   -- Insert_Before --
   -------------------
   
   procedure Insert_Before
     (Node : List_Node;
      Item : Name_Id) is
   begin
      Str_Lists.Insert_Before (Str_Lists.List_Node (Node), Item);
   end Insert_Before;
   
   ------------------
   -- Insert_After --
   ------------------
   
   procedure Insert_After
     (Node : List_Node;
      Item : Name_Id) is
   begin
      Str_Lists.Insert_After (Str_Lists.List_Node (Node), Item);
   end Insert_After;
   
   ------------------
   -- Insert_After --
   ------------------
   
  procedure Insert_After
     (Node : List_Node;
      Item : String) is
  begin
     Str_Lists.Insert_After (Str_Lists.List_Node (Node), String_Find (Item));
  end Insert_After;
  
  ------------
  -- Append --
  ------------
  
  procedure Append
    (L    : List;
     Item : String) is
  begin
     Str_Lists.Append (L.Lst, String_Find (Item));
  end Append;
  
  ------------
  -- Append --
  ------------
  
  procedure Append
     (L    : List;
      Item : Name_Id) is
  begin
     Str_Lists.Append (L.Lst, Item);
  end Append;
  
  --------------
  -- Is_Empty --
  --------------
  
  function Is_Empty (L : List) return Boolean is
  begin
     return Str_Lists.Is_Empty (L.Lst);
  end Is_Empty;
   
  ------------
  -- Length --
  ------------
  
  function Length (L : List) return Natural is
  begin
     return Str_Lists.Length (L.Lst);
  end Length;
  
  ------------
  -- Concat --
  ------------
  
  procedure Concat
    (L1 : List;
     L2 : List) is
  begin
     Str_Lists.Concat (L1.Lst, L2.Lst);
  end Concat;
  
  ------------
  -- Remove --
  ------------
  
  procedure Remove (Node : in out List_Node) is
  begin
     Str_Lists.Remove (Str_Lists.List_Node (Node));
  end Remove;
  
  -----------
  -- First --
  -----------
  
  function First (L : List) return List_Node is
  begin
     return List_Node (Str_Lists.First (L.Lst));
  end First;
  
  ----------
  -- last --
  ----------
  
  function Last (L : List) return List_Node is
  begin
     return List_Node (Str_Lists.Last (L.Lst));
  end Last;
  
  ----------
  -- Next --
  ----------
  
  function Next (Node : List_Node) return List_Node is
  begin
     return List_Node (Str_Lists.Next (Str_Lists.List_Node (Node)));
  end Next;
  
  ----------
  -- Next --
  ----------
  
  procedure Next (Iterate : in out List_Node) is
  begin
     Str_Lists.Next (Str_Lists.List_Node (Iterate));
  end Next;
  
  --------------
  -- Previous --
  --------------
  
  function Previous (Node : List_Node) return List_Node is
  begin
     return List_Node (Str_Lists.Previous (Str_Lists.List_Node (Node)));
  end Previous;
  
  --------------
  -- Previous --
  --------------
  
  procedure Previous (Iterate : in out List_Node) is
  begin
     Str_Lists.Previous(Str_Lists.List_Node (Iterate));
  end Previous;
  
  ----------
  -- Head --
  ----------
  
  function Head (L : List) return String is
     Name : Name_Id := Str_Lists.Head (L.Lst);
  begin
     if Name /= No_Name then
	return Get_String (Name);
     else
	return "";
     end if;
  end Head;
  
  ----------
  -- Head --
  ----------
  
  function Head (L : List) return Name_Id is
  begin
     return Str_Lists.Head (L.Lst);
  end Head;
  
  ----------
  -- Data --
  ----------
  
  function Data (Node : List_Node) return String is
     Name : Name_Id := Str_Lists.Data (Str_Lists.List_Node (Node));
  begin
     if Name /= No_Name then
	return Get_String (Name);
     else
	return "";
     end if;
  end Data;
   
  ----------
  -- Data --
  ----------
  
  function Data (Node : List_Node) return Name_Id is
  begin
     return Str_Lists.Data (Str_Lists.List_Node (Node));
  end Data;
  
  --------------
  -- Set_Data --
  --------------
  
  procedure Set_Data
    (Node : List_Node;
     D    : String) is
  begin
     Set_Data (Node, String_Find (D));
  end Set_Data;
  
  --------------
  -- Set_Data --
  --------------
  
  procedure Set_Data
    (Node : List_Node;
     D    : Name_Id) is
  begin
     Str_Lists.Set_Data (Str_Lists.List_Node (Node), D);
  end Set_Data;
  
end Artics.Strings_Lists;

