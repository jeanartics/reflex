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

package body Artics.Lists_Helpers is
   
   -----------------------
   -- Forward_Cursor_At --
   -----------------------
   
   function Forward_Cursor_At 
     (L     : Lists.List;
      Index : Integer) return Lists.Cursor is
      
      use Lists;
      
      Cur : Lists.Cursor :=  Lists.No_Element;
   begin
      if Index < 1 
        or else Lists.Is_Empty (L) 
        or else Integer (Lists.Length (L)) < Index 
      then
         return Lists.No_Element;
	 
         -- Here we know that index is a valid position in the list, no need to
         -- test Has_Element, as Index is <= length of the list
	 
      else
         Cur := Lists.First (L);
         for I in 2..Index loop
            exit when Cur = Lists.No_Element;
            Lists.Next (Cur);
         end loop;
      end if;
      
      return Cur;
   end Forward_Cursor_At;
   
   ------------
   -- Get_At --
   ------------
   
   function Get_At 
     (L     : Lists.List;
      Index : Integer) return Element_Type is
      
      use Lists;
      
      Cur : Lists.Cursor;
   begin
      Cur := Forward_Cursor_At (L, Index);
      
      if Lists.Has_Element (Cur) then
         return Lists.Element (Cur);
      else
         return No_Element;
      end if;
   end Get_At;
   
   ---------------
   -- Insert_At --
   ---------------
   
   procedure Insert_At
     (L     : in out Lists.List;
      Index : Integer;
      Elmt  : Element_Type) is
      
      use Lists;
      
      Cur : Lists.Cursor;
   begin
      -- If Index is > than the length of the list, the cell is appended at the
      -- end of the list, no exception is raised if the index is > length + 1
      
      if Index < 1 then
         Lists.Prepend (L, Elmt);
	 
      else	 
         Cur := Forward_Cursor_At (L, Index);
	 
         if Lists.Has_Element (Cur) then
            Lists.Insert (L, Cur, Elmt);
         else
            Lists.Append (L, Elmt);
         end if;
      end if;
   end Insert_At;
   
   -------------------
   -- Insert_Before --
   -------------------
   
   procedure Insert_Before
     (L        : in out Lists.List;
      Elmt     : Element_Type;
      New_Elmt : Element_Type)
   is
      Pos : Integer;
   begin
      Pos := Get_Position (L, Elmt);
      Insert_At (L, Pos, New_Elmt);
   end Insert_Before;
   
   ---------------
   -- Remove_At --
   ---------------
   
   procedure Remove_At
     (L     : in out Lists.List;
      Index : Integer) is
      
      Cur : Lists.Cursor;
   begin
      -- If Index is > than the length of the list, the cell is appended at the
      -- end of the list, no exception is raised if the index is > length + 1
      
      if Index > 0 then
	 
         Cur := Forward_Cursor_At (L, Index);
	 
         if Lists.Has_Element (Cur) then
            Lists.Delete (L, Cur);
         end if;
      end if;
   end Remove_At;
   
   ---------------
   -- Repace_At --
   ---------------
   
   procedure Replace_At
     (L     : in out Lists.List;
      Index : Integer;
      Elmt  : Element_Type) is
   begin
      Insert_At (L, Index, Elmt);
      Remove_At (L, Index + 1);
   end Replace_At;
   
   --------------------
   -- Remove_Element --
   --------------------
   
   procedure Remove_Element
     (L    : in out Lists.List;
      Elmt : Element_Type) is
      
      Cur : Lists.Cursor;
   begin
      if L /= Lists.Empty_List then
         Cur := Lists.Find (L, Elmt);
         if Lists.Has_Element (Cur) then
            Lists.Delete (L, Cur);
         end if;
      end if;
   end Remove_Element;
   
   ---------------
   -- Get_First --
   ---------------
   
   function Get_First
     (L : Lists.List) return Element_Type is
   begin
      if Lists.Is_Empty (L) then
         return No_Element;
      else
         return Lists.First_Element (L);
      end if;
   end Get_First;
   
   ----------------
   -- Get_Second --
   ----------------
   
   function Get_Second
     (L : Lists.List) return Element_Type is
      
      use Lists;
      
      Cur : Lists.Cursor;
   begin
      Cur := Forward_Cursor_At (L, 2);
      
      if Lists.Has_Element (Cur) then
         return Lists.Element (Cur);
      else
         return No_Element;
      end if;
   end Get_Second;
   
   --------------
   -- Get_Last --
   --------------
   
   function Get_Last (L : Lists.List) return Element_Type is
   begin
      if Lists.Is_Empty (L) then
         return No_Element;
      else
         return Lists.Last_Element (L);
      end if;
   end Get_Last;
   
   ------------------------
   -- Get_Last_Minus_One --
   ------------------------
   
   function Get_Last_Minus_One (L : Lists.List) return Element_Type is
      
      use Lists;
      
      Cur : Lists.Cursor;
   begin
      Cur := Lists.Last (L);
      if Lists.Has_Element (Cur) then
         Cur := Lists.Previous (Cur);
         if Lists.Has_Element (Cur) then
            return Lists.Element (Cur);
         end if;
      end if;
      
      return No_Element;
   end Get_Last_Minus_One;
   
   ------------------
   -- Get_Position --
   ------------------
   
   function Get_Position
     (L    : Lists.List;
      Elmt : Element_Type) return Integer is

      use Lists;
      Cur   : Lists.Cursor;
      Count : Integer := 0;
   begin
      if L /= Lists.Empty_List then
         Cur := Lists.First (L);
         while Lists.Has_Element (Cur) loop
            Count := Count + 1;
            exit when Lists.Element (Cur) = Elmt;
            Next (Cur);
         end loop;
      end if;
      
      return Count;
   end Get_Position;
   
   -----------------
   -- List_Length --
   -----------------
   
   function List_Length (L : Lists.List) return Integer is
      use Lists;
   begin
      if L /= Lists.Empty_List then
         return Integer (Lists.Length (L));
      else
         return 0;
      end if;
   end List_Length;
   -----------------
   -- Append_List --
   -----------------
   
   procedure Append_List 
     (To   : in out Lists.List;
      From : Lists.List) is
   begin
      for E of From loop
         if not Lists.Contains (To, E) then
            Lists.Append (To, E);
         end if;
      end loop;
   end Append_List;
   
   ------------------------
   -- Add_If_Not_Present --
   ------------------------
   
   procedure Add_If_Not_Present
     (L    : Lists.List;
      Elmt : Element_Type) is
   begin
      --        if not Lists.Has_Elementnt (L, Elmt) then
      --  	 Lists.Append (L, Elmt);
      --        end if;
      null;
   end Add_If_Not_Present;
   
end Artics.Lists_Helpers;
