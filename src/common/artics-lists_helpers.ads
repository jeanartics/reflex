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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;

generic
   type Element_Type is private;
   
   No_Element : Element_Type;
   
   with package Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type);
   
package Artics.Lists_Helpers is
   
   use Lists;
   
   function Forward_Cursor_At 
     (L     : Lists.List;
      Index : Integer) return Lists.Cursor;
   -- Put the cursor at the Index-nth position
   
   function Get_At 
     (L     : Lists.List;
      Index : Integer) return Element_Type;
   -- Returns the element at the given index.
   
   procedure Insert_At
     (L     : in out Lists.List;
      Index : Integer;
      Elmt  : Element_Type);
   -- Returns the element at the given index.
   
   procedure Insert_Before
     (L        : in out Lists.List;
      Elmt     : Element_Type;
      New_Elmt : Element_Type);
   -- Insert New_Element before Element
   
   procedure Remove_At
     (L     : in out Lists.List;
      Index : Integer);
   -- remove element at position Index in the list l
   
   procedure Replace_At
     (L     : in out Lists.List;
      Index : Integer;
      Elmt  : Element_Type);
   -- Replace the element at position Index in the ist by the new element Elmt
   
   procedure Remove_Element
     (L    : in out Lists.List;
      Elmt : Element_Type);
   -- Remove element Elmt in the list L
   
   function Get_First (L : Lists.List) return Element_Type;
   -- Return the first element of the list L.
   
   function Get_Second (L : Lists.List) return Element_Type;
   -- Return the second element in the list L.
   
   function Get_Last (L : Lists.List) return Element_Type;
   -- Return the last pont of the points lists.
   
   function Get_Last_Minus_One (L : Lists.List) return Element_Type;
   -- Return the element preceding the last element in the list.
   
   function Get_Position
     (L    : Lists.List;
      Elmt : Element_Type) return Integer;
   -- return the position of elment Elmt in th elist L
   
   function List_Length (L : Lists.List) return Integer;
   -- return the number of elements in the lsist L
   
   procedure Append_List 
     (To   : in out Lists.List;
      From : Lists.List);
   -- Append ellemnts of list From in the list To. If the element is alredy in
   -- list To, the element is not added again.
   
   procedure Add_If_Not_Present
     (L    : Lists.List;
      Elmt : Element_Type);
     
end Artics.Lists_Helpers;
