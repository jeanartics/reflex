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

with Artics.Iterators;
with Artics.Cells;

generic
   type Item is private;
   No_Item : Item;
package Artics.Containers.Lists is

   pragma Elaborate_Body;

   -- This package defines a common protocol for the lists (single and double
   -- linked list). The concrete implementation adds specific methods for the
   -- type of list (Reverse_Iterator for double linked lists for example).

   -- The choice to provide this package is to enforce a common interface for
   -- all type of lists, and therefore, separate the implementation of the
   -- abstraction. We have pay attention to mimimize the overhead at run-time,
   -- all the method's resolutions are made at the compile Time.

   List_Error : exception;

   type List_Record is new Container_Record with private;
   type List_Id is private;

   No_List : constant List_Id;

   function New_List (Name : String := "") return List_Id;
   -- Create a new empty List and named it Name.

   function New_List
     (Elmt : Item;
      Name : string := "") return List_Id;
   -- Create a new list with the first Item set to Elmt.

   function New_List_Copy
     (List : List_Id;
      Name : String := "") return List_Id;
   -- Create a new list clone of List. The cells are duplicated, so don't
   -- shared by the lists.

   procedure Copy_List_To
     (List : List_Id;
      To   : List_Id);
   -- Copy the list List to the List To. The List To must have beeen created
   -- If the list To is not empty, then To is reset (cells are deallocated),
   -- and then the copy is done.

   function First_Item (List : List_Id) return Item;
   -- Returns the first Item of the list.

   function Last_Item (List : List_Id) return Item;
   -- Returns the last Item of the list.

   function Get_Item
     (List      : List_Id;
      Position  : Positive) return Item;
   -- Returns Item at Position.

   function Length (List : List_Id) return Natural;
   -- Return the number of items in the list.

   function Is_Empty_List (List : List_Id) return Boolean;
   -- Return True if and only if there are no items in the list.

   procedure Append 
     (Elmt : Item;
      To   : List_Id);
   -- Add the Item Elmt at the end of the list.

   procedure Prepend
     (Elmt : in Item;
      To   : in List_Id);
   -- Add the Item Elmt at the beginning of the list.

   procedure Append_List
     (List : in List_Id;
      To   : in out List_Id);
   -- Concat the list "List" to List "To". The two lists shared their cells.
   -- So if a call to Reset is performed on one of the two lists, the cells
   -- are deallocated and therefore, the two lists are no more valid. We made
   -- this choice for performance reasons. Another choice should be to manage
   -- the number of reference to the cell, and when a cell is no more referen-
   -- ced, to deallocate the cell. But this mechanism enforces to walk the list
   -- "To" to mark the cells, and this is run-time overhead. Nevertheless, if
   -- a call to Reset is done in one of the list, all tries to acess an Item,
   -- raised the exception List_Item_acces_Error.

   procedure Prepend_List
     (List : in List_Id;
      To   : in out List_Id);
   -- Concat the list "List" to list "To". Element of lis "List" are prepend
   -- to element of list "To"

   function In_List
     (Elmt : in Item ;
      L    : in List_Id) return Boolean;
   -- returns TRUE if the element is already in L

   function In_List_Before
     (Crt_Elmt : in Item ;
      Elmt     : in Item ;
      L        : in List_Id) return Boolean;
   -- returns True if Crt_Elmt found before Elmt in L

   function In_List_Occurrences
     (Elmt : in Item ;
      L    : in List_Id) return Integer;
   -- returns the number of occurrences of Elmt in L

   -- Stack methods :

   procedure Push
     (Elmt : Item;
      To   : List_Id);
   -- Push item (insert as first).

   procedure Pop
     (List       : in out List_Id;
      First_Item : out Item);
   -- Return First item, and remove first item of the list.

   function Top (List : in List_Id) return Item;

   generic
      with function Less_Than
        (Elmt1 : in Item;
         Elmt2 : in Item) return Boolean;
   procedure Bubble_Sort_List (List : in out List_Id);

   generic
      with function Less_Than
        (Elmt_1 : in Item;
	 Elmt_2 : in Item) return Boolean;
   procedure Insert_Sorted
     (List      : in List_Id;
      Elmt      : in Item);
   -- Insert the item in the list in the right to keep it ordered

   generic
      with function Less_Than
        (Elmt_1 : in Item;
	 Elmt_2 : in Item) return Boolean;
      with function Equal
        (Elmt_1 : in Item;
	 Elmt_2 : in Item) return Boolean;
   procedure Insert_Sorted_No_Duplicate
     (List      : in List_Id;
      Elmt      : in Item);
   -- Insert the item in the list in the right to keep it ordered

   procedure Insert_After
     (List  : List_Id;
      After : Item;
      Elmt  : Item);
   -- Insert the Item Elmt in the List after the Item After.

   procedure Insert_Before
     (List   : List_Id;
      Before : Item;
      Elmt   : Item);
   -- Insert the Item ELmt in the List before the Item Before.

   procedure Insert_List_After
     (L1    : List_Id;
      L2    : List_Id;
      After : Item);
   -- Insert the list L2 in the list L1 after the Item After.

   procedure Insert_List_Before
     (L1     : List_Id;
      L2     : List_Id;
      Before : Item);
   -- Insert the list L2 in the list L1 before the Item Before.

   generic
      with procedure Free_Cell (C : in out Item);
   procedure Generic_Clear_List (List : in List_Id);

   procedure Clear_List (List : List_Id);
   -- Delete all the cells of the list.

   procedure Delete_List (List : in out List_Id);
   -- Delete all the cells of the list and the List.

   procedure Remove_First (L : in out List_Id);
   -- Remove the first item of the list.

   procedure Remove_Last (L : in out List_Id);
   -- Remove the last item of the list

   procedure Remove
     (L    : in out List_Id;
      From : in Positive);
   -- Remove all the items in the list starting at the given index,
   -- inclusive.

   procedure Remove
     (L     : in out List_Id;
      From  : in Positive;
      Count : in Positive);
   -- Remove all the items in the list starting at the given index,
   -- inclusive, for a total of count items.

   procedure Replace
     (L    : in out List_Id;
      Pos  : in Positive;
      Elt  : in Item);
   -- Replace the Pos th element in the list by the given value

   function Get_List_Name (List : List_Id) return String;
   -- Returns the name of the List.

   procedure Set_List_Name
     (List : List_Id;
      Name : String);
   -- Change the list name to Name.

   -- Lists Iterators --
   ---------------------

   ------------------------
   -- Concrete Iterators --
   ------------------------

   --   package Lists_Iterators is new Iterators
   --      (Item => Item,
   --       The_Container => List_Record);
   
   --    use Lists_Iterators;
   
   --    type List_Iterator is new Lists_Iterators.Iterator with record
   --       List : List_Id;
   --       Current_Cell : Cell_Id;
   --       -- The cell currently pointed by the iterator.
   --    end record;
   
   --   type List_Iterator (<>) is  private;
   
   type List_Iterator is  private;
   type List_Iterator_Ptr is access all List_Iterator;

   -- Reset (It);
   -- while not Is_End (It) loop
   --    Elmt := Current_Item (It);
   --    Process (Elmt);
   --    Next (It);
   -- end loop;

   function New_Iterator
     (L     : List_Id;
      Start : Positive := Positive'First) return List_Iterator;
   function New_Iterator
     (L     : List_Id;
      Start : Positive := Positive'First) return List_Iterator_Ptr;
   -- Create a new iterator and link it with list L.

   procedure Delete_Iterator (It : in out List_Iterator);
   procedure Delete_Iterator (It : in out List_Iterator_Ptr);
   -- Delete the iterator.

   procedure Reset (It : in out List_Iterator);
   procedure Reset (It : in out List_Iterator_Ptr);
   -- Rewind the iterator to point on the first Item of the list.

   function Current_Item (It : List_Iterator) return Item;
   function Current_Item (It : List_Iterator_Ptr) return Item;
   -- Return the Item pointed by the iterator It.
   -- Pre conditions :
   --    . An iterator must has been created and links to a list.
   --    . The iterator must pointed on an Item (Is_End (it) = False).

   function Current_Index (It : List_Iterator) return Positive;
   -- Return the index in the list of the item pointed by the iterator.

   procedure Remove_Current_Item (It : in out List_Iterator);
   procedure Remove_Current_Item (It : in out List_Iterator_Ptr);
   -- Remove the Item pointed by the iterator It.
   --
   -- Pre conditions :
   --    . An iterator must has been created and links to a list.
   --    . The iterator must pointed on an Item (Is_End (it) = False).
   --
   -- Post Condition :
   --    . The iterator pointed on the next Item if any. If the next Item
   --      exists, Last (It) = False. If there is no next Item,
   --      Last (It) = True. So a call to Is_End (It) must be perfomed, before
   --      any subsequent access to an Item with this Iterator.

   procedure Insert_After_Current_Item
     (It     : in out List_Iterator;
      Insert : in Item);
   procedure Insert_After_Current_Item
     (It     : in out List_Iterator_Ptr;
      Insert : in Item);
   -- insert a new item after the current and then next
   --
   -- Pre conditions :
   --    . An iterator must has been created and links to a list.
   --    . The iterator must pointed on an Item (Is_End (it) = False).
   --
   -- Post Condition :
   --    . The iterator pointed on the newly inserted item. 


   procedure Replace_Current_Item
     (It  : in out List_Iterator;
      Elt : in Item);
   -- Replace elemt of current item.

   procedure Replace_Current_Item
     (It  : in out List_Iterator_Ptr;
      Elt : in Item);
   -- Replace elemt of current item.

   function Is_End (It : List_Iterator) return Boolean;
   function Is_End (It : List_Iterator_Ptr) return Boolean;
   -- Return True, if the iterator pointed over the last Item.

   procedure Next (It : in out List_Iterator);
   procedure Next (It : in out List_Iterator_Ptr);
   -- Go ahead one place.

   generic
      with procedure Visitor (E : in Item);
   procedure Iterate (L : List_Id);
   -- Masked Iterator: The procedure Visitor will be called for each
   -- element of the List

   generic
      with function To_String (E : in Item) return String;
   function Print_List (L : in List_Id) return String;
   -- Masked Iterator: The procedure Visitor will be called for each
   -- element of the List

   generic
      with procedure Visitor (E : in Item);
   procedure Reverse_Iterate (L : List_Id);
   -- Masked Iterator: The procedure Visitor will be called for each
   -- element of the List in reverse order

   -- Light iterator --
   --------------------

   type Item_Id is private;
   No_Item_Id : constant Item_Id;

   function First (L : in List_Id) return Item_Id;
   pragma Inline (First);
   -- return the first Item of the list.

   function Last (L : in List_Id) return Item_Id;
   pragma Inline (Last);
   -- return the last item of the list.

   procedure Next (It : in out Item_Id);
   pragma Inline (Next);
   -- Return the next item.

   procedure Previous (It : in out Item_Id);
   pragma Inline (Previous);
   -- return the previous item.

   function Start_From
     (L    : in List_Id;
      From : in Item) return Item_Id;
   pragma Inline (Start_From);
   -- return the previous item.

   function Current_Item (It : in Item_Id) return Item;
   pragma Inline (Current_Item);
   -- To retreive the item value pointed by It.

   generic
      with function Less(I1 : in Item; I2 : in Item) return Boolean;
   procedure Bubblesort(L : in out List_Id);

private

   function Alloc_Item return Item;
   -- This function is to allocate an Item. Currently it does nothing.

   procedure Delete_Item (Elmt : Item);
   -- To delete an Item.

   package Lists_Cells is new Cells
     (Item        => Item,
      No_Item     => No_Item,
      Alloc_Item  => Alloc_Item,
      Delete_Item => Delete_Item);
   use Lists_Cells;
   -- The cells manager.

   type Item_Id is new Lists_Cells.Cell_Id;
   No_Item_Id : constant Item_Id := Item_Id (Lists_Cells.No_Cell);

   type String_Ptr is access all String;

   type List_Record is new Container_Record with record
      Name : String_Ptr := null;
      -- Name of the list. It is provided for dump purpose.

      First : Cell_Id := No_Cell;
      -- First Cell of the list.

      Last  : Cell_Id := No_Cell;
      -- Last cell of the list.

      Count : Natural := 0;
      -- Number of Items currently in the list.

   end record;

   type List_Id is access all List_Record'Class;

   No_List : constant List_Id := null;

   ------------------------
   -- Concrete Iterators --
   ------------------------

   package Lists_Iterators is new Iterators
     (Item => Item,
      The_Container => List_Record);

   use Lists_Iterators;

   type List_Iterator is new Iterator with record
      List : List_Id;
      -- The list to iterate.

      Current_Cell : Cell_Id;
      -- The cell currently pointed by the iterator.

      Current_Index : Positive;
      -- Index of the current cell pointed by the Iterator.

      Start_Cell : Cell_Id;
      -- The cell where the iterator starts.

      Start_Index : Positive;
      -- Index of the start cell.

      Is_End : Boolean;
      -- Flag to find the list end.
   end record;

end Artics.Containers.Lists;
