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

   with function "=" (It1: Item; It2: Item) return Boolean;
package Artics.Containers.Sets is

   pragma Elaborate_Body;

   -- This package defines a common protocol for the lists (single and double
   -- linked list). The concrete implementation adds specific methods for the
   -- type of list (Reverse_Iterator for double linked lists for example).

   -- The choice to provide this package is to enforce a common interface for
   -- all type of lists, and therefore, separate the implementation of the
   -- abstraction. We have pay attention to mimimize the overhead at run-time,
   -- all the method's resolutions are made at the compile Time.


   Set_Error            : exception;
   Duplicate_Item_Error : exception;
   Set_Item_Access_Error: exception;

   type Set_Record is new Container_Record with private;
   type Set_Id is private;

   No_Set : constant Set_Id;

   function New_Set (Name : String := "") return Set_Id;
   -- Create a new empty Set and named it Name.

   function New_Set
     (Elmt : Item;
      Name : string) return Set_Id;
   -- Create a new set with the first Item set to Elmt.

   function New_Set_Copy
     (Set  : Set_Id;
      Name : String := "") return Set_Id;
   -- Create a new set clone of Set. The cells are duplicated, so don't
   -- shared by the sets.

   procedure Copy_Set_To
     (Set  : Set_Id;
      To   : Set_Id);
   -- Copy the set Set to the Set To. The Set To must have been created
   -- If the Set To is not empty, then To is reset (cells are deallocated),
   -- and then the copy is done.

   function Length (Set : Set_Id) return Natural;
   -- Return the number of items in the set.

   function Is_Empty_Set (Set : Set_Id) return Boolean;
   -- Return True if and only if there are no items in the set.

   function Is_Member
     (Set  : Set_Id;
      Elmt : Item) return Boolean;
   -- return True if Item has been found in set Set.

   function Get_Member
     (Set  : Set_Id;
      Elmt : Item) return Item;
   -- returns the item corresponding to provided item. If not found
   -- returns No_Item

   procedure Append
     (Elmt      : Item;
      To        : Set_Id;
      OverWrite : Boolean := False);
   -- Add the Item Elmt in the set.
   -- Raise Duplicate_Item_Error if the inserted item already exist and
   -- overwrite equal false. Otherwise the old Item is replaced by the new
   -- one

   procedure Append_Set
     (Set  : Set_Id;
      To   : Set_Id);
   -- Concat the set "Set" to set "To". The two sets shared their cells.
   -- So if a call to Reset is performed on one of the two sets, the cells
   -- are deallocated and therefore, the two sets are no more valid. We made
   -- this choice for performance reasons. Another choice should be to manage
   -- the number of reference to the cell, and when a cell is no more referen-
   -- ced, to deallocate the cell. But this mechanism enforces to walk the list
   -- "To" to mark the cells, and this is run-time overhead. Nevertheless, if
   -- a call to Reset is done in one of the set, all tries to acess an Item,
   -- raised the exception Set_Item_Access_Error.

   procedure Clear_Set
     (Set  : Set_Id);
   -- Delete all the cells of the set.

   function Remove
     (Set  : Set_Id;
      Elmt : Item) return Item;
   procedure Remove
     (Set  : Set_Id;
      Elmt : Item);
   -- Remove matching item from the set
   -- Function returns the removed item

   function Get_Set_Name
     (Set : Set_Id) return String;
   -- Returns the name of the Set.

   procedure Set_Set_Name
     (Set : Set_Id;
      Name : String);
   -- Change the set name to Name.

   procedure Delete_Set
     (Set : in out Set_Id);
   -- Delete all the cells of the set and then the set.

   -- Sets Iterators --
   ---------------------

   ------------------------
   -- Concrete Iterators --
   ------------------------

   --type Set_Iterator (<>) is  private;
   type Set_Iterator  is  private;

   -- Reset (It);
   -- while not Last (It) loop
   --    Elmt := Current_Item (It);
   --    Process (Elmt);
   --    Next (It);
   -- end loop;

   function New_Iterator (Set : Set_Id) return Set_Iterator;
   -- Create a new iterator and link it with set Set.

   procedure Delete_Iterator (It : Set_Iterator);
   -- Delete the iterator.

   procedure Reset (It : in out Set_Iterator);
   -- Rewind the iterator to point on the "first" Item of the set.

   function Current_Item (It : Set_Iterator) return Item;
   -- Return the Item pointed by the iterator It.
   -- Pre conditions :
   --    . An iterator must has been created and links to a set.
   --    . The iterator must pointed on an Item (Is_End (it) = False).

   procedure Remove_Current_Item (It : in out Set_Iterator);
   -- Remove the Item pointed by the iterator It.
   --
   -- Pre conditions :
   --    . An iterator must has been created and links to a set.
   --    . The iterator must pointed on an Item (Is_End (it) = False).
   --
   -- Post Condition :
   --    . The iterator pointed on the next Item if any. If the next Item
   --      exists, Last (It) = False. If there is no next Item,
   --      Last (It) = True. So a call to Is_End (It) must be perfomed, before
   --      any subsequent access to an Item with this Iterator.

   function Is_End (It : Set_Iterator) return Boolean;
   -- Return True, if the iterator pointed over the last Item.

   procedure Next (It : in out Set_Iterator);
   -- Go ahead one place.

   generic
      with procedure Visitor (E: Item);
   procedure Iterate(Set: Set_Id);
   -- Masked Iterator: The procedure Visitor will be called for each
   -- element of the Set

private

   function Alloc_Item return Item;
   -- This function is to allocate an Item. Currently it does nothing.

   procedure Delete_Item (Elmt : Item);
   -- To delete an Item.

   package Sets_Cells is new Cells
     (Item        => Item,
      No_Item     => No_Item,
      Alloc_Item  => Alloc_Item,
      Delete_Item => Delete_Item);
   use Sets_Cells;
   -- The cells manager.

   type String_Ptr is access all String;

   type Set_Record is new Container_Record with record
      Name : String_Ptr;
      -- Name of the list. It is provided for dump purpose.

      First : Cell_Id;
      -- First Cell of the set.

      Number : Natural;
      -- Number of Items currently in the set.

   end record;

   type Set_Id is access all Set_Record'Class;

   No_Set : constant Set_Id := null;

   ------------------------
   -- Concrete Iterators --
   ------------------------

   package Set_Iterators is new Iterators
     (Item => Item,
      The_Container => Set_Record);

   use Set_Iterators;

   type Set_Iterator is new Iterator with record
      Set : Set_Id;
      Current_Cell : Cell_Id;
      -- The cell currently pointed by the iterator.
   end record;

end Artics.Containers.Sets;
