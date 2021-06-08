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

with Artics.Vector;

generic

   type Element_Type is private;

   Initial_Size : Integer;

package Artics.Dynamic_Stack is

   type Stack_Record is private;
   type Stack is access all Stack_Record;

   Stack_Empty : exception;
   Stack_Error : exception;
   -- Raise when attempting to get from an empty stack.

   function New_Stack return Stack;
   -- Create a new_stack, and return a reference to it.

   function Empty (S : in Stack) return Boolean;
   -- Returns true if the stack is empty.

   procedure Push
     (S    : in Stack;
      Elmt : in Element_Type);
   -- Put Elmt in top of the stack.

   function Top (S : in Stack) return Element_Type;
   -- Returns the elemnt in the top of stack. The element is left in top of the
   -- stack. Rasie Stack_Empty, if the stack is empty.

   function Pop (S : in Stack) return Element_Type;
   -- Returns the elemnt in the top of stack, and remove it from stack. Raise
   -- Stack_Empty if stack is empty.

   function Topn
     (S : in Stack;
      N : in Natural) return Element_Type;
   -- Returns the elemnt in the top - n of stack. The element is left in the
   -- stack. Rasie Stack_Error, if the element count in stack is less than N.

   function Popn
     (S : in Stack;
      N : in Natural) return Element_Type;
   -- Returns the elemnt in the top -n of stack, and remove it from stack.
   -- Raise Stack_Error if element count of stack is less than N.

   function Count (S : in Stack) return Natural;
   -- Returns the number of element present in the stack.

   procedure Freeze (S : in Stack);
   -- The stack grows dynamically. To avoid a lot of memory reallocation, the
   -- reallocation is done by chunks. So, this function reallocate the stack,
   -- in order to take the memory just needed to contained the number of
   -- elements in the stack.

   procedure Reset (S : in Stack);
   -- The stak is emptied, and it is realocated to its initial size.

   procedure Delete_Stack (S : in Stack);

   function First (S : in Stack) return Natural;
   pragma Inline (First);
   -- Returns the Low Bound of the Vector (First Index).

   function Last (S : in Stack) return Natural;
   pragma Inline (Last);
   -- Returns the value of the last Index of the vector.

   function Get_Item_At
     (S     : in Stack;
      Index : in Natural) return Element_Type;
   pragma Inline (Get_Item_At);
   -- Returns the item at index Index.

   procedure Set_Item_At
     (S     : in out Stack;
      Index : in Natural;
      Value : in Element_Type);
   pragma Inline (Set_Item_At);
   -- Set the item at index Index to the value Value.

   procedure Set_Top_At
     (S : in out Stack;
      N : in Natural);
   -- The top stack is now Index.

   function Is_In_Stack
     (S    : in Stack;
      Elmt : in Element_Type) return Boolean;
   -- Returns true if the elmt is in stack.

   function Get_Stack_Position
     (S    : in Stack;
      Elmt : in Element_Type) return Natural;
   -- Return the position of Elmt in stack S. The position is the index of the
   -- underlying vector index.

private

   package Stack_Container is new Vector (Element_Type, Integer, 1);
   use Stack_Container;

   type Stack_Record is record
      Table : Stack_Container.Vector;
      -- The storage of the stack.

      Initial : Integer;
      -- Initial allocation for the vector holding the stack.
   end record;

end Artics.Dynamic_Stack;

