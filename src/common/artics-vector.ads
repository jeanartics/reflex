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

generic

   type Table_Component_Type is private;
   type Table_Index_Type     is range <>;

   Table_Low_Bound : Table_Index_Type;

package Artics.Vector is

   Vector_Exception : exception;

   type Table_Type is
     array (Table_Index_Type range <>) of Table_Component_Type;

   subtype Big_Table_Type is Table_Type
     (Table_Low_Bound .. Table_Index_Type'Last);

   type Table_Ptr is access all Big_Table_Type;
   --  The table is actually represented as a pointer to allow reallocation.

   type Vector_Record is private;
   type Vector is access all Vector_Record;

   function New_Vector return Vector;
   -- Create a vector with Low_Bound of the table sets to O, with an initial
   -- allocation sets in order to contains 10 elements. The increment is set
   -- to 10, so no more room is available in vector, the vector allocate
   -- place for 10 additionnal elements.

   function New_Vector (From : Vector) return Vector;
   -- Create a vector, and clone it with From.

   function New_Vector (Initial : Positive) return Vector;
   -- Create a vector with initial room for Initial elements. The vector
   -- increment is set to 10 % of Initial.

   function New_Vector
     (Initial : Positive;
      Increment : Natural) return Vector;
   -- Create a vector with Initial size and which increases by Increment % of
   -- Initial.

   procedure Set_Empty (V : Vector);
   -- Empties the buffer. The item count is 0.

   procedure Destroy (V : in out Vector);
   pragma Inline (Destroy);
   -- Destroy the memory allocated by the vector V.

   procedure Initial_Value (V : Vector; Val : Table_Component_Type);
   pragma Inline (Initial_Value);
   -- Initialize the vector with the value Val. All the allocated space is
   -- set to Val.

   procedure Copy
     (From : Vector;
      To   : in out Vector);
   -- Copy the vector From to To. The vector To is first release.

   procedure Copy
     (From       : in Vector;
      Start_From : in Table_Index_Type;
      To         : in out Vector;
      Start_To   : in Table_Index_Type;
      Count      : in Table_Index_Type);
   -- Copy Count elemnts of vector From to the vector To. The first elements
   -- copied from From is at the position Start_From and it is put at the
   -- index Start_To.

   procedure Append_Vector
     (To   : in out Vector;
      V    : Vector;
      Nb   : in Natural := 0) ;
   -- Append the vector V at the end of the vecteur To.

   procedure Append
     (T       : in out Vector;
      New_Val : Table_Component_Type);
   pragma Inline (Append);
   -- Append the value New_Val at the end of the vector T;

   procedure Set_Vector_Increment
     (V         : in out Vector;
      Increment : Natural);
   pragma Inline (Set_Vector_Increment);
   -- Set the increase of the vector to be Increment.

   procedure Reset (V : in out Vector);
   -- Relase the vector. The memory to store the elemnts are freed. This
   -- procedure dont frees the Record Vector.

   function Length (V : Vector) return Natural;
   pragma Inline (Length);
   -- Return the number of elements stored by the Vector.

   function Count (V : Vector) return Natural;
   pragma Inline (Count);
   -- Return the number of elements stored by the Vector.

   function First (V : in Vector) return Table_Index_Type;
   pragma Inline (First);
   -- Returns the Low Bound of the Vector (First Index).

   function Last (T : in Vector) return Table_Index_Type;
   pragma Inline (Last);
   -- Returns the value of the last Index of the vector.

   function Get_Item_At
     (V     : Vector;
      Index : Table_Index_Type) return Table_Component_Type;
   pragma Inline (Get_Item_At);
   -- Returns the item at index Index.

   procedure Set_Item_At
     (V     : in out Vector;
      Index : Table_Index_Type;
      Value : Table_Component_Type);
   pragma Inline (Set_Item_At);
   -- Set the item at index Index to the value Value.

   function First_Item (V : Vector) return Table_Component_Type;
   -- Return the first item in vector.

   function Last_Item (V : Vector) return Table_Component_Type;
   -- Return the last item in vector.

   function Get_Table (V : Vector) return Table_Ptr;
   pragma Inline (Get_Table);
   -- This function return the pointer on the buffer. It is not intended for
   -- use by client.

   procedure Set_Last
     (T       : in out Vector;
      New_Val : Table_Index_Type);


   function Is_Empty (V : in Vector) return Boolean;
   -- Return True if the vector does not hold items.

   procedure Pop
     (V    : in out Vector;
      Item : out Table_Component_Type);
   -- Return the last item added to the vector, and suppress it from the
   -- vector.

   procedure Push
     (V    : in out Vector;
      Item : in Table_Component_Type);
   -- Append the item to the vector.

   function Top (V : in Vector) return Table_Component_Type;
   -- Return the last item added to the vector.

   procedure Decrement_Last (T : in out Vector);
   -- Decrement the last subscript of the vector.

   procedure Increment_Last (T : in out Vector);
   -- Increment the last subscript of the vector.

   procedure Init (T : in out Vector);
   -- Initialize the vector as it was at its creation.

private

   type Vector_Record Is record
      Table : aliased Table_Ptr := null;
      -- The table itself. The lower bound is the value of Low_Bound. Logically
      -- the upper bound is the current value of Last (although the actual size
      -- of the allocated table may be larger than this). The program may only
      -- access and modify Table entries in the range First .. Last.

      Max : Integer;
      --  Subscript of the maximum entry in the currently allocated table

      Min : Integer;
      --  Subscript of the minimum entry in the currently allocated table

      Length : Integer := 0;
      --  Number of entries in currently allocated table. The value of zero
      --  ensures that we initially allocate the table.

      Last_Val : Integer := 0;
      --  Current value of Last.

      Table_Initial : Positive;
      -- Initial size of the vector.

      Table_Increment : Natural;
      -- Amount of increase if the table has to increased in size.
   end record;

end Artics.Vector;
