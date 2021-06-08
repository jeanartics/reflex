------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 2016, Free Software Foundation, Inc.              --
--                                                                          --
-- Reflex is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware Foundation; either version 3, or (at your option) any later version --
-- Reflex is distributed in the hope that it will be u, but WITH-      --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License distributed with Reflex; see file COPYING3. If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- Reflex is originally developed by Artics                                 --
------------------------------------------------------------------------------

generic
   type Element is private;
   No_Element : Element;
   
   type Index is range <>;
   
package Artics.Dynamic_Arrays_N is
   Max : constant := Integer'Last;

   subtype Element_Index is Integer range 0 .. Max;
   
   type Dynamic_Array is array (Element_Index range <>) of aliased Element;

   type Dynamic_Array_Ptr is access all Dynamic_Array;

   function New_Dynamic_Array
     (Count : Element_Index) return Dynamic_Array_Ptr;
   -- Create a new array with Count entries
   
   procedure Free_Array (Array_Ptr : in out Dynamic_Array_Ptr);
   -- Free the array memory
   
   procedure Resize
     (Array_Ptr : in out Dynamic_Array_Ptr;
      Count     : Element_Index);
   -- Resize array to Count entries. If count is > current array length, the 
   -- array grows to Count entries, if count is < current array length, the 
   -- array is shrinked to count entries. The element values of the array is
   -- copied to the new array.
   
   procedure Copy
     (Src        : Dynamic_Array_Ptr;
      Src_Start  : Element_Index;
      Dest       : Dynamic_Array_Ptr;
      Dest_Start : Element_Index;
      Count      : Element_Index);
   -- Copy element from Src starting at Src_Start index to Dest at position 
   -- Dest_Start. 
   
private
   
   pragma Inline (New_Dynamic_Array);
   pragma Inline (Resize);
   pragma Inline (Free_Array);

end Artics.Dynamic_Arrays_N;
