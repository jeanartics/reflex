------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 2012-2015, Free Software Foundation, Inc.         --
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

--  Hash table searching routines

--  This package contains two separate packages. The Simple_HTable package
--  provides a very simple abstraction that associates one element to one
--  key value and takes care of all allocations automatically using the heap.
--  The Static_HTable package provides a more complex interface that allows
--  complete control over allocation.

--  This package provides a facility similar to that of GNAT.HTable, except
--  that this package declares types that can be used to define dynamic
--  instances of hash tables, while instantiations in GNAT.HTable creates a
--  single instance of the hash table.

with Ada.Unchecked_Deallocation;

generic
   type Header_Num is mod <>;
   
   --  An integer type indicating the number and range of hash headers
   
   type Element is private;
   --  The type of element to be stored
   
   No_Element : Element;
   --  The object that is returned by Get when no element has been set for
   --  a given key
   
   type Key is private;
   with function Hash  (F : Key)      return Header_Num;
   with function Equal (F1, F2 : Key) return Boolean;
   
   with procedure Free_Elmt (E : in out Element);
   
package Artics.Dynamic_HTables is

   -------------------
   -- Static_HTable --
   -------------------

   --  A low-level Hash-Table abstraction, not as easy to instantiate as
   --  Simple_HTable. This mirrors the interface of GNAT.HTable.Static_HTable,
   --  but does require dynamic allocation (since we allow multiple instances
   --  of the table). The model is that each Element contains its own Key that
   --  can be retrieved by Get_Key. Furthermore, Element provides a link that
   --  can be used by the HTable for linking elements with same hash codes:

   --       Element

   --         +-------------------+
   --         |       Key         |
   --         +-------------------+
   --         :     Element       :
   --         +-------------------+
   --         |     Next Elmt     |
   --         +-------------------+


   type Htable is private;
   No_Htable : constant Htable;
   
   function New_Htable return Htable;
   -- create a new hash_table
   
   procedure Reset (T : in out Htable);
   --  Resets the hash table by releasing all memory associated with
   --  it. The hash table can safely be reused after this call. For the
   --  most common case where Elmt_Ptr is an access type, and Null_Ptr is
   --  null, this is only needed if the same table is reused in a new
   --  context. If Elmt_Ptr is other than an access type, or Null_Ptr is
   --  other than null, then Reset must be called before the first use of
   --  the hash table.
   
   procedure Set
     (T : in out Htable; 
      K : Key;
      E : Element);
   --  Insert the element pointer in the HTable
   
   function Get (T : Htable; K : Key) return Element;
   --  Returns the latest inserted element pointer with the given Key
   --  or null if none.
   
   procedure Remove (T : Htable; K : Key);
   --  Removes the latest inserted element pointer associated with the
   --  given key if any, does nothing if none.
   
   --------------
   -- Itarator --
   --------------
   
   Hash_Iterator_Error : exception;
   
   type Hash_Iterator is private;
   
   function New_Iterator (T : Htable) return Hash_Iterator;
   -- create an ietrator, ant put the cursor on the first element of the
   -- first entry in hash_table.
   
   function Is_End (It : Hash_Iterator) return Boolean;
   -- return True if the ietrator is in the last element of the hash table.
   
   procedure Next (It : in out Hash_Iterator);
   -- Forward the iteraor one elemrnt
   
   function Current (It : Hash_Iterator) return Element;
   -- Erturn the current element pointed by iteraor
   
   
private
   
   type Element_Wrapper;
   type Elmt_Ptr is access all Element_Wrapper;
   type Element_Wrapper is record
      K    : Key;
      E    : Element;
      Next : Elmt_Ptr;
   end record;

   Null_Ptr : constant Elmt_Ptr := null;
   
   type Table_Type is array (Header_Num) of Elmt_Ptr;
   
   type Htable_Data is record
      Table : Table_Type;
   end record;
   
   type Htable is access all Htable_Data;
   
   No_Htable : constant Htable := null;
   
   type Hash_Iterator is record
      T              : Htable;
      Iterator_Index : Header_Num;
      Iterator_Ptr   : Elmt_Ptr;
   end record;
   
   procedure Free is new
     Ada.Unchecked_Deallocation (Element_Wrapper, Elmt_Ptr);
   
   procedure Set (T : in out Htable; E : Elmt_Ptr);
   
   procedure Get_Non_Null (It : in out Hash_Iterator);
   --  Returns Null_Ptr if the Table is empty. Returns Iterator_Ptr if non
   -- null, or the next non null element in table if any.
   
   procedure Set_Next_Elmt (E : Elmt_Ptr; Next : Elmt_Ptr);
   function Next_Elmt      (E : Elmt_Ptr) return Elmt_Ptr;
   function  Get_Key       (E : Elmt_Ptr) return Key;
   
end Artics.Dynamic_Htables;
