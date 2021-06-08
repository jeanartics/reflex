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
with Artics.Dynamic_Htables;
with Artics.Namet; use Artics.Namet;

package Artics.Strings_Cells_Hashes is
   
   type String_Cell_Hash is tagged private;
   
   No_String_Cell_Hash : constant String_Cell_Hash;
     
   function New_String_Cell_Hash return String_Cell_Hash;
   -- create a new hash_table
   
   procedure Reset (Sh : in out String_Cell_Hash);
   --  Resets the hash table by releasing all memory associated with
   --  it. The hash table can safely be reused after this call. For the
   --  most common case where Elmt_Ptr is an access type, and Null_Ptr is
   --  null, this is only needed if the same table is reused in a new
   --  context. If Elmt_Ptr is other than an access type, or Null_Ptr is
   --  other than null, then Reset must be called before the first use of
   --  the hash table.
   
   procedure Set
     (Sh : in out String_Cell_Hash; 
      K  : String;
      VC : Cell_id);
   procedure Set
     (Sh : in out String_Cell_Hash; 
      K  : Name_Id;
      C  : Cell_Id);
   --  Insert the element pointer in the HTable
   
   function Get (Sh : String_Cell_Hash; K : String)  return Cell_Id;
   function Get (Sh : String_Cell_Hash; K : Name_Id) return Cell_Id;
   --  Returns the latest inserted element pointer with the given Key
   --  or null if none.
   
   procedure Remove (Sh : String_Cell_Hash; K : String);
   procedure Remove (Sh : String_Cell_Hash; K : Name_Id);
   --  Removes the latest inserted element pointer associated with the
   --  given key if any, does nothing if none.
   
   --------------
   -- Itarator --
   --------------
   
   type String_Cell_Hash_Iterator is private;
   
   function New_String_Cell_Iterator
     (Sh : String_Cell_Hash) return String_Hash_Cell_Iterator;
   -- create an ietrator, ant put the cursor on the first element of the
   -- first entry in hash_table.
   
   function Is_End (It : String_Cell_Hash_Iterator) return Boolean;
   -- return True if the ietrator is in the last element of the hash table.
   
   procedure Next (It : in out String_Cell_Hash_Iterator);
   -- Forward the iteraor one elemrnt
   
   function Current (It : String_Cell_Hash_Iterator) return Cell_Id;
   -- Return the current element pointed by iteraor
   
private   
   
   procedure Free_Cell_String (S : in out Name_Id);
   
   type String_Cell_Hash_Type is mod 2**7 - 1;
   function String_Cell_Hash_Function
     (Key : Name_Id) return String_Cell_Hash_Type;
   
   package Str_Cells_Hashes is new Artics.Dynamic_HTables
     (Header_Num => String_Cell_Hash_Type,
      Element    => Artics.Types.Cell_Id,
      No_Element => Artics.Namet.No_Cell,
      Key        => Artics.Types.Name_Id,
      Hash       => String_Cell_Hash_Function,
      Equal      => "=",
      Free_Elmt  => Free_Cell_String);
   
   String_Cell_Hash_Iterator_Error : exception 
     renames Str_Hashes.Hash_Iterator_Error;
   
   type String_Cell_Hash is tagged record
      Instance : Str_Hashes.Htable;
   end record;
   
   type String_Cell_Hash_Iterator is record
      Hash_It : Str_Hashes.Hash_Iterator;
   end record;

   No_String_Cell_Hash : constant String_Cell_Hash := String_Cell_Hash'
     (Instance => Str_Hashes.No_Htable);
   
end Artics.Strings_Cells_Hashes;

