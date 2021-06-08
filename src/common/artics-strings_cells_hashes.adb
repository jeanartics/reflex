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

with Unchecked_Deallocation;

package body Artics.Strings_Cells_Hashes is
   
   ----------------------
   -- Free_Cell_String --
   ----------------------
   
   procedure Free_Cell_String (S : in out Name_Id) is
   begin
      null;
   end Free_Cell_String;
   pragma Inline (Free_Cell_String);
   
   ------------------------
   -- Name_Hash_Function --
   ------------------------
   
   function String_Cell_Hash_Function
     (Key : Name_Id) return String_Cell_Hash_Type is
   begin
      return String_Cell_Hash_Type (Key mod 2**7);
   end String_Cell_Hash_Function;
   
   ----------------------------
   -- New_String_Cell_Htable --
   ----------------------------
   
   function New_String_Cell_Hash return String_Cell_Hash is
      Sh : String_Cell_Hash;
   begin
      Sh.Instance := Str_Hashes.New_Htable;
      return Sh;
   end New_String_Cell_Hash;
   
   -----------
   -- Reset --
   -----------
   
   procedure Reset (Sh : in out String_Cell_Hash) is
   begin
      Str_Cells_Hashes.Reset (Sh.Instance);
   end Reset;
   
   ---------
   -- Set --
   ---------
   
   procedure Set
     (Sh : in out String_Cell_Hash; 
      K  : Name_Id;
      C  : Cell_Id) is
      
   begin
      Str_Cells_Hashes.Set (Sh.Instance, K, C);
   end Set;
   
   ---------
   -- Set --
   ---------
   
   procedure Set
     (Sh : in out String_Cell_Hash; 
      K  : String;
      C  : Cell_Id) is
   begin
      Str_Cells_Hashes.Set (Sh.Instance, String_Find (K), C);
   end Set;
   
   ---------
   -- Get --
   ---------
   
   function Get (Sh : String_Cell_Hash; K : Name_Id) return Cell_Id is
   begin
      return Str_Cells_Hashes.Get (Sh.Instance, K);
   end Get;
   
   ---------
   -- Get --
   ---------
   
   function Get (Sh : String_Cell_Hash; K : String) return Cell_Id is
      Kname : Name_Id := String_Find (K);
   begin
      return Get_String (Get (Sh, String_Find (K)));
   end Get;
   
   ------------
   -- Remove --
   ------------
   
   procedure Remove (Sh : String_Cell_Hash; K : Name_Id) is
   begin
      Str_Cells_Hashes.Remove (Sh.Instance, K);
   end Remove;
   
   ------------
   -- Remove --
   ------------
   
   procedure Remove (Sh : String_Cell_Hash; K : String) is
   begin
      Remove (Sh, String_Find (K));
   end Remove;
   
   --------------
   -- Itarator --
   --------------
   
   -------------------------
   -- New_String_Iterator --
   -------------------------
   
   function New_String_Iterator
     (Sh : String_Cell_Hash) return String_Cell_Hash_Iterator is
      
      It : Str_Cells_Hashes.Hash_Iterator := 
	Str_Cells_Hashes.New_Iterator (Sh.Instance);
   begin
      return String_Cell_Hash_Iterator'(Hash_It => It);
   end New_String_Iterator;
   
   ------------
   -- Is_End --
   ------------
   
   function Is_End (It : String_Cell_Hash_Iterator) return Boolean is
   begin
      return Str_Cells_Hashes.Is_End (It.Hash_It);
   end Is_End;
   
   ----------
   -- Next --
   ----------
   
   procedure Next (It : in out String_Cell_Hash_Iterator) is
   begin
      Str_Cells_Hashes.Next (It.Hash_It);
   end Next;
   
   -------------
   -- Current --
   -------------
   
   function Current (It : String_Cell_Hash_Iterator) return Cell_Id is
   begin
      return Str_Cells_Hashes.Current (It.Hash_It);
   end Current;
   
end Artics.Strings_Cells_Hashes;

