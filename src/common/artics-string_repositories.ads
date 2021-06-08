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

with Ada.Unchecked_Conversion;
with Table;
pragma Elaborate(Table);

package String_Repositories is

   type String_Id is private;

   No_String : constant String_Id;

   --procedure Get_String (Id: String_Id);

   function Get_String (Id : String_Id) return String;
   function Get_Integer (Id : String_Id) return Integer;
   function Get_Float (Id : String_Id) return Float;
   --  This functional form returns the result as a string without affecting
   --  the contents of either Name_Buffer or Name_Len. Get_Integer returns
   --  an integer if found

   function Get_String_Length (Id : String_Id) return Integer;
   -- Return the string length

   function Insert_String(Str: String) return String_Id;
   -- Search for corresponding name in the list of String. If found it returns
   -- the corresponding String_Id and increments the reference counter. If not
   -- it appends the string to the table_char table by using free space table
   -- and create an entry in table_entry.

   --function Update_String(Id: String_Id; Str: String) return String_Id;
   -- Search for corresponding String_Id; decrease Ref count if null put
   -- space in the free table and call Insert_String with corresponding
   -- string.

   function Is_String(Str: String) return String_Id;
   -- Search for an existing string in the repository

   function Is_Not_Found(Id: String_Id) return Boolean;
   pragma Inline(Is_Not_Found);

   function Is_Found(Id: String_Id) return Boolean;
   pragma Inline(Is_Found);

   --function "+" (Id1: String_Id; Id2: String_Id) return String_Id;
   --function "+" (Id1: String_Id; Str: String) return String_Id;
   --function "+" (Str: String; Id1: String_Id) return String_Id;
   -- Strings Concatenation

   procedure Init;
   --  Initialize all required tables.

   procedure Dump;
   -- Dump the content of the String_Repository

   function Upper_Character(C: Character) return Character;

   ------------------------
   -- Debug of String_Id --
   ------------------------
   function To_String(Id: String_Id) return String;
   function To_Integer(Id: String_Id) return Integer;
private

   type String_Id is new Integer;
   type Character_Id is new Integer;
   type Free_Id is new Integer;

   No_String : constant String_Id := -1;

   --  This table stores the actual string names. Although logically there
   --  is no need for a terminating character (since the length is stored
   --  in the name entry table), we still store a NUL character at the end
   --  of every name (for convenience in interfacing to the C world).

   package String_Chars is new Table.Table (
     Table_Component_Type => Character,
     Table_Index_Type     => Character_Id,
     Table_Low_Bound      => 0,
     Table_Initial        => 5000,
     Table_Increment      => 5000,
     Table_Name           => "String_Chars");

   type String_Entry is record
      String_Chars_Index : Character_Id;
      --  Starting location of characters in the Name_Chars table minus
      --  one (i.e. pointer to character just before first character). The
      --  reason for the bias of one is that indexes in Name_Buffer are
      --  one's origin, so this avoids unnecessary adds and subtracts of 1.
      String_Len : Integer;
      --  Length of this name in characters
      Reference_Count : Integer;
      --  Reference count is used to remove the entry when not used anymore
      Hash_Link : String_Id;
      --  Link to next entry in names table for same hash code
   end record;

   --  This is the table that is referenced by Name_Id entries.
   --  It contains one entry for each unique name in the table.

   package String_Entries is new Table.Table
     (Table_Component_Type => String_Entry,
      Table_Index_Type     => String_Id,
      Table_Low_Bound      => 0,
      Table_Initial        => 500,
      Table_Increment      => 500,
      Table_Name           => "String_Entries");
   
   type Empty_Entry is record
      String_Chars_Index : Character_Id;
      -- Starting location of free space in Name_Chars table minus one
      Free_Len : Integer;
      -- Length of the free space
      Next_Bigger_Zone : Free_Id;
   end record;

   package Empty_Entries is new Table.Table
     (Table_Component_Type => Empty_Entry,
      Table_Index_Type     => Free_Id,
      Table_Low_Bound      => 0,
      Table_Initial        => 50,
      Table_Increment      => 50,
      Table_Name           => "Empty_Entries");


end String_Repositories;
