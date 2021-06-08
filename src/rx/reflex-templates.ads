------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
--                                                                          --
-- Reflex is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public Licensea as published  by the Free Soft- --
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

with Ada.Text_IO; use Ada.Text_IO;

--  This package provides convenient subprograms for replacing specific
--  substrings with string values. The substrings are delimited by an open
--  character delimiter and a close character delimiter for example
--  {substring}. In this exmaple, The open delimiter is '{ and the close
--  delimiter is '}', the tag is the substring delimited. A string value is
--  associated with each tag, and will replaced the tag when emiiting the
--  template. The template can be retreive in sequence, in that that case an
--  index points the last character emited. Retreive_Next emits the string from
--  the index position to the next tag whithout value associated. For example
--  let be a template with 3 tags. The user can replace the first tag, Retreive
--  the string emit from the start of the template to the end of the tag
--  position, replacing the tag with its values. The next call to Retreive_Next
--  returns the string between the character following the first tag to the
--  end position of the second tag, replacing the later by its value.


with Ada.Unchecked_Deallocation;

with Artics.Buffers; use Artics.Buffers;

package Reflex.Templates is

   type Template_Record is tagged private;
   type Template_Ptr is access all Template_Record;

   Template_Exception : exception;

   No_Template_record : constant Template_Record;

   type String_Ptr is access all String;

   procedure Free_String (S : in out String_Ptr);

   function New_Template return Template_Ptr;
   --  Create an empty template

   function New_Template_From_String
     (Name      : String;
      Str       : String_Ptr;
      Tag_Count : Natural;
      Open_Tag  : Character := '{';
      Close_Tag : Character := '}') return Template_Ptr;
   --  Create a template for the model string Str which contains Count Tags
   --  and the tag are delimited by Open_tag and Close_Tag.

   function New_Template_From_File
     (Name           : String;
      Full_File_Name : String;
      Tag_Count      : Natural;
      Open_Tag       : Character;
      Close_Tag      : Character) return Template_Ptr;
   --  The template is created reading the model string from the content of
   --  the file. The file name must be its full path name.

   procedure Free_Template (This : in out Template_Ptr);
   --  Delete the template, freeing all allocated memory (Tags, values, Name..)

   procedure Set_Tag_Value
     (This  : access Template_Record;
      Tag   : in String;
      Value : in String);
   --  Associate the string Value with the tag Tag

   procedure Add_Tag
     (This : access Template_Record;
      Tag  : in String);
   --  Add a tag with an empty to the list of tags.

   function Get_Tag_value
     (This : access Template_Record;
      Tag  : in String) return String_Ptr;
   --  Return the string value of the Tag.

   function Has_Value
     (This : access Template_Record;
      Tag  : in String) return Boolean;
   --  Return true if a value has beeen associated with tha tag.

   function Retreive_Next_String
     (This : access Template_Record) return String_Ptr;
   --  Return the string from the current position to the first Tag
   --  Without value

   procedure Retreive_Next_Buffer
     (This : access Template_Record;
      Ob   : in Output_buffer);
   --  Put the string from the current position to the first Tag
   --  Without value in the buffer Ob.

   procedure Load_Data
     (This           : access Template_Record;
      Full_File_Name : in String);
   --  Read template data on file Full_File_Name and put them in the data of
   --  template.

   procedure Reset_Index_Template (This : access Template_Record);
   --  Reset the current index to the begining of the file

   procedure Reset_Template_Values (This : access Template_Record);
   --  Reset all values associted with tags. The raed index is rewind to the
   --  Begining of the templaate

   procedure Set_Open_Tag
     (This  : access Template_Record;
      Open  : in Character);
   --  Set the open delimiter to the value of character Open

   function Get_Open_Tag (This : access Template_Record) return Character;
   --  Return the open character delimiter

   function Get_Close_Tag (This : access Template_Record) return Character;
   --  Set the close delimiter to the value of character Close

   procedure Set_Close_Tag
     (This  : access Template_Record;
      Close : in Character);
   --  Return the open character delimiter


private

   type Assoc_Record is record
      Tag   : String_Ptr;
      Value : String_Ptr;
   end record;

   No_Assoc_Record : constant Assoc_Record :=
     (Tag   => null,
      Value => null);

   type Assocs_Array is array (Natural range <>) of Assoc_Record;
   type Assocs_Ptr is access all Assocs_Array;

   type Template_Record is tagged record
      Name      : String_Ptr;
      Count     : Natural;
      Open_Tag  : Character;
      Close_Tag : Character;
      Assocs    : Assocs_Ptr;
      Data      : String_Ptr;
      Index     : Natural;
   end record;

   No_Template_Record : constant Template_Record :=
     (Name      => null,
      Count     => 0,
      Open_Tag  => '{',
      Close_Tag => '}',
      Assocs    => null,
      Data      => null,
      Index     => 0);

end Reflex.Templates;
