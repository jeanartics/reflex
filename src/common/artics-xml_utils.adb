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
-- Reflex is originally developed by Artics
------------------------------------------------------------------------------

with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_Io; use Ada.Streams.Stream_Io;
with Unicode;
with Unicode.Encodings;
with DOM.Core.Elements; -- use DOM.Core.Elements;

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.Case_Util; use GNAT.Case_Util;

with Artics.Graph.Names; use Artics.Graph.Names;
with Artics.Utils; use Artics.Utils;

package body Artics.Xml_Utils is
   
   ------------------
   -- Append_Child --
   ------------------
   
   procedure Append_Child
     (Parent : Dom.Core.Node;
      Child  : Dom.Core.Node) is
      
      Dummy_N : Dom.Core.Node;
   begin
      Dummy_N := Append_Child (Parent, Child);
   end Append_Child;
   
   -------------------
   -- Set_Attribute --
   -------------------
   
   procedure Set_Attribute
     (Elem  : Dom.Core.Element;
      Name  : String;
      Value : String) is
      
      Name_Str  : String := Trim (Name, Both);
      Value_Str : String := Trim (Value, Both);
   begin
      Dom.Core.Elements.Set_Attribute (Elem, Name_Str, Value_Str);
   end Set_Attribute;
   
   -------------------
   -- Set_Attribute --
   -------------------
   
   procedure Set_Attribute
     (Elem  : Dom.Core.Element;
      Name  : String;
      Value : Name_Id) is
   begin
      Set_Attribute (Elem, Name, Get_String (Value));
   end Set_Attribute;
   
   -------------------
   -- Set_Attribute --
   -------------------
   
   procedure Set_Attribute
     (Elem  : Dom.Core.Element;
      Name  : Name_Id;
      Value : Name_Id) is
   begin
      Set_Attribute (Elem, Get_String (Name), Get_String (Value));
   end Set_Attribute;
   
   -------------------
   -- Set_Attribute --
   -------------------
   
   procedure Set_Attribute
     (Elem  : Dom.Core.Element;
      Name  : String;
      Value : Float) is
   begin
      Set_Attribute (Elem, Name, Float_To_String (Value));
   end Set_Attribute;
   
   procedure Set_Attribute
     (Elem  : Dom.Core.Element;
      Name  : Name_Id;
      Value : Float) is
   begin
      Set_Attribute (Elem, Get_String (Name), Value);
   end Set_Attribute;
   
   -------------------
   -- Set_Attribute --
   -------------------
   
   procedure Set_Attribute
     (Elem  : Dom.Core.Element;
      Name  : String;
      Value : Integer) is
   begin
      Set_Attribute (Elem, Name, Integer_To_String (Value));
   end Set_Attribute;
   
   procedure Set_Attribute
     (Elem  : Dom.Core.Element;
      Name  : Name_Id;
      Value : Integer) is
   begin
      Set_Attribute (Elem, Get_String (Name), Value);
   end Set_Attribute;
   
   -------------------
   -- Set_Attribute --
   -------------------
   
   procedure Set_Attribute
     (Elem  : Dom.Core.Element;
      Name  : String;
      Value : Boolean) is
   begin
      Set_Attribute (Elem, Name, Boolean_To_String (Value));
   end Set_Attribute;
   
   procedure Set_Attribute
     (Elem  : Dom.Core.Element;
      Name  : Name_Id;
      Value : Boolean) is
   begin
      Set_Attribute (Elem, Get_String (Name), Value);
   end Set_Attribute;
   
   -------------------
   -- Get_Attribute --
   -------------------
   
   function Get_Attribute
     (Elem : Dom.Core.Element;
      Name : String) return String is
   begin
      return Dom.Core.Elements.Get_Attribute (Elem, Name);
   end Get_Attribute;
   
   -------------------
   -- Get_Attribute --
   -------------------
   
   function Get_Attribute
     (Elem : Dom.Core.Element;
      Name : Name_Id) return String is
   begin
      return Get_Attribute (Elem, Get_String (Name));
   end Get_Attribute;
   
   ----------------------------
   -- Write_Document_To_File --
   ----------------------------
   
   procedure Write_Document_To_File
     (File_Name : String;
      Doc       : Document) is
      
      File        : Ada.Streams.Stream_Io.File_Type;
      File_Stream : Stream_Access;
   begin
      Ada.Streams.Stream_Io.Create
	(File => File, Mode => Out_File, Name => File_Name);
      File_Stream := Stream (File);
      
      Dom.Core.Nodes.Write
	(Stream                => File_Stream,
	 N                     => Doc,
	 Print_Comments        => True,
	 Print_XML_Declaration => True,
	 With_URI              => False,
	 Pretty_Print          => True,
	 EOL_Sequence          => "" & ASCII.LF,
	 Encoding              => Unicode.Encodings.Get_By_Name ("utf-8"),
	 Collapse_Empty_Nodes  => True);
      
   end Write_Document_To_File;

end Artics.Xml_Utils;
