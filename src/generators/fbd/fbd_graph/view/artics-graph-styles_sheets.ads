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
-- Reflex is originally developed  by the Artics team at Grenoble (France). --
--                                                                          --
------------------------------------------------------------------------------

with Artics.Types; use Artics.Types;
with Artics.Namet; use Artics.Namet;

with Ada.Strings.Hash;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;

with Artics.Objects; use Artics.Objects;
with Artics.Utils; use Artics.Utils;

-- Defines the appearance of the cells in a graph. The following example 
-- changes the font size for all vertices by changing the default vertex style
-- in-place:
-- <code>
-- getDefaultVertexStyle().put(mxConstants.STYLE_FONTSIZE, 16);
-- </code>
-- To change the default font size for all cells, set DEFAULT_FONTSIZE.

package Artics.Graph.Styles_Sheets is

   type Style_Sheet_Record is new Object_Record with private;
   type Style_Sheet_Ptr is access all Style_Sheet_Record'Class;
   
   No_Style_Sheet_Record : constant Style_Sheet_Record;
   
   function Sheet_Equivalent_Key (Left, Right : Name_Id) return Boolean;
 
   function Sheet_Hash_Func (Key : Name_Id) return Ada.Containers.Hash_Type;
   
   function Equals (Left, Right : Strings_Maps.Map) return Boolean;
   
   package Stylenames_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Name_Id,
      Element_Type    => Strings_Maps.Map,
      Hash            => Sheet_Hash_Func,
      Equivalent_Keys => Sheet_Equivalent_Key,
      "="             => Equals);
   
   Empty_Style : Strings_Maps.Map := Strings_Maps.Empty_Map;
   -- Shared immutable empty hashtable (for undefined cell styles).
   
   function New_Style_Sheet return access Style_Sheet_Record'Class;
   -- Constructs a new stylesheet and assigns default styles.
   
   function Get_Styles
     (S : access Style_Sheet_Record) return Stylenames_Maps.Map;
   -- Returns all styles as map of name, hashtable pairs.
   -- @return All styles in this stylesheet.

   procedure Set_Styles
     (S      : access Style_Sheet_Record;
      Styles : Stylenames_Maps.Map);
   -- Sets all styles in the stylesheet.

   function Create_Default_Vertex_Style
     (S : access Style_Sheet_Record) return Strings_Maps.Map;
   -- Creates and returns the default vertex style.
   -- @return Returns the default vertex style.

   function Create_Default_Edge_Style
     (S : access Style_Sheet_Record) return Strings_Maps.Map;
   -- Creates and returns the default edge style.
   -- @return Returns the default edge style.

   function Get_Default_Vertex_Style 
     (S : access Style_Sheet_Record) return Strings_Maps.Map;
   -- Returns the default style for vertices.
   -- @return Returns the default vertex style.

   procedure Set_Default_Vertex_Style 
     (S     : access Style_Sheet_Record;
      Value : Strings_Maps.Map);
   -- Sets the default style for vertices.
   -- @param value Style to be used for vertices.

   function Get_Default_Edge_Style
     (S : access Style_Sheet_Record) return Strings_Maps.Map;
   -- Returns the default style for edges.
   -- @return Returns the default edge style.

   procedure Set_Default_Edge_Style
     (S     : access Style_Sheet_Record;
      Value : Strings_Maps.Map);
   -- Sets the default style for edges.
   -- @param value Style to be used for edges.

   procedure Put_Cell_Style
     (S     : access Style_Sheet_Record;
      Name  : String;
      Style : Strings_Maps.Map);
   procedure Put_Cell_Style
     (S     : access Style_Sheet_Record;
      Name  : Name_Id;
      Style : Strings_Maps.Map);
   -- Stores the specified style under the given name.
   -- @param name Name for the style to be stored.
   -- @param style Key, value pairs that define the style.
   
   procedure Update_Cell_Style
     (S          : access Style_Sheet_Record;
      Style_Name : String;
      Key        : Name_Id;
      Value      : Name_Id);
   procedure Update_Cell_Style
     (S          : access Style_Sheet_Record;
      Style_Name : String;
      Key        : String;
      Value      : String);
   procedure Update_Cell_Style
     (S          : access Style_Sheet_Record;
      Style_Name : Name_Id;
      Key        : Name_Id;
      Value      : Name_Id);
   -- Update the value of key in the style Setyle_Name. If style does not exist
   -- it is created.
   
 
   Function Clone_Style_From_Name 
     (S          : access Style_Sheet_Record;
      Style_Name : String) return Strings_Maps.Map;
  
   function Get_Style_Key_Value
     (S          : access Style_Sheet_Record;
      Style_Name : Name_Id;
      Key        : Name_Id;
      Value      : Name_Id) return Name_Id;
   -- Return the value of key in style_name, no_name if either style_name or
   -- key does not not exist
   
   function Get_Cell_Style
     (S             : access Style_Sheet_Record;
      Name          : String;
      Default_Style : Strings_Maps.Map) return Strings_Maps.Map;
   function Get_Cell_Style
     (S             : access Style_Sheet_Record;
      Name          : Name_Id;
      Default_Style : Strings_Maps.Map) return Strings_Maps.Map;
   -- Returns the cell style for the specified cell or the given defaultStyle
   -- if no style can be found for the given stylename.
   -- @param name String of the form [(stylename|key=value);] that represents 
   -- the style.
   -- @param defaultStyle Default style to be returned if no style can be found.
   -- @return Returns the style for the given formatted cell style.
   
private
   
   type Style_Sheet_Record is new Object_Record with record
      Styles : Stylenames_Maps.Map;
      -- Maps from names to styles.
   end record;
   
   No_Style_Sheet_Record : constant Style_Sheet_Record :=
     Style_Sheet_Record'
     (No_Object_Record with 
      Styles        => Stylenames_Maps.Empty_Map);
   
end Artics.Graph.Styles_Sheets;
	  
