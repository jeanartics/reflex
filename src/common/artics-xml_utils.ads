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

with Artics.Types; use Artics.Types;
with Artics.Namet; use Artics.Namet;

with Dom; use Dom;
with DOM.Core; use DOM.Core;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Documents; use DOM.Core.Documents;

with Artics.Geometry; use Artics.Geometry;

package Artics.Xml_Utils is
   
   procedure Append_Child
     (Parent : Dom.Core.Node;
      Child  : Dom.Core.Node);
   
   procedure Set_Attribute
     (Elem  : Dom.Core.Element;
      Name  : String;
      Value : String);
   
   procedure Set_Attribute
     (Elem  : Dom.Core.Element;
      Name  : String;
      Value : Name_Id);
   
   procedure Set_Attribute
     (Elem  : Dom.Core.Element;
      Name  : Name_Id;
      Value : Name_Id);
   
   procedure Set_Attribute
     (Elem  : Dom.Core.Element;
      Name  : Name_Id;
      Value : Float);
   procedure Set_Attribute
     (Elem  : Dom.Core.Element;
      Name  : String;
      Value : Float);
   
   procedure Set_Attribute
     (Elem  : Dom.Core.Element;
      Name  : Name_Id;
      Value : Integer);
   procedure Set_Attribute
     (Elem  : Dom.Core.Element;
      Name  : String;
      Value : Integer);
   
   procedure Set_Attribute
     (Elem  : Dom.Core.Element;
      Name  : String;
      Value : Boolean);
   procedure Set_Attribute
     (Elem  : Dom.Core.Element;
      Name  : Name_Id;
      Value : Boolean);
   
   function Get_Attribute
     (Elem : Dom.Core.Element;
      Name : String) return String;
   
   function Get_Attribute
     (Elem : Dom.Core.Element;
      Name : Name_Id) return String;
   
   procedure Write_Document_To_File
     (File_Name : String;
      Doc       : Document);
   
end Artics.Xml_Utils;
