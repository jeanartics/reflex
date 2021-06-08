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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;

with Artics.Name_Stock;
with Types; use Types;

package Reflex.Configs.Vars is
   
   --  Each library level entity has an entries which describe all needed
   --  attributes to generate it for particular target. A map do the link 
   --  between an entity and its attributes. An attribute has a name and 
   --  a vlaue. The value is either a number a boolean value or a string 
   
   --  Attributes are entered with the 
   --   pragma Annotate (Reflex, Attr1, Val1.., Entity_Full_Name);
   --  Or using a config file, csv delimited or xml. In a csv file, all
   --  Atributes of an entity are in a line, each attribute value separeted
   --  by the delimiter ';'
   
   --  Attributes are of two types. Attributes valid for all targets and
   --  Attributes specific to a platform. A list of Known attributes is
   --  defined hereunder.
   
   --  Vars --
   -----------
   
   --  The vars attributes are the same for all plateforms. For a csv file,
   --  the format is :
   --  Current_Entity_Full_Name, Global_Name, Address, Comment
   
   --  Exemple :
   --  P1.V1;type;V1;;"Comment of variable V1 of package P1"
   --  P1.V2;type;V2;%MW100;"Comment of variable V2 of package P1"
   --  P2.X1;type;X;%MW200;"Comment of variable X of package P2V"
   
   --  The field type is only informative and is not changed by reflex.
   
   -- Subprograms --
   -----------------
   
   procedure Parse_Vars (File_Name : String);
   
   function Validate_Unit_Name (Name : String) return Boolean;
   
   function Validate_Entity_Name (Name : String) return Boolean;
   
   function Validate_Entity_Address (Name : String) return Boolean;
   
   function Validate_Entity_Type (Name : String) return Boolean;
   
end Reflex.Configs.Vars;
