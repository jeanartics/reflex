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

with Types; use Types;
with namet; use Namet;

with Artics.Strings_Stocks; use Artics.Strings_Stocks;

package Reflex.Subps_Infos is
   
   type Subprogram_Infos_Record is new Entity_Infos_Record with private;
   type Subprogram_Infos_Ptr is access all Subprogram_Infos_Record;
   type Subprogram_Infos_Class_Ptr is access all Subprogram_Infos_Record'Class;

   No_Subprogram_Infos_Record : constant Subprogram_Infos_Record;
   
   type Generation_Type is
     (Unknown_Generation,
      Section_Type,
      Sr_Type,
      Fb_Type);
   
   Section_Type_Str : Str_Id; --  "section"
   Sr_Type_Str      : Str_Id; --  "sr"
   Fb_Type_Str      : Str_Id; --  "fbd"
   
   type Language_Type is
     (Unknown_Language,
      Literal_Language,
      Ladder_Language,
      Flow_Language,
      Chart_Language);
   
   Literal_Language_Str : Str_Id; --  "literal"
   Ladder_Language_Str  : Str_Id; --  "ladder"
   Flow_Language_Str    : Str_Id; --  "flow"
   Chart_Language_Str   : Str_Id; --  "chart"
   
   function String_From_Language_Type (Lang : Language_Type) return String;
   function String_To_Language_Type   (S    : String) return Language_Type;
   function Str_From_Language_Type    (Lang : Language_Type) return Str_Id;
   function Str_To_Language_Type      (Str  : Str_Id) return Language_Type;
   
   function String_From_Generation_Type (Gen : Generation_Type) return String;
   function String_To_Generation_Type   (S   : String) return Generation_Type;
   function Str_From_Generation_Type    (Gen : Generation_Type) return Str_Id;
   function Str_To_Generation_Type      (Str : Str_Id) return Gneration_Type;
   
   function New_Subprogram_Infos return Subprogram_Infos_Ptr;
   function New_Subprogram_Infos (Node : Node_Id) return Subprogram_Infos_Ptr;
   
   procedure Free_Subprogram_Infos (This : in out Subprogram_Infos_Ptr);
   procedure Free_Subprogram_Infos (Node : Node_Id);
   
   function Get_Subprogram_Infos
     (Node : Node_Or_Entity_Id) return access Subprogram_Infos_Record;
   procedure Set_Subprogram_Infos
     (Node  : Node_Or_Entity_Id;
      Infos : access Subprogram_Infos_Record);
   --  The node is already expanded
      
   function Get_Language (E : Entity_Id) return Language_Type;
   procedure Set_Language
     (E    : Entity_Id; 
      Lang : Language_Type);
   
   function Get_Generation_Type (E : Entity_Id) return Generation_Type;
   procedure Set_Generation_Type
     (E   : Entity_Id; 
      Gen : Generation_Type);
   
   function Get_Subprogram_Instance (E : Entity_Id) return Entity_Id;
   procedure Set_Subprogram_Instance
     (E    : Entity_Id;
      Inst : Entity_Id);
   
private
   
   type Subprogram_Infos_Record is Entity_Infos_Record with record
     Instance : Entity_Id;
     --  Instance to used when generating in a Fb, Empty if no need for an
     --  instance for the subprogram
     
     Gen_Type : Generation_Type;
     --  Where the subprogram is generated, in a section, ina ser ..
     
     Lang : Language_Type;
      --  Le language used to generate the subprogram
   end record;
   
   No_Subprogram_Infos_Record : constant Subprogram_Infos_Record :=
     Subprogram_Infos_Record'
     (Instance => Empty,
      Gen_Type => Unknown_Generation,
      Lang     => Unknown_Language);
      
end Reflex.Subps_Infos;
