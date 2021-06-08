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
with Namet; use Namet;

with Artics.Strings_Stocks; use Artics.Strings_Stocks;

with Reflex.Entities_Lists; use Reflex.Entities_Lists;

package Reflex.Infos is

   type Reflex_Infos_Record is tagged private;
   type Reflex_Infos_Ptr is access all Reflex_Infos_Record;
   type Reflex_Infos_Class_Ptr is access all Reflex_Infos_Record'Class;

   No_Reflex_Infos_Record : constant Reflex_Infos_Record;

   type Expr_With_Actions_Record is record
      Decls           : List_Id;
      Actions         : List_Id;
      Replace_Point   : Node_Id;
      Insertion_Point : Node_Id;
   end record;

   No_Expr_With_Actions_Record : constant Expr_With_Actions_Record :=
     (Decls           => No_List,
      Actions         => No_List,
      Replace_Point   => Empty,
      Insertion_Point => Empty);

   procedure Initialize_Reflex_Infos;
   --  Initialize the package

   function New_Reflex_Infos return Reflex_Infos_Ptr;
   function New_Reflex_Infos (Node : Node_Id) return Reflex_Infos_Ptr;

   procedure Free_Reflex_Infos (This : in out Reflex_Infos_Ptr);
   procedure Free_Reflex_Infos (Node : Node_Id);

   function Get_Reflex_Infos
     (Node : Node_Or_Entity_Id) return access Reflex_Infos_Record;

   procedure Set_Reflex_Infos
     (Node  : Node_Or_Entity_Id;
      Infos : access Reflex_Infos_Record);
   --  The node is already expanded

   function Is_Expanded (Node : Node_Id) return Boolean;
   procedure Set_Expanded
     (Node : Node_Id;
      V    : Boolean);
   --  The node is already expanded

   function Is_Generated (Node : Node_Id) return Boolean;
   procedure Set_Generated
     (Node : Node_Id;
      V    : Boolean);
   --  The node has been generated

   function Is_Covered (Node : Node_Id) return Boolean;
   procedure Set_Covered
     (Node : Node_Id;
      V    : Boolean);
   --  The node is already covered

   function Is_Expansion_Pending (Node : Node_Id) return Boolean;
   procedure Set_Expansion_Pending
     (Node : Node_Id;
      V    : Boolean);

   function Is_Generation_Pending (Node : Node_Id) return Boolean;
   procedure Set_Generation_Pending
     (Node : Node_Id;
      V    : Boolean);

   function Is_Coverage_Pending (Node : Node_Id) return Boolean;
   procedure Set_Coverage_Pending
     (Node : Node_Id;
      V    : Boolean);

   function Is_Homonym_Done (E : Entity_Id) return Boolean;
   procedure Set_Homonym_Done
     (E : Entity_Id;
      V : Boolean);
   --  Remaning of Homonyms done for this entity

   function Get_Enumeration_Literal_Constant (E : Entity_Id) return Entity_Id;
   procedure Set_Enumeration_Literal_Constant
     (E        : Entity_Id;
      List_Cst : Entity_Id);

   function Is_Anonym (E : Entity_Id) return Boolean;
   procedure Set_Is_Anonym
     (E : Entity_Id;
      V : Boolean);

private

   type Reflex_Infos_Record is tagged record
      Expanded : Boolean;
      --  The node is already expanded

      Generated : Boolean;
      --  The node has been generated

      Covered : Boolean;
      --  The node is already covered

      Expansion_Pending : Boolean;
      --  The entity is being expanded

      Generation_Pending : Boolean;
      --  The entity is being generated

      Coverage_Pending : Boolean;
      --  The entity is being expanded

      Homonym_Done : Boolean;
      --  Remaning of Homonyms done for this entity

      No_Generate : Boolean;

      --  Emumaration Literals --
      ---------------------------

      Enum_Literal_Constant : Entity_Id;

      Anonym : Boolean;

      Expr_With_Actions : Expr_With_Actions_Record;
   end record;

   No_Reflex_Infos_Record : constant Reflex_Infos_Record :=
     Reflex_Infos_Record'
       (Expanded              => False,
        Generated             => False,
        Covered               => False,
        Expansion_Pending     => False,
        Generation_Pending    => False,
        Coverage_Pending      => False,
        Homonym_Done          => False,
	No_Generate           => False,
        Enum_Literal_Constant => Empty,
        Anonym                => False,
        Expr_With_Actions     => No_Expr_With_Actions_Record);

end Reflex.Infos;
