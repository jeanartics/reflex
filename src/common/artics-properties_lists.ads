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

with Ada.Unchecked_Deallocation;

with Artics.Types; use Artics.Types;
with Artics.Types; use Artics.Types;
with Artics.Namet; use Artics.Namet;
with Artics.Containers.Sets;

pragma Elaborate(Artics.Containers.Sets);

package Artics.Properties_Lists is

   type Property is private;
   type Properties_List is private;

   type Property_Data_Record is tagged null record;
   type Property_Data is access all Property_Data_Record'Class;

   No_Data : aliased Property_Data_Record;
   No_Property_Data : constant Property_Data := No_Data'access;
   -- This is returned when Property is found but doesn't contain anything

   Null_Property_Data : constant Property_Data := null;
   -- This is used when Property doesn't exist

   procedure Free is new Ada.Unchecked_Deallocation
     (Property_Data_Record'Class, Property_Data);

   type Integer_Data_Record is new Property_Data_Record with record
      Value : Integer;
   end record;
   type Integer_Data is access all Integer_Data_Record;

   procedure Insert_Property
     (L : in Properties_List;
      P : in Name_Id;
      V : in Property_Data);

   procedure Insert_Property
     (L : in Properties_List;
      P : in String;
      V : in Property_Data);

   function Read_Property
     (L : in Properties_List;
      P : in Name_Id) return Property_Data;

   function Read_Property
     (L : in Properties_List;
      P : in String) return Property_Data;

   procedure Delete_Property
     (L : in Properties_List;
      P : in Name_Id;
      F : in Boolean := True);
   procedure Delete_Property
     (L : in Properties_List;
      P : in String;
      F : in Boolean := True);
   -- F : if true, free up content property.

   function Is_Property
     (L : in Properties_List;
      P : in Name_Id) return Boolean;

   procedure Merge_Properties
     (Into : in Properties_List;
      List : in out Properties_List);

   function Empty return Properties_List;

   procedure Delete_Properties (L : in out Properties_List);

   type Property_Iterator is private;

   procedure Reset (It : in out Property_Iterator);

   procedure Next (It : in out Property_Iterator);

   function Is_End (It: in Property_Iterator) return Boolean;

   function Current_Item (It: in Property_Iterator) return Property;

  function New_Property_Iterator
     (L : in Properties_List) return Property_Iterator;

   function Get_Property_Name (P : in Property) return Name_Id;

   function Get_Property_Data (P : in Property) return Property_Data;

   No_Properties : constant Properties_List;
private

   function "=" (P1 : Property; P2: Property) return Boolean;

   type Property is record
      Name : Name_Id;
      Data : Property_Data;
   end record;

   Null_Property : Property := Property'(No_Name, No_Property_Data);

   package Priv_Properties_Sets is new Containers.Sets
     (Item    => Property,
      No_Item => Null_Property,
      "="     => "=");
   use Priv_Properties_Sets;

   type Properties_List is new Priv_Properties_Sets.Set_Id;

   No_Properties : constant Properties_List :=
     Properties_List (Priv_Properties_Sets.No_Set);

   type Property_Iterator is record
      It : Priv_Properties_Sets.Set_Iterator;
   end record;

end Artics.Properties_Lists;

