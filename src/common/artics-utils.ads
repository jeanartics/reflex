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
with Artics.Objects; use Artics.Objects;

with Ada.Strings.Hash;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Artics.Utils is
   
   function Equivalent_Key (Left, Right : Name_Id) return Boolean;
   function Hash_Func (Key : Name_Id) return Ada.Containers.Hash_Type;
   package Strings_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Name_Id,
      Element_Type    => Name_Id,
      Hash            => Hash_Func,
      Equivalent_Keys => Equivalent_Key);
   
   package Strings_lists is 
     new Ada.Containers.Doubly_Linked_Lists (Name_Id);
   
   procedure Copy_Strings_Map
     (From : Strings_Maps.Map;
      To   : in out Strings_Maps.Map);
   -- Compy Element from the Map From to the Map To. From is not modified.
   
   procedure Copy_Replace_Strings_Map
     (From : Strings_Maps.Map;
      To   : in out Strings_Maps.Map);
   
   function Split (Str : String; 
                   Sep : Character) return Strings_lists.List;
   
   function Join (L   : Strings_Lists.List; 
                  Sep : Character) return String;
   
   function Join (L   : Strings_Lists.List; 
                  Sep : String := "") return String;
   
   function Starts_With 
     (N : Name_Id;
      S : String) return Boolean;
	  
   function Starts_With 
     (N : String;
      S : String) return Boolean;
   -- returns true if N contains S
   -- returns true also if S is empty string or both N & S are empty strings
   
   function Contains 
     (N : String;
      S : String) return Boolean;
   
   function Index_Of 
     (N : Name_Id;
      S : String) return Natural;
   
   function Index_Of 
     (N : String;
      S : String) return Natural;
   
--     function Replace_All
--       (S       : String;
--        Search  : String; 
--        Replace : String) return String;
   
--   function To_Lower (S  : String) return String;
   
   function Float_From_String   (Value : String)  return Float;
   function Float_From_Name     (Value : Name_Id) return Float;
   function Integer_From_String (Value : String)  return Integer;
   function Integer_From_Name   (Value : Name_Id) return Integer;
   function Boolean_From_String (Value : String)  return Boolean;
   function Boolean_From_Name   (Value : Name_Id) return Boolean;
      
   function Float_To_String   (Value : Float)   return String;
   function Float_To_Name     (Value : Float)   return Name_Id;
   function Float_To_Fixed_String (Value : Float; Dec : Integer) return String;
   function Word_To_String    (Value : Word) return String;
   function Word_To_Name      (Value : Word) return Name_Id;
   function Integer_To_String (Value : Integer) return String;
   function Integer_To_Name   (Value : Integer) return Name_Id;
   function Long_Integer_To_String (Value : Long_Integer) return String;
   function Long_Integer_To_Name (Value : Long_Integer) return Name_Id;
   function Boolean_To_String (Value : Boolean) return String;
   function Boolean_To_Name   (Value : Boolean) return Name_Id;
   
   function Hex_Integer_From_String (Value : String) return Integer;
   function Hex_Integer_From_Name   (Value : Name_Id) return Integer;
   function Hex_Integer_To_String (Value : Integer) return String;
   function Hex_Integer_To_String (Value : Integer) return Name_Id;
   
   function Epsilon_Equal
     (V1 : Float;
      V2 : Float) return Boolean;
   function Epsilon_Sup
     (V1 : Float;
      V2 : Float) return Boolean;
   function Epsilon_Inf
     (V1 : Float;
      V2 : Float) return Boolean;
   function Epsilon_Sup_Equal
     (V1 : Float;
      V2 : Float) return Boolean;
   function Epsilon_Inf_Equal
     (V1 : Float;
      V2 : Float) return Boolean;
   
end Artics.Utils;
