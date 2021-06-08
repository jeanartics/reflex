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

with Artics.Strings_Stocks; use Artics.Strings_Stocks;

package Reflex.Formats is

   function To_Lower (S  : String) return String;

   function Starts_With
     (N : Str_Id;
      S : String) return Boolean;

   function Starts_With
     (N : String;
      S : String) return Boolean;

   function Float_From_String   (Value : String)  return Float;
   function Float_From_Name     (Value : Str_Id) return Float;
   function Integer_From_String (Value : String)  return Integer;
   function Integer_From_Name   (Value : Str_Id) return Integer;
   function Boolean_From_String (Value : String)  return Boolean;
   function Boolean_From_Name   (Value : Str_Id) return Boolean;

   function Float_To_String   (Value : Float)   return String;
   function Float_To_Name     (Value : Float)   return Str_Id;
   function Float_To_Fixed_String (Value : Float; Dec : Integer) return String;
   function Integer_To_String (Value : Integer) return String;
   function Integer_To_Name   (Value : Integer) return Str_Id;
   function Long_Integer_To_String (Value : Long_Integer) return String;
   function Long_Integer_To_Name (Value : Long_Integer) return Str_Id;
   function Boolean_To_String (Value : Boolean) return String;
   function Boolean_To_Name   (Value : Boolean) return Str_Id;

   function Hex_Integer_From_String (Value : String) return Integer;
   function Hex_Integer_From_Name   (Value : Str_Id) return Integer;
   function Hex_Integer_To_String (Value : Integer) return String;
   function Hex_Integer_To_String (Value : Integer) return Str_Id;

   function Latin1_To_Xml (S : String) return String;
   function Xml_To_Latin1 (S : String) return String;
   function Decode_Xml_Entity (S : String) return Character;

end Reflex.Formats;
