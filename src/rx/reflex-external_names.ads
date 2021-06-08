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

package Reflex.External_Names is

   Max_Name_Length : Positive := 32;

   Aggregate_Prefix : String := "_a";
   Array_Prefix     : String := "_r";
   Variable_Prefix  : String := "_v";
   Label_Prefix     : String := "_l";
   Type_Prefix      : String := "_t";
   Local_Prefix     : String := "_l";
   Global_Prefix    : String := "_g";
   Procedure_Prefix : String := "_p";
   Function_Prefix  : String := "_f";
   Parameter_Prefix : String := "_q";

   function Is_Name_Internal (S : String) return Boolean;

   function New_Internal_Name return Name_Id;

   function New_Ghost_Name return Name_Id;

   function New_Aggregate_Name (S : String) return Name_Id;
   function New_Aggregate_Name (Name : Name_Id) return Name_Id;

   function New_Array_Name (S : String) return Name_Id;
   function New_Array_Name (Name : Name_Id) return Name_Id;

   function New_Variable_Name (S : String) return Name_Id;
   function New_Variable_Name (Name : Name_Id) return Name_Id;

   function New_Label_Name (S : String) return Name_Id;
   function New_Label_Name (Name : Name_Id) return Name_Id;

   function New_Type_Name (S : String) return Name_Id;
   function New_Type_Name (Name : Name_Id) return Name_Id;

   function New_Local_Name (S : String) return Name_Id;
   function New_Local_Name (Name : Name_Id) return Name_Id;

   function New_Global_Name    (S : String) return Name_Id;
   function New_Global_Name (Name : Name_Id) return Name_Id;

   function New_Procedure_Name (S : String) return Name_Id;
   function New_Procedure_Name (Name : Name_Id) return Name_Id;

   function New_Function_Name  (S : String) return Name_Id;
   function New_Function_Name (Name : Name_Id) return Name_Id;

private
   function Equivalent_Key
     (Left, Right : Name_Id) return Boolean;

   function Hash_Func
     (Key : Name_Id) return Ada.Containers.Hash_Type;

   package Names_Hash is new Ada.Containers.Hashed_Maps
     (Key_Type        => Name_Id,
      Element_Type    => Boolean,
      Hash            => Hash_Func,
      Equivalent_Keys => Equivalent_Key);

   Aggregate_Names : Names_Hash.Map;
   Array_Names     : Names_Hash.Map;
   Variable_Names  : Names_Hash.Map;
   Label_Names     : Names_Hash.Map;
   Type_Names      : Names_Hash.Map;
   Local_Names     : Names_Hash.Map;
   Global_Names    : Names_Hash.Map;
   Procedure_Names : Names_Hash.Map;
   Function_Names  : Names_Hash.Map;

   Internal_Count : Natural := 1;
   Ghost_Count    : Natural := 1;

end Reflex.External_Names;
