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

with Artics.Strings_Stocks; use Artics.Strings_Stocks;
with Types; use Types;
with Namet; use Namet;

--  Entities in the vars file are specified with its full names, example
--  entity E of package P1.P2 is noted "p1.p2.e". To search the entity
--  we firts search in Unit Table the entry corresponding to the unit P1
--  Then we search for unit P1.P2, once found, we retreive the entity of
--  P1.P2 with Cunit_Enity function. Finally we walk throught the entoty 
--  chain of P1.P2 to found the entity E
   

package Reflex.Configs.Entities_Renames is
   
   type Command_Type is
     (Unknown,
      Replace,
      Add,
      Remove,
      Translate_Address);
      
   type Name_Command_Record is private;
   type Name_Command_Ptr is access all Name_Command_Record;
   
   function Equivalent_Key
     (Left, Right : Name_Id) return Boolean;
   
   function Hash_Func
     (Key : Name_Id) return Ada.Containers.Hash_Type;
   
   package New_Names is new Ada.Containers.Hashed_Maps
     (Key_Type        => Name_Id,
      Element_Type    => Boolean,
      --  Element_Type    => Name_Command_Ptr,
      Hash            => Hash_Func,
      Equivalent_Keys => Equivalent_Key);
   use New_Names;
   
   New_Names_Reg : New_Names.Map;
   
   function Get_Entity_Name (S : String) return Name_Id;
      
   function Get_Prefix_Name (S : String) return Name_Id;
   
   function To_Unit_Name (S : String) return String;
   function Get_Unit_Entity (Pref : Name_Id) return Entity_Id;
   function Get_Unit_Entity (Pref : String) return Entity_Id;

   function Find_Entity_In_Unit
     (Eunit        : Entity_Id;
      Prefix_Name  : Name_Id;
      Entity_Name  : Name_Id;
      Current_Line : Natural := 0) return Entity_Id;
   
   function Find_Entity 
     (Unit_Name    : String;
      Full_Name    : String;
      Current_Line : Natural := 0) return Entity_Id;
   
   function Find_Entity
     (Unit_Name        : Name_Id;
      Full_Entity_Name : Name_Id;
      Current_Line     : Natural := 0) return Entity_Id;
   
   procedure Register_New_Entity_Name
     (Unit_Name        : Str_Id;
      Full_Entity_Name : Str_Id;
      New_Name         : Str_Id;
      New_Comment      : Str_Id;
      New_Addr         : Str_Id;
      Current_Line     : Natural);
   
   procedure Register_New_Subprogram
     (Unit_Name        : Str_Id;
      Full_Entity_Name : Str_Id;
      New_Name         : Str_Id;
      New_Comment      : Str_Id;
      Gen_Type         : Str_Id;
      Lang             : Str_Id;
      Current_Line     : Natural);
   
   function New_Name_Command
     (E   : Entity_Id;
      Cmd : Command_Type) return Name_Command_Ptr;
   
   function Get_Entity (This : Name_Command_Ptr) return Entity_Id;
   procedure Set_Entity
     (This : Name_Command_Ptr;
      E    : Entity_Id);
   
   function Get_Command (This : Name_Command_Ptr) return Command_Type;
   procedure Set_Command
     (This : Name_Command_Ptr;
      Cmd  : Command_Type);
   
   function Contains (This : Name_Command_Ptr) return Boolean;
   
private
   
   type Name_Command_Record is record
      E       : Entity_Id;
      Command : Command_Type;
   end record;
   
   No_Name_Commd_Record : constant Name_Command_Record := 
     (E       => Empty,
      Command => Unknown);
   
end Reflex.Configs.Entities_Renames;
