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

--  with Reflex.Infos; use Reflex.Infos;
with Reflex_Options; use Reflex_Options;
with Reflex.Entities_Lists; use Reflex.Entities_Lists;

package Reflex.Global_Arecs is

   type Global_Arec_Record is tagged private;
   type Global_Arec_Ptr is access all Global_Arec_Record;
   type Global_Arec_Class_Ptr is access all Global_Arec_Record'Class;

   No_Global_Arec_Record : constant Global_Arec_Record;
   
   Arec_Error_Entity : Entity_Id;
   
   procedure Create_Arec_Error_Enrity;
   
   -- Global Entities --
   ---------------------
   
   type Accessibility_Type is
     (No_Referenced,
      Read,
      Read_Write,
      Write);
   
   type Parameter_Convention_Type is
     (As_Reference,
      As_Actual,
      As_Temp,
      As_Record);
   
   type Global_Assoc_Record is record
      Original   : Entity_Id;
      New_Entity : Entity_Id;
      Access_Typ : Accessibility_Type;
      Parameter_Convention : Parameter_Convention_Type;
   end record;
   type Global_Assoc_Ptr is access all Global_Assoc_Record;

   No_Global_Assoc_Record : constant Global_Assoc_Record :=
     Global_Assoc_Record'(Empty, Empty, Read_Write, As_Reference);

   function Same_Global_Assoc (A1, A2 : Global_Assoc_Ptr) return Boolean;

   package Globals_Assoc_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Global_Assoc_Ptr);
   
   function New_Global_Assoc
     (E, New_E : Entity_Id) return Global_Assoc_Ptr;
   
   -- Called Entities --
   ---------------------
   
   type Called_Assoc_Record is record
      Entity  : Entity_Id;
      Instance_Name : Name_Id;
      Comp_Id : Entity_Id;
      Calls_Stmts : Reflex.Nodes_Lists.List;
   end record;
   type Called_Assoc_Ptr is access all Called_Assoc_Record;

   No_Called_Assoc_Record : constant Called_Assoc_Record :=
     Called_Assoc_Record'(Empty, No_Name, Empty, Reflex.Nodes_Lists.Empty_List);
   
   function Same_Called_Assoc (A1, A2 : Called_Assoc_Ptr) return Boolean;

   package Called_Assoc_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Called_Assoc_Ptr);
   
   function New_Called_Assoc
     (E    : Entity_Id;
      Inst : Name_Id) return Called_Assoc_Ptr;
   
   procedure Append_Call_Stmt
     (Assoc : Called_Assoc_Ptr;
      Call  : Node_Id);
   
   function New_Global_Arec return Global_Arec_Ptr;

   procedure Free_Global_Arec (This : in out Global_Arec_Ptr);
   
   function Get_Subprogram_Mode
     (E : Entity_Id) return Subprogram_Generation_Mode;

   procedure Set_Subprogram_Mode
     (E    : Entity_Id;
      Mode : Subprogram_Generation_Mode);

   function Get_Globals_List (E : Entity_Id) return Globals_Assoc_Lists.List;
   procedure Set_Globals_List
     (E       : Entity_Id;
      Globals : Globals_Assoc_Lists.List);

   function Get_Called_List (E : Entity_Id) return Called_Assoc_Lists.List;
   procedure Set_Called_List
     (E      : Entity_Id;
      Called : Called_Assoc_Lists.List);

   procedure Add_Globals
     (E             : Entity_Id;
      Global_Entity : Entity_Id);

   procedure Remove_Globals
     (E             : Entity_Id;
      Global_Entity : Entity_Id);

   procedure Add_Called_Entity
     (E      : Entity_Id;
      Called : Entity_Id;
      Call   : Node_Id);

   procedure Remove_Called_Entity
     (E      : Entity_Id;
      Called : Entity_Id);

   procedure Collect_Globals (E : Entity_Id);

   procedure Collect_Called (E : Entity_Id);
   
   procedure Create_Access_To_Record (E : Entity_Id);
   
   function Get_Arec_Full_Type (E : Entity_Id) return Entity_Id;
   procedure Set_Arec_Full_Type
     (E : Entity_Id;
      T : Entity_Id);
   --  Associate the AREC Full Type T to the Subprogram entity E.

   function Get_Arec_Variable (E : Entity_Id) return Entity_Id;
   procedure Set_Arec_Variable
     (E : Entity_Id;
      V : Entity_Id);
   --  Entity of the Global AREC type variable for this entity
   
   function Get_Arec_Extra_Formal (E : Entity_Id) return Entity_Id;
   procedure Set_Arec_Extra_Formal
     (E : Entity_Id;
      F : Entity_Id);
   --  Entity of the AREC variale of this entity
   
   procedure Create_Global_Arec_Entities (E : Entity_Id);
   procedure Populate_Arec_Entities (E : Entity_Id);
   
   function Need_Globals_Arec (E : Entity_Id) return Boolean;
   --  ATTTENTION RECURSION
   
   procedure Replace_Globals (E : Entity_Id);
   
   procedure Replace_Globals_By_Deference (E : Entity_Id);
   
   procedure Replace_Globals_By_Arec (E : Entity_Id);

   procedure Replace_Formals_By_Globals (E : Entity_Id);
   
   procedure Create_Arec_Parameter_Type (E : Entity_Id);
   
   procedure Create_Arec_Instance (E : Entity_Id);

   procedure Create_Arec_Extra_Formal (E : Entity_Id);

   procedure Add_Extra_Formal
     (E  : Entity_Id;
      EF : Entity_Id);
   
   function Get_Instance_Name_In_Scope
     (Scp : Node_Id;
      E   : Entity_Id) return Name_Id;
   
   function Extra_Formal_Dfb_Count (E : Entity_Id) return Natural;
   
   function Get_Associated_Entity_For
     (E : Entity_Id;
      G : Entity_Id) return Entity_Id;
   
   procedure Create_Empty_Extra_Actual (Subprogram_Call : Node_Id);
   procedure Add_Extra_Actuals_To_Called (E : Entity_Id);
   procedure Add_Extra_Actual_To_Call
     (Subprogram_Call : Node_Id;
      Extra_Formal    : Entity_Id;
      Extra_Actual    : Node_Id);
     
private

   type Global_Arec_Record is tagged record
      Subprogram_Mode : Subprogram_Generation_Mode;
      --  The generation mode for this subprogram

      Globals : Globals_Assoc_Lists.List;
      --  Lists of globals used by this entity

      Called : Called_Assoc_Lists.List;
      --  List of the subprogram called by this entity

      Globals_Count : Natural;
      Called_Count : Natural;

      Arec_Full_Type : Entity_Id;
      --  Entity of the Global AREC type for this entity
      
      Arec_Variable : Entity_Id;
      --  Entity of the Global AREC variable for this entity
      
      Arec_Extra_Formal : Entity_Id;
      --  Extra_Formal added to the subprogram for the AREC Type
   end record;

   No_Global_Arec_Record : constant Global_Arec_Record :=
     Global_Arec_Record'
     (Subprogram_Mode    => Unknown,
      Globals            => Globals_Assoc_Lists.Empty_List,
      Called             => Called_Assoc_Lists.Empty_List,
      Globals_Count      => 0,
      Called_Count       => 0,
      Arec_Full_Type     => Empty,
      Arec_Variable      => Empty,
      Arec_Extra_Formal  => Empty);
   
   Max_Arec_Stack_Entry : Natural := 1000;
   type Arec_Call_Stack_Array is 
     array (Natural range 1..Max_Arec_Stack_Entry) of Entity_Id;
   
   Arec_Call_Stack : Arec_Call_Stack_Array;
   Last_Call_Stack : Natural := 0;
   
   procedure Push_Call_Stack (E : Entity_Id);
   procedure Pop_Call_Stack;
   
end Reflex.Global_Arecs;
