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
with Artics.Graph.Cells;    use Artics.Graph.Cells;

with Reflex.Entities_Lists; use Reflex.Entities_Lists;
with Reflex.Global_Arecs; use Reflex.Global_Arecs;
with Reflex.Boxes; use Reflex.Boxes;
with Artics.Graph.Cells;  use Artics.Graph.Cells;

package Reflex.Infos is

   type Reflex_Infos_Record is tagged private;
   type Reflex_Infos_Ptr is access all Reflex_Infos_Record;
   type Reflex_Infos_Class_Ptr is access all Reflex_Infos_Record'Class;

   No_Reflex_Infos_Record : constant Reflex_Infos_Record;

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

   function String_From_Language_Type (Lang : Language_Type) return String;
   function String_To_Language_Type   (S    : String) return Language_Type;
   function Str_From_Language_Type    (Lang : Language_Type) return Str_Id;
   function Str_To_Language_Type      (Str  : Str_Id) return Language_Type;

   function String_From_Generation_Type (Gen : Generation_Type) return String;
   function String_To_Generation_Type   (S   : String) return Generation_Type;
   function Str_From_Generation_Type    (Gen : Generation_Type) return Str_Id;
   function Str_To_Generation_Type      (Str : Str_Id) return Generation_Type;

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

   function Is_Negated_Assignment (Node : Node_Id) return Boolean;
   procedure Set_Negated_Assignment
     (Node : Node_Id;
      V    : Boolean);

   function Can_Be_Renamed (E : Entity_Id) return Boolean;

   function Get_New_Name (E : Entity_Id) return Name_Id;
   procedure Set_Entity_New_Name
     (E        : Entity_Id;
      New_Name : Name_Id);

   function Get_Entity_Address (E : Entity_Id) return Str_Id;
   procedure Set_Entity_Address
     (E    : Entity_Id;
      Addr : Str_Id);

   function Get_Entity_Comment (E : Entity_Id) return Str_Id;
   procedure Set_Entity_Comment
     (E       : Entity_Id;
      Comment : Str_Id);

   function Is_Extra_Access_Type (N : Node_Id) return Boolean;
   procedure Set_Extra_Access_Type
     (N : Node_Id;
      V : Boolean);

   function Access_To_Object_Defined (E : Entity_Id) return Entity_Id;
   procedure Set_Access_To_Object_Defined
     (E  : Entity_Id;
      Ac : Entity_Id);

   function Get_Object_Id (N : Node_Id) return Integer;
   procedure Set_Object_Id
     (N : Node_Id;
      V : Integer);

   function Get_Object_Id_String (N : Node_Id) return Integer;

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

   function Get_Function_Result_Formal (E : Entity_Id) return Entity_Id;

   procedure Set_Function_Result_Formal
     (E   : Entity_Id;
      Res : Entity_Id);

   function Get_Internal_Function (E : Entity_Id) return Boolean;

   procedure Set_Internal_Function
     (E : Entity_Id;
      V : Boolean);

   function Get_Subprogram_Global_Arec
     (E : Entity_Id) return access Global_Arec_Record;
   procedure Set_Subprogram_Global_Arec
     (E : Entity_Id;
      G : access Global_Arec_Record);

   function Declare_Global_Arec (E : Entity_Id) return Boolean;
   procedure Set_Declare_Global_Arec
     (E : Entity_Id;
      V : Boolean);

   function Pending_Arec (E : Entity_Id) return Boolean;
   procedure Set_Pending_Arec
     (E : Entity_Id;
      V : Boolean);

   function Get_Formal_Global (Formal : Entity_Id) return Entity_Id;
   procedure Set_Formal_Global
     (Formal : Entity_Id;
      Global : Entity_Id);

   function Get_Pin_Position (E : Entity_Id) return Natural;
   procedure Set_Pin_Position
     (E : Entity_Id;
      P : Natural);

   function Is_Entity_Reusable (E : Entity_Id) return Boolean;
   procedure Set_Entity_Reusable
     (E : Entity_Id;
      V : Boolean);

   function Reflex_No_Generate (E : Entity_Id) return Boolean;
   procedure Set_Reflex_No_Generate
     (E : Entity_Id;
      V : Boolean);

   function Has_Type_For_Generation (E : Entity_Id) return Boolean;
   function Get_Type_For_Generation (E : Entity_Id) return Entity_Id;
   procedure Set_Type_For_Generation
     (E : Entity_Id;
      T : Entity_Id);

   function Has_Init_Record (E : Entity_Id) return Boolean;
   function Get_Init_Record (E : Entity_Id) return Node_Id;
   procedure Set_Init_Record
     (E    : Entity_Id;
      Init : Node_Id);

   function Entity_In_Use (E : Entity_Id) return Boolean;
   procedure Set_Entity_In_Use
     (E : Entity_Id;
      V : Boolean);

   function Get_Enumeration_Literal_Constant (E : Entity_Id) return Entity_Id;
   procedure Set_Enumeration_Literal_Constant
     (E        : Entity_Id;
      List_Cst : Entity_Id);

   function Is_Anonym (E : Entity_Id) return Boolean;
   procedure Set_Is_Anonym
     (E : Entity_Id;
      V : Boolean);

   function Get_Box (N : Node_Id) return access Box_Record'Class;
   procedure Set_Box
     (N : Node_Id;
      B : access Box_Record'Class);

   function Get_Cell (N : Node_Id) return access Cell_Record;
   procedure Set_Cell
     (N    : Node_Id;
      Cell : access Cell_Record);

   function Need_Init_Record (T : Entity_Id) return Boolean;
   procedure Create_Initialization_Record
     (N     : Node_Id;
      After : Node_id);
   function Component_Need_Initialization (Comp : Node_Id) return Boolean;
   function Initialize_Component (Comp : Node_Id) return Node_Id;
   procedure Append_Component_Initialization
     (Comp : Node_Id;
      Rec  : Node_id);

   function Get_Init_Decls (E : Entity_Id) return List_Id;
   procedure Set_Init_Decls
     (E     : Entity_Id;
      Decls : List_Id);
   procedure Append_Init_Decls
     (E    : Entity_Id;
      Decl : Node_Id);

   function Get_Init_Stmts (E : Entity_Id) return List_Id;
   procedure Set_Init_Stmts
     (E     : Entity_Id;
      Stmts : List_Id);
   procedure Append_Init_Stmts
     (E    : Entity_Id;
      Stmt : Node_Id);

--     function Get_Reuse_Boolean (E : Entity_Id) return Entity_Id;
--     function Reset_Reuse_Boolean
--       (E : Entity_Id;
--        B : Entity_Id);

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

      Negated_Assignment : Boolean;
      --  The assignment has been negated. Used in boxes exp_ch4

      Reusable : Boolean;
      In_Use : Boolean;

      No_Generate : Boolean;

      --  Types --
      ------------

      Init_Record : Node_Id;

      Type_For_Generation : Entity_Id;

      --  Emumaration Literals --
      ---------------------------

      Enum_Literal_Constant : Entity_Id;

      Anonym : Boolean;

      --  Vars --
      -----------

      New_Name : Name_Id;
      Addr     : Str_Id;
      Comment  : Str_Id;

      Access_To_Obj_Defined : Entity_Id;

      Extra_Access_Type : Boolean;

      Is_Global_Arec : Boolean;

      Object_Id : Integer;

      --  Procs --
      ------------

      Global_Arec : access Global_Arec_Record;

      Declare_Global_Arec : Boolean;
      Pending_Arec : Boolean;

      Function_Result_Formal : Node_Id;

      Internal_Function : Boolean;

      Instance : Entity_Id;
      --  Instance to used when generating in a Fb, Empty if no need for an
      --  instance for the subprogram

      Gen_Type : Generation_Type;
      --  Where the subprogram is generated, in a section, ina ser ..

      Lang : Language_Type;
      --  Le language used to generate the subprogram

      Formal_Global : Entity_Id;

      --  Globa_Arec in case of procedure

      Pin_Position : Natural;

      Box : access Box_Record'Class;

      Cell : access Cell_Record;

      --  Reusable boolean

      B1 : Entity_Id;
      B2 : Entity_Id;
      B3 : Entity_Id;
      B4 : Entity_Id;
      B5 : Entity_Id;

      B1_Used : Boolean;
      B2_Used : Boolean;
      B3_Used : Boolean;
      B4_Used : Boolean;
      B5_Used : Boolean;

      Init_Decls : List_Id;
      Init_Stmts : List_Id;

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
        Negated_Assignment    => False,
        Reusable              => False,
        In_Use                => False,
        No_Generate           => False,
        Init_Record           => Empty,
        Type_For_Generation   => Empty,
        Enum_Literal_Constant => Empty,
        Anonym                => False,
        New_Name              => No_Name,
        Addr                  => No_Str_Id,
        Comment               => No_Str_Id,
        Access_To_Obj_Defined => Empty,
        Extra_Access_Type     => False,
        Is_Global_Arec        => False,
        Object_Id             => 0,
        Global_Arec           => null,
        Declare_Global_Arec   => False,
        Pending_Arec          => False,
        Function_Result_Formal => Empty,
        Internal_Function     => False,
        Instance              => Empty,
        Gen_Type              => Unknown_Generation,
        Lang                  => Unknown_Language,
        Formal_Global         => Empty,
        Pin_Position          => 0,
        Box                   => null,
        Cell                  => null,
        B1                    => Empty,
        B2                    => Empty,
        B3                    => Empty,
        B4                    => Empty,
        B5                    => Empty,
        B1_Used               => False,
        B2_Used               => False,
        B3_Used               => False,
        B4_Used               => False,
        B5_Used               => False,
        Init_Decls            => No_List,
        Init_Stmts            => No_List,
        Expr_With_Actions     => No_Expr_With_Actions_Record);

end Reflex.Infos;
