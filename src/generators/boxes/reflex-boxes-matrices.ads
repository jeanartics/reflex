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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation;

with Types;          use Types;
with Reflex_Options; use Reflex_Options;

package Reflex.Boxes.Matrices is
   
   package Matrices is new Artics.Generic_Matrices (Cell_Ptr, null);
   use Matrices;
   
   procedure Build_Matrix (B : access Box_Record'Class) return Matices is
   begin
      New_Matrix;
      
      Place_Matrix (B, Matrix);
   end Build_Matrix;
   
   type Box_Record is tagged private;
   type Box_Ptr is access all Box_Record;
   type Box_Class_Ptr is access all Box_Record'Class;
   
   No_Box_Record : constant Box_Record;
   
   type Box_Kind is
     (No_Box_Kind,
      Action_Box,
      Enclosing_Box,
      Dual_Box,
      Multi_Box,
      Terminal_Box,
      Coil_Box);
   
   type Orientation_Type is
     (Vertical,
      Horizontal);
   
   package Boxes_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Box_Class_Ptr);
   
   function New_Box return Box_Ptr;
   
   procedure Free_Box (This : in out Box_Ptr);
   
   function Get_Node (This : access Box_Record) return Node_Id;
   procedure Set_Node
     (This : access Box_Record;
      Node : Node_Id);
   
   function Get_Box_Kind (This : access Box_Record) return Box_Kind;
   procedure Set_Box_Kind
     (This : access Box_Record;
      Kind : Box_Kind);
   
   
   function Get_Parent_Box 
     (This : access Box_Record) return access Box_Record'Class;
   procedure Set_Parent_Box
     (This   : access Box_Record;
      Parent : access Box_Record'Class);
   
   function Get_X (This : access Box_Record) return Natural;
   procedure Set_X
     (This : access Box_Record;
      X    : Natural);
   
   function Get_Y (This : access Box_Record) return Natural;
   procedure Set_Y
     (This : access Box_Record;
      Y    : Natural);
   
   function Get_Xabs (This : access Box_Record) return Natural;
   procedure Set_Xabs
     (This : access Box_Record;
      X    : Natural);
   
   function Get_Yabs (This : access Box_Record) return Natural;
   procedure Set_Yabs
     (This : access Box_Record;
      Y    : Natural);
   
   function Get_Height (This : access Box_Record) return Natural;
   procedure Set_Height
     (This : access Box_Record;
      H    : Natural);
   
   function Get_Width (This : access Box_Record) return Natural;
   procedure Set_Width
     (This : access Box_Record;
      W    : Natural);
   
   procedure Place_Box (This : access Box_Record);
   procedure Absolute_Place_Box (This : access Box_Record);
   
   function Get_Orientation
     (This : access Box_Record) return Orientation_Type;
   procedure Set_Orientation
     (This   : access Box_Record;
      Orient : Orientation_Type);
   
   function Get_Is_Action_Box 
     (This : access Box_Record) return Boolean;
   procedure Set_Is_Action_Box
     (This : access Box_Record;
      Bool : Boolean);

private
   
   type Box_Record is tagged record
      Node : Node_Id;
      Kind : Box_Kind;
      Parent : access Box_Record'Class;
      Orientation : Orientation_Type;
      Is_Action_Box : Boolean;
      X : Natural;
      Y : Natural;
      Xabs : Natural;
      Yabs : Natural;
      H : Natural;
      W : Natural;
   end record;
   
   No_Box_Record : constant Box_Record :=
     (Node          => Empty,
      Kind          => No_Box_Kind,
      Parent        => null,
      Orientation   => Horizontal,
      Is_Action_Box => False,
      X             => 0,
      Y             => 0,
      Xabs          => 0,
      Yabs          => 0,
      H             => 0,
      W             => 0);
   
end Reflex.Boxes.Matrices;
