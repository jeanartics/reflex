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

with Reflex_Options; use Reflex_Options;
with Artics.Generic_Matrices;

package Reflex.Boxes is
   
   type Box_Record is tagged private;
   type Box_Ptr is access all Box_Record;
   type Box_Class_Ptr is access all Box_Record'Class;
   
   No_Box_Record : constant Box_Record;
   
   The_True_Box  : constant access Box_Record;
   The_Empty_Box : constant access Box_Record;
   The_Hlink_Box : constant access Box_Record;
   The_Vlink_Box : constant access Box_Record;
   The_Hlink_Vlink_Box : constant access Box_Record;
   The_Busy_Box  : constant access Box_Record;
   
   type Box_Kind is
     (No_Box_Kind,
      Dual_Box,
      Multi_Box,
      Terminal_Box);
   
   type Orientation_Type is
     (Vertical,
      Horizontal);
   
   package Boxes_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Box_Class_Ptr);
   
   package Matrices is new Artics.Generic_Matrices (Box_Class_Ptr, null);
   use Matrices;
   
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
   
   function Is_Valid_Box (This : access Box_Record'Class) return Boolean;
   
   function Get_Parent_Box 
     (This : access Box_Record) return access Box_Record'Class;
   procedure Set_Parent_Box
     (This   : access Box_Record;
      Parent : access Box_Record'Class);
   
   --     function Get_X (This : access Box_Record) return Natural;
   --     procedure Set_X
   --       (This : access Box_Record;
   --        X    : Natural);
   --     
   --     function Get_Y (This : access Box_Record) return Natural;
   --     procedure Set_Y
   --       (This : access Box_Record;
   --        Y    : Natural);
   
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
   
   --  function Get_Builder
   --    (This : access Box_Record) return access Builder_Interface'Class;
   --  procedure Set_Builder
   --    (This  : access Box_Record;
   --     Build : access Builder_Interface'Class);
   
   --  procedure Place_Box (This : access Box_Record);
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
   
   function Get_Has_Vlink
     (This : access Box_Record) return Boolean;
   procedure Set_Has_Vlink
     (This : access Box_Record;
      Bool : Boolean);
   
   procedure Dump_Box (This : access Box_Record);

   procedure Place_Matrix 
     (This   : access Box_Record;
      Matrix : access Matrices.Matrix_Record) is null;
   
   function Is_Empty_Box (This : access Box_Record) return Boolean;
   function Is_Hlink_Box (This : access Box_Record) return Boolean; 
   function Is_Hlink_Vlink_Box (This : access Box_Record) return Boolean; 
   function Is_Busy_Box (This : access Box_Record) return Boolean;
  
private
   
   type Box_Record is tagged record
      Node : Node_Id;
      Kind : Box_Kind;
      Parent : access Box_Record'Class;
      Orientation : Orientation_Type;
      Is_Action_Box : Boolean;
      
      Has_Vlink : Boolean;
      
      -- X : Natural;
      -- Y : Natural;
      Xabs : Natural;
      Yabs : Natural;
      H : Natural;
      W : Natural;
      -- Builder : access Builder_Interface'Class;
   end record;
   
   No_Box_Record : constant Box_Record :=
     (Node          => Empty,
      Kind          => No_Box_Kind,
      Parent        => null,
      Orientation   => Horizontal,
      Is_Action_Box => False,
      Has_Vlink     => False,
      -- X             => 0,
      -- Y             => 0,
      Xabs          => 0,
      Yabs          => 0,
      H             => 0,
      W             => 0);
   -- Builder       => null);
   
   The_True_Box  : constant access Box_Record
     := new Box_Record'(No_Box_Record);
   
   The_Empty_Box : constant access Box_Record
     := new Box_Record'(No_Box_Record);
   
   The_Hlink_Box : constant access Box_Record
     := new Box_Record'(No_Box_Record);
   
   The_Hlink_Vlink_Box : constant access Box_Record
     := new Box_Record'(No_Box_Record);
   
   The_Busy_Box  : constant access Box_Record
     := new Box_Record'(No_Box_Record);
   
   The_Vlink_Box : constant access Box_Record
     := new Box_Record'(No_Box_Record);

end Reflex.Boxes;
