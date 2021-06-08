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
   
   --  Predefined box representing the boolean constant 
   
   The_True_Box        : constant access Box_Record;
   The_Empty_Box       : constant access Box_Record;
   The_Hlink_Box       : constant access Box_Record;
   The_Vlink_Box       : constant access Box_Record;
   The_Hlink_Vlink_Box : constant access Box_Record;
   The_Busy_Box        : constant access Box_Record;
   
   --  A box is either a Terminal or a container Box. A container box holds
   --  holds at least two Boxes, a Dual box holds two boxes and a multi boxes 
   --  is composed of two or more boxes. Inside the container, the child boxes
   --  are layout horizontaly or vertically. It is to said for an horizontal
   --  box, the childs are linked side by side in the horizontal direction, and
   --  in a vertical box, the childs are linked side by side in the vertical
   --  direction :
   --  Horizontal ;
   --     +----+   +----+
   --     |    |___|    |
   --     |    |   |    |
   --     +----+   +----+
   -- Vertical
   --       +----+
   --    +--|    |--+
   --    |  |    |  |
   --    |  +----+  |
   --     ..........
   --    |  +----+  |
   --    +--|    |--+
   --       |    |
   --       +----+
   
   --  A terminal box represents A Literal, Variable or an unary orpertor
   --  A Dual Box represents a binary operator, a binary relation..
   
   --  The child of a box are layout relative of Parent
   
   type Box_Kind is (No_Box_Kind, Dual_Box, Multi_Box, Terminal_Box);
   
   type Orientation_Type is (Vertical, Horizontal);
   
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
   --  Return the node represented by the box. 
   
   function Get_Box_Kind (This : access Box_Record) return Box_Kind;
   procedure Set_Box_Kind
     (This : access Box_Record;
      Kind : Box_Kind);
   --  return the kind of box, either Terminal, Dual or Multi
   
   function Is_Valid_Box (This : access Box_Record'Class) return Boolean;
   
   function Get_Parent_Box 
     (This : access Box_Record) return access Box_Record'Class;
   procedure Set_Parent_Box
     (This   : access Box_Record;
      Parent : access Box_Record'Class);
   --  return the parent container of the box
   
   function Get_Xabs (This : access Box_Record) return Natural;
   procedure Set_Xabs
     (This : access Box_Record;
      X    : Natural);
   --  Return the box absolute coordinate X in the rung.
   
   function Get_Yabs (This : access Box_Record) return Natural;
   procedure Set_Yabs
     (This : access Box_Record;
      Y    : Natural);
   --  Return the box absolute coordinate Y in the rung.
   
   function Get_Height (This : access Box_Record) return Natural;
   procedure Set_Height
     (This : access Box_Record;
      H    : Natural);
   --  Return the height of a box, Terminal box are height are predefined. 
   --  For a horizontal Dual Box, the height is the max box height of the
   --  two child, and for a multi box, the height is the box Max height of 
   --  all children. For a vertical oriented boxes, the height is the sum of the
   --  height off all its children.
   
   function Get_Width (This : access Box_Record) return Natural;
   procedure Set_Width
     (This : access Box_Record;
      W    : Natural);
   --  Return the width of a box, Terminal box are height are predefined. 
   --  For a dual and a multi box oriented horizontaly, the width is the sum of
   --  all its children, and for a vertical oriented bow, the width is the max
   --  width of all its children.
   
   procedure Absolute_Place_Box (This : access Box_Record);
   --  This procedure does theabsolute  placement of the box in  a rung. It
   --  recursvly compute the placement of the children in a container and then
   --  compute the absolute coordinate of each box relative to the rung origin
   
   function Get_Orientation
     (This : access Box_Record) return Orientation_Type;
   procedure Set_Orientation
     (This   : access Box_Record;
      Orient : Orientation_Type);
   --  Return the layout orientation of a box, its is either vertical or 
   --  horizontal for a Dual or Multi Box, and it is not meaningfull for a
   --  Terminal Box.
   
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
      Has_Vlink     => False,
      Xabs          => 0,
      Yabs          => 0,
      H             => 0,
      W             => 0);
   
   The_True_Box  : constant access Box_Record := 
     new Box_Record'(No_Box_Record);
   
   The_Empty_Box : constant access Box_Record := 
     new Box_Record'(No_Box_Record);
   
   The_Hlink_Box : constant access Box_Record := 
     new Box_Record'(No_Box_Record);
   
   The_Hlink_Vlink_Box : constant access Box_Record := 
     new Box_Record'(No_Box_Record);
   
   The_Busy_Box  : constant access Box_Record := 
     new Box_Record'(No_Box_Record);
   
   The_Vlink_Box : constant access Box_Record := 
     new Box_Record'(No_Box_Record);

end Reflex.Boxes;
