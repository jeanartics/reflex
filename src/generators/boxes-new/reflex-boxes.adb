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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Reflex.Boxes is
   
   -------------
   -- New_Box --
   -------------
   
   function New_Box return Box_Ptr is
      This : Box_Ptr := new Box_Record'(No_Box_Record);
   begin
      return This;
   end New_Box;
   
   --------------
   -- Free_Box --
   --------------
   
   procedure Free_Box (This : in out Box_Ptr) is
   begin
      null;
   end Free_Box;
   
   --------------
   -- Get_Node --
   --------------
   
   function Get_Node (This : access Box_Record) return Node_Id is
   begin
      return This.Node;
   end Get_Node;
   
   --------------
   -- Set_Node --
   --------------
   
   procedure Set_Node
     (This : access Box_Record;
      Node : Node_Id) is
   begin
      This.Node := Node;
   end Set_Node;
   
   ------------------
   -- Get_Box_Kind --
   ------------------
   
   function Get_Box_Kind (This : access Box_Record) return Box_Kind is
   begin
      return This.Kind;
   end Get_Box_Kind;
   
   ------------------
   -- Set_Box_Kind --
   ------------------
   
   procedure Set_Box_Kind
     (This : access Box_Record;
      Kind : Box_Kind) is
   begin
      This.Kind := Kind;
   end Set_Box_Kind;
   
   ------------------
   -- Is_Valid_Box --
   ------------------
   
   function Is_Valid_Box (This : access Box_Record'Class) return Boolean is
   begin
      return This /= null 
        and then This.Get_Box_Kind = Terminal_Box 
        and then This /= The_True_Box
        and then This /= The_Empty_Box
        and then This /= The_Hlink_Box
        and then This /= The_Busy_Box;
   end Is_Valid_Box;
   
   --------------------
   -- Get_Parent_Box --
   --------------------
   
   function Get_Parent_Box 
     (This : access Box_Record) return access Box_Record'Class 
   is
   begin
      return This.Parent;
   end Get_Parent_Box;
   
   --------------------
   -- Set_Parent_Box --
   --------------------
   
   procedure Set_Parent_Box
     (This   : access Box_Record;
      Parent : access Box_Record'Class) is
   begin
      This.Parent := Parent;
   end Set_Parent_Box;
   
   ---------------------
   -- Get_Orientation --
   ---------------------
   
   function Get_Orientation
     (This : access Box_Record) return Orientation_Type is
   begin
      return This.Orientation;
   end Get_Orientation;
   
   ---------------------
   -- Set_Orientation --
   ---------------------
   
   procedure Set_Orientation
     (This   : access Box_Record;
      Orient : Orientation_Type) is
   begin
      This.Orientation := Orient;
   end Set_Orientation;
   
   -----------------------
   -- Get_Is_Action_Box --
   -----------------------
   
   function Get_Is_Action_Box 
     (This : access Box_Record) return Boolean is
   begin
      return This.Is_Action_Box;
   end Get_Is_Action_Box;
   
   -----------------------
   -- Set_Is_Action_Box --
   -----------------------
   
   procedure Set_Is_Action_Box
     (This : access Box_Record;
      Bool : Boolean) is
   begin
      This.Is_Action_Box := Bool;
   end Set_Is_Action_Box;
   
   -------------------
   -- Get_Has_Vlink --
   -------------------
   
   function Get_Has_Vlink
     (This : access Box_Record) return Boolean is
   begin
      return This.Has_Vlink;
   end Get_Has_Vlink;
   
   -------------------
   -- Set_Has_Vlink --
   -------------------
   
   procedure Set_Has_Vlink
     (This : access Box_Record;
      Bool : Boolean) is
   begin
      This.Has_Vlink := Bool;
   end Set_Has_Vlink;
   
   --------------
   -- Get_Xabs --
   --------------
   
   function Get_Xabs (This : access Box_Record) return Natural is
   begin
      return This.Xabs;
   end Get_Xabs;
   
   --------------
   -- Set_Xabs --
   --------------
   
   procedure Set_Xabs
     (This : access Box_Record;
      X    : Natural) is
   begin
      This.Xabs := X;
   end Set_Xabs;
   
   --------------
   -- Get_Yabs --
   --------------
   
   function Get_Yabs (This : access Box_Record) return Natural is
   begin
      return This.Yabs;
   end Get_Yabs;
   
   --------------
   -- Set_Yabs --
   --------------
   
   procedure Set_Yabs
     (This : access Box_Record;
      Y    : Natural) is
   begin
      This.Yabs := Y;
   end Set_Yabs;
   
   ----------------
   -- Get_Height --
   ----------------
   
   function Get_Height (This : access Box_Record) return Natural is
   begin
      return This.H;
   end Get_Height;
   
   ----------------
   -- Set_Height --
   ----------------
   
   procedure Set_Height
     (This : access Box_Record;
      H    : Natural) is
   begin
      This.H := H;
   end Set_Height;
   
   ---------------
   -- Get_Width --
   ---------------
   
   function Get_Width (This : access Box_Record) return Natural is
   begin
      return This.W;
   end Get_Width;
   
   ---------------
   -- Set_Width --
   ---------------
   
   procedure Set_Width
     (This : access Box_Record;
      W    : Natural) is
   begin
      This.W := W;
   end Set_Width;
   
   ------------------------
   -- Absolute_Place_Box --
   ------------------------
   
   procedure Absolute_Place_Box 
     (This : access Box_Record) is
   begin
      null;
   end Absolute_Place_Box;
   
   ------------------
   -- Is_Empty_Box --
   ------------------
   
   function Is_Empty_Box (This : access Box_Record) return Boolean is
   begin
      return This = null;
   end Is_Empty_Box;
   
   ------------------
   -- Is_Hlink_Box --
   ------------------
   
   function Is_Hlink_Box (This : access Box_Record) return Boolean is
   begin
      return This = The_Hlink_Box;
   end Is_Hlink_Box;
   
   ------------------------
   -- Is_Hlink_Vlink_Box --
   ------------------------
   
   function Is_Hlink_Vlink_Box (This : access Box_Record) return Boolean is
   begin
      return This = The_Hlink_Vlink_Box;
   end Is_Hlink_Vlink_Box;
   
   -----------------
   -- Is_Busy_Box --
   -----------------
   
   function Is_Busy_Box (This : access Box_Record) return Boolean is
   begin
      return This = The_Busy_Box;
   end Is_Busy_Box;
  
   
   --------------
   -- Dump_Box --
   --------------
   
   procedure Dump_Box (This : access Box_Record) is
   begin
      Put_Line ("Box Kind = " & This.Kind'Img);
      if This.Get_Node /= Empty then
         Put_Line ("Box Node = " & This.Get_Node'Img);
      else
         Put_Line ("Box Node = Empty");
      end if;
      if This.Parent /= null then
         Put_Line ("Parent Kind = " & This.Parent.Kind'Img);
      else
         Put_Line ("No Parent");
      end if;
      
      Put_Line ("Orientation   = " & This.Orientation'Img);
      Put_Line ("Is_Action_Box = " & This.Is_Action_Box'Img);
      Put_Line ("Xabs          = " & This.Xabs'Img);
      Put_Line ("Yabs          = " & This.Yabs'Img);
      Put_Line ("W             = " & This.W'Img);
      Put_Line ("H             = " & This.H'Img);
      New_Line;
   end Dump_Box;
   
end Reflex.Boxes;
