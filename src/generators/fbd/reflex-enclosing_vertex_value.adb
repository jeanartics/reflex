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

package body Reflex.Enclosing_Vertex_Value is
   
   --------------------------------
   -- New_Enclosing_Vertex_Value --
   --------------------------------
   
   function New_Enclosing_Vertex_Value return Enclosing_Vertex_Value_Ptr is
      This : Enclosing_Vertex_Value_Ptr := 
        new Enclosing_Vertex_Value_Record'(No_Enclosing_Vertex_Value_Record);
   begin
      return This;
   end New_Enclosing_Vertex_Value;
   
   ---------------------------------
   -- Free_Enclosing_Vertex_Value --
   ---------------------------------      
   
   procedure Free_Enclosing_Vertex_Value (This : in out Enclosing_Vertex_Value_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Enclosing_Vertex_Value_Record, Enclosing_Vertex_Value_Ptr);
   begin
      if This /= null then
         Free (This);
      end if;
   end Free_Enclosing_Vertex_Value;
   
   --------------
   -- Get_Node --
   --------------
   
   function Get_Node (This : access Enclosing_Vertex_Value_Record) return Node_Id is
   begin
      return This.Node;
   end Get_Node;

   --------------
   -- Set_Node --
   --------------
   
   procedure Set_Node (This : access Enclosing_Vertex_Value_Record; Node : Node_Id) is
   begin
      This.Node := Node;
   end Set_Node;
   
   ----------------------
   -- Get_Cell_To_Link --
   ----------------------
   
   function Get_Cell_To_Link 
     (This : access Enclosing_Vertex_Value_Record) return access Cell_Record is
   begin
      return This.Cell_To_Link;
   end Get_Cell_To_Link;
   
   ----------------------
   -- Set_Cell_To_Link --
   ----------------------
   
   procedure Set_Cell_To_Link
     (This : access Enclosing_Vertex_Value_Record;
      Cell : access Cell_Record) is
   begin
      This.Cell_To_Link := Cell;
   end Set_Cell_To_Link;
   
   ---------------------
   -- Get_Orientation --
   ---------------------
   
   function Get_Orientation
     (This : access Enclosing_Vertex_Value_Record) 
      return Cell_Orientation is
   begin
      return This.Orientation;
   end Get_Orientation;
   
   ---------------------
   -- Set_Orientation --
   ---------------------
   
   procedure Set_Orientation
     (This        : access Enclosing_Vertex_Value_Record;
      Orientation : Cell_Orientation) is
   begin
      This.Orientation := Orientation;
   end Set_Orientation;
   
end Reflex.Enclosing_Vertex_Value;
