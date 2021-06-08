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

package body Artics.Graph.Models_Changes.Values is
   
   ----------------------
   -- New_Value_Change --
   ----------------------
   
   function New_Value_Change return Value_Change_Ptr is
   begin
      return new Value_Change_Record'(No_Value_Change_Record);
   end New_Value_Change;
   
   ----------------------
   -- New_value_Change --
   ----------------------
   
   function New_Value_Change
     (Model  : access Model_Interface'Class;
      Cell   : access Cell_Record'Class;
      Value  : access Object_Record'Class) return Value_Change_Ptr is
      
      C : Value_Change_Ptr := New_Value_Change;
   begin
      C.Set_Model (Model);
      C.Initialize_Change (Model, Value_Change);
      C.Cell := Cell;
      C.Value := Value;
      -- C.Previous := Value;
      
      return C;
   end New_Value_Change;
   
   --------------
   -- Get_Cell --
   --------------
   
   function Get_Cell
     (C : access  Value_Change_Record) return access Cell_Record'Class is
   begin
      return C.Cell;
   end Get_Cell;
   
   --------------
   -- Set_Cell --
   --------------
   
   procedure Set_Cell
     (C     : access Value_Change_Record;
      Value : access Cell_Record'Class) is
   begin
      C.Cell := Value;
   end Set_Cell;
   
   ---------------
   -- Get_Value --
   ---------------
   
   function Get_Value
     (C : access Value_Change_Record) return access Object_Record'Class is
   begin
      return C.Value;
   end Get_Value;
   
   ---------------
   -- Set_Value --
   ---------------
   
   procedure Set_Value
     (C     : access Value_Change_Record;
      Value : access Object_Record'Class) is
   begin
      C.Value := Value;
   end Set_Value;
   
   ------------------
   -- Get_Previous --
   ------------------
   
   function Get_Previous
     (C : access Value_Change_Record) return access Object_Record'Class is
   begin
      return C.Previous;
   end Get_Previous;
   
   ------------------
   -- Set_Previous --
   ------------------
   
   procedure Set_Previous
     (C     : access Value_Change_Record;
      Value : access Object_Record'Class) is
   begin
      C.Previous := Value;
   end Set_Previous;
   
   -------------
   -- Execute --
   -------------
   
   procedure Execute (C : access Value_Change_Record) 
   is
      M : access Model_Interface'Class;
   begin
      --  	(mxGraphModel) model).valueForCellChanged(cell, previous);
      M := C.Model;
      C.Previous := M.Value_For_Cell_Changed (C.Cell, C.Value);
   end Execute;
   
end Artics.Graph.Models_Changes.Values;
