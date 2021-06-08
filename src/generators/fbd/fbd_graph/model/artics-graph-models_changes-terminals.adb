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

package body Artics.Graph.Models_Changes.Terminals is
   
   -------------------------
   -- New_Terminal_Change --
   -------------------------
   
   function New_Terminal_Change return Terminal_Change_Ptr is
   begin
      return new Terminal_Change_Record'(No_Terminal_Change_Record);
   end New_Terminal_Change;
   
   -------------------------
   -- New_Terminal_Change --
   -------------------------
   
   function New_Terminal_Change
     (Model    : access Model_Interface'Class;
      Cell     : access Cell_Record'Class;
      Terminal : access Cell_Record'Class;
      Source   : Boolean) return Terminal_Change_Ptr
   is 
      C : Terminal_Change_Ptr := New_Terminal_Change;
   begin
      C.Initialize_Change (Model, Terminal_Change);
      C.Cell     := Cell;
      C.Terminal := Terminal;
      C.Previous := C.Terminal;
      C.Source   := Source;
      
      return C;
   end New_Terminal_Change;
   
   --------------
   -- Get_Cell --
   --------------
   
   function Get_Cell
     (C : access  Terminal_Change_Record) return access Cell_Record'Class is
   begin
      return C.Cell;
   end Get_Cell;
   
   --------------
   -- Set_Cell --
   --------------
   
   procedure Set_Cell
     (C     : access Terminal_Change_Record;
      Value : access Cell_Record'Class) is
   begin
      C.Cell := Value;
   end Set_Cell;
   
   ------------------
   -- Get_Terminal --
   ------------------
   
   function Get_Terminal
     (C : access Terminal_Change_Record) return access Cell_Record'Class is
   begin
      return C.Terminal;
   end Get_Terminal;
   
   ------------------
   -- Set_Terminal --
   ------------------
   
   procedure Set_Terminal
     (C     : access Terminal_Change_Record;
      Value : access Cell_Record'Class) is
   begin
      C.Terminal := Value;
   end Set_Terminal;
   
   ------------------
   -- Get_Previous --
   ------------------
   
   function Get_Previous
     (C : access Terminal_Change_Record) return access Cell_Record'Class is
   begin
      return C.Previous;
   end Get_Previous;
   
   ------------------
   -- Set_Previous --
   ------------------
   
   procedure Set_Previous
     (C     : access Terminal_Change_Record;
      Value : access Cell_Record'Class) is
   begin
      C.Previous := Value;
   end Set_Previous;
   
   ---------------
   -- Is_Source --
   ---------------
   
   function Is_Source
     (C : access Terminal_Change_Record) return Boolean is
   begin
      return C.Source;
   end Is_Source;
   
   ----------------
   -- Set_Source --
   ----------------
   
   procedure Set_Source
     (C     : access Terminal_Change_Record;
      Value : Boolean) is
   begin
      C.Source := Value;
   end Set_Source;
   
   -------------
   -- Execute --
   -------------
   
   procedure Execute (C : access Terminal_Change_Record) 
   is
      Prev_Tmp : access Cell_Record'Class;
   begin
      C.Terminal := C.Previous;
      Prev_Tmp := Get_Terminal (C.Cell, C.Source);
      C.Model.Terminal_For_Cell_Changed (C.Cell, C.Previous, C.Source);
      C.Previous := Prev_Tmp;
   end Execute;
   
end Artics.Graph.Models_Changes.Terminals;
