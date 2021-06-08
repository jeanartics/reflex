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

package body Artics.Graph.Models_Changes.Roots is
   
   ---------------------
   -- New_Root_Change --
   ---------------------
   
   function New_Root_Change return Root_Change_Ptr is
   begin
      return new Root_Change_Record'(No_Root_Change_Record);
   end New_Root_Change;
   
   ---------------------
   -- New_Root_Change --
   ---------------------
   
   function New_Root_Change
     (Model : access Model_Interface'Class;
      Root  : access Cell_Record'Class) return Root_Change_Ptr
   is
      R : Root_Change_Ptr:= New_Root_Change;
   begin
      R.Initialize_Change (Model, Root_Change);
      R.Root     := Root;
      R.Previous := Root;
      
      return R;
   end New_Root_Change;
   
   --------------
   -- Get_Root --
   --------------
   
   function Get_Root
     (C : access Root_Change_Record) return access Cell_Record'Class is
   begin
      return C.Root;
   end Get_Root;
   
   --------------
   -- Set_Root --
   --------------
   
   procedure Set_Root
     (C     : access Root_Change_Record;
      Value : access Cell_Record'Class) is
   begin
      C.Root := Value;
   end Set_Root;
   
   ------------------
   -- Get_Previous --
   ------------------
   
   function Get_Previous
     (C : access Root_Change_Record) return access Cell_Record'Class is
   begin
      return C.Previous;
   end Get_Previous;
   
   ------------------
   -- Set_Previous --
   ------------------
   
   procedure Set_Previous
     (C     : access Root_Change_Record;
      Value : access Cell_Record'Class) is
   begin
      C.Previous := Value;
   end Set_Previous;
   
   -------------
   -- Execute --
   -------------
   
   procedure Execute (C : access Root_Change_Record) is
      Model : access Model_Interface'Class := Get_Model (C);
   begin
      C.Root     := C.Previous;
      C.Previous := Model.Root_Changed (C.Previous);
   end Execute;
   
end Artics.Graph.Models_Changes.Roots;
