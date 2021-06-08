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

package body Artics.Graph.Models_Changes is
   
   -----------------------
   -- Initialize_Change --
   -----------------------
   
   procedure Initialize_Change
     (This  : access Model_Change_Record;
      Model : access Model_Interface'Class;
      Typ   : Change_Type) is
   begin
      This.Model := Model;
      This.Set_Change_Type (Typ);
   end Initialize_Change;
   
   ---------------
   -- Get_Model --
   ---------------
   
   function Get_Model 
     (C : access Model_Change_Record) return access Model_Interface'Class is
   begin
      return C.Model;
   end Get_Model;
   
   ---------------
   -- Set_Model --
   ---------------
   
   procedure Set_Model
     (C     : access Model_Change_Record;
      Model : access Model_Interface'Class) is
   begin
      C.Model := Model;
   end Set_Model;
   
   -------------
   -- Execute --
   -------------
   
   procedure Execute (C : access Model_Change_Record) is
   begin
      null;
   end Execute;
   
end Artics.Graph.Models_Changes;
