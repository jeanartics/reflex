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

with Artics.Types; use Artics.Types;
with Artics.Objects; use Artics.Objects;

with Artics.Graph.Cells; use Artics.Graph.Cells;
with Artics.Graph.Models_Interfaces; use Artics.Graph.Models_Interfaces;
with Artics.Graph.Undoables.Changes_Interfaces; use Artics.Graph.Undoables.Changes_Interfaces;
with Artics.Graph.Changes; use Artics.Graph.Changes;

package Artics.Graph.Models_Changes is
   
   type Model_Change_Record is new Change_Record with private;
   type Model_Change_Ptr is access all Model_Change_Record;
   type Model_Change_Class_Ptr is access all Model_Change_Record'Class;

   No_Model_Change_Record : constant Model_Change_Record;
   
   procedure Initialize_Change
     (This  : access Model_Change_Record;
      Model : access Model_Interface'Class;
      Typ   : Change_Type);
   
   function Get_Model
     (C : access Model_Change_Record) return access Model_Interface'Class;
   procedure Set_Model
     (C     : access Model_Change_Record;
      Model : access Model_Interface'Class);
   
   procedure Execute (C : access Model_Change_Record);
   
private

   type Model_Change_Record is new Change_Record with record
	Model : access Model_Interface'Class;
   end record;
   
   No_Model_Change_Record : constant Model_Change_Record :=
     (No_Change_Record with Model => null);
      
end Artics.Graph.Models_Changes;
