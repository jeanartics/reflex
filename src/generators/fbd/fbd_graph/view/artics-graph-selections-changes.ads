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
-- Reflex is originally developed  by the Artics team at Grenoble (France). --
--                                                                          --
------------------------------------------------------------------------------

with Artics.Types; use Artics.Types;
with Artics.Objects; use Artics.Objects;
with Artics.Graph.Cells; use Artics.Graph.Cells;
with Artics.Graph.Models_Interfaces; use Artics.Graph.Models_Interfaces;
with Artics.Graph.Selections.Models; use Artics.Graph.Selections.Models;
with Artics.Graph.Undoables.Changes_Interfaces; use Artics.Graph.Undoables.Changes_Interfaces;
with Artics.Graph.Changes; use Artics.Graph.Changes;

package Artics.Graph.Selections.Changes is
   
   type Selection_Change_Record is new Change_Record with private;
   type Selection_Change_Ptr is access all Selection_Change_Record;
   type Selection_Change_Class_Ptr is access all Selection_Change_Record'Class;
   
   No_Selection_Change_Record : constant Selection_Change_Record;
   
   function New_Selection_Change
     (Sel_Model : access Selection_Model_Record;
      Added     : Cells_Lists.List;
      Removed   : Cells_Lists.List) return Selection_Change_Ptr;
   
   procedure Free_Selection_Change (This : in out Selection_Change_Ptr);
   
   function Get_Selection_Model 
     (This : access Selection_Change_Record) 
     return access Selection_Model_Record;
   
   procedure Set_Selection_Model 
     (This      : access Selection_Change_Record;
      Sel_Model : access Selection_Model_Record);
   
   function Added (S : access Selection_Change_Record) return Cells_Lists.List;
   procedure Set_Added
     (S : access Selection_Change_Record;
      L : Cells_Lists.List);
      
   function Removed
     (S : access Selection_Change_Record) return Cells_Lists.List;
   procedure Set_Removed
     (S : access Selection_Change_Record;
      L : Cells_Lists.List);
   
   procedure Execute (S : access Selection_Change_Record);
   
private
   
   type Selection_Change_Record is new Change_Record with record
      Sel_Model : access Selection_Model_Record;
      Added     : Cells_Lists.List;
      Removed   : Cells_Lists.List;
   end record;
   
   No_Selection_Change_Record : constant Selection_Change_Record :=
     Selection_Change_Record'
     (No_Change_Record with 
      Sel_Model => null,
      Added     => Cells_Lists.Empty_List,
      Removed   => Cells_Lists.Empty_List);
   
end Artics.Graph.Selections.Changes;
		  
		  
