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

with Artics.Objects; use Artics.Objects;

with Artics.Graph.Views_Interfaces; use Artics.Graph.Views_Interfaces;

with Artics.Graph.Cells; use  Artics.Graph.Cells;
with Artics.Graph.Models_Interfaces; use  Artics.Graph.Models_Interfaces;
with Artics.Graph.Graphs_Interfaces; use  Artics.Graph.Graphs_Interfaces;
with Artics.Graph.Undoables.Changes_Interfaces; use Artics.Graph.Undoables.Changes_Interfaces;

with Artics.Geometry; use Artics.Geometry;
with Artics.Geometry.Points; use Artics.Geometry.Points;

with Artics.Graph.Changes; use Artics.Graph.Changes;

-- Action to change the current root in a view.

package Artics.Graph.Current_Roots_Changes is
   
   type Current_Root_Change_Record is new Change_Record with private;
   type Current_Root_Change_Ptr is access all Current_Root_Change_Record;
   type Current_Root_Change_Class_Ptr is
     access all Current_Root_Change_Record'Class;
   
   No_Current_Root_Change_Record : constant Current_Root_Change_Record;
   
   function New_Current_Root_Change return Current_Root_Change_Ptr;
   function New_Current_Root_Change
     (View : access View_Interface'Class;
      Root : access Cell_Record'Class) return Current_Root_Change_Ptr;
   
   function Get_View (This : access Current_Root_Change_Record) 
     return access View_Interface'Class;
   -- Returns the graph view where the change happened.

   function Get_Root (This : access Current_Root_Change_Record)
     return access Cell_Record'Class;
   -- Returns the root.
   
   function Get_Previous (This : access Current_Root_Change_Record)
     return access Cell_Record'Class;
   -- Returns the previous root.

   function Is_Up (This : access Current_Root_Change_Record) return Boolean;
   -- Returns true if the drilling went upwards.

   procedure Execute (This : access Current_Root_Change_Record);
   -- Changes the current root of the view.
   
private
   
   type Current_Root_Change_Record is new Change_Record with record
	View     : access View_Interface'Class;
	Root     : access Cell_Record'Class;
	Previous : access Cell_Record'Class;
	Up       : Boolean;
   end record;
   
   No_Current_Root_Change_Record : constant Current_Root_Change_Record := 
     Current_Root_Change_Record'
     (No_Change_Record with
	View   => null,
      Root     => null,
      Previous => null,
      Up       => False);
   
end Artics.Graph.Current_Roots_Changes;
