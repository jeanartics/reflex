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

with Ada.Strings.Hash;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;

with Artics.Types; use Artics.Types;
with Artics.Graph.Cells; use Artics.Graph.Cells;
with Artics.Graph.Graphs_Interfaces; use Artics.Graph.Graphs_Interfaces;
with Artics.Graph.Events; use Artics.Graph.Events;

-- Implements the selection model for a graph.
-- 
-- This class fires the following events:
-- 
-- mxEvent.UNDO fires after the selection was changed in changeSelection. The
-- <code>edit</code> property contains the mxUndoableEdit which contains the
-- mxSelectionChange.
-- 
-- mxEvent.CHANGE fires after the selection changes by executing an
-- mxSelectionChange. The <code>added</code> and <code>removed</code>
-- properties contain Collections of cells that have been added to or removed
-- from the selection, respectively.
--  
-- To add a change listener to the graph selection model:
-- 
-- <code>
-- addListener(
--   mxEvent.CHANGE, new mxIEventListener()
--   {
--     public void invoke(Object sender, mxEventObject evt)
--     {
--       mxGraphSelectionModel model = (mxSelectionModel) sender;
--       Collection added = (Collection) evt.getProperty("added");
--       Collection removed = (Collection) evt.getProperty("removed");
--       selectionChanged(model, added, removed);
--     }
--   });
-- </code>

with Dummy; use Dummy;

package Artics.Graph.Selections.Models is
   
   use Artics.Graph.Events.Graph_Events;
   use Artics.Graph.Events.Graph_Listeners;
   use Artics.Graph.Events.Graph_Events_Sources;
   
   type Selection_Model_Record is new Event_Source_Record with private;
   type Selection_Model_Ptr is access all Selection_Model_Record;
   type Selection_Model_Class_Ptr is access all Selection_Model_Record'Class;
   
   No_Selection_Model_Record : constant Selection_Model_Record;
   
   function New_Selection_Model return Selection_Model_Ptr;
   -- Constructs a new selection model for the specified graph.
   -- @param graph
   
   function New_Selection_Model
     (Graph : access Graph_Interface'Class) return Selection_Model_Ptr;
   
   procedure Free_Selection_Model (This : in out Selection_Model_Ptr);
   
   function Is_Single_Selection
     (This : access Selection_Model_Record) return Boolean;
   -- @return the singleSelection
   
   procedure Set_Single_Selection
     (This             : access Selection_Model_Record;
      Single_Selection : Boolean);
   -- @param singleSelection the singleSelection to set
   
   function Is_Selected
     (This : access Selection_Model_Record;
      Cell : access Cell_Record'Class) return Boolean;
   -- Returns true if the given cell is selected.
   -- @param cell
   -- @return Returns true if the given cell is selected.
   
   function Is_Empty (This : access Selection_Model_Record) return Boolean;
   -- Returns true if no cells are selected.

   function Size (This : access Selection_Model_Record) return Integer;
   -- Returns the number of selected cells.
   
   procedure Clear (This : access Selection_Model_Record);
   --Clears the selection.
   
   function Get_Cell
     (This : access Selection_Model_Record) return access Cell_Record'Class;
   -- Returns the first selected cell.
   
   function Get_Cells
     (This : access Selection_Model_Record) return Cells_Lists.List;
   -- Returns the selection cells.
   
   procedure Set_Cell
     (This : access Selection_Model_Record;
      Cell : access Cell_Record'Class);
   -- Clears the selection and adds the given cell to the selection.
     
   procedure Set_Cells
     (This  : access Selection_Model_Record;
      Cells : Cells_Lists.List);
     
   -- Clears the selection and adds the given cells.
   
   function Get_First_Selectable_Cell
     (This  : access  Selection_Model_Record;
      Cells : Cells_Lists.List) return access Cell_Record'Class;
   -- Returns the first selectable cell in the given array of cells.
   -- @param cells Array of cells to return the first selectable cell for.
   -- @return Returns the first cell that may be selected.
   
   procedure Add_Cell 
     (This : access Selection_Model_Record;
      Cell : access Cell_Record'Class);
   -- Adds the given cell to the selection.

   procedure Add_Cells
     (This  : access Selection_Model_Record;
      Cells : Cells_Lists.List);
   
   procedure Remove_Cell
     (This : access Selection_Model_Record;
      Cell : access Cell_Record'Class);
   -- Removes the given cell from the selection.
   
   procedure Remove_Cells
     (This  : access Selection_Model_Record;
      Cells : Cells_Lists.List);
   
   procedure Change_Selection
     (This    : access Selection_Model_Record;
      Added   : Cells_Lists.List;
      Removed : Cells_Lists.List);

   procedure Cell_Added
     (This : access Selection_Model_Record;
      Cell : access Cell_Record'Class);
   
   procedure Cell_Removed
     (This : access Selection_Model_Record;
      Cell : access Cell_Record'Class);
   
   function Get_Graph
     (This : access Selection_Model_Record) return access Graph_Interface'Class;
   
private
   
   type Selection_Model_Record is new Event_Source_Record with record
      
      Graph : access Graph_Interface'Class;
      -- Reference to the enclosing graph.

      Single_Selection : Boolean;
      -- Specifies if only one selected item at a time is allowed.
      -- Default is false.

      Cells : Cells_Lists.List;
      -- Holds the selection cells.
   end record;
   
   No_Selection_Model_Record : constant Selection_Model_Record :=
     Selection_Model_Record'
     (No_Event_Source_Record with
      Graph            => null,
      Single_Selection => False,
      Cells            => Cells_Lists.Empty_List);

end Artics.Graph.Selections.Models;
		  
		  
