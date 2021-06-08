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

with Ada.Strings.Hash;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Artics.Types;          use Artics.Types;
with Artics.Namet;          use Artics.Namet;
with Artics.Objects;         use Artics.Objects;
with Artics.Lists_Helpers;

with Artics.Graph.Cells; use Artics.Graph.Cells;

package Artics.Graph.Algos is
   
   procedure Unmark_Cells (Cells : Cells_Lists.List);
      
   function Top_Parent
     (Root : access Cell_Record'Class;
      Item : access Cell_Record'Class) return access Cell_Record'Class;
   
   function Get_Englob
     (Root : access Cell_Record'Class;
      Item : access Cell_Record'Class) return access Cell_Record'Class;
   
   procedure Build_Vertex_Dependencies (Root : access Cell_Record'Class);
   
   procedure Reset_Vertex_Dependencies (Root : access Cell_Record'Class);
   
   function Build_Terminal_Vertices
     (Cells : Cells_Lists.List) return Cells_Lists.List;
   
   function Build_Starting_Vertices
     (Cells : Cells_Lists.List) return Cells_Lists.List;
   
   procedure Build_Connex_Cell
     (Cell   : access Cell_Record'Class;
      Connex : in out Cells_Lists.List);
   
   function Build_Connex_Graphs
     (Cells : Cells_Lists.List) return Cells_Lists_To_Lists.List;
   
   procedure Remove_Cycle (Cell : access Cell_Record'Class);
   
   procedure Compute_Layer_Forward (Root : access Cell_Record'Class);
   
   procedure Inc_Forward_Layer 
     (Cell  : access Cell_Record'Class;
      Layer : Integer);
   
end Artics.Graph.Algos;
