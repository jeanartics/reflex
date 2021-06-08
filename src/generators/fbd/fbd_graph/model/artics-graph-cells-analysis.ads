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
with Artics.Lists_Helpers;

with Artics.Objects;         use Artics.Objects;

package Artics.Graph.Cells.Analysis is
      
   procedure Build_Cell_Dependencies_Graph 
     (Root  : access Cell_Record'Class);
     --  starting from a cell record that contains multiple other cells 
     --  corresponding to code elements, build a list of cells where
     --  each one correspond to a connected graph and set then as childrens of
     --  Root
        
   procedure Compute_Layers (V : access Cell_Record'Class);
   --  for a connected graph V do layers classification for vertices in layers
   
   procedure Remove_Cycle (Cell : access Cell_Record'Class);
   
   procedure Compute_Layer_Forward (Root : access Cell_Record'Class);
   
   procedure Inc_Forward_Layer 
     (Cell  : access Cell_Record'Class;
      Layer : Integer);
   
private
   
   function Get_Parent_To_Add_As_Vertex 
     (Root : access Cell_Record'Class;
      Item : access Cell_Record'Class) return access Cell_Record'Class;
   --  starting from a cell, return Item ancestor located in first level 
   --  childs of given Root
   
   
   procedure Add_Parent_Vertex 
     (L     : in out Cells_Lists.List;
      P     : access Cell_Record'Class;
      Added : out Boolean);
   --  appends vertex P to the list L if not already added and sets output flag
   --  True if add done or False otherwise
   
   
   function Get_Edge 
     (L    : Cells_Lists.List;                        
      From : access Cell_Record'Class;
      To : access Cell_Record'Class) return access Cell_Record'Class; 
   --  returns the edge between From and To vertices contained in list L
   
   
   procedure Unmark_All (VL : Cells_Lists.List);
   --  mark as unvisited all cells contained in VL

   
   function First_Unvisited_Cell (CL : Cells_Lists.List) 
                                  return access Cell_Record'Class;
   --  returns the first unvisited vertex of the list CL

   
   function Next_Unvisited_Cell 
     (CL   : Cells_Lists.List;
      Cell : access Cell_Record'Class) return access Cell_Record'Class;
   --  returns the first not visited vertex of CL after Cell

   
   function Is_In_Vertex (Cell : access Cell_Record'Class;
                          L    : Cells_Lists.List) return Boolean;
   --  for a given vertex the function returns True if Cell is in terminal 
   --  (i.e. vertex which don't have any incomming edge) vertex and False 
   --  otherwise
   
   function Is_Out_Vertex (Cell : access Cell_Record'Class;
                           L    : Cells_Lists.List) return Boolean;
   --  for a given vertex the function returns True if Cell is out terminal 
   --  (i.e. vertex which don't have any outgoing edge) vertex and False 
   --  otherwise
   
   
   function First_Unvisited_In_Terminal_Cell 
     (CL : Cells_Lists.List) return access Cell_Record'Class;
   --  returns the first not visited in terminal vertex (i.e. vertex which 
   --  don't have any incomming edge)

   
   function Next_Unvisited_In_Terminal_Cell 
     (CL   : Cells_Lists.List; 
      Cell : access Cell_Record'Class) return access Cell_Record'Class;
   --  returns the first not visited in terminal vertex (i.e. vertex which 
   --  don't have any incomming edge) after Cell
   
   
   function Get_Outgoing_Edges 
     (V     : access Cell_Record'Class;
      Edges : Cells_Lists.List) return Cells_Lists.List;
   --  returns the list of outgoing edges for a given vertex V

   
   procedure Explore (V     : access Cell_Record'Class;
                      Edges : Cells_Lists.List;
                      CCL   : in out Cells_Lists.List;
                      Union : out Cell_Ptr);
   --  starting from a vetex V recurse until terminal vertex found (any 
   --  outgoing) or all connected vertices was already explored) build the 
   --  connected graph
   --  Union indicates if the visited vertex was marked during current explore
   --  or previously 

   
   procedure Build_Simplified_Dependencies_Graph 
     (Root     : access Cell_Record'Class;
      Graph_VL : out Cells_Lists.List;
      Graph_EL : out Cells_Lists.List);
   --  starting from a cell containing child cells inside, which defines graphs,
   --  find the vertex VL and simplified edges EL list (between 2 vertices only
   --  one invisible edge will be stored, having associated original edges list 
   --  in "depends" field of cell)
   
   
   procedure Build_Connected_Graphs 
     (VL : Cells_Lists.List;
      EL : Cells_Lists.List;
      CL_Lists : out Cells_Lists_To_Lists.List);
   --  starting from vertices and edges lists builds a list of connected graphs

   
   procedure Build_Cells_And_Update_Childs 
     (Root     : access Cell_Record'Class;
      CL_Lists : Cells_Lists_To_Lists.List);
   --  create a new not visible cells list as childrens of root where each cell 
   --  contains a connected graph 
   
   ----------------------------------------------------------------------------
      
   --  layers associations (Vx, Pre (Vx)) structure
   type Vertex_Layer_Association is
      record
         Vx     : access Cell_Record'Class; 
         Pre_Vx : Cells_Lists.List;
      end record;
   No_Vertex_Layer_Association : constant Vertex_Layer_Association 
     := Vertex_Layer_Association'(Vx     => null,
                                  Pre_Vx => Cells_Lists.Empty_List);
   
   type Graph_Vertex_Layers_Array is array (Integer range <>) 
     of Vertex_Layer_Association;
      
   
   function Get_Graph_Vertices_Number 
     (V : access Cell_Record'Class) return Integer;
   --  starting from a root cell find number of child vertices 
   
   function Get_Incomming_Edges 
     (V : access Cell_Record'Class;
      L : Cells_Lists.List) return Cells_Lists.List;
   --  computes all incomming edges of a given vertex V based on a given list L
   --  containing all childs of connected graph that contains V
   
end Artics.Graph.Cells.Analysis;
