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
-- Reflex is originally developed  by the Artics team at Grenoble.          --
--                                                                          --
------------------------------------------------------------------------------

with Artics.Dynamic_Tables;
with Artics.Buffers;      use Artics.Buffers;
with Artics.Graph.Graphs; use Artics.Graph.Graphs;

with Unity.Gen; use Unity.Gen;
with Reflex.Fbd_Emitor; use Reflex.Fbd_Emitor;
with Artics.Graph.Cells; use Artics.Graph.Cells;
with Reflex.Entities_Lists; use Reflex.Entities_Lists;

package Reflex.Fbd_Builders is
   
   type Fbd_Builder_Record is tagged private;
   type Fbd_Builder_Ptr is access all Fbd_Builder_Record;
   type Fbd_Builder_Class_Ptr is access all Fbd_Builder_Record'Class;
   
   No_Fbd_Builder_Record : constant Fbd_Builder_Record;
   
   function New_Builder return Fbd_Builder_Ptr;
   --  Create a new builder and his new graph, new fbd emitor, and set
   --  Default_Parent of graph with a new cell
   
   procedure Free_Builder (This : in out Fbd_Builder_Ptr);
   
   function Get_Cell_Parent
     (This : access Fbd_Builder_Record) return access Cell_Record;
   procedure Set_Cell_Parent 
     (This : access Fbd_Builder_Record; Cell : access Cell_Record);

   function Get_Graph 
     (This : access Fbd_Builder_Record) return access Graph_Record;
   procedure Set_Graph 
     (This  : access Fbd_Builder_Record; 
      Graph : access Graph_Record);

   function Get_Subp 
     (This : access Fbd_Builder_Record) return Node_Id;
   procedure Set_Subp
     (This : access Fbd_Builder_Record;
      Subp : Node_Id);
   
   function Get_Fbd_Emitor 
     (This : access Fbd_Builder_Record) return access Fbd_Emitor_Record;
   procedure Set_Fbd_Emitor 
     (This   : access Fbd_Builder_Record; 
      Emitor : access Fbd_Emitor_Record);
   
   procedure Append_Node
     (This : access Fbd_Builder_Record;
      Node : Node_Id);
   --  Append node in a list when this node is a local variable
   
   function Find_Node
     (This : access Fbd_Builder_Record;
      Node : Node_Id) return Node_Id;
   --  Compare node entity with nodes entities in the list and return the last node 
   --  with same entity
   
   procedure Build_Fbd 
     (This : access Fbd_Builder_Record; 
      Node : Node_Id);
   --  Dispatch all nodes in a subprogram
   
private
  
   type Fbd_Builder_Record is tagged record
      Graph         : access Graph_Record;
      Subp          : Node_Id;
      Cell_Parent   : access Cell_Record;
      Fbd_Emitor    : access Fbd_Emitor_Record;
      Fbd_Node_List : Nodes_Lists.List;  
   end record;
   
   No_Fbd_Builder_Record : constant Fbd_Builder_Record :=
     (Graph         => null,
      Subp          => Empty,
      Cell_Parent   => null,
      Fbd_Emitor    => null,
      Fbd_Node_List => Nodes_Lists.Empty_List);
   
end Reflex.Fbd_Builders;
