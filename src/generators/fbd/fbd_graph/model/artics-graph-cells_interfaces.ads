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

with Artics.Types; use Artics.Types;
with Artics.Objects; use Artics.Objects;
with Artics.Graph.Cells_Geometry; use Artics.Graph.Cells_Geometry;

-- Defines the requirements for a cell that can be used in an mxGraphModel.

package Artics.Graph.Cells_Interfaces is

   type Cell_Interface is interface;
   type Cell_Interface_Ptr is access all Cell_Interface'Class;
   
   -- The Interface methods
   
   function Get_Id (C : access Cell_Interface) return String is abstract;
   -- Returns the Id of the cell as a string.
   -- @return Returns the Id.

   procedure Set_Id 
     (C  : access Cell_Interface;
      Id : String) is abstract;
   -- Sets the Id of the cell to the given string.
   -- @param id String that represents the new Id.
   
   function Get_Value
     (C : access Cell_Interface) return access Object_Record'Class is abstract;
   -- Returns the user object of the cell.
   -- @return Returns the user object.
   
   procedure Set_Value
     (C : access Cell_Interface; 
      V : access Object_Record'Class) is abstract;
   -- Sets the user object of the cell.
   -- @param value Object that represents the new value.

   function Get_Geometry
     (C : access Cell_Interface) 
      return access Cell_Geometry_Record'Class is abstract;
   -- Returns the object that describes the geometry.
   -- @return Returns the cell geometry.
   
   procedure Set_Geometry
     (C : access Cell_Interface;
      G : access Cell_Geometry_Record'Class) is abstract;
   -- Sets the object to be used as the geometry.
   
   function Is_Vertex (C : access Cell_Interface) return Boolean is abstract;
   -- Returns true if the cell is a vertex.
   -- @return Returns true if the cell is a vertex.
   procedure Set_Vertex
     (C      : access Cell_Interface;
      Vertex : Boolean) is abstract;
   
   function Is_Edge  (C : access Cell_Interface) return Boolean is abstract;
   procedure Set_Edge
     (C    : access Cell_Interface;
      Edge : Boolean) is abstract;
   -- Returns true if the cell is an edge.
   -- @return Returns true if the cell is an edge.

   function Get_Parent
     (C : access Cell_Interface) return access Cell_Interface is abstract;
   -- Returns the cell's parent.
   -- @return Returns the parent cell.
   
   procedure Set_Parent
     (C      : access Cell_Interface;
      Parent : access Cell_Interface) is abstract;
   -- Sets the parent cell.
   -- @param parent Cell that represents the new parent.

   function Get_Terminal
     (C         : access Cell_Interface;
      Is_Source : Boolean) return access Cell_Interface is abstract;
   -- Returns the source or target terminal.
   -- @param source Boolean that specifies if the source terminal should be
   -- returned.
   -- @return Returns the source or target terminal.
   
   procedure Set_Terminal
     (C         : access Cell_Interface;
      Terminal  : access Cell_Interface;
      Is_Source : Boolean) is abstract;     
   procedure Reset_Terminal 
     (C         : access Cell_Interface;
      Is_Source : Boolean) is abstract;
   -- Sets the source or target terminal and returns the new terminal.
   -- @param terminal Cell that represents the new source or target terminal.
   -- @param isSource Boolean that specifies if the source or target terminal
   -- should be set.
   -- @return Returns the new terminal.
   
   function Get_Child_Count
     (C : access Cell_Interface) return Integer is abstract;
   -- Returns the number of child cells.
   -- @return Returns the number of children.
   
   procedure Insert
     (C     : access Cell_Interface;
      Child : access Cell_Interface) is abstract;
   -- Appends the specified child into the child array and updates the parent
   -- reference of the child. Returns the appended child.
   -- @param child Cell to be appended to the child array.
   -- @return Returns the new child.
   
   procedure Remove
     (C     : access Cell_Interface;
      Child : access Cell_Interface) is abstract;     
   -- Removes the given child from the child array and returns it. Will remove
   -- the parent reference of the child.
   -- @param child Cell that represents the child to be removed.
   -- @return Returns the child that was removed.
   
   procedure Remove_From_Parent (C : access Cell_Interface) is null;
   -- Removes the cell from its parent.
   
   function Get_Edge_Count (C : access Cell_Interface) return Integer is abstract;
   -- Returns the number of edges in the edge array.
   -- @return Returns the number of edges.

   procedure Insert_Edge
     (C           : access Cell_Interface;
      Edge        : access Cell_Interface;
      Is_Outgoing : Boolean) is abstract;
   -- Inserts the specified edge into the edge array and returns the edge.
   -- Will update the respective terminal reference of the edge.
   -- @param edge Cell to be inserted into the edge array.
   -- @param isOutgoing Boolean that specifies if the edge is outgoing.
   -- @return Returns the new edge.
   
   procedure Remove_Edge
     (C           : access Cell_Interface;
      Edge        : access Cell_Interface;
      Is_Outgoing : Boolean) is abstract;
   -- Removes the specified edge from the edge array and returns the edge.
   -- Will remove the respective terminal reference from the edge.
   -- @param edge Cell to be removed from the edge array.
   -- @param isOutgoing Boolean that specifies if the edge is outgoing.
   -- @return Returns the edge that was removed.
   
   procedure Remove_From_Terminal
     (C         : access Cell_Interface;
      Is_Source : Boolean) is abstract;
   -- Removes the edge from its source or target terminal.
   -- @param isSource Boolean that specifies if the edge should be removed
   -- from its source or target terminal.
   
   --     function Clone
   --       (C : access Cell_Interface) return access Cell_Interface is abstract;
   -- Returns a clone of this cell.
   -- @return Returns a clone of this cell.
   
   function Clone (C                 : access Cell_Interface;
                   Include_Childrens : Boolean := False)  
                   return access Cell_Interface is abstract;
   
end Artics.Graph.Cells_Interfaces;
