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
with Artics.Namet; use Artics.Namet;
with Artics.Output; use Artics.Output;
with Artics.Lists_Helpers;
with Artics.Objects; use Artics.Objects;
with Artics.Geometry; use Artics.Geometry;
with Artics.Graph.Cells_Geometry; use Artics.Graph.Cells_Geometry;
with Artics.Graph.Cells_Interfaces; use Artics.Graph.Cells_Interfaces;

--  with Artics.Fbd.Tree; use Artics.Fbd.Tree;
-- Cells are the elements of the graph model. They represent the state of the
-- groups, vertices and edges in a graph. 
--
-- Edge Labels
-- 
-- Using the x- and y-coordinates of a cell's geometry it is possible to
-- position the label on edges on a specific location on the actual edge shape
-- as it appears on the screen. The x-coordinate of an edge's geometry is used
-- to describe the distance from the center of the edge from -1 to 1 with 0
-- being the center of the edge and the default value. The y-coordinate of an
-- edge's geometry is used to describe the absolute, orthogonal distance in
-- pixels from that point. In addition, the mxGeometry.offset is used as a
-- absolute offset vector from the resulting point.
--  
-- The width and height of an edge geometry are ignored.
--  
-- To add more than one edge label, add a child vertex with a relative 
-- geometry. The x- and y-coordinates of that geometry will have the same
-- semantiv as the above for edge labels.

package Artics.Graph.Cells is
   
   type Cell_Record is new Object_Record and Cell_Interface with private;
   type Cell_Ptr is access all Cell_Record;
   type Cell_Class_Ptr is access all Cell_Record'Class;

   No_Cell_Record : constant Cell_Record;
   
   function Equivalent_Key (Left, Right : Name_Id) return Boolean;
   function Hash_Func (Key : Name_Id) return Ada.Containers.Hash_Type;

   package Cells_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Name_Id,-- Unbounded_String,
      Element_Type    => Cell_Class_Ptr,
      Hash            => Hash_Func,
      Equivalent_Keys => Equivalent_Key);
   
   function Set_Equivalent_Key (Left, Right : Cell_Class_Ptr) return Boolean;
   function Set_Hash_Func(Key : Cell_Class_Ptr) return Ada.Containers.Hash_Type;
   package Cells_Sets is new Ada.Containers.Hashed_Maps
     (Key_Type        => Cell_Class_Ptr,
      Element_Type    => Boolean,
      Hash            => Set_Hash_Func,
      Equivalent_Keys => Set_Equivalent_Key);
   
   function CS_Equivalent_Key (Left, Right : Cell_Class_Ptr) return Boolean;
   function CS_Hash_Func
     (Key : Cell_Class_Ptr) return Ada.Containers.Hash_Type;
   package Maps_Cells_Strings is new Ada.Containers.Hashed_Maps
     (Key_Type        => Cell_Class_Ptr,
      Element_Type    => Unbounded_String, 
      Hash            => Cs_Hash_Func,
      Equivalent_Keys => Cs_Equivalent_Key);
   
   function Int_Equivalent_Key (Left, Right : Cell_Class_Ptr) return Boolean;
   function Int_Hash_Func
     (Key : Cell_Class_Ptr) return Ada.Containers.Hash_Type;
   package Maps_Cells_Integers is new Ada.Containers.Hashed_Maps
     (Key_Type        => Cell_Class_Ptr,
      Element_Type    => Integer,
      Hash            => Int_Hash_Func,
      Equivalent_Keys => Int_Equivalent_Key);
   
   package Cells_To_Cells_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Cell_Class_Ptr,
      Element_Type    => Cell_Class_Ptr,
      Hash            => Cs_Hash_Func,
      Equivalent_Keys => Cs_Equivalent_Key);
   
   package Cells_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Cell_Class_Ptr);   
   type Cells_Lists_Access is access Cells_Lists.List;
   
   package Cells_Lists_To_Lists is 
     new Ada.Containers.Doubly_Linked_Lists (Cells_Lists_Access);
   
   -- Helper function for cells Lists.   
   package Cells_Lists_Helpers is new Artics.Lists_Helpers
     (Element_Type => Cell_Class_Ptr,
      No_Element   => null,
      Lists        => Cells_Lists);
   use Cells_Lists_Helpers;
   
   package Cells_Lists_To_List_Helpers is new Artics.Lists_Helpers
     (Element_Type => Cells_Lists_Access,
      No_Element   => null,
      Lists        => Cells_Lists_To_Lists);
   use Cells_Lists_To_List_Helpers;
   
   --     package Cells_Vectors is new Ada.Containers.Vectors
   --       (Index_Type   => Integer,
   --        Element_Type => Cell_Class_Ptr);
   
   function New_Cell return access Cell_Record'Class;
   function New_Cell
     (Value : access Object_Record'Class) return access Cell_Record'Class;
   function New_Cell
     (Value    : access Object_Record'Class;
      Geometry : access Cell_Geometry_Record'Class) return access Cell_Record'Class;
   -- Create a new scenario node, and initialize it with default values 
   
   function Get_Id (C : access Cell_Record) return String;
   procedure Set_Id
     (C  : access Cell_Record;
      Id : String);
   
   function Get_Value
     (C : access Cell_Record) return access Object_Record'Class;
   procedure Set_Value
     (C : access Cell_Record;
      V : access Object_Record'Class);
   -- Holds the user object. Default is null.
      
   function Get_Geometry
     (C : access Cell_Record) return access Cell_Geometry_Record'Class;
   procedure Set_Geometry
     (C        : access Cell_Record;
      Geometry : access Cell_Geometry_Record'Class);
   -- Holds the geometry. Default is null.
      
   function Is_Vertex (C : access Cell_Record) return Boolean;
   procedure Set_Vertex
     (C      : access Cell_Record;
      Vertex : Boolean);
   
   function Is_Edge (C : access Cell_Record) return Boolean;
   procedure Set_Edge
     (C    : access Cell_Record;
      Edge : Boolean);
 
   function Get_Parent (C : access Cell_Record) return access Cell_Record;
   procedure Set_Parent
     (C      : access Cell_Record;
      Parent : access Cell_Record);
   procedure Reset_Parent (C : access Cell_Record);
   -- Returns the source terminal.
   
   function Get_Source (C : access Cell_Record) return access Cell_Record;
   procedure Set_Source
     (C      : access Cell_Record;
      Source : access Cell_Record'Class);
   -- Returns the source terminal.
   
   function Get_Target (C : access Cell_Record) return access Cell_Record;
   procedure Set_Target
     (C      : access Cell_Record;
      Target : access Cell_Record'Class);
   -- Returns the target terminal.
   
   function Get_Terminal 
     (C         : access Cell_Record;
      Is_Source : Boolean) return access Cell_Record;
   -- Returns the source or target terminal. Source Boolean specifies if the
   -- source terminal should be returned. 

   procedure Set_Terminal 
     (C         : access Cell_Record;
      Terminal  : access Cell_Record;
      Is_Source : Boolean);
   procedure Reset_Terminal 
     (C         : access Cell_Record;
      Is_Source : Boolean);
   -- Sets the source or target terminal and returns the new terminal. Terminal
   -- Cell represents the new source or target terminal. Source Boolean 
   -- specifies if the source or target terminal should be set.
   
   function Get_Children_List
     (C : access Cell_Record) return Cells_Lists.List;
   
   procedure Replace_Children_List
     (C        : access Cell_Record; 
      New_List : Cells_Lists.List);
   
   type Sort_Call_Back is 
     access function (Left, Right : Cell_Class_Ptr) return Boolean;
   -- Define a "<" call_back type to order cells.
   
   procedure Sort_Children (C : Cell_Class_Ptr; CB : Sort_Call_Back);
   
   function Get_Child_Count (C : access Cell_Record) return Integer;
   
   function Get_Index
     (C     : access Cell_Record;
      Child : access Cell_Record) return Integer;
   
   function Get_Child_At
     (C     : access Cell_Record;
      Index : Integer) return access Cell_Record;
   
   procedure Insert_Child_Before_Cell
     (C     : access Cell_Record;
      Cell  : access Cell_Record;
      Child : access Cell_Record);
   
   procedure Insert
     (C     : access Cell_Record;
      Child : access Cell_Record);
   -- @see com.mxgraph.model.mxICell#insert(com.mxgraph.model.mxICell)
   
   procedure Insert
     (C     : access Cell_Record;
      Child : access Cell_Record;
      Index : Integer);
   -- @see com.mxgraph.model.mxICell#insert(com.mxgraph.model.mxICell, int)
   
   procedure Remove
     (C     : access Cell_Record;
      Index : Integer);
   -- @see com.mxgraph.model.mxICell#remove(int)
   
   procedure Remove
     (C     : access Cell_Record;
      Child : access Cell_Record);
   -- @see com.mxgraph.model.mxICell#remove(com.mxgraph.model.mxICell)
   
   procedure Remove_From_Parent (C : access Cell_Record);
   -- @see com.mxgraph.model.mxICell#removeFromParent()
   
   function Get_Edges_List
     (C : access Cell_Record) return Cells_Lists.List;
   
   function Get_Edge_Count (C : access Cell_Record) return Integer;
   -- @see com.mxgraph.model.mxICell#getEdgeCount()
   
   function Get_Edge_Index
     (C    : access Cell_Record;
      Edge : access Cell_Record) return Integer;
   -- @see com.mxgraph.model.mxICell#getEdgeIndex(com.mxgraph.model.mxICell)
   
   function Get_Edge_At
     (C     : access Cell_Record;
      Index : Integer) return access Cell_Record;
   -- @see com.mxgraph.model.mxICell#getEdgeAt(int)
   
   procedure Insert_Edge
     (C           : access Cell_Record;
      Edge        : access Cell_Record;
      Is_Outgoing : Boolean);
   -- @see com.mxgraph.model.mxICell#insertEdge(com.mxgraph.model.mxICell, 
   -- boolean)
   
   procedure Remove_Edge
     (C           : access Cell_Record;
      Edge        : access Cell_Record;
      Is_Outgoing : Boolean);
   -- @see com.mxgraph.model.mxICell#removeEdge(com.mxgraph.model.mxICell, 
   -- boolean)
   
   procedure Remove_From_Terminal
     (C         : access Cell_Record;
      Is_Source : Boolean);
   -- @see com.mxgraph.model.mxICell#removeFromTerminal(boolean)
   
   function Get_Attribute
     (C    : access Cell_Record;
      Name : String) return String;
   -- Returns the specified attribute from the user object if it is an XML
   -- node.
   -- @param name Name of the attribute whose value should be returned.
   -- @return Returns the value of the given attribute or null.
   
   function Get_Attribute
     (C             : access Cell_Record;
      Name          : String;
      Default_Value : String) return String;
   -- Returns the specified attribute from the user object if it is an XML
   -- node.
   -- @param name Name of the attribute whose value should be returned.
   -- @param defaultValue Default value to use if the attribute has no value.
   -- @return Returns the value of the given attribute or defaultValue.
   
   procedure Set_Attribute
     (C     : access Cell_Record;
      Name  : String;
      Value : String);
   -- Sets the specified attribute on the user object if it is an XML node.
   -- @param name Name of the attribute whose value should be set.
   -- @param value New value of the attribute.
   
   --     function Clone (N : access Cell_Record) return access Cell_Record;
   
   function Clone
     (C                 : access Cell_Record;
      Include_Childrens : Boolean := False) return access Cell_Record; 
     
   function Clone_Value return Object_Ptr;
   
   -- Helper functions for Cells sets --
   -------------------------------------
   
   procedure Add_List_To_Cells_Set
     (Set   : in out Cells_Sets.Map;
      Cells : Cells_Lists.List);
   -- Add all cells of list Cells to the set Set. If a cell of the list Cells
   -- is already present in set, it is not aded. The list is not disrupted.
   
   procedure Add_Cell_To_Cells_Set
     (Set  : in out Cells_Sets.Map;
      Cell : access Cell_Record'Class);
   
   -- Cells graph reserved fields handlings --
   -------------------------------------------
   
   function Get_Depends (C : access Cell_Record'Class) return Cells_Lists.List;
   procedure Set_Depends (C       : access Cell_Record'Class;
                          Depends : Cells_Lists.List);
   procedure Sort_Depends (C : Cell_Class_Ptr; CB : Sort_Call_Back);
   
   function Is_Visited
     (C : access Cell_Record) return Boolean;
   procedure Set_Visited
     (C    : access Cell_Record;
      Mark : Boolean);
   
   function Get_Layer  (C     : access Cell_Record) return Integer;
   procedure Set_Layer
     (C     : access Cell_Record;
      Layer : Integer);
   
   function Get_Y_Layer
     (C : access Cell_Record) return Integer;
   procedure Set_Y_Layer
     (C     : access Cell_Record;
      Layer : Integer);
   
   function Get_Vertices_Forward
     (This : access Cell_Record) return Cells_Lists.List;
   
   procedure Set_Vertices_Forward
     (This : access Cell_Record;
      L    : Cells_Lists.List);
   
   procedure Sort_Vertices_Forward 
     (C : Cell_Class_Ptr; CB : Sort_Call_Back);
   
   function Get_Vertices_Backward
     (This : access Cell_Record) return Cells_Lists.List;
   
   procedure Set_Vertices_Backward
     (This : access Cell_Record;
      L    : Cells_Lists.List);
   
   procedure Sort_Vertices_Backward 
     (C : Cell_Class_Ptr; CB : Sort_Call_Back);
   
   procedure Append_Vertices_Forward
     (This : access Cell_Record;
      Cell : access Cell_Record);
   
   procedure Remove_Forward
     (This : access Cell_Record;
      Cell : access Cell_Record);
   
   procedure Append_Vertices_Backward
     (This : access Cell_Record;
      Cell : access Cell_Record);
   
   function Get_Virtual_Precedings
     (This : access Cell_Record) return Cells_Lists.List;
   
   procedure Append_Virtual_Preceding
     (This    : access Cell_Record;
      Cell    : access Cell_Record);
   
   function Has_Virtual_Preceding
     (This    : access Cell_Record;
      Cell    : access Cell_Record;
      Recurse : Boolean := False) return Boolean;
   
   function Get_Virtual_Followings
     (This : access Cell_Record) return Cells_Lists.List;
   
   procedure Append_Virtual_Following
     (This    : access Cell_Record;
      Cell    : access Cell_Record);
   
   function Has_Virtual_Following
     (This    : access Cell_Record;
      Cell    : access Cell_Record;
      Recurse : Boolean := False) return Boolean;
   
   function Get_Edge_Mark (This : access Cell_Record) return Boolean;
   
   procedure Set_Edge_Mark
     (This : access Cell_Record;
      Mark : Boolean);
   
   function Is_Parent_Ancestor
     (Parent : access Cell_Record'Class;
      Child  : access Cell_Record'Class) return Boolean;
   
   procedure Append_Depend
     (This : access Cell_Record;
      Edge : access Cell_Record);
   
   function Get_Max_Width (This : access Cell_Record) return Coordinate;
   procedure Set_Max_Width
     (This  : access Cell_Record;
      Width : Coordinate);
   
   function Get_Max_Height (This : access Cell_Record) return Coordinate;
   procedure Set_Max_Height
     (This  : access Cell_Record;
      Height : Coordinate);
   
   function Get_Bound_Mark (This : access Cell_Record) return Boolean;
   procedure Set_Bound_Mark
     (This : access Cell_Record;
      Mark : Boolean);
   
   function Get_Coordinate_Mark (This : access Cell_Record) return Boolean;   
   procedure Set_Coordinate_Mark
     (This : access Cell_Record;
      Mark : Boolean);
   
   --     function Get_Associated_Node (This : access Cell_Record) return access Node_Record'Class;
   --     procedure Set_Associated_Node
   --       (This : access Cell_Record; 
   --        N    : access Node_Record'Class);
   
private
   
   type Cell_Record is new Object_Record and Cell_Interface with record 
      Id : Name_Id;
      
      Value : access Object_Record'Class;
      --  Holds the user object. Default is null.
      
      Geometry : access Cell_Geometry_Record'Class;
      --  Holds the geometry. Default is null.

      Vertex      : Boolean;
      Edge        : Boolean;
            
      Parent : access Cell_Record;
      Source : access Cell_Record;
      Target : access Cell_Record;
      --  Reference to parent cell and source and target terminals for edges.
      
      Children : Cells_Lists.List;
      Edges    : Cells_Lists.List;
      --  Holds the child cells and connected edges.
            
      -----------------------------------------------------------------------
      --    Fields reserved to cells analysis
      Depends  : Cells_Lists.List;      
      --  Store the list of edges between two cells
      
      Visited : Boolean;
      --  True if cell was already visted, False otherwise
      
      -------------------------Node : access Node_Record'Class;
      Layer   : Integer;
      Y_Layer : Integer;
      
      Edge_Mark : Boolean;
      
      Vertices_Forward  : Cells_Lists.List;
      Vertices_Backward : Cells_Lists.List;
      
      Virtual_Precedings : Cells_Lists.List;
      Virtual_Followings : Cells_Lists.List;
      
      Max_Width : Coordinate;
      Max_Height : Coordinate;
      Bound_Mark : Boolean;
      Coordinate_Mark : Boolean;
   end record;
   
   No_Cell_Record : constant Cell_Record :=
     (No_Object_Record with
      Id        => No_Name,
      Value       => null,
      Geometry    => null,
      Vertex      => False,
      Edge        => False,
      
      Parent      => null,
      Source      => null,
      Target      => null,
      Children    => Cells_Lists.Empty_List,
      Edges       => Cells_Lists.Empty_List,
            
      Depends     => Cells_Lists.Empty_List,
      Visited     => False,
      -----------      Node        => null,
      Layer       => -1,
      Y_Layer     => -1,
      Edge_Mark   => False,
      
      Vertices_Forward  => Cells_Lists.Empty_List,
      Vertices_Backward => Cells_Lists.Empty_List,
      Virtual_Precedings => Cells_Lists.Empty_List,
      Virtual_Followings => Cells_Lists.Empty_List,
      
      Max_Width    => 0.0,
      Max_Height  => 0.0,
      Bound_Mark  => False,
      Coordinate_Mark => False);
      
end Artics.Graph.Cells;
