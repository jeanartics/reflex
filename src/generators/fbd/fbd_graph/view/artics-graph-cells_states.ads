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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;

with Artics.Types; use Artics.Types;
with Artics.Namet; use Artics.Namet;
with Artics.Output; use Artics.Output;

with Artics.Objects; use Artics.Objects;
with Artics.Geometry; use Artics.Geometry;
with Artics.Geometry.Points; use Artics.Geometry.Points;
with Artics.Geometry.Rectangles; use Artics.Geometry.Rectangles;
with Artics.Graph.Cells; use Artics.Graph.Cells;

with Artics.Utils; use Artics.Utils;

with Artics.Lists_Helpers;

-- Represents the current state of a cell in a given graph view.

package Artics.Graph.Cells_States is
   
   use Artics.Utils.Strings_Maps;
   use Artics.Geometry.Coordinates_Lists;
   use Artics.Geometry.Points.Point_Lists;
   
   type Cell_State_Record is new Object_Record with private;
   type Cell_State_Ptr is access all Cell_State_Record;
   type Cell_State_Class_Ptr is access all Cell_State_Record'Class;
   
   No_Cell_State_Record : constant Cell_State_Record;
   
   function Equivalent_Key
     (Left, Right : Cell_Class_Ptr) return Boolean;
   function Hash_Func
     (Key : Cell_Class_Ptr) return Ada.Containers.Hash_Type;
   
   package Cells_States_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Cell_Class_Ptr,
      Element_Type    => Cell_State_Ptr,
      Hash            => Hash_Func,
      Equivalent_Keys => Equivalent_Key);

   package Cells_States_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Cell_State_Ptr);
   
   package Cells_States_Lists_Helpers is new Artics.Lists_Helpers
     (Cell_State_Ptr, null, Cells_States_Lists);
      
     
   function New_Cell_State return access Cell_State_Record;
   -- Constructs a new object that represents the current state of the given
   -- cell in the specified view.
   
   function New_Cell_State
     (View  : access Object_Record'Class; -- View_Interface'Class;
      Cell  : access Cell_Record'Class) return access Cell_State_Record;
   -- Constructs a new object that represents the current state of the given
   -- cell in the specified view.
   -- @param view Graph view that contains the state.
   -- @param cell Cell that this state represents.
   -- @param style Array of key, value pairs that constitute the style.
   
   function Inside_Rectangle
     (C : access Cell_State_Record) return Rectangle_Record;
   procedure Set_Inside_Rectangle
     (C    : access Cell_State_Record;
      Rect : Rectangle_Record);
   
   function Get_View
     (C : access Cell_State_Record) return access Object_Record'Class; --View_Interface'Class;
   procedure Set_View
     (C    : access Cell_State_Record;
      View : access Object_Record'Class); --View_Interface'Class);
   -- Reference to the enclosing graph view.
      
   function Get_Cell
     (C : access Cell_State_Record) return access Cell_Record'Class;
   procedure Set_Cell
     (C    : access Cell_State_Record;
      Cell : access Cell_Record'Class);
   -- Reference to the cell that is represented by this state.
   
   function Get_Label (C : access Cell_State_Record) return String;
   procedure Set_Label
     (C     : access Cell_State_Record;
      Label : String);
   -- Holds the current label value, including newlines which result from
   -- word wrapping.

   function Get_Style (C : access Cell_State_Record) return Strings_Maps.Map;
   procedure Set_Style
     (C     : access Cell_State_Record;
      Style : Strings_Maps.Map);
   procedure Update_Style
     (C     : access Cell_State_Record;
      Key   : String;
      Value : String);
   procedure Update_Style
     (C     : access Cell_State_Record;
      Key   : Name_Id;
      Value : Name_Id);
   -- Contains an array of key, value pairs that represent the style of the
   -- cell.
      
   function Get_Origin (C : access Cell_State_Record) return Point_Record;
   procedure Set_Origin
     (C      : access Cell_State_Record;
      Origin : Point_Record);
   -- Holds the origin for all child cells.
   
   function Get_Absolute_Point 
     (C     : access Cell_State_Record;
      Index : Integer) return Point_Record;
   -- Returns the absolute point at the given index.
   -- @return the mxPoint at the given index
   
   procedure Set_Absolute_Point_At
     (C     : access Cell_State_Record;
      Index : Integer;
      Point : Point_Record);
   -- Returns the absolute point at the given index.
   -- @return the mxPoint at the given index
   
   procedure Remove_Absolute_Point_At
     (C     : access Cell_State_Record;
      Index : Integer);
   
   function Get_First_Absolute_Point
     (C : access Cell_State_Record) return Point_Record;
   
   function Get_Last_Absolute_Point
     (C : access Cell_State_Record) return Point_Record;
   
   function Get_Absolute_Points
     (C : access Cell_State_Record) return Point_Lists.List;
   procedure Set_Absolute_Points
     (C               : access Cell_State_Record;
      Absolute_Points : Point_Lists.List);
   -- List of mxPoints that represent the absolute points of an edge.
   
   function Get_Absolute_Point_Count
     (C : access Cell_State_Record) return Integer;
   -- Returns the number of absolute points.
   -- @return the absolutePoints
   
   function Get_Absolute_Offset
     (C : access Cell_State_Record) return Point_Record;
   procedure Set_Absolute_Offset
     (C               : access Cell_State_Record;
      Absolute_Offset :  Point_Record);
   -- Holds the absolute offset. For edges, this is the absolute coordinates
   -- of the label position. For vertices, this is the offset of the label
   -- relative to the top, left corner of the vertex.
      
   function Get_Terminal_Distance
     (C : access Cell_State_Record) return Coordinate;
   procedure Set_Terminal_Distance
     (C                 : access Cell_State_Record;
      Terminal_Distance : Coordinate);
   
   function Get_Length (C : access Cell_State_Record) return Coordinate;
   procedure Set_Length
     (C      : access Cell_State_Record;
      Length : Coordinate);
   -- Caches the distance between the end points and the length of an edge.

   function Get_Segments
     (C : access Cell_State_Record) return Coordinates_Lists.List;
   procedure Set_Segments
     (C        : access Cell_State_Record;
      Segments : Coordinates_Lists.List);
   -- Array of numbers that represent the cached length of each segment of
   -- the edge.
      
   function Get_Label_Bounds
     (C : access Cell_State_Record) return Rectangle_Record;
   procedure Set_Label_Bounds
     (C            : access Cell_State_Record;
      Label_Bounds : Rectangle_Record);
   -- Holds the rectangle which contains the label.
      
   function Get_Bounding_Box
     (C : access Cell_State_Record) return Rectangle_Record;
   procedure Set_Bounding_Box
     (C            : access Cell_State_Record;
      Bounding_Box : Rectangle_Record);
   -- Holds the largest rectangle which contains all rendering for this cell.
   
   function Get_Perimeter_Bounds
     (C : access Cell_State_Record) return Rectangle_Record;
   -- Returns the rectangle that should be used as the perimeter of the cell.
   -- This implementation adds the perimeter spacing to the rectangle defined
   -- by this cell state.
   -- @return Returns the rectangle that defines the perimeter.

   function Get_Perimeter_Bounds
     (C      : access Cell_State_Record;
      Border : Coordinate) return Rectangle_Record;
   -- Returns the rectangle that should be used as the perimeter of the cell.
   -- @return Returns the rectangle that defines the perimeter.
   
   procedure Set_Absolute_Terminal_Point
     (C         : access Cell_State_Record;
      Point     : Point_Record;
      Is_Source : Boolean);
   -- Sets the first or last point in the list of points depending on isSource.
   -- @param point Point that represents the terminal point.
   -- @param isSource Boolean that specifies if the first or last point should
   -- be assigned.
   
   function Is_Invalid (C : access Cell_State_Record) return Boolean;
   procedure Set_Invalid
     (C       : access Cell_State_Record;
      Invalid : Boolean);
   -- Specifies if the state is invalid. Default is true.
   
   function Get_Or_Create_External_Id
     (C  : access Cell_State_Record;
      Id : in out Integer) return Name_Id;
   function Get_External_Id (C : access Cell_State_Record) return Name_Id;
   procedure Set_External_Id
     (C  : access Cell_State_Record;
      Id : Name_Id);
   
   function Get_Visible_Terminal
     (C      : access Cell_State_Record;
      Source : Boolean) return access Cell_Record'Class;
   -- Returns the visible source or target terminal cell.
   -- @param source Boolean that specifies if the source or target cell should
   --   be returned.

   function Get_Visible_Terminal_State
     (C      : access Cell_State_Record;
      Source : Boolean) return access Cell_State_Record;
   -- Returns the visible source or target terminal state.
   -- @param Boolean that specifies if the source or target state should be
   -- returned.
   
   procedure Set_Visible_Terminal_State
     (C              : access Cell_State_Record;
      Terminal_State : access Cell_State_Record;
      Source : Boolean);
   -- Sets the visible source or target terminal state.
   -- @param terminalState Cell state that represents the terminal.
   -- @param source Boolean that specifies if the source or target state should
   -- be set.
   
   function Get_Visible_Source_State 
     (C : access Cell_State_Record) return access Cell_State_Record;
   function Get_Visible_Target_State
     (C : access Cell_State_Record) return access Cell_State_Record;
   procedure Set_Visible_Source_State
     (C : access Cell_State_Record;
      V : access Cell_State_Record);
   procedure Set_Visible_Target_State
     (C : access Cell_State_Record;
      V : access Cell_State_Record);
   -- Caches the visible source and target terminal states.
   
   -- Inside Rectangle --
   ----------------------
   
   --     function Get_Origin (C : access Cell_State_Record) return Point_record;
   --     procedure Set_Origin
   --       (C : access Cell_State_Record;
   --        P : Point_record);
   
   function Get_X (C : access Cell_State_Record) return Coordinate;
   procedure Set_X
     (C : access Cell_State_Record;
      X : Coordinate);
   -- Returns the x-coordinate of the center.
   
   function Get_Y (C : access Cell_State_Record) return Coordinate;
   procedure Set_Y
     (C : access Cell_State_Record;
      Y : Coordinate);
   -- Returns the y-coordinate of the center.
   
   function Get_Width (C : access Cell_State_Record) return Coordinate;
   procedure Set_Width
     (C : access Cell_State_Record;
      W : Coordinate);
   --  Returns or Sets the width of the rectangle.
   
   function Get_Height (C : access Cell_State_Record) return Coordinate;
   procedure Set_Height
     (C : access Cell_State_Record;
      H : Coordinate);
   --  Returns or Sets the height of the rectangle.
   
   function Get_Center_X (C : access Cell_State_Record) return Coordinate;
   -- Returns the x-coordinate of the center.
   -- @return Returns the x-coordinate of the center.

   function Get_Center_Y (C : access Cell_State_Record) return Coordinate;
   -- Returns the y-coordinate of the center.
   -- @return Returns the y-coordinate of the center.

   procedure Grow
     (C      : access Cell_State_Record;
      Amount : Coordinate);
   -- Grows the rectangle by the given amount, that is, this method subtracts
   -- the given amount from the x- and y-coordinates and adds twice the amount
   -- to the width and height.
   -- @param amount Amount by which the rectangle should be grown.

   function Contains
     (C : access Cell_State_Record;
      X : Coordinate;
      Y : Coordinate) return Boolean;
   -- Returns true if the given point is contained in the rectangle.
   -- @param x X-coordinate of the point.
   -- @param y Y-coordinate of the point.
   -- @return Returns true if the point is contained in the rectangle.

   function Intersect_Line
     (C  : access Cell_State_Record;
      X0 : Coordinate;
      Y0 : Coordinate;
      X1 : Coordinate;
      Y1 : Coordinate) return Point_Record;
   -- Returns the point at which the specified point intersects the perimeter 
   -- of this rectangle or null if there is no intersection.
   -- @param x0 the x co-ordinate of the first point of the line
   -- @param y0 the y co-ordinate of the first point of the line
   -- @param x1 the x co-ordinate of the second point of the line
   -- @param y1 the y co-ordinate of the second point of the line
   -- @return the point at which the line intersects this rectangle, or null
   -- 			if there is no intersection

   
   function Clone_Cell_State (C : Cell_State_Ptr) return Cell_State_Ptr;
   
private
   
   type Cell_State_Record is new Object_Record with record
      
      Inside_Rectangle : Rectangle_Record;
      -- public class mxCellState extends mxRectangle
      
      View : access Object_Record'Class; -- View_Interface'Class;
      -- Reference to the enclosing graph view.
      
      Cell : access Cell_Record'Class;
      -- Reference to the cell that is represented by this state.
      
      Label : Name_Id;
      -- Holds the current label value, including newlines which result from
      -- word wrapping.

      Style : Strings_Maps.Map;
      -- Contains an array of key, value pairs that represent the style of the
      -- cell.
      
      Origin : Point_Record;
      -- Holds the origin for all child cells.
      
      Absolute_Points : Point_Lists.List;
      -- List of mxPoints that represent the absolute points of an edge.
      
      Absolute_Offset : Point_Record;
      -- Holds the absolute offset. For edges, this is the absolute coordinates
      -- of the label position. For vertices, this is the offset of the label
      -- relative to the top, left corner of the vertex.
      
      Terminal_Distance : Coordinate;
      Length : Coordinate;
      -- Caches the distance between the end points and the length of an edge.

      Segments : Coordinates_Lists.List;
      -- Array of numbers that represent the cached length of each segment of
      -- the edge.
      
      Label_Bounds : Rectangle_Record;
      -- Holds the rectangle which contains the label.
      
      Bounding_Box : Rectangle_Record;
      -- Holds the largest rectangle which contains all rendering for this cell.
      
      Invalid : Boolean;
      -- Specifies if the state is invalid. Default is true.
      
      External_Id : Name_Id;
      
      Visible_Source_State : access Cell_State_Record;
      Visible_Target_State : access Cell_State_Record;
      -- Caches the visible source and target terminal states.
   end record;      
   
   No_Cell_State_Record : constant Cell_State_Record := Cell_State_Record'
     (No_Object_Record with      
      Inside_Rectangle   => No_Rectangle_Record,
      View                 => null,
      Cell                 => null,
      Label                => No_Name,
      Style                => Strings_Maps.Empty_Map,
      Origin               => No_Point_Record,
      Absolute_Points      => Point_Lists.Empty_List,
      Absolute_Offset      => No_Point_Record,
      Terminal_Distance    => No_Coordinate,
      Length               => No_Coordinate,
      Segments             => Coordinates_Lists.Empty_List,
      Label_Bounds         => No_Rectangle_Record,
      Bounding_Box         => No_Rectangle_Record,
      Invalid              => True,
      External_Id          => No_Name,
      Visible_Source_State => null,
      Visible_Target_State => null);

end Artics.Graph.Cells_States;

