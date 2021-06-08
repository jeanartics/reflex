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
-- with ada.Text_IO; use ada.Text_IO;

with Ada.Unchecked_Conversion;

package body Artics.Graph.Cells_States is
   
   -------------------
   -- Hash_Function --
   -------------------
   
   function Equivalent_Key
     (Left, Right : Cell_Class_Ptr) return Boolean is
   begin
      return Left = Right;
   end Equivalent_Key;
 
   function Hash_Func
     (Key : Cell_Class_Ptr) return Ada.Containers.Hash_Type is
      
      function To_Integer is new Ada.Unchecked_Conversion
        (Cell_Class_Ptr, Integer);
      K : Integer := To_Integer (Key);
   begin
      return Ada.Containers.Hash_Type (K mod 2**5);
   end Hash_Func;
   
   --------------------
   -- New_Cell_State --
   --------------------
   
   function New_Cell_State return access Cell_State_Record is
      C : Cell_State_Ptr := new Cell_State_Record'(No_Cell_State_Record);
   begin
      return C;
   end New_Cell_State;
   
   --------------------
   -- New_Cell_State --
   --------------------
   
   function New_Cell_State
     (View  : access Object_Record'Class; -- View_Interface'Class;
      Cell  : access Cell_Record'Class) return access Cell_State_Record 
   is      
      C : access Cell_State_Record :=  New_Cell_State;
   begin
      C.View  := View;
      C.Cell  := Cell;
      return C;
   end New_Cell_State;
   
   ----------------------
   -- Inside_Rectangle --
   ----------------------
   
   function Inside_Rectangle
     (C : access Cell_State_Record) return Rectangle_Record is
   begin
      return C.Inside_Rectangle;
   end Inside_Rectangle;
   
   --------------------------
   -- Set_Inside_Rectangle --
   --------------------------
   
   procedure Set_Inside_Rectangle
     (C    : access Cell_State_Record;
      Rect : Rectangle_Record) is
   begin
      C.Inside_Rectangle := Rect;
   end Set_Inside_Rectangle;
      
   ----------
   -- View --
   ----------
   
   function Get_View
     (C : access Cell_State_Record) return access Object_Record'Class is -- View_Interfac'Class is
   begin
      return C.View;
   end Get_View;
   
   --------------
   -- Set_View --
   --------------
   
   procedure Set_View
     (C    : access Cell_State_Record;
      View : access Object_Record'Class) is -- View_Interface'Class) is
   begin
      C.View := View;
   end Set_View;
   
   ----------
   -- Cell --
   ----------
   
   function Get_Cell
     (C : access Cell_State_Record) return access Cell_Record'Class is
   begin
      return C.Cell;
   end Get_Cell;
   
   --------------
   -- Set_Cell --
   --------------
   
   procedure Set_Cell
     (C    : access Cell_State_Record;
      Cell : access Cell_Record'Class) is
   begin
      C.Cell := Cell;
   end Set_Cell;
   
   -----------
   -- Label --
   -----------
   
   function Get_Label (C : access Cell_State_Record) return String is
   begin
      return Get_String (C.Label);
   end Get_Label;
   
   ---------------
   -- Set_Label --
   ---------------
   
   procedure Set_Label
     (C     : access Cell_State_Record;
      Label : String) is
   begin
      C.Label := String_Find (Label);
   end Set_Label;
   
   ---------------
   -- Set_Style --
   ---------------
   
   function Get_Style (C : access Cell_State_Record) return Strings_Maps.Map is
   begin
      return C.Style;
   end Get_Style;
   
   ---------------
   -- Set_Style --
   ---------------
   
   procedure Set_Style
     (C     : access Cell_State_Record;
      Style : Strings_Maps.Map) is
   begin
      C.Style := Style;
   end Set_Style;
   
   ------------------
   -- Update_Style --
   ------------------
   
   procedure Update_Style
     (C     : access Cell_State_Record;
      Key   : String;
      Value : String) is
   begin
      Update_Style (C, String_Find (Key), String_Find (Value));
   end Update_Style;
     
   procedure Update_Style
     (C     : access Cell_State_Record;
      Key   : Name_Id;
      Value : Name_Id) is
   begin
      if Strings_Maps.Contains (C.Style, Key) then
         Strings_Maps.Replace (C.Style, Key, Value);
      else
         Strings_Maps.Insert (C.Style, Key, Value);
      end if;
   end Update_Style;
     
   ------------
   -- Origin --
   ------------
   
   function Get_Origin (C : access Cell_State_Record) return Point_Record is
   begin
      return C.Origin;
   end Get_Origin;
   
   ----------------
   -- Set_Origin --
   ----------------
   
   procedure Set_Origin
     (C      : access Cell_State_Record;
      Origin : Point_Record) is
   begin
      C.Origin := Origin;
   end Set_Origin;
   
   ------------------------
   -- Get_Absolute_Point --
   ------------------------
   
   function Get_Absolute_Point 
     (C     : access Cell_State_Record;
      Index : Integer) return Point_Record is
   begin
      return Get_Point_At (C.Absolute_Points, Index);
   end Get_Absolute_Point;
   
   ------------------------
   -- Set_Absolute_Point --
   ------------------------
   
   procedure Set_Absolute_Point_At
     (C     : access Cell_State_Record;
      Index : Integer;
      Point : Point_Record) is
   begin
      Set_Point_At (C.Absolute_Points, Index, Point);
   end Set_Absolute_Point_At;
   
   ------------------------
   -- Set_Absolute_Point --
   ------------------------
   
   procedure Remove_Absolute_Point_At
     (C     : access Cell_State_Record;
      Index : Integer) is
   begin
      Remove_Point_At (C.Absolute_Points, Index);
   end Remove_Absolute_Point_At;
   
   ------------------------------
   -- Get_First_Absolute_Point --
   ------------------------------
   
   function Get_First_Absolute_Point
     (C : access Cell_State_Record) return Point_Record is
   begin
      return Get_First_Point (C.Absolute_Points);
   end Get_First_Absolute_Point;
   
   -----------------------------
   -- Get_Last_Absolute_Point --
   -----------------------------
   
   function Get_Last_Absolute_Point
     (C : access Cell_State_Record) return Point_Record is
   begin
      return Get_Last_Point (C.Absolute_Points);
   end Get_Last_Absolute_Point;
   
   ---------------------
   -- Absolute_Points --
   ---------------------
   
   function Get_Absolute_Points
     (C : access Cell_State_Record) return Point_Lists.List is
   begin
      return C.Absolute_Points;
   end Get_Absolute_Points;
   
   -------------------------
   -- Set_Absolute_Points --
   -------------------------
   
   procedure Set_Absolute_Points
     (C               : access Cell_State_Record;
      Absolute_Points : Point_Lists.List) is
   begin
      C.Absolute_Points := Absolute_Points;
   end Set_Absolute_Points;
   
   ------------------------------
   -- Get_Absolute_Point_Count --
   ------------------------------
   
   function Get_Absolute_Point_Count
     (C : access Cell_State_Record) return Integer is
   begin
      if C.Absolute_Points = Point_Lists.Empty_List then
         return 0;
      else
         return Integer (Point_Lists.Length (C.Absolute_Points));
      end if;
   end Get_Absolute_Point_Count;
   
  
   ---------------------
   -- Absolute_Offset --
   ---------------------
   
   function Get_Absolute_Offset
     (C : access Cell_State_Record) return Point_Record is
   begin
      return C.Absolute_Offset;
   end Get_Absolute_Offset;
   
   -------------------------
   -- Set_Absolute_Offset --
   -------------------------
   
   procedure Set_Absolute_Offset
     (C               : access Cell_State_Record;
      Absolute_Offset :  Point_Record) is
   begin
      C.Absolute_Offset := Absolute_Offset;
   end Set_Absolute_Offset;
   
   -----------------------
   -- Terminal_Distance --
   -----------------------
   
   function Get_Terminal_Distance
     (C : access Cell_State_Record) return Coordinate is
   begin
      return C.Terminal_Distance;
   end Get_Terminal_Distance;
   
   ---------------------------
   -- Set_Terminal_Distance --
   ---------------------------
   
   procedure Set_Terminal_Distance
     (C                 : access Cell_State_Record;
      Terminal_Distance : Coordinate) is
   begin
      C.Terminal_Distance := Terminal_Distance;
   end Set_Terminal_Distance;
   
   ------------
   -- Length --
   ------------
   
   function Get_Length (C : access Cell_State_Record) return Coordinate is
   begin
      return C.Length;
   end Get_Length;
   
   ----------------
   -- Set_Length --
   ----------------
   
   procedure Set_Length
     (C      : access Cell_State_Record;
      Length : Coordinate) is
   begin
      C.Length := Length;
   end Set_Length;
   
   ------------------
   -- Get_Segments --
   ------------------
   
   function Get_Segments
     (C : access Cell_State_Record) return Coordinates_Lists.List is
   begin
      return C.Segments;
   end Get_Segments;
   
   -------------------
   -- Set_Segements --
   -------------------
   
   procedure Set_Segments
     (C        : access Cell_State_Record;
      Segments : Coordinates_Lists.List) is
   begin
      C.Segments := Segments;
   end Set_Segments;
      
   
   ------------------
   -- Label_Bounds --
   ------------------
   
   function Get_Label_Bounds
     (C : access Cell_State_Record) return Rectangle_Record is
   begin
      return C.Label_Bounds;
   end Get_Label_Bounds;
   
   ----------------------
   -- Set_Label_Bounds --
   ----------------------
   
   procedure Set_Label_Bounds
     (C            : access Cell_State_Record;
      Label_Bounds : Rectangle_Record) is
   begin
      C.Label_Bounds := Label_Bounds;
   end Set_Label_Bounds;
   
   ------------------
   -- Bounding_Box --
   ------------------
   
   function Get_Bounding_Box
     (C : access Cell_State_Record) return Rectangle_Record is
   begin
      return C.Bounding_Box;
   end Get_Bounding_Box;
   
   ----------------------
   -- Set_Bounding_Box --
   ----------------------
   
   procedure Set_Bounding_Box
     (C            : access Cell_State_Record;
      Bounding_Box : Rectangle_Record) is
   begin
      C.Bounding_Box := Bounding_Box;
   end Set_Bounding_Box;
   
   ----------------
   -- Is_Invalid --
   ----------------
   
   function Is_Invalid (C : access Cell_State_Record) return Boolean is
   begin
      return C.Invalid;
   end Is_Invalid;
   
   -----------------
   -- Set_Invalid --
   -----------------
   
   procedure Set_Invalid
     (C       : access Cell_State_Record;
      Invalid : Boolean) is
   begin
      C.Invalid := Invalid;
   end Set_Invalid;
   
   -------------------------------
   -- Get_External_Or_Create_Id --
   -------------------------------
   
   function Get_Or_Create_External_Id
     (C  : access Cell_State_Record;
      Id : in out Integer) return Name_Id is
   begin
      if C.External_Id = No_Name then
         Id := Id + 1;
         declare
            S : String := Integer_To_String (Id);
         begin
            C.External_Id := String_Find (S);
         end;
      end if;	  
	  
      return C.External_Id;
   end Get_Or_Create_External_Id;
   
   ---------------------
   -- Get_External_Id --
   ---------------------
   
   function Get_External_Id (C : access Cell_State_Record) return Name_Id is
   begin
      return C.External_Id;
   end Get_External_Id;
   
   ---------------------
   -- Set_External_Id --
   ---------------------
   
   procedure Set_External_Id
     (C  : access Cell_State_Record;
      Id : Name_Id) is
   begin
      C.External_Id := Id;
   end Set_External_Id;
   
   --------------------------
   -- Get_Visible_Terminal --
   --------------------------
   
   function Get_Visible_Terminal
     (C      : access Cell_State_Record;
      Source : Boolean) return access Cell_Record'Class 
   is      
      Tmp : access Cell_State_Record;
   begin
      Tmp := Get_Visible_Terminal_State (C, Source);
      
      if Tmp /= null then
         return Tmp.Cell;
      else
         return null;
      end if;
   end Get_Visible_Terminal;
   
   --------------------------------
   -- Get_Visible_Terminal_State --
   --------------------------------
   
   function Get_Visible_Terminal_State
     (C      : access Cell_State_Record;
      Source : Boolean) return access Cell_State_Record is
   begin
      if Source then
         return C.Visible_Source_State;
      else
         return C.Visible_Target_State;
      end if;
   end Get_Visible_Terminal_State;
   
   --------------------------------
   -- Set_Visible_Terminal_State --
   --------------------------------
   
   procedure Set_Visible_Terminal_State
     (C              : access Cell_State_Record;
      Terminal_State : access Cell_State_Record;
      Source         : Boolean) is
   begin
      if Source then
         C.Visible_Source_State := Terminal_State;
      else
         C.Visible_Target_State := Terminal_State;
      end if;
   end Set_Visible_Terminal_State;
   
   --------------------------
   -- Visible_Source_State --
   --------------------------
   
   function Get_Visible_Source_State
     (C : access Cell_State_Record) return access Cell_State_Record is
   begin
      return C.Visible_Source_State;
   end Get_Visible_Source_State;
   
   --------------------------
   -- Visible_Target_State --
   --------------------------
   
   function Get_Visible_Target_State
     (C : access Cell_State_Record) return access Cell_State_Record is
   begin
      return C.Visible_Target_State;
   end Get_Visible_Target_State;
   
   ------------------------------
   -- Set_Visible_Source_State --
   ------------------------------
   
   procedure Set_Visible_Source_State
     (C : access Cell_State_Record;
      V : access Cell_State_Record) is
   begin
      C.Visible_Source_State := V;
   end Set_Visible_Source_State;
   
   ------------------------------
   -- Set_Visible_Target_State --
   ------------------------------
   
   procedure Set_Visible_Target_State
     (C : access Cell_State_Record;
      V : access Cell_State_Record) is 
   begin
      C.Visible_Target_State := V;
   end Set_Visible_Target_State;
   
   --------------------------
   -- Get_Perimeter_Bounds --
   --------------------------
   
   function Get_Perimeter_Bounds
     (C : access Cell_State_Record) return Rectangle_Record is
   begin
      return Get_Perimeter_Bounds (C, 0.0);
   end Get_Perimeter_Bounds;

   --------------------------
   -- Get_Perimeter_Bounds --
   --------------------------
   
   function Get_Perimeter_Bounds
     (C      : access Cell_State_Record;
      Border : Coordinate) return Rectangle_Record is
      
      Bounds : Rectangle_Record;
   begin
      Bounds := Inside_Rectangle (C);
      
      if Border /= 0.0 then
         Grow (Bounds, Border);
      end if;
      
      return Bounds;
   end Get_Perimeter_Bounds;
   
   ---------------------------------
   -- Set_Absolute_Terminal_Point --
   ---------------------------------
   
   procedure Set_Absolute_Terminal_Point
     (C         : access Cell_State_Record;
      Point     : Point_Record;
      Is_Source : Boolean) is
   begin
      -- Source terminal point. 
      
      if Is_Source then
         if Point_Lists.Is_Empty (C.Absolute_Points) then
            Point_Lists.Append (C.Absolute_Points, Point);
            Point_Lists.Append (C.Absolute_Points, No_Point_Record);
         else
            Point_Lists.Delete_First (C.Absolute_Points);
            Point_Lists.Prepend (C.Absolute_Points, Point);
         end if;
	 
         -- Target terminal point. 
      
      else
         if Point_Lists.Is_Empty (C.Absolute_Points) then
            Point_Lists.Append (C.Absolute_Points, No_Point_Record);
            Point_Lists.Append (C.Absolute_Points, Point);
         else
            if Point_Lists.Length (C.Absolute_Points) = 1 then
               Point_Lists.Append (C.Absolute_Points, Point);
            else
               Point_Lists.Delete_Last (C.Absolute_Points);
               Point_Lists.Append (C.Absolute_Points, Point);
            end if;
         end if;
      end if;
   end Set_Absolute_Terminal_Point;
   
   
   -- Inside Rectangle --
   ----------------------
   
   ----------------
   -- Get_Origin --
   ----------------
   
   --     function Get_Origin (C : access Cell_State_Record) return Point_Record is
   --     begin
   --        return Get_Origin (C.Inside_Rectangle);
   --     end Get_Origin;
   
   ----------------
   -- Set_Origin --
   ----------------
   
   --     procedure Set_Origin
   --       (C : access Cell_State_Record;
   --        P : Point_record) is
   --     begin
   --        Set_Origin (C.Inside_Rectangle, P);
   --     end Set_Origin;
   
   -----------
   -- Get_X --
   -----------
   
   function Get_X (C : access Cell_State_Record) return Coordinate is
   begin
      return Get_X (C.Inside_Rectangle);
   end Get_X;
   
   -----------
   -- Set_X --
   -----------
   
   procedure Set_X
     (C : access Cell_State_Record;
      X : Coordinate) is
   begin
      Set_X (C.Inside_Rectangle, X);
   end Set_X;
   
   -----------
   -- Get_Y --
   -----------
   
   function Get_Y (C : access Cell_State_Record) return Coordinate is
   begin
      return Get_Y (C.Inside_Rectangle);
   end Get_Y;
   
   -----------
   -- Set_Y --
   -----------
   
   procedure Set_Y
     (C : access Cell_State_Record;
      Y : Coordinate) is
   begin
      Set_Y (C.Inside_Rectangle, Y);
   end Set_Y;
   
   ---------------
   -- Get_Width --
   ---------------
   
   function Get_Width (C : access Cell_State_Record) return Coordinate is
   begin
      return Get_Width (C.Inside_Rectangle);
   end Get_Width;
   
   ---------------
   -- Set_Width --
   ---------------
   
   procedure Set_Width
     (C : access Cell_State_Record;
      W : Coordinate) is
   begin
      Set_Width (C.Inside_Rectangle, W);
   end Set_Width;
   
   ----------------
   -- Get_Height --
   ----------------
   
   function Get_Height (C : access Cell_State_Record) return Coordinate is
   begin
      return Get_Height (C.Inside_Rectangle);
   end Get_Height;
   
   ----------------
   -- Set_Height --
   ----------------
   
   procedure Set_Height
     (C : access Cell_State_Record;
      H : Coordinate) is
   begin
      Set_Height (C.Inside_Rectangle, H);
   end Set_Height;
   
   ------------------
   -- Get_Center_X --
   ------------------
   
   function Get_Center_X (C : access Cell_State_Record) return Coordinate is
   begin
      return Get_Center_X (C.Inside_Rectangle);
   end Get_Center_X;
   
   ------------------
   -- Get_Center_Y --
   ------------------
   
   function Get_Center_Y (C : access Cell_State_Record) return Coordinate is
   begin
      return Get_Center_Y (C.Inside_Rectangle);
   end Get_Center_Y;
   
   ----------
   -- Grow --
   ----------
   
   procedure Grow
     (C      : access Cell_State_Record;
      Amount : Coordinate) is
   begin
      Grow (C.Inside_Rectangle, Amount);
   end Grow;
   
   --------------
   -- Contains --
   --------------
   
   function Contains
     (C : access Cell_State_Record;
      X : Coordinate;
      Y : Coordinate) return Boolean is
   begin
      return Contains (C.Inside_Rectangle, X, Y);
   end Contains;
   
   --------------------
   -- Intersect_Line --
   --------------------
   
   function Intersect_Line
     (C  : access Cell_State_Record;
      X0 : Coordinate;
      Y0 : Coordinate;
      X1 : Coordinate;
      Y1 : Coordinate) return Point_Record is
   begin
      return Intersect_Line 
        (C.Inside_Rectangle, 
         Point_Record'(X0, Y0), 
         Point_Record'(X1, Y1));
   end Intersect_Line;
   
   
   ----------------------
   -- Clone_Cell_State --
   ----------------------
   
   function Clone_Cell_State (C : Cell_State_Ptr) return Cell_State_Ptr is
      Point : Point_Record;
      Clone : Cell_State_Ptr;
   begin
      Clone := New_Cell_State (View  => C.Get_View,
                               Cell  => C.Get_Cell);
      
      Clone.Label             := C.Label;
      Clone.Inside_Rectangle  := C.Inside_Rectangle;
      Clone.Terminal_Distance := C.Terminal_Distance;
      Clone.Segments          := C.Segments;
      Clone.Origin            := C.Origin;
      Clone.Absolute_Offset   := C.Absolute_Offset;
      Clone.Label_Bounds      := C.Label_Bounds;
      Clone.Bounding_Box      := C.Bounding_Box;
      
      for P of C.Absolute_Points loop
         Point := P;
         Point_Lists.Append (C.Absolute_Points, Point);
      end loop;
      
      return Clone;
   end Clone_Cell_State;
   
end Artics.Graph.Cells_States;

