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

with Ada.Text_Io; 

with Artics.Utils; use Artics.Utils;

package body Artics.Graph.Cells_Geometry is
   procedure Put_Line (S : String) is null; -- renames Ada.Text_IO.Put_Line;
   
   use Point_Lists;
   
   -----------------------
   -- New_Cell_Geometry --
   -----------------------
   
   function New_Cell_Geometry return Cell_Geometry_Ptr is
      Geo : Cell_Geometry_Ptr := 
	new Cell_Geometry_Record'(No_Cell_Geometry_Record);
   begin
      return Geo;
   end New_Cell_Geometry;
   
   -----------------------
   -- New_Cell_Geometry --
   -----------------------
   
   function New_Cell_Geometry
     (X      : Coordinate;
      Y      : Coordinate;
      Width  : Coordinate;
      height : Coordinate) return Cell_Geometry_Ptr 
   is      
      Geo : Cell_Geometry_Ptr := New_Cell_Geometry;
   begin
      Set_X      (Geo.Rect, X);
      Set_Y      (Geo.Rect, Y);
      Set_Width  (Geo.Rect, Width);
      Set_Height (Geo.Rect, Height);
      
      return Geo;
   end New_Cell_Geometry;
   
   ------------------------
   -- Geometry_Rectangle --
   ------------------------
   
   function Geometry_Rectangle
     (G : access Cell_Geometry_Record) return Rectangle_Record is
   begin
      return G.Rect;
   end Geometry_Rectangle;
   
   ----------------------------
   -- Set_Geometry_Rectangle --
   ----------------------------
   
   procedure Set_Geometry_Rectangle
     (G : access Cell_Geometry_Record;
      R : Rectangle_Record) is
   begin
      G.Rect := R;
   end Set_Geometry_Rectangle;
   
   ------------------------------
   -- Translate_Control_Points --
   ------------------------------
   
   function Get_Translate_Control_Points
     (G : access Cell_Geometry_Record) return Boolean is
   begin
      return G.Translate_Control_Points;
   end Get_Translate_Control_Points;
   
   ----------------------------------
   -- Set_Translate_Control_Points --
   ----------------------------------
   
   procedure Set_Translate_Control_Points
     (G : access Cell_Geometry_Record;
      B : Boolean) is
   begin
      G.Translate_Control_Points := B;
   end Set_Translate_Control_Points;
   
   ----------------------
   -- Alternate_Bounds --
   ----------------------
   
   function Get_Alternate_Bounds
     (G : access Cell_Geometry_Record) return Rectangle_Record is
   begin
      return G.Alternate_Bounds;
   end Get_Alternate_Bounds;
   
   --------------------------
   -- Set_Alternate_Bounds --
   --------------------------
   
   procedure Set_Alternate_Bounds
     (G : access Cell_Geometry_Record;
      R : Rectangle_Record) is
   begin
      G.Alternate_Bounds := R;
   end Set_Alternate_Bounds;
   
   ------------------
   -- Source_Point --
   ------------------
   
   function Get_Source_Point
     (G : access Cell_Geometry_Record) return Point_Record is
   begin
      return G.Source_Point;
   end Get_Source_Point;
   
   ----------------------
   -- Set_Source_Point --
   ----------------------
   
   procedure Set_Source_Point
     (G : access Cell_Geometry_Record;
      S : Point_Record) is
   begin
      G.Source_Point := S;
   end Set_Source_Point;
   
   ------------------
   -- Target_Point --
   ------------------
   
   function Get_Target_Point
     (G : access Cell_Geometry_Record) return Point_Record is
   begin
      return G.Target_Point;
   end Get_Target_Point;
   
   ----------------------
   -- Set_Target_Point --
   ----------------------
   
   procedure Set_Target_Point
     (G : access Cell_Geometry_Record;
      T : Point_Record) is
   begin
      G.Target_Point := T;
   end Set_Target_Point;
   
   --------------------
   -- Terminal_Point --
   --------------------
   
   function Get_Terminal_Point
     (G      : access Cell_Geometry_Record;
      Source : boolean) return Point_Record is
   begin
      if Source then
	 return G.Source_Point;
      else
	 return G.Target_Point;
      end if;
   end Get_Terminal_Point;
   
   ------------------------
   -- Set_Terminal_Point --
   ------------------------
   
   procedure Set_Terminal_Point
     (G      : access Cell_Geometry_Record;
      T      : Point_Record;
      Source : Boolean) is
   begin
      if Source then
	 G.Source_Point := T;
      else
	 G.Target_Point := T;
      end if;
   end Set_Terminal_Point;
   
   ------------
   -- Points --
   ------------
   
   function Get_Points
     (G : access Cell_Geometry_Record) return Point_Lists.List is
   begin
      return G.Points;
   end Get_Points;
   
   ----------------
   -- Set_Points --
   ----------------
   
   procedure Set_Points
     (G : access Cell_Geometry_Record;
      L : Point_Lists.List) is
   begin
      G.Points := L;
   end Set_Points;
   
   ------------
   -- Offset --
   ------------
   
   function Get_Offset (G : access Cell_Geometry_Record) return Point_Record is
   begin
      return G.Offset;
   end Get_Offset;
   
   ----------------
   -- Set_Offset --
   ----------------
   
   procedure Set_Offset
     (G : access Cell_Geometry_Record;
      O : Point_Record) is
   begin
      G.Offset := O;
   end Set_Offset;
   
   --------------
   -- Relative --
   --------------
   
   function Is_Relative (G : access Cell_Geometry_Record) return Boolean is
   begin
      return G.Relative;
   end Is_Relative;
   
   ------------------
   -- Set_Relative --
   ------------------
   
   procedure Set_Relative
     (G : access Cell_Geometry_Record;
      R : Boolean) is
   begin
      G.Relative := R;
   end Set_Relative;
   
   ----------
   -- Swap --
   ----------
   
   procedure Swap (G : access Cell_Geometry_Record) is
      Old : Rectangle_Record;
   begin
      if G.Alternate_Bounds /= No_Rectangle_Record then
	 Old := G.Rect;
	 G.Rect := G.Alternate_Bounds;
	 G.Alternate_Bounds := Old;
      end if;
   end Swap;
   
   ---------------
   -- Translate --
   ---------------
   
   procedure Translate
     (G  : access Cell_Geometry_Record;
      Dx : Coordinate;
      Dy : Coordinate) is
   begin
      -- Translates the geometry
      
      if not G.Relative then
	 Set_X (G.Rect, Get_X (G.Rect) + Dx);
	 Set_Y (G.Rect, Get_Y (G.Rect) + Dy);
      end if;

      -- Translates the source point
      
      if G.Source_Point /= No_Point_Record then
	 Set_X (G.Source_Point, Get_X (G.Source_Point) + Dx);
	 Set_Y (G.Source_Point, Get_Y (G.Source_Point) + Dy);
      end if;

      -- Translates the target point
      
      if G.Target_Point /= No_Point_Record then
	 Set_X (G.Target_Point, Get_X (G.Target_Point) + Dx);
	 Set_Y (G.Target_Point, Get_Y (G.Target_Point) + Dy);
      end if;

      -- Translate the control points
      
      if G.Translate_Control_Points and G.Points /= Point_Lists.Empty_List then
	 
         for Pt of G.Points loop
	    Set_X (Pt, Get_X (Pt) + Dx);
	    Set_Y (Pt, Get_Y (Pt) + Dy);
         end loop;
      end if;
   end Translate;
   
   
   -- Inside Rectangle --
   ----------------------
   
   ----------------
   -- Get_Center --
   ----------------
   
   function Get_Origin (G : access Cell_Geometry_Record) return Point_Record is
   begin
      return Get_Origin (G.Rect);
   end Get_Origin;
   
   ----------------
   -- Set_Center --
   ----------------
   
   procedure Set_Origin
     (G : access Cell_Geometry_Record;
      p : Point_record) is
   begin
      Set_Origin (G.Rect, P);
   end Set_Origin;
   
   -----------
   -- Get_X --
   -----------
   
   function Get_X (G : access Cell_Geometry_Record) return Coordinate is
   begin
      return Get_X (G.Rect);
   end Get_X;
   
   -----------
   -- Set_X --
   -----------
   
   procedure Set_X
     (G : access Cell_Geometry_Record;
      X : Coordinate) is
   begin
      Set_X (G.Rect, X);
   end Set_X;
   
   -----------
   -- Get_Y --
   -----------
   
   function Get_Y (G : access Cell_Geometry_Record) return Coordinate is
   begin
      return Get_Y (G.Rect);
   end Get_Y;
   
   -----------
   -- Set_Y --
   -----------
   
   procedure Set_Y
     (G : access Cell_Geometry_Record;
      Y : Coordinate) is
   begin
      Set_Y (G.Rect, Y);
   end Set_Y;
   
   ---------------
   -- Get_Width --
   ---------------
   
   function Get_Width (G : access Cell_Geometry_Record) return Coordinate is
   begin
      return Get_Width (G.Rect);
   end Get_Width;
   
   ---------------
   -- Set_Width --
   ---------------
   
   procedure Set_Width
     (G : access Cell_Geometry_Record;
      W : Coordinate) is
   begin
      Set_Width (G.Rect, W);
   end Set_Width;
   
   ----------------
   -- Get_Height --
   ----------------
   
   function Get_Height (G : access Cell_Geometry_Record) return Coordinate is
   begin
      return Get_Height (G.Rect);
   end Get_Height;
   
   ----------------
   -- Set_Height --
   ----------------
   
   procedure Set_Height
     (G : access Cell_Geometry_Record;
      H : Coordinate) is
   begin
      Set_Height (G.Rect, H);
   end Set_Height;
   
   --------------------
   -- Clone_Geometry --
   --------------------
   
   function Clone (G : access Cell_Geometry_Record) 
                   return access Cell_Geometry_Record 
   is
   begin
      return new Cell_Geometry_Record'
        (Object_Record with
         Rect                     => G.Geometry_Rectangle,
         Translate_Control_Points => G.Get_Translate_Control_Points,
         Alternate_Bounds         => G.Alternate_Bounds,
         Source_Point             => G.Get_Source_Point,
         Target_Point             => G.Get_Target_Point,
         Points                   => G.Get_Points,
         Offset                   => G.Get_Offset,
         Relative                 => G.Is_Relative);
   end Clone;
   
   --------------------------------
   -- Generate_Xml_Cell_Geometry --
   --------------------------------
   
   procedure Generate_Xml_Cell_Geometry
     (This : access Cell_Geometry_Record;
      Ob   : Output_Buffer) is
   begin
      null;
   end Generate_Xml_Cell_Geometry;
   
   ----------------------------------
   -- Generate_Xml_Vertex_Geometry --
   ----------------------------------
   
   procedure Generate_Xml_Vertex_Geometry
     (This : access Cell_Geometry_Record;
      Ob   : Output_Buffer) is
   begin
      Write_Indent_Str
	(Ob, "<geometry" &
	   " translate-control=""" & 
	   Boolean_To_String (This.Translate_Control_Points) & """" & 
	   " relative=""" & Boolean_To_String (This.Relative) & """");
      
      if This.Rect = No_Rectangle_Record then
	 Write_Str (Ob, " />");
	 Write_Eol (Ob);
      else
	 Write_Str (Ob, " >");
	 Write_Eol (Ob);
      end if;
      
      Inc_Indent (Ob);
      -----Generate_Xml_Rectangle (This.Rect, Ob);
      Dec_Indent (Ob);
      
      Write_Indent_Str (Ob, "</geometry>");
      Write_Eol (Ob);
   end Generate_Xml_Vertex_Geometry;
   
   ----------------------------------
   -- Generate_Xml_Vertex_Geometry --
   ----------------------------------
   
   procedure Generate_Xml_Edge_Geometry
     (This : access Cell_Geometry_Record;
      Ob   : Output_Buffer) is
      
      use Point_Lists;
      
   begin
      Put_Line ("Generate_Xml_Edge_Geometry Begin");
      Write_Indent_Str
	(Ob, "<geometry" &
	   " translate-control=""" & 
	   Boolean_To_String (This.Translate_Control_Points) & """" & 
	   " relative=""" & Boolean_To_String (This.Relative) & """");
      
      if This.Rect = No_Rectangle_Record 
	and This.Source_Point = No_Point_Record 
	and This.Target_Point = No_Point_Record 
	and This.Offset = No_Point_Record 
	and This.Points = Point_Lists.Empty_List 
      then
	 Write_Str (Ob, " lx=""True"" />");
	 Write_Eol (Ob);
	 Put_Line ("Generate_Xml_Edge_Geometry End />");
	 return;
      else
	 Write_Str (Ob, " >");
	 Write_Eol (Ob);
      end if;	 
      
      Inc_Indent (Ob);
      
      if This.Rect /= No_Rectangle_Record then
         ----Generate_Xml_Rectangle (This.Rect, Ob);
         null;
      end if;
      
      if This.Source_Point /= No_Point_Record then
	 Write_Indent_Str (Ob, "<source-point ");
	 ----Generate_Xml_Point_Attributs (This.Source_Point, Ob);
	 Write_Str (Ob, " />");
	 Write_Eol (Ob);
      end if;
      
      if This.Target_Point /= No_Point_Record then
	 Write_Indent_Str (Ob, "<target-point ");
	 ----Generate_Xml_Point_Attributs (This.Target_Point, Ob);
	 Write_Str (Ob, " />");
	 Write_Eol (Ob);
      end if;
      
      if This.Offset /= No_Point_Record then
	 Write_Indent_Str (Ob, "<offset-point ");
	 ----Generate_Xml_Point_Attributs (This.Offset, Ob);
	 Write_Str (Ob, " />");
	 Write_Eol (Ob);
      end if;

      if This.Points /= Point_Lists.Empty_List then
	 Put_Line ("has path");
	 ----Generate_Xml_Path (This.Points, Ob);
      end if;
      
      Dec_Indent (Ob);
      
      Write_Indent_Str (Ob, "</geometry>");
      Write_Eol (Ob);
      
      Put_Line ("Generate_Xml_Edge_Geometry End");
   end Generate_Xml_Edge_Geometry;
   
end Artics.Graph.Cells_Geometry;
