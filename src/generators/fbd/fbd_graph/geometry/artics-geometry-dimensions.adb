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

package Body Artics.Geometry.Dimensions is

   -------------------
   -- New_Dimension --
   -------------------
   
   function New_Dimension return Dimension_Record is
   begin
      return Zero_Dimension_Record;
   end New_Dimension;
   
   -------------------
   -- New_Dimension --
   -------------------
   
   function New_Dimension
     (Width  : Coordinate;
      Height : Coordinate) return Dimension_Record  is
   begin
      return Dimension_Record'(Width, Height);
   end New_Dimension;
   
   ---------------
   -- Get_Width --
   ---------------
   
   function Get_Width (D : Dimension_Record) return Coordinate is
   begin
      return D.Width;
   end Get_Width;
   
   ---------------
   -- Set_Width --
   ---------------
   
   procedure Set_Width
     (D : in out Dimension_Record;
      W : Coordinate) is
   begin
      D.Width := W;
   end Set_Width;
   
   ----------------
   -- Get_Height --
   ----------------
   
   function Get_Height (D : Dimension_Record) return Coordinate is
   begin
      return D.Height;
   end Get_Height;
   
   ----------------
   -- Set_Height --
   ----------------
   
   procedure Set_Height
     (D : in out Dimension_Record;
      H : Coordinate) is
   begin
      D.Height := H;
   end Set_Height;
   
   --------------
   -- Set_Size --
   --------------
   
   procedure Set_Size
     (D      : in out Dimension_Record;
      Width  : Coordinate;
      Height : Coordinate) is
   begin
      D.Width  := Width;
      D.Height := Height;
   end Set_Size;
   
   --------------
   -- Set_Size --
   --------------
   
   procedure Set_Size
     (D      : in out Dimension_Record;
      Width  : Integer;
      Height : Integer) is
   begin
      D.Width  := Coordinate (Width);
      D.Height := Coordinate (Height);
   end Set_Size;
   
   ---------------
   -- To_String --
   ---------------
   
   function To_String (D : Dimension_Record) return String is
   begin
      return 
	"Dimension [,w=" & D.Width'Img & ",h=" & D.Height'Img & "]";
   end To_String;

end Artics.Geometry.Dimensions;
