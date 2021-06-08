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

with Artics.Types; use Artics.Types;
with Artics.Geometry; use Artics.Geometry;

-- Contains all global constants.

package Artics.Graph.Constants is
   
   Rad_Per_Deg : constant Coordinate := 0.0174532;
   -- Defines the number of radians per degree.

   Deg_Per_Rad : constant Coordinate := 57.2957795;
   -- Defines the number of degrees per radian.

   Min_Scale_For_Rounded_Lines : constant Coordinate := 0.05;
   -- Defines the minimum scale at which rounded polylines should be painted.
   -- Default is 0.05.
   
   Default_Hotspot : constant Coordinate := 0.3;
   -- Defines the portion of the cell which is to be used as a connectable
   -- region. Default is 0.3.
   
   Min_Hotspot_Size : constant Coordinate := 8.0;
   -- Defines the minimum size in pixels of the portion of the cell which is
   -- to be used as a connectable region. Default is 8.
   
   Max_Hotspot_Size : constant Coordinate := 0.0;
   -- Defines the maximum size in pixels of the portion of the cell which is
   -- to be used as a connectable region. Use 0 for no maximum. Default is 0.
   
   Default_Font_Size : constant Integer := 11;
   -- Defines the default font size. Default is 11.
   
   Default_Start_Size : constant Integer := 40;
   -- Defines the default start size for swimlanes. Default is 40.
   
   Lien_Height : constant Float := 1.2;
   -- Default line height for text output. Default is 1.2. This is ignored for
   -- HTML in the current version of Java. See
   -- * http://docs.oracle.com/javase/6/docs/api/index.html?
   --      javax/swing/text/html/CSS.html
   
   Absolute_Line_Height : Boolean := False;
   -- Specifies if absolute line heights should be used (px) in CSS. Default
   -- is false. Set this to true for backwards compatibility.
   
   Line_Spacing : Integer := 0;
   -- Specifies the line spacing. Default is 0.
   
   Split_Words : Boolean := True;
   -- Whether or not to split whole words when applying word wrapping in
   -- mxUtils.wordWrap.
   
   Label_Inset : Integer := 3;
   -- Defines the inset in absolute pixels between the label bounding box and
   -- the label text. Default is 3.
   
   Label_Scale_Buffer : Coordinate := 0.9;
   -- Multiplier to the width that is passed into the word wrapping calculation
   -- See mxUtils.wordWrap for details
   
   Default_Marker_Size : Integer := 6;
   -- Defines the default marker size. Default is 6.
   
   Default_Image_Size : Integer := 24;
   -- Defines the default image size. Default is 24.
   
   Stencil_Shadow_Opacity : Integer := 1;
   -- Defines the default opacity for stencils shadows. Default is 1.
   
   Shadow_Offset_X : Integer := 2;
   -- Defines the x-offset to be used for shadows. Default is 2.
   
   Shadow_Offset_Y : Integer := 3;
   -- Defines the y-offset to be used for shadows. Default is 3.
   
   type Dash_Patern_Type is array (1..2) of Float;
   Default_Dash_Pattern : aliased Dash_Patern_Type := (3.0, 3.0);
   -- Specifies the default dash pattern, 3 pixels solid, 3 pixels clear.
   
   Default_Label_Buffer : Coordinate := 12.0;
   -- Specifies the default distance at 1.0 scale that the label curve is 
   -- created from its base curve
   
   Handle_Size : Integer := 7;
   -- Defines the handle size. Default is 7.
   
   Label_Handle_Size : Integer := 4;
   -- Defines the label handle size. Default is 4.
   
   Connect_Handle_Enabled : Boolean := False;
   -- Defines the default value for the connect handle. Default is false.

   Connect_Handle_Size : Integer := 8;
   -- Defines the connect handle size. Default is 8.

   Entity_Segment : Integer := 30;
   -- Defines the length of the horizontal segment of an Entity Relation.
   -- This can be overridden using mxConstants.STYLE_SEGMENT style.
   -- Default is 30.

   Rectangle_Rounding_Factor : Coordinate := 0.05;
   -- Defines the rounding factor for rounded rectangles in percent between
   -- 0 and 1. Values should be smaller than 0.5. Default is 0.15.
   
   Line_Arcsize : Coordinate := 10.0;
   -- Defines the size of the arcs for rounded edges. Default is 10.

   Arrow_Spacing : Integer := 10;
   -- Defines the spacing between the arrow shape and its terminals. Default
   -- is 10.
   
   Arrow_Width : Integer := 30;
   -- Defines the width of the arrow shape. Default is 30.

   Arrow_Size : Integer := 30;
   -- Defines the size of the arrowhead in the arrow shape. Default is 30.
   
   Font_Bold             : Integer := 2**0;
   Font_Italic           : Integer := 2**1;
   Font_Oblique          : Integer := 2**2;
   Font_Simple_Underline : Integer := 2**3;
   Font_Double_Underline : Integer := 2**4;
   Font_Strikethrough    : Integer := 2**5;
   
   
   -- DIRECTION MASK used in edges styles
   
   DIRECTION_MASK_NONE  : Integer := 0;
   DIRECTION_MASK_WEST  : Integer := 2**0;
   DIRECTION_MASK_NORTH : Integer := 2**1;
   DIRECTION_MASK_SOUTH : Integer := 2**2;
   DIRECTION_MASK_EAST  : Integer := 2**3;
   DIRECTION_MASK_ALL   : Integer := 2**4;

end Artics.Graph.Constants;
