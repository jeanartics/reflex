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
with Artics.Namet; use Artics.Namet;
with Artics.Utils; use Artics.Utils;
with Artics.Array_Types; use Artics.Array_Types;

with Artics.Geometry; use Artics.Geometry;
with Artics.Geometry.Points; use Artics.Geometry.Points;
with Artics.Geometry.Rectangles; use Artics.Geometry.Rectangles;

with Artics.Graph.Cells; use Artics.Graph.Cells;
with Artics.Graph.Cells_States; use Artics.Graph.Cells_States;

with Dummy; use Dummy;

package Artics.Graph.Utils is
   
   function Intersection
     (X0 : Coordinate;
      Y0 : Coordinate;
      X1 : Coordinate;
      Y1 : Coordinate;
      X2 : Coordinate;
      Y2 : Coordinate;
      X3 : Coordinate;
      Y3 : Coordinate) return Point_Record;
   -- Returns the intersection of two lines as a Point.
   --    x0 : X-coordinate of the first line's startpoint.
   --    y0 : Y-coordinate of the first line's startpoint.
   --    x1 : X-coordinate of the first line's endpoint.
   --    y1 : Y-coordinate of the first line's endpoint.
   --    x2 : X-coordinate of the second line's startpoint.
   --    y2 : Y-coordinate of the second line's startpoint.
   --    x3 : X-coordinate of the second line's endpoint.
   --    y3 : Y-coordinate of the second line's endpoint.
   
   function Lines_Intersection
     (P0 : Point_Record;
      P1 : Point_Record;
      P2 : Point_Record;
      P3 : Point_Record) return Point_Record;
   -- Returns the intersection of two lines as a Point.
   --    P0 : is the start point of the first line 
   --    P1 : is the end point of the first line 
   --    P2 : is the start point of the second line 
   --    P3 : is the end point of the second line 
   
   

   --public static boolean IS_MAC = System.getProperty("os.name").toLowerCase   
   -- True if the machine is a Mac.

   --public static boolean IS_LINUX = System.getProperty("os.name")
   --		.toLowerCase().indexOf("linux") >= 0;
   -- True if the machine is running a linux kernel.

   -- Static Graphics used for Font Metrics.
   -- protected static transient Graphics fontGraphics;

   -- Creates a renderer for HTML markup (only possible in non-headless 
   -- environment)
   --  static
   --  {
   --  	try
   --  	{
   --  		fontGraphics = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB)
   --  				.getGraphics();
   --  	}
   --  	catch (Exception e)
   --  	{
   --  		// ignore
   --  	}
   --  }

   function Get_Label_Size
     (Label   : String;
      Style   : Strings_Maps.Map;
      Is_Html : Boolean;
      Scale   : Coordinate) return Rectangle_Record;
   function Get_Label_Size
     (Label   : Name_Id;
      Style   : Strings_Maps.Map;
      Is_Html : Boolean;
      Scale   : Coordinate) return Rectangle_Record;
   -- Returns the size for the given label. If isHtml is true then any HTML
   -- markup in the label is computed as HTML and all newlines inside the HTML
   -- body are converted into linebreaks.

   
   function Get_Label_Size
     (Label           : String;
      Style           : Strings_Maps.Map;
      Is_Html         : Boolean;
      Scale           : Coordinate;
      Html_Wrap_Width : Coordinate) return Rectangle_Record;
   function Get_Label_Size
     (Label           : Name_Id;
      Style           : Strings_Maps.Map;
      Is_Html         : Boolean;
      Scale           : Coordinate;
      Html_Wrap_Width : Coordinate) return Rectangle_Record;
   -- Returns the size for the given label. If isHtml is true then any HTML
   -- markup in the label is computed as HTML and all newlines inside the HTML
   -- body are converted into linebreaks.

   function Get_Body_Markup
     (Markup             : String;
      Replace_Line_Feeds : Boolean) return String;
   -- Returns the body part of the given HTML markup.

   function Get_Label_Paint_Bounds
     (Label         : String;
      Style         : Strings_Maps.Map;
      Is_Html       : Boolean;
      Offset        : Point_Record;
      Vertex_Bounds : Rectangle_Record;
      Scale         : Coordinate) return Rectangle_Record;
   -- Returns the paint bounds for the given label.

   function Get_Label_Paint_Bounds
     (Label         : String;
      Style         : Strings_Maps.Map;
      Is_Html       : Boolean;
      Offset        : Point_Record;
      Vertex_Bounds : Rectangle_Record;
      Scale         : Coordinate;
      Is_Edge       : Boolean) return Rectangle_Record;
   -- Returns the paint bounds for the given label.

   function Get_Scaled_Label_Bounds
     (X            : Coordinate;
      Y            : Coordinate;
      Size         : Rectangle_Record;
      Outer_Width  : Coordinate;
      Outer_Height : Coordinate;
      Style        : Strings_Maps.Map;
      Scale        : Coordinate) return Rectangle_Record;
   -- Returns the bounds for a label for the given location and size, taking
   -- into account the alignment and spacing in the specified style, as well as
   -- the width and height of the rectangle that contains the label. (For edge
   -- labels this width and height is 0.) The scale is used to scale the given
   -- size and the spacings in the specified style.

   --     function Get_Font_Metrics 
   --       (Font : access Font_Record) return access Font_Metrics_Record;
   -- Returns the font metrics of the static font graphics instance 
   -- param font The font whose metrics are to be returned
   -- @return the font metrics of the specified font

   --     function Get_Width_For_String
   --       (Text : String;
   --        Font : access Font_Record) return Float;
   --     function Get_Height_For_String 
   --       (Text : String;
   --        Font : access Font_Record) return Float;
   --     procedure Get_Bounds_For_String
   --       (Text   : String;
   --        Font   : access Font_Record;
   --        Width  : out Float;
   --        Height : out Float);
   --     
   --     function Get_Size_For_String
   --       (Text  : String;
   --        Font  : access Font_Record;
   --        Scale : Coordinate) return Rectangle_Record;
   -- Returns an <mxRectangle> with the size (width and height in pixels) of
   -- the given string.
   -- @param text
   --            String whose size should be returned.
   -- @param font
   --            Font to be used for the computation.

   --     function Word_Wrap
   --       (Text    : String;
   --        Metrics : access Font_Metrics_Record;
   --        Width   : Coordinate) return Strings_Lists.List;
   -- Returns the specified text in lines that fit within the specified width
   -- when the specified font metrics are applied to the text @param text the
   -- text to wrap
   -- @param metrics the font metrics to calculate the text size for
   -- @param width the width that the text must fit within
   -- @return the input text split in lines that fit the specified width

   function Get_Size_For_Html
     (Markup     : String;
      Style      : Strings_Maps.Map;
      Scale      : Coordinate;
      Wrap_Width : Coordinate) return Rectangle_Record;
   -- Returns an mxRectangle with the size (width and height in pixels) of the
   -- given HTML markup.
   -- @param markup
   --            HTML markup whose size should be returned.
   
   function Arc_To_Curves
     (X0             : Coordinate;
      Y0             : Coordinate;
      R1a            : Coordinate;
      R2a            : Coordinate;
      Angle          : Coordinate;
      Large_Arc_Flag : Coordinate;      
      Sweep_Flag     : Coordinate;
      Xa             : Coordinate;
      Ya             : Coordinate) return Coordinates_Lists.List;
   -- Function: arcToCurves
   -- Converts the given arc to a series of curves.

   function Get_Bounding_Box
     (Rect     : Rectangle_Record;
      Rotation : Coordinate) return Rectangle_Record;
   -- Returns the bounding box for the rotated rectangle.

   function First_Char_At
     (Text       : String;
      Input_Char : Integer;
      From_Index : Integer) return Integer;
   -- Find the first character matching the input character in the given string
   -- where the character has no letter preceding it.
   -- @param text the string to test for the presence of the input character
   -- @param inputChar the test character
   -- @param fromIndex the index position of the string to start from
   -- @return the position of the first character matching the input character
   --      in the given string where the character has no letter preceding it.

   function Get_Rotated_Point
     (Pt  : Point_Record;
      Cos : Coordinate;
      Sin : Coordinate) return Point_Record;
   -- Rotates the given point by the given cos and sin.

   function Find_Nearest_Segment
     (State : access Cell_State_Record'Class;
      X     : Coordinate;
      Y     : Coordinate) return Integer;
   -- Finds the index of the nearest segment on the given cell state for the
   -- specified coordinate pair.
   
   function Get_Rotated_Point
     (Pt  : Point_Record;
      Cos : Coordinate;
      Sin : Coordinate;
      C   : Point_Record) return Point_Record;
   -- Rotates the given point by the given cos and sin.

   function Get_Port_Constraints
     (Terminal : access Cell_State_Record'Class;
      Edge     : access Cell_State_Record'Class;
      Source   : Boolean) return Integer;
   -- Returns an integer mask of the port constraints of the given map 
   -- @param terminal the cached cell state of the cell to determine the
   -- 			port constraints for
   -- @param edge the edge connected to the constrained terminal
   -- @param source whether or not the edge specified is connected to the
   -- 			terminal specified at its source end
   -- @return the mask of port constraint directions
   
   function Get_Port_Constraints
     (Terminal      : access Cell_State_Record'Class;
      Edge          : access Cell_State_Record'Class;
      Source        : Boolean;
      Default_Value : Integer) return Integer;
   -- Returns an integer mask of the port constraints of the given map
   -- @param terminal the cached cell state of the cell to determine the
   -- 			port constraints for
   -- @param edge the edge connected to the constrained terminal
   -- @param source whether or not the edge specified is connected to the
   -- 			terminal specified at its source end
   -- @param defaultValue Default value to return if the key is undefined.
   -- @return the mask of port constraint directions
   
   function Reverse_Port_Constraints (Constraint : Integer) return Integer;
   
   --     procedure Draw_Image_Clip
   --       (G        : access Graphics;
   --        Image    : access Buffered_Image;
   --        Observer : access Image_Observer);
   --     -- Draws the image inside the clip bounds to the given graphics object.
   --  
   --     procedure Fill_Clipped_Rect
   --       (G      : access Graphics;
   --        X      : Integer;
   --        Y      : Integer;
   --        Width  : Integer;
   --        Height : Integer);
   
   function Translate_Points
     (Pts : Point_Lists.List;
      Dx  : Coordinate;
      Dy  : Coordinate) return Point_Lists.List;
   -- Creates a new list of new points obtained by translating the points in 
   -- the given list by the given vector. Elements that are not mxPoints are
   -- added to the result as-is.

   function Sort_Cells
     (Cells     : Cells_Lists.List;
      Ascending : Boolean := True) return Cells_Lists.List;
   -- Sorts the given cells according to the order in the cell hierarchy.

   function Contains 
     (Cells : Cells_Lists.List;
      Cell  : access Cell_Record'Class) return Boolean;
   -- Returns true if the given array contains the given object.

   function Index_Of
     (Cells : Cells_Lists.List;
      Cell  : access Cell_Record'Class) return Integer;
   -- Returns the index of the given object in the given array of -1 if the
   -- object is not contained in the array.
   
   
   function Intersects_Hotspot
     (State   : access Cell_State_Record'Class;
      X       : Coordinate;
      Y       : Coordinate;
      Hotspot : Coordinate) return Boolean;

   function Intersects_Hotspot
     (State   : access Cell_State_Record'Class;
      X       : Coordinate;
      Y       : Coordinate;
      Hotspot : Coordinate;
      Min     : Coordinate;
      Max     : Coordinate) return Boolean;
   -- Returns true if the given coordinate pair intersects the hotspot of the
   -- given state.

   function Is_True
     (Dict : Strings_Maps.Map;
      Key  : String) return Boolean;
   function Is_True
     (Dict : Strings_Maps.Map;
      Key  : Name_Id) return Boolean;
   -- Returns true if the dictionary contains true for the given key or false
   -- if no value is defined for the key.
   -- @param dict
   --            Dictionary that contains the key, value pairs.
   -- @param key
   --            Key whose value should be returned.
   -- @return Returns the boolean value for key in dict.

   function Is_True
     (Dict          : Strings_Maps.Map;
      Key           : String;
      Default_Value : Boolean) return Boolean;
   function Is_True
     (Dict          : Strings_Maps.Map;
      Key           : Name_Id;
      Default_Value : Boolean) return Boolean;
   -- Returns true if the dictionary contains true for the given key or the
   -- given default value if no value is defined for the key.
   -- @param dict
   --           Dictionary that contains the key, value pairs.
   -- @param key
   --            Key whose value should be returned.
   -- @param defaultValue
   --            Default value to return if the key is undefined.
   -- @return Returns the boolean value for key in dict.

   function Get_Int
     (Dict : Strings_Maps.Map;
      Key  : String) return Integer;
   function Get_Int
     (Dict : Strings_Maps.Map;
      Key  : Name_Id) return Integer;
   -- Returns the value for key in dictionary as an int or 0 if no value is
   -- defined for the key.
   -- @param dict
   --            Dictionary that contains the key, value pairs.
   -- @param key
   --            Key whose value should be returned.
   -- @return Returns the integer value for key in dict.

   function Get_Int
     (Dict          : Strings_Maps.Map;
      Key           : String;
      Default_Value : Integer) return Integer;
   function Get_Int
     (Dict          : Strings_Maps.Map;
      Key           : Name_Id;
      Default_Value : Integer) return Integer;
   -- Returns the value for key in dictionary as an int or the given default
   -- value if no value is defined for the key.
   -- @param dict
   --            Dictionary that contains the key, value pairs.
   -- @param key
   --            Key whose value should be returned.
   -- @param defaultValue
   --            Default value to return if the key is undefined.
   -- @return Returns the integer value for key in dict.

   function Get_Float
     (Dict : Strings_Maps.Map;
      Key  : String) return Coordinate;
   function Get_Float
     (Dict : Strings_Maps.Map;
      Key  : Name_Id) return Coordinate;
   -- Returns the value for key in dictionary as a float or 0 if no value is
   -- defined for the key.
   -- @param dict
   --            Dictionary that contains the key, value pairs.
   -- @param key
   --            Key whose value should be returned.
   -- @return Returns the float value for key in dict.

   function Get_Float
     (Dict          : Strings_Maps.Map;
      Key           : String;
      Default_Value : Coordinate) return Float;
   function Get_Float
     (Dict          : Strings_Maps.Map;
      Key           : Name_Id;
      Default_Value : Coordinate) return Float;
   
   function Get_Float
     (Dict          : Strings_Maps.Map;
      Key           : String;
      Default_Value : Integer) return Float;
   function Get_Float
     (Dict          : Strings_Maps.Map;
      Key           : Name_Id;
      Default_Value : Integer) return Float;
   -- Returns the value for key in dictionary as a float or the given default
   -- value if no value is defined for the key.
   -- @param dict
   --            Dictionary that contains the key, value pairs.
   -- @param key
   --            Key whose value should be returned.
   -- @param defaultValue
   --            Default value to return if the key is undefined.
   -- @return Returns the float value for key in dict.

   function Get_Float_Array_Ptr
     (Dict          : Strings_Maps.Map;
      Key           : String;
      Default_Value : Float_Array_Ptr;
      Separator     : String := ",") return Float_Array_Ptr;
   function Get_Float_Array_Ptr
     (Dict          : Strings_Maps.Map;
      Key           : Name_Id;
      Default_Value : Float_Array_Ptr;
      Separator     : String := ",") return Float_Array_Ptr;
   
   function Get_Float_Array
     (Dict          : Strings_Maps.Map;
      Key           : String;
      Default_Value : Coordinates_Lists.List;
      Separator     : String := ",") return Coordinates_Lists.List;
   function Get_Float_Array
     (Dict          : Strings_Maps.Map;
      Key           : Name_Id;
      Default_Value : Coordinates_Lists.List;
      Separator     : String := ",") return Coordinates_Lists.List;
   -- Returns the value for key in dictionary as a float array or the given 
   -- default value if no value is defined for the key.
   -- @param dict
   --            Dictionary that contains the key, value pairs.
   -- @param key
   --            Key whose value should be returned.
   -- @param defaultValue
   --            Default value to return if the key is undefined.
   -- @return Returns the float array value for key in dict.

   function Get_Double
     (Dict : Strings_Maps.Map;
      Key  : String) return Coordinate;
   function Get_Double
     (Dict : Strings_Maps.Map;
      Key  : Name_Id) return Coordinate;
   -- Returns the value for key in dictionary as a double or 0 if no value is
   -- defined for the key.
   -- @param dict
   --            Dictionary that contains the key, value pairs.
   -- @param key
   --            Key whose value should be returned.
   -- @return Returns the double value for key in dict.

   function Get_Double
     (Dict          : Strings_Maps.Map;
      Key           : String;
      Default_Value : Coordinate) return Coordinate;
   function Get_Double
     (Dict          : Strings_Maps.Map;
      Key           : Name_Id;
      Default_Value : Coordinate) return Coordinate;
   -- Returns the value for key in dictionary as a double or the given default
   -- value if no value is defined for the key.
   -- @param dict
   --            Dictionary that contains the key, value pairs.
   -- @param key
   --            Key whose value should be returned.
   -- @param defaultValue
   --            Default value to return if the key is undefined.
   -- @return Returns the double value for key in dict.
   
   function Get_String
     (Dict : Strings_Maps.Map;
      Key  : String) return String;
   function Get_String
     (Dict : Strings_Maps.Map;
      Key  : Name_Id) return Name_Id;
   -- Returns the value for key in dictionary as a string or null if no value
   -- is defined for the key.
   -- @param dict
   --            Dictionary that contains the key, value pairs.
   -- @param key
   --            Key whose value should be returned.
   -- @return Returns the string value for key in dict.
	  
   function Get_String
     (Dict          : Strings_Maps.Map;
      Key           : String;
      Default_Value : String) return String;
   function Get_String
     (Dict          : Strings_Maps.Map;
      Key           : Name_Id;
      Default_Value : Name_Id) return Name_Id;
   -- Returns the value for key in dictionary as a string or the given default
   -- value if no value is defined for the key.
   -- @param dict
   --            Dictionary that contains the key, value pairs.
   -- @param key
   --            Key whose value should be returned.
   -- @param defaultValue
   --            Default value to return if the key is undefined.
   -- @return Returns the string value for key in dict.
   
   --     function Get_Color
   --       (Style         : Strings_Maps.Map;
   --        Style_Name    : String;
   --        Default_Value : Name_Id := No_Name) return access Color_Record;
   --     function Get_Color
   --       (Style         : Strings_Maps.Map;
   --        Style_Name    : Name_Id;
   --        Default_Value : Name_Id := No_Name) return access Color_Record;
   --     
   --     function Get_Color_Name
   --       (Style         : Strings_Maps.Map;
   --        Style_Name    : String;
   --        Default_Value : Name_Id := No_Name) return Name_Id;
   --     function Get_Color_Name
   --       (Style         : Strings_Maps.Map;
   --        Style_Name    : Name_Id;
   --        Default_Value : Name_Id := No_Name) return Name_Id;
   -- Returns the value for key in dictionary as a color or null if no value is
   -- defined for the key.
   -- @param dict
   --            Dictionary that contains the key, value pairs.
   -- @param key
   --            Key whose value should be returned.
   -- @return Returns the color value for key in dict.
	  

   --     function Get_Font (Style : Strings_Maps.Map) return access Font_Record;
   --     
   --     function Get_Font
   --       (Style : Strings_Maps.Map;
   --        Scale : Coordinate) return access Font_Record;

   --     function Hex_String (Color : access Color_Record'Class) return String;

   --  --     function Parse_Color
   --  --       (Color_String : String) return access Color_Record'Class;
   -- Convert a string representing a 24/32bit hex color value into a Color
   -- object. The following color names are also supported: white, black, red,
   -- green, blue, orange, yellow, pink, turquoise, gray and none (null).
   -- Examples of possible hex color values are: #C3D9FF, #6482B9 and #774400,
   -- but note that you do not include the "#" in the string passed in
   -- @param colorString
   --            the 24/32bit hex string value (ARGB)
   -- @return java.awt.Color (24bit RGB on JDK 1.1, 24/32bit ARGB on JDK1.2)
   -- @exception NumberFormatException
   --               if the specified string cannot be interpreted as a
   --                hexidecimal integer

   --     function Get_Hex_Color_String
   --       (Color : access Color_Record'Class) return String;
   -- Returns a hex representation for the given color.
   -- @param color
   --            Color to return the hex string for.
   -- @return Returns a hex string for the given color.

   function Parse_Dash_Pattern
     (Dash_Pattern_String : String) return Coordinates_Lists.List;
   -- Convert a string representing a dash pattern into a float array. A valid
   -- dash pattern is a string of dash widths (floating point values) separated
   -- by space characters.
   -- @param dashPatternString
   --            the string representing the dash pattern
   -- @return float[]
   -- @exception NumberFormatException
   --                if any of the dash widths cannot be interpreted as a
   --                floating point number

   function Read_File (File_Name : String) return String;
   -- Reads the given filename into a string.
   -- @param filename
   --            Name of the file to be read.
   -- @return Returns a string representing the file contents.
   -- @throws IOException

   --  function Read_Input_Stream 
   --    (InputStream stream) throws IOException
   -- Reads the given filename into a string.
   -- @param filename
   --            Name of the file to be read.
   -- @return Returns a string representing the file contents.

   procedure Write_File
     (Contents  : String;
      File_Name : String);
   -- Writes the given string into the given file.
   -- @param contents
   --            String representing the file contents.
   -- @param filename
   --            Name of the file to be written.
   -- @throws IOException
   
   
end Artics.Graph.Utils;
