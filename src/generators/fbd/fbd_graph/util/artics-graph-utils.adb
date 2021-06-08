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

with Artics.Maths; use Artics.Maths;

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.String_Split; use GNAT.String_Split;
with GNAT.Case_Util; use GNAT.Case_Util;

with Artics.Graph.Names; use Artics.Graph.Names;
with Artics.Graph.Constants; use Artics.Graph.Constants;
with Artics.Supports; use Artics.Supports;
with Artics.Graph.Cells_Paths; use Artics.Graph.Cells_Paths;

package body Artics.Graph.Utils is
   
   function Pt_Seg_Dist_Sq
     (X1   : Float;
      Y1   : Float;
      X2   : Float;
      Y2   : Float;
      Px   : Float;
      Py   : Float) return Float;
   
   ---------------
   -- Float_Str --
   ---------------
   
   function Float_Str (Value : String) return Coordinate is
   begin
      return 0.0;
   end Float_Str;
   
   ---------------
   -- Float_Str --
   ---------------
   
   function Float_Str (Value : Name_Id) return Coordinate is
      S : String := Get_String (Value);
   begin
      return 0.0;
   end Float_Str;
   
   ------------------
   -- Intersection --
   ------------------
   
   function Intersection
     (X0 : Coordinate;
      Y0 : Coordinate;
      X1 : Coordinate;
      Y1 : Coordinate;
      X2 : Coordinate;
      Y2 : Coordinate;
      X3 : Coordinate;
      Y3 : Coordinate) return Point_Record is
      
      Denom  : Coordinate;
      Nume_A : Coordinate;
      Nume_B : Coordinate;
      Ua     : Coordinate;
      Ub     : Coordinate;
      Intersection_X : Coordinate;
      Intersection_Y : Coordinate;
      
   begin
      Denom  := ((Y3 - Y2) * (X1 - X0)) - ((X3 - X2) * (Y1 - Y0));
      Nume_A := ((X3 - X2) * (Y0 - Y2)) - ((Y3 - Y2) * (X0 - X2));
      Nume_B := ((X1 - X0) * (Y0 - Y2)) - ((Y1 - Y0) * (X0 - X2));

      Ua := Nume_A / Denom;
      Ub := Nume_B / Denom;

      if (Ua >= 0.0 and Ua <= 1.0) and (Ub >= 0.0 and Ub <= 1.0) then
	 
         -- Get the intersection point
	 
         Intersection_X := X0 + Ua * (X1 - X0);
         Intersection_Y := Y0 + Ua * (Y1 - Y0);

         return Point_Record'(X => Intersection_X, Y => Intersection_Y);
      end if;

      return No_Point_Record;
   end Intersection;
   
   ------------------------
   -- Lines_Intersection --
   ------------------------
   
   function Lines_Intersection
     (P0 : Point_Record;
      P1 : Point_Record;
      P2 : Point_Record;
      P3 : Point_Record) return Point_Record is
   begin
      return Intersection
        (P0.X, P0.Y,
         P1.X, P1.Y,
         P2.X, P2.Y,
         P3.X, P3.Y);
   end Lines_Intersection;
   
   --------------------
   -- Get_Label_Size --
   --------------------
   
   function Get_Label_Size
     (Label   : String;
      Style   : Strings_Maps.Map;
      Is_Html : Boolean;
      Scale   : Coordinate) return Rectangle_Record is
   begin
      return Get_Label_Size(Label, Style, Is_Html, Scale, 0.0);
   end Get_Label_Size;
   
   --------------------
   -- Get_Label_Size --
   --------------------
   
   function Get_Label_Size
     (Label   : Name_Id;
      Style   : Strings_Maps.Map;
      Is_Html : Boolean;
      Scale   : Coordinate) return Rectangle_Record is
   begin
      return Get_Label_Size (Get_String (Label), Style, Is_Html, Scale, 0.0);
   end Get_Label_Size;
   
   --------------------
   -- Get_Label_Size --
   --------------------
   
   function Get_Label_Size
     (Label           : String;
      Style           : Strings_Maps.Map;
      Is_Html         : Boolean;
      Scale           : Coordinate;
      Html_Wrap_Width : Coordinate) return Rectangle_Record 
   is
      Size : Rectangle_Record;   
   begin      
      if Is_Html then
         Size := Get_Size_For_Html
           (Get_Body_Markup (Label, True), Style, Scale, Html_Wrap_Width);
		
      else
         null; -- Size := Get_Size_For_String (Label, Get_Font (Style), Scale);
      end if;
      return Size;
   end Get_Label_Size;
   
   --------------------
   -- Get_Label_Size --
   --------------------
   
   function Get_Label_Size
     (Label           : Name_Id;
      Style           : Strings_Maps.Map;
      Is_Html         : Boolean;
      Scale           : Coordinate;
      Html_Wrap_Width : Coordinate) return Rectangle_Record is
   begin
      return Get_Label_Size (Get_String (Label), Style, Is_Html, Scale,
                             Html_Wrap_Width);
   end Get_Label_Size;
   
   ---------------------
   -- Get_Body_Markup --
   ---------------------
   
   function Get_Body_Markup
     (Markup             : String;
      Replace_Line_Feeds : Boolean) return String 
   is
      Lower_Case : String := Markup;
      Body_Start : Integer;
      Body_End   : Integer;
      Line_Feed : constant String := "" & ASCII.LF;
   begin
      To_Lower (Lower_Case);
      Body_Start := Artics.Utils.Index_Of (Lower_Case, "<body>");
      
      if Body_Start >= 1 then
         
         Body_Start := Body_Start + 6;
         Body_End := Artics.Utils.Index_Of (Lower_Case, "</body>") - 1;
         
         if Body_End > Body_Start then
            
            if Replace_Line_Feeds then
               
               return ""; --Artics.Utils.Replace_All 
               -- (Markup (Body_Start .. Body_End),Line_Feed, "<br>");
            else 
               return Markup (Body_Start .. Body_End);
            end if;
         end if;
      end if;
      
      if Replace_Line_Feeds then               
         return ""; --Artics.Utils.Replace_All (Markup, Line_Feed, "<br>");
      end if; 
      
      return Markup;
   end Get_Body_Markup;
   
   ----------------------------
   -- Get_Label_Paint_Bounds --
   ----------------------------
   
   function Get_Label_Paint_Bounds
     (Label         : String;
      Style         : Strings_Maps.Map;
      Is_Html       : Boolean;
      Offset        : Point_Record;
      Vertex_Bounds : Rectangle_Record;
      Scale         : Coordinate) return Rectangle_Record is
   begin
      return Get_Label_Paint_Bounds
        (Label, Style, Is_Html, Offset, Vertex_Bounds, Scale, False);
   end Get_Label_Paint_Bounds;
   
   ----------------------------
   -- Get_Label_Paint_Bounds --
   ----------------------------
   
   function Get_Label_Paint_Bounds
     (Label         : String;
      Style         : Strings_Maps.Map;
      Is_Html       : Boolean;
      Offset        : Point_Record;
      Vertex_Bounds : Rectangle_Record;
      Scale         : Coordinate;
      Is_Edge       : Boolean) return Rectangle_Record is
      
      Wrap_Width : Coordinate := 0.0;
      Size       : Rectangle_Record;
      X          : Coordinate;
      Y          : Coordinate;
      Width      : Coordinate;
      Height     : Coordinate;
      Horizontal : Boolean;
      Start      : Coordinate;
   begin
      if Is_Html and Vertex_Bounds /= No_Rectangle_Record 
        and (Get_String (Style, STYLE_WHITE_SPACE, String_Find ("nowrap"))
             = String_Find ("wrap"))
      then
         Wrap_Width := Get_Width (Vertex_Bounds);
      end if;

      Size := Get_Label_Size (Label, Style, Is_Html, Scale, Wrap_Width);

      -- Measures font with full scale and scales back
      Set_Width (Size, Get_Width (Size) / Scale);
      Set_Height(Size, Get_Height (Size) / Scale);

      X      := Get_X (Offset);
      Y      := Get_Y (Offset);
      Width  := 0.0;
      Height := 0.0;

      if Vertex_Bounds /= No_Rectangle_Record then
         X := X + Get_X (Vertex_Bounds);
         Y := Y + Get_Y (Vertex_Bounds);

         if Get_String (Style, STYLE_SHAPE, No_Name) = SHAPE_SWIMLANE then
	    
            -- Limits the label to the swimlane title
	    
            Horizontal := Is_True (Style, STYLE_HORIZONTAL, True);
            Start := Get_Float
              (Style, STYLE_STARTSIZE, Default_Start_Size) * Scale;

            if Horizontal then
               Width := Width + Get_Width (Vertex_Bounds);
               Height := Height + Start;
            else
               Width := Width + Start;
               Height := Height + Get_Height (Vertex_Bounds);
            end if;
         else
            if not Is_Edge then
               Width := Get_Width (Vertex_Bounds);
            end if;
            Height := Height + Get_Height (Vertex_Bounds);
         end if;
      end if;

      return Get_Scaled_Label_Bounds (X, Y, Size, Width, Height, Style, Scale);
   end Get_Label_Paint_Bounds;
   
   -----------------------------
   -- Get_Scaled_Label_Bounds --
   -----------------------------
   
   function Get_Scaled_Label_Bounds
     (X            : Coordinate;
      Y            : Coordinate;
      Size         : Rectangle_Record;
      Outer_Width  : Coordinate;
      Outer_Height : Coordinate;
      Style        : Strings_Maps.Map;
      Scale        : Coordinate) return Rectangle_Record is
      
      Xl         : Coordinate;
      Yl         : Coordinate;
      Inset      : Float; 
      Width      : Coordinate;
      Height     : Coordinate;
      Horizontal : Boolean;
      Spacing    : Float;
      Align      : Name_Id;
      Valign     : Name_Id;
      Top        : Float;
      Bottom     : Float;
      Left       : Float;
      Right      : Float;
      Tmp        : Float;
      Tmp2       : Float;
   begin
      Xl := X;
      Yl := Y;
      
      Inset := Float (Label_Inset) * Scale;

      -- Scales the size of the label
      -- FIXME: Correct rounded font size and not-rounded scale
      
      Width  := Get_Width (Size) * Scale + 2.0 * Inset;
      Height := Get_Height (Size) * Scale + 2.0 * Inset;

      -- Gets the global spacing and orientation
      
      Horizontal := Is_True (Style, STYLE_HORIZONTAL, True);
      Spacing    := Float (Get_Int (Style, STYLE_SPACING)) * Scale;

      -- Gets the alignment settings
      
      Align  := Get_String (Style, STYLE_ALIGN, ALIGN_CENTER);
      Valign := Get_String (Style, STYLE_VERTICAL_ALIGN, ALIGN_MIDDLE);

      -- Gets the vertical spacing
      Top    := Float (Get_Int (Style, STYLE_SPACING_TOP)) * Scale;
      Bottom := Float (Get_Int (Style, STYLE_SPACING_BOTTOM)) * Scale;

      -- Gets the horizontal spacing
      
      Left  := Float (Get_Int (Style, STYLE_SPACING_LEFT))  * Scale;
      Right := Float (Get_Int (Style, Style_Spacing_Right)) * Scale;

      -- Applies the orientation to the spacings and dimension
      
      if not Horizontal then
         Tmp    := Top;
         Top    := Right;
         Right  := Bottom;
         Bottom := Left;
         Left   := Tmp;

         Tmp2   := Width;
         Width  := Height;
         Height := Tmp2;
      end if;

      -- Computes the position of the label for the horizontal alignment
      
      if (Horizontal and Align = ALIGN_CENTER)
        or (not Horizontal and Valign = ALIGN_MIDDLE)
      then
         Xl := Xl + (Outer_Width - Width) / 2.0 + Left - Right;
	 
      elsif (Horizontal and Align = ALIGN_RIGHT)
        or (not Horizontal and Valign = ALIGN_BOTTOM)
      then
         Xl := Xl + Outer_Width - Width - Spacing - Right;
	 
      else
         Xl := Xl + Spacing + Left;
      end if;

      -- Computes the position of the label for the vertical alignment
      
      if (not Horizontal and Align = ALIGN_CENTER)
        or (Horizontal and Valign = ALIGN_MIDDLE)
      then
         Yl := Yl + (Outer_Height - Height) / 2.0 + Top - Bottom;
	 
      elsif (not Horizontal and Align = ALIGN_LEFT)
        or (Horizontal and Valign = ALIGN_BOTTOM)
      then
         Yl := Yl + Outer_Height - Height - Spacing - Bottom;
	 
      else
         Yl := Yl + Spacing + Top;
      end if;

      return New_Rectangle (Xl, Yl, Width, Height);
   end Get_Scaled_Label_Bounds;
   
   ----------------------
   -- Get_Font_Metrics --
   ----------------------
   
   --     function Get_Font_Metrics
   --       (Font : access Font_Record) return access Font_Metrics_Record is
   --     begin
   --        return Get_Metrics (Font);
   --     end Get_Font_Metrics;
   
   --------------------------
   -- Get_Width_For_String --
   --------------------------
   
   --     function Get_Width_For_String
   --       (Text : String;
   --        Font : access Font_Record) return Float is
   --        
   --        W : Float;
   --        H : Float;
   --     begin
   --        Get_Bounds_For_String (Text, Font, W, H);
   --        return W;
   --     end Get_Width_For_String;
   
   ---------------------------
   -- Get_Height_For_String --
   ---------------------------
   
   --     function Get_Height_For_String 
   --       (Text : String;
   --        Font : access Font_Record) return Float is
   --        
   --        W : Float;
   --        H : Float;
   --     begin
   --        Get_Bounds_For_String (Text, Font, W, H);
   --        return H;
   --     end Get_Height_For_String;
   
   ---------------------------
   -- Get_Bounds_For_String --
   ---------------------------
   
   --     procedure Get_Bounds_For_String
   --       (Text   : String;
   --        Font   : access Font_Record;
   --        Width  : out Float;
   --        Height : out Float) is
   --        
   --        Layout : access Text_Layout_Record;
   --        Ink_Rect, Logical_Rect : Rectangle_Record;
   --     begin
   --        Layout := Get_Transient_Text_Layout;
   --        
   --        Layout.Set_Font_Layout (Font);
   --        Layout.Set_Text (Text);
   --        Layout.Set_Spacing (Float (artics.Graph.Constants.Line_Spacing));
   --        Layout.Apply_Layout_Style;
   --        
   --        Layout.Get_Extents (Ink_Rect, Logical_Rect);
   --        
   --        Width  := Get_Width (Logical_Rect) / Float (Font_Scale);
   --        Height := Get_Height (Logical_Rect) / Float (Font_Scale);
   --     end Get_Bounds_For_String;
  
   
   -------------------------
   -- Get_Size_For_String --
   -------------------------
   
   function Get_Size_For_String
     (Text  : String;
      Font  : access Font_Record;
      Scale : Coordinate) return Rectangle_Record is
      
      W : Float := 0.0;
      H : Float := 0.0;
   begin
      null; -- Get_Text_Bounds (Text, Font, W, H);
      
      return New_Rectangle (0.0, 0.0, W, H);
   end Get_Size_For_String;
   
   ---------------
   -- Word_Wrap --
   ---------------
   
   function Word_Wrap
     (Text    : String;
      Metrics : access Font_Metrics_Record;
      Width   : Coordinate) return Strings_Lists.List is
   begin
      
      --  List<String> result = new ArrayList<String>();
      --  -- First split the processing into lines already delimited by
      --  -- newlines. We want the result to retain all newlines in position.
      --  String[] lines = text.split("\n");

      --  		for (int i = 0; i < lines.length; i++)
      --  		{
      --  		int lineWidth = 0; // the display width of the current line
      --  			int charCount = 0; // keeps count of current position in the line
      --  			StringBuilder currentLine = new StringBuilder();

      --  			// Split the words of the current line by spaces and tabs
      --  			// The words are trimmed of tabs, space and newlines, therefore
      --  			String[] words = lines[i].split("\\s+");

      --  			// Need to a form a stack of the words in reverse order
      --  			// This is because if a word is split during the process 
      --  			// the remainder of the word is added to the front of the 
      --  			// stack and processed next
      --  			Stack<String> wordStack = new Stack<String>();

      --  			for (int j = words.length - 1; j >= 0; j--)
      --  			{
      --  				wordStack.push(words[j]);
      --  			}

      --  			while (!wordStack.isEmpty())
      --  			{
      --  				String word = wordStack.pop();

      --  				// Work out what whitespace exists before this word.
      --  				// and add the width of the whitespace to the calculation
      --  				int whitespaceCount = 0;

      --  				if (word.length() > 0)
      --  				{
      --  					// Concatenate any preceding whitespace to the
      --  					// word and calculate the number of characters of that
      --  					// whitespace
      --  					char firstWordLetter = word.charAt(0);
      --  					int letterIndex = lines[i].indexOf(firstWordLetter,
      --  							charCount);
      --  					String whitespace = lines[i].substring(charCount,
      --  							letterIndex);
      --  					whitespaceCount = whitespace.length();
      --  					word = whitespace.concat(word);
      --  				}

      --  				double wordLength;

      --  				// If the line width is zero, we are at the start of a newline
      --  				// We don't proceed preceeding whitespace in the width
      --  				// calculation
      --  				if (lineWidth > 0)
      --  				{
      --  					wordLength = metrics.stringWidth(word);
      --  				}
      --  				else
      --  				{
      --  					wordLength = metrics.stringWidth(word.trim());
      --  				}

      --  				// Does the width of line so far plus the width of the 
      --  				// current word exceed the allowed width?
      --  				if (lineWidth + wordLength > width)
      --  				{
      --  					if (lineWidth > 0)
      --  					{
      --  						// There is already at least one word on this line
      --  						// and the current word takes the overall width over
      --  						// the allowed width. Because there is something on
      --  						// the line, complete the current line, reset the width
      --  						// counter, create a new line and put the current word
      --  						// back on the stack for processing in the next round
      --  						result.add(currentLine.toString());
      --  						currentLine = new StringBuilder();
      --  						wordStack.push(word.trim());
      --  						lineWidth = 0;
      --  					}
      --  					else if (mxConstants.SPLIT_WORDS)
      --  					{
      --  						// There are no words on the current line and the 
      --  						// current word does not fit on it. Find the maximum
      --  						// number of characters of this word that just fit
      --  						// in the available width
      --  						word = word.trim();

      --  						for (int j = 1; j <= word.length(); j++)
      --  						{
      --  							wordLength = metrics.stringWidth(word.substring(0,
      --  									j));

      --  							if (lineWidth + wordLength > width)
      --  							{
      --  								// The last character took us over the allowed
      --  								// width, deducted it unless there is only one
      --  								// character, in which case we have to use it
      --  								// since we can't split it...
      --  								j = j > 1 ? j - 1 : j;
      --  								String chars = word.substring(0, j);
      --  								currentLine = currentLine.append(chars);
      --  								// Return the unprocessed part of the word 
      --  								// to the stack
      --  								wordStack
      --  										.push(word.substring(j, word.length()));
      --  								result.add(currentLine.toString());
      --  								currentLine = new StringBuilder();
      --  								lineWidth = 0;
      --  								// Increment char counter allowing for white 
      --  								// space in the original word
      --  								charCount = charCount + chars.length()
      --  										+ whitespaceCount;
      --  								break;
      --  							}
      --  						}
      --  					}
      --  					else
      --  					{
      --  						// There are no words on the current line, but
      --  						// we are not splitting.
      --  						word = word.trim();
      --  						result.add(word);
      --  						currentLine = new StringBuilder();
      --  						lineWidth = 0;
      --  						// Increment char counter allowing for white 
      --  						// space in the original word
      --  						charCount = word.length() + whitespaceCount;
      --  					}
      --  				}
      --  				else
      --  				{
      --  					// The current word does not take the total line width
      --  					// over the allowed width. Append the word, removing
      --  					// preceeding whitespace if it is the first word in the
      --  					// line.
      --  					if (lineWidth > 0)
      --  					{
      --  						currentLine = currentLine.append(word);
      --  					}
      --  					else
      --  					{
      --  						currentLine = currentLine.append(word.trim());
      --  					}

      --  					lineWidth += wordLength;
      --  					charCount += word.length();
      --  				}
      --  			}

      --  			result.add(currentLine.toString());
      --  		}

      --  		return result.toArray(new String[result.size()]);
      return Strings_Lists.Empty_List;
   end Word_Wrap;
   
   -----------------------
   -- Get_Size_For_Html --
   -----------------------
   
   function Get_Size_For_Html
     (Markup     : String;
      Style      : Strings_Maps.Map;
      Scale      : Coordinate;
      Wrap_Width : Coordinate) return Rectangle_Record is
   begin
      return No_Rectangle_Record;
   end Get_Size_For_Html;
   
   -------------------
   -- Arc_To_Curves --
   -------------------
   
   function Arc_To_Curves
     (X0             : Coordinate;
      Y0             : Coordinate;
      R1a            : Coordinate;
      R2a            : Coordinate;
      Angle          : Coordinate;
      Large_Arc_Flag : Coordinate;      
      Sweep_Flag     : Coordinate;
      Xa             : Coordinate;
      Ya             : Coordinate) return Coordinates_Lists.List is
      
      X     : Coordinate := Xa;
      Y     : Coordinate := Ya;
      R1    : Coordinate := R1a;
      R2    : Coordinate := R2a;
      FS    : Coordinate;
      Psai  : Coordinate;
      Ctx   : Coordinate;
      Cty   : Coordinate;
      Cpsi  : Coordinate;
      Spsi  : Coordinate;
      Rxd   : Coordinate; 
      Ryd   : Coordinate;
      Rxdd  : Coordinate;
      Rydd  : Coordinate;
      R1x   : Coordinate;
      R2y   : Coordinate;
      Lamda : Coordinate;
      Sds   : Coordinate;
      Seif  : Coordinate;
      
      Txd   : Coordinate;
      Tyd   : Coordinate;
      Tx    : Coordinate;
      Ty    : Coordinate;
      Rad   : Coordinate;
      S1    : Coordinate;
      Dr    : Coordinate;
      
      Sse    : Coordinate;
      Seg    : Integer;
      Segr   : Coordinate;
      T      : Coordinate;
      Cpsir1 : Coordinate;
      Cpsir2 : Coordinate;
      Spsir1 : Coordinate;
      Spsir2 : Coordinate;
      Mc     : Coordinate;
      Ms     : Coordinate;
      X2     : Coordinate;
      Y2     : Coordinate;
      X3     : Coordinate;
      Y3     : Coordinate;
      
      Dx     : Coordinate;
      Dy     : Coordinate;
      
      Result : Coordinates_Lists.List;
   begin
      X := X - X0;
      Y := Y - Y0;

      if R1 = 0.0 or R2 = 0.0 then
         Coordinates_Lists.Append (Result, 0.0);
         return Result;
      end if;

      FS    := Sweep_Flag;
      Psai  := Angle;
      R1    := abs (R1);
      R2    := abs (R2);
      Ctx   := -X / 2.0;
      Cty   := -Y / 2.0;
      Cpsi  := Maths.Cos (Psai * Maths.PI / 180.0);
      Spsi  := Maths.Sin (Psai * Maths.PI / 180.0);
      Rxd   := Cpsi * Ctx + Spsi * Cty;
      Ryd   := -1.0 * Spsi * Ctx + Cpsi * Cty;
      Rxdd  := Rxd * Rxd;
      Rydd  := Ryd * Ryd;
      R1x   := R1 * R1;
      R2y   := R2 * R2;
      Lamda := Rxdd / R1x + Rydd / R2y;

      if Lamda > 1.0 then
         R1  := Maths.Sqrt (Lamda) * R1;
         R2  := Maths.Sqrt (Lamda) * R2;
         Sds := 0.0;
      else
         Seif := 1.0;

         if Large_Arc_Flag = FS then
            Seif := -1.0;
         end if;

         Sds := Seif * Maths.Sqrt
           ((R1x * R2y - R1x * Rydd - R2y * Rxdd) / (R1x * Rydd + R2y * Rxdd));
      end if;
      
      Txd := Sds * R1 * Ryd / R2;
      Tyd := -1.0 * Sds * R2 * Rxd / R1;
      Tx  := Cpsi * Txd - Spsi * Tyd + X / 2.0;
      Ty  := Spsi * Txd + Cpsi * Tyd + Y / 2.0;
      Rad := Maths.Arctan2 ((Ryd - Tyd) / R2, (Rxd - Txd) / R1)
        - Maths.Arctan2 (0.0, 1.0);
      
      if Rad >= 0.0 then
         S1 := Rad; 
      else
         S1 := 2.0 * Maths.PI + Rad;
      end if;
      
      Rad := Maths.Arctan2 ((-Ryd - Tyd) / R2, (-Rxd - Txd) / R1)
        - Maths.Arctan2 ((Ryd - Tyd) / R2, (Rxd - Txd) / R1);
      if Rad >= 0.0 then
         Dr := Rad;
      else
         Dr := 2.0 * Maths.PI + Rad;
      end if;

      if FS = 0.0 and Dr > 0.0 then		
         Dr := Dr - 2.0 * Maths.PI;
      elsif FS /= 0.0 and Dr < 0.0 then
         Dr := Dr + 2.0 * Maths.PI;
      end if;

      Sse := Dr * 2.0 / Maths.PI;
      if Sse < 0.0 then
         Seg := Integer (Maths.Ceil (-1.0 * Sse));
      else
         Seg := Integer (Maths.Ceil (Sse));
      end if;
      
      Segr := Dr / Float (Seg);
      
      T := (8.0 / 3.0) * Maths.Sin (Segr / 4.0) * Maths.Sin (Segr / 4.0)
        / Maths.Sin (Segr / 2.0);
      
      Cpsir1 := Cpsi * R1;
      Cpsir2 := Cpsi * R2;
      Spsir1 := Spsi * R1;
      Spsir2 := Spsi * R2;
      
      Mc := Maths.Cos (S1);
      Ms := Maths.Sin (S1);
      
      X2 := -T * (Cpsir1 * Ms + Spsir2 * Mc);
      Y2 := -T * (Spsir1 * Ms - Cpsir2 * Mc);
      X3 := 0.0;
      Y3 := 0.0;
      
      Coordinates_Lists.Append (Result, Float (Seg) * 6.0);
				
      for N in 0 .. Seg -1 loop
         S1 := S1 + Segr;
         Mc := Maths.Cos (S1);
         Ms := Maths.Sin (S1);

         X3 := Cpsir1 * Mc - Spsir2 * Ms + Tx;
         Y3 := Spsir1 * Mc + Cpsir2 * Ms + Ty;
	 
         Dx := -T * (Cpsir1 * Ms + Spsir2 * Mc);
         Dy := -T * (Spsir1 * Ms - Cpsir2 * Mc);

         -- CurveTo updates x0, y0 so need to restore it
	 
         Coordinates_Lists.Append (Result, X2 + X0);
         Coordinates_Lists.Append (Result, Y2 + Y0);
         Coordinates_Lists.Append (Result, X3 - Dx + X0);
         Coordinates_Lists.Append (Result, Y3 - Dy + Y0);
         Coordinates_Lists.Append (Result, X3 + X0);
         Coordinates_Lists.Append (Result, Y3 + Y0);

         X2 := X3 + Dx;
         Y2 := Y3 + Dy;
      end loop;

      return Result;
   end Arc_To_Curves;
   
   ----------------------
   -- Get_Bounding_Box --
   ----------------------
   
   function Get_Bounding_Box
     (Rect     : Rectangle_Record;
      Rotation : Coordinate) return Rectangle_Record is
      
      Result : Rectangle_Record;
      Rad    : Coordinate;
      Cos    : Coordinate;
      Sin    : Coordinate;
      Cx     : Point_Record; 
      P1     : Point_Record; 
      P2     : Point_Record; 
      P3     : Point_Record; 
      P4     : Point_Record; 
      Tmp    : Rectangle_Record;
   begin
      Result := No_Rectangle_Record;
      if Rect /= No_Rectangle_Record and Rotation /= 0.0 then
         Rad := Maths.To_Radians (Rotation);
         Cos := Maths.Cos (Rad);
         Sin := Maths.Sin (Rad);

         Cx := Point_Record'
           (Get_X (Rect) + Get_Width (Rect) / 2.0,
            Get_Y (Rect) + Get_Height (Rect) / 2.0);
	 
         P1 := Point_Record'
           (Get_X (Rect), Get_Y (Rect));
         P2 := Point_Record'
           (Get_X (Rect) + Get_Width (Rect), Get_Y (Rect));
         P3 := Point_Record'
           (Get_X (P2), Get_Y (Rect) + Get_Height (Rect));
         P4 := Point_Record'
           (Get_X (Rect), Get_Y (P3));
			
         P1 := Get_Rotated_Point (P1, Cos, Sin, Cx);
         P2 := Get_Rotated_Point (P2, Cos, Sin, Cx);
         P3 := Get_Rotated_Point (P3, Cos, Sin, Cx);
         P4 := Get_Rotated_Point (P4, Cos, Sin, Cx);

         Tmp := Rectangle_Record'
           (Point_Record'(Maths.Round (Get_X (P1)), Maths.Round (Get_Y (P1))),
            0.0, 0.0);
	 
         Add (Tmp, P2);
         Add (Tmp, P3);
         Add (Tmp, P4);
	 
         Result := Tmp;
	 
      elsif Rect /= No_Rectangle_Record then
         Result := Rect;
      end if;

      return Result;
   end Get_Bounding_Box;
   
   -------------------
   -- First_Char_At --
   -------------------
   
   function First_Char_At
     (Text       : String;
      Input_Char : Integer;
      From_Index : Integer) return Integer is
   begin
      return 0;
   end First_Char_At;
   
   -----------------------
   -- Get_Rotated_Point --
   -----------------------
   
   function Get_Rotated_Point
     (Pt  : Point_Record;
      Cos : Coordinate;
      Sin : Coordinate) return Point_Record is
   begin
      return Get_Rotated_Point (Pt, Cos, Sin, Zero_Point_Record);
   end Get_Rotated_Point;
   
   --------------------
   -- Pt_Seg_Dist_Sq --
   --------------------
   
   function Pt_Seg_Dist_Sq
     (X1   : Float;
      Y1   : Float;
      X2   : Float;
      Y2   : Float;
      Px   : Float;
      Py   : Float) return Float is
      
      X_1         : Float := X1;
      Y_1         : Float := Y1;
      X_2         : Float := X2;
      Y_2         : Float := Y2;
      P_X         : Float := Py;
      P_Y         : Float := Px;
      Dot_Prod    : Float;
      Proj_Len_Sq : Float;
      Len_Sq      : Float;
   begin
      -- Adjust vectors relative to x1,y1
      -- x2,y2 becomes relative vector from x1,y1 to end of segment
      
      X_2 := X_2 - X_1;
      Y_2 := Y_2 - Y_1;
      
      -- px,py becomes relative vector from x1,y1 to test point
      
      P_X := P_X - X_1;
      P_Y := P_Y - Y_1;
      
      Dot_Prod := P_X * X_2 + P_Y * Y_2;
      
      if Dot_Prod <= 0.0 then
         -- px,py is on the side of x1,y1 away from x2,y2
         -- distance to segment is length of px,py vector
         -- "length of its (clipped) projection" is now 0.0
	 
         Proj_Len_Sq := 0.0;
	 
      else
         -- switch to backwards vectors relative to x2,y2
         -- x2,y2 are already the negative of x1,y1=>x2,y2
         -- to get px,py to be the negative of px,py=>x2,y2
         -- the dot product of two negated vectors is the same
         -- as the dot product of the two normal vectors
	 
         P_X := X_2 - P_X;
         P_Y := Y_2 - P_Y;
	 
         Dot_Prod := P_X * X_2 + P_Y * Y_2;
	 
         if Dot_Prod <= 0.0 then
            -- px,py is on the side of x2,y2 away from x1,y1
            -- distance to segment is length of (backwards) px,py vector
            -- "length of its (clipped) projection" is now 0.0
	    
            Proj_Len_Sq := 0.0;
	    
         else
            -- px,py is between x1,y1 and x2,y2
            -- dotprod is the length of the px,py vector
            -- projected on the x2,y2=>x1,y1 vector times the
            -- length of the x2,y2=>x1,y1 vector
	    
            Proj_Len_Sq := Dot_Prod * Dot_Prod / (X_2 * X_2 + Y_2 * Y_2);
         end if;
      end if;
      
      -- Distance to line is now the length of the relative point
      -- vector minus the length of its projection onto the line
      -- (which is zero if the projection falls outside the range
      --  of the line segment).
      
      Len_Sq := P_X * P_X + P_Y * P_Y - Proj_Len_Sq;
      
      if Len_Sq < 0.0 then
         Len_Sq := 0.0;
      end if;
      
      return Len_Sq;
   end Pt_Seg_Dist_Sq;
   
   --------------------------
   -- Find_Nearest_Segment --
   --------------------------
   
   function Find_Nearest_Segment
     (State : access Cell_State_Record'Class;
      X     : Coordinate;
      Y     : Coordinate) return Integer is
      
      Index   : Integer := -1;
      Last    : Point_Record;
      Min     : Coordinate;
      Dist    : Coordinate;
      Points  : Point_Lists.List;
      I       : Integer;
   begin
      Points := State.Get_Absolute_Points;
      
      if not Points.Is_Empty then
	 
         Last := State.Get_First_Absolute_Point;
         Min  := Coordinate'Last;
	 
         Points := State.Get_Absolute_Points;
	 
         I := 0;
         for Current of Points loop
            Dist := Pt_Seg_Dist_Sq
              (Get_X (Last),    Get_Y (Last), 
               Get_X (Current), Get_Y (Current),
               X, Y);
	    
            if Dist < Min then 
               Min := Dist;
               Index := I;
            end if;

            Last := Current;
            I := I + 1;
         end loop;
      end if;
      
      return Index;
   end Find_Nearest_Segment;
   
   -----------------------
   -- Get_Rotated_Point --
   -----------------------
   
   function Get_Rotated_Point
     (Pt  : Point_Record;
      Cos : Coordinate;
      Sin : Coordinate;
      C   : Point_Record) return Point_Record is
      
      X  : Coordinate;
      Y  : Coordinate;
      X1 : Coordinate;
      Y1 : Coordinate;
   begin
      X := Get_X (Pt) - Get_X (C);
      Y := Get_Y (Pt) - Get_Y (C);

      X1 := X * Cos - Y * Sin;
      Y1 := Y * Cos + X * Sin;
      
      return Point_Record'(X1 + Get_X (C), Y1 + Get_Y (C));
   end Get_Rotated_Point;
   
   --------------------------
   -- Get_Port_Constraints --
   --------------------------
   
   function Get_Port_Constraints
     (Terminal : access Cell_State_Record'Class;
      Edge     : access Cell_State_Record'Class;
      Source   : Boolean) return Integer is
   begin
      return Get_Port_Constraints
        (Terminal, Edge, Source, DIRECTION_MASK_NONE);
   end Get_Port_Constraints;
   
   --------------------------
   -- Get_Port_Constraints --
   --------------------------
   
   function Get_Port_Constraints
     (Terminal      : access Cell_State_Record'Class;
      Edge          : access Cell_State_Record'Class;
      Source        : Boolean;
      Default_Value : Integer) return Integer is
      
      use Gnat;

      Directions   : Name_Id;
      Return_Value : Integer;
   begin
      Directions := Get_String (Terminal.Get_Style, STYLE_PORT_CONSTRAINT);

      if Directions = No_Name then
         Return_Value := Default_Value;
	 
      else
         Return_Value := DIRECTION_MASK_NONE;
	 
         declare
            use Strings_Lists;
	    
            Dirs : String := Get_String (Directions);
            Subs : String_Split.Slice_Set;
            Seps : constant String := "-,";
         begin
            if Dirs /= "" then
               String_Split.Create
                 (S          => Subs,
                  From       => Dirs,
                  Separators => Seps,
                  Mode       => String_Split.Multiple);
	       
               for I in 1..Slice_Count (Subs) loop
                  declare
                     S   : String := String_Split.Slice (Subs, I);
                     Dir : Name_Id;
                  begin
                     Dir := String_Find (S);
		     
                     if Dir = DIRECTION_NORTH then
                        Return_Value := Return_Value or DIRECTION_MASK_NORTH;
                     end if;
		     
                     if Dir = DIRECTION_WEST then
                        Return_Value := Return_Value or DIRECTION_MASK_WEST;
                     end if;
		     
                     if Dir = DIRECTION_SOUTH then
                        Return_Value := Return_Value or DIRECTION_MASK_SOUTH;
                     end if;
		     
                     if Dir = DIRECTION_EAST then
                        Return_Value := Return_Value or DIRECTION_MASK_EAST;
                     end if;
                  end;
               end loop;
            end if;
         end;
      end if;

      return Return_Value;
   end Get_Port_Constraints;
   
   ------------------------------
   -- Reverse_Port_Constraints --
   ------------------------------
   
   function Reverse_Port_Constraints (Constraint : Integer) return Integer is
   begin
      return 0;
   end Reverse_Port_Constraints;
   
   ---------------------
   -- Draw_Image_Clip --
   ---------------------
   
   --  procedure Draw_Image_Clip
   --    (G        : access Graphics;
   --     Image    : access Buffered_Image;
   --     Observer : access Image_Observer) is
   --  begin
   --     null;
   --  end Draw_Image_Clip;
   
   -----------------------
   -- Fill_Clipped_Rect --
   -----------------------
   
   --  procedure Fill_Clipped_Rect
   --    (G      : access Graphics;
   --     X      : Integer;
   --     Y      : Integer;
   --     Width  : Integer;
   --     Height : Integer) is
   --  begin
   --     null;
   --  end Fill_Clipped_Rect;
   
   ----------------------
   -- Translate_Points --
   ----------------------
   
   function Translate_Points
     (Pts : Point_Lists.List;
      Dx  : Coordinate;
      Dy  : Coordinate) return Point_Lists.List is
      
      Result : Point_Lists.List := Point_Lists.Empty_List; 
      New_Pt : Point_Record;
   begin
      if not Point_Lists.Is_Empty (Pts) then
	 
         for Pt of Pts loop
            New_Pt := Translate_Point (Pt, Dx, Dy);
            Point_Lists.Append (Result, New_Pt);
         end loop;
      end if;

      return Result;
   end Translate_Points;
   
   ----------------
   -- Sort_Cells --
   ----------------
   
   function Sort_Cells
     (Cells     : Cells_Lists.List;
      Ascending : Boolean := True) return Cells_Lists.List is
      
      use Cells_Lists;
      
      function Compare (Left, Right : Cell_Class_Ptr) return Boolean;
      
      -------------
      -- Compare --
      -------------
      
      function Compare (Left, Right : Cell_Class_Ptr) return Boolean is
	 
         SL  : String := Artics.Graph.Cells_Paths.Create (Left);
         SR  : String := Artics.Graph.Cells_Paths.Create (Right);
         Res : Integer;
      begin
         Res := Artics.Graph.Cells_Paths.Compare (Sl, Sr);
         if Ascending then
            return Res > 0;
         else
            return Res < 0;
         end if;
      end Compare;
      
      package Sorting is new Cells_Lists.Generic_Sorting (Compare);
      
      Result : Cells_Lists.List := Cells;
   begin
      Sorting.Sort (Result);
      
      return Result;
   end Sort_Cells;
   
   --------------
   -- Contains --
   --------------
   
   function Contains 
     (Cells : Cells_Lists.List;
      Cell   : access Cell_Record'Class) return Boolean is
   begin
      return Cells_Lists.Contains (Cells, Cell);
   end Contains;
   
   --------------
   -- Index_Of --
   --------------
   
   function Index_Of
     (Cells : Cells_Lists.List;
      Cell  : access Cell_Record'Class) return Integer is
   begin
      return Cells_Lists_Helpers.Get_Position (Cells, Cell);
   end Index_Of;
   
   ------------------------
   -- Intersects_Hotspot --
   ------------------------
   
   function Intersects_Hotspot
     (State   : access Cell_State_Record'Class;
      X       : Coordinate;
      Y       : Coordinate;
      Hotspot : Coordinate) return Boolean is
   begin
      return Intersects_Hotspot (State, X, Y, Hotspot, 0.0, 0.0);
   end Intersects_Hotspot;
   
   ------------------------
   -- Intersects_Hotspot --
   ------------------------
   
   function Intersects_Hotspot
     (State   : access Cell_State_Record'Class;
      X       : Coordinate;
      Y       : Coordinate;
      Hotspot : Coordinate;
      Min     : Coordinate;
      Max     : Coordinate) return Boolean is
      
      Cx     : Coordinate;
      Cy     : Coordinate;
      Width  : Coordinate;
      Height : Coordinate;
      Start  : Coordinate;
      W      : Coordinate;
      H      : Coordinate;
      Rect   : Rectangle_Record;
   begin
      if Hotspot > 0.0 then
         Cx     := Maths.Round (State.Get_Center_X);
         Cy     := Maths.Round (State.Get_Center_Y);
         Width  := Maths.Round (State.Get_Width);
         Height := Maths.Round (State.Get_Height);

         if Get_String (State.Get_Style, STYLE_SHAPE, No_Name) = SHAPE_SWIMLANE
         then
            Start := Coordinate (Get_Int (State.Get_Style,
                                 STYLE_STARTSIZE,
                                 Default_Start_Size));
	    
            if Is_True (State.Get_Style, STYLE_HORIZONTAL, True) then
               Cy     := Maths.Round (State.Get_Y + (Start / 2.0));
               Height := Start;
            else
               Cx    := Maths.Round (State.Get_X + (Start / 2.0));
               Width := Start;
            end if;
         end if;
		  
         W := Maths.Max (Min, Width * Hotspot);
         H := Maths.Max (Min, Height * Hotspot);
		
         if Max > 0.0 then
            W := Maths.Min (W, Max);
            H := Maths.Min (H, Max);
         end if;
		    
         Rect := New_Rectangle 
           (Maths.Round (Cx - (W / 2.0)), Maths.Round (Cy - (H / 2.0)), W, H);

         return Contains (Rect, X, Y);
      end if;

      return True;
   end Intersects_Hotspot;
   
   -------------
   -- Is_True --
   -------------
   
   function Is_True
     (Dict : Strings_Maps.Map;
      Key  : String) return Boolean is
   begin
      return Is_True (Dict, Key, False);
   end Is_True;

   -------------
   -- Is_True --
   -------------
   
   function Is_True
     (Dict          : Strings_Maps.Map;
      Key           : String;
      Default_Value : Boolean) return Boolean is
   begin
      return Is_True (Dict, String_Find (Key), Default_Value);
   end Is_True;

   -------------
   -- Is_True --
   -------------
   
   function Is_True
     (Dict : Strings_Maps.Map;
      Key  : Name_Id) return Boolean is
      
   begin
      return Is_True (Dict, Key, False);
   end Is_True;

   -------------
   -- Is_True --
   -------------
   
   function Is_True
     (Dict          : Strings_Maps.Map;
      Key           : Name_Id;
      Default_Value : Boolean) return Boolean is
      
      use Strings_Maps;
      
      Value : Name_Id;
   begin
      if Dict /= Strings_Maps.Empty_Map and then Dict.Contains (Key) then
	 
         Value := Strings_Maps.Element (Dict, Key);
         return Value = Name_One or Value = Name_True;
      else
         return Default_Value;
      end if;
   end Is_True;
   
   -------------
   -- Get_Int --
   -------------
   
   function Get_Int
     (Dict : Strings_Maps.Map;
      Key  : String) return Integer is
   begin
      return Get_Int (Dict, String_Find (Key), 0);
   end Get_Int;

   -------------
   -- Get_Int --
   -------------
   
   function Get_Int
     (Dict          : Strings_Maps.Map;
      Key           : String;
      Default_Value : Integer) return Integer is
   begin
      return Get_Int (Dict, String_Find (Key), Default_Value);
   end Get_Int;
   
   -------------
   -- Get_Int --
   -------------
   
   function Get_Int
     (Dict : Strings_Maps.Map;
      Key  : Name_Id) return Integer is
   begin
      return Get_Int (Dict, Key, 0);
   end Get_Int;
   
   -------------
   -- Get_Int --
   -------------
   
   function Get_Int
     (Dict          : Strings_Maps.Map;
      Key           : Name_Id;
      Default_Value : Integer) return Integer is
      
      use Strings_Maps;
      
      Value : Name_Id;
   begin
      if Dict /= Strings_Maps.Empty_Map and then Dict.Contains (Key) then
	 
         Value := Strings_Maps.Element (Dict, Key);
         return Integer_From_Name (Value);
	 
      else
         return Default_Value;
      end if;
   end Get_Int;
   
   ---------------
   -- Get_Float --
   ---------------
   
   function Get_Float
     (Dict : Strings_Maps.Map;
      Key  : String) return Coordinate is
   begin
      return Get_Float (Dict, String_Find (Key), 0.0);
   end Get_Float;

   ---------------
   -- Get_Float --
   ---------------
   
   function Get_Float
     (Dict          : Strings_Maps.Map;
      Key           : String;
      Default_Value : Coordinate) return Float is
   begin
      return Get_Float (Dict, String_Find (Key), Default_Value);
   end Get_Float;
   
   ---------------
   -- Get_Float --
   ---------------
   
   function Get_Float
     (Dict : Strings_Maps.Map;
      Key  : Name_Id) return Coordinate is
   begin
      return Get_Float (Dict, Key, 0.0);
   end Get_Float;
   
   ---------------
   -- Get_Float --
   ---------------
   
   function Get_Float
     (Dict          : Strings_Maps.Map;
      Key           : Name_Id;
      Default_Value : Coordinate) return Float is
      
      use Strings_Maps;
      
      Value : Name_Id;
   begin
      if Dict /= Strings_Maps.Empty_Map and then Dict.Contains (Key) then
         Value := Strings_Maps.Element (Dict, Key);
         return Float_From_Name (Value);
	 
      else
         return Default_Value;
      end if;
   end Get_Float;
   
   ---------------
   -- Get_Float --
   ---------------
   
   function Get_Float
     (Dict          : Strings_Maps.Map;
      Key           : String;
      Default_Value : Integer) return Float is
   begin
      return Get_Float (Dict, Key, Coordinate (Default_Value));
   end Get_Float;
   
   ---------------
   -- Get_Float --
   ---------------
   
   function Get_Float
     (Dict          : Strings_Maps.Map;
      Key           : Name_Id;
      Default_Value : Integer) return Float is
   begin
      return Get_Float (Dict, Key, Coordinate (Default_Value));
   end Get_Float;
   
   ---------------------
   -- Get_Float_Array --
   ---------------------
   
   function Get_Float_Array_Ptr
     (Dict          : Strings_Maps.Map;
      Key           : String;
      Default_Value : Float_Array_Ptr;
      Separator     : String := ",") return Float_Array_Ptr is
   begin
      return Get_Float_Array_Ptr
        (Dict, String_Find (Key), Default_Value, Separator);
   end Get_Float_Array_Ptr;
   
   ---------------------
   -- Get_Float_Array --
   ---------------------
   
   function Get_Float_Array_Ptr
     (Dict          : Strings_Maps.Map;
      Key           : Name_Id;
      Default_Value : Float_Array_Ptr;
      Separator     : String := ",") return Float_Array_Ptr is

      use Strings_Maps;
      use Gnat;
      use Float_Arrays;
      
      Result : Float_Array_Ptr;
      Pairs  : String_Split.Slice_Set;
      Seps   : String := Separator;
      Value  : Name_Id;
      V      : Coordinate;
   begin
      if Dict /= Strings_Maps.Empty_Map and then Dict.Contains (Key) then
         Value := Strings_Maps.Element (Dict, Key);
        
         declare
            S : String := Get_String (Value);
         begin
            String_Split.Create
              (S          => Pairs,
               From       => S,
               Separators => Seps,
               Mode       => String_Split.Multiple);
            
            if Slice_Count (Pairs) > 0 then
               Result := Float_Arrays.New_Dynamic_Array (Integer (Slice_Count (Pairs)));
               
               for I in 1..Slice_Count (Pairs) loop
                  V := Float_From_String (String_Split.Slice (Pairs, I));
                  Result (Integer (I)) := V;
               end loop;
            end if;
         end;
	 
         return Result;
      else
         return Default_Value;
      end if;
   end Get_Float_Array_Ptr;
   
   ---------------------
   -- Get_Float_Array --
   ---------------------
   
   function Get_Float_Array
     (Dict          : Strings_Maps.Map;
      Key           : String;
      Default_Value : Coordinates_Lists.List;
      Separator     : String := ",") return Coordinates_Lists.List is
   begin
      return Get_Float_Array
        (Dict, String_Find (Key), Default_Value, Separator);
   end Get_Float_Array;
    
   ---------------------
   -- Get_Float_Array --
   ---------------------
   
   function Get_Float_Array
     (Dict          : Strings_Maps.Map;
      Key           : Name_Id;
      Default_Value : Coordinates_Lists.List;
      Separator     : String := ",") return Coordinates_Lists.List is
      
      use Strings_Maps;
      use Gnat;
      
      Result : Coordinates_Lists.List;
      Pairs  : String_Split.Slice_Set;
      Seps   : String := Separator;
      Value  : Name_Id;
      V      : Coordinate;
   begin
      if Dict /= Strings_Maps.Empty_Map and then Dict.Contains (Key) then
         Value := Strings_Maps.Element (Dict, Key);
	 
         declare
            S : String := Get_String (Value);
         begin
            String_Split.Create
              (S          => Pairs,
               From       => S,
               Separators => Seps,
               Mode       => String_Split.Multiple);
	    
            for I in 1..Slice_Count (Pairs) loop
               V := Float_From_String (String_Split.Slice (Pairs, I));
               Coordinates_Lists.Append (Result, V);
            end loop;
         end;
	 
         return Result;
      else
         return Default_Value;
      end if;
   end Get_Float_Array;
   
   ----------------
   -- Get_Double --
   ----------------
   
   function Get_Double
     (Dict : Strings_Maps.Map;
      Key  : String) return Coordinate is
   begin
      return 0.0;
   end Get_Double;

   ----------------
   -- Get_Double --
   ----------------
   
   function Get_Double
     (Dict          : Strings_Maps.Map;
      Key           : String;
      Default_Value : Coordinate) return Coordinate is
   begin
      return 0.0;
   end Get_Double;
   
   function Get_Double
     (Dict : Strings_Maps.Map;
      Key  : Name_Id) return Coordinate is
   begin
      return 0.0;
   end Get_Double;
   
   function Get_Double
     (Dict          : Strings_Maps.Map;
      Key           : Name_Id;
      Default_Value : Coordinate) return Coordinate is
   begin
      return 0.0;
   end Get_Double;
   
   ----------------
   -- Get_String --
   ----------------
   
   function Get_String
     (Dict : Strings_Maps.Map;
      Key  : String) return String is
      
      S : Name_Id := Get_String (Dict, String_Find (Key), No_Name);
   begin
      return Get_String (S);
   end Get_String;
	  
   ----------------
   -- Get_String --
   ----------------
   
   function Get_String
     (Dict : Strings_Maps.Map;
      Key  : Name_Id) return Name_Id is
   begin
      return Get_String (Dict, Key, No_Name);
   end Get_String;
	  
   ----------------
   -- Get_String --
   ----------------
   
   function Get_String
     (Dict          : Strings_Maps.Map;
      Key           : String;
      Default_Value : String) return String is
      
      S : Name_Id := Get_String
        (Dict, String_Find (Key), String_Find (Default_Value));
   begin
      return Get_String (S);
   end Get_String;
   
   ----------------
   -- Get_String --
   ----------------
   
   function Get_String
     (Dict          : Strings_Maps.Map;
      Key           : Name_Id;
      Default_Value : Name_Id) return Name_Id is
      
      use Strings_Maps;
   begin
      if Dict /= Strings_Maps.Empty_Map and then Dict.Contains (Key) then
         return Strings_Maps.Element (Dict, Key);
      else
         return Default_Value;
      end if;
   end Get_String;
   
   --     ---------------
   --     -- Get_Color --
   --     ---------------
   --     
   --     function Get_Color_Name
   --       (Style         : Strings_Maps.Map;
   --        Style_Name    : String;
   --        Default_Value : Name_Id := No_Name) return Name_Id is
   --     begin
   --        return Get_Color_Name (Style, String_Find (Style_Name), Default_Value);
   --     end Get_Color_Name;
   --     
   --     function Get_Color_Name
   --       (Style         : Strings_Maps.Map;
   --        Style_Name    : Name_Id;
   --        Default_Value : Name_Id := No_Name) return Name_Id is
   --     begin
   --        if Strings_Maps.Contains (Style, Style_Name) then
   --           return Strings_Maps.Element (Style, Style_Name);
   --        else
   --           return Default_Value;
   --        end if;
   --     end Get_Color_Name;
   --     
   --     ---------------
   --     -- Get_Color --
   --     ---------------
   --     
   --     function Get_Color
   --       (Style         : Strings_Maps.Map;
   --        Style_Name    : String;
   --        Default_Value : Name_Id := No_Name) return access Color_Record is
   --     begin
   --        return Get_Color (Style, String_Find (Style_Name), Default_Value);
   --     end Get_Color;
   --     
   --     function Get_Color
   --       (Style         : Strings_Maps.Map;
   --        Style_Name    : Name_Id;
   --        Default_Value : Name_Id := No_Name) return access Color_Record is
   --        
   --        Color_Name : Name_Id;
   --     begin
   --        Color_Name := Get_Color_Name (Style, Style_Name, Default_Value);
   --        
   --        if Color_Name /= No_Name then
   --  	 return Get_Html_Color (Color_Name);
   --        else
   --  	 return Null_Color;
   --        end if;
   --     end Get_Color;
   --     
   --------------
   -- Get_Font --
   --------------
   
   --     function Get_Font (Style : Strings_Maps.Map) return access Font_Record is
   --     begin
   --        return Get_Font (Style, 1.0);
   --     end Get_Font;
   
   --------------
   -- Get_Font --
   --------------
   
   --     function Get_Font
   --       (Style : Strings_Maps.Map;
   --        Scale : Coordinate) return access Font_Record is
   --        
   --        Family     : Name_Id;
   --        Size       : Float;
   --        Sdef       : Integer;
   --        Font_Style : Font_Style_Record;
   --     begin
   --        Family := Get_String (Style, STYLE_FONTFAMILY, DEFAULT_FONTFAMILY);
   --        Size   := Float (Get_Int (Style, STYLE_FONTSIZE, Default_Font_Size));
   --        Sdef   := Get_Int (Style, STYLE_FONTSTYLE);
   --        
   --        Font_Style := Default_Font_Style_Record;
   --        
   --        if (Sdef and Font_Bold) /= 0 then
   --  	 Font_Style.Weight := Font_Weight_Bold;
   --        else
   --  	 Font_Style.Weight := Font_Weight_Normal;
   --        end if;
   --        
   --        if (Sdef and FONT_ITALIC) /= 0 then
   --  	 Font_Style.Slant := Font_Slant_Italic;
   --        elsif (Sdef and FONT_OBLIQUE) /= 0 then
   --  	 Font_Style.Slant := Font_Slant_Oblique;
   --        else
   --  	 Font_Style.Slant := Font_Slant_Normal;
   --        end if;
   --        
   --        if (Sdef and Font_Simple_Underline) /= 0 then
   --  	 Font_Style.Underline := Font_Underline_Single;
   --        elsif (Sdef and Font_Double_Underline) /= 0 then
   --  	 Font_Style.Underline := Font_Underline_Double;
   --        else
   --  	 Font_Style.Underline := Font_Underline_None;
   --        end if;
   --        
   --        if (Sdef and Font_Strikethrough) /= 0 then
   --  	 Font_Style.Strikethrough := True;
   --        else
   --  	 Font_Style.Strikethrough := False;
   --        end if;
   --        
   --        return New_Font (Family, Size * Scale, Font_Style);
   --     end Get_Font;
      
   --     ----------------
   --     -- Hex_String --
   --     ----------------
   --     
   --     function Hex_String (Color : access Color_Record'Class) return String is
   --     begin
   --        return "";
   --     end Hex_String;
   --     
   --     -----------------
   --     -- Parse_Color --
   --     -----------------
   --     
   --     function Parse_Color
   --       (Color_String : String) return access Color_Record'Class is
   --     begin
   --        return null;
   --     end Parse_Color;
   --     
   --     --------------------------
   --     -- Get_Hex_Color_String --
   --     --------------------------
   --     
   --     function Get_Hex_Color_String
   --       (Color : access Color_Record'Class) return String is
   --     begin
   --        return "";
   --     end Get_Hex_Color_String;
   --     
   ------------------------
   -- Parse_Dash_Pattern --
   ------------------------
   
   function Parse_Dash_Pattern
     (Dash_Pattern_String : String) return Coordinates_Lists.List is
   begin
      return Coordinates_Lists.Empty_List;
   end Parse_Dash_Pattern;
   
   ---------------
   -- Read_File --
   ---------------
   
   function Read_File (File_Name : String) return String is
   begin
      return "";
   end Read_File;
   
   ----------------
   -- Write_File --
   ----------------
   
   procedure Write_File
     (Contents  : String;
      File_Name : String) is
   begin
      null;
   end Write_File;
   
end Artics.Graph.Utils;
