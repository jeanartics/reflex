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
-- Reflex is originally developed  by the Artics team at Grenoble (France). --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.String_Split; use GNAT.String_Split; 

with Artics.Graph.Names; use Artics.Graph.Names;
with Artics.Graph.Constants; use Artics.Graph.Constants;

package body Artics.Graph.Styles_Sheets is

   --------------------
   -- Equivalent_Key --
   --------------------
   
   function Sheet_Equivalent_Key (Left, Right : Name_Id) return Boolean is
   begin
      return Left = Right;
   end Sheet_Equivalent_Key;
   
   ---------------
   -- Hash_Func --
   ---------------
   
   function Sheet_Hash_Func
     (Key : Name_Id) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Key);
   end Sheet_Hash_Func;
   
   ----------
   -- Egal --
   ----------
   
   function Equals (Left, Right : Strings_Maps.Map) return Boolean is
      use Strings_Maps;
   begin
      return Left = Right;
   end Equals;
   
   ---------------------
   -- New_Style_Sheet --
   ---------------------
   
   function New_Style_Sheet return access Style_Sheet_Record'Class 
   is      
      S : access Style_Sheet_Record := 
        new Style_Sheet_Record'(No_Style_Sheet_Record);
      
      Default_Vertex_Style : Strings_Maps.Map;
      Default_Edge_Style   : Strings_Maps.Map;
   begin
      Default_Vertex_Style := S.Create_Default_Vertex_Style;
      Default_Edge_Style   := S.Create_Default_Edge_Style;
      
      S.Set_Default_Vertex_Style (Default_Vertex_Style);
      S.Set_Default_Edge_Style (Default_Edge_Style);
      return S;
   end New_Style_Sheet;
   
   ----------------
   -- Get_Styles --
   ----------------
   
   function Get_Styles
     (S : access Style_Sheet_Record) return Stylenames_Maps.Map is
   begin
      return S.Styles;
   end Get_Styles;
   
   ----------------
   -- Set_Styles --
   ----------------
   
   procedure Set_Styles
     (S      : access Style_Sheet_Record;
      Styles : Stylenames_Maps.Map) is
   begin
      S.Styles := Styles;
   end Set_Styles;
   
   ---------------------------------
   -- Create_Default_Vertex_Style --
   ---------------------------------
   
   function Create_Default_Vertex_Style
     (S : access Style_Sheet_Record) return Strings_Maps.Map is
      
      Style : Strings_Maps.Map;
   begin
      Strings_Maps.Insert
	(Style, STYLE_SHAPE, SHAPE_RECTANGLE);
      
      Strings_Maps.Insert
	(Style, STYLE_PERIMETER, String_Find ("Rectangle_Perimeter"));
      
      Strings_Maps.Insert
	(Style, STYLE_VERTICAL_ALIGN, ALIGN_MIDDLE);
      
      Strings_Maps.Insert
	(Style, STYLE_ALIGN, ALIGN_CENTER);
      
      Strings_Maps.Insert
	(Style, STYLE_FILLCOLOR, String_Find ("#C3D9FF"));
      
      Strings_Maps.Insert
	(Style, STYLE_STROKECOLOR, String_Find ("#6482B9"));
      
      Strings_Maps.Insert
	(Style, STYLE_STROKEWIDTH, String_Find ("1.0"));
      
      Strings_Maps.Insert
	(Style, STYLE_FONTCOLOR, String_Find ("#774400"));

      return Style;
   end Create_Default_Vertex_Style;
   
   -------------------------------
   -- Create_Default_Edge_Style --
   -------------------------------
   
   function Create_Default_Edge_Style
     (S : access Style_Sheet_Record) return Strings_Maps.Map is
      
      Style : Strings_Maps.Map;
   begin
      Strings_Maps.Insert
	(Style, STYLE_SHAPE, SHAPE_CONNECTOR);
      
      Strings_Maps.Insert
	(Style, STYLE_ENDARROW, ARROW_CLASSIC); 
      
      Strings_Maps.Insert
	(Style, STYLE_VERTICAL_ALIGN, ALIGN_MIDDLE);
      
      Strings_Maps.Insert
	(Style, STYLE_ALIGN, ALIGN_CENTER);
      
      Strings_Maps.Insert
	(Style, STYLE_STROKEWIDTH, String_Find ("2.0"));
      
      Strings_Maps.Insert
	(Style, STYLE_STROKECOLOR, String_Find ("#6482B9"));
	 
      Strings_Maps.Insert
	(Style, STYLE_FONTCOLOR, String_Find ("#446299"));
      
      return Style;
   end Create_Default_Edge_Style;
   
   ------------------------------
   -- Get_Default_Vertex_Style --
   ------------------------------
   
   function Get_Default_Vertex_Style 
     (S : access Style_Sheet_Record) return Strings_Maps.Map is
   begin
      return Stylenames_Maps.Element (S.Styles, Default_Vertex_Style_Name);
   end Get_Default_Vertex_Style;
   
   ------------------------------
   -- Set_Default_Vertex_Style --
   ------------------------------
   
   procedure Set_Default_Vertex_Style 
     (S     : access Style_Sheet_Record;
      Value : Strings_Maps.Map) is
      
      use Stylenames_Maps;
      
      Cur : Stylenames_Maps.Cursor;
   begin
      Cur := Stylenames_Maps.Find (S.Styles, Default_Vertex_Style_Name);
      if Cur /= Stylenames_Maps.No_Element then
	 Stylenames_Maps.Delete (S.Styles, Default_Vertex_Style_Name);
      end if;
      
      Stylenames_Maps.Insert (S.Styles, Default_Vertex_Style_Name, value);
   end Set_Default_Vertex_Style;
   
   ----------------------------
   -- Get_Default_Edge_Style --
   ----------------------------
   
   function Get_Default_Edge_Style
     (S : access Style_Sheet_Record) return Strings_Maps.Map is
   begin
      return Stylenames_Maps.Element (S.Styles, Default_Edge_Style_Name);
   end Get_Default_Edge_Style;
   
   ----------------------------
   -- Set_Default_Edge_Style --
   ----------------------------
   
   procedure Set_Default_Edge_Style
     (S     : access Style_Sheet_Record;
      Value : Strings_Maps.Map) is
      
      use Stylenames_Maps;

      Cur : Stylenames_Maps.Cursor;
   begin
      Cur := Stylenames_Maps.Find (S.Styles, Default_Edge_Style_Name);
      if Cur /= Stylenames_Maps.No_Element then
	 Stylenames_Maps.Delete (S.Styles, Default_Edge_Style_Name);
      end if;

      Stylenames_Maps.Insert (S.Styles, Default_Edge_Style_Name, Value);
   end Set_Default_Edge_Style;
   
   --------------------
   -- Put_Cell_Style --
   --------------------
   
   procedure Put_Cell_Style
     (S     : access Style_Sheet_Record;
      Name  : String;
      Style : Strings_Maps.Map) is
   begin
      Put_Cell_Style (S, String_Find (Name), Style);
   end Put_Cell_Style;
   
   --------------------
   -- Put_Cell_Style --
   --------------------
   
   procedure Put_Cell_Style
     (S     : access Style_Sheet_Record;
      Name  : Name_Id;
      Style : Strings_Maps.Map) is
      
      use Stylenames_Maps;
      
      Cur : Stylenames_Maps.Cursor;
   begin
      Cur := Stylenames_Maps.Find (S.Styles, Name);
      if Cur /= Stylenames_Maps.No_Element then
	 Stylenames_Maps.Delete (S.Styles, Name);
      end if;
      
      Stylenames_Maps.Insert (S.Styles, Name, Style);
   end Put_Cell_Style;
   
   -----------------------
   -- Update_Cell_Style --
   -----------------------
   
   procedure Update_Cell_Style
     (S          : access Style_Sheet_Record;
      Style_Name : String;
      Key        : Name_Id;
      Value      : Name_Id) is
   begin
      Update_Cell_Style (S, String_Find (Style_Name), Key, Value);
   end Update_Cell_Style;
   
   -----------------------
   -- Update_Cell_Style --
   -----------------------
   
   procedure Update_Cell_Style
     (S          : access Style_Sheet_Record;
      Style_Name : String;
      Key        : String;
      Value      : String) is
   begin
      Update_Cell_Style
	(S, 
	 String_Find (Style_Name), 
	 String_Find (Key), 
	 String_Find (Value));
   end Update_Cell_Style;
   
   -----------------------
   -- Update_Cell_Style --
   -----------------------
   
   procedure Update_Cell_Style
     (S          : access Style_Sheet_Record;
      Style_Name : Name_Id;
      Key        : Name_Id;
      Value      : Name_Id) is
      
      use Stylenames_Maps;
      
      Style : Strings_Maps.Map := Strings_Maps.Empty_Map;
      Cur   : Stylenames_Maps.Cursor;
      Found : Boolean := False;
   begin
      if S.Styles.Contains (Style_Name) then
	 Style := S.Styles.Element (Style_Name);
	 Found := True;
      end if;
      
      if Style.Contains (Key) then
	 Style.Replace (Key, Value);
      else
	 Style.Insert (Key, Value);
      end if;
      
      if Found then
	 S.Styles.Replace (Style_Name, Style);
      else
	 S.Styles.Insert (Style_Name, Style);
      end if;
   end Update_Cell_Style;
   
   ---------------------------
   -- Clone_Style_From_Name --
   ---------------------------
   
   Function Clone_Style_From_Name 
     (S          : access Style_Sheet_Record;
      Style_Name : String) return Strings_Maps.Map is
      
      Name      : Name_Id := String_Find (Style_Name);
      Style     : Strings_Maps.Map := Strings_Maps.Empty_Map;
      New_Style : Strings_Maps.Map := Strings_Maps.Empty_Map;
      Cur       : Strings_Maps.Cursor;
      Key       : Name_Id;
      Elmt      : Name_Id;
   begin      
      if S.Styles.Contains (Name) then
	 Style := S.Styles.Element (Name);
	 
	 Cur := Strings_Maps.First (Style);
	 while Strings_Maps.Has_Element (Cur) loop
	    Key := Strings_Maps.Key (Cur);
	    Elmt := Strings_Maps.Element (Cur);
	    
	    Strings_Maps.Insert (New_Style, Key, Elmt);
	    Strings_Maps.Next (Cur);
	 end loop;
      end if;
      
      return New_Style;
   end Clone_Style_From_Name;
      
   -------------------------
   -- Get_Style_Key_Value --
   -------------------------
   
   function Get_Style_Key_Value
     (S          : access Style_Sheet_Record;
      Style_Name : Name_Id;
      Key        : Name_Id;
      Value      : Name_Id) return Name_Id is
      
      use Stylenames_Maps;
      
      Style : Strings_Maps.Map := Strings_Maps.Empty_Map;
   begin
      if S.Styles.Contains (Style_Name) then
	 Style := S.Styles.Element (Style_Name);
	 if Style.Contains (Key) then
	    return Style.Element (Key);
	 end if;
      end if;
      
      return No_Name;
   end Get_Style_Key_Value;
   
   --------------------
   -- Get_Cell_Style --
   --------------------
   
   function Get_Cell_Style
     (S             : access Style_Sheet_Record;
      Name          : String;
      Default_Style : Strings_Maps.Map) return Strings_Maps.Map is
      
      use Gnat;
      use Strings_Maps;
      use Stylenames_Maps;
      
      Subs      : String_Split.Slice_Set;
      Seps      : constant String := ";";
      Style     : Strings_Maps.Map := Strings_Maps.Empty_Map;
      Tmp_Style : Strings_Maps.Map;
      Cur       : Stylenames_Maps.Cursor;
   begin
      Copy_Strings_Map (Default_Style, Style);
      
      if String_Find (Name) /= No_Name then
	 
	 String_Split.Create
	   (S          => Subs,
	    From       => Name,
	    Separators => Seps,
	    Mode       => String_Split.Multiple);
	 
	 for I in 1..Slice_Count (Subs) loop
	    declare
	       Tmp : String  := String_Split.Slice (Subs, I);
	       F   : Positive := Tmp'First;
	       C   : Natural := Index (Tmp, "=");
	    begin
	       if C > 0 then
		  declare
		     Key      : String := Tmp (F..C-1);
		     Value    : String := Tmp (C+1..Tmp'Last);
		     Key_Name : Name_Id := String_Find (Key);
		  begin
		     
		     -- First delete the Key if already present in Style
		     
		     if Style.Contains (Key_Name) then
			Style.Delete (Key_Name);
		     end if;
		     
		     -- Insert the key vith the new value if value is not Empty
		     
		     if String_Find (Value) /= None then
			Style.Insert (Key_Name, String_Find (Value));
		     end if;
		  end;
		  
		  -- The style is a style name, so add all key of the style name
		  -- in the style being build
		  
	       else
		  Cur := Stylenames_Maps.Find (S.Styles, String_Find (Tmp));
		  if Cur /= Stylenames_Maps.No_Element then
		     Tmp_Style := Stylenames_Maps.Element (Cur);

		     Copy_Replace_Strings_Map (Tmp_Style, Style);
		  end if;
	       end if;
	    end;
	 end loop;
      end if;
      
      return Style;
   end Get_Cell_Style;
   
   --------------------
   -- Get_Cell_Style --
   --------------------
   
   function Get_Cell_Style
     (S             : access Style_Sheet_Record;
      Name          : Name_Id;
      Default_Style : Strings_Maps.Map) return Strings_Maps.Map is
   begin
      return Get_Cell_Style (S, Get_String (Name), Default_Style);
   end Get_Cell_Style;
   
end Artics.Graph.Styles_Sheets;
	  
