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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.String_Split; use GNAT.String_Split;

package body Artics.Graph.Cells_Paths is
   procedure Put_Line (S : String) is null; -- renames Ada.Text_IO.Put_Line;
   
   ------------
   -- Create --
   ------------
      
   function Create_Helper 
     (Cell : access Cell_Record'Class) return String 
   is
      Pos    : Integer;      
      Parent : access Cell_Record'Class := null;
   begin
      if Cell = null then
         return "";
      else
         Parent := Get_Parent (Cell);
         if Parent = null then
            return "";
         else
            Pos := Parent.Get_Index (Cell);
	    if Parent.Get_Parent /= null then
	       return Create_Helper (Parent) & 
		 Path_Separator         & 
		 Trim (Pos'Img, Both);
	    else
	       return Trim (Pos'Img, Both);
	      end if;
         end if;
      end if;
   end Create_Helper;

   function Create (Cell : access Cell_Record'Class) return String 
   is
      Parent : access Cell_Record'Class;
   begin
      if Cell /= null then
	 Parent := Cell.Get_Parent;
	 return Create_Helper (Cell);
      else
	 return "";
      end if;
   end Create;
   
   -----------------
   -- Get_Path_Id --
   -----------------
   
   function Get_Path_Id (Cell : access Cell_Record'Class) return String is
      Parent : access Cell_Record'Class;
   begin
      if Cell /= null then
	 Parent := Cell.Get_Parent;
	 if Parent /= null then
	    return Get_Path_Id (Parent) & Path_Separator & Get_Id (Cell);
	 else
	    return Get_Id (Cell);
	 end if;
      else
	 return "";
      end if;
   end Get_Path_Id;
   
   ---------------------
   -- Get_Parent_Path --
   ---------------------
   
   function Get_Parent_Path (Path : String) return String is
      
      Pos : Natural;
   begin
      if Path /= "" then
	 Pos := Index
	   (Source  => Path,
	    Pattern => Path_Separator,
	    Going   => Backward);
	 
	 if Pos - 1 > 0 then
	    return Path (Path'First .. Positive (Pos - 1));
	 end if;
      end if;
      
      return "";
   end Get_Parent_Path;
   
   
   -------------
   -- Resolve --
   -------------
   
   function Resolve (Root : access Cell_Record'Class;
                     Path : String) return access Cell_Record'Class 
   is
      Parent  : access Cell_Record'Class := Root;
      Tokens  : Slice_Set;
      Int_Val : Integer;
   begin
      if Path /= "" then
	 Create
	   (S          => Tokens,
	    From       => Path,
	    Separators => Path_Separator,
	    Mode       => Multiple);
         
         for I in 1..Slice_Count (Tokens) loop
            Int_Val := Integer'Value (Slice (Tokens, I));
	    Parent  := Parent.Get_Child_At (Int_Val);
         end loop;
 
	 return Parent;
      end if;
     
      return null;
   end Resolve;
   
   
   -------------
   -- Compare --
   -------------
   
   function Compare
     (Cp1 : String;
      Cp2 : String) return Integer 
   is
      
      Tok1     : Slice_Set;
      Tok2     : Slice_Set;
      Int1_Val : Integer;
      Int2_Val : Integer;
   begin
      if Cp1 /= "" and Cp2 /= "" then
	 Create
	   (S          => ToK1,
	    From       => Cp1,
	    Separators => Path_Separator,
	    Mode       => Multiple);
	 
	 Create
	   (S          => Tok2,
	    From       => Cp2,
	    Separators => Path_Separator,
	    Mode       => Multiple);
	 
	 if Slice_Count (ToK1) = Slice_Count (Tok2) then
	    for I in 1..Slice_Count (Tok1) loop
	       Int1_Val := Integer'Value (Slice (Tok1, I));
	       Int2_Val := Integer'Value (Slice (Tok2, I));
	       
	       if Int1_Val /= Int2_Val then
		  if Int1_Val > Int2_Val then
		     return 1;
		  else
		     return -1;
		  end if;
	       end if;
	    end loop;
	    
	    return 0;
	 end if;	 
	 
	 if Slice_Count (Tok1) > Slice_Count (Tok2) then
	    return 1;
	 else
	    return -1;
	 end if;
      else
	 return 0;
      end if;
   end Compare;
   
end Artics.Graph.Cells_Paths;
