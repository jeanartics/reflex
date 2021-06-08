------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 2016, Free Software Foundation, Inc.              --
--                                                                          --
-- Reflex is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware Foundation; either version 3, or (at your option) any later version --
-- Reflex is distributed in the hope that it will be u, but WITH-      --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License distributed with Reflex; see file COPYING3. If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- Reflex is originally developed by Artics                                 --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body Artics.Matrices is

   ----------------
   -- New_Matrix --
   ----------------
   
   function New_Matrix
     (Columns_Count : in Natural;
      Lines_Count   : in Natural) return Matrix_Ptr is
      
      This : Matrix_Ptr := new Matrix_Record'(No_Matrix_Record);
   begin
      This.Lines := Matrix_Lines.New_Dynamic_Array (Lines_Count);
      
      for I in 0..Lines_Count - 1 loop
	 This.Lines (I) := Matrix_Columns.New_Dynamic_Array (Columns_Count);
      end loop;
      
      This.Lines_Count := Lines_Count;
      This.Columns_Count := Columns_Count;
      
      return This;
   end New_Matrix;
   
   --------------------
   -- Destroy_Matrix --
   --------------------
   
   procedure Destroy_Matrix (This : in out Matrix_Ptr) is
      procedure Free is
	 new Ada.Unchecked_Deallocation (Matrix_Record, Matrix_Ptr);
   begin
      if This.Lines /= null then
	 for I in 0..This.Lines_Count - 1 loop
	    if This.Lines (I) /= null then
	       Matrix_Columns.Free_Array (This.Lines (I));
	    end if;
	 end loop;
      
	 Matrix_Lines.Free_Array (This.Lines);
      end if;
      
      Free (This);
   end Destroy_Matrix;
   
   -----------------
   -- Append_Line --
   -----------------
   
   procedure Append_Line (This : access Matrix_Record) is
   begin
      This.Lines_Count := This.Lines_Count + 1;
      Resize (This.Lines, This.Lines_Count);
      
      This.Lines (This.Lines_Count - 1) := 
	Matrix_Columns.New_Dynamic_Array (This.Columns_Count);
   end Append_Line;
   
   -------------------
   -- Append_Column --
   -------------------
   
   procedure Append_Column (This : access Matrix_Record) is
   begin
      This.Columns_Count := This.Columns_Count + 1;
      for I in 0..This.Lines_Count - 1 loop
	 Matrix_Columns.Resize (This.Lines (I), This.Columns_Count);
      end loop;
   end Append_Column;
   
   ------------------------
   -- Insert_Line_Before --
   ------------------------
   
   procedure Insert_Line_Before
     (This : access Matrix_Record;
      Line : Natural) is
   begin
      Matrix_Lines.Resize (This.Lines, This.Lines_Count + 1);
      Matrix_Lines.Copy
	(This.Lines, Line, This.Lines, Line + 1, This.Lines_Count - Line);
      
      This.Lines_Count := This.Lines_Count + 1;
   end Insert_Line_Before;
   
   --------------------------
   -- Insert_Column_Before --
   --------------------------
   
   procedure Insert_Column_Before
     (This   : access Matrix_Record;
      Column : Natural) is
   begin
      null;
   end Insert_Column_Before;
   
   -----------------------
   -- Insert_Line_After --
   -----------------------
   
   procedure Insert_Line_After
     (This : access Matrix_Record;
      Line : Natural) is
   begin
      null;
   end Insert_Line_After;
   
   -------------------------
   -- Insert_Column_After --
   -------------------------
   
   procedure Insert_Column_After
     (This   : access Matrix_Record;
      Column : Natural) is
   begin
      null;
   end Insert_Column_After;
   
   -----------------
   -- Remove_Line --
   -----------------
   
   procedure Remove_Line
     (This : access Matrix_Record;
      Line : Natural) is
   begin
      null;
   end Remove_Line;
   
   -------------------
   -- Remove_Column --
   -------------------
   
   procedure Remove_Column
     (This   : access Matrix_Record;
      Column : Natural) is
   begin
      null;
   end Remove_Column;
   
   -----------------
   -- Lines_Count --
   -----------------
   
   function Get_Lines_Count (This : access Matrix_Record) return Natural is
   begin
      return This.Lines_Count;
   end Get_Lines_Count;
   
   -------------------
   -- Columns_Count --
   -------------------
   
   function Get_Columns_Count (This : access Matrix_Record) return Natural is
   begin
      return This.Columns_Count;
   end Get_Columns_Count;
   
   --------------
   -- Get_Item --
   --------------
   
   function Get_Item
     (This : access Matrix_Record;
      X    : Natural;
      Y    : Natural) return Item_Type is
      
      Line : Matrix_Columns.Dynamic_Array_Ptr;
   begin
      Line := This.Lines (X);
      return Line (Y);
   end Get_Item;
   
   --------------
   -- Set_Item --
   --------------
   
   procedure Set_Item
     (This : access Matrix_Record;
      X    : Natural;
      Y    : Natural;
      Item : Item_Type) is
      
      Line : Matrix_Columns.Dynamic_Array_Ptr;
   begin
      Line := This.Lines (X);
      Line (Y) := Item;
   end Set_Item;

end Artics.Matrices;
