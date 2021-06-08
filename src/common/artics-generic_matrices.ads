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

with Artics.Dynamic_Arrays;

generic
   type Item_Type is private;
   Null_Item : in Item_Type;
   
package Artics.Generic_Matrices is
   
   type Matrix_Record is tagged private;
   type Matrix_Ptr is access all Matrix_Record;
   type Matrix_Class_Ptr is access all Matrix_Record'Class;

   No_Matrix_Record : constant Matrix_Record;

   -----------------------
   -- Matrix operations --
   -----------------------

   function New_Matrix
     (Columns_Count : in Natural;
      Lines_Count   : in Natural) return Matrix_Ptr;
   -- Create a new matrix with Nb_lines lines and Nb_Coulmns columns.

   procedure Destroy_Matrix (This : in out Matrix_Ptr);
   -- Destroy all the lines and columns of the matrix.

   procedure Append_Line (This : access Matrix_Record);
   -- Add a new line after the last line of the matrix.

   procedure Append_Column (This : access Matrix_Record);
   -- Append a new column after the last column.

   procedure Insert_Line_Before
     (This : access Matrix_Record;
      Line : Natural);
   
   procedure Insert_Column_Before
     (This   : access Matrix_Record;
      Column : Natural);
   
   procedure Insert_Line_After
     (This : access Matrix_Record;
      Line : Natural);
   
   procedure Insert_Column_After
     (This   : access Matrix_Record;
      Column : Natural);
   
   procedure Remove_Line
     (This : access Matrix_Record;
      Line : Natural);
   -- Append a new column after the last column.

   procedure Remove_Column
     (This   : access Matrix_Record;
      Column : Natural);
   -- Append a new column after the last column.

   function Get_Lines_Count (This : access Matrix_Record) return Natural;
   -- Return the number of lines in the matrix.

   function Get_Columns_Count (This : access Matrix_Record) return Natural;
   -- Return the number of columns in the matrix.

   function Get_Item
     (This : access Matrix_Record;
      X    : Natural;
      Y    : Natural) return Item_Type;
   -- Return the item placed at position X,Y of the matrix.

   procedure Set_Item
     (This : access Matrix_Record;
      X    : Natural;
      Y    : Natural;
      Item : Item_Type);
   -- Sets the item placed at position X,Y of the matrix.

private
   -- A Matrix consists of lines x columns cells, each containing an item.
   
   package Matrix_Columns is 
      new Artics.Dynamic_Arrays (Item_Type, Null_Item);
   use Matrix_Columns;

   package Matrix_Lines is 
      new Artics.Dynamic_Arrays (Matrix_Columns.Dynamic_Array_Ptr, null);
   use Matrix_Lines;
   
   --  Lines correspond to Y coordinate and Column are X coordinate

   type Matrix_Record is tagged record
      Lines : Matrix_Lines.Dynamic_Array_Ptr;
      
      Lines_Count : Natural;
      --Number of lines in the matrix.

      Columns_Count : Natural;
      -- Number of columns in the matrix.
   end record;
   
   No_Matrix_Record : constant Matrix_Record :=
     (Lines         => null,
      Lines_Count   => 0,
      Columns_Count => 0);
end Artics.Generic_Matrices;
