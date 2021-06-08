   ------------------
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
-- Reflex is originally developed  by the Artics team at Grenoble.          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation; 

package body Reflex.Boxes.Terminals is
   
   ----------------------
   -- New_Terminal_Box --
   ----------------------
   
   function New_Terminal_Box return Terminal_Box_Ptr is
      This : Terminal_Box_Ptr := 
        new Terminal_Box_Record'(No_Terminal_Box_Record);
   begin
      Set_Box_Kind (This, Terminal_Box);
      return This;
   end New_Terminal_Box;
   
   -----------------------
   -- Free_Terminal_Box --
   -----------------------
   
   procedure Free_Terminal_Box (This : in out Terminal_Box_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Terminal_Box_Record, Terminal_Box_Ptr);
   begin
      if This /= null then
         Free (This);
      end if;
   end Free_Terminal_Box;
   
   -------------
   -- Get_Typ --
   -------------
    
   function Get_Typ (This : access Terminal_Box_Record) return Box_Type is
   begin
      return This.Typ;
   end Get_Typ;
      
   -------------
   -- Set_Typ --
   -------------
   
   procedure Set_Typ
     (This : access Terminal_Box_Record;
      Typ  : Box_Type) is
   begin
      This.Typ := Typ;
   end Set_Typ;
   
   ------------------------
   -- Absolute_Place_Box --
   ------------------------
   
   procedure Absolute_Place_Box (This : access Terminal_Box_Record) is
   begin
      if This.Get_Type = Label_Box then
         This.Set_Xabs (0);
         
      elsif This.Is_Action_Box then 
         This.Set_Xabs (Max_Unity_Ladder_Horizontal - This.Get_Width);
      end if;
   end Absolute_Place_Box;
   
   ------------------
   -- Place_Matrix --
   ------------------
   
   procedure Place_Matrix
     (This   : access Terminal_Box_Record;
      Matrix : access Matrices.Matrix_Record) is
      
      X : Natural := This.Get_Xabs;
      Y : Natural := This.Get_Yabs;
      W : Natural := This.Get_Width;
      H : Natural := This.Get_Height;
   begin
      Put_Line ("Place_Matrix Begin = " & This.Get_Box_Kind'Img);
      Put_Line (" Type = " & This.Get_Typ'Img);
      Put_Line ("  X = " & X'Img);
      Put_Line ("  Y = " & Y'Img);
      Put_Line ("  W = " & W'Img);
      Put_Line ("  H = " & H'Img);
      
      for I in X .. (X + W - 1) loop
         for J in Y .. (Y + H - 1) loop
            Put_Line ("Busy => " & I'Img & "," & J'Img);
            Set_Item (Matrix, I, J, The_Busy_Box);
         end loop;
      end loop;
      
      Put_Line ("Box => " & X'Img & "," & Y'Img);
      Set_Item (Matrix, X, Y, This);
      
      pragma Assert (Get_Item (Matrix, X, Y) = Box_Class_Ptr (This));
      Put_Line ("Place_Matrix End = " & This.Get_Box_Kind'Img);
   end Place_Matrix;
   
   --------------
   -- Dump_Box --
   --------------
   
   procedure Dump_Box (This : access Terminal_Box_Record) is
   begin
      Dump_Box (Box_Ptr (This));
      Put_Line ("Type = " & This.Typ'Img);
   end Dump_Box;
   
end Reflex.Boxes.Terminals;
