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

package body Reflex.Boxes.Multis is
   
   -------------------
   -- New_Multi_Box --
   -------------------
   
   function New_Multi_Box return Multi_Box_Ptr is
      This : Multi_Box_Ptr := 
        new Multi_Box_Record'(No_Multi_Box_Record);
   begin
      Set_Box_Kind (This, Multi_Box);
      return This;
   end New_Multi_Box;
   
   --------------------
   -- Free_Multi_Box --
   --------------------
   
   procedure Free_Multi_Box (This : in out Multi_Box_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Multi_Box_Record, Multi_Box_Ptr);
   begin
      if This /= null then
         Free (This);
      end if;
   end Free_Multi_Box;
   
   ------------------
   -- Get_Children --
   ------------------
   
   function Get_Children 
     (This : access Multi_Box_Record) return Boxes_Lists.List is
   begin
      return This.Childs;
   end Get_Children;
   
   ------------------
   -- Set_Children --
   ------------------
   
   procedure Set_Children
     (This   : access Multi_Box_Record;
      Childs : Boxes_Lists.List) is
   begin
      This.Childs := Childs;
   end Set_Children;
   
   ----------------------
   -- Append_Child_Box --
   ----------------------
   
   procedure Append_Child_Box 
     (This : access Multi_Box_Record;
      Box  : access Box_Record'Class) is
   begin
      Boxes_Lists.Append (This.Childs, Box_Class_Ptr (Box));
   end Append_Child_Box;
  
   ------------------------
   -- Absolute_Place_Box --
   ------------------------
   
   procedure Absolute_Place_Box (This : access Multi_Box_Record) is
      
      X     : Natural;
      Y     : Natural;
      W     : Natural;
      H     : Natural;
      Min_X : Natural;
      
      use Boxes_Lists;
   begin
      if Boxes_Lists.Is_Empty (This.Childs) then
         return;
      end if;
      
      X := This.Get_Xabs;
      Y := THis.Get_Yabs;
      W := 0;
      H := 0;
      
      Min_X := Max_Unity_Ladder_Horizontal;
      
      for B of This.Childs loop
         B.Set_Xabs (X);
         B.Set_Yabs (Y + H);
	 	    
         B.Absolute_Place_Box;
	 
         Min_X := Min (Min_X, B.Get_XAbs);
	 
         H := H + B.Get_Height;
         W := Max (W, B.Get_Width);
      end loop;
      
      This.Set_Xabs (Min_X);
      This.Set_Width  (W);
      This.Set_Height (H);
   end Absolute_Place_Box;
     
   ------------------
   -- Place_matrix --
   ------------------
   
   procedure Place_Matrix
     (This   : access Multi_Box_Record;
      Matrix : access Matrices.Matrix_Record) is
      use Boxes_Lists;
      
      X : Natural;
      Y : Natural;
   begin
      Put_Line ("Place_Matrix Begin Multi");
      if Boxes_Lists.Is_Empty (This.Childs) then
         return;
      end if;
      
      X := This.Get_Xabs;
      Y := This.Get_Yabs;
      
      for B of This.Childs loop
         B.Place_Matrix (Matrix);
	 
         for I in X .. (B.Get_Xabs - 1) loop
            Set_Item (Matrix, I, B.Get_Yabs, The_Hlink_Box);
            Put_Line (" J'ai mis un HLINK en " & I'Img & "," & B.Get_Yabs'Img);
         end loop;
      end loop;
      
      -- Place VLink
      
      if Y /= 0 and X /= 0 then
         for I in Y + 1 .. Y + (This.Get_Height - 1) loop
            if Length (This.Get_Children) > 1 then
               Add_Matrix_Vlink (Matrix, X - 1, I - 1);
               Put_Line (" J'ai mis un VLINK en " & X'Img & "," & I'Img);
            end if;
         end loop;
      end if;

      if Y = 0 then
         for I in Y .. (This.Get_Height - 1) loop
            if Length (This.Get_Children) > 1 then
               if I /= 0 and X /= 0 then
                  Add_Matrix_Vlink (Matrix, X - 1, I - 1);
               end if;
            end if;
         end loop;
      end if;
      Put_Line ("Place_Matrix End Multi");
   end Place_Matrix;
   
   --------------
   -- Dump_Box --
   --------------
   
   procedure Dump_Box (This : access Multi_Box_Record) is
      Count : Natural := 0;
   begin
      Dump_Box (Box_Ptr (This));
      for B of This.Childs loop
         Put_Line ("Box n: " & Count'Img & " ===> ");
         B.Dump_Box;
         Count := Count + 1;
      end loop;
   end Dump_Box;
   
end Reflex.Boxes.Multis;
