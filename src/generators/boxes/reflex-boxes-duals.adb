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

with Reflex.Boxes.Utils; use Reflex.Boxes.Utils;

package body Reflex.Boxes.Duals is
   
   ------------------
   -- New_Dual_Box --
   ------------------
   
   function New_Dual_Box return Dual_Box_Ptr is
      This : Dual_Box_Ptr := new Dual_Box_Record'(No_Dual_Box_Record);
   begin
      Set_Box_Kind (This, Dual_Box);
      return This;
   end New_Dual_Box;
   
   -------------------
   -- Free_Dual_Box --
   -------------------
   
   procedure Free_Dual_Box (This : in out Dual_Box_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Dual_Box_Record, Dual_Box_Ptr);
   begin
      if This /= null then
         Free (This);
      end if;
   end Free_Dual_Box;
   
   --------------
   -- Get_Box1 --
   --------------
   
   function Get_Box1
     (This : access Dual_Box_Record) return access Box_Record'Class is
   begin
      return This.Box1;
   end Get_Box1;
   
   --------------
   -- Set_Box1 --
   --------------
   
   procedure Set_Box1
     (This : access Dual_Box_Record;
      Box  : access Box_Record'Class) is
   begin
      This.Box1 := Box;
   end Set_Box1;
   
   --------------
   -- Get_Box2 --
   --------------
   
   function Get_Box2
     (This : access Dual_Box_Record) return access Box_Record'Class is
   begin
      return This.Box2;
   end Get_Box2;
   
   --------------
   -- Set_Box2 --
   --------------
   
   procedure Set_Box2
     (This : access Dual_Box_Record;
      Box  : access Box_Record'Class) is
   begin
      This.Box2 := Box;
   end Set_Box2;
   
   ------------------------
   -- Absolute_Place_Box --
   ------------------------
   
   procedure Absolute_Place_Box
     (This : access Dual_Box_Record) is
      
      X    : Natural;
      Y    : Natural;
      B1   : access Box_Record'Class;
      B2   : access Box_Record'Class;
      W1   : Natural;
      W2   : Natural;
      Wmax : Natural;
      B1_Kind : Box_Kind;
      B2_Kind : Box_Kind;
   begin
      --  Box B1
     
      X := This.Get_Xabs;
      Y := This.Get_Yabs;
      
      B1 := This.Get_Box1;
      B2 := This.Get_Box2;

      B1.Set_Xabs (X + B1.Get_Xabs);
      B1.Set_Yabs (This.Get_Yabs + B1.Get_Yabs);
      
      B1.Absolute_Place_Box;
      
      --  Box B2

      if B2.Get_Is_Action_Box then
         Set_Xabs (B2, Max_Unity_Ladder_Horizontal - B2.Get_Width + 1);
	 
      elsif This.Get_Orientation = Horizontal then
         Set_Xabs (B2, Get_Xabs (B1) + Get_Width (B1));
      else      
         Set_Xabs (B2, Get_Xabs (B1));
      end if;  
      
      -- Y Placement
      
      if This.Get_Orientation = Horizontal then
         Set_Yabs (B2, Get_Yabs (B1));
      else      
         Set_Yabs (B2, Get_Yabs (B1) + Get_Height (B1));
      end if;  
      
      B2.Absolute_Place_Box;
      
      --  Dual box Width computation
      
      if This.Get_Orientation = Horizontal then
         This.Set_Width
           (B2.Get_Xabs + B2.Get_Width - B1.Get_Xabs);
	 
      else
         W1 := B1.Get_Width;	
         W2 := B2.Get_Width;
         Wmax := Max (W1, W2);
	 
         B1_Kind := B1.Get_Box_Kind;
         if B1_Kind = Dual_Box
           or else B1_Kind = Multi_Box
         then
            B1.Set_Width (Wmax);
         end if;
	 
         B2_Kind := B2.Get_Box_Kind;
         if B2_Kind = Dual_Box
           or else B2_Kind = Multi_Box
         then
            B2.Set_Width (Wmax);
         end if;
	 
         This.Set_Width (Wmax);
      end if;
	    
      --  Dual box Height computation
      
      if This.Get_Orientation = Horizontal then
         This.Set_Height (Max (B1.Get_Height, B2.Get_Height));
      else
         This.Set_Height (B1.Get_Height + B2.Get_Height);
      end if;
   end Absolute_Place_Box;
   
   ------------------
   -- Place_Matrix --
   ------------------
   
   procedure Place_Matrix 
     (This   : access Dual_Box_Record;
      Matrix : access Matrices.Matrix_Record) is
      
      use Matrices;
      
      B1 : access Box_Record'Class;
      B2 : access Box_Record'Class;
      Direction : Orientation_Type;
      
      X1 : Natural;
      Y1 : Natural;
      W1 : Natural;
      H1 : Natural;
      
      X2 : Natural;
      Y2 : Natural;
      W2 : Natural;
      H2 : Natural;
      
      W  : Natural;
      X  : Natural;
      Y  : Natural;
      
      Count : Natural;
      Ynxt : Natural;
   begin
      Put_Line ("Place_Matrix Begin Dual");
      Direction := This.Get_Orientation;
      
      B1 := This.Get_Box1;
      B2 := This.Get_Box2;
      
      -- Place Box B1
	
      B1.Place_Matrix (Matrix);
      
      --  Place Box B2
      
      B2.Place_Matrix (Matrix);
      
      --  Place Hlink betwenn the two boxes if needed
      
      X1 := B1.Get_Xabs;
      Y1 := B1.Get_Yabs;
      W1 := B1.Get_Width;
      H1 := B1.Get_Height;

      X2 := B2.Get_Xabs;
      Y2 := B2.Get_Yabs;
      W2 := B2.Get_Width;
      H2 := B2.Get_Height;
      
      W := This.Get_Width;
      
      X := This.Get_Xabs;
      Y := This.Get_Yabs;
      
      --  Case Horizontal
      
      if Direction = Horizontal then
	 
         for I in (X1 + W1) .. (X2 - 1) loop
            
            if Get_Item (Matrix, I, Y1) = null then
               Set_Item (Matrix, I, Y1, The_Hlink_Box);
               
            elsif Get_Item (Matrix, I, Y1) = The_Vlink_Box then
               Set_Item (Matrix, I, Y1, The_Hlink_Vlink_Box);
               
            end if;
         end loop;
	 
         for I in (X2 + W2) .. (X + W - 1) loop
            if Get_Item (Matrix, I, Y1) = null then
               Set_Item (Matrix, I, Y1, The_Hlink_Box);
               
            elsif Get_Item (Matrix, I, Y1) = The_Vlink_Box then
               Set_Item (Matrix, I, Y1, The_Hlink_Vlink_Box);
               
            end if;
         end loop;
	 
         if B2.Get_Box_Kind = Dual_Box
           and then B2.Get_Orientation = Vertical 
         then
            Count := B2.Get_Height - 1;
            Ynxt := 0;
            for I in 1..Count loop
               Add_Matrix_Vlink (Matrix, X1 + W1 - 1 , Y1 + Ynxt);
               Ynxt := Ynxt + 1;
            end loop;
         end if;
	 
         --  Case Vertical
        
      else
         --  Place les Hlink
	 
         for I in (X1 + W1) .. (X + W - 1) loop
            
            if Get_Item (Matrix, I, Y1) = null then
               Set_Item (Matrix, I, Y1, The_Hlink_Box);
               
            elsif Get_Item (Matrix, I, Y1) = The_Vlink_Box then
               Set_Item (Matrix, I, Y1, The_Hlink_Vlink_Box);
               
            end if;
         end loop;
	 
         for I in (X2 + W2) .. (X + W - 1) loop
            
            if Get_Item (Matrix, I, Y2) = null then
               Set_Item (Matrix, I, Y2, The_Hlink_Box);
               
            elsif Get_Item (Matrix, I, Y2) = The_Vlink_Box then
               Set_Item (Matrix, I, Y2, The_Hlink_Vlink_Box);
               
            end if;
         end loop;
	 
         --  Place les Vlink 
	 
         Count := B2.Get_Yabs - B1.Get_Yabs; 
         Ynxt := 0;
         for I in 1..Count loop
            Put_Line ("je suis passe");
            Add_Matrix_Vlink (Matrix, X + W - 1 , Y1 + Ynxt);
            Ynxt := Ynxt + 1;
         end loop;
      end if;
      
      Put_Line ("Place_Matrix End Dual");
   end Place_Matrix;
   
   --------------
   -- Dump_Box --
   --------------
   
   procedure Dump_Box (This : access Dual_Box_Record) is
   begin
      Dump_Box (Box_Ptr (This));
      Put_Line ("Box 1 =====> ");
      This.Get_Box1.Dump_Box;
      
      Put_Line ("Box 2 =====> ");
      This.Get_Box2.Dump_Box;
   end Dump_Box;
   
end Reflex.Boxes.Duals;
