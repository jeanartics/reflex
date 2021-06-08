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

package body Reflex.Ladder.Rungs is

   --------------
   -- New_Rung --
   --------------
   
   function New_Rung return Rung_Ptr is
      This : Rung_Ptr := new Rung_Record'(No_Rung_Record);
   begin
      return This;
   end New_Rung;
   
   -----------------
   -- Get_Comment --
   -----------------
   
   function Get_Comment (This : access Rung_Record) return String is
   begin
      return Get_String (This.Comment);
   end Get_Comment;
   
   -----------------
   -- Set_Comment --
   -----------------
   
   procedure Set_Comment
     (This : access Rung_Record;
      Com  : String) is
   begin
      This.Comment := Enter_String (Com);
   end Set_Comment;
   
   -----------------
   -- Get_Comment --
   -----------------
   
   function Get_Comment (This : access Rung_Record) return Str_Id is
   begin
      return This.Comment;
   end Get_Comment;
   
   -----------------
   -- Set_Comment --
   -----------------
   
   procedure Set_Comment
     (This : access Rung_Record;
      Com  : Str_Id) is
   begin
      This.Comment := Com;
   end Set_Comment;
   
   -----------------------
   -- Get_Enclosing_Box --
   -----------------------
   
   function Get_Enclosing_Box
     (This : access Rung_Record) return access Box_Record'Class is
   begin
      return This.Box;
   end Get_Enclosing_Box;
   
   -----------------------
   -- Set_Enclosing_Box --
   -----------------------
   
   procedure Set_Enclosing_Box
     (This : access Rung_Record;
      B    : access Box_Record'Class) is
   begin
      This.Box := B;
   end Set_Enclosing_Box;
   
   -------------------
   -- Get_Max_Width --
   -------------------
   
   function Get_Max_Width (This : access Rung_Record) return Natural is
   begin
      return This.Max_Width;
   end Get_Max_Width;
   
   -------------------
   -- Set_Max_Width --
   -------------------
   
   procedure Set_Max_Width
     (This : access Rung_Record;
      W    : Natural) is
   begin
      This.Max_Width := W;
   end Set_Max_Width;
   
   --------------------
   -- Get_Max_Height --
   --------------------
   
   function Get_Max_Height (This : access Rung_Record) return Natural is
   begin
      return This.Max_Height;
   end Get_Max_Height;
   
   --------------------
   -- Set_Max_Height --
   --------------------
   
   procedure Set_Max_Height
     (This : access Rung_Record;
      H    : Natural) is
   begin
      This.Max_Height := H;
   end Set_Max_Height;
   
   ---------------
   -- Get_Width --
   ---------------
   
   function Get_Width (This : access Rung_Record) return Natural is
   begin
      return This.Width;
   end Get_Width;
   
   ---------------
   -- Set_Width --
   ---------------
   
   procedure Set_Width
     (This : access Rung_Record;
      W    : Natural) is
   begin
      This.Width := W;
   end Set_Width;
   
   ----------------
   -- Get_Height --
   ----------------
   
   function Get_Height (This : access Rung_Record) return Natural is
   begin
      return This.Height;
   end Get_Height;
   
   ----------------
   -- Set_Height --
   ----------------
   
   procedure Set_Height
     (This : access Rung_Record;
      H    : Natural) is
   begin
      This.Height := H;
   end Set_Height;
   
end Reflex.Ladder.Rungs;
