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

with Artics.Strings_Stocks; use Artics.Strings_Stocks;
with Artics.Generic_Matrices;

with Reflex.Boxes; use Reflex.Boxes;

package Reflex.Ladder.Rungs is

   type Rung_Record is tagged private;
   type Rung_Ptr is access all Rung_Record;
   type Rung_Class_Ptr is access all Rung_Record'Class;
   
   function New_Rung return Rung_Ptr;
   
   function Get_Comment (This : access Rung_Record) return String;
   procedure Set_Comment
     (This : access Rung_Record;
      Com  : String);
   
   function Get_Comment (This : access Rung_Record) return Str_Id;
   procedure Set_Comment
     (This : access Rung_Record;
      Com  : Str_Id);
   
   function Get_Enclosing_Box
     (This : access Rung_Record) return access Box_Record'Class;
   procedure Set_Enclosing_Box
     (This : access Rung_Record;
      B    : access Box_Record'Class);
   
   function Get_Max_Width (This : access Rung_Record) return Natural;
   procedure Set_Max_Width
     (This : access Rung_Record;
      W    : Natural);
   
   function Get_Max_Height (This : access Rung_Record) return Natural;
   procedure Set_Max_Height
     (This : access Rung_Record;
      H    : Natural);
      
   function Get_Width (This : access Rung_Record) return Natural;
   procedure Set_Width
     (This : access Rung_Record;
      W    : Natural);
   
   function Get_Height (This : access Rung_Record) return Natural;
   procedure Set_Height
     (This : access Rung_Record;
      H    : Natural);
   
private
   
   type Rung_Record is tagged record
      Comment : Str_Id;
      Box : access Box_Record'Class;
      
      Max_Width  : Natural;
      Max_Height : Natural;
      
      Width  : Natural;
      Height : Natural;
      
      Matrix : access Matrices.Matrix_Record;
   end record;
			      
   No_Rung_Record : constant Rung_Record :=
     (Comment    => No_Str_Id,
      Box        => null,
      Max_Width  => 16,
      Max_Height => 12,
      Width      => 0,
      Height     => 0,
      Matrix     => null);

end Reflex.Ladder.Rungs;
