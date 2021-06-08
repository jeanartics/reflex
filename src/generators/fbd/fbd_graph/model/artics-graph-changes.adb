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

package body Artics.Graph.Changes is
   
   ---------------------
   -- Get_Change_Type --
   ---------------------
   
   function Get_Change_Type
     (This : access Change_Record) return Change_Type is
   begin
      return This.Ctype;
   end Get_Change_Type;
   
   ---------------------
   -- Set_Change_Type --
   ---------------------
   
   procedure Set_Change_Type
     (This : access Change_Record;
      Typ  : Change_Type) is
   begin
      This.Ctype := Typ;
   end Set_Change_Type;
   
end Artics.Graph.Changes;
