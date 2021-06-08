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

package body Reflex.Ladder.Networks is

   -----------------
   -- New_Network --
   -----------------
   
   function New_Network return Network_Ptr is
      This : Network_Ptr := new Network_Record'(No_Network_Record);
   begin
      return This;
   end New_Network;
   
   ---------------------
   -- Get_Rungs_Lists --
   ---------------------
   
   function Get_Rungs_Lists
     (This : access Network_Record) return Rungs_Lists.List is
   begin
      return This.Rungs;
   end Get_Rungs_Lists;
   
   ---------------------
   -- Set_Rungs_Lists --
   ---------------------
   
   procedure Set_Rungs_Lists
     (This : access Network_Record;
      L    : Rungs_Lists.List) is
   begin
      This.Rungs := L;
   end Set_Rungs_Lists;
   
   ---------------------
   -- Get_Page_Height --
   ---------------------
   
   function Get_Page_Height (This : access Network_Record) return Natural is
   begin
      return This.Page_Height;
   end Get_Page_Height;
   
   ---------------------
   -- Set_Page_Height --
   ---------------------
   
   procedure Set_Page_Height
     (This : access Network_Record;
      H    : Natural) is
   begin
      This.Page_Height := H;
   end Set_Page_Height;
   
end Reflex.Ladder.Networks;
