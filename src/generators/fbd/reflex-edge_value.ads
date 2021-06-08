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

with Artics.Objects;     use Artics.Objects;

package Reflex.Edge_Value is
   
   type Edge_Value_Record is new Object_Record with private;
   type Edge_Value_Ptr is access all Edge_Value_Record;
   type Edge_Value_Class_Ptr is access all Edge_Value_Record'Class;

   No_Edge_Value_Record : constant Edge_Value_Record;
   
   function New_Edge_Value return Edge_Value_Ptr;
   procedure Free_Edge_Value (This : in out Edge_Value_Ptr);
   
private
   
   type Edge_Value_Record is new Object_Record with null record;
        
   No_Edge_Value_Record : constant Edge_Value_Record := Edge_Value_Record'
     (No_Object_Record with
      null record);

end Reflex.Edge_Value;
