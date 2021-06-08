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

with Types;          use Types;

package Reflex.Builder_Interfaces is
   
   type Builder_Interface is Interface;
   type Builder_Interface_Ptr is access all Builder_Interface'Class;
   
   procedure Build_Boxes 
     (This : access Builder_Interface;
      Node : Node_Id) is abstract;
   
   procedure Dump_Box
     (This : access Builder_Interface;
      B    : access Box_Record'Class) is abstract;
   
   procedure Generate_Literal_Expression
     (This : access Builder_Interface;
      Node : Node_Id) is abstract;
   
end Reflex.Builder_Interfaces;
