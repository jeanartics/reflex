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

with Artics.Graph.Cells; use Artics.Graph.Cells;

package Artics.Graph.Cells_Visitor_Interfaces is
   
   type Cell_Visitor_Interface is Interface;
   type Cell_Visitor_Interface_Ptr is access all Cell_Visitor_Interface'Class;
   
   
   function Visit
     (Vis    : access Cell_Visitor_Interface;
      Vertex : access Cell_Record'Class;
      Edge   : access Cell_Record'Class) return Boolean is abstract;

end Artics.Graph.Cells_Visitor_Interfaces;
