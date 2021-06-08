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

with Ada.Strings.Fixed;
with GNAT.String_Split;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

with Artics.Types; use Artics.Types;
with Artics.Namet; use Artics.Namet;
with Artics.Graph.Cells; use Artics.Graph.Cells;

-- Implements a mechanism for temporary cell Ids.

package Artics.Graph.Cells_Paths is
   
   Path_Separator : String := ".";
   
   function Create (Cell : access Cell_Record'Class) return String;   
   -- Creates the cell path for the given cell. The cell path is a 
   -- concatenation of the indices of all cells on the (finite) path to the
   -- root, eg. "0.0.0.1".
   -- @param cell Cell whose path should be returned.
   -- @return Returns the string that represents the path.
   
   function Get_Path_Id (Cell : access Cell_Record'Class) return String;
   -- Return a chain composed of ids separated by path_separtor. the first id
   -- in the chain is the upper ancestror of cell. The defautl path separator 
   -- is "."
   
   function Get_Parent_Path (Path : String) return String;
   -- Returns the path for the parent of the cell represented by the given path
   -- Returns null if the given path has no parent.
   -- @param path Path whose parent path should be returned.
   
   function Resolve 
     (Root : access Cell_Record'Class;
      Path : String) return access Cell_Record'Class;
   -- Returns the cell for the specified cell path using the given root as the
   -- root of the path.
   -- @param root Root cell of the path to be resolved.
   -- @param path String that defines the path.
   -- @return Returns the cell that is defined by the path.
   
   function Compare
     (Cp1 : String;
      Cp2 : String) return Integer;
   -- Compares the given cell paths and returns -1 if cp1 is smaller, 0 if cp1
   -- is equal and 1 if cp1 is greater than cp2.
   
private
     function Create_Helper (Cell : access Cell_Record'Class) return String;
 
end Artics.Graph.Cells_Paths;
