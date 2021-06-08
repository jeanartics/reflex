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

with Artics.Types; use Artics.Types;
with Artics.Graph.Cells; use Artics.Graph.Cells;
with Artics.Graph.Models_Interfaces; use Artics.Graph.Models_Interfaces;

package Artics.Graph.Models_Changes.Childs is
   
   type Child_Change_Record is new Model_Change_Record with private;
   type Child_Change_Ptr is access all Child_Change_Record;
   type Child_Change_Class_Ptr is access all Child_Change_Record'Class;

   No_Child_Change_Record : constant Child_Change_Record;
   
   function New_Child_Change return Child_Change_Ptr;
   
   function New_Child_Change
     (Model  : access Model_Interface'Class;
      Parent : access Cell_Record'Class;
      Child  : access Cell_Record'Class) return Child_Change_Ptr;
   
   function New_Child_Change
     (Model  : access Model_Interface'Class;
      Parent : access Cell_Record'Class;
      Child  : access Cell_Record'Class;
      Index  : Integer) return Child_Change_Ptr;
   
   function Get_Parent
     (C : access  Child_Change_Record) return access Cell_Record'Class;
   procedure Set_Parent
     (C     : access Child_Change_Record;
      Value : access Cell_Record'Class);
   -- @return the root
		   
   function Get_Previous
     (C : access Child_Change_Record) return access Cell_Record'Class;
   procedure Set_Previous
     (C     : access Child_Change_Record;
      Value : access Cell_Record'Class);
   -- @return the previous
   
   function Get_Child
     (C : access Child_Change_Record) return access Cell_Record'Class;
   procedure Set_Child
     (C     : access Child_Change_Record;
      Value : access Cell_Record'Class);
   
   function Get_Index
     (C : access Child_Change_Record) return Integer;
   procedure Set_Index
     (C     : access Child_Change_Record;
      Value : Integer);
   -- @return the previous
   
   function Get_Previous_Index
     (C : access Child_Change_Record) return Integer;
   procedure Set_Previous_Index
     (C     : access Child_Change_Record;
      Value : Integer);
   -- @return the previousIndex
   
   function Get_Terminal
     (C      : access Child_Change_Record;
      Edge   : access Cell_Record'Class;
      Source : Boolean) return access Cell_Record'Class;
   -- Gets the source or target terminal field for the given edge even if the
   -- edge is not stored as an incoming or outgoing edge in the respective 
   -- terminal.

   procedure Set_Terminal
     (C        : access Child_Change_Record;
      Edge     : access Cell_Record'Class;
      Terminal : access Cell_Record'Class;
      Source   : Boolean);
   -- Sets the source or target terminal field for the given edge without 
   -- inserting an incoming or outgoing edge in the respective terminal.
   
   procedure Connect
     (C          : access Child_Change_Record;
      Cell       : access Cell_Record'Class;
      Is_Connect : Boolean);
   
   function Get_Child_Index
     (C      : access Child_Change_Record;
      Parent : access Cell_Record'Class;
      Child  : access Cell_Record'Class) return Integer;
   -- Returns the index of the given child inside the given parent.
   
   procedure Execute (C : access Child_Change_Record);
   -- Changes the root of the model.
   
private

   type Child_Change_Record is new Model_Change_Record with record
      Parent         : access Cell_Record'Class;
      Previous       : access Cell_Record'Class;
      Child          : access Cell_Record'Class;
      Index          : Integer;
      Previous_Index : Integer;
      -- Holds the new and previous root cell.
   end record;
   
   No_Child_Change_Record : constant Child_Change_Record := Child_Change_Record'
     (No_Model_Change_Record with
      Parent         => null,
      Previous       => null,
      Child          => null,
      Index          => 0,
      Previous_Index => 0);
   
end Artics.Graph.Models_Changes.Childs;
