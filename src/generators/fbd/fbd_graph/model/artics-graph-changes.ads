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
with Artics.Objects; use Artics.Objects;

with Artics.Graph.Undoables.Changes_Interfaces; use Artics.Graph.Undoables.Changes_Interfaces;

package Artics.Graph.Changes is
   
   type Change_Record is
     new Object_Record and Undoable_Change_Interface with private;
   type Change_Ptr is access all Change_Record;
   type Change_Class_Ptr is access all Change_Record'Class;

   No_Change_Record : constant Change_Record;
   
   function Get_Change_Type
     (This : access Change_Record) return Change_Type;
   procedure Set_Change_Type
     (This : access Change_Record;
      Typ  : Change_Type);
   
   procedure Execute (C : access Change_Record) is null;
   
private

   type Change_Record is 
     new Object_Record and Undoable_Change_Interface with record
	CType : Change_Type;
   end record;
   
   No_Change_Record : constant Change_Record :=
     (No_Object_Record with Ctype => No_Change);
      
end Artics.Graph.Changes;
