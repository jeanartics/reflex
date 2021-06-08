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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Artics.Lists_Helpers;

with Artics.Types; use Artics.Types;
with Artics.Objects; use Artics.Objects;
with Artics.Graph.Undoables.Changes_Interfaces; 
use Artics.Graph.Undoables.Changes_Interfaces;

-- Implements a 2-dimensional rectangle with double precision coordinates.

package Artics.Graph.Undoables.Edits is
   
   type Undoable_Edit_Record is new Object_Record with private;
   type Undoable_Edit_Ptr is access all Undoable_Edit_Record;
   type Undoable_Edit_Class_Ptr is access all Undoable_Edit_Record'Class;
   
   No_Undoable_Edit_Record : constant Undoable_Edit_Record;
   
   package Undoable_Edits_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Undoable_Edit_Class_Ptr);
   use Undoable_Edits_Lists;
   
   package Undoable_Edit_Lists_Helpers is new Artics.Lists_Helpers 
     (Element_Type => Undoable_Edit_Class_Ptr,
      No_Element   => null,
      Lists        => Undoable_Edits_Lists);
   
   function New_Undoable_Edit
     (Source : access Object_Record'Class) return Undoable_Edit_Ptr;
   -- Constructs a new undoable edit for the given source.
   
   function New_Undoable_Edit
     (Source      : access Object_Record'Class;
      Significant : Boolean) return Undoable_Edit_Ptr;
   -- Constructs a new undoable edit for the given source.
   
   procedure Dispatch (E : access Undoable_Edit_Record);
   -- Hook to notify any listeners of the changes after an undo or redo
   -- has been carried out. This implementation is empty.
   
   procedure Die (E : access Undoable_Edit_Record);
   -- Hook to free resources after the edit has been removed from the command
   -- history. This implementation is empty.
   
   function Get_Source
     (E : access Undoable_Edit_Record) return access Object_Record'Class;
   -- @return the source
   
   function Get_Changes
     (E : access Undoable_Edit_Record) return Undoables_Lists.List;
   -- @return the changes
   
   function Is_Significant (E : access Undoable_Edit_Record) return Boolean;
   -- @return the significant
   
   function Is_Undone (E : access Undoable_Edit_Record) return Boolean;
   -- @return the undone
   
   function Is_Redone (E : access Undoable_Edit_Record) return Boolean;
   -- @return the redone
   
   function Is_Empty (E : access Undoable_Edit_Record) return Boolean;
   -- Returns true if the this edit contains no changes.
   
   procedure Add 
     (E      : access Undoable_Edit_Record;
      Change : access Undoable_Change_Interface'Class);
   -- Adds the specified change to this edit. The change is an object that is
   -- expected to either have an undo and redo, or an execute function.
   
   procedure Undo (E : access Undoable_Edit_Record);
   
   procedure Redo (E : access Undoable_Edit_Record);
   
private
   
   type Undoable_Edit_Record is new Object_Record with record
      Source : access Object_Record'Class;
      -- Holds the source of the undoable edit.
      
      Changes : Undoables_Lists.List;
      -- Holds the list of changes that make up this undoable edit.

      Significant : Boolean := True;
      -- Specifies this undoable edit is significant. Default is true.

      Undone : Boolean;
      Redone : Boolean;
      -- Specifies the state of the undoable edit.
   end record;
   
   No_Undoable_Edit_Record : constant Undoable_Edit_Record := 
     Undoable_Edit_Record'
       (No_Object_Record with
        Source      => null,
        Changes     => Undoables_Lists.Empty_List,
        Significant => True,
        Undone      => False,
        Redone      => False);
   
end Artics.Graph.Undoables.Edits;
			       
			       
			       
