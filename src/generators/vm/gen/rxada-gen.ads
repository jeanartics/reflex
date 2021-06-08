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

with Artics.Buffers; use Artics.Buffers;

with Types; use Types;
with Reflex.Generators; use Reflex.Generators;

package Rxada.Gen is

   type Ada_Generator_Record is new Generator_Record with private;
   type Ada_Generator_Ptr is access all Ada_Generator_Record;
   type Ada_Generator_Class_Ptr is access all Ada_Generator_Record'Class;

   No_Ada_Generator_Record : constant Ada_Generator_Record;

   function New_Ada_Generator return Ada_Generator_Ptr;

   procedure Free_Ada_Generator (This : in out Ada_Generator_Ptr);

   function Get_Output_Buffer
     (This : access Ada_Generator_Record) return Output_Buffer;
   procedure Set_Output_Buffer
     (This : access Ada_Generator_Record;
      Ob   : Output_Buffer);
   
   procedure Do_Generation (This : access Ada_Generator_Record);
   
   procedure Generate_Node
     (This        : access Ada_Generator_Record;
      Node        : Node_Id; 
      Declaration : Boolean := False);
   
   procedure Generate_Literal_Expression
     (This : access Ada_Generator_Record;
      Node : Node_Id;
      Ob   : Output_Buffer);
   
private

   type Ada_Generator_Record is new Generator_Record with record
      Current_Entity : Entity_Id;

      Ob : Output_Buffer;
   end record;

   No_Ada_Generator_Record : constant Ada_Generator_Record :=
     Ada_Generator_Record'
     (No_Generator_Record with
        Current_Entity => Empty,
      Ob          => null);

end Rxada.Gen;
