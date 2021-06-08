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

package Unity.Gen.Ladder_Generator is


   type Ladder_Generator_Record is tagged private;
   type Ladder_Generator_Ptr is access all Ladder_Generator_Record;
   type Ladder_Generator_Class_Ptr is access all Ladder_Generator_Record'Class;

   No_Ladder_Generator_Record : constant Ladder_Generator_Record;

   function New_Unity_Ladder return Ladder_Generator_Ptr;

   procedure Free_Ladder_Generator (This : in out Ladder_Generator_Ptr);

   function Get_Ladder_Output_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer;
   procedure Set_Ladder_Output_Buffer
     (This : access Unity_Generator_Record;
      Ob   : Output_Buffer);

   function Get_Tasks_Output_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer;
   procedure Set_Tasks_Output_Buffer
     (This : access Unity_Generator_Record;
      Ob   : Output_Buffer);

   function Get_Current_Entity
     (This : access Unity_Generator_Record) return Entity_Id;
   procedure Set_Current_Entity
     (This : access Unity_Generator_Record;
      E    : Entity_Id);

   procedure Do_Generation (This : access Unity_Generator_Record);

private

   type Ladder_Generator_Record is new Generator_Record with record
      Node : Node_Id;

      Ob : Output_Buffer;
      --  Output buffer containing the whole program

      Ladder_Ob : Output_Buffer;
      Literal_Ob : Output_Buffer;
      
      Network : access Network_Record;
   end record;

   No_Ladder_Generator_Record : constant Unity_Generator_Record :=
     Unity_Generator_Record'
     (No_Generator_Record with
        Current_Entity => Empty,
      Ob               => null,
      Ob_Dummy         => null,
      Ob_Tasks         => null,
      Ob_Types         => null,
      Ob_Dfbs          => null,
      Ob_Vars          => null,
      Ob_Functional    => null,
      Ob_Program       => null,
      Ob_Sections      => null,
      Ob_Sr            => null,
      Ob_Tmp_Prog      => null,
      Fbd_Entity       => Empty);

end Unity.Gen.Ladder_Generator;
