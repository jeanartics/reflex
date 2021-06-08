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

package Unity.Gen is

   type Unity_Generator_Record is new Generator_Record with private;
   type Unity_Generator_Ptr is access all Unity_Generator_Record;
   type Unity_Generator_Class_Ptr is access all Unity_Generator_Record'Class;

   No_Unity_Generator_Record : constant Unity_Generator_Record;

   function New_Unity_Generator return Unity_Generator_Ptr;

   procedure Free_Unity_Generator (This : in out Unity_Generator_Ptr);

   function Get_Tasks_Output_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer;
   procedure Set_Tasks_Output_Buffer
     (This : access Unity_Generator_Record;
      Ob   : Output_Buffer);

   function Get_Types_Output_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer;
   procedure Set_Types_Output_Buffer
     (This : access Unity_Generator_Record;
      Ob   : Output_Buffer);
   --  Output buffer containing the types of the program

   function Get_Dfbs_Output_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer;
   procedure Set_Dfbs_Output_Buffer
     (This : access Unity_Generator_Record;
      Ob   : Output_Buffer);
   --  Output buffer containing the Dfb of the program

   function Get_Vars_Output_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer;
   procedure Set_Vars_Output_Buffer
     (This : access Unity_Generator_Record;
      Ob   : Output_Buffer);
   --  Output buffer containing the vars of the program

   function Get_Functional_Output_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer;
   procedure Set_Functional_Output_Buffer
     (This : access Unity_Generator_Record;
      Ob   : Output_Buffer);
   --  Output buffer containing the program part of unity

   function Get_Program_Output_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer;
   procedure Set_Program_Output_Buffer
     (This : access Unity_Generator_Record;
      Ob   : Output_Buffer);
   --  Output buffer containing the program part of unity

   function Get_Sections_Output_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer;
   procedure Set_Sections_Output_Buffer
     (This : access Unity_Generator_Record;
      Ob   : Output_Buffer);

   function Get_Sr_Output_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer;
   procedure Set_Sr_Output_Buffer
     (This : access Unity_Generator_Record;
      Ob   : Output_Buffer);

   function Get_Tmp_Prog_Output_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer;

   procedure Set_Tmp_Prog_Output_Buffer
     (This : access Unity_Generator_Record;
      Ob   : Output_Buffer);

   function In_Fbd_Generation
     (This : access Unity_Generator_Record) return Boolean;

   function Get_Current_Entity
     (This : access Unity_Generator_Record) return Entity_Id;
   procedure Set_Current_Entity
     (This : access Unity_Generator_Record;
      E    : Entity_Id);

   function Get_Current_Types_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer;

   function Get_Current_Vars_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer;

   function Get_Current_Statments_Buffer
     (This : access Unity_Generator_Record) return Output_Buffer;

   procedure Do_Generation (This : access Unity_Generator_Record);
   
   procedure Generate_Node
     (This        : access Unity_Generator_Record;
      Node        : Node_Id; 
      Declaration : Boolean := False);
   
   procedure Generate_Literal_Expression
     (This : access Unity_Generator_Record;
      Node : Node_Id;
      Ob   : Output_Buffer);
   
private

   type Unity_Generator_Record is new Generator_Record with record
      Current_Entity : Entity_Id;

      Ob_Dummy : Output_Buffer;

      --  Ob_Settings : Output_Buffer;

      Ob_Tasks : Output_Buffer;

      Ob_Types : Output_Buffer;
      --  Output buffer containing the types of the program

      Ob_Dfbs : Output_Buffer;
      --  Output buffer containing the Dfb of the program

      Ob_Vars : Output_Buffer;
      --  Output buffer containing the vars of the program

      Ob_Functional : Output_Buffer;
      --  Unity Functional view of the program

      Ob_Program : Output_Buffer;
      --  Output buffer containing the strcted text of the program

      Ob_Sections : Output_Buffer;
      --  Output buffer containing the strcted text of the program

      Ob_Sr : Output_Buffer;
      --  Output buffer containing the strcted text of the program

      Ob_Tmp_Prog : Output_Buffer;

      Fbd_Entity : Entity_Id;
   end record;

   No_Unity_Generator_Record : constant Unity_Generator_Record :=
     Unity_Generator_Record'
     (No_Generator_Record with
        Current_Entity => Empty,
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

end Unity.Gen;
