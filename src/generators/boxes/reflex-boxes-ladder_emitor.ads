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
with Unity.Gen; use Unity.Gen;
with Reflex.Rungs; use Reflex.Rungs;

package Reflex.Boxes.Ladder_Emitor is
   
   type Ladder_Emitor_Record is tagged private;
   type Ladder_Emitor_Ptr is access all Ladder_Emitor_Record;
   type Ladder_Emitor_Class_Ptr is access all Ladder_Emitor_Record'Class;

   No_Ladder_Emitor_Record : constant Ladder_Emitor_Record;
   
   procedure Initialize (This : access Ladder_Emitor_Record);
   
   function New_Ladder_Emitor return Ladder_Emitor_Ptr;
   procedure Free_Ladder_Emitor (This : in out Ladder_Emitor_Ptr);
  
   procedure Emit_Header (This : access Ladder_Emitor_Record);
   procedure Emit_Tailer (This : access Ladder_Emitor_Record);
         
   function Get_Output_Buffer 
     (This : access Ladder_Emitor_Record) return Output_Buffer;
   procedure Set_Output_Buffer
     (This : access Ladder_Emitor_Record;
      Ob   : Output_Buffer);
      
   function Get_Literal_Generator
     (This : access Ladder_Emitor_Record) return access Unity_Generator_Record;
   procedure Set_Literal_Generator 
     (This    : access Ladder_Emitor_Record;
      Lit_Gen : access Unity_Generator_Record);
   
   function Get_Line_Offset
     (This : access Ladder_Emitor_Record) return Natural;
   procedure Add_Line_Offset
     (This : access Ladder_Emitor_Record;
      Inc  : Natural);
   
   procedure Emit_Begin_Network (This : access Ladder_Emitor_Record);
   procedure Emit_End_Network (This : access Ladder_Emitor_Record);
   
   procedure Emit_Begin_Rung
     (This : access Ladder_Emitor_Record;
      R    : access Rung_Record);
   procedure Emit_End_Rung
     (This : access Ladder_Emitor_Record;
      R    : access Rung_Record);
   
   procedure Update_Comment (This : access Ladder_Emitor_Record);
   procedure Emit_Comment
     (This : access Ladder_Emitor_Record;
      Node : Node_Id;
      R    : access Rung_Record);
   
   procedure Emit_Begin_Line (This : access Ladder_Emitor_Record);
   procedure Emit_End_Line (This   : access Ladder_Emitor_Record) ;
   
   procedure Emit_Open_Vlink (This  : access Ladder_Emitor_Record);
   procedure Emit_Close_Vlink (This : access Ladder_Emitor_Record);
   
   procedure Emit_Simple_Vlink (This  : access Ladder_Emitor_Record);
     
   procedure Emit_Empty
     (This : access Ladder_Emitor_Record;
      Nb   : Natural);
   
   procedure Emit_Hlink
     (This : access Ladder_Emitor_Record;
      Nb   : in Natural;
      Vlink : Boolean := False);
   
   procedure Emit_Open_Contact
     (This : access Ladder_Emitor_Record;
      B    : access Box_Record'Class);
   procedure Emit_Closed_Contact
     (This : access Ladder_Emitor_Record;
      B    : access Box_Record'Class);
   
   procedure Emit_Compare_Block
     (This  : access Ladder_Emitor_Record;
      B     : access Box_Record'Class);
   
   procedure Emit_Operate_Block
     (This  : access Ladder_Emitor_Record;
      B    : access Box_Record'Class) ;
   
   procedure Emit_Coil
     (This : access Ladder_Emitor_Record;
      B    : access Box_Record'Class);
   procedure Emit_Not_Coil
     (This : access Ladder_Emitor_Record;
      B    : access Box_Record'Class);
   
   procedure Emit_Set_Coil
     (This : access Ladder_Emitor_Record;
      B    : access Box_Record'Class) ;
   procedure Emit_Reset_Coil
     (This : access Ladder_Emitor_Record;
      B    : access Box_Record'Class);

   procedure Emit_Jump
     (This : access Ladder_Emitor_Record;
      B    : access Box_Record'Class);

   procedure Emit_Return_Coil
     (This : access Ladder_Emitor_Record;
      B    : access Box_Record'Class);

   --     procedure Emit_Enable_Hlink
   --       (This: in out Ladder_Emitor_Record;
   --        L  : in Natural);
   --     procedure Set_Enable_Hlink
   --       (This: in out Ladder_Emitor_Record;
   --        B  : in Boolean;
   --        L  : in Natural := 0);
   
   procedure Emit_Label 
     (This : access Ladder_Emitor_Record;
      B    : access Box_Record'Class;
      Y    : Natural);
   
private
   
   type Ladder_Emitor_Record is tagged record
      Ob      : Output_Buffer;
      Lit_Gen : access Unity_Generator_Record;
      
      Comment_Ob : Output_Buffer;
      Tmp_Ob : Output_Buffer;
      
      Line_Offset : Natural;
   end record;
   
   No_Ladder_Emitor_Record : constant Ladder_Emitor_Record :=
     (Ob          => null,
      Lit_Gen     => null,
      Comment_Ob  => null,
      Tmp_Ob      => null,
      Line_Offset => 0);
   
end Reflex.Boxes.Ladder_Emitor;
