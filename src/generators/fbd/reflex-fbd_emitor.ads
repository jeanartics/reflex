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

with Artics.Buffers;     use Artics.Buffers;
with Artics.Graph.Cells; use Artics.Graph.Cells;

with Reflex.Vertex_Value; use Reflex.Vertex_Value;

package Reflex.Fbd_Emitor is
   
   type Fbd_Emitor_Record is tagged private;
   type Fbd_Emitor_Ptr is access all Fbd_Emitor_Record;
   type Fbd_Emitor_Class_Ptr is access all Fbd_Emitor_Record'Class;

   No_Fbd_Emitor_Record : constant Fbd_Emitor_Record;
   
   procedure Initialize (This : access Fbd_Emitor_Record);
   
   function New_Fbd_Emitor return Fbd_Emitor_Ptr;
   procedure Free_Fbd_Emitor (This : in out Fbd_Emitor_Ptr);
         
   function Get_Output_Buffer 
     (This : access Fbd_Emitor_Record) return Output_Buffer;
   procedure Set_Output_Buffer
     (This : access Fbd_Emitor_Record;
      Ob   : Output_Buffer);
     
   procedure Emit_Header (This : access Fbd_Emitor_Record);
   procedure Emit_Tailer (This : access Fbd_Emitor_Record);
   
   --  Ffb blocks emitors
   
   procedure Emit_Ffb_Block
     (This : access Fbd_Emitor_Record;
      Cell : access Cell_Record'Class);
   --  Emit any type of FFB block
   
   procedure Emit_Ffb_Header
     (This : access Fbd_Emitor_Record;
      Cell : access Cell_Record'Class);
   --  Emit header of FFB block (Width, Height, typeName...)
   
   procedure Emit_Description_Ffb
     (This : access Fbd_Emitor_Record;
      Cell : access Cell_Record'Class);
   --  Emit Inputs and Outputs variables with all his parameters
   
   --  Links emitors
   
   procedure Emit_Link_Fb
     (This : access Fbd_Emitor_Record;
      Cell : access Cell_Record'Class);
   --  Emit any edge beetween two blocks
   
   procedure Emit_Link_Source
     (This : access Fbd_Emitor_Record;
      Cell : access Cell_Record'Class);
   --  Specify source pin and on which cell the edge is linked
   
   procedure Emit_Link_Destination
     (This : access Fbd_Emitor_Record;
      Cell : access Cell_Record'Class);
   --  Specify destination pin and on which cell the edge is linked
   
   procedure Emit_Grid_Obj_Pos
     (This : access Fbd_Emitor_Record;
      Cell : access Cell_Record'Class);
   --  Not yet used, Specify additional points of the edge
   
   --  Usuals functions
   
   procedure Emit_Obj_Position
     (This : access Fbd_Emitor_Record;
      Cell : access Cell_Record'Class);
   --  Emit postion, used for edges or a FFB block
                     
   function Determinate_Type_Name (Node : Node_Id) return String;
   
   function Determinate_Effectve_Param
     (Param_Value : Vertex_Value_Ptr) return String;
   --  If exist, determinate effective parameter of a formal parameter
   
   function Determinate_Type (Cell : access Cell_Record'Class) return String;
   -- Return type of an operand (ADD, SUB, ...) or name of procedure call
   
private
   type Fbd_Emitor_Record is tagged record
      Ob      : Output_Buffer;
   end record;
   No_Fbd_Emitor_Record : constant Fbd_Emitor_Record :=
     (Ob      => null);
   
end Reflex.Fbd_Emitor;
