---

with Ladder; use Ladder;
--with Visitor; use Visitor;

with Types; use Types;
with Boxes; use Boxes;
--with Unity.Output; use Unity.Output;
with Ladder.Emitor; use Ladder.Emitor;
with Glips.Analyzer; use Glips.Analyzer;
with Glips.Generator; use Glips.Generator;

package Unity.Ladder.Emitor is
   
   type Ladder_Emitor_Record is tagged private;
   
     new Ladder_Emitor_Record with record

        Line_Offset : Natural;
        -- Y Rung Offset from the beginning of the procedure.

        Call_Block : Boolean;
        -- True when the rung is an invoke block.
     end record;
   type Unity_Ladder_Emitor is access all Unity_Ladder_Emitor_Record'Class;

   function Get_Line_Offset
     (Ld : in Unity_Ladder_Emitor_Record) return Natural;
   pragma Inline (Get_Line_Offset);

   procedure Set_Line_Offset
     (Ld : in out Unity_Ladder_Emitor_Record;
      Y  : in Natural);
   pragma Inline (Set_Line_Offset);

   procedure Add_Line_Offset
     (Ld : in out Unity_Ladder_Emitor_Record;
      Y  : in Natural);
   pragma Inline (Set_Line_Offset);

   function Get_Call_Block
     (Ld : in Unity_Ladder_Emitor_Record) return Boolean;
   pragma Inline (Get_Call_Block);

   procedure Set_Call_Block
     (Ld : in out Unity_Ladder_Emitor_Record;
      B  : in Boolean);
   pragma Inline (Set_Call_Block);

   function New_Unity_Ladder_Emitor
     (U : in Units.Unit_Id) return Unity_Ladder_Emitor;
   -- Create an Unity ladder emitor.

   procedure Delete_Unity_Ladder_Emitor
     (Ld : access Unity_Ladder_Emitor_Record'Class);
   -- Freezes the unity ladder emitor.

   procedure Initialize
     (Ld  : in out Unity_Ladder_Emitor_Record;
      U   : in Units.Unit_Id);

   procedure Finalize (Ld  : in out Unity_Ladder_Emitor_Record);

   procedure Emit_Header (Ld : in out Unity_Ladder_Emitor_Record);
   -- Emit the file header.

   procedure Emit_Tailer (Ld : in out Unity_Ladder_Emitor_Record);
   -- Emit the file Tailer.

   procedure Emit_Begin_Network
     (Ld : in out Unity_Ladder_Emitor_Record;
      Nw : in Network);
   -- Emit before the emission of a network (Instr_List).

   procedure Emit_End_Network
     (Ld : in out Unity_Ladder_Emitor_Record;
      Nw : in Network);
   -- Emit after the emission of a network (Instr_List).

   procedure Emit_Begin_Rung
     (Ld : in out Unity_Ladder_Emitor_Record;
      R  : in Rung);
   -- Emit before the emission of a rung (An instruction).

   procedure Emit_End_Rung
     (Ld : in out Unity_Ladder_Emitor_Record;
      R  : in Rung);
   -- Emit Afetr the emission of a rung (An instruction).

   procedure Emit_Begin_Line (Ld : in out Unity_Ladder_Emitor_Record);
   -- Before emitting a new matrix line.

   procedure Emit_End_Line (Ld : in out Unity_Ladder_Emitor_Record);
   -- After the emission a matrix line.

   procedure Emit_Empty
     (Ld  : in out Unity_Ladder_Emitor_Record;
      Nb  : in Natural;
      Fin : in Boolean := False);
   -- Emit an empty cell.

   procedure Emit_Open_Vlink
     (Ld : in out Unity_Ladder_Emitor_Record;
      B  : in Box_Id);
    -- Emit an opened parenthesis.

   procedure Emit_Close_Vlink
     (Ld : in out Unity_Ladder_Emitor_Record;
      B  : in Box_Id);
   -- Emit a closed parenthesis.

   procedure Emit_Hlink
     (Ld : in out Unity_Ladder_Emitor_Record;
      Nb : in Natural;
      B  : in Box_Id);
       -- Emit an horizontal link.

   procedure Emit_Designator_Box
     (Ld : in out Unity_Ladder_Emitor_Record;
      B  : in Box_Id);
      -- Emit a node designator.

   procedure Emit_Contact
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B  : in Box_Id);
   -- Emit a positive contact.

   procedure Emit_Not_Contact
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B  : in Box_Id);
   -- Emit a negative contact.

   procedure Emit_Raise_Contact
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B  : in Box_Id);
   -- Emit a raise contact.

   procedure Emit_Falling_Contact
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B  : in Box_Id);
      -- Emit a falling contact.

   procedure Emit_Compare_Contact
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B  : in Box_Id);
   -- Emit a compare contact.

   procedure Emit_Positive_Coil
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B  : in Box_Id);
   -- Emit a positive coil.

   procedure Emit_Negative_Coil
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B  : in Box_Id);
   -- Emit a positive coil.
   -- Emit a negative coil.

   procedure Emit_Set_Coil
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B  : in Box_Id);
      -- Emit a set coil.

   procedure Emit_Reset_Coil
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B  : in Box_Id);
   -- Emit a reset coil.

   procedure Emit_Goto_Coil
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B  : in Box_Id);
      -- Emit a goto coil.

   procedure Emit_Return_Coil
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B  : in Box_Id);
   -- Emit a return coil.

   procedure Emit_Operate_Coil
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B  : in Box_Id);
   -- Emit an operate coil.

   procedure Emit_Invoke_Block_Begin
     (Ld     : in out Unity_Ladder_Emitor_Record;
      Node   : in Node_Id;
      Act    : in Node_Id;
      Name   : in Name_Id;
      Proto  : in Name_Id;
      Enable : in Boolean;
      B  : in Box_Id);

   procedure Emit_Invoke_Block_End
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B  : in Box_Id);
   -- Emit an invoke block, with a height of H. The height of an invoke
   -- function block depends on the height of the expression connected to its
   -- in slot.

   procedure Emit_Node_In_Slot_Box
     (Ld        : in out Unity_Ladder_Emitor_Record;
      Slot_Node : in Node_Id;
      In_Node   : in Node_Id;
      Xslot     : in Natural;
      Yslot     : in Natural;
      Xin       : in Natural;
      Yin       : in Natural);

   procedure Emit_Node_Out_Slot_Box
     (Ld        : in out Unity_Ladder_Emitor_Record;
      Slot_Node : in Node_Id;
      Out_Node  : in Node_Id;
      Xslot     : in Natural;
      Yslot     : in Natural;
      Xout      : in Natural;
      Yout      : in Natural);

   procedure Emit_Enable_In_Slot (Ld : in out Unity_Ladder_Emitor_Record);
   -- Emit the EN in pin of an Unity DFB.

   procedure Emit_Enable_Out_Slot (Ld : in out Unity_Ladder_Emitor_Record);
   -- Emit the ENO out pin of an Unity DFB.

   procedure Emit_Enable_Hlink
     (Ld : in out Unity_Ladder_Emitor_Record;
      L  : in Natural);

   procedure Set_Enable_Hlink
     (Ld : in out Unity_Ladder_Emitor_Record;
      B  : in Boolean;
      L  : in Natural := 0);


end Unity.Ladder.Emito;
