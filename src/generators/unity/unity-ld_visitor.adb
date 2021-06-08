---
--- This file and its contents are the property of Itris Automation Square.
--- This file contains confidential proprietary information.
--- The reproduction, distribution, utilization or the communication
--- of this file or any part thereof is strictly prohibited.
--- Offenders will be held liable for the payment of damages.
---
--- Copyright 1999-2009 Itris Automation Square. All rights reserved.
---
--- Last author       : $Author$
--- Last revision     : $Rev$
--- Last Changed Date : $Date$
---

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.Case_Util; use GNAT.Case_Util;

with Types; use Types;
with Options;
with Namet; use Namet;


with Glips.Node; use Glips.Node;
with Glips.Node.Entity; use Glips.Node.Entity;
with Glips.Node.Entity.Actions ; use Glips.Node.Entity.Actions;
with Glips.Node.Entity.Vars; use  Glips.Node.Entity.Vars;
with Glips.Node.Names; use  Glips.Node.Names;
with Glips.Node.Instr;

with Boxes; use Boxes;
with Boxes.Terminal; use Boxes.Terminal;
with Boxes.Containers; use Boxes.Containers;
with Boxes.Entity; use Boxes.Entity;
with Boxes.Dump; use Boxes.Dump;

with Walkers; use Walkers;


with Glips.Node.Entity; use Glips.Node.Entity;
with Glips.Node.Entity.Actions ; use Glips.Node.Entity.Actions;
with Glips.Node.Entity.Vars; use  Glips.Node.Entity.Vars;

with Boxes; use Boxes;
with Boxes.Terminal; use Boxes.Terminal;
with Boxes.Containers; use Boxes.Containers;
with Boxes.Entity; use Boxes.Entity;
with Boxes.Dump; use Boxes.Dump;

with Debug; use Debug;

with Glips.Gen.Gls;
with Generic_Accept;

package body Unity.Ld_Visitor is

   procedure Out_Line is new Debug.Out_Line("Unity.Ld_Visitor", True);

   Debug_On : Boolean := True; -- False;

   --------------
   -- Out_line --
   --------------

   procedure DOut_Line (S : in String) is
   begin
      --Out_Line (S);
      null;
      --DOut_Line (Debug_On, S);
   end DOut_Line;



   Instance_Count : Natural := 0;

   ------------------------
   -- Get_Instance_Count --
   ------------------------

   function Get_Instance_Count return Natural is
   begin
      Instance_Count := Instance_Count + 1;
      return Instance_Count;
   end Get_Instance_Count;


   --------------------------------
   -- New_Unity_Ladder_Generator --
   --------------------------------

   function New_Unity_Ladder_Generator
     (Ui  : in Units.Unit_Id) return Unity_Ladder_Visitor is

      V : Unity_Ladder_Visitor := new Unity_Ladder_Visitor_Record;
      E : Unity_Ladder_Emitor :=  New_Unity_Ladder_Emitor (Ui);
   begin
      Set_Ladder_Emitor (V.all, E);
      return V;
   end New_Unity_Ladder_Generator;


   ------------------------------------
   --  Delete_Unity_Ladder_Generator --
   ------------------------------------

   procedure Delete_Unity_Ladder_Generator
     (V : in out Unity_Ladder_Visitor_Record) is
   begin
      null;
   end  Delete_Unity_Ladder_Generator;


   ----------------------
   -- Visit_Initialize --
   ----------------------

   procedure Visit_Initialize
     (V   : in out Unity_Ladder_Visitor_Record;
      Ui  : in Units.Unit_Id;
      Nw  : in Network) is

      Bnw  : Box_Id;
      Node : Node_Id;
      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin
      Bnw := Get_Box (Nw);

      Node := To_Node_Id (Bnw);

      Set_Network (V, Nw);
      Set_Rung (V, No_Rung);
      Set_X (V, 0);
      Set_Y (V, 0);
      Reset_Hlink_Repetition (V);
      Reset_Empty_Repetition (V);

      Emit_Header (Ld.all);
     end Visit_Initialize;


   --------------------
   -- Visit_Finalize --
   --------------------

   procedure Visit_Finalize (V  : in out Unity_Ladder_Visitor_Record) is
      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin
      Emit_Tailer (Ld.all);
      Finalize (Ld.all);
   end Visit_Finalize;


   -------------------
   -- Visit_Network --
   -------------------

   procedure Visit_Network
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is

      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin

      if S = After then
         Emit_End_Network (Ld.all, Get_Network (V));
      else
         Emit_Begin_Network (Ld.all, Get_Network (V));
      end if;
   end Visit_Network;


   ----------------
   -- Visit_Rung --
   ----------------

   procedure Visit_Rung
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is

      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin
      if S = Before then
         Emit_Begin_Rung (Ld.all, Get_Rung (V));

      elsif S = After then
         Emit_End_Rung (Ld.all, Get_Rung (V));
      end if;
   end Visit_Rung;


   ----------------
   -- Visit_Line --
   ----------------

   procedure Visit_Line
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is

      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin
      if S = Before then
         Emit_Begin_Line (Ld.all);

      elsif S = After then
         Emit_End_Line (Ld.all);
      end if;
    end Visit_Line;


   ----------------
   -- Visit_Cell --
   ----------------

   procedure Visit_Cell
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is

      R  : Rung    := Get_Rung (V);
      X  : Natural := Get_X (V);
      Y  : Natural := Get_Y (V);
      B  : Box_Id  := Get_Cell_Box (R, X, Y);
      Lb : Natural;
      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin
      if B /= No_Box then
         Lb := Get_L (B);
      else
         Lb := 1;
      end if;

      if S = Before then
         if X /= Get_Last_X (R) then
            if Get_Cell_Open_Vlink (R, X + Lb, Y) then

               if Get_Hlink_Repetition (V) /= 0 then
                  Emit_Hlink (Ld.all, Get_Hlink_Repetition (V), B);
                  Reset_Hlink_Repetition (V);
               end if;

               if Get_Empty_Repetition (V) /= 0 then
                  Emit_Empty (Ld.all, Get_Empty_Repetition (V));
                  Reset_Empty_Repetition (V);
               end if;

               Emit_Open_Vlink (Ld.all, B);


            elsif Get_Cell_Close_Vlink (R, X + Lb, Y) then

               if Get_Hlink_Repetition (V) /= 0 then
                  Emit_Hlink (Ld.all, Get_Hlink_Repetition (V), B);
                  Reset_Hlink_Repetition (V);
               end if;

               if Get_Empty_Repetition (V) /= 0 then
                  Emit_Empty (Ld.all, Get_Empty_Repetition (V));
                  Reset_Empty_Repetition (V);
               end if;

               Emit_Open_Vlink (Ld.all, B);
            end if;
         end if;


      elsif S = After then
         if X /= Get_Last_X (R) then
            if Get_Cell_Open_Vlink (R, X + Lb, Y) then
               Emit_Close_Vlink (Ld.all, B);

            elsif Get_Cell_Close_Vlink (R, X + Lb, Y) then
               Emit_Close_Vlink (Ld.all, B);
            end if;
         end if;
      end if;
   end Visit_Cell;


   ---------------------
   -- Visit_Cell_Busy --
   ---------------------

   procedure Visit_Cell_Busy
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is
   begin
      null;
   end Visit_Cell_Busy;


   ----------------------
   -- Visit_Empty_Cell --
   ----------------------

   procedure Visit_Empty_Cell
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is

      R  : Rung    := Get_Rung (V);
      X  : Natural := Get_X (V);
      Y  : Natural := Get_Y (V);
      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin
      if S = Before then
         -- Debug.Output (True, "Unity.Visit_Empty_Cell ");
         Inc_Empty_Repetition (V);

         if X /= Get_Last_X (R) then
            if not Get_Cell_Open_Vlink (R, X + 1, Y) and then
              not Get_Cell_Close_Vlink (R, X + 1, Y) then
               if Is_Empty_Cell (R, X + 1, Y)  then
                  return;
               end if;
            end if;
         end if;

         Emit_Empty (Ld.all, Get_Empty_Repetition (V));
         Reset_Empty_Repetition (V);
      end if;
   end Visit_Empty_Cell;


   -----------------
   -- Visit_Hlink --
   -----------------

   procedure Visit_Hlink
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is

      R  : Rung    := Get_Rung (V);
      X  : Natural := Get_X (V);
      Y  : Natural := Get_Y (V);
      B  : Box_Id  := Get_Cell_Box (R, X, Y);
      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin
      if S = Before then

         Inc_Hlink_Repetition (V);

         if X /= Get_Last_X (R) then
            if not Get_Cell_Open_Vlink (R, X + 1, Y) and then
              not Get_Cell_Close_Vlink (R, X + 1, Y) then
               if Get_Cell_Hlink (R, X + 1, Y) then
                  return;
               end if;
            end if;
         end if;

         Emit_Hlink (Ld.all, Get_Hlink_Repetition (V), B);
         Reset_Hlink_Repetition (V);
      end if;
   end Visit_Hlink;


   ----------------------
   -- Visit_Open_Vlink --
   ----------------------

   procedure Visit_Open_Vlink
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is
   begin
      null;
   end Visit_Open_Vlink;


   -----------------------
   -- Visit_Close_Vlink --
   -----------------------

   procedure Visit_Close_Vlink
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is
   begin
      null;
   end Visit_Close_Vlink;


   --------------------------
   -- Visit_Designator_Box --
   --------------------------

   procedure Visit_Designator_Box
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is

      R  : Rung    := Get_Rung (V);
      X  : Natural := Get_X (V);
      Y  : Natural := Get_Y (V);
      B  : Box_Id  := Get_Cell_Box (R, X, Y);
      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin
      if S = Before then
         Emit_Designator_Box
           (Ld.all, B);
      end if;
   end Visit_Designator_Box;


   -------------------------------
   -- Visit_Slot_Designator_Box --
   -------------------------------

   procedure Visit_Slot_Designator_Box
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is
   begin
      null;
   end Visit_Slot_Designator_Box;


   -------------------
   -- Visit_Contact --
   -------------------

   procedure Visit_Contact
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is

      R  : Rung    := Get_Rung (V);
      X  : Natural := Get_X (V);
      Y  : Natural := Get_Y (V);
      B  : Box_Id  := Get_Cell_Box (R, X, Y);
      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin
      if S = Before then
         Emit_Contact (Ld.all, B);
      end if;
   end Visit_Contact;


   --------------------
   -- Visit_Null_Box --
   --------------------

   procedure Visit_Null_Box
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is
   begin
      null;
   end Visit_Null_Box;


   -------------------------
   -- Visit_True_Constant --
   -------------------------

   procedure Visit_True_Constant
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is

      R  : Rung := Get_Rung (V);
      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin
      if S = Before then
         Emit_Hlink (Ld.all, 1, No_Box);
      end if;
   end Visit_True_Constant;


   -----------------------
   -- Visit_Not_Contact --
   -----------------------

   procedure Visit_Not_Contact
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is

      R  : Rung    := Get_Rung (V);
      X  : Natural := Get_X (V);
      Y  : Natural := Get_Y (V);
      B  : Box_Id  := Get_Cell_Box (R, X, Y);
      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin
      if S = Before then
         Emit_Not_Contact (Ld.all, B);
      end if;
   end Visit_Not_Contact;


   -------------------------
   -- Visit_Raise_Contact --
   -------------------------

   procedure Visit_Raise_Contact
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is

      R  : Rung    := Get_Rung (V);
      X  : Natural := Get_X (V);
      Y  : Natural := Get_Y (V);
      B  : Box_Id  := Get_Cell_Box (R, X, Y);
      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin
      if S = Before then
         Emit_Raise_Contact (Ld.all, B);
      end if;
   end Visit_Raise_Contact;


   ---------------------------
   -- Visit_Falling_Contact --
   ---------------------------

   procedure Visit_Falling_Contact
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is

      R  : Rung    := Get_Rung (V);
      X  : Natural := Get_X (V);
      Y  : Natural := Get_Y (V);
      B  : Box_Id  := Get_Cell_Box (R, X, Y);
      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin
      if S = Before then
         Emit_Falling_Contact (Ld.all, B);
      end if;
   end Visit_Falling_Contact;


   ---------------------------
   -- Visit_Compare_Contact --
   ---------------------------

   procedure Visit_Compare_Contact
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is

      R  : Rung    := Get_Rung (V);
      X  : Natural := Get_X (V);
      Y  : Natural := Get_Y (V);
      B  : Box_Id  := Get_Cell_Box (R, X, Y);
      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin
      if S = Before then
         Emit_Compare_Contact (Ld.all, B);
      end if;

   end Visit_Compare_Contact;


   -------------------------
   -- Visit_Positive_Coil --
   -------------------------

   procedure Visit_Positive_Coil
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is

      R  : Rung    := Get_Rung (V);
      X  : Natural := Get_X (V);
      Y  : Natural := Get_Y (V);
      B  : Box_Id  := Get_Cell_Box (R, X, Y);
      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin
      if S = Before then
         Emit_Positive_Coil (Ld.all, B);
      end if;
   end Visit_Positive_Coil;


   -------------------------
   -- Visit_Negative_Coil --
   -------------------------

   procedure Visit_Negative_Coil
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is

      R  : Rung    := Get_Rung (V);
      X  : Natural := Get_X (V);
      Y  : Natural := Get_Y (V);
      B  : Box_Id  := Get_Cell_Box (R, X, Y);
      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin
      if S = Before then
         Emit_Negative_Coil (Ld.all, B);
      end if;
   end Visit_Negative_Coil;


   --------------------
   -- Visit_Set_Coil --
   --------------------

   procedure Visit_Set_Coil
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is

      R  : Rung    := Get_Rung (V);
      X  : Natural := Get_X (V);
      Y  : Natural := Get_Y (V);
      B  : Box_Id  := Get_Cell_Box (R, X, Y);
      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin
      if S = Before then
         Emit_Set_Coil (Ld.all, B);
      end if;
   end Visit_Set_Coil;


   ----------------------
   -- Visit_Reset_Coil --
   ----------------------

   procedure Visit_Reset_Coil
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is

      R  : Rung    := Get_Rung (V);
      X  : Natural := Get_X (V);
      Y  : Natural := Get_Y (V);
      B  : Box_Id  := Get_Cell_Box (R, X, Y);
      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin
      if S = Before then
         Emit_Reset_Coil (Ld.all, B);
      end if;
   end Visit_Reset_Coil;


   ---------------------
   -- Visit_Goto_Coil --
   ---------------------

   procedure Visit_Goto_Coil
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is

      R  : Rung    := Get_Rung (V);
      X  : Natural := Get_X (V);
      Y  : Natural := Get_Y (V);
      B  : Box_Id  := Get_Cell_Box (R, X, Y);
      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin
      if S = Before then
         Emit_Goto_Coil (Ld.all, B);
       end if;
    end Visit_Goto_Coil;


   -----------------------
   -- Visit_Return_Coil --
   -----------------------

   procedure Visit_Return_Coil
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is

      R  : Rung    := Get_Rung (V);
      X  : Natural := Get_X (V);
      Y  : Natural := Get_Y (V);
      B  : Box_Id  := Get_Cell_Box (R, X, Y);
      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin
      if S = Before then
         Emit_Return_Coil (Ld.all, B);
      end if;
   end Visit_Return_Coil;


   ---------------------
   -- Visit_Call_Coil --
   ---------------------

   procedure Visit_Call_Coil
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is

   begin
      null;
   end Visit_Call_Coil;


   ------------------------
   -- Visit_Operate_Coil --
   ------------------------

   procedure Visit_Operate_Coil
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is

      R  : Rung    := Get_Rung (V);
      X  : Natural := Get_X (V);
      Y  : Natural := Get_Y (V);
      B  : Box_Id  := Get_Cell_Box (R, X, Y);
      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin
      if S = Before then
         Emit_Operate_Coil (Ld.all, B);
      end if;
   end Visit_Operate_Coil;


   ------------------------
   -- Visit_Invoke_Block --
   ------------------------

   procedure Visit_Invoke_Block
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is

      R         : Rung    := Get_Rung (V);
      X         : Natural := Get_X (V);
      Y         : Natural := Get_Y (V);
      B         : Box_Id  := Get_Cell_Box (R, X, Y);
      Fb        : Box_Id  := Get_Invoke_Fb (B);
      Act_Node  : Node_Id;
      Bcnx      : Box_Id;
      Bin       : Box_Id;
      Bout      : Box_Id;
      Slot      : Box_Id;
      Proto     : Node_Id;
      Inst_Name : Name_Id;
      Type_Name : Name_Id;
      Nb_In     : Natural;
      Nb_Out    : Natural;
      Ld : Ladder_Emitor := Get_Ladder_Emitor (V);
   begin
      Out_Line ("Unity.Visit_Invoke_Block");

      if S = Before then
         Out_Line ("Unity.Visit_Invoke_Block (Before)");

         -- The procedure to call, it must be the main of a proto or the name
         -- of a procedure or function of the Unity_Lib, in that case the
         -- instance name is as ".X", with X a counter, incremented at each
         -- call.

         Act_Node := Get_Proc_From_Appel_Proc(To_Node_Id (Fb));


         -- The prototype to which the procedure belongs.

         Proto := Get_Current_Proto (Act_Node);

         Out_Line
           (" Action is " & Get_Entity_Name (Proto) &
            "." & Get_Entity_Name (Act_Node));


         -- If the procedure does not belonged to a prototype, then it must be
         -- in the Unity_Lib. The instance name is generated as ".x" with x is
         -- the current value of the Intance Names Counter. The type name is
         -- the name of the procedure or finction.

         if Proto = No_Node then
            Inst_Name  := String_Find("." & Trim(Get_Instance_Count'Img,Both));
            Type_Name := Get_Entity_Name (Act_Node);


            -- If the procedure belongs to a prototype, the Instance Name is
            -- the Prototype Var instance, and the type is the name of the
            -- prototype.

         else
            declare
               Appel      : Node_Id;
               Proto_Inst : Node_Id;
            begin
               Appel := Glips.Node.Instr.Get_Appel_Proc_Proc (To_Node_Id (B));
               Proto_Inst := Get_Proc_Proto_Instance (Appel);

               if Proto_Inst = No_Node then
                  Out_Line ("Unity.Ld_Visitor.Visit_Invoke_Block - " &
                            "Appel=" & Print(Appel) & " Proto_inst=No_Node");
                  raise Program_Error;
               end if;

               Inst_Name := Get_Entity_Name (Proto_Inst);
               Type_Name := Get_Entity_Name (Proto);
            end;
         end if;
         Out_Line ("Inst_Name => " & Get_String (Inst_Name));
         Out_Line ("Type_Name => " & Get_String (Type_Name));

         -- At this point, we know how to generate the name of the instance and
         -- the Prototype or procedure name. So generate the block.

         Emit_Invoke_Block_Begin
           (Ld.all,
            To_Node_Id (B),
            Act_Node,
            Inst_Name,
            Type_Name,
            Get_Invoke_Enable (B),
            Fb);


         -- Now we emit the slot Enable In, which validates the block call, and
         -- its link to the left potential.

         if Get_Invoke_Enable (B) then
            Emit_Enable_In_Slot (Ld.all);
         end if;

         Nb_In  := 0;
         Nb_Out := 0;

         -- Visiter chaque connection.

         Bcnx := Get_First_In_Box (B);
         while Bcnx /= No_Box loop
            Out_Line ("Begin In");

            Slot := Get_Slot_Cnx_Box (Bcnx);
            Bin  := Get_In_Cnx_Box (Bcnx);

            if Bin /= No_Box and then Is_Terminal_Box (Bin) and then
              Get_Terminal_Type (Bin) = Designator_Box then
               Emit_Node_In_Slot_Box
                 (Ld.all,
                  To_Node_Id (Slot), To_Node_Id (Bin),
                  Get_X (Slot), Get_Y (Slot),
                  Get_X (Bin), Get_Y (Bin));
               Nb_In := Nb_In + 1;
            else
               Emit_Node_In_Slot_Box
                 (Ld.all,
                  To_Node_Id (Slot), No_Node,
                  Get_X (Slot), Get_Y (Slot),
                  0,0);
            end if;

            Out_Line ("Next In");

            Next_In (Bcnx);
         end loop;

         Out_Line (" In box emitted");

         -- Emettre Enable en parametre Out.
         if Get_Invoke_Enable (B) then
            Emit_Enable_Out_Slot (Ld.all);
         end if;

         -- Les cnx en out.
         Bcnx := Get_First_Out_Box (B);
         while Bcnx /= No_Box loop
            Out_Line ("Begin Out");

            Slot := Get_Slot_Cnx_Box (Bcnx);
            Bout := Get_Out_Cnx_Box (Bcnx);

            if Bout /= No_Box and then Is_Terminal_Box (Bout) and then
              Get_Terminal_Type (Bout) = Designator_Box then
               Emit_Node_Out_Slot_Box
                 (Ld.all,
                  To_Node_Id (Slot), To_Node_Id (Bout),
                  Get_X (Slot), Get_Y (Slot),
                  Get_X (Bout), Get_Y (Bout));

               Nb_Out := Nb_Out + 1;
            else
               Emit_Node_Out_Slot_Box
                 (Ld.all,
                  To_Node_Id (Slot), No_Node,
                  Get_X (Slot), Get_Y (Slot),
                  0, 0);
            end if;

            Out_Line ("Next Out");
            Next_Out (Bcnx);
         end loop;

         Out_Line (" Out box emitted");

         -- Les connexions en In Out.
         Bcnx := Get_First_InOut_Box (B);
         while Bcnx /= No_Box loop

            Out_Line ("Begin InOut");

            -- Le designator sur le slot de la cnx.

            Slot  := Get_Slot_Cnx_Box (Bcnx);

            -- La connexion In.
            Bin := Get_In_Cnx_Box (Bcnx);

            if Bin /= No_Box and then Is_Terminal_Box (Bin) and then
              Get_Terminal_Type (Bin) = Designator_Box then
               Emit_Node_In_Slot_Box
                 (Ld.all,
                  To_Node_Id (Slot), To_Node_Id (Bin),
                  Get_X (Slot), Get_Y (Slot),
                  Get_X (Bin), Get_Y (Bin));
               Nb_In := Nb_In + 1;
            else
               Emit_Node_In_Slot_Box
                 (Ld.all,
                  To_Node_Id (Slot), No_Node,
                  Get_X (Slot), Get_Y (Slot),
                  0, 0);
            end if;

         Out_Line (" InOut In box emitted");

            -- La connexion Out.
            Bout := Get_Out_Cnx_Box (Bcnx);

            if Bout /= No_Box and then Is_Terminal_Box (Bout) and then
              Get_Terminal_Type (Bout) = Designator_Box then
               Emit_Node_Out_Slot_Box
                 (Ld.all,
                  To_Node_Id (Slot), To_Node_Id (Bout),
                  Get_X (Slot), Get_Y (Slot),
                  Get_X (Bout), Get_Y (Bout));
               Nb_Out := Nb_Out + 1;
            else
               Emit_Node_Out_Slot_Box
                 (Ld.all,
                  To_Node_Id (Slot), No_Node,
                  Get_X (Slot), Get_Y (Slot),
                  0, 0);
            end if;

            Out_Line (" InOut Out box emitted");

            Next_InOut (Bcnx);
         end loop;

         Out_Line (" InOut all box emitted");

         if Nb_In /= 0 or else Nb_Out /= 0 then
            if Nb_In > Nb_Out then
               Set_Emit_Empty_Rows (Nb_In);
            else
               Set_Emit_Empty_Rows (Nb_Out);
            end if;
         end if;

         Out_Line ("Unity.Visit_Invoke_Block (Before End)");


      elsif S = After then
         Out_Line ("Unity.Visit_Invoke_Block (After)");
         Emit_Invoke_Block_End
           (Ld.all, Fb);

         if Get_Invoke_Enable (B) then
            Set_Enable_Hlink (Ld.all, True, Get_X (Fb));
         end if;

         Out_Line ("Unity.Visit_Invoke_Block (After End)");
      end if;
   end Visit_Invoke_Block;



   -------------------
   -- Visit_Cnx_Box --
   -------------------

   procedure Visit_Cnx_Box
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is
   begin
      null;
   end Visit_Cnx_Box;


   ------------------
   -- Visit_If_Box --
   ------------------

   procedure Visit_If_Box
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is
   begin
      null;
   end Visit_If_Box;


   -------------------
   -- Visit_For_Box --
   -------------------

   procedure Visit_For_Box
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is
   begin
      null;
   end Visit_For_Box;


   ---------------------
   -- Visit_While_Box --
   ---------------------

   procedure Visit_While_Box
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is
   begin
      null;
   end Visit_While_Box;


   --------------------
   -- Visit_Loop_Box --
   --------------------

   procedure Visit_Loop_Box
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is
   begin
      null;
   end Visit_Loop_Box;


   --------------------
   -- Visit_Case_Box --
   --------------------

   procedure Visit_Case_Box
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is
   begin
      null;
   end Visit_Case_Box;


   -------------------------
   -- Visit_Enclosing_Box --
   -------------------------

   procedure Visit_Enclosing_Box
     (V  : in out Unity_Ladder_Visitor_Record;
      S  : in Step) is
   begin
      null;
   end Visit_Enclosing_Box;

end Unity.Ld_Visitor;
