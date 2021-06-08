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

with Ada.Text_Io;use Ada.Text_Io;


with Ada.Unchecked_Deallocation;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Common.Strings; use Common.Strings;
with Converts; use Converts;
with Containers.Lists;

with Glips.Node; use Glips.Node;
with Glips.Node.Expression; use Glips.Node.Expression;
with Glips.Node.Names; use Glips.Node.Names;
with Glips.Node.Entity; use Glips.Node.Entity;
with Glips.Node.Instr;
with Glips.Node.Entity.Vars; use Glips.Node.Entity.Vars;

with Options;
with Boxes; use Boxes;

with Debug;

with Boxes.Terminal; use Boxes.Terminal;
with Boxes.Containers; use Boxes.Containers;
--with Unity.Output; use Unity.Output;
with Gls; use Gls;
--with Generic_Accept;
--with Visitor; use Visitor; --_UnityGen;
with Units;
with Unity.Gen;
with Unity.St_Emitor; use Unity.St_Emitor;

package body Unity.Ld_Emitor is

   procedure Out_Line is new Debug.Out_Line("Unity.Ld_Emitor", True);

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


   -- Pour le dessin, l'unite est la cellule. Y represente le nombre de
   -- cellules depuis le debut du rung. Si on a un fonction block


   Unity_Ld_Header : String := "<LDSource nbColumns=""11"">";
   Unity_Network_Header : String := "<networkLD>";

   Unity_Ld_Tailer : String := "</LDSource>";
   Unity_Network_Tailer : String := "</networkLD>";

   type Mode_Type is (Mode_In, Mode_Out, Mode_In_Out);

   package Strings is new Containers.Lists (String_Ptr, null);
   use Strings;

   Unity_Com : Strings.List_Id;

   Enable_Hlink : Boolean := False;
   Enable_Nb    : Natural := 0;


   ---------------------
   -- Get_Line_Offset --
   ---------------------

   function Get_Line_Offset
     (Ld : in Unity_Ladder_Emitor_Record) return Natural is
   begin
      return Ld.Line_Offset;
   end Get_Line_Offset;

   ---------------------
   -- Set_Line_Offset --
   ---------------------

   procedure Set_Line_Offset
     (Ld : in out Unity_Ladder_Emitor_Record;
      Y  : in Natural) is
   begin
      Ld.Line_Offset := Y;
   end Set_Line_Offset;


   ---------------------
   -- Add_Line_Offset --
   ---------------------

   procedure Add_Line_Offset
     (Ld : in out Unity_Ladder_Emitor_Record;
      Y  : in Natural) is
   begin
      Ld.Line_Offset := Ld.Line_Offset + Y;
   end Add_Line_Offset;


   --------------------
   -- Get_Call_Block --
   --------------------

   function Get_Call_Block
     (Ld : in Unity_Ladder_Emitor_Record) return Boolean is
   begin
      return Ld.Call_Block;
   end Get_Call_Block;

   --------------------
   -- Set_Call_Block --
   --------------------

   procedure Set_Call_Block
     (Ld : in out Unity_Ladder_Emitor_Record;
      B  : in Boolean) is
   begin
      Ld.Call_Block := B;
   end Set_Call_Block;


   -----------------------------
   -- New_Unity_Ladder_Emitor --
   -----------------------------

   function New_Unity_Ladder_Emitor
     (U : in Units.Unit_Id) return Unity_Ladder_Emitor is

      E : Unity_Ladder_Emitor := new Unity_Ladder_Emitor_Record;
   begin
      Out_Line ("New_Unity_Ladder_Emitor");
      Initialize (E.all, U);

      Out_Line ("New_Unity_Ladder_Emitor End");
      return E;
   end New_Unity_Ladder_Emitor;


   --------------------------------
   -- Delete_Unity_Ladder_Emitor --
   --------------------------------

   procedure Delete_Unity_Ladder_Emitor
     (Ld : access Unity_Ladder_Emitor_Record'Class) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Unity_Ladder_Emitor_Record'Class, Unity_Ladder_Emitor);

      L   : Unity_Ladder_Emitor := Unity_Ladder_Emitor (Ld);
      Ob  : Output_Buffer := Get_Output_Buffer (Ld.all);
      Vis : Unity_St_Emitor := Unity_St_Emitor (Get_Litteral_Visitor (Ld.all));
   begin
      Out_Line ("Delete_Unity_Ladder_Emitor");

      Delete_Output_Buffer (Ob);
      Delete_Unity_Literal_Generator (Vis);
      Free (L);

      Out_Line ("Delete_Unity_Ladder_Emitor End");
   end Delete_Unity_Ladder_Emitor;


   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Ld : in out Unity_Ladder_Emitor_Record;
      U  : in Units.Unit_Id) is

   begin
      Out_Line ("Initialize");

      Initialize (Ladder_Emitor_Record (Ld), U);

      declare
         Tb : Output_Buffer := Get_Temp_Buffer (Ld);
      begin
         --Set_Simple_Append (Tb);
         Set_Platform (Tb, Platform_Unity);
         Set_Active (Tb, True);
      end;

      -- Create and attach the literal generator.
      Set_Litteral_Visitor (Ld, New_Unity_Literal_Generator);
      Initialize_Emitor (Get_Litteral_Visitor (Ld).all);

      -- Do the literal generator, to use the temp buf.
      Set_Buffer (Get_Litteral_Visitor (Ld).all, Get_Temp_Buffer (Ld));
      -- Do the literal generator, to NOT emit the comment neither the label in litteral part of a ladder(ex coil_operate)
      Set_Emit_Comment (Get_Litteral_Visitor (Ld).all, False);


      -- Output buffer of the ladder generator.
      declare
         Ob : Output_Buffer := New_Output;
      begin
         Set_Output_Buffer (Ld, Ob);
         Set_Active (Ob, True);
--         Set_Simple_Append (Ob);
--         Set_Platform (Ob, Platform_Unity);
      end;

      Ld.Line_Offset := 0;
      Ld.Call_Block := False;

      Out_Line ("Initialize End");
   end Initialize;


   --------------
   -- Finalize --
   --------------

   procedure Finalize (Ld  : in out Unity_Ladder_Emitor_Record) is
   begin
      Out_Line ("Finalize");
      Finalize (Ladder_Emitor_Record (Ld));
      Out_Line ("Finalize End");
   end Finalize;


   -----------------
   -- Emit_Header --
   -----------------

   procedure Emit_Header (Ld : in out Unity_Ladder_Emitor_Record) is
      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Header");
      Write_Line_Indent (Ob, Unity_Ld_Header);
      Inc_Indentation (Ob);
      Write_Line_Indent (Ob, Unity_Network_Header);
      Inc_Indentation (Ob);

      Out_Line ("Emit_Header End");
   end Emit_Header;


   -----------------
   -- Emit_Tailer --
   -----------------

   procedure Emit_Tailer (Ld : in out Unity_Ladder_Emitor_Record) is
      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Tailer");
      Dec_Indentation (Ob);
      Write_Line_Indent (Ob, Unity_Network_Tailer);
      Dec_Indentation (Ob);
      Write_Line_Indent (Ob, Unity_Ld_Tailer);
      Newline (Ob);

      Out_Line ("Emit_Tailer");
   end Emit_Tailer;


   ------------------------
   -- Emit_Begin_Network --
   ------------------------

   procedure Emit_Begin_Network
     (Ld : in out Unity_Ladder_Emitor_Record;
      Nw : in Network) is

      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Begin_Network");

      Inc_Indentation (Ob);

      Unity_Com := Strings.New_List;

      Set_Line_Offset (Ld, 0);

      Out_Line ("Emit_Begin_Network");
   end Emit_Begin_Network;


   ----------------------
   -- Emit_End_Network --
   ----------------------

   procedure Emit_End_Network
     (Ld : in out Unity_Ladder_Emitor_Record;
      Nw : in Network) is
      It : Strings.List_Iterator := New_Iterator (Unity_Com);

      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_End_Network");

      while not Is_End (It) loop
         Write_Line_Indent (Ob, Current_Item (It).all);
         Next (It);
      end loop;

      Out_Line ("Emit_End_Network");
   end Emit_End_Network;


   -----------------
   -- Line_Number --
   -----------------

   function Line_Number (S : in String) return Natural is
      Ln : Natural := 1;
      I  : Positive := S'First;
   begin
      Out_Line ("Emit_Number");

      loop
         exit when I > S'Last;

         if S (I) = Ascii.Cr or else S (I) = Ascii.Lf then
            Ln := Ln + 1;
            if I < S'Last then
               if S (I+1) = Ascii.Cr or else S (I+1) = Ascii.Lf then
                  I := I + 1;
               end if;
            end if;
         end if;

         I := I + 1;
      end loop;

      Out_Line ("Emit_Number => End");
      return Ln;
   exception
      when others =>
         Debug.Output (True, "Exception in Line_Number");
         return Ln;
   end Line_Number;


   ------------------
   -- Emit_Comment --
   ------------------

   procedure Emit_Comment
     (Ld    : in out Unity_Ladder_Emitor_Record;
      R     : in Rung;
      Lab   : in Boolean) is

      Ln : Natural;
      H  : Natural;
      X  : Natural := 1;
      S  : String := Unity.Gen.Normalize_Comment
        (Unity.Gen.To_Unity_Comment (Get_Comment (R)));

      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Comment ");

      Ln := Line_Number (S);
      if Ln mod 3 /= 0 then
         H := Ln / 3 + 1;
      else
         H := Ln / 3;
      end if;

      declare
         Com : String :=
           "<textBox width=""10"" height=""" & Trim (H'Img, Both) &
           """>" &
           S & "<objPosition posX=""" & Trim (X'Img, Both) &
           """ posY=""" & Trim (Get_Line_Offset (Ld)'Img, Both) &
           """>" & "</objPosition> </textBox>";
      begin
         Strings.Append (new String'(Com), Unity_Com);

         if Lab then
            H := H - 1;
         end if;

         if H /= 0 then
            Inc_Indentation (Ob);
            Write_Line_Indent
              (Ob, "<typeLine> <emptyLine nbRows=""" & Trim (H'Img, Both) &
               """></emptyLine>");
            Write_Line_Indent (Ob, "</typeLine>");
            Dec_Indentation (Ob);

            Add_Line_Offset (Ld, H);
         end if;
      end;

      Out_Line ("Emit_Comment End");
   end Emit_Comment;


   ---------------------
   -- Emit_Begin_Rung --
   ---------------------

   procedure Emit_Begin_Rung
     (Ld : in out Unity_Ladder_Emitor_Record;
      R  : in Rung) is

      B    : Box_Id :=  Get_Box (R);
      Node : Node_Id;
      X    : Natural := 1;
      Lab  : Boolean := False;
      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Begin_Rung");

      Set_Call_Block (Ld, False);

      if B /= No_Box then

         Node := To_Node_Id (B);

         if Node /= No_Node then

            -- Skip line if Instruction has a label or a comment
            -- it helps to aerate the code.
            if Glips.Node.Instr.Get_Instr_Label (Node) /= No_Node or else
              Get_Comment (R) /= No_Name then

               Write_Line_Indent
                 (Ob, "<typeLine> <emptyLine nbRows=""1""> </emptyLine>");
               Write_Line_Indent (Ob, "</typeLine>");

               Add_Line_Offset (Ld, 1);
            end if;

            if Glips.Node.Instr.Get_Instr_Label (Node) /= No_Node then


               Write_Line_Indent
                 (Ob, "<typeLine>");
               Inc_Indentation (Ob);

               declare
                  Label : String :=
                    Format (Glips.Node.Instr.Get_Instr_Label (Node));
               begin

                  Write_Line_Indent
                    (Ob, "<labelCell label=""" & Label & """>");
               exception
                  when E: others =>
                     Debug.Unknown_Exception(E, "Emit_Begin_Rung: Bad label");
                     raise;
               end;

               Inc_Indentation (Ob);

               Write_Line_Indent
                 (Ob, "<objPosition posX=""0"" " &
                  "posY=""" & Trim (Get_Line_Offset(Ld)'Img, Both) &
                  """></objPosition>");

               Dec_Indentation (Ob);

               Write_Line_Indent
                 (Ob, "</labelCell>");

               Write_Line_Indent
                 (Ob, "<emptyCell nbCells=""10""></emptyCell>");

               Dec_Indentation (Ob);
               Write_Line_Indent
                 (Ob, "</typeLine>");

               Lab := True;
            end if;
         end if;
      end if;

      -- Emit the comment if any.

      if Get_Comment (R) /= No_Name then
         Emit_Comment (Ld, R, Lab);
      end if;

      if Lab then
         Add_Line_Offset (Ld, 1);
      end if;

      Inc_Indentation (Ob);

      Out_Line ("Emit_Begin_Rung End");
   end Emit_Begin_Rung;


   -------------------
   -- Emit_End_Rung --
   -------------------

   procedure Emit_End_Rung
     (Ld : in out Unity_Ladder_Emitor_Record;
      R  : in Rung) is

      Ob   : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_End_Rung");
      Dec_Indentation (Ob);
      Out_Line ("Emit_End_Rung End");
   end Emit_End_Rung;


   ---------------------
   -- Emit_Begin_Line --
   ---------------------

   procedure Emit_Begin_Line (Ld : in out Unity_Ladder_Emitor_Record) is

      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Begin_Line");

      Write_Line_Indent (Ob, "<typeLine>");
      Inc_Indentation (Ob);

      Out_Line ("Emit_Begin_Line End");
   end Emit_Begin_Line;


   -------------------
   -- Emit_End_Line --
   -------------------

   procedure Emit_End_Line (Ld : in out Unity_Ladder_Emitor_Record) is

      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_End_Line");
      Dec_Indentation (Ob);
      Write_Line_Indent (Ob, "</typeLine>");


       if Enable_Hlink then
          Unity.Ld_Emitor.Emit_Enable_Hlink (Ld, Enable_Nb);
          Unity.Ld_Emitor.Set_Enable_Hlink (Ld, False);
       end if;

       if not Get_Call_Block (Ld) then
          Add_Line_Offset (Ld, 1); -- Get_H (B));
       end if;

      Out_Line ("Emit_End_Line End");
   end Emit_End_Line;


   ---------------------
   -- Emit_Open_Vlink --
   ---------------------

   procedure Emit_Open_Vlink
     (Ld : in out Unity_Ladder_Emitor_Record;
      B  : in Box_Id) is

      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Open_Vlink");
      Write_Line_Indent (Ob, "<shortCircuit>");
      Inc_Indentation (Ob);
      Write_Line_Indent (Ob, "<VLink></VLink>");

      Out_Line ("Emit_Open_Vlink End");
   end Emit_Open_Vlink;


   ----------------------
   -- Emit_Close_Vlink --
   ----------------------

   procedure Emit_Close_Vlink
     (Ld : in out Unity_Ladder_Emitor_Record;
      B  : in Box_Id) is

      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Close_Vlink");
      Dec_Indentation (Ob);
      Write_Line_Indent (Ob, "</shortCircuit>");

      Out_Line ("Emit_Close_Vlink End");
   end Emit_Close_Vlink;


   ----------------
   -- Emit_Empty --
   ----------------

   procedure Emit_Empty
     (Ld  : in out Unity_Ladder_Emitor_Record;
      Nb  : in Natural;
      Fin : in Boolean := False) is

      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Empty");
      Write_Line_Indent
        (Ob,
         "<emptyCell nbCells=""" & Trim (Nb'Img, Both) & """></emptyCell>");

      Out_Line ("Emit_Empty End");
   end Emit_Empty;


   ----------------
   -- Emit_Hlink --
   ----------------

   procedure Emit_Hlink
     (Ld : in out Unity_Ladder_Emitor_Record;
      Nb : in Natural;
      B  : in Box_Id) is

      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Hlink");
      Write_Line_Indent (Ob,
                  "<HLink nbCells=""" & Trim (Nb'Img, Both) & """></HLink>");

      Out_Line ("Emit_Hlink End");
   end Emit_Hlink;


   -------------------------
   -- Emit_Designator_Box --
   -------------------------

   procedure Emit_Designator_Box
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B  : in Box_Id) is
   begin
      Emit_Empty (Ld, 1);
      null;
   end Emit_Designator_Box;


   ------------------
   -- Emit_Contact --
   ------------------

   procedure Emit_Contact
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B    : in Box_Id) is

      Node : Node_Id := To_Node_Id(B);
      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Contact");
      Write_Line_Indent
        (Ob,
         "<contact typeContact=""openContact"" contactVariableName="""
         & Get_Contact_Name (Ld, Node) & """></contact>");

      Out_Line ("Emit_Contact End");
   end Emit_Contact;


   ----------------------
   -- Emit_Not_Contact --
   ----------------------

   procedure Emit_Not_Contact
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B    : in Box_Id) is

      Node : Node_Id := To_Node_Id(B);
      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Not_Contact");
      Write_Line_Indent
        (Ob,
         "<contact typeContact=""closedContact"" contactVariableName="""
         & Get_Contact_Name (Ld, Node) & """></contact>");

      Out_Line ("Emit_Not_Contact End");
   end Emit_Not_Contact;


   ------------------------
   -- Emit_Raise_Contact --
   ------------------------

   procedure Emit_Raise_Contact
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B    : in Box_Id) is

      Node : Node_Id := To_Node_Id(B);
      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Raise_Contact");
      Write_Line_Indent
        (Ob,
         "<contact typeContact=""PContact"" contactVariableName="""
         & Get_Contact_Name (Ld, Node) & """></contact>");

      Out_Line ("Emit_Raise_Contact End");
   end Emit_Raise_Contact;


   --------------------------
   -- Emit_Falling_Contact --
   --------------------------

   procedure Emit_Falling_Contact
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B    : in Box_Id) is

      Node : Node_Id := To_Node_Id(B);
      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Falling_Contact");
      Write_Line_Indent
        (Ob,
         "<contact typeContact=""NContact"" contactVariableName="""
         & Get_Contact_Name (Ld, Node) & """></contact>");

      Out_Line ("Emit_Falling_Contact End");
   end Emit_Falling_Contact;


   --------------------------
   -- Emit_Compare_Contact --
   --------------------------

   procedure Emit_Compare_Contact
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B    : in Box_Id) is

      Node : Node_Id := To_Node_Id(B);
      Ob  : Output_Buffer := Get_Output_Buffer (Ld);
      Vis : Unity_St_Emitor := Unity_St_Emitor (Get_Litteral_Visitor (Ld));
   begin
      Out_Line ("Emit_Compare_Contact");
      --To_Mixed_Utf8 (S);

      Set_Start_Instruction (Vis.all);
      declare
         S : String := Get_Litteral_Expression (Ld, Node);
      begin
         Write_Line_Indent
           (Ob,
            "<compareBlock width=""2"">");
         Inc_Indentation (Ob);
         Write_Line_Indent (Ob,
                            "<expression>" & S & "</expression>");
      end;
      Reset_Start_Instruction (Vis.all);

      Dec_Indentation (Ob);
      Write_Line_Indent (Ob, "</compareBlock>");

      Out_Line ("Emit_Compare_Contact End");
   end Emit_Compare_Contact;


   ------------------------
   -- Emit_Positive_Coil --
   ------------------------

   procedure Emit_Positive_Coil
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B    : in Box_Id) is

      Node : Node_Id := To_Node_Id(B);
      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Positive_Coil");
      Write_Line_Indent
        (Ob,
         "<coil typeCoil=""coil"" coilVariableName=""" &
         Get_Contact_Name (Ld, Node) & """></coil>");

      Out_Line ("Emit_Positive_Coil End");
   end Emit_Positive_Coil;


   ------------------------
   -- Emit_Negative_Coil --
   ------------------------

   procedure Emit_Negative_Coil
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B    : in Box_Id) is

      Node : Node_Id := To_Node_Id(B);
      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Negative_Coil");
      Write_Line_Indent
        (Ob,
         "<coil typeCoil=""notCoil"" coilVariableName=""" &
         Get_Contact_Name (Ld, Node) & """></coil>");

      Out_Line ("Emit_Negative_Coil End");
   end Emit_Negative_Coil;


   -------------------
   -- Emit_Set_Coil --
   -------------------

   procedure Emit_Set_Coil
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B    : in Box_Id) is

      Node : Node_Id := To_Node_Id(B);
      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Set_Coil");
      Write_Line_Indent
        (Ob,
         "<coil typeCoil=""setCoil"" coilVariableName=""" &
         Get_Contact_Name (Ld, Node) & """></coil>");

      Out_Line ("Emit_Set_Coil End");
   end Emit_Set_Coil;


   ---------------------
   -- Emit_Reset_Coil --
   ---------------------

   procedure Emit_Reset_Coil
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B    : in Box_Id) is

      Node : Node_Id := To_Node_Id(B);
      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Reset_Coil");
      Write_Line_Indent
        (Ob,
         "<coil typeCoil=""resetCoil"" coilVariableName=""" &
         Get_Contact_Name (Ld, Node) & """></coil>");

      Out_Line ("Emit_Reset_Coil End");
   end Emit_Reset_Coil;


   --------------------
   -- Emit_Goto_Coil --
   --------------------

   procedure Emit_Goto_Coil
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B    : in Box_Id) is

      Node : Node_Id := To_Node_Id(B);
      S  : String := Format (Glips.Node.Instr.Get_Goto_Dest (Node));
      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Goto_Coil");
      Write_Line_Indent
        (Ob,
         "<control typeControl=""jumpCoil"" label=""" &
         S & """></control>");

      Out_Line ("Emit_Goto_Coil End");

   exception
      when E: others =>
         Debug.Unknown_Exception(E, "Emit_Goto_Coil;");
         raise;
   end Emit_Goto_Coil;


   ----------------------
   -- Emit_Return_Coil --
   ----------------------

   procedure Emit_Return_Coil
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B    : in Box_Id ) is

      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Return_Coil");
      Write_Line_Indent (Ob,
                  "<control typeControl=""retCoil""></control>");

      Out_Line ("Emit_Return_Coil End");
   end Emit_Return_Coil;


   -----------------------
   -- Emit_Operate_Coil --
   -----------------------

   procedure Emit_Operate_Coil
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B    : in Box_Id) is

      Node : Node_Id := To_Node_Id(B);
      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Operate_Coil");
      declare
               S  : String := Get_Litteral_Expression (Ld, Node);
      begin

         Write_Line_Indent (Ob, "<operateBlock width=""4"">");
         Inc_Indentation (Ob);
         Write_Line_Indent (Ob, "<statement>" & S & "</statement>");
         Dec_Indentation (Ob);
         Write_Line_Indent (Ob, "</operateBlock>");
      end;
      Out_Line ("Emit_Operate_Coil End");
   end Emit_Operate_Coil;


   -----------------------------
   -- Emit_Invoke_Block_Begin --
   -----------------------------

   procedure Emit_Invoke_Block_Begin
     (Ld     : in out Unity_Ladder_Emitor_Record;
      Node   : in Node_Id;
      Act    : in Node_Id;
      Name   : in Name_Id;
      Proto  : in Name_Id;
      Enable : in Boolean;
      B      : in Box_Id) is

      X : Natural := Get_X(B);
      Y : Natural := Get_Y(B);
      H : Natural := Get_H(B);
      Fb_Name   : String := Get_String (Name);
      Type_Name : String := Get_String (Proto);
      Xfb       : Natural := X; --  + 1;
      Eno       : String := Trim (Enable'Img, Both);
      Ob        : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Invoke_Block_Begin");

      -- La hauteur + 1 ligne apres le block.
      Set_Call_Block (Ld, True);

      Type_Name := To_Upper_Utf8 (Type_Name);

      --Debug.Output (True, " X = " & X'Img);
      --Debug.Output (True, " Y = " & Y'Img);
      --Debug.Output (True, " H = " & H'Img);

--      Write_Line_Indent
--        (Ob, "<emptyCell nbCells=""" & To_String (X) & """></emptyCell>");

      Eno:= To_Lower_Utf8 (Eno);
      Write_Line_Indent
        (Ob, "<FFBBlock instanceName=""" & Fb_Name &
         """ typeName=""" & Type_Name &
         """ additionnalPinNumber=""0"" enEnO=""" & Eno & """ " &
         "width=""10"" " & "height=""" &
         To_String (H + 1) & """> ");

      Inc_Indentation (Ob);

      Write_Line_Indent (Ob, "<objPosition posX=""" &
                         To_String (Xfb) & """ posY=""" &
                         To_String (Get_Line_Offset (Ld)) &
                         """></objPosition>");
      Write_Line_Indent (Ob, "<descriptionFFB execAfter="""">");

      Add_Line_Offset (Ld, H + 1);

      Out_Line ("Emit_Invoke_Block_Begin End");
   end Emit_Invoke_Block_Begin;


   ---------------------------
   -- Emit_Invoke_Block_End --
   ---------------------------

   procedure Emit_Invoke_Block_End
     (Ld   : in out Unity_Ladder_Emitor_Record;
      B    : in Box_Id) is

      Node : Node_Id := To_Node_Id(B);
      Ob : Output_Buffer := Get_Output_Buffer (Ld);

   begin
      Out_Line( "Emit_Invoke_Block_End");

      Write_Line_Indent (Ob, "</descriptionFFB>");

      Dec_Indentation (Ob);
      Write_Line_Indent (Ob, "</FFBBlock>");

--      L := (11 - X - Get_L (Fb));  -- 11 - X + 2; -- (L -- Xfb + Get_L (Fb)).
--      Write_Line_Indent
--        (Ob,
--         "<emptyCell nbCells=""" & To_String (L) & """></emptyCell>");

      Dec_Indentation (Ob);

      Out_Line( "Emit_Invoke_Block_End End");
   end Emit_Invoke_Block_End;


   ---------------------------
   -- Emit_Node_In_Slot_Box --
   ---------------------------

   procedure Emit_Node_In_Slot_Box
     (Ld        : in out Unity_Ladder_Emitor_Record;
      Slot_Node : in Node_Id;
      In_Node   : in Node_Id;
      Xslot     : in Natural;
      Yslot     : in Natural;
      Xin       : in Natural;
      Yin       : in Natural) is

      Sname : Name_Id;
      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Node_In_Slot_Box");

      Sname := Get_Entity_Name (Get_Real_Node (Slot_Node));
      declare
      begin
         null;
      exception
         when E: others =>
            Debug.Unknown_Exception
              (E, "Emit_Node_In_Slot_Box : Bad Slot Name");
            raise;
      end;
      declare
         Formal : String := Get_String (Sname);
      begin
         Out_Line ("Formal");
         if In_Node /= No_Node then
            declare
               Effect : String := Get_Litteral_Expression (Ld, In_Node);
            begin
               if Effect (Effect'Last) = ';' then
                  Effect (Effect'Last) := ' ';
               end if;
               Formal := To_Upper_Utf8 (Formal);
               --Effect := To_Upper_Utf8 (Effect);
               Write_Line_Indent
                 (Ob,
                  "<inputVariable invertedPin=""false"" formalParameter=""" &
                  Formal & """ effectiveParameter=""" &
                  Effect & """></inputVariable>");
            end;
         else
            Formal := To_Upper_Utf8 (Formal);
            Write_Line_Indent
              (Ob,
               "<inputVariable invertedPin=""false"" formalParameter=""" &
               Formal & """></inputVariable>");
         end if;
      end;

      Out_Line ("Emit_Node_In_Slot_Box End");
   end Emit_Node_In_Slot_Box;


   ----------------------------
   -- Emit_Node_Out_Slot_Box --
   ----------------------------

   procedure Emit_Node_Out_Slot_Box
     (Ld        : in out Unity_Ladder_Emitor_Record;
      Slot_Node : in Node_Id;
      Out_Node  : in Node_Id;
      Xslot     : in Natural;
      Yslot     : in Natural;
      Xout      : in Natural;
      Yout      : in Natural) is

      Sname : Name_Id;
      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Node_Out_Slot_Box");

      if Slot_Node = No_Node then
         Sname := String_Find ("out");
      else
         declare
         begin
            Sname := Get_Entity_Name (Get_Real_Node (Slot_Node));
         exception
            when E: others =>
               Debug.Unknown_Exception(E, "Emit_Node_Out_Slot_Box;");
               raise;
         end;
      end if;

      declare
         Formal : String := Get_String (Sname);
      begin
         if Out_Node /= No_Node then
            declare
               Effect : String := Get_Litteral_Expression (Ld, Out_Node);
            begin
               Formal := To_Upper_Utf8 (Formal);
               --Effect := To_Upper_Utf8 (Effect);
               Write_Line_Indent
                 (Ob,
                  "<outputVariable invertedPin=""false"" formalParameter=""" &
                  Formal & """ effectiveParameter=""" &
                  Effect & """></outputVariable>");
            end;
         else
            Formal := To_Upper_Utf8 (Formal);
            Write_Line_Indent
              (Ob,
               "<outputVariable invertedPin=""false"" formalParameter=""" &
               Formal & """></outputVariable>");
         end if;
      end;

      Out_Line ("Emit_Node_Out_Slot_Box End");
   end Emit_Node_Out_Slot_Box;


   -------------------------
   -- Emit_Enable_In_Slot --
   -------------------------

   procedure Emit_Enable_In_Slot (Ld : in out Unity_Ladder_Emitor_Record) is

      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Enable_In_Slot");
      Write_Line_Indent
        (Ob,
         "<inputVariable invertedPin=""false"" formalParameter=""EN""" &
         "></inputVariable>");

      Out_Line ("Emit_Enable_In_Slot End");
   end Emit_Enable_In_Slot;


   --------------------------
   -- Emit_Enable_Out_Slot --
   --------------------------

   procedure Emit_Enable_Out_Slot (Ld : in out Unity_Ladder_Emitor_Record) is

      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Enable_Out_Slot");
      Write_Line_Indent
        (Ob,
         "<outputVariable invertedPin=""false"" formalParameter=""ENO""" &
         "></outputVariable>");

      Out_Line ("Emit_Enable_Out_Slot End");
   end Emit_Enable_Out_Slot;


   ------------------------
   -- Emit_Enable_Hlink  --
   ------------------------

   procedure Emit_Enable_Hlink
     (Ld : in out Unity_Ladder_Emitor_Record;
      L  : in Natural) is

      Ob : Output_Buffer := Get_Output_Buffer (Ld);
   begin
      Out_Line ("Emit_Enable_Hlink");

      Write_Line_Indent
        (Ob, "<typeLine>");

      Write_Line_Indent
        (Ob, "<HLink nbCells=""" & To_String (L) & """></HLink>");
      Write_Line_Indent
        (Ob, "<emptyCell nbCells=""" & To_String (11-L) & """></emptyCell>");

      Write_Line_Indent
        (Ob, "</typeLine>");

      Out_Line ("Emit_Enable_Hlink End");
   end Emit_Enable_Hlink;


   ----------------------
   -- Set_Enable_Hlink --
   ----------------------

   procedure Set_Enable_Hlink
     (Ld : in out Unity_Ladder_Emitor_Record;
      B  : in Boolean;
      L  : in Natural := 0) is
   begin
      Out_Line ("Set_Enable_Hlink");
      Enable_Hlink := B;
      Enable_Nb := L;
      Out_Line ("Set_Enable_Hlink End");
   end Set_Enable_Hlink;

end Unity.Ld_Emitor;
