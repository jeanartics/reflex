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

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Unchecked_Deallocation;
with Ada.Strings; use Ada.Strings;
with Gnat.Case_Util; use Gnat.Case_Util;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Sinfo; use Sinfo;
with Atree; use Atree;
with Artics.Utils; use Artics.Utils;
with Reflex.Gen.Outputs; use Reflex.Gen.Outputs;

package body Reflex.Boxes.Ladder_Emitor is

   -----------------------
   -- New_Ladder_Emitor --
   -----------------------

   function New_Ladder_Emitor return Ladder_Emitor_Ptr is
      This : Ladder_Emitor_Ptr :=
        new Ladder_Emitor_Record'(No_Ladder_Emitor_Record);
   begin
      This.Ob         := New_Output_Buffer;
      This.Comment_Ob := New_Output_Buffer;
      This.Tmp_Ob     := New_Output_Buffer;
      
      return This;
   end New_Ladder_Emitor;

   ------------------------
   -- Free_Ladder_Emitor --
   ------------------------

   procedure Free_Ladder_Emitor (This : in out Ladder_Emitor_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Ladder_Emitor_Record, Ladder_Emitor_Ptr);
   begin
      if This /= null then
         Free (This);
      end if;
   end Free_Ladder_Emitor;

   --     Unity_Com : Strings.List_Id;
   --
   --     Enable_Hlink : Boolean := False;
   --     Enable_Nb    : Natural := 0;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : access Ladder_Emitor_Record) is
   begin
      null;
   end Initialize;

   -----------------------
   -- Get_Output_Buffer --
   -----------------------

   function Get_Output_Buffer
     (This : access Ladder_Emitor_Record) return Output_Buffer is
   begin
      return This.Ob;
   end Get_Output_Buffer;

   -----------------------
   -- Set_Output_Buffer --
   -----------------------

   procedure Set_Output_Buffer
     (This : access Ladder_Emitor_Record;
      Ob   : Output_Buffer) is
   begin
      This.Ob := Ob;
   end Set_Output_Buffer;

   ---------------------------
   -- Get_Literal_Generator --
   ---------------------------

   function Get_Literal_Generator
     (This : access Ladder_Emitor_Record) return access Unity_Generator_Record 
   is
   begin
      return This.Lit_Gen;
   end Get_Literal_Generator;

   ---------------------------
   -- Set_Literal_Generator --
   ---------------------------

   procedure Set_Literal_Generator
     (This    : access Ladder_Emitor_Record;
      Lit_Gen : access Unity_Generator_Record) is
   begin
      This.Lit_Gen := Lit_Gen;
   end Set_Literal_Generator;
   
   ---------------------
   -- Get_Line_Offset --
   ---------------------
   
   function Get_Line_Offset
     (This : access Ladder_Emitor_Record) return Natural is
   begin
      return This.Line_Offset;
   end Get_Line_Offset;
   
   ---------------------
   -- Add_Line_Offset --
   ---------------------
   
   procedure Add_Line_Offset
     (This : access Ladder_Emitor_Record;
      Inc  : Natural) is
   begin
      This.Line_Offset := This.Line_Offset + Inc;
   end Add_Line_Offset;
     
   -----------------
   -- Emit_Header --
   -----------------

   procedure Emit_Header (This : access Ladder_Emitor_Record) is
      Ob : Output_Buffer := This.Ob;
   begin
      Write_Indent_Str
        (Ob, "<LDSource nbColumns="""
         & Integer_To_String (Max_Unity_Ladder_Horizontal) & """>");
      Indent_Begin (Ob);
      Write_Eol (Ob);

      Write_Indent_Str (Ob, "<networkLD>");
      Indent_Begin (Ob);
      Write_Eol (Ob);
   end Emit_Header;

   -----------------
   -- Emit_Tailer --
   -----------------

   procedure Emit_Tailer (This : access Ladder_Emitor_Record) is
      Ob : Output_Buffer := This.Ob;
   begin

      Indent_End (Ob);
      Write_Indent_Str (Ob, "</networkLD>");
      Write_Eol (Ob);

      Indent_End (Ob);
      Write_Indent_Str (Ob, "</LDSource>");
      Write_Eol (Ob);
   end Emit_Tailer;

   --------------------
   -- Update_Comment --
   --------------------
   
   procedure Update_Comment (This : access Ladder_Emitor_Record) is
   begin
      null; -- Append_Buffer (This.Ob, This.Comment_Ob);
   end Update_Comment;
   
   ------------------
   -- Emit_Comment --
   ------------------
   
   procedure Emit_Comment
     (This : access Ladder_Emitor_Record;
      Node : Node_Id;
      R    : access Rung_Record) is
      
      Ln : Natural;
      H  : Natural;
      X  : Natural := 1;
      Y  : Natural;
      
      Com_Type   : Plc_Comment_Type := Reflex.Gen.Outputs.Comment_Type;
      Skip_Blank : Boolean := Reflex.Gen.Outputs.Skip_Leading_Blank_Lines;
      
      Ob : Output_Buffer := This.ob;
   begin
      Put_Line ("Emit_Comment Begin");
      --  Get Comment until the current node
      
      Reflex.Gen.Outputs.Comment_Type := No_Lead_Character;
      Reflex.Gen.Outputs.Skip_Leading_Blank_Lines := True;
      
      Reset_Buffer (This.Tmp_Ob);
      Put_Line (" 1");
      Write_Comment_Line_To_Node (This.Lit_Gen, Node, This.Tmp_Ob);
      Put_Line (" 2");
      Ln := Line_Count (This.Tmp_Ob);
      Put_Line (" 3");
      
      Reflex.Gen.Outputs.Comment_Type := Com_Type;
      Reflex.Gen.Outputs.Skip_Leading_Blank_Lines := Skip_Blank;
      
      Put_Line ("Lines Count " & Ln'Img);
      
      --  Count the Comment Hight
      
      if (Ln mod 3) /= 0 then
	 H := (Ln / 3) + 1;
      else
	 H := Ln / 3;
      end if;
      
      --  Emit Comment if we found one
      
      if Ln /= 0 and then H /= 0 then
	 
	 Write_Indent_Str 
	   (This.Comment_Ob,
	    "<textBox width=""10"" height=""" & Trim (H'Img, Both) & """>");
	 Append_Buffer (This.Comment_Ob, This.Tmp_Ob);
	 Write_Eol (This.Comment_Ob);
	 
	 Y := Get_Line_Offset (This);
	 Write_Indent_Str
	   (This.Comment_Ob, "<objPosition posX=""" & Trim (X'Img, Both) &
	      """ posY=""" & Trim (Y'Img, Both) &
	      """>" & "</objPosition>");
	 Write_Eol (This.Comment_Ob);
	 Write_Indent_Str
	   (This.Comment_Ob, "</textBox>");
	 Write_Eol (This.Comment_Ob);
	 
	 --  Put Empty lines to avoid recover code and comment
	 
	 Write_Str
	   (This.Ob, "<typeLine> <emptyLine nbRows=""" & 
	      Trim (H'Img, Both) & """></emptyLine>");
	 Write_Eol (This.Ob);
	 Write_Str (This.Ob, "</typeLine>");
	 Write_Eol (This.Ob);
	 
	 --  Update Line Offset
	 
	 Add_Line_Offset (This, H);
      end if;
      
      Put_Line ("Emit_Comment End");
   end Emit_Comment;
   
   ------------------------
   -- Emit_Begin_Network --
   ------------------------
   
   procedure Emit_Begin_Network (This : access Ladder_Emitor_Record) is
   begin
      null;
   end Emit_Begin_Network;
      
   ----------------------
   -- Emit_End_Network --
   ----------------------
   
   procedure Emit_End_Network (This : access Ladder_Emitor_Record) is
   begin
      Append_Buffer (This.Ob, This.Comment_Ob);
   end Emit_End_Network;
      
   ---------------------
   -- Emit_Begin_Rung --
   ---------------------
   
   procedure Emit_Begin_Rung
     (This : access Ladder_Emitor_Record;
      R    : access Rung_Record) is
      
      B    : access Box_Record := R.Get_Enclosing_Box;
      Node : Node_Id;
   begin
      Put_Line ("Emit_Begin_Rung Begin");
      Node := Get_Node (B);
      
      if Present (Node) then
	 Emit_Comment (This, Node, R);
      end if;
      Put_Line ("Emit_End_Rung End");
   end Emit_Begin_Rung;
      
      
      --     ---------------------
   --     -- Emit_Begin_Rung --
   --     ---------------------
   --
   --     procedure Emit_Begin_Rung
   --       (This : access Ladder_Emitor_Record;
   --        R    : in Rung) is
   --
   --        B    : Boxes :=  Get_Box (R);
   --        Node : Node_Id;
   --        X    : Natural := 1;
   --        Lab  : Boolean := False;
   --        Ob   : Output_Buffer := This.ob;
   --     begin
   --        Out_Line ("Emit_Begin_Rung");
   --
   --        Set_Call_Block (Ld, False);
   --
   --        if B /= No_Box then
   --
   --           Node := To_Node_Id (B);
   --
   --           if Node /= No_Node then
   --
   --              -- Skip line if Instruction has a label or a comment
   --              -- it helps to aerate the code.
   --              if Glips.Node.Instr.Get_Instr_Label (Node) /= No_Node or else
   --                Get_Comment (R) /= No_Name then
   --
   --                 Write_Line
   --                   (Ob, "<typeLine> <emptyLine nbRows=""1""> </emptyLine>");
   --                 Write_Line (Ob, "</typeLine>");
   --
   --                 Add_Line_Offset (Ld, 1);
   --              end if;
   --
   --              if Glips.Node.Instr.Get_Instr_Label (Node) /= No_Node then
   --
   --
   --                 Write_Line
   --                   (Ob, "<typeLine>");
   --                 Write_Indent (Ob);
   --
   --                 declare
   --                    Label : String :=
   --                      Format (Glips.Node.Instr.Get_Instr_Label (Node));
   --                 begin
   --
   --                    Write_Line
   --                      (Ob, "<labelCell label=""" & Label & """>");
   --                 exception
   --                    when E: others =>
   --                       Debug.Unknown_Exception(E, "Emit_Begin_Rung: Bad label");
   --                       raise;
   --                 end;
   --
   --                 Write_Indent (Ob);
   --
   --                 Write_Line
   --                   (Ob, "<objPosition posX=""0"" " &
   --                      "posY=""" & Trim (Get_Line_Offset(Ld)'Img, Both) &
   --                      """></objPosition>");
   --
   --                 Dec_Indent (Ob);
   --
   --                 Write_Line
   --                   (Ob, "</labelCell>");
   --
   --                 Write_Line
   --                   (Ob, "<emptyCell nbCells=""10""></emptyCell>");
   --
   --                 Dec_Indent (Ob);
   --                 Write_Line
   --                   (Ob, "</typeLine>");
   --
   --                 Lab := True;
   --              end if;
   --           end if;
   --        end if;
   --
   --        -- Emit the comment if any.
   --
   --        if Get_Comment (R) /= No_Name then
   --           Emit_Comment (Ld, R, Lab);
   --        end if;
   --
   --        if Lab then
   --           Add_Line_Offset (Ld, 1);
   --        end if;
   --
   --        Write_Indent (Ob);
   --
   --        Out_Line ("Emit_Begin_Rung End");
   --     end Emit_Begin_Rung;
   --
   -------------------
   -- Emit_End_Rung --
   -------------------
   
   procedure Emit_End_Rung
     (This : access Ladder_Emitor_Record;
      R    : access Rung_Record) is
      
      Ob   : Output_Buffer := This.ob;
   begin
      Dec_Indent (Ob);
   end Emit_End_Rung;

   ---------------------
   -- Emit_Begin_Line --
   ---------------------

   procedure Emit_Begin_Line (This : access Ladder_Emitor_Record) is

      Ob : Output_Buffer := This.Ob;
   begin
      Write_Indent_Str (Ob, "<typeLine>");
      Indent_Begin (Ob);
      Write_Eol (Ob);
   end Emit_Begin_Line;

   -------------------
   -- Emit_End_Line --
   -------------------

   procedure Emit_End_Line (This : access Ladder_Emitor_Record) is

      Ob : Output_Buffer := This.Ob;
   begin
      Indent_End (Ob);
      Write_Indent_Str (Ob, "</typeLine>");
      Write_Eol (Ob);
      
      Add_Line_Offset (This, 1);
   end Emit_End_Line;

   -----------------------
   -- Emit_Simple_Vlink --
   -----------------------

   procedure Emit_Simple_Vlink
     (This : access Ladder_Emitor_Record) is

      Ob : Output_Buffer := This.Ob;
   begin
      Write_Indent_Str (Ob, "<VLink></VLink>");
      Write_Eol (Ob);
   end Emit_Simple_Vlink;

   ---------------------
   -- Emit_Open_Vlink --
   ---------------------

   procedure Emit_Open_Vlink
     (This : access Ladder_Emitor_Record) is

      Ob : Output_Buffer := This.Ob;
   begin
      Write_Line (Ob, "<shortCircuit>");
      Write_Indent (Ob);
      Write_Line (Ob, "<VLink></VLink>");
   end Emit_Open_Vlink;

   ----------------------
   -- Emit_Close_Vlink --
   ----------------------

   procedure Emit_Close_Vlink
     (This : access Ladder_Emitor_Record) is

      Ob : Output_Buffer := This.Ob;
   begin
      Dec_Indent (Ob);
      Write_Line (Ob, "</shortCircuit>");
   end Emit_Close_Vlink;

   ----------------
   -- Emit_Empty --
   ----------------

   procedure Emit_Empty
     (This : access Ladder_Emitor_Record;
      Nb   : in Natural) is

      Ob : Output_Buffer := This.Ob;
      S  : String := Integer_To_String (Nb);
   begin
      Write_Indent_Str
        (Ob, "<emptyCell nbCells=""" & S & """></emptyCell>");
      Write_Eol (Ob);
   end Emit_Empty;

   ----------------
   -- Emit_Hlink --
   ----------------

   procedure Emit_Hlink
     (This : access Ladder_Emitor_Record;
      Nb   : in Natural;
      Vlink : Boolean := False) is

      Ob : Output_Buffer := This.Ob;
      S  : String := Integer_To_String (Nb);
   begin
      if Vlink then
         Write_Indent_Str (Ob, "<shortCircuit>");
         Write_Eol (Ob);

         Indent_Begin (Ob);
         Write_Indent_Str (Ob, "<VLink></VLink>");
         Write_Eol (Ob);
         Indent_End (Ob);
      end if;

      Write_Indent_Str
        (Ob, "<HLink nbCells=""" & S & """></HLink>");
      Write_Eol (Ob);

      if Vlink then
         Write_Indent_Str (Ob, "</shortCircuit>");
         Write_Eol (Ob);
      end if;
   end Emit_Hlink;

   -----------------------
   -- Emit_Open_Contact --
   -----------------------

   procedure Emit_Open_Contact
     (This : access Ladder_Emitor_Record;
      B    : access Box_Record'Class) is

      Ob : Output_Buffer := This.Ob;
   begin
      if B.Get_Has_Vlink then
         Write_Indent_Str (Ob, "<shortCircuit>");
         Write_Eol (Ob);

         Indent_Begin (Ob);
         Write_Indent_Str (Ob, "<VLink></VLink>");
         Write_Eol (Ob);
         Indent_End (Ob);
      end if;

      Write_Indent_Str
        (Ob,
         "<contact typeContact=""openContact"" contactVariableName=""");
      Generate_Literal_Expression (This.Lit_Gen, B.Get_Node, Ob);

      Write_Str (Ob, """></contact>");
      Write_Eol (Ob);

      if B.Get_Has_Vlink then
         Write_Indent_Str (Ob, "</shortCircuit>");
         Write_Eol (Ob);
      end if;
   end Emit_Open_Contact;

   -------------------------
   -- Emit_Closed_Contact --
   -------------------------

   procedure Emit_Closed_Contact
     (This : access Ladder_Emitor_Record;
      B    : access Box_Record'Class) is

      Ob : Output_Buffer := This.Ob;
   begin
      if B.Get_Has_Vlink then
         Write_Indent_Str (Ob, "<shortCircuit>");
         Write_Eol (Ob);

         Indent_Begin (Ob);
         Write_Indent_Str (Ob, "<VLink></VLink>");
         Write_Eol (Ob);
         Indent_End (Ob);
      end if;

      Write_Indent_Str
        (Ob,
         "<contact typeContact=""closedContact"" contactVariableName=""");
      Generate_Literal_Expression (This.Lit_Gen, Right_Opnd (B.Get_Node), Ob);

      Write_Str (Ob, """></contact>");
      Write_Eol (Ob);

      if B.Get_Has_Vlink then
         Write_Indent_Str (Ob, "</shortCircuit>");
         Write_Eol (Ob);
      end if;
   end Emit_Closed_Contact;


   --     ------------------------
   --     -- Emit_Raise_Contact --
   --     ------------------------
   --
   --     procedure Emit_Raise_Contact
   --       (This : access Ladder_Emitor_Record;
   --        B    : in Boxes) is
   --
   --        Node : Node_Id := To_Node_Id(B);
   --        Ob : Output_Buffer := This.ob;
   --     begin
   --        Out_Line ("Emit_Raise_Contact");
   --        Write_Line
   --          (Ob,
   --           "<contact typeContact=""PContact"" contactVariableName="""
   --           & Get_Contact_Name (Ld, Node) & """></contact>");
   --
   --        Out_Line ("Emit_Raise_Contact End");
   --     end Emit_Raise_Contact;


   --     --------------------------
   --     -- Emit_Falling_Contact --
   --     --------------------------
   --
   --     procedure Emit_Falling_Contact
   --       (This : access Ladder_Emitor_Record;
   --        B    : in Boxes) is
   --
   --        Node : Node_Id := To_Node_Id(B);
   --        Ob : Output_Buffer := This.ob;
   --     begin
   --        Out_Line ("Emit_Falling_Contact");
   --        Write_Line
   --          (Ob,
   --           "<contact typeContact=""NContact"" contactVariableName="""
   --           & Get_Contact_Name (Ld, Node) & """></contact>");
   --
   --        Out_Line ("Emit_Falling_Contact End");
   --     end Emit_Falling_Contact;

   ------------------------
   -- Emit_Compare_Block --
   ------------------------

   procedure Emit_Compare_Block
     (This : access Ladder_Emitor_Record;
      B    : access Box_Record'Class) is

      Ob  : Output_Buffer := This.Ob;
   begin
      if B.Get_Has_Vlink then
         Write_Indent_Str (Ob, "<shortCircuit>");
         Write_Eol (Ob);

         Indent_Begin (Ob);
         Write_Indent_Str (Ob, "<VLink></VLink>");
         Write_Eol (Ob);
         Indent_End (Ob);
      end if;

      Write_Indent_Str (Ob, "<compareBlock width=""2"">");
      Write_Eol (Ob);

      Indent_Begin (Ob);
      Write_Indent_Str (Ob,  "<expression>");

      --  If operator is a '<' operator, we need to generate "&lt;" in ladder
      --  Doesn't work for expression like (X1 < X2) < (X3 < X4)
      --  May be modify Literal generator??

      Indent_Begin (Ob);
      Write_Eol (Ob);
      if Nkind (B.Get_Node) = N_Op_Lt then
         Write_Indent (Ob);
         Generate_Literal_Expression
	   (This.Lit_Gen, Left_Opnd (B.Get_Node), Ob);
         Write_Str (Ob," &lt; ");
         Generate_Literal_Expression
	   (This.Lit_Gen, Right_Opnd (B.Get_Node), Ob);
	 
      elsif Nkind (B.Get_Node) = N_Op_Le then
         Write_Indent (Ob);
         Generate_Literal_Expression
	   (This.Lit_Gen, Left_Opnd (B.Get_Node), Ob);
         Write_Str (Ob," &lt;= ");
         Generate_Literal_Expression
	   (This.Lit_Gen, Right_Opnd (B.Get_Node), Ob);

      elsif Nkind (B.Get_Node) = N_Op_Gt then
         Write_Indent (Ob);
         Generate_Literal_Expression
	   (This.Lit_Gen, Left_Opnd (B.Get_Node), Ob);
         Write_Str (Ob," &gt; ");
         Generate_Literal_Expression
	   (This.Lit_Gen, Right_Opnd (B.Get_Node), Ob);

      elsif Nkind (B.Get_Node) = N_Op_Ge then
         Write_Indent (Ob);
         Generate_Literal_Expression
	   (This.Lit_Gen, Left_Opnd (B.Get_Node), Ob);
         Write_Str (Ob," &gt;= ");
         Generate_Literal_Expression
	   (This.Lit_Gen, Right_Opnd (B.Get_Node), Ob);

      else
         Write_Indent (Ob);
         Generate_Literal_Expression (This.Lit_Gen, B.Get_Node, Ob);
      end if;
      Write_Eol (Ob);
      Indent_End (Ob);

      Write_Indent_Str (Ob, "</expression>");
      Indent_End (Ob);
      Write_Eol (Ob);

      Write_Indent_Str (Ob, "</compareBlock>");
      Write_Eol (Ob);

      if B.Get_Has_Vlink then
         Write_Indent_Str (Ob, "</shortCircuit>");
         Write_Eol (Ob);
      end if;
   end Emit_Compare_Block;

   ---------------
   -- Emit_Coil --
   ---------------

   procedure Emit_Coil
     (This : access Ladder_Emitor_Record;
      B    : access Box_Record'Class) is

      Ob : Output_Buffer := This.Ob;
   begin
      if B.Get_Has_Vlink then
         Write_Indent_Str (Ob, "<shortCircuit>");
         Write_Eol (Ob);

         Indent_Begin (Ob);
         Write_Indent_Str (Ob, "<VLink></VLink>");
         Write_Eol (Ob);
         Indent_End (Ob);
      end if;

      Write_Indent_Str
        (Ob,
         "<coil typeCoil=""coil"" coilVariableName=""");
      Generate_Literal_Expression (This.Lit_Gen, B.Get_Node, Ob);

      Write_Str (Ob, """></coil>");
      Write_Eol (Ob);

      if B.Get_Has_Vlink then
         Write_Indent_Str (Ob, "</shortCircuit>");
         Write_Eol (Ob);
      end if;
   end Emit_Coil;

   -------------------
   -- Emit_Not_Coil --
   -------------------

   procedure Emit_Not_Coil
     (This : access Ladder_Emitor_Record;
      B    : access Box_Record'Class) is

      Ob : Output_Buffer := This.Ob;
   begin
      if B.Get_Has_Vlink then
         Write_Indent_Str (Ob, "<shortCircuit>");
         Write_Eol (Ob);

         Indent_Begin (Ob);
         Write_Indent_Str (Ob, "<VLink></VLink>");
         Write_Eol (Ob);
         Indent_End (Ob);
      end if;

      Write_Indent_Str
        (Ob,
         "<coil typeCoil=""notCoil"" coilVariableName=""");
      Generate_Literal_Expression (This.Lit_Gen, B.Get_Node, Ob);

      Write_Str (Ob, """></coil>");
      Write_Eol (Ob);

      if B.Get_Has_Vlink then
         Write_Indent_Str (Ob, "</shortCircuit>");
         Write_Eol (Ob);
      end if;
   end Emit_Not_Coil;

   -------------------
   -- Emit_Set_Coil --
   -------------------

   procedure Emit_Set_Coil
     (This : access Ladder_Emitor_Record;
      B    : access Box_Record'Class) is

      Ob : Output_Buffer := This.Ob;
   begin
      if B.Get_Has_Vlink then
         Write_Indent_Str (Ob, "<shortCircuit>");
         Write_Eol (Ob);

         Indent_Begin (Ob);
         Write_Indent_Str (Ob, "<VLink></VLink>");
         Write_Eol (Ob);
         Indent_End (Ob);
      end if;

      Write_Indent_Str
        (Ob,
         "<coil typeCoil=""setCoil"" coilVariableName=""");
      Generate_Literal_Expression (This.Lit_Gen, B.Get_Node, Ob);

      Write_Str (Ob, """></coil>");
      Write_Eol (Ob);

      if B.Get_Has_Vlink then
         Write_Indent_Str (Ob, "</shortCircuit>");
         Write_Eol (Ob);
      end if;
   end Emit_Set_Coil;

   ---------------------
   -- Emit_Reset_Coil --
   ---------------------

   procedure Emit_Reset_Coil
     (This : access Ladder_Emitor_Record;
      B    : access Box_Record'Class) is

      Ob : Output_Buffer := This.Ob;
   begin
      if B.Get_Has_Vlink then
         Write_Indent_Str (Ob, "<shortCircuit>");
         Write_Eol (Ob);

         Indent_Begin (Ob);
         Write_Indent_Str (Ob, "<VLink></VLink>");
         Write_Eol (Ob);
         Indent_End (Ob);
      end if;

      Write_Indent_Str
        (Ob,
         "<coil typeCoil=""resetCoil"" coilVariableName=""");
      Generate_Literal_Expression (This.Lit_Gen, B.Get_Node, Ob);

      Write_Str (Ob, """></coil>");
      Write_Eol (Ob);

      if B.Get_Has_Vlink then
         Write_Indent_Str (Ob, "</shortCircuit>");
         Write_Eol (Ob);
      end if;

   end Emit_Reset_Coil;

   ---------------
   -- Emit_Jump --
   ---------------

   procedure Emit_Jump
     (This : access Ladder_Emitor_Record;
      B    : access Box_Record'Class) is

      Ob : Output_Buffer := This.Ob;
   begin
      Write_Indent_Str
        (Ob,
         "<control typeControl=""jumpCoil"" label=""");
      Generate_Literal_Expression (This.Lit_Gen, Name (B.Get_Node), Ob);

      Write_Str (Ob, """></control>");
      Write_Eol (Ob);

   end Emit_Jump;

   ----------------------
   -- Emit_Return_Coil --
   ----------------------

   procedure Emit_Return_Coil
     (This : access Ladder_Emitor_Record;
      B    : access Box_Record'Class ) is

      Ob : Output_Buffer := This.Ob;
   begin
      Write_Line (Ob, "<control typeControl=""retCoil""></control>");
   end Emit_Return_Coil;

   ------------------------
   -- Emit_Operate_Block --
   ------------------------

   procedure Emit_Operate_Block
     (This : access Ladder_Emitor_Record;
      B    : access Box_Record'Class) is

      Ob : Output_Buffer := This.Ob;
   begin
      Write_Indent_Str (Ob, "<operateBlock width=""4"">");
      Write_Eol (Ob);

      Indent_Begin (Ob);
      Write_Indent_Str (Ob, "<statement>");

      Indent_Begin (Ob);
      Write_Eol (Ob);
      Generate_Literal_Expression (This.Lit_Gen, B.Get_Node, Ob);
      Indent_End (Ob);

      Write_Indent_Str (Ob, "</statement>");
      Indent_End (Ob);
      Write_Eol (Ob);

      Write_Indent_Str (Ob, "</operateBlock>");
      Write_Eol (Ob);
   end Emit_Operate_Block;

   ----------------
   -- Emit_Label --
   ----------------

   procedure Emit_Label
     (This : access Ladder_Emitor_Record;
      B    : access Box_Record'Class;
      Y    : Natural) is

      Ob : Output_Buffer := This.Ob;
   begin
      Write_Indent_Str (Ob, "<labelCell label=""");
      Generate_Literal_Expression (This.Lit_Gen, Identifier (B.Get_Node), Ob);

      Write_Str (Ob, """>");

      Write_Eol (Ob);
      Indent_Begin (Ob);

      Write_Indent_Str
        (Ob, "<objPosition posX="""&
           Integer_To_String (B.Get_Xabs) & """ posY="""&
           Integer_To_String (Y) & """></objPosition>");

      Indent_End (Ob);
      Write_Eol (Ob);

      Write_Indent_Str (Ob, "</labelCell>");
      Write_Eol (Ob);
      null;
   end Emit_Label;
   
end Reflex.Boxes.Ladder_Emitor;
