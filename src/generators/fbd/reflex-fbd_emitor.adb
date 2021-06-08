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

with Artics.Utils; use Artics.Utils;

with Urealp; use Urealp;
with Sinfo;  use Sinfo;
with Atree;  use Atree;
with Namet;  use Namet;
with Einfo;  use Einfo;
with Uintp;  use Uintp;

with Reflex.Vertex_Value; use Reflex.Vertex_Value;
with Reflex_Options;      use Reflex_Options;

package body Reflex.Fbd_Emitor is

   -----------------------
   -- New_Fbd_Emitor --
   -----------------------

   function New_Fbd_Emitor return Fbd_Emitor_Ptr is
      This : Fbd_Emitor_Ptr :=
        new Fbd_Emitor_Record'(No_Fbd_Emitor_Record);
   begin
      This.Ob := New_Output_Buffer;
      return This;
   end New_Fbd_Emitor;

   ------------------------
   -- Free_Fbd_Emitor --
   ------------------------

   procedure Free_Fbd_Emitor (This : in out Fbd_Emitor_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Fbd_Emitor_Record, Fbd_Emitor_Ptr);
   begin
      if This /= null then
         Free (This);
      end if;
   end Free_Fbd_Emitor;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : access Fbd_Emitor_Record) is
   begin
      null;
   end Initialize;

   -----------------------
   -- Get_Output_Buffer --
   -----------------------

   function Get_Output_Buffer
     (This : access Fbd_Emitor_Record) return Output_Buffer is
   begin
      return This.Ob;
   end Get_Output_Buffer;

   -----------------------
   -- Set_Output_Buffer --
   -----------------------

   procedure Set_Output_Buffer
     (This : access Fbd_Emitor_Record;
      Ob   : Output_Buffer) is
   begin
      This.Ob := Ob;
   end Set_Output_Buffer;

   -----------------
   -- Emit_Header --
   -----------------

   procedure Emit_Header (This : access Fbd_Emitor_Record) is
      Ob : Output_Buffer := This.Ob;
   begin
      Write_Indent_Str
        (Ob, "<FBDSource nbRows=""" &
           Integer_To_String (Max_Unity_Fbd_Vertical / 10) &

           """ nbColumns=""" &
           Integer_To_String (Max_Unity_Fbd_Horizontal / 10) &
           """>");

      Indent_Begin (Ob);
      Write_Eol (Ob);

      Write_Indent_Str (Ob, "<networkFBD>");
      Indent_Begin (Ob);
      Write_Eol (Ob);
   end Emit_Header;

   -----------------
   -- Emit_Tailer --
   -----------------

   procedure Emit_Tailer (This : access Fbd_Emitor_Record) is
      Ob : Output_Buffer := This.Ob;
   begin

      Indent_End (Ob);
      Write_Indent_Str (Ob, "</networkFBD>");
      Write_Eol (Ob);

      Indent_End (Ob);
      Write_Indent_Str (Ob, "</FBDSource>");
      Write_Eol (Ob);
   end Emit_Tailer;

   --  Ffb blocks emitors

   --------------------
   -- Emit_Ffb_Block --
   --------------------

   procedure Emit_Ffb_Block
     (This : access Fbd_Emitor_Record;
      Cell : access Cell_Record'Class) is
   begin
      Emit_Ffb_Header (This, Cell);
      Emit_Obj_Position (This, Cell);
      Emit_Description_Ffb (This, Cell);
   end Emit_Ffb_Block;

   ---------------------
   -- Emit_Ffb_Header --
   ---------------------

   procedure Emit_Ffb_Header
     (This : access Fbd_Emitor_Record;
      Cell : access Cell_Record'Class) is

      Ob         : Output_Buffer := This.Ob;
      Cell_Value : Vertex_Value_Ptr := Cell.Get_Value;
      EnEno      : Boolean := False;
      Pin_Nb     : Integer := 0;
      Width      : Integer;
      Height     : Integer;
   begin
      Width  := Integer (Cell.Get_Geometry.Get_Width);
      Height := Integer (Cell.Get_Geometry.Get_Height);

      Write_Indent_Str
        (Ob, "<FFBBlock instanceName=""" &
           Cell.Get_Id &

           """ typeName=""" &
           Determinate_Type_Name (Cell_Value.Get_Node) &

           """ additionnalPinNumber=""" &
           Integer_To_String (Pin_Nb) &

           """ enEnO=""" &
           Boolean_To_String (EnEno) &

           """ width=""" &
           Integer_To_String (Width) &

           """ height=""" &
           Integer_To_String (Height) &
           """>");

      Write_Eol (Ob);
   end Emit_Ffb_Header;

   --------------------------
   -- Emit_Description_Ffb --
   --------------------------

   procedure Emit_Description_Ffb
     (This : access Fbd_Emitor_Record;
      Cell : access Cell_Record'Class) is

      Ob          : Output_Buffer := This.Ob;
      Cell_Value  : Vertex_Value_Ptr := Cell.Get_Value;
      Param_Value : Vertex_Value_Ptr;
      Nb          : Integer;
      use Cells_Lists;
   begin
      Write_Indent_Str
        (Ob, "<descriptionFFB execAfter="""">");
      Write_Eol (Ob);
      Indent_Begin (Ob);

      --  Emit Inputs Variables

      Write_Indent_Str
        (Ob,"<inputVariable invertedPin=""false""" &
           " formalParameter=""EN""></inputVariable>");
      Write_Eol (Ob);

      Nb := 0;
      for C of Cell_Value.Get_Vertexs_In loop
         Nb := Nb + 1;
         Param_Value := C.Get_Value;

         Write_Indent_Str
           (Ob, "<inputVariable invertedPin=""" &
              Boolean_To_String (Param_Value.Get_Is_Negate_Vertex) &

              """ formalParameter=""" &
              Get_Name_String (Vertex_Value_Ptr (C.Get_Value).Get_Formal_Name) &

              Determinate_Effectve_Param (Param_Value) &
              """></inputVariable>");
         Write_Eol (Ob);
      end loop;

      --  Emit Outputs Variables

      Write_Indent_Str
        (Ob,"<outputVariable invertedPin=""false""" &
           " formalParameter=""ENO""></outputVariable>");
      Write_Eol (Ob);

      Nb := 0;
      for C of Cell_Value.Get_Vertexs_Out loop
         Nb := Nb + 1;
         Param_Value := C.Get_Value;

         Write_Indent_Str
           (Ob, "<outputVariable invertedPin=""" &
              Boolean_To_String (Param_Value.Get_Is_Negate_Vertex) &

              """ formalParameter=""" &
              Get_Name_String (Vertex_Value_Ptr (C.Get_Value).Get_Formal_Name) &

              Determinate_Effectve_Param (Param_Value) &
              """></outputVariable>");
         Write_Eol (Ob);
      end loop;

      Indent_End (Ob);
      Write_Indent_Str
        (Ob, "</descriptionFFB>");
      Write_Eol (Ob);

      Indent_End (Ob);
      Write_Indent_Str
        (Ob, "</FFBBlock>");
      Write_Eol (Ob);
   end Emit_Description_Ffb;

   ------------------
   -- Emit_Link_Fb --
   ------------------

   procedure Emit_Link_Fb
     (This : access Fbd_Emitor_Record;
      Cell : access Cell_Record'Class) is

      Ob : Output_Buffer := This.Ob;
   begin
      Emit_Link_Source (This, Cell);
      Emit_Link_Destination (This, Cell);

      --emit_grid_obj_pos (this, cell);

      Indent_End (Ob);
      Write_Indent_Str (Ob, "</linkFB>");
      Write_Eol (Ob);
   end Emit_Link_Fb;

   ----------------------
   -- Emit_Link_Source --
   ----------------------

   procedure Emit_Link_Source
     (This : access Fbd_Emitor_Record;
      Cell : access Cell_Record'Class) is

      Src_Cell     : access Cell_Record := Cell.Get_Source;
      Src_Value    : Vertex_Value_Ptr   := Src_Cell.Get_Value;
      Ob           : Output_Buffer      := This.Ob;
      Cell_To_Link : access Cell_Record := Vertex_Value_Ptr
        (Src_Cell.Get_Parent.Get_Value).Get_Cell_To_Link;
   begin

      if Src_Value.Get_Vertex_Kind = Formal_Out_Vertex then
         Write_Indent_Str
           (Ob, "<linkFB>");
         Write_Eol (Ob);

         Indent_Begin (Ob);
         Write_Indent_Str
           (Ob, "<linkSource parentObjectName=""" &
              Cell_To_Link.Get_Id &

              """ pinName=""" &
              Get_Name_String (Src_Value.Get_Formal_Name) &
              """>");
         Write_Eol (Ob);

         Emit_Obj_Position (This, Src_Cell);
         Indent_End (Ob);

         Write_Indent_Str
           (Ob, "</linkSource>");
         Write_Eol (Ob);
      end if;
   end Emit_Link_Source;

   ---------------------------
   -- Emit_Link_destination --
   ---------------------------

   procedure Emit_Link_Destination
     (This : access Fbd_Emitor_Record;
      Cell : access Cell_Record'Class) is

      Trg_Cell     : access Cell_Record := Cell.Get_Target;
      Trg_Value    : Vertex_Value_Ptr := Trg_Cell.Get_Value;
      Ob           : Output_Buffer := This.Ob;
      Cell_To_Link : access Cell_Record := Vertex_Value_Ptr
        (Trg_Cell.Get_Parent.Get_Value).Get_Cell_To_Link;
   begin
      if Trg_Value.Get_Vertex_Kind = Formal_In_Vertex then
         Write_Indent_Str
           (Ob, "<linkDestination parentObjectName=""" &
              Cell_To_Link.Get_Id &

              """ pinName=""" &
              Get_Name_String (Trg_Value.Get_Formal_Name) &
              """>");
         Write_Eol (Ob);

         Indent_End (Ob);
         Emit_Obj_Position (This, Trg_Cell);

         Write_Indent_Str
           (Ob, "</linkDestination>");
         Write_Eol (Ob);
      end if;
   end Emit_Link_Destination;

   --------------------------
   -- Emit_Grid_Obj_Pos --
   --------------------------

   procedure Emit_Grid_Obj_Pos
     (This : access Fbd_Emitor_Record;
      Cell : access Cell_Record'Class) is

      Ob : Output_Buffer := This.Ob;
   begin
      Write_Indent_Str
        (Ob, "<gridObjPosition posX=""" &
           """ posY=""" &
         -- &
           """></gridObjPosition>");
      Write_Eol (Ob);
   end Emit_Grid_Obj_Pos;

   -----------------------
   -- Emit_Obj_position --
   -----------------------

   procedure Emit_Obj_Position
     (This : access Fbd_Emitor_Record;
      Cell : access Cell_Record'Class) is

      Cell_Value : Vertex_Value_Ptr := Cell.Get_Value;
      Ob         : Output_Buffer := This.Ob;
   begin
      Indent_Begin (Ob);
      Write_Indent_Str
        (Ob, "<objPosition posX=""" &
           Integer_To_String (Integer (Cell.Get_Geometry.Get_X)) &

           """ posY=""" &
           Integer_To_String (Integer (Cell.Get_Geometry.Get_Y)) &
           """></objPosition>");
      Write_Eol (Ob);
   end Emit_Obj_Position;

   ---------------------------
   -- Determinate_Type_Name --
   ---------------------------

   function Determinate_Type_Name (Node : Node_Id) return String is
   begin

      --  Determinate type of current operand for ffb blocks

      case Nkind (Node) is
         when N_Op_Add =>
            return "ADD" ;

         when N_Op_Subtract =>
            return "SUB" ;

         when N_Op_Divide =>
            return "DIV" ;

         when N_Op_Multiply =>
            return "MUL" ;

         when N_Op_And =>
            return "AND" ;

         when N_Op_Or =>
            return "OR" ;

         when N_Op_Eq =>
            return "EQ" ;

         when N_Op_Ne =>
            return "NE" ;

         when N_Op_Ge =>
            return "GE" ;

         when N_Op_Gt =>
            return "GT" ;

         when N_Op_Le =>
            return "LE" ;

         when N_Op_Lt =>
            return "LT" ;

         when N_Op_Mod =>
            return "MOD" ;

         when N_Op_Expon =>
            return "EXP" ;

         when N_Op_Rem =>
            return "REM" ;

         when N_Op_Xor =>
            return "XOR" ;

         when N_Op_Abs =>
            return "ABS" ;

         when N_Op_Plus =>
            return "";

         when N_Op_Minus =>
            return "";

         when N_Procedure_Call_Statement =>
            return Get_Name_String (Chars (Name (Node)));

         when others =>
            return "error";
      end case;

   end Determinate_Type_Name;

   --------------------------------
   -- Determinate_Effectve_Param --
   --------------------------------

   function Determinate_Effectve_Param
     (Param_Value : Vertex_Value_Ptr) return String is
      Param : Node_Id := Param_Value.Get_Effective_Param;
   begin

      --  Determinate, if exists value of effective parameter for input and
      --  output in fbd blocks

      if Param /= Empty then
         if Nkind (Param) = N_Identifier then
            return """ effectiveParameter=""" & Get_Name_String (Chars (Param));
         elsif Nkind (Param) = N_Real_Literal then

            --------------------------------------------------------------------
            -- UR_To_Float??
            return """ effectiveParameter="""
              & Integer_To_String (Integer (UI_To_Int
                                   (UR_To_Uint ((Realval (Param))))));
            --------------------------------------------------------------------

         elsif Nkind (Param) = N_Integer_Literal then
            return """ effectiveParameter="""
              & Integer_To_String (Integer (UI_To_Int ((Intval (Param)))));
         else
            return "";
         end if;
      else
         return "";
      end if;

   end Determinate_Effectve_Param;

   -------------------
   -- Emit_Fbd_Decl --
   -------------------

   procedure Emit_Fbd_Decl
     (This : access Fbd_Emitor_Record;
      Cell : access Cell_Record'Class) is

      EnEno      : Boolean := False;
      Pin_Nb     : Integer := 0;
      Ob          : Output_Buffer := This.Ob;
      Cell_Value  : Vertex_Value_Ptr := Cell.Get_Value;
      Param_Value : Vertex_Value_Ptr;
      Nb          : Integer;
      use Cells_Lists;
   begin
      Write_Indent_Str
        (Ob, "<FBSource nameOfFBType=""" &
           Determinate_Type_Name (Cell_Value.Get_Node) &

           """ version= 0.1""" &

           """ dateTime=""dt#2018-07-27-13:43:43"">");

      --  Emit input variables

      Indent_Begin (Ob);
      Write_Indent_Str (Ob, "<inputParameters>");
      Write_Eol (Ob);

      Indent_Begin (Ob);
      Nb := 0;
      for C of Cell_Value.Get_Vertexs_In loop
         Nb := Nb + 1;
         Param_Value := C.Get_Value;

         Write_Indent_Str
           (Ob, "<variables name=""" &
              Get_Name_String (Vertex_Value_Ptr (C.Get_Value).Get_Formal_Name) &

              """ typeName=""" &
              Determinate_Type (C) & """" );
         Write_Eol (Ob);

         Indent_Begin (Ob);
         Write_Indent_Str
           (Ob, "<attribute name=""PositionPin"" value=""" &
              Nb'Img & """" );
         Write_Eol (Ob);
         Indent_End (Ob);

         Write_Indent_Str
           (Ob, "</variables>");

         Write_Eol (Ob);
      end loop;

      Indent_End (Ob);
      Write_Indent_Str
        (Ob, "</inputParameters>");
      Write_Eol (Ob);

      --  Emit output variables

      Write_Indent_Str (Ob, "<outputParameters>");
      Write_Eol (Ob);

      Indent_Begin (Ob);
      Nb := 0;
      for C of Cell_Value.Get_Vertexs_Out loop
         Nb := Nb + 1;
         Param_Value := C.Get_Value;

         Write_Indent_Str
           (Ob, "<variables name=""" &
              Get_Name_String (Vertex_Value_Ptr (C.Get_Value).Get_Formal_Name) &

              """ typeName=""" &
              Determinate_Type (C) & """" );
         Write_Eol (Ob);

         Indent_Begin (Ob);
         Write_Indent_Str
           (Ob, "<attribute name=""PositionPin"" value=""" &
              Nb'Img & """" );
         Write_Eol (Ob);
         Indent_End (Ob);

         Write_Indent_Str
           (Ob, "</variables>");

         Write_Eol (Ob);
      end loop;

      Indent_End (Ob);
      Write_Indent_Str
        (Ob, "</outputParameters>");
      Write_Eol (Ob);

      Indent_End (Ob);
      Write_Indent_Str
        (Ob, "</FBSource>");
      Write_Eol (Ob);

   end Emit_Fbd_Decl;

   ----------------------
   -- Determinate_Type --
   ----------------------

   function Determinate_Type
     (Cell : access Cell_Record'Class) return String is

      Cell_Value : Vertex_Value_Ptr := Cell.Get_Value;
      Node       : Node_Id := Cell_Value.Get_Node;

      use Cells_Lists;
   begin
      if Is_Boolean_Type (Node) then
         return "BOOL";
      elsif Is_Integer_Type (Node) then
         return "INT";
      elsif Is_Floating_Point_Type (Node) then
         return "REAL";
      elsif Is_String_Type (Node) then
         return "STRING";
      else
         return "ERROR";
      end if;
   end Determinate_Type;

end Reflex.Fbd_Emitor;
