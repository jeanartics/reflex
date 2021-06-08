------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
--                                                                          --
-- Reflex is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public Licensea as published  by the Free Soft- --
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

with Ada.Text_Io;use Ada.Text_Io;

with Ada.Unchecked_Deallocation;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.Case_Util; use GNAT.Case_Util;

with Namet;    use Namet;
with Sinfo;    use Sinfo;
with Einfo;    use Einfo;

with Output;

with Rtree;    use Rtree;
with Rinfo;    use Rinfo;
with Nconv;    use Nconv;
with Rlists;   use Rlists;
with Nassoc;
with Opt;      use Opt;

with Gls; use Gls;

package body Graphs_Svg is


   XUnit : constant Integer := 100;
   YUnit : constant Integer := 100;


   Gap_X_Body         : constant Integer := 1*Xunit;
   Gap_X_Body_Handler : constant Integer := 1*Xunit;
   Gap_X_Handler      : constant Integer := 1*Xunit;

   Gap_Y_Body    : constant Integer := 1*Yunit;
   Gap_Y_Handler : constant Integer := 2*Yunit;

   Gap_X_Select : constant Integer := 0; -- 1*Xunit;
   Gap_Y_Select  : constant Integer := 1*Yunit;
   Gap_Select_Alternatives  : constant Integer := 1*Yunit;

   Decal_Abort : constant Integer := 0; -- -1*Xunit;
   Decal_Select : constant Integer := 0; -- -1*Xunit;
   Decal_Fork  : constant Integer := 0; --Xunit;


   -- Pour le dessin, l'unite est la cellule. Y represente le nombre de
   -- cellules depuis le debut du rung. Si on a un fonction block

   use Ascii;
   Crlf : String := "" & Ascii.Lf;
   -- the CR LF sequence.

   Svg_File : File_Type;
   -- The file where to emit the svg ladder graphic.

   Svg_File_Open : Boolean := False;
   -- True whene the file has beeen opened.

   Svg_Header_Fisrt : String :=
     "<?xml version=""1.0"" encoding=""UTF-8"" standalone=""no""?>" & Lf &
     "<!DOCTYPE svg PUBLIC ""-//W3C//DTD SVG 1.0//EN"" " & Lf &
     "    ""http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd"">" & Lf &
     "<svg contentScriptType=""text/ecmascript""  " & Lf &
     "   xmlns:xlink=""http://www.w3.org/1999/xlink"" " & Lf &
     "   zoomAndPan=""magnify"" contentStyleType=""text/css"" ";

   Svg_Header_Last : String := "  preserveAspectRatio=""xMidYMid meet"" "
     & Lf &
     "   xmlns=""http://www.w3.org/2000/svg"" version=""1.0"" >";

   Svg_Header : String :=
     "<?xml version=""1.0"" encoding=""UTF-8"" standalone=""no""?>" & Lf &
     "<!DOCTYPE svg PUBLIC ""-//W3C//DTD SVG 1.0//EN"" " & Lf &
     "    ""http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd"">" & Lf &
     "<svg contentScriptType=""text/ecmascript"" width=""1024"" " & Lf &
     "   xmlns:xlink=""http://www.w3.org/1999/xlink"" " & Lf &
     "   zoomAndPan=""magnify"" contentStyleType=""text/css"" " & Lf &
     "   height=""768"" viewBox=""0 0 1024 768"" " & Lf &
     "   preserveAspectRatio=""xMidYMid meet"" " & Lf &
     "   xmlns=""http://www.w3.org/2000/svg"" version=""1.0"" >";

   Svg_Tailer : String := "</svg>";


   Open_Defs_Str : String := "<defs>";
   Close_Defs_Str : String := "</defs>";


   Procedure_Text_Style : String :=
     "style=""font-size:8pt;fill:#ff3333;"">";

   Operate_Text_Style : String :=
     "style=""font-size:8pt;fill:#ff3333;"">";

   Max_Operate_Expr_Length : constant := 42;
   -- The number of character of an operate expression.

   Compare_Text_Style : String :=
     "style=""font-size:8pt;fill:#ff3333;"">";

   Max_Compare_Expr_Length : constant := 22;
   -- The number of character of a compare expression.

   Var_Text_Style : String :=
     "style=""font-size:8pt;fill:#ff3333;"">";

   Max_Var_Length : constant := 10;
   -- The number of character of the variable name of a conatct or a coil.

   Var_Text_Y : Natural := 8;
   -- Offset from Y the left corner Y box of a contact.

   Block_Text_Style : String :=
     "style=""font-size:8pt;fill:#ff3333;"">";

   Max_Block_Length : constant := 120;
   -- The number of character of the variable name of a conatct or a coil.

   Block_Text_Y : Natural := 25;
   -- Offset from Y the left corner Y box of a contact.

   --     Y_Line_Offset : Natural;
   -- Offset from the begining of file.

   --     Y_Cur_Offset : Natural;
   -- Offset in the current matrix. This offset is sumed to Y_Offset at each
   -- end of the rung.

   Y_High_Contact : constant := 50;
   Y_High_Coil    : constant := 50;


   ----------------
   -- Connection --
   ----------------

   Conection_Def_Str : String :=
     "<g id=""cnx"" " & Lf &
     "     style=""stroke:deepskyblue;stroke-width:3.0; fill:gray;"">" & Lf &
     "   <desc> Connection </desc>" & Lf &
     "   <circle cx=""50"" cy=""0"" r=""10"" />" & Lf &
     "</g>";


   ----------------
   -- Connection --
   ----------------

   Abort_Conection_Def_Str : String :=
     "<g id=""abortcnx"" " & Lf &
     "     style=""stroke:orangered;stroke-width:3.0; fill:orangered;"">" & Lf &
     "   <desc> Connection </desc>" & Lf &
     "   <circle cx=""0"" cy=""0"" r=""8"" " & Lf &
     "     style=""stroke:orangered;stroke-width:3.0; fill:orangered;"" />" & Lf &
     "</g>";



   ---------------
   -- Terminate --
   ---------------

   Terminate_Def_Str : String :=
     "<g id=""wait"" " & Lf &
     "     style=""stroke:deepskyblue;stroke-width:5.0; fill:white;"">" & Lf &
     "   <desc> Wait </desc>" & Lf &
     "   <circle cx=""50"" cy=""50"" r=""10"" />" & Lf &
     "   <circle cx=""50"" cy=""50"" r=""20"" />" & Lf &
     "</g>";

   ----------
   -- Wait --
   ----------

   Wait_Def_Str : String :=
     "<g id=""wait"" " & Lf &
     "     style=""stroke:deepskyblue;stroke-width:5.0; fill:white;"">" & Lf &
     "   <desc> Wait </desc>" & Lf &
     "   <circle cx=""50"" cy=""50"" r=""50"" />" & Lf &
     "</g>";

   -----------
   -- Pause --
   -----------

   Pause_Def_Str : String :=
     "<g id=""pause"" " & Lf &
     "     style=""stroke:gray;stroke-width:5.0;fill:white;"">" & Lf &
     "   <desc> Pause </desc>" & Lf &
     "   <circle cx=""50"" cy=""20"" r=""20"" />" & Lf &
     "   <circle cx=""50"" cy=""20"" r=""10"" />" & Lf &
     "</g>";

   ----------
   -- Sync --
   ----------

   Sync_Def_Str : String :=
     "<g id=""sync"" " & Lf &
     "     style=""stroke:green;stroke-width:5.0; fill:white;"">" & Lf &
     "   <desc> Sync </desc>" & Lf &
     "   <circle cx=""50"" cy=""50"" r=""50"" />" & Lf &
     "   <circle cx=""50"" cy=""50"" r=""40"" />" & Lf &
     "</g>";

   --------
   -- If --
   --------

   If_Def_Str : String :=
     "<g id=""if"" " & Lf &
     "     style=""stroke:gray;stroke-width:3.0; fill:none;"">" & Lf &
     "   <desc> If </desc>" & Lf &
     "   <polyline points=""0 50, 50 0, 100 50, 50 100, 0 50"" />" & Lf &
     "</g>";

   --------------------
   -- Vertical_Trans --
   --------------------

   Vertical_Trans_Def_Str : String :=
     "<g id=""vtrans"" " & Lf &
     "     style=""stroke:gray;stroke-width:2.0; fill:none;"">" & Lf &
     "   <desc> Vertical Trans </desc>" & Lf &
     "   <line x1=""50"" y1=""0"" x2=""50"" y2=""100"" />" & Lf &
     "   <polygon style=""fill:#3333ff;"" " & Lf &
              "points=""40 80, 50 100, 60 80"" />" & Lf &
     "</g>";

   --------------------
   -- Vertical_Trans --
   --------------------

   Vertical_Top_Fleche_Def_Str : String :=
     "<g id=""vtfleche"" " & Lf &
     "     style=""stroke:gray;stroke-width:2.0; fill:none;"">" & Lf &
     "   <desc> Vertical Fleche </desc>" & Lf &
     "   <polygon style=""fill:#808080;"" " & Lf &
              "points=""40 20, 50 0, 60 20"" />" & Lf &
     "</g>";

   Vertical_Down_Fleche_Def_Str : String :=
     "<g id=""vdfleche"" " & Lf &
     "     style=""stroke:gray;stroke-width:2.0; fill:none;"">" & Lf &
     "   <desc> Vertical Fleche </desc>" & Lf &
     "   <polygon style=""fill:#808080;"" " & Lf &
              "points=""40 0, 50 20, 60 0"" />" & Lf &
     "</g>";

   ----------------------
   -- Horizontal_Trans --
   ----------------------

   Horizontal_Left_Fleche_Def_Str : String :=
     "<g id=""hlfleche"" " & Lf &
     "     style=""stroke:gray;stroke-width:2.0; fill:none;"">" & Lf &
     "   <desc> Horizontal to left Fleche </desc>" & Lf &
     "   <polygon style=""fill:#808080;"" " & Lf &
              "points=""0 40, 20 50, 0 60"" />" & Lf &
     "</g>";

   Horizontal_Right_Fleche_Def_Str : String :=
     "<g id=""hrfleche"" " & Lf &
     "     style=""stroke:gray;stroke-width:2.0; fill:none;"">" & Lf &
     "   <desc> Horizontal to right Fleche </desc>" & Lf &
     "   <polygon style=""fill:#808080;"" " & Lf &
              "points=""20 40, 0 50, 20 60"" />" & Lf &
     "</g>";


   -----------------
   -- Fleche Loop --
   -----------------

   loop_Fleche_Def_Str : String :=
     "<g id=""loopfleche"" " & Lf &
     "     style=""stroke:gray;stroke-width:2.0; fill:none;"">" & Lf &
     "   <desc> Horizontal to right Fleche </desc>" & Lf &
     "   <polygon style=""fill:gray;"" " & Lf &
              "points=""0 40, 20 50, 0 60"" />" & Lf &
     "   <polygon style=""fill:gray;"" " & Lf &
              "points=""20 40, 40 50, 20 60"" />" & Lf &
     "</g>";

   --------------------
   -- Vertical_Trans --
   --------------------

   Fleche_Inabort_Def_Str : String :=
     "<g id=""inabort"" " & Lf &
     "     style=""stroke:#808080;stroke-width:2.0; fill:none;"">" & Lf &
     "   <desc> Vertical Fleche </desc>" & Lf &
     "   <polygon style=""fill:#808080;"" " & Lf &
              "points=""35 90, 50 100, 55 80"" />" & Lf &
     "</g>";


   Fleche_Outabort_Def_Str : String :=
     "<g id=""outabort"" " & Lf &
     "     style=""stroke:#808080;stroke-width:2.0; fill:none;"">" & Lf &
     "   <desc> Vertical Fleche </desc>" & Lf &
     "   <polygon style=""fill:#808080;"" " & Lf &
              "points=""55 80, 50 100, 35 90"" />" & Lf &
     "</g>";



   Out_Buf : Output_Buffer;


   function Max
     (X : Integer;
      Y : Integer) return Integer;

   function Min
     (X : Integer;
      Y : Integer) return Integer;

   procedure Compute_Graph
     (G : Rnode_Id;
      X : Integer;
      Y : Integer);

   procedure Compute_Pause_Vertex
     (V : Rnode_Id;
      X : Integer;
      Y : Integer);

   procedure Compute_Wait_Vertex
     (V : Rnode_Id;
      X : Integer;
      Y : Integer);

   procedure Compute_Abort_Vertex
     (V : Rnode_Id;
      X : Integer;
      Y : Integer);

   procedure Compute_Select_Vertex
     (V : Rnode_Id;
      X : Integer;
      Y : Integer);

   procedure Compute_Fork_Vertex
     (V : Rnode_Id;
      X : Integer;
      Y : Integer);

   procedure Compute_If_Vertex
     (V : Rnode_Id;
      X : Integer;
      Y : Integer);

   procedure Compute_Loop_Vertex
     (V : Rnode_Id;
      X : Integer;
      Y : Integer);

   procedure Compute_Exit_Vertex
     (V : Rnode_Id;
      X : Integer;
      Y : Integer);


   procedure Offset_Graph
     (G   : Rnode_Id;
      Xof : Integer;
      Yof : Integer);

   procedure Offset_Wait_Vertex
     (V   : Rnode_Id;
      Xof : Integer;
      Yof : Integer);

   procedure Offset_Pause_Vertex
     (V   : Rnode_Id;
      Xof : Integer;
      Yof : Integer);

   procedure Offset_Abort_Vertex
     (V   : Rnode_Id;
      Xof : Integer;
      Yof : Integer);

   procedure Offset_Select_Vertex
     (V   : Rnode_Id;
      Xof : Integer;
      Yof : Integer);

   procedure Offset_Fork_Vertex
     (V   : Rnode_Id;
      Xof : Integer;
      Yof : Integer);

   procedure Offset_If_Vertex
     (V   : Rnode_Id;
      Xof : Integer;
      Yof : Integer);

   procedure Offset_Loop_Vertex
     (V   : Rnode_Id;
      Xof : Integer;
      Yof : Integer);

   procedure Offset_Exit_Vertex
     (V   : Rnode_Id;
      Xof : Integer;
      Yof : Integer);


   procedure Set_Output_Buffer (Obf : in Output_Buffer);

   function Get_Output_Buffer return Output_Buffer;

   function To_String (X : Integer) return String;

   procedure Do_Indentation;

   procedure Dec_Indentation;

   procedure Inc_Indentation;

   procedure Open_Svg_Output (F : String);

   procedure Close_Svg_Output;

   procedure Newline (I : Natural := 1);


   procedure Write_Out (S : String);

   procedure Line_Write (S : String);

   procedure Emit_Svg_Header
     (X : Integer;
      Y : Integer);

   procedure Emit_Header;

   procedure Emit_Tailer;

   type Line_Type is (X_Before, Y_Before);

   procedure Abort_Connect_X1_Y1_To_X2_Y2
     (X1   : Integer;
      Y1   : Integer;
      X2   : Integer;
      Y2   : Integer;
      T    : Line_Type;
      W    : Integer;
      H    : Integer);

   procedure Connect_X1_Y1_To_X2_Y2
     (X1 : Integer;
      Y1 : Integer;
      X2 : Integer;
      Y2 : Integer;
      T  : Line_Type;
      W  : Integer;
      H  : Integer);

   procedure Connect_V1_To_V2
     (V1   : Rnode_Id;
      V2   : Rnode_Id;
      T    : Line_Type;
      W    : Integer;
      H    : Integer;
      X1of : Integer;
      Y1of : Integer;
      X2of : Integer;
      Y2of : Integer);

   procedure Connect_Begin_Cnx_To_V2
     (V1   : Rnode_Id;
      V2   : Rnode_Id;
      T    : Line_Type;
      W    : Integer;
      H    : Integer;
      X1of : Integer;
      Y1of : Integer;
      X2of : Integer;
      Y2of : Integer);

   procedure Connect_V1_To_End_Cnx
     (V1   : Rnode_Id;
      V2   : Rnode_Id;
      T    : Line_Type;
      W    : Integer;
      H    : Integer;
      X1of : Integer;
      Y1of : Integer;
      X2of : Integer;
      Y2of : Integer);

   procedure Connect_Begin_Cnx_To_End_Cnx
     (V1   : Rnode_Id;
      V2   : Rnode_Id;
      T    : Line_Type;
      W    : Integer;
      H    : Integer;
      X1of : Integer;
      Y1of : Integer;
      X2of : Integer;
      Y2of : Integer);

   procedure Connect_X1_Y1_To_V
     (X1   : Integer;
      Y1   : Integer;
      V    : Rnode_Id;
      T    : Line_Type;
      W    : Integer;
      H    : Integer;
      X1of : Integer;
      Y1of : Integer;
      X2of : Integer;
      Y2of : Integer);

   procedure Connect_XY1_XY2
     (X1 : Integer;
      Y1 : Integer;
      X2 : Integer;
      Y2 : Integer;
      T  : Line_Type);

   procedure Rectangle
     (X : Integer;
      Y : Integer;
      W : Integer;
      H : Integer);

   procedure Emit_Graph (G : Rnode_Id);

   procedure Emit_Pause_Vertex (V : Rnode_Id);

   procedure Emit_Wait_Vertex (V : Rnode_Id);

   procedure Emit_Sync_Vertex (V : Rnode_Id);

   procedure Emit_Abort_Vertex (V : Rnode_Id);

   procedure Emit_Select_Vertex (V : Rnode_Id);

   procedure Emit_Fork_Vertex (V : Rnode_Id);

   procedure Emit_If_Vertex (V : Rnode_Id);

   procedure Emit_Loop_Vertex (V : Rnode_Id);

   procedure Emit_Exit_Vertex (V : Rnode_Id);



   --------------------
   -- Dump_Svg_Graph --
   --------------------

   procedure Dump_Svg_Graph
     (G         : Rnode_Id;
      File_Name : String := "") is
   begin
      Output.Write_Line ("Dump_Svg_Graph");

      Compute_Graph (G, Xunit, Yunit);

      Output.Write_Line ("====> After Compute");
      Output.Write_Line ("====> Xof = " & X_Offset (G)'Img);

      Offset_Graph (G, abs (X_Offset (G)) + 100, 100);

      Output.Write_Line ("====> After Xoffset");

      Out_Buf := New_Output;

      Output.Write_Line ("  =====> Width  => " & Width (G)'Img);
      Output.Write_Line ("  =====> Height => " & Height (G)'Img);


      Emit_Svg_Header (Width (G), Height (G));
      Emit_Header;

      Output.Write_Line ("====> Before emiiting");
      Emit_Graph (G);

      Emit_Tailer;

      Output.Write_Line ("====> After emiiting");

      if File_Name = "" then
	 Open_Svg_Output ("svg-dump.svg");
      else
	 Open_Svg_Output (File_Name & ".svg");
      end if;
      Append_Buffer_To_File (Out_Buf, Svg_File);
      Close_Svg_Output;

      Output.Write_Line ("Dump_Svg_Graph End");
   end Dump_Svg_Graph;

   ---------
   -- Max --
   ---------

   function Max
     (X : Integer;
      Y : Integer) return Integer is
   begin
      if X > Y then
	 return X;
      else
	 return Y;
      end if;
   end Max;

   ---------
   -- Min --
   ---------

   function Min
     (X : Integer;
      Y : Integer) return Integer is
   begin
      if X < Y then
	 return X;
      else
	 return Y;
      end if;
   end Min;

   ------------------------
   -- Compute_Down_Width --
   ------------------------

   function Compute_Down_Width (V : Rnode_Id) return Integer is
      Vd : Rnode_Id;
      W  : Integer;
   begin
      Vd := V;
      W := 0;
      while Vd /= No_Rnode loop
	 W := Max (W, Width (Vd));
	 Next (Vd);
      end loop;

      return W;
   end Compute_Down_Width;

   -------------------
   -- Compute_Graph --
   -------------------

   procedure Compute_Graph
     (G : Rnode_Id;
      X : Integer;
      Y : Integer) is

      V         : Rnode_Id;
      V_Begin   : Rnode_Id;
      V_End     : Rnode_Id;
      X_Current : Integer;
      Y_Current : Integer;
      W         : Integer;
      Xof       : Integer;
      Exit_Count : Natural;
   begin
      Output.Write_Line (" ======================> Begin Sub Graf => ");
      Set_Xrel (G, 0);
      Set_Yrel (G, 0);

      Exit_Count := 0;
      Xof := 0;
      W := 0;
      X_Current := 0;
      Y_Current := 0;
      if Present (Vertices_List (G)) then
	 V := First (Vertices_List (G));
      end if;

      if V /= No_Rnode then
	 V_Begin := V;
      else
	 Set_Width    (G, 0);
	 Set_Height   (G, 0);
	 Set_X_Offset (G, 0);
	 Set_Xrel_Begin_Cnx (G, X);
	 Set_Yrel_Begin_Cnx (G, Y);
	 Set_Xrel_End_Cnx (G, X);
	 Set_Yrel_End_Cnx (G, Y);
	 return;
      end if;

      while V /= No_Rnode loop
	 case Rkind (V) is

	    when R_Unused_At_Start =>
	       null;

	    when R_Graph_Vertex =>
	       null;

	    when R_Contionnal_Graph_Vertex =>
	       null;

	    when R_Reactive_Begin_Vertex =>
	       null;

	    when R_Reactive_End_Vertex =>
	       null;

	    when R_End_Vertex =>
	       null;

	    when R_Begin_Vertex =>
	       null;

	    when R_Repeat_Loop_Vertex =>
	       null;

	    when R_Handler_Vertex =>
	       null;

	    when R_If_Vertex =>
	       Set_Top_Width (V, W);
	       Compute_If_Vertex (V, X_Current, Y_Current);
	       Y_Current := Yrel (V) + Height (V) + Yunit;
	       W := Max (W, Width (V));
	       Xof := Max (Xof, X_Offset (V));
	       V_End := V;

	    when R_Loop_Vertex =>
	       Set_Top_Width (V, W);
	       Compute_Loop_Vertex (V, X_Current, Y_Current);
	       Y_Current := Yrel (V) + Height (V) + Yunit;
	       W := Max (W, Width (V));
	       Xof := Max (Xof, X_Offset (V));
	       V_End := V;

	    when R_Exit_Vertex =>
	       Set_Top_Width (V, W);
	       Compute_Exit_Vertex (V, X_Current, Y_Current);
	       Y_Current := Yrel (V) + Height (V) + Yunit;
	       W := Max (W, Width (V));
	       Xof := Max (Xof, X_Offset (V));
	       V_End := V; -- No_Rnode;
	       Exit_Count := Exit_Count + 1;

	    when R_Fork_Vertex =>
	       Set_Top_Width (V, W);
	       Compute_Fork_Vertex (V, X_Current, Y_Current);
	       Y_Current := Yrel (V) + Height (V);
	       W := Max (W, Width (V));
	       Xof := Max (Xof, X_Offset (V));
	       V_End := V;

	    when R_Abort_Vertex =>
	       Set_Top_Width (V, W);
	       Compute_Abort_Vertex (V, X_Current, Y_Current);
	       Y_Current := Yrel (V) + Height (V);
	       W := Max (W, Width (V));
	       Xof := Max (Xof, X_Offset (V));
	       V_End := V;

	    when R_Weak_Abort_Vertex =>
	       Set_Top_Width (V, W);
	       Compute_Abort_Vertex (V, X_Current, Y_Current);
	       Y_Current := Yrel (V) + Height (V);
	       W := Max (W, Width (V));
	       Xof := Max (Xof, X_Offset (V));
	       V_End := V;

	    when R_Pause_Vertex =>
	       Set_Top_Width (V, W);
	       Compute_Pause_Vertex (V, X_Current, Y_Current);
	       Y_Current := Yrel (V) + Height (V);
	       W := Max (W, Width (V));
	       Xof := Max (Xof, X_Offset (V));
	       V_End := No_Rnode;

	    when R_Wait_Vertex =>
	       Set_Top_Width (V, W);
	       Compute_Wait_Vertex (V, X_Current, Y_Current);
	       Y_Current := Yrel (V) + Height (V);
	       W := Max (W, Width (V));
	       Xof := Max (Xof, abs (Xrel (V)));
	       V_End := V;

	    when R_Sync_Vertex =>
	       Set_Top_Width (V, W);
	       Compute_Wait_Vertex (V, X_Current, Y_Current);
	       Y_Current := Yrel (V) + Height (V);
	       W := Max (W, Width (V));
	       Xof := Max (Xof, X_Offset (V));
	       V_End := V;

	    when R_Select_Vertex =>
	       Compute_Select_Vertex (V, X_Current, Y_Current);
	       Set_Top_Width (V, W);
	       Y_Current := Yrel (V) + Height (V) + Yunit;
	       W := Max (W, Width (V));
	       Xof := Max (Xof, X_Offset (V));
	       V_End := V;

	    when R_Unused_At_End =>
	       null;
	 end case;

	 Next (V);
      end loop;

      Output.Write_Line (" ======================> End Sub Graf => " & Xof'Img);
      Set_Width (G, W + (Exit_Count * Xunit));
      Set_Height (G, Y_Current);
      Set_X_Offset (G, Xof);

      Set_Root_Vertex (G, V_Begin);
      Set_Tail_Vertex (G, V_End);

      if V_Begin /= No_Rnode then
	 Set_Xrel_Begin_Cnx (G, Xrel_Begin_Cnx (V_Begin));
	 Set_Yrel_Begin_Cnx (G, Yrel_Begin_Cnx (V_Begin));
      else
	 Set_Xrel_Begin_Cnx (G, 0);
	 Set_Yrel_Begin_Cnx (G, 0);
      end if;

      if V_End /= No_Rnode then
	 Set_Xrel_End_Cnx (G, Xrel_End_Cnx (V_End));
	 Set_Yrel_End_Cnx (G, Yrel_End_Cnx (V_End));
      else
	 Set_Xrel_End_Cnx (G, 0);
	 Set_Yrel_End_Cnx (G, 0);
      end if;
   end Compute_Graph;

   --------------------------
   -- Compute_Pause_Vertex --
   --------------------------

   procedure Compute_Pause_Vertex
     (V : Rnode_Id;
      X : Integer;
      Y : Integer) is
   begin
      Output.Write_Line ("Compute_Pause_Vertex => " & Rkind (V)'Img);

      Set_Xrel (V, X);
      Set_Yrel (V, Y);

      Set_Height (V, Yunit + Yunit);
      Set_Width (V, Xunit);

      Set_Xrel_Begin_Cnx (V, Xrel (V));
      Set_Yrel_Begin_Cnx (V, Yrel (V));

      Set_Xrel_End_Cnx (V, Xrel (V));
      Set_Yrel_End_Cnx (V, Yrel (V) + 2*Yunit);

      Output.Write_Line ("Height => " & Height (V)'Img);
      Output.Write_Line ("Width  => " & Width (V)'Img);

      Output.Write_Line ("X Begin Cnx => " & Xrel_Begin_Cnx (V)'Img);
      Output.Write_Line ("Y Begin Cnx => " & Yrel_Begin_Cnx (V)'Img);

      Output.Write_Line ("X End Cnx => " & Xrel_End_Cnx (V)'Img);
      Output.Write_Line ("Y End Cnx => " & Yrel_End_Cnx (V)'Img);

      Output.Write_Line ("Compute_Pause_Vertex End");
   end Compute_Pause_Vertex;

   -------------------------
   -- Compute_Wait_Vertex --
   -------------------------

   procedure Compute_Wait_Vertex
     (V : Rnode_Id;
      X : Integer;
      Y : Integer) is
   begin
      Output.Write_Line ("Compute_Wait_Vertex => " & Rkind (V)'Img);

      Set_Height (V, Yunit + Yunit);
      Set_Width (V, Xunit);

      Set_Xrel (V, X - Width (V)/2);
      Set_Yrel (V, Y);

      Set_Xrel_Begin_Cnx (V, Xrel (V));
      Set_Yrel_Begin_Cnx (V, Yrel (V));

      Set_Xrel_End_Cnx (V, Xrel (V));
      Set_Yrel_End_Cnx (V, Yrel (V) + 2*Yunit);

      Output.Write_Line ("Height => " & Height (V)'Img);
      Output.Write_Line ("Width  => " & Width (V)'Img);

      Output.Write_Line ("X Begin Cnx => " & Xrel_Begin_Cnx (V)'Img);
      Output.Write_Line ("Y Begin Cnx => " & Yrel_Begin_Cnx (V)'Img);

      Output.Write_Line ("X End Cnx => " & Xrel_End_Cnx (V)'Img);
      Output.Write_Line ("Y End Cnx => " & Yrel_End_Cnx (V)'Img);


      Output.Write_Line ("Compute_Wait_Vertex End");
   end Compute_Wait_Vertex;

   --------------------------
   -- Compute_Abort_Vertex --
   --------------------------

   procedure Compute_Abort_Vertex
     (V : Rnode_Id;
      X : Integer;
      Y : Integer) is

      H           : Integer;
      W           : Integer;
      Xof         : Integer;
      H_Handler   : Integer;
      W_Handler   : Integer;
      Xof_Handler : Integer;
      H_Body      : Integer;
      W_Body      : Integer;
      Xof_Body    : Integer;
   begin
      Output.Write_Line ("Compute_Abort_Vertex => " & Rkind (V)'Img);

      Set_Xrel (V, X);
      Set_Yrel (V, Y);

      Set_Y_Begin (V, Yunit);

      -- Begin Vertex is Xabs + 1 to put it verticaly to the body
      -- Body sub graph

      Compute_Graph (Body_Graph (V), 0, 0);
      H_Body := Height (Body_Graph (V));
      W_Body := Width (Body_Graph (V));
      Xof_Body := X_Offset (Body_Graph (V));

      -- Handler Sub Graph

      if Handler_Graph (V) /= No_Rnode then
	 Compute_Graph (Handler_Graph (V), 0, 0);
	 H_Handler := Height (Handler_Graph (V));
	 W_Handler := Width (Handler_Graph (V));
	 Xof_Handler := X_Offset (Handler_Graph (V));
      else
	 H_Handler := 0;
	 W_Handler := 0;
	 Xof_Handler := 0;
      end if;

      H := Max (H_Body, H_Handler + 2*Yunit);
      W := W_Body + W_Handler + Gap_Select_Alternatives;

      Xof := Xof_Body + Xof_Handler;

      Set_X_Offset (V, Xof);

      Set_Y_End (V, Yunit);

      Set_Height (V, H + Y_Begin (V) + Y_End (V));
      Set_Width (V, W);

      Set_Xrel_Begin_Cnx (V, Xrel (V));
      Set_Yrel_Begin_Cnx (V, Yrel (V));
      Set_Xrel_End_Cnx (V, Xrel (V));
      Set_Yrel_End_Cnx (V, Yrel (V) + Height (V));

      Output.Write_Line ("Compute_Abort_Vertex End");
   end Compute_Abort_Vertex;

   ---------------------------
   -- Compute_Select_Vertex --
   ---------------------------

   procedure Compute_Select_Vertex
     (V : Rnode_Id;
      X : Integer;
      Y : Integer) is

      Alt      : Rnode_Id;
      W_Alt    : Integer;
      H_Alt    : Integer;
      W_Select : Integer;
      H_Select : Integer;
      Xof      : Integer;
   begin
      Output.Write_Line ("Compute_Select_Vertex => " & Rkind (V)'Img);

      Set_Xrel (V, X);
      Set_Yrel (V, Y);

      Set_Y_Begin (V, 3*Yunit);

      W_Select := 0;
      H_Select := 0; -- 2;
      Xof := 0;


      -- Alternatives.

      if Present (Alternatives (V)) then
	 Alt := First (Alternatives (V));
	 while Alt /= No_Rnode loop
	    Compute_Graph (Alt, 0, 0);

	    W_Alt := Width (Alt) + Gap_Select_Alternatives;
	    H_Alt := Height (Alt);

	    W_Select := W_Select + W_Alt;
	    H_Select := Max (H_Select, H_Alt);
	    Xof := Xof + X_Offset (Alt);

	    Next (Alt);
	 end loop;

	 if First (Alternatives (V)) /= No_Rnode Then
	    W_Select := W_Select - Gap_Select_Alternatives;
	 end if;
      end if;


      Set_Y_End (V, 0);

      -- The declage to left is the total Width /2

      -- Add to width the arc repeat and the arc end.and arc link of loop

      -- In Arc.

      --  Set_Out_Arc_Type (V, H_Trans);
      --  Set_Out_Arc_Width (V, W_Select);
      --  Set_Out_Arc_Height (V, 1);

      Set_X_Offset (V, 0); -- Xof); -- - W_Select/2);
      Set_Width (V, W_Select);

      -- We add 4 Yunit for 2 at the beginning and 2 and the end.

      Set_Height (V, H_Select + Y_Begin (V) + Y_End (V));

      Set_Xrel_Begin_Cnx (V, Xrel (V));
      Set_Yrel_Begin_Cnx (V, Yrel (V) + 2*Yunit);
      Set_Xrel_End_Cnx (V, Xrel (V));
      Set_Yrel_End_Cnx (V, Yrel (V) + Height (V));

      Output.Write_Line ("Compute_Select_Vertex End");
   end Compute_Select_Vertex;

   -------------------------
   -- Compute_Fork_Vertex --
   -------------------------

   procedure Compute_Fork_Vertex
     (V : Rnode_Id;
      X : Integer;
      Y : Integer) is

      Alt    : Rnode_Id;
      Sync   : Rnode_Id;
      W_Alt  : Integer := -1;
      H_Alt  : Integer := -1;
      W_Fork : Integer := -1;
      H_Fork : Integer := -1;
      Xof    : Integer := -1;
      Max_Y  : Integer := -1;
   begin
      Output.Write_Line ("Compute_Fork_Vertex => " & Rkind (V)'Img);

      Set_Xrel (V, X);
      Set_Yrel (V, Y);

      -- Correspond espace entre debut cnx et les sous graphs.

      Set_Y_Begin (V, Yunit);

      -- Alternatives.

      W_Fork := 0;
      H_Fork := 0;
      Xof := Decal_Fork; -- 0;

      if Present (Alternatives (V)) then
	 Alt := First (Alternatives (V));
	 while Alt /= No_Rnode loop
	    Sync := Last (Vertices_List (Alt));

	    if Sync /= No_Rnode then
	       Set_Rkind (Sync, R_Sync_Vertex);
	    end if;

	    Compute_Graph (Alt, 0, 0);

	    W_Alt := Width (Alt) + Gap_Select_Alternatives;
	    H_Alt := Height (Alt);
	    Xof := Xof + X_Offset (Alt);

	    W_Fork := W_Fork + W_Alt;
	    H_Fork := Max (H_Fork, H_Alt);

	    Next (Alt);
	 end loop;

	 Alt := First (Alternatives (V));
	 while Alt /= No_Rnode loop
	    Sync := Last (Vertices_List (Alt));
	    Max_Y := Max (Yrel (Sync), Max_Y);
	    Next (Alt);
	 end loop;

	 Alt := First (Alternatives (V));
	 while Alt /= No_Rnode loop
	    Sync := Last (Vertices_List (Alt));
	    Set_Yrel (Sync, Max_Y);
	    Set_Yrel_Begin_Cnx (Sync, Yrel (Sync));
	    Set_Yrel_End_Cnx (Sync, Yrel (Sync) + 2*Yunit);
	    Set_Yrel_End_Cnx (Alt, Yrel_End_Cnx (Sync));

	    Next (Alt);
	 end loop;

	 if First (Alternatives (V)) /= No_Rnode Then
	    W_Fork := W_Fork - Gap_Select_Alternatives;
	 end if;
      end if;

      -- Correspond à l'espace entre la fin du plus grand des sous
      -- graphs et la cnx.

      Set_Y_End (V, Yunit);

      H_Fork := H_Fork + Y_Begin (V) + Y_End (V);

      Set_Width (V, W_Fork);
      Set_Height (V, H_Fork);
      Set_X_Offset (V, Decal_Fork); -- Xof);

      Set_Xrel_Begin_Cnx (V, Xrel (V));
      Set_Yrel_Begin_Cnx (V, Yrel (V));
      Set_Xrel_End_Cnx (V, Xrel (V));
      Set_Yrel_End_Cnx (V, Yrel (V) + Height (V)); -- - Yunit);

      Output.Write_Line ("Compute_Fork_Vertex End");
   end Compute_Fork_Vertex;

   -----------------------
   -- Compute_If_Vertex --
   -----------------------

   procedure Compute_If_Vertex
     (V : Rnode_Id;
      X : Integer;
      Y : Integer) is

      Alt     : Rnode_Id;
      W_Alt   : Integer;
      H_Alt   : Integer;
      W_If    : Integer;
      H_If    : Integer;
      Xof     : Integer;
      Y_End   : Integer;
      Y_Begin : Integer;
   begin
      Set_Xrel (V, X);
      Set_Yrel (V, Y);

      Y_Begin := 2*Yunit;

      -- Then Alternative.

      Compute_Graph (Then_Graph (V), 0, 0);

      W_If := Width (Then_Graph (V));
      H_If := Height (Then_Graph (V));
      Xof := X_Offset (Then_Graph (V));

      Y_End := 0;

      -- Alternatives.

      if Present (Alternatives (V)) then
	 Alt := First (Alternatives (V));
	 while Alt /= No_Rnode loop
	    Compute_Graph (Alt, 0, 0);

	    W_Alt := Width (Alt) + Gap_Select_Alternatives;
	    H_Alt := Height (Alt);

	    W_If := W_If + W_Alt;
	    H_If := Max (H_If, H_Alt);
	    Xof := Xof + X_Offset (Alt);

	    Next (Alt);
	 end loop;

	 if First (Alternatives (V)) /= No_Rnode Then
	    W_If := W_If - Gap_Select_Alternatives;
	 end if;

	 Y_End := YUnit;
      end if;

      -- Else Alternative.

      if Else_Graph (V) /= No_Rnode then
	 Compute_Graph (Else_Graph (V), 0, 0);

	 W_If := W_If + Width (Else_Graph (V)) + Gap_Select_Alternatives;
	 H_If := Max (H_If, Height (Else_Graph (V)));
	 Xof  := Xof + X_Offset (Else_Graph (V));

	 Y_End := YUnit;
      else
	 W_If := W_If + Xunit;
      end if;

      Set_X_Offset (V, 0); -- Xof);
      Set_Height (V, H_If + Y_End + Y_Begin);
      Set_Width (V, W_If);

      Set_Xrel_Begin_Cnx (V, Xrel (V));
      Set_Yrel_Begin_Cnx (V, Yrel (V));
      Set_Xrel_End_Cnx (V, Xrel (V));
      Set_Yrel_End_Cnx (V, Yrel (V) + Height (V));
   end Compute_If_Vertex;

   -------------------------
   -- Compute_Loop_Vertex --
   -------------------------

   procedure Compute_Loop_Vertex
     (V : Rnode_Id;
      X : Integer;
      Y : Integer) is

   begin
      Set_Xrel (V, X);
      Set_Yrel (V, Y);

      Set_Y_Begin (V, Yunit);
      Set_Y_End (V,Yunit);

      -- Body Graph.

      Compute_Graph (Body_Graph (V), 0, 0);
      Set_Tail_Vertex (Body_Graph (V), No_Rnode);

      Set_Width (V, Width (Body_Graph (V)) + Xunit);
      Set_Height (V, Height (Body_Graph (V)) + Y_Begin (V) + Y_End (V));
      Set_X_Offset (V, X_Offset (Body_Graph (V)));

      Set_Xrel_Begin_Cnx (V, Xrel (V));
      Set_Yrel_Begin_Cnx (V, Yrel (V));
      Set_Xrel_End_Cnx (V, Xrel (V));
      Set_Yrel_End_Cnx (V, Yrel (V) + Height (V));
   end Compute_Loop_Vertex;

   -------------------------
   -- Compute_Exit_Vertex --
   -------------------------

   procedure Compute_Exit_Vertex
     (V : Rnode_Id;
      X : Integer;
      Y : Integer) is
   begin
      Set_Xrel (V, X);
      Set_Yrel (V, Y);

      Set_Width    (V, Xunit);
      Set_Height   (V, Yunit);
      Set_X_Offset (V, 0);

      Set_Xrel_Begin_Cnx (V, Xrel (V));
      Set_Yrel_Begin_Cnx (V, Yrel (V));
      Set_Xrel_End_Cnx (V, Xrel (V));
      Set_Yrel_End_Cnx (V, Yrel (V) + Height (V));
   end Compute_Exit_Vertex;

   ------------------
   -- Offset_Graph --
   ------------------

   procedure Offset_Graph
     (G   : Rnode_Id;
      Xof : Integer;
      Yof : Integer) is

      V : Rnode_Id;
   begin
      Output.Write_Line ("Offset_Graph");
      Set_Xabs (G, Xrel (G) + Xof);
      Set_Yabs (G, Yrel (G) + Yof);

      Set_Xabs_Begin_Cnx (G, Xrel_Begin_Cnx (G) + Xof);
      Set_Yabs_Begin_Cnx (G, Yrel_Begin_Cnx (G) + Yof);

      Set_Xabs_End_Cnx   (G, Xrel_End_Cnx (G) + Xof);
      Set_Yabs_End_Cnx   (G, Yrel_End_Cnx (G) + Yof);

      Output.Write_Line ("   Sub Graph Xof ==> " & Xof'Img);

      V := First (Vertices_List (G));
      while V /= No_Rnode loop
	 case Rkind (V) is

	    when R_Unused_At_Start =>
	       null;

	    when R_Graph_Vertex =>
	       null;

	    when R_Contionnal_Graph_Vertex =>
	       null;

	    when R_Reactive_Begin_Vertex =>
	       null;

	    when R_Reactive_End_Vertex =>
	       null;

	    when R_End_Vertex =>
	       null;

	    when R_Begin_Vertex =>
	       null;

	    when R_Repeat_Loop_Vertex =>
	       null;

	    when R_Handler_Vertex =>
	       null;

	    when R_If_Vertex =>
	       Offset_If_Vertex (V, Xof, Yof);

	    when R_Loop_Vertex =>
	       Offset_Loop_Vertex (V, Xof, Yof);

	    when R_Exit_Vertex =>
	       Offset_Exit_Vertex (V, Xof, Yof);

	    when R_Fork_Vertex =>
	       Offset_Fork_Vertex (V, Xof, Yof);

	    when R_Abort_Vertex =>
	       Offset_Abort_Vertex (V, Xof, Yof);

	    when R_Weak_Abort_Vertex =>
	       Offset_Abort_Vertex (V, Xof, Yof);

	    when R_Pause_Vertex =>
	       Offset_Pause_Vertex (V, Xof, Yof);
	       --  Set_Xabs (V, Xrel (V) + Xof);
	       --  Set_Yabs (V, Yrel (V) + Yof);

	    when R_Wait_Vertex =>
	       Offset_Wait_Vertex (V, Xof, Yof);

	    when R_Sync_Vertex =>
	       Offset_Wait_Vertex (V, Xof, Yof);

	    when R_Select_Vertex =>
	       Offset_Select_Vertex (V, Xof, Yof);

	    when R_Unused_At_End =>
	       null;
	 end case;

	 Next (V);
      end loop;
      Output.Write_Line ("Offset_Graph End");
   end Offset_Graph;

   ------------------------
   -- Offset_Wait_Vertex --
   ------------------------

   procedure Offset_Wait_Vertex
     (V   : Rnode_Id;
      Xof : Integer;
      Yof : Integer) is

      Nxt : Rnode_Id;
   begin
      Output.Write_Line ("Offset_Wait_Vertex => " & Rkind (V)'Img);

      Set_Xabs (V, Xrel (V) + Xof);
      Set_Yabs (V, Yrel (V) + Yof);

      if Out_Arc_Type (V) /= No_Arc then
	 Set_Out_Arc_Xabs (V, Out_Arc_Xrel (V) + Xof);
	 Set_Out_Arc_Yabs (V, Out_Arc_Yrel (V) + Yof);

 	 Nxt := Next (V);
	 if Nxt /= No_Rnode then
	    Set_Out_Arc_Height (V, Yrel (Nxt) - Out_Arc_Yrel (V));
	 end if;
      end if;

      Set_Xabs_Begin_Cnx (V, Xrel_Begin_Cnx (V) + Xof);
      Set_Yabs_Begin_Cnx (V, Yrel_Begin_Cnx (V) + Yof);

      Set_Xabs_End_Cnx (V, Xrel_End_Cnx (V) + Xof);
      Set_Yabs_End_Cnx (V, Yrel_End_Cnx (V) + Yof);

      Output.Write_Line ("Offset_Wait_Vertex End");
   end Offset_Wait_Vertex;


   -------------------------
   -- Offset_Pause_Vertex --
   -------------------------

   procedure Offset_Pause_Vertex
     (V   : Rnode_Id;
      Xof : Integer;
      Yof : Integer) is

      Nxt : Rnode_Id;
   begin
      Output.Write_Line ("Offset_Pause_Vertex => " & Rkind (V)'Img);

      Set_Xabs (V, Xrel (V) + Xof);
      Set_Yabs (V, Yrel (V) + Yof);

      if Out_Arc_Type (V) /= No_Arc then
	 Set_Out_Arc_Xabs (V, Out_Arc_Xrel (V) + Xof);
	 Set_Out_Arc_Yabs (V, Out_Arc_Yrel (V) + Yof);

 	 Nxt := Next (V);
	 if Nxt /= No_Rnode then
	    Set_Out_Arc_Height (V, Yrel (Nxt) - Out_Arc_Yrel (V));
	 end if;
      end if;

      Set_Xabs_Begin_Cnx (V, Xrel_Begin_Cnx (V) + Xof);
      Set_Yabs_Begin_Cnx (V, Yrel_Begin_Cnx (V) + Yof);

      Set_Xabs_End_Cnx (V, Xrel_End_Cnx (V) + Xof);
      Set_Yabs_End_Cnx (V, Yrel_End_Cnx (V) + Yof);

      Output.Write_Line ("Offset_Pause_Vertex End");
   end Offset_Pause_Vertex;


   -------------------------
   -- Offset_Abort_Vertex --
   -------------------------

   procedure Offset_Abort_Vertex
     (V   : Rnode_Id;
      Xof : Integer;
      Yof : Integer) is

      X : Integer;
      Y : Integer;
   begin
      Output.Write_Line ("Offset_Abort_Vertex => " & Rkind (V)'Img);

      Set_Xabs (V, Xrel (V) + Xof);
      Set_Yabs (V, Yrel (V) + Yof);

      -- Body sub graph

      X := Xabs (V);
      Y := Yabs (V) + Y_Begin (V);
      Offset_Graph (Body_Graph (V), X, Y);

      if Handler_Graph (V) /= No_Rnode then
	 X := Xabs (Body_Graph (V)) + Width (Body_Graph (V)) + Gap_Select_Alternatives;
	 Y := Yabs (Body_Graph (V)) + 2*Yunit;
	 Offset_Graph (Handler_Graph (V), X, Y);
      end if;

      Set_Xabs_Begin_Cnx (V, Xrel_Begin_Cnx (V) + Xof);
      Set_Yabs_Begin_Cnx (V, Yrel_Begin_Cnx (V) + Yof);

      Set_Xabs_End_Cnx (V, Xrel_End_Cnx (V) + Xof);
      Set_Yabs_End_Cnx (V, Yrel_End_Cnx (V) + Yof);

      Output.Write_Line ("Offset_Abort_Vertex End");
   end Offset_Abort_Vertex;

   --------------------------
   -- Offset_Select_Vertex --
   --------------------------

   procedure Offset_Select_Vertex
     (V   : Rnode_Id;
      Xof : Integer;
      Yof : Integer) is

      Alt   : Rnode_Id;
      X_Alt : Integer;
      Y_Alt : Integer;
   begin
      Output.Write_Line ("Offset_Select_Vertex => " & Rkind (V)'Img);

      Set_Xabs (V, Xrel (V) + Xof - Decal_Select);
      Set_Yabs (V, Yrel (V) + Yof);

      X_Alt := Xabs (V);
      Y_Alt := Yabs (V) + Y_Begin (V); -- 3*Yunit;

      if Present (Alternatives (V)) then
	 Alt := First (Alternatives (V));
	 while Alt /= No_Rnode loop
	    Offset_Graph (Alt, X_Alt, Y_Alt);
	    X_Alt := X_Alt + Width (Alt) + Gap_Select_Alternatives;
	    Next (Alt);
	 end loop;
      end if;

      Set_Xabs_Begin_Cnx (V, Xrel_Begin_Cnx (V) + Xof);
      Set_Yabs_Begin_Cnx (V, Yrel_Begin_Cnx (V) + Yof);

      Set_Xabs_End_Cnx (V, Xrel_End_Cnx (V) + Xof);
      Set_Yabs_End_Cnx (V, Yrel_End_Cnx (V) + Yof);

      Output.Write_Line ("Offset_Select_Vertex End");
   end Offset_Select_Vertex;

   ------------------------
   -- Offset_Fork_Vertex --
   ------------------------

   procedure Offset_Fork_Vertex
     (V   : Rnode_Id;
      Xof : Integer;
      Yof : Integer) is

      Alt   : Rnode_Id;
      X_Alt : Integer;
      Y_Alt : Integer;
   begin
      Output.Write_Line ("Offset_Fork_Vertex => " & Rkind (V)'Img);

      Set_Xabs (V, Xrel (V) + Xof - Decal_Fork);
      Set_Yabs (V, Yrel (V) + Yof);

      X_Alt := Xabs (V);
      Y_Alt := Yabs (V) + Y_Begin (V); -- Yunit;

      if Present (Alternatives (V)) then
	 Alt := First (Alternatives (V));
	 while Alt /= No_Rnode loop
	    Offset_Graph (Alt, X_Alt, Y_Alt);
	    X_Alt := X_Alt + Width (Alt) + Gap_Select_Alternatives;
	    Next (Alt);
	 end loop;
      end if;

      Set_Xabs_Begin_Cnx (V, Xrel_Begin_Cnx (V) + Xof);
      Set_Yabs_Begin_Cnx (V, Yrel_Begin_Cnx (V) + Yof);

      Set_Xabs_End_Cnx (V, Xrel_End_Cnx (V) + Xof);
      Set_Yabs_End_Cnx (V, Yrel_End_Cnx (V) + Yof);

      Output.Write_Line ("Offset_Fork_Vertex End");
   end Offset_Fork_Vertex;

   ----------------------
   -- Offset_If_Vertex --
   ----------------------

   procedure Offset_If_Vertex
     (V   : Rnode_Id;
      Xof : Integer;
      Yof : Integer) is

      Alt   : Rnode_Id;
      X_Alt : Integer;
      Y_Alt : Integer;
   begin
      Output.Write_Line ("Offset_If_Vertex => " & Rkind (V)'Img);

      Set_Xabs (V, Xrel (V) + Xof);
      Set_Yabs (V, Yrel (V) + Yof);

      X_Alt := Xabs (V);
      Y_Alt := Yabs (V) + 2*Yunit;


      -- Then graph.

      Offset_Graph (Then_Graph (V), X_Alt, Y_Alt);

      X_Alt := X_Alt + Width (Then_Graph (V)) + Gap_Select_Alternatives;

      -- Alternatives.

      if Present (Alternatives (V)) then
	 Alt := First (Alternatives (V));
	 while Alt /= No_Rnode loop
	    Offset_Graph (Alt, X_Alt, Y_Alt);
	    X_Alt := X_Alt + Width (Alt) + Gap_Select_Alternatives;
	    Next (Alt);
	 end loop;
      end if;

      -- Else.

      if Else_Graph (V) /= No_Rnode then
	 Offset_Graph (Else_Graph (V), X_Alt, Y_Alt);
      end if;

      Set_Xabs_Begin_Cnx (V, Xrel_Begin_Cnx (V) + Xof);
      Set_Yabs_Begin_Cnx (V, Yrel_Begin_Cnx (V) + Yof);

      Set_Xabs_End_Cnx (V, Xrel_End_Cnx (V) + Xof);
      Set_Yabs_End_Cnx (V, Yrel_End_Cnx (V) + Yof);

      Output.Write_Line ("Offset_If_Vertex End");
   end Offset_If_Vertex;

   ------------------------
   -- Offset_Loop_Vertex --
   ------------------------

   procedure Offset_Loop_Vertex
     (V   : Rnode_Id;
      Xof : Integer;
      Yof : Integer) is
   begin
      Output.Write_Line ("Offset_Loop_Vertex => " & Rkind (V)'Img);

      Set_Xabs (V, Xrel (V) + Xof);
      Set_Yabs (V, Yrel (V) + Yof);

      -- Body graph.

      Offset_Graph (Body_Graph (V), Xabs (V), Yabs (V) + Y_Begin (V));

      Set_Xabs_Begin_Cnx (V, Xrel_Begin_Cnx (V) + Xof);
      Set_Yabs_Begin_Cnx (V, Yrel_Begin_Cnx (V) + Yof);

      Set_Xabs_End_Cnx (V, Xrel_End_Cnx (V) + Xof);
      Set_Yabs_End_Cnx (V, Yrel_End_Cnx (V) + Yof);

      Output.Write_Line ("Offset_Loop_Vertex End");
   end Offset_Loop_Vertex;

   ------------------------
   -- Offset_Exit_Vertex --
   ------------------------

   procedure Offset_Exit_Vertex
     (V   : Rnode_Id;
      Xof : Integer;
      Yof : Integer) is
   begin
      Output.Write_Line ("Offset_Exit_Vertex => " & Rkind (V)'Img);

      Set_Xabs (V, Xrel (V) + Xof);
      Set_Yabs (V, Yrel (V) + Yof);

      Set_Width (V, Compute_Down_Width (V));

      Set_Xabs_Begin_Cnx (V, Xrel_Begin_Cnx (V) + Xof);
      Set_Yabs_Begin_Cnx (V, Yrel_Begin_Cnx (V) + Yof);

      Set_Xabs_End_Cnx (V, Xrel_End_Cnx (V) + Xof);
      Set_Yabs_End_Cnx (V, Yrel_End_Cnx (V) + Yof);

      Output.Write_Line ("Offset_Exit_Vertex End");
   end Offset_Exit_Vertex;

   ---------------------
   -- Emit_Svg_Header --
   ---------------------

   procedure Emit_Svg_Header
     (X : Integer;
      Y : Integer) is
   begin
      Write_Out (Svg_Header_Fisrt);

      Write_Out ("   width=""");
      Write_Out (To_String ((X) + 3*Xunit));
      Write_Out ("""");

      Write_Out ("   height=""4200""");
      --  Write_Out (To_String ((Y) + 3*Yunit));
      --  Write_Out ("""");

      Write_Out (Svg_Header_Last);
   end Emit_Svg_Header;


   -----------------
   -- Emit_Header --
   -----------------

   procedure Emit_Header is
   begin

--      Write_Out (Svg_Header);
 --     Newline (2);


      Write_Out ("<defs>");
      Newline (2);

      Line_Write (Wait_Def_Str);
      Newline (2);
      Line_Write (Pause_Def_Str);
      Newline (2);
      Line_Write (Sync_Def_Str);
      Newline (2);
      Line_Write (If_Def_Str);
      Newline (2);
      Line_Write (Vertical_Trans_Def_Str);
      Newline (2);

      Line_Write (Vertical_Top_Fleche_Def_Str);
      Newline (2);

      Line_Write (Vertical_Down_Fleche_Def_Str);
      Newline (2);

      Line_Write (Conection_Def_Str);
      Newline (2);

      Line_Write (Abort_Conection_Def_Str);
      Newline (2);

      Line_Write (Fleche_Inabort_Def_Str);
      Newline (2);

      Line_Write (Fleche_Outabort_Def_Str);
      Newline (2);

      Line_Write (Horizontal_Left_Fleche_Def_Str);
      Newline (2);

      Line_Write (Horizontal_Right_Fleche_Def_Str);
      Newline (2);

      Line_Write (loop_Fleche_Def_Str);
      Newline (2);

      Write_Out ("</defs>");
      Newline (2);

      --  for I in 1..10 loop
      --  	 Line_Write
      --  	   ("   <line x1=""0"" " &
      --  	      "y1=""" & To_String (I*100) & """ " &
      --  	      "x2=""1000"" " &
      --  	      "y2=""" & To_String (I*100) & """ " &
      --  	      " style=""stroke:#3333ff;stroke-width:1.0; fill:none;"" />");
      --  	 Newline;
      --  end loop;

      --  Newline;

      --  for I in 1..7 loop
      --  	 Line_Write
      --  	   ("   <line x1=""" & To_String (I*100) & """ " &
      --  	      "y1=""0"" " &
      --  	      "x2=""" & To_String (I*100) & """ " &
      --  	      "y2=""1000""" &
      --  	      " style=""stroke:#3333ff;stroke-width:1.0; fill:none;"" />");
      --  	 Newline;
      --  end loop;

      --  Newline;


   end Emit_Header;


   -----------------
   -- Emit_Tailer --
   -----------------

   procedure Emit_Tailer is
   begin
      Write_Out (Svg_Tailer);
      Newline;
   end Emit_Tailer;


   ----------------
   -- Emit_Graph --
   ----------------

   procedure Emit_Graph
     (G : Rnode_Id) is

      V : Rnode_Id;
      X : Integer;
      Y : Integer;
   begin
      Output.Write_Line ("Emit_Graph => " & Rkind (G)'img);
      X := Xabs (G);
      Y := Yabs (G);

      V := First (Vertices_List (G));
      while V /= No_Rnode loop
	 Output.Write_Line ("             Vertex => " & Rkind (V)'img);
	 case Rkind (V) is

	    when R_Unused_At_Start =>
	       null;

	    when R_Graph_Vertex =>
	       null;

	    when R_Contionnal_graph_Vertex =>
	       null;

	    when R_Reactive_Begin_Vertex =>
	       null;

	    when R_Reactive_End_Vertex =>
	       null;

	    when R_End_Vertex =>
	       null;

	    when R_Begin_Vertex =>
	       null;

	    when R_Repeat_Loop_Vertex =>
	       null;

	    when R_Handler_Vertex =>
	       null;

	    when R_If_Vertex =>
	       Emit_If_Vertex (V);

	    when R_Loop_Vertex =>
	       Emit_Loop_Vertex (V);

	    when R_Exit_Vertex =>
	       Emit_Exit_Vertex (V);

	    when R_Fork_Vertex =>
	       Emit_Fork_Vertex (V);

	    when R_Abort_Vertex =>
	       Emit_Abort_Vertex (V);

	    when R_Weak_Abort_Vertex =>
	       Emit_Abort_Vertex (V);

	    when R_Pause_Vertex =>
	       Emit_Pause_Vertex (V);

	    when R_Wait_Vertex =>
	       Emit_Wait_Vertex (V);

	    when R_Sync_Vertex =>
	       Emit_Sync_Vertex (V);

	    when R_Select_Vertex =>
	       Emit_Select_Vertex (V);

	    when R_Unused_At_End =>
	       null;
	 end case;

	 Next (V);
      end loop;

      Output.Write_Line ("Emit_Graph End");
   end Emit_Graph;

   ---------------------
   -- Skip_State_Name --
   ---------------------

   function Skip_State_Name (S : String) return String is
   begin
      if S'Length > 6 then
	 return S (S'First..(S'First + 5));
      else
	 return S;
      end if;
   end Skip_State_Name;

   ---------------------
   -- Emit_State_Name --
   ---------------------

   procedure Emit_State_Name
     (V : Rnode_Id;
      X : Integer;
      Y : Integer) is

      S : String := Skip_State_Name (Get_String (Name (V)));
   begin
      To_Mixed (S);
      Line_Write
	("<text "  &
	   "x="""  & To_String (X + Xunit/2) & """"   &
	   " y=""" & To_String (Y + Yunit/2 + Yunit/8) & """"   &
	   " style=""font-family: arial; font-size: 18pt; stroke:black; fill:black; text-anchor: middle"">" & S &
	   "</text>");

   end Emit_State_Name;

   -----------------------
   -- Emit_Pause_Vertex --
   -----------------------

   procedure Emit_Pause_Vertex (V : Rnode_Id) is

      X : Integer;
      Y : Integer;
   begin
      Output.Write_Line ("Emit_Pause_Vertex => " & Rkind (V)'Img);

      X := Xabs (V);
      Y := Yabs (V);

      Line_Write ("<use xlink:href=""#pause"" " &
		    "x=""" & To_String (X) & """ " &
		    "y=""" & To_String (Y) & """ />");

      Output.Write_Line ("Emit_Pause_Vertex End");
   end Emit_Pause_Vertex;

   ----------------------
   -- Emit_Sync_Vertex --
   ----------------------

   procedure Emit_Sync_Vertex (V : Rnode_Id) is

      X : Integer;
      Y : Integer;
   begin
      Output.Write_Line ("Emit_Sync_Vertex => " & Rkind (V)'Img);

      X := Xabs (V);
      Y := Yabs (V);

      Line_Write ("<use xlink:href=""#sync"" " &
		    "x=""" & To_String (X) & """ " &
		    "y=""" & To_String (Y) & """ />");
      Newline;

      Emit_State_Name (V, X, Y);

      Output.Write_Line ("Emit_Sync_Vertex End");
   end Emit_Sync_Vertex;

   ----------------------
   -- Emit_Wait_Vertex --
   ----------------------

   procedure Emit_Wait_Vertex (V : Rnode_Id) is

      X : Integer;
      Y : Integer;
   begin
      Output.Write_Line ("Emit_Wait_Vertex => " & Rkind (V)'Img);

      X := Xabs (V);
      Y := Yabs (V);

      Line_Write ("<use xlink:href=""#wait"" " &
		    "x=""" & To_String (X) & """ " &
		    "y=""" & To_String (Y) & """ />");
      Newline;

      Emit_State_Name (V, X, Y);


      if Next (V) /= No_Rnode then
	 if Xabs (Next (V)) /= 0 then
	    Connect_V1_To_V2
	      (V, Next (V), Y_Before, 0, 0,
	       Xunit/2, -Yunit, Xunit/2, 0);
	 end if;
      end if;

      Output.Write_Line ("Emit_Wait_Vertex End");
   end Emit_Wait_Vertex;

   -----------------------
   -- Emit_Abort_Vertex --
   -----------------------

   procedure Emit_Abort_Vertex (V : Rnode_Id) is

      X : Integer;
      Y : Integer;

      procedure Emit_Begin_Abort is
      begin
	 Line_Write
	   ("<use xlink:href=""#cnx"" " &
	      "x=""" & To_String (Xabs_Begin_Cnx (V)) & """ " &
	      "y=""" & To_String (Yabs_Begin_Cnx (V)) & """ />");

	 -- Connect Cnx to the first state of Body Graph.

	 Connect_X1_Y1_To_V
	   (X1   => Xabs_Begin_Cnx (V),
	    Y1   => Yabs_Begin_Cnx (V),
	    V    => Body_Graph (V),
	    T    => Y_Before,
	    W    => 0,
	    H    => 0,
	    X1of => Xunit/2,
	    Y1of => 0,
	    X2of => Xunit/2,
	    Y2of => 0);

      end Emit_Begin_Abort;

      procedure Emit_End_Cnx is
      begin
	 -- End cnx.

	 Line_Write
	   ("<use xlink:href=""#cnx"" " &
	      "x=""" & To_String (Xabs_End_Cnx (V)) & """ " &
	      "y=""" & To_String (Yabs_End_Cnx (V) - Yunit) & """ />");

      end Emit_End_Cnx;

      procedure Emit_Handler_Line is
	 Xc : Integer;
	 Yc : Integer;
      begin
	 Xc := Xabs (Body_Graph (V)) + Width (Body_Graph (V))+ Xunit/4;
	 Yc := Yabs (Body_Graph (V)) + Yunit;

	 Line_Write
	   ("<use xlink:href=""#abortcnx"" " &
	      "x=""" & To_String (Xc) & """ " &
	      "y=""" & To_String (Yc) & """ />");

	 -- Connect to Handler.

	 Connect_X1_Y1_To_V
	   (X1   => Xc,
	    Y1   => Yc,
	    V    => Handler_Graph (V),
	    T    => X_Before,
	    W    => 0,
	    H    => 0,
	    X1of => 0,
	    Y1of => 0,
	    X2of => Xunit/2,
	    Y2of => 0);

	 -- Connect to End Connection

      if Tail_Vertex (Handler_Graph (V)) /= No_Rnode then
	 Connect_V1_To_End_Cnx
	   (Handler_Graph (V), V, Y_Before, 0, 0,
	    Xunit/2, -Yunit, Xunit/2, -Yunit);
      end if;

      end Emit_Handler_Line;

      procedure Emit_No_Handler_Line is
	 Xc : Integer;
	 Yc : Integer;
      begin
	 Xc := Xabs (Body_Graph (V)) + Width (Body_Graph (V))+ Xunit/2;
	 Yc := Yabs (Body_Graph (V)) + Yunit;

	 Line_Write
	   ("<use xlink:href=""#abortcnx"" " &
	      "x=""" & To_String (Xc) & """ " &
	      "y=""" & To_String (Yc) & """ />");

	 -- Connect to End Connection

	 Connect_X1_Y1_To_X2_Y2
	   (X1   => Xc,
	    Y1   => Yc,
	    X2   => Xabs_End_Cnx (V) + Xunit/2,
	    Y2   => Yabs_End_Cnx (V) - Yunit,
	    T    => X_Before,
	    W    => Xunit,
	    H    => 0);

      end Emit_No_Handler_Line;
   begin
      Output.Write_Line ("Emit_Abort_Vertex => " & Rkind (V)'Img);

      -- Emit The rectangle enclosing abort structure.

      X := Xabs (V);
      Y := Yabs (V);

      Emit_Begin_Abort;

      -- Body sub graph

      Emit_Graph (Body_Graph (V));

      -- Emit Red Body Rectangle.
      Rectangle
	(Xabs (Body_Graph (V)) - Xunit/4,
	 Yabs (Body_Graph (V)) - Yunit/4,
	 Width (Body_Graph (V)) + 2*Xunit/4,
	 Height (Body_Graph (V)) - Yunit + 2*Yunit/4);

      Emit_End_Cnx;

      if Tail_Vertex (Body_Graph (V)) /= No_Rnode then
	 Connect_V1_To_End_Cnx
	   (Body_Graph (V), V, Y_Before, 0, 0,
	    Xunit/2, - Yunit, Xunit/2, - Yunit);
      end if;


      -- Handler Sub Graph

      if Handler_Graph (V) /= No_Rnode then
	 Emit_Graph (Handler_Graph (V));

	 Emit_Handler_Line;
      else
	 Emit_No_Handler_Line;
      end if;


      if Next (V) /= No_Rnode then
	 if Xabs (Next (V)) /= 0 then
	    Connect_V1_To_V2
	      (V, Next (V), Y_Before, 0, 0,
	       Xunit/2, -Yunit, Xunit/2, 0);
	 end if;
      end if;

      Output.Write_Line ("Emit_Abort_Vertex End");
   end Emit_Abort_Vertex;

   ------------------------
   -- Emit_Select_Vertex --
   ------------------------

   procedure Emit_Select_Vertex (V : Rnode_Id) is

      Alt   : Rnode_Id;
      X     : Integer;
      Y     : Integer;
      X1    : Integer;
      Y1    : Integer;
      X2    : Integer;
      Y2    : Integer;
      W     : Integer;

      procedure Emit_Wait_Select is
      begin
	 Line_Write ("<use xlink:href=""#wait"" " &
		       "x=""" & To_String (X) & """ " &
		       "y=""" & To_String (Y) & """ />");


	 Emit_State_Name (V, X, Y);

	 Connect_X1_Y1_To_X2_Y2
	   (X1 => X + Xunit/2,
	    Y1 => Y + Yunit,
	    X2 => Xabs_Begin_Cnx (V) + Xunit/2,
	    Y2 => Yabs_Begin_Cnx (V),
	    T  => Y_Before,
	    W  => 0,
	    H  => 0);

	 Line_Write ("<use xlink:href=""#cnx"" " &
		    "x=""" & To_String (Xabs_Begin_Cnx (V)) & """ " &
		    "y=""" & To_String (Yabs_Begin_Cnx (V)) & """ />");
      end Emit_Wait_Select;


      procedure Emit_Or_Else_Line (Sg : Rnode_Id) is
      begin
	 Connect_X1_Y1_To_V
	   (X1   => Xabs_Begin_Cnx (V),
	    Y1   => Yabs_Begin_Cnx (V),
	    V    => Sg,
	    T    => X_Before,
	    W    => W,
	    H    => 0,
	    X1of => Xunit/2,
	    Y1of => 0,
	    X2of => Xunit/2,
	    Y2of => 0);
      end Emit_Or_Else_Line;

      procedure Emit_End_Select is
      begin
	 Line_Write ("<use xlink:href=""#cnx"" " &
		    "x=""" & To_String (Xabs_End_Cnx (V)) & """ " &
		    "y=""" & To_String (Yabs_End_Cnx (V)) & """ />");
	 Newline;
      end Emit_End_Select;


   begin
      Output.Write_Line ("Emit_Select_Vertex => " & Rkind (V)'Img);

      X := Xabs (V);
      Y := Yabs (V);

      X1 := X;
      Y1 := Y;

      -- Emet wait select

      Emit_Wait_Select;


      -- Emet les alternatives.

      W := 0;

      if Present (Alternatives (V)) then
	 Alt := First (Alternatives (V));
	 while Alt /= No_Rnode loop
	    X2 := Xabs (Alt);
	    Y2 := Yabs (Alt);

	    W := Xabs (Alt) - X;
	    Emit_Or_Else_Line (Alt);

	    Emit_Graph (Alt);

	    if Tail_Vertex (Alt) /= No_Rnode then
	       Connect_V1_To_End_Cnx
		 (Alt, V, Y_Before, W, 0,
		  Xunit/2, -Yunit, Xunit/2, 0);
	    end if;

	    X1 := X2;

	    Next (Alt);
	 end loop;
      end if;

      Emit_End_Select;

      if Next (V) /= No_Rnode then
	 if Xabs (Next (V)) /= 0 then
	    Connect_V1_To_V2
	      (V, Next (V), Y_Before, 0, 0,
	       Xunit/2, -Yunit, Xunit/2, 0);
	 end if;
      end if;

      Output.Write_Line ("Emit_Select_Vertex End");
   end Emit_Select_Vertex;

   -----------------------
   -- Emit_Fork_Vertex --
   -----------------------

   procedure Emit_Fork_Vertex (V : Rnode_Id) is

      Alt   : Rnode_Id;
      X     : Integer;
      Y     : Integer;
      X1    : Integer;
      Y1    : Integer;
      X2    : Integer;
      Y2    : Integer;
      Xcnx  : Integer;
      Ycnx  : Integer;
      W     : Integer;

      procedure Emit_Fork_Line is
      begin
	 Xcnx := Xabs_Begin_Cnx (V);
	 Ycnx := Yabs_Begin_Cnx (V);
      end Emit_Fork_Line;

      procedure Emit_And_Then_Line (Sg : Rnode_Id)  is
	 Xsg : Integer;
      begin
	 Xsg := Xabs (Sg);
	 Connect_X1_Y1_To_V
	   (X1   => Xsg,
	    Y1   => Yabs_Begin_Cnx (V) + Yunit/6, -- + Yunit,
	    V    => Last (Vertices_List (Sg)),
	    T    => Y_Before,
	    W    => 0,
	    H    => 0,
	    X1of => Xunit/2,
	    Y1of => 0,
	    X2of => Xunit/2,
	    Y2of => 0);

      end Emit_And_Then_Line;


      procedure Emit_End_Fork_Line (X_Last : Integer) is
      begin
	 Xcnx := Xabs_Begin_Cnx (V);
	 Ycnx := Yabs_Begin_Cnx (V);

	 -- Begin fork

	 Line_Write ("<use xlink:href=""#cnx"" " &
		    "x=""" & To_String (Xcnx) & """ " &
		    "y=""" & To_String (Ycnx) & """ />");

	 Line_Write
	   ("   <line " &
	      "     style=""stroke:black;stroke-width:3.0; fill:none;"" " & Lf &
	      " x1=""" & To_String (X + Xunit/2 - Xunit/8 ) & """" &
	      " y1=""" & To_String (Ycnx)   & """" &
	      " x2=""" & To_String (X_Last + Xunit/2 + Xunit/8) & """" &
	      " y2=""" & To_String (Ycnx) & """ />");

	 Line_Write
	   ("   <line " &
	      "     style=""stroke:black;stroke-width:3.0; fill:none;"" " & Lf &
	      " x1=""" & To_String (X + Xunit/2 - Xunit/8) & """" &
	      " y1=""" & To_String (Ycnx + Yunit/6)   & """" &
	      " x2=""" & To_String (X_Last + Xunit/2 + Xunit/8) & """" &
	      " y2=""" & To_String (Ycnx + Yunit/6) & """ />");

	 -- End fork

	 Xcnx := Xabs_End_Cnx (V);
	 Ycnx := Yabs_End_Cnx (V);

	 Line_Write
	   ("   <line " &
	      "     style=""stroke:black;stroke-width:3.0; fill:none;"" " & Lf &
	      " x1=""" & To_String (X + Xunit/2 - Xunit/8) & """" &
	      " y1=""" & To_String (Ycnx - Yunit)   & """" &
	      " x2=""" & To_String (X_Last + Xunit/2 + Xunit/8) & """" &
	      " y2=""" & To_String (Ycnx - YUnit) & """ />");

	 Line_Write
	   ("   <line " &
	      "     style=""stroke:black;stroke-width:3.0; fill:none;"" " & Lf &
	      " x1=""" & To_String (X + Xunit/2 - Xunit/8) & """" &
	      " y1=""" & To_String (Ycnx - YUnit + Yunit/6)   & """" &
	      " x2=""" & To_String (X_Last + Xunit/2 + Xunit/8) & """" &
	      " y2=""" & To_String (Ycnx - YUnit + Yunit/6) & """ />");

	 Line_Write ("<use xlink:href=""#cnx"" " &
		    "x=""" & To_String (Xcnx) & """ " &
		    "y=""" & To_String (Ycnx - YUnit + Yunit/6) & """ />");


	 Connect_X1_Y1_To_X2_Y2
	   (X1 => Xcnx + Xunit/2,
	    Y1 => Ycnx - YUnit + Yunit/6,
	    X2 => Xcnx + Xunit/2,
	    Y2 => Ycnx,
	    T  => Y_Before,
	    W  => 0,
	    H  => 0);

      end Emit_End_Fork_Line;

   begin
      Output.Write_Line ("Emit_Fork_Vertex => " & Rkind (V)'Img);


      X := Xabs (V);
      Y := Yabs (V);

      X1 := X;
      Y1 := Y;

      -- Alternatives.

      W := 0;
      if Present (Alternatives (V)) then
	 Alt := First (Alternatives (V));
	 while Alt /= No_Rnode loop
	    X2 := Xabs (Alt);
	    Y2 := Yabs (Alt);

	    Emit_And_Then_Line (Alt);

	    Emit_Graph (Alt);

	    if Tail_Vertex (Alt) /= No_Rnode then
	       Connect_X1_Y1_To_X2_Y2
		 (X1 => Xabs_End_Cnx (Alt) + Xunit/2,
		  Y1 => Yabs_End_Cnx (Alt) - Yunit,
		  X2 => Xabs_End_Cnx (Alt) + Xunit/2,
		  Y2 => Yabs_End_Cnx (V) - Y_End (V),
		  T  => Y_Before,
		  W  => 0,
		  H  => 0);
	    end if;

	    W := Width (Alt) + Gap_Select_Alternatives;
	    X1 := X2;

	    Next (Alt);
	 end loop;
      end if;


      -- Emit convergence ET

      Emit_End_Fork_Line (X2);

      if Next (V) /= No_Rnode
	and then Xabs (Next (V)) /= 0 then
	 Connect_V1_To_V2
	 (V, Next (V), Y_Before, 0, 0,
	  Xunit/2, 0, Xunit/2, 0);
      end if;

      Output.Write_Line ("Emit_Fork_Vertex End");
   end Emit_Fork_Vertex;

   ---------------------
   -- Emit_If_Vertex --
   ---------------------

   procedure Emit_If_Vertex (V : Rnode_Id) is

      Alt : Rnode_Id;
      X   : Integer;
      Y   : Integer;
      X1  : Integer;
      Y1  : Integer;
      X2  : Integer;
      Y2  : Integer;
      W   : Integer;

      procedure Emit_If_Line is
      begin
	 Line_Write ("<use xlink:href=""#if"" " &
		       "x=""" & To_String (X) & """ " &
		       "y=""" & To_String (Y) & """ />");

	 Connect_X1_Y1_To_V
	   (X1   => X,
	    Y1   => Y,
	    V    => Then_Graph (V),
	    T    => Y_Before,
	    W    => 0,
	    H    => 0,
	    X1of => Xunit/2,
	    Y1of => Yunit,
	    X2of => Xunit/2,
	    Y2of => 0);
      end Emit_If_Line;

      procedure Emit_Elsif_Line is
      begin
	 --  =>  \
	 --       +------+
	 --      /

	 Connect_XY1_XY2
	   (X1   => X1 + Xunit,
	    Y1   => Y + Yunit/2,
	    X2   => X2,
	    Y2   => Y + Yunit/2,
	    T    => X_Before);

	 -- Emit Test box
	 Line_Write ("<use xlink:href=""#if"" " &
		       "x=""" & To_String (X2) & """ " &
		       "y=""" & To_String (Y) & """ />");

	 -- Connect Test box to th esub of alternative
	 Connect_X1_Y1_To_V
	   (X1   => X2,
	    Y1   => Y,
	    V    => Alt,
	    T    => Y_Before,
	    W    => 0,
	    H    => 0,
	    X1of => Xunit/2,
	    Y1of => Yunit,
	    X2of => Xunit/2,
	    Y2of => 0);
      end Emit_Elsif_Line;

      procedure Emit_Else_Line is
      begin
	 Connect_X1_Y1_To_V
	   (X1   => X1 + Xunit,
	    Y1   => Y1,
	    V    => Else_Graph (V),
	    T    => X_Before,
	    W    => 0,
	    H    => 0,
	    X1of => 0,
	    Y1of => Yunit/2,
	    X2of => Xunit/2,
	    Y2of => 0);
      end Emit_Else_Line;

      procedure Emit_End_If is
      begin
	 Line_Write ("<use xlink:href=""#cnx"" " &
		    "x=""" & To_String (Xabs_End_Cnx (V)) & """ " &
		    "y=""" & To_String (Yabs_End_Cnx (V)) & """ />");
	 Newline;

	 if Next (V) /= No_Rnode
	   and then Xabs (Next (V)) /= 0 then
	    Connect_V1_To_V2
	      (V, Next (V), Y_Before, 0, 0,
	       Xunit/2, 0, Xunit/2, 0);
	 end if;
      end Emit_End_If;

   begin
      Output.Write_Line ("Emit_If_Vertex => " & Rkind (V)'Img);

      X := Xabs (V);
      Y := Yabs (V);

      X1 := X;
      Y1 := Y;

      Emit_If_Line;

      -- Emit Then graph.

      Emit_Graph (Then_Graph (V));


      if Tail_Vertex (Then_Graph (V)) /= No_Rnode then
	 Connect_V1_To_End_Cnx
	 (Then_Graph (V), V, X_Before, 0, 0,
	  Xunit/2, -Yunit, Xunit/2, 0);
      end if;

      -- Emit Alternatives Graph.

      W := 0;
      if Present (Alternatives (V)) then
	 Alt := First (Alternatives (V));
	 while Alt /= No_Rnode loop
	    X2 := Xabs (Alt);
	    Y2 := Yabs (Alt);

	    Emit_Elsif_Line; --  (X1, Y, X2, Y);

	    Emit_Graph (Alt);

	    if Tail_Vertex (Alt) /= No_Rnode then
	       Connect_V1_To_End_Cnx
	       (Alt, V, Y_Before, W, 0,
		Xunit/2, -Yunit, Xunit/2, 0);
	    end if;

	    W := Width (Alt) + Gap_Select_Alternatives;
	    X1 := X2;
	    Next (Alt);
	 end loop;
      end if;

      -- Emit Else alternatives.

      if Else_Graph (V) /= No_Rnode then
	 X2 := Xabs (Else_Graph (V));

	 Emit_Else_Line;

	 Emit_Graph (Else_Graph (V));

	 if Tail_Vertex (Else_Graph (V)) /= No_Rnode then
	    Connect_V1_To_End_Cnx
	      (Else_Graph (V), V, Y_Before, W, 0,
	       Xunit/2, -Yunit, Xunit/2, 0);
	 end if;
      else
	 X2 := X1 + 2*Xunit;

	 if W = 0 then
	    W := Gap_Select_Alternatives;
	 end if;

	 Connect_Begin_Cnx_To_End_Cnx
	   (V, V, X_Before, W, 0,
	    Xunit, Yunit/2, Xunit/2, 0);
      end if;


      Emit_End_If;

      Output.Write_Line ("Emit_If_Vertex End");
   end Emit_If_Vertex;

   ----------------------
   -- Emit_Loop_Vertex --
   ----------------------

   procedure Emit_Loop_Vertex (V : Rnode_Id) is

      X : Integer;
      Y : Integer;

      procedure Emit_Begin_Loop is
      begin
	 Line_Write ("<use xlink:href=""#cnx"" " &
		       "x=""" & To_String (Xabs_Begin_Cnx (V)) & """ " &
		       "y=""" & To_String (Yabs_Begin_Cnx (V)) & """ />");

	 Connect_X1_Y1_To_V
	   (X1   => Xabs_Begin_Cnx (V),
	    Y1   => Yabs_Begin_Cnx (V),
	    V    => Body_Graph (V),
	    T    => Y_Before,
	    W    => 0,
	    H    => 0,
	    X1of => Xunit/2,
	    Y1of => 0,
	    X2of => Xunit/2,
	    Y2of => 0);
      end Emit_Begin_Loop;

      procedure Emit_Repeat is
      begin
	    Connect_X1_Y1_To_X2_Y2
	      (X1 => Xabs_End_Cnx (Body_Graph (V)) + Xunit/2,
	       Y1 => Yabs_End_Cnx (Body_Graph (V)) - Yunit,
	       X2 => Xabs_Begin_Cnx (V) + Xunit/2,
	       Y2 => Yabs_Begin_Cnx (V),
	       T  => Y_Before,
	       W  => -Xunit,
	       H  => 0);

      end Emit_Repeat;

      procedure Emit_End_Loop is
      begin
	 Line_Write ("<use xlink:href=""#cnx"" " &
		       "x=""" & To_String (Xabs_End_Cnx (V)) & """ " &
		       "y=""" & To_String (Yabs_End_Cnx (V)) & """ />");

      end Emit_End_Loop;

      ----------------
      -- Close_Exit --
      ----------------

      procedure Close_Exit is
	 Ve : Rnode_Id;
	 W  : Integer;
      begin
	 Ve := First (Vertices_List (Body_Graph (V)));
	 while Ve /= No_Rnode loop
	    if Rkind (Ve) = R_Exit_Vertex then
	       W := Compute_Down_Width (Ve);

	       Connect_X1_Y1_To_X2_Y2
		 (X1 => Xabs (Ve) + Xunit,
		  Y1 => YAbs (Ve) + Yunit/2,
		  X2 => Xabs_End_Cnx (V) + Xunit/2,
		  Y2 => Yabs_End_Cnx (V),
		  T  => X_Before,
		  W  => W,
		  H  => 0);
	    end if;
	    Next (Ve);
	 end loop;
      end Close_Exit;

   begin
      X := Xabs (V);
      Y := Yabs (V);

      Emit_Begin_Loop;

      Emit_Graph (Body_Graph (V));

      Emit_Repeat;

      Emit_End_Loop;

      Close_Exit;

      if Next (V) /= No_Rnode
	and then Xabs (Next (V)) /= 0 then
	 Connect_V1_To_V2
	 (V, Next (V), Y_Before, 0, 0,
	  Xunit/2, 0, Xunit/2, 0);
      end if;

   end Emit_Loop_Vertex;

   ----------------------
   -- Emit_Exit_Vertex --
   ----------------------

   procedure Emit_Exit_Vertex (V : Rnode_Id) is
      X : Integer;
      Y : Integer;
   begin
      X := Xabs (V);
      Y := Yabs (V);

      Line_Write ("<use xlink:href=""#if"" " &
		    "x=""" & To_String (X) & """ " &
		    "y=""" & To_String (Y) & """ />");

      if Next (V) /= No_Rnode
	and then Xabs (Next (V)) /= 0 then
	 Connect_V1_To_V2
	 (V, Next (V), Y_Before, 0, 0,
	  Xunit/2, 0, Xunit/2, 0);
      end if;
   end Emit_Exit_Vertex;


   ----------------------
   -- Hprizontal_Arrow --
   ----------------------

   procedure Horizontal_Arrow
     (X1 : Integer;
      X2 : Integer;
      Y  : Integer) is
   begin
      if X2 > X1 then
	 Line_Write ("<use xlink:href=""#hlfleche"" " &
		       "x=""" & To_String (X2 - Xunit/4) & """ " &
		       "y=""" & To_String (Y - Yunit/2) & """ />");
      else
	 Line_Write ("<use xlink:href=""#hrfleche"" " &
		       "x=""" & To_String (X2 + Xunit/10) & """ " &
		       "y=""" & To_String (Y - Yunit/2) & """ />");
      end if;
   end Horizontal_Arrow;

   --------------------
   -- vertical_Arrow --
   --------------------

   procedure Vertical_Arrow
     (Y1 : Integer;
      Y2 : Integer;
      X  : Integer) is
   begin
      if Y2 > Y1 then
	 Line_Write ("<use xlink:href=""#vdfleche"" " &
		       "x=""" & To_String (X  - Xunit/2) & """ " &
		       "y=""" & To_String (Y2 - Yunit/4) & """ />");
      else
	 Line_Write ("<use xlink:href=""#vtfleche"" " &
		       "x=""" & To_String (X  - Xunit/2) & """ " &
		       "y=""" & To_String (Y2 - Yunit/10) & """ />");
      end if;
   end Vertical_Arrow;

   ----------------------------
   -- Connect_X1_Y1_To_X2_Y2 --
   ----------------------------

   procedure Connect_X1_Y1_To_X2_Y2
     (X1   : Integer;
      Y1   : Integer;
      X2   : Integer;
      Y2   : Integer;
      T    : Line_Type;
      W    : Integer;
      H    : Integer) is

   begin
      if X1 = X2 and then Y1 = Y2
	and then W = 0 and then H = 0 then
	 return;
      end if;

      if T = X_Before then
	 if W = 0 then
	    if X1 = X2 then
	       Line_Write
		 ("<path d=""M" & " " &
		    To_String (X1) &  " " & To_String (Y1) &
		    " V " & To_String (Y2) & """ " &
		    " style=""stroke:gray;stroke-width:3.0;  stroke-linejoin: round; fill:none;"" />");

	       Vertical_Arrow (Y1, Y2, X2);

	    else
	       if Y1 = Y2 then
		  Line_Write
		    ("<path d=""M" & " " &
		       To_String (X1) &  " " & To_String (Y1) &
		       " H " & To_String (X2) & """ " &
		       " style=""stroke:gray;stroke-width:3.0; stroke-linejoin: round; fill:none;"" />");

		  Horizontal_Arrow (X1, X2, Y2);
	       else
		  Line_Write
		    ("<path d=""M" & " " &
		       To_String (X1) &  " " & To_String (Y1) &
		       " H " & To_String (X2) &
		       " V " & To_String (Y2) & """ " &
		       " style=""stroke:gray;stroke-width:3.0; stroke-linejoin: round; fill:none;"" />");

		  Vertical_Arrow (Y1, Y2, X2);
	       end if;
	    end if;
	 else
	    if X1 + W /= X2 then
	       Line_Write
		 ("<path d=""M" & " " &
		    To_String (X1) &  " " & To_String (Y1) &
		    " H " & To_String (X1 + W)  &
		    " V " & To_String (Y2) &
		    " H " & To_String (X2) & """ " &
		    " style=""stroke:gray;stroke-width:3.0; stroke-linejoin: round; fill:none;"" />");

	       Horizontal_Arrow (X1, X2, Y2);
	    else
	       Line_Write
		 ("<path d=""M" & " " &
		    To_String (X1) &  " " & To_String (Y1) &
		    " H " & To_String (X1 + W)  &
		    " V " & To_String (Y2) & """ " &
		    " style=""stroke:gray;stroke-width:3.0; stroke-linejoin: round; fill:none;"" />");

	       Vertical_Arrow (Y1, Y2, X2);
	    end if;
	 end if;

	 -- Y_Before --
	 --------------

      else
	 if H = 0 then
	    if Y1 = Y2 then
	       Line_Write
		 ("<path d=""M" & " " &
		    To_String (X1) &  " " & To_String (Y1) &
		    " H " & To_String (X2) & """ " &
		    " style=""stroke:gray;stroke-width:3.0;  stroke-linejoin: round; fill:none;"" />");

	       Horizontal_Arrow (X1, X2, Y2);

	    else
	       if X1 = X2 then
		  if W = 0 then
		     Line_Write
		       ("<path d=""M" & " " &
			  To_String (X1) &  " " & To_String (Y1) &
			  " V " & To_String (Y2) & """ " &
			  " style=""stroke:gray;stroke-width:3.0;  stroke-linejoin: round; fill:none;"" />");
		     Vertical_Arrow (Y1, Y2, X2);

		  else
		     Line_Write
		       ("<path d=""M" & " " &
			  To_String (X1) &  " " & To_String (Y1) &
			  " V " & To_String (Y1 + Yunit/2) & " " &
			  " H " & To_String (X1 + W)       & " " &
			  " V " & To_String (Y2)           & " " &
			  " H " & To_String (X2)           & """ " &
			  " style=""stroke:gray;stroke-width:3.0;  stroke-linejoin: round; fill:none;"" />");
		     Line_Write ("<use xlink:href=""#loopfleche"" " &
				   "x=""" & To_String (X2 - Xunit/2) & """ " &
				   "y=""" & To_String (Y2 - Yunit/2) & """ />");

		  end if;

	       else
		  Line_Write
		    ("<path d=""M" & " " &
		       To_String (X1) &  " " & To_String (Y1) &
		       " V " & To_String (Y2) &
		       " H " & To_String (X2) & """ " &
		       " style=""stroke:gray;stroke-width:3.0;  stroke-linejoin: round; fill:none;"" />");
		  Horizontal_Arrow (X1, X2, Y2);
	       end if;
	    end if;
	 else
	    Line_Write
	      ("<path d=""M" & " " &
		 To_String (X1) &  " " & To_String (Y1) &
		 " V " & To_String (H)  &
		 " H " & To_String (X2) &
		 " V " & To_String (Y2) & """ " &
		 " style=""stroke:gray;stroke-width:3.0;  stroke-linejoin: round; fill:none;"" />");

	    Vertical_Arrow (Y1, Y2, X2);
	 end if;
      end if;
   end Connect_X1_Y1_To_X2_Y2;

   ----------------------------
   -- Abort_Connect_X1_Y1_To_X2_Y2 --
   ----------------------------

   procedure Abort_Connect_X1_Y1_To_X2_Y2
     (X1   : Integer;
      Y1   : Integer;
      X2   : Integer;
      Y2   : Integer;
      T    : Line_Type;
      W    : Integer;
      H    : Integer) is

   begin
      if X1 = X2 and then Y1 = Y2
	and then W = 0 and then H = 0 then
	 return;
      end if;

      if T = X_Before then
	 if W = 0 then
	    if X1 = X2 then
	       Line_Write
		 ("<path d=""M" & " " &
		    To_String (X1) &  " " & To_String (Y1) &
		    " V " & To_String (Y2) & """ " &
		    " style=""stroke:red;stroke-width:3.0;  stroke-linejoin: round; fill:none;"" />");

	       Vertical_Arrow (Y1, Y2, X2);

	    else
	       if Y1 = Y2 then
		  Line_Write
		    ("<path d=""M" & " " &
		       To_String (X1) &  " " & To_String (Y1) &
		       " H " & To_String (X2) & """ " &
		       " style=""stroke:red;stroke-width:3.0; stroke-linejoin: round; fill:none;"" />");

		  Horizontal_Arrow (X1, X2, Y2);
	       else
		  Line_Write
		    ("<path d=""M" & " " &
		       To_String (X1) &  " " & To_String (Y1) &
		       " H " & To_String (X2) &
		       " V " & To_String (Y2) & """ " &
		       " style=""stroke:red;stroke-width:3.0; stroke-linejoin: round; fill:none;"" />");

		  Vertical_Arrow (Y1, Y2, X2);
	       end if;
	    end if;
	 else
	    if X1 + W /= X2 then
	       Line_Write
		 ("<path d=""M" & " " &
		    To_String (X1) &  " " & To_String (Y1) &
		    " H " & To_String (X1 + W)  &
		    " V " & To_String (Y2) &
		    " H " & To_String (X2) & """ " &
		    " style=""stroke:red;stroke-width:3.0; stroke-linejoin: round; fill:none;"" />");

	       Horizontal_Arrow (X1, X2, Y2);
	    else
	       Line_Write
		 ("<path d=""M" & " " &
		    To_String (X1) &  " " & To_String (Y1) &
		    " H " & To_String (X1 + W)  &
		    " V " & To_String (Y2) & """ " &
		    " style=""stroke:red;stroke-width:3.0; stroke-linejoin: round; fill:none;"" />");

	       Vertical_Arrow (Y1, Y2, X2);
	    end if;
	 end if;

	 -- Y_Before --
	 --------------

      else
	 if H = 0 then
	    if Y1 = Y2 then
	       Line_Write
		 ("<path d=""M" & " " &
		    To_String (X1) &  " " & To_String (Y1) &
		    " H " & To_String (X2) & """ " &
		    " style=""stroke:red;stroke-width:3.0;  stroke-linejoin: round; fill:none;"" />");

	       Horizontal_Arrow (X1, X2, Y2);

	    else
	       if X1 = X2 then
		  if W = 0 then
		     Line_Write
		       ("<path d=""M" & " " &
			  To_String (X1) &  " " & To_String (Y1) &
			  " V " & To_String (Y2) & """ " &
			  " style=""stroke:red;stroke-width:3.0;  stroke-linejoin: round; fill:none;"" />");
		     Vertical_Arrow (Y1, Y2, X2);

		  else
		     Line_Write
		       ("<path d=""M" & " " &
			  To_String (X1) &  " " & To_String (Y1) &
			  " V " & To_String (Y1 + Yunit/2) & " " &
			  " H " & To_String (X1 + W)       & " " &
			  " V " & To_String (Y2)           & " " &
			  " H " & To_String (X2)           & """ " &
			  " style=""stroke:red;stroke-width:3.0;  stroke-linejoin: round; fill:none;"" />");
		     Line_Write ("<use xlink:href=""#loopfleche"" " &
				   "x=""" & To_String (X2 - Xunit/2) & """ " &
				   "y=""" & To_String (Y2 - Yunit/2) & """ />");

		  end if;

	       else
		  Line_Write
		    ("<path d=""M" & " " &
		       To_String (X1) &  " " & To_String (Y1) &
		       " V " & To_String (Y2) &
		       " H " & To_String (X2) & """ " &
		       " style=""stroke:red;stroke-width:3.0;  stroke-linejoin: round; fill:none;"" />");
		  Horizontal_Arrow (X1, X2, Y2);
	       end if;
	    end if;
	 else
	    Line_Write
	      ("<path d=""M" & " " &
		 To_String (X1) &  " " & To_String (Y1) &
		 " V " & To_String (H)  &
		 " H " & To_String (X2) &
		 " V " & To_String (Y2) & """ " &
		 " style=""stroke:red;stroke-width:3.0;  stroke-linejoin: round; fill:none;"" />");

	    Vertical_Arrow (Y1, Y2, X2);
	 end if;
      end if;
   end Abort_Connect_X1_Y1_To_X2_Y2;

   ---------------------
   -- Connect_XY1_XY2 --
   ---------------------

   procedure Connect_XY1_XY2
     (X1 : Integer;
      Y1 : Integer;
      X2 : Integer;
      Y2 : Integer;
      T  : Line_Type) is
   begin
      Connect_X1_Y1_To_X2_Y2
	(X1, Y1,
	 X2, Y2,
	 T, 0, 0);
   end Connect_XY1_XY2;

   ----------------------
   -- Connect_V1_To_V2 --
   ----------------------

   procedure Connect_V1_To_V2
     (V1   : Rnode_Id;
      V2   : Rnode_Id;
      T    : Line_Type;
      W    : Integer;
      H    : Integer;
      X1of : Integer;
      Y1of : Integer;
      X2of : Integer;
      Y2of : Integer) is

      X1 : Integer;
      Y1 : Integer;
      X2 : Integer;
      Y2 : Integer;
   begin
      X1 := Xabs_End_Cnx (V1) + X1of;
      Y1 := Yabs_End_Cnx (V1) + Y1of;

      X2 := Xabs_Begin_Cnx (V2) + X2of;
      Y2 := Yabs_Begin_Cnx (V2) + Y2of;

      Connect_X1_Y1_To_X2_Y2
	(X1, Y1,
	 X2, Y2,
	 T,
	 W,
	 H);
   end Connect_V1_To_V2;

   -----------------------------
   -- Connect_Begin_Cnx_To_V2 --
   -----------------------------

   procedure Connect_Begin_Cnx_To_V2
     (V1   : Rnode_Id;
      V2   : Rnode_Id;
      T    : Line_Type;
      W    : Integer;
      H    : Integer;
      X1of : Integer;
      Y1of : Integer;
      X2of : Integer;
      Y2of : Integer) is

      X1 : Integer;
      Y1 : Integer;
      X2 : Integer;
      Y2 : Integer;
   begin
      X1 := Xabs_Begin_Cnx (V1) + X1of;
      Y1 := Yabs_Begin_Cnx (V1) + Y1of;

      X2 := Xabs_Begin_Cnx (V2) + X2of;
      Y2 := Yabs_Begin_Cnx (V2) + Y2of;

      Connect_X1_Y1_To_X2_Y2
	(X1, Y1,
	 X2, Y2,
	 T,
	 W,
	 H);
   end Connect_Begin_Cnx_To_V2;

   ---------------------------
   -- Connect_V1_To_End_Cnx --
   ---------------------------

   procedure Connect_V1_To_End_Cnx
     (V1   : Rnode_Id;
      V2   : Rnode_Id;
      T    : Line_Type;
      W    : Integer;
      H    : Integer;
      X1of : Integer;
      Y1of : Integer;
      X2of : Integer;
      Y2of : Integer) is

      X1 : Integer;
      Y1 : Integer;
      X2 : Integer;
      Y2 : Integer;
   begin
      X1 := Xabs_End_Cnx (V1) + X1of;
      Y1 := Yabs_End_Cnx (V1) + Y1of;

      X2 := Xabs_End_Cnx (V2) + X2of;
      Y2 := Yabs_End_Cnx (V2) + Y2of;

      Connect_X1_Y1_To_X2_Y2
	(X1, Y1,
	 X2, Y2,
	 T,
	 W,
	 H);
   end Connect_V1_To_End_Cnx;

   ----------------------------------
   -- Connect_Begin_Cnx_To_End_Cnx --
   ---------------------------------

   procedure Connect_Begin_Cnx_To_End_Cnx
     (V1   : Rnode_Id;
      V2   : Rnode_Id;
      T    : Line_Type;
      W    : Integer;
      H    : Integer;
      X1of : Integer;
      Y1of : Integer;
      X2of : Integer;
      Y2of : Integer) is

      X1 : Integer;
      Y1 : Integer;
      X2 : Integer;
      Y2 : Integer;
   begin
      X1 := Xabs_Begin_Cnx (V1) + X1of;
      Y1 := Yabs_Begin_Cnx (V1) + Y1of;

      X2 := Xabs_End_Cnx (V2) + X2of;
      Y2 := Yabs_End_Cnx (V2) + Y2of;

      Connect_X1_Y1_To_X2_Y2
	(X1, Y1,
	 X2, Y2,
	 T,
	 W,
	 H);
   end Connect_Begin_Cnx_To_End_Cnx;

   ------------------------
   -- Connect_X1_Y1_To_V --
   ------------------------

   procedure Connect_X1_Y1_To_V
     (X1   : Integer;
      Y1   : Integer;
      V    : Rnode_Id;
      T    : Line_Type;
      W    : Integer;
      H    : Integer;
      X1of : Integer;
      Y1of : Integer;
      X2of : Integer;
      Y2of : Integer) is

      X2 : Integer;
      Y2 : Integer;
   begin
      X2 := Xabs_Begin_Cnx (V) + X2of;
      Y2 := Yabs_Begin_Cnx (V) + Y2of;

      Connect_X1_Y1_To_X2_Y2
	(X1 + X1of, Y1 + Y1of,
	 X2, Y2,
	 T,
	 W,
	 H);
   end Connect_X1_Y1_To_V;

   ---------------
   -- Rectangle --
   ---------------

   procedure Rectangle
     (X : Integer;
      Y : Integer;
      W : Integer;
      H : Integer) is
   begin
      Line_Write
	("<rect " &
	   "x="""      & To_String (X) & """ " &
	   "y="""      & To_String (Y) & """ " &
	   "width="""  & To_String (W) & """ " &
	   "height=""" & To_String (H) & """ " &
	   "rx=""10""" &
	   " style=""stroke:orangered;stroke-width:3.0; fill:none;"" />");
   end Rectangle;


   -----------------------
   -- Set_Output_Buffer --
   -----------------------

   procedure Set_Output_Buffer (Obf : in Output_Buffer) is
   begin
      Out_Buf := Obf;
   end Set_Output_Buffer;


   -----------------------
   -- Set_Output_Buffer --
   -----------------------

   function Get_Output_Buffer return Output_Buffer is
   begin
      return Out_Buf;
   end Get_Output_Buffer;


   ---------------
   -- To_String --
   ---------------

   function To_String (X : Integer) return String is
   begin
      return Trim (X'Img, Both);
   end To_String;


   --------------------
   -- Do_Indentation --
   --------------------

   procedure Do_Indentation is
   begin
      Do_Indentation (Out_Buf);
   end Do_Indentation;


   ---------------------
   -- Dec_Indentation --
   ---------------------

   procedure Dec_Indentation is
   begin
      Dec_Indentation (Out_Buf);
   end Dec_Indentation;


   ---------------------
   -- Inc_Indentation --
   ---------------------

   procedure Inc_Indentation is
   begin
      Inc_Indentation (Out_Buf);
   end Inc_Indentation;


   ---------------------
   -- Open_Svg_Output --
   ---------------------

   procedure Open_Svg_Output (F : String) is
   begin
      Output.Write_Line ("Open_Gls ========> " & F);

      Ada.Text_Io.Create (Svg_File, Out_File, F);

      Svg_File_Open := True;

   exception
      when others =>
         Output.Write_Line ("erreur a l'ouverture de " & F);
         raise Program_Error;
   end Open_Svg_Output;


   ----------------------
   -- Close_Svg_Output --
   ----------------------

   procedure Close_Svg_Output is
   begin
      if not Svg_File_Open then
         raise Program_Error;
      end if;

      Close (Svg_File);
      Svg_File_Open := False;
   end Close_Svg_Output;


   -------------
   -- Newline --
   -------------

   procedure Newline (I : Natural := 1) is
   begin
      for X in 1..I loop
         Out_Newline (Out_Buf);
      end loop;
   end Newline;


   ---------------
   -- Write_Out --
   ---------------

   procedure Write_Out (S : String) is
   begin
      Do_Indentation (Out_Buf);
      Out_Write (Out_Buf, S);
   end Write_Out;


   ----------------
   -- Line_Write --
   ----------------

   procedure Line_Write (S : String) is
   begin
      Out_Write (Out_Buf, S);
      Newline;
   end Line_Write;

end Graphs_Svg;
