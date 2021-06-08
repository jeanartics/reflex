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
with Types;  use Types;
with Nmake;  use Nmake;
with Sinfo;  use Sinfo;
with Stand;  use Stand;
with Namet;  use Namet;
with Nlists; use Nlists;
with Atree;  use Atree;

with Back_End; use Back_End;
with Checks;
with Comperr;
with Reflex.Expansion;
with Unity.Generator;
with Csets;    use Csets;
with Debug;    use Debug;
with Elists;
with Errout;   use Errout;
with Exp_CG;
with Fmap;
with Fname;    use Fname;
with Fname.UF; use Fname.UF;
with Frontend;
with Ghost;
with Gnatvsn;  use Gnatvsn;
with Inline;
with Lib;      use Lib;
with Lib.Writ; use Lib.Writ;
with Lib.Xref;
with Opt;      use Opt;
with Osint;    use Osint;
with Osint.C;  use Osint.C;
with Output;   use Output;
with Par_SCO;
with Prepcomp;
with Repinfo;  use Repinfo;
with Restrict;
with Rident;   use Rident;
with Rtsfind;
with SCOs;
with Sem;
with Sem_Ch8;
with Sem_Ch12;
with Sem_Ch13;
with Sem_Elim;
with Sem_Eval;
with Sem_Type;
with Set_Targ;
with Snames;
with Sprint;   use Sprint;
with Stringt;
with Stylesw;  use Stylesw;
with Targparm; use Targparm;
with Tbuild;
with Tree_Gen;
with Treepr;   use Treepr;
with Ttypes;
with Uintp;    use Uintp;
with Uname;    use Uname;
with Urealp;
with Usage;
with Validsw;  use Validsw;

with CStand;
with Exp_Dbug;
with Exp_Unst;
with Lib.Load; use Lib.Load;
with Live;     use Live;
with Par;
with Prep;
with Scn;      use Scn;
with Sem_Aux;
with Sem_SCIL;
with Sem_Elab; use Sem_Elab;
with Sem_Prag; use Sem_Prag;
with Sem_Warn; use Sem_Warn;
with Sinput;   use Sinput;
with Sinput.L; use Sinput.L;
with SCIL_LL;  use SCIL_LL;
with Reflex.Fbd_Builders; use Reflex.Fbd_Builders;
with Reflex.Fbd_Dispatch; use Reflex.Fbd_Dispatch;
with Artics.Graph.Graphs; use Artics.Graph.Graphs;
with Ada.Text_IO; use Ada.Text_IO;
--  with Gtk.Main; use Gtk.Main;
--  with Gtk.Main;

--  with Rxed.Application;
with Artics.Graph.Names;
--  with Artics.Graph.Canvas;
--  with Artics.Graph.Uiconstants;
with Artics.Graph.Graph_Actions_Names;
--  with Artics.Graph.Graph_Handlers;

with Artics.Graphics.Html_Colors;
--  with Artics.Rxwt.Cursors;
--  with Artics.Fbd.Ui_Driver;

with Ada.Command_Line;

package body Driver1 is

   procedure Init is
   begin
      Osint.Initialize;
      Fmap.Reset_Tables;
      Lib.Initialize;
      Lib.Xref.Initialize;
      Scan_Compiler_Arguments;
      Osint.Add_Default_Search_Dirs;
      Atree.Initialize;
      Nlists.Initialize;
      Sinput.Initialize;
      Sem.Initialize;
      Exp_CG.Initialize;
      Csets.Initialize;
      Uintp.Initialize;
      Urealp.Initialize;
      Errout.Initialize;
      SCOs.Initialize;
      Snames.Initialize;
      Stringt.Initialize;
      Ghost.Initialize;
      Inline.Initialize;
      Par_SCO.Initialize;
      Sem_Ch8.Initialize;
      Sem_Ch12.Initialize;
      Sem_Ch13.Initialize;
      Sem_Elim.Initialize;
      Sem_Eval.Initialize;
      Sem_Type.Init_Interp_Tables;

      Rtsfind.Initialize;
      Nlists.Initialize;
      Elists.Initialize;
      Lib.Load.Initialize;
      Sem_Aux.Initialize;
      Sem_Ch8.Initialize;
      Sem_Prag.Initialize;
      Fname.UF.Initialize;
      Checks.Initialize;
      Sem_Warn.Initialize;
      Prep.Initialize;
      --        Gtk.Main.Init;

      Artics.Namet.Initialize;

      Artics.Graph.Names.Initialize;
      --        Artics.Rxwt.Cursors.Initialize_Cursors;
      --        Artics.Graph.Canvas.Initialize;
      Artics.Graph.Graph_Actions_Names.Initialze_Action_Names;
      -- Artics.Graph.Graph_Handlers.Initialize_Graph_Handler;

      Artics.Graphics.Html_Colors.Initialize_Html_Colors;
      --        Artics.Graph.Uiconstants.Initialize_Ui_Constants;
      --        Artics.Fbd.Ui_Driver.Initialize_Fbd;

   end Init;

   procedure Run is
      Op_And1 : Node_Id;
      Op_Or1  : Node_Id;
      Id1     : Node_Id;
      Id2     : Node_Id;
      Id3     : Node_Id;
      Id4     : Node_Id;
      Proc1   : Node_Id;
      Assign1 : Node_Id;
      Param1  : Node_Id;
      Param2  : Node_Id;
      Param3  : Node_Id;
      Selector1 : Node_Id;
      Selector2 : Node_Id;
      Selector3 : Node_Id;
      Params_List : List_Id := New_List;

      This    : access Fbd_Builder_Record;
   begin
      This := New_Builder;

      Id1 := Make_Identifier (1, String_Find ("id1"));
      Id2 := Make_Identifier (1, String_Find ("id2"));
      Id3 := Make_Identifier (1, String_Find ("id3"));
      Id4 := Make_Identifier (1, String_Find ("id4"));
      Selector1 := Make_Identifier (1, String_Find ("Selector1"));
      Selector2 := Make_Identifier (1, String_Find ("Selector2"));
      Selector3 := Make_Identifier (1, String_Find ("Selector3"));
      Set_In_Present  (Selector1, True);
      Set_In_Present  (Selector2, True);
      Set_In_Present  (Selector3, True);
      Set_Out_Present (Selector3, True);

      Op_And1 := Make_Op_And (1, Id1, Id2);
      Op_Or1  := Make_Op_Or (1, Id3, Op_And1);

      Assign1 := Make_Assignment_Statement (1, Id4, Op_And1);

      Param1 := Make_Parameter_Association (1, Selector1, Id1);
      Param2 := Make_Parameter_Association (1, Selector2, Id2);
      Param3 := Make_Parameter_Association (1, Selector3, Id3);
      Append (Param1, Params_List);
      Append (Param2, Params_List);
      Append (Param3, Params_List);

      Proc1   := Make_Procedure_Call_Statement (1, Id3, Params_List);

      --        Fbd_Node_Dispatch (This, Op_Or1);
      --        Fbd_Node_Dispatch (This, Assign1);
      Fbd_Node_Dispatch (This, Proc1);

   end Run;
end Driver1;
