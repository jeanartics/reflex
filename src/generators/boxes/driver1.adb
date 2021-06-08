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

with Reflex.Boxes.Dispatch; use Reflex.Boxes.Dispatch;
with Reflex.Boxes;          use Reflex.Boxes;
with Ada.Text_IO; use Ada.Text_IO;
with Reflex.Infos; use Reflex.Infos;
with Reflex_Options; use Reflex_Options;
with Reflex.Boxes.Builders; use Reflex.Boxes.Builders;

package body Driver1 is
   Id0 : Node_Id;
   Id1 : Node_Id;
   Id2 : Node_Id;
   Id3 : Node_Id;
   Id4 : Node_Id;
   Id5 : Node_Id;
   Id6 : Node_Id;
   Id7 : Node_Id;
   Id8 : Node_Id;
   Id9 : Node_Id;
   Id10: Node_Id;
   Id11: Node_Id;
   Id12: Node_Id;
   Id13: Node_Id;
   Id14: Node_Id;
   Id15: Node_Id;
   Id16: Node_Id;

   Op_Eq1      : Node_Id;
   Op_Lt1      : Node_Id;

   Op_And1     : Node_Id;
   Op_And2     : Node_Id;

   Op_Or1      : Node_Id;
   Op_Or2      : Node_Id;

   Assign0     : Node_Id;
   Assign2     : Node_Id;
   Assign5     : Node_Id;

   Goto1       : Node_Id;
   Goto2       : Node_Id;
   Goto3       : Node_Id;
   Goto4       : Node_Id;
   Goto5       : Node_Id;
   Goto6       : Node_Id;
   Label1      : Node_Id;
   Label2      : Node_Id;

   If_Stmt1    : Node_Id;
   If_Stmt2    : Node_Id;
   Stmts1      : List_Id;
   Stmts2      : List_Id;

   A : Node_Id;
   B : Node_Id;
   C : Node_Id;
   D : Node_Id;
   E : Node_Id;

   F : Node_Id;
   G : Node_Id;
   H : Node_Id;
   I : Node_Id;
   J : Node_Id;
   K : Node_Id;
   Id_Lab : Node_Id;

   If1 : Node_Id;
   If2 : Node_Id;
   If3 : Node_Id;

   Elif     : List_Id;
   IfStmts1 : List_Id;
   IfStmts2 : List_Id;
   IfStmts3 : List_Id;

   Assigna1 : Node_Id;
   Assigna2 : Node_Id;
   Assigna3 : Node_Id;

   Op_Plus : Node_Id;
   Op_Mul  : Node_Id;

   Jump1 : Node_Id;
   Jump2 : Node_Id;
   Jump3 : Node_Id;

   Lab1  : Node_Id;

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

      CStand.Create_Standard;

      Id0 := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("Id0"));
      Set_Etype (Id0, Standard_Boolean);

      Id1 := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("Id1"));
      Set_Etype (Id1, Standard_Boolean);

      Id2 := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("Id2"));
      Set_Etype (Id2, Standard_Boolean);

      Id3 := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("Id3"));
      Set_Etype (Id3, Standard_Boolean);

      Id4 := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("Id4"));
      Set_Etype (Id4, Standard_Boolean);

      Id5 := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("Id5"));
      Set_Etype (Id5, Standard_Boolean);

      Id6 := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("Id6"));
      Set_Etype (Id6, Standard_Integer);

      Id7 := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("Id7"));
      Set_Etype (Id7, Standard_Boolean);

      Id8 := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("Id8"));
      Set_Etype (Id8, Standard_Integer);

      Id9 := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("Id9"));
      Set_Etype (Id9, Standard_Integer);

      Id10 := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("Id10"));
      Set_Etype (Id10, Standard_Integer);

      Id11 := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("Id11"));
      Set_Etype (Id11, Standard_Boolean);

      Id12 := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("Id12"));
      Set_Etype (Id12, Standard_Boolean);

      Id13 := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("Id13"));
      Set_Etype (Id13, Standard_Boolean);

      Id14 := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("Id14"));
      Set_Etype (Id14, Standard_Boolean);

      Id15 := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("Id15"));
      Set_Etype (Id15, Standard_Boolean);

      Id16 := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("Id16"));
      Set_Etype (Id16, Standard_Boolean);

      Op_Lt1 := Make_Op_Lt
        (Sloc       => 1,
         Left_Opnd  => Id6,
         Right_Opnd => Id8);

      Op_Eq1 := Make_Op_Eq
        ( Sloc       => 1,
          Left_Opnd  => Id9,
          Right_Opnd => Id6);

      Op_Or1 := Make_Op_Or
        (Sloc       => 1,
         Left_Opnd  => Id5,
         Right_Opnd => Id4);

      Op_And1 := Make_Op_And
        (Sloc       => 1,
         Left_Opnd  => Id3,
         Right_Opnd => Id2);

      Op_And2 := Make_Op_And
        (Sloc       => 1,
         Left_Opnd  => Op_And1,
         Right_Opnd => Op_Or1);

      Op_Or2 := Make_Op_Or
        (Sloc       => 1,
         Left_Opnd  => Id12,
         Right_Opnd => Id13);

      Assign2 := Make_Assignment_Statement
        (Sloc       => 1,
         Name       => Id1,
         Expression => Op_And2);

      Assign0 := Make_Assignment_Statement
        (Sloc       => 1,
         Name       => Id14,
         Expression => Op_Or2);

      Assign5 := Make_Assignment_Statement
        (Sloc       => 1,
         Name       => Id7,
         Expression => Op_Eq1);

      Label1 := Make_Label
        (Sloc       => 1,
         Identifier => Id16);

      Label2 := Make_Label
        (Sloc       => 1,
         Identifier => Id15);

      Goto1 := Make_Goto_Statement
        (Sloc => 1,
         Name => Label2);
      Goto2 := Make_Goto_Statement
        (Sloc => 1,
         Name => Label2);
      Goto3 := Make_Goto_Statement
        (Sloc => 1,
         Name => Label2);
      Goto4 := Make_Goto_Statement
        (Sloc => 1,
         Name => Label2);
      Goto5 := Make_Goto_Statement
        (Sloc => 1,
         Name => Label2);
      Goto6 := Make_Goto_Statement
        (Sloc => 1,
         Name => Label1);


      Stmts1 := New_List;
      Append (Goto1, Stmts1);
      Append (Assign0, Stmts1);
      Append (Goto6, Stmts1);

      If_Stmt1 := Make_If_Statement
        (Sloc            => 1,
         Condition       => Id11,
         Then_Statements => Stmts1);

      Stmts2 := New_List;
      Append (Goto3, Stmts2);
      Append (Assign2, Stmts2);
      Append (Assign5, Stmts2);
      Append (If_Stmt1, Stmts2);
      Append (Goto2, Stmts2);

      If_Stmt2 := Make_If_Statement
        (Sloc            => 1,
         Condition       => Id0,
         Then_Statements => Stmts2);

      -----------------
      --  Test Elsif --
      -----------------
      --  if A then
      --  F := G + H;
      --  goto Lend;

      --  elsif B then
      --  I := J * K;
      --  goto Lend;

      --  elsif C then
      --  D := E;
      --  Goto Lend;

      --  Lend;

      A := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("A"));
      Set_Etype (A, Standard_Boolean);

      B := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("B"));
      Set_Etype (B, Standard_Boolean);

      C := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("C"));
      Set_Etype (C, Standard_Boolean);

      D := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("D"));
      Set_Etype (D, Standard_Boolean);

      E := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("E"));
      Set_Etype (E, Standard_Boolean);

      F := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("F"));
      Set_Etype (F, Standard_Integer);

      G := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("G"));
      Set_Etype (G, Standard_Integer);

      H := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("H"));
      Set_Etype (H, Standard_Integer);

      I := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("I"));
      Set_Etype (I, Standard_Integer);

      J := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("J"));
      Set_Etype (J, Standard_Integer);

      K := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("K"));
      Set_Etype (K, Standard_Integer);

      Id_Lab := Make_Identifier
        (Sloc  => 1,
         Chars => String_Find ("Id_Lab"));
      Set_Etype (Id_Lab, Standard_String);

      Op_Plus := Make_Op_Add
        (Sloc       => 1,
         Left_Opnd  => G,
         Right_Opnd => H);

      Op_Mul := Make_Op_Multiply
        (Sloc       => 1,
         Left_Opnd  => J,
         Right_Opnd => K);

      Assigna1 := Make_Assignment_Statement
        (Sloc       => 1,
         Name       => F,
         Expression => Op_Plus);

      Assigna2 := Make_Assignment_Statement
        (Sloc       => 1,
         Name       => I,
         Expression => Op_Mul);

      Assigna3 := Make_Assignment_Statement
        (Sloc       => 1,
         Name       => D,
         Expression => E);

      Lab1 := Make_Label
        (Sloc       => 1,
         Identifier => Id_Lab);

      Jump1 := Make_Goto_Statement
        (Sloc => 1,
         Name => Id_Lab);

      Jump2 := Make_Goto_Statement
        (Sloc => 1,
         Name => Id_Lab);

      Jump3 := Make_Goto_Statement
        (Sloc => 1,
         Name => Id_Lab);

      IfStmts1 := New_List;
      IfStmts2 := New_List;
      IfStmts3 := New_List;
      Elif := New_List;


      Append (Assigna2, IfStmts2);
      Append (Jump2, IfStmts2);

      If2 := Make_Elsif_Part
        (Sloc            => 1,
         Condition       => B,
         Then_Statements => IfStmts2);


      Append (Assigna3, IfStmts3);
      Append (Jump3, IfStmts3);

      If3 := Make_Elsif_Part
        (Sloc            => 1,
         Condition       => C,
         Then_Statements => IfStmts3);

      Append (If2, Elif);
      Append (If3, Elif);
      Append (Assigna1, IfStmts1);
      Append (Jump1, IfStmts1);

      If1 := Make_If_Statement
        (Sloc            => 1,
         Condition       => A,
         Then_Statements => IfStmts1,
         Elsif_Parts     => Elif);
   end Init;

   procedure Run is
   begin
      Put_Line("");
      Put_Line("=============================================================");
      Put_Line("================== Test Absolute Placement ==================");
      Put_Line("=============================================================");
      Put_Line("");
      Boxes_Node_Dispatch
        (This        => New_Builder,
         Node        => If_Stmt2);
      Absolute_Place_Box (Get_Box (If_Stmt2));
      Dump_Box (Get_Box (If_Stmt2));
      Put_Line("===================== Test Dump Network =====================");

      Dump_Network(Get_Box (If_Stmt2));

      --        Put_Line("");
      --        Put_Line("Id0 xabs"& Get_Xabs (Get_Box (Id0))'Img);
      --        Put_Line("Id0 yabs"& Get_Yabs (Get_Box (Id0))'Img);
      --        Put_Line("");
      --        Put_Line("Goto3 xabs"& Get_Xabs (Get_Box (Goto3))'Img);
      --        Put_Line("Goto3 yabs"& Get_Yabs (Get_Box (Goto3))'Img);
      --        Put_Line("");
      --        Put_Line("assign2 xabs"& Get_Xabs (Get_Box (Assign2))'Img);
      --        Put_Line("assign2 yabs"& Get_Yabs (Get_Box (Assign2))'Img);
      --        Put_Line("");
      --  --        Put_Line("id1 xabs"& Get_Xabs (Get_Box (Id1))'Img);
      --  --        Put_Line("id1 yabs"& Get_Yabs (Get_Box (Id1))'Img);
      --  --        Put_Line("");
      --  --        Put_Line("id2 xabs"& Get_Xabs (Get_Box (Id2))'Img);
      --  --        Put_Line("id2 yabs"& Get_Yabs (Get_Box (Id2))'Img);
      --  --        Put_Line("");
      --  --        Put_Line("id3 xabs"& Get_Xabs (Get_Box (Id3))'Img);
      --  --        Put_Line("id3 yabs"& Get_Yabs (Get_Box (Id3))'Img);
      --  --        Put_Line("");
      --  --        Put_Line("id5 xabs"& Get_Xabs (Get_Box (Id5))'Img);
      --  --        Put_Line("id5 yabs"& Get_Yabs (Get_Box (Id5))'Img);
      --  --        Put_Line("");
      --  --        Put_Line("id4 xabs"& Get_Xabs (Get_Box (Id4))'Img);
      --  --        Put_Line("id4 yabs"& Get_Yabs (Get_Box (Id4))'Img);
      --  --        Put_Line("");
      --        Put_Line("Assign5 xabs"& Get_Xabs (Get_Box (Assign5))'Img);
      --        Put_Line("Assign5 yabs"& Get_Yabs (Get_Box (Assign5))'Img);
      --        Put_Line("");
      --  --        Put_Line("id7 xabs"& Get_Xabs (Get_Box (Id7))'Img);
      --  --        Put_Line("id7 yabs"& Get_Yabs (Get_Box (Id7))'Img);
      --  --        Put_Line("");
      --  --        Put_Line("Op_Eq1 xabs"& Get_Xabs (Get_Box (Op_Eq1))'Img);
      --  --        Put_Line("Op_Eq1 yabs"& Get_Yabs (Get_Box (Op_Eq1))'Img);
      --  --        Put_Line("");
      --        Put_Line("if in if xabs"& Get_Xabs (Get_Box (If_Stmt1))'Img);
      --        Put_Line("if in if yabs"& Get_Yabs (Get_Box (If_Stmt1))'Img);
      --        Put_Line("");
      --  --        Put_Line("id11 xabs"& Get_Xabs (Get_Box (Id11))'Img);
      --  --        Put_Line("id11 yabs"& Get_Yabs (Get_Box (Id11))'Img);
      --  --        Put_Line("");
      --        Put_Line("goto1 xabs"& Get_Xabs (Get_Box (Goto1))'Img);
      --        Put_Line("goto1 yabs"& Get_Yabs (Get_Box (Goto1))'Img);
      --        Put_Line("");
      --        Put_Line("Assign0 xabs"& Get_Xabs (Get_Box (Assign0))'Img);
      --        Put_Line("Assign0 yabs"& Get_Yabs (Get_Box (Assign0))'Img);
      --        Put_Line("");
      --  --        Put_Line("Id14 xabs"& Get_Xabs (Get_Box (Id14))'Img);
      --  --        Put_Line("Id14 yabs"& Get_Yabs (Get_Box (Id14))'Img);
      --  --        Put_Line("");
      --  --        Put_Line("Id12 xabs"& Get_Xabs (Get_Box (Id12))'Img);
      --  --        Put_Line("Id12 yabs"& Get_Yabs (Get_Box (Id12))'Img);
      --  --        Put_Line("");
      --  --        Put_Line("Id13 xabs"& Get_Xabs (Get_Box (Id13))'Img);
      --  --        Put_Line("Id13 yabs"& Get_Yabs (Get_Box (Id13))'Img);
      --  --        Put_Line("");
      --        Put_Line("Goto6 xabs"& Get_Xabs (Get_Box (Goto6))'Img);
      --        Put_Line("Goto6 yabs"& Get_Yabs (Get_Box (Goto6))'Img);
      --        Put_Line("");
      --        Put_Line("Goto2 xabs"& Get_Xabs (Get_Box (Goto2))'Img);
      --        Put_Line("Goto2 yabs"& Get_Yabs (Get_Box (Goto2))'Img);
      --        Put_Line("");

      --        Put_Line("");
      --        Put_Line("=============================================================");
      --        Put_Line("================== Test Absolute Placement ==================");
      --        Put_Line("=============================================================");
      --        Put_Line("");
      --        Boxes_Node_Dispatch
      --          (This        => New_Builder,
      --           Node        => If1);
      --        Absolute_Place_Box (Get_Box (If1));
      --        Put_Line("===================== Test Dump Network =====================");
      --
      --        Dump_Network(Get_Box (If_Stmt2));
      --        Put_Line("");
      --        Put_Line("A xabs"& Get_Xabs (Get_Box (A))'Img);
      --        Put_Line("A yabs"& Get_Yabs (Get_Box (A))'Img);
      --        Put_Line("");
      --        Put_Line("Assigna1 xabs"& Get_Xabs (Get_Box (Assigna1))'Img);
      --        Put_Line("Assigna1 yabs"& Get_Yabs (Get_Box (Assigna1))'Img);
      --        Put_Line("");
      --        Put_Line("Jump1 xabs"& Get_Xabs (Get_Box (Jump1))'Img);
      --        Put_Line("Jump1 yabs"& Get_Yabs (Get_Box (Jump1))'Img);
      --        Put_Line("");
      --        Put_Line("If2 xabs"& Get_Xabs (Get_Box (If2))'Img);
      --        Put_Line("If2 yabs"& Get_Yabs (Get_Box (If2))'Img);
      --        Put_Line("");
      --        Put_Line("B xabs"& Get_Xabs (Get_Box (B))'Img);
      --        Put_Line("B yabs"& Get_Yabs (Get_Box (B))'Img);
      --        Put_Line("");
      --        Put_Line("Assigna2 xabs"& Get_Xabs (Get_Box (Assigna2))'Img);
      --        Put_Line("Assigna2 yabs"& Get_Yabs (Get_Box (Assigna2))'Img);
      --        Put_Line("");
      --        Put_Line("Jump2 xabs"& Get_Xabs (Get_Box (Jump2))'Img);
      --        Put_Line("Jump2 yabs"& Get_Yabs (Get_Box (Jump2))'Img);
      --        Put_Line("");
      --        Put_Line("If3 xabs"& Get_Xabs (Get_Box (If3))'Img);
      --        Put_Line("If3 yabs"& Get_Yabs (Get_Box (If3))'Img);
      --        Put_Line("");
      --        Put_Line("C xabs"& Get_Xabs (Get_Box (C))'Img);
      --        Put_Line("C yabs"& Get_Yabs (Get_Box (C))'Img);
      --        Put_Line("");
      --        Put_Line("Assigna3 xabs"& Get_Xabs (Get_Box (Assigna3))'Img);
      --        Put_Line("Assigna3 yabs"& Get_Yabs (Get_Box (Assigna3))'Img);
      --        Put_Line("");
      --  --        Put_Line("D xabs"& Get_Xabs (Get_Box (D))'Img);
      --  --        Put_Line("D yabs"& Get_Yabs (Get_Box (D))'Img);
      --  --        Put_Line("");
      --  --        Put_Line("E xabs"& Get_Xabs (Get_Box (E))'Img);
      --  --        Put_Line("E yabs"& Get_Yabs (Get_Box (E))'Img);
      --  --        Put_Line("");
      --        Put_Line("Jump3 xabs"& Get_Xabs (Get_Box (Jump3))'Img);
      --        Put_Line("Jump3 yabs"& Get_Yabs (Get_Box (Jump3))'Img);
      --        Put_Line("");

      Put_Line("========================== END TEST =========================");
   end Run;

end Driver1;


--   begin
--        declare
--           Stmts_If   : List_Id := Then_Statements (N);
--           Stmts_Else : List_Id := Else_Statements (N);
--           Stmt_If    : Node_Id := First (Stmts_If);
--           Stmt_Else  : Node_Id := First (Stmts_Else);
--           Name_If    : Node_Id;
--           Name_Else  : Node_Id;
--           New_Assign : Node_Id;
--        begin
--           if Present (Stmts_If)
--             and not Present (Elsif_Parts (N)) then
--              if not Present (Next (Stmt_If))
--                and not Present (Next (Stmt_Else)) then
--                 if Nkind (Stmt_If) = N_Assignment_Statement
--                   and Nkind (Stmt_Else) = N_Assignment_Statement then
--
--                    Name_If   := Name (Stmt_If);
--                    Name_Else := Name (Stmt_Else);
--
--                    if Name_If = Name_Else -- pas possible
--                      and Expression (Stmt_If) = Standard_True
--                      and Expression (Stmt_Else) = Standard_False then
--
--                       New_Assign := Make_Assignment_Statement
--                         (Sloc       => Sloc (N),
--                          Name       => Name_If,
--                          Expression => Condition (N));
--
--                       Insert_Before (N, New_Assign);
--                       Remove (N);
--
--                    elsif Expression (Stmt_If) = Standard_False
--                      and Expression (Stmt_Else) = Standard_True then
--
--                       New_Assign := Make_Assignment_Statement
--                         (Sloc       => Sloc (N),
--                          Name       => Name_If,
--                          Expression => Condition (N));
--                       Set_Negated_Assignment (New_Assign, True);
--
--                       Insert_Before (N, New_Assign);
--                       Remove (N);
--                    end if;
--                 end if;
--              end if;
--           end if;
--        end;
--        --------------------------------------------------------------------------
