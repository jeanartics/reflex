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

with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps;  use Ada.Strings.Maps;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with GNAT.Case_Util;  use GNAT.Case_Util;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gnat.Strings;

separate (Artics.Graphs)

package body Dump is

   ----------------------
   -- Visit_Initialize --
   ----------------------

   procedure Visit_Initialize
     (V : in out Grafvis_Visitor_Record;
      G : in Graph) is
   begin
      Visit_Initialize (Graph_Visitor_Record (V), G);

      V.Tb := New_Output_Buffer;
      Reset_Buffer (V.Tb);

      V.Ob := New_Output_Buffer;
      Reset_Buffer (V.Ob);
   end Visit_Initialize;

   --------------------
   -- Visit_Finalize --
   --------------------

   procedure Visit_Finalize (V : in out Grafvis_Visitor_Record) is
   begin
      --Delete_Output_Buffer (V.Tb);
      --Delete_Output_Buffer (V.Ob);

      Visit_Finalize (Graph_Visitor_Record (V));
   end Visit_Finalize;

   -----------------
   -- Visit_Graph --
   -----------------

   procedure Visit_Graph
     (V : in out Grafvis_Visitor_Record;
      S : in Visit_Step) is

      G : Graph;
   begin
      G := Get_Graph (V);

      declare
         Name  : String := Get_Graph_Name (G);
         Gname : Name_Id;
         Fname : Name_Id;
      begin
         Gname := String_Find (Name);
         if Gname = No_Name then
            Fname := String_Find ("graf");
         else
            Fname := Gname;
         end if;

         if S = Before then
            declare
               S : String := Get_String (Fname);
            begin
               To_Mixed (S);
               Emit_Line (V, "digraph " & S & " {");

               Emit_Line (V, "size = ""10,10"";");
            end;

         elsif S = After then
            Emit_Line (V, "}");

            declare
               Name  : String := Get_Dump_Name (G);
            begin
               if Name = "" then
                  null;
               else
                  Fname := String_Find (Name);
               end if;

               declare
                  Ob : Output_Buffer := Get_Output_Buffer (V);
               begin
                  Write_To_Text_File (Ob, Get_String (Fname) & ".dot");
               end;
            end;
         end if;
      end;
   end Visit_Graph;

   ------------------
   -- Visit_Vertex --
   ------------------

   procedure Visit_Vertex
     (V : in out Grafvis_Visitor_Record;
      S : in Visit_Step) is

   begin
      if S /= Before then
         return;
      end if;
   end Visit_Vertex;

   ---------------
   -- Visit_Arc --
   ---------------

   procedure Visit_Arc
     (V : in out Grafvis_Visitor_Record;
      S : in Visit_Step) is

      A : Arc;
   begin
      if S /= Before then
         return;
      end if;

      A := Get_Arc (V);

      Generate_Arc (V, A);
   end Visit_Arc;

   ----------------
   -- Dump_Graph --
   ----------------

   procedure Dump_Graph (G : in Graph) is

      V : Grafvis_Visitor_Record;
   begin
      -- Visit_Initialize (V, G);
      Do_Visit (V, G);
      -- Visit_Finalize (V);
   end Dump_Graph;

   -------------------
   -- Get_Dump_Name --
   -------------------

   function Get_Dump_Name (G : in Graph) return String is
   begin
      if G.Dump_Name /= null then
         return G.Dump_Name.all;
      else
         return "";
      end  if;
   end Get_Dump_Name;

   -------------------
   -- Set_Dump_Name --
   -------------------

   procedure Set_Dump_Name
     (G : in Graph;
      N : in String) is
   begin
      if G.Dump_Name /= null then
         Free (G.Dump_Name);
      end if;
      G.Dump_Name := new String'(N);
   end Set_Dump_Name;

   ------------------
   -- Change_Cr_Lf --
   ------------------

   function Change_Cr_Lf (S : in String) return String is

      S1        : Unbounded_String := Null_Unbounded_String;
      Eol_Found : Boolean := False;
   begin
      for I in S'Range loop
         if S (I) = Ascii.Cr or else S (I) = Ascii.Lf then
            Eol_Found := True;
         else
            if Eol_Found then
               Append (S1, "\n ");
            end if;
            Eol_Found := False;

            Append (S1, S (I));
         end if;
      end loop;

      if S1 = Null_Unbounded_String then
         return "";
      else
         return To_String (S1);
      end if;
   end Change_Cr_Lf;

   ------------------
   -- Change_Space --
   ------------------

   function Change_Space (S : in String) return String is

      S1 : String := S;
   begin
      for I in S1'Range loop
         if S1 (I) = ' ' then
            S1 (I) := '_';
         end if;
      end loop;

      return S1;
   end Change_Space;

   ------------------
   -- Generate_Arc --
   ------------------

   procedure Generate_Arc
     (V : in out Grafvis_Visitor_Record;
      A : in Arc) is

      From : Vertex;
      To   : Vertex;
   begin
      From := Get_Source_Vertex (A);
      To := Get_Target_Vertex (A);

      declare
         Sfrom : String := Get_Vertex_Name (From);
         Sto   : String := Get_Vertex_Name (To);
      begin
         To_Mixed (Sfrom);
         To_Mixed (Sto);
         declare
            Aname : String := Get_Arc_Name (A);
         begin
            if Aname /= "" then
               Emit_Line
                 (V,
                  Change_Space (Sfrom) & " -> " & Change_Space (Sto) &
                  "[label=""" & Change_Cr_Lf  (Aname) & """];");
            else
               Emit_Line
                 (V, Change_Space (Sfrom) & " -> " & Change_Space (Sto) & ";");
            end if;
         exception
            when others =>
               Emit_Line
                 (V, Change_Space (Sfrom) & " -> " & Change_Space (Sto) & ";");
         end;
      end;
   end Generate_Arc;

   -----------------------
   -- Get_Output_Buffer --
   -----------------------

   function Get_Output_Buffer
     (V : in Grafvis_Visitor_Record) return Output_Buffer is
   begin
      return V.Ob;
   end Get_Output_Buffer;

   ---------------------
   -- Get_Temp_Buffer --
   ---------------------

   function Get_Temp_Buffer
     (V : in Grafvis_Visitor_Record) return Output_Buffer is
   begin
      return V.Tb;
   end Get_Temp_Buffer;

   -----------------------
   -- Set_Output_Buffer --
   -----------------------

   procedure Set_Output_Buffer
     (V  : in out Grafvis_Visitor_Record;
      Ob : in Output_Buffer) is
   begin
      V.Ob := Ob;
   end Set_Output_Buffer;

   ---------------------
   -- Set_Temp_Buffer --
   ----------------------

   procedure Set_Temp_Buffer
     (V  : in out Grafvis_Visitor_Record;
      Ob : in Output_Buffer) is
   begin
      V.Tb := Ob;
   end Set_Temp_Buffer;

   -------------------------
   -- Reset_Output_Buffer --
   -------------------------

   procedure Reset_Output_Buffer (V : in out Grafvis_Visitor_Record) is
   begin
      Reset_Buffer (V.Ob);
   end Reset_Output_Buffer;

   -----------------------
   -- Reset_Temp_Buffer --
   -----------------------

   procedure Reset_Temp_Buffer (V : in out Grafvis_Visitor_Record) is
   begin
      Reset_Buffer (V.Tb);
   end Reset_Temp_Buffer;

   ----------
   -- Emit --
   ----------

   procedure Emit
     (V : in out Grafvis_Visitor_Record;
      S : in String) is

      Ob : Output_Buffer := Get_Output_Buffer (V);
   begin
      Write_Str (Ob, S);
   end Emit;

   ---------------
   -- Emit_Line --
   ---------------

   procedure Emit_Line
     (V : in out Grafvis_Visitor_Record;
      S : in String) is

      Ob : Output_Buffer := Get_Output_Buffer (V);
   begin
      Write_Line (Ob, S);
   end Emit_Line;

   ------------------
   -- Emit_Newline --
   ------------------

   procedure Emit_Newline (V : in out Grafvis_Visitor_Record) is

      Ob : Output_Buffer := Get_Output_Buffer (V);
   begin
      Write_Line (Ob, " ");
   end Emit_Newline;

   --------------------
   -- Do_Indentation --
   --------------------

   procedure Do_Indentation (V : in out Grafvis_Visitor_Record) is

      Ob : Output_Buffer := Get_Output_Buffer (V);
   begin
     null; ------- Do_Indentation (Ob);
   end Do_Indentation;

   ---------------------
   -- Inc_Indentation --
   ---------------------

   procedure Inc_Indentation (V : in out Grafvis_Visitor_Record) is

      Ob : Output_Buffer := Get_Output_Buffer (V);
   begin
      V.Indent := V.Indent + 1;
      Indent (Ob);
   end Inc_Indentation;

   ---------------------
   -- Dec_Indentation --
   ---------------------

   procedure Dec_Indentation (V : in out Grafvis_Visitor_Record) is

      Ob : Output_Buffer := Get_Output_Buffer (V);
   begin
      if V.Indent > 0 then
         V.Indent := V.Indent - 1;
      end if;

      Dec_Indent (Ob);
   end Dec_Indentation;

   ---------------------
   -- Set_Indentation --
   ---------------------

   procedure Set_Indentation (V : in out Grafvis_Visitor_Record) is

      Ob : Output_Buffer := Get_Output_Buffer (V);
   begin
      if V.Indent > 0 then
         V.Indent := V.Indent - 1;
      end if;

      Dec_Indent (Ob);
   end Set_Indentation;

end Dump;
