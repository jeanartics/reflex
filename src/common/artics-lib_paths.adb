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

with GNAT.IO_Aux;                use Gnat.IO_Aux;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Maps;           use Ada.Strings.Maps;
with Ada.Strings;                use AdA.Strings;
--with Scomp.Utils; use Scomp.Utils;
with Artics.Debug;

with Artics.Logutils; use Artics.Logutils;

package body Artics.Lib_Paths is


   ----------------
   -- Initialize --
   ----------------
   
   procedure Initialize is
   begin
      Library_Path := New_List ("Lib_Path");
      Add_New_Lib_Path (Get_Current_Dir);
   end Initialize;

   ---------------------
   -- Remove_Lib_Path --
   ---------------------

   procedure Remove_Lib_Path (S : in String) is
      
      SP : String_Ptr    := new String'(S);
      It : List_Iterator := New_Iterator(Library_Path);
      PE : Path_Entry;
   begin
      -- Log ("Lib_Paths.Remove_Lib_Path: " & S);

      Reset (It);
      while not Is_End (It) loop
         PE := Current_Item (It);
         if SP.all = PE.Str.all then
            if PE.Ref > 1 then
               PE.Ref := PE.Ref - 1;
            else
               Remove_Current_Item (It);
            end if;
            return;
         end if;
         Next(It);
      end loop;
   end Remove_Lib_Path;

   ----------------------
   -- Add_New_Lib_Path --
   ----------------------

   procedure Add_New_Lib_Path
     (S     : in String;
      First : in Boolean := False) is
      
      SP : String_Ptr := new String'(S);
      PE : Path_Entry;
      It : List_Iterator := New_Iterator (Library_Path);
   begin
      Log_Line ("Lib_Paths.Add_New_Lib_Path: " & S);
      
      Reset (It);
      while not Is_End (It) loop
	 PE := Current_Item (It);
	 if SP.all = PE.Str.all then
	    PE.Ref := PE.Ref + 1;
	    return;
	 end if;
	 Next (It);
      end loop;
      
      if First then
	 Insert_Before
	   (Library_Path,
	    First_Item (Library_Path),
	    new Path_Entry_Record'(SP, 1));
      else
	 Append (new Path_Entry_Record'(SP, 1), Library_Path);
      end if;
   end Add_New_Lib_Path;

   -----------------------
   -- Dump_Library_Path --
   -----------------------

   procedure Dump_Library_Path is
      
      It : List_Iterator := New_Iterator (Library_Path);
   begin
      Reset (It);
      Log_Line ("LIB_Paths.Dump_Library_Path: ");
      while not Is_End (It) loop
         Log_Line (Current_Item(It).Str.all);
         Next (It);
      end loop;
   end Dump_Library_Path;

   -------------------------
   -- Split_Path_Filename --
   -------------------------

   procedure Split_Path_Filename
     (PF : in String;
      P  : out String_Ptr;
      F  : out String_Ptr) is
      
   begin
         P := new String'(Dir_Name (Pf));
         F := new String'(File_Name (Pf));
         -- Pos := Index (PF, To_Set (Directory_Separator), Inside, Backward);
         -- P := new String'(PF (PF'First..Pos - 1));
         -- F := new String'(PF (Pos + 1 .. PF'Last));
         
   end Split_Path_Filename;

   ----------------------
   -- Resolve_Filename --
   ----------------------

   function Resolve_Filename (S : in String) return String is
   begin
      if S (S'First) = Directory_Separator or else
        (S'Length > 2 and then S (S'First + 1) = ':') then
	 
         if File_Exists (S) then
            return S;
         else
            raise File_Not_Found;
         end if;
	 
      else
         declare
            It : List_Iterator := New_Iterator(Library_Path);
            CS : String_Ptr;
         begin
            Reset (It);
            while not Is_End (It) loop
               CS := Current_Item(It).Str;
               if File_Exists (CS.all & Directory_Separator & S) then
                  return CS.all & Directory_Separator & S;
               end if;
	       
               Next (It);
            end loop;
         end;
         raise File_Not_Found;
      end if;
   exception
      when File_Not_Found =>
         Log_Line("***********************************************");
         Log_line("Lib_Paths.Resolve_Filename: File " &
		    S & " was not found.");
         Log_Line("Lib path dump= ");
         Dump_Library_Path;
         Log_Line("***********************************************");
         raise;
	 
   end Resolve_Filename;

end Artics.Lib_Paths;

