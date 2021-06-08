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

with System; use System;

with Ada.Directories,
     Ada.Direct_IO,
     Ada.Text_IO;
 

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation; 
with Gnat.Os_Lib; use Gnat.Os_Lib;
with GNAT.Case_Util; use GNAT.Case_Util;

package body Artics.Buffers is

   Indentation_Amount : constant Positive := 3;
   --  Number of spaces to output for each indentation level

   Indentation_Limit : constant Positive := 40;
   --  Indentation beyond this number of spaces wraps around

   -----------------------
   -- New_Output_Buffer --
   -----------------------

   function New_Output_Buffer return Output_Buffer is
      
      Ob : Output_Buffer := new Output_Buffer_Record;
   begin
      Ob.Buffer         := Null_Unbounded_String;
      Ob.Next_Col        := 1;
      Ob.Cur_Indentation := 0;
      Ob.Column_Max      := 1024;
      
      return Ob;
   end New_Output_Buffer;

   ------------------
   -- Reset_Buffer --
   ------------------

   procedure Reset_Buffer (Ob : Output_Buffer) is
   begin
      Ob.Buffer         := Null_Unbounded_String;
      Ob.Next_Col        := 1;
      Ob.Cur_Indentation := 0;
      Ob.Column_Max      := 1024;
   end Reset_Buffer;

   -----------------
   -- Free_Buffer --
   -----------------

   procedure Free_Buffer (Ob : in out Output_Buffer) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Output_Buffer_Record, Output_Buffer);
   begin
      if Ob /= null then
	 Reset_Buffer (Ob);
	 Free (Ob);
      end if;
   end Free_Buffer;
   
   -------------------
   -- Append_Buffer --
   -------------------
   
   procedure Append_Buffer
     (Dest : Output_Buffer; 
      Src  : Output_Buffer) is
   begin
      Append (Dest.Buffer, Src.Buffer);
   end Append_Buffer;
   
   ------------
   -- Column --
   ------------

   function Column (Ob : Output_Buffer) return Positive is
   begin
      return Positive (Ob.Next_Col);
   end Column;

   ------------
   -- Column --
   ------------

   function Max_Line_Length (Ob : Output_Buffer) return Positive is
   begin
      return Positive (Ob.Column_Max);
   end Max_Line_Length;

   ------------
   -- Indent --
   ------------

   procedure Indent (Ob : Output_Buffer) is
   begin
      --  The "mod" in the following assignment is to cause a wrap around in
      --  the case where there is too much indentation.

      Ob.Cur_Indentation :=
        (Ob.Cur_Indentation + Indentation_Amount) mod Indentation_Limit;
   end Indent;

   ----------------
   -- Dec_Indent --
   ----------------

   procedure Dec_Indent (Ob : Output_Buffer) is
   begin
      --  The "mod" in the following assignment is to cause a wrap around in
      --  the case where there is too much indentation.

      Ob.Cur_Indentation :=
        (Ob.Cur_Indentation - Indentation_Amount) mod Indentation_Limit;
   end Dec_Indent;

   ----------------
   -- Write_Char --
   ----------------

   procedure Write_Char
     (Ob : Output_Buffer;
      C  : Character) is
   begin
      --  if Ob.Next_Col = Ob.Column_Max then
      --     Write_Eol (Ob);
      --  end if;

      if C = ASCII.LF then
         Write_Eol (Ob);
      else
         Append (Ob.Buffer, C);
         Ob.Next_Col := Ob.Next_Col + 1;
      end if;
   end Write_Char;

   ---------------
   -- Write_Eol --
   ---------------

   procedure Write_Eol (Ob : Output_Buffer) is
   begin
      Append (Ob.Buffer, ASCII.LF);
      Ob.Next_Col := 1;
   end Write_Eol;

   ------------------
   -- Write_Indent --
   ------------------
   
   procedure Write_Indent (Ob : Output_Buffer) is
   begin
      for J in 1 .. Ob.Cur_Indentation loop
	 Write_Char (Ob, ' ');
      end loop;
   end Write_Indent;
   
   ----------------------
   -- Write_Indent_Str --
   ----------------------
   
   procedure Write_Indent_Str 
     (Ob : Output_Buffer;
      S  : String) is
   begin
      Write_Indent (Ob);
      Write_Str (Ob, S);
   end Write_Indent_Str;
   
   ---------------
   -- Write_Int --
   ---------------

   procedure Write_Int
     (Ob  : Output_Buffer;
      Val : Integer) is
   begin
      if Val < 0 then
         Write_Char (Ob, '-');
         Write_Int (Ob, -Val);

      else
         if Val > 9 then
            Write_Int (Ob, Val / 10);
         end if;

         Write_Char
	   (Ob,
	    Character'Val ((Val mod 10) + Character'Pos ('0')));
      end if;
   end Write_Int;

   ----------------
   -- Write_Line --
   ----------------

   procedure Write_Line
     (Ob : Output_Buffer;
      S  : String) is
   begin
      Write_Str (Ob, S);
      Write_Eol (Ob);
   end Write_Line;

   ------------------
   -- Write_Spaces --
   ------------------

   procedure Write_Spaces
     (Ob : Output_Buffer;
      N  : Natural) is
   begin
      for J in 1 .. N loop
         Write_Char (Ob, ' ');
      end loop;
   end Write_Spaces;

   ---------------
   -- Write_Str --
   ---------------

   procedure Write_Str
     (Ob : Output_Buffer;
      S  : String) is
   begin
      for J in S'Range loop
         Write_Char (Ob, S (J));
      end loop;
   end Write_Str;
   
   --------------
   -- Write_Id --
   --------------

   procedure Write_Id
     (Ob     : Output_Buffer;
      Id     : String;
      Casing : Casing_Type := Mixed_Case) is
      
      S : String := Id;
   begin
      case Casing is
	 when Unchanged_Case =>
	    null;
	 when Lower_Case =>
	    To_Upper (S);
	 when Upper_Case =>
	    To_Lower (S);
	 when Mixed_Case =>
	    To_Mixed (S);
      end case;
	
      for J in S'Range loop
         Write_Char (Ob, S (J));
      end loop;
   end Write_Id;
   
   -----------------------
   -- Write_Casing_Name --
   -----------------------

   procedure Write_Casing_Name
     (Ob     : Output_Buffer;
      Id     : String;
      Casing : Casing_Type := Mixed_Case) is
      
      S : String := Id;
   begin
      if S(S'First) = '_' then
	 To_Lower (S);
      else
	 case Casing is
	    when Unchanged_Case =>
	       null;
	    when Lower_Case =>
	       To_Lower (S);
	    when Upper_Case =>
	       To_Upper (S);
	    when Mixed_Case =>
	       To_Mixed (S);
	 end case;
      end if;
	
      for J in S'Range loop
         Write_Char (Ob, S (J));
      end loop;
   end Write_Casing_Name;
   
   ----------------------
   -- Buffer_To_String --
   ----------------------
   
   function Buffer_To_String (Ob : Output_Buffer) return String is
   begin
      return To_String (Ob.Buffer);
   end Buffer_To_String;
   
   ---------------------------
   -- Append_Buffer_To_File --
   ---------------------------
   
   procedure Append_Buffer_To_File   
     (Ob : Output_Buffer;
      F  : GNAT.OS_Lib.File_Descriptor) is
      
      S   : String := Buffer_To_String (Ob);
      Len : Integer;
   begin
      Len := Integer (S'Length);
      if Len > 0 then
	 Len := Write (F, S'Address, Len);
      end if;
   end Append_Buffer_To_File;
   
   -------------------------
   -- Read_From_Text_File --
   -------------------------
   
   procedure Read_From_Text_File 
     (Ob   : Output_Buffer;
      Name : String)
   is
      File_Size : Natural := Natural (Ada.Directories.Size (Name));
      
      subtype File_String    is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);
      
      File     : File_String_IO.File_Type;
      Contents : File_String;
   begin
      File_String_IO.Open  (File, Mode => File_String_IO.In_File,
			    Name => Name);
      File_String_IO.Read  (File, Item => Contents);
      File_String_IO.Close (File);
      Write_Str (Ob, Contents);
      
   end Read_From_Text_File;
   
   ------------------------
   -- Write_To_Text_File --
   ------------------------
   
   procedure Write_To_Text_File 
     (Ob        : Output_Buffer;
      File_Name : String) is 

      F     : GNAT.OS_Lib.File_Descriptor;
      Count : Integer;
   begin
      F := Create_File (File_Name, Text);      
      declare
         S : String := Buffer_To_String (Ob);
      begin
         Count := Integer (S'Length);
         Count := Write (F, S'Address, Count);
      end;
   end Write_To_Text_File;
   
   ----------------
   -- Line_Count --
   ----------------
   
   function Line_Count (Ob : Output_Buffer) return Natural is
      
      P     : Positive;
      Count : Natural;
      Len   : Natural;
      Chr   : Character;
      Chr1  : Character;
   begin
      Count := 0;
      
      Len := Length (Ob.Buffer);
      if Len /= 0 then
	 Count := 1;
	 
	 P := 1;
	 while P in 1..Len - 1 loop
	    Chr := Element (Ob.Buffer, P);
	    
	    if Chr = Ascii.CR then
	       Count := Count + 1;
	       
	       Chr1 := Element (Ob.Buffer, P+1);
	       if Chr1 = Ascii.LF then
		  P := P + 1;
	       end if;
	 
	    elsif Chr = Ascii.LF then
	       Count := Count + 1;
	 
	    elsif Chr = Ascii.FF 
	      or else Chr = Ascii.VT 
	    then
	       Count := Count + 1;

	    else
	       null;
	    end if;
	    
	    P := P + 1;
	 end loop;
      end if;
      
      return Count;
   end Line_Count;
   
end Artics.Buffers;
