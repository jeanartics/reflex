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

with Ada.Unchecked_Conversion;
with Ada.Text_Io;

package body Artics.Output is

   Current_FD : File_Descriptor := Standout;
   --  File descriptor for current output

   Special_Output_Proc : Output_Proc := null;
   --  Record argument to last call to Set_Special_Output. If this is
   --  non-null, then we are in special output mode.

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
      Init (Ob.Buffer);
      Ob.Next_Col        := 1;
      Ob.Cur_Indentation := 0;
      Ob.Column_Max      := Max_Line_Length_Default;
      Ob.FD              := Standout;

      return Ob;
   end New_Output_Buffer;
   
   ---------------------------
   -- Append_Buffer_To_File --
   ---------------------------

   procedure Append_Buffer_To_File
     (Ob    : in Output_Buffer;
      Fname : in String) is
      
      F : File_Descriptor;
   begin
      F := Create_File (Name  => Fname, Fmode => Text);
      
      Append_Buffer_To_File (Ob, F);
      
      Close (F);      
   end Append_Buffer_To_File;
   
   ---------------------------
   -- Append_Buffer_To_File --
   ---------------------------

   procedure Append_Buffer_To_File
     (Ob : in Output_Buffer;
      F  : in File_Descriptor) is

      Len : Integer;
      
      function To_Address is new Ada.Unchecked_Conversion
      	(Chars_Tables.Table_Ptr, System.Address);
   begin
      --Ada.Text_Io.Put_Line ("Append_Buffer_To_File Begin");
      
      if F /= Invalid_Fd then
        Len := Write
	  (F, To_Address (Ob.Buffer.Table), Integer (Last (Ob.Buffer)));
      else
	 null;
      end if;
      --  Len := Write
      --  	(F, To_Address (Ob.Buffer.Table),
      --  	 Integer (Chars_Tables.First - Last (Ob.Buffer)));
      --Ada.Text_Io.Put_Line ("Append_Buffer_To_File End");
   end Append_Buffer_To_File;

   ----------------
   -- Flush_File --
   ----------------

   procedure Flush_Buffer (Ob : in Output_Buffer) is
   begin
      Append_Buffer_To_File (Ob, Ob.Fd);
   end Flush_Buffer;

   ------------
   -- Column --
   ------------

   function Column (Ob : Output_Buffer) return Pos is
   begin
      return Pos (Ob.Next_Col);
   end Column;

   ------------
   -- Column --
   ------------

   function Max_Line_Length (Ob : Output_Buffer) return Pos is
   begin
      return Pos (Ob.Column_Max);
   end Max_Line_Length;

   ----------------
   -- Set_Indent --
   ----------------
   
   procedure Set_Indent
     (Ob : Output_Buffer;
      N  : Natural) is
   begin
      Ob.Cur_Indentation := N;
   end Set_Indent;

   ----------------
   -- Inc_Indent --
   ----------------

   procedure Inc_Indent (Ob : Output_Buffer) is
   begin
      --  The "mod" in the following assignment is to cause a wrap around in
      --  the case where there is too much indentation.

      Ob.Cur_Indentation :=
        (Ob.Cur_Indentation + Indentation_Amount) mod Indentation_Limit;
   end Inc_Indent;

   ----------------
   -- Dec_Indent --
   ----------------

   procedure Dec_Indent (Ob : Output_Buffer) is
   begin
      --  The "mod" here undoes the wrap around from Indent above

      Ob.Cur_Indentation :=
        (Ob.Cur_Indentation - Indentation_Amount) mod Indentation_Limit;
   end Dec_Indent;

   ----------------
   -- Set_Output --
   ----------------

   procedure Set_Output
     (Ob : Output_Buffer;
      FD : File_Descriptor) is
   begin
      Ob.FD := FD;
   end Set_Output;

   ----------------
   -- Write_Char --
   ----------------

   procedure Write_Char
     (Ob : Output_Buffer;
      C  : Character) is
   begin
      if Ob.Next_Col = Ob.Column_Max then
         Write_Eol (Ob);
      end if;

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
      --  Remove any trailing space

      while Last (Ob.Buffer) > Chars_Tables.First
	and then Ob.Buffer.Table (Last (Ob.Buffer)) = ' ' loop
         Erase_Char (Ob);
      end loop;

      Append (Ob.Buffer, ASCII.LF);
      Ob.Next_Col := 1;
   end Write_Eol;

   ---------------------------
   -- Write_Eol_Keep_Blanks --
   ---------------------------

   procedure Write_Eol_Keep_Blanks (Ob : Output_Buffer) is
   begin
      Append (Ob.Buffer, ASCII.LF);
      Ob.Next_Col := 1;
   end Write_Eol_Keep_Blanks;

   ----------------
   -- Erase_Char --
   ----------------

   procedure Erase_Char (Ob : Output_Buffer) is
   begin
      if Last (Ob.Buffer) > Chars_Tables.First then
         Set_Last (Ob.Buffer, Last (Ob.Buffer) - 1);
      end if;
   end Erase_Char;
   
   ----------------------
   -- Write_Erase_Char --
   ----------------------

   procedure Write_Erase_Char
     (Ob : Output_Buffer;
      C  : Character) is
   begin
      if Ob.Next_Col /= 1 and then Ob.Buffer.Table (Last (Ob.Buffer)) = C then
	 Ob.Buffer.Table (Last (Ob.Buffer)) := ' ';
         Set_Last (Ob.Buffer, Last (Ob.Buffer) - 1);
         Ob.Next_Col := Ob.Next_Col - 1;
      end if;      
   end Write_Erase_Char;

   ------------------
   -- Write_Indent --
   ------------------
   
   procedure Write_Indent (Ob : Output_Buffer) is
   begin
      for J in 1 .. Ob.Cur_Indentation loop
	 Write_Char (Ob, ' ');
      end loop;
   end Write_Indent;
   
   -----------------------
   -- Write_Indent_Str( --
   -----------------------
   
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
      Val : Int) is
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

   -----------------------
   -- Write_Indent_Line --
   -----------------------

   procedure Write_Indent_Line
     (Ob : Output_Buffer;
      S  : String) is
   begin
      Write_Indent_Str (Ob, S);
      Write_Eol (Ob);
   end Write_Indent_Line;

   ------------------
   -- Write_Spaces --
   ------------------

   procedure Write_Spaces
     (Ob : Output_Buffer;
      N  : Nat) is
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
   
   ------------------------------
   -- Write_Str_With_Col_Check --
   ------------------------------

   procedure Write_Str_With_Col_Check
     (Ob : Output_Buffer;
      S  : String) is
   begin
      if Positive (S'Last) + Ob.Next_Col > Ob.Column_Max then
         Write_Indent_Str (Ob, "  ");

         if S (S'First) = ' ' then
            Write_Str (Ob, S (S'First + 1 .. S'Last));
         else
            Write_Str (Ob, S);
         end if;

      else
         Write_Str (Ob, S);
      end if;
   end Write_Str_With_Col_Check;
   
end Artics.Output;
