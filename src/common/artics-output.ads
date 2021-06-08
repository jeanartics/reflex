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

--  This package contains low level output routines used by the compiler for
--  writing error messages and informational output. It is also used by the
--  debug source file output routines (see Sprint.Print_Debug_Line).

-- with Types;    use Types;
with GNAT.Dynamic_Tables;

with Gnat.OS_Lib; use Gnat.OS_Lib;

package Artics.Output is
   pragma Elaborate_Body;
   
   type Output_Buffer_Record is private;
   type Output_Buffer is access all Output_Buffer_Record;
   
   type Output_Proc is access procedure (S : String);
   --  This type is used for the Set_Special_Output procedure. If Output_Proc
   --  is called, then instead of lines being written to standard error or
   --  standard output, a call is made to the given procedure for each line,
   --  passing the line with an end of line character (which is a single
   --  ASCII.LF character, even in systems which normally use CR/LF or some
   --  other sequence for line end).
   
   subtype Pos is Positive;
   subtype Nat is Natural;
   subtype Int is Integer;
   
   -----------------
   -- Subprograms --
   -----------------
   
   function New_Output_Buffer return Output_Buffer;
   
   procedure Append_Buffer_To_File
     (Ob    : in Output_Buffer;
      Fname : in String);
   -- Create a new file with name Fname and append the buffer content to the
   -- newly created file.
   
   procedure Append_Buffer_To_File
     (Ob : Output_Buffer;
      F  : File_Descriptor);
   --  Flush the buffer contain into file FD.
   
   procedure Flush_Buffer (Ob : in Output_Buffer);
   --  Output the buffer contains to the output Fd associated with 
   --  Output_Buffer.
   
   procedure Set_Indent
     (Ob : Output_Buffer;
      N  : Natural);
   -- Set current indentation to N

   procedure Inc_Indent (Ob : Output_Buffer);
   procedure Indent_Begin (Ob : Output_Buffer) renames Inc_Indent;
   --  Increases the current indentation level. Whenever a line is written
   --  (triggered by Eol), an appropriate amount of whitespace is added to the
   --  beginning of the line, wrapping around if it gets too long.

   procedure Dec_Indent (Ob : Output_Buffer);
   procedure Indent_End (Ob : Output_Buffer) renames Dec_Indent;
   --  Decreases the current indentation level

   procedure Set_Output
     (Ob : Output_Buffer;
      FD : File_Descriptor);
   -- Direct Ouput buffer to file FD.
   
   procedure Write_Char
     (Ob : Output_Buffer;
       C  : Character);
   --  Write one character to the standard output file. If the character is LF,
   --  this is equivalent to Write_Eol.

   procedure Erase_Char (Ob : Output_Buffer);
   --  If last character in buffer matches C, erase it, otherwise no effect
   
   procedure Write_Erase_Char
     (Ob : Output_Buffer;
      C  : Character);
   --  If last character in buffer matches C, erase it, otherwise no effect
   
   procedure Write_Eol (Ob : Output_Buffer);
   --  Write an end of line (whatever is required by the system in use, e.g.
   --  CR/LF for DOS, or LF for Unix) to the standard output file. This routine
   --  also empties the line buffer, actually writing it to the file. Note that
   --  Write_Eol is the only routine that causes any actual output to be
   --  written. Trailing spaces are removed.

   procedure Write_Eol_Keep_Blanks (Ob : Output_Buffer);
   --  Similar as Write_Eol, except that trailing spaces are not removed
   
   procedure Write_Indent (Ob : Output_Buffer);
   --  write current Indentation.
   
   procedure Write_Indent_Str
     (Ob : Output_Buffer;
      S  : String);
   --  write current Indentation.
   
   procedure Write_Int 
     (Ob  : Output_Buffer;
      Val : Int);
   --  Write an integer value with no leading blanks or zeroes. Negative values
   --  are preceded by a minus sign).

   procedure Write_Spaces
     (Ob : Output_Buffer;
      N  : Nat);
   --  Write N spaces

   procedure Write_Str
     (Ob : Output_Buffer;
      S  : String);
   --  Write a string of characters to the standard output file. Note that
   --  end of line is normally handled separately using WRITE_EOL, but it is
   --  allowable for the string to contain LF (but not CR) characters, which
   --  are properly interpreted as end of line characters. The string may also
   --  contain horizontal tab characters.
   
   procedure Write_Str_With_Col_Check
     (Ob : Output_Buffer;
      S  : String);
   --  Write string (using Write_Str) with initial column check, and possible
   --  initial Write_Indent (to get new line) if current line is too full.
   
   procedure Write_Line
     (Ob : Output_Buffer;
      S  : String);
   --  Equivalent to Write_Str (S) followed by Write_Eol;
   
   procedure Write_Indent_Line
     (Ob : Output_Buffer;
      S  : String);
   -- Write indentation and then sting S followed by an End Of Line
      
   
   function Column (Ob : Output_Buffer) return Pos;
   pragma Inline (Column);
   --  Returns the number of the column about to be written (e.g. a value of 1
   --  means the current line is empty).
   
   function Max_Line_Length (Ob : Output_Buffer) return Pos;
   pragma Inline (Max_Line_Length);
   --  Returns the maximun of line length (default set to 
   --  Hostparm.Max_Line_Length).
   
private
   
   Max_Line_Length_Default : Pos := 1024;

   Chars_Tables_Initial  : constant := 10_000;
   Chars_Tables_Increment : constant := 1_000;
   
   package Chars_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => Character,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => Natural'First,
      Table_Initial        => Chars_Tables_Initial,
      Table_Increment      => Chars_Tables_Increment);
   use Chars_Tables;
   
   type Output_Buffer_Record is record
      Buffer          : Chars_Tables.Instance;
      Next_Col        : Positive := 1;
      Cur_Indentation : Natural;
      Column_Max      : Positive := Max_Line_Length_Default;
      FD              : File_Descriptor;
   end record;

end Artics.Output;
