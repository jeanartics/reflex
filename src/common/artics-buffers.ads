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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Text_IO; use Ada.Text_Io;
with Gnat.Strings;

package Artics.Buffers is
   
   type Output_Buffer_Record is private;
   type Output_Buffer is access all Output_Buffer_Record;
   
   type Casing_Type is
     (Unchanged_Case,
      Lower_Case,
      Upper_Case,
      Mixed_Case);
   
   -----------------
   -- Subprograms --
   -----------------
   
   function New_Output_Buffer return Output_Buffer;
   
   procedure Reset_Buffer (Ob : Output_Buffer);
   --  Reset buffer to its initail values. The previous content of buffer is
   --  lost
   
   procedure Free_Buffer (Ob : in out Output_Buffer);
   --  Deallocate memory and delete buffer
   
   procedure Append_Buffer
     (Dest : Output_Buffer; 
      Src  : Output_Buffer);
   -- Copy buffer Src to buffer Dest. Src is let unchanged
   
   procedure Indent (Ob : Output_Buffer);
   procedure Indent_Begin (Ob : Output_Buffer) renames Indent;
   --  Increases the current indentation level. Whenever a line is written
   --  (triggered by Eol), an appropriate amount of whitespace is added to the
   --  beginning of the line, wrapping around if it gets too long.
   
   procedure Dec_Indent (Ob : Output_Buffer);
   procedure Indent_End (Ob : Output_Buffer) renames Dec_Indent;
   
   procedure Write_Char
     (Ob : Output_Buffer;
      C  : Character);
   --  Write one character to the standard output file. If the character is LF,
   --  this is equivalent to Write_Eol.

   procedure Write_Eol (Ob : Output_Buffer);
   --  Write an end of line (whatever is required by the system in use, e.g.
   --  CR/LF for DOS, or LF for Unix) to the standard output file. This routine
   --  also empties the line buffer, actually writing it to the file. Note that
   --  Write_Eol is the only routine that causes any actual output to be
   --  written. Trailing spaces are removed.

   procedure Write_Indent (Ob : Output_Buffer);
   --  write current Indentation.
   
   procedure Write_Indent_Str
     (Ob : Output_Buffer;
      S  : String);
   --  write current Indentation.
   
   procedure Write_Int 
     (Ob  : Output_Buffer;
      Val : Integer);
   --  Write an integer value with no leading blanks or zeroes. Negative values
   --  are preceded by a minus sign).

   procedure Write_Spaces
     (Ob : Output_Buffer;
      N  : Natural);
   --  Write N spaces

   procedure Write_Str
     (Ob : Output_Buffer;
      S  : String);
   --  Write a string of characters to the standard output file. Note that
   --  end of line is normally handled separately using WRITE_EOL, but it is
   --  allowable for the string to contain LF (but not CR) characters, which
   --  are properly interpreted as end of line characters. The string may also
   --  contain horizontal tab characters.
   
   procedure Write_Id
     (Ob     : Output_Buffer;
      Id     : String;
      Casing : Casing_Type := Mixed_Case);
   -- Write a name in mixed_case or upper or in lower cas regarding the 
   -- prameter casing
   
   procedure Write_Casing_Name
     (Ob     : Output_Buffer;
      Id     : String;
      Casing : Casing_Type := Mixed_Case);
   -- Write a name in mixed_case or upper or in lower cas regarding the 
   -- prameter casing

   procedure Write_Line
     (Ob : Output_Buffer;
      S  : String);
   --  Equivalent to Write_Str (S) followed by Write_Eol;

   function Column (Ob : Output_Buffer) return Positive;
   pragma Inline (Column);
   --  Returns the number of the column about to be written (e.g. a value of 1
   --  means the current line is empty).
   
   function Max_Line_Length (Ob : Output_Buffer) return Positive;
   pragma Inline (Max_Line_Length);
   --  Returns the maximun of line length (default set to 
   --  Hostparm.Max_Line_Length).
   
   function Buffer_To_String (Ob : Output_Buffer) return String;
   --  Return the content of buffer Ob as a String
   
   function Line_Count (Ob : Output_Buffer) return Natural;
   --  return the number of lines in buffer
   
   procedure Append_Buffer_To_File   
     (Ob : Output_Buffer;
      F  : GNAT.OS_Lib.File_Descriptor);
   --  Output the contains of buffer Ob at the end of the file F

   procedure Read_From_Text_File 
     (Ob   : Output_Buffer;
      Name : String);
   --  Read content of file named Name and put it at the ned of buffer Ob
   
   procedure Write_To_Text_File 
     (Ob        : Output_Buffer;
      File_Name : String);
   --  Xrite the content of the buffer Ob in the file File_Name.
   
private
   
   type Output_Buffer_Record is record
      Buffer          : Unbounded_String;
      Next_Col        : Positive;
      Cur_Indentation : Natural;
      Column_Max      : Positive;
   end record;

end Artics.Buffers;
