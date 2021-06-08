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

with Artics.Dynamic_Tables;
with Artics.Chars_Codes; use Artics.Chars_Codes;

package Artics.Input_Buffers is
   
   type Input_Buffer_Record is private;
   type Input_Buffer is access all Input_Buffer_Record;
   
   EOF : constant Character := ASCII.SUB;
   --  The character SUB (16#1A#) is used in DOS and other systems derived
   --  from DOS (XP, NT etc) to signal the end of a text file. Internally
   --  all source files are ended by an EOF character, even on Unix systems.
   --  An EOF character acts as the end of file only as the last character
   --  of a source buffer, in any other position, it is treated as a blank
   --  if it appears between tokens, and as an illegal character otherwise.
   --  This makes life easier dealing with files that originated from DOS,
   --  including concatenated files with interspersed EOF characters.
   
   subtype Graphic_Character is Character range ' ' .. '~';
   --  Graphic characters, as defined in ARM
   
   subtype Line_Terminator is Character range ASCII.LF .. ASCII.CR;
   --  Line terminator characters (LF, VT, FF, CR). For further details, see
   --  the extensive discussion of line termination in the Sinput spec.
   
   subtype Upper_Half_Character is
     Character range Character'Val (16#80#) .. Character'Val (16#FF#);
   --  8-bit Characters with the upper bit set

   
   type Word is mod 2 ** 32;
   --  Unsigned 32-bit integer
   
   subtype Source_Ptr is Integer;
   --  Type used to represent a source location, which is a subscript of a
   --  character in the source buffer. As noted above, different source buffers
   --  have different ranges, so it is possible to tell from a Source_Ptr value
   --  which source it refers to. Note that negative numbers are allowed to
   --  accommodate the following special values.
   
   type Source_Buffer is array (Source_Ptr range <>) of Character;
   --  Type used to store text of a source file. The buffer for the main
   --  source (the source specified on the command line) has a lower bound
   --  starting at zero. Subsequent subsidiary sources have lower bounds
   --  which are one greater than the previous upper bound, rounded up to
   --  a multiple of Source_Align.

   subtype Big_Source_Buffer is Source_Buffer (0 .. Source_Ptr'Last);
   --  This is a virtual type used as the designated type of the access type
   --  Source_Buffer_Ptr, see Osint.Read_Source_File for details.

   type Source_Buffer_Ptr is access all Big_Source_Buffer;
   --  Pointer to source buffer. We use virtual origin addressing for source
   --  buffers, with thin pointers. The pointer points to a virtual instance
   --  of type Big_Source_Buffer, where the actual type is in fact of type
   --  Source_Buffer. The address is adjusted so that the virtual origin
   --  addressing works correctly. See Osint.Read_Source_Buffer for further
   --  details. Again, as for Big_String_Ptr, we should never allocate using
   --  this type, but we don't give a storage size clause of zero, since we
   --  may end up doing deallocations of instances allocated manually.

   subtype Line_Number is Natural;
   --  Line number type, used for storing logical line numbers (i.e. line
   --  numbers that include effects of any Source_Reference pragmas in the
   --  source file). The value zero indicates a line containing a source
   --  reference pragma.

   No_Line_Number : constant Line_Number := 0;
   --  Special value used to indicate no line number

   subtype Column_Number is Natural;
   --  Column number (assume that 2**15 - 1 is large enough). The range for
   --  this type is used to compute Hostparm.Max_Line_Length. See also the
   --  processing for -gnatyM in Stylesw).

   No_Column_Number : constant Column_Number := 0;
   --  Special value used to indicate no column number

   No_Location : constant Source_Ptr := -1;
   --  Value used to indicate no source position set in a node. A test for a
   --  Source_Ptr value being > No_Location is the approved way to test for a
   --  standard value that does not include No_Location or any of the following
   --  special definitions. One important use of No_Location is to label
   --  generated nodes that we don't want the debugger to see in normal mode
   --  (very often we conditionalize so that we set No_Location in normal mode
   --  and the corresponding source line in -gnatD mode).

   First_Source_Ptr : constant Source_Ptr := 0;
   --  Starting source pointer index value for first source program
   
   procedure Initialize_Input_Buffers;
   
   -----------------
   -- Subprograms --
   -----------------
   
   function New_Input_Buffer return Input_Buffer;
   --  Create an empty input buffer. Source_First and SOurce_Last are set to 0
   
   procedure Reset_Buffer (Buf : in out Input_Buffer);
   --  Reset buffer to its initail values. The previous content of buffer is
   --  lost
   
   procedure Free_Buffer (Buf : in out Input_Buffer);
   --  Deallocate memory and delete buffer
   
   function Get_Source_Buffer (Buf : Input_Buffer) return Source_Buffer_Ptr;
   --  Return the source text buffer 
   
   function Source_Char
     (Buf    : Input_Buffer;
      Offset : Source_Ptr := 0) return Character;
   --  Reurn the character pointed by the current scan position + offset
   
   function Get_Scan_Ptr (Buf : Input_Buffer) return Source_Ptr;
   --  Return the current poisition pointed in buffer
   
   procedure Set_Scan_Ptr
     (Buf     : Input_Buffer;
      Src_Ptr : Source_Ptr);     
   --  Set the current pointed poistion in the buffer
   
   procedure Inc_Src_Ptr
     (Buf : Input_Buffer; 
      V   : Source_Ptr := 1);
   --  Increment the position pointer by V
   
   function Get_Checksum (Buf : Input_Buffer) return Word;
   --  Return the checksum of the buffer
   
   procedure Accumulate_Checksum
     (Buf : Input_Buffer;
      C   : Character);
   pragma Inline (Accumulate_Checksum);
   --  This routine accumulates the checksum given character C. During the
   --  scanning of a source file, this routine is called with every character
   --  in the source, excluding non signifiants character for the current 
   --  parser. For example in language the excluded character are blanks, and
   --  all control characters (except that ESC is included in the checksum). 
   --  Upper case letters not in string literals are folded by the caller. See
   --  Sinput spec for the documentation of the checksum algorithm. 

   procedure Accumulate_Checksum
     (Buf : Input_Buffer;
      C   : Char_Code);
   pragma Inline (Accumulate_Checksum);
   --  This version is identical, except that the argument, C, is a character
   --  code value instead of a character. This is used when wide characters
   --  are scanned. We use the character code rather than the ASCII characters
   --  so that the checksum is independent of wide character encoding method.

   procedure Initialize_Checksum (Buf : Input_Buffer);
   pragma Inline (Initialize_Checksum);
   --  Initialize checksum value
   
   procedure Reallocate_Buffer
     (Buf     : Input_Buffer;
      New_Len : Integer);
   --  Re allocate buffer with a size of New_Len. If current buffer content is 
   --  preserved. 
   
   procedure Read_Source_File
     (Buf       : Input_Buffer;
      File_Name : String);
   --  Initialize the buffer content with the content of file File_Name
   
   procedure Read_Source_String
     (Buf : Input_Buffer;
      S   : String);
   --  Initialize the buffer content with the content of the string S
   
   procedure Append_Source_File
     (Buf       : Input_Buffer;
      File_Name : String);
   --  Append the content of the file File_Name at the end of the buffer. An  
   --  EOF character is added at the end of the buffer
   
   procedure Append_Source_String 
     (Buf : Input_Buffer;
      S   : String);
   --  Append the content of the string S at the end of the buffer. An EOF
   --  character is added at the end of the buffer
   
   function Get_Column_Number
     (Buf : Input_Buffer;
      P   : Source_Ptr) return Column_Number;
   --  Return the column number of position P in buffer
   
   function Get_Column_Number (Buf : Input_Buffer) return Column_Number;
   --  Return the column number of the current position
   
   function Set_Start_Column
     (Buf : Input_Buffer;
      P   : in out Source_Ptr) return Column_Number;
   function Set_Start_Column (Buf : Input_Buffer) return Column_Number;
   --  This routine is called with Scan_Ptr pointing to the first character
   --  of a line. On exit, Scan_Ptr is advanced to the first non-blank
   --  character of this line (or to the terminating format effector if the
   --  line contains no non-blank characters), and the returned result is the
   --  column number of this non-blank character (zero origin), which is the
   --  value to be stored in the Start_Column scan variable.
   
   function Line_Start
     (Buf : Input_Buffer;
      P   : Source_Ptr) return Source_Ptr;
   --  Return the start position of line containing position P
   
   function Line_Start (Buf : Input_Buffer) return Source_Ptr;
   --  Return the start position of line containing the current position in 
   --  buffer

   procedure Skip_Line_Terminators
     (Buf      : Input_Buffer;
      P        : in out Source_Ptr;
      Physical : in out Boolean);
   --  Skip line terminators pointed by P
   
   procedure Skip_Line_Terminators
     (Buf      : Input_Buffer; 
      Physical : in out Boolean);
   
   function Last_Source_Line (Buf : Input_Buffer) return Source_Ptr;
   -- return The last line already in buffer
   
   procedure Add_Lines_Entry
     (Buf : Input_Buffer;
      P   : Source_Ptr);
   procedure Add_Lines_Entry (Buf : Input_Buffer);

   function Get_Line_Number
     (Buf : Input_Buffer;
      P   : Source_Ptr) return Line_Number;
   function Get_Line_Number (Buf : Input_Buffer) return Line_Number;
   
   function Source_First (Buf : Input_Buffer) return Source_Ptr;
   
   procedure Set_Source_First
     (Buf  : Input_Buffer;
      Frst : Source_Ptr);
   
   function Source_Last (Buf : Input_Buffer) return Source_Ptr;
   
   procedure Set_Source_Last
     (Buf : Input_Buffer;
      Lst : Source_Ptr);

   function Source_Lines_Count (Buf : Input_Buffer) return Natural;

   function Source_Offset
     (Buf : Input_Buffer;
      S   : Source_Ptr) return Natural;
   
private
   
   package Lines_Tables is new Artics.Dynamic_Tables
     (Table_Component_Type => Source_Ptr,
      Table_Index_Type     => Line_Number,
      Table_Low_Bound      => 0,
      Table_Initial        => 500,
      Table_Increment      => 500);
   use Lines_Tables;
   
   type Input_Buffer_Record is record
      Buffer : Source_Buffer_Ptr;
      --  Buffer contains the source text to scan
      
      Source_First : Source_Ptr;
      --  Index of the first char in buffer
      
      Source_Last : Source_Ptr;
      --  Index of the last char in buffer, points to EOF character
      
      Scan_Ptr : Source_Ptr;
      --  Current pointer in the input buffer
      
      Lines : Lines_Tables.Instance;
      --  Table of the first character of each line in buffer
      
      Checksum : Word;
      --  Used to accumulate a CRC representing the tokens in the source
      --  file being compiled. This CRC includes only program tokens, and
      --  excludes comments.
      
   end record;
   
   No_Input_Buffer_Record : constant Input_Buffer_Record := 
     (Buffer       => null,
      Source_First => 0,
      Source_Last  => 0,
      Scan_Ptr     => 0,
      Lines        => Lines_Tables.No_Instance,
      Checksum     => 0);
   
end Artics.Input_Buffers;
