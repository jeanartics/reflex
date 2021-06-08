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
with GNAT.OS_Lib;  use GNAT.OS_Lib;
with Ada.Text_IO;
with Gnat.Strings;
with GNAT.Dynamic_Tables; 

package Artics.Scan_Buffers is
   procedure Put_Line (S : String) is null; -- renames Ada.Text_IO.Put_Line;
   
   type Scan_Buffer_Record is private;
   type Scan_Buffer is access all Scan_Buffer_Record;
   
   -----------------
   -- Subprograms --
   -----------------
   
   function New_Scan_Buffer return Scan_Buffer;
   function New_String_Scan_Buffer (S : String) return Scan_Buffer;
   function New_File_Scan_Buffer (File_Name : String) return Scan_Buffer;
   
   procedure Reset_Scan_Buffer (Sb : Scan_Buffer);
   
   procedure Delete_Scan_Buffer (Sb : in out Scan_Buffer);
   
   function Buffer_To_String (Sb : Scan_Buffer) return String;
   -- Return the content of the scan buffer as a string
   
   procedure String_To_Buffer
     (Sb : Scan_Buffer;
      S  : String);
   -- Add string S at the end of scan buffer

   procedure Read_From_Text_File 
     (Sb        : Scan_Buffer;
      File_Name : String);
   -- Initialize the buffer content with a text file

   procedure Write_To_Text_File 
     (Sb        : Scan_Buffer;
      File_Name : String);
   -- Writhe the text buffer in a file
   
   function Scan_Ptr (Sb : Scan_Buffer) return Natural;
   -- The current pointer in source
   
   function Line (Sb : Scan_Buffer) return Natural;
   -- Rteurn the current Line being scanned

   function Column (Sb : Scan_Buffer) return Natural;
   -- return the current column being scanned

   function Current_Char (Sb : Scan_Buffer) return Character;
   -- Return the current character scanned

   function Next_Char (Sb : Scan_Buffer) return Character;
   -- Return the current character scanned

   function Previous_Char (Sb : Scan_Buffer) return Character;
   -- Return the current character scanned

   function Look_Next_Char (Sb : Scan_Buffer) return Character;
   -- Return the current character scanned

   function Look_Previous_Char (Sb : Scan_Buffer) return Character;
   -- Return the previous character scanned

   function Char_At_Offset
     (Sb     : Scan_Buffer;
      Offset : Natural) return Character;
   -- Return the character at offset from the beginning of the buffer

   procedure Next (Sb : Scan_Buffer);
   -- Forward scan pointer to the next character
   
   procedure Previous (Sb : Scan_Buffer);
   -- rewind the scan pointer from one character
   
   procedure Move
     (Sb     : Scan_Buffer;
      Offset : Natural);
   -- Move the scan pointer from offset character. The offset can be negatif,
   -- in this case the scan pointer is rewind 

   function Is_Last (Sb : Scan_Buffer) return Boolean;
   -- return True if the scan  pointer is at the end of text buffer
   
   function Size (Sb : Scan_Buffer) return Integer;
   -- Return the size of the text buffer
   
   procedure Push_State (Sb : Scan_Buffer);
   
   procedure Pop_State (Sb : Scan_Buffer);
   
   procedure Restore_State (Sb : Scan_Buffer);
   
private
   
   type Scan_State_Record is record
      Size            : Natural;
      Scan_Ptr        : Natural;
      Line            : Natural;
      Column          : Natural;
      Column_Max      : Natural;
   end record;
   
   No_Scan_State_Record : constant Scan_State_Record :=
     (Size        => 0,
      Scan_Ptr    => 0,
      Line        => 0,
      Column      => 0,
      Column_Max  => 4098);
   
   package Save_States_Stack is new GNAT.Dynamic_Tables
     (Table_Component_Type => Scan_State_Record,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 100,
      Table_Increment      => 10);
   use Save_States_Stack;
   
   type Scan_Buffer_Record is record
      Loc    : Natural;
      Buffer : Unbounded_String;
      State  : Scan_State_Record;
      T      : Save_States_Stack.Instance;
   end record;

end Artics.Scan_Buffers;
