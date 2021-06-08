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
-- with System.Address; use System.Address;
with Gnat.Os_Lib; use Gnat.Os_Lib;

package body Artics.Scan_Buffers is

   ---------------------
   -- New_Scan_Buffer --
   ---------------------

   function New_Scan_Buffer return Scan_Buffer is
      Sb : Scan_Buffer := new Scan_Buffer_Record;
   begin
      Sb.Loc    := 0;
      Sb.Buffer := Null_Unbounded_String;
      Sb.State  := No_Scan_State_Record;
      Init (Sb.T);
      
      return Sb;
   end New_Scan_Buffer;
   
   ----------------------------
   -- New_String_Scan_Buffer --
   ----------------------------
   
   function New_String_Scan_Buffer (S : String) return Scan_Buffer is
      Sb : Scan_Buffer := New_Scan_Buffer;
   begin
      String_To_Buffer (Sb , S);
      return Sb;
   end New_String_Scan_Buffer;
   
   --------------------------
   -- New_File_Scan_Buffer --
   --------------------------
   
   function New_File_Scan_Buffer (File_Name : String) return Scan_Buffer is
      Sb : Scan_Buffer := New_Scan_Buffer;
   begin
      Read_From_Text_File (Sb, File_Name);
      return Sb;
   end New_File_Scan_Buffer;
   
   -----------------------
   -- Reset_Scan_Buffer --
   -----------------------

   procedure Reset_Scan_Buffer (Sb : Scan_Buffer) is
   begin
      Sb.Buffer := Null_Unbounded_String;
      Sb.State  := No_Scan_State_Record;
      Init (Sb.T);
   end Reset_Scan_Buffer;
   
   procedure Free is new Ada.Unchecked_Deallocation
     (Scan_Buffer_Record, Scan_Buffer);
   
   ------------------------
   -- Delete_Scan_Buffer --
   ------------------------

   procedure Delete_Scan_Buffer (Sb : in out Scan_Buffer) is
   begin
      Sb.Buffer := Null_Unbounded_String;
      Free (Sb.T);
      
      Free (Sb);
   end Delete_Scan_Buffer;
   
   ----------------------
   -- Buffer_To_String --
   ----------------------
   
   function Buffer_To_String (Sb : Scan_Buffer) return String is
   begin
      return To_String (Sb.Buffer);
   end Buffer_To_String;
   
   ----------------------
   -- String_To_Buffer --
   ----------------------
   
   procedure String_To_Buffer
     (Sb : Scan_Buffer;
      S  : String) is
      
      Size : Natural := S'Length;
   begin
      Append (Sb.Buffer, S);
      Sb.State.Size := Sb.State.Size + Size;
   end String_To_Buffer;
   
   -------------------------
   -- Read_From_Text_File --
   -------------------------
   
   procedure Read_From_Text_File 
     (Sb        : Scan_Buffer;
      File_Name : String)
   is
      File_Size : Natural := Natural (Ada.Directories.Size (File_Name));
      
      subtype File_String    is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);
      
      File     : File_String_IO.File_Type;
      Contents : File_String;
   begin
      File_String_IO.Open
	(File, Mode => File_String_IO.In_File,
	 Name => File_Name);
      
      File_String_IO.Read  (File, Item => Contents);
      File_String_IO.Close (File);
      Append (Sb.Buffer, Contents);
      
      Sb.State.Size := Sb.State.Size + File_Size;
   end Read_From_Text_File;
   
   ------------------------
   -- Write_To_Text_File --
   ------------------------
   
   procedure Write_To_Text_File 
     (Sb        : Scan_Buffer;
      File_Name : String) is 

      F     : File_Descriptor;
      Count : Integer;
   begin
      F := Create_File (File_Name, Text);      
      declare
         S : String := Buffer_To_String (Sb);
      begin
         Count := Integer (S'Length);
         Count := Write (F, S'Address, Count);
      end;
   end Write_To_Text_File;
   
   --------------
   -- Scan_Ptr --
   --------------
   
   function Scan_Ptr (Sb : Scan_Buffer) return Natural is
   begin
      return Sb.State.Scan_Ptr;
   end Scan_Ptr;
   
   ----------
   -- Line --
   ----------
   
   function Line (Sb : Scan_Buffer) return Natural is
   begin
      return Sb.State.Line;
   end Line;
   
   ------------
   -- Column --
   ------------
   
   function Column (Sb : Scan_Buffer) return Natural is
   begin
      return Sb.State.Column;
   end Column;
   
   ------------------
   -- Current_Char --
   ------------------
   
   function Current_Char (Sb : in Scan_Buffer) return Character is
   begin
      return Element (Sb.Buffer, Positive (Sb.State.Scan_Ptr));
   end Current_Char;
   
   ---------------
   -- Next_Char --
   ---------------
   
   function Next_Char (Sb : in Scan_Buffer) return Character is
   begin
      Next (Sb);
      return Current_Char (Sb);
   end Next_Char;
   
   ---------------
   -- Previous_Char --
   ---------------
   
   function Previous_Char (Sb : in Scan_Buffer) return Character is
   begin
      Previous (Sb);
      return Current_Char (Sb);
   end Previous_Char;
   
   --------------------
   -- Look_Next_Char --
   --------------------
   
   function Look_Next_Char (Sb : in Scan_Buffer) return Character is
   begin
      if Sb.State.Scan_Ptr < Sb.State.Size then
	 return Element (Sb.Buffer, Positive (Sb.State.Scan_Ptr + 1));
      else
	 return Ascii.nul; -- EOF;
      end if;
   end Look_Next_Char;
   
   ------------------------
   -- Look_Previous_Char --
   ------------------------
   
   function Look_Previous_Char (Sb : in Scan_Buffer) return Character is
   begin
      if Sb.State.Scan_Ptr > 0 then
	 return Element (Sb.Buffer, Positive (Sb.State.Scan_Ptr - 1));
      else
	 raise Program_Error;
      end if;
   end Look_Previous_Char;
   
   -------------------
   -- Char_At_Offset --
   -------------------
   
   function Char_At_Offset
     (Sb     : Scan_Buffer;
      Offset : Natural) return Character is
   begin
      if Offset >= 0 then
	 if (Sb.State.Scan_Ptr + Offset) > Sb.State.Size then
	    raise Program_Error;
	 else
	    return Element (Sb.Buffer, Positive (Sb.State.Scan_Ptr + Offset));
	 end if;
      else
	 if Sb.State.Scan_Ptr <= Offset then
	    raise Program_Error;
	 else
	    return Element (Sb.Buffer, Positive (Sb.State.Scan_Ptr - Offset));
	 end if;
      end if;
   end Char_At_Offset;
   
   ----------
   -- Next --
   ----------
   
   procedure Next (Sb : Scan_Buffer) is
   begin
      if Sb.State.Scan_Ptr < Sb.State.Size then
	 Sb.State.Scan_Ptr := Sb.State.Scan_Ptr + 1;
      else
	 null;
      end if;
   end Next;
   
   --------------
   -- Previous --
   --------------
   
   procedure Previous (Sb : Scan_Buffer) is
   begin
      if Sb.State.Scan_Ptr > 0 then
	 Sb.State.Scan_Ptr := Sb.State.Scan_Ptr - 1;
     else
	 raise Program_Error;
      end if;
   end Previous;
   
   ----------
   -- Move --
   ----------
   
   procedure Move
     (Sb     : Scan_Buffer;
      Offset : Natural) is
   begin
      if (Sb.State.Scan_Ptr + Offset) > Sb.State.Size then
	 Sb.State.Scan_Ptr := Sb.State.Size;
      else
	 Sb.State.Scan_Ptr := Sb.State.Scan_Ptr + Offset;
      end if;
   end Move;
   
   -------------
   -- Is_Last --
   -------------
   
   function Is_Last (Sb : Scan_Buffer) return Boolean is
   begin
      return Sb.State.Scan_Ptr = Sb.State.Size;
   end Is_Last;
   
   ----------
   -- Size --
   ----------
   
   function Size (Sb : Scan_Buffer) return Integer is
   begin
      return Sb.State.Size;
   end Size;
   
   ----------------
   -- Push_State --
   ----------------
   
   procedure Push_State (Sb : Scan_Buffer) is
   begin
      Append (Sb.T, Sb.State);
   end Push_State;
   
   ---------------
   -- Pop_State --
   ---------------
   
   procedure Pop_State (Sb : Scan_Buffer) is
   begin
      Decrement_Last (Sb.T);
   end Pop_State;
   
   -------------------
   -- Restore_State --
   -------------------
   
   procedure Restore_State (Sb : Scan_Buffer) is
   begin
      Sb.State := Sb.T.Table (Last (Sb.T));
      Decrement_Last (Sb.T);
   end Restore_State;
   
end Artics.Scan_Buffers;
