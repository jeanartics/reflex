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
with System.Memory;
with Gnat.Crc32; use Gnat.Crc32;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation; 
with Gnat.Os_Lib; use Gnat.Os_Lib;
with GNAT.Case_Util; use GNAT.Case_Util;

with Artics.Widechar; use Artics.Widechar;

package body Artics.Input_Buffers is
   
--     procedure Free is new Ada.Unchecked_Deallocation
--       (Source_Buffer, Source_Buffer_Ptr);
   --  Procedure for freeing dynamically allocated text buffers

   
   ------------------------------
   -- Initialize_Input_Buffers --
   ------------------------------
   
   procedure Initialize_Input_Buffers is
   begin
      null;
   end Initialize_Input_Buffers;
   
   ----------------------
   -- New_Input_Buffer --
   ----------------------

   function New_Input_Buffer return Input_Buffer is
      
      Buf : Input_Buffer := new Input_Buffer_Record'(No_Input_Buffer_Record);
   begin
      Lines_Tables.Init (Buf.Lines);
      return Buf;
   end New_Input_Buffer;

   ------------------
   -- Reset_Buffer --
   ------------------

   procedure Reset_Buffer (Buf : in out Input_Buffer) is
   begin
      null;
   end Reset_Buffer;

   -----------------
   -- Free_Buffer --
   -----------------

   procedure Free_Buffer (Buf : in out Input_Buffer) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Input_Buffer_Record, Input_Buffer);
   begin
      if Buf /= null then
	 Reset_Buffer (Buf);
	 Free (Buf);
      end if;
   end Free_Buffer;
   
   -----------------------
   -- Get_Source_Buffer --
   -----------------------
   
   function Get_Source_Buffer (Buf : Input_Buffer) return Source_Buffer_Ptr is
   begin
      return Buf.Buffer;
   end Get_Source_Buffer;
   
   -----------------
   -- Source_Char --
   -----------------
   
   function Source_Char
     (Buf    : Input_Buffer;
      Offset : Source_Ptr := 0) return Character is
      
      Sptr : Source_Ptr := Buf.Scan_Ptr + Offset;
   begin
      pragma Assert (Sptr in Buf.Source_First..Buf.Source_Last);
      return Buf.Buffer (Sptr);
   end Source_Char;
   
   ------------------
   -- Get_Scan_Ptr --
   ------------------
   
   function Get_Scan_Ptr (Buf : Input_Buffer) return Source_Ptr is
   begin
      return Buf.Scan_Ptr;
   end Get_Scan_Ptr;
   
   ------------------
   -- Set_Scan_Ptr --
   ------------------
   
   procedure Set_Scan_Ptr
     (Buf     : Input_Buffer;
      Src_Ptr : Source_Ptr) is
   begin
      pragma Assert (Src_Ptr in Buf.Source_First..Buf.Source_Last);
      Buf.Scan_Ptr := Src_Ptr;
   end Set_Scan_Ptr;
   
   -----------------
   -- Inc_Src_Ptr --
   -----------------
   
   procedure Inc_Src_Ptr
     (Buf : Input_Buffer; 
      V   : Source_Ptr := 1) is
      
      New_Ptr : Source_Ptr := Buf.Scan_Ptr + V;
   begin
      pragma Assert (New_Ptr in Buf.Source_First..Buf.Source_Last);
      Buf.Scan_Ptr := New_Ptr;
   end Inc_Src_Ptr;
   
   ------------------
   -- Get_Checksum --
   ------------------
   
   function Get_Checksum (Buf : Input_Buffer) return Word is
   begin
      return Buf.Checksum;
   end Get_Checksum;
   
   -------------------------
   -- Accumulate_Checksum --
   -------------------------
   
   procedure Accumulate_Checksum
     (Buf : Input_Buffer;
      C   : Character) is
   begin
      Gnat.Crc32.Update (Gnat.Crc32.Crc32 (Buf.Checksum), C);
   end Accumulate_Checksum;

   -------------------------
   -- Accumulate_Checksum --
   -------------------------
   
   procedure Accumulate_Checksum
     (Buf : Input_Buffer;
      C   : Char_Code) is
   begin
      if C > 16#FFFF# then
         Accumulate_Checksum (Buf, Character'Val (C / 2 ** 24));
         Accumulate_Checksum (Buf, Character'Val ((C / 2 ** 16) mod 256));
         Accumulate_Checksum (Buf, Character'Val ((C / 256) mod 256));
      else
         Accumulate_Checksum (Buf, Character'Val (C / 256));
      end if;

      Accumulate_Checksum (Buf, Character'Val (C mod 256));
   end Accumulate_Checksum;
   
   -------------------------
   -- Initialize_Checksum --
   -------------------------
   
   procedure Initialize_Checksum (Buf : Input_Buffer) is
   begin
      Gnat.Crc32.Initialize (Gnat.Crc32.Crc32 (Buf.Checksum));
   end Initialize_Checksum;
   
   -----------------------
   -- Reallocate_Buffer --
   -----------------------
   
   procedure Reallocate_Buffer
     (Buf     : Input_Buffer;
      New_Len : Integer)
   is
      subtype size_t is Memory.size_t;
      
      New_Size : constant size_t :=
	size_t (New_Len * Source_Buffer'Component_Size / Storage_Unit);
      
      pragma Suppress (All_Checks);
      
      pragma Warnings (Off);
      --  This use of unchecked conversion is aliasing safe
      
      function To_Address is
	 new Ada.Unchecked_Conversion (Source_Buffer_Ptr, Address);
      
      function To_Source_Buffer_Ptr is new
	Ada.Unchecked_Conversion (Address, Source_Buffer_Ptr);
      
      pragma Warnings (On);
      
      New_Table : Source_Buffer_Ptr;
   begin
      if Buf.Buffer = null then
	 New_Table := To_Source_Buffer_Ptr (Memory.Alloc (New_Size));
	 
      else
	 New_Table := To_Source_Buffer_Ptr
	   (Memory.Realloc (To_Address (Buf.Buffer), New_Size));
      end if;
      
      if New_Table = null then
	 raise Storage_Error;
      end if;
      
      Buf.Buffer := New_Table;
   end Reallocate_Buffer;
   
   ----------------------
   -- Read_Source_File --
   ----------------------

   procedure Read_Source_File
     (Buf       : Input_Buffer;
      File_Name : String)
   is
   begin
      Append_Source_File (Buf, File_Name);
   end Read_Source_File;
   
   ------------------------
   -- Append_Source_File --
   ------------------------

   procedure Append_Source_File
     (Buf       : Input_Buffer;
      File_Name : String)
   is
      
      Source_File_FD : File_Descriptor;
      --  The file descriptor for the current source file. A negative value
      --  indicates failure to open the specified source file.

      Len : Integer;
      --  Length of file, assume no more than 2 gigabytes of source

      Actual_Len : Integer;

      Status : Boolean;
      pragma Warnings (Off, Status);
      --  For the call to Close
      
      Lo  : Source_Ptr;
      Hi  : Source_Ptr;

      New_Len : Integer;
      
      Source_Name : String := File_Name & ASCII.NUL;
   begin
      if Buf.Buffer = null then
         Buf.Source_First := 0;
         Buf.Source_Last := 0;
      end if;
      
      --  Open the source FD, note that we open in binary mode, because as
      --  documented in the spec, the caller is expected to handle either
      --  DOS or Unix mode files, and there is no point in wasting time on
      --  text translation when it is not required.
      
      Source_File_FD := Open_Read (Source_Name'Address, Binary);

      if Source_File_FD = Invalid_FD then
         return;
      end if;
      
      --  Prepare to read data from the file

      Len := Integer (File_Length (Source_File_FD));

      --  Set Hi so that length is one more than the physical length,
      --  allowing for the extra EOF character at the end of the buffer
      
      Lo := Buf.Source_First;
      Hi := Buf.Source_Last;
      
      if Buf.Buffer = null then
	 New_Len := Len + 1;
      else
	 New_Len := Integer (Hi - Lo) + Len;
      end if;

      --  Do the actual read operation

      --  Allocate source buffer, allowing extra character at end for EOF
      
      Reallocate_Buffer (Buf, New_Len);
      
      --  Some systems have file types that require one read per line,
      --  so read until we get the Len bytes or until there are no more
      --  characters.
      
      Hi := Buf.Source_Last;
      loop
	 Actual_Len := Read (Source_File_FD, Buf.Buffer (Hi)'Address, Len);
	 Hi := Hi + Source_Ptr (Actual_Len);
	 exit when Actual_Len = Len or else Actual_Len <= 0;
      end loop;
      
      Buf.Buffer (Hi) := EOF;
	 
      Buf.Source_First := Lo;
      Buf.Source_Last  := Hi;
      
      --  Read is complete, get time stamp and close file and we are done

      Close (Source_File_FD, Status);

      --  The status should never be False. But, if it is, what can we do?
      --  So, we don't test it.

   end Append_Source_File;
   
   ------------------------
   -- Read_Source_String --
   ------------------------

   procedure Read_Source_String 
     (Buf : Input_Buffer;
      S   : String) 
   is
   begin
      Append_Source_String (Buf, S);
   end Read_Source_String;
   
   --------------------------
   -- Append_Source_String --
   --------------------------

   procedure Append_Source_String 
     (Buf : Input_Buffer;
      S   : String) 
   is
      
      Len     : Integer;
      New_Len : Integer;
      Lo      : Source_Ptr;
      Hi      : Source_Ptr;
   begin
      Len := Integer (S'Length);
      
      if Len = 0 then
         return;
      end if;
      
      if Buf.Buffer = null then
         Buf.Source_First := 0;
         Buf.Source_Last := 0;
      end if;
      
      --  New length is the length of the string to append + Actual length
      --  of buffer. We do not add  one to store EOF, if the actual buffer is
      --  empty, as we reuse the last character of actual buffer wich is EOF.
      --  If the actual buffer is empty, we add one to store the extra EOF.
      
      Lo := Buf.Source_First;
      Hi := Buf.Source_Last;
      
      if Buf.Buffer = null then
	 New_Len := Len + 1;
      else
	 New_Len := Integer (Hi - Lo) + Len;
      end if;

      Reallocate_Buffer (Buf, New_Len);

      declare
	 J : Source_Ptr;
      begin
	 J := Buf.Source_Last;
	 for I in S'Range loop
	    Buf.Buffer (J) := S (I);
	 end loop;
      
         Buf.Buffer (J + 1) := EOF;
         Buf.Source_Last := J + 1;
      end;
   end Append_Source_String;
   
   -----------------------
   -- Get_Column_Number --
   -----------------------

   function Get_Column_Number
     (Buf : Input_Buffer;
      P   : Source_Ptr) return Column_Number is
      
      S      : Source_Ptr;
      C      : Column_Number;
      Src    : Source_Buffer_Ptr;

   begin
      --  If the input source pointer is not a meaningful value then return
      --  at once with column number 1. This can happen for a file not found
      --  condition for a file loaded indirectly by RTE, and also perhaps on
      --  some unknown internal error conditions. In either case we certainly
      --  don't want to blow up.

      if P < 1 then
         return 1;

      else
         Src := Buf.Buffer;
         S := Line_Start (Buf, P);
         C := 1;

         while S < P loop
            if Src (S) = Ascii.HT then
               C := (C - 1) / 8 * 8 + (8 + 1);
               S := S + 1;

            --  Deal with wide character case, but don't include brackets
            --  notation in this circuit, since we know that this will
            --  display unencoded (no one encodes brackets notation).

            --  elsif Src(S) /= '[' and then Is_Start_Of_Wide_Char (Src, S) then
            --     C := C + 1;
            --     Skip_Wide (Src, S);

            --  Normal (non-wide) character case or brackets sequence

            else
               C := C + 1;
               S := S + 1;
            end if;
         end loop;

         return C;
      end if;
   end Get_Column_Number;
   
   -----------------------
   -- Get_Column_Number --
   -----------------------

   function Get_Column_Number (Buf : Input_Buffer) return Column_Number is
   begin
      return Get_Column_Number (Buf, Buf.Scan_Ptr);
   end Get_Column_Number;
   
   ----------------------
   -- Set_Start_Column --
   ----------------------

   function Set_Start_Column
     (Buf : Input_Buffer;
      P   : in out Source_Ptr) return Column_Number is
      
      Src          : constant Source_Buffer_Ptr := Buf.Buffer;
      Start_Column : Column_Number := 0;
   begin
      --  Outer loop scans past horizontal tab characters

      Tabs_Loop : loop

         --  Inner loop scans past blanks as fast as possible, bumping Scan_Ptr
         --  past the blanks and adjusting Start_Column to account for them.

         Blanks_Loop : loop
            if Src (P) = ' ' then
               if Src (P + 1) = ' ' then
                  if Src (P + 2) = ' ' then
                     if Src (P + 3) = ' ' then
                        if Src (P + 4) = ' ' then
                           if Src (P + 5) = ' ' then
                              if Src (P + 6) = ' ' then
                                 P := P + 7;
                                 Start_Column := Start_Column + 7;
                              else
                                 P := P + 6;
                                 Start_Column := Start_Column + 6;
                                 exit Blanks_Loop;
                              end if;
                           else
                              P := P + 5;
                              Start_Column := Start_Column + 5;
                              exit Blanks_Loop;
                           end if;
                        else
                           P := P + 4;
                           Start_Column := Start_Column + 4;
                           exit Blanks_Loop;
                        end if;
                     else
                        P := P + 3;
                        Start_Column := Start_Column + 3;
                        exit Blanks_Loop;
                     end if;
                  else
                     P := P + 2;
                     Start_Column := Start_Column + 2;
                     exit Blanks_Loop;
                  end if;
               else
                  P := P + 1;
                  Start_Column := Start_Column + 1;
                  exit Blanks_Loop;
               end if;
            else
               exit Blanks_Loop;
            end if;
         end loop Blanks_Loop;

         --  Outer loop keeps going only if a horizontal tab follows

         if Src (P) = Ascii.Ht then
            P := P + 1;
            Start_Column := (Start_Column / 8) * 8 + 8;
         else
            exit Tabs_Loop;
         end if;
      end loop Tabs_Loop;

      return Start_Column;
   end Set_Start_Column;
   
   ----------------------
   -- Set_Start_Column --
   ----------------------
   
   function Set_Start_Column (Buf : Input_Buffer) return Column_Number is
   begin
      return Set_Start_Column (Buf, Buf.Scan_Ptr);
   end Set_Start_Column;
   
   ----------------
   -- Line_Start --
   ----------------

   function Line_Start
     (Buf : Input_Buffer;
      P   : Source_Ptr) return Source_Ptr is
      
      Src    : constant Source_Buffer_Ptr := Buf.Buffer;
      Sfirst : constant Source_Ptr := Buf.Source_First;
      S      : Source_Ptr;
   begin
      S := P;
      while S > Sfirst
        and then Src (S - 1) /= Ascii.CR
        and then Src (S - 1) /= Ascii.LF
      loop
         S := S - 1;
      end loop;

      return S;
   end Line_Start;

   ----------------
   -- Line_Start --
   ----------------

   function Line_Start (Buf : Input_Buffer) return Source_Ptr is
   begin
         return Line_Start (Buf, Buf.Scan_Ptr);
   end Line_Start;
   
   ---------------------------
   -- Skip_Line_Terminators --
   ---------------------------

   procedure Skip_Line_Terminators
     (Buf      : Input_Buffer;
      P        : in out Source_Ptr;
      Physical : in out Boolean) is
      
      Chr : constant Character := Source_Char (Buf);
   begin
      if Chr = Ascii.CR then
         if Source_Char (Buf, 1) = Ascii.LF then
            P := P + 2;
         else
	    P := P + 1;
         end if;

      elsif Chr = Ascii.LF then
	 P := P + 1;
	 
      elsif Chr = Ascii.FF or else Chr = Ascii.VT then
	 P := P + 1;
         return;

         --  Otherwise we have a wide character

      else
         Skip_Wide (Buf.Buffer, P);
      end if;

      --  Make entry in lines table if not already made (in some scan backup
      --  cases, we will be rescanning previously scanned source, so the
      --  entry may have already been made on the previous forward scan).
      
      if Source_Char (Buf) /= EOF
	and then P > Last_Source_Line (Buf)
      then
	 Add_Lines_Entry (Buf, P);
      end if;
   end Skip_Line_Terminators;
   
   ---------------------------
   -- Skip_Line_Terminators --
   ---------------------------

   procedure Skip_Line_Terminators
     (Buf      : Input_Buffer; 
      Physical : in out Boolean) is
   begin
      Skip_Line_Terminators (Buf, Buf.Scan_Ptr, Physical);
   end Skip_Line_Terminators;
   
   ----------------------
   -- Last_Source_Line --
   ----------------------
   
   function Last_Source_Line (Buf : Input_Buffer) return Source_Ptr is
      L : Line_Number;
   begin
      L := Lines_Tables.Last (Buf.Lines);
      return Buf.Lines.Table (L);
   end Last_Source_Line;
   
   ---------------------
   -- Add_Lines_Entry --
   ---------------------

   procedure Add_Lines_Entry
     (Buf : Input_Buffer;
      P   : Source_Ptr) is
   begin
      Lines_Tables.Increment_Last (Buf.Lines);
      Buf.Lines.Table (Lines_Tables.Last (Buf.Lines)) := P;
   end Add_Lines_Entry;
   
   ---------------------
   -- Add_Lines_Entry --
   ---------------------

   procedure Add_Lines_Entry (Buf : Input_Buffer) is
   begin
      Add_Lines_Entry (Buf, Buf.Scan_Ptr);
   end Add_Lines_Entry;
   
   ---------------------
   -- Get_Line_Number --
   ---------------------

   function Get_Line_Number
     (Buf : Input_Buffer;
      P   : Source_Ptr) return Line_Number is
      
      Lines : Lines_Tables.Instance := Buf.Lines;
      Lo    : Line_Number;
      Hi    : Line_Number;
      Mid   : Line_Number;
      Loc   : Source_Ptr;

   begin
      --  If the input source pointer is not a meaningful value then return
      --  at once with line number 1. This can happen for a file not found
      --  condition for a file loaded indirectly by RTE, and also perhaps on
      --  some unknown internal error conditions. In either case we certainly
      --  don't want to blow up.

      if P < 1 then
         return 1;

      --  Otherwise we can do the binary search

      else
         Loc   := P;
         Lines := Buf.Lines;
         Lo    := 1;
         Hi    := Last_Source_Line (Buf);

         loop
            Mid := (Lo + Hi) / 2;

            if Loc < Lines.Table (Mid) then
               Hi := Mid - 1;

            else -- Loc >= Table (Mid)

               if Mid = Hi or else
                  Loc < Lines.Table (Mid + 1)
               then
                  return Mid;
               else
                  Lo := Mid + 1;
               end if;

            end if;

         end loop;
      end if;
   end Get_Line_Number;
   
   ---------------------
   -- Get_Line_Number --
   ---------------------

   function Get_Line_Number (Buf : Input_Buffer) return Line_Number is
   begin
      return Get_Line_Number (Buf, Buf.Scan_Ptr);
   end Get_Line_Number;
      
   
   ------------------
   -- Source_First --
   ------------------
   
   function Source_First (Buf : Input_Buffer) return Source_Ptr is
   begin
      return Buf.Source_First;
   end Source_First;
   
   ----------------------
   -- Set_Source_First --
   ----------------------
   
   procedure Set_Source_First
     (Buf  : Input_Buffer;
      Frst : Source_Ptr) is
   begin
      Buf.Source_First := Frst;
   end Set_Source_First;
   
   -----------------
   -- Source_Last --
   -----------------
   
   function Source_Last (Buf : Input_Buffer) return Source_Ptr is
   begin
      return Buf.Source_Last;
   end Source_Last;
   
   ---------------------
   -- Set_Source_Last --
   ---------------------
   
   procedure Set_Source_Last
     (Buf : Input_Buffer;
      Lst : Source_Ptr) is
   begin
      Buf.Source_Last := Lst;
   end Set_Source_Last;
   
   ------------------------
   -- Source_Lines_Count --
   ------------------------

   function Source_Lines_Count (Buf : Input_Buffer) return Natural is
   begin
      return Natural (Buf.Source_Last - Buf.Source_First);
   end Source_Lines_Count;

   -------------------
   -- Source_Offset --
   -------------------

   function Source_Offset
     (Buf : Input_Buffer;
      S   : Source_Ptr) return Natural is
   begin
      return Natural (S - Buf.Source_First);
   end Source_Offset;
   
end Artics.Input_Buffers;
