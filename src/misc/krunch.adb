------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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
-- Reflex is a fork from the GNAT compiler. GNAT was originally developed   --
-- by the GNAT team at  New York University. Extensive  contributions to    --
-- GNAT were provided by Ada Core Technologies Inc. Reflex is developed  by --
-- the Artics team at Grenoble.                                             --
--                                                                          --
------------------------------------------------------------------------------

with Hostparm;
procedure Krunch
  (Buffer    : in out String;
   Len       : in out Natural;
   Maxlen    : Natural;
   No_Predef : Boolean)

is
   B1       : Character renames Buffer (Buffer'First);
   Curlen   : Natural;
   Krlen    : Natural;
   Num_Seps : Natural;
   Startloc : Natural;

begin
   --  Deal with special predefined children cases. Startloc is the first
   --  location for the krunch, set to 1, except for the predefined children
   --  case, where it is set to 3, to start after the standard prefix.

   if No_Predef then
      Startloc := 1;
      Curlen := Len;
      Krlen := Maxlen;

   elsif Len >= 18
     and then Buffer (Buffer'First .. Buffer'First + 16) = "ada-wide_text_io-"
   then
      Startloc := 3;
      Buffer (Buffer'First + 1 .. Buffer'First + 4) := "-wt-";
      Buffer (Buffer'First + 5 .. Len - 12) := Buffer (Buffer'First + 17 .. Len);
      Curlen := Len - 12;
      Krlen  := 8;

   elsif Len >= 4 and then Buffer (Buffer'First .. Buffer'First + 3) = "ada-" then
      Startloc := 3;
      Buffer (Buffer'First + 1 .. Len - 2) := Buffer (Buffer'First + 3 .. Len);
      Curlen := Len - 2;
      Krlen  := 8;

   elsif Len >= 5 and then Buffer (Buffer'First .. Buffer'First + 4) = "gnat-" then
      Startloc := 3;
      Buffer (Buffer'First + 1 .. Len - 3) := Buffer (Buffer'First + 4 .. Len);
      Curlen := Len - 3;
      Krlen  := 8;

   elsif Len >= 7 and then Buffer (Buffer'First .. Buffer'First + 6) = "system-" then
      Startloc := 3;
      Buffer (Buffer'First + 1 .. Len - 5) := Buffer (Buffer'First + 6 .. Len);
      Curlen := Len - 5;
      Krlen  := 8;

   elsif Len >= 11 and then Buffer (Buffer'First .. Buffer'First + 10) = "interfaces-" then
      Startloc := 3;
      Buffer (Buffer'First + 1 .. Len - 9) := Buffer (Buffer'First + 10 .. Len);
      Curlen := Len - 9;
      Krlen  := 8;

   --  For the renamings in the obsolescent section, we also force krunching
   --  to 8 characters, but no other special processing is required here.
   --  Note that text_io and calendar are already short enough anyway.

   elsif     (Len =  9 and then Buffer (Buffer'First ..  Buffer'First + 8) = "direct_io")
     or else (Len = 10 and then Buffer (Buffer'First .. Buffer'First + 9) = "interfaces")
     or else (Len = 13 and then Buffer (Buffer'First .. Buffer'First + 12) = "io_exceptions")
     or else (Len = 12 and then Buffer (Buffer'First .. Buffer'First + 11) = "machine_code")
     or else (Len = 13 and then Buffer (Buffer'First .. Buffer'First + 12) = "sequential_io")
     or else (Len = 20 and then Buffer (Buffer'First .. Buffer'First + 19) = "unchecked_conversion")
     or else (Len = 22 and then Buffer (Buffer'First .. Buffer'First + 21) = "unchecked_deallocation")
   then
      Startloc := 1;
      Krlen    := 8;
      Curlen   := Len;

   --  Special case of a child unit whose parent unit is a single letter that
   --  is A, G, I, or S. In order to prevent confusion with krunched names
   --  of predefined units use a tilde rather than a minus as the second
   --  character of the file name.  On VMS a tilde is an illegal character
   --  in a file name, so a dollar_sign is used instead.

   elsif Len > 1
     and then Buffer (Buffer'First + 1) = '-'
     and then (B1 = 'a' or else B1 = 'g' or else B1 = 'i' or else B1 = 's')
     and then Len <= Maxlen
   then
      if Hostparm.OpenVMS then
         Buffer (Buffer'First + 1) := '$';
      else
         Buffer (Buffer'First + 1) := '~';
      end if;

      return;

   --  Normal case, not a predefined file

   else
      Startloc := 1;
      Curlen   := Len;
      Krlen    := Maxlen;
   end if;

   --  Immediate return if file name is short enough now

   if Curlen <= Krlen then
      Len := Curlen;
      return;
   end if;

   --  For now, refuse to krunch a name that contains an ESC character (wide
   --  character sequence) since it's too much trouble to do this right ???

   for J in 1 .. Curlen loop
      if Buffer (J) = ASCII.ESC then
         return;
      end if;
   end loop;

   --  Count number of separators (minus signs and underscores) and for now
   --  replace them by spaces. We keep them around till the end to control
   --  the krunching process, and then we eliminate them as the last step

   Num_Seps := 0;

   for J in Startloc .. Curlen loop
      if Buffer (J) = '-' or else Buffer (J) = '_' then
         Buffer (J) := ' ';
         Num_Seps := Num_Seps + 1;
      end if;
   end loop;

   --  Now we do the one character at a time krunch till we are short enough

   while Curlen - Num_Seps > Krlen loop
      declare
         Long_Length : Natural := 0;
         Long_Last   : Natural := 0;
         Piece_Start : Natural;
         Ptr         : Natural;

      begin
         Ptr := Startloc;

         --  Loop through pieces to find longest piece

         while Ptr <= Curlen loop
            Piece_Start := Ptr;

            --  Loop through characters in one piece of name

            while Ptr <= Curlen and then Buffer (Ptr) /= ' ' loop
               Ptr := Ptr + 1;
            end loop;

            if Ptr - Piece_Start > Long_Length then
               Long_Length := Ptr - Piece_Start;
               Long_Last := Ptr - 1;
            end if;

            Ptr := Ptr + 1;
         end loop;

         --  Remove last character of longest piece

         if Long_Last < Curlen then
            Buffer (Long_Last .. Curlen - 1) :=
              Buffer (Long_Last + 1 .. Curlen);
         end if;

         Curlen := Curlen - 1;
      end;
   end loop;

   --  Final step, remove the spaces

   Len := 0;

   for J in 1 .. Curlen loop
      if Buffer (J) /= ' ' then
         Len := Len + 1;
         Buffer (Len) := Buffer (J);
      end if;
   end loop;

   return;

end Krunch;
