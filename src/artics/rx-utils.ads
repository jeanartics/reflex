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

--  Various utility subprograms used in GNATCOLL, and that can easily be reused
--  elsewhere

pragma Ada_2012;

with Ada.Calendar.Time_Zones; use Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with GNAT.Calendar;
with GNAT.Expect;
with GNAT.Strings;

package Rx.Utils is

   -------------
   -- Strings --
   -------------

   --  Some simple-minded string manipulation routines.
   --  See also GNATCOLL.Strings providing alternative more efficient
   --  implementation

   type Cst_String_Access is access constant String;

   procedure Free (List : in out GNAT.Strings.String_List);
   --  Free the memory used by List.

   function Equal (S1, S2 : String; Case_Sensitive : Boolean) return Boolean;
   function Case_Insensitive_Equal (S1, S2 : String) return Boolean;
   pragma Inline (Equal, Case_Insensitive_Equal);
   --  Compare two strings

   function Image
     (Value      : Integer;
      Min_Width  : Integer;
      Force_Sign : Boolean := False;
      Padding    : Character := '0') return String;
   --  Returns Value as a string, using at least Width digits (padded with
   --  leading characters Padding if necessary); negative values will always
   --  have a leading minus sign; positive values will have a leading plus sign
   --  if Force_Sign is True.
   --  If you set Min_Width to 1, the result is similar to 'Image, without the
   --  leading space for positive numbers.

   procedure Replace
     (S           : in out Ada.Strings.Unbounded.Unbounded_String;
      Pattern     : String;
      Replacement : String)
     with Pre => Pattern /= "";
   --  Returns S, with all occurrences of Pattern replaced with Replacement

   function Replace
     (S : String; Pattern : String; Replacement : String) return String
     with Pre => Pattern /= "";
   --  Returns S, with all occurrences of Pattern replaced with Replacement

   procedure Split
     (Str      : String;
      On       : String;
      For_Each : access function (Item : String) return Boolean);
   --  Splits the string on the given delimiter "On" and calls the function
   --  For_Each for every found substring not including the delimiter.
   --  If For_Each returns False the string processing stops.

   function Split
     (Str              : String;
      On               : Character;
      Omit_Empty_Lines : Boolean := True)
      return GNAT.Strings.String_List_Access;
   --  Splits the string on the given character.
   --  The result depends on the value of Omit_Empty_Lines. For instance, the
   --  string    "a" & ASCII.LF & ASCII.LF & "b"   will be split as:
   --       ["a", "b"]  if Omit_Empty_Lines is true
   --       ["a", "", "b"] otherwise
   --
   --  Result must be freed by caller.
   --  See also Split below

   type Unbounded_String_Array is array (Natural range <>) of
     Ada.Strings.Unbounded.Unbounded_String;

   Empty_Array : constant Unbounded_String_Array;

   function Split
     (Str              : String;
      On               : Character;
      Omit_Empty_Lines : Boolean := True) return Unbounded_String_Array;
   --  Same as Split above, returning an Unbounded_String_Array that does not
   --  need to be freed.

   function Capitalize (Name : String) return String;
   --  Capitalizes a string, i.e. puts in upper case the first character and
   --  any character preceded by '_'

   function Is_Whitespace (Char : Character) return Boolean;
   --  Returns True if Char is a space, new line, or tab; otherwise returns
   --  False.

   function Starts_With (Str : String; Prefix : String) return Boolean;
   --  Returns True if Str starts with Prefix

   function Ends_With (Str : String; Suffix : String) return Boolean;
   --  Returns True if Str ends with Suffix

   procedure Skip_Blanks (Str : String; Index : in out Natural);
   procedure Skip_Blanks_Backward (Str : String; Index : in out Natural);
   --  If Str(Index) is a white space, new line, or tab, then skip it and all
   --  following ones. On exit, Index points to the first non white space
   --  character, or after Str'Last.
   --  Skip_Blanks_Backward moves Index backward instead, and will leave it
   --  before Str'First if no non-whitespace was found.

   function Find_Char (Str : String; Char : Character) return Natural;
   --  Returns the first occurrence of Char after Str'First (use substrings for
   --  later occurrences). Returns Str'Last + 1 if there is no match

   function Join (Str : String; List : GNAT.Strings.String_List) return String;
   --  Returns a string that is the concatenation of the list elements,
   --  separated by Str: (List(1) & Str & List(2) & Str & ...)
   --  null elements in list are skipped

   function EOL (Str : String) return Natural;
   pragma Inline (EOL);
   --  Returns the first ASCII.LF character after Str'First (use substrings for
   --  subsequent lines). The result is either Str'Last+1 or points to the
   --  first ASCII.LF found.

   function Line_Start (Str : String; P : Natural) return Natural;
   --  Returns the start of the line pointed by P

   function Line_End (Str : String; P : Natural) return Natural;
   --  Returns the end of the line pointed by P

   procedure Skip_Lines
     (Str           : String;
      Lines         : Integer;
      Index         : in out Natural;
      Lines_Skipped : out Natural);
   --  Skips lines forward or backward. Sets Index to the beginning of a line.
   --  Lines_Skipped is the number of lines that have actually been skipped.
   --  Use with Skip_To_Column to go to a specific position in a buffer.

   procedure Skip_To_Column
     (Str       : String;
      Columns   : Integer := 0;
      Index     : in out Integer;
      Tab_Width : Integer := 8);
   --  Assuming Index points to the beginning of a line (as is the case after
   --  Skip_Lines for instance), jumps to the specific column on that line.
   --  This procedure handles tabulations (i.e. Columns are columns visible to
   --  the user following the tab expansion).

   function Forward_UTF8_Char
     (Str   : String;
      Index : Integer) return Integer;
   --  Moves Index one character forward, taking into account UTF8 encoding.

   function Next_Line (Str : String; P : Natural) return Natural;
   --  Returns the start of the next line or Str'Last if the end of Str
   --  is reached without finding next line.

   function Previous_Line (Str : String; P : Natural) return Natural;
   --  Returns the start of the previous line or Str'First if P already
   --  points to the first line of Str.

   function Is_Blank_Line
     (Str : String; Index : Natural := 0) return Boolean;
   --  Returns True if the line pointed by Index only contains blank characters
   --  (' ', HT, LF, CR). By default, if Index is 0, then the line considered
   --  is the first line of the buffer.

   procedure Skip_To_String
     (Str      : String;
      Index     : in out Natural;
      Substring : String);
   --  Skips every character until an occurrence of Substring is found.
   --  Index is set to the first character of the occurrence.

   function Strip_Character (Text : String; C : Character) return String;
   --  Returns a version of Text after stripping all C's from the string

   function Strip_CR (Text : String) return String;
   pragma Inline (Strip_CR);
   --  Returns a version of Text after stripping all ASCII.CR from the string.
   --  This function is used on Windows or when the Strip_CR preference is
   --  enabled (for systems that share DOS files).
   --  CR/LF sequences are replaced by LF chars.

   function Predicate
      (Text : String;
        Predicate : access function (Item : Character) return Boolean)
      return Boolean
      is (for all C of Text => Predicate (C));
   --  Whether all characters in Text match Predicate.
   --  This can be used with the various utilities in Ada.Characters.Handling,
   --  for instance to check whether a string is made up of only lower case
   --  characters.

   function Is_Alphanumeric (Text : String) return Boolean
     is (Predicate (Text, Ada.Characters.Handling.Is_Alphanumeric'Access));
   function Is_Lower (Text : String) return Boolean
     is (Predicate (Text, Ada.Characters.Handling.Is_Lower'Access));
   function Is_Upper (Text : String) return Boolean
     is (Predicate (Text, Ada.Characters.Handling.Is_Upper'Access));

   function Is_Identifier (C : Character) return Boolean
      is (C = '_' or else Ada.Characters.Handling.Is_Alphanumeric (C));
   function Is_Identifier (Text : String) return Boolean
      is (Predicate (Text, Is_Identifier'Access));
   --  Whether C is a valid character for an identifier (in most programming
   --  languages). It doesn't check whether the identifier starts with an
   --  underscore for instance, just whether the characters would be valid.

   ------------
   -- Expect --
   ------------

   function Get_Command_Output
     (Command : access GNAT.Expect.Process_Descriptor'Class) return String;
   --  Runs Command until it finishes, and return its output.
   --  This automatically closes the process cleanly.

   ------------------
   -- File systems --
   ------------------

   function Full_Executable_Location return String;
   --  Returns the name of the directory where the executable is stored
   --  (so if you are running "prefix/my_exe", you would get "prefix/").
   
   function Executable_Location return String;
   --  Returns the name of the parent directory where the executable is stored
   --  (so if you are running "prefix/my_exe", you would get "prefix/").
   --  A special case is done for "bin" directories, which are consumed
   --  (so if you are running "prefix/bin/my_exe", you would get "prefix/").
   --  The returned directory always ends with a directory separator.

   function Executable_Path return String;
   --  Returns absolute path to the current executable.
   --
   --  On Linux, Windows and MacOS the procedure is safe and will return always
   --  the right executable. For other platforms the function might return
   --  an incorrect value if environment is modified (executable parameters,
   --  current directory and/or PATH variable).

   function Is_Directory_Separator (C : Character) return Boolean;
   pragma Inline (Is_Directory_Separator);
   --  Returns True if C is a directory separator

   function Join_Path
      (Path : String; Path1, Path2, Path3, Path4 : String := "") return String;
   --  Join one or more path into a single one. Note that if one argument is an
   --  absolute path then previous arguments will be ignored.

   procedure Add_Search_Path (Variable : String; Path : String);
   --  Prepend a path to an environment variable containing a list of paths.
   --  If Path is already in the search list, subsequent occurences will be
   --  removed and thus limit final path value size.

   -----------
   -- Dates --
   -----------

   No_Time : Ada.Calendar.Time renames GNAT.Calendar.No_Time;

   function Time_Value (Str : String) return Ada.Calendar.Time;
   --  Checks the validity of Str as a string representing a date
   --  using the same formats as in GNAT.Calendar.Time_IO.Value. In addition,
   --  it also supports timezones (as output for instance by PostgreSQL)
   --     1970-01-01 12:00:00+01
   --  and the ISO format
   --     1970-01-01T12:00:00+01   or   1970-01-01T12:00:00Z
   --  All the above can start with the day spelled out, as in "thu, "
   --
   --  The input date is assumed to be in UTC unless a timezone is specified
   --  as hours with a final "[+-]\d\d", or as hours and minutes with
   --  "[+-]\d\d\d\d" or "[+-]\d\d:\d\d"
   --
   --  The output date is always returned for the UTC time zone.
   --  So if you are in GMT+12 and you parse "2017-01-01T11:00:00", the
   --  result date will be:  year=2016, month=12, day=31, time=23:00:00.
   --  If you want to spit the resulting time to extract the components,
   --  you should use:
   --     Ada.Calendar.Formatting.Split (.., Time_Zone => 0);

   function Truncate
     (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0) return Time;
   --  Removes time part from the date in specified timezone.
   --  For example, if we want to truncate "2015 May 10 05:00 GMT+6" time at
   --  UTC timezone we are going to get "2015 May 9, 00:00 UTC" because
   --  "2015 May 10 05:00 GMT+6" equal to "2015 May 9 23:00 UTC".

private

   Empty_Array : constant Unbounded_String_Array (1 .. 0) :=
                   (others => Ada.Strings.Unbounded.Null_Unbounded_String);

end Rx.Utils;
