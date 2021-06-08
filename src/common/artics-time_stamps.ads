------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 2012-2015, Free Software Foundation, Inc.         --
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

with Gnat.OS_Lib; use Gnat.OS_Lib;

package Artics.Time_Stamps is
   
   -----------------------------------
   -- Representation of Time Stamps --
   -----------------------------------

   --  All compiled units are marked with a time stamp which is derived from
   --  the source file (we assume that the host system has the concept of a
   --  file time stamp which is modified when a file is modified). These
   --  time stamps are used to ensure consistency of the set of units that
   --  constitutes a library. Time stamps are 12 character strings with
   --  with the following format:

   --     YYYYMMDDHHMMSS

   --       YYYY   year
   --       MM     month (2 digits 01-12)
   --       DD     day (2 digits 01-31)
   --       HH     hour (2 digits 00-23)
   --       MM     minutes (2 digits 00-59)
   --       SS     seconds (2 digits 00-59)

   --  In the case of Unix systems (and other systems which keep the time in
   --  GMT), the time stamp is the GMT time of the file, not the local time.
   --  This solves problems in using libraries across networks with clients
   --  spread across multiple time-zones.

   Time_Stamp_Length : constant := 14;
   --  Length of time stamp value

   subtype Time_Stamp_Index is Natural range 1 .. Time_Stamp_Length;
   type Time_Stamp_Type is new String (Time_Stamp_Index);
   --  Type used to represent time stamp

   Empty_Time_Stamp : constant Time_Stamp_Type := (others => ' ');
   --  Value representing an empty or missing time stamp. Looks less than any
   --  real time stamp if two time stamps are compared. Note that although this
   --  is not private, clients should not rely on the exact way in which this
   --  string is represented, and instead should use the subprograms below.

   Dummy_Time_Stamp : constant Time_Stamp_Type := (others => '0');
   --  This is used for dummy time stamp values used in the D lines for
   --  non-existent files, and is intended to be an impossible value.

   function "="  (Left, Right : Time_Stamp_Type) return Boolean;
   function "<=" (Left, Right : Time_Stamp_Type) return Boolean;
   function ">=" (Left, Right : Time_Stamp_Type) return Boolean;
   function "<"  (Left, Right : Time_Stamp_Type) return Boolean;
   function ">"  (Left, Right : Time_Stamp_Type) return Boolean;
   --  Comparison functions on time stamps. Note that two time stamps are
   --  defined as being equal if they have the same day/month/year and the
   --  hour/minutes/seconds values are within 2 seconds of one another. This
   --  deals with rounding effects in library file time stamps caused by
   --  copying operations during installation. We have particularly noticed
   --  that WinNT seems susceptible to such changes.
   --
   --  Note : the Empty_Time_Stamp value looks equal to itself, and less than
   --  any non-empty time stamp value.
   
   procedure Split_Time_Stamp
     (TS      : Time_Stamp_Type;
      Year    : out Natural;
      Month   : out Natural;
      Day     : out Natural;
      Hour    : out Natural;
      Minutes : out Natural;
      Seconds : out Natural);
   --  Given a time stamp, decompose it into its components

   procedure Make_Time_Stamp
     (Year    : Natural;
      Month   : Natural;
      Day     : Natural;
      Hour    : Natural;
      Minutes : Natural;
      Seconds : Natural;
      TS      : out Time_Stamp_Type);
   --  Given the components of a time stamp, initialize the value
   
   function File_Stamp (Name : String) return Time_Stamp_Type;

   function OS_Time_To_Time_Stamp (T : OS_Time) return Time_Stamp_Type;
   
end Artics.Time_Stamps;
