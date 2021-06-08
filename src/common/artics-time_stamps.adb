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

package body Artics.Time_Stamps is
   
   function V (T : Time_Stamp_Type; X : Time_Stamp_Index) return Natural;
   --  Extract two decimal digit value from time stamp

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Time_Stamp_Type) return Boolean is
   begin
      return not (Left = Right) and then String (Left) < String (Right);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Time_Stamp_Type) return Boolean is
   begin
      return not (Left > Right);
   end "<=";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Time_Stamp_Type) return Boolean is
      Sleft  : Natural;
      Sright : Natural;

   begin
      if String (Left) = String (Right) then
         return True;

      elsif Left (1) = ' ' or else Right (1) = ' ' then
         return False;
      end if;

      --  In the following code we check for a difference of 2 seconds or less

      --  Recall that the time stamp format is:

      --     Y  Y  Y  Y  M  M  D  D  H  H  M  M  S  S
      --    01 02 03 04 05 06 07 08 09 10 11 12 13 14

      --  Note that we do not bother to worry about shifts in the day.
      --  It seems unlikely that such shifts could ever occur in practice
      --  and even if they do we err on the safe side, i.e., we say that the
      --  time stamps are different.

      Sright := V (Right, 13) + 60 * (V (Right, 11) + 60 * V (Right, 09));
      Sleft  := V (Left,  13) + 60 * (V (Left,  11) + 60 * V (Left,  09));

      --  So the check is: dates must be the same, times differ 2 sec at most

      return abs (Sleft - Sright) <= 2
         and then String (Left (1 .. 8)) = String (Right (1 .. 8));
   end "=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Time_Stamp_Type) return Boolean is
   begin
      return not (Left = Right) and then String (Left) > String (Right);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Time_Stamp_Type) return Boolean is
   begin
      return not (Left < Right);
   end ">=";
   
   ---------------------
   -- Make_Time_Stamp --
   ---------------------

   procedure Make_Time_Stamp
     (Year    : Natural;
      Month   : Natural;
      Day     : Natural;
      Hour    : Natural;
      Minutes : Natural;
      Seconds : Natural;
      TS      : out Time_Stamp_Type)
   is
      Z : constant := Character'Pos ('0');

   begin
      TS (01) := Character'Val (Z + Year / 1000);
      TS (02) := Character'Val (Z + (Year / 100) mod 10);
      TS (03) := Character'Val (Z + (Year / 10) mod 10);
      TS (04) := Character'Val (Z + Year mod 10);
      TS (05) := Character'Val (Z + Month / 10);
      TS (06) := Character'Val (Z + Month mod 10);
      TS (07) := Character'Val (Z + Day / 10);
      TS (08) := Character'Val (Z + Day mod 10);
      TS (09) := Character'Val (Z + Hour / 10);
      TS (10) := Character'Val (Z + Hour mod 10);
      TS (11) := Character'Val (Z + Minutes / 10);
      TS (12) := Character'Val (Z + Minutes mod 10);
      TS (13) := Character'Val (Z + Seconds / 10);
      TS (14) := Character'Val (Z + Seconds mod 10);
   end Make_Time_Stamp;

   ----------------------
   -- Split_Time_Stamp --
   ----------------------

   procedure Split_Time_Stamp
     (TS      : Time_Stamp_Type;
      Year    : out Natural;
      Month   : out Natural;
      Day     : out Natural;
      Hour    : out Natural;
      Minutes : out Natural;
      Seconds : out Natural)
   is

   begin
      --     Y  Y  Y  Y  M  M  D  D  H  H  M  M  S  S
      --    01 02 03 04 05 06 07 08 09 10 11 12 13 14

      Year    := 100 * V (TS, 01) + V (TS, 03);
      Month   := V (TS, 05);
      Day     := V (TS, 07);
      Hour    := V (TS, 09);
      Minutes := V (TS, 11);
      Seconds := V (TS, 13);
   end Split_Time_Stamp;

   -------
   -- V --
   -------

   function V (T : Time_Stamp_Type; X : Time_Stamp_Index) return Natural is
   begin
      return 10 * (Character'Pos (T (X))     - Character'Pos ('0')) +
                   Character'Pos (T (X + 1)) - Character'Pos ('0');
   end V;

   ----------------
   -- File_Stamp --
   ----------------

   function File_Stamp (Name : String) return Time_Stamp_Type is
   begin
      if Name = "" then
         return Empty_Time_Stamp;
      end if;

      --  File_Time_Stamp will always return Invalid_Time if the file does
      --  not exist, and OS_Time_To_Time_Stamp will convert this value to
      --  Empty_Time_Stamp. Therefore we do not need to first test whether
      --  the file actually exists, which saves a system call.

      return OS_Time_To_Time_Stamp (File_Time_Stamp (Name));
   end File_Stamp;
   
   ---------------------------
   -- OS_Time_To_Time_Stamp --
   ---------------------------

   function OS_Time_To_Time_Stamp (T : OS_Time) return Time_Stamp_Type is
      Time_Stamp : Time_Stamp_Type;

      Y  : Year_Type;
      Mo : Month_Type;
      D  : Day_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      S  : Second_Type;

   begin
      if T = Invalid_Time then
         return Empty_Time_Stamp;
      end if;

      GM_Split (T, Y, Mo, D, H, Mn, S);
      Make_Time_Stamp
        (Year    => Natural (Y),
         Month   => Natural (Mo),
         Day     => Natural (D),
         Hour    => Natural (H),
         Minutes => Natural (Mn),
         Seconds => Natural (S),
         TS      => Time_Stamp);

      return Time_Stamp;
   end OS_Time_To_Time_Stamp;
   
end Artics.Time_Stamps;
