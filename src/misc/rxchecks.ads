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

--  Package containing routines used to deal with runtime checks. These
--  routines are used both by the semantics and by the expander. In some
--  cases, checks are enabled simply by setting flags for gigi, and in
--  other cases the code for the check is expanded.

--  The approach used for range and length checks, in regards to suppressed
--  checks, is to attempt to detect at compilation time that a constraint
--  error will occur. If this is detected a warning or error is issued and the
--  offending expression or statement replaced with a constraint error node.
--  This always occurs whether checks are suppressed or not.  Dynamic range
--  checks are, of course, not inserted if checks are suppressed.

with Types; use Types;
with Uintp; use Uintp;

package RxChecks is

   procedure Determine_Range
     (N  : Node_Id;
      OK : out Boolean;
      Lo : out Uint;
      Hi : out Uint);
   --  N is a node for a subexpression. If N is of a discrete type with
   --  no error indications, and no other peculiarities (e.g. missing
   --  type fields), then OK is True on return, and Lo and Hi are set
   --  to a conservative estimate of the possible range of values of N.
   --  Thus if OK is True on return, the value of the subexpression N is
   --  known to like in the range Lo .. Hi (inclusive). If the expression
   --  is not of a discrete type, or some kind of error condition is
   --  detected, then OK is False on exit, and Lo/Hi are set to No_Uint.
   --  Thus the significance of OK being False on return is that no
   --  useful information is available on the range of the expression.

end RxChecks;
