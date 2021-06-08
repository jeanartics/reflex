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

package body Artics.Iterators is

   --  Iteration support

   -----------
   -- Visit --
   -----------

   procedure Visit (It : in out Iterator'Class) is
   begin
      Reset (It);
      while not Is_End (It) loop
         Apply (Current_Item (It));
         Next (It);
      end loop;
   end Visit;

   --  Primitive implementations

   ----------
   -- Lock --
   ----------

   procedure Lock (C : in out The_Container) is
      pragma Warnings (Off, C);
   begin
      null;
   end Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (C : in out The_Container) is
      pragma Warnings (Off, C);
   begin
      null;
   end Unlock;

end Artics.Iterators;
