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

package body Artics.Semaphores is

   protected body Semaphore is
      
      ---------
      -- Get --
      ---------
      
      entry Get when Counter > 0 is
      begin
         Counter := Counter - 1;
      end Get;
      
      -------------
      -- Release --
      -------------
      
      procedure Release is
      begin
         Counter := Counter + 1;
         if Counter > Initial then
            raise Program_Error;
         end if;
      end Release;
      
   end Semaphore;
   
   -------
   -- P --
   -------
   
   procedure P(S : in out Semaphore) is
   begin
      S.Get;
   end P;
   
   -------
   -- V --
   -------
   
   procedure V(S : in out Semaphore) is
   begin
      S.Release;
   end V;

end Artics.Semaphores;
