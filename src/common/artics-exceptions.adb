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
-- Reflex is originally developed by Artics
------------------------------------------------------------------------------

with Ada.Text_Io; use Ada.Text_Io;
package body Artics.Exceptions is
   
   -----------------------
   -- Exception_Message --
   -----------------------
   
   function Exception_Message
     (E   : Ada.Exceptions.Exception_Occurrence;
      Msg : String) return String is
   begin
      return 
	"Exception: " & Exception_Name (E) & ":" & Exception_Message (E) &
	" : " & Msg;
   end Exception_Message;
   
   -------------------
   -- Log_Exception --
   -------------------
   
   procedure Log_Exception (S : String) is
   begin
      Put_Line (S);
   end Log_Exception;
     
end Artics.Exceptions;
