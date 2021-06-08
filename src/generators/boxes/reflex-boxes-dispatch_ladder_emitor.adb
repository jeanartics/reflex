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

with Reflex.Boxes.Terminals;     use Reflex.Boxes.Terminals;

package body Reflex.Boxes.Dispatch_Ladder_Emitor is

   procedure Emitor_Dispatch
     (This   : access Ladder_Emitor_Record;
      Box    : access Box_Record'Class;
      Repeat : Natural := 0) is
   begin
      if Box = null then
         This.Emit_Empty (Repeat);
         
      elsif Box = The_Vlink_Box then
         This.Emit_Simple_Vlink;
         
      elsif Box = The_Hlink_Box then
         This.Emit_Hlink (Repeat);
	 
      elsif Box = The_Hlink_Vlink_Box then
         This.Emit_Hlink (Repeat, True);
	 
      elsif Box.Get_Box_Kind = Terminal_Box then
         --  Box.Dump_Box;
	 
         case Get_Typ (Terminal_Box_Ptr (Box)) is

         when Open_Contact_Box =>
            Emit_Open_Contact (This, Box);

         when Closed_Contact_Box =>
            Emit_Closed_Contact (This, Box);

         when Pos_Trans_Contact_Box =>
            null;

         when Neg_Trans_Contact_Box =>
            null;

         when Coil_Box =>
            Emit_Coil (This, Box);

         when Not_Coil_Box =>
            Emit_Not_Coil (This, Box);

         when Set_Coil_Box =>
            Emit_Set_Coil (This, Box);

         when Reset_Coil_Box =>
            Emit_Reset_Coil (This, Box);

         when Pos_Trans_Coil_Box =>
            null;

         when Neg_Trans_Coil_Box =>
            null;

         when Halt_Coil_Box =>
            null;

         when  Call_Coil_Box =>
            null;

         when  Compare_Block_Box =>
            Emit_Compare_Block (This, Box);

         when  Operate_Block_Box =>
            Emit_Operate_Block (This, Box);

         when Jump_Box =>
            Emit_Jump (This, Box);

         when Label_Box =>
            Emit_Label (This, Box, Repeat);
	    
         when Return_Box =>
            Emit_Return_Coil (This, Box);

         when Ffb_Box =>
            null;

         when others =>
            null;
            
         end case;
      end if;
   end Emitor_Dispatch;
   
end Reflex.Boxes.Dispatch_Ladder_Emitor;
