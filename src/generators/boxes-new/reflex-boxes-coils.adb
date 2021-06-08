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

with Atree; use Atree;
with Sinfo; use Sinfo;

package body Reflex.Boxes.Coils is
   
   ------------------
   -- New_Coil_Box --
   ------------------
   
   function New_Coil_Box return Coil_Box_Ptr is
      This : Coil_Box_Ptr := 
        new Coil_Box_Record'(No_Coil_Box_Record);
   begin
      -- Set_Box_Kind (This, Coil_Box);
      return This;
   end New_Coil_Box;
   
   -------------------
   -- Free_Coil_Box --
   -------------------
   
   procedure Free_Coil_Box (This : in out Coil_Box_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Coil_Box_Record, Coil_Box_Ptr);
   begin
      if This /= null then
         Free (This);
      end if;
   end Free_Coil_Box;
   
   ---------------
   -- Place_Box --
   ---------------
   
   procedure Place_Box (This : access Coil_Box_Record) is
      
      Node : Node_Id := This.Get_Node;
   begin
      --        This.Set_X (0);
      --        This.Set_Y (0);
      --        
      This.Set_Width (0);
      This.Set_Height (1);
      
      case Nkind (Node) is
      when N_Op_And =>
         null;
      when others =>
         null;
      end case;
   end Place_Box;
   
   ------------------------
   -- Absolute_Place_Box --
   ------------------------
   
   procedure Absolute_Place_Box (This : access Coil_Box_Record) is
   begin
--        This.Set_X (0);
--        This.Set_Y (0);
      null;
   end Absolute_Place_Box;
   
end Reflex.Boxes.Coils;
