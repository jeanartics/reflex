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

package Reflex.Boxes.Terminals is
   
   type Box_Type is
     (No_Box_Type,
      
      --  Contact
      Open_Contact_Box,
      Closed_Contact_Box,
      Pos_Trans_Contact_Box,
      Neg_Trans_Contact_Box,
      
      --  Coil
      Coil_Box,
      Not_Coil_Box,
      Set_Coil_Box,
      Reset_Coil_Box,
      Pos_Trans_Coil_Box,
      Neg_Trans_Coil_Box,
      Halt_Coil_Box,
      Call_Coil_Box,
      
      --  Object
      Compare_Block_Box,
      Operate_Block_Box,
      
      --  Control
      Jump_Box,
      Label_Box,
      Return_Box,
     
      --  Ffb
      Ffb_Box);
   
   subtype Contacts_Box is
     Box_Typ range Open_Contact_Box..Neg_Trans_Contact_Box;
   
   subtype Coils_Box is
     Box_Typ range Coil_Box..Call_Coil_Box;
   
   subtype Objects_Box is
     Box_Typ range Compare_Block_Box..Operate_Block_Box;

   type Terminal_Box_Record is new Box_Record with private;
   type Terminal_Box_Ptr is access all Terminal_Box_Record;
   type Terminal_Box_Class_Ptr is access all Terminal_Box_Record'Class;
   
   No_Terminal_Box_Record : constant Terminal_Box_Record;
   
   function New_Terminal_Box return Terminal_Box_Ptr;
   
   procedure Free_Terminal_Box (This : in out Terminal_Box_Ptr);
      
   function Get_Type (This : access Terminal_Box_Record) return Box_Type;
   procedure Set_Type
     (This : access Terminal_Box_Record;
      Typ  : Box_Type);
   
   procedure Absolute_Place_Box (This : access Terminal_Box_Record);

   procedure Place_Matrix
     (This   : access Terminal_Box_Record;
      Matrix : access Matrices.Matrix_Record);
   
   procedure Dump_Box (This : access Terminal_Box_Record);

private
   
   type Terminal_Box_Record is new Box_Record with record
      Typ  : Box_Type;
   end record;
   
   No_Terminal_Box_Record : constant Terminal_Box_Record :=
     (No_Box_Record with
      Typ  => No_Box_Type);
   
end Reflex.Boxes.Terminals;
