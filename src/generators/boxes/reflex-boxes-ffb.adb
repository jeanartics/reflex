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

package body Reflex.Boxes.Ffb is

   -----------------
   -- New_Ffb_Box --
   -----------------

   function New_Ffb_Box return Ffb_Box_Ptr is
      This : Ffb_Box_Ptr :=
        new Ffb_Box_Record'(No_Ffb_Box_Record);
   begin
      Set_Box_Kind(This, Terminal_Box);
      Set_Typ (This, Ffb_Box);
      return This;
   end New_Ffb_Box;

   ------------------
   -- Free_ffb_Box --
   ------------------

   procedure Free_Ffb_Box (This : in out Ffb_Box_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Ffb_Box_Record, Ffb_Box_Ptr);
   begin
      if This /= null then
         Free (This);
      end if;

   end Free_Ffb_Box;

   ---------------
   -- Place_Box --
   ---------------

   procedure Place_Box (This : access Ffb_Box_Record) is
   begin
      null;
   end Place_Box;

   ------------------------
   -- Absolute_Place_Box --
   ------------------------

   procedure Absolute_Place_Box (This : access Ffb_Box_Record) is
   begin
      null;
   end Absolute_Place_Box;

   ------------------
   -- Get_Ffb_Kind --
   ------------------

   function Get_Ffb_Kind (This : access Ffb_Box_Record) return Ffb_Kind is
   begin
      return This.Kind_Ffb;
   end Get_Ffb_Kind;

   ------------------
   -- Set_Ffb_Kind --
   ------------------

   procedure Set_Ffb_Kind
     (This : access Ffb_Box_Record;
      Kind : Ffb_Kind) is
   begin
      This.Kind_Ffb := Kind;
   end Set_Ffb_Kind;

   ------------
   -- Get_En --
   ------------

   function Get_En (This : access Ffb_Box_Record) return Boolean is
   begin
      return This.En;
   end Get_En;

   ------------
   -- Set_En --
   ------------

   procedure Set_En
     (This : access Ffb_Box_Record;
      En : Boolean) is
   begin
      This.En := En;
   end Set_En;

   -------------
   -- Get_Eno --
   -------------

   function Get_Eno (This : access Ffb_Box_Record) return Boolean is
   begin
      return This.Eno;
   end Get_Eno;

   -------------
   -- Set_Eno --
   -------------

   procedure Set_Eno
     (This : access Ffb_Box_Record;
      Eno : Boolean) is
   begin
      This.Eno := Eno;
   end Set_Eno;

   --------------
   -- Get_In_1 --
   --------------

   function Get_In_1 (This : access Ffb_Box_Record) return Node_Id is
   begin
      return This.In_1;
   end Get_In_1;

   --------------
   -- Set_In_1 --
   --------------

   procedure Set_In_1
     (This : access Ffb_Box_Record;
      In_1 : Node_Id) is
   begin
      This.In_1 := In_1;
   end Set_In_1;

   --------------
   -- Get_In_2 --
   --------------

   function Get_In_2 (This : access Ffb_Box_Record) return Node_Id is
   begin
      return This.In_2;
   end Get_In_2;

   --------------
   -- Set_In_2 --
   --------------

   procedure Set_In_2
     (This : access Ffb_Box_Record;
      In_2 : Node_Id) is
   begin
      This.In_2 := In_2;
   end Set_In_2;

   --------------
   -- Get_In_3 --
   --------------

   function Get_In_3 (This : access Ffb_Box_Record) return Node_Id is
   begin
      return This.In_3;
   end Get_In_3;

   --------------
   -- Set_In_3 --
   --------------

   procedure Set_In_3
     (This : access Ffb_Box_Record;
      In_3 : Node_Id)is
   begin
      This.In_3 := In_3;
   end Set_In_3;

   ---------------
   -- Get_Out_1 --
   ---------------

   function Get_Out_1 (This : access Ffb_Box_Record) return Node_Id is
   begin
      return This.Out_1;
   end Get_Out_1;

   ---------------
   -- Set_Out_1 --
   ---------------

   procedure Set_Out_1
     (This : access Ffb_Box_Record;
      Out_1 : Node_Id)is
   begin
      This.Out_1 := Out_1;
   end Set_Out_1;

end Reflex.Boxes.Ffb;
