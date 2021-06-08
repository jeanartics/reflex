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

with Sinfo; use Sinfo;
with Atree; use Atree;
with Stand; use Stand;

with Reflex.Infos;           use Reflex.Infos;
with Reflex.Boxes.Terminals; use Reflex.Boxes.Terminals;

package body Reflex.Boxes.Ch2 is

   ----------------------------
   -- Boxes_Build_Identifier --
   ----------------------------

   procedure Boxes_Build_Identifier
     (This        : access Builder_Record;
      Node        : Node_Id) is

      Open_Contact : Terminal_Box_Ptr;
   begin

      --  If this procedure is call, node's etype is Standard_Boolean and
      --  Nkind of node could not be a N_Op_Or. Then Box_typ is inevitably an
      --  Open_Contact_Box

      Open_Contact := New_Terminal_Box;
      Open_Contact.Set_Node (Node);
      Open_Contact.Set_Typ (Open_Contact_Box);
      Open_Contact.Set_Width (1);
      Open_Contact.Set_Height (1);

      Set_Box (Node, Open_Contact);
   end Boxes_Build_Identifier;

   --------------------------
   -- Boxes_Create_Contact --
   --------------------------

   procedure Boxes_Create_Contact
     (This   : access Builder_Record;
      Node   : Node_Id;
      Negate : Boolean := False) is

      Contact : Terminal_Box_Ptr;
   begin

      --  If this procedure is call, node's etype is Standard_Boolean and
      --  Nkind of node could not be a N_Op_Or. Then Box_typ is inevitably an
      --  Open_Contact_Box

      Contact := New_Terminal_Box;
      Contact.Set_Node (Node);
      if Negate then
         Contact.Set_Typ (Closed_Contact_Box);
      else
         Contact.Set_Typ (Open_Contact_Box);
      end if;
      Contact.Set_Width (1);
      Contact.Set_Height (1);

      Set_Box (Node, Contact);
   end Boxes_Create_Contact;

   ----------------------------
   -- Boxes_Build_Identifier --
   ----------------------------

   procedure Boxes_Create_Coil
     (This   : access Builder_Record;
      Node   : Node_Id;
      Negate : Boolean := False) is

      Coil : Terminal_Box_Ptr;
   begin

      --  If this procedure is call, node's etype is Standard_Boolean and
      --  Nkind of node could not be a N_Op_Or. Then Box_typ is inevitably an
      --  Open_Contact_Box

      Coil := New_Terminal_Box;
      Coil.Set_Node (Node);
      if Negate then
         Coil.Set_Typ (Not_Coil_Box);
      else
         Coil.Set_Typ (Coil_Box);
      end if;
      Coil.Set_Width (1);
      Coil.Set_Height (1);
      Coil.Set_Is_Action_Box (True);

      Set_Box (Node, Coil);
   end Boxes_Create_Coil;

end Reflex.Boxes.Ch2;
