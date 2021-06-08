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

with Ada.Text_Io; use Ada.Text_IO;

with Atree;    use Atree;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Namet;    use Namet;
with Nlists; use Nlists;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Ttypes;   use Ttypes;
with Types;    use Types;
with Uintp;    use Uintp;
with Urealp;   use Urealp;
with Gnat.HTable; use Gnat.HTable;

with Reflex.Expanders.Utils; use Reflex.Expanders.Utils;
with Reflex.Expanders.Dispatch; use Reflex.Expanders.Dispatch;

package body Reflex.Expanders.Supports is

   ------------------------
   -- Expand_Left_Opnd --
   ------------------------

   procedure Expand_Left_Opnd
     (This : access Reflex_Expander_Record;
      N    : Node_Id) is
      
      Opnd : constant Node_Id := Left_Opnd (N);
   begin
      Expand_Node (This, Opnd);
   end Expand_Left_Opnd;

   ------------------------
   -- Expand_Node_List --
   ------------------------

   procedure Expand_Node_List
     (This      : access Reflex_Expander_Record;
      List      : List_Id; 
      New_Lines : Boolean := False) is
      Node      : Node_Id;
   begin
      if Is_Non_Empty_List (List) then
         Node := Nlists.First (List);

         loop
            Expand_Node (This, Node);
            Next (Node);
            exit when Node = Empty;
         end loop;
      end if;
   end Expand_Node_List;

   -----------------------
   -- Expand_Opt_Node --
   -----------------------

   procedure Expand_Opt_Node
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      if Present (Node) then
         Expand_Node (This, Node);
      end if;
   end Expand_Opt_Node;

   ----------------------------
   -- Expand_Opt_Node_List --
   ----------------------------

   procedure Expand_Opt_Node_List
     (This : access Reflex_Expander_Record;
      List : List_Id) is
   begin
      if Present (List) then
         Expand_Node_List (This, List);
      end if;
   end Expand_Opt_Node_List;

   -------------------------
   -- Expand_Right_Opnd --
   -------------------------

   procedure Expand_Right_Opnd
     (This : access Reflex_Expander_Record;
      N    : Node_Id) is
      
      Opnd : constant Node_Id := Right_Opnd (N);
   begin
      Expand_Node (This, Opnd);
   end Expand_Right_Opnd;

end Reflex.Expanders.Supports;
