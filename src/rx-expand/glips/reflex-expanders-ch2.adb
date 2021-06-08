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

with Types; use Types;
with Atree; use Atree;
with Stand; use Stand;
with Sinfo; use Sinfo;
with Einfo; use Einfo;
with Sem_Util; use Sem_Util;
with Sem_Aux; use Sem_Aux;
with Uintp; use Uintp;
with Urealp; use Urealp;

with system; use system;
with Ada.Unchecked_Conversion;

package body Reflex.Expanders.Ch2 is
   
   ---------------------------------------
   -- Expand_Defining_Character_Literal --
   ---------------------------------------
   
   procedure Expand_Defining_Character_Literal
     (This : access Reflex_Expander_Record;
      Node : Node_id) is
   begin
      null;
   end Expand_Defining_Character_Literal;
   
   --------------------------------
   -- Expand_Defining_Identifier --
   --------------------------------
   
   procedure Expand_Defining_Identifier 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Defining_Identifier;
   
   -------------------------------------
   -- Expand_Defining_Operator_Symbol --
   -------------------------------------
   
   procedure Expand_Defining_Operator_Symbol
     (This : access Reflex_Expander_Record;
      Node : Node_id) is
   begin
      null;
   end Expand_Defining_Operator_Symbol;
   
   --------------------------
   -- Expand_Expanded_Name --
   --------------------------
   
   procedure Expand_Expanded_Name
     (This : access Reflex_Expander_Record;
      Node : Node_id) is
   begin
      null;
   end Expand_Expanded_Name;
   
   -----------------------
   -- Expand_Identifier --
   -----------------------
   
   procedure Expand_Identifier
     (This : access Reflex_Expander_Record;
      Node : Node_id) is
   begin
      null;
   end Expand_Identifier;
   
   -------------------------------
   -- Expand_Selected_Component --
   -------------------------------
   
   procedure Expand_Selected_Component
     (This : access Reflex_Expander_Record;
      Node : Node_id) is
   begin
      null;
   end Expand_Selected_Component;
   
   ----------------------
   -- Expand_Type_Name --
   ----------------------

   procedure Expand_Type_Name
     (This : access Reflex_Expander_Record;
      Typ  : Entity_Id) is
   begin
      null;
   end Expand_Type_Name;
   
end Reflex.Expanders.Ch2;
