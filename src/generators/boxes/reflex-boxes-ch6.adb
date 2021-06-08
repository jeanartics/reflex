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

with Einfo; use Einfo;
with Atree; use Atree;

with Reflex.Boxes.Terminals; use Reflex.Boxes.Terminals;
with Reflex.Infos;           use Reflex.Infos;

package body Reflex.Boxes.Ch6 is
   
   -----------------------------------------
   -- Boxes_Build_Procedure_Instantiation --
   -----------------------------------------
   
   procedure Boxes_Build_Procedure_Instantiation
     (This : access Builder_Record;
      Node : Node_Id) is 
   begin
      null;
   end Boxes_Build_Procedure_Instantiation;
   
   -----------------------------------------
   -- Boxes_Build_Procedure_Specification --
   -----------------------------------------
   
   procedure Boxes_Build_Procedure_Specification
     (This : access Builder_Record;
      Node : Node_Id) is
   begin
      null;
   end Boxes_Build_Procedure_Specification;
   
   -----------------------------------------
   -- Boxes_Build_Simple_Return_Statement --
   -----------------------------------------
    
   procedure Boxes_Build_Simple_Return_Statement
     (This : access Builder_Record;
      Node : Node_Id) is
      
      --      Ptr  : Terminal_Box_Ptr;
   begin
      --        --  ou on recupere grace aux fils du noeud node?
      --        --  orientation utile si chaque terminal box à un typ?
      --        
      --        Ptr := New_Terminal_Box;
      --        Ptr.Set_Node (Node);
      --        Ptr.Set_Is_Action_Box (True);
      --        Ptr.Set_Orientation (Horizontal);
      --        Ptr.Set_Typ (Return_Box);
      --        Ptr.Set_Height (1);
      --        Ptr.Set_Width (1);
      --        
      --        Set_Box (Node, Ptr);
      null;
   end Boxes_Build_Simple_Return_Statement;
      
   -------------------------------
   -- Boxes_Build_Function_Call --
   -------------------------------
   
   procedure Boxes_Build_Function_Call
     (This : access Builder_Record;
      Node : Node_Id) is 
      
      --        Ptr : Ffb_Box_Ptr;
   begin
      --        if Name (Node) = Sqrt then
      --           Ptr := New_Ffb_Box;
      --           Ptr.Set_Node (Node);
      --           Ptr.Set_Height (3);
      --           Ptr.Set_Width (1);
      --           Ptr.Set_Orientation (Vertical);
      --           Ptr.Set_Is_Action_Box (True);
      --           
      --           Set_Box (Node, Ptr); 
      --        elsif Name (Node) = Cos then
      --           null;
      --        elsif Name (Node) = Sin then 
      --           null;
      --           .........
      --        end if;
      null;
   end Boxes_Build_Function_Call;
   
end  Reflex.Boxes.Ch6;
