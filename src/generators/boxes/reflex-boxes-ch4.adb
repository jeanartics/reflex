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
 
with Sinfo;  use Sinfo;
with Atree;  use Atree;

with Reflex.Infos;           use Reflex.Infos;
with Reflex.Boxes.Duals;     use Reflex.Boxes.Duals;
with Reflex.Boxes.Terminals; use Reflex.Boxes.Terminals;
with Reflex.Boxes.Dispatch;  use Reflex.Boxes.Dispatch;

package body Reflex.Boxes.Ch4 is

   ------------------------
   -- Boxes_Build_Op_And --
   ------------------------
   
   procedure Boxes_Build_Op_And
     (This : access Builder_Record;
      Node : Node_Id) is
      
      Dual_Box : Dual_Box_Ptr;
      Lhs_Node : Node_Id := Left_Opnd (Node);
      Rhs_Node : Node_Id := Right_Opnd (Node);
   begin
      Dual_Box := New_Dual_Box;
      Dual_Box.Set_Node (Node);
      
      Boxes_Node_Dispatch (This, Lhs_Node);
      Dual_Box.Set_Box1 (Get_Box (Lhs_Node));
      Get_Box (Lhs_Node).Set_Parent_Box (Dual_Box);
      
      Boxes_Node_Dispatch (This, Rhs_Node);
      Dual_Box.Set_Box2 (Get_Box (Rhs_Node)); 
      Get_Box (Rhs_Node).Set_Parent_Box (Dual_Box);
     
      Set_Box (Node, Dual_Box);
   end Boxes_Build_Op_And;
   
   ------------------------
   -- Boxes_Build_Op_Not --
   ------------------------
    
   procedure Boxes_Build_Op_Not
     (This : access Builder_Record;
      Node : Node_Id) is
    
      Closed_Contact : Terminal_Box_Ptr;
   begin
      
      --  We does not dispatch other node(s) because a N_Op_Not means that the
      --  current box is a Terminal_Box and his typ is Closed_Contact_Box.

      Closed_Contact := New_Terminal_Box;
      Closed_Contact.Set_Node (Node);  
      Closed_Contact.Set_Height (1);
      Closed_Contact.Set_Width (1);
      Closed_Contact.Set_Typ (Closed_Contact_Box);
      
      Set_Box (Node, Closed_Contact);
   end Boxes_Build_Op_Not;
   
   -----------------------
   -- Boxes_Build_Op_Or --
   -----------------------
    
   procedure Boxes_Build_Op_Or
     (This : access Builder_Record;
      Node : Node_Id) is
       
      Dual_Box : Dual_Box_Ptr;
      Lhs_Node : Node_Id := Left_Opnd (Node);
      Rhs_Node : Node_Id := Right_Opnd (Node);
   begin
      Dual_Box := New_Dual_Box;
      Dual_Box.Set_Node (Node);  
      Dual_Box.Set_Orientation (Vertical);

      Boxes_Node_Dispatch (This, Lhs_Node);
      Dual_Box.Set_Box1 (Get_Box (Lhs_Node));
      Get_Box (Lhs_Node).Set_Parent_Box (Dual_Box);

      Boxes_Node_Dispatch (This, Rhs_Node);
      Dual_Box.Set_Box2 (Get_Box (Rhs_Node)); 
      Get_Box (Rhs_Node).Set_Parent_Box (Dual_Box);
           
      Set_Box (Node, Dual_Box);
   end Boxes_Build_Op_Or;
         
   -----------------------
   -- Boxes_Build_Op_Eq --
   -----------------------
   
   procedure Boxes_Build_Op_Eq 
     (This : access Builder_Record;
      Node : Node_Id) is

      Compare_Block  : Terminal_Box_Ptr;
   begin
      
      --  We does not dispatch other node(s) because a N_Op_Eq means that the
      --  current box is a Terminal_Box and his typ is Compare_Block_Box.
      
      Compare_Block := New_Terminal_Box;
      Compare_Block.Set_Node (Node);
      Compare_Block.Set_Orientation (Horizontal);
      Compare_Block.Set_Typ (Compare_Block_Box);
      Compare_Block.Set_Height (1);
      Compare_Block.Set_Width (2);

      Set_Box (Node, Compare_Block);
      
   end Boxes_Build_Op_Eq;
  
   -----------------------
   -- Boxes_Build_Op_Ne --
   -----------------------
   
   procedure Boxes_Build_Op_Ne
     (This : access Builder_Record;
      Node : Node_Id) is
   
      Compare_Block  : Terminal_Box_Ptr;
   begin
      --  We does not dispatch other node(s) because a N_Op_Ne means that the
      --  current box is a Terminal_Box and his typ is Compare_Block_Box.
       
      Compare_Block := New_Terminal_Box;
      Compare_Block.Set_Node (Node);
      Compare_Block.Set_Orientation (Horizontal);
      Compare_Block.Set_Typ (Compare_Block_Box);
      Compare_Block.Set_Height (1);
      Compare_Block.Set_Width (2);

      Set_Box (Node, Compare_Block);
            
   end Boxes_Build_Op_Ne;
   
   -----------------------
   -- Boxes_Build_Op_Ge --
   -----------------------
   
   procedure Boxes_Build_Op_Ge 
     (This : access Builder_Record;
      Node : Node_Id) is

      Compare_Block  : Terminal_Box_Ptr;
   begin
      
      --  We does not dispatch other node(s) because a N_Op_Ge means that the
      --  current box is a Terminal_Box and his typ is Compare_Block_Box.
       
      Compare_Block := New_Terminal_Box;
      Compare_Block.Set_Node (Node);
      Compare_Block.Set_Orientation (Horizontal);
      Compare_Block.Set_Typ (Compare_Block_Box);
      Compare_Block.Set_Height (1);
      Compare_Block.Set_Width (2);

      Set_Box (Node, Compare_Block);
            
   end Boxes_Build_Op_Ge;
   
   -----------------------
   -- Boxes_Build_Op_Gt --
   -----------------------
            
   procedure Boxes_Build_Op_Gt 
     (This : access Builder_Record;
      Node : Node_Id) is
      
      Compare_Block  : Terminal_Box_Ptr;
   begin
      
      --  We does not dispatch other node(s) because a N_Op_Gt means that the
      --  current box is a Terminal_Box and his typ is Compare_Block_Box.
       
      Compare_Block := New_Terminal_Box;
      Compare_Block.Set_Node (Node);
      Compare_Block.Set_Orientation (Horizontal);
      Compare_Block.Set_Typ (Compare_Block_Box);
      Compare_Block.Set_Height (1);
      Compare_Block.Set_Width (2);

      Set_Box (Node, Compare_Block);
      
   end Boxes_Build_Op_Gt;
         
   -----------------------
   -- Boxes_Build_Op_Le --
   -----------------------
   
   procedure Boxes_Build_Op_Le 
     (This : access Builder_Record;
      Node : Node_Id) is

      Compare_Block  : Terminal_Box_Ptr;
   begin
      
      --  We does not dispatch other node(s) because a N_Op_Le means that the
      --  current box is a Terminal_Box and his typ is Compare_Block_Box.
       
      Compare_Block := New_Terminal_Box;
      Compare_Block.Set_Node (Node);
      Compare_Block.Set_Orientation (Horizontal);
      Compare_Block.Set_Typ (Compare_Block_Box);
      Compare_Block.Set_Height (1);
      Compare_Block.Set_Width (2);
      
      Set_Box (Node, Compare_Block);

   end Boxes_Build_Op_Le;
   
   -----------------------
   -- Boxes_Build_Op_Lt --
   -----------------------

   procedure Boxes_Build_Op_Lt 
     (This : access Builder_Record;
      Node : Node_Id) is
      
      Compare_Block  : Terminal_Box_Ptr;
   begin
      
      --  We does not dispatch other node(s) because a N_Op_Lt means that the
      --  current box is a Terminal_Box and his typ is Compare_Block_Box.
       
      Compare_Block := New_Terminal_Box;
      Compare_Block.Set_Node (Node);
      Compare_Block.Set_Orientation (Horizontal);
      Compare_Block.Set_Typ (Compare_Block_Box);
      Compare_Block.Set_Height (1);
      Compare_Block.Set_Width (2);
      
      Set_Box (Node, Compare_Block);

   end Boxes_Build_Op_Lt;
   
   --------------------------------
   -- Boxes_Build_Op_Shift_Right --
   --------------------------------
   
   procedure Boxes_Build_Op_Shift_Right
     (This : access Builder_Record;
      Node : Node_Id;
      Name : Node_Id) is
     
      --  Ffb_Box : Ffb_Box_Ptr;
   begin
      null;
   end Boxes_Build_Op_Shift_Right;
            
   -------------------------------
   -- Boxes_Build_Op_Shift_Left --
   -------------------------------
   
   procedure Boxes_Build_Op_Shift_Left
     (This : access Builder_Record;
      Node : Node_Id;
      Name : Node_Id) is
     
      --  Ffb_Box : Ffb_Box_Ptr;
   begin
      null;
   end Boxes_Build_Op_Shift_Left;
   
   -------------------------
   -- Boxes_Build_Op_Plus --
   -------------------------
      
   procedure Boxes_Build_Op_Plus
     (This : access Builder_Record;
      Node : Node_Id) is
   begin
      null;
   end Boxes_Build_Op_Plus;
   
   --------------------------
   -- Boxes_Build_Op_Minus --
   --------------------------
   
   procedure Boxes_Build_Op_Minus 
     (This : access Builder_Record;
      Node : Node_Id) is
   begin
      null;
   end Boxes_Build_Op_Minus;
   
   ------------------------
   -- Boxes_Build_Op_Abs --
   ------------------------
   
   procedure Boxes_Build_Op_Abs
     (This : access Builder_Record;
      Node : Node_Id;
      Name : Node_Id) is
      
      --  Ffb_Box : Ffb_Box_Ptr;
   begin
      --        
      --        --  We does not dispatch other node(s) because a N_Op_Abs means 
      --        --  that The current box is a Terminal_Box and his typ is Ffb_Box.
      --         
      --        Ffb_Box := New_Ffb_Box;
      --        Ffb_Box.Set_Node (Node);
      --        Ffb_Box.Set_Orientation (Vertical);
      --        Ffb_Box.Set_Height (3);
      --        Ffb_Box.Set_Width (1);
      --        Ffb_Box.Set_Is_Action_Box (True);
      --        Ffb_Box.Set_Ffb_Kind (Abs_Ffb);
      --        
      --        Ffb_Box.Set_In_1 (Right_Opnd (Node));
      --        Ffb_Box.Set_Out_1 (Name);
      --                
      --        Set_Box (Node, Ffb_Box);
      null;
   end Boxes_Build_Op_Abs;
      
   ------------------------
   -- Boxes_Build_Op_Mod --
   ------------------------
   
   procedure Boxes_Build_Op_Mod 
     (This : access Builder_Record;
      Node : Node_Id;
      Name : Node_Id) is 
      
      --  Ffb_Box : Ffb_Box_Ptr;
   begin
      --        
      --        --  We does not dispatch other node(s) because a N_Op_Mod means 
      --        --  that The current box is a Terminal_Box and his typ is Ffb_Box.
      --        
      --        Ffb_Box := New_Ffb_Box;
      --        Ffb_Box.Set_Node (Node);
      --        Ffb_Box.Set_Orientation (Vertical);
      --        Ffb_Box.Set_Height (3);
      --        Ffb_Box.Set_Width (1);
      --        Ffb_Box.Set_Is_Action_Box (True);
      --        Ffb_Box.Set_Ffb_Kind (Mod_Ffb);
      --        
      --        Ffb_Box.Set_In_1 (Left_Opnd (Node));
      --        Ffb_Box.Set_In_2 (Right_Opnd (Node));
      --        Ffb_Box.Set_Out_1 (Name);
      --        
      --        Set_Box (Node, Ffb_Box);
      null;
   end Boxes_Build_Op_Mod;
   
   --------------------------
   -- Boxes_Build_Op_Expon --
   --------------------------
   
   procedure Boxes_Build_Op_Expon 
     (This : access Builder_Record;
      Node : Node_Id;
      Name : Node_Id) is
      
      --  Ffb_Box : Ffb_Box_Ptr;
   begin
      --        
      --        --  We does not dispatch other node(s) because a N_Op_Expon means 
      --        --  that The current box is a Terminal_Box and his typ is Ffb_Box.
      --        
      --        Ffb_Box := New_Ffb_Box;
      --        Ffb_Box.Set_Node (Node);
      --        Ffb_Box.Set_Orientation (Vertical);
      --        Ffb_Box.Set_Height (3);
      --        Ffb_Box.Set_Width (1);
      --        Ffb_Box.Set_Is_Action_Box (True);
      --        Ffb_Box.Set_Ffb_Kind (Exp_Ffb);
      --        --        Ptr.Set_In_1 (Left_Opnd (Node));
      --        --        Ptr.Set_In_2 (Right_Opnd (Node));
      --        Ffb_Box.Set_Out_1 (Name);
      --        
      --        Set_Box (Node, Ffb_Box);
      null;
   end Boxes_Build_Op_Expon;
   
   ------------------------
   -- Boxes_Build_Op_Rem --
   ------------------------
    
   procedure Boxes_Build_Op_Rem 
     (This : access Builder_Record;
      Node : Node_Id) is
   begin
      null;
   end Boxes_Build_Op_Rem;
        
end Reflex.Boxes.Ch4;
