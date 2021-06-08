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

with Ada.Text_IO; use Ada.Text_IO;

with AUnit.Assertions; use AUnit.Assertions;
with Atree; use Atree;
with Sinfo; use Sinfo;

package body Reflex.Boxes.Tests is

   procedure Set_Up (T : in out Test_Case) is
   begin
      --  Do any necessary set ups.  If there are none,
      --  omit from both spec and body, as a default
      --  version is provided in Test_Cases.
      null;
   end Set_Up;


   procedure Tear_Down (T : in out Test_Case) is
   begin
      --  Do any necessary cleanups, so the next test
      --  has a clean environment.  If there is no
      --  cleanup, omit spec and body, as default is
      --  provided in Test_Cases.
      null;
   end Tear_Down;

   ---------------------
   -- Test_Initialize --
   ---------------------

   procedure Test_Initialize
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      Assert (True, "Initialize : testing Bad ");
   end Test_Initialize;


   -------------
   -- New_Box --
   -------------
   
   procedure Test_New_Box
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : access Box_Record := New_Box;
   begin
      Assert
        (B.all = No_Box_Record,
         "Test_New_Box: B.all /= No_Box_Record");
      
      Assert (True, "Test_New_Box : testing Bad ");
   end Test_New_Box;
   
   --------------
   -- Free_Box --
   --------------
   
   procedure Test_Free_Box
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      Assert (True, "Test_Free_Box : testing Bad ");
   end Test_Free_Box;
   
   --------------
   -- Get_Node --
   --------------
   
   procedure Test_Get_Node 
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : access Box_Record := New_Box;
      N : Node_Id;
   begin
      N := Get_Node (B);
      
      Assert 
        (N = B.Node, 
         "Test_Get_Node: B.Node /= B.Get_Node");
      Assert (True, "Test_Get_Node : testing Bad ");
   end Test_Get_Node;
   
   --------------
   -- Set_Node --
   --------------
   
   procedure Test_Set_Node
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
   
      B : access Box_Record := New_Box;
      N : Node_Id := Node_Id (5);
   begin
      B.Set_Node (N);
      
      Assert 
        (N = B.Node, 
         "Test_Set_Node: B.Node /= B.Get_Node");
      Assert (True, "Test_Set_Node : testing Bad ");
   end Test_Set_Node;
   
   ------------------
   -- Get_Box_Kind --
   ------------------
   
   procedure Test_Get_Box_Kind
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : access Box_Record := New_Box;
      K : Box_Kind;
   begin
      K := Get_Box_Kind (B);
      
      Assert 
        (K = B.Kind, 
         "Test_Get_Box_Kind: B.kind /= K.Get_Box_Kind");
      Assert (True, "Test_Get_Box_Kind : testing Bad ");
   end Test_Get_Box_Kind;
   
   ------------------
   -- Set_Box_Kind --
   ------------------
   
   procedure Test_Set_Box_Kind
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : access Box_Record := New_Box;
      K : Box_Kind := Dual_Box;
   begin
      B.Set_Box_Kind (K);
      
      Assert 
        (K = B.Kind, 
         "Test_Set_Box_Kind: B.Kind /= K");
      Assert (True, "Test_Set_Box_Kind : testing Bad ");
   end Test_Set_Box_Kind;
   
   --------------------
   -- Get_Parent_Box --
   --------------------
   
   procedure Test_Get_Parent_Box 
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : access Box_Record := New_Box;
      P : access Box_Record;
   begin
      P := Get_Parent_Box (B);
      
      Assert 
        (P = B.Parent, 
         "Test_Get_Parent_Box: B.Parent /= P");   
      Assert (True, "Test_Get_Parent_Box : testing Bad ");
   end Test_Get_Parent_Box;
   
   --------------------
   -- Set_Parent_Box --
   --------------------
   
   procedure Test_Set_Parent_Box 
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : access Box_Record := New_Box;
      P : access Box_Record := New_Box;
   begin
      B.Set_Parent_Box (P);
      
      Assert 
        (P = B.Parent, 
         "Test_Set_Parent_Box: B.Parent /= P");
      Assert (True, "Test_Set_Parent_Box : testing Bad ");
   end Test_Set_Parent_Box;
   
   ---------------------
   -- Get_Orientation --
   ---------------------
   
   procedure Test_Get_Orientation
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : access Box_Record := New_Box;
      O : Orientation_Type;
   begin
      O := Get_Orientation (B);
      
      Assert 
        (O = B.Orientation, 
         "Test_Get_Orientation: O /= B.Orientation");   
      Assert (True, "Test_Get_Orientation : testing Bad ");
   end Test_Get_Orientation;
   
   ---------------------
   -- Set_Orientation --
   ---------------------
   
   procedure Test_Set_Orientation
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : access Box_Record := New_Box;
      O : Orientation_Type := Horizontal;
   begin
      B.Set_Orientation (O);
      
      Assert 
        (O = B.Get_Orientation, 
         "Test_Set_Orientation: O /= B.Get_Orientation");
      Assert (True, "Test_Set_Orientation : testing Bad ");
   end Test_Set_Orientation;
   
   -----------------------
   -- Get_Is_Action_Box --
   -----------------------
   
   procedure Test_Get_Is_Action_Box 
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : access Box_Record := New_Box;
      A : Boolean;
   begin
      A := Get_Is_Action_Box (B);
      
      Assert 
        (A = B.Is_Action_Box, 
         "Test_Get_Is_Action_Box: A /= B.Is_Action_Box");
      Assert (True, "Test_Get_Is_Action_Box : testing Bad ");
   end Test_Get_Is_Action_Box;
   
   -----------------------
   -- Set_Is_Action_Box --
   -----------------------
   
   procedure Test_Set_Is_Action_Box
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : access Box_Record := New_Box;
      A : Boolean := True;
   begin
      B.Set_Is_Action_Box (A);
      
      Assert 
        (A = B.Is_Action_Box, 
         "Test_Set_Is_Action_Box: A /= B.Get_Is_Action_Box");
      Assert (True, "Test_Set_Is_Action_Box : testing Bad ");
   end Test_Set_Is_Action_Box;
   
   -----------
   -- Get_X --
   -----------
   
   procedure Test_Get_X
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : access Box_Record := New_Box;
      X : Natural;
   begin
      X := B.Get_X;
      
      Assert 
        (X = B.X, 
         "Test_Get_X: X /= B.X");     
      Assert (True, "Test_Get_X : testing Bad ");
   end Test_Get_X;
   
   -----------
   -- Set_X --
   -----------
   
   procedure Test_Set_X
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
   
      B : access Box_Record := New_Box;
      X : Natural := 8;
   begin
      B.Set_X (X);
      
      Assert 
        (X = B.X, 
         "Test_Set_X: X /= B.X"); 
      
      X := 0;
      B.Set_X (X);
      
      Assert 
        (X = B.X, 
         "Test_Set_X: X /= B.X"); 
      Assert (True, "Test_Set_X : testing Bad ");
   end Test_Set_X;
   
   -----------
   -- Get_Y --
   -----------
   
   procedure Test_Get_Y
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : access Box_Record := New_Box;
      Y : Natural;
   begin
      Y := B.Get_Y;
      
      Assert 
        (Y = B.Y, 
         "Test_Get_Y: Y /= B.Y");     
      Assert (True, "Test_Get_Y : testing Bad ");
   end Test_Get_Y;
   
   -----------
   -- Set_Y --
   -----------
   
   procedure Test_Set_Y
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
   
      B : access Box_Record := New_Box;
      Y : Natural := 8;
   begin
      B.Set_Y (Y);
      
      Assert 
        (Y = B.Y, 
         "Test_Set_Y: Y /= B.Y"); 
      Y := 0;
      B.Set_Y (Y);
      
      Assert 
        (Y = B.Y, 
         "Test_Set_Y: Y /= B.Y"); 
      Assert (True, "Test_Set_Y : testing Bad ");
   end Test_Set_Y;
   
   --------------
   -- Get_Xabs --
   --------------
   
   procedure Test_Get_Xabs
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : access Box_Record := New_Box;
      Xabs : Natural;
   begin
      Xabs := B.Get_Xabs;
      
      Assert 
        (Xabs = B.Xabs, 
         "Test_Get_X: Xabs /= B.Xabs");     
      Assert (True, "Test_Get_Xabs : testing Bad ");
   end Test_Get_Xabs;
   
   --------------
   -- Set_Xabs --
   --------------
   
   procedure Test_Set_Xabs
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : access Box_Record := New_Box;
      Xabs : Natural := 8;
   begin
      B.Set_Xabs (Xabs);
      
      Assert 
        (Xabs = B.Xabs, 
         "Test_Set_Xabs: Xabs /= B.Xabs"); 
      
      Xabs := 0;
       B.Set_Xabs (Xabs);
      
      Assert 
        (Xabs = B.Xabs, 
         "Test_Set_Xabs: Xabs /= B.Xabs"); 
      Assert (True, "Test_Set_Xabs : testing Bad ");
   end Test_Set_Xabs;
   
   --------------
   -- Get_Yabs --
   --------------
   
   procedure Test_Get_Yabs
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : access Box_Record := New_Box;
      Yabs : Natural;
   begin
      Yabs := B.Get_Yabs;
      
      Assert 
        (Yabs = B.Yabs,        
         "Test_Get_Yabs: Yabs /= B.Yabs"); 
      Assert (True, "Test_Get_Yabs : testing Bad ");
   end Test_Get_Yabs;
   
   --------------
   -- Set_Yabs --
   --------------
   
   procedure Test_Set_Yabs
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : access Box_Record := New_Box;
      Yabs : Natural := 8;
   begin
      B.Set_Yabs (Yabs);
      
      Assert 
        (Yabs = B.Yabs, 
         "Test_Set_Yabs: Yabs /= B.Yabs");
      Yabs := 0;
      B.Set_Yabs (Yabs);
      
      Assert 
        (Yabs = B.Yabs, 
         "Test_Set_Yabs: Yabs /= B.Yabs"); 
      Assert (True, "Test_Set_Yabs : testing Bad ");
   end Test_Set_Yabs;
   
   ----------------
   -- Get_Height --
   ----------------
   
   procedure Test_Get_Height
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B      : access Box_Record := New_Box;
      H : Natural;
   begin
      H := B.Get_Height;
      
      Assert 
        (H = B.H,        
         "Test_Get_Height: H /= B.Get_Height");   
      Assert (True, "Test_Get_Height : testing Bad ");
   end Test_Get_Height;
   
   ----------------
   -- Set_Height --
   ----------------
   
   procedure Test_Set_Height
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : access Box_Record := New_Box;
      H : Natural := 8;
   begin
      B.Set_Height (H);
      
      Assert 
        (H = B.H, 
         "Test_Set_Height: H /= B.Get_Height"); 
      
      H := 0;
      B.Set_Height (H);
      
      Assert 
        (H = B.H, 
         "Test_Set_Height: H /= B.Get_Height"); 
      
      Assert (True, "Test_Set_Height : testing Bad ");
   end Test_Set_Height;
   
   ---------------
   -- Get_Width --
   ---------------
   
   procedure Test_Get_Width
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : access Box_Record := New_Box;
      W : Natural;
   begin
      W := B.Get_Width;
      
      Assert 
        (W = B.W,        
         "Test_Get_Width: W /= B.W");   
      Assert (True, "Test_Get_Width : testing Bad ");
   end Test_Get_Width;
   
   ---------------
   -- Set_Width --
   ---------------
   
   procedure Test_Set_Width
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : access Box_Record := New_Box;
      W : Natural := 8;
   begin
      B.Set_Width (W);
      
      Assert 
        (W = B.W,        
         "Test_Set_Width: W /= B.W");  
      
      W := 0;
      B.Set_Width (W);
      
      Assert 
        (W = B.W,        
         "Test_Set_Width: W /= B.W");
      Assert (True, "Test_Set_Width : testing Bad ");
   end Test_Set_Width;
   
   ---------------
   -- Place_Box --
   ---------------
   
   procedure Test_Place_Box
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      Assert (True, "Test_Place_Box : testing Bad ");
   end Test_Place_Box;
   
   ------------------------
   -- Absolute_Place_Box --
   ------------------------
   
   procedure Test_Absolute_Place_Box
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      Assert (True, "Test_Absolute_Place_Box : testing Bad ");
   end Test_Absolute_Place_Box;
   
   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
      use Test_Cases, Test_Cases.Registration;
   begin
      
      Register_Routine
        (T, Test_Initialize'Access, "Test_Initialize");
      
      Register_Routine 
        (T, Test_New_Box'Access, "Test_New_Box");
      Register_Routine
        (T, Test_Free_Box'Access, "Test_Free_Box");
      Register_Routine
        (T, Test_Get_Node'Access, "Test_Get_Node");
      Register_Routine
        (T, Test_Set_Node'Access, "Test_Set_Node");
      Register_Routine
        (T, Test_Get_Box_Kind'Access, "Test_Get_Box_Kind");
      Register_Routine
        (T, Test_Set_Box_Kind'Access, "Test_Set_Box_Kind");
      Register_Routine
        (T, Test_Get_Parent_Box'Access, "Test_Get_Parent_Box");
      Register_Routine
        (T, Test_Set_Parent_Box'Access, "Test_Set_Parent_Box");
      Register_Routine
        (T, Test_Get_Orientation'Access, "Test_Get_Orientation");
      Register_Routine
        (T, Test_Set_Orientation'Access, "Test_Set_Orientation");
      Register_Routine
        (T, Test_Get_Is_Action_Box'Access, "Test_Get_Is_Action_Box");
      Register_Routine
        (T, Test_Set_Is_Action_Box'Access, "Test_Set_Is_Action_Box");
      Register_Routine
        (T, Test_Get_X'Access, "Test_Get_X");
      Register_Routine
        (T, Test_Set_X'Access, "Test_Set_X");
      Register_Routine
        (T, Test_Get_Y'Access, "Test_Get_Y");
      Register_Routine
        (T, Test_Set_Y'Access, "Test_Set_Y");
      Register_Routine
        (T, Test_Get_Xabs'Access, "Test_Get_Xabs");
      Register_Routine
        (T, Test_Set_Xabs'Access, "Test_Set_Xabs");
      Register_Routine
        (T, Test_Get_Yabs'Access, "Test_Get_Yabs");
      Register_Routine
        (T, Test_Set_Yabs'Access, "Test_Set_Yabs");
      Register_Routine
        (T, Test_Get_Height'Access, "Test_Get_Height");
      Register_Routine
        (T, Test_Set_Height'Access, "Test_Set_Height");
      Register_Routine
        (T, Test_Get_Width'Access, "Test_Get_Width");
      Register_Routine
        (T, Test_Set_Width'Access, "Test_Set_Width");
      Register_Routine
        (T, Test_Place_Box'Access, "Test_Place_Box");
      Register_Routine
        (T, Test_Absolute_Place_Box'Access, "Test_Absolute_Place_Box");
      
   end Register_Tests;

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case) return Message_String is
   begin
      return Format ("Reflex.Boxes.Tests");
   end Name;

end Reflex.Boxes.Tests;
