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

package body Reflex.Boxes.Duals.Tests is

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
      
      B : access Dual_Box_Record := New_Dual_Box;
   begin
      Assert
        (B.Node = Empty,
         "Test_New_Box: B.node /= Empty");
      Assert
        (B.Kind = Dual_Box,
         "Test_New_Box: B.Kind /= Dual_Box");
      Assert
        (B.Parent = null,
         "Test_New_Box: B.Parent /= null");
      Assert
        (B.Orientation = Horizontal,
         "Test_New_Box: B.Orientation /= Horizontal");
      Assert
        (B.Is_Action_Box = False,
         "Test_New_Box: B.Is_Action_Box = False");
      Assert
        (B.X = 0,
         "Test_New_Box: B.X = 0");
      Assert
        (B.Y = 0,
         "Test_New_Box: B.Y = 0");
      Assert
        (B.Xabs = 0,
         "Test_New_Box: B.Xabs = 0");
      Assert
        (B.Yabs = 0,
         "Test_New_Box: B.Yabs = 0");
      Assert
        (B.H = 0,
         "Test_New_Box: B.H = 0");
      Assert
        (B.W = 0,
         "Test_New_Box: B.W = 0");
      Assert
        (B.Box1 = null,
         "Test_New_Box: B.Box1 /= null");
      Assert
        (B.Box2 = null,
         "Test_New_Box: B.Box2 /= null");
      
      Assert (True, "Test_New_Box : testing Bad ");
   end Test_New_Box;
   
   --------------
   -- Free_Box --
   --------------
   
   procedure Test_Free_Box
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : Dual_Box_Ptr := New_Dual_Box;
   begin
      Free_Dual_Box (B);
      Assert 
        (B = null,
         "Test_Free_Box: B /= null");
      Assert (True, "Test_Free_Box : testing Bad ");
   end Test_Free_Box;
   
   --------------
   -- Get_Box1 --
   --------------
   
   procedure Test_Get_Box1
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B  : Dual_Box_Ptr := New_Dual_Box;
      B1 : Box_Ptr;
   begin
      B1 := B.Get_Box1;
      Assert 
        (B1 = B.Box1,
         "Test_Get_Box1: B1 /= B.Box1");
      Assert (True, "Test_Get_Box1 : testing Bad ");
   end Test_Get_Box1;
   
   --------------
   -- Set_Box1 --
   --------------
   
   procedure Test_Set_Box1
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B  : Dual_Box_Ptr := New_Dual_Box;
      B1 : Box_Ptr := New_Box;
   begin
      B.Set_Box1 (B1);
      Assert 
        (B1 = B.Box1,
         "Test_Set_Box1: B1 /= B.Box1");
      Assert (True, "Test_Set_Box1 : testing Bad ");
   end Test_Set_Box1;
   
   --------------
   -- Get_Box2 --
   --------------
   
   procedure Test_Get_Box2
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B  : Dual_Box_Ptr := New_Dual_Box;
      B2 : Box_Ptr;
   begin
      B2:= B.Get_Box2;
      Assert 
        (B2 = B.Box2,
         "Test_Get_Box2: B2 /= B.Box1");
      Assert (True, "Test_Get_Box2 : testing Bad ");
   end Test_Get_Box2;
   
   --------------
   -- Set_Box2 --
   --------------
   
   procedure Test_Set_Box2
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B  : Dual_Box_Ptr := New_Dual_Box;
      B2 : Dual_Box_Ptr := New_Dual_Box;
   begin
      B.Set_Box2 (B2);
      Assert 
        (B2 = B.Box2,
         "Test_Set_Box2: B2 = B.Box2");
      Assert (True, "Test_Set_Box2 : testing Bad ");
   end Test_Set_Box2;
   
   ---------------
   -- Place_Box --
   ---------------
   
   procedure Test_Place_Box
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B  : Dual_Box_Ptr := New_Dual_Box;
      B1 : Box_Ptr := New_Box;
      B2 : Box_Ptr := New_Box;
      
   begin   
      B.Box1 := B1;
      B.Box2 := B2;
      
      B1.X := 0;
      B1.Y := 0;
      B1.W := 0;
      B1.H := 0;
      
      B2.X := 0;
      B2.Y := 0;  
      B2.W := 0;
      B2.H := 0;
      
      B.Place_Box;

      Assert 
        (B1.X = 0,
         "Test_Set_Box2: B1.X /= 0");
      Assert 
        (B1.Y = 0,
         "Test_Place_Box: B1.Y /= 0");
      Assert 
        (B2.X = 0,
         "Test_Place_Box: B2.X /= 0");
      Assert 
        (B2.Y = 0,
         "Test_Place_Box: B2.Y /= 0");
      Assert 
        (B.W = 0,
         "Test_Place_Box: B.W /= 0");
      Assert 
        (B.H = 0,
         "Test_Place_Box: B.H /= 0");
      
      B.Orientation := Vertical;
      B.Place_Box;

      Assert 
        (B1.X = 0,
         "Test_Place_Box: B1.X /= 0");
      Assert 
        (B1.Y = 0,
         "Test_Place_Box: B1.Y /= 0");
      Assert 
        (B2.X = 0,
         "Test_Place_Box: B2.X /= 0");
      Assert 
        (B2.Y = 0,
         "Test_Place_Box: B2.Y /= 0");
      Assert 
        (B.W = 0,
         "Test_Place_Box: B.W /= 0");
      Assert 
        (B.H = 0,
         "Test_Place_Box: B.H /= 0");
      
      B1.X := 1;
      B1.Y := 2;
      B1.W := 5;
      B1.H := 6;
      
      B2.X := 3;
      B2.Y := 4;  
      B2.W := 7;
      B2.H := 8;
      
      B.Orientation := Horizontal;
      B.Place_Box;

      Assert 
        (B1.X = 0,
         "Test_Set_Box2: B1.X /= 0");
      Assert 
        (B1.Y = 0,
         "Test_Place_Box: B1.Y /= 0");
      Assert 
        (B2.X = 5,
         "Test_Place_Box: B2.X /= 5");
      Assert 
        (B2.Y = 0,
         "Test_Place_Box: B2.Y /= 0");
      Assert 
        (B.W = 12,
         "Test_Place_Box: B.W /= 12");
      Assert 
        (B.H = 8,
         "Test_Place_Box: B.H /= 8");
      
      B.Orientation := Vertical;
      B.Place_Box;

      Assert 
        (B1.X = 0,
         "Test_Place_Box: B1.X /= 0");
      Assert 
        (B1.Y = 0,
         "Test_Place_Box: B1.Y /= 0");
      Assert 
        (B2.X = 0,
         "Test_Place_Box: B2.X /= 0");
      Assert 
        (B2.Y = 6,
         "Test_Place_Box: B2.Y /= 6");
      Assert 
        (B.W = 7,
         "Test_Place_Box: B.W /= 7");
      Assert 
        (B.H = 14,
         "Test_Place_Box: B.H /= 14");
            
      Assert (True, "Test_Place_Box : testing Bad ");
   end Test_Place_Box;
   
   ------------------------
   -- Absolute_Place_Box --
   ------------------------
   
   procedure Test_Absolute_Place_Box
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B  : Dual_Box_Ptr := New_Dual_Box;
      B1 : Box_Ptr := New_Box;
      B2 : Box_Ptr := New_Box;
   begin   
      B.Box1  := B1;
      B.Box2  := B2;
      B.Xabs := 0;
      B.Yabs := 0;
      
      B1.X := 0;
      B1.Y := 0;
      
      B2.X := 0;
      B2.Y := 0;  
      
      B.Absolute_Place_Box;

      Assert 
        (B1.X = 0,
         "Test_Set_Box2: B1.X /= 0");
      Assert 
        (B1.Y = 0,
         "Test_Place_Box: B1.Y /= 0");
      Assert 
        (B2.X = 0,
         "Test_Place_Box: B2.X /= 0");
      Assert 
        (B2.Y = 0,
         "Test_Place_Box: B2.Y /= 0");
      
      B.Xabs := 2;
      B.Yabs := 3;
      
      B1.X := 4;
      B1.Y := 5;
      
      B2.X := 6;
      B2.Y := 7;  
      
      B.Absolute_Place_Box;

      Assert 
        (B1.X = 6,
         "Test_Set_Box2: B1.X /= 6");
      Assert 
        (B1.Y = 8,
         "Test_Place_Box: B1.Y /= 8");
      Assert 
        (B2.X = 8,
         "Test_Place_Box: B2.X /= 8");
      Assert 
        (B2.Y = 10,
         "Test_Place_Box: B2.Y /= 10");
      
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
        (T, Test_Get_Box1'Access, "Test_Get_Box1");
      Register_Routine
        (T, Test_Set_Box1'Access, "Test_Set_Box1");
      Register_Routine
        (T, Test_Get_Box2'Access, "Test_Get_Box2");
      Register_Routine
        (T, Test_Set_Box2'Access, "Test_Set_Box2");
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
      return Format ("Reflex.Boxes.duals.Tests");
   end Name;

end Reflex.Boxes.Duals.Tests;
