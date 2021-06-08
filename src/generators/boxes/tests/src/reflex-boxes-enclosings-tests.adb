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

package body Reflex.Boxes.Enclosings.Tests is

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
      
      B : access Enclosing_Box_Record := New_Enclosing_Box;
   begin
      Assert
        (B.Node = Empty,
         "Test_New_Box: B.node /= Empty");
      Assert
        (B.Kind = Enclosing_Box,
         "Test_New_Box: B.Kind /= Enclosing_Box");
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
        (B.Box = null,
         "Test_New_Box: B.Box /= null");
      
      Assert (True, "Test_New_Box : testing Bad ");
   end Test_New_Box;
   
   --------------
   -- Free_Box --
   --------------
   
   procedure Test_Free_Box
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : Enclosing_Box_Ptr := New_Enclosing_Box;
   begin
      Free_Enclosing_Box (B);
      Assert 
        (B = null,
         "Test_Free_Box: B /= null");      
      Assert (True, "Test_Free_Box : testing Bad ");
   end Test_Free_Box;
   
   -------------
   -- Get_Box --
   -------------
   
   procedure Test_Get_Box
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B  : Enclosing_Box_Ptr := New_Enclosing_Box;
      B1 : Enclosing_Box_Ptr;
   begin
      B1 := B.Get_Box;
      Assert 
        (B1 = B.Box,
         "Test_Get_Box1: B1 /= B.Box");
      Assert (True, "Test_Get_Box1 : testing Bad ");
   end Test_Get_Box;
   
   -------------
   -- Set_Box --
   -------------
   
   procedure Test_Set_Box
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B  : Enclosing_Box_Ptr := New_Enclosing_Box;
      B1 : Enclosing_Box_Ptr := New_Enclosing_Box;
   begin
      B.Set_Box (B1);
      Assert 
        (B1 = B.Box,
         "Test_Set_Box1: B1 /= B.Box");
      Assert (True, "Test_Set_Box1 : testing Bad ");
   end Test_Set_Box;
   
  
   ---------------
   -- Place_Box --
   ---------------
   
   procedure Test_Place_Box
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B  : Enclosing_Box_Ptr := New_Enclosing_Box;
   begin         
      B.X := 0;
      B.Y := 0;
      B.W := 0;
      B.H := 0;
     
      B.Place_Box;

      Assert 
        (B.X = 0,
         "Test_Set_Box2: B1.X /= 0");
      Assert 
        (B.Y = 0,
         "Test_Place_Box: B1.Y /= 0");
      Assert 
        (B.W = Max_Unity_Ladder_Horizontal,
         "Test_Place_Box: B.W /= Max_Unity_Ladder_Horizontal");
      Assert 
        (B.H = Max_Unity_Ladder_Vertical,
         "Test_Place_Box: B.H /= Max_Unity_Ladder_Vertical");
     
      B.Set_Orientation (Vertical);
      B.Place_Box;
      
      Assert 
        (B.X = 0,
         "Test_Set_Box2: B1.X /= 0");
      Assert 
        (B.Y = 0,
         "Test_Place_Box: B1.Y /= 0");
      Assert 
        (B.W = Max_Unity_Ladder_Horizontal,
         "Test_Place_Box: B.W /= Max_Unity_Ladder_Horizontal");
      Assert 
        (B.H = Max_Unity_Ladder_Vertical,
         "Test_Place_Box: B.H /= Max_Unity_Ladder_Vertical");
      
      B.X := 2;
      B.Y := 3;
      B.W := 4;
      B.H := 5;
      
      B.Set_Orientation (Horizontal);
      B.Place_Box;

      Assert 
        (B.X = 0,
         "Test_Set_Box2: B1.X /= 0");
      Assert 
        (B.Y = 0,
         "Test_Place_Box: B1.Y /= 0");
      Assert 
        (B.W = Max_Unity_Ladder_Horizontal,
         "Test_Place_Box: B.W /= Max_Unity_Ladder_Horizontal");
      Assert 
        (B.H = Max_Unity_Ladder_Vertical,
         "Test_Place_Box: B.H /= Max_Unity_Ladder_Vertical"); 
      
      B.Set_Orientation (Vertical);
      B.Place_Box;
      
      Assert 
        (B.X = 0,
         "Test_Set_Box2: B1.X /= 0");
      Assert 
        (B.Y = 0,
         "Test_Place_Box: B1.Y /= 0");
      Assert 
        (B.W = Max_Unity_Ladder_Horizontal,
         "Test_Place_Box: B.W /= Max_Unity_Ladder_Horizontal");
      Assert 
        (B.H = Max_Unity_Ladder_Vertical,
         "Test_Place_Box: B.H /= Max_Unity_Ladder_Vertical"); 
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
        (T, Test_Get_Box'Access, "Test_Get_Box");
      Register_Routine
        (T, Test_Set_Box'Access, "Test_Set_Box");
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
      return Format ("Reflex.Boxes.Enclosings2.Tests");
   end Name;

end Reflex.Boxes.Enclosings.Tests;
