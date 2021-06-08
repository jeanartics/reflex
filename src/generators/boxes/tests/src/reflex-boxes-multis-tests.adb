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

package body Reflex.Boxes.Multis.Tests is

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
      use Boxes_Lists;
      B : access Multi_Box_Record := New_Multi_Box;
   begin
      Assert
        (B.Node = Empty,
         "Test_New_Box: B.node /= Empty");
      Assert
        (B.Kind = Multi_Box,
         "Test_New_Box: B.Kind /= Multi_Box");
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
        (B.Childs = Boxes_Lists.Empty_List,
         "Test_New_Box: B.Childs /= Boxes_Lists.Empty_List");
            
      Assert (True, "Test_New_Box : testing Bad ");
   end Test_New_Box;
   
   --------------
   -- Free_Box --
   --------------
   
   procedure Test_Free_Box
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      B : Multi_Box_Ptr := New_Multi_Box;
   begin
      Free_Multi_Box (B);
      Assert 
        (B = null,
         "Test_Free_Box: B /= null");
      Assert (True, "Test_Free_Box : testing Bad ");
   end Test_Free_Box;
   
   ------------------
   -- Get_Children --
   ------------------
   
   procedure Test_Get_Children 
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      use Boxes_Lists;
      
      B  : Multi_Box_Ptr := New_Multi_Box;  
      B1 : Box_Ptr := New_Box;
      B2 : Box_Ptr := New_Box;
      B3 : Box_Ptr := New_Box;
      BL : Boxes_Lists.List := Boxes_Lists.Empty_List;
   begin
      BL := B.Get_Children;
      Assert
        (BL = B.Childs,
         "Test_Get_Children: B.Childs /= BL");
      
      Boxes_Lists.Append (B.Childs, Box_Class_Ptr (B1));     
      Boxes_Lists.Append (B.Childs, Box_Class_Ptr (B2));
      Boxes_Lists.Append (B.Childs, Box_Class_Ptr (B3));
      
      BL := B.Get_Children;
      Assert
        (BL = B.Childs,
         "Test_Get_Children: B.Childs /= BL");

      Assert (True, "Test_Get_Children : testing Bad ");
   end Test_Get_Children;
   
   ------------------
   -- Set_Children --
   ------------------
   
   procedure Test_Set_Children
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      use Boxes_Lists;
      
      B  : Multi_Box_Ptr := New_Multi_Box;  
      B1 : Box_Ptr := New_Box;
      B2 : Box_Ptr := New_Box;
      B3 : Box_Ptr := New_Box;
      BL : Boxes_Lists.List := Boxes_Lists.Empty_List;
   begin
      B.Set_Children (BL);

      Assert
        (BL = B.Childs,
         "Test_Set_Children: B.Childs /= BL");
      
      Boxes_Lists.Append (BL, Box_Class_Ptr (B1));
      Boxes_Lists.Append (BL, Box_Class_Ptr (B2));
      Boxes_Lists.Append (BL, Box_Class_Ptr (B3));
      
      B.Set_Children (BL);

      Assert
        (BL = B.Childs,
         "Test_Set_Children: B.Childs /= BL");
      
      Assert (True, "Test_Set_Children : testing Bad ");
   end Test_Set_Children;
   
   ----------------------
   -- Append_Child_Box --
   ----------------------
   
   procedure Test_Append_Child_Box 
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B  : Multi_Box_Ptr := New_Multi_Box;    
      B1 : Box_Ptr := New_Box;  
   begin
      Append_Child_Box (B,B1);
      
      Assert
        (Box_Ptr(B.Childs.Last_Element) = B1,
         "Test_Append_Child_Box: Box_Ptr(B.Childs.Last_Element) /= B1");
      
      Assert (True, "Test_Append_Child_Box : testing Bad ");
   end Test_Append_Child_Box;
   
   ---------------
   -- Place_Box --
   ---------------
   
   procedure Test_Place_Box
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B  : Multi_Box_Ptr := New_Multi_Box;  
      B1 : Box_Ptr := New_Box;
      B2 : Box_Ptr := New_Box;
      B3 : Box_Ptr := New_Box;
   begin
      B1.X := 0;
      B1.Y := 0;
      B1.W := 0;
      B1.H := 0;
      
      B2.X := 0;
      B2.Y := 0;
      B2.W := 0;
      B2.H := 0;
      
      B3.X := 0;
      B3.Y := 0;
      B3.W := 0;
      B3.H := 0;
      
      B.Append_Child_Box (B1);
      B.Append_Child_Box (B2);
      B.Append_Child_Box (B3);
      
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
        (B3.X = 0,
         "Test_Set_Box2: B1.X /= 0");
      Assert 
        (B3.Y = 0,
         "Test_Place_Box: B1.Y /= 0");
      Assert 
        (B.W = 0,
         "Test_Place_Box: B.W /= 0");
      Assert 
        (B.H = 0,
         "Test_Place_Box: B.H /= 0");
      
      B.Set_Orientation (Vertical);
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
        (B3.X = 0,
         "Test_Set_Box2: B1.X /= 0");
      Assert 
        (B3.Y = 0,
         "Test_Place_Box: B1.Y /= 0");
      Assert 
        (B.W = 0,
         "Test_Place_Box: B.W /= 0");
      Assert 
        (B.H = 0,
         "Test_Place_Box: B.H /= 0");
      
      B1.X := 1;
      B1.Y := 2;
      B1.W := 3;
      B1.H := 4;
      
      B2.X := 5;
      B2.Y := 6;
      B2.W := 7;
      B2.H := 8;
      
      B3.X := 9;
      B3.Y := 1;
      B3.W := 2;
      B3.H := 3;
      
      B.Set_Orientation (Horizontal);
      B.Place_Box;
      
      Assert 
        (B1.X = 0,
         "Test_Place_Box: B1.X /= 0");
      Assert 
        (B1.Y = 0,
         "Test_Place_Box: B1.Y /= 0");
      Assert 
        (B2.X = 3,
         "Test_Place_Box: B2.X /= 3");
      Assert 
        (B2.Y = 0,
         "Test_Place_Box: B2.Y /= 0");
      Assert 
        (B3.X = 10,
         "Test_Place_Box: B1.X /= 10");
      Assert 
        (B3.Y = 0,
         "Test_Place_Box: B1.Y /= 0");
      Assert 
        (B.W = 12,
         "Test_Place_Box: B.W /= 12");
      Assert 
        (B.H = 8,
         "Test_Place_Box: B.H /= 0");
      
     
      B.Set_Orientation (Vertical);
      B.Place_Box;
            
      Assert 
        (B1.X = 0,
         "Test_Place_Box: B1.X /= 0");
      Assert 
        (B1.Y = 0,
         "Test_Place_Box: B1.Y /= 0");
      Assert 
        (B2.X = 0,
         "Test_Place_Box: B2.X /= 3");
      Assert 
        (B2.Y = 4,
         "Test_Place_Box: B2.Y /= 0");
      Assert 
        (B3.X = 0,
         "Test_Place_Box: B1.X /= 10");
      Assert 
        (B3.Y = 12,
         "Test_Place_Box: B1.Y /= 0");
      Assert 
        (B.W = 7,
         "Test_Place_Box: B.W /= 7");
      Assert 
        (B.H = 15,
         "Test_Place_Box: B.H /= 0");
      Assert (True, "Test_Place_Box : testing Bad ");
   end Test_Place_Box;
   
   ------------------------
   -- Absolute_Place_Box --
   ------------------------
   
   procedure Test_Absolute_Place_Box
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B  : Multi_Box_Ptr := New_Multi_Box;  
      B1 : Box_Ptr := New_Box;
      B2 : Box_Ptr := New_Box;
      B3 : Box_Ptr := New_Box;
   begin
      B.Xabs := 0;
      B.Yabs := 0;
      
      B1.X := 0;
      B1.Y := 0;
     
      B2.X := 0;
      B2.Y := 0;
     
      B3.X := 0;
      B3.Y := 0;
     
      B.Append_Child_Box (B1);
      B.Append_Child_Box (B2);
      B.Append_Child_Box (B3);
      
      B.Absolute_Place_Box;

      Assert 
        (B1.X = 0,
         "Test_Absolute_Place_Box: B1.X /= 0");
      Assert 
        (B1.Y = 0,
         "Test_Absolute_Place_Box: B1.Y /= 0");
      Assert 
        (B2.X = 0,
         "Test_Absolute_Place_Box: B2.X /= 0");
      Assert 
        (B2.Y = 0,
         "Test_Absolute_Place_Box: B2.Y /= 0");
      Assert 
        (B3.X = 0,
         "Test_Absolute_Place_Box: B3.X /= 0");
      Assert 
        (B3.Y = 0,
         "Test_Absolute_Place_Box: B3.Y /= 0");
      
      B.Xabs := 2;
      B.Yabs := 3;
      
      B1.X := 4;
      B1.Y := 5;
     
      B2.X := 6;
      B2.Y := 7;
     
      B3.X := 8;
      B3.Y := 9;
      
      B.Absolute_Place_Box;

      Assert 
        (B1.X = 6,
         "Test_Absolute_Place_Box: B1.X /= 6");
      Assert 
        (B1.Y = 8,
         "Test_Absolute_Place_Box: B1.Y /= 8");
      Assert 
        (B2.X = 8,
         "Test_Absolute_Place_Box: B2.X /= 8");
      Assert 
        (B2.Y = 10,
         "Test_Absolute_Place_Box: B2.Y /= 10");
      Assert 
        (B3.X = 10,
         "Test_Absolute_Place_Box: B3.X /= 10");
      Assert 
        (B3.Y = 12,
         "Test_Absolute_Place_Box: B3.Y /= 12");
      
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
        (T, Test_Get_Children'Access, "Test_Get_Children");
      Register_Routine
        (T, Test_Set_Children'Access, "Test_Set_Children");
      Register_Routine
        (T, Test_Append_Child_Box'Access, "Test_Append_Child_Box");
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
      return Format ("Reflex.Boxes.Multis.Tests");
   end Name;

end Reflex.Boxes.Multis.Tests;
