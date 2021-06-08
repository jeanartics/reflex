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

package body Reflex.Boxes.Terminals.Tests is

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
      
      B : access Terminal_Box_Record := New_Terminal_Box;
   begin
      Assert
        (B.Node = Empty,
         "Test_New_Box: B.node /= Empty");
      Assert
        (B.Kind = Terminal_Box,
         "Test_New_Box: B.Kind /= Terminal_Box");
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
        (B.Typ = No_Box_Typ,
         "Test_New_Box: B.Box1 /= null");
            
      Assert (True, "Test_New_Box : testing Bad ");
   end Test_New_Box;
   
   --------------
   -- Free_Box --
   --------------
   
   procedure Test_Free_Box
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : Terminal_Box_Ptr := New_Terminal_Box ;
   begin
      Free_Terminal_Box (B);
      Assert 
        (B = null,
         "Test_Free_Box: B /= null");
      Assert (True, "Test_Free_Box : testing Bad ");
   end Test_Free_Box;
   
   -------------
   -- Get_Typ --
   -------------
    
   procedure Test_Get_Typ 
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : Terminal_Box_Ptr := New_Terminal_Box ;
      T : Box_Typ;
   begin
      T := B.Get_Typ;
      Assert 
        (T = B.Typ,
         "Test_Get_Typ: T /= B.typ");
      Assert (True, "Test_Get_Typ : testing Bad ");
   end Test_Get_Typ;
      
   -------------
   -- Set_Typ --
   -------------
   
   procedure Test_Set_Typ
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : Terminal_Box_Ptr := New_Terminal_Box ;
      T : Box_Typ := Coil_Box;
   begin
      B.Set_Typ (T);
      Assert 
        (T = B.Typ,
         "Test_Set_Typ: T /= B.typ");
      
      T := Open_Contact_Box;
      B.Set_Typ (T);
      Assert 
        (T = B.Typ,
         "Test_Set_Typ: T /= B.typ");
      
      T := Ffb_Box;
      B.Set_Typ (T);
      Assert 
        (T = B.Typ,
         "Test_Set_Typ: T /= B.typ");
      
      T := No_Box_Typ;
      B.Set_Typ (T);
      Assert 
        (T = B.Typ,
         "Test_Set_Typ: T /= B.typ");
      
      T := Not_Coil_Box;
      B.Set_Typ (T);
      Assert 
        (T = B.Typ,
         "Test_Set_Typ: T /= B.typ");
      
      Assert (True, "Test_Set_Typ : testing Bad ");
   end Test_Set_Typ;
   
   ---------------
   -- Place_Box --
   ---------------
   
   procedure Test_Place_Box
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : Terminal_Box_Ptr := New_Terminal_Box ;      
   begin  
      B.X := 0;
      B.Y := 0;
      
      B.Place_Box;
      
      Assert 
        (B.X = 0,
         "Test_Place_Box: B1.X /= 0");
      Assert 
        (B.Y = 0,
         "Test_Place_Box: B1.Y /= 0");
      
      B.Set_Orientation (Vertical);
      B.Place_Box;
      
      Assert 
        (B.X = 0,
         "Test_Place_Box: B1.X /= 0");
      Assert 
        (B.Y = 0,
         "Test_Place_Box: B1.Y /= 0");
      
      B.X := 5;
      B.Y := 6;
      
      B.Set_Orientation (Horizontal);
      B.Place_Box;
      
      Assert 
        (B.X = 0,
         "Test_Place_Box: B1.X /= 0");
      Assert 
        (B.Y = 0,
         "Test_Place_Box: B1.Y /= 0");
      
      B.Set_Orientation (Vertical);
      B.Place_Box;
      
      Assert 
        (B.X = 0,
         "Test_Place_Box: B1.X /= 0");
      Assert 
        (B.Y = 0,
         "Test_Place_Box: B1.Y /= 0");
      
      Assert (True, "Test_Place_Box : testing Bad ");
   end Test_Place_Box;
   
   ------------------------
   -- Absolute_Place_Box --
   ------------------------
   
   procedure Test_Absolute_Place_Box
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      
      B : Terminal_Box_Ptr := New_Terminal_Box ;
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
        (T, Test_Get_Typ'Access, "Test_Get_Typ");
      Register_Routine
        (T, Test_Set_Typ'Access, "Test_Set_Typ");
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

end Reflex.Boxes.Terminals.Tests;
