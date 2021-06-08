with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_Io; use Ada.Text_Io;

package body Artics.Maths.Tests is
   
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
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert (True, "Initialize : testing OK ");
   end Test_Initialize;
   
   --------------------
   -- Register_Tests --
   --------------------
   
   procedure Register_Tests (T : in out Test_Case) 
   is
      use Test_Cases, Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Initialize'Access, "Initialize");
      
      Register_Routine (T, Test_Epsilon_Equal'Access, "Test_Epsilon_Equal");
      Register_Routine (T, Test_Min'Access, "Test_Min");
      Register_Routine (T, Test_Max'Access, "Test_Max");
      Register_Routine (T, Test_Round'Access, "Test_Round");
      Register_Routine (T, Test_Truncate'Access, "Test_Truncate");
      Register_Routine (T, Test_Floor'Access, "Test_Floor");
      
   end Register_Tests;

   ----------
   -- Name --
   ----------
   
   function Name (T : Test_Case) return Message_String is
   begin
      return Format ("Artics.Maths.Tests");
   end Name;


end Artics.Maths.Tests;

