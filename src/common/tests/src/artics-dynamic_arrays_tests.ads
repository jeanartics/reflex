with AUnit; use AUnit;
with AUnit.Test_Cases;
with Artics.Dynamic_Arrays;

package Artics.Dynamic_Arrays_Tests is
   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   --  Override:

   --  Register routines to be run:
   procedure Register_Tests (T : in out Test_Case);

   --  Provide name identifying the test case:
   function Name (T : Test_Case) return Message_String;

   --  Override if needed. Default empty implementations provided:

   --  Preparation performed before each routine:
   procedure Set_Up (T : in out Test_Case);

   --  Cleanup performed after each routine:
   procedure Tear_Down (T :  in out Test_Case);
   
end Artics.Dynamic_Arrays_Tests;
