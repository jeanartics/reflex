with AUnit; use AUnit;
with AUnit.Test_Cases;

package Artics.Events_Tests is
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
   
   procedure Test_Initialize
     (R : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_New_Event_Object
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   
   procedure Test_Event_Type
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   
   procedure Test_Object
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   
   procedure Test_Time
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   
   procedure Test_Sender
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   
   procedure Test_Consume
     (R : in out AUnit.Test_Cases.Test_Case'Class);

end Artics.Events_Tests;
