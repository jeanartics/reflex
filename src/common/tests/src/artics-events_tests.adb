with AUnit.Assertions; use AUnit.Assertions;

with Artics.Objects; use Artics.Objects;
with Artics.Generic_Events_Objects;
with Artics.Generic_Listeners_Interfaces;
with Artics.Generic_Events_Sources; 

--with Artics.Events_Tests_Types; use Artics.Events_Tests_Types;

package body Artics.Events_Tests is
   
   type Event_Type_Enum is 
     (No_Event,
      Event1,
      Event2,
      Event3);

   package Events is new Artics.Generic_Events_Objects (Event_Type_Enum);
   
   package Listeners is new Artics.Generic_Listeners_Interfaces 
     (Event_Type_Enum, Events);
   
   package Events_Sources is new Artics.Generic_Events_Sources
     (Event_Type_Enum,
      Events,
      Listeners);
   
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

   ---------------------------
   -- Test_New_Event_Object --
   ---------------------------
   
   procedure Test_New_Event_Object
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert (True, "Test_New_Event_Object : testing OK ");
   end Test_New_Event_Object;
   
   ---------------------
   -- Test_Event_Type --
   ---------------------
   
   procedure Test_Event_Type
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert (True, "Test_Event_Type : testing OK ");
   end Test_Event_Type;
   
   -----------------
   -- Test_Object --
   -----------------
   
   procedure Test_Object
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert (True, "Test_Object : testing OK ");
   end Test_Object;
      
   ---------------
   -- Test_Time --
   ---------------
   
   procedure Test_Time
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert (True, "Test_Time : testing OK ");
   end Test_Time;
      
   -----------------
   -- Test_Sender --
   -----------------
   
   procedure Test_Sender
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert (True, "Test_Sender : testing OK ");
   end Test_Sender;
      
   ------------------
   -- Test_Consume --
   ------------------
   
   procedure Test_Consume
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert (True, "Test_Consume : testing OK ");
   end Test_Consume;
   
   --------------------
   -- Register_Tests --
   --------------------
   
   procedure Register_Tests (T : in out Test_Case) 
   is
      use Test_Cases, Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Initialize'Access, "Initialize");
      Register_Routine (T, Test_Event_Type'Access, "Event_Type");
      Register_Routine (T, Test_Object'Access    , "Object");
      Register_Routine (T, Test_Time'Access      , "Time");
      Register_Routine (T, Test_Sender'Access    , "Sender");
      Register_Routine (T, Test_Consume'Access   , "Consume");
   end Register_Tests;

   ----------
   -- Name --
   ----------
   
   function Name (T : Test_Case) return Message_String is
   begin
      return Format ("Artics.Events_Tests");
   end Name;


end Artics.Events_Tests;

