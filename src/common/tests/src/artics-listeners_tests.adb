with AUnit.Assertions; use AUnit.Assertions;

with Artics.Objects; use Artics.Objects;
with Artics.Generic_Events_Objects;
with Artics.Generic_Listeners_Interfaces;

package body Artics.Listeners_Tests is
   
   type My_Event_Type_Enum is 
     (No_Event,
      Event1,
      Event2,
      Event3);

   package Events is new Artics.Generic_Events_Objects (My_Event_Type_Enum);
   use Events;
   
   package Listeners is new Artics.Generic_Listeners_Interfaces 
     (My_Event_Type_Enum, Events);
   use Listeners;
   
   type Listen_Record is new Object_Record and Listener_Interface 
     with record
	I          : Integer;
	Evt_Listen : My_Event_Type_Enum;
   end record;
   type Listen_Ptr is access all Listen_Record;

   procedure Invoke
     (Listener : access Listen_Record;
      Sender   : access Object_Record'Class;
      Evt      : access Events.Event_Object_Record'Class);

   No_Listen_Record : constant Listen_Record :=
     Listen_Record'
     (No_Object_Record with 
	I => 0,
      Evt_Listen => No_Event);
   
   
   type Sender_Record is new Object_Record with record
	I        : Integer;
	Evt_Sender : My_Event_Type_Enum;
   end record;
   type Sender_Ptr is access all Sender_Record;

   No_Sender_Record : constant Sender_Record :=
     Sender_Record'
     (No_Object_Record with 
	I      => 0,
      Evt_Sender => No_Event);
   
   
   ------------
   -- Invoke --
   ------------
   
   procedure Invoke
     (Listener : access Listen_Record;
      Sender   : access Object_Record'Class;
      Evt      : access Events.Event_Object_Record'Class) is
      Sn : Sender_Ptr;
   begin
      Sn := Sender_Ptr (Sender);
      Listener.I   := Sn.I;
      Listener.Evt_Listen := Get_Event_Type (Evt);
   end Invoke;
   
   
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
      Assert 
	(True, "Initialize : testing OK ");
   end Test_Initialize;

   -----------------
   -- Test_Invoke --
   -----------------
   
   procedure Test_Invoke
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Listen : access Listen_Record := new Listen_Record'(No_Listen_Record);
      Sender : access Sender_Record := new Sender_Record'(No_Sender_Record);
      Evt    : access Events.Event_Object_Record :=
	new Events.Event_Object_Record'(Events.No_Event_Object_Record);
   begin
      Sender.Evt_Sender := Event1;
      Set_Event_Type (Evt, Sender.Evt_Sender);
      Sender.I := 10;
      Listen.Invoke
	(Sender   => Sender,
	 Evt      => Evt);
      
      Assert 
	(Listen.Evt_Listen = Sender.Evt_Sender,
	 "Test_Invoke (1) : testing OK ");
      Assert 
	(Listen.I = Sender.I,
	 "Test_Invoke (2) : testing OK ");
   end Test_Invoke;
   
   --------------------
   -- Register_Tests --
   --------------------
   
   procedure Register_Tests (T : in out Test_Case) 
   is
      use Test_Cases, Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Initialize'Access, "Initialize");
      Register_Routine (T, Test_Invoke'Access, "Test_Invoke");
   end Register_Tests;

   ----------
   -- Name --
   ----------
   
   function Name (T : Test_Case) return Message_String is
   begin
      return Format ("Artics.Listeners_Objects_Tests");
   end Name;


end Artics.Listeners_Tests;

