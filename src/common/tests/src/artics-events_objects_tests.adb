with AUnit.Assertions; use AUnit.Assertions;

with Artics.Objects; use Artics.Objects;
with Artics.Generic_Events_Objects;

package body Artics.Events_Objects_Tests is
   
   type Event_Type_Enum is 
     (No_Event,
      Event1,
      Event2,
      Event3);

   package Events is new Artics.Generic_Events_Objects (Event_Type_Enum);
   use Events;
   
   type My_Object_Record is new Object_Record with record
      I : Integer;
   end record;
   type My_Object_Ptr is access all My_Object_Record;
   
   No_My_Object_Record : constant My_Object_Record :=
     My_Object_Record'
     (No_Object_Record with I => 0);
   
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

   ---------------------------
   -- Test_New_Event_Object --
   ---------------------------
   
   procedure Test_New_Event_Object
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Ev : Event_Object_Ptr;
      
      Ev_Cst : Event_Object_Ptr;
   begin
      Ev := Events.New_Event_Object;
      Assert
	(Ev.all = No_Event_Object_Record, 
	 "Test_New_Event_Object (1) : testing OK ");
      
      Ev := Events.New_Event_Object (Event1);
      Ev_Cst := Events.New_Event_Object;
      Set_Event_Type (Ev_Cst, Event1);
      Assert
	(Ev.all = Ev_Cst.all,
	 "Test_New_Event_Object (2) : testing OK ");
   end Test_New_Event_Object;
   
   ---------------------
   -- Test_Event_Type --
   ---------------------
   
   procedure Test_Event_Type
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Ev : Event_Object_Ptr;
   begin
      Ev := Events.New_Event_Object;
      Set_Event_Type (Ev, Event1);
      
      Assert
	(Get_Event_Type (Ev) = Event1 ,
	 "Test_Event_Type : testing OK ");
   end Test_Event_Type;
   
   -----------------
   -- Test_Object --
   -----------------
   
   procedure Test_Object
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is 
      Ev  : Event_Object_Ptr;
      My  : access My_Object_Record'Class := 
	new My_Object_Record'(No_My_Object_Record);
      My2 : access My_Object_Record'Class := 
	new My_Object_Record'(No_My_Object_Record);
   begin
      Ev := Events.New_Event_Object;
      
      Set_Object (Ev, My);
      My2 := My_Object_Ptr (Get_Object (Ev));
      Assert
	(My2= My,
	 "Test_Object (1) : testing OK ");
      
      My.I := 2;
      My2 := My_Object_Ptr (get_Object (Ev));
      Assert
	(My2.I = 2,
	 "Test_Object (2) : testing OK ");
   end Test_Object;
      
   ---------------
   -- Test_Time --
   ---------------
   
   procedure Test_Time
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Ev : Event_Object_Ptr;
   begin
      Ev := Events.New_Event_Object;
      Set_Time (Ev, 23);
      
      Assert
	(Get_Time (Ev) = 23,
	 "Test_Time : testing OK ");
   end Test_Time;
      
   -----------------
   -- Test_Sender --
   -----------------
   
   procedure Test_Sender
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Ev  : Event_Object_Ptr;
      My  : access My_Object_Record'Class := 
	new My_Object_Record'(No_My_Object_Record);
      My2 : access My_Object_Record'Class := 
	new My_Object_Record'(No_My_Object_Record);
   begin
      Ev := Events.New_Event_Object;
      
      Set_Sender (Ev, My);
      My2 := My_Object_Ptr (Get_Sender (Ev));
      Assert
	(My2 = My,
	 "Test_Sender : testing OK ");
   end Test_Sender;
      
   ------------------
   -- Test_Consume --
   ------------------
   
   procedure Test_Consumed
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Ev  : Event_Object_Ptr;
   begin
      Ev := Events.New_Event_Object;
      
      Assert
	(not Is_Consumed (Ev),
	 "Test_Consumed (1) : testing OK ");
      
      Consume (Ev);
      Assert
	(Is_Consumed (Ev),
	 "Test_Consumed (2) : testing OK ");
   end Test_Consumed;
   
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
      Register_Routine (T, Test_Consumed'Access   , "Consumed");
   end Register_Tests;

   ----------
   -- Name --
   ----------
   
   function Name (T : Test_Case) return Message_String is
   begin
      return Format ("Artics.Events_Objects_Tests");
   end Name;


end Artics.Events_Objects_Tests;

