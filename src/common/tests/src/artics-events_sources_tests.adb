with AUnit.Assertions; use AUnit.Assertions;

with Artics.Objects; use Artics.Objects;
with Artics.Generic_Events_Objects;
with Artics.Generic_Listeners_Interfaces;
with Artics.Generic_Events_Sources; 

package body Artics.Events_Sources_Tests is
   
   type My_Event_Type_Enum is 
     (No_Event,
      Event1,
      Event2,
      Event3,
      Event4);

   package Events is new Artics.Generic_Events_Objects (My_Event_Type_Enum);
   use Events;
   
   package Listeners is new Artics.Generic_Listeners_Interfaces 
     (My_Event_Type_Enum, Events);
   use Listeners;
   
   package Events_Sources is new Artics.Generic_Events_Sources
     (Event_Type_Enum,
      Events,
      Listeners);
   use Events_Sources;
   
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

   ----------------------
   -- New_Event_Source --
   ----------------------
   
   procedure Test_New_Event_Source
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Src1 : Event_Source_Ptr;
      Src2 : Event_Source_Ptr;
   begin
      Src1 := New_Event_Source;
      Assert 
	(Src1.all = No_Event_Source_Record,
	 "Test_New_Event_Source (1) : testing OK ");
      
      Src2 := New_Event_Source (Src1);
      Assert 
	(Get_Sender (Src2) = Src1,
	 "Test_New_Event_Source (2) : testing OK ");
   end Test_New_Event_Source;
   
   ---------------
   -- Listeners --
   ---------------
   
   procedure Test_Listeners
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use Listeners_Lists;

      Listen1 : access Listen_Record'Class
	:= new Listen_Record'(No_Listen_Record);
      Listen2 : access Listen_Record'Class
	:= new Listen_Record'(No_Listen_Record);
      Listen3 : access Listen_Record'Class
	:= new Listen_Record'(No_Listen_Record);
      Listen4 : access Listen_Record'Class
	:= new Listen_Record'(No_Listen_Record);
      
      Src1 : Event_Source_Ptr;
      Src2 : Event_Source_Ptr;
      
      Listen1_Item : Events_Sources.Listener_Item_Record :=
	(Evt_Type => Event1,
	 Listener => Listen1);
      Listen2_Item : Events_Sources.Listener_Item_Record :=
	(Evt_Type => Event2,
	 Listener => Listen1);
      Listen3_Item : Events_Sources.Listener_Item_Record :=
	(Evt_Type => Event3,
	 Listener => Listen1);
      Listen4_Item : Events_Sources.Listener_Item_Record :=
	(Evt_Type => Event4,
	 Listener => Listen1);
	
      L1 : Listeners_Lists.List;
      L2 : Listeners_Lists.List;
   begin
      Listen1.I := 1;
      Listen2.I := 2;
      Listen3.I := 3;
      Listen4.I := 4;
      
      Events_Sources.Listeners_Lists.Append (L1, Listen1_Item);
      Events_Sources.Listeners_Lists.Append (L1, Listen2_Item);
      Events_Sources.Listeners_Lists.Append (L1, Listen3_Item);
      Events_Sources.Listeners_Lists.Append (L1, Listen4_Item);
      
      Events_Sources.Listeners_Lists.Append (L2, Listen2_Item);
      Events_Sources.Listeners_Lists.Append (L2, Listen4_Item);
      
      Src1 := New_Event_Source;
      Src2 := New_Event_Source;
      
      Set_Sender (Src1, Src1);
      Set_Sender (Src2, Src2);
      
      Set_Listeners (Src1, L1);
      Set_Listeners (Src2, L2);
      
      Assert 
	(Get_Listeners (Src1) = L1,
	 "Test_Listeners Src1 list  : testing OK ");
      Assert 
	(Get_Listeners (Src2) = L2,
	 "Test_Listeners Src2 list  : testing OK ");
   end Test_Listeners;
   
   -------------------
   -- Set_Listeners --
   -------------------
   
   procedure Test_Set_Listeners
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      --  Tests Done in Test_Listeners
      Assert 
	(True, "Test_Set_Listeners : testing OK ");
   end Test_Set_Listeners;
   
   ------------
   -- Sender --
   ------------
   
   procedure Test_Sender
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert 
	(True, "Test_Sender : testing OK ");
   end Test_Sender;
   
   ----------------
   -- Set_Sender --
   ----------------
   
   procedure Test_Set_Sender
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert 
	(True, "Test_Set_Sender : testing OK ");
   end Test_Set_Sender;
   
   --------------------
   -- Events_Enabled --
   --------------------
   
   procedure Test_Is_Events_Enabled
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert 
	(True, "Test_Is_Events_Enabled : testing OK ");
   end Test_Is_Events_Enabled;
   
   ------------------------
   -- Set_Events_Enabled --
   ------------------------
   
   procedure Test_Set_Events_Enabled
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert 
	(True, "Test_Set_Events_Enabled : testing OK ");
   end Test_Set_Events_Enabled;
   
   ------------------
   -- Add_Listener --
   ------------------
   
   procedure Test_Add_Listener
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Listen1 : access Listen_Record'Class
	:= new Listen_Record'(No_Listen_Record);
      Listen2 : access Listen_Record'Class
	:= new Listen_Record'(No_Listen_Record);
      Listen3 : access Listen_Record'Class
	:= new Listen_Record'(No_Listen_Record);
      Listen4 : access Listen_Record'Class
	:= new Listen_Record'(No_Listen_Record);
      
      Src1 : Event_Source_Ptr;
      Src2 : Event_Source_Ptr;
      
      L      : Listeners_Lists.List;      
      Found1 : Boolean;
      Found2 : Boolean;
      Found3 : Boolean;
      Found4 : Boolean;
      Nb : Integer;
   begin
      Listen1.I := 1;
      Listen2.I := 2;
      Listen3.I := 3;
      Listen4.I := 4;
      
      Src1 := New_Event_Source;
      Src2 := New_Event_Source;
      
      Set_Sender (Src1, Src1);
      Set_Sender (Src2, Src2);
      
      Add_Listener
	(Event_Source => Src1,
	 Evt_type     => Event1,
	 Listener     => Listen1);
	   
      Add_Listener
	(Event_Source => Src1,
	 Evt_type     => Event1,
	 Listener     => Listen2);
      
      L             := Get_Listeners (Src1);
      Found1 := False;
      Found2 := False;
      Found3 := False;
      Found4 := False;
      Nb := 0;
      for Item of L loop
	 if Listen_Ptr (Item.Listener) = Listen_Ptr (Listen1) then
	    Found1 := True;
	 end if;
	 if Listen_Ptr (Item.Listener) = Listen_Ptr (Listen2) then
	    Found2 := True;
	 end if;
	 if Listen_Ptr (Item.Listener) = Listen_Ptr (Listen3) then
	    Found3 := True;
	 end if;
	 if Listen_Ptr (Item.Listener) = Listen_Ptr (Listen4) then
	    Found4 := True;
	 end if;
      end loop;
      
      Assert 
	(Found1 and Found2 and not Found3 and not Found4,
	 "Test_Add_Listener (1) : testing OK " &
	   " Found 1 => " & Found1'Img &
	   " Found 2 => " & Found2'Img &
	   " Found 3 => " & Found3'Img &
	   " Found 4 => " & Found4'Img);
      
      Found1 := False;
      Found2 := False;
      Found3 := False;
      Found4 := False;
      for Item of L loop
	 if Listen_Ptr (Item.Listener) = Listen_Ptr (Listen1) 
	   and then Item.Evt_Type = Event1 then
	    Found1 := True;
	 end if;
	 if Listen_Ptr (Item.Listener) = Listen_Ptr (Listen1) 
	   and then Item.Evt_Type /= Event1 then
	    Found2 := True;
	 end if;
      end loop;
      
      Assert 
	(Found1 and not Found2 and not Found3 and not Found4,
	 "Test_Add_Listener (2) : testing OK " &
	   " Found 1 => " & Found1'Img &
	   " Found 2 => " & Found2'Img &
	   " Found 3 => " & Found3'Img &
	   " Found 4 => " & Found4'Img);
      
      
      -- Listener 1 with Event 1 and Event 2
      
      Add_Listener
	(Event_Source => Src1,
	 Evt_type     => Event2,
	 Listener     => Listen1);
      
      L             := Get_Listeners (Src1);
      Found1 := False;
      Found2 := False;
      Found3 := False;
      Found4 := False;
      for Item of L loop
	 if Listen_Ptr (Item.Listener) = Listen_Ptr (Listen1) 
	   and then Item.Evt_Type = Event1 then
	    Found1 := True;
	 end if;
	 if Listen_Ptr (Item.Listener) = Listen_Ptr (Listen1) 
	   and then Item.Evt_Type = Event2 then
	    Found2 := True;
	 end if;
	 
	 if Listen_Ptr (Item.Listener) = Listen_Ptr (Listen1) 
	   and then Item.Evt_Type /= Event1 
	   and then  Item.Evt_Type /= Event2 then
	    Found3 := True;
	 end if;
      end loop;
      
      Assert 
	(Found1 and Found2 and not Found3 and not Found4,
	 "Test_Add_Listener (3) : testing OK " &
	   " Found 1 => " & Found1'Img &
	   " Found 2 => " & Found2'Img &
	   " Found 3 => " & Found3'Img &
	   " Found 4 => " & Found4'Img);
      
      -- Add Listen 3 abd Listen 4
      
      Add_Listener
	(Event_Source => Src1,
	 Evt_type     => Event3,
	 Listener     => Listen3);
      
      Add_Listener
	(Event_Source => Src1,
	 Evt_type     => Event4,
	 Listener     => Listen4);
      
      L             := Get_Listeners (Src1);
      Found1 := False;
      Found2 := False;
      Found3 := False;
      Found4 := False;
      Nb := 0;
      for Item of L loop
	 if Listen_Ptr (Item.Listener) = Listen_Ptr (Listen1) then
	    Found1 := True;
	 end if;
	 if Listen_Ptr (Item.Listener) = Listen_Ptr (Listen2) then
	    Found2 := True;
	 end if;
	 if Listen_Ptr (Item.Listener) = Listen_Ptr (Listen3) then
	    Found3 := True;
	 end if;
	 if Listen_Ptr (Item.Listener) = Listen_Ptr (Listen4) then
	    Found4 := True;
	 end if;
      end loop;
      
      Assert 
	(Found1 and Found2 and Found3 and Found4,
	 "Test_Add_Listener (4) : testing OK " &
	   " Found 1 => " & Found1'Img &
	   " Found 2 => " & Found2'Img &
	   " Found 3 => " & Found3'Img &
	   " Found 4 => " & Found4'Img);
      
      
      -- Remove a Listener 1 for Event 2
      
      Remove_Listener
	(Event_Source => Src1,
	 Evt_type     => Event2,
	 Listener     => Listen1);
      
      L      := Get_Listeners (Src1);
      Found1 := False;
      Found2 := False;
      Found3 := False;
      Found4 := False;
      for Item of L loop
	 if Listen_Ptr (Item.Listener) = Listen_Ptr (Listen1) 
	   and then Item.Evt_Type = Event1 then
	    Found1 := True;
	 end if;
	 if Listen_Ptr (Item.Listener) = Listen_Ptr (Listen1) 
	   and then Item.Evt_Type /= Event1 then
	    Found2 := True;
	 end if;
      end loop;
      
      Assert 
	(Found1 and not Found2 and not Found3 and not Found4,
	 "Test_Add_Listener (5) : testing OK " &
	   " Found 1 => " & Found1'Img &
	   " Found 2 => " & Found2'Img &
	   " Found 3 => " & Found3'Img &
	   " Found 4 => " & Found4'Img);
      
      -- Remove a Listener 1 for Event 1 no more Listen 1
      
      Remove_Listener
	(Event_Source => Src1,
	 Evt_type     => Event1,
	 Listener     => Listen1);
      
      L      := Get_Listeners (Src1);
      Found1 := False;
      Found2 := False;
      Found3 := False;
      Found4 := False;
      for Item of L loop
	 if Listen_Ptr (Item.Listener) = Listen_Ptr (Listen1) then
	    Found1 := True;
	 end if;
      end loop;
      
      Assert 
	(not Found1 and not Found2 and not Found3 and not Found4,
	 "Test_Add_Listener (6) : testing OK " &
	   " Found 1 => " & Found1'Img &
	   " Found 2 => " & Found2'Img &
	   " Found 3 => " & Found3'Img &
	   " Found 4 => " & Found4'Img);
      
      -- Add Listen 1 for Event 1 and Event 2 and romove all at once
      
      Add_Listener
	(Event_Source => Src1,
	 Evt_type     => Event1,
	 Listener     => Listen1);
      
      Add_Listener
	(Event_Source => Src1,
	 Evt_type     => Event2,
	 Listener     => Listen1);
      
      Remove_Listener
	(Event_Source => Src1,
	 Evt_type     => No_Event,
	 Listener     => Listen1);
      
      L      := Get_Listeners (Src1);
      Found1 := False;
      Found2 := False;
      Found3 := False;
      Found4 := False;
      for Item of L loop
	 if Listen_Ptr (Item.Listener) = Listen_Ptr (Listen1) 
	   and then Item.Evt_Type = Event1 then
	    Found1 := True;
	 end if;
	 if Listen_Ptr (Item.Listener) = Listen_Ptr (Listen1) 
	   and then Item.Evt_Type = Event2 then
	    Found2 := True;
	 end if;
      end loop;
      
      Assert 
	(not Found1 and not Found2 and not Found3 and not Found4,
	 "Test_Add_Listener (7) : testing OK " &
	   " Found 1 => " & Found1'Img &
	   " Found 2 => " & Found2'Img &
	   " Found 3 => " & Found3'Img &
	   " Found 4 => " & Found4'Img);
      
   end Test_Add_Listener;
   
   ---------------------
   -- Remove_Listener --
   ---------------------
   
   procedure Test_Remove_Listener
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      --  Done in Test_Add_Listener
      Assert 
	(True, "Test_Remove_Listener : testing OK ");
   end Test_Remove_Listener;
   
   ----------------
   -- Fire_Event --
   ----------------
   
   procedure Test_Fire_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Listen1 : access Listen_Record'Class
	:= new Listen_Record'(No_Listen_Record);
      Listen2 : access Listen_Record'Class
	:= new Listen_Record'(No_Listen_Record);
      Listen3 : access Listen_Record'Class
	:= new Listen_Record'(No_Listen_Record);
      Listen4 : access Listen_Record'Class
	:= new Listen_Record'(No_Listen_Record);
      
      Src1 : access Event_Source_Record;
      Src2 : access Event_Source_Record;
      
      Evt_Obj : Access Event_Object_Record;      
   begin
      --  Listen 1 waiting on Event 1
      
      Listen1.I := 1;
      Listen2.I := 2;
      Listen3.I := 3;
      Listen4.I := 4;
      
      Src1 := New_Event_Source;
      Src2 := New_Event_Source;
      
      Set_Sender (Src1, Src1);
      Set_Sender (Src2, Src2);
      
      Add_Listener
	(Event_Source => Src1,
	 Evt_type     => Event1,
	 Listener     => Listen1);
      
      Evt_Obj := New_Event_Object;
      Set_Event_Type (Evt_Obj, Event1);
      Set_Object (Evt_Obj, Src2);
      
      Fire_Event 
	(Event_Source  => Src1,
	 Event_Object  => Evt_Obj,
	 Sender_Source => Src1);
	
      Assert 
	(Listen1.Evt_Listen = Event1,
	 "Test_Fire_Event (1) : testing OK ");
   end Test_Fire_Event;
   
   --------------------
   -- Register_Tests --
   --------------------
   
   procedure Register_Tests (T : in out Test_Case) 
   is
      use Test_Cases, Test_Cases.Registration;
   begin
      Register_Routine
	(T, Test_Initialize'Access, "Initialize");
      
      Register_Routine 
	(T, Test_New_Event_Source'Access, "Test_New_Event_Source");
      
      Register_Routine
	(T, Test_Listeners'Access, "Test_Listeners");
      
      Register_Routine
	(T, Test_Set_Listeners'Access, "Test_Set_Listeners");
   
      Register_Routine
	(T, Test_Sender'Access, "Test_Sender");
   
      Register_Routine
	(T, Test_Set_Sender'Access, "Test_Set_Sender");
   
      Register_Routine
	(T, Test_Is_Events_Enabled'Access, "Test_Is_Events_Enabled");
   
      Register_Routine
	(T, Test_Set_Events_Enabled'Access, "Test_Set_Events_Enabled");
   
      Register_Routine
	(T, Test_Add_Listener'Access, "Test_Add_Listener");
   
      Register_Routine
	(T, Test_Remove_Listener'Access, "Test_Remove_Listener");
   
      Register_Routine
	(T, Test_Fire_Event'Access, "Test_Fire_Event");
   
   end Register_Tests;

   ----------
   -- Name --
   ----------
   
   function Name (T : Test_Case) return Message_String is
   begin
      return Format ("Artics.Events_Sources_Tests");
   end Name;


end Artics.Events_Sources_Tests;

