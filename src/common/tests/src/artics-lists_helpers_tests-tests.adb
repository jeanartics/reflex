with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_Io; use Ada.Text_Io;

package body Artics.Lists_Helpers_Tests.Tests is
   
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
   
   Zero   : Integer := 0;
   Un     : Integer := 1;
   Deux   : Integer := 2;
   Trois  : Integer := 3;
   Quatre : Integer := 4;
   Cinq   : Integer := 5;
   Six    : Integer := 6;
   Sept   : Integer := 7;
   Huit   : Integer := 8;
   Neuf   : Integer := 9;
   Dix    : Integer := 10;
   Onze   : Integer := 11;
   Douze  : Integer := 12;
   
   use Integers_Lists;
   use Integers_Lists_Helpers;
   
   L   : Integers_Lists.List;
   
   -----------------
   -- Create_List --
   -----------------
   
   procedure Create_Integer_List is
   begin
      L := Integers_Lists.Empty_List;
      
      Integers_Lists.Append (L, Un);
      Integers_Lists.Append (L, Deux);
      Integers_Lists.Append (L, Trois);
      Integers_Lists.Append (L, Quatre);
      Integers_Lists.Append (L, Cinq);
      Integers_Lists.Append (L, Six);
      Integers_Lists.Append (L, Sept);
      Integers_Lists.Append (L, Huit);
      Integers_Lists.Append (L, Neuf);
      Integers_Lists.Append (L, Dix);
      Integers_Lists.Append (L, Onze);
      Integers_Lists.Append (L, Douze);
   end Create_Integer_List;
   
   -----------------------
   -- Forward_Cursor_At --
   -----------------------
   
   procedure Test_Forward_Cursor_At 
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      N   : Integer;
      Cur : Integers_Lists.Cursor;
   begin
      Create_Integer_List;
      
      Cur := Forward_Cursor_At (L, 0);
      Assert
	(Cur = Integers_Lists.No_Element,
	 "Test_Forward_Cursor_At 0 : testing bad ");
	 
	 Cur := Forward_Cursor_At (L, 1);
      N := Integers_Lists.Element (Cur);
      Assert
	(N = Un,
	 "Test_Forward_Cursor_At UN : testing bad ");
      
      Cur := Forward_Cursor_At (L, 2);
      N := Integers_Lists.Element (Cur);
      Assert
	(N = Deux,
	 "Test_Forward_Cursor_At DEUX: testing bad ");
      
      Cur := Forward_Cursor_At (L, 3);
      N := Integers_Lists.Element (Cur);
      Assert
	(N = Trois,
	 "Test_Forward_Cursor_At Trois : testing bad ");
      
      Cur := Forward_Cursor_At (L, 4);
      N := Integers_Lists.Element (Cur);
      Assert
	(N = Quatre,
	 "Test_Forward_Cursor_At Quatre : testing bad ");
      
      Cur := Forward_Cursor_At (L, 5);
      N := Integers_Lists.Element (Cur);
      Assert
	(N = Cinq,
	 "Test_Forward_Cursor_At Cinq : testing bad ");
      
      Cur := Forward_Cursor_At (L, 6);
      N := Integers_Lists.Element (Cur);
      Assert
	(N = Six,
	 "Test_Forward_Cursor_At Six : testing bad ");
      
      Cur := Forward_Cursor_At (L, 7);
      N := Integers_Lists.Element (Cur);
      Assert
	(N = Sept,
	 "Test_Forward_Cursor_At Sept : testing bad ");
      
      Cur := Forward_Cursor_At (L, 8);
      N := Integers_Lists.Element (Cur);
      Assert
	(N = Huit,
	 "Test_Forward_Cursor_At Huit: testing bad ");
      
      Cur := Forward_Cursor_At (L, 9);
      N := Integers_Lists.Element (Cur);
      Assert
	(N = Neuf,
	 "Test_Forward_Cursor_At Neuf : testing bad ");
      
      Cur := Forward_Cursor_At (L, 10);
      N := Integers_Lists.Element (Cur);
      Assert
	(N = Dix,
	 "Test_Forward_Cursor_At Dix : testing bad ");
      
      Cur := Forward_Cursor_At (L, 11);
      N := Integers_Lists.Element (Cur);
      Assert
	(N = Onze,
	 "Test_Forward_Cursor_At Onze : testing bad ");
      
      Cur := Forward_Cursor_At (L, 12);
      N := Integers_Lists.Element (Cur);
      Assert
	(N = Douze,
	 "Test_Forward_Cursor_At Douze : testing bad ");
      
      Cur := Forward_Cursor_At (L, 13);
      Assert
	(Cur = Integers_Lists.No_Element,
	 "Test_Forward_Cursor_At 13 : testing bad ");
      
      Assert (True, "Test_Forward_Cursor_At : testing bad ");
   end Test_Forward_Cursor_At;
   
   ------------
   -- Get_At --
   ------------
   
   procedure Test_Get_At 
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      N : Integer;
   begin
      Create_Integer_List;
      
      N:= Get_At (L, 0);
      Assert
	(N = No_Elmt,
	 "Test_Get_At 0 : testing bad ");
	 
      N := Get_At (L, 1);
      Assert
	(N = Un,
	 "Test_Get_At UN : testing bad ");
      
      N := Get_At (L, 2);
      Assert
	(N = Deux,
	 "Test_Get_At DEUX: testing bad ");
      
      n := Get_At (L, 3);
      Assert
	(N = Trois,
	 "Test_Get_At Trois : testing bad ");
      
      N := Get_At (L, 4);
      Assert
	(N = Quatre,
	 "Test_Get_At Quatre : testing bad ");
      
      N := Get_At (L, 5);
      Assert
	(N = Cinq,
	 "Test_Get_At Cinq : testing bad ");
      
      N := Get_At (L, 6);
      Assert
	(N = Six,
	 "Test_Get_At Six : testing bad ");
      
      N := Get_At (L, 7);
      Assert
	(N = Sept,
	 "Test_Get_At Sept : testing bad ");
      
      N := Get_At (L, 8);
      Assert
	(N = Huit,
	 "Test_Get_At Huit: testing bad ");
      
      N := Get_At (L, 9);
      Assert
	(N = Neuf,
	 "Test_Get_At Neuf : testing bad ");
      
      N := Get_At (L, 10);
      Assert
	(N = Dix,
	 "Test_Get_At Dix : testing bad ");
      
      N := Get_At (L, 11);
      Assert
	(N = Onze,
	 "Test_Get_At Onze : testing bad ");
      
      N := Get_At (L, 12);
      Assert
	(N = Douze,
	 "Test_Get_At Douze : testing bad ");
      
      N := Get_At (L, 13);
      Assert
	(N = No_Elmt,
	 "Test_Get_At 13 : testing bad ");

      Assert (True, "Test_Get_At : testing bad ");
   end Test_Get_At;
   
   ---------------
   -- Insert_At --
   ---------------
   
   procedure Test_Insert_At
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      N : Integer;
   begin
      Create_Integer_List;
      
      Insert_At (L, 0, 100);
      Assert
	(Get_At (L, 1) = 100,
	 "Test_Insert_At pos 0 : testing bad ");
      
      Insert_At (L, 1, 101);
      Assert
	(Get_At (L, 1) = 101,
	 "Test_Insert_At pos 1 : testing bad ");
      
      Insert_At (L, 5, 105);
      Assert
	(Get_At (L, 5) = 105,
	 "Test_Insert_At pos 5 : testing bad ");
      
      Insert_At (L, 5, 1055);
      Assert
	(Get_At (L, 5) = 1055,
	 "Test_Insert_At pos 5 again : testing bad ");
      
      N := Integer (Integers_Lists.Length (L));
      Assert
	(N = 16,
	 "Test_Insert_At length is now 16 : testing bad found " & N'Img);
      
      Insert_At (L, N, 26);
      N := Get_At (L, Integer (Integers_Lists.Length (L)));
      Assert
	(N = 12,
	 "Test_Insert_At pos last : testing bad value = " & N'Img);
      
      Assert (True, "Test_Insert_At : testing bad ");
   end Test_Insert_At;
   
   ---------------
   -- Get_First --
   ---------------
   
   procedure Test_Get_First
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      -- Create the list to have a clean list
      Create_Integer_List;
      
      Assert
	(Get_First (L) =  Un,
	 "Test_Get_First must be Un : testing bad ");
      
      Insert_At (L, 0, 100);
      Assert
	(Get_At (L, 1) = 100,
	 "Test_Get_First add 100 at pos 1 : testing bad ");
      
      Assert
	(Get_First (L) =  100,
	 "Test_Get_First must be Un : testing bad ");
      
      Assert (True, "Test_Get_First : testing bad ");
   end Test_Get_First;
   
   ----------------
   -- Get_Second --
   ----------------
   
   procedure Test_Get_Second
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      -- Create the list to have a clean list
      Create_Integer_List;
      Assert
	(Get_Second (L) = Deux,
	 "Test_Get_Second is Deux : testing bad ");
      
      Assert (True, "Test_Get_Second : testing bad ");
   end Test_Get_Second;
   
   --------------
   -- Get_Last --
   --------------
   
   procedure Test_Get_Last
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Create_Integer_List;
      Assert
	(Get_Last (L) = 12,
	 "Test_Get_Last is 12 : testing bad ");
      
      Integers_Lists.Append (L, 200);
      Assert
	(Get_Last (L) = 200,
	 "Test_Get_Last is 200 : testing bad ");
      
      
      Assert (True, "Test_Get_Last : testing bad ");
   end Test_Get_Last;
   
   ------------------------
   -- Get_Last_Minus_One --
   ------------------------
   
   procedure Test_Get_Last_Minus_One
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      -- Create the list to have a clean list
      Create_Integer_List;
      
      Assert
	(Get_Last_Minus_One (L) = Onze,
	 "Test_Get_Last_Minus_One is 12 : testing bad ");
      
      Insert_At (L, Integer (Integers_Lists.Length (L)), 100);
      Assert
	(Get_Last_Minus_One (L) = 100,
	 "Test_Get_First add 100 and then last minus one is 100: testing bad ");
      
      
      Assert (True, "Test_Get_Last_Minus_One : testing bad ");
   end Test_Get_Last_Minus_One;
   
   -----------------
   -- Append_List --
   -----------------
   
   procedure Test_Append_List 
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      L1 : Integers_Lists.List;
      N  : Integer;
   begin
      Create_Integer_List;
      
      Integers_Lists.Append (L1, 100);
      Integers_Lists.Append (L1, 101);
      Integers_Lists.Append (L1, 102);
      Integers_Lists.Append (L1, 103);
      
      Append_List (L, L1);
	
      N := Get_At (L, 1);
      Assert
	(N = Un,
	 "Test_Append_List UN : testing bad ");
      
      N := Get_At (L, 2);
      Assert
	(N = Deux,
	 "Test_Append_List DEUX: testing bad ");
      
      n := Get_At (L, 3);
      Assert
	(N = Trois,
	 "Test_Append_List Trois : testing bad ");
      
      N := Get_At (L, 4);
      Assert
	(N = Quatre,
	 "Test_Append_List Quatre : testing bad ");
      
      N := Get_At (L, 5);
      Assert
	(N = Cinq,
	 "Test_Append_List Cinq : testing bad ");
      
      N := Get_At (L, 6);
      Assert
	(N = Six,
	 "Test_Append_List Six : testing bad ");
      
      N := Get_At (L, 7);
      Assert
	(N = Sept,
	 "Test_Append_List Sept : testing bad ");
      
      N := Get_At (L, 8);
      Assert
	(N = Huit,
	 "Test_Append_List Huit: testing bad ");
      
      N := Get_At (L, 9);
      Assert
	(N = Neuf,
	 "Test_Append_List Neuf : testing bad ");
      
      N := Get_At (L, 10);
      Assert
	(N = Dix,
	 "Test_Append_List Dix : testing bad ");
      
      N := Get_At (L, 11);
      Assert
	(N = Onze,
	 "Test_Append_List Onze : testing bad ");
      
      N := Get_At (L, 12);
      Assert
	(N = Douze,
	 "Test_Append_List Douze : testing bad ");
      
      N := Get_At (L, 13);
      Assert
	(N = 100,
	 "Test_Append_List 100 : testing bad ");
      
      N := Get_At (L, 14);
      Assert
	(N = 101,
	 "Test_Append_List 101 : testing bad ");
      
      N := Get_At (L, 15);
      Assert
	(N = 102,
	 "Test_Append_List 102 : testing bad ");
      
      N := Get_At (L, 16);
      Assert
	(N = 103,
	 "Test_Append_List 103 : testing bad ");
      
      N := Get_At (L, 17);
      Assert
	(N = No_Elmt,
	 "Test_Append_List at pos 1è is No_Elmt : testing bad ");
      
      Assert (True, "Test_Append_List : testing bad ");
   end Test_Append_List;
   
 
   --------------------
   -- Register_Tests --
   --------------------
   
   procedure Register_Tests (T : in out Test_Case) 
   is
      use Test_Cases, Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Initialize'Access, "Initialize");
      
      Register_Routine
	(T, Test_Forward_Cursor_At'Access, "Test_Forward_Cursor_At");
      Register_Routine
	(T, Test_Get_At'Access, "Test_Get_At");
      Register_Routine
	(T, Test_Insert_At'Access, "Test_Insert_At");
      Register_Routine
	(T, Test_Get_First'Access, "Test_Get_First");
      Register_Routine
	(T, Test_Get_Second'Access, "Test_Get_Second");
      Register_Routine
	(T, Test_Get_Last'Access, "Test_Get_Last");
      Register_Routine
	(T, Test_Get_Last_Minus_One'Access, "Test_Get_Last_Minus_One");
      Register_Routine
	(T, Test_Append_List'Access, "Test_Append_List");
      
   end Register_Tests;

   ----------
   -- Name --
   ----------
   
   function Name (T : Test_Case) return Message_String is
   begin
      return Format ("Artics.Lists_Helpers.Tests");
   end Name;


end Artics.Lists_Helpers_Tests.Tests;

