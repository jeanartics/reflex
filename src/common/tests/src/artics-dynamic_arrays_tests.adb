with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_Io; use Ada.Text_Io;
with Artics.Utils; use Artics.Utils;

package body Artics.Dynamic_Arrays_Tests is
   
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
   
   -------------------------
   -- Test_Integer_Arrays --
   -------------------------

   procedure Test_Integer_Arrays
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      package Integer_Arrays is new Artics.Dynamic_Arrays (Integer, 0);
      use Integer_Arrays;
      subtype Integer_Array is Integer_Arrays.Dynamic_Array;
      subtype Integer_Array_Ptr is Integer_Arrays.Dynamic_Array_Ptr;
      
      Ints : Integer_Array_Ptr;
      Dest : Integer_Array_Ptr;
   begin
      Ints := Integer_Arrays.New_Dynamic_Array (10);
      
      for I in Ints'Range loop
	 Assert (ints (I) = 0, "Test_Integer_Arrays 'init No_Element)");
      end loop;
      
      for I in Ints'Range loop
	 ints (I) := I;
      end loop;
      
      for I in Ints'Range loop
	 Assert (Ints (I) = I, "Test_Integer_Arrays (Test1) => I " & I'Img);
      end loop;
      
      -- Resize the array
      
      Integer_Arrays.Resize (Ints, 20);
      
      Assert (Ints'Length = 20, "Test_Integer_Arrays (Test2) => 20 elements");
      
      for I in Integer range 0..9 loop
	 Assert (Ints (I) = I, "Test_Integer_Arrays (Test1) => I " & I'Img);
      end loop;
      
      Integer_Arrays.Resize (Ints, 5);
      
      Assert (Ints'Length = 5, "Test_Integer_Arrays (Test2) =< 20 elements");
      
      for I in Ints'Range loop
	 Assert
	   (Ints (I) = I, "Test_Integer_Arrays (5 elements ) => I " & I'Img);
      end loop;
      
      Integer_Arrays.Resize (Ints, 10);
      for I in Ints'Range loop
	 Ints (I) := Integer (I);
      end loop;
      
      Dest := Integer_Arrays.New_Dynamic_Array (10);
      
      Integer_Arrays.Copy (Ints, 2, Dest, 3, 5);
      
      Assert
        ((Dest (0) = 0)
         and (Dest (1) = 0)
         and (Dest (2) = 0)
         and (Dest (3) = 2)
         and (Dest (4) = 3)
         and (Dest (5) = 4)
         and (Dest (6) = 5) 
         and (Dest (7) = 6)
         and (Dest (8) = 0)
         and (Dest (9) = 0),
         "Test_Integer_Arrays (Copy 1) : testing OK " &
           " 0 => " & Integer_To_String (Dest (0)) & ", " &
           " 1 => " & Integer_To_String (Dest (1)) & ", " &
           " 2 => " & Integer_To_String (Dest (2)) & ", " &
           " 3 => " & Integer_To_String (Dest (3)) & ", " &
           " 4 => " & Integer_To_String (Dest (4)) & ", " &
           " 5 => " & Integer_To_String (Dest (5)) & ", " &
           " 6 => " & Integer_To_String (Dest (6)) & ", " &
           " 7 => " & Integer_To_String (Dest (7)) & ", " &
           " 8 => " & Integer_To_String (Dest (8)) & ", " &
           " 9 => " & Integer_To_String (Dest (9)));
      
      Dest := Integer_Arrays.New_Dynamic_Array (10);
      
      Integer_Arrays.Copy (Ints, 2, Dest, 3, 10);
      
      Assert (Dest'Length = 10, "Test_Integer_Arrays (Copy Length) : testing OK ");
      Assert
        ((Dest (0) = 0)
         and (Dest (1) = 0)
         and (Dest (2) = 0)
         and (Dest (3) = 2)
         and (Dest (4) = 3)
         and (Dest (5) = 4)
         and (Dest (6) = 5) 
         and (Dest (7) = 6)
         and (Dest (8) = 7)
         and (Dest (9) = 8),
         "Test_Integer_Arrays (Copy 2) : testing OK " &
           " 0 => " & Integer_To_String (Dest (0)) & ", " &
           " 1 => " & Integer_To_String (Dest (1)) & ", " &
           " 2 => " & Integer_To_String (Dest (2)) & ", " &
           " 3 => " & Integer_To_String (Dest (3)) & ", " &
           " 4 => " & Integer_To_String (Dest (4)) & ", " &
           " 5 => " & Integer_To_String (Dest (5)) & ", " &
           " 6 => " & Integer_To_String (Dest (6)) & ", " &
           " 7 => " & Integer_To_String (Dest (7)) & ", " &
           " 8 => " & Integer_To_String (Dest (8)) & ", " &
           " 9 => " & Integer_To_String (Dest (9)));
      
      Ints := Integer_Arrays.New_Dynamic_Array (5);
      for I in Ints'Range loop
	 ints (I) := I;
      end loop;
      Dest := Integer_Arrays.New_Dynamic_Array (10);
      
      Integer_Arrays.Copy (Ints, 2, Dest, 3, 5);
      
      Assert (Dest'Length = 10, "Test_Integer_Arrays (Copy Length 3) : testing OK ");
      Assert
        ((Dest (0) = 0)
         and (Dest (1) = 0)
         and (Dest (2) = 0)
         and (Dest (3) = 2)
         and (Dest (4) = 3)
         and (Dest (5) = 4)
         and (Dest (6) = 0) 
         and (Dest (7) = 0)
         and (Dest (8) = 0)
         and (Dest (9) = 0),
         "Test_Integer_Arrays (Copy 3) : testing OK " &
           " 0 => " & Integer_To_String (Dest (0)) & ", " &
           " 1 => " & Integer_To_String (Dest (1)) & ", " &
           " 2 => " & Integer_To_String (Dest (2)) & ", " &
           " 3 => " & Integer_To_String (Dest (3)) & ", " &
           " 4 => " & Integer_To_String (Dest (4)) & ", " &
           " 5 => " & Integer_To_String (Dest (5)) & ", " &
           " 6 => " & Integer_To_String (Dest (6)) & ", " &
           " 7 => " & Integer_To_String (Dest (7)) & ", " &
           " 8 => " & Integer_To_String (Dest (8)) & ", " &
           " 9 => " & Integer_To_String (Dest (9)));
      
      
      
      Assert (True, "Test_Integer_Arrays : testing OK ");
   end Test_Integer_Arrays;
   
   --------------------
   -- Register_Tests --
   --------------------
   
   procedure Register_Tests (T : in out Test_Case) 
   is
      use Test_Cases, Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Initialize'Access, "Initialize");
      
      Register_Routine (T, Test_Integer_Arrays'Access, "Test_Integer_Arrays");
      
   end Register_Tests;

   ----------
   -- Name --
   ----------
   
   function Name (T : Test_Case) return Message_String is
   begin
      return Format ("Artics.Dynamic_Arrays_Tests");
   end Name;


end Artics.Dynamic_Arrays_Tests;

