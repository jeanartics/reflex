with AUnit.Assertions; use AUnit.Assertions;

package body Artics.Matrices.Tests is
   
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
   
   ----------------
   -- New_Matrix --
   ----------------
   
   procedure Test_New_Matrix
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      This : access Matrix_Record;
   begin
      This := New_Matrix
	(Columns_Count => 10,
	 Lines_Count   => 5);
      
      Assert
	(This.Lines_Count = 5,
	 "Test_New_Matrix Lines_Count = 5 : testing Bad " &
	   " found = " & This.Lines_Count'Img);
      
      Assert
	(This.Columns_Count = 10,
	 "Test_New_Matrix Columns_Count = 5 : testing Bad " &
	   " found = " & This.Columns_Count'Img);
      
      for I in This.Lines'Range loop
	 for J in This.Lines (I)'Range loop
	    Assert
	      (This.Lines (I)(J) = Null_Item,
	       "Test_New_Matrix " & I'Img & "," & J'Img & " : testing Bad ");
	 end loop;
      end loop;
      
      Assert (True, "Test_New_Matrix : testing OK ");
   end Test_New_Matrix;
   
   --------------------
   -- Destroy_Matrix --
   --------------------
   
   procedure Test_Destroy_Matrix
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      This : access Matrix_Record;
   begin
      This := New_Matrix
	(Columns_Count => 10,
	 Lines_Count   => 5);
      
      Assert
	(This /= null, "Test_Destroy_Matrix This is not null : testing OK ");
      
      Destroy_Matrix (Matrix_Ptr (This));
      
      Assert
	(This = null, "Test_Destroy_Matrix This is null : testing OK ");
      
      Assert (True, "Test_Destroy_Matrix : testing OK ");
   end Test_Destroy_Matrix;
   
   -----------------
   -- Append_Line --
   -----------------
   
   procedure Test_Append_Line
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      This : access Matrix_Record;
   begin
      This := New_Matrix
	(Columns_Count => 10,
	 Lines_Count   => 5);
      
      Assert
	(This.Lines_Count = 5,
	 "Test_Append_Line_Count = 5 : testing Bad " &
	   " found = " & This.Lines_Count'Img);
      
      Assert
	(This.Columns_Count = 10,
	 "Test_Append_Line Columns_Count = 10 : testing Bad " &
	   " found = " & This.Columns_Count'Img);
      
      Append_Line (This);
      
      Assert
	(This.Lines_Count = 6,
	 "Test_Append_Line_Count = 6 changed : testing Bad " &
	   " found = " & This.Lines_Count'Img);
      
      Assert
	(This.Columns_Count = 10,
	 "Test_Append_Line Columns_Count = 5 not changed : testing Bad " &
	   " found = " & This.Columns_Count'Img);
      
      
      Assert (True, "Test_Append_Line : testing OK ");
   end Test_Append_Line;
   
   -------------------
   -- Append_Column --
   -------------------
   
   procedure Test_Append_Column
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      This : access Matrix_Record;
      L    : Natural;
   begin
      This := New_Matrix
	(Columns_Count => 10,
	 Lines_Count   => 5);
      
      Assert
	(This.Lines_Count = 5,
	 "Test_Append_Column = 5 : testing Bad " &
	   " found = " & This.Lines_Count'Img);
      
      Assert
	(This.Columns_Count = 10,
	 "Test_Append_Column Columns_Count = 10 : testing Bad " &
	   " found = " & This.Columns_Count'Img);
      
      Append_Column (This);
      
      Assert
	(This.Lines_Count = 5,
	 "Test_Append_Column = 5 not changed : testing Bad " &
	   " found = " & This.Lines_Count'Img);
      
      Assert
	(This.Columns_Count = 11,
	 "Test_Append_Column Columns_Count = 11 : testing Bad " &
	   " found = " & This.Columns_Count'Img);
      
      for I in 0..This.Lines_Count - 1 loop
	 L := This.Lines (I)'Length;
	 Assert
	   (L = This.Columns_Count,
	    "Test_Append_Column expected 11 " &
	      " found " & L'Img & " for " & I'Img & " : testing OK ");
	 
	 Assert
	   (This.Lines (I)(This.Columns_Count - 1) = Null_item,
	    "Test_Append_Column last column is null item " &
	      " for " & I'Img & ": testing OK ");
      end loop;
      
      
      Assert (True, "Test_Append_Column : testing OK ");
   end Test_Append_Column;
   
   ------------------------
   -- Insert_Line_Before --
   ------------------------
   
   procedure Test_Insert_Line_Before
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert (True, "Test_Insert_Line_Before : testing OK ");
   end Test_Insert_Line_Before;
   
   --------------------------
   -- Insert_Column_Before --
   --------------------------
   
   procedure Test_Insert_Column_Before
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert (True, "Test_Insert_Column_Before : testing OK ");
   end Test_Insert_Column_Before;
   
   -----------------------
   -- Insert_Line_After --
   -----------------------
   
   procedure Test_Insert_Line_After
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert (True, "Test_Insert_Line_After : testing OK ");
   end Test_Insert_Line_After;
   
   -------------------------
   -- Insert_Column_After --
   -------------------------
   
   procedure Test_Insert_Column_After
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert (True, "Test_Insert_Column_After : testing OK ");
   end Test_Insert_Column_After;
   
   -----------------
   -- Remove_Line --
   -----------------
   
   procedure Test_Remove_Line
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert (True, "Test_Remove_Line : testing OK ");
   end Test_Remove_Line;
   
   -------------------
   -- Remove_Column --
   -------------------
   
   procedure Test_Remove_Column
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert (True, "Test_Remove_Column : testing OK ");
   end Test_Remove_Column;
   
   -----------------
   -- Lines_Count --
   -----------------
   
   procedure Test_Get_Lines_Count
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      This : access Matrix_Record;
   begin
      This := New_Matrix
	(Columns_Count => 10,
	 Lines_Count   => 5);
      
      Assert
	(This.Get_Lines_Count = 5,
	 "Test_Get_Lines_Count = 5 : testing Bad " &
	   " found = " & This.Get_Lines_Count'Img);
      
      Assert (True, "Test_Get_Lines_Count : testing OK ");
   end Test_Get_Lines_Count;
   
   -------------------
   -- Columns_Count --
   -------------------
   
   procedure Test_Get_Columns_Count
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      This : access Matrix_Record;
   begin
      This := New_Matrix
	(Columns_Count => 10,
	 Lines_Count   => 5);

      Assert
	(This.Get_Columns_Count = 10,
	 "Test_Get_Columns_Count = 10 : testing Bad " &
	   " found = " & This.Get_Columns_Count'Img);
      
      Assert (True, "Test_Get_Columns_Count : testing OK ");
   end Test_Get_Columns_Count;
   
   --------------
   -- Get_Item --
   --------------
   
   procedure Test_Get_Item
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      This  : access Matrix_Record;
      Item  : Integer;
      Value : Integer;
   begin
      This := New_Matrix
	(Columns_Count => 10,
	 Lines_Count   => 5);
      
      for I in 0..This.Lines_Count - 1 loop
	 for J in 0..This.Columns_Count - 1 loop
	    Value := Integer (I * 100 + J);
	    This.Lines (I)(J) := Value;
	 end loop;
      end loop;
      
      for I in 0..This.Lines_Count - 1 loop
	 for J in 0..This.Columns_Count - 1 loop
	    Item := This.Get_Item (I, J);
	    Value := Integer (I * 100 + J);
	    Assert
	      (Item = Value,
	       "Test_Get_Item Value " & Value'Img & " : testing OK " &
	      " found : " & Item'Img);
	 end loop;
      end loop;
      
      
      Assert (True, "Test_Get_Item : testing OK ");
   end Test_Get_Item;
   
   --------------
   -- Set_Item --
   --------------
   
   procedure Test_Set_Item
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      This  : access Matrix_Record;
      Item  : Integer;
      Value : Integer;
   begin
      This := New_Matrix
	(Columns_Count => 10,
	 Lines_Count   => 5);
      
      for I in 0..This.Lines_Count - 1 loop
	 for J in 0..This.Columns_Count - 1 loop
	    Value := Integer (I * 100 + J);
	    This.Set_Item (I, J, Value);
	 end loop;
      end loop;
      
      for I in 0..This.Lines_Count - 1 loop
	 for J in 0..This.Columns_Count - 1 loop
	    Item := This.Lines (I)(J);
	    Value := Integer (I * 100 + J);
	    Assert
	      (Item = Value,
	       "Test_Set_Item Value " & Value'Img & " : testing OK " &
	      " found : " & Item'Img);
	 end loop;
      end loop;
      

      Assert (True, "Test_Set_Item : testing OK ");
   end Test_Set_Item;
   
   --------------------
   -- Register_Tests --
   --------------------
   
   procedure Register_Tests (T : in out Test_Case) 
   is
      use Test_Cases, Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Initialize'Access, "Initialize");
      
      Register_Routine
	(T, Test_New_Matrix'Access, "Test_New_Matrix");
      Register_Routine
	(T, Test_Destroy_Matrix'Access, "Test_Destroy_Matrix");
      Register_Routine
	(T, Test_Append_Line'Access, "Test_Append_Line");
      Register_Routine
	(T, Test_Append_Column'Access, "Test_Append_Column");
      Register_Routine
	(T, Test_Insert_Line_Before'Access, "Test_Insert_Line_Before");
      Register_Routine
	(T, Test_Insert_Column_Before'Access, "Test_Insert_Column_Before");
      Register_Routine
	(T, Test_Insert_Line_After'Access, "Test_Insert_Line_After");
      Register_Routine
	(T, Test_Insert_Column_After'Access, "Test_Insert_Column_After");
      Register_Routine
	(T, Test_Remove_Line'Access, "Test_Remove_Line");
      Register_Routine
	(T, Test_Remove_Column'Access, "Test_Remove_Column");
      Register_Routine
	(T, Test_Get_Lines_Count'Access, "Test_Get_Lines_Count");
      Register_Routine
	(T, Test_Get_Columns_Count'Access, "Test_Get_Columns_Count");
      Register_Routine
	(T, Test_Get_Item'Access, "Test_Get_Item");
      Register_Routine
	(T, Test_Set_Item'Access, "Test_Set_Item");
   
   end Register_Tests;

   ----------
   -- Name --
   ----------
   
   function Name (T : Test_Case) return Message_String is
   begin
      return Format ("Artics.Matrices.Tests");
   end Name;


end Artics.Matrices.Tests;

