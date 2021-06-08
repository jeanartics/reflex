------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
--                                                                          --
-- Reflex is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public Licensea as published  by the Free Soft- --
-- ware Foundation; either version 3, or (at your option) any later version --
-- Reflex is distributed in the hope that it will be useful, but WITH-      --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License distributed with Reflex; see file COPYING3. If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- Reflex is originally developed  by the Artics team at Grenoble.          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_Io; use Ada.Text_Io;

with Ada.Unchecked_Deallocation;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;

with Atree; use ATree;
with Types; use Types;
with Einfo; use Einfo;
with Namet; use Namet;
with Nlists; use Nlists;
with Nmake; use Nmake;
with Sinfo; use Sinfo;
with Sem_Aux; use Sem_Aux;
with Sem_Util; use Sem_Util;
with Sem_Eval; use Sem_Eval;
with Snames; use Snames;
with Tbuild; use Tbuild;
with Errout; use Errout;
with Stand; use Stand;

with Reflex.Infos; use Reflex.Infos;
with Reflex.Expanders.Utils; use Reflex.Expanders.Utils;
with Reflex.External_Names; use Reflex.External_Names;

package body Reflex.Global_Arecs is
   
   -----------------------
   -- Same_Global_Assoc --
   -----------------------
   
   function Same_Global_Assoc (A1, A2 : Global_Assoc_Ptr) return Boolean is
   begin
      return A1.Original = A2.Original;
   end Same_Global_Assoc;
   
   ---------------------
   -- Push_Call_Stack --
   ---------------------
   
   procedure Push_Call_Stack (E : Entity_Id) is
   begin
      pragma Assert (Last_Call_Stack < Max_Arec_Stack_Entry);
      Last_Call_Stack := Last_Call_Stack + 1;
      Arec_Call_Stack (Last_Call_Stack) := E;
   end Push_Call_Stack;
   
   --------------------
   -- Pop_Call_Stack --
   --------------------
   
   procedure Pop_Call_Stack is
   begin
      pragma Assert (Last_Call_Stack > 0);
      Last_Call_Stack := Last_Call_Stack - 1;
   end Pop_Call_Stack;
   
   ----------------------
   -- New_Global_Assoc --
   ----------------------
   
   function New_Global_Assoc (E, New_E : Entity_Id) return Global_Assoc_Ptr is
   begin
      return new Global_Assoc_Record'(E, New_E, Read_Write, As_Reference);
   end New_Global_Assoc;
   
   -----------------------
   -- Free_Global_Assoc --
   -----------------------
   
   procedure Free_Global_Assoc (This : in out Global_Assoc_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Global_Assoc_Record, Global_Assoc_Ptr);
   begin
      if This /= null then
	 Free (This);
      end if;
   end Free_Global_Assoc;
   
   -----------------------
   -- Same_Called_Assoc --
   -----------------------
   
   function Same_Called_Assoc (A1, A2 : Called_Assoc_Ptr) return Boolean is
   begin
      return A1.Entity = A2.Entity;
   end Same_Called_Assoc;
   
   ----------------------
   -- New_Called_Assoc --
   ----------------------
   
   function New_Called_Assoc
     (E    : Entity_Id;
      Inst : Name_Id) return Called_Assoc_Ptr is
   begin
      return new Called_Assoc_Record'
	(E, Inst, Empty, Reflex.Nodes_Lists.Empty_List);
   end New_Called_Assoc;
   
   -----------------------
   -- Free_Called_Assoc --
   -----------------------
   
   procedure Free_Called_Assoc (This : in out Called_Assoc_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Called_Assoc_Record, Called_Assoc_Ptr);
   begin
      if This /= null then
	 Free (This);
      end if;
   end Free_Called_Assoc;
   
   ----------------------
   -- Append_Call_Stmt --
   ----------------------
   
   procedure Append_Call_Stmt
     (Assoc : Called_Assoc_Ptr;
      Call  : Node_Id) is
       
   begin
      if Present (Call) then
	 Reflex.Nodes_Lists.Append (Assoc.Calls_Stmts, Call);
      end if;
   end Append_Call_Stmt;
   
   ---------------------
   -- New_Global_Arec --
   ---------------------
   
   function New_Global_Arec return Global_Arec_Ptr is
   begin
      return new Global_Arec_Record'(No_Global_Arec_Record);
   end New_Global_Arec;
   
   ----------------------
   -- Free_Global_Arec --
   ----------------------
   
   procedure Free_Global_Arec (This : in out Global_Arec_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Global_Arec_Record, Global_Arec_Ptr);
   begin
      if This /= null then
	 --  Free Globals
	 for A of This.Globals loop
	    Free_Global_Assoc (A);
	 end loop;
	 
	 Free (This);
      end if;
   end Free_Global_Arec;
   
   -------------------------
   -- Get_Subprogram_Mode --
   -------------------------
   
   function Get_Subprogram_Mode 
     (E : Entity_Id) return Subprogram_Generation_Mode is
      
      This : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
   begin
      return This.Subprogram_Mode;
   end Get_Subprogram_Mode;
   
   -------------------------
   -- Set_Subprogram_Mode --
   -------------------------
   
   procedure Set_Subprogram_Mode
     (E    : Entity_Id;
      Mode : Subprogram_Generation_Mode) is
      
      This : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
   begin
      This.Subprogram_Mode := Mode;
   end Set_Subprogram_Mode;
   
  -----------------------
  -- Get_Globals_Count --
  -----------------------
  
  function Get_Globals_Count (E : Entity_Id) return Natural is
      This : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
  begin
     return This.Globals_Count;
  end Get_Globals_Count;
    
  ----------------------
  -- Get_Called_Count --
  ----------------------
  
  function Get_Called_Count (E : Entity_Id) return Natural is
      This : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
  begin
     return This.Called_Count;
  end Get_Called_Count;
  
  ----------------------
  -- Get_Globals_List --
  ----------------------
  
  function Get_Globals_List (E : Entity_Id) return Globals_Assoc_Lists.List is
     
     This : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
  begin
     return This.Globals;
  end Get_Globals_List;
  
  ----------------------
  -- Set_Globals_List --
  ----------------------
  
  procedure Set_Globals_List
    (E       : Entity_Id;
     Globals : Globals_Assoc_Lists.List) is
     
     This : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
  begin
     This.Globals := Globals;
  end Set_Globals_List;
  
  ---------------------
  -- Get_Called_list --
  ---------------------
  
  function Get_Called_List (E : Entity_Id) return Called_Assoc_Lists.List is
      This : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
  begin
     return This.Called;
  end Get_Called_List;
  
  ---------------------
  -- Set_Called_List --
  ---------------------
  
  procedure Set_Called_List
    (E      : Entity_Id;
     Called : Called_Assoc_Lists.List) is
      This : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
  begin
     This.Called := Called;
  end Set_Called_List;
   
   -----------------
   -- Add_Globals --
   -----------------
   
   procedure Add_Globals
     (E             : Entity_Id;
      Global_Entity : Entity_Id) is
      
      This : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
      Cur  : Globals_Assoc_Lists.Cursor;
      A    : Global_Assoc_Ptr;
   begin
      Cur := Globals_Assoc_Lists.First (This.Globals);
      while Globals_Assoc_Lists.Has_Element (Cur) loop
	 A := Globals_Assoc_Lists.Element (Cur);
	 if A.Original = Global_Entity then
	    return;
         end if;
         Globals_Assoc_Lists.Next (Cur);
      end loop;
      
      Globals_Assoc_Lists.Append
	(This.Globals, New_Global_Assoc (Global_Entity, Empty));
   end Add_Globals;
   
   --------------------
   -- Remove_Globals --
   --------------------
   
   procedure Remove_Globals 
     (E             : Entity_Id;
      Global_Entity : Entity_Id) is
      
      This : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
      Cur  : Globals_Assoc_Lists.Cursor;
      A    : Global_Assoc_Ptr;
   begin
      Cur := Globals_Assoc_Lists.First (This.Globals);
      while Globals_Assoc_Lists.Has_Element (Cur) loop
	 A := Globals_Assoc_Lists.Element (Cur);
	 if A.Original = Global_Entity then
	    Globals_Assoc_Lists.Delete (This.Globals, Cur);
	    return;
	 end if;
	 Globals_Assoc_Lists.Next (Cur);
      end loop;
   end Remove_Globals;
   
   -----------------------
   -- Add_Called_Entity --
   -----------------------
   
   procedure Add_Called_Entity
     (E      : Entity_Id;
      Called : Entity_Id;
      Call   : Node_Id) is
      
      This : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
      C    : Called_Assoc_Ptr;
      Cur  : Called_Assoc_Lists.Cursor;
   begin
      Cur := Called_Assoc_Lists.First (This.Called);
      while Called_Assoc_Lists.Has_Element (Cur) loop
	 C := Called_Assoc_Lists.Element (Cur);
	 if C.Entity = Called then
	    if Present (Call) then
	       Append_Call_Stmt (C, Call);
	    end if;
	    return;
         end if;
         Called_Assoc_Lists.Next (Cur);
      end loop;
      
      C := New_Called_Assoc (Called, No_Name);
      if Present (Call) then
	 Append_Call_Stmt (C, Call);
      end if;
      
      Called_Assoc_Lists.Append (This.Called, C);
   end Add_Called_Entity;
   
   --------------------------
   -- Remove_Called_Entity --
   --------------------------
   
   procedure Remove_Called_Entity
     (E      : Entity_Id;
      Called : Entity_Id) is
      
      This : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
      C    : Called_Assoc_Ptr;
      Cur  : Called_Assoc_Lists.Cursor;
   begin
      Cur := Called_Assoc_Lists.First (This.Called);
      while Called_Assoc_Lists.Has_Element (Cur) loop
	 C := Called_Assoc_Lists.Element (Cur);
	 if C.Entity = Called then
	    Called_Assoc_Lists.Delete (This.Called, Cur);
	    return;
	 end if;
	 Called_Assoc_Lists.Next (Cur);
      end loop;
   end Remove_Called_Entity;
   
   ----------------------------------
   -- Create_Expander_Error_Enrity --
   ----------------------------------
   
   procedure Create_Arec_Error_Enrity is   
   begin
      Arec_Error_Entity := New_Entity (N_Defining_Identifier, No_Location);
      Set_Ekind (Arec_Error_Entity, E_Void);
      Set_Chars (Arec_Error_Entity, String_Find ("_error_"));
      Set_Etype (Arec_Error_Entity, Standard_Void_Type);
      Set_Scope (Arec_Error_Entity, Standard_Standard);
   end Create_Arec_Error_Enrity;
   
   
   ------------------------
   -- Get_Arec_Full_Type --
   ------------------------
   
   function Get_Arec_Full_Type (E : Entity_Id) return Entity_Id is
      This : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
   begin
      return This.Arec_Full_Type;
   end Get_Arec_Full_Type;
   
   -------------------
   -- Set_Arec_Type --
   -------------------
   
   procedure Set_Arec_Full_Type
     (E : Entity_Id;
      T : Entity_Id) is
      
      This : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
   begin
      This.Arec_Full_Type := T;
   end Set_Arec_Full_Type;
   
   -----------------------
   -- Get_Arec_Variable --
   -----------------------
   
   function Get_Arec_Variable (E : Entity_Id) return Entity_Id is
      This : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
   begin
      return This.Arec_Variable;
   end Get_Arec_Variable;
   
   -----------------------
   -- Set_Arec_Variable --
   -----------------------
   
   procedure Set_Arec_Variable
     (E : Entity_Id;
      V : Entity_Id) is
      This : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
   begin
      This.Arec_Variable := V;
   end Set_Arec_Variable;
   
   ---------------------------
   -- Get_Arec_Extra_Formal --
   ---------------------------
   
   function Get_Arec_Extra_Formal (E : Entity_Id) return Entity_Id is
      This : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
   begin
      return This.Arec_Extra_Formal;
   end Get_Arec_Extra_Formal;
   
   ---------------------------
   -- Set_Arec_Extra_Formal --
   ---------------------------
   
   procedure Set_Arec_Extra_Formal
     (E : Entity_Id;
      F : Entity_Id) is
      This : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
   begin
      This.Arec_Extra_Formal := F;
   end Set_Arec_Extra_Formal;
   
   -------------------------------
   -- Get_Associated_Entity_For --
   -------------------------------
   
   function Get_Associated_Entity_For
     (E : Entity_Id;
      G : Entity_Id) return Entity_Id is
      
      This : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
   begin
      for A of This.Globals loop
	 if A.Original = G then
	    return A.New_Entity;
	 end if;
      end loop;
      
      return Empty;
   end Get_Associated_Entity_For;
   
   --------------------------
   -- Get_Enclosing_Object --
   --------------------------

   function Get_Enclosing_Object (N : Node_Id) return Entity_Id is
   begin
      Put_Line ("Get_Enclosing_Object Begin");
      if Is_Entity_Name (N) then
	 Put_Line
	   ("Get_Enclosing_Object End " & Get_String (Chars (Entity (N))));
         return Entity (N);
      else
         case Nkind (N) is
            when N_Indexed_Component  =>
	       return Get_Enclosing_Object (Prefix (N));
	       
	    when N_Slice              |
	      N_Selected_Component =>
	       return Get_Enclosing_Object (Prefix (N));

            when N_Type_Conversion =>
               return Get_Enclosing_Object (Expression (N));

            when others =>
               return Empty;
         end case;
      end if;
   end Get_Enclosing_Object;
   
   -----------------------
   -- Need_Globals_Arec --
   -----------------------
   
   function Need_Globals_Arec (E : Entity_Id) return Boolean is
      
      use Globals_Assoc_Lists;
      
      This        : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
      Called_List : Called_Assoc_Lists.List;
   begin
      if Declare_Global_Arec (E) then
	 return True;
      end if;
      
      if not Is_Empty (This.Globals) then
	 Set_Declare_Global_Arec (E, True);
	 return True;
      end if;
      
      Called_List := Get_Called_List (E);
      for Call of Called_List loop
	 if Need_Globals_Arec (Call.Entity) then
	    Set_Declare_Global_Arec (E, True);
	    return True;
	 end if;
      end loop;
      
      return False;
   end Need_Globals_Arec;
   
   --------------------------
   -- Can_Global_As_Actual --
   --------------------------
   
   --  function Can_Global_As_Actual (E : Entity_Id) return Boolean is
      
   --     use Globals_Assoc_Lists;
      
   --     This        : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
   --     Called_List : Called_Assoc_Lists.List;
   --  begin
   --     --  Now Compute the called nneded
      
   --     Called_List := Get_Called_List (E);
   --     for Call of Called_List loop
   --  	 Called_Count := Can_Global_As_Actual (Call.Entity);
	 
   --  	 Count := Count + Called_Cound;
   --  	 if Count > Max_Actuals_Allowed then
   --  	    return False;
   --  	 end if;
   --     end loop;
      
   --     --  Count the number of formal
      
   --     Formal := First_Formal_With_Extras (Subp_Id);
   --     while Present (Formal) loop
   --        if Ekind (Formal) = E_In_Parameter then
   --           Count_In :=  Artics.Maths.Max(Count_In , Count_In_Out) + 1;
	    
   --        elsif Ekind (Formal) = E_In_Out_Parameter then
   --           Count_In_Out := Artics.Maths.Max(Count_In , Count_In_Out);
   --           Count_In_Out := Artics.Maths.Max(Count_Out , Count_In_Out) + 1;
	    
   --        elsif Ekind (Formal) = E_Out_Parameter then
   --           Count_Out := Artics.Maths.Max(Count_Out , Count_In_Out) + 1;
	    
   --           -- Never arise
   --        else
   --           raise Program_Error;
   --        end if;
	 
   --        Next_Formal_With_Extras (Formal);
   --     end loop;
      
   --     if Count_In_Out /= 0 then
   --  	 Count := Count + Count_In_Out;
   --     else
   --  	 Count := Count + Artics.Maths.Max(Count_In, Count_Out);
   --     end if;
      
   --     if Count > Max_Actuals_Allowed then
   --  	 return False;
   --     end if;
      
   --  end Can_Global_As_Actual;
   
   ---------------------
   -- Replace_Globals --
   ---------------------
   
   procedure Replace_Globals (E : Entity_Id) is
      
      use Globals_Assoc_Lists;
      
      This : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
   begin
      if not Is_Empty (This.Globals) then
	 	
	 --Create_Full_Type (E);
	 Replace_Globals_By_Deference (E);
	 
	 --  Count := Extra_Formal_Dfb_Count (E) + Natural (Length (This.Globals));
	 
	 --  if Count > Max_Unity_Dfb_Parameter_Count then	
	 --     Create_Global_Arec_Full_Type (This, E);
	 --     Replace_Globals_By_Deference (This, E);
	    
	 --  else
	 --     Create_Global_Arec_Parameter_Type (This, E);
	 --     Replace_Globals_By_Arec_Parameter (This, E);
	 --  end if;
	 
      end if;	      
   end Replace_Globals;
   
   ----------------------------------
   -- Replace_Globals_By_Deference --
   ----------------------------------
   
   procedure Replace_Globals_By_Deference (E : Entity_Id) is
      
      function Process (N : Node_Id) return Traverse_Result;
      
      -------------
      -- Process --
      -------------
      
      function Process (N : Node_Id) return Traverse_Result is
	 
	 Ent   : Entity_Id;
	 D     : Entity_Id;
	 Sel   : Node_Id;
	 Pref  : Node_Id;
	 Desig : Node_Id;
	 Deref : Node_Id;
      begin
	 if Nkind_In (N, N_Identifier, N_Expanded_Name) then
	    Ent := Entity (N);
	    if Present (Ent) then
	       D := Get_Associated_Entity_For (E, Ent);
	       if Present (D) then
		  Pref  := New_Occurrence_Of 
		    (Get_Arec_Extra_Formal (E), Sloc (N));
		  Desig := New_Occurrence_Of (D, Sloc (Ent));
		  
		  Sel := Make_Selected_Component
		    (Sloc          => Sloc (N),
		     Prefix        => Pref,
		     Selector_Name => Desig);
		  
		  Deref := Make_Explicit_Dereference
		    (Sloc => Sloc (N),
		     Prefix => Sel);
		  
		  Rewrite (N, Deref);
		  
		  return Skip;
		  
--  	       elsif Compile_Time_Known_Value (Ent) then
--  		 Rewrite (N, Expr_Value (Ent));
	       end if;
	    end if;
	 end if;
	 
	 return Ok;
      end Process;
      
      function Traverse is new Traverse_Func (Process => Process);
      
      Subp  : Node_Id;
      Dummy : Traverse_Final_Result;
   begin
      Subp := Subprogram_Body (E);
	 
      if Present (Subp) then
	 Dummy := Traverse (Handled_Statement_Sequence (Subp));
      end if;
      
   end Replace_Globals_By_Deference;
   
   -----------------------------
   -- Replace_Globals_By_Arec --
   -----------------------------
   
   procedure Replace_Globals_By_Arec (E : Entity_Id) is
      
      function Process (N : Node_Id) return Traverse_Result;
      
      -------------
      -- Process --
      -------------
      
      function Process (N : Node_Id) return Traverse_Result is
	 
	 Ent : Entity_Id;
	 Id  : Node_Id;
	 D   : Entity_Id;
      begin
	 if Nkind_In (N, N_Identifier, N_Expanded_Name) then
	    
	    Ent := Entity (N);
	    if Present (Ent) then
	       D := Get_Associated_Entity_For (E, Ent);
	       if Present (D) then
		  Id := New_Occurrence_Of (D, Sloc (N));
		  Rewrite (N, Id);
		  return Skip;
	       end if;
	    end if;
	 end if;
	 
	 return Ok;
      end Process;
      
      function Traverse is new Traverse_Func (Process => Process);
      
      Subp  : Node_Id;
      Dummy : Traverse_Final_Result;
   begin
      Subp := Subprogram_Body (E);
	 
      if Present (Subp) then
	 Dummy := Traverse (Handled_Statement_Sequence (Subp));
      end if;
   end Replace_Globals_By_Arec;
   
   --------------------------------
   -- Replace_Formals_By_Globals --
   --------------------------------
   
   procedure Replace_Formals_By_Globals (E : Entity_Id) is
      
      function Process (N : Node_Id) return Traverse_Result;
      
      -------------
      -- Process --
      -------------
      
      function Process (N : Node_Id) return Traverse_Result is
	 
	 Ent    : Entity_Id;
	 Id     : Node_Id;
	 Global : Entity_Id;
      begin
	 if Nkind_In (N, N_Identifier, N_Expanded_Name) then
	    
	    Ent := Entity (N);
	    if Present (Ent) then
	       if Ekind (Ent) = E_In_Parameter
		 or else Ekind (Ent) = E_In_Out_Parameter
		 or else Ekind (Ent) = E_Out_Parameter
	       then
		  Global := Get_Formal_Global (Ent);
		  if Present (Global) then
		     Id := New_Occurrence_Of (Global, Sloc (N));
		     Rewrite (N, Id);
		     return Skip;
		  end if;
	       end if;
	    end if;
	 end if;
	 
	 return Ok;
      end Process;
      
      function Traverse is new Traverse_Func (Process => Process);
      
      Subp  : Node_Id;
      Dummy : Traverse_Final_Result;
   begin
      Subp := Subprogram_Body (E);
	 
      if Present (Subp) then
	 Dummy := Traverse (Handled_Statement_Sequence (Subp));
      end if;
   end Replace_Formals_By_Globals;
   
   ---------------------
   -- Collect_Globals --
   ---------------------
   
   procedure Collect_Globals (E : Entity_Id) is
      
      function Process (N : Node_Id) return Traverse_Result;
      
      -------------
      -- Process --
      -------------
      
      function Process (N : Node_Id) return Traverse_Result is
	 Obj : Entity_Id;
	 -- Scp : Entity_Id;
      begin
	 Obj := Get_Enclosing_Object (N);
	 Put_Line ("After Enclose");
	 if Present (Obj) then
--              if not Plc_Library_Scope (Obj) 
--  	      and then
            if Is_Library_Level_Entity (Obj)
              and then (Ekind (Obj) = E_Variable 
                        or else Ekind (Obj) = E_Constant)
            then
	       if not Compile_Time_Known_Value (E) then
		  Add_Globals (E, Obj);
	       end if;
               return Skip;
            end if;
         end if;	 
         
         return Ok;
      end Process;
      
      function Traverse is new Traverse_Func (Process => Process);
      
      Subp  : Node_Id;
      Dummy : Traverse_Final_Result;
   begin
      if Ekind (E) = E_Procedure or else Ekind (E) = E_Function then
	 Subp := Subprogram_Body (E);
	 
	 if Present (Subp) then
	    Dummy := Traverse (Handled_Statement_Sequence (Subp));
	 end if;
      end if;
      
      declare
         Globals : Globals_Assoc_Lists.List;
      begin
	 Globals := Get_Globals_List (E);
	 for A of Globals loop
	    Put_Line ("A.E     =  " & Get_Name_String (Chars (A.Original)));
            Put_Line ("A.New_E =  " & Get_Name_String (Chars (A.New_Entity)));
         end loop;
      end;
   end Collect_Globals;
   
   -----------------------------
   -- Create_Access_To_Record --
   -----------------------------
   
   procedure Create_Access_To_Record (E : Entity_Id) is
      
      Access_Type : Node_Id;
      Def_Id      : Entity_Id;
      Nxt         : Entity_Id;
      S           : String := Get_Name_String (Chars (E)) & "_ptr";
   begin
      --  The name of the access type is the name of the record type with the
      --  suffix "_ptr";
      
      Def_Id := Make_Defining_Identifier
	(Sloc  => Sloc (E),
	 Chars => String_Find (S));
      
      --  Create the access Full Type for the entity record E
      
      Access_Type := Make_Full_Type_Declaration
	(Sloc                => Sloc (E),
	 Defining_Identifier => Def_Id,
	 Type_Definition     => 
	   Make_Access_To_Object_Definition
	   (Sloc             => Sloc (E),
	    All_Present      => True,
	    Subtype_Indication =>
	      New_Occurrence_Of (E, Sloc (E)),
	    Constant_Present => False));	   
      
      --  Set the entity attributes
      
      Set_Ekind (Def_Id, E_General_Access_Type);
      Set_Etype (Def_Id, Def_Id);
      Set_Scope (Def_Id, Scope (E));
      Set_Directly_Designated_Type (Def_Id, E);
      
      --  Link Full Type entity in the scope just after the Record entity E
      --  Note: The following code is not necessary for as we do not care 
      --  preserving link order for entity. Append_Entity should be sufficient.
      
      Nxt := Next_Entity (E);
      Set_Next_Entity (E, Def_Id);
      Set_Next_Entity (Def_Id, Nxt);
      
      if No (Next_Entity (Def_Id)) then
	 Set_Last_Entity (Scope (E), Def_Id);
      end if;
      
      --  Insert the Full Type Declaration just after the declaration of the 
      --  record type
      
      Insert_After (Parent (E), Access_Type);
      
      --  Associated the entity record type to the newly create enti of the 
      --  access Full Type
      
      Set_Access_To_Object_Defined (E, Def_Id);
      
   end Create_Access_To_Record;
   
   -------------------------------
   -- Globals_In_Arec_Full_Type --
   -------------------------------
   
   procedure Globals_In_Arec_Full_Type (E : Entity_Id) is
      
      Globals    : Globals_Assoc_Lists.List; 
      Frst       : Entity_Id;
      Lst        : Entity_Id;
      Original   : Entity_Id;
      Gtyp       : Entity_Id;
      Comp_Def   : Node_Id;
      Attr       : Node_Id;
      Comp_Id    : Node_id;
      Comp_Dec   : Node_Id;
      Arec_Type  : Entity_Id;
      Comp_List  : List_Id;
      Init       : Node_Id;
      Aggr       : Node_Id;
      Arec_Var   : Entity_Id;
   begin
      Frst := First_Entity (E);
      Lst  := Last_Entity (E);
      
      Arec_Type := Get_Arec_Full_Type (E);
      --  Init := Get_Init_Record (Arec_Type);
      Arec_Var := Get_Arec_Variable (E);
      Aggr     := Expression (Parent (Arec_Var));
      
      --  pragma Assert (Present (Init));
      
      --  Aggr := Expression (Init);
      
      pragma Assert (Present (Aggr));
      
      --  Here we know that list is create by Create_Global_Arec_Entities
      
      Comp_List := 
	Component_Items
	(Component_List (Type_Definition (Parent (Arec_Type))));

      Globals := Get_Globals_List (E);
      if not Globals_Assoc_Lists.Is_Empty (Globals) then
	 
	 --  For each global variables used by the subprogram, create a field
	 --  in the AREC Full Type holding a refernec to this global variable,
	 --  and initiliaze it with the address of the global
	 
         for A of Globals loop
            Original := A.Original;
	    Gtyp     := Etype (Original);
	    
	    --  Unity cannot accept to write a deferenced anonymous global
	    --  reference, as we cannot set the R/W attribute for an anonymous
	    --  refrence, so we type the component associated with global with
	    --  an Access Full Type To avoid to create multiple Full Type, we 
	    --  associate the Full Type with record, in order to reuse it, if
	    --  the global is used in an other procedure. The Full Type is only
	    --  created if there is not an already user Access Full Type 
	    --  created by the source program. The semantic do the association,
	    --  and we can used it. This problem in Unity is only for global 
	    --  record variables, for all variables of a type other than record
	    --  we created an anonymous access type.
	    
	    --  if Is_Record_Type (Gtyp) then
	       
	    --     Arec_Type := Get_Arec_Full_Type (Original);
	       
	    --     Comp_Def := Make_Component_Definition
	    --  	 (Sloc              => Sloc (E),
	    --  	  Access_Definition => 
	    --  	    Make_Access_Definition
	    --  	    (Sloc             => Sloc (E),
	    --  	     All_Present      => True,
	    --  	     Subtype_Mark     => 
	    --  	       New_Occurrence_Of (Arec_Type, Sloc (E))));
	       
	       --  If an access to object definition Original si already
	       --  defined, reuse it, else create the type and associated it
	       --  to the type of original
	       
	       -- Access_Type_Entity := Access_To_Object_Defined (Gtyp);
	       
	       --  If no already defined access to the type of the vaiable,
	       --  create It.
	       
	       --  if No (Access_Type_Entity) then
	       --  	  Create_Access_To_Record (Gtyp);
	       --  	  Access_Type_Entity := Access_To_Object_Defined (Gtyp);
	       --  end if;
	       
	       --  Comp_Def := Make_Component_Definition
	       --  	 (Sloc               => Sloc (E),
	       --  	  Subtype_Indication => 
	       --  	    New_Occurrence_Of (Access_Type_Entity, Sloc (Original)));
	       
	    --  else
	    --     --  In the case of a simple type or an array type, just create
	    --     --  an anonymous access type
	       
	    --     Comp_Def := Make_Component_Definition
	    --  	 (Sloc              => Sloc (E),
	    --  	  Access_Definition => 
	    --  	    Make_Access_Definition
	    --  	    (Sloc             => Sloc (E),
	    --  	     All_Present      => True,
	    --  	     Subtype_Mark     => 
	    --  	       New_Occurrence_Of (Gtyp, Sloc (E))));
	    --  end if;
	    
	    Comp_Def := Make_Component_Definition
	      (Sloc              => Sloc (E),
	       Access_Definition => 
		 Make_Access_Definition
		 (Sloc             => Sloc (E),
		  All_Present      => True,
		  Subtype_Mark     => 
		    New_Occurrence_Of (Gtyp, Sloc (E))));
	    
	    --  Expression is statically initialized to the Global Reference
	    --  (Address)
	    
	    Attr := Make_Attribute_Reference
	      (Sloc           => Sloc (E),
	       Prefix         => New_Occurrence_Of (Original, Sloc (E)),
	       Attribute_Name => Name_Access);
	    Set_Etype (Attr, Build_Ghost_Access_Object_Type (Gtyp, Attr));
	    
	    --  Create the component
	    
	    Comp_Id := Make_Defining_Identifier
	      (Sloc  => Sloc (E),
	       Chars => String_Find
		 (Get_Name_String (Chars (Original))));
	    
	    --  Associate the Global Entity with its new Entity 
	    
	    A.New_Entity := Comp_Id;
	    
	    Comp_Dec := Make_Component_Declaration
	      (Sloc => Sloc (E),
	       Defining_Identifier  => Comp_Id,
	       Component_Definition => Comp_Def,
	       Expression           => Empty); -- Attr);
	    
	    --  Here we avoid to create an entity to the anomynous access 
	    --  type, so we mark the componennt as Extra_Access_Type to
	    --  generate a deference for the occurence of the component
	    --  this simplify generation a lot.
	    
	    Set_Ekind (Comp_Id, E_Component);
	    Set_Etype (Comp_Id, Etype (Original));
	    Set_Scope (Comp_Id, E);
	    Set_Extra_Access_Type (Comp_Id, True);
	    
	    --  Link component entity with already defined components in the
	    --  AREC record
	    
	    if No (Frst) then
	       Frst := Comp_Id;
	       Set_First_Entity (E, Comp_Id);
	       Lst := Comp_Id;
	    else
	       Set_Next_Entity (Lst, Comp_Id);
	       Lst := Comp_Id;
	    end if;
	    
	    --  Append component to the record
	    
	    Append (Comp_Dec, Comp_List);
	    
	    --  Set the aggregate with initialization
	    
	    Init := Make_Component_Association 
	      (Sloc       => Sloc (Comp_Id),
	       Choices    => New_List
		 (New_Occurrence_Of (Comp_Id, Sloc (Comp_Id))),
	       Expression => Attr);
	    
	    Append (Init, Component_Associations (Aggr));
	    
	 end loop;
	 
	 --  last entity of record is the most recent created component
	 
	 Set_Last_Entity (E, Lst);
      end if;
      
   end Globals_In_Arec_Full_Type;
   
   --------------------------------------
   -- Called_Globals_In_Arec_Full_Type --
   --------------------------------------
   
   procedure Called_Globals_In_Arec_Full_Type (E  : Entity_Id) is
      
      Called_List      : Called_Assoc_Lists.List;
      Called_Arec      : access Global_Arec_Record;
      Called_Arec_Type : Entity_Id;
      Arec_Var         : Entity_Id;
      Comp_Name        : Name_Id;
      Comp_Def         : Node_Id;
      Attr             : Node_Id;
      Comp_Id          : Node_id;
      Comp_Dec         : Node_Id;
      Frst             : Entity_Id;
      Lst              : Entity_Id;
      Arec_Type        : Entity_Id;
      Comp_List        : List_Id;
      E_Arec_Var       : Entity_Id;
      Aggr             : Node_Id;
      Init             : Node_Id;
   begin
      Frst := First_Entity (E);
      Lst  := Last_Entity (E);
      
      Arec_Type := Get_Arec_Full_Type (E);
      E_Arec_Var := Get_Arec_Variable (E);
      Aggr     := Expression (Parent (E_Arec_Var));
      
      --  Here we know that list is create by Create_Global_Arec_Entities
      
      Comp_List := 
	Component_Items
	(Component_List (Type_Definition (Parent (Arec_Type))));
      
      Called_List := Get_Called_List (E);
      if not Called_Assoc_Lists.Is_Empty (Called_List) then
	 
	 for Called of Called_List loop
	    
	    Called_Arec      := Get_Subprogram_Global_Arec (Called.Entity);
	    Called_Arec_Type := Get_Arec_Full_Type (Called.Entity);
	    
	    if Present (Called_Arec_Type) then
	       declare
		  S : String := "_p" & 
		    Get_Name_String (Chars (Called.Entity)); -- & "_arec";
	       begin
		  Comp_Name := String_Find (S);
	       end;
	       
	       --  If an access to object definition Original si already
	       --  defined, reuse it, else create the type and associated it
	       --  to the type of original
	       
	       --  Access_Type_Entity := 
	       --  	 Access_To_Object_Defined (Called_Arec_Type);
	       --  pragma Assert (Present (Access_Type_Entity));
	       
	       --  Comp_Def := Make_Component_Definition
	       --  	 (Sloc               => Sloc (E),
	       --  	  Subtype_Indication => 
	       --  	    New_Occurrence_Of (Access_Type_Entity, Sloc (E)));
	       
	       Comp_Def := Make_Component_Definition
		 (Sloc              => Sloc (E),
		  Access_Definition => 
		    Make_Access_Definition
		    (Sloc             => Sloc (E),
		     All_Present      => True,
		     Subtype_Mark     => 
		       New_Occurrence_Of (Called_Arec_Type, Sloc (E))));
	    
	       
	       --  Initialize the component to the address of the AREC 
	       --  Variable of the called entity
	       
	       Arec_Var := Get_Arec_Variable (Called.Entity);
	       Attr := Make_Attribute_Reference
		 (Sloc    => Sloc (E),
		  Prefix  => 
		    New_Occurrence_Of (Arec_Var, Sloc (Called.Entity)),
		  Attribute_Name => Name_Access);
	       
	       Comp_Id := Make_Defining_Identifier
		 (Sloc  => Sloc (Called.Entity),
		  Chars => Comp_Name);
	       
	       --  Associated the called and its corresponding AREC
	       
	       Called.Comp_Id := Comp_Id;
	       
	       --  Create Component for the AREC of the call
	       
	       Comp_Dec := Make_Component_Declaration
		 (Sloc => Sloc (Called.Entity),
		  Defining_Identifier   => Comp_Id,
		  Component_Definition  => Comp_Def,
		  Expression            => Empty); -- Attr);
	       
	       --  Link compoenents into the record
	       
	       if No (Frst) then
		  Frst := Comp_Id;
		  Set_First_Entity (E, Comp_Id);
		  Lst := Comp_Id;
	       else
		  Set_Next_Entity (Lst, Comp_Id);
		  Lst := Comp_Id;
	       end if;
	       
	       --  Here we avoid to create an entity to the anomynous access 
	       --  type, so we mark the componennt as Extra_Access_Type to
	       --  generate a deference for the occurence of the component
	       
	       Set_Ekind (Comp_Id, E_Component);
	       Set_Etype (Comp_Id, Etype (Called_Arec_Type));
	       Set_Scope (Comp_Id, E);
	       Set_Extra_Access_Type (Comp_Id, True);
	       
	       Append (Comp_Dec, Comp_List);
	       
	       Init := Make_Component_Association 
		 (Sloc       => Sloc (Comp_Id),
		  Choices    => New_List
		    (New_Occurrence_Of (Comp_Id, Sloc (Comp_Id))),
		  Expression => Attr);
	       
	       Append (Init, Component_Associations (Aggr));
	    end if;
	 end loop;
	 
	 Set_Last_Entity (E, Lst);
      end if;
   end Called_Globals_In_Arec_Full_Type;
   
   -------------------------------
   -- Create_Empty_Extra_Actual --
   -------------------------------
   
   procedure Create_Empty_Extra_Actual (Subprogram_Call : Node_Id) is
      
      Loc         : constant Source_Ptr := Sloc (Subprogram_Call);
      Call        : Entity_Id;
      Param_Assoc : Node_Id;
      Params      : List_Id;
      Last        : Node_Id;
      Actual      : Node_Id;
      Formal      : Node_Id;
      First_Named : Node_Id;
      
      procedure Dump_Param_Name is
	 Actual  : Node_Id;
	 Formal  : Node_Id;
      begin
	 Actual := First_Actual (Subprogram_Call);
	 while Present (Actual) loop
	    if Nkind (Parent (Actual)) = N_Parameter_Association then
	       Formal := Selector_Name (Parent (Actual));
	       Put_Line ("Formal = " & Get_String (Chars (Formal)));
	    else
	       Put_Line ("No Parameter Assoc");
	    end if;
	    
	    if Is_Entity_Name (Actual) then
	       Put_Line ("Actual = " & Get_String (Chars (Actual)));
	    else
	       Put_Line ("Actual is not Entity " & Nkind (Actual)'Img);
	    end if;
	    
	    Next_Actual (Actual);
	 end loop;
      end Dump_Param_Name;
      
   begin
      Put_Line ("Create_Empty_Extra_Actual Begin");
      
      Put_Line ("1");
      Call := Entity (Name (Subprogram_Call));
      
      if No (Arec_Error_Entity) then
	 Create_Arec_Error_Enrity;
      end if;
      
      pragma Assert (Present (Call));
      
      Put_Line ("2 " & Get_String (Chars (Call)));
      Params := Parameter_Associations (Subprogram_Call);
      
      Last   := Empty;
      Actual := First_Actual (Subprogram_Call);
      Formal := First_Formal_With_Extras (Call);
      
      if Is_Empty_List (Params) and then Present (Formal) then
	 Params := New_List;
	 Set_Parameter_Associations (Subprogram_Call, Params);
      end if;
      
      First_Named := First_Named_Actual (Subprogram_Call);
      
      while Present (Formal) loop
	 Put_Line ("3 " & Get_String (Chars (Formal)));
	 
	 if No (Actual) then
	    Actual := New_Occurrence_Of (Arec_Error_Entity, Loc);
	    Param_Assoc :=
	      Make_Parameter_Association
	      (Loc,
	       Selector_Name             => New_Occurrence_Of (Formal, Loc),
	       Explicit_Actual_Parameter => Actual);
	    Append (Param_Assoc, Params);
	    
	    if No (First_Named) then
	       Set_First_Named_Actual (Subprogram_Call, Actual);
	       Set_Next_Named_Actual  (Parent (Actual), Empty);
	       First_Named := First_Named_Actual (Subprogram_Call);
	    else	
	       Set_Next_Named_Actual 
		 (Parent (Actual), Next_Named_Actual (Parent (Last)));
	       Set_Next_Named_Actual (Parent (Last), Actual);
	    end if;
	    
	    Set_Parent (Actual, Param_Assoc);
	 end if;
	 
	 Last := Actual;
	 Next_Actual (Actual);
	 Next_Formal_With_Extras (Formal);
      end loop;
      
      Dump_Param_Name;
      
      Put_Line ("Create_Empty_Extra_Actual End");
   exception
      when others =>
	 Put_Line ("Exception ===================> Create_Empty_Extra_Actual");
   end Create_Empty_Extra_Actual;
   
   ------------------------------
   -- Add_Extra_Actual_To_Call --
   ------------------------------

   procedure Add_Extra_Actual_To_Call
     (Subprogram_Call : Node_Id;
      Extra_Formal    : Entity_Id;
      Extra_Actual    : Node_Id)
   is
      Loc         : constant Source_Ptr := Sloc (Subprogram_Call);
      Call        : Entity_Id;
      Actual      : Node_Id;
      Formal      : Node_Id;
      Par         : Node_Id;
      Prv         : Node_Id;
      First_Named : Node_Id;
      
      procedure Dump_Param_Name is
	 Actual  : Node_Id;
	 Formal  : Node_Id;
      begin
	 Actual := First_Actual (Subprogram_Call);
	 while Present (Actual) loop
	    if Nkind (Parent (Actual)) = N_Parameter_Association then
	       Formal := Selector_Name (Parent (Actual));
	       Put_Line ("Formal = " & Get_String (Chars (Formal)));
	    else
	       Put_Line ("No Parameter Assoc");
	    end if;
	    
	    if Is_Entity_Name (Actual) then
	       Put_Line ("Actual = " & Get_String (Chars (Actual)));
	    else
	       Put_Line ("Actual is not Entity " & Nkind (Actual)'Img);
	    end if;
	    
	    Next_Actual (Actual);
	 end loop;
      end Dump_Param_Name;
      
   begin
      Put_Line ("Add_Extra_Actual_To_Call Begin " & Subprogram_Call'Img);
      Put_Line ("Extra_Formal " & Get_String (Chars (Extra_Formal)));
      if Is_Entity_Name (Extra_Actual) then
	 Put_Line ("Actual = " & Get_String (Chars (Extra_Actual)));
      else
	 Put_Line ("Actual is not Entity " & Nkind (Extra_Actual)'Img);
      end if;
      
      Create_Empty_Extra_Actual (Subprogram_Call);

      Call := Entity (Name (Subprogram_Call));
      
      --  Now we know thet all Parameters Association are been created. Find
      --  the correponding formal in parameter association and replace its 
      --  explicit actual by the actual
      
      First_Named := First_Named_Actual (Subprogram_Call);
      
      Prv := Empty;
      
      Actual := First_Actual (Subprogram_Call);
      Formal := First_Formal_With_Extras (Call);
      while Present (Formal) loop
	 Put_Line ("5 " & Get_String (Chars (Formal)));
	 Par := Parent (Actual);
	 
	 if Extra_Formal = Formal then
	    pragma Assert (Nkind (Par) = N_Parameter_Association);
	    --  Rewrite (Actual, Extra_Actual);
	    Set_Explicit_Actual_Parameter (Par, Extra_Actual);
	    
	    if First_Named = Actual then
	       Set_First_Named_Actual (Subprogram_Call, Extra_Actual);
	    else
	       Set_Next_Named_Actual (Parent (Prv), Extra_Actual);
	    end if;
	    
	    Put_Line ("82");
	    Dump_Param_Name;
	    return;
	 end if;
	 
	 Prv := Actual;
	 Next_Actual (Actual);
	 Next_Formal_With_Extras (Formal);
      end loop;
      
      Dump_Param_Name;
      Put_Line ("Add_Extra_Actual_To_Call End");
   exception
      when others =>
	 Put_Line ("Exception ===================> Add_Extra_Actual_To_Call");
   end Add_Extra_Actual_To_Call;
   
   ---------------------------------
   -- Add_Extra_Actuals_To_Called --
   ---------------------------------
   
   procedure Add_Extra_Actuals_To_Called (E : Entity_Id) is
      
      Called_List : Called_Assoc_Lists.List;
      Arec        : access Global_Arec_Record;
      Formal      : Entity_Id;
      Ptr         : Entity_Id;
      Sel         : Node_Id;
   begin
      Called_List := Get_Called_List (E);
      
      Ptr := Get_Arec_Extra_Formal (E);
      
      if not Called_Assoc_Lists.Is_Empty (Called_List) then
	 
	 for Called of Called_List loop
	    
	    Arec   := Get_Subprogram_Global_Arec (Called.Entity);
	    Formal := Get_Arec_Extra_Formal (Called.Entity);
	    
	    if Present (Formal) then
	       
	       --  Add the extra actual to each call of Called.Entity
	       
	       if not Reflex.Nodes_Lists.Is_Empty (Called.Calls_Stmts) then
		  
		  for Call of Called.Calls_Stmts loop
		     
		     Sel := Make_Selected_Component
		       (Sloc          => Sloc (Call),
			Prefix        => 
			  New_Occurrence_Of (Ptr, Sloc (Call)),
			Selector_Name => 
			  New_Occurrence_Of (Called.Comp_Id, Sloc (Call)));
		  
		     Add_Extra_Actual_To_Call (Call, Formal, Sel);
		  end loop;
	       end if;
	    end if;
	 end loop;
      end if;
      
   exception
      when others =>
	 Put_Line
	   ("Exception ===================> Add_Extra_Actuals_To_Called");
   end Add_Extra_Actuals_To_Called;
   
   ------------------------------
   -- Create_Arec_Extra_Formal --
   ------------------------------
   
   procedure Create_Arec_Extra_Formal (E : Entity_Id) is
      
      S          : String := "_ptr"; -- & Get_Name_String (Chars (E)) & "_arec";
      Id         : Node_Id;
      Formal     : Node_Id;
      Def_Type   : Entity_Id;
      Tac_Entity : Entity_Id;
   begin
      --  Retreive the AREC Full Type asscoiated to the Subprogram entity.
      
      Def_Type   := Get_Arec_Full_Type (E);
      Tac_Entity := Access_To_Object_Defined (Def_Type);

      --  Create extra formal of type anonymous access to the AREC Full
      --  Type of the Subprogram entity.
      
      Id := Make_Defining_Identifier
	(Sloc  => Sloc (E),
	 Chars => String_Find (S));
      
      Formal := Make_Parameter_Specification
	(Sloc                => Sloc (E),
	 Defining_Identifier => Id,
	 In_Present          => True,
	 Parameter_Type      => 
	   --	   New_Occurrence_Of (Tac_Entity, Sloc (E)));
	   Make_Access_Definition
	   (Sloc         => Sloc (E),
	    All_Present  => True,
	    Subtype_Mark => 
	      New_Occurrence_Of (Def_Type, Sloc (E))));
      
      -- New_Occurrence_Of (Tac_Entity, Sloc (E)));
      
      --  Set the entities attributes 
      
      Set_Ekind (Id, E_In_Parameter);
      Set_Etype (Id, Tac_Entity);
      Set_Scope (Id, E);
      
      --  Add an AREC extra formal to the Subprogram
      
      Add_Extra_Formal (E, Id);
      
      --  Associate the AREC extra formal with the Subprogram entity
      
      Set_Arec_Extra_Formal (E, Id);
   end Create_Arec_Extra_Formal;
   
   ---------------------------------
   -- Create_Global_Arec_Entities --
   ---------------------------------
   
   procedure Create_Global_Arec_Entities (E : Entity_Id) is
      
      Type_Id   : Node_Id;
      Full_Type : Node_Id;
      Subp_Body : Node_id;
      Body_Id   : Entity_id;
      Var_Id    : Entity_Id;
      Decl      : Node_Id;
      Prev      : Entity_Id;
      Stype     : String := Get_Name_String (Chars (E)) & "_arec";
      Svar      : String := Get_Name_String (Chars (E)) & "_arec";
      Aggr      : Node_Id;
      Access_Id : Entity_Id;      
   begin
      --  The AREC entities are declared just before Subprogram, so retreive
      --  the Subprogram body definition point.
      
      Body_Id := E;
      Subp_Body := Parent (Parent (E));
      if Nkind (Subp_Body) = N_Subprogram_Declaration then
	 Body_Id := Corresponding_Body (Subp_Body);
	 
	 pragma Assert (Ekind (Body_Id) = E_Subprogram_Body);
	 
	 Subp_Body := Parent (Parent (Body_Id));
      end if;
      pragma Assert (Nkind (Subp_Body) = N_Subprogram_Body);
      
      --  Create the AREC record full type
      
      Full_Type := Make_Full_Type_Declaration
	(Sloc                => Sloc (E),
	 Defining_Identifier => 
	   Make_Defining_Identifier
	   (Sloc  => Sloc (E),
	    Chars => New_Type_Name (Stype)),
	 Type_Definition     => 
	   Make_Record_Definition
	   (Sloc           => Sloc (E),
	    Component_List => 
	      Make_Component_List
	      (Sloc            => Sloc (E),
	       Component_Items => New_List)));
      
      --  Fill entity attributes for the AREC Full Type
      
      Type_Id := Defining_Identifier (Full_Type);
      Set_Etype (Type_Id, Type_Id);
      Set_Ekind (Type_Id, E_Record_Type);
      Set_Scope (Type_Id, Scope (Body_Id));
      
      --  Add declaration of Full Type at the library Level, just before 
      --  the body. We know that is not a visible Reflex declaration for the 
      --  users of this type. But for generation there is no matter
      
      Insert_Before (Subp_Body, Full_Type);
      
      --  Create the Init_Record for the AREC Full Type
      
      Aggr := Make_Aggregate
	(Sloc                   => Sloc (E),
	 Expressions            => No_List,
	 Component_Associations => New_List);
      
      --  Declare Arec Variable
      
      Decl := Make_Object_Declaration
	(Sloc                => Sloc (E),
	 Defining_Identifier => Make_Defining_Identifier
	   (Sloc  => Sloc (E),
	    Chars => New_Variable_Name (Svar)),
	 Object_Definition   => 
	   New_Occurrence_Of (Type_Id, Sloc (E)),
	 Expression          => Aggr);
      
      --  Insert just before Subprogram body and after AREC full type 
      --  declaration
      
      Insert_Before (Subp_Body, Decl);
      
      --  Fill entity attributes for the AREC Variable
      
      Var_Id := Defining_Identifier (Decl);
      Set_Ekind (Var_Id, E_Variable);
      Set_Etype (Var_Id, Type_Id);
      Set_Scope (Var_Id, Scope (Body_Id));
      
      -- Link the new entities AREC Type and AREC Variable in the scope of
      -- the Subprogram. The new entities are put before the subprohram entity
      
      Prev := First_Entity (Scope (Body_Id));
      
      while Present (Prev)
	and then Next_Entity (Prev) /= Body_Id
      loop
	 Next_Entity (Prev);
      end loop;
      
      if Present (Prev) then
	 Set_Next_Entity (Prev, Type_Id);
      else
	 Set_First_Entity (Scope (Body_Id), Type_Id);
      end if;
      
      Set_Next_Entity (Type_Id, Var_Id);
      Set_Next_Entity (Var_Id, Body_Id);
      
      --  Declare Access to AREC Full Type
      
      Create_Access_To_Record (Type_Id);
      
      Access_Id := Access_To_Object_Defined (Type_Id);
      Set_Is_Anonym (Access_Id, True);
      
      --  Associate AREC Full Type and AREC Variable to the entity
      
      Set_Arec_Full_Type (E, Type_Id);
      Set_Arec_Variable (E, Var_Id);
      
   end Create_Global_Arec_Entities;
   
   -------------------------------
   -- Need_Declare_Globals_Arec --
   -------------------------------
   
   procedure Need_Declare_Globals_Arec (E : Entity_Id) is
      
      use Globals_Assoc_Lists;
      
      This        : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
      Called_List : Called_Assoc_Lists.List;
      Ecall       : Entity_Id;
      Calling_Entity : Entity_Id;
   begin
      if Pending_Arec (E) then
      	 Ecall := E;
      	 for I in reverse 1..Last_Call_Stack loop
      	    Calling_Entity := Arec_Call_Stack (I);
      	    if Ekind (Calling_Entity) = E_Function then
      	       Error_Msg_NE (" function & call & ", Calling_Entity, E);
      	    else
      	       Error_Msg_NE (" procedure & call & ", Calling_Entity, E);
      	    end if;
      	    Ecall := Calling_Entity;
      	 end loop;
      	 return;
      end if;
      
      if not Is_Empty (This.Globals) then
	 Set_Declare_Global_Arec (E, True);
	 return;
      end if;
      
      Called_List := Get_Called_List (E);
      for Call of Called_List loop
	 Need_Declare_Globals_Arec (Call.Entity);
	 if Need_Globals_Arec (Call.Entity) then
	    Set_Declare_Global_Arec (E, True);
	 end if;
      end loop;
      
   end Need_Declare_Globals_Arec;
   
   ----------------------------
   -- Populate_Arec_Entities --
   ----------------------------
   
   procedure Populate_Arec_Entities (E : Entity_Id) is
   begin
      
      if Declare_Global_Arec (E) then
	 
	 --  Create Extra Formal
	 Create_Arec_Extra_Formal (E);
	 
	 --  Populate Arec Record with acces to globals
	 
	 Globals_In_Arec_Full_Type (E);
	 
	 --  Populate Arec Record global used in call
	 
	 Called_Globals_In_Arec_Full_Type (E);
	 
	 --  Add extra actual for each calls
	 
	 Add_Extra_Actuals_To_Called (E);      
	 
	 Replace_Globals (E);
      end if;
      
   end Populate_Arec_Entities;
   
   --------------------------------
   -- Create_Arec_Parameter_Type --
   --------------------------------
   
   procedure Create_Arec_Parameter_Type (E : Entity_Id) is
      
      use Globals_Assoc_Lists;
      
      This    : access Global_Arec_Record := Get_Subprogram_Global_Arec (E);
      Ent     : Entity_Id;
      Id      : Entity_Id;
      Formal  : Node_Id;
      Globals : Globals_Assoc_Lists.List;
   begin
      Globals := Get_Globals_List (E);
      if not Globals_Assoc_Lists.Is_Empty (Globals) then
	 
	 for A of Globals loop
	    Ent := A.Original;
	    
	    Id := Make_Defining_Identifier
	      (Sloc  => Sloc (Ent),
	       Chars => Chars (Ent));
	    
	    A.New_Entity := Id;
	    
	    Formal := Make_Parameter_Specification
	      (Sloc                => Sloc (Ent),
	       Defining_Identifier => Id,
	       In_Present          => True,
	       Parameter_Type      => 
		 New_Occurrence_Of (Etype (Ent), Sloc (Ent)));
	 
	    Set_Ekind (Id, E_In_Out_Parameter);
	    Set_Etype (Id, Etype (Ent));
	    Set_Scope (Id, E);
	    
	    Add_Extra_Formal
	      (E  => E,
	       EF => Id);
	    
	    Set_Arec_Extra_Formal (E, Id);
	 end loop;
      end if;	 
   end Create_Arec_Parameter_Type;
            
   --------------------------
   -- Create_Arec_Instance --
   --------------------------
   
   procedure Create_Arec_Instance (E : Entity_Id) is
   begin
      null;
   end Create_Arec_Instance;
            
   ------------------------------
   -- Create_Arec_Extra_Formal --
   ------------------------------
   
   procedure Create_Arec_Extra_Formal
     (This : access Global_Arec_Record;
      E    : Entity_Id) is
   begin
      null;
   end Create_Arec_Extra_Formal;
            
   --------------------
   -- Collect_Called --
   --------------------
   
   procedure Collect_Called  (E : Entity_Id) is
      
      function Process (N : Node_Id) return Traverse_Result;
      
      -------------
      -- Process --
      -------------
      
      function Process (N : Node_Id) return Traverse_Result is
	 
	 Subp_Id : Entity_Id;
      begin
	 if Nkind (N) = N_Procedure_Call_Statement
	   or else Nkind (N) = N_Function_Call
	 then
	    Subp_Id := Entity (Name (N));
	    if not Plc_Library_Scope (Subp_Id) 
	      and then Is_Library_Level_Entity (Subp_Id) then
	       Add_Called_Entity (E, Subp_Id, N);	       
	       return Skip;
	    end if;
	 end if;
	 
	 return Ok;
      end Process;
      
      function Traverse is new Traverse_Func (Process => Process);
      
      Subp  : Node_Id;
      Dummy : Traverse_Final_Result;
   begin
      --  Then look in handled sequence of statements
      
      if Ekind (E) = E_Procedure or else Ekind (E) = E_Function then
	 Subp := Subprogram_Body (E);
	 if Present (Subp) then
	    Dummy := Traverse (Handled_Statement_Sequence (Subp));
	 end if;
      end if;
      
      declare
         Called : Called_Assoc_Lists.List;
      begin
      	 Called := Get_Called_List (E);
      	 for A of Called loop
      	    Put_Line
      	      ("Called Entity =>  " & Get_Name_String (Chars (A.Entity)));
         end loop;
      end;
   end Collect_Called;
   
   ----------------------
   -- Add_Extra_Formal --
   ----------------------
   
   procedure Add_Extra_Formal
     (E  : Entity_Id;
      EF : Entity_Id) 
   is
     First_Extra : Entity_Id := Extra_Formals (E);
     Last_Formal : Entity_Id;
     Formal      : Entity_Id;
   begin
      Put_Line ("Add_Extra_Formal Begin");
      
      --  It is the first extra formal
      
      if No (First_Extra) then
	 Set_Extra_Formals (E, Ef);
      
	 Last_Formal := Empty;
	 Formal := First_Formal (E);
	 while Present (Formal) loop
	    Last_Formal := Formal;
	    Next_Formal_With_Extras (Formal);
	 end loop;
      
	 if Present (Last_Formal) then
	   Set_Extra_Formal (Last_Formal, EF);
	 end if;
	 
	 Set_Extra_Formal (EF, Empty);
	 
	 --  Extra Formals already exists
	 
      else
	 Last_Formal := First_Extra; 
	 
	 pragma Assert (Present (Last_Formal));
	 
	 Formal := Last_Formal;
	 while Present (Formal) loop
	    Last_Formal := Formal;
	    Next_Formal_With_Extras (Formal);
	 end loop;
	 
	 Set_Extra_Formal (Last_Formal, EF);
	 Set_Extra_Formal (EF, Empty);
      end if;
      
   end Add_Extra_Formal;
   
   -----------------------
   -- Get_Instance_Name --
   -----------------------
   
   function Get_Instance_Name_In_Scope
     (Scp : Entity_Id;
      E   : Entity_Id) return Name_Id is
      
      Global_Arec : access Global_Arec_Record;
   begin
      Global_Arec := Get_Subprogram_Global_Arec (Scp);
      
      for Call of Global_Arec.Called loop
	 if Call.Entity = E then
	    return Call.Instance_Name;
	 end if;
      end loop;
      
      return No_Name;
   end Get_Instance_Name_In_Scope;
   
   ----------------------------
   -- Extra_Formal_Dfb_Count --
   ----------------------------
   
   function Extra_Formal_Dfb_Count (E : Entity_Id) return Natural is
      
      Formal       : Entity_Id;
      In_Count     : Natural;
      Out_Count    : Natural;
      In_Out_Count : Natural;
   begin
      In_Count     := 0;
      Out_Count    := 0;
      In_Out_Count := 0;
      
      Formal := First_Formal_With_Extras (E);
      while Present (Formal) loop
	 if Ekind (Formal) = E_In_Parameter then
	    In_Count := In_Count + 1;
	 elsif Ekind (Formal) = E_Out_Parameter then
	    Out_Count := Out_Count + 1;
	 else
	    In_Out_Count := In_Out_Count + 1;
	 end if;
	 Next_Formal_With_Extras (Formal);
      end loop;
      
      In_Count  := In_Count  + In_Out_Count;
      Out_Count := Out_Count + In_Out_Count;
      
      if In_Count > Out_Count then
	 return In_Count;
      else
	 return Out_Count;
      end if;
   end Extra_Formal_Dfb_Count;
   
end Reflex.Global_Arecs;
