------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
--                                                                          --
-- Reflex is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
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

with Ada.text_io; use Ada.text_io;

with Atree; use Atree;
with Einfo; use Einfo;
with Sinfo; use Sinfo;
with Nlists; use Nlists;
with Sem_Util; use Sem_Util;
with Types; use Types;

with Reflex.Expanders.Types; use Reflex.Expanders.Types;
with Reflex.Expanders.Utils; use Reflex.Expanders.Utils;
with Reflex.Expanders.Dispatch; use Reflex.Expanders.Dispatch;
with Reflex.Expanders.Supports; use Reflex.Expanders.Supports;

package body Reflex.Expanders.Ch7 is
   
   -----------------------------------------
   -- Expand_Defining_Program_Unit_Name --
   -----------------------------------------
   
   procedure Expand_Defining_Program_Unit_Name 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      Expand_Node (This, Defining_Identifier (Node));
   end Expand_Defining_Program_Unit_Name;
   
   -------------------------
   -- Expand_Designator --
   -------------------------
   
   procedure Expand_Designator 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
   begin
      null;
   end Expand_Designator;
   
   ---------------------------
   -- Expand_Package_Body --
   ---------------------------
   
   procedure Expand_Package_Body 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
      Body_Id : Entity_Id := Defining_Entity (Node);
      Spec_Id : Entity_Id := Unique_Defining_Entity (Node);
      Last_Spec_Entity : Entity_Id;
   begin
      if Ekind (Corresponding_Spec (Node)) = E_Generic_Package then
	 return;
      end if;
      
      --  A package body must have a package specification
      
      pragma Assert (Present (Corresponding_Spec (Node)));
	  
      Last_Spec_Entity := Last_Entity (Spec_Id);
      Set_Last_Entity (Spec_Id, Last_Entity (Body_Id));
      Set_Next_Entity (Last_Spec_Entity, First_Entity (Body_Id));
     
      Expand_Node_List (This, Declarations (Node));
      
      declare
	 Stmts     : constant Node_Id := Handled_Statement_Sequence (Node);
	 Has_Stmts : constant Boolean := Present (Stmts)
	   and then Has_Non_Null_Statements (Statements (Stmts));
	 Unit : Node_Id;
	 
      begin
	 --  Only Expand elaboration procedures when in main unit.
	 
	 if not In_Main_Unit then
	    null;
	    
	    --  For packages inside subprograms, Expand elaboration
	    --  code as standard code as part of the enclosing unit.
	    
	 elsif not Library_Level then
	    if Has_Stmts then
	       Open_Scope (This);
	       Set_In_Statements (This, True);
	       Close_Scope (This);
	    end if;
	    
	 elsif Nkind (Parent (Node)) /= N_Compilation_Unit then
	    if Has_Stmts then
	       Elaboration_Table.Append (Stmts);
	    end if;
	    
	 elsif Elaboration_Table.Last = 0
	   and then not Has_Stmts
	 then
	    Set_Has_No_Elaboration_Code (Parent (Node), True);
	    
	 else
	    Unit := Defining_Unit_Name (Node);
	    
	    if Nkind (Unit) = N_Defining_Program_Unit_Name then
	       Unit := Defining_Identifier (Unit);
	    end if;
	    
	    Expand_Node (This, Unit, Declaration => True);
	    Open_Scope (This);
	    
	    declare
	       Save_Library_Level : constant Boolean := Library_Level;
	    begin
	       Library_Level := False;
	       Special_Elaboration_Code := True;
	       
	       for J in 1 .. Elaboration_Table.Last loop
		  Current_Elab_Entity := Elaboration_Table.Table (J);
		  Expand_Node (This, Current_Elab_Entity);
	       end loop;
	       
	       Elaboration_Table.Set_Last (0);
	       Current_Elab_Entity := Empty;
	       Special_Elaboration_Code := False;
	       
	       if Has_Stmts then
		  Expand_Node (This, Stmts);
	       end if;
	       
	       Library_Level := Save_Library_Level;
	    end;
	    
	    Close_Scope (This);
	 end if;
	 
      --  End package
      
	 if Nkind (Parent (Node)) = N_Compilation_Unit then
	    Expand_Node (This, Defining_Unit_Name (Node));
	 end if;
      end;
      
   end Expand_Package_Body;
   
   --------------------------------
   -- Expand_Package_Declaration --
   --------------------------------
   
   procedure Expand_Package_Declaration 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
   begin
      Open_Scope (This, Defining_Entity (Node));
      This.Set_Scope_Node (Node);
      
      Expand_Node (This, Defining_Unit_Name (Specification (Node)));
      Expand_Node (This, Specification (Node), True);
      
      --  End package
      
      if not Present (Corresponding_Body (Node)) 
        and then Nkind (Parent (Node)) = N_Compilation_Unit 
      then
         Expand_Node (This, Defining_Unit_Name (Specification (Node)));
      end if;
      
      This.Close_Scope;
   end Expand_Package_Declaration;
   
   ------------------------------------
   -- Expand_Package_Specification --
   ------------------------------------
   
   procedure Expand_Package_Specification 
     (This : access Reflex_Expander_Record;
      Node : Node_Id) is
      
   begin
      --  Open the new scope associated with this package specification
      --  to ensure that we are ready to start processing declarations
      --  (see Open_Scope). No explicit block is associated with this
      --  scope because:
      --    * for library level packages must not be Expandd
      --    * for nested packages the block is not needed
      
      --  Open_Scope (This, Defining_Entity (Node));
      --  This.Set_Scope_Node (Node);
      
      declare
	 Scope_Id : constant Natural := Current_Scope_Id (This);

      begin
	 Expand_Node_List (This, Visible_Declarations (Node));

	 if Present (Private_Declarations (Node)) then
	    Expand_Node_List (This, Private_Declarations (Node));
	 end if;

	 Set_In_Statements (This, True);

	 --  We can safely close this package scope if it has no inner
	 --  back-end scopes to close.

	 if Current_Scope_Id (This) = Scope_Id then
	    null;
	    
	    --  For library level packages we can also close this scope and
	    --  all its inner back-end scopes (if any)

	 elsif Is_Library_Level_Entity (Defining_Entity (Node)) then
	    null;

	    --  For nested packages we must defer closing it (and its extra
	    --  scopes) since its extra back-end scopes may have been added
	    --  to handle declarations which can be referenced from its
	    --  enclosing scope.

	 else
	    null;
	 end if;
      end;
      
      --  Only Expand elaboration procedures for library-level packages
      --  and when part of the main unit.

      if In_Main_Unit
	and then Nkind (Parent (Parent (Node))) = N_Compilation_Unit
      then
	 if Elaboration_Table.Last = 0 then
	    Set_Has_No_Elaboration_Code (Parent (Parent (Node)), True);
	 else
	    declare
	       Unit : Node_Id := Defining_Unit_Name (Node);
	    begin
	       if Nkind (Unit) = N_Defining_Program_Unit_Name then
		  Unit := Defining_Identifier (Unit);
	       end if;

	       Expand_Node (This, Unit, Declaration => True);
	    end;

	    declare
	       Save_Library_Level : constant Boolean := Library_Level;
	    begin
	       Library_Level := False;
	       Special_Elaboration_Code := True;
	       Set_In_Statements (This, True);

	       for J in 1 .. Elaboration_Table.Last loop
		  Current_Elab_Entity := Elaboration_Table.Table (J);
		  Expand_Node (This, Elaboration_Table.Table (J));
	       end loop;

	       Current_Elab_Entity := Empty;
	       Special_Elaboration_Code := False;
	       Library_Level := Save_Library_Level;
	    end;

	    Elaboration_Table.Set_Last (0);
	 end if;
      end if;
      
      -- Close_Scope (This);
   end Expand_Package_Specification;

end Reflex.Expanders.Ch7;
