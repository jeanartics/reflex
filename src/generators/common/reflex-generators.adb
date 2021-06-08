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

with Ada.Text_Io; use Ada.Text_Io;

with Ada.Unchecked_Deallocation;

with Atree; use Atree;
with Namet; use Namet;
with Sinfo; use Sinfo;
with Einfo; use Einfo;

package body Reflex.Generators is
   
   -------------------
   -- New_Generator --
   -------------------
   
   function New_Generator return Generator_Ptr is
      This : Generator_Ptr := new Generator_Record'(No_Generator_Record);
   begin
      This.Open_Scope (Empty);
      return This;
   end New_Generator;
   
   --------------------------
   -- Initialize_Generator --
   --------------------------
   
   procedure Initialize_Generator (This : in out Generator_Record) is
   begin
      null;
   end Initialize_Generator;
   
   
   --------------------
   -- Free_Generator --
   --------------------
   
   procedure Free_Generator (This : in out Generator_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Generator_Record, Generator_Ptr);
   begin
      Free (This);
   end Free_Generator;
   
   -------------------
   -- Get_Main_Node --
   -------------------
   
   function Get_Main_Node
     (This : access Generator_Record) return Node_Id is
   begin
      return This.Main_Node;
   end Get_Main_Node;
   
   -------------------
   -- Set_Main_Node --
   -------------------
   
   procedure Set_Main_Node
     (This : access Generator_Record;
      Node : Node_Id) is
   begin
      This.Main_Node := Node;
   end Set_Main_Node;
   
   -----------------------
   -- Get_Output_Buffer --
   -----------------------
   
   function Get_Output_Buffer
     (This : access Generator_Record) return Output_Buffer is
   begin
      return This.Ob;
   end Get_Output_Buffer;
   
   --------------------------
   -- Create_Output_Buffer --
   --------------------------
   
   procedure Create_Output_Buffer (This : access Generator_Record) is
   begin
      This.Ob := New_Output_Buffer;
   end Create_Output_Buffer;
   
   --------------------------
   -- Delete_Output_Buffer --
   --------------------------
   
   procedure Delete_Output_Buffer (This : access Generator_Record) is
   begin
      Free_Buffer (This.Ob);
   end Delete_Output_Buffer;
   
   -----------------------
   -- Set_Output_Buffer --
   -----------------------
   
   procedure Set_Output_Buffer
     (This : access Generator_Record;
      Ob   : Output_Buffer) is
   begin
      This.Ob := Ob;
   end Set_Output_Buffer;
   
   -----------------------------
   -- Get_Current_Source_File --
   -----------------------------
   
   function Get_Current_Source_File
     (This : access Generator_Record) return Source_File_Index is
   begin
      return This.Current_Source_File;
   end Get_Current_Source_File;
   
   -----------------------------
   -- Set_Current_Source_File --
   -----------------------------
   
   procedure Set_Current_Source_File
     (This        : access Generator_Record;
      Source_File : Source_File_Index) is
   begin
      This.Current_Source_File := Source_File;
   end Set_Current_Source_File;
   
   ------------------------------
   -- Get_Full_Code_Generation --
   ------------------------------
   
   function Get_Full_Code_Generation
     (This : access Generator_Record) return Boolean is
   begin
      return This.Full_Code_Generation;
   end Get_Full_Code_Generation;
   
   ------------------------------
   -- Set_Full_Code_Generation --
   ------------------------------
   
   procedure Set_Full_Code_Generation
     (This : access Generator_Record;
      Gen  : Boolean) is
   begin
      This.Full_Code_Generation := Gen;
   end Set_Full_Code_Generation;
   
   ---------------------------
   -- Get_Last_Line_Printed --
   ---------------------------
   
   function Get_Last_Line_Printed
     (This : access Generator_Record) return Physical_Line_Number is
   begin
      return This.Last_Line_Printed;
   end Get_Last_Line_Printed;
   
   ---------------------------
   -- Set_Last_Line_Printed --
   ---------------------------
   
   procedure Set_Last_Line_Printed
     (This : access Generator_Record;
      Line : Physical_Line_Number) is
   begin
      This.Last_Line_Printed := Line;
   end Set_Last_Line_Printed;
   
   -----------------------------
   -- Get_Dump_Source_Comment --
   -----------------------------
   
   function Get_Dump_Source_Comment
     (This : access Generator_Record) return Boolean is
   begin
      return This.Dump_Source_Comment;
   end Get_Dump_Source_Comment;
   
   -----------------------------
   -- Set_Dump_Source_Comment --
   -----------------------------
   
   procedure Set_Dump_Source_Comment
     (This    : access Generator_Record;
      Comment : Boolean) is
   begin
      This.Dump_Source_Comment := Comment;
   end Set_Dump_Source_Comment;
   
   -------------------
   -- Get_Dump_Node --
   -------------------
   
   function Get_Dump_Node
     (This : access Generator_Record) return Node_Id is
   begin
      return This.Dump_Node;
   end Get_Dump_Node;
   
   -------------------
   -- Set_Dump_Node --
   -------------------
   
   procedure Set_Dump_Node
     (This : access Generator_Record;
      Node : Node_Id) is
   begin
      This.Dump_Node := Node;
   end Set_Dump_Node;
   
   ------------------------------
   -- Get_In_Package_Body_Init --
   ------------------------------
   
   function Get_In_Package_Body_Init
     (This : access Generator_Record) return Boolean is
   begin
      return This.In_Package_Body_Init;
   end Get_In_Package_Body_Init;
   
   ------------------------------
   -- Set_In_Package_Body_Init --
   ------------------------------
   
   procedure Set_In_Package_Body_Init
     (This : access Generator_Record;
      Init : Boolean) is
   begin
      This.In_Package_Body_Init := Init;
   end Set_In_Package_Body_Init;
   
   -------------------
   -- Do_Generation --
   -------------------
   
--     procedure Do_Generation (This : access Generator_Record) is
--     begin
--        null;
--     end Do_Generation;
   
   
   ----------------
   -- Open_Scope --
   ----------------

   procedure Open_Scope
     (This : access Generator_Record;
      E    : Entity_Id := Empty) is
      use Scope_Stack;
   begin
      Increment_Last (This.Scopes);
      
      This.Scopes.Table (Last (This.Scopes)) := No_Scope_Stack_Entry;
      
      This.Scopes.Table (Last (This.Scopes)).Scope_Entity := E;
   end Open_Scope;

   -----------------
   -- Close_Scope --
   -----------------

   procedure Close_Scope
     (This : access Generator_Record) is
      use Scope_Stack;
   begin
      Decrement_Last (This.Scopes);
   end Close_Scope;

   -----------------
   -- Close_Scope --
   -----------------

   procedure Close_Scope
     (This    : access Generator_Record;
      Scop_Id : Natural) is
      use Scope_Stack;
   begin
      loop
	 Close_Scope (This);
	 exit when Last (This.Scopes) < Scop_Id;
      end loop;
   end Close_Scope;
   
   ----------------------
   -- Open_Extra_Scope --
   ----------------------

   procedure Open_Extra_Scope
     (This : access Generator_Record;
      E    : Entity_Id := Empty) is
      use Scope_Stack;
   begin
      Increment_Last (This.Scopes);
      
      This.Scopes.Table (Last (This.Scopes)) := No_Scope_Stack_Entry;
      This.Scopes.Table (Last (This.Scopes)).Extra_Scope := True;
   end Open_Extra_Scope;
   
   -----------------------
   -- Close_Extra_Scope --
   -----------------------

   procedure Close_Extra_Scope
     (This : access Generator_Record) is
      use Scope_Stack;
   begin
      Decrement_Last (This.Scopes);
   end Close_Extra_Scope;
 
   ---------------------------
   -- Close_All_Extra_Scope --
   ---------------------------

   procedure Close_All_Extra_Scope
     (This    : access Generator_Record;
      Scop_Id : Natural) is
      use Scope_Stack;
      
      Lst : Natural;
   begin
      loop
	 Lst := Last (This.Scopes);
	 exit when not This.Scopes.Table (Last (This.Scopes)).Extra_Scope;
	 Close_Scope (This);
      end loop;
   end Close_All_Extra_Scope;
   
   ----------------------
   -- Current_Scope_Id --
   ----------------------

   function Current_Scope_Id
     (This : access Generator_Record) return Natural is
      use Scope_Stack;
   begin
      return Last (This.Scopes);
   end Current_Scope_Id;

   ----------------------
   -- Get_Scope_Entity --
   ----------------------

   function Get_Scope_Entity
     (This : access Generator_Record) return Entity_Id is
      use Scope_Stack;
   begin
      return This.Scopes.Table (Last (This.Scopes)).Scope_Entity;
   end Get_Scope_Entity;

   ----------------------
   -- Set_Scope_Entity --
   ----------------------

   procedure Set_Scope_Entity
     (This : access Generator_Record;
      E    : Entity_Id) is
      use Scope_Stack;
   begin
      This.Scopes.Table (Last (This.Scopes)).Scope_Entity := E;
   end Set_Scope_Entity;
   
   --------------------------
   -- Get_Declarative_List --
   --------------------------
   
   function Get_Declarative_List
     (This : access Generator_Record) return List_Id is
      use Scope_Stack;
   begin
      return This.Scopes.Table (Last (This.Scopes)).Declarative_List;
   end Get_Declarative_List;
   
   --------------------------
   -- Set_Declarative_List --
   --------------------------
   
   procedure Set_Declarative_List
     (This : access Generator_Record;
      L    : List_Id) is
      use Scope_Stack;
   begin
      This.Scopes.Table (Last (This.Scopes)).Declarative_List := L;
   end Set_Declarative_List;
   
   ---------------------------
   -- Declare_Current_Scope --
   ---------------------------
   
   procedure Declare_Current_Scope 
     (This : access Generator_Record;
      Decl : Node_Id) is
      
      use Scope_Stack;
      
      Lst : Natural;
   begin
      loop
	 Lst := Last (This.Scopes);
	 exit when not This.Scopes.Table (Lst).Extra_Scope;
      end loop;
      
      if Is_Empty_List (This.Scopes.Table (Lst).Declarative_List)
      then 
	This.Scopes.Table (Lst).Declarative_List := New_List;
      end if;
      
      Append (Decl, This.Scopes.Table (Lst).Declarative_List);
   end Declare_Current_Scope;
   
   ----------------------
   -- Gnerate_And_List --
   ----------------------

   procedure Generate_And_List
     (This : access Generator_Record;
      List : List_Id) is
      
      Interf : access Generator_Record'Class := This;
      Ob   : Output_Buffer := This.Get_Output_Buffer;
      Node : Node_Id;
   begin
      if Is_Non_Empty_List (List) then
         Node := First (List);
         loop
            Interf.Generate_Node (Node);
            Next (Node);
            exit when Node = Empty;
            Write_Str (Ob, " and ");
         end loop;
      end if;
   end Generate_And_List;

   -----------------------
   -- Generate_Bar_List --
   -----------------------

   procedure Generate_Bar_List
     (This : access Generator_Record;
      List : List_Id) is
      
      Interf : access Generator_Record'Class := This;
      Ob     : Output_Buffer := This.Get_Output_Buffer;
      Node   : Node_Id;
   begin
      if Is_Non_Empty_List (List) then
         Node := First (List);
         loop
            Interf.Generate_Node (Node);
            Next (Node);
            exit when Node = Empty;
            Write_Str (Ob, " | ");
         end loop;
      end if;
   end Generate_Bar_List;

   -------------------------
   -- Generate_Comma_List --
   -------------------------

   function Generate_Comma_List
     (This : access Generator_Record;
      List : List_Id) return Integer is
      
      Interf : access Generator_Record'Class := This;
      Ob   : Output_Buffer := This.Get_Output_Buffer;
      Node : Node_Id;
      Num  : Integer := 0;
   begin
      if Is_Non_Empty_List (List) then
         Node := First (List);
         loop
            if Nkind (Node) /= N_Null_Statement then
               Interf.Generate_Node (Node);
               Num := Num + 1;

--                 if Last_Char = ';' then
--                    Delete_Last_Char;
--                 end if;
            end if;

            Next (Node);
            exit when Node = Empty;

            if Nkind (Node) /= N_Null_Statement then
               Write_Str (Ob, ", ");
            end if;
         end loop;
      end if;

      return Num;
   end Generate_Comma_List;

   procedure Generate_Comma_List
     (This : access Generator_Record;
      List : List_Id) is
      
      Ignore : Integer;
   begin
      Ignore := Generate_Comma_List (This, List);
   end Generate_Comma_List;

   ----------------------------
   -- Generate_Indented_List --
   ----------------------------

   procedure Generate_Indented_List
     (This : access Generator_Record;
      List : List_Id) is
      
      Interf : access Generator_Record'Class := This;
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      Indent_Begin (Ob);
      Interf.Generate_Node_List (List);
      Indent_End (Ob);
   end Generate_Indented_List;

   ------------------------
   -- Generate_Left_Opnd --
   ------------------------

   procedure Generate_Left_Opnd
     (This : access Generator_Record;
      N    : Node_Id) is
      
      Interf : access Generator_Record'Class := This;
      Opnd : constant Node_Id := Left_Opnd (N);
   begin
      Interf.Generate_Node_Paren (Opnd);
   end Generate_Left_Opnd;

   ------------------------
   -- Generate_Node_List --
   ------------------------

   procedure Generate_Node_List
     (This      : access Generator_Record;
      List      : List_Id; 
      New_Lines : Boolean := False) is
      
      Interf : access Generator_Record'Class := This;
      Node : Node_Id;
   begin
      if Is_Non_Empty_List (List) then
         Node := First (List);

         loop
            Interf.Generate_Node (Node);
            Next (Node);
            exit when Node = Empty;
         end loop;
      end if;
   end Generate_Node_List;

   -------------------------
   -- Generate_Node_Paren --
   -------------------------

   procedure Generate_Node_Paren
     (This : access Generator_Record;
      N    : Node_Id) is
      
      function Parens_Needed (N : Node_Id) return Boolean;
      
      function Parens_Needed (N : Node_Id) return Boolean is
	 P : constant Node_Id := Parent (N);
      begin
	 if Nkind (P) = N_Assignment_Statement then
	    return N /= Expression (P);
	 else
	    return True;
	 end if;
      end Parens_Needed;
      
      Interf : access Generator_Record'Class := This;
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      --  Add parens if we have an operator or short circuit operation. But
      --  don't add the parens if already parenthesized, since we will get
      --  them anyway and don't add if definitely not needed.

      if (Nkind (N) in N_Op
           or else Nkind_In (N, N_And_Then,
                                N_Explicit_Dereference,
--                                  N_If_Expression,
                                N_In,
                                N_Not_In,
                                N_Or_Else))
        and then Parens_Needed (N)
      then
         Write_Char (Ob, '(');
         Interf.Generate_Node (N);
         Write_Char (Ob, ')');
      else
         Interf.Generate_Node (N);
      end if;
   end Generate_Node_Paren;

   -----------------------
   -- Generate_Opt_Node --
   -----------------------

   procedure Generate_Opt_Node
     (This : access Generator_Record;
      Node : Node_Id) is
      
      Interf : access Generator_Record'Class := This;
      Ob : Output_Buffer := This.Get_Output_Buffer;
   begin
      if Present (Node) then
         Write_Char (Ob, ' ');
         Interf.Generate_Node (Node);
      end if;
   end Generate_Opt_Node;

   ----------------------------
   -- Generate_Opt_Node_List --
   ----------------------------

   procedure Generate_Opt_Node_List
     (This : access Generator_Record;
      List : List_Id) is
      Interf : access Generator_Record'Class := This;
   begin
      if Present (List) then
         Interf.Generate_Node_List (List);
      end if;
   end Generate_Opt_Node_List;

   -------------------------
   -- Generate_Right_Opnd --
   -------------------------

   procedure Generate_Right_Opnd
     (This : access Generator_Record;
      N    : Node_Id) is
      
      Interf : access Generator_Record'Class := This;
      Opnd : constant Node_Id := Right_Opnd (N);
   begin
      Interf.Generate_Node_Paren (Opnd);
   end Generate_Right_Opnd;
   
end Reflex.Generators;
