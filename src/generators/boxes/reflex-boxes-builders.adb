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

with Artics.Buffers; use Artics.Buffers;

with Ada.Text_IO; use Ada.Text_IO;
with Sinfo; use Sinfo;
with Atree; use Atree;

with Unity.Gen.Dispatch; use Unity.Gen.Dispatch;
with Reflex.Boxes.Ch5; use Reflex.Boxes.Ch5;
with Reflex.Boxes.Dispatch_Ladder_Emitor; 
use Reflex.Boxes.Dispatch_Ladder_Emitor;

package body Reflex.Boxes.Builders is
      
   -----------------
   -- New_Builder --
   -----------------
   
   function New_Builder return Builder_Ptr is
      This : Builder_Ptr := new Builder_Record'(No_Builder_Record);
   begin
      This.Literal_Buffer := New_Output_Buffer;
      This.Ladder_Emitor := New_Ladder_Emitor;
      
      Scope_Stack.Init (This.Scopes);
      return This;
   end New_Builder;
   
   --------------------------
   -- Free_Reflex_Expander --
   --------------------------
   
   procedure Free_Builder (This : in out Builder_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Builder_Record, Builder_Ptr);
      use Scope_Stack;
   begin
      if This.Scopes /= Scope_Stack.No_Instance then
         Scope_Stack.Free (This.Scopes);
      end if;
      Free (This);
   end Free_Builder;
   
   --------------
   -- Get_Subp --
   --------------
   
   function Get_Subp (This : access Builder_Record) return Node_Id is
   begin
      return This.Subp;
   end Get_Subp;
   
   --------------
   -- Set_Subp --
   --------------
   
   procedure Set_Subp
     (This : access Builder_Record;
      Subp : Node_Id) is
   begin
      This.Subp := Subp;
   end Set_Subp;
   
   --------------------
   -- Get_Subp_Boxes --
   --------------------
   
   function Get_Subp_Boxes
     (This : access Builder_Record) return Boxes_Lists.List is
   begin
      return This.Subp_Boxes;
   end Get_Subp_Boxes;
   
   --------------------
   -- Set_Subp_Boxes --
   --------------------
   
   procedure Set_Subp_Boxes
     (This : access Builder_Record;
      L    : Boxes_Lists.List) is
   begin
      This.Subp_Boxes := L;
   end Set_Subp_Boxes;

   ---------------------
   -- In_If_Statement --
   ---------------------
   
   function In_If_Statement (This : access Builder_Record) return Boolean is
   begin
      return This.Scopes.Table (Last (This.Scopes)).In_If_Statement;
   end In_If_Statement;
   
   -------------------------
   -- Set_In_If_Statement --
   -------------------------
   
   procedure Set_In_If_Statement
     (This : access Builder_Record;
      V    : Boolean) is
   begin
      This.Scopes.Table (Last (This.Scopes)).In_If_Statement := V;
   end Set_In_If_Statement;
         
   ---------------------------
   -- Get_Literal_Generator --
   ---------------------------
   
   function Get_Literal_Generator 
     (This : access Builder_Record) return access Unity_Generator_Record is
   begin
      return This.Literal_Generator;
   end Get_Literal_Generator;
   
   ---------------------------
   -- Set_Literal_Generator --
   ---------------------------
   
   procedure Set_Literal_Generator 
     (This : access Builder_Record;
      Gen  : access Unity_Generator_Record) is
   begin
      This.Literal_Generator := Gen;
   end Set_Literal_Generator;
   
   ------------------------
   -- Get_Literal_Buffer --
   ------------------------
   
   function Get_Literal_Buffer
     (This : access Builder_Record) return Output_Buffer is
   begin
      return This.Literal_Buffer;
   end Get_Literal_Buffer;
   
   ------------------------
   -- Set_Literal_Buffer --
   ------------------------
   
   procedure Set_Literal_Buffer
     (This : access Builder_Record;
      Ob   : Output_Buffer) is
   begin
      This.Literal_Buffer := Ob;
   end Set_Literal_Buffer;
   
   -----------------------
   -- Get_Ladder_Emitor --
   -----------------------
   
   function Get_Ladder_Emitor
     (This : access Builder_Record) return access Ladder_Emitor_Record is
   begin
      return This.Ladder_Emitor;
   end Get_Ladder_Emitor;
   
   -----------------------
   -- Set_Ladder_Emitor --
   -----------------------
   
   procedure Set_Ladder_Emitor
     (This : access Builder_Record;
      Ld   : access Ladder_Emitor_Record) is
   begin
      This.Ladder_Emitor := Ld;
   end Set_Ladder_Emitor;
   
   ----------------
   -- Open_Scope --
   ----------------

   procedure Open_Scope
     (This : access Builder_Record;
      N    : Node_Id := Empty) is
      use Scope_Stack;
   begin
      Increment_Last (This.Scopes);
      This.Scopes.Table (Last (This.Scopes)) := No_Scope_Stack_Entry;
      This.Scopes.Table (Last (This.Scopes)).Scope_Node := N;
   end Open_Scope;

   -----------------
   -- Close_Scope --
   -----------------

   procedure Close_Scope
     (This : access Builder_Record) is
      use Scope_Stack;
   begin
      Decrement_Last (This.Scopes);
   end Close_Scope;

   -----------------
   -- Append_Rung --
   -----------------
   
   procedure Append_Rung
     (This : access Builder_Record;
      R    : access Rung_Record) is
   begin
      Rungs_Lists.Append (This.Rungs, Rung_Ptr (R));
   end Append_Rung;
   
   ----------------
   -- Append_Box --
   ----------------
   
   procedure Append_Box
     (This : access Builder_Record;
      B    : access Box_Record'Class) is
   begin
      Boxes_Lists.Append (This.Subp_Boxes, Box_Class_Ptr (B));
   end Append_Box;
   
   -----------------
   -- Build_Rungs --
   -----------------
   
   procedure Build_Rungs (This : access Builder_Record) is
      R : access Rung_Record;
      Nb :Integer := 0;
   begin
      for B of This.Subp_Boxes loop
         R := New_Rung;
	 Set_Enclosing_Box (R, B);
         B.Place_Matrix (R.Get_Matrix);
         R.Set_Max_Height (B.Get_Height);
         This.Append_Rung (R);
         Nb := Nb +1;
      end loop;
   end Build_Rungs;
   
   -----------------
   -- Build_Boxes --
   -----------------
   
   procedure Build_Boxes 
     (This : access Builder_Record;
      Node : Node_Id) is
   begin
      This.Set_Subp (Node);
      
      This.Open_Scope (Node);
      
      Boxes_Build_Handled_Sequence_Of_Statements
        (This, Handled_Statement_Sequence (Node));
   end Build_Boxes;
      
   ---------------------------------
   -- Generate_Literal_Expression --
   ---------------------------------
   
   procedure Generate_Literal_Expression
     (This : access Builder_Record;
      Node : Node_Id) is
      
      Ob  : Output_Buffer := This.Get_Literal_Buffer;
      Lg  : access Unity_Generator_Record;
      Prv : Output_Buffer;
   begin
      Reset_Buffer (Ob);
      
      Lg := This.Get_Literal_Generator;
      Prv := Lg.Get_Output_Buffer;
      Lg.Set_Output_Buffer (Ob);
      
      if Present (Node) then
         Unity.Gen.Generate_Node (Lg, Node);
      else
         Write_Eol (Ob);
         Write_Str (Ob, "NO STATEMENT");
         Write_Eol (Ob);
      end if;
      
      Lg.Set_Output_Buffer (Prv);
   end Generate_Literal_Expression;
   
   --------------------
   -- Generate_Rungs --
   --------------------
   
   procedure Generate_Rungs (This : access Builder_Record) is
      
      B            : access Box_Record'Class;
      Bnxt         : access Box_Record'Class;
      Matrix       : access Matrix_Record;
      Hmax         : Natural;
      Empty_Count  : Natural;
      Hlink_Count  : Natural;
      Line_Count   : Natural;
      Column_Count : Natural;
      Lg           : access Unity_Generator_Record;
      Last         : Boolean;

   begin
      Put_Line ("Generate_Rungs Begin");
      
      Reset_Buffer (This.Ladder_Emitor.Get_Output_Buffer);
      
      Lg := This.Get_Literal_Generator;
      This.Ladder_Emitor.Set_Literal_Generator (Lg);
            
      Emit_Header (This.Ladder_Emitor);
      
      Emit_Begin_Network (This.Ladder_Emitor);

      for R of This.Rungs loop
	 Put_Line ("Inclosing Box " & Get_Box_Kind (R.Get_Enclosing_Box)'Img);
         Matrix := R.Get_Matrix;
         if Matrix /= null then
	    
            Hmax         := R.Get_Max_Height;
            Line_Count   := Matrix.Get_Lines_Count;
            Column_Count := Matrix.Get_Columns_Count;
	    
	    Emit_Begin_Rung (This.Ladder_Emitor, R);
	    
            for I in 0.. (Line_Count - 1) loop
               exit when I >= Hmax;
	       
               Empty_Count := 0;
               Hlink_Count := 0;
	       
               Emit_Begin_Line (This.Ladder_Emitor);
	       
               for J in 0.. (Column_Count - 1) loop
                  B := Matrix.Get_Item (J, I);
                  
                  if B /= The_Busy_Box then
                     
                     Last := J + 1 = Column_Count - 1;
		    
                     if not Last then
                        Bnxt := Matrix.Get_Item (J + 1, I);
                     end if;
		    		    
                     --  Empty box
		    
                     if B = null then
			Put_Line ("    Empty ");
		       
                        Empty_Count := Empty_Count + 1;
                        if Last 
                          or else Bnxt /= null
                        then
                           Emitor_Dispatch
                             (This.Ladder_Emitor, null, Empty_Count);
                           Empty_Count := 0;
                        end if;
		       
                        --  Hlink Vlink
		       
                     elsif Is_Hlink_Vlink_Box (B) then
			Put_Line ("    Hlink_Vlink ");
		       
                        Hlink_Count := Hlink_Count + 1;
                        Emitor_Dispatch 
                          (This.Ladder_Emitor, 
                           The_Hlink_Vlink_Box, Hlink_Count);
                        Hlink_Count := 0;
		       
                        --  Hlink
		       
                     elsif Is_Hlink_Box (B) then
			Put_Line ("    Hlink ");
		       
                        Hlink_Count := Hlink_Count + 1;
		       
                        if Last 
                          or else Bnxt = null
                          or else (not Is_Hlink_Box (Bnxt) 
                                   and not Is_Hlink_Vlink_Box (Bnxt))
                        then
                           Emitor_Dispatch 
                             (This.Ladder_Emitor, The_Hlink_Box, Hlink_Count);
                           Hlink_Count := 0;
                        end if;
                        
                        --  Normal Box
                        
                     else
			Put_Line ("    B " & Get_Box_Kind (B)'Img);
                        Emitor_Dispatch (This.Ladder_Emitor, B, 0);
                     end if;
		  else
		     Put_Line ("    Busy Box ");
                  end if;
               end loop;
               Emit_End_Line (This.Ladder_Emitor);
            end loop;
	    
	    Emit_End_Rung (This.Ladder_Emitor, R);
         end if;
      end loop;
      
       Emit_End_Network (This.Ladder_Emitor);
      
      Emit_Tailer (This.Ladder_Emitor);
      declare
         S : String := Buffer_To_String (This.Ladder_Emitor.Get_Output_Buffer);
      begin
         null;
         Put_Line (S);
      end;
      Put_Line ("Generate_Rungs End");
   end Generate_Rungs;

   --------------
   -- Dump_Box --
   --------------
   
   procedure Dump_Box
     (This : access Builder_Record;
      B    : access Box_Record'Class) is
   begin
      Put_Line ("");
      Put_Line ("=======> Dump for Node : ");
      This.Generate_Literal_Expression (Get_Node (B));
      declare
         S : String := Buffer_To_String (This.Get_Literal_Buffer);
      begin
         Put_Line (S);
         Put_Line ("");
      end;
      Put_Line ("=> Box ");
      
      B.Dump_Box;
   end Dump_Box;
   
   -----------------
   -- Dump_Matrix --
   -----------------
   
   procedure Dump_Matrix (This : access Builder_Record) is
      Line_Count : Natural;
      Column_Count : Natural;
      Hmax : Natural;
      B : access Box_Record'Class;
      Matrix : access Matrix_Record;
   begin
      Put_Line (" ==================================== ");

      for R of This.Rungs loop
         
         Matrix := R.Get_Matrix;
         if Matrix /= null then
            
            Hmax         := R.Get_Max_Height;
            Line_Count   := Matrix.Get_Lines_Count;
            Column_Count := Matrix.Get_Columns_Count;
      
            for I in 0.. (Column_Count - 1) loop
               Put_Line (" ----------------- ");
               Put_Line ("Line = " & I'Img);
               Put_Line ("");
               
               for J in 0.. (Line_Count - 1) loop
                  B := Matrix.Get_Item (I, J);
                  if B = null then
                     Put ("Empty;");
                  elsif B = The_Hlink_Box then
                     Put ("Hink;");
                  else
                     Put_Line ("B kind = " & Get_Box_Kind (B)'Img);
                  end if;
               end loop;
               
            end loop;
         else
            Put_Line ("no_matrix");
         end if;
      end loop;
      
      Put_Line ("");
      Put_Line (" ==================================== ");
   end Dump_Matrix;
   
   -------------------
   -- Dump_A_Matrix --
   -------------------
   
   procedure Dump_A_Matrix (Matrix : access Matrix_Record) is
      Line_Count : Natural;
      Column_Count : Natural;
      B : access Box_Record'Class;
   begin
      Put_Line (" ==================================== ");

      if Matrix /= null then
	 
         Line_Count   := Matrix.Get_Lines_Count;
         Column_Count := Matrix.Get_Columns_Count;
	 
         for I in 0.. (Line_Count - 1) loop
            Put_Line (" ----------------- ");
            Put_Line ("Line = " & I'Img);
            Put_Line ("");
	    
            for J in 0.. (Column_Count - 1) loop
               B := Matrix.Get_Item (J, I);
               if B = null then
                  Put ("Empty;");
               elsif B = The_Hlink_Box then
                  Put ("Hlink;");
               else
                  Put_Line ("B kind = " & Get_Box_Kind (B)'Img);
               end if;
            end loop;
         end loop;
      else
         Put_Line ("no_matrix");
      end if;
      
      Put_Line ("");
      Put_Line (" ==================================== ");
   end Dump_A_Matrix;
   
end Reflex.Boxes.Builders;
