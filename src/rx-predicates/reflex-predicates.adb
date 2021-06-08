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

with Atree;  use Atree;
with Sinfo;  use Sinfo;
with Nlists; use Nlists;

with Reflex.Boxes.Utils; use Reflex.Boxes.Utils;
with Reflex_Options;     use Reflex_Options;
with Stand; use Stand;

package body Reflex.Predicates is

   ----------------------
   -- Expression_Width --
   ----------------------

   function Expression_Width (Node : Node_Id) return Natural is
      Wl : Natural := 0;
      Wr : Natural := 0;
   begin

      case Nkind (Node) is
         when N_Op_And =>
            Wl := Expression_Width (Left_Opnd (Node));
            Wr := Expression_Width (Right_Opnd (Node));
            return Wl + Wr;

         when N_Op_Or =>
            Wl := Expression_Width (Left_Opnd (Node));
            Wr := Expression_Width (Right_Opnd (Node));
            return Max (Wl, Wr);

         when N_Op_Compare =>
            return 2;

         when N_Identifier =>
            return 1;

         when others =>
            return 0;
      end case;
   end Expression_Width;

   ------------------------------
   -- Expression_Height_Worker --
   ------------------------------

   function Expression_Height_Worker (Node : Node_Id) return Natural is
      Hl : Natural := 0;
      Hr : Natural := 0;
   begin

      case Nkind (Node) is
         when N_Op_And =>
            Hl := Expression_Height_Worker (Left_Opnd (Node));
            Hr := Expression_Height_Worker (Right_Opnd (Node));
            return Max (Hl, Hr);

         when N_Op_Or =>
            Hl := Expression_Height_Worker (Left_Opnd (Node)) + 1;
            Hr := Expression_Height_Worker (Right_Opnd (Node)) + 1;
            return Max (Hl, Hr);

         when others =>
            return 0;
      end case;
   end Expression_Height_Worker;

   -----------------------
   -- Expression_Height --
   -----------------------

   function Expression_Height (Node : Node_Id) return Natural is
      H : Natural := 0;
   begin
      H := Expression_Height_Worker (Node) + 1;
      return H;
   end Expression_Height;

   -----------------
   -- Resize_Expr --
   -----------------

   procedure Resize_Expr (Node : Node_Id) is
      H : Natural;
      W : Natural;

      Nb_Line     : Natural;
      Ob_Per_Line : Natural;
   begin

      H := Expression_Height (Node);
      W := Expression_Width (Node);

      if W > Max_Unity_Ladder_Horizontal then

         -- if we have only N_Op_And :

         Nb_Line     := W mod (Max_Unity_Ladder_Horizontal) + 1;
         Ob_Per_Line := W mod (Nb_Line) + 1;

      end if;

      if H > Max_Unity_Ladder_Vertical then
         null;
      end if;
   end Resize_Expr;
   
--     -----------------------------
--     -- If_Statement_Predicates --
--     -----------------------------
--  
--     procedure If_Statement_Predicates (Node : Node_Id) is
--  
--        Stmts    : List_Id;
--        N_Elsifs : Node_Id;
--     begin
--  
--        --  if part
--  
--        Stmts := Then_Statements (Node);
--        --  Set_Expandable_If (Node, Check_Assign_Etype (Stmts));
--  
--        --  Elsif parts
--  
--        if Present (Elsif_Parts (Node)) then
--           N_Elsifs := First (Elsif_Parts (Node));
--           while Present (N_Elsifs) loop
--              Stmts := Then_Statements (N_Elsifs);
--              --  Set_Expandable_If (N_Elsifs, Check_Assign_Etype (Stmts));
--  
--              Next (N_Elsifs);
--           end loop;
--        end if;
--  
--     end If_Statement_Predicates;

--     ------------------------
--     -- Check_Assign_Etype --
--     ------------------------
--  
--     function Check_Assign_Etype (Stmts : List_Id) return Boolean is
--        Stmt : Node_Id := First (Stmts);
--        Bool : Boolean := False;
--     begin
--        while Present (Stmt) loop
--           if Nkind (Stmt) = N_Assignment_Statement then
--              if Etype (Stmt) /= Standard_Boolean then
--                 Bool := True;
--              end if;
--           else
--              Bool := True;
--           end if;
--  
--           Next (Stmt);
--        end loop;
--  
--        return Bool;
--     end Check_Assign_Etype;
   
   -------------------------------
   -- Has_Only_Simple_Statement --
   -------------------------------
   
   function Has_Only_Simple_Statement (N : Node_Id) return Boolean is
      Elif : Node_Id;
   begin
      case Nkind (N) is
	 when N_If_Statement =>
	    
	    if not Is_Simple_Statements_List (Then_Statements (N)) then
	       return False;
	    end if;
      
	    --  Elsif Parts
	    
	    if Present (Elsif_Parts (N)) then
	       
	       Elif := First (Elsif_Parts (N));
	       while Present (Elif) loop
		  
		  if not Is_Simple_Statements_List (Then_Statements (Elif)) 
		  then
		     return False;
		  end if;
	    
		  Next (Elif);
	       end loop;
	    end if;
      
	    return True;
	    
	 when others =>
	    return False;
      end case;
   end Has_Only_Simple_Statement;
     
   -------------------------------
   -- Is_Simple_Statements_List --
   -------------------------------

   function Is_Simple_Statements_List (Stmts : List_Id) return Boolean is
      Stmt : Node_Id;
   begin
      if not Is_Empty_List (Stmts) then
	 
	 Stmt := First (Stmts);
	 while Present (Stmt) loop
	    if not Is_Simple_Statement (Stmt) then
	       return False;
	    end if;
	    
	    Next (Stmt);
	 end loop;
      end if;
      
      return True;
   end Is_Simple_Statements_List;

   -------------------------
   -- Is_Simple_Statement --
   -------------------------
   
   function Is_Simple_Statement (Stmt : Node_Id) return Boolean is
   begin
      return Nkind_In
	      (Stmt, 
	       N_Null_Statement,
	       N_Assignment_Statement, 
	       N_Goto_Statement, 
	       N_Procedure_Call_Statement);
   end Is_Simple_Statement;
   
   -----------------------------
   -- Is_Null_Statements_List --
   -----------------------------

   function Is_Null_Statements_List (Stmts : List_Id) return Boolean is
      Stmt : Node_Id;
   begin
      if Is_Empty_List (Stmts) then
	 return True;
      end if;
      
      Stmt := First (Stmts);
      while Present (Stmt) loop
	 if Nkind (Stmt) /= N_Null_Statement then
	    return False;
	 end if;
	 Next (Stmt);
      end loop;
      
      return True;
   end Is_Null_Statements_List;
   
   --------------------------------
   -- Must_Statements_List_Break --
   --------------------------------
   
   function Must_Statements_List_Break (Stmts : List_Id) return Boolean is
      
      Count : Natural;
      Stmt  : Node_Id;
   begin
      if not Is_Empty_List (Stmts) then
	 Count := 0;
	 Stmt := First (Stmts);
	 while Present (Stmt) loop
	    Count := Count + 1;
	    if Count > Max_Unity_Ladder_Vertical 
	      or else Nkind (Stmt) = N_Procedure_Call_Statement 
	    then
	       return True;
	    end if;
	    
	    Next (Stmt);
	 end loop;
      end if;
      
      return False;
   end Must_Statements_List_Break;
   
   -------------------
   -- Must_Break_If --
   -------------------
   
   function Must_Break_If (N : Node_Id) return Boolean is
      Elif : Node_Id;
   begin
      --  Then parts
      
      if Must_Statements_List_Break (Then_Statements (N)) then
	 return True;
      end if;
      
      --  Elsif Parts
      
      if Present (Elsif_Parts (N)) then
	 
	 Elif := First (Elsif_Parts (N));
	 while Present (Elif) loop
	    
	    if Must_Statements_List_Break (Then_Statements (Elif)) then
	       return True;
	    end if;
	    
	    Next (Elif);
	 end loop;
      end if;
      
      --  Else Parts
      
      if Present (Else_Statements (N)) then
	 if Must_Statements_List_Break (Else_Statements (N)) then
	    return True;
	 end if;
      end if;
      
      return False;
   end Must_Break_If;
     
end Reflex.Predicates;
