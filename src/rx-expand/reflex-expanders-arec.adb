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

with Atree;    use Atree;
with Checks;   use Checks;
with Csets;    use Csets;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Tss;  use Exp_Tss;
with Exp_Unst; use Exp_Unst;
with Exp_Util; use Exp_Util;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Osint;    use Osint;
with Osint.C;  use Osint.C;
with Output;   use Output;
with Restrict; use Restrict;
with Rident;   use Rident;
with Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Eval; use Sem_Eval;
with Sem_Mech; use Sem_Mech;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Table;
with Ttypes;   use Ttypes;
with Types;    use Types;
with Uintp;    use Uintp;
with Urealp;   use Urealp;
with System.HTable; use System.HTable;

with Glips.Outputs; use Glips.Outputs;
with Glips.Fat_Pointers; use Glips.Fat_Pointers;

package body Reflex.Expanders.Arec is
   
   ------------
   -- ARECnF --
   ------------

   function ARECnF (Subp_Id : Entity_Id) return Node_Id is
   begin
      return Subps.Table (Subp_Index (Subp_Id)).ARECnF;
   end ARECnF;

   ------------
   -- ARECnU --
   ------------

   function ARECnU (Subp_Id : Entity_Id) return Node_Id is
   begin
      return Subps.Table (Subp_Index (Subp_Id)).ARECnU;
   end ARECnU;

   -----------------
   -- AREC_Entity --
   -----------------

   function AREC_Entity (N : Node_Id) return Entity_Id is
      Subp : Entity_Id := Current_Subp_Entity;

   begin
      pragma Assert (Nkind (N) = N_Identifier);
      loop
	 declare
	    J    : constant SI_Type := UI_To_Int (Subps_Index (Subp));
	    Elmt : Elmt_Id;
	    STJ  : Subp_Entry renames Subps.Table (J);

	 begin
	    if Present (STJ.Uents) then
	       Elmt := First_Elmt (STJ.Uents);

	       while Present (Elmt) loop
		  if Entity (N) = Activation_Record_Component (Node (Elmt))
		  then
		     return Node (Elmt);
		  end if;

		  Next_Elmt (Elmt);
	       end loop;
	    end if;
	 end;

	 exit when No (Enclosing_Subprogram (Subp));
	 Subp := Enclosing_Subprogram (Subp);
      end loop;

      return Empty;
   end AREC_Entity;

   ---------------------
   -- AREC_Subprogram --
   ---------------------

   function AREC_Subprogram (Formal : Entity_Id) return Entity_Id is
      Subp : Entity_Id := Current_Subp_Entity;

   begin
      pragma Assert (Is_Formal (Formal));
      loop
	 declare
	    J    : constant SI_Type := UI_To_Int (Subps_Index (Subp));
	    Elmt : Elmt_Id;
	    STJ  : Subp_Entry renames Subps.Table (J);

	 begin
	    if Present (STJ.Uents) then
	       Elmt := First_Elmt (STJ.Uents);

	       while Present (Elmt) loop
		  if Node (Elmt) = Formal then
		     return Subp;
		  end if;

		  Next_Elmt (Elmt);
	       end loop;
	    end if;
	 end;

	 exit when No (Enclosing_Subprogram (Subp));
	 Subp := Enclosing_Subprogram (Subp);
      end loop;

      return Empty;
   end AREC_Subprogram;

   --------------------
   -- Get_AREC_Field --
   --------------------

   function Get_AREC_Field (N : Node_Id) return Node_Id is
   begin
      pragma Assert (Is_AREC_Reference (N));
      return First (Expressions (N));
   end Get_AREC_Field;

   -----------------------
   -- Is_AREC_Reference --
   -----------------------

   function Is_AREC_Reference (N : Node_Id) return Boolean is
      Typ      : constant Entity_Id := Etype (N);
      Full_Typ : Entity_Id;
      Expr     : Node_Id;
      Pref     : Node_Id;

   begin
      if Is_Access_Type (Typ) then
	 Full_Typ := Get_Full_View (Designated_Type (Typ));
      else
	 Full_Typ := Get_Full_View (Typ);
      end if;

      if Nkind (N) = N_Attribute_Reference
	and then Get_Attribute_Id (Attribute_Name (N)) = Attribute_Deref
	and then Is_Array_Type (Full_Typ)
	and then Nkind (First (Expressions (N))) = N_Selected_Component
      then
	 Expr := First (Expressions (N));

	 --  Locate the ultimate prefix

	 Pref := Prefix (Expr);
	 while Nkind_In (Pref, N_Explicit_Dereference,
			 N_Selected_Component)
	 loop
	    Pref := Prefix (Pref);
	 end loop;

	 if Nkind (Pref) = N_Identifier
	   and then Entity (Pref) = ARECnF (Current_Subp_Entity)
	   and then Present (AREC_Entity (Selector_Name (Expr)))
	 then
	    return True;
	 end if;
      end if;

      return False;
   end Is_AREC_Reference;

   -------------------------------------
   -- Write_Up_Level_Formal_Reference --
   -------------------------------------

   procedure Write_Up_Level_Formal_Reference
     (This   : access Reflex_Expander_Record;
      Subp   : Entity_Id;
      Formal : Entity_Id)
   is
      Ob : Output_Buffer := This.Get_Output_Buffer;
      
      procedure Write_Up_Level_AREC_Access
	(Current_Subp   : Entity_Id;
	 Enclosing_Subp : Entity_Id);
      --  Output code that climbs through the activation records from
      --  Current_Subp to Enclosing_Subp.

      --------------------------------
      -- Write_Up_Level_AREC_Access --
      --------------------------------

      procedure Write_Up_Level_AREC_Access
	(Current_Subp   : Entity_Id;
	 Enclosing_Subp : Entity_Id)
      is
      begin
	 if Get_Level (Enclosing_Subp, Current_Subp) > 1 then
	    declare
	       Subp_Id : Entity_Id := Enclosing_Subprogram (Current_Subp);

	    begin
	       while Subp_Id /= Enclosing_Subp loop
		  Write_Id (This, ARECnU (Subp_Id));
		  Write_Str (Ob, ".");

		  Subp_Id := Enclosing_Subprogram (Subp_Id);
	       end loop;
	    end;
	 end if;
      end Write_Up_Level_AREC_Access;

      --  Start of processing for Write_Up_Level_Formal_Reference

   begin
      --  Generate
      --    (*((_fatptr_UNCarray *) ARECnF->{ARECnU->})).
      --    ARECnF->{ARECnU->})).

      Write_Id (This, ARECnF (Subp));
      Write_Str (Ob, ".");

      Write_Up_Level_AREC_Access
	(Current_Subp   => Current_Subp_Entity,
	 Enclosing_Subp => AREC_Subprogram (Formal));

      Write_Id (This, Formal);
   end Write_Up_Level_Formal_Reference;

end Reflex.Expanders.Arec;

