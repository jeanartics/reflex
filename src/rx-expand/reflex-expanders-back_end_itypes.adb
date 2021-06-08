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

with Ada.Text_Io; use Ada.Text_IO;

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
with Glips.Utils; use Glips.Utils;
with Glips.Fat_Pointers; use Glips.Fat_Pointers;

package body Reflex.Expanders.Back_End_Itypes is
   
   Entities_With_Back_End_Itype      : Elist_Id := No_Elist;
   Entities_With_Back_End_AREC_Itype : Elist_Id := No_Elist;

   procedure Register_Entity_With_Back_End_AREC_Itype (E : Entity_Id);
   --  Register E in the list of entities with extra AREC back-end itype

   procedure Register_Entity_With_Back_End_Itype (E : Entity_Id);
   --  Register E in the list of entities with extra back-end itype

   -----------------------------
   -- Declare_Back_End_Itypes --
   -----------------------------

   procedure Declare_Back_End_Itypes (Subp_Id : Entity_Id) is
      function Back_End_Itypes_Needed return Boolean;
      --  Return True if Subp_Id needs back-end itypes

      function Back_End_Itype_Needed (Formal : Entity_Id) return Boolean;
      --  Return True if Formal requires a back-end itype

      procedure Declare_Itype
	(Formal : Node_Id; 
	 Typ    : Entity_Id);
      --  Output the typedef which would correspond with the itype of an
      --  access to an unconstrained multidimensional array type.

      procedure Declare_AREC_Itype
	(Subp   : Entity_Id; 
	 Formal : Entity_Id);
      --  Output the typedef which would correspond with the itype of the
      --  unconstrained multidimensional array type Formal of the enclosing
      --  subprogram Subp.

      ---------------------------
      -- Back_End_Itype_Needed --
      ---------------------------

      function Back_End_Itype_Needed (Formal : Entity_Id) return Boolean is
      begin
	 return
	   Is_Access_Type (Etype (Formal))
	   and then
	   Is_Unconstrained_Array_Type
	   (Get_Type_Full_View (Designated_Type (Etype (Formal))))
	   and then not
	   Is_Unidimensional_Array_Type
	   (Get_Type_Full_View (Designated_Type (Etype (Formal))));
      end Back_End_Itype_Needed;

      ----------------------------
      -- Back_End_Itypes_Needed --
      ----------------------------

      function Back_End_Itypes_Needed return Boolean is
	 Formal : Node_Id;

      begin
	 Formal := First_Formal_With_Extras (Subp_Id);
	 while Present (Formal) loop
	    if Back_End_Itype_Needed (Formal) then
	       return True;
	    end if;

	    Next_Formal_With_Extras (Formal);
	 end loop;

	 --  For nested procedures check if the enclosing subprograms need
	 --  back-end itypes for unconstrained array types.

	 declare
	    E        : Entity_Id;
	    Elmt     : Elmt_Id;
	    Subp     : Entity_Id;
	    Subp_Idx : SI_Type;

	 begin
	    Subp := Enclosing_Subprogram (Current_Subp_Entity);
	    while Present (Subp) loop
	       Subp_Idx := UI_To_Int (Subps_Index (Subp));

	       if Subp_Idx > 0
		 and then Present (Subps.Table (Subp_Idx).Uents)
	       then
		  Elmt := First_Elmt (Subps.Table (Subp_Idx).Uents);
		  while Present (Elmt) loop
		     E := Node (Elmt);

		     if Is_Unconstrained_Array_Type
		       (Get_Type_Full_View (Etype (E)))
		     then
			return True;
		     end if;

		     Next_Elmt (Elmt);
		  end loop;
	       end if;

	       Subp := Enclosing_Subprogram (Subp);
	    end loop;
	 end;

	 return False;
      end Back_End_Itypes_Needed;

      ------------------------
      -- Declare_AREC_Itype --
      ------------------------

      procedure Declare_AREC_Itype (Subp : Entity_Id; Formal : Entity_Id) is
	 Typ : constant Entity_Id := Get_Type_Full_View (Etype (Formal));

      begin
	 Write_Indent;

	 --  Generate
	 --    typedef <Component_Type> itypeId
	 --      [(last[1]-first[1]) + 1]
	 --      [(last[2]-first[2]) + 1]
	 --      ...

	 Write_Indent;
	 Write_Str ("typedef ");
	 Write_Id (Component_Type (Typ));
	 Write_Char (' ');
	 Write_Id (Actual_Subtype (Formal));

	 declare
	    Idx : Pos     := 1;
	    Ind : Node_Id := First_Index (Typ);

	 begin
	    while Present (Ind) loop
	       Write_Str_Col_Check ("[(");
	       Write_Up_Level_Formal_Reference (Subp, Formal);
	       Write_Char ('.');
	       Write_Fatptr_Last (Typ, Idx);

	       Write_Str_Col_Check (" - ");

	       Write_Up_Level_Formal_Reference (Subp, Formal);
	       Write_Char ('.');
	       Write_Fatptr_First (Typ, Idx);

	       Write_Str_Col_Check (") + 1]");

	       Idx := Idx + 1;
	       Next_Index (Ind);
	    end loop;

	    Write_Char (';');
	 end;

	 --  Remember that this entity is defined

	 Register_Entity_With_Back_End_AREC_Itype (Actual_Subtype (Formal));
      end Declare_AREC_Itype;

      -------------------
      -- Declare_Itype --
      -------------------

      procedure Declare_Itype (Formal : Node_Id; Typ : Entity_Id) is
      begin
	 --  Generate
	 --    typedef <Component_Type> itypeId
	 --      [(last[1]-first[1]) + 1]
	 --      [(last[2]-first[2]) + 1]
	 --      ...

	 Write_Indent;
	 Write_Str ("typedef ");
	 Write_Id (Component_Type (Typ));
	 Write_Char (' ');
	 Write_Back_End_Itype_Id (Formal);

	 declare
	    Idx : Pos     := 1;
	    Ind : Node_Id := First_Index (Typ);

	 begin
	    while Present (Ind) loop
	       Write_Str_Col_Check ("[(");
	       Write_Id (Formal);

	       if Pass_Pointer (Formal) then
		  Write_Str ("->");
	       else
		  Write_Char ('.');
	       end if;

	       Write_Fatptr_Last (Typ, Idx);
	       Write_Str_Col_Check (" - ");
	       Write_Id (Formal);

	       if Pass_Pointer (Formal) then
		  Write_Str ("->");
	       else
		  Write_Char ('.');
	       end if;

	       Write_Fatptr_First (Typ, Idx);
	       Write_Str_Col_Check (") + 1]");

	       Idx := Idx + 1;
	       Next_Index (Ind);
	    end loop;

	    Write_Char (';');
	 end;
      end Declare_Itype;

      --  Local variables

      Formal : Node_Id;

      --  Start of processing for Declare_Back_End_Itypes

   begin
      if not Back_End_Itypes_Needed then
	 return;
      end if;

      Indent_Begin;

      --  Declare itypes associated with the formals of Subp_Id

      Formal := First_Formal_With_Extras (Subp_Id);
      while Present (Formal) loop
	 if Back_End_Itype_Needed (Formal) then
	    Register_Entity_With_Back_End_Itype (Formal);
	    Declare_Itype
	      (Formal,
	       Get_Type_Full_View (Designated_Type (Etype (Formal))));
	 end if;

	 Next_Formal_With_Extras (Formal);
      end loop;

      --  Declare itypes of unconstrained array type formals of enclosing
      --  subprograms.

      declare
	 E        : Entity_Id;
	 Elmt     : Elmt_Id;
	 Subp     : Entity_Id;
	 Subp_Idx : SI_Type;

      begin
	 Subp := Enclosing_Subprogram (Current_Subp_Entity);
	 while Present (Subp) loop
	    Subp_Idx := UI_To_Int (Subps_Index (Subp));

	    if Subp_Idx > 0
	      and then Present (Subps.Table (Subp_Idx).Uents)
	    then
	       Elmt := First_Elmt (Subps.Table (Subp_Idx).Uents);
	       while Present (Elmt) loop
		  E := Node (Elmt);

		  if Is_Unconstrained_Array_Type(Get_Type_Full_View (Etype (E)))
		  then
		     Declare_AREC_Itype
		       (Subp   => Subp_Id,
			Formal => E);
		  end if;

		  Next_Elmt (Elmt);
	       end loop;
	    end if;

	    Subp := Enclosing_Subprogram (Subp);
	 end loop;
      end;

      Indent_End;
   end Declare_Back_End_Itypes;

   -----------------------------
   -- Has_Back_End_AREC_Itype --
   -----------------------------

   function Has_Back_End_AREC_Itype (E : Entity_Id) return Boolean is
   begin
      return Contains (Entities_With_Back_End_AREC_Itype, E);
   end Has_Back_End_AREC_Itype;

   ------------------------
   -- Has_Back_End_Itype --
   ------------------------

   function Has_Back_End_Itype (E : Entity_Id) return Boolean is
   begin
      return Contains (Entities_With_Back_End_Itype, E);
   end Has_Back_End_Itype;

   ----------------------------------------------
   -- Register_Entity_With_Back_End_AREC_Itype --
   ----------------------------------------------

   procedure Register_Entity_With_Back_End_AREC_Itype (E : Entity_Id) is
   begin
      Append_New_Elmt (E, Entities_With_Back_End_AREC_Itype);
   end Register_Entity_With_Back_End_AREC_Itype;

   -----------------------------------------
   -- Register_Entity_With_Back_End_Itype --
   -----------------------------------------

   procedure Register_Entity_With_Back_End_Itype (E : Entity_Id) is
   begin
      Append_New_Elmt (E, Entities_With_Back_End_Itype);
   end Register_Entity_With_Back_End_Itype;

   -----------------------------
   -- Write_Back_End_Itype_Id --
   -----------------------------

   procedure Write_Back_End_Itype_Id (E : Entity_Id) is
      pragma Assert (Has_Back_End_Itype (E));
   begin
      Write_Id (E);
      Write_Str ("_Ib");
   end Write_Back_End_Itype_Id;
   
end Reflex.Expanders.Back_End_Itypes;

