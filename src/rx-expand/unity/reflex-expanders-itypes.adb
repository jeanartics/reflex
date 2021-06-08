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
-- with Checks;   use Checks;
with Csets;    use Csets;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
-- with Exp_Tss;  use Exp_Tss;
--with Exp_Unst; use Exp_Unst;
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
with Gnat.HTable; use Gnat.HTable;

with Reflex.Expanders.Dispatch; use Reflex.Expanders.Dispatch;

package body Reflex.Expanders.Itypes is
   
   Delayed_Itype_Decls : Elist_Id := No_Elist;

   ----------------------------------
   -- Check_No_Delayed_Itype_Decls --
   ----------------------------------

   procedure Check_No_Delayed_Itype_Decls
     (This : access Reflex_Expander_Record) is
   
      Elmt : Elmt_Id;
   begin
      if Delayed_Itype_Decls /= No_Elist then
         Elmt := First_Elmt (Delayed_Itype_Decls);
         while Present (Elmt) loop
            Error_Msg_N ("unsupported type reference", Node (Elmt));
            Next_Elmt (Elmt);
         end loop;
      end if;
   end Check_No_Delayed_Itype_Decls;

   ------------------------------
   -- Dump_Delayed_Itype_Decls --
   ------------------------------

   procedure Dump_Delayed_Itype_Decls
     (This : access Reflex_Expander_Record) is
      
      Elmt  : Elmt_Id;
      Itype : Entity_Id;
   begin
      if No (Delayed_Itype_Decls) then
         return;
      end if;

      Elmt := First_Elmt (Delayed_Itype_Decls);
      while Present (Elmt) loop
         Itype := Node (Elmt);

         --  Ensure that its parent type has been output before generating
         --  the declaration of the Itype.

         null; -- JMA  Dump_Type (Etype (Itype));

         --  Cannot invoke here Dump_Type since it would append again Itype
         --  to the list of pending record subtypes thus entering into a
         --  never-ending loop. Hence we invoke directly Cprint_Declare().

         --  Generate_Declare (Itype);

         Next_Elmt (Elmt);
      end loop;

      Delayed_Itype_Decls := No_Elist;
   end Dump_Delayed_Itype_Decls;

   ---------------------------------
   -- Register_Delayed_Itype_Decl --
   ---------------------------------

   procedure Register_Delayed_Itype_Decl
     (This : access Reflex_Expander_Record;
      E    : Entity_Id) is
   begin
      null; -- Append_New_Elmt (E, Delayed_Itype_Decls);
   end Register_Delayed_Itype_Decl;

end Reflex.Expanders.Itypes;

