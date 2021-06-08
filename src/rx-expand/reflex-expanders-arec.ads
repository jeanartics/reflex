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

--  Routines which facilitate handling the activation record of unnested
--  subprograms.
!
package Reflex.Expanders.AREC is
   
   function ARECnU (Subp_Id : Entity_Id) return Node_Id;
   --  Return the uplink component of the given subprogram

   function ARECnF (Subp_Id : Entity_Id) return Node_Id;
   --  Return the extra formal that contains the pointer to the activation
   --  record for uplevel references of the given subprogram.

   function AREC_Entity (N : Node_Id) return Entity_Id;
   --  Given an N_Identifier node N which references a field of an
   --  activation record, return the entity of the corresponding formal.

   function AREC_Subprogram (Formal : Entity_Id) return Entity_Id;
   --  Return the subprogram that has a field in its activation record to
   --  pass Formal to its nested subprograms.

   function Get_AREC_Field (N : Node_Id) return Node_Id;
   --  Given the AREC reference N return the AREC field

   function Is_AREC_Reference (N : Node_Id) return Boolean;
   --  Return True if N is a reference to an AREC field

   procedure Write_Up_Level_Formal_Reference
     (Subp   : Entity_Id;
      Formal : Entity_Id);
   --  Write code that climbs through the activation record of the enclosing
   --  subprograms and references the pointer to the fat pointer Formal
   --  parameter of Subp.
   
end Reflex.Expanders.AREC;
