------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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
-- Reflex is a fork from the GNAT compiler. GNAT was originally developed   --
-- by the GNAT team at  New York University. Extensive  contributions to    --
-- GNAT were provided by Ada Core Technologies Inc. Reflex is developed  by --
-- the Artics team at Grenoble.                                             --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
--with Checks;   use Checks;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Lib;      use Lib;
with Lib.Xref; use Lib.Xref;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch5;  use Sem_Ch5;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Uintp;    use Uintp;

package body Sem_Ch11 is

   -----------------------------------
   -- Analyze_Exception_Declaration --
   -----------------------------------

   procedure Analyze_Exception_Declaration (N : Node_Id) is
   begin
      null;
   end Analyze_Exception_Declaration;

   --------------------------------
   -- Analyze_Exception_Handlers --
   --------------------------------

   procedure Analyze_Exception_Handlers (L : List_Id) is
   begin
      null;
   end Analyze_Exception_Handlers;

   --------------------------------
   -- Analyze_Handled_Statements --
   --------------------------------

   procedure Analyze_Handled_Statements (N : Node_Id) is
   begin
      Analyze_Statements (Statements (N));

      if Present (At_End_Proc (N)) then
         Analyze (At_End_Proc (N));
      end if;
   end Analyze_Handled_Statements;

   -----------------------------
   -- Analyze_Raise_Statement --
   -----------------------------

   procedure Analyze_Raise_Statement (N : Node_Id) is
   begin
      null;
   end Analyze_Raise_Statement;

   -----------------------------
   -- Analyze_Raise_xxx_Error --
   -----------------------------

   --  Normally, the Etype is already set (when this node is used within
   --  an expression, since it is copied from the node which it rewrites).
   --  If this node is used in a statement context, then we set the type
   --  Standard_Void_Type. This is used both by Gigi and by the front end
   --  to distinguish the statement use and the subexpression use.

   --  The only other required processing is to take care of the Condition
   --  field if one is present.

   procedure Analyze_Raise_xxx_Error (N : Node_Id) is
   begin
      if No (Etype (N)) then
         Set_Etype (N, Standard_Void_Type);
      end if;

      if Present (Condition (N)) then
         Analyze_And_Resolve (Condition (N), Standard_Boolean);
      end if;

      --  Deal with static cases in obvious manner

      if Nkind (Condition (N)) = N_Identifier then
         if Entity (Condition (N)) = Standard_True then
            Set_Condition (N, Empty);

         elsif Entity (Condition (N)) = Standard_False then
            Rewrite (N, Make_Null_Statement (Sloc (N)));
         end if;
      end if;

   end Analyze_Raise_xxx_Error;

   -----------------------------
   -- Analyze_Subprogram_Info --
   -----------------------------

   procedure Analyze_Subprogram_Info (N : Node_Id) is
   begin
      Set_Etype (N, RTE (RE_Code_Loc));
   end Analyze_Subprogram_Info;

end Sem_Ch11;
