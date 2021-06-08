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

with Types; use Types;

-- Back_End_Scopes_Stack --

--  Stack associated with the generated code. Used to identify declarations
--  that requires the generation of extra scopes in order to generate C90
--  compliant code, since the front-end routine Insert_Actions may insert
--  temporaries in statement lists and C90 does not accept mixing
--  declarations and statements.

package Reflex.Expanders.Scopes_Stacks is

   Extra_Scopes_Allowed : Boolean := True;
   --  Enable/disable the ability to create extra scopes

   procedure Open_Scope;
   --  Make new scope stack entry in the top of the scopes stack and output
   --  character '{' if With_Block is True. The new scope is enabled to
   --  start processing declarations; it must be disabled by the caller
   --  invoking the routine Set_In_Statements when it starts generating
   --  code for the statements of this scope.

   procedure Open_Extra_Scope;
   --  Check if an extra scope is needed, and if true then output '{',
   --  push a new scope stack entry, and mark it as extra scope.

   procedure Close_Scope;
   --  Remove from the top of the stack all the entries of inner extra
   --  scopes (if any) and the first non-extra scope. Output '}' for
   --  each closed scope that was opened with With_Block set to True.

   procedure Close_Scope (Scop_Id : Nat);
   --  Remove from the top of the stack all the entries of inner extra
   --  scopes (if any) until the scope Scop_Id is removed from the stack.
   --  Output '}' for each closed scope that was opened with With_Blocks
   --  set to True.

   function Current_Scope_Id return Nat;
   --  Return the id of the current scope

   function In_Declarations return Boolean;
   --  Return True if we are processing the declarations of the scope in
   --  the top of the stack.

   procedure Set_In_Statements;
   --  Remember in the top of the stack entry that we are processing its
   --  declarations.

end Reflex.Expanders.Scopes_Stacks;
