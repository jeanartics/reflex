------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 5                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2008, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Types; use Types;
with Namet; use Namet;

package Exp_Ch14 is

   procedure Initialize;

   function Make_Reactive_Graph (N : in Node_Id) return Rnode_Id;

   function Exist_Name_Entity_In_Scope
     (Name : Name_Id;
      Scop : Entity_Id) return Boolean;
   --  Return true if an entity having te name Name exit in scope S.

   function Exist_Name_Entity_In_Scope
     (S    : String;
      Scop : Entity_Id) return Boolean;
   --  Return true if an entity having te name S exit in current scope.

   function Enter_State_Name
     (Prefix : String;
      Suffix : String) return Name_Id;
   --  Create a new state name composed by concatening the
   --  Prefix State_Count Suffix. State_Count is a natural incremented
   --  at each call of this function. The resulting name is uniq in
   --  the current scope.

   function Make_New_Defining_Identifier
     (Prefix : String;
      Suffix : String;
      Scop   : Entity_Id;
      Sloc   : Source_Ptr) return Node_Id;


end Exp_Ch14;
