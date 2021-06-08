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
with Namet; use Namet;

with Artics.Dynamic_Tables;
with Artics.Buffers;        use Artics.Buffers;
with Reflex.Infos;          use Reflex.Infos;

package Reflex.Predicates is

   function Expression_Width (Node : Node_Id) return Natural;
   function Expression_Height (Node : Node_Id) return Natural;
   function Expression_Height_Worker (Node : Node_Id) return Natural;

--     procedure If_Statement_Predicates (Node : Node_Id);
--     function Check_Assign_Etype (Stmts : List_Id) return Boolean;

   function Has_Only_Simple_Statement (N : Node_Id) return Boolean;


   function Is_Simple_Statements_List (Stmts : List_Id) return Boolean;

   function Is_Simple_Statement (Stmt : Node_Id) return Boolean;

   function Is_Null_Statements_List (Stmts : List_Id) return Boolean;

   function Must_Statements_List_Break (Stmts : List_Id) return Boolean;

   function Must_Break_If (N : Node_Id) return Boolean;

   function Directly_Designate_Aggregate (N : Node_Id) return Node_Id;

   function Designate_Aggregate (N : Node_Id) return Node_Id;
   --  Return the aggregate or extansion aggregate node if N designate an
   --  an aggregate, so it is N_Aggegate or N_Extension_Aggregate or an
   --  N_Identifier or an N_Expanded_Name or an N_Qualified_Expression which
   --  designate an aggregate

   function Expression_Has_Side_Effect (N : Node_Id) return Boolean;
   --  Return true if the condition has expresion wich need to be executed
   --  before evaluting the whole expression. This is the case for function
   --  calls in the expression, the call is executed first and the call in
   --  expression is replaced by its result.

   procedure Insert_Statement_Actions
     (Assoc_Node  : Node_Id;
      Ins_Actions : List_Id);

end Reflex.Predicates;
