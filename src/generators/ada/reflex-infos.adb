------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
--                                                                          --
-- Reflex is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public Licensea as pu  by the Free Soft- --
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

with System;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Gnat.Case_Util; use Gnat.Case_Util;

with Atree; use Atree;
with Types; use Types;
with Sinfo; use Sinfo;
with Nlists; use Nlists;
with Nmake; use Nmake;
with Tbuild; use Tbuild;

with Reflex.Formats; use Reflex.Formats;
with Einfo; use Einfo;
with Ada.Text_IO; use Ada.Text_IO;
with Artics.Generic_Lists;

package body Reflex.Infos is
   
   function To_Address is new Ada.Unchecked_Conversion 
     (Reflex_Infos_Ptr, System.Address);
   
   function To_Reflex_Infos is new Ada.Unchecked_Conversion 
     (System.Address, Reflex_Infos_Ptr);
   pragma No_Strict_Aliasing (Reflex_Infos_Ptr);
   
   -----------------------------
   -- Initialize_Reflex_Infos --
   -----------------------------
   
   procedure Initialize_Reflex_Infos is
   begin
      null;
   end Initialize_Reflex_Infos;
   
   ----------------------
   -- New_Reflex_Infos --
   ----------------------
   
   function New_Reflex_Infos return Reflex_Infos_Ptr is
   begin
      return new Reflex_Infos_Record'(No_Reflex_Infos_Record);
   end New_Reflex_Infos;
   
   ----------------------
   -- New_Reflex_Infos --
   ----------------------
   
   function New_Reflex_Infos (Node : Node_Id) return Reflex_Infos_Ptr is
      
      This : Reflex_Infos_Ptr := New_Reflex_Infos;
   begin
      Set_Rx_Infos (Node, To_Address (This));
      
      return This;
   end New_Reflex_Infos;
   
   -----------------------
   -- Free_Reflex_Infos --
   -----------------------
   
   procedure Free_Reflex_Infos (This : in out Reflex_Infos_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation 
        (Reflex_Infos_Record, Reflex_Infos_Ptr);
   begin
      if This /= null then
         Free (This);
      end if;
   end Free_Reflex_Infos;
   
   -----------------------
   -- Free_Reflex_Infos --
   -----------------------
   
   procedure Free_Reflex_Infos (Node : Node_Id) is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      Free_Reflex_Infos (This);
      Set_Rx_Infos (Node, System.Null_Address);
   end Free_Reflex_Infos;
   
   ----------------------
   -- Get_Reflex_Infos --
   ----------------------
   
   function Get_Reflex_Infos
     (Node : Node_Or_Entity_Id) return access Reflex_Infos_Record is
      
      This : Reflex_Infos_Ptr := To_Reflex_Infos (Get_Rx_Infos (Node));
   begin
      if This = null then
         This := New_Reflex_Infos (Node);
      end if;
      
      return This;
   end Get_Reflex_Infos;
   
   ----------------------
   -- Set_Reflex_Infos --
   ----------------------
   
   procedure Set_Reflex_Infos
     (Node  : Node_Or_Entity_Id;
      Infos : access Reflex_Infos_Record) is
   begin
      Set_Rx_Infos (Node, To_Address (Reflex_Infos_Ptr (Infos)));
   end Set_Reflex_Infos;
   
   -----------------
   -- Is_Expanded --
   -----------------
   
   function Is_Expanded (Node : Node_Id) return Boolean is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      return This.Expanded;
   end Is_Expanded;
   
   ------------------
   -- Set_Expanded --
   ------------------
   
   procedure Set_Expanded
     (Node : Node_Id;
      V    : Boolean) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      This.Expanded := V;
   end Set_Expanded;
   
   ------------------
   -- Is_Generated --
   ------------------
   
   function Is_Generated (Node : Node_Id) return Boolean is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      return This.Generated;
   end Is_Generated;
   
   -------------------
   -- Set_Generated --
   -------------------
   
   procedure Set_Generated
     (Node : Node_Id;
      V    : Boolean) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      This.Generated := V;
   end Set_Generated;
   
   ----------------
   -- Is_Covered --
   ----------------
   
   function Is_Covered (Node : Node_Id) return Boolean is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      return This.Covered;
   end Is_Covered;
   
   -----------------
   -- Set_Covered --
   -----------------
   
   procedure Set_Covered
     (Node : Node_Id;
      V    : Boolean) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      This.Covered := V;
   end Set_Covered;
   
   --------------------------
   -- Is_Expansion_Pending --
   --------------------------
   
   function Is_Expansion_Pending (Node : Node_Id) return Boolean is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      return This.Expansion_Pending;
   end Is_Expansion_Pending;
   
   ---------------------------
   -- Set_Expansion_Pending --
   ---------------------------
   
   procedure Set_Expansion_Pending
     (Node : Node_Id;
      V    : Boolean) is 
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      This.Expansion_Pending := V;
   end Set_Expansion_Pending;
   
   ---------------------------
   -- Is_Generation_Pending --
   ---------------------------
   
   function Is_Generation_Pending (Node : Node_Id) return Boolean is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      return This.Generation_Pending;
   end Is_Generation_Pending;
   
   ----------------------------
   -- Set_Generation_Pending --
   ----------------------------
   
   procedure Set_Generation_Pending
     (Node : Node_Id;
      V    : Boolean) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      This.Generation_Pending := V;
   end Set_Generation_Pending;
   
   -------------------------
   -- Is_Coverage_Pending --
   -------------------------
   
   function Is_Coverage_Pending (Node : Node_Id) return Boolean is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      return This.Coverage_Pending;
   end Is_Coverage_Pending;
   
   --------------------------
   -- Set_Coverage_Pending --
   --------------------------
   
   procedure Set_Coverage_Pending
     (Node : Node_Id;
      V    : Boolean) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      This.Coverage_Pending := V;
   end Set_Coverage_Pending;

   ---------------------
   -- Is_Homonym_Done --
   ---------------------
   
   function Is_Homonym_Done (E : Entity_Id) return Boolean is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Homonym_Done;
   end Is_Homonym_Done;
   
   ----------------------
   -- Set_Homonym_Done --
   ----------------------
   
   procedure Set_Homonym_Done
     (E : Entity_Id;
      V : Boolean) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Homonym_Done := V;
   end Set_Homonym_Done;
   
   ---------------------
   -- Is_Homonym_Done --
   ---------------------
   
   function Get_Ada_Real_Literal (E : Entity_Id) return String is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      if This.Ada_Real_Literal = null then
	 return "?";
      end if;
      return This.Ada_Real_Literal.all;
   end Get_Ada_Real_Literal;
   
   ----------------------
   -- Set_Ada_Real_Literal --
   ----------------------
   
   procedure Set_Ada_Real_Literal
     (E : Entity_Id;
      V : String) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Ada_Real_Literal := new String'(V);
   end Set_Ada_Real_Literal;
   
   --------------------------------------
   -- Get_Enumeration_Literal_Constant --
   --------------------------------------
   
   function Get_Enumeration_Literal_Constant (E : Entity_Id) return Entity_Id 
   is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Enum_Literal_Constant;
   end Get_Enumeration_Literal_Constant;
   
   --------------------------------------
   -- Set_Enumeration_Literal_Constant --
   --------------------------------------
   
   procedure Set_Enumeration_Literal_Constant
     (E        : Entity_Id;
      List_Cst : Entity_Id) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Enum_Literal_Constant := List_Cst;
   end Set_Enumeration_Literal_Constant;
   
   ---------------
   -- Is_Anonym --
   ---------------
   
   function Is_Anonym (E : Entity_Id) return Boolean is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Anonym;
   end Is_Anonym;
   
   -------------------
   -- Set_Is_Anonym --
   -------------------
   
   procedure Set_Is_Anonym
     (E : Entity_Id;
      V : Boolean) is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Anonym := V;
   end Set_Is_Anonym;
   
   ----------------
   -- Get_Vertex --
   ----------------
   
   function Get_Vertex (N : Node_Id) return Vertex_Id is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (N);
   begin
      return This.Vertex;
   end Get_Vertex;
   
   ----------------
   -- Set_Vertex --
   ----------------
   
   procedure Set_Vertex 
     (N : Node_Id;
      V : Vertex_Id) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (N);
   begin
      This.Vertex := V;
   end Set_Vertex;
   
   ---------------------
   -- Get_Entity_Refs --
   ---------------------
   
   function Get_Entity_Refs (E : Node_Id) return Reflex.Nodes_Lists.List is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Entity_Refs;
   end Get_Entity_Refs;
   
   -----------------------------
   -- Remove_Entity_Reference --
   -----------------------------
   
   procedure Remove_Entity_Reference 
     (E : Node_Id;
      N : Node_Id) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
      Cur  : Reflex.Nodes_Lists.Cursor;
   begin
      Cur := This.Entity_Refs.First;
      while Reflex.Nodes_Lists.Has_Element  (Cur) loop
	 if Reflex.Nodes_Lists.Element (Cur) = N then
	    This.Entity_Refs.Delete (Cur);
	    return;
	 end if;
	 Reflex.Nodes_Lists.Next (Cur);
      end loop;
   end Remove_Entity_Reference;
   
   --------------------------
   -- Set_Entity_Reference --
   --------------------------
   
   procedure Set_Entity_Reference 
     (N : Node_Id;
      E : Node_Id) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
      Old  : Node_Id;
   begin
      Old := Entity (N);
      Remove_Entity_Reference (Old, N);
      
      if not This.Entity_Refs.Contains (N) then
	 This.Entity_Refs.Append (N);
      end if;
   end Set_Entity_Reference;
   
end Reflex.Infos;
