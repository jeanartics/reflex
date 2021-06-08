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

separate (Artics.Graphs)
package body Visitor is

   ---------------
   -- Set_Graph --
   ---------------

   procedure Set_Graph
     (V : in out Graph_Visitor_Record;
      G : in Graph) is
   begin
      V.Current_Graph := G;
   end Set_Graph;

   ---------------
   -- Get_Graph --
   ---------------

   function Get_Graph (V : in Graph_Visitor_Record) return Graph is
   begin
      return V.Current_Graph;
   end Get_Graph;

   ----------------
   -- Set_Vertex --
   ----------------

   procedure Set_Vertex
     (V  : in out Graph_Visitor_Record;
      Vx : in Vertex) is
   begin
      V.Current_Vertex := Vx;
   end Set_Vertex;

   ----------------
   -- Get_Vertex --
   ----------------

   function Get_Vertex (V : in Graph_Visitor_Record) return Vertex is
   begin
      return V.Current_Vertex;
   end Get_Vertex;

   -------------
   -- Set_Arc --
   -------------

   procedure Set_Arc
     (V : in out Graph_Visitor_Record;
      A : in Arc) is
   begin
      V.Current_Arc := A;
   end Set_Arc;

   -------------
   -- Get_Arc --
   -------------

   function Get_Arc (V : in Graph_Visitor_Record) return Arc is
   begin
      return V.Current_Arc;
   end Get_Arc;

   ----------------------
   -- Visit_Initialize --
   ----------------------

   procedure Visit_Initialize
     (V : in out Graph_Visitor_Record;
      G : in Graph) is
   begin
      Set_Graph (V, G);
   end Visit_Initialize;

   --------------------
   -- Visit_Finalize --
   --------------------

   procedure Visit_Finalize (V : in out Graph_Visitor_Record) is
   begin
      null;
   end Visit_Finalize;

   -----------------
   -- Visit_Graph --
   -----------------

   procedure Visit_Graph
     (V : in out Graph_Visitor_Record;
      S : in Visit_Step) is
   begin
      null;
   end Visit_Graph;

   ------------------
   -- Visit_Vertex --
   ------------------

   procedure Visit_Vertex
     (V : in out Graph_Visitor_Record;
      S : in Visit_Step) is
   begin
      null;
   end Visit_Vertex;

   ---------------
   -- Visit_Arc --
   ---------------

   procedure Visit_Arc
     (V : in out Graph_Visitor_Record;
      S : in Visit_Step) is
   begin
      null;
   end Visit_Arc;

 end Visitor;
