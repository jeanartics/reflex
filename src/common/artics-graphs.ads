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

with Artics.Types; use Artics.Types;

with Artics.Containers.Lists;

with Artics.Properties_Lists; use Artics.Properties_Lists;
with Artics.Buffers; use Artics.Buffers;

generic
   type Graph_Item is private;
   No_Graph_Item : Graph_Item;

   type Vertex_Item is private;
   No_Vertex_Item : Vertex_Item;

   type Arc_Item is private;
   No_Arc_Item : Arc_Item;

package Artics.Graphs is

   pragma Elaborate_Body;

   type Graph_Node_Record is tagged private;
   type Gnode is access all Graph_Node_Record;

   -----------
   -- GRAPH --
   -----------

   Graph_Exception : exception;
   -- Raise when accessing to not existing element (Graph, Vertices, Arcs).

   type Graph_Record is new Graph_Node_Record with private;
   type Graph is access all Graph_Record;
   Null_Graph : constant Graph := null;
   -- Represents a graph.


   ------------
   -- VERTEX --
   ------------

   type Vertex_Kind is
     (Normal_Vertex,
      Divergence_Vertex);

   type Vertex_Record is new Graph_Node_Record with private;
   type Vertex is access all Vertex_Record;
   Null_Vertex : constant Vertex := null;
   -- Represents a vertex. A vertex belongs to a Graph.

   package Vertex_Lists is new Containers.Lists (Vertex, Null_Vertex);
   use Vertex_Lists;
   type Vertex_Id is new Vertex_Lists.Item_Id;
   No_Vertex_Id : constant Vertex_Id := Vertex_Id (Vertex_Lists.No_Item_Id);
   -- list of vertices

   ---------
   -- ARC --
   ---------

   type Arc_Record is new Graph_Node_Record with private;
   type Arc is access all Arc_Record;
   Null_Arc : constant Arc := null;
   -- Represents an Arc. Its is a link between two vertex.

   package Arc_Lists is new Containers.Lists (Arc, Null_Arc);
   use Arc_Lists;
   type Arc_Id is new Arc_Lists.Item_Id;
   No_Arc_Id : constant Arc_Id := Arc_Id (Arc_Lists.No_Item_Id);
   -- list of arcs

   package Sibling_Lists is new Containers.Lists
     (Vertex_Lists.List_Id, Vertex_Lists.No_List);
   use Sibling_Lists;

   ------------
   -- BRANCH --
   ------------

   type Branch_Elmt_Record is tagged private;
   type Branch_Elmt is access all Branch_Elmt_Record'Class;
   No_Branch_Elmt : constant Branch_Elmt := null;

   package Branch_Elmt_Lists is new Containers.Lists
     (Branch_Elmt, No_Branch_Elmt);
   use Branch_Elmt_Lists;
   -- list of elmts contained by the branch

   MAX_BRANCHES : constant Integer := 25;
   type Branch_Array is array (1..MAX_BRANCHES) of Branch_Elmt_Lists.List_Id;

   type Branches_Record is
      record
         Arr      : Branch_Array := (others => Branch_Elmt_Lists.No_List);
         Free_Idx : Integer := Branch_Array'First;
      end record;
   -- structures used for reducing graphs (graphs.algotithms)

   function Get_Properties (Gn : in Gnode) return Properties_List;
   -- return the complete attribute list.

   procedure Set_Property
     (Gn   : in Gnode;
      Name : in Name_Id;
      Data : in Property_Data);
   -- Change un attribut du noeud.

   procedure Set_Property
     (Gn   : in Gnode;
      Name : in String;
      Data : in Property_Data);
   -- Change un attribut du noeud.

   function Get_Property
     (Gn   : in Gnode;
      Name : in Name_Id) return Property_Data;
   -- Renvoie un attribut du noeud.

   function Get_Property
     (Gn   : in Gnode;
      Name : in String) return Property_Data;
   -- Renvoie un attribut du noeud.


   -- Debug procedures --
   ----------------------

   procedure Dump_Graph (G : in Graph);

   procedure Dump_Vertex
     (V        : in Vertex;
      Arc_Dump : in Boolean := False);

   procedure Dump_Arc (A : in Arc);

   function Get_Gnode_Name (G : in Gnode) return String;
   -- Returns the name of G

   procedure Set_Gnode_Name
     (G : in Gnode;
      N : in String);
   -- Sets the name of G

   ----------------------
   -- Graph operations --
   ----------------------

   function New_Graph (I : in Graph_Item) return Graph;
   -- Create a new graph.

   function New_Graph
     (Name : in String;
      I    : in Graph_Item) return Graph;
   -- Create a new named graph

   function Get_Graph_Name (G : in Graph) return String;
   -- Returns the name of G

   procedure Set_Graph_Name
     (G : in Graph;
      N : in String);
   -- Sets the name of G

   procedure Set_Item
     (G : in Graph;
      I : in Graph_Item);
   --  Set the item of the given arc.

   function Get_Item (G : in Graph) return Graph_Item;
   --  Return the item associated with the arc.


   procedure Reinitialize_Graph
     (G : in out Graph );
   -- Unmark all graph vertices

   procedure Destroy_Graph (G : in out Graph);
   -- Destroy all the vertices in the graph, and by implication, all the arcs
   -- in the graph. The semantics of destroy are such that any aliased vertices
   -- and arcs are not eliminated from the graph, because to do so would
   -- introduce dangling references.

   procedure Add_Vertex
     (G : in Graph;
      V : in Vertex);
   -- Create a new vertex and add it to the graph, setting the second argument
   -- of this function as an alias to this new vertex.

   procedure Add_Arc
     (G : in Graph;
      A : in Arc);
   -- Create a new vertex and add it to the graph, setting the second argument
   -- of this function as an alias to this new vertex.

   procedure Remove_Vertex
     (G : in Graph;
      V : in Vertex);
   -- Destroy the given vertex and any associated arcs. If the vertex has no
   -- other aliases, eliminate it from the graph.

   procedure Remove_Arc
     (G : in Graph;
      A : in Arc);
   -- Destroy the given arc. If the arc has no other aliases, eliminate it from
   -- the graph.

   function Number_Of_Vertices (G : in Graph) return Natural;
   -- Return the number of vertices in the graph.

   function Number_Of_Arcs (G : in Graph) return Natural;
   -- Return the number of arcs in the graph.

   function Is_Empty (G : in Graph) return Boolean;
   -- Return True if and only if the graph does not contain any vertices or
   -- arcs.

   function Get_Vertex_List (G : in Graph) return Vertex_Lists.List_Id;
   -- returns the list of vertices of the graph

   function Is_Root_Vertex (G : in Graph; V : in Vertex) return Boolean;
   -- Return True if and only if the graph does not contain any vertices or
   -- arcs.

   function Get_Root_Vertex (G : in Graph) return Vertex;
   -- Root is now the root vertex so the first vertex in vertex list, and
   -- iterators on list start from this node.

   procedure Set_Root_Vertex
     (G    : in Graph;
      Root : in Vertex);
   -- Root is now the root vertex so the first vertex in vertex list, and
   -- iterators on list start from this node.


   function First_Vertex (G : in Graph) return Vertex_Id;
   pragma Inline (First_Vertex);
   -- Return a reference on the root vertex of the graph.

   function Last_Vertex (G : in Graph) return Vertex_Id;
   pragma Inline (Last_Vertex);
   -- Return a reference on the last vertex of the graph.

   procedure Next_Vertex (Vid : in out Vertex_Id);
   pragma Inline (Next_Vertex);
   -- Return a reference on the vertex following the vertex refrenced by Vid.

   procedure Previous_Vertex (Vid : in out Vertex_Id);
   pragma Inline (Previous_Vertex);
   -- Return a reference on the vertex preceding the vertex refrenced by Vid.

   function Get_Vertex (Vid : in Vertex_Id) return Vertex;
   pragma Inline (Get_Vertex);
   -- Return The value of the vertex referenced by Vid.

   -------------
   -- Vetices --
   -------------

   function Is_Member
     (G : in Graph;
      V : in Vertex) return Boolean;
   -- Return True if and only if the given vertex is not null and denotes a
   -- vertex in the graph.

   function Is_Member
     (G : in Graph;
      V : in Vertex_Item) return Boolean;
   -- Return True if and only if the given vertex is not null and denotes a
   -- vertex in the graph.

   function Get_Member_Vertex
     (G : in Graph;
      V : in Vertex_Item ) return Vertex;
   -- return the vertex having the Item = V


   ----------
   -- Arcs --
   ----------

   function Is_Member
     (G : in Graph;
      A : in Arc) return Boolean;

   function Is_Member
     (G    : in Graph;
      Src  : in Vertex;
      Dest : in Vertex) return Boolean;

   function Is_Member
     (G    : in Graph;
      Src  : in Vertex;
      Dest : in Vertex;
      A    : in Arc_Item) return Boolean;
   -- Return True if and only if the given arc is not null and denotes an arc
   -- in the graph.


   -----------------------
   -- Vertex operations --
   -----------------------

   function New_Vertex (I : in Vertex_Item) return Vertex;
   -- Create a vertex, with user data set to I.

   procedure Destroy_Vertex (V : in out Vertex);
   --  If the vertex is not null, remove this alias.

   function Get_Vertex_Name (V : in Vertex) return String;
   -- Returns the name of G

   procedure Set_Vertex_Name
     (V : in Vertex;
      N : in String);
   -- Sets the name of G


   function Get_Vertex_Nb (V : in Vertex) return Natural;

   procedure Set_Vertex_Nb
     (V : in Vertex;
      N : in Natural);

   procedure Set_Item
     (V : in Vertex;
      I : in Vertex_Item);
   --  Set the item of the given vertex.

   function Get_Kind (V : in Vertex) return Vertex_Kind;

   procedure Set_Kind
     (V    : in out Vertex;
      Kind : in Vertex_Kind);

   function Get_Item (V : in Vertex) return Vertex_Item;
   --  Return the item associated with the vertex.

   function Enclosing_Graph (V : in Vertex) return Graph;
   --  Return the graph enclosing the vertex.

   procedure Set_Enclosing_Graph
     (V : in Vertex;
      G : in Graph);

   function Get_Source_List_Of_Vertex
     (V : in Vertex) return Vertex_Lists.List_Id;

   function Get_Target_List_Of_Vertex
     (V : in Vertex) return Vertex_Lists.List_Id;

   function Get_Incoming_Arcs (V : in Vertex) return Arc_Lists.List_Id;
   -- return the list of incoming arcs for V

   function Get_Outgoing_Arcs (V : in Vertex) return Arc_Lists.List_Id;
   -- return the list of outgoing arcs for V


   function Get_First_Outgoing_Arc (V : in Vertex) return Arc;

   function First_In_Arc (V : in Vertex) return Arc;
   pragma Inline (First_In_Arc);
   -- Return a reference on the first incomming Arc.

   function Last_In_Arc (V : in vertex) return Arc;
   pragma Inline (Last_In_Arc);
   -- Return a reference on the last incomming Arc.

   function First_Out_Arc (V : in Vertex) return Arc;
   pragma Inline (First_Out_Arc);
   -- Return a reference on the first outgoing Arc.

   function Last_Out_Arc (V : in vertex) return Arc;
   pragma Inline (Last_Out_Arc);
   -- Return a reference on the first outgoing Arc.


   function First_In_Arc (V : in Vertex) return Arc_Id;
   pragma Inline (First_In_Arc);
   -- Return a reference on the first incomming Arc.

   function Last_In_Arc (V : in vertex) return Arc_Id;
   pragma Inline (Last_In_Arc);
   -- Return a reference on the last incomming Arc.

   function First_Out_Arc (V : in Vertex) return Arc_Id;
   pragma Inline (First_Out_Arc);
   -- Return a reference on the first outgoing Arc.

   function Last_Out_Arc (V : in vertex) return Arc_Id;
   pragma Inline (Last_Out_Arc);
   -- Return a reference on the first outgoing Arc.

   procedure Next_Arc (Aid : in out Arc_Id);
   pragma Inline (Next_Arc);
   -- Return the arc following the arc refrenced by Aid.

   procedure Previous_Arc (Aid : in out Arc_Id);
   pragma Inline (Previous_Arc);
   -- Return the arc preceding the arc refrenced by Aid.

   function Get_Arc (Aid : in Arc_Id) return Arc;
   pragma Inline (Get_Arc);
   -- return the arc refrenced by Aid.

   function Get_Distance_To_Deep (V : in Vertex) return Natural;
   -- Return the number of vertex to go throught to get the deepest vertex.

   procedure Set_Distance_To_Deep
     (V : in Vertex;
      D : in Natural);
   -- Set the distance to the most deeper vertex going from this vertex.

   function Get_Distance_To_Root (V : in Vertex) return Natural;
   -- Return the number of vertex to go throught to get the deepest vertex.

   procedure Set_Distance_To_Root
     (V : in Vertex;
      D : in Natural);
   -- Set the distance to the most deeper vertex going from this vertex.

   function Get_Tree_Cover (V : in Vertex) return Natural;
   -- Return the number of vertex to go throught to get the deepest vertex.

   procedure Set_Tree_Cover
     (V : in Vertex;
      C : in Natural);
   -- Set the distance to the most deeper vertex going from this vertex.


   procedure Set_Branches_Record
     (V  : in out Vertex;
      Br : in Branches_Record );
   -- set the branches record for V

   function Get_Branches_Record
     (V  : in Vertex) return Branches_Record;
   -- return the branches record associated to V

   function Has_Branches
     (V : in Vertex) return Boolean;
   -- return TRUE if there are associated branches to V

   function Branches_Number
     (V : in Vertex) return Integer;
   -- return the number of branches associated to vertex

   procedure Add_Branch_To_Branches_Record
     (V  : in out Vertex;
      BR : in Branch_Elmt_Lists.List_Id);


   --------------------
   -- Arc operations --
   --------------------

   function New_Arc
     (From : in Vertex;
      To   : in Vertex;
      I    : in Arc_Item) return Arc;
   -- Create an arc between From and To. The value associated is I.

   procedure Destroy_Arc (A : in out Arc);
   --  Destroy the arc.

   procedure Make_Arcs_From_To_Vertex_List
     (G    : in Graph;
      From : in Vertex;
      To_L : in Vertex_Lists.List_Id);

   procedure Make_Arcs_To_From_Vertex_List
     (G      : in Graph;
      To     : in Vertex;
      From_L : in Vertex_Lists.List_Id);

   function Get_Arc_Nb (A : in Arc) return Natural;

   procedure Set_Arc_Nb
     (A : in Arc;
      N : in Natural);

   function Get_Arc_Name (A : in Arc) return String;
   -- Returns the name of G

   procedure Set_Arc_Name
     (A : in Arc;
      N : in String);
   -- Sets the name of G


   procedure Set_Item
     (A : in Arc;
      I : in Arc_Item);
   --  Set the item of the given arc.

   function Get_Item (A : in Arc) return Arc_Item;
   --  Return the item associated with the arc.

   function Get_Source_Vertex (A : in Arc) return Vertex;
   -- Returns the source vertex of the arc A.

   procedure Set_Source_Vertex
     (A : in Arc;
      V : in Vertex);
   -- Sets the source vertex of the arc A.

   function Get_Target_Vertex (A : in Arc) return Vertex;
   -- Returns the target vertex of the arc A.

   procedure Set_Target_Vertex
     (A : in Arc;
      V : in Vertex);
   -- Sets the target vertex of the arc A.

   procedure Redirect_Source_Vertex
     (A : in Arc;
      V : in Vertex);

   procedure Redirect_Target_Vertex
     (A : in Arc;
      V : in Vertex);

   function Enclosing_Graph (A : in Arc) return Graph;
   --  Return the graph enclosing the arc.

   procedure Set_Enclosing_Graph
     (A : in Arc;
      G : in Graph);
   --  Set the graph enclosing the arc.

   function Vertex_Degre (V : in Vertex) return Natural;
   -- Return the degree of the vertex. The degree of a vertex is the sum of
   -- the Internal_Dgree and External_Degree.

   function Vertex_Internal_Degre (V : in Vertex) return Natural;
   -- Return the internal degree of the vertex. The internal degree of a vertex
   -- is the number of incoming arcs.

   function Vertex_External_Degre (V : in Vertex) return Natural;
   -- Return the external degree of the vertex. The external degree of a vertex
   -- is the number of outgoing arcs.

   procedure Mark
     (V : in Vertex;
      B : in Boolean);

   function Marked (V : in Vertex) return Boolean;

   procedure Unmark_All (G : in Graph);

   function Target_Distance_To_Deep (A : in Arc) return Natural;
   -- Return the distance to Deep of target vertex of this arc.

   function Source_Distance_To_Deep (A : in Arc) return Natural;
   -- Return the distance to Deep of source vertex of this arc.

   function Target_Distance_To_Root (A : in Arc) return Natural;
   -- Return the distance to Root of target vertex of this arc.

   function Source_Distance_To_Root (A : in Arc) return Natural;
   -- Return the distance to Root of source vertex of this arc.

   function Target_Tree_Cover (A : in Arc) return Natural;
   -- Return the Cover number of target vertex of this arc.

   function Source_Tree_Cover (A : in Arc) return Natural;
   -- Return the Cover number of source vertex of this arc.


   -------------
   -- Visitor --
   -------------

   package Visitor is

      type Graph_Visitor_Record is tagged private;
      type Graph_Visitor is access all Graph_Visitor_Record;

      type Visit_Step is (Before, After);

      procedure Set_Graph
        (V : in out Graph_Visitor_Record;
         G : in Graph);
      -- Set the current Graph to visit.

      function Get_Graph (V : in Graph_Visitor_Record) return Graph;
      -- Returns the current visited Graph.

      procedure Set_Vertex
        (V  : in out Graph_Visitor_Record;
         Vx : in Vertex);
      -- Set the current Vertex to visit.

      function Get_Vertex (V : in Graph_Visitor_Record) return Vertex;
      -- Returns the current visited Vertex.

      procedure Set_Arc
        (V : in out Graph_Visitor_Record;
         A : in Arc);
      -- Set the current Vertex to visit.

      function Get_Arc (V : in Graph_Visitor_Record) return Arc;
      -- Returns the current visited Arc.

      procedure Visit_Initialize
        (V : in out Graph_Visitor_Record;
         G : in Graph);
      -- Initialize a visit for the graph G.

      procedure Visit_Finalize (V : in out Graph_Visitor_Record);
      -- Finalize the graph visit.

      procedure Visit_Graph
        (V : in out Graph_Visitor_Record;
         S : in Visit_Step);
      -- Performs a deep first walk throught the graph G.

      procedure Visit_Vertex
        (V : in out Graph_Visitor_Record;
         S : in Visit_Step);
      -- Visit a vertex.

      procedure Visit_Arc
        (V : in out Graph_Visitor_Record;
         S : in Visit_Step);
      -- Visit an Arc. This procedure is called before any call to the two
      -- followings, no matter the arc is in or outgoing.

   private
      type Graph_Visitor_Record is tagged record
         Current_Graph  : Graph;
         Current_Vertex : Vertex;
         Current_Arc    : Arc;
      end record;

   end Visitor;
   use Visitor;



   -------------
   -- Walkers --
   -------------

   package Walkers is

      -- This package provides somme graph walkers. The main walk are the deep
      -- first walk, which recursly visit the graph going to the deepest nodes.

      procedure Do_Visit
        (V : in out Graph_Visitor_Record'Class;
         G : in Graph);
      -- Entry point of walkers.

      procedure Deep_First_Walk
        (Visit : in out Graph_Visitor_Record'Class;
         G     : in Graph);
      -- Walk throught the graf using a deep first walk. During the walk, the
      -- arcs are traverse in the order of the list, so in the order where they
      -- have been attached to the list, so in the creation order, unless the
      -- order of arcs in list have been re arrange by an order procedure in
      -- package algorithm.

      procedure Sibling_First_Walk
        (Visit : in out Graph_Visitor_Record'Class;
         G     : in Graph);
      -- Walk throught the graf in the tree ordering order. Previous to the
      -- walk, the tree covering must have been comptuted by the procedure
      -- Tree_Covering in package Algorithms.

      function Build_Sibling_List (G : in Graph) return Sibling_Lists.List_Id;

   end Walkers;
   use Walkers;


   ----------------
   -- Algorithms --
   ----------------

   package Algorithms is

      -- This package provides somme general algorithms needed by Glips to
      -- build and generate reactive code. The algorithms are the followings :

      -- Distance_To_Root computes for all vertex, the distance to the root.

      -- Distance_To_Deep computes for each arcs the maximun distance to access
      -- the most deep nodes, using the longest way.

      -- Deep_Most_Longest_Way_Order, orders for arcs of each vertex to obtain
      -- a list where arcs are ordered in decreasing way to access the most
      -- deep nodes.

      -- Tree_Covering returns a covering miminmun tree of the graph.

      procedure Distance_To_Root (G : in Graph);
      -- computes for all vertex, the distance from the vertex to the root
      -- vertex.

      procedure Distance_To_Deep (G : in Graph);
      -- computes for each arcs the maximun distance to access the most deep
      -- nodes, using the longest way.

--      function Tree_Covering (G : in Graph) return Vertex_List_Id;
      -- Returns the corresponding Tree for a graph.

      function Max_Depth_Search (G : in Graph ) return Vertex_Lists.List_Id;
      -- Returns an ordered list of vertices of G representing the longest path
      -- starting from root vertex of graph (first vertex of vertex_lists of
      -- graph)

      procedure Distance_To_Deep_Order (G : in Graph);

      procedure Distance_To_Root_Order (G : in Graph);

      procedure Cover_Order (G : in Graph);

      function Is_Backward_Arc (A : in Arc) return Boolean;
      function Is_Forward_Arc (A : in Arc) return Boolean;
      function Is_Cover_Arc (A : in Arc) return Boolean;
      function Is_Crossed_Arc (A : in Arc) return Boolean;


      -- BRANCHES --

      function New_Branch_Element
        (V : in Vertex := Null_Vertex;
         A : in Arc    := Null_Arc)
        return Branch_Elmt;
      -- create a new branch element

      procedure Set_Branch_Elmt_Vertex
        (Elmt : in out Branch_Elmt;
         V    : in Vertex);
      -- set the vertex of the branch elmt

      function Get_Branch_Elmt_Vertex
        (Elmt : in Branch_Elmt) return Vertex;
      -- return the vertex from the branch elmt

      procedure Set_Branch_Elmt_Arc
        (Elmt : in out Branch_Elmt;
         A    : in Arc);
      -- set the arc of the branch elmt

      function Get_Branch_Elmt_Arc
        (Elmt : in Branch_Elmt) return Arc;
      -- return the arc from the branch elmt

      ---------------------------------
      -- Procedures to REDUCE GRAPHS --
      ---------------------------------

      procedure Reduce_Graph (G  : in out Graph);

      procedure Reduce_Simple_Graph
        (G  : in out Graph;
         VL : in Vertex_Lists.List_Id;
         V  : in out Vertex);

   procedure Sort_By_Deep_Order (G : in Graph);

   end Algorithms;
   use Algorithms;


   package Dump is

      type Grafvis_Visitor_Record is new Graph_Visitor_Record with record
         Indent : Natural := 0;
         Ob : Output_Buffer;
         Tb : Output_Buffer;
      end record;

      procedure Visit_Initialize
        (V : in out Grafvis_Visitor_Record;
         G : in Graph);

      procedure Visit_Finalize (V : in out Grafvis_Visitor_Record);

      procedure Visit_Graph
        (V : in out Grafvis_Visitor_Record;
         S : in Visit_Step);

      procedure Visit_Vertex
        (V : in out Grafvis_Visitor_Record;
         S : in Visit_Step);

      procedure Visit_Arc
        (V : in out Grafvis_Visitor_Record;
         S : in Visit_Step);

      procedure Dump_Graph (G : in Graph);

      function Get_Dump_Name (G : in Graph) return String;

      procedure Set_Dump_Name
        (G : in Graph;
         N : in String);

      procedure Generate_Arc
        (V : in out Grafvis_Visitor_Record;
         A : in Arc);


      function Get_Output_Buffer
        (V : in Grafvis_Visitor_Record) return Output_Buffer;

      function Get_Temp_Buffer
        (V : in Grafvis_Visitor_Record) return Output_Buffer;

      procedure Set_Output_Buffer
        (V  : in out Grafvis_Visitor_Record;
         Ob : in Output_Buffer);

      procedure Set_Temp_Buffer
        (V  : in out Grafvis_Visitor_Record;
         Ob : in Output_Buffer);

      procedure Reset_Output_Buffer (V : in out Grafvis_Visitor_Record);

      procedure Reset_Temp_Buffer (V : in out Grafvis_Visitor_Record);

      procedure Emit
        (V : in out Grafvis_Visitor_Record;
         S : in String);

      procedure Emit_Line
        (V : in out Grafvis_Visitor_Record;
         S : in String);

      procedure Emit_Newline (V : in out Grafvis_Visitor_Record);

      procedure Do_Indentation (V : in out Grafvis_Visitor_Record);

      procedure Inc_Indentation (V : in out Grafvis_Visitor_Record);

      procedure Dec_Indentation (V : in out Grafvis_Visitor_Record);

      procedure Set_Indentation (V : in out Grafvis_Visitor_Record);

   end Dump;
   use Dump;


private

   type Graph_Node_Record is tagged record
      Name : String_Ptr := null;
      -- String used to name the ellements of graph.

      GItem : Graph_Item := No_Graph_Item;
      -- The user data.

      Properties : Properties_List := Properties_Lists.No_Properties;
      -- Liste des attributs du noeud (comment, version titre language locked)
   end record;


   -- A Vertex consists of an item, a pointer to the enclosing graph, a
   -- a list of incomming arcs and a list of outgoing arcs.

   type Vertex_Record is new Graph_Node_Record with record
      Vertex_Nb : Natural := 0;

      Item : Vertex_Item := No_Vertex_Item;
      -- The user data.

      Kind : Vertex_Kind;

      Enclosing : Graph := Null_Graph;
      -- The graf to wich the vertex belonged.

      Incoming : Arc_Lists.List_Id := Arc_Lists.No_List;
      -- The lists of all incomming arc to the vertex.

      Outgoing : Arc_Lists.List_Id := Arc_Lists.No_List;
      -- The list of all outgoing arc of the vertex.

      Mark : Boolean;
      -- Used by iterator.

      To_Root : Natural;
      -- Length from this vertex to root.

      To_Deep : Natural;

      BR : Branches_Record ;
      -- branches corresponding to vertex

      Tree_Cover : Natural;
      -- Number in Tree covering.
   end record;


   -- An Arc consists of an item, a pointer to the enclosing graph, a pointer
   -- the source vertex and a pointer to the target vertex.

   type Arc_Record is new Graph_Node_Record with record
      Arc_Nb : Natural := 0;

      Item : Arc_Item := No_Arc_Item;
      -- The user data attached to the arc.

      Enclosing : Graph := Null_Graph;
      -- The graf to which the arc belonged.

      From : Vertex := Null_Vertex;
      -- The source vertex.

      To : Vertex := Null_Vertex;
      -- the target vertex.

      Mark : Boolean := False;
      -- Used by iterator.
   end record;


   -- A graph is a set of vertex linked by arcs. One vertex is the root of the
   -- graph.
   type Graph_record is new Graph_Node_Record with record
      Dump_Name : String_Ptr := null;
      Vertex_List : Vertex_Lists.List_Id;
      -- The vertex of graph.
   end record;

   type Branch_Elmt_Record is tagged record
      V : Vertex := Null_Vertex;
      A : Arc := Null_Arc;
   end record;


end Artics.Graphs;
