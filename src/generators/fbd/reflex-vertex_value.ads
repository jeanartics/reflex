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
-- Reflex is originally developed by Artics
------------------------------------------------------------------------------

with Artics.Objects;     use Artics.Objects;
with Artics.Graph.Cells; use Artics.Graph.Cells;

with Sem_Util;  use Sem_Util;
with Sinfo;     use Sinfo;
with Einfo;     use Einfo;
with Atree;     use Atree;
with Namet;     use Namet;
with Nlists;    use Nlists;
with Nmake;     use Nmake;
with Tbuild;    use Tbuild;

package Reflex.Vertex_Value is
      
   type Vertex_Value_Record is new Object_Record with private;
   type Vertex_Value_Ptr is access all Vertex_Value_Record;
   type Vertex_Value_Class_Ptr is access all Vertex_Value_Record'Class;

   type Vertex_Kind is
     (No_Vertex_Kind,
      Enclose_All_Vertex,
      Formal_In_Vertex,
      Formal_Out_Vertex,
      Operator_Vertex,
      Assignement_Vertex,
      Identifier_Vertex,
      Procedure_Call_Vertex,
      Enclose_Vertex);
   
   No_Vertex_Value_Record : constant Vertex_Value_Record;
   
   function New_Vertex_Value return Vertex_Value_Ptr;
   procedure Free_Vertex_Value (This : in out Vertex_Value_Ptr);
   
   function Get_Node (This : access Vertex_Value_Record) return Types.Node_Id;
   procedure Set_Node (This : access Vertex_Value_Record; Node : Node_Id);

   function Get_Vertex_Kind
     (This : access Vertex_Value_Record) return Vertex_Kind;
   procedure Set_Vertex_Kind
     (This   : access Vertex_Value_Record; 
      Vertex : Vertex_Kind);
   
   function  Get_Has_Multiple_Out 
     (This : access Vertex_Value_Record) return Boolean;
   procedure Set_Has_Multiple_Out 
     (This : access Vertex_Value_Record; 
      Bool : Boolean);
   
   function  Get_Is_Negate_Vertex
     (This : access Vertex_Value_Record) return Boolean;
   procedure Set_Is_Negate_Vertex 
     (This : access Vertex_Value_Record; 
      Bool : Boolean);
   
   function Get_Vertexs_In 
     (This : access Vertex_Value_Record) return Cells_Lists.List;
   procedure Set_Vertexs_In
     (This    : access Vertex_Value_Record;
      Vertexs : Cells_Lists.List);
   
   function Get_Vertexs_Out
     (This : access Vertex_Value_Record) return Cells_Lists.List;
   procedure Set_Vertexs_Out
     (This   : access Vertex_Value_Record;
      Vertexs : Cells_Lists.List);
      
   procedure Append_Vertex_In
     (This   : access Vertex_Value_Record;
      Vertex : access Cell_Record);
   
   procedure Append_Vertex_Out
     (This   : access Vertex_Value_Record;
      Vertex : access Cell_Record);
      
   function Get_Cell_To_Link
     (This : access Vertex_Value_Record) return access Cell_Record;
   procedure Set_Cell_To_Link
     (This : access Vertex_Value_Record;
      Cell : access Cell_Record);
   
   function Get_Effective_Param
     (This : access Vertex_Value_Record) return Node_Id;
   procedure Set_Effective_Param
     (This : access Vertex_Value_Record;
      Node : Node_Id);
   
   function Get_Formal_Name
     (This : access Vertex_Value_Record) return Name_Id;
   procedure Set_Formal_Name
     (This   : access Vertex_Value_Record;
      String : Name_Id );

private
   
   type Vertex_Value_Record is new Object_Record with record
      Node             : Node_Id;
      Kind             : Vertex_Kind;
      Has_Multiple_Out : Boolean;
      Is_Negate_Vertex : Boolean;
      Vertexs_In       : Cells_Lists.List;
      Vertexs_Out      : Cells_Lists.List;
      Cell_To_Link     : access Cell_Record;
      Effective_Param  : Node_Id;
      Formal_Name      : Name_Id;
      
   end record;
   
   No_Vertex_Value_Record : constant Vertex_Value_Record := Vertex_Value_Record'
     (No_Object_Record with
      Node             => Empty,
      Kind             => No_Vertex_Kind,
      Has_Multiple_Out => False,
      Is_Negate_Vertex => False,
      Vertexs_In       => Cells_Lists.Empty_List,
      Vertexs_Out      => Cells_Lists.Empty_List,
      Cell_To_Link     => null,
      Effective_Param  => Empty,
      Formal_Name      => No_Name);

end Reflex.Vertex_Value;
