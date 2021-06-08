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
-- Reflex is originally developed  by the Artics team at Grenoble (France). --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;

with Artics.Types; use Artics.Types;
with Artics.Namet; use Artics.Namet;
with Artics.Strings_Lists; use Artics.Strings_Lists;

with Artics.Objects; use Artics.Objects;
with Artics.Graph.Cells; use Artics.Graph.Cells;

with Dummy; use Dummy;

package Artics.Graph.Multiplicities is
   
   -----------------------------
   type Graph_Ptr is new Integer;
   -----------------------------
   
   type Multiplicity_Record is new Object_Record with private;
   type Multiplicity_Ptr is access all Multiplicity_Record;
   
   No_Multiplicity_Record : constant Multiplicity_Record;
   
   package Multiplicties_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Multiplicity_Ptr);
   
   
   function New_Multiplicity
     (Source                  : Boolean;
      Typ                     : String;
      Attr                    : String;
      Value                   : String;
      Min                     : Integer;
      Max                     : String;
      Valid_Neighbors         : Strings_Lists.List;
      Valid_Neighbors_Allowed : Boolean) return Multiplicity_Ptr;
      
   function Check
     (M          : access Multiplicity_Record;
      G          : Graph_Ptr;
      Edge       : Cell_Ptr;
      Source     : Cell_Ptr;
      Target     : Cell_Ptr;
      Source_Out : Integer;
      Target_In  : Integer) return String;
   -- Checks the multiplicity for the given arguments and returns the error
   -- for the given connection or null if the multiplicity does not apply.
   -- graph - Reference to the enclosing graph instance.
   -- edge - Cell that represents the edge to validate.
   -- source - Cell that represents the source terminal.
   -- target - Cell that represents the target terminal.
   -- sourceOut - Number of outgoing edges from the source terminal.
   -- targetIn - Number of incoming edges for the target terminal.
   
   function Check_Neighbors
     (M      : access Multiplicity_Record;
      G      : Graph_Ptr;
      Edge   : Cell_Ptr;
      Source : Cell_Ptr;
      Target : Cell_Ptr) return Boolean;
   -- Checks the type of the given value.
   
   function Check_Terminal
     (M        : access Multiplicity_Record;
      G        : Graph_Ptr;
      Terminal : Cell_Ptr;
      Edge     : Cell_Ptr) return Boolean;
   -- Checks the type of the given value.

   function Check_Type
     (M     : access Multiplicity_Record;
      G     : Graph_Ptr;
      Value : Cell_Ptr;
      Typ   : String) return Boolean;
   -- Checks the type of the given value.
   
   function Check_Type
     (M          : access Multiplicity_Record;
      G          : Graph_Ptr;
      Value      : Cell_Ptr;
      Typ        : String;
      Attr       : String;
      Attr_Value : String) return Boolean;
   -- Checks the type of the given value.
   
   function Is_Unlimited (M : access Multiplicity_Record) return Boolean;
   -- Returns true if max is "n" (unlimited).
   
   function Get_Max_Value (M : access Multiplicity_Record) return Integer;
   -- Returns the numeric value of max.
   
   function Clone_Multiplicity
     (M : Multiplicity_Ptr) return Multiplicity_Ptr;
   
private
   
   type Multiplicity_Record is new Object_Record with record
      Typ : Name_Id;
      -- Defines the type of the source or target terminal. The type is a string
      -- passed to mxUtils.isNode together with the source or target vertex
      -- value as the first argument.
      
      Attr : Name_Id;
      -- Optional string that specifies the attributename to be passed to
      -- mxCell.is to check if the rule applies to a cell.
      
      Value : Name_Id;
      -- Optional string that specifies the value of the attribute to be passed
      -- to mxCell.is to check if the rule applies to a cell.
      
      Source : Boolean;
      -- Boolean that specifies if the rule is applied to the source or target
      -- terminal of an edge.
      
      Min : Integer;
      -- Defines the minimum number of connections for which this rule applies.
      -- Default is 0.
      
      Max : Name_Id;
      -- Defines the maximum number of connections for which this rule applies.
      -- A value of 'n' means unlimited times. Default is 'n'. 
      
      Valid_Neighbors : Strings_Lists.List;
      -- Holds an array of strings that specify the type of neighbor for which
      -- this rule applies. The strings are used in mxCell.is on the opposite
      -- terminal to check if the rule applies to the connection.
      
      Valid_Neighbors_Allowed : Boolean;
      -- Boolean indicating if the list of validNeighbors are those that are
      -- allowed for this rule or those that are not allowed for this rule.
      
      Count_Error : Name_Id;
      -- Holds the localized error message to be displayed if the number of
      -- connections for which the rule applies is smaller than min or greater
      -- than max.
      
      Type_Error : Name_Id;
      -- Holds the localized error message to be displayed if the type of the
      -- neighbor for a connection does not match the rule.
   end record;
   
   No_Multiplicity_Record : constant Multiplicity_Record := Multiplicity_Record'
     (No_Object_Record with
      Typ                     => No_Name,
      Attr                    => No_Name,
      Value                   => No_Name,
      Source                  => False,
      Min                     => 0,
      Max                     => String_Find ("n"),
      Valid_Neighbors         => Strings_Lists.Null_List,
      Valid_Neighbors_Allowed => True,
      Count_Error             => No_Name,
      Type_Error              => No_Name);
   
end Artics.Graph.Multiplicities;
	  
	  
