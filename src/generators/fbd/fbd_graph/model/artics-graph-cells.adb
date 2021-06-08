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
------------------------------------------------------------------------------

with Ada.Text_Io; 
with Ada.Unchecked_Conversion;
with Artics.Namet; use Artics.Namet;
with Artics.Utils; use Artics.Utils;
with Artics.Graph.Cells_Paths; use Artics.Graph.Cells_Paths;
with Ada.Text_IO; use Ada.Text_IO;

package body Artics.Graph.Cells is
   
   -------------------
   -- Hash_Function --
   -------------------
   
   function Equivalent_Key (Left, Right : Name_Id) return Boolean is
   begin
      return Left = Right;
   end Equivalent_Key;
 
   function Hash_Func(Key : Unbounded_String) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash(To_String(Key));
   end Hash_Func;
   
   --------------
   -- New_Cell --
   --------------
   
   function New_Cell return access Cell_Record'Class is
      C : access Cell_Record := new Cell_Record'(No_Cell_Record);
   begin
      return C;
   end New_Cell;
   
   --------------
   -- New_Cell --
   --------------
   
   function New_Cell
     (Value : access Object_Record'Class) return access Cell_Record'Class is
      C : access Cell_Record := New_Cell;
   begin
      Set_Value (C, Value);
      return C;
   end New_Cell;
   
   --------------
   -- New_Cell --
   --------------
   
   function New_Cell
     (Value    : access Object_Record'Class;
      Geometry : access Cell_Geometry_Record'Class) return access Cell_Record'Class is
      
      C : access Cell_Record := New_Cell;
   begin
      Set_Value (C, Value);
      Set_Geometry (C, Geometry);
      
      return C;
   end New_Cell;
   
   ------------
   -- Get_Id --
   ------------
   
   function Get_Id (C : access Cell_Record) return String is
   begin
      return Get_String (C.Id);
   end Get_Id;
   
   ------------
   -- Set_Id --
   ------------
   
   procedure Set_Id
     (C  : access Cell_Record;
      Id : String) is
   begin
      if Id = "" then
         C.Id := No_Name;
      else
         C.Id := String_Find (Id);
      end if;
   end Set_Id;
   
   ---------------
   -- Get_Value --
   ---------------
   
   function Get_Value
     (C : access Cell_Record) return access Object_Record'Class is
   begin
      return C.Value;
   end Get_Value;
   
   ---------------
   -- Set_Value --
   ---------------
   
   procedure Set_Value
     (C : access Cell_Record;
      V : access Object_Record'Class) is
   begin
      C.Value := V;
   end Set_Value;
   
   ------------------
   -- Get_Geometry --
   ------------------
   
   function Get_Geometry
     (C : access Cell_Record) return access Cell_Geometry_Record'Class is
   begin
      return C.Geometry;
   end Get_Geometry;
   
   ------------------
   -- Set_Geometry --
   ------------------
   
   procedure Set_Geometry
     (C        : access Cell_Record;
      Geometry : access Cell_Geometry_Record'Class) is
   begin
      C.Geometry := Geometry;
   end Set_Geometry;
   
  
   ---------------
   -- Is_Vertex --
   ---------------
   
   function Is_Vertex (C : access Cell_Record) return Boolean is
   begin
      return C.Vertex;
   end Is_Vertex;
   
   ----------------
   -- Set_Vertex --
   ----------------
   
   procedure Set_Vertex
     (C      : access Cell_Record;
      Vertex : Boolean) is
   begin
      C.Vertex := Vertex;
   end Set_Vertex;
   
   -------------
   -- Is_Edge --
   -------------
   
   function Is_Edge (C : access Cell_Record) return Boolean is
   begin
      return C.Edge;
   end Is_Edge;
   
   --------------
   -- Set_Edge --
   --------------
   
   procedure Set_Edge
     (C    : access Cell_Record;
      Edge : Boolean) is
   begin
      C.Edge := Edge;
   end Set_Edge;
      
   ----------------
   -- Get_parent --
   ----------------
   
   function Get_Parent (C : access Cell_Record) return access Cell_Record
   is
   begin
      return C.Parent;
   end Get_Parent;
   
   ----------------
   -- Set_Parent --
   ----------------
   
   procedure Set_Parent
     (C      : access Cell_Record;
      Parent : access Cell_Record) is
   begin
      C.Parent := Parent;
   end Set_Parent;
   
   ------------------
   -- Reset_Parent --
   ------------------
   
   procedure Reset_Parent (C : access Cell_Record) is
   begin
      C.Parent := null;
   end Reset_Parent;
   
   ----------------
   -- Get_Source --
   ----------------
   
   function Get_Source (C : access Cell_Record) return access Cell_Record
   is
   begin
      return C.Source;
   end Get_Source;
   
   ----------------
   -- Set_Source --
   ----------------
   
   procedure Set_Source
     (C      : access Cell_Record;
      Source : access Cell_Record'Class) is
   begin
      C.Source := Source;
   end Set_Source;
   
   ----------------
   -- Get_Target --
   ----------------
   
   function Get_Target (C : access Cell_Record) return access Cell_Record
   is
   begin
      return C.Target;
   end Get_Target;
   
   
   ----------------
   -- Set_Target --
   ----------------
   
   procedure Set_Target
     (C      : access Cell_Record;
      Target : access Cell_Record'Class) is
   begin
      C.Target := Target;
   end Set_Target;
   
   
   ------------------
   -- Get_Terminal --
   ------------------
   
   function Get_Terminal 
     (C         : access Cell_Record;
      Is_Source : Boolean) return access Cell_Record is
   begin
      if Is_Source then
         return Get_Source (C);
      else
         return Get_Target (C);
      end if;
   end Get_Terminal;
   
   ------------------
   -- Set_Terminal --
   ------------------
   
   procedure Set_Terminal 
     (C         : access Cell_Record;
      Terminal  : access Cell_Record;
      Is_Source : Boolean) is
   begin
      if Is_Source then
         C.Set_Source (Terminal);
      else
         C.Set_Target (Terminal);
      end if;
   end Set_Terminal;
   
   --------------------
   -- Reset_Terminal --
   --------------------
   
   procedure Reset_Terminal 
     (C         : access Cell_Record;
      Is_Source : Boolean) is
   begin
      if Is_Source then
         C.Source := null;
      else
         C.Target := null;
      end if;
   end Reset_Terminal;
   
   -----------------------
   -- Get_Children_List --
   -----------------------
   
   function Get_Children_List
     (C : access Cell_Record) return Cells_Lists.List is
   begin
      return C.Children;
   end Get_Children_List;
   
   ---------------------------
   -- Replace_Children_List --
   ---------------------------
   
   procedure Replace_Children_List
     (C : access Cell_Record; New_List : Cells_Lists.List) is
   begin
      C.Children := New_List;
   end Replace_Children_List;
      
   -------------------
   -- Sort_Children --
   -------------------
   
   procedure Sort_Children (C : Cell_Class_Ptr; CB : Sort_Call_Back) is
      function Call_CB_Inf (Left, Right : Cell_Class_Ptr) return Boolean is
      begin
         return CB.all (Left, Right);
      end Call_CB_Inf;
      
      package Sort_Pack is new Cells_Lists.Generic_Sorting (Call_CB_Inf);
   begin
      if CB /= null then         
         Sort_Pack.Sort (C.Children);
      end if;
   end Sort_Children;
   
   ----------------
   -- Cell_Count --
   -----------------
   
   function Get_Child_Count (C : access Cell_Record) return Integer is
   begin
      return List_Length (C.Children);
   end Get_Child_Count;
   
   ---------------
   -- Get_Index --
   ---------------
   
   function Get_Index
     (C     : access Cell_Record;
      Child : access Cell_Record) return Integer
   is
   begin
      return Get_Position (C.Children, Child);
   end Get_Index;
   
   ------------------
   -- Get_Child_At --
   ------------------
   
   function Get_Child_At 
     (C     : access Cell_Record;
      Index : Integer) return access Cell_Record is
   begin
      return Get_At (C.Children, Index);
   end Get_Child_At;
   
   ------------
   -- Insert --
   ------------
   
   procedure Insert
     (C     : access Cell_Record;
      Child : access Cell_Record) 
   is
   begin
      if Child /= null then
         Remove_From_Parent (Child);
         Child.Set_Parent (C);
         Cells_Lists.Append (C.Children, Child);
      end if;
   end Insert;
   
   ------------
   -- Insert --
   ------------
   
   procedure Insert_Child_Before_Cell
     (C     : access Cell_Record;
      Cell  : access Cell_Record;
      Child : access Cell_Record) 
   is
      Pos : Integer;
   begin
      if Child /= null then
         Remove_From_Parent (Child);
         Child.Set_Parent (C);
         Pos := Get_Position (C.Children, Cell);
	 
         if Pos < 1 then
            Cells_Lists.Append (C.Children, Child);
         else
            Cells_Lists_Helpers.Insert_At (C.Children, Pos, Child);
         end if;
      end if;
   end Insert_Child_Before_Cell;
   
   ------------
   -- Insert --
   ------------
   
   procedure Insert
     (C     : access Cell_Record;
      Child : access Cell_Record;
      Index : Integer) is
   begin
      if Child /= null then
         Remove_From_Parent (Child);
         Child.Set_Parent (C);
	 
         Insert_At (C.Children, Index, Child);
      end if;
   end Insert;
   
   ------------
   -- Remove --
   ------------
  
   procedure Remove
     (C     : access Cell_Record;
      Index : Integer) is
      
      Child : access Cell_Record;
   begin
      if C /= null then
         Child := Get_At (C.Children, Index);
         Remove (C, Child);
      end if;
   end Remove;
   
   ------------
   -- Remove -- 
   ------------
  
   procedure Remove
     (C     : access Cell_Record;
      Child : access Cell_Record) is
   begin
      if Child /= null then
         Remove_Element (C.Children, Child);
	 
         Child.Reset_Parent;
      end if;
   end Remove;
   
   ------------------------
   -- Remove_From_Parent --
   ------------------------
   
   procedure Remove_From_Parent (C : access Cell_Record) is
      
      Parent : access Cell_Record := Get_Parent (C);
   begin
      if Parent /= null then
         Remove (Parent, C);
      end if;
   end Remove_From_Parent;
   
   --------------------
   -- Get_Edges_List --
   --------------------
   
   function Get_Edges_List
     (C : access Cell_Record) return Cells_Lists.List is
   begin
      return C.Edges;
   end Get_Edges_List;
   
   --------------------
   -- Get_Edge_Count --
   --------------------
   
   function Get_Edge_Count (C : access Cell_Record) return Integer is
   begin
      return List_Length (C.Edges);
   end Get_Edge_Count;
   
   --------------------
   -- Get_Edge_Index --
   --------------------
   
   function Get_Edge_Index
     (C    : access Cell_Record;
      Edge : access Cell_Record) return Integer is
   begin
      return Get_Position (C.Edges, Edge);
   end Get_Edge_Index;
   
   -----------------
   -- Get_Edge_At --
   -----------------
   
   function Get_Edge_At
     (C     : access Cell_Record;
      Index : Integer) return access Cell_Record is
   begin
      return Get_At (C.Edges, Index);
   end Get_Edge_At;
   
   -----------------
   -- Insert_Edge --
   -----------------
   
   procedure Insert_Edge
     (C           : access Cell_Record;
      Edge        : access Cell_Record;
      Is_Outgoing : Boolean) is
   begin
      if Edge /= null then
         Edge.Remove_From_Terminal (Is_Outgoing);
         Edge.Set_Terminal (C, Is_Outgoing);
	 
         if not Cells_Lists.Contains (C.Edges, Edge) then
            Cells_Lists.Append (C.Edges, Edge);
         end if;
      end if;
   end Insert_Edge;
   
   -----------------
   -- Remove_Edge --
   -----------------
   
   procedure Remove_Edge
     (C           : access Cell_Record;
      Edge        : access Cell_Record;
      Is_Outgoing : Boolean) is
      
      use Cells_Lists;
      Index : Cells_Lists.Cursor;
   begin
      if Edge /= null then
         if Edge.Get_Terminal (not Is_Outgoing) /= C then
            Remove_Element (C.Edges, Edge);
         end if;
         Edge.Reset_Terminal (Is_Outgoing);
      end if;
   end Remove_Edge;
   
   --------------------------
   -- Remove_From_Terminal --
   --------------------------
   
   procedure Remove_From_Terminal
     (C         : access Cell_Record;
      Is_Source : Boolean) is
      
      Terminal : access Cell_Record;
   begin
      Terminal := C.Get_Terminal (Is_Source);
      if Terminal /= null then
         Terminal.Remove_Edge (C, Is_Source);
      end if;
   end Remove_From_Terminal;
   
   -------------------
   -- Get_Attribute --
   -------------------
   
   function Get_Attribute
     (C    : access Cell_Record;
      Name : String) return String is
   begin
      return C.Get_Attribute (Name, "");
   end Get_Attribute;
   
   -------------------
   -- Get_Attribute --
   -------------------
   
   function Get_Attribute
     (C             : access Cell_Record;
      Name          : String;
      Default_Value : String) return String is
   begin
      return "";
   end Get_Attribute;
   
   -------------------
   -- Set_Attribute --
   -------------------
   
   procedure Set_Attribute
     (C     : access Cell_Record;
      Name  : String;
      Value : String) 
   is
   begin
      null;
   end Set_Attribute;
   
   -----------
   -- Clone --
   -----------
   
   function Clone
     (C                 : access Cell_Record;
      Include_Childrens : Boolean := False) return access Cell_Record 
   is
      Cloned       : access Cell_Record := null;
      Childs_List  : Cells_Lists.List; 
      Crt          : Cells_Lists.Cursor;
      Crt_Child    : access Cell_Record;
      Cloned_Child : access Cell_Record;
   begin
      if C = null then      
         return Cloned;
      end if;
      
      Cloned := New_Cell;      
      Cloned.Set_Id (C.Get_Id);               
      Cloned.Set_Value (C.Get_Value);      
         
      Cloned.Set_Edge (C.Is_Edge);               
      Cloned.Set_Vertex (C.Is_Vertex);  
      Cloned.Parent := null;               
      Cloned.Set_Source (null);      
      Cloned.Set_Target (null);            
      Cloned.Edges := Cells_Lists.Empty_List; 
      
      declare 
         Geom : access Cell_Geometry_Record'Class := C.Get_Geometry;
      begin
         Cloned.Set_Geometry (Geom);
      end;
      
      --  clone childrens of cell
      if Include_Childrens 
        and then not Cells_Lists.Is_Empty (C.Get_Children_List) 
      then
         Childs_List := C.Get_Children_List;
         Crt := Cells_Lists.First (Childs_List);
         while Cells_Lists.Has_Element (Crt) loop
            Crt_Child := Cell_Class_Ptr (Cells_Lists.Element (Crt));
            Cloned_Child := Crt_Child.Clone (Include_Childrens => True);
            Cloned.Insert (Cloned_Child);
            Cells_Lists.Next (Crt);
         end loop;
      else
         Cloned.Children := Cells_Lists.Empty_List;         
      end if;

      return Cloned;
   end Clone;
   
   -----------------
   -- Clone_Value --
   -----------------
   
   function Clone_Value return Object_Ptr is
   begin
      return null;
   end Clone_Value;
   
   
   function Hash_Func(Key : Name_Id) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (Get_String (Key));
   end Hash_Func;
   
   ----------------------
   -- Cells String Map --
   ----------------------
   
   function Cs_Equivalent_Key
     (Left, Right : Cell_Class_Ptr) return Boolean is
   begin
      return Left = Right;
   end Cs_Equivalent_Key;
 
   function Cs_Hash_Func
     (Key : Cell_Class_Ptr) return Ada.Containers.Hash_Type is
      function To_Integer is new Ada.Unchecked_Conversion 
        (Cell_Class_Ptr, Integer);
   begin
      return Ada.Containers.Hash_Type (To_Integer (Key));
   end Cs_Hash_Func;
   
   ----------------
   -- Cells Sets --
   ----------------
   
   function Set_Equivalent_Key
     (Left, Right : Cell_Class_Ptr) return Boolean is
   begin
      return Left = Right;
   end Set_Equivalent_Key;
 
   function Set_Hash_Func
     (Key : Cell_Class_Ptr) return Ada.Containers.Hash_Type is
      function To_Integer is new Ada.Unchecked_Conversion 
        (Cell_Class_Ptr, Integer);
   begin
      return Ada.Containers.Hash_Type (To_Integer (Key));
   end Set_Hash_Func;
   
   function Int_Equivalent_Key
     (Left, Right : Cell_Class_Ptr) return Boolean is
   begin
      return Left = Right;
   end Int_Equivalent_Key;
 
   function Int_Hash_Func
     (Key : Cell_Class_Ptr) return Ada.Containers.Hash_Type is
      function To_Integer is new Ada.Unchecked_Conversion 
        (Cell_Class_Ptr, Integer);
   begin
      return Ada.Containers.Hash_Type (To_Integer (Key));
   end Int_Hash_Func;
   
   ---------------------------
   -- Add_List_To_Cells_Set --
   ---------------------------
   
   procedure Add_List_To_Cells_Set
     (Set   : in out Cells_Sets.Map;
      Cells : Cells_Lists.List) is
   begin
      for Cell of Cells loop
         if not Cells_Sets.Contains (Set, Cell) then
            Cells_Sets.Insert (Set, Cell, True);
         end if;
      end loop;
   end Add_List_To_Cells_Set;
   
   ---------------------------
   -- Add_Cell_To_Cells_Set --
   ---------------------------
   
   procedure Add_Cell_To_Cells_Set
     (Set  : in out Cells_Sets.Map;
      Cell : access Cell_Record'Class) is
   begin
      if not Cells_Sets.Contains (Set, Cell) then
         Cells_Sets.Insert (Set, Cell, True);
      end if;
   end Add_Cell_To_Cells_Set;
   
   ----------------------
   -- String_Cells Map --
   ----------------------
   
   function SC_Equivalent_Key
     (Left, Right : Name_Id) return Boolean is
   begin
      return Left = Right;
   end SC_Equivalent_Key;
 
   function SC_Hash_Func
     (Key : Name_Id) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Key);
   end SC_Hash_Func;
   
   function Cell_Equal (Left, Right : Cell_Class_Ptr) return Boolean is
   begin
      return Left = Right;
   end Cell_Equal;
   
   
   ----------------------------
   -- Forward_Cursor_Cell_At --
   ----------------------------
   
   function Forward_Cursor_Cell_At 
     (Cells : Cells_Lists.List;
      Index : Integer) return Cells_Lists.Cursor is
      
      use Cells_Lists;
      
      Cur : Cells_Lists.Cursor;
   begin
      if Index < 1 
        or else Cells_Lists.Is_Empty (Cells) 
        or else Integer (Cells_Lists.Length (Cells)) < Index 
      then
         return Cells_Lists.No_Element;
	 
         -- Here we know that index is a valid position in the list, no need to
         -- test Has_Element, as Index is <= length of the list
	 
      else
         Cur := Cells_Lists.First (Cells);
         for I in 2..Index loop
            Cells_Lists.Next (Cur);
            exit when Cur = Cells_Lists.No_Element;
         end loop;
      end if;
      
      return Cur;
   end Forward_Cursor_Cell_At;
   
   -----------------
   -- Get_Cell_At --
   -----------------
   
   function Get_Cell_At 
     (Cells : Cells_Lists.List;
      Index : Integer) return access Cell_Record'Class is
      
      use Cells_Lists;
      
      Cur : Cells_Lists.Cursor;
   begin
      Cur := Forward_Cursor_Cell_At (Cells, Index);
      
      if Cur /= Cells_Lists.No_Element then
         return Cells_Lists.Element (Cur);
      else
         return null;
      end if;
   end Get_Cell_At;
   
   -----------------
   -- Set_Cell_At --
   -----------------
   
   procedure Set_Cell_At
     (Cells : in out Cells_Lists.List;
      Index : Integer;
      Cell  : access Cell_Record'Class) is
      
      use Cells_Lists;
      
      Cur : Cells_Lists.Cursor;
   begin
      -- If Index is > thanthe length of the list, the cell is appended at the
      -- end of the list, no exception is raised if the index is > length + 1
      
      if Index = 0 then
         Cells_Lists.Prepend (Cells, Cell);
	 
      else	 
         Cur := Forward_Cursor_Cell_At (Cells, Index);
	 
         if Cur = Cells_Lists.No_Element then
            Cells_Lists.Append (Cells, Cell);
         else
            Cells_Lists.Insert (Cells, Cur, Cell);
         end if;
      end if;
   end Set_Cell_At;
   
   --------------------
   -- Get_First_Cell --
   --------------------
   
   function Get_First_Cell
     (Cells : Cells_Lists.List) return access Cell_Record'Class is
   begin
      if Cells_Lists.Is_Empty (Cells) then
         return null;
      else
         return Cells_Lists.First_Element (Cells);
      end if;
   end Get_First_Cell;
   
   ---------------------
   -- Get_Second_Cell --
   ---------------------
   
   function Get_Second_Cell
     (Cells : Cells_Lists.List) return access Cell_Record'Class is
      
      use Cells_Lists;
      
      Cur : Cells_Lists.Cursor;
   begin
      Cur := Forward_Cursor_Cell_At (Cells, 2);
      
      if Cur /= Cells_Lists.No_Element then
         return Cells_Lists.Element (Cur);
      else
         return null;
      end if;
   end Get_Second_Cell;
   
   -------------------
   -- Get_Last_Cell --
   -------------------
   
   function Get_Last_Cell
     (Cells : Cells_Lists.List) return access Cell_Record'Class is
   begin
      if Cells_Lists.Is_Empty (Cells) then
         return null;
      else
         return Cells_Lists.Last_Element (Cells);
      end if;
   end Get_Last_Cell;
   
   -----------------------------
   -- Get_Last_Minus_One_Cell --
   -----------------------------
   
   function Get_Last_Minus_One_Cell
     (Cells : Cells_Lists.List) return access Cell_Record'Class is
      
      use Cells_Lists;
      
      Cur : Cells_Lists.Cursor;
   begin
      Cur := Cells_Lists.Last (Cells);
      Cur := Cells_Lists.Previous (Cur);
      if Cur /= Cells_Lists.No_Element then
         return Cells_Lists.Element (Cur);
      else
         return null;
      end if;
   end Get_Last_Minus_One_Cell;
   
   -----------------
   -- Append_List --
   -----------------
   
   procedure Append_List 
     (To   : in out Cells_Lists.List;
      From : Cells_Lists.List) is
   begin
      for Cell of From loop
         if not Cells_Lists.Contains (To, Cell) then
            Cells_Lists.Append (To, Cell);
         end if;
      end loop;
   end Append_List;
   
   -----------------
   -- Get_Depends --
   -----------------
   
   function Get_Depends (C : access Cell_Record'Class) return Cells_Lists.List
   is
   begin
      return C.Depends;
   end Get_Depends;
   
   -----------------
   -- Set_Depends --
   -----------------
   
   procedure Set_Depends
     (C       : access Cell_Record'Class;
      Depends : Cells_Lists.List) 
   is
   begin
      C.Depends := Depends;
   end Set_Depends;   
      
   ------------------
   -- Sort_Depends --
   -------------------
   
   procedure Sort_Depends (C : Cell_Class_Ptr; CB : Sort_Call_Back) is
      function Call_CB_Inf (Left, Right : Cell_Class_Ptr) return Boolean is
      begin
         return CB.all (Left, Right);
      end Call_CB_Inf;
      
      package Sort_Pack is new Cells_Lists.Generic_Sorting (Call_CB_Inf);
   begin
      if CB /= null then        
         Sort_Pack.Sort (C.Depends);
      end if;
   end Sort_Depends;
   
   ------------------
   --  Is Visited  --
   ------------------
   
   function Is_Visited (C : access Cell_Record) return Boolean
   is 
   begin
      return C.Visited;
   end Is_Visited;
   
   
   -------------------
   --  Set_Visited  --
   -------------------
   
   procedure Set_Visited
     (C     : access Cell_Record;
      Mark  : Boolean)
   is
   begin
      C.Visited := Mark;
   end Set_Visited;
      

   -----------------
   --  Get_Layer  --
   -----------------
   
   function Get_Layer  (C     : access Cell_Record) return Integer
   is
   begin
      return C.Layer;
   end Get_Layer;
   
   
   -----------------
   --  Set_Layer  --
   -----------------
   
   procedure Set_Layer (C     : access Cell_Record;
                        Layer : Integer)
   is
   begin
      C.Layer := Layer;
   end Set_Layer;
   
   -----------------
   -- Get_Y_Layer --
   -----------------
   
   function Get_Y_Layer
     (C : access Cell_Record) return Integer is
   begin
      return C.Y_Layer;
   end Get_Y_Layer;
   
   -----------------
   -- Set_Y_Layer --
   -----------------
   
   procedure Set_Y_Layer
     (C     : access Cell_Record;
      Layer : Integer) is
   begin
      C.Y_Layer := Layer;
   end Set_Y_Layer;
   
   --------------------------
   -- Get_Vertices_Forward --
   --------------------------
   
   function Get_Vertices_Forward
     (This : access Cell_Record) return Cells_Lists.List is
   begin
      return This.Vertices_Forward;
   end Get_Vertices_Forward;
   
   --------------------------
   -- Set_Vertices_Forward --
   --------------------------
   
   procedure Set_Vertices_Forward
     (This : access Cell_Record;
      L    : Cells_Lists.List) is
   begin
      This.Vertices_Forward := L;
   end Set_Vertices_Forward;
      
   ---------------------------
   -- Sort_Vertices_Forward --
   ---------------------------
   
   procedure Sort_Vertices_Forward (C : Cell_Class_Ptr; CB : Sort_Call_Back) is
      function Call_CB_Inf (Left, Right : Cell_Class_Ptr) return Boolean is
      begin
         return CB.all (Left, Right);
      end Call_CB_Inf;
      
      package Sort_Pack is new Cells_Lists.Generic_Sorting (Call_CB_Inf);
   begin
      if CB /= null then        
         Sort_Pack.Sort (C.Vertices_Forward);
      end if;
   end Sort_Vertices_Forward;
      
   ---------------------------
   -- Get_Vertices_Backward --
   ---------------------------
   
   function Get_Vertices_Backward
     (This : access Cell_Record) return Cells_Lists.List is
   begin
      return This.Vertices_Backward;
   end Get_Vertices_Backward;
   
   ---------------------------
   -- Set_Vertices_Backward --
   ---------------------------
   
   procedure Set_Vertices_Backward
     (This : access Cell_Record;
      L    : Cells_Lists.List) is
   begin
      This.Vertices_Backward := L;
   end Set_Vertices_Backward;
      
   ---------------------------
   -- Sort_Vertices_Backward --
   ---------------------------
   
   procedure Sort_Vertices_Backward (C : Cell_Class_Ptr; CB : Sort_Call_Back) is
      function Call_CB_Inf (Left, Right : Cell_Class_Ptr) return Boolean is
      begin
         return CB.all (Left, Right);
      end Call_CB_Inf;
      
      package Sort_Pack is new Cells_Lists.Generic_Sorting (Call_CB_Inf);
   begin
      if CB /= null then        
         Sort_Pack.Sort (C.Vertices_Backward);
      end if;
   end Sort_Vertices_Backward;
   
   -----------------------------
   -- Append_Vertices_Forward --
   -----------------------------
   
   procedure Append_Vertices_Forward
     (This : access Cell_Record;
      Cell : access Cell_Record) is
   begin
      if not Cells_Lists.Contains (This.Vertices_Forward, Cell) then
         Cells_Lists.Append (This.Vertices_Forward, Cell);
      end if;
   end Append_Vertices_Forward;
   
   --------------------
   -- Remove_Forward --
   --------------------
   
   procedure Remove_Forward
     (This : access Cell_Record;
      Cell : access Cell_Record) is
   begin
      Remove_Element (This.Vertices_Forward, Cell);
   end Remove_Forward;
   
   ------------------------------
   -- Append_Vertices_Backward --
   ------------------------------
   
   procedure Append_Vertices_Backward
     (This : access Cell_Record;
      Cell : access Cell_Record) is
   begin
      if not Cells_Lists.Contains (This.Vertices_Backward, Cell) then
         Cells_Lists.Append (This.Vertices_Backward, Cell);
      end if;
   end Append_Vertices_Backward;
   
   ----------------------------
   -- Get_Virtual_Precedings --
   ----------------------------
   
   function Get_Virtual_Precedings
     (This : access Cell_Record) return Cells_Lists.List
   is
   begin
      return This.Virtual_Precedings;
   end Get_Virtual_Precedings;
   
   ------------------------------
   -- Append_Virtual_Preceding --
   ------------------------------
   
   procedure Append_Virtual_Preceding
     (This    : access Cell_Record;
      Cell    : access Cell_Record) is
   begin
      if not Cells_Lists.Contains (This.Virtual_Precedings, Cell) then
         Cells_Lists.Append (This.Virtual_Precedings, Cell);
      end if;      
   end Append_Virtual_Preceding;
   
   ---------------------------
   -- Has_Virtual_Preceding --
   ---------------------------
   
   function Has_Virtual_Preceding
     (This    : access Cell_Record;
      Cell    : access Cell_Record;
      Recurse : Boolean := False) return Boolean is
      
      Prec : Cell_Class_Ptr;
   begin
      if not Recurse then
         return Cells_Lists.Contains (This.Virtual_Precedings, Cell);
      end if;
      
      if Cells_Lists.Contains (This.Virtual_Precedings, Cell) then
         return True;
      end if;
      for P of This.Virtual_Precedings loop
         Prec := P;
         if Prec.Has_Virtual_Preceding (Cell_Class_Ptr (Cell),  Recurse) then
            return True;
         end if;
      end loop;
      return False;
   end Has_Virtual_Preceding;
   
   ----------------------------
   -- Get_Virtual_Followings --
   ----------------------------
   
   function Get_Virtual_Followings
     (This : access Cell_Record) return Cells_Lists.List
   is
   begin
      return This.Virtual_Followings;
   end Get_Virtual_Followings;
   
   ------------------------------
   -- Append_Virtual_Following --
   ------------------------------
   
   procedure Append_Virtual_Following
     (This    : access Cell_Record;
      Cell    : access Cell_Record) is
   begin
      if not Cells_Lists.Contains (This.Virtual_Followings, Cell) then         
         Cells_Lists.Append (This.Virtual_Followings, Cell);
      end if;
   end Append_Virtual_Following;
   
   ---------------------------
   -- Has_Virtual_Following --
   ---------------------------
   
   function Has_Virtual_Following
     (This    : access Cell_Record;
      Cell    : access Cell_Record;
      Recurse : Boolean := False) return Boolean is
      
      Follower : Cell_Class_Ptr;
   begin
      if not Recurse then
         return Cells_Lists.Contains (This.Virtual_Followings, Cell);
      end if;
      
      if Cells_Lists.Contains (This.Virtual_Followings, Cell) then
         return True;
      end if;
      for F of This.Virtual_Followings loop
         Follower := F;
         if Follower.Has_Virtual_Following (Cell_Class_Ptr (Cell), Recurse) then
            return True;
         end if;
      end loop;
      return False;
   end Has_Virtual_Following;
   
   -------------------
   -- Get_Edge_Mark --
   -------------------
   
   function Get_Edge_Mark (This : access Cell_Record) return Boolean is
   begin
      return This.Edge_Mark;
   end Get_Edge_Mark;
   
   -------------------
   -- Set_Edge_Mark --
   -------------------
   
   procedure Set_Edge_Mark
     (This : access Cell_Record;
      Mark : Boolean) is
   begin
      This.Edge_Mark := Mark;
   end Set_Edge_Mark;
   
   -----------------
   -- Is_Ancestor --
   -----------------
   
   function Is_Parent_Ancestor
     (Parent : access Cell_Record'Class;
      Child  : access Cell_Record'Class) return Boolean 
   is 
      C : access Cell_Record'Class := Child;
   begin
      while C /= null and then C /= Parent loop
         C := Get_Parent (C);
      end loop;
      
      return C = Parent;
   end Is_Parent_Ancestor;
   
   -------------------
   -- Append_Depend --
   -------------------
   
   procedure Append_Depend
     (This : access Cell_Record;
      Edge : access Cell_Record) is
   begin
      Cells_Lists.Append (This.Depends, Edge);
   end Append_Depend;
   
   -------------------
   -- Get_Max_With --
   -------------------
   
   function Get_Max_Width (This : access Cell_Record) return Coordinate is
   begin
      return This.Max_Width;
   end Get_Max_Width;
   
   ------------------
   -- Set_Max_Width --
   ------------------
   
   procedure Set_Max_Width
     (This  : access Cell_Record;
      Width : Coordinate) is
   begin
      This.Max_Width := Width;
   end Set_Max_Width;
   
   --------------------
   -- Get_Max_Height --
   --------------------
   
   function Get_Max_Height (This : access Cell_Record) return Coordinate is
   begin
      return This.Max_Height;
   end Get_Max_Height;
   
   --------------------
   -- Set_Max_Height --
   --------------------
   
   procedure Set_Max_Height
     (This   : access Cell_Record;
      Height : Coordinate) is
   begin
      This.Max_Height := Height;
   end Set_Max_Height;
   
   --------------------
   -- Get_Bound_Mark --
   --------------------
   
   function Get_Bound_Mark (This : access Cell_Record) return Boolean is
   begin
      return This.Bound_Mark;
   end Get_Bound_Mark;
   
   --------------------
   -- Set_Bound_Mark --
   --------------------
   
   procedure Set_Bound_Mark
     (This : access Cell_Record;
      Mark : Boolean) is
   begin
      This.Bound_Mark := Mark;
   end Set_Bound_Mark;
   
   -------------------------
   -- Get_Coordinate_Mark --
   -------------------------
   
   function Get_Coordinate_Mark (This : access Cell_Record) return Boolean is
   begin
      return This.Coordinate_Mark;
   end Get_Coordinate_Mark;
   
   -------------------------
   -- Set_Coordinate_Mark --
   -------------------------
   
   procedure Set_Coordinate_Mark
     (This : access Cell_Record;
      Mark : Boolean) is
   begin
      This.Coordinate_Mark := Mark;
   end Set_Coordinate_Mark;
   
   -------------------------
   -- Get_Associated_Node --
   -------------------------
   
   --     function Get_Associated_Node (This : access Cell_Record) return access Node_Record'Class is
   --     begin
   --        return This.Node;
   --     end Get_Associated_Node;
   
   -------------------------
   -- Set_Associated_Node --
   -------------------------
   
   --     procedure Set_Associated_Node
   --       (This : access Cell_Record; 
   --        N    : access Node_Record'Class) is
   --     begin
   --        This.Node := N;
   --     end Set_Associated_Node;
   
end Artics.Graph.Cells;
