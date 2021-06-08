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

with Ada.Text_IO; use Ada.Text_IO;

with Artics.Exceptions; use Artics.Exceptions;
with Artics.Utils; use Artics.Utils;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Artics.Graph.Models_Changes.Roots; 
use Artics.Graph.Models_Changes.Roots;

with Artics.Graph.Models_Changes.Childs; 
use Artics.Graph.Models_Changes.Childs;

with Artics.Graph.Models_Changes.Terminals; 
use Artics.Graph.Models_Changes.Terminals;

with Artics.Graph.Models_Changes.Values; 
use Artics.Graph.Models_Changes.Values;

with Artics.Graph.Models_Changes.Geometries; 
use Artics.Graph.Models_Changes.Geometries;

with Artics.Graph.Cells_Paths; use Artics.Graph.Cells_Paths;

with Artics.Graph.Events.Execute_Events; 
use Artics.Graph.Events.Execute_Events;

with Artics.Graph.Events.Begin_Update_Events;
use Artics.Graph.Events.Begin_Update_Events;
with Artics.Graph.Events.End_Update_Events;
use Artics.Graph.Events.End_Update_Events;
with Artics.Graph.Events.Before_Undo_Events;
use Artics.Graph.Events.Before_Undo_Events;
with Artics.Graph.Events.Undo_Events;
use Artics.Graph.Events.Undo_Events;

with Artics.Graph.Models_Interfaces; use Artics.Graph.Models_Interfaces;
--with Artics.Graph.Cells;


package body Artics.Graph.Models is

   use Cells_Lists_Helpers;
   
   ---------------------
   -- New_Graph_Model --
   ---------------------
   
   function New_Graph_Model return access Model_Record'Class is
   begin
      return New_Graph_Model (null);
   end New_Graph_Model;

   
   ---------------------
   -- New_Graph_Model --
   ---------------------
   
   function New_Graph_Model
     (Root : access Cell_Record'Class) return access Model_Record'Class  
   is
      M : Model_Ptr := new Model_Record'(No_Model_Record);
   begin
      M.Current_Edit := Create_Undoable_Edit (M);
      if Root /= null then
         M.Set_Root (Root);
      else
         M.Clear;
      end if;
      
      return M;
   end New_Graph_Model;

   
   -----------
   -- Clear --
   -----------
   
   procedure Clear (M : access Model_Record) 
   is
      Root : access Cell_Record;
   begin
      Root := M.Create_Root;
      M.Set_Root (Root);
   end Clear;
   
   
   ----------------------
   -- Get_Update_Level --
   ----------------------
   
   function Get_Update_Level (M : access Model_Record) return Integer 
   is
   begin
      return M.Update_Level;
   end Get_Update_Level;
   
   
   -----------------
   -- Create_Root --
   -----------------
   
   function Create_Root (M : access Model_Record) 
                         return access Cell_Record'Class 
   is 
      use Artics.Graph.Cells;
      
      Root : access Cell_Record'Class;
      Cell : access Cell_Record'Class;
   begin
      Cell := New_Cell;
      Cell.Set_Id ("child");

      Root := New_Cell;
      Root.Set_Id ("root");
      Root.Insert (Cell);
   
      return Root;
   end Create_Root;
   
   
   ---------------
   -- Get_Cells --
   ---------------
   
   function Get_Cells (M : access Model_Record) return Cells_Maps.Map 
   is
   begin
      return M.Cells;
   end Get_Cells;
   
   --------------
   -- Get_Cell --
   --------------
   
   function Get_Cell 
     (M  : access Model_Record;
      Id : String) return access Cell_Record'Class 
   is 
      use Cells_Maps;
      Result    : access Cell_Record'Class := null;
      Cell_Name : Name_Id := String_Find (Id);
   begin
      if M.Cells /= Cells_Maps.Empty_Map then
         if Cells_Maps.Contains (M.Cells, Cell_Name) then
            Result := Cells_Maps.Element (M.Cells, Cell_Name);         
         end if;
      end if;
      return Result;
   end Get_Cell;
   
   
   -----------------------------
   -- Is_Maintain_Edge_Parent --
   -----------------------------
   
   function Is_Maintain_Edge_Parent (M : access Model_Record) return Boolean 
   is
   begin
      return M.Maintain_Edge_Parent;
   end Is_Maintain_Edge_Parent;
   
   
   ------------------------------
   -- Set_Maintain_Edge_Parent --
   ------------------------------
   
   procedure Set_Maintain_Edge_Parent 
     (M                    : access Model_Record;
      Maintain_Edge_Parent : Boolean) 
   is
   begin
      M.Maintain_Edge_Parent := Maintain_Edge_Parent;
   end Set_Maintain_Edge_Parent;
   
   
   -------------------
   -- Is_Create_Ids --
   -------------------
   
   function Is_Create_Ids (M : access Model_Record) return Boolean 
   is
   begin
      return M.Create_Ids;
   end Is_Create_Ids;
   
   
   --------------------
   -- Set_Create_Ids --
   --------------------
   
   procedure Set_Create_Ids
     (M     : access Model_Record;
      Value : Boolean) is 
   begin
      M.Create_Ids := Value;
   end Set_Create_Ids;
   
   --------------
   -- Get_Root --
   --------------
   
   function Get_Root
     (M : access Model_Record) return access Cell_Record'Class is
   begin
      return M.Root;
   end Get_Root;
   
   
   --------------
   -- Set_Root --
   --------------
   
   procedure Set_Root (M    : access Model_Record;
                       Root : access Cell_Record'Class)      
   is      
      Model  : access Model_Interface'Class;
      Change : access Root_Change_Record;
   begin
      if M.Current_Edit = null then
         M.Current_Edit := M.Create_Undoable_Edit;
      end if;
      
      Model := M;
      Change := New_Root_Change (Model, Root);
      
      M.Execute (Change); -- 'Access);
   end Set_Root;
   
   
   ------------------
   -- Root_Changed --
   ------------------
   
   function Root_Changed (M    : access Model_Record;
                          Root : access Cell_Record'Class) 
                          return access Cell_Record'Class 
   is 
      Old_Root   : access Cell_Record'Class := M.Root;
   begin
      M.Root := Root;
      M.Next_Id := 0;
      M.Cells := Cells_Maps.Empty_Map;
      M.Cell_Added (Root);

      return Old_Root;
   end Root_Changed;
   
   
   --------------------------
   -- Create_Undoable_Edit --
   --------------------------
   
   function Create_Undoable_Edit (M : access Model_Record) 
                                  return access Undoable_Edit_Record'Class 
   is      
      U : Undoable_Edit_Ptr := New_Undoable_Edit (Object_Class_Ptr (M));
   begin
      return U;
   end Create_Undoable_Edit;
   
   
   -----------------
   -- Clone_Cells --
   -----------------
   
   function Clone_Cells
     (M                : access Model_Record;
      Cells            : Cells_Lists.List;
      Include_Children : Boolean) return Cells_Lists.List 
   is      
      use Cells_Lists;
      
      Mapping      : Cells_To_Cells_Maps.Map;
      Clones       : Cells_Lists.List;
      Clone_Cursor : Cells_Lists.Cursor;
      Cell_Cursor  : Cells_Lists.Cursor;
   begin
      for C of Cells loop
         Clones.Append (M.Clone_Cell (C, Mapping, Include_Children));
      end loop;
      
      Clone_Cursor := Clones.First;
      Cell_Cursor  := Cells.First;
      while Clone_Cursor /= Cells_Lists.No_Element 
        and Cell_Cursor  /= Cells_Lists.No_Element
      loop
         M.Restore_Clone
           (Element (Clone_Cursor), Element (Cell_Cursor), Mapping);
         Cells_Lists.Next (Clone_Cursor);
         Cells_Lists.Next (Cell_Cursor);
      end loop;
      
      return Clones;
   end Clone_Cells;
   
   
   ----------------
   -- Clone_Cell --
   ----------------
   
   function Clone_Cell
     (M                : access Model_Record;
      Cell             : access Cell_Record'Class;
      Mapping          : in out Cells_To_Cells_Maps.Map;
      Include_Children : Boolean) 
      return access Cell_Record'Class 
   is 
      use Artics.Graph.Cells;
      use Cells_Lists;
      
      Mxc    : access Cell_Record'Class;
   begin
      if Cell /= null then
         Mxc := Cell.Clone (Include_Children);
         Mapping.Insert (Cell, Mxc);
         return Mxc;
      end if;
     
      return null;
   end Clone_Cell;
   
   
   -------------------
   -- Restore_Clone --
   -------------------
   
   procedure Restore_Clone
     (M       : access Model_Record;
      Clone   : access Cell_Record'Class;
      Cell    : access Cell_Record'Class;
      Mapping : in out Cells_To_Cells_Maps.Map) 
   is 
      use Cells_Lists;
      
      Mxc          : access Cell_Record'Class;
      Source       : access Cell_Record'Class;
      Target       : access Cell_Record'Class;
      Tmp          : access Cell_Record'Class;
      Clone_Cursor : Cells_Lists.Cursor;
      Cell_Cursor  : Cells_Lists.Cursor;
      Clone_Childs : Cells_Lists.List;
      Cell_Childs  : Cells_Lists.List;
   begin
      if Clone /= null then
         Mxc := Clone;
         Source := M.Get_Terminal (Cell, True);
	 
         if Source /= null then
            Tmp := Mapping.Element (Source);
            if Tmp /= null then
               Tmp.Insert_Edge (Mxc, True);
            end if;
         end if;
	 
         Target := M.Get_Terminal (Cell, False);
         if Target /= null then
            Tmp := Mapping.Element (Target);
            if Tmp /= null then
               Tmp.Insert_Edge (Mxc, False);
            end if;
         end if;
         Clone_Childs := Clone.Get_Children_List;
         Clone_Cursor := Clone_Childs.First;
         Clone_Childs := Cell.Get_Children_List;
         Cell_Cursor  := Cell_Childs.First;
         while Clone_Cursor /= Cells_Lists.No_Element 
           and Cell_Cursor  /= Cells_Lists.No_Element
         loop
            M.Restore_Clone
              (Element (Clone_Cursor), Element (Cell_Cursor), Mapping);
	    
            Cells_Lists.Next (Clone_Cursor);
            Cells_Lists.Next (Cell_Cursor);
         end loop;
      end if;
   end Restore_Clone;
   
   -----------------
   -- Is_Ancestor --
   -----------------
   
   function Is_Ancestor
     (M      : access Model_Record;
      Parent : access Cell_Record'Class;
      Child  : access Cell_Record'Class) return Boolean 
   is 
      C : access Cell_Record'Class := Child;
   begin
      while C /= null and then C /= Parent loop
         C := Get_Parent (C);
      end loop;
      return C = Parent;
   end Is_Ancestor;
   
   
   -------------
   -- Contains--
   -------------
   
   function Contains (M    : access Model_Record;
                      Cell : access Cell_Record'Class) return Boolean      
   is
      Root : access Cell_Record'Class;
   begin
      Root := M.Get_Root;
      return Is_Ancestor (M, M.Get_Root, Cell);
   end Contains;
   
   
   ----------------
   -- Get_Parent --
   ----------------
   
   function Get_Parent
     (M     : access Model_Record;
      Child : access Cell_Record'Class) return access Cell_Record'Class 
   is
   begin
      if Child /= null then
         return Child.Get_Parent;
      else
         return null;
      end if;
   end Get_Parent;

   --------------------
   -- Get_Top_Parent --
   --------------------
   
   function Get_Top_Parent
     (M     : access Model_Record;
      Child : access Cell_Record'Class) return access Cell_Record'Class 
   is
      Cell : access Cell_Record'Class;
      Root : access Cell_Record'Class;
   begin
      Cell := Child;
      if Cell /= null then
         Root := Cell.Get_Parent;
	 
         while Cell /= null and then Cell.Get_Parent /= Root loop
            Cell := Cell.Get_Parent;
         end loop;
      end if;
      
      return Cell;
   end Get_Top_Parent;
   
   ---------
   -- Add --
   ---------
   
   procedure Add
     (M      : access Model_Record;
      Parent : access Cell_Record'Class;
      Child  : access Cell_Record'Class;
      Index  : Integer) 
   is      
      Parent_Changed : Boolean;
      Change         : access Child_Change_Record;
   begin
      if Child /= Parent and Parent /= null and Child /= null then
         Parent_Changed := Get_Parent (Child) /= Parent;
         Change := New_Child_Change (M, Parent, Child, Index);
         M.Execute (Change);
	 
         if M.Maintain_Edge_Parent and Parent_Changed then
            Update_Edge_Parents (M, Child);
         end if;
      end if;
   end Add;
   
   ----------------
   -- Cell_Added --
   ----------------
   
   procedure Cell_Added
     (M    : access Model_Record;
      Cell : access Cell_Record'Class) 
   is 
      Collision  : access Cell_Record'Class;
      Child_List : Cells_Lists.List := Cells_Lists.Empty_List;
      
      use Cells_Lists;
      
   begin
      if Cell /= null then
         if Is_Create_Ids (M) then
            if Get_Id (Cell) = "" then
               Set_Id (Cell, Create_Id (M));
            end if;
         end if;
	 
         if Cell.Get_Id /= "" then
            Collision := Get_Cell (M, Get_Id (Cell));
            if Collision /= Cell then
               declare
                  Old_Id : String := Get_Id (Cell);
               begin
                  while Collision /= null loop
                     Set_Id (Cell, Old_Id & "_" & Create_Id (M));
                     Collision := Get_Cell (M, Get_Id (Cell));
                  end loop;
               end;
            end if;
	    
            M.Cells.Insert (String_Find (Cell.Get_Id), Cell);
         end if;
	 
         Child_List := Cell.Get_Children_List;

         if Child_List /= Cells_Lists.Empty_List then
            for Child of Child_List loop
               M.Cell_Added (Child);
            end loop;
         end if;
      end if;
   end Cell_Added;
   
   ---------------
   -- Create_Id --
   ---------------
   
   function Create_Id (M : access Model_Record) return String is
   begin
      M.Next_Id := M.Next_Id + 1;
      return "rx-" & Integer_To_String (M.Next_Id);
   end Create_Id;
   
   ------------
   -- Remove --
   ------------
   
   procedure Remove
     (M    : access Model_Record;
      Cell : access Cell_Record'Class) 
   is 
      Change : access Child_Change_Record;
   begin
      if Cell = M.Get_Root then
         M.Set_Root (null);
	 
      elsif Get_Parent (M, Cell) /= null then
         Change := New_Child_Change (M, null, Cell);
         Change.Execute;
      end if;
   end Remove;
   
   ------------------
   -- Cell_Removed --
   ------------------
   
   procedure Cell_Removed
     (M    : access Model_Record;
      Cell : access Cell_Record'Class) 
   is 
      Child_List : Cells_Lists.List;
      Id         : Name_Id;
      use Cells_Lists;
   begin
      if Cell /= null then
         
         Child_List := Cell.Get_Children_List;
         if not Cells_Lists.Is_Empty (Child_List) then
            for C of Child_List loop               
               M.Cell_Removed (C);               
            end loop;
         end if;

         if Get_Id (Cell) /= "" then
            Id := String_Find (Get_Id (Cell));
            if Cells_Maps.Contains (M.Cells, Id) then
               Cells_Maps.Delete (M.Cells, Id);
            end if;
         end if;
      end if;
   end Cell_Removed;
   
   -----------------------------
   -- Parent_For_Cell_Changed --
   -----------------------------
   
   function Parent_For_Cell_Changed
     (M      : access Model_Record;
      Cell   : access Cell_Record'Class;
      Parent : access Cell_Record'Class;
      Index  : Integer) return access Cell_Record'Class 
   is 
      use Cells_Lists;
      
      Previous  : access Cell_Record'Class;
      Old_Index : Integer;
   begin
      Previous := M.Get_Parent (Cell);
      
      if Parent /= null then
         if Parent /= Previous 
           or else Previous.Get_Index (Cell) /= Index 
         then 
            Parent.Insert (Cell, Index);
         end if;
	 
      elsif Previous /= null then
         Old_Index := Previous.Get_Index (Cell);
         Previous.Remove (Old_Index);
      end if;
      
      -- Checks if the previous parent was already in the model and avoids
      -- calling cellAdded if it was.
      
      if not M.Contains (Previous) and Parent /= null then 
         M.Cell_Added (Cell);
         
      elsif Parent = null then
         M.Cell_Removed (Cell);
      end if;
      
      return Previous;
   end Parent_For_Cell_Changed;
   
   
   -----------------------
   -- Get_Children_List --
   -----------------------
   
   function Get_Children_List
     (M    : access Model_Record;
      Cell : access Cell_Record'Class) return Cells_Lists.List is
   begin
      return Cell.Get_Children_List;
   end Get_Children_List;
   
   
   ---------------------
   -- Get_Child_Count --
   ---------------------
   
   function Get_Child_Count
     (M      : access Model_Record;
      Cell   : access Cell_Record'Class) return Integer is
   begin
      return Cell.Get_Child_Count;
   end Get_Child_Count;
   
   
   ------------------
   -- Get_Child_At --
   ------------------
   
   function Get_Child_At
     (M      : access Model_Record;
      Parent : access Cell_Record'Class;
      Index  : Integer) return access Cell_Record'Class is
   begin
      return Parent.Get_Child_At (Index);
   end Get_Child_At;
   
   
   ------------------
   -- Get_Terminal --
   ------------------
   
   function Get_Terminal
     (M         : access Model_Record;
      Edge      : access Cell_Record'Class;
      Is_Source : Boolean) return access Cell_Record'Class is
      use Artics.Graph.Cells;
      
      --E : access Cell_Record := Cell_Ptr (Edge);
   begin
      return Edge.Get_Terminal (Is_Source);
   end Get_Terminal;
   
   
   ------------------
   -- Set_Terminal --
   ------------------
   
   procedure Set_Terminal
     (M         : access Model_Record;
      Edge      : access Cell_Record'Class;
      Terminal  : access Cell_Record'Class;
      Is_Source : Boolean) 
   is      
      Terminal_Changed : Boolean;
      Change           : Terminal_Change_Ptr;
   begin
      Terminal_Changed := Terminal /= Get_Terminal (M, Edge, Is_Source);
      Change := New_Terminal_Change (M, Edge, Terminal, Is_Source);
      Change.Execute;
      
      if M.Maintain_Edge_Parent and Terminal_Changed then                     
         M.Update_Edge_Parent (Edge, M.Get_Root);                     
      end if;
   end Set_Terminal;
   
   
   -------------------------------
   -- Terminal_For_Cell_Changed --
   -------------------------------
   
   procedure Terminal_For_Cell_Changed
     (M         : access Model_Record;
      Edge      : access Cell_Record'Class;
      Terminal  : access Cell_Record'Class;
      Is_Source : Boolean) 
   is 
      Previous  : access Cell_Record'Class;
   begin
      Previous := M.Get_Terminal (Edge, Is_Source);
      
      if Terminal /= null then
         Terminal.Insert_Edge (Edge, Is_Source);
	 
      elsif Previous /= null then
         Previous.Remove_Edge (Edge, Is_Source);
      end if;
   end Terminal_For_Cell_Changed;
   
   -------------------------
   -- Update_Edge_Parents --
   -------------------------
   
   procedure Update_Edge_Parents
     (M    : access Model_Record;
      Cell : access Cell_Record'Class) is
   begin
      M.Update_Edge_Parents (Cell, M.Get_Root);
   end Update_Edge_Parents;
   
   -------------------------
   -- Update_Edge_Parents --
   -------------------------
   
   procedure Update_Edge_Parents
     (M    : access Model_Record;
      Cell : access Cell_Record'Class;
      Root : access Cell_Record'Class) is
      
      Childs    : Cells_Lists.List;
      Edges     : Cells_Lists.List;
      New_Edges : Cells_Lists.List;
   begin
      -- Updates edges on children first
      
      Childs := Cell.Get_Children_List;
      for Child of Childs loop
         M.Update_Edge_Parents (Child, Root);
      end loop;
      
      -- Updates the parents of all connected edges
      Edges := Cell.Get_Edges_List;
      New_Edges := Cells_Lists.Empty_List;
      for Edge of Edges loop
         New_Edges.Append (Edge);
      end loop;
      
      for Edge of New_Edges loop
         -- Updates edge parent if edge and child have a common root node
         -- (does not need to be the model root node)
	 
         if M.Is_Ancestor (Root, Edge) then
            M.Update_Edge_Parent (Edge, Root);
         end if;
      end loop;
   end Update_Edge_Parents;
   
   ------------------------
   -- Update_Edge_Parent --
   ------------------------
   
   procedure Update_Edge_Parent
     (M    : access Model_Record;
      Edge : access Cell_Record'Class;
      Root : access Cell_Record'Class) 
   is      
      Source  : access Cell_Record'Class;
      Target  : access Cell_Record'Class;
      Cell    : access Cell_Record'Class := null;
      Geo     : Cell_Geometry_Ptr;
      Origin1 : Point_Record;
      Origin2 : Point_Record;
      Dx      : Coordinate;
      Dy      : Coordinate;
   begin
      Source := M.Get_Terminal (Edge, True);
      Target := M.Get_Terminal (Edge, False);
		
      -- Uses the first non-relative descendants of the source terminal
      
      while Source /= null and then not M.Is_Edge (Source) 
        and then M.Get_Geometry (Source) /= null 
        and then M.Get_Geometry (Source).Is_Relative
      loop
         Source := Get_Parent (Source);
      end loop;
		
      -- Uses the first non-relative descendants of the target terminal
      
      while Target /= null and then not M.Is_Edge (Target) 
        and then M.Get_Geometry (Target) /= null 
        and then M.Get_Geometry (Target).Is_Relative
      loop
         Target := M.Get_Parent (Target);
      end loop;
      
      if M.Is_Ancestor (Root, Source) and M.Is_Ancestor (Root, Target) then
         if Source = Target then
            Cell := M.Get_Parent (Source);
         else
            Cell := M.Get_Nearest_Common_Ancestor (Source, Target);
         end if;
         
         -- Keeps the edge in the same layer
         
         if Cell /= null 
           and then (M.Get_Parent (Cell) /= M.Root
                     or else M.Is_Ancestor (Cell, Edge))
           and then M.Get_Parent (Edge) /= Cell 
         then
            Geo := M.Get_Geometry (Edge);
            
            if Geo /= null then
               Origin1 := M.Get_Origin (M.Get_Parent (Edge));
               Origin2 := M.Get_Origin (Cell);
               
               Dx := Get_X (Origin2) - Get_X (Origin1);
               Dy := Get_Y (Origin2) - Get_Y (Origin1);
               
               Geo.Translate (-Dx, -Dy);
               M.Set_Geometry (Edge, Geo);
            end if;
	    
            M.Add (Cell, Edge, List_Length (Cell.Get_Children_List)  + 1);
         end if;
      end if;
   end Update_Edge_Parent;
   
   ----------------
   -- Get_Origin --
   ----------------
   
   function Get_Origin
     (M    : access Model_Record;
      Cell : access Cell_Record'Class) return Point_Record 
   is
      
      Result : Point_Record := No_Point_Record;
      Geo    : Cell_Geometry_Ptr;
   begin
      if Cell /= null then
         Result := M.Get_Origin (M.Get_Parent (Cell));

         if not M.Is_Edge (Cell) then
            Geo := M.Get_Geometry (Cell);

            if Geo /= null then
               Set_X
                 (Result, Get_X (Result) + Get_X (Geometry_Rectangle (Geo)));
               Set_Y
                 (Result, Get_Y (Result) + Get_Y (Geometry_Rectangle (Geo)));
            end if;
         end if;
      else
         Result := Point_Record'(0.0, 0.0);
      end if;

      return Result;
   end Get_Origin;
   
   
   ---------------------------------
   -- Get_Nearest_Common_Ancestor --
   ---------------------------------
   
   function Get_Nearest_Common_Ancestor
     (M     : access Model_Record;
      Cell1 : access Cell_Record'Class;
      Cell2 : access Cell_Record'Class) return access Cell_Record'Class 
   is      
      Path    : Unbounded_String;
      Current : Unbounded_String;
      Cell    : access Cell_Record'Class;
      Parent  : access Cell_Record'Class;
      Pos     : Natural;
   begin
      if Cell1 /= null and Cell2 /= null then
	 
         Path := To_Unbounded_String (Create (Cell2));
	 
         -- Creates the cell path for the second cell
	 
         if To_String (Path) /= "" then
	    
            -- Bubbles through the ancestors of the first
            -- cell to find the nearest common ancestor.
	    
            Cell := Cell1;
	    
            Current := To_Unbounded_String (Create (Cell));
            while Cell /= null loop
               Parent := Get_Parent (Cell);
	       
               -- Checks if the cell path is equal to the beginning
               -- of the given cell path
	       
               Pos := Index
                 (Source  => Path,
                  Pattern => To_String (Current) & Path_Separator);
	       
               if Pos = 1 and Parent /= null then
                  return Cell;
               end if;
	       
               Current := To_Unbounded_String 
                 (Get_Parent_Path (To_String (Current)));
	       
               Cell := Parent;
            end loop;
         end if;
      end if;
      
      return null;
   end Get_Nearest_Common_Ancestor;
   
   
   --------------------
   -- Get_Edges_List --
   --------------------
   
   function Get_Edges_List
     (M    : access Model_Record;
      Cell : access Cell_Record'Class) return Cells_Lists.List is
   begin
      return Cell.Get_Edges_List;
   end Get_Edges_List;
   
   --------------------
   -- Get_Edge_Count --
   --------------------
   
   function Get_Edge_Count
     (M    : access Model_Record;
      Cell : access Cell_Record'Class) return Integer is
   begin
      if Cell /= null then
         return Cell.Get_Edge_Count;
      else
         return 0;
      end if;
   end Get_Edge_Count;
   
   -----------------
   -- Get_Edge_At --
   -----------------
   
   function Get_Edge_At
     (M      : access Model_Record;
      Parent : access Cell_Record'Class;
      Index  : Integer) return access Cell_Record'Class is
   begin
      if Parent /= null then
         return Parent.Get_Edge_At (Index);
      else
         return null;
      end if;
   end Get_Edge_At;
   
   ---------------
   -- Is_Vertex --
   ---------------
   
   function Is_Vertex
     (M    : access Model_Record;
      Cell : access Cell_Record'Class) return Boolean is
   begin
      if Cell /= null then
         return Cell.Is_Vertex;
      else
         return False;
      end if;
   end Is_Vertex;
   
   -------------
   -- Is_Edge --
   -------------
   
   function Is_Edge
     (M    : access Model_Record;
      Cell : access Cell_Record'Class) return Boolean is
   begin
      if Cell /= null then
         return Cell.Is_Edge;
      else
         return False;
      end if;
   end Is_Edge;
   
   ---------------
   -- Get_Value --
   ---------------
   
   function Get_Value
     (M    : access Model_Record;
      Cell : access Cell_Record'Class) return access Object_Record'Class is
   begin
      if Cell /= null then
         return Cell.Get_Value;
      else
         return null;
      end if;
   end Get_Value;
   
   ---------------
   -- Set_Value --
   ---------------
   
   procedure Set_Value
     (M     : access Model_Record;
      Cell  : access Cell_Record'Class;
      Value : access Object_Record'Class) 
   is
      Val_Change : Value_Change_Ptr;
   begin
      if M.Get_Value (Cell) /= Value then
         Val_Change := New_Value_Change
           (Model => M,
            Cell  => Cell,
            Value => Value);
         Val_Change.Execute;
      end if;
   end Set_Value;
      
   
   ----------------------------
   -- Value_For_Cell_Changed --
   ----------------------------
  
 
   function Value_For_Cell_Changed
     (M     : access Model_Record;
      Cell  : access Cell_Record'Class;
      Value : access Object_Record'Class) return access Object_Record'Class 
   is 
      Old_Value : Object_Ptr;
   begin
      Old_Value := Cell.Get_Value;      
      Cell.Set_Value (Value);      

      return Old_Value;
   end Value_For_Cell_Changed;
   
   
   ------------------
   -- Get_Geometry --
   ------------------
   
   function Get_Geometry
     (M     : access Model_Record;
      Cell  : access Cell_Record'Class) 
      return access Cell_Geometry_Record'Class is
   begin
      if Cell /= null then
         return Cell.Get_Geometry;
      else
         return null;
      end if;
   end Get_Geometry;
   
   
   ------------------
   -- Set_Geometry --
   ------------------
   
   procedure Set_Geometry
     (M        : access Model_Record;
      Cell     : access Cell_Record'Class;
      Geometry : access Cell_Geometry_Record'Class) 
   is
      Geom_Change : Geometry_Change_Ptr;
   begin
      if Geometry /= M.Get_Geometry (Cell) then
         Geom_Change := New_Geometry_Change
           (Model    => M,           
            Cell     => Cell,
            Geometry => Cell_Geometry_Ptr (Geometry));
         
         Geom_Change.Execute;
      end if;
   end Set_Geometry;
   
   -------------------------------
   -- Geometry_For_Cell_Changed --
   -------------------------------
   
   function Geometry_For_Cell_Changed
     (M        : access Model_Record;
      Cell     : access Cell_Record'Class;
      Geometry : access Cell_Geometry_Record'Class)
      return access Cell_Geometry_Record'Class is
      
      Previous : access Cell_Geometry_Record'Class;
   begin
      Previous := M.Get_Geometry(Cell);
      Cell.Set_Geometry (Geometry);

      return Previous;
   end Geometry_For_Cell_Changed;

   -------------
   -- Execute --
   -------------
   
   procedure Execute 
     (M      : access Model_Record;
      Change : access Model_Change_Record'Class) is
   begin
      Change.Execute;
      
      M.Begin_Update;
      M.Current_Edit.Add (Change);
      
      declare
         Evt : access Execute_Event_Record := New_Execute_Event (Change);
      begin
         Fire_Event (M, Evt);
         Free_Execute_Event (Execute_Event_Ptr (Evt));
         --  	 new mxEventObject(mxEvent.EXECUTE, "change", change));
      end;
      M.End_Update;
      
   end Execute;
   
   ------------------
   -- Begin_Update --
   ------------------
   
   procedure Begin_Update (M : access Model_Record) is
   begin
      M.Update_Level := M.Update_Level + 1;
      declare
         Evt : access Begin_Update_Event_Record := New_Begin_Update_Event;
      begin
         Fire_Event (M, Evt);
         Free_Begin_Update_Event (Begin_Update_Event_Ptr (Evt));
         --fireEvent(new mxEventObject(mxEvent.BEGIN_UPDATE));
      end;
   end Begin_Update;
   
   ----------------
   -- End_Update --
   ----------------
   
   procedure End_Update (M : access Model_Record) 
   is      
      Tmp : access Undoable_Edit_Record'Class; -- Ptr;
   begin
      M.Update_Level := M.Update_Level - 1;
      
      if not M.Ending_Update then
         M.Ending_Update := M.Update_Level = 0;
	 
         declare
            Evt : access End_Update_Event_Record := 
              New_End_Update_Event (M.Current_Edit);
         begin
            Fire_Event (M, Evt);
            Free_End_Update_Event (End_Update_Event_Ptr (Evt));
            --fireEvent(new mxEventObject(mxEvent.END_UPDATE, "edit",
            --currentEdit));
         end;
	 
         begin
            if M.Ending_Update and not M.Current_Edit.Is_Empty then
               declare
                  Evt : access Before_Undo_Event_Record := 
                    New_Before_Undo_Event (M.Current_Edit);
               begin
                  Fire_Event (M, Evt);
                  Free_Before_Undo_Event (Before_Undo_Event_Ptr (Evt));
                  --fireEvent(new mxEventObject(mxEvent.BEFORE_UNDO, "edit",
                  --currentEdit));
               end;
	       
	       
               Tmp := M.Current_Edit;
               M.Current_Edit := M.Create_Undoable_Edit;
               Tmp.Dispatch;
	       
               declare
                  Evt : access Undo_Event_Record := New_Undo_Event (Tmp);
               begin
                  Fire_Event (M, Evt);
                  Free_Undo_Event (Undo_Event_Ptr (Evt));
                  ---fireEvent(new mxEventObject(mxEvent.UNDO, "edit", tmp));
               end;
	       
            end if;
	    
         exception
            when E : others =>
               Log_Exception 
                 (Exception_Message (E, "Exception: Models.End_Update"));
         end;
	 
         M.Ending_Update := False;
      end if;
   end End_Update;
   
   
   --------------------
   -- Merge_Children --
   --------------------
   
   procedure Merge_Children
     (M               : access Model_Record;
      From            : access Cell_Record'Class;
      To              : access Cell_Record'Class;
      Clone_All_Edges : Boolean) 
   is     
      use Cells_To_Cells_Maps;
      
      Mapping  : Cells_To_Cells_Maps.Map;
      Cursor   : Cells_To_Cells_Maps.Cursor;
      Cell     : access Cell_Record'Class;
      Edge     : access Cell_Record'Class;
      Terminal : access Cell_Record'Class;
   begin
      M.Begin_Update;
      
      begin
         M.Merge_Children_Impl (From, To, Clone_All_Edges, Mapping);
      
         -- Post-processes all edges in the mapping and reconnects the terminals
         -- to the corresponding cells in the target model
      
         Cursor := Cells_To_Cells_Maps.First (Mapping);
         while Cursor /= Cells_To_Cells_Maps.No_Element loop
            Edge := Cells_To_Cells_Maps.Key (Cursor);
            Cell := Cells_To_Cells_Maps.Element (Cursor);
	 
            if Terminal /= null then
               Terminal := M.Get_Terminal (Edge, True);
               M.Set_Terminal (Cell, Terminal, True);
            end if;
	 
            Terminal := M.Get_Terminal (Edge, False);
	 
            if Terminal /= null then
               Terminal := Cells_To_Cells_Maps.Element (Mapping, Terminal);
               M.Set_Terminal (Cell, Terminal, False);
            end if;
	 
            Cells_To_Cells_Maps.Next (Cursor);	 
         end loop;

      exception
         when E : others =>
            Log_Exception 
              (Exception_Message (E, "Models.Merge_Children"));
      end;
      
      M.End_Update;
   end Merge_Children;
   
   
   -------------------------
   -- Merge_Children_Impl --
   -------------------------
   
   procedure Merge_Children_Impl
     (M               : access Model_Record;
      From            : access Cell_Record'Class;
      To              : access Cell_Record'Class;
      Clone_All_Edges : Boolean;
      Mapping         : in out Cells_To_Cells_Maps.Map) 
   is 
      use Cells_Lists;
      use Cells_To_Cells_Maps;
      
      Childs : Cells_Lists.List;
   begin
      
      M.Begin_Update;
      
      Childs := From.Get_Children_List;
      for Cell of Childs loop
         declare
            Id     : String := Get_Id (Cell);
            Target : access Cell_Record'Class;
            Clone  : access Cell_Record'Class;
         begin
            if Id /= "" and (not M.Is_Edge (Cell) or not Clone_All_Edges) then
               Target := M.Get_Cell (Id);
            else
               Target := null;
            end if;
	    
            -- Clones and adds the child if no cell exists for the id
            if Target = null then
               Clone := Cell.Clone;
               Clone.Set_Id (Id);
	       
               -- Do *NOT* use model.add as this will move the edge away from
               -- the parent in updateEdgeParent if maintainEdgeParent is
               -- enabled in the target model
	       
               To.Insert (Clone);
               Target := Clone;
               M.Cell_Added (Target);
            end if;

            -- Stores the mapping for later reconnecting edges 
	    
            Mapping.Insert (Cell, Target);

            -- Recurses
            M.Merge_Children_Impl (Cell, Target, Clone_All_Edges, Mapping);
	    
         exception
            when E : others =>
               Log_Exception 
                 (Exception_Message (E, "Models.Merge_Children_Impl"));
         end;	    
      end loop;
      
      M.End_Update;
   end Merge_Children_Impl;
   
   
   -----------------
   -- Read_Object --
   -----------------
   
   procedure Read_Object (M : access Model_Record) is
   begin
      --Ois.Defaultreadobject();
      --currentEdit = createUndoableEdit();
      null;
   end Read_Object;
   
end Artics.Graph.Models;
