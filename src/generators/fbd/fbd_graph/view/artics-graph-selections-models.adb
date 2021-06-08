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

with Ada.Unchecked_Deallocation;

with Artics.Objects; use Artics.Objects;
with Artics.Graph.Selections.Changes; use Artics.Graph.Selections.Changes;
with Artics.Graph.Undoables.Edits; use Artics.Graph.Undoables.Edits;
with Artics.Graph.Events.Undo_Events; use Artics.Graph.Events.Undo_Events;

package body Artics.Graph.Selections.Models is
   
   -------------------------
   -- New_Selection_Model --
   -------------------------
   
   function New_Selection_Model return Selection_Model_Ptr is
   begin
      return new Selection_Model_Record'(No_Selection_Model_Record);
   end New_Selection_Model;
   
   -------------------------
   -- New_Selection_Model --
   -------------------------
   
   function New_Selection_Model
     (Graph : access Graph_Interface'Class) 
     return Selection_Model_Ptr is
      
      This : Selection_Model_Ptr := New_Selection_Model;
   begin
      This.Graph := Graph;
      return This;
   end New_Selection_Model;
   
   --------------------------
   -- Free_Selection_Model --
   --------------------------
   
   procedure Free_Selection_Model (This : in out Selection_Model_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Selection_Model_Record, Selection_Model_Ptr);
   begin
      Cells_Lists.Clear (This.Cells);
      Free (This);
   end Free_Selection_Model;
   
   -------------------------
   -- Is_Single_Selection --
   -------------------------
   
   function Is_Single_Selection
     (This : access Selection_Model_Record) return Boolean is
   begin
      return This.Single_Selection;
   end Is_Single_Selection;
   
   --------------------------
   -- Set_Single_Selection --
   --------------------------
   
   procedure Set_Single_Selection
     (This             : access Selection_Model_Record;
      Single_Selection : Boolean) is
   begin
      This.Single_Selection := Single_Selection;
   end Set_Single_Selection;
   
   -----------------
   -- Is_Selected --
   -----------------
   
   function Is_Selected
     (This : access Selection_Model_Record;
      Cell : access Cell_Record'Class) return Boolean is
   begin
      if Cell /= null then
	 return Cells_Lists.Contains (This.Cells, Cell);
      else
	 return False;
      end if;
   end Is_Selected;
   
   --------------
   -- Is_Empty --
   --------------
   
   function Is_Empty (This : access Selection_Model_Record) return Boolean is
   begin
      return Cells_Lists.Is_Empty (This.Cells);
   end Is_Empty;
   
   ----------
   -- Size --
   ----------
   
   function Size (This : access Selection_Model_Record) return Integer is
   begin
      return Integer (Cells_Lists.Length (This.Cells));
   end Size;
   
   -----------
   -- Clear --
   -----------
   
   procedure Clear (This : access Selection_Model_Record) is
   begin
      Change_Selection (This, Cells_Lists.Empty_List, This.Cells);
   end Clear;
   
   --------------
   -- Get_Cell --
   --------------
   
   function Get_Cell 
     (This : access Selection_Model_Record) return access Cell_Record'Class is
      
      use Cells_Lists;
      
      Cur : Cells_Lists.Cursor;
   begin
      Cur := Cells_Lists.First (This.Cells);
      if Cur /= Cells_Lists.No_Element then
	 return Cells_Lists.Element (Cur);
      else
	 return null;
      end if;
   end Get_Cell;
   
   ---------------
   -- Get_Cells --
   ---------------
   
   function Get_Cells
     (This : access Selection_Model_Record) return Cells_Lists.List is
   begin
      return This.Cells;
   end Get_Cells;
   
   --------------
   -- Set_Cell --
   --------------
   
   procedure Set_Cell
     (This : access Selection_Model_Record;
      Cell : access Cell_Record'Class) is
      
      Cells : Cells_Lists.List;
   begin
      if Cell /= null then
	 Cells_Lists.Append (Cells, Cell);
	 This.Set_Cells (Cells);
      else
	 This.Clear;
      end if;
   end Set_Cell;
   
   ---------------
   -- Set_Cells --
   ---------------
   
   procedure Set_Cells
     (This  : access Selection_Model_Record;
      Cells : Cells_Lists.List) is
      
      use Cells_Lists;
      
      Local_Cells : Cells_Lists.List := Cells;
      Tmp         : Cells_Lists.List;
      Cell        : access Cell_Record'Class := null;
   begin
      if not Cells_Lists.Is_Empty (Cells) then
	 
	 if This.Single_Selection then
	    Cell := Get_First_Selectable_Cell (This, Cells);
	    Cells_Lists.Append (Local_Cells, Cell);
	 else
	    Local_Cells := Cells;
	 end if;
	 
  	 for C of Local_Cells loop
  	    if This.Graph.Is_Cell_Selectable (C) then
  	      Cells_Lists.Append (Tmp, C);
 	    end if;
  	 end loop;
	 
	 This.Change_Selection (Tmp, This.Cells);
      	 
      else
	 This.Clear;
      end if;
   end Set_Cells;
   
   -------------------------------
   -- Get_First_Selectable_Cell --
   -------------------------------
   
   function Get_First_Selectable_Cell
     (This  : access Selection_Model_Record;
      Cells : Cells_Lists.List) return access Cell_Record'Class is
      
      use Cells_Lists;
   begin
      if Cells /= Cells_Lists.Empty_List then
	 for Cell of Cells loop
	    if This.Graph.Is_Cell_Selectable (Cell) then
	       return Cell;
	    end if;
	 end loop;
      end if;
      
      return null;
   end Get_First_Selectable_Cell;
   
   --------------
   -- Add_Cell --
   --------------
   
   procedure Add_Cell 
     (This : access Selection_Model_Record;
      Cell : access Cell_Record'Class) is
      
      Tmp : Cells_Lists.List;
   begin
      if Cell /= null then
	 Cells_Lists.Append (Tmp, Cell);
	 Add_Cells (This, Tmp);
      end if;
   end Add_Cell;
   
   ---------------
   -- Add_Cells --
   ---------------
   
   procedure Add_Cells
     (This  : access Selection_Model_Record;
      Cells : Cells_Lists.List) is
      
      use Cells_Lists;
      
      Local_Cells : Cells_Lists.List := Cells;
      Remove      : Cells_Lists.List;
      Cell        : access Cell_Record'Class;
      Tmp         : Cells_Lists.List;
   begin
      if Cells /= Cells_Lists.Empty_List then
	 
	 if This.Single_Selection then
	    Remove := This.Cells;
	    Cell        := This.Get_First_Selectable_Cell (Local_Cells);
	    Local_Cells := Cells_Lists.Empty_List;
	    Cells_Lists.Append (Local_Cells, Cell);
	 end if;
	 
	 for C of Local_Cells loop
	    if not This.Is_Selected (C) and This.Graph.Is_Cell_Selectable (C) 
	    then
	       Cells_Lists.Append (Tmp, C);
	    end if;
	 end loop;
	 
	 This.Change_Selection (Tmp, Remove);
      end if;
   end Add_Cells;
   
   -----------------
   -- Remove_Cell --
   -----------------
   
   procedure Remove_Cell
     (This : access Selection_Model_Record;
      Cell : access Cell_Record'Class) is
      
      use Cells_Lists;
      Tmp : Cells_Lists.List;
   begin
      if Cell /= null then
	 Cells_Lists.Append (Tmp, Cell);
	 Remove_Cells (This, Tmp);
      end If;
   end Remove_Cell;
   
   ------------------
   -- Remove_Cells --
   ------------------
   
   procedure Remove_Cells
     (This     : access Selection_Model_Record;
      Cells : Cells_Lists.List) is
      
      use Cells_Lists;
      Tmp : Cells_Lists.List;
   begin
      if This.Cells /= Cells_Lists.Empty_List then
	 for C of Cells loop
	    if Is_Selected (This, C) then
	       Cells_Lists.Append (Tmp, C);
	    end if;
	 end loop;
	 
	 This.Change_Selection (Cells_Lists.Empty_List, Tmp);
      end if;
   end Remove_Cells;
   
   ----------------------
   -- Change_Selection --
   ----------------------
   
   procedure Change_Selection
     (This    : access Selection_Model_Record;
      Added   : Cells_Lists.List;
      Removed : Cells_Lists.List) is
      
      use Cells_Lists;
      
      Change : access Selection_Change_Record;
      Edit   : access Undoable_Edit_Record;
   begin
      if not Cells_Lists.Is_Empty (Added) 
  	or not Cells_Lists.Is_Empty (Removed)
      then
  	 Change := New_Selection_Change (This, Added, Removed);
  	 Change.Execute;
  	 Edit := New_Undoable_Edit (Object_Ptr (This), False);
  	 Edit.Add (Change);
  	 
	 declare
	    use Artics.Graph.Events.Undo_Events;
	    Evt : access Undo_Event_Record := New_Undo_Event (Edit);
	 begin
	    Fire_Event (This, Evt);
	    Free_Undo_Event (Undo_Event_Ptr (Evt));
	 end;
	 --  	 -- fireEvent(new mxEventObject(mxEvent.UNDO, "edit", edit));
      end if;
   end Change_Selection;
   
   ----------------
   -- Cell_Added --
   ----------------
   
   procedure Cell_Added
     (This : access Selection_Model_Record;
      Cell : access Cell_Record'Class) is
   begin
      if Cell /= null then
	 Cells_Lists.Append (This.Cells, Cell);
      end if;
   end Cell_Added;
   
   ------------------
   -- Cell_Removed --
   ------------------
   
   procedure Cell_Removed
     (This : access Selection_Model_Record;
      Cell : access Cell_Record'Class) is
      
      use Cells_Lists;
      Cur : Cells_Lists.Cursor;
   begin
      Cur := Cells_Lists.Find (This.Cells, Cell);
      if Cur /= Cells_Lists.No_Element then
	 Cells_Lists.Delete (This.Cells, Cur);
      end if;
   end Cell_Removed;
   
   
   -----------------
   --  Get_Graph  --
   -----------------
   
   function Get_Graph
     (This : access Selection_Model_Record) return access Graph_Interface'Class
   is
   begin
      return This.Graph;
   end Get_Graph;
   
end Artics.Graph.Selections.Models;
		  
		  
