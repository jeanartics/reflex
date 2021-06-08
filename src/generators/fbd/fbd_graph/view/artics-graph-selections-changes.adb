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

with Artics.Graph.Events.Select_Events; use Artics.Graph.Events.Select_Events;

package body Artics.Graph.Selections.Changes is
   
   --------------------------
   -- New_Selection_Change --
   --------------------------
   
   function New_Selection_Change
     (Sel_Model : access Selection_Model_Record;
      Added     : Cells_Lists.List;
      Removed   : Cells_Lists.List) return Selection_Change_Ptr is
      
      S : Selection_Change_Ptr := 
	new Selection_Change_Record'(No_Selection_Change_Record);
   begin
      S.Set_Change_Type (Select_Change);
      S.Sel_Model := Sel_Model;
      S.Added   := Added;
      S.Removed := Removed;
      
      return S;
   end New_Selection_Change;
   
   ---------------------------
   -- Free_Selection_Change --
   ---------------------------
   
   procedure Free_Selection_Change (This : in out Selection_Change_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Selection_Change_Record, Selection_Change_Ptr);
   begin
      Cells_Lists.Clear (This.Added);
      Cells_Lists.Clear (This.Removed);
      Free (This);
   end Free_Selection_Change;
   
   -------------------------
   -- Get_Selection_Model --
   -------------------------
   
   function Get_Selection_Model 
     (This : access Selection_Change_Record) 
      return access Selection_Model_Record is
   begin
      return This.Sel_Model;
   end Get_Selection_Model;
   
   -------------------------
   -- Set_Selection_Model --
   -------------------------
   
   procedure Set_Selection_Model 
     (This      : access Selection_Change_Record;
      Sel_Model : access Selection_Model_Record) is
   begin
      This.Sel_Model := Sel_Model;
   end Set_Selection_Model;
   
   -------------
   -- Execute --
   -------------
   
   procedure Execute (S : access Selection_Change_Record) is
      
      Tmp : Cells_Lists.List;
   begin
      for Cell of S.Removed loop
	 S.Sel_Model.Cell_Removed (Cell);
      end loop;

      for Cell of S.Added loop
	 S.Sel_Model.Cell_Added (Cell);
      end loop;
      
      Tmp := S.Added;
      S.Added := S.Removed;
      S.Removed := Tmp;
      
      declare
      	 use Artics.Graph.Events.Select_Events;
      	 Evt : access Select_Event_Record := 
	   New_Select_Event (S.Added, S.Removed);
      begin
      	 Fire_Event (S.Sel_Model, Evt);
      	 Free_Select_Event (Select_Event_Ptr (Evt));
      end;
      --------model.fireEvent(new mxEventObject(mxEvent.CHANGE, "added", added,
      ------				"removed", removed));
      
   end Execute;
   
   -----------
   -- Added --
   -----------
   
   function Added
     (S : access Selection_Change_Record) return Cells_Lists.List is
   begin
      return S.Added;
   end Added;
   
   ---------------
   -- Set_Added --
   ---------------
   
   procedure Set_Added
     (S : access Selection_Change_Record;
      L : Cells_Lists.List) is
   begin
      S.Added := L;
   end Set_Added;
   
   -------------
   -- Removed --
   -------------
   
   function Removed
     (S : access Selection_Change_Record) return Cells_Lists.List is
   begin
      return S.Removed;
   end Removed;
   
   -----------------
   -- Set_Removed --
   -----------------
   
   procedure Set_Removed
     (S : access Selection_Change_Record;
      L : Cells_Lists.List) is
   begin
      S.Removed := L;
   end Set_Removed;
   
end Artics.Graph.Selections.Changes;
		  
		  
