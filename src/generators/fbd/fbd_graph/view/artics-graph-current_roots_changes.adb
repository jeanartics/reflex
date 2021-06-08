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

package body Artics.Graph.Current_Roots_Changes is
   
   -----------------------------
   -- New_Current_Root_Change --
   -----------------------------
   
   function New_Current_Root_Change return Current_Root_Change_Ptr is
      This  : Current_Root_Change_Ptr := 
        new Current_Root_Change_Record'(No_Current_Root_Change_Record);
   begin
      return This;
   end New_Current_Root_Change;
   
   -----------------------------
   -- New_Current_Root_Change --
   -----------------------------
   
   function New_Current_Root_Change
     (View : access View_Interface'Class;
      Root : access Cell_Record'Class) return Current_Root_Change_Ptr is
      
      This  : Current_Root_Change_Ptr := New_Current_Root_Change;
      Tmp   : access Cell_Record'Class;
      Model : access Model_Interface'Class;
      Graph : access Graph_Interface'Class;
   begin
      This.Set_Change_Type (Current_Root_Change);
      This.View     := View;
      This.Root     := Root;
      This.Previous := This.Root;
      This.Up       := Root /= null;

      if not This.Up then
	 Tmp   := View.Get_Current_Root;
	 Graph := View.Get_Graph;
	 Model := Graph.Get_Model;

	 while Tmp /= null loop
	    if Tmp = Root then
	       This.Up := True;
	       exit;
	    end if;

	    Tmp := Model.Get_Parent (Tmp);
	 end loop;
      end if;
      
      return This;
   end New_Current_Root_Change;
   
   --------------
   -- Get_View --
   --------------
   
   function Get_View (This : access Current_Root_Change_Record) 
     return access View_Interface'Class is
   begin
      return This.View;
   end Get_View;
   
   --------------
   -- Get_Root --
   --------------
   
   function Get_Root (This : access Current_Root_Change_Record)
     return access Cell_Record'Class is
   begin
      return This.Root;
   end Get_Root;
   
   ------------------
   -- Get_Previous --
   ------------------
   
   function Get_Previous (This : access Current_Root_Change_Record)
     return access Cell_Record'Class is
   begin
      return This.Previous;
   end Get_Previous;
   
   -----------
   -- Is_Up --
   -----------
   
   function Is_Up (This : access Current_Root_Change_Record) return Boolean is
   begin
      return This.Up;
   end Is_Up;
   
   -------------
   -- Execute --
   -------------
   
   procedure Execute (This : access Current_Root_Change_Record) is
      Tmp       : access Cell_Record'Class;
      Translate : Point_Record;
      Graph     : access Graph_Interface'Class;
      View      : access View_Interface'Class;
   begin
      View := This.View;
      Tmp := View.Get_Current_Root;
      View.Set_Current_Root (This.Previous);
      
      This.Previous := Tmp;
      
      Graph := View.Get_Graph;
      Translate := Graph.Get_Translate_For_Root (View.Get_Current_Root);

      if Translate /= No_Point_Record then
	 View.Set_Translate (Point_Record'(- Get_X (Translate), Get_Y (Translate)));
      end if;

      -- Removes all existing cell states and revalidates
      
      View.Reload;
      This.Up := not This.Up;

      --  String eventName = (up) ? mxEvent.UP : mxEvent.DOWN;
      --  view.fireEvent(new mxEventObject(eventName, "root",
      --  			     view.currentRoot, "previous", previous));
   end Execute;
   
end Artics.Graph.Current_Roots_Changes;
