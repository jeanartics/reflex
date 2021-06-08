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

package body Artics.Graph.Models_Changes.Childs is
   
   ----------------------
   -- New_Child_Change --
   ----------------------
   
   function New_Child_Change return Child_Change_Ptr
   is
   begin
      return New_Child_Change (null, null, null, 0);
   end New_Child_Change;
   
   
   ----------------------
   -- New_Child_Change --
   ----------------------
   
   function New_Child_Change
     (Model  : access Model_Interface'Class;
      Parent : access Cell_Record'Class;
      Child  : access Cell_Record'Class) return Child_Change_Ptr
   is
   begin
      return New_Child_Change (Model, Parent, Child, 0);
   end New_Child_Change;
   
   ----------------------
   -- New_Child_Change --
   ----------------------
   
   function New_Child_Change
     (Model  : access Model_Interface'Class;
      Parent : access Cell_Record'Class;
      Child  : access Cell_Record'Class;
      Index  : Integer) return Child_Change_Ptr 
   is
      
      Cc : Child_Change_Ptr := 
	new Child_Change_Record'(No_Child_Change_Record);
   begin
      Cc.Initialize_Change (Model, Child_Change);
      
      Cc.Parent         := Parent;
      Cc.Previous       := Parent;
      Cc.Child          := Child;
      Cc.Index          := Index;
      Cc.Previous_Index := Index;
      
      return Cc;
   end New_Child_Change;
   
   ----------------
   -- Get_Parent --
   ----------------
   
   function Get_Parent
     (C : access  Child_Change_Record) return access Cell_Record'Class is
   begin
      return C.Parent;
   end Get_Parent;
   
   ----------------
   -- Set_Parent --
   ----------------
   
   procedure Set_Parent
     (C     : access Child_Change_Record;
      Value : access Cell_Record'Class) is
   begin
      C.Parent := Value;
   end Set_Parent;
   
   ------------------
   -- Get_Previous --
   ------------------
   
   function Get_Previous
     (C : access Child_Change_Record) return access Cell_Record'Class is
   begin
      return C.Previous;
   end Get_Previous;
   
   ------------------
   -- Set_Previous --
   ------------------
   
   procedure Set_Previous
     (C     : access Child_Change_Record;
      Value : access Cell_Record'Class) is
   begin
      C.Previous := Value;
   end Set_Previous;
   
   ---------------
   -- Get_Child --
   ---------------
   
   function Get_Child
     (C : access Child_Change_Record) return access Cell_Record'Class is
   begin
      return C.Child;
   end Get_Child;
   
   ---------------
   -- Set_Child --
   ---------------
   
   procedure Set_Child
     (C     : access Child_Change_Record;
      Value : access Cell_Record'Class) is
   begin
      C.Child := Value;
   end Set_Child;
   
   ---------------
   -- Get_Index --
   ---------------
   
   function Get_Index
     (C : access Child_Change_Record) return integer is
   begin
      return C.Index;
   end Get_Index;
   
   ---------------
   -- Set_Index --
   ---------------
   
   procedure Set_Index
     (C     : access Child_Change_Record;
      Value : Integer) is
   begin
      C.Index := Value;
   end Set_Index;
   
   ------------------------
   -- Get_Previous_Index --
   ------------------------
   
   function Get_Previous_Index
     (C : access Child_Change_Record) return Integer is
   begin
      return C.Previous_Index;
   end Get_Previous_Index;
   
   ------------------------
   -- Set_Previous_Index --
   ------------------------
   
   procedure Set_Previous_Index
     (C     : access Child_Change_Record;
      Value : Integer) is
   begin
      C.Previous_Index := Value;
   end Set_Previous_Index;
   
   ------------------
   -- Get_Terminal --
   ------------------
   
   function Get_Terminal
     (C      : access Child_Change_Record;
      Edge   : access Cell_Record'Class;
      Source : Boolean) return access Cell_Record'Class is
      
      Model : access Model_Interface'Class := C.Get_Model;
   begin
      return Model.Get_Terminal (Edge, Source);
   end Get_Terminal;
   
   ------------------
   -- Set_Terminal --
   ------------------
   
   procedure Set_Terminal
     (C        : access Child_Change_Record;
      Edge     : access Cell_Record'Class;
      Terminal : access Cell_Record'Class;
      Source   : Boolean) is
   begin
      if Terminal = null then
	 Edge.Reset_Terminal (Source);
      else
	 Edge.Set_Terminal (Terminal, Source);
      end if;
   end Set_Terminal;
   
   -------------
   -- Connect --
   -------------
   
   procedure Connect
     (C          : access Child_Change_Record;
      Cell       : access Cell_Record'Class;
      Is_Connect : Boolean) 
   is      
      Model  : access Model_Interface'Class;
      Source : access Cell_Record'Class;
      Target : access Cell_Record'Class;
      Childs : Cells_Lists.List;
   begin
      Model  := C.Get_Model;
      Source := C.Get_Terminal (Cell, True);
      Target := C.Get_Terminal (Cell, False);
      
      if Source /= null then
	 if Is_Connect then
	    Model.Terminal_For_Cell_Changed (Cell, Source, True);
	 else
	    Model.Terminal_For_Cell_Changed (Cell, null, True);
	 end if;
      end if;
      
      if Target /= null then
	 if Is_Connect then
	    Model.Terminal_For_Cell_Changed (Cell, Target, False);
	 else
	    Model.Terminal_For_Cell_Changed (Cell, null, False);
	 end if;
      end if;
      
      -- Stores the previous terminals in the edge
      
      C.Set_Terminal (Cell, Source, True);
      C.Set_Terminal (Cell, Target, False);
      
      Childs := Cell.Get_Children_List;
      for Child of Childs loop
	 C.Connect (Child, Is_Connect);
      end loop;
      
   end Connect;
   
   ---------------------
   -- Get_Child_Index --
   ---------------------
   
   function Get_Child_Index
     (C      : access Child_Change_Record;
      Parent : access Cell_Record'Class;
      Child  : access Cell_Record'Class) return Integer is
   begin
      if Parent /= null then
	 return Parent.Get_Index (Child);
      else
	 return Integer'Last;
      end if;
   end Get_Child_Index;
   
   
   -------------
   -- Execute --
   -------------
   
   procedure Execute (C : access Child_Change_Record) 
   is 
      Model : access Model_Interface'Class := C.Get_Model;
      Tmp   : access Cell_Record'Class;
      Tmp2  : Integer;
   begin
      Tmp := Model.Get_Parent (C.Child);
      Tmp2 := C.Get_Child_Index (Tmp, C.Child);

      if C.Previous = null then
	 Connect (C, C.Child, False);
      end if;

      Tmp := Model.Parent_For_Cell_Changed 
        (C.Child, C.Previous, C.Previous_Index);

      if C.Previous /= null then
         Connect (C, C.Child, True);
      end if;
      
      C.Parent         := C.Previous;
      C.Previous       := Tmp;
      C.Index          := C.Previous_Index;
      C.Previous_Index := Tmp2;
   end Execute;
   
end Artics.Graph.Models_Changes.Childs;
