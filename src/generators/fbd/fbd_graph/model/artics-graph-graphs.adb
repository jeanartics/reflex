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

with Ada.Tags;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Artics.Utils; use Artics.Utils;
with Artics.Exceptions; use Artics.Exceptions;
with Artics.Output; use Artics.Output;

with Artics.Graph.Cells_Geometry; use Artics.Graph.Cells_Geometry;

with Artics.Graph.Utils; use Artics.Graph.Utils;
with Artics.Graph.Models;
with Artics.Graph.Models_Changes;
with Artics.Graph.Models_Changes.Childs;
with Artics.Graph.Models_Changes.Geometries;
with Artics.Graph.Models_Changes.Roots;
with Artics.Graph.Models_Changes.Terminals;
with Artics.Graph.Models_Changes.Values;

with Artics.Graph.Constants; use Artics.Graph.Constants;
  
with Artics.Graph.Filters_Interfaces; use Artics.Graph.Filters_Interfaces;
with Artics.Graph.Events.Root_Events; 
with Artics.Graph.Events.Align_Cells_Events; 
with Artics.Graph.Events.Flip_Edge_Events; 
with Artics.Graph.Events.Order_Cells_Events; 
with Artics.Graph.Events.Ordered_Cells_Events; 
with Artics.Graph.Events.Group_Cells_Events; 
with Artics.Graph.Events.Ungroup_Cells_Events;
with Artics.Graph.Events.Remove_Cells_From_Parent_Events;
with Artics.Graph.Events.Add_Cells_Events;
with Artics.Graph.Events.Cells_Added_Events;
with Artics.Graph.Events.Remove_Cells_Events;

with Artics.Graph.Events.Move_Cells_Events;
with Artics.Graph.Events.Moved_Cells_Events;
with Artics.Graph.Events.Cells_Removed_Events;


with Artics.Graph.Events.Connected_Cell_Events;
with Artics.Graph.Events.Connect_Cell_Events;
with Artics.Graph.Events.Update_Cell_Size_Events;
with Artics.Graph.Events.Rezise_Cells_Events;

with Artics.Named_Objects;use Artics.Named_Objects;
with Artics.Logutils; use Artics.Logutils;

package body Artics.Graph.Graphs is
   
   -- procedure Put_Line (S : String) is null; -- renames Ada.Text_IO.Put_Line;
   
   use Cells_Lists_Helpers;
   
   --------------------------------
   -- New_Full_Repainter_Handler --
   --------------------------------
   
   function New_Full_Repainter_Handler
     (G : Graph_Ptr) return access Full_Repainter_Handler_Record 
   is
      Listen : access Full_Repainter_Handler_Record := 
        new Full_Repainter_Handler_Record;
   begin
      Listen.Graph := G;
      return Listen;
   end New_Full_Repainter_Handler;
   
   ------------
   -- Invoke --
   ------------
   
   procedure Invoke 
     (Listener : access Full_Repainter_Handler_Record;
      Sender   : access Object_Record'Class;
      Evt      : access Graph_Events.Event_Object_Record'Class) is
   begin
      Listener.Graph.Repaint;
   end Invoke;
   
   -------------------------------
   -- New_Update_Origin_Handler --
   -------------------------------
   
   function New_Update_Origin_Handler 
     (G : Graph_Ptr) return access Update_Origin_Handler_Record is
      Listen : access Update_Origin_Handler_Record := 
        new Update_Origin_Handler_Record;
   begin
      Listen.Graph := G;
      return Listen;
   end New_Update_Origin_Handler;
   
   ------------
   -- Invoke --
   ------------
   
   procedure Invoke 
     (Listener : access Update_Origin_Handler_Record;
      Sender   : access Object_Record'Class;
      Evt      : access Graph_Events.Event_Object_Record'Class) is

      G : access Graph_Record := Listener.Graph;
   begin
      if G.Is_Auto_Origin then
         G.Update_Origin;
      end if;
   end Invoke;
   
   ------------------------------------
   -- New_Graph_Model_Change_Handler --
   ------------------------------------
   
   function New_Graph_Model_Change_Handler
     (G : Graph_Ptr) return access Graph_Model_Change_Handler_Record
   is
      Listen : access Graph_Model_Change_Handler_Record :=
        new Graph_Model_Change_Handler_Record;
   begin
      Listen.Graph := G;
      return Listen;
   end New_Graph_Model_Change_Handler;
   
   ------------
   -- Invoke --
   ------------
   
   procedure Invoke 
     (Listener : access Graph_Model_Change_Handler_Record;
      Sender   : access Object_Record'Class;
      Evt      : access Graph_Events.Event_Object_Record'Class) is
      
      G     : access Graph_Record := Listener.Graph;
      Model : access Model_Interface'Class := Model_Interface_Ptr (Sender);
      Dirty : Rectangle_Record;
      Event : access Change_Event_Record := Change_Event_Class_Ptr (Evt);
      Edit  : access Undoable_Edit_Record;
   begin
      Edit  := Get_Edit (Event);
      Dirty := G.Graph_Model_Changed (Model, Get_Changes (Edit));
      
      G.Repaint (Dirty);
   end Invoke;
   
   ---------------
   -- New_Graph --
   ---------------
   
   function New_Graph return access Graph_Record is
   begin
      return New_Graph (null);
   end New_Graph;

   ---------------
   -- New_Graph --
   ---------------
   
   function New_Graph
     (Model       : access Model_Interface'Class) return access Graph_Record 
   is
      G : Graph_Ptr := new Graph_Record'(No_Graph_Record);
   begin
      G.Full_Repainter_Handler     := New_Full_Repainter_Handler (G);
      G.Update_Origin_Handler      := New_Update_Origin_Handler (G);
      G.Graph_Model_Change_Handler := New_Graph_Model_Change_Handler (G);
            
      
      G.Selection_Model := G.Create_Selection_Model;
      
      if Model /= null then
         G.Set_Model (Model);
      else
         null;
         G.Set_Model (Artics.Graph.Models.New_Graph_Model);
      end if;
      
      G.Set_View (G.Create_Graph_View);
      
      return G;
   end New_Graph;

   ----------------------------
   -- Create_Selection_Model --
   ----------------------------
   
   function Create_Selection_Model
     (G : access Graph_Record) return access Selection_Model_Record'Class is
   begin
      return Artics.Graph.Selections.Models.New_Selection_Model (Graph_Ptr (G));
   end Create_Selection_Model;
   
   -----------------------
   -- Create_Graph_View --
   -----------------------
   
   function Create_Graph_View 
     (G : access Graph_Record) return access View_Record'Class is
   begin
      return Artics.Graph.Views.New_View (G);
   end Create_Graph_View;

   -----------
   -- Model --
   -----------
   
   function Get_Model
     (G : access Graph_Record) return access Model_Interface'Class is
   begin
      return G.Model;
   end Get_Model;
   
   ---------------
   -- Set_Model --
   ---------------
   
   procedure Set_Model
     (G : access Graph_Record;
      M : access Model_Interface'Class) 
   is      
      Old_Model : access Model_Interface'Class;
   begin
      if G.Model /= null then
         G.Model.Remove_Listener (G.Graph_Model_Change_Handler);
      end if;
      
      Old_Model := G.Model;
      G.Model := M;
      
      if G.View /= null then
         G.View.Revalidate;
      end if;
      
      G.Model.Add_Listener (Event_Change, G.Graph_Model_Change_Handler);
     
      G.Repaint;
   end Set_Model;
   
   ----------
   -- View --
   ----------
   
   function Get_View
     (G : access Graph_Record) return access View_Record'Class is
   begin
      return G.View;
   end Get_View;
   
   --------------
   -- Set_View --
   --------------
   
   procedure Set_View
     (G : access Graph_Record;
      V : access View_Record'Class) is
      
      Old_View : access View_Record'Class;
   begin
      if G.View /= null then
         G.View.Remove_Listener (G.Full_Repainter_Handler);
         G.View.Remove_Listener (G.Update_Origin_Handler);
      end if;

      Old_View := G.View;
      G.View := V;
      
      if G.View /= null then
         G.View.Revalidate;
      end if;

      -- Listens To Changes in The View
      
      G.View.Add_Listener (Event_Scale,               G.Full_Repainter_Handler);
      G.View.Add_Listener (Event_Scale,               G.Update_Origin_Handler);
      G.View.Add_Listener (Event_Translate,           G.Full_Repainter_Handler);
      G.View.Add_Listener (Event_Scale_And_Translate, G.Full_Repainter_Handler);
      G.View.Add_Listener (Event_Scale_And_Translate, G.Update_Origin_Handler);
      G.View.Add_Listener (Event_Up,                  G.Full_Repainter_Handler);
      G.View.Add_Listener (Event_Down,                G.Full_Repainter_Handler);
      
   end Set_View;
  
   -------------------------------------
   -- Get_Selection_Cells_For_Changes --
   -------------------------------------
   
   function Get_Selection_Cells_For_Changes
     (G       : access Graph_Record;
      Changes : Undoables_Lists.List) return Cells_Lists.List 
   is
      
      use Cells_Lists;
      
      use Artics.Graph.Models_Changes;
      use Artics.Graph.Models_Changes.Childs;
      use Artics.Graph.Models_Changes.Geometries;
      use Artics.Graph.Models_Changes.Roots;
      use Artics.Graph.Models_Changes.Terminals;
      use Artics.Graph.Models_Changes.Values;

      Cells : Cells_Lists.List;
      Cell  : access Cell_Record'Class;
   begin
      for Change of Changes loop
         if Change.Get_Change_Type = Child_Change then
            declare
               C : access Child_Change_Record := Child_Change_Ptr (Change);
            begin
               Cell := C.Get_Child;
               Cells.Append (Cell);
            end;
              
         elsif Change.Get_Change_Type = Terminal_Change then
            Cell := Terminal_Change_Ptr (Change).Get_Cell;
            Cells.Append (Cell);
              
         elsif Change.Get_Change_Type = Value_Change then
            Cell := Value_Change_Ptr (Change).Get_Cell;
            Cells.Append (Cell);
        
         elsif Change.Get_Change_Type = Geometry_Change then
            Cell := Geometry_Change_Ptr (Change).Get_Cell;               
            Cells.Append (Cell);
        
         end if;
      end loop;
      
      return G.Model.Get_Topmost_Cells (Cells);
   end Get_Selection_Cells_For_Changes;
   
   -------------------------
   -- Graph_Model_Changed --
   -------------------------
   
   function Graph_Model_Changed
     (G       : access Graph_Record;
      Sender  : access Model_Interface'Class;
      Changes : Undoables_Lists.List) return Rectangle_Record is
      
      use Artics.Graph.Models_Changes.Roots;
      use Ada.Tags;
      
      Thresh       : Integer;
      Ignore_Dirty : Boolean;
      Dirty        : Rectangle_Record;
      Tmp          : Rectangle_Record;
   begin
      Thresh := G.Get_Changes_Repaint_Threshold;
      Ignore_Dirty := Thresh > 0 and Integer (Changes.Length) > Thresh;

      -- Ignores Dirty Rectangle if There Was A Root Change
      
      if not Ignore_Dirty then
         for Change of Changes loop
            if Change'Tag = Root_Change_Record'Tag then
               Ignore_Dirty := True;
               exit;
            end if;
         end loop;
      end if;
      
      Dirty := G.Process_Changes (Changes, True, Ignore_Dirty);
      G.View.Validate;
      if G.Is_Auto_Origin then
         G.Update_Origin;
      end if;

      if not Ignore_Dirty then
         Tmp := G.Process_Changes (Changes, False, Ignore_Dirty);
         
         if Tmp /= No_Rectangle_Record then
            if Dirty = No_Rectangle_Record then
               Dirty := Tmp;
            else
               Add (Dirty, Tmp);
            end if;
         end if;
      end if;

      G.Remove_Selection_Cells (G.Get_Removed_Cells_For_Changes (Changes));

      return Dirty;
   end Graph_Model_Changed;
   
   -------------------
   -- Update_Origin --
   -------------------
   
   procedure Update_Origin (G : access Graph_Record) is
      
      Bounds : Rectangle_Record;
      Scale  : Coordinate;
      X      : Coordinate;
      Y      : Coordinate;
      X0     : Coordinate;
      Y0     : Coordinate;
      T      : Point_Record;
      Dx     : Coordinate;
      Dy     : Coordinate;
   begin
      Bounds := G.Get_Graph_Bounds;

      if Bounds /= No_Rectangle_Record then
	 
         Scale := G.View.Get_Scale;
         X := Get_X (Bounds) / (Scale - Coordinate (G.Get_Border));
         Y := Get_Y (Bounds) / (Scale - Coordinate (G.Get_Border));

         if X < 0.0 or Y < 0.0 then
            X0 := Artics.Maths.Min (0.0, X);
            Y0 := Artics.Maths.Min (0.0, Y);
	    
            Set_X (G.Origin, Get_X (G.Origin) + X0);
            Set_Y (G.Origin, Get_Y (G.Origin) + Y0);

            T := G.View.Get_Translate;
            G.View.Set_Translate
              (Point_Record'(Get_X (T) - X0, Get_Y (T) - Y0));
	    
         else
            if (X > 0.0 or Y > 0.0)
              and (Get_X (G.Origin) < 0.0 or Get_Y (G.Origin) < 0.0)
            then
               Dx := Artics.Maths.Min (- Get_X (G.Origin), X);
               Dy := Artics.Maths.Min (- Get_Y (G.Origin), Y);

               Set_X (G.Origin, Get_X (G.Origin) + Dx);
               Set_Y (G.Origin, Get_Y (G.Origin) + Dy);
	       
               T := G.View.Get_Translate;
               G.View.Set_Translate
                 (Point_Record'(Get_X (T) - Dx, Get_Y (T) - Dy));
            end if;
         end if;
      end if;
   end Update_Origin;
   
   -----------------------------------
   -- Get_Removed_Cells_For_Changes --
   -----------------------------------
   
   function Get_Removed_Cells_For_Changes
     (G       : access Graph_Record;
      Changes : Undoables_Lists.List) return Cells_Lists.List is
      
      use Undoables_Lists;
      use Cells_Lists;
      use Artics.Graph.Models_Changes.Roots;
      use Artics.Graph.Models_Changes.Childs;
      use Ada.Tags;
      
      Result         : Cells_Lists.List;
      Child_Change   : Child_Change_Ptr;
      Descendants    : Cells_Lists.List;
   begin
      for Change of Changes loop
         if Change'Tag = Root_Change_Record'Tag then
            exit;
         elsif Change'Tag = Child_Change_Record'Tag then
            Child_Change := Child_Change_Ptr (Change);
	    
            if Child_Change.Get_Parent = null then
               Descendants := Get_Descendants (G.Model, Child_Change.Get_Child);
               for Descendant of Descendants loop
                  Result.Append (Descendant);
               end loop;
            end if;

         end if;
      end loop;
      
      return Result;
   end Get_Removed_Cells_For_Changes;
   
   ---------------------
   -- Process_Changes --
   ---------------------
   
   function Process_Changes
     (G            : access Graph_Record;
      Changes      : Undoables_Lists.List;
      Invalidate   : Boolean;
      Ignore_Dirty : Boolean) return Rectangle_Record is
      
      Bounds : Rectangle_Record;
      Rect   : Rectangle_Record;
   begin
      Bounds := No_Rectangle_Record;
      
      for Change of Changes loop
         Rect := G.Process_Change (Change, Invalidate, Ignore_Dirty);
	 
         if Bounds = No_Rectangle_Record then
            Bounds := Rect;
         else
            Add (Bounds, Rect);
         end if;
      end loop;
      
      return Bounds;
   end Process_Changes;
   
   --------------------
   -- Process_Change --
   --------------------
   
   function Process_Change
     (G            : access Graph_Record;
      Change       : access Undoable_Change_Interface'Class;
      Invalidate   : Boolean;
      Ignore_Dirty : Boolean) return Rectangle_Record is
      
      use Ada.Tags;
      use Artics.Graph.Models_Changes;
      use Artics.Graph.Models_Changes.Childs;
      use Artics.Graph.Models_Changes.Geometries;
      use Artics.Graph.Models_Changes.Roots;
      use Artics.Graph.Models_Changes.Terminals;
      use Artics.Graph.Models_Changes.Values;
      
      Result          : Rectangle_Record := No_Rectangle_Record;
      Cell            : access Cell_Record'Class;
      Root_Change     : Root_Change_Ptr;
      Child_Change    : Child_Change_Ptr;
      Terminal_Change : Terminal_Change_Ptr;
      Value_Change    : Value_Change_Ptr;
      Geometry_Change : Geometry_Change_Ptr;
   begin
      if Change'Tag = Root_Change_Record'Tag then
         Root_Change := Root_Change_Ptr (Change);
	 
         if not Ignore_Dirty then
            Result := G.Get_Graph_Bounds;
         end if;	    

         if Invalidate then
            G.Clear_Selection;
            G.Remove_State_For_Cell (Root_Change.Get_Previous);
            
            if G.Is_Reset_View_On_Root_Change then
               G.View.Set_Events_Enabled (False);
	       
               begin
                  G.View.Scale_And_Translate (1.0, 0.0, 0.0);
               exception
                  when E : others =>
                     Log_Exception 
                       (Exception_Message (E, "Graphs.Process_Change"));
                     G.View.Set_Events_Enabled (True);
               end;
            end if;
         end if;
	 
         declare
            use Artics.Graph.Events.Root_Events;
            Evt : access Root_Event_Record := New_Root_Event;
         begin
            Fire_Event (G, Evt);
            Free_Root_Event (Root_Event_Ptr (Evt));
         end;
	 
      elsif Change'Tag = Child_Change_Record'Tag then
         Child_Change := Child_Change_Ptr (Change);

         -- Repaints the parent area if it is a rendered cell (vertex or edge)
         -- otherwise only the child area is repainted, same holds if the parent
         -- and previous are the same object, in which case only the child area
         -- needs to be repainted (change of order)
	
         if not Ignore_Dirty then
            if Child_Change.Get_Parent /= Child_Change.Get_Previous then
               if G.Model.Is_Vertex (Child_Change.Get_Parent) 
                 or G.Model.Is_Edge (Child_Change.Get_Parent)
               then
                  Result := G.Get_Bounding_Box
                    (Child_Change.Get_Parent, True, True);
               end if;

               if G.Model.Is_Vertex (Child_Change.Get_Previous)
                 or G.Model.Is_Edge (Child_Change.Get_Previous) then
                  if Result /= No_Rectangle_Record then
                     Add 
                       (Result, G.Get_Bounding_Box
                          (Child_Change.Get_Previous, True, True));
                  else
                     Result := G.Get_Bounding_Box 
                       (Child_Change.Get_Previous, True, True);
                  end if;
               end if;
            end if; 
	
            if Result = No_Rectangle_Record then
               Result := G.Get_Bounding_Box (Child_Change.Get_Child, True, True);
            end if;
         end if;

         if Invalidate then
            if Child_Change.Get_Parent /= null then
               G.View.Clear (Child_Change.Get_Child, False, True);
            else
               G.Remove_State_For_Cell (Child_Change.Get_Child);
            end if;
         end if;
	   
      elsif Change'Tag = Terminal_Change_Record'Tag then
         Terminal_Change := Terminal_Change_Ptr (Change);
         Cell := Terminal_Change.Get_Cell;
	 
         if not Ignore_Dirty then
            Result := G.Get_Bounding_Box (Cell, True);
         end if;
	 
         if Invalidate then
            G.View.Invalidate (Cell);
         end if;

      elsif Change'Tag = Value_Change_Record'Tag then
         Value_Change := Value_Change_Ptr (Change);
         Cell := Value_Change.Get_Cell;
	 
         if not Ignore_Dirty then
            Result := G.Get_Bounding_Box (Cell);
         end if;
	     
         if Invalidate then
            G.View.Clear (Cell, False, False);
         end if;
         
      elsif Change'Tag = Geometry_Change_Record'Tag then
         Geometry_Change := Geometry_Change_Ptr (Change);
         Cell := Geometry_Change.Get_Cell;

         if not Ignore_Dirty then
            Result := G.Get_Bounding_Box (Cell, True, True);
         end if;
	     
         if Invalidate then
            G.View.Invalidate (Cell);
         end if;
	 
      end if;
      
      return Result;
   end Process_Change;
   
   ---------------------------
   -- Remove_State_For_Cell --
   ---------------------------
   
   procedure Remove_State_For_Cell
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) is
      
      Childs : Cells_Lists.List;
   begin
      Childs := Cell.Get_Children_List;
      for Child of Childs loop
         G.Remove_State_For_Cell (Child);
      end loop;
      G.View.Invalidate (Cell);
      G.View.Remove_State (Cell);
   end Remove_State_For_Cell;

   -----------------
   -- Align_Cells --
   -----------------
   
   function Align_Cells
     (G     : access Graph_Record;
      Align : String) return Cells_Lists.List is
   begin
      return G.Align_Cells (String_Find (Align), Cells_Lists.Empty_List);
   end Align_Cells;
   
   -----------------
   -- Align_Cells --
   -----------------
   
   function Align_Cells
     (G     : access Graph_Record;
      Align : Name_Id) return Cells_Lists.List is
   begin
      return G.Align_Cells (Align, Cells_Lists.Empty_List);
   end Align_Cells;
   
   -----------------
   -- Align_Cells --
   -----------------
   
   function Align_Cells
     (G     : access Graph_Record;
      Align : String;
      Cells : Cells_Lists.List) return Cells_Lists.List is
   begin
      return G.Align_Cells (String_Find (Align), Cells);
   end Align_Cells;
   
   function Align_Cells
     (G     : access Graph_Record;
      Align : Name_Id;
      Cells : Cells_Lists.List) return Cells_Lists.List is
   begin
      return G.Align_Cells (Align, Cells, 0.0);
   end Align_Cells;
   
   -----------------
   -- Align_Cells --
   -----------------
   
   function Align_Cells
     (G     : access Graph_Record;
      Align : String;
      Cells : Cells_Lists.List;
      Param : Coordinate) return Cells_Lists.List is
   begin
      return Align_Cells (G, String_Find (Align), Cells, Param);
   end Align_Cells;
   
   -----------------
   -- Align_Cells --
   -----------------
   
   function Align_Cells
     (G     : access Graph_Record;
      Align : Name_Id;
      Cells : Cells_Lists.List;
      Param : Coordinate) return Cells_Lists.List is
      
      use Cells_Lists;
      
      Lparam      : Coordinate;
      Local_Cells : Cells_Lists.List := Cells;
      Geo         : access Cell_Geometry_Record'Class;
      Tmp         : Coordinate;
      Resets      : Cells_Lists.List;
   begin
      if Local_Cells /= Cells_Lists.Empty_List then
         Local_Cells := G.Get_Selection_Cells;
      end if;

      if Local_Cells /= Cells_Lists.Empty_List then
	 
         -- Finds the required coordinate for the alignment
         if Param = No_Coordinate then
	    
            for Cell of Local_Cells loop
               Geo := G.Get_Cell_Geometry (Cell);

               if Geo /= null and then not G.Model.Is_Edge (Cell) then
		  
                  if Param = No_Coordinate then
                     if Align = No_Name or Align = Artics.Graph.Names.ALIGN_LEFT then
                        LParam := Geo.Get_X;
			
                     elsif Align = Artics.Graph.Names.ALIGN_CENTER then
                        LParam := Geo.Get_X + Geo.Get_Width / 2.0;
                        exit;
			
                     elsif Align = Artics.Graph.Names.ALIGN_RIGHT then
                        LParam := Geo.Get_X + Geo.Get_Width;
			
                     elsif Align = Artics.Graph.Names.ALIGN_TOP then
                        LParam := Geo.Get_Y;
			
                     elsif Align = Artics.Graph.Names.ALIGN_MIDDLE then
                        LParam := Geo.Get_Y + Geo.Get_Height / 2.0;
                        exit;
			
                     elsif Align = Artics.Graph.Names.ALIGN_BOTTOM then
                        LParam := Geo.Get_Y + Geo.Get_Height;
                     end if;
		     
                  else
                     Tmp := Param;

                     if Align /= No_Name or Align = Artics.Graph.Names.ALIGN_LEFT then
                        LParam := Maths.Min (Tmp, Geo.Get_X);
			  
                     elsif Align = Artics.Graph.Names.ALIGN_RIGHT then
                        LParam := Maths.Max (Tmp, Geo.Get_X + Geo.Get_Width);
			
                     elsif Align = Artics.Graph.Names.ALIGN_TOP then
                        LParam := Maths.Min (Tmp, Geo.Get_Y);
			
                     elsif Align = Artics.Graph.Names.ALIGN_BOTTOM then
                        LParam := Maths.Max (Tmp, Geo.Get_Y + Geo.Get_Height);
                     end if;
                  end if;
               end if;
            end loop;
         end if;
	 
         -- Aligns the cells to the coordinate
	 
         G.Model.Begin_Update;
         begin
            -- double tmp = Double.parseDouble(String.valueOf(param));
	    
            for Cell of Local_Cells loop
               Geo := G.Get_Cell_Geometry (Cell);
	       
               if Geo /= null and not G.Model.Is_Edge (Cell) then
                  -----		  Geo := Geo.Clone;
		  
                  if Align /= No_Name or Align = Artics.Graph.Names.ALIGN_LEFT then
                     Geo.Set_X (Tmp);
		    
                  elsif Align = Artics.Graph.Names.ALIGN_CENTER then
                     Geo.Set_X (Tmp - Geo.Get_Width / 2.0);
		     
                  elsif Align = Artics.Graph.Names.ALIGN_RIGHT then
                     Geo.Set_X (Tmp - Geo.Get_Width);
		     
                  elsif Align = Artics.Graph.Names.ALIGN_TOP then
                     Geo.Set_Y (Tmp);
		     
                  elsif Align = Artics.Graph.Names.ALIGN_MIDDLE then
                     Geo.Set_Y (Tmp - Geo.Get_Height / 2.0);
		     
                  elsif Align = Artics.Graph.Names.ALIGN_BOTTOM then
                     Geo.Set_Y (Tmp - Geo.Get_Height);
                  end if;
		  
                  G.Model.Set_Geometry (Cell, Geo);
		  
                  if G.Is_Reset_Edges_On_Move then
                     Resets := Cells_Lists.Empty_List;
                     Resets.Append (Cell);
                     G.Reset_Edges (Resets);
                  end if;
               end if;
	       
               declare
                  use Artics.Graph.Events.Align_Cells_Events;
                  Evt : access Align_Cells_Event_Record := 
                    New_Align_Cells_Event (Cells, Align);
               begin
                  Fire_Event (G, Evt);
                  Free_Align_Cells_Event (Align_Cells_Event_Ptr (Evt));
                  --fireEvent(new mxEventObject(mxEvent.ALIGN_CELLS, "cells",
                  ---cells, "align", align));
               end;
            end loop;
	    
         exception
            when E : others =>
               Log_Exception 
                 (Exception_Message (E, "Graphs.Align_Cells"));
         end;
         G.Model.End_Update;
      end if;
      
      return Cells;
   end Align_Cells;

   ---------------
   -- Flip_Edge --
   ---------------
   
   function Flip_Edge 
     (G    : access Graph_Record;
      Edge : access Cell_Record'Class) return access Cell_Record'Class is
      
   begin
      if Edge /= null then
	 
         G.Model.Begin_Update;
         begin
            
            -- Removes all existing control points
	    
            G.Reset_Edge (Edge);
	    
            declare
               use Artics.Graph.Events.Flip_Edge_Events;
               Evt : access Flip_Edge_Event_Record := 
                 New_Flip_Edge_Event (Edge);
            begin
               Fire_Event (G, Evt);
               Free_Flip_Edge_Event (Flip_Edge_Event_Ptr (Evt));
               ---fireEvent(new mxEventObject(mxEvent.FLIP_EDGE, "edge", edge));
            end;
         exception
            when E : others =>
               Log_Exception 
                 (Exception_Message (E, "Graphs.Flip_Edge"));
         end;
         G.Model.End_Update;
      end if;

      return Edge;
   end Flip_Edge;
   
   -----------------
   -- Order_Cells --
   -----------------
   
   function Order_Cells
     (G     : access Graph_Record;
      Back  : Boolean;
      Cells : Cells_Lists.List := Cells_Lists.Empty_List) 
      return Cells_Lists.List is
      
      use Cells_Lists;
      
      Local_Cells : Cells_Lists.List := Cells;
   begin
      if Local_Cells /= Cells_Lists.Empty_List then
         Local_Cells := Sort_Cells (G.Get_Selection_Cells, True);
      end if;

      G.Model.Begin_Update;
      begin
         G.Cells_Ordered (Local_Cells, Back);
         declare
            use Artics.Graph.Events.Order_Cells_Events;
            Evt : access Order_Cells_Event_Record := 
              New_Order_Cells_Event (Back, Cells);
         begin
            Fire_Event (G, Evt);
            Free_Order_Cells_Event (Order_Cells_Event_Ptr (Evt));
            ---fireEvent(new mxEventObject(mxEvent.ORDER_CELLS, "cells", cells,
            ----"back", back));
         end;
      exception
         when E : others =>
            Log_Exception 
              (Exception_Message (E, "Graphs.Order_Cells"));
      end;
      G.Model.End_Update;

      return Cells;
   end Order_Cells;
   
   -------------------
   -- Cells_Ordered --
   -------------------
   
   procedure Cells_Ordered
     (G     : access Graph_Record;
      Cells : Cells_Lists.List;
      Back  : Boolean) is
      
      use Cells_Lists;
      
      Local_Cells : Cells_Lists.List := Cells;
      Parent      : access Cell_Record'Class;
      Index       : Integer;
   begin
      if Local_Cells /= Cells_Lists.Empty_List then
         begin 
            G.Model.Begin_Update;
            Index := 1;
            for Cell of Cells loop
               Parent := G.Model.Get_Parent (Cell);
               
               if Back then
                  G.Model.Add (Parent, Cell, Index);
               else
                  G.Model.Add (Parent, Cell, G.Model.Get_Child_Count (Parent));
               end if;
               
               Index := Index + 1;
            end loop;
            
            declare
               use Artics.Graph.Events.Ordered_Cells_Events;
               Evt : access Ordered_Cells_Event_Record := 
                 New_Ordered_Cells_Event (Back, Cells);
            begin
               Fire_Event (G, Evt);
               Free_Ordered_Cells_Event (Ordered_Cells_Event_Ptr (Evt));
               --fireEvent(new mxEventObject(mxEvent.CELLS_ORDERED, "cells",
               --cells, "back", back));
            end;
         exception 
            when E : others =>
               Log_Exception 
                 (Exception_Message (E, "Graphs.Cells_Ordered"));
         end;
         G.Model.End_Update;
      end if;
   end Cells_Ordered;
   
   -----------------
   -- Group_Cells --
   -----------------
   
   function Group_Cells
     (G      : access Graph_Record;
      Group  : access Cell_Record'Class := null;
      Border : Coordinate := 0.0;
      Cells  : Cells_Lists.List := Cells_Lists.Empty_List) 
      return access Cell_Record'Class is
      
      use Cells_Lists;
      
      Local_Group : access Cell_Record'Class := Group;
      Local_Cells : Cells_Lists.List := Cells;
      Bounds      : Rectangle_Record;
      Parent      : access Cell_Record'Class;
      Index       : Integer;
      Tmp         : Cells_Lists.List;
      Rects       : Rectangles_Lists.List;
   begin
      if Local_Cells = Cells_Lists.Empty_List then
         Local_Cells := Sort_Cells (G.Get_Selection_Cells, True);
      end if;

      Local_Cells := G.Get_Cells_For_Group (Local_Cells);

      if Local_Group = null then
         Local_Group := G.Create_Group_Cell (Local_Cells);
      end if;
      
      Bounds := G.Get_Bounds_For_Group (Local_Group, Local_Cells, Border);
      
      if Local_Cells /= Cells_Lists.Empty_List 
        and Bounds /= No_Rectangle_Record 
      then
	 
         -- Uses parent of group or previous parent of first child
	 
         Parent := G.Model.Get_Parent (Local_Group);

         if Parent = null then
            Parent := G.Model.Get_Parent (Get_First (Local_Cells));
         end if;
	 
         G.Model.Begin_Update;
         begin
            -- Checks if the group has a geometry and creates one if one does
            -- not exist
	    
            if G.Get_Cell_Geometry (Local_Group) = null then
               G.Model.Set_Geometry (Local_Group, New_Cell_Geometry);
            end if;

            -- Adds the children into the group and moves
	    
            Index := G.Model.Get_Child_Count (Local_Group);
	    
            G.Cells_Added
              (Local_Cells, Local_Group, Index, null, null, False);
            G.Cells_Moved
              (Local_Cells, - Get_X (Bounds), - Get_Y (Bounds), False, True);

            -- Adds the group into the parent and resizes
	    
            Index := G.Model.Get_Child_Count (Parent);
	    
            Tmp := Cells_Lists.Empty_List;
            Cells_Lists.Append (Tmp, Local_Group);
            G.Cells_Added
              (Tmp, Parent, Index, null, null, False, False);
	    
            Tmp := Cells_Lists.Empty_List;
            Cells_Lists.Append (Tmp, Local_Group);
            Rectangles_Lists.Append (Rects, Bounds);
            G.Cells_Resized (Tmp, Rects);
	    
            declare
               use Artics.Graph.Events.Group_Cells_Events;
               Evt : access Group_Cells_Event_Record := 
                 New_Group_Cells_Event (Group, Border, Cells);
            begin
               Fire_Event (G, Evt);
               Free_Group_Cells_Event (Group_Cells_Event_Ptr (Evt));
               --fireEvent(new mxEventObject(mxEvent.GROUP_CELLS, "group",
               -----group, "cells", cells, "border", border));
            end;
         exception
            when E : others =>
               Log_Exception 
                 (Exception_Message (E, "Graphs.Group_Cells"));
         end;
         G.Model.End_Update;
      end if;

      return Local_Group;
   end Group_Cells;
   
   -------------------------
   -- Get_Cells_For_Group --
   -------------------------
   
   function Get_Cells_For_Group 
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) return Cells_Lists.List is
      
      use Cells_Lists;
      
      Result : Cells_Lists.List;
      Parent : access Cell_Record'Class;
   begin
      if Cells /= Cells_Lists.Empty_List then
         Parent := G.Model.Get_Parent (Get_First (Cells));

         -- Filters selection cells with the same parent
	 
         for Cell of Cells loop
            if G.Model.Get_Parent (Cell) = Parent then
               Cells_Lists.Append (Result, Cell);
            end if;  
         end loop;
      end if;
      
      return Result;
   end Get_Cells_For_Group;
   
   --------------------------
   -- Get_Bounds_For_Group --
   --------------------------
   
   function Get_Bounds_For_Group
     (G        : access Graph_Record;
      Group    : access Cell_Record'Class;
      Children : Cells_Lists.List;
      Border   : Coordinate) return Rectangle_Record is
      
      Result : Rectangle_Record;
      Size   : Rectangle_Record;
   begin
      Result := G.Get_Bounding_Box_From_Geometry (Children);
      
      if Result /= No_Rectangle_Record then
	 
         if G.Is_Swimlane (Group) then
            Size := G.Get_Start_Size (Group);

            Set_X (Result, Get_X (Result) - Get_Width (Size));
	    
            Set_Y      (Result, Get_Y (Result) - Get_Height (Size));
            Set_Width  (Result, Get_Width (Result) + Get_Width (Size));
            Set_Height (Result, Get_Height (Result) + Get_Height (Size));
         end if;

         -- Adds the border
	 
         Set_X (Result, Get_X (Result) - Border);
         Set_Y (Result, Get_Y (Result) - Border);
         Set_Width  (Result, Get_Width (Result)  + 2.0 * Border);
         Set_Height (Result, Get_Height (Result) + 2.0 * Border);
      end if;
      
      return Result;
   end Get_Bounds_For_Group;
   
   -----------------------
   -- Create_Group_Cell --
   -----------------------
   
   function Create_Group_Cell
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) return access Cell_Record'Class is
      
      Group : access Cell_Record'Class;
      Geo   : access Cell_Geometry_Record'Class;
   begin
      Geo := New_Cell_Geometry;
      Group := New_Cell (null, Geo);
      Group.Set_Vertex (True);

      return Group;
   end Create_Group_Cell;
   
   -------------------
   -- Ungroup_Cells --
   -------------------
   
   function Ungroup_Cells
     (G     : access Graph_Record;
      Cells : Cells_Lists.List := Cells_Lists.Empty_List) 
      return Cells_Lists.List is
      
      use Cells_Lists;
      
      Local_Cells : Cells_Lists.List := Cells;
      Result      : Cells_Lists.List;
      Tmp         : Cells_Lists.List;
      Children    : Cells_Lists.List;
      Parent      : access Cell_Record'Class;
      Index       : Integer;
   begin
      if Local_Cells = Cells_Lists.Empty_List then
	 
         Local_Cells := G.Get_Selection_Cells;

         -- Finds the cells with children
	 
         for Cell of Local_Cells loop
	    
            if G.Model.Get_Child_Count (Cell) > 0 then
               Cells_Lists.Append (Tmp, Cell);
            end if;
         end loop;

         Local_Cells := Tmp;
      end if;

      if Local_Cells /= Cells_Lists.Empty_List then
	 
         G.Model.Begin_Update;
	 
         begin
            for Cell of Local_Cells loop
               Children := G.Model.Get_Children (Cell);

               if Children /= Cells_Lists.Empty_List then
                  Parent := G.Model.Get_Parent (Cell);
                  Index  := G.Model.Get_Child_Count (Parent);

                  G.Cells_Added (Children, Parent, Index, null, null, True);
		 
                  for Cell of Children loop
                     Cells_Lists.Append (Result, Cell);
                  end loop;
               end if;
            end loop;
	    
            G.Cells_Removed (Add_All_Edges (G, Local_Cells));
            declare
               use Artics.Graph.Events.Ungroup_Cells_Events;
               Evt : access Ungroup_Cells_Event_Record := 
                 New_Ungroup_Cells_Event (Cells);
            begin
               Fire_Event (G, Evt);
               Free_Ungroup_Cells_Event (Ungroup_Cells_Event_Ptr (Evt));
               --fireEvent(new mxEventObject(mxEvent.UNGROUP_CELLS, "cells",
               --cells));
            end;
         exception
            when E : others =>
               Log_Exception 
                 (Exception_Message (E, "Grap.Ungroup_Cells"));
         end;
         G.Model.End_Update;
      end if;

      return Result;
   end Ungroup_Cells;
   
   ------------------------------
   -- Remove_Cells_From_Parent --
   ------------------------------
   
   procedure Remove_Cells_From_Parent
     (G     : access Graph_Record;
      Cells : Cells_Lists.List := Cells_Lists.Empty_List) is
      
      use Cells_Lists;
      
      Local_Cells : Cells_Lists.List := Cells;
      Parent      : access Cell_Record'Class;
      Index       : Integer;
   begin
      if Local_Cells = Cells_Lists.Empty_List then
         Local_Cells := G.Get_Selection_Cells;
      end if;
      
      G.Model.Begin_Update;
      begin
         Parent := G.Get_Default_Parent;
         Index := G.Model.Get_Child_Count (Parent);

         G.Cells_Added (Cells, Parent, Index, null, null, True);
         declare
            use Artics.Graph.Events.Remove_Cells_From_Parent_Events;
            Evt : access Remove_Cells_From_Parent_Event_Record := 
              New_Remove_Cells_From_Parent_Event (Cells);
         begin
            Fire_Event (G, Evt);
            Free_Remove_Cells_From_Parent_Event
              (Remove_Cells_From_Parent_Event_Ptr (Evt));
            ---fireEvent(new mxEventObject(mxEvent.REMOVE_CELLS_FROM_PARENT,
            --				"cells", cells));
         end;
      exception
         when E : others =>
            Log_Exception 
              (Exception_Message (E, "Graphs.Remove_Cells_From_Parent"));
      end;
      G.Model.End_Update;
   end Remove_Cells_From_Parent;
   
   -------------------------
   -- Update_Group_Bounds --
   -------------------------
   
   procedure Update_Group_Bounds
     (G           : access Graph_Record;
      Cells       : Cells_Lists.List := Cells_Lists.Empty_List;
      Border      : Coordinate := 0.0;
      Move_Parent : Boolean := False) is
      
      use Cells_Lists;
      
      Local_Cells  : Cells_Lists.List := Cells;
      Geo          : access Cell_Geometry_Record'Class;
      Children     : Cells_Lists.List;
      Child_Bounds : Rectangle_Record;
      Size         : Rectangle_Record;
      Dummy        : Cells_Lists.List;
   begin
      if Local_Cells = Cells_Lists.Empty_List then
         Local_Cells := G.Get_Selection_Cells;
      end if;

      G.Model.Begin_Update;
      begin
         for Cell of Local_Cells loop
            Geo := G.Get_Cell_Geometry (Cell);

            if Geo /= null then
               Children := G.Get_Child_Cells (Cell);

               if Children /= Cells_Lists.Empty_List then
                  Child_Bounds := G.Get_Bounding_Box_From_Geometry (Children);

                  if Get_Width(Child_Bounds) > 0.0 
                    and Get_Height (Child_Bounds) > 0.0 
                  then
                     if G.Is_Swimlane (Cell) then
                        Size := G.Get_Start_Size (Cell);
                     else
                        Size := No_Rectangle_Record;

                        if Move_Parent then
                           Set_X
                             (Geo, Get_X (Geo) 
                              + Get_X (Child_Bounds) - Get_Width (Size)
                              - Border);
                           Set_Y (Geo, Get_Y (Geo) 
                                  + Get_Y (Child_Bounds) 
                                  - Get_Height (Size) - Border);
                        end if;

                        Set_Width
                          (Geo, Get_Width (Child_Bounds)
                           + Get_Width (Size) + 2.0 * Border);
                        Set_Height
                          (Geo, Get_Height (Child_Bounds)
                           + Get_Height (Size) + 2.0 * Border);

                        G.Model.Set_Geometry (Cell, Geo);
                        Dummy := G.Move_Cells
                          (Children,
                           - Get_X (Child_Bounds) 
                           + Get_Width (Size)
                           + Border, -Get_Y (Child_Bounds)
                           + Get_Height (Size) + Border);
                     end if;
                  end if;
               end if;
            end if;
         end loop;
      exception
         when E : others =>
            Log_Exception 
              (Exception_Message (E, "Graphs.Remove_Update_Group_Bounds"));
      end;
      G.Model.End_Update;
      
      -- return Cells;
   end Update_Group_Bounds;
   
   -----------------
   -- Clone_Cells --
   -----------------
   
   function Clone_Cells
     (G                   : access Graph_Record;
      Cells               : Cells_Lists.List;
      Allow_Invalid_Edges : Boolean := True) return Cells_Lists.List is
      
      use Cells_Lists;
      use Point_Lists;
      
      Clones : Cells_Lists.List;
      Scale  : Coordinate;
      Trans  : Point_Record;
      Cell   : access Cell_Record'Class;
      Cur    : Cells_Lists.Cursor;
      Geo    : access Cell_Geometry_Record'Class;
      State  : access Cell_State_Record'Class;
      Pstate : access Cell_State_Record'Class;
      Dx     : Coordinate;
      Dy     : Coordinate;
      Origin : Point_Record;
      Src    : access Cell_Record'Class;
      Trg    : access Cell_Record'Class;
      Pt     : Point_Record;
      Points : Point_Lists.List;
      Pt_Cur : Point_Lists.Cursor;
   begin
      if Cells /= Cells_Lists.Empty_List then
	 
         Scale := G.View.Get_Scale;
         Trans := G.View.Get_Translate;
         Clones := G.Model.Clone_Cells (Cells, True);
	 
         Cur := Cells_Lists.First (Clones);
         while Cells_Lists.Has_Element (Cur) loop
            Cell := Cells_Lists.Element (Cur);
	    
            if not Allow_Invalid_Edges
              and  G.Model.Is_Edge (Cell)
              and  G.Get_Edge_Validation_Error
                (Cell, 
                 G.Model.Get_Terminal (Cell, True),
                 G.Model.Get_Terminal (Cell, False)) /= ""
            then
               Cells_Lists.Delete (Clones, Cur);
            else
               Geo := G.Model.Get_Geometry (Cell);
	       
               if Geo /= null then
                  State  := G.View.Get_State (Cell);
                  Pstate := G.View.Get_State (G.Model.Get_Parent (Cell));

                  if State /= null and Pstate /= null then
                     Origin :=  Pstate.Get_Origin;
                     Dx := Get_X (Origin);
                     Dy := Get_Y (Origin);

                     if G.Model.Is_Edge (Cell) then
			
                        -- Checks if the source is cloned or sets the terminal
                        -- point
			
                        Src := G.Model.Get_Terminal (Cell, True);
			
                        while Src /= null 
                          and not Cells_Lists.Contains (Clones , Src) loop
                           Src := G.Model.Get_Parent (Src);
                        end loop;

                        if Src = null then
                           Pt := State.Get_First_Absolute_Point;
                           Geo.Set_Terminal_Point
                             (Point_Record'
                                (Get_X (Pt) / Scale - Get_X (Trans), 
                                 Get_Y (Pt) / Scale - Get_Y (Trans)), 
                              True);
                        end if;
			     
                        -- Checks if the target is cloned or sets the terminal 
                        -- point
			
                        Trg := G.Model.Get_Terminal (Cell, False);

                        while Trg /= null 
                          and not Cells_Lists.Contains (Clones, Trg) loop
                           Trg := G.Model.Get_Parent (Trg);
                        end loop;

                        if Trg = null then
                           Pt := State.Get_Last_Absolute_Point;
                           Geo.Set_Terminal_Point
                             (Point_Record'
                                (Get_X (Pt) / Scale - Get_X (Trans), 
                                 Get_Y (Pt) / Scale - Get_Y (Trans)), 
                              False);
                        end if;

                        -- Translates the control points
			
                        Points := Geo.Get_Points;
			
                        if Points /= Point_Lists.Empty_List then
                           Pt_Cur := Point_Lists.First (Points);
                           while Point_Lists.Has_Element (Pt_Cur) loop
                              Pt := Point_Lists.Element (Pt_Cur);
                              Set_X (Pt, Get_X (Pt) + Dx);
                              Set_Y (Pt, Get_Y (Pt) + Dy);
                              Point_Lists.Replace_Element (Points, Pt_Cur, Pt);
                              Next (Pt_Cur);   
                           end loop;
                        end if;
			
                     else
                        Geo.Set_X (Geo.Get_X + Dx);
                        Geo.Set_Y (Geo.Get_Y + Dy);
                     end if;
                  end if;
               end if;
            end if;
         end loop;
      else
         Clones := Cells_Lists.Empty_List;
      end if;
      
      return Clones;
   end Clone_Cells;
   
   -------------------
   -- Insert_Vertex --
   -------------------
   
   function Insert_Vertex
     (G        : access Graph_Record;
      Parent   : access Cell_Record'Class;
      Id       : String;
      Value    : access Object_Record'Class;
      X        : Coordinate;
      Y        : Coordinate;
      Width    : Coordinate;
      Height   : Coordinate;
      Relative : Boolean := False) return access Cell_Record'Class is
      
      Vertex : access Cell_Record'Class;
   begin
      Vertex := G.Create_Vertex
        (Parent, Id, Value, X, Y, Width, Height, Relative);
      G.Add_Cell (Vertex, Parent);
      return Vertex;
   end Insert_Vertex;
   
   
   -------------------
   -- Create_Vertex --
   -------------------
   
   function Create_Vertex
     (G        : access Graph_Record;
      Parent   : access Cell_Record'Class;
      Id       : String;
      Value    : access Object_Record'Class;
      X        : Coordinate;
      Y        : Coordinate;
      Width    : Coordinate;
      Height   : Coordinate;
      Relative : Boolean := False) return access Cell_Record'Class 
   is
      Geometry : access Cell_Geometry_Record'Class;
      Vertex   : access Cell_Record'Class;
   begin
      Geometry := New_Cell_Geometry (X, Y, Width, Height);
      Geometry.Set_Relative (Relative);

      Vertex := Artics.Graph.Cells.New_Cell (Value, Geometry);
      Vertex.Set_Id (Id);
      Vertex.Set_Vertex (True);
      --        Vertex.Set_Parent (Parent);
      return Vertex;
   end Create_Vertex;
   
   -----------------
   -- Insert_Edge --
   -----------------
   
   function Insert_Edge
     (G      : access Graph_Record;
      Parent : access Cell_Record'Class;
      Id     : String;
      Value  : access Object_Record'Class;
      Source : access Cell_Record'Class;
      Target : access Cell_Record'Class) return access Cell_Record'Class is
      
      Edge : access Cell_Record'Class;
   begin
      Edge := G.Create_Edge (Id, Value);

      G.Add_Edge (Edge, Parent, Source, Target, 0);
      
      return Edge;
   end Insert_Edge;
   
   -----------------
   -- Create_Edge --
   -----------------
   
   function Create_Edge
     (G     : access Graph_Record;
      Id    : String;
      Value : access Object_Record'Class) return access Cell_Record'Class is
      
      Edge : access Cell_Record'Class;
      Geo  : access Cell_Geometry_Record'Class;
   begin
      Geo := New_Cell_Geometry;
      Geo.Set_Relative (True);
      
      Edge := Artics.Graph.Cells.New_Cell (Value, Geo);
      Edge.Set_Id (Id);
      Edge.Set_Edge (True);

      return Edge;
   end Create_Edge;
   
   --------------
   -- Add_Edge --
   --------------
   
   procedure Add_Edge
     (G      : access Graph_Record;
      Edge   : access Cell_Record'Class;
      Parent : access Cell_Record'Class;
      Source : access Cell_Record'Class;
      Target : access Cell_Record'Class;
      Index  : Integer) is
   begin
      G.Add_Cell (Edge, Parent, Index, Source, Target);
   end Add_Edge;
   
   --------------
   -- Add_Cell --
   --------------
   
   procedure Add_Cell
     (G      : access Graph_Record;
      Cell   : access Cell_Record'Class;
      Parent : access Cell_Record'Class := null;
      Index  : Integer := 0;
      Source : access Cell_Record'Class := null;
      Target : access Cell_Record'Class := null) is

      Cells : Cells_Lists.List;
   begin
      Cells_Lists.Append (Cells, Cell);
      G.Add_Cells
        (Cells, Parent, Index, Source, Target);
   end Add_Cell;
   
   ---------------
   -- Add_Cells --
   ---------------
   
   procedure Add_Cells
     (G      : access Graph_Record;
      Cells  : Cells_Lists.List;
      Parent : access Cell_Record'Class := null;
      Index  : Integer := 0;
      Source : access Cell_Record'Class := null;
      Target : access Cell_Record'Class := null) is
      
      P   : access Cell_Record'Class := Parent;
      Pos : Integer := Index;
   begin
                 
      if P = null then
         P := G.Get_Default_Parent;
      end if;

      if Index = 0 then 
         Pos := G.Model.Get_Child_Count (Parent);
      end if;
      
      G.Model.Begin_Update;
      begin
         G.Cells_Added (Cells, P, Pos, Source, Target, False, True);
	 
         declare
            use Artics.Graph.Events.Add_Cells_Events;
	    
            Evt : access Add_Cells_Event_Record := 
              New_Add_Cells_Event (Cells, P, Pos, Source, Target);
         begin
            Fire_Event (G, Evt);
            Free_Add_Cells_Event (Add_Cells_Event_Ptr (Evt));
            --  fireEvent(new MxEventObject
            --  	(mxEvent.ADD_CELLS, "cells", cells,
            --  	parent", parent, "index", index, "source", source,
            --  	"target", target));
         end;
      exception
         when E : others =>
            Log_Exception 
              (Exception_Message (E, "Graphs.Add_Cells"));
      end;
      G.Model.End_Update;
   end Add_Cells;
   
   -----------------
   -- Cells_Added --
   -----------------
   
   procedure Cells_Added
     (G         : access Graph_Record;
      Cells     : Cells_Lists.List;
      Parent    : access Cell_Record'Class;
      Index     : Integer;
      Source    : access Cell_Record'Class;
      Target    : access Cell_Record'Class;
      Absolute  : Boolean;
      Constrain : Boolean := True) is
      
      use Cells_Lists;
      
      Parent_State : access Cell_State_Record'Class;
      O1           : Point_Record;
      Zero         : Point_Record := Zero_Point_Record;
      Previous     : access Cell_Record'Class;
      Old_State    : access Cell_State_Record'Class;
      O2           : Point_Record;
      Geo          : access Cell_Geometry_Record'Class;
      Dx           : Coordinate;
      Dy           : Coordinate;
      I            : Integer;
      Childs       : Cells_Lists.List;
      Cur          : Cells_Lists.Cursor;
   begin
      if Cells /= Cells_Lists.Empty_List and Parent /= null then
	 
         G.Model.Begin_Update;
         begin
            if Absolute then
               Parent_State := G.View.Get_State (Parent);
            else
               Parent_State := null;
            end if;
	    
            if Parent_State /= null then
               O1 := Parent_State.Get_Origin;
            else
               O1 := No_Point_Record;
            end if;
	    
            I := 0;
            for Cell of Cells loop
               if Cell /= null then
                  Previous := G.Model.Get_Parent (Cell);

                  -- Keeps the cell at its absolute location
		  
                  if O1 /= No_Point_Record 
                    and Cell /= Parent 
                    and Parent /= Previous 
                  then
                     Old_State := G.View.Get_State (Previous);
		     
                     if Old_State /= null then
                        O2 := Old_State.Get_Origin;
                     else
                        O2 := Zero;
                     end if;
		     
                     Geo := G.Model.Get_Geometry (Cell);

                     if Geo /= null then
                        Dx := Get_X (O2) - Get_X (O1);
                        Dy := Get_Y (O2) - Get_Y (O1);
                        Geo.Translate (Dx, Dy);
			
                        if not Geo.Is_Relative
                          and G.Model.Is_Vertex (Cell)
                          and not G.Is_Allow_Negative_Coordinates
                        then
                           Geo.Set_X (Maths.Max (0.0, Geo.Get_X));
                           Geo.Set_Y (Maths.Max (0.0, Geo.Get_Y));
                        end if;
                     end if;
                  end if;

                  -- Decrements all following indices if cell is already in
                  -- parent
                  G.Model.Add (Parent, Cell, Index + 1);
		  
                  -- Extends the parent
                  if G.Is_Extend_Parents_On_Add 
                    and G.Is_Extend_Parent (Cell)
                  then
                     Extend_Parent (G, Cell);
                  end if;
		  
                  -- Constrains the child
                  if Constrain then
                     G.Constrain_Child (Cell);
                  end if;
		  
                  -- Sets the source terminal
                  if Source /= null then
                     G.Cell_Connected (Cell, Source, True, null);
                  end if;
		  
                  -- Sets the target terminal
                  if Target /= null then
                     G.Cell_Connected (Cell, Target, False, null);
                  end if;
                  
                  I := I + 1;
               end if;
            end loop;
	    
            declare
               use Artics.Graph.Events.Cells_Added_Events;
               Evt : access Cells_Added_Event_Record := 
                 New_Cells_Added_Event
                   (Cells, Parent, Index, Source, Target, Absolute);
            begin
               Fire_Event (G, Evt);
               Free_Cells_Added_Event (Cells_Added_Event_Ptr (Evt));
               -- fireEvent(new mxEventObject(mxEvent.CELLS_ADDED, "cells",
               -- cells, "parent", parent, "index", index, "source",
               -- source, "target", target, "absolute", absolute));
            end;
	    
         exception
            when E : others =>
               Log_Exception 
                 (Exception_Message (E, "Graphs.Cells_Added"));
         end;
         G.Model.End_Update;
      end if;
   end Cells_Added;
   
   ------------------
   -- Remove_Cells --
   ------------------
   
   procedure Remove_Cells
     (G             : access Graph_Record;
      Cells         : Cells_Lists.List := Cells_Lists.Empty_List;
      Include_Edges : Boolean := True) is
      
      use Cells_Lists;
      
      Cls : Cells_Lists.List := Cells;
   begin
      if Cls = Cells_Lists.Empty_List then
         Cls := G.Get_Deletable_Cells (G.Get_Selection_Cells);
      end if;

      -- Adds all edges to the cells
      
      if Include_Edges then
         Cls := G.Get_Deletable_Cells (Add_All_Edges (G, Cls));
      end if;

      G.Model.Begin_Update;
      begin
         G.Cells_Removed (Cls);
         declare 
            use Artics.Graph.Events.Remove_Cells_Events;
            Evt : access Remove_Cells_Event_Record := 
              New_Remove_Cells_Event (Cells, Include_Edges);
         begin
            Fire_Event (G, Evt);
            Free_Remove_Cells_Event (Remove_Cells_Event_Ptr (Evt));
            --  fireEvent(new mxEventObject(mxEvent.REMOVE_CELLS, "cells", 
            --      cells, "includeEdges", includeEdges));
         end;
      exception
         when E : others =>
            Log_Exception 
              (Exception_Message (E, "Graphs.Remove_Cells"));
      end;
      G.Model.End_Update;
   end Remove_Cells;
   
   -------------------
   -- Cells_Removed --
   -------------------
   
   procedure Cells_Removed
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) is
      
      use Cells_Lists;
      
      Scale  : Coordinate;
      Tr     : Point_Record;
      Edges  : Cells_Lists.List;
      Geo    : access Cell_Geometry_Record'Class;
      State  : access Cell_State_Record'Class;
      Source : Boolean;
      Pt     : Point_Record;
      Tmp    : access Cell_Record'Class;
   begin
      if Cells /= Cells_Lists.Empty_List then
	 
         Scale := G.View.Get_Scale;
         Tr    := G.View.Get_Translate;

         G.Model.Begin_Update;
         begin
            for Cell of Cells loop
	    
               -- Disconnects edges which are not in cells
	       
               Edges := G.Get_Connections (Cell);
	       
               for Edge of Edges loop
		  
                  if Cells.Contains (Edge) then
                     Geo := G.Model.Get_Geometry (Edge);
		     
                     if Geo /= null then
                        State := G.View.Get_State (Edge);
			
                        if State /= null then
			   
                           -- Checks which side of the edge is being 
                           -- disconnected
			   
                           Tmp := State.Get_Visible_Terminal (True);
                           Source := False;
			   
                           while Tmp /= null loop
                              if Cell = Tmp then
                                 Source := True;
                                 exit;
                              end if;
			      
                              Tmp := G.Model.Get_Parent (Tmp);
                           end loop;
			   
                           ---Geo := Clone (Cell_Geometry_Ptr (Geo));
			   
                           if Source then
                              Pt := State.Get_First_Absolute_Point;
                           else
                              Pt := State.Get_Last_Absolute_Point;
                           end if;
			   
                           Geo.Set_Terminal_Point
                             (Point_Record'
                                (Get_X (Pt) / Scale - Get_X (Tr), 
                                 Get_Y (Pt) / Scale - Get_Y (Tr)), 
                              Source);
			   
                           G.Model.Set_Terminal (Edge, null, Source);
                           G.Model.Set_Geometry (Edge, Geo);
                        end if;
                     end if;
                  end if;
               end loop;
		       
	    
               G.Model.Remove (Cell);
            end loop;
	    
            declare
               use Artics.Graph.Events.Cells_Removed_Events;
               Evt : access Cells_Removed_Event_Record := 
                 New_Cells_Removed_Event (Cells);
            begin
               Fire_Event (G, Evt);
               Free_Cells_Removed_Event (Cells_Removed_Event_Ptr (Evt));
               --  fireEvent(new mxEventObject(mxEvent.CELLS_REMOVED, "cells",
               --  				cells));
            end;
         exception
            when E : others =>
               Log_Exception 
                 (Exception_Message (E, "Graphs.Cells_Removed"));
         end;
         G.Model.End_Update;
      end if;
   end Cells_Removed;
   
   ----------------
   -- Split_Edge --
   ----------------
   
   procedure Split_Edge 
     (G        : access Graph_Record;
      Edge     : access Cell_Record'Class;
      Cells    : Cells_Lists.List) is
      
   begin
      G.Split_Edge (Edge, Cells, null, 0.0, 0.0);
   end Split_Edge;
   
   ----------------
   -- Split_Edge --
   ----------------
   
   procedure Split_Edge 
     (G        : access Graph_Record;
      Edge     : access Cell_Record'Class;
      Cells    : Cells_Lists.List;
      Dx       : Coordinate;
      Dy       : Coordinate) is
      
   begin
      G.Split_Edge (Edge, Cells, null, Dx, Dy);
   end Split_Edge;
   
   ----------------
   -- Split_Edge --
   ----------------
   
   procedure Split_Edge 
     (G        : access Graph_Record;
      Edge     : access Cell_Record'Class;
      Cells    : Cells_Lists.List;
      New_Edge : access Cell_Record'Class;
      Dx       : Coordinate;
      Dy       : Coordinate) is
      
   begin
      null;
   end Split_Edge;
  
   -----------------
   -- Swap_Bounds --
   -----------------
   
   procedure Swap_Bounds
     (G             : access Graph_Record;
      Cell          : access Cell_Record'Class) is
      
      use Cells_Lists;
      
      Geo : access Cell_Geometry_Record'Class;
   begin
      if Cell /= null then
         Geo := G.Model.Get_Geometry (Cell);

         if Geo /= null then
            Geo := Clone (Cell_Geometry_Ptr (Geo));

            G.Update_Alternate_Bounds (Cell, Geo);
            Geo.Swap;

            G.Model.Set_Geometry (Cell, Geo);
         end if;
      end if;
   end Swap_Bounds;
   
   -----------------------------
   -- Update_Alternate_Bounds --
   -----------------------------
   
   procedure Update_Alternate_Bounds
     (G             : access Graph_Record;
      Cell          : access Cell_Record'Class;
      Geo           : access Cell_Geometry_Record'Class) is
      
      Bounds : Rectangle_Record;
      Size   : Rectangle_Record;
   begin
      if Cell /= null and Geo /= null then
	 
         if Geo.Get_Alternate_Bounds = No_Rectangle_Record then
            Bounds := No_Rectangle_Record;
				
            if G.Is_Collapse_To_Preferred_Size then
               Bounds := G.Get_Preferred_Size_For_Cell (Cell);

               if G.Is_Swimlane (Cell) then
                  Size := G.Get_Start_Size (Cell);

                  Set_Height
                    (Bounds, 
                     Maths.Max (Get_Height (Bounds), Get_Height (Size)));
		  
                  Set_Width
                    (Bounds, 
                     Maths.Max (Get_Width (Bounds), Get_Width (Size)));
               end if;
            end if;

            if Bounds = No_Rectangle_Record then
               Bounds := Geo.Geometry_Rectangle;
            end if;

            Geo.Set_Alternate_Bounds
              (Rectangle_Record'
                 (Point_Record'(Geo.Get_X, Geo.Get_Y),
                  Get_Width (Bounds), Get_Height (Bounds)));
	    
         else
            Bounds := Geo.Get_Alternate_Bounds;
            Set_X (Bounds, Geo.Get_X);
            Set_Y (Bounds, Geo.Get_Y);
            Geo.Set_Alternate_Bounds (Bounds);
         end if;
      end if;
   end Update_Alternate_Bounds;
   
   -------------------
   -- Add_All_Edges --
   -------------------
   
   function Add_All_Edges
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) return Cells_Lists.List is
      
      All_Cells : Cells_Lists.List;
      Edges     : Cells_Lists.List; 
   begin
      for Cell of Cells loop
         Cells_Lists.Append (All_Cells, Cell);
      end loop;
      
      Edges := G.Get_All_Edges (Cells);
      for Cell of Edges loop
         Cells_Lists.Append (All_Cells, Cell);
      end loop;
      
      return All_Cells;
   end Add_All_Edges;
   
   -------------------
   -- Get_All_Edges --
   -------------------
   
   function Get_All_Edges
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) return Cells_Lists.List is
      
      use Cells_Lists;
      
      Edges       : Cells_Lists.List := Cells_Lists.Empty_List;
      Cell_Edges  : Cells_Lists.List := Cells_Lists.Empty_List;
      Children    : Cells_Lists.List := Cells_Lists.Empty_List;
      Child_Edges : Cells_Lists.List := Cells_Lists.Empty_List;
   begin
      if Cells /= Cells_Lists.Empty_List then
	 
         for Cell of Cells loop
	    
            Cell_Edges := Cell.Get_Edges_List;
            for Edge of Cell_Edges loop
               Cells_Lists.Append (Edges, Edge);
            end loop;
	    
            Children := Get_Children (G.Model, Cell);
            Child_Edges := G.Get_All_Edges (Children);
	    
            for Edge of Child_Edges loop
               Cells_Lists.Append (Edges, Edge);
            end loop;
         end loop;	    
      end if;
	 
      return Edges;
   end Get_All_Edges;
   
   ----------------------
   -- Update_Cell_Size --
   ----------------------
   
   procedure Update_Cell_Size
     (G               : access Graph_Record;
      Cell            : access Cell_Record'Class;
      Ignore_Children : Boolean := False) is
   begin
      G.Model.Begin_Update;
      begin
         G.Cell_Size_Updated (Cell, Ignore_Children);
         declare
            use Artics.Graph.Events.Update_Cell_Size_Events;
            Evt : access Update_Cell_Size_Event_Record := 
              New_Update_Cell_Size_Event (Cell, Ignore_Children);
         begin
            Fire_Event (G, Evt);
            Free_Update_Cell_Size_Event (Update_Cell_Size_Event_Ptr (Evt));
            -- fireEvent
            -- (new mxEventObject(mxEvent.UPDATE_CELL_SIZE, "cell", cell,
            -- 		"ignoreChildren", ignoreChildren));
         end;
	 
      exception
         when E : others =>
            Log_Exception 
              (Exception_Message (E, "Graphs.Update_Cell_Size"));
      end;
      G.Model.End_Update;
   end Update_Cell_Size;
   
   -----------------------
   -- Cell_Size_Updated --
   -----------------------
      
   procedure Cell_Size_Updated
     (G               : access Graph_Record;
      Cell            : access Cell_Record'Class;
      Ignore_Children : Boolean) is
         
      use Cells_Lists;
         
      Size       : Rectangle_Record;
      Geo        : access Cell_Geometry_Record'Class;
      State      : access Cell_State_Record'Class;
      Style      : Strings_Maps.Map;
      Bounds     : Rectangle_Record;
      Tr         : Point_Record;
      Scale      : Coordinate;
      Width      : Coordinate;
      Height     : Coordinate;
      Cells      : Cells_Lists.List;
      Rects      : Rectangles_Lists.List;
   begin
      if Cell /= null then
   	 
         G.Model.Begin_Update;
         begin
            Size := G.Get_Preferred_Size_For_Cell (Cell);
            Geo := G.Model.Get_Geometry (Cell);
   
            if Size /= No_Rectangle_Record and Geo /= null then
   	       
               Geo := Clone (Cell_Geometry_Ptr (Geo));
   
               if G.Is_Swimlane (Cell) then
                  State := G.View.Get_State (Cell);
   		  
                  if Is_True (Style, Artics.Graph.Names.STYLE_HORIZONTAL, True)
                  then
                     Geo.Set_Width (Get_Width (Size));
                  else
                     Geo.Set_Height (Get_Height (Size));
                  end if;
   
               else
                  Geo.Set_Width (Get_Width (Size));
                  Geo.Set_Height (Get_Height (Size));
               end if;
   
               if not Ignore_Children then
                  Bounds := G.View.Get_Bounds (Get_Children (G.Model, Cell));
   
                  if Bounds /= No_Rectangle_Record then
                     Tr := G.View.Get_Translate;
                     Scale := G.View.Get_Scale;
   
                     Width := (Get_X (Bounds) 
                               + Get_Width (Bounds)) 			
                       / Scale - Geo.Get_X - Get_X (Tr);
                     Height := (Get_Y (Bounds) + Get_Height (Bounds))
                       / Scale - Geo.Get_Y - Get_Y (Tr);
   
                     Geo.Set_Width (Maths.Max (Geo.Get_Width, Width));
                     Geo.Set_Height (Maths.Max (Geo.Get_Height, Height));
                  end if;
               end if;
   	       
               Cells.Append (Cell);
               Rects.Append (Geometry_Rectangle (Geo));
               G.Cells_Resized (Cells, Rects);
            end if;
         exception
            when E : others =>
               Log_Exception 
                 (Exception_Message (E, "Graphs.Cell_Size_Updated"));
         end;
         G.Model.End_Update;
      end if;
   end Cell_Size_Updated;
   
   ---------------------------------
   -- Get_Preferred_Size_For_Cell --
   ---------------------------------
      
   function Get_Preferred_Size_For_Cell
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Rectangle_Record is
         
      use Artics.Graph.Names;
      use Strings_Maps;
         
      Result  : Rectangle_Record := No_Rectangle_Record;
      State   : access Cell_State_Record'Class;
      Style   : Strings_Maps.Map;
      Dx      : Coordinate;
      Dy      : Coordinate;
      Spacing : Coordinate;
      Size    : Rectangle_Record;
      Width   : Coordinate;
      Height  : Coordinate;
      Value   : Name_Id;
      Tmp     : Coordinate;
      Gs2     : Coordinate;
   begin
      if Cell /= null then
         State := G.View.Get_State (Cell);
   	 
         if Style /= Strings_Maps.Empty_Map 
           and not G.Model.Is_Edge (Cell)
         then
            Dx := 0.0;
            Dy := 0.0;
   
            -- Adds dimension of image if shape is a label
   	    
            if G.Get_Image (State) /= ""
              or Get_String (Style, STYLE_IMAGE) /= No_Name
            then
               if Get_String (Style, STYLE_SHAPE, No_Name) = SHAPE_LABEL 
               then
                  if Get_String (Style, STYLE_VERTICAL_ALIGN, No_Name)
                    = ALIGN_MIDDLE 
                  then
                     Dx := Dx + Get_Double
                       (Style, 
                        STYLE_IMAGE_WIDTH, Coordinate (Default_Image_Size));
                  end if;
   		  
                  if Get_String (Style, STYLE_ALIGN, No_Name) = ALIGN_CENTER 
                  then
                     Dy := Dx + Get_Double
                       (Style,
                        STYLE_IMAGE_HEIGHT, Coordinate (Default_Image_Size));
                  end if;
               end if;
            end if;
   	       
            -- Adds spacings
            Spacing := Get_Double (Style, STYLE_SPACING);
            Dx := Dx + 2.0 * Spacing;
            Dx := Dx + Get_Double (Style, STYLE_SPACING_LEFT);
            Dx := Dx + Get_Double (Style, STYLE_SPACING_RIGHT);
   
            Dy := Dy + 2.0 * Spacing;
            Dy := Dy + Get_Double (Style, STYLE_SPACING_TOP);
            Dy := Dy + Get_Double (Style, STYLE_SPACING_BOTTOM);
   
            -- LATER: Add space for collapse/expand icon if applicable
   	    
            -- Adds space for label
            Value := G.Get_Label (Cell);
   
            if Value /= No_Name then
               Size := Get_Label_Size
                 (Value, Style, G.Is_Html_Label (Cell), 1.0);
               Width := Get_Width (Size) + Dx;
               Height := Get_Height (Size) + Dy;
   
               if not Is_True (Style, STYLE_HORIZONTAL, True) then
                  Tmp    := Height;
                  Height := Width;
                  Width  := Tmp;
               end if;
   
               if G.Grid_Enabled then
                  Width  := G.Snap (Width  + G.Grid_Size / 2.0);
                  Height := G.Snap (Height + G.Grid_Size / 2.0);
               end if;
   
               Result := New_Rectangle (0.0, 0.0, Width, Height);
            else
               Gs2 := 4.0 * G.Grid_Size;
               Result := New_Rectangle (0.0, 0.0, Gs2, Gs2);
            end if;
         end if;
      end if;
         
      return Result;
   end Get_Preferred_Size_For_Cell;
   
   -----------------
   -- Resize_Cell --
   -----------------
   
   function Resize_Cell
     (G      : access Graph_Record;
      Cell   : access Cell_Record'Class;
      Bounds : Rectangle_Record) return access Cell_Record'Class is
      
      Cells       : Cells_Lists.List;
      Bounds_List : Rectangles_Lists.List;
      Resized     : Cells_Lists.List;
   begin
      Cells_Lists.Append (Cells, Cell);
      Rectangles_Lists.Append (Bounds_List, Bounds);
      Resized := G.Resize_Cells (Cells, Bounds_List);
      return Cells_Lists.First_Element (Resized);
   end Resize_Cell;
   
   ------------------
   -- Resize_Cells --
   ------------------
   
   function Resize_Cells
     (G      : access Graph_Record;
      Cells  : Cells_Lists.List;
      Bounds : Rectangles_Lists.List) return Cells_Lists.List is
      
   begin
      G.Model.Begin_Update;
      begin
         G.Cells_Resized (Cells, Bounds);
         declare
            use Artics.Graph.Events.Rezise_Cells_Events;
            Evt : access Rezise_Cells_Event_Record := 
              New_Rezise_Cells_Event (Cells, Bounds);
         begin
            Fire_Event (G, Evt);
            Free_Rezise_Cells_Event (Rezise_Cells_Event_Ptr (Evt));
            --  fireEvent
            --  (new mxEventObject(mxEvent.RESIZE_CELLS, "cells", cells,
            --  				"bounds", bounds));
         end;
      exception
         when E : others =>
            Log_Exception 
              (Exception_Message (E, "Graphs.Resize_Cells"));
      end;
      G.Model.End_Update;
      
      return Cells;
   end Resize_Cells;
   
   -------------------
   -- Cells_Resized --
   -------------------
   
   procedure Cells_Resized
     (G      : access Graph_Record;
      Cells  : Cells_Lists.List;
      Bounds : Rectangles_Lists.List) is
      
      use Cells_Lists;
      use Rectangles_Lists;
      
      Tmp      : Rectangle_Record;
      Geo      : access Cell_Geometry_Record'Class;
      Cell     : access Cell_Record'Class;
      Offset   : Point_Record;
      Cell_Cur : Cells_Lists.Cursor;
      Rect_Cur : Rectangles_Lists.Cursor;
   begin
      if Cells /= Cells_Lists.Empty_List 
        and Bounds /= Rectangles_Lists.Empty_List 
        and Cells_Lists.Length (Cells) = Rectangles_Lists.Length (Bounds)
      then
         G.Model.Begin_Update;
         begin
            Cell_Cur := Cells_Lists.First (Cells);
            Rect_Cur := Rectangles_Lists.First (Bounds);
	    
            while Cells_Lists.Has_Element (Cell_Cur) 
              and Rectangles_Lists.Has_Element (Rect_Cur) 
            loop
               Tmp  := Rectangles_Lists.Element (Rect_Cur);
               Cell := Cells_Lists.Element (Cell_Cur);
               Geo  := G.Model.Get_Geometry (Cell);

               if Geo /= null
                 and then (Geo.Get_X /= Get_X (Tmp)
                           or Geo.Get_Y      /= Get_Y (Tmp)
                           or Geo.Get_Width  /= Get_Width (Tmp)
                           or Geo.Get_Height /= Get_Height (Tmp))
               then
                  Geo := Clone (Cell_Geometry_Ptr (Geo));

                  if Geo.Is_Relative then
                     Offset := Geo.Get_Offset;

                     if Offset /= No_Point_Record then
                        Set_X (Offset, Get_X (Offset) + Get_X (Tmp));
                        Set_Y (Offset, Get_Y (Offset) + Get_Y (Tmp));
                     end if;
                  else
                     Geo.Set_X (Get_X (Tmp));
                     Geo.Set_Y (Get_Y (Tmp));
                  end if;

                  Geo.Set_Width  (Get_Width  (Tmp));
                  Geo.Set_Height (Get_Height (Tmp));

                  if not Geo.Is_Relative and G.Model.Is_Vertex (Cell)
                    and not G.Is_Allow_Negative_Coordinates
                  then
                     Geo.Set_X (Maths.Max (0.0, Geo.Get_X));
                     Geo.Set_Y (Maths.Max (0.0, Geo.Get_Y));
                  end if;

                  G.Model.Set_Geometry (Cell, Geo);

                  if G.Is_Extend_Parent (Cell) then 
                     G.Extend_Parent (Cell);
                  end if;
		 
               end if;
	       
               Cells_Lists.Next (Cell_Cur);
               Rectangles_Lists.Next (Rect_Cur);
            end loop;

            if G.Is_Reset_Edges_On_Resize then
               G.Reset_Edges (Cells);
            end if;

            -- RENAME BOUNDSARRAY TO BOUNDS
	    
            declare
               use Artics.Graph.Events.Rezise_Cells_Events;
               Evt : access Rezise_Cells_Event_Record := 
                 New_Rezise_Cells_Event (Cells, Bounds);
            begin
               Fire_Event (G, Evt);
               Free_Rezise_Cells_Event (Rezise_Cells_Event_Ptr (Evt));
               --  fireEvent(new mxEventObject(mxEvent.CELLS_RESIZED, "cells",
               --  				cells, "bounds", bounds));
            end;
         exception
            when E : others =>
               Log_Exception 
                 (Exception_Message (E, "Graphs.Cells_Resized"));
         end;
         G.Model.End_Update;
      end if;
   end Cells_Resized;
   
   -------------------
   -- Extend_Parent --
   -------------------
   
   procedure Extend_Parent
     (G      : access Graph_Record;
      Cell   : access Cell_Record'Class) is
      
      Parent  : access Cell_Record'Class;
      P       : access Cell_Geometry_Record'Class;
      Geo     : access Cell_Geometry_Record'Class;
      Cells   : Cells_Lists.List;
      Rects   : Rectangles_Lists.List;
      Rect    : Rectangle_Record;
      Resized : Cells_Lists.List;
   begin
      if Cell /= null then
         Parent := G.Model.Get_Parent (Cell);
         P := G.Model.Get_Geometry (Parent);
      
         if Parent /= null and P /= null then
            Geo := G.Model.Get_Geometry (Cell);

            if Geo /= null
              and then (P.Get_Width < Geo.Get_X + Geo.Get_Width
                        or P.Get_Height < Geo.Get_Y + Geo.Get_Height)
            then
               Set_X (Rect, Get_X (P));
               Set_Y (Rect, Get_Y (P));

               Set_Width (Rect,
                          Maths.Max 
                            (P.Get_Width, Geo.Get_X + Geo.Get_Width));
               Set_Height (Rect,
                           Maths.Max
                             (P.Get_Height, Geo.Get_Y + Geo.Get_Height));
	       
	       
               Cells_Lists.Append (Cells, Parent);
               Rectangles_Lists.Append (Rects, Rect);
               Resized := G.Resize_Cells (Cells, Rects);
            end if;
         end if;
      end if;
   end Extend_Parent;
   
   ----------------
   -- Move_Cells --
   ----------------
   
   function Move_Cells
     (G        : access Graph_Record;
      Cells    : Cells_Lists.List;
      Dx       : Coordinate;
      Dy       : Coordinate;
      Clone    : Boolean := False;
      Target   : access Cell_Record'Class := null;
      Location : Point_Record := No_Point_Record)  return Cells_Lists.List is
      
      use Cells_Lists;
      
      Previous : Boolean;
      Trg      : access Cell_Record'Class := Target;
      Index    : Integer;
      Res      : Cells_Lists.List := Cells;
   begin
      Log_Line ("Move_Cells Begin");
      if Res /= Cells_Lists.Empty_List
        and (Dx /= 0.0 or Dy /= 0.0 or Clone or Target /= null)
      then
         G.Model.Begin_Update;
         begin
            if Clone then
               Res := G.Clone_Cells (Res, G.Is_Clone_Invalid_Edges);
	       
               if Trg = null then
                  Trg := G.Get_Default_Parent;
               end if;
            end if;

            -- Need to disable allowNegativeCoordinates if target not null to
            -- allow for temporary negative numbers until cellsAdded is called.
	    
            Previous := G.Is_Allow_Negative_Coordinates;

            if Trg /= null then
               G.Set_Allow_Negative_Coordinates (True);
            end if;
	    
            Log_Line ("Graph:Cells_Moved");
            G.Cells_Moved
              (Res, 
               Dx, Dy, 
               not Clone
               and G.Is_Disconnect_On_Move and G.Is_Allow_Dangling_Edges, 
               Trg = null);

            G.Set_Allow_Negative_Coordinates (Previous);

            if Trg /= null then
               Index := G.Model.Get_Child_Count (Trg);
               G.Cells_Added (Res, Trg, Index, null, null, True);
            end if;

            declare
               use Artics.Graph.Events.Move_Cells_Events;
               Evt : access Move_Cells_Event_Record := 
                 New_Move_Cells_Event
                   (Cells    => Res,
                    Dx       => Dx,
                    Dy       => Dy,
                    Clone    => Clone,
                    Target   => Target,
                    Location => Location);
            begin
               Fire_Event (G, Evt);
               Free_Move_Cells_Event (Move_Cells_Event_Ptr (Evt));
               --  fireEvent(new MxEventObject
               --  		(mxEvent.MOVE_CELLS, "cells", cells,
               --  	 "dx", dx, "dy", dy, "clone", clone, "target", target,
               --  		 "location", location));
            end;
         exception
            when E : others =>
               Log_Exception 
                 (Exception_Message (E, "Graphs.Move_Cells"));
         end;
         G.Model.End_Update;
      end if;
      
      Log_Line ("Move_Cells End");
      return Res;
   end Move_Cells;
   
   -----------------
   -- Cells_Moved --
   -----------------
   
   procedure Cells_Moved
     (G          : access Graph_Record;
      Cells      : Cells_Lists.List;
      Dx         : Coordinate;
      Dy         : Coordinate;
      Disconnect : Boolean;
      Constrain  : Boolean) is
      
      use Cells_Lists;
      
   begin
      Log_Line ("Cells_Moved Begin");
      if Cells /= Cells_Lists.Empty_List and (Dx /= 0.0 or Dy /= 0.0) then
	 
         G.Model.Begin_Update;
         begin
            if Disconnect then
               G.Disconnect_Graph (Cells);
            end if;
	    
            for Cell of Cells loop
               G.Translate_Cell (Cell, Dx, Dy);
	       
               if Constrain then
                  G.Constrain_Child (Cell);
               end if;
            end loop;

            if G.Is_Reset_Edges_On_Move then
               G.Reset_Edges (Cells);
            end if;

            declare
               use Artics.Graph.Events.Moved_Cells_Events;
               Evt : access Moved_Cells_Event_Record := 
                 New_Moved_Cells_Event (Cells, Dx, Dy, Disconnect);
            begin
               Fire_Event (G, Evt);
               Free_Moved_Cells_Event (Moved_Cells_Event_Ptr (Evt));
            end;
            --  fireEvent(new mxEventObject(mxEvent.CELLS_MOVED, "cells",
            --  				cells, "dx", dx, "dy", dy,
            --  				"disconnect", disconnect));
         exception
            when E : others =>
               Log_Exception 
                 (Exception_Message (E, "Graphs.Cells_Moved"));
         end;
         G.Model.End_Update;
      end if;
      
      Log_Line ("Cells_Moved End");
   end Cells_Moved;
   
   --------------------
   -- Translate_Cell --
   --------------------
   
   procedure Translate_Cell
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class;
      Dx   : Coordinate;
      Dy   : Coordinate) is
      
      Geo    : access Cell_Geometry_Record'Class;
      Offset : Point_Record;
   begin
      Log_Line ("Translate_Cell Begin");
      Log_Line ("   Dx = " & Dx'Img);
      Log_Line ("   Dy = " & Dy'Img);
      Geo := G.Model.Get_Geometry (Cell);

      if Geo /= null then
         -- Geo := Clone (Cell_Geometry_Ptr (Geo));
	 
         Log_Line (" Xs  = " & Geo.Get_X'Img);
         Log_Line (" Ys  = " & Geo.Get_Y'Img);
	 
         Geo.Translate (Dx, Dy);
	 
         if not Geo.Is_Relative 
           and G.Model.Is_Vertex (Cell)
           and not G.Is_Allow_Negative_Coordinates 
         then
            Geo.Set_X (Maths.Max (0.0, Geo.Get_X));
            Geo.Set_Y (Maths.Max (0.0, Geo.Get_Y));
         end if;
	 
         Log_Line (" Xe  = " & Geo.Get_X'Img);
         Log_Line (" Ye  = " & Geo.Get_Y'Img);
	 
         if Geo.Is_Relative and not G.Model.Is_Edge (Cell) then
            if Geo.Get_Offset = No_Point_Record then
               Geo.Set_Offset (New_Point (Dx, Dy));
            else
               Offset := Geo.Get_Offset;
	       
               Set_X (Offset, Get_X (Offset) + Dx);
               Set_Y (Offset, Get_Y (Offset) + Dy);
            end if;
         end if;
	 
         G.Model.Set_Geometry (Cell, Geo);
      end if;
      
      Log_Line ("Translate_Cell End");
   end Translate_Cell;
   
   -------------------------------
   -- Get_Cell_Containment_Area --
   -------------------------------
   
   function Get_Cell_Containment_Area
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Rectangle_Record is
      
      Parent : access Cell_Record'Class;
      Geo    : access Cell_Geometry_Record'Class;
      X      : Coordinate;
      Y      : Coordinate;
      W      : Coordinate;
      H      : Coordinate;
      Size   : Rectangle_Record;
   begin
      if Cell /= null and not G.Model.Is_Edge (Cell) then
         Parent := G.Model.Get_Parent (Cell);

         if Parent = G.Get_Default_Parent or Parent = G.Get_Current_Root then
            return G.Get_Maximum_Graph_Bounds;
	    
         elsif Parent /= null and Parent /= G.Get_Default_Parent then
            Geo := G.Model.Get_Geometry (Parent);

            if Geo /= null then
               X := 0.0;
               Y := 0.0;
               W := Geo.Get_Width;
               H := Geo.Get_Height;
	       
               if G.Is_Swimlane (Parent) then
                  Size := G.Get_Start_Size (Parent);
		  
                  X := Get_Width (Size);
                  W := W - Get_Width (Size);
                  Y := Get_Height (Size);
                  H := H - Get_Height (Size);
               end if;

               return New_Rectangle (X, Y, W, H);
            end if;
         end if;
      end if;

      return No_Rectangle_Record;
   end Get_Cell_Containment_Area;
   
   ------------------------------
   -- Get_Maximum_Graph_Bounds --
   ------------------------------
   
   function Get_Maximum_Graph_Bounds
     (G : access Graph_Record) return Rectangle_Record is
   begin
      return G.Maximum_Graph_Bounds;
   end Get_Maximum_Graph_Bounds;
   
   ------------------------------
   -- Set_Maximum_Graph_Bounds --
   ------------------------------
   
   procedure Set_Maximum_Graph_Bounds
     (G     : access Graph_Record;
      Value : Rectangle_Record) is
      
      Old_Value : Rectangle_Record;
   begin
      Old_Value := G.Maximum_Graph_Bounds;
      G.Maximum_Graph_Bounds := Value;
      
   end Set_Maximum_Graph_Bounds;
   
   ---------------------
   -- Constrain_Child --
   ---------------------
   
   procedure Constrain_Child
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) is
      
      
      Geo     : access Cell_Geometry_Record'Class;
      Area    : Rectangle_Record;
      Overlap : Coordinate;
   begin
      if Cell /= null then
         Geo := G.Model.Get_Geometry (Cell);
         if G.Is_Constrain_Child (Cell) then
            Area := G.Get_Cell_Containment_Area (Cell);
         else
            Area := G.Get_Maximum_Graph_Bounds;
         end if;

         if Geo /= null and Area /= No_Rectangle_Record then
	    
            -- Keeps child within the content area of the parent
	    
            if not Geo.Is_Relative 
              and (Geo.Get_X < Get_X (Area)
                   or Geo.Get_Y < Get_Y (Area)
                   or Get_Width (Area) < Geo.Get_X + Geo.Get_Width
                   or Get_Height (Area) < Geo.Get_Y + Geo.Get_Height)
            then
               Overlap := G.Get_Overlap (Cell);

               if Get_Width (Area) > 0.0 then
                  Geo.Set_X
                    (Maths.Min
                       (Geo.Get_X,
                        Get_X (Area) + Get_Width (Area) 
                        - (1.0 - Overlap) * Geo.Get_Width));
               end if;

               if Get_Height (Area) > 0.0 then
                  Geo.Set_Y
                    (Maths.Min
                       (Geo.Get_Y,
                        Get_Y (Area) + Get_Height (Area) 
                        - (1.0 - Overlap) * Geo.Get_Height));
               end if;

               Geo.Set_X
                 (Maths.Max
                    (Geo.Get_X, Get_X (Area) - Geo.Get_Width * Overlap));
	       
               Geo.Set_Y
                 (Maths.Max 
                    (Geo.Get_Y, Get_Y (Area) - Geo.Get_Height * Overlap));
            end if;
         end if;
      end if;
   end Constrain_Child;
   
   -----------------
   -- Reset_Edges --
   -----------------
   
   procedure Reset_Edges
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) is
      
      use Cells_Lists;
      
      Edges  : Cells_Lists.List;
      State  : access Cell_State_Record'Class;  
      Source : access Cell_Record'Class;
      Target : access Cell_Record'Class;
   begin
      Log_Line ("Reset_Edges Begin");
      if Cells /= Cells_Lists.Empty_List then
         --  // Prepares a hashtable for faster cell lookups
         --  HashSet<Object> set = new HashSet<Object>(Arrays.asList(cells));
	 
         G.Model.Begin_Update;
         begin
	    
            for C of Cells loop
	       
               Edges := Get_Edges (G.Model, C);

               if Edges /= Cells_Lists.Empty_List then 
		  
                  for Edge of Edges loop
                     State := G.View.Get_State (Edge);
		     
                     if State /= null then
                        Source := State.Get_Visible_Terminal (True);
                     else
                        Source := G.View.Get_Visible_Terminal (Edge, True);
                     end if;
		     
                     if State /= null then
                        Target := State.Get_Visible_Terminal (False);
                     else
                        Target := G.View.Get_Visible_Terminal (Edge, False);
                     end if;
		     
		     
                     -- Checks if one of the terminals is not in the given array
		     
                     if not Cells_Lists.Contains (Cells, Source) 
                       or not Cells_Lists.Contains (Cells, Target)
                     then
                        G.Reset_Edge (Edge);
                     end if;
                  end loop;
               end if;

               G.Reset_Edges (Get_Children (G.Model, C));
            end loop;
	    
         exception 
            when E : others =>
               Log_Exception 
                 (Exception_Message (E, "Graphs.Reset_Edges"));
         end;
         G.Model.End_Update;
      end if;
      
      Log_Line ("Reset_Edges End");
   end Reset_Edges;
   
   
   ----------------
   -- Reset_Edge --
   ----------------
   
   procedure Reset_Edge
     (G    : access Graph_Record;
      Edge : access Cell_Record'Class) is
      
      use Point_Lists;
      
      Geo    : access Cell_Geometry_Record'Class;
      Points : Point_Lists.List;
   begin
      Geo := G.Model.Get_Geometry (Edge);

      if Geo /= null then
         Geo.Set_Points (Point_Lists.Empty_List);
      end if;
   end Reset_Edge;

   
   -- Cell connecting and connection constraints --
   ------------------------------------------------
   
   ------------------------------------
   -- Get_All_Connection_Constraints --
   ------------------------------------
   
   function Get_All_Connection_Constraints
     (G        : access Graph_Record;
      Terminal : access Cell_Record'Class;
      Source   : Boolean) return access Connection_Constraint_Record'Class is
   begin
      return null;
   end Get_All_Connection_Constraints;
   
   -------------------------------
   -- Get_Connection_Constraint --
   -------------------------------
   
   function Get_Connection_Constraint
     (G        : access Graph_Record;
      Edge     : access Cell_State_Record'Class;
      Terminal : access Cell_State_Record'Class;
      Source   : Boolean) return access Connection_Constraint_Record'Class is
      
      use Artics.Graph.Names;
      
      Point     : Point_Record;
      Perimeter : Boolean;
      X         : Coordinate;
      Y         : Coordinate;
      Style     : Strings_Maps.Map;
   begin
      Point     := No_Point_Record;
      Perimeter := False;
            
      if Source then
         X := Get_Float (Style, STYLE_EXIT_X, No_Coordinate);
         Y := Get_Float (Style, STYLE_EXIT_Y, No_Coordinate);
      else
         X := Get_Float (Style, STYLE_ENTRY_X, No_Coordinate);
         Y := Get_Float (Style, STYLE_ENTRY_Y, No_Coordinate);
      end if;
      
      Point := Point_Record'(X,Y);
      
      if Point /= No_Point_Record then
         if Source then
            Perimeter := Is_True (Style, STYLE_EXIT_PERIMETER, True);
         else
            Perimeter := Is_True (Style, STYLE_ENTRY_PERIMETER, True);
         end if;
         --  else
         --  	 Point := Point_Record'(1.0, 1.0); -- Zero_Point_Record;
      end if;
      
      return New_Connection_Constraint (Point, Perimeter);	      
   end Get_Connection_Constraint;
   
   -------------------------------
   -- Set_Connection_Constraint --
   -------------------------------
   
   procedure Set_Connection_Constraint
     (G          : access Graph_Record;
      Edge       : access Cell_Record'Class;
      Terminal   : access Cell_Record'Class;
      Source     : Boolean;
      Constraint : access Connection_Constraint_Record'Class) is
      
      use Artics.Graph.Names;
      
      Cells : Cells_Lists.List;
   begin
      if Constraint /= null then
         G.Model.Begin_Update;
         begin
            Cells_Lists.Append (Cells, Edge);

         exception
            when E : others =>
               Log_Exception 
                 (Exception_Message (E, "Graphs.Set_Connection_Constraint"));
         end;
         G.Model.End_Update;
	       
      end if;
   end Set_Connection_Constraint;
   
   
   --------------------------
   -- Get_Connection_Point --
   --------------------------
   
   function Get_Connection_Point
     (G          : access Graph_Record;
      Vertex     : access Cell_State_Record'Class;
      Constraint : access Connection_Constraint_Record'Class) 
      return Point_Record is
      
      Point : Point_Record := No_Point_Record;
      Pt    : Point_Record := No_Point_Record;
   begin
      if Vertex /= null and Constraint.Point /= No_Point_Record then
         Pt :=  Constraint.Point;
         Point := New_Point 
           (Vertex.Get_X + Get_X (Pt) * Vertex.Get_Width, 
            Vertex.Get_Y + Get_Y (Pt) * Vertex.Get_Height);
      end if;

      if Point /= No_Point_Record and Constraint.Perimeter then
         Point := G.View.Get_Perimeter_Point (Vertex, Point, False);
      end if;

      return Point;
   end Get_Connection_Point;

   
   ------------------
   -- Connect_Cell --
   ------------------
   
   function Connect_Cell
     (G        : access Graph_Record;
      Edge     : access Cell_Record'Class;
      Terminal : access Cell_Record'Class;
      Source   : Boolean) return access Cell_Record'Class is
   begin
      return G.Connect_Cell (Edge, Terminal, Source, null);
   end Connect_Cell;
   
   ------------------
   -- Connect_Cell --
   ------------------
   
   function Connect_Cell
     (G          : access Graph_Record;
      Edge       : access Cell_Record'Class;
      Terminal   : access Cell_Record'Class;
      Source     : Boolean;
      Constraint : access Connection_Constraint_Record'Class) 
      return access Cell_Record'Class is
      
      Previous : access Cell_Record'Class;
   begin
      G.Model.Begin_Update;
      begin
         Previous := G.Model.Get_Terminal (Edge, Source);
         G.Cell_Connected (Edge, Terminal, Source, Constraint);
	 
         declare
            use Artics.Graph.Events.Connect_Cell_Events;
            Evt : access Connect_Cell_Event_Record :=
              New_Connect_Cell_Event (Edge, Terminal, Source, Previous);
         begin
            Fire_Event (G, Evt);
            Free_Connect_Cell_Event (Connect_Cell_Event_Ptr (Evt));
            --  fireEvent(new mxEventObject(mxEvent.CONNECT_CELL, "edge", edge,
            --  	     "terminal", terminal, "source", source,
            --  	     "previous", previous));
         end;
      exception
         when E : others =>
            Log_Exception 
              (Exception_Message (E, "Graphs.Connect_Cell"));
      end;
      G.Model.End_Update;
      
      return Edge;
   end Connect_Cell;
   
   
   --------------------
   -- Cell_Connected --
   --------------------
   
   procedure Cell_Connected
     (G          : access Graph_Record;
      Edge       : access Cell_Record'Class;
      Terminal   : access Cell_Record'Class;
      Source     : Boolean;
      Constraint : access Connection_Constraint_Record'Class) is
      
      use Artics.Graph.Names;
      
      Term     : access Cell_Record'Class := Terminal;
      Previous : access Cell_Record'Class;
      Id       : Name_Id;
      Key      : Name_Id;
      Edges    : Cells_Lists.List;
   begin
      if Edge /= null then
         G.Model.Begin_Update;
         begin
            Previous := G.Model.Get_Terminal (Edge, Source);

            -- Updates the constraint
            G.Set_Connection_Constraint (Edge, Term, Source, Constraint);

            -- Checks if the new terminal is a port, uses the ID of the port
            -- in the style and the parent of the port as the actual terminal
            -- of the edge.
	       
            -- Checks if the new terminal is a port
	       
            Id := No_Name;
	       
            if G.Is_Port (Terminal) and Term /= null then
               Id   := String_Find (Term.Get_Id);
               Term := G.Get_Terminal_For_Port (Term, Source);
            end if;

            -- Sets or resets all previous information for connecting to a
            -- child port
	       
            if Source then
               Key := STYLE_SOURCE_PORT;
            else
               Key := STYLE_TARGET_PORT;
            end if;
	       
            Cells_Lists.Append (Edges, Edge);
				  
            G.Model.Set_Terminal (Edge, Term, Source);
	    
            if G.Is_Reset_Edges_On_Connect then
               G.Reset_Edge (Edge);
            end if;
	    
            declare
               use Artics.Graph.Events.Connected_Cell_Events;
               Evt : access Connected_Cell_Event_Record := 
                 New_Connected_Cell_Event (Edge, Terminal, Source, Previous);
            begin
               Fire_Event (G, Evt);
               Free_Connected_Cell_Event (Connected_Cell_Event_Ptr (Evt));
               --  fireEvent(new mxEventObject(mxEvent.CELL_CONNECTED, "edge",
               --  		edge, "terminal", terminal, 
               --  		"source", source,
               --  		"previous", previous));
            end;
	    
         exception
            when E : others =>
               Log_Exception 
                 (Exception_Message (E, "Graphs.Cell_Connected"));
         end;
         G.Model.End_Update;
      end if;
   end Cell_Connected;

   
   ----------------------
   -- Disconnect_Graph --
   ----------------------
   
   procedure Disconnect_Graph
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) is
      
      use Cells_Lists;
      
      Scale  : Coordinate;
      Dx     : Coordinate;
      Dy     : Coordinate;
      Tr     : Point_Record;
      Pt     : Point_Record;
      Geo    : access Cell_Geometry_Record'Class;
      State  : access Cell_State_Record'Class;
      Pstate : access Cell_State_Record'Class; 
      Src    : access Cell_Record'Class;
      Trg    : access Cell_Record'Class;
   begin
      if Cells /= Cells_Lists.Empty_List then
         G.Model.Begin_Update;
         begin
            Scale := G.View.Get_Scale;
            Tr    := G.View.Get_Translate;

            -- Prepares a hashtable for faster cell lookups
	    
            for Cell of Cells loop
	       
               if G.Model.Is_Edge (Cell) then
                  Geo := G.Model.Get_Geometry (Cell);

                  if Geo /= null then
                     State := G.View.Get_State (Cell);
                     Pstate := G.View.Get_State (G.Model.Get_Parent (Cell));

                     if State /= null and Pstate /= null then
                        Geo := Clone (Cell_Geometry_Ptr (Geo));

                        Dx := -Get_X (Pstate.Get_Origin);
                        Dy := -Get_Y (Pstate.Get_Origin);

                        Src := G.Model.Get_Terminal (Cell, True);

                        if Src /= null 
                          and G.Is_Cell_Disconnectable (Cell, Src, True)
                        then
                           while Src /= null 
                             and not Cells_Lists.Contains (Cells, Src)
                           loop
                              Src := G.Model.Get_Parent (Src);
                           end loop;
									  
                           if Src = null then
                              Pt := State.Get_First_Absolute_Point;
                              Geo.Set_Terminal_Point 
                                (New_Point
                                   (Get_X (Pt) / Scale - Get_X (Tr) + Dx, 
                                    Get_Y (Pt) / Scale - Get_Y (Tr) + Dy), 
                                 True);
			      
                              G.Model.Set_Terminal (Cell, null, True);
                           end if;
                        end if;

                        Trg := G.Model.Get_Terminal (Cell, False);

                        if Trg /= null
                          and G.Is_Cell_Disconnectable (Cell, Trg, False)
                        then
                           while Trg /= null 
                             and not Cells_Lists.Contains (Cells, Trg)
                           loop
                              Trg := G.Model.Get_Parent (Trg);
                           end loop;

                           if Trg = null then
                              Pt := State.Get_Last_Absolute_Point;
                              Geo.Set_Terminal_Point
                                (New_Point
                                   (Get_X (Pt) / Scale - Get_X (Tr) + Dx, 
                                    Get_Y (Pt) / Scale - Get_Y (Tr) + Dy),
                                 False);
			      
                              G.Model.Set_Terminal (Cell, null, False);
                           end if;
                        end if;
                     end if;
		     
                     G.Model.Set_Geometry (Cell, Geo);
                  end if;
               end if;		  
            end loop;
         exception
            when E : others =>
               Log_Exception 
                 (Exception_Message (E, "Graphs.Disconnect_Graph"));
         end;
         G.Model.End_Update;
      end if;
   end Disconnect_Graph;
   
   
   --  Drilldown --
   ----------------
   
   ----------------------
   -- Get_Current_Root --
   ----------------------
   
   function Get_Current_Root
     (G : access Graph_Record) return access Cell_Record'Class is
   begin
      return G.View.Get_Current_Root;
   end Get_Current_Root;
   
   
   ----------------------------
   -- Get_Translate_For_Root --
   ----------------------------
   
   function Get_Translate_For_Root 
     (G : access Graph_Record;
      C : access Cell_Record'Class) return Point_Record is
   begin
      return No_Point_Record;
   end Get_Translate_For_Root;
   
   
   -------------
   -- Is_Port --
   -------------
   
   function Is_Port
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean is
   begin
      return False;
   end Is_Port;
   
   
   ---------------------------
   -- Get_Terminal_For_Port --
   ---------------------------
   
   function Get_Terminal_For_Port
     (G      : access Graph_Record;
      Cell   : access Cell_Record'Class;
      Source : Boolean) return access Cell_Record'Class is
   begin
      return G.Model.Get_Parent (Cell);
   end Get_Terminal_For_Port;
   
   
   -------------------------------
   -- Get_Child_Offset_For_Cell --
   -------------------------------
   
   function Get_Child_Offset_For_Cell
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Point_Record is
   begin
      return No_Point_Record;
   end Get_Child_Offset_For_Cell;
   
   
   -----------------
   -- Enter_Group --
   -----------------
   
   procedure Enter_Group
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class := null) is
      
      Local_Cell : access Cell_Record'Class := Cell;
   begin
      if Local_Cell = null then
         Local_Cell := G.Get_Selection_Cell;
      end if;

      if Local_Cell /= null and G.Is_Valid_Root (Local_Cell) then
         G.View.Set_Current_Root (Local_Cell);
         G.Clear_Selection;
      end if;
   end Enter_Group;
   
   
   ----------------
   -- Exit_Group --
   ----------------
   
   procedure Exit_Group (G : access Graph_Record) is
      
      Root    : access Cell_Record'Class;
      Current : access Cell_Record'Class;
      Nxt     : access Cell_Record'Class;
      State   : access Cell_State_Record'Class;
   begin
      Root    := G.Model.Get_Root;
      Current := G.Get_Current_Root;

      if Current /= null then
         Nxt := G.Model.Get_Parent (Current);

         -- Finds the next valid root in the hierarchy
	 
         while Nxt /= Root 
           and not G.Is_Valid_Root (Nxt)
           and G.Model.Get_Parent (Nxt) /= Root 
         loop
            Nxt := G.Model.Get_Parent (Nxt);
         end loop;

         -- Clears the current root if the new root is the model's root or one
         -- of the layers.
	 
         if Nxt = Root or G.Model.Get_Parent (Nxt) = Root then
            G.View.Set_Current_Root (null);
         else
            G.View.Set_Current_Root (Nxt);
         end if;

         State := G.View.Get_State (Current);

         -- Selects the previous root in the graph
	 
         if State /= null then
            G.Set_Selection_Cell (Current);
         end if;
      end if;
   end Exit_Group;
   
   
   ----------
   -- Home --
   ----------
   
   procedure Home (G : access Graph_Record) is
      
      Current : access Cell_Record'Class;
      State   : access Cell_State_Record'Class;
   begin
      Current := G.Get_Current_Root;

      if Current /= null then
         G.View.Set_Current_Root (null);
         State := G.View.Get_State (Current);

         if State /= null then
            G.Set_Selection_Cell (Current);
         end if;
      end if;
   end Home;
   
   
   -------------------
   -- Is_Valid_Root --
   -------------------
   
   function Is_Valid_Root
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean is
   begin
      return Cell /= null;
   end Is_Valid_Root;

   
   -- Graph display --
   -------------------
   
   ----------------------
   -- Get_Graph_Bounds --
   ----------------------
   
   function Get_Graph_Bounds (G : access Graph_Record)return Rectangle_Record is
   begin
      return G.View.Get_Graph_Bounds;
   end Get_Graph_Bounds;
   
   
   ---------------------
   -- Get_Cell_Bunds --
   ---------------------
   
   function Get_Cell_Bounds
     (G                   : access Graph_Record;
      Cell                : access Cell_Record'Class;
      Include_Edges       : Boolean := False;
      Include_Descendants : Boolean := False) return Rectangle_Record is
   begin
      return G.Get_Cell_Bounds
        (Cell, Include_Edges, Include_Descendants, False);
   end Get_Cell_Bounds;
   
   
   ------------------------------------
   -- Get_Bounding_Box_From_Geometry --
   ------------------------------------
   
   function Get_Bounding_Box_From_Geometry
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) return Rectangle_Record is
      
      use Cells_Lists;
      
      Result : Rectangle_Record := No_Rectangle_Record;
      Geo    : access Cell_Geometry_Record'Class;
   begin
      if Cells /= Cells_Lists.Empty_List then
	 
         for Cell of Cells loop
	    
            if G.Model.Is_Vertex (Cell) then
               Geo := G.Get_Cell_Geometry (Cell);

               if Result = No_Rectangle_Record then
                  Result := Geo.Geometry_Rectangle;
               else
                  Add (Result, Geo.Geometry_Rectangle);
               end if;
            end if;
         end loop;
      end if;

      return Result;
   end Get_Bounding_Box_From_Geometry;
   
   
   ----------------------
   -- Get_Bounding_Box --
   ----------------------
   
   function Get_Bounding_Box
     (G                   : access Graph_Record;
      Cell                : access Cell_Record'Class;
      Include_Edges       : Boolean := False;
      Include_Descendants : Boolean := False) return Rectangle_Record is
   begin
      return G.Get_Cell_Bounds
        (Cell, Include_Edges, Include_Descendants, True);
   end Get_Bounding_Box;
   
   
   ----------------------
   -- Get_Paint_Bounds --
   ----------------------
   
   function Get_Paint_Bounds
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) return Rectangle_Record is
   begin
      return G.Get_Bounds_For_Cells (Cells, False, True, True);
   end Get_Paint_Bounds;
   
   
   --------------------------
   -- Get_Bounds_For_Cells --
   --------------------------
   
   function Get_Bounds_For_Cells
     (G                   : access Graph_Record;
      Cells               : Cells_Lists.List;
      Include_Edges       : Boolean;
      Include_Descendants : Boolean;
      Bounding_Box        : Boolean) return Rectangle_Record is
      
      use Cells_Lists;
      
      Result : Rectangle_Record := No_Rectangle_Record;
      Tmp    : Rectangle_Record;
   begin
      if Cells /= Cells_Lists.Empty_List then
	 
         for Cell of Cells loop
            Tmp := G.Get_Cell_Bounds
              (Cell, Include_Edges, Include_Descendants, Bounding_Box);

            if Tmp /= No_Rectangle_Record then
               if Result = No_Rectangle_Record then
                  Result := Tmp;
               else
                  Add (Result, Tmp);
               end if;
            end if;
         end loop;
      end if;

      return Result;
   end Get_Bounds_For_Cells;
   
   
   ---------------------
   -- Get_Cell_Bounds --
   ---------------------
   
   function Get_Cell_Bounds
     (G                   : access Graph_Record;
      Cell                : access Cell_Record'Class;
      Include_Edges       : Boolean;
      Include_Descendants : Boolean;
      Bounding_Box        : Boolean) return Rectangle_Record is
      
      Cells     : Cells_Lists.List;
      --  All_Cells : Cells_Sets.Map;
      --  Edges     : Cells_Sets.Map;
      --  Tmp       : Cells_Sets.Map;
      --  Cell8edges : Cells_Lists.List;
      Result : Rectangle_Record;
      Tmp    : Rectangle_Record;
      Childs : Cells_Lists.List;
   begin
      -- Recursively includes connected edges
        
      --  if G.Include_Edges then
  	 
      --     All_Cells.Append (Cell);
	   
      --     Cell_Edges := This.Get_Edges (Cell);
	   
      --     for E of Cell_Edges loop
      --        Cells_Sets.Insert (Edges, E, True);
      --     end loop;
	   
      --     while not Cells_Sets.Is_Empty (Edges) loop
	      
      --        for E of Cell_Edges loop
      --  	 if Cells_Sets.Conatins (All_Cells, E) then
      --  	    Cells_Sets.Insert (All_Cells, E, True);
      --  	 end if;
      --        end loop;
	      
      --        for E of 
      --  				Iterator<Object> it = edges.iterator();
      --  
      --  				while (it.hasNext())
      --  				{
      --  					Object edge = it.next();
      --  					tmp.addAll(Arrays.asList(getEdges(edge)));
      --  				}
      --  
      --  				edges = tmp;
      --  			}
      --  
      --  			cells = allCells.toArray();
      --  		}
      --  		else
      --  		{
      Cells_Lists.Append (Cells, Cell);
      
      Result := G.Get_View.Get_Bounds (Cells, Bounding_Box);
      
      -- Recursively includes the bounds of the children
      
      if Include_Descendants then
	 
         for C of Cells loop
            Childs := Cell.Get_Children_List;
	    
            for Child of Childs loop
               Tmp := G.Get_Cell_Bounds 
                 (Child, Include_Edges, True,Bounding_Box);

               if Result /= No_Rectangle_Record then
                  Add (Result, Tmp);
               else
                  Result := Tmp;
               end if;
            end loop;
         end loop;
      end if;
      
      return Result;
   end Get_Cell_Bounds;
   
   
   -------------
   -- Refresh --
   -------------
   
   procedure Refresh (G : access Graph_Record) is
   begin
      G.View.Reload;
      G.Repaint;
   end Refresh;
   
   
   -------------
   -- Repaint --
   -------------
   
   procedure Repaint (G : access Graph_Record) is
   begin
      G.Repaint (No_Rectangle_Record);
   end Repaint;
   
   
   -------------
   -- Repaint --
   -------------
   
   procedure Repaint 
     (G      : access Graph_Record;
      Region : Rectangle_Record) is
      
      use Artics.Graph.Events.Repaint_Events;
      Evt : access Repaint_Event_Record := New_Repaint_Event (Region);
   begin
      Fire_Event (G, Evt);
      Free_Repaint_Event (Repaint_Event_Ptr (Evt));
      -- fireEvent(new mxEventObject(mxEvent.REPAINT, "region", region));
   end Repaint;
   
   
   ----------
   -- Snap --
   ----------
   
   function Snap 
     (G     : access Graph_Record;
      Value : Coordinate) return Coordinate is
      
      Val : Coordinate := Value;
   begin
      if G.Grid_Enabled then
         Val := Maths.Round (Val / G.Grid_Size) * G.Grid_Size;
      end if;

      return Val;
   end Snap;
   
   -----------------------
   -- Get_Cell_Geometry --
   -----------------------
   
   function Get_Cell_Geometry
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) 
      return access Cell_Geometry_Record'Class is
   begin
      return G.Model.Get_Geometry (Cell);
   end Get_Cell_Geometry;
   
   -------------------
   -- Is_Orthogonal --
   -------------------
   
   function Is_Orthogonal
     (G    : access Graph_Record;
      Edge : access Cell_State_Record'Class) return Boolean is
      
      Tmp : Name_Id;
   begin
    
      Tmp := G.View.Get_Edge_Style (Edge, Point_Lists.Empty_List, null, null);

      return Tmp = Artics.Graph.Names.Segment_Connector_Function_Name
        or Tmp = Artics.Graph.Names.Elbow_Connector_Function_Name
        or Tmp = Artics.Graph.Names.Side_To_Side_Connector_Function_Name
        or Tmp = Artics.Graph.Names.Top_To_Bottom_Connector_Function_Name
        or Tmp = Artics.Graph.Names.Entity_Relation_Connector_Function_Name
        or Tmp = Artics.Graph.Names.Orth_Connector_Function_Name;
   end Is_Orthogonal;
   
   
   -------------
   -- Is_Loop --
   -------------
   
   function Is_Loop
     (G     : access Graph_Record;
      State : access Cell_State_Record'Class) return Boolean is
      
      Src : access Cell_State_Record'Class;
      Trg : access Cell_State_Record'Class;
   begin
      Src := State.Get_Visible_Terminal_State (True);
      Trg := State.Get_Visible_Terminal_State (False);

      return Src /= null and then Src = Trg;
   end Is_Loop;

   
   -- Cell validation --
   ---------------------
   
   ------------------------
   -- Set_Multiplicities --
   ------------------------
   
   procedure Set_Multiplicities
     (G     : access Graph_Record;
      Value : Multiplicties_Lists.List) is
   begin
      null;
   end Set_Multiplicities;
   
   
   ------------------------
   -- Get_Multiplicities --
   ------------------------
   
   function Get_Multiplicities 
     (G : access Graph_Record) return Multiplicties_Lists.List is
   begin
      return Multiplicties_Lists.Empty_List;
   end Get_Multiplicities;
   
   
   -------------------
   -- Is_Edge_Valid --
   -------------------
   
   function Is_Edge_Valid
     (G      : access Graph_Record;
      Edge   : access Cell_Record'Class;
      Source : access Cell_Record'Class;
      Target : access Cell_Record'Class) return Boolean is
   begin
      return False;
   end Is_Edge_Valid;
   
   
   -------------------------------
   -- Get_Edge_Validation_Error --
   -------------------------------
   
   function Get_Edge_Validation_Error
     (G      : access Graph_Record;
      Edge   : access Cell_Record'Class;
      Source : access Cell_Record'Class;
      Target : access Cell_Record'Class) return String is
   begin
      return "";
   end Get_Edge_Validation_Error;
   
   
   -------------------
   -- Validate_Edge --
   -------------------
   
   function Validate_Edge
     (G      : access Graph_Record;
      Edge   : access Cell_Record'Class;
      Source : access Cell_Record'Class;
      Target : access Cell_Record'Class) return String is
   begin
      return "";
   end Validate_Edge;
   
   
   -------------------------------
   -- Get_Cell_Validation_Error --
   -------------------------------
   
   function Get_Cell_Validation_Error
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return String is
   begin
      return "";
   end Get_Cell_Validation_Error;
   
   
   -------------------
   -- Validate_Cell --
   -------------------
   
   function Validate_Cell
     (G       : access Graph_Record;
      Cell    : access Cell_Record'Class;
      Context : Cells_Maps.Map) return String is
   begin
      return "";
   end Validate_Cell;
   
   
   --------------------
   -- Labels_Visible --
   --------------------
   
   function Is_Labels_Visible (G : access Graph_Record) return Boolean is
   begin
      return G.Labels_Visible;
   end Is_Labels_Visible;
   
   
   ------------------------
   -- Set_Labels_Visible --
   ------------------------
   
   procedure Set_Labels_Visible
     (G : access Graph_Record;
      V : Boolean) is
   begin
      G.Labels_Visible := V;
   end Set_Labels_Visible;
   
   
   -----------------
   -- Html_Labels --
   -----------------
   
   function Is_Html_Labels (G : access Graph_Record) return Boolean is
   begin
      return G.Html_Labels;
   end Is_Html_Labels;
   
   
   ---------------------
   -- Set_Html_Labels --
   ---------------------
   
   procedure Set_Html_Labels
     (G : access Graph_Record;
      V : Boolean) is
   begin
      G.Html_Labels := V;
   end Set_Html_Labels;
   
   
   -----------------------------
   -- Convert_Value_To_String --
   -----------------------------
   
   function Convert_Value_To_String
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return String 
   is
   begin
      return Get_String (Convert_Value_To_Name (G, Cell));
   end Convert_Value_To_String;
   
   
   ---------------------------
   -- Convert_Value_To_Name --
   ---------------------------
   
   function Convert_Value_To_Name
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Name_Id is
      
      Value       : access Object_Record'Class;
      Named_Value : access Named_Object_Record;
   begin
      Value := Cell.Get_Value;
      if Value = null then
         Log_Line ("Convert_Value_To_Name No Name");
         return No_Name;
      else 
         Named_Value := Named_Object_Ptr (Value);
         Log_Line ("Convert_Value_To_Name " & Named_Value.Get_Name);
         return Named_Value.Get_Name;
      end if;
   end Convert_Value_To_Name;
   
   ---------------
   -- Get_Label --
   ---------------
   
   function Get_Label 
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return String 
   is
      State   : access Cell_State_Record'Class;
      Style   : Strings_Maps.Map;
      Nolabel : Boolean;
      
      --Label   : Name_Id;
   begin
      if Cell /= null then
         State := G.View.Get_State (Cell);
         
         Nolabel := Utils.Is_True 
           (Dict          => Style,
            Key           => Artics.Graph.Names.STYLE_NOLABEL,
            Default_Value => False);
	 
         Log_Line ("Nolabel is " & Nolabel'Img);
         Log_Line ("Visible is " & G.Labels_Visible'Img);
         if G.Labels_Visible and then (not Nolabel) then
            return G.Convert_Value_To_String (Cell);    
         end if;
      end if;
      
      return "";
   end Get_Label;
   
   
   ---------------
   -- Get_Label --
   ---------------
   
   function Get_Label 
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Name_Id is
      
      use Artics.Graph.Names;
   begin
      return String_Find (G.Get_Label(Cell));
   end Get_Label;
   
   
   ------------------------
   -- Cell_Label_Changed --
   ------------------------
   
   procedure Cell_Label_Changed
     (G         : access Graph_Record;
      Cell      : access Cell_Record'Class;
      Value     : access Object_Record'Class;
      Auto_Size : Boolean) 
   is
   begin 
      G.Model.Begin_Update;
      begin
         G.Get_Model.Set_Value
           (Cell  => Cell, Value => Value);
         if Auto_Size then
            G.Cell_Size_Updated
              (Cell            => Cell,
               Ignore_Children => False);
         end if;
      exception
         when E : others =>
            Log_Exception 
              (Exception_Message (E, "Graphs.Cell_Label_Changed"));
      end;
      G.Model.End_Update;
   end Cell_Label_Changed;
   
   
   -------------------
   -- Is_Html_Label --
   -------------------
   
   function Is_Html_Label
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean is
   begin 
      return G.Html_Labels;
   end Is_Html_Label;
   
   
   --------------------------
   -- Get_Tooltip_For_Cell --
   --------------------------
   
   function Get_Tooltip_For_Cell
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return String is
   begin 
      return G.Convert_Value_To_String (Cell);
   end Get_Tooltip_For_Cell;
   
   
   --------------------
   -- Get_Start_Size --
   --------------------
   
   function Get_Start_Size
     (G         : access Graph_Record;
      Swim_Lane : access Cell_Record'Class) return Rectangle_Record is
   begin 
      return No_Rectangle_Record;
   end Get_Start_Size;
   
   
   ---------------
   -- Get_Image --
   ---------------
   
   function Get_Image
     (G     : access Graph_Record;
      State : access Cell_State_Record'Class) return String is
   begin 
      return "";
   end Get_Image;
   
   
   ------------
   -- Border --
   ------------
   
   function Get_Border (G : access Graph_Record) return Integer is
   begin
      return G.Border;
   end Get_Border;
   
   
   ----------------
   -- Set_Border --
   ----------------
   
   procedure Set_Border
     (G : access Graph_Record;
      V : Integer) is
   begin
      G.Border := V;
   end Set_Border;
   
   -----------------
   -- Is_Swimlane --
   -----------------
   
   function Is_Swimlane
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean is
   begin
      return False;
   end Is_Swimlane;
   
   
   --------------------
   -- Is_Cell_Locked --
   --------------------
   
   function Is_Cell_Locked 
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean is
      
      Geometry : access Cell_Geometry_Record'Class;
   begin
      Geometry := G.Model.Get_Geometry (Cell);

      return G.Is_Cells_Locked
        or (Geometry /= null
            and then (G.Model.Is_Vertex (Cell) and Geometry.Is_Relative));
   end Is_Cell_Locked;
   
   
   ---------------------
   -- Is_Cells_Locked --
   ---------------------
   
   function Is_Cells_Locked (G : access Graph_Record) return Boolean is
   begin
      return G.Cells_Locked;
   end Is_Cells_Locked;
   
   
   ----------------------
   -- Set_Cells_Locked --
   ----------------------
   
   procedure Set_Cells_Locked
     (G : access Graph_Record;
      L : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Cells_Locked;
      G.Cells_Locked := L;
     
   end Set_Cells_Locked;
   
   
   ----------------------
   -- Is_Cell_Editable --
   ----------------------
   
   function Is_Cell_Editable
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean 
   is      
      State : access Cell_State_Record'Class;
      Style : Strings_Maps.Map;
   begin
      State := G.View.Get_State (Cell);
    
      return G.Is_Cells_Editable and not G.Is_Cell_Locked (Cell)
        and Is_True (Style, Artics.Graph.Names.STYLE_EDITABLE, True);
   end Is_Cell_Editable;
   
   
   --------------------
   -- Cells_Editable --
   --------------------
   
   function Is_Cells_Editable (G : access Graph_Record) return Boolean is
   begin
      return G.Cells_Editable;
   end Is_Cells_Editable;
   
   
   ------------------------
   -- Set_Cells_Editable --
   ------------------------
   
   procedure Set_Cells_Editable
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Cells_Editable;
      G.Cells_Editable := V;
   end Set_Cells_Editable;
   
   --------------------
   -- Cell_Resizable --
   --------------------
   
   function Is_Cell_Resizable   
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean is
      
      State : access Cell_State_Record'Class;
      Style : Strings_Maps.Map;
   begin
      State := G.View.Get_State (Cell);
     
      return G.Is_Cells_Resizable and not G.Is_Cell_Locked (Cell)
        and Is_True(Style, Artics.Graph.Names.STYLE_RESIZABLE, True);
   end Is_Cell_Resizable;
   
   
   ---------------------
   -- Cells_Resizable --
   ---------------------
   
   function Is_Cells_Resizable (G : access Graph_Record) return Boolean is
   begin
      return G.Cells_Resizable;
   end Is_Cells_Resizable;
   
   
   -------------------------
   -- Set_Cells_Resizable --
   -------------------------
   
   procedure Set_Cells_Resizable
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Cells_Resizable;
      G.Cells_Resizable := V;

      --  changeSupport.firePropertyChange("cellsResizable", oldValue,
      --  				       cellsResizable);
   end Set_Cells_Resizable;
   
   
   -------------------
   -- Cells_Movable --
   -------------------
   
   function Get_Movable_Cells 
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) return Cells_Lists.List is
      
      Movables : Cells_Lists.List;
   begin
      for C of Cells loop
         if G.Is_Cell_Movable (C) then
            Cells_Lists.Append (Movables, C);
         end if;
      end loop;
      
      return Movables;
   end Get_Movable_Cells;
   
   
   ---------------------
   -- Is_Cell_Movable --
   ---------------------
   
   function Is_Cell_Movable
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean is
      
      State   : access Cell_State_Record'Class;
      Style   : Strings_Maps.Map;
      Movable : Boolean;
      C       : Name_Id;
   begin
      State := G.View.Get_State (Cell); 
      
      if Strings_Maps.Contains (Style, Artics.Graph.Names.STYLE_MOVABLE) then
         C := Strings_Maps.Element (Style, Artics.Graph.Names.STYLE_MOVABLE);
      end if;
      Movable := Is_True (Style, Artics.Graph.Names.STYLE_MOVABLE, True);
      
      return G.Is_Cells_Movable 
        and not G.Is_Cell_Locked (Cell) 
        and Movable;
   end Is_Cell_Movable;
   
   
   ----------------------
   -- Is_Cells_Movable --
   ----------------------
   
   function Is_Cells_Movable (G : access Graph_Record) return Boolean is
   begin
      return G.Cells_Movable;
   end Is_Cells_Movable;
   
   
   -----------------------
   -- Set_Cells_Movable --
   -----------------------
   
   procedure Set_Cells_Movable
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Cells_Movable;
      G.Cells_Movable := V;

      --  changeSupport
      --  	.firePropertyChange("cellsMovable", oldValue, cellsMovable);
   end Set_Cells_Movable;
   
   
   -------------------------------
   -- Is_Terminal_Point_Movable --
   -------------------------------
   
   function Is_Terminal_Point_Movable
     (G      : access Graph_Record;
      Cell   : access Cell_Record'Class;
      Source : Boolean) return Boolean is
   begin
      return True;
   end Is_Terminal_Point_Movable;
   
   
   --------------------
   -- Cells_Bendable --
   --------------------
   
   function Is_Cell_Bendable
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean is
      
      State : access Cell_State_Record'Class;
      Style : Strings_Maps.Map;
   begin
      State := G.View.Get_State (Cell);
      
      return G.Is_Cells_Bendable and not G.Is_Cell_Locked (Cell)
        and Is_True (Style, Artics.Graph.Names.STYLE_BENDABLE, True);
   end Is_Cell_Bendable;
   
   
   --------------------
   -- Cells_Bendable --
   --------------------
   
   function Is_Cells_Bendable (G : access Graph_Record) return Boolean is
   begin
      return G.Cells_Bendable;
   end Is_Cells_Bendable;
   
   
   ------------------------
   -- Set_Cells_Bendable --
   ------------------------
   
   procedure Set_Cells_Bendable
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Cells_Bendable;
      G.Cells_Bendable := V;

      --  changeSupport.firePropertyChange("cellsBendable", oldValue,
      --  				       cellsBendable);
   end Set_Cells_Bendable;
   
   
   ---------------------
   -- Cell_Selectable --
   ---------------------
   
   function Is_Cell_Selectable
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean is
   begin
      return G.Is_Cells_Selectable;
   end Is_Cell_Selectable;
   
   
   ----------------------
   -- Cells_Selectable --
   ----------------------
   
   function Is_Cells_Selectable (G : access Graph_Record) return Boolean is
   begin
      return G.Cells_Selectable;
   end Is_Cells_Selectable;
   
   
   --------------------------
   -- Set_Cells_Selectable --
   --------------------------
   
   procedure Set_Cells_Selectable
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Cells_Selectable;
      G.Cells_Selectable := V;

      --  changeSupport.firePropertyChange("cellsSelectable", oldValue,
      --  				       cellsSelectable);
   end Set_Cells_Selectable;
   
   
   ---------------------
   -- Cells_Deletable --
   ---------------------
   
   function Get_Deletable_Cells
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) return Cells_Lists.List is
      
      type Deletable_Filter_Record is
        new Object_Record and Filter_Interface with null record;
      
      function Filter
        (F    : access Deletable_Filter_Record;
         Cell : access Cell_Record'Class) return Boolean;
      
      function Filter
        (F    : access Deletable_Filter_Record;
         Cell : access Cell_Record'Class) return Boolean is
      begin
         return Is_Cell_Deletable (G, Cell);
      end Filter;
      
      Deletable_Filter : aliased Deletable_Filter_Record := 
        Deletable_Filter_Record'(No_Object_Record with null record);
   begin
      return Filter_Cells (G.Model, Cells, Deletable_Filter'Access); 
      --  return mxGraphModel.filterCells(cells, new Filter()
      --  {
      --  	public boolean filter(Object cell)
      --  	{
      --  		return isCellDeletable(cell);
      --  	}
      --  });
		
   end Get_Deletable_Cells;
   
   
   -----------------------
   -- Is_Cell_Deletable --
   -----------------------
   
   function Is_Cell_Deletable
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean is
      
      State : access Cell_State_Record'Class;
      Style : Strings_Maps.Map;
   begin
      State := G.View.Get_State (Cell);
      
      return G.Is_Cells_Deletable
        and Is_True (Style, Artics.Graph.Names.STYLE_DELETABLE, True);
   end Is_Cell_Deletable;
   
   
   ------------------------
   -- Is_Cells_Deletable --
   ------------------------
   
   function Is_Cells_Deletable (G : access Graph_Record) return Boolean is
   begin
      return G.Cells_Deletable;
   end Is_Cells_Deletable;
   
   
   -------------------------
   -- Set_Cells_Deletable --
   -------------------------
   
   procedure Set_Cells_Deletable
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Cells_Deletable;
      G.Cells_Deletable := V;

      --  changeSupport.firePropertyChange("cellsDeletable", oldValue,
      --  				       cellsDeletable);
   end Set_Cells_Deletable;
   
   
   ---------------------
   -- Cells_Cloneable --
   ---------------------
   
   function Get_Cloneable_Cells 
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) return Cells_Lists.List is
      
      type Cloneable_Filter_Record is
        new Object_Record and Filter_Interface with null record;
      
      function Filter
        (F    : access Cloneable_Filter_Record;
         Cell : access Cell_Record'Class) return Boolean;
      
      function Filter
        (F    : access Cloneable_Filter_Record;
         Cell : access Cell_Record'Class) return Boolean is
      begin
         return Is_Cell_Cloneable (G, Cell);
      end Filter;
      
      Cloneable_Filter : aliased Cloneable_Filter_Record := 
        Cloneable_Filter_Record'(No_Object_Record with null record);
   begin
      return Filter_Cells (G.Model, Cells, Cloneable_Filter'Access); 
      --  return mxGraphModel.filterCells(cells, new Filter()
      --  {
      --  	public boolean filter(Object cell)
      --  	{
      --  		return isCellCloneable(cell);
      --  	}
      --  });
   end Get_Cloneable_Cells;
   
   
   -----------------------
   -- Is_Cell_Cloneable --
   -----------------------
   
   function Is_Cell_Cloneable
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean is
      
      State : access Cell_State_Record'Class;
      Style : Strings_Maps.Map;
   begin
      State := G.View.Get_State (Cell);
      
      return G.Is_Cells_Cloneable
        and Is_True (Style, Artics.Graph.Names.STYLE_CLONEABLE, True);
   end Is_Cell_Cloneable;
   
   
   ------------------------
   -- Is_Cells_Cloneable --
   ------------------------
   
   function Is_Cells_Cloneable (G : access Graph_Record) return Boolean is
   begin
      return G.Cells_Cloneable;
   end Is_Cells_Cloneable;
   
   
   -------------------------
   -- Set_Cells_Cloneable --
   -------------------------
   
   procedure Set_Cells_Cloneable
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Cells_Cloneable;
      G.Cells_Cloneable := V;

      --  changeSupport.firePropertyChange("cellsCloneable", oldValue,
      --  				cellsCloneable);
   end Set_Cells_Cloneable;
   
   
   ----------------------------
   -- Is_Cell_Disconnectable --
   ----------------------------
   
   function Is_Cell_Disconnectable
     (G        : access Graph_Record;
      Cell     : access Cell_Record'Class;
      Terminal : access Cell_Record'Class;
      Source   : Boolean) return Boolean is
   begin
      return G.Is_Cells_Disconnectable and not G.Is_Cell_Locked (Cell);
   end Is_Cell_Disconnectable;
   
   
   --------------------------
   -- Cells_Disconnectable --
   --------------------------
   
   function Is_Cells_Disconnectable (G : access Graph_Record) return Boolean is
   begin
      return G.Cells_Disconnectable;
   end Is_Cells_Disconnectable;
   
   
   ------------------------------
   -- Set_Cells_Disconnectable --
   ------------------------------
   
   procedure Set_Cells_Disconnectable
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Cells_Disconnectable;
      G.Cells_Disconnectable := V;

      --  changeSupport.firePropertyChange("cellsDisconnectable", oldValue,
      --  				cellsDisconnectable);
   end Set_Cells_Disconnectable;
   
   
   --------------------
   -- Labels_Clipped --
   --------------------
   
   function Is_Label_Clipped
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean is
      
      use Strings_Maps;
      
      State : access Cell_State_Record'Class;
      Style : Strings_Maps.Map;
   begin
      if not G.Is_Labels_Clipped then
         State := G.View.Get_State (Cell);
         return False;
      end if;
      
      return G.Is_Labels_Clipped;
   end Is_Label_Clipped;
   
   
   --------------------
   -- Labels_Clipped --
   --------------------
   
   function Is_Labels_Clipped (G : access Graph_Record) return Boolean is
   begin
      return G.Labels_Clipped;
   end Is_Labels_Clipped;
   
   
   ------------------------
   -- Set_Labels_Clipped --
   ------------------------
   
   procedure Set_Labels_Clipped
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Labels_Clipped;
      G.Labels_Clipped := V;

      --  changeSupport.firePropertyChange("labelsClipped", oldValue,
      --  				labelsClipped);
   end Set_Labels_Clipped;
   
   
   ----------------------
   -- Is_Label_Movable --
   ----------------------
   
   function Is_Label_Movable
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean is
   begin
      return not G.Is_Cell_Locked (Cell)
        and ((G.Model.Is_Edge (Cell) and G.Is_Edge_Labels_Movable)
             or (G.Model.Is_Vertex (Cell) and G.Is_Vertex_Labels_Movable));
   end Is_Label_Movable;
   
   
   --------------------------
   -- Vertex_Label_Movable --
   --------------------------
   
   function Is_Vertex_Labels_Movable (G : access Graph_Record) return Boolean is
   begin
      return G.Vertex_Labels_Movable;
   end Is_Vertex_Labels_Movable;
   
   
   ------------------------------
   -- Set_Vertex_Labels_Movable --
   ------------------------------
   
   procedure Set_Vertex_Labels_Movable
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Vertex_Labels_Movable;
      G.Vertex_Labels_Movable := V;

      --  changeSupport.firePropertyChange("vertexLabelsMovable", oldValue,
      --  				vertexLabelsMovable);
   end Set_Vertex_Labels_Movable;
   
   
   ------------------------
   -- Edge_Label_Movable --
   ------------------------
   
   function Is_Edge_Labels_Movable (G : access Graph_Record) return Boolean is
   begin
      return G.Edge_Labels_Movable;
   end Is_Edge_Labels_Movable;
   
   
   ----------------------------
   -- Set_Edge_Label_Movable --
   ----------------------------
   
   procedure Set_Edge_Labels_Movable
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Edge_Labels_Movable;
      G.Edge_Labels_Movable := V;

      --  changeSupport.firePropertyChange("edgeLabelsMovable", oldValue,
      --  				edgeLabelsMovable);
   end Set_Edge_Labels_Movable;
   
   
   -------------
   -- Enabled --
   -------------
   
   function Is_Enabled (G : access Graph_Record) return Boolean is
   begin
      return G.Enabled;
   end Is_Enabled;
   
   
   -----------------
   -- Set_Enabled --
   -----------------
   
   procedure Set_Enabled
     (G : access Graph_Record;
      E : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Enabled;
      G.Enabled := E;

      --changeSupport.firePropertyChange("enabled", oldValue, enabled);
   end Set_Enabled;
   
   
   ------------------
   -- Drop_Enabled --
   ------------------
   
   function Is_Drop_Enabled (G : access Graph_Record) return Boolean is
   begin
      return G.Drop_Enabled;
   end Is_Drop_Enabled;
   
   
   ----------------------
   -- Set_Drop_Enabled --
   ----------------------
   
   procedure Set_Drop_Enabled
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Drop_Enabled;
      G.Drop_Enabled := V;

      -- changeSupport.firePropertyChange("dropEnabled", oldValue, dropEnabled);
   end Set_Drop_Enabled;
   
   
   -------------------
   -- Split_Enabled --
   -------------------
   
   function Is_Split_Enabled (G : access Graph_Record) return Boolean is
   begin
      return G.Split_Enabled;
   end Is_Split_Enabled;
   
   
   -----------------------
   -- Set_Split_Enabled --
   -----------------------
   
   procedure Set_Split_Enabled
     (G : access Graph_Record;
      V : Boolean) is
   begin
      G.Split_Enabled := V;
   end Set_Split_Enabled;
   
   
   ----------------
   -- Multigraph --
   ----------------
   
   function Is_Multigraph (G : access Graph_Record) return Boolean is
   begin
      return G.Multigraph;
   end Is_Multigraph;
   
   
   --------------------
   -- Set_Multigraph --
   --------------------
   
   procedure Set_Multigraph
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Multigraph;
      G.Multigraph := V;

      -- changeSupport.firePropertyChange("multigraph", oldValue, multigraph);
   end Set_Multigraph;
   
   
   -----------------------
   -- Swim_Lane_Nesting --
   -----------------------
   
   function Is_Swimlane_Nesting (G : access Graph_Record) return Boolean is
   begin
      return G.Swimlane_Nesting;
   end Is_Swimlane_Nesting;
   
   
   ---------------------------
   -- Set_Swim_Lane_Nesting --
   ---------------------------
   
   procedure Set_Swimlane_Nesting
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Swimlane_Nesting;
      G.Swimlane_Nesting := V;

      --  changeSupport.firePropertyChange("swimlaneNesting", oldValue,
      --  				       swimlaneNesting);
   end Set_Swimlane_Nesting;
   
   
   -----------------------------
   -- Is_Allow_Dangling_Edges --
   -----------------------------
   
   function Is_Allow_Dangling_Edges (G : access Graph_Record) return Boolean is
   begin
      return G.Allow_Dangling_Edges;
   end Is_Allow_Dangling_Edges;
   
   
   ------------------------------
   -- Set_Allow_Dangling_Edges --
   ------------------------------
   
   procedure Set_Allow_Dangling_Edges
     (G     : access Graph_Record;
      Value : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Allow_Dangling_Edges;
      G.Allow_Dangling_Edges := Value;

      --  changeSupport.firePropertyChange("allowDanglingEdges", oldValue,
      --  				       allowDanglingEdges);
   end Set_Allow_Dangling_Edges;
   
   
   ----------------------------
   -- Is_Clone_Invalid_Edges --
   ----------------------------
   
   function Is_Clone_Invalid_Edges (G : access Graph_Record) return Boolean is
   begin
      return G.Clone_Invalid_Edges;
   end Is_Clone_Invalid_Edges;
   
   
   -----------------------------
   -- Set_Clone_Invalid_Edges --
   -----------------------------
   
   procedure Set_Clone_Invalid_Edges
     (G     : access Graph_Record;
      Value : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Clone_Invalid_Edges;
      G.Clone_Invalid_Edges := Value;

      --  changeSupport.firePropertyChange("cloneInvalidEdges", oldValue,
      --  				cloneInvalidEdges);
   end Set_Clone_Invalid_Edges;
   
   
   ------------------------
   -- Disconnect_On_Move --
   ------------------------
   
   function Is_Disconnect_On_Move (G : access Graph_Record) return Boolean is
   begin
      return G.Disconnect_On_Move;
   end Is_Disconnect_On_Move;
   
   
   ----------------------------
   -- Set_Disconnect_On_Move --
   ----------------------------
   
   procedure Set_Disconnect_On_Move
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Disconnect_On_Move;
      G.Disconnect_On_Move := V;

      --  changeSupport.firePropertyChange("disconnectOnMove", oldValue,
      --  				       disconnectOnMove);
   end Set_Disconnect_On_Move;
   
   
   -----------------
   -- Allow_Loops --
   -----------------
   
   function Is_Allow_Loops (G : access Graph_Record) return Boolean is
   begin
      return G.Allow_Loops;
   end Is_Allow_Loops;
   
   
   ---------------------
   -- Set_Allow_Loops --
   ---------------------
   
   procedure Set_Allow_Loops
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Allow_Loops;
      G.Allow_Loops := V;

      -- changeSupport.firePropertyChange("allowLoops", oldValue, allowLoops);
   end Set_Allow_Loops;
   
   
   --------------------------
   -- Is_Connectable_Edges --
   --------------------------
   
   function Is_Connectable_Edges (G : access Graph_Record) return Boolean is
   begin
      return G.Connectable_Edges;
   end Is_Connectable_Edges;
   
   
   ---------------------------
   -- Set_Connectable_Edges --
   ---------------------------
   
   procedure Set_Connectable_Edges
     (G     : access Graph_Record;
      Value : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Connectable_Edges;
      G.Connectable_Edges := Value;

      --  changeSupport.firePropertyChange("connectableEdges", oldValue,
      --  				       connectableEdges);
   end Set_Connectable_Edges;
   
   
   -------------------------
   -- Reset_Edges_On_Move --
   -------------------------
   
   function Is_Reset_Edges_On_Move (G : access Graph_Record) return Boolean is
   begin
      return G.Reset_Edges_On_Move;
   end Is_Reset_Edges_On_Move;
   
   
   -----------------------------
   -- Set_Reset_Edges_On_Move --
   -----------------------------
   
   procedure Set_Reset_Edges_On_Move
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Reset_Edges_On_Move;
      G.Reset_Edges_On_Move := V;

      --  changeSupport.firePropertyChange("resetEdgesOnMove", oldValue,
      --  				       resetEdgesOnMove);
   end Set_Reset_Edges_On_Move;
   
   
   -------------------------------
   -- Reset_View_On_Root_Change --
   -------------------------------
   
   function Is_Reset_View_On_Root_Change
     (G : access Graph_Record) return Boolean is
   begin
      return G.Reset_View_On_Root_Change;
   end Is_Reset_View_On_Root_Change;
   
   
   -----------------------------------
   -- Set_Reset_View_On_Root_Change --
   -----------------------------------
   
   procedure Set_Reset_View_On_Root_Change
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Reset_View_On_Root_Change;
      G.Reset_View_On_Root_Change := V;

      --  changeSupport.firePropertyChange("resetViewOnRootChange", oldValue,
      --  				       resetViewOnRootChange);
   end Set_Reset_View_On_Root_Change;
   
   
   ---------------------------
   -- Reset_Edges_On_Resize --
   ---------------------------
   
   function Is_Reset_Edges_On_Resize
     (G : access Graph_Record) return Boolean is
   begin
      return G.Reset_Edges_On_Resize;
   end Is_Reset_Edges_On_Resize;
   
   
   -------------------------------
   -- Set_Reset_Edges_On_Resize --
   -------------------------------
   
   procedure Set_Reset_Edges_On_Resize
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Reset_Edges_On_Resize;
      G.Reset_Edges_On_Resize := V;

      --  changeSupport.firePropertyChange("resetEdgesOnResize", oldValue,
      --  				       resetEdgesOnResize);
   end Set_Reset_Edges_On_Resize;
   
   
   ----------------------------
   -- Reset_Edges_On_Connect --
   ----------------------------
   
   function Is_Reset_Edges_On_Connect
     (G : access Graph_Record) return Boolean is
   begin
      return G.Reset_Edges_On_Connect;
   end Is_Reset_Edges_On_Connect;
   
   
   --------------------------------
   -- Set_Reset_Edges_On_Connect --
   --------------------------------
   
   procedure Set_Reset_Edges_On_Connect
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Reset_Edges_On_Connect;
      G.Reset_Edges_On_Connect := V;

      --  changeSupport.firePropertyChange("resetEdgesOnConnect", oldValue,
      --  				       resetEdgesOnResize);
   end Set_Reset_Edges_On_Connect;
   
   
   -----------------------
   -- Is_Auto_Size_Cell --
   -----------------------
   
   function Is_Auto_Size_Cell
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean is
      
      Style : Strings_Maps.Map;
   begin
      
      return G.Is_Auto_Size_Cells
        or Is_True (Style, Artics.Graph.Names.STYLE_AUTOSIZE, False);
   end Is_Auto_Size_Cell;
   
   
   ---------------------
   -- Auto_Size_Cells --
   ---------------------
   
   function Is_Auto_Size_Cells (G : access Graph_Record) return Boolean is
   begin
      return G.Auto_Size_Cells;
   end Is_Auto_Size_Cells;
   
   
   -------------------------
   -- Set_Auto_Size_Cells --
   -------------------------
   
   procedure Set_Auto_Size_Cells
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Auto_Size_Cells;
      G.Auto_Size_Cells := V;

      --  changeSupport.firePropertyChange("autoSizeCells", oldValue,
      --  				       autoSizeCells);
   end Set_Auto_Size_Cells;
   
   
   ----------------------
   -- Is_Extend_Parent --
   ----------------------
   
   function Is_Extend_Parent
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean is
   begin
      return not G.Model.Is_Edge (Cell) and G.Is_Extend_Parents;
   end Is_Extend_Parent;
   
   
   -------------------
   -- Extend_Parent --
   -------------------
   
   function Is_Extend_Parents (G : access Graph_Record) return Boolean is
   begin
      return G.Extend_Parents;
   end Is_Extend_Parents;
   
   
   -----------------------
   -- Set_Extend_Parent --
   -----------------------
   
   procedure Set_Extend_Parents
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Extend_Parents;
      G.Extend_Parents := V;

      --  changeSupport.firePropertyChange("extendParents", oldValue,
      --  				       extendParents);
   end Set_Extend_Parents;
   
   
   --------------------------
   -- Extend_Parent_On_Add --
   --------------------------
   
   function Is_Extend_Parents_On_Add (G : access Graph_Record) return Boolean is
   begin
      return G.Extend_Parents_On_Add;
   end Is_Extend_Parents_On_Add;
   
   
   ------------------------------
   -- Set_Extend_Parent_On_Add --
   ------------------------------
   
   procedure Set_Extend_Parents_On_Add
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Extend_Parents_On_Add;
      G.Extend_Parents_On_Add := V;

      --  changeSupport.firePropertyChange("extendParentsOnAdd", oldValue,
      --  				       extendParentsOnAdd);
   end Set_Extend_Parents_On_Add;
   
   
   ------------------------
   -- Is_Constrain_Child --
   ------------------------
   
   function Is_Constrain_Child
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean is
   begin
      return G.Constraint_Children
        and not G.Model.Is_Edge (G.Model.Get_Parent (Cell));
   end Is_Constrain_Child;
   
   
   -------------------------
   -- Constraint_Children --
   -------------------------
   
   function Is_Constraint_Children (G : access Graph_Record) return Boolean is
   begin
      return G.Constraint_Children;
   end Is_Constraint_Children;
   
   
   -----------------------------
   -- Set_Constraint_Children --
   -----------------------------
   
   procedure Set_Constraint_Children
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Constraint_Children;
      G.Constraint_Children := V;

      --  changeSupport.firePropertyChange("constrainChildren", oldValue,
      --  				       constrainChildren);
   end Set_Constraint_Children;
   
   
   -----------------
   -- Auto_Origin --
   -----------------
   
   function Is_Auto_Origin (G : access Graph_Record) return Boolean is
   begin
      return G.Auto_Origin;
   end Is_Auto_Origin;
   
   
   ---------------------
   -- Set_Auto_Origin --
   ---------------------
   
   procedure Set_Auto_Origin
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Auto_Origin;
      G.Auto_Origin := V;

      -- changeSupport.firePropertyChange("autoOrigin", oldValue, autoOrigin);
   end Set_Auto_Origin;
   
   
   ------------
   -- Origin --
   ------------
   
   function Get_Origin (G : access Graph_Record) return Point_Record is
   begin
      return G.Origin;
   end Get_Origin;
   
   
   ----------------
   -- Set_Origin --
   ----------------
   
   procedure Set_Origin
     (G : access Graph_Record;
      V : Point_Record) is
      
      Old_Value : Point_Record;
   begin
      Old_Value := G.Origin;
      G.Origin := V;

      --changeSupport.firePropertyChange("origin", oldValue, origin);
   end Set_Origin;

   
   -------------------------------
   -- Changes_Repaint_Threshold --
   -------------------------------
   
   function Get_Changes_Repaint_Threshold
     (G : access Graph_Record) return Integer is
   begin
      return G.Changes_Repaint_Threshold;
   end Get_Changes_Repaint_Threshold;
   
   
   -----------------------------------
   -- Set_Changes_Repaint_Threshold --
   -----------------------------------
   
   procedure Set_Changes_Repaint_Threshold
     (G : access Graph_Record;
      V : Integer) is
      
      Old_Value : Integer;
   begin
      Old_Value := G.Changes_Repaint_Threshold;
      G.Changes_Repaint_Threshold := V;

      --  changeSupport.firePropertyChange("changesRepaintThreshold", oldValue,
      --  				       changesRepaintThreshold);
   end Set_Changes_Repaint_Threshold;
   
   
   --------------------------------
   -- Allow_Negative_Coordinates --
   --------------------------------
   
   function Is_Allow_Negative_Coordinates
     (G : access Graph_Record) return Boolean is
   begin
      return G.Allow_Negative_Coordinates;
   end Is_Allow_Negative_Coordinates;
   
   
   ------------------------------------
   -- Set_Allow_Negative_Coordinates --
   ------------------------------------
   
   procedure Set_Allow_Negative_Coordinates
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Allow_Negative_Coordinates;
      G.Allow_Negative_Coordinates := V;

      --  changeSupport.firePropertyChange("allowNegativeCoordinates", oldValue,
      --  				       allowNegativeCoordinates);
   end Set_Allow_Negative_Coordinates;
   
   
   --------------------------------
   -- Collapse_To_Preferred_Size --
   --------------------------------
   
   function Is_Collapse_To_Preferred_Size
     (G : access Graph_Record) return Boolean is
   begin
      return G.Collapse_To_Preferred_Size;
   end Is_Collapse_To_Preferred_Size;
   
   
   ------------------------------------
   -- Set_Collapse_To_Preferred_Size --
   ------------------------------------
   
   procedure Set_Collapse_To_Preferred_Size
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Collapse_To_Preferred_Size;
      G.Collapse_To_Preferred_Size := V;

      --  changeSupport.firePropertyChange("collapseToPreferredSize", oldValue,
      --  				       collapseToPreferredSize);
   end Set_Collapse_To_Preferred_Size;
   
   
   ------------------------------
   -- Keep_Edges_In_Foreground --
   ------------------------------
   
   function Is_Keep_Edges_In_Foreground
     (G : access Graph_Record) return Boolean is
   begin
      return G.Keep_Edges_In_Foreground;
   end Is_Keep_Edges_In_Foreground;
   
   
   ----------------------------------
   -- Set_Keep_Edges_In_Foreground --
   ----------------------------------
   
   procedure Set_Keep_Edges_In_Foreground
     (G : access Graph_Record;
      V : Boolean) is

      Old_Value : Boolean;
   begin
      Old_Value := G.Keep_Edges_In_Foreground;
      G.Keep_Edges_In_Foreground := V;

      --  changeSupport.firePropertyChange("keepEdgesInForeground", oldValue,
      --  				       keepEdgesInForeground);
   end Set_Keep_Edges_In_Foreground;
   
   
   ------------------------------
   -- Keep_Edges_In_Background --
   ------------------------------
   
   function Is_Keep_Edges_In_Background
     (G : access Graph_Record) return Boolean is
   begin
      return G.Keep_Edges_In_Background;
   end Is_Keep_Edges_In_Background;
   
   
   ----------------------------------
   -- Set_Keep_Edges_In_Background --
   ----------------------------------
   
   procedure Set_Keep_Edges_In_Background
     (G : access Graph_Record;
      V : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Keep_Edges_In_Background;
      G.Keep_Edges_In_Background := V;
      
      --  changeSupport.firePropertyChange("keepEdgesInBackground", oldValue,
      --  					  keepEdgesInBackground);
   end Set_Keep_Edges_In_Background;
   
   
   ---------------------
   -- Is_Valid_Source --
   ---------------------
   
   function Is_Valid_Source
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean is
   begin
      return (Cell = null and G.Allow_Dangling_Edges)
        or (Cell /= null 
            and (not G.Model.Is_Edge (Cell)));
   end Is_Valid_Source;
   
   
   ---------------------
   -- Is_Valid_Target --
   ---------------------
   
   function Is_Valid_Target
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean is
   begin
      return G.Is_Valid_Source (Cell);
   end Is_Valid_Target;
   
   
   -------------------------
   -- Is_Valid_Connection --
   -------------------------
   
   function Is_Valid_Connection
     (G      : access Graph_Record;
      Source : access Cell_Record'Class;
      Target : access Cell_Record'Class) return Boolean is
   begin
      return G.Is_Valid_Source (Source) and G.Is_Valid_Target (Target)
        and (G.Is_Allow_Loops or Source /= Target);
   end Is_Valid_Connection;
   
   
   ------------------------
   -- Minimum_Graph_Size --
   ------------------------
   
   function Get_Minimum_Graph_Size
     (G : access Graph_Record) return Rectangle_Record is
   begin
      return G.Minimum_Graph_Size;
   end Get_Minimum_Graph_Size;
   
   
   ----------------------------
   -- Set_Minimum_Graph_Size --
   ----------------------------
   
   procedure Set_Minimum_Graph_Size
     (G : access Graph_Record;
      V : Rectangle_Record) is
      
      Old_Value : Rectangle_Record;
   begin
      Old_Value := G.Minimum_Graph_Size;
      G.Minimum_Graph_Size := V;

      -- changeSupport.firePropertyChange("minimumGraphSize", oldValue, value);
   end Set_Minimum_Graph_Size;
   
   
   -----------------
   -- Get_Overlap --
   -----------------
   
   function Get_Overlap
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Coordinate is
   begin
      if G.Is_Allow_Overlap_Parent (Cell) then
         return G.Get_Default_Overlap;
      else
         return 0.0;
      end if;
   end Get_Overlap;
   
   
   ---------------------
   -- Default_Overlap --
   ---------------------
   
   function Get_Default_Overlap (G : access Graph_Record) return Coordinate is
   begin
      return G.Default_Overlap;
   end Get_Default_Overlap;
   
   
   -------------------------
   -- Set_Default_Overlap --
   -------------------------
   
   procedure Set_Default_Overlap
     (G : access Graph_Record;
      O : Coordinate) is
      
      Old_Value : Coordinate;
   begin
      Old_Value := G.Default_Overlap;
      G.Default_Overlap := O;

      -- changeSupport.firePropertyChange("defaultOverlap", oldValue, value);
   end Set_Default_Overlap;
   
   -----------------------------
   -- Is_Allow_Overlap_Parent --
   -----------------------------
   
   function Is_Allow_Overlap_Parent
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean is
   begin
      return False;
   end Is_Allow_Overlap_Parent;
   
   -----------------
   -- Grid_Enable --
   -----------------
   
   function Is_Grid_Enabled (G : access Graph_Record) return Boolean is
   begin
      return G.Grid_Enabled;
   end Is_Grid_Enabled;
   
   ---------------------
   -- Set_Grid_Enable --
   ---------------------
   
   procedure Set_Grid_Enabled
     (G : access Graph_Record;
      E : Boolean) is
      
      Old_Value : Boolean;
   begin
      Old_Value := G.Grid_Enabled;
      G.Grid_Enabled := E;

      -- changeSupport.firePropertyChange("gridEnabled", oldValue, gridEnabled);
   end Set_Grid_Enabled;
 
   ---------------
   -- Grid_Size --
   ---------------
   
   function Get_Grid_Size (G : access Graph_Record) return Coordinate is
   begin
      return G.Grid_Size;
   end Get_Grid_Size;
   
   -------------------
   -- Set_Grid_Size --
   -------------------
   
   procedure Set_Grid_Size
     (G : access Graph_Record;
      S : Coordinate) is
      
      Old_Value : Coordinate;
   begin
      Old_Value := G.Grid_Size;
      G.Grid_Size := S;

      -- changeSupport.firePropertyChange("gridSize", oldValue, gridSize);
   end Set_Grid_Size;
   
   --------------------------
   -- Is_Valid_Drop_Target --
   --------------------------
   
   function Is_Valid_Drop_Target
     (G     : access Graph_Record;
      Cell  : access Cell_Record'Class;
      Cells : Cells_Lists.List) return Boolean is
   begin
      return Cell /= null
        and ((G.Is_Split_Enabled and G.Is_Split_Target (Cell, Cells)) 
             or (not G.Model.Is_Edge (Cell) 
                 and (G.Is_Swimlane (Cell) 
                      or (G.Model.Get_Child_Count (Cell) > 0))));
   end Is_Valid_Drop_Target;
   
   
   ---------------------
   -- Is_Split_Target --
   ---------------------
   
   function Is_Split_Target
     (G      : access Graph_Record;
      Target : access Cell_Record'Class;
      Cells  : Cells_Lists.List) return Boolean is
      
      use Cells_Lists;
      
      Src : access Cell_Record'Class;
      Trg : access Cell_Record'Class;
   begin
      if Target /= null and Cells /= Cells_Lists.Empty_List then
         Src := G.Model.Get_Terminal (Target, True);
         Trg := G.Model.Get_Terminal (Target, False);

         return (G.Model.Is_Edge (Target)
                 and G.Get_Edge_Validation_Error
                   (Target, 
                    G.Model.Get_Terminal
                      (Target, True), Get_First (Cells)) = ""
                 and not G.Model.Is_Ancestor (Get_First (Cells), Src) 
                 and not G.Model.Is_Ancestor (Get_First(Cells), Trg));
      end if;
      return False;
   end Is_Split_Target;
   
   
   ---------------------
   -- Get_Drop_Target --
   ---------------------
   
   function Get_Drop_Target
     (G      : access Graph_Record;
      Cells  : Cells_Lists.List;
      Pt     : Point_Record;
      Cell   : access Cell_Record'Class) return access Cell_Record'Class
   is
      
      Local_Cell : access Cell_Record'Class := Cell;
      Swimlane   : access Cell_Record'Class;
   begin
      if not G.Is_Swimlane_Nesting then
	 
         for Cel of Cells loop
            if G.Is_Swimlane (Cel) then
               return null;
            end if;
         end loop;
      end if;

      -- FIXME the else below does nothing if swimlane is null
      Swimlane := null; -- getSwimlaneAt(pt.x, pt.y);

      if Local_Cell = null then
         Local_Cell := Swimlane;
      end if;
      --  else if (swimlane != null)
      --  {
      --  	// Checks if the cell is an ancestor of the swimlane
      --  	// under the mouse and uses the swimlane in that case
      --  	Object tmp = model.getParent(swimlane);

      --  	while (tmp != null && isSwimlane(tmp) && tmp != cell)
      --  	{
      --  		tmp = model.getParent(tmp);
      --  	}

      --  	if (tmp == cell)
      --  	{
      --  		cell = swimlane;
      --  	}
      --  }*/

      while Local_Cell /= null and not G.Is_Valid_Drop_Target (Local_Cell, Cells)
        and G.Model.Get_Parent (Local_Cell) /= G.Model.Get_Root
      loop
         Local_Cell := G.Model.Get_Parent (Local_Cell);
      end loop;
      
      if G.Model.Get_Parent (Local_Cell) /= G.Model.Get_Root then
         if not Cells_Lists.Contains (Cells, Local_Cell) then
            return Local_Cell;
         end if;
      end if;
      
      return null;
   end Get_Drop_Target;
   
   
   --
   -- Cell retrieval
   --
   
   --------------------
   -- Default_Parent --
   --------------------
   
   function Get_Default_Parent
     (G : access Graph_Record) return access Cell_Record'Class is
      
      Parent     : access Cell_Record'Class;
      Root       : access Cell_Record'Class;
      Root_Child : Cells_Lists.List;
   begin
      Parent := G.Default_Parent;

      if Parent = null then
         Parent := G.View.Get_Current_Root;

         if Parent = null then
            Root := G.Model.Get_Root;
            Root_Child := G.Model.Get_Children_List (Root);
            Parent := G.Model.Get_Child_At (Root, 1);
         end if;
      end if;

      return Parent;
   end Get_Default_Parent;
   
   ------------------------
   -- Set_Default_Parent --
   ------------------------
   
   procedure Set_Default_Parent
     (G : access Graph_Record;
      P : access Cell_Record'Class) is
   begin
      G.Default_Parent := P;
   end Set_Default_Parent;
   
   
   ------------------------
   -- Get_Child_Vertices --
   ------------------------
   
   function Get_Child_Vertices
     (G      : access Graph_Record;
      Parent : access Cell_Record'Class) return Cells_Lists.List is
   begin
      return G.Get_Child_Cells (Parent, True, False);
   end Get_Child_Vertices;
   
   
   ---------------------
   -- Get_Child_Edges --
   ---------------------
   
   function Get_Child_Edges
     (G      : access Graph_Record;
      Parent : access Cell_Record'Class) return Cells_Lists.List is
   begin
      return G.Get_Child_Cells (Parent, False, True);
   end Get_Child_Edges;
   
   
   ---------------------
   -- Get_Child_Cells --
   ---------------------
   
   function Get_Child_Cells
     (G      : access Graph_Record;
      Parent : access Cell_Record'Class) return Cells_Lists.List is
   begin
      return G.Get_Child_Cells (Parent, False, False);
   end Get_Child_Cells;

   
   ---------------------
   -- Get_Child_Cells --
   ---------------------
   
   function Get_Child_Cells
     (G        : access Graph_Record;
      Parent   : access Cell_Record'Class;
      Vertices : Boolean;
      Edges    : Boolean) return Cells_Lists.List is
      
      Cells  : Cells_Lists.List;
      Result : Cells_Lists.List;
   begin
      Cells := Get_Child_Cells (G.Model, Parent, Vertices, Edges);
      
      for Cell of Cells loop
         Cells_Lists.Append (Result, Cell);
      end loop;
      
      return Result;
   end Get_Child_Cells;
   
   
   ---------------------
   -- Get_Connections --
   ---------------------
   
   function Get_Connections
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Cells_Lists.List is
   begin
      return G.Get_Connections (Cell, null);
   end Get_Connections;
   
   
   ---------------------
   -- Get_Connections --
   ---------------------
   
   function Get_Connections
     (G      : access Graph_Record;
      Cell   : access Cell_Record'Class;
      Parent : access Cell_Record'Class) return Cells_Lists.List is
   begin
      return G.Get_Connections (Cell, Parent, False);
   end Get_Connections;
   
   
   ---------------------
   -- Get_Connections --
   ---------------------
   
   function Get_Connections
     (G       : access Graph_Record;
      Cell    : access Cell_Record'Class;
      Parent  : access Cell_Record'Class;
      Recurse : Boolean) return Cells_Lists.List is
   begin
      return G.Get_Edges (Cell, Parent, True, True, False, Recurse);
   end Get_Connections;
   
   
   ------------------------
   -- Get_Incoming_Edges --
   ------------------------
   
   function Get_Incoming_Edges
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Cells_Lists.List is
   begin
      return G.Get_Incoming_Edges (Cell, null);
   end Get_Incoming_Edges;
   
   
   ------------------------
   -- Get_Incoming_Edges --
   ------------------------
   
   function Get_Incoming_Edges
     (G      : access Graph_Record;
      Cell   : access Cell_Record'Class;
      Parent : access Cell_Record'Class) return Cells_Lists.List is
   begin
      return G.Get_Edges (Cell, Parent, True, False, False);
   end Get_Incoming_Edges;

   
   -------------------------
   -- Get_Outcoming_Edges --
   -------------------------
   
   function Get_Outgoing_Edges
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Cells_Lists.List is
   begin
      return G.Get_Outgoing_Edges (Cell, null);
   end Get_Outgoing_Edges;
   
   
   -------------------------
   -- Get_Outcoming_Edges --
   -------------------------
   
   function Get_Outgoing_Edges
     (G      : access Graph_Record;
      Cell   : access Cell_Record'Class;
      Parent : access Cell_Record'Class) return Cells_Lists.List is
   begin
      return G.Get_Edges (Cell, Parent, False, True, False);
   end Get_Outgoing_Edges;
   
   
   ---------------
   -- Get_Edges --
   ---------------
   
   function Get_Edges
     (G             : access Graph_Record;
      Cell          : access Cell_Record'Class;
      Parent        : access Cell_Record'Class := null;
      Incoming      : Boolean := True;
      Outgoing      : Boolean := True;
      Include_Loops : Boolean := True;
      Recurse       : Boolean := False) return Cells_Lists.List is
      
      Edges        : Cells_Lists.List;
      Result       : Cells_Lists.List;
      Source       : access Cell_Record'Class;
      Target       : access Cell_Record'Class;
      State        : access Cell_State_Record'Class;
      Childs       : Cells_Lists.List;
      Cell_Edges   : Cells_Lists.List;
   begin
      Childs := G.Model.Get_Children_List (Cell);

      Cell_Edges := Get_Edges
        (G.Model, Cell, Incoming, Outgoing, Include_Loops);
      Append_List (Edges, Cell_Edges);
      
      for Edge of Edges loop
         State := G.View.Get_State (Edge);
	 
         if State /= null then
            Source := State.Get_Visible_Terminal (True);
         else
            Source := G.View.Get_Visible_Terminal (Edge, True);
         end if;
	 
         if State /= null then
            Target := State.Get_Visible_Terminal (False);
         else
            Target := G.View.Get_Visible_Terminal (Edge, False);
         end if;

         if ((Include_Loops and Source = Target)
             or ((Source /= Target) 
                 and ((Incoming and Target = Cell 
                       and (Parent = null or G.Is_Valid_Ancestor
                            (Source, Parent, Recurse))) 
                      or (Outgoing and Source = Cell 
                          and (Parent = null or G.Is_Valid_Ancestor
                               (Target, Parent, Recurse))))))
         then
            Cells_Lists.Append (Result, Edge);
         end if;
      end loop;

      return Result;
   end Get_Edges;
   
   
   -----------------------
   -- Is_Valid_Ancestor --
   -----------------------
   
   function Is_Valid_Ancestor
     (G       : access Graph_Record;
      Cell    : access Cell_Record'Class;
      Parent  : access Cell_Record'Class;
      Recurse : Boolean) return Boolean is
   begin
      if Recurse then
         return G.Model.Is_Ancestor (Parent, Cell);
      else
         return G.Model.Get_Parent (Cell) = Parent;
      end if;
   end Is_Valid_Ancestor;
   
   
   -------------------
   -- Get_Opposites --
   -------------------
   
   function Get_Opposites
     (G        : access Graph_Record;
      Edges    : Cells_Lists.List;
      Terminal : access Cell_Record'Class;
      Sources  : Boolean := True;
      Targets  : Boolean := True) return Cells_Lists.List is
      
      use Cells_Lists;
      
      Terminals : Cells_Lists.List;
      State     : access Cell_State_Record'Class;
      Source    : access Cell_Record'Class;
      Target    : access Cell_Record'Class;
   begin
      if Edges /= Cells_Lists.Empty_List then
	 
         for Edge of Edges loop
            State := G.View.Get_State (Edge);
	    
            if State /= null then
               Source := State.Get_Visible_Terminal (True);
            else
               Source := G.View.Get_Visible_Terminal (Edge, True);
            end if;
	    
            if State /= null then
               Target := State.Get_Visible_Terminal (False);
            else
               Target := G.View.Get_Visible_Terminal (Edge, False);
            end if;

            -- Checks if the terminal is the source of the edge and if the
            -- target should be stored in the result
	    
            if Targets 
              and Source = Terminal 
              and Target /= null 
              and Target /= Terminal
            then
               Cells_Lists.Append (Terminals, Target);

               -- Checks if the terminal is the taget of the edge and if the 
               -- source should be stored in the result
	       
            elsif Sources 
              and Target = Terminal 
              and Source /= null
              and Source /= Terminal 
            then
               Cells_Lists.Append (Terminals, Source);
            end if;
         end loop;
      end if;

      return Terminals;
   end Get_Opposites;
   
   
   -----------------------
   -- Get_Edges_Between --
   -----------------------
   
   function Get_Edges_Between
     (G        : access Graph_Record;
      Source   : access Cell_Record'Class;
      Target   : access Cell_Record'Class;
      Directed : Boolean := False) return Cells_Lists.List is
      
      use Cells_Lists;
      
      Edges  : Cells_Lists.List;
      Result : Cells_Lists.List;
      State  : access Cell_State_Record'Class;
      Src    : access Cell_Record'Class;
      Trg    : access Cell_Record'Class;
   begin
      
      Edges := G.Get_Edges (Source);

      -- Checks if the edge is connected to the correct cell and adds any match
      -- to the result
      
      for Edge of Edges loop
         State := G.View.Get_State (Edge);
	 
         if State /= null then
            Src := State.Get_Visible_Terminal (True);
         else
            Src := G.View.Get_Visible_Terminal (Edge, True);
         end if;
	 
         if State /= null then
            Trg := State.Get_Visible_Terminal (False);
         else
            Trg := G.View.Get_Visible_Terminal (Edge, False);
         end if;
	 
         if (Src = Source and Trg = Target)
           or (not Directed  and Src = Target and Trg = Source) 
         then
            Cells_Lists.Append (Result, Edge);
         end if;
      end loop;

      return Result;
   end Get_Edges_Between;
   
   
   ----------------------
   -- Get_Cells_Beyond --
   ----------------------
   
   function Get_Cells_Beyond
     (G                : access Graph_Record;
      X0               : Coordinate;
      Y0               : Coordinate;
      Parent           : access Cell_Record'Class;
      Right_Half_Pane  : Boolean;
      Bottom_Half_Pane : Boolean) return Cells_Lists.List is
      
      Par           : access Cell_Record'Class := Parent;
      Result        : Cells_Lists.List;
      State         : access Cell_State_Record'Class;
      Parent_Childs : Cells_Lists.List;
   begin
      if Par = null then
         Par := G.Get_Default_Parent;
      end if;

      --  int childCount = model.getChildCount(parent);
      --  List<Object> result = new ArrayList<Object>(childCount);
      
      if Right_Half_Pane or Bottom_Half_Pane then

         if Par /= null then
            Parent_Childs := G.Model.Get_Children_List (Par);
	    
            for Child of Parent_Childs loop
               State := G.View.Get_State (Child);

               if State /= null then

                  if (not Right_Half_Pane or State.Get_X >= X0)
                    and (not Bottom_Half_Pane or State.Get_Y >= Y0)
                  then
                     Cells_Lists.Append (Result, Child);
                  end if;
               end if;
            end loop;
         end if;
      end if;
      
      return Result;
   end Get_Cells_Beyond;
   
   
   ---------------------
   -- Find_Tree_Roots --
   ---------------------
   
   function Find_Tree_Roots
     (G       : access Graph_Record;
      Parent  : access Cell_Record'Class;
      Isolate : Boolean := False;
      Invert  : Boolean := False) return Cells_Lists.List is
      
      use Cells_Lists;
      
      Roots         : Cells_Lists.List := Cells_Lists.Empty_List;
      Best          : access Cell_Record'Class;
      Max_Diff      : Integer;
      Fan_Out       : Integer;
      Fan_In        : Integer;
      Diff          : Integer;
      Conns         : Cells_Lists.List;
      Parent_Childs : Cells_Lists.List;
      Src           : access Cell_Record'Class;
   begin
      if Parent /= null then
         Parent_Childs := G.Model.Get_Children_List (Parent);
	 
         Best     := null;
         Max_Diff := 0;
	 
         for Cell of Parent_Childs loop

            if G.Model.Is_Vertex (Cell) then
	       
               if Isolate then
                  Conns := G.Get_Connections (Cell, Parent);
               else
                  Conns := G.Get_Connections (Cell,  null);
               end if;
	       
               Fan_Out := 0;
               Fan_In  := 0;

	       
               for Con of Conns loop
                  Src := G.View.Get_Visible_Terminal (Con, True);

                  if Src = Cell then
                     Fan_Out := Fan_Out + 1;
                  else
                     Fan_In  := Fan_In + 1;
                  end if;
               end loop;

               if (Invert and Fan_Out = 0 and Fan_In > 0)
                 or (not Invert and Fan_In = 0 and Fan_Out > 0)
               then
                  Cells_Lists.Append (Roots, Cell);
               end if;
	       
               if Invert then
                  Diff := Fan_In - Fan_Out;
               else
                  Diff := Fan_Out - Fan_In;
               end if;
	       
               if Diff > Max_Diff then
                  Max_Diff := Diff;
                  Best := Cell;
               end if;
            end if;
         end loop;

         if Roots = Cells_Lists.Empty_List and Best /= null then
            Cells_Lists.Append (Roots, Best);
         end if;
      end if;
      
      return Roots;
   end Find_Tree_Roots;
   
   
   --------------
   -- Traverse --
   --------------
   
   procedure Traverse 
     (G        : access Graph_Record;
      Vertex   : access Cell_Record'Class;
      Directed : Boolean;
      Visitor  : access Cell_Visitor_Interface'Class) is
      
      Visited : Cells_Lists.List := Cells_Lists.Empty_List;
   begin
      G.Traverse (Vertex, Directed, Visitor, null, Visited);
   end Traverse;
   
   
   --------------
   -- Traverse --
   --------------
   
   procedure Traverse 
     (G        : access Graph_Record;
      Vertex   : access Cell_Record'Class;
      Directed : Boolean;
      Visitor  : access Cell_Visitor_Interface'Class;
      Edge     : access Cell_Record'Class;
      Visited  : in out Cells_Lists.List) is
      
      use Cells_Lists;
      
      Vertex_Edges : Cells_Lists.List;
      Is_Source    : Boolean;
      Nxt          : access Cell_Record'Class;
   begin
      if Vertex /= null and Visitor /= null then
	 
         if not Cells_Lists.Contains (Visited, Vertex) then
	    
            Cells_Lists.Append (Visited, Vertex);

            if Visitor.Visit (Vertex, Edge) then
	       
               Vertex_Edges := G.Model.Get_Edges_List (Vertex);
	       
               for E of Vertex_Edges loop
                  Is_Source := G.Model.Get_Terminal (E, True) = Vertex;

                  if not Directed or Is_Source then
                     Nxt := G.Model.Get_Terminal (E, not Is_Source);
                     G.Traverse (Nxt, Directed, Visitor, E, Visited);
                  end if;
               end loop;
            end if;
         end if;
      end if;
   end Traverse;
   
   
   -- Selection --
   ---------------
   
   -------------------------
   -- Get_Selection_Model --
   -------------------------
   
   function Get_Selection_Model
     (G : access Graph_Record) return access Selection_Model_Record'Class is
   begin
      return G.Selection_Model;
   end Get_Selection_Model;
   
   
   -------------------------
   -- Set_Selection_Model --
   -------------------------
   
   procedure Set_Selection_Model
     (G : access Graph_Record;
      M : access Selection_Model_Record'Class) is
   begin
      G.Selection_Model := M;
   end Set_Selection_Model;
   
   
   -------------------------
   -- Get_Selection_Count --
   -------------------------
   
   function Get_Selection_Count (G : access Graph_Record) return Integer is
   begin
      return G.Selection_Model.Size;
   end Get_Selection_Count;
   
   
   ----------------------
   -- Is_Cell_Selected --
   ----------------------
   
   function Is_Cell_Selected 
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) return Boolean is
   begin
      return G.Selection_Model.Is_Selected (Cell);
   end Is_Cell_Selected;
   
   
   ------------------------
   -- Is_Selection_Empty --
   ------------------------
   
   function Is_Selection_Empty (G : access Graph_Record) return Boolean is
   begin
      return G.Selection_Model.Is_Empty;
   end Is_Selection_Empty;
   
   
   ---------------------
   -- Clear_Selection --
   ---------------------
   
   procedure Clear_Selection (G : access Graph_Record) is
   begin
      G.Selection_Model.Clear;
   end Clear_Selection;
   
   
   ------------------------
   -- Get_Selection_Cell --
   ------------------------
   
   function Get_Selection_Cell
     (G : access Graph_Record) return access Cell_Record'Class is
   begin
      return G.Selection_Model.Get_Cell;
   end Get_Selection_Cell;
   
   
   ------------------------
   -- Set_Selection_Cell --
   ------------------------
   
   procedure Set_Selection_Cell
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) is
   begin
      G.Selection_Model.Set_Cell (Cell);
   end Set_Selection_Cell;
   
   
   -------------------------
   -- Get_Selection_Cells --
   -------------------------
   
   function Get_Selection_Cells
     (G : access Graph_Record) return Cells_Lists.List is
   begin
      return G.Selection_Model.Get_Cells;
   end Get_Selection_Cells;
   
   
   -------------------------
   -- Set_Selection_Cells --
   -------------------------
   
   procedure Set_Selection_Cells
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) is
   begin
      G.Selection_Model.Set_Cells (Cells);
   end Set_Selection_Cells;
   
   
   ------------------------
   -- Add_Selection_Cell --
   ------------------------
   
   procedure Add_Selection_Cell 
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) is
   begin
      G.Selection_Model.Add_Cell (Cell);
   end Add_Selection_Cell;
   
   
   -------------------------
   -- Add_Selection_Cells --
   -------------------------
   
   procedure Add_Selection_Cells
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) is
   begin
      G.Selection_Model.Add_Cells (Cells);
   end Add_Selection_Cells;
   
   
   ---------------------------
   -- Remove_Selection_Cell --
   ---------------------------
   
   procedure Remove_Selection_Cell
     (G    : access Graph_Record;
      Cell : access Cell_Record'Class) is
   begin
      G.Selection_Model.Remove_Cell (Cell);
   end Remove_Selection_Cell;
   
   
   ----------------------------
   -- Remove_Selection_Cells --
   ----------------------------
   
   procedure Remove_Selection_Cells
     (G     : access Graph_Record;
      Cells : Cells_Lists.List) is
   begin
      G.Selection_Model.Remove_Cells (Cells);
   end Remove_Selection_Cells;
   
   
   ----------------------
   -- Select_Next_Cell --
   ----------------------
   
   procedure Select_Next_Cell (G : access Graph_Record) is
   begin
      G.Select_Cell (True, False, False);
   end Select_Next_Cell;
     
   
   --------------------------
   -- Select_Previous_Cell --
   --------------------------
   
   procedure Select_Previous_Cell (G : access Graph_Record) is
   begin
      G.Select_Cell (False, False, False);
   end Select_Previous_Cell;
   
   
   ------------------------
   -- Select_Parent_Cell --
   ------------------------
   
   procedure Select_Parent_Cell (G : access Graph_Record) is
   begin
      G.Select_Cell (False, True, False);
   end Select_Parent_Cell;
   
   
   -----------------------
   -- Select_Child_Cell --
   -----------------------
   
   procedure Select_Child_Cell (G : access Graph_Record) is
   begin
      G.Select_Cell (False, False, True);
   end Select_Child_Cell;
   
   
   -----------------
   -- Select_Cell --
   -----------------
   
   procedure Select_Cell
     (G         : access Graph_Record;
      Is_Next   : Boolean;
      Is_Parent : Boolean;
      Is_Child  : Boolean) is
      
      Cell        : access Cell_Record'Class;
      Parent      : access Cell_Record'Class;
      Child       : access Cell_Record'Class;
      Child_Count : Integer;
      Tmp         : Integer;
      I           : Integer;
      Index       : Integer;
   begin
      Cell := G.Get_Selection_Cell;

      if G.Get_Selection_Count > 1 then
         G.Clear_Selection;
      end if;
      
      if Cell /= null then
         Parent := G.Model.Get_Parent (Cell);
      else
         Parent := G.Get_Default_Parent;
      end if;
      
      Child_Count := G.Model.Get_Child_Count (Parent);
      
      if Cell = null and Child_Count > 0 then
         Child := G.Model.Get_Child_At (Parent, 1);
         G.Set_Selection_Cell (Child);
	 
      elsif (Cell = null or Is_Parent) 
        and G.View.Get_State (Parent) /= null
        and G.Model.Get_Geometry (Parent) /= null
      then
         if G.Get_Current_Root /= Parent then
            G.Set_Selection_Cell (Parent);
         end if;
	 
      elsif Cell /= null and Is_Child then
         Tmp := G.Model.Get_Child_Count (Cell);

         if Tmp > 0 then
            Child := G.Model.Get_Child_At (Cell, 1);
            G.Set_Selection_Cell (Child);
         end if;
	 
      elsif Child_Count > 0 then
         I := Parent.Get_Index (Cell);
	 
         if Is_Next then
            I := I + 1;
            G.Set_Selection_Cell
              (G.Model.Get_Child_At (Parent, I mod Child_Count));
         else
            I := I - 1;
            if I < 0 then
               Index := Child_Count - 1;
            else
               Index := I;
            end if;
	    
            G.Set_Selection_Cell
              (G.Model.Get_Child_At (Parent, Index));
         end if;
      end if;
   end Select_Cell;
   
   ---------------------
   -- Select_Vertices --
   ---------------------
   
   procedure Select_Vertices
     (G      : access Graph_Record;
      Parent : access Cell_Record'Class := null) is
   begin
      G.Select_Cells (True, False, Parent);
   end Select_Vertices;
   
   
   ------------------
   -- Select_Edges --
   ------------------
   
   procedure Select_Edges
     (G      : access Graph_Record;
      Parent : access Cell_Record'Class := null) is
   begin
      G.Select_Cells (False, True, Parent);
   end Select_Edges;
   
   
   ------------------
   -- Select_Cells --
   ------------------
   
   procedure Select_Cells
     (G        : access Graph_Record;
      Vertices : Boolean;
      Edges    : Boolean;
      Parent   : access Cell_Record'Class := null) is
      
      
      type Select_Filter_Record is
        new Object_Record and Filter_Interface with null record;
      
      function Filter
        (F    : access Select_Filter_Record;
         Cell : access Cell_Record'Class) return Boolean;
      
      function Filter
        (F    : access Select_Filter_Record;
         Cell : access Cell_Record'Class) return Boolean is
      begin
         return G.View.Get_State (Cell) /= null
           and G.Model.Get_Child_Count (Cell) = 0
           and ((G.Model.Is_Vertex (Cell) and Vertices) 
                or (G.Model.Is_Edge (Cell) and Edges));
      end Filter;
      
      Select_Filter : aliased Select_Filter_Record := 
        Select_Filter_Record'(No_Object_Record with null record);
      
      
      Par   : access Cell_Record'Class := Parent;
      Cells : Cells_Lists.List;
   begin
      if Par = null then
         Par := G.Get_Default_Parent;
      end if;

      Cells := Filter_Descendants (G.Model, Select_Filter'Access);
      
      --  new mxGraphModel.Filter()
      --  {
      --
      --      public boolean filter(Object cell)
      --      {
      --  	    return view.getState(cell) != null
      --  	          && model.getChildCount(cell) == 0
      --  	          && ((model.isVertex(cell) && vertices) || (model
      --  					    .isEdge(cell) && edges));
      --      }
      --
      --   });
      
      G.Set_Selection_Cells (Cells);
   end Select_Cells;
   
   
   ----------------
   -- Select_All --
   ----------------
   
   procedure Select_All
     (G      : access Graph_Record;
      Parent : access Cell_Record'Class := null) is
      
      use Cells_Lists;
      
      Par      : access Cell_Record'Class := Parent;
      Children : Cells_Lists.List;
   begin
      if Par = null then
         Par := G.Get_Default_Parent;
      end if;

      Children := Get_Children (G.Model, Par);

      if Children /= Cells_Lists.Empty_List then
         G.Set_Selection_Cells (Children);
      end if;
   end Select_All;

   
   
   --  ===>>> protected mxEdgeStyle.mxEdgeStyleFunction defaultLoopStyle = mxEdgeStyle.Loop;
   -- Specifies the default style for loops.
   
   
   --------------------------
   -- Allow_Dandling_Edges --
   --------------------------
   
   function Get_Allow_Dandling_Edges (G : access Graph_Record) return Boolean is
   begin
      return G.Allow_Dangling_Edges;
   end Get_Allow_Dandling_Edges;
   
   
   ------------------------------
   -- Set_Allow_Dandling_Edges --
   ------------------------------
   
   procedure Set_Allow_Dandling_Edges
     (G : access Graph_Record;
      V : Boolean) is
   begin
      G.Allow_Dangling_Edges := V;
   end Set_Allow_Dandling_Edges;
   
   
   -------------------------
   -- Clone_Invalid_Edges --
   -------------------------
   
   function Get_Clone_Invalid_Edges (G : access Graph_Record) return Boolean is
   begin
      return G.Clone_Invalid_Edges;
   end Get_Clone_Invalid_Edges;
   
   
   -----------------
   -- Clone_Graph --
   -----------------
   
   function Clone_Graph (G : Graph_Ptr) return Graph_Ptr is
   begin
      return null;
   end Clone_Graph;
   
   -------------------
   -- Get_Root_Node --
   -------------------
   
   --     function Get_Root_Node
   --       (This : access Graph_Record) return access Node_Record'Class is
   --     begin
   --        return This.Root_Node;
   --     end Get_Root_Node;
   
   -------------------
   -- Set_Root_Node --
   -------------------
   
   --     procedure Set_Root_Node
   --       (This : access Graph_Record;
   --        Root : access Node_Record'Class) is
   --     begin
   --        This.Root_Node := Root;
   --     end Set_Root_Node;
   
end Artics.Graph.Graphs;
