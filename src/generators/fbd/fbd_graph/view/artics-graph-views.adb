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

with Artics.Maths; use Artics.Maths; 

with Artics.Graph.Events.Undo_Events;
with Artics.Graph.Events.Scale_And_Translate_Events;
with Artics.Graph.Events.Scale_Events;
with Artics.Graph.Events.Translate_Events;

with Artics.Logutils; use Artics.Logutils;

package body Artics.Graph.Views is

  
   -------------------
   -- New_View_Cell --
   -------------------
   
   function New_View return View_Ptr is
      View : View_Ptr :=  new View_Record'(No_View_Record);
   begin
      return View;
   end New_View;
   
   --------------
   -- New_View --
   --------------
   
   function New_View (Graph : access Graph_Interface'Class) return View_Ptr is 
      View : View_Ptr := New_View;
   begin
      View.Graph := Graph;
      
      return View;
   end New_View;
   
   -----------
   -- Graph --
   -----------
   
   function Get_Graph
     (V : access View_Record) return access Graph_Interface'Class is
   begin
      return V.Graph;
   end Get_Graph;
   
   ---------------
   -- Set_Graph --
   ---------------
   
   procedure Set_Graph
     (V : access View_Record;
      G : access Graph_Interface'Class) is
   begin
      V.Graph := G;
   end Set_Graph;
   
   ----------------
   -- Get_States --
   ----------------
   
   function Get_States (V : access View_Record) return Cells_States_Maps.Map is
   begin
      return V.States;
   end Get_States;
   
   ----------------
   -- Set_States --
   ----------------
   
   procedure Set_States
     (V      : access View_Record;
      States : Cells_States_Maps.Map) is
   begin
      V.States := States;
   end Set_States;
   
   ------------------
   -- Graph_Bounds --
   ------------------
   
   function Get_Graph_Bounds (V : access View_Record) return Rectangle_Record is
   begin
      return V.Graph_Bounds;
   end Get_Graph_Bounds;
   
   ----------------------
   -- Set_Graph_Bounds --
   ----------------------
   
   procedure Set_Graph_Bounds
     (V : access View_Record;
      R : Rectangle_Record) is
   begin
      V.Graph_Bounds := R;
   end Set_Graph_Bounds;
   
   ----------------------
   -- Get_Current_Root --
   ----------------------
   
   function Get_Current_Root 
     (V : access View_Record) return access Cell_Record'Class is
   begin
      return V.Current_Root;
   end Get_Current_Root;
   
   ----------------------
   -- Set_Cuurent_Root --
   ----------------------
   
   procedure Set_Current_Root
     (V    : access View_Record;
      Root : access Cell_Record'Class) is
      
      Change : access Current_Root_Change_Record;
      Edit   : access Undoable_Edit_Record;
   begin
      if V.Current_Root /= Root then
         Change := New_Current_Root_Change (V, Root);
         Change.Execute;
	 
         Edit := New_Undoable_Edit (V, False);
         Edit.Add (Change);
	 
         declare
            use Artics.Graph.Events.Undo_Events;
            Evt : access Undo_Event_Record :=
              New_Undo_Event (Edit);
         begin
            Fire_Event (V, Evt);
            Free_Undo_Event (Undo_Event_Ptr (Evt));
            --- fireEvent(new mxEventObject(mxEvent.UNDO, "edit", edit));
         end;
      end if;
   end Set_Current_Root;
   
   -------------------------
   -- Scale_And_Translate --
   -------------------------
   
   procedure Scale_And_Translate
     (V     : access View_Record;
      Scale : Coordinate;
      Dx    : Coordinate;
      Dy    : Coordinate) is
      
      Previous_Scale     : Coordinate := V.Scale;
      Previous_Translate : Point_Record := V.Translate;
   begin
      if Scale /= V.Scale 
        or Dx /= Get_X (V.Translate) 
        or Dy /= Get_Y (V.Translate)
      then
         V.Scale := Scale;
         V.Translate := Point_Record'(Dx, Dy);
	 
         if Is_Events_Enabled (V) then
            V.Revalidate;
         end if;
      end if;
      
      declare
         use Artics.Graph.Events.Scale_And_Translate_Events;
         Evt : access Scale_And_Translate_Event_Record :=
           New_Scale_And_Translate_Event
             (Scale, Previous_Scale, V.Translate, Previous_Translate);
      begin
         Fire_Event (V, Evt);
         Free_Scale_And_Translate_Event (Scale_And_Translate_Event_Ptr (Evt));
         --fireEvent(new mxEventObject(mxEvent.SCALE_AND_TRANSLATE, 
         --  "scale", scale, 
         --  "previousScale", previousScale, 
         --  "translate", translate,
         --  "previousTranslate", previousTranslate));
      end;
   end Scale_And_Translate;
   
   -----------
   -- Scale --
   -----------
   
   function Get_Scale (V : access View_Record) return Coordinate is
   begin
      return V.Scale;
   end Get_Scale;
   
   ---------------
   -- Set_Scale --
   ---------------
   
   procedure Set_Scale
     (V     : access View_Record;
      Value : Coordinate) is
      
      Previous_Scale : Coordinate := V.Scale;
   begin
      if V.Scale /= Value then
         V.Scale := Value;
	 
         if Is_Events_Enabled (V) then
            V.Revalidate;
         end if;
      end if;
      
      declare
         use Artics.Graph.Events.Scale_Events;
         Evt : access Scale_Event_Record := 
           New_Scale_Event (V.Scale, Previous_Scale);
      begin
         Fire_Event (V, Evt);
         Free_Scale_Event (Scale_Event_Ptr (Evt));
         -- fireEvent(new mxEventObject(mxEvent.SCALE, "scale", scale,
         --   "previousScale", previousScale));
      end;
   end Set_Scale;
   
   ---------------
   -- Translate --
   ---------------
   
   function Get_Translate (V : access View_Record) return Point_Record is
      
   begin
      return V.Translate;
   end Get_Translate;
   
   -------------------
   -- Set_Translate --
   -------------------
   
   procedure Set_Translate
     (V     : access View_Record;
      Value : Point_Record) is
      
      Previous_Translate : Point_Record := V.Translate;
   begin
      if Value /= No_Point_Record 
        and (Get_X (Value) /= Get_X (V.Translate)
             or Get_Y (Value) /= Get_Y (V.Translate))
      then
         V.Translate := Value;

         if Is_Events_Enabled (V) then
            V.Revalidate;
         end if;
      end if;
      
      declare
         use Artics.Graph.Events.Translate_Events;
         Evt : access Translate_Event_Record :=
           New_Translate_Event (V.Translate, Previous_Translate);
      begin
         Fire_Event (V, Evt);
         Free_Translate_Event (Translate_Event_Ptr (Evt));
         -- fireEvent(new mxEventObject(mxEvent.TRANSLATE, 
         --   "translate", translate,
         --   "previousTranslate", previousTranslate));
      end;
      
   end Set_Translate;
   
   ----------------------
   -- Get_Bounding_Box --
   ----------------------
   
   function Get_Bounding_Box
     (V     : access View_Record;
      Cells : Cells_Lists.List) return Rectangle_Record is
   begin
      return Get_Bounds (V, Cells, True);
   end Get_Bounding_Box;
   
   ----------------
   -- Get_Bounds --
   ----------------
   
   function Get_Bounds 
     (V            : access View_Record;
      Cells        : Cells_Lists.List;
      Bounding_Box : Boolean := False) return Rectangle_Record is
      
      use Cells_Lists;
      
      Result : Rectangle_Record;
      Model  : access Model_Interface'Class := V.Graph.Get_Model;
      State  : access Cell_State_Record'Class;
      Tmp    : Rectangle_Record;
      Graph  : access Graph_Interface'Class := V.Graph;
   begin
      Result := No_Rectangle_Record;
      
      if not Cells_Lists.Is_Empty (Cells) then
      
         Tmp := No_Rectangle_Record;
	 
         for C of Cells loop
            if Model.Is_Vertex (C) or Model.Is_Edge (C) then
               State := Get_State (V, C);
	       
               if State /= null then
                  if Bounding_Box then
                     Tmp := Get_Bounding_Box (V, State);
                  else
                     Tmp := Inside_Rectangle (State);
                  end if;
               end if;
	       
               if Tmp /= No_Rectangle_Record then
                  if Result = No_Rectangle_Record then
                     Result := Tmp;
                  else
                     Add (Result, Tmp);
                  end if;
               end if;
            end if;
         end loop;
      end if;
      
      return Result;
   end Get_Bounds;
   
   ------------
   -- Reload --
   ------------
   
   procedure Reload (V : access View_Record) is
   begin
      Cells_States_Maps.Clear (V.States);
      V.Validate;
   end Reload;
   
   ----------------
   -- Revalidate --
   ----------------
   
   procedure Revalidate (V : access View_Record) is
   begin
      V.Invalidate;
      V.Validate;
   end Revalidate;
   
   -----------------
   -- Empty_Point --
   -----------------
   
   function Get_Empty_Point (V : access View_Record) return Point_Record is
   begin
      return V.Empty_Point;
   end Get_Empty_Point;
   
   ---------------------
   -- Set_Empty_Point --
   ---------------------
   
   procedure Set_Empty_Point
     (V : access View_Record;
      P : Point_Record) is
   begin
      V.Empty_Point := P;
   end Set_Empty_Point;
   
   -----------
   -- Clear --
   -----------
   
   procedure Clear
     (V       : access View_Record;
      Cell    : access Cell_Record'Class;
      Force   : Boolean;
      Recurse : Boolean) is
      
      Model      : access Model_Interface'Class;
      Child_List : Cells_Lists.List;
   begin
      V.Remove_State (Cell);
      
      if Recurse and (Force or Cell /= V.Current_Root) then
         Model := V.Graph.Get_Model;
	 
         Child_List := Cell.Get_Children_List;
         for C of Child_List loop
            Clear (V, C, Force, Recurse);
         end loop;
      else
         Invalidate (V, Cell);
      end if;
   end Clear;
   
   ----------------
   -- Invalidate --
   ----------------

   procedure Invalidate
     (V : access View_Record;
      C : access Cell_Record'Class := null) 
   is
      Model  : access Model_Interface'Class;
      Cell   : access Cell_Record'Class := C;
      State  : access Cell_State_Record'Class;
      Childs : Cells_Lists.List;
      Edges  : Cells_Lists.List;
   begin
      Model := V.Graph.Get_Model;
      if C = null then
         Cell := Model.Get_Root;
      else
         Cell := C;
      end if;
      
      State := V.Get_State (Cell, True);   

      if State = null or else not Is_Invalid (State) then
         if State /= null then
            State.Set_Invalid (True);
         end if;
	 
         -- Recursively invalidates all descendants
	 
         Childs := Cell.Get_Children_List;
         for Child of Childs loop
            declare
               Ch : access Cell_Record'Class := Child;
            begin
               V.Invalidate (Ch);
            end;
         end loop;
	 
         -- Propagates invalidation to all connected edges
	   
         Edges := Cell.Get_Edges_List;
         for Edge of Edges loop
            declare
               E : access Cell_Record'Class := Edge;
            begin
               V.Invalidate (E);
            end;
         end loop;
      end if;
   end Invalidate;
   
   --------------
   -- Validate --
   --------------
   
   procedure Validate (V : access View_Record) is
      
      Graph_Bounds : Rectangle_Record;
      State        : access Cell_State_Record'Class;
      Cell         : access Cell_Record'Class;
      Model        : access Model_Interface'Class;
   begin
      if V.Current_Root /= null then
         Cell := V.Current_Root;
      else
         Model := V.Graph.Get_Model;
         Cell := Model.Get_Root;
      end if;
      
      V.Validate_Cell (Cell);
      State := V.Validate_Cell_State (Cell);
      Graph_Bounds := V.Get_Bounding_Box (State);
      
      V.Set_Graph_Bounds (Graph_Bounds);
   end Validate;
   
   
   ----------------------
   -- Get_Bounding_Box --
   ----------------------
   
   function Get_Bounding_Box
     (V     : access View_Record;
      State : access Cell_State_Record'Class) return Rectangle_Record is
   begin
      return V.Get_Bounding_Box (State, True);
   end Get_Bounding_Box;
   
   ----------------------
   -- Get_Bounding_Box --
   ----------------------
   
   function Get_Bounding_Box
     (V       : access View_Record;
      State   : access Cell_State_Record'Class;
      Recurse : Boolean) return Rectangle_Record is
      
      Bbox   : Rectangle_Record := No_Rectangle_Record;
      Model  : access Model_Interface'Class;
      Bounds : Rectangle_Record := No_Rectangle_Record;
      Childs : Cells_Lists.List;
      Cell   : access Cell_Record'Class := null;
   begin
      if State /= null then
         Bbox := State.Get_Bounding_Box;
         
         if Recurse then
            Model := V.Graph.Get_Model;
            Cell := State.Get_Cell;
            
            Childs := Cell.Get_Children_List;
            for Child of Childs loop
               Bounds := V.Get_Bounding_Box (V.Get_State (Child), True);
               if Bounds /= No_Rectangle_Record then
                  if Bbox = No_Rectangle_Record then
                     Bbox := Bounds;
                  else
                     Add (Bbox, Bounds);
                  end if;
               end if;
            end loop;
         end if;
      end if;
      
      return Bbox;
   end Get_Bounding_Box;
   
   -------------------
   -- Validate_Cell -- 
   -------------------
   
   procedure Validate_Cell
     (V       : access View_Record;
      Cell    : access Cell_Record'Class) is
      
      State  : access Cell_State_Record'Class;
      Model  : access Model_Interface'Class;
      Childs : Cells_Lists.List := Cells_Lists.Empty_List;
   begin
      if Cell /= null then
         State := V.Get_State (Cell, True);
         Model := V.Graph.Get_Model;
         Childs := Cell.Get_Children_List;
         for Child of Childs loop
            declare
               C      : access Cell_Record'Class;
               Isroot : Boolean := Cell = V.Current_Root;
            begin
               C := Child;
               V.Validate_Cell
                 (C);
            end;
         end loop;
      end if;
   end Validate_Cell;
   
   -------------------------
   -- Validate_Cell_State --
   -------------------------
   
   function Validate_Cell_State
     (V    : access View_Record;
      Cell : access Cell_Record'Class) 
      return access Cell_State_Record'Class is
   begin
      return V.Validate_Cell_State (Cell, True);
   end Validate_Cell_State;

   -------------------------
   -- Validate_Cell_State --
   -------------------------
     
   function Validate_Cell_State
     (V       : access View_Record;
      Cell    : access Cell_Record'Class;
      Recurse : Boolean) return access Cell_State_Record'Class is
      
      State           : access Cell_State_Record'Class := null;
      Terminal_State  : access Cell_State_Record'Class := null;
      Terminal        : access Cell_Record'Class;
      Model           : access Model_Interface'Class;
      Childs          : Cells_Lists.List := Cells_Lists.Empty_List;
      Dummy_State     : access Cell_State_Record'Class;
      Parent          : access Cell_Record'Class;
      Inv             : Boolean;
   begin
      if Cell /= null then
         State := V.Get_State (Cell);
	 
         if State /= null then
            Model := V.Graph.Get_Model;
	    
            Inv := State.Is_Invalid;
            if State.Is_Invalid then
               State.Set_Invalid (False);
	       
               if Cell /= V.Current_Root then
                  Parent      := Model.Get_Parent (Cell);
                  Dummy_State := V.Validate_Cell_State (Parent, False);
               end if;

               Terminal := V.Get_Visible_Terminal (Cell, True);
               if Terminal /= null then
                  Terminal_State := V.Validate_Cell_State (Terminal, False);
                  State.Set_Visible_Terminal_State (Terminal_State, True);
               end if;
		   
               Terminal := V.Get_Visible_Terminal (Cell, False);
               if Terminal /= null then
                  Terminal_State := V.Validate_Cell_State (Terminal, False);
                  State.Set_Visible_Terminal_State (Terminal_State, False);
               end if;
               
               V.Update_Cell_State (State);
	       
               if Model.Is_Edge (Cell) or Model.Is_Vertex (Cell) then
                  V.Update_Label_Bounds (State);
                  V.Update_Bounding_Box (State);
               end if;
            end if;
	    
            if Recurse then
               Childs := Cell.Get_Children_List;
               -- Childs := Model.Get_Children (Cell);
               for Child of Childs loop
                  declare
                     Child_Access : Cell_Class_Ptr := Child;
                  begin
                     Dummy_State := V.Validate_Cell_State (Child_Access);
                  end;
               end loop;
            end if;
         end if;
      end if;
      
      return State;
   end Validate_Cell_State;
   
   -----------------------
   -- Update_Cell_State --
   -----------------------
   
   procedure Update_Cell_State
     (V     : access View_Record;
      State : access Cell_State_Record'Class) is
      
      Model       : access Model_Interface'Class;
      PState      : access Cell_State_Record'Class;
      Offset      : Point_Record;
      Geo         : access Cell_Geometry_Record'Class;
      Origin      : Point_Record;
      Orig        : Point_Record;
      Inside      : Rectangle_Record;
      X           : Coordinate;
      Y           : Coordinate;
      Cell        : access Cell_Record'Class;
      Parent      : access Cell_Record'Class;
   begin
      Inside := Inside_Rectangle (State);
      
      State.Set_Absolute_Offset (Zero_Point_Record);
      State.Set_Origin (Zero_Point_Record);
      State.Set_Length (0.0);
      
      if Get_Cell (State) /= V.Current_Root then

         Model  := V.Graph.Get_Model;
         Cell   := State.Get_Cell;
         Parent := Model.Get_Parent (Cell);
         Pstate := V.Get_State (Parent);
	 
         -- Compute the asbolute position of the cell (add the origin deplacemnt
         -- of the parent).
	 
         if Pstate /= null and then Get_Cell (Pstate) /= V.Current_Root then
            X := Get_X (State.Get_Origin) + Get_X (Pstate.Get_Origin);
            Y := Get_Y (State.Get_Origin) + Get_Y (Pstate.Get_Origin);
            State.Set_Origin (Point_Record'(X, Y));
         end if;
	 
         -- Currently Get_Child_Offset_For_Cell No_Point_Record
	 
         Offset := V.Graph.Get_Child_Offset_For_Cell (State.Get_Cell);
	 
         if Offset /= No_Point_Record then
            X := Get_X (State.Get_Origin) + Get_X (Offset);
            Y := Get_Y (State.Get_Origin) + Get_Y (Offset);
            State.Set_Origin (Point_Record'(X, Y));
         end if;
	 
         -- If Geometry is null then the cell is not drawn 
	 
         Geo := V.Graph.Get_Cell_Geometry (Get_Cell (State));
	 
         if Geo /= null then
            if not Is_Edge (Model, Get_Cell (State)) then
               Origin := Get_Origin (State);
               Offset := Get_Offset (Geo);
	       
               if Offset = No_Point_Record then
                  Offset := Zero_Point_Record;
               end if;
	       
               if Is_Relative (Geo) and Pstate /= null then
		  
                  -- Vertex Relative Mode when Vertex Parent is an Edge
		  
                  if Model.Is_Edge (Get_Cell (Pstate)) then
                     Orig := V.Get_Point (Pstate, Geo);
		     
                     if Orig /= No_Point_Record then
                        X := Get_X (Origin) 
                          + (Get_X (Orig) / V.Scale) 
                          - Get_X (Get_Origin (Pstate))
                          - Get_X (V.Translate);
			
                        Y := Get_Y (Origin) 
                          + (Get_Y (Orig) / V.Scale) 
                          - Get_Y (Get_Origin (Pstate))
                          - Get_Y (V.Translate);
			
                        Set_X (Origin, X);
                        Set_Y (Origin, Y);
			
                        State.Set_Origin (Origin);
                     end if;
		     
                     -- Vertex Relative Mode when Vertex Parent is a Vertex
                     -- X,Y are percent of Parent Vertex Witdh and Height
		     
                  else
                     X := Get_X (Origin) 
                       + Get_X (Geo) * Get_Width (Pstate) / V.Scale
                       + Get_X (Offset);
		     
                     Y := Get_Y (Origin) 
                       + Get_Y (Geo) * Get_Height (Pstate) / V.Scale
                       + Get_Y (Offset);
		     
                     Set_X (Origin, X);
                     Set_Y (Origin, Y);
		     
                     State.Set_Origin (Origin);
                  end if;
		  
                  -- Non Relative Mode
		  
               else
                  State.Set_Absolute_Offset 
                    (Point_Record'
                       (V.Scale * Get_X (Offset), V.Scale * Get_Y (Offset)));
		  
                  Set_X (Origin, Get_X (Origin) + Get_X (Geo));
                  Set_Y (Origin, Get_Y (Origin) + Get_Y (Geo));
		  
                  State.Set_Origin (Origin);
               end if;
            end if;
	    
            State.Set_X
              (V.Scale * (Get_X (V.Translate) + Get_X (State.Get_Origin)));
	    
            State.Set_Y
              (V.Scale * (Get_Y (V.Translate) + Get_Y (State.Get_Origin)));
	    
            State.Set_Width  (V.Scale * Get_Width  (Geo));
            State.Set_Height (V.Scale * Get_Height (Geo));

            if Model.Is_Vertex (Get_Cell (State)) then
               V.Update_Vertex_State (State, Geo);
            end if;

            if Model.Is_Edge (Get_Cell (State)) then
               V.Update_Edge_State (State, Geo);
            end if;
	    
            -- Updates the cached label
	    
            V.Update_Label (State);
         end if;
      end if;
      
   end Update_Cell_State;
   
   -------------------------
   -- Update_Vertex_State --
   -------------------------
   
   procedure Update_Vertex_State
     (V     : access View_Record;
      State : access Cell_State_Record'Class;
      Geo   : access Cell_Geometry_Record'Class) is
   begin
      -- LATER: Add support for rotation
      V.Update_Vertex_Label_Offset (State);
   end Update_Vertex_State;
   -- Validates the given cell state.
   
   -----------------------
   -- Update_Edge_State --
   -----------------------
   
   procedure Update_Edge_State
     (V     : access View_Record;
      State : access Cell_State_Record'Class;
      Geo   : access Cell_Geometry_Record'Class) is
      
      Source : access Cell_State_Record'Class;
      Target : access Cell_State_Record'Class;
      Model  : access Model_Interface'Class;
   begin
      Source := State.Get_Visible_Terminal_State (True);
      Target := State.Get_Visible_Terminal_State (False);
      
      -- This will remove edges with no terminals and no terminal points as such
      -- edges are invalid and produce NPEs in the edge styles. Also removes 
      -- connected edges that have no visible terminals.
      
      Model := Get_Model (V.Graph);
      
      if (Model.Get_Terminal (Get_Cell (State), True) /= null
          and Source = null)
        or (Source = null and Geo.Get_Terminal_Point (True) = No_Point_Record)
        or (Model.Get_Terminal (Get_Cell (State), False) /= null
            and Target = null)
        or (Target = null and Geo.Get_Terminal_Point (False) = No_Point_Record)
      then
         V.Clear (Get_Cell (State), True, True);
	 
      else
         V.Update_Fixed_Terminal_Points (State, Source, Target);
         V.Update_Points (State, Geo.Get_Points, Source, Target);
         V.Update_Floating_Terminal_Points (State, Source, Target);
	 
         if (State.Get_Cell /= V.Get_Current_Root)
           and (Get_Absolute_Point_Count (State) < 2
                or State.Get_First_Absolute_Point = No_Point_Record 
                or State.Get_Last_Absolute_Point = No_Point_Record)
         then
	    
            -- This will remove edges with invalid points from the list of
            -- states in the view. Happens if the one of the terminals and the
            -- corresponding terminal point is null.
     
            V.Clear (Get_Cell (State), True, True);
         else
            V.Update_Edge_Bounds (State);
            State.Set_Absolute_Offset (V.Get_Point (State, Geo));
         end if;
      end if;
      
   end Update_Edge_State;
   
   --------------------------------
   -- Update_Vertex_Label_Offset --
   --------------------------------
   
   procedure Update_Vertex_Label_Offset
     (V     : access View_Record;
      State : access Cell_State_Record'Class) is
      
      use Artics;
      
      Horizontal : Name_Id := Graph.Utils.Get_String 
        (State.Get_Style, Style_Label_Position, Align_Center);
	
      Vertical : Name_Id := Graph.Utils.Get_String
        (State.Get_Style, Style_Vertical_Label_Position, Align_Middle);
      
      X : Coordinate;
      Y : Coordinate;
   begin
      X := Get_X (State.Get_Absolute_Offset);  -- State.Get_X;
      Y := Get_Y (State.Get_Absolute_Offset);  -- State.Get_Y;
      
      if Horizontal = ALIGN_LEFT then
         X := X - State.Get_Width;
	 
      elsif Horizontal = ALIGN_RIGHT then
         X := X + State.Get_Width;
      end if;
      
      if Vertical = ALIGN_TOP then
         Y := Y - State.Get_Height;
	 
      elsif Vertical = ALIGN_BOTTOM then
         Y := Y + State.Get_Height;
      end if;
      
      State.Set_Absolute_Offset (Point_Record'(X, Y));
   end Update_Vertex_Label_Offset;
   
   ------------------
   -- Update_Label --
   ------------------
   
   procedure Update_Label
     (V     : access View_Record;
      State : access Cell_State_Record'Class) is
      
      Label : String := V.Graph.Get_Label (State.Get_Cell);
      Style : Strings_Maps.Map := State.Get_Style;
      W     : Coordinate;
      Lines : Strings_Lists.List;
      --  			StringBuffer buffer = new StringBuffer();
      
      Wrap    : Name_Id := String_Find ("wrap");
      No_Wrap : Name_Id := String_Find ("nowrap");
   begin
      -- Applies word wrapping to non-HTML labels and stores the result in the
      -- state
      
      Log_Line ("Update_Label: label => " & Label);
      if Label /= "" 
        and not V.Graph.Is_Html_Label (State.Get_Cell)
        and not V.Graph.Get_Model.Is_Edge (State.Get_Cell)
        and Get_String (Style, STYLE_WHITE_SPACE, No_Wrap) = Wrap
      then
         W := V.Get_Word_Wrap_Width (State);

         -- The lines for wrapping within the given width are calculated for no
         -- scale. The reason for this is the granularity of actual displayed
         -- font can cause the displayed lines to change based on scale. A 
         -- factor is used to allow for different overalls widths, it ensures 
         -- the largest font size/scale factor still stays within the bounds. 
         -- All this ensures the wrapped lines are constant overing scaling, at
         -- the expense the label bounds will vary.
         --           Lines := Word_Wrap 
         --             (Label,
         --              Get_Font_Metrics
         --                (Get_Font (State.Get_Style)), W * LABEL_SCALE_BUFFER);
	   
         declare
            Result : Unbounded_String := Null_Unbounded_String;
         begin
            for Line of Lines loop
               declare
                  S : String := Get_String (Line);
               begin
                  Append (Result, S & Ascii.Lf); 
               end;
            end loop;
	   
            State.Set_Label (To_String (Result));
            return;
         end;
      end if;
      
      State.Set_Label (Label);
      
   end Update_Label;
   
   -------------------------
   -- Get_Word_Wrap_Width --
   -------------------------
   
   function Get_Word_Wrap_Width
     (V     : access View_Record;
      State : access Cell_State_Record'Class) return Coordinate is
      
      Style      : Strings_Maps.Map;
      Horizontal : Boolean;
      W          : Coordinate;
   begin 
      Style      := State.Get_Style;
      Horizontal := Is_True (Style, STYLE_HORIZONTAL, True);
      W          := 0.0;

      -- Computes the available width for the wrapped label
      if Horizontal then
         W := (State.Get_Width / V.Scale) 
           - 2.0 * Coordinate (Label_Inset) 
           - 2.0 * Get_Float (Style, Style_Spacing)
           - Get_Float (Style, Style_Spacing_Left)
           - Get_Float (Style, Style_Spacing_Right);
      else
         W := (State.Get_Height / V.Scale)
           - 2.0 * Coordinate (Label_Inset)
           - 2.0 * Get_Float (Style, Style_Spacing)
           - Get_Float (Style, Style_Spacing_Top)
           + Get_Float (Style, STYLE_SPACING_BOTTOM);
      end if;
      
      return W;
   end Get_Word_Wrap_Width;
   
   
   -------------------------
   -- Update_Label_Bounds --
   -------------------------
   
   procedure Update_Label_Bounds
     (V     : access View_Record;
      State : access Cell_State_Record'Class) is
      
      Cell          : access Cell_Record'Class := State.Get_Cell;
      Style         : Strings_Maps.Map := State.Get_Style;
      Overflow      : Name_Id := Get_String (Style, STYLE_OVERFLOW, No_Name);
      Fill_Name     : Name_Id := String_Find ("fill");
      Width_Name    : Name_Id := String_Find ("width");
      Vertex_Bounds : Rectangle_Record;
      Model         : access Model_Interface'Class := V.Graph.Get_Model;
      Geo           : access Cell_Geometry_Record'Class;
      R             : Rectangle_Record;
   begin 
      if Overflow = Fill_Name then
         State.Set_Label_Bounds (State.Inside_Rectangle);
	 
      elsif State.Get_Label /= "" then
         -- For edges, the width of the geometry is used for wrapping HTML
         --/ labels or no wrapping is applied if the width is set to 0
         Vertex_Bounds := State.Inside_Rectangle;

         if Model.Is_Edge (Cell) then
            Geo := V.Graph.Get_Cell_Geometry (Cell);

            if Geo /= null and then Geo.Get_Width > 0.0 then
               Vertex_Bounds := Rectangle_Record'
                 ((0.0, 0.0), Geo.Get_Width * V.Get_Scale, 0.0);
	      
            else
               Vertex_Bounds := No_Rectangle_Record;
            end if;
         end if;

         State.Set_Label_Bounds
           (Get_Label_Paint_Bounds
              (State.Get_Label,
               Style, 
               V.Graph.Is_Html_Label (Cell), 
               State.Get_Absolute_Offset,
               Vertex_Bounds, 
               V.Scale, 
               Model.Is_Edge (Cell)));
	    
         if Overflow = Width_Name then
            R := State.Get_Label_Bounds;
            Set_X (R, State.Get_X);
            Set_Width (R, State.Get_Width);
            State.Set_Label_Bounds (R);
         end if;
      end if;
   end Update_Label_Bounds;

   
   -------------------------
   -- Update_Bounding_Box --
   -------------------------
   
   procedure Update_Bounding_Box
     (V     : access View_Record;
      State : access Cell_State_Record'Class) is -- return Rectangle_Record is
      
      Rect         : Rectangle_Record;
      Style        : Strings_Maps.Map;
      Stroke_Width : Coordinate;
      Stroke       : Coordinate;
      Round        : Coordinate;
      Model        : access Model_Interface'Class := V.Graph.Get_Model;
      Ms           : Integer;
      W            : Coordinate;
      H            : Coordinate;
      X            : Coordinate;
      Y            : Coordinate;
      Rotation     : Coordinate;
      Bbox         : Rectangle_Record;
      Img_Align    : Name_Id;
      Img_Valign   : Name_Id;
   begin 
      -- Gets the cell bounds and adds shadows and markers
      Rect := State.Inside_Rectangle;
      Style := State.Get_Style;

      -- Adds extra pixels for the marker and stroke assuming that the border
      -- stroke is centered around the bounds and the first pixel is drawn
      -- inside the bounds
      
      Stroke := Get_Float (Style, STYLE_STROKEWIDTH, 1) * V.Scale;
      Round  := Maths.Round (Stroke);
      Stroke_Width := Maths.Max (1.0, Round);
      Stroke_Width := Stroke_Width - Maths.Max (1.0, Stroke_Width / 2.0);

      if Model.Is_Edge (State.Get_Cell) then
         Ms := 0;

         if Strings_Maps.Contains (Style, STYLE_ENDARROW) 
           or Strings_Maps.Contains (Style, STYLE_STARTARROW)
         then
            Ms := Integer (Maths.Round (Float (DEFAULT_MARKER_SIZE) * V.Scale));
         end if;

         -- Adds the strokewidth
         Grow (Rect, Coordinate (Ms) + Stroke_Width);

         -- Adds worst case border for an arrow shape
	 
         if Get_String (Style, STYLE_SHAPE, No_Name) = SHAPE_ARROW then
            Grow (Rect, Coordinate (ARROW_WIDTH) / 2.0);
         end if;
	 
      else
         Grow (Rect, Stroke_Width);
      end if;

      -- Adds extra pixels for the shadow
      
      if Is_True (Style, STYLE_SHADOW) then
         Set_Width  (Rect, Get_Width(Rect)  + Coordinate (SHADOW_OFFSET_X));
         Set_Height (Rect, Get_Height(Rect) + Coordinate (SHADOW_OFFSET_Y));
      end if;

      -- Adds oversize images in labels
      
      if Get_String (Style, STYLE_SHAPE, No_Name) = SHAPE_LABEL then
         if Get_String (Style, STYLE_IMAGE) /= No_Name then
            W := Coordinate 
              (Get_Int
                 (Style, STYLE_IMAGE_WIDTH, DEFAULT_IMAGE_SIZE)) * V.Scale;
	    
            H := Coordinate
              (Get_Int 
                 (Style, STYLE_IMAGE_HEIGHT, DEFAULT_IMAGE_SIZE)) * V.Scale;
	    
            X := State.Get_X;
            Y := 0.0;
				
            Img_Align := Get_String
              (Style, STYLE_IMAGE_ALIGN, ALIGN_LEFT);
            Img_Valign := Get_String
              (Style, STYLE_IMAGE_VERTICAL_ALIGN, ALIGN_MIDDLE);

            if Img_Align = ALIGN_RIGHT then
               X := X + State.Get_Width - W;
	       
            elsif Img_Align = ALIGN_CENTER then
               X := X + (State.Get_Width - W) / 2.0;
            end if;

            if Img_Valign = ALIGN_TOP then
               Y := State.Get_Y;
            elsif Img_Valign = ALIGN_BOTTOM then
               Y := State.Get_Y + State.Get_Height - H;
					
               -- MIDDLE
	       
            else
               Y := State.Get_Y + (State.Get_Height - H) / 2.0;
            end if;

            Add (Rect, Rectangle_Record'((X, Y), W, H));
         end if;
      end if;

      -- Adds the rotated bounds to the bounding box if the shape is rotated
      
      Rotation := Get_Float (Style, STYLE_ROTATION);
      Bbox     := Get_Bounding_Box (Rect, Rotation);
      
      -- Add the rotated bounding box to the non-rotated so that all handles
      -- are also covered
      Add (Rect, Bbox);
      
      -- Unifies the cell bounds and the label bounds
      Add (Rect, State.Get_Label_Bounds);

      State.Set_Bounding_Box (Rect);
   end Update_Bounding_Box;
   
   
   ----------------------------------
   -- Update_Fixed_Terminal_Points --
   ----------------------------------
   
   procedure Update_Fixed_Terminal_Points
     (V      : access View_Record;
      Edge   : access Cell_State_Record'Class;
      Source : access Cell_State_Record'Class;
      Target : access Cell_State_Record'Class) is
   begin 
      -- Source
      
      V.Update_Fixed_Terminal_Point
        (Edge, 
         Source, 
         True, 
         V.Graph.Get_Connection_Constraint (Edge, Source, True));
      
      -- Target
      
      V.Update_Fixed_Terminal_Point
        (Edge, 
         Target, 
         False,
         V.Graph.Get_Connection_Constraint (Edge, Target, False));
   end Update_Fixed_Terminal_Points;
   
   ---------------------------------
   -- Update_Fixed_Terminal_Point --
   ---------------------------------
   
   procedure Update_Fixed_Terminal_Point
     (V          : access View_Record;
      Edge       : access Cell_State_Record'Class;
      Terminal   : access Cell_State_Record'Class;
      Source     : Boolean;
      Constraint : access Connection_Constraint_Record'Class) is
      
      Pt   : Point_Record := No_Point_Record;
      Orig : Point_Record;
      Geo  : access Cell_Geometry_Record'Class;
   begin 
      if Constraint /= null then
         Pt := V.Graph.Get_Connection_Point (Terminal, Constraint);
      end if;
      
      -- If the edge is connected to a source and target vertex, the source
      -- point and target point of edge are the ccordinates of Source and
      -- Target Vertex. If the the edge is not connected to a vertex, then the
      -- source point and target point are the Source_Point and Target_Point
      -- fields of the Geometry of the edge. 
      
      -- So If connection constraint point is null and there is no terminal  
      -- connected, get the terminal point from cell geometry, as Source_Point
      -- and taget_Point of geometry is signifiant. 
      
      -- IMPORTANT: Source_Point and Target_Point fields of Geometry of the
      -- Edge are meaningfull only if the edge is not connected to a Vertex.
      
      if Pt = No_Point_Record and Terminal = null then
         Orig := Edge.Get_Origin;
         Geo  := V.Graph.Get_Cell_Geometry (Edge.Get_Cell);
         Pt   := Geo.Get_Terminal_Point (Source);

         if Pt /= No_Point_Record then
            Pt := Point_Record'
              (V.Scale * (Get_X (V.Translate) + Get_X (Pt) + Get_X (Orig)), 
               V.Scale * (Get_Y (V.Translate) + Get_Y (Pt) + Get_Y (Orig)));
         end if;
      end if;
      
      -- Be carrefull, here Pt can be null. If it has no contraint and is not
      -- connected and either Source_Point or Target_Point of the geometry are
      -- null
      
      Edge.Set_Absolute_Terminal_Point (Pt, Source);
   end Update_Fixed_Terminal_Point;

   
   -------------------
   -- Update_Points --
   -------------------
   
   procedure Update_Points
     (V      : access View_Record;
      Edge   : access Cell_State_Record'Class;
      Points : Point_Lists.List;
      Source : access Cell_State_Record'Class;
      Target : access Cell_State_Record'Class) is
      
      Pts        : Point_Lists.List;
   begin 
      if Edge /= null then
         Pts.Append (Edge.Get_First_Absolute_Point);
         
         if not Point_Lists.Is_Empty (Points) then 
            for Pt of Points loop
               Pts.Append (V.Transform_Control_Point (Edge, Pt));
            end loop;
         end if;

         Pts.Append (Edge.Get_Last_Absolute_Point);
	
         Edge.Set_Absolute_Points (Pts);
      end if;
      
   end Update_Points;
   
   -----------------------------
   -- Transform_Control_Point --
   -----------------------------
   
   function Transform_Control_Point
     (V     : access View_Record;
      State : access Cell_State_Record'Class;
      Point : Point_Record) return Point_Record is
      
      Origin : Point_Record;
   begin 
      Origin := State.Get_Origin;

      return Point_Record'
        (V.Scale * (Get_X (Point) + Get_X (V.Translate) + Get_X (Origin)), 
         V.Scale * (Get_Y (Point) + Get_Y (V.Translate) + Get_Y (Origin)));
   end Transform_Control_Point;
   
   --------------------
   -- Get_Edge_Style --
   --------------------
   
   function Get_Edge_Style
     (V      : access View_Record;
      Edge   : access Cell_State_Record'Class;
      Points : Point_Lists.List;
      Source : access Cell_State_Record'Class;
      Target : access Cell_State_Record'Class) return Name_Id is 
      
   begin 
      return No_Name;
   end Get_Edge_Style;
   
   -------------------------------------
   -- Update_Floating_Terminal_Points --
   -------------------------------------
   
   procedure Update_Floating_Terminal_Points
     (V      : access View_Record;
      State  : access Cell_State_Record'Class;
      Source : access Cell_State_Record'Class;
      Target : access Cell_State_Record'Class) is
      
      P0 : Point_Record := No_Point_Record;
      Pe : Point_Record := No_Point_Record;
   begin 
      P0 := State.Get_First_Absolute_Point;
      Pe := State.Get_Last_Absolute_Point;
      
      if Pe = No_Point_Record and Target /= null then
         V.Update_Floating_Terminal_Point (State, Target, Source, False);
      end if;

      if P0 = No_Point_Record and Source /= null then
         V.Update_Floating_Terminal_Point (State, Source, Target, True);
      end if;
      
   end Update_Floating_Terminal_Points;

   
   ------------------------------------
   -- Update_Floating_Terminal_Point --
   ------------------------------------
   
   procedure Update_Floating_Terminal_Point
     (V      : access View_Record;
      Edge   : access Cell_State_Record'Class;
      Start  : access Cell_State_Record'Class;
      Ends   : access Cell_State_Record'Class;
      Source : Boolean) is
      
      Start_State     : access Cell_State_Record'Class;
      Next            : Point_Record;
      Border          : Coordinate;
      Pt              : Point_Record;
      Perimeter_Style : Name_Id;
   begin 
      Start_State := V.Get_Terminal_Port (Edge, Start, Source);
      Next        := V.Get_Next_Point (Edge, Ends, Source);
      Border      := Get_Float (Edge.Get_Style, STYLE_PERIMETER_SPACING);
      
      if Source then
         Perimeter_Style := STYLE_SOURCE_PERIMETER_SPACING;
      else
         Perimeter_Style := STYLE_TARGET_PERIMETER_SPACING;
      end if;
				
      Border := Border + Get_Float (Edge.Get_Style, Perimeter_Style);
      
      Pt := V.Get_Perimeter_Point
        (Start_State, Next, V.Graph.Is_Orthogonal (Edge), Border);
      
      Edge.Set_Absolute_Terminal_Point (Pt, Source);
   end Update_Floating_Terminal_Point;

   
   -----------------------
   -- Get_Terminal_Port --
   -----------------------
   
   function Get_Terminal_Port
     (V        : access View_Record;
      State    : access Cell_State_Record'Class;
      Terminal : access Cell_State_Record'Class;
      Source   : Boolean) return access Cell_State_Record'Class is
      
      Key    : Name_Id;
      Id     : Name_Id;
      Tmp    : access Cell_State_Record'Class;
      Model  : access Model_Interface'Class;
      Result : access Cell_State_Record'Class;
   begin 
      Result := Terminal;
      
      if Source then
         Key := STYLE_SOURCE_PORT;
      else
         Key := STYLE_TARGET_PORT;
      end if;
      
      Id := Get_String (State.Get_Style, Key);

      if Id /= No_Name then
         Model := V.Graph.Get_Model;
	
         Tmp := V.Get_State (Model.Get_Cell (Get_String (Id)));

         -- Only uses ports where a cell state exists
	
         if Tmp /= null then
            Result := Tmp;
         end if;
      end if;

      return Result;
   end Get_Terminal_Port;

   
   -------------------------
   -- Get_Perimeter_Point --
   -------------------------
   
   function Get_Perimeter_Point
     (V          : access View_Record;
      Terminal   : access Cell_State_Record'Class;
      Next       : Point_Record;
      Orthogonal : Boolean) return Point_Record is
   begin 
      return V.Get_Perimeter_Point (Terminal, Next, Orthogonal, 0.0);
   end Get_Perimeter_Point;
   
   
   -------------------------
   -- Get_Perimeter_Point --
   -------------------------
   
   function Get_Perimeter_Point
     (V          : access View_Record;
      Terminal   : access Cell_State_Record'Class;
      Next       : Point_Record;
      Orthogonal : Boolean;
      Border     : Coordinate) return Point_Record is
      
      Point     : Point_Record := No_Point_Record;
      Perimeter : Name_Id;
      Bounds    : Rectangle_Record;
   begin
      if Terminal /= null then
         Perimeter := V.Get_Perimeter_Function (Terminal);
	 
         if Perimeter /= No_Name and then Next /= No_Point_Record then
            Bounds := V.Get_Perimeter_Bounds (Terminal, Border);

         end if;
	 
         if Point = No_Point_Record then 
            Point := V.Get_Point (Terminal);
         end if;
      end if;

      return Point;
   end Get_Perimeter_Point;
   
   
   --------------------------
   -- Get_Routing_Center_X --
   --------------------------
   
   function Get_Routing_Center_X
     (V     : access View_Record;
      State : access Cell_State_Record'Class) return Coordinate is
      
      F     : Coordinate;
      Style : Strings_Maps.Map;
   begin 
      Style := State.Get_Style;
      F := Get_Float (Style, STYLE_ROUTING_CENTER_X);
	 
      return State.Get_Center_X + F * State.Get_Width;
   end Get_Routing_Center_X;
   
   
   --------------------------
   -- Get_Routing_Center_Y --
   --------------------------
   
   function Get_Routing_Center_Y
     (V     : access View_Record;
      State : access Cell_State_Record'Class) return Coordinate is
      
      F     : Coordinate;
      Style : Strings_Maps.Map;
   begin 
      Style := State.Get_Style;
      F := Get_Float (Style, STYLE_ROUTING_CENTER_Y);

      return State.Get_Center_Y + F * State.Get_Height;
   end Get_Routing_Center_Y;
   
   
   --------------------------
   -- Get_Perimeter_Bounds --
   --------------------------
   
   function Get_Perimeter_Bounds
     (V          : access View_Record;
      Terminal   : access Cell_State_Record'Class;
      Border     : Coordinate) return Rectangle_Record is
      
      Bord : Coordinate := Border;
   begin 
      if Terminal /= null then
         Bord := Border 
           + Get_Float (Terminal.Get_Style, STYLE_PERIMETER_SPACING);
      end if;

      return Terminal.Get_Perimeter_Bounds (Bord * V.Scale);
   end Get_Perimeter_Bounds;
   
   
   ----------------------------
   -- Get_Perimeter_Function --
   ----------------------------
   
   function Get_Perimeter_Function
     (V     : access View_Record;
      State : access Cell_State_Record'Class) return Name_Id is
      
      Style : Strings_Maps.Map;
   begin 
      Style := State.Get_Style;
      if Strings_Maps.Contains (Style, STYLE_PERIMETER) then
         return Strings_Maps.Element (Style, STYLE_PERIMETER);
      else
         return No_Name;
      end if;
   end Get_Perimeter_Function;
   
   
   --------------------
   -- Get_Next_Point --
   --------------------
   
   function Get_Next_Point
     (V        : access View_Record;
      Edge     : access Cell_State_Record'Class;
      Opposite : access Cell_State_Record'Class;
      Source   : Boolean) return Point_Record is
      
      use Point_Lists;
      
      Pts   : Point_Lists.List;
      Point : Point_Record := No_Point_Record;
   begin 
      Pts := Edge.Get_Absolute_Points;

      if Pts /= Point_Lists.Empty_List 
        and Integer (Point_Lists.Length (Pts)) >= 2 
      then
         if Source then
            Point := Get_Last_Point (Pts);
         else
            Point := Get_Last_Minus_One_Point (Pts);
         end if;
      end if;

      if Point = No_Point_Record and Opposite /= null then
         Point := Point_Record'
           (Get_Center_X (Opposite), Get_Center_Y(Opposite));
      end if;
      
      return Point;
   end Get_Next_Point;
   
   
   --------------------------
   -- Get_Visible_Terminal --
   --------------------------
   
   function Get_Visible_Terminal
     (V        : access View_Record;
      Edge     : access Cell_Record'Class;
      Source   : Boolean) return access Cell_Record'Class is
      
      Model  : access Model_Interface'Class;
      Result : access Cell_Record'Class;
      Best   : access Cell_Record'Class;
   begin 
      Model  := V.Graph.Get_Model;
      Result := Model.Get_Terminal (Edge, Source);
      Best   := Result;
      
      while Result /= null and Result /= V.Current_Root loop
         Result := Model.Get_Parent (Result);
      end loop;

      -- Checks if the result is not a layer
      
      --  if Model.Get_Parent (Best) = Model.Get_Root then
      --  	 Best := null;
      --  end if;
      
      return Best;
   end Get_Visible_Terminal;

   
   ------------------------
   -- Update_Edge_Bounds --
   ------------------------
   
   procedure Update_Edge_Bounds
     (V     : access View_Record;
      State : access Cell_State_Record'Class) is
      
      Points      : Point_Lists.List;
      P0          : Point_Record := No_Point_Record;
      Pe          : Point_Record := No_Point_Record;
      Dx          : Coordinate;
      Dy          : Coordinate;
      Length      : Coordinate;
      Segment     : Coordinate;
      Segments    : Coordinates_Lists.List;
      Pt          : Point_Record;
      MinX        : Coordinate;
      MinY        : Coordinate; 
      MaxX        : Coordinate;
      MaxY        : Coordinate;
      Marker_Size : Coordinate;
      Cur         : Point_Lists.Cursor;
      Tmp         : Point_Record;
   begin 
      Points := State.Get_Absolute_Points;
      P0 := State.Get_First_Absolute_Point;
      Pe := State.Get_Last_Absolute_Point;

      if Get_X (P0) /= Get_X (Pe) or Get_Y (P0) /= Get_Y (Pe) then
         Dx := Get_X (Pe) - Get_X (P0);
         Dy := Get_Y (Pe) - Get_Y (P0);
         State.Set_Terminal_Distance (Sqrt (Dx * Dx + Dy * Dy));
				      
      else
         State.Set_Terminal_Distance (0.0);
      end if;

      Length := 0.0;
      -- ouble[] segments = new double[points.size() - 1];
      Pt := P0;

      MinX := Get_X (Pt);
      MinY := Get_Y (Pt);
      MaxX := MinX;
      MaxY := MinY;
      
      Cur := Point_Lists.First (Points);
      
      -- Skip first point P0
      
      Point_Lists.Next (Cur);
      
      while Point_Lists.Has_Element (Cur) loop
         Tmp := Point_Lists.Element (Cur);
	 
         if Tmp /= No_Point_Record then
	    
            -- Distance between the two consecutive points
            Dx := Get_X (Pt) - Get_X(Tmp);
            Dy := Get_Y (Pt) - Get_Y (Tmp);
            Segment := Sqrt (Dx * Dx + Dy * Dy);
	    
            Coordinates_Lists.Append (Segments, Segment);
	    
            -- Total Distance of edge
            Length := Length  + Segment;
	    
            Pt := Tmp;
	    
            -- Min and Max x,y to later compute width and height of the edge
            -- bound
	    
            MinX := Maths.Min (Get_X (Pt), MinX);
            MinY := Maths.Min (Get_Y (Pt), MinY);
            MaxX := Maths.Max (Get_X (Pt), MaxX);
            MaxY := Maths.Max (Get_Y (Pt), MaxY);
         end if;
	 
         Point_Lists.Next (Cur);
      end loop;

      State.Set_Length (Length);
      State.Set_Segments (Segments);
      
      Marker_Size := 1.0; -- TODO: include marker size
      
      State.Set_X (MinX);
      State.Set_Y (MinY);
      State.Set_Width  (Maths.Max (Marker_Size, MaxX - MinX));
      State.Set_Height (Maths.Max (Marker_Size, MaxY - MinY));
   end Update_Edge_Bounds;
   
   ---------------
   -- Get_Point --
   ---------------
   
   function Get_Point 
     (V     : access View_Record;
      State : access Cell_State_Record'Class) return Point_Record is
   begin 
      return V.Get_Point (State, null);
   end Get_Point;
   
   ---------------
   -- Get_Point --
   ---------------
   
   function Get_Point
     (V        : access View_Record;
      State    : access Cell_State_Record'Class;
      Geometry : access Cell_Geometry_Record'Class) return Point_Record is
      
      X           : Coordinate;
      Y           : Coordinate;
      Gx          : Coordinate;
      Point_Count : Integer;
      Dist        : Coordinate;
      Segments    : Coordinates_Lists.List;
      Segment     : Coordinate := 0.0;
      Length      : Coordinate;
      Index       : Integer;
      Factor      : Coordinate;
      P0          : Point_Record := No_Point_Record;
      Pe          : Point_Record := No_Point_Record;
      Gy          : Coordinate;
      OffsetX     : Coordinate;
      OffsetY     : Coordinate;
      Offset      : Point_Record;
      Dx          : Coordinate;
      Dy          : Coordinate;
      Nx          : Coordinate;
      Ny          : Coordinate;
      Cur         : Coordinates_Lists.Cursor;
   begin 
      X        := State.Get_Center_X;
      Y        := State.Get_Center_Y;
      Segments := State.Get_Segments;
      
      if not Coordinates_Lists.Is_Empty (Segments) 
        and (Geometry = null or else Geometry.Is_Relative)
      then
         if Geometry /= null then
            Gx := Geometry.Get_X / 2.0;
         else
            Gx := 0.0;
         end if;
	 
         Point_Count := State.Get_Absolute_Point_Count;
         Dist        := (Gx + 0.5) * State.Get_Length;
         Length      := 0.0;
         Index       := 1;
         Segment     := 0.0;
	 
         Cur := Coordinates_Lists.First (Segments);
         while Coordinates_Lists.Has_Element (Cur) loop
            Segment := Coordinates_Lists.Element (Cur);
            exit when (Dist > Length + Segment);
            Length := Length + Segment;
            Index := Index + 1;
            Coordinates_Lists.Next (Cur);
         end loop;
	 
         if Segment = 0.0 then
            Factor := 0.0;
         else
            Factor := (Dist - Length) / Segment;
         end if;
	 
         P0 := State.Get_Absolute_Point (Index - 1);
         Pe := State.Get_Absolute_Point (Index);
	 
         if P0 /= No_Point_Record and Pe /= No_Point_Record then
            Gy := 0.0;
            Offsetx := 0.0;
            Offsety := 0.0;
            
            if Geometry /= null then
               Gy := Geometry.Get_Y;
               Offset := Geometry.Get_Offset;

               if Offset /= No_Point_Record then
                  Offsetx := Get_X (Offset);
                  Offsety := Get_Y (Offset);
               end if;
            end if;

            Dx := Get_X (Pe) - Get_X (P0);
            Dy := Get_Y (Pe) - Get_Y (P0);
            
            if Segment = 0.0 then
               Nx := 0.0;
               Ny := 0.0;
            else
               Nx := Dy / Segment;
               Ny := Dx / Segment;
            end if;
            
            X := Get_X (P0) + Dx * Factor + (Nx * Gy + Offsetx) * V.Scale;
            Y := Get_Y (P0) + Dy * Factor - (Ny * Gy - Offsety) * V.Scale;
         end if;
	 
      elsif Geometry /= null then
         Offset := Geometry.Get_Offset;
	 
         if Offset /= No_Point_Record then
            X := X + Get_X (Offset);
            Y := Y + Get_Y (Offset);
         end if;
      end if;
      return Point_Record'(X, Y);
   end Get_Point;
   
   ------------------------
   -- Get_Relative_Point --
   ------------------------
   
   function Get_Relative_Point
     (V          : access View_Record;
      Edge_State : access Cell_State_Record'Class;
      X          : Coordinate;
      Y          : Coordinate) return Point_Record is
      
      --        Model        : access Model_Interface'Class;
      --        Geometry     : access Cell_Geometry_Record;
      --        Point_Count  : Integer;
      --        Total_Length : Coordinate;
      --        Segments     : Coordinates_Lists.List;
      --        P0           : Point_Record;
      --        Pe           : Point_Record;
      --        -- Line2D line = new Line2D.Double(p0.getPoint(), pe.getPoint());
      --        Min_Dist     : Coordinate;
      --        Index        : Integer;
      --        Tmp          : Coordinate;
      --        Length       : Coordinate;
   begin 
      --        Model := V.Graph.Get_Model;
      --        Geometry := Model.Get_Geometry (Edge_State.Get_Cell);
      --  
      --        if Geometry /= null then
      --  	 Point_Count = Edge_State.Get_Absolute_Point_Count;
      --  
      --  	 if Geometry.Is_Relative and Point_Count > 1 then
      --  	    Total_Length = Edge_State.Get_Length;
      --  	    Segments := Edge_State.Get_Segments;
      --  
      --  	    -- Works which line segment the point of the label is closest to
      --  	    
      --  	    P0 := Edge_State.Get_Absolute_Point(0);
      --  	    Pe := Edge_State.Get_Absolute_Point(1);
      --  	    line = new Line2D.Double(p0.getPoint(), pe.getPoint());
      --  	    Min_Dist := line.ptSegDistSq(x, y);
      --  
      --  	    Index := 0;
      --  	    Tmp := 0.0;
      --  	    Length := 0.0;
      --  
      --  	    for (int i = 2; i < pointCount; i++)
      --  	    {
      --  	    tmp += segments[i - 2];
      --  	    pe = edgeState.getAbsolutePoint(i);
      --  	    
      --  	    line = new Line2D.Double(p0.getPoint(), pe.getPoint());
      --  	    double dist = line.ptSegDistSq(x, y);
      --  	    
      --  	    if (dist < minDist)
      --  	      {
      --  	      minDist = dist;
      --  	      index = i - 1;
      --  	      length = tmp;
      --  	      }
      --  
      --  		p0 = pe;
      --  	      }
      --  
      --  				double seg = segments[index];
      --  				p0 = edgeState.getAbsolutePoint(index);
      --  				pe = edgeState.getAbsolutePoint(index + 1);
      --  
      --  				double x2 = p0.getX();
      --  				double y2 = p0.getY();
      --  
      --  				double x1 = pe.getX();
      --  				double y1 = pe.getY();
      --  
      --  				double px = x;
      --  				double py = y;
      --  
      --  				double xSegment = x2 - x1;
      --  				double ySegment = y2 - y1;
      --  
      --  				px -= x1;
      --  				py -= y1;
      --  				double projlenSq = 0;
      --  
      --  				px = xSegment - px;
      --  				py = ySegment - py;
      --  				double dotprod = px * xSegment + py * ySegment;
      --  
      --  				if (dotprod <= 0.0)
      --  				{
      --  					projlenSq = 0;
      --  				}
      --  				else
      --  				{
      --  					projlenSq = dotprod * dotprod
      --  							/ (xSegment * xSegment + ySegment * ySegment);
      --  				}
      --  
      --  				double projlen = Math.sqrt(projlenSq);
      --  
      --  				if (projlen > seg)
      --  				{
      --  					projlen = seg;
      --  				}
      --  
      --  				double yDistance = Line2D.ptLineDist(p0.getX(), p0.getY(),
      --  						pe.getX(), pe.getY(), x, y);
      --  				int direction = Line2D.relativeCCW(p0.getX(), p0.getY(),
      --  						pe.getX(), pe.getY(), x, y);
      --  
      --  				if (direction == -1)
      --  				{
      --  					yDistance = -yDistance;
      --  				}
      --  
      --  				// Constructs the relative point for the label
      --  				return new mxPoint(
      --  						Math.round(((totalLength / 2 - length - projlen) / totalLength)
      --  								* -2), Math.round(yDistance / scale));
      --  			}
      --  		}
      --  
      --  		return new mxPoint();
      --  	}
      return No_Point_Record;
   end Get_Relative_Point;

   
   ---------------------
   -- Get_Cell_States --
   ---------------------
   
   function Get_Cell_States
     (V     : access View_Record;
      Cells : Cells_Lists.List) return Cells_States_Lists.List is
      
      Result : Cells_States_Lists.List;
      State : access Cell_State_Record'Class;
   begin 
      for Cell  of Cells loop
         State := V.Get_State (Cell);

         if State /= null then
            Cells_States_Lists.Append (Result, Cell_State_Ptr (State));
         end if;
      end loop;

      return Result;
   end Get_Cell_States;
   
   
   ---------------
   -- Get_State --
   ---------------
   
   function Get_State
     (V    : access View_Record;
      Cell : access Cell_Record'Class) 
      return access Cell_State_Record'Class is
   begin 
      return V.Get_State (Cell, False);
   end Get_State;
   
   ---------------
   -- Get_State --
   ---------------
   
   function  Get_State
     (V      : access View_Record;
      Cell   : access Cell_Record'Class;
      Create : Boolean) return access Cell_State_Record'Class is
      
      use Cells_States_Maps;
      
      State : access Cell_State_Record'Class;
      Cur   : Cells_States_Maps.Map;
   begin 
      State := null;
     
      if Cell /= null then
         if Cells_States_Maps.Contains (V.States, Cell)  
         then
            State := Cells_States_Maps.Element (V.States, Cell);
         else
            State := null;
         end if;
			
         if State = null 
           and Create 
         then
            State := V.Create_State (Cell);
            Cells_States_Maps.Insert
              (V.States, Cell, Cell_State_Ptr (State));
         end if;
      end if;

      return State;
   end Get_State;

   
   ------------------
   -- Remove_State --
   ------------------
   
   procedure Remove_State
     (V    : access View_Record;
      Cell : access Cell_Record'Class) is
   begin 
      if Cell /= null then
         if Cells_States_Maps.Contains (V.States, Cell) then
            Cells_States_Maps.Delete (V.States, Cell);
         end if;
      end if;
   end Remove_State;

   
   ------------------
   -- Create_State --
   ------------------
   
   function Create_State
     (V    : access View_Record;
      Cell : access Cell_Record'Class) return access Cell_State_Record'Class
   is
      State : access Cell_State_Record;
   begin 
      State := New_Cell_State
        (View  => Object_Class_Ptr (V),
         Cell  => Cell);
      
      return State;
   end Create_State;
   
   
   ----------------
   -- Clone_View --
   ----------------
   
   function Clone_View (V : View_Ptr) return View_Ptr is
   begin
      return null;
   end Clone_View;
   
end Artics.Graph.Views;
