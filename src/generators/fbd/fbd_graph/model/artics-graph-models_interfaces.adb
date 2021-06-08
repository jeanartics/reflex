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

package body Artics.Graph.Models_Interfaces is
   
   -----------------------------
   -- Get_Directed_Edge_Count --
   -----------------------------
   
   function Get_Directed_Edge_Count
     (M        : access Model_Interface'Class;
      Cell     : access Cell_Record'Class;
      Outgoing : Boolean) return Integer is
   begin
      return Get_Directed_Edge_Count (M, Cell, Outgoing, null);
   end Get_Directed_Edge_Count;
   
   -----------------------------
   -- Get_Directed_Edge_Count --
   -----------------------------
   
   function Get_Directed_Edge_Count
     (M           : access Model_Interface'Class;
      Cell        : access Cell_Record'Class;
      Outgoing    : Boolean;
      Ignore_Edge : access Cell_Record'Class) return Integer is
      
      Edges : Cells_Lists.List;
      Count : Integer;
   begin
      Count := 0;
      
      Edges := Cell.Get_Edges_List;
      for Edge of Edges loop
         if Edge /= Ignore_Edge 
           and M.Get_Terminal (Edge, Outgoing) = Cell then
            Count := Count + 1;
         end if;
      end loop;
      
      return Count;
   end Get_Directed_Edge_Count;
   
   ---------------
   -- Get_Edges --
   ---------------
   
   function Get_Edges
     (M    : access Model_Interface'Class;
      Cell : access Cell_Record'Class) return Cells_Lists.List is
   begin
      return Get_Edges (M, Cell, True, True, True);
   end Get_Edges;
   
   ---------------------
   -- Get_Connections --
   ---------------------
   
   function Get_Connections
     (M    : access Model_Interface'Class;
      Cell : access Cell_Record'Class) return Cells_Lists.List is
   begin
      return Get_Edges (M, Cell, True, True, False);
   end Get_Connections;
   
   ------------------------
   -- Get_Incoming_Edges --
   ------------------------
   
   function Get_Incoming_Edges
     (M    : access Model_Interface'Class;
      Cell : access Cell_Record'Class) return Cells_Lists.List is
   begin
      return Get_Edges (M, Cell, True, False, False);
   end Get_Incoming_Edges;
   
   ------------------------
   -- Get_Outgoing_Edges --
   ------------------------
   
   function Get_Outgoing_Edges
     (M    : access Model_Interface'Class;
      Cell : access Cell_Record'Class) return Cells_Lists.List is
   begin
      return Get_Edges (M, Cell, False, True, False);
   end Get_Outgoing_Edges;
   
   ---------------
   -- Get_Edges --
   ---------------
   
   function Get_Edges
     (M             : access Model_Interface'Class;
      Cell          : access Cell_Record'Class;
      Incoming      : Boolean;
      Outgoing      : Boolean;
      Include_Loops : Boolean) return Cells_Lists.List is
      
      Result : Cells_Lists.List;
      Source : access Cell_Record'Class;
      Target : access Cell_Record'Class;
      Edges  : Cells_Lists.List;
   begin
      Edges := Cell.Get_Edges_List;
      for Edge of Edges loop
         Source := M.Get_Terminal (Edge, True);
         Target := M.Get_Terminal (Edge, False);

         if (Include_loops and Source = Target)
           or ((Source /= Target) 
	       and ((Incoming and Target = Cell)
		 or (Outgoing and Source = Cell)))
         then
            Result.Append (Edge);
         end if;
      end loop;
      
      return Result;
   end Get_Edges;
   
   -----------------------
   -- Get_Edges_Between --
   -----------------------
   
   function Get_Edges_Between
     (M      : access Model_Interface'Class;
      Source : access Cell_Record'Class;
      Target : access Cell_Record'Class) return Cells_Lists.List is
   begin
      return Get_Edges_Between (M, Source, Target, False);
   end Get_Edges_Between;
   
   -----------------------
   -- Get_Edges_Between --
   -----------------------
   
   function Get_Edges_Between
     (M        : access Model_Interface'Class;
      Source   : access Cell_Record'Class;
      Target   : access Cell_Record'Class;
      Directed : Boolean) return Cells_Lists.List is
      
      Terminal       : access Cell_Record'Class;
      Src            : access Cell_Record'Class;
      Trg            : access Cell_Record'Class;
      Directed_Match : Boolean;
      Opposite_Match : Boolean;
      Edges          : Cells_Lists.List;
      Result         : Cells_Lists.List;
   begin
      -- Uses The Smaller array of Connected Edges for searching the edge
      
      if M.Get_Edge_Count (Source) > M.Get_Edge_Count (Target)  then
         Terminal := Source;
      else
         Terminal := Target;
      end if;

      -- Checks if The Edge is Connected To The Correct cell and returns the
      -- first match
      
      Edges := Terminal.Get_Edges_List;
      
      for Edge of Edges loop
         Src := M.Get_Terminal (Edge, True);
         Trg := M.Get_Terminal (Edge, False);
         
         Directed_Match := (Src = Source) and (Trg = Target);
         Opposite_Match := (Trg = Source) and (Src = Target);
         
         if Directed_Match or (not Directed and Opposite_Match) then
            Result.Append (Edge);
         end if;
      end loop;

      return Result;
   end Get_Edges_Between;
   
   -------------------
   -- Get_Opposites --
   -------------------
   
   function Get_Opposites
     (M        : access Model_Interface'Class;
      Edges    : Cells_Lists.List;
      Terminal : access Cell_Record'Class) return Cells_Lists.List is
   begin
      return Get_Opposites (M, Edges, Terminal, True, True);
   end Get_Opposites;
   
   -------------------
   -- Get_Opposites --
   -------------------
   
   function Get_Opposites
     (M        : access Model_Interface'Class;
      Edges    : Cells_Lists.List;
      Terminal : access Cell_Record'Class;
      Sources  : Boolean;
      Targets  : boolean) return Cells_Lists.List is
      
      use Cells_Lists;
      
      Terminals : Cells_Lists.List := Cells_Lists.Empty_List;
      Source    : access Cell_Record'Class;
      Target    : access Cell_Record'Class;
   begin
      if Edges /= Cells_Lists.Empty_List then
         
         for Edge of Edges loop
            Source := M.Get_Terminal (Edge, True);
            Target := M.Get_Terminal (Edge, False);

            -- Checks if The Terminal is The Source of the edge and if the
            -- target should be stored in the result

            if Targets 
              and Source = Terminal 
              and Target /= null 
              and Target /= Terminal 
            then
               Terminals.Append (Target);

               -- Checks if The Terminal is The Taget of the edge and if the
               -- source should be stored in the result
               
            elsif Sources 
              and Target = Terminal 
              and Source /= null
              and Source /= Terminal 
            then
               Terminals.Append (Source);
            end if;
         end loop;    
      end if;

      return Terminals;
   end Get_Opposites;
   
   -------------------
   -- Set_Terminals --
   -------------------
   
   procedure Set_Terminals
     (M      : access Model_Interface'Class;
      Edge   : access Cell_Record'Class;
      Source : access Cell_Record'Class;
      Target : access Cell_Record'Class) is
   begin
      M.Begin_Update;
      begin
         M.Set_Terminal (Edge, Source, True);
         M.Set_Terminal (Edge, Target, False);
      exception
            when others =>
	       null;
      end;
      M.End_Update;
   end Set_Terminals;
   
   ------------------
   -- Get_Children --
   ------------------
   
   function Get_Children
     (M      : access Model_Interface'Class;
      Parent : access Cell_Record'Class) return Cells_Lists.List is
   begin
      return M.Get_Child_Cells (Parent, False, False);
   end Get_Children;
   
   ------------------------
   -- Get_Child_Vertices --
   ------------------------
   
   function Get_Child_Vertices
       (M      : access Model_Interface'Class;
	Parent : access Cell_Record'Class) return Cells_Lists.List is
   begin
      return M.Get_Child_Cells (Parent, True, False);
   end Get_Child_Vertices;
   
   ---------------------
   -- Get_Child_Edges --
   ---------------------
   
   function Get_Child_Edges
     (M      : access Model_Interface'Class;
      Parent : access Cell_Record'Class) return Cells_Lists.List is
   begin
      return M.Get_Child_Cells (Parent, False, True);
   end Get_Child_Edges;
   
   ---------------------
   -- Get_Child_Cells --
   ---------------------
   
   function Get_Child_Cells
     (M        : access Model_Interface'Class;
      Parent   : access Cell_Record'Class;
      Vertices : Boolean;
      Edges    : Boolean) return Cells_Lists.List is
      
      Childs : Cells_Lists.List := Cells_Lists.Empty_List;
      Result : Cells_Lists.List := Cells_Lists.Empty_List;
   begin
      Childs := Parent.Get_Children_List;
      
      for Child of Childs loop
         if (not Edges and not Vertices) 
           or (Edges    and M.Is_Edge (Child))
           or (Vertices and M.Is_Vertex (Child))
         then
            Result.Append (Child);
         end if;
      end loop;

      return Result;
   end Get_Child_Cells;
   
   -----------------
   -- Get_Parents --
   -----------------
   
   function Get_Parents
     (M     : access Model_Interface'Class;
      Cells : Cells_Lists.List) return Cells_Lists.list is
      
      use Cells_Lists;
      
      Parent  : access Cell_Record'Class;
      Parents : Cells_Lists.List := Cells_Lists.Empty_List;
   begin
      if Cells /= Cells_Lists.Empty_List then
         for Cell of Cells loop
            Parent := M.Get_Parent (Cell);

            if Parent /= null then
               Parents.Append (Parent);
            end if;
         end loop;
      end if;

      return Parents;
   end Get_Parents;
   
   ------------------
   -- Filter_Cells --
   ------------------
   
   function Filter_Cells
     (M      : access Model_Interface'Class;
      Cells  : Cells_Lists.List;
      Filter : access Filter_Interface'Class) return Cells_Lists.list is
      
      use Cells_Lists;
      
      Result : Cells_Lists.List := Cells_Lists.Empty_List;
   begin
      if Cells /= Cells_Lists.Empty_List  then
         for Cell of Cells loop
            if Filter.Filter (Cell) then
               Result.Append (Cell);
            end if;
         end loop;
      end if;

      return Result;
   end Filter_Cells;
   
   ---------------------
   -- Get_Descendants --
   ---------------------
   
   function Get_Descendants
     (M      : access Model_Interface'Class;
      Parent : access Cell_Record'Class) return Cells_Lists.list is
   begin
      return M.Filter_Descendants (null, Parent);
   end Get_Descendants;
   
   ------------------------
   -- Filter_Descendants --
   ------------------------
   
   function Filter_Descendants
     (M     : access Model_Interface'Class;
      Filter : access Filter_Interface'Class) return Cells_Lists.list is
   begin
      return M.Filter_Descendants (Filter, M.Get_Root);
   end Filter_Descendants;
   
   ------------------------
   -- Filter_Descendants --
   ------------------------
   
   function Filter_Descendants
     (M      : access Model_Interface'Class;
      Filter : access Filter_Interface'Class;
      Parent : access Cell_Record'Class) return Cells_Lists.list is

      use Cells_Lists;
      
      Parents     : Cells_Lists.List;
      Filter_List : Cells_Lists.List;
      Result      : Cells_Lists.List;
   begin
      if Filter = null 
        or else Filter.Filter (Parent) 
      then
         Result.Append (Parent);
      end if;

      Parents := Parent.Get_Children_List;
      for Child of Parents loop
         Filter_List := M.Filter_Descendants (Filter, Child);
         for F of Filter_List loop
            Result.Append (F);
         end loop;
      end loop;
      
      return Result;
   end Filter_Descendants;
   
   -----------------------
   -- Get_Topmost_Cells --
   -----------------------
   
   function Get_Topmost_Cells
     (M     : access Model_Interface'Class;
      Cells : Cells_Lists.List) return Cells_Lists.list is
      
      Topmost : Boolean;
      Parent  : access Cell_Record'Class;
      Result  : Cells_Lists.List;
   begin
      for Cell of Cells loop
         Topmost := True;
         Parent := M.Get_Parent (Cell);
         
         while Parent /= null loop
            if Cells.Contains (Parent) then
               Topmost := False;
               exit;
            end if;
            Parent := M.Get_Parent (Parent);
         end loop;
         
         if Topmost then
            Result.Append (Cell);
         end if;
      end loop;

      return Result;
   end Get_Topmost_Cells;
   
end Artics.Graph.Models_Interfaces;
