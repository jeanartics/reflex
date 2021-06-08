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
-- Reflex is originally developed  by the Artics team at Grenoble.          --
--                                                                          --
------------------------------------------------------------------------------

with Artics.Graph.Algos;         use Artics.Graph.Algos;
with Artics.Logutils;            use Artics.Logutils;
with Artics.Maths;               use Artics.Maths;

with Reflex.Vertex_Value; use Reflex.Vertex_Value;
with Reflex.Fbd_Util;     use Reflex.Fbd_Util;
with Ada.Text_IO; use Ada.Text_IO;

package body Reflex.Fbd_Placements is
   
   function Has_Collsion_Source_Target
     (Src       : access Cell_Record'Class;
      Trg       : access Cell_Record'Class;
      Src_Point : Point_Record;
      Trg_Point : Point_Record;
      Cells     : Cells_Lists.List) return Rectangle_Record;
   
   procedure Compute_Orthogonal_Edges (Root : access Cell_Record'Class);
      
   procedure Compute_Orthogonal_Path
     (Src       : access Cell_Record'Class;
      Trg       : access Cell_Record'Class;
      Src_Point : Point_Record;
      Trg_Point : Point_Record;
      Cells     : Cells_Lists.List;
      Points    : in out Point_Lists.List;
      Ydecal    : in out Coordinate);
      
   ---------------------
   -- Place_Vertex_In --
   ---------------------
   
   procedure Place_Vertex_In
     (This      : access Fbd_Builder_Record;
      Vertex_In : access Cell_Record'Class) is
      
      Formals_In_List  : Cells_Lists.List;
      Formals_Out_List : Cells_Lists.List;
      
      use Cells_Lists;
   begin
      Formals_In_List := 
        Vertex_Value_Ptr (Vertex_In.Get_Value).Get_Vertexs_In;
      Formals_Out_List := 
        Vertex_Value_Ptr (Vertex_In.Get_Value).Get_Vertexs_Out;
      
      if Formals_In_List /= Cells_Lists.Empty_List then
         Vertex_In.Get_Geometry.Set_X (Formal_In_Width / 2.0);
      else 
         Vertex_In.Get_Geometry.Set_X (0.0);
      end if;
   end Place_Vertex_In;
   
   ----------------------------
   -- Place_Formals_Vertices --
   ----------------------------
   
   procedure Place_Formals_Vertices
     (This      : access Fbd_Builder_Record;
      Vertex_In : access Cell_Record'Class) is
      
      Formals_In  : Cells_Lists.List;
      Formals_Out : Cells_Lists.List;
      
      Geo_Vertex_In  : Cell_Geometry_Ptr := Vertex_In.Get_Geometry;
      Geo            : Cell_Geometry_Ptr;
      Y1             : Coordinate;
      Y              : Coordinate := 1.0;
      
      use Cells_Lists;
   begin
      
      Formals_In := Vertex_Value_Ptr (Vertex_In.Get_Value).Get_Vertexs_In;
      Formals_Out := Vertex_Value_Ptr (Vertex_In.Get_Value).Get_Vertexs_Out;
      
      for Formal of reverse Formals_In loop
         Geo := Formal.Get_Geometry;
         Geo.Set_X (Geo_Vertex_In.Get_X + 1.0);
         Geo.Set_Y (Geo_Vertex_In.Get_Y + Geo_Vertex_In.Get_Height - Y);
         Y := Y + 1.0;
      end loop;
      
      Y1 := (Geo_Vertex_In.Get_Height) / Coordinate (Formals_Out.Length + 1);
      Y  := Y1;
      
      for Formal of reverse Formals_Out loop
         Geo := Formal.Get_Geometry;
         Geo.Set_X (Geo_Vertex_In.Get_X + Geo_Vertex_In.Get_Width - 1.0);
         Geo.Set_Y (Y + Geo_Vertex_In.Get_Y + 1.0);
         Y := Y + Y1;
      end loop;
      
   end Place_Formals_Vertices;
   
   --------------------
   -- Place_Vertices --
   --------------------
   
   procedure Place_Vertices
     (This      : access Fbd_Builder_Record;
      Vertex_In : access Cell_Record'Class) is

      use Cells_Lists;
   begin
      Place_Vertex_In (This, Vertex_In);
      Place_Formals_Vertices (This, Vertex_In);
   end Place_Vertices;
   
   -----------------
   -- Place_Graph --
   -----------------
   
   procedure Place_Graph
     (This      : access Fbd_Builder_Record) is
      
      Parent : access Cell_Record'Class;
      Cells  : Cells_Lists.List;
      Connex : Cells_Lists_To_Lists.List;
   begin      
      Parent := This.Get_Graph.Get_Default_Parent;
      --        Cells  := Parent.Get_Children_List;
      --        Connex := Build_Connex_Graphs (Cells);
      Make_Layer_Placement (Parent);
   end Place_Graph;
   
   ----------------
   -- Place_Cell --
   ----------------
   
   procedure Place_Cell
     (Cell : access Cell_Record;
      Xc   : Float;
      Yc   : Float) is
      
      Backs      : Cells_Lists.List;
      Geo        : access Cell_Geometry_Record;
      X          : Float;
      Y          : Float;
      Max_Width  : Float;
      Max_Height : Float;
   begin
      
      if Cell.Is_Visited then
         return;
      end if;
      Cell.Set_Visited (True);
      
      Backs := Get_Vertices_Backward (Cell);
      
      X := Xc;
      Y := Yc;
      
      Max_Width  := 0.0;
      Max_Height := 0.0;
      if not Backs.Is_Empty then
	 
         for B of Backs loop
            -- if B is In, out, loc, gloab and mode is in place it just left
            --  of block
            -- if B is In, out, loc, gloab and mode is in place it just right
            --  of block
	    
            if not Is_Global_Vertex (B) 
              and not Is_Local_Vertex (B)
              and not Is_In_Vertex (B)
              and not Is_Out_Vertex (B)
            then
               if not B.Is_Visited then
                  Place_Cell (Cell_Ptr (B), X, Y);
		  
                  Max_Width  := 
                    Max(Max_Width, Get_Max_Width (B) + X_Distance_Min_Block);
		  
                  Geo := B.Get_Geometry;
                  Max_Height := 
                    Max_Height + B.Get_Max_Height + Y_Distance_Min_Block;
		  
                  Y := Yc + Max_Height;
               else
                  Max_Width  := 
                    Max(Max_Width, Get_Max_Width (B) + X_Distance_Min_Block);
		  
                  Max_Height := 
                    Max (Max_Height, B.Get_Max_Height);
               end if;
            end if;
         end loop;
      end if;
      
      Geo := Get_Geometry (Cell);
      Geo.Set_Y (Yc);
      Geo.Set_X (Xc + Max_Width);
      
      Cell.Set_Max_Width (Max_Width + Geo.Get_Width);
      
      Cell.Set_Max_Height (Max (Max_Height, Geo.Get_Height));
      
   end Place_Cell;
   
   ---------------------
   -- Place_Cell_On_Y --
   ---------------------
   
   procedure Place_Cell_On_Y
     (Cell : access Cell_Record;
      Yc   : Float) is
      
      Backs      : Cells_Lists.List;
      Geo        : access Cell_Geometry_Record;
      Y          : Float;
      --H          : Float;
      Max_Height : Float;
   begin      
      if Cell.Is_Visited then
         return;
      end if;
      Cell.Set_Visited (True);
      
      Backs := Get_Vertices_Backward (Cell);
      
      Y := Yc;
      
      Max_Height := 0.0;
      if not Backs.Is_Empty then
         for B of Backs loop
            if not Is_Global_Vertex (B) 
              and not Is_Local_Vertex (B)
              and not Is_In_Vertex (B)
              and not Is_Out_Vertex (B)
              and not Is_Id_Vertex 
                (Vertex_Value_Ptr (B.Get_Value).Get_Cell_To_Link)
            then
               if not B.Is_Visited then
                  Place_Cell_On_Y (Cell_Ptr (B), Y);
		  
                  Geo := B.Get_Geometry;
                  Max_Height := 
                    Max_Height + B.Get_Max_Height + Y_Distance_Min_Block;
		  
                  Y := Yc + Max_Height;
               else
                  Geo := B.Get_Geometry;
                  Max_Height := 
                    Max (Max_Height, Geo.Get_Height);
               end if;
            end if;
         end loop;
      end if;
      
      Geo := Get_Geometry (Cell);
      Geo.Set_Y (Yc);
      Cell.Set_Max_Height (Max (Max_Height, Geo.Get_Height));
   end Place_Cell_On_Y;
   
   ---------------------
   -- Place_Cell_On_X --
   ---------------------
   
   procedure Place_Cell_On_X
     (Cell : access Cell_Record;
      Xc   : Float) is
      
      Backs      : Cells_Lists.List;
      Geo        : access Cell_Geometry_Record;
      X          : Float;
      Max_Width  : Float;
   begin      
      if Cell.Is_Visited then
         return;
      end if;
      Cell.Set_Visited (True);
      
      Backs := Get_Vertices_Backward (Cell);
      
      X := Xc;
      
      Max_Width  := 0.0;
      if not Backs.Is_Empty then
	 
         for B of Backs loop
            Place_Cell_On_X (Cell_Ptr (B), X);
	    
            Max_Width  := 
              Max (Max_Width, Get_Max_Width (B) + X_Distance_Min_Block);
         end loop;
      end if;
      
      Geo := Get_Geometry (Cell);
      Geo.Set_X (Xc + Max_Width);
      
      Cell.Set_Max_Width (Max_Width + Geo.Get_Width);
      
      Log_Line ("Place_Cell_On_X End => " & Get_Id (Cell));
   end Place_Cell_On_X;
   
   ---------------------
   -- Place_In_Vertex --
   ---------------------
   
   procedure Place_In_Vertex (Cell : access Cell_Record'Class) is
   begin
      null;
   end Place_In_Vertex;
   
   function Get_Connection_Vertex
     (Cell : access Cell_Record'Class) return access Cell_Record'Class is
   begin
      return null;
   end Get_Connection_Vertex;
   
   ------------------
   -- Place_In_Out --
   ------------------
   
   procedure Place_In_Out_Near (Root : access Cell_Record'Class) is
      
      Childs  : Cells_Lists.List;
      Src_Geo : access Cell_Geometry_Record;
      Trg_Geo : access Cell_Geometry_Record;
      Src     : access Cell_Record'Class;
      Trg     : access Cell_Record'Class;
      P       : Point_Record;
   begin
      Log_Line ("Place_In_Out Begin");
      Childs := Root.Get_Children_List;
      
      Unmark_Cells (Childs);
      
      for Cell of Childs loop
         if Cell.Is_Edge then
            Src := Cell.Get_Terminal (True);
            Trg := Cell.Get_Terminal (False);
	    
            Log_Line ("  Src => " & Get_Id (Src));
            Log_Line ("  Trg => " & Get_Id (Trg));
	    
            if Src /= null and Trg /= null then
	       
               if Is_In_Vertex (Src) then
                  if not Src.Is_Visited then
		     
                     Src := Top_Parent (Root, Src);
                     Src.Set_Visited (True);
                     Src_Geo := Src.Get_Geometry;
		     
                     P := Compute_Absolute_Coordinate (Root, Trg);
                     Src_Geo.Set_Y (Get_Y (P) - Formal_In_Height / 2.0);
                     Src_Geo.Set_X
                       (Get_X (P) - Formal_In_Width - X_Distance_Near_Pin);
		     
                     Log_Line (" IN:   Y => " & Get_Y (P)'Img);
		     
                  else
                     null;
                  end if;
		  
               elsif Is_Out_Vertex (Trg) and not Trg.Is_Visited then
		  
                  Trg := Top_Parent (Root, Trg);
                  Trg.Set_Visited (True);
                  Trg_Geo := Trg.Get_Geometry;
		  
                  P := Compute_Absolute_Coordinate (Root, Src);
                  Trg_Geo.Set_Y (Get_Y (P) - Formal_In_Height / 2.0);
                  Trg_Geo.Set_X
                    (Get_X (P) + X_Distance_Near_Pin);
		  
                  Log_Line (" Out:   Y => " & Get_Y (P)'Img);
		  
               else
                  null;
               end if;
            end if;
	    
         end if;
      end loop; 
      Log_Line ("Place_In_Out_Near End");
   end Place_In_Out_Near;
   
   ------------------
   -- Place_In_Out --
   ------------------
   
   procedure Place_In_Out (Root : access Cell_Record'Class) is
      
      Childs  : Cells_Lists.List;
      Src_Geo : access Cell_Geometry_Record;
      Trg_Geo : access Cell_Geometry_Record;
      Src     : access Cell_Record'Class;
      Trg     : access Cell_Record'Class;
      P       : Point_Record;
   begin
      Log_Line ("Place_In_Out Begin");
      Childs := Root.Get_Children_List;
      
      Unmark_Cells (Childs);
      
      for Cell of Childs loop
         if Cell.Is_Edge then
            Src := Cell.Get_Terminal (True);
            Trg := Cell.Get_Terminal (False);
	    
            Log_Line ("  Src => " & Get_Id (Src));
            Log_Line ("  Trg => " & Get_Id (Trg));
	    
            if Src /= null and Trg /= null then
	       
               if Is_In_Vertex (Src) and not Src.Is_Visited then
		  
                  Src := Top_Parent (Root, Src);
                  Src.Set_Visited (True);
                  Src_Geo := Src.Get_Geometry;
		  
                  P := Compute_Absolute_Coordinate (Root, Trg);
                  Src_Geo.Set_Y (Get_Y (P) - Formal_In_Height / 2.0);
		  
                  Log_Line (" IN:   Y => " & Get_Y (P)'Img);
		  
               elsif Is_Out_Vertex (Trg) and not Trg.Is_Visited then
		  
                  Trg := Top_Parent (Root, Trg);
                  Trg.Set_Visited (True);
                  Trg_Geo := Trg.Get_Geometry;
		  
                  P := Compute_Absolute_Coordinate (Root, Src);
                  Trg_Geo.Set_Y (Get_Y (P) - Formal_In_Height / 2.0);
		  
                  Log_Line (" Out:   Y => " & Get_Y (P)'Img);
		  
               else
                  null;
               end if;
            end if;
	    
         end if;
      end loop; 
      Log_Line ("Place_In_Out End");
   end Place_In_Out;
   
   ---------------------
   -- Place_Variables --
   ---------------------
   
   procedure Place_Variables (Root : access Cell_Record'Class) is
      
      Childs  : Cells_Lists.List;
      Src_Geo : access Cell_Geometry_Record;
      Trg_Geo : access Cell_Geometry_Record;
      Src     : access Cell_Record'Class;
      Trg     : access Cell_Record'Class;
      P       : Point_Record;
      Do_Src  : Boolean;
      Do_Trg  : Boolean;
   begin
      Log_Line ("Place_Variables Begin");
      Childs := Root.Get_Children_List;
      
      Unmark_Cells (Childs);
      
      for Cell of Childs loop
         if Cell.Is_Edge then
            Do_Src := False;
            Do_Trg := False;
	    
            Src := Cell.Get_Terminal (True);
            Trg := Cell.Get_Terminal (False);
	    
            if Src /= null and Trg /= null then
	       
               if Is_Local_Global_Vertex (Src) then 
                  if not Src.Is_Visited then
                     Do_Src := True;
                  end if;
		  
               elsif Is_Local_Global_Vertex (Trg) then 
                  if not Trg.Is_Visited then
                     Do_Trg := True;
                  end if;
               end if;
            end if;
	    
            if Do_Src then
	       
               Src := Top_Parent (Root, Src);
               Src.Set_Visited (True);
               Src_Geo := Src.Get_Geometry;
	       
               P := Compute_Absolute_Coordinate (Root, Trg);
               Src_Geo.Set_Y (Get_Y (P) - Formal_In_Height / 2.0);
	       
               Log_Line (" Var Src:   Y => " & Get_Y (P)'Img);
	       
            elsif Do_Trg then
	       
               Trg := Top_Parent (Root, Trg);
               Trg.Set_Visited (True);
               Trg_Geo := Trg.Get_Geometry;
	       
               P := Compute_Absolute_Coordinate (Root, Src);
               Trg_Geo.Set_Y (Get_Y (P) - Formal_In_Height / 2.0);
	       
               Log_Line (" Var Trg:   Y => " & Get_Y (P)'Img);
	       
            else
               null;
            end if;
         end if;
      end loop; 
      Log_Line ("Place_Variables End");
   end Place_Variables;
   
   --------------------------
   -- Place_Variables_Near --
   --------------------------
   
   procedure Place_Variables_Near (Root : access Cell_Record'Class) is
      
      Childs  : Cells_Lists.List;
      Src_Geo : access Cell_Geometry_Record;
      Trg_Geo : access Cell_Geometry_Record;
      Src     : access Cell_Record'Class;
      Trg     : access Cell_Record'Class;
      P       : Point_Record;
      Do_Src  : Boolean;
      Do_Trg  : Boolean;
   begin
      Log_Line ("Place_Variables_Near Begin");
      Childs := Root.Get_Children_List;
      
      Unmark_Cells (Childs);
      
      for Cell of Childs loop
         if Cell.Is_Edge then
            Do_Src := False;
            Do_Trg := False;
	    
            Src := Cell.Get_Terminal (True);
            Trg := Cell.Get_Terminal (False);
	    
            if Src /= null and Trg /= null then
	       
               if Is_Local_Global_Vertex (Src) then 
                  if not Src.Is_Visited then
                     Do_Src := True;
                  end if;
		  
               elsif Is_Local_Global_Vertex (Trg) then 
                  if not Trg.Is_Visited then
                     Do_Trg := True;
                  end if;
               end if;
            end if;
	    
            if Do_Src then
	       
               Src := Top_Parent (Root, Src);
               Src.Set_Visited (True);
               Src_Geo := Src.Get_Geometry;
	       
               P := Compute_Absolute_Coordinate (Root, Trg);
               Src_Geo.Set_Y (Get_Y (P) - Formal_In_Height / 2.0);
               Src_Geo.Set_X
               --  (Get_X (P) - Global_Vertex_Width - X_Distance_Near_Pin);
                 (Get_X (P) - Src.Get_Geometry.Get_Width - X_Distance_Near_Pin);
               
               Log_Line (" Var Src:   Y => " & Get_Y (P)'Img);
	       
            elsif Do_Trg then
	       
               Trg := Top_Parent (Root, Trg);
               Trg.Set_Visited (True);
               Trg_Geo := Trg.Get_Geometry;
	       
               P := Compute_Absolute_Coordinate (Root, Src);
               Trg_Geo.Set_Y (Get_Y (P) - Formal_In_Height / 2.0);
               Src_Geo.Set_X
                 (Get_X (P) + X_Distance_Near_Pin);
	       
               Log_Line (" Var Trg:   Y => " & Get_Y (P)'Img);
	       
            else
               null;
            end if;
         end if;
      end loop; 
      Log_Line ("Place_Variables_Near End");
   end Place_Variables_Near;
   
   ---------------------
   -- Route_Edges_Old --
   ---------------------
   
   procedure Route_Edges_Old (Root : access Cell_Record'Class)  is
      
      Edges   : Cells_Lists.List;
      Geo     : access Cell_Geometry_Record;
      Psrc    : Point_Record;
      Ptrg    : Point_Record;
      Xsrc    : Float;
      Ysrc    : Float;
      Xtrg    : Float;
      Ytrg    : Float;
      Yinter  : Float;
      Yinter2 : Float;
      L       : Point_Lists.List;
      P       : Point_Record;
      Src     : access Cell_Record'Class;
      Trg     : access Cell_Record'Class;
   begin
      Log_Line ("Route_Edges begin");
      -- 1. If Ysrc = Ytrg and no intersection => Ok
      -- 2. If intersection 
      --   2.1 Try with 2 segments and a derivation point:
      --       Derivation
      --          +
      --          |
      --          |
      --          +-----
      
      -- 3. Get Around intersection with 4 segments and a derivation
      --       Derivation
      --          +
      --          |
      --          |
      --          +-----+
      --                |
      --                *--
      
      Edges := Root.Get_Children_List;
      for Edge of Edges loop
         if Edge.Is_Edge then 
            Src := Edge.Get_Terminal (True);
            Trg := Edge.Get_Terminal (False);
	 
            if not Is_Local_Global_Vertex (Src)
              and not Is_In_Vertex (Src)
              and not Is_Out_Vertex (Src)
              and not Is_Local_Global_Vertex (Trg)
              and not Is_In_Vertex (Trg)
              and not Is_Out_Vertex (Trg)
            then
	   
               Log_Line ("======================>> Edge found ");	 
               Geo := Edge.Get_Geometry;
	    
               Psrc := Geo.Get_Source_Point;
               Ptrg := Geo.Get_Target_Point;
	    
               Xsrc := Get_X (Psrc);
               Ysrc := Get_Y (Psrc);
               Xtrg := Get_X (Ptrg);
               Ytrg := Get_Y (Ptrg);
	    
               Yinter := Edge_Intersection (Root, Psrc, Ptrg);
	    
               if Yinter = 0.0 then
                  Log_Line ("  Inter is false ");	 
                  -- No Intersection
                  null;
               else
                  Log_Line (" Inter is true ");	 
                  -- Try case 2 
                  -- We know that the first segment which go up or down don't
                  -- instersect, so verify intersection, only on the second segment 
                  -- Ytrg -> (Y + Delta). If interset then case 3
	       
                  Yinter2 := Edge_Intersection
                    (Root, 
                     Point_Record'(Xsrc, Yinter),
                     Ptrg);
	       
                  if Yinter2 = 0.0 then
		  
                     Log_Line (" Inter2 is false ");	 
                     -- From Src to middle of channel
                     P := Point_Record'(Xsrc + X_Distance_Min_Block / 2.0, Ysrc);
                     L.Append (P);
		  
                     -- strait on middle of channel
                     P := Point_Record'(Xsrc + X_Distance_Min_Block / 2.0, Ytrg);
                     L.Append (P);
		  
                     Geo.Set_Points (L);
		  
		  
                  else
                     Log_Line (" Inter2 is true ");	 
                     null;
                  end if;
                  null;
               end if;
            end if;
         end if;
      end loop;
      
      null;
   end Route_Edges_Old;
   
   --------------------------------
   -- Has_Collsion_Source_Target --
   --------------------------------
   
   function Has_Collsion_Source_Target
     (Src       : access Cell_Record'Class;
      Trg       : access Cell_Record'Class;
      Src_Point : Point_Record;
      Trg_Point : Point_Record;
      Cells     : Cells_Lists.List) return Rectangle_Record is
      
      Geo      : access Cell_Geometry_Record'Class;
      Rect     : Rectangle_Record;
      Rect_Geo : Rectangle_Record;
   begin
      Rect := No_Rectangle_Record;
      
      for C of Cells loop
         if Is_Vertex (C) then
            Geo      := C.Get_Geometry;
            Rect_Geo := Geo.Geometry_Rectangle;
            
            if C /= Src 
              and then C /= Trg
              and then Get_Layer (C) > Get_Layer (Src) + 1 
            then
               if Intersect_Line 
                 (Rect_Geo, Src_Point, Trg_Point) /= No_Point_Record 
               then
                  if Rect = No_Rectangle_Record then
                     Rect := Rect_Geo;
                  else
                     Add (Rect, Rect_Geo);
                  end if;
               end if;
            end if;
         end if;
      end loop;
      
      return Rect;
   end Has_Collsion_Source_Target;
   
   ------------------------------
   -- Compute_Orthogonal_Edges --
   ------------------------------
   
   procedure Compute_Orthogonal_Edges (Root : access Cell_Record'Class) is
      
      use Point_Lists;
      
      Src        : access Cell_Record'Class;
      Trg        : access Cell_Record'Class;
      Cells      : Cells_Lists.List;
      Src_Parent : access Cell_Record'Class;
      Trg_Parent : access Cell_Record'Class;
      Src_Point  : Point_Record;
      Trg_Point  : Point_Record;
      Geo        : access Cell_Geometry_Record'Class;
      Points     : Point_Lists.List;
      Ydecal     : Coordinate;
   begin
      Log_Line ("Compute_Orthogonal_Edges Begin");
      -- The dependence list is placed in the root cell of the flow
      
      Cells := Root.Get_Children_List;
      
      -- Edges go always from left to right. 
      
      Ydecal := Delta_X_Cnx;
      
      for C of Cells loop
         if Is_Edge (C) then
            Src := C.Get_Terminal (True);
            Trg := C.Get_Terminal (False);
	    
            Src_Parent := Top_Parent (Root, Src);
            Trg_Parent := Top_Parent (Root, Trg);
	    
            if Is_Local_Global_Vertex (Src)
              or Is_In_Vertex (Src)
              or Is_Out_Vertex (Src)
              or Is_Local_Global_Vertex (Trg)
              or Is_In_Vertex (Trg)
              or Is_Out_Vertex (Trg)
            then
               Log_Line ("Compute_Orthogonal_Edges: In or Out .. ");
	       
            else
	       
               Geo       := C.Get_Geometry;
               Src_Point := Geo.Get_Source_Point;
               Src_Point := Compute_Absolute_Coordinate (Root, Src);
               Trg_Point := Geo.Get_Target_Point;
               Trg_Point := Compute_Absolute_Coordinate (Root, Trg);
	       
               if Get_Y (Src_Point) = Get_Y (Trg_Point)
                 and Get_Layer (Trg_Parent) = Get_Layer (Src_Parent) + 1
               then
                  Log_Line
                    ("Compute_Orthogonal_Edges: Strait to next");
               else	       
                  Points := Point_Lists.Empty_List;
		  
                  Compute_Orthogonal_Path
                    (Src        => Src_Parent,
                     Trg        => Trg_Parent,
                     Src_Point  => Src_Point,
                     Trg_Point  => Trg_Point,
                     Cells      => Cells,
                     Points     => Points,
                     Ydecal     => Ydecal);
		  
                  Geo.Set_Points (Points);
               end if;
            end if;
         end if;
      end loop;
      
      Log_Line ("Compute_Orthogonal_Edges End");
   end Compute_Orthogonal_Edges;
   
   -----------------------------
   -- Compute_Orthogonal_Path --
   -----------------------------
   
   procedure Compute_Orthogonal_Path
     (Src       : access Cell_Record'Class;
      Trg       : access Cell_Record'Class;
      Src_Point : Point_Record;
      Trg_Point : Point_Record;
      Cells     : Cells_Lists.List;
      Points    : in out Point_Lists.List;
      Ydecal    : in out Coordinate) is
  
      Xstart : Coordinate;
      Ystart : Coordinate;
      Xend   : Coordinate;
      Yend   : Coordinate;
      Col    : Rectangle_Record;
      Xcol   : Coordinate;
      Ycol   : Coordinate;
      Wcol   : Coordinate;
      Hcol   : Coordinate;
      Yp     : Coordinate;
      Dy     : Coordinate;
      P1     : Point_Record;
      P2     : Point_Record;
      P3     : Point_Record;
      P4     : Point_Record;
   begin
      Log_Line ("Compute_Orthogonal_Path Begin");
      Xstart := Get_X (Src_Point);
      Ystart := Get_Y (Src_Point);
      
      Xend := Get_X (Trg_Point);
      Yend := Get_Y (Trg_Point);
      
      -- 1. If Y are the same Try Xstrat,Ystart and Xend,Ystart
      --   Case No collision, Then Draw
      --   Case collision Ortho Algo
      
      -- 2. Else Try Xstart,Yend and Xend, Yend
      --   Case no collision then Draw
      --   Case collision Otho Algo
      
      -- 3. Else Try Xstart,Ystart and Xend, Ystart
      --   Case no collision then Draw
      --   Case Collision Ortho Algo
      
      
      -- 1. Try go straight :  Xstrat,Ystart and Xend,Yend
      
      Dy := Yend - Ystart;
      
      --  if Ystart = Yend then
      
      --  if abs (Dy) < 0.01 then
      --  	 -- Source and End Points are sufficients to draw the link
      --  	 return;
      --  end if;
      
      -- 3. Else Try Xstart,Ystart, and Xend, Ystart
      
      Col := Has_Collsion_Source_Target
        (Src        => Src,
         Trg        => Trg,
         Src_Point  => Point_Record'(Xstart, Ystart),
         Trg_Point  => Point_Record'(Xend, Ystart),
         Cells      => Cells);
      
      if Col = No_Rectangle_Record then
         Log_Line (" No_Collison for (Xstart, Ystart) and (Xend, Ystart) ");
	 
         -- 2nd version : break the line at the middle of the path
	 
         if Get_Layer (Trg) = Get_Layer (Src) + 1 then
            Log_Line ("  Layer +1 ");
            P1 := Point_Record'((Xend - (Xend - Xstart) / 2.0), Ystart);
            --P2 := Point_Record'((Xend - (Xend - Xstart) / 2.0) + 4.0, Yend);
            P2 := Point_Record'(Xend - (Xend - Xstart) / 2.0, Yend);
         else
            Log_Line ("  no Layer +1 ");
            P1 := Point_Record'(Xstart + Delta_X_Cnx, Ystart);
            --P2 := Point_Record'(Xend - Delta_X_Cnx, Yend);
            P2 := Point_Record'(Xstart + Delta_X_Cnx, Yend);
         end if;
	 
         Point_Lists.Append (Points, P1);
         Point_Lists.Append (Points, P2);
	 
         return;
      end if;
      
      
      -- 2. Else Try Xstart,Yend and Xend, Yend
      
      Col := Has_Collsion_Source_Target
        (Src        => Src,
         Trg        => Trg,
         Src_Point  => Point_Record'(Xstart, Yend),
         Trg_Point  => Point_Record'(Xend,   Yend),
         Cells      => Cells);
      
      if Col = No_Rectangle_Record then
         Log_Line (" No_Collison for (Xstart, Yend) and (Xend, Yend) ");
	 
         if Get_Layer (Trg) = Get_Layer (Src) + 1 then
            Log_Line ("  Layer +1 ");
            P1 := Point_Record'((Xstart + (Xend - Xstart) / 2.0), Ystart);
            --P2 := Point_Record'((Xstart + (Xend - Xstart) / 2.0) + 4.0, Yend);
            P2 := Point_Record'(Xstart + (Xend - Xstart) / 2.0, Yend);
         else
            Log_Line ("  no xLayer +1 ");
            P1 := Point_Record'(Xstart + Delta_X_Cnx, Ystart);
            --P2 := Point_Record'(Xstart + Delta_X_Cnx, Yend);
            P2 := Point_Record'(Xstart + Delta_X_Cnx, Yend);
         end if;
	 
         Point_Lists.Append (Points, P1);
         Point_Lists.Append (Points, P2);
	 
         return;
      end if;
      
      Log_Line (" default case ");
      
      -- Default case is the Orthogonal path. Go around the rectangle 
      -- collision.
      
      Col := Has_Collsion_Source_Target
        (Src        => Src,
         Trg        => Trg,
         Src_Point  => Point_Record'(Xstart, Ystart),
         Trg_Point  => Point_Record'(Xend, Yend),
         Cells      => Cells);
	 
      -- Chhoose to go up or down first
      
      Xcol := Get_X (Col);
      Ycol := Get_Y (Col);
      Wcol := Get_Width (Col);
      Hcol := Get_Height (Col);
      
      -- Go up first if the Target is over the collision rectangle
      
      if Yend > Ycol then
         Yp := Ycol - Delta_Y_Cnx;
	 
         -- Go down first if the Target is under the collision rectangle
      else
         Yp := Ycol + Hcol + Delta_Y_Cnx;
      end if;
      
      P1 := Point_Record'(Xstart + Delta_X_Cnx, Ystart);
      P2 := Point_Record'(Xstart + Delta_X_Cnx, Yp);
      P3 := Point_Record'(Xend   - Delta_X_Cnx, Yp);
      P4 := Point_Record'(Xend   - Delta_X_Cnx, Yend);
      
      Point_Lists.Append (Points, P1);
      Point_Lists.Append (Points, P2);
      Point_Lists.Append (Points, P3);
      Point_Lists.Append (Points, P4);
      
      Log_Line ("Compute_Orthogonal_Path End");
   end Compute_Orthogonal_Path;

   --------------------
   -- Make_Placement --
   --------------------
   
   procedure Make_Placement (Root : access Cell_Record'Class) is
      
      Cells     : Cells_Lists.List;
      Terminals : Cells_Lists.List;
      Geo       : access Cell_Geometry_Record;
      X         : Float;
      Y         : Float;
   begin
      Cells := Root.Get_Children_List;
      
      -- Build the terminals list
      
      Terminals := Build_Terminal_Vertices (Cells);      
      
      Log_Line ("====  Begin Terlinals  =====");
      for T of Terminals loop
         Log_Line (" T => " & Get_Id (T));
      end loop;
      Log_Line ("====  End Terlinals  =====");
	
      -- For each terminal make placement
      
      Unmark_Cells (Cells);
      
      X := 10.0;
      Y := 20.0;
      
      for T of Terminals loop
         Geo := T.Get_Geometry;
         -- Geo.Set_Y (Y);
         Place_Cell (Cell_Ptr (T), X, Y);
         -- Place_Cell_On_X (Cell_Ptr (T), X);
         X := 0.0;
         Y := Y + Get_Max_Height (T);
      end loop;
      
      Place_In_Out_Near (Root);
      --  Place_In_Out (Root);
      --  Place_Variables (Root);
      
      Route_Edges (Root);
      
      Log_Line ("Make_Placement End");
   end Make_Placement;
   
   --------------------------
   -- Make_Layer_Placement --
   --------------------------
   
   procedure Make_Layer_Placement (Root : access Cell_Record'Class) is
      Cells : Cells_Lists.List;
      Geo   : access Cell_Geometry_Record;
      Layer : Integer;
      X     : Float;
      Y     : Float;
      Terminals : Cells_Lists.List;
   begin
      Build_Vertex_Dependencies (Root);
        
      Remove_Cycle (Root);
      Compute_Layer_Forward (Root);

      Cells := Root.Get_Children_List;
      
      for Cell of Cells loop
         Layer := Cell.Get_Layer;
         X := Float (Layer - 1) * 12.0  - 5.0;
         Geo := Cell.Get_Geometry;
         Geo.Set_X (X);
      end loop;

      Y := 3.0;
      Unmark_Cells (Cells);
      Terminals := Build_Terminal_Vertices (Cells);      
      Unmark_Cells (Cells);
      for T of Terminals loop
         if not Is_Global_Vertex (T) 
           and not Is_Local_Vertex (T)
           and not Is_In_Vertex (T)
           and not Is_Out_Vertex (T)
         then
            Geo := T.Get_Geometry;
            Place_Cell_On_Y (Cell_Ptr (T), Y);
            Y := Y + Get_Max_Height (T) + 4.0;
         end if;
      end loop;

      Cells := Root.Get_Children_List;
      Unmark_Cells (Cells);
      for Cell of Cells loop
         if Vertex_Value_Ptr (Cell.Get_Value).Get_Vertex_Kind = Enclose_Vertex then
            for C of Cell.Get_Children_List loop
               Geo := C.Get_Geometry;
               Geo.Set_X (Geo.Get_X + Cell.Get_Geometry.Get_X);
               Geo.Set_Y (Geo.Get_Y + Cell.Get_Geometry.Get_Y);                
            end loop;
         end if;
      end loop;

      --        Place_In_Out_Near (Root);
      --        Place_Variables_Near (Root);

      -- Compute_Orthogonal_Edges (Root);
      
      Route_Edges (Root);
   end Make_Layer_Placement;
   
   ----------------
   -- Workaround --
   ----------------
   
   procedure Workaround 
     (Root : access Cell_Record'Class;
      Edge : access Cell_Record'Class) is
      
      Childs     : Cells_Lists.List;
      Src        : access Cell_Record'Class;
      Trg        : access Cell_Record'Class;
      Src_Parent : access Cell_Record'Class;
      Trg_Parent : access Cell_Record'Class;
      
      Geo        : access Cell_Geometry_Record'Class;
      Src_Point  : Point_Record;
      Trg_Point  : Point_Record;
      
      Xcur       : Coordinate;
      Ycur       : Coordinate;
      Xtrg       : Coordinate;
      Ytrg       : Coordinate;
      Xcol       : Coordinate;
      Ycol       : Coordinate;
      
      Points     : Point_Lists.List;
      Rect_Col   : Rectangle_Record;
   begin
      Childs := Root.Get_Children_List;
      
      Src := Get_Source (Edge);
      Trg := Get_Target (Edge);
      
      Src_Parent := Top_Parent (Root, Src);
      Trg_Parent := Top_Parent (Root, Trg);
      
      Src_Point := Compute_Absolute_Coordinate (Root, Src);
      Trg_Point := Compute_Absolute_Coordinate (Root, Trg);
      
      Xcur := Get_X (Src_Point);
      Ycur := Get_Y (Src_Point);
      
      Xtrg := Get_X (Trg_Point);
      Ytrg := Get_Y (Trg_Point);
      
      Points := Point_Lists.Empty_List;
      
      -- Apply the workaround algo.
      
      loop
         exit when Xcur = Xtrg and Ycur = Ytrg;
	 
         -- Collision on X ? Workaround on Y
	 
         Rect_Col := Has_Collision
           (Src       => Src_Parent,
            Trg       => Trg_Parent,
            Src_Point => Point_Record'(Xcur, Ycur),
            Trg_Point => Point_Record'(Xtrg, Ycur),
            Cells     => Childs);
	 
         if Rect_Col /= No_Rectangle_Record then
	    
            Xcol := Get_X (Rect_Col);
            Points.Append (Point_Record'(Xcol, Ycur));
	    
            Xcur := Xcol;
         else
            Xcur := Xtrg;
         end if;
	 
         -- Collision on Y ? Workaround on X
	 
         Rect_Col := Has_Collision
           (Src       => Src_Parent,
            Trg       => Trg_Parent,
            Src_Point => Point_Record'(Xcur, Ycur),
            Trg_Point => Point_Record'(Xcur, Ytrg),
            Cells     => Childs);
	 
         if Rect_Col /= No_Rectangle_Record then
	    
            Ycol := Get_Y (Rect_Col);
            Points.Append (Point_Record'(Xcur, Ycol));
	    
            Ycur := Ycol;
         else
            Ycur := Ytrg;
         end if;
      end loop;
      
      Geo := Edge.Get_Geometry;
      Geo.Set_Source_Point (Src_Point);
      Geo.Set_Target_Point (Trg_Point);
      
      -- If the link is direct add a control point o,n the middle of the edge
      
      if Points.Is_Empty then
         Points.Append (Point_Record'(Xcur + (Xtrg - Xcur) / 2.0, Ytrg));
      end if;
      
      Geo.Set_Points (Points);
   end Workaround;
   
   ------------------
   -- Edge_Routing --
   ------------------
   
   procedure Edge_Routing
     (Root : access Cell_Record'Class;
      Edge : access Cell_Record'Class) is
      
      Childs     : Cells_Lists.List;
      Src        : access Cell_Record'Class;
      Trg        : access Cell_Record'Class;
      Src_Parent : access Cell_Record'Class;
      Trg_Parent : access Cell_Record'Class;
      
      Geo        : access Cell_Geometry_Record'Class;
      Src_Point  : Point_Record;
      Trg_Point  : Point_Record;
      
      Xcur       : Coordinate;
      Ycur       : Coordinate;
      Xtrg       : Coordinate;
      Ytrg       : Coordinate;
      Ctrl_X     : Coordinate;
      
      Points     : Point_Lists.List;
      Rect_Col   : Rectangle_Record;
   begin
      Childs := Root.Get_Children_List;
      
      Src := Get_Source (Edge);
      Trg := Get_Target (Edge);
      
      Src_Parent := Top_Parent (Root, Src);
      Trg_Parent := Top_Parent (Root, Trg);
      
      Src_Point := Compute_Absolute_Coordinate (Root, Src);
      Trg_Point := Compute_Absolute_Coordinate (Root, Trg);
      
      Xcur := Get_X (Src_Point);
      Ycur := Get_Y (Src_Point);
      
      Xtrg := Get_X (Trg_Point);
      Ytrg := Get_Y (Trg_Point);
      
      Points := Point_Lists.Empty_List;
      
      -- In case of Ysrc /= Ytarget, try if we can go with two segments.
      
      if Ycur /= Ytrg then
	 
         -- Verify collision for first segment :
         -- (Xstart, Ystart), (Xstart + (Xtarget - Xstart)/ 2, Ystart)
	 
         Ctrl_X := Xcur + (Xtrg - Xcur) / 2.0;
	 
         Rect_Col := Has_Collision
           (Src       => Src_Parent,
            Trg       => Trg_Parent,
            Src_Point => Point_Record'(Xcur, Ycur),
            Trg_Point => Point_Record'(Ctrl_X, Ycur),
            Cells     => Childs);
	 
         if Rect_Col = No_Rectangle_Record then
	    
            -- Verify collision for second segment :
            -- (Xstart, Ystart), (Xstart + (Xtarget - Xstart)/ 2, Ystart)
	    
            Rect_Col := Has_Collision
              (Src       => Src_Parent,
               Trg       => Trg_Parent,
               Src_Point => Point_Record'(Ctrl_X, Ycur),
               Trg_Point => Point_Record'(Ctrl_X, Ytrg),
               Cells     => Childs);
	    
            if Rect_Col = No_Rectangle_Record then
	       
               -- Verify collision for third segment :
               -- (Xstart, Ystart), (Xstart + (Xtarget - Xstart)/ 2, Ystart)
	 
               Rect_Col := Has_Collision
                 (Src       => Src_Parent,
                  Trg       => Trg_Parent,
                  Src_Point => Point_Record'(Ctrl_X, Ytrg),
                  Trg_Point => Point_Record'(Xtrg, Ycur),
                  Cells     => Childs);
	       
               if Rect_Col = No_Rectangle_Record then
		  
                  -- Draw the three segmants with a control point on the middle
                  -- of the edge
		  
                  Geo := Edge.Get_Geometry;
                  Geo := Edge.Get_Geometry;
                  Geo.Set_Source_Point (Src_Point);
                  Geo.Set_Target_Point (Trg_Point);
		  
                  Points.Append (Point_Record'(Ctrl_X, Ycur));
                  Points.Append (Point_Record'(Ctrl_X, Ytrg));
                  Geo.Set_Points (Points);
		  
                  return;
               end if;
            end if;
         end if;
      end if;	 
      
      -- Try to got down (up) after with two segments
      -- Verify collision for first segment :
      -- (Xstart, Ystart), (Xtrg + Ystart)
      
      Rect_Col := Has_Collision
        (Src       => Src_Parent,
         Trg       => Trg_Parent,
         Src_Point => Point_Record'(Xcur, Ycur),
         Trg_Point => Point_Record'(Xtrg, Ycur),
         Cells     => Childs);
      
      if Rect_Col = No_Rectangle_Record then
	 
         -- Verify collision for second segment :
         -- (Xstart, Ystart), (Xstart + (Xtarget - Xstart)/ 2, Ystart)
	 
         Rect_Col := Has_Collision
           (Src       => Src_Parent,
            Trg       => Trg_Parent,
            Src_Point => Point_Record'(Xtrg, Ycur),
            Trg_Point => Point_Record'(Xtrg, Ytrg),
            Cells     => Childs);
	 
         if Rect_Col = No_Rectangle_Record then
	    
            -- Draw the three segmants with a control point on the middle
            -- of the edge
	    
            Geo := Edge.Get_Geometry;
            Geo := Edge.Get_Geometry;
            Geo.Set_Source_Point (Src_Point);
            Geo.Set_Target_Point (Trg_Point);
	    
            Points.Append (Point_Record'(Xtrg, Ycur));
            Geo.Set_Points (Points);
	    
            return;
         end if;
      end if;
	    
      -- Try to got down (up) before with two segments
	 
      -- Verify collision for first segment :
      -- (Xstart, Ystart), (Xtrg + Ystart)
      
      Rect_Col := Has_Collision
        (Src       => Src_Parent,
         Trg       => Trg_Parent,
         Src_Point => Point_Record'(Xcur, Ycur),
         Trg_Point => Point_Record'(Xcur, Ytrg),
         Cells     => Childs);
      
      if Rect_Col = No_Rectangle_Record then
	 
         -- Verify collision for second segment :
         -- (Xstart, Ystart), (Xstart + (Xtarget - Xstart)/ 2, Ystart)
	 
         Rect_Col := Has_Collision
           (Src       => Src_Parent,
            Trg       => Trg_Parent,
            Src_Point => Point_Record'(Xcur, Ytrg),
            Trg_Point => Point_Record'(Xtrg, Ytrg),
            Cells     => Childs);
	 
         if Rect_Col = No_Rectangle_Record then
	    
            -- Draw the three segmants with a control point on the middle
            -- of the edge
	    
            Geo := Edge.Get_Geometry;
            Geo := Edge.Get_Geometry;
            Geo.Set_Source_Point (Src_Point);
            Geo.Set_Target_Point (Trg_Point);
	    
            Points.Append (Point_Record'(Xcur, Ytrg));
            Geo.Set_Points (Points);
	    
            return;
         end if;
      end if;
      
      -- Default apply the workaround algo.
      
      Workaround (Root, Edge);
      
   end Edge_Routing;
   
   -------------------
   -- Has_Collision --
   -------------------
   
   function Has_Collision
     (Src       : access Cell_Record'Class;
      Trg       : access Cell_Record'Class;
      Src_Point : Point_Record;
      Trg_Point : Point_Record;
      Cells     : Cells_Lists.List) return Rectangle_Record is
      
      Geo      : access Cell_Geometry_Record'Class;
      Rect     : Rectangle_Record;
      Rect_Geo : Rectangle_Record;
   begin
      Rect := No_Rectangle_Record;
      
      for C of Cells loop
         if Is_Vertex (C) then
            Geo      := C.Get_Geometry;
            Rect_Geo := Geo.Geometry_Rectangle;
            
            if C /= Src 
              and then C /= Trg
            then
               if Intersect_Line 
                 (Rect_Geo, Src_Point, Trg_Point) /= No_Point_Record 
               then
                  if Rect = No_Rectangle_Record then
                     Rect := Rect_Geo;
                  else
                     Add (Rect, Rect_Geo);
                  end if;
               end if;
            end if;
         end if;
      end loop;
      
      return Rect;
   end Has_Collision;
   
   -----------------
   -- Route_Edges --
   -----------------
   
   procedure Route_Edges (Root : access Cell_Record'Class)  is
      
      Edges : Cells_Lists.List;
      Src   : access Cell_Record'Class;
      Trg   : access Cell_Record'Class;
   begin
      Log_Line ("Route_Edges begin");
      
      Edges := Root.Get_Children_List;
      
      for Edge of Edges loop
	 
         if Edge.Is_Edge then 
            Src := Edge.Get_Terminal (True);
            Trg := Edge.Get_Terminal (False);
	    
            if not Is_Local_Global_Vertex (Src)
              and not Is_In_Vertex (Src)
              and not Is_Out_Vertex (Src)
              and not Is_Local_Global_Vertex (Trg)
              and not Is_In_Vertex (Trg)
              and not Is_Out_Vertex (Trg)
            then
               Edge_Routing (Root, Edge);
            end if;
         end if;
      end loop;
   end Route_Edges;
   
end Reflex.Fbd_Placements;
