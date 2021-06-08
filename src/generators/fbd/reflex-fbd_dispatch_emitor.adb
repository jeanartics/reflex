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

with Artics.Graph.Cells; use Artics.Graph.Cells;
with Artics.Buffers;     use Artics.Buffers;

with Atree; use Atree;
with Sinfo; use Sinfo;

with Reflex.Vertex_Value; use Reflex.Vertex_Value;
with Reflex.Edge_Value;   use Reflex.Edge_Value;
with Reflex.Fbd_Emitor;   use Reflex.Fbd_Emitor;
with Reflex.Fbd_Util;     use Reflex.Fbd_Util;

package body Reflex.Fbd_Dispatch_Emitor is

   procedure Emitor_Dispatch
     (This : access Fbd_Builder_Record) is
      
      Curr_Node     : Node_Id;
      Curr_Cell     : access Cell_Record;
      Src_Val       : Vertex_Value_Ptr;
      Trg_Val       : Vertex_Value_Ptr;
      Emitor        : Fbd_Emitor_Ptr := This.Get_Fbd_Emitor;
      Handled_Cells : Cells_Lists.List;
      --  Handled_cells contain all cell which are already emit.
      
      use Cells_Lists;
   begin
      
      Emit_Header (Emitor);
      
      --  Emit all vertices

      for Cell of This.Get_Graph.Get_Default_Parent.Get_Children_List loop
         if Cell.Is_Vertex then
            Curr_Cell := Vertex_Value_Ptr (Cell.Get_Value).Get_Cell_To_Link;
            
            if not Handled_Cells.Contains (Curr_Cell) then
               Curr_Node := Vertex_Value_Ptr (Cell.Get_Value).Get_Node;
               
               if Nkind (Curr_Node) /= N_Identifier
               --                 and Nkind (Curr_Node) then -- not in N_Numeric_Or_String_Literal 
               then
                  Dump_Vertex (This, Curr_Cell);
                  Emit_Ffb_Block (Emitor, Curr_Cell);
               end if;
               
               Handled_Cells.Append (Curr_Cell);
            end if;
         end if;
      end loop;

      Handled_Cells.Clear;
         
      --  Emit all edges
         
      for Cell of This.Get_Graph.Get_Default_Parent.Get_Children_List loop
         if Cell.Is_Edge then
            Curr_Cell := Cell;
            
            if not Handled_Cells.Contains (Curr_Cell) then
               Src_Val := Vertex_Value_Ptr (Cell.Get_Source.Get_Value);
               Trg_Val := Vertex_Value_Ptr (Cell.Get_Target.Get_Value);
               
               if Nkind (Src_Val.Get_Node) /= N_Identifier
--                   and Nkind (Src_Val.Get_Node) not in N_Numeric_Or_String_Literal
                 and Nkind (Trg_Val.Get_Node) /= N_Identifier 
--                 and Nkind (Trg_Val.Get_Node) not in N_Numeric_Or_String_Literal 
               then
                  Emit_Link_Fb (Emitor, Curr_Cell);
               end if;
               
               Handled_Cells.Append (Curr_Cell);
            end if;
         end if;
      end loop;
      Emit_Tailer (Emitor);
   end Emitor_Dispatch;
   
end Reflex.Fbd_Dispatch_Emitor;

