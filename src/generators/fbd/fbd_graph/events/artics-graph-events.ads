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

pragma Style_Checks (Off);

with Artics.Graph; use Artics.Graph;
with Artics.Generic_Events_Objects;
with Artics.Generic_Listeners_Interfaces;
with Artics.Generic_Events_Sources; 

package Artics.Graph.Events is
   
   type Event_Type_Enum is
     (No_Event,
      Event_Done,
      Event_Add_Cells,
      Event_Cells_Added,
      Event_Align_Cells,
      Event_Connect_Cell,
      Event_Connect,
      Event_Cell_Connected,
      Event_Flip_Edge,
      Event_Fold_Cells,
      Event_Cells_Folded,
      Event_Group_Cells,
      Event_Ungroup_Cells,
      Event_Remove_Cells_From_Parent,
      Event_Move_Cells,
      Event_Cells_Moved,
      Event_Order_Cells,
      Event_Cells_Ordered,
      Event_Remove_Cells,
      Event_Cells_Removed,
      Event_Repaint,
      Event_Resize_Cells,
      Event_Cells_Resized,
      Event_Split_Edge,
      Event_Toggle_Cells,
      Event_Cells_Toggles,
      Event_Update_Cell_Size,
      Event_Label_Changed,
      Event_Add_Overlay,
      Event_Remove_Overlay,
      Event_Before_Paint,
      Event_Paint,
      Event_After_Paint,
      Event_Start_Editing,
      Event_Undo,
      Event_Redo,
      Event_Up,
      Event_Down,
      Event_Scale,
      Event_Translate,
      Event_Scale_And_Translate,
      
      Event_Change,
      -- Holds the name for the change event. First and only argument in the
      -- argument array is the list of mxAtomicGraphChanges that have been
      -- executed on the model.
      
      Event_Execute,
      -- Holds the name for the execute event. First and only argument in the
      -- argument array is the mxAtomicGraphChange that has been executed on
      -- the model. This event fires before the change event.
      
      Event_Before_Undo,
      -- Holds the name for the beforeUndo event. First and only argument in the
      -- argument array is the current edit that is currently in progress in
      -- the model. This event fires before notify is called on the currentEdit
      -- in the model.
      
      Event_Notify,
      -- Holds the name for the norify event. First and only argument in the
      -- argument array is the list of mxAtomicGraphChanges that have been
      -- executed on the model. This event fires after the change event.
      
      Event_Begin_Update,
      -- Holds the name for the beginUpdate event. This event has no arguments
      -- and fires after the updateLevel has been changed in model.
      
      Event_End_Update,
      -- Holds the name for the endUpdate event. This event has no arguments
      -- and fires after the updateLevel has been changed in the model. First
      -- argument is the currentEdit.
      
      Event_Insert,
      Event_Add,
      Event_Clear,
      Event_Fired,
      Event_Select,
      
      Event_Mark,
      -- Holds the name for the mark event, which fires after a cell has been
      -- marked. First and only argument in the array is the cell state that has
      -- been marked or null, if no state has been marked.
      -- 
      -- To add a mark listener to the cell marker:
      -- 
      -- <code>
      -- addListener(
      --   mxEvent.MARK, new mxEventListener()
      --   {
      --     public void invoke(Object source, Object[] args)
      --     {
      --       cellMarked((mxCellMarker) source, (mxCellState) args[0]);
      --     }
      --   });
      -- </code>
      
      Event_Root,
      Event_Layout_Cells,
      Event_Start,
      Event_Continue,
      Event_Stop
     );
   
   
   package Graph_Events is new Artics.Generic_Events_Objects
     (Event_Type_Enum);
   
   package Graph_Listeners is new Artics.Generic_Listeners_Interfaces
     (Event_Type_Enum, Graph_Events);
   
   package Graph_Events_Sources is new Artics.Generic_Events_Sources
     (Event_Type_Enum,
      Graph_Events,
      Graph_Listeners);
   
   use Graph_Events;
   use Graph_Listeners;
   use Graph_Events_Sources;
   
   type Graph_Event_Record is new Event_Object_Record with private;
   
   No_Graph_Event_Record : constant Graph_Event_Record;
   
   procedure Initialize;
   
   
private
   
   type Graph_Event_Record is new Event_Object_Record with null record;
  
   No_Graph_Event_Record : constant Graph_Event_Record := Graph_Event_Record'
     (No_Event_Object_Record with null record);
   
end Artics.Graph.Events;
