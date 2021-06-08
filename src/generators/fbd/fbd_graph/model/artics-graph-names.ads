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

with Artics.Types; use Artics.Types;
with Artics.Namet; use Artics.Namet;

package Artics.Graph.Names is
   
   -- Names used through the graph editor
   
   ------------
   -- Events --
   ------------
   
   Event_Done_Name                     : Name_Id; -- "done";
   Event_Add_Cells_Name                : Name_Id; -- "addCells";
   Event_Cells_Added_Name              : Name_Id; -- "cellsAdded";
   Event_Align_Cells_Name              : Name_Id; -- "alignCells";
   Event_Connect_Cell_Name             : Name_Id; -- "connectCell";
   Event_Connect_Name                  : Name_Id; -- "connect";
   Event_Cell_Connected_Name           : Name_Id; -- "cellConnected";
   Event_Flip_Edge_Name                : Name_Id; -- "flipEdge";
   Event_Fold_Cells_Name               : Name_Id; -- "foldCells";
   Event_Cells_Folded_Name             : Name_Id; -- "cellsFolded";
   Event_Group_Cells_Name              : Name_Id; -- "groupCells";
   Event_Ungroup_Cells_Name            : Name_Id; -- "ungroupCells";
   Event_Remove_Cells_From_Parent_Name : Name_Id; -- "removeCellsFromParent";
   Event_Move_Cells_Name               : Name_Id; -- "moveCells";
   Event_Cells_Moved_Name              : Name_Id; -- "cellsMoved";
   Event_Order_Cells_Name              : Name_Id; -- "orderCells";
   Event_Cells_Ordered_Name            : Name_Id; -- "cellsOrdered";
   Event_Remove_Cells_Name             : Name_Id; -- "removeCells";
   Event_Cells_Removed_Name            : Name_Id; -- "cellsRemoved";
   Event_Repaint_Name                  : Name_Id; -- "repaint";
   Event_Resize_Cells_Name             : Name_Id; -- "resizeCells";
   Event_Cells_Resized_Name            : Name_Id; -- "cellsResized";
   Event_Split_Edge_Name               : Name_Id; -- "splitEdge";
   Event_Toggle_Cells_Name             : Name_Id; -- "toggleCells";
   Event_Cells_Toggles_Name            : Name_Id; -- "cellsToggled";
   Event_Update_Cell_Size_Name         : Name_Id; -- "updateCellSize";
   Event_Label_Changed_Name            : Name_Id; -- "labelChanged";
   Event_Add_Overlay_Name              : Name_Id; -- "addOverlay";
   Event_Remove_Overlay_Name           : Name_Id; -- "removeOverlay";
   Event_Before_Paint_Name             : Name_Id; -- "beforePaint";
   Event_Paint_Name                    : Name_Id; -- "paint";
   Event_After_Paint_Name              : Name_Id; -- "afterPaint";
   Event_Start_Editing_Name            : Name_Id; -- "startEditing";
   Event_Undo_Name                     : Name_Id; -- "undo";
   Event_Redo_Name                     : Name_Id; -- "redo";
   Event_Up_Name                       : Name_Id; -- "up";
   Event_Down_Name                     : Name_Id; -- "down";
   Event_Scale_Name                    : Name_Id; -- "scale";
   Event_Translate_Name                : Name_Id; -- "translate";
   Event_Scale_And_Translate_Name      : Name_Id; -- "scaleAndTranslate";

   Event_Change_Name : Name_Id; -- "change";
   -- Holds the name for the change event. First and only argument in the
   -- argument array is the list of mxAtomicGraphChanges that have been
   -- executed on the model.

   Event_Execute_Name : Name_Id; -- "execute";
   -- Holds the name for the execute event. First and only argument in the
   --- argument array is the mxAtomicGraphChange that has been executed on the 
   -- model. This event fires before the change event.

   Event_Before_Undo_Name : Name_Id; -- "beforeUndo";
   -- Holds the name for the beforeUndo event. First and only argument in the
   -- argument array is the current edit that is currently in progress in the 
   -- model. This event fires before notify is called on the currentEdit in
   -- he model.

   Event_Notify_Name : Name_Id; -- "notify";
   -- Holds the name for the norify event. First and only argument in the
   -- argument array is the list of mxAtomicGraphChanges that have been
   -- executed on the model. This event fires after the change event.

   Event_Begin_Update_Name : Name_Id; -- "beginUpdate";
   -- Holds the name for the beginUpdate event. This event has no arguments and
   -- fires after the updateLevel has been changed in model.

   Event_End_Update_Name : Name_Id; -- "endUpdate";
   -- Holds the name for the endUpdate event. This event has no arguments and 
   -- fires after the updateLevel has been changed in the model. First argument
   -- is the currentEdit.

   Event_Insert_Name  : Name_Id; -- "insert";
   Event_Add_Name     : Name_Id; -- "add";
   Event_Clear_Name   : Name_Id; -- "clear";
   Event_Fired_Name   : Name_Id; -- "fired";
   Event_Select_Name  : Name_Id; -- "select";

   Event_Mark_Name : Name_Id; -- "mark";
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

   Event_Root_Name         : Name_Id; -- "root";
   Event_Layout_Cells_Name : Name_Id; -- "layoutCells";
   Event_Start_Name        : Name_Id; -- "start";
   Event_Continue_Name     : Name_Id; -- "continue";
   Event_Stop_Name         : Name_Id; -- "stop";

   
   
   NS_SVG : Name_Id; -- "http://www.w3.org/2000/svg";
   -- Defines the SVG namespace.
	   
   NS_XHTML : Name_Id; -- "http://www.w3.org/1999/xhtml";
		       -- Defines the XHTML namespace.

   NS_XLINK : Name_Id; -- "http://www.w3.org/1999/xlink";
		       -- Defines the XLink namespace.

   -- Comma separated list of default fonts for CSS properties.
   -- And the default font family value for new image export.
   -- Default is Arial, Helvetica.
   DEFAULT_FONTFAMILIES : Name_Id; -- "Arial,Helvetica";
   
   DEFAULT_Font_Graphic : Name_Id; -- "Segoe UI";

   -- Defines the default font family. Default is "Dialog". (To be replaced
   -- with Font.DIALOG after EOL of Java 1.5.)
   DEFAULT_FONTFAMILY : Name_Id; -- "Dialog";

   -- Defines the default shadow color for stencils. Default is "gray".
   STENCIL_SHADOWCOLOR : Name_Id; -- "gray";

   -- Defines the color to be used to draw shadows in W3C standards. Default
   -- is gray.
   W3C_SHADOWCOLOR : Name_Id; -- "gray";

   -- Defines the transformation used to draw shadows in SVG.
   SVG_SHADOWTRANSFORM : Name_Id; -- "translate(2 3)";

   -- Defines the value for none. Default is "none".
   NONE : Name_Id; -- "none";

   -- Defines the key for the perimeter style.
   -- This is a function that defines the perimeter around a particular shape.
   -- Possible values are the functions defined in mxPerimeter that use the 
   -- <code>mxPerimeterFunction</code> interface. Alternatively, the constants
   -- in this class that start with <code>PERIMETER_</code> may be used to 
   -- access perimeter styles in <code>mxStyleRegistry</code>.
   STYLE_PERIMETER : Name_Id; -- "perimeter";

   -- Defines the ID of the cell that should be used for computing the
   -- perimeter point of the source for an edge. This allows for graphically
   -- connecting to a cell while keeping the actual terminal of the edge.
   STYLE_SOURCE_PORT : Name_Id; -- "sourcePort";

   -- Defines the ID of the cell that should be used for computing the
   -- perimeter point of the target for an edge. This allows for graphically
   -- connecting to a cell while keeping the actual terminal of the edge.
   STYLE_TARGET_PORT : Name_Id; -- "targetPort";

   -- Defines the direction(s) that edges are allowed to connect to cells in.
   -- Possible values are <code>DIRECTION_NORTH, DIRECTION_SOUTH, 
   -- DIRECTION_EAST</code> and <code>DIRECTION_WEST</code>.
   STYLE_PORT_CONSTRAINT : Name_Id; -- "portConstraint";

   -- Defines the key for the opacity style. The type of the value is 
   -- <code>float</code> and the possible range is 0-100.
   STYLE_OPACITY : Name_Id; -- "opacity";

   -- Defines the key for the text opacity style. The type of the value is 
   -- <code>float</code> and the possible range is 0-100.
   STYLE_TEXT_OPACITY : Name_Id; -- "textOpacity";

   -- Defines the key for the overflow style. Possible values are "visible",
   -- "hidden" and "fill". The default value is "visible". This value
   -- specifies how overlapping vertex labels are handles. A value of
   -- "visible" will show the complete label. A value of "hidden" will clip
   -- the label so that it does not overlap the vertex bounds. A value of
   -- "fill" will use the vertex bounds for the label.
   -- @see com.mxgraph.view.mxGraph#isLabelClipped(Object)
   STYLE_OVERFLOW : Name_Id; -- "overflow";

   -- Defines if the connection points on either end of the edge should be
   -- computed so that the edge is vertical or horizontal if possible and
   -- if the point is not at a fixed location. Default is false. This is
   -- used in mxGraph.isOrthogonal, which also returns true if the edgeStyle
   -- of the edge is an elbow or entity.
   STYLE_ORTHOGONAL : Name_Id; -- "orthogonal";

   -- Defines the key for the horizontal relative coordinate connection point
   -- of an edge with its source terminal.
   STYLE_EXIT_X : Name_Id; -- "exitX";

   -- Defines the key for the vertical relative coordinate connection point
   -- of an edge with its source terminal.
   STYLE_EXIT_Y : Name_Id; -- "exitY";
   
   -- Defines if the perimeter should be used to find the exact entry point
   -- along the perimeter of the source. Possible values are 0 (false) and
   -- 1 (true). Default is 1 (true).
   STYLE_EXIT_PERIMETER : Name_Id; -- "exitPerimeter";
	
   -- Defines the key for the horizontal relative coordinate connection point
   -- of an edge with its target terminal.
   STYLE_ENTRY_X : Name_Id; -- "entryX";
	
   -- Defines the key for the vertical relative coordinate connection point
   -- of an edge with its target terminal.
   STYLE_ENTRY_Y : Name_Id; -- "entryY";
	
   -- Defines if the perimeter should be used to find the exact entry point
   -- along the perimeter of the target. Possible values are 0 (false) and
   -- 1 (true). Default is 1 (true).
   STYLE_ENTRY_PERIMETER : Name_Id; -- "entryPerimeter";
	
   -- Defines the key for the white-space style. Possible values are "nowrap"
   -- and "wrap". The default value is "nowrap". This value specifies how
   -- white-space inside a HTML vertex label should be handled. A value of
   -- "nowrap" means the text will never wrap to the next line until a
   -- linefeed is encountered. A value of "wrap" means text will wrap when
   -- necessary.
   STYLE_WHITE_SPACE : Name_Id; -- "whiteSpace";
	
   -- Defines the key for the rotation style. The type of the value is 
   -- <code>double</code> and the possible range is 0-360.
   STYLE_ROTATION : Name_Id; -- "rotation";
	
   -- Defines the key for the fill color of the swimlane background.
   -- The value is a string expression supported by mxUtils.parseColor.
   -- @see com.mxgraph.util.mxUtils#parseColor(String)
   STYLE_SWIMLANE_FILLCOLOR : Name_Id; -- "swimlaneFillColor";
	
   -- Defines the key for the fillColor style. The value is a string
   -- expression supported by mxUtils.parseColor.
   -- @see com.mxgraph.util.mxUtils#parseColor(String)
   STYLE_FILLCOLOR : Name_Id; -- "fillColor";
	
   -- Defines the key for the gradientColor style. The value is a string
   -- expression supported by mxUtils.parseColor. This is ignored if no fill
   -- color is defined.
   -- @see com.mxgraph.util.mxUtils#parseColor(String)
   STYLE_GRADIENTCOLOR : Name_Id; -- "gradientColor";

   -- Defines the key for the gradient direction. Possible values are
   -- <code>DIRECTION_EAST</code>, <code>DIRECTION_WEST</code>,
   -- <code>DIRECTION_NORTH</code> and <code>DIRECTION_SOUTH</code>. Default
   -- is <code>DIRECTION_SOUTH</code>. Generally, and by default in mxGraph,
   -- gradient painting is done from the value of <code>STYLE_FILLCOLOR</code>
   -- to the value of <code>STYLE_GRADIENTCOLOR</code>. Taking the example of
   -- <code>DIRECTION_NORTH</code>, this means <code>STYLE_FILLCOLOR</code>
   -- color at the bottom of paint pattern and
   -- <code>STYLE_GRADIENTCOLOR</code> at top, with a gradient in-between.
   STYLE_GRADIENT_DIRECTION : Name_Id; -- "gradientDirection";
	
   -- Defines the key for the strokeColor style. The value is a string
   -- expression supported by mxUtils.parseColor.
   -- @see com.mxgraph.util.mxUtils#parseColor(String)
   STYLE_STROKECOLOR : Name_Id; -- "strokeColor";
	
   -- Defines the key for the separatorColor style. The value is a string
   -- expression supported by mxUtils.parseColor. This style is only used
   -- for SHAPE_SWIMLANE shapes.
   -- @see com.mxgraph.util.mxUtils#parseColor(String)
   STYLE_SEPARATORCOLOR : Name_Id; -- "separatorColor";
   
   -- Defines the key for the strokeWidth style. The type of the value is
   -- <code>float</code> and the possible range is any non-negative value.
   -- The value reflects the stroke width in pixels.
   STYLE_STROKEWIDTH : Name_Id; -- "strokeWidth";
   
   -- Defines the key for the align style. Possible values are
   -- <code>ALIGN_LEFT</code>, <code>ALIGN_CENTER</code> and
   -- <code>ALIGN_RIGHT</code>. This value defines how the lines of the label
   -- are horizontally aligned. <code>ALIGN_LEFT</code> mean label text lines
   -- are aligned to left of the label bounds, <code>ALIGN_RIGHT</code> to the
   -- right of the label bounds and <code>ALIGN_CENTER</code> means the
   -- center of the text lines are aligned in the center of the label bounds.
   -- Note this value doesn't affect the positioning of the overall label
   -- bounds relative to the vertex, to move the label bounds horizontally, use
   -- <code>STYLE_LABEL_POSITION</code>.
   STYLE_ALIGN : Name_Id; -- "align";
	
   -- Defines the key for the verticalAlign style. Possible values are
   -- <code>ALIGN_TOP</code>, <code>ALIGN_MIDDLE</code> and
   -- <code>ALIGN_BOTTOM</code>. This value defines how the lines of the label
   -- are vertically aligned. <code>ALIGN_TOP</code> means the topmost label
   -- text line is aligned against the top of the label bounds,
   -- <code>ALIGN_BOTTOM</code> means the bottom-most label text line is
   -- aligned against the bottom of the label bounds and
   -- <code>ALIGN_MIDDLE</code> means there is equal spacing between the
   -- topmost text label line and the top of the label bounds and the
   -- bottom-most text label line and the bottom of the label bounds. Note
   -- this value doesn't affect the positioning of the overall label bounds
   -- relative to the vertex, to move the label bounds vertically, use
   -- <code>STYLE_VERTICAL_LABEL_POSITION</code>.
   STYLE_VERTICAL_ALIGN : Name_Id; -- "verticalAlign";
	
   -- Defines the key for the horizontal label position of vertices. Possible
   -- values are <code>ALIGN_LEFT</code>, <code>ALIGN_CENTER</code> and
   -- <code>ALIGN_RIGHT</code>. Default is <code>ALIGN_CENTER</code>. The
   -- label align defines the position of the label relative to the cell.
   -- <code>ALIGN_LEFT</code> means the entire label bounds is placed
   -- completely just to the left of the vertex, <code>ALIGN_RIGHT</code>
   -- means adjust to the right and <code>ALIGN_CENTER</code> means the label
   -- bounds are vertically aligned with the bounds of the vertex. Note this
   -- value doesn't affect the positioning of label within the label bounds,
   -- to move the label horizontally within the label bounds, use
   -- <code>STYLE_ALIGN</code>.
   STYLE_LABEL_POSITION : Name_Id; -- "labelPosition";
	
   -- Defines the key for the vertical label position of vertices. Possible
   -- values are <code>ALIGN_TOP</code>, <code>ALIGN_BOTTOM</code> and
   -- <code>ALIGN_MIDDLE</code>. Default is <code>ALIGN_MIDDLE</code>. The
   -- label align defines the position of the label relative to the cell.
   -- <code>ALIGN_TOP</code> means the entire label bounds is placed
   -- completely just on the top of the vertex, <code>ALIGN_BOTTOM</code>
   -- means adjust on the bottom and <code>ALIGN_MIDDLE</code> means the label
   -- bounds are horizontally aligned with the bounds of the vertex. Note
   -- this value doesn't affect the positioning of label within the label
   -- bounds, to move the label vertically within the label bounds, use
   -- <code>STYLE_VERTICAL_ALIGN</code>.
   STYLE_VERTICAL_LABEL_POSITION : Name_Id; -- "verticalLabelPosition";

   -- Defines the key for the align style. Possible values are
   -- <code>ALIGN_LEFT</code>, <code>ALIGN_CENTER</code> and
   -- <code>ALIGN_RIGHT</code>. The value defines how any image in the vertex
   -- label is aligned horizontally within the label bounds of a SHAPE_LABEL
   -- shape.
   STYLE_IMAGE_ALIGN : Name_Id; -- "imageAlign";
	
   -- Defines the key for the verticalAlign style. Possible values are
   -- <code>ALIGN_TOP</code>, <code>ALIGN_MIDDLE</code> and
   -- <code>ALIGN_BOTTOM</code>. The value defines how any image in the vertex
   -- label is aligned vertically within the label bounds of a SHAPE_LABEL
   -- shape.
   STYLE_IMAGE_VERTICAL_ALIGN : Name_Id; -- "imageVerticalAlign";
	
   -- Defines the key for the glass style. Possible values are 0 (disabled) and
   -- 1(enabled). The default value is 0. This is used in mxLabel.
   STYLE_GLASS : Name_Id; -- "glass";
	
   -- Defines the key for the image style. Possible values are any image URL,
   -- registered key in mxImageResources or short data URI as defined in
   -- mxImageBundle.
   -- The type of the value is <code>String</code>. This is the path to the
   -- image to image that is to be displayed within the label of a vertex. See
   -- mxGraphics2DCanvas.getImageForStyle, loadImage and setImageBasePath on
   -- how the image URL is resolved. Finally, mxUtils.loadImage is used for
   -- loading the image for a given value.
   STYLE_IMAGE : Name_Id; -- "image";

   -- Defines the key for the imageWidth style. The type of this value is
   -- <code>int</code>, the value is the image width in pixels and must be
   -- greated than 0.
   STYLE_IMAGE_WIDTH : Name_Id; -- "imageWidth";
	
   -- Defines the key for the imageHeight style The type of this value is
   -- <code>int</code>, the value is the image height in pixels and must be
   -- greater than 0.
   STYLE_IMAGE_HEIGHT : Name_Id; -- "imageHeight";
 	
   -- Defines the key for the image background color. This style is only used
   -- for image shapes. Possible values are all HTML color names or HEX codes.
   STYLE_IMAGE_BACKGROUND : Name_Id; -- "imageBackground";
	
   -- Defines the key for the image border color. This style is only used for
   -- image shapes. Possible values are all HTML color names or HEX codes.
     STYLE_IMAGE_BORDER : Name_Id; -- "imageBorder";
	
     -- Defines the key for the horizontal image flip. This style is only used
     -- in mxImageShape. Possible values are 0 and 1. Default is 0.
     STYLE_IMAGE_FLIPH : Name_Id; -- "imageFlipH";
	
     -- Defines the key for the vertical image flip. This style is only used
     -- in mxImageShape. Possible values are 0 and 1. Default is 0.
     STYLE_IMAGE_FLIPV : Name_Id; -- "imageFlipV";
	
     -- Defines the key for the horizontal stencil flip. This style is only used
     -- for <mxStencilShape>. Possible values are 0 and 1. Default is 0.
     STYLE_STENCIL_FLIPH : Name_Id; -- "stencilFlipH";
	
     -- Defines the key for the vertical stencil flip. This style is only used
     -- for <mxStencilShape>. Possible values are 0 and 1. Default is 0.
     STYLE_STENCIL_FLIPV : Name_Id; -- "stencilFlipV";
	
     -- Defines the key for the horizontal image flip. This style is only used
     -- in <mxImageShape>. Possible values are 0 and 1. Default is 0.
     STYLE_FLIPH : Name_Id; -- "flipH";
	
     -- Variable: STYLE_FLIPV
     -- 
     -- Defines the key for the vertical flip. Possible values are 0 and 1.
     -- Default is 0.
     STYLE_FLIPV : Name_Id; -- "flipV";
	
     -- Defines the key for the noLabel style. If this is
     -- true then no label is visible for a given cell.
     -- Possible values are true or false (1 or 0).
     -- Default is false.
     STYLE_NOLABEL : Name_Id; -- "noLabel";
     
     -- Defines the key for the noEdgeStyle style. If this is true then no edge
     -- style is applied for a given edge. Possible values are true or false
     -- (1 or 0). Default is false.
     STYLE_NOEDGESTYLE : Name_Id; -- "noEdgeStyle";
     
     -- Defines the key for the label background color. The value is a string
     -- expression supported by mxUtils.parseColor.
     -- @see com.mxgraph.util.mxUtils#parseColor(String)
     STYLE_LABEL_BACKGROUNDCOLOR : Name_Id; -- "labelBackgroundColor";
	
     -- Defines the key for the label border color. The value is a string
     -- expression supported by mxUtils.parseColor.
     -- @see com.mxgraph.util.mxUtils#parseColor(String)
     STYLE_LABEL_BORDERCOLOR : Name_Id; -- "labelBorderColor";
	
     -- Defines the key for the indicatorShape style.
     -- Possible values are any of the SHAPE_*
     -- constants.
     STYLE_INDICATOR_SHAPE : Name_Id; -- "indicatorShape";
	
     -- Defines the key for the indicatorImage style.
     -- Possible values are any image URL, the type of the value is
     -- <code>String</code>.
     STYLE_INDICATOR_IMAGE : Name_Id; -- "indicatorImage";
	
     -- Defines the key for the indicatorColor style. The value is a string
     -- expression supported by mxUtils.parseColor.
     -- @see com.mxgraph.util.mxUtils#parseColor(String)
     STYLE_INDICATOR_COLOR : Name_Id; -- "indicatorColor";
	
     -- Defines the key for the indicatorGradientColor style. The value is a
     -- string expression supported by mxUtils.parseColor. This style is only
     -- supported in SHAPE_LABEL shapes.
     -- @see com.mxgraph.util.mxUtils#parseColor(String)
     STYLE_INDICATOR_GRADIENTCOLOR : Name_Id; -- "indicatorGradientColor";

     -- Defines the key for the indicatorSpacing style (in px).
     STYLE_INDICATOR_SPACING : Name_Id; -- "indicatorSpacing";
	
     -- Defines the key for the indicatorWidth style (in px).
     STYLE_INDICATOR_WIDTH : Name_Id; -- "indicatorWidth";
     
     -- Defines the key for the indicatorHeight style (in px).
     STYLE_INDICATOR_HEIGHT : Name_Id; -- "indicatorHeight";
     
     -- Defines the key for the shadow style. The type of the value is
     -- <code>boolean</code>. This style applies to vertices and arrow style
     -- edges.
     STYLE_SHADOW : Name_Id; -- "shadow";
	
     -- Defines the key for the segment style. The type of this value is
     -- <code>float</code> and the value represents the size of the horizontal
     -- segment of the entity relation style. Default is ENTITY_SEGMENT.
     STYLE_SEGMENT : Name_Id; -- "segment";
	
     -- Defines the key for the endArrow style.
     -- Possible values are all constants in this
     -- class that start with ARROW_. This style is
     -- supported in the <code>mxConnector</code> shape.
     STYLE_ENDARROW : Name_Id; -- "endArrow";
	
     -- Defines the key for the startArrow style.
     -- Possible values are all constants in this
     -- class that start with ARROW_.
     -- See STYLE_ENDARROW.
     -- This style is supported in the mxConnector shape.
     STYLE_STARTARROW : Name_Id; -- "startArrow";
     
     -- Defines the key for the endSize style. The type of this value is 
     -- <code>float</code> and the value represents the size of the end 
     -- marker in pixels.
     STYLE_ENDSIZE : Name_Id; -- "endSize";
	
     -- Defines the key for the startSize style. The type of this value is
     -- <code>float</code> and the value represents the size of the start marker
     -- or the size of the swimlane title region depending on the shape it is
     -- used for.
     STYLE_STARTSIZE : Name_Id; -- "startSize";
	
     -- Defines the key for the swimlaneLine style. This style specifies whether
     -- the line between the title regio of a swimlane should be visible. Use 0
     -- for hidden or 1 (default) for visible. Value is "swimlaneLine".
     STYLE_SWIMLANE_LINE : Name_Id; -- "swimlaneLine";
	
     -- Defines the key for the endFill style. Use 0 for no fill or 1
     -- (default) for fill. (This style is only exported via <mxImageExport>.)
     STYLE_ENDFILL : Name_Id; -- "endFill";
	
     -- Defines the key for the startFill style. Use 0 for no fill or 1
     -- (default) for fill. (This style is only exported via <mxImageExport>.)
     STYLE_STARTFILL : Name_Id; -- "startFill";
	
     -- Defines the key for the dashed style. The type of this value is
     -- <code>boolean</code> and the value determines whether or not an edge or
     -- border is drawn with a dashed pattern along the line.
     STYLE_DASHED : Name_Id; -- "dashed";
	
     -- Defines the key for the dashed pattern style. The type of this value
     -- is <code>float[]</code> and the value specifies the dashed pattern 
     -- to apply to edges drawn with this style. This style allows the user
     -- to specify a custom-defined dash pattern. This is done using a series
     -- of numbers. Dash styles are defined in terms of the length of the dash
     -- (the drawn part of the stroke) and the length of the space between the
     -- dashes. The lengths are relative to the line width: a length of "1" is
     -- equal to the line width.
     STYLE_DASH_PATTERN : Name_Id; -- "dashPattern";
     
     -- Defines the key for the rounded style. The type of this value is
     -- <code>boolean</code>. For edges this determines whether or not joins
     -- between edges segments are smoothed to a rounded finish. For vertices
     -- that have the rectangle shape, this determines whether or not the
     -- rectangle is rounded.
     STYLE_ROUNDED : Name_Id; -- "rounded";
     
     -- Defines the rounding factor for a rounded rectangle in percent
     -- (without the percent sign). Possible values are between 0 and 100. If
     -- this value is not specified then RECTANGLE_ROUNDING_FACTOR -- 100 is
     -- used. For edges, this defines the absolute size of rounded corners in
     -- pixels. If this values is not specified then LINE_ARCSIZE is used.
     -- (This style is only exported via <mxImageExport>.) Value is "arcSize".
     STYLE_ARCSIZE : Name_Id; -- "arcSize";
     
     -- Defines the key for the source perimeter spacing. The type of this value
     -- is <code>double</code>. This is the distance between the source
     -- connection point of an edge and the perimeter of the source vertex in
     -- pixels. This style only applies to edges.
     STYLE_SOURCE_PERIMETER_SPACING : Name_Id; -- "sourcePerimeterSpacing";
     
     -- Defines the key for the target perimeter spacing. The type of this value
     -- is <code>double</code>. This is the distance between the target
     -- connection point of an edge and the perimeter of the target vertex in
     -- pixels. This style only applies to edges.
     STYLE_TARGET_PERIMETER_SPACING : Name_Id; -- "targetPerimeterSpacing";
	
     -- Defines the key for the perimeter spacing. This is the distance between
     -- the connection point and the perimeter in pixels. When used in a vertex
     -- style, this applies to all incoming edges to floating ports (edges that
     -- terminate on the perimeter of the vertex). When used in an edge style,
     -- this spacing applies to the source and target separately, if they
     -- terminate in floating ports (on the perimeter of the vertex).
     STYLE_PERIMETER_SPACING : Name_Id; -- "perimeterSpacing";
	
     -- Defines the key for the spacing. The value represents the spacing, in
     -- pixels, added to each side of a label in a vertex (style applies to
     -- vertices only).
     STYLE_SPACING : Name_Id; -- "spacing";
	
     -- Defines the key for the spacingTop style. The value represents the
     -- spacing, in pixels, added to the top side of a label in a vertex (style
     -- applies to vertices only).
     STYLE_SPACING_TOP : Name_Id; -- "spacingTop";
     
     -- Defines the key for the spacingLeft style. The value represents the
     -- spacing, in pixels, added to the left side of a label in a vertex (style
     -- applies to vertices only).
     STYLE_SPACING_LEFT : Name_Id; -- "spacingLeft";

     -- Defines the key for the spacingBottom style The value represents the
     -- spacing, in pixels, added to the bottom side of a label in a vertex
     -- (style applies to vertices only).
     STYLE_SPACING_BOTTOM : Name_Id; -- "spacingBottom";
	
     -- Defines the key for the spacingRight style The value represents the
     -- spacing, in pixels, added to the right side of a label in a vertex
     -- (style applies to vertices only).
     STYLE_SPACING_RIGHT : Name_Id; -- "spacingRight";
	
     -- Defines the key for the horizontal style. Possible values are
     -- <code>true</code> or <code>false</code>. This value only applies to
     -- vertices. If the <code>STYLE_SHAPE</code> is <code>SHAPE_SWIMLANE</code>
     -- a value of <code>false</code> indicates that the swimlane should be
     -- drawn vertically, <code>true</code> indicates to draw it horizontally. 
     -- If the shape style does not indicate that this vertex is a swimlane,
     -- this value affects only whether the label is drawn horizontally or
     -- vertically.
     STYLE_HORIZONTAL : Name_Id; -- "horizontal";
	
     -- Defines the key for the direction style. The direction style is used to
     -- specify the direction of certain shapes (eg. <code>mxTriangle</code>).
     -- Possible values are <code>DIRECTION_EAST</code> (default),
     -- <code>DIRECTION_WEST</code>, <code>DIRECTION_NORTH</code> and
     -- <code>DIRECTION_SOUTH</code>. This value only applies to vertices.
     STYLE_DIRECTION : Name_Id; -- "direction";
	
     -- Defines the key for the elbow style. Possible values are
     -- <code>ELBOW_HORIZONTAL</code> and <code>ELBOW_VERTICAL</code>. Default
     -- is <code>ELBOW_HORIZONTAL</code>. This defines how the three segment
     -- orthogonal edge style leaves its terminal vertices. The vertical style
     -- leaves the terminal vertices at the top and bottom sides.
     STYLE_ELBOW : Name_Id; -- "elbow";
	
     -- Defines the key for the fontColor style. The value is type
     -- <code>String</code> and of the expression supported by
     -- mxUtils.parseColor.
     -- @see com.mxgraph.util.mxUtils#parseColor(String)
     STYLE_FONTCOLOR : Name_Id; -- "fontColor";
	
     -- Defines the key for the fontFamily style. Possible values are names such
     -- as Arial; Dialog; Verdana; Times New Roman. The value is of type
     -- <code>String</code>.
     STYLE_FONTFAMILY : Name_Id; -- "fontFamily";
	
     -- Defines the key for the fontSize style (in points). The type of the 
     -- value is <code>int</code>.
     STYLE_FONTSIZE : Name_Id; -- "fontSize";
     
     -- Defines the key for the fontStyle style. Values may be any logical AND
     -- (sum) of FONT_BOLD, FONT_ITALIC and FONT_UNDERLINE. The type
     -- of the value is <code>int</code>.
     STYLE_FONTSTYLE : Name_Id; -- "fontStyle";
     
     -- Defines the key for the autosize style. This specifies if a cell should
     -- be resized automatically if the value has changed. Possible values are 
     -- 0 or 1. Default is 0. See mxGraph.isAutoSizeCell. This is normally 
     -- combined with STYLE_RESIZABLE to disable manual sizing.
     STYLE_AUTOSIZE : Name_Id; -- "autosize";
	
     -- Defines the key for the foldable style. This specifies if a cell is 
     -- foldable using a folding icon. Possible values are 0 or 1. Default is 
     -- 1. 
     -- See mxGraph.isCellFoldable.
     STYLE_FOLDABLE : Name_Id; -- "foldable";
	
     -- Defines the key for the editable style. This specifies if the value of
     -- a cell can be edited using the in-place editor. Possible values are 0 or
     -- 1. Default is 1. See mxGraph.isCellEditable.
     STYLE_EDITABLE : Name_Id; -- "editable";
	
     -- Defines the key for the bendable style. This specifies if the control
     -- points of an edge can be moved. Possible values are 0 or 1. Default is
     -- 1. See mxGraph.isCellBendable.
     STYLE_BENDABLE : Name_Id; -- "bendable";
	
     -- Defines the key for the movable style. This specifies if a cell can
     -- be moved. Possible values are 0 or 1. Default is 1. See
     -- mxGraph.isCellMovable.
     STYLE_MOVABLE : Name_Id; -- "movable";
	
     -- Defines the key for the resizable style. This specifies if a cell can
     -- be resized. Possible values are 0 or 1. Default is 1. See
     -- mxGraph.isCellResizable.
     STYLE_RESIZABLE : Name_Id; -- "resizable";
	
     -- Defines the key for the cloneable style. This specifies if a cell can
     -- be cloned. Possible values are 0 or 1. Default is 1. See
     -- mxGraph.isCellCloneable.
     STYLE_CLONEABLE : Name_Id; -- "cloneable";
	
     -- Defines the key for the deletable style. This specifies if a cell can be
     -- deleted. Possible values are 0 or 1. Default is 1. See
     -- mxGraph.isCellDeletable.
     STYLE_DELETABLE : Name_Id; -- "deletable";
	
     -- Defines the key for the shape style.
     -- Possible values are any of the SHAPE_*
     -- constants.
     STYLE_SHAPE : Name_Id; -- "shape";
	
     -- Takes a function that creates points. Possible values are the
     -- functions defined in mxEdgeStyle.
     STYLE_EDGE : Name_Id; -- "edgeStyle";
	
     -- Defines the key for the loop style. Possible values are the
     -- functions defined in mxEdgeStyle.
     STYLE_LOOP : Name_Id; -- "loopStyle";
	
     -- Defines the key for the horizontal routing center. Possible values are
     -- between -0.5 and 0.5. This is the relative offset from the center used
     -- for connecting edges. The type of this value is <code>float</code>.
     STYLE_ROUTING_CENTER_X : Name_Id; -- "routingCenterX";
	
     -- Defines the key for the vertical routing center. Possible values are
     -- between -0.5 and 0.5. This is the relative offset from the center used
     -- for connecting edges. The type of this value is <code>float</code>.
     STYLE_ROUTING_CENTER_Y : Name_Id; -- "routingCenterY";
	
     SHAPE_RECTANGLE : Name_Id;
     -- SHAPE_RECTANGLE
     
     -- SHAPE_ELLIPSE
     SHAPE_ELLIPSE : Name_Id; -- "ellipse";
     
     -- SHAPE_DOUBLE_RECTANGLE
     SHAPE_DOUBLE_RECTANGLE : Name_Id; -- "doubleRectangle";
     
     -- SHAPE_DOUBLE_ELLIPSE
     SHAPE_DOUBLE_ELLIPSE : Name_Id; -- "doubleEllipse";
     
     -- SHAPE_RHOMBUS
     SHAPE_RHOMBUS : Name_Id; -- "rhombus";
     
     -- SHAPE_LINE
     SHAPE_LINE : Name_Id; -- "line";
     
     -- SHAPE_IMAGE
     SHAPE_IMAGE : Name_Id; -- "image";
     
     -- SHAPE_ARROW
     SHAPE_ARROW : Name_Id; -- "arrow";
     
     -- SHAPE_ARROW
     SHAPE_CURVE : Name_Id; -- "curve";
     
     -- SHAPE_LABEL
     SHAPE_LABEL : Name_Id; -- "label";
     
     -- SHAPE_CYLINDER
     SHAPE_CYLINDER : Name_Id; -- "cylinder";
     
     -- SHAPE_SWIMLANE
     SHAPE_SWIMLANE : Name_Id; -- "swimlane";
     
     -- SHAPE_CONNECTOR
     SHAPE_CONNECTOR : Name_Id; -- "connector";
     
     -- SHAPE_ACTOR
     SHAPE_ACTOR : Name_Id; -- "actor";
     
     -- SHAPE_CLOUD
     SHAPE_CLOUD : Name_Id; -- "cloud";
     
     -- SHAPE_TRIANGLE
     SHAPE_TRIANGLE : Name_Id; -- "triangle";
     
     -- SHAPE_HEXAGON
     SHAPE_HEXAGON : Name_Id; -- "hexagon";
     
     -- ARROW_CLASSIC
     ARROW_CLASSIC : Name_Id; -- "classic";
     
     -- ARROW_BLOCK
     ARROW_BLOCK : Name_Id; -- "block";
     
     -- ARROW_OPEN
     ARROW_OPEN : Name_Id; -- "open";
     
     -- ARROW_BLOCK
     ARROW_OVAL : Name_Id; -- "oval";
     
     -- ARROW_OPEN
     ARROW_DIAMOND : Name_Id; -- "diamond";
     
     -- ALIGN_LEFT
     ALIGN_LEFT : Name_Id; -- "left";
     
     -- ALIGN_CENTER
     ALIGN_CENTER : Name_Id; -- "center";
     
     -- ALIGN_RIGHT
     ALIGN_RIGHT : Name_Id; -- "right";
     
     -- ALIGN_TOP
     ALIGN_TOP : Name_Id; -- "top";
     
     -- ALIGN_MIDDLE
     ALIGN_MIDDLE : Name_Id; -- "middle";
     
     -- ALIGN_BOTTOM
     ALIGN_BOTTOM : Name_Id; -- "bottom";
     
     -- DIRECTION_NORTH
     DIRECTION_NORTH : Name_Id; -- "north";
     
     -- DIRECTION_SOUTH
     DIRECTION_SOUTH : Name_Id; -- "south";
     
     -- DIRECTION_EAST
     DIRECTION_EAST : Name_Id; -- "east";
     
     -- DIRECTION_WEST
     DIRECTION_WEST : Name_Id; -- "west";
     
     -- ELBOW_VERTICAL
     ELBOW_VERTICAL : Name_Id; -- "vertical";
     
     -- ELBOW_HORIZONTAL
     ELBOW_HORIZONTAL : Name_Id; -- "horizontal";
     
     -- Name of the elbow edge style. Can be used as a string value
     -- for the STYLE_EDGE style.
     EDGESTYLE_ELBOW : Name_Id; -- "elbowEdgeStyle";
     
     -- Name of the entity relation edge style. Can be used as a string value
     -- for the STYLE_EDGE style.
     EDGESTYLE_ENTITY_RELATION : Name_Id; -- "entityRelationEdgeStyle";
     
     -- Name of the loop edge style. Can be used as a string value
     -- for the STYLE_EDGE style.
     EDGESTYLE_LOOP : Name_Id; -- "loopEdgeStyle";
     
     -- Name of the side to side edge style. Can be used as a string value
     -- for the STYLE_EDGE style.
     EDGESTYLE_SIDETOSIDE : Name_Id; -- "sideToSideEdgeStyle";
     
     -- Name of the top to bottom edge style. Can be used as a string value
     -- for the STYLE_EDGE style.
     EDGESTYLE_TOPTOBOTTOM : Name_Id; -- "topToBottomEdgeStyle";
     
     -- Name of the orthogonal edge style. Can be used as a string value for
     -- the STYLE_EDGE style.
     EDGESTYLE_ORTHOGONAL : Name_Id; -- "orthogonalEdgeStyle";
     
     -- Name of the generic segment edge style. Can be used as a string value
     -- for the STYLE_EDGE style.
     EDGESTYLE_SEGMENT : Name_Id; -- "segmentEdgeStyle";
     
     -- Name of the ellipse perimeter. Can be used as a string value
     -- for the STYLE_PERIMETER style.
     PERIMETER_ELLIPSE : Name_Id; -- "ellipsePerimeter";
     
     -- Name of the rectangle perimeter. Can be used as a string value
     -- for the STYLE_PERIMETER style.
     PERIMETER_RECTANGLE : Name_Id; -- "rectanglePerimeter";
     
     -- Name of the rhombus perimeter. Can be used as a string value
     -- for the STYLE_PERIMETER style.
     PERIMETER_RHOMBUS : Name_Id; -- "rhombusPerimeter";
     
     -- Name of the triangle perimeter. Can be used as a string value
     -- for the STYLE_PERIMETER style.
     PERIMETER_TRIANGLE : Name_Id; -- "trianglePerimeter";
     
     -- Name of the hexagon perimeter. Can be used as a string value
     -- for the STYLE_PERIMETER style.
     PERIMETER_HEXAGON  : Name_Id; -- "hexagonPerimeter";
     
     Default_Vertex_Style_Name : Name_Id;
     Default_Edge_Style_Name   : Name_Id;
     
     
     Elbow_Connector_Function_Name           : Name_Id;
     Entity_Relation_Connector_Function_Name : Name_Id;
     Loop_Connector_Function_Name            : Name_Id;
     Side_To_Side_Connector_Function_Name    : Name_Id;
     Top_To_Bottom_Connector_Function_Name   : Name_Id;
     Orth_Connector_Function_Name            : Name_Id;
     Segment_Connector_Function_Name         : Name_Id;
     Ellipse_Perimeter_Function_Name         : Name_Id;
     Rectangle_Perimeter_Function_Name       : Name_Id;
     Rhombus_Perimeter_Function_Name         : Name_Id;
     Triangle_Perimeter_Function_Name        : Name_Id;
     Hexagon_Perimeter_Function_Name         : Name_Id;
     
     Name_True  : Name_Id;
     Name_False : Name_Id;
     Name_One   : Name_Id;
     Name_Zero  : Name_Id;
     
     -- Colors Names --
     ------------------
     
     
     AliceBlue_Color_Name         : Name_Id; -- "aliceblue",            "#F0F8FF"
     AntiqueWhite_Color_Name      : Name_Id; -- "antiquewhite",         "#FAEBD7"
     Aqua_Color_Name              : Name_Id; -- "aqua",                 "#00FFFF"
     AquaMarine_Color_Name        : Name_Id; -- "aquamarine",           "#7FFFD4"
     Azure_Color_Name             : Name_Id; -- "azure",                "#F0FFFF"
     Beige_Coor              : Name_Id; -- "beige",                "#F5F5DC"
     Bisque_Color_Name            : Name_Id; -- "bisque",               "#FFE4C4"
     Black_Color_Name             : Name_Id; -- "black",                "#000000"
     BlancheDalmond_Color_Name    : Name_Id; -- "blanchedalmond",       "#FFEBCD"
     Blue_Color_Name              : Name_Id; -- "blue",                 "#0000FF"
     BlueViolet_Color_Name        : Name_Id; -- "blueviolet",           "#8A2BE2"
     Brown_Color_Name             : Name_Id; -- "brown",                "#A52A2A"
     BurlyWood_Color_Name         : Name_Id; -- "burlywood",            "#DEB887"
     CadetBlue_Color_Name         : Name_Id; -- "cadetblue",            "#5F9EA0"
     Chartreuse_Color_Name        : Name_Id; -- "chartreuse",           "#7FFF00"
     Chocolate_Color_Name         : Name_Id; -- "chocolate",            "#D2691E"
     Coral_Color_Name             : Name_Id; -- "coral",                "#FF7F50"
     CornFlowerBlue_Color_Name    : Name_Id; -- "cornflowerblue",       "#6495ED"
     Cornsilk_Color_Name          : Name_Id; -- "cornsilk",             "#FFF8DC"
     Crimson_Color_Name           : Name_Id; -- "crimson",              "#DC143C"
     Cyan_Color_Name              : Name_Id; -- "cyan",                 "#00FFFF"
     Darkblue_Color_Name          : Name_Id; -- "darkblue",           "#00008B"
     Darkcyan_Color_Name          : Name_Id; -- "darkcyan",           "#008B8B"
     Darkgoldenrod_Color_Name     : Name_Id; -- "darkgoldenrod",      "#B8860B"
     Darkgray_Color_Name          : Name_Id; -- "darkgray",           "#A9A9A9"
     Darkgrey_Color_Name          : Name_Id; -- "darkgrey",           "#A9A9A9"
     Darkgreen_Color_Name         : Name_Id; -- "darkgreen",          "#006400"
     Darkkhaki_Color_Name         : Name_Id; -- "darkkhaki",          "#BDB76B"
     Darkmagenta_Color_Name       : Name_Id; -- "darkmagenta",        "#8B008B"
     Darkolivegreen_Color_Name    : Name_Id; -- "darkolivegreen",     "#556B2F"
     Darkorange_Color_Name        : Name_Id; -- "darkorange",         "#FF8C00"
     Darkorchid_Color_Name        : Name_Id; -- "darkorchid",         "#9932CC"
     Darkred_Color_Name           : Name_Id; -- "darkred",            "#8B0000"
     Darksalmon_Color_Name        : Name_Id; -- "darksalmon",         "#E9967A"
     Darkseagreen_Color_Name      : Name_Id; -- "darkseagreen",       "#8FBC8F"
     Darkslateblue_Color_Name     : Name_Id; -- "darkslateblue",      "#483D8B"
     Darkslategray_Color_Name     : Name_Id; -- "darkslategray",      "#2F4F4F"
     Darkslategrey_Color_Name     : Name_Id; -- "darkslategrey",      "#2F4F4F"
     Darkturquoise_Color_Name     : Name_Id; -- "darkturquoise",      "#00CED1"
     Darkviolet_Color_Name        : Name_Id; -- "darkviolet",         "#9400D3"
     Deeppink_Color_Name          : Name_Id; -- "deeppink",           "#FF1493"
     Deepskyblue_Color_Name       : Name_Id; -- "deepskyblue",        "#00BFFF"
     Dimgray_Color_Name           : Name_Id; -- "dimgray",            "#696969"
     Dimgrey_Color_Name           : Name_Id; -- "dimgrey",            "#696969"
     Dodgerblue_Color_Name        : Name_Id; -- "dodgerblue",         "#1E90FF"
     Firebrick_Color_Name         : Name_Id; -- "firebrick",          "#B22222"
     Floralwhite_Color_Name       : Name_Id; -- "floralwhite",        "#FFFAF0"
     Forestgreen_Color_Name       : Name_Id; -- "forestgreen",        "#228B22"
     Fuchsia_Color_Name           : Name_Id; -- "fuchsia",            "#FF00FF"
     Gainsboro_Color_Name         : Name_Id; -- "gainsboro",          "#DCDCDC"
     Ghostwhite_Color_Name        : Name_Id; -- "ghostwhite",         "#F8F8FF"
     Gold_Color_Name              : Name_Id; -- "gold",               "#FFD700"
     Goldenrod_Color_Name         : Name_Id; -- "goldenrod",          "#DAA520"
     Gray_Color_Name              : Name_Id; -- "gray",               "#808080"
     Grey_Color_Name              : Name_Id; -- "grey",               "#808080"
     Green_Color_Name             : Name_Id; -- "green",              "#008000"
     Greenyellow_Color_Name       : Name_Id; -- "greenyellow",        "#ADFF2F"
     Honeydew_Color_Name          : Name_Id; -- "honeydew",           "#F0FFF0"
     Hotpink_Color_Name           : Name_Id; -- "hotpink",            "#FF69B4"
     Indianred_Color_Name         : Name_Id; -- "indianred",          "#CD5C5C"
     Indigo_Color_Name            : Name_Id; -- "indigo",             "#4B0082"
     Ivory_Color_Name             : Name_Id; -- "ivory",              "#FFFFF0"
     Khaki_Color_Name             : Name_Id; -- "khaki",              "#F0E68C"
     Lavender_Color_Name          : Name_Id; -- "lavender",           "#E6E6FA"
     Lavenderblush_Color_Name     : Name_Id; -- "lavenderblush",      "#FFF0F5"
     Lawngreen_Color_Name         : Name_Id; -- "lawngreen",          "#7CFC00"
     Lemonchiffon_Color_Name      : Name_Id; -- "lemonchiffon",       "#FFFACD"
     Lightblue_Color_Name         : Name_Id; -- "lightblue",          "#ADD8E6"
     Lightcoral_Color_Name        : Name_Id; -- "lightcoral",         "#F08080"
     Lightcyan_Color_Name         : Name_Id; -- "lightcyan",          "#E0FFFF"
     Lightgoldenrodyellow_Color_Name : Name_Id; --"lightgoldenrodyellow", "#FAFAD2"
     Lightgray_Color_Name         : Name_Id; -- "lightgray",          "#D3D3D3"
     Lightgrey_Color_Name         : Name_Id; -- "lightgrey",          "#D3D3D3"
     Lightgreen_Color_Name        : Name_Id; -- "lightgreen",         "#90EE90"
     Lightpink_Color_Name         : Name_Id; -- "lightpink",          "#FFB6C1"
     Lightsalmon_Color_Name       : Name_Id; -- "lightsalmon",        "#FFA07A"
     Lightseagreen_Color_Name     : Name_Id; -- "lightseagreen",      "#20B2AA"
     Lightskyblue_Color_Name      : Name_Id; -- "lightskyblue",       "#87CEFA"
     Lightslategray_Color_Name    : Name_Id; -- "lightslategray",     "#778899"
     Lightslategrey_Color_Name    : Name_Id; -- "lightslategrey",     "#778899"
     Lightsteelblue_Color_Name    : Name_Id; -- "lightsteelblue",     "#B0C4DE"
     Lightyellow_Color_Name       : Name_Id; -- "lightyellow",        "#FFFFE0"
     Lime_Color_Name              : Name_Id; -- "lime",               "#00FF00"
     Limegreen_Color_Name         : Name_Id; -- "limegreen",          "#32CD32"
     Linen_Color_Name             : Name_Id; -- "linen",              "#FAF0E6"
     Magenta_Color_Name           : Name_Id; -- "magenta",            "#FF00FF"
     Maroon_Color_Name            : Name_Id; -- "maroon",             "#800000"
     Mediumaquamarine_Color_Name  : Name_Id; -- "mediumaquamarine",   "#66CDAA"
     Mediumblue_Color_Name        : Name_Id; -- "mediumblue",         "#0000CD"
     Mediumorchid_Color_Name      : Name_Id; -- "mediumorchid",       "#BA55D3"
     Mediumpurple_Color_Name      : Name_Id; -- "mediumpurple",       "#9370DB"
     Mediumseagreen_Color_Name    : Name_Id; -- "mediumseagreen",     "#3CB371"
     Mediumslateblue_Color_Name   : Name_Id; -- "mediumslateblue",    "#7B68EE"
     Mediumspringgreen_Color_Name : Name_Id; -- "mediumspringgreen",  "#00FA9A"
     Mediumturquoise_Color_Name   : Name_Id; -- "mediumturquoise",    "#48D1CC"
     Mediumvioletred_Color_Name   : Name_Id; -- "mediumvioletred",    "#C71585"
     Midnightblue_Color_Name      : Name_Id; -- "midnightblue",       "#191970"
     Mintcream_Color_Name         : Name_Id; -- "mintcream",          "#F5FFFA"
     Mistyrose_Color_Name         : Name_Id; -- "mistyrose",          "#FFE4E1"
     Moccasin_Color_Name          : Name_Id; -- "moccasin",           "#FFE4B5"
     Navajowhite_Color_Name       : Name_Id; -- "navajowhite",        "#FFDEAD"
     Navy_Color_Name              : Name_Id; -- "navy",               "#000080"
     Oldlace_Color_Name           : Name_Id; -- "oldlace",            "#FDF5E6"
     Olive_Color_Name             : Name_Id; -- "olive",              "#808000"
     Olivedrab_Color_Name         : Name_Id; -- "olivedrab",          "#6B8E23"
     Orange_Color_Name            : Name_Id; -- "orange",             "#FFA500"
     Orangered_Color_Name         : Name_Id; -- "orangered",          "#FF4500"
     Orchid_Color_Name            : Name_Id; -- "orchid",             "#DA70D6"
     Palegoldenrod_Color_Name     : Name_Id; -- "palegoldenrod",      "#EEE8AA"
     Palegreen_Color_Name         : Name_Id; -- "palegreen",          "#98FB98"
     Paleturquoise_Color_Name     : Name_Id; -- "paleturquoise",      "#AFEEEE"
     Palevioletred_Color_Name     : Name_Id; -- "palevioletred",      "#DB7093"
     Papayawhip_Color_Name        : Name_Id; -- "papayawhip",         "#FFEFD5"
     Peachpuff_Color_Name         : Name_Id; -- "peachpuff",          "#FFDAB9"
     Peru_Color_Name              : Name_Id; -- "peru",               "#CD853F"
     Pink_Color_Name              : Name_Id; -- "pink",               "#FFC0CB"
     Plum_Color_Name              : Name_Id; -- "plum",               "#DDA0DD"
     Powderblue_Color_Name        : Name_Id; -- "powderblue",         "#B0E0E6"
     Purple_Color_Name            : Name_Id; -- "purple",             "#800080"
     Red_Color_Name               : Name_Id; -- "red",                "#FF0000"
     Rosybrown_Color_Name         : Name_Id; -- "rosybrown",          "#BC8F8F"
     Royalblue_Color_Name         : Name_Id; -- "royalblue",          "#4169E1"
     Saddlebrown_Color_Name       : Name_Id; -- "saddlebrown",        "#8B4513"
     Salmon_Color_Name            : Name_Id; -- "salmon",             "#FA8072"
     Sandybrown_Color_Name        : Name_Id; -- "sandybrown",         "#F4A460"
     Seagreen_Color_Name          : Name_Id; -- "seagreen",           "#2E8B57"
     Seashell_Color_Name          : Name_Id; -- "seashell",           "#FFF5EE"
     Sienna_Color_Name            : Name_Id; -- "sienna",             "#A0522D"
     Silver_Color_Name            : Name_Id; -- "silver",             "#C0C0C0"
     Skyblue_Color_Name           : Name_Id; -- "skyblue",            "#87CEEB"
     Slateblue_Color_Name         : Name_Id; -- "slateblue",          "#6A5ACD"
     Slategray_Color_Name         : Name_Id; -- "slategray",          "#708090"
     Slategrey_Color_Name         : Name_Id; -- "slategrey",          "#708090"
     Snow_Color_Name              : Name_Id; -- "snow",               "#FFFAFA"
     Springgreen_Color_Name       : Name_Id; -- "springgreen",        "#00FF7F"
     Steelblue_Color_Name         : Name_Id; -- "steelblue",          "#4682B4"
     Tan_Color_Name               : Name_Id; -- "tan",                "#D2B48C"
     Teal_Color_Name              : Name_Id; -- "teal",               "#008080"
     Thistle_Color_Name           : Name_Id; -- "thistle",            "#D8BFD8"
     Tomato_Color_Name            : Name_Id; -- "tomato",             "#FF6347"
     Turquoise_Color_Name         : Name_Id; -- "turquoise",          "#40E0D0"
     Violet_Color_Name            : Name_Id; -- "violet",             "#EE82EE"
     Wheat_Color_Name             : Name_Id; -- "wheat",              "#F5DEB3"
     White_Color_Name             : Name_Id; -- "white",              "#FFFFFF"
     Whitesmoke_Color_Name        : Name_Id; -- "whitesmoke",         "#F5F5F5"
     Yellow_Color_Name            : Name_Id; -- "yellow",             "#FFFF00"
     Yellowgreen_Color_Name       : Name_Id; -- "yellowgreen",        "#9ACD32"
     
     -- Dom attributtes
     
     Name_Graph             : Name_Id;
     Name_Model             : Name_Id;
     Name_View              : Name_Id;
     Name_Cell              : Name_Id;
     Name_Root              : Name_Id;
     Name_Vertex            : Name_Id;
     Name_Edge              : Name_Id;
     Name_Geometry          : Name_Id;
     Name_Cell_Geometry     : Name_Id;
     Name_Edge_Geometry     : Name_Id;
     Name_Xml_Id            : Name_Id;
     Name_Source            : Name_Id;
     Name_Target            : Name_Id;
     Name_Type              : Name_Id;
     Name_Source_Point      : Name_Id;
     Name_Target_Point      : Name_Id;
     Name_Offset_Point      : Name_Id;
     
     Name_Edges_List        : Name_Id;
     Name_Edge_Ref          : Name_Id;
     
     Name_Layer             : Name_Id;
     
     Name_Rectangle         : Name_Id;
     Name_Path              : Name_Id;
     Name_X                 : Name_Id;
     Name_Y                 : Name_Id;
     Name_Width             : Name_Id;
     Name_Height            : Name_Id;
     Name_Relative          : Name_Id;
     Name_Translate_Control : Name_Id;
     
     Name_Cell_State        : Name_Id;
     Name_Label             : Name_Id;
     Name_Invalid           : Name_Id;
     Name_Terminal_Distance : Name_Id;
     Name_Length            : Name_Id;
     Name_Origin_Point      : Name_Id;
     Name_Absolute_Points   : Name_Id;
     Name_Absolute_Offset   : Name_Id;
     Name_Segments          : Name_Id;
     Name_Label_Bounds      : Name_Id;
     Name_Bounding_Box      : Name_Id;
     Name_Visible_Terminals : Name_Id;
     
     Name_Comment       : Name_Id;
     
     No_Attribute : constant Name_Id := No_Name;
   
   procedure Initialize;
      
end Artics.Graph.Names;
