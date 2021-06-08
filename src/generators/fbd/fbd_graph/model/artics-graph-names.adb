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

package body Artics.Graph.Names is
   
   ----------------
   -- Initialize --
   ----------------
   
   procedure Initialize is
   begin
      
      -- Enter Tokens names
      
      Event_Done_Name                     := String_Find("done");
      Event_Add_Cells_Name                := String_Find("addCells");
      Event_Cells_Added_Name              := String_Find("cellsAdded");
      Event_Align_Cells_Name              := String_Find("alignCells");
      Event_Connect_Cell_Name             := String_Find("connectCell");
      Event_Connect_Name                  := String_Find("connect");
      Event_Cell_Connected_Name           := String_Find("cellConnected");
      Event_Flip_Edge_Name                := String_Find("flipEdge");
      Event_Fold_Cells_Name               := String_Find("foldCells");
      Event_Cells_Folded_Name             := String_Find("cellsFolded");
      Event_Group_Cells_Name              := String_Find("groupCells");
      Event_Ungroup_Cells_Name            := String_Find("ungroupCells");
      Event_Remove_Cells_From_Parent_Name := String_Find("removeCellsFromParent");
      Event_Move_Cells_Name               := String_Find("moveCells");
      Event_Cells_Moved_Name              := String_Find("cellsMoved");
      Event_Order_Cells_Name              := String_Find("orderCells");
      Event_Cells_Ordered_Name            := String_Find("cellsOrdered");
      Event_Remove_Cells_Name             := String_Find("removeCells");
      Event_Cells_Removed_Name            := String_Find("cellsRemoved");
      Event_Repaint_Name                  := String_Find("repaint");
      Event_Resize_Cells_Name             := String_Find("resizeCells");
      Event_Cells_Resized_Name            := String_Find("cellsResized");
      Event_Split_Edge_Name               := String_Find("splitEdge");
      Event_Toggle_Cells_Name             := String_Find("toggleCells");
      Event_Cells_Toggles_Name            := String_Find("cellsToggled");
      Event_Update_Cell_Size_Name         := String_Find("updateCellSize");
      Event_Label_Changed_Name            := String_Find("labelChanged");
      Event_Add_Overlay_Name              := String_Find("addOverlay");
      Event_Remove_Overlay_Name           := String_Find("removeOverlay");
      Event_Before_Paint_Name             := String_Find("beforePaint");
      Event_Paint_Name                    := String_Find("paint");
      Event_After_Paint_Name              := String_Find("afterPaint");
      Event_Start_Editing_Name            := String_Find("startEditing");
      Event_Undo_Name                     := String_Find("undo");
      Event_Redo_Name                     := String_Find("redo");
      Event_Up_Name                       := String_Find("up");
      Event_Down_Name                     := String_Find("down");
      Event_Scale_Name                    := String_Find("scale");
      Event_Translate_Name                := String_Find("translate");
      Event_Scale_And_Translate_Name      := String_Find("scaleAndTranslate");
      Event_Change_Name                   := String_Find("change");
      Event_Execute_Name                  := String_Find("execute");
      Event_Before_Undo_Name              := String_Find("beforeUndo");
      Event_Notify_Name                   := String_Find("notify");
      Event_Begin_Update_Name             := String_Find("beginUpdate");
      Event_End_Update_Name               := String_Find("endUpdate");
      Event_Insert_Name                   := String_Find("insert");
      Event_Add_Name                      := String_Find("add");
      Event_Clear_Name                    := String_Find("clear");
      Event_Fired_Name                    := String_Find("fired");
      Event_Select_Name                   := String_Find("select");
      Event_Mark_Name                     := String_Find("mark");
      Event_Root_Name                     := String_Find("root");
      Event_Layout_Cells_Name             := String_Find("layoutCells");
      Event_Start_Name                    := String_Find("start");
      Event_Continue_Name                 := String_Find("continue");
      Event_Stop_Name                     := String_Find("stop");
      
      
      NS_SVG                         := String_Find("http://www.w3.org/2000/svg");
      NS_XHTML                       := String_Find("http://www.w3.org/1999/xhtml");
      NS_XLINK                       := String_Find("http://www.w3.org/1999/xlink");
      DEFAULT_Font_Graphic           := String_Find("Segoe UI");
      DEFAULT_FONTFAMILIES           := String_Find("Arial");
      DEFAULT_FONTFAMILY             := String_Find("dialog");
      STENCIL_SHADOWCOLOR            := String_Find("gray");
      W3C_SHADOWCOLOR                := String_Find("gray");
      SVG_SHADOWTRANSFORM            := String_Find("translate(2 3)");
      NONE                           := String_Find("none");
      STYLE_PERIMETER                := String_Find("perimeter");
      
      STYLE_SOURCE_PORT              := String_Find("sourcePort");
      STYLE_TARGET_PORT              := String_Find("targetPort");
      STYLE_PORT_CONSTRAINT          := String_Find("portConstraint");
      STYLE_OPACITY                  := String_Find("opacity");
      STYLE_TEXT_OPACITY             := String_Find("textOpacity");
      STYLE_OVERFLOW                 := String_Find("overflow");
      STYLE_ORTHOGONAL               := String_Find("orthogonal");
      STYLE_EXIT_X                   := String_Find("exitX");
      STYLE_EXIT_Y                   := String_Find("exitY");
      STYLE_EXIT_PERIMETER           := String_Find("exitPerimeter");
      STYLE_ENTRY_X                  := String_Find("entryX");
      STYLE_ENTRY_Y                  := String_Find("entryY");
      STYLE_ENTRY_PERIMETER          := String_Find("entryPerimeter");
      STYLE_WHITE_SPACE              := String_Find("whiteSpace");
      STYLE_ROTATION                 := String_Find("rotation");
      STYLE_SWIMLANE_FILLCOLOR       := String_Find("swimlaneFillColor");
      STYLE_FILLCOLOR                := String_Find("fillColor");
      STYLE_GRADIENTCOLOR            := String_Find("gradientColor");
      STYLE_GRADIENT_DIRECTION       := String_Find("gradientDirection");
      STYLE_STROKECOLOR              := String_Find("strokeColor");
      STYLE_SEPARATORCOLOR           := String_Find("separatorColor");
      STYLE_STROKEWIDTH              := String_Find("strokeWidth");
      STYLE_ALIGN                    := String_Find("align");
      STYLE_VERTICAL_ALIGN           := String_Find("verticalAlign");
      STYLE_LABEL_POSITION           := String_Find("labelPosition");
      STYLE_VERTICAL_LABEL_POSITION  := String_Find("verticalLabelPosition");
      STYLE_IMAGE_ALIGN              := String_Find("imageAlign");
      STYLE_IMAGE_VERTICAL_ALIGN     := String_Find("imageVerticalAlign");
      STYLE_GLASS                    := String_Find("glass");
      STYLE_IMAGE                    := String_Find("image");
      STYLE_IMAGE_WIDTH              := String_Find("imageWidth");
      STYLE_IMAGE_HEIGHT             := String_Find("imageHeight");
      STYLE_IMAGE_BACKGROUND         := String_Find("imageBackground");
      STYLE_IMAGE_BORDER             := String_Find("imageBorder");
      STYLE_IMAGE_FLIPH              := String_Find("imageFlipH");
      STYLE_IMAGE_FLIPV              := String_Find("imageFlipV");
      STYLE_STENCIL_FLIPH            := String_Find("stencilFlipH");
      STYLE_STENCIL_FLIPV            := String_Find("stencilFlipV");
      STYLE_FLIPH                    := String_Find("flipH");
      STYLE_FLIPV                    := String_Find("flipV");
      STYLE_NOLABEL                  := String_Find("noLabel");
      STYLE_NOEDGESTYLE              := String_Find("noEdgeStyle");
      STYLE_LABEL_BACKGROUNDCOLOR    := String_Find("labelBackgroundColor");
      STYLE_LABEL_BORDERCOLOR        := String_Find("labelBorderColor");
      STYLE_INDICATOR_SHAPE          := String_Find("indicatorShape");
      STYLE_INDICATOR_IMAGE          := String_Find("indicatorImage");
      STYLE_INDICATOR_COLOR          := String_Find("indicatorColor");
      STYLE_INDICATOR_GRADIENTCOLOR  := String_Find("indicatorGradientColor");
      STYLE_INDICATOR_SPACING        := String_Find("indicatorSpacing");
      STYLE_INDICATOR_WIDTH          := String_Find("indicatorWidth");
      STYLE_INDICATOR_HEIGHT         := String_Find("indicatorHeight");
      STYLE_SHADOW                   := String_Find("shadow");
      STYLE_SEGMENT                  := String_Find("segment");
      STYLE_ENDARROW                 := String_Find("endArrow");
      STYLE_STARTARROW               := String_Find("startArrow");
      STYLE_ENDSIZE                  := String_Find("endSize");
      STYLE_STARTSIZE                := String_Find("startSize");
      STYLE_SWIMLANE_LINE            := String_Find("swimlaneLine");
      STYLE_ENDFILL                  := String_Find("endFill");
      STYLE_STARTFILL                := String_Find("startFill");
      STYLE_DASHED                   := String_Find("dashed");
      STYLE_DASH_PATTERN             := String_Find("dashPattern");
      STYLE_ROUNDED                  := String_Find("rounded");
      STYLE_ARCSIZE                  := String_Find("arcSize");
      STYLE_SOURCE_PERIMETER_SPACING := String_Find("sourcePerimeterSpacing");
      STYLE_TARGET_PERIMETER_SPACING := String_Find("targetPerimeterSpacing");
      STYLE_PERIMETER_SPACING        := String_Find("perimeterSpacing");
      STYLE_SPACING                  := String_Find("spacing");
      STYLE_SPACING_TOP              := String_Find("spacingTop");
      STYLE_SPACING_LEFT             := String_Find("spacingLeft");
      STYLE_SPACING_BOTTOM           := String_Find("spacingBottom");
      STYLE_SPACING_RIGHT            := String_Find("spacingRight");
      STYLE_HORIZONTAL               := String_Find("horizontal");
      STYLE_DIRECTION                := String_Find("direction");
      STYLE_ELBOW                    := String_Find("elbow");
      STYLE_FONTCOLOR                := String_Find("fontColor");
      STYLE_FONTFAMILY               := String_Find("fontFamily");
      STYLE_FONTSIZE                 := String_Find("fontSize");
      STYLE_FONTSTYLE                := String_Find("fontStyle");
      STYLE_AUTOSIZE                 := String_Find("autosize");
      STYLE_FOLDABLE                 := String_Find("foldable");
      STYLE_EDITABLE                 := String_Find("editable");
      STYLE_BENDABLE                 := String_Find("bendable");
      STYLE_MOVABLE                  := String_Find("movable");
      STYLE_RESIZABLE                := String_Find("resizable");
      STYLE_CLONEABLE                := String_Find("cloneable");
      STYLE_DELETABLE                := String_Find("deletable");
      STYLE_SHAPE                    := String_Find("shape");
      STYLE_EDGE                     := String_Find("edgeStyle");
      STYLE_LOOP                     := String_Find("loopStyle");
      STYLE_ROUTING_CENTER_X         := String_Find("routingCenterX");
      STYLE_ROUTING_CENTER_Y         := String_Find("routingCenterY");
      
      SHAPE_RECTANGLE                := String_Find("rectangle");
      SHAPE_ELLIPSE                  := String_Find("ellipse");
      SHAPE_DOUBLE_RECTANGLE         := String_Find("doubleRectangle");
      SHAPE_DOUBLE_ELLIPSE           := String_Find("doubleEllipse");
      SHAPE_RHOMBUS                  := String_Find("rhombus");
      SHAPE_LINE                     := String_Find("line");
      SHAPE_IMAGE                    := String_Find("image");
      SHAPE_ARROW                    := String_Find("arrow");
      SHAPE_CURVE                    := String_Find("curve");
      SHAPE_LABEL                    := String_Find("label");
      SHAPE_CYLINDER                 := String_Find("cylinder");
      SHAPE_SWIMLANE                 := String_Find("swimlane");
      SHAPE_CONNECTOR                := String_Find("connector");
      SHAPE_ACTOR                    := String_Find("actor");
      SHAPE_CLOUD                    := String_Find("cloud");
      SHAPE_TRIANGLE                 := String_Find("triangle");
      SHAPE_HEXAGON                  := String_Find("hexagon");
      
      ARROW_CLASSIC                  := String_Find("classic");
      ARROW_BLOCK                    := String_Find("block");
      ARROW_OPEN                     := String_Find("open");
      ARROW_OVAL                     := String_Find("oval");
      ARROW_DIAMOND                  := String_Find("diamond");
      
      ALIGN_LEFT                     := String_Find("left");
      ALIGN_CENTER                   := String_Find("center");
      ALIGN_RIGHT                    := String_Find("right");
      ALIGN_TOP                      := String_Find("top");
      ALIGN_MIDDLE                   := String_Find("middle");
      ALIGN_BOTTOM                   := String_Find("bottom");
      
      DIRECTION_NORTH                := String_Find("north");
      DIRECTION_SOUTH                := String_Find("south");
      DIRECTION_EAST                 := String_Find("east");
      DIRECTION_WEST                 := String_Find("west");
      
      ELBOW_VERTICAL                 := String_Find("vertical");
      ELBOW_HORIZONTAL               := String_Find("horizontal");
      
      EDGESTYLE_ELBOW                := String_Find("elbowEdgeStyle");
      EDGESTYLE_ENTITY_RELATION      := String_Find("entityRelationEdgeStyle");
      EDGESTYLE_LOOP                 := String_Find("loopEdgeStyle");
      EDGESTYLE_SIDETOSIDE           := String_Find("sideToSideEdgeStyle");
      EDGESTYLE_TOPTOBOTTOM          := String_Find("topToBottomEdgeStyle");
      EDGESTYLE_ORTHOGONAL           := String_Find("orthogonalEdgeStyle");
      EDGESTYLE_SEGMENT              := String_Find("segmentEdgeStyle");
      
      PERIMETER_ELLIPSE              := String_Find("ellipsePerimeter");
      PERIMETER_RECTANGLE            := String_Find("rectanglePerimeter");
      PERIMETER_RHOMBUS              := String_Find("rhombusPerimeter");
      PERIMETER_TRIANGLE             := String_Find("trianglePerimeter");
      PERIMETER_HEXAGON              := String_Find("hexagonPerimeter");
      
      Default_Vertex_Style_Name      := String_Find("defaultVertexStyle");
      Default_Edge_Style_Name        := String_Find("defaultEdegeStyle");
      
      Elbow_Connector_Function_Name     := String_Find ("Elbow_Connector");
      Entity_Relation_Connector_Function_Name := String_Find ("Entity_Relation");
      Loop_Connector_Function_Name      := String_Find ("Loop_Connector");
      Side_To_Side_Connector_Function_Name   := String_Find ("Side_To_Side");
      Top_To_Bottom_Connector_Function_Name  := String_Find ("Top_To_Bottom");
      Orth_Connector_Function_Name      := String_Find ("Orth_Connector");
      Segment_Connector_Function_Name   := String_Find ("Segment_Connector");
      Ellipse_Perimeter_Function_Name   := String_Find ("Ellipse_Perimeter");
      Rectangle_Perimeter_Function_Name := String_Find ("Rectangle_Perimeter");
      Rhombus_Perimeter_Function_Name   := String_Find ("Rhombus_Perimeter");
      Triangle_Perimeter_Function_Name  := String_Find ("Triangle_Perimeter");
      Hexagon_Perimeter_Function_Name   := String_Find ("Hexagon_Perimeter");
      
      Name_True                      := String_Find("true");
      Name_False                     := String_Find("false");
     
      Name_One                       := String_Find ("1");
      Name_Zero                      := String_Find ("0");
      
      

     
      
      -- Colors Names --
      ------------------
      
      AliceBlue_Color_Name         := String_Find("aliceblue"); --     "#F0F8FF"
      AntiqueWhite_Color_Name      := String_Find("antiquewhite"); --  "#FAEBD7"
      Aqua_Color_Name              := String_Find("aqua"); --          "#00FFFF"
      AquaMarine_Color_Name        := String_Find("aquamarine"); --    "#7FFFD4"
      Azure_Color_Name             := String_Find("azure"); --         "#F0FFFF"
      Beige_Coor              := String_Find("beige"); --         "#F5F5DC"
      Bisque_Color_Name            := String_Find("bisque"); --        "#FFE4C4"
      Black_Color_Name             := String_Find("black"); --         "#000000"
      BlancheDalmond_Color_Name    := String_Find("blanchedalmond"); --"#FFEBCD"
      Blue_Color_Name              := String_Find("blue"); --          "#0000FF"
      BlueViolet_Color_Name        := String_Find("blueviolet"); --    "#8A2BE2"
      Brown_Color_Name             := String_Find("brown"); --         "#A52A2A"
      BurlyWood_Color_Name         := String_Find("burlywood"); --     "#DEB887"
      CadetBlue_Color_Name         := String_Find("cadetblue"); --     "#5F9EA0"
      Chartreuse_Color_Name        := String_Find("chartreuse"); --    "#7FFF00"
      Chocolate_Color_Name         := String_Find("chocolate"); --     "#D2691E"
      Coral_Color_Name             := String_Find("coral"); --         "#FF7F50"
      CornFlowerBlue_Color_Name    := String_Find("cornflowerblue"); --"#6495ED"
      Cornsilk_Color_Name          := String_Find("cornsilk"); --      "#FFF8DC"
      Crimson_Color_Name           := String_Find("crimson"); --       "#DC143C"
      Cyan_Color_Name              := String_Find("cyan"); --          "#00FFFF"
      Darkblue_Color_Name          := String_Find("darkblue"); --      "#00008B"
      Darkcyan_Color_Name          := String_Find("darkcyan"); --      "#008B8B"
      Darkgoldenrod_Color_Name     := String_Find("darkgoldenrod"); -- "#B8860B"
      Darkgray_Color_Name          := String_Find("darkgray"); --      "#A9A9A9"
      Darkgrey_Color_Name          := String_Find("darkgrey"); --      "#A9A9A9"
      Darkgreen_Color_Name         := String_Find("darkgreen"); --     "#006400"
      Darkkhaki_Color_Name         := String_Find("darkkhaki"); --     "#BDB76B"
      Darkmagenta_Color_Name       := String_Find("darkmagenta"); --   "#8B008B"
      Darkolivegreen_Color_Name    := String_Find("darkolivegreen"); --"#556B2F"
      Darkorange_Color_Name        := String_Find("darkorange"); --    "#FF8C00"
      Darkorchid_Color_Name        := String_Find("darkorchid"); --    "#9932CC"
      Darkred_Color_Name           := String_Find("darkred"); --       "#8B0000"
      Darksalmon_Color_Name        := String_Find("darksalmon"); --    "#E9967A"
      Darkseagreen_Color_Name      := String_Find("darkseagreen");  --  "#8FBC8F"
      Darkslateblue_Color_Name     := String_Find("darkslateblue"); -- "#483D8B"
      Darkslategray_Color_Name     := String_Find("darkslategray"); -- "#2F4F4F"
      Darkslategrey_Color_Name     := String_Find("darkslategrey"); -- "#2F4F4F"
      Darkturquoise_Color_Name     := String_Find("darkturquoise"); -- "#00CED1"
      Darkviolet_Color_Name        := String_Find("darkviolet");    -- "#9400D3"
      Deeppink_Color_Name          := String_Find("deeppink");      -- "#FF1493"
      Deepskyblue_Color_Name       := String_Find("deepskyblue");   -- "#00BFFF"
      Dimgray_Color_Name           := String_Find("dimgray");    --    "#696969"
      Dimgrey_Color_Name           := String_Find("dimgrey"); --       "#696969"
      Dodgerblue_Color_Name        := String_Find("dodgerblue"); --    "#1E90FF"
      Firebrick_Color_Name         := String_Find("firebrick"); --     "#B22222"
      Floralwhite_Color_Name       := String_Find("floralwhite"); --   "#FFFAF0"
      Forestgreen_Color_Name       := String_Find("forestgreen"); --   "#228B22"
      Fuchsia_Color_Name           := String_Find("fuchsia"); --       "#FF00FF"
      Gainsboro_Color_Name         := String_Find("gainsboro"); --     "#DCDCDC"
      Ghostwhite_Color_Name        := String_Find("ghostwhite"); --    "#F8F8FF"
      Gold_Color_Name              := String_Find("gold"); --          "#FFD700"
      Goldenrod_Color_Name         := String_Find("goldenrod"); --     "#DAA520"
      Gray_Color_Name              := String_Find("gray"); --          "#808080"
      Grey_Color_Name              := String_Find("grey"); --          "#808080"
      Green_Color_Name             := String_Find("green"); --         "#008000"
      Greenyellow_Color_Name       := String_Find("greenyellow"); --   "#ADFF2F"
      Honeydew_Color_Name          := String_Find("honeydew"); --      "#F0FFF0"
      Hotpink_Color_Name           := String_Find("hotpink"); --       "#FF69B4"
      Indianred_Color_Name         := String_Find("indianred"); --     "#CD5C5C"
      Indigo_Color_Name            := String_Find("indigo"); --        "#4B0082"
      Ivory_Color_Name             := String_Find("ivory"); --         "#FFFFF0"
      Khaki_Color_Name             := String_Find("khaki"); --         "#F0E68C"
      Lavender_Color_Name          := String_Find("lavender"); --      "#E6E6FA"
      Lavenderblush_Color_Name     := String_Find("lavenderblush"); -- "#FFF0F5"
      Lawngreen_Color_Name         := String_Find("lawngreen"); --     "#7CFC00"
      Lemonchiffon_Color_Name      := String_Find("lemonchiffon"); --  "#FFFACD"
      Lightblue_Color_Name         := String_Find("lightblue"); --     "#ADD8E6"
      Lightcoral_Color_Name        := String_Find("lightcoral"); --    "#F08080"
      Lightcyan_Color_Name         := String_Find("lightcyan"); --     "#E0FFFF"
      Lightgoldenrodyellow_Color_Name := String_Find("lightgoldenrodyellow");-- "#FAFAD2"
      Lightgray_Color_Name         := String_Find("lightgray"); --     "#D3D3D3"
      Lightgrey_Color_Name         := String_Find("lightgrey"); --     "#D3D3D3"
      Lightgreen_Color_Name        := String_Find("lightgreen"); --    "#90EE90"
      Lightpink_Color_Name         := String_Find("lightpink"); --     "#FFB6C1"
      Lightsalmon_Color_Name       := String_Find("lightsalmon"); --   "#FFA07A"
      Lightseagreen_Color_Name     := String_Find("lightseagreen"); -- "#20B2AA"
      Lightskyblue_Color_Name      := String_Find("lightskyblue"); --  "#87CEFA"
      Lightslategray_Color_Name    := String_Find("lightslategray"); --"#778899"
      Lightslategrey_Color_Name    := String_Find("lightslategrey"); --"#778899"
      Lightsteelblue_Color_Name    := String_Find("lightsteelblue"); --"#B0C4DE"
      Lightyellow_Color_Name       := String_Find("lightyellow"); --   "#FFFFE0"
      Lime_Color_Name              := String_Find("lime"); --          "#00FF00"
      Limegreen_Color_Name         := String_Find("limegreen"); --     "#32CD32"
      Linen_Color_Name             := String_Find("linen"); --         "#FAF0E6"
      Magenta_Color_Name           := String_Find("magenta"); --       "#FF00FF"
      Maroon_Color_Name            := String_Find("maroon"); --        "#800000"
      Mediumaquamarine_Color_Name  := String_Find("mediumaquamarine"); -- "#66CDAA"
      Mediumblue_Color_Name        := String_Find("mediumblue"); --    "#0000CD"
      Mediumorchid_Color_Name      := String_Find("mediumorchid"); --  "#BA55D3"
      Mediumpurple_Color_Name      := String_Find("mediumpurple"); --  "#9370DB"
      Mediumseagreen_Color_Name    := String_Find("mediumseagreen"); --"#3CB371"
      Mediumslateblue_Color_Name   := String_Find("mediumslateblue"); --  "#7B68EE"
      Mediumspringgreen_Color_Name := String_Find("mediumspringgreen"); --"#00FA9A"
      Mediumturquoise_Color_Name   := String_Find("mediumturquoise"); --  "#48D1CC"
      Mediumvioletred_Color_Name   := String_Find("mediumvioletred"); --  "#C71585"
      Midnightblue_Color_Name      := String_Find("midnightblue"); --  "#191970"
      Mintcream_Color_Name         := String_Find("mintcream"); --     "#F5FFFA"
      Mistyrose_Color_Name         := String_Find("mistyrose"); --     "#FFE4E1"
      Moccasin_Color_Name          := String_Find("moccasin"); --      "#FFE4B5"
      Navajowhite_Color_Name       := String_Find("navajowhite"); --   "#FFDEAD"
      Navy_Color_Name              := String_Find("navy"); --          "#000080"
      Oldlace_Color_Name           := String_Find("oldlace"); --       "#FDF5E6"
      Olive_Color_Name             := String_Find("olive"); --         "#808000"
      Olivedrab_Color_Name         := String_Find("olivedrab"); --     "#6B8E23"
      Orange_Color_Name            := String_Find("orange"); --        "#FFA500"
      Orangered_Color_Name         := String_Find("orangered"); --     "#FF4500"
      Orchid_Color_Name            := String_Find("orchid"); --        "#DA70D6"
      Palegoldenrod_Color_Name     := String_Find("palegoldenrod"); -- "#EEE8AA"
      Palegreen_Color_Name         := String_Find("palegreen"); --     "#98FB98"
      Paleturquoise_Color_Name     := String_Find("paleturquoise"); -- "#AFEEEE"
      Palevioletred_Color_Name     := String_Find("palevioletred"); -- "#DB7093"
      Papayawhip_Color_Name        := String_Find("papayawhip"); --    "#FFEFD5"
      Peachpuff_Color_Name         := String_Find("peachpuff"); --     "#FFDAB9"
      Peru_Color_Name              := String_Find("peru"); --          "#CD853F"
      Pink_Color_Name              := String_Find("pink"); --          "#FFC0CB"
      Plum_Color_Name              := String_Find("plum"); --          "#DDA0DD"
      Powderblue_Color_Name        := String_Find("powderblue"); --    "#B0E0E6"
      Purple_Color_Name            := String_Find("purple"); --        "#800080"
      Red_Color_Name               := String_Find("red"); --           "#FF0000"
      Rosybrown_Color_Name         := String_Find("rosybrown"); --     "#BC8F8F"
      Royalblue_Color_Name         := String_Find("royalblue"); --     "#4169E1"
      Saddlebrown_Color_Name       := String_Find("saddlebrown"); --   "#8B4513"
      Salmon_Color_Name            := String_Find("salmon"); --        "#FA8072"
      Sandybrown_Color_Name        := String_Find("sandybrown"); --    "#F4A460"
      Seagreen_Color_Name          := String_Find("seagreen"); --      "#2E8B57"
      Seashell_Color_Name          := String_Find("seashell"); --      "#FFF5EE"
      Sienna_Color_Name            := String_Find("sienna"); --        "#A0522D"
      Silver_Color_Name            := String_Find("silver"); --        "#C0C0C0"
      Skyblue_Color_Name           := String_Find("skyblue"); --       "#87CEEB"
      Slateblue_Color_Name         := String_Find("slateblue"); --     "#6A5ACD"
      Slategray_Color_Name         := String_Find("slategray"); --     "#708090"
      Slategrey_Color_Name         := String_Find("slategrey"); --     "#708090"
      Snow_Color_Name              := String_Find("snow"); --          "#FFFAFA"
      Springgreen_Color_Name       := String_Find("springgreen"); --   "#00FF7F"
      Steelblue_Color_Name         := String_Find("steelblue"); --     "#4682B4"
      Tan_Color_Name               := String_Find("tan"); --           "#D2B48C"
      Teal_Color_Name              := String_Find("teal"); --          "#008080"
      Thistle_Color_Name           := String_Find("thistle"); --       "#D8BFD8"
      Tomato_Color_Name            := String_Find("tomato"); --        "#FF6347"
      Turquoise_Color_Name         := String_Find("turquoise"); --     "#40E0D0"
      Violet_Color_Name            := String_Find("violet"); --        "#EE82EE"
      Wheat_Color_Name             := String_Find("wheat"); --         "#F5DEB3"
      White_Color_Name             := String_Find("white"); --         "#FFFFFF"
      Whitesmoke_Color_Name        := String_Find("whitesmoke"); --    "#F5F5F5"
      Yellow_Color_Name            := String_Find("yellow"); --        "#FFFF00"
      Yellowgreen_Color_Name       := String_Find("yellowgreen"); --   "#9ACD32"
     
      
     Name_Graph             := String_Find("graph");
     Name_Model             := String_Find("model");
     Name_View              := String_Find("view");
     Name_Root              := String_Find("root");
     Name_Cell              := String_Find("cell");
     Name_Vertex            := String_Find("vertex");
     Name_Edge              := String_Find("edge");
     Name_Edges_List        := String_Find("edges-list");
     Name_Geometry          := String_Find("geometry");
     Name_Cell_Geometry     := String_Find("cell-geometry");
     Name_Edge_Geometry     := String_Find("edge-geometry");
     
     Name_Edge_Ref          := String_Find("edge-ref");
     
     Name_Xml_Id            := String_Find("id");
     Name_Source            := String_Find("source");
     Name_Target            := String_Find("target");
     Name_Type              := String_Find("type");
     
     Name_Source_Point      := String_Find("source-point");
     Name_Target_Point      := String_Find("target-point");
     Name_Offset_Point      := String_Find("offset-point");
     
     Name_Layer             := String_Find("layer");
     Name_Rectangle         := String_Find("rect");
     Name_Path              := String_Find("path");
     
     Name_X                 := String_Find("x");
     Name_Y                 := String_Find("y");
     Name_Width             := String_Find("width");
     Name_Height            := String_Find("height");
     Name_Relative          := String_Find("relative");
     Name_Translate_Control := String_Find("translate-control");
     
     Name_Cell_State        := String_Find("cell-state");
     Name_Label             := String_Find("label");
     Name_Invalid           := String_Find("invalid");
     Name_Terminal_Distance := String_Find("terminal-distance");
     Name_Length            := String_Find("length");
     Name_Origin_Point      := String_Find("origin-point");
     Name_Absolute_Points   := String_Find("absolute_points");
     Name_Absolute_Offset   := String_Find("absolute-offset");
     Name_Segments          := String_Find("segments");
     Name_Label_Bounds      := String_Find("label-bounds");
     Name_Bounding_Box      := String_Find("bounding-box");
     Name_Visible_Terminals := String_Find("visible-terminals");
     
     
     Name_Comment           := String_Find("comment");
     
   end Initialize;   
      
end Artics.Graph.Names;
