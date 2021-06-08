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

package Reflex.Expanders.Ch7 is

   --   ??? procedure Expand_Defining_Program_Unit_Name 
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);

   --  procedure Expand_Designator 
   --    (This : access Reflex_Expander_Record;
   --     Node : Node_Id);
    
   procedure Expand_Package_Body 
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Package_Declaration 
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

   procedure Expand_Package_Specification 
     (This : access Reflex_Expander_Record;
      Node : Node_Id);

end Reflex.Expanders.Ch7;
