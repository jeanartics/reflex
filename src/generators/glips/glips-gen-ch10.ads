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

package Glips.Gen.Ch10 is

   procedure Generate_Compilation_Unit 
     (This : access Glips_Generator_Record;
      Node : Node_Id);

   procedure Generate_Compilation_Unit_Aux 
     (This : access Glips_Generator_Record;
      Node : Node_Id);

   procedure Generate_Use_Package_Clause 
     (This : access Glips_Generator_Record;
      Node : Node_Id);
   
   procedure Generate_Use_Type_Clause 
     (This : access Glips_Generator_Record;
      Node : Node_Id);

   procedure Generate_With_Clause 
     (This : access Glips_Generator_Record;
      Node : Node_Id);

end Glips.Gen.Ch10;
