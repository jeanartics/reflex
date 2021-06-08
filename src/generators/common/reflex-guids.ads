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

package Reflex.Guids is
   
   ---------------------
   -- GUID management --
   ---------------------
      
   -- Generate a new unique GUID
   function Get_One_GUID return String;
   
   -- Generate a GUID relative to the last unique Generated GUID
   -- Allows to create as second GUID dependant on the first.
   -- If Imposed_Nbr is not < 0, then Generate a GUID using it.
   -- Caution : Cannot generate more than 9999 different GUIDs

   -- Used to generate the folders GUIDs
   function Get_Current_Folder_GUID(Imposed_Nbr : Integer := -1)
    return String;
   
   -- Associate and/or retrieve an associated GUID to/from a
   -- Node (used to retrieve GUIDs)
   function Get_Package_GUID(Node_Pack : Node_Id)
    return String;
    
   -- Emit the pending content on buffer and reset it
   procedure Emit_And_Reset_Buffer(Cc : Codesys_Content);
   
   
   
end Reflex.Guids;
