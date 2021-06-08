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

package Dummy is
   
   ---------------------------------------------
   type Edges_Style_Function_Record is access all Integer;
   
   --  type Font_Record is new Integer;
   --  type Font_Metrics_Record is new Integer;
   type Image_Observer is new Integer;
   type Buffered_Image is new Integer;
   type Buffered_Image_Record is tagged record
      D : Integer;
   end record;
   
   type Font_Record is tagged record
      D : Integer;
   end record;
   
   type Font_Metrics_Record  is tagged record
      D : Integer;
   end record;
   
   No_Buffered_Image : constant Buffered_Image := 0;
   
   type Document_Record is tagged record
     D : Integer;
   end record;
   
end Dummy;
