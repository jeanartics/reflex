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


package Artics.Java.Methods is
   
   pragma Convention_Identifier (JNICall, C);
   
   
   type Method_Descriptor_Record is record 
      Name : String_Ptr;
      -- Java Method Name
      
      Sig : String_Ptr;
      -- Metofe_signature
      
      -- Type of method (Static, Virtual..)
   end record;
   
   No_Method_Descriptor_Record : Method_Descriptor_Record :=
     Method_Descriptor_Record'(Name => null, Sig => null);
   
end Artics.Java.Methods;

