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

package body Artics.Data_Types is
   
   --------------------
   -- Type_To_String --
   --------------------
   
   function Type_To_String (T : Data_Type) return String is
   begin
      case T is
	 when Unknown_Type =>
	    return "";
	 when Type_Boolean =>
	    return "boolean";
	 when Type_Integer =>
	    return "integer";
	 when Type_Float =>
	    return "float";
	 when Type_Duration =>
	    return "duration";
	 when Type_Reference =>
	    return "alias";
	 when Type_String =>
	    return "string";
      end case;
   end Type_To_String;
   
end Artics.Data_Types;

