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

with Types; use Types;

-- Back_End_Itypes_Support

--  This package provides support to the back end to define extra itypes
--  not available in the tree. Currently it is used to generate an extra
--  itype associated with subprogram formals whose type is an access to
--  an unconstrained multidimensional array type (for unidimensional array
--  types this extra itype is not needed because the formal is defined as
--  a pointer to the component type).

package Reflex.Expanders.Back_End_Itypes is
   
   procedure Declare_Back_End_Itypes (Subp_Id : Entity_Id);
   --  Declare back-end itypes associated with the formals of a subprogram
   --  whose type is an access to a multidimensional unconstrained array

   function Has_Back_End_AREC_Itype (E : Entity_Id) return Boolean;
   --  Return True if E has an extra back-end AREC itype

   function Has_Back_End_Itype (E : Entity_Id) return Boolean;
   --  Return True if E has an extra back-end itype

   procedure Write_Back_End_Itype_Id (E : Entity_Id);
   --  Output the identifier of the back-end itype of E
   
end Reflex.Expanders.Back_End_Itypes;
