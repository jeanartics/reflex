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

with Reflex.Fbd_Dispatch;
with Reflex.Fbd_Gen_Ch2;
with Reflex.Fbd_Gen_Ch4;
with Reflex.Fbd_Gen_Ch5;
with Reflex.Fbd_Gen_Ch6;
with Reflex.Fbd_Builders;
with Reflex.Vertex_Value;
with Reflex.Edge_Value;
with Reflex.Fbd_Util;
with Reflex.Fbd_Dispatch_Emitor;
with Reflex.Fbd_Emitor;
with Reflex.Fbd_Builders; use Reflex.Fbd_Builders;
with Reflex.Fbd_Placements;
with Fbd_Driver1; use Fbd_Driver1;
procedure Fbd is
begin
   Init;
   Run;
end Fbd;
