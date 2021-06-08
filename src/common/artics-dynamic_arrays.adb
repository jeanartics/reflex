------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 2016, Free Software Foundation, Inc.              --
--                                                                          --
-- Reflex is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware Foundation; either version 3, or (at your option) any later version --
-- Reflex is distributed in the hope that it will be u, but WITH-      --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License distributed with Reflex; see file COPYING3. If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- Reflex is originally developed by Artics                                 --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;
with System.Address_To_Access_Conversions;
with Artics.Maths; use Artics.Maths;

package body Artics.Dynamic_Arrays is
   
   -----------------------
   -- New_Dynamic_Array --
   -----------------------
   
   function New_Dynamic_Array
     (Count : Element_Index) return Dynamic_Array_Ptr is
      Ar    : Dynamic_Array_Ptr := new Dynamic_Array (0 .. (Count - 1));
   begin
      Ar.all := (others => No_Element);
      return Ar;
   end New_Dynamic_Array;
   
   -----------------------
   -- New_Dynamic_Array --
   -----------------------
   
   function New_Dynamic_Array_From_Copy
     (Array_Ptr : Dynamic_Array_Ptr) return Dynamic_Array_Ptr is
      
      Ar    : Dynamic_Array_Ptr := null;
      Count : Element_Index;
   begin
      if Array_Ptr /= null then
	 
	 Count := Array_Ptr'Length;
	 Ar := New_Dynamic_Array (Count);
	 Ar.all := Array_Ptr.all;
      end if;
      
      return Ar;
   end New_Dynamic_Array_From_Copy;
   
   ----------------
   -- Free_Array --
   ----------------
   
   procedure Free_Array (Array_Ptr : in out Dynamic_Array_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation 
	(Dynamic_Array, Dynamic_Array_Ptr);
   begin
      Free (Array_Ptr);
   end Free_Array;
      
   ------------
   -- Resize --
   ------------
   
   procedure Resize
     (Array_Ptr : in out Dynamic_Array_Ptr;
      Count     : Element_Index) is
      
      Current_Count : Element_Index;
      Ptr           : Dynamic_Array_Ptr;
   begin
      Current_Count := Array_Ptr'Length;
      
      if Current_Count = Count then
	 return;
      else
	 Ptr := New_Dynamic_Array (Count);
	 
	 if Current_Count > Count then
	    Ptr.all (0..Count-1) := Array_Ptr (0..Count-1);
	 else
	    Ptr.all (0..Current_Count-1) := Array_Ptr (0..Current_Count-1);
	    Ptr.all (Current_Count..Count-1) := (others => No_Element);
	 end if;
      end if;
      
      Free_Array (Array_Ptr);
      Array_Ptr := Ptr;
   end Resize;
   
   -----------------
   -- Resize_Init --
   -----------------
   
   procedure Resize_Init
     (Array_Ptr : in out Dynamic_Array_Ptr;
      Count     : Element_Index) is
      
      Ptr : Dynamic_Array_Ptr;
   begin
      Ptr := New_Dynamic_Array (Count);
      Ptr.all := (others => No_Element);
      
      Free_Array (Array_Ptr);
      Array_Ptr := Ptr;
   end Resize_Init;
   
   ----------
   -- Copy --
   ----------
   
   procedure Copy
     (Src        : Dynamic_Array_Ptr;
      Src_Start  : Element_Index;
      Dest       : Dynamic_Array_Ptr;
      Dest_Start : Element_Index;
      Count      : Element_Index) is
      
      Src_Last   : Element_Index;
      Dest_Last  : Element_Index;
      Ncount     : Element_Index;
      Src_Count  : Element_Index;
      Dest_Count : Element_Index;
   begin
      Src_Last := Src_Start + Count;
      if Src_Last > Src'Length then
	 Src_Count := Src'Length - Src_Start;
      else
	 Src_Count := Count;
      end if;
      
      Dest_Last := Dest_Start + Count;
      if Dest_Last > Dest'Length then
	 Dest_Count := Dest'Length - Dest_Start; 
      else
	 Dest_Count := Count;
      end if;
      
      Ncount    := Maths.Min (Src_Count, Dest_Count);
      Src_Last  := Src_Start  + Ncount - 1;
      Dest_Last := Dest_Start + Ncount - 1;
      
      Dest (Dest_Start..Dest_Last) := Src (Src_Start..Src_Last);
   end Copy;
   
   ----------
   -- Copy --
   ----------
   
   function Clone_Array (Src : Dynamic_Array_Ptr) return Dynamic_Array_Ptr is
      
      Count : Element_Index := Src'Length;
      Dest  : Dynamic_Array_Ptr;
   begin
      Dest := New_Dynamic_Array (Count);
      Dest.all := Src.all;
      
      return Dest;
   end Clone_Array;
   
end Artics.Dynamic_Arrays;
