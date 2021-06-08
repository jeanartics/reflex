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

package Reflex.Expanders.Ch8 is
   
   --------------------
   -- Homonym_Suffix --
   --------------------

   --  The string defined here (and its associated length) is used to gather
   --  the homonym string that will be appended to Name_Buffer when the name
   --  is complete. Strip_Suffixes appends to this string as does
   --  Append_Homonym_Number, and Output_Homonym_Numbers_Suffix appends the
   --  string to the end of Name_Buffer.

   Homonym_Numbers : String (1 .. 256);
   Homonym_Len     : Natural := 0;

   ---------------------------
   -- Append_Homonym_Number --
   ---------------------------
   
   procedure Append_Homonym_Number (E : Entity_Id) is
      
      procedure Add_Nat_To_H (Nr : Nat);
      --  Little procedure to append Nr to Homonym_Numbers
      
      ------------------
      -- Add_Nat_To_H --
      ------------------
      
      procedure Add_Nat_To_H (Nr : Nat) is
      begin
         if Nr >= 10 then
            Add_Nat_To_H (Nr / 10);
         end if;
	 
         Homonym_Len := Homonym_Len + 1;
         Homonym_Numbers (Homonym_Len) :=
           Character'Val (Nr mod 10 + Character'Pos ('0'));
      end Add_Nat_To_H;
      
      --  Start of processing for Append_Homonym_Number
      
   begin
      if Has_Homonym (E) then
         declare
            H  : Entity_Id := Homonym (E);
            Nr : Nat := 1;
         begin
	    Nr := 0;
            while Present (H) loop
               -- if Scope (H) = Scope (E) then
	       if Is_Library_Level (H) and then Is_Library_Level (E) then
                  Nr := Nr + 1;
               end if;
	       
               H := Homonym (H);
            end loop;
	    
            if Homonym_Len > 0 then
               Homonym_Len := Homonym_Len + 1;
               Homonym_Numbers (Homonym_Len) := '_';
            end if;

            Add_Nat_To_H (Nr);
         end;
      end if;
   end Append_Homonym_Number;
   
end Reflex.Expanders.Ch8;
