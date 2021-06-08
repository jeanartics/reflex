------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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
-- Reflex is a fork from the GNAT compiler. GNAT was originally developed   --
-- by the GNAT team at  New York University. Extensive  contributions to    --
-- GNAT were provided by Ada Core Technologies Inc. Reflex is developed  by --
-- the Artics team at Grenoble.                                             --
--                                                                          --
------------------------------------------------------------------------------

package body Switch is

   -------------------------
   -- Is_Front_End_Switch --
   -------------------------

   function Is_Front_End_Switch (Switch_Chars : String) return Boolean is
      Ptr : constant Positive := Switch_Chars'First;

   begin
      return Is_Switch (Switch_Chars)
        and then
        (Switch_Chars (Ptr + 1) = 'I'
         or else (Switch_Chars'Length >= 3
                  and then Switch_Chars (Ptr + 1 .. Ptr + 2) = "rx")
         or else (Switch_Chars'Length >= 5
                  and then Switch_Chars (Ptr + 1 .. Ptr + 4) = "gnat")
         or else (Switch_Chars'Length >= 5
                  and then Switch_Chars (Ptr + 1 .. Ptr + 4) = "fRTS"));
   end Is_Front_End_Switch;

   ---------------
   -- Is_Switch --
   ---------------

   function Is_Switch (Switch_Chars : String) return Boolean is
   begin
      return Switch_Chars'Length > 1
        and then Switch_Chars (Switch_Chars'First) = '-';
   end Is_Switch;

   ------------------------
   --------------
   -- Scan_Nat --
   --------------

   procedure Scan_Nat
     (Switch_Chars : String;
      Max          : Integer;
      Ptr          : in out Integer;
      Result       : out Nat)
   is
   begin
      Result := 0;

      if Ptr > Max or else Switch_Chars (Ptr) not in '0' .. '9' then
         raise Missing_Switch_Value;
      end if;

      while Ptr <= Max and then Switch_Chars (Ptr) in '0' .. '9' loop
         Result := Result * 10 +
           Character'Pos (Switch_Chars (Ptr)) - Character'Pos ('0');
         Ptr := Ptr + 1;

         if Result > Switch_Max_Value then
            raise Bad_Switch_Value;
         end if;
      end loop;
   end Scan_Nat;

   --------------
   -- Scan_Pos --
   --------------

   procedure Scan_Pos
     (Switch_Chars : String;
      Max          : Integer;
      Ptr          : in out Integer;
      Result       : out Pos) is

      Temp : Nat;

   begin
      Scan_Nat (Switch_Chars, Max, Ptr, Temp);

      if Temp = 0 then
         raise Bad_Switch_Value;
      end if;

      Result := Temp;
   end Scan_Pos;

end Switch;
