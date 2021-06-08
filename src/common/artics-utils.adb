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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.String_Split; use GNAT.String_Split;
with GNAT.Case_Util; use GNAT.Case_Util;

with Ada.Text_IO;
package body Artics.Utils is
   procedure Put_Line (S : String) is null; -- renames Ada.Text_IO.Put_Line;
   
   ---------------------------
   -- String_Equivalent_Key --
   ---------------------------
   
   function Equivalent_Key (Left, Right : Name_Id) return Boolean is
   begin
      return Left = Right;
   end Equivalent_Key;
   
   ----------------------
   -- String_Hash_Func --
   ----------------------
   
   function Hash_Func(Key : Name_Id) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Key);
      -- return Ada.Strings.Hash (Key);
   end Hash_Func;
   
   ----------------------
   -- Copy_Strings_Map --
   ----------------------
   
   procedure Copy_Strings_Map
     (From : Strings_Maps.Map;
      To   : in out Strings_Maps.Map) is
      
      Cur : Strings_Maps.Cursor;
      K   : Name_Id;
      E   : Name_Id;
   begin
      if not Strings_Maps.Is_Empty (From) then
	 
	 Cur := Strings_Maps.First (From);
	 while Strings_Maps.Has_Element (Cur) loop
	    K := Strings_Maps.Key (Cur);
	    E := Strings_Maps.Element (Cur);
	    Strings_Maps.Insert (To, K, E);
	    
	    Strings_Maps.Next (Cur);
	 end loop;
      end if;
   end Copy_Strings_Map;
   
   ------------------------------
   -- Copy_Replace_Strings_Map --
   ------------------------------
   
   procedure Copy_Replace_Strings_Map
     (From : Strings_Maps.Map;
      To   : in out Strings_Maps.Map) is
      
      use Strings_Maps;
      
      Cur : Strings_Maps.Cursor;
      K   : Name_Id;
      E   : Name_Id;
   begin
      if not Strings_Maps.Is_Empty (From) then
	 
	 Cur := Strings_Maps.First (From);
	 while Strings_Maps.Has_Element (Cur) loop
	    K := Strings_Maps.Key (Cur);
	    E := Strings_Maps.Element (Cur);
	    
	    -- Insert the Key into the map To buit before remove the key K into
	    -- map To, il key K is present
	    
	    if To /= Strings_Maps.Empty_Map 
	      and then Strings_Maps.Find (To, K) /= Strings_Maps.No_Element 
	    then
	       Strings_Maps.Delete (To, K);
	    end if;
	    
	    Strings_Maps.Insert (To, K, E);
	    
	    Strings_Maps.Next (Cur);
	 end loop;
      end if;
   end Copy_Replace_Strings_Map;
   
   -----------
   -- Split --
   -----------

   function Split (Str : String; 
                   Sep : Character) return Strings_Lists.List is
      use Strings_Lists;
      L     : List;
      First : Natural := Str'First;
      Last  : Natural := Str'First;
   begin

      if Str = "" then
         Append (L, No_Name);
         return L;
      end if;

      if First = Str'Last then
         if Str (First) = Sep then
            Append (L, No_Name);
            return L;
         else
            Append (L, String_Find(Str));
            return L;
         end if;
      end if;

      while First < Str'Last loop
         First := Last;

         -- search 1st non-separator of a non-empty field
         while First <= Str'Last and then
           Last <= Str'Last and then
           Str (First) = Sep loop
            First := First + 1;
            Last := First;
         end loop;

         -- search last chr of a non-empty field
         if First <= Str'Last then

            -- stop field at separator
            while Last <= Str'Last and then
              Str (Last) /= Sep loop
               Last := Last + 1;
            end loop;

            declare
               Field : constant String := Str (First .. Last - 1);
               Name : constant Name_Id := String_Find (Field);
            begin
               Append (L, Name);
            end;
         end if;

      end loop;

      return L;
   end Split;
   
   ----------
   -- Join --
   ----------
   
   function Join (L   : Strings_Lists.List; 
                  Sep : Character) return String is
   begin
      return Join (L, "" & Sep);
   end Join;
   
   ----------
   -- Join --
   ----------
   
   function Join (L   : Strings_Lists.List; 
                  Sep : String := "") return String is
      use Strings_Lists;
      S     : access String := new String'("");
      First : Boolean;
   begin
      First := True;
      for Name of L loop
         if First then
            S := new String'(Get_String (Name));
         else
            S := new String'(S.all & Sep & Get_String (Name));
         end if;
         First := False;
      end loop;
      return S.all;
   end Join;
   
   -----------------
   -- Starts_With --
   -----------------
   
   function Starts_With 
     (N : Name_Id;
      S : String) return Boolean is
      
   begin
      return Starts_With (Get_String (N), S);
   end Starts_With;
   
   -----------------
   -- Starts_With --
   -----------------
   
   function Starts_With 
     (N : String;
      S : String) return Boolean 
   is
   begin
      if N'Length >= S'Length then
         return ( N(N'First.. N'First + S'Length - 1) = S);
      else
         return False;
      end if;
   end Starts_With;
   
   --------------
   -- Contains --
   --------------
   
   function Contains 
     (N : String;
      S : String) return Boolean
   is
   begin
      if S'Length > N'Length then
         return False;
      end if;
      for i in N'First .. N'Last - (S'Length - 1) loop
         declare
            Substring : String := N ( i .. i + (S'Length - 1));
         begin
            if Substring = S then
               return True;
            end if;
         end;
      end loop;
      
      return False;
   end Contains;
   
   --------------
   -- Index_Of --
   --------------
   
   function Index_Of 
     (N : Name_Id;
      S : String) return Natural is
      
      Sn : String := Get_String (N);
   begin
      return Index_Of (Get_String (N), S);
   end Index_Of;
   
   --------------
   -- Index_Of --
   --------------
   
   function Index_Of 
     (N : String;
      S : String) return Natural is
   begin
      return Ada.Strings.Fixed.Index (N, S);
   end Index_Of;
   
   -----------------
   -- Replace_All --
   -----------------
   
   function Replace_All
     (S       : String;
      Search  : String; 
      Replace : String) return String is      
      
      Len_Search : constant Natural := Search'Length;
      Pos : Natural;
   begin
      Pos := Index_Of (S, Search);
      if Pos > 0 then
         declare  
            Left : String := S (S'First .. Pos-1);
            Right : String := S (Pos + Len_Search .. S'Last);
         begin
            return Left & Replace & Replace_All (Right, Search, Replace);
         end;
      end if;
      return S;
   end Replace_All;
   
   -------------
   --To_Lower --
   -------------

   function To_Lower (S  : String) return String is
      W : String := S;
   begin
      Gnat.Case_Util.To_Lower (W);
      return W;
   end To_Lower;

   -----------------------
   -- Float_From_String --
   -----------------------
   
   function Float_From_String   (Value : String) return Float is
      Val : String := Value;
   begin
      Trim (Val, Both);
      return Float'Value (Val);
   end Float_From_String;
   
   function Float_From_Name (Value : Name_Id) return Float is
   begin
      return Float_From_String (Get_String (Value));
   end Float_From_Name;
   
   -------------------------
   -- Integer_From_String --
   -------------------------
   
   function Integer_From_String (Value : String) return Integer is
      
      Val : String := Value;
   begin
      Trim (Val, Both);
      
      if Value'Length > 2 and then 
        Starts_With (Val, "0x") then
         -- handle C style hexa notation
         declare
            V : constant String := "16#" & Val (Val'First + 3..Val'Last) & "#";
         begin
            return Integer'Value (V);
         end;
      end if;
      
      return Integer'Value (Val);
   end Integer_From_String;
   
   function Integer_From_Name (Value : Name_Id) return Integer is
   begin
      return Integer_From_String (Get_String (Value));
   end Integer_From_Name;
   
   -----------------------------
   -- Hex_Integer_From_String --
   -----------------------------
   
   function Hex_Integer_From_String (Value : String) return Integer is
      
      function From_Hex (C : Character) return Integer is
      begin
	 case C is
	    when '0' =>
	       return 0;
	    when '1' =>
	       return 1;
	    when '2' =>
	       return 2;
	    when '3' =>
	       return 3;
	    when '4' =>
	       return 4;
	    when '5' =>
	       return 5;
	    when '6' =>
	       return 6;
	    when '7' =>
	       return 7;
	    when '8' =>
	       return 8;
	    when '9' =>
	       return 9;
	    when 'a' | 'A' =>
	       return 10;
	    when 'b' | 'B' =>
	       return 11;
	    when 'c' | 'C' =>
	       return 12;
	    when 'd' | 'D' =>
	       return 13;
	    when 'e' | 'E' =>
	       return 14;
	    when 'f' | 'F' =>
	       return 15;
	    when others =>
	       raise Program_Error;
	 end case;
      end From_Hex;
      
      Val : String := Value;
      Res : Integer;
   begin
      Trim (Val, Both);
      
      Res := 0;
      for I in Val'Range loop
	 Res := Res * 16 + From_Hex (Val (I));
      end loop;
      
      return Res;
   end Hex_Integer_From_String;
   
   function Hex_Integer_From_Name (Value : Name_Id) return Integer is
   begin
      return Hex_Integer_From_String (Get_String (Value));
   end Hex_Integer_From_Name;
   
   ---------------------------
   -- Hex_Integer_To_String --
   ---------------------------
   
   function Hex_Integer_To_String (Value : Integer) return String is
      
      type Hex_Digit_Array is array (0..15) of Character;
      Hex_Digit : Hex_Digit_Array := 
	('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
	 'a', 'b', 'c', 'd', 'e', 'f');
     
      Dig    : Integer;
      Res    : Integer;
      Buffer : Unbounded_String := Null_Unbounded_String;
   begin
      Res := Value;
      loop
	 Dig := Res mod 16;
	 Append (Buffer, Hex_Digit (Dig));
	 Res := Res / 16;
	 
	 exit when Res = 0;
      end loop;
      
      return To_String (Buffer);
   end Hex_Integer_To_String;
   
   function Hex_Integer_To_String (Value : Integer) return Name_Id is
      S : String := Hex_Integer_To_String (Value);
   begin
      return String_Find (S);
   end Hex_Integer_To_String;
   
   -------------------------
   -- Boolean_From_String --
   -------------------------
   
   function Boolean_From_String (Value : String) return Boolean is
      Val : String := Value;
   begin
      Trim (Val, Both);
      To_Lower (Val);
      return Val = "1" or else Val = "true";
   end Boolean_From_String;
   
   function Boolean_From_Name (Value : Name_Id) return Boolean is
   begin
      return Value = String_Find ("true");
   end Boolean_From_Name;
   
   ---------------------
   -- Float_To_String --
   ---------------------
   
   function Float_To_String (Value : Float) return String is
      Val : String := Float'Image (Value);
      Res : String := Trim (Val, Both);
   begin
      --Trim (Val, Both);
      To_Lower (Res);
      return Res;
   end Float_To_String;
   
   function Float_To_Name (Value : Float) return Name_Id is
      Val : String := Float'Image (Value);
      Res : String := Trim (Val, Both);
   begin
      return String_Find (Res);
   end Float_To_Name;
   
   ---------------------------
   -- Float_To_Fixed_String --
   ---------------------------
   
   function Float_To_Fixed_String_Old
     (Value : Float; Dec : Integer) return String is

      Integ     : Long_Long_Integer;
      F         : Float;
      Decim     : Long_Long_Integer;
     
   begin
      Integ := Long_Long_Integer (Float'Truncation (Value));
      declare
         Str_Int : String := Trim (Integ'Img, Both);
      begin     
         -- extract decimal part only (0.xxxxxx)
         F := abs (Value - Float'Truncation (Value));

         -- trick : add '1' as head digit to keep all 
         -- signifiant zeros after dot
         -- (1.[000]xxxxx)
         F := 1.0 + F;
         -- (1[000]xxxxx.-----)
         F := F * (10.0 ** Dec);
         
         -- (1[000]xxxxx)
         Decim := Long_Long_Integer (Float'Truncation (F));
      
         declare
            Str_Decim : String := Trim (Decim'Img, Both);
            
         begin
            -- Suppr zeros in tail
            for I in reverse Str_Decim'Range loop
               if Str_Decim (I) = '0' then
                  Str_Decim (I) := ' ';
               else
                  exit;
               end if;
            end loop;
            -- Suppr heading '1'
            Str_Decim(Str_Decim'First) := ' ';
            declare
               Filtered_Decim : String := Trim (Str_Decim, Both);
            begin
               if Filtered_Decim = "" then
                  return Str_Int & ".0";
               end if;
               return Str_Int & '.' & Filtered_Decim;
            end;
         end;
      end;
   end Float_To_Fixed_String_old;
   
   ---------------------------
   -- Float_To_Fixed_String --
   ---------------------------
   
   function Float_To_Fixed_String
     (Value : Float; Dec : Integer) return String is

      Integ : Long_Long_Integer;
      F     : Float;
      Fabs  : Float;
      Decim : Long_Long_Integer;
      Sign  : Boolean;
   begin
      Sign := Value < 0.0;
      Fabs := abs (Value);
      Integ := Long_Long_Integer (Float'Truncation (Fabs));
      
      declare
         Str_Int : String := Trim (Integ'Img, Both);
      begin     
         -- extract decimal part only (0.xxxxxx)
         F := Fabs - Float'Truncation (Fabs);

         -- trick : add '1' as head digit to keep all 
         -- signifiant zeros after dot
         -- (1.[000]xxxxx)
         F := 1.0 + F;
         -- (1[000]xxxxx.-----)
         F := F * (10.0 ** Dec);
         
         -- (1[000]xxxxx)
         Decim := Long_Long_Integer (Float'Truncation (F));
      
         declare
            Str_Decim : String := Trim (Decim'Img, Both);
            
         begin
            -- Suppr zeros in tail
            for I in reverse Str_Decim'Range loop
               if Str_Decim (I) = '0' then
                  Str_Decim (I) := ' ';
               else
                  exit;
               end if;
            end loop;
	    
            -- Suppr heading '1'
	    
            Str_Decim (Str_Decim'First) := ' ';
	    
            declare
               Filtered_Decim : String := Trim (Str_Decim, Both);
            begin
               if Filtered_Decim = "" then
		  if Sign then
		     return "-" & Str_Int & ".0";
		  else
		     return Str_Int & ".0";
		  end if;
               end if;
	       
	       if Sign then
		  return "-" & Str_Int & '.' & Filtered_Decim;
	       else
		  return Str_Int & '.' & Filtered_Decim;
	       end if;
            end;
         end;
      end;
   end Float_To_Fixed_String;
   
   -----------------------
   -- Integer_To_String --
   -----------------------
   
   function Integer_To_String (Value : Integer) return String is
      
      Val : String := Integer'Image (Value);
      Res : String := Trim (Val, Both);
   begin
      return Res;
   end Integer_To_String;
   
   function Integer_To_Name (Value : Integer) return Name_Id is
   begin
      return String_Find (Integer_To_String (Value));
   end Integer_To_Name;
   
   ----------------------------
   -- Long_Integer_To_String --
   ----------------------------
   
   function Long_Integer_To_String (Value : Long_Integer) return String is
      
      Val : String := Long_Integer'Image (Value);
      Res : String := Trim (Val, Both);
   begin
      return Res;
   end Long_Integer_To_String;
      
   function Long_Integer_To_Name (Value : Long_Integer) return Name_Id is
   begin
      return String_Find (Long_Integer_To_String (Value));
   end Long_Integer_To_Name;
   
   --------------------
   -- Word_To_String --
   --------------------
   
   function Word_To_String (Value : Word) return String is
      
      Val : String := Word'Image (Value);
      Res : String := Trim (Val, Both);
   begin
      return Res;
   end Word_To_String;
   
   function Word_To_Name (Value : Word) return Name_Id is
   begin
      return String_Find (Word_To_String (Value));
   end Word_To_Name;
   
   -----------------------
   -- Boolean_To_String --
   -----------------------
   
   function Boolean_To_String (Value : Boolean) return String is
   begin
      if Value then
	 return "true";
      else
	 return "false";
      end if;
   end Boolean_To_String;
   
   function Boolean_To_Name (Value : Boolean) return Name_Id is
   begin
      if Value then
	 return String_Find ("true");
      else
	 return String_Find ("false");
      end if;
   end Boolean_To_Name;
   
   Epsilon : constant Float := 0.001;
   --------------------
   -- Epsilon_Equal --
   --------------------
   
   function Epsilon_Equal
     (V1 : Float;
      V2 : Float) return Boolean is
   begin
      return (abs (V2-V1)) < Epsilon;
   end Epsilon_Equal;
   
   -----------------
   -- Epsilon_Sup --
   -----------------
   
   function Epsilon_Sup
     (V1 : Float;
      V2 : Float) return Boolean is
   begin
      return not Epsilon_Equal (V1, V2) and then V1 > V2;
   end Epsilon_Sup;
   
   -----------------
   -- Epsilon_Inf --
   -----------------
   
   function Epsilon_Inf
     (V1 : Float;
      V2 : Float) return Boolean is
   begin
      return not Epsilon_Equal (V1, V2) and then V1 < V2;
   end Epsilon_Inf;
   
   -----------------------
   -- Epsilon_Sup_Equal --
   -----------------------
   
   function Epsilon_Sup_Equal
     (V1 : Float;
      V2 : Float) return Boolean is
   begin
      return Epsilon_Equal (V1, V2) or else V1 > V2;
   end Epsilon_Sup_Equal;
   
   -----------------
   -- Epsilon_Inf --
   -----------------
   
   function Epsilon_Inf_Equal
     (V1 : Float;
      V2 : Float) return Boolean is
   begin
      return Epsilon_Equal (V1, V2) or else V1 < V2;
   end Epsilon_Inf_Equal;
   
end Artics.Utils;
