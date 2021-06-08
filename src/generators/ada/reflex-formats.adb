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
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with Ada.Text_IO;

package body Reflex.Formats is
   
   -------------
   --To_Lower --
   -------------

   function To_Lower (S  : String) return String is
      W : String := S;
   begin
      Gnat.Case_Util.To_Lower (W);
      return W;
   end To_Lower;
   
   -----------------
   -- Starts_With --
   -----------------
   
   function Starts_With 
     (N : Str_Id;
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
         return (N(N'First.. N'First + S'Length - 1) = S);
      else
         return False;
      end if;
   end Starts_With;
   
   -----------------------
   -- Float_From_String --
   -----------------------
   
   function Float_From_String   (Value : String) return Float is
      Val : String := Value;
   begin
      Trim (Val, Both);
      return Float'Value (Val);
   end Float_From_String;
   
   function Float_From_Name (Value : Str_Id) return Float is
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
   
   function Integer_From_Name (Value : Str_Id) return Integer is
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
   
   function Hex_Integer_From_Name (Value : Str_Id) return Integer is
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
   
   function Hex_Integer_To_String (Value : Integer) return Str_Id is
      S : String := Hex_Integer_To_String (Value);
   begin
      return Enter_String (S);
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
   
   function Boolean_From_Name (Value : Str_Id) return Boolean is
   begin
      return Value = Enter_String ("true");
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
   
   function Float_To_Name (Value : Float) return Str_Id is
      Val : String := Float'Image (Value);
      Res : String := Trim (Val, Both);
   begin
      return Enter_String (Res);
   end Float_To_Name;
   
   ---------------------------
   -- Float_To_Fixed_String --
   ---------------------------
   
   function Float_To_Fixed_String_Old
     (Value : Float; 
      Dec   : Integer) return String is

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
     (Value : Float; 
      Dec   : Integer) return String is

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
   
   function Integer_To_Name (Value : Integer) return Str_Id is
   begin
      return Enter_String (Integer_To_String (Value));
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
      
   function Long_Integer_To_Name (Value : Long_Integer) return Str_Id is
   begin
      return Enter_String (Long_Integer_To_String (Value));
   end Long_Integer_To_Name;
   
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
   
   function Boolean_To_Name (Value : Boolean) return Str_Id is
   begin
      if Value then
	 return True_String;
      else
	 return False_String;
      end if;
   end Boolean_To_Name;
   
   -------------------
   -- Latin1_To_Xml --
   -------------------
   
   function Latin1_To_Xml (S : String) return String is
      
      R : Unbounded_String;
   begin
      
      for I in S'Range loop
	 case S(I) is
	    
	    when NUL ..BS =>
	       Append (R, S (I));
	       
	    when HT =>
	       Append (R, S (I));
	       
	    when LF =>
	       Append (R, S (I));
	       
	    when VT =>
	       Append (R, S (I));
	       
	    when FF => 
	       Append (R, S (I));
	       
	    when CR =>
	       Append (R, S (I));
	       
	    when SO .. US =>
	       Append (R, S (I));
	       
	       --------------------------------
	       -- ISO 646 Graphic Characters --
	       --------------------------------
	       
	    when Ada.Characters.Latin_1.Space => 
	       Append (R, S (I));
	       
	    when Exclamation =>
	       Append (R, S (I));
	       
	    when Quotation =>
	      -- '"' Character'Val(34)
	      Append (R, "&quot;");
	      
	    when Number_Sign =>
	       Append (R, S (I));
	       
	    when Dollar_Sign =>
	       Append (R, S (I));
	       
	    when Percent_Sign => 
	       Append (R, S (I));
	       
	    when Ampersand =>
	       -- '&' Character'Val(38)
	       Append (R, "&amp;");
	       
	    when Apostrophe =>
	       --  ''' Character'Val(39)
	       Append (R, "&apos;");
	       
	    when Left_Parenthesis =>
	       Append (R, S (I));
	       
	    when Right_Parenthesis =>
	       Append (R, S (I));
	       
	    when Asterisk =>
	      -- '*' Character'Val(42)
	      Append (R, '*'); --  "&ast;");
	      
	    when Plus_Sign => 
	       Append (R, S (I));
	       
	    when Comma =>
	       Append (R, S (I));
	       
	    when Minus_Sign =>
	       Append (R, S (I));
	       
	    when Full_Stop =>
	       Append (R, S (I));
	       
	    when Solidus =>
	       Append (R, S (I));
	       
	       --  Decimal digits '0' though '9' are at positions 48 through 57
	       
	    when '0' .. '9' =>
	       Append (R, S (I));
	       
	    when Colon => 
	       Append (R, S (I));
	       
	    when Semicolon => 
	       Append (R, S (I));
	       
	    when Less_Than_Sign =>
	       --  '<' Character'Val(60)
	       Append (R, "&lt;");
	       
	    when Equals_Sign =>
	       Append (R, S (I));
	       
	    when Greater_Than_Sign =>
	       --  '>' Character'Val(62
	       Append (R, "&gt;");
	       
	    when Question =>
	       Append (R, S (I));
	       
	    when Commercial_At =>
	       Append (R, S (I));

	       --  Letters 'A' through 'Z' are at positions 65 through 90
	    when 'A' .. 'Z' =>
	       Append (R, S (I));
	       
	       
	    when Left_Square_Bracket =>
	       Append (R, S (I));
	       
	    when Reverse_Solidus =>
	       Append (R, S (I));
	       
	    when Right_Square_Bracket =>
	       Append (R, S (I));
	       
	    when Circumflex =>
	       Append (R, S (I));
	       
	    when Low_Line =>
	       Append (R, S (I));
	       
	    when Grave => 
	       Append (R, S (I));
	       
	    when LC_A .. LC_Z =>
	       Append (R, S (I));
	       
	    when Left_Curly_Bracket =>
	       Append (R, S (I));
	       
	    when Vertical_Line => 
	       Append (R, S (I));
	       
	    when Right_Curly_Bracket =>
	       Append (R, S (I));
	       
	    when Tilde => 
	       Append (R, S (I));
	       
	    when DEL => 
	       Append (R, S (I));
	       

	    when Reserved_128 =>
	       Append (R, S (I));
	       
	    when Reserved_129 =>
	       Append (R, S (I));
	       
	    when BPH => 
	       Append (R, S (I));
	       
	    when NBH =>
	       Append (R, S (I));
	       
	    when Reserved_132 =>
	       Append (R, S (I));
	       
	    when NEL =>
	       Append (R, S (I));
	       
	    when SSA =>
	       Append (R, S (I));
	       
	    when ESA =>
	       Append (R, S (I));
	       
	    when HTS => 
	       Append (R, S (I));
	       
	    when HTJ => 
	       Append (R, S (I));
	       
	    when VTS =>
	       Append (R, S (I));
	       
	    when PLD =>
	       Append (R, S (I));
	       
	    when PLU =>
	       Append (R, S (I));
	       
	    when RI => 
	       Append (R, S (I));
	       
	    when SS2 =>
	       Append (R, S (I));
	       
	    when SS3 =>
	       Append (R, S (I));
	       
	    when DCS => 
	       Append (R, S (I));
	       
	    when PU1 =>
	       Append (R, S (I));
	       
	    when PU2 =>
	       Append (R, S (I));
	       
	    when STS =>
	       Append (R, S (I));
	       
	    when CCH =>
	       Append (R, S (I));
	       
	    when MW =>
	       Append (R, S (I));
	       
	    when SPA =>
	       Append (R, S (I));
	       
	    when EPA =>
	       Append (R, S (I));
	       
	    when SOS =>
	       Append (R, S (I));
	       
	    when Reserved_153 =>
	       Append (R, S (I));
	       
	    when SCI =>
	       Append (R, S (I));
	       
	    when CSI =>
	       Append (R, S (I));
	       
	    when ST =>
	       Append (R, S (I));
	       
	    when OSC =>
	       Append (R, S (I));
	       
	    when PM =>
	       Append (R, S (I));
	       
	    when APC =>
	       Append (R, S (I));

	       ------------------------------
	       -- Other Graphic Characters --
	       ------------------------------
	       
	       --  Character positions 160 (16#A0#) .. 175 (16#AF#)

	    when No_Break_Space =>
	       --  Character'Val (160);
	       --  Non-breaking space &nbsp; &#160;
	       Append (R, "&nbsp;");
	       
	    when Inverted_Exclamation =>
	       Append (R, "&iexcl;");
	       
	    when Cent_Sign =>
	       --  haracter'Val (162);
	       --  &cent; &#162;
	       Append (R, "&cent;");

	    when Pound_Sign =>
	       -- Character'Val (163);
	       -- pound &pound; &#163;
	       Append (R, "&pound;");
	       
	    when Currency_Sign =>
	       Append (R, S (I));
	       
	    when Yen_Sign =>
	       -- Character'Val (165);
	       -- yen &yen; &#165;
	       Append (R, "&yen;");
	       
	    when Broken_Bar =>
	       Append (R, "&brvbar;");
	       
	    when Section_Sign =>
	       Append (R, "&sect;");
	       
	    when Diaeresis =>
	       Append (R, "&uml;");
	       
	    when Copyright_Sign =>
	       --  Character'Val (169);
	       --  copyright &copy; &#169;
	       Append (R, "&copy;");
	       
	    when Feminine_Ordinal_Indicator =>
	       Append (R, S (I));
	       
	    when Left_Angle_Quotation =>
	       Append (R, S (I));
	       
	    when Not_Sign =>
	       Append (R, "&not;");
	       
	    when Soft_Hyphen => 
	       Append (R, "&shy;");
	       
	    when Registered_Trade_Mark_Sign =>
	       -- Character'Val (174);
	       -- registered trademark &reg; &#174;   
	       Append (R, "&reg;");
	       
	    when Macron =>
	       Append (R, "&macr;");

	       --  Character positions 176 (16#B0#) .. 191 (16#BF#)

	    when Degree_Sign =>
	       Append (R, S (I));
	       
	    when Plus_Minus_Sign =>
	       Append (R, S (I));
	       
	    when Superscript_Two =>
	       Append (R, S (I));
	       
	    when Superscript_Three =>
	       Append (R, S (I));
	       
	    when Acute =>
	       -- Character'Val (180);
	       -- acute 000B4 180
	       Append (R, "&acute;");
	       
	    when Micro_Sign =>
	       Append (R, S (I));
	       
	    when Paragraph_Sign =>
	       Append (R, S (I));
	       
	    when Middle_Dot =>
	       Append (R, S (I));
	       
	    when Cedilla =>
	       Append (R, S (I));
	       
	    when Superscript_One =>
	       Append (R, S (I));
	       
	    when Masculine_Ordinal_Indicator =>
	       Append (R, S (I));
	       
	    when Right_Angle_Quotation =>
	       Append (R, S (I));
	       
	    when Fraction_One_Quarter =>
	       Append (R, S (I));
	       
	    when Fraction_One_Half =>
	       Append (R, S (I));
	       
	    when Fraction_Three_Quarters =>
	       Append (R, S (I));
	       
	    when Inverted_Question =>
	       Append (R, S (I));
	       

	       --  Character positions 192 (16#C0#) .. 207 (16#CF#)

	    when UC_A_Grave =>
	       -- Character'Val (192);
	       -- Agrave 000C0 192
	       Append (R, "&Agrave;");
	       
	    when UC_A_Acute =>
	       --  Character'Val (193);
	       --  Aacute 000C1 193
	       Append (R, "&Aacute;");
	       
	    when UC_A_Circumflex =>
	       --  Character'Val (194);
	       --  Acirc 000C2 194
	       Append (R, "&Acirc;");
	       
	    when UC_A_Tilde =>
	       --  Character'Val (195);
	       --  Atilde 000C3 195
	       Append (R, "&Atilde;");
	       
	    when UC_A_Diaeresis =>
	       -- Character'Val (196);
	       -- Auml 000C4 196
	       Append (R, "&Auml;");
	       
	    when UC_A_Ring =>
	       -- Character'Val (197);
	       -- Aring 000C5 197
	       Append (R, "&Aring;");

	    when UC_AE_Diphthong =>
	       -- Character'Val (198);
	       -- AElig 000C6 198
	       Append (R, "&AElig;");
	       
	    when UC_C_Cedilla =>
	       Append (R, S (I));
	       
	    when UC_E_Grave =>
	       Append (R, S (I));
	       
	    when UC_E_Acute =>
	       Append (R, S (I));
	       
	    when UC_E_Circumflex =>
	       Append (R, S (I));
	       
	    when UC_E_Diaeresis =>
	       Append (R, S (I));
	       
	    when UC_I_Grave =>
	       Append (R, S (I));
	       
	    when UC_I_Acute =>
	       Append (R, S (I));
	       
	    when UC_I_Circumflex =>
	       Append (R, S (I));
	       
	    when UC_I_Diaeresis =>
	       Append (R, S (I));
	       

	       --  Character positions 208 (16#D0#) .. 223 (16#DF#)

	    when UC_Icelandic_Eth =>
	       Append (R, S (I));
	       
	    when UC_N_Tilde =>
	       Append (R, S (I));
	       
	    when UC_O_Grave =>
	       Append (R, S (I));
	       
	    when UC_O_Acute =>
	       Append (R, S (I));
	       
	    when UC_O_Circumflex =>
	       Append (R, S (I));
	       
	    when UC_O_Tilde =>
	       Append (R, S (I));
	       
	    when UC_O_Diaeresis =>
	       Append (R, S (I));
	       
	    when Multiplication_Sign =>
	       Append (R, S (I));
	       
	    when UC_O_Oblique_Stroke =>
	       Append (R, S (I));
	       
	    when UC_U_Grave =>
	       Append (R, S (I));
	       
	    when UC_U_Acute =>
	       Append (R, S (I));
	       
	    when UC_U_Circumflex =>
	       Append (R, S (I));
	       
	    when UC_U_Diaeresis =>
	       -- Character'Val (220);
	       -- auml 000E4 228
	       Append (R, "&auml;");
	       
	    when UC_Y_Acute =>
	       Append (R, S (I));
	       
	    when UC_Icelandic_Thorn =>
	       Append (R, S (I));
	       
	    when LC_German_Sharp_S =>
	       Append (R, S (I));
	       

	       --  Character positions 224 (16#E0#) .. 239 (16#EF#)
	       
	    when LC_A_Grave =>
	       -- Character'Val (224);
	       -- agrave 000E0 224
	       Append (R, "&agrave;");
	       
	    when LC_A_Acute =>
	       --  Character'Val (225);
	       -- aacute 000E1 225
	       Append (R, "&aacute;");
	       
	    when LC_A_Circumflex =>
	       -- Character'Val (226);
	       -- acirc 000E2 226
	       Append (R, "&acirc;");
	       
	    when LC_A_Tilde =>
	       -- Character'Val (227);
	       -- atilde 000E3 227
	       Append (R, "&atilde;");
	       
	    when LC_A_Diaeresis =>
	       Append (R, S (I));
	       
	    when LC_A_Ring =>
	       --  Character'Val (229);
	       --  aring 000E5 229
	       Append (R, "&aring;");
	       
	    when LC_AE_Diphthong =>
	       -- Character'Val (230);
	       -- 	aelig 000E6 230
	       Append (R, "&aelig;");
	       
	    when LC_C_Cedilla =>
	       Append (R, S (I));
	       
	    when LC_E_Grave =>
	       Append (R, S (I));
	       
	    when LC_E_Acute =>
	       Append (R, S (I));
	       
	    when LC_E_Circumflex =>
	       Append (R, S (I));
	       
	    when LC_E_Diaeresis =>
	       Append (R, S (I));
	       
	    when LC_I_Grave =>
	       Append (R, S (I));
	       
	    when LC_I_Acute =>
	       Append (R, S (I));
	       
	    when LC_I_Circumflex =>
	       Append (R, S (I));
	       
	    when LC_I_Diaeresis =>
	       Append (R, S (I));
	       

	       --  Character positions 240 (16#F0#) .. 255 (16#FF)
	    when LC_Icelandic_Eth =>
	       Append (R, S (I));
	       
	    when LC_N_Tilde =>
	       Append (R, S (I));
	       
	    when LC_O_Grave =>
	       Append (R, S (I));
	       
	    when LC_O_Acute =>
	       Append (R, S (I));
	       
	    when LC_O_Circumflex =>
	       Append (R, S (I));
	       
	    when LC_O_Tilde =>
	       Append (R, S (I));
	       
	    when LC_O_Diaeresis =>
	       Append (R, S (I));
	       
	    when Division_Sign =>
	       Append (R, S (I));
	       
	    when LC_O_Oblique_Stroke =>
	       Append (R, S (I));
	       
	    when LC_U_Grave =>
	       Append (R, S (I));
	       
	    when LC_U_Acute =>
	       Append (R, S (I));
	       
	    when LC_U_Circumflex =>
	       Append (R, S (I));
	       
	    when LC_U_Diaeresis =>
	       Append (R, S (I));
	       
	    when LC_Y_Acute =>
	       Append (R, S (I));
	       
	    when LC_Icelandic_Thorn =>
	       Append (R, S (I));
	       
	    when LC_Y_Diaeresis =>
	       Append (R, S (I));
	 end case;
      end loop;
      
      return To_String (R);
   end Latin1_To_Xml;   
   
   -------------------
   -- Xml_To_Latin1 --
   -------------------
   
   function Xml_To_Latin1 (S : String) return String is
      
      C : Character;
      I : Positive;
      J : Positive;
      R : Unbounded_String;
   begin
      I := S'First;
      while I <= S'Last loop
	 if S(I) = '&' then
	    J := I;
	    loop
	       exit when J = S'Last;
	       J := J + 1;
	       exit when S (J) = ';';
	    end loop;
	    
	    if J = S'Last and then S (J) /= ';' then
	       Append (R, S (I..J));
	    else
	       C := Decode_Xml_Entity (S (I..J));
	       Append (R, C);
	    end if;
	    
	    I := J + 1;
	    
	 else
	    Append (R, S (I));
	    I := I + 1;
	 end if;
      end loop;
      
      return To_String (R);
   end Xml_To_Latin1;
   
   -----------------------
   -- Decode_Xml_Entity --
   -----------------------
   
   function Decode_Xml_Entity (S : String) return Character is
   begin
      if S = "&quot;" then
	 return Quotation;
	 
      elsif S = "&amp;" then
	 return Ampersand;
	 
      elsif S = "&apos;" then
	 return Apostrophe;
	 
      elsif S = "&ast;" then
	 return Asterisk;
	 
      elsif S = "&lt;" then
	 return Less_Than_Sign;
	 
      elsif S = "&gt;" then
	 return Greater_Than_Sign;
	 
      elsif S = "&nbsp;" then
	 return No_Break_Space;
	 
      elsif S = "&cent;" then
	 return Cent_Sign;
	 
      elsif S = "&pound;" then
	 return Pound_Sign;
	       
      elsif S = "&yen;" then
	 return Yen_Sign;
	 
      elsif S = "&copy;" then
	 return Copyright_Sign;
	 
      elsif S = "&reg;" then
	   return Registered_Trade_Mark_Sign;
	   
      elsif S = "&acute;" then
	 return Acute;
	 
      elsif S = "&Agrave;" then
	 return UC_A_Grave;
	 
      elsif S = "&Aacute;" then
	 return UC_A_Acute;
	 
      elsif S = "&Acirc;" then
	 return UC_A_Circumflex;
	 
      elsif S = "&Atilde;" then
	 return UC_A_Tilde;
	 
      elsif S = "&Auml;" then
	 return UC_A_Diaeresis;
	 
      elsif S = "&Aring;" then
	 return UC_A_Ring;
	 
      elsif S = "&AElig;" then
	 return UC_AE_Diphthong;
	 
      elsif S = "&auml;" then
	 return UC_U_Diaeresis;
	 
      elsif S = "&agrave;" then
	 return LC_A_Grave;
	 
      elsif S = "&aacute;" then
	 return LC_A_Acute;
	 
      elsif S = "&acirc;" then
	 return LC_A_Circumflex;
	 
      elsif S = "&atilde;" then
	 return LC_A_Tilde;
	 
      elsif S = "&aring;" then
	 return LC_A_Ring;
	 
      elsif S = "&aelig;" then
	 return LC_AE_Diphthong;
      else
	 return '!';
      end if;
   end Decode_Xml_Entity;
   
end Reflex.Formats;
