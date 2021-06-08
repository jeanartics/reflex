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

with Ada.Text_Io; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.Case_Util; use GNAT.Case_Util;

with Types; use Types;
with Atree;    use Atree;
with Csets;    use Csets;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Eval; use Sem_Eval;
with Sem_Mech; use Sem_Mech;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Table;
with Ttypes;   use Ttypes;
with Types;    use Types;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

with Reflex.Gen.Types; use Reflex.Gen.Types;
with Reflex.Gen.Utils; use Reflex.Gen.Utils;

with Reflex.Gen.Dispatch_Interface; use Reflex.Gen.Dispatch_Interface;
with Glips.Options;

package body Reflex.Gen.Outputs is
   
   ----------------------
   -- Is_Unity_Keyword --
   ----------------------
   
   function Is_Unity_Keyword (Name : Name_Id) return Boolean is
   begin
      Get_Name_String (Name);
      
      for J in 1 .. Name_Len loop
	 Name_Buffer (J) := Fold_Lower (Name_Buffer (J));
      end loop;
      
      declare
	 Str_Name : String renames Name_Buffer (1 .. Name_Len);
      begin
	 --  No need to check C keywords which are also Ada reserved words
	 --  since (if present) they were rejected by the Ada front end.
	 --  Those keywords are: case do else for goto if return while.
	 
	 return Str_Name = "auto"
	   or else Str_Name = "break"
	   or else Str_Name = "char"
	   or else Str_Name = "const"
	   or else Str_Name = "continue"
	   or else Str_Name = "default"
	   or else Str_Name = "double"
	   or else Str_Name = "enum"
	   or else Str_Name = "extern"
	   or else Str_Name = "float"
	   or else Str_Name = "int"
	   or else Str_Name = "long"
	   or else Str_Name = "register"
	   or else Str_Name = "short"
	   or else Str_Name = "signed"
	   or else Str_Name = "sizeof"
	   or else Str_Name = "static"
	   or else Str_Name = "struct"
	   or else Str_Name = "switch"
	   or else Str_Name = "typedef"
	   or else Str_Name = "union"
	   or else Str_Name = "unsigned"
	   or else Str_Name = "void"
	   or else Str_Name = "volatile";
      end;
   end Is_Unity_Keyword;

   ------------------
   -- Is_Qualified --
   ------------------
   
   function Is_Qualified (Name : Name_Id) return Boolean is
   begin
      Get_Name_String (Name);
      
      --  Names starting with an upper-case letter are not qualified
      
      if Name_Buffer (1) in 'A' .. 'Z' then
	 return False;
	 
      else
	 --  Names containing __ are qualified, others aren't
	 
	 for J in 2 .. Name_Len loop
	    if Name_Buffer (J) = '_' and then Name_Buffer (J - 1) = '_' then
	       return True;
	    end if;
	 end loop;
	 
	 return False;
      end if;
   end Is_Qualified;

   ----------------------
   -- Remove_Qualified --
   ----------------------
   
   function Remove_Qualified (Name : Name_Id) return String is
   begin
      Get_Name_String (Name);
      
      --  Names containing __ are qualified, others aren't
      
      for J in 2 .. Name_Len - 1 loop
	 if Name_Buffer (J) = '_' and then Name_Buffer (J - 1) = '_' then
	    return Name_Buffer (J + 1 .. Name_Len);
	 end if;
      end loop;
      
      return Name_Buffer (1 .. Name_Len);
   end Remove_Qualified;
   
   ---------------------
   -- Float_To_String --
   ---------------------
   
   function Float_To_String (Value : Float) return String is
      Val : String := Float'Image (Value);
      Res : String := Trim (Val, Both);
   begin
      To_Lower (Res);
      return Res;
   end Float_To_String;
   
   -----------------------
   -- Write_C_Char_Code --
   -----------------------

   Hex : constant array (Char_Code range 0 .. 15) of Character :=
     "0123456789abcdef";

   procedure Write_C_Char_Code
     (Ob : Output_Buffer;
      CC : Char_Code) is
      
      C : Character;
   begin
      --  For now, output wide characters simply as ?

      if CC > 255 then
         Write_Char (Ob, '?');
         return;
      end if;

      C := Character'Val (CC);

      --  Remaining characters in range 0 .. 255, output with most appropriate
      --  C (escape) sequence.

      case C is
         when ASCII.BS =>
            Write_Str (Ob, "\b");

         when ASCII.FF =>
            Write_Str (Ob, "\f");

         when ASCII.LF =>
            Write_Str (Ob, "\n");

         when ASCII.CR =>
            Write_Str (Ob, "\r");

         when ASCII.HT =>
            Write_Str (Ob, "\t");

         when ASCII.VT =>
            Write_Str (Ob, "\v");

         when ' ' .. '~' =>
            if C = '\' or C = '"' or C = ''' then
               Write_Char (Ob, '\');
            end if;

            Write_Char (Ob, C);

         when others =>
            Write_Str (Ob, "\x");
            Write_Char (Ob, Hex (CC / 16));
            Write_Char (Ob, Hex (CC mod 16));
      end case;
   end Write_C_Char_Code;

   --------------
   -- Write_Id --
   --------------

   procedure Write_Id
     (Ob : Output_Buffer;
      N  : Node_Id) is
      
   begin
      --  Case of a defining identifier

      if Nkind (N) = N_Defining_Identifier then

         --  Itypes defined in package specs are propagated to the units
         --  depending on them through with clauses and do not always have
         --  a fully expanded name. This looks like a bug in the front end,
         --  which we workaround here for now???

         if Is_Itype (N) then

            --  Minimize cases where we add a prefix explicitly, to avoid
            --  generating pkg__pkg__Txxs instead of pkg__Txxs when the
            --  name has already been expanded.

            if not Is_Qualified (Chars (N)) then
               Write_Name (Ob, Chars (Enclosing_Package_Or_Subprogram (N)));
               Write_Str (Ob, "__");
            end if;

            --  if Is_Qualified (Chars (N)) then
	    --     Put_Line ("Qaulified ===============");
	    --     declare
            --        S : String := Remove_Qualified (Chars (N));
            --        Name : Name_Id := name_Find (S);
	    --     begin
	    --  	  Write_Name (Name);
	    --     end;
	    --  else
	    
	    Write_Name (Ob, Chars (N));
            
	    --  end if;

         --  If defining identifier has an interface name (and no address
         --  clause), then we output the interface name.

         elsif (Is_Imported (N) or else Is_Exported (N))
           and then Present (Interface_Name (N))
           and then No (Address_Clause (N))
         then
            String_To_Name_Buffer (Strval (Interface_Name (N)));
            Write_Casing_Name (Ob, Name_Buffer (1 .. Name_Len));

         --  Handle renamings of enumeration literals

         elsif Ekind (N) = E_Enumeration_Literal then
            Write_Name (Ob, Chars (Ultimate_Alias (N)));

         --  Change names that match C keywords except when the reference
         --  an entity defined in Standard (i.e. Float or Unsigned) since
         --  they correspond exactly with the C types with such name.

         --  elsif Scope (N) /= Standard_Standard
         --    and then Is_Unity_Keyword (Chars (N))
         --  then
         --     Write_Name (Ob, Chars (N));
         --     Write_Str (Ob, "_");

         --  If no interface name (or inactive because there was an address
         --  clause), then just output the Chars name.

         else
	    --  if Is_Qualified (Chars (N)) then
	    --     declare
	    --  	  S    : String := Remove_Qualified (Chars (N));
	    --  	  Name : Name_Id := Name_Find (S);
	    --     begin
	    --  	  Write_Name (Name);
	    --     end;
	    --  else
	    Write_Name (Ob, Chars (N));
	    --  end if;
         end if;

      --  Case of selector of an expanded name where the expanded name has
      --  an associated entity, output this entity. Check that the entity
      --  or associated node is of the right kind, see above.

      --  elsif Nkind (Parent (N)) = N_Expanded_Name
      --    and then Selector_Name (Parent (N)) = N
      --    and then Present (Entity_Or_Associated_Node (Parent (N)))
      --    and then Nkind (Entity (Parent (N))) in N_Entity
      --  then
      --  	 Put_Line ("1");
      --  	 Write_Id (Ob, Entity (Parent (N)));

      --  For enumeration literal, print representation value

      elsif Nkind (N) in N_Has_Entity
        and then Present (Entity (N))
        and then Ekind (Entity (N)) = E_Enumeration_Literal
      then
	 if Entity(N) = Standard_False then
	    Write_Str (Ob, "False");
	 elsif Entity (N) = Standard_True then
	    Write_Str (Ob, "True");
	 else
	    case Glips.Options.Literal_Gen is
	       when Glips.Options.Literal_Values =>
		  Write_Uint
		    (Ob, Enumeration_Rep (Entity (N)), Column_Check => False);
		  
	       when Glips.Options.Literal_Constants =>
		  null;
	       when Glips.Options.Literal_Enum =>
		  null;
	    end case;
	 end if;

      elsif Nkind (N) = N_Expanded_Name then
	 Write_Id (Ob, Prefix (N));
	 Write_Char (Ob, '.');
	 Write_Id (Ob, Selector_Name (N));

      --  For any other node with an associated entity, output entity name

      --  elsif Nkind (N) in N_Has_Entity
      --    and then Present (Entity_Or_Associated_Node (N))
      --    and then Nkind (Entity_Or_Associated_Node (N)) in N_Entity
      --  then
      --  	 Put_Line ("4");
      --     if Is_Private_Type (Entity (N)) then
      --        Write_Id (Ob, Get_Full_View (Entity (N)));
      --     else
      --        Write_Id (Ob, Entity (N));
      --     end if;
	 
      --  All other cases, we just print the Chars field
      --  ??? Might be missing some useful cases here

      else
	 Put_Line ("Name = " & Get_Name_String (Chars (N)));
         Write_Name (Ob, Chars (N));
      end if;
   end Write_Id;

   ------------------------
   -- Write_Integer_Type --
   ------------------------

   procedure Write_Integer_Type
     (Ob     : Output_Buffer;
      Siz    : Int; 
      Signed : Boolean) is
   begin
      Write_Str (Ob, "Int");
      return;
      if Signed then
	 if Siz <= 8 then
	    Write_Str (Ob, "Byte");
	 elsif Siz <= 16 then
	    Write_Str (Ob, "Int");
	 elsif Siz <= 32 then
	    Write_Str (Ob, "Dint");
	 else
	    Write_Str (Ob, "Dint");
	 end if;
	 
      else
	 if Siz <= 8 then
	    Write_Str (Ob, "Byte");
	 elsif Siz <= 16 then
	    Write_Str (Ob, "Word");
	 elsif Siz <= 32 then
	    Write_Str (Ob, "Dword");
	 else
	    Write_Str (Ob, "Dword");
	 end if;
      end if;
   end Write_Integer_Type;

   ----------------
   -- Write_Name --
   ----------------

   procedure Write_Name
     (Ob : Output_Buffer;
      N  : Name_Id) is
   begin
      Put_Line ("Write_Name : Name = " & Get_String (N));
      if N = Name_UParent then
	 Write_Casing_Name (Ob, "_parent");
	 
      elsif N = Name_UTag then
	 Write_Casing_Name (Ob, "tag");
	 
      else
	 Write_Casing_Name (Ob, Get_String (N));
      end if;
   end Write_Name;

   ------------------------
   -- Write_Source_Lines --
   ------------------------

   procedure Write_Source_Lines
     (Ob : Output_Buffer;
      N  : Node_Id) is
   begin
      if not Check_Sloc (Sloc (N)) then
         return;
      end if;

      Write_Source_Lines (Ob, First_Line (N), Last_Line (N));
   end Write_Source_Lines;

   procedure Write_Source_Lines 
     (Ob : Output_Buffer;
      S  : Source_Ptr) is
      
      L : constant Physical_Line_Number := Get_Physical_Line_Number (S);
   begin
      if not Check_Sloc (S) then
         return;
      end if;

      Write_Source_Lines (Ob, L, L);
   end Write_Source_Lines;

   procedure Write_Source_Lines
     (Ob   : Output_Buffer;
      From : Source_Ptr;
      To   : Physical_Line_Number) is
   begin
      if not Check_Sloc (From) then
         return;
      end if;

      Write_Source_Lines (Ob, Get_Physical_Line_Number (From), To);
   end Write_Source_Lines;

   procedure Write_Source_Lines 
     (Ob   : Output_Buffer;
      From : Physical_Line_Number;
      To   : Physical_Line_Number) is
      Src : constant Source_Buffer_Ptr := Source_Text (Current_Source_File);

      Write_Blank_Line : Boolean;
      --  If this is True, then a blank line is printed before outputting a
      --  source line, and the flag is reset.

      function Is_Comment_Line (L : Physical_Line_Number) return Boolean;
      --  Returns true if line L is a comment line or blank line

      procedure Write_Line_Directive (L : Physical_Line_Number);
      --  Write line directive for line L, no effect if L is a comment line

      procedure Write_Source_Line (L : Physical_Line_Number);
      --  Write source line L as C comment, no effect if L is a comment line.
      --  Outputs initial blank line if Write_Blank_Line flag is set and then
      --  resets the flag.

      ---------------------
      -- Is_Comment_Line --
      ---------------------

      function Is_Comment_Line (L : Physical_Line_Number) return Boolean is
         Scn : Source_Ptr;

      begin
         Scn := Line_Start (L, Current_Source_File);
         while Src (Scn) = ' ' or else Src (Scn) = ASCII.HT loop
            Scn := Scn + 1;
         end loop;

         return Src (Scn) in Line_Terminator
           or else Src (Scn .. Scn + 1) = "--";
      end Is_Comment_Line;

      --------------------------
      -- Write_Line_Directive --
      --------------------------

      procedure Write_Line_Directive (L : Physical_Line_Number) is
      begin
         --  No #line directives for comments or if no -g set

         if Is_Comment_Line (L) then
            return;
         end if;

         if Column (Ob) /= 1 then
            Write_Eol (Ob);
         end if;

         Write_Str (Ob, "#line ");
         Write_Int (Ob, Integer (L));
         Write_Str (Ob, ": ");
         Write_Str (Ob, Get_Name_String (File_Name (Current_Source_File)));
         Write_Eol (Ob);
      end Write_Line_Directive;

      -----------------------
      -- Write_Source_Line --
      -----------------------

      procedure Write_Source_Line (L : Physical_Line_Number) is
         Scn : Source_Ptr;

      begin
         if not Is_Comment_Line (L) then
            return;
         end if;

         if Write_Blank_Line then
            Write_Eol (Ob);
            Write_Blank_Line := False;
         end if;

         --  Write_Eol;
	 Write_Str (Ob, "-- ");
	 Write_Int (Ob, Integer (L));
	 Write_Str (Ob, ": ");

         Scn := Line_Start (L, Current_Source_File);
         while Src (Scn) not in Line_Terminator loop
            Write_Char (Ob, Src (Scn));
            Scn := Scn + 1;
         end loop;

	 Write_Eol (Ob);
      end Write_Source_Line;

      --  Local Variables

      From_Line : Physical_Line_Number := From;
      To_Line   : Physical_Line_Number := To;
      --  Effective from and to lines as adjusted below

   --  Start of processing for Write_Source_Lines

   begin
      --  Deal with no line number values

      if From_Line = No_Physical_Line_Number then
         if To_Line = No_Physical_Line_Number then
            return;
         else
            From_Line := To_Line;
         end if;
      end if;

      if To_Line = No_Physical_Line_Number then
         To_Line := From_Line;
      end if;

      --  If some lines already dealt with, adjust From_Line

      if Last_Line_Printed >= From_Line then
         From_Line := Last_Line_Printed + 1;
      end if;

      --  Return if all lines already printed. Adjust #line directive before
      --  to ensure that we resync the #line info.

      if From_Line > To_Line then
         --  Write_Line_Directive (To_Line);
         return;
      end if;

      --  If we are in Dump_Source_Text mode, and there are unprinted source
      --  lines before the first line for the current construct, print these
      --  source lines, but without line directives.

      if Dump_Source_Text and then Last_Line_Printed < From_Line - 1 then
         Write_Blank_Line := True;

         loop
            Last_Line_Printed := Last_Line_Printed + 1;
            exit when Last_Line_Printed = From_Line - 1;
            Write_Source_Line (Last_Line_Printed);
         end loop;
      end if;

      --  If we are in Dump_Source_Text mode, then print the source lines for
      --  the current construct, preceded by a blank line.

      if Dump_Source_Text then
         Write_Blank_Line := True;

         for J in From_Line .. To_Line loop
            Write_Source_Line (J);
         end loop;
      end if;

      --  Write line directive for the last line, no need to output multiple
      --  line directives.

      Write_Line_Directive (To_Line);

      --  Note all lines up to To processed and we are done

      Last_Line_Printed := To_Line;
      return;
   end Write_Source_Lines;
   
   --------------------------------
   -- Write_Source_Comment_Lines --
   --------------------------------
   
   procedure Write_Source_Comment_Lines
     (Ob : Output_Buffer;
      S  : Source_Ptr) is
      
      L : constant Physical_Line_Number := Get_Physical_Line_Number (S);
   begin
      if not Check_Sloc (S) then
         return;
      end if;

      Write_Source_Comment_Lines (Ob, Next_Comment_Line_To_Print, L);
   end Write_Source_Comment_Lines;

   --------------------------------
   -- Write_Source_Comment_Lines --
   --------------------------------
   
   procedure Write_Source_Comment_Lines
     (Ob   : Output_Buffer;
      From : Physical_Line_Number;
      To   : Physical_Line_Number) is
      
      Src : constant Source_Buffer_Ptr := Source_Text (Current_Source_File);

      Write_Blank_Line : Boolean;
      --  If this is True, then a blank line is printed before outputting a
      --  source line, and the flag is reset.

      function Is_Comment_Line (L : Physical_Line_Number) return Boolean;
      --  Returns true if line L is a comment line or blank line

      procedure Write_Source_Line (L : Physical_Line_Number);
      --  Write source line L as C comment, no effect if L is a comment line.
      --  Outputs initial blank line if Write_Blank_Line flag is set and then
      --  resets the flag.

      ---------------------
      -- Is_Comment_Line --
      ---------------------

      function Is_Comment_Line (L : Physical_Line_Number) return Boolean is
         Scn : Source_Ptr;
      begin
         Scn := Line_Start (L, Current_Source_File);
         while Src (Scn) = ' ' or else Src (Scn) = ASCII.HT loop
            Scn := Scn + 1;
         end loop;

         return Src (Scn) in Line_Terminator
           or else Src (Scn .. Scn + 1) = "--";
      end Is_Comment_Line;

      -----------------------
      -- Write_Source_Line --
      -----------------------

      procedure Write_Source_Line (L : Physical_Line_Number) is
         Scn : Source_Ptr;

      begin
         if not Is_Comment_Line (L) then
            return;
         end if;
 
        if Write_Blank_Line then
            Write_Eol (Ob);
            Write_Blank_Line := False;
         end if;

         --  Write_Eol;
	 Write_Str (Ob, "-- ");

         Scn := Line_Start (L, Current_Source_File);
         while Src (Scn) not in Line_Terminator loop
            Write_Char (Ob, Src (Scn));
            Scn := Scn + 1;
         end loop;

	 Write_Eol (Ob);
      end Write_Source_Line;

      --  Local Variables

      From_Line : Physical_Line_Number := From;
      To_Line   : Physical_Line_Number := To;
      --  Effective from and to lines as adjusted below

   --  Start of processing for Write_Source_Lines

   begin
      --  Deal with no line number values

      if From_Line = No_Physical_Line_Number then
         if To_Line = No_Physical_Line_Number then
            return;
         else
            From_Line := To_Line;
         end if;
      end if;

      if To_Line = No_Physical_Line_Number then
         To_Line := From_Line;
      end if;

      --  If some lines already dealt with, adjust From_Line

      if Next_Comment_Line_To_Print >= From_Line then
         From_Line := Next_Comment_Line_To_Print + 1;
      end if;

      --  Return if all lines already printed. Adjust #line directive before
      --  to ensure that we resync the #line info.

      if From_Line > To_Line then
         return;
      end if;

      --  If we are in Dump_Source_Text mode, and there are unprinted source
      --  lines before the first line for the current construct, print these
      --  source lines, but without line directives.

      if Next_Comment_Line_To_Print < From_Line - 1 then
         Write_Blank_Line := True;

         loop
            Next_Comment_Line_To_Print := Next_Comment_Line_To_Print + 1;
            exit when Next_Comment_Line_To_Print = From_Line - 1;
            Write_Source_Line (Next_Comment_Line_To_Print);
         end loop;
	 Write_Eol (Ob);
      end if;

      --  If we are in Dump_Source_Text mode, then print the source lines for
      --  the current construct, preceded by a blank line.

--        if Dump_Source_Comment then
--           Write_Blank_Line := True;
--  
--           for J in From_Line .. To_Line loop
--              Write_Source_Line (J);
--           end loop;
--  	 Write_Eol (Ob);
--        end if;

      --  Note all lines up to To processed and we are done

      Next_Comment_Line_To_Print := To_Line;
   end Write_Source_Comment_Lines;

   -------------------------
   -- Write_Str_Col_Check --
   -------------------------

   procedure Write_Str_Col_Check
     (Ob : Output_Buffer;
      S  : String) is

      Sprint_Line_Limit : Int := 120;
   begin
      if Int (S'Last) + Int (Column (Ob)) > Sprint_Line_Limit then
         Write_Indent_Str (Ob, "  ");

         if S (S'First) = ' ' then
            Write_Str (Ob, S (S'First + 1 .. S'Last));
         else
            Write_Str (Ob, S);
         end if;

      else
         Write_Str (Ob, S);
      end if;
   end Write_Str_Col_Check;

   ----------------
   -- Write_Uint --
   ----------------

   --  Note: we go out of our way to be compatible with ancient versions of C
   --  here, since we anticipate the output being compiled on such compilers.

   procedure Write_Uint
     (Ob           : Output_Buffer;
      U            : Uint;
      Column_Check : Boolean := True;
      Modular      : Boolean := False)
   is
      DDH : constant Nat := UI_Decimal_Digits_Hi (U);

      procedure Check_Column (Val : Nat);
      pragma Inline (Check_Column);
      --  Call Col_Check if Column_Check is True, otherwise do nothing

      ------------------
      -- Check_Column --
      ------------------

      procedure Check_Column (Val : Nat) is
      begin
         if Column_Check then
            Col_Check (Ob, Val);
         end if;
      end Check_Column;

   --  Start of processing for Write_Uint

   begin
      --  Output largest negative int value as (-X-1) where X is largest
      --  positive int value, to avoid generating out of range int value.

      if U = LNegInt then
         Check_Column (DDH + 4);
         Write_Char (Ob, '(');
         Write_Str (Ob, UI_Image (U + 1, Decimal));
         --  Write_Str (Ob, "-1)");

      --  Most common case of in int range other than largest neg number

      elsif LNegInt < U and then U <= LPosInt then
         Check_Column (DDH);
         Write_Str (Ob, UI_Image (U, Decimal));

         --  if Modular then
         --     Write_Char (Ob, 'U');
         --  end if;

      --  Output largest negative long value as (-XL-1) where X is largest
      --  positive long value, to avoid generating out of range long value.

      elsif U = LNegLong then
         Check_Column (DDH + 5);
         Write_Char (Ob, '(');
	 Write_Str (Ob, UI_Image (U + 1, Decimal));
         --  Write_Str (Ob, "L-1)");

      --  If in range of unsigned but not int, output with suffix U

      elsif LNegU <= U and then U <= LPosU then
         Check_Column (DDH + 1);
	 Write_Str (Ob, UI_Image (U, Decimal));
         --  Write_Char (Ob, 'U');

      --  If in range of long then output with suffix L

      elsif LNegLong < U and then U <= LPosLong then
         Check_Column (DDH + 1);
	 Write_Str (Ob, UI_Image  (U, Decimal));
         --  Write_Char (Ob, 'L');

         --  if Modular then
         --     Write_Char (Ob, 'U');
         --  end if;

      --  Remaining processing depends on whether we are allowing long long,
      --  which is controlled by restriction No_Long_Long_Integers.

      else
         --  Long_Long_Integer not allowed

--           if Restriction_Active (No_Long_Long_Integers) then
--  
--              --  We must be in range of long unsigned, output with suffix LU
--  
--              if LNegUL <= U and then U <= LPosUL then
--                 Check_Column (DDH + 2);
--  	       Write_Str (Ob, UI_Image  (U, Decimal));
--                 Write_Str (Ob, "LU");
--  
--              --  Anything else should be impossible!
--  
--              else
--                 raise Program_Error;
--              end if;
--  
--           --  Long_Long_Integer is allowed
--  
--           else
            --  If in range of long long, output with suffix LL. Note that we
            --  do not bother with largest negative number case here. We assume
            --  that if long long is allowed, the compiler is more modern.

            if LNegLL <= U and then U <= LPosLL then
               Check_Column (DDH + 2);
	       Write_Str (Ob, UI_Image (U, Decimal));
               --  Write_Str (Ob, "LL");

               --  if Modular then
               --     Write_Char (Ob, 'U');
               --  end if;

            --  If in range of long long unsigned, output with suffix LLU

            elsif LNegULL <= U and then U <= LPosULL then
               Check_Column (DDH + 3);
	       Write_Str (Ob, UI_Image (U, Decimal));
               --  Write_Str (Ob, "LLU");

            --  Anything else is capped to LPosULL. This can happen when
            --  outputing an unconstrained array indexed by Long_Long_Integer,
            --  see e.g. Ada.Streams.Stream_Element_Array

            else
               Check_Column (DDH + 2);
	       Write_Str (Ob, UI_Image (LPosULL, Decimal));
               --  Write_Str (Ob, "LLU");
            end if;
         end if;
      --end if;
   end Write_Uint;

   ---------------------------
   -- Write_Ureal_Col_Check --
   ---------------------------

   procedure Write_Ureal_Col_Check 
     (Ob : Output_Buffer;
      U  : Ureal) is
      
      procedure Write (Real : Ureal);
      --  Writes value of Real to standard output. As a result of evaluation of
      --  static expressions, it is possible to generate constants (e.g. 1/13)
      --  which have no such representation.

      -----------
      -- Write --
      -----------

      procedure Write (Real : Ureal) is
         T : Uint;

      begin
         --  If value is negative, we precede the constant by a minus sign

         if UR_Is_Negative (Real) then
            Write_Char (Ob, '-');
         end if;

         --  Zero is zero

         if UR_Is_Zero (Real) then
            Write_Str (Ob, "0.0");

         --  For constants with a denominator of zero, the value is simply the
         --  numerator value, since we are dividing by base**0, which is 1.

         elsif Denominator (Real) = 0 then
	    Write_Str (Ob, UI_Image (Numerator (Real), Decimal));
            Write_Str (Ob, ".0");

         --  Small powers of 2 get written in decimal fixed-point format

         elsif Rbase (Real) = 2
           and then Denominator (Real) <= 3
           and then Denominator (Real) >= -16
         then
            if Denominator (Real) = 1 then
	       T := Numerator (Real) * (10 / 2);
	       Write_Str (Ob, UI_Image (T / 10, Decimal));
               Write_Char (Ob, '.');
	       Write_Str (Ob, UI_Image (T mod 10, Decimal));

            elsif Denominator (Real) = 2 then
               T := Numerator (Real) * (100 / 4);
	       Write_Str (Ob, UI_Image (T / 100, Decimal));
               Write_Char (Ob, '.');
	       Write_Str (Ob, UI_Image (T mod 100 / 10, Decimal));

               if T mod 10 /= 0 then
		  Write_Str (Ob, UI_Image (T mod 10, Decimal));
               end if;

            elsif Denominator (Real) = 3 then
               T := Numerator (Real) * (1000 / 8);
	       Write_Str (Ob, UI_Image (T / 1000, Decimal));
               Write_Char (Ob, '.');
	       Write_Str (Ob, UI_Image (T mod 1000 / 100, Decimal));

               if T mod 100 /= 0 then
		  Write_Str (Ob, UI_Image (T mod 100 / 10, Decimal));

                  if T mod 10 /= 0 then
		     Write_Str (Ob, UI_Image (T mod 10, Decimal));
                  end if;
               end if;

            else
                Write_Str (Ob, UI_Image 
                 (Numerator (Real) * (Uint_2 ** (-Denominator (Real))),
                  Decimal));
               Write_Str (Ob, ".0");
            end if;

         --  If the base is non-zero, we normalize the real number and
         --  use recursion to process the resulting number.

         elsif Rbase (Real) /= 0 then

            --  Note that we do not propagate the negative sign since
            --  the minus character was alredy sent to the output

            Write
              (UR_From_Components
                (Num => Norm_Num (Real),
                 Den => Norm_Den (Real)));

         --  Rationals where numerator is divisible by denominator can be
         --  output as literals after we do the division. This includes the
         --  common case where the denominator is 1.

         elsif Numerator (Real) mod Denominator (Real) = 0 then
            Write_Str (Ob, UI_Image
			 (Numerator (Real) / Denominator (Real), Decimal));
            Write_Str (Ob, ".0");

         --  Other non-based (rational) constants are written in num/den style

         else
            Write_Str (Ob, UI_Image (Numerator (Real), Decimal));
            Write_Str (Ob, ".0/");
            Write_Str (Ob, UI_Image (Denominator (Real), Decimal));
            Write_Str (Ob, ".0");
         end if;
      end Write;

      --  Local variables

      D : constant Uint := Denominator (U);
      N : constant Uint := Numerator (U);

   begin
      Col_Check (Ob, UI_Decimal_Digits_Hi (D) + UI_Decimal_Digits_Hi (N) + 4);
      Write (U);
   end Write_Ureal_Col_Check;
   
   -------------------
   -- Is_Blank_Line --
   -------------------
   
   function Is_Blank_Line
     (L                   : Physical_Line_Number;
      Current_Source_File : Source_File_Index) return Boolean is
      
      Src : Source_Buffer_Ptr;
      Scn : Source_Ptr;
   begin
      Src := Source_Text (Current_Source_File);
      Scn := Line_Start (L, Current_Source_File);
      while Src (Scn) = ' ' or else Src (Scn) = ASCII.HT loop
	 Scn := Scn + 1;
      end loop;
      
      return Src (Scn) in Line_Terminator;
   end Is_Blank_Line;

   ------------------------------
   -- Is_Blank_Or_Comment_Line --
   ------------------------------
   
   function Is_Blank_Or_Comment_Line
     (L                   : Physical_Line_Number;
      Current_Source_File : Source_File_Index) return Boolean is
      
      Src : Source_Buffer_Ptr;
      Scn : Source_Ptr;
   begin
      Src := Source_Text (Current_Source_File);
      Scn := Line_Start (L, Current_Source_File);
      while Src (Scn) = ' ' or else Src (Scn) = ASCII.HT loop
	 Scn := Scn + 1;
      end loop;
      
      return Src (Scn) in Line_Terminator
	or else Src (Scn .. Scn + 1) = "--";
   end Is_Blank_Or_Comment_Line;

   ---------------------
   -- Is_Comment_Line --
   ---------------------
   
   function Is_Comment_Line
     (L                   : Physical_Line_Number;
      Current_Source_File : Source_File_Index) return Boolean is
      
      Src : Source_Buffer_Ptr;
      Scn : Source_Ptr;
   begin
      Src := Source_Text (Current_Source_File);
      Scn := Line_Start (L, Current_Source_File);
      while Src (Scn) = ' ' or else Src (Scn) = ASCII.HT loop
	 Scn := Scn + 1;
      end loop;
      
      return Src (Scn .. Scn + 1) = "--";
   end Is_Comment_Line;

   --------------------------------
   -- Write_Comment_Line_To_Node --
   --------------------------------
   
   procedure Write_Comment_Line_To_Node 
     (This   : access Generator_Record'Class;
      Node   : Node_Id;
      Buffer : Output_Buffer := null) is
      
      S  : Source_Ptr := Sloc (Node);
      To : Physical_Line_Number;
      Ob : Output_Buffer;
   begin
      if Comes_From_Source (Node) then
	 if not In_Instantiation (S) 
	   and then Get_Source_File_Index (S) = This.Get_Current_Source_File
	 then
	    To := Get_Physical_Line_Number (S);
	    if Buffer /= null then
	       Ob := Buffer;
	    else
	       Ob := This.Get_Output_Buffer;
	    end if;
	    Write_Comment_Line (This, To, Ob);
	 end if;
      end if;
   end Write_Comment_Line_To_Node;
   
   ------------------------
   -- Write_Comment_Line --
   ------------------------
   
   procedure Write_Comment_Line
     (This : access Generator_Record'Class;
      To   : Physical_Line_Number;
      Ob   : Output_Buffer) is
      
      procedure Emit_Or_Skip_Blank_Lines;
      --  Emit subsequent blank lines from the source file
      
      function More_Than_One_Comment_Line 
	(Comment_Line : Physical_Line_Number) return Boolean;
      --  Return True if the comment is composed of more then one line, in this
      --  case se emit the xomment as the following model :
      --  (*
      --   * Comment line
      --   * Comment line
      --   * .....
      --   *)
      --  If there is only one comment the model used is :
      --  (* Comment line *)
      
      procedure Emit_Comment_Lines;
      --  Emit the subsequent comment lines from the source file
      
      -- Ob                  : Output_Buffer := This.Get_Output_Buffer;
      Current_Source_File : Source_File_Index;
      Src                 : Source_Buffer_Ptr;
      Last_Line_Printed   : Physical_Line_Number;
      Scn                 : Source_Ptr;
      
      ------------------------------
      -- Emit_Or_Skip_Blank_Lines --
      ------------------------------
      
      procedure Emit_Or_Skip_Blank_Lines 
      is
      begin
	 while Last_Line_Printed <= To loop
	    exit when 
	      not Is_Blank_Line (Last_Line_Printed, Current_Source_File);
	    
	    if not Skip_Leading_Blank_Lines then
	       Write_Eol (Ob);
	    end if;
	    
	    Last_Line_Printed := Last_Line_Printed + 1;
	 end loop;
      end Emit_Or_Skip_Blank_Lines;
      
      --------------------------------
      -- More_Than_One_Comment_Line --
      --------------------------------
      
      function More_Than_One_Comment_Line 
	(Comment_Line : Physical_Line_Number) return Boolean is
	 
         Count : Natural := 0;
         line  : Physical_Line_Number := Comment_Line;
      begin
	 while line <= To loop
	    if Is_Comment_Line (line, Current_Source_File) 
	    then
	       Count := Count + 1;
	       exit when Count > 1;
	    else
	       exit;
	    end if;
	    line := line + 1;
	 end loop;
	 
	 return Count > 1;
      end More_Than_One_Comment_Line;
      
      ------------------------
      -- Emit_Comment_Lines --
      ------------------------
      
      procedure Emit_Comment_Lines is
	 
	 Multi_Line : Boolean;
	 First_Line : Boolean := True;
      begin
	 Multi_Line := More_Than_One_Comment_Line (Last_Line_Printed);
	 
	 --  Emit start command string
	 
	 case Comment_Type is
	    when No_Lead_Character =>
	       Write_Indent_Str (Ob, "");
	    when Iec_Comment_Type =>
	       Write_Indent_Str (Ob, "(*");
	    when C_Comment_Type =>
	       Write_Indent_Str (Ob, "/*");
	    when Ada_Comment_Type =>
	       Write_Indent_Str (Ob, "--");
	 end case;

	 --  Emit comment lines
	 
	 loop
	    Scn := Line_Start (Last_Line_Printed, Current_Source_File);
	    
	    --  Go to the begining of comment
	    
	    while Src (Scn) not in Line_Terminator loop
	       if Src (Scn) = '-' and then Src (Scn + 1) = '-' then
		  Scn := Scn + 2;
		  exit;
	       end if;
	       Scn := Scn + 1;
	    end loop;
	    
	    --  Emit the comment line
	    
	    if Multi_Line and then not First_Line then
	       case Comment_Type is
		  when No_Lead_Character =>
		     Write_Indent_Str (Ob, "");
		  when Iec_Comment_Type =>
		     Write_Indent_Str (Ob, " *");
		  when C_Comment_Type =>
		     Write_Indent_Str (Ob, " *");
		  when Ada_Comment_Type =>
		     Write_Indent_Str (Ob, "--");
	       end case;
	    end if;
	    
	    while Src (Scn) not in Line_Terminator loop
	       Write_Char (Ob, Src (Scn));
	       Scn := Scn + 1;
	    end loop;
	    
	    First_Line := False;
	    
	    Last_Line_Printed := Last_Line_Printed + 1;
	    
	    exit when Last_Line_Printed > To 
	      or else not Is_Comment_Line
	      (Last_Line_Printed, Current_Source_File);
	    
	    if Multi_Line then
	       Write_Eol (Ob);
	    end if;
	 end loop;
	 
	 -- Emit the closed comment line string
	 
	 case Comment_Type is
	    when No_Lead_Character =>
	       Write_Indent_Str (Ob, "");
	    when Iec_Comment_Type =>
	       Write_Indent_Str (Ob, " *)");
	    when C_Comment_Type =>
	       Write_Indent_Str (Ob, " */");
	    when Ada_Comment_Type =>
	       Write_Indent_Str (Ob, " ");
	 end case;
	 
	 Write_Eol (Ob);
      end Emit_Comment_Lines;
      
      --  Start of Write_Comment_Line
      
   begin
      Current_Source_File := This.Get_Current_Source_File;
      Src                 := Source_Text (Current_Source_File);
      Last_Line_Printed   := This.Get_Last_Line_Printed;
      
      while Last_Line_Printed <= To loop
	 
	 if Is_Blank_Line (Last_Line_Printed, Current_Source_File) then
	    Emit_Or_Skip_Blank_Lines;
	    
	 elsif Is_Comment_Line (Last_Line_Printed, Current_Source_File) then
	    Emit_Comment_Lines;
	    
	 else
	    Last_Line_Printed := Last_Line_Printed + 1;
	 end if;
      end loop;
      
      This.Set_Last_Line_Printed (Last_Line_Printed);
   end Write_Comment_Line;
   
   -------------------------------
   -- Skip_Comment_Line_To_Node --
   ------------------------------
   
   procedure Skip_Comment_Line_To_Node 
     (This : access Generator_Record'Class;
      Node : Node_Id) is
      
      S  : Source_Ptr := Sloc (Node);
      To : Physical_Line_Number;
   begin
      if Comes_From_Source (Node) then
	 if not In_Instantiation (S) 
	   and then Get_Source_File_Index (S) = This.Get_Current_Source_File
	 then
	    To := Get_Physical_Line_Number (S);
	    This.Set_Last_Line_Printed (To);
	 end if;
      end if;
   end Skip_Comment_Line_To_Node;
   
end Reflex.Gen.Outputs;
