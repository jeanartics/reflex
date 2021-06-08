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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Gnat.Case_Util; use Gnat.Case_Util;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Atree;    use Atree;
--with Checks;   use Checks;
with Csets;    use Csets;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
---with Exp_Tss;  use Exp_Tss;
--with Exp_Unst; use Exp_Unst;
with Exp_Util; use Exp_Util;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Osint;    use Osint;
with Osint.C;  use Osint.C;
with Output;   use Output;
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
with Gnat.HTable; use Gnat.HTable;

with Reflex.Gen.Types; use Reflex.Gen.Types;
--with Reflex.Gen.Ada_Outputs; use Reflex.Gen.Ada_Outputs;
-- with Unity.Gen.Dispatch; use Unity.Gen.Dispatch;

with Reflex.Gen.Dispatch_Interface; use Reflex.Gen.Dispatch_Interface;

package body Reflex.Gen.Utils is
   
   FLCache_N  : Node_Id := Empty;
   FLCache_FL : Physical_Line_Number;
   FLCache_LL : Physical_Line_Number;
   --  Cache for First_Line and Last_Line (N records last node for which any
   --  of these subprograms were called, FL and LL record the corresponding
   --  First and Last physical line numbers for this node).
   
   ------------
   -- To_XML --
   ------------
   
   function To_XML (S : in String) return String is
      Xml_Str : Unbounded_String := Null_Unbounded_String;
   begin
      for I in S'Range loop
         case S (I) is
            when '<' =>
               Append (Xml_Str, "&lt;");

            when '>' =>
               Append (Xml_Str, "&gt;");

            when '&' =>
               Append (Xml_Str, "&amp;");

            when '"' =>
               Append (Xml_Str, "&quot;");

            when ''' =>
               Append (Xml_Str, "&apos;");
               
--              when Sub =>
--                 Append (Xml_Str, ' ');

            when others =>
               Append (Xml_Str, S (I));

         end case;
      end loop;

      return To_String (Xml_Str);
   end To_XML;

   -------------
   -- Unquote --
   -------------

   function Unquote (S : in String) return String is
      Unq : String := S;
   begin
      for I in Unq'Range loop
         if Unq (I) = '"' then
            Unq (I) := ' ';
         end if;
      end loop;
      
      return Unq;
   end Unquote;
   
   -----------------------
   -- Normalize_Comment --
   -----------------------

   function Normalize_Comment (S : in String) return String is
      
      St : String := Trim (To_Xml (Unquote (To_Plc_Comment (S))), Both);
   begin
      -- Reduce comment's size due to undocumented Plc limitation (1024 bytes)
      
      if St'length > 1023 then
         declare
            minimized_String    : String := St(St'first .. St'first + 1020);
            last_space_position : Integer := minimized_String'last;
         begin
            last_space_position := Index (Source  => minimized_String,
                                          Pattern => " ",
                                          Going   => Ada.Strings.Backward);
            return minimized_String
              (minimized_String'first .. last_space_position) & "...";
         end;
      end if;
      return St;

   end Normalize_Comment;
   
   --------------------
   -- To_Plc_Comment --
   --------------------
   
   function To_Plc_Comment (S : in String) return String is
      
      Nc         : Unbounded_String := Null_Unbounded_String;
      I          : Positive := S'First;
      Start_Line : Boolean := True;

      use Ascii;
   begin
      while I in S'Range loop
	 
         if (I+1) in S'Range 
	   and then S (I) = '-' 
	   and then S (I+1) = '-' 
	 then
            if not Start_Line then
               Append (Nc, S (I));
	       I := I + 1;
            end if;
            Start_Line := False;

         elsif S (I) = Ascii.Lf then
            Append (Nc, CR);
            Append (Nc, LF);

            if (I+1) in S'Range and then S (I+1) = Ascii.Cr then
               I := I +1;
            end if;
            Start_Line := True;

         elsif S (I) = Ascii.Cr then

            Append (Nc, CR);
            Append (Nc, LF);

            if (I+1) in S'Range and then S (I+1) = Ascii.Lf then
               I := I + 1;
            end if;
            Start_Line := True;

         else
            Append (Nc, S (I));
            Start_Line := False;
         end if;
         I := I + 1;
      end loop;

      return To_String (Nc);
   end To_Plc_Comment;

   ------------------------
   -- To_Plc_Var_Comment --
   ------------------------

   function To_Plc_Var_Comment (S : in String) return String is

      Nc         : Unbounded_String := Null_Unbounded_String;
      I          : Positive := S'First;
      Start_Line : Boolean  := True;

      use Ascii;
   begin
      while I in S'Range loop
         exit when I > 80;
	 
	 --  Skip "--"
	 
         if S (I) = '-' then 
            if (I+1) in S'Range and then S (I+1) = '-' then
	       if Start_Line then
		  I := I + 1;
	       else
		  Append (Nc, S (I));
	       end if;
            else
               Append (Nc, S (I));
            end if;

            Start_Line := False;

         elsif S (I) = Ascii.Lf or else S (I) = Ascii.Cr then
            Append (Nc, " ");
            Start_Line := True;

         else
            Append (Nc, S (I));
            Start_Line := False;
         end if;
	 
         I := I + 1;
      end loop;

      return To_String (Nc);
   end To_Plc_Var_Comment;
   
   -----------------------------
   -- Append_Subpogram_Prefix --
   -----------------------------

   procedure Append_Subprogram_Prefix (Spec : Node_Id) is
      function Name_String (Name : Name_Id) return String;
      --  Returns the name string associated with Name

      function New_Name_Id (Name : String) return Name_Id;
      --  Returns a Name_Id corresponding to the given name string

      -----------------
      -- Name_String --
      -----------------

      function Name_String (Name : Name_Id) return String is
      begin
         pragma Assert (Name /= No_Name);
         return Get_Name_String (Name);
      end Name_String;

      -----------------
      -- New_Name_Id --
      -----------------

      function New_Name_Id (Name : String) return Name_Id is
      begin
         for J in 1 .. Name'Length loop
            Name_Buffer (J) := Name (Name'First + (J - 1));
         end loop;

         Name_Len := Name'Length;
         return Name_Find;
      end New_Name_Id;

      --  Local variables

      Subp : constant Entity_Id := Unique_Defining_Entity (Spec);

   --  Start of processing for Append_Subprogram_Prefix

   begin
      if Is_Compilation_Unit (Subp) then
         declare
            Prefix    : constant String := "_ada_";
            Subp_Name : Name_Id := Chars (Subp);
            Subp_Str  : constant String := Name_String (Subp_Name);

         begin
            --  Do not append the prefix if already done as part of processing
            --  its declaration.

            if Subp_Str'Length <= Prefix'Length
              or else
                Subp_Str (Subp_Str'First ..
                          Subp_Str'First + Prefix'Length - 1) /= Prefix
            then
               Subp_Name := New_Name_Id ("_ada_" & Name_String (Subp_Name));
               Set_Chars (Subp, Subp_Name);
            end if;
         end;
      end if;
   end Append_Subprogram_Prefix;

   ----------------
   -- Check_Sloc --
   ----------------

   function Check_Sloc (S : Source_Ptr) return Boolean is
   begin
      return
        not In_Instantiation (S)
	and then Get_Source_File_Index (S) = Current_Source_File;
   end Check_Sloc;

   ---------------
   -- Col_Check --
   ---------------

   procedure Col_Check
     (Ob : Output_Buffer;
      N  : Nat) is
      Sprint_Line_Limit : Int := 120;
   begin
      if N + Column > Sprint_Line_Limit then
         Write_Indent_Str (Ob, "  ");
      end if;
   end Col_Check;

   -----------------------------------
   -- Compound_Statement_Compatible --
   -----------------------------------

   function Compound_Statement_Compatible (L : List_Id) return Boolean is
      Result : Boolean := True;

      function Search_Complex_Node (Node : Node_Id) return Traverse_Result;
      --  Subtree visitor that looks for nodes incompatible with compound
      --  statements.

      -------------------------
      -- Search_Complex_Node --
      -------------------------

      function Search_Complex_Node (Node : Node_Id) return Traverse_Result is
      begin
         case Nkind (Node) is
            when N_Declaration | N_Statement_Other_Than_Procedure_Call =>
               if not Nkind_In (Node, N_Null_Statement, N_If_Statement) then
                  Result := False;
                  return Abandon;
               end if;

            when others =>
               return OK;
         end case;

         return OK;
      end Search_Complex_Node;

      procedure Search is new Traverse_Proc (Search_Complex_Node);
      --  Subtree visitor instantiation

      --  Local variables

      N : Node_Id;

   --  Start of processing for Compound_Statement_Compatible

   begin
      if Is_Non_Empty_List (L) then
         N := First (L);

         loop
            Search (N);
            Next (N);
            exit when N = Empty;
         end loop;
      end if;

      return Result;
   end Compound_Statement_Compatible;

   ---------------------
   -- Ensure_New_Line --
   ---------------------

   procedure Ensure_New_Line (Ob : Output_Buffer) is
   begin
      if Column /= 1 then
         Write_Eol (Ob);
      end if;

      Write_Indent (Ob);
   end Ensure_New_Line;

   ----------------
   -- First_Line --
   ----------------

   function First_Line (N : Node_Id) return Physical_Line_Number is
   begin
      Get_First_Last_Line (N);
      return FLCache_FL;
   end First_Line;

   ------------------------
   -- Get_Type_Full_View --
   ------------------------

   function Get_Type_Full_View (Id : Entity_Id) return Entity_Id is
   begin
      if Id /= Standard_Void_Type
        and then (Is_Type (Id) or else Ekind (Id) = E_Constant)
        and then Present (Full_View (Id))
      then
         return Full_View (Id);
      else
         return Id;
      end if;
   end Get_Type_Full_View;

   -------------------------
   -- Get_First_Last_Line --
   -------------------------

   procedure Get_First_Last_Line (N : Node_Id) is
      Loc        : constant Source_Ptr := Sloc (N);
      First_Sloc : Source_Ptr;
      Last_Sloc  : Source_Ptr;

      function Process (N : Node_Id) return Traverse_Result;
      --  Process function for traversal

      procedure Traverse is new Traverse_Proc (Process);

      -------------
      -- Process --
      -------------

      function Process (N : Node_Id) return Traverse_Result is
         Loc : constant Source_Ptr := Sloc (N);

      begin
         if Loc > No_Location
           and then Get_Source_File_Index (Loc) = Current_Source_File
         then
            if First_Sloc = No_Location or else Loc < First_Sloc then
               First_Sloc := Loc;
            end if;

            if Last_Sloc = No_Location or else Loc > Last_Sloc then
               Last_Sloc := Loc;
            end if;
         end if;

         return OK;
      end Process;

   --  Start of processing for Get_First_Last_Line

   begin
      --  Nothing to do if this is cached value

      if N = FLCache_N then
         return;
      else
         FLCache_N := N;
      end if;

      --  If not from current source file, or no source location available,
      --  then set no line number results

      if Loc <= No_Location
        or else Get_Source_File_Index (Loc) /= Current_Source_File
      then
         FLCache_FL := No_Physical_Line_Number;
         FLCache_LL := No_Physical_Line_Number;
         return;
      end if;

      --  Otherwise do the traversal

      First_Sloc := No_Location;
      Last_Sloc  := No_Location;
      Traverse (N);

      if First_Sloc = No_Location then
         FLCache_FL := No_Physical_Line_Number;
      else
         FLCache_FL := Get_Physical_Line_Number (First_Sloc);
      end if;

      if Last_Sloc = No_Location then
         FLCache_LL := No_Physical_Line_Number;
      else
         FLCache_LL := Get_Physical_Line_Number (Last_Sloc);
      end if;

      FLCache_N := N;
   end Get_First_Last_Line;

   -----------------------------
   -- Has_Non_Null_Statements --
   -----------------------------

   function Has_Non_Null_Statements (L : List_Id) return Boolean is
      
      Node : Node_Id;
   begin
      if Is_Non_Empty_List (L) then
         Node := First (L);

         loop
            if Nkind (Node) /= N_Null_Statement then
               return True;
            end if;

            Next (Node);
            exit when Node = Empty;
         end loop;
      end if;

      return False;
   end Has_Non_Null_Statements;

   -------------------------------------
   -- Has_Or_Inherits_Enum_Rep_Clause --
   -------------------------------------

   function Has_Or_Inherits_Enum_Rep_Clause (E : Entity_Id) return Boolean is
      
      Typ    : Entity_Id := Get_Type_Full_View (E);
      Result : Boolean   := Has_Enumeration_Rep_Clause (Typ);
   begin
      while Get_Type_Full_View (Etype (Typ)) /= Typ loop
         Typ    := Get_Type_Full_View (Etype (Typ));
         Result := Result or Has_Enumeration_Rep_Clause (Typ);
      end loop;

      return Result;
   end Has_Or_Inherits_Enum_Rep_Clause;

   ------------------------
   -- Has_Same_Int_Value --
   ------------------------

   function Has_Same_Int_Value
     (Val1 : Node_Id;
      Val2 : Node_Id) return Boolean
   is
   begin
      return Compile_Time_Known_Value (Val1)
        and then Compile_Time_Known_Value (Val2)
        and then Expr_Value (Val1) = Expr_Value (Val2);
   end Has_Same_Int_Value;

   ----------
   -- Hash --
   ----------

   function Hash (N : Node_Id) return Header_Num is
   begin
      return Header_Num (1 + N mod Node_Id (Header_Num'Last));
   end Hash;

   ----------------------
   -- In_Instantiation --
   ----------------------

   function In_Instantiation (S : Source_Ptr) return Boolean is
      SI : constant Source_File_Index := Get_Source_File_Index (S);
   begin
      return Instantiation (SI) /= No_Location;
   end In_Instantiation;

   ---------------------------------------------
   -- Is_Enum_Literal_Of_Enclosing_Subprogram --
   ---------------------------------------------

   function Is_Enum_Literal_Of_Enclosing_Subprogram
     (E : Entity_Id) return Boolean
   is
   begin
      return Ekind (E) = E_Enumeration_Literal
        and then not Is_Library_Level_Entity (E)
        and then Present (Current_Subp_Entity)
        and then not Within_Scope (E, Current_Subp_Entity);
   end Is_Enum_Literal_Of_Enclosing_Subprogram;

   -------------------------------
   -- Is_Out_Mode_Access_Formal --
   -------------------------------

   function Is_Out_Mode_Access_Formal (E : Node_Id) return Boolean is
   begin
      return Is_Formal (E)
        and then Is_Access_Type (Etype (E))
        and then Ekind_In (E, E_In_Out_Parameter, E_Out_Parameter);
   end Is_Out_Mode_Access_Formal;

   ---------------------
   -- Is_Packed_Array --
   ---------------------

   function Is_Packed_Array (Typ : Entity_Id) return Boolean is
   begin
      return False;
--        Is_Array_Type (Typ)
--          and then Present (Packed_Array_Impl_Type (Typ));
   end Is_Packed_Array;

   ---------------------------------------
   -- Is_Supported_Variable_Size_Record --
   ---------------------------------------

   function Is_Supported_Variable_Size_Record
     (Typ : Entity_Id) return Boolean
   is
   begin
      return False;
   end Is_Supported_Variable_Size_Record;

   ----------------
   -- Last_Field --
   ----------------

   function Last_Field (Typ : Node_Id) return Node_Id is
      Field  : Node_Id := First_Entity (Typ);
      Result : Node_Id := Empty;

   begin
      while Present (Field) loop
         if Ekind (Field) in Object_Kind then
            Result := Field;
         end if;

         Next_Entity (Field);
      end loop;

      return Result;
   end Last_Field;

   ---------------
   -- Last_Line --
   ---------------

   function Last_Line (N : Node_Id) return Physical_Line_Number is
   begin
      Get_First_Last_Line (N);
      return FLCache_LL;
   end Last_Line;

   -------------------
   -- Parens_Needed --
   -------------------

   function Parens_Needed (N : Node_Id) return Boolean is
      P : constant Node_Id := Parent (N);
   begin
      if Nkind (P) = N_Assignment_Statement then
         return N /= Expression (P);
      else
         return True;
      end if;
   end Parens_Needed;

   ------------------
   -- Pass_Pointer --
   ------------------

   function Pass_Pointer (Ent : Entity_Id) return Boolean is
      Typ : constant Entity_Id := Get_Type_Full_View (Etype (Ent));
   begin
      if Is_Array_Type (Typ) then
         return False;
      else
      --  Pass "flexible arrays" (arrays whose size is determined by a
      --  discriminant) by reference.

         return Mechanism (Ent) = By_Reference;
      end if;
   end Pass_Pointer;

   -------------------------
   -- Ultimate_Expression --
   -------------------------

   function Ultimate_Expression (N : Node_Id) return Node_Id is
      Expr : Node_Id := N;

   begin
      while Nkind_In (Expr, N_Qualified_Expression,
                            N_Type_Conversion,
                            N_Unchecked_Type_Conversion)
      loop
         Expr := Expression (Expr);
      end loop;

      return Expr;
   end Ultimate_Expression;

   ----------------------
   -- Requires_Address --
   ----------------------

   function Requires_Address (Typ : Node_Id) return Boolean is
   begin
      return False;
 --       not Is_Array_Type (Typ)
--            or else (Is_Packed_Array (Typ)
--                      and then Is_Integer_Type (Packed_Array_Impl_Type (Typ)));
   end Requires_Address;

   -----------------------------
   -- Unimplemented_Attribute --
   -----------------------------

   procedure Unimplemented_Attribute
     (Ob      : Output_Buffer;
      N       : Node_Id;
      Attr    : Name_Id;
      Context : String := "")
   is
      Name : constant String := Get_Name_String (Attr);

   begin
      Error_Msg_Name_1 := Attr;

      if Context = "" then
         Error_Msg_N ("unsupported attribute%", N);
      else
--           Error_Msg_Strlen := Context'Length;
--           Error_Msg_String (1 .. Error_Msg_Strlen) := Context;
         Error_Msg_N ("unsupported attribute% in this context (~)", N);
      end if;

      Write_Str (Ob, "/* unsupported attribute: " & Name & " */");
   end Unimplemented_Attribute;
   
   -----------------
   -- Unqual_Conv --
   -----------------

   function Unqual_Conv (Expr : Node_Id) return Node_Id is
   begin
      --  Recurse to handle unlikely case of multiple levels of qualification
      --  and/or conversion.

      if Nkind_In (Expr, N_Qualified_Expression,
                         N_Type_Conversion,
                         N_Unchecked_Type_Conversion)
      then
         return Unqual_Conv (Expression (Expr));

      --  Normal case, not a qualified expression

      else
         return Expr;
      end if;
   end Unqual_Conv;
   
   -------------------------
   -- Get_Inner_Loop_Node --
   -------------------------
   
   function Get_Inner_Loop_Node (Node : Node_Id) return Node_Id is
      Par : Node_Id;
   begin
      Par := Node;
      while Present (Par) loop
	 if Nkind (Par) = N_Loop_Statement then
	    return Par;
	 end if;
	 Par := Parent (Par);
      end loop;
      
      return Empty;
   end Get_Inner_Loop_Node;
   
end Reflex.Gen.Utils;
