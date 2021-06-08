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

with Namet; use Namet;
with Uintp; use Uintp;
with Urealp; use Urealp;

with Artics.Buffers; use Artics.Buffers;

with Reflex.Generators; use Reflex.Generators;

package Reflex.Gen.Outputs is
   
   type Plc_Comment_Type is
     (No_Lead_Character,
      Iec_Comment_Type,
      C_Comment_Type,
      Ada_Comment_Type);
   
   Comment_Type             : Plc_Comment_Type := Iec_Comment_Type;
   Skip_Leading_Blank_Lines : Boolean := False;
   
   function Is_Unity_Keyword (Name : Name_Id) return Boolean;
   --  Return True if Name is a Unity keyword
   
   function Is_Qualified (Name : Name_Id) return Boolean;
   --  Return True if Name is already fully qualified
   
   function Remove_Qualified (Name : Name_Id) return String;
   
   function Float_To_String (Value : Float) return String;
   
   procedure Write_C_Char_Code 
     (Ob : Output_Buffer;
      CC : Char_Code);
   --  Write a given character in a suitable form for the C language.

   procedure Write_Id
     (Ob : Output_Buffer;
      N  : Node_Id);
   --  N is a node with a Chars field. This procedure writes the name that
   --  will be used in the generated code associated with the name. For a
   --  node with no associated entity, this is simply the Chars field. For
   --  the case where there is an entity associated with the node, we print
   --  the name associated with the entity (since it may have been encoded).
   --  One other special case is that an entity has an active external name
   --  (i.e. an external name present with no address clause), then this
   --  external name is output. This procedure also deals with outputting
   --  declarations of referenced itypes, if not output earlier.

   procedure Write_Integer_Type
     (Ob     : Output_Buffer;
      Siz    : Int; 
      Signed : Boolean);
   --  Output an integer type given the size Siz in bits, rounded to the next
   --  power of two (8, 16, 32, 64). If Signed, reference integer_xx, otherwise
   --  reference unsigned_xx.

   procedure Write_Name
     (Ob : Output_Buffer;
      N  : Name_Id);
   
   procedure Write_Source_Lines
     (Ob   : Output_Buffer;
      From : Physical_Line_Number;
      To   : Physical_Line_Number);
   --  From, To are the start/end physical line numbers for the construct
   --  whose C translation is about to be printed. This routine takes care of
   --  generating required #line directives, and also in Dump_Source_Text mode,
   --  prints non-comment source Ada lines as C comments.

   procedure Write_Source_Lines 
     (Ob : Output_Buffer;
      N  : Node_Id);
   --  Same, but From, To are First_Line, Last_Line of node N

   procedure Write_Source_Lines
     (Ob : Output_Buffer;
      S  : Source_Ptr);
   --  Same, but From and To both correspond to the given Source_Ptr value

   procedure Write_Source_Lines
     (Ob   : Output_Buffer;
      From : Source_Ptr; 
      To   : Physical_Line_Number);
   --  Same, but From is line corresponding to given source_Ptr value.
   
   procedure Write_Source_Comment_Lines
     (Ob : Output_Buffer;
      S  : Source_Ptr);
   procedure Write_Source_Comment_Lines
     (Ob   : Output_Buffer;
      From : Physical_Line_Number;
      To   : Physical_Line_Number);
   
   procedure Write_Str_Col_Check
     (Ob : Output_Buffer;
      S  : String);
   --  Write string (using Write_Str) with initial column check, and possible
   --  initial Write_Indent (to get new line) if current line is too full.

   procedure Write_Uint
     (Ob           : Output_Buffer;
      U            : Uint;
      Column_Check : Boolean := True;
      Modular      : Boolean := False);
   --  Write Uint.
   --  If Column_Check is True, perform initial column check and possible
   --  initial Write_Indent (to get new line) if current line is too full.
   --  The output is always in decimal. Takes care of special cases of the
   --  largest negative number, and possible long integer output.
   --  If Modular is True, output Uint as an unsigned C integer.

   procedure Write_Ureal_Col_Check
     (Ob : Output_Buffer;
      U  : Ureal);
   --  Write Ureal with column checks and a possible initial Write_Indent (to
   --  get new line) if current line is too full.
   
   procedure Write_Comment_Line_To_Node 
     (This   : access Generator_Record'Class;
      Node   : Node_Id;
      Buffer : Output_Buffer := null);
   
   procedure Write_Comment_Line
     (This : access Generator_Record'Class;
      To   : Physical_Line_Number;
      Ob   : Output_Buffer);
   
   procedure Skip_Comment_Line_To_Node 
     (This : access Generator_Record'Class;
      Node : Node_Id);
   
end Reflex.Gen.Outputs;
