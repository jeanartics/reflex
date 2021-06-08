------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                S C A N S                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2014, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Artics.Namet; use Artics.Namet;
with Artics.Rx_Names; use Artics.Rx_Names;

package body Artics.Rx_Tokens is

   ---------------------
   -- Is_Keyword_Name --
   ---------------------
   
   function Is_Keyword_Name (N : Name_Id) return Boolean is
   begin
      return False;
   end Is_Keyword_Name;
   
   -----------------------------
   -- Initialize_Ada_Keywords --
   -----------------------------

   procedure Initialize_Ada_Keywords is
      procedure Set_Reserved (N : Name_Id; T : Token_Type);
      pragma Inline (Set_Reserved);
      --  Set given name as a reserved word (T is the corresponding token)

      ------------------
      -- Set_Reserved --
      ------------------

      procedure Set_Reserved (N : Name_Id; T : Token_Type) is
      begin
         --  Set up Token_Type values in Names table entries for reserved
         --  words. We use the Pos value of the Token_Type value. Note that
         --  Is_Keyword_Name relies on the fact that Token_Type'Val (0) is not
         --  a reserved word.

         Set_Name_Table_Byte (N, Token_Type'Pos (T));
      end Set_Reserved;

   --  Start of processing for Initialize_Ada_Keywords

   begin
      --  Establish reserved words

      Set_Reserved (Name_Abort,     Tok_Abort);
      Set_Reserved (Name_Abs,       Tok_Abs);
      Set_Reserved (Name_Abstract,  Tok_Abstract);
      Set_Reserved (Name_Accept,    Tok_Accept);
      Set_Reserved (Name_Access,    Tok_Access);
      Set_Reserved (Name_And,       Tok_And);
      Set_Reserved (Name_Aliased,   Tok_Aliased);
      Set_Reserved (Name_All,       Tok_All);
      Set_Reserved (Name_Array,     Tok_Array);
      Set_Reserved (Name_At,        Tok_At);
      Set_Reserved (Name_Begin,     Tok_Begin);
      Set_Reserved (Name_Body,      Tok_Body);
      Set_Reserved (Name_Case,      Tok_Case);
      Set_Reserved (Name_Constant,  Tok_Constant);
      Set_Reserved (Name_Declare,   Tok_Declare);
      Set_Reserved (Name_Delay,     Tok_Delay);
      Set_Reserved (Name_Delta,     Tok_Delta);
      Set_Reserved (Name_Digits,    Tok_Digits);
      
      Set_Reserved (Name_Do,        Tok_Do);
      Set_Reserved (Name_Else,      Tok_Else);
      Set_Reserved (Name_Elsif,     Tok_Elsif);
      Set_Reserved (Name_End,       Tok_End);
      Set_Reserved (Name_Entry,     Tok_Entry);
      Set_Reserved (Name_Exception, Tok_Exception);
      Set_Reserved (Name_Exit,      Tok_Exit);
      Set_Reserved (Name_For,       Tok_For);
      Set_Reserved (Name_Function,  Tok_Function);
      Set_Reserved (Name_Generic,   Tok_Generic);
      Set_Reserved (Name_Goto,      Tok_Goto);
      Set_Reserved (Name_If,        Tok_If);
      Set_Reserved (Name_In,        Tok_In);
      Set_Reserved (Name_Is,        Tok_Is);
      Set_Reserved (Name_Limited,   Tok_Limited);
      Set_Reserved (Name_Loop,      Tok_Loop);
      Set_Reserved (Name_Mod,       Tok_Mod);
      Set_Reserved (Name_New,       Tok_New);
      Set_Reserved (Name_Not,       Tok_Not);
      Set_Reserved (Name_Null,      Tok_Null);
      Set_Reserved (Name_Of,        Tok_Of);
      Set_Reserved (Name_Or,        Tok_Or);
      Set_Reserved (Name_Others,    Tok_Others);
      Set_Reserved (Name_Out,       Tok_Out);
      Set_Reserved (Name_Package,   Tok_Package);
      Set_Reserved (Name_Pragma,    Tok_Pragma);
      Set_Reserved (Name_Private,   Tok_Private);
      Set_Reserved (Name_Procedure, Tok_Procedure);
      Set_Reserved (Name_Protected, Tok_Protected);
      Set_Reserved (Name_Raise,     Tok_Raise);
      Set_Reserved (Name_Range,     Tok_Range);
      Set_Reserved (Name_Record,    Tok_Record);
      Set_Reserved (Name_Rem,       Tok_Rem);
      Set_Reserved (Name_Renames,   Tok_Renames);
      Set_Reserved (Name_Requeue,   Tok_Requeue);
      Set_Reserved (Name_Return,    Tok_Return);
      Set_Reserved (Name_Reverse,   Tok_Reverse);
      Set_Reserved (Name_Select,    Tok_Select);
      Set_Reserved (Name_Separate,  Tok_Separate);
      Set_Reserved (Name_Subtype,   Tok_Subtype);
      Set_Reserved (Name_Tagged,    Tok_Tagged);
      Set_Reserved (Name_Task,      Tok_Task);
      Set_Reserved (Name_Terminate, Tok_Terminate);
      Set_Reserved (Name_Then,      Tok_Then);
      Set_Reserved (Name_Type,      Tok_Type);
      Set_Reserved (Name_Until,     Tok_Until);
      Set_Reserved (Name_Use,       Tok_Use);
      Set_Reserved (Name_When,      Tok_When);
      Set_Reserved (Name_While,     Tok_While);
      Set_Reserved (Name_With,      Tok_With);
      Set_Reserved (Name_Xor,       Tok_Xor);

      --  Ada 2005 reserved words

      Set_Reserved (Name_Interface,    Tok_Interface);
      Set_Reserved (Name_Overriding,   Tok_Overriding);
      Set_Reserved (Name_Synchronized, Tok_Synchronized);

      --  Ada 2012 reserved words

      Set_Reserved (Name_Some, Tok_Some);
      
      --  Reflex Add
      --  Reactive reserved keywords
      
      Set_Reserved (Name_Reactive, Tok_Reactive);
      Set_Reserved (Name_Wait,     Tok_Wait);
      Set_Reserved (Name_Fork,     Tok_Fork);
      Set_Reserved (Name_Pause,    Tok_Pause);
      Set_Reserved (Name_Step,     Tok_Step);
      Set_Reserved (Name_Flow,     Tok_Flow);
      
   end Initialize_Ada_Keywords;

end Artics.Rx_Tokens;
