------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 2012-2015, Free Software Foundation, Inc.         --
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

with Artics.Types;    use Artics.Types;
with Artics.Namet;    use Artics.Namet;
with Artics.Uintp;    use Artics.Uintp;
with Artics.Urealp;   use Artics.Urealp;
with Artics.Widechar; use Artics.Widechar;
with artics.Stringt; use Artics.Stringt;

generic
   type Token_Type is (<>);

package Artics.Generic_Tokens is
   
   type Token_Record is tagged private;

   No_Token_Record : constant Token_Record;

   type Token_Entry_Record is record
      Token_Name   : Name_Id;
      Tok_Position : Integer;
   end record;
   type Token_Array is array (Token_Type) of Token_Entry_Record;
   
   procedure Initialize (T : in Token_Array);

   procedure Free (T : in Token_Array);

   --procedure Free_Tokens;
   
   function Get_Token_Type (T : Token_Record) return Token_Type;
   
   procedure Set_Token_Type
     (T   : in out Token_Record;
      Typ : Token_Type);
   
   function Type_Token_Location (T : Token_Record) return Natural;
   
   procedure Set_Type_Token_Location
     (T : in out Token_Record; 
      S : Natural);
   
   function Token_Node (T : Token_Record) return Node_Id;
   
   procedure Set_Token_Node
     (T : in out Token_Record;
      N : Node_Id);
   
   function Token_Name (T : Token_Record) return Name_Id;
   
   procedure Set_Token_Name
     (T : in out Token_Record;
      N : Name_Id);
   
   function Character_Code (T : Token_Record) return Char_Code;
   
   procedure Set_Character_Code
     (T : in out Token_Record;
      C : Char_Code);
   
   function Real_Literal_Value (T : Token_Record) return Ureal;
   
   procedure Set_Real_Literal_Value
     (T : in out Token_Record;
      V : Ureal);
   
   function Int_Literal_Value (T : Token_Record) return Uint;
   
   procedure Set_Int_Literal_Value
     (T : in out Token_Record;
      V : Uint);
   
   function String_Literal_Id (T : Token_Record) return String_Id;
   
   procedure Set_String_Literal_Id
     (T : in out Token_Record;
      S : String_Id);
   
   function Wide_Character_Found (T : Token_Record) return Boolean;
   
   procedure Set_Wide_Character_Found
     (T : in out Token_Record;
      W : Boolean);

   function Wide_Wide_Character_Found (T : Token_Record) return Boolean;
   
   procedure Set_Wide_Wide_Character_Found
     (T : in out Token_Record;
      W : Boolean);

   procedure Set_Token_Infos
     (Tok      : in out Token_Record;
      Tok_Type : Token_Type;
      Loc      : Natural;
      Name     : Name_Id);

   procedure Set_Token_Infos
     (T                         : in out Token_Record;
      Token                     : Token_Type := Token_Type'First;
      Type_Token_Location       : Natural    := 0; -- No_Location;
      Token_Node                : Node_Id    := No_Node;
      Token_Name                : Name_Id    := No_Name;
      Character_Code            : Char_Code  := Char_Code'First;
      Real_Literal_Value        : Ureal      := No_Ureal;
      Int_Literal_Value         : Uint       := No_Uint;
      String_Literal_Id         : String_Id  := No_String;
      Wide_Character_Found      : Boolean    := False;
      Wide_Wide_Character_Found : Boolean    := False);
   
private

   type Token_Record is tagged record
      
      Token : Token_Type;
      --  Type of current token
      
      Type_Token_Location : Natural;
      -- Within a type declaration, gives the location of the TYPE keyword that
      -- opened the type declaration. Used in checking the end column of a
      -- record declaration, which can line up either with the TYPE keyword, or
      -- with the start of the line containing the RECORD keyword.

      Token_Node : Node_Id;
      --  Node table Id for the current token. This is set only if the current
      --  token is one for which the scanner constructs a node (i.e. it is an
      --  identifier, operator symbol, or literal. For other token types,
      --  Token_Node is undefined.
      
      Token_Name : Name_Id;
      --  For identifiers, this is set to the Name_Id of the identifier scanned.
      --  For all other tokens, Token_Name is set to Error_Name. Note that it
      --  would be possible for the caller to extract this information from
      --  Token_Node. We set Token_Name separately for two reasons. First it
      --  allows a quicker test for a specific identifier. Second, it allows
      --  a version of the parser to be built that does not build tree nodes,
      --  usable as a syntax checker.
      
      Character_Code : Char_Code;
      --  Valid only when Token is Tok_Char_Literal. Contains the value of the
      --  scanned literal.
      
      Real_Literal_Value : Ureal;
      --  Valid only when Token is Tok_Real_Literal, contains the value of the
      --  scanned literal.
      
      Int_Literal_Value : Uint;
      --  Valid only when Token = Tok_Integer_Literal, contains the value of the
      --  scanned literal.
      
      String_Literal_Id : String_Id;
      --  Valid only when Token = Tok_String_Literal or Tok_Operator_Symbol.
      --  Contains the Id for currently scanned string value.
      
      Wide_Character_Found : Boolean;
      --  Valid only when Token = Tok_String_Literal. Set True if wide character
      --  found (i.e. a character that does not fit in Character, but fits in
      --  Wide_Wide_Character).
      
      Wide_Wide_Character_Found : Boolean;
      --  Valid only when Token = Tok_String_Literal. Set True if wide wide
      --  character found (i.e. a character that does not fit in Character or
      --  Wide_Character).
      
   end record;
   
   No_Token_Record : constant Token_Record := 
     (Token                     => Token_Type'First,
      Type_Token_Location       => 0, -- No_Location,
      Token_Node                => No_Node,
      Token_Name                => No_Name,
      Character_Code            => 0,
      Real_Literal_Value        => No_Ureal,
      Int_Literal_Value         => No_Uint,
      String_Literal_Id         => No_String,
      Wide_Character_Found      => False,
      Wide_Wide_Character_Found => False
     );
   
end Artics.Generic_Tokens;

