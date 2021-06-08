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

with Unchecked_Deallocation;

package body Artics.Generic_Tokens is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (T : in Token_Array) is
   begin
      for I in Token_Type loop
         Set_Name_Table_Int (T (I).Token_Name, T (I).Tok_Position);
      end loop;
   end Initialize;

   ----------
   -- Free --
   ----------

   procedure Free (T : in Token_Array) is
   begin
      for I in Token_Type loop
         Set_Name_Table_Int (T (I).Token_Name, 0);
      end loop;
   end Free;

   --------------------
   -- Get_Token_Type --
   --------------------
   
   function Get_Token_Type (T : Token_Record) return Token_Type is
   begin
      return T.Token;
   end Get_Token_Type;
   
   --------------------
   -- Set_Token_Type --
   --------------------
   
   procedure Set_Token_Type
     (T   : in out Token_Record;
      Typ : Token_Type) is
   begin
      T.Token := Typ;
   end Set_Token_Type;
   
   -------------------------
   -- Type_Token_Location --
   -------------------------
   
   function Type_Token_Location (T : Token_Record) return Natural is
   begin
      return T.Type_Token_Location;
   end Type_Token_Location;
   
   -----------------------------
   -- Set_Type_Token_Location --
   -----------------------------
   
   procedure Set_Type_Token_Location
     (T : in out Token_Record; 
      S : Natural) is
   begin
      T.Type_Token_Location := S;
   end Set_Type_Token_Location;
   
   ----------------
   -- Token_Node --
   ----------------
   
   function Token_Node (T : Token_Record) return Node_Id is
   begin
      return T.Token_Node;
   end Token_Node;
   
   --------------------
   -- Set_Token_Node --
   --------------------
   
   procedure Set_Token_Node
     (T : in out Token_Record;
      N : Node_Id) is
   begin
      T.Token_Node := N;
   end Set_Token_Node;
   
   ----------------
   -- Token_Name --
   ----------------
   
   function Token_Name (T : Token_Record) return Name_Id is
   begin
      return T.Token_Name;
   end Token_Name;
   
   --------------------
   -- Set_Token_Name --
   --------------------
   
   procedure Set_Token_Name
     (T : in out Token_Record;
      N : Name_Id) is
   begin
      T.Token_Name := N;
   end Set_Token_Name;
   
   --------------------
   -- Character_Code --
   --------------------
   
   function Character_Code (T : Token_Record) return Char_Code is
   begin
      return T.Character_Code;
   end Character_Code;
   
   ------------------------
   -- Set_Character_Code --
   ------------------------
   
   procedure Set_Character_Code
     (T : in out Token_Record;
      C : Char_Code) is
   begin
      T.Character_Code := C;
   end Set_Character_Code;
   
   ------------------------
   -- Real_Literal_Value --
   ------------------------
   
   function Real_Literal_Value (T : Token_Record) return Ureal is
   begin
      return T.Real_Literal_Value;
   end Real_Literal_Value;
   
   ----------------------------
   -- Set_Real_Literal_Value --
   ----------------------------
   
   procedure Set_Real_Literal_Value
     (T : in out Token_Record;
      V : Ureal) is
   begin
      T.Real_Literal_Value := V;
   end Set_Real_Literal_Value;
   
   -----------------------
   -- Int_Literal_Value --
   -----------------------
   
   function Int_Literal_Value (T : Token_Record) return Uint is
   begin
      return T.Int_Literal_Value;
   end Int_Literal_Value;
   
   ---------------------------
   -- Set_Int_Literal_Value --
   ---------------------------
   
   procedure Set_Int_Literal_Value
     (T : in out Token_Record;
      V : Uint) is
   begin
      T.Int_Literal_Value := V;
   end Set_Int_Literal_Value;
   
   -----------------------
   -- String_Literal_Id --
   -----------------------
   
   function String_Literal_Id (T : Token_Record) return String_Id is
   begin
      return T.String_Literal_Id;
   end String_Literal_Id;
   
   ---------------------------
   -- Set_String_Literal_Id --
   ---------------------------
   
   procedure Set_String_Literal_Id
     (T : in out Token_Record;
      S : String_Id) is
   begin
      T.String_Literal_Id := S;
   end Set_String_Literal_Id;
   
   --------------------------
   -- Wide_Character_Found --
   --------------------------
   
   function Wide_Character_Found (T : Token_Record) return Boolean is
   begin
      return T.Wide_Character_Found;
   end Wide_Character_Found;
   
   ------------------------------
   -- Set_Wide_Character_Found --
   ------------------------------
   
   procedure Set_Wide_Character_Found
     (T : in out Token_Record;
      W : Boolean) is
   begin
      T.Wide_Character_Found := W;
   end Set_Wide_Character_Found;
   
   -------------------------------
   -- Wide_Wide_Character_Found --
   -------------------------------

   function Wide_Wide_Character_Found (T : Token_Record) return Boolean is
   begin
      return T.Wide_Wide_Character_Found;
   end Wide_Wide_Character_Found;
   
   -----------------------------------
   -- Set_Wide_Wide_Character_Found --
   -----------------------------------
   
   procedure Set_Wide_Wide_Character_Found
     (T : in out Token_Record;
      W : Boolean) is
   begin
      T.Wide_Wide_Character_Found := W;
   end Set_Wide_Wide_Character_Found;
   
   ---------------------
   -- Set_Token_Infos --
   ---------------------

   procedure Set_Token_Infos
     (Tok      : in out Token_Record;
      Tok_Type : Token_Type;
      Loc      : Natural;
      Name     : Name_Id) is
   begin
      Tok := No_Token_Record;
      
      Tok.Token               := Tok_Type;
      Tok.Type_Token_Location := Loc;
      Tok.Token_Name          := Name;
   end Set_Token_Infos;
   
   ---------------------
   -- Set_Token_Infos --
   ---------------------

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
      Wide_Wide_Character_Found : Boolean    := False)
   is
   begin
      T.Token                     := Token;
      T.Type_Token_Location       := Type_Token_Location;
      T.Token_Node                := Token_Node;
      T.Token_Name                := Token_Name;
      T.Character_Code            := Character_Code;
      T.Real_Literal_Value        := Real_Literal_Value;
      T.Int_Literal_Value         := Int_Literal_Value;
      T.String_Literal_Id         := String_Literal_Id;
      T.Wide_Character_Found      := Wide_Character_Found;
      T.Wide_Wide_Character_Found := Wide_Wide_Character_Found;
   end Set_Token_Infos;
    
end Artics.Generic_Tokens;

