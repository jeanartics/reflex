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

with Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Artics.Properties_Lists is
   
   use Priv_Properties_Sets;

   ---------
   -- "=" --
   ---------

   function "="
     (P1 : Property;
      P2 : Property) return Boolean is
   begin
      return P1.Name = P2.Name;
   end "=";

   --------------
   -- To_Lower --
   --------------

   function To_Lower (Name : in Name_Id) return Name_Id is
   begin
      return String_Find (To_Lower (Get_String (Name)));
   end To_Lower;

   ---------------------
   -- Insert_Property --
   ---------------------

   procedure Insert_Property
     (L : in Properties_List;
      P : in Name_Id;
      V : in Property_Data) is

      Pro : Property :=  Property'(To_Lower (P), V);
   begin
      Append (Pro, L, True);
   end Insert_Property;

   ---------------------
   -- Insert_Property --
   ---------------------

   procedure Insert_Property
     (L : in Properties_List;
      P : in String;
      V : in Property_Data) is

   begin
      Insert_Property (L, String_Find (P), V);
   end Insert_Property;

   -------------------
   -- Read_Property --
   -------------------

   function Read_Property
     (L : in Properties_List;
      P : in Name_Id) return Property_Data is

      N    : Name_Id := To_Lower (P);
      It   : Set_Iterator := New_Iterator (L);
      Pcur : Property;
   begin
      Reset (It);

      while not Is_End (It) loop
         Pcur := Current_Item (It);
         exit when Pcur.Name = N;
         Next(It);
      end loop;

      if Is_End (It) then
         return Null_Property_Data;
      else
         return Pcur.Data;
      end if;
   end Read_Property;

   -----------------
   -- Is_Property --
   -----------------

   function Is_Property
     (L : in Properties_List;
      P : in Name_Id) return Boolean is

      N    : Name_Id := To_Lower (P);
      It   : Set_Iterator := New_Iterator (L);
      Pcur : Property;
   begin
      Reset (It);
      while not Is_End (It) loop
         Pcur := Current_Item (It);
         exit when Pcur.Name = N;
         Next(It);
      end loop;

      if Is_End (It) then
         return False;
      else
         return True;
      end if;
   end Is_Property;

   -------------------
   -- Read_Property --
   -------------------

   function Read_Property
     (L : in Properties_List;
      P : in String) return Property_Data is
   begin
     return Read_Property (L, String_Find (P));
   end Read_Property;

   ---------------------
   -- Delete_Property --
   ---------------------

   procedure Delete_Property
     (L : in Properties_List;
      P : in Name_Id;
      F : in Boolean := True)
   is
   begin
      if L /= No_Properties and then P /= No_Name then
         declare
            N    : Name_Id := To_Lower (P);
            It   : Set_Iterator := New_Iterator (L);
            Pcur : Property;
         begin
            Reset (It);
            while not Is_End (It) loop
               Pcur := Current_Item (It);
               if Pcur.Name = N then
                  if F then
                     Free(PCur.Data);
                  end if;
                  Remove (L, Pcur);
                  exit;
               end if;

               Next(It);
            end loop;
         end;
      end if;
   end Delete_Property;

   ---------------------
   -- Delete_Property --
   ---------------------

   procedure Delete_Property
     (L : in Properties_List;
      P : in String;
      F : in Boolean := True)
   is
   begin
      Delete_Property(L, String_Find(P), F);
   end Delete_Property;

   -----------
   -- Empty --
   -----------

   function Empty return Properties_List is
   begin
      return Properties_List(Priv_Properties_Sets.New_Set);
   end Empty;

   ----------------------
   -- Merge_Properties --
   ----------------------

   procedure Merge_Properties
     (Into : in Properties_List;
      List : in out Properties_List) is
   begin
      Append_Set (List, Into);
   end Merge_Properties;

   -----------------------
   -- Delete_Properties --
   -----------------------

   procedure Delete_Properties (L : in out Properties_List) is
   begin
      if L /= No_Properties then
         declare
            It : Set_Iterator := New_Iterator (Set_Id (L));
         begin
            Reset (It);
            while not Is_End (It) loop
               Remove_Current_Item (It);
            end loop;

            Delete_Set (Set_Id (L));
         end;
      end if;
   end Delete_Properties;

   -----------
   -- Reset --
   -----------

   procedure Reset (It : in out Property_Iterator) is
   begin
      Priv_Properties_Sets.Reset (It.It);
   end Reset;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Property_Iterator) is
   begin
      Priv_Properties_Sets.Next (It.It);
   end Next;

   ------------------
   -- Current_Item --
   ------------------

   function Current_Item (It : in Property_Iterator) return Property is
   begin
      return Priv_Properties_Sets.Current_Item (It.It);
   end Current_Item;

   ------------
   -- Is_End --
   ------------

   function Is_End (It : in Property_Iterator) return Boolean is
   begin
      return Priv_Properties_Sets.Is_End (It.It);
   end Is_End;

   ---------------------------
   -- New_Property_Iterator --
   ---------------------------

   function New_Property_Iterator
     (L : in Properties_List) return Property_Iterator is

      It  : Set_Iterator := New_Iterator (L);
      Pit : Property_Iterator := Property_Iterator'(It => It);
   begin
      return Pit;
   end New_Property_Iterator;

   -----------------------
   -- Get_Property_Name --
   -----------------------

   function Get_Property_Name (P : in Property) return Name_Id is
   begin
      return P.Name;
   end Get_Property_Name;

   -----------------------
   -- Get_Property_Data --
   -----------------------

   function Get_Property_Data (P : in Property) return Property_Data is
   begin
      return P.Data;
   end Get_Property_Data;

end Artics.Properties_Lists;
