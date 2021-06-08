------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
--                                                                          --
-- Reflex is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public Licensea as published  by the Free Soft- --
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

with Reflex.Formats; use Reflex.Formats;

package body Reflex.External_Names is
   
   function New_Name_For
     (Name  : Name_Id;
      Pref  : String;
      Names : in out Names_Hash.Map) return Name_Id;
   
   --------------------
   -- Equivalent_Key --
   --------------------
   
   function Equivalent_Key (Left, Right : Name_Id) return Boolean is
   begin
      return Left = Right;
   end Equivalent_Key;
   
   ---------------
   -- Hash_Func --
   ---------------
   
   function Hash_Func
     (Key : Name_Id) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Key);
   end Hash_Func;
   
   ----------------------
   -- Is_Name_Internal --
   ----------------------
   
   function Is_Name_Internal (S : String) return Boolean is
   begin
      return S (S'First) = '_';
   end Is_Name_Internal;
   
   function Is_Name_Internal (Name : Name_Id) return Boolean is
      S : String := Get_String (Name);
   begin
      return Is_Name_Internal (S);
   end Is_Name_Internal;
   
   ------------------
   -- New_Name_For --
   ------------------
   
   function New_Name_For
     (Name  : Name_Id;
      Pref  : String;
      Names : in out Names_Hash.Map) return Name_Id is
      
      use Names_Hash;
      
      Sname : String := Get_String (Name);
      N     : Name_Id;
      Count : Natural := 0;
   begin
      declare
	 S : String := Pref & Sname;
      begin
	 if S'Length > Max_Name_Length then
	    N := String_Find
              (S (S'First .. S'First + (Max_Name_Length - 1)));
         else
            N := String_Find (S);
	 end if;
      end;
      
      loop
	 if Names.Contains (N) then
	    Count := Count + 1;
	 else
	    Insert (Names, N, True);
	    exit;
	 end if;
	 
	 declare
	    S : String := Pref & Integer_To_String (Count) & Sname;
	 begin
	    if S'Length > Max_Name_Length then
	       N := String_Find
		 (S (S'First .. S'First + (Max_Name_Length - 1)));
         else
            N := String_Find (S);
	    end if;
	 end;
      end loop;
      
      return N;
   end New_Name_For;
   
   -----------------------
   -- New_Internal_Name --
   -----------------------
   
   function New_Internal_Name return Name_Id is
      N : Name_Id := String_Find ("_rx" & Integer_To_String (Internal_Count));
   begin
      Internal_Count := Internal_Count + 1;
      return N;
   end New_Internal_Name;
   
   --------------------
   -- New_Ghost_Name --
   --------------------
   
   function New_Ghost_Name return Name_Id is
      N : Name_Id := String_Find ("_rxg" & Integer_To_String (Ghost_Count));
   begin
      Ghost_Count := Ghost_Count + 1;
      return N;
   end New_Ghost_Name;
   
   ------------------------
   -- New_Aggregate_Name --
   ------------------------
   
   function New_Aggregate_Name (S : String) return Name_Id is
   begin
      return New_Name_For (String_Find (S), Aggregate_Prefix, Aggregate_Names);
   end New_Aggregate_Name;
   
   function New_Aggregate_Name (Name : Name_Id) return Name_Id is
   begin
      return New_Name_For (Name, Aggregate_Prefix, Aggregate_Names);
   end New_Aggregate_Name;
   
   --------------------
   -- New_Array_Name --
   --------------------
   
   function New_Array_Name (S : String) return Name_Id is
   begin
      return New_Name_For (String_Find (S), Array_Prefix, Array_Names);
   end New_Array_Name;
   
   function New_Array_Name (Name : Name_Id) return Name_Id is
   begin
      return New_Name_For (Name, Array_Prefix, Array_Names);
   end New_Array_Name;
   
   -----------------------
   -- New_Variable_Name --
   -----------------------
   
   function New_Variable_Name (S : String) return Name_Id is
   begin
      return New_Name_For (String_Find (S), Variable_Prefix, Variable_Names);
   end New_Variable_Name;
   
   function New_Variable_Name (Name : Name_Id) return Name_Id is
   begin
      return New_Name_For (Name, Variable_Prefix, Variable_Names);
   end New_Variable_Name;
   
   --------------------
   -- New_Label_Name --
   --------------------
   
   function New_Label_Name (S : String) return Name_Id is
   begin
      return New_Name_For (String_Find (S), Label_Prefix, Label_Names);
   end New_Label_Name;
   
   function New_Label_Name (Name : Name_Id) return Name_Id is
   begin
      return New_Name_For (Name, Label_Prefix, Label_Names);
   end New_Label_Name;
   
   -------------------
   -- New_Type_Name --
   -------------------
   
   function New_Type_Name (S : String) return Name_Id is
   begin
      return New_Name_For (String_Find (S), Type_Prefix, Type_Names);
   end New_Type_Name;
   
   function New_Type_Name (Name : Name_Id) return Name_Id is
   begin
      return New_Name_For (Name, Type_Prefix, Type_Names);
   end New_Type_Name;
   
   --------------------
   -- New_Local_Name --
   --------------------
   
   function New_Local_Name (S : String) return Name_Id is
   begin
      return New_Name_For (String_Find (S), Local_Prefix, Local_Names);
   end New_Local_Name;
   
   function New_Local_Name (Name : Name_Id) return Name_Id is
   begin
      return New_Name_For (Name, Local_Prefix, Local_Names);
   end New_Local_Name;
   
   ---------------------
   -- New_Global_Name --
   ---------------------
   
   function New_Global_Name (S : String) return Name_Id is
   begin
      return New_Name_For (String_Find (S), Global_Prefix, Global_Names);
   end New_Global_Name;
   
   function New_Global_Name (Name : Name_Id) return Name_Id is
   begin
      return New_Name_For (Name, Global_Prefix, Global_Names);
   end New_Global_Name;
   
   ------------------------
   -- New_Procedure_Name --
   ------------------------
   
   function New_Procedure_Name (S : String) return Name_Id is
   begin
      return New_Name_For (String_Find (S), Procedure_Prefix, Procedure_Names);
   end New_Procedure_Name;
   
   function New_Procedure_Name (Name : Name_Id) return Name_Id is
   begin
      return New_Name_For (Name, Procedure_Prefix, Procedure_Names);
   end New_Procedure_Name;
   
   -----------------------
   -- New_Function_Name --
   -----------------------
   
   function New_Function_Name (S : String) return Name_Id is
   begin
      return New_Name_For (String_Find (S), Function_Prefix, Function_Names);
   end New_Function_Name;
   
   function New_Function_Name (Name : Name_Id) return Name_Id is
   begin
      return New_Name_For (Name, Function_Prefix, Function_Names);
   end New_Function_Name;
   
end Reflex.External_Names;
