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

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Unchecked_Conversion;
with Gnat.Awk; use Gnat.Awk;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Gnat.Case_Util; use Gnat.Case_Util;

with Artics.Strings_Stocks; use Artics.Strings_Stocks;
with Reflex.Configs.Entities_Renames; use Reflex.Configs.Entities_Renames;

package body Reflex.Configs.Vars is
   
   use Gnat;
   
   Unit_Field     : Awk.Count := 1;
   Entity_Field   : Awk.Count := 2;
   New_Name_Field : Awk.Count := 3;
   Type_Field     : Awk.Count := 4;
   Address_Field  : Awk.Count := 5;
   Comment_Field  : Awk.Count := 6;
     
   ------------------------
   -- Validate_Unit_Name --
   ------------------------
   
   function Validate_Unit_Name (Name : String) return Boolean is
   begin
      return True;
   end Validate_unit_Name;
   
   --------------------------
   -- Validate_Entity_Name --
   --------------------------
   
   function Validate_Entity_Name (Name : String) return Boolean is
   begin
      return True;
   end Validate_Entity_Name;
   
   -----------------------------
   -- Validate_Entity_Address --
   -----------------------------
   
   function Validate_Entity_Address (Name : String) return Boolean is
   begin
      return True;
   end Validate_Entity_Address;
   
   ---------------------------
   -- Validate_Entity_Type -- 
   ---------------------------
   
   function Validate_Entity_Type (Name : String) return Boolean is
   begin
      return True;
   end Validate_Entity_Type;
   
   ----------------
   -- Parse_Vars --
   ----------------
   
   procedure Parse_Vars (File_Name : String) is
      
      Max_Field      : Awk.Count;
      Vars_Session   : Session_Type;
      Unit_Entity_Id : Str_Id;
      Full_Entity_Id : Str_Id;
      New_Name_Id    : Str_Id;
      Type_Id        : Str_Id;
      Address_Id     : Str_Id;
      Comment_Id     : Str_Id;
      Current_Line   : Awk.Count;
   begin
      AWK.Set_Current (Vars_Session);
      AWK.Open (Separators => ";", Filename   => File_Name);
      
      if not AWK.End_Of_File then
	 
	 --  Skip Csv Header
	 
	 AWK.Get_Line;
	 
	 Current_Line := 1;
	 while not AWK.End_Of_File loop
	    AWK.Get_Line;
	    Current_Line := Current_Line + 1;
	    
	    Max_Field := Number_Of_Fields (Vars_Session);
	    
	    Unit_Entity_Id := No_Str_Id;
	    Full_Entity_Id := No_Str_Id;
	    New_Name_Id    := No_Str_Id;
	    Type_Id        := No_Str_Id;
	    Address_Id     := No_Str_Id;
	    Comment_Id     := No_Str_Id;
	    
	    if Unit_Field <= Max_Field then
	       declare
		  S : String := AWK.Field (Unit_Field);
	       begin
		  Trim (S, Both);
		  To_Lower (S);
		  Unit_Entity_Id := Enter_String (S);
	       end;
	    end if;
	    
	    if Entity_Field <= Max_Field then
	       declare
		  S : String := AWK.Field (Entity_Field);
	       begin
		  Trim (S, Both);
		  To_Lower (S);
		  Full_Entity_Id := Enter_String (S);
	       end;
	    end if;
	    
	    if New_Name_Field <= Max_Field then
	       declare
		  S : String := AWK.Field (New_Name_Field);
	       begin
		  Trim (S, Both);
		  To_Lower (S);
		  New_Name_Id := Enter_String (S);
	       end;
	    end if;
	    
	    if Type_Field <= Max_Field then
	       declare
		  S : String := AWK.Field (Type_Field);
	       begin
		  Trim (S, Both);
		  To_Lower (S);
		  Type_Id := Enter_String (S);
	       end;
	    end if;
	    
	    if Address_Field <= Max_Field then
	       declare
		  S : String := AWK.Field (Address_Field);
	       begin
		  Trim (S, Both);
		  To_Lower (S);
		  Address_Id := Enter_String (S);
	       end;
	    end if;
	    
	    if Comment_Field <= Max_Field then
	       declare
		  S : String := AWK.Field (Comment_Field);
	       begin
		  Trim (S, Both);
		  To_Lower (S);
		  Comment_Id := Enter_String (S);
	       end;
	    end if;
	    
	    Register_New_Entity_Name
	      (Unit_Name        => Unit_Entity_Id,
	       Full_Entity_Name => Full_Entity_Id,
	       New_Name         => New_Name_Id,
	       New_Comment      => Comment_Id,
	       New_Addr         => Address_Id,
	       Current_Line     => Integer (Current_Line));
	 end loop;
	 
      else
	 Put_Line ("  ==== PAS LINE =====");
      end if;
      
      AWK.Close (Vars_Session);
   end Parse_Vars;
   
end Reflex.Configs.Vars;
