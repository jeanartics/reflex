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

with Ada.Text_Io; use Ada.Text_Io;

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Gnat.Case_Util; use Gnat.Case_Util;

with Atree; use Atree;
with Einfo; use Einfo;
with Fname; use Fname;
with Lib; use Lib;
with Opt; use Opt;
with Namet; use Namet;
with Sinfo; use Sinfo;
with Sem_Util; use Sem_Util;
with Types; use Types;
with Uname; use Uname;
with Output; use Output;

with Artics.Strings_Stocks; use Artics.Strings_Stocks;

with Reflex.Infos; use Reflex.Infos;

package body Reflex.Configs.Entities_Renames is
   
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
   
   ---------------------
   -- Get_Entity_Name -- 
   ---------------------
   
   function Get_Entity_Name (S : String) return Name_Id is
      Estart : Natural;
   begin
      if S = "" then
	 return No_Name;
      end if;
      
      Estart := 0;
      for J in reverse S'Range loop
         if S (J) = '.' then
	    exit;
         else
            Estart := Estart +1;
         end if;
      end loop;
      
      if Estart /= 0 then
	 return String_Find (S ((S'Last - (Positive (Estart)) + 1) .. S'Last));
      else
	 return String_Find (S);
      end if;
   end Get_Entity_Name;
      
   ----------------------------
   -- Get_Entity_Prefix_Name -- 
   ----------------------------
   
   function Get_Prefix_Name (S : String) return Name_Id is
      
      Estart : Natural;
   begin
      if S = "" then
	 return No_Name;
      end if;
      
      Estart := 0;
      for J in reverse S'Range loop
         if S (J) = '.' then
	    exit;
         else
            Estart := Estart + 1;
         end if;
      end loop;
      
      if Estart /= 0 then
	 Estart := Estart + 1;
	 declare
	    Str : String := S (S'First .. (S'Last - Positive (Estart)));
	    N : Name_Id;
	 begin
	    N := String_Find (Str);
	    return N;
	 end;
      else
	 return String_Find (S);
      end if;
   end Get_Prefix_Name;

   ------------------
   -- To_Unit_Name --
   ------------------
   
   function To_Unit_Name (S : String) return String is
   begin
      if S'Length > 2 then
	 return S(S'First..S'Last-2);
      else
	 return "";
      end if;
   end To_Unit_Name;
   
   ---------------------
   -- Get_Unit_Entity --
   ---------------------
   
   function Get_Unit_Entity (Pref : Name_Id) return Entity_Id is
      S : String := Get_String (Pref);
   begin
      return Get_Unit_Entity (S);
   end Get_Unit_Entity;
   
   function Get_Unit_Entity (Pref : String) return Entity_Id is
      S : String := Pref;
   begin
      if S /= "" then
	 To_Lower (S);
	 
	 for U in 1..Last_Unit loop
	    if not Is_Predefined_File_Name (Unit_File_Name (U)) then
	       if To_Unit_Name (Get_Name_String (Unit_Name (U))) = S then
		  return Cunit_Entity (U);
	       end if;
	    end if;
	 end loop;
      end if;
      
      return Empty;
   end Get_Unit_Entity;
   
   -------------------------
   -- Find_Entity_In_Unit --
   -------------------------
   
   function Find_Entity_In_Unit
     (Eunit        : Entity_Id;
      Prefix_Name  : Name_Id;
      Entity_Name  : Name_Id;
      Current_Line : Natural := 0) return Entity_Id is
      
      Ebody : Entity_Id := Empty;
      Epref : Entity_Id := Empty;
      E     : Entity_Id := Empty;
   begin
      if Ekind (Eunit) = E_Package then
	 Ebody := Body_Entity (Eunit);
      end if;
      
      if Prefix_Name /= No_Name then
	 Put_Line ("Prefix found = " & Get_String (Prefix_Name));
	 --  Search Prefix in unit
	 
	 E := Get_Name_Entity_Id (Prefix_Name);
	 while Present (E) loop
	    Put_Line ("E       = " & Get_String (Chars (E)));
	    Put_Line ("Scope E = " & Get_String (Chars (Scope (E))));
	    Put_Line ("Eunit   = " & Get_String (Chars (Eunit)));
	    Put_Line ("Comes_From_Source = " & Comes_From_Source (E)'Img);
	    if Comes_From_Source (E) then
	       if Scope (E) = Eunit or else Scope (E) = Ebody then
		  Epref := E;
		  Put_Line ("Epref found = " & Get_String (Chars (Epref)));
		  Put_Line ("Epref Ekind = " & Ekind (Epref)'Img);
		  exit;
	       end if;
	    end if;
	    E := Homonym (E);
	 end loop;
	 
	 if No (Epref) then
	    Set_Standard_Error;
	    Write_Str ("line:");
	    Write_Int (Int (Current_Line));
	    Write_Str
	      (" cannot find record type " & Get_Name_String (Prefix_Name));
	    Write_Eol;
	    Set_Standard_Output;

	 elsif Ekind (Epref) not in Record_Kind then
	    Set_Standard_Error;
	    Write_Str ("line:");
	    Write_Int (Int (Current_Line));
	    Write_Str
	      (" prefix of an entity must be a record type, found " & 
		 Get_Name_String (Prefix_Name));
	    Write_Eol;
	    Set_Standard_Output;
	    
	 else
	    
	    --  Epref must be a record type, so earch Entity in Record type
	    
	    E := First_Component (Epref);
	    while Present (E) loop
	       if Comes_From_Source (E) then
		  if Scope (E) = Epref then
		     return E;
		  end if;
	       end if;
	       E := Next_Component (E);
	    end loop;
	 end if;
	 
	 --  Global entity in unit.
	 
      else
	 E := Get_Name_Entity_Id (Entity_Name);
	 while Present (E) loop
	    if Comes_From_Source (E) then
	       if Scope (E) = Eunit or else Scope (E) = Ebody then
		  return E;
	       end if;
	    end if;
	    E := Homonym (E);
	 end loop;
      end if;	 
      
      -- If we fall here, there is no entity
      
      Set_Standard_Error;
      Write_Str ("line:");
      Write_Int (Int (Current_Line));
      Write_Str
	(" entity " & 
	   Get_String (Prefix_Name) & "." & Get_String (Entity_Name) &
	   " not found in unit " & Get_String (Chars (E)));
      Write_Eol;
      Set_Standard_Output;
      
      return Empty;
   end Find_Entity_In_Unit;
   
   -----------------
   -- Find_Entity --
   -----------------
   
   function Find_Entity 
     (Unit_Name    : String;
      Full_Name    : String;
      Current_Line : Natural := 0) return Entity_Id is
      
      Uref        : Name_Id;
      Eref        : Name_Id;
   begin
      if Unit_Name /= "" then
	 Uref := No_Name;
      else
	 Uref := String_Find (Unit_Name);
      end if;
      
      if Full_Name /= "" then
	 Eref := No_Name;
      else
	 Eref := String_Find (Full_Name);
      end if;
      
      return Find_Entity (Uref, Eref, Current_Line);
   end Find_Entity;
   
   -----------------
   -- Find_Entity --
   -----------------
   
   function Find_Entity
     (Unit_Name        : Name_Id;
      Full_Entity_Name : Name_Id;
      Current_Line     : Natural := 0) return Entity_Id is
      
      Eunit : Entity_Id := Empty;
      Ebody : Entity_Id := Empty;
      E     : Entity_Id := Empty;
   begin
      if Unit_Name = No_Name then
	 Set_Standard_Error;
	 Write_Str ("line:");
	 Write_Int (Int (Current_Line));
	 Write_Str (" prefix for entity to rename is mandatory");
	 Write_Eol;
	 Set_Standard_Output;
      else
	 Eunit := Get_Unit_Entity (Unit_Name);
	 
	 if No (Eunit) then
	    Set_Standard_Error;
	    Write_Str ("line:");
	    Write_Int (Int (Current_Line));
	    Write_Str (" cannot find unit " & Get_Name_String (Unit_Name));
	    Write_Eol;
	    Set_Standard_Output;
	    
	 elsif Full_Entity_Name = No_Name then
	    Set_Standard_Error;
	    Write_Str ("line:");
	    Write_Int (Int (Current_Line));
	    Write_Str (" name for entity to rename is mandatory");
	    Write_Eol;
	    Set_Standard_Output;
	    
	 else
	    declare
	       Prefix_Name : Name_Id := 
		 Get_Prefix_Name (Get_Name_String (Full_Entity_Name));
	       Entity_Name : Name_Id := 
		 Get_Entity_Name (Get_Name_String (Full_Entity_Name));
	    begin
	       if Prefix_Name = No_Name then
		  Put_Line ("Prefix is No Name");
	       else
		  Put_Line ("Prefix = " & Get_String (Prefix_Name));
	       end if;
	       if Entity_Name = No_Name then
		  Put_Line ("Entity is No Name");
	       else
		  Put_Line ("Entity = " & Get_String (Entity_Name));
	       end if;
	       E := Find_Entity_In_Unit
		 (Eunit, Prefix_Name, Entity_Name, Current_Line);
	    end;
	 end if;
      end if;
      
      return E;
   end Find_Entity;
   
   ------------------------------
   -- Register_New_Entity_Name --
   ------------------------------
   
   procedure Register_New_Entity_Name
     (Unit_Name        : Str_Id;
      Full_Entity_Name : Str_Id;
      New_Name         : Str_Id;
      New_Comment      : Str_Id;
      New_Addr         : Str_Id;
      Current_Line     : Natural)
   is
      E : Entity_Id;
   begin
      if Full_Entity_Name = No_Str_Id then
	 return;
      end if;
      
      declare
         Sunit       : String  := Get_String (Unit_Name);
         Unit_Name   : Name_Id := String_Find (Sunit);
         Sent        : String  := Get_String (Full_Entity_Name);
         Entity_Name : Name_Id := String_Find (Sent);
      begin	 
	 E := Find_Entity (Unit_Name, Entity_Name, Current_Line);
	 
         if No (E) then
            Set_Standard_Error;
            Write_Str ("line:");
            Write_Int (Int (Current_Line));
            Write_Str (" unkonwn entity for renaming vars");
            Write_Eol;
            Set_Standard_Output;
            return;
         end if;
      end;
      
      if New_Name /= No_Str_Id then
         declare
            New_Name_Str    : String := Get_String (New_Name);
            Entity_New_Name : Name_Id;
         begin
	    Entity_New_Name := Name_Find (New_Name_Str);
            if New_Names.Contains (New_Names_Reg, Entity_New_Name) then
               Set_Standard_Error;
               Write_Str ("line:");
               Write_Int (Int (Current_Line));
               Write_Str 
                 (" new name " & New_Name_Str & " is already specified");
               Write_Eol;
               Set_Standard_Output;
               return;
            else
               New_Names.Insert (New_Names_Reg, Entity_New_Name, True);
            end if;
            
            Set_Entity_New_Name (E, Entity_New_Name);
         end;
      end if;
         
      if New_Addr /= No_Str_Id then
	 Set_Entity_Address  (E, New_Addr);
      end if;
      
      if New_Comment /= No_Str_Id then
	 Set_Entity_Comment  (E, New_Comment);
      end if;
   end Register_New_Entity_Name;
   
   -----------------------------
   -- Register_New_Subprogram --
   -----------------------------
   
   procedure Register_New_Subprogram
     (Unit_Name        : Str_Id;
      Full_Entity_Name : Str_Id;
      New_Name         : Str_Id;
      New_Comment      : Str_Id;
      Gen_Type         : Str_Id;
      Lang             : Str_Id;
      Current_Line     : Natural)
   is
      E : Entity_Id;
   begin
      if Full_Entity_Name = No_Str_Id then
	 return;
      end if;
      
      declare
         Sunit       : String := Get_String (Unit_Name);
         Unit_Name   : Name_Id  := String_Find (Sunit);
         Entity_Name : Name_Id := 
	   Get_Entity_Name (Get_String (Full_Entity_Name));
      begin	 
	 E := Find_Entity (Unit_Name, Entity_Name, Current_Line);

         if No (E) then
            Set_Standard_Error;
            Write_Str ("line:");
            Write_Int (Int (Current_Line));
            Write_Str (" unkonwn subprogram entity for subprogram infos");
            Write_Eol;
            Set_Standard_Output;
            return;
         end if;
      end;
      
      if New_Name /= No_Str_Id then
         declare
            New_Name_Str    : String := Get_String (New_Name);
            Entity_New_Name : Name_Id;
         begin
               Entity_New_Name := Name_Find (New_Name_Str);
            if New_Names.Contains (New_Names_Reg, Entity_New_Name) then
               Set_Standard_Error;
               Write_Str ("line:");
               Write_Int (Int (Current_Line));
               Write_Str 
                 (" new name " & New_Name_Str & " is already specified");
               Write_Eol;
               Set_Standard_Output;
               return;
            else
               New_Names.Insert (New_Names_Reg, Entity_New_Name, True);
            end if;
            
            Set_Entity_New_Name (E, Entity_New_Name);
         end;
      end if;
         
      if Gen_Type /= No_Str_Id then
	 null; --  Set_Entity_Address  (E, New_Addr);
      end if;
      
      if Lang /= No_Str_Id then
	 null; --  Set_Entity_Address  (E, New_Addr);
      end if;
      
      if New_Comment /= No_Str_Id then
	 Set_Entity_Comment  (E, New_Comment);
      end if;
   end Register_New_Subprogram;
   
   ---------------------
   -- New_Name_Comand --
   ---------------------
   
   function New_Name_Command return Name_Command_Ptr is
   begin
      return new Name_Command_Record'(No_Name_Commd_Record);
   end New_Name_Command;
   
   ----------------------
   -- New_Name_Command --
   ----------------------
   
   function New_Name_Command
     (E   : Entity_Id;
      Cmd : Command_Type) return Name_Command_Ptr is
      
      This : Name_Command_Ptr := New_Name_Command;
   begin
      This.E       := E;
      This.Command := Cmd;
      
      return This;
   end New_Name_Command;
   
   ----------------
   -- Get_Entity --
   ----------------
   
   function Get_Entity (This : Name_Command_Ptr) return Entity_Id is
   begin
      return This.E;
   end Get_Entity;
   
   ----------------
   -- Set_Entity --
   ----------------
   
   procedure Set_Entity
     (This : Name_Command_Ptr;
      E    : Entity_Id) is
   begin
      This.E := E;
   end Set_Entity;
   
   -----------------
   -- Get_Command --
   -----------------
   
   function Get_Command (This : Name_Command_Ptr) return Command_Type is
   begin
      return This.Command;
   end Get_Command;
   
   -----------------
   -- Set_Command --
   -----------------
   
   procedure Set_Command
     (This : Name_Command_Ptr;
      Cmd  : Command_Type) is
   begin
      This.Command := Cmd;
   end Set_Command;
   
   --------------
   -- Contains --
   --------------
   
   function Contains (This : Name_Command_Ptr) return Boolean is
   begin
      return False;
   end Contains;
   
end Reflex.Configs.Entities_Renames;
