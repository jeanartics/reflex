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
with Ada.Direct_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Reflex.Templates is

   package Buffer_IO is new Ada.Direct_IO (Character);
   
   -----------------
   -- Free_String --
   -----------------
   
   procedure Free_String (S : in out String_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(String, String_Ptr);
   begin
      if S /= null then
	 Free (S);
      end if;
   end Free_String;
   
   -----------------
   -- Free_Assocs --
   -----------------
   
   procedure Free_Assocs (A : in out Assocs_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Assocs_Array, Assocs_Ptr);
   begin
      if A /= null then
         Free (A);
      end if;
   end Free_Assocs;
   
   ------------------
   -- New_Template --
   ------------------
   
   function New_Template return Template_Ptr is
      This : Template_Ptr := new Template_Record'(No_Template_Record);
   begin
      return This;
   end New_Template;
   
   ------------------
   -- New_Template --
   ------------------
   
   function New_Template_From_String
     (Name      : String;
      Str       : String_Ptr;
      Tag_Count : Natural;
      Open_Tag  : Character := '{';
      Close_Tag : Character := '}') return Template_Ptr is
      
      subtype Dico_Array is Assocs_Array (1..Tag_Count);
      
      This : Template_Ptr := New_Template;
   begin
      This.Name      := new String'(Name);
      This.Count     := Tag_Count;
      This.Open_Tag  := Open_Tag;
      This.Close_Tag := Close_Tag;
      This.Assocs    := new Dico_Array;
      This.Data      := Str;
      This.Index     := 1;
      
      return This;
   end New_Template_From_String;
   
   ----------------------------
   -- New_Template_From_File --
   ----------------------------
   
   function New_Template_From_File
     (Name           : String;
      Full_File_Name : String;
      Tag_Count      : Natural;
      Open_Tag       : Character;
      Close_Tag      : Character) return Template_Ptr is
      
      subtype Dico_Array is Assocs_Array (1..Tag_Count);
      
      This : Template_Ptr := New_Template;
   begin
      This.Name      := new String'(Name);
      This.Count     := Tag_Count;
      This.Open_Tag  := Open_Tag;
      This.Close_Tag := Close_Tag;
      This.Assocs    := new Dico_Array;
      This.Index     := 1;
      
      Load_Data (This, Full_File_Name);

      return This;
   end New_Template_From_File;

   -------------------
   -- Free_Template --
   -------------------
   
   procedure Free_Template (This : in out Template_Ptr) is
      
      procedure Free is new Ada.Unchecked_Deallocation
	(Template_Record, Template_Ptr);
      
   begin
      if This /= null then
	 if This.Name /= null then
	    Free_String (This.Name);
	 end if;
	 
	 if This.Assocs /= null then
	    for I in This.Assocs'Range loop
	       if This.Assocs (I).Tag /= null then
		  Free_String (This.Assocs (I).Tag);
	       end if;
	       
	       if This.Assocs (I).Value /= null then
		  Free_String (This.Assocs (I).Value);
	       end if;
	    end loop;
	    
	    Free_Assocs (This.Assocs);
	 end if;
	 
	 Free (This);
      end if;
   end Free_Template;
   
   -------------------
   -- Set_Tag_Value --
   -------------------

   procedure Set_Tag_Value
     (This  : access Template_Record;
      Tag   : in String;
      Value : in String) is
      
      First_Empty : Natural := 0;
   begin
      for I in This.Assocs'Range loop
         if This.Assocs (I).Tag = null then
	    First_Empty := I;
	    
	 elsif This.Assocs (I).Tag.all = Tag then
	    This.Assocs (I).Value := new String'(Value);
            return;
         end if;
      end loop;
      
      if First_Empty /= 0 then
	 This.Assocs (First_Empty).Tag   := new String'(Tag);
	 This.Assocs (First_Empty).Value := new String'(Value);
      else
	 raise Program_Error;
      end if;
   end Set_Tag_Value;

   -------------
   -- Add_Tag --
   -------------

   procedure Add_Tag
     (This : access Template_Record;
      Tag  : in String) is
      
      First_Empty : Natural := 0;
   begin
      for I in This.Assocs'Range loop
         if This.Assocs(I).Tag = null then
	    First_Empty := I;
	    
	 elsif This.Assocs(I).Tag.all = Tag then
	    This.Assocs (I).Value := null;
            return;
         end if;
      end loop;
      
      if First_Empty /= 0 then
	 This.Assocs (First_Empty).Tag   := new String'(Tag);
	 This.Assocs (First_Empty).Value := null;
      else
	 raise Program_Error;
      end if;
   end Add_Tag;

   -------------------
   -- Get_Tag_Value --
   -------------------

   function Get_Tag_value
     (This : access Template_Record;
      Tag  : in String) return String_Ptr is
   begin
      for I in This.Assocs'Range loop
         if This.Assocs (I).Tag /= null 
	   and then  This.Assocs (I).Tag.all = Tag 
	 then
            return This.Assocs (I).Value;
         end if;
      end loop;
      
      return null;
   end Get_Tag_Value;
   
   ---------------
   -- Has_Value --
   ---------------

   function Has_Value
     (This : access Template_Record; 
      Tag  : in String) return Boolean is
   begin
      if Tag /= "" then
         for I in This.Assocs'Range loop
            if This.Assocs (I).Tag /= null 
	      and then  This.Assocs (I).Tag.all = Tag 
	    then
               return This.Assocs (I).Value /= null;
            end if;
         end loop;
      end if;

      return False;
   end Has_Value;

   --------------------------
   -- Retreive_Next_String --
   --------------------------

   function Retreive_Next_String
     (This : access Template_Record) return String_Ptr is
      
      procedure Replace;
      
      Str       : Unbounded_String := Null_Unbounded_String;
      Tag_Start : Natural := 0;
      Tag_Stop  : Natural := 0;
      
      -------------
      -- Replace --
      -------------
      
      procedure Replace is
	 Replace : String_Ptr;
      begin
	 -- Start of possible tag...
	 
	 This.Index := This.Index + 1;
	 Tag_Start  := This.Index;
	 
	 -- Goto the end of tag.
	 
	 while This.Index < This.Data'Last loop
	    exit when This.Data (This.Index) = This.Close_Tag;
	    This.Index := This.Index + 1;
	 end loop;
	 
	 Tag_Stop   := This.Index - 1;
	 This.Index := This.Index + 1;
	 
	 -- Search for corresponding value
	 
	 if Tag_Stop > Tag_Start 
	 and then Has_Value (This, This.Data (Tag_Start..Tag_Stop)) then
	    Replace := Get_Tag_Value (This, This.Data (Tag_Start..Tag_Stop));
	    Append (Str, Replace.all);
	 end if;
      end Replace;
      
      --  Start of processing Emit_String
      
   begin
      while This.Index <= This.Data'Last loop
	 if This.Data (This.Index) /= This.Open_Tag then
	   Append (Str, This.Data (This.Index));
	   This.Index := This.Index + 1;
	 else
	    Replace;
	 end if;
   end loop;
   
   if Str /= Null_Unbounded_String then
      return new String'(To_String (Str));
   else
      return null;
   end if;
   end Retreive_Next_String;

   ---------------------------
   -- Retreive_Next_Buffer  --
   ---------------------------

   procedure Retreive_Next_Buffer
     (This : access Template_Record;
      Ob   : in Output_buffer) is
      
      procedure Replace;
      
      Data      : String renames This.Data.all;
      Tag_Start : Natural := 0;
      Tag_Stop  : Natural := 0;
      
      -------------
      -- Replace --
      -------------
      
      procedure Replace is
	 Replace : String_Ptr;
      begin
	 -- Start of possible tag...
	 
	 This.Index := This.Index + 1;
	 Tag_Start  := This.Index;
	 
	 -- Goto the end of tag.
	 
	 while This.Index < Data'Last loop
	    exit when Data (This.Index) = This.Close_Tag;
	    This.Index := This.Index + 1;
	 end loop;
	 
	 Tag_Stop   := This.Index - 1;
	 This.Index := This.Index + 1;
	 
	 -- Search for corresponding value
	 
	 if Tag_Stop > Tag_Start 
	 and then Has_Value (This, Data (Tag_Start..Tag_Stop)) then
	    Replace := Get_Tag_Value (This, Data (Tag_Start..Tag_Stop));
	    Write_Str (Ob, Replace.all);
	 end if;
      end Replace;
      
      --  Start of processing Emit_String
      
   begin
      while This.Index <= Data'Last loop
	 if Data (This.Index) /= This.Open_Tag then
	   Write_Char (Ob, Data (This.Index));
	   This.Index := This.Index + 1;
	 else
	    Replace;
	 end if;
      end loop;
   end Retreive_Next_Buffer;

   --------------------------
   -- Reset_Index_Template --
   --------------------------

   procedure Reset_Index_Template (This : access Template_Record) is
   begin
      This.Index := This.Data.all'First;
   end Reset_Index_Template;

   ---------------------------
   -- Reset_Template_Values --
   ---------------------------

   procedure Reset_Template_Values (This : access Template_Record) is
   begin
      for I in This.Assocs'Range loop
         if This.Assocs (I).Value /= null then
            Free_String (This.Assocs (I).Value);
	    This.Assocs (I).Value := null;
         end if;
      end loop;

      This.Index := 1;
   end Reset_Template_Values;

   ---------------
   -- Load_Data --
   ---------------

   procedure Load_Data
     (This           : access Template_Record;
      Full_File_Name : in String) is
      
      use Buffer_IO;
      
      File  : Buffer_IO.File_Type;
      Siz   : Natural;
      Count : Natural;
   begin
      Open (File, In_File, Full_File_Name);
      Siz := Natural (Size (File));

      This.Data := new String (1..Siz);
      
      Count := This.Data'First;
      while Count <= This.Data'Last and then not End_Of_File (File) loop
         Read (File, This.Data (Count));
         Count := Count + 1;
      end loop;
      Count := Count - 1;
      
      Close (File);
   exception
      when E: others =>
         -- Debug.Unknown_Exception(E, "Templates.Load_Data");
         Put_Line ("Templates.Load_Data: Count = " & Count'Img);
   end Load_Data;

   ------------------
   -- Set_Open_Tag --
   ------------------

   procedure Set_Open_Tag
     (This  : access Template_Record;
      Open  : in Character) is
   begin
      This.Open_Tag := Open;
   end Set_Open_Tag;

   ------------------
   -- Get_Open_Tag --
   ------------------

   function Get_Open_Tag (This : access Template_Record) return Character is
   begin
      return This.Open_Tag;
   end Get_Open_Tag;

   -------------------
   -- Get_Close_Tag --
   -------------------

   function Get_Close_Tag (This : access Template_Record) return Character is
   begin
      return This.Close_Tag;
   end Get_Close_Tag;

   -------------------
   -- Set_Close_Tag --
   -------------------

   procedure Set_Close_Tag
     (This  : access Template_Record;
      Close : in Character) is
   begin
      This.Close_Tag := Close;
   end Set_Close_Tag;

end Reflex.Templates;
