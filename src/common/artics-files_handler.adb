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

with Ada.Text_Io; use Ada.Text_Io;

package body Artics.Files_Handler is
   
   ---------------
   -- Error_Msg --
   ---------------
   
   procedure Error_Msg (S : String) is
   begin
      Put_Line (Standard_Error, "error: " & S);
   end Error_Msg;
   
   ----------------
   -- Initialize --
   ----------------
   
   procedure Initialize is
   begin
      Files_Tables.Init (Files);
      Files_Tables.Increment_Last (Files);
   end Initialize;
   
   ----------------------
   -- Add_File_Handler --
   ----------------------
   
   function Add_File_Handler (F : Name_Id) return File_Address is
      
      I : File_Address;
      H : File_Handler_Record;
   begin
      I := File_Address'First + 1;
      loop
	 exit when I >= Last (Files);
	 if Files.Table (I).Name = F then
	    return I;
	 end if;
	 I := I + 1;
      end loop;
      
      H.Name := F;
      H.Sb   := new Output_Buffer_Record;
      
      Append (Files, H);
      return Last (Files);
   end Add_File_Handler;
   
   -------------------
   -- Get_File_Name --
   -------------------
   
   function Get_File_Name (F : File_Address) return Name_Id is
   begin
      return Files.Table (F).Name;
   end Get_File_Name;
   
   --------------------------
   -- Get_File_Name_String --
   --------------------------
   
   function Get_File_Name_String (F : File_Address) return String is
      
      N : Name_Id := Files.Table (F).Name;
   begin
      if N /= No_Name then
	 return Get_String (Files.Table (F).Name);
      else
	 return "";
      end if;
   end Get_File_Name_String;
   
   ----------------
   -- Get_Buffer --
   ----------------
   
   function Get_Buffer (F : File_Address) return Output_Buffer is
   begin
      return Files.Table (F).Sb;
   end Get_Buffer;
   
   ----------------
   -- Set_Buffer --
   ----------------
   
   procedure Set_Buffer 
     (F  : File_Address;
      Sb : Output_Buffer) is
   begin
      Files.Table (F).Sb := Sb;
   end Set_Buffer;
   
   ------------------
   -- Reset_Buffer --
   ------------------
   
   procedure Reset_Buffer (F : File_Address) is
      Sb : Output_Buffer;
   begin
      if F /= No_File_Address then
	 Sb := Files.Table (F).Sb;
	 Artics.Files_Handler.Reset_Buffer (F);
      end if;
   end Reset_Buffer;
  
   -----------------------
   -- Flush_File_Buffer --
   -----------------------
   
   procedure Flush_File_Buffer (F : File_Address) is
      H : File_Handler_Record;
   begin
      if F /= No_File_Address then
	 H := Files.Table (F);
	 if H.Name /= No_Name then
	    Artics.Buffers.Write_To_Text_File
	      (H.Sb, Get_String (H.Name));
	 else
	    Error_Msg ("cannot flush buffer, no file name for the buffer");
	 end if;
      end if;
   end Flush_File_Buffer;
   
end Artics.Files_Handler;
