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

with GNAT.Dynamic_Tables;
with Artics.Types; use Artics.Types;
with Artics.Buffers; use Artics.Buffers;
with Artics.Namet; use Artics.Namet;

package Artics.Files_Handler is

   type File_Handler_Record is private;
   
   No_File_Handler_Record : constant File_Handler_Record;
   
   subtype File_Address is Natural;
   
   No_File_Address : constant File_Address;
   
   procedure Initialize;
   
   function Add_File_Handler (F : Name_Id) return File_Address;
      
   function Get_File_Name (F : File_Address) return Name_Id;
   
   function Get_File_Name_String (F : File_Address) return String;
   
   function Get_Buffer (F : File_Address) return Output_Buffer;
   
   procedure Set_Buffer 
     (F  : File_Address;
      Sb : Output_Buffer);
   
   procedure Reset_Buffer (F : File_Address);
   
   procedure Flush_File_Buffer (F : File_Address);
   
  
private 
   
   type File_Handler_Record is record
      Name : Name_Id;
      Sb   : Output_Buffer;
   end record;
   
   No_File_Handler_Record : constant File_Handler_Record :=
     (Name => No_Name,
      Sb   => null);
   
   No_File_Address : constant File_Address := File_Address'First;
   
   package Files_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => File_Handler_Record,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 512,
      Table_Increment      => 512);
   use Files_Tables;
   
   Files : Files_Tables.Instance;
   
end Artics.Files_Handler;
