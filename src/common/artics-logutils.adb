------------------------------------------------------------------------------
--                                                                          --
--                             ARTICS COMPONENTS                            --
--                                                                          --
--                                CSV PARSER                                --
--                                                                          --
--                                  BODY                                    --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;             use Ada.Text_Io;
with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Characters.Handling; 

with Ada.Characters.Handling;

with GNAT.Case_Util;          use GNAT.Case_Util;

package body Artics.Logutils is

   Log_File    : Ada.Text_Io.File_Type; --  := Standard_Output;
   File_Create : Boolean := False;
   
   --------------------------
   -- Create_Loginfos_File --
   --------------------------
   
   procedure Create_Loginfos_File (File_Name : String) is
   begin
      Ada.Text_Io.Create
	(File => Log_File,
	 Mode => Ada.Text_Io.Out_File,
	 Name => File_Name);
      File_Create := True;
   end Create_Loginfos_File;
   
   -------------------------
   -- Close_Loginfos_File --
   -------------------------
   
   procedure Close_Loginfos_File is
   begin
      Ada.Text_Io.Close (File => Log_File);
      File_Create := False;
   end Close_Loginfos_File;
   
   --------------
   -- Log_Line --
   --------------
   
   procedure Log_Line (Msg : String) is
   begin
      if File_Create then
         Ada.Text_Io.Put_Line (Log_File, Msg);
         Flush ( Log_File );
      end if;
   end Log_Line;
      
   ------------------
   -- Log_New_Line --
   ------------------
   
   procedure Log_New_Line is
   begin
      if File_Create then
	 Ada.Text_Io.New_Line (Log_File);
      end if;
   end Log_New_Line;
      
end Artics.Logutils;
