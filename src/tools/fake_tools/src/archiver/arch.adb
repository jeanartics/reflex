
with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; Use Ada.Text_IO;

package body Arch is
   Arch_File   : Ada.Text_Io.File_Type;
   
   procedure Parse_Argument (Arg : String) is
   begin
      
      If Ada.Text_IO.Is_Open (Arch_File) then
         -- add given file to archive
         Ada.Text_IO.Put_Line (Arch_File, Arg);
         
      else
         -- 1st argument is File to create
         if Exists (Arg) then
            Rename (Arg, Arg & "--delete");
            Delete_File (Arg & "--delete");
         end if;
         Put_Line ("creating : " & Arg);
         Ada.Text_IO.Create (File => Arch_File,
                             Mode => Ada.Text_IO.Append_File,
                             Name => Arg,
                             Form => "");         
      end if;
      
   end Parse_Argument;
   
   procedure Archive is
      Arg_Count : constant Natural := Ada.Command_Line.Argument_Count;
   begin
      if Arg_Count < 1 then
         Put_Line ("No argument found");
      else
         for I in 1 .. Arg_Count loop
            Parse_Argument (Ada.Command_Line.Argument (I));
         end loop;
         
         if Ada.Text_IO.Is_Open (Arch_File) then
            Ada.Text_IO.Close (Arch_File);
         end if;
      end if;
   end Archive;
end Arch;
