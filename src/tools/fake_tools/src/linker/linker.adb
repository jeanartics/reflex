--* FAKE * Linker : copy source to dest if option -o

with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; Use Ada.Text_IO;

package body Linker is
   Option_O    : Boolean := False;
   Source_Path : access String := new String'("");
   Destination : access String := new String'("");
	
   procedure Parse_Argument (Arg : String) is 
      S : String := Arg;
   begin
      if Arg_Obj = Arg then
         Option_O := True;
         return;
      end if;
      
      if Option_O then 
         Destination := new String'(S);
			
      elsif Source_Path.all = "" then
         if Exists (S) then
            Source_Path := new String'(S);
         end if;
      end if;
   end Parse_Argument;
	
   procedure Parse_Command_Line is 
      Arg_Count : constant Natural := Ada.Command_Line.Argument_Count;
   begin
      if Arg_Count < 1 then
         PUt_Line ("No argument found");
      else
         for I in 1 .. Arg_Count loop
            Parse_Argument (Ada.Command_Line.Argument (I));
         end loop;
      end if;
   end Parse_Command_Line;
	
   procedure Copy_Source_To_Dest is
   begin
      if (not Exists (Source_Path.all)) then
         Put_Line ("Error source does not exist.");
      end if;
      if Exists (Destination.all) then
         -- deleting directly takes OS time, so we rename before...
         Rename (Destination.all, Destination.all & "--delete");
         Delete_File (Destination.all & "--delete");
      end if;
      Copy_File (Source_Path.all, Destination.all);
         Put_Line ("Reflex link -> " & Destination.all);
   end Copy_Source_To_Dest;
	
   function Check_Bexch return Boolean is
      F : String := Source_Path.all;
   begin
      if Extension(F) = "bexch" Then
         -- job was called with a .bexch extension file, update file
         -- (nb: we should do things in file to prepare final link...)
         
         -- deleting directly costs OS time, so we rename before...
         Rename (Source_Path.all, Source_Path.all & "--delete");
         Copy_File (Source_Path.all & "--delete", Source_Path.all);
         
         Delete_File (Destination.all & "--delete");
         
         return True;
      else
         return False;
      end if;
   end Check_Bexch;
   
   ----------
   -- Link --
   ----------
	
   procedure Link is
   begin
		
      Parse_Command_Line;
      
      if not Check_Bexch then      
         if Option_O then
            Copy_Source_To_Dest;
         else
            null;-- what else ?
         end if;
      end if;
      
      -- don't catch exceptions...
   end Link;
end Linker;

