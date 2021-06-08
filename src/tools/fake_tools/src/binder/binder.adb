--* FAKE * binder: update project.bexch file -o

with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with Binder.Bexch_Types; use Binder.Bexch_Types;
package body Binder is
   Bexch_Path : access String := new String'("");
	
   -- Parse_Argument
   procedure Parse_Argument (Arg : String) is 
      S : String := Arg;
   begin
      if Exists (S) and then (Extension (S) = "bexch") then
         Bexch_Path := new String'(S);
      end if;
   end Parse_Argument;
	
   -- Parse_Command_Line
   procedure Parse_Command_Line is 
      Arg_Count : constant Natural := Ada.Command_Line.Argument_Count;
   begin
      if Arg_Count < 1 then
         Put_Line ("No argument found");
      elsif Arg_Count > 1 then
         Put_Line ("Accept only 1 argument in command");
      else
         for I in 1 .. Arg_Count loop
            Parse_Argument (Ada.Command_Line.Argument (I));
         end loop;
      end if;
   end Parse_Command_Line;
   
   -- Check_Bexch
   function Check_Bexch return Boolean is
      S : String := Bexch_Path.all;
   begin
      if Exists (S) and then (Extension (S) = "bexch") then
         return True; -- OK
      end if;
      return False; -- not OK
   end Check_Bexch;
   
   -- Bind_Bexch
   procedure Bind_Bexch is
      B         : Bexch_Type;
      Temp_Path : access String;
      Main_Base : access String;
      Deps      : access String;
      Dep_File  : File_Type;
      Old_Bexch : File_Type;
      New_Bexch : File_Type;
      Gof_File  : File_Type;
      Sof_File  : File_Type;
      Line      : access String;
      Count     : Integer;
      Gof_Name  :access String; --fake generated object file name
      Sof_Name  :access String; --fake generated source file name
   begin
      -- Open old .bexch to parse
      B := New_Open (Bexch_Path.all);
      
      -- file like 'GNAT-TEMP-000002.TMP' (?? empty file)
      Temp_Path := new String'(Get_First_Content (B, "[MAPPING FILE]"));
      -- Debug_Dump_Txt_File (Temp_Path.all);
      -- Temps file -> working directory base
      Temp_Path := new String'(Containing_Directory(Temp_Path.all));
              
      -- main file for this build -> get .deps file 
      Main_Base := new String'(Get_First_Content (B, "[MAIN BASE NAME]"));
      Deps := new String'(Compose 
                          (Containing_Directory => Temp_Path.all,
                           Name                 => "lib" & Main_Base.all,
                           Extension            => "deps"));
      
      
      
      -- rename initial .bexch -> .bexch--OLD ; create new
      if (Exists ( B.Path.all & "--OLD")) then
         Delete_File (B.Path.all & "--OLD");
      end if;
      Rename (B.Path.all , B.Path.all & "--OLD");
      Create (File => New_Bexch,
              Mode => Out_File,
              Name => B.Path.all);
      Open (Old_Bexch, In_File, B.Path.all & "--OLD");
      
      -- Create section [GENERATED OBJECT FILE] -> fake obj
      Put_Line (New_Bexch, "[GENERATED OBJECT FILE]");
      Gof_Name := new String'
        (Compose (Containing_Directory => Temp_Path.all,
                  Name                 => "b__" & Main_Base.all ,
                  Extension            => "xef"));
      Create (File => Gof_File,
              Mode => Out_File,
              Name => Gof_Name.all );
      Put_Line (Gof_File, "<document/>"); -- empty but xml correct
      Close (Gof_File );
      Put_Line (New_Bexch, "b__" & Main_Base.all & ".xef" );
      
      -- Copy [PROJECT FILE] section from old to new (3 lines)
      Count := 0;
      while not End_Of_File (Old_Bexch) loop
         Line := new String'(Get_Line (Old_Bexch));
         if (Line.all = "[PROJECT FILES]") then
            Put_Line(New_Bexch, Line.all);
            Count := Count + 1;
         elsif (Count > 0) then
            exit when Line(1) = '[';-- exit on another balise (or eof)
            Put_Line(New_Bexch, Line.all);
            Count := Count + 1;
         end if;
      end loop; 
      
      -- Create section [BOUND OBJECT FILES] get from .deps file
      Put_Line (New_Bexch, "[BOUND OBJECT FILES]");      
      if (Exists (Deps.all)) then
         Open (Dep_File, In_File, Deps.all );
         
         while not End_Of_File (Dep_File) loop
            Line := new String'(Get_Line (Dep_File));
            Put_Line (New_Bexch, Line.all);
         end loop;
         
         Close (Dep_File);
      end if;
      
      -- Create section [GENERATED SOURCE FILES] - fake source
      Put_Line (New_Bexch, "[GENERATED SOURCE FILES]");
      Sof_Name := new String'
        (Compose (Containing_Directory => Temp_Path.all,
                  Name                 => "b__" & Main_Base.all,
                  Extension            => "ads")
         );
      Create (File => Sof_File,
              Mode => Out_File,
              Name => Sof_Name.all );
      Line := new String'("package b__" & Main_Base.all & " is");
      Put_Line (Sof_File, Line.all );
      Line := new String'("end b__" & Main_Base.all & ";");
      Put_Line (Sof_File, Line.all );
      Close (Sof_File);
      Put_Line (New_Bexch, "b__" & Main_Base.all & ".ads");
      
      -- create an empty .ali      
      Sof_Name := new String'
        (Compose (Containing_Directory => Temp_Path.all,
                  Name                 => "b__" & Main_Base.all,
                  Extension            => "ali")
        );
      Create (File => Sof_File,
              Mode => Out_File,
              Name => Sof_Name.all );
      Line := new String'("V REFLEX V2018");
      Put_Line (Sof_File, Line.all );
      Close (Sof_File);
      Put_Line(New_Bexch, "b__" & Main_Base.all & ".ali");
      
      -- Create section [RESULTING OPTIONS] - empty
      Put_Line (New_Bexch, "[RESULTING OPTIONS]");
      Put_Line (New_Bexch, "-L" & Temp_Path.all );
            
      Close (New_Bexch);
      Close (Old_Bexch);
      Delete_File(B.Path.all & "--OLD");
   end Bind_Bexch;
   
      ----------
      -- Bind --
      ----------
	
      procedure Bind is
      begin
         Parse_Command_Line;
      
         if Check_Bexch then      
            Bind_Bexch;
         end if;
      
         -- don't catch exceptions...
      end Bind;
   end Binder;
   
