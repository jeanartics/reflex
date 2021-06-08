with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; Use Ada.Text_IO;

package body Binder.Bexch_Types is

   -- New_Open
   function New_Open (Path : String) return Bexch_Type
   is
      Ret : Bexch_Type := new Bexch_Type_Record;
   begin
      Ret.Path := new String'(Path);
      return Ret;
   end New_Open;
   
   -- Debug_Dump
   procedure Debug_Dump (B : Bexch_Type)
   is
      F : Ada.Text_IO.File_Type;
      L : access String ;
   begin
      Put_Line ("*** DUMP of : " & B.Path.all );
      Ada.Text_IO.Open (File => F,
                        Mode => Ada.Text_IO.In_File,
                        Name => B.Path.all );
      
      while not End_Of_File (F) loop
         L := new String'(Get_Line (F));
         Put_Line (L.all );         
      end loop;
      
      Ada.Text_IO.Close (F);
      
      Put_Line ("*** DUMP end. ");
   end Debug_Dump;
   
   -- Debug_Dump_Txt_File
   procedure Debug_Dump_Txt_File (Path : String)
   is
      F : Ada.Text_IO.File_Type;
      L : access String ;
   begin
      
      Put_Line ("*** DUMP of : " & Path);
      
      if (not Exists (Path)) then
         Put_Line ("file does not exist");
      else
         Ada.Text_IO.Open (File => F,
                           Mode => Ada.Text_IO.In_File,
                           Name => Path);
         
         while not End_Of_File (F) loop
            L := new String'(Get_Line (F));
            Put_Line (L.all);         
         end loop;
         
         Ada.Text_IO.Close (F);
         
      end if;
      
      Put_Line ("*** DUMP end. ");
   end Debug_Dump_Txt_File;
   
   
   -- Debug_Back_Copy_File
   procedure Debug_Back_Copy_File (Path : String)
   is
      F : Ada.Text_IO.File_Type;
      Copy : String := Path & "--BACK-COPY";
   begin
      
      Put_Line ("*** DUMP of : " & Path);
      
      if (not Exists (Copy)) then
         if Exists (Path) then
            Ada.Directories.Copy_File (Source_Name => Path,
                                       Target_Name => Copy);
         end if;
      end if;
      
   end Debug_Back_Copy_File;
   
   -- Get_First_Content
   function Get_First_Content (B : Bexch_Type; Balise : String) return String
   is
      F    : Ada.Text_IO.File_Type;
      L    : access String ;
      Ret  : access String := new String'("");
      Next : Boolean;
   begin
      if (not Exists (B.Path.all )) then
         Put_Line ("file does not exist: " & B.Path.all );
      else
         Ada.Text_IO.Open (File => F,
                           Mode => Ada.Text_IO.In_File,
                           Name => B.Path.all );
         
         Next := False;
         while not End_Of_File (F) loop
            if Next then
               Ret := new String'(Get_Line (F));
               Ada.Text_IO.Close (F);
               return Ret.all ;
            else
               L := new String'(Get_Line (F));
               if L.all = Balise then 
                  Next := True;
               end if;
            end if;
         end loop;
         
         Ada.Text_IO.Close (F);
         
      end if;
      
      return Ret.all ;      
   end Get_First_Content;
   
end Binder.Bexch_Types;
