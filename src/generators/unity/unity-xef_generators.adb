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

with Ada.Unchecked_Deallocation;
with Ada.Directories;


with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions; use Ada.Exceptions;
with Gnat.Os_Lib; use Gnat.Os_Lib;

--  with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
--  with Ada.Strings.Fixed; use Ada.Strings.Fixed;
--  with Ada.Strings.Maps; use Ada.Strings.Maps;
--  with Ada.Strings; use Ada.Strings;

with Input_Sources.File;
with Sax.Locators; use Sax.Locators;
with Sax.Exceptions; use Sax.Exceptions;
with Sax.Attributes; use Sax.Attributes;
with Unicode; use Unicode;

--  with Options;
with Reflex.Formats; use Reflex.Formats;

package body Unity.Xef_Generators is
   
   --  The xef generation uses a state machine. For start element of the Sax 
   --  parser 
   
   ----------------------
   -- New_Unity_Reader --
   ----------------------
   
   function New_Xef_Generator return Xef_Generator_Ptr is
      This : Xef_Generator_Ptr := new Xef_Generator_Record;
   begin
      This.Ob                     := null;
      This.Level                  := 0;
      This.Flush                  := False;
      This.Unity_Generator        := null;
      This.Xef_File_Name          := No_Str_Id;
      This.Xef_File_Descriptor    := Invalid_FD;
      This.Template_Xef_File_Name := No_Str_Id;
      This.Task_Flag              := False;
      This.Header_Flag            := False;
      This.Types_Flag             := False;
      This.Dfb_Flag               := False;
      This.Datablock_Flag         := False;
      This.Section_Flag           := False;
      This.Sr_Flag                := False;
      This.Functional_Flag        := False;
      This.Program_Flag           := False;
      This.Settings_Flag          := False;

      return This;
   end New_Xef_Generator;
   
   -----------------------
   -- New_Xef_Generator --
   -----------------------
   
   function New_Xef_Generator
     (Generator : access Unity_Generator_Record;
      File_Name : String) return Xef_Generator_Ptr is
   begin
      return New_Xef_Generator (Generator, Enter_String (File_Name));
   end New_Xef_Generator;
   
   -----------------------
   -- New_Xef_Generator --
   -----------------------
   
   function New_Xef_Generator
     (Generator : access Unity_Generator_Record;
      File_Name : Str_Id) return Xef_Generator_Ptr is
      
      This : Xef_Generator_Ptr := New_Xef_Generator;
   begin
      This.Ob              := New_Output_Buffer;
      This.Unity_Generator := Generator;
      This.Xef_File_Name   := File_Name;
      
      return This;
   end New_Xef_Generator;
   
   ------------------------
   -- Free_Xef_Generator --
   ------------------------
   
   procedure Free_Xef_Generator (This : in out Xef_Generator_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
	(Xef_Generator_Record, Xef_Generator_Ptr);
   begin
      if This /= null then
	 if This.Ob /= null then
	    Free_Buffer (This.Ob);
	 end if;
	 
	 Free (This);
      end if;
   end Free_Xef_Generator;
      
   -----------------------
   -- Get_Xef_File_Name --
   -----------------------
   
   function Get_Xef_File_Name
     (This : access Xef_Generator_Record) return String is
   begin
      if This.Xef_File_Name /= No_Str_Id then
	 return Get_String (This.Xef_File_Name);
      else
	 return "";
      end if;
   end Get_Xef_File_Name;
   
   function Get_Xef_File_Name_Id
     (This : access Xef_Generator_Record) return Str_Id is
   begin
      return This.Xef_File_Name;
   end Get_Xef_File_Name_Id;
   
   -----------------------
   -- Set_Xef_File_Name --
   -----------------------
   
   procedure Set_Xef_File_Name
     (This      : access Xef_Generator_Record;
      File_Name : String) is
   begin
      This.Xef_File_Name := Enter_String (File_Name);
   end Set_Xef_File_Name;
   
   procedure Set_Xef_File_Name_Id
     (This      : access Xef_Generator_Record;
      File_Name : Str_Id) is
   begin
      This.Xef_File_Name := File_Name;
   end Set_Xef_File_Name_Id;
   
   --------------------------------
   -- Get_Template_Xef_File_Name --
   --------------------------------
   
   function Get_Template_Xef_File_Name
     (This : access Xef_Generator_Record) return String is
   begin
      if This.Template_Xef_File_Name /= No_Str_Id then
	 return Get_String (This.Template_Xef_File_Name);
      else
	 return "";
      end if;
   end Get_Template_Xef_File_Name;
   
   function Get_Template_Xef_File_Name_Id
     (This : access Xef_Generator_Record) return Str_Id is
   begin
      return This.Template_Xef_File_Name;
   end Get_Template_Xef_File_Name_Id;
   
   --------------------------------
   -- Set_Template_Xef_File_Name --
   --------------------------------
   
   procedure Set_Template_Xef_File_Name
     (This      : access Xef_Generator_Record;
      File_Name : String) is
   begin
      This.Template_Xef_File_Name := Enter_String (File_Name);
   end Set_Template_Xef_File_Name;
   
   procedure Set_Template_Xef_File_Name_Id
     (This      : access Xef_Generator_Record;
      File_Name : Str_Id) is
   begin
      This.Template_Xef_File_Name := File_Name;
   end Set_Template_Xef_File_Name_Id;
   
   -------------------------
   -- Get_Unity_Generator --
   -------------------------
   
   function Get_Unity_Generator
     (This : access Xef_Generator_Record) return access Unity_Generator_Record
   is
   begin
      return This.Unity_Generator;
   end Get_Unity_Generator;
   
   -------------------------
   -- Set_Unity_Generator --
   -------------------------
   
   procedure Set_Unity_Generator
     (This : access Xef_Generator_Record;
      G    : access Unity_Generator_Record) is
   begin
      This.Unity_Generator := G;
   end Set_Unity_Generator;
   
   ------------------
   -- Generate_Xef --
   ------------------
   
   procedure Generate_Xef (This : access Xef_Generator_Record) is
      
      use Input_Sources.File;
      
      Input : File_Input;
   begin
      --  ??? Do you get a default template file 
      
      if This.Template_Xef_File_Name /= No_Str_Id then
	 --  Reset the temporary buffer for the new parsing
	 
	 if This.Ob = null then
	    This.Ob := New_Output_Buffer;
	 else
	    Reset_Buffer (This.Ob);
	 end if;
	 
	 --  If no output file specified, we get the default output file name
	 --  "reflex.xef"
	 
	 if This.Xef_File_Name = No_Str_Id then
	    This.Set_Xef_File_Name ("reflex.xef");
	 end if;
	 
	 --  Open the output file
	 
	 This.Xef_File_Descriptor := 
	   Create_File (This.Get_Xef_File_Name, Text);

	 if This.Xef_File_Descriptor /= Invalid_FD then
	   
	   --  Initialize Sax Parser

	    declare
	       S : String := This.Get_Template_Xef_File_Name;
	    begin
	       Set_Public_Id (Input, Ada.Directories.Simple_Name (S));
	       Set_System_Id (Input, S);
	       
	       --  Open the template Xef file
	       
	       Open (S, Input);
	       
	       Set_Feature (This.all, Namespace_Prefixes_Feature, False);
	       Set_Feature (This.all, Validation_Feature, False);
	       
	       --  Parse and build the output Xef file to import in Unity
	       --  platform
	       
	       Parse (This.all, Input);
	       Close (Input);
	    end;	   
	    --  Close the Ouput File and we are on
	    
	    Close (This.Xef_File_Descriptor);
	   
	 else
	    Put_Line ("error when creating generated xef file");
	 end if;
	 
	 --  No Template specified, so cannot generate the Output Xef file
	 
      else
	 Put_Line ("cannot open Xef template file, file name is missing ");
      end if;
      
   exception
      when E: others =>
	 Put_Line ("Generate_Xef: exception");
	 raise Program_Error;
   end Generate_Xef;
   
   -------------------
   -- Generate_Task --
   -------------------

   procedure Generate_Task
     (Handler : in out Xef_Generator_Record;
      Atts    : in Sax.Attributes.Attributes'Class;
      Opening : in Boolean) is
   begin
      if Opening then
         Handler.Level := 1;
         Handler.Flush := False;

      else
         if not Handler.Task_Flag then
            --  Gen_Tasks_Declaration (Handler.Gui);
            Handler.Task_Flag := True;
         end if;
      end if;
   end Generate_Task;

   -----------------------------
   -- Generate_Header_Comment --
   -----------------------------

   procedure Generate_Header_Comment
     (Handler : in out Xef_Generator_Record;
      Opening : in Boolean) is
   begin
      if Handler.Header_Flag then
         if Opening then
	    
            --  in this case, a comment of the Header already exists in the
            --  xef file and we don't want to keep it
	    
            Handler.Level := 1;
            Handler.Flush := False;
         else
            --  we put the program comment just before the end of the Header
            --  Gen_Program_Comment (Handler.Gui);
            Handler.Header_Flag := False;
         end if;
      end if;
   end Generate_Header_Comment;

   --------------------
   -- Generate_Types --
   --------------------

   procedure Generate_Types (Handler : in out Xef_Generator_Record) is
      
      Types_Ob : Output_Buffer := 
        Handler.Unity_Generator.Get_Types_Output_Buffer;
   begin
      if not Handler.Types_Flag then
	 if Types_Ob /= null then
	    Append_Buffer_To_File (Types_Ob, Handler.Xef_File_Descriptor);
	 end if;
	 
         Handler.Types_Flag := True;
      end if;
   end Generate_Types;

   ------------------
   -- Generate_DFB --
   ------------------

   procedure Generate_DFB (Handler : in out Xef_Generator_Record) is
      
      Dfbs_Ob : Output_Buffer := 
        Handler.Unity_Generator.Get_Dfbs_Output_Buffer;
   begin
      Handler.Level := 1;
      Handler.Flush := False;

      if not Handler.Types_Flag then
	 Generate_Types (Handler);
      end if;

      if not Handler.Dfb_Flag then
	 if Dfbs_Ob /= null then
	    Append_Buffer_To_File (Dfbs_Ob, Handler.Xef_File_Descriptor);
	 end if;
	 
         Handler.Dfb_Flag := True;
      end if;
   end Generate_DFB;

   ------------------------
   -- Generate_Datablock --
   ------------------------

   procedure Generate_Datablock (Handler : in out Xef_Generator_Record) is
      
      Vars_Ob : Output_Buffer := 
        Handler.Unity_Generator.Get_Vars_Output_Buffer;
   begin
      if not Handler.DFB_Flag then
         Generate_DFB (Handler);
      end if;

      if not Handler.Datablock_Flag then
	 if Vars_Ob /= null then
	    Append_Buffer_To_File (Vars_Ob, Handler.Xef_File_Descriptor);
	 end if;
	 
         Handler.Datablock_Flag := True;
      end if;
   end Generate_Datablock;

   ----------------------
   -- Generate_Section --
   ----------------------

   procedure Generate_Section (Handler : in out Xef_Generator_Record) is
      
      Section_Ob : Output_Buffer := 
        Handler.Unity_Generator.Get_Sections_Output_Buffer;
   begin
      if not Handler.Datablock_Flag then
         Generate_Datablock (Handler);
      end if;

      if not Handler.Section_Flag then
	 if Section_Ob /= null then
	    Append_Buffer_To_File (Section_Ob, Handler.Xef_File_Descriptor);
	 end if;
	 
         Handler.Section_Flag := True;
      end if;
   end Generate_Section;

   -----------------
   -- Generate_Sr --
   -----------------

   procedure Generate_Sr (Handler : in out Xef_Generator_Record) is
      
      Sr_Ob : Output_Buffer := 
        Handler.Unity_Generator.Get_Sr_Output_Buffer;
   begin
      if not Handler.Section_Flag then
         Generate_Section (Handler);
      end if;

      if not Handler.Sr_Flag then
	 if Sr_Ob /= null then
	    Append_Buffer_To_File (Sr_Ob, Handler.Xef_File_Descriptor);
	 end if;
	 
         Handler.Sr_Flag := True;
      end if;
   end Generate_Sr;

   -------------------------
   -- Generate_Functional --
   -------------------------
   
   procedure Generate_Functional (Handler : in out Xef_Generator_Record) is
   begin
      if not Handler.Functional_Flag then
         Handler.Flush := False;
         Handler.Level := 1;
         Handler.Functional_Flag := True;
      else
         Handler.Flush := False;
         Handler.Level := 1;
      end if;
   end Generate_Functional;
   
   ----------------------
   -- Generate_Program --
   ----------------------

   procedure Generate_Program (Handler : in out Xef_Generator_Record) is
   begin
      if not Handler.Functional_Flag then
	 Generate_Functional (Handler);
      end if;
      
      if not Handler.Program_Flag then
         Handler.Flush := False;
         Handler.Level := 1;
--       Unity.Gen.Code.Gen_Application_Code (Handler.Gui);
         --  Unity.Gen.Code.Gen_Recurse_Application_Code (Handler.Gui);
         Handler.Program_Flag := True;
      else
         -- Don't keep what is already in the program
         Handler.Flush := False;
         Handler.Level := 1;
      end if;
   end Generate_Program;
   
   -----------------------
   -- Generate_Settings --
   -----------------------

   procedure Generate_Settings (Handler : in out Xef_Generator_Record) is
   begin
      if not Handler.Settings_Flag then
         Handler.Flush := False;
         Handler.Level := 1;
         --  Unity.Gen.Gen_Settings;
         Handler.Settings_Flag := True;
      end if;
   end Generate_Settings;

   --------------------------
   -- Start_Prefix_Mapping --
   --------------------------

   procedure Start_Prefix_Mapping
     (Handler : in out Xef_Generator_Record;
      Prefix  : Unicode.CES.Byte_Sequence;
      URI     : Unicode.CES.Byte_Sequence) is
   begin
      Reset_Buffer (Handler.Ob);
      Write_Str(Handler.Ob, "  xmlns:" & Prefix & "=""" & URI & """");
   end Start_Prefix_Mapping;

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Xef_Generator_Record;
      Ch      : Unicode.CES.Byte_Sequence) is
   begin
      --  Put_Line ( "Sax.Characters (" & Ch & ','
      --               & Integer'Image (Ch'Length) & ") ");
      Reset_Buffer (Handler.Ob);
      Write_Str (Handler.Ob, Latin1_To_Xml (Ch));
      Append_Buffer_To_File (Handler.Ob, Handler.Xef_File_Descriptor);
      null;
   end Characters;

   --------------------------
   -- Ignorable_Whitespace --
   --------------------------

   procedure Ignorable_Whitespace
     (Handler : in out Xef_Generator_Record;
      Ch      : Unicode.CES.Byte_Sequence) is
      
      --  Index : Natural := Ch'First;
   begin
      --Debug.Output ("Sax.Ignorable_Whitespace (");
      --while Index <= Ch'Last loop
      --   -- DCH: newer version of xmlada does support procedure and
      --   -- not function- C := Encoding.Read (Ch, Index);
      --   Encoding.Read (Ch, Index, C);
      --   Index := Index + Encoding.Width (C);
      --   Debug.Output (Unicode_Char'Image (C));
      --end loop;
      --Debug.Output (True, ',' & Integer'Image (Ch'Length) & ") ");
      null;
   end Ignorable_Whitespace;

   ----------------------------
   -- Processing_Instruction --
   ----------------------------

   procedure Processing_Instruction
     (Handler : in out Xef_Generator_Record;
      Target  : Unicode.CES.Byte_Sequence;
      Data    : Unicode.CES.Byte_Sequence)
   is
   begin
      --  Put_line ("Processing_Instruction (" & Target & ", " & Data & ")");
      null;
   end Processing_Instruction;

   --------------------------
   -- Internal_entity_Decl --
   --------------------------

   procedure Internal_Entity_Decl
     (Handler : in out Xef_Generator_Record;
      Name    : Unicode.CES.Byte_Sequence;
      Value   : Unicode.CES.Byte_Sequence)
   is
   begin
      null;
      --  Put_Line ("Internal_Entity_Decl (" & Name & ", " & Value & ")");
   end Internal_Entity_Decl;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Handler       : in out Xef_Generator_Record;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class) is
      
      use Sax.Attributes;
      
      Name_Idx : Integer := Get_Index(Atts, "name");
      Id_Idx   : Integer := Get_Index(Atts, "id");
      Cp_Atts  : Sax.Attributes.Attributes'Class := Atts;
   begin
      Handler.Flush := True;
      
      if Qname = "taskDesc" then
         null; --  Generate_Task (Handler, Cp_Atts, True);
	 
      elsif Qname = "contentHeader" then
         null; --  Handler.Header_Flag := True;

      elsif Qname = "comment" then
         null; --  Generate_Header_Comment (Handler, True);

      elsif Qname = "DDTSource" then
         null; --  Generate_Types (Handler);

      elsif Qname = "FBSource" then
         null; --  Generate_DFB (Handler);

      elsif Qname = "dataBlock" then
         null; --  Generate_Datablock (Handler);

      elsif Qname = "program" then
         null; --  Generate_Program (Handler);

      elsif QName = "masteredBusInfo" then
         null; --  Check_LES20_Presence(Handler, Cp_Atts);

      elsif QName = "PLC" then
         null; --  Check_Memory_Size(Handler, Cp_Atts);

      elsif QName = "settings" then
         null; --  Generate_Settings (Handler);

      elsif QName = "IOScreen" then
         --  if not Handler.DFB_Flag then
         --     --  Unity.Gen.Dfb.Gen_Application_Protos (Handler.Gui);
         --     Handler.DFB_Flag := True;
         --  end if;

         --  if not Handler.Datablock_Flag then
         --     --  Unity.Gen.Vars.Gen_Application_Vars (Handler.Gui);
         --     Handler.Datablock_Flag := True;
         --  end if;

         --  if not Handler.Program_Flag then
         --     --  Unity.Gen.Code.Gen_Recurse_Application_Code (Handler.Gui);
         --     Handler.Program_Flag := True;
         --  end if;
	 null;
	 
      elsif QName = "reflexddt" then
         Generate_Types (Handler);
	 Handler.Flush := False;
	 
      elsif QName = "reflexvars" then
         Generate_Datablock (Handler);
	 Handler.Flush := False;
	 
      elsif QName = "reflexdfb" then
         Generate_DFB (Handler);
	 Handler.Flush := False;
	 
      elsif QName = "reflexsection" then
         Generate_Section (Handler);
	 Handler.Flush := False;
	 
      elsif QName = "reflexsr" then
         Generate_Sr (Handler);
	 Handler.Flush := False;
      end if;
      
      if Handler.Flush then
	 Reset_Buffer (Handler.Ob);
	 Write_Str (Handler.Ob, "<" & QName);
        null; -----------Append_Buffer_To_File (Handler.Ob, "<" & QName);
	 
	 --  Put_Line(Current_File, Buffer.all);
	 --  Free (Buffer);
	 
         -- Dump attributes
	 for I in 0..Get_Length(Cp_Atts) - 1 loop
	    Write_Str 
	      (Handler.Ob, " " & 
		 Get_QName(Cp_Atts, I) & "=""" &
		 Get_Value(Cp_Atts, I) & """");
	    
	 end loop;
         Write_Str (Handler.Ob, ">");
	 Write_Eol (Handler.Ob);
	 
	 Append_Buffer_To_File (Handler.Ob, Handler.Xef_File_Descriptor);
      end if;
      
   exception
      when E: others =>
         --  Debug.Unknown_Exception(E, "Unity.Gen.Sax_XML.Start_Element");
	 raise Program_Error;
         --  Raise_Exception (Xml_Fatal_Error'Identity, "Start_Element");
   end Start_Element;


   -----------------
   -- End_Element --
   -----------------

   procedure End_Element
     (Handler       : in out Xef_Generator_Record;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "") is
      
      Atts : Sax.Attributes.Attributes;
   begin
      Handler.Flush := True;
      
      --  if Qname = "contentHeader" then
      --     Generate_Header_Comment (Handler, False);
      --  end if;
      
      --  if Qname = "taskDesc" then
      --     Generate_Task (Handler, Atts, false);
      --  end if;

      --  if Handler.Level > 0 then
      --     Handler.Level := Handler.Level - 1;

      --     if Handler.Level = 0 then
      --        Handler.Flush := True;
      --     end if;
      --  end if;
      
      if QName = "reflexddt" 
	or else QName = "reflexvars" 
	or else QName = "reflexdfb"
	or else QName = "reflexsection"
	or else QName = "reflexsr"
      then
	 Handler.Flush := False;
      end if;
      
      if Handler.Flush then
	 Reset_Buffer (Handler.Ob);
         Write_Str (Handler.Ob, "</" & QName & ">");
	 Write_Eol (Handler.Ob);
	 Append_Buffer_To_File (Handler.Ob, Handler.Xef_File_Descriptor);
      end if;
   end End_Element;

end Unity.Xef_Generators;
