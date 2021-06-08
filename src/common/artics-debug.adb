
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
use Ada.Float_Text_IO;
--with GNAT.Traceback.Symbolic;

package body Artics.Debug is


   -----------------------
   -- Unknown_Exception --
   -----------------------
   procedure Unknown_Exception
     (E: in Ada.Exceptions.Exception_Occurrence;
      F: in String) is
   begin
      Output("ERROR: Exception in " & F);
      Dump(E);
   end Unknown_Exception;

   ------------
   -- Output --
   ------------
   procedure Output (S: String) is
   begin
      Put(S);
   end Output;

   ------------
   -- Output --
   ------------
   procedure Output (I: Integer) is
   begin
      Put(I);
   end Output;

   ------------
   -- Output --
   ------------
   procedure Output (C: Character) is
   begin
      Put(C);
   end Output;

   ------------
   -- Output --
   ------------
   procedure Output (F: Float) is
   begin
      Put(F);
   end Output;

   ------------
   -- Output --
   ------------
   procedure Output is
   begin
      Put_Line(" ");
   end Output;

   ------------
   -- Output --
   ------------
   procedure Output(B: Boolean; S: String) is
   begin
      if B then
         Put_Line(S);
      end if;
   end Output;
   pragma Inline(Output);

   ----------
   -- Dump --
   ----------
   procedure Dump(E: Ada.Exceptions.Exception_Occurrence) is
   begin
      Put_Line(Exception_Name(E) & ":" & Exception_Message(E));
   --   Put(GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
   end Dump;


   --------------
   -- Out_Line --
   --------------

   procedure Out_Line (S : String) is
   begin
      if Print then
         Put_Line (Package_Name & ": " & S);
      end if;
   end Out_Line;


   -----------------------
   -- Out_Function_Name --
   -----------------------

   procedure Out_Function_Name (S : String) is
   begin
      if Print then
         Put_Line (Package_Name & ": " & S);
      end if;
   end Out_Function_Name;
end Artics.Debug;
