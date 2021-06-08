with Linker;
with Ada.Text_IO; use Ada.Text_IO;
procedure Reflex_Link is
   version : constant string := "0.0";
   target  : constant string := "unity";
begin
   Put_Line("reflex linker v" & version & " - target " & target);
   
   Linker.Link;
end Reflex_Link;
