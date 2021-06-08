with Ada.Text_IO; use Ada.Text_IO;
with Arch;
procedure reflex_ar is
   version : constant string := "0.0";
begin
   Put_Line ("reflex archive builder v" & version);
   Arch.Archive;
end reflex_ar;
