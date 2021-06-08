with Ada.Text_IO; use Ada.Text_IO;
with Binder;
procedure reflex_bind is
   version : constant string := "0.0";
   target  : constant string := "";
begin
   Put_Line ("reflex binder v" & version);
   Binder.Bind;
end reflex_bind;
