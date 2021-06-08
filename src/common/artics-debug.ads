
with Ada.Exceptions; use Ada.Exceptions;

package Artics.Debug is
--pragma Preelaborate (Debug);

--  This package contains global flags used to control the inclusion
--  of debugging code

   Debug_Flag : constant Boolean := True;
   -- TO BE REMOVED

   Notification_Structure   : constant Boolean := True;
   Notification_Tag         : constant Boolean := True;
   Memory_Allocation        : constant Boolean := False;
   Memory_Allocation_Counter: constant Integer := 1000;
   Document_Loading         : constant Boolean := False;
   Sockets                  : constant Boolean := False;
   Tasks                    : constant Boolean := True;
   Tag_Update               : constant Boolean := True;
   Semaphores               : constant Boolean := False;
   Warnings                 : constant Boolean := False;
   Timers                   : constant Boolean := False;
   File_Path                : constant Boolean := False;
   GenericCom_Comm          : constant Boolean := False;
   Generic_Com_Main         : constant Boolean := False;
   Generic_Com_Config       : constant Boolean := False;
   Generic_Com_Bd           : constant Boolean := False;
   Generic_Com_Master       : constant Boolean := False;
   Generic_Com_Slave        : constant Boolean := False;
   Specific_Com_Main        : constant Boolean := False;
   Specific_Com_Master      : constant Boolean := False;
   Specific_Com_Slave       : constant Boolean := False;

   procedure Unknown_Exception
     (E: in Ada.Exceptions.Exception_Occurrence;
      F: in String);
   procedure Output (S: String);
   procedure Output (I: Integer);
   procedure Output (C: Character);
   procedure Output (F: Float);
   procedure Output;
   procedure Output (B: Boolean; S: String);
   --  This print out the debug message on the standard output

   generic
      Package_Name : String;
      Print        : Boolean;
   procedure Out_Line (S : String);

   generic
      Package_Name : String;
      Print        : Boolean;
   procedure Out_Function_Name (S : String);

   procedure Dump(E: Exception_Occurrence);

end Artics.Debug;
