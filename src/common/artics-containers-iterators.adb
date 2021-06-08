
package body Artics.Containers.Iterators is

   --  Iteration support

   -----------
   -- Visit --
   -----------

   procedure Visit
     (It : in out Iterator'Class) is

   begin
      Reset (It);
      while not Is_End (It) loop
         Apply (Current_Item (It));
         Next (It);
      end loop;
   end Visit;

   --  Primitive implementations

   ----------
   -- Lock --
   ----------

   procedure Lock
     (C : in out The_Container) is
      pragma Warnings (Off, C);
   begin
      null;
   end Lock;


   ------------
   -- Unlock --
   ------------

   procedure Unlock
     (C : in out The_Container) is
      pragma Warnings (Off, C);
   begin
      null;
   end Unlock;

end Artics.Containers.Iterators;
