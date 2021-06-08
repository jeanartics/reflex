
with Ada.Finalization;

package Artics.Containers is

--   pragma Elaborate_Body;

   --  This package defined the root class of al containers.
   

   type Container_Record is abstract tagged private;
   
   type Container is access all Container_Record'Class;
   
   function Null_Container return Container is abstract;

private

   type Container_Record is 
     abstract new Ada.Finalization.Controlled with null record;

end Artics.Containers;
