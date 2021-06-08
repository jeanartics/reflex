
with Artics.Maths.Tests;

package body Artics.Maths_Suite is

   Result : aliased Test_Suite;

   Test_Case_Maths : aliased Artics.Maths.Tests.Test_Case;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_Case_Maths'Access);

      return Result'Access;
   end Suite;

end Artics.Maths_Suite;
