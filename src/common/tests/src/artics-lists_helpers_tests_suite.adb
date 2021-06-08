
with Artics.Lists_Helpers_Tests.Tests;

package body Artics.Lists_Helpers_Tests_Suite is

   Result : aliased Test_Suite;

   Test_Case_Lists_Helpers : 
     aliased Artics.Lists_Helpers_Tests.Tests.Test_Case;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_Case_Lists_Helpers'Access);

      return Result'Access;
   end Suite;

end Artics.Lists_Helpers_Tests_Suite;
