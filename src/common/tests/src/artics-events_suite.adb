with Artics.Events_Tests;

package body Artics.Events_Suite is

   Result : aliased Test_Suite;

   Test_Case_Events : aliased Artics.Events_Tests.Test_Case;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_Case_Events'Access);

      return Result'Access;
   end Suite;

end Artics.Events_Suite;
