
with Artics.Dynamic_Arrays_tests;

package body Artics.Dynamic_Arrays_Suite is

   Result : aliased Test_Suite;

   Test_Case_Darrays : aliased Artics.Dynamic_Arrays_tests.Test_Case;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_Case_Darrays'Access);

      return Result'Access;
   end Suite;

end Artics.Dynamic_Arrays_Suite;
