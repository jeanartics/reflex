
with Artics.Matrices.Tests;

package body Artics.Matrices_Suite is

   Result : aliased Test_Suite;

   Test_Case_Matrices : aliased Artics.Matrices.Tests.Test_Case;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_Case_Matrices'Access);

      return Result'Access;
   end Suite;

end Artics.Matrices_Suite;
