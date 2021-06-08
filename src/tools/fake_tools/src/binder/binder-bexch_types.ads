with Ada.Containers.Formal_Ordered_Sets;
package Binder.Bexch_Types is


   type Bexch_Type_Record is record 
      Path : access String;
   end record;
   type Bexch_Type is access all Bexch_Type_Record;

   function New_Open (Path : String) return Bexch_Type;
   procedure Debug_Dump (B : Bexch_Type);

   procedure Debug_Dump_Txt_File (Path : String);
   
   procedure Debug_Back_Copy_File (Path : String);
   
   function Get_First_Content (B : Bexch_Type; Balise : String) return String;

end Binder.Bexch_Types;
