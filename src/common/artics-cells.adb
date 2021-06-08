
with Ada.Unchecked_Deallocation;

package body Artics.Cells is

   procedure Free is new Ada.Unchecked_Deallocation (Cell_Record, Cell_Id);

   --------------
   -- New_Cell --
   --------------

   function New_Cell return Cell_Id is

   begin
      return new Cell_Record'(Next => No_Cell,
                              Prev => No_Cell,
                              Elmt => No_Item);
   end New_Cell;


   --------------
   -- New_Cell --
   --------------

   function New_Cell
     (Next : in Cell_Id;
      Prev : in Cell_Id;
      Elmt : in Item) return Cell_Id is
   begin
      return new Cell_Record'(Next => Next,
                              Prev => Prev,
                              Elmt => Elmt);
   end New_Cell;


   -----------------
   -- Delete_Cell --
   -----------------

   procedure Delete_Cell
     (C : in out Cell_Id) is

   begin
      Free (C);
   end Delete_Cell;


   --------------
   -- Get_Item --
   --------------

   function Get_Item
     (C : Cell_Id) return Item is
   begin
      return C.Elmt;
   end Get_Item;


   --------------
   -- Set_Item --
   --------------

   procedure Set_Item
     (C    : Cell_Id;
      Elmt : Item) is
   begin
      C.Elmt := Elmt;
   end Set_Item;


   --------------
   -- Get_Next --
   --------------

   function Get_Next
     (C : Cell_Id) return Cell_Id is
   begin
      return C.Next;
   end Get_Next;


   ------------------
   -- Get_Previous --
   ------------------

   function Get_Previous
     (C : Cell_Id) return Cell_Id is
   begin
      return C.Prev;
   end Get_Previous;


   --------------
   -- Set_Next --
   --------------

   procedure Set_Next
     (C    : Cell_Id;
      Next : Cell_Id) is
   begin
      C.Next := Next;
   end Set_Next;


   ------------------
   -- Set_Previous --
   ------------------

   procedure Set_Previous
     (C    : Cell_Id;
      Prev : Cell_Id) is

   begin
      C.Prev := Prev;
   end Set_Previous;

end Artics.Cells;
