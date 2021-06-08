
generic
   type Item is private;

   No_Item : Item;

   with function Alloc_Item return Item;

   with procedure Delete_Item (Elmt : Item);

package Artics.Cells is

   type Cell_Record is private;
   type Cell_Id is private;

   No_Cell : constant Cell_Id;

   function New_Cell return Cell_Id;
   pragma Inline (New_Cell);
   -- Create a new empty cell.

   function New_Cell
     (Next : in Cell_Id;
      Prev : in Cell_Id;
      Elmt : in Item) return Cell_Id;
   pragma Inline (New_Cell);
   -- Create a new cell holding Elmt. And initialize it with Next, Prev.

   procedure Delete_Cell
     (C : in out Cell_Id);
   pragma Inline (Delete_Cell);
   -- Detruit la cellule. Cette procedure detruit aussi l'item contenu
   -- par la cellule.

   function Get_Item
     (C : Cell_Id) return Item;
   pragma Inline (Get_Item);
   -- Retourne l'element contenu par C.

   procedure Set_Item
     (C    : Cell_Id;
      Elmt : Item);
   pragma Inline (Set_Item);
   -- Place l'Item Elmt dans cell.

   function Get_Next
     (C : Cell_Id) return Cell_Id;
   pragma Inline (Get_Next);
   -- Return the next Cell.

   function Get_Previous
     (C : Cell_Id) return Cell_Id;
   pragma Inline (Get_Previous);
   -- Return the previous Cell.

   procedure Set_Next
     (C    : Cell_Id;
      Next : Cell_Id);
   pragma Inline (Set_Next);
   -- Set the next cell of C to Next.

   procedure Set_Previous
     (C    : Cell_Id;
      Prev : Cell_Id);
   pragma Inline (Set_Previous);
   -- Set the previous cell of C to Prev.

private

   type Cell_Record is record
      Next : Cell_Id;
      -- The cell folowing this one.

      Prev : Cell_Id;
      -- The cell preceding this one.

      Elmt : Item;
      -- The Item holded by this cell.
   end record;

   type Cell_Id is access all Cell_Record;

   No_Cell : constant Cell_Id := null;
end Artics.Cells;
