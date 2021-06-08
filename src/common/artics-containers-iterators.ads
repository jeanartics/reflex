
with Ada.Finalization;
--with Containers; use Containers;

generic
   type Item is private;
   -- The type of the user data store by the container.

   type The_Container is new Container_Record with private;
   -- The container to which the iterator is associated.

package Artics.Containers.Iterators is

   pragma Elaborate_Body;

   -- This package specifies the common protocol of the iterators to all
   -- Container classes. This package contains only abstract method, which
   -- must be overriden by the container to provide concrete iterator.

   -- This package is not intented to be public. Instead it must be instanciate
   -- in the private part of the container. The container must exports its own
   -- iterator which must conforms to the interface defined here.


   -- Example of use of this package for List.

   -- In List.ads we defined the interface for the List_Iterator :
   -- ... type list_iterator is limited private;
   -- ...
   --  function New_Iterator (List : List_Id) return List_Iterator;
   --  procedure Reset (It : List_Iterator);
   --   ... and so on ....
   --
   -- private
   --     package List_Iterator_Pkg is new Iterator (Item => Item,
   --                                          The_Container => List_Record);
   --    type List_Iterator is new Iterator with record
   --        Current_Cell : Cell_Id;
   --    end record;



   -- Active iteration --
   -----------------------

   -- type Iterator (<>) is abstract new Ada.Finalization.Controlled with private;
   type Iterator is abstract new Ada.Finalization.Controlled with private;

   function New_Iterator
     (C : The_Container) return Iterator'Class  is abstract;
   --  Return a reset Iterator bound to the specific Container.

   procedure Reset
     (It : in out Iterator) is abstract;
   --  Reset the Iterator to the beginning.

   procedure Next
     (It : in out Iterator) is abstract;
   --  Advance the Iterator to the next Item in the Container.

   function Is_End
     (It : Iterator) return Boolean is abstract;
   --  Return True if there are no more Items in the Container.

   function Current_Item
     (It : Iterator) return Item is abstract;
   --  Return a copy of the current Item.

   procedure Remove_Current_Item
     (It : Iterator) is abstract;
   --  Remove the current item.

--    function Item_At
--      (C     : The_Container;
--       Index : Positive) return Item_Ptr;

   -- Passive iteration --
   -----------------------

   generic
      with procedure Apply (Elmt : in Item);
   procedure Visit (It : in out Iterator'Class);
   --  Call Apply with a copy of each Item in the Container to which
   --  the iterator Using is bound.

private

   --  Support for concurrency protection. The base implementation of
   --  these procedures does nothing; derived types override as required.

   procedure Lock
     (C : in out The_Container);

   procedure Unlock
     (C : in out The_Container);

   --  Iteration

   type Iterator is abstract new Ada.Finalization.Controlled with null record;
--      Contain : The_Container;
--   end record;

end Artics.Containers.Iterators;
