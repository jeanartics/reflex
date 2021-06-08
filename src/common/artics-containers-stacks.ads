-- ------------------------------------------------------------------------ --
--  Filename        : containers-stacks.ads
--  Description     : A generic stack implementation based on list
--  Author          :                                                       --
--  Created On      : Tue Jan 28 15:02:18 2003                              --
--  Last Modified By: .                                                     --
--  Status          : Unknown, Use with caution!                            --
--                                                                          --
-- Copyright : (c) Itris 1999-2000. <open-control@itris.fr>                 --
-- ------------------------------------------------------------------------ --
--                                                                          --
-- This program is free software; you can redistribute it and-or modify     --
-- it under the terms of the GNU General Public License as published by     --
-- the Free Software Foundation; either version 2 of the License, or        --
-- (at your option) any later version.                                      --
--                                                                          --
-- This program is distributed in the hope that it will be useful,          --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of           --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            --
-- GNU General Public License for more details.                             --
--                                                                          --
-- You should have received a copy of the GNU General Public License        --
-- along with this software in a file called 'COPYING'; if not, write to:   --
--      Free Software Foundation, Inc.,                                     --
--      59 Temple Place, Suite 330,                                         --
--      Boston, MA                                                          --
--      02111-1307  USA                                                     --
--                                                                          --
-- For information see : Itris Inc. (http://www.itris.fr)                   --
------------------------------------------------------------------------------


with Cells;

generic
   type Item is private;

   No_Item : Item;

package Artics.Containers.Stacks is

   pragma Elaborate_Body;

   Stack_Error: exception;

   type Stack_Record is new Container_Record with private;
   type Stack_Id is private;

   procedure Delete_Stack
     (Stack: in out Stack_Id);

   function New_Stack
     ( Name : in String := "") return Stack_Id;

   function New_Stack
     ( Elmt : in Item;
       Name : in String) return Stack_Id;

   function Top_Item
     ( Stack: in Stack_Id) return Item;

   procedure Push_Item
     ( Elmt : in Item;
       Stack: in Stack_Id);

   function Pop_Item
     ( Stack: in Stack_Id) return Item;

   procedure Pop_Item
     ( Stack: in Stack_Id);

   procedure Pop_Items
     ( Stack: in Stack_Id;
       NbElt: in Positive);

   function Is_Empty_Stack
     ( Stack: in Stack_Id) return Boolean;

   function Length
     ( Stack: in Stack_Id) return Natural;

private

   function Alloc_Item return Item;

   procedure Delete_Item
     ( Elmt: Item);

   package Stacks_Cells is new Cells (Item        => Item,
                                      No_Item     => No_Item,
                                      Alloc_Item  => Alloc_Item,
                                      Delete_Item => Delete_Item);
   use Stacks_Cells;
   -- The cells manager.

   type String_Ptr is access all String;

   type Stack_Record is new Container_Record with record
      Name   : String_Ptr;
      Base   : Cell_Id;
      Top    : Cell_Id;
      Number : Natural;
   end record;

   type Stack_Id is access all Stack_Record'Class;

end Artics.Containers.Stacks;

