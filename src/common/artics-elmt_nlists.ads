------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
--                                                                          --
-- Reflex is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware Foundation; either version 3, or (at your option) any later version --
-- Reflex is distributed in the hope that it will be useful, but WITH-      --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License distributed with Reflex; see file COPYING3. If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- Reflex is originally developed  by the Artics team at Grenoble.          --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides facilities for manipulating lists of Objects (see
--  package Bd_Object for format and implementation of objects). The Link field
--  of the Objects is used as the forward pointer for these lists. See also
--  package Elists which provides another form of lists that are not threaded
--  through the objects (and therefore allow objects to be on multiple lists).

with System;
with Artics.Types; use Artics.Types;

generic
   type Elmt_T  is private;
   type Elmt_Id is range <>;

   type List_Id is range <>;

   Elmts_Initial    : Pos;
   Elmts_Increment  : Nat;

   Lists_Initial    : Pos;
   Lists_Increment  : Nat;

package Artics.Elmt_Nlists is

   --  An object list is a list of objects in a special format that means that
   --  objects can be on at most one such list. For each object list, a list
   --  header is allocated in the lists table, and a List_Id value references
   --  this header, which may be used to access the objects in the list using
   --  the set of routines that define this interface.

    --  Note: object lists can contain either objects
    Exception_No_Elmt : exception;

   type Elmt_T_Ptr is access all Elmt_T;

   type Union_Id is new Int;
   --  The type in the tree for a union of possible ID values

   Elmt_Low_Bound  : constant Elmt_Id := Elmt_Id'First;
   Elmt_High_Bound : constant Elmt_Id := Elmt_Id'Last;
   Error_Elmt      : constant Elmt_Id := Elmt_Low_Bound;
   No_Elmt         : constant Elmt_Id := Elmt_Low_Bound;
   First_Elmt_Id   : constant Elmt_Id := Elmt_Low_Bound;

   List_Low_Bound  : constant List_Id := List_Id'First;
   List_High_Bound : constant List_Id := List_Id'last;
   No_List_Id      : constant List_Id := List_Low_Bound;
   Error_List_Id   : constant List_Id := List_Low_Bound;
   First_List_Id   : constant List_Id := List_Low_Bound;

    function Last_List_Id return List_Id;
    pragma Inline (Last_List_Id);
    --  Returns Id of last allocated list header

    function Lists_Address return System.Address;
    pragma Inline (Lists_Address);
    --  Return address of Lists table (used in Back_End for Gigi call)

    function Num_Lists return Nat;
    pragma Inline (Num_Lists);
    --  Number of currently allocated lists

    function Is_Valid_Elmt (Obj : Elmt_Id) return Boolean;
    --  Return True if Obj in Elmts Table range

    function New_Elmt return Elmt_Id;
    --  Creates a new empty elmt in Elmts table and return it's Object Id.

    function New_Elmt (Elmt : Elmt_T) return Elmt_Id;
    --  Creates a new elmt, initialize it with Elmt, and return it's Object Id.

    procedure Delete_Elmt (Obj : Elmt_Id);
    --  Delete elemt referenced by Object Id Obj.

    procedure Put_Elmt
      (Elmt : Elmt_T; 
       Obj  : Elmt_Id);
    --  Put Elmt in position Obj in table Elmts.

    function Get_Elmt (Obj : Elmt_Id) return Elmt_T;
    procedure Set_Elmt
      (Obj  : Elmt_Id;
       Elmt : Elmt_T);
    --  Retreive the elmt in position Obj in table Elmts.

    function Get_Elmt_Ptr (Obj : Elmt_Id) return Elmt_T_Ptr;
    --  Retreive the address of the elmt stored in
    --  position Obj in table Elmts.

    function New_List return List_Id;
    --  Creates a new empty object list. Typically this is used to initialize
    --  a field in some other object which points to a object list where the 
    --  list is then subsequently filled in using Append calls.

    function New_List (Obj : Elmt_Id) return List_Id;
    --  Build a new list initially containing the given object

    function New_List_Copy (List : List_Id) return List_Id;
    --  Creates a new list containing copies (made with Atree.New_Copy) of every
    --  node in the original list. If the argument is No_List, then the returned
    --  result is No_List. If the argument is an empty list, then the returned
    --  result is a new empty list.

    function Empty_List return List_Id renames New_List;
    --  Used in contexts where an empty list (as opposed to an initially empty
    --  list to be filled in) is required.

    function First (List : List_Id) return Elmt_Id;
    pragma Inline (First);
    --  Obtains the first element of the given object list or, if the object 
    --  list has no items or is equal to No_List, then Empty is returned.

    function Last (List : List_Id) return Elmt_Id;
    pragma Inline (Last);
    --  Obtains the last element of the given object list or, if the object list
    --  has no items, then Empty is returned. It is an error to call Last with
    --  a Elmt_Id or No_List. (No_List is not considered to be the same as an
    --  empty object list).

    function List_Length (List : List_Id) return Nat;
    pragma Inline (List_Length);
    --  Returns number of items in the given list. It is an error to call
    --  this function with No_List (No_List is not considered to be the same
    --  as an empty list).

    function Next (Obj : Elmt_Id) return Elmt_Id;
    pragma Inline (Next);
    --  This function returns the next object on an object list, or Empty if 
    --  object is the last element of the object list. The argument must be a 
    --  member of a object list.

    procedure Next (Obj : in out Elmt_Id);
    pragma Inline (Next);
    --  Iterator :
    --  Equivalent to Object := Next (Object);

    function Prev (Obj : Elmt_Id) return Elmt_Id;
    pragma Inline (Prev);
    --  This function returns the previous object on an object list list, or 
    --  Empty if Obj is the first element of the object list. The argument 
    --  must be a member of an object list. Note that the implementation does 
    --  not maintain back pointers, so this function potentially requires 
    --  traversal of the entire list, or more accurately of the part of the
    --  list preceding Obj.

    function Pick
      (List  : List_Id; 
       Index : Pos) return Elmt_Id;
    --  Given a list, picks out the Index'th entry (1 = first entry). The
    --  caller must ensure that Index is in range.

    procedure Prev (Obj : in out Elmt_Id);
    pragma Inline (Prev);
    --  Iterator :
    --  Equivalent to Obj := Prev (Obj);

    function Is_Empty_List (List : List_Id) return Boolean;
    pragma Inline (Is_Empty_List);
    --  This function determines if a given list id references an object list 
    --  that contains no items. No_List is a not a legitimate argument.

    function Is_Non_Empty_List (List : List_Id) return Boolean;
    pragma Inline (Is_Non_Empty_List);
    --  This function determines if a given list id references an object list 
    --  that contains at least one item. No_List as an argument returns False.

    function Is_List_Member (Obj : Elmt_Id) return Boolean;
    pragma Inline (Is_List_Member);
    --  This function determines if a given object is a member of an object 
    --  list. It is an error for Obj to be Empty, or to be an object list.

    function List_Containing (Obj : Elmt_Id) return List_Id;
    pragma Inline (List_Containing);
    --  This function provides a pointer to the object list containing Obj.
    --  Obj must be a member of an object list.

    procedure Append
      (Obj  : Elmt_Id;
       To   : List_Id);
    --  Appends Obj at the end of object list To. Obj must be a non-empty object
    --  that is not already a member of an object list, and To must be an
    --  object list. An attempt to append an error object is ignored without
    --  complaint and the list is unchanged.

    procedure Append_To
      (To   : List_Id;
       Obj  : Elmt_Id);
    pragma Inline (Append_To);
    --  Like Append, but arguments are the other way round

    procedure Append_List
      (List : List_Id; 
       To   : List_Id);
    --  Appends object list List to the end of object list To. On return,
    --  List is reset to be empty.

    procedure Append_List_To
      (To   : List_Id; 
       List : List_Id);
    pragma Inline (Append_List_To);
    --  Like Append_List, but arguments are the other way round

    procedure Insert_After
      (After : Elmt_Id;
       Obj   : Elmt_Id);
    --  Insert Obj, which must be a non-empty object that is not already a
    --  member of an object list, immediately past object After, which must be 
    --  an object that is currently a member of an object list. An attempt to 
    --  insert an error object is ignored without complaint (and the list is 
    --  unchanged).

    procedure Insert_List_After
      (After : Elmt_Id; 
       List  : List_Id);
    --  Inserts the entire contents of object list List immediately after object
    --  After, which must be a member of an object list. On return, the object 
    --  list List is reset to be the empty object list.

    procedure Insert_Before
      (Before : Elmt_Id; 
       Obj    : Elmt_Id);
    --  Insert Obj, which must be a non-empty object that is not already a
    --  member of an object list, immediately before Before, which must be an 
    --  object that is currently a member of an object list. An attempt to 
    --  insert an error object is ignored without complaint (and the list is 
    --  unchanged).

    procedure Insert_List_Before
      (Before : Elmt_Id; 
       List   : List_Id);
    --  Inserts the entire contents of object list List immediately before 
    --  object Before, which must be a member of an object list. On return, the 
    --  object list List is reset to be the empty object list.

   function Position
     (List  : in List_Id;
      Elmt  : in Elmt_Id) return Nat;
   -- Return Position of an element in the list
   -- return 0 if not found
   -- return 1 if first element...

   procedure Prepend
     (Obj  : Elmt_Id;
      To   : List_Id);
    pragma Inline (Prepend);
    --  Prepends Obj at the start of object list To. Obj must be a non-empty
    --  object that is not already a member of an object list, and To must be an
    --  object list. An attempt to prepend an error object is ignored without
    --  complaint and the list is unchanged.

    procedure Prepend_To
      (To   : List_Id;
       Obj  : Elmt_Id);
    pragma Inline (Prepend_To);
    --  Like Prepend, but arguments are the other way round

    procedure Remove (Obj : Elmt_Id);
    --  Removes Obj, which must be an object that is a member of an object list,
    --  from this object list. The contents of Obj are not otherwise affected.

    function Remove_Head (List : List_Id) return Elmt_Id;
    --  Removes the head element of an object list, and returns the object 
    --  (whose contents are not otherwise affected) as the result. If the 
    --  object list is empty, then Empty is returned.

    function Remove_Next (Obj : Elmt_Id) return Elmt_Id;
    pragma Inline (Remove_Next);
    --  Removes the item immediately following the given object, and returns it
    --  as the result. If Obj is the last element of the list, then Empty is
    --  returned. Obj must be a member of a list. Unlike Remove, Remove_Next
    --  is fast and does not involve any list traversal.

    procedure Initialize;
    --  Called at the start of compilation of each new main source file to
    --  initialize the allocation of the list table.

    procedure Lock;
    --  Called to lock tables before back end is called

    function Parent (List : List_Id) return Elmt_Id;
    pragma Inline (Parent);
    --  Object lists may have a parent in the same way as an object. The 
    --  function accesses the Parent value, which is either Empty when a list 
    --  header is first created, or the value that has been set by Set_Parent.

    procedure Set_Parent
      (List : List_Id; 
       Obj  : Elmt_Id);
    pragma Inline (Set_Parent);
    --  Sets the parent field of the given list to reference the given object

    function No (List : List_Id) return Boolean;
    pragma Inline (No);
    --  Tests given Id for equality with No_List. This allows notations like
    --  "if No (Statements)" as opposed to "if Statements = No_List".

    function No (Obj : Elmt_Id) return Boolean;
    pragma Inline (No);
    --  return true if Obj is not in a list.

    function Present (List : List_Id) return Boolean;
    pragma Inline (Present);
    --  Tests given Id for inequality with No_List. This allows notations like
    --  "if Present (Statements)" as opposed to "if Statements /= No_List".

    function Present (Obj : Elmt_Id) return Boolean;
    pragma Inline (Present);
    --  Tests given Id for inequality with Empty. This allows notations like
    --  "if Present (Statements)" as opposed to "if Statements /= Empty".

    procedure Allocate_List_Tables (Obj : Elmt_Id);
    --  Called when objects table is expanded to include object N. This call
    --  makes sure that list structures internal to Nlists are adjusted
    --  apropriately to reflect this increase in the size of the objects table.

    function Next_Elmt_Address return System.Address;
    function Prev_Elmt_Address return System.Address;
    --  These functions return the addresses of the Next_Object and Prev_Object
    --  tables (used in Back_End for Gigi).

--    function p (U : Union_Id) return Elmt_Id;
    --  This function is intended for use from the debugger, it determines
    --  whether U is an Object_Id or List_Id, and calls the appropriate Parent
    --  function and returns the parent Object in either case. This is shorter
    --  to type, and avoids the overloading problem of using Parent. It
    --  should NEVER be used except from the debugger. If p is called with
    --  other than an object or list id value, it returns 99_999_999.

end Artics.Elmt_Nlists;




