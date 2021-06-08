------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 2012-2015, Free Software Foundation, Inc.         --
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

package body Artics.Dynamic_HTables is

   -------------------
   -- Set_Next_Elmt --
   -------------------
   
   procedure Set_Next_Elmt (E : Elmt_Ptr; Next : Elmt_Ptr) is
   begin
      E.Next := Next;
   end Set_Next_Elmt;
   
   ---------------
   -- Next_Elmt --
   ---------------
   
   function Next_Elmt (E : Elmt_Ptr) return Elmt_Ptr is
   begin
      return E.Next;
   end Next_Elmt;
   
   -------------
   -- Get_Key --
   -------------
   
   function  Get_Key  (E : Elmt_Ptr) return Key is
   begin
      return E.K;
   end Get_Key;
   
   ----------------
   -- New_Htable --
   ----------------
   
   function New_Htable return Htable is
   begin
      return new Htable_Data;
   end New_Htable;
   
   ---------
   -- Get --
   ---------
   
   function  Get
     (T : Htable; 
      K : Key) return Element is
      
      Elmt  : Elmt_Ptr;
   begin
      if T = null then
	 return No_Element;
      end if;
      
      Elmt := T.Table (Hash (K));
      
      loop
	 if Elmt = Null_Ptr then
	    return No_Element;
	    
	 elsif Equal (Get_Key (Elmt), K) then
	    return Elmt.E;
	    
	 else
	    Elmt := Next_Elmt (Elmt);
	 end if;
      end loop;
   end Get;

   ------------
   -- Remove --
   ------------
   
   procedure Remove
     (T : Htable; 
      K : Key) is
      
      Index : constant Header_Num := Hash (K);
      Elmt  : Elmt_Ptr;
      Nxt   : Elmt_Ptr;
   begin
      if T = null then
	 return;
      end if;
      
      Elmt := T.Table (Index);
      
      if Elmt = Null_Ptr then
	 return;
	 
      elsif Equal (Get_Key (Elmt), K) then
	 T.Table (Index) := Next_Elmt (Elmt);
	 Free_Elmt (Elmt.E);
	 Free (Elmt);
	 
      else
	 loop
	    Nxt :=  Next_Elmt (Elmt);
	    
	    if Nxt = Null_Ptr then
	       return;
	       
	    elsif Equal (Get_Key (Nxt), K) then
	       Set_Next_Elmt (Elmt, Next_Elmt (Nxt));
	       Free_Elmt (Nxt.E);
	       Free (Nxt);
	       return;
	       
	    else
	       Elmt := Nxt;
	    end if;
	 end loop;
      end if;
   end Remove;
      
   -----------
   -- Reset --
   -----------
   
   procedure Reset (T : in out Htable) is
      procedure Free is
	 new Ada.Unchecked_Deallocation (Htable_Data, Htable);
      
      Elmt : Elmt_Ptr;
      Nxt  : Elmt_Ptr;
   begin
      if T = null then
	 return;
      end if;

      for J in T.Table'Range loop
	 
	 Elmt := T.Table (J);
	 while Elmt /= Null_Ptr loop
	    Nxt := Next_Elmt (Elmt);
	    Free_Elmt (Elmt.E);
	    Free (Elmt);
	    Elmt := Nxt;
	 end loop;
	 
	 T.Table (J) := Null_Ptr;
      end loop;
      
      Free (T);
   end Reset;
   
   ---------
   -- Set --
   ---------
   
   procedure Set (T : in out Htable; E : Elmt_Ptr) is
      Index : Header_Num;
      
   begin
      if T = null then
	 T := new Htable_Data;
      end if;
      
      Index := Hash (Get_Key (E));
      Set_Next_Elmt (E, T.Table (Index));
      T.Table (Index) := E;
   end Set;
   
   ---------
   -- Set --
   ---------
   
   procedure Set
     (T : in out Htable; 
      K : Key;
      E : Element) is
      
      Eptr : Elmt_Ptr := new Element_Wrapper'(K, E, Null_Ptr);
   begin
      Set (T, Eptr);
   end Set;
   
   --------------
   -- Iterator --
   --------------
   
   ------------------
   -- New_Iterator --
   ------------------
   
   function New_Iterator (T : Htable) return Hash_Iterator is
      It : Hash_Iterator;
   begin
      It.T              := T;
      It.Iterator_Index := Header_Num'First;
      It.Iterator_Ptr   := T.Table (It.Iterator_Index);
      Get_Non_Null (It);
      return It;
   end New_Iterator;
   
   -------------
   -- Is_Last --
   -------------
   
   function Is_End (It : Hash_Iterator) return Boolean is
   begin
      return It.Iterator_Ptr = Null_Ptr;
   end Is_End;

   ----------
   -- Next --
   ----------
   
   procedure Next (It : in out Hash_Iterator) is 
      T : Htable := It.T;
   begin
      if T = null then
	 raise Hash_Iterator_Error;
      end if;
      
      It.Iterator_Ptr := Next_Elmt (It.Iterator_Ptr);
      Get_Non_Null (It);
   end Next;
   
   -------------
   -- Current --
   -------------
   
   function Current (It : Hash_Iterator) return Element is
   begin
      return It.Iterator_Ptr.E;
   end Current;
   
   ------------------
   -- Get_Non_Null --
   ------------------
   
   procedure Get_Non_Null (It : in out Hash_Iterator) is
      T : Htable := It.T;
   begin
      if T = null then
	 It.Iterator_Ptr := Null_Ptr;
      end if;
      
      while It.Iterator_Ptr = Null_Ptr  loop
	 if It.Iterator_Index = T.Table'Last then
	    It.Iterator_Ptr := Null_Ptr;
	    return;
	 end if;
	 
	 It.Iterator_Index := It.Iterator_Index + 1;
	 It.Iterator_Ptr   := T.Table (It.Iterator_Index);
      end loop;
   end Get_Non_Null;
   
end Artics.Dynamic_HTables;
