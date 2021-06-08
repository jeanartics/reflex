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
-- Reflex is originally developed  by the Artics team at Grenoble (France). --
--                                                                          --
------------------------------------------------------------------------------


package body Artics.Graph.Multiplicities is
   
   ----------------
   -- Initialize --
   ----------------
   
   procedure Initialize is
   begin
--        Cells_Tables.Init (Multi_Tables);
--        Multiplicities_Tables.Increment_Last (Files);
null;
   end Initialize;
   
   ----------------------
   -- New_Multiplicity --
   ----------------------
   
   function New_Multiplicity
     (Source                  : Boolean;
      Typ                     : String;
      Attr                    : String;
      Value                   : String;
      Min                     : Integer;
      Max                     : String;
      Valid_Neighbors         : Strings_Lists.List;
      Valid_Neighbors_Allowed : Boolean) return Multiplicity_Ptr is
      
      M : Multiplicity_Ptr :=  new Multiplicity_Record;
   begin
      M.all := No_Multiplicity_Record;
      M.Source                  := Source;
      M.Typ                     := String_Find (Typ);
      M.Attr                    := String_Find (Attr);
      M.Value                   := String_Find (Value);
      M.Min                     := Min;
      M.Max                     := String_Find (Max);
      M.Valid_Neighbors         := Valid_Neighbors;
      M.Valid_Neighbors_Allowed := Valid_Neighbors_Allowed;
      
      return M;
   end New_Multiplicity;
   
   -----------
   -- Check --
   -----------
   
   function Check
     (M          : access Multiplicity_Record;
      G          : Graph_Ptr;
      Edge       : Cell_Ptr;
      Source     : Cell_Ptr;
      Target     : Cell_Ptr;
      Source_Out : Integer;
      Target_In  : Integer) return String is
   begin
      return "";
   end Check;
   
   ---------------------
   -- Check_Neighbors --
   ---------------------
   
   function Check_Neighbors
     (M      : access Multiplicity_Record;
      G      : Graph_Ptr;
      Edge   : Cell_Ptr;
      Source : Cell_Ptr;
      Target : Cell_Ptr) return Boolean is
   begin
      return False;
   end Check_Neighbors;
   
   --------------------
   -- Check_Terminal --
   --------------------
   
   function Check_Terminal
     (M        : access Multiplicity_Record;
      G        : Graph_Ptr;
      Terminal : Cell_Ptr;
      Edge     : Cell_Ptr) return Boolean is
   begin
      return False;
   end Check_Terminal;
   
   ----------------
   -- Check_Type --
   ----------------
   
   function Check_Type
     (M     : access Multiplicity_Record;
      G     : Graph_Ptr;
      Value : Cell_Ptr;
      Typ   : String) return Boolean is
   begin
      return False;
   end Check_Type;
   
   ----------------
   -- Check_Type --
   ----------------
   
   function Check_Type
     (M          : access Multiplicity_Record;
      G          : Graph_Ptr;
      Value      : Cell_Ptr;
      Typ        : String;
      Attr       : String;
      Attr_Value : String) return Boolean is
   begin
      return False;
   end Check_Type;
   
   -------------------
   -- Get_Max_Value --
   -------------------
   
   function Get_Max_Value (M : access Multiplicity_Record) return Integer is
   begin
      return 0;
   end Get_Max_Value;
   
   ------------------
   -- Is_Unlimited --
   ------------------
   
   function Is_Unlimited (M : access Multiplicity_Record) return Boolean is
   begin
      return False;
   end Is_Unlimited;
   
   ------------------------
   -- Clone_Multiplicity --
   ------------------------
   
   function Clone_Multiplicity (M : Multiplicity_Ptr) return Multiplicity_Ptr is
   begin
      return null;
   end Clone_Multiplicity;
   
end Artics.Graph.Multiplicities;
	  
	  
