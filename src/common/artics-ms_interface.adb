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

with Artics.Types; use Artics.Types;
with Artics.Namet; use Artics.Namet;

with Artics.Data_Types; use Artics.Data_Types;
with Artics.Data_Infos; use Artics.Data_Infos;

with Artics.Scomp.Tree; use Artics.Scomp.Tree;
with Artics.Scomp.Tree.Values; use Artics.Scomp.Tree.Values;
with Artics.Scomp.Tree.Variables; use Artics.Scomp.Tree.Variables;

with Artics.Logutils; use Artics.Logutils;

package body Artics.Ms_Interface is
   
   --------------------
   -- Write_Variable --
   --------------------
   
   procedure Write_Variable (V : Node_Id) is
      Val      : Node_Id;
      Var_Name : String := Variable_Name (V);
      Dtype    : Data_Types.Data_Type;
      Vtype    : Data_Types.Data_Type;
      
      Vb  : Boolean;
      Vbr : Boolean;
      Vi  : Integer;
      Vir : Integer;
      Vf  : Float;
      Vfr : Float;
      Vd  : Duration;
      Vdr : Duration;
   begin
      Log_Line
	("Write_Variable " & Var_Name & " Begin");
      
      Val := Value_Node (V);
      
      Dtype := Data_Infos.Get_Data_Type (Var_Name);
      Vtype := Value_Type (Val);
      
      if Dtype = Data_Types.Type_Boolean then
	 Vb := Boolean_Value (Val);
	 Log_Line ("     Boolean Write Value is " & Vb'Img);
	 Data_Infos.Set_Boolean_Value (Var_Name, Boolean_Value (Val));
	 Vbr := Data_Infos.Get_Boolean_Value (Var_Name);
	 Log_Line ("     Boolean Read Value is " & Vbr'Img);
	 
      elsif Dtype = Data_Types.Type_Integer then
	 Vi := Integer_Value (Val);
	 Log_Line ("     Integer Write Value is " & Vi'Img);
	 Data_Infos.Set_Integer_Value (Var_Name, Integer_Value (Val));
	 Vir := Data_Infos.Get_Integer_Value (Var_Name);
	 Log_Line ("     Integer Read Value is " & Vir'Img);
	 
      elsif Dtype = Data_Types.Type_Float then
	 Vf := Float_Value (Val);
	 Log_Line ("     Float Write Value is " & Vf'Img);
	 Data_Infos.Set_Float_Value (Var_Name, Float_Value (Val));
	 Vfr := Data_Infos.Get_Float_Value (Var_Name);
	 Log_Line ("     Float Read Value is " & Vfr'Img);
	   
      elsif Dtype = Data_Types.Type_Duration then
	 Vd := Duration_Value (Val);
	 Log_Line ("     Duration Write Value is " & Vd'Img);
	 Data_Infos.Set_Duration_Value (Var_Name, Duration_Value (Val));
	 Vdr := Data_Infos.Get_Duration_Value (Var_Name);
	 Log_Line ("     Duration Read Value is " & Vdr'Img);
      else
	 null;
      end if;
      
      Log_Line
	("Write_Variable " & Var_Name & " End");
   end Write_Variable;
   
   -------------------
   -- Read_Variable --
   -------------------
   
   procedure Read_Variable (V : Node_Id) is
      Vb       : Boolean;
      Vi       : Integer;
      Vf       : Float;
      Vd       : Duration;
      Val      : Node_Id;
      Var_Name : String := Get_String (Variable_Name (V));
      Dtype    : Data_Types.Data_Type;

   begin
      Log_Line
	("Read_Variable " & Var_Name & " Begin");
      Dtype := Data_Infos.Get_Data_Type (Var_Name);

      Val := Value_Node (V);

      if Dtype = Data_Types.Type_Boolean then
	 Vb := Data_Infos.Get_Boolean_Value (Var_Name);
	 Set_Boolean_Value (Val, Vb);
	 Log_Line ("     Boolean Read Value is " & Vb'Img);
	 
      elsif Dtype = Data_Types.Type_Integer then
	 Vi := Data_Infos.Get_Integer_Value (Var_Name);
	 Set_Integer_Value (Val, Vi);
	 Log_Line ("     Integer Read Value is " & Vi'Img);
	 
      elsif Dtype = Data_Types.Type_Float then
	 Vf := Data_Infos.Get_Float_Value (Var_Name);
	 Set_Float_Value (Val, Vf);
	 Log_Line ("     Float Read Value is " & Vf'Img);
	   
      elsif Dtype = Data_Types.Type_Duration then
	 Vd := Data_Infos.Get_Duration_Value (Var_Name);
	 Set_Duration_Value (Val, Vd);
	 Log_Line ("     Duration Read Value is " & Vd'Img);
	   
      else
	 null;
      end if;
      
      Log_Line
	("Read_Variable " & Var_Name & " End");
   end Read_Variable;
   
   ---------
   -- Run --
   ---------
   
   procedure Run is
   begin
      null;
   end Run;
   
   --------------
   -- Pause --
   --------------
   
   procedure Pause is
   begin
      null;
   end Pause;
   
   -----------
   -- Cycle --
   -----------
   
   procedure Cycle is
   begin
      null;
   end Cycle;
   
   --------------
   -- Shutdown --
   --------------
   
   procedure Shutdown  is
   begin
      null;
   end Shutdown;
   
end Artics.Ms_Interface;
