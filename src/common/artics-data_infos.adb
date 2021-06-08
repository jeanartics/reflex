---
--- This file and its contents are the property of Itris Automation Square.
--- This file contains confidential proprietary information.
--- The reproduction, distribution, utilization or the communication
--- of this file or any part thereof is strictly prohibited.
--- Offenders will be held liable for the payment of damages.
---
--- Copyright 1999-2009 Itris Automation Square. All rights reserved.
---
--- Last author       : $Author: dev $
--- Last revision     : $Rev: 10717 $
--- Last Changed Date : $Date: 2012-08-28 14:01:34 +0200 (mar., 28 août 2012) $
---

with Ada.Text_Io;

with Ada.Unchecked_Conversion;
with Ada.Strings;
with Ada.Characters.Handling;
with Artics.Logutils; use Artics.Logutils;

package body Artics.Data_Infos is
   
   type Boolean_Ptr  is access all Boolean;
   type Integer_Ptr  is access all Integer;
   type Float_Ptr    is access all Float;
   type Duration_Ptr is access all Duration;
   
   function To_Boolean is 
      new Ada.Unchecked_Conversion (System.Address, Boolean_Ptr);
   
   function To_Integer is 
      new Ada.Unchecked_Conversion (System.Address, Integer_Ptr);
   
   function To_Float is 
      new Ada.Unchecked_Conversion (System.Address, Float_Ptr);
   
   function To_Duration is 
      new Ada.Unchecked_Conversion (System.Address, Duration_Ptr);
   
   -----------------
   -- To_Dinfo_Id --
   -----------------
   
   function To_Dinfos_Id (Nid : Dname_Id) return Dinfos_Id is
   begin
      if Nid /= No_Name then
	 return Dinfos_Id (Get_Name_Table_Int (Nid));
      else
	 return No_Dinfos_Id;
      end if;
   end To_Dinfos_Id;
   
   -----------------
   -- To_Dname_Id --
   -----------------
   
   function To_Dname_Id (Did : Dinfos_Id) return Dname_Id is
   begin
      return Data_Infos_Table.Table (Did).Name;
   end To_Dname_Id;
   
   -----------------------
   -- Name_To_Dinfos_Id --
   -----------------------
   
   function Name_To_Dinfos_Id (S : String) return Dinfos_Id is
      Nid : Dname_Id;
   begin
      Nid := Name_To_Dname_Id (S);
      if Nid /= No_Name then
	 return Dinfos_Id (Get_Name_Table_Int (Nid));
      else
	 return No_Dinfos_Id;
      end if;
   end Name_To_Dinfos_Id;
   
   ----------------------
   -- Name_To_Dname_Id --
   ----------------------
   
   function Name_To_Dname_Id (S : String) return Dname_Id is
   begin
      return String_Look_Up (Ada.Characters.Handling.To_Lower (S));
   end Name_To_Dname_Id;
   
   -------------------
   -- Get_Data_Type --
   -------------------
   
   function Get_Data_Type (S : String) return Data_Type is
      
      Nid     : Dname_Id;
      Did     : Dinfos_Id;
      Dtype   : Data_Type;
      Daccess : System.Address;
   begin
      Log_Line ("S   => " & S);
      Nid := Name_To_Dname_Id (S);
      Log_Line ("Nid => " & Nid'Img);
      if Nid /= No_Name then
	 Did := Dinfos_Id (Get_Name_Table_Int (Nid));
	 Dtype   := Data_Infos_Table.Table (Did).Dtype;
	 Daccess := Data_Infos_Table.Table (Did).Daccess;
	 
	 return Dtype;
      else
	 return Unknown_Type;
      end if;
   end Get_Data_Type;
   
   ---------------------
   -- Get_Data_Access --
   ---------------------
   
   function Get_Data_Access (S : String) return System.Address is
      
      Nid     : Dname_Id;
      Did     : Dinfos_Id;
      Dtype   : Data_Type;
      Daccess : System.Address;
   begin
      Nid := Name_To_Dname_Id (S);
      if Nid /= No_Name then
	 Did := Dinfos_Id (Get_Name_Table_Int (Nid));
	 Dtype   := Data_Infos_Table.Table (Did).Dtype;
	 Daccess := Data_Infos_Table.Table (Did).Daccess;
	 
	 return Daccess;
      else
	 return System.Null_Address;
      end if;
   end Get_Data_Access;
   
   -------------------
   -- Register_Data --
   -------------------
   
   procedure Register_Data
     (Name    : String;
      Dtype   : Data_Type;
      Daccess : System.Address) is
      
      Nid : Dname_Id;
      Did : Dinfos_Id;
   begin
      Nid := String_Find (Ada.Characters.Handling.To_Lower (Name));
      Data_Infos_Table.Increment_Last;
      Did := Data_Infos_Table.Last;
      Data_Infos_Table.Table (Did) := Data_Infos_Record'(Nid, Dtype, Daccess);
      Set_Name_Table_Int (Nid, Integer (Did));
      
      Last_Data_Infos_Id := Did;
   end Register_Data;
   
   ------------------------
   -- Register_Task_Data --
   ------------------------
   
   procedure Register_Package_Data is
   begin
      null;
   end Register_Package_Data;
   
   ----------------
   -- Initialize --
   ----------------
   
   procedure Initialize is
   begin
      Data_Infos_Table.Init;
      Data_Infos_Table.Increment_Last;
      Dnames.Initialize;
      
--        for I in Dname_Id'First  .. (Dname_Id'First + 256) loop
--           Nid := String_Enter ("a");
--           Log_Line (" Nid => " & Nid'Img);
--        end loop;
      
   end Initialize;
   
   --------------
   -- Get_Data --
   --------------
   
   function Get_Data (S : String) return String is
      
      Nid     : Dname_Id;
      Did     : Dinfos_Id;
      Dtype   : Data_Type;
      Daccess : System.Address;
      Vb_Ptr  : Boolean_Ptr;
      Vi_Ptr  : Integer_Ptr;
      Vf_Ptr  : Float_Ptr;
      Vd_Ptr  : Duration_Ptr;
      Vb      : Boolean;
      Vi      : Integer;
      Vf      : Float;
      Vd      : Duration;
   begin
      Nid := Name_To_Dname_Id (S);
      if Nid /= No_Name then
	 Did := Dinfos_Id (Get_Name_Table_Int (Nid));
	 
	 Dtype   := Data_Infos_Table.Table (Did).Dtype;
	 Daccess := Data_Infos_Table.Table (Did).Daccess;
	 
	 case Dtype is
	    when Type_Boolean =>
	       Vb_Ptr := To_Boolean (Daccess);
	       Vb := Vb_Ptr.all;
	       return Vb'Img;
	       
	    when Type_Integer =>
	       Vi_Ptr := To_Integer (Daccess);
	       Vi := Vi_Ptr.all;
	       return Vi'Img;
	       
	    when Type_Float   =>
	       Vf_Ptr := To_Float (Daccess);
	       Vf := Vf_Ptr.all;
	       return Vf'Img;
	       
	    when Type_Duration   =>
	       Vd_Ptr := To_Duration (Daccess);
	       Vd := Vd_Ptr.all;
	       return Vd'Img;
	       
            when Type_String =>
               return "";
               
            when Type_Reference =>
               return "";
               
	    when Unknown_Type =>
	       return "error: " & S & " is not found";
	 end case;	    
	 
      else
	 return "error: " & S & " is not found";
      end if;
   end Get_Data;

   --------------
   -- Set_Data --
   --------------
   
   function Set_Data 
     (s     : String; 
      Value : String) return String is
      
      Nid     : Dname_Id;
      Did     : Dinfos_Id;
      Dtype   : Data_Type;
      Daccess : System.Address;
      Vb_Ptr  : Boolean_Ptr;
      Vi_Ptr  : Integer_Ptr;
      Vf_Ptr  : Float_Ptr;
      Vd_Ptr  : Duration_Ptr;
      Vb      : Boolean;
      Vi      : Integer;
      Vf      : Float;
      Vd      : Duration;
   begin
      Nid := Name_To_Dname_Id (S);
      if Nid /= No_Name then
	 Did := Dinfos_Id (Get_Name_Table_Int (Nid));
	 
	 Dtype   := Data_Infos_Table.Table (Did).Dtype;
	 Daccess := Data_Infos_Table.Table (Did).Daccess;
	 
	 case Dtype is
	    when Type_Boolean =>
	       Vb_Ptr := To_Boolean (Daccess);
	       Vb_Ptr.all := Boolean'Value (Value);
	       Vb := Vb_Ptr.all;
	       return Vb'Img;
	       
	    when Type_Integer =>
	       Vi_Ptr := To_Integer (Daccess);
	       Vi_Ptr.all := Integer'Value (Value);
	       Vi := Vi_Ptr.all;
	       return Vi'Img;
	       
	    when Type_Float   =>
	       Vf_Ptr := To_Float (Daccess);
	       Vf_Ptr.all := Float'Value (Value);
	       Vf := Vf_Ptr.all;
	       return Vf'Img;
	       
	    when Type_Duration   =>
	       Vd_Ptr := To_Duration (Daccess);
	       Vd_Ptr.all := Duration'Value (Value);
	       Vd := Vd_Ptr.all;
	       return Vd'Img;
	       
            when Type_Reference =>
               return "";
               
            when Type_String =>
               return "";
               
	    when Unknown_Type =>
	       return "error: " & S & " is not found";
	 end case;	    
	 
      else
	 return "error: " & S & " is not found";
      end if;
   end Set_Data;
   
   -----------------------
   -- Get_Boolean_Value --
   -----------------------

   function Get_Boolean_Value (S : String) return Boolean is
      Nid     : Dname_Id;
      Did     : Dinfos_Id;
      Dtype   : Data_Type;
      Daccess : System.Address;
      V_Ptr   : Boolean_Ptr;
   begin
      Nid := Name_To_Dname_Id (S);
      if Nid /= No_Name then
	 Did := Dinfos_Id (Get_Name_Table_Int (Nid));
	 
	 Dtype   := Data_Infos_Table.Table (Did).Dtype;
	 Daccess := Data_Infos_Table.Table (Did).Daccess;
	 
	 if Dtype = Type_Boolean then
	    V_Ptr := To_Boolean (Daccess);
	    return V_Ptr.all;
	 end if;
      end if;
      
      return False;
   end Get_Boolean_Value;
   
   -----------------------
   -- Set_Boolean_Value --
   -----------------------
   
   procedure Set_Boolean_Value
     (S     : String; 
      Value : Boolean) is
      
      Nid     : Dname_Id;
      Did     : Dinfos_Id;
      Dtype   : Data_Type;
      Daccess : System.Address;
      V_Ptr   : Boolean_Ptr;
   begin
      Log_Line ("Set_Boolean_Value  Begin");
      Nid := Name_To_Dname_Id (S);
      Log_Line ("Nid => " & Nid'Img);
      if Nid /= No_Name then
	 Did := Dinfos_Id (Get_Name_Table_Int (Nid));
	 
	 Dtype   := Data_Infos_Table.Table (Did).Dtype;
	 Daccess := Data_Infos_Table.Table (Did).Daccess;
	 Log_Line ("Dtype => " & Dtype'Img);
	 
	 if Dtype = Type_Boolean then
	    V_Ptr := To_Boolean (Daccess);
	    V_Ptr.all := Value;
	    Log_Line ("V_Ptr.all => " & V_Ptr.all'Img);
	 end if;
      end if;
      Log_Line ("Set_Boolean_Value  End");
   end Set_Boolean_Value;

   -----------------------
   -- Get_Integer_Value --
   -----------------------
   
   function Get_Integer_Value (S : String) return Integer is
      Nid     : Dname_Id;
      Did     : Dinfos_Id;
      Dtype   : Data_Type;
      Daccess : System.Address;
      V_Ptr   : Integer_Ptr;
      V       : Integer;
   begin
      Nid := Name_To_Dname_Id (S);
      if Nid /= No_Name then
	 Did := Dinfos_Id (Get_Name_Table_Int (Nid));
	 
	 Dtype   := Data_Infos_Table.Table (Did).Dtype;
	 Daccess := Data_Infos_Table.Table (Did).Daccess;
	 
	 if Dtype = Type_Integer then
	    V_Ptr := To_Integer (Daccess);
	    V := V_Ptr.all;
	    return V;
	 end if;
      end if;
      
      return 0;
   end Get_Integer_Value;
   
   -----------------------
   -- Set_Integer_Value --
   -----------------------
   
   procedure Set_Integer_Value
     (S     : String; 
      Value : Integer) is
      
      Nid     : Dname_Id;
      Did     : Dinfos_Id;
      Dtype   : Data_Type;
      Daccess : System.Address;
      V_Ptr   : Integer_Ptr;
   begin
      Nid := Name_To_Dname_Id (S);
      if Nid /= No_Name then
	 Did := Dinfos_Id (Get_Name_Table_Int (Nid));
	 
	 Dtype   := Data_Infos_Table.Table (Did).Dtype;
	 Daccess := Data_Infos_Table.Table (Did).Daccess;
	 
	 if Dtype = Type_Integer then
	    V_Ptr := To_Integer (Daccess);
	    V_Ptr.all := Value;
	 end if;
      end if;
   end Set_Integer_Value;
   
   ----------------------------------
   -- Get_Boolean_Or_Integer_Value --
   ----------------------------------
   
   function Get_Boolean_Or_Integer_Value (S : String) return Integer is
      Nid     : Dname_Id;
      Did     : Dinfos_Id;
      Dtype   : Data_Type;
      Daccess : System.Address;
   begin
      Nid := Name_To_Dname_Id (S);
      if Nid /= No_Name then
	 Did := Dinfos_Id (Get_Name_Table_Int (Nid));
	 
	 Dtype   := Data_Infos_Table.Table (Did).Dtype;
	 Daccess := Data_Infos_Table.Table (Did).Daccess;
	 
         if Dtype = Type_Integer then
            return Get_Integer_Value (S);
         elsif Dtype = Type_Boolean then
            if Get_Boolean_Value (S) then
               return 1;
            else
               return 0;
            end if;
	 end if;
      end if;
      
      return 0;
   end Get_Boolean_Or_Integer_Value;
   
   ----------------------------------
   -- Set_Boolean_Or_Integer_Value --
   ----------------------------------
   
   procedure Set_Boolean_Or_Integer_Value
     (S     : String; 
      Value : Integer) is
      
      Nid     : Dname_Id;
      Did     : Dinfos_Id;
      Dtype   : Data_Type;
      Daccess : System.Address;
   begin
      Nid := Name_To_Dname_Id (S);
      if Nid /= No_Name then
	 Did := Dinfos_Id (Get_Name_Table_Int (Nid));
         
         Dtype   := Data_Infos_Table.Table (Did).Dtype;
         Daccess := Data_Infos_Table.Table (Did).Daccess;
         
	 if Dtype = Type_Integer then
            Set_Integer_Value (S, Value);
         elsif Dtype = Type_Boolean then
            Set_Boolean_Value (S, Value /= 0);
         end if;
      end if;
   end Set_Boolean_Or_Integer_Value;

   ---------------------
   -- Get_Float_Value --
   ---------------------
   
   function Get_Float_Value (S : String) return Float is
      Nid     : Dname_Id;
      Did     : Dinfos_Id;
      Dtype   : Data_Type;
      Daccess : System.Address;
      V_Ptr   : Float_Ptr;
      V       : Float := 0.0;
   begin
      Nid := Name_To_Dname_Id (S);
      if Nid /= No_Name then
	 Did := Dinfos_Id (Get_Name_Table_Int (Nid));
	 
	 Dtype   := Data_Infos_Table.Table (Did).Dtype;
	 Daccess := Data_Infos_Table.Table (Did).Daccess;
	 
	 if Dtype = Type_Float then
	    V_Ptr := To_Float (Daccess);
	    V := V_Ptr.all;
	    return V;
	 end if;
      end if;
      
      return V;
   end Get_Float_Value;
   
   ---------------------
   -- Set_Float_Value --
   ---------------------
   
   procedure Set_Float_Value
     (S     : String; 
      Value : Float) is
      
      Nid     : Dname_Id;
      Did     : Dinfos_Id;
      Dtype   : Data_Type;
      Daccess : System.Address;
      V_Ptr   : Float_Ptr;
   begin
      Nid := Name_To_Dname_Id (S);
      if Nid /= No_Name then
	 Did := Dinfos_Id (Get_Name_Table_Int (Nid));
	 
	 Dtype   := Data_Infos_Table.Table (Did).Dtype;
	 Daccess := Data_Infos_Table.Table (Did).Daccess;
	 
	 if Dtype = Type_Float then
	    V_Ptr := To_Float (Daccess);
	    V_Ptr.all := Value;
	 end if;
      end if;
   end Set_Float_Value;
   
   ------------------------
   -- Get_Duration_Value --
   ------------------------
   
   function Get_Duration_Value (S : String) return Duration is
      Nid     : Dname_Id;
      Did     : Dinfos_Id;
      Dtype   : Data_Type;
      Daccess : System.Address;
      V_Ptr   : Duration_Ptr;
      V       : Duration := 0.0;
   begin
      Nid := Name_To_Dname_Id (S);
      if Nid /= No_Name then
	 Did := Dinfos_Id (Get_Name_Table_Int (Nid));
	 
	 Dtype   := Data_Infos_Table.Table (Did).Dtype;
	 Daccess := Data_Infos_Table.Table (Did).Daccess;
	 
	 if Dtype = Type_Duration then
	    V_Ptr := To_Duration (Daccess);
	    V := V_Ptr.all;
	    return V;
	 end if;
      end if;
      
      return V;
   end Get_Duration_Value;
   
   ------------------------
   -- Set_Duration_Value --
   ------------------------
   
   procedure Set_Duration_Value
     (S     : String; 
      Value : Duration) is
      
      Nid     : Dname_Id;
      Did     : Dinfos_Id;
      Dtype   : Data_Type;
      Daccess : System.Address;
      V_Ptr   : Duration_Ptr;
   begin
      Nid := Name_To_Dname_Id (S);
      if Nid /= No_Name then
	 Did := Dinfos_Id (Get_Name_Table_Int (Nid));
	 
	 Dtype   := Data_Infos_Table.Table (Did).Dtype;
	 Daccess := Data_Infos_Table.Table (Did).Daccess;
	 
	 if Dtype = Type_Duration then
	    V_Ptr := To_Duration (Daccess);
	    V_Ptr.all := Value;
	 end if;
      end if;
   end Set_Duration_Value;
   
   ---------------------
   -- Dump_Data_Infos --
   ---------------------
   
   procedure Dump_Data_Infos is
      
      Name    : Dname_Id;
      Dtype   : Data_Type;
      Daccess : System.Address;
   begin
      for I in Dinfos_Id range (Dinfos_Id'First + 1) .. Last_Data_Infos_Id loop
	 Name    := Data_Infos_Table.Table (I).Name;
	 Dtype   := Data_Infos_Table.Table (I).Dtype;
	 Daccess := Data_Infos_Table.Table (I).Daccess;
	 
	 Log_Line ("Name         =>  " & Get_String (Name));
	 Log_Line ("   Data_Type =>  " & Dtype'Img);
	 Log_New_Line;
      end loop;
   end Dump_Data_Infos;
   
end Artics.Data_Infos;

