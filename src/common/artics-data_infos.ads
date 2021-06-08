
with System;
with Artics.Types; use Artics.Types;
with Artics.Name_Stock;
with Artics.Table;
with Artics.Data_Types;

package Artics.Data_Infos is
   
   use Artics.Data_Types;
   
   procedure Register_Data
     (Name    : String;
      Dtype   : Data_Type;
      Daccess : System.Address);
   
   No_Dname : constant Dname_Id;
   
   procedure Initialize;
   
   function To_Dinfos_Id (Nid : Dname_Id) return Dinfos_Id;
   
   function To_Dname_Id (Did : Dinfos_Id) return Dname_Id;
   
   function Name_To_Dinfos_Id (S : String) return Dinfos_Id;
   
   function Name_To_Dname_Id (S : String) return Dname_Id;
   
   
   function Get_Data_Type (S : String) return Data_Type;
   
   function Get_Data_Access (S : String) return System.Address;
   
   function Get_Data (S : String) return String;
   
   function Set_Data 
     (S     : String; 
      Value : String) return String;
      
   function Get_Boolean_Value (S : String) return Boolean;
   
   procedure Set_Boolean_Value
     (S     : String; 
      Value : Boolean);
   
   function Get_Integer_Value (S : String) return Integer;
   procedure Set_Integer_Value
     (S     : String; 
      Value : Integer);
      
  function Get_Boolean_Or_Integer_Value (S : String) return Integer;
   procedure Set_Boolean_Or_Integer_Value
     (S     : String; 
      Value : Integer);
   
   function Get_Float_Value (S : String) return Float;
   procedure Set_Float_Value
     (S     : String; 
      Value : Float);
   
   function Get_Duration_Value (S : String) return Duration;
   procedure Set_Duration_Value
     (S     : String; 
      Value : Duration);
   
   procedure Dump_Data_Infos;
   
private
    
   First_Dinfos_Id  : Dinfos_Id := Dinfos_Id'First;
   Dinfos_Initial   : constant := 1000;
   Dinfos_Increment : constant := 100;
   
   type Data_Infos_Record is record
      Name    : Dname_Id;
      Dtype   : Data_Type;
      Daccess : System.Address;
   end record;
   
   package Data_Infos_Table is new Table.table
     (Table_Component_Type => Data_Infos_Record,
      Table_Index_Type     => Dinfos_Id'Base,
      Table_Low_Bound      => First_Dinfos_Id,
      Table_Initial        => Dinfos_Initial,
      Table_Increment      => Dinfos_Increment,
      Table_Name           => "Data_Infos");
   use Data_Infos_Table;
   
   type User_Infos is new Integer;
   User_Default : constant User_Infos := 0;
   
   package Dnames is new Name_Stock (Dname_id, User_Infos, User_Default);
   use Dnames;
   
   Last_Data_Infos_Id : Dinfos_Id := Dinfos_Id'First + 1;
   
   No_Dname : constant Dname_Id := Dname_id'First;
   
end Artics.Data_Infos;

