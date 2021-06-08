------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
--                                                                          --
-- Reflex is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public Licensea as pu  by the Free Soft- --
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

with System;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Gnat.Case_Util; use Gnat.Case_Util;

with Atree; use Atree;
with Types; use Types;
with Sinfo; use Sinfo;
with Nlists; use Nlists;
with Nmake; use Nmake;
with Tbuild; use Tbuild;

with Reflex.Formats; use Reflex.Formats;
with Reflex.External_Names; use Reflex.External_Names;
with Reflex.Predicates; use Reflex.Predicates;
with Reflex.Templates;
with Einfo; use Einfo;
with Ada.Text_IO; use Ada.Text_IO;
with Artics.Generic_Lists;

package body Reflex.Infos is
   
   function To_Address is new Ada.Unchecked_Conversion 
     (Reflex_Infos_Ptr, System.Address);
   
   function To_Reflex_Infos is new Ada.Unchecked_Conversion 
     (System.Address, Reflex_Infos_Ptr);
   pragma No_Strict_Aliasing (Reflex_Infos_Ptr);
   
   -----------------------------
   -- Initialize_Reflex_Infos --
   -----------------------------
   
   procedure Initialize_Reflex_Infos is
   begin
      Section_Type_Str     := Enter_String ("section");
      Sr_Type_Str          := Enter_String ("sr");
      Fb_Type_Str          := Enter_String ("fbd");
      Literal_Language_Str := Enter_String ("literal");
      Ladder_Language_Str  := Enter_String ("ladder");
      Flow_Language_Str    := Enter_String ("flow");
      Chart_Language_Str   := Enter_String ("chart");
   end Initialize_Reflex_Infos;
   
   -------------------------------
   -- String_From_Language_Type --
   -------------------------------
   
   function String_From_Language_Type (Lang : Language_Type) return String is
   begin
      case Lang is
      when Unknown_Language =>
         return "unknown";
      when Literal_Language =>
         return "literal";
      when Ladder_Language =>
         return "ladder";
      when Flow_Language =>
         return "flow";
      when Chart_Language =>
         return "chart";
      end case;
   end String_From_Language_Type;
   
   -----------------------------
   -- String_To_Language_Type --
   -----------------------------
   
   function String_To_Language_Type (S : String) return Language_Type is
      Sl : String := S;
   begin
      Trim (Sl, Both);
      To_Lower (Sl);
      if Sl = "unknown" then
         return Unknown_Language;
      elsif Sl = "literal" then
         return Literal_Language;
      elsif Sl = "ladder" then
         return Ladder_Language;
      elsif Sl = "flow" then
         return Flow_Language;
      elsif Sl = "chart" then
         return Chart_Language;
      else
         return Unknown_Language;
      end if;
   end String_To_Language_Type;
   
   ----------------------------
   -- Str_From_Language_Type --
   ----------------------------
   
   function Str_From_Language_Type (Lang : Language_Type) return Str_Id is
   begin
      case Lang is
      when Unknown_Language =>
         return No_Str_Id;
      when Literal_Language =>
         return Literal_Language_Str;
      when Ladder_Language =>
         return Ladder_Language_Str;
      when Flow_Language =>
         return Flow_Language_Str;
      when Chart_Language =>
         return Chart_Language_Str;
      end case;
   end Str_From_Language_Type;
   
   --------------------------
   -- Str_To_Language_Type --
   --------------------------
   
   function Str_To_Language_Type (Str : Str_Id) return Language_Type is
   begin
      if Str = No_Str_Id then
         return Unknown_Language;
      elsif Str = Literal_Language_Str then
         return Literal_Language;
      elsif Str = Ladder_Language_Str then
         return Ladder_Language;
      elsif Str = Flow_Language_Str then
         return Flow_Language;
      elsif Str = Chart_Language_Str then
         return Chart_Language;
      else
         return Unknown_Language;
      end if;
   end Str_To_Language_Type;
   
   ---------------------------------
   -- String_From_Generation_Type --
   ---------------------------------
   
   function String_From_Generation_Type (Gen : Generation_Type) return String
   is
   begin
      case Gen is
      when Unknown_Generation =>
         return "unknown";
      when Section_Type =>
         return "section";
      when Sr_Type =>
         return "sr";
      when Fb_Type =>
         return "fbd";
      end case;
   end String_From_Generation_Type;
   
   -------------------------------
   -- String_To_Generation_Type --
   -------------------------------
   
   function String_To_Generation_Type (S : String) return Generation_Type is
      Sl : String := S;
   begin
      Trim (Sl, Both);
      To_Lower (Sl);
      
      if Sl = "unknown" then
         return Unknown_Generation;
      elsif Sl = "section" then
         return Section_Type;
      elsif Sl = "sr" then
         return Sr_Type;
      elsif Sl = "fbd" then
         return Fb_Type;
      else
         return Unknown_Generation;
      end if;
   end String_To_Generation_Type;
   
   ------------------------------
   -- Str_From_Generation_Type --
   ------------------------------
   
   function Str_From_Generation_Type (Gen : Generation_Type) return Str_Id is
   begin
      case Gen is
      when Unknown_Generation =>
         return No_Str_Id;
      when Section_Type =>
         return Section_Type_Str;
      when Sr_Type =>
         return Sr_Type_Str;
      when Fb_Type =>
         return Fb_Type_Str;
      end case;
   end Str_From_Generation_Type;
   
   ----------------------------
   -- Str_To_Generation_Type --
   ----------------------------
   
   function Str_To_Generation_Type (Str : Str_Id) return Generation_Type is
   begin
      if Str = No_Str_Id then
         return Unknown_Generation;
      elsif Str = Section_Type_Str then
         return Section_Type;
      elsif Str = Sr_Type_Str then
         return Sr_Type;
      elsif Str = Fb_Type_Str then
         return Fb_Type;
      else
         return Unknown_Generation;
      end if;
   end Str_To_Generation_Type;
   
   ----------------------
   -- New_Reflex_Infos --
   ----------------------
   
   function New_Reflex_Infos return Reflex_Infos_Ptr is
   begin
      return new Reflex_Infos_Record'(No_Reflex_Infos_Record);
   end New_Reflex_Infos;
   
   ----------------------
   -- New_Reflex_Infos --
   ----------------------
   
   function New_Reflex_Infos (Node : Node_Id) return Reflex_Infos_Ptr is
      
      This : Reflex_Infos_Ptr := New_Reflex_Infos;
   begin
      Set_Rx_Infos (Node, To_Address (This));
      
      return This;
   end New_Reflex_Infos;
   
   -----------------------
   -- Free_Reflex_Infos --
   -----------------------
   
   procedure Free_Reflex_Infos (This : in out Reflex_Infos_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation 
        (Reflex_Infos_Record, Reflex_Infos_Ptr);
   begin
      if This /= null then
         Free (This);
      end if;
   end Free_Reflex_Infos;
   
   -----------------------
   -- Free_Reflex_Infos --
   -----------------------
   
   procedure Free_Reflex_Infos (Node : Node_Id) is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      Free_Reflex_Infos (This);
      Set_Rx_Infos (Node, System.Null_Address);
   end Free_Reflex_Infos;
   
   ----------------------
   -- Get_Reflex_Infos --
   ----------------------
   
   function Get_Reflex_Infos
     (Node : Node_Or_Entity_Id) return access Reflex_Infos_Record is
      
      This : Reflex_Infos_Ptr := To_Reflex_Infos (Get_Rx_Infos (Node));
   begin
      if This = null then
         This := New_Reflex_Infos (Node);
      end if;
      
      return This;
   end Get_Reflex_Infos;
   
   ----------------------
   -- Set_Reflex_Infos --
   ----------------------
   
   procedure Set_Reflex_Infos
     (Node  : Node_Or_Entity_Id;
      Infos : access Reflex_Infos_Record) is
   begin
      Set_Rx_Infos (Node, To_Address (Reflex_Infos_Ptr (Infos)));
   end Set_Reflex_Infos;
      
   -----------------
   -- Is_Expanded --
   -----------------
   
   function Is_Expanded (Node : Node_Id) return Boolean is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      return This.Expanded;
   end Is_Expanded;
   
   ------------------
   -- Set_Expanded --
   ------------------
   
   procedure Set_Expanded
     (Node : Node_Id;
      V    : Boolean) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      This.Expanded := V;
   end Set_Expanded;
   
   ------------------
   -- Is_Generated --
   ------------------
   
   function Is_Generated (Node : Node_Id) return Boolean is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      return This.Generated;
   end Is_Generated;
   
   -------------------
   -- Set_Generated --
   -------------------
   
   procedure Set_Generated
     (Node : Node_Id;
      V    : Boolean) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      This.Generated := V;
   end Set_Generated;
   
   ----------------
   -- Is_Covered --
   ----------------
   
   function Is_Covered (Node : Node_Id) return Boolean is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      return This.Covered;
   end Is_Covered;
   
   -----------------
   -- Set_Covered --
   -----------------
   
   procedure Set_Covered
     (Node : Node_Id;
      V    : Boolean) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      This.Covered := V;
   end Set_Covered;
   
   --------------------------
   -- Is_Expansion_Pending --
   --------------------------
   
   function Is_Expansion_Pending (Node : Node_Id) return Boolean is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      return This.Expansion_Pending;
   end Is_Expansion_Pending;
   
   ---------------------------
   -- Set_Expansion_Pending --
   ---------------------------
   
   procedure Set_Expansion_Pending
     (Node : Node_Id;
      V    : Boolean) is 
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      This.Expansion_Pending := V;
   end Set_Expansion_Pending;
   
   ---------------------------
   -- Is_Generation_Pending --
   ---------------------------
   
   function Is_Generation_Pending (Node : Node_Id) return Boolean is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      return This.Generation_Pending;
   end Is_Generation_Pending;
   
   ----------------------------
   -- Set_Generation_Pending --
   ----------------------------
   
   procedure Set_Generation_Pending
     (Node : Node_Id;
      V    : Boolean) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      This.Generation_Pending := V;
   end Set_Generation_Pending;
   
   -------------------------
   -- Is_Coverage_Pending --
   -------------------------
   
   function Is_Coverage_Pending (Node : Node_Id) return Boolean is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      return This.Coverage_Pending;
   end Is_Coverage_Pending;
   
   --------------------------
   -- Set_Coverage_Pending --
   --------------------------
   
   procedure Set_Coverage_Pending
     (Node : Node_Id;
      V    : Boolean) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      This.Coverage_Pending := V;
   end Set_Coverage_Pending;

   ---------------------
   -- Is_Homonym_Done --
   ---------------------
   
   function Is_Homonym_Done (E : Entity_Id) return Boolean is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Homonym_Done;
   end Is_Homonym_Done;
   
   ----------------------
   -- Set_Homonym_Done --
   ----------------------
   
   procedure Set_Homonym_Done
     (E : Entity_Id;
      V : Boolean) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Homonym_Done := V;
   end Set_Homonym_Done;
   
   ---------------------------
   -- Is_Negated_Assignment --
   ---------------------------
   
   function Is_Negated_Assignment (Node : Node_Id) return Boolean is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      return This.Negated_Assignment;
   end Is_Negated_Assignment;
     
   ----------------------------
   -- Set_Negated_Assignment --
   ----------------------------
   
   procedure Set_Negated_Assignment 
     (Node : Node_Id;
      V    : Boolean) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Node);
   begin
      This.Negated_Assignment := V;
   end Set_Negated_Assignment;
   
   --------------------
   -- Can_Be_Renamed --
   --------------------
   
   function Can_Be_Renamed (E : Entity_Id) return Boolean is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return False;
   end Can_Be_Renamed;
   
   ------------------
   -- Get_New_Name --
   ------------------
   
   function Get_New_Name (E : Entity_Id) return Name_Id is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.New_Name;
   end Get_New_Name;
   
   -------------------------
   -- Set_Entity_New_Name --
   -------------------------
   
   procedure Set_Entity_New_Name
     (E        : Entity_Id; 
      New_Name : Name_Id) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.New_Name := New_Name;
      Set_Chars (E, New_Name);
   end Set_Entity_New_Name;
   
   -----------------------
   -- Get_Entity_Address -
   -----------------------
   
   function Get_Entity_Address (E : Entity_Id) return Str_Id is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Addr;
   end Get_Entity_Address;
   
   ------------------------
   -- Set_Entity_Address --
   ------------------------
   
   procedure Set_Entity_Address
     (E    : Entity_Id; 
      Addr : Str_Id) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Addr := Addr;
   end Set_Entity_Address;
   
   ------------------------
   -- Get_Entity_Comment --
   ------------------------
   
   function Get_Entity_Comment (E : Entity_Id) return Str_Id is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Comment;
   end Get_Entity_Comment;
   
   ------------------------
   -- Set_Entity_Comment --
   ------------------------
   
   procedure Set_Entity_Comment
     (E       : Entity_Id; 
      Comment : Str_Id) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Comment := Comment;
   end Set_Entity_Comment;
   
   ------------------------------
   -- Access_To_Object_Defined --
   ------------------------------
   
   function Access_To_Object_Defined (E : Entity_Id) return Entity_Id is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Access_To_Obj_Defined;
   end Access_To_Object_Defined;
   
   ----------------------------------
   -- Set_Access_To_Object_Defined --
   ----------------------------------
   
   procedure Set_Access_To_Object_Defined
     (E  : Entity_Id;
      Ac : Entity_Id) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Access_To_Obj_Defined := Ac;
   end Set_Access_To_Object_Defined;
      
   --------------------------
   -- Is_Extra_Access_Type --
   --------------------------
   
   function Is_Extra_Access_Type (N : Node_Id) return Boolean is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (N);
   begin
      return This.Extra_Access_Type;
   end Is_Extra_Access_Type;
   
   ---------------------------
   -- Set_Extra_Access_Type --
   ---------------------------
   
   procedure Set_Extra_Access_Type
     (N : Node_Id;
      V : Boolean) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (N);
   begin
      This.Extra_Access_Type := V;
   end Set_Extra_Access_Type;
   
   -------------------
   -- Get_Object_Id --
   -------------------
   
   function Get_Object_Id (N : Node_Id) return Integer is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (N);
   begin
      return This.Object_Id;
   end Get_Object_Id;
   
   -------------------
   -- Set_Object_Id --
   -------------------
   
   procedure Set_Object_Id
     (N : Node_Id;
      V : Integer) is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (N);
   begin
      This.Object_Id := V;
   end Set_Object_Id;
   
   --------------------------
   -- Get_Object_Id_String --
   --------------------------
   
   function Get_Object_Id_String (N : Node_Id) return Integer is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (N);
   begin
      return This.Object_Id;
   end Get_Object_Id_String;
   
   ------------------
   -- Get_Language --
   ------------------
   
   function Get_Language (E : Entity_Id) return Language_Type is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Lang;
   end Get_Language;
   
   ------------------
   -- Set_Language --
   ------------------
   
   procedure Set_Language
     (E    : Entity_Id; 
      Lang : Language_Type) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Lang := Lang;
   end Set_Language;
   
   -------------------------
   -- Get_Generation_Type --
   -------------------------
   
   function Get_Generation_Type (E : Entity_Id) return Generation_Type is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Gen_Type;
   end Get_Generation_Type;
   
   -------------------------
   -- Set_Generation_Type --
   -------------------------
   
   procedure Set_Generation_Type
     (E   : Entity_Id; 
      Gen : Generation_Type) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Gen_Type := Gen;
   end Set_Generation_Type;
   
   --------------------------------
   -- Get_Function_Result_Formal --
   --------------------------------
   
   function Get_Function_Result_Formal (E : Entity_Id) return Entity_Id is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Function_Result_Formal;
   end Get_Function_Result_Formal;
   
   --------------------------------
   -- Set_Function_Result_Formal --
   --------------------------------
   
   procedure Set_Function_Result_Formal
     (E   : Entity_Id;
      Res : Entity_Id) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Function_Result_Formal := Res;
   end Set_Function_Result_Formal;
   
   ---------------------------
   -- Get_Internal_Function --
   ---------------------------
   
   function Get_Internal_Function (E : Entity_Id) return Boolean is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Internal_Function;
   end Get_Internal_Function;
   
   ---------------------------
   -- Set_Internal_Function --
   ---------------------------
   
   procedure Set_Internal_Function
     (E : Entity_Id;
      V : Boolean) is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Internal_Function := V;
   end Set_Internal_Function;
  
   -----------------------------
   -- Get_Subprogram_Instance --
   -----------------------------
   
   function Get_Subprogram_Instance (E : Entity_Id) return Entity_Id is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Instance;
   end Get_Subprogram_Instance;
   
   -----------------------------
   -- Set_Subprogram_Instance --
   -----------------------------
   
   procedure Set_Subprogram_Instance
     (E    : Entity_Id;
      Inst : Entity_Id) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Instance := Inst;
   end Set_Subprogram_Instance;
   
   --------------------------------
   -- Get_Subprogram_Global_Arec --
   --------------------------------
   
   function Get_Subprogram_Global_Arec
     (E : Entity_Id) return access Global_Arec_Record is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Global_Arec;
   end Get_Subprogram_Global_Arec;
   
   --------------------------------
   -- Set_Subprogram_Global_Arec --
   --------------------------------
   
   procedure Set_Subprogram_Global_Arec
     (E : Entity_Id;
      G : access Global_Arec_Record) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Global_Arec := G;
   end Set_Subprogram_Global_Arec;
   
   -------------------------
   -- Declare_Global_Arec --
   -------------------------
   
   function Declare_Global_Arec (E : Entity_Id) return Boolean is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Declare_Global_Arec;
   end Declare_Global_Arec;
   
   -----------------------------
   -- Set_Declare_Global_Arec --
   -----------------------------
   
   procedure Set_Declare_Global_Arec
     (E : Entity_Id;
      V : Boolean) is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Declare_Global_Arec := V;
   end Set_Declare_Global_Arec;
   
   ------------------
   -- Pending_Arec --
   ------------------
   
   function Pending_Arec (E : Entity_Id) return Boolean is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Pending_Arec;
   end Pending_Arec;
   
   ----------------------
   -- Set_Pending_Arec --
   ----------------------
   
   procedure Set_Pending_Arec
     (E : Entity_Id;
      V : Boolean) is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Pending_Arec := V;
   end Set_Pending_Arec;
   
   -----------------------
   -- Get_Formal_Global --
   -----------------------
   
   function Get_Formal_Global (Formal : Entity_Id) return Entity_Id is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Formal);
   begin
      return This.Formal_Global;
   end Get_Formal_Global;
   
   -----------------------
   -- Set_Formal_Global --
   -----------------------
   
   procedure Set_Formal_Global
     (Formal : Entity_Id;
      Global : Entity_Id) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (Formal);
   begin
      This.Formal_Global := Global;
   end Set_Formal_Global;
   
   ----------------------
   -- Get_Pin_Position --
   ----------------------
   
   function Get_Pin_Position (E : Entity_Id) return Natural is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Pin_Position;
   end Get_Pin_Position;
   
   ----------------------
   -- Set_Pin_Position --
   ----------------------
   
   procedure Set_Pin_Position
     (E : Entity_Id;
      P : Natural) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Pin_Position := P;
   end Set_Pin_Position;
   
   ------------------------
   -- Is_Entity_Reusable --
   ------------------------
   
   function Is_Entity_Reusable (E : Entity_Id) return Boolean is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Reusable;
   end Is_Entity_Reusable;
   
   -------------------------
   -- Set_Entity_Reusable --
   -------------------------
   
   procedure Set_Entity_Reusable
     (E : Entity_Id;
      V : Boolean) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Reusable := V;
   end Set_Entity_Reusable;
   
   ------------------------
   -- Reflex_No_Generate --
   ------------------------
   
   function Reflex_No_Generate (E : Entity_Id) return Boolean is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.No_Generate;
   end Reflex_No_Generate;
   
   ----------------------------
   -- Set_Reflex_No_Generate --
   ----------------------------
   
   procedure Set_Reflex_No_Generate
     (E : Entity_Id;
      V : Boolean) is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.No_Generate := V;
   end Set_Reflex_No_Generate;
   
   -----------------------------
   -- Has_Type_For_Generation --
   -----------------------------
   
   function Has_Type_For_Generation (E : Entity_Id) return Boolean is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return Present (This.Type_For_Generation);
   end Has_Type_For_Generation;
   
   -----------------------------
   -- Get_Type_For_Generation --
   -----------------------------
   
   function Get_Type_For_Generation (E : Entity_Id) return Entity_Id is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Type_For_Generation;
   end Get_Type_For_Generation;
   
   -----------------------------
   -- Set_Type_For_Generation --
   -----------------------------
   
   procedure Set_Type_For_Generation
     (E : Entity_Id;
      T : Entity_Id) is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Type_For_Generation := T;
   end Set_Type_For_Generation;
   
   ---------------------
   -- Has_Init_Record --
   ---------------------
   
   function Has_Init_Record (E : Entity_Id) return Boolean is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return Present (This.Init_Record);
   end Has_Init_Record;
   
   ---------------------
   -- Get_Init_Record --
   ---------------------
   
   function Get_Init_Record (E : Entity_Id) return Node_Id is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Init_Record;
   end Get_Init_Record;
   
   ---------------------
   -- Set_Init_Record --
   ---------------------
   
   procedure Set_Init_Record
     (E    : Entity_Id;
      Init : Node_Id) is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Init_Record := Init;
   end Set_Init_Record;

   -------------------
   -- Entity_In_Use --
   -------------------
   
   function Entity_In_Use (E : Entity_Id) return Boolean is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.In_Use;
   end Entity_In_Use;
   
   -----------------------
   -- Set_Entity_In_Use --
   -----------------------
   
   procedure Set_Entity_In_Use
     (E : Entity_Id;
      V : Boolean) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.In_Use := V;
   end Set_Entity_In_Use;
   
   --------------------------------------
   -- Get_Enumeration_Literal_Constant --
   --------------------------------------
   
   function Get_Enumeration_Literal_Constant (E : Entity_Id) return Entity_Id 
   is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Enum_Literal_Constant;
   end Get_Enumeration_Literal_Constant;
   
   --------------------------------------
   -- Set_Enumeration_Literal_Constant --
   --------------------------------------
   
   procedure Set_Enumeration_Literal_Constant
     (E        : Entity_Id;
      List_Cst : Entity_Id) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Enum_Literal_Constant := List_Cst;
   end Set_Enumeration_Literal_Constant;
   
   ---------------
   -- Is_Anonym --
   ---------------
   
   function Is_Anonym (E : Entity_Id) return Boolean is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Anonym;
   end Is_Anonym;
   
   -------------------
   -- Set_Is_Anonym --
   -------------------
   
   procedure Set_Is_Anonym
     (E : Entity_Id;
      V : Boolean) is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Anonym := V;
   end Set_Is_Anonym;
   
   -------------
   -- Get_Box --
   -------------
   
   function Get_Box (N : Node_Id) return access Box_Record'Class is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (N);
   begin
      return This.Box;
   end Get_Box;
   
   -------------
   -- Set_Box --
   -------------
   
   procedure Set_Box
     (N : Node_Id;
      B : access Box_Record'Class) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (N);
   begin
      This.Box := B;
   end Set_Box;
   
   --------------
   -- Get_Cell --
   --------------
   
   function Get_Cell (N : Node_Id) return access Cell_Record is
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (N);
   begin
      return This.Cell;
   end Get_Cell;
   
   --------------
   -- Set_Cell --
   --------------
   
   procedure Set_Cell
     (N : Node_Id;
      Cell : access Cell_Record) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (N);
   begin
      This.Cell := Cell;
   end Set_Cell;
   
   -----------------------------------
   -- Component_Need_Initialization --
   -----------------------------------
   
   function Component_Need_Initialization (Comp : Node_Id) return Boolean is
      
      Comp_Def : Node_Id;
      Mark     : Entity_Id;
      Full     : Entity_Id;
   begin
      if Present (Expression (Comp)) then
	 return True;
      end if;
      
      Comp_Def := Subtype_Indication (Component_Definition (Comp));
      
      if Present (Comp_Def) then
	 
	 if Nkind (Comp_Def) = N_Subtype_Indication then
	    Mark := Entity (Subtype_Mark (Comp_Def));
			    
	 elsif (Nkind (Comp_Def) = N_Identifier
		  or else Nkind (Comp_Def) = N_Expanded_Name)
	 then
	    Mark := Entity (Comp_Def);
	 end if;
	 
	 if Is_Record_Type (Mark) or else Is_Array_Type (Mark) then
	    Full := Full_View (Base_Type (Mark));
	    
	    return Present (Get_Init_Record (Mark));
	 end if;
      end if;
      
      return False;
   end Component_Need_Initialization;
   
   --------------------------
   -- Initialize_Component --
   --------------------------
   
   function Initialize_Component (Comp : Node_Id) return Node_Id is
      
      Comp_Def : Node_Id;
      Mark     : Entity_Id;
      Full     : Entity_Id;
      Aggr     : Node_Id;
      Init     : Node_Id;
   begin
      if Present (Expression (Comp)) then
	 return Expression (Comp);
      end if;
      
      Comp_Def := Subtype_Indication (Component_Definition (Comp));
      
      if Present (Comp_Def) then
	 
	 if Nkind (Comp_Def) = N_Subtype_Indication then
	    Mark := Entity (Subtype_Mark (Comp_Def));
			    
	 elsif (Nkind (Comp_Def) = N_Identifier
		  or else Nkind (Comp_Def) = N_Expanded_Name)
	 then
	    Mark := Entity (Comp_Def);
	 end if;
	 
	 if Is_Record_Type (Mark) or else Is_Array_Type (Mark) then
	    Full := Full_View (Base_Type (Mark));
	    Aggr := Get_Init_Record (Mark);
	    Init := Designate_Aggregate (Mark);
	    return New_Occurrence_Of (Defining_Identifier (Init), Sloc (Comp));
	 end if;
      end if;
      
      return Empty;
   end Initialize_Component;
   
   ----------------------
   -- Need_Init_Record --
   ----------------------
   
   function Need_Init_Record (T : Entity_Id) return Boolean is
      Full_T   : Entity_Id;
      Ptype    : Entity_Id;
      Comp     : Entity_Id;
      Par      : Node_Id;      
   begin
      
      Full_T := Base_Type (T);
      
      if Is_Record_Type (Full_T) then
	 if Is_Derived_Type (Full_T) 
	   and then Is_Tagged_Type (Full_T)
	 then
	    Ptype := Base_Type (Parent_Subtype (Full_T));
	    if Has_Init_Record (Ptype) then
	       return True;
	    end if;
	 end if;

	 Comp := First_Entity (Full_T);
	 while Present (Comp) loop
	    
	    if Ekind (Comp) = E_Component then
	       Par := Parent (Comp);
	       
	       if Component_Need_Initialization (Comp) then
		  return True;
	       end if;
	    end if;
	    
	    Next_Entity (Comp);
	 end loop;
	 
      elsif Is_Array_Type (Comp) then
	 null;
      end if;
      
      return False;
   end Need_Init_Record;
      
   ----------------------------------
   -- Create_Initialization_Record --
   ----------------------------------
   
   procedure Create_Initialization_Record 
     (N     : Node_Id;
      After : Node_id) is
      
      Def_Id    : Entity_Id := Defining_Identifier (N);
      Trec      : Entity_Id;
      Extension : Boolean;
      Ptype     : Entity_Id;
      Init      : Node_Id;
      Name      : Name_Id;
      Aggr      : Node_Id;
      Aggr_Id   : Node_Id;
      Dec       : Node_Id;
      Expr      : Node_Id;
   begin
      pragma Assert (Is_Record_Type (Def_Id));
      
      Trec := Base_Type (Def_Id);
      
      Extension := Is_Derived_Type (Trec) and then Is_Tagged_Type (Trec);
      
      if Need_Init_Record (Trec) then
	 
	 if Extension then
	    Ptype := Full_View (Base_Type (Parent_Subtype (Trec)));
	    Init  := Get_Init_Record (Ptype);
	    
	    if Present (Init) then
	       Expr := New_Occurrence_Of (Defining_Identifier (Init), Sloc (N));
	    else
	       Expr := Empty;
	    end if;
	    
	    Aggr  := Make_Extension_Aggregate
	      (Sloc                   => Sloc (N),
	       Ancestor_Part          => Expr,
	       Expressions            => No_List,
	       Component_Associations => New_List);
	 else
	    Aggr := Make_Aggregate
	      (Sloc                   => Sloc (N),
	       Expressions            => No_List,
	       Component_Associations => New_List);
	 end if;
	 
	 Name := New_Ghost_Name;
	 
	 Aggr_Id := Make_Defining_Identifier 
	   (Sloc  => Sloc (N),
	    Chars => Name);
	 Set_Reflex_No_Generate (Aggr_Id, True);
	 
	 Set_Etype (Aggr_Id, Trec);
	 Set_Ekind (Aggr_Id, E_Variable);
	 
	 --  JMA ??? For statment The scope is the englobing scope of the
	 --  statment
	 
	 Set_Scope (Aggr_Id, Scope (Trec));
	 
	 --  We dont link Aggr_Id in the entity list of the scope as it is 
	 --  not generated
	 
	 Dec := Make_Object_Declaration
	   (Sloc                => Sloc (N),
	    Defining_Identifier => Aggr_Id,
	    Constant_Present    => True,
	    Object_Definition   => 
	      New_Occurrence_Of (Trec, Sloc (N)),
	    Expression          => Aggr);
	 
	 Set_Reflex_No_Generate (Dec, True);
	 
	 --  Associate the Init Record Object to record
	 
	 Set_Init_Record (Def_Id, Dec);
	 
	 --  Insert after the full type definition of the record
	 --  Do we really need to link it in the declaration ???
	 
	 Insert_After (After, Dec);
      end if;
      
   end Create_Initialization_Record;
   
   -------------------------------------
   -- Append_Component_Initialization --
   -------------------------------------
   
   procedure Append_Component_Initialization
     (Comp : Node_Id;
      Rec  : Node_id) is
      
      Def_Id : Entity_Id := Defining_Identifier (Rec);
      Aggr   : Node_Id;
      Expr   : Node_Id;
      Init   : Node_Id;
   begin
      --  Put_Line
      --  	("Append_Component_Initialization Begin => " & 
      --  	   Get_String (Chars (Def_Id)));
      
      if Component_Need_Initialization (Comp) then
	 Expr := Initialize_Component (Comp);
	 Init := Get_Init_Record (Def_Id);
	 Aggr := Expression (Init);
	 
	 pragma Assert (Present (Aggr));
	 
	 Append
	   (Make_Component_Association 
	      (Sloc     => Sloc (Comp),
	       Choices  => New_List
		 (New_Occurrence_Of (Defining_Identifier (Comp), Sloc (Comp))),
	       Expression                => Expr),
	    Component_Associations (Aggr));
      end if;
   end Append_Component_Initialization;
   
   --------------------
   -- Get_Init_Decls --
   --------------------
   
   function Get_Init_Decls (E : Entity_Id) return List_Id is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Init_Decls;
   end Get_Init_Decls;
   
   --------------------
   -- Set_Init_Decls --
   --------------------
   
   procedure Set_Init_Decls
     (E     : Entity_Id;
      Decls : List_Id) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Init_Decls := Decls;
   end Set_Init_Decls;
   
   -----------------------
   -- Append_Init_Decls --
   -----------------------
   
   procedure Append_Init_Decls 
     (E    : Entity_Id;
      Decl : Node_Id) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      if This.Init_Decls = No_List then
	 This.Init_Decls := New_List;
      end if;
      
      Append (Decl, This.Init_Decls);
   end Append_Init_Decls;
   
   --------------------
   -- Get_Init_Stmts --
   --------------------
   
   function Get_Init_Stmts (E : Entity_Id) return List_Id is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      return This.Init_Stmts;
   end Get_Init_Stmts;
   
   --------------------
   -- Set_Init_Stmts --
   --------------------
   
   procedure Set_Init_Stmts
     (E     : Entity_Id;
      Stmts : List_Id) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      This.Init_Stmts := Stmts;
   end Set_Init_Stmts;
   
   -----------------------
   -- Append_Init_Stmts --
   -----------------------
   
   procedure Append_Init_Stmts 
     (E    : Entity_Id;
      Stmt : Node_Id) is
      
      This : Reflex_Infos_Ptr := Get_Reflex_Infos (E);
   begin
      if This.Init_Stmts = No_List then
	 This.Init_Stmts := New_List;
      end if;
      
      Append (Stmt, This.Init_Stmts);
   end Append_Init_Stmts;
   
end Reflex.Infos;
