------------------------------------------------------------------------------
--                                                                          --
--                         REFLEX COMPILER COMPONENTS                       --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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
-- Reflex is a fork from the GNAT compiler. GNAT was originally developed   --
-- by the GNAT team at  New York University. Extensive  contributions to    --
-- GNAT were provided by Ada Core Technologies Inc. Reflex is developed  by --
-- the Artics team at Grenoble.                                             --
--                                                                          --
------------------------------------------------------------------------------

with Namet; use Namet;
with Table;

package body Snames is

   --  Table used to record convention identifiers

   type Convention_Id_Entry is record
      Name       : Name_Id;
      Convention : Convention_Id;
   end record;

   package Convention_Identifiers is new Table.Table (
     Table_Component_Type => Convention_Id_Entry,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 1,
     Table_Initial        => 50,
     Table_Increment      => 200,
     Table_Name           => "Name_Convention_Identifiers");

   --  Table of names to be set by Initialize. Each name is terminated by a
   --  single #, and the end of the list is marked by a null entry, i.e. by
   --  two # marks in succession. Note that the table does not include the
   --  entries for a-z, since these are initialized by Namet itself.

   Preset_Names : constant String :=
     "_parent#" &
     "_tag#" &
     "off#" &
     "space#" &
     "time#" &
     "dynamic_predicate#" &
     "post#" &
     "postcondition#" &
     "pre#" &
     "precondition#" &
     "predicate#" &
     "predicate_failure#" &
     "preelaborable_initialization#" &
     "preelaborate#" &
     "static_predicate#" &
     "type_invariant#" &
     "_alignment#" &
     "_assign#" &
     "_chain#" &
     "_clean#" &
     "_controller#" &
     "_entry_bodies#" &
     "_expunge#" &
     "_final_list#" &
     "_idepth#" &
     "_init#" &
     "_local_final_list#" &
     "_master#" &
     "_object#" &
     "_priority#" &
     "_process_atsd#" &
     "_secondary_stack#" &
     "_service#" &
     "_size#" &
     "_tags#" &
     "_trace_sp#" &
     "initialize#" &
     "finalize#" &
     "next#" &
     "prev#" &
     "allocate#" &
     "deallocate#" &
     "dereference#" &
     "decimal_io#" &
     "enumeration_io#" &
     "fixed_io#" &
     "float_io#" &
     "integer_io#" &
     "modular_io#" &
     "a_textio#" &
     "a_witeio#" &
     "const#" &
     "error#" &
     "go#" &
     "put#" &
     "put_line#" &
     "to#" &
     "finalization#" &
     "finalization_root#" &
     "interfaces#" &
     "standard#" &
     "system#" &
     "text_io#" &
     "wide_text_io#" &
     "addr#" &
     "async#" &
     "origin#" &
     "params#" &
     "partition#" &
     "partition_interface#" &
     "ras#" &
     "result#" &
     "subp_id#" &
     "Oabs#" &
     "Oand#" &
     "Omod#" &
     "Onot#" &
     "Oor#" &
     "Orem#" &
     "Oxor#" &
     "Oeq#" &
     "One#" &
     "Olt#" &
     "Ole#" &
     "Ogt#" &
     "Oge#" &
     "Oadd#" &
     "Osubtract#" &
     "Oconcat#" &
     "Omultiply#" &
     "Odivide#" &
     "Oexpon#" &
     "c_pass_by_copy#" &
     "compile_time_warning#" &
     "component_alignment#" &
     "convention_identifier#" &
     "discard_names#" &
     "elaboration_checks#" &
     "eliminate#" &
     "explicit_overriding#" &
     "extensions_allowed#" &
     "external_name_casing#" &
     "initialize_scalars#" &
     "long_float#" &
     "no_run_time#" &
     "normalize_scalars#" &
     "persistent_data#" &
     "persistent_object#" &
     "ravenscar#" &
     "restricted_run_time#" &
     "restrictions#" &
     "restriction_warnings#" &
     "style_checks#" &
     "suppress#" &
     "test_case#" &
     "universal_data#" &
     "unsuppress#" &
     "validity_checks#" &
     "warnings#" &
     "annotate#" &
     "assert#" &
     "asynchronous#" &
     "attach_handler#" &
     "comment#" &
     "common_object#" &
     "convention#" &
     "contract_cases#" &
     "debug#" &
     "depends#" &
     "elaborate#" &
     "elaborate_all#" &
     "elaborate_body#" &
     "export#" &
     "export_exception#" &
     "export_function#" &
     "export_object#" &
     "export_procedure#" &
     "external#" &
     "finalize_storage_only#" &
     "global#" &
     "ident#" &
     "import#" &
     "import_exception#" &
     "import_function#" &
     "import_object#" &
     "import_procedure#" &
     "initial_condition#" &
     "initializes#" &
     "inline#" &
     "inline_always#" &
     "inline_generic#" &
     "inspection_point#" &
     "interface#" &
     "interface_name#" &
     "invariant#" &
     "keep_names#" &
     "link_with#" &
     "linker_alias#" &
     "linker_options#" &
     "list#" &
     "memory_size#" &
     "no_return#" &
     "obsolescent#" &
     "optimize#" &
     "optional_overriding#" &
     "overriding#" &
     "pack#" &
     "pure#" &
     "pure_function#" &
     "source_reference#" &
     "suppress_all#" &
     "suppress_debug_info#" &
     "suppress_initialization#" &
     "system_name#" &
     "unimplemented#" &
     "unmodified#" &
     "unreferenced#" &
     "unreferenced_objects#" &
     "volatile#" &
     "volatile_components#" &
     "ada#" &
     "assembler#" &
     "intrinsic#" &
     "stdcall#" &
     "stubbed#" &
     "asm#" &
     "assembly#" &
     "default#" &
     "dll#" &
     "win32#" &
     "as_is#" &
     "body_file_name#" &
     "casing#" &
     "code#" &
     "component#" &
     "component_size_4#" &
     "copy#" &
     "d_float#" &
     "descriptor#" &
     "dot_replacement#" &
     "dynamic#" &
     "entity#" &
     "external_name#" &
     "first_optional_parameter#" &
     "form#" &
     "g_float#" &
     "gcc#" &
     "gnat#" &
     "gpl#" &
     "ieee_float#" &
     "homonym_number#" &
     "internal#" &
     "link_name#" &
     "lowercase#" &
     "max_size#" &
     "mechanism#" &
     "mixedcase#" &
     "modified_gpl#" &
     "name#" &
     "nca#" &
     "no#" &
     "on#" &
     "parameter_types#" &
     "reference#" &
     "restricted#" &
     "result_mechanism#" &
     "result_type#" &
     "runtime#" &
     "sb#" &
     "secondary_stack_size#" &
     "section#" &
     "semaphore#" &
     "spec_file_name#" &
     "static#" &
     "stack_size#" &
     "subunit_file_name#" &
     "task_stack_size_default#" &
     "task_type#" &
     "time_slicing_enabled#" &
     "top_guard#" &
     "uba#" &
     "ubs#" &
     "ubsb#" &
     "unit_name#" &
     "unknown#" &
     "unrestricted#" &
     "uppercase#" &
     "user#" &
     "vax_float#" &
     "vms#" &
     "working_storage#" &
     "access#" &
     "address#" &
     "address_size#" &
     "alignment#" &
     "bit#" &
     "bit_position#" &
     "component_size#" &
     "constrained#" &
     "enum_rep#" &
     "exponent#" &
     "first#" &
     "first_bit#" &
     "img#" &
     "integer_value#" &
     "large#" &
     "last#" &
     "last_bit#" &
     "length#" &
     "machine_rounds#" &
     "machine_size#" &
     "mantissa#" &
     "max_size_in_storage_elements#" &
     "maximum_alignment#" &
     "modulus#" &
     "object_size#" &
     "passed_by_reference#" &
     "pool_address#" &
     "pos#" &
     "position#" &
     "range#" &
     "range_length#" &
     "round#" &
     "scale#" &
     "signed_zeros#" &
     "size#" &
     "storage_size#" &
     "storage_unit#" &
     "tag#" &
     "target_name#" &
     "terminated#" &
     "to_address#" &
     "type_class#" &
     "unchecked_access#" &
     "unconstrained_array#" &
     "universal_literal_string#" &
     "unrestricted_access#" &
     "val#" &
     "valid#" &
     "value_size#" &
     "wchar_t_size#" &
     "wide_width#" &
     "width#" &
     "word_size#" &
     "ceiling#" &
     "floor#" &
     "fraction#" &
     "image#" &
     "input#" &
     "machine#" &
     "max#" &
     "min#" &
     "model#" &
     "output#" &
     "remainder#" &
     "rounding#" &
     "succ#" &
     "truncation#" &
     "value#" &
     "wide_image#" &
     "wide_value#" &
     "storage_pool#" &
     "base#" &
     "class#" &
     "access_check#" &
     "accessibility_check#" &
     "division_check#" &
     "elaboration_check#" &
     "index_check#" &
     "length_check#" &
     "overflow_check#" &
     "range_check#" &
     "storage_check#" &
     "tag_check#" &
     "all_checks#" &
     "abort#" &
     "abs#" &
     "and#" &
     "all#" &
     "array#" &
     "at#" &
     "begin#" &
     "body#" &
     "case#" &
     "constant#" &
     "declare#" &
     "delay#" &
     "do#" &
     "else#" &
     "elsif#" &
     "end#" &
     "exception#" &
     "exit#" &
     "for#" &
     "function#" &
     "generic#" &
     "goto#" &
     "if#" &
     "in#" &
     "is#" &
     "loop#" &
     "mod#" &
     "new#" &
     "not#" &
     "null#" &
     "of#" &
     "or#" &
     "others#" &
     "out#" &
     "package#" &
     "pragma#" &
     "private#" &
     "procedure#" &
     "raise#" &
     "record#" &
     "rem#" &
     "renames#" &
     "return#" &
     "reverse#" &
     "subtype#" &
     "task#" &
     "terminate#" &
     "then#" &
     "type#" &
     "use#" &
     "when#" &
     "while#" &
     "with#" &
     "xor#" &
     "divide#" &
     "enclosing_entity#" &
     "exception_information#" &
     "exception_message#" &
     "exception_name#" &
     "file#" &
     "import_address#" &
     "import_largest_value#" &
     "import_value#" &
     "is_negative#" &
     "line#" &
     "rotate_left#" &
     "rotate_right#" &
     "shift_left#" &
     "shift_right#" &
     "shift_right_arithmetic#" &
     "source_location#" &
     "unchecked_conversion#" &
     "unchecked_deallocation#" &
     "to_pointer#" &
     "abstract#" &
     "aliased#" &
     "until#" &
     "tagged#" &
     "reactive#" &
     "wait#" &
     "pause#" &
     "select#" &
     "fork#" &
     "reaction#" &
     "react#" &
     "flow#" &
     "activity#" &
     "raise_exception#" &
     "binder#" &
     "body_suffix#" &
     "builder#" &
     "compiler#" &
     "cross_reference#" &
     "default_switches#" &
     "exec_dir#" &
     "executable#" &
     "executable_suffix#" &
     "extends#" &
     "finder#" &
     "global_configuration_pragmas#" &
     "gnatls#" &
     "gnatstub#" &
     "implementation#" &
     "implementation_exceptions#" &
     "implementation_suffix#" &
     "languages#" &
     "library_dir#" &
     "library_auto_init#" &
     "library_gcc#" &
     "library_interface#" &
     "library_kind#" &
     "library_name#" &
     "library_options#" &
     "library_reference_symbol_file#" &
     "library_src_dir#" &
     "library_symbol_file#" &
     "library_symbol_policy#" &
     "library_version#" &
     "linker#" &
     "local_configuration_pragmas#" &
     "locally_removed_files#" &
     "naming#" &
     "object_dir#" &
     "pretty_printer#" &
     "project#" &
     "separate_suffix#" &
     "source_dirs#" &
     "source_files#" &
     "source_list_file#" &
     "spec#" &
     "spec_suffix#" &
     "specification#" &
     "specification_exceptions#" &
     "specification_suffix#" &
     "switches#" &
     "unaligned_valid#" &
     "#";

   ---------------------
   -- Generated Names --
   ---------------------

   --  This section lists the various cases of generated names which are
   --  built from existing names by adding unique leading and/or trailing
   --  upper case letters. In some cases these names are built recursively,
   --  in particular names built from types may be built from types which
   --  themselves have generated names. In this list, xxx represents an
   --  existing name to which identifying letters are prepended or appended,
   --  and a trailing n represents a serial number in an external name that
   --  has some semantic significance (e.g. the n'th index type of an array).

   --    xxxA    access type for formal xxx in entry param record   (Exp_Ch9)
   --    xxxB    tag table for tagged type xxx                      (Exp_Ch3)
   --    xxxB    task body procedure for task xxx                   (Exp_Ch9)
   --    xxxD    dispatch table for tagged type xxx                 (Exp_Ch3)
   --    xxxD    discriminal for discriminant xxx                   (Sem_Ch3)
   --    xxxDn   n'th discr check function for rec type xxx         (Exp_Ch3)
   --    xxxE    elaboration boolean flag for task xxx              (Exp_Ch9)
   --    xxxE    dispatch table pointer type for tagged type xxx    (Exp_Ch3)
   --    xxxE    parameters for accept body for entry xxx           (Exp_Ch9)
   --    xxxFn   n'th primitive of a tagged type (named xxx)        (Exp_Ch3)
   --    xxxJ    tag table type index for tagged type xxx           (Exp_Ch3)
   --    xxxM    master Id value for access type xxx                (Exp_Ch3)
   --    xxxP    tag table pointer type for tagged type xxx         (Exp_Ch3)
   --    xxxP    parameter record type for entry xxx                (Exp_Ch9)
   --    xxxPA   access to parameter record type for entry xxx      (Exp_Ch9)
   --    xxxPn   pointer type for n'th primitive of tagged type xxx (Exp_Ch3)
   --    xxxR    dispatch table pointer for tagged type xxx         (Exp_Ch3)
   --    xxxT    tag table type for tagged type xxx                 (Exp_Ch3)
   --    xxxT    literal table for enumeration type xxx             (Sem_Ch3)
   --    xxxV    type for task value record for task xxx            (Exp_Ch9)
   --    xxxX    entry index constant                               (Exp_Ch9)
   --    xxxY    dispatch table type for tagged type xxx            (Exp_Ch3)
   --    xxxZ    size variable for task xxx                         (Exp_Ch9)

   --  TSS names

   --    xxxDA   deep adjust routine for type xxx                   (Exp_TSS)
   --    xxxDF   deep finalize routine for type xxx                 (Exp_TSS)
   --    xxxDI   deep initialize routine for type xxx               (Exp_TSS)
   --    xxxEQ   composite equality routine for record type xxx     (Exp_TSS)
   --    xxxIP   initialization procedure for type xxx              (Exp_TSS)
   --    xxxRA   RAs type access routine for type xxx               (Exp_TSS)
   --    xxxRD   RAs type dereference routine for type xxx          (Exp_TSS)
   --    xxxRP   Rep to Pos conversion for enumeration type xxx     (Exp_TSS)
   --    xxxSI   stream input attribute subprogram for type xxx     (Exp_TSS)
   --    xxxSO   stream output attribute subprogram for type xxx    (Exp_TSS)
   --    xxxSR   stream read attribute subprogram for type xxx      (Exp_TSS)
   --    xxxSW   stream write attribute subprogram for type xxx     (Exp_TSS)

   --  Implicit type names

   --    TxxxT   type of literal table for enumeration type xxx     (Sem_Ch3)

   --  (Note: this list is not complete or accurate ???)

   ----------------------
   -- Get_Attribute_Id --
   ----------------------

   function Get_Attribute_Id (N : Name_Id) return Attribute_Id is
   begin
      return Attribute_Id'Val (N - First_Attribute_Name);
   end Get_Attribute_Id;

   ------------------
   -- Get_Check_Id --
   ------------------

   function Get_Check_Id (N : Name_Id) return Check_Id is
   begin
      return Check_Id'Val (N - First_Check_Name);
   end Get_Check_Id;

   -----------------------
   -- Get_Convention_Id --
   -----------------------

   function Get_Convention_Id (N : Name_Id) return Convention_Id is
   begin
      case N is
         when Name_Ada        => return Convention_Ada;
         when Name_Assembler  => return Convention_Assembler;
         when Name_C          => return Convention_C;
         when Name_Intrinsic  => return Convention_Intrinsic;
         when Name_Stdcall    => return Convention_Stdcall;
         when Name_Stubbed    => return Convention_Stubbed;

         --  If no direct match, then we must have a convention
         --  identifier pragma that has specified this name.

         when others          =>
            for J in 1 .. Convention_Identifiers.Last loop
               if N = Convention_Identifiers.Table (J).Name then
                  return Convention_Identifiers.Table (J).Convention;
               end if;
            end loop;

            raise Program_Error;
      end case;
   end Get_Convention_Id;

   -------------------
   -- Get_Pragma_Id --
   -------------------

   function Get_Pragma_Id (N : Name_Id) return Pragma_Id is
   begin
      if N = Name_Storage_Size then
         return Pragma_Storage_Size;
      elsif N = Name_Storage_Unit then
         return Pragma_Storage_Unit;
      elsif N not in First_Pragma_Name .. Last_Pragma_Name then
         return Unknown_Pragma;
      else
         return Pragma_Id'Val (N - First_Pragma_Name);
      end if;
   end Get_Pragma_Id;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      P_Index      : Natural;
      Discard_Name : Name_Id;

   begin
      P_Index := Preset_Names'First;

      loop
         Name_Len := 0;

         while Preset_Names (P_Index) /= '#' loop
            Name_Len := Name_Len + 1;
            Name_Buffer (Name_Len) := Preset_Names (P_Index);
            P_Index := P_Index + 1;
         end loop;

         --  We do the Name_Find call to enter the name into the table, but
         --  we don't need to do anything with the result, since we already
         --  initialized all the preset names to have the right value (we
         --  are depending on the order of the names and Preset_Names).

         Discard_Name := Name_Find;
         P_Index := P_Index + 1;
         exit when Preset_Names (P_Index) = '#';
      end loop;

      --  Make sure that number of names in standard table is correct. If
      --  this check fails, run utility program XSNAMES to construct a new
      --  properly matching version of the body.

      pragma Assert (Discard_Name = Last_Predefined_Name);

      --  Initialize the convention identifiers table with the standard
      --  set of synonyms that we recognize for conventions.

      Convention_Identifiers.Init;

      Convention_Identifiers.Append ((Name_Asm,      Convention_Assembler));
      Convention_Identifiers.Append ((Name_Assembly, Convention_Assembler));

      Convention_Identifiers.Append ((Name_Default,  Convention_C));
      Convention_Identifiers.Append ((Name_External, Convention_C));

      Convention_Identifiers.Append ((Name_DLL,      Convention_Stdcall));
      Convention_Identifiers.Append ((Name_Win32,    Convention_Stdcall));
   end Initialize;

   -----------------------
   -- Is_Attribute_Name --
   -----------------------

   function Is_Attribute_Name (N : Name_Id) return Boolean is
   begin
      return N in First_Attribute_Name .. Last_Attribute_Name;
   end Is_Attribute_Name;

   -------------------
   -- Is_Check_Name --
   -------------------

   function Is_Check_Name (N : Name_Id) return Boolean is
   begin
      return N in First_Check_Name .. Last_Check_Name;
   end Is_Check_Name;

   ------------------------
   -- Is_Convention_Name --
   ------------------------

   function Is_Convention_Name (N : Name_Id) return Boolean is
   begin
      --  Check if this is one of the standard conventions

      if N in First_Convention_Name .. Last_Convention_Name
        or else N = Name_C
      then
         return True;

      --  Otherwise check if it is in convention identifier table

      else
         for J in 1 .. Convention_Identifiers.Last loop
            if N = Convention_Identifiers.Table (J).Name then
               return True;
            end if;
         end loop;

         return False;
      end if;
   end Is_Convention_Name;

   ------------------------------
   -- Is_Entity_Attribute_Name --
   ------------------------------

   function Is_Entity_Attribute_Name (N : Name_Id) return Boolean is
   begin
      return N in First_Entity_Attribute_Name .. Last_Entity_Attribute_Name;
   end Is_Entity_Attribute_Name;

   --------------------------------
   -- Is_Function_Attribute_Name --
   --------------------------------

   function Is_Function_Attribute_Name (N : Name_Id) return Boolean is
   begin
      return N in
        First_Renamable_Function_Attribute ..
          Last_Renamable_Function_Attribute;
   end Is_Function_Attribute_Name;

   -----------------------------
   -- Is_Operator_Symbol_Name --
   -----------------------------

   function Is_Operator_Symbol_Name (N : Name_Id) return Boolean is
   begin
      return N in First_Operator_Name .. Last_Operator_Name;
   end Is_Operator_Symbol_Name;

   --------------------
   -- Is_Pragma_Name --
   --------------------

   function Is_Pragma_Name (N : Name_Id) return Boolean is
   begin
      return N in First_Pragma_Name .. Last_Pragma_Name
        or else N = Name_Storage_Size
        or else N = Name_Storage_Unit;
   end Is_Pragma_Name;

   ----------------------------
   -- Is_Type_Attribute_Name --
   ----------------------------

   function Is_Type_Attribute_Name (N : Name_Id) return Boolean is
   begin
      return N in First_Type_Attribute_Name .. Last_Type_Attribute_Name;
   end Is_Type_Attribute_Name;

   ----------------------------------
   -- Record_Convention_Identifier --
   ----------------------------------

   procedure Record_Convention_Identifier
     (Id         : Name_Id;
      Convention : Convention_Id)
   is
   begin
      Convention_Identifiers.Append ((Id, Convention));
   end Record_Convention_Identifier;

end Snames;
