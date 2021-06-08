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

with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
--  with Exp_Ch2;  use Exp_Ch2;
--  with Exp_Util; use Exp_Util;
with Elists;   use Elists;
with Freeze;   use Freeze;
with Lib;      use Lib;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Restrict; use Restrict;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Eval; use Sem_Eval;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Sprint;   use Sprint;
with Stand;    use Stand;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Urealp;   use Urealp;
with Validsw;  use Validsw;
with Namet; use Namet;

package body RxChecks is

   ---------------------
   -- Determine_Range --
   ---------------------

   Cache_Size : constant := 2 ** 10;
   type Cache_Index is range 0 .. Cache_Size - 1;
   --  Determine size of below cache (power of 2 is more efficient!)

   Determine_Range_Cache_N  : array (Cache_Index) of Node_Id;
   Determine_Range_Cache_Lo : array (Cache_Index) of Uint;
   Determine_Range_Cache_Hi : array (Cache_Index) of Uint;
   --  The above arrays are used to implement a small direct cache
   --  for Determine_Range calls. Because of the way Determine_Range
   --  recursively traces subexpressions, and because overflow checking
   --  calls the routine on the way up the tree, a quadratic behavior
   --  can otherwise be encountered in large expressions. The cache
   --  entry for node N is stored in the (N mod Cache_Size) entry, and
   --  can be validated by checking the actual node value stored there.

   procedure Determine_Range
     (N  : Node_Id;
      OK : out Boolean;
      Lo : out Uint;
      Hi : out Uint)
   is
      Typ : constant Entity_Id := Etype (N);

      Lo_Left : Uint;
      Hi_Left : Uint;
      --  Lo and Hi bounds of left operand

      Lo_Right : Uint;
      Hi_Right : Uint;
      --  Lo and Hi bounds of right (or only) operand

      Bound : Node_Id;
      --  Temp variable used to hold a bound node

      Hbound : Uint;
      --  High bound of base type of expression

      Lor : Uint;
      Hir : Uint;
      --  Refined values for low and high bounds, after tightening

      OK1 : Boolean;
      --  Used in lower level calls to indicate if call succeeded

      Cindex : Cache_Index;
      --  Used to search cache

      function OK_Operands return Boolean;
      --  Used for binary operators. Determines the ranges of the left and
      --  right operands, and if they are both OK, returns True, and puts
      --  the results in Lo_Right, Hi_Right, Lo_Left, Hi_Left

      -----------------
      -- OK_Operands --
      -----------------

      function OK_Operands return Boolean is
      begin
         Determine_Range (Left_Opnd  (N), OK1, Lo_Left,  Hi_Left);

         if not OK1 then
            return False;
         end if;

         Determine_Range (Right_Opnd (N), OK1, Lo_Right, Hi_Right);
         return OK1;
      end OK_Operands;

   --  Start of processing for Determine_Range

   begin
      --  Prevent junk warnings by initializing range variables

      Lo  := No_Uint;
      Hi  := No_Uint;
      Lor := No_Uint;
      Hir := No_Uint;

      --  If the type is not discrete, or is undefined, then we can't
      --  do anything about determining the range.

      if No (Typ) or else not Is_Discrete_Type (Typ)
        or else Error_Posted (N)
      then
         OK := False;
         return;
      end if;

      --  For all other cases, we can determine the range

      OK := True;

      --  If value is compile time known, then the possible range is the
      --  one value that we know this expression definitely has!

      if Compile_Time_Known_Value (N) then
         Lo := Expr_Value (N);
         Hi := Lo;
         return;
      end if;

      --  Return if already in the cache

      Cindex := Cache_Index (N mod Cache_Size);

      if Determine_Range_Cache_N (Cindex) = N then
         Lo := Determine_Range_Cache_Lo (Cindex);
         Hi := Determine_Range_Cache_Hi (Cindex);
         return;
      end if;

      --  Otherwise, start by finding the bounds of the type of the
      --  expression, the value cannot be outside this range (if it
      --  is, then we have an overflow situation, which is a separate
      --  check, we are talking here only about the expression value).

      --  We use the actual bound unless it is dynamic, in which case
      --  use the corresponding base type bound if possible. If we can't
      --  get a bound then we figure we can't determine the range (a
      --  peculiar case, that perhaps cannot happen, but there is no
      --  point in bombing in this optimization circuit.

      --  First the low bound

      Bound := Type_Low_Bound (Typ);

      if Compile_Time_Known_Value (Bound) then
         Lo := Expr_Value (Bound);

      elsif Compile_Time_Known_Value (Type_Low_Bound (Base_Type (Typ))) then
         Lo := Expr_Value (Type_Low_Bound (Base_Type (Typ)));

      else
         OK := False;
         return;
      end if;

      --  Now the high bound

      Bound := Type_High_Bound (Typ);

      --  We need the high bound of the base type later on, and this should
      --  always be compile time known. Again, it is not clear that this
      --  can ever be false, but no point in bombing.

      if Compile_Time_Known_Value (Type_High_Bound (Base_Type (Typ))) then
         Hbound := Expr_Value (Type_High_Bound (Base_Type (Typ)));
         Hi := Hbound;

      else
         OK := False;
         return;
      end if;

      --  If we have a static subtype, then that may have a tighter bound
      --  so use the upper bound of the subtype instead in this case.

      if Compile_Time_Known_Value (Bound) then
         Hi := Expr_Value (Bound);
      end if;

      --  We may be able to refine this value in certain situations. If
      --  refinement is possible, then Lor and Hir are set to possibly
      --  tighter bounds, and OK1 is set to True.

      case Nkind (N) is

         --  For unary plus, result is limited by range of operand

         when N_Op_Plus =>
            Determine_Range (Right_Opnd (N), OK1, Lor, Hir);

         --  For unary minus, determine range of operand, and negate it

         when N_Op_Minus =>
            Determine_Range (Right_Opnd (N), OK1, Lo_Right, Hi_Right);

            if OK1 then
               Lor := -Hi_Right;
               Hir := -Lo_Right;
            end if;

         --  For binary addition, get range of each operand and do the
         --  addition to get the result range.

         when N_Op_Add =>
            if OK_Operands then
               Lor := Lo_Left + Lo_Right;
               Hir := Hi_Left + Hi_Right;
            end if;

         --  Division is tricky. The only case we consider is where the
         --  right operand is a positive constant, and in this case we
         --  simply divide the bounds of the left operand

         when N_Op_Divide =>
            if OK_Operands then
               if Lo_Right = Hi_Right
                 and then Lo_Right > 0
               then
                  Lor := Lo_Left / Lo_Right;
                  Hir := Hi_Left / Lo_Right;

               else
                  OK1 := False;
               end if;
            end if;

         --  For binary subtraction, get range of each operand and do
         --  the worst case subtraction to get the result range.

         when N_Op_Subtract =>
            if OK_Operands then
               Lor := Lo_Left - Hi_Right;
               Hir := Hi_Left - Lo_Right;
            end if;

         --  For MOD, if right operand is a positive constant, then
         --  result must be in the allowable range of mod results.

         when N_Op_Mod =>
            if OK_Operands then
               if Lo_Right = Hi_Right
                 and then Lo_Right /= 0
               then
                  if Lo_Right > 0 then
                     Lor := Uint_0;
                     Hir := Lo_Right - 1;

                  else -- Lo_Right < 0
                     Lor := Lo_Right + 1;
                     Hir := Uint_0;
                  end if;

               else
                  OK1 := False;
               end if;
            end if;

         --  For REM, if right operand is a positive constant, then
         --  result must be in the allowable range of mod results.

         when N_Op_Rem =>
            if OK_Operands then
               if Lo_Right = Hi_Right
                 and then Lo_Right /= 0
               then
                  declare
                     Dval : constant Uint := (abs Lo_Right) - 1;

                  begin
                     --  The sign of the result depends on the sign of the
                     --  dividend (but not on the sign of the divisor, hence
                     --  the abs operation above).

                     if Lo_Left < 0 then
                        Lor := -Dval;
                     else
                        Lor := Uint_0;
                     end if;

                     if Hi_Left < 0 then
                        Hir := Uint_0;
                     else
                        Hir := Dval;
                     end if;
                  end;

               else
                  OK1 := False;
               end if;
            end if;

         --  Attribute reference cases

         when N_Attribute_Reference =>
            case Attribute_Name (N) is

               --  For Pos/Val attributes, we can refine the range using the
               --  possible range of values of the attribute expression

               when Name_Pos | Name_Val =>
                  Determine_Range (First (Expressions (N)), OK1, Lor, Hir);

               --  For Length attribute, use the bounds of the corresponding
               --  index type to refine the range.

               when Name_Length =>
                  declare
                     Atyp : Entity_Id := Etype (Prefix (N));
                     Inum : Nat;
                     Indx : Node_Id;

                     LL, LU : Uint;
                     UL, UU : Uint;

                  begin
                     if Is_Access_Type (Atyp) then
                        Atyp := Designated_Type (Atyp);
                     end if;

                     --  For string literal, we know exact value

                     if Ekind (Atyp) = E_String_Literal_Subtype then
                        OK := True;
                        Lo := String_Literal_Length (Atyp);
                        Hi := String_Literal_Length (Atyp);
                        return;
                     end if;

                     --  Otherwise check for expression given

                     if No (Expressions (N)) then
                        Inum := 1;
                     else
                        Inum :=
                          UI_To_Int (Expr_Value (First (Expressions (N))));
                     end if;

                     Indx := First_Index (Atyp);
                     for J in 2 .. Inum loop
                        Indx := Next_Index (Indx);
                     end loop;

                     Determine_Range
                       (Type_Low_Bound (Etype (Indx)), OK1, LL, LU);

                     if OK1 then
                        Determine_Range
                          (Type_High_Bound (Etype (Indx)), OK1, UL, UU);

                        if OK1 then

                           --  The maximum value for Length is the biggest
                           --  possible gap between the values of the bounds.
                           --  But of course, this value cannot be negative.

                           Hir := UI_Max (Uint_0, UU - LL);

                           --  For constrained arrays, the minimum value for
                           --  Length is taken from the actual value of the
                           --  bounds, since the index will be exactly of
                           --  this subtype.

                           if Is_Constrained (Atyp) then
                              Lor := UI_Max (Uint_0, UL - LU);

                           --  For an unconstrained array, the minimum value
                           --  for length is always zero.

                           else
                              Lor := Uint_0;
                           end if;
                        end if;
                     end if;
                  end;

               --  No special handling for other attributes
               --  Probably more opportunities exist here ???

               when others =>
                  OK1 := False;

            end case;

         --  For type conversion from one discrete type to another, we
         --  can refine the range using the converted value.

         when N_Type_Conversion =>
            Determine_Range (Expression (N), OK1, Lor, Hir);

         --  Nothing special to do for all other expression kinds

         when others =>
            OK1 := False;
            Lor := No_Uint;
            Hir := No_Uint;
      end case;

      --  At this stage, if OK1 is true, then we know that the actual
      --  result of the computed expression is in the range Lor .. Hir.
      --  We can use this to restrict the possible range of results.

      if OK1 then

         --  If the refined value of the low bound is greater than the
         --  type high bound, then reset it to the more restrictive
         --  value. However, we do NOT do this for the case of a modular
         --  type where the possible upper bound on the value is above the
         --  base type high bound, because that means the result could wrap.

         if Lor > Lo
           and then not (Is_Modular_Integer_Type (Typ)
                           and then Hir > Hbound)
         then
            Lo := Lor;
         end if;

         --  Similarly, if the refined value of the high bound is less
         --  than the value so far, then reset it to the more restrictive
         --  value. Again, we do not do this if the refined low bound is
         --  negative for a modular type, since this would wrap.

         if Hir < Hi
           and then not (Is_Modular_Integer_Type (Typ)
                          and then Lor < Uint_0)
         then
            Hi := Hir;
         end if;
      end if;

      --  Set cache entry for future call and we are all done

      Determine_Range_Cache_N  (Cindex) := N;
      Determine_Range_Cache_Lo (Cindex) := Lo;
      Determine_Range_Cache_Hi (Cindex) := Hi;
      return;

   --  If any exception occurs, it means that we have some bug in the compiler
   --  possibly triggered by a previous error, or by some unforseen peculiar
   --  occurrence. However, this is only an optimization attempt, so there is
   --  really no point in crashing the compiler. Instead we just decide, too
   --  bad, we can't figure out a range in this case after all.

   exception
      when others =>

         --  Debug flag K disables this behavior (useful for debugging)

         if Debug_Flag_K then
            raise;
         else
            OK := False;
            Lo := No_Uint;
            Hi := No_Uint;
            return;
         end if;
   end Determine_Range;

end RxChecks;
