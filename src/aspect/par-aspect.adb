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

package body Ch13 is

   -----------------------------------
   -- Aspect_Specifications_Present --
   -----------------------------------

   function Aspect_Specifications_Present
     (Strict : Boolean := Ada_Version < Ada_2012) return Boolean
   is
      Scan_State : Saved_Scan_State;
      Result     : Boolean;

      function Possible_Misspelled_Aspect return Boolean;
      --  Returns True, if Token_Name is a misspelling of some aspect name

      function With_Present return Boolean;
      --  Returns True if WITH is present, indicating presence of aspect
      --  specifications. Also allows incorrect use of WHEN in place of WITH.

      --------------------------------
      -- Possible_Misspelled_Aspect --
      --------------------------------

      function Possible_Misspelled_Aspect return Boolean is
      begin
         for J in Aspect_Id_Exclude_No_Aspect loop
            if Is_Bad_Spelling_Of (Token_Name, Aspect_Names (J)) then
               return True;
            end if;
         end loop;

         return False;
      end Possible_Misspelled_Aspect;

      ------------------
      -- With_Present --
      ------------------

      function With_Present return Boolean is
      begin
         if Token = Tok_With then
            return True;

         --  Check for WHEN used in place of WITH

         elsif Token = Tok_When then
            declare
               Scan_State : Saved_Scan_State;

            begin
               Save_Scan_State (Scan_State);
               Scan; -- past WHEN

               if Token = Tok_Identifier
                 and then Get_Aspect_Id (Token_Name) /= No_Aspect
               then
                  Error_Msg_SC ("WHEN should be WITH");
                  Restore_Scan_State (Scan_State);
                  return True;

               else
                  Restore_Scan_State (Scan_State);
                  return False;
               end if;
            end;

         else
            return False;
         end if;
      end With_Present;

   --  Start of processing for Aspect_Specifications_Present

   begin
      --  Definitely must have WITH to consider aspect specs to be present

      --  Note that this means that if we have a semicolon, we immediately
      --  return False. There is a case in which this is not optimal, namely
      --  something like

      --    type R is new Integer;
      --      with bla bla;

      --  where the semicolon is redundant, but scanning forward for it would
      --  be too expensive. Instead we pick up the aspect specifications later
      --  as a bogus declaration, and diagnose the semicolon at that point.

      if not With_Present then
         return False;
      end if;

      --  Have a WITH or some token that we accept as a legitimate bad attempt
      --  at writing WITH. See if it looks like an aspect specification

      Save_Scan_State (Scan_State);
      Scan; -- past WITH (or WHEN or other bad keyword)

      --  If no identifier, then consider that we definitely do not have an
      --  aspect specification.

      if Token /= Tok_Identifier then
         Result := False;

      --  This is where we pay attention to the Strict mode. Normally when
      --  we are in Ada 2012 mode, Strict is False, and we consider that we
      --  have an aspect specification if the identifier is an aspect name
      --  or a likely misspelling of one (even if not followed by =>) or
      --  the identifier is not an aspect name but is followed by =>, by
      --  a comma, or by a semicolon. The last two cases correspond to
      --  (misspelled) Boolean aspects with a defaulted value of True.
      --  P_Aspect_Specifications will generate messages if the aspect
      --  specification is ill-formed.

      else
         if Get_Aspect_Id (Token_Name) /= No_Aspect
           or else Possible_Misspelled_Aspect
         then
            Result := True;
         else
            Scan; -- past identifier
            Result := Token = Tok_Arrow or else
                      Token = Tok_Comma or else
                      Token = Tok_Semicolon;
         end if;
      end if;

      Restore_Scan_State (Scan_State);
      return Result;
   end Aspect_Specifications_Present;

   -------------------------------
   -- Get_Aspect_Specifications --
   -------------------------------

   function Get_Aspect_Specifications
     (Semicolon : Boolean := True) return List_Id
   is
      A_Id    : Aspect_Id;
      Aspect  : Node_Id;
      Aspects : List_Id;
      OK      : Boolean;

      Opt : Boolean;
      --  True if current aspect takes an optional argument

   begin
      Aspects := Empty_List;

      --  Check if aspect specification present

      if not Aspect_Specifications_Present then
         if Semicolon then
            TF_Semicolon;
         end if;

         return Aspects;
      end if;

      Scan; -- past WITH (or possible WHEN after error)
      Aspects := Empty_List;

      --  Loop to scan aspects

      loop
         OK := True;

         --  The aspect mark is not an identifier

         if Token /= Tok_Identifier then
            Error_Msg_SC ("aspect identifier expected");

            --  Skip the whole aspect specification list

            if Semicolon then
               Resync_Past_Semicolon;
            end if;

            return Aspects;
         end if;

         A_Id := Get_Aspect_Id (Token_Name);
         Aspect :=
           Make_Aspect_Specification (Token_Ptr,
             Identifier => Token_Node);

         --  The aspect mark is not recognized

         if A_Id = No_Aspect then
            Error_Msg_N ("& is not a valid aspect identifier", Token_Node);
            OK := False;

            --  Check bad spelling

            for J in Aspect_Id_Exclude_No_Aspect loop
               if Is_Bad_Spelling_Of (Token_Name, Aspect_Names (J)) then
                  Error_Msg_Name_1 := Aspect_Names (J);
                  Error_Msg_N -- CODEFIX
                    ("\possible misspelling of%", Token_Node);
                  exit;
               end if;
            end loop;

            Scan; -- past incorrect identifier

            if Token = Tok_Apostrophe then
               Scan; -- past apostrophe
               Scan; -- past presumably CLASS
            end if;

            --  Attempt to parse the aspect definition by assuming it is an
            --  expression.

            if Token = Tok_Arrow then
               Scan; -- past arrow
               Set_Expression (Aspect, P_Expression);

            --  If we have a correct terminator (comma or semicolon, or a
            --  reasonable likely missing comma), then just proceed.

            elsif Token = Tok_Comma     or else
                  Token = Tok_Semicolon or else
                  Token = Tok_Identifier
            then
               null;

            --  Otherwise the aspect contains a junk definition

            else
               if Semicolon then
                  Resync_Past_Semicolon;
               end if;

               return Aspects;
            end if;

         --  Aspect mark is OK

         else
            Scan; -- past identifier
            Opt := Aspect_Argument (A_Id) = Optional_Expression
                      or else
                   Aspect_Argument (A_Id) = Optional_Name;

            --  Check for 'Class present

            if Token = Tok_Apostrophe then
               if Class_Aspect_OK (A_Id) then
                  Scan; -- past apostrophe

                  if Token = Tok_Identifier
                    and then Token_Name = Name_Class
                  then
                     Scan; -- past CLASS
                     Set_Class_Present (Aspect);
                  else
                     Error_Msg_SC ("Class attribute expected here");
                     OK := False;

                     if Token = Tok_Identifier then
                        Scan; -- past identifier not CLASS
                     end if;
                  end if;

               --  The aspect does not allow 'Class

               else
                  Error_Msg_Node_1 := Identifier (Aspect);
                  Error_Msg_SC ("aspect& does not permit attribute here");
                  OK := False;

                  Scan; -- past apostrophe
                  Scan; -- past presumably CLASS
               end if;
            end if;

            --  Check for a missing aspect definition. Aspects with optional
            --  definitions are not considered.

            if Token = Tok_Comma or else Token = Tok_Semicolon then
               if not Opt then
                  Error_Msg_Node_1 := Identifier (Aspect);
                  Error_Msg_AP ("aspect& requires an aspect definition");
                  OK := False;
               end if;

            --  Here we do not have a comma or a semicolon, we are done if we
            --  do not have an arrow and the aspect does not need an argument

            elsif Opt and then Token /= Tok_Arrow then
               null;

            --  Here we have either an arrow, or an aspect that definitely
            --  needs an aspect definition, and we will look for one even if
            --  no arrow is preseant.

            --  Otherwise we have an aspect definition

            else
               if Token = Tok_Arrow then
                  Scan; -- past arrow
               else
                  T_Arrow;
                  OK := False;
               end if;

               --  Detect a common error where the non-null definition of
               --  aspect Depends, Global, Refined_Depends, Refined_Global
               --  or Refined_State lacks enclosing parentheses.

               if Token /= Tok_Left_Paren and then Token /= Tok_Null then

                  --  [Refined_]Depends

                  if A_Id = Aspect_Depends
                       or else
                     A_Id = Aspect_Refined_Depends
                  then
                     Error_Msg_SC -- CODEFIX
                       ("missing ""(""");
                     Resync_Past_Malformed_Aspect;

                     --  Return when the current aspect is the last in the list
                     --  of specifications and the list applies to a body.

                     if Token = Tok_Is then
                        return Aspects;
                     end if;

                  --  [Refined_]Global

                  elsif A_Id = Aspect_Global
                          or else
                        A_Id = Aspect_Refined_Global
                  then
                     declare
                        Scan_State : Saved_Scan_State;

                     begin
                        Save_Scan_State (Scan_State);
                        Scan; -- past item or mode_selector

                        --  Emit an error when the aspect has a mode_selector
                        --  as the moded_global_list must be parenthesized:
                        --    with Global => Output => Item

                        if Token = Tok_Arrow then
                           Restore_Scan_State (Scan_State);
                           Error_Msg_SC -- CODEFIX
                             ("missing ""(""");
                           Resync_Past_Malformed_Aspect;

                           --  Return when the current aspect is the last in
                           --  the list of specifications and the list applies
                           --  to a body.

                           if Token = Tok_Is then
                              return Aspects;
                           end if;

                        elsif Token = Tok_Comma then
                           Scan; -- past comma

                           --  An item followed by a comma does not need to
                           --  be parenthesized if the next token is a valid
                           --  aspect name:
                           --    with Global => Item,
                           --         Aspect => ...

                           if Token = Tok_Identifier
                             and then Get_Aspect_Id (Token_Name) /= No_Aspect
                           then
                              Restore_Scan_State (Scan_State);

                           --  Otherwise this is a list of items in which case
                           --  the list must be parenthesized.

                           else
                              Restore_Scan_State (Scan_State);
                              Error_Msg_SC -- CODEFIX
                                ("missing ""(""");
                              Resync_Past_Malformed_Aspect;

                              --  Return when the current aspect is the last
                              --  in the list of specifications and the list
                              --  applies to a body.

                              if Token = Tok_Is then
                                 return Aspects;
                              end if;
                           end if;

                        --  The definition of [Refined_]Global does not need to
                        --  be parenthesized.

                        else
                           Restore_Scan_State (Scan_State);
                        end if;
                     end;

                  --  Refined_State

                  elsif A_Id = Aspect_Refined_State then
                     if Token = Tok_Identifier then
                        declare
                           Scan_State : Saved_Scan_State;

                        begin
                           Save_Scan_State (Scan_State);
                           Scan;  --  past state

                           --  The refinement contains a constituent, the whole
                           --  argument of Refined_State must be parenthesized.

                           --    with Refined_State => State => Constit

                           if Token = Tok_Arrow then
                              Restore_Scan_State (Scan_State);
                              Error_Msg_SC -- CODEFIX
                                ("missing ""(""");
                              Resync_Past_Malformed_Aspect;

                              --  Return when the current aspect is the last
                              --  in the list of specifications and the list
                              --  applies to a body.

                              if Token = Tok_Is then
                                 return Aspects;
                              end if;

                           --  The refinement lacks constituents. Do not flag
                           --  this case as the error would be misleading. The
                           --  diagnostic is left to the analysis.

                           --    with Refined_State => State

                           else
                              Restore_Scan_State (Scan_State);
                           end if;
                        end;
                     end if;
                  end if;
               end if;

               --  Note if inside Depends aspect

               if A_Id = Aspect_Depends then
                  Inside_Depends := True;
               end if;

               --  Parse the aspect definition depening on the expected
               --  argument kind.

               if Aspect_Argument (A_Id) = Name
                 or else Aspect_Argument (A_Id) = Optional_Name
               then
                  Set_Expression (Aspect, P_Name);

               else
                  pragma Assert
                    (Aspect_Argument (A_Id) = Expression
                       or else
                     Aspect_Argument (A_Id) = Optional_Expression);
                  Set_Expression (Aspect, P_Expression);
               end if;

               --  Unconditionally reset flag for Inside_Depends

               Inside_Depends := False;
            end if;

            --  Add the aspect to the resulting list only when it was properly
            --  parsed.

            if OK then
               Append (Aspect, Aspects);
            end if;
         end if;

         --  Merge here after good or bad aspect (we should be at a comma
         --  or a semicolon, but there might be other possible errors).

         --  The aspect specification list contains more than one aspect

         if Token = Tok_Comma then
            Scan; -- past comma
            goto Continue;

         --  Check for a missing comma between two aspects. Emit an error
         --  and proceed to the next aspect.

         elsif Token = Tok_Identifier
           and then Get_Aspect_Id (Token_Name) /= No_Aspect
         then
            declare
               Scan_State : Saved_Scan_State;

            begin
               Save_Scan_State (Scan_State);
               Scan; -- past identifier

               --  Attempt to detect ' or => following a potential aspect
               --  mark.

               if Token = Tok_Apostrophe or else Token = Tok_Arrow then
                  Restore_Scan_State (Scan_State);
                  Error_Msg_AP -- CODEFIX
                    ("|missing "",""");
                  goto Continue;

               --  The construct following the current aspect is not an
               --  aspect.

               else
                  Restore_Scan_State (Scan_State);
               end if;
            end;

         --  Check for a mistyped semicolon in place of a comma between two
         --  aspects. Emit an error and proceed to the next aspect.

         elsif Token = Tok_Semicolon then
            declare
               Scan_State : Saved_Scan_State;

            begin
               Save_Scan_State (Scan_State);
               Scan; -- past semicolon

               if Token = Tok_Identifier
                 and then Get_Aspect_Id (Token_Name) /= No_Aspect
               then
                  Scan; -- past identifier

                  --  Attempt to detect ' or => following potential aspect mark

                  if Token = Tok_Apostrophe or else Token = Tok_Arrow then
                     Restore_Scan_State (Scan_State);
                     Error_Msg_SC -- CODEFIX
                       ("|"";"" should be "",""");
                     Scan; -- past semicolon
                     goto Continue;
                  end if;
               end if;

               --  Construct following the current aspect is not an aspect

               Restore_Scan_State (Scan_State);
            end;
         end if;

         --  Require semicolon if caller expects to scan this out

         if Semicolon then
            T_Semicolon;
         end if;

         exit;

      <<Continue>>
         null;
      end loop;

      return Aspects;
   end Get_Aspect_Specifications;

   --------------------------------
   -- 13.1  Aspect Specification --
   --------------------------------

   --  ASPECT_SPECIFICATION ::=
   --    with ASPECT_MARK [=> ASPECT_DEFINITION] {,
   --         ASPECT_MARK [=> ASPECT_DEFINITION] }

   --  ASPECT_MARK ::= aspect_IDENTIFIER['Class]

   --  ASPECT_DEFINITION ::= NAME | EXPRESSION

   --  Error recovery: cannot raise Error_Resync

   procedure P_Aspect_Specifications
     (Decl      : Node_Id;
      Semicolon : Boolean := True)
   is
      Aspects : List_Id;
      Ptr     : Source_Ptr;

   begin
      --  Aspect Specification is present

      Ptr := Token_Ptr;

      --  Here we have an aspect specification to scan, note that we don't
      --  set the flag till later, because it may turn out that we have no
      --  valid aspects in the list.

      Aspects := Get_Aspect_Specifications (Semicolon);

      --  Here if aspects present

      if Is_Non_Empty_List (Aspects) then

         --  If Decl is Empty, we just ignore the aspects (the caller in this
         --  case has always issued an appropriate error message).

         if Decl = Empty then
            null;

         --  If Decl is Error, we ignore the aspects, and issue a message

         elsif Decl = Error then
            Error_Msg ("aspect specifications not allowed here", Ptr);

         --  Here aspects are allowed, and we store them

         else
            Set_Parent (Aspects, Decl);
            Set_Aspect_Specifications (Decl, Aspects);
         end if;
      end if;
   end P_Aspect_Specifications;

end Ch13;
