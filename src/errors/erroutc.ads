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

--  This packages contains global variables and routines common to error
--  reporting packages, including Errout and Prj.Err.

with Hostparm;
with Table;
with Types;  use Types;

package Erroutc is

   Class_Flag : Boolean := False;
   --  This flag is set True when outputting a reference to a class-wide
   --  type, and is used by Add_Class to insert 'Class at the proper point

   Continuation : Boolean := False;
   --  Indicates if current message is a continuation. Intialized from the
   --  Msg_Cont parameter in Error_Msg_Internal and then set True if a \
   --  insertion character is encountered.

   Flag_Source : Source_File_Index;
   --  Source file index for source file where error is being posted

   Is_Warning_Msg : Boolean := False;
   --  Set True to indicate if current message is warning message

   Is_Style_Msg : Boolean := False;
   --  Set True to indicate if the current message is a style message

   Is_Serious_Error : Boolean := False;
   --  Set by Set_Msg_Text to indicate if current message is serious error

   Is_Unconditional_Msg : Boolean := False;
   --  Set by Set_Msg_Text to indicate if current message is unconditional

   Kill_Message : Boolean := False;
   --  A flag used to kill weird messages (e.g. those containing uninterpreted
   --  implicit type references) if we have already seen at least one message
   --  already. The idea is that we hope the weird message is a junk cascaded
   --  message that should be suppressed.

   Last_Killed : Boolean := False;
   --  Set True if the most recently posted non-continuation message was
   --  killed. This is used to determine the processing of any continuation
   --  messages that follow.

   List_Pragmas_Index : Int := 0;
   --  Index into List_Pragmas table

   List_Pragmas_Mode : Boolean := False;
   --  Starts True, gets set False by pragma List (Off), True by List (On)

   Manual_Quote_Mode : Boolean := False;
   --  Set True in manual quotation mode

   Max_Msg_Length : constant := 80 + 2 * Hostparm.Max_Line_Length;
   --  Maximum length of error message. The addition of Max_Line_Length
   --  ensures that two insertion tokens of maximum length can be accomodated.

   Msg_Buffer : String (1 .. Max_Msg_Length);
   --  Buffer used to prepare error messages

   Msglen : Integer := 0;
   --  Number of characters currently stored in the message buffer

   Suppress_Message : Boolean;
   --  A flag used to suppress certain obviously redundant messages (i.e.
   --  those referring to a node whose type is Any_Type). This suppression
   --  is effective only if All_Errors_Mode is off.

   Suppress_Instance_Location : Boolean := False;
   --  Normally, if a # location in a message references a location within
   --  a generic template, then a note is added giving the location of the
   --  instantiation. If this variable is set True, then this note is not
   --  output. This is used for internal processing for the case of an
   --  illegal instantiation. See Error_Msg routine for further details.

   ----------------------------
   -- Message ID Definitions --
   ----------------------------

   type Error_Msg_Id is new Int;
   --  A type used to represent specific error messages. Used by the clients
   --  of this package only in the context of the Get_Error_Id and
   --  Change_Error_Text subprograms.

   No_Error_Msg : constant Error_Msg_Id := 0;
   --  A constant which is different from any value returned by Get_Error_Id.
   --  Typically used by a client to indicate absense of a saved Id value.

   Cur_Msg : Error_Msg_Id := No_Error_Msg;
   --  Id of most recently posted error message

   function Get_Msg_Id return Error_Msg_Id;
   --  Returns the Id of the message most recently posted using one of the
   --  Error_Msg routines.

   function Get_Location (E : Error_Msg_Id) return Source_Ptr;
   --  Returns the flag location of the error message with the given id E.

   -----------------------------------
   -- Error Message Data Structures --
   -----------------------------------

   --  The error messages are stored as a linked list of error message objects
   --  sorted into ascending order by the source location (Sloc). Each object
   --  records the text of the message and its source location.

   --  The following record type and table are used to represent error
   --  messages, with one entry in the table being allocated for each message.

   type Error_Msg_Object is record
      Text : String_Ptr;
      --  Text of error message, fully expanded with all insertions

      Next : Error_Msg_Id;
      --  Pointer to next message in error chain

      Sfile : Source_File_Index;
      --  Source table index of source file. In the case of an error that
      --  refers to a template, always references the original template
      --  not an instantiation copy.

      Sptr : Source_Ptr;
      --  Flag pointer. In the case of an error that refers to a template,
      --  always references the original template, not an instantiation copy.
      --  This value is the actual place in the source that the error message
      --  will be posted. Note that an error placed on an instantiation will
      --  have Sptr pointing to the instantiation point.

      Optr : Source_Ptr;
      --  Flag location used in the call to post the error. This is normally
      --  the same as Sptr, except when an error is posted on a particular
      --  instantiation of a generic. In such a case, Sptr will point to
      --  the original source location of the instantiation itself, but
      --  Optr will point to the template location (more accurately to the
      --  template copy in the instantiation copy corresponding to the
      --  instantiation referenced by Sptr).

      Line : Physical_Line_Number;
      --  Line number for error message

      Col : Column_Number;
      --  Column number for error message

      Warn : Boolean;
      --  True if warning message (i.e. insertion character ? appeared)

      Style : Boolean;
      --  True if style message (starts with "(style)")

      Serious : Boolean;
      --  True if serious error message (not a warning and no | character)

      Uncond : Boolean;
      --  True if unconditional message (i.e. insertion character ! appeared)

      Msg_Cont : Boolean;
      --  This is used for logical messages that are composed of multiple
      --  individual messages. For messages that are not part of such a
      --  group, or that are the first message in such a group. Msg_Cont
      --  is set to False. For subsequent messages in a group, Msg_Cont
      --  is set to True. This is used to make sure that such a group of
      --  messages is either suppressed or retained as a group (e.g. in
      --  the circuit that deletes identical messages).

      Deleted : Boolean;
      --  If this flag is set, the message is not printed. This is used
      --  in the circuit for deleting duplicate/redundant error messages.
   end record;

   package Errors is new Table.Table (
     Table_Component_Type => Error_Msg_Object,
     Table_Index_Type     => Error_Msg_Id,
     Table_Low_Bound      => 1,
     Table_Initial        => 200,
     Table_Increment      => 200,
     Table_Name           => "Error");

   First_Error_Msg : Error_Msg_Id;
   --  The list of error messages, i.e. the first entry on the list of error
   --  messages. This is not the same as the physically first entry in the
   --  error message table, since messages are not always inserted in sequence.

   Last_Error_Msg : Error_Msg_Id;
   --  The last entry on the list of error messages. Note that this is not
   --  the same as the physically last entry in the error message table, since
   --  messages are not always inserted in sequence.

   --------------------------
   -- Warning Mode Control --
   --------------------------

   --  Pragma Warnings allows warnings to be turned off for a specified
   --  region of code, and the following tabl is the data structure used
   --  to keep track of these regions.

   --  It contains pairs of source locations, the first being the start
   --  location for a warnings off region, and the second being the end
   --  location. When a pragma Warnings (Off) is encountered, a new entry
   --  is established extending from the location of the pragma to the
   --  end of the current source file. A subsequent pragma Warnings (On)
   --  adjusts the end point of this entry appropriately.

   --  If all warnings are suppressed by comamnd switch, then there is a
   --  dummy entry (put there by Errout.Initialize) at the start of the
   --  table which covers all possible Source_Ptr values. Note that the
   --  source pointer values in this table always reference the original
   --  template, not an instantiation copy, in the generic case.

   type Warnings_Entry is record
      Start : Source_Ptr;
      Stop  : Source_Ptr;
   end record;

   package Warnings is new Table.Table (
     Table_Component_Type => Warnings_Entry,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => 1,
     Table_Initial        => 100,
     Table_Increment      => 200,
     Table_Name           => "Warnings");

   -----------------
   -- Subprograms --
   -----------------

   procedure Add_Class;
   --  Add 'Class to buffer for class wide type case (Class_Flag set)

   function Buffer_Ends_With (S : String) return Boolean;
   --  Tests if message buffer ends with given string preceded by a space

   procedure Buffer_Remove (S : String);
   --  Removes given string from end of buffer if it is present
   --  at end of buffer, and preceded by a space.

   function Compilation_Errors return Boolean;
   --  Returns true if errors have been detected, or warnings in -gnatwe
   --  (treat warnings as errors) mode.

   procedure dmsg (Id : Error_Msg_Id);
   --  Debugging routine to dump an error message

   procedure Debug_Output (N : Node_Id);
   --  Called from Error_Msg_N and Error_Msg_NE to generate line of debug
   --  output giving node number (of node N) if the debug X switch is set.

   procedure Check_Duplicate_Message (M1, M2 : Error_Msg_Id);
   --  This function is passed the Id values of two error messages. If
   --  either M1 or M2 is a continuation message, or is already deleted,
   --  the call is ignored. Otherwise a check is made to see if M1 and M2
   --  are duplicated or redundant. If so, the message to be deleted and
   --  all its continuations are marked with the Deleted flag set to True.

   procedure Output_Error_Msgs (E : in out Error_Msg_Id);
   --  Output source line, error flag, and text of stored error message and
   --  all subsequent messages for the same line and unit. On return E is
   --  set to be one higher than the last message output.

   procedure Output_Line_Number (L : Logical_Line_Number);
   --  Output a line number as six digits (with leading zeroes suppressed),
   --  followed by a period and a blank (note that this is 8 characters which
   --  means that tabs in the source line will not get messed up). Line numbers
   --  that match or are less than the last Source_Reference pragma are listed
   --  as all blanks, avoiding output of junk line numbers.

   procedure Output_Msg_Text (E : Error_Msg_Id);
   --  Outputs characters of text in the text of the error message E, excluding
   --  any final exclamation point. Note that no end of line is output, the
   --  caller is responsible for adding the end of line.

   procedure Purge_Messages (From : Source_Ptr; To : Source_Ptr);
   --  All error messages whose location is in the range From .. To (not
   --  including the end points) will be deleted from the error listing.

   function Same_Error (M1, M2 : Error_Msg_Id) return Boolean;
   --  See if two messages have the same text. Returns true if the text
   --  of the two messages is identical, or if one of them is the same
   --  as the other with an appended "instance at xxx" tag.

   procedure Set_Msg_Blank;
   --  Sets a single blank in the message if the preceding character is a
   --  non-blank character other than a left parenthesis. Has no effect if
   --  manual quote mode is turned on.

   procedure Set_Msg_Blank_Conditional;
   --  Sets a single blank in the message if the preceding character is a
   --  non-blank character other than a left parenthesis or quote. Has no
   --  effect if manual quote mode is turned on.

   procedure Set_Msg_Char (C : Character);
   --  Add a single character to the current message. This routine does not
   --  check for special insertion characters (they are just treated as text
   --  characters if they occur).

   procedure Set_Msg_Insertion_File_Name;
   --  Handle file name insertion (left brace insertion character)

   procedure Set_Msg_Insertion_Line_Number (Loc, Flag : Source_Ptr);
   --  Handle line number insertion (# insertion character). Loc is the
   --  location to be referenced, and Flag is the location at which the
   --  flag is posted (used to determine whether to add "in file xxx")

   procedure Set_Msg_Insertion_Name;
   --  Handle name insertion (% insertion character)

   procedure Set_Msg_Insertion_Reserved_Name;
   --  Handle insertion of reserved word name (* insertion character).

   procedure Set_Msg_Insertion_Reserved_Word
     (Text : String;
      J    : in out Integer);
   --  Handle reserved word insertion (upper case letters). The Text argument
   --  is the current error message input text, and J is an index which on
   --  entry points to the first character of the reserved word, and on exit
   --  points past the last character of the reserved word.

   procedure Set_Msg_Insertion_Run_Time_Name;
   --  If package System contains a definition for Run_Time_Name (see package
   --  Targparm for details), then this procedure will insert a message of
   --  the form (name) into the current error message, with name set in mixed
   --  case (upper case after any spaces). If no run time name is defined,
   --  then this routine has no effect).

   procedure Set_Msg_Insertion_Uint;
   --  Handle Uint insertion (^ insertion character)

   procedure Set_Msg_Int (Line : Int);
   --  Set the decimal representation of the argument in the error message
   --  buffer with no leading zeroes output.

   procedure Set_Msg_Name_Buffer;
   --  Output name from Name_Buffer, with surrounding quotes unless manual
   --  quotation mode is in effect.

   procedure Set_Msg_Quote;
   --  Set quote if in normal quote mode, nothing if in manual quote mode

   procedure Set_Msg_Str (Text : String);
   --  Add a sequence of characters to the current message. This routine does
   --  not check for special insertion characters (they are just treated as
   --  text characters if they occur).

   procedure Set_Next_Non_Deleted_Msg (E : in out Error_Msg_Id);
   --  Given a message id, move to next message id, but skip any deleted
   --  messages, so that this results in E on output being the first non-
   --  deleted message following the input value of E, or No_Error_Msg if
   --  the input value of E was either already No_Error_Msg, or was the
   --  last non-deleted message.

   procedure Set_Warnings_Mode_Off (Loc : Source_Ptr);
   --  Called in response to a pragma Warnings (Off) to record the source
   --  location from which warnings are to be turned off.

   procedure Set_Warnings_Mode_On (Loc : Source_Ptr);
   --  Called in response to a pragma Warnings (On) to record the source
   --  location from which warnings are to be turned back on.

   procedure Test_Style_Warning_Serious_Msg (Msg : String);
   --  Sets Is_Warning_Msg true if Msg is a warning message (contains a
   --  question mark character), and False otherwise. Sets Is_Style_Msg
   --  true if Msg is a style message (starts with "(style)"). Sets
   --  Is_Serious_Error True unless the message is a warning or style
   --  message or contains the character | indicating a non-serious
   --  error message. Note that the call has no effect for continuation
   --  messages (those whose first character is \).

   function Warnings_Suppressed (Loc : Source_Ptr) return Boolean;
   --  Determines if given location is covered by a warnings off suppression
   --  range in the warnings table (or is suppressed by compilation option,
   --  which generates a warning range for the whole source file).

end Erroutc;
