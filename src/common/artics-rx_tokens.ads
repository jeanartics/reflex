------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                S C A N S                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2014, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Artics.Types; use Artics.Types;

package Artics.Rx_Tokens is

--  The scanner maintains a current state in the global variables defined
--  in this package. The call to the Scan routine advances this state to
--  the next token. The state is initialized by the call to one of the
--  initialization routines in Sinput.

   --  The following type is used to identify token types returned by Scan.
   --  The class column in this table indicates the token classes which
   --  apply to the token, as defined by subsequent subtype declarations.

   --  Note: Namet.Is_Keyword_Name depends on the fact that the first entry in
   --  this type declaration is *not* for a reserved word. For details on why
   --  there is this requirement, see Initialize_Ada_Keywords below.

   type Token_Type is (

      --  Token name          Token type   Class(es)

      Tok_Integer_Literal, -- numeric lit  Literal, Lit_Or_Name

      Tok_Real_Literal,    -- numeric lit  Literal, Lit_Or_Name

      Tok_String_Literal,  -- string lit   Literal. Lit_Or_Name

      Tok_Char_Literal,    -- char lit     Name, Literal. Lit_Or_Name

      Tok_Operator_Symbol, -- op symbol    Name, Literal, Lit_Or_Name, Desig

      Tok_Identifier,      -- identifier   Name, Lit_Or_Name, Desig

      Tok_Double_Asterisk, -- **

      Tok_Ampersand,       -- &            Binary_Addop
      Tok_Minus,           -- -            Binary_Addop, Unary_Addop
      Tok_Plus,            -- +            Binary_Addop, Unary_Addop

      Tok_Asterisk,        -- *            Mulop
      Tok_Mod,             -- MOD          Mulop
      Tok_Rem,             -- REM          Mulop
      Tok_Slash,           -- /            Mulop

      Tok_New,             -- NEW

      Tok_Abs,             -- ABS
      Tok_Others,          -- OTHERS
      Tok_Null,            -- NULL

      --  Note: Tok_Raise is in no categories now, it used to be Cterm, Eterm,
      --  After_SM, but now that Ada 2012 has added raise expressions, the
      --  raise token can appear anywhere. Note in particular that Tok_Raise
      --  being in Eterm stopped the parser from recognizing "return raise
      --  exception-name". This degrades error recovery slightly, and perhaps
      --  we could do better, but not worth the effort.

      Tok_Raise,           -- RAISE

      Tok_Dot,             -- .            Namext
      Tok_Apostrophe,      -- '            Namext

      Tok_Left_Paren,      -- (            Namext, Consk

      Tok_Delta,           -- DELTA        Atkwd, Sterm, Consk
      Tok_Digits,          -- DIGITS       Atkwd, Sterm, Consk
      Tok_Range,           -- RANGE        Atkwd, Sterm, Consk

      Tok_Right_Paren,     -- )            Sterm
      Tok_Comma,           -- ,            Sterm

      Tok_And,             -- AND          Logop, Sterm
      Tok_Or,              -- OR           Logop, Sterm
      Tok_Xor,             -- XOR          Logop, Sterm

      Tok_Less,            -- <            Relop, Sterm
      Tok_Equal,           -- =            Relop, Sterm
      Tok_Greater,         -- >            Relop, Sterm
      Tok_Not_Equal,       -- /=           Relop, Sterm
      Tok_Greater_Equal,   -- >=           Relop, Sterm
      Tok_Less_Equal,      -- <=           Relop, Sterm

      Tok_In,              -- IN           Relop, Sterm
      Tok_Not,             -- NOT          Relop, Sterm

      Tok_Box,             -- <>           Relop, Eterm, Sterm
      Tok_Colon_Equal,     -- :=           Eterm, Sterm
      Tok_Colon,           -- :            Eterm, Sterm
      Tok_Greater_Greater, -- >>           Eterm, Sterm

      Tok_Abstract,        -- ABSTRACT     Eterm, Sterm
      Tok_Access,          -- ACCESS       Eterm, Sterm
      Tok_Aliased,         -- ALIASED      Eterm, Sterm
      Tok_All,             -- ALL          Eterm, Sterm
      Tok_Array,           -- ARRAY        Eterm, Sterm
      Tok_At,              -- AT           Eterm, Sterm
      Tok_Body,            -- BODY         Eterm, Sterm
      Tok_Constant,        -- CONSTANT     Eterm, Sterm
      Tok_Do,              -- DO           Eterm, Sterm
      Tok_Is,              -- IS           Eterm, Sterm
      Tok_Interface,       -- INTERFACE    Eterm, Sterm
      Tok_Limited,         -- LIMITED      Eterm, Sterm
      Tok_Of,              -- OF           Eterm, Sterm
      Tok_Out,             -- OUT          Eterm, Sterm
      Tok_Record,          -- RECORD       Eterm, Sterm
      Tok_Renames,         -- RENAMES      Eterm, Sterm
      Tok_Reverse,         -- REVERSE      Eterm, Sterm
      Tok_Some,            -- SOME         Eterm, Sterm
      Tok_Tagged,          -- TAGGED       Eterm, Sterm
      Tok_Then,            -- THEN         Eterm, Sterm

      Tok_Less_Less,       -- <<           Eterm, Sterm, After_SM

      Tok_Abort,           -- ABORT        Eterm, Sterm, After_SM
      Tok_Accept,          -- ACCEPT       Eterm, Sterm, After_SM
      Tok_Case,            -- CASE         Eterm, Sterm, After_SM
      Tok_Delay,           -- DELAY        Eterm, Sterm, After_SM
      Tok_Else,            -- ELSE         Eterm, Sterm, After_SM
      Tok_Elsif,           -- ELSIF        Eterm, Sterm, After_SM
      Tok_End,             -- END          Eterm, Sterm, After_SM
      Tok_Exception,       -- EXCEPTION    Eterm, Sterm, After_SM
      Tok_Exit,            -- EXIT         Eterm, Sterm, After_SM
      Tok_Goto,            -- GOTO         Eterm, Sterm, After_SM
      Tok_If,              -- IF           Eterm, Sterm, After_SM
      Tok_Pragma,          -- PRAGMA       Eterm, Sterm, After_SM
      Tok_Requeue,         -- REQUEUE      Eterm, Sterm, After_SM
      Tok_Return,          -- RETURN       Eterm, Sterm, After_SM
      Tok_Select,          -- SELECT       Eterm, Sterm, After_SM
      Tok_Terminate,       -- TERMINATE    Eterm, Sterm, After_SM
      Tok_Until,           -- UNTIL        Eterm, Sterm, After_SM
      Tok_When,            -- WHEN         Eterm, Sterm, After_SM

      Tok_Begin,           -- BEGIN        Eterm, Sterm, After_SM, Labeled_Stmt
      Tok_Declare,         -- DECLARE      Eterm, Sterm, After_SM, Labeled_Stmt
      Tok_For,             -- FOR          Eterm, Sterm, After_SM, Labeled_Stmt
      Tok_Loop,            -- LOOP         Eterm, Sterm, After_SM, Labeled_Stmt
      Tok_While,           -- WHILE        Eterm, Sterm, After_SM, Labeled_Stmt

      Tok_Entry,           -- ENTRY        Eterm, Sterm, Declk, Deckn, After_SM
      Tok_Protected,       -- PROTECTED    Eterm, Sterm, Declk, Deckn, After_SM
      Tok_Task,            -- TASK         Eterm, Sterm, Declk, Deckn, After_SM
      Tok_Type,            -- TYPE         Eterm, Sterm, Declk, Deckn, After_SM
      Tok_Subtype,         -- SUBTYPE      Eterm, Sterm, Declk, Deckn, After_SM
      Tok_Overriding,      -- OVERRIDING   Eterm, Sterm, Declk, Declk, After_SM
      Tok_Synchronized,    -- SYNCHRONIZED Eterm, Sterm, Declk, Deckn, After_SM
      Tok_Use,             -- USE          Eterm, Sterm, Declk, Deckn, After_SM

      Tok_Function,        -- FUNCTION     Eterm, Sterm, Cunit, Declk, After_SM
      Tok_Generic,         -- GENERIC      Eterm, Sterm, Cunit, Declk, After_SM
      Tok_Package,         -- PACKAGE      Eterm, Sterm, Cunit, Declk, After_SM
      Tok_Procedure,       -- PROCEDURE    Eterm, Sterm, Cunit, Declk, After_SM

      Tok_Private,         -- PRIVATE      Eterm, Sterm, Cunit, After_SM
      Tok_With,            -- WITH         Eterm, Sterm, Cunit, After_SM
      Tok_Separate,        -- SEPARATE     Eterm, Sterm, Cunit, After_SM

      Tok_EOF,             -- End of file  Eterm, Sterm, Cterm, After_SM

      Tok_Semicolon,       -- ;            Eterm, Sterm, Cterm

      Tok_Arrow,           -- =>           Sterm, Cterm, Chtok

      Tok_Vertical_Bar,    -- |            Cterm, Sterm, Chtok

      Tok_Dot_Dot,         -- ..           Sterm, Chtok

      Tok_Reactive,        -- REACTIVE
      Tok_Wait,            -- WAIT         Reacive_Labeled_Stmt
      Tok_Fork,            -- FORK
      Tok_Pause,           -- PAUSE
      Tok_Step,            -- STEP
      Tok_Flow,            -- FLOW

      Tok_Project,
      Tok_Extends,
      Tok_External,
      Tok_External_As_List,
      --  These four entries represent keywords for the project file language
      --  and can be returned only in the case of scanning project files.

      Tok_Comment,
      --  This entry is used when scanning project files (where it represents
      --  an entire comment), and in preprocessing with the -C switch set
      --  (where it represents just the "--" of a comment). For the project
      --  file case, the text of the comment is stored in Comment_Id.

      Tok_End_Of_Line,
      --  Represents an end of line. Not used during normal compilation scans
      --  where end of line is ignored. Active for preprocessor scanning and
      --  also when scanning project files (where it is needed because of ???)

      Tok_Special,
      --  Used only in preprocessor scanning (to represent one of the
      --  characters '#', '$', '?', '@', '`', '\', '^', '~', or '_'. The
      --  character value itself is stored in Scans.Special_Character.

      Tok_SPARK_Hide,
      --  HIDE directive in SPARK

      No_Token);
      --  No_Token is used for initializing Token values to indicate that
      --  no value has been set yet.

   --  Note: in the RM, operator symbol is a special case of string literal.
   --  We distinguish at the lexical level in this compiler, since there are
   --  many syntactic situations in which only an operator symbol is allowed.

   --  The following subtype declarations group the token types into classes.
   --  These are used for class tests in the parser.

   subtype Token_Class_Numeric_Literal is
     Token_Type range Tok_Integer_Literal .. Tok_Real_Literal;
   --  Numeric literal

   subtype Token_Class_Literal is
     Token_Type range Tok_Integer_Literal .. Tok_Operator_Symbol;
   --  Literal

   subtype Token_Class_Lit_Or_Name is
     Token_Type range Tok_Integer_Literal .. Tok_Identifier;

   subtype Token_Class_Binary_Addop is
     Token_Type range Tok_Ampersand .. Tok_Plus;
   --  Binary adding operator (& + -)

   subtype Token_Class_Unary_Addop is
     Token_Type range Tok_Minus .. Tok_Plus;
   --  Unary adding operator (+ -)

   subtype Token_Class_Mulop is
     Token_Type range Tok_Asterisk .. Tok_Slash;
   --  Multiplying operator

   subtype Token_Class_Logop is
     Token_Type range Tok_And .. Tok_Xor;
   --  Logical operator (and, or, xor)

   subtype Token_Class_Relop is
     Token_Type range Tok_Less .. Tok_Box;
   --  Relational operator (= /= < <= > >= not, in plus <> to catch misuse
   --  of Pascal style not equal operator).

   subtype Token_Class_Name is
     Token_Type range Tok_Char_Literal .. Tok_Identifier;
   --  First token of name (4.1),
   --    (identifier, char literal, operator symbol)

   subtype Token_Class_Desig is
     Token_Type range Tok_Operator_Symbol .. Tok_Identifier;
   --  Token which can be a Designator (identifier, operator symbol)

   subtype Token_Class_Namext is
     Token_Type range Tok_Dot .. Tok_Left_Paren;
   --  Name extension tokens. These are tokens which can appear immediately
   --  after a name to extend it recursively (period, quote, left paren)

   subtype Token_Class_Consk is
     Token_Type range Tok_Left_Paren .. Tok_Range;
   --  Keywords which can start constraint
   --    (left paren, delta, digits, range)

   subtype Token_Class_Eterm is
     Token_Type range Tok_Colon_Equal .. Tok_Semicolon;
   --  Expression terminators. These tokens can never appear within a simple
   --  expression. This is used for error recovery purposes (if we encounter
   --  an error in an expression, we simply scan to the next Eterm token).

   subtype Token_Class_Sterm is
     Token_Type range Tok_Delta .. Tok_Dot_Dot;
   --  Simple_Expression terminators. A Simple_Expression must be followed
   --  by a token in this class, or an error message is issued complaining
   --  about a missing binary operator.

   subtype Token_Class_Atkwd is
     Token_Type range Tok_Delta .. Tok_Range;
   --  Attribute keywords. This class includes keywords which can be used
   --  as an Attribute_Designator, namely DELTA, DIGITS and RANGE

   subtype Token_Class_Cterm is
     Token_Type range Tok_EOF .. Tok_Vertical_Bar;
   --  Choice terminators. These tokens terminate a choice. This is used for
   --  error recovery purposes (if we encounter an error in a Choice, we
   --  simply scan to the next Cterm token).

   subtype Token_Class_Chtok is
     Token_Type range Tok_Arrow .. Tok_Dot_Dot;
   --  Choice tokens. These tokens signal a choice when used in an Aggregate

   subtype Token_Class_Cunit is
     Token_Type range Tok_Function .. Tok_Separate;
   --  Tokens which can begin a compilation unit

   subtype Token_Class_Declk is
     Token_Type range Tok_Entry .. Tok_Procedure;
   --  Keywords which start a declaration

   subtype Token_Class_Deckn is
     Token_Type range Tok_Entry .. Tok_Use;
   --  Keywords which start a declaration but can't start a compilation unit

   subtype Token_Class_After_SM is
     Token_Type range Tok_Less_Less .. Tok_EOF;
   --  Tokens which always, or almost always, appear after a semicolon. Used
   --  in the Resync_Past_Semicolon routine to avoid gobbling up stuff when
   --  a semicolon is missing. Of significance only for error recovery.

   subtype Token_Class_Labeled_Stmt is
     Token_Type range Tok_Begin .. Tok_While;
   --  Tokens which start labeled statements

   type Token_Flag_Array is array (Token_Type) of Boolean;
   Is_Reserved_Keyword : constant Token_Flag_Array :=
                           Token_Flag_Array'
                             (Tok_Mod      .. Tok_Rem      => True,
                              Tok_New      .. Tok_Null     => True,
                              Tok_Delta    .. Tok_Range    => True,
                              Tok_And      .. Tok_Xor      => True,
                              Tok_In       .. Tok_Not      => True,
                              Tok_Abstract .. Tok_Then     => True,
                              Tok_Abort    .. Tok_Separate => True,
                              others                       => False);
   --  Flag array used to test for reserved word

   procedure Initialize_Ada_Keywords;
   --  Set up Token_Type values in Names table entries for Ada reserved
   --  words. This ignores Ada_Version; Ada_Version is taken into account in
   --  Snames.Is_Keyword_Name.
   
end Artics.Rx_Tokens;
