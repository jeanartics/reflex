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

with Artics.Hostparm; use Artics.Hostparm;
with Artics.Types;    use Artics.Types;

pragma Warnings (Off);
--  This package is used also by gnatcoll
with System.Strings; use System.Strings;
with System.WCh_Con; use System.WCh_Con;
pragma Warnings (On);

package Artics.Opt is

   type Warning_Mode_Type is (Suppress, Normal, Treat_As_Error);
   Warning_Mode : Warning_Mode_Type := Normal;
   --  Controls treatment of warning messages. If set to Suppress, warning
   --  messages are not generated at all. In Normal mode, they are generated
   --  but do not count as errors. In Treat_As_Error mode, warning messages
   --  are generated and are treated as errors.

   Wide_Character_Encoding_Method : WC_Encoding_Method := WCEM_Brackets;
   --  Method used for encoding wide characters in the source program. See
   --  description of type in unit System.WCh_Con for a list of the methods
   --  that are currently supported. Note that brackets notation is always
   --  recognized in source programs regardless of the setting of this
   --  variable. The default setting causes only the brackets notation to be
   --  recognized. If this is the main unit, this setting also controls the
   --  output of the W=? parameter in the ALI file, which is used to provide
   --  the default for encoding [Wide_[Wide_]]Text_IO files. For the binder,
   --  the value set here overrides this main unit default.

   Wide_Character_Encoding_Method_Specified : Boolean := False;
   --  GNAT, GNATBIND
   --  Set True if the value in Wide_Character_Encoding_Method was set as
   --  a result of an explicit -gnatW? or -W? switch. False otherwise.
   
   Upper_Half_Encoding : Boolean := False;
   --  GNAT, GNATBIND
   --  Normally set False, indicating that upper half ISO 8859-1 characters are
   --  used in the normal way to represent themselves. If the wide character
   --  encoding method uses the upper bit for this encoding, then this flag is
   --  set True, and upper half characters in the source indicate the start of
   --  a wide character sequence. Set by -gnatW or -W switches.
   
   Identifier_Character_Set : Character := '1';
   --  GNAT
   --  This variable indicates the character set to be used for identifiers.
   --  The possible settings are:
   --    '1'  Latin-1 (ISO-8859-1)
   --    '2'  Latin-2 (ISO-8859-2)
   --    '3'  Latin-3 (ISO-8859-3)
   --    '4'  Latin-4 (ISO-8859-4)
   --    '5'  Latin-Cyrillic (ISO-8859-5)
   --    '9'  Latin-9 (ISO-8859-15)
   --    'p'  PC (US, IBM page 437)
   --    '8'  PC (European, IBM page 850)
   --    'f'  Full upper set (all distinct)
   --    'n'  No upper characters (Ada 83 rules)
   --    'w'  Latin-1 plus wide characters allowed in identifiers
   --
   --  The setting affects the set of letters allowed in identifiers and the
   --  upper/lower case equivalences. It does not affect the interpretation of
   --  character and string literals, which are always stored using the actual
   --  coding in the source program. This variable is initialized to the
   --  default value appropriate to the system (in Osint.Initialize), and then
   --  reset if a command line switch is used to change the setting.
   
end Artics.Opt;
