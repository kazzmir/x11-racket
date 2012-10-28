#lang racket

(require ffi/unsafe)
(provide KeySym)

;;; from keysymdef.h
;;; Author: Laurent Orseau -- laurent orseau gmail com

;;; See also keysymdef.rkt

#| 
(almost entirely) generated with (except the -enum definition header):

sed -E /usr/include/X11/keysymdef.h \
-e 's@/\*(.*)\*/@;\1@' \
-e 's@/\*@#|@g' \
-e 's@\*/@|#@g' \
-e 's/^#define\s*(\S*\s*)0x(\S*)/\1= #x\2/' \
-e 's/^#ifdef/;#ifdef/' \
-e 's/^#endif/;#endif/' \
-e 's/-/-/g' \
> keysymdef.rkt

|#

#|**********************************************************
Copyright 1987, 1994, 1998  The Open Group

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation.

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE OPEN GROUP BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of The Open Group shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from The Open Group.


Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the name of Digital not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

*****************************************************************|#

#|
 * The "X11 Window System Protocol" standard defines in Appendix A the
 * keysym codes. These 29-bit integer values identify characters or
 * functions associated with each key (e.g., via the visible
 * engraving) of a keyboard layout. This file assigns mnemonic macro
 * names for these keysyms.
 *
 * This file is also compiled (by src/util/makekeys.c in libX11) into
 * hash tables that can be accessed with X11 library functions such as
 * XStringToKeysym() and XKeysymToString().
 *
 * Where a keysym corresponds one-to-one to an ISO 10646 / Unicode
 * character, this is noted in a comment that provides both the U+xxxx
 * Unicode position, as well as the official Unicode name of the
 * character.
 *
 * Where the correspondence is either not one-to-one or semantically
 * unclear, the Unicode position and name are enclosed in
 * parentheses. Such legacy keysyms should be considered deprecated
 * and are not recommended for use in future keyboard mappings.
 *
 * For any future extension of the keysyms with characters already
 * found in ISO 10646 / Unicode, the following algorithm shall be
 * used. The new keysym code position will simply be the character's
 * Unicode number plus 0x01000000. The keysym values in the range
 * 0x01000100 to 0x0110ffff are reserved to represent Unicode
 * characters in the range U+0100 to U+10FFFF.
 * 
 * While most newer Unicode-based X11 clients do already accept
 * Unicode-mapped keysyms in the range 0x01000100 to 0x0110ffff, it
 * will remain necessary for clients -- in the interest of
 * compatibility with existing servers -- to also understand the
 * existing legacy keysym values in the range 0x0100 to 0x20ff.
 *
 * Where several mnemonic names are defined for the same keysym in this
 * file, all but the first one listed should be considered deprecated.
 *
 * Mnemonic names for keysyms are defined in this file with lines
 * that match one of these Perl regular expressions:
 *
 *    /^\#define XK-([a-zA-Z-0-9]+)\s+0x([0-9a-f]+)\s*\/\* U+([0-9A-F]{4,6}) (.*) \*\/\s*$/
 *    /^\#define XK-([a-zA-Z-0-9]+)\s+0x([0-9a-f]+)\s*\/\*\(U+([0-9A-F]{4,6}) (.*)\)\*\/\s*$/
 *    /^\#define XK-([a-zA-Z-0-9]+)\s+0x([0-9a-f]+)\s*(\/\*\s*(.*)\s*\*\/)?\s*$/
 *
 * Before adding new keysyms, please do consider the following: In
 * addition to the keysym names defined in this file, the
 * XStringToKeysym() and XKeysymToString() functions will also handle
 * any keysym string of the form "U0020" to "U007E" and "U00A0" to
 * "U10FFFF" for all possible Unicode characters. In other words,
 * every possible Unicode character has already a keysym string
 * defined algorithmically, even if it is not listed here. Therefore,
 * defining an additional keysym macro is only necessary where a
 * non-hexadecimal mnemonic name is needed, or where the new keysym
 * does not represent any existing Unicode character.
 *
 * When adding new keysyms to this file, do not forget to also update the
 * following:
 *
 *   - the mappings in src/KeyBind.c in the repo
 *     git://anongit.freedesktop.org/xorg/lib/libX11
 *
 *   - the protocol specification in specs/XProtocol/X11.keysyms
 *     in the repo git://anongit.freedesktop.org/xorg/doc/xorg-docs
 *
 |#

(define KeySym
  (_enum 
   '(

XK-VoidSymbol                  = #xffffff  ; Void symbol 

;#ifdef XK-MISCELLANY
#|
 * TTY function keys, cleverly chosen to map to ASCII, for convenience of
 * programming, but could have been arbitrary (at the cost of lookup
 * tables in client code).
 |#

XK-BackSpace                     = #xff08  ; Back space, back char 
XK-Tab                           = #xff09
XK-Linefeed                      = #xff0a  ; Linefeed, LF 
XK-Clear                         = #xff0b
XK-Return                        = #xff0d  ; Return, enter 
XK-Pause                         = #xff13  ; Pause, hold 
XK-Scroll-Lock                   = #xff14
XK-Sys-Req                       = #xff15
XK-Escape                        = #xff1b
XK-Delete                        = #xffff  ; Delete, rubout 



; International & multi-key character composition 

XK-Multi-key                     = #xff20  ; Multi-key character compose 
XK-Codeinput                     = #xff37
XK-SingleCandidate               = #xff3c
XK-MultipleCandidate             = #xff3d
XK-PreviousCandidate             = #xff3e

; Japanese keyboard support 

XK-Kanji                         = #xff21  ; Kanji, Kanji convert 
XK-Muhenkan                      = #xff22  ; Cancel Conversion 
XK-Henkan-Mode                   = #xff23  ; Start/Stop Conversion 
XK-Henkan                        = #xff23  ; Alias for Henkan-Mode 
XK-Romaji                        = #xff24  ; to Romaji 
XK-Hiragana                      = #xff25  ; to Hiragana 
XK-Katakana                      = #xff26  ; to Katakana 
XK-Hiragana-Katakana             = #xff27  ; Hiragana/Katakana toggle 
XK-Zenkaku                       = #xff28  ; to Zenkaku 
XK-Hankaku                       = #xff29  ; to Hankaku 
XK-Zenkaku-Hankaku               = #xff2a  ; Zenkaku/Hankaku toggle 
XK-Touroku                       = #xff2b  ; Add to Dictionary 
XK-Massyo                        = #xff2c  ; Delete from Dictionary 
XK-Kana-Lock                     = #xff2d  ; Kana Lock 
XK-Kana-Shift                    = #xff2e  ; Kana Shift 
XK-Eisu-Shift                    = #xff2f  ; Alphanumeric Shift 
XK-Eisu-toggle                   = #xff30  ; Alphanumeric toggle 
XK-Kanji-Bangou                  = #xff37  ; Codeinput 
XK-Zen-Koho                      = #xff3d  ; Multiple/All Candidate(s) 
XK-Mae-Koho                      = #xff3e  ; Previous Candidate 

; 0xff31 thru 0xff3f are under XK-KOREAN 

; Cursor control & motion 

XK-Home                          = #xff50
XK-Left                          = #xff51  ; Move left, left arrow 
XK-Up                            = #xff52  ; Move up, up arrow 
XK-Right                         = #xff53  ; Move right, right arrow 
XK-Down                          = #xff54  ; Move down, down arrow 
XK-Prior                         = #xff55  ; Prior, previous 
XK-Page-Up                       = #xff55
XK-Next                          = #xff56  ; Next 
XK-Page-Down                     = #xff56
XK-End                           = #xff57  ; EOL 
XK-Begin                         = #xff58  ; BOL 


; Misc functions 

XK-Select                        = #xff60  ; Select, mark 
XK-Print                         = #xff61
XK-Execute                       = #xff62  ; Execute, run, do 
XK-Insert                        = #xff63  ; Insert, insert here 
XK-Undo                          = #xff65
XK-Redo                          = #xff66  ; Redo, again 
XK-Menu                          = #xff67
XK-Find                          = #xff68  ; Find, search 
XK-Cancel                        = #xff69  ; Cancel, stop, abort, exit 
XK-Help                          = #xff6a  ; Help 
XK-Break                         = #xff6b
XK-Mode-switch                   = #xff7e  ; Character set switch 
XK-script-switch                 = #xff7e  ; Alias for mode-switch 
XK-Num-Lock                      = #xff7f

; Keypad functions, keypad numbers cleverly chosen to map to ASCII 

XK-KP-Space                      = #xff80  ; Space 
XK-KP-Tab                        = #xff89
XK-KP-Enter                      = #xff8d  ; Enter 
XK-KP-F1                         = #xff91  ; PF1, KP-A, ... 
XK-KP-F2                         = #xff92
XK-KP-F3                         = #xff93
XK-KP-F4                         = #xff94
XK-KP-Home                       = #xff95
XK-KP-Left                       = #xff96
XK-KP-Up                         = #xff97
XK-KP-Right                      = #xff98
XK-KP-Down                       = #xff99
XK-KP-Prior                      = #xff9a
XK-KP-Page-Up                    = #xff9a
XK-KP-Next                       = #xff9b
XK-KP-Page-Down                  = #xff9b
XK-KP-End                        = #xff9c
XK-KP-Begin                      = #xff9d
XK-KP-Insert                     = #xff9e
XK-KP-Delete                     = #xff9f
XK-KP-Equal                      = #xffbd  ; Equals 
XK-KP-Multiply                   = #xffaa
XK-KP-Add                        = #xffab
XK-KP-Separator                  = #xffac  ; Separator, often comma 
XK-KP-Subtract                   = #xffad
XK-KP-Decimal                    = #xffae
XK-KP-Divide                     = #xffaf

XK-KP-0                          = #xffb0
XK-KP-1                          = #xffb1
XK-KP-2                          = #xffb2
XK-KP-3                          = #xffb3
XK-KP-4                          = #xffb4
XK-KP-5                          = #xffb5
XK-KP-6                          = #xffb6
XK-KP-7                          = #xffb7
XK-KP-8                          = #xffb8
XK-KP-9                          = #xffb9



#|
 * Auxiliary functions; note the duplicate definitions for left and right
 * function keys;  Sun keyboards and a few other manufacturers have such
 * function key groups on the left and/or right sides of the keyboard.
 * We've not found a keyboard with more than 35 function keys total.
 |#

XK-F1                            = #xffbe
XK-F2                            = #xffbf
XK-F3                            = #xffc0
XK-F4                            = #xffc1
XK-F5                            = #xffc2
XK-F6                            = #xffc3
XK-F7                            = #xffc4
XK-F8                            = #xffc5
XK-F9                            = #xffc6
XK-F10                           = #xffc7
XK-F11                           = #xffc8
XK-L1                            = #xffc8
XK-F12                           = #xffc9
XK-L2                            = #xffc9
XK-F13                           = #xffca
XK-L3                            = #xffca
XK-F14                           = #xffcb
XK-L4                            = #xffcb
XK-F15                           = #xffcc
XK-L5                            = #xffcc
XK-F16                           = #xffcd
XK-L6                            = #xffcd
XK-F17                           = #xffce
XK-L7                            = #xffce
XK-F18                           = #xffcf
XK-L8                            = #xffcf
XK-F19                           = #xffd0
XK-L9                            = #xffd0
XK-F20                           = #xffd1
XK-L10                           = #xffd1
XK-F21                           = #xffd2
XK-R1                            = #xffd2
XK-F22                           = #xffd3
XK-R2                            = #xffd3
XK-F23                           = #xffd4
XK-R3                            = #xffd4
XK-F24                           = #xffd5
XK-R4                            = #xffd5
XK-F25                           = #xffd6
XK-R5                            = #xffd6
XK-F26                           = #xffd7
XK-R6                            = #xffd7
XK-F27                           = #xffd8
XK-R7                            = #xffd8
XK-F28                           = #xffd9
XK-R8                            = #xffd9
XK-F29                           = #xffda
XK-R9                            = #xffda
XK-F30                           = #xffdb
XK-R10                           = #xffdb
XK-F31                           = #xffdc
XK-R11                           = #xffdc
XK-F32                           = #xffdd
XK-R12                           = #xffdd
XK-F33                           = #xffde
XK-R13                           = #xffde
XK-F34                           = #xffdf
XK-R14                           = #xffdf
XK-F35                           = #xffe0
XK-R15                           = #xffe0

; Modifiers 

XK-Shift-L                       = #xffe1  ; Left shift 
XK-Shift-R                       = #xffe2  ; Right shift 
XK-Control-L                     = #xffe3  ; Left control 
XK-Control-R                     = #xffe4  ; Right control 
XK-Caps-Lock                     = #xffe5  ; Caps lock 
XK-Shift-Lock                    = #xffe6  ; Shift lock 

XK-Meta-L                        = #xffe7  ; Left meta 
XK-Meta-R                        = #xffe8  ; Right meta 
XK-Alt-L                         = #xffe9  ; Left alt 
XK-Alt-R                         = #xffea  ; Right alt 
XK-Super-L                       = #xffeb  ; Left super 
XK-Super-R                       = #xffec  ; Right super 
XK-Hyper-L                       = #xffed  ; Left hyper 
XK-Hyper-R                       = #xffee  ; Right hyper 
;#endif ; XK-MISCELLANY 

#|
 * Keyboard (XKB) Extension function and modifier keys
 * (from Appendix C of "The X Keyboard Extension: Protocol Specification")
 * Byte 3 = 0xfe
 |#

;#ifdef XK-XKB-KEYS
XK-ISO-Lock                      = #xfe01
XK-ISO-Level2-Latch              = #xfe02
XK-ISO-Level3-Shift              = #xfe03
XK-ISO-Level3-Latch              = #xfe04
XK-ISO-Level3-Lock               = #xfe05
XK-ISO-Level5-Shift              = #xfe11
XK-ISO-Level5-Latch              = #xfe12
XK-ISO-Level5-Lock               = #xfe13
XK-ISO-Group-Shift               = #xff7e  ; Alias for mode-switch 
XK-ISO-Group-Latch               = #xfe06
XK-ISO-Group-Lock                = #xfe07
XK-ISO-Next-Group                = #xfe08
XK-ISO-Next-Group-Lock           = #xfe09
XK-ISO-Prev-Group                = #xfe0a
XK-ISO-Prev-Group-Lock           = #xfe0b
XK-ISO-First-Group               = #xfe0c
XK-ISO-First-Group-Lock          = #xfe0d
XK-ISO-Last-Group                = #xfe0e
XK-ISO-Last-Group-Lock           = #xfe0f

XK-ISO-Left-Tab                  = #xfe20
XK-ISO-Move-Line-Up              = #xfe21
XK-ISO-Move-Line-Down            = #xfe22
XK-ISO-Partial-Line-Up           = #xfe23
XK-ISO-Partial-Line-Down         = #xfe24
XK-ISO-Partial-Space-Left        = #xfe25
XK-ISO-Partial-Space-Right       = #xfe26
XK-ISO-Set-Margin-Left           = #xfe27
XK-ISO-Set-Margin-Right          = #xfe28
XK-ISO-Release-Margin-Left       = #xfe29
XK-ISO-Release-Margin-Right      = #xfe2a
XK-ISO-Release-Both-Margins      = #xfe2b
XK-ISO-Fast-Cursor-Left          = #xfe2c
XK-ISO-Fast-Cursor-Right         = #xfe2d
XK-ISO-Fast-Cursor-Up            = #xfe2e
XK-ISO-Fast-Cursor-Down          = #xfe2f
XK-ISO-Continuous-Underline      = #xfe30
XK-ISO-Discontinuous-Underline   = #xfe31
XK-ISO-Emphasize                 = #xfe32
XK-ISO-Center-Object             = #xfe33
XK-ISO-Enter                     = #xfe34

XK-dead-grave                    = #xfe50
XK-dead-acute                    = #xfe51
XK-dead-circumflex               = #xfe52
XK-dead-tilde                    = #xfe53
XK-dead-perispomeni              = #xfe53  ; alias for dead-tilde 
XK-dead-macron                   = #xfe54
XK-dead-breve                    = #xfe55
XK-dead-abovedot                 = #xfe56
XK-dead-diaeresis                = #xfe57
XK-dead-abovering                = #xfe58
XK-dead-doubleacute              = #xfe59
XK-dead-caron                    = #xfe5a
XK-dead-cedilla                  = #xfe5b
XK-dead-ogonek                   = #xfe5c
XK-dead-iota                     = #xfe5d
XK-dead-voiced-sound             = #xfe5e
XK-dead-semivoiced-sound         = #xfe5f
XK-dead-belowdot                 = #xfe60
XK-dead-hook                     = #xfe61
XK-dead-horn                     = #xfe62
XK-dead-stroke                   = #xfe63
XK-dead-abovecomma               = #xfe64
XK-dead-psili                    = #xfe64  ; alias for dead-abovecomma 
XK-dead-abovereversedcomma       = #xfe65
XK-dead-dasia                    = #xfe65  ; alias for dead-abovereversedcomma 
XK-dead-doublegrave              = #xfe66
XK-dead-belowring                = #xfe67
XK-dead-belowmacron              = #xfe68
XK-dead-belowcircumflex          = #xfe69
XK-dead-belowtilde               = #xfe6a
XK-dead-belowbreve               = #xfe6b
XK-dead-belowdiaeresis           = #xfe6c
XK-dead-invertedbreve            = #xfe6d
XK-dead-belowcomma               = #xfe6e
XK-dead-currency                 = #xfe6f

; dead vowels for universal syllable entry 
XK-dead-a                        = #xfe80
XK-dead-A                        = #xfe81
XK-dead-e                        = #xfe82
XK-dead-E                        = #xfe83
XK-dead-i                        = #xfe84
XK-dead-I                        = #xfe85
XK-dead-o                        = #xfe86
XK-dead-O                        = #xfe87
XK-dead-u                        = #xfe88
XK-dead-U                        = #xfe89
XK-dead-small-schwa              = #xfe8a
XK-dead-capital-schwa            = #xfe8b

XK-First-Virtual-Screen          = #xfed0
XK-Prev-Virtual-Screen           = #xfed1
XK-Next-Virtual-Screen           = #xfed2
XK-Last-Virtual-Screen           = #xfed4
XK-Terminate-Server              = #xfed5

XK-AccessX-Enable                = #xfe70
XK-AccessX-Feedback-Enable       = #xfe71
XK-RepeatKeys-Enable             = #xfe72
XK-SlowKeys-Enable               = #xfe73
XK-BounceKeys-Enable             = #xfe74
XK-StickyKeys-Enable             = #xfe75
XK-MouseKeys-Enable              = #xfe76
XK-MouseKeys-Accel-Enable        = #xfe77
XK-Overlay1-Enable               = #xfe78
XK-Overlay2-Enable               = #xfe79
XK-AudibleBell-Enable            = #xfe7a

XK-Pointer-Left                  = #xfee0
XK-Pointer-Right                 = #xfee1
XK-Pointer-Up                    = #xfee2
XK-Pointer-Down                  = #xfee3
XK-Pointer-UpLeft                = #xfee4
XK-Pointer-UpRight               = #xfee5
XK-Pointer-DownLeft              = #xfee6
XK-Pointer-DownRight             = #xfee7
XK-Pointer-Button-Dflt           = #xfee8
XK-Pointer-Button1               = #xfee9
XK-Pointer-Button2               = #xfeea
XK-Pointer-Button3               = #xfeeb
XK-Pointer-Button4               = #xfeec
XK-Pointer-Button5               = #xfeed
XK-Pointer-DblClick-Dflt         = #xfeee
XK-Pointer-DblClick1             = #xfeef
XK-Pointer-DblClick2             = #xfef0
XK-Pointer-DblClick3             = #xfef1
XK-Pointer-DblClick4             = #xfef2
XK-Pointer-DblClick5             = #xfef3
XK-Pointer-Drag-Dflt             = #xfef4
XK-Pointer-Drag1                 = #xfef5
XK-Pointer-Drag2                 = #xfef6
XK-Pointer-Drag3                 = #xfef7
XK-Pointer-Drag4                 = #xfef8
XK-Pointer-Drag5                 = #xfefd

XK-Pointer-EnableKeys            = #xfef9
XK-Pointer-Accelerate            = #xfefa
XK-Pointer-DfltBtnNext           = #xfefb
XK-Pointer-DfltBtnPrev           = #xfefc

;#endif ; XK-XKB-KEYS 

#|
 * 3270 Terminal Keys
 * Byte 3 = 0xfd
 |#

;#ifdef XK-3270
XK-3270-Duplicate                = #xfd01
XK-3270-FieldMark                = #xfd02
XK-3270-Right2                   = #xfd03
XK-3270-Left2                    = #xfd04
XK-3270-BackTab                  = #xfd05
XK-3270-EraseEOF                 = #xfd06
XK-3270-EraseInput               = #xfd07
XK-3270-Reset                    = #xfd08
XK-3270-Quit                     = #xfd09
XK-3270-PA1                      = #xfd0a
XK-3270-PA2                      = #xfd0b
XK-3270-PA3                      = #xfd0c
XK-3270-Test                     = #xfd0d
XK-3270-Attn                     = #xfd0e
XK-3270-CursorBlink              = #xfd0f
XK-3270-AltCursor                = #xfd10
XK-3270-KeyClick                 = #xfd11
XK-3270-Jump                     = #xfd12
XK-3270-Ident                    = #xfd13
XK-3270-Rule                     = #xfd14
XK-3270-Copy                     = #xfd15
XK-3270-Play                     = #xfd16
XK-3270-Setup                    = #xfd17
XK-3270-Record                   = #xfd18
XK-3270-ChangeScreen             = #xfd19
XK-3270-DeleteWord               = #xfd1a
XK-3270-ExSelect                 = #xfd1b
XK-3270-CursorSelect             = #xfd1c
XK-3270-PrintScreen              = #xfd1d
XK-3270-Enter                    = #xfd1e
;#endif ; XK-3270 

#|
 * Latin 1
 * (ISO/IEC 8859-1 = Unicode U+0020..U+00FF)
 * Byte 3 = 0
 |#
;#ifdef XK-LATIN1
XK-space                         = #x0020  ; U+0020 SPACE 
XK-exclam                        = #x0021  ; U+0021 EXCLAMATION MARK 
XK-quotedbl                      = #x0022  ; U+0022 QUOTATION MARK 
XK-numbersign                    = #x0023  ; U+0023 NUMBER SIGN 
XK-dollar                        = #x0024  ; U+0024 DOLLAR SIGN 
XK-percent                       = #x0025  ; U+0025 PERCENT SIGN 
XK-ampersand                     = #x0026  ; U+0026 AMPERSAND 
XK-apostrophe                    = #x0027  ; U+0027 APOSTROPHE 
XK-quoteright                    = #x0027  ; deprecated 
XK-parenleft                     = #x0028  ; U+0028 LEFT PARENTHESIS 
XK-parenright                    = #x0029  ; U+0029 RIGHT PARENTHESIS 
XK-asterisk                      = #x002a  ; U+002A ASTERISK 
XK-plus                          = #x002b  ; U+002B PLUS SIGN 
XK-comma                         = #x002c  ; U+002C COMMA 
XK-minus                         = #x002d  ; U+002D HYPHEN-MINUS 
XK-period                        = #x002e  ; U+002E FULL STOP 
XK-slash                         = #x002f  ; U+002F SOLIDUS 
XK-0                             = #x0030  ; U+0030 DIGIT ZERO 
XK-1                             = #x0031  ; U+0031 DIGIT ONE 
XK-2                             = #x0032  ; U+0032 DIGIT TWO 
XK-3                             = #x0033  ; U+0033 DIGIT THREE 
XK-4                             = #x0034  ; U+0034 DIGIT FOUR 
XK-5                             = #x0035  ; U+0035 DIGIT FIVE 
XK-6                             = #x0036  ; U+0036 DIGIT SIX 
XK-7                             = #x0037  ; U+0037 DIGIT SEVEN 
XK-8                             = #x0038  ; U+0038 DIGIT EIGHT 
XK-9                             = #x0039  ; U+0039 DIGIT NINE 
XK-colon                         = #x003a  ; U+003A COLON 
XK-semicolon                     = #x003b  ; U+003B SEMICOLON 
XK-less                          = #x003c  ; U+003C LESS-THAN SIGN 
XK-equal                         = #x003d  ; U+003D EQUALS SIGN 
XK-greater                       = #x003e  ; U+003E GREATER-THAN SIGN 
XK-question                      = #x003f  ; U+003F QUESTION MARK 
XK-at                            = #x0040  ; U+0040 COMMERCIAL AT 
XK-A                             = #x0041  ; U+0041 LATIN CAPITAL LETTER A 
XK-B                             = #x0042  ; U+0042 LATIN CAPITAL LETTER B 
XK-C                             = #x0043  ; U+0043 LATIN CAPITAL LETTER C 
XK-D                             = #x0044  ; U+0044 LATIN CAPITAL LETTER D 
XK-E                             = #x0045  ; U+0045 LATIN CAPITAL LETTER E 
XK-F                             = #x0046  ; U+0046 LATIN CAPITAL LETTER F 
XK-G                             = #x0047  ; U+0047 LATIN CAPITAL LETTER G 
XK-H                             = #x0048  ; U+0048 LATIN CAPITAL LETTER H 
XK-I                             = #x0049  ; U+0049 LATIN CAPITAL LETTER I 
XK-J                             = #x004a  ; U+004A LATIN CAPITAL LETTER J 
XK-K                             = #x004b  ; U+004B LATIN CAPITAL LETTER K 
XK-L                             = #x004c  ; U+004C LATIN CAPITAL LETTER L 
XK-M                             = #x004d  ; U+004D LATIN CAPITAL LETTER M 
XK-N                             = #x004e  ; U+004E LATIN CAPITAL LETTER N 
XK-O                             = #x004f  ; U+004F LATIN CAPITAL LETTER O 
XK-P                             = #x0050  ; U+0050 LATIN CAPITAL LETTER P 
XK-Q                             = #x0051  ; U+0051 LATIN CAPITAL LETTER Q 
XK-R                             = #x0052  ; U+0052 LATIN CAPITAL LETTER R 
XK-S                             = #x0053  ; U+0053 LATIN CAPITAL LETTER S 
XK-T                             = #x0054  ; U+0054 LATIN CAPITAL LETTER T 
XK-U                             = #x0055  ; U+0055 LATIN CAPITAL LETTER U 
XK-V                             = #x0056  ; U+0056 LATIN CAPITAL LETTER V 
XK-W                             = #x0057  ; U+0057 LATIN CAPITAL LETTER W 
XK-X                             = #x0058  ; U+0058 LATIN CAPITAL LETTER X 
XK-Y                             = #x0059  ; U+0059 LATIN CAPITAL LETTER Y 
XK-Z                             = #x005a  ; U+005A LATIN CAPITAL LETTER Z 
XK-bracketleft                   = #x005b  ; U+005B LEFT SQUARE BRACKET 
XK-backslash                     = #x005c  ; U+005C REVERSE SOLIDUS 
XK-bracketright                  = #x005d  ; U+005D RIGHT SQUARE BRACKET 
XK-asciicircum                   = #x005e  ; U+005E CIRCUMFLEX ACCENT 
XK-underscore                    = #x005f  ; U+005F LOW LINE 
XK-grave                         = #x0060  ; U+0060 GRAVE ACCENT 
XK-quoteleft                     = #x0060  ; deprecated 
XK-a                             = #x0061  ; U+0061 LATIN SMALL LETTER A 
XK-b                             = #x0062  ; U+0062 LATIN SMALL LETTER B 
XK-c                             = #x0063  ; U+0063 LATIN SMALL LETTER C 
XK-d                             = #x0064  ; U+0064 LATIN SMALL LETTER D 
XK-e                             = #x0065  ; U+0065 LATIN SMALL LETTER E 
XK-f                             = #x0066  ; U+0066 LATIN SMALL LETTER F 
XK-g                             = #x0067  ; U+0067 LATIN SMALL LETTER G 
XK-h                             = #x0068  ; U+0068 LATIN SMALL LETTER H 
XK-i                             = #x0069  ; U+0069 LATIN SMALL LETTER I 
XK-j                             = #x006a  ; U+006A LATIN SMALL LETTER J 
XK-k                             = #x006b  ; U+006B LATIN SMALL LETTER K 
XK-l                             = #x006c  ; U+006C LATIN SMALL LETTER L 
XK-m                             = #x006d  ; U+006D LATIN SMALL LETTER M 
XK-n                             = #x006e  ; U+006E LATIN SMALL LETTER N 
XK-o                             = #x006f  ; U+006F LATIN SMALL LETTER O 
XK-p                             = #x0070  ; U+0070 LATIN SMALL LETTER P 
XK-q                             = #x0071  ; U+0071 LATIN SMALL LETTER Q 
XK-r                             = #x0072  ; U+0072 LATIN SMALL LETTER R 
XK-s                             = #x0073  ; U+0073 LATIN SMALL LETTER S 
XK-t                             = #x0074  ; U+0074 LATIN SMALL LETTER T 
XK-u                             = #x0075  ; U+0075 LATIN SMALL LETTER U 
XK-v                             = #x0076  ; U+0076 LATIN SMALL LETTER V 
XK-w                             = #x0077  ; U+0077 LATIN SMALL LETTER W 
XK-x                             = #x0078  ; U+0078 LATIN SMALL LETTER X 
XK-y                             = #x0079  ; U+0079 LATIN SMALL LETTER Y 
XK-z                             = #x007a  ; U+007A LATIN SMALL LETTER Z 
XK-braceleft                     = #x007b  ; U+007B LEFT CURLY BRACKET 
XK-bar                           = #x007c  ; U+007C VERTICAL LINE 
XK-braceright                    = #x007d  ; U+007D RIGHT CURLY BRACKET 
XK-asciitilde                    = #x007e  ; U+007E TILDE 

XK-nobreakspace                  = #x00a0  ; U+00A0 NO-BREAK SPACE 
XK-exclamdown                    = #x00a1  ; U+00A1 INVERTED EXCLAMATION MARK 
XK-cent                          = #x00a2  ; U+00A2 CENT SIGN 
XK-sterling                      = #x00a3  ; U+00A3 POUND SIGN 
XK-currency                      = #x00a4  ; U+00A4 CURRENCY SIGN 
XK-yen                           = #x00a5  ; U+00A5 YEN SIGN 
XK-brokenbar                     = #x00a6  ; U+00A6 BROKEN BAR 
XK-section                       = #x00a7  ; U+00A7 SECTION SIGN 
XK-diaeresis                     = #x00a8  ; U+00A8 DIAERESIS 
XK-copyright                     = #x00a9  ; U+00A9 COPYRIGHT SIGN 
XK-ordfeminine                   = #x00aa  ; U+00AA FEMININE ORDINAL INDICATOR 
XK-guillemotleft                 = #x00ab  ; U+00AB LEFT-POINTING DOUBLE ANGLE QUOTATION MARK 
XK-notsign                       = #x00ac  ; U+00AC NOT SIGN 
XK-hyphen                        = #x00ad  ; U+00AD SOFT HYPHEN 
XK-registered                    = #x00ae  ; U+00AE REGISTERED SIGN 
XK-macron                        = #x00af  ; U+00AF MACRON 
XK-degree                        = #x00b0  ; U+00B0 DEGREE SIGN 
XK-plusminus                     = #x00b1  ; U+00B1 PLUS-MINUS SIGN 
XK-twosuperior                   = #x00b2  ; U+00B2 SUPERSCRIPT TWO 
XK-threesuperior                 = #x00b3  ; U+00B3 SUPERSCRIPT THREE 
XK-acute                         = #x00b4  ; U+00B4 ACUTE ACCENT 
XK-mu                            = #x00b5  ; U+00B5 MICRO SIGN 
XK-paragraph                     = #x00b6  ; U+00B6 PILCROW SIGN 
XK-periodcentered                = #x00b7  ; U+00B7 MIDDLE DOT 
XK-cedilla                       = #x00b8  ; U+00B8 CEDILLA 
XK-onesuperior                   = #x00b9  ; U+00B9 SUPERSCRIPT ONE 
XK-masculine                     = #x00ba  ; U+00BA MASCULINE ORDINAL INDICATOR 
XK-guillemotright                = #x00bb  ; U+00BB RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK 
XK-onequarter                    = #x00bc  ; U+00BC VULGAR FRACTION ONE QUARTER 
XK-onehalf                       = #x00bd  ; U+00BD VULGAR FRACTION ONE HALF 
XK-threequarters                 = #x00be  ; U+00BE VULGAR FRACTION THREE QUARTERS 
XK-questiondown                  = #x00bf  ; U+00BF INVERTED QUESTION MARK 
XK-Agrave                        = #x00c0  ; U+00C0 LATIN CAPITAL LETTER A WITH GRAVE 
XK-Aacute                        = #x00c1  ; U+00C1 LATIN CAPITAL LETTER A WITH ACUTE 
XK-Acircumflex                   = #x00c2  ; U+00C2 LATIN CAPITAL LETTER A WITH CIRCUMFLEX 
XK-Atilde                        = #x00c3  ; U+00C3 LATIN CAPITAL LETTER A WITH TILDE 
XK-Adiaeresis                    = #x00c4  ; U+00C4 LATIN CAPITAL LETTER A WITH DIAERESIS 
XK-Aring                         = #x00c5  ; U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE 
XK-AE                            = #x00c6  ; U+00C6 LATIN CAPITAL LETTER AE 
XK-Ccedilla                      = #x00c7  ; U+00C7 LATIN CAPITAL LETTER C WITH CEDILLA 
XK-Egrave                        = #x00c8  ; U+00C8 LATIN CAPITAL LETTER E WITH GRAVE 
XK-Eacute                        = #x00c9  ; U+00C9 LATIN CAPITAL LETTER E WITH ACUTE 
XK-Ecircumflex                   = #x00ca  ; U+00CA LATIN CAPITAL LETTER E WITH CIRCUMFLEX 
XK-Ediaeresis                    = #x00cb  ; U+00CB LATIN CAPITAL LETTER E WITH DIAERESIS 
XK-Igrave                        = #x00cc  ; U+00CC LATIN CAPITAL LETTER I WITH GRAVE 
XK-Iacute                        = #x00cd  ; U+00CD LATIN CAPITAL LETTER I WITH ACUTE 
XK-Icircumflex                   = #x00ce  ; U+00CE LATIN CAPITAL LETTER I WITH CIRCUMFLEX 
XK-Idiaeresis                    = #x00cf  ; U+00CF LATIN CAPITAL LETTER I WITH DIAERESIS 
XK-ETH                           = #x00d0  ; U+00D0 LATIN CAPITAL LETTER ETH 
XK-Eth                           = #x00d0  ; deprecated 
XK-Ntilde                        = #x00d1  ; U+00D1 LATIN CAPITAL LETTER N WITH TILDE 
XK-Ograve                        = #x00d2  ; U+00D2 LATIN CAPITAL LETTER O WITH GRAVE 
XK-Oacute                        = #x00d3  ; U+00D3 LATIN CAPITAL LETTER O WITH ACUTE 
XK-Ocircumflex                   = #x00d4  ; U+00D4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX 
XK-Otilde                        = #x00d5  ; U+00D5 LATIN CAPITAL LETTER O WITH TILDE 
XK-Odiaeresis                    = #x00d6  ; U+00D6 LATIN CAPITAL LETTER O WITH DIAERESIS 
XK-multiply                      = #x00d7  ; U+00D7 MULTIPLICATION SIGN 
XK-Oslash                        = #x00d8  ; U+00D8 LATIN CAPITAL LETTER O WITH STROKE 
XK-Ooblique                      = #x00d8  ; U+00D8 LATIN CAPITAL LETTER O WITH STROKE 
XK-Ugrave                        = #x00d9  ; U+00D9 LATIN CAPITAL LETTER U WITH GRAVE 
XK-Uacute                        = #x00da  ; U+00DA LATIN CAPITAL LETTER U WITH ACUTE 
XK-Ucircumflex                   = #x00db  ; U+00DB LATIN CAPITAL LETTER U WITH CIRCUMFLEX 
XK-Udiaeresis                    = #x00dc  ; U+00DC LATIN CAPITAL LETTER U WITH DIAERESIS 
XK-Yacute                        = #x00dd  ; U+00DD LATIN CAPITAL LETTER Y WITH ACUTE 
XK-THORN                         = #x00de  ; U+00DE LATIN CAPITAL LETTER THORN 
XK-Thorn                         = #x00de  ; deprecated 
XK-ssharp                        = #x00df  ; U+00DF LATIN SMALL LETTER SHARP S 
XK-agrave                        = #x00e0  ; U+00E0 LATIN SMALL LETTER A WITH GRAVE 
XK-aacute                        = #x00e1  ; U+00E1 LATIN SMALL LETTER A WITH ACUTE 
XK-acircumflex                   = #x00e2  ; U+00E2 LATIN SMALL LETTER A WITH CIRCUMFLEX 
XK-atilde                        = #x00e3  ; U+00E3 LATIN SMALL LETTER A WITH TILDE 
XK-adiaeresis                    = #x00e4  ; U+00E4 LATIN SMALL LETTER A WITH DIAERESIS 
XK-aring                         = #x00e5  ; U+00E5 LATIN SMALL LETTER A WITH RING ABOVE 
XK-ae                            = #x00e6  ; U+00E6 LATIN SMALL LETTER AE 
XK-ccedilla                      = #x00e7  ; U+00E7 LATIN SMALL LETTER C WITH CEDILLA 
XK-egrave                        = #x00e8  ; U+00E8 LATIN SMALL LETTER E WITH GRAVE 
XK-eacute                        = #x00e9  ; U+00E9 LATIN SMALL LETTER E WITH ACUTE 
XK-ecircumflex                   = #x00ea  ; U+00EA LATIN SMALL LETTER E WITH CIRCUMFLEX 
XK-ediaeresis                    = #x00eb  ; U+00EB LATIN SMALL LETTER E WITH DIAERESIS 
XK-igrave                        = #x00ec  ; U+00EC LATIN SMALL LETTER I WITH GRAVE 
XK-iacute                        = #x00ed  ; U+00ED LATIN SMALL LETTER I WITH ACUTE 
XK-icircumflex                   = #x00ee  ; U+00EE LATIN SMALL LETTER I WITH CIRCUMFLEX 
XK-idiaeresis                    = #x00ef  ; U+00EF LATIN SMALL LETTER I WITH DIAERESIS 
XK-eth                           = #x00f0  ; U+00F0 LATIN SMALL LETTER ETH 
XK-ntilde                        = #x00f1  ; U+00F1 LATIN SMALL LETTER N WITH TILDE 
XK-ograve                        = #x00f2  ; U+00F2 LATIN SMALL LETTER O WITH GRAVE 
XK-oacute                        = #x00f3  ; U+00F3 LATIN SMALL LETTER O WITH ACUTE 
XK-ocircumflex                   = #x00f4  ; U+00F4 LATIN SMALL LETTER O WITH CIRCUMFLEX 
XK-otilde                        = #x00f5  ; U+00F5 LATIN SMALL LETTER O WITH TILDE 
XK-odiaeresis                    = #x00f6  ; U+00F6 LATIN SMALL LETTER O WITH DIAERESIS 
XK-division                      = #x00f7  ; U+00F7 DIVISION SIGN 
XK-oslash                        = #x00f8  ; U+00F8 LATIN SMALL LETTER O WITH STROKE 
XK-ooblique                      = #x00f8  ; U+00F8 LATIN SMALL LETTER O WITH STROKE 
XK-ugrave                        = #x00f9  ; U+00F9 LATIN SMALL LETTER U WITH GRAVE 
XK-uacute                        = #x00fa  ; U+00FA LATIN SMALL LETTER U WITH ACUTE 
XK-ucircumflex                   = #x00fb  ; U+00FB LATIN SMALL LETTER U WITH CIRCUMFLEX 
XK-udiaeresis                    = #x00fc  ; U+00FC LATIN SMALL LETTER U WITH DIAERESIS 
XK-yacute                        = #x00fd  ; U+00FD LATIN SMALL LETTER Y WITH ACUTE 
XK-thorn                         = #x00fe  ; U+00FE LATIN SMALL LETTER THORN 
XK-ydiaeresis                    = #x00ff  ; U+00FF LATIN SMALL LETTER Y WITH DIAERESIS 
;#endif ; XK-LATIN1 

#|
 * Latin 2
 * Byte 3 = 1
 |#

;#ifdef XK-LATIN2
XK-Aogonek                       = #x01a1  ; U+0104 LATIN CAPITAL LETTER A WITH OGONEK 
XK-breve                         = #x01a2  ; U+02D8 BREVE 
XK-Lstroke                       = #x01a3  ; U+0141 LATIN CAPITAL LETTER L WITH STROKE 
XK-Lcaron                        = #x01a5  ; U+013D LATIN CAPITAL LETTER L WITH CARON 
XK-Sacute                        = #x01a6  ; U+015A LATIN CAPITAL LETTER S WITH ACUTE 
XK-Scaron                        = #x01a9  ; U+0160 LATIN CAPITAL LETTER S WITH CARON 
XK-Scedilla                      = #x01aa  ; U+015E LATIN CAPITAL LETTER S WITH CEDILLA 
XK-Tcaron                        = #x01ab  ; U+0164 LATIN CAPITAL LETTER T WITH CARON 
XK-Zacute                        = #x01ac  ; U+0179 LATIN CAPITAL LETTER Z WITH ACUTE 
XK-Zcaron                        = #x01ae  ; U+017D LATIN CAPITAL LETTER Z WITH CARON 
XK-Zabovedot                     = #x01af  ; U+017B LATIN CAPITAL LETTER Z WITH DOT ABOVE 
XK-aogonek                       = #x01b1  ; U+0105 LATIN SMALL LETTER A WITH OGONEK 
XK-ogonek                        = #x01b2  ; U+02DB OGONEK 
XK-lstroke                       = #x01b3  ; U+0142 LATIN SMALL LETTER L WITH STROKE 
XK-lcaron                        = #x01b5  ; U+013E LATIN SMALL LETTER L WITH CARON 
XK-sacute                        = #x01b6  ; U+015B LATIN SMALL LETTER S WITH ACUTE 
XK-caron                         = #x01b7  ; U+02C7 CARON 
XK-scaron                        = #x01b9  ; U+0161 LATIN SMALL LETTER S WITH CARON 
XK-scedilla                      = #x01ba  ; U+015F LATIN SMALL LETTER S WITH CEDILLA 
XK-tcaron                        = #x01bb  ; U+0165 LATIN SMALL LETTER T WITH CARON 
XK-zacute                        = #x01bc  ; U+017A LATIN SMALL LETTER Z WITH ACUTE 
XK-doubleacute                   = #x01bd  ; U+02DD DOUBLE ACUTE ACCENT 
XK-zcaron                        = #x01be  ; U+017E LATIN SMALL LETTER Z WITH CARON 
XK-zabovedot                     = #x01bf  ; U+017C LATIN SMALL LETTER Z WITH DOT ABOVE 
XK-Racute                        = #x01c0  ; U+0154 LATIN CAPITAL LETTER R WITH ACUTE 
XK-Abreve                        = #x01c3  ; U+0102 LATIN CAPITAL LETTER A WITH BREVE 
XK-Lacute                        = #x01c5  ; U+0139 LATIN CAPITAL LETTER L WITH ACUTE 
XK-Cacute                        = #x01c6  ; U+0106 LATIN CAPITAL LETTER C WITH ACUTE 
XK-Ccaron                        = #x01c8  ; U+010C LATIN CAPITAL LETTER C WITH CARON 
XK-Eogonek                       = #x01ca  ; U+0118 LATIN CAPITAL LETTER E WITH OGONEK 
XK-Ecaron                        = #x01cc  ; U+011A LATIN CAPITAL LETTER E WITH CARON 
XK-Dcaron                        = #x01cf  ; U+010E LATIN CAPITAL LETTER D WITH CARON 
XK-Dstroke                       = #x01d0  ; U+0110 LATIN CAPITAL LETTER D WITH STROKE 
XK-Nacute                        = #x01d1  ; U+0143 LATIN CAPITAL LETTER N WITH ACUTE 
XK-Ncaron                        = #x01d2  ; U+0147 LATIN CAPITAL LETTER N WITH CARON 
XK-Odoubleacute                  = #x01d5  ; U+0150 LATIN CAPITAL LETTER O WITH DOUBLE ACUTE 
XK-Rcaron                        = #x01d8  ; U+0158 LATIN CAPITAL LETTER R WITH CARON 
XK-Uring                         = #x01d9  ; U+016E LATIN CAPITAL LETTER U WITH RING ABOVE 
XK-Udoubleacute                  = #x01db  ; U+0170 LATIN CAPITAL LETTER U WITH DOUBLE ACUTE 
XK-Tcedilla                      = #x01de  ; U+0162 LATIN CAPITAL LETTER T WITH CEDILLA 
XK-racute                        = #x01e0  ; U+0155 LATIN SMALL LETTER R WITH ACUTE 
XK-abreve                        = #x01e3  ; U+0103 LATIN SMALL LETTER A WITH BREVE 
XK-lacute                        = #x01e5  ; U+013A LATIN SMALL LETTER L WITH ACUTE 
XK-cacute                        = #x01e6  ; U+0107 LATIN SMALL LETTER C WITH ACUTE 
XK-ccaron                        = #x01e8  ; U+010D LATIN SMALL LETTER C WITH CARON 
XK-eogonek                       = #x01ea  ; U+0119 LATIN SMALL LETTER E WITH OGONEK 
XK-ecaron                        = #x01ec  ; U+011B LATIN SMALL LETTER E WITH CARON 
XK-dcaron                        = #x01ef  ; U+010F LATIN SMALL LETTER D WITH CARON 
XK-dstroke                       = #x01f0  ; U+0111 LATIN SMALL LETTER D WITH STROKE 
XK-nacute                        = #x01f1  ; U+0144 LATIN SMALL LETTER N WITH ACUTE 
XK-ncaron                        = #x01f2  ; U+0148 LATIN SMALL LETTER N WITH CARON 
XK-odoubleacute                  = #x01f5  ; U+0151 LATIN SMALL LETTER O WITH DOUBLE ACUTE 
XK-rcaron                        = #x01f8  ; U+0159 LATIN SMALL LETTER R WITH CARON 
XK-uring                         = #x01f9  ; U+016F LATIN SMALL LETTER U WITH RING ABOVE 
XK-udoubleacute                  = #x01fb  ; U+0171 LATIN SMALL LETTER U WITH DOUBLE ACUTE 
XK-tcedilla                      = #x01fe  ; U+0163 LATIN SMALL LETTER T WITH CEDILLA 
XK-abovedot                      = #x01ff  ; U+02D9 DOT ABOVE 
;#endif ; XK-LATIN2 

#|
 * Latin 3
 * Byte 3 = 2
 |#

;#ifdef XK-LATIN3
XK-Hstroke                       = #x02a1  ; U+0126 LATIN CAPITAL LETTER H WITH STROKE 
XK-Hcircumflex                   = #x02a6  ; U+0124 LATIN CAPITAL LETTER H WITH CIRCUMFLEX 
XK-Iabovedot                     = #x02a9  ; U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE 
XK-Gbreve                        = #x02ab  ; U+011E LATIN CAPITAL LETTER G WITH BREVE 
XK-Jcircumflex                   = #x02ac  ; U+0134 LATIN CAPITAL LETTER J WITH CIRCUMFLEX 
XK-hstroke                       = #x02b1  ; U+0127 LATIN SMALL LETTER H WITH STROKE 
XK-hcircumflex                   = #x02b6  ; U+0125 LATIN SMALL LETTER H WITH CIRCUMFLEX 
XK-idotless                      = #x02b9  ; U+0131 LATIN SMALL LETTER DOTLESS I 
XK-gbreve                        = #x02bb  ; U+011F LATIN SMALL LETTER G WITH BREVE 
XK-jcircumflex                   = #x02bc  ; U+0135 LATIN SMALL LETTER J WITH CIRCUMFLEX 
XK-Cabovedot                     = #x02c5  ; U+010A LATIN CAPITAL LETTER C WITH DOT ABOVE 
XK-Ccircumflex                   = #x02c6  ; U+0108 LATIN CAPITAL LETTER C WITH CIRCUMFLEX 
XK-Gabovedot                     = #x02d5  ; U+0120 LATIN CAPITAL LETTER G WITH DOT ABOVE 
XK-Gcircumflex                   = #x02d8  ; U+011C LATIN CAPITAL LETTER G WITH CIRCUMFLEX 
XK-Ubreve                        = #x02dd  ; U+016C LATIN CAPITAL LETTER U WITH BREVE 
XK-Scircumflex                   = #x02de  ; U+015C LATIN CAPITAL LETTER S WITH CIRCUMFLEX 
XK-cabovedot                     = #x02e5  ; U+010B LATIN SMALL LETTER C WITH DOT ABOVE 
XK-ccircumflex                   = #x02e6  ; U+0109 LATIN SMALL LETTER C WITH CIRCUMFLEX 
XK-gabovedot                     = #x02f5  ; U+0121 LATIN SMALL LETTER G WITH DOT ABOVE 
XK-gcircumflex                   = #x02f8  ; U+011D LATIN SMALL LETTER G WITH CIRCUMFLEX 
XK-ubreve                        = #x02fd  ; U+016D LATIN SMALL LETTER U WITH BREVE 
XK-scircumflex                   = #x02fe  ; U+015D LATIN SMALL LETTER S WITH CIRCUMFLEX 
;#endif ; XK-LATIN3 


#|
 * Latin 4
 * Byte 3 = 3
 |#

;#ifdef XK-LATIN4
XK-kra                           = #x03a2  ; U+0138 LATIN SMALL LETTER KRA 
XK-kappa                         = #x03a2  ; deprecated 
XK-Rcedilla                      = #x03a3  ; U+0156 LATIN CAPITAL LETTER R WITH CEDILLA 
XK-Itilde                        = #x03a5  ; U+0128 LATIN CAPITAL LETTER I WITH TILDE 
XK-Lcedilla                      = #x03a6  ; U+013B LATIN CAPITAL LETTER L WITH CEDILLA 
XK-Emacron                       = #x03aa  ; U+0112 LATIN CAPITAL LETTER E WITH MACRON 
XK-Gcedilla                      = #x03ab  ; U+0122 LATIN CAPITAL LETTER G WITH CEDILLA 
XK-Tslash                        = #x03ac  ; U+0166 LATIN CAPITAL LETTER T WITH STROKE 
XK-rcedilla                      = #x03b3  ; U+0157 LATIN SMALL LETTER R WITH CEDILLA 
XK-itilde                        = #x03b5  ; U+0129 LATIN SMALL LETTER I WITH TILDE 
XK-lcedilla                      = #x03b6  ; U+013C LATIN SMALL LETTER L WITH CEDILLA 
XK-emacron                       = #x03ba  ; U+0113 LATIN SMALL LETTER E WITH MACRON 
XK-gcedilla                      = #x03bb  ; U+0123 LATIN SMALL LETTER G WITH CEDILLA 
XK-tslash                        = #x03bc  ; U+0167 LATIN SMALL LETTER T WITH STROKE 
XK-ENG                           = #x03bd  ; U+014A LATIN CAPITAL LETTER ENG 
XK-eng                           = #x03bf  ; U+014B LATIN SMALL LETTER ENG 
XK-Amacron                       = #x03c0  ; U+0100 LATIN CAPITAL LETTER A WITH MACRON 
XK-Iogonek                       = #x03c7  ; U+012E LATIN CAPITAL LETTER I WITH OGONEK 
XK-Eabovedot                     = #x03cc  ; U+0116 LATIN CAPITAL LETTER E WITH DOT ABOVE 
XK-Imacron                       = #x03cf  ; U+012A LATIN CAPITAL LETTER I WITH MACRON 
XK-Ncedilla                      = #x03d1  ; U+0145 LATIN CAPITAL LETTER N WITH CEDILLA 
XK-Omacron                       = #x03d2  ; U+014C LATIN CAPITAL LETTER O WITH MACRON 
XK-Kcedilla                      = #x03d3  ; U+0136 LATIN CAPITAL LETTER K WITH CEDILLA 
XK-Uogonek                       = #x03d9  ; U+0172 LATIN CAPITAL LETTER U WITH OGONEK 
XK-Utilde                        = #x03dd  ; U+0168 LATIN CAPITAL LETTER U WITH TILDE 
XK-Umacron                       = #x03de  ; U+016A LATIN CAPITAL LETTER U WITH MACRON 
XK-amacron                       = #x03e0  ; U+0101 LATIN SMALL LETTER A WITH MACRON 
XK-iogonek                       = #x03e7  ; U+012F LATIN SMALL LETTER I WITH OGONEK 
XK-eabovedot                     = #x03ec  ; U+0117 LATIN SMALL LETTER E WITH DOT ABOVE 
XK-imacron                       = #x03ef  ; U+012B LATIN SMALL LETTER I WITH MACRON 
XK-ncedilla                      = #x03f1  ; U+0146 LATIN SMALL LETTER N WITH CEDILLA 
XK-omacron                       = #x03f2  ; U+014D LATIN SMALL LETTER O WITH MACRON 
XK-kcedilla                      = #x03f3  ; U+0137 LATIN SMALL LETTER K WITH CEDILLA 
XK-uogonek                       = #x03f9  ; U+0173 LATIN SMALL LETTER U WITH OGONEK 
XK-utilde                        = #x03fd  ; U+0169 LATIN SMALL LETTER U WITH TILDE 
XK-umacron                       = #x03fe  ; U+016B LATIN SMALL LETTER U WITH MACRON 
;#endif ; XK-LATIN4 

#|
 * Latin 8
 |#
;#ifdef XK-LATIN8
XK-Wcircumflex                = #x1000174  ; U+0174 LATIN CAPITAL LETTER W WITH CIRCUMFLEX 
XK-wcircumflex                = #x1000175  ; U+0175 LATIN SMALL LETTER W WITH CIRCUMFLEX 
XK-Ycircumflex                = #x1000176  ; U+0176 LATIN CAPITAL LETTER Y WITH CIRCUMFLEX 
XK-ycircumflex                = #x1000177  ; U+0177 LATIN SMALL LETTER Y WITH CIRCUMFLEX 
XK-Babovedot                  = #x1001e02  ; U+1E02 LATIN CAPITAL LETTER B WITH DOT ABOVE 
XK-babovedot                  = #x1001e03  ; U+1E03 LATIN SMALL LETTER B WITH DOT ABOVE 
XK-Dabovedot                  = #x1001e0a  ; U+1E0A LATIN CAPITAL LETTER D WITH DOT ABOVE 
XK-dabovedot                  = #x1001e0b  ; U+1E0B LATIN SMALL LETTER D WITH DOT ABOVE 
XK-Fabovedot                  = #x1001e1e  ; U+1E1E LATIN CAPITAL LETTER F WITH DOT ABOVE 
XK-fabovedot                  = #x1001e1f  ; U+1E1F LATIN SMALL LETTER F WITH DOT ABOVE 
XK-Mabovedot                  = #x1001e40  ; U+1E40 LATIN CAPITAL LETTER M WITH DOT ABOVE 
XK-mabovedot                  = #x1001e41  ; U+1E41 LATIN SMALL LETTER M WITH DOT ABOVE 
XK-Pabovedot                  = #x1001e56  ; U+1E56 LATIN CAPITAL LETTER P WITH DOT ABOVE 
XK-pabovedot                  = #x1001e57  ; U+1E57 LATIN SMALL LETTER P WITH DOT ABOVE 
XK-Sabovedot                  = #x1001e60  ; U+1E60 LATIN CAPITAL LETTER S WITH DOT ABOVE 
XK-sabovedot                  = #x1001e61  ; U+1E61 LATIN SMALL LETTER S WITH DOT ABOVE 
XK-Tabovedot                  = #x1001e6a  ; U+1E6A LATIN CAPITAL LETTER T WITH DOT ABOVE 
XK-tabovedot                  = #x1001e6b  ; U+1E6B LATIN SMALL LETTER T WITH DOT ABOVE 
XK-Wgrave                     = #x1001e80  ; U+1E80 LATIN CAPITAL LETTER W WITH GRAVE 
XK-wgrave                     = #x1001e81  ; U+1E81 LATIN SMALL LETTER W WITH GRAVE 
XK-Wacute                     = #x1001e82  ; U+1E82 LATIN CAPITAL LETTER W WITH ACUTE 
XK-wacute                     = #x1001e83  ; U+1E83 LATIN SMALL LETTER W WITH ACUTE 
XK-Wdiaeresis                 = #x1001e84  ; U+1E84 LATIN CAPITAL LETTER W WITH DIAERESIS 
XK-wdiaeresis                 = #x1001e85  ; U+1E85 LATIN SMALL LETTER W WITH DIAERESIS 
XK-Ygrave                     = #x1001ef2  ; U+1EF2 LATIN CAPITAL LETTER Y WITH GRAVE 
XK-ygrave                     = #x1001ef3  ; U+1EF3 LATIN SMALL LETTER Y WITH GRAVE 
;#endif ; XK-LATIN8 

#|
 * Latin 9
 * Byte 3 = 0x13
 |#

;#ifdef XK-LATIN9
XK-OE                            = #x13bc  ; U+0152 LATIN CAPITAL LIGATURE OE 
XK-oe                            = #x13bd  ; U+0153 LATIN SMALL LIGATURE OE 
XK-Ydiaeresis                    = #x13be  ; U+0178 LATIN CAPITAL LETTER Y WITH DIAERESIS 
;#endif ; XK-LATIN9 

#|
 * Katakana
 * Byte 3 = 4
 |#

;#ifdef XK-KATAKANA
XK-overline                      = #x047e  ; U+203E OVERLINE 
XK-kana-fullstop                 = #x04a1  ; U+3002 IDEOGRAPHIC FULL STOP 
XK-kana-openingbracket           = #x04a2  ; U+300C LEFT CORNER BRACKET 
XK-kana-closingbracket           = #x04a3  ; U+300D RIGHT CORNER BRACKET 
XK-kana-comma                    = #x04a4  ; U+3001 IDEOGRAPHIC COMMA 
XK-kana-conjunctive              = #x04a5  ; U+30FB KATAKANA MIDDLE DOT 
XK-kana-middledot                = #x04a5  ; deprecated 
XK-kana-WO                       = #x04a6  ; U+30F2 KATAKANA LETTER WO 
XK-kana-a                        = #x04a7  ; U+30A1 KATAKANA LETTER SMALL A 
XK-kana-i                        = #x04a8  ; U+30A3 KATAKANA LETTER SMALL I 
XK-kana-u                        = #x04a9  ; U+30A5 KATAKANA LETTER SMALL U 
XK-kana-e                        = #x04aa  ; U+30A7 KATAKANA LETTER SMALL E 
XK-kana-o                        = #x04ab  ; U+30A9 KATAKANA LETTER SMALL O 
XK-kana-ya                       = #x04ac  ; U+30E3 KATAKANA LETTER SMALL YA 
XK-kana-yu                       = #x04ad  ; U+30E5 KATAKANA LETTER SMALL YU 
XK-kana-yo                       = #x04ae  ; U+30E7 KATAKANA LETTER SMALL YO 
XK-kana-tsu                      = #x04af  ; U+30C3 KATAKANA LETTER SMALL TU 
XK-kana-tu                       = #x04af  ; deprecated 
XK-prolongedsound                = #x04b0  ; U+30FC KATAKANA-HIRAGANA PROLONGED SOUND MARK 
XK-kana-A                        = #x04b1  ; U+30A2 KATAKANA LETTER A 
XK-kana-I                        = #x04b2  ; U+30A4 KATAKANA LETTER I 
XK-kana-U                        = #x04b3  ; U+30A6 KATAKANA LETTER U 
XK-kana-E                        = #x04b4  ; U+30A8 KATAKANA LETTER E 
XK-kana-O                        = #x04b5  ; U+30AA KATAKANA LETTER O 
XK-kana-KA                       = #x04b6  ; U+30AB KATAKANA LETTER KA 
XK-kana-KI                       = #x04b7  ; U+30AD KATAKANA LETTER KI 
XK-kana-KU                       = #x04b8  ; U+30AF KATAKANA LETTER KU 
XK-kana-KE                       = #x04b9  ; U+30B1 KATAKANA LETTER KE 
XK-kana-KO                       = #x04ba  ; U+30B3 KATAKANA LETTER KO 
XK-kana-SA                       = #x04bb  ; U+30B5 KATAKANA LETTER SA 
XK-kana-SHI                      = #x04bc  ; U+30B7 KATAKANA LETTER SI 
XK-kana-SU                       = #x04bd  ; U+30B9 KATAKANA LETTER SU 
XK-kana-SE                       = #x04be  ; U+30BB KATAKANA LETTER SE 
XK-kana-SO                       = #x04bf  ; U+30BD KATAKANA LETTER SO 
XK-kana-TA                       = #x04c0  ; U+30BF KATAKANA LETTER TA 
XK-kana-CHI                      = #x04c1  ; U+30C1 KATAKANA LETTER TI 
XK-kana-TI                       = #x04c1  ; deprecated 
XK-kana-TSU                      = #x04c2  ; U+30C4 KATAKANA LETTER TU 
XK-kana-TU                       = #x04c2  ; deprecated 
XK-kana-TE                       = #x04c3  ; U+30C6 KATAKANA LETTER TE 
XK-kana-TO                       = #x04c4  ; U+30C8 KATAKANA LETTER TO 
XK-kana-NA                       = #x04c5  ; U+30CA KATAKANA LETTER NA 
XK-kana-NI                       = #x04c6  ; U+30CB KATAKANA LETTER NI 
XK-kana-NU                       = #x04c7  ; U+30CC KATAKANA LETTER NU 
XK-kana-NE                       = #x04c8  ; U+30CD KATAKANA LETTER NE 
XK-kana-NO                       = #x04c9  ; U+30CE KATAKANA LETTER NO 
XK-kana-HA                       = #x04ca  ; U+30CF KATAKANA LETTER HA 
XK-kana-HI                       = #x04cb  ; U+30D2 KATAKANA LETTER HI 
XK-kana-FU                       = #x04cc  ; U+30D5 KATAKANA LETTER HU 
XK-kana-HU                       = #x04cc  ; deprecated 
XK-kana-HE                       = #x04cd  ; U+30D8 KATAKANA LETTER HE 
XK-kana-HO                       = #x04ce  ; U+30DB KATAKANA LETTER HO 
XK-kana-MA                       = #x04cf  ; U+30DE KATAKANA LETTER MA 
XK-kana-MI                       = #x04d0  ; U+30DF KATAKANA LETTER MI 
XK-kana-MU                       = #x04d1  ; U+30E0 KATAKANA LETTER MU 
XK-kana-ME                       = #x04d2  ; U+30E1 KATAKANA LETTER ME 
XK-kana-MO                       = #x04d3  ; U+30E2 KATAKANA LETTER MO 
XK-kana-YA                       = #x04d4  ; U+30E4 KATAKANA LETTER YA 
XK-kana-YU                       = #x04d5  ; U+30E6 KATAKANA LETTER YU 
XK-kana-YO                       = #x04d6  ; U+30E8 KATAKANA LETTER YO 
XK-kana-RA                       = #x04d7  ; U+30E9 KATAKANA LETTER RA 
XK-kana-RI                       = #x04d8  ; U+30EA KATAKANA LETTER RI 
XK-kana-RU                       = #x04d9  ; U+30EB KATAKANA LETTER RU 
XK-kana-RE                       = #x04da  ; U+30EC KATAKANA LETTER RE 
XK-kana-RO                       = #x04db  ; U+30ED KATAKANA LETTER RO 
XK-kana-WA                       = #x04dc  ; U+30EF KATAKANA LETTER WA 
XK-kana-N                        = #x04dd  ; U+30F3 KATAKANA LETTER N 
XK-voicedsound                   = #x04de  ; U+309B KATAKANA-HIRAGANA VOICED SOUND MARK 
XK-semivoicedsound               = #x04df  ; U+309C KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK 
XK-kana-switch                   = #xff7e  ; Alias for mode-switch 
;#endif ; XK-KATAKANA 

#|
 * Arabic
 * Byte 3 = 5
 |#

;#ifdef XK-ARABIC
XK-Farsi-0                    = #x10006f0  ; U+06F0 EXTENDED ARABIC-INDIC DIGIT ZERO 
XK-Farsi-1                    = #x10006f1  ; U+06F1 EXTENDED ARABIC-INDIC DIGIT ONE 
XK-Farsi-2                    = #x10006f2  ; U+06F2 EXTENDED ARABIC-INDIC DIGIT TWO 
XK-Farsi-3                    = #x10006f3  ; U+06F3 EXTENDED ARABIC-INDIC DIGIT THREE 
XK-Farsi-4                    = #x10006f4  ; U+06F4 EXTENDED ARABIC-INDIC DIGIT FOUR 
XK-Farsi-5                    = #x10006f5  ; U+06F5 EXTENDED ARABIC-INDIC DIGIT FIVE 
XK-Farsi-6                    = #x10006f6  ; U+06F6 EXTENDED ARABIC-INDIC DIGIT SIX 
XK-Farsi-7                    = #x10006f7  ; U+06F7 EXTENDED ARABIC-INDIC DIGIT SEVEN 
XK-Farsi-8                    = #x10006f8  ; U+06F8 EXTENDED ARABIC-INDIC DIGIT EIGHT 
XK-Farsi-9                    = #x10006f9  ; U+06F9 EXTENDED ARABIC-INDIC DIGIT NINE 
XK-Arabic-percent             = #x100066a  ; U+066A ARABIC PERCENT SIGN 
XK-Arabic-superscript-alef    = #x1000670  ; U+0670 ARABIC LETTER SUPERSCRIPT ALEF 
XK-Arabic-tteh                = #x1000679  ; U+0679 ARABIC LETTER TTEH 
XK-Arabic-peh                 = #x100067e  ; U+067E ARABIC LETTER PEH 
XK-Arabic-tcheh               = #x1000686  ; U+0686 ARABIC LETTER TCHEH 
XK-Arabic-ddal                = #x1000688  ; U+0688 ARABIC LETTER DDAL 
XK-Arabic-rreh                = #x1000691  ; U+0691 ARABIC LETTER RREH 
XK-Arabic-comma                  = #x05ac  ; U+060C ARABIC COMMA 
XK-Arabic-fullstop            = #x10006d4  ; U+06D4 ARABIC FULL STOP 
XK-Arabic-0                   = #x1000660  ; U+0660 ARABIC-INDIC DIGIT ZERO 
XK-Arabic-1                   = #x1000661  ; U+0661 ARABIC-INDIC DIGIT ONE 
XK-Arabic-2                   = #x1000662  ; U+0662 ARABIC-INDIC DIGIT TWO 
XK-Arabic-3                   = #x1000663  ; U+0663 ARABIC-INDIC DIGIT THREE 
XK-Arabic-4                   = #x1000664  ; U+0664 ARABIC-INDIC DIGIT FOUR 
XK-Arabic-5                   = #x1000665  ; U+0665 ARABIC-INDIC DIGIT FIVE 
XK-Arabic-6                   = #x1000666  ; U+0666 ARABIC-INDIC DIGIT SIX 
XK-Arabic-7                   = #x1000667  ; U+0667 ARABIC-INDIC DIGIT SEVEN 
XK-Arabic-8                   = #x1000668  ; U+0668 ARABIC-INDIC DIGIT EIGHT 
XK-Arabic-9                   = #x1000669  ; U+0669 ARABIC-INDIC DIGIT NINE 
XK-Arabic-semicolon              = #x05bb  ; U+061B ARABIC SEMICOLON 
XK-Arabic-question-mark          = #x05bf  ; U+061F ARABIC QUESTION MARK 
XK-Arabic-hamza                  = #x05c1  ; U+0621 ARABIC LETTER HAMZA 
XK-Arabic-maddaonalef            = #x05c2  ; U+0622 ARABIC LETTER ALEF WITH MADDA ABOVE 
XK-Arabic-hamzaonalef            = #x05c3  ; U+0623 ARABIC LETTER ALEF WITH HAMZA ABOVE 
XK-Arabic-hamzaonwaw             = #x05c4  ; U+0624 ARABIC LETTER WAW WITH HAMZA ABOVE 
XK-Arabic-hamzaunderalef         = #x05c5  ; U+0625 ARABIC LETTER ALEF WITH HAMZA BELOW 
XK-Arabic-hamzaonyeh             = #x05c6  ; U+0626 ARABIC LETTER YEH WITH HAMZA ABOVE 
XK-Arabic-alef                   = #x05c7  ; U+0627 ARABIC LETTER ALEF 
XK-Arabic-beh                    = #x05c8  ; U+0628 ARABIC LETTER BEH 
XK-Arabic-tehmarbuta             = #x05c9  ; U+0629 ARABIC LETTER TEH MARBUTA 
XK-Arabic-teh                    = #x05ca  ; U+062A ARABIC LETTER TEH 
XK-Arabic-theh                   = #x05cb  ; U+062B ARABIC LETTER THEH 
XK-Arabic-jeem                   = #x05cc  ; U+062C ARABIC LETTER JEEM 
XK-Arabic-hah                    = #x05cd  ; U+062D ARABIC LETTER HAH 
XK-Arabic-khah                   = #x05ce  ; U+062E ARABIC LETTER KHAH 
XK-Arabic-dal                    = #x05cf  ; U+062F ARABIC LETTER DAL 
XK-Arabic-thal                   = #x05d0  ; U+0630 ARABIC LETTER THAL 
XK-Arabic-ra                     = #x05d1  ; U+0631 ARABIC LETTER REH 
XK-Arabic-zain                   = #x05d2  ; U+0632 ARABIC LETTER ZAIN 
XK-Arabic-seen                   = #x05d3  ; U+0633 ARABIC LETTER SEEN 
XK-Arabic-sheen                  = #x05d4  ; U+0634 ARABIC LETTER SHEEN 
XK-Arabic-sad                    = #x05d5  ; U+0635 ARABIC LETTER SAD 
XK-Arabic-dad                    = #x05d6  ; U+0636 ARABIC LETTER DAD 
XK-Arabic-tah                    = #x05d7  ; U+0637 ARABIC LETTER TAH 
XK-Arabic-zah                    = #x05d8  ; U+0638 ARABIC LETTER ZAH 
XK-Arabic-ain                    = #x05d9  ; U+0639 ARABIC LETTER AIN 
XK-Arabic-ghain                  = #x05da  ; U+063A ARABIC LETTER GHAIN 
XK-Arabic-tatweel                = #x05e0  ; U+0640 ARABIC TATWEEL 
XK-Arabic-feh                    = #x05e1  ; U+0641 ARABIC LETTER FEH 
XK-Arabic-qaf                    = #x05e2  ; U+0642 ARABIC LETTER QAF 
XK-Arabic-kaf                    = #x05e3  ; U+0643 ARABIC LETTER KAF 
XK-Arabic-lam                    = #x05e4  ; U+0644 ARABIC LETTER LAM 
XK-Arabic-meem                   = #x05e5  ; U+0645 ARABIC LETTER MEEM 
XK-Arabic-noon                   = #x05e6  ; U+0646 ARABIC LETTER NOON 
XK-Arabic-ha                     = #x05e7  ; U+0647 ARABIC LETTER HEH 
XK-Arabic-heh                    = #x05e7  ; deprecated 
XK-Arabic-waw                    = #x05e8  ; U+0648 ARABIC LETTER WAW 
XK-Arabic-alefmaksura            = #x05e9  ; U+0649 ARABIC LETTER ALEF MAKSURA 
XK-Arabic-yeh                    = #x05ea  ; U+064A ARABIC LETTER YEH 
XK-Arabic-fathatan               = #x05eb  ; U+064B ARABIC FATHATAN 
XK-Arabic-dammatan               = #x05ec  ; U+064C ARABIC DAMMATAN 
XK-Arabic-kasratan               = #x05ed  ; U+064D ARABIC KASRATAN 
XK-Arabic-fatha                  = #x05ee  ; U+064E ARABIC FATHA 
XK-Arabic-damma                  = #x05ef  ; U+064F ARABIC DAMMA 
XK-Arabic-kasra                  = #x05f0  ; U+0650 ARABIC KASRA 
XK-Arabic-shadda                 = #x05f1  ; U+0651 ARABIC SHADDA 
XK-Arabic-sukun                  = #x05f2  ; U+0652 ARABIC SUKUN 
XK-Arabic-madda-above         = #x1000653  ; U+0653 ARABIC MADDAH ABOVE 
XK-Arabic-hamza-above         = #x1000654  ; U+0654 ARABIC HAMZA ABOVE 
XK-Arabic-hamza-below         = #x1000655  ; U+0655 ARABIC HAMZA BELOW 
XK-Arabic-jeh                 = #x1000698  ; U+0698 ARABIC LETTER JEH 
XK-Arabic-veh                 = #x10006a4  ; U+06A4 ARABIC LETTER VEH 
XK-Arabic-keheh               = #x10006a9  ; U+06A9 ARABIC LETTER KEHEH 
XK-Arabic-gaf                 = #x10006af  ; U+06AF ARABIC LETTER GAF 
XK-Arabic-noon-ghunna         = #x10006ba  ; U+06BA ARABIC LETTER NOON GHUNNA 
XK-Arabic-heh-doachashmee     = #x10006be  ; U+06BE ARABIC LETTER HEH DOACHASHMEE 
XK-Farsi-yeh                  = #x10006cc  ; U+06CC ARABIC LETTER FARSI YEH 
XK-Arabic-farsi-yeh           = #x10006cc  ; U+06CC ARABIC LETTER FARSI YEH 
XK-Arabic-yeh-baree           = #x10006d2  ; U+06D2 ARABIC LETTER YEH BARREE 
XK-Arabic-heh-goal            = #x10006c1  ; U+06C1 ARABIC LETTER HEH GOAL 
XK-Arabic-switch                 = #xff7e  ; Alias for mode-switch 
;#endif ; XK-ARABIC 

#|
 * Cyrillic
 * Byte 3 = 6
 |#
;#ifdef XK-CYRILLIC
XK-Cyrillic-GHE-bar           = #x1000492  ; U+0492 CYRILLIC CAPITAL LETTER GHE WITH STROKE 
XK-Cyrillic-ghe-bar           = #x1000493  ; U+0493 CYRILLIC SMALL LETTER GHE WITH STROKE 
XK-Cyrillic-ZHE-descender     = #x1000496  ; U+0496 CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER 
XK-Cyrillic-zhe-descender     = #x1000497  ; U+0497 CYRILLIC SMALL LETTER ZHE WITH DESCENDER 
XK-Cyrillic-KA-descender      = #x100049a  ; U+049A CYRILLIC CAPITAL LETTER KA WITH DESCENDER 
XK-Cyrillic-ka-descender      = #x100049b  ; U+049B CYRILLIC SMALL LETTER KA WITH DESCENDER 
XK-Cyrillic-KA-vertstroke     = #x100049c  ; U+049C CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE 
XK-Cyrillic-ka-vertstroke     = #x100049d  ; U+049D CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE 
XK-Cyrillic-EN-descender      = #x10004a2  ; U+04A2 CYRILLIC CAPITAL LETTER EN WITH DESCENDER 
XK-Cyrillic-en-descender      = #x10004a3  ; U+04A3 CYRILLIC SMALL LETTER EN WITH DESCENDER 
XK-Cyrillic-U-straight        = #x10004ae  ; U+04AE CYRILLIC CAPITAL LETTER STRAIGHT U 
XK-Cyrillic-u-straight        = #x10004af  ; U+04AF CYRILLIC SMALL LETTER STRAIGHT U 
XK-Cyrillic-U-straight-bar    = #x10004b0  ; U+04B0 CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE 
XK-Cyrillic-u-straight-bar    = #x10004b1  ; U+04B1 CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE 
XK-Cyrillic-HA-descender      = #x10004b2  ; U+04B2 CYRILLIC CAPITAL LETTER HA WITH DESCENDER 
XK-Cyrillic-ha-descender      = #x10004b3  ; U+04B3 CYRILLIC SMALL LETTER HA WITH DESCENDER 
XK-Cyrillic-CHE-descender     = #x10004b6  ; U+04B6 CYRILLIC CAPITAL LETTER CHE WITH DESCENDER 
XK-Cyrillic-che-descender     = #x10004b7  ; U+04B7 CYRILLIC SMALL LETTER CHE WITH DESCENDER 
XK-Cyrillic-CHE-vertstroke    = #x10004b8  ; U+04B8 CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE 
XK-Cyrillic-che-vertstroke    = #x10004b9  ; U+04B9 CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE 
XK-Cyrillic-SHHA              = #x10004ba  ; U+04BA CYRILLIC CAPITAL LETTER SHHA 
XK-Cyrillic-shha              = #x10004bb  ; U+04BB CYRILLIC SMALL LETTER SHHA 

XK-Cyrillic-SCHWA             = #x10004d8  ; U+04D8 CYRILLIC CAPITAL LETTER SCHWA 
XK-Cyrillic-schwa             = #x10004d9  ; U+04D9 CYRILLIC SMALL LETTER SCHWA 
XK-Cyrillic-I-macron          = #x10004e2  ; U+04E2 CYRILLIC CAPITAL LETTER I WITH MACRON 
XK-Cyrillic-i-macron          = #x10004e3  ; U+04E3 CYRILLIC SMALL LETTER I WITH MACRON 
XK-Cyrillic-O-bar             = #x10004e8  ; U+04E8 CYRILLIC CAPITAL LETTER BARRED O 
XK-Cyrillic-o-bar             = #x10004e9  ; U+04E9 CYRILLIC SMALL LETTER BARRED O 
XK-Cyrillic-U-macron          = #x10004ee  ; U+04EE CYRILLIC CAPITAL LETTER U WITH MACRON 
XK-Cyrillic-u-macron          = #x10004ef  ; U+04EF CYRILLIC SMALL LETTER U WITH MACRON 

XK-Serbian-dje                   = #x06a1  ; U+0452 CYRILLIC SMALL LETTER DJE 
XK-Macedonia-gje                 = #x06a2  ; U+0453 CYRILLIC SMALL LETTER GJE 
XK-Cyrillic-io                   = #x06a3  ; U+0451 CYRILLIC SMALL LETTER IO 
XK-Ukrainian-ie                  = #x06a4  ; U+0454 CYRILLIC SMALL LETTER UKRAINIAN IE 
XK-Ukranian-je                   = #x06a4  ; deprecated 
XK-Macedonia-dse                 = #x06a5  ; U+0455 CYRILLIC SMALL LETTER DZE 
XK-Ukrainian-i                   = #x06a6  ; U+0456 CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I 
XK-Ukranian-i                    = #x06a6  ; deprecated 
XK-Ukrainian-yi                  = #x06a7  ; U+0457 CYRILLIC SMALL LETTER YI 
XK-Ukranian-yi                   = #x06a7  ; deprecated 
XK-Cyrillic-je                   = #x06a8  ; U+0458 CYRILLIC SMALL LETTER JE 
XK-Serbian-je                    = #x06a8  ; deprecated 
XK-Cyrillic-lje                  = #x06a9  ; U+0459 CYRILLIC SMALL LETTER LJE 
XK-Serbian-lje                   = #x06a9  ; deprecated 
XK-Cyrillic-nje                  = #x06aa  ; U+045A CYRILLIC SMALL LETTER NJE 
XK-Serbian-nje                   = #x06aa  ; deprecated 
XK-Serbian-tshe                  = #x06ab  ; U+045B CYRILLIC SMALL LETTER TSHE 
XK-Macedonia-kje                 = #x06ac  ; U+045C CYRILLIC SMALL LETTER KJE 
XK-Ukrainian-ghe-with-upturn     = #x06ad  ; U+0491 CYRILLIC SMALL LETTER GHE WITH UPTURN 
XK-Byelorussian-shortu           = #x06ae  ; U+045E CYRILLIC SMALL LETTER SHORT U 
XK-Cyrillic-dzhe                 = #x06af  ; U+045F CYRILLIC SMALL LETTER DZHE 
XK-Serbian-dze                   = #x06af  ; deprecated 
XK-numerosign                    = #x06b0  ; U+2116 NUMERO SIGN 
XK-Serbian-DJE                   = #x06b1  ; U+0402 CYRILLIC CAPITAL LETTER DJE 
XK-Macedonia-GJE                 = #x06b2  ; U+0403 CYRILLIC CAPITAL LETTER GJE 
XK-Cyrillic-IO                   = #x06b3  ; U+0401 CYRILLIC CAPITAL LETTER IO 
XK-Ukrainian-IE                  = #x06b4  ; U+0404 CYRILLIC CAPITAL LETTER UKRAINIAN IE 
XK-Ukranian-JE                   = #x06b4  ; deprecated 
XK-Macedonia-DSE                 = #x06b5  ; U+0405 CYRILLIC CAPITAL LETTER DZE 
XK-Ukrainian-I                   = #x06b6  ; U+0406 CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I 
XK-Ukranian-I                    = #x06b6  ; deprecated 
XK-Ukrainian-YI                  = #x06b7  ; U+0407 CYRILLIC CAPITAL LETTER YI 
XK-Ukranian-YI                   = #x06b7  ; deprecated 
XK-Cyrillic-JE                   = #x06b8  ; U+0408 CYRILLIC CAPITAL LETTER JE 
XK-Serbian-JE                    = #x06b8  ; deprecated 
XK-Cyrillic-LJE                  = #x06b9  ; U+0409 CYRILLIC CAPITAL LETTER LJE 
XK-Serbian-LJE                   = #x06b9  ; deprecated 
XK-Cyrillic-NJE                  = #x06ba  ; U+040A CYRILLIC CAPITAL LETTER NJE 
XK-Serbian-NJE                   = #x06ba  ; deprecated 
XK-Serbian-TSHE                  = #x06bb  ; U+040B CYRILLIC CAPITAL LETTER TSHE 
XK-Macedonia-KJE                 = #x06bc  ; U+040C CYRILLIC CAPITAL LETTER KJE 
XK-Ukrainian-GHE-WITH-UPTURN     = #x06bd  ; U+0490 CYRILLIC CAPITAL LETTER GHE WITH UPTURN 
XK-Byelorussian-SHORTU           = #x06be  ; U+040E CYRILLIC CAPITAL LETTER SHORT U 
XK-Cyrillic-DZHE                 = #x06bf  ; U+040F CYRILLIC CAPITAL LETTER DZHE 
XK-Serbian-DZE                   = #x06bf  ; deprecated 
XK-Cyrillic-yu                   = #x06c0  ; U+044E CYRILLIC SMALL LETTER YU 
XK-Cyrillic-a                    = #x06c1  ; U+0430 CYRILLIC SMALL LETTER A 
XK-Cyrillic-be                   = #x06c2  ; U+0431 CYRILLIC SMALL LETTER BE 
XK-Cyrillic-tse                  = #x06c3  ; U+0446 CYRILLIC SMALL LETTER TSE 
XK-Cyrillic-de                   = #x06c4  ; U+0434 CYRILLIC SMALL LETTER DE 
XK-Cyrillic-ie                   = #x06c5  ; U+0435 CYRILLIC SMALL LETTER IE 
XK-Cyrillic-ef                   = #x06c6  ; U+0444 CYRILLIC SMALL LETTER EF 
XK-Cyrillic-ghe                  = #x06c7  ; U+0433 CYRILLIC SMALL LETTER GHE 
XK-Cyrillic-ha                   = #x06c8  ; U+0445 CYRILLIC SMALL LETTER HA 
XK-Cyrillic-i                    = #x06c9  ; U+0438 CYRILLIC SMALL LETTER I 
XK-Cyrillic-shorti               = #x06ca  ; U+0439 CYRILLIC SMALL LETTER SHORT I 
XK-Cyrillic-ka                   = #x06cb  ; U+043A CYRILLIC SMALL LETTER KA 
XK-Cyrillic-el                   = #x06cc  ; U+043B CYRILLIC SMALL LETTER EL 
XK-Cyrillic-em                   = #x06cd  ; U+043C CYRILLIC SMALL LETTER EM 
XK-Cyrillic-en                   = #x06ce  ; U+043D CYRILLIC SMALL LETTER EN 
XK-Cyrillic-o                    = #x06cf  ; U+043E CYRILLIC SMALL LETTER O 
XK-Cyrillic-pe                   = #x06d0  ; U+043F CYRILLIC SMALL LETTER PE 
XK-Cyrillic-ya                   = #x06d1  ; U+044F CYRILLIC SMALL LETTER YA 
XK-Cyrillic-er                   = #x06d2  ; U+0440 CYRILLIC SMALL LETTER ER 
XK-Cyrillic-es                   = #x06d3  ; U+0441 CYRILLIC SMALL LETTER ES 
XK-Cyrillic-te                   = #x06d4  ; U+0442 CYRILLIC SMALL LETTER TE 
XK-Cyrillic-u                    = #x06d5  ; U+0443 CYRILLIC SMALL LETTER U 
XK-Cyrillic-zhe                  = #x06d6  ; U+0436 CYRILLIC SMALL LETTER ZHE 
XK-Cyrillic-ve                   = #x06d7  ; U+0432 CYRILLIC SMALL LETTER VE 
XK-Cyrillic-softsign             = #x06d8  ; U+044C CYRILLIC SMALL LETTER SOFT SIGN 
XK-Cyrillic-yeru                 = #x06d9  ; U+044B CYRILLIC SMALL LETTER YERU 
XK-Cyrillic-ze                   = #x06da  ; U+0437 CYRILLIC SMALL LETTER ZE 
XK-Cyrillic-sha                  = #x06db  ; U+0448 CYRILLIC SMALL LETTER SHA 
XK-Cyrillic-e                    = #x06dc  ; U+044D CYRILLIC SMALL LETTER E 
XK-Cyrillic-shcha                = #x06dd  ; U+0449 CYRILLIC SMALL LETTER SHCHA 
XK-Cyrillic-che                  = #x06de  ; U+0447 CYRILLIC SMALL LETTER CHE 
XK-Cyrillic-hardsign             = #x06df  ; U+044A CYRILLIC SMALL LETTER HARD SIGN 
XK-Cyrillic-YU                   = #x06e0  ; U+042E CYRILLIC CAPITAL LETTER YU 
XK-Cyrillic-A                    = #x06e1  ; U+0410 CYRILLIC CAPITAL LETTER A 
XK-Cyrillic-BE                   = #x06e2  ; U+0411 CYRILLIC CAPITAL LETTER BE 
XK-Cyrillic-TSE                  = #x06e3  ; U+0426 CYRILLIC CAPITAL LETTER TSE 
XK-Cyrillic-DE                   = #x06e4  ; U+0414 CYRILLIC CAPITAL LETTER DE 
XK-Cyrillic-IE                   = #x06e5  ; U+0415 CYRILLIC CAPITAL LETTER IE 
XK-Cyrillic-EF                   = #x06e6  ; U+0424 CYRILLIC CAPITAL LETTER EF 
XK-Cyrillic-GHE                  = #x06e7  ; U+0413 CYRILLIC CAPITAL LETTER GHE 
XK-Cyrillic-HA                   = #x06e8  ; U+0425 CYRILLIC CAPITAL LETTER HA 
XK-Cyrillic-I                    = #x06e9  ; U+0418 CYRILLIC CAPITAL LETTER I 
XK-Cyrillic-SHORTI               = #x06ea  ; U+0419 CYRILLIC CAPITAL LETTER SHORT I 
XK-Cyrillic-KA                   = #x06eb  ; U+041A CYRILLIC CAPITAL LETTER KA 
XK-Cyrillic-EL                   = #x06ec  ; U+041B CYRILLIC CAPITAL LETTER EL 
XK-Cyrillic-EM                   = #x06ed  ; U+041C CYRILLIC CAPITAL LETTER EM 
XK-Cyrillic-EN                   = #x06ee  ; U+041D CYRILLIC CAPITAL LETTER EN 
XK-Cyrillic-O                    = #x06ef  ; U+041E CYRILLIC CAPITAL LETTER O 
XK-Cyrillic-PE                   = #x06f0  ; U+041F CYRILLIC CAPITAL LETTER PE 
XK-Cyrillic-YA                   = #x06f1  ; U+042F CYRILLIC CAPITAL LETTER YA 
XK-Cyrillic-ER                   = #x06f2  ; U+0420 CYRILLIC CAPITAL LETTER ER 
XK-Cyrillic-ES                   = #x06f3  ; U+0421 CYRILLIC CAPITAL LETTER ES 
XK-Cyrillic-TE                   = #x06f4  ; U+0422 CYRILLIC CAPITAL LETTER TE 
XK-Cyrillic-U                    = #x06f5  ; U+0423 CYRILLIC CAPITAL LETTER U 
XK-Cyrillic-ZHE                  = #x06f6  ; U+0416 CYRILLIC CAPITAL LETTER ZHE 
XK-Cyrillic-VE                   = #x06f7  ; U+0412 CYRILLIC CAPITAL LETTER VE 
XK-Cyrillic-SOFTSIGN             = #x06f8  ; U+042C CYRILLIC CAPITAL LETTER SOFT SIGN 
XK-Cyrillic-YERU                 = #x06f9  ; U+042B CYRILLIC CAPITAL LETTER YERU 
XK-Cyrillic-ZE                   = #x06fa  ; U+0417 CYRILLIC CAPITAL LETTER ZE 
XK-Cyrillic-SHA                  = #x06fb  ; U+0428 CYRILLIC CAPITAL LETTER SHA 
XK-Cyrillic-E                    = #x06fc  ; U+042D CYRILLIC CAPITAL LETTER E 
XK-Cyrillic-SHCHA                = #x06fd  ; U+0429 CYRILLIC CAPITAL LETTER SHCHA 
XK-Cyrillic-CHE                  = #x06fe  ; U+0427 CYRILLIC CAPITAL LETTER CHE 
XK-Cyrillic-HARDSIGN             = #x06ff  ; U+042A CYRILLIC CAPITAL LETTER HARD SIGN 
;#endif ; XK-CYRILLIC 

#|
 * Greek
 * (based on an early draft of, and not quite identical to, ISO/IEC 8859-7)
 * Byte 3 = 7
 |#

;#ifdef XK-GREEK
XK-Greek-ALPHAaccent             = #x07a1  ; U+0386 GREEK CAPITAL LETTER ALPHA WITH TONOS 
XK-Greek-EPSILONaccent           = #x07a2  ; U+0388 GREEK CAPITAL LETTER EPSILON WITH TONOS 
XK-Greek-ETAaccent               = #x07a3  ; U+0389 GREEK CAPITAL LETTER ETA WITH TONOS 
XK-Greek-IOTAaccent              = #x07a4  ; U+038A GREEK CAPITAL LETTER IOTA WITH TONOS 
XK-Greek-IOTAdieresis            = #x07a5  ; U+03AA GREEK CAPITAL LETTER IOTA WITH DIALYTIKA 
XK-Greek-IOTAdiaeresis           = #x07a5  ; old typo 
XK-Greek-OMICRONaccent           = #x07a7  ; U+038C GREEK CAPITAL LETTER OMICRON WITH TONOS 
XK-Greek-UPSILONaccent           = #x07a8  ; U+038E GREEK CAPITAL LETTER UPSILON WITH TONOS 
XK-Greek-UPSILONdieresis         = #x07a9  ; U+03AB GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA 
XK-Greek-OMEGAaccent             = #x07ab  ; U+038F GREEK CAPITAL LETTER OMEGA WITH TONOS 
XK-Greek-accentdieresis          = #x07ae  ; U+0385 GREEK DIALYTIKA TONOS 
XK-Greek-horizbar                = #x07af  ; U+2015 HORIZONTAL BAR 
XK-Greek-alphaaccent             = #x07b1  ; U+03AC GREEK SMALL LETTER ALPHA WITH TONOS 
XK-Greek-epsilonaccent           = #x07b2  ; U+03AD GREEK SMALL LETTER EPSILON WITH TONOS 
XK-Greek-etaaccent               = #x07b3  ; U+03AE GREEK SMALL LETTER ETA WITH TONOS 
XK-Greek-iotaaccent              = #x07b4  ; U+03AF GREEK SMALL LETTER IOTA WITH TONOS 
XK-Greek-iotadieresis            = #x07b5  ; U+03CA GREEK SMALL LETTER IOTA WITH DIALYTIKA 
XK-Greek-iotaaccentdieresis      = #x07b6  ; U+0390 GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS 
XK-Greek-omicronaccent           = #x07b7  ; U+03CC GREEK SMALL LETTER OMICRON WITH TONOS 
XK-Greek-upsilonaccent           = #x07b8  ; U+03CD GREEK SMALL LETTER UPSILON WITH TONOS 
XK-Greek-upsilondieresis         = #x07b9  ; U+03CB GREEK SMALL LETTER UPSILON WITH DIALYTIKA 
XK-Greek-upsilonaccentdieresis   = #x07ba  ; U+03B0 GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS 
XK-Greek-omegaaccent             = #x07bb  ; U+03CE GREEK SMALL LETTER OMEGA WITH TONOS 
XK-Greek-ALPHA                   = #x07c1  ; U+0391 GREEK CAPITAL LETTER ALPHA 
XK-Greek-BETA                    = #x07c2  ; U+0392 GREEK CAPITAL LETTER BETA 
XK-Greek-GAMMA                   = #x07c3  ; U+0393 GREEK CAPITAL LETTER GAMMA 
XK-Greek-DELTA                   = #x07c4  ; U+0394 GREEK CAPITAL LETTER DELTA 
XK-Greek-EPSILON                 = #x07c5  ; U+0395 GREEK CAPITAL LETTER EPSILON 
XK-Greek-ZETA                    = #x07c6  ; U+0396 GREEK CAPITAL LETTER ZETA 
XK-Greek-ETA                     = #x07c7  ; U+0397 GREEK CAPITAL LETTER ETA 
XK-Greek-THETA                   = #x07c8  ; U+0398 GREEK CAPITAL LETTER THETA 
XK-Greek-IOTA                    = #x07c9  ; U+0399 GREEK CAPITAL LETTER IOTA 
XK-Greek-KAPPA                   = #x07ca  ; U+039A GREEK CAPITAL LETTER KAPPA 
XK-Greek-LAMDA                   = #x07cb  ; U+039B GREEK CAPITAL LETTER LAMDA 
XK-Greek-LAMBDA                  = #x07cb  ; U+039B GREEK CAPITAL LETTER LAMDA 
XK-Greek-MU                      = #x07cc  ; U+039C GREEK CAPITAL LETTER MU 
XK-Greek-NU                      = #x07cd  ; U+039D GREEK CAPITAL LETTER NU 
XK-Greek-XI                      = #x07ce  ; U+039E GREEK CAPITAL LETTER XI 
XK-Greek-OMICRON                 = #x07cf  ; U+039F GREEK CAPITAL LETTER OMICRON 
XK-Greek-PI                      = #x07d0  ; U+03A0 GREEK CAPITAL LETTER PI 
XK-Greek-RHO                     = #x07d1  ; U+03A1 GREEK CAPITAL LETTER RHO 
XK-Greek-SIGMA                   = #x07d2  ; U+03A3 GREEK CAPITAL LETTER SIGMA 
XK-Greek-TAU                     = #x07d4  ; U+03A4 GREEK CAPITAL LETTER TAU 
XK-Greek-UPSILON                 = #x07d5  ; U+03A5 GREEK CAPITAL LETTER UPSILON 
XK-Greek-PHI                     = #x07d6  ; U+03A6 GREEK CAPITAL LETTER PHI 
XK-Greek-CHI                     = #x07d7  ; U+03A7 GREEK CAPITAL LETTER CHI 
XK-Greek-PSI                     = #x07d8  ; U+03A8 GREEK CAPITAL LETTER PSI 
XK-Greek-OMEGA                   = #x07d9  ; U+03A9 GREEK CAPITAL LETTER OMEGA 
XK-Greek-alpha                   = #x07e1  ; U+03B1 GREEK SMALL LETTER ALPHA 
XK-Greek-beta                    = #x07e2  ; U+03B2 GREEK SMALL LETTER BETA 
XK-Greek-gamma                   = #x07e3  ; U+03B3 GREEK SMALL LETTER GAMMA 
XK-Greek-delta                   = #x07e4  ; U+03B4 GREEK SMALL LETTER DELTA 
XK-Greek-epsilon                 = #x07e5  ; U+03B5 GREEK SMALL LETTER EPSILON 
XK-Greek-zeta                    = #x07e6  ; U+03B6 GREEK SMALL LETTER ZETA 
XK-Greek-eta                     = #x07e7  ; U+03B7 GREEK SMALL LETTER ETA 
XK-Greek-theta                   = #x07e8  ; U+03B8 GREEK SMALL LETTER THETA 
XK-Greek-iota                    = #x07e9  ; U+03B9 GREEK SMALL LETTER IOTA 
XK-Greek-kappa                   = #x07ea  ; U+03BA GREEK SMALL LETTER KAPPA 
XK-Greek-lamda                   = #x07eb  ; U+03BB GREEK SMALL LETTER LAMDA 
XK-Greek-lambda                  = #x07eb  ; U+03BB GREEK SMALL LETTER LAMDA 
XK-Greek-mu                      = #x07ec  ; U+03BC GREEK SMALL LETTER MU 
XK-Greek-nu                      = #x07ed  ; U+03BD GREEK SMALL LETTER NU 
XK-Greek-xi                      = #x07ee  ; U+03BE GREEK SMALL LETTER XI 
XK-Greek-omicron                 = #x07ef  ; U+03BF GREEK SMALL LETTER OMICRON 
XK-Greek-pi                      = #x07f0  ; U+03C0 GREEK SMALL LETTER PI 
XK-Greek-rho                     = #x07f1  ; U+03C1 GREEK SMALL LETTER RHO 
XK-Greek-sigma                   = #x07f2  ; U+03C3 GREEK SMALL LETTER SIGMA 
XK-Greek-finalsmallsigma         = #x07f3  ; U+03C2 GREEK SMALL LETTER FINAL SIGMA 
XK-Greek-tau                     = #x07f4  ; U+03C4 GREEK SMALL LETTER TAU 
XK-Greek-upsilon                 = #x07f5  ; U+03C5 GREEK SMALL LETTER UPSILON 
XK-Greek-phi                     = #x07f6  ; U+03C6 GREEK SMALL LETTER PHI 
XK-Greek-chi                     = #x07f7  ; U+03C7 GREEK SMALL LETTER CHI 
XK-Greek-psi                     = #x07f8  ; U+03C8 GREEK SMALL LETTER PSI 
XK-Greek-omega                   = #x07f9  ; U+03C9 GREEK SMALL LETTER OMEGA 
XK-Greek-switch                  = #xff7e  ; Alias for mode-switch 
;#endif ; XK-GREEK 

#|
 * Technical
 * (from the DEC VT330/VT420 Technical Character Set, http://vt100.net/charsets/technical.html)
 * Byte 3 = 8
 |#

;#ifdef XK-TECHNICAL
XK-leftradical                   = #x08a1  ; U+23B7 RADICAL SYMBOL BOTTOM 
XK-topleftradical                = #x08a2  ;(U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT)
XK-horizconnector                = #x08a3  ;(U+2500 BOX DRAWINGS LIGHT HORIZONTAL)
XK-topintegral                   = #x08a4  ; U+2320 TOP HALF INTEGRAL 
XK-botintegral                   = #x08a5  ; U+2321 BOTTOM HALF INTEGRAL 
XK-vertconnector                 = #x08a6  ;(U+2502 BOX DRAWINGS LIGHT VERTICAL)
XK-topleftsqbracket              = #x08a7  ; U+23A1 LEFT SQUARE BRACKET UPPER CORNER 
XK-botleftsqbracket              = #x08a8  ; U+23A3 LEFT SQUARE BRACKET LOWER CORNER 
XK-toprightsqbracket             = #x08a9  ; U+23A4 RIGHT SQUARE BRACKET UPPER CORNER 
XK-botrightsqbracket             = #x08aa  ; U+23A6 RIGHT SQUARE BRACKET LOWER CORNER 
XK-topleftparens                 = #x08ab  ; U+239B LEFT PARENTHESIS UPPER HOOK 
XK-botleftparens                 = #x08ac  ; U+239D LEFT PARENTHESIS LOWER HOOK 
XK-toprightparens                = #x08ad  ; U+239E RIGHT PARENTHESIS UPPER HOOK 
XK-botrightparens                = #x08ae  ; U+23A0 RIGHT PARENTHESIS LOWER HOOK 
XK-leftmiddlecurlybrace          = #x08af  ; U+23A8 LEFT CURLY BRACKET MIDDLE PIECE 
XK-rightmiddlecurlybrace         = #x08b0  ; U+23AC RIGHT CURLY BRACKET MIDDLE PIECE 
XK-topleftsummation              = #x08b1
XK-botleftsummation              = #x08b2
XK-topvertsummationconnector     = #x08b3
XK-botvertsummationconnector     = #x08b4
XK-toprightsummation             = #x08b5
XK-botrightsummation             = #x08b6
XK-rightmiddlesummation          = #x08b7
XK-lessthanequal                 = #x08bc  ; U+2264 LESS-THAN OR EQUAL TO 
XK-notequal                      = #x08bd  ; U+2260 NOT EQUAL TO 
XK-greaterthanequal              = #x08be  ; U+2265 GREATER-THAN OR EQUAL TO 
XK-integral                      = #x08bf  ; U+222B INTEGRAL 
XK-therefore                     = #x08c0  ; U+2234 THEREFORE 
XK-variation                     = #x08c1  ; U+221D PROPORTIONAL TO 
XK-infinity                      = #x08c2  ; U+221E INFINITY 
XK-nabla                         = #x08c5  ; U+2207 NABLA 
XK-approximate                   = #x08c8  ; U+223C TILDE OPERATOR 
XK-similarequal                  = #x08c9  ; U+2243 ASYMPTOTICALLY EQUAL TO 
XK-ifonlyif                      = #x08cd  ; U+21D4 LEFT RIGHT DOUBLE ARROW 
XK-implies                       = #x08ce  ; U+21D2 RIGHTWARDS DOUBLE ARROW 
XK-identical                     = #x08cf  ; U+2261 IDENTICAL TO 
XK-radical                       = #x08d6  ; U+221A SQUARE ROOT 
XK-includedin                    = #x08da  ; U+2282 SUBSET OF 
XK-includes                      = #x08db  ; U+2283 SUPERSET OF 
XK-intersection                  = #x08dc  ; U+2229 INTERSECTION 
XK-union                         = #x08dd  ; U+222A UNION 
XK-logicaland                    = #x08de  ; U+2227 LOGICAL AND 
XK-logicalor                     = #x08df  ; U+2228 LOGICAL OR 
XK-partialderivative             = #x08ef  ; U+2202 PARTIAL DIFFERENTIAL 
XK-function                      = #x08f6  ; U+0192 LATIN SMALL LETTER F WITH HOOK 
XK-leftarrow                     = #x08fb  ; U+2190 LEFTWARDS ARROW 
XK-uparrow                       = #x08fc  ; U+2191 UPWARDS ARROW 
XK-rightarrow                    = #x08fd  ; U+2192 RIGHTWARDS ARROW 
XK-downarrow                     = #x08fe  ; U+2193 DOWNWARDS ARROW 
;#endif ; XK-TECHNICAL 

#|
 * Special
 * (from the DEC VT100 Special Graphics Character Set)
 * Byte 3 = 9
 |#

;#ifdef XK-SPECIAL
XK-blank                         = #x09df
XK-soliddiamond                  = #x09e0  ; U+25C6 BLACK DIAMOND 
XK-checkerboard                  = #x09e1  ; U+2592 MEDIUM SHADE 
XK-ht                            = #x09e2  ; U+2409 SYMBOL FOR HORIZONTAL TABULATION 
XK-ff                            = #x09e3  ; U+240C SYMBOL FOR FORM FEED 
XK-cr                            = #x09e4  ; U+240D SYMBOL FOR CARRIAGE RETURN 
XK-lf                            = #x09e5  ; U+240A SYMBOL FOR LINE FEED 
XK-nl                            = #x09e8  ; U+2424 SYMBOL FOR NEWLINE 
XK-vt                            = #x09e9  ; U+240B SYMBOL FOR VERTICAL TABULATION 
XK-lowrightcorner                = #x09ea  ; U+2518 BOX DRAWINGS LIGHT UP AND LEFT 
XK-uprightcorner                 = #x09eb  ; U+2510 BOX DRAWINGS LIGHT DOWN AND LEFT 
XK-upleftcorner                  = #x09ec  ; U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT 
XK-lowleftcorner                 = #x09ed  ; U+2514 BOX DRAWINGS LIGHT UP AND RIGHT 
XK-crossinglines                 = #x09ee  ; U+253C BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL 
XK-horizlinescan1                = #x09ef  ; U+23BA HORIZONTAL SCAN LINE-1 
XK-horizlinescan3                = #x09f0  ; U+23BB HORIZONTAL SCAN LINE-3 
XK-horizlinescan5                = #x09f1  ; U+2500 BOX DRAWINGS LIGHT HORIZONTAL 
XK-horizlinescan7                = #x09f2  ; U+23BC HORIZONTAL SCAN LINE-7 
XK-horizlinescan9                = #x09f3  ; U+23BD HORIZONTAL SCAN LINE-9 
XK-leftt                         = #x09f4  ; U+251C BOX DRAWINGS LIGHT VERTICAL AND RIGHT 
XK-rightt                        = #x09f5  ; U+2524 BOX DRAWINGS LIGHT VERTICAL AND LEFT 
XK-bott                          = #x09f6  ; U+2534 BOX DRAWINGS LIGHT UP AND HORIZONTAL 
XK-topt                          = #x09f7  ; U+252C BOX DRAWINGS LIGHT DOWN AND HORIZONTAL 
XK-vertbar                       = #x09f8  ; U+2502 BOX DRAWINGS LIGHT VERTICAL 
;#endif ; XK-SPECIAL 

#|
 * Publishing
 * (these are probably from a long forgotten DEC Publishing
 * font that once shipped with DECwrite)
 * Byte 3 = 0x0a
 |#

;#ifdef XK-PUBLISHING
XK-emspace                       = #x0aa1  ; U+2003 EM SPACE 
XK-enspace                       = #x0aa2  ; U+2002 EN SPACE 
XK-em3space                      = #x0aa3  ; U+2004 THREE-PER-EM SPACE 
XK-em4space                      = #x0aa4  ; U+2005 FOUR-PER-EM SPACE 
XK-digitspace                    = #x0aa5  ; U+2007 FIGURE SPACE 
XK-punctspace                    = #x0aa6  ; U+2008 PUNCTUATION SPACE 
XK-thinspace                     = #x0aa7  ; U+2009 THIN SPACE 
XK-hairspace                     = #x0aa8  ; U+200A HAIR SPACE 
XK-emdash                        = #x0aa9  ; U+2014 EM DASH 
XK-endash                        = #x0aaa  ; U+2013 EN DASH 
XK-signifblank                   = #x0aac  ;(U+2423 OPEN BOX)
XK-ellipsis                      = #x0aae  ; U+2026 HORIZONTAL ELLIPSIS 
XK-doubbaselinedot               = #x0aaf  ; U+2025 TWO DOT LEADER 
XK-onethird                      = #x0ab0  ; U+2153 VULGAR FRACTION ONE THIRD 
XK-twothirds                     = #x0ab1  ; U+2154 VULGAR FRACTION TWO THIRDS 
XK-onefifth                      = #x0ab2  ; U+2155 VULGAR FRACTION ONE FIFTH 
XK-twofifths                     = #x0ab3  ; U+2156 VULGAR FRACTION TWO FIFTHS 
XK-threefifths                   = #x0ab4  ; U+2157 VULGAR FRACTION THREE FIFTHS 
XK-fourfifths                    = #x0ab5  ; U+2158 VULGAR FRACTION FOUR FIFTHS 
XK-onesixth                      = #x0ab6  ; U+2159 VULGAR FRACTION ONE SIXTH 
XK-fivesixths                    = #x0ab7  ; U+215A VULGAR FRACTION FIVE SIXTHS 
XK-careof                        = #x0ab8  ; U+2105 CARE OF 
XK-figdash                       = #x0abb  ; U+2012 FIGURE DASH 
XK-leftanglebracket              = #x0abc  ;(U+27E8 MATHEMATICAL LEFT ANGLE BRACKET)
XK-decimalpoint                  = #x0abd  ;(U+002E FULL STOP)
XK-rightanglebracket             = #x0abe  ;(U+27E9 MATHEMATICAL RIGHT ANGLE BRACKET)
XK-marker                        = #x0abf
XK-oneeighth                     = #x0ac3  ; U+215B VULGAR FRACTION ONE EIGHTH 
XK-threeeighths                  = #x0ac4  ; U+215C VULGAR FRACTION THREE EIGHTHS 
XK-fiveeighths                   = #x0ac5  ; U+215D VULGAR FRACTION FIVE EIGHTHS 
XK-seveneighths                  = #x0ac6  ; U+215E VULGAR FRACTION SEVEN EIGHTHS 
XK-trademark                     = #x0ac9  ; U+2122 TRADE MARK SIGN 
XK-signaturemark                 = #x0aca  ;(U+2613 SALTIRE)
XK-trademarkincircle             = #x0acb
XK-leftopentriangle              = #x0acc  ;(U+25C1 WHITE LEFT-POINTING TRIANGLE)
XK-rightopentriangle             = #x0acd  ;(U+25B7 WHITE RIGHT-POINTING TRIANGLE)
XK-emopencircle                  = #x0ace  ;(U+25CB WHITE CIRCLE)
XK-emopenrectangle               = #x0acf  ;(U+25AF WHITE VERTICAL RECTANGLE)
XK-leftsinglequotemark           = #x0ad0  ; U+2018 LEFT SINGLE QUOTATION MARK 
XK-rightsinglequotemark          = #x0ad1  ; U+2019 RIGHT SINGLE QUOTATION MARK 
XK-leftdoublequotemark           = #x0ad2  ; U+201C LEFT DOUBLE QUOTATION MARK 
XK-rightdoublequotemark          = #x0ad3  ; U+201D RIGHT DOUBLE QUOTATION MARK 
XK-prescription                  = #x0ad4  ; U+211E PRESCRIPTION TAKE 
XK-minutes                       = #x0ad6  ; U+2032 PRIME 
XK-seconds                       = #x0ad7  ; U+2033 DOUBLE PRIME 
XK-latincross                    = #x0ad9  ; U+271D LATIN CROSS 
XK-hexagram                      = #x0ada
XK-filledrectbullet              = #x0adb  ;(U+25AC BLACK RECTANGLE)
XK-filledlefttribullet           = #x0adc  ;(U+25C0 BLACK LEFT-POINTING TRIANGLE)
XK-filledrighttribullet          = #x0add  ;(U+25B6 BLACK RIGHT-POINTING TRIANGLE)
XK-emfilledcircle                = #x0ade  ;(U+25CF BLACK CIRCLE)
XK-emfilledrect                  = #x0adf  ;(U+25AE BLACK VERTICAL RECTANGLE)
XK-enopencircbullet              = #x0ae0  ;(U+25E6 WHITE BULLET)
XK-enopensquarebullet            = #x0ae1  ;(U+25AB WHITE SMALL SQUARE)
XK-openrectbullet                = #x0ae2  ;(U+25AD WHITE RECTANGLE)
XK-opentribulletup               = #x0ae3  ;(U+25B3 WHITE UP-POINTING TRIANGLE)
XK-opentribulletdown             = #x0ae4  ;(U+25BD WHITE DOWN-POINTING TRIANGLE)
XK-openstar                      = #x0ae5  ;(U+2606 WHITE STAR)
XK-enfilledcircbullet            = #x0ae6  ;(U+2022 BULLET)
XK-enfilledsqbullet              = #x0ae7  ;(U+25AA BLACK SMALL SQUARE)
XK-filledtribulletup             = #x0ae8  ;(U+25B2 BLACK UP-POINTING TRIANGLE)
XK-filledtribulletdown           = #x0ae9  ;(U+25BC BLACK DOWN-POINTING TRIANGLE)
XK-leftpointer                   = #x0aea  ;(U+261C WHITE LEFT POINTING INDEX)
XK-rightpointer                  = #x0aeb  ;(U+261E WHITE RIGHT POINTING INDEX)
XK-club                          = #x0aec  ; U+2663 BLACK CLUB SUIT 
XK-diamond                       = #x0aed  ; U+2666 BLACK DIAMOND SUIT 
XK-heart                         = #x0aee  ; U+2665 BLACK HEART SUIT 
XK-maltesecross                  = #x0af0  ; U+2720 MALTESE CROSS 
XK-dagger                        = #x0af1  ; U+2020 DAGGER 
XK-doubledagger                  = #x0af2  ; U+2021 DOUBLE DAGGER 
XK-checkmark                     = #x0af3  ; U+2713 CHECK MARK 
XK-ballotcross                   = #x0af4  ; U+2717 BALLOT X 
XK-musicalsharp                  = #x0af5  ; U+266F MUSIC SHARP SIGN 
XK-musicalflat                   = #x0af6  ; U+266D MUSIC FLAT SIGN 
XK-malesymbol                    = #x0af7  ; U+2642 MALE SIGN 
XK-femalesymbol                  = #x0af8  ; U+2640 FEMALE SIGN 
XK-telephone                     = #x0af9  ; U+260E BLACK TELEPHONE 
XK-telephonerecorder             = #x0afa  ; U+2315 TELEPHONE RECORDER 
XK-phonographcopyright           = #x0afb  ; U+2117 SOUND RECORDING COPYRIGHT 
XK-caret                         = #x0afc  ; U+2038 CARET 
XK-singlelowquotemark            = #x0afd  ; U+201A SINGLE LOW-9 QUOTATION MARK 
XK-doublelowquotemark            = #x0afe  ; U+201E DOUBLE LOW-9 QUOTATION MARK 
XK-cursor                        = #x0aff
;#endif ; XK-PUBLISHING 

#|
 * APL
 * Byte 3 = 0x0b
 |#

;#ifdef XK-APL
XK-leftcaret                     = #x0ba3  ;(U+003C LESS-THAN SIGN)
XK-rightcaret                    = #x0ba6  ;(U+003E GREATER-THAN SIGN)
XK-downcaret                     = #x0ba8  ;(U+2228 LOGICAL OR)
XK-upcaret                       = #x0ba9  ;(U+2227 LOGICAL AND)
XK-overbar                       = #x0bc0  ;(U+00AF MACRON)
XK-downtack                      = #x0bc2  ; U+22A4 DOWN TACK 
XK-upshoe                        = #x0bc3  ;(U+2229 INTERSECTION)
XK-downstile                     = #x0bc4  ; U+230A LEFT FLOOR 
XK-underbar                      = #x0bc6  ;(U+005F LOW LINE)
XK-jot                           = #x0bca  ; U+2218 RING OPERATOR 
XK-quad                          = #x0bcc  ; U+2395 APL FUNCTIONAL SYMBOL QUAD 
XK-uptack                        = #x0bce  ; U+22A5 UP TACK 
XK-circle                        = #x0bcf  ; U+25CB WHITE CIRCLE 
XK-upstile                       = #x0bd3  ; U+2308 LEFT CEILING 
XK-downshoe                      = #x0bd6  ;(U+222A UNION)
XK-rightshoe                     = #x0bd8  ;(U+2283 SUPERSET OF)
XK-leftshoe                      = #x0bda  ;(U+2282 SUBSET OF)
XK-lefttack                      = #x0bdc  ; U+22A3 LEFT TACK 
XK-righttack                     = #x0bfc  ; U+22A2 RIGHT TACK 
;#endif ; XK-APL 

#|
 * Hebrew
 * Byte 3 = 0x0c
 |#

;#ifdef XK-HEBREW
XK-hebrew-doublelowline          = #x0cdf  ; U+2017 DOUBLE LOW LINE 
XK-hebrew-aleph                  = #x0ce0  ; U+05D0 HEBREW LETTER ALEF 
XK-hebrew-bet                    = #x0ce1  ; U+05D1 HEBREW LETTER BET 
XK-hebrew-beth                   = #x0ce1  ; deprecated 
XK-hebrew-gimel                  = #x0ce2  ; U+05D2 HEBREW LETTER GIMEL 
XK-hebrew-gimmel                 = #x0ce2  ; deprecated 
XK-hebrew-dalet                  = #x0ce3  ; U+05D3 HEBREW LETTER DALET 
XK-hebrew-daleth                 = #x0ce3  ; deprecated 
XK-hebrew-he                     = #x0ce4  ; U+05D4 HEBREW LETTER HE 
XK-hebrew-waw                    = #x0ce5  ; U+05D5 HEBREW LETTER VAV 
XK-hebrew-zain                   = #x0ce6  ; U+05D6 HEBREW LETTER ZAYIN 
XK-hebrew-zayin                  = #x0ce6  ; deprecated 
XK-hebrew-chet                   = #x0ce7  ; U+05D7 HEBREW LETTER HET 
XK-hebrew-het                    = #x0ce7  ; deprecated 
XK-hebrew-tet                    = #x0ce8  ; U+05D8 HEBREW LETTER TET 
XK-hebrew-teth                   = #x0ce8  ; deprecated 
XK-hebrew-yod                    = #x0ce9  ; U+05D9 HEBREW LETTER YOD 
XK-hebrew-finalkaph              = #x0cea  ; U+05DA HEBREW LETTER FINAL KAF 
XK-hebrew-kaph                   = #x0ceb  ; U+05DB HEBREW LETTER KAF 
XK-hebrew-lamed                  = #x0cec  ; U+05DC HEBREW LETTER LAMED 
XK-hebrew-finalmem               = #x0ced  ; U+05DD HEBREW LETTER FINAL MEM 
XK-hebrew-mem                    = #x0cee  ; U+05DE HEBREW LETTER MEM 
XK-hebrew-finalnun               = #x0cef  ; U+05DF HEBREW LETTER FINAL NUN 
XK-hebrew-nun                    = #x0cf0  ; U+05E0 HEBREW LETTER NUN 
XK-hebrew-samech                 = #x0cf1  ; U+05E1 HEBREW LETTER SAMEKH 
XK-hebrew-samekh                 = #x0cf1  ; deprecated 
XK-hebrew-ayin                   = #x0cf2  ; U+05E2 HEBREW LETTER AYIN 
XK-hebrew-finalpe                = #x0cf3  ; U+05E3 HEBREW LETTER FINAL PE 
XK-hebrew-pe                     = #x0cf4  ; U+05E4 HEBREW LETTER PE 
XK-hebrew-finalzade              = #x0cf5  ; U+05E5 HEBREW LETTER FINAL TSADI 
XK-hebrew-finalzadi              = #x0cf5  ; deprecated 
XK-hebrew-zade                   = #x0cf6  ; U+05E6 HEBREW LETTER TSADI 
XK-hebrew-zadi                   = #x0cf6  ; deprecated 
XK-hebrew-qoph                   = #x0cf7  ; U+05E7 HEBREW LETTER QOF 
XK-hebrew-kuf                    = #x0cf7  ; deprecated 
XK-hebrew-resh                   = #x0cf8  ; U+05E8 HEBREW LETTER RESH 
XK-hebrew-shin                   = #x0cf9  ; U+05E9 HEBREW LETTER SHIN 
XK-hebrew-taw                    = #x0cfa  ; U+05EA HEBREW LETTER TAV 
XK-hebrew-taf                    = #x0cfa  ; deprecated 
XK-Hebrew-switch                 = #xff7e  ; Alias for mode-switch 
;#endif ; XK-HEBREW 

#|
 * Thai
 * Byte 3 = 0x0d
 |#

;#ifdef XK-THAI
XK-Thai-kokai                    = #x0da1  ; U+0E01 THAI CHARACTER KO KAI 
XK-Thai-khokhai                  = #x0da2  ; U+0E02 THAI CHARACTER KHO KHAI 
XK-Thai-khokhuat                 = #x0da3  ; U+0E03 THAI CHARACTER KHO KHUAT 
XK-Thai-khokhwai                 = #x0da4  ; U+0E04 THAI CHARACTER KHO KHWAI 
XK-Thai-khokhon                  = #x0da5  ; U+0E05 THAI CHARACTER KHO KHON 
XK-Thai-khorakhang               = #x0da6  ; U+0E06 THAI CHARACTER KHO RAKHANG 
XK-Thai-ngongu                   = #x0da7  ; U+0E07 THAI CHARACTER NGO NGU 
XK-Thai-chochan                  = #x0da8  ; U+0E08 THAI CHARACTER CHO CHAN 
XK-Thai-choching                 = #x0da9  ; U+0E09 THAI CHARACTER CHO CHING 
XK-Thai-chochang                 = #x0daa  ; U+0E0A THAI CHARACTER CHO CHANG 
XK-Thai-soso                     = #x0dab  ; U+0E0B THAI CHARACTER SO SO 
XK-Thai-chochoe                  = #x0dac  ; U+0E0C THAI CHARACTER CHO CHOE 
XK-Thai-yoying                   = #x0dad  ; U+0E0D THAI CHARACTER YO YING 
XK-Thai-dochada                  = #x0dae  ; U+0E0E THAI CHARACTER DO CHADA 
XK-Thai-topatak                  = #x0daf  ; U+0E0F THAI CHARACTER TO PATAK 
XK-Thai-thothan                  = #x0db0  ; U+0E10 THAI CHARACTER THO THAN 
XK-Thai-thonangmontho            = #x0db1  ; U+0E11 THAI CHARACTER THO NANGMONTHO 
XK-Thai-thophuthao               = #x0db2  ; U+0E12 THAI CHARACTER THO PHUTHAO 
XK-Thai-nonen                    = #x0db3  ; U+0E13 THAI CHARACTER NO NEN 
XK-Thai-dodek                    = #x0db4  ; U+0E14 THAI CHARACTER DO DEK 
XK-Thai-totao                    = #x0db5  ; U+0E15 THAI CHARACTER TO TAO 
XK-Thai-thothung                 = #x0db6  ; U+0E16 THAI CHARACTER THO THUNG 
XK-Thai-thothahan                = #x0db7  ; U+0E17 THAI CHARACTER THO THAHAN 
XK-Thai-thothong                 = #x0db8  ; U+0E18 THAI CHARACTER THO THONG 
XK-Thai-nonu                     = #x0db9  ; U+0E19 THAI CHARACTER NO NU 
XK-Thai-bobaimai                 = #x0dba  ; U+0E1A THAI CHARACTER BO BAIMAI 
XK-Thai-popla                    = #x0dbb  ; U+0E1B THAI CHARACTER PO PLA 
XK-Thai-phophung                 = #x0dbc  ; U+0E1C THAI CHARACTER PHO PHUNG 
XK-Thai-fofa                     = #x0dbd  ; U+0E1D THAI CHARACTER FO FA 
XK-Thai-phophan                  = #x0dbe  ; U+0E1E THAI CHARACTER PHO PHAN 
XK-Thai-fofan                    = #x0dbf  ; U+0E1F THAI CHARACTER FO FAN 
XK-Thai-phosamphao               = #x0dc0  ; U+0E20 THAI CHARACTER PHO SAMPHAO 
XK-Thai-moma                     = #x0dc1  ; U+0E21 THAI CHARACTER MO MA 
XK-Thai-yoyak                    = #x0dc2  ; U+0E22 THAI CHARACTER YO YAK 
XK-Thai-rorua                    = #x0dc3  ; U+0E23 THAI CHARACTER RO RUA 
XK-Thai-ru                       = #x0dc4  ; U+0E24 THAI CHARACTER RU 
XK-Thai-loling                   = #x0dc5  ; U+0E25 THAI CHARACTER LO LING 
XK-Thai-lu                       = #x0dc6  ; U+0E26 THAI CHARACTER LU 
XK-Thai-wowaen                   = #x0dc7  ; U+0E27 THAI CHARACTER WO WAEN 
XK-Thai-sosala                   = #x0dc8  ; U+0E28 THAI CHARACTER SO SALA 
XK-Thai-sorusi                   = #x0dc9  ; U+0E29 THAI CHARACTER SO RUSI 
XK-Thai-sosua                    = #x0dca  ; U+0E2A THAI CHARACTER SO SUA 
XK-Thai-hohip                    = #x0dcb  ; U+0E2B THAI CHARACTER HO HIP 
XK-Thai-lochula                  = #x0dcc  ; U+0E2C THAI CHARACTER LO CHULA 
XK-Thai-oang                     = #x0dcd  ; U+0E2D THAI CHARACTER O ANG 
XK-Thai-honokhuk                 = #x0dce  ; U+0E2E THAI CHARACTER HO NOKHUK 
XK-Thai-paiyannoi                = #x0dcf  ; U+0E2F THAI CHARACTER PAIYANNOI 
XK-Thai-saraa                    = #x0dd0  ; U+0E30 THAI CHARACTER SARA A 
XK-Thai-maihanakat               = #x0dd1  ; U+0E31 THAI CHARACTER MAI HAN-AKAT 
XK-Thai-saraaa                   = #x0dd2  ; U+0E32 THAI CHARACTER SARA AA 
XK-Thai-saraam                   = #x0dd3  ; U+0E33 THAI CHARACTER SARA AM 
XK-Thai-sarai                    = #x0dd4  ; U+0E34 THAI CHARACTER SARA I 
XK-Thai-saraii                   = #x0dd5  ; U+0E35 THAI CHARACTER SARA II 
XK-Thai-saraue                   = #x0dd6  ; U+0E36 THAI CHARACTER SARA UE 
XK-Thai-sarauee                  = #x0dd7  ; U+0E37 THAI CHARACTER SARA UEE 
XK-Thai-sarau                    = #x0dd8  ; U+0E38 THAI CHARACTER SARA U 
XK-Thai-sarauu                   = #x0dd9  ; U+0E39 THAI CHARACTER SARA UU 
XK-Thai-phinthu                  = #x0dda  ; U+0E3A THAI CHARACTER PHINTHU 
XK-Thai-maihanakat-maitho        = #x0dde
XK-Thai-baht                     = #x0ddf  ; U+0E3F THAI CURRENCY SYMBOL BAHT 
XK-Thai-sarae                    = #x0de0  ; U+0E40 THAI CHARACTER SARA E 
XK-Thai-saraae                   = #x0de1  ; U+0E41 THAI CHARACTER SARA AE 
XK-Thai-sarao                    = #x0de2  ; U+0E42 THAI CHARACTER SARA O 
XK-Thai-saraaimaimuan            = #x0de3  ; U+0E43 THAI CHARACTER SARA AI MAIMUAN 
XK-Thai-saraaimaimalai           = #x0de4  ; U+0E44 THAI CHARACTER SARA AI MAIMALAI 
XK-Thai-lakkhangyao              = #x0de5  ; U+0E45 THAI CHARACTER LAKKHANGYAO 
XK-Thai-maiyamok                 = #x0de6  ; U+0E46 THAI CHARACTER MAIYAMOK 
XK-Thai-maitaikhu                = #x0de7  ; U+0E47 THAI CHARACTER MAITAIKHU 
XK-Thai-maiek                    = #x0de8  ; U+0E48 THAI CHARACTER MAI EK 
XK-Thai-maitho                   = #x0de9  ; U+0E49 THAI CHARACTER MAI THO 
XK-Thai-maitri                   = #x0dea  ; U+0E4A THAI CHARACTER MAI TRI 
XK-Thai-maichattawa              = #x0deb  ; U+0E4B THAI CHARACTER MAI CHATTAWA 
XK-Thai-thanthakhat              = #x0dec  ; U+0E4C THAI CHARACTER THANTHAKHAT 
XK-Thai-nikhahit                 = #x0ded  ; U+0E4D THAI CHARACTER NIKHAHIT 
XK-Thai-leksun                   = #x0df0  ; U+0E50 THAI DIGIT ZERO 
XK-Thai-leknung                  = #x0df1  ; U+0E51 THAI DIGIT ONE 
XK-Thai-leksong                  = #x0df2  ; U+0E52 THAI DIGIT TWO 
XK-Thai-leksam                   = #x0df3  ; U+0E53 THAI DIGIT THREE 
XK-Thai-leksi                    = #x0df4  ; U+0E54 THAI DIGIT FOUR 
XK-Thai-lekha                    = #x0df5  ; U+0E55 THAI DIGIT FIVE 
XK-Thai-lekhok                   = #x0df6  ; U+0E56 THAI DIGIT SIX 
XK-Thai-lekchet                  = #x0df7  ; U+0E57 THAI DIGIT SEVEN 
XK-Thai-lekpaet                  = #x0df8  ; U+0E58 THAI DIGIT EIGHT 
XK-Thai-lekkao                   = #x0df9  ; U+0E59 THAI DIGIT NINE 
;#endif ; XK-THAI 

#|
 * Korean
 * Byte 3 = 0x0e
 |#

;#ifdef XK-KOREAN

XK-Hangul                        = #xff31  ; Hangul start/stop(toggle) 
XK-Hangul-Start                  = #xff32  ; Hangul start 
XK-Hangul-End                    = #xff33  ; Hangul end, English start 
XK-Hangul-Hanja                  = #xff34  ; Start Hangul->Hanja Conversion 
XK-Hangul-Jamo                   = #xff35  ; Hangul Jamo mode 
XK-Hangul-Romaja                 = #xff36  ; Hangul Romaja mode 
XK-Hangul-Codeinput              = #xff37  ; Hangul code input mode 
XK-Hangul-Jeonja                 = #xff38  ; Jeonja mode 
XK-Hangul-Banja                  = #xff39  ; Banja mode 
XK-Hangul-PreHanja               = #xff3a  ; Pre Hanja conversion 
XK-Hangul-PostHanja              = #xff3b  ; Post Hanja conversion 
XK-Hangul-SingleCandidate        = #xff3c  ; Single candidate 
XK-Hangul-MultipleCandidate      = #xff3d  ; Multiple candidate 
XK-Hangul-PreviousCandidate      = #xff3e  ; Previous candidate 
XK-Hangul-Special                = #xff3f  ; Special symbols 
XK-Hangul-switch                 = #xff7e  ; Alias for mode-switch 

; Hangul Consonant Characters 
XK-Hangul-Kiyeog                 = #x0ea1
XK-Hangul-SsangKiyeog            = #x0ea2
XK-Hangul-KiyeogSios             = #x0ea3
XK-Hangul-Nieun                  = #x0ea4
XK-Hangul-NieunJieuj             = #x0ea5
XK-Hangul-NieunHieuh             = #x0ea6
XK-Hangul-Dikeud                 = #x0ea7
XK-Hangul-SsangDikeud            = #x0ea8
XK-Hangul-Rieul                  = #x0ea9
XK-Hangul-RieulKiyeog            = #x0eaa
XK-Hangul-RieulMieum             = #x0eab
XK-Hangul-RieulPieub             = #x0eac
XK-Hangul-RieulSios              = #x0ead
XK-Hangul-RieulTieut             = #x0eae
XK-Hangul-RieulPhieuf            = #x0eaf
XK-Hangul-RieulHieuh             = #x0eb0
XK-Hangul-Mieum                  = #x0eb1
XK-Hangul-Pieub                  = #x0eb2
XK-Hangul-SsangPieub             = #x0eb3
XK-Hangul-PieubSios              = #x0eb4
XK-Hangul-Sios                   = #x0eb5
XK-Hangul-SsangSios              = #x0eb6
XK-Hangul-Ieung                  = #x0eb7
XK-Hangul-Jieuj                  = #x0eb8
XK-Hangul-SsangJieuj             = #x0eb9
XK-Hangul-Cieuc                  = #x0eba
XK-Hangul-Khieuq                 = #x0ebb
XK-Hangul-Tieut                  = #x0ebc
XK-Hangul-Phieuf                 = #x0ebd
XK-Hangul-Hieuh                  = #x0ebe

; Hangul Vowel Characters 
XK-Hangul-A                      = #x0ebf
XK-Hangul-AE                     = #x0ec0
XK-Hangul-YA                     = #x0ec1
XK-Hangul-YAE                    = #x0ec2
XK-Hangul-EO                     = #x0ec3
XK-Hangul-E                      = #x0ec4
XK-Hangul-YEO                    = #x0ec5
XK-Hangul-YE                     = #x0ec6
XK-Hangul-O                      = #x0ec7
XK-Hangul-WA                     = #x0ec8
XK-Hangul-WAE                    = #x0ec9
XK-Hangul-OE                     = #x0eca
XK-Hangul-YO                     = #x0ecb
XK-Hangul-U                      = #x0ecc
XK-Hangul-WEO                    = #x0ecd
XK-Hangul-WE                     = #x0ece
XK-Hangul-WI                     = #x0ecf
XK-Hangul-YU                     = #x0ed0
XK-Hangul-EU                     = #x0ed1
XK-Hangul-YI                     = #x0ed2
XK-Hangul-I                      = #x0ed3

; Hangul syllable-final (JongSeong) Characters 
XK-Hangul-J-Kiyeog               = #x0ed4
XK-Hangul-J-SsangKiyeog          = #x0ed5
XK-Hangul-J-KiyeogSios           = #x0ed6
XK-Hangul-J-Nieun                = #x0ed7
XK-Hangul-J-NieunJieuj           = #x0ed8
XK-Hangul-J-NieunHieuh           = #x0ed9
XK-Hangul-J-Dikeud               = #x0eda
XK-Hangul-J-Rieul                = #x0edb
XK-Hangul-J-RieulKiyeog          = #x0edc
XK-Hangul-J-RieulMieum           = #x0edd
XK-Hangul-J-RieulPieub           = #x0ede
XK-Hangul-J-RieulSios            = #x0edf
XK-Hangul-J-RieulTieut           = #x0ee0
XK-Hangul-J-RieulPhieuf          = #x0ee1
XK-Hangul-J-RieulHieuh           = #x0ee2
XK-Hangul-J-Mieum                = #x0ee3
XK-Hangul-J-Pieub                = #x0ee4
XK-Hangul-J-PieubSios            = #x0ee5
XK-Hangul-J-Sios                 = #x0ee6
XK-Hangul-J-SsangSios            = #x0ee7
XK-Hangul-J-Ieung                = #x0ee8
XK-Hangul-J-Jieuj                = #x0ee9
XK-Hangul-J-Cieuc                = #x0eea
XK-Hangul-J-Khieuq               = #x0eeb
XK-Hangul-J-Tieut                = #x0eec
XK-Hangul-J-Phieuf               = #x0eed
XK-Hangul-J-Hieuh                = #x0eee

; Ancient Hangul Consonant Characters 
XK-Hangul-RieulYeorinHieuh       = #x0eef
XK-Hangul-SunkyeongeumMieum      = #x0ef0
XK-Hangul-SunkyeongeumPieub      = #x0ef1
XK-Hangul-PanSios                = #x0ef2
XK-Hangul-KkogjiDalrinIeung      = #x0ef3
XK-Hangul-SunkyeongeumPhieuf     = #x0ef4
XK-Hangul-YeorinHieuh            = #x0ef5

; Ancient Hangul Vowel Characters 
XK-Hangul-AraeA                  = #x0ef6
XK-Hangul-AraeAE                 = #x0ef7

; Ancient Hangul syllable-final (JongSeong) Characters 
XK-Hangul-J-PanSios              = #x0ef8
XK-Hangul-J-KkogjiDalrinIeung    = #x0ef9
XK-Hangul-J-YeorinHieuh          = #x0efa

; Korean currency symbol 
XK-Korean-Won                    = #x0eff  ;(U+20A9 WON SIGN)

;#endif ; XK-KOREAN 

#|
 * Armenian
 |#

;#ifdef XK-ARMENIAN
XK-Armenian-ligature-ew       = #x1000587  ; U+0587 ARMENIAN SMALL LIGATURE ECH YIWN 
XK-Armenian-full-stop         = #x1000589  ; U+0589 ARMENIAN FULL STOP 
XK-Armenian-verjaket          = #x1000589  ; U+0589 ARMENIAN FULL STOP 
XK-Armenian-separation-mark   = #x100055d  ; U+055D ARMENIAN COMMA 
XK-Armenian-but               = #x100055d  ; U+055D ARMENIAN COMMA 
XK-Armenian-hyphen            = #x100058a  ; U+058A ARMENIAN HYPHEN 
XK-Armenian-yentamna          = #x100058a  ; U+058A ARMENIAN HYPHEN 
XK-Armenian-exclam            = #x100055c  ; U+055C ARMENIAN EXCLAMATION MARK 
XK-Armenian-amanak            = #x100055c  ; U+055C ARMENIAN EXCLAMATION MARK 
XK-Armenian-accent            = #x100055b  ; U+055B ARMENIAN EMPHASIS MARK 
XK-Armenian-shesht            = #x100055b  ; U+055B ARMENIAN EMPHASIS MARK 
XK-Armenian-question          = #x100055e  ; U+055E ARMENIAN QUESTION MARK 
XK-Armenian-paruyk            = #x100055e  ; U+055E ARMENIAN QUESTION MARK 
XK-Armenian-AYB               = #x1000531  ; U+0531 ARMENIAN CAPITAL LETTER AYB 
XK-Armenian-ayb               = #x1000561  ; U+0561 ARMENIAN SMALL LETTER AYB 
XK-Armenian-BEN               = #x1000532  ; U+0532 ARMENIAN CAPITAL LETTER BEN 
XK-Armenian-ben               = #x1000562  ; U+0562 ARMENIAN SMALL LETTER BEN 
XK-Armenian-GIM               = #x1000533  ; U+0533 ARMENIAN CAPITAL LETTER GIM 
XK-Armenian-gim               = #x1000563  ; U+0563 ARMENIAN SMALL LETTER GIM 
XK-Armenian-DA                = #x1000534  ; U+0534 ARMENIAN CAPITAL LETTER DA 
XK-Armenian-da                = #x1000564  ; U+0564 ARMENIAN SMALL LETTER DA 
XK-Armenian-YECH              = #x1000535  ; U+0535 ARMENIAN CAPITAL LETTER ECH 
XK-Armenian-yech              = #x1000565  ; U+0565 ARMENIAN SMALL LETTER ECH 
XK-Armenian-ZA                = #x1000536  ; U+0536 ARMENIAN CAPITAL LETTER ZA 
XK-Armenian-za                = #x1000566  ; U+0566 ARMENIAN SMALL LETTER ZA 
XK-Armenian-E                 = #x1000537  ; U+0537 ARMENIAN CAPITAL LETTER EH 
XK-Armenian-e                 = #x1000567  ; U+0567 ARMENIAN SMALL LETTER EH 
XK-Armenian-AT                = #x1000538  ; U+0538 ARMENIAN CAPITAL LETTER ET 
XK-Armenian-at                = #x1000568  ; U+0568 ARMENIAN SMALL LETTER ET 
XK-Armenian-TO                = #x1000539  ; U+0539 ARMENIAN CAPITAL LETTER TO 
XK-Armenian-to                = #x1000569  ; U+0569 ARMENIAN SMALL LETTER TO 
XK-Armenian-ZHE               = #x100053a  ; U+053A ARMENIAN CAPITAL LETTER ZHE 
XK-Armenian-zhe               = #x100056a  ; U+056A ARMENIAN SMALL LETTER ZHE 
XK-Armenian-INI               = #x100053b  ; U+053B ARMENIAN CAPITAL LETTER INI 
XK-Armenian-ini               = #x100056b  ; U+056B ARMENIAN SMALL LETTER INI 
XK-Armenian-LYUN              = #x100053c  ; U+053C ARMENIAN CAPITAL LETTER LIWN 
XK-Armenian-lyun              = #x100056c  ; U+056C ARMENIAN SMALL LETTER LIWN 
XK-Armenian-KHE               = #x100053d  ; U+053D ARMENIAN CAPITAL LETTER XEH 
XK-Armenian-khe               = #x100056d  ; U+056D ARMENIAN SMALL LETTER XEH 
XK-Armenian-TSA               = #x100053e  ; U+053E ARMENIAN CAPITAL LETTER CA 
XK-Armenian-tsa               = #x100056e  ; U+056E ARMENIAN SMALL LETTER CA 
XK-Armenian-KEN               = #x100053f  ; U+053F ARMENIAN CAPITAL LETTER KEN 
XK-Armenian-ken               = #x100056f  ; U+056F ARMENIAN SMALL LETTER KEN 
XK-Armenian-HO                = #x1000540  ; U+0540 ARMENIAN CAPITAL LETTER HO 
XK-Armenian-ho                = #x1000570  ; U+0570 ARMENIAN SMALL LETTER HO 
XK-Armenian-DZA               = #x1000541  ; U+0541 ARMENIAN CAPITAL LETTER JA 
XK-Armenian-dza               = #x1000571  ; U+0571 ARMENIAN SMALL LETTER JA 
XK-Armenian-GHAT              = #x1000542  ; U+0542 ARMENIAN CAPITAL LETTER GHAD 
XK-Armenian-ghat              = #x1000572  ; U+0572 ARMENIAN SMALL LETTER GHAD 
XK-Armenian-TCHE              = #x1000543  ; U+0543 ARMENIAN CAPITAL LETTER CHEH 
XK-Armenian-tche              = #x1000573  ; U+0573 ARMENIAN SMALL LETTER CHEH 
XK-Armenian-MEN               = #x1000544  ; U+0544 ARMENIAN CAPITAL LETTER MEN 
XK-Armenian-men               = #x1000574  ; U+0574 ARMENIAN SMALL LETTER MEN 
XK-Armenian-HI                = #x1000545  ; U+0545 ARMENIAN CAPITAL LETTER YI 
XK-Armenian-hi                = #x1000575  ; U+0575 ARMENIAN SMALL LETTER YI 
XK-Armenian-NU                = #x1000546  ; U+0546 ARMENIAN CAPITAL LETTER NOW 
XK-Armenian-nu                = #x1000576  ; U+0576 ARMENIAN SMALL LETTER NOW 
XK-Armenian-SHA               = #x1000547  ; U+0547 ARMENIAN CAPITAL LETTER SHA 
XK-Armenian-sha               = #x1000577  ; U+0577 ARMENIAN SMALL LETTER SHA 
XK-Armenian-VO                = #x1000548  ; U+0548 ARMENIAN CAPITAL LETTER VO 
XK-Armenian-vo                = #x1000578  ; U+0578 ARMENIAN SMALL LETTER VO 
XK-Armenian-CHA               = #x1000549  ; U+0549 ARMENIAN CAPITAL LETTER CHA 
XK-Armenian-cha               = #x1000579  ; U+0579 ARMENIAN SMALL LETTER CHA 
XK-Armenian-PE                = #x100054a  ; U+054A ARMENIAN CAPITAL LETTER PEH 
XK-Armenian-pe                = #x100057a  ; U+057A ARMENIAN SMALL LETTER PEH 
XK-Armenian-JE                = #x100054b  ; U+054B ARMENIAN CAPITAL LETTER JHEH 
XK-Armenian-je                = #x100057b  ; U+057B ARMENIAN SMALL LETTER JHEH 
XK-Armenian-RA                = #x100054c  ; U+054C ARMENIAN CAPITAL LETTER RA 
XK-Armenian-ra                = #x100057c  ; U+057C ARMENIAN SMALL LETTER RA 
XK-Armenian-SE                = #x100054d  ; U+054D ARMENIAN CAPITAL LETTER SEH 
XK-Armenian-se                = #x100057d  ; U+057D ARMENIAN SMALL LETTER SEH 
XK-Armenian-VEV               = #x100054e  ; U+054E ARMENIAN CAPITAL LETTER VEW 
XK-Armenian-vev               = #x100057e  ; U+057E ARMENIAN SMALL LETTER VEW 
XK-Armenian-TYUN              = #x100054f  ; U+054F ARMENIAN CAPITAL LETTER TIWN 
XK-Armenian-tyun              = #x100057f  ; U+057F ARMENIAN SMALL LETTER TIWN 
XK-Armenian-RE                = #x1000550  ; U+0550 ARMENIAN CAPITAL LETTER REH 
XK-Armenian-re                = #x1000580  ; U+0580 ARMENIAN SMALL LETTER REH 
XK-Armenian-TSO               = #x1000551  ; U+0551 ARMENIAN CAPITAL LETTER CO 
XK-Armenian-tso               = #x1000581  ; U+0581 ARMENIAN SMALL LETTER CO 
XK-Armenian-VYUN              = #x1000552  ; U+0552 ARMENIAN CAPITAL LETTER YIWN 
XK-Armenian-vyun              = #x1000582  ; U+0582 ARMENIAN SMALL LETTER YIWN 
XK-Armenian-PYUR              = #x1000553  ; U+0553 ARMENIAN CAPITAL LETTER PIWR 
XK-Armenian-pyur              = #x1000583  ; U+0583 ARMENIAN SMALL LETTER PIWR 
XK-Armenian-KE                = #x1000554  ; U+0554 ARMENIAN CAPITAL LETTER KEH 
XK-Armenian-ke                = #x1000584  ; U+0584 ARMENIAN SMALL LETTER KEH 
XK-Armenian-O                 = #x1000555  ; U+0555 ARMENIAN CAPITAL LETTER OH 
XK-Armenian-o                 = #x1000585  ; U+0585 ARMENIAN SMALL LETTER OH 
XK-Armenian-FE                = #x1000556  ; U+0556 ARMENIAN CAPITAL LETTER FEH 
XK-Armenian-fe                = #x1000586  ; U+0586 ARMENIAN SMALL LETTER FEH 
XK-Armenian-apostrophe        = #x100055a  ; U+055A ARMENIAN APOSTROPHE 
;#endif ; XK-ARMENIAN 

#|
 * Georgian
 |#

;#ifdef XK-GEORGIAN
XK-Georgian-an                = #x10010d0  ; U+10D0 GEORGIAN LETTER AN 
XK-Georgian-ban               = #x10010d1  ; U+10D1 GEORGIAN LETTER BAN 
XK-Georgian-gan               = #x10010d2  ; U+10D2 GEORGIAN LETTER GAN 
XK-Georgian-don               = #x10010d3  ; U+10D3 GEORGIAN LETTER DON 
XK-Georgian-en                = #x10010d4  ; U+10D4 GEORGIAN LETTER EN 
XK-Georgian-vin               = #x10010d5  ; U+10D5 GEORGIAN LETTER VIN 
XK-Georgian-zen               = #x10010d6  ; U+10D6 GEORGIAN LETTER ZEN 
XK-Georgian-tan               = #x10010d7  ; U+10D7 GEORGIAN LETTER TAN 
XK-Georgian-in                = #x10010d8  ; U+10D8 GEORGIAN LETTER IN 
XK-Georgian-kan               = #x10010d9  ; U+10D9 GEORGIAN LETTER KAN 
XK-Georgian-las               = #x10010da  ; U+10DA GEORGIAN LETTER LAS 
XK-Georgian-man               = #x10010db  ; U+10DB GEORGIAN LETTER MAN 
XK-Georgian-nar               = #x10010dc  ; U+10DC GEORGIAN LETTER NAR 
XK-Georgian-on                = #x10010dd  ; U+10DD GEORGIAN LETTER ON 
XK-Georgian-par               = #x10010de  ; U+10DE GEORGIAN LETTER PAR 
XK-Georgian-zhar              = #x10010df  ; U+10DF GEORGIAN LETTER ZHAR 
XK-Georgian-rae               = #x10010e0  ; U+10E0 GEORGIAN LETTER RAE 
XK-Georgian-san               = #x10010e1  ; U+10E1 GEORGIAN LETTER SAN 
XK-Georgian-tar               = #x10010e2  ; U+10E2 GEORGIAN LETTER TAR 
XK-Georgian-un                = #x10010e3  ; U+10E3 GEORGIAN LETTER UN 
XK-Georgian-phar              = #x10010e4  ; U+10E4 GEORGIAN LETTER PHAR 
XK-Georgian-khar              = #x10010e5  ; U+10E5 GEORGIAN LETTER KHAR 
XK-Georgian-ghan              = #x10010e6  ; U+10E6 GEORGIAN LETTER GHAN 
XK-Georgian-qar               = #x10010e7  ; U+10E7 GEORGIAN LETTER QAR 
XK-Georgian-shin              = #x10010e8  ; U+10E8 GEORGIAN LETTER SHIN 
XK-Georgian-chin              = #x10010e9  ; U+10E9 GEORGIAN LETTER CHIN 
XK-Georgian-can               = #x10010ea  ; U+10EA GEORGIAN LETTER CAN 
XK-Georgian-jil               = #x10010eb  ; U+10EB GEORGIAN LETTER JIL 
XK-Georgian-cil               = #x10010ec  ; U+10EC GEORGIAN LETTER CIL 
XK-Georgian-char              = #x10010ed  ; U+10ED GEORGIAN LETTER CHAR 
XK-Georgian-xan               = #x10010ee  ; U+10EE GEORGIAN LETTER XAN 
XK-Georgian-jhan              = #x10010ef  ; U+10EF GEORGIAN LETTER JHAN 
XK-Georgian-hae               = #x10010f0  ; U+10F0 GEORGIAN LETTER HAE 
XK-Georgian-he                = #x10010f1  ; U+10F1 GEORGIAN LETTER HE 
XK-Georgian-hie               = #x10010f2  ; U+10F2 GEORGIAN LETTER HIE 
XK-Georgian-we                = #x10010f3  ; U+10F3 GEORGIAN LETTER WE 
XK-Georgian-har               = #x10010f4  ; U+10F4 GEORGIAN LETTER HAR 
XK-Georgian-hoe               = #x10010f5  ; U+10F5 GEORGIAN LETTER HOE 
XK-Georgian-fi                = #x10010f6  ; U+10F6 GEORGIAN LETTER FI 
;#endif ; XK-GEORGIAN 

#|
 * Azeri (and other Turkic or Caucasian languages)
 |#

;#ifdef XK-CAUCASUS
; latin 
XK-Xabovedot                  = #x1001e8a  ; U+1E8A LATIN CAPITAL LETTER X WITH DOT ABOVE 
XK-Ibreve                     = #x100012c  ; U+012C LATIN CAPITAL LETTER I WITH BREVE 
XK-Zstroke                    = #x10001b5  ; U+01B5 LATIN CAPITAL LETTER Z WITH STROKE 
XK-Gcaron                     = #x10001e6  ; U+01E6 LATIN CAPITAL LETTER G WITH CARON 
XK-Ocaron                     = #x10001d1  ; U+01D2 LATIN CAPITAL LETTER O WITH CARON 
XK-Obarred                    = #x100019f  ; U+019F LATIN CAPITAL LETTER O WITH MIDDLE TILDE 
XK-xabovedot                  = #x1001e8b  ; U+1E8B LATIN SMALL LETTER X WITH DOT ABOVE 
XK-ibreve                     = #x100012d  ; U+012D LATIN SMALL LETTER I WITH BREVE 
XK-zstroke                    = #x10001b6  ; U+01B6 LATIN SMALL LETTER Z WITH STROKE 
XK-gcaron                     = #x10001e7  ; U+01E7 LATIN SMALL LETTER G WITH CARON 
XK-ocaron                     = #x10001d2  ; U+01D2 LATIN SMALL LETTER O WITH CARON 
XK-obarred                    = #x1000275  ; U+0275 LATIN SMALL LETTER BARRED O 
XK-SCHWA                      = #x100018f  ; U+018F LATIN CAPITAL LETTER SCHWA 
XK-schwa                      = #x1000259  ; U+0259 LATIN SMALL LETTER SCHWA 
; those are not really Caucasus 
; For Inupiak 
XK-Lbelowdot                  = #x1001e36  ; U+1E36 LATIN CAPITAL LETTER L WITH DOT BELOW 
XK-lbelowdot                  = #x1001e37  ; U+1E37 LATIN SMALL LETTER L WITH DOT BELOW 
;#endif ; XK-CAUCASUS 

#|
 * Vietnamese
 |#
 
;#ifdef XK-VIETNAMESE
XK-Abelowdot                  = #x1001ea0  ; U+1EA0 LATIN CAPITAL LETTER A WITH DOT BELOW 
XK-abelowdot                  = #x1001ea1  ; U+1EA1 LATIN SMALL LETTER A WITH DOT BELOW 
XK-Ahook                      = #x1001ea2  ; U+1EA2 LATIN CAPITAL LETTER A WITH HOOK ABOVE 
XK-ahook                      = #x1001ea3  ; U+1EA3 LATIN SMALL LETTER A WITH HOOK ABOVE 
XK-Acircumflexacute           = #x1001ea4  ; U+1EA4 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE 
XK-acircumflexacute           = #x1001ea5  ; U+1EA5 LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE 
XK-Acircumflexgrave           = #x1001ea6  ; U+1EA6 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE 
XK-acircumflexgrave           = #x1001ea7  ; U+1EA7 LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE 
XK-Acircumflexhook            = #x1001ea8  ; U+1EA8 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE 
XK-acircumflexhook            = #x1001ea9  ; U+1EA9 LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE 
XK-Acircumflextilde           = #x1001eaa  ; U+1EAA LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE 
XK-acircumflextilde           = #x1001eab  ; U+1EAB LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE 
XK-Acircumflexbelowdot        = #x1001eac  ; U+1EAC LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW 
XK-acircumflexbelowdot        = #x1001ead  ; U+1EAD LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW 
XK-Abreveacute                = #x1001eae  ; U+1EAE LATIN CAPITAL LETTER A WITH BREVE AND ACUTE 
XK-abreveacute                = #x1001eaf  ; U+1EAF LATIN SMALL LETTER A WITH BREVE AND ACUTE 
XK-Abrevegrave                = #x1001eb0  ; U+1EB0 LATIN CAPITAL LETTER A WITH BREVE AND GRAVE 
XK-abrevegrave                = #x1001eb1  ; U+1EB1 LATIN SMALL LETTER A WITH BREVE AND GRAVE 
XK-Abrevehook                 = #x1001eb2  ; U+1EB2 LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE 
XK-abrevehook                 = #x1001eb3  ; U+1EB3 LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE 
XK-Abrevetilde                = #x1001eb4  ; U+1EB4 LATIN CAPITAL LETTER A WITH BREVE AND TILDE 
XK-abrevetilde                = #x1001eb5  ; U+1EB5 LATIN SMALL LETTER A WITH BREVE AND TILDE 
XK-Abrevebelowdot             = #x1001eb6  ; U+1EB6 LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW 
XK-abrevebelowdot             = #x1001eb7  ; U+1EB7 LATIN SMALL LETTER A WITH BREVE AND DOT BELOW 
XK-Ebelowdot                  = #x1001eb8  ; U+1EB8 LATIN CAPITAL LETTER E WITH DOT BELOW 
XK-ebelowdot                  = #x1001eb9  ; U+1EB9 LATIN SMALL LETTER E WITH DOT BELOW 
XK-Ehook                      = #x1001eba  ; U+1EBA LATIN CAPITAL LETTER E WITH HOOK ABOVE 
XK-ehook                      = #x1001ebb  ; U+1EBB LATIN SMALL LETTER E WITH HOOK ABOVE 
XK-Etilde                     = #x1001ebc  ; U+1EBC LATIN CAPITAL LETTER E WITH TILDE 
XK-etilde                     = #x1001ebd  ; U+1EBD LATIN SMALL LETTER E WITH TILDE 
XK-Ecircumflexacute           = #x1001ebe  ; U+1EBE LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE 
XK-ecircumflexacute           = #x1001ebf  ; U+1EBF LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE 
XK-Ecircumflexgrave           = #x1001ec0  ; U+1EC0 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE 
XK-ecircumflexgrave           = #x1001ec1  ; U+1EC1 LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE 
XK-Ecircumflexhook            = #x1001ec2  ; U+1EC2 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE 
XK-ecircumflexhook            = #x1001ec3  ; U+1EC3 LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE 
XK-Ecircumflextilde           = #x1001ec4  ; U+1EC4 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE 
XK-ecircumflextilde           = #x1001ec5  ; U+1EC5 LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE 
XK-Ecircumflexbelowdot        = #x1001ec6  ; U+1EC6 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW 
XK-ecircumflexbelowdot        = #x1001ec7  ; U+1EC7 LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW 
XK-Ihook                      = #x1001ec8  ; U+1EC8 LATIN CAPITAL LETTER I WITH HOOK ABOVE 
XK-ihook                      = #x1001ec9  ; U+1EC9 LATIN SMALL LETTER I WITH HOOK ABOVE 
XK-Ibelowdot                  = #x1001eca  ; U+1ECA LATIN CAPITAL LETTER I WITH DOT BELOW 
XK-ibelowdot                  = #x1001ecb  ; U+1ECB LATIN SMALL LETTER I WITH DOT BELOW 
XK-Obelowdot                  = #x1001ecc  ; U+1ECC LATIN CAPITAL LETTER O WITH DOT BELOW 
XK-obelowdot                  = #x1001ecd  ; U+1ECD LATIN SMALL LETTER O WITH DOT BELOW 
XK-Ohook                      = #x1001ece  ; U+1ECE LATIN CAPITAL LETTER O WITH HOOK ABOVE 
XK-ohook                      = #x1001ecf  ; U+1ECF LATIN SMALL LETTER O WITH HOOK ABOVE 
XK-Ocircumflexacute           = #x1001ed0  ; U+1ED0 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE 
XK-ocircumflexacute           = #x1001ed1  ; U+1ED1 LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE 
XK-Ocircumflexgrave           = #x1001ed2  ; U+1ED2 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE 
XK-ocircumflexgrave           = #x1001ed3  ; U+1ED3 LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE 
XK-Ocircumflexhook            = #x1001ed4  ; U+1ED4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE 
XK-ocircumflexhook            = #x1001ed5  ; U+1ED5 LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE 
XK-Ocircumflextilde           = #x1001ed6  ; U+1ED6 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE 
XK-ocircumflextilde           = #x1001ed7  ; U+1ED7 LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE 
XK-Ocircumflexbelowdot        = #x1001ed8  ; U+1ED8 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW 
XK-ocircumflexbelowdot        = #x1001ed9  ; U+1ED9 LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW 
XK-Ohornacute                 = #x1001eda  ; U+1EDA LATIN CAPITAL LETTER O WITH HORN AND ACUTE 
XK-ohornacute                 = #x1001edb  ; U+1EDB LATIN SMALL LETTER O WITH HORN AND ACUTE 
XK-Ohorngrave                 = #x1001edc  ; U+1EDC LATIN CAPITAL LETTER O WITH HORN AND GRAVE 
XK-ohorngrave                 = #x1001edd  ; U+1EDD LATIN SMALL LETTER O WITH HORN AND GRAVE 
XK-Ohornhook                  = #x1001ede  ; U+1EDE LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE 
XK-ohornhook                  = #x1001edf  ; U+1EDF LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE 
XK-Ohorntilde                 = #x1001ee0  ; U+1EE0 LATIN CAPITAL LETTER O WITH HORN AND TILDE 
XK-ohorntilde                 = #x1001ee1  ; U+1EE1 LATIN SMALL LETTER O WITH HORN AND TILDE 
XK-Ohornbelowdot              = #x1001ee2  ; U+1EE2 LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW 
XK-ohornbelowdot              = #x1001ee3  ; U+1EE3 LATIN SMALL LETTER O WITH HORN AND DOT BELOW 
XK-Ubelowdot                  = #x1001ee4  ; U+1EE4 LATIN CAPITAL LETTER U WITH DOT BELOW 
XK-ubelowdot                  = #x1001ee5  ; U+1EE5 LATIN SMALL LETTER U WITH DOT BELOW 
XK-Uhook                      = #x1001ee6  ; U+1EE6 LATIN CAPITAL LETTER U WITH HOOK ABOVE 
XK-uhook                      = #x1001ee7  ; U+1EE7 LATIN SMALL LETTER U WITH HOOK ABOVE 
XK-Uhornacute                 = #x1001ee8  ; U+1EE8 LATIN CAPITAL LETTER U WITH HORN AND ACUTE 
XK-uhornacute                 = #x1001ee9  ; U+1EE9 LATIN SMALL LETTER U WITH HORN AND ACUTE 
XK-Uhorngrave                 = #x1001eea  ; U+1EEA LATIN CAPITAL LETTER U WITH HORN AND GRAVE 
XK-uhorngrave                 = #x1001eeb  ; U+1EEB LATIN SMALL LETTER U WITH HORN AND GRAVE 
XK-Uhornhook                  = #x1001eec  ; U+1EEC LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE 
XK-uhornhook                  = #x1001eed  ; U+1EED LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE 
XK-Uhorntilde                 = #x1001eee  ; U+1EEE LATIN CAPITAL LETTER U WITH HORN AND TILDE 
XK-uhorntilde                 = #x1001eef  ; U+1EEF LATIN SMALL LETTER U WITH HORN AND TILDE 
XK-Uhornbelowdot              = #x1001ef0  ; U+1EF0 LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW 
XK-uhornbelowdot              = #x1001ef1  ; U+1EF1 LATIN SMALL LETTER U WITH HORN AND DOT BELOW 
XK-Ybelowdot                  = #x1001ef4  ; U+1EF4 LATIN CAPITAL LETTER Y WITH DOT BELOW 
XK-ybelowdot                  = #x1001ef5  ; U+1EF5 LATIN SMALL LETTER Y WITH DOT BELOW 
XK-Yhook                      = #x1001ef6  ; U+1EF6 LATIN CAPITAL LETTER Y WITH HOOK ABOVE 
XK-yhook                      = #x1001ef7  ; U+1EF7 LATIN SMALL LETTER Y WITH HOOK ABOVE 
XK-Ytilde                     = #x1001ef8  ; U+1EF8 LATIN CAPITAL LETTER Y WITH TILDE 
XK-ytilde                     = #x1001ef9  ; U+1EF9 LATIN SMALL LETTER Y WITH TILDE 
XK-Ohorn                      = #x10001a0  ; U+01A0 LATIN CAPITAL LETTER O WITH HORN 
XK-ohorn                      = #x10001a1  ; U+01A1 LATIN SMALL LETTER O WITH HORN 
XK-Uhorn                      = #x10001af  ; U+01AF LATIN CAPITAL LETTER U WITH HORN 
XK-uhorn                      = #x10001b0  ; U+01B0 LATIN SMALL LETTER U WITH HORN 

;#endif ; XK-VIETNAMESE 

;#ifdef XK-CURRENCY
XK-EcuSign                    = #x10020a0  ; U+20A0 EURO-CURRENCY SIGN 
XK-ColonSign                  = #x10020a1  ; U+20A1 COLON SIGN 
XK-CruzeiroSign               = #x10020a2  ; U+20A2 CRUZEIRO SIGN 
XK-FFrancSign                 = #x10020a3  ; U+20A3 FRENCH FRANC SIGN 
XK-LiraSign                   = #x10020a4  ; U+20A4 LIRA SIGN 
XK-MillSign                   = #x10020a5  ; U+20A5 MILL SIGN 
XK-NairaSign                  = #x10020a6  ; U+20A6 NAIRA SIGN 
XK-PesetaSign                 = #x10020a7  ; U+20A7 PESETA SIGN 
XK-RupeeSign                  = #x10020a8  ; U+20A8 RUPEE SIGN 
XK-WonSign                    = #x10020a9  ; U+20A9 WON SIGN 
XK-NewSheqelSign              = #x10020aa  ; U+20AA NEW SHEQEL SIGN 
XK-DongSign                   = #x10020ab  ; U+20AB DONG SIGN 
XK-EuroSign                      = #x20ac  ; U+20AC EURO SIGN 
;#endif ; XK-CURRENCY 

;#ifdef XK-MATHEMATICAL
; one, two and three are defined above. 
XK-zerosuperior               = #x1002070  ; U+2070 SUPERSCRIPT ZERO 
XK-foursuperior               = #x1002074  ; U+2074 SUPERSCRIPT FOUR 
XK-fivesuperior               = #x1002075  ; U+2075 SUPERSCRIPT FIVE 
XK-sixsuperior                = #x1002076  ; U+2076 SUPERSCRIPT SIX 
XK-sevensuperior              = #x1002077  ; U+2077 SUPERSCRIPT SEVEN 
XK-eightsuperior              = #x1002078  ; U+2078 SUPERSCRIPT EIGHT 
XK-ninesuperior               = #x1002079  ; U+2079 SUPERSCRIPT NINE 
XK-zerosubscript              = #x1002080  ; U+2080 SUBSCRIPT ZERO 
XK-onesubscript               = #x1002081  ; U+2081 SUBSCRIPT ONE 
XK-twosubscript               = #x1002082  ; U+2082 SUBSCRIPT TWO 
XK-threesubscript             = #x1002083  ; U+2083 SUBSCRIPT THREE 
XK-foursubscript              = #x1002084  ; U+2084 SUBSCRIPT FOUR 
XK-fivesubscript              = #x1002085  ; U+2085 SUBSCRIPT FIVE 
XK-sixsubscript               = #x1002086  ; U+2086 SUBSCRIPT SIX 
XK-sevensubscript             = #x1002087  ; U+2087 SUBSCRIPT SEVEN 
XK-eightsubscript             = #x1002088  ; U+2088 SUBSCRIPT EIGHT 
XK-ninesubscript              = #x1002089  ; U+2089 SUBSCRIPT NINE 
XK-partdifferential           = #x1002202  ; U+2202 PARTIAL DIFFERENTIAL 
XK-emptyset                   = #x1002205  ; U+2205 NULL SET 
XK-elementof                  = #x1002208  ; U+2208 ELEMENT OF 
XK-notelementof               = #x1002209  ; U+2209 NOT AN ELEMENT OF 
XK-containsas                 = #x100220B  ; U+220B CONTAINS AS MEMBER 
XK-squareroot                 = #x100221A  ; U+221A SQUARE ROOT 
XK-cuberoot                   = #x100221B  ; U+221B CUBE ROOT 
XK-fourthroot                 = #x100221C  ; U+221C FOURTH ROOT 
XK-dintegral                  = #x100222C  ; U+222C DOUBLE INTEGRAL 
XK-tintegral                  = #x100222D  ; U+222D TRIPLE INTEGRAL 
XK-because                    = #x1002235  ; U+2235 BECAUSE 
XK-approxeq                   = #x1002248  ; U+2245 ALMOST EQUAL TO 
XK-notapproxeq                = #x1002247  ; U+2247 NOT ALMOST EQUAL TO 
XK-notidentical               = #x1002262  ; U+2262 NOT IDENTICAL TO 
XK-stricteq                   = #x1002263  ; U+2263 STRICTLY EQUIVALENT TO           
;#endif ; XK-MATHEMATICAL 

;#ifdef XK-BRAILLE
XK-braille-dot-1                 = #xfff1
XK-braille-dot-2                 = #xfff2
XK-braille-dot-3                 = #xfff3
XK-braille-dot-4                 = #xfff4
XK-braille-dot-5                 = #xfff5
XK-braille-dot-6                 = #xfff6
XK-braille-dot-7                 = #xfff7
XK-braille-dot-8                 = #xfff8
XK-braille-dot-9                 = #xfff9
XK-braille-dot-10                = #xfffa
XK-braille-blank              = #x1002800  ; U+2800 BRAILLE PATTERN BLANK 
XK-braille-dots-1             = #x1002801  ; U+2801 BRAILLE PATTERN DOTS-1 
XK-braille-dots-2             = #x1002802  ; U+2802 BRAILLE PATTERN DOTS-2 
XK-braille-dots-12            = #x1002803  ; U+2803 BRAILLE PATTERN DOTS-12 
XK-braille-dots-3             = #x1002804  ; U+2804 BRAILLE PATTERN DOTS-3 
XK-braille-dots-13            = #x1002805  ; U+2805 BRAILLE PATTERN DOTS-13 
XK-braille-dots-23            = #x1002806  ; U+2806 BRAILLE PATTERN DOTS-23 
XK-braille-dots-123           = #x1002807  ; U+2807 BRAILLE PATTERN DOTS-123 
XK-braille-dots-4             = #x1002808  ; U+2808 BRAILLE PATTERN DOTS-4 
XK-braille-dots-14            = #x1002809  ; U+2809 BRAILLE PATTERN DOTS-14 
XK-braille-dots-24            = #x100280a  ; U+280a BRAILLE PATTERN DOTS-24 
XK-braille-dots-124           = #x100280b  ; U+280b BRAILLE PATTERN DOTS-124 
XK-braille-dots-34            = #x100280c  ; U+280c BRAILLE PATTERN DOTS-34 
XK-braille-dots-134           = #x100280d  ; U+280d BRAILLE PATTERN DOTS-134 
XK-braille-dots-234           = #x100280e  ; U+280e BRAILLE PATTERN DOTS-234 
XK-braille-dots-1234          = #x100280f  ; U+280f BRAILLE PATTERN DOTS-1234 
XK-braille-dots-5             = #x1002810  ; U+2810 BRAILLE PATTERN DOTS-5 
XK-braille-dots-15            = #x1002811  ; U+2811 BRAILLE PATTERN DOTS-15 
XK-braille-dots-25            = #x1002812  ; U+2812 BRAILLE PATTERN DOTS-25 
XK-braille-dots-125           = #x1002813  ; U+2813 BRAILLE PATTERN DOTS-125 
XK-braille-dots-35            = #x1002814  ; U+2814 BRAILLE PATTERN DOTS-35 
XK-braille-dots-135           = #x1002815  ; U+2815 BRAILLE PATTERN DOTS-135 
XK-braille-dots-235           = #x1002816  ; U+2816 BRAILLE PATTERN DOTS-235 
XK-braille-dots-1235          = #x1002817  ; U+2817 BRAILLE PATTERN DOTS-1235 
XK-braille-dots-45            = #x1002818  ; U+2818 BRAILLE PATTERN DOTS-45 
XK-braille-dots-145           = #x1002819  ; U+2819 BRAILLE PATTERN DOTS-145 
XK-braille-dots-245           = #x100281a  ; U+281a BRAILLE PATTERN DOTS-245 
XK-braille-dots-1245          = #x100281b  ; U+281b BRAILLE PATTERN DOTS-1245 
XK-braille-dots-345           = #x100281c  ; U+281c BRAILLE PATTERN DOTS-345 
XK-braille-dots-1345          = #x100281d  ; U+281d BRAILLE PATTERN DOTS-1345 
XK-braille-dots-2345          = #x100281e  ; U+281e BRAILLE PATTERN DOTS-2345 
XK-braille-dots-12345         = #x100281f  ; U+281f BRAILLE PATTERN DOTS-12345 
XK-braille-dots-6             = #x1002820  ; U+2820 BRAILLE PATTERN DOTS-6 
XK-braille-dots-16            = #x1002821  ; U+2821 BRAILLE PATTERN DOTS-16 
XK-braille-dots-26            = #x1002822  ; U+2822 BRAILLE PATTERN DOTS-26 
XK-braille-dots-126           = #x1002823  ; U+2823 BRAILLE PATTERN DOTS-126 
XK-braille-dots-36            = #x1002824  ; U+2824 BRAILLE PATTERN DOTS-36 
XK-braille-dots-136           = #x1002825  ; U+2825 BRAILLE PATTERN DOTS-136 
XK-braille-dots-236           = #x1002826  ; U+2826 BRAILLE PATTERN DOTS-236 
XK-braille-dots-1236          = #x1002827  ; U+2827 BRAILLE PATTERN DOTS-1236 
XK-braille-dots-46            = #x1002828  ; U+2828 BRAILLE PATTERN DOTS-46 
XK-braille-dots-146           = #x1002829  ; U+2829 BRAILLE PATTERN DOTS-146 
XK-braille-dots-246           = #x100282a  ; U+282a BRAILLE PATTERN DOTS-246 
XK-braille-dots-1246          = #x100282b  ; U+282b BRAILLE PATTERN DOTS-1246 
XK-braille-dots-346           = #x100282c  ; U+282c BRAILLE PATTERN DOTS-346 
XK-braille-dots-1346          = #x100282d  ; U+282d BRAILLE PATTERN DOTS-1346 
XK-braille-dots-2346          = #x100282e  ; U+282e BRAILLE PATTERN DOTS-2346 
XK-braille-dots-12346         = #x100282f  ; U+282f BRAILLE PATTERN DOTS-12346 
XK-braille-dots-56            = #x1002830  ; U+2830 BRAILLE PATTERN DOTS-56 
XK-braille-dots-156           = #x1002831  ; U+2831 BRAILLE PATTERN DOTS-156 
XK-braille-dots-256           = #x1002832  ; U+2832 BRAILLE PATTERN DOTS-256 
XK-braille-dots-1256          = #x1002833  ; U+2833 BRAILLE PATTERN DOTS-1256 
XK-braille-dots-356           = #x1002834  ; U+2834 BRAILLE PATTERN DOTS-356 
XK-braille-dots-1356          = #x1002835  ; U+2835 BRAILLE PATTERN DOTS-1356 
XK-braille-dots-2356          = #x1002836  ; U+2836 BRAILLE PATTERN DOTS-2356 
XK-braille-dots-12356         = #x1002837  ; U+2837 BRAILLE PATTERN DOTS-12356 
XK-braille-dots-456           = #x1002838  ; U+2838 BRAILLE PATTERN DOTS-456 
XK-braille-dots-1456          = #x1002839  ; U+2839 BRAILLE PATTERN DOTS-1456 
XK-braille-dots-2456          = #x100283a  ; U+283a BRAILLE PATTERN DOTS-2456 
XK-braille-dots-12456         = #x100283b  ; U+283b BRAILLE PATTERN DOTS-12456 
XK-braille-dots-3456          = #x100283c  ; U+283c BRAILLE PATTERN DOTS-3456 
XK-braille-dots-13456         = #x100283d  ; U+283d BRAILLE PATTERN DOTS-13456 
XK-braille-dots-23456         = #x100283e  ; U+283e BRAILLE PATTERN DOTS-23456 
XK-braille-dots-123456        = #x100283f  ; U+283f BRAILLE PATTERN DOTS-123456 
XK-braille-dots-7             = #x1002840  ; U+2840 BRAILLE PATTERN DOTS-7 
XK-braille-dots-17            = #x1002841  ; U+2841 BRAILLE PATTERN DOTS-17 
XK-braille-dots-27            = #x1002842  ; U+2842 BRAILLE PATTERN DOTS-27 
XK-braille-dots-127           = #x1002843  ; U+2843 BRAILLE PATTERN DOTS-127 
XK-braille-dots-37            = #x1002844  ; U+2844 BRAILLE PATTERN DOTS-37 
XK-braille-dots-137           = #x1002845  ; U+2845 BRAILLE PATTERN DOTS-137 
XK-braille-dots-237           = #x1002846  ; U+2846 BRAILLE PATTERN DOTS-237 
XK-braille-dots-1237          = #x1002847  ; U+2847 BRAILLE PATTERN DOTS-1237 
XK-braille-dots-47            = #x1002848  ; U+2848 BRAILLE PATTERN DOTS-47 
XK-braille-dots-147           = #x1002849  ; U+2849 BRAILLE PATTERN DOTS-147 
XK-braille-dots-247           = #x100284a  ; U+284a BRAILLE PATTERN DOTS-247 
XK-braille-dots-1247          = #x100284b  ; U+284b BRAILLE PATTERN DOTS-1247 
XK-braille-dots-347           = #x100284c  ; U+284c BRAILLE PATTERN DOTS-347 
XK-braille-dots-1347          = #x100284d  ; U+284d BRAILLE PATTERN DOTS-1347 
XK-braille-dots-2347          = #x100284e  ; U+284e BRAILLE PATTERN DOTS-2347 
XK-braille-dots-12347         = #x100284f  ; U+284f BRAILLE PATTERN DOTS-12347 
XK-braille-dots-57            = #x1002850  ; U+2850 BRAILLE PATTERN DOTS-57 
XK-braille-dots-157           = #x1002851  ; U+2851 BRAILLE PATTERN DOTS-157 
XK-braille-dots-257           = #x1002852  ; U+2852 BRAILLE PATTERN DOTS-257 
XK-braille-dots-1257          = #x1002853  ; U+2853 BRAILLE PATTERN DOTS-1257 
XK-braille-dots-357           = #x1002854  ; U+2854 BRAILLE PATTERN DOTS-357 
XK-braille-dots-1357          = #x1002855  ; U+2855 BRAILLE PATTERN DOTS-1357 
XK-braille-dots-2357          = #x1002856  ; U+2856 BRAILLE PATTERN DOTS-2357 
XK-braille-dots-12357         = #x1002857  ; U+2857 BRAILLE PATTERN DOTS-12357 
XK-braille-dots-457           = #x1002858  ; U+2858 BRAILLE PATTERN DOTS-457 
XK-braille-dots-1457          = #x1002859  ; U+2859 BRAILLE PATTERN DOTS-1457 
XK-braille-dots-2457          = #x100285a  ; U+285a BRAILLE PATTERN DOTS-2457 
XK-braille-dots-12457         = #x100285b  ; U+285b BRAILLE PATTERN DOTS-12457 
XK-braille-dots-3457          = #x100285c  ; U+285c BRAILLE PATTERN DOTS-3457 
XK-braille-dots-13457         = #x100285d  ; U+285d BRAILLE PATTERN DOTS-13457 
XK-braille-dots-23457         = #x100285e  ; U+285e BRAILLE PATTERN DOTS-23457 
XK-braille-dots-123457        = #x100285f  ; U+285f BRAILLE PATTERN DOTS-123457 
XK-braille-dots-67            = #x1002860  ; U+2860 BRAILLE PATTERN DOTS-67 
XK-braille-dots-167           = #x1002861  ; U+2861 BRAILLE PATTERN DOTS-167 
XK-braille-dots-267           = #x1002862  ; U+2862 BRAILLE PATTERN DOTS-267 
XK-braille-dots-1267          = #x1002863  ; U+2863 BRAILLE PATTERN DOTS-1267 
XK-braille-dots-367           = #x1002864  ; U+2864 BRAILLE PATTERN DOTS-367 
XK-braille-dots-1367          = #x1002865  ; U+2865 BRAILLE PATTERN DOTS-1367 
XK-braille-dots-2367          = #x1002866  ; U+2866 BRAILLE PATTERN DOTS-2367 
XK-braille-dots-12367         = #x1002867  ; U+2867 BRAILLE PATTERN DOTS-12367 
XK-braille-dots-467           = #x1002868  ; U+2868 BRAILLE PATTERN DOTS-467 
XK-braille-dots-1467          = #x1002869  ; U+2869 BRAILLE PATTERN DOTS-1467 
XK-braille-dots-2467          = #x100286a  ; U+286a BRAILLE PATTERN DOTS-2467 
XK-braille-dots-12467         = #x100286b  ; U+286b BRAILLE PATTERN DOTS-12467 
XK-braille-dots-3467          = #x100286c  ; U+286c BRAILLE PATTERN DOTS-3467 
XK-braille-dots-13467         = #x100286d  ; U+286d BRAILLE PATTERN DOTS-13467 
XK-braille-dots-23467         = #x100286e  ; U+286e BRAILLE PATTERN DOTS-23467 
XK-braille-dots-123467        = #x100286f  ; U+286f BRAILLE PATTERN DOTS-123467 
XK-braille-dots-567           = #x1002870  ; U+2870 BRAILLE PATTERN DOTS-567 
XK-braille-dots-1567          = #x1002871  ; U+2871 BRAILLE PATTERN DOTS-1567 
XK-braille-dots-2567          = #x1002872  ; U+2872 BRAILLE PATTERN DOTS-2567 
XK-braille-dots-12567         = #x1002873  ; U+2873 BRAILLE PATTERN DOTS-12567 
XK-braille-dots-3567          = #x1002874  ; U+2874 BRAILLE PATTERN DOTS-3567 
XK-braille-dots-13567         = #x1002875  ; U+2875 BRAILLE PATTERN DOTS-13567 
XK-braille-dots-23567         = #x1002876  ; U+2876 BRAILLE PATTERN DOTS-23567 
XK-braille-dots-123567        = #x1002877  ; U+2877 BRAILLE PATTERN DOTS-123567 
XK-braille-dots-4567          = #x1002878  ; U+2878 BRAILLE PATTERN DOTS-4567 
XK-braille-dots-14567         = #x1002879  ; U+2879 BRAILLE PATTERN DOTS-14567 
XK-braille-dots-24567         = #x100287a  ; U+287a BRAILLE PATTERN DOTS-24567 
XK-braille-dots-124567        = #x100287b  ; U+287b BRAILLE PATTERN DOTS-124567 
XK-braille-dots-34567         = #x100287c  ; U+287c BRAILLE PATTERN DOTS-34567 
XK-braille-dots-134567        = #x100287d  ; U+287d BRAILLE PATTERN DOTS-134567 
XK-braille-dots-234567        = #x100287e  ; U+287e BRAILLE PATTERN DOTS-234567 
XK-braille-dots-1234567       = #x100287f  ; U+287f BRAILLE PATTERN DOTS-1234567 
XK-braille-dots-8             = #x1002880  ; U+2880 BRAILLE PATTERN DOTS-8 
XK-braille-dots-18            = #x1002881  ; U+2881 BRAILLE PATTERN DOTS-18 
XK-braille-dots-28            = #x1002882  ; U+2882 BRAILLE PATTERN DOTS-28 
XK-braille-dots-128           = #x1002883  ; U+2883 BRAILLE PATTERN DOTS-128 
XK-braille-dots-38            = #x1002884  ; U+2884 BRAILLE PATTERN DOTS-38 
XK-braille-dots-138           = #x1002885  ; U+2885 BRAILLE PATTERN DOTS-138 
XK-braille-dots-238           = #x1002886  ; U+2886 BRAILLE PATTERN DOTS-238 
XK-braille-dots-1238          = #x1002887  ; U+2887 BRAILLE PATTERN DOTS-1238 
XK-braille-dots-48            = #x1002888  ; U+2888 BRAILLE PATTERN DOTS-48 
XK-braille-dots-148           = #x1002889  ; U+2889 BRAILLE PATTERN DOTS-148 
XK-braille-dots-248           = #x100288a  ; U+288a BRAILLE PATTERN DOTS-248 
XK-braille-dots-1248          = #x100288b  ; U+288b BRAILLE PATTERN DOTS-1248 
XK-braille-dots-348           = #x100288c  ; U+288c BRAILLE PATTERN DOTS-348 
XK-braille-dots-1348          = #x100288d  ; U+288d BRAILLE PATTERN DOTS-1348 
XK-braille-dots-2348          = #x100288e  ; U+288e BRAILLE PATTERN DOTS-2348 
XK-braille-dots-12348         = #x100288f  ; U+288f BRAILLE PATTERN DOTS-12348 
XK-braille-dots-58            = #x1002890  ; U+2890 BRAILLE PATTERN DOTS-58 
XK-braille-dots-158           = #x1002891  ; U+2891 BRAILLE PATTERN DOTS-158 
XK-braille-dots-258           = #x1002892  ; U+2892 BRAILLE PATTERN DOTS-258 
XK-braille-dots-1258          = #x1002893  ; U+2893 BRAILLE PATTERN DOTS-1258 
XK-braille-dots-358           = #x1002894  ; U+2894 BRAILLE PATTERN DOTS-358 
XK-braille-dots-1358          = #x1002895  ; U+2895 BRAILLE PATTERN DOTS-1358 
XK-braille-dots-2358          = #x1002896  ; U+2896 BRAILLE PATTERN DOTS-2358 
XK-braille-dots-12358         = #x1002897  ; U+2897 BRAILLE PATTERN DOTS-12358 
XK-braille-dots-458           = #x1002898  ; U+2898 BRAILLE PATTERN DOTS-458 
XK-braille-dots-1458          = #x1002899  ; U+2899 BRAILLE PATTERN DOTS-1458 
XK-braille-dots-2458          = #x100289a  ; U+289a BRAILLE PATTERN DOTS-2458 
XK-braille-dots-12458         = #x100289b  ; U+289b BRAILLE PATTERN DOTS-12458 
XK-braille-dots-3458          = #x100289c  ; U+289c BRAILLE PATTERN DOTS-3458 
XK-braille-dots-13458         = #x100289d  ; U+289d BRAILLE PATTERN DOTS-13458 
XK-braille-dots-23458         = #x100289e  ; U+289e BRAILLE PATTERN DOTS-23458 
XK-braille-dots-123458        = #x100289f  ; U+289f BRAILLE PATTERN DOTS-123458 
XK-braille-dots-68            = #x10028a0  ; U+28a0 BRAILLE PATTERN DOTS-68 
XK-braille-dots-168           = #x10028a1  ; U+28a1 BRAILLE PATTERN DOTS-168 
XK-braille-dots-268           = #x10028a2  ; U+28a2 BRAILLE PATTERN DOTS-268 
XK-braille-dots-1268          = #x10028a3  ; U+28a3 BRAILLE PATTERN DOTS-1268 
XK-braille-dots-368           = #x10028a4  ; U+28a4 BRAILLE PATTERN DOTS-368 
XK-braille-dots-1368          = #x10028a5  ; U+28a5 BRAILLE PATTERN DOTS-1368 
XK-braille-dots-2368          = #x10028a6  ; U+28a6 BRAILLE PATTERN DOTS-2368 
XK-braille-dots-12368         = #x10028a7  ; U+28a7 BRAILLE PATTERN DOTS-12368 
XK-braille-dots-468           = #x10028a8  ; U+28a8 BRAILLE PATTERN DOTS-468 
XK-braille-dots-1468          = #x10028a9  ; U+28a9 BRAILLE PATTERN DOTS-1468 
XK-braille-dots-2468          = #x10028aa  ; U+28aa BRAILLE PATTERN DOTS-2468 
XK-braille-dots-12468         = #x10028ab  ; U+28ab BRAILLE PATTERN DOTS-12468 
XK-braille-dots-3468          = #x10028ac  ; U+28ac BRAILLE PATTERN DOTS-3468 
XK-braille-dots-13468         = #x10028ad  ; U+28ad BRAILLE PATTERN DOTS-13468 
XK-braille-dots-23468         = #x10028ae  ; U+28ae BRAILLE PATTERN DOTS-23468 
XK-braille-dots-123468        = #x10028af  ; U+28af BRAILLE PATTERN DOTS-123468 
XK-braille-dots-568           = #x10028b0  ; U+28b0 BRAILLE PATTERN DOTS-568 
XK-braille-dots-1568          = #x10028b1  ; U+28b1 BRAILLE PATTERN DOTS-1568 
XK-braille-dots-2568          = #x10028b2  ; U+28b2 BRAILLE PATTERN DOTS-2568 
XK-braille-dots-12568         = #x10028b3  ; U+28b3 BRAILLE PATTERN DOTS-12568 
XK-braille-dots-3568          = #x10028b4  ; U+28b4 BRAILLE PATTERN DOTS-3568 
XK-braille-dots-13568         = #x10028b5  ; U+28b5 BRAILLE PATTERN DOTS-13568 
XK-braille-dots-23568         = #x10028b6  ; U+28b6 BRAILLE PATTERN DOTS-23568 
XK-braille-dots-123568        = #x10028b7  ; U+28b7 BRAILLE PATTERN DOTS-123568 
XK-braille-dots-4568          = #x10028b8  ; U+28b8 BRAILLE PATTERN DOTS-4568 
XK-braille-dots-14568         = #x10028b9  ; U+28b9 BRAILLE PATTERN DOTS-14568 
XK-braille-dots-24568         = #x10028ba  ; U+28ba BRAILLE PATTERN DOTS-24568 
XK-braille-dots-124568        = #x10028bb  ; U+28bb BRAILLE PATTERN DOTS-124568 
XK-braille-dots-34568         = #x10028bc  ; U+28bc BRAILLE PATTERN DOTS-34568 
XK-braille-dots-134568        = #x10028bd  ; U+28bd BRAILLE PATTERN DOTS-134568 
XK-braille-dots-234568        = #x10028be  ; U+28be BRAILLE PATTERN DOTS-234568 
XK-braille-dots-1234568       = #x10028bf  ; U+28bf BRAILLE PATTERN DOTS-1234568 
XK-braille-dots-78            = #x10028c0  ; U+28c0 BRAILLE PATTERN DOTS-78 
XK-braille-dots-178           = #x10028c1  ; U+28c1 BRAILLE PATTERN DOTS-178 
XK-braille-dots-278           = #x10028c2  ; U+28c2 BRAILLE PATTERN DOTS-278 
XK-braille-dots-1278          = #x10028c3  ; U+28c3 BRAILLE PATTERN DOTS-1278 
XK-braille-dots-378           = #x10028c4  ; U+28c4 BRAILLE PATTERN DOTS-378 
XK-braille-dots-1378          = #x10028c5  ; U+28c5 BRAILLE PATTERN DOTS-1378 
XK-braille-dots-2378          = #x10028c6  ; U+28c6 BRAILLE PATTERN DOTS-2378 
XK-braille-dots-12378         = #x10028c7  ; U+28c7 BRAILLE PATTERN DOTS-12378 
XK-braille-dots-478           = #x10028c8  ; U+28c8 BRAILLE PATTERN DOTS-478 
XK-braille-dots-1478          = #x10028c9  ; U+28c9 BRAILLE PATTERN DOTS-1478 
XK-braille-dots-2478          = #x10028ca  ; U+28ca BRAILLE PATTERN DOTS-2478 
XK-braille-dots-12478         = #x10028cb  ; U+28cb BRAILLE PATTERN DOTS-12478 
XK-braille-dots-3478          = #x10028cc  ; U+28cc BRAILLE PATTERN DOTS-3478 
XK-braille-dots-13478         = #x10028cd  ; U+28cd BRAILLE PATTERN DOTS-13478 
XK-braille-dots-23478         = #x10028ce  ; U+28ce BRAILLE PATTERN DOTS-23478 
XK-braille-dots-123478        = #x10028cf  ; U+28cf BRAILLE PATTERN DOTS-123478 
XK-braille-dots-578           = #x10028d0  ; U+28d0 BRAILLE PATTERN DOTS-578 
XK-braille-dots-1578          = #x10028d1  ; U+28d1 BRAILLE PATTERN DOTS-1578 
XK-braille-dots-2578          = #x10028d2  ; U+28d2 BRAILLE PATTERN DOTS-2578 
XK-braille-dots-12578         = #x10028d3  ; U+28d3 BRAILLE PATTERN DOTS-12578 
XK-braille-dots-3578          = #x10028d4  ; U+28d4 BRAILLE PATTERN DOTS-3578 
XK-braille-dots-13578         = #x10028d5  ; U+28d5 BRAILLE PATTERN DOTS-13578 
XK-braille-dots-23578         = #x10028d6  ; U+28d6 BRAILLE PATTERN DOTS-23578 
XK-braille-dots-123578        = #x10028d7  ; U+28d7 BRAILLE PATTERN DOTS-123578 
XK-braille-dots-4578          = #x10028d8  ; U+28d8 BRAILLE PATTERN DOTS-4578 
XK-braille-dots-14578         = #x10028d9  ; U+28d9 BRAILLE PATTERN DOTS-14578 
XK-braille-dots-24578         = #x10028da  ; U+28da BRAILLE PATTERN DOTS-24578 
XK-braille-dots-124578        = #x10028db  ; U+28db BRAILLE PATTERN DOTS-124578 
XK-braille-dots-34578         = #x10028dc  ; U+28dc BRAILLE PATTERN DOTS-34578 
XK-braille-dots-134578        = #x10028dd  ; U+28dd BRAILLE PATTERN DOTS-134578 
XK-braille-dots-234578        = #x10028de  ; U+28de BRAILLE PATTERN DOTS-234578 
XK-braille-dots-1234578       = #x10028df  ; U+28df BRAILLE PATTERN DOTS-1234578 
XK-braille-dots-678           = #x10028e0  ; U+28e0 BRAILLE PATTERN DOTS-678 
XK-braille-dots-1678          = #x10028e1  ; U+28e1 BRAILLE PATTERN DOTS-1678 
XK-braille-dots-2678          = #x10028e2  ; U+28e2 BRAILLE PATTERN DOTS-2678 
XK-braille-dots-12678         = #x10028e3  ; U+28e3 BRAILLE PATTERN DOTS-12678 
XK-braille-dots-3678          = #x10028e4  ; U+28e4 BRAILLE PATTERN DOTS-3678 
XK-braille-dots-13678         = #x10028e5  ; U+28e5 BRAILLE PATTERN DOTS-13678 
XK-braille-dots-23678         = #x10028e6  ; U+28e6 BRAILLE PATTERN DOTS-23678 
XK-braille-dots-123678        = #x10028e7  ; U+28e7 BRAILLE PATTERN DOTS-123678 
XK-braille-dots-4678          = #x10028e8  ; U+28e8 BRAILLE PATTERN DOTS-4678 
XK-braille-dots-14678         = #x10028e9  ; U+28e9 BRAILLE PATTERN DOTS-14678 
XK-braille-dots-24678         = #x10028ea  ; U+28ea BRAILLE PATTERN DOTS-24678 
XK-braille-dots-124678        = #x10028eb  ; U+28eb BRAILLE PATTERN DOTS-124678 
XK-braille-dots-34678         = #x10028ec  ; U+28ec BRAILLE PATTERN DOTS-34678 
XK-braille-dots-134678        = #x10028ed  ; U+28ed BRAILLE PATTERN DOTS-134678 
XK-braille-dots-234678        = #x10028ee  ; U+28ee BRAILLE PATTERN DOTS-234678 
XK-braille-dots-1234678       = #x10028ef  ; U+28ef BRAILLE PATTERN DOTS-1234678 
XK-braille-dots-5678          = #x10028f0  ; U+28f0 BRAILLE PATTERN DOTS-5678 
XK-braille-dots-15678         = #x10028f1  ; U+28f1 BRAILLE PATTERN DOTS-15678 
XK-braille-dots-25678         = #x10028f2  ; U+28f2 BRAILLE PATTERN DOTS-25678 
XK-braille-dots-125678        = #x10028f3  ; U+28f3 BRAILLE PATTERN DOTS-125678 
XK-braille-dots-35678         = #x10028f4  ; U+28f4 BRAILLE PATTERN DOTS-35678 
XK-braille-dots-135678        = #x10028f5  ; U+28f5 BRAILLE PATTERN DOTS-135678 
XK-braille-dots-235678        = #x10028f6  ; U+28f6 BRAILLE PATTERN DOTS-235678 
XK-braille-dots-1235678       = #x10028f7  ; U+28f7 BRAILLE PATTERN DOTS-1235678 
XK-braille-dots-45678         = #x10028f8  ; U+28f8 BRAILLE PATTERN DOTS-45678 
XK-braille-dots-145678        = #x10028f9  ; U+28f9 BRAILLE PATTERN DOTS-145678 
XK-braille-dots-245678        = #x10028fa  ; U+28fa BRAILLE PATTERN DOTS-245678 
XK-braille-dots-1245678       = #x10028fb  ; U+28fb BRAILLE PATTERN DOTS-1245678 
XK-braille-dots-345678        = #x10028fc  ; U+28fc BRAILLE PATTERN DOTS-345678 
XK-braille-dots-1345678       = #x10028fd  ; U+28fd BRAILLE PATTERN DOTS-1345678 
XK-braille-dots-2345678       = #x10028fe  ; U+28fe BRAILLE PATTERN DOTS-2345678 
XK-braille-dots-12345678      = #x10028ff  ; U+28ff BRAILLE PATTERN DOTS-12345678 
;#endif ; XK-BRAILLE 

#|
 * Sinhala (http://unicode.org/charts/PDF/U0D80.pdf)
 * http://www.nongnu.org/sinhala/doc/transliteration/sinhala-transliteration-6.html
 |#

;#ifdef XK-SINHALA
XK-Sinh-ng            = #x1000d82  ; U+0D82 SINHALA ANUSVARAYA 
XK-Sinh-h2            = #x1000d83  ; U+0D83 SINHALA VISARGAYA 
XK-Sinh-a             = #x1000d85  ; U+0D85 SINHALA AYANNA 
XK-Sinh-aa            = #x1000d86  ; U+0D86 SINHALA AAYANNA 
XK-Sinh-ae            = #x1000d87  ; U+0D87 SINHALA AEYANNA 
XK-Sinh-aee           = #x1000d88  ; U+0D88 SINHALA AEEYANNA 
XK-Sinh-i             = #x1000d89  ; U+0D89 SINHALA IYANNA 
XK-Sinh-ii            = #x1000d8a  ; U+0D8A SINHALA IIYANNA 
XK-Sinh-u             = #x1000d8b  ; U+0D8B SINHALA UYANNA 
XK-Sinh-uu            = #x1000d8c  ; U+0D8C SINHALA UUYANNA 
XK-Sinh-ri            = #x1000d8d  ; U+0D8D SINHALA IRUYANNA 
XK-Sinh-rii           = #x1000d8e  ; U+0D8E SINHALA IRUUYANNA 
XK-Sinh-lu            = #x1000d8f  ; U+0D8F SINHALA ILUYANNA 
XK-Sinh-luu           = #x1000d90  ; U+0D90 SINHALA ILUUYANNA 
XK-Sinh-e             = #x1000d91  ; U+0D91 SINHALA EYANNA 
XK-Sinh-ee            = #x1000d92  ; U+0D92 SINHALA EEYANNA 
XK-Sinh-ai            = #x1000d93  ; U+0D93 SINHALA AIYANNA 
XK-Sinh-o             = #x1000d94  ; U+0D94 SINHALA OYANNA 
XK-Sinh-oo            = #x1000d95  ; U+0D95 SINHALA OOYANNA 
XK-Sinh-au            = #x1000d96  ; U+0D96 SINHALA AUYANNA 
XK-Sinh-ka            = #x1000d9a  ; U+0D9A SINHALA KAYANNA 
XK-Sinh-kha           = #x1000d9b  ; U+0D9B SINHALA MAHA. KAYANNA 
XK-Sinh-ga            = #x1000d9c  ; U+0D9C SINHALA GAYANNA 
XK-Sinh-gha           = #x1000d9d  ; U+0D9D SINHALA MAHA. GAYANNA 
XK-Sinh-ng2           = #x1000d9e  ; U+0D9E SINHALA KANTAJA NAASIKYAYA 
XK-Sinh-nga           = #x1000d9f  ; U+0D9F SINHALA SANYAKA GAYANNA 
XK-Sinh-ca            = #x1000da0  ; U+0DA0 SINHALA CAYANNA 
XK-Sinh-cha           = #x1000da1  ; U+0DA1 SINHALA MAHA. CAYANNA 
XK-Sinh-ja            = #x1000da2  ; U+0DA2 SINHALA JAYANNA 
XK-Sinh-jha           = #x1000da3  ; U+0DA3 SINHALA MAHA. JAYANNA 
XK-Sinh-nya           = #x1000da4  ; U+0DA4 SINHALA TAALUJA NAASIKYAYA 
XK-Sinh-jnya          = #x1000da5  ; U+0DA5 SINHALA TAALUJA SANYOOGA NAASIKYAYA 
XK-Sinh-nja           = #x1000da6  ; U+0DA6 SINHALA SANYAKA JAYANNA 
XK-Sinh-tta           = #x1000da7  ; U+0DA7 SINHALA TTAYANNA 
XK-Sinh-ttha          = #x1000da8  ; U+0DA8 SINHALA MAHA. TTAYANNA 
XK-Sinh-dda           = #x1000da9  ; U+0DA9 SINHALA DDAYANNA 
XK-Sinh-ddha          = #x1000daa  ; U+0DAA SINHALA MAHA. DDAYANNA 
XK-Sinh-nna           = #x1000dab  ; U+0DAB SINHALA MUURDHAJA NAYANNA 
XK-Sinh-ndda          = #x1000dac  ; U+0DAC SINHALA SANYAKA DDAYANNA 
XK-Sinh-tha           = #x1000dad  ; U+0DAD SINHALA TAYANNA 
XK-Sinh-thha          = #x1000dae  ; U+0DAE SINHALA MAHA. TAYANNA 
XK-Sinh-dha           = #x1000daf  ; U+0DAF SINHALA DAYANNA 
XK-Sinh-dhha          = #x1000db0  ; U+0DB0 SINHALA MAHA. DAYANNA 
XK-Sinh-na            = #x1000db1  ; U+0DB1 SINHALA DANTAJA NAYANNA 
XK-Sinh-ndha          = #x1000db3  ; U+0DB3 SINHALA SANYAKA DAYANNA 
XK-Sinh-pa            = #x1000db4  ; U+0DB4 SINHALA PAYANNA 
XK-Sinh-pha           = #x1000db5  ; U+0DB5 SINHALA MAHA. PAYANNA 
XK-Sinh-ba            = #x1000db6  ; U+0DB6 SINHALA BAYANNA 
XK-Sinh-bha           = #x1000db7  ; U+0DB7 SINHALA MAHA. BAYANNA 
XK-Sinh-ma            = #x1000db8  ; U+0DB8 SINHALA MAYANNA 
XK-Sinh-mba           = #x1000db9  ; U+0DB9 SINHALA AMBA BAYANNA 
XK-Sinh-ya            = #x1000dba  ; U+0DBA SINHALA YAYANNA 
XK-Sinh-ra            = #x1000dbb  ; U+0DBB SINHALA RAYANNA 
XK-Sinh-la            = #x1000dbd  ; U+0DBD SINHALA DANTAJA LAYANNA 
XK-Sinh-va            = #x1000dc0  ; U+0DC0 SINHALA VAYANNA 
XK-Sinh-sha           = #x1000dc1  ; U+0DC1 SINHALA TAALUJA SAYANNA 
XK-Sinh-ssha          = #x1000dc2  ; U+0DC2 SINHALA MUURDHAJA SAYANNA 
XK-Sinh-sa            = #x1000dc3  ; U+0DC3 SINHALA DANTAJA SAYANNA 
XK-Sinh-ha            = #x1000dc4  ; U+0DC4 SINHALA HAYANNA 
XK-Sinh-lla           = #x1000dc5  ; U+0DC5 SINHALA MUURDHAJA LAYANNA 
XK-Sinh-fa            = #x1000dc6  ; U+0DC6 SINHALA FAYANNA 
XK-Sinh-al            = #x1000dca  ; U+0DCA SINHALA AL-LAKUNA 
XK-Sinh-aa2           = #x1000dcf  ; U+0DCF SINHALA AELA-PILLA 
XK-Sinh-ae2           = #x1000dd0  ; U+0DD0 SINHALA AEDA-PILLA 
XK-Sinh-aee2          = #x1000dd1  ; U+0DD1 SINHALA DIGA AEDA-PILLA 
XK-Sinh-i2            = #x1000dd2  ; U+0DD2 SINHALA IS-PILLA 
XK-Sinh-ii2           = #x1000dd3  ; U+0DD3 SINHALA DIGA IS-PILLA 
XK-Sinh-u2            = #x1000dd4  ; U+0DD4 SINHALA PAA-PILLA 
XK-Sinh-uu2           = #x1000dd6  ; U+0DD6 SINHALA DIGA PAA-PILLA 
XK-Sinh-ru2           = #x1000dd8  ; U+0DD8 SINHALA GAETTA-PILLA 
XK-Sinh-e2            = #x1000dd9  ; U+0DD9 SINHALA KOMBUVA 
XK-Sinh-ee2           = #x1000dda  ; U+0DDA SINHALA DIGA KOMBUVA 
XK-Sinh-ai2           = #x1000ddb  ; U+0DDB SINHALA KOMBU DEKA 
XK-Sinh-o2            = #x1000ddc  ; U+0DDC SINHALA KOMBUVA HAA AELA-PILLA
XK-Sinh-oo2           = #x1000ddd  ; U+0DDD SINHALA KOMBUVA HAA DIGA AELA-PILLA
XK-Sinh-au2           = #x1000dde  ; U+0DDE SINHALA KOMBUVA HAA GAYANUKITTA 
XK-Sinh-lu2           = #x1000ddf  ; U+0DDF SINHALA GAYANUKITTA 
XK-Sinh-ruu2          = #x1000df2  ; U+0DF2 SINHALA DIGA GAETTA-PILLA 
XK-Sinh-luu2          = #x1000df3  ; U+0DF3 SINHALA DIGA GAYANUKITTA 
XK-Sinh-kunddaliya    = #x1000df4  ; U+0DF4 SINHALA KUNDDALIYA 
;#endif ; XK-SINHALA 
     )
   _ulong))
           