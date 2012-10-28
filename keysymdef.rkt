#lang racket

(provide (all-defined-out))

;;; from keysymdef.h
;;; Author: Laurent Orseau -- laurent orseau gmail com

;;; see also keysymtype.rkt

#| 
Generated with (except the -enum definition header):

sed -E /usr/include/X11/keysymdef.h \
-e 's@/\*(.*)\*/@;\1@' \
-e 's@/\*@#|@g' \
-e 's@\*/@|#@g' \
-e 's/^#define\s*(\S*\s*)0x(\S*)/(define \1#x\2)/' \
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
 *    /^\#define XK_([a-zA-Z_0-9]+)\s+0x([0-9a-f]+)\s*\/\* U+([0-9A-F]{4,6}) (.*) \*\/\s*$/
 *    /^\#define XK_([a-zA-Z_0-9]+)\s+0x([0-9a-f]+)\s*\/\*\(U+([0-9A-F]{4,6}) (.*)\)\*\/\s*$/
 *    /^\#define XK_([a-zA-Z_0-9]+)\s+0x([0-9a-f]+)\s*(\/\*\s*(.*)\s*\*\/)?\s*$/
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

(define XK_VoidSymbol                  #xffffff)  ; Void symbol 

;#ifdef XK_MISCELLANY
#|
 * TTY function keys, cleverly chosen to map to ASCII, for convenience of
 * programming, but could have been arbitrary (at the cost of lookup
 * tables in client code).
 |#

(define XK_BackSpace                     #xff08)  ; Back space, back char 
(define XK_Tab                           #xff09)
(define XK_Linefeed                      #xff0a)  ; Linefeed, LF 
(define XK_Clear                         #xff0b)
(define XK_Return                        #xff0d)  ; Return, enter 
(define XK_Pause                         #xff13)  ; Pause, hold 
(define XK_Scroll_Lock                   #xff14)
(define XK_Sys_Req                       #xff15)
(define XK_Escape                        #xff1b)
(define XK_Delete                        #xffff)  ; Delete, rubout 



; International & multi-key character composition 

(define XK_Multi_key                     #xff20)  ; Multi-key character compose 
(define XK_Codeinput                     #xff37)
(define XK_SingleCandidate               #xff3c)
(define XK_MultipleCandidate             #xff3d)
(define XK_PreviousCandidate             #xff3e)

; Japanese keyboard support 

(define XK_Kanji                         #xff21)  ; Kanji, Kanji convert 
(define XK_Muhenkan                      #xff22)  ; Cancel Conversion 
(define XK_Henkan_Mode                   #xff23)  ; Start/Stop Conversion 
(define XK_Henkan                        #xff23)  ; Alias for Henkan_Mode 
(define XK_Romaji                        #xff24)  ; to Romaji 
(define XK_Hiragana                      #xff25)  ; to Hiragana 
(define XK_Katakana                      #xff26)  ; to Katakana 
(define XK_Hiragana_Katakana             #xff27)  ; Hiragana/Katakana toggle 
(define XK_Zenkaku                       #xff28)  ; to Zenkaku 
(define XK_Hankaku                       #xff29)  ; to Hankaku 
(define XK_Zenkaku_Hankaku               #xff2a)  ; Zenkaku/Hankaku toggle 
(define XK_Touroku                       #xff2b)  ; Add to Dictionary 
(define XK_Massyo                        #xff2c)  ; Delete from Dictionary 
(define XK_Kana_Lock                     #xff2d)  ; Kana Lock 
(define XK_Kana_Shift                    #xff2e)  ; Kana Shift 
(define XK_Eisu_Shift                    #xff2f)  ; Alphanumeric Shift 
(define XK_Eisu_toggle                   #xff30)  ; Alphanumeric toggle 
(define XK_Kanji_Bangou                  #xff37)  ; Codeinput 
(define XK_Zen_Koho                      #xff3d)  ; Multiple/All Candidate(s) 
(define XK_Mae_Koho                      #xff3e)  ; Previous Candidate 

; 0xff31 thru 0xff3f are under XK_KOREAN 

; Cursor control & motion 

(define XK_Home                          #xff50)
(define XK_Left                          #xff51)  ; Move left, left arrow 
(define XK_Up                            #xff52)  ; Move up, up arrow 
(define XK_Right                         #xff53)  ; Move right, right arrow 
(define XK_Down                          #xff54)  ; Move down, down arrow 
(define XK_Prior                         #xff55)  ; Prior, previous 
(define XK_Page_Up                       #xff55)
(define XK_Next                          #xff56)  ; Next 
(define XK_Page_Down                     #xff56)
(define XK_End                           #xff57)  ; EOL 
(define XK_Begin                         #xff58)  ; BOL 


; Misc functions 

(define XK_Select                        #xff60)  ; Select, mark 
(define XK_Print                         #xff61)
(define XK_Execute                       #xff62)  ; Execute, run, do 
(define XK_Insert                        #xff63)  ; Insert, insert here 
(define XK_Undo                          #xff65)
(define XK_Redo                          #xff66)  ; Redo, again 
(define XK_Menu                          #xff67)
(define XK_Find                          #xff68)  ; Find, search 
(define XK_Cancel                        #xff69)  ; Cancel, stop, abort, exit 
(define XK_Help                          #xff6a)  ; Help 
(define XK_Break                         #xff6b)
(define XK_Mode_switch                   #xff7e)  ; Character set switch 
(define XK_script_switch                 #xff7e)  ; Alias for mode_switch 
(define XK_Num_Lock                      #xff7f)

; Keypad functions, keypad numbers cleverly chosen to map to ASCII 

(define XK_KP_Space                      #xff80)  ; Space 
(define XK_KP_Tab                        #xff89)
(define XK_KP_Enter                      #xff8d)  ; Enter 
(define XK_KP_F1                         #xff91)  ; PF1, KP_A, ... 
(define XK_KP_F2                         #xff92)
(define XK_KP_F3                         #xff93)
(define XK_KP_F4                         #xff94)
(define XK_KP_Home                       #xff95)
(define XK_KP_Left                       #xff96)
(define XK_KP_Up                         #xff97)
(define XK_KP_Right                      #xff98)
(define XK_KP_Down                       #xff99)
(define XK_KP_Prior                      #xff9a)
(define XK_KP_Page_Up                    #xff9a)
(define XK_KP_Next                       #xff9b)
(define XK_KP_Page_Down                  #xff9b)
(define XK_KP_End                        #xff9c)
(define XK_KP_Begin                      #xff9d)
(define XK_KP_Insert                     #xff9e)
(define XK_KP_Delete                     #xff9f)
(define XK_KP_Equal                      #xffbd)  ; Equals 
(define XK_KP_Multiply                   #xffaa)
(define XK_KP_Add                        #xffab)
(define XK_KP_Separator                  #xffac)  ; Separator, often comma 
(define XK_KP_Subtract                   #xffad)
(define XK_KP_Decimal                    #xffae)
(define XK_KP_Divide                     #xffaf)

(define XK_KP_0                          #xffb0)
(define XK_KP_1                          #xffb1)
(define XK_KP_2                          #xffb2)
(define XK_KP_3                          #xffb3)
(define XK_KP_4                          #xffb4)
(define XK_KP_5                          #xffb5)
(define XK_KP_6                          #xffb6)
(define XK_KP_7                          #xffb7)
(define XK_KP_8                          #xffb8)
(define XK_KP_9                          #xffb9)



#|
 * Auxiliary functions; note the duplicate definitions for left and right
 * function keys;  Sun keyboards and a few other manufacturers have such
 * function key groups on the left and/or right sides of the keyboard.
 * We've not found a keyboard with more than 35 function keys total.
 |#

(define XK_F1                            #xffbe)
(define XK_F2                            #xffbf)
(define XK_F3                            #xffc0)
(define XK_F4                            #xffc1)
(define XK_F5                            #xffc2)
(define XK_F6                            #xffc3)
(define XK_F7                            #xffc4)
(define XK_F8                            #xffc5)
(define XK_F9                            #xffc6)
(define XK_F10                           #xffc7)
(define XK_F11                           #xffc8)
(define XK_L1                            #xffc8)
(define XK_F12                           #xffc9)
(define XK_L2                            #xffc9)
(define XK_F13                           #xffca)
(define XK_L3                            #xffca)
(define XK_F14                           #xffcb)
(define XK_L4                            #xffcb)
(define XK_F15                           #xffcc)
(define XK_L5                            #xffcc)
(define XK_F16                           #xffcd)
(define XK_L6                            #xffcd)
(define XK_F17                           #xffce)
(define XK_L7                            #xffce)
(define XK_F18                           #xffcf)
(define XK_L8                            #xffcf)
(define XK_F19                           #xffd0)
(define XK_L9                            #xffd0)
(define XK_F20                           #xffd1)
(define XK_L10                           #xffd1)
(define XK_F21                           #xffd2)
(define XK_R1                            #xffd2)
(define XK_F22                           #xffd3)
(define XK_R2                            #xffd3)
(define XK_F23                           #xffd4)
(define XK_R3                            #xffd4)
(define XK_F24                           #xffd5)
(define XK_R4                            #xffd5)
(define XK_F25                           #xffd6)
(define XK_R5                            #xffd6)
(define XK_F26                           #xffd7)
(define XK_R6                            #xffd7)
(define XK_F27                           #xffd8)
(define XK_R7                            #xffd8)
(define XK_F28                           #xffd9)
(define XK_R8                            #xffd9)
(define XK_F29                           #xffda)
(define XK_R9                            #xffda)
(define XK_F30                           #xffdb)
(define XK_R10                           #xffdb)
(define XK_F31                           #xffdc)
(define XK_R11                           #xffdc)
(define XK_F32                           #xffdd)
(define XK_R12                           #xffdd)
(define XK_F33                           #xffde)
(define XK_R13                           #xffde)
(define XK_F34                           #xffdf)
(define XK_R14                           #xffdf)
(define XK_F35                           #xffe0)
(define XK_R15                           #xffe0)

; Modifiers 

(define XK_Shift_L                       #xffe1)  ; Left shift 
(define XK_Shift_R                       #xffe2)  ; Right shift 
(define XK_Control_L                     #xffe3)  ; Left control 
(define XK_Control_R                     #xffe4)  ; Right control 
(define XK_Caps_Lock                     #xffe5)  ; Caps lock 
(define XK_Shift_Lock                    #xffe6)  ; Shift lock 

(define XK_Meta_L                        #xffe7)  ; Left meta 
(define XK_Meta_R                        #xffe8)  ; Right meta 
(define XK_Alt_L                         #xffe9)  ; Left alt 
(define XK_Alt_R                         #xffea)  ; Right alt 
(define XK_Super_L                       #xffeb)  ; Left super 
(define XK_Super_R                       #xffec)  ; Right super 
(define XK_Hyper_L                       #xffed)  ; Left hyper 
(define XK_Hyper_R                       #xffee)  ; Right hyper 
;#endif ; XK_MISCELLANY 

#|
 * Keyboard (XKB) Extension function and modifier keys
 * (from Appendix C of "The X Keyboard Extension: Protocol Specification")
 * Byte 3 = 0xfe
 |#

;#ifdef XK_XKB_KEYS
(define XK_ISO_Lock                      #xfe01)
(define XK_ISO_Level2_Latch              #xfe02)
(define XK_ISO_Level3_Shift              #xfe03)
(define XK_ISO_Level3_Latch              #xfe04)
(define XK_ISO_Level3_Lock               #xfe05)
(define XK_ISO_Level5_Shift              #xfe11)
(define XK_ISO_Level5_Latch              #xfe12)
(define XK_ISO_Level5_Lock               #xfe13)
(define XK_ISO_Group_Shift               #xff7e)  ; Alias for mode_switch 
(define XK_ISO_Group_Latch               #xfe06)
(define XK_ISO_Group_Lock                #xfe07)
(define XK_ISO_Next_Group                #xfe08)
(define XK_ISO_Next_Group_Lock           #xfe09)
(define XK_ISO_Prev_Group                #xfe0a)
(define XK_ISO_Prev_Group_Lock           #xfe0b)
(define XK_ISO_First_Group               #xfe0c)
(define XK_ISO_First_Group_Lock          #xfe0d)
(define XK_ISO_Last_Group                #xfe0e)
(define XK_ISO_Last_Group_Lock           #xfe0f)

(define XK_ISO_Left_Tab                  #xfe20)
(define XK_ISO_Move_Line_Up              #xfe21)
(define XK_ISO_Move_Line_Down            #xfe22)
(define XK_ISO_Partial_Line_Up           #xfe23)
(define XK_ISO_Partial_Line_Down         #xfe24)
(define XK_ISO_Partial_Space_Left        #xfe25)
(define XK_ISO_Partial_Space_Right       #xfe26)
(define XK_ISO_Set_Margin_Left           #xfe27)
(define XK_ISO_Set_Margin_Right          #xfe28)
(define XK_ISO_Release_Margin_Left       #xfe29)
(define XK_ISO_Release_Margin_Right      #xfe2a)
(define XK_ISO_Release_Both_Margins      #xfe2b)
(define XK_ISO_Fast_Cursor_Left          #xfe2c)
(define XK_ISO_Fast_Cursor_Right         #xfe2d)
(define XK_ISO_Fast_Cursor_Up            #xfe2e)
(define XK_ISO_Fast_Cursor_Down          #xfe2f)
(define XK_ISO_Continuous_Underline      #xfe30)
(define XK_ISO_Discontinuous_Underline   #xfe31)
(define XK_ISO_Emphasize                 #xfe32)
(define XK_ISO_Center_Object             #xfe33)
(define XK_ISO_Enter                     #xfe34)

(define XK_dead_grave                    #xfe50)
(define XK_dead_acute                    #xfe51)
(define XK_dead_circumflex               #xfe52)
(define XK_dead_tilde                    #xfe53)
(define XK_dead_perispomeni              #xfe53)  ; alias for dead_tilde 
(define XK_dead_macron                   #xfe54)
(define XK_dead_breve                    #xfe55)
(define XK_dead_abovedot                 #xfe56)
(define XK_dead_diaeresis                #xfe57)
(define XK_dead_abovering                #xfe58)
(define XK_dead_doubleacute              #xfe59)
(define XK_dead_caron                    #xfe5a)
(define XK_dead_cedilla                  #xfe5b)
(define XK_dead_ogonek                   #xfe5c)
(define XK_dead_iota                     #xfe5d)
(define XK_dead_voiced_sound             #xfe5e)
(define XK_dead_semivoiced_sound         #xfe5f)
(define XK_dead_belowdot                 #xfe60)
(define XK_dead_hook                     #xfe61)
(define XK_dead_horn                     #xfe62)
(define XK_dead_stroke                   #xfe63)
(define XK_dead_abovecomma               #xfe64)
(define XK_dead_psili                    #xfe64)  ; alias for dead_abovecomma 
(define XK_dead_abovereversedcomma       #xfe65)
(define XK_dead_dasia                    #xfe65)  ; alias for dead_abovereversedcomma 
(define XK_dead_doublegrave              #xfe66)
(define XK_dead_belowring                #xfe67)
(define XK_dead_belowmacron              #xfe68)
(define XK_dead_belowcircumflex          #xfe69)
(define XK_dead_belowtilde               #xfe6a)
(define XK_dead_belowbreve               #xfe6b)
(define XK_dead_belowdiaeresis           #xfe6c)
(define XK_dead_invertedbreve            #xfe6d)
(define XK_dead_belowcomma               #xfe6e)
(define XK_dead_currency                 #xfe6f)

; dead vowels for universal syllable entry 
(define XK_dead_a                        #xfe80)
(define XK_dead_A                        #xfe81)
(define XK_dead_e                        #xfe82)
(define XK_dead_E                        #xfe83)
(define XK_dead_i                        #xfe84)
(define XK_dead_I                        #xfe85)
(define XK_dead_o                        #xfe86)
(define XK_dead_O                        #xfe87)
(define XK_dead_u                        #xfe88)
(define XK_dead_U                        #xfe89)
(define XK_dead_small_schwa              #xfe8a)
(define XK_dead_capital_schwa            #xfe8b)

(define XK_First_Virtual_Screen          #xfed0)
(define XK_Prev_Virtual_Screen           #xfed1)
(define XK_Next_Virtual_Screen           #xfed2)
(define XK_Last_Virtual_Screen           #xfed4)
(define XK_Terminate_Server              #xfed5)

(define XK_AccessX_Enable                #xfe70)
(define XK_AccessX_Feedback_Enable       #xfe71)
(define XK_RepeatKeys_Enable             #xfe72)
(define XK_SlowKeys_Enable               #xfe73)
(define XK_BounceKeys_Enable             #xfe74)
(define XK_StickyKeys_Enable             #xfe75)
(define XK_MouseKeys_Enable              #xfe76)
(define XK_MouseKeys_Accel_Enable        #xfe77)
(define XK_Overlay1_Enable               #xfe78)
(define XK_Overlay2_Enable               #xfe79)
(define XK_AudibleBell_Enable            #xfe7a)

(define XK_Pointer_Left                  #xfee0)
(define XK_Pointer_Right                 #xfee1)
(define XK_Pointer_Up                    #xfee2)
(define XK_Pointer_Down                  #xfee3)
(define XK_Pointer_UpLeft                #xfee4)
(define XK_Pointer_UpRight               #xfee5)
(define XK_Pointer_DownLeft              #xfee6)
(define XK_Pointer_DownRight             #xfee7)
(define XK_Pointer_Button_Dflt           #xfee8)
(define XK_Pointer_Button1               #xfee9)
(define XK_Pointer_Button2               #xfeea)
(define XK_Pointer_Button3               #xfeeb)
(define XK_Pointer_Button4               #xfeec)
(define XK_Pointer_Button5               #xfeed)
(define XK_Pointer_DblClick_Dflt         #xfeee)
(define XK_Pointer_DblClick1             #xfeef)
(define XK_Pointer_DblClick2             #xfef0)
(define XK_Pointer_DblClick3             #xfef1)
(define XK_Pointer_DblClick4             #xfef2)
(define XK_Pointer_DblClick5             #xfef3)
(define XK_Pointer_Drag_Dflt             #xfef4)
(define XK_Pointer_Drag1                 #xfef5)
(define XK_Pointer_Drag2                 #xfef6)
(define XK_Pointer_Drag3                 #xfef7)
(define XK_Pointer_Drag4                 #xfef8)
(define XK_Pointer_Drag5                 #xfefd)

(define XK_Pointer_EnableKeys            #xfef9)
(define XK_Pointer_Accelerate            #xfefa)
(define XK_Pointer_DfltBtnNext           #xfefb)
(define XK_Pointer_DfltBtnPrev           #xfefc)

;#endif ; XK_XKB_KEYS 

#|
 * 3270 Terminal Keys
 * Byte 3 = 0xfd
 |#

;#ifdef XK_3270
(define XK_3270_Duplicate                #xfd01)
(define XK_3270_FieldMark                #xfd02)
(define XK_3270_Right2                   #xfd03)
(define XK_3270_Left2                    #xfd04)
(define XK_3270_BackTab                  #xfd05)
(define XK_3270_EraseEOF                 #xfd06)
(define XK_3270_EraseInput               #xfd07)
(define XK_3270_Reset                    #xfd08)
(define XK_3270_Quit                     #xfd09)
(define XK_3270_PA1                      #xfd0a)
(define XK_3270_PA2                      #xfd0b)
(define XK_3270_PA3                      #xfd0c)
(define XK_3270_Test                     #xfd0d)
(define XK_3270_Attn                     #xfd0e)
(define XK_3270_CursorBlink              #xfd0f)
(define XK_3270_AltCursor                #xfd10)
(define XK_3270_KeyClick                 #xfd11)
(define XK_3270_Jump                     #xfd12)
(define XK_3270_Ident                    #xfd13)
(define XK_3270_Rule                     #xfd14)
(define XK_3270_Copy                     #xfd15)
(define XK_3270_Play                     #xfd16)
(define XK_3270_Setup                    #xfd17)
(define XK_3270_Record                   #xfd18)
(define XK_3270_ChangeScreen             #xfd19)
(define XK_3270_DeleteWord               #xfd1a)
(define XK_3270_ExSelect                 #xfd1b)
(define XK_3270_CursorSelect             #xfd1c)
(define XK_3270_PrintScreen              #xfd1d)
(define XK_3270_Enter                    #xfd1e)
;#endif ; XK_3270 

#|
 * Latin 1
 * (ISO/IEC 8859-1 = Unicode U+0020..U+00FF)
 * Byte 3 = 0
 |#
;#ifdef XK_LATIN1
(define XK_space                         #x0020)  ; U+0020 SPACE 
(define XK_exclam                        #x0021)  ; U+0021 EXCLAMATION MARK 
(define XK_quotedbl                      #x0022)  ; U+0022 QUOTATION MARK 
(define XK_numbersign                    #x0023)  ; U+0023 NUMBER SIGN 
(define XK_dollar                        #x0024)  ; U+0024 DOLLAR SIGN 
(define XK_percent                       #x0025)  ; U+0025 PERCENT SIGN 
(define XK_ampersand                     #x0026)  ; U+0026 AMPERSAND 
(define XK_apostrophe                    #x0027)  ; U+0027 APOSTROPHE 
(define XK_quoteright                    #x0027)  ; deprecated 
(define XK_parenleft                     #x0028)  ; U+0028 LEFT PARENTHESIS 
(define XK_parenright                    #x0029)  ; U+0029 RIGHT PARENTHESIS 
(define XK_asterisk                      #x002a)  ; U+002A ASTERISK 
(define XK_plus                          #x002b)  ; U+002B PLUS SIGN 
(define XK_comma                         #x002c)  ; U+002C COMMA 
(define XK_minus                         #x002d)  ; U+002D HYPHEN-MINUS 
(define XK_period                        #x002e)  ; U+002E FULL STOP 
(define XK_slash                         #x002f)  ; U+002F SOLIDUS 
(define XK_0                             #x0030)  ; U+0030 DIGIT ZERO 
(define XK_1                             #x0031)  ; U+0031 DIGIT ONE 
(define XK_2                             #x0032)  ; U+0032 DIGIT TWO 
(define XK_3                             #x0033)  ; U+0033 DIGIT THREE 
(define XK_4                             #x0034)  ; U+0034 DIGIT FOUR 
(define XK_5                             #x0035)  ; U+0035 DIGIT FIVE 
(define XK_6                             #x0036)  ; U+0036 DIGIT SIX 
(define XK_7                             #x0037)  ; U+0037 DIGIT SEVEN 
(define XK_8                             #x0038)  ; U+0038 DIGIT EIGHT 
(define XK_9                             #x0039)  ; U+0039 DIGIT NINE 
(define XK_colon                         #x003a)  ; U+003A COLON 
(define XK_semicolon                     #x003b)  ; U+003B SEMICOLON 
(define XK_less                          #x003c)  ; U+003C LESS-THAN SIGN 
(define XK_equal                         #x003d)  ; U+003D EQUALS SIGN 
(define XK_greater                       #x003e)  ; U+003E GREATER-THAN SIGN 
(define XK_question                      #x003f)  ; U+003F QUESTION MARK 
(define XK_at                            #x0040)  ; U+0040 COMMERCIAL AT 
(define XK_A                             #x0041)  ; U+0041 LATIN CAPITAL LETTER A 
(define XK_B                             #x0042)  ; U+0042 LATIN CAPITAL LETTER B 
(define XK_C                             #x0043)  ; U+0043 LATIN CAPITAL LETTER C 
(define XK_D                             #x0044)  ; U+0044 LATIN CAPITAL LETTER D 
(define XK_E                             #x0045)  ; U+0045 LATIN CAPITAL LETTER E 
(define XK_F                             #x0046)  ; U+0046 LATIN CAPITAL LETTER F 
(define XK_G                             #x0047)  ; U+0047 LATIN CAPITAL LETTER G 
(define XK_H                             #x0048)  ; U+0048 LATIN CAPITAL LETTER H 
(define XK_I                             #x0049)  ; U+0049 LATIN CAPITAL LETTER I 
(define XK_J                             #x004a)  ; U+004A LATIN CAPITAL LETTER J 
(define XK_K                             #x004b)  ; U+004B LATIN CAPITAL LETTER K 
(define XK_L                             #x004c)  ; U+004C LATIN CAPITAL LETTER L 
(define XK_M                             #x004d)  ; U+004D LATIN CAPITAL LETTER M 
(define XK_N                             #x004e)  ; U+004E LATIN CAPITAL LETTER N 
(define XK_O                             #x004f)  ; U+004F LATIN CAPITAL LETTER O 
(define XK_P                             #x0050)  ; U+0050 LATIN CAPITAL LETTER P 
(define XK_Q                             #x0051)  ; U+0051 LATIN CAPITAL LETTER Q 
(define XK_R                             #x0052)  ; U+0052 LATIN CAPITAL LETTER R 
(define XK_S                             #x0053)  ; U+0053 LATIN CAPITAL LETTER S 
(define XK_T                             #x0054)  ; U+0054 LATIN CAPITAL LETTER T 
(define XK_U                             #x0055)  ; U+0055 LATIN CAPITAL LETTER U 
(define XK_V                             #x0056)  ; U+0056 LATIN CAPITAL LETTER V 
(define XK_W                             #x0057)  ; U+0057 LATIN CAPITAL LETTER W 
(define XK_X                             #x0058)  ; U+0058 LATIN CAPITAL LETTER X 
(define XK_Y                             #x0059)  ; U+0059 LATIN CAPITAL LETTER Y 
(define XK_Z                             #x005a)  ; U+005A LATIN CAPITAL LETTER Z 
(define XK_bracketleft                   #x005b)  ; U+005B LEFT SQUARE BRACKET 
(define XK_backslash                     #x005c)  ; U+005C REVERSE SOLIDUS 
(define XK_bracketright                  #x005d)  ; U+005D RIGHT SQUARE BRACKET 
(define XK_asciicircum                   #x005e)  ; U+005E CIRCUMFLEX ACCENT 
(define XK_underscore                    #x005f)  ; U+005F LOW LINE 
(define XK_grave                         #x0060)  ; U+0060 GRAVE ACCENT 
(define XK_quoteleft                     #x0060)  ; deprecated 
(define XK_a                             #x0061)  ; U+0061 LATIN SMALL LETTER A 
(define XK_b                             #x0062)  ; U+0062 LATIN SMALL LETTER B 
(define XK_c                             #x0063)  ; U+0063 LATIN SMALL LETTER C 
(define XK_d                             #x0064)  ; U+0064 LATIN SMALL LETTER D 
(define XK_e                             #x0065)  ; U+0065 LATIN SMALL LETTER E 
(define XK_f                             #x0066)  ; U+0066 LATIN SMALL LETTER F 
(define XK_g                             #x0067)  ; U+0067 LATIN SMALL LETTER G 
(define XK_h                             #x0068)  ; U+0068 LATIN SMALL LETTER H 
(define XK_i                             #x0069)  ; U+0069 LATIN SMALL LETTER I 
(define XK_j                             #x006a)  ; U+006A LATIN SMALL LETTER J 
(define XK_k                             #x006b)  ; U+006B LATIN SMALL LETTER K 
(define XK_l                             #x006c)  ; U+006C LATIN SMALL LETTER L 
(define XK_m                             #x006d)  ; U+006D LATIN SMALL LETTER M 
(define XK_n                             #x006e)  ; U+006E LATIN SMALL LETTER N 
(define XK_o                             #x006f)  ; U+006F LATIN SMALL LETTER O 
(define XK_p                             #x0070)  ; U+0070 LATIN SMALL LETTER P 
(define XK_q                             #x0071)  ; U+0071 LATIN SMALL LETTER Q 
(define XK_r                             #x0072)  ; U+0072 LATIN SMALL LETTER R 
(define XK_s                             #x0073)  ; U+0073 LATIN SMALL LETTER S 
(define XK_t                             #x0074)  ; U+0074 LATIN SMALL LETTER T 
(define XK_u                             #x0075)  ; U+0075 LATIN SMALL LETTER U 
(define XK_v                             #x0076)  ; U+0076 LATIN SMALL LETTER V 
(define XK_w                             #x0077)  ; U+0077 LATIN SMALL LETTER W 
(define XK_x                             #x0078)  ; U+0078 LATIN SMALL LETTER X 
(define XK_y                             #x0079)  ; U+0079 LATIN SMALL LETTER Y 
(define XK_z                             #x007a)  ; U+007A LATIN SMALL LETTER Z 
(define XK_braceleft                     #x007b)  ; U+007B LEFT CURLY BRACKET 
(define XK_bar                           #x007c)  ; U+007C VERTICAL LINE 
(define XK_braceright                    #x007d)  ; U+007D RIGHT CURLY BRACKET 
(define XK_asciitilde                    #x007e)  ; U+007E TILDE 

(define XK_nobreakspace                  #x00a0)  ; U+00A0 NO-BREAK SPACE 
(define XK_exclamdown                    #x00a1)  ; U+00A1 INVERTED EXCLAMATION MARK 
(define XK_cent                          #x00a2)  ; U+00A2 CENT SIGN 
(define XK_sterling                      #x00a3)  ; U+00A3 POUND SIGN 
(define XK_currency                      #x00a4)  ; U+00A4 CURRENCY SIGN 
(define XK_yen                           #x00a5)  ; U+00A5 YEN SIGN 
(define XK_brokenbar                     #x00a6)  ; U+00A6 BROKEN BAR 
(define XK_section                       #x00a7)  ; U+00A7 SECTION SIGN 
(define XK_diaeresis                     #x00a8)  ; U+00A8 DIAERESIS 
(define XK_copyright                     #x00a9)  ; U+00A9 COPYRIGHT SIGN 
(define XK_ordfeminine                   #x00aa)  ; U+00AA FEMININE ORDINAL INDICATOR 
(define XK_guillemotleft                 #x00ab)  ; U+00AB LEFT-POINTING DOUBLE ANGLE QUOTATION MARK 
(define XK_notsign                       #x00ac)  ; U+00AC NOT SIGN 
(define XK_hyphen                        #x00ad)  ; U+00AD SOFT HYPHEN 
(define XK_registered                    #x00ae)  ; U+00AE REGISTERED SIGN 
(define XK_macron                        #x00af)  ; U+00AF MACRON 
(define XK_degree                        #x00b0)  ; U+00B0 DEGREE SIGN 
(define XK_plusminus                     #x00b1)  ; U+00B1 PLUS-MINUS SIGN 
(define XK_twosuperior                   #x00b2)  ; U+00B2 SUPERSCRIPT TWO 
(define XK_threesuperior                 #x00b3)  ; U+00B3 SUPERSCRIPT THREE 
(define XK_acute                         #x00b4)  ; U+00B4 ACUTE ACCENT 
(define XK_mu                            #x00b5)  ; U+00B5 MICRO SIGN 
(define XK_paragraph                     #x00b6)  ; U+00B6 PILCROW SIGN 
(define XK_periodcentered                #x00b7)  ; U+00B7 MIDDLE DOT 
(define XK_cedilla                       #x00b8)  ; U+00B8 CEDILLA 
(define XK_onesuperior                   #x00b9)  ; U+00B9 SUPERSCRIPT ONE 
(define XK_masculine                     #x00ba)  ; U+00BA MASCULINE ORDINAL INDICATOR 
(define XK_guillemotright                #x00bb)  ; U+00BB RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK 
(define XK_onequarter                    #x00bc)  ; U+00BC VULGAR FRACTION ONE QUARTER 
(define XK_onehalf                       #x00bd)  ; U+00BD VULGAR FRACTION ONE HALF 
(define XK_threequarters                 #x00be)  ; U+00BE VULGAR FRACTION THREE QUARTERS 
(define XK_questiondown                  #x00bf)  ; U+00BF INVERTED QUESTION MARK 
(define XK_Agrave                        #x00c0)  ; U+00C0 LATIN CAPITAL LETTER A WITH GRAVE 
(define XK_Aacute                        #x00c1)  ; U+00C1 LATIN CAPITAL LETTER A WITH ACUTE 
(define XK_Acircumflex                   #x00c2)  ; U+00C2 LATIN CAPITAL LETTER A WITH CIRCUMFLEX 
(define XK_Atilde                        #x00c3)  ; U+00C3 LATIN CAPITAL LETTER A WITH TILDE 
(define XK_Adiaeresis                    #x00c4)  ; U+00C4 LATIN CAPITAL LETTER A WITH DIAERESIS 
(define XK_Aring                         #x00c5)  ; U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE 
(define XK_AE                            #x00c6)  ; U+00C6 LATIN CAPITAL LETTER AE 
(define XK_Ccedilla                      #x00c7)  ; U+00C7 LATIN CAPITAL LETTER C WITH CEDILLA 
(define XK_Egrave                        #x00c8)  ; U+00C8 LATIN CAPITAL LETTER E WITH GRAVE 
(define XK_Eacute                        #x00c9)  ; U+00C9 LATIN CAPITAL LETTER E WITH ACUTE 
(define XK_Ecircumflex                   #x00ca)  ; U+00CA LATIN CAPITAL LETTER E WITH CIRCUMFLEX 
(define XK_Ediaeresis                    #x00cb)  ; U+00CB LATIN CAPITAL LETTER E WITH DIAERESIS 
(define XK_Igrave                        #x00cc)  ; U+00CC LATIN CAPITAL LETTER I WITH GRAVE 
(define XK_Iacute                        #x00cd)  ; U+00CD LATIN CAPITAL LETTER I WITH ACUTE 
(define XK_Icircumflex                   #x00ce)  ; U+00CE LATIN CAPITAL LETTER I WITH CIRCUMFLEX 
(define XK_Idiaeresis                    #x00cf)  ; U+00CF LATIN CAPITAL LETTER I WITH DIAERESIS 
(define XK_ETH                           #x00d0)  ; U+00D0 LATIN CAPITAL LETTER ETH 
(define XK_Eth                           #x00d0)  ; deprecated 
(define XK_Ntilde                        #x00d1)  ; U+00D1 LATIN CAPITAL LETTER N WITH TILDE 
(define XK_Ograve                        #x00d2)  ; U+00D2 LATIN CAPITAL LETTER O WITH GRAVE 
(define XK_Oacute                        #x00d3)  ; U+00D3 LATIN CAPITAL LETTER O WITH ACUTE 
(define XK_Ocircumflex                   #x00d4)  ; U+00D4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX 
(define XK_Otilde                        #x00d5)  ; U+00D5 LATIN CAPITAL LETTER O WITH TILDE 
(define XK_Odiaeresis                    #x00d6)  ; U+00D6 LATIN CAPITAL LETTER O WITH DIAERESIS 
(define XK_multiply                      #x00d7)  ; U+00D7 MULTIPLICATION SIGN 
(define XK_Oslash                        #x00d8)  ; U+00D8 LATIN CAPITAL LETTER O WITH STROKE 
(define XK_Ooblique                      #x00d8)  ; U+00D8 LATIN CAPITAL LETTER O WITH STROKE 
(define XK_Ugrave                        #x00d9)  ; U+00D9 LATIN CAPITAL LETTER U WITH GRAVE 
(define XK_Uacute                        #x00da)  ; U+00DA LATIN CAPITAL LETTER U WITH ACUTE 
(define XK_Ucircumflex                   #x00db)  ; U+00DB LATIN CAPITAL LETTER U WITH CIRCUMFLEX 
(define XK_Udiaeresis                    #x00dc)  ; U+00DC LATIN CAPITAL LETTER U WITH DIAERESIS 
(define XK_Yacute                        #x00dd)  ; U+00DD LATIN CAPITAL LETTER Y WITH ACUTE 
(define XK_THORN                         #x00de)  ; U+00DE LATIN CAPITAL LETTER THORN 
(define XK_Thorn                         #x00de)  ; deprecated 
(define XK_ssharp                        #x00df)  ; U+00DF LATIN SMALL LETTER SHARP S 
(define XK_agrave                        #x00e0)  ; U+00E0 LATIN SMALL LETTER A WITH GRAVE 
(define XK_aacute                        #x00e1)  ; U+00E1 LATIN SMALL LETTER A WITH ACUTE 
(define XK_acircumflex                   #x00e2)  ; U+00E2 LATIN SMALL LETTER A WITH CIRCUMFLEX 
(define XK_atilde                        #x00e3)  ; U+00E3 LATIN SMALL LETTER A WITH TILDE 
(define XK_adiaeresis                    #x00e4)  ; U+00E4 LATIN SMALL LETTER A WITH DIAERESIS 
(define XK_aring                         #x00e5)  ; U+00E5 LATIN SMALL LETTER A WITH RING ABOVE 
(define XK_ae                            #x00e6)  ; U+00E6 LATIN SMALL LETTER AE 
(define XK_ccedilla                      #x00e7)  ; U+00E7 LATIN SMALL LETTER C WITH CEDILLA 
(define XK_egrave                        #x00e8)  ; U+00E8 LATIN SMALL LETTER E WITH GRAVE 
(define XK_eacute                        #x00e9)  ; U+00E9 LATIN SMALL LETTER E WITH ACUTE 
(define XK_ecircumflex                   #x00ea)  ; U+00EA LATIN SMALL LETTER E WITH CIRCUMFLEX 
(define XK_ediaeresis                    #x00eb)  ; U+00EB LATIN SMALL LETTER E WITH DIAERESIS 
(define XK_igrave                        #x00ec)  ; U+00EC LATIN SMALL LETTER I WITH GRAVE 
(define XK_iacute                        #x00ed)  ; U+00ED LATIN SMALL LETTER I WITH ACUTE 
(define XK_icircumflex                   #x00ee)  ; U+00EE LATIN SMALL LETTER I WITH CIRCUMFLEX 
(define XK_idiaeresis                    #x00ef)  ; U+00EF LATIN SMALL LETTER I WITH DIAERESIS 
(define XK_eth                           #x00f0)  ; U+00F0 LATIN SMALL LETTER ETH 
(define XK_ntilde                        #x00f1)  ; U+00F1 LATIN SMALL LETTER N WITH TILDE 
(define XK_ograve                        #x00f2)  ; U+00F2 LATIN SMALL LETTER O WITH GRAVE 
(define XK_oacute                        #x00f3)  ; U+00F3 LATIN SMALL LETTER O WITH ACUTE 
(define XK_ocircumflex                   #x00f4)  ; U+00F4 LATIN SMALL LETTER O WITH CIRCUMFLEX 
(define XK_otilde                        #x00f5)  ; U+00F5 LATIN SMALL LETTER O WITH TILDE 
(define XK_odiaeresis                    #x00f6)  ; U+00F6 LATIN SMALL LETTER O WITH DIAERESIS 
(define XK_division                      #x00f7)  ; U+00F7 DIVISION SIGN 
(define XK_oslash                        #x00f8)  ; U+00F8 LATIN SMALL LETTER O WITH STROKE 
(define XK_ooblique                      #x00f8)  ; U+00F8 LATIN SMALL LETTER O WITH STROKE 
(define XK_ugrave                        #x00f9)  ; U+00F9 LATIN SMALL LETTER U WITH GRAVE 
(define XK_uacute                        #x00fa)  ; U+00FA LATIN SMALL LETTER U WITH ACUTE 
(define XK_ucircumflex                   #x00fb)  ; U+00FB LATIN SMALL LETTER U WITH CIRCUMFLEX 
(define XK_udiaeresis                    #x00fc)  ; U+00FC LATIN SMALL LETTER U WITH DIAERESIS 
(define XK_yacute                        #x00fd)  ; U+00FD LATIN SMALL LETTER Y WITH ACUTE 
(define XK_thorn                         #x00fe)  ; U+00FE LATIN SMALL LETTER THORN 
(define XK_ydiaeresis                    #x00ff)  ; U+00FF LATIN SMALL LETTER Y WITH DIAERESIS 
;#endif ; XK_LATIN1 

#|
 * Latin 2
 * Byte 3 = 1
 |#

;#ifdef XK_LATIN2
(define XK_Aogonek                       #x01a1)  ; U+0104 LATIN CAPITAL LETTER A WITH OGONEK 
(define XK_breve                         #x01a2)  ; U+02D8 BREVE 
(define XK_Lstroke                       #x01a3)  ; U+0141 LATIN CAPITAL LETTER L WITH STROKE 
(define XK_Lcaron                        #x01a5)  ; U+013D LATIN CAPITAL LETTER L WITH CARON 
(define XK_Sacute                        #x01a6)  ; U+015A LATIN CAPITAL LETTER S WITH ACUTE 
(define XK_Scaron                        #x01a9)  ; U+0160 LATIN CAPITAL LETTER S WITH CARON 
(define XK_Scedilla                      #x01aa)  ; U+015E LATIN CAPITAL LETTER S WITH CEDILLA 
(define XK_Tcaron                        #x01ab)  ; U+0164 LATIN CAPITAL LETTER T WITH CARON 
(define XK_Zacute                        #x01ac)  ; U+0179 LATIN CAPITAL LETTER Z WITH ACUTE 
(define XK_Zcaron                        #x01ae)  ; U+017D LATIN CAPITAL LETTER Z WITH CARON 
(define XK_Zabovedot                     #x01af)  ; U+017B LATIN CAPITAL LETTER Z WITH DOT ABOVE 
(define XK_aogonek                       #x01b1)  ; U+0105 LATIN SMALL LETTER A WITH OGONEK 
(define XK_ogonek                        #x01b2)  ; U+02DB OGONEK 
(define XK_lstroke                       #x01b3)  ; U+0142 LATIN SMALL LETTER L WITH STROKE 
(define XK_lcaron                        #x01b5)  ; U+013E LATIN SMALL LETTER L WITH CARON 
(define XK_sacute                        #x01b6)  ; U+015B LATIN SMALL LETTER S WITH ACUTE 
(define XK_caron                         #x01b7)  ; U+02C7 CARON 
(define XK_scaron                        #x01b9)  ; U+0161 LATIN SMALL LETTER S WITH CARON 
(define XK_scedilla                      #x01ba)  ; U+015F LATIN SMALL LETTER S WITH CEDILLA 
(define XK_tcaron                        #x01bb)  ; U+0165 LATIN SMALL LETTER T WITH CARON 
(define XK_zacute                        #x01bc)  ; U+017A LATIN SMALL LETTER Z WITH ACUTE 
(define XK_doubleacute                   #x01bd)  ; U+02DD DOUBLE ACUTE ACCENT 
(define XK_zcaron                        #x01be)  ; U+017E LATIN SMALL LETTER Z WITH CARON 
(define XK_zabovedot                     #x01bf)  ; U+017C LATIN SMALL LETTER Z WITH DOT ABOVE 
(define XK_Racute                        #x01c0)  ; U+0154 LATIN CAPITAL LETTER R WITH ACUTE 
(define XK_Abreve                        #x01c3)  ; U+0102 LATIN CAPITAL LETTER A WITH BREVE 
(define XK_Lacute                        #x01c5)  ; U+0139 LATIN CAPITAL LETTER L WITH ACUTE 
(define XK_Cacute                        #x01c6)  ; U+0106 LATIN CAPITAL LETTER C WITH ACUTE 
(define XK_Ccaron                        #x01c8)  ; U+010C LATIN CAPITAL LETTER C WITH CARON 
(define XK_Eogonek                       #x01ca)  ; U+0118 LATIN CAPITAL LETTER E WITH OGONEK 
(define XK_Ecaron                        #x01cc)  ; U+011A LATIN CAPITAL LETTER E WITH CARON 
(define XK_Dcaron                        #x01cf)  ; U+010E LATIN CAPITAL LETTER D WITH CARON 
(define XK_Dstroke                       #x01d0)  ; U+0110 LATIN CAPITAL LETTER D WITH STROKE 
(define XK_Nacute                        #x01d1)  ; U+0143 LATIN CAPITAL LETTER N WITH ACUTE 
(define XK_Ncaron                        #x01d2)  ; U+0147 LATIN CAPITAL LETTER N WITH CARON 
(define XK_Odoubleacute                  #x01d5)  ; U+0150 LATIN CAPITAL LETTER O WITH DOUBLE ACUTE 
(define XK_Rcaron                        #x01d8)  ; U+0158 LATIN CAPITAL LETTER R WITH CARON 
(define XK_Uring                         #x01d9)  ; U+016E LATIN CAPITAL LETTER U WITH RING ABOVE 
(define XK_Udoubleacute                  #x01db)  ; U+0170 LATIN CAPITAL LETTER U WITH DOUBLE ACUTE 
(define XK_Tcedilla                      #x01de)  ; U+0162 LATIN CAPITAL LETTER T WITH CEDILLA 
(define XK_racute                        #x01e0)  ; U+0155 LATIN SMALL LETTER R WITH ACUTE 
(define XK_abreve                        #x01e3)  ; U+0103 LATIN SMALL LETTER A WITH BREVE 
(define XK_lacute                        #x01e5)  ; U+013A LATIN SMALL LETTER L WITH ACUTE 
(define XK_cacute                        #x01e6)  ; U+0107 LATIN SMALL LETTER C WITH ACUTE 
(define XK_ccaron                        #x01e8)  ; U+010D LATIN SMALL LETTER C WITH CARON 
(define XK_eogonek                       #x01ea)  ; U+0119 LATIN SMALL LETTER E WITH OGONEK 
(define XK_ecaron                        #x01ec)  ; U+011B LATIN SMALL LETTER E WITH CARON 
(define XK_dcaron                        #x01ef)  ; U+010F LATIN SMALL LETTER D WITH CARON 
(define XK_dstroke                       #x01f0)  ; U+0111 LATIN SMALL LETTER D WITH STROKE 
(define XK_nacute                        #x01f1)  ; U+0144 LATIN SMALL LETTER N WITH ACUTE 
(define XK_ncaron                        #x01f2)  ; U+0148 LATIN SMALL LETTER N WITH CARON 
(define XK_odoubleacute                  #x01f5)  ; U+0151 LATIN SMALL LETTER O WITH DOUBLE ACUTE 
(define XK_rcaron                        #x01f8)  ; U+0159 LATIN SMALL LETTER R WITH CARON 
(define XK_uring                         #x01f9)  ; U+016F LATIN SMALL LETTER U WITH RING ABOVE 
(define XK_udoubleacute                  #x01fb)  ; U+0171 LATIN SMALL LETTER U WITH DOUBLE ACUTE 
(define XK_tcedilla                      #x01fe)  ; U+0163 LATIN SMALL LETTER T WITH CEDILLA 
(define XK_abovedot                      #x01ff)  ; U+02D9 DOT ABOVE 
;#endif ; XK_LATIN2 

#|
 * Latin 3
 * Byte 3 = 2
 |#

;#ifdef XK_LATIN3
(define XK_Hstroke                       #x02a1)  ; U+0126 LATIN CAPITAL LETTER H WITH STROKE 
(define XK_Hcircumflex                   #x02a6)  ; U+0124 LATIN CAPITAL LETTER H WITH CIRCUMFLEX 
(define XK_Iabovedot                     #x02a9)  ; U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE 
(define XK_Gbreve                        #x02ab)  ; U+011E LATIN CAPITAL LETTER G WITH BREVE 
(define XK_Jcircumflex                   #x02ac)  ; U+0134 LATIN CAPITAL LETTER J WITH CIRCUMFLEX 
(define XK_hstroke                       #x02b1)  ; U+0127 LATIN SMALL LETTER H WITH STROKE 
(define XK_hcircumflex                   #x02b6)  ; U+0125 LATIN SMALL LETTER H WITH CIRCUMFLEX 
(define XK_idotless                      #x02b9)  ; U+0131 LATIN SMALL LETTER DOTLESS I 
(define XK_gbreve                        #x02bb)  ; U+011F LATIN SMALL LETTER G WITH BREVE 
(define XK_jcircumflex                   #x02bc)  ; U+0135 LATIN SMALL LETTER J WITH CIRCUMFLEX 
(define XK_Cabovedot                     #x02c5)  ; U+010A LATIN CAPITAL LETTER C WITH DOT ABOVE 
(define XK_Ccircumflex                   #x02c6)  ; U+0108 LATIN CAPITAL LETTER C WITH CIRCUMFLEX 
(define XK_Gabovedot                     #x02d5)  ; U+0120 LATIN CAPITAL LETTER G WITH DOT ABOVE 
(define XK_Gcircumflex                   #x02d8)  ; U+011C LATIN CAPITAL LETTER G WITH CIRCUMFLEX 
(define XK_Ubreve                        #x02dd)  ; U+016C LATIN CAPITAL LETTER U WITH BREVE 
(define XK_Scircumflex                   #x02de)  ; U+015C LATIN CAPITAL LETTER S WITH CIRCUMFLEX 
(define XK_cabovedot                     #x02e5)  ; U+010B LATIN SMALL LETTER C WITH DOT ABOVE 
(define XK_ccircumflex                   #x02e6)  ; U+0109 LATIN SMALL LETTER C WITH CIRCUMFLEX 
(define XK_gabovedot                     #x02f5)  ; U+0121 LATIN SMALL LETTER G WITH DOT ABOVE 
(define XK_gcircumflex                   #x02f8)  ; U+011D LATIN SMALL LETTER G WITH CIRCUMFLEX 
(define XK_ubreve                        #x02fd)  ; U+016D LATIN SMALL LETTER U WITH BREVE 
(define XK_scircumflex                   #x02fe)  ; U+015D LATIN SMALL LETTER S WITH CIRCUMFLEX 
;#endif ; XK_LATIN3 


#|
 * Latin 4
 * Byte 3 = 3
 |#

;#ifdef XK_LATIN4
(define XK_kra                           #x03a2)  ; U+0138 LATIN SMALL LETTER KRA 
(define XK_kappa                         #x03a2)  ; deprecated 
(define XK_Rcedilla                      #x03a3)  ; U+0156 LATIN CAPITAL LETTER R WITH CEDILLA 
(define XK_Itilde                        #x03a5)  ; U+0128 LATIN CAPITAL LETTER I WITH TILDE 
(define XK_Lcedilla                      #x03a6)  ; U+013B LATIN CAPITAL LETTER L WITH CEDILLA 
(define XK_Emacron                       #x03aa)  ; U+0112 LATIN CAPITAL LETTER E WITH MACRON 
(define XK_Gcedilla                      #x03ab)  ; U+0122 LATIN CAPITAL LETTER G WITH CEDILLA 
(define XK_Tslash                        #x03ac)  ; U+0166 LATIN CAPITAL LETTER T WITH STROKE 
(define XK_rcedilla                      #x03b3)  ; U+0157 LATIN SMALL LETTER R WITH CEDILLA 
(define XK_itilde                        #x03b5)  ; U+0129 LATIN SMALL LETTER I WITH TILDE 
(define XK_lcedilla                      #x03b6)  ; U+013C LATIN SMALL LETTER L WITH CEDILLA 
(define XK_emacron                       #x03ba)  ; U+0113 LATIN SMALL LETTER E WITH MACRON 
(define XK_gcedilla                      #x03bb)  ; U+0123 LATIN SMALL LETTER G WITH CEDILLA 
(define XK_tslash                        #x03bc)  ; U+0167 LATIN SMALL LETTER T WITH STROKE 
(define XK_ENG                           #x03bd)  ; U+014A LATIN CAPITAL LETTER ENG 
(define XK_eng                           #x03bf)  ; U+014B LATIN SMALL LETTER ENG 
(define XK_Amacron                       #x03c0)  ; U+0100 LATIN CAPITAL LETTER A WITH MACRON 
(define XK_Iogonek                       #x03c7)  ; U+012E LATIN CAPITAL LETTER I WITH OGONEK 
(define XK_Eabovedot                     #x03cc)  ; U+0116 LATIN CAPITAL LETTER E WITH DOT ABOVE 
(define XK_Imacron                       #x03cf)  ; U+012A LATIN CAPITAL LETTER I WITH MACRON 
(define XK_Ncedilla                      #x03d1)  ; U+0145 LATIN CAPITAL LETTER N WITH CEDILLA 
(define XK_Omacron                       #x03d2)  ; U+014C LATIN CAPITAL LETTER O WITH MACRON 
(define XK_Kcedilla                      #x03d3)  ; U+0136 LATIN CAPITAL LETTER K WITH CEDILLA 
(define XK_Uogonek                       #x03d9)  ; U+0172 LATIN CAPITAL LETTER U WITH OGONEK 
(define XK_Utilde                        #x03dd)  ; U+0168 LATIN CAPITAL LETTER U WITH TILDE 
(define XK_Umacron                       #x03de)  ; U+016A LATIN CAPITAL LETTER U WITH MACRON 
(define XK_amacron                       #x03e0)  ; U+0101 LATIN SMALL LETTER A WITH MACRON 
(define XK_iogonek                       #x03e7)  ; U+012F LATIN SMALL LETTER I WITH OGONEK 
(define XK_eabovedot                     #x03ec)  ; U+0117 LATIN SMALL LETTER E WITH DOT ABOVE 
(define XK_imacron                       #x03ef)  ; U+012B LATIN SMALL LETTER I WITH MACRON 
(define XK_ncedilla                      #x03f1)  ; U+0146 LATIN SMALL LETTER N WITH CEDILLA 
(define XK_omacron                       #x03f2)  ; U+014D LATIN SMALL LETTER O WITH MACRON 
(define XK_kcedilla                      #x03f3)  ; U+0137 LATIN SMALL LETTER K WITH CEDILLA 
(define XK_uogonek                       #x03f9)  ; U+0173 LATIN SMALL LETTER U WITH OGONEK 
(define XK_utilde                        #x03fd)  ; U+0169 LATIN SMALL LETTER U WITH TILDE 
(define XK_umacron                       #x03fe)  ; U+016B LATIN SMALL LETTER U WITH MACRON 
;#endif ; XK_LATIN4 

#|
 * Latin 8
 |#
;#ifdef XK_LATIN8
(define XK_Wcircumflex                #x1000174)  ; U+0174 LATIN CAPITAL LETTER W WITH CIRCUMFLEX 
(define XK_wcircumflex                #x1000175)  ; U+0175 LATIN SMALL LETTER W WITH CIRCUMFLEX 
(define XK_Ycircumflex                #x1000176)  ; U+0176 LATIN CAPITAL LETTER Y WITH CIRCUMFLEX 
(define XK_ycircumflex                #x1000177)  ; U+0177 LATIN SMALL LETTER Y WITH CIRCUMFLEX 
(define XK_Babovedot                  #x1001e02)  ; U+1E02 LATIN CAPITAL LETTER B WITH DOT ABOVE 
(define XK_babovedot                  #x1001e03)  ; U+1E03 LATIN SMALL LETTER B WITH DOT ABOVE 
(define XK_Dabovedot                  #x1001e0a)  ; U+1E0A LATIN CAPITAL LETTER D WITH DOT ABOVE 
(define XK_dabovedot                  #x1001e0b)  ; U+1E0B LATIN SMALL LETTER D WITH DOT ABOVE 
(define XK_Fabovedot                  #x1001e1e)  ; U+1E1E LATIN CAPITAL LETTER F WITH DOT ABOVE 
(define XK_fabovedot                  #x1001e1f)  ; U+1E1F LATIN SMALL LETTER F WITH DOT ABOVE 
(define XK_Mabovedot                  #x1001e40)  ; U+1E40 LATIN CAPITAL LETTER M WITH DOT ABOVE 
(define XK_mabovedot                  #x1001e41)  ; U+1E41 LATIN SMALL LETTER M WITH DOT ABOVE 
(define XK_Pabovedot                  #x1001e56)  ; U+1E56 LATIN CAPITAL LETTER P WITH DOT ABOVE 
(define XK_pabovedot                  #x1001e57)  ; U+1E57 LATIN SMALL LETTER P WITH DOT ABOVE 
(define XK_Sabovedot                  #x1001e60)  ; U+1E60 LATIN CAPITAL LETTER S WITH DOT ABOVE 
(define XK_sabovedot                  #x1001e61)  ; U+1E61 LATIN SMALL LETTER S WITH DOT ABOVE 
(define XK_Tabovedot                  #x1001e6a)  ; U+1E6A LATIN CAPITAL LETTER T WITH DOT ABOVE 
(define XK_tabovedot                  #x1001e6b)  ; U+1E6B LATIN SMALL LETTER T WITH DOT ABOVE 
(define XK_Wgrave                     #x1001e80)  ; U+1E80 LATIN CAPITAL LETTER W WITH GRAVE 
(define XK_wgrave                     #x1001e81)  ; U+1E81 LATIN SMALL LETTER W WITH GRAVE 
(define XK_Wacute                     #x1001e82)  ; U+1E82 LATIN CAPITAL LETTER W WITH ACUTE 
(define XK_wacute                     #x1001e83)  ; U+1E83 LATIN SMALL LETTER W WITH ACUTE 
(define XK_Wdiaeresis                 #x1001e84)  ; U+1E84 LATIN CAPITAL LETTER W WITH DIAERESIS 
(define XK_wdiaeresis                 #x1001e85)  ; U+1E85 LATIN SMALL LETTER W WITH DIAERESIS 
(define XK_Ygrave                     #x1001ef2)  ; U+1EF2 LATIN CAPITAL LETTER Y WITH GRAVE 
(define XK_ygrave                     #x1001ef3)  ; U+1EF3 LATIN SMALL LETTER Y WITH GRAVE 
;#endif ; XK_LATIN8 

#|
 * Latin 9
 * Byte 3 = 0x13
 |#

;#ifdef XK_LATIN9
(define XK_OE                            #x13bc)  ; U+0152 LATIN CAPITAL LIGATURE OE 
(define XK_oe                            #x13bd)  ; U+0153 LATIN SMALL LIGATURE OE 
(define XK_Ydiaeresis                    #x13be)  ; U+0178 LATIN CAPITAL LETTER Y WITH DIAERESIS 
;#endif ; XK_LATIN9 

#|
 * Katakana
 * Byte 3 = 4
 |#

;#ifdef XK_KATAKANA
(define XK_overline                      #x047e)  ; U+203E OVERLINE 
(define XK_kana_fullstop                 #x04a1)  ; U+3002 IDEOGRAPHIC FULL STOP 
(define XK_kana_openingbracket           #x04a2)  ; U+300C LEFT CORNER BRACKET 
(define XK_kana_closingbracket           #x04a3)  ; U+300D RIGHT CORNER BRACKET 
(define XK_kana_comma                    #x04a4)  ; U+3001 IDEOGRAPHIC COMMA 
(define XK_kana_conjunctive              #x04a5)  ; U+30FB KATAKANA MIDDLE DOT 
(define XK_kana_middledot                #x04a5)  ; deprecated 
(define XK_kana_WO                       #x04a6)  ; U+30F2 KATAKANA LETTER WO 
(define XK_kana_a                        #x04a7)  ; U+30A1 KATAKANA LETTER SMALL A 
(define XK_kana_i                        #x04a8)  ; U+30A3 KATAKANA LETTER SMALL I 
(define XK_kana_u                        #x04a9)  ; U+30A5 KATAKANA LETTER SMALL U 
(define XK_kana_e                        #x04aa)  ; U+30A7 KATAKANA LETTER SMALL E 
(define XK_kana_o                        #x04ab)  ; U+30A9 KATAKANA LETTER SMALL O 
(define XK_kana_ya                       #x04ac)  ; U+30E3 KATAKANA LETTER SMALL YA 
(define XK_kana_yu                       #x04ad)  ; U+30E5 KATAKANA LETTER SMALL YU 
(define XK_kana_yo                       #x04ae)  ; U+30E7 KATAKANA LETTER SMALL YO 
(define XK_kana_tsu                      #x04af)  ; U+30C3 KATAKANA LETTER SMALL TU 
(define XK_kana_tu                       #x04af)  ; deprecated 
(define XK_prolongedsound                #x04b0)  ; U+30FC KATAKANA-HIRAGANA PROLONGED SOUND MARK 
(define XK_kana_A                        #x04b1)  ; U+30A2 KATAKANA LETTER A 
(define XK_kana_I                        #x04b2)  ; U+30A4 KATAKANA LETTER I 
(define XK_kana_U                        #x04b3)  ; U+30A6 KATAKANA LETTER U 
(define XK_kana_E                        #x04b4)  ; U+30A8 KATAKANA LETTER E 
(define XK_kana_O                        #x04b5)  ; U+30AA KATAKANA LETTER O 
(define XK_kana_KA                       #x04b6)  ; U+30AB KATAKANA LETTER KA 
(define XK_kana_KI                       #x04b7)  ; U+30AD KATAKANA LETTER KI 
(define XK_kana_KU                       #x04b8)  ; U+30AF KATAKANA LETTER KU 
(define XK_kana_KE                       #x04b9)  ; U+30B1 KATAKANA LETTER KE 
(define XK_kana_KO                       #x04ba)  ; U+30B3 KATAKANA LETTER KO 
(define XK_kana_SA                       #x04bb)  ; U+30B5 KATAKANA LETTER SA 
(define XK_kana_SHI                      #x04bc)  ; U+30B7 KATAKANA LETTER SI 
(define XK_kana_SU                       #x04bd)  ; U+30B9 KATAKANA LETTER SU 
(define XK_kana_SE                       #x04be)  ; U+30BB KATAKANA LETTER SE 
(define XK_kana_SO                       #x04bf)  ; U+30BD KATAKANA LETTER SO 
(define XK_kana_TA                       #x04c0)  ; U+30BF KATAKANA LETTER TA 
(define XK_kana_CHI                      #x04c1)  ; U+30C1 KATAKANA LETTER TI 
(define XK_kana_TI                       #x04c1)  ; deprecated 
(define XK_kana_TSU                      #x04c2)  ; U+30C4 KATAKANA LETTER TU 
(define XK_kana_TU                       #x04c2)  ; deprecated 
(define XK_kana_TE                       #x04c3)  ; U+30C6 KATAKANA LETTER TE 
(define XK_kana_TO                       #x04c4)  ; U+30C8 KATAKANA LETTER TO 
(define XK_kana_NA                       #x04c5)  ; U+30CA KATAKANA LETTER NA 
(define XK_kana_NI                       #x04c6)  ; U+30CB KATAKANA LETTER NI 
(define XK_kana_NU                       #x04c7)  ; U+30CC KATAKANA LETTER NU 
(define XK_kana_NE                       #x04c8)  ; U+30CD KATAKANA LETTER NE 
(define XK_kana_NO                       #x04c9)  ; U+30CE KATAKANA LETTER NO 
(define XK_kana_HA                       #x04ca)  ; U+30CF KATAKANA LETTER HA 
(define XK_kana_HI                       #x04cb)  ; U+30D2 KATAKANA LETTER HI 
(define XK_kana_FU                       #x04cc)  ; U+30D5 KATAKANA LETTER HU 
(define XK_kana_HU                       #x04cc)  ; deprecated 
(define XK_kana_HE                       #x04cd)  ; U+30D8 KATAKANA LETTER HE 
(define XK_kana_HO                       #x04ce)  ; U+30DB KATAKANA LETTER HO 
(define XK_kana_MA                       #x04cf)  ; U+30DE KATAKANA LETTER MA 
(define XK_kana_MI                       #x04d0)  ; U+30DF KATAKANA LETTER MI 
(define XK_kana_MU                       #x04d1)  ; U+30E0 KATAKANA LETTER MU 
(define XK_kana_ME                       #x04d2)  ; U+30E1 KATAKANA LETTER ME 
(define XK_kana_MO                       #x04d3)  ; U+30E2 KATAKANA LETTER MO 
(define XK_kana_YA                       #x04d4)  ; U+30E4 KATAKANA LETTER YA 
(define XK_kana_YU                       #x04d5)  ; U+30E6 KATAKANA LETTER YU 
(define XK_kana_YO                       #x04d6)  ; U+30E8 KATAKANA LETTER YO 
(define XK_kana_RA                       #x04d7)  ; U+30E9 KATAKANA LETTER RA 
(define XK_kana_RI                       #x04d8)  ; U+30EA KATAKANA LETTER RI 
(define XK_kana_RU                       #x04d9)  ; U+30EB KATAKANA LETTER RU 
(define XK_kana_RE                       #x04da)  ; U+30EC KATAKANA LETTER RE 
(define XK_kana_RO                       #x04db)  ; U+30ED KATAKANA LETTER RO 
(define XK_kana_WA                       #x04dc)  ; U+30EF KATAKANA LETTER WA 
(define XK_kana_N                        #x04dd)  ; U+30F3 KATAKANA LETTER N 
(define XK_voicedsound                   #x04de)  ; U+309B KATAKANA-HIRAGANA VOICED SOUND MARK 
(define XK_semivoicedsound               #x04df)  ; U+309C KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK 
(define XK_kana_switch                   #xff7e)  ; Alias for mode_switch 
;#endif ; XK_KATAKANA 

#|
 * Arabic
 * Byte 3 = 5
 |#

;#ifdef XK_ARABIC
(define XK_Farsi_0                    #x10006f0)  ; U+06F0 EXTENDED ARABIC-INDIC DIGIT ZERO 
(define XK_Farsi_1                    #x10006f1)  ; U+06F1 EXTENDED ARABIC-INDIC DIGIT ONE 
(define XK_Farsi_2                    #x10006f2)  ; U+06F2 EXTENDED ARABIC-INDIC DIGIT TWO 
(define XK_Farsi_3                    #x10006f3)  ; U+06F3 EXTENDED ARABIC-INDIC DIGIT THREE 
(define XK_Farsi_4                    #x10006f4)  ; U+06F4 EXTENDED ARABIC-INDIC DIGIT FOUR 
(define XK_Farsi_5                    #x10006f5)  ; U+06F5 EXTENDED ARABIC-INDIC DIGIT FIVE 
(define XK_Farsi_6                    #x10006f6)  ; U+06F6 EXTENDED ARABIC-INDIC DIGIT SIX 
(define XK_Farsi_7                    #x10006f7)  ; U+06F7 EXTENDED ARABIC-INDIC DIGIT SEVEN 
(define XK_Farsi_8                    #x10006f8)  ; U+06F8 EXTENDED ARABIC-INDIC DIGIT EIGHT 
(define XK_Farsi_9                    #x10006f9)  ; U+06F9 EXTENDED ARABIC-INDIC DIGIT NINE 
(define XK_Arabic_percent             #x100066a)  ; U+066A ARABIC PERCENT SIGN 
(define XK_Arabic_superscript_alef    #x1000670)  ; U+0670 ARABIC LETTER SUPERSCRIPT ALEF 
(define XK_Arabic_tteh                #x1000679)  ; U+0679 ARABIC LETTER TTEH 
(define XK_Arabic_peh                 #x100067e)  ; U+067E ARABIC LETTER PEH 
(define XK_Arabic_tcheh               #x1000686)  ; U+0686 ARABIC LETTER TCHEH 
(define XK_Arabic_ddal                #x1000688)  ; U+0688 ARABIC LETTER DDAL 
(define XK_Arabic_rreh                #x1000691)  ; U+0691 ARABIC LETTER RREH 
(define XK_Arabic_comma                  #x05ac)  ; U+060C ARABIC COMMA 
(define XK_Arabic_fullstop            #x10006d4)  ; U+06D4 ARABIC FULL STOP 
(define XK_Arabic_0                   #x1000660)  ; U+0660 ARABIC-INDIC DIGIT ZERO 
(define XK_Arabic_1                   #x1000661)  ; U+0661 ARABIC-INDIC DIGIT ONE 
(define XK_Arabic_2                   #x1000662)  ; U+0662 ARABIC-INDIC DIGIT TWO 
(define XK_Arabic_3                   #x1000663)  ; U+0663 ARABIC-INDIC DIGIT THREE 
(define XK_Arabic_4                   #x1000664)  ; U+0664 ARABIC-INDIC DIGIT FOUR 
(define XK_Arabic_5                   #x1000665)  ; U+0665 ARABIC-INDIC DIGIT FIVE 
(define XK_Arabic_6                   #x1000666)  ; U+0666 ARABIC-INDIC DIGIT SIX 
(define XK_Arabic_7                   #x1000667)  ; U+0667 ARABIC-INDIC DIGIT SEVEN 
(define XK_Arabic_8                   #x1000668)  ; U+0668 ARABIC-INDIC DIGIT EIGHT 
(define XK_Arabic_9                   #x1000669)  ; U+0669 ARABIC-INDIC DIGIT NINE 
(define XK_Arabic_semicolon              #x05bb)  ; U+061B ARABIC SEMICOLON 
(define XK_Arabic_question_mark          #x05bf)  ; U+061F ARABIC QUESTION MARK 
(define XK_Arabic_hamza                  #x05c1)  ; U+0621 ARABIC LETTER HAMZA 
(define XK_Arabic_maddaonalef            #x05c2)  ; U+0622 ARABIC LETTER ALEF WITH MADDA ABOVE 
(define XK_Arabic_hamzaonalef            #x05c3)  ; U+0623 ARABIC LETTER ALEF WITH HAMZA ABOVE 
(define XK_Arabic_hamzaonwaw             #x05c4)  ; U+0624 ARABIC LETTER WAW WITH HAMZA ABOVE 
(define XK_Arabic_hamzaunderalef         #x05c5)  ; U+0625 ARABIC LETTER ALEF WITH HAMZA BELOW 
(define XK_Arabic_hamzaonyeh             #x05c6)  ; U+0626 ARABIC LETTER YEH WITH HAMZA ABOVE 
(define XK_Arabic_alef                   #x05c7)  ; U+0627 ARABIC LETTER ALEF 
(define XK_Arabic_beh                    #x05c8)  ; U+0628 ARABIC LETTER BEH 
(define XK_Arabic_tehmarbuta             #x05c9)  ; U+0629 ARABIC LETTER TEH MARBUTA 
(define XK_Arabic_teh                    #x05ca)  ; U+062A ARABIC LETTER TEH 
(define XK_Arabic_theh                   #x05cb)  ; U+062B ARABIC LETTER THEH 
(define XK_Arabic_jeem                   #x05cc)  ; U+062C ARABIC LETTER JEEM 
(define XK_Arabic_hah                    #x05cd)  ; U+062D ARABIC LETTER HAH 
(define XK_Arabic_khah                   #x05ce)  ; U+062E ARABIC LETTER KHAH 
(define XK_Arabic_dal                    #x05cf)  ; U+062F ARABIC LETTER DAL 
(define XK_Arabic_thal                   #x05d0)  ; U+0630 ARABIC LETTER THAL 
(define XK_Arabic_ra                     #x05d1)  ; U+0631 ARABIC LETTER REH 
(define XK_Arabic_zain                   #x05d2)  ; U+0632 ARABIC LETTER ZAIN 
(define XK_Arabic_seen                   #x05d3)  ; U+0633 ARABIC LETTER SEEN 
(define XK_Arabic_sheen                  #x05d4)  ; U+0634 ARABIC LETTER SHEEN 
(define XK_Arabic_sad                    #x05d5)  ; U+0635 ARABIC LETTER SAD 
(define XK_Arabic_dad                    #x05d6)  ; U+0636 ARABIC LETTER DAD 
(define XK_Arabic_tah                    #x05d7)  ; U+0637 ARABIC LETTER TAH 
(define XK_Arabic_zah                    #x05d8)  ; U+0638 ARABIC LETTER ZAH 
(define XK_Arabic_ain                    #x05d9)  ; U+0639 ARABIC LETTER AIN 
(define XK_Arabic_ghain                  #x05da)  ; U+063A ARABIC LETTER GHAIN 
(define XK_Arabic_tatweel                #x05e0)  ; U+0640 ARABIC TATWEEL 
(define XK_Arabic_feh                    #x05e1)  ; U+0641 ARABIC LETTER FEH 
(define XK_Arabic_qaf                    #x05e2)  ; U+0642 ARABIC LETTER QAF 
(define XK_Arabic_kaf                    #x05e3)  ; U+0643 ARABIC LETTER KAF 
(define XK_Arabic_lam                    #x05e4)  ; U+0644 ARABIC LETTER LAM 
(define XK_Arabic_meem                   #x05e5)  ; U+0645 ARABIC LETTER MEEM 
(define XK_Arabic_noon                   #x05e6)  ; U+0646 ARABIC LETTER NOON 
(define XK_Arabic_ha                     #x05e7)  ; U+0647 ARABIC LETTER HEH 
(define XK_Arabic_heh                    #x05e7)  ; deprecated 
(define XK_Arabic_waw                    #x05e8)  ; U+0648 ARABIC LETTER WAW 
(define XK_Arabic_alefmaksura            #x05e9)  ; U+0649 ARABIC LETTER ALEF MAKSURA 
(define XK_Arabic_yeh                    #x05ea)  ; U+064A ARABIC LETTER YEH 
(define XK_Arabic_fathatan               #x05eb)  ; U+064B ARABIC FATHATAN 
(define XK_Arabic_dammatan               #x05ec)  ; U+064C ARABIC DAMMATAN 
(define XK_Arabic_kasratan               #x05ed)  ; U+064D ARABIC KASRATAN 
(define XK_Arabic_fatha                  #x05ee)  ; U+064E ARABIC FATHA 
(define XK_Arabic_damma                  #x05ef)  ; U+064F ARABIC DAMMA 
(define XK_Arabic_kasra                  #x05f0)  ; U+0650 ARABIC KASRA 
(define XK_Arabic_shadda                 #x05f1)  ; U+0651 ARABIC SHADDA 
(define XK_Arabic_sukun                  #x05f2)  ; U+0652 ARABIC SUKUN 
(define XK_Arabic_madda_above         #x1000653)  ; U+0653 ARABIC MADDAH ABOVE 
(define XK_Arabic_hamza_above         #x1000654)  ; U+0654 ARABIC HAMZA ABOVE 
(define XK_Arabic_hamza_below         #x1000655)  ; U+0655 ARABIC HAMZA BELOW 
(define XK_Arabic_jeh                 #x1000698)  ; U+0698 ARABIC LETTER JEH 
(define XK_Arabic_veh                 #x10006a4)  ; U+06A4 ARABIC LETTER VEH 
(define XK_Arabic_keheh               #x10006a9)  ; U+06A9 ARABIC LETTER KEHEH 
(define XK_Arabic_gaf                 #x10006af)  ; U+06AF ARABIC LETTER GAF 
(define XK_Arabic_noon_ghunna         #x10006ba)  ; U+06BA ARABIC LETTER NOON GHUNNA 
(define XK_Arabic_heh_doachashmee     #x10006be)  ; U+06BE ARABIC LETTER HEH DOACHASHMEE 
(define XK_Farsi_yeh                  #x10006cc)  ; U+06CC ARABIC LETTER FARSI YEH 
(define XK_Arabic_farsi_yeh           #x10006cc)  ; U+06CC ARABIC LETTER FARSI YEH 
(define XK_Arabic_yeh_baree           #x10006d2)  ; U+06D2 ARABIC LETTER YEH BARREE 
(define XK_Arabic_heh_goal            #x10006c1)  ; U+06C1 ARABIC LETTER HEH GOAL 
(define XK_Arabic_switch                 #xff7e)  ; Alias for mode_switch 
;#endif ; XK_ARABIC 

#|
 * Cyrillic
 * Byte 3 = 6
 |#
;#ifdef XK_CYRILLIC
(define XK_Cyrillic_GHE_bar           #x1000492)  ; U+0492 CYRILLIC CAPITAL LETTER GHE WITH STROKE 
(define XK_Cyrillic_ghe_bar           #x1000493)  ; U+0493 CYRILLIC SMALL LETTER GHE WITH STROKE 
(define XK_Cyrillic_ZHE_descender     #x1000496)  ; U+0496 CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER 
(define XK_Cyrillic_zhe_descender     #x1000497)  ; U+0497 CYRILLIC SMALL LETTER ZHE WITH DESCENDER 
(define XK_Cyrillic_KA_descender      #x100049a)  ; U+049A CYRILLIC CAPITAL LETTER KA WITH DESCENDER 
(define XK_Cyrillic_ka_descender      #x100049b)  ; U+049B CYRILLIC SMALL LETTER KA WITH DESCENDER 
(define XK_Cyrillic_KA_vertstroke     #x100049c)  ; U+049C CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE 
(define XK_Cyrillic_ka_vertstroke     #x100049d)  ; U+049D CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE 
(define XK_Cyrillic_EN_descender      #x10004a2)  ; U+04A2 CYRILLIC CAPITAL LETTER EN WITH DESCENDER 
(define XK_Cyrillic_en_descender      #x10004a3)  ; U+04A3 CYRILLIC SMALL LETTER EN WITH DESCENDER 
(define XK_Cyrillic_U_straight        #x10004ae)  ; U+04AE CYRILLIC CAPITAL LETTER STRAIGHT U 
(define XK_Cyrillic_u_straight        #x10004af)  ; U+04AF CYRILLIC SMALL LETTER STRAIGHT U 
(define XK_Cyrillic_U_straight_bar    #x10004b0)  ; U+04B0 CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE 
(define XK_Cyrillic_u_straight_bar    #x10004b1)  ; U+04B1 CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE 
(define XK_Cyrillic_HA_descender      #x10004b2)  ; U+04B2 CYRILLIC CAPITAL LETTER HA WITH DESCENDER 
(define XK_Cyrillic_ha_descender      #x10004b3)  ; U+04B3 CYRILLIC SMALL LETTER HA WITH DESCENDER 
(define XK_Cyrillic_CHE_descender     #x10004b6)  ; U+04B6 CYRILLIC CAPITAL LETTER CHE WITH DESCENDER 
(define XK_Cyrillic_che_descender     #x10004b7)  ; U+04B7 CYRILLIC SMALL LETTER CHE WITH DESCENDER 
(define XK_Cyrillic_CHE_vertstroke    #x10004b8)  ; U+04B8 CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE 
(define XK_Cyrillic_che_vertstroke    #x10004b9)  ; U+04B9 CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE 
(define XK_Cyrillic_SHHA              #x10004ba)  ; U+04BA CYRILLIC CAPITAL LETTER SHHA 
(define XK_Cyrillic_shha              #x10004bb)  ; U+04BB CYRILLIC SMALL LETTER SHHA 

(define XK_Cyrillic_SCHWA             #x10004d8)  ; U+04D8 CYRILLIC CAPITAL LETTER SCHWA 
(define XK_Cyrillic_schwa             #x10004d9)  ; U+04D9 CYRILLIC SMALL LETTER SCHWA 
(define XK_Cyrillic_I_macron          #x10004e2)  ; U+04E2 CYRILLIC CAPITAL LETTER I WITH MACRON 
(define XK_Cyrillic_i_macron          #x10004e3)  ; U+04E3 CYRILLIC SMALL LETTER I WITH MACRON 
(define XK_Cyrillic_O_bar             #x10004e8)  ; U+04E8 CYRILLIC CAPITAL LETTER BARRED O 
(define XK_Cyrillic_o_bar             #x10004e9)  ; U+04E9 CYRILLIC SMALL LETTER BARRED O 
(define XK_Cyrillic_U_macron          #x10004ee)  ; U+04EE CYRILLIC CAPITAL LETTER U WITH MACRON 
(define XK_Cyrillic_u_macron          #x10004ef)  ; U+04EF CYRILLIC SMALL LETTER U WITH MACRON 

(define XK_Serbian_dje                   #x06a1)  ; U+0452 CYRILLIC SMALL LETTER DJE 
(define XK_Macedonia_gje                 #x06a2)  ; U+0453 CYRILLIC SMALL LETTER GJE 
(define XK_Cyrillic_io                   #x06a3)  ; U+0451 CYRILLIC SMALL LETTER IO 
(define XK_Ukrainian_ie                  #x06a4)  ; U+0454 CYRILLIC SMALL LETTER UKRAINIAN IE 
(define XK_Ukranian_je                   #x06a4)  ; deprecated 
(define XK_Macedonia_dse                 #x06a5)  ; U+0455 CYRILLIC SMALL LETTER DZE 
(define XK_Ukrainian_i                   #x06a6)  ; U+0456 CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I 
(define XK_Ukranian_i                    #x06a6)  ; deprecated 
(define XK_Ukrainian_yi                  #x06a7)  ; U+0457 CYRILLIC SMALL LETTER YI 
(define XK_Ukranian_yi                   #x06a7)  ; deprecated 
(define XK_Cyrillic_je                   #x06a8)  ; U+0458 CYRILLIC SMALL LETTER JE 
(define XK_Serbian_je                    #x06a8)  ; deprecated 
(define XK_Cyrillic_lje                  #x06a9)  ; U+0459 CYRILLIC SMALL LETTER LJE 
(define XK_Serbian_lje                   #x06a9)  ; deprecated 
(define XK_Cyrillic_nje                  #x06aa)  ; U+045A CYRILLIC SMALL LETTER NJE 
(define XK_Serbian_nje                   #x06aa)  ; deprecated 
(define XK_Serbian_tshe                  #x06ab)  ; U+045B CYRILLIC SMALL LETTER TSHE 
(define XK_Macedonia_kje                 #x06ac)  ; U+045C CYRILLIC SMALL LETTER KJE 
(define XK_Ukrainian_ghe_with_upturn     #x06ad)  ; U+0491 CYRILLIC SMALL LETTER GHE WITH UPTURN 
(define XK_Byelorussian_shortu           #x06ae)  ; U+045E CYRILLIC SMALL LETTER SHORT U 
(define XK_Cyrillic_dzhe                 #x06af)  ; U+045F CYRILLIC SMALL LETTER DZHE 
(define XK_Serbian_dze                   #x06af)  ; deprecated 
(define XK_numerosign                    #x06b0)  ; U+2116 NUMERO SIGN 
(define XK_Serbian_DJE                   #x06b1)  ; U+0402 CYRILLIC CAPITAL LETTER DJE 
(define XK_Macedonia_GJE                 #x06b2)  ; U+0403 CYRILLIC CAPITAL LETTER GJE 
(define XK_Cyrillic_IO                   #x06b3)  ; U+0401 CYRILLIC CAPITAL LETTER IO 
(define XK_Ukrainian_IE                  #x06b4)  ; U+0404 CYRILLIC CAPITAL LETTER UKRAINIAN IE 
(define XK_Ukranian_JE                   #x06b4)  ; deprecated 
(define XK_Macedonia_DSE                 #x06b5)  ; U+0405 CYRILLIC CAPITAL LETTER DZE 
(define XK_Ukrainian_I                   #x06b6)  ; U+0406 CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I 
(define XK_Ukranian_I                    #x06b6)  ; deprecated 
(define XK_Ukrainian_YI                  #x06b7)  ; U+0407 CYRILLIC CAPITAL LETTER YI 
(define XK_Ukranian_YI                   #x06b7)  ; deprecated 
(define XK_Cyrillic_JE                   #x06b8)  ; U+0408 CYRILLIC CAPITAL LETTER JE 
(define XK_Serbian_JE                    #x06b8)  ; deprecated 
(define XK_Cyrillic_LJE                  #x06b9)  ; U+0409 CYRILLIC CAPITAL LETTER LJE 
(define XK_Serbian_LJE                   #x06b9)  ; deprecated 
(define XK_Cyrillic_NJE                  #x06ba)  ; U+040A CYRILLIC CAPITAL LETTER NJE 
(define XK_Serbian_NJE                   #x06ba)  ; deprecated 
(define XK_Serbian_TSHE                  #x06bb)  ; U+040B CYRILLIC CAPITAL LETTER TSHE 
(define XK_Macedonia_KJE                 #x06bc)  ; U+040C CYRILLIC CAPITAL LETTER KJE 
(define XK_Ukrainian_GHE_WITH_UPTURN     #x06bd)  ; U+0490 CYRILLIC CAPITAL LETTER GHE WITH UPTURN 
(define XK_Byelorussian_SHORTU           #x06be)  ; U+040E CYRILLIC CAPITAL LETTER SHORT U 
(define XK_Cyrillic_DZHE                 #x06bf)  ; U+040F CYRILLIC CAPITAL LETTER DZHE 
(define XK_Serbian_DZE                   #x06bf)  ; deprecated 
(define XK_Cyrillic_yu                   #x06c0)  ; U+044E CYRILLIC SMALL LETTER YU 
(define XK_Cyrillic_a                    #x06c1)  ; U+0430 CYRILLIC SMALL LETTER A 
(define XK_Cyrillic_be                   #x06c2)  ; U+0431 CYRILLIC SMALL LETTER BE 
(define XK_Cyrillic_tse                  #x06c3)  ; U+0446 CYRILLIC SMALL LETTER TSE 
(define XK_Cyrillic_de                   #x06c4)  ; U+0434 CYRILLIC SMALL LETTER DE 
(define XK_Cyrillic_ie                   #x06c5)  ; U+0435 CYRILLIC SMALL LETTER IE 
(define XK_Cyrillic_ef                   #x06c6)  ; U+0444 CYRILLIC SMALL LETTER EF 
(define XK_Cyrillic_ghe                  #x06c7)  ; U+0433 CYRILLIC SMALL LETTER GHE 
(define XK_Cyrillic_ha                   #x06c8)  ; U+0445 CYRILLIC SMALL LETTER HA 
(define XK_Cyrillic_i                    #x06c9)  ; U+0438 CYRILLIC SMALL LETTER I 
(define XK_Cyrillic_shorti               #x06ca)  ; U+0439 CYRILLIC SMALL LETTER SHORT I 
(define XK_Cyrillic_ka                   #x06cb)  ; U+043A CYRILLIC SMALL LETTER KA 
(define XK_Cyrillic_el                   #x06cc)  ; U+043B CYRILLIC SMALL LETTER EL 
(define XK_Cyrillic_em                   #x06cd)  ; U+043C CYRILLIC SMALL LETTER EM 
(define XK_Cyrillic_en                   #x06ce)  ; U+043D CYRILLIC SMALL LETTER EN 
(define XK_Cyrillic_o                    #x06cf)  ; U+043E CYRILLIC SMALL LETTER O 
(define XK_Cyrillic_pe                   #x06d0)  ; U+043F CYRILLIC SMALL LETTER PE 
(define XK_Cyrillic_ya                   #x06d1)  ; U+044F CYRILLIC SMALL LETTER YA 
(define XK_Cyrillic_er                   #x06d2)  ; U+0440 CYRILLIC SMALL LETTER ER 
(define XK_Cyrillic_es                   #x06d3)  ; U+0441 CYRILLIC SMALL LETTER ES 
(define XK_Cyrillic_te                   #x06d4)  ; U+0442 CYRILLIC SMALL LETTER TE 
(define XK_Cyrillic_u                    #x06d5)  ; U+0443 CYRILLIC SMALL LETTER U 
(define XK_Cyrillic_zhe                  #x06d6)  ; U+0436 CYRILLIC SMALL LETTER ZHE 
(define XK_Cyrillic_ve                   #x06d7)  ; U+0432 CYRILLIC SMALL LETTER VE 
(define XK_Cyrillic_softsign             #x06d8)  ; U+044C CYRILLIC SMALL LETTER SOFT SIGN 
(define XK_Cyrillic_yeru                 #x06d9)  ; U+044B CYRILLIC SMALL LETTER YERU 
(define XK_Cyrillic_ze                   #x06da)  ; U+0437 CYRILLIC SMALL LETTER ZE 
(define XK_Cyrillic_sha                  #x06db)  ; U+0448 CYRILLIC SMALL LETTER SHA 
(define XK_Cyrillic_e                    #x06dc)  ; U+044D CYRILLIC SMALL LETTER E 
(define XK_Cyrillic_shcha                #x06dd)  ; U+0449 CYRILLIC SMALL LETTER SHCHA 
(define XK_Cyrillic_che                  #x06de)  ; U+0447 CYRILLIC SMALL LETTER CHE 
(define XK_Cyrillic_hardsign             #x06df)  ; U+044A CYRILLIC SMALL LETTER HARD SIGN 
(define XK_Cyrillic_YU                   #x06e0)  ; U+042E CYRILLIC CAPITAL LETTER YU 
(define XK_Cyrillic_A                    #x06e1)  ; U+0410 CYRILLIC CAPITAL LETTER A 
(define XK_Cyrillic_BE                   #x06e2)  ; U+0411 CYRILLIC CAPITAL LETTER BE 
(define XK_Cyrillic_TSE                  #x06e3)  ; U+0426 CYRILLIC CAPITAL LETTER TSE 
(define XK_Cyrillic_DE                   #x06e4)  ; U+0414 CYRILLIC CAPITAL LETTER DE 
(define XK_Cyrillic_IE                   #x06e5)  ; U+0415 CYRILLIC CAPITAL LETTER IE 
(define XK_Cyrillic_EF                   #x06e6)  ; U+0424 CYRILLIC CAPITAL LETTER EF 
(define XK_Cyrillic_GHE                  #x06e7)  ; U+0413 CYRILLIC CAPITAL LETTER GHE 
(define XK_Cyrillic_HA                   #x06e8)  ; U+0425 CYRILLIC CAPITAL LETTER HA 
(define XK_Cyrillic_I                    #x06e9)  ; U+0418 CYRILLIC CAPITAL LETTER I 
(define XK_Cyrillic_SHORTI               #x06ea)  ; U+0419 CYRILLIC CAPITAL LETTER SHORT I 
(define XK_Cyrillic_KA                   #x06eb)  ; U+041A CYRILLIC CAPITAL LETTER KA 
(define XK_Cyrillic_EL                   #x06ec)  ; U+041B CYRILLIC CAPITAL LETTER EL 
(define XK_Cyrillic_EM                   #x06ed)  ; U+041C CYRILLIC CAPITAL LETTER EM 
(define XK_Cyrillic_EN                   #x06ee)  ; U+041D CYRILLIC CAPITAL LETTER EN 
(define XK_Cyrillic_O                    #x06ef)  ; U+041E CYRILLIC CAPITAL LETTER O 
(define XK_Cyrillic_PE                   #x06f0)  ; U+041F CYRILLIC CAPITAL LETTER PE 
(define XK_Cyrillic_YA                   #x06f1)  ; U+042F CYRILLIC CAPITAL LETTER YA 
(define XK_Cyrillic_ER                   #x06f2)  ; U+0420 CYRILLIC CAPITAL LETTER ER 
(define XK_Cyrillic_ES                   #x06f3)  ; U+0421 CYRILLIC CAPITAL LETTER ES 
(define XK_Cyrillic_TE                   #x06f4)  ; U+0422 CYRILLIC CAPITAL LETTER TE 
(define XK_Cyrillic_U                    #x06f5)  ; U+0423 CYRILLIC CAPITAL LETTER U 
(define XK_Cyrillic_ZHE                  #x06f6)  ; U+0416 CYRILLIC CAPITAL LETTER ZHE 
(define XK_Cyrillic_VE                   #x06f7)  ; U+0412 CYRILLIC CAPITAL LETTER VE 
(define XK_Cyrillic_SOFTSIGN             #x06f8)  ; U+042C CYRILLIC CAPITAL LETTER SOFT SIGN 
(define XK_Cyrillic_YERU                 #x06f9)  ; U+042B CYRILLIC CAPITAL LETTER YERU 
(define XK_Cyrillic_ZE                   #x06fa)  ; U+0417 CYRILLIC CAPITAL LETTER ZE 
(define XK_Cyrillic_SHA                  #x06fb)  ; U+0428 CYRILLIC CAPITAL LETTER SHA 
(define XK_Cyrillic_E                    #x06fc)  ; U+042D CYRILLIC CAPITAL LETTER E 
(define XK_Cyrillic_SHCHA                #x06fd)  ; U+0429 CYRILLIC CAPITAL LETTER SHCHA 
(define XK_Cyrillic_CHE                  #x06fe)  ; U+0427 CYRILLIC CAPITAL LETTER CHE 
(define XK_Cyrillic_HARDSIGN             #x06ff)  ; U+042A CYRILLIC CAPITAL LETTER HARD SIGN 
;#endif ; XK_CYRILLIC 

#|
 * Greek
 * (based on an early draft of, and not quite identical to, ISO/IEC 8859-7)
 * Byte 3 = 7
 |#

;#ifdef XK_GREEK
(define XK_Greek_ALPHAaccent             #x07a1)  ; U+0386 GREEK CAPITAL LETTER ALPHA WITH TONOS 
(define XK_Greek_EPSILONaccent           #x07a2)  ; U+0388 GREEK CAPITAL LETTER EPSILON WITH TONOS 
(define XK_Greek_ETAaccent               #x07a3)  ; U+0389 GREEK CAPITAL LETTER ETA WITH TONOS 
(define XK_Greek_IOTAaccent              #x07a4)  ; U+038A GREEK CAPITAL LETTER IOTA WITH TONOS 
(define XK_Greek_IOTAdieresis            #x07a5)  ; U+03AA GREEK CAPITAL LETTER IOTA WITH DIALYTIKA 
(define XK_Greek_IOTAdiaeresis           #x07a5)  ; old typo 
(define XK_Greek_OMICRONaccent           #x07a7)  ; U+038C GREEK CAPITAL LETTER OMICRON WITH TONOS 
(define XK_Greek_UPSILONaccent           #x07a8)  ; U+038E GREEK CAPITAL LETTER UPSILON WITH TONOS 
(define XK_Greek_UPSILONdieresis         #x07a9)  ; U+03AB GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA 
(define XK_Greek_OMEGAaccent             #x07ab)  ; U+038F GREEK CAPITAL LETTER OMEGA WITH TONOS 
(define XK_Greek_accentdieresis          #x07ae)  ; U+0385 GREEK DIALYTIKA TONOS 
(define XK_Greek_horizbar                #x07af)  ; U+2015 HORIZONTAL BAR 
(define XK_Greek_alphaaccent             #x07b1)  ; U+03AC GREEK SMALL LETTER ALPHA WITH TONOS 
(define XK_Greek_epsilonaccent           #x07b2)  ; U+03AD GREEK SMALL LETTER EPSILON WITH TONOS 
(define XK_Greek_etaaccent               #x07b3)  ; U+03AE GREEK SMALL LETTER ETA WITH TONOS 
(define XK_Greek_iotaaccent              #x07b4)  ; U+03AF GREEK SMALL LETTER IOTA WITH TONOS 
(define XK_Greek_iotadieresis            #x07b5)  ; U+03CA GREEK SMALL LETTER IOTA WITH DIALYTIKA 
(define XK_Greek_iotaaccentdieresis      #x07b6)  ; U+0390 GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS 
(define XK_Greek_omicronaccent           #x07b7)  ; U+03CC GREEK SMALL LETTER OMICRON WITH TONOS 
(define XK_Greek_upsilonaccent           #x07b8)  ; U+03CD GREEK SMALL LETTER UPSILON WITH TONOS 
(define XK_Greek_upsilondieresis         #x07b9)  ; U+03CB GREEK SMALL LETTER UPSILON WITH DIALYTIKA 
(define XK_Greek_upsilonaccentdieresis   #x07ba)  ; U+03B0 GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS 
(define XK_Greek_omegaaccent             #x07bb)  ; U+03CE GREEK SMALL LETTER OMEGA WITH TONOS 
(define XK_Greek_ALPHA                   #x07c1)  ; U+0391 GREEK CAPITAL LETTER ALPHA 
(define XK_Greek_BETA                    #x07c2)  ; U+0392 GREEK CAPITAL LETTER BETA 
(define XK_Greek_GAMMA                   #x07c3)  ; U+0393 GREEK CAPITAL LETTER GAMMA 
(define XK_Greek_DELTA                   #x07c4)  ; U+0394 GREEK CAPITAL LETTER DELTA 
(define XK_Greek_EPSILON                 #x07c5)  ; U+0395 GREEK CAPITAL LETTER EPSILON 
(define XK_Greek_ZETA                    #x07c6)  ; U+0396 GREEK CAPITAL LETTER ZETA 
(define XK_Greek_ETA                     #x07c7)  ; U+0397 GREEK CAPITAL LETTER ETA 
(define XK_Greek_THETA                   #x07c8)  ; U+0398 GREEK CAPITAL LETTER THETA 
(define XK_Greek_IOTA                    #x07c9)  ; U+0399 GREEK CAPITAL LETTER IOTA 
(define XK_Greek_KAPPA                   #x07ca)  ; U+039A GREEK CAPITAL LETTER KAPPA 
(define XK_Greek_LAMDA                   #x07cb)  ; U+039B GREEK CAPITAL LETTER LAMDA 
(define XK_Greek_LAMBDA                  #x07cb)  ; U+039B GREEK CAPITAL LETTER LAMDA 
(define XK_Greek_MU                      #x07cc)  ; U+039C GREEK CAPITAL LETTER MU 
(define XK_Greek_NU                      #x07cd)  ; U+039D GREEK CAPITAL LETTER NU 
(define XK_Greek_XI                      #x07ce)  ; U+039E GREEK CAPITAL LETTER XI 
(define XK_Greek_OMICRON                 #x07cf)  ; U+039F GREEK CAPITAL LETTER OMICRON 
(define XK_Greek_PI                      #x07d0)  ; U+03A0 GREEK CAPITAL LETTER PI 
(define XK_Greek_RHO                     #x07d1)  ; U+03A1 GREEK CAPITAL LETTER RHO 
(define XK_Greek_SIGMA                   #x07d2)  ; U+03A3 GREEK CAPITAL LETTER SIGMA 
(define XK_Greek_TAU                     #x07d4)  ; U+03A4 GREEK CAPITAL LETTER TAU 
(define XK_Greek_UPSILON                 #x07d5)  ; U+03A5 GREEK CAPITAL LETTER UPSILON 
(define XK_Greek_PHI                     #x07d6)  ; U+03A6 GREEK CAPITAL LETTER PHI 
(define XK_Greek_CHI                     #x07d7)  ; U+03A7 GREEK CAPITAL LETTER CHI 
(define XK_Greek_PSI                     #x07d8)  ; U+03A8 GREEK CAPITAL LETTER PSI 
(define XK_Greek_OMEGA                   #x07d9)  ; U+03A9 GREEK CAPITAL LETTER OMEGA 
(define XK_Greek_alpha                   #x07e1)  ; U+03B1 GREEK SMALL LETTER ALPHA 
(define XK_Greek_beta                    #x07e2)  ; U+03B2 GREEK SMALL LETTER BETA 
(define XK_Greek_gamma                   #x07e3)  ; U+03B3 GREEK SMALL LETTER GAMMA 
(define XK_Greek_delta                   #x07e4)  ; U+03B4 GREEK SMALL LETTER DELTA 
(define XK_Greek_epsilon                 #x07e5)  ; U+03B5 GREEK SMALL LETTER EPSILON 
(define XK_Greek_zeta                    #x07e6)  ; U+03B6 GREEK SMALL LETTER ZETA 
(define XK_Greek_eta                     #x07e7)  ; U+03B7 GREEK SMALL LETTER ETA 
(define XK_Greek_theta                   #x07e8)  ; U+03B8 GREEK SMALL LETTER THETA 
(define XK_Greek_iota                    #x07e9)  ; U+03B9 GREEK SMALL LETTER IOTA 
(define XK_Greek_kappa                   #x07ea)  ; U+03BA GREEK SMALL LETTER KAPPA 
(define XK_Greek_lamda                   #x07eb)  ; U+03BB GREEK SMALL LETTER LAMDA 
(define XK_Greek_lambda                  #x07eb)  ; U+03BB GREEK SMALL LETTER LAMDA 
(define XK_Greek_mu                      #x07ec)  ; U+03BC GREEK SMALL LETTER MU 
(define XK_Greek_nu                      #x07ed)  ; U+03BD GREEK SMALL LETTER NU 
(define XK_Greek_xi                      #x07ee)  ; U+03BE GREEK SMALL LETTER XI 
(define XK_Greek_omicron                 #x07ef)  ; U+03BF GREEK SMALL LETTER OMICRON 
(define XK_Greek_pi                      #x07f0)  ; U+03C0 GREEK SMALL LETTER PI 
(define XK_Greek_rho                     #x07f1)  ; U+03C1 GREEK SMALL LETTER RHO 
(define XK_Greek_sigma                   #x07f2)  ; U+03C3 GREEK SMALL LETTER SIGMA 
(define XK_Greek_finalsmallsigma         #x07f3)  ; U+03C2 GREEK SMALL LETTER FINAL SIGMA 
(define XK_Greek_tau                     #x07f4)  ; U+03C4 GREEK SMALL LETTER TAU 
(define XK_Greek_upsilon                 #x07f5)  ; U+03C5 GREEK SMALL LETTER UPSILON 
(define XK_Greek_phi                     #x07f6)  ; U+03C6 GREEK SMALL LETTER PHI 
(define XK_Greek_chi                     #x07f7)  ; U+03C7 GREEK SMALL LETTER CHI 
(define XK_Greek_psi                     #x07f8)  ; U+03C8 GREEK SMALL LETTER PSI 
(define XK_Greek_omega                   #x07f9)  ; U+03C9 GREEK SMALL LETTER OMEGA 
(define XK_Greek_switch                  #xff7e)  ; Alias for mode_switch 
;#endif ; XK_GREEK 

#|
 * Technical
 * (from the DEC VT330/VT420 Technical Character Set, http://vt100.net/charsets/technical.html)
 * Byte 3 = 8
 |#

;#ifdef XK_TECHNICAL
(define XK_leftradical                   #x08a1)  ; U+23B7 RADICAL SYMBOL BOTTOM 
(define XK_topleftradical                #x08a2)  ;(U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT)
(define XK_horizconnector                #x08a3)  ;(U+2500 BOX DRAWINGS LIGHT HORIZONTAL)
(define XK_topintegral                   #x08a4)  ; U+2320 TOP HALF INTEGRAL 
(define XK_botintegral                   #x08a5)  ; U+2321 BOTTOM HALF INTEGRAL 
(define XK_vertconnector                 #x08a6)  ;(U+2502 BOX DRAWINGS LIGHT VERTICAL)
(define XK_topleftsqbracket              #x08a7)  ; U+23A1 LEFT SQUARE BRACKET UPPER CORNER 
(define XK_botleftsqbracket              #x08a8)  ; U+23A3 LEFT SQUARE BRACKET LOWER CORNER 
(define XK_toprightsqbracket             #x08a9)  ; U+23A4 RIGHT SQUARE BRACKET UPPER CORNER 
(define XK_botrightsqbracket             #x08aa)  ; U+23A6 RIGHT SQUARE BRACKET LOWER CORNER 
(define XK_topleftparens                 #x08ab)  ; U+239B LEFT PARENTHESIS UPPER HOOK 
(define XK_botleftparens                 #x08ac)  ; U+239D LEFT PARENTHESIS LOWER HOOK 
(define XK_toprightparens                #x08ad)  ; U+239E RIGHT PARENTHESIS UPPER HOOK 
(define XK_botrightparens                #x08ae)  ; U+23A0 RIGHT PARENTHESIS LOWER HOOK 
(define XK_leftmiddlecurlybrace          #x08af)  ; U+23A8 LEFT CURLY BRACKET MIDDLE PIECE 
(define XK_rightmiddlecurlybrace         #x08b0)  ; U+23AC RIGHT CURLY BRACKET MIDDLE PIECE 
(define XK_topleftsummation              #x08b1)
(define XK_botleftsummation              #x08b2)
(define XK_topvertsummationconnector     #x08b3)
(define XK_botvertsummationconnector     #x08b4)
(define XK_toprightsummation             #x08b5)
(define XK_botrightsummation             #x08b6)
(define XK_rightmiddlesummation          #x08b7)
(define XK_lessthanequal                 #x08bc)  ; U+2264 LESS-THAN OR EQUAL TO 
(define XK_notequal                      #x08bd)  ; U+2260 NOT EQUAL TO 
(define XK_greaterthanequal              #x08be)  ; U+2265 GREATER-THAN OR EQUAL TO 
(define XK_integral                      #x08bf)  ; U+222B INTEGRAL 
(define XK_therefore                     #x08c0)  ; U+2234 THEREFORE 
(define XK_variation                     #x08c1)  ; U+221D PROPORTIONAL TO 
(define XK_infinity                      #x08c2)  ; U+221E INFINITY 
(define XK_nabla                         #x08c5)  ; U+2207 NABLA 
(define XK_approximate                   #x08c8)  ; U+223C TILDE OPERATOR 
(define XK_similarequal                  #x08c9)  ; U+2243 ASYMPTOTICALLY EQUAL TO 
(define XK_ifonlyif                      #x08cd)  ; U+21D4 LEFT RIGHT DOUBLE ARROW 
(define XK_implies                       #x08ce)  ; U+21D2 RIGHTWARDS DOUBLE ARROW 
(define XK_identical                     #x08cf)  ; U+2261 IDENTICAL TO 
(define XK_radical                       #x08d6)  ; U+221A SQUARE ROOT 
(define XK_includedin                    #x08da)  ; U+2282 SUBSET OF 
(define XK_includes                      #x08db)  ; U+2283 SUPERSET OF 
(define XK_intersection                  #x08dc)  ; U+2229 INTERSECTION 
(define XK_union                         #x08dd)  ; U+222A UNION 
(define XK_logicaland                    #x08de)  ; U+2227 LOGICAL AND 
(define XK_logicalor                     #x08df)  ; U+2228 LOGICAL OR 
(define XK_partialderivative             #x08ef)  ; U+2202 PARTIAL DIFFERENTIAL 
(define XK_function                      #x08f6)  ; U+0192 LATIN SMALL LETTER F WITH HOOK 
(define XK_leftarrow                     #x08fb)  ; U+2190 LEFTWARDS ARROW 
(define XK_uparrow                       #x08fc)  ; U+2191 UPWARDS ARROW 
(define XK_rightarrow                    #x08fd)  ; U+2192 RIGHTWARDS ARROW 
(define XK_downarrow                     #x08fe)  ; U+2193 DOWNWARDS ARROW 
;#endif ; XK_TECHNICAL 

#|
 * Special
 * (from the DEC VT100 Special Graphics Character Set)
 * Byte 3 = 9
 |#

;#ifdef XK_SPECIAL
(define XK_blank                         #x09df)
(define XK_soliddiamond                  #x09e0)  ; U+25C6 BLACK DIAMOND 
(define XK_checkerboard                  #x09e1)  ; U+2592 MEDIUM SHADE 
(define XK_ht                            #x09e2)  ; U+2409 SYMBOL FOR HORIZONTAL TABULATION 
(define XK_ff                            #x09e3)  ; U+240C SYMBOL FOR FORM FEED 
(define XK_cr                            #x09e4)  ; U+240D SYMBOL FOR CARRIAGE RETURN 
(define XK_lf                            #x09e5)  ; U+240A SYMBOL FOR LINE FEED 
(define XK_nl                            #x09e8)  ; U+2424 SYMBOL FOR NEWLINE 
(define XK_vt                            #x09e9)  ; U+240B SYMBOL FOR VERTICAL TABULATION 
(define XK_lowrightcorner                #x09ea)  ; U+2518 BOX DRAWINGS LIGHT UP AND LEFT 
(define XK_uprightcorner                 #x09eb)  ; U+2510 BOX DRAWINGS LIGHT DOWN AND LEFT 
(define XK_upleftcorner                  #x09ec)  ; U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT 
(define XK_lowleftcorner                 #x09ed)  ; U+2514 BOX DRAWINGS LIGHT UP AND RIGHT 
(define XK_crossinglines                 #x09ee)  ; U+253C BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL 
(define XK_horizlinescan1                #x09ef)  ; U+23BA HORIZONTAL SCAN LINE-1 
(define XK_horizlinescan3                #x09f0)  ; U+23BB HORIZONTAL SCAN LINE-3 
(define XK_horizlinescan5                #x09f1)  ; U+2500 BOX DRAWINGS LIGHT HORIZONTAL 
(define XK_horizlinescan7                #x09f2)  ; U+23BC HORIZONTAL SCAN LINE-7 
(define XK_horizlinescan9                #x09f3)  ; U+23BD HORIZONTAL SCAN LINE-9 
(define XK_leftt                         #x09f4)  ; U+251C BOX DRAWINGS LIGHT VERTICAL AND RIGHT 
(define XK_rightt                        #x09f5)  ; U+2524 BOX DRAWINGS LIGHT VERTICAL AND LEFT 
(define XK_bott                          #x09f6)  ; U+2534 BOX DRAWINGS LIGHT UP AND HORIZONTAL 
(define XK_topt                          #x09f7)  ; U+252C BOX DRAWINGS LIGHT DOWN AND HORIZONTAL 
(define XK_vertbar                       #x09f8)  ; U+2502 BOX DRAWINGS LIGHT VERTICAL 
;#endif ; XK_SPECIAL 

#|
 * Publishing
 * (these are probably from a long forgotten DEC Publishing
 * font that once shipped with DECwrite)
 * Byte 3 = 0x0a
 |#

;#ifdef XK_PUBLISHING
(define XK_emspace                       #x0aa1)  ; U+2003 EM SPACE 
(define XK_enspace                       #x0aa2)  ; U+2002 EN SPACE 
(define XK_em3space                      #x0aa3)  ; U+2004 THREE-PER-EM SPACE 
(define XK_em4space                      #x0aa4)  ; U+2005 FOUR-PER-EM SPACE 
(define XK_digitspace                    #x0aa5)  ; U+2007 FIGURE SPACE 
(define XK_punctspace                    #x0aa6)  ; U+2008 PUNCTUATION SPACE 
(define XK_thinspace                     #x0aa7)  ; U+2009 THIN SPACE 
(define XK_hairspace                     #x0aa8)  ; U+200A HAIR SPACE 
(define XK_emdash                        #x0aa9)  ; U+2014 EM DASH 
(define XK_endash                        #x0aaa)  ; U+2013 EN DASH 
(define XK_signifblank                   #x0aac)  ;(U+2423 OPEN BOX)
(define XK_ellipsis                      #x0aae)  ; U+2026 HORIZONTAL ELLIPSIS 
(define XK_doubbaselinedot               #x0aaf)  ; U+2025 TWO DOT LEADER 
(define XK_onethird                      #x0ab0)  ; U+2153 VULGAR FRACTION ONE THIRD 
(define XK_twothirds                     #x0ab1)  ; U+2154 VULGAR FRACTION TWO THIRDS 
(define XK_onefifth                      #x0ab2)  ; U+2155 VULGAR FRACTION ONE FIFTH 
(define XK_twofifths                     #x0ab3)  ; U+2156 VULGAR FRACTION TWO FIFTHS 
(define XK_threefifths                   #x0ab4)  ; U+2157 VULGAR FRACTION THREE FIFTHS 
(define XK_fourfifths                    #x0ab5)  ; U+2158 VULGAR FRACTION FOUR FIFTHS 
(define XK_onesixth                      #x0ab6)  ; U+2159 VULGAR FRACTION ONE SIXTH 
(define XK_fivesixths                    #x0ab7)  ; U+215A VULGAR FRACTION FIVE SIXTHS 
(define XK_careof                        #x0ab8)  ; U+2105 CARE OF 
(define XK_figdash                       #x0abb)  ; U+2012 FIGURE DASH 
(define XK_leftanglebracket              #x0abc)  ;(U+27E8 MATHEMATICAL LEFT ANGLE BRACKET)
(define XK_decimalpoint                  #x0abd)  ;(U+002E FULL STOP)
(define XK_rightanglebracket             #x0abe)  ;(U+27E9 MATHEMATICAL RIGHT ANGLE BRACKET)
(define XK_marker                        #x0abf)
(define XK_oneeighth                     #x0ac3)  ; U+215B VULGAR FRACTION ONE EIGHTH 
(define XK_threeeighths                  #x0ac4)  ; U+215C VULGAR FRACTION THREE EIGHTHS 
(define XK_fiveeighths                   #x0ac5)  ; U+215D VULGAR FRACTION FIVE EIGHTHS 
(define XK_seveneighths                  #x0ac6)  ; U+215E VULGAR FRACTION SEVEN EIGHTHS 
(define XK_trademark                     #x0ac9)  ; U+2122 TRADE MARK SIGN 
(define XK_signaturemark                 #x0aca)  ;(U+2613 SALTIRE)
(define XK_trademarkincircle             #x0acb)
(define XK_leftopentriangle              #x0acc)  ;(U+25C1 WHITE LEFT-POINTING TRIANGLE)
(define XK_rightopentriangle             #x0acd)  ;(U+25B7 WHITE RIGHT-POINTING TRIANGLE)
(define XK_emopencircle                  #x0ace)  ;(U+25CB WHITE CIRCLE)
(define XK_emopenrectangle               #x0acf)  ;(U+25AF WHITE VERTICAL RECTANGLE)
(define XK_leftsinglequotemark           #x0ad0)  ; U+2018 LEFT SINGLE QUOTATION MARK 
(define XK_rightsinglequotemark          #x0ad1)  ; U+2019 RIGHT SINGLE QUOTATION MARK 
(define XK_leftdoublequotemark           #x0ad2)  ; U+201C LEFT DOUBLE QUOTATION MARK 
(define XK_rightdoublequotemark          #x0ad3)  ; U+201D RIGHT DOUBLE QUOTATION MARK 
(define XK_prescription                  #x0ad4)  ; U+211E PRESCRIPTION TAKE 
(define XK_minutes                       #x0ad6)  ; U+2032 PRIME 
(define XK_seconds                       #x0ad7)  ; U+2033 DOUBLE PRIME 
(define XK_latincross                    #x0ad9)  ; U+271D LATIN CROSS 
(define XK_hexagram                      #x0ada)
(define XK_filledrectbullet              #x0adb)  ;(U+25AC BLACK RECTANGLE)
(define XK_filledlefttribullet           #x0adc)  ;(U+25C0 BLACK LEFT-POINTING TRIANGLE)
(define XK_filledrighttribullet          #x0add)  ;(U+25B6 BLACK RIGHT-POINTING TRIANGLE)
(define XK_emfilledcircle                #x0ade)  ;(U+25CF BLACK CIRCLE)
(define XK_emfilledrect                  #x0adf)  ;(U+25AE BLACK VERTICAL RECTANGLE)
(define XK_enopencircbullet              #x0ae0)  ;(U+25E6 WHITE BULLET)
(define XK_enopensquarebullet            #x0ae1)  ;(U+25AB WHITE SMALL SQUARE)
(define XK_openrectbullet                #x0ae2)  ;(U+25AD WHITE RECTANGLE)
(define XK_opentribulletup               #x0ae3)  ;(U+25B3 WHITE UP-POINTING TRIANGLE)
(define XK_opentribulletdown             #x0ae4)  ;(U+25BD WHITE DOWN-POINTING TRIANGLE)
(define XK_openstar                      #x0ae5)  ;(U+2606 WHITE STAR)
(define XK_enfilledcircbullet            #x0ae6)  ;(U+2022 BULLET)
(define XK_enfilledsqbullet              #x0ae7)  ;(U+25AA BLACK SMALL SQUARE)
(define XK_filledtribulletup             #x0ae8)  ;(U+25B2 BLACK UP-POINTING TRIANGLE)
(define XK_filledtribulletdown           #x0ae9)  ;(U+25BC BLACK DOWN-POINTING TRIANGLE)
(define XK_leftpointer                   #x0aea)  ;(U+261C WHITE LEFT POINTING INDEX)
(define XK_rightpointer                  #x0aeb)  ;(U+261E WHITE RIGHT POINTING INDEX)
(define XK_club                          #x0aec)  ; U+2663 BLACK CLUB SUIT 
(define XK_diamond                       #x0aed)  ; U+2666 BLACK DIAMOND SUIT 
(define XK_heart                         #x0aee)  ; U+2665 BLACK HEART SUIT 
(define XK_maltesecross                  #x0af0)  ; U+2720 MALTESE CROSS 
(define XK_dagger                        #x0af1)  ; U+2020 DAGGER 
(define XK_doubledagger                  #x0af2)  ; U+2021 DOUBLE DAGGER 
(define XK_checkmark                     #x0af3)  ; U+2713 CHECK MARK 
(define XK_ballotcross                   #x0af4)  ; U+2717 BALLOT X 
(define XK_musicalsharp                  #x0af5)  ; U+266F MUSIC SHARP SIGN 
(define XK_musicalflat                   #x0af6)  ; U+266D MUSIC FLAT SIGN 
(define XK_malesymbol                    #x0af7)  ; U+2642 MALE SIGN 
(define XK_femalesymbol                  #x0af8)  ; U+2640 FEMALE SIGN 
(define XK_telephone                     #x0af9)  ; U+260E BLACK TELEPHONE 
(define XK_telephonerecorder             #x0afa)  ; U+2315 TELEPHONE RECORDER 
(define XK_phonographcopyright           #x0afb)  ; U+2117 SOUND RECORDING COPYRIGHT 
(define XK_caret                         #x0afc)  ; U+2038 CARET 
(define XK_singlelowquotemark            #x0afd)  ; U+201A SINGLE LOW-9 QUOTATION MARK 
(define XK_doublelowquotemark            #x0afe)  ; U+201E DOUBLE LOW-9 QUOTATION MARK 
(define XK_cursor                        #x0aff)
;#endif ; XK_PUBLISHING 

#|
 * APL
 * Byte 3 = 0x0b
 |#

;#ifdef XK_APL
(define XK_leftcaret                     #x0ba3)  ;(U+003C LESS-THAN SIGN)
(define XK_rightcaret                    #x0ba6)  ;(U+003E GREATER-THAN SIGN)
(define XK_downcaret                     #x0ba8)  ;(U+2228 LOGICAL OR)
(define XK_upcaret                       #x0ba9)  ;(U+2227 LOGICAL AND)
(define XK_overbar                       #x0bc0)  ;(U+00AF MACRON)
(define XK_downtack                      #x0bc2)  ; U+22A4 DOWN TACK 
(define XK_upshoe                        #x0bc3)  ;(U+2229 INTERSECTION)
(define XK_downstile                     #x0bc4)  ; U+230A LEFT FLOOR 
(define XK_underbar                      #x0bc6)  ;(U+005F LOW LINE)
(define XK_jot                           #x0bca)  ; U+2218 RING OPERATOR 
(define XK_quad                          #x0bcc)  ; U+2395 APL FUNCTIONAL SYMBOL QUAD 
(define XK_uptack                        #x0bce)  ; U+22A5 UP TACK 
(define XK_circle                        #x0bcf)  ; U+25CB WHITE CIRCLE 
(define XK_upstile                       #x0bd3)  ; U+2308 LEFT CEILING 
(define XK_downshoe                      #x0bd6)  ;(U+222A UNION)
(define XK_rightshoe                     #x0bd8)  ;(U+2283 SUPERSET OF)
(define XK_leftshoe                      #x0bda)  ;(U+2282 SUBSET OF)
(define XK_lefttack                      #x0bdc)  ; U+22A3 LEFT TACK 
(define XK_righttack                     #x0bfc)  ; U+22A2 RIGHT TACK 
;#endif ; XK_APL 

#|
 * Hebrew
 * Byte 3 = 0x0c
 |#

;#ifdef XK_HEBREW
(define XK_hebrew_doublelowline          #x0cdf)  ; U+2017 DOUBLE LOW LINE 
(define XK_hebrew_aleph                  #x0ce0)  ; U+05D0 HEBREW LETTER ALEF 
(define XK_hebrew_bet                    #x0ce1)  ; U+05D1 HEBREW LETTER BET 
(define XK_hebrew_beth                   #x0ce1)  ; deprecated 
(define XK_hebrew_gimel                  #x0ce2)  ; U+05D2 HEBREW LETTER GIMEL 
(define XK_hebrew_gimmel                 #x0ce2)  ; deprecated 
(define XK_hebrew_dalet                  #x0ce3)  ; U+05D3 HEBREW LETTER DALET 
(define XK_hebrew_daleth                 #x0ce3)  ; deprecated 
(define XK_hebrew_he                     #x0ce4)  ; U+05D4 HEBREW LETTER HE 
(define XK_hebrew_waw                    #x0ce5)  ; U+05D5 HEBREW LETTER VAV 
(define XK_hebrew_zain                   #x0ce6)  ; U+05D6 HEBREW LETTER ZAYIN 
(define XK_hebrew_zayin                  #x0ce6)  ; deprecated 
(define XK_hebrew_chet                   #x0ce7)  ; U+05D7 HEBREW LETTER HET 
(define XK_hebrew_het                    #x0ce7)  ; deprecated 
(define XK_hebrew_tet                    #x0ce8)  ; U+05D8 HEBREW LETTER TET 
(define XK_hebrew_teth                   #x0ce8)  ; deprecated 
(define XK_hebrew_yod                    #x0ce9)  ; U+05D9 HEBREW LETTER YOD 
(define XK_hebrew_finalkaph              #x0cea)  ; U+05DA HEBREW LETTER FINAL KAF 
(define XK_hebrew_kaph                   #x0ceb)  ; U+05DB HEBREW LETTER KAF 
(define XK_hebrew_lamed                  #x0cec)  ; U+05DC HEBREW LETTER LAMED 
(define XK_hebrew_finalmem               #x0ced)  ; U+05DD HEBREW LETTER FINAL MEM 
(define XK_hebrew_mem                    #x0cee)  ; U+05DE HEBREW LETTER MEM 
(define XK_hebrew_finalnun               #x0cef)  ; U+05DF HEBREW LETTER FINAL NUN 
(define XK_hebrew_nun                    #x0cf0)  ; U+05E0 HEBREW LETTER NUN 
(define XK_hebrew_samech                 #x0cf1)  ; U+05E1 HEBREW LETTER SAMEKH 
(define XK_hebrew_samekh                 #x0cf1)  ; deprecated 
(define XK_hebrew_ayin                   #x0cf2)  ; U+05E2 HEBREW LETTER AYIN 
(define XK_hebrew_finalpe                #x0cf3)  ; U+05E3 HEBREW LETTER FINAL PE 
(define XK_hebrew_pe                     #x0cf4)  ; U+05E4 HEBREW LETTER PE 
(define XK_hebrew_finalzade              #x0cf5)  ; U+05E5 HEBREW LETTER FINAL TSADI 
(define XK_hebrew_finalzadi              #x0cf5)  ; deprecated 
(define XK_hebrew_zade                   #x0cf6)  ; U+05E6 HEBREW LETTER TSADI 
(define XK_hebrew_zadi                   #x0cf6)  ; deprecated 
(define XK_hebrew_qoph                   #x0cf7)  ; U+05E7 HEBREW LETTER QOF 
(define XK_hebrew_kuf                    #x0cf7)  ; deprecated 
(define XK_hebrew_resh                   #x0cf8)  ; U+05E8 HEBREW LETTER RESH 
(define XK_hebrew_shin                   #x0cf9)  ; U+05E9 HEBREW LETTER SHIN 
(define XK_hebrew_taw                    #x0cfa)  ; U+05EA HEBREW LETTER TAV 
(define XK_hebrew_taf                    #x0cfa)  ; deprecated 
(define XK_Hebrew_switch                 #xff7e)  ; Alias for mode_switch 
;#endif ; XK_HEBREW 

#|
 * Thai
 * Byte 3 = 0x0d
 |#

;#ifdef XK_THAI
(define XK_Thai_kokai                    #x0da1)  ; U+0E01 THAI CHARACTER KO KAI 
(define XK_Thai_khokhai                  #x0da2)  ; U+0E02 THAI CHARACTER KHO KHAI 
(define XK_Thai_khokhuat                 #x0da3)  ; U+0E03 THAI CHARACTER KHO KHUAT 
(define XK_Thai_khokhwai                 #x0da4)  ; U+0E04 THAI CHARACTER KHO KHWAI 
(define XK_Thai_khokhon                  #x0da5)  ; U+0E05 THAI CHARACTER KHO KHON 
(define XK_Thai_khorakhang               #x0da6)  ; U+0E06 THAI CHARACTER KHO RAKHANG 
(define XK_Thai_ngongu                   #x0da7)  ; U+0E07 THAI CHARACTER NGO NGU 
(define XK_Thai_chochan                  #x0da8)  ; U+0E08 THAI CHARACTER CHO CHAN 
(define XK_Thai_choching                 #x0da9)  ; U+0E09 THAI CHARACTER CHO CHING 
(define XK_Thai_chochang                 #x0daa)  ; U+0E0A THAI CHARACTER CHO CHANG 
(define XK_Thai_soso                     #x0dab)  ; U+0E0B THAI CHARACTER SO SO 
(define XK_Thai_chochoe                  #x0dac)  ; U+0E0C THAI CHARACTER CHO CHOE 
(define XK_Thai_yoying                   #x0dad)  ; U+0E0D THAI CHARACTER YO YING 
(define XK_Thai_dochada                  #x0dae)  ; U+0E0E THAI CHARACTER DO CHADA 
(define XK_Thai_topatak                  #x0daf)  ; U+0E0F THAI CHARACTER TO PATAK 
(define XK_Thai_thothan                  #x0db0)  ; U+0E10 THAI CHARACTER THO THAN 
(define XK_Thai_thonangmontho            #x0db1)  ; U+0E11 THAI CHARACTER THO NANGMONTHO 
(define XK_Thai_thophuthao               #x0db2)  ; U+0E12 THAI CHARACTER THO PHUTHAO 
(define XK_Thai_nonen                    #x0db3)  ; U+0E13 THAI CHARACTER NO NEN 
(define XK_Thai_dodek                    #x0db4)  ; U+0E14 THAI CHARACTER DO DEK 
(define XK_Thai_totao                    #x0db5)  ; U+0E15 THAI CHARACTER TO TAO 
(define XK_Thai_thothung                 #x0db6)  ; U+0E16 THAI CHARACTER THO THUNG 
(define XK_Thai_thothahan                #x0db7)  ; U+0E17 THAI CHARACTER THO THAHAN 
(define XK_Thai_thothong                 #x0db8)  ; U+0E18 THAI CHARACTER THO THONG 
(define XK_Thai_nonu                     #x0db9)  ; U+0E19 THAI CHARACTER NO NU 
(define XK_Thai_bobaimai                 #x0dba)  ; U+0E1A THAI CHARACTER BO BAIMAI 
(define XK_Thai_popla                    #x0dbb)  ; U+0E1B THAI CHARACTER PO PLA 
(define XK_Thai_phophung                 #x0dbc)  ; U+0E1C THAI CHARACTER PHO PHUNG 
(define XK_Thai_fofa                     #x0dbd)  ; U+0E1D THAI CHARACTER FO FA 
(define XK_Thai_phophan                  #x0dbe)  ; U+0E1E THAI CHARACTER PHO PHAN 
(define XK_Thai_fofan                    #x0dbf)  ; U+0E1F THAI CHARACTER FO FAN 
(define XK_Thai_phosamphao               #x0dc0)  ; U+0E20 THAI CHARACTER PHO SAMPHAO 
(define XK_Thai_moma                     #x0dc1)  ; U+0E21 THAI CHARACTER MO MA 
(define XK_Thai_yoyak                    #x0dc2)  ; U+0E22 THAI CHARACTER YO YAK 
(define XK_Thai_rorua                    #x0dc3)  ; U+0E23 THAI CHARACTER RO RUA 
(define XK_Thai_ru                       #x0dc4)  ; U+0E24 THAI CHARACTER RU 
(define XK_Thai_loling                   #x0dc5)  ; U+0E25 THAI CHARACTER LO LING 
(define XK_Thai_lu                       #x0dc6)  ; U+0E26 THAI CHARACTER LU 
(define XK_Thai_wowaen                   #x0dc7)  ; U+0E27 THAI CHARACTER WO WAEN 
(define XK_Thai_sosala                   #x0dc8)  ; U+0E28 THAI CHARACTER SO SALA 
(define XK_Thai_sorusi                   #x0dc9)  ; U+0E29 THAI CHARACTER SO RUSI 
(define XK_Thai_sosua                    #x0dca)  ; U+0E2A THAI CHARACTER SO SUA 
(define XK_Thai_hohip                    #x0dcb)  ; U+0E2B THAI CHARACTER HO HIP 
(define XK_Thai_lochula                  #x0dcc)  ; U+0E2C THAI CHARACTER LO CHULA 
(define XK_Thai_oang                     #x0dcd)  ; U+0E2D THAI CHARACTER O ANG 
(define XK_Thai_honokhuk                 #x0dce)  ; U+0E2E THAI CHARACTER HO NOKHUK 
(define XK_Thai_paiyannoi                #x0dcf)  ; U+0E2F THAI CHARACTER PAIYANNOI 
(define XK_Thai_saraa                    #x0dd0)  ; U+0E30 THAI CHARACTER SARA A 
(define XK_Thai_maihanakat               #x0dd1)  ; U+0E31 THAI CHARACTER MAI HAN-AKAT 
(define XK_Thai_saraaa                   #x0dd2)  ; U+0E32 THAI CHARACTER SARA AA 
(define XK_Thai_saraam                   #x0dd3)  ; U+0E33 THAI CHARACTER SARA AM 
(define XK_Thai_sarai                    #x0dd4)  ; U+0E34 THAI CHARACTER SARA I 
(define XK_Thai_saraii                   #x0dd5)  ; U+0E35 THAI CHARACTER SARA II 
(define XK_Thai_saraue                   #x0dd6)  ; U+0E36 THAI CHARACTER SARA UE 
(define XK_Thai_sarauee                  #x0dd7)  ; U+0E37 THAI CHARACTER SARA UEE 
(define XK_Thai_sarau                    #x0dd8)  ; U+0E38 THAI CHARACTER SARA U 
(define XK_Thai_sarauu                   #x0dd9)  ; U+0E39 THAI CHARACTER SARA UU 
(define XK_Thai_phinthu                  #x0dda)  ; U+0E3A THAI CHARACTER PHINTHU 
(define XK_Thai_maihanakat_maitho        #x0dde)
(define XK_Thai_baht                     #x0ddf)  ; U+0E3F THAI CURRENCY SYMBOL BAHT 
(define XK_Thai_sarae                    #x0de0)  ; U+0E40 THAI CHARACTER SARA E 
(define XK_Thai_saraae                   #x0de1)  ; U+0E41 THAI CHARACTER SARA AE 
(define XK_Thai_sarao                    #x0de2)  ; U+0E42 THAI CHARACTER SARA O 
(define XK_Thai_saraaimaimuan            #x0de3)  ; U+0E43 THAI CHARACTER SARA AI MAIMUAN 
(define XK_Thai_saraaimaimalai           #x0de4)  ; U+0E44 THAI CHARACTER SARA AI MAIMALAI 
(define XK_Thai_lakkhangyao              #x0de5)  ; U+0E45 THAI CHARACTER LAKKHANGYAO 
(define XK_Thai_maiyamok                 #x0de6)  ; U+0E46 THAI CHARACTER MAIYAMOK 
(define XK_Thai_maitaikhu                #x0de7)  ; U+0E47 THAI CHARACTER MAITAIKHU 
(define XK_Thai_maiek                    #x0de8)  ; U+0E48 THAI CHARACTER MAI EK 
(define XK_Thai_maitho                   #x0de9)  ; U+0E49 THAI CHARACTER MAI THO 
(define XK_Thai_maitri                   #x0dea)  ; U+0E4A THAI CHARACTER MAI TRI 
(define XK_Thai_maichattawa              #x0deb)  ; U+0E4B THAI CHARACTER MAI CHATTAWA 
(define XK_Thai_thanthakhat              #x0dec)  ; U+0E4C THAI CHARACTER THANTHAKHAT 
(define XK_Thai_nikhahit                 #x0ded)  ; U+0E4D THAI CHARACTER NIKHAHIT 
(define XK_Thai_leksun                   #x0df0)  ; U+0E50 THAI DIGIT ZERO 
(define XK_Thai_leknung                  #x0df1)  ; U+0E51 THAI DIGIT ONE 
(define XK_Thai_leksong                  #x0df2)  ; U+0E52 THAI DIGIT TWO 
(define XK_Thai_leksam                   #x0df3)  ; U+0E53 THAI DIGIT THREE 
(define XK_Thai_leksi                    #x0df4)  ; U+0E54 THAI DIGIT FOUR 
(define XK_Thai_lekha                    #x0df5)  ; U+0E55 THAI DIGIT FIVE 
(define XK_Thai_lekhok                   #x0df6)  ; U+0E56 THAI DIGIT SIX 
(define XK_Thai_lekchet                  #x0df7)  ; U+0E57 THAI DIGIT SEVEN 
(define XK_Thai_lekpaet                  #x0df8)  ; U+0E58 THAI DIGIT EIGHT 
(define XK_Thai_lekkao                   #x0df9)  ; U+0E59 THAI DIGIT NINE 
;#endif ; XK_THAI 

#|
 * Korean
 * Byte 3 = 0x0e
 |#

;#ifdef XK_KOREAN

(define XK_Hangul                        #xff31)  ; Hangul start/stop(toggle) 
(define XK_Hangul_Start                  #xff32)  ; Hangul start 
(define XK_Hangul_End                    #xff33)  ; Hangul end, English start 
(define XK_Hangul_Hanja                  #xff34)  ; Start Hangul->Hanja Conversion 
(define XK_Hangul_Jamo                   #xff35)  ; Hangul Jamo mode 
(define XK_Hangul_Romaja                 #xff36)  ; Hangul Romaja mode 
(define XK_Hangul_Codeinput              #xff37)  ; Hangul code input mode 
(define XK_Hangul_Jeonja                 #xff38)  ; Jeonja mode 
(define XK_Hangul_Banja                  #xff39)  ; Banja mode 
(define XK_Hangul_PreHanja               #xff3a)  ; Pre Hanja conversion 
(define XK_Hangul_PostHanja              #xff3b)  ; Post Hanja conversion 
(define XK_Hangul_SingleCandidate        #xff3c)  ; Single candidate 
(define XK_Hangul_MultipleCandidate      #xff3d)  ; Multiple candidate 
(define XK_Hangul_PreviousCandidate      #xff3e)  ; Previous candidate 
(define XK_Hangul_Special                #xff3f)  ; Special symbols 
(define XK_Hangul_switch                 #xff7e)  ; Alias for mode_switch 

; Hangul Consonant Characters 
(define XK_Hangul_Kiyeog                 #x0ea1)
(define XK_Hangul_SsangKiyeog            #x0ea2)
(define XK_Hangul_KiyeogSios             #x0ea3)
(define XK_Hangul_Nieun                  #x0ea4)
(define XK_Hangul_NieunJieuj             #x0ea5)
(define XK_Hangul_NieunHieuh             #x0ea6)
(define XK_Hangul_Dikeud                 #x0ea7)
(define XK_Hangul_SsangDikeud            #x0ea8)
(define XK_Hangul_Rieul                  #x0ea9)
(define XK_Hangul_RieulKiyeog            #x0eaa)
(define XK_Hangul_RieulMieum             #x0eab)
(define XK_Hangul_RieulPieub             #x0eac)
(define XK_Hangul_RieulSios              #x0ead)
(define XK_Hangul_RieulTieut             #x0eae)
(define XK_Hangul_RieulPhieuf            #x0eaf)
(define XK_Hangul_RieulHieuh             #x0eb0)
(define XK_Hangul_Mieum                  #x0eb1)
(define XK_Hangul_Pieub                  #x0eb2)
(define XK_Hangul_SsangPieub             #x0eb3)
(define XK_Hangul_PieubSios              #x0eb4)
(define XK_Hangul_Sios                   #x0eb5)
(define XK_Hangul_SsangSios              #x0eb6)
(define XK_Hangul_Ieung                  #x0eb7)
(define XK_Hangul_Jieuj                  #x0eb8)
(define XK_Hangul_SsangJieuj             #x0eb9)
(define XK_Hangul_Cieuc                  #x0eba)
(define XK_Hangul_Khieuq                 #x0ebb)
(define XK_Hangul_Tieut                  #x0ebc)
(define XK_Hangul_Phieuf                 #x0ebd)
(define XK_Hangul_Hieuh                  #x0ebe)

; Hangul Vowel Characters 
(define XK_Hangul_A                      #x0ebf)
(define XK_Hangul_AE                     #x0ec0)
(define XK_Hangul_YA                     #x0ec1)
(define XK_Hangul_YAE                    #x0ec2)
(define XK_Hangul_EO                     #x0ec3)
(define XK_Hangul_E                      #x0ec4)
(define XK_Hangul_YEO                    #x0ec5)
(define XK_Hangul_YE                     #x0ec6)
(define XK_Hangul_O                      #x0ec7)
(define XK_Hangul_WA                     #x0ec8)
(define XK_Hangul_WAE                    #x0ec9)
(define XK_Hangul_OE                     #x0eca)
(define XK_Hangul_YO                     #x0ecb)
(define XK_Hangul_U                      #x0ecc)
(define XK_Hangul_WEO                    #x0ecd)
(define XK_Hangul_WE                     #x0ece)
(define XK_Hangul_WI                     #x0ecf)
(define XK_Hangul_YU                     #x0ed0)
(define XK_Hangul_EU                     #x0ed1)
(define XK_Hangul_YI                     #x0ed2)
(define XK_Hangul_I                      #x0ed3)

; Hangul syllable-final (JongSeong) Characters 
(define XK_Hangul_J_Kiyeog               #x0ed4)
(define XK_Hangul_J_SsangKiyeog          #x0ed5)
(define XK_Hangul_J_KiyeogSios           #x0ed6)
(define XK_Hangul_J_Nieun                #x0ed7)
(define XK_Hangul_J_NieunJieuj           #x0ed8)
(define XK_Hangul_J_NieunHieuh           #x0ed9)
(define XK_Hangul_J_Dikeud               #x0eda)
(define XK_Hangul_J_Rieul                #x0edb)
(define XK_Hangul_J_RieulKiyeog          #x0edc)
(define XK_Hangul_J_RieulMieum           #x0edd)
(define XK_Hangul_J_RieulPieub           #x0ede)
(define XK_Hangul_J_RieulSios            #x0edf)
(define XK_Hangul_J_RieulTieut           #x0ee0)
(define XK_Hangul_J_RieulPhieuf          #x0ee1)
(define XK_Hangul_J_RieulHieuh           #x0ee2)
(define XK_Hangul_J_Mieum                #x0ee3)
(define XK_Hangul_J_Pieub                #x0ee4)
(define XK_Hangul_J_PieubSios            #x0ee5)
(define XK_Hangul_J_Sios                 #x0ee6)
(define XK_Hangul_J_SsangSios            #x0ee7)
(define XK_Hangul_J_Ieung                #x0ee8)
(define XK_Hangul_J_Jieuj                #x0ee9)
(define XK_Hangul_J_Cieuc                #x0eea)
(define XK_Hangul_J_Khieuq               #x0eeb)
(define XK_Hangul_J_Tieut                #x0eec)
(define XK_Hangul_J_Phieuf               #x0eed)
(define XK_Hangul_J_Hieuh                #x0eee)

; Ancient Hangul Consonant Characters 
(define XK_Hangul_RieulYeorinHieuh       #x0eef)
(define XK_Hangul_SunkyeongeumMieum      #x0ef0)
(define XK_Hangul_SunkyeongeumPieub      #x0ef1)
(define XK_Hangul_PanSios                #x0ef2)
(define XK_Hangul_KkogjiDalrinIeung      #x0ef3)
(define XK_Hangul_SunkyeongeumPhieuf     #x0ef4)
(define XK_Hangul_YeorinHieuh            #x0ef5)

; Ancient Hangul Vowel Characters 
(define XK_Hangul_AraeA                  #x0ef6)
(define XK_Hangul_AraeAE                 #x0ef7)

; Ancient Hangul syllable-final (JongSeong) Characters 
(define XK_Hangul_J_PanSios              #x0ef8)
(define XK_Hangul_J_KkogjiDalrinIeung    #x0ef9)
(define XK_Hangul_J_YeorinHieuh          #x0efa)

; Korean currency symbol 
(define XK_Korean_Won                    #x0eff)  ;(U+20A9 WON SIGN)

;#endif ; XK_KOREAN 

#|
 * Armenian
 |#

;#ifdef XK_ARMENIAN
(define XK_Armenian_ligature_ew       #x1000587)  ; U+0587 ARMENIAN SMALL LIGATURE ECH YIWN 
(define XK_Armenian_full_stop         #x1000589)  ; U+0589 ARMENIAN FULL STOP 
(define XK_Armenian_verjaket          #x1000589)  ; U+0589 ARMENIAN FULL STOP 
(define XK_Armenian_separation_mark   #x100055d)  ; U+055D ARMENIAN COMMA 
(define XK_Armenian_but               #x100055d)  ; U+055D ARMENIAN COMMA 
(define XK_Armenian_hyphen            #x100058a)  ; U+058A ARMENIAN HYPHEN 
(define XK_Armenian_yentamna          #x100058a)  ; U+058A ARMENIAN HYPHEN 
(define XK_Armenian_exclam            #x100055c)  ; U+055C ARMENIAN EXCLAMATION MARK 
(define XK_Armenian_amanak            #x100055c)  ; U+055C ARMENIAN EXCLAMATION MARK 
(define XK_Armenian_accent            #x100055b)  ; U+055B ARMENIAN EMPHASIS MARK 
(define XK_Armenian_shesht            #x100055b)  ; U+055B ARMENIAN EMPHASIS MARK 
(define XK_Armenian_question          #x100055e)  ; U+055E ARMENIAN QUESTION MARK 
(define XK_Armenian_paruyk            #x100055e)  ; U+055E ARMENIAN QUESTION MARK 
(define XK_Armenian_AYB               #x1000531)  ; U+0531 ARMENIAN CAPITAL LETTER AYB 
(define XK_Armenian_ayb               #x1000561)  ; U+0561 ARMENIAN SMALL LETTER AYB 
(define XK_Armenian_BEN               #x1000532)  ; U+0532 ARMENIAN CAPITAL LETTER BEN 
(define XK_Armenian_ben               #x1000562)  ; U+0562 ARMENIAN SMALL LETTER BEN 
(define XK_Armenian_GIM               #x1000533)  ; U+0533 ARMENIAN CAPITAL LETTER GIM 
(define XK_Armenian_gim               #x1000563)  ; U+0563 ARMENIAN SMALL LETTER GIM 
(define XK_Armenian_DA                #x1000534)  ; U+0534 ARMENIAN CAPITAL LETTER DA 
(define XK_Armenian_da                #x1000564)  ; U+0564 ARMENIAN SMALL LETTER DA 
(define XK_Armenian_YECH              #x1000535)  ; U+0535 ARMENIAN CAPITAL LETTER ECH 
(define XK_Armenian_yech              #x1000565)  ; U+0565 ARMENIAN SMALL LETTER ECH 
(define XK_Armenian_ZA                #x1000536)  ; U+0536 ARMENIAN CAPITAL LETTER ZA 
(define XK_Armenian_za                #x1000566)  ; U+0566 ARMENIAN SMALL LETTER ZA 
(define XK_Armenian_E                 #x1000537)  ; U+0537 ARMENIAN CAPITAL LETTER EH 
(define XK_Armenian_e                 #x1000567)  ; U+0567 ARMENIAN SMALL LETTER EH 
(define XK_Armenian_AT                #x1000538)  ; U+0538 ARMENIAN CAPITAL LETTER ET 
(define XK_Armenian_at                #x1000568)  ; U+0568 ARMENIAN SMALL LETTER ET 
(define XK_Armenian_TO                #x1000539)  ; U+0539 ARMENIAN CAPITAL LETTER TO 
(define XK_Armenian_to                #x1000569)  ; U+0569 ARMENIAN SMALL LETTER TO 
(define XK_Armenian_ZHE               #x100053a)  ; U+053A ARMENIAN CAPITAL LETTER ZHE 
(define XK_Armenian_zhe               #x100056a)  ; U+056A ARMENIAN SMALL LETTER ZHE 
(define XK_Armenian_INI               #x100053b)  ; U+053B ARMENIAN CAPITAL LETTER INI 
(define XK_Armenian_ini               #x100056b)  ; U+056B ARMENIAN SMALL LETTER INI 
(define XK_Armenian_LYUN              #x100053c)  ; U+053C ARMENIAN CAPITAL LETTER LIWN 
(define XK_Armenian_lyun              #x100056c)  ; U+056C ARMENIAN SMALL LETTER LIWN 
(define XK_Armenian_KHE               #x100053d)  ; U+053D ARMENIAN CAPITAL LETTER XEH 
(define XK_Armenian_khe               #x100056d)  ; U+056D ARMENIAN SMALL LETTER XEH 
(define XK_Armenian_TSA               #x100053e)  ; U+053E ARMENIAN CAPITAL LETTER CA 
(define XK_Armenian_tsa               #x100056e)  ; U+056E ARMENIAN SMALL LETTER CA 
(define XK_Armenian_KEN               #x100053f)  ; U+053F ARMENIAN CAPITAL LETTER KEN 
(define XK_Armenian_ken               #x100056f)  ; U+056F ARMENIAN SMALL LETTER KEN 
(define XK_Armenian_HO                #x1000540)  ; U+0540 ARMENIAN CAPITAL LETTER HO 
(define XK_Armenian_ho                #x1000570)  ; U+0570 ARMENIAN SMALL LETTER HO 
(define XK_Armenian_DZA               #x1000541)  ; U+0541 ARMENIAN CAPITAL LETTER JA 
(define XK_Armenian_dza               #x1000571)  ; U+0571 ARMENIAN SMALL LETTER JA 
(define XK_Armenian_GHAT              #x1000542)  ; U+0542 ARMENIAN CAPITAL LETTER GHAD 
(define XK_Armenian_ghat              #x1000572)  ; U+0572 ARMENIAN SMALL LETTER GHAD 
(define XK_Armenian_TCHE              #x1000543)  ; U+0543 ARMENIAN CAPITAL LETTER CHEH 
(define XK_Armenian_tche              #x1000573)  ; U+0573 ARMENIAN SMALL LETTER CHEH 
(define XK_Armenian_MEN               #x1000544)  ; U+0544 ARMENIAN CAPITAL LETTER MEN 
(define XK_Armenian_men               #x1000574)  ; U+0574 ARMENIAN SMALL LETTER MEN 
(define XK_Armenian_HI                #x1000545)  ; U+0545 ARMENIAN CAPITAL LETTER YI 
(define XK_Armenian_hi                #x1000575)  ; U+0575 ARMENIAN SMALL LETTER YI 
(define XK_Armenian_NU                #x1000546)  ; U+0546 ARMENIAN CAPITAL LETTER NOW 
(define XK_Armenian_nu                #x1000576)  ; U+0576 ARMENIAN SMALL LETTER NOW 
(define XK_Armenian_SHA               #x1000547)  ; U+0547 ARMENIAN CAPITAL LETTER SHA 
(define XK_Armenian_sha               #x1000577)  ; U+0577 ARMENIAN SMALL LETTER SHA 
(define XK_Armenian_VO                #x1000548)  ; U+0548 ARMENIAN CAPITAL LETTER VO 
(define XK_Armenian_vo                #x1000578)  ; U+0578 ARMENIAN SMALL LETTER VO 
(define XK_Armenian_CHA               #x1000549)  ; U+0549 ARMENIAN CAPITAL LETTER CHA 
(define XK_Armenian_cha               #x1000579)  ; U+0579 ARMENIAN SMALL LETTER CHA 
(define XK_Armenian_PE                #x100054a)  ; U+054A ARMENIAN CAPITAL LETTER PEH 
(define XK_Armenian_pe                #x100057a)  ; U+057A ARMENIAN SMALL LETTER PEH 
(define XK_Armenian_JE                #x100054b)  ; U+054B ARMENIAN CAPITAL LETTER JHEH 
(define XK_Armenian_je                #x100057b)  ; U+057B ARMENIAN SMALL LETTER JHEH 
(define XK_Armenian_RA                #x100054c)  ; U+054C ARMENIAN CAPITAL LETTER RA 
(define XK_Armenian_ra                #x100057c)  ; U+057C ARMENIAN SMALL LETTER RA 
(define XK_Armenian_SE                #x100054d)  ; U+054D ARMENIAN CAPITAL LETTER SEH 
(define XK_Armenian_se                #x100057d)  ; U+057D ARMENIAN SMALL LETTER SEH 
(define XK_Armenian_VEV               #x100054e)  ; U+054E ARMENIAN CAPITAL LETTER VEW 
(define XK_Armenian_vev               #x100057e)  ; U+057E ARMENIAN SMALL LETTER VEW 
(define XK_Armenian_TYUN              #x100054f)  ; U+054F ARMENIAN CAPITAL LETTER TIWN 
(define XK_Armenian_tyun              #x100057f)  ; U+057F ARMENIAN SMALL LETTER TIWN 
(define XK_Armenian_RE                #x1000550)  ; U+0550 ARMENIAN CAPITAL LETTER REH 
(define XK_Armenian_re                #x1000580)  ; U+0580 ARMENIAN SMALL LETTER REH 
(define XK_Armenian_TSO               #x1000551)  ; U+0551 ARMENIAN CAPITAL LETTER CO 
(define XK_Armenian_tso               #x1000581)  ; U+0581 ARMENIAN SMALL LETTER CO 
(define XK_Armenian_VYUN              #x1000552)  ; U+0552 ARMENIAN CAPITAL LETTER YIWN 
(define XK_Armenian_vyun              #x1000582)  ; U+0582 ARMENIAN SMALL LETTER YIWN 
(define XK_Armenian_PYUR              #x1000553)  ; U+0553 ARMENIAN CAPITAL LETTER PIWR 
(define XK_Armenian_pyur              #x1000583)  ; U+0583 ARMENIAN SMALL LETTER PIWR 
(define XK_Armenian_KE                #x1000554)  ; U+0554 ARMENIAN CAPITAL LETTER KEH 
(define XK_Armenian_ke                #x1000584)  ; U+0584 ARMENIAN SMALL LETTER KEH 
(define XK_Armenian_O                 #x1000555)  ; U+0555 ARMENIAN CAPITAL LETTER OH 
(define XK_Armenian_o                 #x1000585)  ; U+0585 ARMENIAN SMALL LETTER OH 
(define XK_Armenian_FE                #x1000556)  ; U+0556 ARMENIAN CAPITAL LETTER FEH 
(define XK_Armenian_fe                #x1000586)  ; U+0586 ARMENIAN SMALL LETTER FEH 
(define XK_Armenian_apostrophe        #x100055a)  ; U+055A ARMENIAN APOSTROPHE 
;#endif ; XK_ARMENIAN 

#|
 * Georgian
 |#

;#ifdef XK_GEORGIAN
(define XK_Georgian_an                #x10010d0)  ; U+10D0 GEORGIAN LETTER AN 
(define XK_Georgian_ban               #x10010d1)  ; U+10D1 GEORGIAN LETTER BAN 
(define XK_Georgian_gan               #x10010d2)  ; U+10D2 GEORGIAN LETTER GAN 
(define XK_Georgian_don               #x10010d3)  ; U+10D3 GEORGIAN LETTER DON 
(define XK_Georgian_en                #x10010d4)  ; U+10D4 GEORGIAN LETTER EN 
(define XK_Georgian_vin               #x10010d5)  ; U+10D5 GEORGIAN LETTER VIN 
(define XK_Georgian_zen               #x10010d6)  ; U+10D6 GEORGIAN LETTER ZEN 
(define XK_Georgian_tan               #x10010d7)  ; U+10D7 GEORGIAN LETTER TAN 
(define XK_Georgian_in                #x10010d8)  ; U+10D8 GEORGIAN LETTER IN 
(define XK_Georgian_kan               #x10010d9)  ; U+10D9 GEORGIAN LETTER KAN 
(define XK_Georgian_las               #x10010da)  ; U+10DA GEORGIAN LETTER LAS 
(define XK_Georgian_man               #x10010db)  ; U+10DB GEORGIAN LETTER MAN 
(define XK_Georgian_nar               #x10010dc)  ; U+10DC GEORGIAN LETTER NAR 
(define XK_Georgian_on                #x10010dd)  ; U+10DD GEORGIAN LETTER ON 
(define XK_Georgian_par               #x10010de)  ; U+10DE GEORGIAN LETTER PAR 
(define XK_Georgian_zhar              #x10010df)  ; U+10DF GEORGIAN LETTER ZHAR 
(define XK_Georgian_rae               #x10010e0)  ; U+10E0 GEORGIAN LETTER RAE 
(define XK_Georgian_san               #x10010e1)  ; U+10E1 GEORGIAN LETTER SAN 
(define XK_Georgian_tar               #x10010e2)  ; U+10E2 GEORGIAN LETTER TAR 
(define XK_Georgian_un                #x10010e3)  ; U+10E3 GEORGIAN LETTER UN 
(define XK_Georgian_phar              #x10010e4)  ; U+10E4 GEORGIAN LETTER PHAR 
(define XK_Georgian_khar              #x10010e5)  ; U+10E5 GEORGIAN LETTER KHAR 
(define XK_Georgian_ghan              #x10010e6)  ; U+10E6 GEORGIAN LETTER GHAN 
(define XK_Georgian_qar               #x10010e7)  ; U+10E7 GEORGIAN LETTER QAR 
(define XK_Georgian_shin              #x10010e8)  ; U+10E8 GEORGIAN LETTER SHIN 
(define XK_Georgian_chin              #x10010e9)  ; U+10E9 GEORGIAN LETTER CHIN 
(define XK_Georgian_can               #x10010ea)  ; U+10EA GEORGIAN LETTER CAN 
(define XK_Georgian_jil               #x10010eb)  ; U+10EB GEORGIAN LETTER JIL 
(define XK_Georgian_cil               #x10010ec)  ; U+10EC GEORGIAN LETTER CIL 
(define XK_Georgian_char              #x10010ed)  ; U+10ED GEORGIAN LETTER CHAR 
(define XK_Georgian_xan               #x10010ee)  ; U+10EE GEORGIAN LETTER XAN 
(define XK_Georgian_jhan              #x10010ef)  ; U+10EF GEORGIAN LETTER JHAN 
(define XK_Georgian_hae               #x10010f0)  ; U+10F0 GEORGIAN LETTER HAE 
(define XK_Georgian_he                #x10010f1)  ; U+10F1 GEORGIAN LETTER HE 
(define XK_Georgian_hie               #x10010f2)  ; U+10F2 GEORGIAN LETTER HIE 
(define XK_Georgian_we                #x10010f3)  ; U+10F3 GEORGIAN LETTER WE 
(define XK_Georgian_har               #x10010f4)  ; U+10F4 GEORGIAN LETTER HAR 
(define XK_Georgian_hoe               #x10010f5)  ; U+10F5 GEORGIAN LETTER HOE 
(define XK_Georgian_fi                #x10010f6)  ; U+10F6 GEORGIAN LETTER FI 
;#endif ; XK_GEORGIAN 

#|
 * Azeri (and other Turkic or Caucasian languages)
 |#

;#ifdef XK_CAUCASUS
; latin 
(define XK_Xabovedot                  #x1001e8a)  ; U+1E8A LATIN CAPITAL LETTER X WITH DOT ABOVE 
(define XK_Ibreve                     #x100012c)  ; U+012C LATIN CAPITAL LETTER I WITH BREVE 
(define XK_Zstroke                    #x10001b5)  ; U+01B5 LATIN CAPITAL LETTER Z WITH STROKE 
(define XK_Gcaron                     #x10001e6)  ; U+01E6 LATIN CAPITAL LETTER G WITH CARON 
(define XK_Ocaron                     #x10001d1)  ; U+01D2 LATIN CAPITAL LETTER O WITH CARON 
(define XK_Obarred                    #x100019f)  ; U+019F LATIN CAPITAL LETTER O WITH MIDDLE TILDE 
(define XK_xabovedot                  #x1001e8b)  ; U+1E8B LATIN SMALL LETTER X WITH DOT ABOVE 
(define XK_ibreve                     #x100012d)  ; U+012D LATIN SMALL LETTER I WITH BREVE 
(define XK_zstroke                    #x10001b6)  ; U+01B6 LATIN SMALL LETTER Z WITH STROKE 
(define XK_gcaron                     #x10001e7)  ; U+01E7 LATIN SMALL LETTER G WITH CARON 
(define XK_ocaron                     #x10001d2)  ; U+01D2 LATIN SMALL LETTER O WITH CARON 
(define XK_obarred                    #x1000275)  ; U+0275 LATIN SMALL LETTER BARRED O 
(define XK_SCHWA                      #x100018f)  ; U+018F LATIN CAPITAL LETTER SCHWA 
(define XK_schwa                      #x1000259)  ; U+0259 LATIN SMALL LETTER SCHWA 
; those are not really Caucasus 
; For Inupiak 
(define XK_Lbelowdot                  #x1001e36)  ; U+1E36 LATIN CAPITAL LETTER L WITH DOT BELOW 
(define XK_lbelowdot                  #x1001e37)  ; U+1E37 LATIN SMALL LETTER L WITH DOT BELOW 
;#endif ; XK_CAUCASUS 

#|
 * Vietnamese
 |#
 
;#ifdef XK_VIETNAMESE
(define XK_Abelowdot                  #x1001ea0)  ; U+1EA0 LATIN CAPITAL LETTER A WITH DOT BELOW 
(define XK_abelowdot                  #x1001ea1)  ; U+1EA1 LATIN SMALL LETTER A WITH DOT BELOW 
(define XK_Ahook                      #x1001ea2)  ; U+1EA2 LATIN CAPITAL LETTER A WITH HOOK ABOVE 
(define XK_ahook                      #x1001ea3)  ; U+1EA3 LATIN SMALL LETTER A WITH HOOK ABOVE 
(define XK_Acircumflexacute           #x1001ea4)  ; U+1EA4 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE 
(define XK_acircumflexacute           #x1001ea5)  ; U+1EA5 LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE 
(define XK_Acircumflexgrave           #x1001ea6)  ; U+1EA6 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE 
(define XK_acircumflexgrave           #x1001ea7)  ; U+1EA7 LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE 
(define XK_Acircumflexhook            #x1001ea8)  ; U+1EA8 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE 
(define XK_acircumflexhook            #x1001ea9)  ; U+1EA9 LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE 
(define XK_Acircumflextilde           #x1001eaa)  ; U+1EAA LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE 
(define XK_acircumflextilde           #x1001eab)  ; U+1EAB LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE 
(define XK_Acircumflexbelowdot        #x1001eac)  ; U+1EAC LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW 
(define XK_acircumflexbelowdot        #x1001ead)  ; U+1EAD LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW 
(define XK_Abreveacute                #x1001eae)  ; U+1EAE LATIN CAPITAL LETTER A WITH BREVE AND ACUTE 
(define XK_abreveacute                #x1001eaf)  ; U+1EAF LATIN SMALL LETTER A WITH BREVE AND ACUTE 
(define XK_Abrevegrave                #x1001eb0)  ; U+1EB0 LATIN CAPITAL LETTER A WITH BREVE AND GRAVE 
(define XK_abrevegrave                #x1001eb1)  ; U+1EB1 LATIN SMALL LETTER A WITH BREVE AND GRAVE 
(define XK_Abrevehook                 #x1001eb2)  ; U+1EB2 LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE 
(define XK_abrevehook                 #x1001eb3)  ; U+1EB3 LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE 
(define XK_Abrevetilde                #x1001eb4)  ; U+1EB4 LATIN CAPITAL LETTER A WITH BREVE AND TILDE 
(define XK_abrevetilde                #x1001eb5)  ; U+1EB5 LATIN SMALL LETTER A WITH BREVE AND TILDE 
(define XK_Abrevebelowdot             #x1001eb6)  ; U+1EB6 LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW 
(define XK_abrevebelowdot             #x1001eb7)  ; U+1EB7 LATIN SMALL LETTER A WITH BREVE AND DOT BELOW 
(define XK_Ebelowdot                  #x1001eb8)  ; U+1EB8 LATIN CAPITAL LETTER E WITH DOT BELOW 
(define XK_ebelowdot                  #x1001eb9)  ; U+1EB9 LATIN SMALL LETTER E WITH DOT BELOW 
(define XK_Ehook                      #x1001eba)  ; U+1EBA LATIN CAPITAL LETTER E WITH HOOK ABOVE 
(define XK_ehook                      #x1001ebb)  ; U+1EBB LATIN SMALL LETTER E WITH HOOK ABOVE 
(define XK_Etilde                     #x1001ebc)  ; U+1EBC LATIN CAPITAL LETTER E WITH TILDE 
(define XK_etilde                     #x1001ebd)  ; U+1EBD LATIN SMALL LETTER E WITH TILDE 
(define XK_Ecircumflexacute           #x1001ebe)  ; U+1EBE LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE 
(define XK_ecircumflexacute           #x1001ebf)  ; U+1EBF LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE 
(define XK_Ecircumflexgrave           #x1001ec0)  ; U+1EC0 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE 
(define XK_ecircumflexgrave           #x1001ec1)  ; U+1EC1 LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE 
(define XK_Ecircumflexhook            #x1001ec2)  ; U+1EC2 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE 
(define XK_ecircumflexhook            #x1001ec3)  ; U+1EC3 LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE 
(define XK_Ecircumflextilde           #x1001ec4)  ; U+1EC4 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE 
(define XK_ecircumflextilde           #x1001ec5)  ; U+1EC5 LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE 
(define XK_Ecircumflexbelowdot        #x1001ec6)  ; U+1EC6 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW 
(define XK_ecircumflexbelowdot        #x1001ec7)  ; U+1EC7 LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW 
(define XK_Ihook                      #x1001ec8)  ; U+1EC8 LATIN CAPITAL LETTER I WITH HOOK ABOVE 
(define XK_ihook                      #x1001ec9)  ; U+1EC9 LATIN SMALL LETTER I WITH HOOK ABOVE 
(define XK_Ibelowdot                  #x1001eca)  ; U+1ECA LATIN CAPITAL LETTER I WITH DOT BELOW 
(define XK_ibelowdot                  #x1001ecb)  ; U+1ECB LATIN SMALL LETTER I WITH DOT BELOW 
(define XK_Obelowdot                  #x1001ecc)  ; U+1ECC LATIN CAPITAL LETTER O WITH DOT BELOW 
(define XK_obelowdot                  #x1001ecd)  ; U+1ECD LATIN SMALL LETTER O WITH DOT BELOW 
(define XK_Ohook                      #x1001ece)  ; U+1ECE LATIN CAPITAL LETTER O WITH HOOK ABOVE 
(define XK_ohook                      #x1001ecf)  ; U+1ECF LATIN SMALL LETTER O WITH HOOK ABOVE 
(define XK_Ocircumflexacute           #x1001ed0)  ; U+1ED0 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE 
(define XK_ocircumflexacute           #x1001ed1)  ; U+1ED1 LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE 
(define XK_Ocircumflexgrave           #x1001ed2)  ; U+1ED2 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE 
(define XK_ocircumflexgrave           #x1001ed3)  ; U+1ED3 LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE 
(define XK_Ocircumflexhook            #x1001ed4)  ; U+1ED4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE 
(define XK_ocircumflexhook            #x1001ed5)  ; U+1ED5 LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE 
(define XK_Ocircumflextilde           #x1001ed6)  ; U+1ED6 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE 
(define XK_ocircumflextilde           #x1001ed7)  ; U+1ED7 LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE 
(define XK_Ocircumflexbelowdot        #x1001ed8)  ; U+1ED8 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW 
(define XK_ocircumflexbelowdot        #x1001ed9)  ; U+1ED9 LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW 
(define XK_Ohornacute                 #x1001eda)  ; U+1EDA LATIN CAPITAL LETTER O WITH HORN AND ACUTE 
(define XK_ohornacute                 #x1001edb)  ; U+1EDB LATIN SMALL LETTER O WITH HORN AND ACUTE 
(define XK_Ohorngrave                 #x1001edc)  ; U+1EDC LATIN CAPITAL LETTER O WITH HORN AND GRAVE 
(define XK_ohorngrave                 #x1001edd)  ; U+1EDD LATIN SMALL LETTER O WITH HORN AND GRAVE 
(define XK_Ohornhook                  #x1001ede)  ; U+1EDE LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE 
(define XK_ohornhook                  #x1001edf)  ; U+1EDF LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE 
(define XK_Ohorntilde                 #x1001ee0)  ; U+1EE0 LATIN CAPITAL LETTER O WITH HORN AND TILDE 
(define XK_ohorntilde                 #x1001ee1)  ; U+1EE1 LATIN SMALL LETTER O WITH HORN AND TILDE 
(define XK_Ohornbelowdot              #x1001ee2)  ; U+1EE2 LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW 
(define XK_ohornbelowdot              #x1001ee3)  ; U+1EE3 LATIN SMALL LETTER O WITH HORN AND DOT BELOW 
(define XK_Ubelowdot                  #x1001ee4)  ; U+1EE4 LATIN CAPITAL LETTER U WITH DOT BELOW 
(define XK_ubelowdot                  #x1001ee5)  ; U+1EE5 LATIN SMALL LETTER U WITH DOT BELOW 
(define XK_Uhook                      #x1001ee6)  ; U+1EE6 LATIN CAPITAL LETTER U WITH HOOK ABOVE 
(define XK_uhook                      #x1001ee7)  ; U+1EE7 LATIN SMALL LETTER U WITH HOOK ABOVE 
(define XK_Uhornacute                 #x1001ee8)  ; U+1EE8 LATIN CAPITAL LETTER U WITH HORN AND ACUTE 
(define XK_uhornacute                 #x1001ee9)  ; U+1EE9 LATIN SMALL LETTER U WITH HORN AND ACUTE 
(define XK_Uhorngrave                 #x1001eea)  ; U+1EEA LATIN CAPITAL LETTER U WITH HORN AND GRAVE 
(define XK_uhorngrave                 #x1001eeb)  ; U+1EEB LATIN SMALL LETTER U WITH HORN AND GRAVE 
(define XK_Uhornhook                  #x1001eec)  ; U+1EEC LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE 
(define XK_uhornhook                  #x1001eed)  ; U+1EED LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE 
(define XK_Uhorntilde                 #x1001eee)  ; U+1EEE LATIN CAPITAL LETTER U WITH HORN AND TILDE 
(define XK_uhorntilde                 #x1001eef)  ; U+1EEF LATIN SMALL LETTER U WITH HORN AND TILDE 
(define XK_Uhornbelowdot              #x1001ef0)  ; U+1EF0 LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW 
(define XK_uhornbelowdot              #x1001ef1)  ; U+1EF1 LATIN SMALL LETTER U WITH HORN AND DOT BELOW 
(define XK_Ybelowdot                  #x1001ef4)  ; U+1EF4 LATIN CAPITAL LETTER Y WITH DOT BELOW 
(define XK_ybelowdot                  #x1001ef5)  ; U+1EF5 LATIN SMALL LETTER Y WITH DOT BELOW 
(define XK_Yhook                      #x1001ef6)  ; U+1EF6 LATIN CAPITAL LETTER Y WITH HOOK ABOVE 
(define XK_yhook                      #x1001ef7)  ; U+1EF7 LATIN SMALL LETTER Y WITH HOOK ABOVE 
(define XK_Ytilde                     #x1001ef8)  ; U+1EF8 LATIN CAPITAL LETTER Y WITH TILDE 
(define XK_ytilde                     #x1001ef9)  ; U+1EF9 LATIN SMALL LETTER Y WITH TILDE 
(define XK_Ohorn                      #x10001a0)  ; U+01A0 LATIN CAPITAL LETTER O WITH HORN 
(define XK_ohorn                      #x10001a1)  ; U+01A1 LATIN SMALL LETTER O WITH HORN 
(define XK_Uhorn                      #x10001af)  ; U+01AF LATIN CAPITAL LETTER U WITH HORN 
(define XK_uhorn                      #x10001b0)  ; U+01B0 LATIN SMALL LETTER U WITH HORN 

;#endif ; XK_VIETNAMESE 

;#ifdef XK_CURRENCY
(define XK_EcuSign                    #x10020a0)  ; U+20A0 EURO-CURRENCY SIGN 
(define XK_ColonSign                  #x10020a1)  ; U+20A1 COLON SIGN 
(define XK_CruzeiroSign               #x10020a2)  ; U+20A2 CRUZEIRO SIGN 
(define XK_FFrancSign                 #x10020a3)  ; U+20A3 FRENCH FRANC SIGN 
(define XK_LiraSign                   #x10020a4)  ; U+20A4 LIRA SIGN 
(define XK_MillSign                   #x10020a5)  ; U+20A5 MILL SIGN 
(define XK_NairaSign                  #x10020a6)  ; U+20A6 NAIRA SIGN 
(define XK_PesetaSign                 #x10020a7)  ; U+20A7 PESETA SIGN 
(define XK_RupeeSign                  #x10020a8)  ; U+20A8 RUPEE SIGN 
(define XK_WonSign                    #x10020a9)  ; U+20A9 WON SIGN 
(define XK_NewSheqelSign              #x10020aa)  ; U+20AA NEW SHEQEL SIGN 
(define XK_DongSign                   #x10020ab)  ; U+20AB DONG SIGN 
(define XK_EuroSign                      #x20ac)  ; U+20AC EURO SIGN 
;#endif ; XK_CURRENCY 

;#ifdef XK_MATHEMATICAL
; one, two and three are defined above. 
(define XK_zerosuperior               #x1002070)  ; U+2070 SUPERSCRIPT ZERO 
(define XK_foursuperior               #x1002074)  ; U+2074 SUPERSCRIPT FOUR 
(define XK_fivesuperior               #x1002075)  ; U+2075 SUPERSCRIPT FIVE 
(define XK_sixsuperior                #x1002076)  ; U+2076 SUPERSCRIPT SIX 
(define XK_sevensuperior              #x1002077)  ; U+2077 SUPERSCRIPT SEVEN 
(define XK_eightsuperior              #x1002078)  ; U+2078 SUPERSCRIPT EIGHT 
(define XK_ninesuperior               #x1002079)  ; U+2079 SUPERSCRIPT NINE 
(define XK_zerosubscript              #x1002080)  ; U+2080 SUBSCRIPT ZERO 
(define XK_onesubscript               #x1002081)  ; U+2081 SUBSCRIPT ONE 
(define XK_twosubscript               #x1002082)  ; U+2082 SUBSCRIPT TWO 
(define XK_threesubscript             #x1002083)  ; U+2083 SUBSCRIPT THREE 
(define XK_foursubscript              #x1002084)  ; U+2084 SUBSCRIPT FOUR 
(define XK_fivesubscript              #x1002085)  ; U+2085 SUBSCRIPT FIVE 
(define XK_sixsubscript               #x1002086)  ; U+2086 SUBSCRIPT SIX 
(define XK_sevensubscript             #x1002087)  ; U+2087 SUBSCRIPT SEVEN 
(define XK_eightsubscript             #x1002088)  ; U+2088 SUBSCRIPT EIGHT 
(define XK_ninesubscript              #x1002089)  ; U+2089 SUBSCRIPT NINE 
(define XK_partdifferential           #x1002202)  ; U+2202 PARTIAL DIFFERENTIAL 
(define XK_emptyset                   #x1002205)  ; U+2205 NULL SET 
(define XK_elementof                  #x1002208)  ; U+2208 ELEMENT OF 
(define XK_notelementof               #x1002209)  ; U+2209 NOT AN ELEMENT OF 
(define XK_containsas                 #x100220B)  ; U+220B CONTAINS AS MEMBER 
(define XK_squareroot                 #x100221A)  ; U+221A SQUARE ROOT 
(define XK_cuberoot                   #x100221B)  ; U+221B CUBE ROOT 
(define XK_fourthroot                 #x100221C)  ; U+221C FOURTH ROOT 
(define XK_dintegral                  #x100222C)  ; U+222C DOUBLE INTEGRAL 
(define XK_tintegral                  #x100222D)  ; U+222D TRIPLE INTEGRAL 
(define XK_because                    #x1002235)  ; U+2235 BECAUSE 
(define XK_approxeq                   #x1002248)  ; U+2245 ALMOST EQUAL TO 
(define XK_notapproxeq                #x1002247)  ; U+2247 NOT ALMOST EQUAL TO 
(define XK_notidentical               #x1002262)  ; U+2262 NOT IDENTICAL TO 
(define XK_stricteq                   #x1002263)  ; U+2263 STRICTLY EQUIVALENT TO           
;#endif ; XK_MATHEMATICAL 

;#ifdef XK_BRAILLE
(define XK_braille_dot_1                 #xfff1)
(define XK_braille_dot_2                 #xfff2)
(define XK_braille_dot_3                 #xfff3)
(define XK_braille_dot_4                 #xfff4)
(define XK_braille_dot_5                 #xfff5)
(define XK_braille_dot_6                 #xfff6)
(define XK_braille_dot_7                 #xfff7)
(define XK_braille_dot_8                 #xfff8)
(define XK_braille_dot_9                 #xfff9)
(define XK_braille_dot_10                #xfffa)
(define XK_braille_blank              #x1002800)  ; U+2800 BRAILLE PATTERN BLANK 
(define XK_braille_dots_1             #x1002801)  ; U+2801 BRAILLE PATTERN DOTS-1 
(define XK_braille_dots_2             #x1002802)  ; U+2802 BRAILLE PATTERN DOTS-2 
(define XK_braille_dots_12            #x1002803)  ; U+2803 BRAILLE PATTERN DOTS-12 
(define XK_braille_dots_3             #x1002804)  ; U+2804 BRAILLE PATTERN DOTS-3 
(define XK_braille_dots_13            #x1002805)  ; U+2805 BRAILLE PATTERN DOTS-13 
(define XK_braille_dots_23            #x1002806)  ; U+2806 BRAILLE PATTERN DOTS-23 
(define XK_braille_dots_123           #x1002807)  ; U+2807 BRAILLE PATTERN DOTS-123 
(define XK_braille_dots_4             #x1002808)  ; U+2808 BRAILLE PATTERN DOTS-4 
(define XK_braille_dots_14            #x1002809)  ; U+2809 BRAILLE PATTERN DOTS-14 
(define XK_braille_dots_24            #x100280a)  ; U+280a BRAILLE PATTERN DOTS-24 
(define XK_braille_dots_124           #x100280b)  ; U+280b BRAILLE PATTERN DOTS-124 
(define XK_braille_dots_34            #x100280c)  ; U+280c BRAILLE PATTERN DOTS-34 
(define XK_braille_dots_134           #x100280d)  ; U+280d BRAILLE PATTERN DOTS-134 
(define XK_braille_dots_234           #x100280e)  ; U+280e BRAILLE PATTERN DOTS-234 
(define XK_braille_dots_1234          #x100280f)  ; U+280f BRAILLE PATTERN DOTS-1234 
(define XK_braille_dots_5             #x1002810)  ; U+2810 BRAILLE PATTERN DOTS-5 
(define XK_braille_dots_15            #x1002811)  ; U+2811 BRAILLE PATTERN DOTS-15 
(define XK_braille_dots_25            #x1002812)  ; U+2812 BRAILLE PATTERN DOTS-25 
(define XK_braille_dots_125           #x1002813)  ; U+2813 BRAILLE PATTERN DOTS-125 
(define XK_braille_dots_35            #x1002814)  ; U+2814 BRAILLE PATTERN DOTS-35 
(define XK_braille_dots_135           #x1002815)  ; U+2815 BRAILLE PATTERN DOTS-135 
(define XK_braille_dots_235           #x1002816)  ; U+2816 BRAILLE PATTERN DOTS-235 
(define XK_braille_dots_1235          #x1002817)  ; U+2817 BRAILLE PATTERN DOTS-1235 
(define XK_braille_dots_45            #x1002818)  ; U+2818 BRAILLE PATTERN DOTS-45 
(define XK_braille_dots_145           #x1002819)  ; U+2819 BRAILLE PATTERN DOTS-145 
(define XK_braille_dots_245           #x100281a)  ; U+281a BRAILLE PATTERN DOTS-245 
(define XK_braille_dots_1245          #x100281b)  ; U+281b BRAILLE PATTERN DOTS-1245 
(define XK_braille_dots_345           #x100281c)  ; U+281c BRAILLE PATTERN DOTS-345 
(define XK_braille_dots_1345          #x100281d)  ; U+281d BRAILLE PATTERN DOTS-1345 
(define XK_braille_dots_2345          #x100281e)  ; U+281e BRAILLE PATTERN DOTS-2345 
(define XK_braille_dots_12345         #x100281f)  ; U+281f BRAILLE PATTERN DOTS-12345 
(define XK_braille_dots_6             #x1002820)  ; U+2820 BRAILLE PATTERN DOTS-6 
(define XK_braille_dots_16            #x1002821)  ; U+2821 BRAILLE PATTERN DOTS-16 
(define XK_braille_dots_26            #x1002822)  ; U+2822 BRAILLE PATTERN DOTS-26 
(define XK_braille_dots_126           #x1002823)  ; U+2823 BRAILLE PATTERN DOTS-126 
(define XK_braille_dots_36            #x1002824)  ; U+2824 BRAILLE PATTERN DOTS-36 
(define XK_braille_dots_136           #x1002825)  ; U+2825 BRAILLE PATTERN DOTS-136 
(define XK_braille_dots_236           #x1002826)  ; U+2826 BRAILLE PATTERN DOTS-236 
(define XK_braille_dots_1236          #x1002827)  ; U+2827 BRAILLE PATTERN DOTS-1236 
(define XK_braille_dots_46            #x1002828)  ; U+2828 BRAILLE PATTERN DOTS-46 
(define XK_braille_dots_146           #x1002829)  ; U+2829 BRAILLE PATTERN DOTS-146 
(define XK_braille_dots_246           #x100282a)  ; U+282a BRAILLE PATTERN DOTS-246 
(define XK_braille_dots_1246          #x100282b)  ; U+282b BRAILLE PATTERN DOTS-1246 
(define XK_braille_dots_346           #x100282c)  ; U+282c BRAILLE PATTERN DOTS-346 
(define XK_braille_dots_1346          #x100282d)  ; U+282d BRAILLE PATTERN DOTS-1346 
(define XK_braille_dots_2346          #x100282e)  ; U+282e BRAILLE PATTERN DOTS-2346 
(define XK_braille_dots_12346         #x100282f)  ; U+282f BRAILLE PATTERN DOTS-12346 
(define XK_braille_dots_56            #x1002830)  ; U+2830 BRAILLE PATTERN DOTS-56 
(define XK_braille_dots_156           #x1002831)  ; U+2831 BRAILLE PATTERN DOTS-156 
(define XK_braille_dots_256           #x1002832)  ; U+2832 BRAILLE PATTERN DOTS-256 
(define XK_braille_dots_1256          #x1002833)  ; U+2833 BRAILLE PATTERN DOTS-1256 
(define XK_braille_dots_356           #x1002834)  ; U+2834 BRAILLE PATTERN DOTS-356 
(define XK_braille_dots_1356          #x1002835)  ; U+2835 BRAILLE PATTERN DOTS-1356 
(define XK_braille_dots_2356          #x1002836)  ; U+2836 BRAILLE PATTERN DOTS-2356 
(define XK_braille_dots_12356         #x1002837)  ; U+2837 BRAILLE PATTERN DOTS-12356 
(define XK_braille_dots_456           #x1002838)  ; U+2838 BRAILLE PATTERN DOTS-456 
(define XK_braille_dots_1456          #x1002839)  ; U+2839 BRAILLE PATTERN DOTS-1456 
(define XK_braille_dots_2456          #x100283a)  ; U+283a BRAILLE PATTERN DOTS-2456 
(define XK_braille_dots_12456         #x100283b)  ; U+283b BRAILLE PATTERN DOTS-12456 
(define XK_braille_dots_3456          #x100283c)  ; U+283c BRAILLE PATTERN DOTS-3456 
(define XK_braille_dots_13456         #x100283d)  ; U+283d BRAILLE PATTERN DOTS-13456 
(define XK_braille_dots_23456         #x100283e)  ; U+283e BRAILLE PATTERN DOTS-23456 
(define XK_braille_dots_123456        #x100283f)  ; U+283f BRAILLE PATTERN DOTS-123456 
(define XK_braille_dots_7             #x1002840)  ; U+2840 BRAILLE PATTERN DOTS-7 
(define XK_braille_dots_17            #x1002841)  ; U+2841 BRAILLE PATTERN DOTS-17 
(define XK_braille_dots_27            #x1002842)  ; U+2842 BRAILLE PATTERN DOTS-27 
(define XK_braille_dots_127           #x1002843)  ; U+2843 BRAILLE PATTERN DOTS-127 
(define XK_braille_dots_37            #x1002844)  ; U+2844 BRAILLE PATTERN DOTS-37 
(define XK_braille_dots_137           #x1002845)  ; U+2845 BRAILLE PATTERN DOTS-137 
(define XK_braille_dots_237           #x1002846)  ; U+2846 BRAILLE PATTERN DOTS-237 
(define XK_braille_dots_1237          #x1002847)  ; U+2847 BRAILLE PATTERN DOTS-1237 
(define XK_braille_dots_47            #x1002848)  ; U+2848 BRAILLE PATTERN DOTS-47 
(define XK_braille_dots_147           #x1002849)  ; U+2849 BRAILLE PATTERN DOTS-147 
(define XK_braille_dots_247           #x100284a)  ; U+284a BRAILLE PATTERN DOTS-247 
(define XK_braille_dots_1247          #x100284b)  ; U+284b BRAILLE PATTERN DOTS-1247 
(define XK_braille_dots_347           #x100284c)  ; U+284c BRAILLE PATTERN DOTS-347 
(define XK_braille_dots_1347          #x100284d)  ; U+284d BRAILLE PATTERN DOTS-1347 
(define XK_braille_dots_2347          #x100284e)  ; U+284e BRAILLE PATTERN DOTS-2347 
(define XK_braille_dots_12347         #x100284f)  ; U+284f BRAILLE PATTERN DOTS-12347 
(define XK_braille_dots_57            #x1002850)  ; U+2850 BRAILLE PATTERN DOTS-57 
(define XK_braille_dots_157           #x1002851)  ; U+2851 BRAILLE PATTERN DOTS-157 
(define XK_braille_dots_257           #x1002852)  ; U+2852 BRAILLE PATTERN DOTS-257 
(define XK_braille_dots_1257          #x1002853)  ; U+2853 BRAILLE PATTERN DOTS-1257 
(define XK_braille_dots_357           #x1002854)  ; U+2854 BRAILLE PATTERN DOTS-357 
(define XK_braille_dots_1357          #x1002855)  ; U+2855 BRAILLE PATTERN DOTS-1357 
(define XK_braille_dots_2357          #x1002856)  ; U+2856 BRAILLE PATTERN DOTS-2357 
(define XK_braille_dots_12357         #x1002857)  ; U+2857 BRAILLE PATTERN DOTS-12357 
(define XK_braille_dots_457           #x1002858)  ; U+2858 BRAILLE PATTERN DOTS-457 
(define XK_braille_dots_1457          #x1002859)  ; U+2859 BRAILLE PATTERN DOTS-1457 
(define XK_braille_dots_2457          #x100285a)  ; U+285a BRAILLE PATTERN DOTS-2457 
(define XK_braille_dots_12457         #x100285b)  ; U+285b BRAILLE PATTERN DOTS-12457 
(define XK_braille_dots_3457          #x100285c)  ; U+285c BRAILLE PATTERN DOTS-3457 
(define XK_braille_dots_13457         #x100285d)  ; U+285d BRAILLE PATTERN DOTS-13457 
(define XK_braille_dots_23457         #x100285e)  ; U+285e BRAILLE PATTERN DOTS-23457 
(define XK_braille_dots_123457        #x100285f)  ; U+285f BRAILLE PATTERN DOTS-123457 
(define XK_braille_dots_67            #x1002860)  ; U+2860 BRAILLE PATTERN DOTS-67 
(define XK_braille_dots_167           #x1002861)  ; U+2861 BRAILLE PATTERN DOTS-167 
(define XK_braille_dots_267           #x1002862)  ; U+2862 BRAILLE PATTERN DOTS-267 
(define XK_braille_dots_1267          #x1002863)  ; U+2863 BRAILLE PATTERN DOTS-1267 
(define XK_braille_dots_367           #x1002864)  ; U+2864 BRAILLE PATTERN DOTS-367 
(define XK_braille_dots_1367          #x1002865)  ; U+2865 BRAILLE PATTERN DOTS-1367 
(define XK_braille_dots_2367          #x1002866)  ; U+2866 BRAILLE PATTERN DOTS-2367 
(define XK_braille_dots_12367         #x1002867)  ; U+2867 BRAILLE PATTERN DOTS-12367 
(define XK_braille_dots_467           #x1002868)  ; U+2868 BRAILLE PATTERN DOTS-467 
(define XK_braille_dots_1467          #x1002869)  ; U+2869 BRAILLE PATTERN DOTS-1467 
(define XK_braille_dots_2467          #x100286a)  ; U+286a BRAILLE PATTERN DOTS-2467 
(define XK_braille_dots_12467         #x100286b)  ; U+286b BRAILLE PATTERN DOTS-12467 
(define XK_braille_dots_3467          #x100286c)  ; U+286c BRAILLE PATTERN DOTS-3467 
(define XK_braille_dots_13467         #x100286d)  ; U+286d BRAILLE PATTERN DOTS-13467 
(define XK_braille_dots_23467         #x100286e)  ; U+286e BRAILLE PATTERN DOTS-23467 
(define XK_braille_dots_123467        #x100286f)  ; U+286f BRAILLE PATTERN DOTS-123467 
(define XK_braille_dots_567           #x1002870)  ; U+2870 BRAILLE PATTERN DOTS-567 
(define XK_braille_dots_1567          #x1002871)  ; U+2871 BRAILLE PATTERN DOTS-1567 
(define XK_braille_dots_2567          #x1002872)  ; U+2872 BRAILLE PATTERN DOTS-2567 
(define XK_braille_dots_12567         #x1002873)  ; U+2873 BRAILLE PATTERN DOTS-12567 
(define XK_braille_dots_3567          #x1002874)  ; U+2874 BRAILLE PATTERN DOTS-3567 
(define XK_braille_dots_13567         #x1002875)  ; U+2875 BRAILLE PATTERN DOTS-13567 
(define XK_braille_dots_23567         #x1002876)  ; U+2876 BRAILLE PATTERN DOTS-23567 
(define XK_braille_dots_123567        #x1002877)  ; U+2877 BRAILLE PATTERN DOTS-123567 
(define XK_braille_dots_4567          #x1002878)  ; U+2878 BRAILLE PATTERN DOTS-4567 
(define XK_braille_dots_14567         #x1002879)  ; U+2879 BRAILLE PATTERN DOTS-14567 
(define XK_braille_dots_24567         #x100287a)  ; U+287a BRAILLE PATTERN DOTS-24567 
(define XK_braille_dots_124567        #x100287b)  ; U+287b BRAILLE PATTERN DOTS-124567 
(define XK_braille_dots_34567         #x100287c)  ; U+287c BRAILLE PATTERN DOTS-34567 
(define XK_braille_dots_134567        #x100287d)  ; U+287d BRAILLE PATTERN DOTS-134567 
(define XK_braille_dots_234567        #x100287e)  ; U+287e BRAILLE PATTERN DOTS-234567 
(define XK_braille_dots_1234567       #x100287f)  ; U+287f BRAILLE PATTERN DOTS-1234567 
(define XK_braille_dots_8             #x1002880)  ; U+2880 BRAILLE PATTERN DOTS-8 
(define XK_braille_dots_18            #x1002881)  ; U+2881 BRAILLE PATTERN DOTS-18 
(define XK_braille_dots_28            #x1002882)  ; U+2882 BRAILLE PATTERN DOTS-28 
(define XK_braille_dots_128           #x1002883)  ; U+2883 BRAILLE PATTERN DOTS-128 
(define XK_braille_dots_38            #x1002884)  ; U+2884 BRAILLE PATTERN DOTS-38 
(define XK_braille_dots_138           #x1002885)  ; U+2885 BRAILLE PATTERN DOTS-138 
(define XK_braille_dots_238           #x1002886)  ; U+2886 BRAILLE PATTERN DOTS-238 
(define XK_braille_dots_1238          #x1002887)  ; U+2887 BRAILLE PATTERN DOTS-1238 
(define XK_braille_dots_48            #x1002888)  ; U+2888 BRAILLE PATTERN DOTS-48 
(define XK_braille_dots_148           #x1002889)  ; U+2889 BRAILLE PATTERN DOTS-148 
(define XK_braille_dots_248           #x100288a)  ; U+288a BRAILLE PATTERN DOTS-248 
(define XK_braille_dots_1248          #x100288b)  ; U+288b BRAILLE PATTERN DOTS-1248 
(define XK_braille_dots_348           #x100288c)  ; U+288c BRAILLE PATTERN DOTS-348 
(define XK_braille_dots_1348          #x100288d)  ; U+288d BRAILLE PATTERN DOTS-1348 
(define XK_braille_dots_2348          #x100288e)  ; U+288e BRAILLE PATTERN DOTS-2348 
(define XK_braille_dots_12348         #x100288f)  ; U+288f BRAILLE PATTERN DOTS-12348 
(define XK_braille_dots_58            #x1002890)  ; U+2890 BRAILLE PATTERN DOTS-58 
(define XK_braille_dots_158           #x1002891)  ; U+2891 BRAILLE PATTERN DOTS-158 
(define XK_braille_dots_258           #x1002892)  ; U+2892 BRAILLE PATTERN DOTS-258 
(define XK_braille_dots_1258          #x1002893)  ; U+2893 BRAILLE PATTERN DOTS-1258 
(define XK_braille_dots_358           #x1002894)  ; U+2894 BRAILLE PATTERN DOTS-358 
(define XK_braille_dots_1358          #x1002895)  ; U+2895 BRAILLE PATTERN DOTS-1358 
(define XK_braille_dots_2358          #x1002896)  ; U+2896 BRAILLE PATTERN DOTS-2358 
(define XK_braille_dots_12358         #x1002897)  ; U+2897 BRAILLE PATTERN DOTS-12358 
(define XK_braille_dots_458           #x1002898)  ; U+2898 BRAILLE PATTERN DOTS-458 
(define XK_braille_dots_1458          #x1002899)  ; U+2899 BRAILLE PATTERN DOTS-1458 
(define XK_braille_dots_2458          #x100289a)  ; U+289a BRAILLE PATTERN DOTS-2458 
(define XK_braille_dots_12458         #x100289b)  ; U+289b BRAILLE PATTERN DOTS-12458 
(define XK_braille_dots_3458          #x100289c)  ; U+289c BRAILLE PATTERN DOTS-3458 
(define XK_braille_dots_13458         #x100289d)  ; U+289d BRAILLE PATTERN DOTS-13458 
(define XK_braille_dots_23458         #x100289e)  ; U+289e BRAILLE PATTERN DOTS-23458 
(define XK_braille_dots_123458        #x100289f)  ; U+289f BRAILLE PATTERN DOTS-123458 
(define XK_braille_dots_68            #x10028a0)  ; U+28a0 BRAILLE PATTERN DOTS-68 
(define XK_braille_dots_168           #x10028a1)  ; U+28a1 BRAILLE PATTERN DOTS-168 
(define XK_braille_dots_268           #x10028a2)  ; U+28a2 BRAILLE PATTERN DOTS-268 
(define XK_braille_dots_1268          #x10028a3)  ; U+28a3 BRAILLE PATTERN DOTS-1268 
(define XK_braille_dots_368           #x10028a4)  ; U+28a4 BRAILLE PATTERN DOTS-368 
(define XK_braille_dots_1368          #x10028a5)  ; U+28a5 BRAILLE PATTERN DOTS-1368 
(define XK_braille_dots_2368          #x10028a6)  ; U+28a6 BRAILLE PATTERN DOTS-2368 
(define XK_braille_dots_12368         #x10028a7)  ; U+28a7 BRAILLE PATTERN DOTS-12368 
(define XK_braille_dots_468           #x10028a8)  ; U+28a8 BRAILLE PATTERN DOTS-468 
(define XK_braille_dots_1468          #x10028a9)  ; U+28a9 BRAILLE PATTERN DOTS-1468 
(define XK_braille_dots_2468          #x10028aa)  ; U+28aa BRAILLE PATTERN DOTS-2468 
(define XK_braille_dots_12468         #x10028ab)  ; U+28ab BRAILLE PATTERN DOTS-12468 
(define XK_braille_dots_3468          #x10028ac)  ; U+28ac BRAILLE PATTERN DOTS-3468 
(define XK_braille_dots_13468         #x10028ad)  ; U+28ad BRAILLE PATTERN DOTS-13468 
(define XK_braille_dots_23468         #x10028ae)  ; U+28ae BRAILLE PATTERN DOTS-23468 
(define XK_braille_dots_123468        #x10028af)  ; U+28af BRAILLE PATTERN DOTS-123468 
(define XK_braille_dots_568           #x10028b0)  ; U+28b0 BRAILLE PATTERN DOTS-568 
(define XK_braille_dots_1568          #x10028b1)  ; U+28b1 BRAILLE PATTERN DOTS-1568 
(define XK_braille_dots_2568          #x10028b2)  ; U+28b2 BRAILLE PATTERN DOTS-2568 
(define XK_braille_dots_12568         #x10028b3)  ; U+28b3 BRAILLE PATTERN DOTS-12568 
(define XK_braille_dots_3568          #x10028b4)  ; U+28b4 BRAILLE PATTERN DOTS-3568 
(define XK_braille_dots_13568         #x10028b5)  ; U+28b5 BRAILLE PATTERN DOTS-13568 
(define XK_braille_dots_23568         #x10028b6)  ; U+28b6 BRAILLE PATTERN DOTS-23568 
(define XK_braille_dots_123568        #x10028b7)  ; U+28b7 BRAILLE PATTERN DOTS-123568 
(define XK_braille_dots_4568          #x10028b8)  ; U+28b8 BRAILLE PATTERN DOTS-4568 
(define XK_braille_dots_14568         #x10028b9)  ; U+28b9 BRAILLE PATTERN DOTS-14568 
(define XK_braille_dots_24568         #x10028ba)  ; U+28ba BRAILLE PATTERN DOTS-24568 
(define XK_braille_dots_124568        #x10028bb)  ; U+28bb BRAILLE PATTERN DOTS-124568 
(define XK_braille_dots_34568         #x10028bc)  ; U+28bc BRAILLE PATTERN DOTS-34568 
(define XK_braille_dots_134568        #x10028bd)  ; U+28bd BRAILLE PATTERN DOTS-134568 
(define XK_braille_dots_234568        #x10028be)  ; U+28be BRAILLE PATTERN DOTS-234568 
(define XK_braille_dots_1234568       #x10028bf)  ; U+28bf BRAILLE PATTERN DOTS-1234568 
(define XK_braille_dots_78            #x10028c0)  ; U+28c0 BRAILLE PATTERN DOTS-78 
(define XK_braille_dots_178           #x10028c1)  ; U+28c1 BRAILLE PATTERN DOTS-178 
(define XK_braille_dots_278           #x10028c2)  ; U+28c2 BRAILLE PATTERN DOTS-278 
(define XK_braille_dots_1278          #x10028c3)  ; U+28c3 BRAILLE PATTERN DOTS-1278 
(define XK_braille_dots_378           #x10028c4)  ; U+28c4 BRAILLE PATTERN DOTS-378 
(define XK_braille_dots_1378          #x10028c5)  ; U+28c5 BRAILLE PATTERN DOTS-1378 
(define XK_braille_dots_2378          #x10028c6)  ; U+28c6 BRAILLE PATTERN DOTS-2378 
(define XK_braille_dots_12378         #x10028c7)  ; U+28c7 BRAILLE PATTERN DOTS-12378 
(define XK_braille_dots_478           #x10028c8)  ; U+28c8 BRAILLE PATTERN DOTS-478 
(define XK_braille_dots_1478          #x10028c9)  ; U+28c9 BRAILLE PATTERN DOTS-1478 
(define XK_braille_dots_2478          #x10028ca)  ; U+28ca BRAILLE PATTERN DOTS-2478 
(define XK_braille_dots_12478         #x10028cb)  ; U+28cb BRAILLE PATTERN DOTS-12478 
(define XK_braille_dots_3478          #x10028cc)  ; U+28cc BRAILLE PATTERN DOTS-3478 
(define XK_braille_dots_13478         #x10028cd)  ; U+28cd BRAILLE PATTERN DOTS-13478 
(define XK_braille_dots_23478         #x10028ce)  ; U+28ce BRAILLE PATTERN DOTS-23478 
(define XK_braille_dots_123478        #x10028cf)  ; U+28cf BRAILLE PATTERN DOTS-123478 
(define XK_braille_dots_578           #x10028d0)  ; U+28d0 BRAILLE PATTERN DOTS-578 
(define XK_braille_dots_1578          #x10028d1)  ; U+28d1 BRAILLE PATTERN DOTS-1578 
(define XK_braille_dots_2578          #x10028d2)  ; U+28d2 BRAILLE PATTERN DOTS-2578 
(define XK_braille_dots_12578         #x10028d3)  ; U+28d3 BRAILLE PATTERN DOTS-12578 
(define XK_braille_dots_3578          #x10028d4)  ; U+28d4 BRAILLE PATTERN DOTS-3578 
(define XK_braille_dots_13578         #x10028d5)  ; U+28d5 BRAILLE PATTERN DOTS-13578 
(define XK_braille_dots_23578         #x10028d6)  ; U+28d6 BRAILLE PATTERN DOTS-23578 
(define XK_braille_dots_123578        #x10028d7)  ; U+28d7 BRAILLE PATTERN DOTS-123578 
(define XK_braille_dots_4578          #x10028d8)  ; U+28d8 BRAILLE PATTERN DOTS-4578 
(define XK_braille_dots_14578         #x10028d9)  ; U+28d9 BRAILLE PATTERN DOTS-14578 
(define XK_braille_dots_24578         #x10028da)  ; U+28da BRAILLE PATTERN DOTS-24578 
(define XK_braille_dots_124578        #x10028db)  ; U+28db BRAILLE PATTERN DOTS-124578 
(define XK_braille_dots_34578         #x10028dc)  ; U+28dc BRAILLE PATTERN DOTS-34578 
(define XK_braille_dots_134578        #x10028dd)  ; U+28dd BRAILLE PATTERN DOTS-134578 
(define XK_braille_dots_234578        #x10028de)  ; U+28de BRAILLE PATTERN DOTS-234578 
(define XK_braille_dots_1234578       #x10028df)  ; U+28df BRAILLE PATTERN DOTS-1234578 
(define XK_braille_dots_678           #x10028e0)  ; U+28e0 BRAILLE PATTERN DOTS-678 
(define XK_braille_dots_1678          #x10028e1)  ; U+28e1 BRAILLE PATTERN DOTS-1678 
(define XK_braille_dots_2678          #x10028e2)  ; U+28e2 BRAILLE PATTERN DOTS-2678 
(define XK_braille_dots_12678         #x10028e3)  ; U+28e3 BRAILLE PATTERN DOTS-12678 
(define XK_braille_dots_3678          #x10028e4)  ; U+28e4 BRAILLE PATTERN DOTS-3678 
(define XK_braille_dots_13678         #x10028e5)  ; U+28e5 BRAILLE PATTERN DOTS-13678 
(define XK_braille_dots_23678         #x10028e6)  ; U+28e6 BRAILLE PATTERN DOTS-23678 
(define XK_braille_dots_123678        #x10028e7)  ; U+28e7 BRAILLE PATTERN DOTS-123678 
(define XK_braille_dots_4678          #x10028e8)  ; U+28e8 BRAILLE PATTERN DOTS-4678 
(define XK_braille_dots_14678         #x10028e9)  ; U+28e9 BRAILLE PATTERN DOTS-14678 
(define XK_braille_dots_24678         #x10028ea)  ; U+28ea BRAILLE PATTERN DOTS-24678 
(define XK_braille_dots_124678        #x10028eb)  ; U+28eb BRAILLE PATTERN DOTS-124678 
(define XK_braille_dots_34678         #x10028ec)  ; U+28ec BRAILLE PATTERN DOTS-34678 
(define XK_braille_dots_134678        #x10028ed)  ; U+28ed BRAILLE PATTERN DOTS-134678 
(define XK_braille_dots_234678        #x10028ee)  ; U+28ee BRAILLE PATTERN DOTS-234678 
(define XK_braille_dots_1234678       #x10028ef)  ; U+28ef BRAILLE PATTERN DOTS-1234678 
(define XK_braille_dots_5678          #x10028f0)  ; U+28f0 BRAILLE PATTERN DOTS-5678 
(define XK_braille_dots_15678         #x10028f1)  ; U+28f1 BRAILLE PATTERN DOTS-15678 
(define XK_braille_dots_25678         #x10028f2)  ; U+28f2 BRAILLE PATTERN DOTS-25678 
(define XK_braille_dots_125678        #x10028f3)  ; U+28f3 BRAILLE PATTERN DOTS-125678 
(define XK_braille_dots_35678         #x10028f4)  ; U+28f4 BRAILLE PATTERN DOTS-35678 
(define XK_braille_dots_135678        #x10028f5)  ; U+28f5 BRAILLE PATTERN DOTS-135678 
(define XK_braille_dots_235678        #x10028f6)  ; U+28f6 BRAILLE PATTERN DOTS-235678 
(define XK_braille_dots_1235678       #x10028f7)  ; U+28f7 BRAILLE PATTERN DOTS-1235678 
(define XK_braille_dots_45678         #x10028f8)  ; U+28f8 BRAILLE PATTERN DOTS-45678 
(define XK_braille_dots_145678        #x10028f9)  ; U+28f9 BRAILLE PATTERN DOTS-145678 
(define XK_braille_dots_245678        #x10028fa)  ; U+28fa BRAILLE PATTERN DOTS-245678 
(define XK_braille_dots_1245678       #x10028fb)  ; U+28fb BRAILLE PATTERN DOTS-1245678 
(define XK_braille_dots_345678        #x10028fc)  ; U+28fc BRAILLE PATTERN DOTS-345678 
(define XK_braille_dots_1345678       #x10028fd)  ; U+28fd BRAILLE PATTERN DOTS-1345678 
(define XK_braille_dots_2345678       #x10028fe)  ; U+28fe BRAILLE PATTERN DOTS-2345678 
(define XK_braille_dots_12345678      #x10028ff)  ; U+28ff BRAILLE PATTERN DOTS-12345678 
;#endif ; XK_BRAILLE 

#|
 * Sinhala (http://unicode.org/charts/PDF/U0D80.pdf)
 * http://www.nongnu.org/sinhala/doc/transliteration/sinhala-transliteration_6.html
 |#

;#ifdef XK_SINHALA
(define XK_Sinh_ng            #x1000d82)  ; U+0D82 SINHALA ANUSVARAYA 
(define XK_Sinh_h2            #x1000d83)  ; U+0D83 SINHALA VISARGAYA 
(define XK_Sinh_a             #x1000d85)  ; U+0D85 SINHALA AYANNA 
(define XK_Sinh_aa            #x1000d86)  ; U+0D86 SINHALA AAYANNA 
(define XK_Sinh_ae            #x1000d87)  ; U+0D87 SINHALA AEYANNA 
(define XK_Sinh_aee           #x1000d88)  ; U+0D88 SINHALA AEEYANNA 
(define XK_Sinh_i             #x1000d89)  ; U+0D89 SINHALA IYANNA 
(define XK_Sinh_ii            #x1000d8a)  ; U+0D8A SINHALA IIYANNA 
(define XK_Sinh_u             #x1000d8b)  ; U+0D8B SINHALA UYANNA 
(define XK_Sinh_uu            #x1000d8c)  ; U+0D8C SINHALA UUYANNA 
(define XK_Sinh_ri            #x1000d8d)  ; U+0D8D SINHALA IRUYANNA 
(define XK_Sinh_rii           #x1000d8e)  ; U+0D8E SINHALA IRUUYANNA 
(define XK_Sinh_lu            #x1000d8f)  ; U+0D8F SINHALA ILUYANNA 
(define XK_Sinh_luu           #x1000d90)  ; U+0D90 SINHALA ILUUYANNA 
(define XK_Sinh_e             #x1000d91)  ; U+0D91 SINHALA EYANNA 
(define XK_Sinh_ee            #x1000d92)  ; U+0D92 SINHALA EEYANNA 
(define XK_Sinh_ai            #x1000d93)  ; U+0D93 SINHALA AIYANNA 
(define XK_Sinh_o             #x1000d94)  ; U+0D94 SINHALA OYANNA 
(define XK_Sinh_oo            #x1000d95)  ; U+0D95 SINHALA OOYANNA 
(define XK_Sinh_au            #x1000d96)  ; U+0D96 SINHALA AUYANNA 
(define XK_Sinh_ka            #x1000d9a)  ; U+0D9A SINHALA KAYANNA 
(define XK_Sinh_kha           #x1000d9b)  ; U+0D9B SINHALA MAHA. KAYANNA 
(define XK_Sinh_ga            #x1000d9c)  ; U+0D9C SINHALA GAYANNA 
(define XK_Sinh_gha           #x1000d9d)  ; U+0D9D SINHALA MAHA. GAYANNA 
(define XK_Sinh_ng2           #x1000d9e)  ; U+0D9E SINHALA KANTAJA NAASIKYAYA 
(define XK_Sinh_nga           #x1000d9f)  ; U+0D9F SINHALA SANYAKA GAYANNA 
(define XK_Sinh_ca            #x1000da0)  ; U+0DA0 SINHALA CAYANNA 
(define XK_Sinh_cha           #x1000da1)  ; U+0DA1 SINHALA MAHA. CAYANNA 
(define XK_Sinh_ja            #x1000da2)  ; U+0DA2 SINHALA JAYANNA 
(define XK_Sinh_jha           #x1000da3)  ; U+0DA3 SINHALA MAHA. JAYANNA 
(define XK_Sinh_nya           #x1000da4)  ; U+0DA4 SINHALA TAALUJA NAASIKYAYA 
(define XK_Sinh_jnya          #x1000da5)  ; U+0DA5 SINHALA TAALUJA SANYOOGA NAASIKYAYA 
(define XK_Sinh_nja           #x1000da6)  ; U+0DA6 SINHALA SANYAKA JAYANNA 
(define XK_Sinh_tta           #x1000da7)  ; U+0DA7 SINHALA TTAYANNA 
(define XK_Sinh_ttha          #x1000da8)  ; U+0DA8 SINHALA MAHA. TTAYANNA 
(define XK_Sinh_dda           #x1000da9)  ; U+0DA9 SINHALA DDAYANNA 
(define XK_Sinh_ddha          #x1000daa)  ; U+0DAA SINHALA MAHA. DDAYANNA 
(define XK_Sinh_nna           #x1000dab)  ; U+0DAB SINHALA MUURDHAJA NAYANNA 
(define XK_Sinh_ndda          #x1000dac)  ; U+0DAC SINHALA SANYAKA DDAYANNA 
(define XK_Sinh_tha           #x1000dad)  ; U+0DAD SINHALA TAYANNA 
(define XK_Sinh_thha          #x1000dae)  ; U+0DAE SINHALA MAHA. TAYANNA 
(define XK_Sinh_dha           #x1000daf)  ; U+0DAF SINHALA DAYANNA 
(define XK_Sinh_dhha          #x1000db0)  ; U+0DB0 SINHALA MAHA. DAYANNA 
(define XK_Sinh_na            #x1000db1)  ; U+0DB1 SINHALA DANTAJA NAYANNA 
(define XK_Sinh_ndha          #x1000db3)  ; U+0DB3 SINHALA SANYAKA DAYANNA 
(define XK_Sinh_pa            #x1000db4)  ; U+0DB4 SINHALA PAYANNA 
(define XK_Sinh_pha           #x1000db5)  ; U+0DB5 SINHALA MAHA. PAYANNA 
(define XK_Sinh_ba            #x1000db6)  ; U+0DB6 SINHALA BAYANNA 
(define XK_Sinh_bha           #x1000db7)  ; U+0DB7 SINHALA MAHA. BAYANNA 
(define XK_Sinh_ma            #x1000db8)  ; U+0DB8 SINHALA MAYANNA 
(define XK_Sinh_mba           #x1000db9)  ; U+0DB9 SINHALA AMBA BAYANNA 
(define XK_Sinh_ya            #x1000dba)  ; U+0DBA SINHALA YAYANNA 
(define XK_Sinh_ra            #x1000dbb)  ; U+0DBB SINHALA RAYANNA 
(define XK_Sinh_la            #x1000dbd)  ; U+0DBD SINHALA DANTAJA LAYANNA 
(define XK_Sinh_va            #x1000dc0)  ; U+0DC0 SINHALA VAYANNA 
(define XK_Sinh_sha           #x1000dc1)  ; U+0DC1 SINHALA TAALUJA SAYANNA 
(define XK_Sinh_ssha          #x1000dc2)  ; U+0DC2 SINHALA MUURDHAJA SAYANNA 
(define XK_Sinh_sa            #x1000dc3)  ; U+0DC3 SINHALA DANTAJA SAYANNA 
(define XK_Sinh_ha            #x1000dc4)  ; U+0DC4 SINHALA HAYANNA 
(define XK_Sinh_lla           #x1000dc5)  ; U+0DC5 SINHALA MUURDHAJA LAYANNA 
(define XK_Sinh_fa            #x1000dc6)  ; U+0DC6 SINHALA FAYANNA 
(define XK_Sinh_al            #x1000dca)  ; U+0DCA SINHALA AL-LAKUNA 
(define XK_Sinh_aa2           #x1000dcf)  ; U+0DCF SINHALA AELA-PILLA 
(define XK_Sinh_ae2           #x1000dd0)  ; U+0DD0 SINHALA AEDA-PILLA 
(define XK_Sinh_aee2          #x1000dd1)  ; U+0DD1 SINHALA DIGA AEDA-PILLA 
(define XK_Sinh_i2            #x1000dd2)  ; U+0DD2 SINHALA IS-PILLA 
(define XK_Sinh_ii2           #x1000dd3)  ; U+0DD3 SINHALA DIGA IS-PILLA 
(define XK_Sinh_u2            #x1000dd4)  ; U+0DD4 SINHALA PAA-PILLA 
(define XK_Sinh_uu2           #x1000dd6)  ; U+0DD6 SINHALA DIGA PAA-PILLA 
(define XK_Sinh_ru2           #x1000dd8)  ; U+0DD8 SINHALA GAETTA-PILLA 
(define XK_Sinh_e2            #x1000dd9)  ; U+0DD9 SINHALA KOMBUVA 
(define XK_Sinh_ee2           #x1000dda)  ; U+0DDA SINHALA DIGA KOMBUVA 
(define XK_Sinh_ai2           #x1000ddb)  ; U+0DDB SINHALA KOMBU DEKA 
(define XK_Sinh_o2            #x1000ddc)  ; U+0DDC SINHALA KOMBUVA HAA AELA-PILLA
(define XK_Sinh_oo2           #x1000ddd)  ; U+0DDD SINHALA KOMBUVA HAA DIGA AELA-PILLA
(define XK_Sinh_au2           #x1000dde)  ; U+0DDE SINHALA KOMBUVA HAA GAYANUKITTA 
(define XK_Sinh_lu2           #x1000ddf)  ; U+0DDF SINHALA GAYANUKITTA 
(define XK_Sinh_ruu2          #x1000df2)  ; U+0DF2 SINHALA DIGA GAETTA-PILLA 
(define XK_Sinh_luu2          #x1000df3)  ; U+0DF3 SINHALA DIGA GAYANUKITTA 
(define XK_Sinh_kunddaliya    #x1000df4)  ; U+0DF4 SINHALA KUNDDALIYA 
;#endif ; XK_SINHALA 