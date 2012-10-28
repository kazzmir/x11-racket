#lang racket

(require "keysymdef.rkt")
(provide (all-defined-out))

;;; From /usr/include/X11/Xutil.h
;;; Author: Laurent Orseau -- laurent orseau gmail com

;; key-sym: _ulong
(define (IsKeypadKey keysym)
  (and (keysym . >= . XK_KP_Space) (keysym . <= . XK_KP_Equal)))

(define (IsPrivateKeypadKey keysym)
  (and (keysym . >= . #x11000000) (keysym . <= . #x1100FFFF)))

(define (IsCursorKey keysym) 
  (and (keysym . >= . XK_Home) (keysym . < . XK_Select)))

(define (IsPFKey keysym) 
  (and (keysym . >= . XK_KP_F1) (keysym . <= . XK_KP_F4)))

(define (IsFunctionKey keysym)
  (and (keysym . >= . XK_F1) (keysym . <= . XK_F35)))

(define (IsMiscFunctionKey keysym)
  (and (keysym . >= . XK_Select) (keysym . <= . XK_Break)))

#|ifdef XK_XKB_KEYS
(define (IsModifierKey keysym)
  (or
   (and (keysym . >= . XK_Shift_L) (keysym . <= . XK_Hyper_R))
   (and (keysym . >= . XK_ISO_Lock)
        (keysym . <= . XK_ISO_Last_Group_Lock))
   (keysym . = . XK_Mode_switch)
   (keysym . = . XK_Num_Lock)))
|#;else
(define (IsModifierKey keysym)
  (or (keysym . >= . XK_Shift_L) (keysym . <= . XK_Hyper_R)
      (keysym . = . XK_Mode_switch)
      (keysym . = . XK_Num_Lock)))
;#endif
