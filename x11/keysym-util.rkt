#lang racket

(require "keysymdef.rkt")
(provide (all-defined-out))

;;; From /usr/include/X11/Xutil.h
;;; Author: Laurent Orseau -- laurent orseau gmail com

;; key-sym: _ulong
(define (IsKeypadKey keysym)
  (and (keysym . >= . XK-KP-Space) (keysym . <= . XK-KP-Equal)))

(define (IsPrivateKeypadKey keysym)
  (and (keysym . >= . #x11000000) (keysym . <= . #x1100FFFF)))

(define (IsCursorKey keysym)
  (and (keysym . >= . XK-Home) (keysym . < . XK-Select)))

(define (IsPFKey keysym)
  (and (keysym . >= . XK-KP-F1) (keysym . <= . XK-KP-F4)))

(define (IsFunctionKey keysym)
  (and (keysym . >= . XK-F1) (keysym . <= . XK-F35)))

(define (IsMiscFunctionKey keysym)
  (and (keysym . >= . XK-Select) (keysym . <= . XK-Break)))

#|ifdef XK-XKB-KEYS
(define (IsModifierKey keysym)
  (or
   (and (keysym . >= . XK-Shift-L) (keysym . <= . XK-Hyper-R))
   (and (keysym . >= . XK-ISO-Lock)
        (keysym . <= . XK-ISO-Last-Group-Lock))
   (keysym . = . XK-Mode-switch)
   (keysym . = . XK-Num-Lock)))
|#;else
(define (IsModifierKey keysym)
  (or (keysym . >= . XK-Shift-L) (keysym . <= . XK-Hyper-R)
      (keysym . = . XK-Mode-switch)
      (keysym . = . XK-Num-Lock)))
;#endif
