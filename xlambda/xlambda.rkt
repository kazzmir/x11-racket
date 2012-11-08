#lang racket/base

(require "../x11.rkt"
         "../x11-xpm.rkt"
         "../fd.rkt"
         "../keysymdef.rkt"
         ffi/unsafe
         racket/list
         racket/math
         racket/class
         racket/match
         "screenstack.rkt"
         "rect.rkt")

(define (xlambda)
  (define display (XOpenDisplay #f))
  (define screen (DefaultScreen display))
  (define rootWindow (RootWindow display screen))
  (define black (BlackPixel display screen))
  (define white (WhitePixel display screen))
  (define display-w (DisplayWidth display screen))
  (define display-h (DisplayHeight display screen))
  (define center-x (/ display-w 2))
  (define center-y (/ display-h 2))
  (define (alloc-color name default)
    (AllocNamedColor display screen name default))
  (define redColor (alloc-color "red" black))
  (define whiteColor (alloc-color "white" white))
  (define greenColor (alloc-color "green" black))
  (define blackColor (alloc-color "black" black))
  (define snowColor (alloc-color "snow" white))
  (define treeColor (alloc-color "chartreuse" black))
  (define sleighColor (alloc-color "chartreuse" black))
  (define backgroundColor (alloc-color "none" white))

  ;; dont care about errors
  (XSetErrorHandler (lambda (display e)
                      (printf "ERROR ~a ~a ~a ~a\n" display (XErrorEvent-error-code e)
                              (XErrorEvent-request-code e)
                              (XErrorEvent-minor-code e))
                      0))

  (XClearArea display rootWindow 0 0 display-w display-h #f) 
  (XSelectInput display rootWindow '(SubstructureRedirectMask 
                                     SubstructureNotifyMask
                                     EnterWindowMask
                                     LeaveWindowMask
                                     StructureNotifyMask
                                     ButtonPressMask))

  (for ([k (list XK-Tab XK-space XK-h XK-j XK-k XK-l XK-H XK-J XK-K XK-L XK-c 
                 XK-F1 XK-F2 XK-F3 XK-F4 XK-F5 XK-F6 XK-F7 XK-F8 XK-F9 XK-F10 XK-F11 XK-F12 )])
    (XGrabKey display (XKeysymToKeycode display k) 'ControlMask rootWindow #t 'GrabModeAsync 'GrabModeAsync))

  (define atom_WM_STATE (XInternAtom display "WM_STATE" #f))


  (define (setWMState w s)
    (define withdrawnState 0)
    (define normalState    1)
    (define propModeReplace 0)
    (define propModePrepend 1)
    (define propModeAppend  2)
    (XChangeProperty display w atom_WM_STATE atom_WM_STATE 32 propModeReplace (list->cblock (list s 0) _int32) 1))
  (define (set-WM-State-withdrawn! w) 
    (define withdrawnState 0)
    (setWMState w withdrawnState))
  (define (set-WM-State-normal! w)
    (define normalState    1)
    (setWMState w normalState))
  (define (set-WM-State-iconic! w)
    (define iconicState    3)
    (setWMState w iconicState))

  (define we (new work-environment%))

  (define (size-show-window win r)
    (match-define (rect x y w h) r)
    (XMoveResizeWindow display win x y w h)
    (set-WM-State-normal! win)
    (XMapWindow display win))

  (define root-rect (rect 0 0 display-w display-h))
   
  (define ops% 
    (class object%
      (define/public (fullscreen w) (size-show-window w root-rect))
      (define/public (size w r) (size-show-window w r))
      (define/public (hide w) (XUnmapWindow display w))
      (define/public (hide-all l) (for ([x l]) (hide x)))
      (super-new)))


  (define ops (new ops%))
  (define layouter (new xmonad-layout%))

  (define (relayout)
    (send layouter layout (send we get-ws) ops root-rect))

  (define (handle-x11-event event)
    (case (XEvent-type event)
      ((KeyPress)
       ;CTRL
       (when (equal? '(ControlMask) (XKeyEvent-state event))
         (define XKKEY (XKeycodeToKeysym display (XKeyEvent-keycode event) 0))
         (printf "KEY ~a ~a ~a\n" (XKeyEvent-keycode event) XKKEY XK-space)
         (case XKKEY
           ;F1 - F12
           [(65470 65471 65472 65473 65474 65475 65476 65477 65478 65479 65480 65481) (send we switch (- XKKEY 65470) ops)]
           ;space
           [(32) (send layouter rotate-layout)]
           ;j
           [(106) (send we rotate-left)]
           ;k
           [(107) (send we rotate-right)]
           [else
            (printf "Key-Event ~a ~a\n" (XKeyEvent-state event) (XKeyEvent-keycode event))])
         (relayout)
       ))
      ((MapRequest)
       (send we push-win (XMapRequestEvent-window event))
       (relayout)
       )
      ((ConfigureNotify)
       (begin
         (printf "Configure-Event ~a ~a ~a ~a ~a ~a ~a ~a\n" 
                 display-w
                 display-h
                 center-x
                 center-y
                 (XConfigureEvent-x event)
                 (XConfigureEvent-y event)
                 (XConfigureEvent-width event)
                 (XConfigureEvent-height event))))
      ((DestroyNotify)
       (send we remove-win (XDestroyWindowEvent-window event))
       (relayout)
       )
      ((EnterNotify) (void))
      ((LeaveNotify) (void))
      (else
        (begin
          (printf "Event type: ~a\n" (XEvent-type event))))))

  (define x11-port (open-fd-input-port (XConnectionNumber display) #;'x11-connection))

  (let loop ()
    (sync
      (handle-evt x11-port 
                  (lambda (e)
                    (let loop2 ()
                      (when (XPending display)
                        (handle-x11-event (XNextEvent* display))
                        (loop2)))
                  ))
      (handle-evt (current-input-port)
                  (lambda (e)
                    (printf "INPUT ~a ~a\n" e (read-line e)))))
    (loop))
)

(xlambda)
