#lang racket

(require (lib "trace.ss")
         ; (require (lib "errortrace.ss" "errortrace"))
         "../x11.rkt"
         "../x11-xpm.rkt"
         (lib "foreign.ss"))
(unsafe!)
(require scheme/mpair
         ;; (require #%foreign)
         (lib "list.ss")
         "../keysymdef.rkt"
         racket/math
         racket/class
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

  (XGrabKey display (XKeysymToKeycode display XK-Tab) 'ControlMask rootWindow #t 'GrabModeAsync 'GrabModeAsync)
  (XGrabKey display (XKeysymToKeycode display XK-space) 'ControlMask rootWindow #t 'GrabModeAsync 'GrabModeAsync)
  (XGrabKey display (XKeysymToKeycode display XK-j) 'ControlMask rootWindow #t 'GrabModeAsync 'GrabModeAsync)
  (XGrabKey display (XKeysymToKeycode display XK-k) 'ControlMask rootWindow #t 'GrabModeAsync 'GrabModeAsync)

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

  (define ss (new screen-stack%))

  (define (size-show-window win r)
    (match-define (rect x y w h) r)
    (XMoveResizeWindow display win x y w h)
    (set-WM-State-normal! win)
    (XMapWindow display win))

  (define root-rect (rect 0 0 display-w display-h))
   
  (define ops% 
    (class object%
      (define/public (fullscreen w) 
                     (size-show-window w root-rect))
      (define/public (size w r) (size-show-window w r))
      (define/public (hide w) (XUnmapWindow display w))
      (super-new)))

  (define ops (new ops%))
  (define layouter (new xmonad-layout%))

  (define (handle-x11-event event)
    (case (XEvent-type event)
      ((KeyPress)
       ;CTRL
       (when (equal? '(ControlMask) (XKeyEvent-state event))
         (case (XKeyEvent-keycode event)
           ;space
           [(65) (send layouter rotate-layout)]
           ;j
           [(44) (send ss rotate-left)]
           ;k
           [(45) (send ss rotate-right)]
           [else
            (printf "Key-Event ~a ~a\n" (XKeyEvent-state event) (XKeyEvent-keycode event))])
         (send layouter layout (send ss get-ws) ops root-rect)
       ))
      ((MapRequest)
       (send ss push-win (XMapRequestEvent-window event))
       (send layouter layout (send ss get-ws) ops root-rect)
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
       (send ss remove-win (XDestroyWindowEvent-window event))
       (send layouter layout (send ss get-ws) ops root-rect)
       )
      ((EnterNotify) (void))
      ((LeaveNotify) (void))
      (else
        (begin
          (printf "Event type: ~a\n" (XEvent-type event))))))

  (define scheme-make-fd-input-port
    (let ([fun (get-ffi-obj "scheme_make_fd_input_port" #f (_fun _int _racket _int _int -> _racket))])
      (lambda (fd name)
        (fun fd name 0 0))))

  (define x11-port (scheme-make-fd-input-port (XConnectionNumber display) 'x11-connection))

  (let loop ()
    (sync
      (handle-evt x11-port 
                  (lambda (e)
                    (let loop2 ()
                      (when (XPending display)
                        (handle-x11-event (XNextEvent* display))
                        (loop2)))
                  )))
    (loop))
)

(xlambda)
