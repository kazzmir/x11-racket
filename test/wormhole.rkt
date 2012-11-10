#lang racket/base

(require "../x11.rkt"
         racket/stxparam
         (for-syntax racket/base))

(struct wormhole
        [diameter diameter-change
         x y
         speed
         angle want-angle
         want-x want-y
         max-Z
         add-star
         spiral
         color-changer
         stars]
        #:mutable)

(define (angle-to x1 y1 x2 y2)
  (atan (- y2 y1) (- x2 x1)))

(define (create-starline wormhole)
  0)

(define (create-color-changer display)
  0)

(define (create-wormhole display window)
  (define attributes (XGetWindowAttributes display window))
  (define width (XWindowAttributes-width attributes))
  (define height (XWindowAttributes-height attributes))
  (define want-x (+ 25 (random (- width 50))))
  (define want-y (+ 25 (random (- height 50))))
  (define x (/ width 2))
  (define y (/ height 2))
  (define out
    (wormhole (+ (random 10) 15) (+ (random 10) 15)
              x y
              (/ width 180)
              (angle-to x y want-x want-y) 
              (angle-to x y want-x want-y)
              want-x want-y
              600
              20
              0
              (create-color-changer display)
              (list)))
  (set-wormhole-stars! out (for/list ([star 64])
                                  (create-starline out)))
  out)

(define (draw-wormhole wormhole work display)
  #f)

(define-syntax-parameter quit (lambda (stx) (raise-syntax-error 'quit "syntax parameter")))
(define-syntax-rule (block code ...)
  (let/ec break
    (syntax-parameterize ([quit (make-rename-transformer #'break)])
      code ...)))

(define (application frames-per-second logic draw)
  (define time current-inexact-milliseconds)

  (let loop ([now (time)])
    #;
    (printf "current ~a now ~a logic ~a\n" (time) now 
            (/ (- (time) now)
               (/ 1000 frames-per-second)))

    ;; compute number of times logic should be called
    (define loops (/ (- (time) now)
                     (/ 1000 frames-per-second)))

    (if (>= loops 1)
      (begin
        (for ([i (in-range 1 loops)])
          (logic))
        (draw)
        (loop (time)))
      (begin
        (sleep 0.001)
        (loop now)))))
  
(define (main-loop display window depth)
  (define frames-per-second 45)

  (block
    (define (handle-key-press event)
      (define escape 9)
      (when (= (XKeyPressedEvent-keycode event)
               escape)
        (quit)))

    ;; handles all events
    (define (handle-events)
      (when (XPending display)
        #;
        (printf "Event\n")
        (define event (XNextEvent* display))
        (case (XEvent-type event)
          [(KeyPress) (handle-key-press event)])
        (handle-events)))

    (define graphics-context (XCreateGC display window 0 #f))
    (define colormap (XWindowAttributes-colormap (XGetWindowAttributes display window)))
    (define work (XCreatePixmap display window 640 480 depth))
    (define (alloc-color name default)
      (AllocNamedColor display window name default))
    (define black (XAllocColor display colormap (make-XColor-rgb 0 0 0)))
    (define white (XAllocColor display colormap (make-XColor-rgb 60000 60000 60000)))

    (define wormhole (create-wormhole display window))

    (define (draw work screen)
      (printf "draw\n")
      (define attributes (XGetWindowAttributes display window))
      (define width (XWindowAttributes-width attributes))
      (define height (XWindowAttributes-height attributes))
      (XSetForeground display graphics-context white)
      (XFillRectangle display work graphics-context 0 0 width height)
      (draw-wormhole wormhole work screen)
      (XCopyArea display work screen graphics-context 0 0 width height 0 0))

    (define (logic)
      (handle-events)
      (void))

    (application frames-per-second logic (lambda () (draw work window)))))

(define (x11-init display window)
  (XSelectInput display window '(ExposureMask KeyPressMask))
  (XMapWindow display window)
  (XFlush display))

(define (x11-shutdown display window)
  (XUnmapWindow display window)
  (XDestroyWindow display window)
  (XFlush display))

(define (run)
  (define display (XOpenDisplay #f))
  (define screen (DefaultScreen display))
  (define root (RootWindow display screen))
  (define depth (DefaultDepth display screen))
  (define visual (DefaultVisual display screen))
  (define attributes (make-XSetWindowAttributes #:background-pixel (XBlackPixel display screen)))
  (define window (XCreateWindow display root 0 0 640 480 5 depth 'InputOutput visual 'BackPixel attributes))

  (x11-init display window)
  (main-loop display window depth)
  (x11-shutdown display window)
  (void))

(run)
