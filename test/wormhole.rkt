#lang racket/base

(require "../x11.rkt")

(define (main-loop display window depth)
  (define frames-per-second 45)
  (let/ec quit
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

          #;
          (define time (lambda () (/ (current-inexact-milliseconds) 1000)))
          (define time current-inexact-milliseconds)

          (define graphics-context (XCreateGC display window 0 #f))
          (define colormap (XWindowAttributes-colormap (XGetWindowAttributes display window)))
          (define work (XCreatePixmap display window 640 480 depth))
          (define (alloc-color name default)
            (AllocNamedColor display window name default))
          (define black (XAllocColor display colormap (make-XColor-rgb 0 0 0)))
          (define white (XAllocColor display colormap (make-XColor-rgb 60000 60000 60000)))
          (printf "black is ~a white is ~a\n" black white)

          (define (draw work screen)
            (XSetForeground display graphics-context white)
            (XFillRectangle display work graphics-context 0 0 640 480)
            (XCopyArea display work screen graphics-context 0 0 640 480 0 0))

          (let loop ([now (time)])
            (handle-events)
            #;
            (printf "current ~a now ~a logic ~a\n" (time) now 
                    (/ (- (time) now)
                       (/ 1000 frames-per-second)))
            (define loops (/ (- (time) now)
                             (/ 1000 frames-per-second)))
            (if (>= loops 1)
              (begin
                (for ([logic (in-range 1 loops)])
                  (printf "Logic ~a\n" logic))
                (draw work window)
                (loop (time)))
              (begin
                (sleep 0.001)
                (loop now))))))

(define (run)
  (define display (XOpenDisplay #f))
  (define screen (DefaultScreen display))
  (define root (RootWindow display screen))
  (define depth (DefaultDepth display screen))
  (define visual (DefaultVisual display screen))
  (define attributes (make-XSetWindowAttributes #:background-pixel (XBlackPixel display screen)))
  (define window (XCreateWindow display root 0 0 640 480 5 depth 'InputOutput visual 'BackPixel attributes))
  (XSelectInput display window '(ExposureMask KeyPressMask))
  (XMapWindow display window)
  (XFlush display)
  (main-loop display window depth)
  (XUnmapWindow display window)
  (XDestroyWindow display window)
  (XFlush display))

(run)
