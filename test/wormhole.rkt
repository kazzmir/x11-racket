#lang racket/base

(require "../x11.rkt"
         racket/stxparam
         racket/math
         (for-syntax racket/base))

(struct star [x y z center-x center-y] #:mutable)

(struct starline [start end])

(struct wormhole
        [diameter diameter-change
         x y
         speed
         angle want-angle
         want-x want-y
         max-z
         add-star
         spiral
         color-changer
         stars]
        #:mutable)

(define (angle-to x1 y1 x2 y2)
  (atan (- y2 y1) (- x2 x1)))

(define (random-angle)
  (* (random 360) pi 1/180))

(define (create-star z angle wormhole)
  (star (* (cos angle) (wormhole-diameter wormhole))
        (* (sin angle) (wormhole-diameter wormhole))
        z
        (wormhole-x wormhole)
        (wormhole-y wormhole)))

(define (create-starline wormhole)
  (define angle (random-angle))
  (define z (random (wormhole-max-z wormhole)))
  (starline (create-star z angle wormhole)
            (create-star (- z (random 6) 4) angle wormhole)))

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
  (set-wormhole-stars! out (for/list ([star 400])
                             (create-starline out)))
  out)

;; R3 -> R2
(define (project coordinate center z)
  (if (<= z 0)
    (+ center (* coordinate 1024))
    (+ center (/ (* coordinate 1024) z))))

(define (project-star star)
  (values (project
            (star-x star)
            (star-center-x star)
            (star-z star))
          (project
            (star-y star)
            (star-center-y star)
            (star-z star))))

(define (int x)
  (inexact->exact (round x)))

(define (in what start end)
  (and (>= what start)
       (<= what end)))

(define (draw-starline line work display gc colors max-z)
  (define z (star-z (starline-start line)))
  (define color (list-ref colors (int (/ (* (- max-z z) (sub1 (length colors))) max-z))))
  ; (define use (int (/ (* (- max-z z) (sub1 (length colors))) max-z)))
  ; (define use (random 2))
  ; (define color (list-ref colors use))
  (XSetForeground display gc color)

  (define-values (begin-x begin-y)
                 (project-star (starline-start line)))
  (define-values (end-x end-y)
                 (project-star (starline-end line)))

  (define-values (x1 y1 x2 y2)
                 (values
                 (int begin-x) (int begin-y)
                 (int end-x) (int end-y)))
  
  #;
  (printf "Line ~a, ~a to ~a, ~a\n" x1 y1 x2 y2)
  (XDrawLine display work gc x1 y1 x2 y2)

  #;
  (XDrawLine display work gc
             (int begin-x) (int begin-y)
             (int end-x) (int end-y)))

(define (draw-wormhole wormhole work display gc colors)
  (for ([star (wormhole-stars wormhole)])
    (draw-starline star work display gc colors (wormhole-max-z wormhole))))

(define (move-star star)
  (define z-speed 10)
  (set-star-z! star (- (star-z star) z-speed)))

(define (update-star original update)
  (set-star-x! original (star-x update))
  (set-star-y! original (star-y update))
  (set-star-z! original (star-z update))
  (set-star-center-x! original (star-center-x update))
  (set-star-center-y! original (star-center-y update)))

(define (move-starline starline wormhole)
  (move-star (starline-start starline))
  (move-star (starline-end starline))
  (when (<= (star-z (starline-end starline)) 0)
    (define angle (random-angle))
    (update-star (starline-start starline)
                 (create-star (wormhole-max-z wormhole)
                              angle
                              wormhole))
    (update-star (starline-end starline)
                 (create-star (- (wormhole-max-z wormhole)
                                 (random 6) 4)
                              angle
                              wormhole))))

(define (move-wormhole wormhole display)
  (for ([starline (wormhole-stars wormhole)])
    (move-starline starline wormhole)))

(define-syntax-parameter quit (lambda (stx) (raise-syntax-error 'quit "syntax parameter")))
(define-syntax-rule (block code ...)
  (let/ec break
    (syntax-parameterize ([quit (make-rename-transformer #'break)])
      code ...)))

(define-syntax-rule (print-time what expr ...)
                    (begin
                      (printf "~a\n" what)
                      (time expr ...)))

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
        (print-time "Draw" (draw))
        (printf "FPS ~a\n" (/ 1000 (- (time) now)))
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
      (unless (zero? (XPending display))
        #;
        (printf "Event\n")
        (define event (XNextEvent* display))
        (case (XEvent-type event)
          [(KeyPress) (handle-key-press event)])
        ;; received an event so search for another one
        (handle-events)))

    (define colormap (XWindowAttributes-colormap (XGetWindowAttributes display window)))
    (define work (XCreatePixmap display window 640 480 depth))
    (define (alloc-color name default)
      (AllocNamedColor display window name default))
    (define black (make-XColor-rgb 0 0 0))
    (XAllocColor display colormap black)
    (define white (make-XColor-rgb 60000 60000 60000))
    (XAllocColor display colormap white)

    (define gc-values (make-dummy-XGCValues))
    (define graphics-context (XCreateGC display window '() gc-values))

    (define wormhole (create-wormhole display window))

    (define max-colors 50)
    (define colors (for/list ([i max-colors])
                     (define value (int (* i (/ 65535 max-colors))))
                     (define color (make-XColor-rgb value value value))
                     (XAllocColor display colormap color)
                     (XColor-pixel color)))

    (define (draw work window)
      (define attributes (XGetWindowAttributes display window))
      (define width (XWindowAttributes-width attributes))
      (define height (XWindowAttributes-height attributes))
      (XSetForeground display graphics-context (XColor-pixel black))
      (XFillRectangle display work graphics-context 0 0 width height)
      (draw-wormhole wormhole work display graphics-context colors)
      (XCopyArea display work window graphics-context 0 0 width height 0 0))

    (define (logic)
      (handle-events)
      (move-wormhole wormhole display))

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
