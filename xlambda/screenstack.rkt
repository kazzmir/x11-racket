#lang racket/base
(require racket/class
         racket/list
         racket/match
         "rect.rkt")

(provide screen-stack%
         xmonad-layout%)

(define (in-rect-split-horz r c)
  (match-define (rect x y w h) r)
  (define-values (q rem) (quotient/remainder w c))
  (for/list ([i c])
    (rect (+ x (* i q) (if (< i rem) i 0)) y 
          (+ q (if (< i rem) 1 0)) h)))

(define (in-rect-split-vert r c)
  (match-define (rect x y w h) r)
  (define-values (q rem) (quotient/remainder h c))
  (for/list ([i c])
    (rect x (+ y (* i q) (if (< i rem) i 0))
          w (+ q (if (< i rem) 1 0)))))

(define screen-stack% 
  (class object%
    (field [ws '()]
           [focus null])
    (define/public (push-win w) (set! ws (append ws (list w))))
    (define/public (remove-win w) (set! ws (remove w ws)))
    (define/public (rotate-left) 
      (when (>= (length ws) 2)
        (set! ws (append (rest ws) (list (first ws))))))
    (define/public (rotate-right) 
      (when (>= (length ws) 2)
        (define-values (l r) (split-at-right ws 1))
        (set! ws (append r l))))
    (define/public (get-ws) ws)
    (super-new)))


(define xmonad-layout%
  (class object%
    (field [layout-t '(horz vert fullscreen)])
    (define/public (rotate-layout)
      (set! layout-t (append (rest layout-t) (list (first layout-t)))))
    (define/public (layout ss ops r)
      (match ss
        [(list) (void)]
        [(list w) 
         (send ops fullscreen w)]
        [(list-rest m ws)
         (case (first layout-t)
           [(fullscreen) 
            (send ops fullscreen m)
            (for ([w ws]) (send ops hide w))]
           [else
            (define-values (s1 s2)
              (if (eq? (first layout-t) 'horz)
                  (values in-rect-split-horz in-rect-split-vert)
                  (values in-rect-split-vert in-rect-split-horz)))
            (match-define (list r1 r2) (s1 r 2))
            (send ops size m r1)
            (for ([w ws]
                  [r (s2 r2 (length ws))])
              (send ops size w r))])]))
    (super-new)))
