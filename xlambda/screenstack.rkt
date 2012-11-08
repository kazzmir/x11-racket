#lang racket/base
(require racket/class
         racket/list
         racket/match
         "rect.rkt")

(provide work-environment%
         screen-stack%
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

(define work-environment%
  (class object%
    (field [current-ss (new screen-stack%)]
           [sss (hash 0 current-ss)])
    (define/public (push-win w) (send current-ss push-win w))
    (define/public (remove-win w) (for ([css (in-hash-values sss)])
                                       (send current-ss remove-win w)))
    (define/public (rotate-left) (send current-ss rotate-left))
    (define/public (rotate-right) (send current-ss rotate-right))
    (define/public (get-ws) (send current-ss get-ws))
    (define/public (switch ss-id ops)
      (define new-ss (hash-ref sss ss-id (lambda ()
                                           (define new-ss (new screen-stack%))
                                           (set! sss (hash-set sss ss-id new-ss))
                                           new-ss)))
      (when (not (eq? current-ss new-ss))
        (send ops hide-all (send current-ss get-ws))
        (set! current-ss new-ss)))

    (super-new)))

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
