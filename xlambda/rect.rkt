#lang racket/base

(provide (struct-out rect))

(struct rect (x y w h) #:transparent)
