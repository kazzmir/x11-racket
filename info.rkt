#lang setup/infotab

(define collection 'multi)
(define deps '("base" "rackunit-lib" "compatibility-lib" "scheme-lib"))

(define test-omit-paths
  '("x11/test" ; these are not actual tests
    "xlambda/xlambda.rkt"
    "main.rkt"))
