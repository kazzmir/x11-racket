#lang racket/base

(require ffi/unsafe
         (for-syntax racket/base
                     racket/syntax))
(provide (all-defined-out))

(define-syntax define*
  (syntax-rules ()
    [(_ (name args ...) body ...)
     (begin (define (name args ...) body ...)
            (provide name))]
    [(_ name val)
     (begin (define name val)
            (provide name))]))

(define-for-syntax (format-syntax str . args)
    (apply format str
	   (map (lambda (x)
		  (symbol->string
		    (syntax->datum x)))
		args)))

;; provide all the fields of a cstruct and struct-tag
(define-syntax (define-cstruct* stx)
  (syntax-case stx ()
    ((_ name ((field type) ...))
     (with-syntax ((id (datum->syntax #'name
                                      (string->symbol
                                       (substring
                                        (format-syntax "~a" #'name)
                                        1))))
                   (name-pointer (format-id #'name "~a-pointer" #'name))
                   (name-pointer/null (format-id #'name "~a-pointer/null" #'name))
                   (->list* (datum->syntax #'name
                                           (string->symbol
                                            (substring
                                             (format-syntax "~a->list*" #'name)
                                             1))))
                   (tag (datum->syntax #'name
                                       (string->symbol
                                         (substring
                                           (format-syntax "~a-tag" #'name)
                                           1)))))
       #'(begin
           (define-cstruct name ((field type) ...))
           (provide name name-pointer name-pointer/null)
	   (provide (struct-out id) ->list* tag)))))) ; useful for match

(define-syntax (define-cstructs* stx)
  (syntax-case stx ()
    ((_ (name) rest)
     #'(define-cstruct* name rest))
    ((_ (name1 names ...) rest)
     #'(begin
         (define-cstruct* name1 rest)
         (define-cstructs* (names ...) rest)))))

;; Register-finalizer wrapper, to avoid argument problems and code duplication
(define (c->racket/register-finalizer c->racket c-pointer free)
  (register-finalizer c->racket (λ(c->racket)(free c-pointer)))
  c->racket)

;; (wait, couldn't we free the data just after the cblock->list operation?
;; Or is the data still used somehow?)
(define (cblock->list/finalizer data type count free)
  (c->racket/register-finalizer (cblock->list data type count) data free))
; TODO: Do the same with vector?
