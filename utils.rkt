#lang scheme

(require scheme/foreign)
(provide (all-defined-out))

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
     (with-syntax (((provides ...)
                    (map (lambda (field)
                           (datum->syntax
                             field
                             (string->symbol
                               (substring
                                 (format-syntax "~a-~a" #'name field)
                                 1))))
                         (syntax->list #'(field ...))))
                   (tag (datum->syntax #'name
                                       (string->symbol
                                         (substring
                                           (format-syntax "~a-tag" #'name)
                                           1)))))
       #'(begin
           (define-cstruct name ((field type) ...))
	     (provide tag provides ...))))))

(define-syntax (define-cstructs* stx)
  (syntax-case stx ()
    ((_ (name) rest)
     #'(define-cstruct* name rest))
    ((_ (name1 names ...) rest)
     #'(begin
         (define-cstruct* name1 rest)
         (define-cstructs* (names ...) rest)))))
