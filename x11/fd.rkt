#lang racket/base

(require ffi/unsafe)

(define process-image (ffi-lib #f))
(provide open-fd-input-port)

;; opens an input port given a file descriptor.
;; (open-fd-input-port 1) ;; would open stdin
(define open-fd-input-port 
  (get-ffi-obj "scheme_make_fd_input_port" process-image
    (_fun (fd : _int) (_scheme = "fd-port") (_int = 0) (_int = 0) -> _racket)))
