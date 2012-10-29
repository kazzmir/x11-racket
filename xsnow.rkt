;; XSnow by Jon Rafkind
;; 12/30/2005

;; 0.1 12/30/2005 - Snow falls and collects on top of windows. The trees are visible
;; 0.2 1/8/2006 - Ignore X11 errors via XSetErrorHandler. Fixed acceleration with
;; and without wind.
;; 0.3 7/1/2007 - Added santa
;; 0.4 7/13/2007 - Added KDE fix back in
;; 0.5 9/9/2008 - Fixes for mzscheme 4.0
;; 0.6 10/29/2012 - Fixes for racket 5.3

#lang racket

; (require trace)
; (require (lib "errortrace.ss" "errortrace"))
(require "x11.rkt")
(require "x11-xpm.rkt")
(require ffi/unsafe)

;; TODO: get rid of mpairs
(require scheme/mpair)

(define (real->int real)
  (inexact->exact (round real)))

(define-cstruct _SnowMap
  ((snowBits _string)
   (pixmap Pixmap)
   (width _int)
   (height _int)))

(define-struct pic (width height data pixmap) #:mutable)
(define-struct sprite (pic gc))
(define-struct (masked-sprite sprite) (mask))

(define (sprite-width sprite)
  (pic-width (sprite-pic sprite)))

(define (sprite-height sprite)
  (pic-height (sprite-pic sprite)))

(define santa-speed (list 1 2 4))

(define SNOW-FREE 25)
(define Max-Screen-Snow-Depth 50)
(define Snow-Flake-Max-Type 6)

(define (create-pixmap display rootWindow pic)
  (XCreateBitmapFromData display rootWindow (pic-data pic)
			 (pic-width pic) (pic-height pic)))

(define (create-tree display rootWindow)
  (let ((pic (make-pic 56 56 (list->cblock
			       (list
				 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
				 #x04 #x00 #x00 #x00 #x00 #x00 #x00 #x04 #x00 #x00
				 #x00 #x00 #x00 #x00 #x04 #x00 #x00 #x00 #x00 #x00
				 #x00 #x04 #x00 #x00 #x00 #x00 #x00 #x00 #x04 #x00
				 #x00 #x00 #x00 #x00 #x00 #x04 #x00 #x00 #x00 #x00
				 #x00 #x00 #x04 #x00 #x00 #x00 #x00 #x00 #x00 #x04
				 #x00 #x00 #x00 #x00 #x00 #x00 #x0a #x00 #x00 #x00
				 #x00 #x00 #x00 #x0a #x00 #x00 #x00 #x00 #x00 #x00
				 #x0a #x00 #x00 #x00 #x00 #x00 #x00 #x11 #x00 #x00
				 #x00 #x00 #x00 #x00 #x11 #x00 #x00 #x00 #x00 #x00
				 #x80 #x20 #x00 #x00 #x00 #x00 #x00 #x80 #x20 #x00
				 #x00 #x00 #x00 #x00 #x40 #x40 #x00 #x00 #x00 #x00
				 #x00 #x20 #x80 #x00 #x00 #x00 #x00 #x00 #xd0 #x0f
				 #x01 #x00 #x00 #x00 #x00 #x3c #xf9 #x07 #x00 #x00
				 #x00 #x00 #x87 #x10 #x1c #x00 #x00 #x00 #x80 #x80
				 #x20 #x20 #x00 #x00 #x00 #x00 #x40 #x20 #x00 #x00
				 #x00 #x00 #x00 #x40 #x40 #x00 #x00 #x00 #x00 #x00
				 #x20 #x42 #x00 #x00 #x00 #x00 #x00 #x20 #x82 #x00
				 #x00 #x00 #x00 #x00 #x10 #x81 #x00 #x00 #x00 #x00
				 #x00 #x18 #x00 #x01 #x00 #x00 #x00 #x00 #x48 #x20
				 #x03 #x00 #x00 #x00 #x00 #x04 #x00 #x02 #x00 #x00
				 #x00 #x00 #x02 #x07 #x04 #x00 #x00 #x00 #x80 #xe1
				 #x1c #x08 #x00 #x00 #x00 #xc0 #x18 #x20 #x30 #x00
				 #x00 #x00 #x30 #x1c #xc0 #x7f #x00 #x00 #x00 #x1c
				 #x0a #x80 #xf0 #x01 #x00 #x00 #xa2 #x8b #x00 #x81
				 #x07 #x00 #x00 #x89 #x44 #x00 #x01 #x00 #x00 #x00
				 #x00 #x02 #x10 #x02 #x00 #x00 #x00 #x00 #x0b #x00
				 #x04 #x00 #x00 #x00 #x80 #x00 #x00 #x0c #x00 #x00
				 #x00 #x60 #x00 #x00 #x10 #x00 #x00 #x00 #x30 #x00
				 #x00 #x60 #x00 #x00 #x00 #x0e #xf8 #x01 #xc2 #x00
				 #x00 #xe0 #x01 #x06 #x07 #x00 #x07 #x00 #xbf #x8f
				 #x01 #x39 #x00 #x78 #x00 #x00 #x78 #x00 #xc5 #x01
				 #xfc #x0f #x00 #x00 #x00 #x05 #xfe #x03 #x00 #x00
				 #x00 #x00 #x05 #x00 #x00 #x00 #x00 #x00 #x00 #x01
				 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
				 #x00 #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
				 #x00 #x00 #x00 #x01 #x00 #x00 #x00 #x00 #x00 #x00
				 #x00 #x00 #x00 #x00 #x00 #x01 #x00 #x00 #x00 #x00
				 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x01 #x00 #x00
				 #x00 #x00)
			       _byte)
		       #f)))
    (set-pic-pixmap! pic (create-pixmap display rootWindow pic))
    pic))

(define (create-sleigh display rootWindow size)
  ;; santas is a list with 3 items. each item containing 3 pictures of reindeer
  (let ((sleighs (list-ref 
		   (list
		     (list
		       (make-pic 64 8
				 (list->cblock
				   (list #x20 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x20
					 #x00 #x00 #x08 #x00 #x02 #x80 #x00 #x60 #x00
					 #x00 #x05 #x40 #x01 #x50 #x00 #xb2 #x01 #x00
					 #x02 #x80 #x00 #x20 #x00 #xb2 #x04 #x10 #xa2
					 #x84 #x28 #x21 #x00 #xfe #x2c #xe1 #x03 #xf8
					 #x00 #x3e #x00 #x94 #x02 #x60 #x01 #x58 #x00
					 #x16 #x00 #xff #x01 #x10 #x02 #x84 #x00 #x21
					 #x00)
				   _byte)
				 #f)
		       (make-pic 64 8
				 (list->cblock
				   (list #x20 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x20
					 #x00 #x00 #x08 #x00 #x02 #x80 #x00 #x60 #x00
					 #x00 #x05 #x40 #x01 #x50 #x00 #xb2 #x01 #x00
					 #x02 #x80 #x00 #x20 #x00 #xb2 #x04 #x10 #xa2
					 #x84 #x28 #x21 #x00 #xfe #x2c #xe1 #x03 #xf8
					 #x00 #x3e #x00 #x94 #x02 #x60 #x01 #x58 #x00
					 #x16 #x00 #xff #x01 #x20 #x01 #x48 #x00 #x12
					 #x00)
				   _byte)
				 #f)
		       (make-pic 64 8
				 (list->cblock
				   (list #x20 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x20 
					 #x00 #x00 #x08 #x00 #x02 #x80 #x00 #x60 #x00 
					 #x00 #x05 #x40 #x01 #x50 #x00 #xb2 #x01 #x00 
					 #x02 #x80 #x00 #x20 #x00 #xb2 #x04 #x10 #xa2 
					 #x84 #x28 #x21 #x00 #xfe #x2c #xe1 #x03 #xf8 
					 #x00 #x3e #x00 #x94 #x02 #x60 #x01 #x58 #x00 
					 #x16 #x00 #xff #x01 #xc0 #x00 #x30 #x00 #x0c 
					 #x00)
				   _byte)
				 #f))
		     (list
		       (make-pic 128 16
				 (list->cblock
				   (list #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
					 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
					 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x08 
					 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
					 #x00 #x00 #x40 #x00 #x00 #x00 #x08 #x00 #x00 
					 #x54 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
					 #xc2 #x00 #x00 #x10 #x00 #x00 #x80 #x90 #x00 
					 #x00 #x68 #x30 #x00 #x00 #x00 #x00 #x40 #x00 
					 #x00 #x10 #x00 #x00 #x00 #x13 #x00 #x00 #x48 
					 #x30 #x00 #x00 #x00 #x00 #x04 #x00 #x00 #x20 
					 #x02 #x00 #x00 #x37 #x00 #x00 #x7c #xc0 #x03 
					 #x00 #x00 #x00 #x08 #x00 #x00 #xc0 #x00 #x00 
					 #x00 #x0c #x00 #x00 #x7c #xc0 #x0b #x00 #x00 
					 #x00 #x0c #x00 #x00 #xc0 #x00 #x00 #x00 #x0c 
					 #x00 #x00 #x5c #xc0 #x31 #x00 #x00 #x01 #x0c 
					 #x08 #x00 #xc0 #x80 #x00 #xfc #x0f #x00 #x00 
					 #x74 #xc0 #x71 #x00 #x00 #x03 #x0f #xfc #x70 
					 #xcd #xf8 #x7f #x04 #x0c #x00 #x00 #x0c #x80 
					 #xf0 #x0c #x23 #xfe #x0f #x00 #xe0 #xff #x00 
					 #x00 #xfc #x0f #x00 #x00 #xf8 #xff #xf0 #x0c 
					 #x03 #xfc #x07 #x00 #xc0 #xff #x00 #x00 #xfc 
					 #x0f #x00 #x00 #x30 #xc3 #x38 #x00 #x00 #xfc 
					 #x03 #x00 #xc0 #x7f #x00 #x00 #xfc #x07 #x00 
					 #x00 #x30 #xc3 #x18 #x00 #x00 #xfc #x03 #x00 
					 #xc0 #x70 #x00 #x00 #x7c #x07 #x00 #x00 #x30 
					 #xc3 #x0c #x00 #x00 #x06 #x0e #x00 #x20 #xe0 
					 #x00 #x00 #x08 #x0c #x00 #x00 #xff #xff #x07 
					 #x00 #x00 #x03 #x08 #x00 #x30 #x80 #x00 #x00 
					 #x06 #x00 #x00 #x00)
				   _byte)
				 #f)
		       (make-pic 128 16
				 (list->cblock
				   (list
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x80 #x00 #x00 #x00 #x08 #x00 #x00 
				     #x50 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x40 #x00 #x00 #x80 #x06 #x00 #x80 #x12 #x00 
				     #x00 #x68 #xf0 #x00 #x00 #x00 #x00 #x12 #x00 
				     #x00 #x40 #x02 #x00 #x00 #x13 #x00 #x00 #x48 
				     #x30 #x01 #x00 #x00 #x00 #x20 #x00 #x00 #x20 
				     #x02 #x00 #x00 #x36 #x00 #x00 #x7c #xc0 #x03 
				     #x00 #x00 #x00 #x08 #x00 #x00 #x40 #x01 #x00 
				     #x00 #x0c #x00 #x00 #x7c #xc0 #x1b #x00 #x00 
				     #x00 #x0c #x00 #x00 #xc0 #x00 #x00 #x00 #x0c 
				     #x00 #x00 #x5c #xc0 #x31 #x00 #x00 #x01 #x0c 
				     #xcc #x00 #xc0 #x00 #x00 #x03 #x0f #x00 #x00 
				     #x74 #xc0 #xb0 #x00 #x80 #x03 #x8c #xef #x02 
				     #xf8 #xe0 #x3e #x03 #x0c #x00 #x00 #x0c #xc0 
				     #xf0 #x0d #x1b #xfe #x0f #x00 #xe0 #xff #x00 
				     #x00 #xfc #x0f #x00 #x00 #xf8 #xff #xf0 #x0c 
				     #x02 #xfc #x07 #x00 #x80 #x7f #x00 #x00 #xfc 
				     #x07 #x00 #x00 #x30 #xc3 #x38 #x00 #x00 #xf8 
				     #x03 #x00 #x80 #x3f #x00 #x00 #xf8 #x03 #x00 
				     #x00 #x30 #xc3 #x18 #x00 #x00 #x38 #x03 #x00 
				     #x80 #x33 #x00 #x00 #x38 #x03 #x00 #x00 #x30 
				     #xc3 #x0c #x00 #x00 #x18 #x01 #x00 #x80 #x31 
				     #x00 #x00 #x30 #x03 #x00 #x00 #xff #xff #x07 
				     #x00 #x00 #x08 #x01 #x00 #x80 #x11 #x00 #x00 
				     #x10 #x03 #x00 #x00)
				   _byte)
				 #f)
		       (make-pic 128 16
				 (list->cblock
				   (list 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x20 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x20 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x01 #x00 #x00 #x04 #x00 #x00 
				     #x64 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #xc0 #x00 #x00 #x50 #x0c #x00 #x00 #xe5 #x00 
				     #x00 #x60 #x30 #x00 #x00 #x00 #x00 #x02 #x00 
				     #x00 #x30 #x00 #x00 #x00 #x32 #x00 #x00 #x48 
				     #x30 #x01 #x00 #x00 #x00 #x20 #x00 #x00 #x20 
				     #x02 #x00 #x00 #x16 #x00 #x00 #x7c #xc0 #x02 
				     #x00 #x00 #x00 #x0c #x00 #x00 #xc0 #x00 #x00 
				     #x00 #x0c #x00 #x00 #x7c #xc0 #x0b #x00 #x00 
				     #x00 #x0c #x00 #x00 #xc0 #x00 #x00 #x00 #x0c 
				     #x00 #x00 #x5c #xc0 #x31 #x00 #x00 #x03 #x0c 
				     #x80 #xf0 #xd8 #xc0 #x00 #x30 #x0f #x00 #x00 
				     #x74 #xc0 #xb0 #x00 #xe0 #x02 #x0e #xef #x20 
				     #xc0 #xc0 #xff #x07 #x0c #x00 #x00 #x0c #xc0 
				     #xf0 #x0c #x03 #xfc #x0f #x00 #xe0 #xff #x00 
				     #x00 #xfe #x0f #x00 #x00 #xf8 #xff #xf0 #x0c 
				     #x03 #xf8 #x07 #x00 #xc0 #xff #x00 #x00 #xfc 
				     #x0f #x00 #x00 #x30 #xc3 #x38 #x00 #x00 #x38 
				     #x03 #x00 #x80 #x3b #x00 #x00 #x3c #x03 #x00 
				     #x00 #x30 #xc3 #x18 #x00 #x00 #x70 #x01 #x00 
				     #x00 #x33 #x00 #x00 #x38 #x03 #x00 #x00 #x30 
				     #xc3 #x0c #x00 #x00 #xa0 #x01 #x00 #x00 #x1e 
				     #x00 #x00 #xf0 #x01 #x00 #x00 #xff #xff #x07 
				     #x00 #x00 #xe0 #x00 #x00 #x00 #x1e #x00 #x00 
				     #xe0 #x00 #x00 #x00)
				   _byte)
				 #f))
		     (list
		       (make-pic 256 32
				 (list->cblock
				   (list 
				     #x00 #x00 #xc0 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xe0 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #xf0 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x08 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x40 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xf0 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x08 #x00 #x00 #x00 #x00 #x00 #x00 #x40 
				     #x00 #x00 #x00 #x00 #x00 #x80 #x09 #x00 #x00 
				     #x00 #x00 #x00 #x00 #xf0 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x18 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x60 #x00 #x00 #x00 #x00 
				     #x00 #x18 #x9b #x00 #x00 #x00 #x00 #x00 #x00 
				     #xf0 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x04 #xc8 #x01 #x00 #x00 #x00 #x00 #x00 
				     #x64 #x00 #x00 #x00 #x00 #x40 #x08 #x87 #x00 
				     #x00 #x00 #x00 #x00 #x00 #xf0 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x0c #xf0 #x00 
				     #x00 #x00 #x00 #x00 #x21 #x28 #x00 #x00 #x00 
				     #x00 #xc0 #x08 #x43 #x00 #x00 #x00 #x00 #x00 
				     #x00 #xf0 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x08 #x70 #x00 #x00 #x00 #x00 #x00 
				     #x31 #x18 #x00 #x00 #x00 #x00 #x80 #x04 #x31 
				     #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x0f #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x08 #x30 
				     #x00 #x00 #x00 #x00 #x00 #x09 #x08 #x00 #x00 
				     #x00 #x00 #x00 #x05 #x0b #x00 #x00 #x00 #x00 
				     #x00 #x00 #xf0 #x0f #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x08 #x18 #x00 #x00 #x00 #x00 
				     #x00 #x0f #x0c #x00 #x00 #x00 #x00 #x00 #x07 
				     #x07 #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x0f 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x10 
				     #x08 #x00 #x00 #x00 #x00 #x00 #x04 #x06 #x00 
				     #x00 #x00 #x00 #x00 #x1f #x05 #x00 #x00 #x00 
				     #x00 #x00 #x00 #xf0 #x0f #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x30 #x07 #x00 #x00 #x00 
				     #x00 #x00 #x08 #x07 #x00 #x00 #x00 #x00 #x00 
				     #x0e #x03 #x00 #x00 #x00 #x00 #xf0 #x00 #xfc 
				     #xf0 #x0f #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #xe0 #x02 #x00 #x00 #x00 #x00 #x00 #xf0 #x00 
				     #x00 #x00 #x00 #x00 #x00 #xf0 #x00 #x00 #x00 
				     #x00 #x00 #xf0 #x00 #xfe #xf0 #x3f #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #xf0 #x00 #x00 #x00 
				     #x00 #x00 #x00 #xf0 #x00 #x00 #x00 #x00 #x00 
				     #x00 #xf0 #x00 #x00 #x00 #x00 #x00 #xf0 #x00 
				     #xff #xf0 #x4f #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #xf0 #x00 #x00 #x00 #x00 #x00 #x00 #xf0 
				     #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x00 #x00 
				     #x00 #x00 #x00 #xf0 #x00 #xff #xf0 #x8f #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x00 #x00 
				     #x00 #x00 #x00 #x00 #xf0 #x00 #x00 #x00 #x00 
				     #x00 #x00 #xf0 #x00 #x00 #x00 #x00 #x00 #xf0 
				     #x00 #xff #xf0 #x01 #x0f #x00 #x00 #x00 #x00 
				     #x01 #x00 #xf0 #x00 #xe0 #x00 #x00 #x02 #x00 
				     #xf0 #x00 #xc0 #x00 #x00 #xf8 #xff #xff #x00 
				     #x00 #x00 #x00 #x00 #xf0 #x00 #xff #xf0 #x01 
				     #x0f #x00 #x00 #x00 #x00 #x03 #x00 #xf8 #xff 
				     #xff #x70 #xc0 #x0f #x80 #xff #x1f #x70 #x70 
				     #xc0 #x1f #x00 #xf0 #x00 #x00 #x00 #x00 #x00 
				     #xf0 #x00 #xff #xf0 #x01 #x1f #x00 #x00 #x00 
				     #x00 #x07 #x80 #xf7 #x00 #xf0 #xff #xa0 #xbf 
				     #x7b #xf0 #xe0 #xff #xff #x3f #x38 #x00 #xf0 
				     #x00 #x00 #x00 #x00 #x00 #xf0 #x01 #xff #xf0 
				     #x00 #x6f #x00 #x00 #x00 #xf0 #x0f #xfc #xf0 
				     #x00 #xc0 #xf0 #xfb #x7f #x00 #xf0 #x00 #xf0 
				     #xf0 #x00 #x78 #x00 #xf0 #x00 #x00 #x00 #x00 
				     #x00 #xf0 #xff #xff #xff #x00 #xff #xf0 #x00 
				     #x0f #x0e #xfc #xff #xff #x00 #x00 #x00 #x00 
				     #xfc #xff #xff #x00 #x00 #x00 #x00 #xf8 #xff 
				     #xff #x00 #x00 #x00 #x00 #x00 #xf0 #xff #xff 
				     #xff #x80 #xff #xff #xff #xff #x01 #xfc #xff 
				     #xff #x00 #x00 #x00 #x00 #xf8 #xff #x7f #x00 
				     #x00 #x00 #x00 #xf0 #xff #xff #x00 #x00 #x00 
				     #x00 #x00 #xe0 #xff #xff #xff #x80 #xff #xf0 
				     #x00 #x0f #x00 #xf8 #xff #x7f #x00 #x00 #x00 
				     #x00 #xf0 #xff #x7f #x00 #x00 #x00 #x00 #xf0 
				     #xff #x7f #x00 #x00 #x00 #x00 #x00 #xc0 #xff 
				     #xff #xff #xc0 #xff #x80 #x00 #x00 #x00 #xf8 
				     #xff #x3f #x00 #x00 #x00 #x00 #xf0 #xff #x3f 
				     #x00 #x00 #x00 #x00 #xf0 #xff #x7f #x00 #x00 
				     #x00 #x00 #x00 #x00 #x0f #x0f #xf0 #xf0 #x07 
				     #x00 #x00 #x00 #x00 #xf8 #xff #x0f #x00 #x00 
				     #x00 #x00 #xf0 #xff #x1f #x00 #x00 #x00 #x00 
				     #xf0 #xff #x3f #x00 #x00 #x00 #x00 #x00 #x00 
				     #x0f #x0f #xf0 #xf0 #x03 #x00 #x00 #x00 #x00 
				     #xf8 #xff #x0f #x00 #x00 #x00 #x00 #xf0 #xff 
				     #x1f #x00 #x00 #x00 #x00 #xf0 #xff #x3f #x00 
				     #x00 #x00 #x00 #x00 #x00 #x0f #x0f #xf0 #xf8 
				     #x03 #x00 #x00 #x00 #x00 #xf8 #xdf #x0f #x00 
				     #x00 #x00 #x00 #x78 #x00 #x1f #x00 #x00 #x00 
				     #x00 #xf0 #x1f #x3f #x00 #x00 #x00 #x00 #x00 
				     #x00 #x0f #x0f #xf0 #xfc #x01 #x00 #x00 #x00 
				     #x00 #xf8 #x07 #x1f #x00 #x00 #x00 #x00 #x3c 
				     #x00 #x1f #x00 #x00 #x00 #x00 #xf8 #x0f #x7c 
				     #x00 #x00 #x00 #x00 #x00 #xff #xff #xff #xff 
				     #xfe #x00 #x00 #x00 #x00 #x00 #x3c #x00 #x7c 
				     #x00 #x00 #x00 #x00 #x0e #x00 #x7c #x00 #x00 
				     #x00 #x00 #x68 #x00 #xf0 #x00 #x00 #x00 #x00 
				     #x00 #xff #xff #xff #xff #x7f #x00 #x00 #x00 
				     #x00 #x00 #x0e #x00 #xe0 #x00 #x00 #x00 #x00 
				     #x0e #x00 #x70 #x00 #x00 #x00 #x00 #x3e #x00 
				     #xc0 #x00 #x00 #x00 #x00 #x00 #xff #xff #xff 
				     #xff #x3f #x00 #x00 #x00 #x00 #x00 #x0f #x00 
				     #xc0 #x00 #x00 #x00 #x00 #x07 #x00 #xe0 #x00 
				     #x00 #x00 #x00 #x1e #x00 #x80 #x00 #x00 #x00 
				     #x00 #x00 #xff #xff #xff #xff #x1f #x00 #x00 
				     #x00 #x00 #x00 #x03 #x00 #xc0 #x01 #x00 #x00 
				     #x80 #x03 #x00 #xc0 #x00 #x00 #x00 #x00 #x07 
				     #x00 #x00 #x01 #x00 #x00 #x00 #x00)
				   _byte)
				 #f)
		       (make-pic 256 32
				 (list->cblock
				   (list 
				     #x00 #x00 #xc0 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xc0 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #xc0 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xc0 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x10 #x01 #x00 #x00 
				     #x00 #x00 #x00 #x00 #xe0 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x42 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x40 #x00 #x00 #x00 #x00 
				     #x20 #x08 #x9b #x00 #x00 #x00 #x00 #x00 #x00 
				     #xe0 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x02 #x62 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x60 #x00 #x00 #x00 #x00 #x60 #x0c #xce #x00 
				     #x00 #x00 #x00 #x00 #x00 #xf0 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x02 #x32 #x00 
				     #x00 #x00 #x00 #x00 #x40 #x14 #x00 #x00 #x00 
				     #x00 #xc0 #x04 #x23 #x00 #x00 #x00 #x00 #x00 
				     #x00 #xf0 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x02 #x12 #x00 #x00 #x00 #x00 #x00 
				     #x62 #x0c #x00 #x00 #x00 #x00 #x80 #x04 #x13 
				     #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x7f #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x04 #x0b 
				     #x00 #x00 #x00 #x00 #x00 #x12 #x0c #x00 #x00 
				     #x00 #x00 #x00 #x0f #x0b #x00 #x00 #x00 #x00 
				     #x00 #x00 #xf0 #x8f #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x0c #x04 #x00 #x00 #x00 #x00 
				     #x00 #x16 #x04 #x00 #x00 #x00 #x00 #x00 #x0f 
				     #x0f #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x0f 
				     #x03 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x08 
				     #x06 #x00 #x00 #x00 #x00 #x00 #x0c #x06 #x00 
				     #x00 #x00 #x00 #x00 #x1e #x07 #x00 #x00 #x00 
				     #x00 #x00 #x00 #xf0 #x0f #x04 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x10 #x01 #x00 #x00 #x00 
				     #x00 #x00 #x18 #x03 #x00 #x00 #x00 #x00 #x00 
				     #x6c #x03 #x00 #x00 #x00 #x00 #xf0 #x00 #xfc 
				     #xf0 #x0f #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #xe0 #x00 #x00 #x00 #x00 #x00 #x00 #xb0 #x01 
				     #x00 #x00 #x00 #x00 #x00 #xf0 #x00 #x00 #x00 
				     #x00 #x00 #xf0 #x00 #xfe #xf0 #x3f #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #xf0 #x01 #x00 #x00 
				     #x00 #x00 #x00 #xf0 #x00 #x00 #x00 #x00 #x00 
				     #x00 #xf0 #x00 #x00 #x00 #x00 #x00 #xf0 #x00 
				     #xff #xf0 #xcf #x03 #x00 #x00 #x00 #x00 #x00 
				     #x00 #xf0 #x00 #x00 #x00 #x00 #x00 #x00 #xf0 
				     #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x00 #x00 
				     #x00 #x00 #x00 #xf0 #x00 #xff #xf0 #x0f #x06 
				     #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x00 #x00 
				     #x00 #x00 #x00 #x00 #xf0 #x00 #x00 #x00 #x00 
				     #x00 #x00 #xf0 #x00 #x00 #x00 #x00 #x00 #xf0 
				     #x00 #xff #xf0 #x01 #x0f #x00 #x00 #x00 #x00 
				     #x03 #x00 #xf0 #x00 #xf0 #xf0 #x00 #x00 #x00 
				     #xf0 #x00 #x80 #x00 #x00 #x0f #x80 #xff #x00 
				     #x00 #x00 #x00 #x00 #xf0 #x00 #xff #xf0 #x00 
				     #x1f #x00 #x00 #x00 #x00 #x07 #x00 #xf0 #x1f 
				     #xf0 #xfd #x01 #x00 #x00 #xf0 #xff #x31 #xe0 
				     #xf8 #xef #xff #xf0 #x00 #x00 #x00 #x00 #x00 
				     #xf0 #x01 #xff #xf0 #x00 #x6f #x00 #x00 #x00 
				     #xe0 #x0f #x00 #xf8 #xc0 #xff #xfe #x06 #x00 
				     #x60 #xff #x00 #x7e #xf4 #x07 #x0f #x00 #xf0 
				     #x00 #x00 #x00 #x00 #x00 #xf0 #x03 #xff #xf0 
				     #x00 #x8f #x00 #x00 #x00 #x3e #x0f #x00 #xf6 
				     #x00 #xf0 #x30 #xf8 #x5d #x1f #xf0 #x00 #xf0 
				     #xf3 #x00 #x0d #x00 #xf0 #x00 #x00 #x00 #x00 
				     #x00 #xf0 #xff #xff #xff #x00 #xff #xf1 #x00 
				     #xef #x01 #xfc #xff #xff #x00 #x00 #x00 #x00 
				     #xfc #xff #xff #x00 #x00 #x00 #x00 #xf8 #xff 
				     #xff #x00 #x00 #x00 #x00 #x00 #xf0 #xff #xff 
				     #xff #x00 #xff #xff #xff #x1f #x00 #xfc #xff 
				     #xff #x00 #x00 #x00 #x00 #xe0 #xff #xff #x00 
				     #x00 #x00 #x00 #xf0 #xff #xff #x00 #x00 #x00 
				     #x00 #x00 #xe0 #xff #xff #xff #x80 #xff #xf0 
				     #x00 #x0e #x00 #xf8 #xff #x7f #x00 #x00 #x00 
				     #x00 #xc0 #xff #x7f #x00 #x00 #x00 #x00 #xf0 
				     #xff #x7f #x00 #x00 #x00 #x00 #x00 #xc0 #xff 
				     #xff #xff #xc0 #xff #x00 #x00 #x00 #x00 #xf0 
				     #xff #x3f #x00 #x00 #x00 #x00 #xc0 #xff #x3f 
				     #x00 #x00 #x00 #x00 #xe0 #xff #x3f #x00 #x00 
				     #x00 #x00 #x00 #x00 #x0f #x0f #xf0 #xf0 #x07 
				     #x00 #x00 #x00 #x00 #xe0 #xff #x0f #x00 #x00 
				     #x00 #x00 #xc0 #xff #x0f #x00 #x00 #x00 #x00 
				     #xc0 #xff #x0f #x00 #x00 #x00 #x00 #x00 #x00 
				     #x0f #x0f #xf0 #xf0 #x07 #x00 #x00 #x00 #x00 
				     #xc0 #x1f #x07 #x00 #x00 #x00 #x00 #xc0 #xff 
				     #x0f #x00 #x00 #x00 #x00 #xc0 #xbf #x0f #x00 
				     #x00 #x00 #x00 #x00 #x00 #x0f #x0f #xf0 #xf8 
				     #x03 #x00 #x00 #x00 #x00 #xc0 #x0f #x07 #x00 
				     #x00 #x00 #x00 #xc0 #x8f #x0f #x00 #x00 #x00 
				     #x00 #xc0 #x0f #x0f #x00 #x00 #x00 #x00 #x00 
				     #x00 #x0f #x0f #xf0 #xfc #x01 #x00 #x00 #x00 
				     #x00 #xc0 #x0f #x07 #x00 #x00 #x00 #x00 #xc0 
				     #x0f #x07 #x00 #x00 #x00 #x00 #x80 #x0f #x07 
				     #x00 #x00 #x00 #x00 #x00 #xff #xff #xff #xff 
				     #xff #x00 #x00 #x00 #x00 #x00 #xc0 #x01 #x03 
				     #x00 #x00 #x00 #x00 #xc0 #x03 #x07 #x00 #x00 
				     #x00 #x00 #x80 #x07 #x07 #x00 #x00 #x00 #x00 
				     #x00 #xff #xff #xff #xff #x7f #x00 #x00 #x00 
				     #x00 #x00 #xe0 #x00 #x03 #x00 #x00 #x00 #x00 
				     #xc0 #x01 #x03 #x00 #x00 #x00 #x00 #x80 #x03 
				     #x07 #x00 #x00 #x00 #x00 #x00 #xff #xff #xff 
				     #xff #x3f #x00 #x00 #x00 #x00 #x00 #xe0 #x00 
				     #x03 #x00 #x00 #x00 #x00 #xc0 #x01 #x03 #x00 
				     #x00 #x00 #x00 #x80 #x03 #x07 #x00 #x00 #x00 
				     #x00 #x00 #xff #xff #xff #xff #x1f #x00 #x00 
				     #x00 #x00 #x00 #x60 #x00 #x03 #x00 #x00 #x00 
				     #x00 #xc0 #x00 #x03 #x00 #x00 #x00 #x00 #x80 
				     #x01 #x03 #x00 #x00 #x00 #x00 #x00)
				   _byte)
				 #f)
		       (make-pic 256 32
				 (list->cblock
				   (list 
				     #x00 #x00 #xc0 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xc0 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x04 #x00 #x00 #x00 
				     #x00 #x00 #x00 #xc0 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x04 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x24 #x00 #x00 #x00 #x00 #x00 #x00 #xe0 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x04 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x10 #x2c #x00 #x00 
				     #x00 #x00 #x00 #x00 #xe0 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x02 #x88 #x01 #x00 
				     #x00 #x00 #x00 #x00 #x90 #x00 #x00 #x00 #x00 
				     #x00 #x10 #xbc #x00 #x00 #x00 #x00 #x00 #x00 
				     #xf0 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x02 #x88 #x00 #x00 #x00 #x00 #x00 #x00 
				     #xd0 #x00 #x00 #x00 #x00 #x40 #x10 #xc4 #x00 
				     #x00 #x00 #x00 #x00 #x00 #xf0 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x02 #xf0 #x00 
				     #x00 #x00 #x00 #x00 #x11 #x70 #x00 #x00 #x00 
				     #x00 #x80 #x11 #x74 #x00 #x00 #x00 #x00 #x00 
				     #x00 #xf0 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x04 #x70 #x00 #x00 #x00 #x00 #x00 
				     #x09 #x10 #x00 #x00 #x00 #x00 #x00 #x0b #x14 
				     #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x0f #x00 
				     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x04 #x08 
				     #x00 #x00 #x00 #x00 #x00 #x05 #x08 #x00 #x00 
				     #x00 #x00 #x00 #x0e #x0f #x00 #x00 #x00 #x00 
				     #x00 #x00 #xf0 #xef #x00 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x0c #x0e #x00 #x00 #x00 #x00 
				     #x00 #x07 #x04 #x00 #x00 #x00 #x00 #x00 #x1c 
				     #x03 #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x0f 
				     #x03 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x08 
				     #x04 #x00 #x00 #x00 #x00 #x00 #x06 #x06 #x00 
				     #x00 #x00 #x00 #x00 #x9c #x03 #x00 #x00 #x00 
				     #x00 #x00 #x00 #xf0 #x0f #x04 #x00 #x00 #x00 
				     #x00 #x00 #x00 #x00 #x08 #x03 #x00 #x00 #x00 
				     #x00 #x00 #x0c #x03 #x00 #x00 #x00 #x00 #x00 
				     #xe8 #x01 #x00 #x00 #x00 #x00 #xf0 #x00 #xfc 
				     #xf0 #x0c #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				     #xf8 #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x00 
				     #x00 #x00 #x00 #x00 #x00 #xf0 #x00 #x00 #x00 
				     #x00 #x00 #xf0 #x00 #xfe #xf0 #x1f #x00 #x00 
				     #x00 #x00 #x00 #x00 #x00 #xf0 #x00 #x00 #x00 
				     #x00 #x00 #x00 #xf0 #x00 #x00 #x00 #x00 #x00 
				     #x00 #xf0 #x00 #x00 #x00 #x00 #x00 #xf0 #x00 
				     #xff #xf0 #x6f #x00 #x00 #x00 #x00 #x00 #x00 
				     #x00 #xf0 #x00 #x00 #x00 #x00 #x00 #x00 #xf0 
				     #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x00 #x00 
				     #x00 #x00 #x00 #xf0 #x00 #xff #xf0 #x8f #x07 
				     #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x00 #x00 
				     #x00 #x00 #x80 #x3f #xf0 #x00 #x00 #x00 #x00 
				     #x00 #xf0 #xf0 #x00 #x00 #x00 #x00 #x00 #xf0 
				     #x00 #xff #xf0 #x03 #x0f #x00 #x00 #x00 #x00 
				     #x07 #x00 #xf8 #x00 #x00 #xe0 #x00 #xff #xc0 
				     #xf3 #x00 #x70 #x00 #x00 #x00 #x0d #xff #x00 
				     #x00 #x00 #x00 #x00 #xf0 #x00 #xff #xf0 #x00 
				     #x1f #x00 #x00 #x00 #x00 #x07 #x00 #xf8 #xeb 
				     #xf0 #xf0 #xf5 #x0f #x00 #xfc #xff #xff #xf0 
				     #x00 #xf7 #x00 #xf0 #x00 #x00 #x00 #x00 #x00 
				     #xf0 #x03 #xff #xf0 #x00 #x6f #x00 #x00 #x00 
				     #xfc #x0c #x00 #xf4 #x80 #xf7 #xfe #x00 #x0e 
				     #x00 #xf0 #x00 #xf0 #xff #xff #x1f #x00 #xf0 
				     #x00 #x00 #x00 #x00 #x00 #xf0 #x03 #xff #xf0 
				     #x00 #xcf #x00 #x00 #xc0 #x03 #x0e #x00 #xf6 
				     #x00 #xf8 #xf0 #x00 #x1e #x00 #xf0 #x00 #xe0 
				     #xf0 #x00 #x1f #x00 #xf0 #x00 #x00 #x00 #x00 
				     #x00 #xf0 #xff #xff #xff #x00 #xff #xf0 #x00 
				     #x2f #x00 #xf8 #xff #xff #x00 #x00 #x00 #x00 
				     #xfc #xff #xff #x00 #x00 #x00 #x00 #xf6 #xff 
				     #xff #x00 #x00 #x00 #x00 #x00 #xf0 #xff #xff 
				     #xff #x00 #xff #xff #xff #x3f #x00 #xf0 #xff 
				     #xff #x00 #x00 #x00 #x00 #xf8 #xff #xff #x00 
				     #x00 #x00 #x00 #xfc #xff #xff #x00 #x00 #x00 
				     #x00 #x00 #xe0 #xff #xff #xff #x80 #xff #xf0 
				     #x00 #x0f #x00 #xe0 #xff #xff #x00 #x00 #x00 
				     #x00 #xf0 #xff #x7f #x00 #x00 #x00 #x00 #xf8 
				     #xff #x7f #x00 #x00 #x00 #x00 #x00 #xc0 #xff 
				     #xff #xff #xc0 #xff #x40 #x00 #x08 #x00 #xe0 
				     #xff #x7f #x00 #x00 #x00 #x00 #xe0 #xff #x3f 
				     #x00 #x00 #x00 #x00 #xf0 #xff #x3f #x00 #x00 
				     #x00 #x00 #x00 #x00 #x0f #x0f #xf0 #xf0 #x07 
				     #x00 #x00 #x00 #x00 #xc0 #x0f #x0f #x00 #x00 
				     #x00 #x00 #xe0 #xef #x0f #x00 #x00 #x00 #x00 
				     #xf0 #x8f #x0f #x00 #x00 #x00 #x00 #x00 #x00 
				     #x0f #x0f #xf0 #xf0 #x03 #x00 #x00 #x00 #x00 
				     #x80 #x9f #x0f #x00 #x00 #x00 #x00 #x80 #x0f 
				     #x0f #x00 #x00 #x00 #x00 #xe0 #x0f #x0f #x00 
				     #x00 #x00 #x00 #x00 #x00 #x0f #x0f #xf0 #xf8 
				     #x01 #x00 #x00 #x00 #x00 #x00 #x9f #x03 #x00 
				     #x00 #x00 #x00 #x00 #x0f #x0f #x00 #x00 #x00 
				     #x00 #xe0 #x0f #x07 #x00 #x00 #x00 #x00 #x00 
				     #x00 #x0f #x0f #xf0 #xfc #x00 #x00 #x00 #x00 
				     #x00 #x00 #xdf #x01 #x00 #x00 #x00 #x00 #x00 
				     #x1f #x07 #x00 #x00 #x00 #x00 #xc0 #x9f #x03 
				     #x00 #x00 #x00 #x00 #x00 #xff #xff #xff #xff 
				     #xff #x00 #x00 #x00 #x00 #x00 #x00 #xee #x01 
				     #x00 #x00 #x00 #x00 #x00 #xfe #x03 #x00 #x00 
				     #x00 #x00 #x00 #xff #x01 #x00 #x00 #x00 #x00 
				     #x00 #xff #xff #xff #xff #x7f #x00 #x00 #x00 
				     #x00 #x00 #x00 #xee #x00 #x00 #x00 #x00 #x00 
				     #x00 #xfc #x01 #x00 #x00 #x00 #x00 #x00 #xfe 
				     #x00 #x00 #x00 #x00 #x00 #x00 #xff #xff #xff 
				     #xff #x3f #x00 #x00 #x00 #x00 #x00 #x00 #x5e 
				     #x00 #x00 #x00 #x00 #x00 #x00 #xfc #x01 #x00 
				     #x00 #x00 #x00 #x00 #x7e #x00 #x00 #x00 #x00 
				     #x00 #x00 #xff #xff #xff #xff #x1f #x00 #x00 
				     #x00 #x00 #x00 #x00 #x18 #x00 #x00 #x00 #x00 
				     #x00 #x00 #x70 #x00 #x00 #x00 #x00 #x00 #x00 
				     #x18 #x00 #x00 #x00 #x00 #x00 #x00)
				   _byte)
				 #f)))
		 size)))
    (map (lambda (pic) 
	   (set-pic-pixmap! pic (create-pixmap display rootWindow pic))
	   pic)
	 sleighs)))

(define (create-sleigh2 display window size)
  (define (create-xpm data)
    (XpmCreatePixmapFromData display window data #f))
  (let ((sleighs (list-ref (list
			     (list
			       (list->cblock
				 '(
				   ;; width height num_colors chars_per_pixel 
				   "    56     8        6            1"
				   ;; colors 
				   ". c #ffffff"
				   "# c #ff9999"
				   "a c #ff0000"
				   "b c none"
				   "c c #663333"
				   "d c #000000"
				   ;; /* pixels */
				   "bbbaa.bbbbbbbbbbbbbbbbbbbbbcbbbbbbbbbbbbbcbbbbbbbbbbbbbc"
				   "bb.b#bbbbbbbbbbbbbbbbbbbcbcbbbbbbbbbbbcbcbbbbbbbbbbbcbcb"
				   "bbbb#ab.bbbbbbbbbbbbbbbbbcbbbbbbbbbbbbbcbbbbbbbbbbbbbcbb"
				   "bdbbaaabddbdbdbbdbbbcbbbbcbbbbbbbbcbbbbcbbbdbdbbcbbbbcab"
				   "bdb...adbbdbbbbbbbbbbcccccbbbbbbbbbcccccbbbbbbbbbcccccbb"
				   "bddddadbbbdbbbbbbbbbbccbcbbbbdbdbbbccbcbbbbbbbbbbccbcbbb"
				   "bbdbdba.bddbbbbbbbbbbbcbcbbbbbbbbbbbcbcbbbbbbbbbbbcbcbbb"
				   "ddddddddddbbbbbbbbbbbbccbbbbbbbbbbbbccbbbbbbbbbbbbccbbbb")
				 _string)
			       (list->cblock
				 '(
				   ;; width height num_colors chars_per_pixel
				   "    56     8        6            1"
				   ;; colors
				   ". c #ffffff"
				   "# c #ff9999"
				   "a c #ff0000"
				   "b c none"
				   "c c #663333"
				   "d c #000000"
				   ;; pixels
				   "bbbaa.bbbbbbbbbbbbbbbbbbbbbcbbbbbbbbbbbbbcbbbbbbbbbbbbbc"
				   "bb.b#bbbbbbbbbbbbbbbbbbbcbcbbbbbbbbbbbcbcbbbbbbbbbbbcbcb"
				   "bbbb#ab.bbbbbbbbbbbbbbbbbcbbbbbbbbbbbbbcbbbbbbbbbbbbbcbb"
				   "bdbbaaabddbbbbbbbbbbcbbbbcbbbbbbbbcbbbbcbbbbbbbbcbbbbcbb"
				   "bdb...adbbddbdbbdbbbbcccccbbbdbdbbbcccccbbbdbdbbbcccccbb"
				   "bddddadbbbdbbbbbbbbbbccbcbbbbbbbbbbccbcbbbbbbbbbbccbcbbb"
				   "bbdbdbabbddbbbbbbbbbbcbbcbbbbbbbbbbcbbcbbbbbbbbbbcbbcbbb"
				   "ddddddddddbbbbbbbbbbbcbbcbbbbbbbbbbcbbcbbbbbbbbbbcbbcbbb"
				   )
				 _string)
			       (list->cblock
				 '(
				   ;; width height num_colors chars_per_pixel
				   "    56     8        6            1"
				   ;; colors
				   ". c #ffffff"
				   "# c #ff9999"
				   "a c #ff0000"
				   "b c none"
				   "c c #663333"
				   "d c #000000"
				   ;; pixels
				   "bbbaa.bbbbbbbbbbbbbbbbbbbbbcbbbbbbbbbbbbbcbbbbbbbbbbbbbc"
				   "bb.b#bbbbbbbbbbbbbbbbbbbcbcbbbbbbbbbbbcbcbbbbbbbbbbbcbcb"
				   "bbbb#ab.bbbbbbbbbbbbbbbbbcbbbbbbbbbbbbbcbbbbbbbbbbbbbcbb"
				   "bdbbaaabddbdbdbbdbbbcbbbbcbbbbbbbbcbbbbcbbbdbdbbcbbbbcbb"
				   "bdb...adbbdbbbbbbbbbbcccccbbbbbbbbbcccccbbbbbbbbbcccccbb"
				   "bddddadbbbdbbbbbbbbbbccbcbbbbdbdbbbccbcbbbbbbbbbbccbcbbb"
				   "bbdbdba.bddbbbbbbbbbbbcbcbbbbbbbbbbbcbcbbbbbbbbbbbcbcbbb"
				   "ddddddddddbbbbbbbbbbbbccbbbbbbbbbbbbccbbbbbbbbbbbbccbbbb"
				   )
				 _string)
			       (list->cblock
				 '(
				   ;; width height num_colors chars_per_pixel
				   "    56     8        6            1"
				   ;; colors
				   ". c #ffffff"
				   "# c #ff9999"
				   "a c #ff0000"
				   "b c none"
				   "c c #663333"
				   "d c #000000"
				   ;; pixels
				   "bbbaa.bbbbbbbbbbbbbbbbbbbbbcbbbbbbbbbbbbbcbbbbbbbbbbbbbc"
				   "bb.b#bbbbbbbbbbbbbbbbbbbcbcbbbbbbbbbbbcbcbbbbbbbbbbbcbcb"
				   "bbbb#abbbbbbbbbbbbbbbbbbbcbbbbbbbbbbbbbcbbbbbbbbbbbbbcbb"
				   "bdbbaaa.ddbbbbbbbbbbcbbbbcbbbbbbbbcbbbbcbbbbbbbbcbbbbcbb"
				   "bdb...adbbddbdbbdbbbbcccccbbbdbdbbbcccccbbbdbdbbbcccccbb"
				   "bddddadbbbdbbbbbbbbbbccbcbbbbbbbbbbccbcbbbbbbbbbbccbcbbb"
				   "bbdbdba.bddbbbbbbbbbbcbbcbbbbbbbbbbcbbcbbbbbbbbbbcbbcbbb"
				   "ddddddddddbbbbbbbbbbbcbbcbbbbbbbbbbcbbcbbbbbbbbbbcbbcbbb"
				   )
				 _string)
			       )
			     (list)
			     (list
			       (list->cblock
				 '(
				   ;; width height num_colors chars_per_pixel
				   "   177    34       18            1"
				   ;; colors
				   ". c #ffffff"
				   "# c #ff9933"
				   "a c #ff0000"
				   "b c #f1f1f1"
				   "c c #cccccc"
				   "d c #cc9999"
				   "e c #cc9933"
				   "f c #cc6633"
				   "g c #cc0000"
				   "h c none"
				   "i c #993333"
				   "j c #990000"
				   "k c #663300"
				   "l c #33cc00"
				   "m c #111111"
				   "n c #009900"
				   "o c #0066cc"
				   "p c #003399"
				   ;; pixels
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhihhhihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhihhhihhhhhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhihhhhihiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhiihiiihhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhhiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhiiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhiiihhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiiihiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiiiihhhhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhhgggghhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiiihhhhhhihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhhhhhihhhh"
				   "hhhhhhhhhhhhhhhhhhhhhggjgggghhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhihhhhhhiiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhihhhhhhhihhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhhhhihiih"
				   "hhhhhhhhhhhhhhhhhhhhjgjgggggghhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhhhhiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhhhhiiiihhhhhhhhhhhhhhhhhhhhhhhhhhiihhhhhiiihh"
				   "hhhhhhhhhhhhhhhhhhhggjggjgggggbhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhhhihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhhiiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhhhiihhh"
				   "hhhhhhhhhhhhhhhhhhgjjgjjgjgggchhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiiiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihiihhhhh"
				   "hhhhhhhhhhhhhhhhhgjhhhggjggdbdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiiihhhhhh"
				   "hhhhhhhhhhhhhhhh.jhhhhhjggdbmddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhifffffhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffffffhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiffffhhhhh"
				   "hhhhhhhhhhhhhhh...hhhhhgcbcdddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhmmfiikkiffhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffkiififfhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffkiififfhhh"
				   "hhhhhhhhhhhhhhhh.hhhhhhcccbcdccddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffffffhhhhhhffffmmfkkkffmffhhhhhhhhhhhhffffffffhhhhhhffmmfffkikffmffhhhhhhhhhhhffffffhhhhhhfffmmmffkkffmffhh"
				   "hhhhhhhhhhhhhhhhhhhhhhhhcccbbcbbdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffffffffffffffffffffmmfffffffffhhhhhhhhhfffffffffffffffffffmmffffffffffhhhhhhhhfffffffffffffffffffmmffffffffaaa"
				   "hhhhhhhhhhhhhhhhjgghhhhhcbcccbcbhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhfffffffffffffffffffffmmffiiiffkhhhhhhhhhffffffffffffffffffffmmfffiifffffmhmhhhhffffffffffffffffffffmmfffiiffaaa"
				   "hhhhhhhhhhhhhhhhgggiihhjjbjcbccbhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhfffffffffffffffffffffffmmiiiiifkmhhhhhhhfffffffffffffffffffffffmiiiiffffkhhhmhmfffffffffffffffffffffffmiiififaaa"
				   "hhhhhhhhhhhhhhhcjgjiihjgjjjbjbhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhmhmffffffffffffffffffffffifmmhhhiihhmhmhmhmffffffffffffffffffffffifmiihhhffkhhhhhhffffffffffffffffffffffifmihhhifhh"
				   "hhhhhhhbhbhhlgncccciihjggggjggghhhmhhhhhhhhhhhhhhhhhhhhmhmhmhhhhhffffffifffffffffffffiifiimhhhhhhhhhhhhhhffffffifffffffffffffiifimmhhhhhhhhhhhhhffffffifffffffffffffiifimhhhhhhhh"
				   "hhhchocoboghlg##g#ghiijgggggggjjhggmmmhmhmhmhmhmhmhmhmhhhhhhhhhhhfffffffiiifffffffffffifhhmmmhhhhhhhhhhhhfffffffiiifffffffffffifhmmmmmmmmmmmmmmmfffffffiiifffffffffffifhhhhhhhhhh"
				   "hhhcccoobpohlgl##gghiijjgjgjggggbggghhhhhhhhhhhhhhhhhhhhhhhhmmmmmifffffffiiiiiiiifffffffhhmmmmmmmmmmmmmmmifffffffiiiiiiiifffffffhmmmmmmmmmmmmmmmifffffffiiiiiiiifffffffhhhhhhhhhh"
				   "hnn#cnnnbopppoop#opoiijjggjjjjggbgggmmmmmmmmmmmmmmmmmmmmmmmmmmmhhiifffffffiiiiiiiffffffihhhhhmmmmmmmmmmmmiifffffffiiiiiiiffffffihhhhhhhhhhhhhhhhiifffffffiiiiiiiffffffihhhhhhhhhh"
				   "hhencennggpgbooo#oooiijjjgggggjjgghhhmmmmmmmmmmmmmmmmmmmmmmmhhhhhhfffffffffiiiiiifffffihhhhhhhhhhhhhhhhhhhfffffffffiiiiiifffffihhhhhhhhhhhhhhhhhhfffffffffiiiiiifffffihhhhhhhhhhh"
				   "hinncnneggpggooo#opoiiijjjgggggggbhhhhiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhffffffhhhhhhhfffihhhhhhhhhhhhhhhhhhhhhhhffffffhhhhhhhfffihhhhhhhhhhhhhhhhhhhhhhffffffhhhhhhhfffihhhhhhhhhhhh"
				   "inencenngbpggoop#ooohiijjjjjggggggbhhhiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffffhhhhhhhfffihhhhhhhhhhhhhhhhhhhhhhhhhffffhhhhhhhfffihhhhhhhhhhhhhhhhhhhhhhhhffffhhhhhhhfffihhhhhhhhhhhh"
				   "i#nncn#nggpgbooo#poohiihjjjjjjggggbhhhiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffihhhhhhhffihhhhhhhhhhhhhhhhhhhhhhhhhhhffihhhhhhhffihhhhhhhhhhhhhhhhhhhhhhhhhhffihhhhhhhffihhhhhhhhhhhhh"
				   "hiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhfffhhhhhhfffihhhhhhhhhhhhhhhhhhhhhhhhhhhfffhhhhhhfffihhhhhhhhhhhhhhhhhhhhhhhhhhfffhhhhhhfffihhhhhhhhhhhhh"
				   "hkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffffhhhhhffiihhhhhhhhhhhhhhhhhhhhhhhhhhhffffhhhhhffiihhhhhhhhhhhhhhhhhhhhhhhhhhffffhhhhhffiihhhhhhhhhhhhh"
				   "hhhhhkkhhhhhhhhhhhhhhhhhhhkkjjjjjjbhhhiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiffhhhhhffihhhhhhhhhhhhhhhhhhhhhhhhhhhhhiffhhhhhffihhhhhhhhhhhhhhhhhhhhhhhhhhhhiffhhhhhffihhhhhhhhhhhhhh"
				   "hhhhhkkhhhhhhhhhhhhhhhhhhhkkbbbbbbmmhhiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiifmhhhffiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhiifmhhhffiihhhhhhhhhhhhhhhhhhhhhhhhhhhhiifmhhhffiihhhhhhhhhhhhhh"
				   "hhihhkkhhhhhhhhhhhhhhhhhhhkkhhmmmmmmhiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhifmhhhffihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhifmhhhffihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhifmhhhffihhhhhhhhhhhhhhh"
				   "hihhhkkhhhhhhhhhhhhhhhhhhhkkhhhmmmhhiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhmmhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhmmhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhmmhhhhhhhhhhhhhhhh"
				   "hhiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
				   )
				 _string)
			       (list->cblock
				 '(
				   ;; width height num_colors chars_per_pixel
				   "   177    34       19            1"
				   ;; colors
				   ". c #ffffff"
				   "# c #ff9933"
				   "a c #ff0000"
				   "b c #f1f1f1"
				   "c c #cccccc"
				   "d c #cc9999"
				   "e c #cc9933"
				   "f c #cc6633"
				   "g c #cc0000"
				   "h c none"
				   "i c #b2b2b2"
				   "j c #993333"
				   "k c #990000"
				   "l c #663300"
				   "m c #33cc00"
				   "n c #111111"
				   "o c #009900"
				   "p c #0066cc"
				   "q c #003399"
				   ;; pixels 
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjhhhjhhhhhhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjhhhhjhjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjhhhjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhjjhjjjhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhjjjhhhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjhjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhjjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjjhhhhhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhgggghhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhggkgggghhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhhhjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhhhjhhhh"
				   "hhhhhhhhhhhhhhhhhhhkgkgggggghhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhhjjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhhhhjhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhhjhjjh"
				   "hhhhhhhhhhhhhhhhhhggkggkgggggbhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhhjjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhjjjhh"
				   "hhhhhhhhhhhhhhhhhgkkgkkgkgggchhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjjjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjhhjjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjjjjjhhh"
				   "hhhhhhhhhhhhhhhhgkhhhggkggdbdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhhh"
				   "hhhhhhhhhhhhhhh.khhhhhkggdbnddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjfffffhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffffffhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjffffhhhhh"
				   "hhhhhhhhhhhhhh...hhhhhgcbcdddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhnnfjjlljffhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffljjfjffhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffljjfjffhhh"
				   "hhhhhhhhhhhhhhh.hhhhhhcccbcdccddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffffffhhhhhhffffnnflllffnffhhhhhhhhhhhhffffffffhhhhhhffnnfffljlffnffhhhhhhhhhhhffffffhhhhhhfffnnnffllffnffhh"
				   "hhhhhhhhhhhhhhhhkgghhhhcccbbcbbdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffffffffffffffffffffnnfffffffffhhhhhhhhhfffffffffffffffffffnnffffffffffhhhhhhhhfffffffffffffffffffnnffffffffaaa"
				   "hhhhhhhhhhhhhhhhggghhhhcbcccbcbhhhnhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhfffffffffffffffffffffnnffjjjfflhhhhhhhhhffffffffffffffffffffnnfffjjfffffnhnhnhnffffffffffffffffffffnnfffjjffaaa"
				   "hhhhhhhhhhhhhhhckgkjjhhkkkcbccbhhgghhhhhhhhhhhhhhhhhhhhhhhhhhhhhhfffffffffffffffffffffffnnjjjjjflnhnhnhnhffffffffffffffffffffffnnjjjjfffflhhhhhhffffffffffffffffffffffnnjjjfjfaaa"
				   "hhhhhhhbhbhhmgoccccjjhkgkkkkbiggbggghhhhhhhhhhhhhhhhhhhhhhhhhhhnhffffffffffffffffffffffjfnnhhhjjhhhhhhhhhffffffffffffffffffffffjfnjjhhhfflhhhhhhffffffffffffffffffffffjfnjhhhjfhh"
				   "hhhchpcpbpghmg##g#gjjhkggggkggggbgggnnnhnhnhnnhnhnhnhnhnhnhnhnhhhjfffffjfffffffffffffjjfjjnnnhhhhhhhhhhhhjfffffjfffffffffffffjjfjnnhhhhhhhhhhhhhjfffffjfffffffffffffjjfjnhhhhhhhh"
				   "hhhcccppggqgbgm##gg.jjkggggggkkkhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjffffffjjjffffffffffjjjfnnnnnnnnnnnnnnnnjffffffjjjffffffffffjjjfnnnnhhhhhhnnnnnjffffffjjjffffffffffjjjfhhhhhhhhh"
				   "hoo#coooggqggppq#pqpjjkkgkgkkkgbhhhhjjhhhhhhhhhhhhhhhhhhhhhhnnnnnjjfffffjjjjjjjjjfffffjjhhhhhnnnnnnnnnnnnjjfffffjjjjjjjjjfffffjjhhnnnnnnnnnnnnnnjjfffffjjjjjjjjjfffffjjhhhhhhhhhh"
				   "hoeoceoogbqggppp#pppjjkkggkkggggbhhjhjnnnnnnnnnnnnnnnnnnnnnnnnnnnjjffffffjjjjjjjfffffffhhhhhhhhhhhhhhhhhhjjffffffjjjjjjjfffffffhhhhhhnnnnnnnhhhhjjffffffjjjjjjjfffffffhhhhhhhhhhh"
				   "hooocooeggqghppp#pqpjjkkkggggggggbhhhjjnnnnnnnnnnnnnnnnnnnnnhhhhhjjffffffhhjjjjjjjfffffhhhhhhhhhhhhhhhhhhjjffffffhhjjjjjjjfffffhhhhhhhhhhhhhhhhhjjffffffhhjjjjjjjfffffhhhhhhhhhhh"
				   "hjeoceoohhhhhppq#pppjjjkkkgggggggbhhhhjjhhhhhhhhhhhhhhhhhhhhhhhhhhjjfffffhhhhhhhhjjjffffhhhhhhhhhhhhhhhhhhjjfffffhhhhhhhhjjjffffhhhhhhhhhhhhhhhhhjjfffffhhhhhhhhjjjffffhhhhhhhhhh"
				   "j#ooco#ohhhhhppp#qpphjjkkkkkggggggbhhhjjhhhhhhhhhhhhhhhhhhhhhhhhhhhjffffhhhhhhhhhhhhfffjhhhhhhhhhhhhhhhhhhhjffffhhhhhhhhhhhhfffjhhhhhhhhhhhhhhhhhhjffffhhhhhhhhhhhhfffjhhhhhhhhhh"
				   "jhhhhhhhhhhhhhhhhhhhhjjhkkkkkkggggbhhhjjhhhhhhhhhhhhhhhhhhhhhhhhhhhfffffhhhhhhhhhhhhhffjhhhhhhhhhhhhhhhhhhhfffffhhhhhhhhhhhhhffjhhhhhhhhhhhhhhhhhhfffffhhhhhhhhhhhhhffjhhhhhhhhhh"
				   "hjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhfffjhhhhhhhhhhhhhhffjhhhhhhhhhhhhhhhhhhhfffjhhhhhhhhhhhhhhffjhhhhhhhhhhhhhhhhhhfffjhhhhhhhhhhhhhhffjhhhhhhhhhh"
				   "hllllllllllllllllllllllllllllllllllllljjhhhhhhhhhhhhhhhhhhhhhhhhhhhfffjhhhhhhhhhhhhhfffnhhhhhhhhhhhhhhhhhhhfffjhhhhhhhhhhhhhfffnhhhhhhhhhhhhhhhhhhfffjhhhhhhhhhhhhhfffnhhhhhhhhhh"
				   "hhhhhllhhhhhhhhhhhhhhhhhhhllkkkkkkbhhhjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhffjhhhhhhhhhhhhnnfhhhhhhhhhhhhhhhhhhhhhhffjhhhhhhhhhhhhnnfhhhhhhhhhhhhhhhhhhhhhffjhhhhhhhhhhhhnnfhhhhhhhhhhhh"
				   "hhhhhllhhhhhhhhhhhhhhhhhhhllbbbbbbnnhhjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhffjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffjhhhhhhhhhhhhhhhhhhhhhhhhhhh"
				   "hhjhhllhhhhhhhhhhhhhhhhhhhllhhnnnnnnhjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhffjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffjjhhhhhhhhhhhhhhhhhhhhhhhhhh"
				   "hjhhhllhhhhhhhhhhhhhhhhhhhllhhhnnnhhjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffnhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffnhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffnhhhhhhhhhhhhhhhhhhhhhhhhhh"
				   "hhjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhnnhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhnnhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhnnhhhhhhhhhhhhhhhhhhhhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
				   )
				 _string)
			       (list->cblock
				 '(
				   ;; width height num_colors chars_per_pixel
				   "   177    34       18            1"
				   ;; colors 
				   ". c #ffffff"
				   "# c #ff9933"
				   "a c #ff0000"
				   "b c #f1f1f1"
				   "c c #cccccc"
				   "d c #cc9999"
				   "e c #cc9933"
				   "f c #cc6633"
				   "g c #cc0000"
				   "h c none"
				   "i c #993333"
				   "j c #990000"
				   "k c #663300"
				   "l c #33cc00"
				   "m c #111111"
				   "n c #009900"
				   "o c #0066cc"
				   "p c #003399"
				   ;; pixels
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhihhhhihiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhihhhihhhhhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhhiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhihhhihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhiihiiihhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiiihiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhiiihhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhiiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhiiiihhhhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhgggghhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhhhhhihhhh"
				   "hhhhhhhhhhhhhhhhhhhggjgggghhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhhhihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhhhhhhihhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhhhhihiih"
				   "hhhhhhhhhhhhhhhhhhjgjgggggghhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhhiiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhihhhhhhiiiihhhhhhhhhhhhhhhhhhhhhhhhhhiihhhhhiiihh"
				   "hhhhhhhhhhhhhhhhhggjggjgggggbhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhihhhiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhhhiiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhhhiihhh"
				   "hhhhhhhhhhhhhhhhgjjgjjgjgggchhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiiiiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhhihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihiihhhhh"
				   "hhhhhhhhhhhhhhhgjhhhggjggdbdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiiiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiiihhhhhh"
				   "hhhhhhhhhhhhhh.jhhhhhjggdbmddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhifffffhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffffffhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhiffffhhhhh"
				   "hhhhhhhhhhhhh...hhhhhgcbcdddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhmmfiikkiffhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffkiififfhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffkiififfhhh"
				   "hhhhhhhhhhhhhh.hjgghhcccbcdccddhhhmhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffffffhhhhhhffffmmfkkkffmffhhhhhhhhhhhhfffffffhhhhhhfffmmfffkikffmffhhhhhhhhhhhffffffhhhhhhfffmmmffkkffmffhh"
				   "hhhhhhhhhhhhhhhhggghhhcccbbcbbdhhgghhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffffffffffffffffffffmmfffffffffhhhhhhhhhfffffffffffffffffffmmffffffffffhhhmhmhhfffffffffffffffffffmmffffffffaaa"
				   "hhhhhhhhbpohlhhcjgjhhhcbcccbcbghbggghhhmhmhmhmhmhmhmhmhhhhhhhhhhhhfffffffffffffffffffffmmffiiiffkhhmmhmhmmffffffffffffffffffffmmfffiifffffmhhhhhmffffffffffffffffffffmmfffiiffaaa"
				   "hhhhhhhbbopppgncccciihhjjcbccgggbgggmmmhhhhhhhhhhhhhhhhmhhmhmhhhhfffffffffffffffffffffffmmiiiiifkmhhhhhhhffffffffffffffffffffffmmiiiiffffkhhhhhhffffffffffffffffffffffmmiiififaaa"
				   "hhhchocoggpgbg##g#giihjgjjjbgggghhhhhhhhhhhhhhhhhhhhhhhhhhhhhhmhmffffffffffffffffffffffifmmhhhiihhhhhhhhhfffffffffffffffffffffffmmiihhhffkhhhhhhfffffffffffffffffffffffmmihhhifhh"
				   "hhhcccooggpgggl##ggiihjggggjgggghhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffffffifffffffffffffiifiimhhhhhhhhhhhhhhffffffifffffffffffffiifiimhhhhhhhhhhhhhffffffifffffffffffffiifiimhhhhhhh"
				   "hnn#cnnngbpggoop#opoiijggggggjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhfffffffiiiffffffffffiiifmmmmhhhhhhhhhhhhfffffffiiiffffffffffiiifmmmmmmmmmmmmmmhfffffffiiiffffffffffiiifmmmhhhhhh"
				   "hnencennggpghooo#oooiijjgjgjjjgbhhhhiihhhhhhhhhhhhhhhhhhhhhhmmmmmfffffffiiiiiiiifffffiiihhmmmmmmmmmmmmmmmfffffffiiiiiiiifffffiiihhmmmmmmmmmmmmmmfffffffiiiiiiiifffffiiihhmmhhhhhh"
				   "hnnncnnehhhhhooo#opoiijjggjjggggbhhihimmmmmmmmmmmmmmmmmmmmmmmmmmmffffffffiiiiiiiifffffiihhhhhmmmmmmmmmmmmffffffffiiiiiiiifffffiihhhhhhhhhhhhhhhmffffffffiiiiiiiifffffiihhhhhhhhhh"
				   "hnencennhhhhhoop#oooiijjjggggggggbhhhiimmmmmmmmmmmmmmmmmmmmmhhhhhhfffffffihiiiiiiifffffihhhhhhhhhhhhhhhhhhfffffffihiiiiiiifffffihhhhhhhhhhhhhhhhhfffffffihiiiiiiifffffihhhhhhhhhh"
				   "hinncn#nhhhhhooo#pooiiijjjgggggggbhhhhiihhhhhhhhhhhhhhhhhhhhhhhhhhhfffffiihhhhhhhhhhfffiihhhhhhhhhhhhhhhhhhfffffiihhhhhhhhhhfffiihhhhhhhhhhhhhhhhhfffffiihhhhhhhhhhfffiihhhhhhhhh"
				   "ihhhhhhhhhhhhhhhhhhhhiijjjjjggggggbhhhiihhhhhhhhhhhhhhhhhhhhhhhhhhhffffiihhhhhhhhhhhhffffiihhhhhhhhhhhhhhhhffffiihhhhhhhhhhhhffffiihhhhhhhhhhhhhhhffffiihhhhhhhhhhhhffffiihhhhhhh"
				   "ihhhhhhhhhhhhhhhhhhhhiihjjjjjjggggbhhhiihhhhhhhhhhhhhhhhhhhhhhhhhhfffffihhhhhhhhhhhhhhfffffihhhhhhhhhhhhhhfffffihhhhhhhhhhhhhhfffffihhhhhhhhhhhhhfffffihhhhhhhhhhhhhhfffffihhhhhh"
				   "hiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiihhhhhhhhhhhhhhhhhhhhhhhhhffffiihhhhhhhhhhhhhhhhhfifmhhhhhhhhhhhhhffffiihhhhhhhhhhhhhhhhhfifmhhhhhhhhhhhhffffiihhhhhhhhhhhhhhhhhfifmhhhhhh"
				   "hkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkiihhhhhhhhhhhhhhhhhhhhhhhhhfffihhhhhhhhhhhhhhhhhhhhfmmhhhhhhhhhhhhhfffihhhhhhhhhhhhhhhhhhhhfmmhhhhhhhhhhhhfffihhhhhhhhhhhhhhhhhhhhfmmhhhhhh"
				   "hhhhhkkhhhhhhhhhhhhhhhhhhhkkjjjjjjbmmhiihhhhhhhhhhhhhhhhhhhhhhhhfffiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhfffiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhfffiihhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
				   "hhhhhkkhhhhhhhhhhhhhhhhhhhkkbbbbbbmmmhiihhhhhhhhhhhhhhhhhhhhhhhhffiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffiiihhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
				   "hhihhkkhhhhhhhhhhhhhhhhhhhkkhhhmmmmmhiiihhhhhhhhhhhhhhhhhhhhhhhhffhimhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffhimhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffhimhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
				   "hihhhkkhhhhhhhhhhhhhhhhhhhkkhhhhhhhhiiihhhhhhhhhhhhhhhhhhhhhhhhhmfhmmhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhmfhmmhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhmfhmmhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
				   "hhiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiihhhhhhhhhhhhhhhhhhhhhhhhhhmmhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhmmhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhmmhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
				   )
				 _string)
			       (list->cblock
				 '(
				   ;; width height num_colors chars_per_pixel
				   "   177    34       19            1"
				   ;; colors 
				   ". c #ffffff"
				   "# c #ff9933"
				   "a c #ff0000"
				   "b c #f1f1f1"
				   "c c #cccccc"
				   "d c #cc9999"
				   "e c #cc9933"
				   "f c #cc6633"
				   "g c #cc0000"
				   "h c none"
				   "i c #b2b2b2"
				   "j c #993333"
				   "k c #990000"
				   "l c #663300"
				   "m c #33cc00"
				   "n c #111111"
				   "o c #009900"
				   "p c #0066cc"
				   "q c #003399"
				   ;; pixels
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjhhhjhhhhhhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjhhhhjhjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjhhhjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhjjhjjjhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhjjjhhhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjhjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhjjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjjhhhhhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhgggghhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhggkgggghhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhhhjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhhhjhhhh"
				   "hhhhhhhhhhhhhhhhhhhkgkgggggghhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhhjjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhhhhjhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhhjhjjh"
				   "hhhhhhhhhhhhhhhhhhggkggkgggggbhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhhjjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhjjjhh"
				   "hhhhhhhhhhhhhhhhhgkkgkkgkgggchhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjjjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjhhjjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjjjjjhhh"
				   "hhhhhhhhhhhhhhhhgkhhhggkggdbdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjjhhhhhh"
				   "hhhhhhhhhhhhhhh.khhhhhkggdbnddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjfffffhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffffffhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjffffhhhhh"
				   "hhhhhhhhhhhhhh...hhhhhgcbcdddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhnnfjjlljffhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffljjfjffhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffljjfjffhhh"
				   "hhhhhhhhhhhhhhh.hhhhhhcccbcdccddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffffffhhhhhhffffnnflllffnffhhhhhhhhhhhhffffffffhhhhhhffnnfffljlffnffhhhhhhhhhhhffffffhhhhhhfffnnnffllffnffhh"
				   "hhhhhhhhhhhhhhhhkgghhhhcccbbcbbdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhffffffffffffffffffffnnfffffffffhhhhhhhhhfffffffffffffffffffnnffffffffffhhhhhhhhfffffffffffffffffffnnffffffffaaa"
				   "hhhhhhhhhhhhhhhhggghhhhcbcccbcbhhhnhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhfffffffffffffffffffffnnffjjjfflhhhhhhhhhffffffffffffffffffffnnfffjjfffffnhnhnhnffffffffffffffffffffnnfffjjffaaa"
				   "hhhhhhhhhhhhhhhckgkjjhhkkkcbccbhhgghhhhhhhhhhhhhhhhhhhhhhhhhhhhhhfffffffffffffffffffffffnnjjjjjflnhnhnhnhfffffffffffffffffffffffnjjjjfffflhhhhfffffffffffffffffffffffnnnjjjfjfaaa"
				   "hhhhhhhbhbhhmgoccccjjhkgkkkkbiggbggghhhhhhhhhhhhhhhhhhhhhhhhhhhnhffffffffffjjjfffffffffjfnnhhhjjhhhhhhhhhffffffffffjjjfffffffffjfnjjhhhfflhhhhffffffffffjjjfffffffffjffnnjhhhjfhh"
				   "hhhchpcpbpghmg##g#gjjhkggggkggggbgggnnnhnhnhnnhnhnhnhnhnhnhnhnhhhjfffffffffjjjfffffffjjfjjnnnhhhhhhhhhhhhjfffffffffjjjfffffffjjfjnnhhhhhhhhhhhjfffffffffjjjfffffffjjfjjfnhhhhhhhh"
				   "hhhcccppggqgbgm##gghjjkggggggkkkhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhfffffffffffjjfffffffjjjfnnnnnnnnnnnnnnnnfffffffffffjjfffffffjjjfnnnnhhhhhhnnnfffffffffffjjfffffffjjjfjfhhhhhhhhh"
				   "hoo#coooggqggppq#pqpjjkkgkgkkkgbhhhhjjhhhhhhhhhhhhhhhhhhhhhhnnnnnjffffffffffjfjjjfffffjjhhhhhnnnnnnnnnnnnjffffffffffjfjjjfffffjjhnnnnnnnnnnnnnjffffffffffjfjjjfffffjjhhhhhhhhhhhh"
				   "hoeoceoogbqggppp#pppjjkkggkkggggbhhjhjnnnnnnnnnnnnnnnnnnnnnnnnnnnjjffffffffjjfjjfffffffjhhhhhhhhhhhhhhhhhjjffffffffjjfjjfffffffjhhhhhnnnnnnnhhjjffffffffjjfjjfffffffjhhhhhhhhhhhh"
				   "hooocooeggqghppp#pqpjjkkkggggggggbhhhjjnnnnnnnnnnnnnnnnnnnnnhhhhhhjjffffffjjffjjjjffffffhhhhhhhhhhhhhhhhhhjjffffffjjffjjjjffffffhhhhhhhhhhhhhhhjjffffffjjffjjjjffffffhhhhhhhhhhhh"
				   "hjeoceoohhhhhppq#pppjjjkkkgggggggbhhhhjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhfffffjhhhhhhjjjffffhhhhhhhhhhhhhhhhhhhhhfffffjhhhhhhjjjffffhhhhhhhhhhhhhhhhhhfffffjhhhhhhjjjffffhhhhhhhhhhhh"
				   "j#ooco#ohhhhhppp#qpphjjkkkkkggggggbhhhjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhffffjjhhhhhhhhhhfffjhhhhhhhhhhhhhhhhhhhhffffjjhhhhhhhhhhfffjhhhhhhhhhhhhhhhhhffffjjhhhhhhhhhhfffjhhhhhhhhhhhh"
				   "jhhhhhhhhhhhhhhhhhhhhjjhkkkkkkggggbhhhjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhfffjhhhhhhhhhhhhhfffhhhhhhhhhhhhhhhhhhhhfffjhhhhhhhhhhhhhfffhhhhhhhhhhhhhhhhhfffjhhhhhhhhhhhhhfffhhhhhhhhhhhh"
				   "hjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhfffjjhhhhhhhhhhhhhffffhhhhhhhhhhhhhhhhhhfffjjhhhhhhhhhhhhhffffhhhhhhhhhhhhhhhfffjjhhhhhhhhhhhhhffffhhhhhhhhhhh"
				   "hllllllllllllllllllllllllllllllllllllljjhhhhhhhhhhhhhhhhhhhhhhhhhhhffjjhhhhhhhhhhhhhhhfffhhhhhhhhhhhhhhhhhhffjjhhhhhhhhhhhhhhhfffhhhhhhhhhhhhhhhffjjhhhhhhhhhhhhhhhfffhhhhhhhhhhh"
				   "hhhhhllhhhhhhhhhhhhhhhhhhhllkkkkkkbhhhjjhhhhhhhhhhhhhhhhhhhhhhhhhhhffjjhhhhhhhhhhhhhhhjffhhhhhhhhhhhhhhhhhhffjjhhhhhhhhhhhhhhhjffhhhhhhhhhhhhhhhffjjhhhhhhhhhhhhhhhjffhhhhhhhhhhh"
				   "hhhhhllhhhhhhhhhhhhhhhhhhhllbbbbbbnnhhjjhhhhhhhhhhhhhhhhhhhhhhhhhhhnfjnhhhhhhhhhhhhhhhjffhhhhhhhhhhhhhhhhhhnfjnhhhhhhhhhhhhhhhjffhhhhhhhhhhhhhhhnfjnhhhhhhhhhhhhhhhjffhhhhhhhhhhh"
				   "hhjhhllhhhhhhhhhhhhhhhhhhhllhhnnnnnnhjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhnnhhhhhhhhhhhhhhhhjjffhhhhhhhhhhhhhhhhhhnnhhhhhhhhhhhhhhhhjjffhhhhhhhhhhhhhhhnnhhhhhhhhhhhhhhhhjjffhhhhhhhhhhh"
				   "hjhhhllhhhhhhhhhhhhhhhhhhhllhhhnnnhhjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhnnfnhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhnnfnhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhnnfnhhhhhhhhhhh"
				   "hhjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhnnhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhnnhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhnnhhhhhhhhhhh"
				   "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
				   )
				 _string))
			     )
			   size))
	(sizes '((56 8)
		 (0 0)
		 (177 34))))
    (let ((x (car (list-ref sizes size)))
	  (y (cadr (list-ref sizes size))))
      (map (lambda (xpm-data)
	     (let-values (((pic mask) (create-xpm xpm-data)))
			 (list
			   (make-pic x y xpm-data pic)
			   (make-pic x y xpm-data mask))))
	   sleighs))))

(define (create-santa-pic display rootWindow size)
  (let ((santa (list-ref (list
			   (make-pic 8 8 (list->cblock
					   (list #x20 #x20 #x60 #x30 #x30 #x00 #x20 #x00)
					   _byte)
				     #f)
			   (make-pic 16 16 (list->cblock
					     (list
					       #x00 #x0c #x00 #x0c #x00 #x0c #x00 #x0e #x00 
					       #x0e #x00 #x3f #x00 #x0f #x00 #x0f #x00 #x0f 
					       #x00 #x0e #x00 #x00 #x00 #x08 #x00 #x08 #x00 
					       #x18 #x00 #x00 #x00 #x00)
					     _byte)
				     #f)
			   (make-pic 32 32 (list->cblock
					     (list 
					       #x00 #x00 #x20 #x00 #x00 #x00 #x70 #x00 #x00 
					       #x00 #x70 #x00 #x00 #x00 #x70 #x00 #x00 #x00 
					       #xf0 #x00 #x00 #x00 #xf0 #x00 #x00 #x00 #xf8 
					       #x00 #x00 #x00 #xf8 #x00 #x00 #x00 #xfc #x08 
					       #x00 #x00 #xfe #x0f #x00 #x00 #xff #x0f #x00 
					       #x00 #xff #x00 #x00 #x00 #xff #x00 #x00 #x80 
					       #xff #x00 #x00 #x80 #xff #x00 #x00 #x80 #xff 
					       #x00 #x00 #x80 #xff #x00 #x00 #x00 #xff #x00 
					       #x00 #x00 #xfe #x00 #x00 #x00 #x7c #x00 #x00 
					       #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
					       #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x60 
					       #x00 #x00 #x00 #x60 #x00 #x00 #x00 #xe0 #x00 
					       #x00 #x00 #xe0 #x01 #x00 #x00 #x00 #x00 #x00 
					       #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
					       #x00 #x00)
					     _byte)
				     #f))
			 size)))
    (set-pic-pixmap! santa (create-pixmap display rootWindow santa))
    santa))

(define (create-fur display rootWindow size)
  (let ((fur (list-ref (list
			 (make-pic 8 8
				   (list->cblock
				     (list #x00 #x20 #x00 #x00 #x00 #x00 #x00 #x00)
				     _byte)
				   #f)
			 (make-pic 16 16
				   (list->cblock
				     (list
				       #x00 #x00 #x00 #x00 #x00 #x0f #x00 #x00 #x00 
				       #x00 #x00 #x40 #x80 #x00 #x00 #x0f #x00 #x00 
				       #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				       #x00 #x00 #x00 #x00 #x00)
				     _byte)
				   #f)
			 (make-pic 32 32
				   (list->cblock
				     (list 
				       #x00 #x00 #x00 #x00 #x00 #x00 #x80 #x00 #x00 
				       #x00 #x8c #x00 #x00 #x00 #x8c #x00 #x00 #x00 
				       #x70 #x00 #x00 #x00 #x80 #x01 #x00 #x00 #x80 
				       #x01 #x00 #x00 #x80 #x01 #x00 #x00 #x00 #x0c 
				       #x00 #x00 #x00 #x0c #x00 #x00 #x00 #x0c #x00 
				       #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80 
				       #xc1 #x00 #x00 #x80 #xff #x00 #x00 #x00 #x3c 
				       #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				       #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				       #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				       #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				       #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				       #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				       #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
				       #x00 #x00)
				     _byte)
				   #f))
		       size)))
    (set-pic-pixmap! fur (create-pixmap display rootWindow fur))
    fur))

(define (create-flakes display rootWindow)

  ;; inline .xbm files
  (define snow-00 (make-pic 3 3 (list->cblock (list #x05 #x02 #x05) _byte) #f))
  (define snow-01 (make-pic 8 8 (list->cblock
				    (list #x22 #x6b #x14 #x2a #x14 #x6b #x22 #x00) _byte) #f))
  (define snow-02 (make-pic 8 8 (list->cblock
				    (list #x14 #x08 #x49 #x36 #x49 #x08 #x14 #x00) _byte) #f))
  (define snow-03 (make-pic 8 8 (list->cblock
				    (list #x14 #x08 #x49 #x36 #x49 #x08 #x14 #x00) _byte) #f))
  (define snow-04 (make-pic 8 8 (list->cblock
				    (list #x22 #x6b #x14 #x2a #x14 #x6b #x22 #x00) _byte) #f))
  (define snow-05 (make-pic 8 8 (list->cblock
				    (list #x14 #x08 #x49 #x36 #x49 #x08 #x14 #x00) _byte) #f))
  (define snow-06 (make-pic 3 3 (list->cblock
				    (list #x05 #x02 #x05) _byte) #f))

  (let ((snow-pixs (list snow-00 snow-01 snow-02 snow-03 snow-04 snow-05 snow-06)))
    (for-each (lambda (flake)
		(set-pic-pixmap! flake (create-pixmap display rootWindow flake)))
	      snow-pixs)
    snow-pixs))

(define (create-gc display rootWindow gc xgcv color pic)
  (let ((new-gc (XCreateGC display rootWindow 0 xgcv)))
    (XCopyGC display gc 0 new-gc)
    (XSetStipple display new-gc (pic-pixmap pic))
    (XSetForeground display new-gc color)
    (XSetFillStyle display new-gc 'FillStippled)
    new-gc))

(define-struct object (x y dx dy active visible insnow sprite) #:mutable)

(define (object-gc object)
  (sprite-gc (object-sprite object)))

(define (object-width object)
  (sprite-width (object-sprite object)))

(define (object-height object)
  (sprite-height (object-sprite object)))

(define (init-snow-flake flake max-width max-height max-y-step wind)
  (set-object-dy! flake (add1 (random (add1 max-y-step))))
  (set-object-dx! flake (let ((x (random (real->int (add1 (/ (object-dy flake) 4))))))
			  (if (= 0 (random 2))
			    (- x)
			    x)))
  (case wind
    ;; blowing left
    ((SlowLeft HardLeft) (begin
		  (set-object-x! flake (random (real->int (/ max-width 3))))
		  (set-object-y! flake (random max-height))))
    ;; blowing right
    ((SlowRight HardRight) (begin
		  (set-object-x! flake (- max-width 
					  (random (real->int (/ max-width 3)))))
		  (set-object-y! flake (random max-height))))
    (else
      (begin
	(set-object-x! flake (random (- max-width (object-width flake))))
	(set-object-y! flake (random (real->int (/ max-height 10)))))))
  (set-object-x! flake (random (- max-width (object-width flake))))
  flake)

(define (create-snow-flake max-width max-height max-y-step wind sprite)
  (let ((s (make-object 0 0 0 0 #t #t #f sprite)))
    (init-snow-flake s max-width max-height max-y-step wind)))

(define-struct (santa object) (sleigh) #:mutable)

(define (infinite-list l)
  (let ((mlist (list->mlist l)))
    (let loop ((l* mlist))
      (cond ((null? (mcdr l*))
	     (begin
	       (set-mcdr! l* mlist)
	       mlist))
	    (else (loop (mcdr l*)))))))

(define (create-santa max-height speed santa-size default-gc screen display root-window)
  (define (alloc-color name default)
    (AllocNamedColor display screen name default))
  (define black (BlackPixel display screen))
  (define red-color (alloc-color "red" black))
  (define sleigh-color (alloc-color "chartreuse" black))
  (let* 
    ((sleigh-pics (create-sleigh2 display root-window santa-size))
     (gc (let ((gc (XCreateGC display root-window 0 (make-dummy-XGCValues))))
	   (XSetForeground display gc black)
	   (XSetFillStyle display gc 'FillStippled)
	   gc))
     (sleigh-sprites (infinite-list
		       (map (lambda (pic&masked)
			      (let ((pic (car pic&masked))
				    (mask (cadr pic&masked)))
				(make-masked-sprite pic gc mask)))
			    sleigh-pics)))

     (santa-pic (create-santa-pic display root-window santa-size))
     (santa-gc (create-gc display root-window default-gc
			  (make-dummy-XGCValues) red-color santa-pic))
     (santa-sprite (make-sprite santa-pic santa-gc)))
    (let* ((x (- (sprite-width santa-sprite)))
	   (x 40)
	   (y (+ 40 (random (real->int (/ max-height 3)))))
	   (y 50)
	   (dont-care #f))
      (make-santa x y speed 0.1 dont-care dont-care dont-care santa-sprite
		  sleigh-sprites))))

(define (choose-random lst)
  (list-ref lst (random (sub1 (length lst)))))

(define (make-list maker num)
  (let loop ((things null)
	     (n num))
    (if (> n 0)
      (loop (cons (maker) things) (sub1 n))
      things)))

(define (CreateRegion)
  (let ((x (XCreateRegion)))
    (register-finalizer x (lambda (x) (XDestroyRegion x)))
    x))

(define (xtest2 display win)
  (XQueryTree display win)
  #;
  (begin
    (XGrabServer display)
    (printf "Begin!\n")
    (sleep 4)
    (printf "End!\n")
    (XUngrabServer display)))

;; This freezes X.. why!?!?!
(define (xtest display win)
  (XGrabServer display)
  ; (CreateRegion)
  (let-values (((lst real) (XQueryTree display win)))
    (XFree real))
  (XUngrabServer display))

(define debug? #f)

(define-syntax debug
  (syntax-rules ()
    ((_ x ...) (when debug?
		 (printf x ...)))))

(define (calculate-window-tops display root-window snow-catch 
			       max-width max-height max-y-step 
			       max-snow-flake-height max-win-snow-depth)
  (let ((windows (CreateRegion))
	(snow-allowed (CreateRegion))
	(children (XQueryTree display root-window)))
    (call-with-values 
      (lambda ()
        (let/cc ret
                (let loop ((children children))
                  (debug "Children = ~a\n" (length children))
                  (cond
                    ((null? children) (begin
                                        ; (printf "f1\n")
                                        (ret #f windows snow-allowed)))
                    ((not (= 0 (XEventsQueued display 'QueuedAlready))) 
                     (begin
                       (ret #t windows snow-allowed)))
                    (else
                      (begin
                        (let* [(current-window (car children))
                               (attrs (XGetWindowAttributes display current-window))]
                          (when (XWindowAttributes-save-under attrs)
                            (loop (cdr children)))
                          (when (eq? 'IsViewable (XWindowAttributes-map-state attrs))
                            (debug "IsViewable\n")
                            (let-values (((parent x y width height border-width depth)
                                          (XGetGeometry display current-window)))
                              (when (or (> x max-width)
                                        (> y max-height)
                                        (<= y 0))
                                (loop (cdr children)))
                              (let ((x (- x border-width))
                                    (y (- y border-width))
                                    (width (- width (* 2 border-width)))
                                    (height (- height (* 2 border-width))))
                                (debug "x1\n")
                                (when (< (+ x width) 0)
                                  (loop (cdr children)))
                                (when (> (+ y height) max-height)
                                  (set! height (- max-height y)))
                                (when (< y 0)
                                  (set! height (+ height y))
                                  (set! y 0))
                                (debug "x2\n")
                                (when (< x 0)
                                  (set! width (+ width x (- border-width)))
                                  (set! x 0))
                                (debug "x2 1\n")
                                (let* ((rect 
                                         (make-XRectangle x y
                                                          (+ width (* 2 border-width))
                                                          (+ height (* 2 border-width))))
                                       (catch-rect 
                                         (make-XRectangle x y
                                                          (XRectangle-width rect)
                                                          (+ max-y-step max-snow-flake-height)))
                                       (subr (let ((s (CreateRegion)))
                                               (XUnionRectWithRegion rect s s)
                                               s)))
                                  (XUnionRectWithRegion rect windows windows)
                                  (XSubtractRegion snow-catch subr snow-catch)
                                  (XUnionRectWithRegion catch-rect snow-catch snow-catch)
                                  (set! y (- (XRectangle-y catch-rect) max-win-snow-depth))
                                  (let ((allow 
                                          (make-XRectangle x y
                                                           (+ (XRectangle-height catch-rect)
                                                              max-win-snow-depth)
                                                           (XRectangle-width catch-rect))))
                                    (debug "x4\n")
                                    (when (< y 0)
                                      (set-XRectangle-height! allow (+ y (XRectangle-height catch-rect)))
                                      (set-XRectangle-y! allow 0))
                                    (debug "tfff\n")
                                    (XUnionRectWithRegion allow snow-allowed snow-allowed))))
                              (debug "Loop!\n"))))
                        (loop (cdr children))))))))
      (lambda z 
        (begin
          ; (printf "Ungrab server = ~a\n" (XUngrabServer display))
          (values (list-ref z 0) (list-ref z 1) (list-ref z 2)))))))

(define (handle-events display rootWindow snow-catch max-height 
		       max-snow-flake-height max-win-snow-depth max-y-step)
  (let loop ((pending (XPending display))
	     (ret #f))
    (if (not pending)
      ret
      (begin
	(let ((event (XNextEvent* display)))
	  (case (XEvent-type event)
	    #;
	    ((Expose) (begin
			(let* ((add-rect
				 (let ((x (- (XExposeEvent-x event) 
					     max-snow-flake-height))
				       (y (- (XExposeEvent-y event) 
					     max-win-snow-depth))
				       (width (+ (XExposeEvent-width event)
						 (* 2 max-snow-flake-height)))
				       (height (+ (XExposeEvent-height event)
						  max-win-snow-depth)))
				   (printf "add rect x ~a y ~a width ~a height ~a\n"
					   x y width height)
				   (let-values (((rect-x rect-y rect-width rect-height)
						 (cond
						   ((< y 0)
						    (values x 0 width (+ y height)))
						   ((> (+ y height) max-height)
						    (values x y width (- max-height y)))
						   (else (values x y width height)))))
				     (make-XRectangle rect-x rect-y
						      rect-width rect-height))))
			       (sub-rect
				 (let ((x (XRectangle-x add-rect))
				       (y (- (XRectangle-y add-rect)
					     max-y-step
					     max-snow-flake-height))
				       (width (XRectangle-width add-rect))
				       (height (+ max-win-snow-depth
						  (* 2 max-y-step)
						  (* 2 max-snow-flake-height))))
				   (let-values (((rect-x rect-y rect-width rect-height)
						 (cond
						   ((< y 0)
						    (values x 0 width (+ y height)))
						   ((> (+ y height) max-height)
						    (values x y width (- max-height y)))
						   (else (values x y width height)))))
				     (printf "y ~a height ~a max-height ~a rect-height ~a\n" y height max-height rect-height)
				     (make-XRectangle rect-x rect-y
						      rect-width rect-height))))
			       (sub-region (CreateRegion)))
			  (for-each (lambda (rect)
				      (XUnionRectWithRegion rect sub-region sub-region))
				    (list add-rect sub-rect))
			  (printf "clear ~a ~a ~a ~a\n" 
				  (XRectangle-x sub-rect)
				  (XRectangle-y sub-rect)
				  (XRectangle-width sub-rect)
				  (XRectangle-height sub-rect))
			  (XClearArea display rootWindow 
				      (XRectangle-x sub-rect)
				      (XRectangle-y sub-rect)
				      (XRectangle-width sub-rect)
				      (XRectangle-height sub-rect)
				      #f)
			  (XSubtractRegion snow-catch sub-region snow-catch))
			(loop (XPending display) #t)))
	    ((MapNotify UnmapNotifiy ConfigureNotify) 
	     (loop (XPending display) #t))
	    (else loop (XPending display) ret)))))))

(define (handle-events1 display rootWindow snow-catch max-height 
		       max-snow-flake-height max-win-snow-depth max-y-step)
  ; (printf "Handle xevents\n")
  ; (let ((event (make-XEvent 'LASTEvent 0 0)))
  (let ((event (make-dummy-XEvent)))
    (let loop ((pending (XPending display))
	       (ret #f))
      ; (printf "Events pending\n")
      (if pending
	(begin
	  (XNextEvent display event)
	  ; (printf "Got event ~a\n" (XEvent-type event))
	  (case (XEvent-type event)
	    ((Expose) (begin
			(let ((add (make-XRectangle (- (XExposeEvent-x event)
						       max-snow-flake-height)
						    (- (XExposeEvent-y event)
						       max-win-snow-depth)
						    (+ (XExposeEvent-width event)
						       (* 2 max-snow-flake-height))
						    (+ (XExposeEvent-height event)
						       max-win-snow-depth))))
			  (when (< (XRectangle-y add) 0)
			    (set-XRectangle-height! add (+ (XRectangle-height add)
							   (XRectangle-y add)))
			    (set-XRectangle-y! add 0))
			  (when (> (+ (XRectangle-y add) (XRectangle-height add))
				   max-height)
			    (set-XRectangle-height! add (- max-height (XRectangle-y add))))
			  (let ((subr (let ((s (CreateRegion)))
					(XUnionRectWithRegion add s s)
					s)))
			    (set-XRectangle-y! add (- (XRectangle-y add)
						      max-y-step max-snow-flake-height))
			    (set-XRectangle-height! add (+ max-win-snow-depth
							   (* 2 max-y-step)
							   (* 2 max-snow-flake-height)))
			    (when (< (XRectangle-y add) 0)
			      (set-XRectangle-height! add (+ (XRectangle-height add)
							     (XRectangle-y add)))
			      (set-XRectangle-y! add 0))
			    (when (> (+ (XRectangle-y add) (XRectangle-height add))
				     max-height)
			      (set-XRectangle-height! add (- max-height (XRectangle-y add))))
			    (XUnionRectWithRegion add subr subr)
			    (XClearArea display rootWindow 
					(XRectangle-x add)
					(XRectangle-y add)
					(XRectangle-width add)
					(XRectangle-height add)
					#f)
			    (XSubtractRegion snow-catch subr snow-catch)))
			(loop (XPending display) #t)))
	    ((MapNotify UnmapNotifiy ConfigureNotify) 
	     (loop (XPending display) #t))
	    (else loop (XPending display) ret)))
	(begin
	  ret)))))

(define (test-stuff)
  (XOpenDisplay #f)
  (printf "f1\n")
  (XUnionRectWithRegion (make-XRectangle 28 60 1 0) (CreateRegion))
  (printf "f2\n"))

(define (erase-object obj display rootWindow)
  (let ((x (object-x obj))
	(y (object-y obj))
	(width (object-width obj))
	(height (object-height obj)))
    (XClearArea display rootWindow x y width height #f)))

(define (erase-snow-flake flake display rootWindow)
  (erase-object flake display rootWindow))

#;
(define (erase-snow-flake flake display rootWindow)
  (XClearArea display rootWindow 
	      (object-x flake) (object-y flake)
	      (object-width flake) (object-height flake) #f))

(define (draw-object obj display rootWindow)
  (let ((x (object-x obj))
	(y (object-y obj))
	(gc (object-gc obj))
	(width (object-width obj))
	(height (object-height obj)))
    (XSetTSOrigin display gc x y)
    (XFillRectangle display rootWindow gc x y width height)))

(define (draw-snow-flake flake display rootWindow)
  (draw-object flake display rootWindow))

(define (draw-santa santa display root-window)
  (let ((m (make-object (object-x santa)
			(object-y santa)
			0 0
			#f #f #f
			(mcar (santa-sleigh santa)))))
    (let ((x (object-x m))
	  (y (object-y m))
	  (width (object-width m))
	  (height (object-height m))
	  (gc (object-gc m))
	  (pic (sprite-pic (object-sprite m)))
	  (mask (masked-sprite-mask (object-sprite m))))
      (XSetClipMask display gc (pic-pixmap mask))
      (XSetClipOrigin display gc x y)
      (XCopyArea display (pic-pixmap pic) root-window gc 0 0 width height x y)))
  )

(define (update-santa santa wind display window)
  (define screen-size (let [(attr (XGetWindowAttributes display window))]
                        (XWindowAttributes-width attr)))
  (let ((m (make-object (object-x santa)
                        (object-y santa)
                        0 0
                        #f #f #f
                        (mcar (santa-sleigh santa)))))
    (erase-object m display window)
    (set-santa-sleigh! santa (mcdr (santa-sleigh santa)))
    (let ((y (real->int (+ (- (/ (random 20) 5) 2)
                           (object-y santa)))))
      (set-object-y! santa (cond
                             ((< y 20) 20)
                             ((> y 100) 100)
                             (else y))))
    (let ((dx (case wind
                ;; pushed backwards
                ((HardLeft) -3.5)
                ;; struggling to move
                ((SlowLeft) 2.5)
                ;; normal walking
                ((NoWind) 4)
                ;; speed up
                ((SlowRight) 5.5)
                ;; lets go!
                ((HardRight) 8.0))))
      (let ((nx (real->int (+ dx (object-x santa)))))
        (set-object-x! santa (cond
                               ((> nx screen-size) (- (object-width santa)))
                               ((< nx (- (object-width santa))) screen-size)
                               (else nx)))))

    (draw-santa santa display window)))

(define (range low mid high)
  (min high (max mid low)))

;; this needs to be cleaned up
(define (update-snow-flake flake display rootWindow max-width max-height 
			   max-y-step wind windows snow-catch snow-allowed)
  (define whirl-factor 4)
  (define max-x 4)
  (let ((no-erase #f)
	(old-x (object-x flake))
	(old-y (object-y flake)))
    (when (not (object-active flake))
      (init-snow-flake flake max-width max-height max-y-step wind)
      (set-object-insnow! flake #t))

    ;; update horizontal velocity
    (set-object-dx! flake
		    (case wind
		      ((SlowLeft SlowRight HardLeft HardRight)
		       (range -50
			      (* (let ((tmp (abs (object-dx flake))))
				   (case wind
				     ((SlowLeft SlowRight)
				      (+ tmp 
					 (random (add1 whirl-factor))
					 (- (/ (random whirl-factor) 2))))
				     ((HardLeft HardRight) (+ tmp (random 20)))))
				 (case wind
				   ((SlowLeft HardLeft) -1)
				   ((SlowRight HardRight) 1)))
			      50))
		      ((NoWind) (let ((nx (range (- max-x) 
						 (+ (object-dx flake)
						    (random (add1 whirl-factor))
						    (- (/ whirl-factor 2)))
						 max-x)))
				  (if no-erase
				    (/ nx 2)
				    nx)))
		      (else (printf "Wind = ~a\n" wind))))
    (set-object-x! flake (real->int (+ (object-x flake)
				       (object-dx flake))))
    (set-object-y! flake (real->int (+ (object-y flake)
				       (object-dy flake))))
    (set-object-active! flake (< (object-y flake) max-height))
    (when (object-active flake)
      (set-object-active! flake (and (> (object-x flake) 0)
				     (< (object-x flake) max-width))))
    (set-object-visible! flake (not (eq? 'RectangleIn
					 (XRectInRegion windows
							(object-x flake)
							(object-y flake)
							(object-width flake)
							(object-height flake)))))
    (when (object-visible flake)
      (let ((touchdown (XPointInRegion snow-catch 
				       (real->int
					 (+ (object-x flake)
					    (/ (object-width flake) 2)))
				       (+ (object-y flake)
					  (object-height flake)))))
	(when (and touchdown (object-visible flake))
	  (set! no-erase #t)
	  (when (eq? 'RectanglePart (XRectInRegion snow-catch 
						   (object-x flake)
						   (object-y flake)
						   (object-width flake)
						   (object-height flake)))
	    (let ((rect (make-XRectangle (object-x flake)
					 (+ (object-y flake)
					    (object-height flake)
					    (- 2))
					 (object-width flake)
					 2)))
	      (when (eq? 'RectangleIn (XRectInRegion snow-allowed
						     (XRectangle-x rect)
						     (XRectangle-y rect)
						     (XRectangle-width rect)
						     (XRectangle-height rect)))
		(XUnionRectWithRegion rect snow-catch snow-catch)))))))
    (when (not (object-insnow flake))
      (let ((new-x (object-x flake))
	    (new-y (object-y flake)))
	(set-object-x! flake old-x)
	(set-object-y! flake old-y)
	(erase-snow-flake flake display rootWindow)
	(set-object-x! flake new-x)
	(set-object-y! flake new-y)))
    (set-object-insnow! flake no-erase)
    (when (and (object-active flake) (object-visible flake))
      (draw-snow-flake flake display rootWindow))))

(define (draw-tree tree display rootWindow)
  (draw-object tree display rootWindow))

(define (winning-chance probability)
  (>= probability (random 1000)))

(define (update-wind wind)
  (case wind
    ((SlowLeft) (cond
		  ((winning-chance 10) 'NoWind)
		  ((winning-chance 10) 'HardLeft)
		  (else wind)))
    ((SlowRight) (cond
		   ((winning-chance 10) 'NoWind)
		   ((winning-chance 10) 'HardRight)
		   (else wind)))
    ((HardLeft) (if (winning-chance 20)
		  'SlowLeft
		  wind))
    ((HardRight) (if (winning-chance 20)
		   'SlowRight
		   wind))
    ((NoWind) (cond
		((winning-chance 1) 'SlowLeft)
		((winning-chance 1) 'SlowRight)
		(else 'NoWind)))
    (else wind)))

(define (random-range min max)
  (+ (random (- max min)) min))

(provide xsnow)
(define (xsnow snowflakes)
  (define display (XOpenDisplay #f))
  (define screen (DefaultScreen display))
  (define rootWindow (RootWindow display screen))
  (define black (BlackPixel display screen))
  (define white (WhitePixel display screen))
  (define display-w (DisplayWidth display screen))
  (define display-h (DisplayHeight display screen))
  (define center-x (/ display-w 2))
  (define center-y (/ display-h 2))
  (define max-snow-flakes snowflakes)
  (define max-win-snow-depth 15)
  (define santa-size 2)
  (define max-y-step 8)
  (define max-snow-flake-height 0)
  (define (alloc-color name default)
    (AllocNamedColor display screen name default))
  (define redColor (alloc-color "red" black))
  (define whiteColor (alloc-color "white" white))
  (define greenColor (alloc-color "green" black))
  (define blackColor (alloc-color "black" black))
  (define snowColor (alloc-color "snow" white))
  (define treeColor (alloc-color "chartreuse" black))
  (define sleighColor (alloc-color "chartreuse" black))
  (define backgroundColor (alloc-color "none" white))
  (define xgcv (make-dummy-XGCValues))

  ;; dont care about errors
  (XSetErrorHandler (lambda (display error)
                      (printf "Ignoring error\n")
                      0))
  (when (> Max-Screen-Snow-Depth (- display-h SNOW-FREE))
    (set! Max-Screen-Snow-Depth (- display-h SNOW-FREE)))
  (XClearArea display rootWindow 0 0 display-w display-h #f) 
  (XSelectInput display rootWindow '(ExposureMask SubstructureNotifyMask))
  #;
  (let* ((snow-catch (let ((s (CreateRegion))
                           (rect (make-XRectangle 0 display-h 
                                                  (sub1 display-w) 
                                                  (+ max-y-step
                                                     max-snow-flake-height))))
                       (XUnionRectWithRegion rect s s)
                       s)))
    (let ((event (make-dummy-XEvent)))
      (let loop ((pending (XPending display))
                 (ret #f))
        (printf "Pending = ~a\n" pending)
        (if (not pending)
          (begin
            (sleep 0.01)
            (loop (XPending display) #t))
          (begin
            (printf "Before event = ~a\n" event)
            (XNextEvent display event)
            (printf "Next Event = ~a\n" event)
            (case (XEvent-type event)
              ((Expose) (begin
                          (printf "Expose!\n")
                          (loop (XPending display) #f)))
              (else
                (begin
                  (printf "Event type: \n~a\n" (XEvent-type event))
                  (loop (XPending display) #t)))))))))
  (let* ((gc (let ((gc (XCreateGC display rootWindow 0 #f)))
               (XGetGCValues display gc 0 xgcv)
               (XSetForeground display gc blackColor)
               (XSetFillStyle display gc 'FillStippled)
               gc))
         (wind 'NoWind) ;; no wind to start with
         (snow-pixs (create-flakes display rootWindow))
         (snow-gcs (map (lambda (pic) (create-gc display rootWindow gc xgcv snowColor pic))
                        snow-pixs))
         (max-snow-flake-height (foldl max max-snow-flake-height
                                       (map (lambda (flake) (pic-height flake))
                                            snow-pixs)))
         (tree-pic (create-tree display rootWindow))
         (tree-gc (create-gc display rootWindow gc xgcv treeColor tree-pic))
         (tree-sprite (make-sprite tree-pic tree-gc))
         (trees (let ((min-x (real->int (/ display-w (sprite-width tree-sprite))))
                      (max-x (real->int (- display-w
                                           (/ display-w (sprite-width tree-sprite)))))
                      (min-y (real->int (/ display-h (sprite-height tree-sprite))))
                      (max-y (real->int (- display-h 
                                           (/ display-h (sprite-height tree-sprite))))))
                  (make-list (lambda ()
                               (make-object (random-range min-x max-x)
                                            (random-range min-y max-y)
                                            0 0 #t #t #f tree-sprite))
                             (random-range 4 8))))
         (sleigh-pics (create-sleigh display rootWindow santa-size))
         (sleigh-gcs (map (lambda (pic) 
                            (create-gc display rootWindow gc xgcv sleighColor pic))
                          sleigh-pics))
         (sleigh-sprites (map make-sprite sleigh-pics sleigh-gcs))
         ;; (santa-pic (create-santa-pic display rootWindow santa-size))
         ;; (santa-gc (create-gc display rootWindow gc xgcv redColor santa-pic))
         ;; (santa-sprite (make-sprite santa-pic santa-gc))
         (fur-pic (create-fur display rootWindow santa-size))
         (fur-gc (create-gc display rootWindow gc xgcv whiteColor fur-pic))
         (fur-sprite (make-sprite fur-pic fur-gc))
         (snow-sprites (map make-sprite snow-pixs snow-gcs))
         (rudolf-gc (let ((rgc (XCreateGC display rootWindow 0 xgcv)))
                      (XCopyGC display gc 0 rgc)
                      (XSetFillStyle display rgc 'FillSolid)
                      (XSetForeground display rgc redColor)
                      rgc))
         (windows (CreateRegion))
         (snow-catch (let ((s (CreateRegion))
                           (rect (make-XRectangle 0 display-h 
                                                  (sub1 display-w) 
                                                  (+ max-y-step
                                                     max-snow-flake-height))))
                       (XUnionRectWithRegion rect s s)
                       s))
         (snow-allow (CreateRegion))
         (add-rect (make-XRectangle 0 display-h
                                    (sub1 display-w) 
                                    (+ max-y-step max-snow-flake-height)))
         (flakes (make-list (lambda ()
                              (create-snow-flake display-w display-h
                                                 max-y-step wind
                                                 (choose-random snow-sprites)))
                            max-snow-flakes))
         (santa (create-santa display-h (list-ref santa-speed 1) santa-size gc screen display rootWindow)))

    (let-values (((need-calc windows snow-allow) 
                  (calculate-window-tops display rootWindow snow-catch display-w 
                                         display-h max-y-step max-snow-flake-height 
                                         max-win-snow-depth)))
      (let loop ((done #f)
                 (wind 'NoWind)
                 (counter 0))
        ;; (set! wind (update-wind wind))
        (set! need-calc (handle-events display rootWindow 
                                       snow-catch display-h 
                                       max-snow-flake-height 
                                       max-win-snow-depth
                                       max-y-step))
        (when need-calc
          (let-values (((need w1 s1)
                        (calculate-window-tops display rootWindow snow-catch 
                                               display-w display-h max-y-step 
                                               max-snow-flake-height 
                                               max-win-snow-depth)))
            (set! need-calc need)
            (when need-calc
              (set! windows w1)
              (set! snow-allow s1))))
	(sleep 0.05)
        (for-each (lambda (flake)
                    (update-snow-flake flake display rootWindow
                                       display-w display-h 
                                       max-y-step wind windows 
                                       snow-catch snow-allow))
                  flakes)
        (for-each (lambda (tree) (draw-tree tree display rootWindow))
                  trees)
        (when (= (modulo counter 2) 0)
          (update-santa santa wind display rootWindow))
        (loop #f (update-wind wind) (add1 counter)))
      )))

  (provide run)
  (define (run)
    (xsnow 100))

(run)
