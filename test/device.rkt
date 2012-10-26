#lang scheme

(require "../x11.ss")
(require "../xinput.ss")

(define display (XOpenDisplay #f))
(for-each (lambda (device)
            (printf "got device ~a\n" device)
            (printf " class is ~a\n" (XAnyClassInfo-class (XDeviceInfo-input-class-info device)))
            (printf " length is ~a\n" (XAnyClassInfo-length (XDeviceInfo-input-class-info device)))
            )
          (XListInputDevices display))
