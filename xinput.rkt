#lang scheme

(require (lib "foreign.ss")) (unsafe!)
(require "x11.rkt" "utils.rkt")
  
(define libxinput (ffi-lib "libXi"))

(define-syntax defx11
  (syntax-rules (:)
    #;
    ((_ id : x ...)
     (define id
       (let ((f (get-ffi-obj (regexp-replaces 'id '((#rx"-" "_")))
                             libxinput (_fun x ...))))
         (lambda v
           (printf "~a ~a\n" 'id v)
           (apply f v)))))
    ((_ id : x ...)
     (define id
       (get-ffi-obj (regexp-replaces (symbol->string 'id) '((#rx"-" "_")))
                    libxinput (_fun x ...))))))

;; just provide the above
(define-syntax defx11*
  (syntax-rules (:)
    ((_ id : x ...)
     (begin
       (defx11 id : x ...)
       (provide id)))
    ((_ (id x ...) expr ...)
     (begin
       (provide id)
       (define id (lambda (x ...)
                    expr ...))))))

(provide ClassInfo)
(define ClassInfo
    (_enum '(KeyClass = 0
             ButtonClass = 1
             ValuatorClass = 2
             FeedbackClass = 3 
             ProximityClass = 4
             FocusClass = 5
             OtherClass = 6)
           XID))

(define-cstruct* _XAnyClassInfo
  ([class ClassInfo]
   [length _int]))

(define-cstruct* _XDeviceInfo
  ([id XID]
   [type Atom]
   [name _string]
   [num-classes _int]
   [use _int]
   [input-class-info _XAnyClassInfo-pointer]))

(define-cstruct* _XKeyInfo
  ([class ClassInfo]
   [length _int]
   [min-keycode _ushort]
   [max-keycode _ushort]
   [num-keys _ushort]))

(define-cstruct* _XButtonInfo
  ([class ClassInfo]
   [length _int]
   [num-buttons _short]))

(define-cstruct* _XValuatorInfo
  ([class ClassInfo]
   [length _int]
   [num-axes _ubyte]
   [mode _ubyte]
   [motion-buffer _ubyte]
   ;; todo - axes : XAxisInfoPtr
   [axes _pointer]))

(define-cstruct* _XInputClassInfo
  ([input-class _ubyte]
   [event-type-base _ubyte]))

(define-cstruct* _XDevice
  ([device-id XID]
   [num-classes _int]
   [classes _XInputClassInfo-pointer]))

(define-cstruct* _XInputClass
  ([class _ubyte]
   [length _ubyte]))

(define-cstruct* _XDeviceState
  ([device-id XID]
   [num-classes _int]
   [data _XInputClass-pointer]))

(defx11 XFreeDeviceList : _XDeviceInfo-pointer -> _int)
(defx11* XListInputDevices : _XDisplay-pointer (devices : (_ptr o _int)) -> (infos : _XDeviceInfo-pointer) ->
         (let ([out (cblock->list infos _XDeviceInfo devices)])
           (register-finalizer out (lambda (c) (XFreeDeviceList infos)))
           (for-each (lambda (info)
                       (case (XAnyClassInfo-class info)
                         [(KeyClass) (cpointer-push-tag! info XKeyInfo-tag)]
                         [(ButtonClass) (cpointer-push-tag! info XButtonInfo-tag)]
                         [(ValuatorClass) (cpointer-push-tag! info XValuatorInfo-tag)]))
                     (map XDeviceInfo-input-class-info out))
           out))

(defx11* XOpenDevice : _XDisplay-pointer XID -> _XDevice-pointer)
(defx11* XCloseDevice : _XDevice-pointer _XDevice-pointer -> _int)
(defx11 XFreeDeviceState : _XDevice-pointer -> _void)
(defx11* XQueryDeviceState : _XDisplay-pointer _XDisplay-pointer -> (out : _XDeviceState-pointer) ->
         (begin
           (register-finalizer out (lambda (c) (XFreeDeviceState out)))
           out))

(define dont-care
  (list 
    XListInputDevices XFreeDeviceList
    XQueryDeviceState XOpenDevice
    ))
