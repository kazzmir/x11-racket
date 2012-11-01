#lang racket

  ;; good resources
  ;; http://tronche.com/gui/x/xlib/function-index.html
  ;; http://www.xfree86.org/current/manindex3.html

#| TODO
- most functions don't have the correct type
  They must use the defined types instead of _int, _uint, etc.

- When functions return a status-fail?, print an error/warning (if debug is on?)

|#

  (require ffi/unsafe
           ffi/unsafe/cvector)

  ;; (require mzlib/kw)
  ;; (require (lib "list.ss"))
  (require "fd.rkt" 
           "utils.rkt"
           ;"keysymtype.rkt"
           )

  (define libx11 (ffi-lib "libX11"))

  ;; Checks the environment for a DEBUG variable
  ;; Usage example: X11_RACKET_DEBUG=1 racket test-x11.rkt
  ;; But be warned that *compilation* must be done with this flag,
  ;; and so it depends on various things (raco make, DrRacket, racket)
  (define-for-syntax (debugging-enabled?)
    ;; will return #f if DEBUG is not defined
    (getenv "X11_RACKET_DEBUG"))
  
  (define (x11-dprintf str . args)
    (printf (x11-debug-prefix))
    (apply printf str args))

  ;; give it two expressions, it will select the correct one depending
  ;; on whether debugging is currently enabled
  (define-syntax (if-debug stx)
    (syntax-case stx ()
      [(_ debugged non-debugged)
       (if (debugging-enabled?)
         #'debugged
         #'non-debugged)]))

  (define-syntax defx11
    (syntax-rules (:)
      [(_ id : x ...)
       (begin
         (define func
           (get-ffi-obj (regexp-replaces (symbol->string 'id) '((#rx"-" "_")))
                        libx11 (_fun x ...)))
         ;; use debug to select between the two things so we don't always
         ;; pay the cost of an extra lambda on top of the ffi function
         (if-debug (define (id . v)
                     (x11-dprintf "~a: ~a" 'id v)
                     (flush-output)
                     (let ([res (apply func v)])
                       (printf " -> ~a\n" res)
                       res))
                   (define id func)))]))

  ;; just provide the above
  (define-syntax defx11*
    (syntax-rules (:)
      [(_ id : x ...)
       (begin
         (defx11 id : x ...)
         (provide id))]
      [(_ (id x ...) expr ...)
       (begin
         (provide id)
         (define id (lambda (x ...)
                      expr ...)))]))
  
  ;; Parameter to control how debug messages are printed
  ;; (useful for grepping)
  (define* x11-debug-prefix (make-parameter ""))

  ;; We should make a guarded/wrapped ctype
  ;; to check for failures.
  (define* Status _int)
  (define* (status-fail? status)
    (= 0 status))
  (define* (status-ok? status)
    (not (status-fail? status)))
  
  ;; we could make a transformer for that, if we want to keep a variable-like object.
  (define* (get-Xdebug)
    (get-ffi-obj '_Xdebug libx11 _bool))
  (define* (set-Xdebug! b)
    (set-ffi-obj! '_Xdebug libx11 _bool b))
  
  (define* Pixel _ulong)
  (define* XID _ulong)
  (define* Time _ulong)
  (define* Atom _ulong)
  (define* VisualID _ulong)
  (define* KeyCode _ubyte)
  (define* XContext _int)
  (define* XPointer _pointer)
  (define* Drawable XID)
  (define* Pixmap XID)
  (define* Cursor XID)
  (define* Font XID)
  (define* ColorMap XID)
  (define* GContext XID)
  (define* KeySym XID) ; defined as a big _enum in keysymdef.rkt, but not used (no need I think)
  (define* XrmQuark _int)
  ;(define* Window XID)
  ; null windows are turned into #f
  (define* Window 
    (make-ctype XID
                (λ(w)(or w None)) ; scheme->c
                (λ(w)(and (not (= None w)) w)) ; c->scheme
                ))

  (define RectangleRegion
    (_enum '(RectangleOut = 0
             RectangleIn = 1
             RectanglePart = 2)))

  (define AtomProperty
   (_enum '(XA_PRIMARY = 1
	    XA_SECONDARY = 2
	    XA_ARC = 3
	    XA_ATOM = 4
	    XA_BITMAP = 5
	    XA_CARDINAL = 6
	    XA_COLORMAP = 7
	    XA_CURSOR = 8
	    XA_CUT_BUFFER0 = 9
	    XA_CUT_BUFFER1 = 10
	    XA_CUT_BUFFER2 = 11
	    XA_CUT_BUFFER3 = 12
	    XA_CUT_BUFFER4 = 13
	    XA_CUT_BUFFER5 = 14
	    XA_CUT_BUFFER6 = 15
	    XA_CUT_BUFFER7 = 16
	    XA_DRAWABLE = 17
	    XA_FONT = 18
	    XA_INTEGER = 19
	    XA_PIXMAP = 20
	    XA_POINT = 21
	    XA_RECTANGLE = 22
	    XA_RESOURCE_MANAGER = 23
	    XA_RGB_COLOR_MAP = 24
	    XA_RGB_BEST_MAP = 25
	    XA_RGB_BLUE_MAP = 26
	    XA_RGB_DEFAULT_MAP = 27
	    XA_RGB_GRAY_MAP = 28
	    XA_RGB_GREEN_MAP = 29
	    XA_RGB_RED_MAP = 30
	    XA_STRING = 31
	    XA_VISUALID = 32
	    XA_WINDOW = 33
	    XA_WM_COMMAND = 34
	    XA_WM_HINTS = 35
	    XA_WM_CLIENT_MACHINE = 36
	    XA_WM_ICON_NAME = 37
	    XA_WM_ICON_SIZE = 38
	    XA_WM_NAME = 39
	    XA_WM_NORMAL_HINTS = 40
	    XA_WM_SIZE_HINTS = 41
	    XA_WM_ZOOM_HINTS = 42
	    XA_MIN_SPACE = 43
	    XA_NORM_SPACE = 44
	    XA_MAX_SPACE = 45
	    XA_END_SPACE = 46
	    XA_SUPERSCRIPT_X = 47
	    XA_SUPERSCRIPT_Y = 48
	    XA_SUBSCRIPT_X = 49
	    XA_SUBSCRIPT_Y = 50
	    XA_UNDERLINE_POSITION = 51
	    XA_UNDERLINE_THICKNESS = 52
	    XA_STRIKEOUT_ASCENT = 53
	    XA_STRIKEOUT_DESCENT = 54
	    XA_ITALIC_ANGLE = 55
	    XA_X_HEIGHT = 56
	    XA_QUAD_WIDTH = 57
	    XA_WEIGHT = 58
	    XA_POINT_SIZE = 59
	    XA_RESOLUTION = 60
	    XA_COPYRIGHT = 61
	    XA_NOTICE = 62
	    XA_FONT_NAME = 63
	    XA_FAMILY_NAME = 64
	    XA_FULL_NAME = 65
	    XA_CAP_HEIGHT = 66
	    XA_WM_CLASS = 67
	    XA_WM_TRANSIENT_FOR = 68
	    XA_LAST_PREDEFINED = 68)))

  (define XK-Pointer
    (_enum '(Any = 0
	     Left = #xfee0
	     Right = #xfee1
	     Up = #xfee2
	     Down = #xfee3
	     UpLeft = #xfee4
	     UpRight = #xfee5
	     DownLeft = #xfee6
	     DownRight = #xfee7
	     Button_Dflt = #xfee8
	     Button1 = #xfee9
	     Button2 = #xfeea
	     Button3 = #xfeeb
	     Button4 = #xfeec
	     Button5 = #xfeed)
	   _uint))

  (define BackingStoreHint
    (_enum '(NotUseful = 0
             WhenMapped = 1
             Always = 2)))

  (define InputType
    (_enum '(InputOutput = 1
             InputOnly = 2)
           _uint))

  (define Gravity
    (_enum '(ForgetGravity = 0
	     NorthWestGravity = 1
	     NorthGravity = 2
	     NorthEastGravity = 3
	     WestGravity = 4
	     CenterGravity = 5
	     EastGravity = 6
	     SouthWestGravity = 7
	     SouthGravity = 8
	     SouthEastGravity = 9
	     StaticGravity = 10)))

  (define Modifiers
    (_bitmask '(ShiftMask  = #b0000000000001
		LockMask     = #b0000000000010 ; CapsLock
		ControlMask  = #b0000000000100
		Mod1Mask     = #b0000000001000 ; Alt/meta
		Mod2Mask     = #b0000000010000 ; NumLock
		Mod3Mask     = #b0000000100000 ; Super
		Mod4Mask     = #b0000001000000 ; 
		Mod5Mask     = #b0000010000000 ; AltGr
		Button1Mask  = #b0000100000000
		Button2Mask  = #b0001000000000
		Button3Mask  = #b0010000000000
		Button4Mask  = #b0100000000000
		Button5Mask  = #b1000000000000
		Any          = #x8000)))
  
  (define* keyboard-modifiers 
    #(ShiftMask  LockMask  ControlMask  Mod1Mask  Mod2Mask  Mod3Mask  Mod4Mask  Mod5Mask))


  (define GrabMode
    (_enum '(GrabModeSync = 0
             GrabModeAsync = 1)))

  (define WindowChanges
    (_bitmask '(X =     #b0000001
		Y =           #b0000010
		Width =       #b0000100
		Height =      #b0001000
		BorderWidth = #b0010000
		Sibling =     #b0100000
		StackMode =   #b1000000)
	      _uint))

  (define WindowChanges-long
    (_bitmask '(X =      #b0000001
		Y =            #b0000010
		Width =        #b0000100
		Height =       #b0001000
		BorderWidth =  #b0010000
		Sibling =      #b0100000
		StackMode =    #b1000000)
	      _ulong))

  (define ChangeWindowAttributes
    (_bitmask '(BackPixmap       = #b000000000000001
                BackPixel        = #b000000000000010
                BorderPixmap     = #b000000000000100
                BorderPixel      = #b000000000001000
                BitGravity       = #b000000000010000
                WinGravity       = #b000000000100000
                BackingStore     = #b000000001000000
                BackingPlanes    = #b000000010000000
                BackingPixel     = #b000000100000000
                OverrideRedirect = #b000001000000000
                SaveUnder        = #b000010000000000
                EventMask        = #b000100000000000
                DontPropagate    = #b001000000000000
                Colormap         = #b010000000000000
                Cursor           = #b100000000000000)
              _ulong))

  (define XICCEncodingStyle
    (_enum '(XStringStyle
	     XCompoundTextStyle
	     XTextStyle
	     XStdICCTextStyle
	     XUTF8StringStyle)))

  (define FillStyle
    (_enum '(FillSolid = 0
	     FillTiled = 1
	     FillStippled = 2
	     FillOpaqueStippled = 3)))

  (define WindowViewable
    (_enum '(IsUnmapped = 0
             IsUnviewable = 1
             IsViewable = 2)))

  (define EventQueue
    (_enum '(QueuedAlready = 0
             QueuedAfterReading = 1
             QueuedAfterFlush 2)))

  (define EventType
    (_enum '(KeyPress = 2
             KeyRelease = 3
             ButtonPress = 4
             ButtonRelease = 5
             MotionNotify = 6
             EnterNotify = 7
             LeaveNotify = 8
             FocusIn = 9
             FocusOut = 10
             KeymapNotify = 11
             Expose = 12
             GraphicsExpose = 13
             NoExpose = 14
             VisibilityNotify = 15
             CreateNotify = 16
             DestroyNotify = 17
             UnmapNotify = 18
             MapNotify = 19
             MapRequest = 20
             ReparentNotify = 21
             ConfigureNotify = 22
             ConfigureRequest = 23
             GravityNotify = 24
             ResizeRequest = 25
             CirculateNotify = 26
             CirculateRequest = 27
             PropertyNotify = 28
             SelectionClear = 29
             SelectionRequest = 30
             SelectionNotify = 31
             ColormapNotify = 32
             ClientMessage = 33
             MappingNotify = 34
             LASTEvent = 35)))

  (define* CopyFromParent #f)
  (define* CopyFromParent/int 0)
  (define* None 0)
  (define* (None? x) (or (not x) (equal? x None))) ; sometimes the null pointer is turned into #f, sometimes into 0?
  
  (define* CurrentTime 0);(get-ffi-obj 'CurrentTime libx11 _long)) ; nope...

  (define* InputMask ;sometimes called event_mask
    (_bitmask '(NoEventMask =              #x00000000
                KeyPressMask =             #x00000001
                KeyReleaseMask =           #x00000002
                ButtonPressMask =          #x00000004
                ButtonReleaseMask =        #x00000008
                EnterWindowMask =          #x00000010
                LeaveWindowMask =          #x00000020
                PointerMotionMask =        #x00000040
                PointerMotionHintMask =    #x00000080
                Button1MotionMask =        #x00000100
                Button2MotionMask =        #x00000200
                Button3MotionMask =        #x00000400
                Button4MotionMask =        #x00000800
                Button5MotionMask =        #x00001000
                ButtonMotionMask =         #x00002000
                KeymapStateMask =          #x00004000
                ExposureMask =             #x00008000
                VisibilityChangeMask =     #x00010000
                StructureNotifyMask =      #x00020000
                ResizeRedirectMask =       #x00040000
                SubstructureNotifyMask =   #x00080000
                SubstructureRedirectMask = #x00100000
                FocusChangeMask =          #x00200000
                PropertyChangeMask =       #x00400000
                ColormapChangeMask =       #x00800000
                OwnerGrabButtonMask =      #x01000000)
	      _long))


  #|
  typedef struct {
	XExtData *ext_data;	/* hook for extension to hang data */
	struct _XDisplay *display;/* back pointer to display structure */
	Window root;		/* Root window id. */
	int width, height;	/* width and height of screen */
	int mwidth, mheight;	/* width and height of  in millimeters */
	int ndepths;		/* number of depths possible */
	Depth *depths;		/* list of allowable depths on the screen */
	int root_depth;		/* bits per pixel */
	Visual *root_visual;	/* root visual */
	GC default_gc;		/* GC for the root root visual */
	Colormap cmap;		/* default color map */
	unsigned long white_pixel;
	unsigned long black_pixel;	/* White and Black pixel values */
	int max_maps, min_maps;	/* max and min color maps */
	int backing_store;	/* Never, WhenMapped, Always */
	Bool save_unders;
	long root_input_mask;	/* initial root input mask */
} Screen;

  |#

  (define-cstruct _XIconSize
    ((min-width _int)
     (min-height _int)
     (max-width _int)
     (max-height _int)
     (width-inc _int)
     (height-inc _int)))
  
  (define-cstruct _XGC
    ((ext-data _pointer)
     (gid GContext)))

  #|
  typedef struct {
          Time time;
	  	short x, y;
		} XTimeCoord;
		|#
  
  (define-cstruct _XTimeCoord
		  ((time Time)
		   (x _short)
		   (y _short)))

  (define-cstruct _XExtData
    ((number _int)
     (next _XExtData-pointer)
     (free-private _fpointer)
     (private-data XPointer)))

#|
typedef struct {
XExtData *ext_data;	/* hook for extension to hang data */
VisualID visualid;	/* visual id of this visual */
#if defined(__cplusplus) || defined(c_plusplus)
int c_class;		/* C++ class of screen (monochrome, etc.) */
#else
int class;		/* class of screen (monochrome, etc.) */
#endif
unsigned long red_mask, green_mask, blue_mask;	/* mask values */
int bits_per_rgb;	/* log base 2 of distinct color values */
int map_entries;	/* color map entries */
} Visual;
|#

  (define-cstruct _Visual
		  ((ext-data _XExtData-pointer)
		   (visualid VisualID)
		   (class _int)
		   (red-mask _ulong)
		   (green-mask _ulong)
		   (blue-mask _ulong)
		   (bits-per-rgb _int)
		   (map-entries _int)))
  (provide _Visual-pointer)

  (define-cstruct _XWMHints
		  ((flags _long)
		   (input _bool)
		   (initial-state _int)
		   (icon-pixmap Pixmap)
		   (icon-window Window)
		   (icon-x _int)
		   (icon-y _int)
		   (icon-mask Pixmap)
		   (window-group XID)))

  (define-cstruct _XDisplay
    ((ext-data _pointer)
     (private1 _pointer)
     (fd _int)
     (private2 _int)
     (proto_major_version _int)
     (proto_minor_version _int)
     (vendor _string)
     (private3 XID)
     (private4 XID)
     (private5 XID)
     (private6 XID)
     (resource_alloc _pointer)
     (byte-order _int)
     (bitmap-unit _int)
     (bitmap-pad _int)
     (bitmap-bit-order _int)
     (nformats _int)
     (pixmap-format _pointer)
     (private8 _int)
     (releaes _int)
     (private9 _pointer)
     (private10 _pointer)
     (qlen _int)
     (last-request-read _ulong)
     (request _ulong)
     (private11 _pointer)
     (private12 _pointer)
     (private13 _pointer)
     (private14 _pointer)
     (max-request-size _uint)
     (db _pointer)
     (private15 _pointer)
     (display-name _string)
     (default-screen _int)
     (nscreens _int)
     (screens _pointer)
     (motion-buffer _ulong)
     (private16 _ulong)
     (min-keycode _int)
     (max-keycode _int)
     (private17 _pointer)
     (private18 _pointer)
     (private19 _int)
     (xdefaults _string)))
  (provide _XDisplay-pointer)

  #|

  typedef struct
#ifdef XLIB_ILLEGAL_ACCESS
_XDisplay
#endif
{
	XExtData *ext_data;	/* hook for extension to hang data */
	struct _XPrivate *private1;
	int fd;			/* Network socket. */
	int private2;
	int proto_major_version;/* major version of server's X protocol */
	int proto_minor_version;/* minor version of servers X protocol */
	char *vendor;		/* vendor of the server hardware */
        XID private3;
	XID private4;
	XID private5;
	int private6;
	XID (*resource_alloc)(	/* allocator function */
		struct _XDisplay*
	);
	int byte_order;		/* screen byte order, LSBFirst, MSBFirst */
	int bitmap_unit;	/* padding and data requirements */
	int bitmap_pad;		/* padding requirements on bitmaps */
	int bitmap_bit_order;	/* LeastSignificant or MostSignificant */
	int nformats;		/* number of pixmap formats in list */
	ScreenFormat *pixmap_format;	/* pixmap format list */
	int private8;
	int release;		/* release of the server */
	struct _XPrivate *private9, *private10;
	int qlen;		/* Length of input event queue */
	unsigned long last_request_read; /* seq number of last event read */
	unsigned long request;	/* sequence number of last request. */
	XPointer private11;
	XPointer private12;
	XPointer private13;
	XPointer private14;
	unsigned max_request_size; /* maximum number 32 bit words in request*/
	struct _XrmHashBucketRec *db;
	int (*private15)(
		struct _XDisplay*
		);
	char *display_name;	/* "host:display" string used on this connect*/
	int default_screen;	/* default screen for operations */
	int nscreens;		/* number of screens on this server*/
	Screen *screens;	/* pointer to list of screens */
	unsigned long motion_buffer;	/* size of motion buffer */
	unsigned long private16;
	int min_keycode;	/* minimum defined keycode */
	int max_keycode;	/* maximum defined keycode */
	XPointer private17;
	XPointer private18;
	int private19;
	char *xdefaults;	/* contents of defaults from server */
	/* there is more to this structure, but it is private to Xlib */
}
#ifdef XLIB_ILLEGAL_ACCESS
Display,
#endif
*_XPrivDisplay;

  |#

  ;; Im too lazy to really figure out what this should be
  ;; so ill just overcompensate
  ;; Explanation: (Laurent Orseau -- 2012-10-27)
  ;; XEvent is defined as a union (of XKeyPressedEvent, XButtonPressedEvent, etc.) in Xlib.h,
  ;; but there is (yet) nothing exactly equivalent in Racket (except _union which is not perfect).
  ;; The idea here is simple though: 
  ;; The type of a pointer as considered by Racket is checked with its cpointer-tag list,
  ;; Further down this file, the XNextEvent* procedure adds the correct tag to the list,
  ;; depending on the received event (and returns the event).
  ;; Then the user can simply consider the XEvent as an instance of the type it needs,
  ;; like XKeyPressedEvent (see the provided examples).
  ;@@ XEvent
  (define-cstruct _XEvent
    ((type EventType)))

  ;; more laziness
  ;; Instead of making the union of all the possible event types,
  ;; we just create an empty event of the maximum size and 
  ;; then turn it into the correct type using pointer-tag
  (define (make-dummy-XEvent)
    ;; 24 comes from Xlib.h
    ;; typedef union XEvent { ...; long pad[24]; }
    ;(let ([s (malloc _int (* 24 (ctype-sizeof _long)))])
    ; from the docs, the above would result in  (* 24 (ctype-sizeof _int) (ctype-sizeof _long))
    (let ([s (malloc 24 _long)])
    ;(let ([s (malloc _long 24 'raw)]) ; for testing:(WARNING: memory leak) ; Nope, still crashes...
      (memset s 0 24 _long)
      (cpointer-push-tag! s XEvent-tag)
      s))
  (provide XEvent-type make-XEvent XEvent->list* make-dummy-XEvent)
  
  (define-cstruct* _XExposeEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (window Window)
     (x _int)
     (y _int)
     (width _int)
     (height _int)
     (count _int)))

  (define-cstruct* _XAnyEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (window Window)))

#|
typedef struct {
int type;		/* of event */
unsigned long serial;	/* # of last request processed by server */
Bool send_event;	/* true if this came from a SendEvent request */
Display *display;	/* Display the event was read from */
Window window;	        /* "event" window it is reported relative to */
Window root;	        /* root window that the event occurred on */
Window subwindow;	/* child window */
Time time;		/* milliseconds */
int x, y;		/* pointer x, y coordinates in event window */
int x_root, y_root;	/* coordinates relative to root */
unsigned int state;	/* key or button mask */
unsigned int keycode;	/* detail */
Bool same_screen;	/* same screen flag */
} XKeyEvent;
|#

  (define-cstructs* (_XKeyEvent _XKeyPressedEvent _XKeyReleasedEvent)
		  ((type _int)
		   (serial _ulong)
		   (send-event _bool)
		   (display _XDisplay-pointer)
		   (window Window)
		   (root Window)
		   (subwindow Window)
		   (time Time)
		   (x _int)
		   (y _int)
		   (x-root _int)
		   (y-root _int)
		   ;(state _uint)
		   (state Modifiers) ; Laurent Orseau -- 2012-10-26
		   (keycode _uint)
		   ;(keycode KeyCode) ; no, we can't, beause KeyCode is _ubyte... go figure...
		   (same-screen _bool)))

  (define-cstructs* (_XButtonEvent _XButtonPressedEvent _XButtonReleasedEvent)
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (window Window)
     (root Window)
     (subwindow Window)
     (time Time)
     (x _int)
     (y _int)
     (x-root _int)
     (y-root _int)
     (state Modifiers)
     (button _uint)
     (same-screen _bool)))

  (define-cstructs* (_XMotionEvent _XPointerMovedEvent)
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (window Window)
     (root Window)
     (subwindow Window)
     (time Time)
     (x _int)
     (y _int)
     (x-root _int)
     (y-root _int)
     (state Modifiers)
     (is-hint _byte)
     (same-screen _bool)))

  (define-cstructs* (_XCrossingEvent _XEnterWindowEvent _XLeaveWindowEvent)
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (window Window)
     (root Window)
     (subwindow Window)
     (time Time)
     (x _int)
     (y _int)
     (x-root _int)
     (y-root _int)
     (mode _int)
     (detail _int)
     (same-screen _bool)
     (focus _bool)
     (state _uint)))

  (define-cstructs* (_XFocusChangeEvent _XFocusInEvent _XFocusOutEvent)
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (window Window)
     (mode _int)
     (detail _int)))

  (define-cstruct* _XKeymapEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (window Window)
     (key-vector _cvector)))

  (define-cstruct* _XGraphicsExposeEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (drawable Drawable)
     (x _int)
     (y _int)
     (width _int)
     (height _int)
     (count _int)
     (major-code _int)
     (minor-code _int)))

  (define-cstruct* _XNoExposeEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (drawable Drawable)
     (major-code _int)
     (minor-code _int)))

  (define-cstruct* _XVisibilityEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (window Window)
     (state _int)))

  (define-cstruct* _XCreateWindowEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (parent Window)
     (window Window)
     (x _int)
     (y _int)
     (width _int)
     (height _int)
     (border-width _int)
     (override-redirect _bool)))

  (define-cstruct* _XDestroyWindowEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (event Window)
     (window Window)))

  (define-cstruct* _XUnmapEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (event Window)
     (window Window)
     (from-configure _bool)))

  (define-cstruct* _XMapEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (event Window)
     (window Window)
     (override-redirect _bool)))

  (define-cstruct* _XMapRequestEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (parent Window)
     (window Window)))

  (define-cstruct* _XReparentEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (event Window)
     (window Window)
     (parent Window)
     (x _int)
     (y _int)
     (override-redirect _bool)))

  (define-cstruct* _XConfigureEvent
    ((type _int)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (event Window)
     (window Window)
     (x _int)
     (y _int)
     (width _int)
     (height _int)
     (border-width _int)
     (above Window)
     (override-redirect _bool)))

  (define-cstruct* _XGravityEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (event Window)
     (window Window)
     (x _int)
     (y _int)))

  
  (define-cstruct* _XResizeRequestEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (window Window)
     (width _int)
     (height _int)))

  (define-cstruct* _XConfigureRequestEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (parent Window)
     (window Window)
     (x _int)
     (y _int)
     (width _int)
     (height _int)
     (border-width _int)
     (above Window)
     (detail _int) ; = stack-mode
     (value-mask WindowChanges-long)))

  (define-cstruct* _XCirculateEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (event Window)
     (window Window)
     (place _int)))

  (define-cstruct* _XCirculateRequestEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (parent Window)
     (window Window)
     (place _int)))

  (define-cstruct* _XPropertyEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (window Window)
     (atom Atom)
     (time Time)
     (state _int)))

  (define-cstruct* _XSelectionClearEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (window Window)
     (selection Atom)
     (time Time)))

  (define-cstruct* _XSelectionRequestEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (owner Window)
     (requestor Window)
     (selection Atom)
     (target Atom)
     (property Atom)
     (time Time)))

  (define-cstruct* _XSelectionEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (requestor Window)
     (selection Atom)
     (target Atom)
     (property Atom)
     (time Time)))

  (define-cstruct* _XColormapEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (window Window)
     (colormap ColorMap)
     (new _bool)
     (state _int)))

  (define-cstruct* _XClientMessageEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (window Window)
     (message-type Atom)
     (format _int)
     (data _cvector)))
  
  ;; Laurent Orseau -- 2012-10-27
  ;; It's not really an XEvent?
  (define-cstruct* _XErrorEvent
    ((type _int)
     (display _XDisplay-pointer)
     (resourceid XID)
     (serial _ulong)
     (error-code _uint8)
     (request-code _uint8)
     (minor-code _uint8)))

  (define-cstruct* _XMappingEvent
    ((type EventType)
     (serial _ulong)
     (send-event _bool)
     (display _XDisplay-pointer)
     (window Window)
     (request _int)
     (first-keycode _int)
     (count _int)))

  (define-cstruct _XCharStruct
    ((lbearing _short)
     (rbearing _short)
     (width _short)
     (ascent _short)
     (descent _short)
     (attributes _ushort)))

  (define-cstruct _XFontStruct
    ((ext-data _XExtData-pointer)
     (fid Font)
     (direction _uint)
     (min-char-or-byte2 _uint)
     (max-char-or-byte2 _uint)
     (min-byte1 _uint)
     (max-byte1 _uint)
     (all-chars-exist _bool)
     (default-char _uint)
     (n-properties _int)
     (properties _pointer)
     (min-bounds _XCharStruct)
     (max-bounds _XCharStruct)
     (per-char _XCharStruct-pointer)
     (ascent _int)
     (descent _int)))

  (define-cstruct _XStandardColormap
    ((colormap ColorMap)
     (red-max _ulong)
     (red-mult _ulong)
     (green-max _ulong)
     (blue-max _ulong)
     (blue-mult _ulong)
     (base-pixel _ulong)
     (visualid VisualID)
     (killid XID)))

  (define-cstruct _XGCValues
    ((function _int)
     (plane-mask _ulong)
     (foreground _ulong)
     (background _ulong)
     (line-width _int)
     (line-style _int)
     (cap-style _int)
     (join-style _int)
     (fill-style _int)
     (fill-rule _int)
     (arc-mode _int)
     (tile Pixmap)
     (stipple Pixmap)
     (ts-x-origin _int)
     (ts-y-origin _int)
     (font Font)
     (subwindow-mode _int)
     (graphics-exposures _bool)
     (clip-x-origin _int)
     (clip-y-origin _int)
     (clip-mask Pixmap)
     (dash-offset _int)
     (dashes _byte)))
  (define (make-dummy-XGCValues)
    (make-XGCValues 0 0 0 0
		    0 0 0 0
		    0 0 0 0
		    0 0 0 0
		    0 #f 0 0
		    0 0 0))

  (provide _XGCValues XGCValues-tag make-dummy-XGCValues)

#|
typedef struct {
Pixmap background_pixmap;	/* background or None or ParentRelative */
unsigned long background_pixel;	/* background pixel */
Pixmap border_pixmap;	/* border of the window */
unsigned long border_pixel;	/* border pixel value */
int bit_gravity;		/* one of bit gravity values */
int win_gravity;		/* one of the window gravity values */
int backing_store;		/* NotUseful, WhenMapped, Always */
unsigned long backing_planes;/* planes to be preseved if possible */
unsigned long backing_pixel;/* value to use in restoring planes */
Bool save_under;		/* should bits under be saved? (popups) */
long event_mask;		/* set of events that should be saved */
long do_not_propagate_mask;	/* set of events that should not propagate */
Bool override_redirect;	/* boolean value for override-redirect */
Colormap colormap;		/* color map to be associated with window */
Cursor cursor;		/* cursor to be displayed (or None) */
} XSetWindowAttributes;
|#

  (define-cstruct _XSetWindowAttributes
		  ((background-pixmap Pixmap)
		   (background-pixel _ulong)
		   (border-pixmap Pixmap)
		   (border-pixel _ulong)
		   (bit-gravity Gravity)
		   (win-gravity Gravity)
		   (backing-store BackingStoreHint)
		   (backing-planes _ulong)
		   (backing-pixel _ulong)
		   (save-under _bool)
		   (event-mask InputMask)
		   (do-not-propagate-mask _long)
		   (override-redirect _bool)
		   (colormap ColorMap)
		   (cursor Cursor)))
  (provide (rename-out (create-xsetwindowattributes make-XSetWindowAttributes)))
  (define (create-xsetwindowattributes
	    	#:background-pixmap
	       (background-pixmap None)
	       #:background-pixel
	       (background-pixel 0)
	       #:border-pixmap
	       (border-pixmap CopyFromParent/int)
	       #:border-pixel
	       (border-pixel 0)
	       #:bit-gravity
	       (bit-gravity 'ForgetGravity)
	       #:win-gravity
	       (win-gravity 'NorthWestGravity)
	       #:backing-store
	       (backing-store 'NotUseful)
	       #:backing-planes
	       (backing-planes (sub1 (expt 2 32)))
	       #:backing-pixel
	       (backing-pixel 0)
	       #:save-under
	       (save-under #f)
	       #:event-mask
	       (event-mask '())
	       #:do-not-propagate-mask
	       (do-not-propagate-mask 0)
	       #:override-redirect
	       (override-redirect #f)
	       #:colormap
	       (colormap CopyFromParent/int)
	       #:cursor
	       (cursor None))
	     (make-XSetWindowAttributes
	       background-pixmap background-pixel border-pixmap border-pixel
	       bit-gravity win-gravity backing-store backing-planes
	       backing-pixel save-under event-mask
	       do-not-propagate-mask override-redirect colormap
	       cursor))

  (provide XSetWindowAttributes->mask)
  (define (XSetWindowAttributes->mask attrs)
    (let-syntax ((combine (syntax-rules ()
			    ((_ (test expr) ...)
			     (filter symbol? (list (if test expr #f) ...))))))
      (combine ((not (eq? (XSetWindowAttributes-background-pixmap attrs) None))
	      'BackPixmap)
	     ((not (eq? (XSetWindowAttributes-background-pixel attrs) 0))
	      'BackPixel)
	     ((not (null? (XSetWindowAttributes-event-mask attrs)))
	      'EventMask)
	     ((not (eq? (XSetWindowAttributes-border-pixmap attrs) None))
	      'BorderPixmap)
	     ((not (= (XSetWindowAttributes-border-pixel attrs) 0))
	      'BorderPixel)
	     ((not (eq? (XSetWindowAttributes-bit-gravity attrs) 'ForgetGravity))
	      'BitGravity)
	     ((not (eq? (XSetWindowAttributes-win-gravity attrs) 'NorthWestGravity))
	      'WinGravity)
	     ((not (eq? (XSetWindowAttributes-backing-store attrs) 'NotUseful))
	      'BackingStore)
	     ((not (eq? (XSetWindowAttributes-backing-planes attrs) (sub1 (expt 2 32))))
	      'BackingPlanes)
	     ((not (= (XSetWindowAttributes-backing-pixel attrs) 0))
	      'BackingPixel)
	     ((not (eq? (XSetWindowAttributes-override-redirect attrs) #f))
	      'OverrideRedirect)
	     ((not (eq? (XSetWindowAttributes-save-under attrs) #f))
	      'SaveUnder)
	     ((not (= (XSetWindowAttributes-do-not-propagate-mask attrs) 0))
	      'DontPropagate)
	     ((not (= (XSetWindowAttributes-colormap attrs) CopyFromParent/int))
	      'Colormap)
	     ((not (eq? (XSetWindowAttributes-cursor attrs) None))
	      'Cursor))))


  #|
typedef struct {
    	long flags;	/* marks which fields in this structure are defined */
	int x, y;		/* obsolete for new window mgrs, but clients */
	int width, height;	/* should set so old wm's don't mess up */
	int min_width, min_height;
	int max_width, max_height;
    	int width_inc, height_inc;
	struct {
		int x;	/* numerator */
		int y;	/* denominator */
	} min_aspect, max_aspect;
	int base_width, base_height;		/* added by ICCCM version 1 */
	int win_gravity;			/* added by ICCCM version 1 */
} XSizeHints;
|#

  (define-cstruct _Aspect
    ((x _int)
     (y _int)))

  (define-cstruct _XSizeHints
    ((flags _long)
     (x _int)
     (y _int)
     (width _int)
     (height _int)
     (min-width _int)
     (min-height _int)
     (max-width _int)
     (max-height _int)
     (width-inc _int)
     (height-inc _int)
     (min-aspect _Aspect)
     (max-aspect _Aspect)
     (base-width _int)
     (base-height _int)
     (win-gravity _int)))

  (define-cstruct* _XTextProperty
    ((value _pointer)
     (encoding Atom)
     (format _int)
     (nitems _ulong)))

#|
typedef struct _XImage {
int width, height;		/* size of image */
int xoffset;		/* number of pixels offset in X direction */
int format;			/* XYBitmap, XYPixmap, ZPixmap */
char *data;			/* pointer to image data */
int byte_order;		/* data byte order, LSBFirst, MSBFirst */
int bitmap_unit;		/* quant. of scanline 8, 16, 32 */
int bitmap_bit_order;	/* LSBFirst, MSBFirst */
int bitmap_pad;		/* 8, 16, 32 either XY or ZPixmap */
int depth;			/* depth of image */
int bytes_per_line;		/* accelarator to next line */
int bits_per_pixel;		/* bits per pixel (ZPixmap) */
unsigned long red_mask;	/* bits in z arrangment */
unsigned long green_mask;
unsigned long blue_mask;
XPointer obdata;		/* hook for the object routines to hang on */
struct funcs {		/* image manipulation routines */
struct _XImage *(*create_image)(
struct _XDisplay* /* display */,
Visual*		/* visual */,
unsigned int	/* depth */,
int		/* format */,
int		/* offset */,
char*		/* data */,
unsigned int	/* width */,
unsigned int	/* height */,
int		/* bitmap_pad */,
int		/* bytes_per_line */);
int (*destroy_image)        (struct _XImage *);
unsigned long (*get_pixel)  (struct _XImage *, int, int);
int (*put_pixel)            (struct _XImage *, int, int, unsigned long);
struct _XImage *(*sub_image)(struct _XImage *, int, int, unsigned int, unsigned int);
int (*add_pixel)            (struct _XImage *, long);
} f;
} XImage;

|#

  (define-cstruct _XImage
		  ((width _int)
		   (height _int)
		   (xoffset _int)
		   (format _int)
		   (data _pointer)
		   (byte-order _int)
		   (bitmap-unit _int)
		   (bitmap-bit-order _int)
		   (bitmap-pad _int)
		   (depth _int)
		   (bytes-per-line _int)
		   (bits-per-pixel _int)
		   (red-mask _ulong)
		   (green-mask _ulong)
		   (blue-mask _ulong)
		   (obdata _pointer)))
  (provide _XImage-pointer)

  (define-cstruct _XColor
    ((pixel _ulong)
     (red _ushort)
     (green _ushort)
     (blue _ushort)
     (flags _byte)
     (pad _byte)))

  (provide make-XColor-rgb)
  (define (make-XColor-rgb red green blue)
    (make-XColor 0 red green blue 0 0))

  (define-cstruct _Box
    ((x1 _short)
     (x2 _short)
     (y1 _short)
     (y2 _short)))

  (define-cstruct _XSegment
    ((x1 _short)
     (y1 _short)
     (x2 _short)
     (y2 _short)))

  (define-cstruct _XPoint
    ((x _short)
     (y _short)))

  (define-cstruct* _XModifierKeymap
    ((max-keypermod _int)
     (modifiermap _pointer)))

  (define-cstruct _XChar2b
		  ((byte1 _ubyte)
		   (byte2 _ubyte)))

  (define-cstruct _XTextItem
		  ((chars _string)
		   (nchars _int)
		   (delta _int)
		   (font Font)))

  (define-cstruct _XTextItem16
		  ((chars _XChar2b-pointer)
		   (nchars _int)
		   (delta _int)
		   (font Font)))

#|
typedef struct {
int key_click_percent;
int bell_percent;
unsigned int bell_pitch, bell_duration;
unsigned long led_mask;
int global_auto_repeat;
char auto_repeats[32];
} XKeyboardState;
|#

  (define-cstruct _XKeyboardState
		  ((key-click-percent _int)
		   (bell-percent _int)
		   (bell-pitch _uint)
		   (bell-duration _uint)
		   (led-mask _ulong)
		   (global-auto-repeat _int)
		   (auto-repeats _pointer)))

  (define-cstruct _XRectangle
    ((x _short)
     (y _short)
     (width _short)
     (height _short)))
  (provide make-XRectangle XRectangle-width XRectangle-height XRectangle-x XRectangle-y
	   set-XRectangle-height! set-XRectangle-width! set-XRectangle-x! set-XRectangle-y!)
  #;
  (provide _XRectangle XRectangle-tag set-XRectangle-x! set-XRectangle-y!
	   set-XRectangle-width! set-XRectangle-height!)

  #|
  typedef struct {
  	short x, y;
  	unsigned short width, height;
  	short angle1, angle2;
  } XArc;
  |#

  (define-cstruct _XArc
    ((x _short)
     (y _short)
     (width _ushort)
     (height _ushort)
     (angle1 _short)
     (angle2 _short)))

#|
typedef struct {
int depth;
int bits_per_pixel;
int scanline_pad;
} XPixmapFormatValues;
|#

  (define-cstruct _XPixmapFormatValues
		  ((depth _int)
		   (bits-per-pixel _int)
		   (scanline-pad _int)))

#|
typedef struct {
int x, y;
int width, height;
int border_width;
Window sibling;
int stack_mode;
} XWindowChanges;
|#
  
  (define-cstruct _XWindowChanges
		  ((x _int)
		   (y _int)
		   (width _int)
		   (height _int)
		   (border-width _int)
		   (sibling Window)
		   (stack-mode _int)))

  (provide (rename-out [XWindowChanges* XWindowChanges]))
  (define (XWindowChanges* #:x
			  (x 0)
			  #:y
			  (y 0)
			  #:width
			  (width 0)
			  #:height
			  (height 0)
			  #:border-width
			  (border-width 0)
			  #:sibling
			  (sibling 0)
			  #:stack-mode
			  (stack-mode 0))
	     (make-XWindowChanges x y width height border-width sibling stack-mode))
  (provide make-XWindowChanges)

  (define-cstruct _XFontSetExtents
		  ((max-ink-extent _XRectangle)
		   (max-logical-extent _XRectangle)))

#|
typedef struct {
int key_click_percent;
int bell_percent;
int bell_pitch;
int bell_duration;
int led;
int led_mode;
int key;
int auto_repeat_mode;   /* On, Off, Default */
} XKeyboardControl;
|#

  (define-cstruct _XKeyboardControl
    ((key-click-percent _int)
     (bell-percent _int)
     (bell-pitch _int)
     (bell-duration _int)
     (led _int)
     (led-mode _int)
     (key _int)
     (auto-repeat-mode _int)))

  (define-cstruct _XRegion
    ((size _long)
     (numRects _long)
     (rects _Box-pointer)
     (extends _Box)))

  (define-cstruct* _XClassHint
    ((res-name _string)
     (res-class _string)))

  (define-cstruct _XFontSet ((a _int)))
  (define-cstruct _XrmHashBucketRec ((a _int)))
  (define-cstruct _XOM ((a _int)))
  (define-cstruct _XOC ((a _int)))
  (define-cstruct _XIM ((a _int)))
  (define-cstruct _XIC ((a _int)))

  (define-cstruct _XComposeStatus
    ((compose-prt _pointer)
     (chars-matched _int)))

#|
typedef struct {
char           *chars;
int             nchars;
int             delta;
XFontSet        font_set;
} XmbTextItem;
|#

  (define-cstruct _XwcTextItem
		  ((chars _string)
		   (nchars _int)
		   (delta _int)
		   (font-set _XFontSet)))

#|
typedef struct {
Visual *visual;
VisualID visualid;
int screen;
int depth;
#if defined(__cplusplus) || defined(c_plusplus)
int c_class;					/* C++ */
#else
int class;
#endif
unsigned long red_mask;
unsigned long green_mask;
unsigned long blue_mask;
int colormap_size;
int bits_per_rgb;
} XVisualInfo;
|#

  (define-cstruct _XVisualInfo
		  ((visual _Visual-pointer)
		   (visualid VisualID)
		   (screen _int)
		   (depth _int)
		   (class _int)
		   (red-mask _ulong)
		   (green-mask _ulong)
		   (blue-mask _ulong)
		   (colormap-size _int)
		   (bits-per-rgb _int)))

#|
typedef struct {
char           *chars;
int             nchars;
int             delta;
XFontSet        font_set;
} XmbTextItem;
|#

  (define-cstruct _XmbTextItem
		  ((chars _string)
		   (nchars _int)
		   (delta _int)
		   (font-set _XFontSet)))


#|
typedef struct {		/* public to extension, cannot be changed */
int extension;		/* extension number */
int major_opcode;	/* major op-code assigned by server */
int first_event;	/* first event number for the extension */
int first_error;	/* first error number for the extension */
} XExtCodes;
|#

  (define-cstruct _XExtCodes
		  ((extension _int)
		   (major-opcode _int)
		   (first-event _int)
		   (first-error _int)))

#|
typedef struct {
int family;		/* for example FamilyInternet */
int length;		/* length of address, in bytes */
char *address;		/* pointer to where to find the bytes */
} XHostAddress;
|#

  (define-cstruct _XHostAddress
		  ((family _int)
		   (length _int)
		   (address _string)))

#|
typedef struct {
int type;
unsigned long serial;	/* # of last request processed by server */
Bool send_event;	/* true if this came from a SendEvent request */
Display *display;	/* Display the event was read from */
Window window;		/* unused */
int request;		/* one of MappingModifier, MappingKeyboard,
MappingPointer */
int first_keycode;	/* first keycode */
int count;		/* defines range of change w. first_keycode*/
} XMappingEvent;
|#

#;
  (define-cstruct _XMappingEvent
		  ((type _int)
		   (serial _ulong)
		   (send-event _bool)
		   (display _XDisplay-pointer)
		   (window Window)
		   (request _int)
		   (first-keycode _int)
		   (count _int)))

  (define-cstruct _Screen
    ((ext-data _pointer)
     (display _XDisplay-pointer)
     (root Window)
     (width _int)
     (height _int)
     (mwidth _int)
     (mheight _int)
     (ndepths _int)
     (depths _pointer)
     (root-depth _int)
     (root-visual _pointer)
     (default-gc _pointer)
     (cmap ColorMap)
     (white-pixel _ulong)
     (black-pixel _ulong)
     (max-maps _int)
     (min-maps _int)
     (backing-store _int)
     (save-unders _bool)
     (root-input-mask _ulong)))

  (define-cstruct _XWindowAttributes
    ((x _int)
     (y _int)
     (width _int)
     (height _int)
     (border-width _int)
     (depth _int)
     (visual _pointer)
     (root Window)
     (class _int)
     (bit-gravity _int)
     (win-gravity _int)
     (backing-store _int)
     (backing-planes _ulong)
     (backing-pixel _ulong)
     (save-under _bool)
     (colormap ColorMap)
     (map-installed _bool)
     (map-state WindowViewable)
     (all-event-maskes _long)
     (your-event-mask _long)
     (do-not-propagate-mask _long)
     (override-redirect _bool)
     (screen _Screen-pointer/null)))
  (define (make-dummy-attributes)
    (make-XWindowAttributes 0 0 0 0
			    0 0 #f 0
			    0 0 0 0
			    0 0 #f 0 #f
			    'IsUnmapped 
			    0 0 0 #f #f))
  (provide (except-out (struct-out XWindowAttributes) make-XWindowAttributes)
           (rename-out (make-dummy-attributes make-XWindowAttributes)))
  #;
  (provide XWindowAttributes-save-under XWindowAttributes-map-state 
	   XWindowAttributes-width
	   XWindowAttributes-height
	   (rename make-dummy-attributes make-XWindowAttributes))

  ;; this makes things work in KDE
  (define (virtual-root-window screen)
   (let* ((display (Screen-display screen))
	  (root (Screen-root screen))
	  (swm-vroot (XInternAtom display "__SWM_VROOT" #f)))
    (for-each (lambda (window)
	       (let ((new-window (XGetWindowProperty display window
				  swm-vroot 0 1 #f 'XA_WINDOW)))
		(when new-window
		 (set! root (ptr-ref new-window Window 0)))))
     (XQueryTree display root))
    root))

  (defx11* XInternAtom : _XDisplay-pointer _string _bool -> Atom)

  (defx11* XGetWindowProperty :
   _XDisplay-pointer Window
   Atom _long _long _bool AtomProperty
   (_ptr o Atom)
   (_ptr o _int)
   (_ptr o _ulong)
   (_ptr o _ulong)
   (ret : (_ptr o _pointer))
   -> _bool
   -> ret)

  ;; Return &(display->screens[screen])
  (define (screen-of-display display screen)
    (ptr-ref (XDisplay-screens display) _Screen screen))

  (defx11* (DisplayWidth display screen)
	     (Screen-width (screen-of-display display screen)))

  (defx11* (DisplayHeight display screen)
	     (Screen-height (screen-of-display display screen)))

  (defx11* (BlackPixel display screen)
	     (Screen-black-pixel (screen-of-display display screen)))

  (defx11* (WhitePixel display screen)
	     (Screen-white-pixel (screen-of-display display screen)))

  (defx11* (DefaultScreen display)
           (XDisplay-default-screen display))

  (defx11* (DefaultDepth display screen)
    (Screen-root-depth (screen-of-display display screen)))

  (defx11* (DefaultVisual display screen)
    (XDefaultVisual display screen)
    #;
    (Screen-root-visual (screen-of-display display screen)))

  (defx11* (RootWindow display screen)
	     (virtual-root-window (screen-of-display display screen)))

  (defx11* (DefaultColorMap display screen)
	     (Screen-cmap (screen-of-display display screen)))

  (defx11* (AllocNamedColor display screen name default)
	     (let-values (((ret screen-color exact-color)
			       (XAllocNamedColor display 
						 (DefaultColorMap display screen) 
						 name)))
               (if (not (= 0 ret))
		 (XColor-pixel screen-color)
		 default)))

  (defx11* XSetClipOrigin : _XDisplay-pointer _XGC-pointer _int _int -> _int)
  (defx11* XSetClipMask : _XDisplay-pointer _XGC-pointer Pixmap -> _int)

  (defx11* XGrabServer : _XDisplay-pointer -> _int)
  (defx11* XUngrabServer : _XDisplay-pointer -> _int)

  (defx11 XAllocNamedColor : _XDisplay-pointer ColorMap _string
	                     (screen-color : (_ptr o _XColor))
			     (exact-color : (_ptr o _XColor))
			     -> (out : _int)
			     -> (values out screen-color exact-color))

  (defx11* XFree : _pointer -> _int)

  (defx11* XCopyArea : _XDisplay-pointer Drawable Drawable _XGC-pointer
	   _int _int _uint _uint _int _int -> _int)

  (defx11* XPending : _XDisplay-pointer -> _bool)

  ;@@ XNextEvent
  (defx11* XNextEvent : _XDisplay-pointer _XEvent-pointer -> _int)
  (defx11* (XNextEvent* display)
    ;(printf "In XNextEvent*\n")(flush-output)
    (let ((e (make-dummy-XEvent))
          (push-tags! (λ(e . tags)(for [(t tags)] (cpointer-push-tag! e t))))
          )
      ;(printf "Getting event... display=~a e=~a" display e)(flush-output)
      ;(XPeekEvent display e) ; just in case
      ;(printf "After XPeekEvent\n")(flush-output)
      ; WARNING: I have some crashes on XNExtEvent with MotionEvent
      ; I can't find where it comes from...
      ; (same with XPeekEvent)
      ;(printf "e: ~a\n" e) ; is it null ? ; No crash when I print e?!
      (XNextEvent display e)
      ;(printf "Ok.\n")(flush-output)
      ;(printf "Pushing XAnyEvent tag... ")(flush-output)
      (cpointer-push-tag! e XAnyEvent-tag)
      ;(printf "Ok.\n")(flush-output)
      (case (XAnyEvent-type e)
        ; place the most specific type at the end since it's the the one that is printed.
        ((KeyPress)          (push-tags! e XKeyEvent-tag XKeyPressedEvent-tag))
        ((KeyRelease)        (push-tags! e XKeyEvent-tag XKeyReleasedEvent-tag))
        ((ButtonPress)       (push-tags! e XButtonEvent-tag XButtonPressedEvent-tag))
        ((ButtonRelease)     (push-tags! e XButtonEvent-tag XButtonReleasedEvent-tag))
        ((MotionNotify)      (push-tags! e XMotionEvent-tag XPointerMovedEvent-tag))
        ((EnterNotify)       (push-tags! e XCrossingEvent-tag XEnterWindowEvent-tag))
        ((LeaveNotify)       (push-tags! e XCrossingEvent-tag XLeaveWindowEvent-tag))
        ((FocusIn)           (push-tags! e XFocusChangeEvent-tag XFocusInEvent-tag))
        ((FocusOut)          (push-tags! e XFocusChangeEvent-tag XFocusOutEvent-tag))
        ((KeymapNotify)      (push-tags! e XKeymapEvent-tag))
        ((Expose)            (push-tags! e XExposeEvent-tag))
        ((GraphicsExpose)    (push-tags! e XGraphicsExposeEvent-tag))
        ((NoExpose)          (push-tags! e XNoExposeEvent-tag))
        ((VisibilityNotify)  (push-tags! e XVisibilityEvent-tag))
        ((CreateNotify)      (push-tags! e XCreateWindowEvent-tag))
        ((DestroyNotify)     (push-tags! e XDestroyWindowEvent-tag))
        ((UnmapNotify)       (push-tags! e XUnmapEvent-tag))
        ((MapNotify)         (push-tags! e XMapEvent-tag))
        ((MapRequest)        (push-tags! e XMapRequestEvent-tag))
        ((ReparentNotify)    (push-tags! e XReparentEvent-tag))
        ((ConfigureNotify)   (push-tags! e XConfigureEvent-tag))
        ((ConfigureRequest)  (push-tags! e XConfigureRequestEvent-tag))
        ((GravityNotify)     (push-tags! e XGravityEvent-tag))
        ((ResizeRequest)     (push-tags! e XResizeRequestEvent-tag))
        ((CirculateNotify)   (push-tags! e XCirculateEvent-tag))
        ((CirculateRequest)  (push-tags! e XCirculateRequestEvent-tag))
        ((PropertyNotify)    (push-tags! e XPropertyEvent-tag))
        ((SelectionClear)    (push-tags! e XSelectionClearEvent-tag))
        ((SelectionRequest)  (push-tags! e XSelectionRequestEvent-tag))
        ((SelectionNotify)   (push-tags! e XSelectionEvent-tag))
        ((ColormapNotify)    (push-tags! e XColormapEvent-tag))
        ((ClientMessage)     (push-tags! e XClientMessageEvent-tag))
        ((MappingNotify)     (push-tags! e XMappingEvent-tag))
        ;(else (printf "No tag added!\n"))
        )
      ;(printf "After pushing all tags\n")(flush-output)
      e))

  (defx11* XGetGeometry :
	   _XDisplay-pointer Drawable (parent : (_ptr o Window))
	   (x : (_ptr o _int)) (y : (_ptr o _int))
	   (width : (_ptr o _uint)) (height : (_ptr o _uint))
	   (border-width : (_ptr o _uint)) (depth : (_ptr o _uint))
	   -> (status : Status)
	   ->
	   (if (not (= status 1))
	     (error "XGetGeometry broke!\n")
	     (values parent x y width height border-width depth)))

  ;; Return a list of int's( Window ids )
  (defx11* XQueryTree :
	   _XDisplay-pointer Window 
	   (f1 : (_ptr o Window)) ;; dont care
	   (f2 : (_ptr o Window)) ;; dont care
	   (children : (_ptr o _pointer))
	   (nchildren : (_ptr o _int))
	   -> Status
	   -> (let ([out (cblock->list children Window nchildren)])
            (register-finalizer out (lambda (c) (XFree c)))
            out))

  (defx11* XSetErrorHandler : (_fun _XDisplay-pointer _XErrorEvent-pointer -> _int) -> _void)

  (defx11* XCreateGC :
	   _XDisplay-pointer Drawable _ulong _XGCValues-pointer/null -> _XGC-pointer)

  (defx11* XGetGCValues :
	   _XDisplay-pointer _XGC-pointer _ulong _XGCValues-pointer -> Status)

  (defx11* XCreateBitmapFromData :
	   _XDisplay-pointer Drawable _pointer _uint _uint -> Pixmap)

  (defx11* XSetForeground :
	   _XDisplay-pointer _XGC-pointer _ulong -> _int)

  (defx11* XPointInRegion : _XRegion-pointer _int _int -> _bool)
  
  (defx11* XUnionRectWithRegion :
	   _XRectangle-pointer _XRegion-pointer _XRegion-pointer
	   -> _int)

  (defx11* XSubtractRegion :
	   _XRegion-pointer _XRegion-pointer _XRegion-pointer
	   -> _int)

  (defx11* XCreateRegion :
	   -> _XRegion-pointer)

  (defx11* XDestroyRegion : _XRegion-pointer -> _int)

  (defx11* XSetStipple :
	   _XDisplay-pointer _XGC-pointer Pixmap -> _int)

  (defx11* XSetFillStyle :
	   _XDisplay-pointer _XGC-pointer FillStyle -> _int)

  (defx11* XSetTSOrigin :
	   _XDisplay-pointer _XGC-pointer _int _int -> _int)

  (defx11* XFillRectangle :
	   _XDisplay-pointer Drawable _XGC-pointer _int _int _uint _uint -> _int)

  (defx11* XRectInRegion : 
	   _XRegion-pointer _int _int _uint _uint -> RectangleRegion)

  (defx11* XCopyGC :
	   _XDisplay-pointer _XGC-pointer _ulong _XGC-pointer -> _int)

  (defx11* XClearArea :
	   _XDisplay-pointer Window _int _int _uint _uint _bool -> _int)

  (defx11* XSelectInput :
	   _XDisplay-pointer Window InputMask -> _int)

  (defx11* XEventsQueued :
	   _XDisplay-pointer EventQueue -> _int)

  (defx11* XGetWindowAttributes :
	   _XDisplay-pointer Window
       (attributes : (_ptr o _XWindowAttributes))
	   -> Status -> attributes)

  ;(defx11* XOpenDisplay : _string -> _XDisplay-pointer)
  (defx11* XOpenDisplay : _string -> _XDisplay-pointer/null) ; Laurent Orseau -- 2012-10-27

  (defx11* XAllPlanes : -> _uint)

  (defx11* XESetWireToError : _XDisplay-pointer _int (_fun _void -> _bool)
	   -> _bool)

  (defx11* XSetClipRectangles : _XDisplay-pointer _XGC-pointer 
	   _int _int
	   (_list i _XRectangle) _int _int -> _int)

  (defx11* XBitmapBitOrder : _XDisplay-pointer -> _int)

  (defx11* XESetWireToEvent : _XDisplay-pointer _int Status -> _int)

  (defx11* XSetCloseDownMode : _XDisplay-pointer _int -> _int)

  (defx11* XBitmapPad : _XDisplay-pointer -> _int)

  (defx11* XEmptyRegion : _XRegion-pointer -> _bool)

  (defx11* XSetCommand : _XDisplay-pointer Window
	   (_list i _string) _int -> _int)

  (defx11* XBitmapUnit : _XDisplay-pointer -> _int)

  (defx11* XEnableAccessControl : _XDisplay-pointer -> _int)

  (defx11* XSetDashes : _XDisplay-pointer _XGC-pointer _int
	   _string _int -> _int)

  (defx11* XEqualRegion : _XRegion-pointer _XRegion-pointer -> _bool)

  (defx11* (BlackPixelOfScreen screen)
	   (Screen-black-pixel screen))

  (defx11* XSetFillRule : _XDisplay-pointer _XGC-pointer _int -> _int)

  ;; fix
  #;
  (defx11* XcmsClientWhitePointOfCCC : _XcmsCCC-pointer -> _XcmsColor)

  (defx11* XExtendedMaxRequestSize : _XDisplay-pointer -> _long)

  (defx11* XSetFont : _XDisplay-pointer _XGC-pointer Font -> _int)

  (defx11* XConnectionNumber : _XDisplay-pointer -> _int)

  (provide XConnectionPort)
  ;; returns an input port that can read the X11 socket
  (define (XConnectionPort display)
    (open-fd-input-port (XConnectionNumber display)))

  ;; starts a thread gets x11 events and passes them to the provided channel
  (provide start-x11-event-thread)
  (define (start-x11-event-thread display channel)
    (let ((port (XConnectionPort display)))
      (thread
        (lambda ()
          (let loop ()
            (sync (handle-evt port (lambda (x)
                                     (channel-put channel (XNextEvent* display))
                                     (loop)))))))))

  ;; possibly fix
  (defx11* XFetchBuffer : _XDisplay-pointer (bytes : (_ptr o _int)) _int ->
	   (data : _pointer) -> (values bytes data))

  (defx11 XSetFontPath : _XDisplay-pointer _pointer _int -> _int) 
  (provide (rename-out (XSetFontPath-user XSetFontPath)))
  ;; paths is list-of(string)
  ;; (XSetFontPath some-display '("dir1" "dir2"))
  (define (XSetFontPath-user display paths)
    (let ((count (length paths))
	  (directories (list->cblock paths _string)))
      (XSetFontPath display directories count)))

  (defx11* XDefaultColormap : _XDisplay-pointer _int -> ColorMap)

  ;; listof(_char)
  (defx11* XFetchBytes : _XDisplay-pointer (bytes : (_ptr o _int))
	   -> (data : _pointer)
	   -> (let ((l (cblock->list data _byte _bytes)))
		(register-finalizer l (lambda (x)
					(XFree x)))
		l))

  ;; fix
  (defx11* XFetchName : _XDisplay-pointer Window (_ptr o _string) -> Status)

  (defx11* XSetFunction : _XDisplay-pointer _XGC-pointer _int -> _int)

  (defx11* XDefaultDepth : _XDisplay-pointer _int -> _int)

  (defx11* XFillArc : _XDisplay-pointer Drawable _XGC-pointer
	   (x : _int) (y : _int)
	   (width : _uint) (height : _uint)
	   (angle1 : _int) (angle2 : _int) -> _int)

  (defx11* XSetGraphicsExposures : _XDisplay-pointer _XGC-pointer
	   _bool -> _int)

  (defx11* XFillArcs : _XDisplay-pointer Drawable _XGC-pointer
	   (_list i _XArc) _int -> _int)

  (defx11* XSetIOErrorHandler : (_fun _XDisplay-pointer -> _int) -> _int)

  (defx11* XDefaultGC : _XDisplay-pointer _int -> _XGC-pointer)

  (defx11* XFillPolygon : _XDisplay-pointer Drawable _XGC-pointer
	   (_list i _XPoint) _int _int _int -> _int)

  (defx11* XSetIconName : _XDisplay-pointer Window _string -> _int)

  (defx11* XSetIconSizes : _XDisplay-pointer Window
	   (_list i _XIconSize) _int -> _int)

  (defx11* XFillRectangles : _XDisplay-pointer Drawable _XGC-pointer
	   (_list i _XRectangle) _int -> _int)

  (defx11* XSetInputFocus : _XDisplay-pointer Window _int Time -> _int)

  (defx11* XFindContext : _XDisplay-pointer XID XContext (_ptr o XPointer)
	   -> _int)

  (defx11* XSetLineAttributes : _XDisplay-pointer _XGC-pointer _ulong
	   _int _int _int -> _int)

  (defx11* XFlush : _XDisplay-pointer -> _int)

  (defx11* XSetModifierMapping : _XDisplay-pointer _XModifierKeymap-pointer
	   -> _int)

  (defx11* XFlushGC : _XDisplay-pointer _XGC-pointer -> _void)

  (defx11* XSetNormalHints : _XDisplay-pointer Window _XSizeHints-pointer
	   -> _int)

  (defx11* XForceScreenSaver : _XDisplay-pointer _int -> _int)

  (defx11* XSetPlaneMask : _XDisplay-pointer _XGC-pointer _ulong -> _int)

  (defx11* XSetPointerMapping : _XDisplay-pointer (_list i _ubyte)
	   (nmap : _int) -> _int)

  (defx11* XFreeColormap : _XDisplay-pointer ColorMap -> _int)

  (defx11* XFreeColors : _XDisplay-pointer ColorMap
	   (_list i _ulong) _int _ulong -> _int)

  (defx11* XSetRGBColormaps : _XDisplay-pointer Window
	   _XStandardColormap-pointer _int Atom -> _void)

  (defx11* XFreeCursor : _XDisplay-pointer Cursor -> _int)

  (defx11* XFreeExtensionList : (_list i _string) -> _int)

  (defx11* XSetRegion : _XDisplay-pointer _XGC-pointer _XRegion-pointer
	   -> _int)

  (defx11* XFreeFont : _XDisplay-pointer _XFontStruct-pointer -> _int)

  (defx11* XSetScreenSaver : _XDisplay-pointer _int _int _int _int -> _int)

  (defx11* XFreeFontInfo : (_list i _string) _XFontStruct-pointer _int
	   -> _int)

  (defx11* XSetSelectionOwner : _XDisplay-pointer Atom Window Time -> _int)

  (defx11* XFreeFontNames : (_list i _string) -> _int)

  (defx11* XSetSizeHints : _XDisplay-pointer Window _XSizeHints-pointer
	   Atom -> _int)

  (defx11* XFreeFontPath : (_list i _string) -> _int)

  (defx11* XSetStandardColormap : _XDisplay-pointer Window
	   _XStandardColormap-pointer Atom -> _int)

  (defx11* XFreeGC : _XDisplay-pointer _XGC-pointer -> _int)

  (defx11* XSetStandardProperties : _XDisplay-pointer Window _string _string
	   Pixmap (_list i _string) _int _XSizeHints-pointer -> _int)

  (defx11* XFreeModifiermap : _XModifierKeymap-pointer -> _int)

  (defx11* XSetState : _XDisplay-pointer _XGC-pointer _ulong _ulong
	   _int _ulong -> _int)

  (defx11* XFreePixmap : _XDisplay-pointer Pixmap -> _int)

  (defx11* XSetSubwindowMode : _XDisplay-pointer _XGC-pointer _int -> _int)

  (defx11* XFreeStringList : (_list i _string) -> _int)

  (defx11* XGContextFromGC  : _XGC-pointer -> GContext)

  (defx11* XSetTextProperty : _XDisplay-pointer Window _XTextProperty-pointer
	   Atom -> _void)

  (defx11* XGeometry : _XDisplay-pointer _int _pointer _pointer _uint _uint _int _int
	   (_ptr o _int) (_ptr o _int)
	   (_ptr o _int) (_ptr o _int) -> _int)

  (defx11* XSetTile : _XDisplay-pointer _XGC-pointer Pixmap -> _int)

  (defx11* XGetAtomName : _XDisplay-pointer Atom -> _string)

  (defx11* XSetTransientForHint : _XDisplay-pointer Window Window -> _int)

  ;; (defx11* XGetAtomNames : _XDisplay-pointer (_list i Atom count) (count : _int) -> (_list o _string count))
  (defx11 XGetAtomNames : _XDisplay-pointer _pointer (c : _int) -> (_list o _string c))

  (provide (rename-out (XGetAtomNames-user XGetAtomNames)))
  (define (XGetAtomNames-user display atoms count)
	   (XGetAtomNames display (list->cblock Atom count) count))

  (defx11* XSetWMClientMachine : _XDisplay-pointer Window _XTextProperty-pointer -> _void)

  (defx11* XGetClassHint : _XDisplay-pointer Window (hint : (_ptr o _XClassHint)) -> (s : Status) -> (values s hint))

  (provide (rename-out [XSetWMColormapWindows-user XSetWMColormapWindows]))
  (defx11 XSetWMColormapWindows : _XDisplay-pointer Window _pointer _int -> Status)
  (define (XSetWMColormapWindows-user display window colormaps count)
    (XSetWMColormapWindows display window (list->cblock colormaps Window count) count))

  (provide (rename-out [XGetCommand-user XGetCommand]))
  (defx11 XGetCommand : _XDisplay-pointer Window (strings : (_ptr o _pointer)) (count : (_ptr o _int)) -> (s : Status) -> (values s strings count))
  (define (XGetCommand-user display window)
    (let-values (((status strings count) (XGetCommand display window)))
      (values status
	      (cblock->list strings _string count)
	      count)))

  (defx11* XSetWMHints : _XDisplay-pointer Window _XWMHints-pointer -> _int)

  (defx11* XGetDefault : _XDisplay-pointer _string _string -> _string)

  (defx11* XSetWMIconName : _XDisplay-pointer Window _XTextProperty-pointer -> _void)

  (provide (rename-out [XGetErrorDatabaseText-user XGetErrorDatabaseText]))
  (defx11 XGetErrorDatabaseText : _XDisplay-pointer _string _string _pointer _int -> _int)
  (define (XGetErrorDatabaseText-user display name message length)
    (let ((str (malloc _byte length)))
      (XGetErrorDatabaseText display name message str length)
      str))

  (defx11* XSetWMName : _XDisplay-pointer Window _XTextProperty-pointer -> _void)

 (provide (rename-out [XGetErrorText-user XGetErrorText]))
 ;(defx11 XGetErrorText : _XDisplay-pointer _int _pointer (length : _int) -> _void)
 (defx11 XGetErrorText : _XDisplay-pointer _int _bytes (length : _int) -> _void)
 (define (XGetErrorText-user display code length)
   ;(let ((str (malloc _byte length)))
   (let ([str (make-bytes length)])
     (XGetErrorText display code str length)
     ;str))
     (make-sized-byte-string str length))) ; ?
 
  (defx11* XSetWMNormalHints : _XDisplay-pointer Window _XSizeHints-pointer -> _void)

  (provide (rename-out [XGetFontPath-user XGetFontPath]))
  (defx11 XGetFontPath : _XDisplay-pointer (paths : (_ptr o _int)) -> (ret : _pointer) -> (values ret paths))
  (define (XGetFontPath-user display)
    (let-values (((block num) (XGetFontPath display)))
      (cblock->list block _string num)))

  (provide (rename-out [XGetFontProperty-user XGetFontProperty]))
  (defx11 XGetFontProperty : _XFontStruct-pointer Atom (value : (_ptr i _int)) -> (ret : _bool) -> (values ret value))
  (define (XGetFontProperty-user font atom)
    (let-values (((ok value) (XGetFontProperty font atom)))
      (if ok value ok)))

  (provide (rename-out [XSetWMProtocols-user XSetWMProtocols]))
  (defx11 XSetWMProtocols : _XDisplay-pointer Window _pointer _int -> _int)
  (define (XSetWMProtocols-user display window protocols)
    (let ((n (length protocols)))
      (XSetWMProtocols display window (list->cblock protocols Atom) n)))

  (defx11* XSetWMSizeHints : _XDisplay-pointer Window _XSizeHints-pointer
	   Atom -> _int)

  (defx11* XSetWindowBackground : _XDisplay-pointer Window _ulong -> _int)

  (defx11* XGetIconName : _XDisplay-pointer Window (str : (_ptr o _string)) ->
	   (status : Status) -> (values status str))

  (defx11* XSetWindowBackgroundPixmap : _XDisplay-pointer Window Pixmap -> _int)

(defx11* _Xmblen : _string _int -> _int)
(defx11* XLoadQueryFont : _XDisplay-pointer _string -> _XFontStruct-pointer)
(defx11* XQueryFont : _XDisplay-pointer _ulong -> _XFontStruct-pointer)
(defx11* XGetMotionEvents : _XDisplay-pointer _ulong _ulong _ulong (_ptr i _int) -> _XTimeCoord-pointer)
(defx11* XDeleteModifiermapEntry : _XModifierKeymap-pointer _ubyte _int -> _XModifierKeymap-pointer)
;@@ XGetModifierMapping
;(defx11* XGetModifierMapping : _XDisplay-pointer -> _XModifierKeymap-pointer)
(defx11* XGetModifierMapping : _XDisplay-pointer -> _XModifierKeymap-pointer/null)
(define* (XModifierKeymap->vector keymap)
  (begin0
    (and keymap
         (cblock->vector (XModifierKeymap-modifiermap keymap) KeyCode
                         (* 8 ;(vector-length keyboard-modifiers)
                            (XModifierKeymap-max-keypermod keymap))))
    ))
;Test:
#;(begin 
    (define d (XOpenDisplay ":0"))
    (define k (XGetModifierMapping d))
    (define mods (XModifierKeymap->vector k)
    (XFreeModifiermap k)))
           
(defx11* XInsertModifiermapEntry : _XModifierKeymap-pointer _ubyte _int -> _XModifierKeymap-pointer)
(defx11* XNewModifiermap : _int -> _XModifierKeymap-pointer)
(defx11* XCreateImage : _XDisplay-pointer _Visual-pointer _uint _int _int _string _uint _uint _int _int -> _XImage-pointer)
(defx11* XInitImage : _XImage-pointer -> _int)
(defx11* XGetImage : _XDisplay-pointer _ulong _int _int _uint _uint _ulong _int -> _XImage-pointer)
(defx11* XGetSubImage : _XDisplay-pointer _ulong _int _int _uint _uint _ulong _int _XImage-pointer _int _int -> _XImage-pointer)
(defx11* XrmInitialize : -> _void)
(defx11* XDisplayName : _string -> _string)
(defx11* XKeysymToString : KeySym -> _string)

;; FIXME
;; Should be ok ; Laurent Orseau -- 2012-10-30
(defx11* XSynchronize : _XDisplay-pointer _bool -> (_fun _XDisplay-pointer -> _int))
(defx11* XSetAfterFunction : _XDisplay-pointer (_fun _XDisplay-pointer -> _int) -> (_fun _XDisplay-pointer -> _int))

(defx11* XInternAtoms : _XDisplay-pointer (_ptr i _string) _int _int (_ptr i _ulong) -> _int)
(defx11* XCopyColormapAndFree : _XDisplay-pointer _ulong -> _ulong)
(defx11* XCreateColormap : _XDisplay-pointer _ulong _Visual-pointer _int -> _ulong)
(defx11* XCreatePixmapCursor : _XDisplay-pointer _ulong _ulong _XColor-pointer _XColor-pointer _uint _uint -> _ulong)
(defx11* XCreateGlyphCursor : _XDisplay-pointer _ulong _ulong _uint _uint _XColor-pointer _XColor-pointer -> _ulong)
(defx11* XCreateFontCursor : _XDisplay-pointer _uint -> _ulong)
(defx11* XLoadFont : _XDisplay-pointer _string -> _ulong)
(defx11* XCreatePixmap : _XDisplay-pointer _ulong _uint _uint _uint -> _ulong)
(defx11* XCreatePixmapFromBitmapData : _XDisplay-pointer _ulong _string _uint _uint _ulong _ulong _uint -> _ulong)
(defx11* XCreateSimpleWindow : _XDisplay-pointer _ulong _int _int _uint _uint _uint _ulong _ulong -> Window)
(defx11* XGetSelectionOwner : _XDisplay-pointer _ulong -> Window)
(defx11* XCreateWindow : _XDisplay-pointer Window (x : _int) (y : _int) (width : _uint) (height : _uint) (border-width : _uint) (depth : _int) InputType _Visual-pointer/null ChangeWindowAttributes _XSetWindowAttributes-pointer/null -> Window)
(defx11* XListInstalledColormaps : _XDisplay-pointer _ulong (_ptr i _int) -> (_ptr i _ulong))
(defx11* XListFonts : _XDisplay-pointer _string _int (_ptr i _int) -> (_ptr i _string))
(defx11* XListFontsWithInfo : _XDisplay-pointer _string _int (_ptr i _int) _pointer -> (_ptr i _string))
(defx11* XListExtensions : _XDisplay-pointer (_ptr i _int) -> (_ptr i _string))
(defx11* XListProperties : _XDisplay-pointer _ulong (_ptr i _int) -> (_ptr i _ulong))
(defx11* XListHosts : _XDisplay-pointer (_ptr i _int) (_ptr i _int) -> _XHostAddress-pointer)
(defx11* XKeycodeToKeysym : _XDisplay-pointer _ubyte _int -> KeySym)
(defx11* XLookupKeysym : _XKeyEvent-pointer _int -> KeySym)
(defx11* XGetKeyboardMapping : _XDisplay-pointer _ubyte _int (_ptr i _int) -> (_ptr i _ulong))
;(defx11* XStringToKeysym : _string -> _ulong)
(defx11* XStringToKeysym : _string -> KeySym)
(defx11* XMaxRequestSize : _XDisplay-pointer -> _long)
(defx11* XResourceManagerString : _XDisplay-pointer -> _string)
(defx11* XScreenResourceString : _Screen-pointer -> _string)
(defx11* XDisplayMotionBufferSize : _XDisplay-pointer -> _ulong)
(defx11* XVisualIDFromVisual : _Visual-pointer -> _ulong)
(defx11* XInitThreads : -> _int)
(defx11* XLockDisplay : _XDisplay-pointer -> _void)
(defx11* XUnlockDisplay : _XDisplay-pointer -> _void)
(defx11* XInitExtension : _XDisplay-pointer _string -> _XExtCodes-pointer)
(defx11* XAddExtension : _XDisplay-pointer -> _XExtCodes-pointer)
(defx11* XFindOnExtensionList : _pointer _int -> _XExtData-pointer)

;;(defx11* XEHeadOfExtensionList : _comp -> _pointer)

(defx11* XRootWindow : _XDisplay-pointer _int -> Window)
(defx11* XDefaultRootWindow : _XDisplay-pointer -> Window)
(defx11* XRootWindowOfScreen : _Screen-pointer -> Window)
(defx11* XDefaultVisual : _XDisplay-pointer _int -> _Visual-pointer)
(defx11* XDefaultVisualOfScreen : _Screen-pointer -> _Visual-pointer)
(defx11* XDefaultGCOfScreen : _Screen-pointer -> _XGC-pointer)
(defx11* XBlackPixel : _XDisplay-pointer _int -> _ulong)
(defx11* XWhitePixel : _XDisplay-pointer _int -> _ulong)
(defx11* XBlackPixelOfScreen : _Screen-pointer -> _ulong)
(defx11* XWhitePixelOfScreen : _Screen-pointer -> _ulong)
(defx11* XNextRequest : _XDisplay-pointer -> _ulong)
(defx11* XLastKnownRequestProcessed : _XDisplay-pointer -> _ulong)
(defx11* XServerVendor : _XDisplay-pointer -> _string)
(defx11* XDisplayString : _XDisplay-pointer -> _string)
(defx11* XDefaultColormapOfScreen : _Screen-pointer -> _ulong)
(defx11* XDisplayOfScreen : _Screen-pointer -> _XDisplay-pointer)
(defx11* XScreenOfDisplay : _XDisplay-pointer _int -> _Screen-pointer)
(defx11* XDefaultScreenOfDisplay : _XDisplay-pointer -> _Screen-pointer)
(defx11* XEventMaskOfScreen : _Screen-pointer -> _long)
(defx11* XScreenNumberOfScreen : _Screen-pointer -> _int)
(defx11* XListPixmapFormats : _XDisplay-pointer (_ptr i _int) -> _XPixmapFormatValues-pointer)
(defx11* XListDepths : _XDisplay-pointer _int (_ptr i _int) -> (_ptr i _int))
(defx11* XReconfigureWMWindow : _XDisplay-pointer _ulong _int _uint _XWindowChanges-pointer -> _int)
(defx11* XGetWMProtocols : _XDisplay-pointer _ulong _pointer (_ptr i _int) -> _int)
(defx11* XIconifyWindow : _XDisplay-pointer Window _int -> _int)
(defx11* XWithdrawWindow : _XDisplay-pointer Window _int -> _int)
(defx11* XGetWMColormapWindows : _XDisplay-pointer Window _pointer (_ptr i _int) -> _int)
(defx11* XActivateScreenSaver : _XDisplay-pointer -> _int)
(defx11* XAddHost : _XDisplay-pointer _XHostAddress-pointer -> _int)
(defx11* XAddHosts : _XDisplay-pointer _XHostAddress-pointer _int -> _int)
(defx11* XAddToExtensionList : _pointer _XExtData-pointer -> _int)
(defx11* XAddToSaveSet : _XDisplay-pointer _ulong -> _int)
(defx11* XAllocColor : _XDisplay-pointer _ulong _XColor-pointer -> _int)
(defx11* XAllocColorCells : _XDisplay-pointer _ulong _int (_ptr i _ulong) _uint (_ptr i _ulong) _uint -> _int)
(defx11* XAllocColorPlanes : _XDisplay-pointer _ulong _int (_ptr i _ulong) _int _int _int _int (_ptr i _ulong) (_ptr i _ulong) (_ptr i _ulong) -> _int)
(defx11* XAllowEvents : _XDisplay-pointer _int _ulong -> _int)
(defx11* XAutoRepeatOff : _XDisplay-pointer -> _int)
(defx11* XAutoRepeatOn : _XDisplay-pointer -> _int)
(defx11* XBell : _XDisplay-pointer _int -> _int)
(defx11* XCellsOfScreen : _Screen-pointer -> _int)
(defx11* XChangeActivePointerGrab : _XDisplay-pointer _uint _ulong _ulong -> _int)
(defx11* XChangeGC : _XDisplay-pointer _XGC-pointer _ulong _XGCValues-pointer -> _int)
(defx11* XChangeKeyboardControl : _XDisplay-pointer _ulong _XKeyboardControl-pointer -> _int)
(defx11* XChangeKeyboardMapping : _XDisplay-pointer _int _int (_ptr i _ulong) _int -> _int)
(defx11* XChangePointerControl : _XDisplay-pointer _int _int _int _int _int -> _int)
(defx11* XChangeProperty : _XDisplay-pointer _ulong _ulong _ulong _int _int (_ptr i _ubyte) _int -> _int)
(defx11* XChangeSaveSet : _XDisplay-pointer _ulong _int -> _int)
(defx11* XChangeWindowAttributes : _XDisplay-pointer Window ChangeWindowAttributes _XSetWindowAttributes-pointer -> _int)

(defx11* XCheckIfEvent : _XDisplay-pointer _XEvent-pointer (_fun _XDisplay-pointer _XEvent-pointer _pointer -> _bool) _string -> _int)

(defx11* XCheckMaskEvent : _XDisplay-pointer _long _XEvent-pointer -> _int)
;(defx11* XCheckTypedEvent : _XDisplay-pointer _int _XEvent-pointer -> _int)
(defx11* XCheckTypedEvent : _XDisplay-pointer EventType _XEvent-pointer -> _int)
(defx11* XCheckTypedWindowEvent : _XDisplay-pointer Window EventType _XEvent-pointer -> _int)
(defx11* XCheckWindowEvent : _XDisplay-pointer Window _long _XEvent-pointer -> _int)
(defx11* XCirculateSubwindows : _XDisplay-pointer Window _int -> _int)
(defx11* XCirculateSubwindowsDown : _XDisplay-pointer Window -> _int)
(defx11* XCirculateSubwindowsUp : _XDisplay-pointer Window -> _int)
(defx11* XClearWindow : _XDisplay-pointer Window -> _int)
(defx11* XCloseDisplay : _XDisplay-pointer -> _int)
(defx11* XConfigureWindow : _XDisplay-pointer Window WindowChanges _XWindowChanges-pointer/null -> _int)
(defx11* XConvertSelection : _XDisplay-pointer _ulong _ulong _ulong Window Time -> _int)
(defx11* XCopyPlane : _XDisplay-pointer _ulong _ulong _XGC-pointer _int _int _uint _uint _int _int _ulong -> _int)
(defx11* XDefaultDepthOfScreen : _Screen-pointer -> _int)
(defx11* XDefaultScreen : _XDisplay-pointer -> _int)
(defx11* XDefineCursor : _XDisplay-pointer Window _ulong -> _int)
(defx11* XDeleteProperty : _XDisplay-pointer Window _ulong -> _int)
;(defx11* XDestroyWindow : _XDisplay-pointer _ulong -> _int)
(defx11* XDestroyWindow : _XDisplay-pointer Window -> _int)
(defx11* XDestroySubwindows : _XDisplay-pointer Window -> _int)
(defx11* XDoesBackingStore : _Screen-pointer -> _int)
(defx11* XDoesSaveUnders : _Screen-pointer -> _int)
(defx11* XDisableAccessControl : _XDisplay-pointer -> _int)
(defx11* XDisplayCells : _XDisplay-pointer _int -> _int)
(defx11* XDisplayHeight : _XDisplay-pointer _int -> _int)
(defx11* XDisplayHeightMM : _XDisplay-pointer _int -> _int)
(defx11* XDisplayKeycodes : _XDisplay-pointer (_ptr i _int) (_ptr i _int) -> _int)
(defx11* XDisplayPlanes : _XDisplay-pointer _int -> _int)
(defx11* XDisplayWidth : _XDisplay-pointer _int -> _int)
(defx11* XDisplayWidthMM : _XDisplay-pointer _int -> _int)
(defx11* XDrawArc : _XDisplay-pointer _ulong _XGC-pointer _int _int _uint _uint _int _int -> _int)
(defx11* XDrawArcs : _XDisplay-pointer _ulong _XGC-pointer _XArc-pointer _int -> _int)
(defx11* XDrawImageString : _XDisplay-pointer _ulong _XGC-pointer _int _int _string _int -> _int)
(defx11* XDrawImageString16 : _XDisplay-pointer _ulong _XGC-pointer _int _int _XChar2b-pointer _int -> _int)
(defx11* XDrawLine : _XDisplay-pointer _ulong _XGC-pointer _int _int _int _int -> _int)
(defx11* XDrawLines : _XDisplay-pointer _ulong _XGC-pointer _XPoint-pointer _int _int -> _int)
(defx11* XDrawPoint : _XDisplay-pointer _ulong _XGC-pointer _int _int -> _int)
(defx11* XDrawPoints : _XDisplay-pointer _ulong _XGC-pointer _XPoint-pointer _int _int -> _int)
(defx11* XDrawRectangle : _XDisplay-pointer _ulong _XGC-pointer _int _int _uint _uint -> _int)
(defx11* XDrawRectangles : _XDisplay-pointer _ulong _XGC-pointer _XRectangle-pointer _int -> _int)
(defx11* XDrawSegments : _XDisplay-pointer _ulong _XGC-pointer _XSegment-pointer _int -> _int)
(defx11* XDrawString : _XDisplay-pointer _ulong _XGC-pointer _int _int _string _int -> _int)
(defx11* XDrawString16 : _XDisplay-pointer _ulong _XGC-pointer _int _int _XChar2b-pointer _int -> _int)
(defx11* XDrawText : _XDisplay-pointer _ulong _XGC-pointer _int _int _XTextItem-pointer _int -> _int)
(defx11* XDrawText16 : _XDisplay-pointer _ulong _XGC-pointer _int _int _XTextItem16-pointer _int -> _int)
;(defx11* XGetInputFocus : _XDisplay-pointer (_ptr i _ulong) (_ptr i _int) -> _int)
(defx11* XGetInputFocus : _XDisplay-pointer (window : (_ptr o Window)) (revert-to : (_ptr o _int)) -> _int
  -> (list window revert-to))
(defx11* XGetKeyboardControl : _XDisplay-pointer _XKeyboardState-pointer -> _int)
(defx11* XGetPointerControl : _XDisplay-pointer (_ptr i _int) (_ptr i _int) (_ptr i _int) -> _int)
(defx11* XGetPointerMapping : _XDisplay-pointer (_ptr i _ubyte) _int -> _int)
(defx11* XGetScreenSaver : _XDisplay-pointer (_ptr i _int) (_ptr i _int) (_ptr i _int) (_ptr i _int) -> _int)
(defx11* XGetTransientForHint : _XDisplay-pointer _ulong (_ptr i _ulong) -> _int)
;(defx11* XGrabButton : _XDisplay-pointer XK-Pointer Modifiers Window _bool InputMask GrabMode GrabMode Window Cursor -> _int)
; apparently the button number is not an XK-Pointer...
(defx11* XGrabButton : _XDisplay-pointer _uint Modifiers Window _bool InputMask GrabMode GrabMode Window Cursor -> _void)
;(defx11* XGrabKey : _XDisplay-pointer _int _uint _ulong _int _int _int -> _int)
(defx11* XGrabKey : _XDisplay-pointer KeyCode Modifiers Window _bool GrabMode GrabMode -> _void)
; can generate BadAccess , BadValue , and BadWindow errors. 
(defx11* XGrabKeyboard : _XDisplay-pointer _ulong _int _int _int _ulong -> _int)
;@@ XGrabPointer
(define xgrabpointer-return-code #(GrabSuccess AlreadyGrabbed GrabInvalidTime GrabNotViewable GrabFrozen))
(defx11* XGrabPointer : _XDisplay-pointer Window _bool InputMask GrabMode GrabMode Window Cursor Time 
  -> (ret : _int)
  -> (if-debug (begin (when (> ret 0) 
                        (x11-dprintf "Error: XGrabPointer failed with code ~a: ~a\n" ret
                                     (vector-ref xgrabpointer-return-code ret)))
                      ret)
               ret))
(defx11* XHeightMMOfScreen : _Screen-pointer -> _int)
(defx11* XHeightOfScreen : _Screen-pointer -> _int)

(defx11* XIfEvent : _XDisplay-pointer _XEvent-pointer (_fun _XDisplay-pointer _XEvent-pointer _pointer -> _bool) _pointer -> _int)

(defx11* XImageByteOrder : _XDisplay-pointer -> _int)
(defx11* XInstallColormap : _XDisplay-pointer _ulong -> _int)
;(defx11* XKeysymToKeycode : _XDisplay-pointer _ulong -> _ubyte)
(defx11* XKeysymToKeycode : _XDisplay-pointer KeySym -> KeyCode)
(defx11* XKillClient : _XDisplay-pointer _ulong -> _int)
(defx11* XLookupColor : _XDisplay-pointer _ulong _string _XColor-pointer _XColor-pointer -> _int)
(defx11* XLowerWindow : _XDisplay-pointer Window -> _int)
(defx11* XMapRaised : _XDisplay-pointer Window -> _int)
(defx11* XMapSubwindows : _XDisplay-pointer Window -> _int)
(defx11* XMapWindow : _XDisplay-pointer Window -> _int)
(defx11* XMaskEvent : _XDisplay-pointer _long _XEvent-pointer -> _int)
(defx11* XMaxCmapsOfScreen : _Screen-pointer -> _int)
(defx11* XMinCmapsOfScreen : _Screen-pointer -> _int)
;(defx11* XMoveResizeWindow : _XDisplay-pointer _ulong _int _int _uint _uint -> _int)
(defx11* XMoveResizeWindow : _XDisplay-pointer Window _int _int _uint _uint -> _int)
(defx11* XMoveWindow : _XDisplay-pointer Window _int _int -> _int)
(defx11* XNoOp : _XDisplay-pointer -> _int)
(defx11* XParseColor : _XDisplay-pointer _ulong _string _XColor-pointer -> _int)
(defx11* XParseGeometry : _string (_ptr i _int) (_ptr i _int) (_ptr i _uint) (_ptr i _uint) -> _int)
(defx11* XPeekEvent : _XDisplay-pointer _XEvent-pointer -> _int)
;(defx11* XPeekEvent : _XDisplay-pointer (event : (_ptr o _XEvent)) -> _int -> event) ; ??

(defx11* XPeekIfEvent : _XDisplay-pointer _XEvent-pointer (_fun _XDisplay-pointer _XEvent-pointer _pointer -> _bool) _pointer -> _int)

(defx11* XPlanesOfScreen : _Screen-pointer -> _int)
(defx11* XProtocolRevision : _XDisplay-pointer -> _int)
(defx11* XProtocolVersion : _XDisplay-pointer -> _int)
(defx11* XPutBackEvent : _XDisplay-pointer _XEvent-pointer -> _int)
(defx11* XPutImage : _XDisplay-pointer _ulong _XGC-pointer _XImage-pointer _int _int _int _int _uint _uint -> _int)
(defx11* XQLength : _XDisplay-pointer -> _int)
(defx11* XQueryBestCursor : _XDisplay-pointer _ulong _uint _uint (_ptr i _uint) (_ptr i _uint) -> _int)
(defx11* XQueryBestSize : _XDisplay-pointer _int _ulong _uint _uint (_ptr i _uint) (_ptr i _uint) -> _int)
(defx11* XQueryBestStipple : _XDisplay-pointer _ulong _uint _uint (_ptr i _uint) (_ptr i _uint) -> _int)
(defx11* XQueryBestTile : _XDisplay-pointer _ulong _uint _uint (_ptr i _uint) (_ptr i _uint) -> _int)
(defx11* XQueryColor : _XDisplay-pointer _ulong _XColor-pointer -> _int)
(defx11* XQueryColors : _XDisplay-pointer _ulong _XColor-pointer _int -> _int)
(defx11* XQueryExtension : _XDisplay-pointer _string (_ptr i _int) (_ptr i _int) (_ptr i _int) -> _int)
(defx11* XQueryKeymap : _XDisplay-pointer _string -> _int)

;; ticket 187: http://planet.plt-scheme.org/trac/ticket/187
;; (defx11* XQueryPointer : _XDisplay-pointer _ulong (_ptr i _ulong) (_ptr i _ulong) (_ptr i _int) (_ptr i _int) (_ptr i _int) (_ptr i _int) (_ptr i _uint) -> _int)
(defx11* XQueryPointer : _XDisplay-pointer
         Window
         (root : (_ptr o Window))
         (child : (_ptr o Window))
         (x : (_ptr o _int))
         (y : (_ptr o _int))
         (win-x : (_ptr o _int))
         (win-y : (_ptr o _int))
         (mask : (_ptr o _uint))
         -> (rc : _int) -> (values rc root child x y win-x win-y mask))

(defx11* XQueryTextExtents : _XDisplay-pointer _ulong _string _int (_ptr i _int) (_ptr i _int) (_ptr i _int) _XCharStruct-pointer -> _int)
(defx11* XQueryTextExtents16 : _XDisplay-pointer _ulong _XChar2b-pointer _int (_ptr i _int) (_ptr i _int) (_ptr i _int) _XCharStruct-pointer -> _int)
(defx11* XRaiseWindow : _XDisplay-pointer Window -> _int)
(defx11* XReadBitmapFile : _XDisplay-pointer _ulong _string (_ptr i _uint) (_ptr i _uint) (_ptr i _ulong) (_ptr i _int) (_ptr i _int) -> _int)
(defx11* XReadBitmapFileData : _string (_ptr i _uint) (_ptr i _uint) _pointer (_ptr i _int) (_ptr i _int) -> _int)
(defx11* XRebindKeysym : _XDisplay-pointer _ulong (_ptr i _ulong) _int (_ptr i _ubyte) _int -> _int)
(defx11* XRecolorCursor : _XDisplay-pointer _ulong _XColor-pointer _XColor-pointer -> _int)
(defx11* XRefreshKeyboardMapping : _XMappingEvent-pointer -> _int)
(defx11* XRemoveFromSaveSet : _XDisplay-pointer Window -> _int)
(defx11* XRemoveHost : _XDisplay-pointer _XHostAddress-pointer -> _int)
(defx11* XRemoveHosts : _XDisplay-pointer _XHostAddress-pointer _int -> _int)
(defx11* XReparentWindow : _XDisplay-pointer Window Window _int _int -> _int)
(defx11* XResetScreenSaver : _XDisplay-pointer -> _int)
(defx11* XResizeWindow : _XDisplay-pointer Window _uint _uint -> _int)
(defx11* XRestackWindows : _XDisplay-pointer (_ptr i Window) _int -> _int) ; we should give a list of windows here
(defx11* XRotateBuffers : _XDisplay-pointer _int -> _int)
(defx11* XRotateWindowProperties : _XDisplay-pointer Window (_ptr i _ulong) _int _int -> _int)
(defx11* XScreenCount : _XDisplay-pointer -> _int)
(defx11* XSendEvent : _XDisplay-pointer Window _int _long _XEvent-pointer -> _int)
(defx11* XSetAccessControl : _XDisplay-pointer _int -> _int)
(defx11* XSetArcMode : _XDisplay-pointer _XGC-pointer _int -> _int)
(defx11* XSetBackground : _XDisplay-pointer _XGC-pointer _ulong -> _int)
(defx11* XSetWindowBorder : _XDisplay-pointer Window _ulong -> _int)
(defx11* XSetWindowBorderPixmap : _XDisplay-pointer Window _ulong -> _int)
(defx11* XSetWindowBorderWidth : _XDisplay-pointer Window _uint -> _int)
(defx11* XSetWindowColormap : _XDisplay-pointer Window _ulong -> _int)
(defx11* XStoreBuffer : _XDisplay-pointer _string _int _int -> _int)
(defx11* XStoreBytes : _XDisplay-pointer _string _int -> _int)
(defx11* XStoreColor : _XDisplay-pointer _ulong _XColor-pointer -> _int)
(defx11* XStoreColors : _XDisplay-pointer _ulong _XColor-pointer _int -> _int)
(defx11* XStoreName : _XDisplay-pointer _ulong _string -> _int)
(defx11* XStoreNamedColor : _XDisplay-pointer _ulong _string _ulong _int -> _int)
(defx11* XSync : _XDisplay-pointer _bool -> _int)
(defx11* XTextExtents : _XFontStruct-pointer _string _int (_ptr i _int) (_ptr i _int) (_ptr i _int) _XCharStruct-pointer -> _int)
(defx11* XTextExtents16 : _XFontStruct-pointer _XChar2b-pointer _int (_ptr i _int) (_ptr i _int) (_ptr i _int) _XCharStruct-pointer -> _int)
(defx11* XTextWidth : _XFontStruct-pointer _string _int -> _int)
(defx11* XTextWidth16 : _XFontStruct-pointer _XChar2b-pointer _int -> _int)
(defx11* XTranslateCoordinates : _XDisplay-pointer _ulong _ulong _int _int (_ptr i _int) (_ptr i _int) (_ptr i _ulong) -> _int)
(defx11* XUndefineCursor : _XDisplay-pointer _ulong -> _int)
(defx11* XUngrabButton : _XDisplay-pointer XK-Pointer Modifiers Window -> _int)
(defx11* XUngrabKey : _XDisplay-pointer _int _uint _ulong -> _int)
(defx11* XUngrabKeyboard : _XDisplay-pointer _ulong -> _int)
;(defx11* XUngrabPointer : _XDisplay-pointer _ulong -> _int)
(defx11* XUngrabPointer : _XDisplay-pointer Time -> _int)
(defx11* XUninstallColormap : _XDisplay-pointer _ulong -> _int)
(defx11* XUnloadFont : _XDisplay-pointer _ulong -> _int)
(defx11* XUnmapSubwindows : _XDisplay-pointer Window -> _int)
(defx11* XUnmapWindow : _XDisplay-pointer Window -> _int)
(defx11* XVendorRelease : _XDisplay-pointer -> _int)
(defx11* XWarpPointer : _XDisplay-pointer Window Window _int _int _uint _uint _int _int -> _int)
(defx11* XWidthMMOfScreen : _Screen-pointer -> _int)
(defx11* XWidthOfScreen : _Screen-pointer -> _int)
(defx11* XWindowEvent : _XDisplay-pointer Window _long _XEvent-pointer -> _int)
(defx11* XWriteBitmapFile : _XDisplay-pointer _string _ulong _uint _uint _int _int -> _int)
(defx11* XSupportsLocale : -> _int)
(defx11* XSetLocaleModifiers : _string -> _string)
(defx11* XOpenOM : _XDisplay-pointer _XrmHashBucketRec-pointer _string _string -> _XOM-pointer)
(defx11* XCloseOM : _XOM-pointer -> _int)
(defx11* XSetOMValues : _XOM-pointer -> _string)
(defx11* XGetOMValues : _XOM-pointer -> _string)
(defx11* XDisplayOfOM : _XOM-pointer -> _XDisplay-pointer)
(defx11* XLocaleOfOM : _XOM-pointer -> _string)
(defx11* XCreateOC : _XOM-pointer -> _XOC-pointer)
(defx11* XDestroyOC : _XOC-pointer -> _void)
(defx11* XOMOfOC : _XOC-pointer -> _XOM-pointer)
(defx11* XSetOCValues : _XOC-pointer -> _string)
(defx11* XGetOCValues : _XOC-pointer -> _string)
(defx11* XCreateFontSet : _XDisplay-pointer _string _pointer (_ptr i _int) (_ptr i _string) -> _XOC-pointer)
(defx11* XFreeFontSet : _XDisplay-pointer _XOC-pointer -> _void)
(defx11* XFontsOfFontSet : _XOC-pointer _pointer _pointer -> _int)
(defx11* XBaseFontNameListOfFontSet : _XOC-pointer -> _string)
(defx11* XLocaleOfFontSet : _XOC-pointer -> _string)
(defx11* XContextDependentDrawing : _XOC-pointer -> _int)
(defx11* XDirectionalDependentDrawing : _XOC-pointer -> _int)
(defx11* XContextualDrawing : _XOC-pointer -> _int)
(defx11* XExtentsOfFontSet : _XOC-pointer -> _XFontSetExtents-pointer)
(defx11* XmbTextEscapement : _XOC-pointer _string _int -> _int)
(defx11* XwcTextEscapement : _XOC-pointer (_ptr i _int) _int -> _int)
(defx11* Xutf8TextEscapement : _XOC-pointer _string _int -> _int)
(defx11* XmbTextExtents : _XOC-pointer _string _int _XRectangle-pointer _XRectangle-pointer -> _int)
(defx11* XwcTextExtents : _XOC-pointer (_ptr i _int) _int _XRectangle-pointer _XRectangle-pointer -> _int)
(defx11* Xutf8TextExtents : _XOC-pointer _string _int _XRectangle-pointer _XRectangle-pointer -> _int)
(defx11* XmbTextPerCharExtents : _XOC-pointer _string _int _XRectangle-pointer _XRectangle-pointer _int (_ptr i _int) _XRectangle-pointer _XRectangle-pointer -> _int)
(defx11* XwcTextPerCharExtents : _XOC-pointer (_ptr i _int) _int _XRectangle-pointer _XRectangle-pointer _int (_ptr i _int) _XRectangle-pointer _XRectangle-pointer -> _int)
(defx11* Xutf8TextPerCharExtents : _XOC-pointer _string _int _XRectangle-pointer _XRectangle-pointer _int (_ptr i _int) _XRectangle-pointer _XRectangle-pointer -> _int)

(defx11* XmbDrawText           : _XDisplay-pointer _ulong _XGC-pointer _int _int _XmbTextItem-pointer _int -> _void)
(defx11* XwcDrawText           : _XDisplay-pointer _ulong _XGC-pointer _int _int _XwcTextItem-pointer _int -> _void)
(defx11* Xutf8DrawText         : _XDisplay-pointer _ulong _XGC-pointer _int _int _XmbTextItem-pointer _int -> _void)
(defx11* XmbDrawString         : _XDisplay-pointer _ulong _XOC-pointer _XGC-pointer _int _int _string _int -> _void)
(defx11* XwcDrawString         : _XDisplay-pointer _ulong _XOC-pointer _XGC-pointer _int _int (_ptr i _int) _int -> _void)
(defx11* Xutf8DrawString       : _XDisplay-pointer _ulong _XOC-pointer _XGC-pointer _int _int _string _int -> _void)
(defx11* XmbDrawImageString    : _XDisplay-pointer _ulong _XOC-pointer _XGC-pointer _int _int _string _int -> _void)
(defx11* XwcDrawImageString    : _XDisplay-pointer _ulong _XOC-pointer _XGC-pointer _int _int (_ptr i _int) _int -> _void)
(defx11* Xutf8DrawImageString  : _XDisplay-pointer _ulong _XOC-pointer _XGC-pointer _int _int _string _int -> _void)

(defx11* XOpenIM : _XDisplay-pointer _XrmHashBucketRec-pointer _string _string -> _XIM-pointer)

(defx11* XCloseIM : _XIM-pointer -> _int)
(defx11* XGetIMValues : _XIM-pointer -> _string)
(defx11* XSetIMValues : _XIM-pointer -> _string)
(defx11* XDisplayOfIM : _XIM-pointer -> _XDisplay-pointer)
(defx11* XLocaleOfIM : _XIM-pointer -> _string)
(defx11* XCreateIC : _XIM-pointer -> _XIC-pointer)
(defx11* XDestroyIC : _XIC-pointer -> _void)
(defx11* XSetICFocus : _XIC-pointer -> _void)
(defx11* XUnsetICFocus : _XIC-pointer -> _void)
(defx11* XwcResetIC : _XIC-pointer -> (_ptr i _int))
(defx11* XmbResetIC : _XIC-pointer -> _string)
(defx11* Xutf8ResetIC : _XIC-pointer -> _string)
(defx11* XSetICValues : _XIC-pointer -> _string)
(defx11* XGetICValues : _XIC-pointer -> _string)
(defx11* XIMOfIC : _XIC-pointer -> _XIM-pointer)
(defx11* XFilterEvent : _XEvent-pointer Window -> _int)
(defx11* XmbLookupString : _XIC-pointer _XKeyEvent-pointer _string _int (_ptr i _ulong) (_ptr i _int) -> _int)
(defx11* XwcLookupString : _XIC-pointer _XKeyEvent-pointer (_ptr i _int) _int (_ptr i _ulong) (_ptr i _int) -> _int)
(defx11* Xutf8LookupString : _XIC-pointer _XKeyEvent-pointer _string _int (_ptr i _ulong) (_ptr i _int) -> _int)
(defx11* XVaCreateNestedList : _int -> (_ptr i _void))
(defx11* XRegisterIMInstantiateCallback : _XDisplay-pointer _XrmHashBucketRec-pointer _string _string (_fun _XDisplay-pointer _pointer _pointer -> _void) _string -> _int)
(defx11* XUnregisterIMInstantiateCallback : _XDisplay-pointer _XrmHashBucketRec-pointer _string _string (_fun _XDisplay-pointer _pointer _pointer -> _void) _string -> _int)
(defx11* XInternalConnectionNumbers : _XDisplay-pointer _pointer (_ptr i _int) -> _int)
(defx11* XProcessInternalConnection : _XDisplay-pointer _int -> _void)
(defx11* XAddConnectionWatch : _XDisplay-pointer (_fun _XDisplay-pointer _pointer _int _bool _pointer -> _void) _pointer -> _int)
(defx11* XRemoveConnectionWatch : _XDisplay-pointer (_fun _XDisplay-pointer _pointer _int _bool _pointer -> _void) _pointer -> _void)
(defx11* XSetAuthorization : _string _int _string _int -> _void)
(defx11* _Xmbtowc : (_ptr i _int) _string _int -> _int)
(defx11* _Xwctomb : _string _int -> _int)

(defx11* XGetIconSizes : _XDisplay-pointer _ulong _pointer (_ptr o _int) -> _int)
(defx11* XSetZoomHints : _XDisplay-pointer _ulong _XSizeHints-pointer -> _int)
(defx11* XShrinkRegion : _XRegion-pointer _int _int -> _int)
(defx11* XGetNormalHints : _XDisplay-pointer _ulong _XSizeHints-pointer -> _int)

(defx11* XGetPixel : _XImage-pointer _int _int -> _ulong)

(defx11* XGetRGBColormaps : _XDisplay-pointer _ulong _pointer (_ptr i _int) _ulong -> _int)

(defx11* XStringListToTextProperty : (_ptr i _string) _int _XTextProperty-pointer -> _int)

(defx11* XSubImage : _XImage-pointer _int _int _uint _uint -> _XImage-pointer)

(defx11* XGetSizeHints : _XDisplay-pointer _ulong _XSizeHints-pointer _ulong -> _int)

(defx11* XGetStandardColormap : _XDisplay-pointer _ulong _XStandardColormap-pointer _ulong -> _int)

(defx11* XGetTextProperty : _XDisplay-pointer _ulong _XTextProperty-pointer _ulong -> _int)
(defx11* XGetVisualInfo : _XDisplay-pointer _long _XVisualInfo-pointer (_ptr i _int) -> _XVisualInfo-pointer)

(defx11* XGetWMClientMachine : _XDisplay-pointer _ulong _XTextProperty-pointer -> _int) 
;@@ XTextPropertyToStringList
(defx11* XTextPropertyToStringList : 
  _XTextProperty-pointer (lstr : (_ptr o _pointer)) (count : (_ptr o _int))
  -> _int
  -> (let ([out (cblock->list lstr _string count)])
       (register-finalizer out (lambda (c) (XFreeStringList c)))
       out))
; Status XTextPropertyToStringList(text_prop, list_return, count_return)
;       XTextProperty *text_prop;
;       char ***list_return;
;       int *count_return;


(defx11* XAddPixel               : _XImage-pointer _long -> _int)
(defx11* XGetWMHints             : _XDisplay-pointer Window -> _XWMHints-pointer)
(defx11* XGetWMIconName          : _XDisplay-pointer Window _XTextProperty-pointer -> _int)
(defx11* XAllocClassHint         : -> _XClassHint-pointer)
;(defx11* XGetWMName              : _XDisplay-pointer Window _XTextProperty-pointer -> _int) ; -> _bool ? or even #f or a XTexProperty data?
(defx11* XGetWMName              : _XDisplay-pointer Window (prop : (_ptr o _XTextProperty)) -> (status : _int) 
  -> (and (> status 0) prop))
(defx11* XGetWMNormalHints       : _XDisplay-pointer Window _XSizeHints-pointer (_ptr i _long) -> _int)
(defx11* XGetWMSizeHints         : _XDisplay-pointer Window _XSizeHints-pointer (_ptr i _long) _ulong -> _int)
(defx11* XAllocIconSize          : -> _XIconSize-pointer)
(defx11* XAllocSizeHints         : -> _XSizeHints-pointer)
(defx11* XAllocStandardColormap  : -> _XStandardColormap-pointer)
(defx11* XGetZoomHints           : _XDisplay-pointer _ulong _XSizeHints-pointer -> _int)
(defx11* XAllocWMHints           : -> _XWMHints-pointer)
(defx11* XUnionRegion            : _XRegion-pointer _XRegion-pointer _XRegion-pointer -> _int)

(defx11* XrmUniqueQuark : -> XrmQuark)
(define* XUniqueContext XrmUniqueQuark)

(defx11* XWMGeometry                : _XDisplay-pointer _int _string _string _uint _XSizeHints-pointer (_ptr i _int) (_ptr i _int) (_ptr i _int) (_ptr i _int) (_ptr i _int) -> _int)
(defx11* XXorRegion                 : _XRegion-pointer _XRegion-pointer _XRegion-pointer -> _int)
(defx11* XIntersectRegion           : _XRegion-pointer _XRegion-pointer _XRegion-pointer -> _int)
(defx11* XClipBox                   : _XRegion-pointer _XRectangle-pointer -> _int)
(defx11* XConvertCase               : _ulong (_ptr i _ulong) (_ptr i _ulong) -> _void)
;; (defx11* XLookUpAssoc            : -> _string)
(defx11* XLookupString              : _XKeyEvent-pointer _string _int (_ptr i _ulong) _XComposeStatus-pointer -> _int)
(defx11* XMatchVisualInfo           : _XDisplay-pointer _int _int _int _XVisualInfo-pointer -> _int)
(defx11* XOffsetRegion              : _XRegion-pointer _int _int -> _int)
(defx11* XDefaultString             : -> _string)
(defx11* XPolygonRegion             : _XPoint-pointer _int _int -> _XRegion-pointer)
(defx11* XDeleteContext             : _XDisplay-pointer _ulong _int -> _int)
(defx11* XDestroyImage              : _XImage-pointer -> _int)
(defx11* XPutPixel                  : _XImage-pointer _int _int _ulong -> _int)
(defx11* XmbSetWMProperties         : _XDisplay-pointer _ulong _string _string (_ptr i _string) _int _XSizeHints-pointer _XWMHints-pointer _XClassHint-pointer -> _void)
(defx11* XmbTextListToTextProperty  : _XDisplay-pointer (_ptr i _string) _int XICCEncodingStyle _XTextProperty-pointer -> _int)
(defx11* XmbTextPropertyToTextList  : _XDisplay-pointer _XTextProperty-pointer _pointer (_ptr i _int) -> _int)
(defx11* Xpermalloc                 : _uint -> _pointer)
(defx11* XSaveContext               : _XDisplay-pointer _ulong _int _string -> _int)
(defx11* XwcFreeStringList          : _pointer -> _void)
(defx11* XwcTextListToTextProperty  : _XDisplay-pointer _pointer _int XICCEncodingStyle _XTextProperty-pointer -> _int)
(defx11* XwcTextPropertyToTextList  : _XDisplay-pointer _XTextProperty-pointer _pointer (_ptr i _int) -> _int)
(defx11* XSetClassHint              : _XDisplay-pointer _ulong _XClassHint-pointer -> _int)
(defx11* XSetWMProperties           : _XDisplay-pointer _ulong _XTextProperty-pointer _XTextProperty-pointer (_ptr i _string) _int _XSizeHints-pointer _XWMHints-pointer _XClassHint-pointer -> _void)

  ;; this just makes sure everything listed in the x11 manual is
  ;; defined somewhere above
(module+ main
  (require "keysym-util.rkt")
  
  (define not-used
  (list 
    XAllPlanes  	 XESetWireToError  	 XSetClipRectangles
    XBitmapBitOrder 	XESetWireToEvent 	XSetCloseDownMode
    XBitmapPad 	XEmptyRegion 	XSetCommand
    XBitmapUnit 	XEnableAccessControl 	XSetDashes
    BlackPixel 	XEqualRegion 	XSetErrorHandler
    BlackPixelOfScreen 	#;XEventMaskOfScreen
    XSetFillRule
    #;CellsOfScreen 
    XEventsQueued 	XSetFillStyle
    #;XcmsClientWhitePointOfCCC
    XExtendedMaxRequestSize 	XSetFont
    XConnectionNumber 	XFetchBuffer 	XSetFontPath
    XDefaultColormap 	XFetchBytes 	XSetForeground
    XFetchName 	XSetFunction
    XDefaultDepth 	XFillArc 	XSetGraphicsExposures
    XFillArcs 	XSetIOErrorHandler
    XDefaultGC 	XFillPolygon 	XSetIconName
    #;DefaultGCOfScreen 
    XFillRectangle 	XSetIconSizes
    #;DefaultRootWindow 
    XFillRectangles 	XSetInputFocus
    DefaultScreen 	DefaultDepth DefaultVisual
    XFindContext 	XSetLineAttributes
    #;DefaultScreenOfDisplay 
    XFlush 	XSetModifierMapping
    #;DefaultVisual 	
    XFlushGC 	XSetNormalHints
    #;DefaultVisualOfScreen 	
    XForceScreenSaver 	XSetPlaneMask
    #;DisplayCells 	
    XFree 	XSetPointerMapping
    DisplayHeight 	XFreeColormap 	
    #;XSetProperty
    #;DisplayHeightMM 	
    XFreeColors 	XSetRGBColormaps
    #;DisplayOfCCC 
    XFreeCursor 	XSetRGBColormaps
    #;DisplayOfScreen 	
    XFreeExtensionList 	XSetRegion
    #;DisplayPlanes 	
    XFreeFont 	XSetScreenSaver
    #;DisplayString 	
    XFreeFontInfo 	XSetSelectionOwner
    DisplayWidth 	XFreeFontNames 	XSetSizeHints
    #;DisplayWidthMM 
    XFreeFontPath 	XSetStandardColormap
    #;DoesBackingStore 	
    XFreeGC 	XSetStandardProperties
    #;DoesSaveUnders	
    XFreeModifiermap 	XSetState
    #;EventMaskOfScreen 	
    XSetStipple
    #;HeightMMOfScreen 	
    XFreePixmap 	XSetSubwindowMode
    #;HeightOfScreen 	
    XFreeStringList 	XSetTSOrigin
    #;ImageByteOrder 
    XGContextFromGC 	XSetTextProperty
    #;InitExtension 
    XGeometry 	XSetTile
    IsCursorKey 	; see keysym-util.rkt
    XGetAtomName 	XSetTransientForHint
    IsFunctionKey 	
    XGetAtomNames 	XSetWMClientMachine
    IsKeypadKey 	
    XGetClassHint 	XSetWMColormapWindows
    IsMiscFunctionKey 	
    XGetCommand 	XSetWMHints
    IsModifierKey 	
    XGetDefault 	XSetWMIconName
    IsPFKey 	
    XGetErrorDatabaseText 	XSetWMName
    IsPrivateKeypadKey 	
    XGetErrorText 	XSetWMNormalHints
    #;LastKnownRequestProcessed 	
    XGetFontPath 	
    XGetFontProperty 	XSetWMProtocols
    #;MinCmapsOfScreen 	
    XGetGCValues 	XSetWMSizeHints
    #;NextRequest 
    XGetGeometry 	XSetWindowBackground
    #;PlanesOfScreen 	
    XGetIconName 	XSetWindowBackgroundPixmap
    #;ProtocolRevision 	
    XGetIconSizes 	XSetWindowBorder
    #;ProtocolVersion 	
    XGetImage 	XSetWindowBorderPixmap
    #;QLength 	
    XGetInputFocus 	XSetWindowBorderWidth
    RootWindow 	XGetKeyboardControl 	XSetWindowColormap
    #;RootWindowOfScreen 	
    XGetKeyboardMapping 	XSetZoomHints
    #;ScreenCount 	
    XGetModifierMapping 	XShrinkRegion
    #;ScreenNumberOfCCC 	
    XGetMotionEvents 	XStoreBuffer
    #;ScreenWhitePointOfCCC 	
    XGetNormalHints 	XStoreBytes
    #;ScreensOfDisplay 	
    XGetPixel 	XStoreColor
    #;ServerVendor 	
    XGetPointerControl 	XStoreColors
    #;VendorRelease 	
    XGetPointerMapping 	XStoreName
    #;VertexDrawLastPoint 	
    XGetRGBColormaps 	XStoreNamedColor
    #;VisualOfCCC 	
    XGetRGBColormaps 	XStringListToTextProperty
    WhitePixel 	XGetScreenSaver 	XStringToKeysym
    #;WhitePixelOfScreen 	
    XGetSelectionOwner 	XSubImage
    #;WidthMMOfScreen 	
    XGetSizeHints 	XSubtractRegion
    #;WidthOfScreen 	
    XGetStandardColormap 	
    XSync
    XActivateScreenSaver 	XGetSubImage 	XSynchronize
    XAddConnectionWatch 	XGetTextProperty 	XTextExtents
    XAddExtension 	XGetTransientForHint 	XTextExtents16
    XAddHost 	XGetVisualInfo 
    XAddHosts 	XGetWMClientMachine 	XTextPropertyToStringList
    XAddPixel 	XGetWMColormapWindows 	XTextWidth
    XAddToSaveSet 	XGetWMHints 	XTextWidth16
    XAllPlanes 	XGetWMIconName 	XTranslateCoordinates
    XAllocClassHint 	XGetWMName 	XUndefineCursor
    XAllocColor 	XGetWMNormalHints 	XUngrabButton
    XAllocColorCells 	XGetWMProtocols 	XUngrabKey
    XAllocColorPlanes 	XGetWMSizeHints 	XUngrabKeyboard
    XAllocIconSize 	XGetWindowAttributes 	XUngrabPointer
    XAllocNamedColor 	XGetWindowProperty 	XUngrabServer
    XAllocSizeHints 	XGetWindowProperty 	XUninstallColormap
    XAllocStandardColormap 	XGetZoomHints 	XUnionRectWithRegion
    XAllocWMHints 	XGrabButton 	XUnionRegion
    XAllowEvents 	XGrabKey 	XUniqueContext
    #;XAppendVertex 	
    XGrabKeyboard 	XUnloadFont
    XAutoRepeatOff 	XGrabPointer 	XUnlockDisplay
    XAutoRepeatOn 	XGrabServer 	XUnmapSubwindows
    XBell 	XHeightMMOfScreen 	XUnmapWindow
    XBitmapBitOrder 	XHeightOfScreen 	XVendorRelease
    XBitmapPad 	XIconifyWindow 	XVisualIDFromVisual
    XBitmapUnit 	XIfEvent 	XWMGeometry
    XBlackPixel 	XIfEvent 	XWarpPointer
    XBlackPixelOfScreen 	XImageByteOrder 	XWhitePixel
    XCellsOfScreen 	XInitExtension 	XWhitePixelOfScreen
    XChangeActivePointerGrab 	XInitImage 	XWidthMMOfScreen
    XChangeGC 	XInitThreads 	XWidthOfScreen
    XChangeKeyboardControl 	XInsertModifiermapEntry 	XWindowEvent
    XChangeKeyboardMapping 	XInstallColormap 	XWithdrawWindow
    XChangePointerControl 	XInternAtom 	XWriteBitmapFile
    XChangeProperty 	XInternAtoms 	XXorRegion
    XChangeSaveSet 	XInternalConnectionNumbers 	#;XcmsAddColorSpace
    XChangeWindowAttributes 	XIntersectRegion 	#;XcmsAddFunctionSet
    XCheckIfEvent 	XKeycodeToKeysym 	#;XcmsAllocColor
    XCheckMaskEvent 	XKeysymToKeycode 	#;XcmsAllocNamedColor
    XCheckTypedEvent 	XKeysymToString 	#;XcmsCCCOfColormap
    XCheckTypedWindowEvent 	XKillClient 	#;XcmsCIELabQueryMaxC
    XCheckWindowEvent 	XLastKnownRequestProcessed 	#;XcmsCIELabQueryMaxL
    XCirculateSubwindows 	XListDepths 	#;XcmsCIELabQueryMaxLC
    XCirculateSubwindowsDown 	XListExtensions 	#;XcmsCIELabQueryMinL
    XCirculateSubwindowsUp 	XListFonts 	#;XcmsCIELabToCIEXYZ
    XClearArea 	XListFontsWithInfo 	#;XcmsCIELuvQueryMaxC
    #;XClearVertexFlag 	
    XListHosts 	#;XcmsCIELuvQueryMaxL
    XClearWindow 	XListInstalledColormaps 	#;XcmsCIELuvQueryMaxLC
    XClipBox 	XListPixmapFormats 	#;XcmsCIELuvQueryMinL
    XCloseDisplay 	XListProperties 	#;XcmsCIELuvToCIEuvY
    XConfigureWindow 	XListProperties 	#;XcmsCIEXYZToCIELab
    XConnectionNumber 	XLoadFont 	#;XcmsCIEXYZToCIEuvY
    XConvertCase 	XLoadQueryFont 	#;XcmsCIEXYZToCIExyY
    XConvertSelection 	XLockDisplay 	#;XcmsCIEXYZToRGBi
    XCopyArea 	#;XLookUpAssoc 	#;XcmsCIEuvYToCIELuv
    XCopyColormapAndFree 	XLookupColor 	#;XcmsCIEuvYToCIEXYZ
    XCopyGC 	XLookupKeysym 	#;XcmsCIEuvYToTekHVC
    XCopyPlane 	XLookupString 	#;XcmsCIExyYToCIEXYZ
    #;XCreateAssocTable 	
    XLowerWindow 	#;XcmsClientWhitePointOfCCC
    XCreateBitmapFromData 	#;XMakeAssoc 	#;XcmsClientWhitePointOfCCC
    XCreateColormap 	XMapRaised 	#;XcmsConvertColors
    XCreateFontCursor 	XMapSubwindows 	#;XcmsCreateCCC
    XCreateFontSet 	XMapWindow 	#;XcmsDefaultCCC
    XCreateGC 	XMaskEvent 	#;XcmsDisplayOfCCC
    XCreateGlyphCursor 	XMatchVisualInfo 	#;XcmsFormatOfPrefix
    XCreateImage 	XMaxCmapsOfScreen 	#;XcmsFreeCCC
    XCreatePixmap 	XMaxRequestSize 	#;XcmsLookupColor
    XCreatePixmapCursor 	XMinCmapsOfScreen 	#;XcmsPrefixOfFormat
    XCreatePixmapFromBitmapData 	XMoveResizeWindow 	#;XcmsQueryBlack
    XCreateRegion 	XMoveWindow 	#;XcmsQueryBlue
    XCreateSimpleWindow 	XNewModifiermap 	#;XcmsQueryColor
    XCreateWindow 	XNextEvent 	#;XcmsQueryColors
    XDefaultColormapOfScreen 	XNextRequest 	#;XcmsQueryGreen
    XDefaultDepthOfScreen 	XNoOp 	#;XcmsQueryRed
    XDefaultGC 	XOffsetRegion 	#;XcmsQueryWhite
    XDefaultGCOfScreen 	XOpenDisplay 	#;XcmsRGBToRGBi
    XDefaultRootWindow 	XParseColor 	#;XcmsRGBiToCIEXYZ
    XDefaultScreen 	XParseGeometry 	#;XcmsRGBiToRGB
    XDefaultScreenOfDisplay 	XPeekEvent 	#;XcmsScreenNumberOfCCC
    XDefaultString 	XPeekIfEvent 	#;XcmsScreenWhitePointOfCCC
    XDefaultVisual 	XPending 	#;XcmsSetCCCOfColormap
    XDefaultVisualOfScreen 	XPlanesOfScreen 	#;XcmsSetCompressionProc
    XDefineCursor 	XPointInRegion 	#;XcmsSetWhiteAdjustProc
    #;XDeleteAssoc 	
    XPolygonRegion 	#;XcmsSetWhitePoint
    XDeleteContext 	XProcessInternalConnection 	#;XcmsStoreColor
    XDeleteModifiermapEntry 	XProtocolRevision 	#;XcmsStoreColors
    XDeleteProperty 	XProtocolVersion 	#;XcmsTekHVCQueryMaxC
    XDeleteProperty 	XPutBackEvent 	#;XcmsTekHVCQueryMaxV
    #;XDestroyAssocTable 	
    XPutImage 	#;XcmsTekHVCQueryMaxVC
    XDestroyImage 	XPutPixel 	#;XcmsTekHVCQueryMaxVSamples
    XDestroyRegion 	XQLength 	#;XcmsTekHVCQueryMinV
    XDestroySubwindows 	XQueryBestCursor 	#;XcmsTekHVCToCIEuvY
    XDestroyWindow 	XQueryBestSize 	#;XcmsVisualOfCCC
    XDisableAccessControl 	XQueryBestStipple 	XmbSetWMProperties
    XDisplayCells 	XQueryBestTile 	XmbTextListToTextProperty
    XDisplayHeight 	XQueryColor 	XmbTextPropertyToTextList
    XDisplayHeightMM 	XQueryColors 	Xpermalloc
    XDisplayKeycodes 	XQueryExtension 	#;XrmClassToString
    XDisplayMotionBufferSize 	XQueryFont 	#;XrmCombineDatabase
    XDisplayName 	XQueryKeymap 	#;XrmCombineFileDatabase
    XDisplayOfScreen 	XQueryPointer 	#;XrmDestroyDatabase
    XDisplayPlanes 	XQueryTextExtents 	#;XrmEnumerateDatabase
    XDisplayString 	XQueryTextExtents16 	#;XrmGetDatabase
    XDisplayWidth 	XQueryTree 	#;XrmGetFileDatabase
    XDisplayWidthMM 	XRaiseWindow 	#;XrmGetResource
    XDoesBackingStore 	XReadBitmapFile 	#;XrmGetStringDatabase
    XDoesSaveUnders 	XReadBitmapFileData 	#;XrmInitialize
    #;XDraw 	
    XRebindKeysym 	#;XrmLocaleOfDatabase
    XDrawArc 	XRecolorCursor 	#;XrmMergeDatabases
    XDrawArcs 	XReconfigureWMWindow 	#;XrmNameToString
    #;XDrawDashed 	
    XRectInRegion 	#;XrmParseCommand
    #;XDrawFilled 	
    XRefreshKeyboardMapping 	#;XrmPermStringToQuark
    XDrawImageString 	XRemoveConnectionWatch 	#;XrmPutFileDatabase
    XDrawImageString16 	XRemoveFromSaveSet 	#;XrmPutLineResource
    XDrawLine 	XRemoveHost 	#;XrmPutResource
    XDrawLines 	XRemoveHosts 	#;XrmPutStringResource
    #;XDrawPatterned 	
    XReparentWindow 	#;XrmQGetResource
    XDrawPoint 	XResetScreenSaver 	#;XrmQGetSearchList
    XDrawPoints 	XResizeWindow 	#;XrmQGetSearchResource
    XDrawRectangle 	XResourceManagerString 	#;XrmQPutResource
    XDrawRectangles 	XRestackWindows 	#;XrmQPutStringResource
    XDrawSegments 	XRootWindow 	#;XrmQuarkToString
    XDrawString 	XRootWindowOfScreen 	#;XrmRepresentationToString
    XDrawString16 	XRotateBuffers 	#;XrmSetDataBase
    XDrawText 	XRotateWindowProperties 	#;XrmSetDatabase
    XDrawText16 	XRotateWindowProperties 	#;XrmStringToBindingQuarkList
    #;XDrawTiled 	
    XSaveContext 	#;XrmStringToClass
    #;XESetBeforeFlush 	
    XScreenCount 	#;XrmStringToClassList
    #;XESetCloseDisplay 	
    XScreenNumberOfScreen 	#;XrmStringToName
    #;XESetCloseDisplay 	
    XScreenResourceString 	#;XrmStringToNameList
    #;XESetCopyGC 	
    #;XScreensOfDisplay 	#;XrmStringToQuark
    #;XESetCreateFont 	
    XSelectInput 	#;XrmStringToQuarkList
    #;XESetCreateGC 	
    XSendEvent 	#;XrmStringToRepresentation
    #;XESetCreateGC 	
    XServerVendor 	#;XrmUniqueQuark
    #;XESetError 	
    XSetAccessControl 	XwcFreeStringList
    #;XESetErrorString 	
    XSetAfterFunction 	XwcTextListToTextProperty
    #;XESetEventToWire 	
    XSetArcMode 	XwcTextPropertyToTextList
    #;XESetFlushGC 	
    XSetBackground 	#;_XSetLastRequestRead
    #;XESetFreeFont 	
    XSetClassHint 	#;_XSetLastRequestRead
    #;XESetFreeGC 	
    XSetClipMask
    #;XESetPrintErrorValues 	
    XSetClipOrigin
    XSetWMProperties

    ))
)
