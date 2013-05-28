#lang racket/base

(require ;(for-syntax racket/base)
         ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/cvector
         ;(only-in '#%foreign ctype-c->scheme ctype-scheme->c)
         "utils.rkt"
         "x11.rkt"
         )

#| Resources
- XineramaApi.txt (included)
- http://en.wikipedia.org/wiki/Xinerama
- http://xinerama.sourceforge.net/
- http://www.opensource.apple.com/source/X11proto/X11proto-15/xineramaproto/xineramaproto-X11R7.0-1.1.2/Xinerama.h

|#

#| Overview

The Xinerama extension provides a way for a multi-headed system to function 
as one large screen. Windows can span multiple screens and can move from one 
screen to another.

Xinerama requires a homogeneous graphics environment to operate. A homogeneous 
environment is one in which a common set of visuals, depths and screen sizes 
can be found. It is required that at least one common visual is present on 
each card. These can be 8 bit pseudo color, or 24 bit true color etc. A mix of 
8 and 24 bit visuals is allowed, but all cards need to support at least one 
common visual ID. Xinerama will find and use the intersection of the lowest 
common set of visual IDs. It is undesirable to run Xinerama with dissimilar 
screen sizes, changes in screen size can confuse window managers.

For the purpose of Xinerama, a screen is defined as a set of frambuffers 
joined logically as a single work surface.   
|#

(define xinerama-lib (ffi-lib "libXinerama" '("1")))
(define-ffi-definer define-xinerama xinerama-lib
  #:provide provide
  #:default-make-fail 
  make-not-available
  #;(λ(s)(format "Xinerama Warning: foreign identifier ~a not found\n" s)
                        (λ()(make-not-available s))))

(define-cstruct* _XineramaScreenInfo
 ((screen-number _int)
  (x-org _short)
  (y_org _short)
  (width _short)
  (height _short)))

#|
Bool XineramaQueryExtension(display,event_base,error_base)

        Display *display;
        int *event_base, *error_base;

display 	Specifies the connection to the Xserver.
event_base   	Specifies the return location for the assigned base event
error_base   	Specifies the return location for the assigned base error

The XineramaQueryExtension function queries the Xserver to determine the 
availability of the Xinerama Extension. If the extension is available, the 
return value is TRUE, and event_base and error_base are set to the base event 
number and base error number for the extension, respectively. Otherwise, the 
return value is FALSE, and the values of event_base and error_base are 
undefined. |#
(define-xinerama XineramaQueryExtension 
  (_fun _XDisplay-pointer (event-base : (_ptr o _int)) (error-base : (_ptr o _int)) 
        -> (ret : _bool)
        -> (and ret (list event-base error-base))))

#|
Status XineramaQueryVersion(display,major_version,minor_version)

        Display *display;
        int     *major_version, *minor_version;

display         Specifies the connection to the Xserver.
major_version   Specifies the return location for the extension major version.
minor_version   Specifies the return location for the extension minor version.

The XineramaQueryVersion function returns the version of the Xinerama extension
implemented by the Xserver. The version is returned in major_version and 
minor_version. The major version and minor version for this specification are 
'1' and '2', respectively. The major version will be incremented for protocol 
incompatible changes, and the minor version will be incremented for small, 
upwardly compatible changes.

Status returned is Success for all okay. |#
(define-xinerama XineramaQueryVersion 
  (_fun _XDisplay-pointer (major-version : (_ptr o _int)) (minor-version : (_ptr o _int))
        -> (status : Status)
        -> (and status (list major-version minor-version))))


(define-xinerama XineramaIsActive
  (_fun _XDisplay-pointer -> _bool))

#|
   Returns the number of heads and a pointer to an array of
   structures describing the position and size of the individual
   heads.  Returns NULL and number = 0 if Xinerama is not active.
  
   Returned array should be freed with XFree(). |#
(define-xinerama XineramaQueryScreens
  (_fun _XDisplay-pointer (number : (_ptr o _int))
        -> (screen-infos : _pointer)
        -> (and (not (zero? number))
                (cblock->vector/finalizer screen-infos _XineramaScreenInfo number XFree))))

;;; The following functions seem to be specific to version (1 2)
;;; which I don't have (having '(1 1)).

#|
BOOL XineramaActive(display,win)

	Display *display;
	Window	win;

display 	Specifies the connection to the Xserver.
win		Specifies any window.

The XineramaActive function returns a Boolean operator used to determine if 
Xinerama is activated on the screen. Returns TRUE for active and FALSE for not 
active. BadWindow is returned for an invalid window. |#
(define-xinerama XineramaActive
  (_fun _XDisplay-pointer Window -> _bool))

#|
Status XineramaGetData(display, win, framebuffer_rects, num_framebuffers)

	Display *display;
	Window win;
	XRectangle  **framebuffer_rects;
	int *number_framebuffers;

display 	Specifies the connection to the Xserver.
win		Specifies any window.

The XineramaGetData function uses the two pointers below to return info about 
each individual framebuffer within the Xinerama Screen.

framebuffer_rects	An array of rects, one for each framebuffer in the order
             	         that the framebuffers are specified on the Xserver 
			 command line.
num_framebuffers	The number of framebuffers managed by Xinerama
             	         for this Screen.

Use XFree() to release the memory.

Return Status will be Success for no error, and BadWindow for invalid window, 
BadAlloc for memory allocation failure. |#
(define-xinerama XineramaGetData
  (_fun _XDisplay-pointer Window (frame-buffer-rects : (_ptr o _pointer)) (number-frame-buffers : (_ptr o _int))
        -> (status : Status)
        -> (and status
                (cblock->vector/finalizer frame-buffer-rects _XRectangle number-frame-buffers XFree))))

#|
Status XineramaGetCenterHint(display, root_window, x,  y)

	Display * display;
	Window root_window;
	int * x;
	int * y;

display 	Specifies the connection to the Xserver.
root_window	Specifies the root window.
x		Used to return value of x
y		Used to return value of y

The XineramaGetCenterHint function returns x and y to indicate where a window 
that wants to display in the "center" should be centered.

The Xinerama center hint is also a root window property. The property ATOM is 
called XINERAMA_CENTER_HINT.  The property format is 2 16-bit integers the 
X coordinate followed by the Y coordinate.

Status is Success for everything okay, and BadWindow if the window is invalid. |#
(define-xinerama XineramaGetCenterHint
  (_fun _XDisplay-pointer Window (x : (_ptr o _int)) (y : (_ptr o _int)) 
        -> (status : Status)
        -> (and status (list x y))))

#|
void XineramaSetCenterHint(display, root_window, x,  y)

	Display * display;
	Window root_window;
	int x;
	int y;

display 	Specifies the connection to the Xserver.
root_window	Specifies the root window.
x		Value of x
y		Value of y

The XineramaSetCenterHint allows for setting of the XINERAMA_CENTER_HINT atom 
based on given x and y.

The Xinerama center hint is also a root window property. The property ATOM is 
called XINERAMA_CENTER_HINT.  The property format is 2 16-bit integers the 
X coordinate followed by the Y coordinate.

BadWindow is returned if the window is invalid. |#
(define-xinerama XineramaSetCenterHint
  (_fun _XDisplay-pointer Window _int _int -> _void))

(module+ main
  (require racket/vector)
  (define dpy (XOpenDisplay #f))
  (XineramaQueryExtension dpy)
  (XineramaQueryVersion dpy)
  (XineramaIsActive dpy)
  (vector-map XineramaScreenInfo->list* (XineramaQueryScreens dpy))
  (XCloseDisplay dpy)
  )
