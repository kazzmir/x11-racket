#lang racket
(require "../x11.rkt")

;;; This is a direct translation of:
;;; http://en.wikibooks.org/wiki/X_Windows_Programming/XLib

;int main() {
;   Display *d;
;   int s;
;   Window w;
;   XEvent e;
 
;                        /* open connection with the server */
;   d=XOpenDisplay(NULL);
(define display (XOpenDisplay #f))

;   if(d==NULL) {
;     printf("Cannot open display\n");
;     exit(1);
;   }
(unless display
  (error "Could not open display\n"))

;   s=DefaultScreen(d);
(define screen (DefaultScreen display))

;                        /* create window */
;   w=XCreateSimpleWindow(d, RootWindow(d, s), 10, 10, 100, 100, 1,
;                         BlackPixel(d, s), WhitePixel(d, s));
(define root (RootWindow display screen)) 
(define black (BlackPixel display screen))
(define white (WhitePixel display screen))
(define window (XCreateSimpleWindow display root 10 10 100 100 1
                               black white))
 
;   // Process Window Close Event through event handler so XNextEvent does Not fail
;        Atom delWindow = XInternAtom( d, "WM_DELETE_WINDOW", 0 );
(define delWindow (XInternAtom display "WM_DELETE_WINDOW" #f))

;        XSetWMProtocols(d , w, &delWindow, 1);
(XSetWMProtocols display window (list delWindow))
 
;                        /* select kind of events we are interested in */
;   XSelectInput(d, w, ExposureMask | KeyPressMask);
(XSelectInput display window '(ExposureMask KeyPressMask))
 
;                        /* map (show) the window */
;   XMapWindow(d, w);
(XMapWindow display window)
 
;                        /* event loop */
;   while(1) {
(let loop ()
  
;     XNextEvent(d, &e);
  (define event (XNextEvent* display))
  (printf "Next Event = ~a\n" event)
  
;  /* draw or redraw the window */
;     if(e.type==Expose) {
;       XFillRectangle(d, w, DefaultGC(d, s), 20, 20, 10, 10);
;     }
;                        /* exit on key press */
;     if(e.type==KeyPress)
;       break;
; 
;     // Handle Windows Close Event
;     if(e.type==ClientMessage)
;        break;
  (case (XEvent-type event)
    ((Expose) 
     (printf "Expose!\n")
     (XFillRectangle display window (XDefaultGC display screen) 20 20 10 10)
     (loop))
    (else
     (printf "Event type: \n~a (exiting)\n" (XEvent-type event))
     (printf "Exiting.\n"))))
;  }
 
;                        /* destroy our window */
;   XDestroyWindow(d, w);
(XDestroyWindow display window)
 
;                        /* close connection to server */
;   XCloseDisplay(d);
(XCloseDisplay display)
