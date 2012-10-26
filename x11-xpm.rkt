#lang scheme

  (require (lib "foreign.ss")) (unsafe!)
  (require "x11.ss")

  (define libx11 (ffi-lib "libXpm"))

  (define-syntax defx11
    (syntax-rules (:)
      ((_ id : x ...)
         (define id
	   #; (get-ffi-obj (symbol->string 'id) liballegro (_fun x ...))

	   (get-ffi-obj (regexp-replaces (symbol-.string 'id) '((#rx"-" "_"))) libx11 (_fun x ...))
	   ))))

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

  (define-cstruct _XpmColorSymbol
		  ((name _string)
		   (value _pointer)
		   (pixel Pixel)))

  (define-cstruct _XpmExtension
		  ((name _string)
		   (nlines _uint)
		   (lines _pointer)))

  (define-cstruct _XpmColor
		  ((string _string)
		   (symbolic _string)
		   (m-color _pointer)
		   (g4-color _pointer)
		   (g-color _pointer)
		   (c-color _pointer)))

  (define-cstruct _XpmImage
		  ((width _uint)
		   (height _uint)
		   (cpp _uint)
		   (ncolors _uint)
		   (color-table _XpmColor-pointer)
		   (data _pointer)))

  (define-cstruct _XpmInfo
		  ((valuemask _ulong)
		   (hints-cmt _string)
		   (colors-cmt _string)
		   (pixels-cmt _string)
		   (x-hotspot _uint)
		   (y-hotsport _uint)
		   (nextensions _uint)
		   (extensions _XpmExtension-pointer)))

#|
typedef struct {
unsigned long valuemask;		/* Specifies which attributes are
defined */

Visual *visual;			/* Specifies the visual to use */
Colormap colormap;			/* Specifies the colormap to use */
unsigned int depth;			/* Specifies the depth */
unsigned int width;			/* Returns the width of the created
pixmap */
unsigned int height;		/* Returns the height of the created
pixmap */
unsigned int x_hotspot;		/* Returns the x hotspot's
coordinate */
unsigned int y_hotspot;		/* Returns the y hotspot's
coordinate */
unsigned int cpp;			/* Specifies the number of char per
pixel */
Pixel *pixels;			/* List of used color pixels */
unsigned int npixels;		/* Number of used pixels */
XpmColorSymbol *colorsymbols;	/* List of color symbols to override */
unsigned int numsymbols;		/* Number of symbols */
char *rgb_fname;			/* RGB text file name */
unsigned int nextensions;		/* Number of extensions */
XpmExtension *extensions;		/* List of extensions */

unsigned int ncolors;               /* Number of colors */
XpmColor *colorTable;               /* List of colors */
/* 3.2 backward compatibility code */
char *hints_cmt;                    /* Comment of the hints section */
char *colors_cmt;                   /* Comment of the colors section */
char *pixels_cmt;                   /* Comment of the pixels section */
/* end 3.2 bc */
unsigned int mask_pixel;            /* Color table index of transparent
color */

/* Color Allocation Directives */
Bool exactColors;			/* Only use exact colors for visual */
unsigned int closeness;		/* Allowable RGB deviation */
unsigned int red_closeness;		/* Allowable red deviation */
unsigned int green_closeness;	/* Allowable green deviation */
unsigned int blue_closeness;	/* Allowable blue deviation */
int color_key;			/* Use colors from this color set */

Pixel *alloc_pixels;		/* Returns the list of alloc'ed color
pixels */
int nalloc_pixels;			/* Returns the number of alloc'ed
color pixels */

Bool alloc_close_colors;    	/* Specify whether close colors should
be allocated using XAllocColor
or not */
int bitmap_format;			/* Specify the format of 1bit depth
images: ZPixmap or XYBitmap */

/* Color functions */
XpmAllocColorFunc alloc_color;	/* Application color allocator */
XpmFreeColorsFunc free_colors;	/* Application color de-allocator */
void *color_closure;		/* Application private data to pass to
alloc_color and free_colors */

}      XpmAttributes;
|#

  (define-cstruct _XpmAttributes
		  ((valuemask _ulong)
		   (visual _Visual-pointer)
		   (colormap ColorMap)
		   (depth _uint)
		   (width _uint)
		   (height _uint)
		   (x-hotspot _uint)
		   (y-hotspot _int)
		   (cpp _uint)
		   (pixels _pointer)
		   (npixels _int)
		   (colorsymbols _XpmColorSymbol)
		   (numsymbols _uint)
		   (rgb-fname _string)
		   (nextensions _uint)
		   (extensions _XpmExtension-pointer)
		   (ncolors _uint)
		   (colortable _XpmColor-pointer)
		   (hints-cmt _string)
		   (colors-cmt _string)
		   (pixels-cmt _string)
		   (mask-pixel _uint)
		   (exact-colors _bool)
		   (closeness _uint)
		   (red-closeness _uint)
		   (green-closeness _uint)
		   (blue-closeness _uint)
		   (color-key _int)
		   (alloc-pixels _pointer)
		   (nalloc-pixels _int)
		   (alloc-close-colors _bool)
		   (bitmap-format _int)
		   (alloc-color _pointer)
		   (free-colors _pointer)
		   (color-closure _pointer)))

  (defx11* XpmCreatePixmapFromData : _XDisplay-pointer (root : Window) (data : _pointer)
	   (result : (_ptr o Pixmap))
	   (mask : (_ptr o Pixmap))
	   _pointer -> _int
	   -> (values result mask))

  (defx11* XpmCreateDataFromPixmap : _XDisplay-pointer _pointer _ulong _ulong _XpmAttributes-pointer -> _int)
  (defx11* XpmReadFileToPixmap : _XDisplay-pointer _ulong _string (_ptr i _ulong) (_ptr i _ulong) _XpmAttributes-pointer -> _int)
  (defx11* XpmWriteFileFromPixmap : _XDisplay-pointer _string _ulong _ulong _XpmAttributes-pointer -> _int)
  (defx11* XpmCreateImageFromData : _XDisplay-pointer (_ptr i _string) _pointer _pointer _XpmAttributes-pointer -> _int)
  (defx11* XpmCreateDataFromImage : _XDisplay-pointer _pointer _XImage-pointer _XImage-pointer _XpmAttributes-pointer -> _int)
  (defx11* XpmReadFileToImage : _XDisplay-pointer _string _pointer _pointer _XpmAttributes-pointer -> _int)
  (defx11* XpmWriteFileFromImage : _XDisplay-pointer _string _XImage-pointer _XImage-pointer _XpmAttributes-pointer -> _int)
  (defx11* XpmCreateImageFromBuffer : _XDisplay-pointer _string _pointer _pointer _XpmAttributes-pointer -> _int)
  (defx11* XpmCreatePixmapFromBuffer : _XDisplay-pointer _ulong _string (_ptr i _ulong) (_ptr i _ulong) _XpmAttributes-pointer -> _int)
  (defx11* XpmCreateBufferFromImage : _XDisplay-pointer (_ptr i _string) _XImage-pointer _XImage-pointer _XpmAttributes-pointer -> _int)
  (defx11* XpmCreateBufferFromPixmap : _XDisplay-pointer (_ptr i _string) _ulong _ulong _XpmAttributes-pointer -> _int)
  (defx11* XpmReadFileToBuffer : _string (_ptr i _string) -> _int)
  (defx11* XpmWriteFileFromBuffer : _string _string -> _int)
  (defx11* XpmReadFileToData : _string _pointer -> _int)
  (defx11* XpmWriteFileFromData : _string (_ptr i _string) -> _int)
  (defx11* XpmAttributesSize : -> _int)
  (defx11* XpmFreeAttributes : _XpmAttributes-pointer -> _void)
  (defx11* XpmFreeExtensions : _XpmExtension-pointer _int -> _void)
  (defx11* XpmFreeXpmImage : _XpmImage-pointer -> _void)
  (defx11* XpmFreeXpmInfo : _XpmInfo-pointer -> _void)
  (defx11* XpmGetErrorString : _int -> _string)
  (defx11* XpmLibraryVersion : -> _int)
  (defx11* XpmReadFileToXpmImage : _string _XpmImage-pointer _XpmInfo-pointer -> _int)
  (defx11* XpmWriteFileFromXpmImage : _string _XpmImage-pointer _XpmInfo-pointer -> _int)
  (defx11* XpmCreatePixmapFromXpmImage : _XDisplay-pointer _ulong _XpmImage-pointer (_ptr i _ulong) (_ptr i _ulong) _XpmAttributes-pointer -> _int)
  (defx11* XpmCreateImageFromXpmImage : _XDisplay-pointer _XpmImage-pointer _pointer _pointer _XpmAttributes-pointer -> _int)
  (defx11* XpmCreateXpmImageFromImage : _XDisplay-pointer _XImage-pointer _XImage-pointer _XpmImage-pointer _XpmAttributes-pointer -> _int)
  (defx11* XpmCreateXpmImageFromPixmap : _XDisplay-pointer _ulong _ulong _XpmImage-pointer _XpmAttributes-pointer -> _int)
  (defx11* XpmCreateDataFromXpmImage : _pointer _XpmImage-pointer _XpmInfo-pointer -> _int)
  (defx11* XpmCreateXpmImageFromData : (_ptr i _string) _XpmImage-pointer _XpmInfo-pointer -> _int)
  (defx11* XpmCreateXpmImageFromBuffer : _string _XpmImage-pointer _XpmInfo-pointer -> _int)
  (defx11* XpmCreateBufferFromXpmImage : (_ptr i _string) _XpmImage-pointer _XpmInfo-pointer -> _int)
  ;; (defx11* XpmGetParseError : _string (_ptr i _int) (_ptr i _int) -> _int)
  (defx11* XpmFree : (_ptr i _void) -> _void)

