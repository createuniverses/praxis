/*!
    \file  og_window.c
    \brief Window management methods
*/
/*
 * Portions copyright (C) 2004, the OpenGLUT project contributors.
 * OpenGLUT branched from freeglut in February, 2004.
 *
 * Copyright (c) 1999-2000 Pawel W. Olszta. All Rights Reserved.
 * Written by Pawel W. Olszta, <olszta@sourceforge.net>
 * Creation date: Fri Dec 3 1999
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * PAWEL W. OLSZTA BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <GL/openglut.h>
#include "og_internal.h"

/* -- PRIVATE FUNCTIONS ---------------------------------------------------- */

#if TARGET_HOST_WINCE
#include <aygshell.h>                  /*! \todo Move this to og_internal.h */
/*!
    Daniel Wagner provided a similar routine to freeglut.  Why it is at the top
    of the file is not clear.  The placement suggests an external source,
    but it is presumed to be his.  I've re-implemented it from the implied
    description.  This removes any question about the origin.  Doing a
    post-hoc comparison, this version is slightly safer and in line with
    OpenGLUT style.  It was also renamed to fit the OpenGLUT nomenclature
    for a helper function.

    \todo Move this into a better location.

    \todo We probably shouldn't allocate a buffer that our caller has to
          deallocate.
*/
static wchar_t *oghStrToWstr( const char *str )
{
    size_t bytes = strlen( str ) + 1;
    wchar_t *ret = malloc( 2 * bytes );
    size_t i;

    if( ret )
        for( i = 0; i < bytes; ++i )
            ret[ i ] = str[ i ];

    return ret;
}
#endif

/*
 * These special helper functions (and support macros) are used to
 * cascade keyboard and mouse events to the parents of client menu windows.
 *
 * The code is located at the top to separate it from the API code, but they
 * are {static} functions of interest solely for menu windows.
 *
 * oghX/oghY: These map a coordinates in a child window to coordinates
 *            in the parent window.
 * X/Y:       These recurred enough that it seemed simpler to use a macro.
 *            Especially since, at one time, the oghX/oghY functions
 *            were inline.  (^&  At one point the same definitions
 *            were also used in glutCreateMenuWindow().
 * oghClientMenu*(): A bunch of callback functions to be registered with
 *                   the client menus.  They redirect all keyboard and mouse
 *                   input to the parent window's handler, after translating
 *                   mouse coords.
 *
 * NB: You cannot use the X and Y macros within the INVOKE_WCB() macro
 *     invocations.  INVOKE_WCB(), being a macro, evaluates its
 *     parameters when, and only when, they are used.  It sets the
 *     current window before evaluating the translated coordinates,
 *     and at that point it's a bit late to run around translating the
 *     coords, since we no longer know the child window in order to
 *     get its position.
 *
 * XXX oghX() and oghY() should probably reuse some common code to avoid
 * XXX risk of getting out of sync if they are changed.
 */
static int oghX( const int x )
{
    int ret = x;
    SOG_Window *win = ogStructure.Window;
    SOG_Window *parent;

    assert( win );
    assert( win->Parent );
    parent = win->Parent;

    ret += glutGet( GLUT_WINDOW_X );
    ogSetWindow( parent );
    ret -= glutGet( GLUT_WINDOW_X );
    ogSetWindow( win );

    return ret;
}
static int oghY( const int y )
{
    int ret = y;
    SOG_Window *win = ogStructure.Window;
    SOG_Window *parent;

    assert( win );
    assert( win->Parent );
    parent = win->Parent;

    ret += glutGet( GLUT_WINDOW_Y );
    ogSetWindow( parent );
    ret -= glutGet( GLUT_WINDOW_Y );
    ogSetWindow( win );

    return ret;
}
#define X oghX(x)
#define Y oghY(y)
static void oghClientMenuKeyboard( unsigned char key, int x, int y )
{
    int _x = X;
    int _y = Y;
    INVOKE_WCB( *ogStructure.Window->Parent, Keyboard, ( key, _x, _y ) );
}
static void oghClientMenuKeyboardUp( unsigned char key, int x, int y )
{
    int _x = X;
    int _y = Y;
    INVOKE_WCB( *ogStructure.Window->Parent, KeyboardUp, ( key, _x, _y ) );
}
static void oghClientMenuSpecial( int key, int x, int y )
{
    int _x = X;
    int _y = Y;
    INVOKE_WCB( *ogStructure.Window->Parent, Special, ( key, _x, _y ) );
}
static void oghClientMenuSpecialUp( int key, int x, int y )
{
    int _x = X;
    int _y = Y;
    INVOKE_WCB( *ogStructure.Window->Parent, SpecialUp, ( key, _x, _y ) );
}

static void oghClientMenuMotion( int x, int y )
{
    int _x = X;
    int _y = Y;
    INVOKE_WCB( *ogStructure.Window->Parent, Motion, ( _x, _y ) );
}
static void oghClientMenuPassiveMotion( int x, int y )
{
    int _x = X;
    int _y = Y;
    INVOKE_WCB( *ogStructure.Window->Parent, Passive, ( _x, _y ) );
}
static void oghClientMenuMouse( int button, int state, int x, int y )
{
    int _x = X;
    int _y = Y;
    INVOKE_WCB(
        *ogStructure.Window->Parent, Mouse, ( button, state, _x, _y )
    );
}
static void oghClientMenuMouseWheel( int wheel, int dir, int x, int y )
{
    int _x = X;
    int _y = Y;
    INVOKE_WCB(
        *ogStructure.Window->Parent, MouseWheel, ( wheel, dir, _x, _y )
    );
}
#undef X
#undef Y


/*
 * Chooses a visual based on the current display mode settings.
 */
#if TARGET_HOST_UNIX_X11
XVisualInfo *ogChooseVisual( void )
{
#define BUFFER_SIZES 6
    int bufferSize[ BUFFER_SIZES ] = { 16, 12, 8, 4, 2, 1 };
    GLboolean wantIndexedMode = GL_FALSE;
    int attributes[ 32 ];
    int where = 0;

    /* First we have to process the display mode settings... */
#define ATTRIB(a)       ( attributes[ where++ ] = ( a ) )
#define ATTRIB_VAL(a,v) { ATTRIB( a ); ATTRIB( v ); }

    if( ogState.DisplayMode & GLUT_INDEX )
    {
        ATTRIB_VAL( GLX_BUFFER_SIZE, 8 );
        wantIndexedMode = GL_TRUE;
    }
    else
    {
        ATTRIB( GLX_RGBA );
        ATTRIB_VAL( GLX_RED_SIZE,   1 );
        ATTRIB_VAL( GLX_GREEN_SIZE, 1 );
        ATTRIB_VAL( GLX_BLUE_SIZE,  1 );
        if( ogState.DisplayMode & GLUT_ALPHA )
            ATTRIB_VAL( GLX_ALPHA_SIZE, 1 );
    }

    if( ogState.DisplayMode & GLUT_DOUBLE )
        ATTRIB( GLX_DOUBLEBUFFER );

    if( ogState.DisplayMode & GLUT_STEREO )
        ATTRIB( GLX_STEREO );

    if( ogState.DisplayMode & GLUT_DEPTH )
        ATTRIB_VAL( GLX_DEPTH_SIZE, 1 );

    if( ogState.DisplayMode & GLUT_STENCIL )
        ATTRIB_VAL( GLX_STENCIL_SIZE, 1 );

    if( ogState.DisplayMode & GLUT_ACCUM )
    {
        ATTRIB_VAL( GLX_ACCUM_RED_SIZE,   1 );
        ATTRIB_VAL( GLX_ACCUM_GREEN_SIZE, 1 );
        ATTRIB_VAL( GLX_ACCUM_BLUE_SIZE,  1 );
        if( ogState.DisplayMode & GLUT_ALPHA )
            ATTRIB_VAL( GLX_ACCUM_ALPHA_SIZE, 1 );
    }

    /* Push a null at the end of the list */
    ATTRIB( None );

    if( ! wantIndexedMode )
        return glXChooseVisual( ogDisplay.Display, ogDisplay.Screen,
                                attributes );
    else
    {
        XVisualInfo *visualInfo;
        int i;

        /*
         * In indexed mode, we need to check how many bits of depth can we
         * achieve.  We do this by trying each possibility from the list
         * given in the {bufferSize} array.  If we match, we return to caller.
         */
        for( i = 0; i < BUFFER_SIZES; i++ )
        {
            attributes[ 1 ] = bufferSize[ i ];
            visualInfo = glXChooseVisual( ogDisplay.Display, ogDisplay.Screen,
                                          attributes );
            if( visualInfo )
                return visualInfo;
        }
        return NULL;
    }
}
#endif

/*
 * Setup the pixel format for a Win32 window.
 */
#if TARGET_HOST_WIN32
GLboolean ogSetupPixelFormat( SOG_Window *window, GLboolean checkOnly,
                              unsigned char layer_type )
{
    PIXELFORMATDESCRIPTOR *ppfd, pfd;
    int flags, pixelformat;

    /* XXX This if() probably should be an assert() */
    if( !window )
        return 0;

    if( window->State.IsOffscreen )
	{
        flags = PFD_DRAW_TO_BITMAP;
		// No double buffering for bitmap
	}
    else
	{
        flags = PFD_DRAW_TO_WINDOW;

		if( ogState.DisplayMode & GLUT_DOUBLE )
			flags |= PFD_DOUBLEBUFFER;
	}

    flags |= PFD_SUPPORT_OPENGL;

    //flags |= PFD_GENERIC_ACCELERATED;


    /*! \todo ogSetupPixelFormat(): there is still some work to do here! */

    /* Specify which pixel format do we opt for... */
	{
		pfd.nSize           = sizeof( PIXELFORMATDESCRIPTOR );
		pfd.nVersion        = 1;
		pfd.dwFlags         = flags;
		pfd.iPixelType      = PFD_TYPE_RGBA;
		pfd.cColorBits      = 24;
		pfd.cRedBits        = 0;
		pfd.cRedShift       = 0;
		pfd.cGreenBits      = 0;
		pfd.cGreenShift     = 0;
		pfd.cBlueBits       = 0;
		pfd.cBlueShift      = 0;
		pfd.cAlphaBits      = 0;
		pfd.cAlphaShift     = 0;
		pfd.cAccumBits      = 0;
		pfd.cAccumRedBits   = 0;
		pfd.cAccumGreenBits = 0;
		pfd.cAccumBlueBits  = 0;
		pfd.cAccumAlphaBits = 0;
		pfd.cDepthBits      = 24;
		pfd.cStencilBits    = 8;
		pfd.cAuxBuffers     = 0;
		pfd.iLayerType      = layer_type; // This is always PFD_MAIN_PLANE;
		pfd.bReserved       = 0;
		pfd.dwLayerMask     = 0;
		pfd.dwVisibleMask   = 0;
		pfd.dwDamageMask    = 0;

		if( window->State.IsOffscreen )
		{
			// There is no such thing as a 32 bit bitmap, apparently
		}
		else
		{
			// Lets always have 24 bits!
			pfd.cColorBits = ( BYTE )GetDeviceCaps( window->Window.Device, BITSPIXEL );
		}
	}

    ppfd = &pfd;

    pixelformat = ChoosePixelFormat( window->Window.Device, ppfd );
    if( pixelformat == 0 )
        return GL_FALSE;

    if( checkOnly )
        return GL_TRUE;
    return SetPixelFormat( window->Window.Device, pixelformat, ppfd );
}
#endif

/*
 * Sets the OpenGL context and the ogStructure "Current Window" pointer to
 * the window structure passed in.
 */
void ogSetWindow( SOG_Window *window )
{
    if( window && ( window != ogStructure.Window ) )
    {
#if TARGET_HOST_UNIX_X11
        glXMakeCurrent(
            ogDisplay.Display,
            window->Window.Handle,
            window->Window.Context
        );
#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE
        if( ogStructure.Window && !( ogStructure.Window->State.IsOffscreen ) )
            ReleaseDC( ogStructure.Window->Window.Handle,
                       ogStructure.Window->Window.Device );
        if( !( window->State.IsOffscreen ) )
            window->Window.Device = GetDC( window->Window.Handle );
        wglMakeCurrent(
            window->Window.Device,
            window->Window.Context
        );
#endif
        ogStructure.Window = window;
    }
}


/*! \internal
    Opens a window. Requires a SOG_Window object created and attached
    to the OpenGLUT structure. OpenGL context is created here.
*/
/* ARGSUSED6 */
void ogOpenWindow( SOG_Window *window, const char *title,
                   int x, int y, int w, int h,
                   GLboolean gameMode, GLboolean isSubWindow )
{
#if TARGET_HOST_UNIX_X11
    XSetWindowAttributes winAttr;
    XTextProperty textProperty;
    XSizeHints sizeHints;
    XWMHints wmHints;
    unsigned long mask;

    freeglut_assert_ready;
    /*!
      \todo ogChooseVisual() is a common part of all three.
       With a little thought, we should be able to greatly
       simplify this.
    */
    /*
    if( !window->IsMenu )
        window->Window.VisualInfo = ogChooseVisual( );
    else if( ogStructure.MenuContext )
        window->Window.VisualInfo = ogChooseVisual( );
    else
    */
    {
        unsigned int current_DisplayMode = ogState.DisplayMode;
        /*! \todo Why are menus double- and depth-buffered? */
        if( window->IsMenu && !ogStructure.MenuContext )
            ogState.DisplayMode = GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH;
        window->Window.VisualInfo = ogChooseVisual( );
        if( window->IsMenu && !ogStructure.MenuContext )
            ogState.DisplayMode = current_DisplayMode;
    }

    /*! \todo Do we need the MenuContext check? */
    if(
        !window->Window.VisualInfo &&
        ( !window->IsMenu /* || ogStructure.MenuContext */ )
    )
        /*
         * The ogChooseVisual() returned a null meaning that the visual
         * context is not available.
         * Try a couple of variations to see if they will work.
         */
        if( !( ogState.DisplayMode & GLUT_DOUBLE ) )
        {
            ogState.DisplayMode |= GLUT_DOUBLE;
            window->Window.VisualInfo = ogChooseVisual( );
            ogState.DisplayMode &= ~GLUT_DOUBLE;
        }
        /*
         * GLUT also checks for multi-sampling, but I don't see that
         * anywhere else in freeglut/OpenGLUT so I won't bother with
         * it for the moment.
         */

    /*!
      \bug This seems to be abusing an assert() for error-checking.
       It is possible that the visual simply can't be found,
       in which case we should print an error and return a 0
       for the window id, I think.  Failing that, call ogError().
    */
    assert( window->Window.VisualInfo != NULL );

    window->State.IsOffscreen = GL_FALSE;
    if( ogState.DisplayMode & GLUT_OFFSCREEN )
    {
        window->State.IsOffscreen = GL_TRUE;
        window->Window.Pixmap = XCreatePixmap(
            ogDisplay.Display, ogDisplay.RootWindow,
            w, h,
            window->Window.VisualInfo->depth
        );
        if( False != window->Window.Pixmap )
        {
            window->Window.Handle = glXCreateGLXPixmap(
                ogDisplay.Display,
                window->Window.VisualInfo,
                window->Window.Pixmap
            );
            if( False == window->Window.Handle )
                XFreePixmap( ogDisplay.Display, window->Window.Pixmap );
            else
            {
                window->State.Width = w;
                window->State.Height = h;
            }
        }
    }
    else
    {
        /*!
           \todo HINT: the masks should be updated when adding/removing
           callbacks.  This might speed up message processing. Is that true?
           <b>A:</b> Not appreciably, but it <b>will</b> make it easier to
              debug.  Try tracing old GLUT and try tracing freeglut/OpenGLUT.
              Old GLUT turns off events that it doesn't need and is a whole
              lot more pleasant to trace.  (Think mouse-motion!  Tons of
              ``bonus'' GUI events stream in.)
        */
        winAttr.event_mask        =
            StructureNotifyMask | SubstructureNotifyMask | ExposureMask |
            ButtonPressMask | ButtonReleaseMask | KeyPressMask | KeyRelease |
            VisibilityChangeMask | EnterWindowMask | LeaveWindowMask |
            PointerMotionMask | ButtonMotionMask;
        winAttr.background_pixmap = None;
        winAttr.background_pixel  = 0;
        winAttr.border_pixel      = 0;

        winAttr.colormap = XCreateColormap(
            ogDisplay.Display, ogDisplay.RootWindow,
            window->Window.VisualInfo->visual, AllocNone
        );

        mask = CWBackPixmap | CWBorderPixel | CWColormap | CWEventMask;

        if( window->IsUnmanaged )
        {
            winAttr.override_redirect = True;
            mask |= CWOverrideRedirect;
        }

        window->Window.Handle = XCreateWindow(
            ogDisplay.Display,
            ( window->Parent && !window->IsClientMenu ) ?
            window->Parent->Window.Handle : ogDisplay.RootWindow,
            x, y, w, h, 0,
            window->Window.VisualInfo->depth, InputOutput,
            window->Window.VisualInfo->visual, mask,
            &winAttr
        );
    }
    /*
     * The GLX context creation, possibly trying the direct context rendering
     *  or else use the current context if the user has so specified
     */
    if( window->IsMenu )
    {
        /*
         * If there isn't already an OpenGL rendering context for menu
         * windows, make one.
         *
         * XXX Why do we need *both* a global menu context *and*
         * XXX a per-menu-window context?
         */
        if( !ogStructure.MenuContext )
        {
            ogStructure.MenuContext =
                ( SOG_MenuContext * )malloc( sizeof( SOG_MenuContext ) );
            ogStructure.MenuContext->VisualInfo = window->Window.VisualInfo;
            ogStructure.MenuContext->Context = glXCreateContext(
                ogDisplay.Display, ogStructure.MenuContext->VisualInfo,
                NULL, ogState.ForceDirectContext | ogState.TryDirectContext
            );
        }

        /* window->Window.Context = ogStructure.MenuContext->Context; */
        window->Window.Context = glXCreateContext(
            ogDisplay.Display, window->Window.VisualInfo,
            NULL, ogState.ForceDirectContext | ogState.TryDirectContext
        );
    }
    else if( ogState.UseCurrentContext )
    {
        window->Window.Context = glXGetCurrentContext( );

        if( !window->Window.Context ||
            ( ( ogState.ForceDirectContext || ogState.TryDirectContext ) &&
              !glXIsDirect( ogDisplay.Display, window->Window.Context ) )
        )
            window->Window.Context = glXCreateContext(
                ogDisplay.Display, window->Window.VisualInfo,
                NULL,
                ( ogState.ForceDirectContext | ogState.TryDirectContext ) &&
                !window->Window.Pixmap
            );
    }
    else
        window->Window.Context = glXCreateContext(
            ogDisplay.Display, window->Window.VisualInfo,
            NULL,
            ( ogState.ForceDirectContext | ogState.TryDirectContext ) &&
            !window->Window.Pixmap
        );

    if( ogState.ForceDirectContext &&
        !glXIsDirect( ogDisplay.Display, window->Window.Context ) )
        ogError( "unable to force direct context rendering for window '%s'",
                 title );

    /*!
       \todo Assume the new window is visible by default
        Is this a  safe assumption?
     */
    window->State.Visible = GL_TRUE;

    sizeHints.flags = 0;
    if( ogState.Position.Use )
        sizeHints.flags |= USPosition;
    if( ogState.Size.Use )
        sizeHints.flags |= USSize;

    /*
     * Fill in the size hints values now (the x, y, width and height
     * settings are obsolete, are there any more WMs that support them?)
     * Unless the X servers actually stop supporting these, we should
     * continue to fill them in.  It is *not* our place to tell the user
     * that they should replace a window manager that they like, and which
     * works, just because *we* think that it's not "modern" enough.
     */
    /*!
       \todo WINCE wants to set these to {0, 0, 230, 240} unconditionally.
        If that's necessary, we'll use a #if/#endif, but it seems a
        rather bad idea to kill the application's window size requests.
    */
    sizeHints.x      = x;
    sizeHints.y      = y;
    sizeHints.width  = w;
    sizeHints.height = h;

    /*
     * XXX May have to turn on InputHint, and set wmHints.input = True.
     * XXX Maybe not.
     */
    wmHints.flags = StateHint;
    wmHints.initial_state = ogState.ForceIconic ? IconicState : NormalState;
    if( GL_FALSE == window->State.IsOffscreen )
    {
        /* Prepare the window and iconified window names. */
        /*
         * XXX Should ogStrDup() the title, since XStringListToTextProperty()
         * XXX apparently allows the right to edit {title}.
         */
        XStringListToTextProperty( ( char ** )&title, 1, &textProperty );
        XSetWMProperties(
            ogDisplay.Display,
            window->Window.Handle,
            &textProperty,
            &textProperty,
            0,
            0,
            &sizeHints,
            &wmHints,
            NULL
        );
        XSetWMProtocols( ogDisplay.Display, window->Window.Handle,
                         &ogDisplay.DeleteWindow, 1 );
        XMapWindow( ogDisplay.Display, window->Window.Handle );
    }

#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE

    WNDCLASS wc;
    DWORD flags;
    DWORD exFlags = 0;
    ATOM atom;

    freeglut_assert_ready;

    /* Grab the window class we have registered on glutInit(): */
    atom = GetClassInfo( ogDisplay.Instance, _T( OPENGLUT_STRING ), &wc );
    assert( atom != 0 );

    window->State.IsOffscreen = GL_FALSE;
    if( gameMode )
    {
        assert( window->Parent == NULL );

        /*
         * Set the window creation flags appropriately to make the window
         * entirely visible:
         */
        flags = WS_POPUP | WS_CLIPSIBLINGS | WS_CLIPCHILDREN | WS_VISIBLE;// | CS_HREDRAW | CS_VREDRAW;
    }
    else if( GLUT_OFFSCREEN & ogState.DisplayMode )
    {
        /*
         * XXX Resource allocation failure in this section currently causes
         * XXX an ogError() invocation.  Perhaps it would be better to use
         * XXX a return-0-for-window-ID?  That would be more graceful,
         * XXX in some ways, but places burden on the client.
         */
        static BITMAPINFO  info;

        window->State.IsOffscreen = GL_TRUE;
        info.bmiHeader.biSize        = sizeof( BITMAPINFOHEADER );
        info.bmiHeader.biWidth       = w;
        info.bmiHeader.biHeight      = h;
        info.bmiHeader.biPlanes      = 1;
        info.bmiHeader.biBitCount    = 24;
        info.bmiHeader.biCompression = BI_RGB;
        window->Window.Device = CreateCompatibleDC( NULL );
        if( !( window->Window.Device ) )
            ogError( "Could not create Device Context for offscreen.\n" );
        window->Window.Bitmap = CreateDIBSection(
            window->Window.Device, &info, DIB_RGB_COLORS,
            ( void ** ) &( window->Window.Bits ), NULL, 0
        );
        if( !( window->Window.Bitmap ) )
            ogError( "Could not create bitmap for offscreen.\n" );
        SelectObject( window->Window.Device, window->Window.Bitmap );
        window->Window.Handle = 0;
        if( !ogSetupPixelFormat( window, GL_FALSE, PFD_MAIN_PLANE ) )
            ogError(
                "Failed to setup pixelformat.\n"
                "  GetLastError() -=> %d\n",
                GetLastError ()
            );
        window->Window.Context = wglCreateContext( window->Window.Device );
        if( !window->Window.Context )
            ogError(
                "Failed to create rendering context.\n"
                "  GetLastError() -=> %d\n",
                GetLastError( )
            );
        window->State.Width = w;
        window->State.Height = h;
    }
    else
    {
#if !TARGET_HOST_WINCE
        if( ( !isSubWindow ) && ( !window->IsUnmanaged ) )
        {
            /*!
                \note Move this comment to a more central place?
                (Already relocated to general, end-user documentation.)
                Update the window dimensions, taking account of window
                decorations.  OpenGLUT is to create the window with the
                outside of its border at (x,y) and with dimensions (w,h).
            */
            w += ( GetSystemMetrics( SM_CXSIZEFRAME ) )*2;
            h += ( GetSystemMetrics( SM_CYSIZEFRAME ) )*2 +
                GetSystemMetrics( SM_CYCAPTION );
        }
#endif

        if( !ogState.Position.Use )
        {
            x = CW_USEDEFAULT;
            y = CW_USEDEFAULT;
        }
        if( !ogState.Size.Use )
        {
            w = CW_USEDEFAULT;
            h = CW_USEDEFAULT;
        }

        /*
         * There's a small difference between creating the top, child and
         * game mode windows
         */
        flags = WS_CLIPSIBLINGS | WS_CLIPCHILDREN;// | CS_HREDRAW | CS_VREDRAW;
        flags |= WS_POPUP;
        flags |= WS_OVERLAPPEDWINDOW; // Uncomment this to start the window with a border
        // flags |= WS_VISIBLE; // Unnecessary, due to call to ShowWindow

#if 0
        if( window->IsUnmanaged )
        {
            flags |= WS_POPUP;
            exFlags |= WS_EX_TOOLWINDOW;
        }
#if !TARGET_HOST_WINCE
        /* XXX Maybe also check for current-window is a client Menu Window */
        else if( !window->Parent )
            flags |= WS_OVERLAPPEDWINDOW;
#endif
        else
            flags |= WS_CHILD;
#endif
    }

    if( !window->State.IsOffscreen )
    {
#if TARGET_HOST_WINCE
        wchar_t *wstr = oghStrToWstr( title );
        window->Window.Handle = CreateWindow(
            _T( OPENGLUT_STRING ),
            wstr,
            WS_VISIBLE | WS_POPUP,
            0,0, 240,320,
            NULL,
            NULL,
            ogDisplay.Instance,
            ( LPVOID )window
        );
        free( wstr );

        SHFullScreen( window->Window.Handle, SHFS_HIDESTARTICON );
        SHFullScreen( window->Window.Handle, SHFS_HIDESIPBUTTON );
        SHFullScreen( window->Window.Handle, SHFS_HIDETASKBAR );
        MoveWindow( window->Window.Handle, 0, 0, 240, 320, TRUE );
        ShowWindow( window->Window.Handle, SW_SHOW );
        UpdateWindow( window->Window.Handle );
#else
        window->Window.Handle = CreateWindowEx(
            exFlags,
            OPENGLUT_STRING,
            title,
            flags,
            x, y, w, h,
            ( HWND )
                ( window->Parent && !window->IsClientMenu ) ?
                window->Parent->Window.Handle : NULL,
            ( HMENU )NULL,
            ogDisplay.Instance,
            ( LPVOID )window
        );
#endif
        if( !( window->Window.Handle ) )
            ogError( "Failed to create a window (%s)!", title );
        if( window->IsClientMenu || window->IsMenu || gameMode )
            SetWindowPos(
                window->Window.Handle,
                HWND_TOPMOST,
                0, 0, 0, 0,
                SWP_NOMOVE | SWP_NOSIZE
            );

#if TARGET_HOST_WINCE
        ShowWindow( window->Window.Handle, SW_SHOW );
#else
        ShowWindow( window->Window.Handle,
                    ogState.ForceIconic ? SW_SHOWMINIMIZED : SW_SHOW );
#endif
        UpdateWindow( window->Window.Handle );
        ShowCursor( TRUE );  /*! \bug Old comments say "hide cusror"! */
    }
#endif
    ogSetWindow( window );

    if( window->State.IsOffscreen )
        window->Window.DoubleBuffered = 0;
    else
    {
        window->Window.DoubleBuffered =
            ( ogState.DisplayMode & GLUT_DOUBLE ) ? 1 : 0;

        if( ! window->Window.DoubleBuffered )
        {
            glDrawBuffer ( GL_FRONT );
            glReadBuffer ( GL_FRONT );
        }
    }
}

/*
 * Closes a window, destroying the frame and OpenGL context
 */
void ogCloseWindow( SOG_Window *window )
{
    freeglut_assert_ready;

#if TARGET_HOST_UNIX_X11

    glXDestroyContext( ogDisplay.Display, window->Window.Context );
    if( GL_FALSE == window->State.IsOffscreen )
        XDestroyWindow( ogDisplay.Display, window->Window.Handle );
    else
    {
        glXDestroyGLXPixmap( ogDisplay.Display, window->Window.Handle );
        XFreePixmap( ogDisplay.Display, window->Window.Pixmap );
    }
    XFlush( ogDisplay.Display ); /*! \todo Shouldn't need this XFlush */

#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE

    /* Make sure we don't close a window with current context active */
    if( ogStructure.Window == window )
        wglMakeCurrent( NULL, NULL );

    /*
     * Step through the list of windows.  If the rendering context
     * is not being used by another window, then we delete it.
     */
    {
        int used = FALSE;
        SOG_Window *iter;

        for(
            iter = ( SOG_Window * )ogStructure.Windows.First;
            iter;
            iter = ( SOG_Window * )iter->Node.Next
        )
            if(
                ( iter->Window.Context == window->Window.Context ) &&
                ( iter != window )
            )
                used = TRUE;

        if( ! used )
            wglDeleteContext( window->Window.Context );
    }
    if( !( window->State.IsOffscreen ) )
        DestroyWindow( window->Window.Handle );
    else
    {
        DeleteObject( window->Window.Bitmap );
        DeleteDC( window->Window.Device );
    }
#endif
}


/* -- INTERFACE FUNCTIONS -------------------------------------------------- */

/*!
    \fn
    \brief    Create a new top-level window
    \ingroup  window
    \param    title      Title for created window

    This function sends a request for a window to be constructed.
    OpenGLUT immediately constructs a data structure to track further
    events with the window, on the theory that eventually the window
    manager will get back to us with a real window.  This allows us
    to begin registering callbacks immediately.

    In fact, you <b>must</b> register a display callback via
    glutDisplayFunc() before you enter glutMainLoop().

    For onscreen windows, you should not depend upon the window
    concretely existing or being visibile until you are told
    that it exists and is visible via a registered callback.

    The return value is an \a int.  It should be positive for
    valid windows or 0 if failure occurred for some reason
    (Though traditional GLUT tends to bail out and abort
    rather than returning errors.)  The integer is your
    <i>window id</i>.  Old GLUT promises that these integers
    are ``small''; we do not reuse old <i>id</i>s, but do
    produce them sequentially.

    You can change the title later via glutSetWindowTitle().

    \see glutDestroyWindow(), glutCreateSubWindow(), glutSetWindowTitle(),
         glutCreateMenuWindow()
*/
int OGAPIENTRY glutCreateWindow( const char* title )
{
    return ogCreateWindow(
        NULL, title, ogState.Position.X, ogState.Position.Y,
        ogState.Size.X, ogState.Size.Y, OG_CW_NORMAL
    )->ID;
}

/*!
    \fn
    \brief    Create a subwindow
    \ingroup  window
    \param    parentID   Parent window identifier
    \param    x          Horizontal position of subwindow
    \param    y          Vertical position of subwindow
    \param    w          Width of subwindow
    \param    h          Height of subwindow

    In almost every regard that is important to you, a subwindow is like
    a top-level window.  It has a window id; it has its own set of
    event callbacks; you can render to it; you are notified of its
    creation; ...

    A subwindow lives inside of some other window (possibly a top-level
    window, possibly another subwindow).  Because of this, it generally
    only interacts with other windows of your own creation, hence
    it is not subjected to a window manager.  This is the primary
    source for its differences from a top-level window:

    - There are no borders or decorations.
    - There is no title bar, hence no title.
    - Requests tend to be acted on a little more directly,
      without interference from a window manager.
    - The subwindow inherits the display mode of its parent.

    Like a top-level window, you <b>must</b> register a display
    callback function if you wish to use glutMainloop().

    A notable case where this function can fail is for offscreen
    windows.  A coherent concept of a subwindow of an offscreen
    window would introduce more complication than is presently
    believed to be worthwhile.  Attempting such a window presently
    just fails.  Failure is denoted by a 0 <i>window id</i> being
    returned.

    Subwindows can be very useful for partitioning a window into
    GUI elements: They have their own input callbacks, so you
    don't have to figure out which window an event is for.
    Graphics are clipped to the boundaries of your subwindows,
    so you do not need to worry much about where your drawing
    goes.  Because windows and subwindows work almost identically
    from the perspective of a GLUT program, it is relatively easy
    to move a cluster of related controls into a separate top-level
    window---or, conversely, embed what was a top-level window
    inside of another window.  OpenGLUT can also report
    some basic statistics about your (sub)window, relieving you
    of the duty of tracking all of that information for yourself.

    \see glutCreateWindow(), glutDestroyWindow(),
         glutCreateMenuWindow()
*/
int OGAPIENTRY glutCreateSubWindow( int parentID, int x, int y, int w, int h )
{
    int ret = 0;

    if( GL_FALSE == ogStructure.Window->State.IsOffscreen )
    {
        SOG_Window *window = NULL;
        SOG_Window *parent = NULL;

        freeglut_assert_ready; /*! \todo This looks like an abuse of assert */
        parent = ogWindowByID( parentID );
        if( !parent )
            return 0;
        window = ogCreateWindow( parent, "", x, y, w, h, OG_CW_NORMAL );
        ret = window->ID;
    }
    return ret;
}



/*
 * X/Y are macros provided to slightly simplify code.  At one time, they
 * were common to macros used for the various callbacks, but that was
 * logically not the right thing to do when I was thinking more clearly.
 *
 * XXX As the X/Y macros are no longer shared with anything else, we should
 * XXX probably remove them and replace with inline code.
 */
#define X ( x + glutGet( GLUT_WINDOW_X ) )
#define Y ( y + glutGet( GLUT_WINDOW_Y ) )
/*!
    \fn
    \brief    Create a client-controlled menu window.
    \ingroup  experimental
    \param    parentID    Parent window identifier.
    \param    x           Horizontal coordinate on the screen.
    \param    y           Vertical coordinate on the screen.
    \param    w           Width of the new window.
    \param    h           Height of the new window.

    This is a highly experimental function.  It creates a menu-like
    window, of requested dimensions, and at a position relative
    to your current window.

    The documentation for this function is currently the OpenGLUT
    Menu Window proposal.  Variance from that proposal may generally
    be explained by the fact that this feature is highly experimental.
    It may also be explained by the fact that the implementation may
    simply be incomplete.

    \note   Did you notice that this feature is highly experimental?
    \see    glutCreateWindow(), glutCreateSubWindow(), glutDestroyWindow(),
            glutKeyboardFunc(), glutKeyboardUpFunc(), glutSpecialFunc(),
            glutSpecialUpFunc(), glutMotionFunc(), glutPassiveMotionFunc(),
            glutMouseFunc(), glutMouseWheelFunc()
*/
int OGAPIENTRY glutCreateMenuWindow( int parentID, int x, int y, int w, int h )
{
    int ret = 0;

    if( GL_FALSE == ogStructure.Window->State.IsOffscreen )
    {
        SOG_Window *window = NULL;
        SOG_Window *parent = NULL;

        freeglut_assert_ready; /* XXX This looks like assert() abuse */
        parent = ogWindowByID( parentID );
        if( parent )
        {
            window = ogCreateWindow(
                parent, "", X, Y, w, h, OG_CW_CLIENT_MENU
            );
            if( window )
            {
                /*
                 * These 3 things should already be done by ogCreateWindow(),
                 * I think.
                 */
                window->Parent = parent;
                ret = window->ID;
                glutSetWindow( ret );

                /*
                 * Since menu windows should not have certain callbacks
                 * defined by client applications (mouse motion, for
                 * example, must pass through to the parent window), the
                 * callbacks do not allow IsClientMenu windows to register
                 * those callbacks.  But *we* must register them now, so
                 * we very briefly lie and claim that the window is not
                 * in fact a client menu.
                 *
                 * It might be better to generalize this and say that *no*
                 * callbacks are allowed to the client for these windows
                 * and instead we can register every type of callback and
                 * do a pass-through.  (Or simply not register some
                 * types.  Does one ever need a joystick in a client
                 * menu window?)
                 *
                 * The point is that then we could implement this as a
                 * generalized "callback lock".  We could also block
                 * callbacks by some kind of list or bitmap...
                 */
                ogStructure.Window->IsClientMenu = GL_FALSE;
                glutKeyboardFunc( oghClientMenuKeyboard );
                glutKeyboardUpFunc( oghClientMenuKeyboardUp );
                glutSpecialFunc( oghClientMenuSpecial );
                glutSpecialUpFunc( oghClientMenuSpecialUp );
                glutMotionFunc( oghClientMenuMotion );
                glutPassiveMotionFunc( oghClientMenuPassiveMotion );
                glutMouseFunc( oghClientMenuMouse );
                glutMouseWheelFunc( oghClientMenuMouseWheel );
                ogStructure.Window->IsClientMenu = GL_TRUE;
            }
        }
    }
    return ret;
}
#undef Y
#undef X

/*!
    \fn
    \brief    Destroy a window and associated subwindows
    \ingroup  window
    \param    windowID   Window identifier

    After this function is invoked, the only further event
    that may be delivered for your window is the one for its
    destruction.  All other events should be discarded.

    Once a window has been destroyed, further attempts to
    use the window named by \a windowID are undefined.  OpenGLUT generally
    tries to be sensible, and should not recycle the dead
    \a windowID, but you should treat a destroyed window much
    like a pointer to deallocated memory and try not to use it.

    \see      glutCreateWindow()
*/
void OGAPIENTRY glutDestroyWindow( int windowID )
{
    SOG_Window *window = ogWindowByID( windowID );
    if( !window )
        return;
    /*! \todo Clean this up. */
    {
        ogExecutionState ExecState = ogState.ExecState;
        ogAddToWindowDestroyList( window );
        ogState.ExecState = ExecState;
    }
}

/*!
    \fn
    \brief    Select the <i>current window</i>
    \ingroup  window
    \param    ID       Window identifier

    Sets the <i>current window</i> to \a ID.

    All OpenGL rendering goes to the <i>current window</i>.
    Many OpenGLUT functions also implicitly use the
    <i>current window</i>.

    Many OpenGLUT callback operations are tied to a window.
    When your callback is invoked, OpenGLUT will set that
    particular window to be the <i>current window</i>.
    However, some callbacks---such as that registered via
    glutIdleFunc()---do not have associated windows.  If
    a callback is not associated to a particular window,
    then when OpenGLUT invokes that callback
    you should <b>always</b> use glutSetWindow() to
    select the appropriate window before doing any
    OpenGL rendering or doing any OpenGLUT window-related
    operations.

    There may be cases when you can get away with assuming
    that the <i>current window</i> is unchanged since some
    prior time, but OpenGLUT has considerable liberaty with
    respect to when it invokes your functions.  Also, your
    program may add more windows or more operations on other
    windows as you develop it.

    Lastly, this is a convenient way to select among
    multiple windows for drawing without actually waiting
    for that window's display callback.  Simply set the
    <i>current window</i> and draw immediately.  This is
    not always advisable, but may be practical.

    It is an error to set the <i>current window</i> to
    a non-existant window (e.g., one that you have closed).
    A warning will be printed on \a stderr if you
    try to do so, and the <i>current window</i> should be
    unchanged.

    \see glutGetWindow()
*/
void OGAPIENTRY glutSetWindow( int ID )
{
    SOG_Window *window = NULL;

    freeglut_assert_ready; /*! \todo Looks like an abuse of assert */
    if( ogStructure.Window )
        if( ogStructure.Window->ID == ID )
            return;

    window = ogWindowByID( ID );
    if( !window )
    {
        ogWarning( "glutSetWindow(): window ID %i not found!", ID );
        return;
    }

    ogSetWindow( window );
}

/*!
    \fn
    \brief    Return the current window identifier, 0 if undefined
    \ingroup  window

    glutGetWindow() returns the <i>window id</i> of the
    <i>current window</i>.  This is useful, e.g., if you have
    a generic function that is used with several windows and
    it needs to temporarily change to another window.
    (There is no window stack for you to use with pushes and
    pops.  Do not be confused by glutPushWindow() and glutPopWindow();
    those pushes and pops are <b>not</b> stack-related!)

    One cause for the function to return 0 is if you have
    called glutDestroyWindow() on the <i>current window</i> and have
    done nothing to set a new window as current.

    \see glutSetWindow()
*/
int OGAPIENTRY glutGetWindow( void )
{
    freeglut_assert_ready; /*! \todo looks like an abuse of assert */
    if( !ogStructure.Window )
        return 0;
    return ogStructure.Window->ID;
}

/*!
    \fn
    \brief    Request that the <i>current window</i> be visible
    \ingroup  window

    glutShowWindow() requests that the window system make
    the <i>current window</i> visible.

    This is generally not necessary.  When you create a
    window, it will normally become visible.  Unless you specifically
    hide it, it will remain visible.  Though visible,
    of course, it may be covered by other windows;
    that would be an issue for window stacking order not
    visibility.

    When, and if, your window's visibility status changes,
    you may find out via a glutWindowStatusFunc() callback.

    \see      glutHideWindow(), glutPopWindow(), glutPushWindow(),
              glutWindowStatusFunc()
*/
void OGAPIENTRY glutShowWindow( void )
{
    freeglut_assert_ready;  /*! \todo Looks like an abuse of assert */
    freeglut_assert_window; /*! \todo Looks like an abuse of assert */

    if( GL_FALSE == ogStructure.Window->State.IsOffscreen )
    {
#if TARGET_HOST_UNIX_X11

        XMapWindow( ogDisplay.Display, ogStructure.Window->Window.Handle );
        XFlush( ogDisplay.Display ); /*! \todo Shouldn't need this XFlush */

#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE

        ShowWindow( ogStructure.Window->Window.Handle, SW_SHOW );

#endif
    }
    ogStructure.Window->State.Redisplay = GL_TRUE;
}

/*!
    \fn
    \brief    Make the current window hidden
    \ingroup  window

    Even if a window is ``open'', it need not be visible.
    It may be convenient to hide a window rather than to close it,
    if you want to re-display the window at the same location and
    size, later.  Redefining all of the OpenGLUT features of a
    window and adding its <i>window id</i> to your tracking
    when re-opening a window may also be bothersome.  So, rather
    than destroying it, you can simply ask for it to be hidden.

    \see      glutShowWindow()
*/
void OGAPIENTRY glutHideWindow( void )
{
    freeglut_assert_ready;  /*! \todo Looks like an abuse of assert */
    freeglut_assert_window; /*! \todo Looks like an abuse of assert */

    if( GL_FALSE == ogStructure.Window->State.IsOffscreen )
    {
#if TARGET_HOST_UNIX_X11

        if( !ogStructure.Window->Parent )
            XWithdrawWindow( ogDisplay.Display,
                             ogStructure.Window->Window.Handle,
                             ogDisplay.Screen );
        else
            XUnmapWindow( ogDisplay.Display,
                          ogStructure.Window->Window.Handle );
        XFlush( ogDisplay.Display ); /*! \todo Shouldn't need this XFlush */

#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE

        ShowWindow( ogStructure.Window->Window.Handle, SW_HIDE );

#endif
    }
    ogStructure.Window->State.Redisplay = GL_FALSE;
}

/*!
    \fn
    \brief    Iconify the current window
    \ingroup  window
    \note     Applies only to onscreen, top-level windows.
    \note     Not guaranteed to have any effect; effect may be
              arbitrarily delayed.
    \note     There is no callback that specifically tells you
              when (or if) your window is iconified.

    Most window systems have some kind of ``minimized'' or ``iconified''
    state for windows.  All systems currently supported by OpenGLUT
    do so.  The exact meaning of iconification is somewhat
    system-dependant, but this makes a request of the window system
    to place the window into this state.

    Graphic output is usually suspended in this form.
    User input may be partially or wholly suspended.

    If and when your window is iconified by the window system,
    it may be uniconified at any time by the system.  This usually
    happens at the request of a user.  Because of this, you should
    not use this function to hide a window.  Rather, it is to help
    unclutter the user's display, and is more or less consensual with
    the user.  Use glutHideWindow() if you want to hide the window
    entirely.

    \see      glutSetIconTitle(), glutHideWindow(), and glutShowWindow()
*/
void OGAPIENTRY glutIconifyWindow( void )
{
    freeglut_assert_ready;  /*! \todo Looks like an abuse of assert */
    freeglut_assert_window; /*! \todo Looks like an abuse of assert */

    ogStructure.Window->State.Visible   = GL_FALSE;
    if( GL_FALSE == ogStructure.Window->State.IsOffscreen )
    {
#if TARGET_HOST_UNIX_X11

        XIconifyWindow( ogDisplay.Display, ogStructure.Window->Window.Handle,
                        ogDisplay.Screen );
        XFlush( ogDisplay.Display ); /*! \todo Shouldn't need this XFlush */

#elif TARGET_HOST_WIN32

        ShowWindow( ogStructure.Window->Window.Handle, SW_MINIMIZE );

#endif
    }
    ogStructure.Window->State.Redisplay = GL_FALSE;
}

/*!
    \fn
    \brief    Request changing the title of the current window
    \ingroup  window
    \param    title    New window title
    \note     Only for managed, onscreen, top-level windows.
    \note     Not all window systems display titles.
    \note     May be ignored or delayed by window manager.

    glutSetWindowTitle() requests that the window system
    change the title of the window.

    Normally a window system displays a title for every
    top-level window in the system.  The initial title is
    set when you call glutCreateWindow().  By means of this
    function you can set the titles for your top-level
    OpenGLUT windows.

    Some window systems do not provide titles for
    windows, in which case this function may have no
    useful effect.

    Because the effect may be delayed or lost, you
    should not count on the effect of this function.
    However, it can be a nice touch to use the window
    title bar for a one-line status bar in some cases.
    Use discretion.

    If you just want one title for the window over the window's
    entire life, you should set it when you open the window
    with glutCreateWindow().

    \see glutCreateWindow(), glutSetIconTitle()
*/
void OGAPIENTRY glutSetWindowTitle( const char* title )
{
    freeglut_assert_ready;  /*! \todo Looks like an abuse of assert */
    freeglut_assert_window; /*! \todo looks like an abuse of assert */
    if( ! ogStructure.Window->Parent &&
        ( GL_FALSE == ogStructure.Window->State.IsOffscreen ) )
    {
#if TARGET_HOST_UNIX_X11

        XTextProperty text;

        text.value = ( unsigned char * )ogStrDup( title );
        text.encoding = XA_STRING;
        text.format = 8;
        text.nitems = strlen( title );

        XSetWMName(
            ogDisplay.Display,
            ogStructure.Window->Window.Handle,
            &text
        );

        XFlush( ogDisplay.Display ); /*! \todo Shouldn't need this XFlush */
        free( text.value );

#elif TARGET_HOST_WIN32

        SetWindowText( ogStructure.Window->Window.Handle, title );

#elif TARGET_HOST_WINCE

        {
            wchar_t *wstr = oghStrToWstr( title );
            SetWindowText( ogStructure.Window->Window.Handle, wstr );
            free( wstr );
        }

#endif
    }
}

/*!
    \fn
    \brief    Requests changing the iconified title of the current window
    \ingroup  window
    \param    title    New window title
    \note     Effect is system-dependant.

    Requests that the window system change the title of the
    icon (or whatever) that is displayed when the
    <i>current window</i> is in iconified mode.

    As discussed under glutIconifyWindow(), most window systems allow
    a window to be placed in some kind of minimized, or iconified,
    state.  In that state, the normal interior of the window is
    likely to be obscured, and the only clue about the window
    contents may be the window title.

    \note There Exactly what "iconified" means is system
          dependant.  Iconification may not be supported, or
          the title may not be available---or legible.  Avoid
          putting essential information into the icon title.
    \see  glutSetWindowTitle(), glutIconifyWindow()

*/
void OGAPIENTRY glutSetIconTitle( const char* title )
{
    freeglut_assert_ready;  /*! \todo looks like an abuse of assert */
    freeglut_assert_window; /*! \todo looks like an abuse of assert */

    if( !ogStructure.Window->Parent &&
        GL_FALSE == ogStructure.Window->State.IsOffscreen )
    {
#if TARGET_HOST_UNIX_X11

        XTextProperty text;

        text.value = ( unsigned char * )ogStrDup( title );
        text.encoding = XA_STRING;
        text.format = 8;
        text.nitems = strlen( title );

        XSetWMIconName(
            ogDisplay.Display,
            ogStructure.Window->Window.Handle,
            &text
        );

        XFlush( ogDisplay.Display ); /*! \todo Shouldn't need this xflush */
        free( text.value );

#elif TARGET_HOST_WIN32

        SetWindowText( ogStructure.Window->Window.Handle, title );

#elif TARGET_HOST_WINCE
        {
            wchar_t *wstr = oghStrToWstr( title );
            SetWindowText( ogStructure.Window->Window.Handle, wstr );
            free( wstr );
        }

#endif
    }
}

#define _NET_WM_STATE_REMOVE        0    /* remove/unset property */
#define _NET_WM_STATE_ADD           1    /* add/set property */
#define _NET_WM_STATE_TOGGLE        2    /* toggle property  */

/*!
    \fn
    \brief    Request changing the size of the current window
    \ingroup  window
    \param    width    Requested width of the current window
    \param    height   Requested height of the current window

    The glutReshapeWindow() function adjusts the width and height of
    the <i>current window</i>, if it is an onscreen
    top-level or subwindow.  Subwindows are typically
    resized and repositioned in response to window resize events.

    The window system may delay or even alter your request.
    Use the glutReshapeFunc() callback registration for the window
    if you want

    If you try to make a subwindow smaller than its parent, the
    parent will not grow to accomodate the child.

    \todo     Add support for offscreen windows.
    \see      glutInit(), glutInitWindowSize(), glutReshapeFunc() and
              glutCreateSubWindow()
*/
void OGAPIENTRY glutReshapeWindow( int width, int height )
{
    freeglut_assert_ready;  /*! \todo looks like an abuse of assert */
    freeglut_assert_window; /*! \todo looks like an abuse of assert */

    /*! \todo Could delete/create/set-window-id for offscreen. */
    if( GL_FALSE == ogStructure.Window->State.IsOffscreen )
    {
        ogStructure.Window->State.NeedToResize = GL_TRUE;
        ogStructure.Window->State.Width  = width;
        ogStructure.Window->State.Height = height;
    }
}


/*!
    \fn
    \brief    Request to change the position of the current window
    \ingroup  window
    \param    x       Requested horizontal position of the current window
    \param    y       Requested vertical position of the current window

    The glutPositionWindow() function requests that the window system
    position a top-level or subwindow
    relative to the top-left corner.  Subwindows are typically
    resized and repositioned in response to window resize events.

    \note     The position of top-level windows is ultimately determined
              by the windowing system.  Therefore, a position request
              by an OpenGLUT application may not necessarily succeed.
    \note     May not take immediate effect; wait for the callback.
    \note     Not applicable to offscreen windows.
    \see      glutInit(), glutInitWindowPosition(), glutReshapeFunc(), and
              glutCreateSubWindow()
*/
void OGAPIENTRY glutPositionWindow( int x, int y )
{
    freeglut_assert_ready;  /*! \todo Looks like an abuse of assert */
    freeglut_assert_window; /*! \todo looks like an abuse of assert */

    if( GL_FALSE == ogStructure.Window->State.IsOffscreen )
    {
#if TARGET_HOST_UNIX_X11

        XSetWindowAttributes  xattr;
        xattr.override_redirect = False;
        XChangeWindowAttributes ( ogDisplay.Display, ogStructure.Window->Window.Handle, CWOverrideRedirect, &xattr );

        Atom atom = XInternAtom ( ogDisplay.Display, "_NET_WM_STATE_FULLSCREEN", True );
        XChangeProperty (
           ogDisplay.Display, ogStructure.Window->Window.Handle,
           XInternAtom ( ogDisplay.Display, "_NET_WM_STATE", True ),
           XA_ATOM,  32,  PropModeReplace,
           (unsigned char*) &atom,  1 );

        //// get identifiers for the provided atom name strings
        Atom wm_state   = XInternAtom ( ogDisplay.Display, "_NET_WM_STATE", False );
        Atom fullscreen = XInternAtom ( ogDisplay.Display, "_NET_WM_STATE_FULLSCREEN", False );

        XEvent xev;
        memset ( &xev, 0, sizeof(xev) );

        xev.type                 = ClientMessage;
        xev.xclient.window       = ogStructure.Window->Window.Handle;
        xev.xclient.message_type = wm_state;
        xev.xclient.format       = 32;
        xev.xclient.data.l[0]    = _NET_WM_STATE_REMOVE;
        xev.xclient.data.l[1]    = fullscreen;
        XSendEvent (                // send an event mask to the X-server
           ogDisplay.Display,
           DefaultRootWindow ( ogDisplay.Display ),
           False,
           SubstructureNotifyMask,
           &xev );

        XMoveWindow( ogDisplay.Display, ogStructure.Window->Window.Handle,
                     x, y );
        XFlush( ogDisplay.Display ); /*! \todo Shouldn't need this XFlush */

#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE

        RECT winRect;

        GetWindowRect( ogStructure.Window->Window.Handle, &winRect );
   //     MoveWindow(
   //         ogStructure.Window->Window.Handle,
			//winRect.left,
   //         winRect.top,
   //         winRect.right - winRect.left,
   //         winRect.bottom - winRect.top,
   //         TRUE
   //     );

        MoveWindow(
            ogStructure.Window->Window.Handle,
            x,
            y,
            winRect.right - winRect.left,
            winRect.bottom - winRect.top,
            TRUE
        );

#endif
    }
}

#if __PRAXIS_WINDOWS__
HWND    OGAPIENTRY glutGetWindowHandle( void )
{
    return ogStructure.Window->Window.Handle;
}
#endif


/*!
    \fn
    \brief    Resize the current window to cover the entire screen
    \ingroup  window

    The glutFullScreen() function resizes the window to cover the
    entire screen and hide window decorations such as title bars
    and icons.

    \note    The desktop resolution is not affected by a call
             to glutReshapeWindow() or glutFullScreen().
    \note    The size of windows is ultimately determined by the
             windowing system.  Therefore, a fullscreen request
             by an OpenGLUT application may not necessarily succeed
             or take immediate effect.
    \note    Not applicable to offscreen or subwindows.
    \see     glutInit(), glutInitWindowPosition(), glutInitWindowSize(),
             glutGet(), glutPositionWindow() and glutReshapeWindow()
*/
void OGAPIENTRY glutFullScreen( void )
{
    freeglut_assert_ready;  /*! \todo looks like an abuse of assert */
    freeglut_assert_window; /*! \todo looks like an abuse of assert */

    if( GL_FALSE == ogStructure.Window->State.IsOffscreen )
    {
#if TARGET_HOST_UNIX_X11

#if 1
//        typedef struct {
//                         unsigned long   flags;
//                         unsigned long   functions;
//                         unsigned long   decorations;
//                         long            inputMode;
//                         unsigned long   status;
//                       } Hints;

//        Hints   hints;
//        Atom    property;
//        hints.flags = 2;        // Specify that we're changing the window decorations.
//        hints.decorations = 0;  // 0 (false) means that window decorations should go bye-bye.
//        property = XInternAtom(ogDisplay.Display,"_MOTIF_WM_HINTS",True);

//        XChangeProperty(
//                    ogDisplay.Display,
//                    ogStructure.Window->Window.Handle,
//                    property,property,32,
//                    PropModeReplace,
//                    (unsigned char *)&hints,5);

//        XMoveResizeWindow(
//            ogDisplay.Display,
//            ogStructure.Window->Window.Handle,
//            0, 0,
//            ogDisplay.ScreenWidth,
//            ogDisplay.ScreenHeight );

//        XFlush( ogDisplay.Display ); /* This is needed */



        // Now try to tell X to make it fullscreen...

        XSetWindowAttributes  xattr;
        xattr.override_redirect = True;
        XChangeWindowAttributes ( ogDisplay.Display, ogStructure.Window->Window.Handle, CWOverrideRedirect, &xattr );

        Atom atom = XInternAtom ( ogDisplay.Display, "_NET_WM_STATE_FULLSCREEN", True );
        XChangeProperty (
           ogDisplay.Display, ogStructure.Window->Window.Handle,
           XInternAtom ( ogDisplay.Display, "_NET_WM_STATE", True ),
           XA_ATOM,  32,  PropModeReplace,
           (unsigned char*) &atom,  1 );

        //// get identifiers for the provided atom name strings
        Atom wm_state   = XInternAtom ( ogDisplay.Display, "_NET_WM_STATE", False );
        Atom fullscreen = XInternAtom ( ogDisplay.Display, "_NET_WM_STATE_FULLSCREEN", False );

        XEvent xev;
        memset ( &xev, 0, sizeof(xev) );

        xev.type                 = ClientMessage;
        xev.xclient.window       = ogStructure.Window->Window.Handle;
        xev.xclient.message_type = wm_state;
        xev.xclient.format       = 32;
        xev.xclient.data.l[0]    = _NET_WM_STATE_ADD;
        xev.xclient.data.l[1]    = fullscreen;
        XSendEvent (                // send an event mask to the X-server
           ogDisplay.Display,
           DefaultRootWindow ( ogDisplay.Display ),
           False,
           SubstructureNotifyMask,
           &xev );

#else
        int x, y;
        Window w;

        XMoveResizeWindow(
            ogDisplay.Display,
            ogStructure.Window->Window.Handle,
            0, 0,
            ogDisplay.ScreenWidth,
            ogDisplay.ScreenHeight
        );

        XFlush( ogDisplay.Display ); /* This is needed */

        XTranslateCoordinates(
            ogDisplay.Display,
            ogStructure.Window->Window.Handle,
            ogDisplay.RootWindow,
            0, 0, &x, &y, &w
        );

        if( x || y )
        {
            XMoveWindow(
                ogDisplay.Display,
                ogStructure.Window->Window.Handle,
                -x, -y
            );
            XFlush( ogDisplay.Display ); /*! \todo Shouldn't need to XFlush */
        }
#endif

#elif TARGET_HOST_WIN32
        RECT rect;

        /*
         * For fullscreen mode, force the top-left corner to 0,0
         * and adjust the window rectangle so that the client area
         * covers the whole screen.
         */
        //ogDisplay.ScreenWidth  = GetSystemMetrics( SM_CXMAXIMIZED );
        //ogDisplay.ScreenHeight = GetSystemMetrics( SM_CYMAXIMIZED );

        rect.left   = 0;
        rect.top    = 0;
        rect.right  = ogDisplay.ScreenWidth;
        rect.bottom = ogDisplay.ScreenHeight;

        AdjustWindowRect ( &rect, WS_OVERLAPPEDWINDOW | WS_CLIPSIBLINGS |
                                  WS_CLIPCHILDREN, FALSE );

        /*
         * SWP_NOACTIVATE     Do not activate the window
         * SWP_NOOWNERZORDER  Do not change position in z-order
         * SWP_NOSENDCHANGING Supress WM_WINDOWPOSCHANGING message
         * SWP_NOZORDER       Retains the current Z order (ignore 2nd param)
         */
        SetWindowPos(
            ogStructure.Window->Window.Handle,
            HWND_TOP,
            rect.left,
            rect.top,
            rect.right  - rect.left,
            rect.bottom - rect.top,
            SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_NOSENDCHANGING |
            SWP_NOZORDER
        );
#endif
    }
}

/*!
    \fn
    \brief    Request to lower the current window to the bottom.
    \ingroup  window

    This function requests that the <i>current window</i> be ``pushed''
    to the back.

    A window can be in front of or behind other windows, as determined
    by the z-order from front to back.  Top-level OpenGLUT windows
    can be placed at the front or back of the z-order by means of
    the glutPopWindow() and glutPushWindow() API functions.

    A z-order also applies to the subwindows of a top-level window.
    While the z-order of top-level windows can usually be
    adjusted by the user, subwindow z-order is controlled entirely
    by the application.

    There may not be an immediate effect to this function.  Wait for
    the glutWindowStatusFunc() callback to tell you about whatever
    obscured/visible status your window achieves.

    \note    The z-order of top-level windows is ultimately managed by
             the windowing system.  Therefore, a push or pop request
             by an OpenGLUT application may not necessarily succeed
             or take immediate effect.
    \note    Not applicable to offscreen windows.
    \see     glutPopWindow(), glutWindowStatusFunc()
*/
void OGAPIENTRY glutPushWindow( void )
{
    freeglut_assert_ready;  /*! \todo looks like an abuse of assert */
    freeglut_assert_window; /*! \todo looks like an abuse of assert */

    if( GL_FALSE == ogStructure.Window->State.IsOffscreen )
    {
#if TARGET_HOST_UNIX_X11

        XLowerWindow( ogDisplay.Display, ogStructure.Window->Window.Handle );

#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE

        SetWindowPos(
            ogStructure.Window->Window.Handle,
            HWND_BOTTOM,
            0, 0, 0, 0,
            SWP_NOSIZE | SWP_NOMOVE
        );

#endif
    }
}

/*!
    \fn
    \brief    Request to raise the current window to the top
    \ingroup  window

    Request that the <i>current window</i> be brought to the top.

    A window can be in front of or behind other windows, as determined
    by the z-order from front to back.  Top-level OpenGLUT windows
    can be placed at the front or back of the z-order by means of
    the glutPopWindow() and glutPushWindow() API functions.

    A z-order also applies to the subwindows of a top-level window.
    While the z-order of top-level windows can usually be
    adjusted by the user, subwindow z-order is controlled entirely
    by the application.

    If this has any effect on your window's visibility, you should
    receive a glutWindowStatusFunc() callback and a
    glutDisplayFunc() callback.

    \note    The z-order of top-level windows is ultimately managed by
             the windowing system.  Therefore, a push or pop request
             by an OpenGLUT application may not necessarily succeed
             or take immediate effect.
    \note    Not applicable to offscreen windows.
    \see     glutCreateWindow(), glutDisplayFunc(), glutPushWindow(),
             glutWindowStatusFunc()
*/
void OGAPIENTRY glutPopWindow( void )
{
    freeglut_assert_ready;  /*! \todo looks like an abuse of assert */
    freeglut_assert_window; /*! \todo looks like an abuse of assert */

    if( GL_FALSE == ogStructure.Window->State.IsOffscreen )
    {
#if TARGET_HOST_UNIX_X11

        XRaiseWindow( ogDisplay.Display, ogStructure.Window->Window.Handle );

#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE

        SetWindowPos(
            ogStructure.Window->Window.Handle,
            HWND_TOP,
            0, 0, 0, 0,
            SWP_NOSIZE | SWP_NOMOVE
        );

#endif
    }
}

/*!
    \fn
    \brief    Get the user data for the current window
    \ingroup  window

    This function will return whatever \a void* value is
    associated with the <i>current window</i> via glutSetWindowData().
    This is \a NULL if you did not associate a pointer with your window.
    This can be useful in a situation where you have a single
    callback function performing services for many windows.
    You <b>could</b> keep track of the <i>window id</i>s in a
    global list and search for the <i>current window</i> in
    that list.  But this is quicker than searching a data
    structure, and allows you to avoid the use of globals for this.

    \see      glutSetWindowData()

*/
void* OGAPIENTRY glutGetWindowData( void )
{
    return ogStructure.Window->UserData;
}

/*!
    \fn
    \brief    Set the user data for the current window
    \ingroup  window
    \param    data    Arbitrary client-supplied pointer.

    This associates an arbitrary \a void* value with the
    <i>current window</i>.  This is especially useful
    in client-side callbacks that service many windows, if
    the client needs to know more about the window than
    OpenGLUT normally will provide.

    \see      glutGetWindowData()
*/
void OGAPIENTRY glutSetWindowData(void* data)
{
    ogStructure.Window->UserData = data;
}

/*!
    \fn
    \brief   Set stay on top mode for current window
    \ingroup experimental
    \param   enable Either \a GL_TRUE or \a GL_FALSE

    \note    Does not work on all window managers.
    \note    Sends the Icewm style message to all window
             managers (KDE, twm, blackbox, ratpoison, amiwm,
             and whatever others you have).  Can we detect
             Icewm reliably and only send the Icewm formatted
             message for Icewm?  Possibbly harmless as it stands,
             but looks wrong.
    \todo    Can a glutGet() be defined to tell us whether
             a window can be made to stay on top?  Or whether
             a window has (successfully) been marked for staying
             on top?
    \todo    Should walk the tree of menus and glutPopWindow()
             (or all windows that are of menu-window type?).
    \todo    Investigate making a workalike variant using
             glutPopWindow() to mimic the feature where not
             directly supported.
*/

void OGAPIENTRY glutSetWindowStayOnTop( int enable )
{
#if TARGET_HOST_WIN32
    SetWindowPos(
        ogStructure.Window->Window.Handle,
        enable ? HWND_TOPMOST : HWND_NOTOPMOST,
        0, 0, 0, 0, 0
    );
#elif TARGET_HOST_UNIX_X11
    Display *display = ogDisplay.Display;
    Window   window  = ogStructure.Window->Window.Handle;
    XEvent e;

    /*
     * Where this feature is supported, these seem to be common bits
     * to set up an XEvent for requesting a top-most window.
     */
    e.xclient.type         = ClientMessage;
    e.xclient.display      = display;
    e.xclient.window       = window;
    e.xclient.format       = 32;
    e.xclient.data.l[ 2 ]  = 0;
    e.xclient.data.l[ 3 ]  = 0;
    e.xclient.data.l[ 4 ]  = 0;

    /* These two are for EWMH and KDE style window managers. */
    e.xclient.message_type = XInternAtom( display, "_NET_WM_STATE", FALSE );
    e.xclient.data.l[ 0 ]  = enable ? 1 : 0;

    if( XInternAtom(ogDisplay.Display, "KWIN_RUNNING", TRUE)       &&
        XInternAtom(ogDisplay.Display, "_KDE_NET_USER_TIME", TRUE) &&
        XInternAtom(ogDisplay.Display, "_DT_SM_WINDOW_INFO", TRUE)
    )
        e.xclient.data.l[ 1 ]  = XInternAtom( /* KDE */
            display, "_NET_WM_STATE_STAYS_ON_TOP", FALSE
        );
    else if( XInternAtom(display, "_NET_SUPPORTING_WM_CHECK", TRUE) &&
             XInternAtom(display, "_NET_WORKAREA", TRUE)            &&
             !XInternAtom(display, "_ICEWM_WINOPTHINT", TRUE)
    )
        e.xclient.data.l[ 1 ]  = XInternAtom( /* EWMH */
            display, "_NET_WM_STATE_ABOVE", FALSE
        );
    else
    {
        /* Others; maybe Window Maker, IceWM; we assume that this is safe! */
        e.xclient.message_type = XInternAtom( display, "_WIN_LAYER", FALSE );
        e.xclient.data.l[ 0 ]  = enable ? 10 : 4;
        e.xclient.data.l[ 1 ]  = 0;
    }

    XSendEvent( display, window, TRUE, SubstructureRedirectMask, &e );
#endif
}
